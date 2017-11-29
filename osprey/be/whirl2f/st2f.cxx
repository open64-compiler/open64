/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 * Module: st2f.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/st2f.cxx,v $
 *
 * Revision history:
 *  07-May-95 - Original Version
 *
 * Description:
 *
 *    See st2f.h for a description of the exported functions and 
 *    variables.  This module translates ST nodes into variable and
 *    function declarations (ST2F_decl_translate), and gets the 
 *    lvalue for a variable or function when directly referenced in
 *    an expression (ST2F_use_translate).  We provide a special 
 *    interface to deal with pseudo registers (pregs), but some 
 *    symbols must be handled by the context in which they appear,
 *    since this context uniquely determines the reference (e.g. 
 *    labels has label-numbers in the WN tree).
 *
 *    Possibly necessary TODO: sym_consts are only partially
 *    supported at the moment.
 *
 *    Fortran pointers are represented by two declarations, where
 *    one declares the pointer object (which is allocated memory)
 *    and one denotes the pointer dereference which also serves to
 *    specify the type of object to which is pointed:
 *
 *        INTEGER*4 a(12)
 *        POINTER(p, a)
 *
 *    Only "p" occurs in the WHIRL symbol table.  We have to derive
 *    "a" from "p" (with a name derived from "p").  The w2cf_symtab.h
 *    facilities coordinates this for us.
 *
 *    It is crucial that names with external linkage are generated 
 *    with the same name between compilation units.  For this reason
 *    we give file-scope variables precedence in name-ownership (i.e.
 *    they are entered first into the symbol-table).  If, despite this
 *    effort, there are clashes between names with static and external 
 *    linkage, the generated code may not be compilable or correctly
 *    executable.  TODO: Emit warning about this.
 * 
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/st2f.cxx,v $ $Revision: 1.1 $";
#endif

#include "whirl2f_common.h"
#include "PUinfo.h"
#include "tcon2f.h"
#include "wn2f.h"
#include "ty2f.h"
#include "st2f.h"
#include "init2f.h"
#include "cxx_memory.h"
#include "be_symtab.h"

 /* Defined in ty2f.c; signifies special translation of adjustable and
  * assumed sized arrays.
  */
/*------- Fwd refs for miscellaneous utilities ------------------------*/
/*---------------------------------------------------------------------*/

static BOOL ST2F_Is_Dummy_Procedure(ST *st) ;
static void ST2F_Declare_Return_Type(TOKEN_BUFFER tokens,TY_IDX return_ty, const char* name) ;

/*------- Handlers for references to and declarations of symbols ------*/
/*---------------------------------------------------------------------*/

static void ST2F_ignore(TOKEN_BUFFER tokens, ST *st);

static void ST2F_decl_error(TOKEN_BUFFER tokens, ST *st);
static void ST2F_decl_var(TOKEN_BUFFER tokens, ST *st);
static void ST2F_decl_func(TOKEN_BUFFER tokens, ST *st);
static void ST2F_decl_const(TOKEN_BUFFER tokens, ST *st);

static void ST2F_use_error(TOKEN_BUFFER tokens, ST *st);
static void ST2F_use_var(TOKEN_BUFFER tokens, ST *st);
static void ST2F_use_func(TOKEN_BUFFER tokens, ST *st);
static void ST2F_use_const(TOKEN_BUFFER tokens, ST *st);
static void ST2F_use_block(TOKEN_BUFFER tokens, ST *st);

/* The following maps every ST class to a function that can translate
 * it to C.
 */
typedef void (*ST2F_HANDLER_FUNC)(TOKEN_BUFFER, ST *);

static const ST2F_HANDLER_FUNC ST2F_Decl_Handler[CLASS_COUNT] =
{
  &ST2F_ignore,      /* CLASS_UNK   == 0x00 */
  &ST2F_decl_var,    /* CLASS_VAR   == 0x01 */
  &ST2F_decl_func,   /* CLASS_FUNC  == 0x02 */
  &ST2F_decl_const,  /* CLASS_CONST == 0x03 */
  &ST2F_decl_error,  /* CLASS_PREG  == 0x04 */
  &ST2F_decl_error,  /* CLASS_BLOCK == 0x05 */
  &ST2F_decl_error,  /* CLASS_NAME  == 0x06 */
}; /* ST2F_Decl_Handler */

static const ST2F_HANDLER_FUNC ST2F_Use_Handler[CLASS_COUNT] =
{
  &ST2F_ignore,        /* CLASS_UNK   == 0x00 */
  &ST2F_use_var,       /* CLASS_VAR   == 0x01 */
  &ST2F_use_func,      /* CLASS_FUNC  == 0x02 */
  &ST2F_use_const,     /* CLASS_CONST == 0x03 */
  &ST2F_use_error,     /* CLASS_PREG  == 0x04 */
  &ST2F_use_block,     /* CLASS_BLOCK == 0x05 */
  &ST2F_use_error      /* CLASS_NAME  == 0x06 */
}; /* ST2F_Use_Handler */


/*----------- hidden routines to handle ST declarations ---------------*/
/*---------------------------------------------------------------------*/
static void
ST2F_Define_Preg(const char *name, TY_IDX ty)
{
   /* Declare a preg of the given type, name and offset as a local
    * (register) variable in the current context.
    */
   TOKEN_BUFFER decl_tokens = New_Token_Buffer();
   UINT         current_indent = Current_Indentation();

   Set_Current_Indentation(PUinfo_local_decls_indent);
   Append_F77_Indented_Newline(PUinfo_local_decls, 1, NULL/*label*/);
   Append_Token_String(decl_tokens, name);
   TY2F_translate(decl_tokens, ty);
   Append_And_Reclaim_Token_List(PUinfo_local_decls, &decl_tokens);
   Set_Current_Indentation(current_indent);
} /* ST2F_Define_Preg */


static void 
ST2F_ignore(TOKEN_BUFFER tokens, ST *st)
{
   return; /* Just ignore it, i.e. do nothing! */
} /* ST2F_ignore */

static void 
ST2F_decl_error(TOKEN_BUFFER tokens, ST *st)
{
   ASSERT_DBG_FATAL(FALSE, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS,
		     ST_sym_class(st), "ST2F_decl_error"));
} /* ST2F_decl_error */

static void 
ST2F_decl_var(TOKEN_BUFFER tokens, ST *st)
{
   INITO_IDX    inito;
   const char  *pointee_name;
   const char  *st_name = W2CF_Symtab_Nameof_St(st);
   TOKEN_BUFFER decl_tokens = New_Token_Buffer();
   TY_IDX       ty_rt = ST_type(st);

   ASSERT_DBG_FATAL(ST_sym_class(st)==CLASS_VAR, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_decl_var"));

   if (Current_scope > GLOBAL_SYMTAB) 
   {
       ASSERT_DBG_FATAL(!PUINFO_RETURN_TO_PARAM || st != PUINFO_RETURN_PARAM, 
		       (DIAG_W2F_DECLARE_RETURN_PARAM, "ST2F_decl_var"));
   }

   /* Declare the variable */
   if (Stab_Is_Common_Block(st))
   {
      /* Declare a common block */
      TY2F_Translate_Common(decl_tokens, st_name, ST_type(st));
   }
   else if (Stab_Is_Equivalence_Block(st))
   {
      if (ST_is_return_var(st))
	 TY2F_Translate_Equivalence(decl_tokens, 
				    ST_type(st), 
				    TRUE /* alternate return point */);
      else
	 TY2F_Translate_Equivalence(decl_tokens, 
				    ST_type(st), 
				    FALSE /* regular equivalence */);
   }
   else if (TY_Is_Pointer(ty_rt) && ST_sclass(st) != SCLASS_FORMAL)
   {
      /* Declare pointee with the name specified in the symbol table */
      pointee_name = W2CF_Symtab_Nameof_St_Pointee(st);
      Append_Token_String(decl_tokens, pointee_name);

      if (TY_ptr_as_array(Ty_Table[ty_rt]))
	 TY2F_translate(decl_tokens, 
			Stab_Array_Of(TY_pointed(ty_rt), 0/*size*/));
      else
	 TY2F_translate(decl_tokens, TY_pointed(ty_rt));

      Append_F77_Indented_Newline(decl_tokens, 1, NULL/*label*/);

      /* Declare the pointer object */
      Append_Token_String(decl_tokens, "POINTER");
      Append_Token_Special(decl_tokens, '(');
      Append_Token_String(decl_tokens, st_name);
      Append_Token_Special(decl_tokens, ',');
      Append_Token_String(decl_tokens, pointee_name);
      Append_Token_Special(decl_tokens, ')');
   }
   else if (ST_sclass(st) == SCLASS_FORMAL && !ST_is_value_parm(st))
   {
      /* ie, regular f77 dummy argument,expect pointer TY      */
      /* To counteract the Fortran call-by-reference semantics */

      ASSERT_DBG_FATAL(TY_Is_Pointer(ty_rt), 
		       (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
			TY_kind(ty_rt), "ST2F_decl_var"));
      Append_Token_String(decl_tokens, st_name);
      if (TY_kind(TY_pointed(ST_type(st))) == KIND_FUNCTION)
      {
	 Prepend_Token_String(decl_tokens, "EXTERNAL");
      }
      else
      {
	 TY_IDX ty;
	 TY_IDX ty1 = TY_pointed(ty_rt);

	 if (TY_Is_Pointer(ty1) && TY_ptr_as_array(Ty_Table[ty1]))
	 {
	    /* Handle ptr as array parameters
	     */
	    ty = Stab_Array_Of(TY_pointed(ty1), 0/*size*/);
	 }
	 else
	 {
	    ty = TY_pointed(ty_rt);
	 }
	 TY2F_translate(decl_tokens, ty);
      }
   }
   else if (ST2F_Is_Dummy_Procedure(st))
   {
       TYLIST tylist_idx = TY_tylist(TY_pointed(ST_type(st)));
       TY_IDX rt = TY_IDX_ZERO;
       if (tylist_idx != (TYLIST) 0)
         rt = TYLIST_type(Tylist_Table[tylist_idx]);

       ST2F_Declare_Return_Type(tokens,rt,ST_name(st));
   }
   else if (ST_sclass(st) == SCLASS_EXTERN &&
	    (strcmp(ST_name(st), "__mp_cur_numthreads") == 0 ||
	     strcmp(ST_name(st), "__mp_sug_numthreads") == 0))
   {
      /* Special case */
      st_name = Concat3_Strings(ST_name(st), "_func", "$");
      Append_Token_String(decl_tokens, st_name);
      TY2F_translate(decl_tokens, ST_type(st));
      Append_F77_Indented_Newline(decl_tokens, 1, NULL/*label*/);
      Append_Token_String(decl_tokens, "EXTERNAL ");
      Append_Token_String(decl_tokens, st_name);
   }
   else
   {
      /* Declare as specified in the symbol table */
      Append_Token_String(decl_tokens, st_name);
      TY2F_translate(decl_tokens, ST_type(st));
   }
   TY2F_Prepend_Structures(decl_tokens);
   Append_And_Reclaim_Token_List(tokens, &decl_tokens);

   /* Save it's value between calls, if so specified, unless it is
    * an equivalence, in which case it is implicitly SAVE.
    */
   if (!Stab_Is_Equivalence_Block(st) &&
       (ST_sclass(st) == SCLASS_FSTATIC || ST_sclass(st) == SCLASS_PSTATIC))
   {
      Append_F77_Indented_Newline(tokens, 1, NULL/*label*/);
      Append_Token_String(tokens, "SAVE");
      Append_Token_String(tokens, st_name);
   }

   /* Generate a DATA statement for initializers */
   if (ST_is_initialized(st) && 
       !Stab_No_Linkage(st)  &&
       (!TY_Is_Structured(ST_type(st)) ||
	Stab_Is_Common_Block(st)       ||
	Stab_Is_Equivalence_Block(st)))
   {
      inito = Find_INITO_For_Symbol(st);
      if (inito != (INITO_IDX) 0)
	 INITO2F_translate(Data_Stmt_Tokens, inito);
   }
} /* ST2F_decl_var */


static void 
ST2F_decl_func(TOKEN_BUFFER tokens, ST *st)
{
   /* This only makes sense for "external" functions in Fortran,
    * while we should not do anything for other functions.
    */
   ASSERT_DBG_FATAL(ST_sym_class(st)==CLASS_FUNC,
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_decl_func"));
   
   /* if f90 internal procedure, don't declare it */

   if (ST_export(st) == EXPORT_LOCAL_INTERNAL)
     return ;

   const char  *func_name = W2CF_Symtab_Nameof_St(st);
   TY_IDX       return_ty;


   /* Specify whether or not the function is EXTERNAL */

   if (ST_sclass(st) == SCLASS_EXTERN)
   {
      Append_Token_String(tokens, "EXTERNAL");
      Append_Token_String(tokens, func_name);
   }

   /* Specify the function return type, unless it is void */

   return_ty = Func_Return_Type(ST_pu_type(st));
   ST2F_Declare_Return_Type(tokens,return_ty,func_name);

} /* ST2F_decl_func */

static void 
ST2F_decl_const(TOKEN_BUFFER tokens, ST *st)
{
   /* A CLASS_CONST symbol never has a name, and as such cannot be
    * declared!
    */
   ASSERT_DBG_FATAL(FALSE, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_decl_const"));
} /* ST2F_decl_const */


/*---------------- hidden routines to handle ST uses ------------------*/
/*---------------------------------------------------------------------*/

static void 
ST2F_use_error(TOKEN_BUFFER tokens, ST *st)
{
   ASSERT_DBG_FATAL(FALSE, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS,
		     ST_sym_class(st), "ST2F_use_error"));
} /* ST2F_use_error */

static void 
ST2F_use_var(TOKEN_BUFFER tokens, ST *st)
{
   TY_IDX return_ty;

   ASSERT_DBG_FATAL(ST_sym_class(st)==CLASS_VAR, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_use_var"));

   /* Note that we do not trust the ST_is_return_var() flag,
    * unless the return_ty is non-void.  This is due to purple,
    * which may change a function into a subroutine.
    */
#ifdef KEY
   // To compile 191.fma3d/relink_scratch_.f90
   if (PUinfo_current_func)
#endif /* KEY */
   return_ty = PUINFO_RETURN_TY;
#ifdef KEY
   // To compile 191.fma3d/relink_scratch_.f90
   else
     return_ty = (TY_IDX) 0;
#endif /* KEY */
   if ((return_ty != (TY_IDX) 0 && 
	TY_kind(return_ty) == KIND_SCALAR && 
	ST_is_return_var(st)) ||
#ifdef KEY       
       // To compile 191.fma3d/relink_scratch_.f90
       (PUinfo_return_preg && PUINFO_RETURN_TO_PARAM && st == PUINFO_RETURN_PARAM))
#else
       (PUINFO_RETURN_TO_PARAM && st == PUINFO_RETURN_PARAM))
#endif /* KEY */
   {
      /* If we have a reference to the implicit return-variable, then
       * refer to the function return value.
       */
      Append_Token_String(tokens, PUINFO_FUNC_NAME);
   }
   else if (ST_keep_name_w2f(st))
   {
      /* Use the name as it is (after making it a legal fortran name)
       * and do not mark this variable as having been referenced.
       * Assume this a special symbol not to be declared.
       */
      Append_Token_String(tokens, 
			  WHIRL2F_make_valid_name(ST_name(st),WN2F_F90_pu && !ST_is_temp_var(st)));
   }
   else if (Stab_Is_Based_At_Common_Or_Equivalence(st))
   {
      /* Reference the corresponding field in the common block (we do this
       * only to ensure that the name referenced matches the one used for
       * the member of the common-block at the place of declaration).  Note
       * that will full splitting, the original common block can be found
       * at ST_full(ST_base(st)).
       */
      WN2F_CONTEXT context = INIT_WN2F_CONTEXT;

      WN2F_Offset_Symref(tokens, 
			 ST_base(st),                         /* base-symbol */
			 Stab_Pointer_To(ST_type(ST_base(st))), /* base-type */
			 ST_type(st),                         /* object-type */
		         ST_ofst(st),                         /* object-ofst */
		         context);
      Set_BE_ST_w2fc_referenced((ST *)ST_base(st));
   }
   else if (ST_sclass(st) == SCLASS_EXTERN &&
	    (strcmp(ST_name(st), "__mp_cur_numthreads") == 0 ||
	     strcmp(ST_name(st), "__mp_sug_numthreads") == 0))
   {
      /* Special case */
      Append_Token_String(tokens, Concat3_Strings(ST_name(st), "_func", "$"));
      Append_Token_Special(tokens, '(');
      Append_Token_Special(tokens, ')');
      Set_BE_ST_w2fc_referenced(st);
   }
   else
   {
      Append_Token_String(tokens, W2CF_Symtab_Nameof_St(st));
      Set_BE_ST_w2fc_referenced(st);
   }
} /* ST2F_use_var */


static void 
ST2F_use_func(TOKEN_BUFFER tokens, ST *st)
{
   ASSERT_DBG_FATAL(ST_sym_class(st)==CLASS_FUNC, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_use_func"));

   Append_Token_String(tokens, W2CF_Symtab_Nameof_St(st));
   Set_BE_ST_w2fc_referenced(st);
}

static void 
ST2F_use_const(TOKEN_BUFFER tokens, ST *st)
{
   TY_IDX ty_idx = ST_type(st);
   TY& ty = Ty_Table[ty_idx];

   ASSERT_DBG_FATAL(ST_sym_class(st)==CLASS_CONST, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_use_const"));
   
   /* A CLASS_CONST symbol never has a name, so just emit the value.
    */

   if (TY_mtype(ty) == MTYPE_STR && TY_align(ty_idx) > 1)
   {
      /* This must be a hollerith constant */
      TCON2F_hollerith(tokens, STC_val(st));
   }
   else
   {
      TCON2F_translate(tokens, STC_val(st), TY_is_logical(ty));
   }
} /* ST2F_use_const */


static void 
ST2F_use_block(TOKEN_BUFFER tokens, ST *st)
{
  /* with f90 at -O2, CLASS_BLOCK can appear on LDAs etc. in IO */
  /* put out something, so whirlbrowser doesn't fall over       */

   ASSERT_DBG_FATAL(ST_sym_class(st)==CLASS_BLOCK, 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_use_block"));


   Append_Token_String(tokens, ST_name(st));   
} 

/*------------------------ exported routines --------------------------*/
/*---------------------------------------------------------------------*/


void 
ST2F_initialize()
{

  return; 
} /* ST2F_initialize */

void 
ST2F_finalize()
{
  return; 
} 

void 
ST2F_use_translate(TOKEN_BUFFER tokens, ST *st)
{ 
   ST2F_Use_Handler[ST_sym_class(st)](tokens, st);
} 

void 
ST2F_deref_translate(TOKEN_BUFFER tokens, ST *st)
{
   ASSERT_DBG_FATAL(ST_sym_class(st)==CLASS_VAR && 
		    TY_Is_Pointer(ST_type(st)) &&
		    !Stab_Is_Based_At_Common_Or_Equivalence(st), 
		    (DIAG_W2F_UNEXPECTED_SYMCLASS, 
		     ST_sym_class(st), "ST2F_deref_translate"));

   /* Consider this a reference to the pointer value */
   Append_Token_String(tokens, W2CF_Symtab_Nameof_St_Pointee(st));
   Set_BE_ST_w2fc_referenced(st);

} /* ST2F_deref_translate */

// dispatch handler for declarations..
void 
ST2F_decl_translate(TOKEN_BUFFER tokens, const ST *st)
{ 
   ST2F_Decl_Handler[ST_sym_class(st)](tokens, (ST *) st);
} 

void
ST2F_func_header(TOKEN_BUFFER tokens,
		 ST          *st,          /* Function ST entry     */
		 ST         **params,      /* Array of  parameters  */
		 INT32        num_params,  /* # of parms in array   */
		 BOOL         is_altentry) /* Alternate entry point */
{
   /* Emit the header for a function definition or an alternate entry
    * point.  Note that the resultant token buffer will not have 
    * appended a newline after the function header.
    */
   TOKEN_BUFFER header_tokens = New_Token_Buffer();
   INT          param, first_param, implicit_parms = 0;
   TY_IDX       funtype = ST_pu_type(st);
   TY_IDX       return_ty;

   ASSERT_DBG_FATAL(TY_kind(funtype) == KIND_FUNCTION,
		    (DIAG_W2F_UNEXPECTED_SYMBOL, "ST2F_func_header"));

   return_ty = Func_Return_Type(funtype);

   /* Append the function name */

   Append_Token_String(header_tokens, W2CF_Symtab_Nameof_St(st));

   /* Emit the parameter name-list, if one is present, and skip any
    * implicit "length" parameters associated with character strings.
    * Such implicit parameters should be at the end of the parameter list.
    */
   first_param = ST2F_FIRST_PARAM_IDX(funtype);
   if (params[first_param] != NULL)
   {
      Append_Token_Special(header_tokens, '(');
      for (param = first_param; 
	   param < num_params - implicit_parms; 
	   param++)
      {
	 
	 Append_Token_String(header_tokens, 
			     W2CF_Symtab_Nameof_St(params[param]));

	 if (STAB_PARAM_HAS_IMPLICIT_LENGTH(params[param])) 
	 {
	    implicit_parms++;

	    /* is function returning character_TY? if length follows    */
	    /* address - skip over it, but account for ',' in arg list  */

	    if ((param == first_param) && (params[param+1] != NULL)) 
	    {
	      if (ST_is_value_parm(params[param]) && ST_is_value_parm(params[param+1])) 
	      {
		if (return_ty != (TY_IDX) 0 && TY_kind(return_ty) == KIND_VOID) 
		{
		  param ++ ;
		  params[param] = NULL; 
		  implicit_parms--;
		}
	      }
            }
	 }

	 if (param+implicit_parms+1 < num_params)
	    Append_Token_Special(header_tokens, ',');
      }
      Append_Token_Special(header_tokens, ')');
   }
   else if (!PU_is_mainpu(Get_Current_PU()))
   {
      /* Use the "()" notation for "no parameters", except for
       * the main program definition.
       */
      Append_Token_Special(header_tokens, '(');
      Append_Token_Special(header_tokens, ')');
   }
   
   /* Prepend one of the keywords ENTRY, PROGRAM, FUNCTION, or
    * SUBROUTINE to the function name, as is appropriate.
    */

   if (PU_is_mainpu(Get_Current_PU()))
   {
      Prepend_Token_String(header_tokens, "PROGRAM");
   }
   else if (return_ty != (TY_IDX) 0 && TY_kind(return_ty) != KIND_VOID)
   {
     if (is_altentry)
       Prepend_Token_String(header_tokens, "ENTRY");
     else
     {
       Prepend_Token_String(header_tokens, "FUNCTION");
       
       /* Note that we cannot have functions returning pointer types
	* in Fortran, so we use the corresponding integral type
	* instead.
	*/

       if (TY_Is_Pointer(return_ty))
	 TY2F_translate(header_tokens, 
			Stab_Mtype_To_Ty(TY_mtype(return_ty)));
       else
	 TY2F_translate(header_tokens, return_ty);
     }
   }
   else /* subroutine */
   {
      if (is_altentry)
	 Prepend_Token_String(header_tokens, "ENTRY");
      else
	 Prepend_Token_String(header_tokens, "SUBROUTINE");
   }
   
   if (!is_altentry)
   {
      /* Emit parameter declarations, indented and on a new line */
      Append_F77_Indented_Newline(header_tokens, 1, NULL/*label*/);
      Append_Token_String(header_tokens, "IMPLICIT NONE");

      for (param = first_param; param < num_params -implicit_parms; param++)
      {
	 Append_F77_Indented_Newline(header_tokens, 1, NULL/*label*/);
	 if (params[param] != NULL)
	   ST2F_decl_translate(header_tokens, params[param]);
      }
   }

   Append_Token_Special(tokens, '\n');
   Append_F77_Indented_Newline(tokens, 0, NULL);
   Append_And_Reclaim_Token_List(tokens, &header_tokens);
} /* ST2F_func_header */

void
ST2F_Use_Preg(TOKEN_BUFFER tokens,
	      TY_IDX       preg_ty,
	      PREG_IDX     preg_idx)
{
   /* Append the name of the preg to the token-list and declare the
    * preg in the current PU context unless it is already declared.
    */
   const char *preg_name;

   preg_ty = PUinfo_Preg_Type(preg_ty, preg_idx);
   preg_name = W2CF_Symtab_Nameof_Preg(preg_ty, preg_idx);

   /* Declare the preg, if it has not already been declared */
   if (!PUinfo_Is_Preg_Declared(preg_ty, preg_idx))
   {
      ST2F_Define_Preg(preg_name, preg_ty);
      PUinfo_Set_Preg_Declared(preg_ty, preg_idx);
   }

   Append_Token_String(tokens, preg_name);
} /* ST2F_Use_Preg */

void 
ST2F_Declare_Tempvar(TY_IDX ty, UINT idx)
{
   TOKEN_BUFFER tmp_tokens = New_Token_Buffer();
   UINT         current_indent = Current_Indentation();

   Set_Current_Indentation(PUinfo_local_decls_indent);
   Append_F77_Indented_Newline(PUinfo_local_decls, 1, NULL/*label*/);
   if (TY_Is_Pointer(ty))
   {
      /* Assume we never need to dereference the pointer, or else we
       * need to maintain a map from tmp_idx->pointee_idx (new temporary
       * for pointee_idx), so declare this temporary variable to be of
       * an integral type suitable for a pointer value.
       */
      ty = Stab_Mtype_To_Ty(Pointer_Mtype);
   }
   Append_Token_String(tmp_tokens, W2CF_Symtab_Nameof_Tempvar(idx)); /* name */
   TY2F_translate(tmp_tokens, ty);                                   /* type */
   Append_And_Reclaim_Token_List(PUinfo_local_decls, &tmp_tokens);
   Set_Current_Indentation(current_indent);
} /* ST2F_Declare_Tempvar */


static BOOL
ST2F_Is_Dummy_Procedure(ST *st)
{
  /* Does this ST represent a dummy procedure ? */

  BOOL dummy = FALSE;

  if (ST_sclass(st) == SCLASS_FORMAL && ST_is_value_parm(st))
  {
      TY_IDX ty = ST_type(st);

      if (TY_kind(ty) == KIND_POINTER)
	if (TY_kind(TY_pointed(ty)) == KIND_FUNCTION)
	  dummy = TRUE ;
  }
  return dummy ;
}


static void
ST2F_Declare_Return_Type(TOKEN_BUFFER tokens,TY_IDX return_ty, const char *name)
{
  /* The TY represents a dummy procedure or a function return type */

  if (return_ty != (TY_IDX) 0) 
  {
    if (TY_kind(return_ty) != KIND_VOID)
    {
	TOKEN_BUFFER decl_tokens = New_Token_Buffer();
	
	Append_F77_Indented_Newline(tokens, 1, NULL/*label*/);
	Append_Token_String(decl_tokens, name);

	/* Use integral type for pointer returns */

	if (TY_Is_Pointer(return_ty))
	  TY2F_translate(decl_tokens, Stab_Mtype_To_Ty(TY_mtype(return_ty)));
	else
	  TY2F_translate(decl_tokens, return_ty);

	TY2F_Prepend_Structures(decl_tokens);
	Append_And_Reclaim_Token_List(tokens, &decl_tokens);
    }
  }
}








