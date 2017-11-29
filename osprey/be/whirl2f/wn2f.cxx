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
 * Module: wn2f.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f.cxx,v $
 *
 * Revision history:
 *  12-Apr-95 - Original Version
 *
 * Description:
 *
 *   Translate a WN subtree to Fortran by means of an inorder recursive
 *   descent traversal of the WHIRL IR.  Note that the routines to
 *   handle expressions, statements, and loads/stores have been
 *   separated into different source files.  However, the interfaces
 *   to those source-files should only ever be accessed from this file.
 *
 *      WN2F_translate:
 *         Translates an arbitrary WN tree into a sequence of tokens,
 *         appended to the end of the given token-buffer.  The task of
 *         translation will be dispatched to a member in a set of
 *         "handler" routines, and these handler routines should
 *         only be called from this routine.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f.cxx,v $ $Revision: 1.1 $";
#endif

#include <alloca.h>
#include "whirl2f_common.h"
#include "PUinfo.h"          /* From be/whirl2c directory */
#include "wn2f.h"
#include "wn2f_stmt.h"       /* Only used by this module */
#include "wn2f_pragma.h"     /* Only used by this module */
#include "wn2f_expr.h"       /* Only used by this module */
#include "wn2f_load_store.h" /* Only used by this module */
#include "wn2f_io.h"         /* Only used by this module */
#include "st2f.h"
#include "ty2f.h"
#include "tcon2f.h"


const char * sgi_comment_str = "CSGI$ " ;

static BOOL  PU_Need_End_Contains = FALSE;  // f90 needs CONTAINS/END around nested procs.
static BOOL  PU_Dangling_Contains = FALSE;  // f90 have done CONTAINS, need END...
static INT32 PU_Host_Func_Id = 0 ;           // func id for END/CONTAINS

static void WN2F_End_Routine_Strings(TOKEN_BUFFER tokens, INT32 func_id);

/*---------------- Buffers to hold intermediate results ----------------*/
/*----------------------------------------------------------------------*/
 
/* Should be initialized when entering a PU block and reclaimed 
 * when exiting a PU block.
 */
TOKEN_BUFFER Data_Stmt_Tokens = NULL; /* Defined in init2f.c */


/*-------------------- Function handle for each OPR -------------------*/
/*---------------------------------------------------------------------*/

/* Type of handler-functions for translation from WHIRL to Fortran.
 */
typedef WN2F_STATUS (*WN2F_HANDLER_FUNC)(TOKEN_BUFFER, WN*, WN2F_CONTEXT);


/* Declarations of top-level and exceptional handler-functions for 
 * translation from WHIRL to Fortran.  The others are declared through
 * "wn2f_stmt.h", "wn2f_expr.h", and "wn2f_load_store.h".
 */
static WN2F_STATUS 
   WN2F_ignore(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
static WN2F_STATUS 
   WN2F_unsupported(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
static WN2F_STATUS 
   WN2F_func_entry(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
static WN2F_STATUS 
   WN2F_altentry(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
static WN2F_STATUS 
   WN2F_comment(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);


/* WN2F_Handler[] maps an OPR (../common/com/opcode_gen_core.h) to
 * the handler-function that translates it to Fortran.  This table
 * will be dynamically initialized through WN2F_initialize().  This
 * dynamic initialization ensures that the initiated elements of the
 * table are valid regardless of future changes to the OPERATOR
 * enumeration.  Operators not yet correctly handled by whirl2f, and
 * therefore not present in the WN2F_Opr_Handler_list[], will be
 * handled by WN2F_unsupported().
 */
#define NUMBER_OF_OPERATORS (OPERATOR_LAST + 1)
static WN2F_HANDLER_FUNC WN2F_Handler[NUMBER_OF_OPERATORS];


typedef struct WN2F_Opr_Handler
{
   OPERATOR           opr;
   WN2F_HANDLER_FUNC  handler;
} WN2F_OPR_HANDLER;

#define NUMBER_OF_OPR_HANDLERS \
   (sizeof(WN2F_Opr_Handler_List) / sizeof(WN2F_OPR_HANDLER))

static const WN2F_OPR_HANDLER WN2F_Opr_Handler_List[] =
{
   {OPR_FUNC_ENTRY, &WN2F_func_entry},
   {OPR_BLOCK, &WN2F_block},
   {OPR_REGION, &WN2F_region},
   {OPR_REGION_EXIT, &WN2F_goto},
/*   {OPR_SWITCH, &WN2F_switch}, Not a Fortran construct! */
   {OPR_COMPGOTO, &WN2F_compgoto},
   {OPR_DO_LOOP, &WN2F_do_loop},
   {OPR_DO_WHILE, &WN2F_do_while},
   {OPR_WHILE_DO, &WN2F_while_do},
   {OPR_IF, &WN2F_if},
   {OPR_GOTO, &WN2F_goto},
   {OPR_AGOTO, &WN2F_agoto},
   {OPR_ALTENTRY, &WN2F_altentry},
   {OPR_FALSEBR, &WN2F_condbr},
   {OPR_TRUEBR, &WN2F_condbr},
   {OPR_RETURN, &WN2F_return},
   {OPR_RETURN_VAL, &WN2F_return_val},
   {OPR_LABEL, &WN2F_label},
   {OPR_ISTORE, &WN2F_istore},
   {OPR_ISTOREX, &WN2F_istorex},
   {OPR_MSTORE, &WN2F_mstore},
   {OPR_STID, &WN2F_stid},
   {OPR_CALL, &WN2F_call},
   {OPR_INTRINSIC_CALL, &WN2F_intrinsic_call},
   {OPR_ICALL, &WN2F_call},
   {OPR_PICCALL, &WN2F_call},
   {OPR_EVAL, &WN2F_eval},
   {OPR_PREFETCH, &WN2F_prefetch},
   {OPR_PREFETCHX, &WN2F_prefetch},
   {OPR_PRAGMA, &WN2F_pragma},
   {OPR_XPRAGMA, &WN2F_pragma},
   {OPR_IO, &WN2F_io},
   {OPR_COMMENT, &WN2F_comment},
   {OPR_ILOAD, &WN2F_iload},
   {OPR_ILOADX, &WN2F_iloadx},
   {OPR_MLOAD, &WN2F_mload},
   {OPR_ARRAY, &WN2F_array},
   {OPR_INTRINSIC_OP, &WN2F_intrinsic_op},
   {OPR_TAS, &WN2F_tas},
   {OPR_SELECT, &WN2F_select},
   {OPR_CVT, &WN2F_cvt},
   {OPR_CVTL, &WN2F_cvtl},
   {OPR_NEG, &WN2F_unaryop},
   {OPR_ABS, &WN2F_unaryop},
   {OPR_SQRT, &WN2F_unaryop},
   {OPR_REALPART, &WN2F_realpart},
   {OPR_IMAGPART, &WN2F_imagpart},
   {OPR_PAREN, &WN2F_paren},
   {OPR_RND, &WN2F_unaryop},
   {OPR_TRUNC, &WN2F_unaryop},
   {OPR_CEIL, &WN2F_ceil},
   {OPR_FLOOR, &WN2F_floor},
   {OPR_BNOT, &WN2F_unaryop},
   {OPR_LNOT, &WN2F_unaryop},
   {OPR_ADD, &WN2F_binaryop},
   {OPR_SUB, &WN2F_binaryop},
   {OPR_MPY, &WN2F_binaryop},
   {OPR_DIV, &WN2F_binaryop},
   {OPR_MOD, &WN2F_binaryop},
   {OPR_REM, &WN2F_binaryop},
   {OPR_MAX, &WN2F_binaryop},
   {OPR_MIN, &WN2F_binaryop},
   {OPR_BAND, &WN2F_binaryop},
   {OPR_BIOR, &WN2F_binaryop},
   {OPR_BNOR, &WN2F_bnor},
   {OPR_BXOR, &WN2F_binaryop},
   {OPR_LAND, &WN2F_binaryop},
   {OPR_LIOR, &WN2F_binaryop},
   {OPR_CAND, &WN2F_binaryop},
   {OPR_CIOR, &WN2F_binaryop},
   {OPR_SHL, &WN2F_binaryop},
   {OPR_ASHR, &WN2F_ashr},
   {OPR_LSHR, &WN2F_lshr},
   {OPR_COMPLEX, &WN2F_complex},
   {OPR_RECIP, &WN2F_recip},
   {OPR_RSQRT, &WN2F_rsqrt},
   {OPR_MADD, &WN2F_madd},
   {OPR_MSUB, &WN2F_msub},
   {OPR_NMADD, &WN2F_nmadd},
   {OPR_NMSUB, &WN2F_nmsub},
   {OPR_EQ, &WN2F_eq},
   {OPR_NE, &WN2F_ne},
   {OPR_GT, &WN2F_binaryop},
   {OPR_GE, &WN2F_binaryop},
   {OPR_LT, &WN2F_binaryop},
   {OPR_LE, &WN2F_binaryop},
   {OPR_LDID, &WN2F_ldid},
   {OPR_LDA, &WN2F_lda},
   {OPR_CONST, &WN2F_const},
   {OPR_INTCONST, &WN2F_intconst},
   {OPR_PARM, &WN2F_parm},
   {OPR_TRAP, &WN2F_ignore},
   {OPR_ASSERT, &WN2F_ignore},
   {OPR_FORWARD_BARRIER, &WN2F_ignore},
   {OPR_BACKWARD_BARRIER, &WN2F_ignore},
   {OPR_ALLOCA, &WN2F_alloca},
   {OPR_DEALLOCA, &WN2F_dealloca}
}; /* WN2F_Opr_Handler_List */


/*------------------ Statement newline directives ----------------------*/
/*----------------------------------------------------------------------*/

void 
WN2F_Stmt_Newline(TOKEN_BUFFER tokens,
		  const char  *label,
		  SRCPOS       srcpos,
		  WN2F_CONTEXT context)
{
   if (WN2F_CONTEXT_no_newline(context))
   {
      if (W2F_File[W2F_LOC_FILE] != NULL)
	 Append_Srcpos_Map(tokens, srcpos);
   }
   else
   {
      if (W2F_Emit_Linedirs)
	 Append_Srcpos_Directive(tokens, srcpos);
      Append_F77_Indented_Newline(tokens, 1, label);
      if (W2F_File[W2F_LOC_FILE] != NULL)
	 Append_Srcpos_Map(tokens, srcpos);
   }
} /* WN2F_Stmt_Newline */


/*------------ Translation of addressing and dereferencing -------------*/
/*----------------------------------------------------------------------*/

/* just used to maintain the state of the recursions when */
/* marking FLDs in nested addresses                       */

class LOC_INFO{

private:
  FLD_PATH_INFO * _flds_left;   /* points to tail of fld_path */
  STAB_OFFSET _off;             /* offset of last FLD used in fld_path */
  BOOL   _base_is_array;        /* was ST of address an array? */

public:
  WN * _nested_addr;

  LOC_INFO(FLD_PATH_INFO * path) {
    _flds_left = path;

    _off  = 0;
    _nested_addr = NULL;
    _base_is_array = FALSE ;
  }

  void WN2F_Find_And_Mark_Nested_Address(WN * addr);
};

void LOC_INFO::
WN2F_Find_And_Mark_Nested_Address(WN * addr)
{
  /* If this address expression contains nested ARRAY nodes */
  /* (and isn't a character expression), the ARRAYs refer   */
  /* to structure components, eg: aaa(1).kkk(3) yields      */
  /* ARRAY(ADD(const,ARRAY(LDA)). Add a pointer to the      */
  /* array elements of the fld path, associating each with  */
  /* corresponding OPC_ARRAY. TY2F_Translate_Fld_Path will  */
  /* write the subscript list.                              */


  /* In general, just the lowest LDID/LDA remains to be     */
  /* processed, however if the lowest ARRAY node is not a   */
  /* fld, and belongs to the address ST, then return that   */
  /* ARRAY.                                                 */

  switch (WN_operator(addr))
  {
  case OPR_ARRAY: 
    {
      WN * kid = WN_kid0(addr);
      WN2F_Find_And_Mark_Nested_Address(kid);
      if ((_flds_left && _flds_left->arr_elt) &&
	  (!(_base_is_array)))
      {
	_flds_left-> arr_wn = addr;
	_flds_left = TY2F_Point_At_Path(_flds_left,_off);
      } 
      else 
	_nested_addr = addr;

      _base_is_array = FALSE;
    }
    break;

  case OPR_ADD:
    {
      WN * cnst = WN_kid0(addr);
      WN * othr = WN_kid1(addr);

      if (WN_operator(cnst) != OPR_INTCONST) 
      {
	cnst = WN_kid1(addr);
	othr = WN_kid0(addr);
      }
      WN2F_Find_And_Mark_Nested_Address(othr);
      _off = WN_const_val(cnst);
      _flds_left = TY2F_Point_At_Path(_flds_left,_off);
      _base_is_array = FALSE;
    }
    break;

  case OPR_LDID:
    _off = 0;
    _nested_addr = addr;
    _flds_left = TY2F_Point_At_Path(_flds_left,_off);
    _base_is_array = ((TY_kind(WN_ty(addr)) == KIND_POINTER) && 
		      (TY_kind(TY_pointed(WN_ty(addr))) == KIND_ARRAY));
    break;

  case OPR_LDA:
    _off = WN_lda_offset(addr);
    _nested_addr = addr;
    _flds_left = TY2F_Point_At_Path(_flds_left,_off);
    _base_is_array = ((TY_kind(WN_ty(addr)) == KIND_POINTER) && 
		      (TY_kind(TY_pointed(WN_ty(addr))) == KIND_ARRAY));
    break;

  case OPR_ILOAD:
    _off = 0;
    _nested_addr = addr;
    _flds_left = TY2F_Point_At_Path(_flds_left,0);
    _base_is_array = ((TY_kind(WN_ty(addr)) == KIND_POINTER) && 
		      (TY_kind(TY_pointed(WN_ty(addr))) == KIND_ARRAY));
    break;

  default:
    ASSERT_WARN((0),
		(DIAG_W2F_UNEXPECTED_OPC,"WN2F_Find_And_Mark_Nested_Address"));

    break;
  }
  return;
}


extern WN_OFFSET
WN2F_Sum_Offsets(WN *addr)
{
  /* Accumulate any offsets (ADDs) in this address   */
  /* tree. Used for computing Fld paths              */

  BOOL sum = 0;

  switch (WN_operator(addr))
  {
    case OPR_ARRAY: 
    sum += WN2F_Sum_Offsets(WN_kid0(addr));
    break;

    case OPR_ADD:
    sum += WN2F_Sum_Offsets(WN_kid0(addr));
    sum += WN2F_Sum_Offsets(WN_kid1(addr));
    break;

    case OPR_INTCONST:
    sum = WN_const_val(addr);
    break;
  }
  return sum;
}


void 
WN2F_Address_Of(TOKEN_BUFFER tokens)
{
   Prepend_Token_Special(tokens, '(');
   Prepend_Token_String(tokens, "loc%");
   Append_Token_Special(tokens, '(');
} /* WN2F_Address_Of */

WN2F_STATUS
WN2F_Offset_Symref(TOKEN_BUFFER tokens, 
		   ST          *st,         /* base-symbol */
                   TY_IDX       addr_ty,    /* expected base-address type */
                   TY_IDX       object_ty,  /* object type */
		   STAB_OFFSET  offset,     /* offset from base-address */
		   WN2F_CONTEXT context)
{
   /* Given a symbol and an offset within the location of the symbol,
    * append a Fortran expression to "tokens" to access an object
    * of the given "object_ty" at this location.  
    *
    * The base symbol will unconditionally be treated as having an
    * lvalue (address) type as given by "addr_ty", except when "deref"
    * is TRUE, when the rvalue of the base-symbol is assumed to have
    * the "addr_ty" and must either explicitly (for POINTER variables) 
    * or implicitly (for pass by reference arguments) be dereferenced. 
    * Note that for a compatible base-type and object-type, this is simply
    * a reference to the given ST_name();  in all other cases we expect 
    * the object_ty to be a field (FLD) within the base-type (KIND_STRUCT)
    * or an offset within an array.
    *
    * Note that we must have special handling for common-blocks and
    * equivalences.  Note that "addr_ty" may be different from
    * "Stab_Pointer_To(ST_type(st))", both for "deref" cases and 
    * ptr_as_array variables.
    */
   TY_IDX       base_ty = TY_pointed(addr_ty);
   const BOOL deref_val = WN2F_CONTEXT_deref_addr(context);
   BOOL       deref_fld;
   void     (*translate_var_ref)(TOKEN_BUFFER, ST *);

#ifdef __USE_COMMON_BLOCK_NAME__
   /* Do the symbol translation from the base of BASED symbols */
   if (Stab_Is_Based_At_Common_Or_Equivalence(st))
   {
      offset += ST_ofst(st); /* offset of based symbol */
      st = ST_base(st);      /* replace based symbol with its base */
      base_ty = ST_type(st);
      addr_ty = Stab_Pointer_To(base_ty);
      Set_BE_ST_w2fc_referenced(st);
   }

   /* Do the symbol translation from the base of fully split common symbols */
   if (ST_is_split_common(st))
   {
      Clear_BE_ST_w2fc_referenced(st);       /* don't put out split base, just user COMMON */
      st = ST_full(st);
      Set_BE_ST_w2fc_referenced(st);
      base_ty = ST_type(st);
      addr_ty = Stab_Pointer_To(base_ty);
   }
#endif 

   /* Select variable-reference translation function */
   if (deref_val                      && 
       ST_sclass(st) != SCLASS_FORMAL && 
       TY_Is_Pointer(ST_type(st)))
   {
      /* An explicitly dereference */
      translate_var_ref = &ST2F_deref_translate;
   }
   else
   {
      /* A direct reference or an implicit dereference */
      translate_var_ref = &ST2F_use_translate;
   }

   if (WN2F_Can_Assign_Types(base_ty, object_ty) || 
       (TY_kind(base_ty) == KIND_FUNCTION &&
	TY_kind(base_ty) == TY_kind(object_ty) &&
        TY_kind(object_ty) != KIND_STRUCT))
   {
      /* Since the types are compatible, we cannot have an offset
       * into one of the objects.  Simply generate a reference to
       * the symbol.
       */
      ASSERT_WARN(offset==0, (DIAG_W2F_UNEXPEXTED_OFFSET,
			      offset, "WN2F_Offset_Symref"));
      translate_var_ref(tokens, st);
   }
   else if (TY_Is_Array(base_ty))
   {
      ASSERT_DBG_WARN(WN2F_Can_Assign_Types(TY_AR_etype(base_ty), object_ty),
		      (DIAG_W2F_INCOMPATIBLE_TYS, "WN2F_Offset_Symref"));

      if (TY_Is_Character_String(base_ty))
      {
	 Append_Token_String(tokens, "ichar");
	 Append_Token_Special(tokens, '(');
	 translate_var_ref(tokens, st);
	 TY2F_Translate_ArrayElt(tokens, base_ty, offset);
	 Append_Token_Special(tokens, ')');
      }
      else
      {
	 translate_var_ref(tokens, st);
	 TY2F_Translate_ArrayElt(tokens, base_ty, offset);
      }
   }
   else /* incompatible base and object types */
   {
      FLD_PATH_INFO *fld_path;
      
      /* Get the path to the (undereferenced) object type.
       *
       * The following should no longer be necessary, since the
       * appropriate dereferencing of a field should happen in the 
       * translation of the field-path, not through a change of the
       * object type in the context.
       *
       *   if (deref)
       *     fld_path = 
       *        TY2F_Get_Fld_Path(base_ty, Stab_Pointer_To(object_ty), offset);
       *    else
       */

      /* We only dereference a field when the base need not be 
       * dereferenced.  We never need to have both dereferenced, 
       * since pointers cannot occur in RECORDS and common/
       * equivalence blocks cannot be referenced through pointer 
       * identifiers.
       */
      deref_fld = (deref_val && !TY_Is_Pointer(ST_type(st)))? TRUE : FALSE;
      if (deref_fld)
	 object_ty = Stab_Pointer_To(object_ty);

      fld_path = TY2F_Get_Fld_Path(base_ty, object_ty, offset);

      if (fld_path == NULL)
      {
	/* return vars for entry points may have equivalence classes  */
	/* without anything at offset 0. Just put out the ST of the   */
        /* return variable, as we don't put out the equivalence group */

	if (ST_is_return_var(st)) 
	 (void)translate_var_ref(tokens, st);
	else
	{
	  ASSERT_DBG_WARN(FALSE, 
			  (DIAG_W2F_NONEXISTENT_FLD_PATH,
			   "WN2F_Offset_Symref"));
	  Append_Token_String(tokens, "SOMEWHERE_IN");
	  Append_Token_Special(tokens, '(');
	  (void)translate_var_ref(tokens, st);
	  Append_Token_Special(tokens, ')');
         }
      }
      else
      {
	 if (!Stab_Is_Common_Block(st) && !Stab_Is_Equivalence_Block(st))
	 {
	    /* Base the path at the st object, and separate it from 
	     * the remainder of the path with the field selection 
	     * operator ('.').
	     */
	    (void)translate_var_ref(tokens, st);
	    Append_Token_Special(tokens, WN2F_F90_pu ? '%' : '.');
	 }
	 if (Stab_Is_Equivalence_Block(st) &&
	     (ST_is_return_var(st) ||
	      (PUinfo_current_func != NULL && 
                (PUINFO_RETURN_TO_PARAM && st == PUINFO_RETURN_PARAM))))
	    TY2F_Translate_Fld_Path(tokens, fld_path, 
				    deref_fld,FALSE, TRUE,context);
	 else
	    TY2F_Translate_Fld_Path(tokens, fld_path, 
				    deref_fld, 
				    (Stab_Is_Common_Block(st) || 
				     Stab_Is_Equivalence_Block(st)),
				    FALSE/*as_is*/,
				    context);

	 TY2F_Free_Fld_Path(fld_path);
      } /* if (the field was found) */
   } /* if (base-type is compatible with object-type */
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_Offset_Symref */


WN2F_STATUS
WN2F_Offset_Memref(TOKEN_BUFFER tokens, 
		   WN          *addr,       /* base-address expression */
                   TY_IDX       addr_ty,    /* expected base-address type */
                   TY_IDX       object_ty,  /* object type */
		   STAB_OFFSET  offset,     /* offset from base-address */
		   WN2F_CONTEXT context)
{
   /* Given an address expression and an offset from this address,
    * append a Fortran expression to "tokens" to access an object
    * of the given "object_ty" at this offset address.  I.e. this
    * is a dereferencing operation on the base-address. The resultant
    * value (e.g. after a struct-field access) may be further
    * dereferenced.
    *
    * The address expression is unconditionally treated as an expression
    * of the addr_ty.
    *
    * For non-zero offsets, or when "!WN2F_Can_Assign_Types(object_ty,
    * TY_pointed(addr_ty))", we expect the base-address to denote the
    * address of a structure or an array, where an object of the given 
    * object_ty can be found at the given offset.
    *
    * Since Fortran does not have an explicit (only implicit) dereference
    * operation we cannot first calculate the address and then 
    * dereference. This constrains the kind of expression we may handle
    * here.  Note that equivalences and common-blocks always should be 
    * accessed through an LDID or an LDA(?) node.
    */

   const BOOL deref_fld = WN2F_CONTEXT_deref_addr(context);

   /* Prepare to dereference the base-address expression */
   set_WN2F_CONTEXT_deref_addr(context);

   if (WN2F_Is_Address_Preg(addr,addr_ty))
   {
     /* Optimizer may put address PREGS into ARRAYs */
     /* and high level type is more or less useless */
     /* just go with WN tree ADDs etc.              */

     (void)WN2F_translate(tokens, addr, context);

     if (offset != 0)
     {
       Append_Token_Special(tokens, '+');
       Append_Token_String(tokens, Number_as_String(offset, "%lld"));
     }
   }
   else 
   {
     TY_IDX base_ty = TY_pointed(addr_ty);

     if (WN2F_Can_Assign_Types(base_ty, object_ty))
     {
      /* Since the types are compatible, we cannot have an offset
       * into one of the objects, and we further dispatch the task
       * of translation.
       */

      ASSERT_WARN(offset==0, (DIAG_W2F_UNEXPEXTED_OFFSET,
			      offset, "WN2F_Offset_Memref"));

      (void)WN2F_translate(tokens, addr, context);
     }
     else /* Accessing a field in a record or an element of an array */
     {
       if (TY_Is_Array(base_ty))
       {
	 ASSERT_DBG_WARN(WN2F_Can_Assign_Types(TY_AR_etype(base_ty), 
					       object_ty),
			 (DIAG_W2F_INCOMPATIBLE_TYS, "WN2F_Offset_Memref"));

	 if (TY_Is_Character_String(base_ty))
	 {
	    Append_Token_String(tokens, "ichar");
	    Append_Token_Special(tokens, '(');
	    (void)WN2F_translate(tokens, addr, context); /* String lvalue */
	    TY2F_Translate_ArrayElt(tokens, base_ty, offset);
	    Append_Token_Special(tokens, ')');
	 }
	 else
	 {
	    (void)WN2F_translate(tokens, addr, context); /* Array lvalue */
	    TY2F_Translate_ArrayElt(tokens, base_ty, offset);
	 }
       }
       else if ((WN_opc_operator(addr) == OPR_LDA || 
		 WN_opc_operator(addr) == OPR_LDID) &&
		(TY_kind(base_ty) != KIND_STRUCT) &&
		(Stab_Is_Common_Block(WN_st(addr)) || 
		 Stab_Is_Equivalence_Block(WN_st(addr))))
       {
	 /* A common-block or equivalence-block, both of which we handle
	  * only in WN2F_Offset_Symref().
	  */

	 ASSERT_WARN(WN2F_Can_Assign_Types(ST_type(WN_st(addr)), base_ty) ,
		     (DIAG_W2F_INCOMPATIBLE_TYS, "WN2F_Offset_Symref"));

	 if (WN_opc_operator(addr) == OPR_LDA)
	   reset_WN2F_CONTEXT_deref_addr(context);
	 (void)WN2F_Offset_Symref(tokens, 
				  WN_st(addr),
				  addr_ty,                      /* base */
				  object_ty,                    /* object */
				  offset + WN_lda_offset(addr), /* offset */
				  context);
       }
       else /* Neither common-block nor equivalence field-access */
       {
	 /* Find the path to the field we wish to access and append
	  * this path to the base-object reference.
	  */

	 FLD_PATH_INFO *fld_path;

	 /* Get any offset given by an address ADDition node.  The type
	  * of the addition, as given by WN_Tree_Type(), is the type
	  * of base-object within which we are accessing, so the addr_ty
	  * is already set up correctly to handle the combined offsets.
	  */

         WN_OFFSET tmp = WN2F_Sum_Offsets(addr);
         if (tmp < TY_size(TY_pointed(addr_ty)))
         offset += tmp;

	 fld_path = TY2F_Get_Fld_Path(base_ty, object_ty, offset);
	 ASSERT_DBG_WARN(fld_path != NULL,
			 (DIAG_W2F_NONEXISTENT_FLD_PATH, 
			  "WN2F_Offset_Memref"));


	 /* May have ARRAY(ADD(ARRAY(LDA),CONST)) or some such. */
	 /* The deepest ARRAY (with the address) is handled     */
	 /* by the WN2F_array processing, but the others        */
	 /* are field references with array components.         */
	 
	 LOC_INFO det(fld_path);
	 det.WN2F_Find_And_Mark_Nested_Address(addr);
	 addr = det._nested_addr;

	 /* Get the base expression to precede the path */

	 (void)WN2F_translate(tokens, addr, context);
	 TY2F_Fld_Separator(tokens);

	 /* Append the path-name, perhaps w/o array subscripts. */

	 if (fld_path != NULL)
         {
	   TY2F_Translate_Fld_Path(tokens, 
				   fld_path, 
				   deref_fld, 
				   FALSE/*common*/,
				   FALSE/*as_is*/,
				   context);
					 
	   TY2F_Free_Fld_Path(fld_path);
	 }
	 else
         {
	   Append_Token_String(tokens, 
			       Number_as_String(offset, 
						"<field-at-offset=%lld>"));
	 }
       } /* if (neither common-block nor equivalence field-access */
     } /* if (object_ty is incompatible with base_ty) */
   } /* else */

   return EMPTY_WN2F_STATUS;
} /* WN2F_Offset_Memref */

/*---------------- Translation of function entry points ----------------*/
/*----------------------------------------------------------------------*/

void
WN2F_Entry_Point(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* This will translate an alternate or function entry point with
    * parameter declarations into Fortran.  Note that the 
    * PUinfo_current_func will not change as a result of this call.
    *
    */
   ST    **param_st;
   INT     param, num_formals;

   ASSERT_DBG_FATAL(WN_opcode(wn) == OPC_ALTENTRY || 
		    WN_opcode(wn) == OPC_FUNC_ENTRY,
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_Entry_Point"));

   if (WN_opcode(wn) == OPC_ALTENTRY)
      num_formals = WN_kid_count(wn);
   else
      num_formals = WN_num_formals(wn);

   /* Accumulate the parameter ST entries */
   param_st = (ST **)alloca((num_formals + 1) * sizeof(ST *));
   for (param = 0; param < num_formals; param++)
   {
      param_st[param] = WN_st(WN_formal(wn, param));
   }
   /* Terminate the list of parameter STs */
   param_st[num_formals] = NULL;

   /* Write out the entry point with parameter declarations 
    * on a new line.
    */
   //   WN2F_Stmt_Newline(tokens, NULL/*label*/, WN_Get_Linenum(wn), context);
   ST2F_func_header(tokens, 
		    &St_Table[WN_entry_name(wn)], 
		    param_st, 
		    num_formals,
		    WN_opcode(wn) == OPC_ALTENTRY);

} /* WN2F_Entry_Point */


/*--------- The operator handlers implemented in this module ----------*/
/*---------------------------------------------------------------------*/


static WN2F_STATUS
WN2F_ignore(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   return EMPTY_WN2F_STATUS;
} /* WN2F_ignore */


static WN2F_STATUS
WN2F_unsupported(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* Warn about opcodes we cannot translate, but keep translating.
    */
   ASSERT_WARN(FALSE,
	       (DIAG_W2F_CANNOT_HANDLE_OPC, WN_opc_name(wn), WN_opcode(wn)));

   WN2F_Stmt_Newline(tokens, NULL/*label*/, WN_Get_Linenum(wn), context);
   Append_Token_String(tokens, Concat3_Strings("<", WN_opc_name(wn), ">"));
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_unsupported */


static WN2F_STATUS
WN2F_func_entry(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* Add tokens for the function header and body to "tokens".  Note
    * that the whole function definition will appended to the buffer,
    * while the task of writing the tokens to file and freeing up
    * the buffer is left to the caller.
    *
    * Assume that Current_Symtab has been updated (see bedriver.c).
    * Note that Current_PU is not maintained, but we instead get to
    * it through PUinfo_current_func.
    *
    */
   INT32 func_id = 0;

   ASSERT_DBG_FATAL(WN_opcode(wn) == OPC_FUNC_ENTRY, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_func_entry"));
   
   /* For local declarations, set the indentation to the current
    * indentation */

   PUinfo_local_decls_indent = Current_Indentation();

   /* Translate the function header */
   WN2F_Entry_Point(tokens, wn, context);
   
   /* Emit the function pragmas before local variables */
   if (!W2F_No_Pragmas)
      WN2F_pragma_list_begin(PUinfo_pragmas, 
                             WN_first(WN_func_pragmas(wn)),
                             context);
   
   set_WN2F_CONTEXT_new_pu(context);
   (void)WN2F_translate(tokens, WN_func_body(wn), context);

   /* While necessary for regions, we probably need not end any pragmas
    * in a func_entry, but we make the call anyway;  for consistency.
    */
   if (!W2F_No_Pragmas)
      WN2F_pragma_list_end(tokens,
                           WN_first(WN_func_pragmas(wn)),
                           context);

   WN2F_Stmt_Newline(tokens,NULL, WN_Get_Linenum(wn), context); 

   WN2F_End_Routine_Strings(tokens,func_id);

   return EMPTY_WN2F_STATUS;
 }

WN2F_STATUS 
WN2F_altentry(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* This is very similar to a func_entry, but without the function
    * body.
    */
   ASSERT_DBG_FATAL(WN_opcode(wn) == OPC_ALTENTRY,
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_altentry"));
   
   /* Translate the function entry point */
   WN2F_Entry_Point(tokens, wn, context);

   return EMPTY_WN2F_STATUS;
} /* WN2F_altentry */


WN2F_STATUS 
WN2F_comment(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opcode(wn) == OPC_COMMENT,
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_comment"));
   
   /* Avoid comments with special interpretation
    */
   if (strcmp(Index_To_Str(WN_GetComment(wn)), "ENDLOOP") != 0)
   {
      Append_F77_Comment_Newline(tokens, 1/*empty-lines*/, TRUE/*indent*/);
      Append_Token_String(tokens, Index_To_Str(WN_GetComment(wn)));
   }

   return EMPTY_WN2F_STATUS;
} /* WN2F_comment */


/*------------------------ exported routines --------------------------*/
/*---------------------------------------------------------------------*/


void 
WN2F_initialize(void)
{
   INT opr;
   INT map;

   /* Reset the WN2F_Handler array */
   for (opr = 0; opr < NUMBER_OF_OPERATORS; opr++)
      WN2F_Handler[opr] = &WN2F_unsupported;

   /* Initialize the WN2F_Handler array */
   for (map = 0; map < NUMBER_OF_OPR_HANDLERS; map++)
      WN2F_Handler[WN2F_Opr_Handler_List[map].opr] =
	 WN2F_Opr_Handler_List[map].handler;

   WN2F_Stmt_initialize();
   WN2F_Expr_initialize();
   WN2F_Load_Store_initialize();
   WN2F_Io_initialize();
   
} /* WN2F_initialize */


void 
WN2F_finalize(void)
{
   /* Reset the auxiliary WN translator modules, and the stab_attr
    * facility.
    */
   WN2F_Stmt_finalize();
   WN2F_Expr_finalize();
   WN2F_Load_Store_finalize();
   WN2F_Io_finalize();
   Stab_Free_Tmpvars();
} /* WN2F_finalize */

// utility to interpret context information 

void 
WN2F_dump_context( WN2F_CONTEXT c)
{
  printf ("(");

  if (WN2F_CONTEXT_new_pu(c))            printf (" new_pu") ;
  if (WN2F_CONTEXT_insert_induction(c))  printf (" induct_tmp_reqd") ;
  if (WN2F_CONTEXT_deref_addr(c))        printf (" deref") ;
  if (WN2F_CONTEXT_no_newline(c))        printf (" no_newline") ;
  if (WN2F_CONTEXT_has_logical_arg(c))   printf (" logical_arg") ;
  if (WN2F_CONTEXT_no_parenthesis(c))    printf (" no_paren") ;
  if (WN2F_CONTEXT_keyword_ioctrl(c))    printf (" ioctrl") ;
  if (WN2F_CONTEXT_io_stmt(c))           printf (" in_io") ;
  if (WN2F_CONTEXT_deref_io_item(c))     printf (" deref_io") ;
  if (WN2F_CONTEXT_origfmt_ioctrl(c))    printf (" varfmt")   ;
  if (WN2F_CONTEXT_emit_stid(c))         printf (" emit_stid") ;
  if (WN2F_CONTEXT_explicit_region(c))   printf (" region_pragma") ;
  if (WN2F_CONTEXT_fmt_io(c))            printf (" formatted io") ;
  if (WN2F_CONTEXT_cray_io(c))           printf (" craylib") ;
  printf (")\n");
}


WN2F_STATUS 
WN2F_translate(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* Determine whether we are in a context where we expect this
    * expression to have logically valued arguments, or whether
    * we are entering a context where we expect this expression
    * to be a logically valued argument.
    */
   if (OPCODE_is_boolean(WN_opcode(wn)) && 
       WN2F_expr_has_boolean_arg(WN_opcode(wn)))  /* expect logical args */
   {
      /* Note that this may also be a logical argument, so 
       * WN2F_CONTEXT_is_logical_arg(context) may also hold
       * TRUE.
       */
      set_WN2F_CONTEXT_has_logical_arg(context);
   }
   else if (WN2F_CONTEXT_has_logical_arg(context)) /* is a logical arg */
   {
      /* This is the only place where we should need to check whether
       * this is expected to be a logical valued expression. I.e. the
       * only place where we apply WN2F_CONTEXT_has_logical_arg(context).
       * However, it may be set at other places (e.g. in wn2f_stmt.c).
       */
      reset_WN2F_CONTEXT_has_logical_arg(context);
      set_WN2F_CONTEXT_is_logical_arg(context);
   }
   else
   {
      reset_WN2F_CONTEXT_has_logical_arg(context);
      reset_WN2F_CONTEXT_is_logical_arg(context);
   }
   
   /* Dispatch to the appropriate handler for this construct.
    */
   return WN2F_Handler[WN_opc_operator(wn)](tokens, wn, context);
} /* WN2F_translate */


/*------------------------ sundry utilities  --------------------------*/
/*---------------------------------------------------------------------*/

extern void
WN2F_Emit_End_Stmt(TOKEN_BUFFER tokens, BOOL start)
{
  /* For F90 host routine don't know about first/last internal procedures until
   * they're processed, so the host didn't get an END.  Emit the enclosing 
   * CONTAINS/END if required.
   */ 

  if (PU_Need_End_Contains) 
  {
    if (start)
    {
      if(PU_Dangling_Contains) 
      {
	PU_Dangling_Contains = FALSE;
	Append_Token_String(tokens,"CONTAINS");
	Append_Token_Special(tokens, '\n');
      }
    }
    else 
    { 
      PU_Need_End_Contains = FALSE;
      if (Is_Empty_Token_Buffer(tokens))
	Append_F77_Indented_Newline(tokens,0,NULL);
      Append_Token_String(tokens,"END");
      Append_Token_Special(tokens,'\n');
    }
  }
}

static void
WN2F_End_Routine_Strings(TOKEN_BUFFER tokens, INT32 func_id)
{
  // figures out how to END the current function.
  // An f77 routine, or f90 non-host just needs an END.
  // An f90 host requires a CONTAINs plus an END when the
  // last internal routine was seen. Distinguish functions
  // and subroutines for f90. 

  PU & pu = Pu_Table[ST_pu(PUINFO_FUNC_ST)];

  if (WN2F_F90_pu) {
    if (PU_has_nested(pu) && PU_lexical_level(pu) == GLOBAL_SYMTAB+1)
    {
      PU_Need_End_Contains = TRUE;
      PU_Dangling_Contains = TRUE;
      PU_Host_Func_Id = func_id; 
    }
    else {

      const char * p ;

      if (PU_is_mainpu(pu)) 
	p = "END";

      else {
	TY_IDX rt = PUINFO_RETURN_TY;

	if (TY_kind(rt) == KIND_VOID)
	  p = "END SUBROUTINE";
	else
      	  p = "END FUNCTION";
      }
      Append_Token_String(tokens,p);
      Append_Token_Special(tokens, '\n');
    }                                             

  } else {  /* F77 routine */

    Append_Token_String(tokens, "END");
    Append_Token_String(tokens, "!");
    Append_Token_String(tokens, PUINFO_FUNC_NAME) ;
    Append_Token_Special(tokens, '\n');
    Append_Token_Special(tokens, '\n');
  }
}

