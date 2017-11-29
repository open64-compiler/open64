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
 * Module: st2c.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:59-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.st2c.cxx $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *    See st2c.h for a description of the exported functions and 
 *    variables.  This module translates ST nodes into variable and
 *    function declarations (ST2C_decl_translate), and gets the 
 *    lvalue for a variable or function when directly referenced in
 *    an expression (ST2C_use_translate).  We provide a special 
 *    interface to deal with pseudo registers (pregs), but some 
 *    symbols must be handled by the context in which they appear,
 *    since this context uniquely determines the reference (e.g. 
 *    labels has label-numbers in the WN tree).
 *
 *    Possibly necessary TODO: sym_consts are only partially
 *    supported at the moment.

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
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.st2c.cxx $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "whirl2c_common.h"
#include "PUinfo.h"
#include "tcon2c.h"
#include "st2c.h"
#include "ty2c.h"
#include "init2c.h"


/*--------- General purpose macros to get ST attributes ---------------*/
/*---------------------------------------------------------------------*/


/* Two common block types are compatible when they are identical,
 * excluding qualifiers, but differentiating between differing
 * scalars and pointers.
 */
#define ST2C_COMPATIBLE_COMMON_BLOCK_TYPES(ty1, ty2) \
   Stab_Identical_Types(ty1, ty2, FALSE, TRUE, FALSE)


/*------- Handlers for references to and declarations of symbols ------*/
/*---------------------------------------------------------------------*/

static void ST2C_ignore(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);

static void ST2C_decl_error(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);
static void ST2C_decl_var(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);
static void ST2C_decl_func(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);
static void ST2C_decl_const(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);

static void ST2C_use_error(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);
static void ST2C_use_var(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);
static void ST2C_use_func(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);
static void ST2C_use_const(TOKEN_BUFFER tokens, const ST *st, CONTEXT context);


/* The following maps every ST class to a function that can translate
 * it to C.
 */
typedef void (*ST2C_HANDLER_FUNC)(TOKEN_BUFFER, const ST*, CONTEXT);

static const ST2C_HANDLER_FUNC ST2C_Decl_Handle[CLASS_COUNT] =
{
  &ST2C_ignore,      /* CLASS_UNK == 0x00 */
  &ST2C_decl_var,    /* CLASS_VAR == 0x01 */
  &ST2C_decl_func,   /* CLASS_FUNC == 0x02 */
  &ST2C_decl_const,  /* CLASS_CONST == 0x03 */
  &ST2C_decl_error,  /* CLASS_PREG == 0x04 */
  &ST2C_decl_error,  /* CLASS_BLOCK == 0x05 */
  &ST2C_decl_error   /* CLASS_NAME == 0x06 */
}; /* ST2C_Decl_Handle */

static const ST2C_HANDLER_FUNC ST2C_Use_Handle[CLASS_COUNT] =
{
  &ST2C_ignore,        /* CLASS_UNK == 0x00 */
  &ST2C_use_var,       /* CLASS_VAR == 0x01 */
  &ST2C_use_func,      /* CLASS_FUNC == 0x02 */
  &ST2C_use_const,     /* CLASS_CONST == 0x03 */
  &ST2C_use_error,     /* CLASS_PREG == 0x04 */
  &ST2C_decl_error,    /* CLASS_BLOCK == 0x05 */
  &ST2C_decl_error     /* CLASS_NAME == 0x06 */
}; /* ST2C_Use_Handle */


/*----- Utilities for combining Fortran common blocks into unions -----
 *
 * We use a hash-table with linked-list buckets to maintain information
 * about the common-blocks encountered for a compilation unit.  This
 * should be freed up when ST2C_finalize() is called, but never before
 * then.  Note that we allocate batches of TYLIST items at a time.
 *
 * This implementation of common-block handling when translating from
 * Fortran to C circumvents the w2cf_symtab.h symbol naming, instead
 * employing its own naming scheme.  It seemed simpler that way.
 *---------------------------------------------------------------------*/

#define COMMON_BLOCK_MEMBER_NAME(num) \
   Concat2_Strings("u", Number_as_String(num, "%lld"))

typedef struct Ty2c_List TY2C_LIST;
struct Ty2c_List
{
   SYMTAB_IDX   symtab_id; /* Current_Symtab->id */
   TOKEN_BUFFER tokens;    /* Block declaration, preceded by newline */
   TY_IDX       common_ty; /* Not live across PUs */
   TY2C_LIST   *next;
};
#define TY2C_LIST_symtab_id(l) ((l)->symtab_id)
#define TY2C_LIST_tokens(l) ((l)->tokens)
#define TY2C_LIST_common_ty(l) ((l)->common_ty)
#define TY2C_LIST_next(l) ((l)->next)

typedef struct Common_Block COMMON_BLOCK;
struct Common_Block
{
   const char   *name;         /* Name of common block, as given by STs */
   UINT64        hash_value;   /* The hash-value for the name */
   TOKEN_BUFFER  initializer;  /* Initialization */
   TY2C_LIST    *initialized;  /* An initialized member of the tylist */
   TY2C_LIST    *variations;   /* The variations in declaration of the block */
   TY2C_LIST    *last_variation; /* Last of the variations */
   COMMON_BLOCK *next;         /* The next common block in this bucket */
};
#define COMMON_BLOCK_name(cb) (cb)->name
#define COMMON_BLOCK_hash_value(cb) (cb)->hash_value
#define COMMON_BLOCK_initializer(cb) (cb)->initializer
#define COMMON_BLOCK_initialized(cb) (cb)->initialized
#define COMMON_BLOCK_variations(cb) (cb)->variations
#define COMMON_BLOCK_last_variation(cb) (cb)->last_variation
#define COMMON_BLOCK_next(cb) (cb)->next

#define COMMON_BLOCK_HASH_TABLE_SIZE 373
static COMMON_BLOCK *Common_Block_Hash_Tbl[COMMON_BLOCK_HASH_TABLE_SIZE];


#define TY2C_LIST_BLOCK_SIZE 16
typedef struct Ty2c_List_Block TY2C_LIST_BLOCK;
struct Ty2c_List_Block
{
   TY2C_LIST        element[TY2C_LIST_BLOCK_SIZE];
   TY2C_LIST_BLOCK *next;
};
#define TY2C_LIST_BLOCK_element(tb, n) &(tb)->element[n]
#define TY2C_LIST_BLOCK_next(tb) (tb)->next

static TY2C_LIST_BLOCK *ST2C_Ty2c_List_Blocks = NULL; /* All alloced Blocks */
static TY2C_LIST *ST2C_Free_Ty2c_Lists = NULL;        /* Unused tylists */


static BOOL 
In_Visible_Symtab(SYMTAB_IDX symtab, SYMTAB_IDX id)
{
   SYMTAB_IDX tab;

   for (tab = symtab; tab != 0 && tab != id; tab--);
   return tab != 0;
} /* In_Visible_Symtab */


static COMMON_BLOCK *
ST2C_Find_Common_Block(const char *name, UINT64 hash_value)
{
   /* Find a common block matching the given name and hash-value,
    * returning NULL if no match is found.
    */
   COMMON_BLOCK *common;
   const UINT32  hash_idx = Name_Hash_Idx(hash_value, 
					  COMMON_BLOCK_HASH_TABLE_SIZE);
   Is_True((name != NULL && *name != '\0'), 
	   ("Expected non-empty name in ST2C_Find_Common_Block()"));

   for (common = Common_Block_Hash_Tbl[hash_idx];
	(common != NULL && 
	 (COMMON_BLOCK_hash_value(common) != hash_value ||
	  strcmp(COMMON_BLOCK_name(common), name) != 0));
	common = COMMON_BLOCK_next(common));

   return common;
} /* ST2C_Find_Common_Block */


static COMMON_BLOCK *
ST2C_Get_Common_Block(const char *name, UINT64 hash_value)
{
   /* Return a COMMON_BLOCK for the given name and hash_value.  Create
    * a new common block if none with the given name exists.
    */
   COMMON_BLOCK *common;
   const UINT32  hash_idx = Name_Hash_Idx(hash_value, 
					  COMMON_BLOCK_HASH_TABLE_SIZE);

   common = ST2C_Find_Common_Block(name, hash_value);
   if (common == NULL)
   {
      /* Add a new common block to the beginning of the hash bucket */
      common = TYPE_ALLOC_N(COMMON_BLOCK, 1);
      COMMON_BLOCK_name(common) = 
	 strcpy(TYPE_ALLOC_N(char, strlen(name)+1), name);
      COMMON_BLOCK_hash_value(common) = hash_value;
      COMMON_BLOCK_initializer(common) = NULL;
      COMMON_BLOCK_initialized(common) = NULL;
      COMMON_BLOCK_variations(common) = NULL;
      COMMON_BLOCK_last_variation(common) = NULL;
      COMMON_BLOCK_next(common) = Common_Block_Hash_Tbl[hash_idx];
      Common_Block_Hash_Tbl[hash_idx] = common;
   }
   return common;
} /* ST2C_Get_Common_Block */


static TY2C_LIST *
ST2C_Get_Common_Ty2c_List(COMMON_BLOCK *common,
			  mUINT32       symtab_id,
			  const ST     *common_st,
			  TY_IDX        ty)
{
   /* Return the TY2C_LIST in the given common block, which is
    * compatible with the given ty and the symtab_id.  Create a new one 
    * and add it to the end of the ty2c list if none is found, updating
    * the given common block accordingly.
    */
   INT              ty2c_pos;
   TY2C_LIST       *ty2c_list;
   TY2C_LIST_BLOCK *ty2c_list_block;

   if (ST2C_Free_Ty2c_Lists == NULL)
   {
      /* Our repository of tylists is empty, so replenish it */
      ty2c_list_block = TYPE_ALLOC_N(TY2C_LIST_BLOCK, 1);
      TY2C_LIST_BLOCK_next(ty2c_list_block) = ST2C_Ty2c_List_Blocks;
      ST2C_Ty2c_List_Blocks = ty2c_list_block;
      
      ST2C_Free_Ty2c_Lists = 
	 TY2C_LIST_BLOCK_element(ST2C_Ty2c_List_Blocks, 0);
      for (ty2c_pos = 1; ty2c_pos < TY2C_LIST_BLOCK_SIZE; ty2c_pos++)
	 TY2C_LIST_next(&ST2C_Free_Ty2c_Lists[ty2c_pos-1]) = 
	    &ST2C_Free_Ty2c_Lists[ty2c_pos];
      TY2C_LIST_next(&ST2C_Free_Ty2c_Lists[TY2C_LIST_BLOCK_SIZE-1]) = NULL;
   }

   /* See if we already have a type in this common block which is 
    * compatible with the new given type.
    */
   for (ty2c_list = COMMON_BLOCK_variations(common);
	(ty2c_list != NULL && 
	 !(In_Visible_Symtab(CURRENT_SYMTAB, TY2C_LIST_symtab_id(ty2c_list)) &&
	   ST2C_COMPATIBLE_COMMON_BLOCK_TYPES(TY2C_LIST_common_ty(ty2c_list),
					      ty)));
	ty2c_list = TY2C_LIST_next(ty2c_list));
   
   if (ty2c_list == NULL)
   {
      /* No existing TY in this block is compatible with the new
       * type, so add it in the form of a new TY2C_LIST and update
       * the given common block accordingly.
       */
      CONTEXT context = INIT_CONTEXT;
      UINT    indentation;

      ty2c_list = ST2C_Free_Ty2c_Lists;
      ST2C_Free_Ty2c_Lists = TY2C_LIST_next(ST2C_Free_Ty2c_Lists);

      TY2C_LIST_symtab_id(ty2c_list) = symtab_id;
      TY2C_LIST_common_ty(ty2c_list) = ty;
      TY2C_LIST_next(ty2c_list) = NULL;

      indentation = Current_Indentation();
      Set_Current_Indentation(0);
      Increment_Indentation(); /* One of many common block variations */
      TY2C_LIST_tokens(ty2c_list) = New_Token_Buffer();
      //Avoid outputting duplicate struct types (see bug574)
      if (TY_kind(ty) != KIND_STRUCT)  
	Reset_TY_is_translated_to_c(ty);
      STR_IDX name_idx = TY_name_idx(Ty_Table[ty]);
      //      Set_TY_name_idx(Ty_Table[ty], 0);
      TY2C_translate(TY2C_LIST_tokens(ty2c_list), ty, context);
      //      Set_TY_name_idx(Ty_Table[ty], name_idx);
      Set_TY_is_translated_to_c(ty);
      Set_Current_Indentation(indentation);

      if (COMMON_BLOCK_variations(common) == NULL)
      {
	 COMMON_BLOCK_variations(common) = ty2c_list;
	 COMMON_BLOCK_last_variation(common) = ty2c_list;
      }
      else
      {
	 TY2C_LIST_next(COMMON_BLOCK_last_variation(common)) = ty2c_list;
      }
      if (ST_is_initialized(common_st))
      {
	 INITO_IDX inito = Find_INITO_For_Symbol(common_st);

	 if (inito != 0)
	 {
	    Is_True(!COMMON_BLOCK_initialized(common),
		    ("Common block (%s) is initialized twice",
		     ST_name(common_st)));

	    COMMON_BLOCK_initialized(common) = ty2c_list;
	    COMMON_BLOCK_initializer(common) = New_Token_Buffer();
	    inito = Find_INITO_For_Symbol(common_st);
	    Append_Token_Special(COMMON_BLOCK_initializer(common), '=');
	    INITO2C_translate(COMMON_BLOCK_initializer(common), inito);
	 }
      }
   }
   return ty2c_list;

} /* ST2C_Get_Common_Ty2c_List */


static void
ST2C_Define_A_Common_Block(TOKEN_BUFFER  tokens, 
			   COMMON_BLOCK *common, 
			   CONTEXT       context)
{
   TOKEN_BUFFER union_tokens;
   const char *variation_name;
   const char *base_name;
   INT         ordinal;
   TY2C_LIST  *ty2c_list;
   
   base_name = WHIRL2C_make_valid_c_name(COMMON_BLOCK_name(common));

   /* Get a declaration for each of the union elements, being careful
    * to put the initializing member before any other member.
    */
   union_tokens = New_Token_Buffer();
   ordinal = 0;
   for (ty2c_list = COMMON_BLOCK_variations(common);
	ty2c_list != NULL;
	ty2c_list = TY2C_LIST_next(ty2c_list), ordinal++)
   {
      variation_name = COMMON_BLOCK_MEMBER_NAME(ordinal);
      //  WE DON'T WANT TO PUT GLOBAL TYPE DECLS IN A UNION, CODE
      //  COMMMENTED OUT

      if (COMMON_BLOCK_initialized(common) == ty2c_list)
      {
	// if (ordinal > 0)
	//   Prepend_Indented_Newline(union_tokens, 1/*Lines between decls*/);
	Prepend_Token_Special(union_tokens, ';');
	// Prepend_Token_String(union_tokens, variation_name);
	Prepend_And_Reclaim_Token_List(union_tokens, 
					&TY2C_LIST_tokens(ty2c_list));
      }
      else
      {
	 Append_And_Reclaim_Token_List(union_tokens, 
				       &TY2C_LIST_tokens(ty2c_list));
	 // Append_Token_String(union_tokens, variation_name);
	 Append_Token_Special(union_tokens, ';');
	 // if (TY2C_LIST_next(ty2c_list) != NULL)
	 //   Append_Indented_Newline(union_tokens, 1/*Lines between decls*/);
      }
   }


   /* Do initialization */
   if (COMMON_BLOCK_initialized(common) != NULL)
   {
      Append_And_Reclaim_Token_List(union_tokens, 
				    &COMMON_BLOCK_initializer(common));
   }
   
   //   Append_Token_Special(union_tokens, ';');
   Append_And_Reclaim_Token_List(tokens, &union_tokens);
} /* ST2C_Define_A_Common_Block */


static const char *
ST2C_Get_Common_Block_Name(const ST *st)
{
   const char   *base_name;
   INT           ordinal;
   COMMON_BLOCK *common;
   TY2C_LIST    *ty2c_list;
   TY2C_LIST    *ty2c_list_iter;
   
   /* Get the basic data */
   common = 
      ST2C_Get_Common_Block(ST_name(st), Get_Hash_Value_For_Name(ST_name(st)));
   ty2c_list = ST2C_Get_Common_Ty2c_List(common, 
					 CURRENT_SYMTAB,
					 st, ST_type(st));
   base_name = WHIRL2C_make_valid_c_name(COMMON_BLOCK_name(common));

   //WEI: Since we're not putting global type decls in unions anymore,
   //name should be identical to the symbol's name(no need to append ".u0")
   return base_name;

} /* ST2C_Get_Common_Block_Name */


/*---------------- Various hidden utility routines --------------------*/
/*---------------------------------------------------------------------*/

static void
ST2C_formal_ref_decl(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   TOKEN_BUFFER decl_tokens = New_Token_Buffer();
   
   Is_True(ST_sclass(st) == SCLASS_FORMAL_REF, 
	   ("Unexpected ST_sclass in ST2C_formal_ref_decl()"));

   Append_Token_String(decl_tokens, 
		       W2CF_Symtab_Nameof_St(st));    /* name */
   TY2C_translate(decl_tokens, Stab_Pointer_To(ST_type(st)), context); /*type*/

   Append_And_Reclaim_Token_List(tokens, &decl_tokens);
} /* ST2C_formal_ref_decl */


static void
ST2C_basic_decl(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   TOKEN_BUFFER decl_tokens = New_Token_Buffer();
   
   Append_Token_String(decl_tokens, 
		       W2CF_Symtab_Nameof_St(st));    /* name */
   //WEI:
   //If type of st is struct, make it incomplete because the complete type will
   //be declared in w2c.h (see WN2C_Append_Symtab_Types)
   TY_IDX ty = ST_class(st) == CLASS_FUNC ? ST_pu_type(st) : ST_type(st);

     if (TY_kind(ty) == KIND_STRUCT ||
         (TY_kind(ty) == KIND_FUNCTION &&
          TY_kind(Func_Return_Type(ty)) == KIND_STRUCT)) {
       CONTEXT_set_incomplete_ty2c(context);
     }

   CONTEXT_reset_unqualified_ty2c(context);
   TY_IDX tidx = ST_sym_class(st) == CLASS_FUNC ? ST_pu_type(st) : ST_type(st);
   TY2C_translate(decl_tokens, tidx, context); /* type */

   if (!Stab_No_Linkage(st))
   {
      /* Static, common, or extern declarations */
      if (ST_sym_class(st) == CLASS_FUNC &&
          PU_is_inline_function(Pu_Table[ST_pu(st)]))
      {
	 Prepend_Token_String(decl_tokens, "__inline");
      }
      else if (ST_sym_class(st) == CLASS_FUNC &&
          PU_is_cdecl(ST_pu(st)))
      {
	 Prepend_Token_String(decl_tokens, "__cdecl");
      }
      else if (ST_sym_class(st) == CLASS_FUNC &&
           ST_export(st) == EXPORT_LOCAL) 
      {
         /* static functions */
         Prepend_Token_String(decl_tokens, "static");
      }
      else if (ST_sclass(st) == SCLASS_FSTATIC        || 
	       ST_sclass(st) == SCLASS_PSTATIC        ||
	       ST_sclass(st) == SCLASS_CPLINIT        ||
	       ST_sclass(st) == SCLASS_EH_REGION      ||
	       ST_sclass(st) == SCLASS_EH_REGION_SUPP ||
	       ST_sclass(st) == SCLASS_DISTR_ARRAY)
      {
	 Prepend_Token_String(decl_tokens, "static");
      }
      else if (ST_sclass(st) == SCLASS_EXTERN || 
	       ST_sclass(st) == SCLASS_TEXT)
      {
	 Prepend_Token_String(decl_tokens, "extern");
      }
   }

   Append_And_Reclaim_Token_List(tokens, &decl_tokens);
} /* ST2C_basic_decl */


static void
ST2C_Define_Preg(const char *name, TY_IDX ty, CONTEXT context)
{
   /* Declare a preg of the given type and name as a local
    * register variable in the current context.
    */
   TOKEN_BUFFER decl_tokens = New_Token_Buffer();
   UINT         current_indent = Current_Indentation();

   Set_Current_Indentation(PUinfo_local_decls_indent);
   Append_Token_String(decl_tokens, name);
   TY2C_translate(decl_tokens, ty, context);
   Prepend_Token_String(decl_tokens, "register");
   Append_Token_Special(decl_tokens, ';');
   Append_Indented_Newline(decl_tokens, 1);
   Append_And_Reclaim_Token_List(PUinfo_local_decls, &decl_tokens);
   Set_Current_Indentation(current_indent);
} /* ST2C_Define_Preg */


/*----------- hidden routines to handle ST declarations ---------------*/
/*---------------------------------------------------------------------*/

static void 
ST2C_ignore(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   return; /* Just ignore it, i.e. do nothing! */
} /* ST2C_ignore */


static void 
ST2C_decl_error(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   Is_True(FALSE, 
	   ("ST2C cannot declare this ST_sym_class (%d)", ST_sym_class(st)));
} /* ST2C_decl_error */


static void 
ST2C_decl_var(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   INITO_IDX inito;
   Is_True(ST_sym_class(st)==CLASS_VAR, ("expected CLASS_VAR ST"));

   if (ST_is_initialized(st) && !Stab_No_Linkage(st)) /* initialize */
   {
      ST2C_basic_decl(tokens, st, context); /*type, name, storage class*/
      inito = Find_INITO_For_Symbol(st);
      if (inito != 0)
      {
	 Append_Token_Special(tokens, '=');
	 INITO2C_translate(tokens, inito);
      }
   }
   else if (ST_sclass(st) == SCLASS_FORMAL_REF)
   {
      /* This should only occur for Fortran reference parameters
       */
      ST2C_formal_ref_decl(tokens, st, context); /*type, name, storage class*/
   }
   else
   {
      /* Ignore the (const) qualifier for automatic and temporary
       * variables, since the initialization is done as statements
       * for these.
       */
      if (ST_sclass(st) == SCLASS_AUTO)
	 CONTEXT_set_unqualified_ty2c(context);
      ST2C_basic_decl(tokens, st, context); /*type, name, storage class*/
   }
} /* ST2C_decl_var */


static void 
ST2C_decl_func(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   Is_True(ST_sym_class(st)==CLASS_FUNC, ("expected CLASS_FUNC ST"));

   /* Note, this is a function declaration, not a definition! */
   ST2C_basic_decl(tokens, st, context);   /* type, name and storage class */

} /* ST2C_decl_func */


static void 
ST2C_decl_const(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   Is_True(ST_sym_class(st)==CLASS_CONST, ("expected CLASS_CONST ST"));

   ST2C_basic_decl(tokens, st, context);   /* type, name and storage class */
   Append_Token_Special(tokens, '=');
   TCON2C_translate(tokens, STC_val(st));  /* value */
} /* ST2C_decl_const */


/*---------------- hidden routines to handle ST uses ------------------*/
/*---------------------------------------------------------------------*/


static void 
ST2C_use_error(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   Is_True(FALSE, 
	   ("ST2C cannot use an ST_sym_class (%d)", ST_sym_class(st)));
} /* ST2C_use_error */


static void 
ST2C_use_var(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   Is_True(ST_sym_class(st)==CLASS_VAR, ("expected CLASS_VAR ST"));

   // when compiling UPC, don't output the initialization expression of DGLOBAL vars
   if (Stab_Is_Common_Block(st) && !(ST_sclass(st) == SCLASS_DGLOBAL))
   {
      /* Do not mark the variable as referenced, since we do not
       * want to declare it in the local scope.
       */
      Append_Token_String(tokens, ST2C_Get_Common_Block_Name(st));
   }
   else
   {
      Append_Token_String(tokens, W2CF_Symtab_Nameof_St(st));
      /* Mark the variable as referenced, unless it is an external
       * defining variable.
       */
      if (!Stab_External_Def_Linkage(st))
	 Set_BE_ST_w2fc_referenced(st);
   }
} /* ST2C_use_var */


static void 
ST2C_use_func(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   Is_True(ST_sym_class(st)==CLASS_FUNC, ("expected CLASS_FUNC ST"));
   Append_Token_String(tokens, W2CF_Symtab_Nameof_St(st));
   if (!Stab_External_Def_Linkage(st))
      Set_BE_ST_w2fc_referenced(st);
} /* ST2C_use_func */


static void 
ST2C_use_const(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{
   Is_True(ST_sym_class(st)==CLASS_CONST, ("expected CLASS_CONST ST"));
   
   Append_Token_String(tokens, W2CF_Symtab_Nameof_St(st));
} /* ST2C_use_const */


/*------------------------ exported routines --------------------------*/
/*---------------------------------------------------------------------*/


void 
ST2C_initialize(CONTEXT context)
{
   return; /* Do nothing for now */
} /* ST2C_initialize */


void 
ST2C_finalize(void)
{
   INT              hash_idx;
   COMMON_BLOCK    *common;
   TY2C_LIST_BLOCK *ty2c_list_block;
   void            *to_be_freed;
   
   /* Free up the common-block hash table */
   for (hash_idx = 0; hash_idx < COMMON_BLOCK_HASH_TABLE_SIZE; hash_idx++)
   {
      /* Free up the common-block hash-table bucket */
      common = Common_Block_Hash_Tbl[hash_idx];
      while (common != NULL)
      {
	 to_be_freed = (void *)COMMON_BLOCK_name(common);
	 FREE(to_be_freed);
	 to_be_freed = common;
	 common = COMMON_BLOCK_next(common);
	 FREE(to_be_freed);
      }
      Common_Block_Hash_Tbl[hash_idx] = NULL;
   }

   /* Free up the common-block tylist data structure */
   ty2c_list_block = ST2C_Ty2c_List_Blocks;
   while (ty2c_list_block != NULL)
   {
      to_be_freed = ty2c_list_block;
      ty2c_list_block = TY2C_LIST_BLOCK_next(ty2c_list_block);
      FREE(to_be_freed);
   }
   ST2C_Ty2c_List_Blocks = NULL;

} /* ST2C_finalize */


void 
ST2C_decl_translate(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{ 
   ST2C_Decl_Handle[ST_sym_class(st)](tokens, st, context);
} /* ST2C_decl_translate */


void 
ST2C_weakext_translate(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{ 
   Is_True(ST_is_weak_symbol(st), 
	   ("Expected weak symbol in ST2C_weakext_translate()"));

   ST2C_decl_translate(tokens, st, context);
   Append_Token_Special(tokens, ';');
   Append_Indented_Newline(tokens, 1/*number of lines*/);
   Append_Token_String(tokens, "#pragma");
   Append_Token_String(tokens, "weak");
   ST2C_use_translate(tokens, st, context);

   if (ST_is_weak_symbol(st) && 
       Has_Base_Block(st) && 
       ST_sym_class(ST_base(st)) != CLASS_BLOCK)
   {
      Append_Token_Special(tokens, '=');
      ST2C_use_translate(tokens, ST_strong(st), context);
   }
} /* ST2C_weakext_translate */


void 
ST2C_use_translate(TOKEN_BUFFER tokens, const ST *st, CONTEXT context)
{ 
   ST2C_Use_Handle[ST_sym_class(st)](tokens, st, context);
} /* ST2C_use_translate */


void
ST2C_func_header(TOKEN_BUFFER  tokens, 
		 const ST     *st,     /* ST for function */
		 ST           **params, /*list of formal parms */
		 CONTEXT       context)
{
   /* Emit the header for a function definition!  Note that the resultant
    * token buffer will not have appended a newline after the function
    * header.
    */
   TOKEN_BUFFER header_tokens = New_Token_Buffer();
   INT          param, first_param;
   TY_IDX       funtype = ST_pu_type(st);
   BOOL         has_prototype = TY_has_prototype(funtype);
   
   Is_True((ST_sclass(st) == SCLASS_TEXT  
     || ST_sclass(st) == SCLASS_EXTERN) && TY_Is_Function(funtype),
     ("Illegal ST_sclass for function"));

   /* NOTE: We assume that when we return a value through a parameter,
    * the parameter will invariably be the first one.
    */
   first_param = (PUINFO_RETURN_TO_PARAM? 1 : 0);

   /* Append the function name */
   if (PU_is_mainpu(Pu_Table[ST_pu(st)]))
      Append_Token_String(header_tokens, "main");
   else
      Append_Token_String(header_tokens, W2CF_Symtab_Nameof_St(st));

   /* Append the parameter list */
   Append_Token_Special(header_tokens, '(');

   /* Emit non_prototype parameter names, if necessary */
   if (!has_prototype)
   {
      for (param = first_param; params[param] != NULL; param++)
      {
	 Append_Token_String(header_tokens, 
			     W2CF_Symtab_Nameof_St(params[param]));
	 if (params[param+1] != NULL)
	    Append_Token_Special(header_tokens, ',');
      }
      Append_Token_Special(header_tokens, ')');
      // If a struct appears in the function return type, it must be declared as incomplete
      CONTEXT_set_incomplete_ty2c(context);
      TY2C_translate(header_tokens, Func_Return_Type(funtype), context);

      /* Emit parameter declarations, indented and on a new line */
      Increment_Indentation();
      for (param = first_param; params[param] != NULL; param++)
      {
	 Append_Indented_Newline(header_tokens, 1);
	 ST2C_decl_translate(header_tokens, params[param], context);
	 Append_Token_Special(header_tokens, ';');
      }
      Decrement_Indentation();
   }
   else // (has_prototype)
   {
      /* We want the const qualifer for function prototypes */
      CONTEXT_set_const(context);

      /* Emit parameter declarations, indented and on a new line */
      TYLIST_IDX param_tylist = TY_parms(funtype);
      Increment_Indentation();
      for (param = first_param; params[param] != NULL; param++)
      {
	 Append_Indented_Newline(header_tokens, 1);
	 if (FALSE/*Turn this off for now*/ &&
	     Tylist_Table[param_tylist] != TY_IDX_ZERO)
	 {
	    // Use prototype types, rather than trusting the parameter types.
	    //
	    TY_IDX param_ty_idx = ST_type(params[param]);
	    Set_ST_type(*params[param], Tylist_Table[param_tylist]);
	    ST2C_decl_translate(header_tokens, params[param], context);
	    Set_ST_type(*params[param], param_ty_idx);
	    param_tylist = TYLIST_next(param_tylist);
	 }
	 else
	 {
	    ST2C_decl_translate(header_tokens, params[param], context);
	 }
	 if (params[param+1] != NULL)
	    Append_Token_Special(header_tokens, ',');
      }
      CONTEXT_reset_const(context);

      /* Finish off the parameter list, with varargs if appropriate */
      if (TY_is_varargs(funtype))
      {
	 Append_Token_Special(header_tokens, ',');
	 Append_Token_String(header_tokens, "...");
      }
      Append_Token_Special(header_tokens, ')');
      Decrement_Indentation();
      TY2C_translate(header_tokens, Func_Return_Type(funtype), context);
   }
   
   if (PU_is_inline_function(Pu_Table[ST_pu(st)])) {
      Prepend_Token_String(header_tokens, "__inline");
   } else if (ST_export(st) == EXPORT_LOCAL) {
     //at this point, any static function will be SCLASS_TEXT,
     //so we look at its export scopt instead.  See bug 476.
     Prepend_Token_String(header_tokens, "static");
   } else {
     //all non-static functions in C should have external linkage
     Prepend_Token_String(header_tokens, "extern");
   }

   Append_And_Reclaim_Token_List(tokens, &header_tokens);
} /* ST2C_func_header */


void
ST2C_Use_Preg(TOKEN_BUFFER tokens,
	      TY_IDX       preg_ty,
	      PREG_IDX     preg_idx,
	      CONTEXT      context)
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
      ST2C_Define_Preg(preg_name, preg_ty, context);
      PUinfo_Set_Preg_Declared(preg_ty, preg_idx);
   }

   Append_Token_String(tokens, preg_name);
} /* ST2C_Use_Preg */


void ST2C_Declare_Tempvar(TY_IDX ty, UINT idx)
{
   TOKEN_BUFFER tmp_tokens = New_Token_Buffer();
   UINT         current_indent = Current_Indentation();
   CONTEXT      ty_context;
   
   Set_Current_Indentation(PUinfo_local_decls_indent);
   Append_Token_String(
      tmp_tokens, W2CF_Symtab_Nameof_Tempvar(idx));   /* name */
      
   /* Ignore the (const) qualifier for automatic and temporary
    * variables, since the initialization is done as statements
    * for these.
    */
   CONTEXT_reset(ty_context);
   CONTEXT_set_unqualified_ty2c(ty_context);
   TY2C_translate(tmp_tokens, ty, ty_context);         /* type */
   Append_Token_Special(tmp_tokens, ';');
   Append_Indented_Newline(tmp_tokens, 1);
   Append_And_Reclaim_Token_List(PUinfo_local_decls, &tmp_tokens);
   Set_Current_Indentation(current_indent);
} /* ST2C_Declare_Tempvar */


void
ST2C_New_Common_Block(const ST *st)
{
   /* Given a Fortran common block st, associate it with the
    * corresponding COMMON_BLOCK representation.  Note that 
    * only one common block type may have an initializer
    * associated with it.
    */
   const char   *name = ST_name(st);
   const UINT64  hash_value = Get_Hash_Value_For_Name(name);
   TY_IDX        ty = ST_type(st);
   COMMON_BLOCK *common;
   
   Is_True(Stab_Is_Common_Block(st), 
	   ("Expected common block in ST2C_New_Common_Block()"));

   /* Create the common block and the associated ty2c list, as defined by
    * the given st.
    */
   common = ST2C_Get_Common_Block(name, hash_value);
   (void)ST2C_Get_Common_Ty2c_List(common, CURRENT_SYMTAB, st, ty);
   /* Ensure that the type will not be declared in the local PU scope */
   Set_TY_is_translated_to_c(ty);
} /* ST2C_New_Common_Block */


void 
ST2C_Define_Common_Blocks(TOKEN_BUFFER tokens, CONTEXT context)
{
   INT           hash_idx;
   COMMON_BLOCK *common;
   
   /* Run through the hash-table */
   for (hash_idx = 0; hash_idx < COMMON_BLOCK_HASH_TABLE_SIZE; hash_idx++)
   {
      /* Run through the list of common blocks */
      for (common = Common_Block_Hash_Tbl[hash_idx];
	   common != NULL;
	   common = COMMON_BLOCK_next(common))
      {
	 ST2C_Define_A_Common_Block(tokens, common, context);
	 Append_Indented_Newline(tokens, 2/*Lines between decls*/);
      }
   }
} /* ST2C_Define_Common_Blocks */



