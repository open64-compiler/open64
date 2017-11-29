/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: wn2f_load_store.c
 * $Revision: 1.5 $
 * $Date: 05/12/05 08:59:32-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2f/SCCS/s.wn2f_load_store.cxx $
 *
 * Revision history:
 *  12-Apr-95 - Original Version
 *
 * Description:
 *
 *   Translate a WN load/store subtree to Fortran by means of an inorder 
 *   recursive descent traversal of the WHIRL IR.  Note that the routines
 *   handle statements and expressions are in separate source files.
 *   Recursive translation of WN nodes should only use WN2F_Translate()!
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2f/SCCS/s.wn2f_load_store.cxx $ $Revision: 1.5 $";
#endif

#include "whirl2f_common.h"
#include "PUinfo.h"          /* In be/whirl2c directory */
#include "pf_cg.h"
#include "wn2f.h"
#include "st2f.h"
#include "ty2f.h"
#include "tcon2f.h"
#include "wn2f_load_store.h"

static void WN2F_Block(TOKEN_BUFFER tokens, ST * st, STAB_OFFSET off,WN2F_CONTEXT context) ;

static WN *WN2F_ZeroInt_Ptr = NULL;
static WN *WN2F_OneInt_Ptr = NULL;

#define WN2F_INTCONST_ZERO\
   (WN2F_ZeroInt_Ptr == NULL? WN2F_ZeroInt_Ptr = WN2F_Initiate_ZeroInt() \
                            : WN2F_ZeroInt_Ptr)
#define WN2F_INTCONST_ONE\
   (WN2F_OneInt_Ptr == NULL? WN2F_OneInt_Ptr = WN2F_Initiate_OneInt() \
                            : WN2F_OneInt_Ptr)


void WN2F_Array_Slots(TOKEN_BUFFER tokens, WN *wn,WN2F_CONTEXT context,BOOL parens);

/*------------------------- Utility Functions ------------------------*/
/*--------------------------------------------------------------------*/

static ST *
WN2F_Get_Named_Param(const WN *pu, const char *param_name)
{
   /* Find a parameter with a matching name, if possible, otherwise
    * return NULL.
    */
   ST *param_st = NULL;
   INT param, num_formals;

   if (WN_opcode(pu) == OPC_ALTENTRY)
      num_formals = WN_kid_count(pu);
   else
      num_formals = WN_num_formals(pu);

   /* Search through the parameter ST entries
    */
   for (param = 0; param_st == NULL && param < num_formals; param++)
   {
      if (ST_name(WN_st(WN_formal(pu, param))) != NULL &&
	  strcmp(ST_name(WN_st(WN_formal(pu, param))), param_name) == 0)
	 param_st = WN_st(WN_formal(pu, param));
   }
   return param_st;
} /* WN2F_Get_Named_Param */

static void
WN2F_Translate_StringLEN(TOKEN_BUFFER tokens, ST *param_st)
{
   INT dim;
   TY_IDX param_ty = (TY_Is_Pointer(ST_type(param_st))? 
                   TY_pointed(ST_type(param_st)) : ST_type(param_st));

   Append_Token_String(tokens, "LEN");
   Append_Token_Special(tokens, '(');
   Append_Token_String(tokens, W2CF_Symtab_Nameof_St(param_st));

   if (TY_Is_Array(param_ty) && !TY_Is_Character_String(param_ty))
   {
      /* Append index values (any arbitrary value will do for each dimension)
       */
      Append_Token_Special(tokens, '(');


      ARB_HANDLE arb_base = TY_arb(param_ty);
      dim = ARB_dimension(arb_base) - 1; 

      while ( dim >= 0)
      {
	ARB_HANDLE arb = arb_base[dim];

         Append_Token_String(tokens, "1");
	 if (dim-- > 0)
            Append_Token_Special(tokens, ',');
      }
      Append_Token_Special(tokens, ')');
   }
   else
   {
      ASSERT_WARN(TY_Is_Character_String(param_ty), 
                  (DIAG_W2F_EXPECTED_PTR_TO_CHARACTER,
		    "WN2F_Translate_StringLEN"));
   }
   Append_Token_Special(tokens, ')');
} /* WN2F_Translate_StringLEN */

static WN *
WN2F_Initiate_ZeroInt(void)
{
   static char ZeroInt [sizeof (WN)];
   WN       *wn = (WN*) &ZeroInt;
   OPCODE    opcode = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);

   bzero(wn, sizeof(WN));
   WN_set_opcode(wn, opcode);
   WN_set_kid_count(wn, 0);
   WN_set_map_id(wn, WN_MAP_UNDEFINED);
   WN_const_val(wn) = 0LL;
   return wn;
} /* WN2F_Initiate_ZeroInt */

static WN *
WN2F_Initiate_OneInt(void)
{
   static char OneInt [sizeof (WN)];
   WN       *wn = (WN*) &OneInt;
   OPCODE    opcode = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);

   bzero(wn, sizeof(WN));
   WN_set_opcode(wn, opcode);
   WN_set_kid_count(wn, 0);
   WN_set_map_id(wn, WN_MAP_UNDEFINED);
   WN_const_val(wn) = 1LL;
   return wn;
} /* WN2F_Initiate_ZeroInt */


static BOOL
WN2F_Expr_Plus_Literal(TOKEN_BUFFER tokens,
		       WN          *wn,
		       INT64        literal,
		       WN2F_CONTEXT context)
{
   /* Returns TRUE if the resultant value is constant and different
    * from zero.
    */
   const BOOL parenthesize = !WN2F_CONTEXT_no_parenthesis(context);
   BOOL       is_const = TRUE;
   INT64      value;
   
   if (WN_opc_operator(wn) == OPR_INTCONST)
      value = WN_const_val(wn) + literal;
   else if (WN_opc_operator(wn) == OPR_CONST)
      value = Targ_To_Host(STC_val(WN_st(wn))) + literal;
   else
      is_const = FALSE;
   
   if (is_const)
   {
      TCON2F_translate(tokens,
		       Host_To_Targ(WN_opc_rtype(wn), value),
		       FALSE /*is_logical*/);
   }
   else
   {
      if (parenthesize)
      {
	 reset_WN2F_CONTEXT_no_parenthesis(context);
	 Append_Token_Special(tokens, '(');
      }
      WN2F_translate(tokens, wn, context);
      Append_Token_Special(tokens, '+');
      TCON2F_translate(tokens,
		       Host_To_Targ(MTYPE_I4, 1),
		       FALSE /*is_logical*/);
      if (parenthesize)
	 Append_Token_Special(tokens, ')');
   }

   return is_const && (value != 0LL);
} /* WN2F_Expr_Plus_Literal */


static WN2F_STATUS
WN2F_Denormalize_Array_Idx(TOKEN_BUFFER tokens, 
			   WN          *idx_expr, 
			   WN2F_CONTEXT context)
{
   const BOOL   parenthesize = !WN2F_CONTEXT_no_parenthesis(context);
   TOKEN_BUFFER tmp_tokens;
   BOOL         non_zero, cexpr_is_lhs;
   WN          *nexpr, *cexpr;
   INT64        plus_value;
   
   /* Given an index expression, translate it to Fortran and append
    * the tokens to the given token-buffer.  If the value of the idx
    * expression is "v", then the appended tokens should represent
    * the value "v+1".  This denormalization moves the base of the
    * array from index zero to index one.
    */
   if (WN_opc_operator(idx_expr) == OPR_ADD && 
       (WN_is_constant_expr(WN_kid1(idx_expr)) || 
	WN_is_constant_expr(WN_kid0(idx_expr))))
   {
      /* Do the "e+c" ==> "e+(c+1)" translation, using the property
       * that addition is commutative.
       */
      if (WN_is_constant_expr(WN_kid1(idx_expr)))
      {
	 cexpr = WN_kid1(idx_expr);
	 nexpr = WN_kid0(idx_expr);
      }
      else /* if (WN_is_constant_expr(WN_kid0(idx_expr))) */
      {
	 cexpr = WN_kid0(idx_expr);
	 nexpr = WN_kid1(idx_expr);
      }
      tmp_tokens = New_Token_Buffer();
      non_zero = WN2F_Expr_Plus_Literal(tmp_tokens, cexpr, 1LL, context);
      if (non_zero)
      {
	 if (parenthesize)
	 {
	    reset_WN2F_CONTEXT_no_parenthesis(context);
	    Append_Token_Special(tokens, '(');
	 }
	 WN2F_translate(tokens, nexpr, context);
	 Append_Token_Special(tokens, '+');
	 Append_And_Reclaim_Token_List(tokens, &tmp_tokens);
	 if (parenthesize)
	    Append_Token_Special(tokens, ')');
      }
      else
      {
	 Reclaim_Token_Buffer(&tmp_tokens);
	 WN2F_translate(tokens, nexpr, context);
      }
   }
   else if (WN_opc_operator(idx_expr) == OPR_SUB && 
	    (WN_is_constant_expr(WN_kid1(idx_expr)) || 
	     WN_is_constant_expr(WN_kid0(idx_expr))))
   {
      /* Do the "e-c" ==> "e-(c-1)" or the  "c-e" ==> "(c+1)-e"
       * translation.
       */
      cexpr_is_lhs = WN_is_constant_expr(WN_kid0(idx_expr));
      if (!cexpr_is_lhs)
      {
	 cexpr = WN_kid1(idx_expr);
	 nexpr = WN_kid0(idx_expr);
	 plus_value = -1LL;
      }
      else
      {
	 cexpr = WN_kid0(idx_expr);
	 nexpr = WN_kid1(idx_expr);
	 plus_value = 1LL;
      }
	
      /* Do the "e-c" ==> "e-(c-1)" or the  "c-e" ==> "(c+1)-e"
       * translation.
       */
      tmp_tokens = New_Token_Buffer();
      non_zero = 
	 WN2F_Expr_Plus_Literal(tmp_tokens, cexpr, plus_value, context);
      if (non_zero)
      {
	 if (parenthesize)
	 {
	    reset_WN2F_CONTEXT_no_parenthesis(context);
	    Append_Token_Special(tokens, '(');
	 }
	 if (!cexpr_is_lhs)
	 {
	    WN2F_translate(tokens, nexpr, context);
	    Append_Token_Special(tokens, '-');
	    Append_And_Reclaim_Token_List(tokens, &tmp_tokens);
	 }
	 else
	 {
	    Append_And_Reclaim_Token_List(tokens, &tmp_tokens);
	    Append_Token_Special(tokens, '-');
	    WN2F_translate(tokens, nexpr, context);
	 }
	 if (parenthesize)
	    Append_Token_Special(tokens, ')');
      }
      else
      {
	 Reclaim_Token_Buffer(&tmp_tokens); 
	 if (cexpr_is_lhs)
	 {
	    if (parenthesize)
	    {
	       reset_WN2F_CONTEXT_no_parenthesis(context);
	       Append_Token_Special(tokens, '(');
	    }
	    Append_Token_Special(tokens, '-');
	    WN2F_translate(tokens, nexpr, context);
	    if (parenthesize)
	       Append_Token_Special(tokens, ')');
	 }
	 else
	 {
	    WN2F_translate(tokens, nexpr, context);
	 }
      }
   }
   else
   {
      WN2F_Expr_Plus_Literal(tokens, idx_expr, 1LL, context);
   }
   return EMPTY_WN2F_STATUS;
} /* WN2F_Denormalize_Array_Idx */


static void
WN2F_Normalize_Idx_To_Onedim(TOKEN_BUFFER tokens, 
			     WN*          wn,
			     WN2F_CONTEXT context)
{
   INT32 dim1, dim2;

   /* Parenthesize the normalized index expressions */
   reset_WN2F_CONTEXT_no_parenthesis(context);

   for (dim1 = 0; dim1 < WN_num_dim(wn); dim1++)
   {
      if (dim1 > 0)
	 Append_Token_Special(tokens, '+');

      /* Multiply the index expression with the product of the sizes
       * of subordinate dimensions, where a higher dimension-number
       * means a more subordinate dimension.  Do not parenthesize the
       * least significant index expression.
       */   
      if (dim1+1 == WN_num_dim(wn))
	 set_WN2F_CONTEXT_no_parenthesis(context);
      WN2F_Denormalize_Array_Idx(tokens, WN_array_index(wn, dim1), context);
      for (dim2 = dim1+1; dim2 < WN_num_dim(wn); dim2++)
      {
	 Append_Token_Special(tokens, '*');
	 (void)WN2F_translate(tokens, WN_array_dim(wn, dim2), context);
      } /*for*/
   } /*for*/
} /* WN2F_Normalize_Idx_To_Onedim */


static void
WN2F_Substring(TOKEN_BUFFER tokens, 
	       INT64        string_size,
	       WN          *lower_bnd,
	       WN          *substring_size,
	       WN2F_CONTEXT context)
{
   /* Given a substring offset from the base of a character string 
    * (lower_bnd), the size of the whole string, and the size of the
    * substring, generate the notation necessary as a suffix to the
    * string reference to denote the substring.
    */
   if (WN_opc_operator(lower_bnd) != OPR_INTCONST      ||
       WN_const_val(lower_bnd) != 0                    ||
       WN_opc_operator(substring_size) != OPR_INTCONST ||
       WN_const_val(substring_size) != string_size)
   {
      /* Need to generate substring expression "(l+1:l+size)" */
      Append_Token_Special(tokens, '(');
      set_WN2F_CONTEXT_no_parenthesis(context);
      WN2F_Denormalize_Array_Idx(tokens, lower_bnd, context);
      reset_WN2F_CONTEXT_no_parenthesis(context);
      Append_Token_Special(tokens, ':');
      if (WN_opc_operator(lower_bnd) != OPR_INTCONST ||
	  WN_const_val(lower_bnd) != 0)
      {
	 WN2F_translate(tokens, lower_bnd, context);
	 Append_Token_Special(tokens, '+');
      }
      WN2F_translate(tokens, substring_size, context);
      Append_Token_Special(tokens, ')');
   }
} /* WN2F_Substring */


static void
WN2F_Get_Substring_Info(WN **base,         /* Possibly OPR_ARRAY node (in/out) */
			TY_IDX *string_ty, /* The string type (out) */
			WN **lower_bnd)    /* The lower bound index (out) */
{
   /* There are two possibilities concerning the array base expressions.
    * It can be a pointer to a complete character-string (array) or it
    * can be a pointer to a character within a character-string (single
    * character).  In the first instance, the offset off the base of 
    * string is zero.  In the latter case, the offset is given by the
    * array indexing operation.
    */
   TY_IDX ptr_ty = WN_Tree_Type(*base);

   *string_ty = TY_pointed(ptr_ty);

   if (TY_size(*string_ty) == 1 && 
       !TY_Is_Array(*string_ty) &&
       WN_opc_operator(*base) == OPR_ARRAY)
   {
      /* Let the base of the string be denoted as the base of the array
       * expression.
       */
      *string_ty = TY_pointed(WN_Tree_Type(WN_kid0(*base)));
      *lower_bnd = WN_array_index(*base, 0);
      *base = WN_kid0(*base);
   }
   else if (WN_opc_operator(*base) == OPR_ARRAY &&
	    TY_Is_Array(*string_ty)             &&
	    TY_AR_ndims(*string_ty) == 1        &&
	    TY_Is_Character_String(*string_ty)  &&
	    !TY_ptr_as_array(Ty_Table[ptr_ty]))
   {
      /* Presumably, the lower bound is given by the array operator
       */
      *lower_bnd = WN_array_index(*base, 0);
      *base = WN_kid0(*base);
   }
   else
   {
      *lower_bnd = WN2F_INTCONST_ZERO;
   }
} /* WN2F_Get_Substring_Info */

static WN *
WN2F_Find_Base(WN *addr)
{
  /* utility to find base of address tree */

  WN *res = addr;

  switch (WN_operator(addr))
  {
    case OPR_ARRAY: 
    case OPR_ILOAD:
    res = WN2F_Find_Base(WN_kid0(addr));
    break;

    case OPR_ADD:
      if (WN_operator(WN_kid0(addr)) == OPR_INTCONST)
	res = WN2F_Find_Base(WN_kid1(addr));
      else
	res = WN2F_Find_Base(WN_kid0(addr));
    break;

  default:
    res = addr;
    break;
  }
  return res;
}

extern BOOL
WN2F_Is_Address_Preg(WN * ad ,TY_IDX ptr_ty)
{
  /* Does this look like a preg or variable being used as an address ? */
  /* These are propagated by opt/pfa                                   */

  BOOL is_somewhat_address_like = TY_kind(ptr_ty) == KIND_POINTER;
  
  if (TY_kind(ptr_ty) == KIND_SCALAR) 
  {
    TYPE_ID tid = TY_mtype(ptr_ty);

    is_somewhat_address_like |= (MTYPE_is_pointer(tid)) || (tid == MTYPE_I8) || (tid == MTYPE_I4) ;
  }

  if (is_somewhat_address_like)
  {
    WN * wn = WN2F_Find_Base(ad);
    
    if (WN_operator(wn) == OPR_LDID) 
    {
      ST * st = WN_st(wn) ;
      if (ST_class(st) == CLASS_PREG)
	return TRUE ;
      
      if (ST_class(st) == CLASS_VAR) 
      {
	if (TY_kind(ptr_ty) == KIND_SCALAR)
	  return TRUE;
	
	if (TY_kind(WN_ty(wn)) == KIND_SCALAR)
	{
	  TYPE_ID wtid = TY_mtype(WN_ty(wn));
	  
	  /* Looks like a Cray pointer (I4/I8) ? */
	  
	  if ((wtid == MTYPE_I8)|| (wtid == MTYPE_I4))
	    if (ad != wn)
	      return TRUE ;
	  
	  /* Looks like a VAR with a U4/U8? used  */
	  /* only with offsets, or FORMALs would  */
	  /* qualify, if intrinsic mtype          */
	  
	  if (MTYPE_is_pointer(wtid))
	    if (TY_kind(ST_type(st)) != KIND_SCALAR)
	      return TRUE;
	}
      }
    }
  }
  return FALSE;
}

/*---------------------- Prefetching Comments ------------------------*/
/*--------------------------------------------------------------------*/

static void
WN2F_Append_Prefetch_Map(TOKEN_BUFFER tokens, WN *wn)
{
   PF_POINTER* pfptr;
   const char *info_str;
   
   pfptr = (PF_POINTER*)WN_MAP_Get(WN_MAP_PREFETCH, wn);
   info_str = "prefetch (ptr, lrnum): ";
   if (pfptr->wn_pref_1L)
   {
      info_str = 
	 Concat2_Strings(    info_str,
          Concat2_Strings(   "1st <", 
           Concat2_Strings(  Ptr_as_String(pfptr->wn_pref_1L),
            Concat2_Strings( ", ",
             Concat2_Strings(WHIRL2F_number_as_name(pfptr->lrnum_1L),
			     ">")))));
   }
   if (pfptr->wn_pref_2L)
   {
      info_str = 
	 Concat2_Strings(    info_str,
          Concat2_Strings(   "2nd <", 
           Concat2_Strings(  Ptr_as_String(pfptr->wn_pref_2L),
            Concat2_Strings( ", ",
             Concat2_Strings(WHIRL2F_number_as_name(pfptr->lrnum_2L),
			     ">")))));
   }
   Append_Token_String(tokens, info_str);
} /* WN2F_Append_Prefetch_Map */


/*----------------------- Exported Functions ------------------------*/
/*--------------------------------------------------------------------*/

void WN2F_Load_Store_initialize(void)
{
   /* Nothing to do at the moment */
} /* WN2F_Load_Store_initialize */


void WN2F_Load_Store_finalize(void)
{
   /* Nothing to do at the moment */
} /* WN2F_Load_Store_finalize */


extern WN2F_STATUS 
WN2F_istore(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TOKEN_BUFFER  lhs_tokens;
   TOKEN_BUFFER  rhs_tokens;
   TY_IDX        base_ty;
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_ISTORE, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_istore"));

   /* Get the base address into which we are storing a value */
   base_ty = WN_Tree_Type(WN_kid1(wn));
   if (!TY_Is_Pointer(base_ty))
      base_ty = WN_ty(wn);

   /* Get the lhs of the assignment (dereference address) */
   lhs_tokens = New_Token_Buffer();
   WN2F_Offset_Memref(lhs_tokens, 
		      WN_kid1(wn),           /* base-symbol */
		      base_ty,               /* base-type */
		      TY_pointed(WN_ty(wn)), /* object-type */
		      WN_store_offset(wn),   /* object-ofst */
		      context);
   
   /* The rhs */
   rhs_tokens = New_Token_Buffer();
   if (TY_is_logical(Ty_Table[TY_pointed(WN_ty(wn))]))
   {
      set_WN2F_CONTEXT_has_logical_arg(context);
      WN2F_translate(rhs_tokens, WN_kid0(wn), context);
      reset_WN2F_CONTEXT_has_logical_arg(context);
   }
   else
      WN2F_translate(rhs_tokens, WN_kid0(wn), context);

   /* See if we need to apply a "char" conversion to the rhs
    */
   if (TY_Is_Character_String(W2F_TY_pointed(WN_ty(wn), "ISTORE lhs")) &&
       TY_Is_Integral(WN_Tree_Type(WN_kid0(wn))))
   {
      Prepend_Token_Special(rhs_tokens, '(');
      Prepend_Token_String(rhs_tokens, "char");
      Append_Token_Special(rhs_tokens, ')');
   }

   /* Assign the rhs to the lhs.
    */
   if (Identical_Token_Lists(lhs_tokens, rhs_tokens))
   {
      /* Ignore this redundant assignment statement! */
      Reclaim_Token_Buffer(&lhs_tokens);
      Reclaim_Token_Buffer(&rhs_tokens);
   }
   else
   {
      /* See if there is any prefetch information with this store,
       * and if so insert information about it as a comment preceeding
       * the store.
       */
      if (W2F_Emit_Prefetch && WN_MAP_Get(WN_MAP_PREFETCH, wn))
      {
	 Append_F77_Comment_Newline(tokens, 1, TRUE/*indent*/);
	 WN2F_Append_Prefetch_Map(tokens, wn);
      }

      /* The assignment statement on a new line */
      WN2F_Stmt_Newline(tokens, NULL/*label*/, WN_linenum(wn), context);
      Append_And_Reclaim_Token_List(tokens, &lhs_tokens);
      Append_Token_Special(tokens, '=');
      Append_And_Reclaim_Token_List(tokens, &rhs_tokens);
   }

   return EMPTY_WN2F_STATUS;
} /* WN2F_istore */

WN2F_STATUS 
WN2F_istorex(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_WARN(FALSE, (DIAG_UNIMPLEMENTED, "WN2F_istorex"));
   WN2F_Stmt_Newline(tokens, NULL/*label*/, WN_linenum(wn), context);
   Append_Token_String(tokens, WN_opc_name(wn));

   return EMPTY_WN2F_STATUS;
} /* WN2F_istorex */

WN2F_STATUS 
WN2F_mstore(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TOKEN_BUFFER  lhs_tokens;
   TOKEN_BUFFER  rhs_tokens;
   TY_IDX        base_ty;

   /* Note that we make the assumption that this is just like an 
    * ISTORE, and handle it as though it were.  We do not handle
    * specially assignment-forms where the lhs is incompatible with
    * the rhs, since we assume this will never happen for Fortran
    * and we cannot easily get around this like we do in C (i.e.
    * with cast expressions.
    */
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_MSTORE, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_mstore"));

   /* Get the base address into which we are storing a value */
   base_ty = WN_Tree_Type(WN_kid1(wn));
   if (!TY_Is_Pointer(base_ty))
      base_ty = WN_ty(wn);

   /* Get the lhs of the assignment (dereference address) */
   lhs_tokens = New_Token_Buffer();
   WN2F_Offset_Memref(lhs_tokens, 
		      WN_kid1(wn),           /* base-symbol */
		      base_ty,               /* base-type */
		      TY_pointed(WN_ty(wn)), /* object-type */
		      WN_store_offset(wn),   /* object-ofst */
		      context);
   
   /* The rhs */
   rhs_tokens = New_Token_Buffer();
   WN2F_translate(rhs_tokens, WN_kid0(wn), context);

   /* Assign the rhs to the lhs.
    */
   if (Identical_Token_Lists(lhs_tokens, rhs_tokens))
   {
      /* Ignore this redundant assignment statement! */
      Reclaim_Token_Buffer(&lhs_tokens);
      Reclaim_Token_Buffer(&rhs_tokens);
   }
   else
   {
      /* The assignment statement on a new line */
      WN2F_Stmt_Newline(tokens, NULL/*label*/, WN_linenum(wn), context);
      Append_And_Reclaim_Token_List(tokens, &lhs_tokens);
      Append_Token_Special(tokens, '=');
      Append_And_Reclaim_Token_List(tokens, &rhs_tokens);
   }


   return EMPTY_WN2F_STATUS;
} /* WN2F_mstore */

WN2F_STATUS 
WN2F_stid(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TOKEN_BUFFER lhs_tokens, rhs_tokens;
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_STID, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_stid"));

   /* Get the lhs of the assignment */
   lhs_tokens = New_Token_Buffer();
   if (ST_class(WN_st(wn)) == CLASS_PREG)
   {
      ST2F_Use_Preg(lhs_tokens, ST_type(WN_st(wn)), WN_store_offset(wn));
   }
   else if (ST_sym_class(WN_st(wn))==CLASS_VAR && ST_is_not_used(WN_st(wn)))
   {
      /* This is a redundant assignment statement, so determined
       * by IPA, so just assign it to a temporary variable
       * instead.
       */
      UINT tmp_idx = Stab_Lock_Tmpvar(WN_ty(wn), &ST2F_Declare_Tempvar);
      Append_Token_String(lhs_tokens, W2CF_Symtab_Nameof_Tempvar(tmp_idx));
      Stab_Unlock_Tmpvar(tmp_idx);
   }
   else
   {
      WN2F_Offset_Symref(lhs_tokens, 
			 WN_st(wn),                        /* base-symbol */
			 Stab_Pointer_To(ST_type(WN_st(wn))),/* base-type */
			 WN_ty(wn),                        /* object-type */
			 WN_store_offset(wn),              /* object-ofst */
			 context);
   }
   
   /* The rhs */
   rhs_tokens = New_Token_Buffer();
   if (TY_is_logical(Ty_Table[WN_ty(wn)]))
   {
      set_WN2F_CONTEXT_has_logical_arg(context);
      WN2F_translate(rhs_tokens, WN_kid0(wn), context);
      reset_WN2F_CONTEXT_has_logical_arg(context);
   }
   else
      WN2F_translate(rhs_tokens, WN_kid0(wn), context);

   /* See if we need to apply a "char" conversion to the rhs
    */
   if (TY_Is_Character_String(WN_ty(wn)) && 
       TY_Is_Integral(WN_Tree_Type(WN_kid0(wn))))
   {
      Prepend_Token_Special(rhs_tokens, '(');
      Prepend_Token_String(rhs_tokens, "char");
      Append_Token_Special(rhs_tokens, ')');
   }

   /* Assign the rhs to the lhs.
    */
   if (!WN2F_CONTEXT_emit_stid(context) &&
       Identical_Token_Lists(lhs_tokens, rhs_tokens))
   {
      /* Ignore this redundant assignment statement! */
      Reclaim_Token_Buffer(&lhs_tokens);
      Reclaim_Token_Buffer(&rhs_tokens);
   }
   else
   {
      /* The assignment statement on a new line */
      WN2F_Stmt_Newline(tokens, NULL/*label*/, WN_linenum(wn), context);
      Append_And_Reclaim_Token_List(tokens, &lhs_tokens);
      Append_Token_Special(tokens, '=');
      Append_And_Reclaim_Token_List(tokens, &rhs_tokens);
   }

   return EMPTY_WN2F_STATUS;
} /* WN2F_stid */

WN2F_STATUS 
WN2F_iload(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX base_ty;
   
   /* Note that we handle this just like we do the lhs of an ISTORE.
    */
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_ILOAD, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_iload"));

   /* Get the type of the base from which we are loading */
   base_ty = WN_Tree_Type(WN_kid0(wn));
   if (!TY_Is_Pointer(base_ty))
      base_ty = WN_load_addr_ty(wn);

   /* Get the object to be loaded (dereference address) */
   WN2F_Offset_Memref(tokens, 
		      WN_kid0(wn),                     /* base-symbol */
		      base_ty,                         /* base-type */
		      TY_pointed(WN_load_addr_ty(wn)), /* object-type */
		      WN_load_offset(wn),              /* object-ofst */
		      context);

   /* See if there is any prefetch information with this load, and 
    * if so insert information about it as a comment on a separate
    * continuation line.
    */
   if (W2F_Emit_Prefetch && WN_MAP_Get(WN_MAP_PREFETCH, wn))
   {
      Set_Current_Indentation(Current_Indentation()+3);
      Append_F77_Indented_Continuation(tokens);
      Append_Token_Special(tokens, '!');
      WN2F_Append_Prefetch_Map(tokens, wn);
      Set_Current_Indentation(Current_Indentation()-3); 
      Append_F77_Indented_Continuation(tokens);
   }
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_iload */

WN2F_STATUS 
WN2F_iloadx(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_WARN(FALSE, (DIAG_UNIMPLEMENTED, "WN2F_iloadx"));
   Append_Token_String(tokens, WN_opc_name(wn));

   return EMPTY_WN2F_STATUS;
} /* WN2F_iloadx */


WN2F_STATUS 
WN2F_mload(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX base_ty;
   
   /* This should only appear the as the rhs of an ISTORE.  Treat
    * it just like an ILOAD.
    */
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_MLOAD, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_mload"));

   /* Get the type of the base from which we are loading */
   base_ty = WN_Tree_Type(WN_kid0(wn));
   if (!TY_Is_Pointer(base_ty))
      base_ty = WN_ty(wn);

   /* Get the object to be loaded */
   WN2F_Offset_Memref(tokens, 
		      WN_kid0(wn),                     /* base-symbol */
		      base_ty,                         /* base-type */
		      TY_pointed(WN_ty(wn)), /* object-type */
		      WN_load_offset(wn),              /* object-ofst */
		      context);
   return EMPTY_WN2F_STATUS;
} /* WN2F_mload */


WN2F_STATUS 
WN2F_ldid(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   const BOOL deref = WN2F_CONTEXT_deref_addr(context);
   TY_IDX    base_ptr_ty;
   TY_IDX    object_ty;
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_LDID, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_ldid"));

   /* Special recognition of a LEN intrinsic call
    */
   if (WN_load_offset(wn) == 0             &&
       TY_Is_Integral(WN_ty(wn))           &&
       ST_sclass(WN_st(wn))==SCLASS_FORMAL &&
       ST_is_value_parm(WN_st(wn))         &&
       strncmp(ST_name(WN_st(wn)), ".length.", strlen(".length.")) == 0)
   {
      /* Search the current PU parameters for a name that matches the
       * part of the ST_name that follows the ".length." prefix.
       */
      ST *st_param = 
	 WN2F_Get_Named_Param(PUinfo_current_func,
			      ST_name(WN_st(wn)) + strlen(".length."));

      if (st_param != NULL)
      {
	 WN2F_Translate_StringLEN(tokens, st_param);
	 return EMPTY_WN2F_STATUS;
      }
   }

   if (ST_class(WN_st(wn)) == CLASS_PREG)
   {
      char buffer[64];
      STAB_OFFSET addr_offset = WN_load_offset(wn);
      object_ty = PUinfo_Preg_Type(ST_type(WN_st(wn)), addr_offset);
      if (addr_offset == -1) {
         switch (TY_mtype(Ty_Table[WN_ty(wn)])) {
         case MTYPE_I8:
         case MTYPE_U8:
         case MTYPE_I1:
         case MTYPE_I2:
         case MTYPE_I4:
         case MTYPE_U1:
         case MTYPE_U2:
         case MTYPE_U4:
            sprintf(buffer, "reg%d", First_Int_Preg_Return_Offset);
            Append_Token_String(tokens, buffer);
            break;
         case MTYPE_F4:
         case MTYPE_F8:
         case MTYPE_F10:
         case MTYPE_FQ:
         case MTYPE_C4:
         case MTYPE_C8:
         case MTYPE_C10:
         case MTYPE_CQ:
            sprintf(buffer, "reg%d", First_Float_Preg_Return_Offset);
            Append_Token_String(tokens, buffer);
            break;
         case MTYPE_M:
            Fail_FmtAssertion ("MLDID of Return_Val_Preg not allowed in middle"
               " of expression");
            break;
         default:
            Fail_FmtAssertion ("Unexpected type in WN2C_ldid()");
	    break;
         } 
      }
      else  
      {
         ST2F_Use_Preg(tokens, ST_type(WN_st(wn)), WN_load_offset(wn));
      } 
   } 
   else     
   {
      /* Get the base and object type symbols.
       */
      if (deref && TY_Is_Pointer(ST_type(WN_st(wn))))
      {
	 /* Expect the loaded type to be a pointer to the type of object
	  * to be dereferenced.  The only place (besides declaration sites)
	  * where we expect to have to specially handle ptr_as_array 
	  * objects.
	  */
	 if (TY_ptr_as_array(Ty_Table[WN_ty(wn)]))
	    object_ty = Stab_Array_Of(TY_pointed(WN_ty(wn)), 0/*size*/);
	 else
	    object_ty = TY_pointed(WN_ty(wn));

	 /* There are two possibilities for the base type:  A regular 
	  * pointer or a pointer to be treated as a pointer to an array.
	  * In either case, the "base_ptr_ty" is a pointer to the 
	  * derefenced base type.  
	  *
	  * Note that this does not handle a pointer to a struct to be
	  * treated as an array of structs, where the object type and
	  * offset denote a member of the struct, since WN2F_Offset_Symref() 
	  * cannot access a struct member through an array access.
	  */
	 if (TY_ptr_as_array(Ty_Table[ST_type(WN_st(wn))]))
	    base_ptr_ty = 
	       Stab_Pointer_To(Stab_Array_Of(TY_pointed(ST_type(WN_st(wn))),
							0/*size*/));
	 else
	    base_ptr_ty = ST_type(WN_st(wn));
      }
      else
      {
	 /* Either not a dereference, or possibly a dereference off a 
	  * record/map/common/equivalence field.  The base symbol is
	  * not a pointer, and any dereferencing on a field will occur
	  * in WN2F_Offset_Symref().
	  */
	 object_ty = WN_ty(wn);
	 base_ptr_ty = Stab_Pointer_To(ST_type(WN_st(wn)));
      }

      if (!deref && STAB_IS_POINTER_REF_PARAM(WN_st(wn)))
      {
	 /* Since we do not wish to dereference a load of a reference 
          * parameter, this must mean we are taking the address of the
          * parameter.
          */
	 Append_Token_String(tokens, "%loc");
	 Append_Token_Special(tokens, '(');
	 set_WN2F_CONTEXT_no_parenthesis(context);

      }

      WN2F_Offset_Symref(tokens, 
			 WN_st(wn),           /* base-symbol */
			 base_ptr_ty,         /* base-type */
			 object_ty,           /* object-type */
			 WN_load_offset(wn),  /* object-ofst */
			 context);

      if (!deref && STAB_IS_POINTER_REF_PARAM(WN_st(wn)))
      {
	 Append_Token_Special(tokens, ')');
      }
   }
   return EMPTY_WN2F_STATUS;
} /* WN2F_ldid */


WN2F_STATUS 
WN2F_lda(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
  const BOOL deref = WN2F_CONTEXT_deref_addr(context);
  
  ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_LDA, 
		   (DIAG_W2F_UNEXPECTED_OPC, "WN2F_lda"));
  ASSERT_DBG_FATAL(ST_class(WN_st(wn)) != CLASS_PREG, 
		   (DIAG_W2F_CANNOT_LDA_PREG));

  TY_IDX object_ty;
  
  if (!deref)
    {
      /* A true address-of operation */
      Append_Token_String(tokens, "%loc");
      Append_Token_Special(tokens, '(');
      set_WN2F_CONTEXT_no_parenthesis(context);
    }
  
  /* Sometimes we need to deal with buggy WHIRL code, where the TY
   * associated with an LDA is not a pointer type.  For such cases
   * we infer a type here.
   */

  if (TY_Is_Pointer(WN_ty(wn)))
    {
      object_ty = TY_pointed(WN_ty(wn));
    }
  else
    {
      /* May be wrong, but the best we can do under these exceptional */
      /* circumstances. */
	
      object_ty = ST_type(WN_st(wn));
    }

  ST * st = WN_st(wn);
  TY_IDX ty ;
  reset_WN2F_CONTEXT_deref_addr(context);

  if (ST_sym_class(st) == CLASS_BLOCK)
  {
    WN2F_Block(tokens,st,WN_lda_offset(wn),context);
  }
  else 
  {
    ty = Stab_Pointer_To(ST_type(st));

    WN2F_Offset_Symref(tokens, 
		       WN_st(wn),                           /* base-symbol */
		       ty,                                  /* base type   */
		       object_ty,                           /* object-type */
		       WN_lda_offset(wn),                   /* object-ofst */
		       context);
  }
  if (!deref)
    Append_Token_Special(tokens, ')');

  return EMPTY_WN2F_STATUS;
} /* WN2F_lda */


WN2F_STATUS
WN2F_array(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* Note that array indices have been normalized to assume the
    * array is based at index zero.  Since a base at index 1 is
    * the default for Fortran, we denormalize to base 1 here.
    */
   BOOL  deref = WN2F_CONTEXT_deref_addr(context);
   WN    * kid;
   TY_IDX ptr_ty;
   TY_IDX array_ty;


   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_ARRAY, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_array"));

   /* Only allow taking the address of an array element for F90!
    *
    */
   if (!deref) {
      Append_Token_String(tokens, "%loc(");
      set_WN2F_CONTEXT_deref_addr(context);

   }

   /* Get the array or, for ptr-as-array types, the element type */

   kid    = WN_kid0(wn);
   ptr_ty = WN_Tree_Type(kid);

   if (WN2F_Is_Address_Preg(kid,ptr_ty))
   {
       /* a preg or sym has been used as an address, usually after optimization      */
       /* don't know base type, or anything else so use OPR_ARRAY to generate bounds */

     WN2F_translate(tokens, kid, context);
     WN2F_Array_Slots(tokens,wn,context,TRUE);     
   } 
   else 
   {

     array_ty = W2F_TY_pointed(ptr_ty, "base of OPC_ARRAY");

     if (WN_opc_operator(kid) == OPR_LDID       &&
	 ST_sclass(WN_st(kid)) == SCLASS_FORMAL &&
	 !ST_is_value_parm(WN_st(kid))          &&
	 WN_element_size(wn) == TY_size(array_ty)       &&
	 WN_num_dim(wn) == 1                            &&
	 WN_opc_operator(WN_array_index(wn, 0)) == OPR_INTCONST &&
	 WN_const_val(WN_array_index(wn, 0)) == 0       &&
	 !TY_ptr_as_array(Ty_Table[WN_ty(kid)])           &&
	 (!TY_Is_Array(array_ty) || 
	  TY_size(TY_AR_etype(array_ty)) < TY_size(array_ty)))
     {
	 /* This array access is just a weird representation for an implicit
	  * reference parameter dereference.  Ignore the array indexing.
	  */

       WN2F_translate(tokens, kid, context);
     }
     else if (!TY_ptr_as_array(Ty_Table[ptr_ty]) && TY_Is_Character_String(array_ty))
     {
	 /* We assume that substring accesses are treated in the handling
	  * of intrinsic functions, except when the substrings are to be
	  * handled as integral types and thus are encountered here.
	  */

       if (!WN2F_F90_pu)
       {
	 Append_Token_String(tokens, "ichar");
	 Append_Token_Special(tokens, '(');
       }
       WN2F_String_Argument(tokens, wn, WN2F_INTCONST_ONE, context);
       if (!WN2F_F90_pu)
	 Append_Token_Special(tokens, ')');
     }
     else /* A regular array access */
     {
	   /* Get the base of the object to be indexed into, still using
	    * WN2F_CONTEXT_deref_addr(context).
	    */
       WN2F_translate(tokens, kid, context);
       reset_WN2F_CONTEXT_deref_addr(context);

       WN2F_array_bounds(tokens,wn,array_ty,context);
     }

     if (!deref)
       Append_Token_Special(tokens, ')');
   }
   return EMPTY_WN2F_STATUS;
} /* WN2F_array */


void
WN2F_Array_Slots(TOKEN_BUFFER tokens, WN *wn,WN2F_CONTEXT context,BOOL parens)
{
  INT32 dim;

  /* Gets bounds from the slots of an OPC_ARRAY node  */

  /* Append the "denormalized" indexing expressions in reverse order
   * of the way they occur in the indexing expression, since Fortran
   * employs column-major array layout, meaning the leftmost indexing
   * expression represents array elements laid out in contiguous 
   * memory locations.
   */

  if (parens)
  {
    Append_Token_Special(tokens, '(');
    set_WN2F_CONTEXT_no_parenthesis(context);
  }

  for (dim = WN_num_dim(wn)-1; dim >= 0; dim--)
  {
    (void)WN2F_Denormalize_Array_Idx(tokens, 
				     WN_array_index(wn, dim), 
				     context);
	 
    if (dim > 0)
      Append_Token_Special(tokens, ',');
  }

  if (parens)
    Append_Token_Special(tokens, ')');
}

void
WN2F_array_bounds(TOKEN_BUFFER tokens, WN *wn, TY_IDX array_ty,WN2F_CONTEXT context)
{
  /* This prints the array subscript expression. It was part of
   * WN2F_array, but was split so it could be used for bounds 
   * of structure components.
   */
 
  INT32 dim;

  Append_Token_Special(tokens, '(');
  set_WN2F_CONTEXT_no_parenthesis(context);

  if (TY_Is_Array(array_ty) && TY_AR_ndims(array_ty) >= WN_num_dim(wn))
    {
      /* Cannot currently handle differing element sizes at place of
       * array declaration versus place of array access (TODO?).
       */

      ASSERT_DBG_WARN((TY_size(TY_AR_etype(array_ty)) == WN_element_size(wn)) ||
		      WN_element_size(wn) < 0 ||
		      TY_size(TY_AR_etype(array_ty)) == 0,
		      (DIAG_UNIMPLEMENTED, 
		       "access/declaration mismatch in array element size"));

      WN2F_Array_Slots(tokens,wn,context,FALSE);

      /* We handle the case when an array is declared to have more 
       * dimensions than that given by this array addressing expression.
       */
      if (TY_AR_ndims(array_ty) > WN_num_dim(wn))
	{
	  /* Substitute in '1' for the missing dimensions */
	  for (dim = TY_AR_ndims(array_ty) - WN_num_dim(wn); dim > 0; dim--)
	    {
	      Append_Token_Special(tokens, ',');
	      Append_Token_String(tokens, "1");
	    }
	}
    }
  else /* Normalize array access to assume a single dimension */
    {
      ASSERT_DBG_WARN(!TY_Is_Array(array_ty) || TY_AR_ndims(array_ty) == 1,
		      (DIAG_UNIMPLEMENTED, 
		       "access/declaration mismatch in array dimensions"));

      WN2F_Normalize_Idx_To_Onedim(tokens, wn, context);
    }
  Append_Token_Special(tokens, ')');
}

/*----------- Character String Manipulation Translation ---------------*/
/*---------------------------------------------------------------------*/

void
WN2F_String_Argument(TOKEN_BUFFER  tokens,
		     WN           *base_parm,
		     WN           *length,
		     WN2F_CONTEXT  context)
{
   /* Append the tokens denoting the substring expression represented
    * by the base-expression.
    *
    * There are two possibilities concerning the array base 
    * expressions.  It can be a pointer to a complete character-
    * string (array) or it can be a pointer to a character within 
    * a character-string (single character).  In the first instance,
    * the offset off the base of string is zero.  In the latter 
    * case, the offset is given by the array indexing operation.
    *
    * NOTE: In some cases (notably for IO_ITEMs), we may try to 
    * derive a substring off an OPC_VINTRINSIC_CALL node or a
    * VCALL node.  This should only happend when the returned value
    * is the first argument and the length is the second argument.
    */
   WN   *base = WN_Skip_Parm(base_parm);
   WN   *lower_bnd;
   WN   *arg_expr;
   TY_IDX str_ty;
   INT64 str_length;

   /* Skip any INTR_ADRTMP and INTR_VALTMP nodes */
   if (WN_opc_operator(base) == OPR_INTRINSIC_OP &&
       (INTR_is_adrtmp(WN_intrinsic(base)) || 
	INTR_is_valtmp(WN_intrinsic(base))))
   {
      base = WN_kid0(base);
   }

   if (WN_operator(base) == OPR_CVTL)  
   {
     /* probably CHAR(INT) within IO stmt. convert via CHAR & process rest elsewhere */

      Append_Token_Special(tokens, '(');
      Append_Token_String(tokens, "char");
      WN2F_translate(tokens,WN_kid0(base),context);
      Append_Token_Special(tokens, ')');
      return;
   }
   
   /* Handle VCALLs specially, since the string information is given
    * by the first two arguments to the call.  Note that we can 
    * always assume a lower bound of zero for these, as we never 
    * generate code for the return-address.  This should only occur
    * within an IO stmt.  Note that the type of VCALLs must be 
    * accessed in the context of an ADRTMP or VALTMP.
    */
   if (WN_opcode(base) == OPC_VCALL ||
       WN_opcode(base) == OPC_VINTRINSIC_CALL)
   {
      arg_expr  = WN_Skip_Parm(WN_kid1(base));
      lower_bnd = WN2F_INTCONST_ZERO;

      /* fixed size string? */

      if (WN_opc_operator(arg_expr) == OPR_INTCONST)
	 str_length = WN_const_val(arg_expr);
      else
	 str_length = -1 ;  

      set_WN2F_CONTEXT_deref_addr(context);
      WN2F_translate(tokens, base, context);
      reset_WN2F_CONTEXT_deref_addr(context);

   } 
   else 
   {
     /* A regular address expression as base */

      WN2F_Get_Substring_Info(&base, &str_ty, &lower_bnd);

      /* Was this a character component of an array of derived type? */
      /* eg: vvv(2)%ccc(:)(1:5) - offset to ccc is added above base, */
      /* ADD(8,ARRAY(2,LDA VVV)) with array section for CCC on top   */
      /* of the ADD, and the substring above the array section. Take */
      /* the substring off the top, and process the rest             */

      if (TY_kind(str_ty) == KIND_STRUCT) 
      {
	FLD_PATH_INFO *fld_path ;
	FLD_HANDLE fld;
	TY_IDX  ty_idx ; 

	TY & ty = New_TY(ty_idx);

	TY_Init (ty, 1, KIND_SCALAR, MTYPE_U1, Save_Str(".w2fch."));
	Set_TY_is_character(ty);

	fld_path = TY2F_Get_Fld_Path(str_ty, 
				     ty_idx,
				     WN2F_Sum_Offsets(base));

        fld = TY2F_Last_Fld(fld_path);
	TY2F_Free_Fld_Path(fld_path);

	/* call memref for FLD offset, otherwise the ADD is */
	/* just another binary op                           */

	WN2F_Offset_Memref(tokens, 
			   WN_kid0(base),
			   WN_Tree_Type(base),
                           FLD_type(fld),
			   0,
			   context);
      } 
      else 
      {
	str_length = TY_size(str_ty);

        /* with optimization, may not have useful address TY 
         * when TreeType will return array of U1 from SubstringInfo */

	ASSERT_DBG_WARN(TY_Is_Character_String(str_ty) || TY_Is_Array_Of_UChars(str_ty),
			(DIAG_W2F_EXPECTED_PTR_TO_CHARACTER,
			 "WN2F_String_Argument"));


	/* Get the string base and substring notation for the argument.  */

	set_WN2F_CONTEXT_deref_addr(context);
	WN2F_translate(tokens, base, context);
	reset_WN2F_CONTEXT_deref_addr(context);
      }

      WN2F_Substring(tokens, 
		     str_length,
		     lower_bnd,
		     WN_Skip_Parm(length),
		     context);
      return ;
   }
} /* WN2F_String_Argument */


/*----------- Miscellaneous  routines ---------------------------------*/
/*---------------------------------------------------------------------*/

static void
WN2F_Block(TOKEN_BUFFER tokens, ST * st, STAB_OFFSET offset,WN2F_CONTEXT context)
{
  /* An ST of CLASS_BLOCK may appear in f90 IO, at -O2 */
  /* put out something for the whirl browser           */

  ST2F_use_translate(tokens,st);

  if (offset != 0)
  {
      Append_Token_Special(tokens, '+');
      Append_Token_String(tokens, Number_as_String(offset, "%lld"));
  }
}
