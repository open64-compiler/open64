/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifdef __MINGW32__
#include <WINDOWS.h>
#endif /* __MINGW32__ */
#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>                        // Elf64_Word
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>                  // ir_bwrite.h needs it
#include "wn_util.h"
#include "lwn_util.h"
#include "ipa_section.h"
#include "ipa_lno_file.h"
#include "ipl_lno_util.h"
#include "ipl_summarize.h"
#include "ipl_array_bread_write.h"

//-----------------------------------------------------------------------
// NAME: Machine_Type
// FUNCTION: Given an argument passed to a call 'wn_argument', determine 
//   return the machine type of its base type.
//-----------------------------------------------------------------------
extern TYPE_ID 
Machine_Type(WN* wn_argument)
{
  INT mtype = MTYPE_UNKNOWN;
  if (WN_operator(wn_argument) == OPR_PARM)
    wn_argument = WN_kid0(wn_argument);
  if (!OPCODE_has_sym(WN_opcode(wn_argument)))
    return mtype; 
  ST* st_formal = WN_st(wn_argument);
  TY_IDX ty_idx_formal = ST_type(st_formal);
  while (TY_kind(ty_idx_formal) == KIND_POINTER)
    ty_idx_formal = TY_pointed(ty_idx_formal);
  if (TY_kind(ty_idx_formal) == KIND_ARRAY)
    mtype = TY_mtype(TY_etype(ty_idx_formal));
  else
    mtype = TY_mtype(ty_idx_formal);
  return mtype; 
}

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

extern SUMMARY *Summary;
extern ARRAY_SUMMARY Array_Summary;

// ---------------------------------------------------------
// Find the position of the formal parameter in the function
// ---------------------------------------------------------
extern INT32
Formal_Position (const ST* formal_st)
{
  Is_True(ST_sclass(formal_st) == SCLASS_FORMAL || 
          ST_sclass(formal_st) == SCLASS_FORMAL_REF,
          ("Expected a formal ST"));

  FmtAssert(Current_PU_Info, ("Current_PU_Info is not set"));
  WN* func_entry = PU_Info_tree_ptr(Current_PU_Info);
  FmtAssert(func_entry, ("Function entry is not set"));

  for (INT32 pos = 0; pos < WN_num_formals(func_entry); ++pos) {
    if (WN_st(WN_formal(func_entry, pos)) == formal_st) {
      return pos;
    }
  }
  Fail_FmtAssertion("Couldn't find formal %s in function %s",
                    ST_name(formal_st), ST_name(WN_st(func_entry)));
  return -1;
}

//-----------------------------------------------------------------------
// NAME: Node_Count
// FUNCTION: Return the node count of the tree rooted at 'wn_node'.  If
//   'limit' > 0, stop counting when you get over 'limit' nodes. If
//   'symbol_only' is true, count only nodes with symbols.
//-----------------------------------------------------------------------

extern INT Node_Count(WN* wn_node,
                      INT limit,
                      BOOL symbol_only)
{
  INT count = 0;
  if (!symbol_only || OPCODE_has_sym(WN_opcode(wn_node)))
    count++;
  if (limit > 0 && count > limit)
    return count;
  if (WN_opcode(wn_node) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_node); wn != NULL; wn = WN_next(wn)) {
      count += Node_Count(wn, limit, symbol_only);
      if (limit > 0 && count > limit)
        return count;
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_node); i++) {
      count += Node_Count(WN_kid(wn_node, i), limit, symbol_only);
      if (limit > 0 && count > limit)
        return count;
    }
  }
  return count;
}

//-----------------------------------------------------------------------
// NAME: True_Bound
// FUNCTION: Given the array bound expression 'wn_exp' in the subprogram
//   'wn_func', return an equivalent expression which uses original rather
//   than frozen symbol values in the LDIDs.  Get memory for the expression
//   from 'mem_pool'.
//-----------------------------------------------------------------------

extern WN* True_Bound(WN* wn_func,
                      ST_IDX st_idx_exp)
{
  WN_ITER *wni = WN_WALK_TreeIter(wn_func);
  WN* wn = NULL;
  for (; wni != NULL; wni = WN_WALK_TreeNext(wni)) {
    wn = wni->wn;
    if (WN_operator(wn) == OPR_STID && WN_st_idx(wn) == st_idx_exp)
      break;
  }
  if (wni == NULL)
    return NULL; 
  WN* wn_exp = WN_kid0(wn);
  return LWN_Copy_Tree(wn_exp);
}

//-----------------------------------------------------------------------
// NAME: Is_Constant_Tree
// FUNCTION: If 'wn_exp' is a tree all of whose leaves are INTCONSTs,
//   (and whose interior nodes are of a few simple types), then return
//   TRUE and set '*const_value' to the value of 'wn_exp'.  Otherwise
//   return FALSE.
//-----------------------------------------------------------------------

static BOOL Is_Constant_Tree(WN* wn_exp,
                             INT64* const_value)
{
  INT64 const_left, const_right;
  switch (WN_operator(wn_exp)) {
  case OPR_ADD:
    if (!Is_Constant_Tree(WN_kid0(wn_exp), &const_left))
      return FALSE;
    if (!Is_Constant_Tree(WN_kid1(wn_exp), &const_right))
      return FALSE;
    *const_value = const_left + const_right;
    break;
  case OPR_SUB:
    if (!Is_Constant_Tree(WN_kid0(wn_exp), &const_left))
      return FALSE;
    if (!Is_Constant_Tree(WN_kid1(wn_exp), &const_right))
      return FALSE;
    *const_value = const_left - const_right;
    break;
  case OPR_MPY:
    if (!Is_Constant_Tree(WN_kid0(wn_exp), &const_left))
      return FALSE;
    if (!Is_Constant_Tree(WN_kid1(wn_exp), &const_right))
      return FALSE;
    *const_value = const_left * const_right;
    break;
  case OPR_NEG:
    if (!Is_Constant_Tree(WN_kid0(wn_exp), &const_left))
      return FALSE;
    *const_value = -const_left;
    break;
  case OPR_INTCONST:
    *const_value = WN_const_val(wn_exp);
    break;
  default:
    return FALSE;
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Is_Exp_Linexable
// FUNCTION: Returns TRUE if 'wn_exp' can be converted to a LINEX (easily)
//   FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Is_Exp_Linexable(WN* wn_exp, 
			     BOOL Is_LNO)
{
  INT64 dummy_const;
  switch (WN_operator(wn_exp)) {
  case OPR_ADD:
  case OPR_SUB:
  case OPR_NEG:
    break;
  case OPR_MPY:
    if (Is_Constant_Tree(WN_kid0(wn_exp), &dummy_const))
      return Is_Exp_Linexable(WN_kid1(wn_exp), Is_LNO);
    if (Is_Constant_Tree(WN_kid1(wn_exp), &dummy_const))
      return Is_Exp_Linexable(WN_kid0(wn_exp), Is_LNO);
    return FALSE;
  case OPR_INTCONST:
    return TRUE;
  case OPR_LDID: 
    if (Is_LNO)
      return TRUE;
    else {
      const ST* st_exp = WN_st(wn_exp);
      return (ST_class(st_exp) == CLASS_VAR &&
              (ST_level(st_exp) == GLOBAL_SYMTAB ||
               (ST_level(st_exp) == CURRENT_SYMTAB &&
                (ST_sclass(st_exp) == SCLASS_FORMAL ||
                 ST_sclass(st_exp) == SCLASS_FORMAL_REF))));
    }
  default:
    return FALSE;
  }
  for (INT i = 0; i < WN_kid_count(wn_exp); i++)
    if (!Is_Exp_Linexable(WN_kid(wn_exp, i), Is_LNO))
      return FALSE;
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Exp_To_Linex_Array
// FUNCTION: Place on the TERM_ARRAY 'terms' the equivalent LINEX repre-
//   sentation of 'wn_exp'.  Take needed meory from 'mem_pool'.
//-----------------------------------------------------------------------

static void Exp_To_Linex_Array(WN* wn_exp,
                               TERM_ARRAY* terms,
                               MEM_POOL* mem_pool,
			       BOOL Is_LNO,
			       IPA_LNO_READ_FILE* IPA_LNO_File)
{
  INT i = 0;
  INT idx = -1;
  INT64 const_value = 0;
  TERM* tm = NULL;
  TERM_ARRAY tm_left(mem_pool);
  TERM_ARRAY tm_right(mem_pool);
  OPERATOR opr = WN_operator(wn_exp);
  switch (opr) {
  case OPR_ADD:
    Exp_To_Linex_Array(WN_kid0(wn_exp), &tm_left, mem_pool, Is_LNO,
      IPA_LNO_File);
    Exp_To_Linex_Array(WN_kid1(wn_exp), &tm_right, mem_pool, Is_LNO,
      IPA_LNO_File);
    for (i = 0; i <= tm_left.Lastidx(); i++) {
      idx = terms->Newidx();
      (*terms)[idx] = tm_left[i];
    }
    for (i = 0; i <= tm_right.Lastidx(); i++) {
      idx = terms->Newidx();
      (*terms)[idx] = tm_right[i];
    }
    tm_left.Free_array();
    tm_right.Free_array();
    break;
  case OPR_SUB:
    Exp_To_Linex_Array(WN_kid0(wn_exp), &tm_left, mem_pool, Is_LNO,
      IPA_LNO_File);
    Exp_To_Linex_Array(WN_kid1(wn_exp), &tm_right, mem_pool, Is_LNO,
      IPA_LNO_File);
    for (i = 0; i <= tm_left.Lastidx(); i++) {
      idx = terms->Newidx();
      (*terms)[idx] = tm_left[i];
    }
    for (i = 0; i <= tm_right.Lastidx(); i++) {
      idx = terms->Newidx();
      tm_right[i].Set_coeff(-tm_right[i].Get_coeff());
      (*terms)[idx] = tm_right[i];
    }
    tm_left.Free_array();
    tm_right.Free_array();
    break;
  case OPR_NEG:
    Exp_To_Linex_Array(WN_kid0(wn_exp), &tm_left, mem_pool, Is_LNO,
      IPA_LNO_File);
    for (i = 0; i <= tm_left.Lastidx(); i++) {
      idx = terms->Newidx();
      tm_left[i].Set_coeff(-tm_left[i].Get_coeff());
      (*terms)[idx] = tm_left[i];
    }
    tm_left.Free_array();
    break;
  case OPR_MPY: {
    WN* wn_base = NULL;
    if (Is_Constant_Tree(WN_kid0(wn_exp), &const_value))
      wn_base = WN_kid1(wn_exp);
    else if (Is_Constant_Tree(WN_kid1(wn_exp), &const_value))
      wn_base = WN_kid0(wn_exp);
    FmtAssert(wn_base != NULL,
      ("Exp_To_Linex_Array: Should have screened this out"));
    Exp_To_Linex_Array(wn_base, &tm_left, mem_pool, Is_LNO, IPA_LNO_File);
    for (i = 0; i <= tm_left.Lastidx(); i++) {
      idx = terms->Newidx();
      tm_left[i].Set_coeff((COEFF) const_value * tm_left[i].Get_coeff());
      (*terms)[idx] = tm_left[i];
    }
    tm_left.Free_array();
    }
    break;
  case OPR_INTCONST:
    const_value = WN_const_val(wn_exp);
    terms->AddElement(TERM(LTKIND_CONST, (COEFF) const_value, CONST_DESC, 0));
    break;
  case OPR_LDID: {
    const ST* st = WN_st(wn_exp);
    WN_OFFSET offset = WN_offset(wn_exp);
    TYPE_ID mtype = WN_rtype(wn_exp);
    INT32 ivar_idx;
    IVAR ivar;

    if (Is_LNO) { 
      new (&ivar) IVAR(st, offset, mtype);
      ivar_idx = IPA_LNO_File->Add_Translated_Ivar_Unique(ivar);
    } 
    else {
      if (ST_IDX_level(ST_st_idx(st)) == GLOBAL_SYMTAB) {
        new (&ivar) IVAR(st, offset, mtype);
      }
      else if (ST_sclass(st) == SCLASS_FORMAL ||
               ST_sclass(st) == SCLASS_FORMAL_REF) {
        UINT32 position = Formal_Position(st);
        new (&ivar) IVAR(position, offset, mtype);
      }
      IVAR_ARRAY& ivar_array = *Array_Summary_Output->Get_ivar_array();
      for (ivar_idx = 0; ivar_idx < ivar_array.Elements(); ivar_idx++) {
	if (ivar_array[ivar_idx] == ivar) {
	  break;
        }
      }
      if (ivar_idx == ivar_array.Elements()) {
	ivar_array.AddElement(ivar);
      }
    }
    terms->AddElement(TERM(LTKIND_IV, (COEFF) 1, ivar_idx, 0));
    break;
  }
  default:
    FmtAssert(TRUE, ("Exp_To_Linex_Array: Should have screened this out"));
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: Exp_To_Linex
// FUNCTION: Convert 'wn_exp' to LINEX form, adding terms to the LINEX i
//   'lx_exp'.  Return TRUE if the conversion worked, FALSE otherwise.
//-----------------------------------------------------------------------

extern BOOL Exp_To_Linex(WN* wn_exp,
                         LINEX* lx_exp,
                         MEM_POOL* mem_pool,
			 BOOL negate,
			 BOOL Is_LNO, 
			 IPA_LNO_READ_FILE* IPA_LNO_File)
{
  TERM_ARRAY terms(mem_pool);
  if (!Is_Exp_Linexable(wn_exp, Is_LNO))
    return FALSE;
  Exp_To_Linex_Array(wn_exp, &terms, mem_pool, Is_LNO, IPA_LNO_File);
  for (INT i = 0; i <= terms.Lastidx(); i++) {
    TERM* tm = &terms[i];
    if (negate)
      tm->Set_coeff(-tm->Get_coeff());
    lx_exp->Set_term(tm);
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Projected_Region_From_St
// FUNCTION: For the array variable with the given 'st' in the function 
//   'wn_func' use memory from 'mem_pool' to construct a projected region 
//   its declaration.  Return NULL if the variable is not an array.
// NOTE: This has been changed so that the PROJECTED_REGION created is 
//   "zero-based". 
//-----------------------------------------------------------------------

extern PROJECTED_REGION* Projected_Region_From_St(WN* wn_func, 
						  ST* st,
					          MEM_POOL* mem_pool,
						  BOOL Is_LNO, 
						  IPA_LNO_READ_FILE* 
						    IPA_LNO_File)
{
  TY_IDX ty_idx = ST_type(st);
  if (TY_kind(ty_idx) == KIND_POINTER)
    ty_idx = TY_pointed(ty_idx);
  if (TY_kind(ty_idx) != KIND_ARRAY)
    return NULL;
  INT dim_count = TY_AR_ndims(ty_idx);
  PROJECTED_REGION* pr = 
    CXX_NEW(PROJECTED_REGION(NON_MESSY_REGION, 0, dim_count, mem_pool),
            mem_pool);
  pr->Set_is_formal();
  pr->Reset_is_unprojected();
  PROJECTED_ARRAY* pa = pr->Get_projected_array();
  for (INT j = 0; j < dim_count; j++) {
    PROJECTED_NODE* pn = &(*pa)[j];
    pn->Init(mem_pool);
    pn->Reset_is_unprojected();
    LINEX* lx_lb = pn->Get_lower_linex();
    mINT64 lb_const = 0LL;
    lx_lb->Set_term(LTKIND_CONST, (COEFF) lb_const, CONST_DESC, 0);
    LINEX* lx_ub = pn->Get_upper_linex();
    if (TY_AR_const_lbnd(ty_idx, j)) {
      mINT64 lb_const = TY_AR_lbnd_val(ty_idx, j);
      lx_ub->Set_term(LTKIND_CONST, (COEFF) -lb_const, CONST_DESC, 0);
    } else {
      ST_IDX st_idx_lb = TY_AR_lbnd_var(ty_idx, j);
      if (st_idx_lb == (ST_IDX)0) {
	FmtAssert(j == 0,
	  ("Process_Array_Formals: Expecting assumed shape array"));
	pn->Set_assumed_shape();
      } else {
	WN* wn_lb_true = True_Bound(wn_func, st_idx_lb);
        if (wn_lb_true == NULL) {
	  FmtAssert(j == 0,
            ("Process_Array_Formals: Expecting assumed shape array"));
	  pn->Set_assumed_shape();
	} else { 
	  BOOL ok = Exp_To_Linex(wn_lb_true, lx_ub, mem_pool, TRUE,
	    Is_LNO, IPA_LNO_File);
	  if (!ok)
	    pn->Set_messy_ub();
	} 
      }
    }
    if (!pn->Is_messy_ub()) {  
      if (TY_AR_const_ubnd(ty_idx, j)) {
	mINT64 ub_const = TY_AR_ubnd_val(ty_idx, j);
	lx_ub->Set_term(LTKIND_CONST, (COEFF) ub_const, CONST_DESC, 0);
      } else {
	ST_IDX st_idx_ub = TY_AR_ubnd_var(ty_idx, j);
	if (st_idx_ub == (ST_IDX)0) {
	  FmtAssert(j == 0,
	    ("Process_Array_Formals: Expecting assumed shape array"));
	  pn->Set_assumed_shape();
	} else {
	  WN* wn_ub_true = True_Bound(wn_func, st_idx_ub);
	  if (wn_ub_true == NULL) { 
	    FmtAssert(j == 0,
	      ("Process_Array_Formals: Expecting assumed shape array"));
	   pn->Set_assumed_shape();
	  } else { 
	    BOOL ok = Exp_To_Linex(wn_ub_true, lx_ub, mem_pool, FALSE,
	      Is_LNO, IPA_LNO_File);
	    if (!ok)
	      pn->Set_messy_ub();
	  } 
	}
      }
      lx_ub->Simplify();
    }
    LINEX* lx_stride = pn->Get_step_linex();
    mINT64 stride_const = 1LL;
    lx_stride->Set_term(LTKIND_CONST, (COEFF) stride_const, CONST_DESC, 0);
  }
  return pr;
}

//-----------------------------------------------------------------------
// NAME: Projected_Region_From_Access_Array
// FUNCTION: Return a projected region from the access array 'aa' using 
//   memory from the 'mem_pool'.
//-----------------------------------------------------------------------

extern PROJECTED_REGION* Projected_Region_From_Access_Array(ACCESS_ARRAY* aa,
  MEM_POOL* mem_pool, IPA_LNO_READ_FILE* IPA_LNO_File)
{
  return CXX_NEW(PROJECTED_REGION(aa, mem_pool, NULL, FALSE, 
    IPA_LNO_File), mem_pool);
}

 
#endif // _STANDALONE_INLINER
