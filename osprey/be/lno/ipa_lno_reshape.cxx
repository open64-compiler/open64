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


#include <sys/types.h>
#include <limits.h>
#include <stdio.h>
#include "pu_info.h"
#include "defs.h"
#include "wn.h"
#include "lwn_util.h"
#include "strtab.h"
#include "ipa_section.h"
#include "ipa_lno_summary.h"
#include "ipa_lno_info.h"
#include "ara_loop.h"
#include "lnopt_main.h"
#include "opt_du.h"
#include "debug.h"
#include "ipa_lno_util.h"
#include "ipa_lno_reshape.h"
#include "ipa_section.h" 
#include "ipl_lno_util.h" 
#include "lnoutils.h"
#include "be_util.h"
#include "dep_graph.h"
#include "ipa_lno_read.h"

//-----------------------------------------------------------------------
// NAME: LINEX::LNO_Simplify
// FUNCTION: Simplify the LINEX, evaluating it at the 'wn_call'. 
//-----------------------------------------------------------------------

void LINEX::LNO_Simplify(IPA_LNO_READ_FILE* IPA_LNO_File,
                         WN* wn_call)
{
  DU_MANAGER* du = Du_Mgr; 
  MEM_POOL* mem_pool = &LNO_local_pool;
  MEM_POOL_Push(mem_pool);
  {
    STACK<INT> stk_int(mem_pool);
    INT64 const_value = 0;
    INT i;
    for (i = 0; i <= Num_terms(); i++) {
      TERM* tm = Get_term(i);
      switch (tm->Get_type()) {
      case LTKIND_CONST:
        const_value += tm->Get_coeff();
        stk_int.Push(i);
        break;
      case LTKIND_IV:
        {
          IVAR* iv = IPA_LNO_File->Ivar(tm->Get_desc());
	  if (iv->Is_Formal()) {
	    INT formal_number = iv->Formal_Position();
	    FmtAssert(formal_number >= 0 
	      && formal_number < WN_kid_count(wn_call),
	      ("LNO_Simplify: Formal number out of range"));
	    WN* wn_parm = WN_kid(wn_call, formal_number);
	    FmtAssert(WN_operator(wn_parm) == OPR_PARM,
	      ("Map_Term: Expecting PARAM node"));
	    WN* wn_lda = WN_kid0(wn_parm);
	    if (WN_operator(wn_lda) == OPR_LDA
		|| WN_operator(wn_lda) == OPR_LDID) {
	      INT64 integer_value = -1;
	      if (Wn_Is_Intconst(wn_lda, &integer_value)) {
		const_value += integer_value;
		stk_int.Push(i);
	      } else {
		WN* wn_chain = WN_operator(wn_lda) == OPR_LDA
		  ? wn_parm : wn_lda;
		DEF_LIST* def_list = du->Ud_Get_Def(wn_chain);
		if (def_list != NULL && !def_list->Incomplete()) {
		  DEF_LIST_ITER iter(def_list);
		  const DU_NODE* node = NULL;
		  INT def_count = 0;
		  WN* wn_def = NULL;
		  for (node = iter.First(); !iter.Is_Empty();
		      node = iter.Next()) {
		    wn_def = node->Wn();
		    def_count++;
		  }
		  if (def_count == 1 && WN_kid_count(wn_def) > 0) {
		    WN* wn_value = WN_kid0(wn_def);
		    if (WN_operator(wn_value) == OPR_INTCONST) {
		      const_value += WN_const_val(wn_value);
		      stk_int.Push(i);
		    } else if (WN_operator(wn_value) == OPR_LDID
			&& Wn_Is_Intconst(wn_value, &integer_value)) {
		      const_value += integer_value;
		      stk_int.Push(i);
		    }
		  }
		}
	      }
	    }
	  }
        } 
        break;
      default:
        break;
      }
    }
    DYN_ARRAY<TERM> new_terms(mem_pool);
    for (i = 0; i <= Num_terms(); i++) {
      INT j;
      for (j = 0; j < stk_int.Elements(); j++)
        if (stk_int.Bottom_nth(j) == i)
          break;
      if (j < stk_int.Elements())
        continue;
      TERM* tm = Get_term(i);
      new_terms.AddElement(*tm);
    }
    _larray.Resetidx();
    for (i = 0; i < new_terms.Elements(); i++) {
      TERM* tm = &new_terms[i];
      _larray.AddElement(*tm);
    }
    if (const_value != 0 || new_terms.Elements() == 0)
      Set_term(LTKIND_CONST, const_value, CONST_DESC, 0);
  }
  MEM_POOL_Pop(mem_pool);
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_NODE::LNO_Simplify
// FUNCTION: Simplify the LINEXes in the PROJECTED_NODE, evaluating 
//   them at 'wn_call'.
//-----------------------------------------------------------------------

void PROJECTED_NODE::LNO_Simplify(IPA_LNO_READ_FILE* IPA_LNO_File,
				  WN* wn_call)
{ 
  if (!Is_messy_lb()) { 
    LINEX* lx_lower = Get_lower_linex();
    lx_lower->LNO_Simplify(IPA_LNO_File, wn_call);
  }
  if (!Is_messy_ub()) { 
    LINEX* lx_upper = Get_upper_linex();
    lx_upper->LNO_Simplify(IPA_LNO_File, wn_call);
  } 
  if (!Is_messy_step()) {
    LINEX* lx_step = Get_step_linex();
    lx_step->LNO_Simplify(IPA_LNO_File, wn_call);
  } 
  if (Get_segment_length_linex() != NULL) { 
    LINEX* lx_segment_length = Get_segment_length_linex();
    lx_segment_length->LNO_Simplify(IPA_LNO_File, wn_call);
  } 
  if (Get_segment_stride_linex() != NULL) { 
    LINEX* lx_segment_stride = Get_segment_stride_linex();
    lx_segment_stride->LNO_Simplify(IPA_LNO_File, wn_call);
  } 
} 

//-----------------------------------------------------------------------
// NAME: PROJECTED_REGION::LNO_Simplify
// FUNCTION: Simplify the LINEXes in the PROJECTED_REGION, evaluating
//   them at 'wn_call'.
//-----------------------------------------------------------------------

void PROJECTED_REGION::LNO_Simplify(IPA_LNO_READ_FILE* IPA_LNO_File,
                                    WN* wn_call)
{ 
  if (Is_messy_region()) 
    return; 
  for (INT i = 0; i < Get_num_dims(); i++) {
    PROJECTED_NODE* pn = Get_projected_node(i);
    pn->LNO_Simplify(IPA_LNO_File, wn_call);
  } 
} 


//-----------------------------------------------------------------------
// NAME: Is_Scalar
// FUNCTION: Returns TRUE if 'st_formal' is scalar, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Scalar(ST* st_formal)
{
  TY_IDX ty_idx_formal = ST_type(st_formal);
  if (TY_kind(ty_idx_formal) == KIND_POINTER)
    ty_idx_formal = TY_pointed(ty_idx_formal);
  if (TY_kind(ty_idx_formal) != KIND_ARRAY)
    return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Can_Map_Term
// FUNCTION: Returns TRUE if it is possible to map the callee TERM 'tm' 
//   to the caller at callsite 'wn_call'.  Return FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Can_Map_Term(IPA_LNO_READ_FILE* IPA_LNO_File,
                         TERM* tm, 
                         WN* wn_call)
{
  switch (tm->Get_type()) {
  case LTKIND_CONST:
  case LTKIND_LINDEX: 
  case LTKIND_SUBSCR: 
    return TRUE; 
  case LTKIND_IV: {
    IVAR* iv = IPA_LNO_File->Ivar(tm->Get_desc());
    if (iv->Is_Formal()) {
      INT formal_number = iv->Formal_Position();
      if (formal_number < 0 || formal_number >= WN_kid_count(wn_call))
	return FALSE; 
      WN* wn_parm = WN_kid(wn_call, formal_number);
      if (WN_operator(wn_parm) == OPR_PARM) {
        WN* wn_lda = WN_kid0(wn_parm);
	if (WN_operator(wn_lda) != OPR_LDA && 
            WN_operator(wn_lda) != OPR_LDID && 
            WN_operator(wn_lda) != OPR_INTCONST) {
	  DYN_ARRAY<WN*> wn_list(&LNO_local_pool);
	  DYN_ARRAY<INT> int_list(&LNO_local_pool);
	  INT64 const_value = 0;
	  if (!Scalar_Expr(wn_lda) || !Linear_Expr(wn_lda, 
	      &wn_list, &int_list, &const_value))
	    return FALSE; 
	} 
        return TRUE;
      } else  
        return FALSE; 
    } else { 
      return TRUE; 
    } 
  } 
  default: 
    FmtAssert(FALSE, ("Can_Map_Term: Unknown term type"));
    break;
  } 
  return FALSE;
} 

//-----------------------------------------------------------------------
// NAME: Can_Map_Linex
// FUNCTION: Returns TRUE if it is possible to map the callee LINEX 'lx' 
//   to the caller at callsite 'wn_call'.  Return FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Can_Map_Linex(IPA_LNO_READ_FILE* IPA_LNO_File,
			  LINEX* lx, 
			  WN* wn_call)
{
  for (INT i = 0; i <= lx->Num_terms(); i++)
    if (!Can_Map_Term(IPA_LNO_File, lx->Get_term(i), wn_call))
      return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Can_Map_Projected_Region
// FUNCTION: Returns TRUE if it is possible to map the PROJECTED_REGION
//   'pr_old' to the caller at callsite 'wn_call'.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Can_Map_Projected_Region(IPA_LNO_READ_FILE* IPA_LNO_File,
				     PROJECTED_REGION* pr_old,
				     WN* wn_call)
{
  if (pr_old->Is_messy_region())
    return FALSE;
  for (INT i = 0; i < pr_old->Get_num_dims(); i++) {
    PROJECTED_NODE* pn = pr_old->Get_projected_node(i);
    if (!pn->Is_messy_ub()) {
      LINEX* ub = pn->Get_upper_linex();
      if (ub != NULL && !Can_Map_Linex(IPA_LNO_File, ub, wn_call))
	  return FALSE;  
    }
    if (!pn->Is_messy_lb()) {
      LINEX* lb = pn->Get_lower_linex();
      if (lb != NULL && !Can_Map_Linex(IPA_LNO_File, lb, wn_call))
	  return FALSE; 
    }
    if (!pn->Is_messy_step()) {
      LINEX* step = pn->Get_step_linex();
      if (step != NULL && !Can_Map_Linex(IPA_LNO_File, step, wn_call))
          return FALSE;
    }
    if (pn->Get_segment_length_linex() != NULL) {
      LINEX* sl = pn->Get_segment_length_linex();
      if (sl != NULL && !Can_Map_Linex(IPA_LNO_File, sl, wn_call))
          return FALSE;
    } 
    if (pn->Get_segment_stride_linex() != NULL) {
      LINEX* ss = pn->Get_segment_stride_linex();
      if (ss != NULL && !Can_Map_Linex(IPA_LNO_File, ss, wn_call))
          return FALSE;
    } 
  }
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Map_Linear_Expr
// FUNCTION: Map the callee TERM 'tm' to the caller through the callsite
//   parameter 'wn_lda' adding additional terms to 'lx' if needed, 
//   provided that the 'wn_lda' is a single definition temp that is 
//   expressable as a linear expression.  
//-----------------------------------------------------------------------

static BOOL Map_Linear_Expr(IPA_LNO_READ_FILE* IPA_LNO_File,
			    LINEX* lx,
			    TERM* tm,
			    WN* wn_expr)
{
  IVAR* ivar = IPA_LNO_File->Ivar(tm->Get_desc());
  DYN_ARRAY<WN*> wn_list(&LNO_local_pool);
  DYN_ARRAY<INT> int_list(&LNO_local_pool);
  INT64 const_value = 0;
  if (!Scalar_Expr(wn_expr) || !Linear_Expr(wn_expr, &wn_list, &int_list, 
       &const_value))
    return FALSE; 
  new (tm) TERM(LTKIND_CONST, const_value, CONST_DESC, 0);
  for (INT i = 0; i <= wn_list.Lastidx(); i++) { 
    WN* wn = wn_list[i]; 
    INT coeff = int_list[i]; 
    INT64 integer_value = -1;
    if (Wn_Is_Intconst(wn, &integer_value)) {
      lx->Set_term(LTKIND_CONST,  coeff * integer_value, CONST_DESC, 0);
    } else { 
      IVAR iv_lcl(WN_st(wn), ivar->Offset(), ivar->Mtype());
      INT new_ivar_idx = IPA_LNO_File->Add_Translated_Ivar_Unique(iv_lcl);
      lx->Set_term(LTKIND_IV, coeff, new_ivar_idx, 0);
    } 
  }
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Map_Term
// FUNCTION: Map the callee TERM 'tm' to the caller through the callsite
//   'wn_call'. 
//-----------------------------------------------------------------------

static void Map_Term(IPA_LNO_READ_FILE* IPA_LNO_File,
		     LINEX* lx, 
                     TERM* tm,
                     WN* wn_call)
{
  switch (tm->Get_type()) {
  case LTKIND_CONST:
  case LTKIND_LINDEX: 
  case LTKIND_SUBSCR: 
    break; 
  case LTKIND_IV: {
    IVAR* ivar = IPA_LNO_File->Ivar(tm->Get_desc());
    if (ivar->Is_Formal()) {
      INT formal_number = ivar->Formal_Position();
      WN* wn_parm = WN_kid(wn_call, formal_number);
      FmtAssert(WN_operator(wn_parm) == OPR_PARM, 
	("Map_Term: Expecting PARAM node"));
      WN* wn_lda = WN_kid0(wn_parm);
      if (WN_operator(wn_lda) == OPR_LDA || WN_operator(wn_lda) == OPR_LDID) {
	INT64 integer_value = -1;
	if (Wn_Is_Intconst(wn_lda, &integer_value)) {
	  new (tm) TERM(LTKIND_CONST, integer_value, CONST_DESC, 0);
        } else { 
	  WN* wn_single = Single_Definition_Temp(wn_lda);
	  if (wn_single == NULL 
	      || !Map_Linear_Expr(IPA_LNO_File, lx, tm, wn_single)) { 
	    IVAR iv_lcl(WN_st(wn_lda), ivar->Offset(), ivar->Mtype());
	    INT new_ivar_idx = IPA_LNO_File->Add_Translated_Ivar_Unique(iv_lcl);
	    tm->Set_desc(new_ivar_idx);
	  } 
	} 
      } else if (WN_operator(wn_lda) == OPR_INTCONST) { 
	new (tm) TERM(LTKIND_CONST, WN_const_val(wn_lda), CONST_DESC, 0);
      } else { 
	BOOL mapped = Map_Linear_Expr(IPA_LNO_File, lx, tm, wn_lda);
        FmtAssert(mapped, ("Map_Term: Non-scalar or non-linear expression"));
      } 
    } else { 
      INT new_ivar_idx = IPA_LNO_File->Add_Translated_Ivar_Unique(*ivar);
      tm->Set_desc(new_ivar_idx);
    }  
    break;
  }
  default: 
    FmtAssert(FALSE, ("Map_Term: Unknown type"));
    break; 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Map_Linex
// FUNCTION: Map the callee LINEX 'lx' to the caller through the callsite
//   'wn_call'. 
//-----------------------------------------------------------------------

static void Map_Linex(IPA_LNO_READ_FILE* IPA_LNO_File,
		      LINEX* lx, 
		      WN* wn_call)
{
  for (INT i = 0; i <= lx->Num_terms(); i++)
    Map_Term(IPA_LNO_File, lx, lx->Get_term(i), wn_call);
} 

//-----------------------------------------------------------------------
// NAME: Map_Projected_Region
// FUNCTION: Map the callee PROJECTED_REGION 'pr_memory' to the caller  
//   through the callsite 'wn_call'. 
//-----------------------------------------------------------------------

extern PROJECTED_REGION* Map_Projected_Region(IPA_LNO_READ_FILE* IPA_LNO_File,
				              PROJECTED_REGION* pr_memory, 
				              WN* wn_call)
{
  if (!Can_Map_Projected_Region(IPA_LNO_File, pr_memory, wn_call)) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
      fprintf(stdout, 
	"SHAPE: Projected Region Problem calling 0x%p\n", wn_call);
    } 
    return NULL; 
  } 
  LINEX* lx_simple = NULL; 
  for (INT i = 0; i < pr_memory->Get_num_dims(); i++) { 
    PROJECTED_NODE* pn = pr_memory->Get_projected_node(i);
    if (!pn->Is_messy_ub()) {
      LINEX* ub = pn->Get_upper_linex();
      if (ub != NULL)  
        Map_Linex(IPA_LNO_File, ub, wn_call);
    }
    if (!pn->Is_messy_lb()) {
      LINEX* lb = pn->Get_lower_linex();
      if (lb != NULL)  
        Map_Linex(IPA_LNO_File, lb, wn_call);
    }
    if (!pn->Is_messy_step()) {
      LINEX* step = pn->Get_step_linex();
      if (step != NULL)  
        Map_Linex(IPA_LNO_File, step, wn_call);
    }
    if (pn->Get_segment_length_linex() != NULL) {
      LINEX* lx_segment_length = pn->Get_segment_length_linex();
      Map_Linex(IPA_LNO_File, lx_segment_length, wn_call);
    }
    if (pn->Get_segment_stride_linex() != NULL) {
      LINEX* lx_segment_stride = pn->Get_segment_stride_linex();
      Map_Linex(IPA_LNO_File, lx_segment_stride, wn_call);
    }
  }
  return pr_memory; 
} 

//-----------------------------------------------------------------------
// NAME: Projected_Region_To_Memory
// FUNCTION: Convert the projected region 'pr' to an in-memory version 
//   and return it.  Use memory from 'mem_pool'. 
//-----------------------------------------------------------------------

static PROJECTED_REGION* Projected_Region_To_Memory(IPA_LNO_READ_FILE* 
						      IPA_LNO_File, 
						    PROJECTED_REGION* pr,
						    MEM_POOL* mem_pool)
{
  PROJECTED_REGION* pr_memory = (PROJECTED_REGION*)
    MEM_POOL_Alloc(mem_pool, sizeof(PROJECTED_REGION));
  pr_memory->Set_num_dims(pr->Get_num_dims());
  pr_memory->Set_type(pr->Get_type());
  pr_memory->Set_projected_kernel(NULL);
  pr_memory->Set_depth(pr->Get_depth());
  if (pr->Is_messy_region()) { 
    pr_memory->Set_projected_array(NULL);
    return pr_memory;
  }  
  PROJECTED_ARRAY* pa = CXX_NEW(PROJECTED_ARRAY(mem_pool), mem_pool);
  pr_memory->Set_projected_array(pa);
  INT pr_base_index = pr->Get_id();
  FmtAssert(pr_base_index >= 0, 
    ("Projected_Region_To_Memory: Attempt to convert NULL projected region"));
  INT pr_count = pr->Get_num_dims();
  PROJECTED_NODE* pn = IPA_LNO_File->Projected_Node(pr->Get_id());
  PROJECTED_NODE* pn_memory = (PROJECTED_NODE*)
    MEM_POOL_Alloc(mem_pool, pr_count * sizeof(PROJECTED_NODE));
  bcopy(pn, pn_memory, pr_count * sizeof(PROJECTED_NODE));

  INT i;
  for (i = 0; i < pr_count; i++)
    pn_memory[i].Set_Mem_Pool(mem_pool);
  TERM* term_array = IPA_LNO_File->Term(0);
  for (i = pr_base_index; i < pr_base_index + pr_count; i++) {
    PROJECTED_NODE* pn = IPA_LNO_File->Projected_Node(i);
    pn_memory[i - pr_base_index].Create_linex(term_array);
    pr_memory->Set_projected_node(&pn_memory[i - pr_base_index]);
  }
  return pr_memory;
} 

//-----------------------------------------------------------------------
// NAME: Map_Formal
// FUNCTION: Map the declaration associated with the formal at position 
//   'idx_formal' from the callee to the caller at callsite 'wn_call'. 
//-----------------------------------------------------------------------

extern PROJECTED_REGION* Map_Formal(IPA_LNO_READ_FILE* IPA_LNO_File,
                                    WN* wn_call,
			            INT idx_formal)
{
  IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
  INT pr_idx = sf->Decl_Array_Section_Index();
  PROJECTED_REGION* pr_original = IPA_LNO_File->Projected_Region(pr_idx);
  PROJECTED_REGION* pr_memory = Projected_Region_To_Memory(IPA_LNO_File,
    pr_original, &ARA_memory_pool);
  PROJECTED_REGION* pr_new = Map_Projected_Region(IPA_LNO_File, pr_memory, 
    wn_call);
  pr_new->Simplify();
  pr_new->LNO_Simplify(IPA_LNO_File, wn_call);
  return pr_new;  
}

//-----------------------------------------------------------------------
// NAME: Projected_Region
// FUNCTION: Return the declaration of the formal at position 'idx_formal' 
//   at the call 'wn_call'.
//-----------------------------------------------------------------------

extern PROJECTED_REGION* Projected_Region(IPA_LNO_READ_FILE* IPA_LNO_File,
                                          INT idx_pr,
					  WN* wn_call)
{
  PROJECTED_REGION* pr_original = IPA_LNO_File->Projected_Region(idx_pr);
  PROJECTED_REGION* pr_memory = Projected_Region_To_Memory(IPA_LNO_File,
    pr_original, &ARA_memory_pool);
  pr_memory->Simplify();
  pr_memory->LNO_Simplify(IPA_LNO_File, wn_call);
  return pr_memory;  
}

//-----------------------------------------------------------------------
// NAME: Bound_Difference_To_Linex
// FUNCTION: Return a LINEX representing the lower bound of the 'posi-
//   tion'th dimension of 'ty_idx_formal', or NULL, if this is not possible. 
//-----------------------------------------------------------------------

static LINEX* Bound_Difference_To_Linex(IPA_LNO_READ_FILE* IPA_LNO_File, 
				        TY_IDX ty_idx_formal, 
			                INT position)
{ 
  LINEX* lx = CXX_NEW(LINEX(&ARA_memory_pool), &ARA_memory_pool);
  if (TY_AR_const_lbnd(ty_idx_formal, position)) {
    mINT64 lb_const = TY_AR_lbnd_val(ty_idx_formal, position);
    lx->Set_term(LTKIND_CONST, -lb_const, CONST_DESC, 0);
  } else { 
    ST_IDX st_idx = TY_AR_lbnd_var(ty_idx_formal, position);
    if (st_idx == ST_IDX_ZERO) { 
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, "BND LINEX: No lower bound\n"); 
      return NULL; 
    } 
    WN* wn_true = True_Bound(Current_Func_Node, st_idx);
    if (wn_true == NULL) { 
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, "BND LINEX: No True_Bound for ST_IDX %d\n",
	  st_idx); 
      return NULL; 
    } 
    BOOL ok = Exp_To_Linex(wn_true, lx, &ARA_memory_pool, TRUE, 
      TRUE, IPA_LNO_File);
    if (!ok) { 
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, "BND LINEX: Could not convert exp to linex\n"); 
      return NULL; 
    } 
  } 
  if (TY_AR_const_ubnd(ty_idx_formal, position)) {
    mINT64 ub_const = TY_AR_ubnd_val(ty_idx_formal, position);
    lx->Set_term(LTKIND_CONST, ub_const, CONST_DESC, 0);
  } else { 
    ST_IDX st_idx = TY_AR_ubnd_var(ty_idx_formal, position);
    if (st_idx == ST_IDX_ZERO) { 
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, "BND LINEX: No upper bound\n"); 
      return NULL; 
    } 
    WN* wn_true = True_Bound(Current_Func_Node, st_idx);
    if (wn_true == NULL) { 
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, "BND LINEX: No True_Bound for ST_IDX %d\n",
	  st_idx); 
      return NULL; 
    } 
    BOOL ok = Exp_To_Linex(wn_true, lx, &ARA_memory_pool, FALSE,
      TRUE, IPA_LNO_File);
    if (!ok) {
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, "BND LINEX: Could not convert exp to linex\n"); 
      return NULL; 
    } 
  } 
  lx->Simplify();
  return lx; 
} 

//-----------------------------------------------------------------------
// NAME: Are_Equal_Dims
// FUNCTION: Return TRUE if the 'position'th dimension of 'pr' and the 
//   'position'th dimension of 'ty_idx_formal' are equal.  Return FALSE
//   if they are not or if we are not sure. 
//-----------------------------------------------------------------------

static BOOL Are_Equal_Dims(IPA_LNO_READ_FILE* IPA_LNO_File,
			   WN* wn_call, 
			   PROJECTED_REGION* pr, 
			   TY_IDX ty_idx_formal,
			   INT position)
{
  PROJECTED_NODE* pn = pr->Get_projected_node(position);
  // Don't bother with these cases now. 
  if (pn->Is_messy_lb() || pn->Is_messy_ub() || pn->Is_messy_step())
    return FALSE;  
  LINEX* lx_ub_st = Bound_Difference_To_Linex(IPA_LNO_File, ty_idx_formal,
    position);
  if (lx_ub_st == NULL)
    return FALSE; 
  LINEX* lx_ub_pr = pn->Get_upper_linex();
  lx_ub_pr->Simplify();
  lx_ub_st->Simplify();
  lx_ub_pr->LNO_Simplify(IPA_LNO_File, wn_call);
  lx_ub_st->LNO_Simplify(IPA_LNO_File, wn_call);
  if (!lx_ub_pr->Equivalent(*lx_ub_st))
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Shape_Mismatch_At_Formal
// FUNCTION: Return TRUE if the 'position'th argument of the call 
//   'wn_call' cannot be reshaped to represent its sections at  
//   that call.  The information about the formal corresponding to this 
//   actual argument is stored in the IPA_LNO_SUMMARY_FORMAL number 
//   'idx_formal' in the 'IPA_LNO_File'.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Shape_Mismatch_At_Formal(IPA_LNO_READ_FILE* IPA_LNO_File,
                                     WN* wn_call,
                                     INT position,
                                     INT idx_formal)
{
  if (position < 0 || position >= WN_kid_count(wn_call)) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, 
	"SHAPE: Formal/Actual counts do not match for call 0x%p\n", wn_call);
    return TRUE;
  }
  WN* wn_formal = WN_kid(wn_call, position);
  char* callee_name = ST_name(WN_st(wn_call));
  WN* wn_base = NULL; 
  WN* wn_argument = wn_formal;
  OPCODE op_argument = WN_opcode(wn_formal);
  if (OPCODE_operator(op_argument) == OPR_PARM) {
    wn_argument = WN_kid0(wn_formal);
    op_argument = WN_opcode(WN_kid0(wn_formal));
  }
  ST* st_argument = NULL;
  BOOL scalar_temp = FALSE; 
  IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
  switch (OPCODE_operator(op_argument)) {
  case OPR_INTCONST:
    // Passed a constant, can't be modified.
    return FALSE;
  case OPR_ARRAY: {
      wn_base = WN_array_base(wn_argument);
      OPERATOR opr_base = WN_operator(wn_base);
      if (opr_base != OPR_LDID && opr_base != OPR_LDA) {
	if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
	  fprintf(stdout, 
	    "SHAPE: %s ARG %d: Passing complex ARRAY\n", 
	    callee_name, position);
	}
	return TRUE; 
      } 
      st_argument = WN_st(wn_base);
      ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_argument);
      if (aa == NULL || Bound_Is_Too_Messy(aa)) { 
        if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
	  fprintf(stdout, "SHAPE: %s ARG %d: NULL or Messy Access Array on Array Section\n", 
	    callee_name, position);
	return TRUE; 
      } 
    } 
    break;
  case OPR_LDID:
  case OPR_LDA: {
      if (!sf->Is_May_Kill() && !sf->Is_Use()) 
        return FALSE; 
      st_argument = WN_st(wn_argument);
    }
    break;
  default: { 
      DYN_ARRAY<WN*> wn_list(&LNO_local_pool);
      DYN_ARRAY<INT> int_list(&LNO_local_pool);
      INT64 const_value = 0;
      if (!(Scalar_Expr(wn_argument) && Linear_Expr(wn_argument, &wn_list, 
	  &int_list, &const_value))) {  
	if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
	  fprintf(stdout, "SHAPE: %s ARG %d: Odd pass type\n",
	    callee_name, position);
	return TRUE;
      } else { 
	scalar_temp = TRUE; 
      } 
    } 
  }
  if (sf->Decl_Array_Section_Index() == -1) {
    if (scalar_temp || Is_Scalar(st_argument)) 
      return FALSE;
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, "SHAPE: %s ARG %d: Array Passed to Scalar\n",
        callee_name, position);
    return TRUE;
  }
  if (sf->Is_Scalar()) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, "SHAPE: %s ARG %d: Scalar Passed to Array\n",
        callee_name, position);
    return TRUE;
  }
  TY_IDX ty_idx_formal = ST_type(st_argument); 
  if (TY_kind(ty_idx_formal) == KIND_POINTER)
    ty_idx_formal = TY_pointed(ty_idx_formal);
  if (TY_kind(ty_idx_formal) != KIND_ARRAY) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, "SHAPE: %s ARG %d: Unusual argument type\n", 
        callee_name, position);
    return TRUE; 
  } 
  if (sf->Machine_Type() != Machine_Type(wn_base ? wn_base : wn_argument)) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, "SHAPE: %s ARG %d: Mismatched machine types\n",
        callee_name, position);
    return TRUE; 
  } 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Array_Shapes_Match_At_Formal
// FUNCTION: Return TRUE if the shape of the array at the caller matches
//   the shape of the array at the callee.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Array_Shapes_Match_At_Formal(IPA_LNO_READ_FILE* IPA_LNO_File,
					 WN* wn_call,
                                         INT position,
					 PROJECTED_REGION* pr_formal)
{ 
  WN* wn_formal = WN_kid(wn_call, position);
  WN* wn_argument = wn_formal;
  char* callee_name = ST_name(WN_st(wn_call));
  OPCODE op_argument = WN_opcode(wn_formal);
  if (OPCODE_operator(op_argument) == OPR_PARM) {
    wn_argument = WN_kid0(wn_formal);
    op_argument = WN_opcode(WN_kid0(wn_formal));
  }
  ST* st_argument = NULL;
  switch (OPCODE_operator(op_argument)) {
  case OPR_ARRAY: 
    st_argument = WN_st(WN_array_base(wn_argument));
    break;
  case OPR_LDID:
  case OPR_LDA: 
    st_argument = WN_st(wn_argument);
    break;
  default: 
    FmtAssert(FALSE, 
     ("Array_Shapes_Match_At_Formal: Should have screened these out"));
  }
  TY_IDX ty_idx_formal = ST_type(st_argument); 
  if (TY_kind(ty_idx_formal) == KIND_POINTER)
    ty_idx_formal = TY_pointed(ty_idx_formal);
  FmtAssert(pr_formal != NULL, 
    ("Array_Shapes_Match_At_Formal: Expecting non-NULL projected region"));
  if (TY_AR_ndims(ty_idx_formal) != pr_formal->Get_num_dims()) { 
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, 
	"TRY RESHAPE: %s ARG %d: Mismatched Array Dims\n",
        callee_name, position);
    for (INT i = 0; i < pr_formal->Get_num_dims(); i++) {
      PROJECTED_NODE* pn = pr_formal->Get_projected_node(i);
      LINEX* lx_lb_pr = pn->Get_lower_linex();
      lx_lb_pr->Simplify();
      lx_lb_pr->LNO_Simplify(IPA_LNO_File, wn_call);
      LINEX* lx_ub_pr = pn->Get_upper_linex();
      lx_ub_pr->Simplify();
      lx_ub_pr->LNO_Simplify(IPA_LNO_File, wn_call);
    } 
    return FALSE; 
  } 
  for (INT i = 1; i < pr_formal->Get_num_dims(); i++) { 
    if (!Are_Equal_Dims(IPA_LNO_File, wn_call, pr_formal, ty_idx_formal, i)) {
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
        fprintf(stdout, "TRY RESHAPE: %s ARG %d: Mismatched Array Subs\n",
	  callee_name, position);
      return FALSE; 
    } 
  } 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Shape_Mismatch_At_Common
// FUNCTION: Return TRUE if the common represented by the IPA_LNO_SUMMARY_
//   COMMON at index 'idx_common' in 'IPA_LNO_File' cannot be reshaped 
//   when translated through the call 'wn_call'.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Shape_Mismatch_At_Common(IPA_LNO_READ_FILE* IPA_LNO_File,
                                     INT idx_global)
{
  IPA_LNO_SUMMARY_GLOBAL* sc = IPA_LNO_File->Global(idx_global); 
  if (sc->Is_Scalar())
    return FALSE; 
  return ST_is_equivalenced(ST_ptr(sc->St_Idx()));
}

