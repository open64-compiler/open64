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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "stab.h"
#include "targ_const.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "strtab.h"
#include "config.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "wintrinsic.h"
#include "lno_bv.h"
#include "dep_graph.h"
#include "debug.h"
#include "cxx_memory.h"
#include "snl.h"
#include "snl_trans.h" 
#include "snl_utils.h"
#include "fusion.h"
#include "lego_pragma.h"
#include "lego_util.h"
#include "lego_opts.h"
#include "tile.h"
#include "move.h"
#include "tlog.h"

#include "ir_reader.h"

#define MAX_NAME_LENGTH 132 
#define TLOG_STRING_LENGTH 1000

static ARRAY_DIRECTED_GRAPH16* dg;
static DU_MANAGER* du;

enum KERNEL_SECTION {KERNEL_NONE, KERNEL_OUTER, KERNEL_MIDDLE, KERNEL_INNER}; 

//-----------------------------------------------------------------------
// NAME: SNL_Kernel_Section
// FUNCTION: Given a loop 'wn' within a 'stack' of loops containing a 
//   'permutation' of length 'nloops', return a characterization of the 
//    loop: 
// 	KERNEL_NONE: Loop is not one of the loops in the permutation 
//      KERNEL_OUTER: Loop is in the permutation set, but is not itself 
//        permuted, and lies before permuted loops in the permutation set.
//      KERNEL_MIDDLE: Loop is in the permutation set, and in the middle
//        section where the loops are permuted. 
//      KERNEL_INNER: Loop is in the permutation set, but is not itself
//        permuted, and lies inside all of the permuted loops inside 
//        the permutation set. 
//  Example: If stack consists of loops (L0 L1 L2 L3 L4 L5) and 
//    permutation is (0 2 1 3) with nloops == 4, then: 
//      L0 is KERNEL_NONE 
//      L1 is KERNEL_OUTER 
//      L2 and L3 are KERNEL_MIDDLE 
//      L4 is KERNEL_INNER  
//-----------------------------------------------------------------------

static KERNEL_SECTION SNL_Kernel_Section(WN* wn, 
					 DOLOOP_STACK* stack, 
				 	 INT permutation[], 
					 INT nloops)
{
  INT i;
  for (i = 0; i < stack->Elements(); i++) 
    if (stack->Bottom_nth(i) == wn) 
      break;
  if (i == stack->Elements())
    return KERNEL_NONE; 
  INT first_in_stack = stack->Elements() - nloops; 
  INT loop_index = i - first_in_stack;
  for (i = 0; i < loop_index && permutation[i] == i; i++); 
  if (i == loop_index)
    return KERNEL_OUTER; 
  for (i = nloops - 1; i > loop_index && permutation[i] == i; i--);
  if (i == loop_index)
    return KERNEL_INNER; 
  return KERNEL_MIDDLE;  
}
	
// Forward declaration. 
static BOOL Is_Non_Dependent_Expression(WN* wn, WN* outerloop); 

//-----------------------------------------------------------------------
// NAME: Is_Non_Dependent_Load 
// FUNCTION: Returns TRUE if load 'wn' is not directly a function of 
//   the index variable of 'outerloop', FALSE if it is unknown or it is 
//   not directly a function of the index variable of 'outerloop'.  
//-----------------------------------------------------------------------

static BOOL Is_Non_Dependent_Load(WN* wn,
                                  WN* outerloop)
{
  switch (WN_operator(wn)) {
  case OPR_LDID:
  case OPR_ILOAD:
  case OPR_IO:
  case OPR_RETURN:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    break;
  default:
    FmtAssert(0, ("Is_Non_Dependent_Load called with improper node type."));
  }

  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
  if (def_list == NULL) 
    return TRUE; 
  if (def_list != NULL && def_list->Incomplete())
    return FALSE;
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  if (WN_operator(wn) == OPR_LDID) {
    if (SYMBOL(wn) == SYMBOL(WN_index(outerloop)))
      return FALSE; 
    for (WN* wn_tp = wn; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp))
      if (WN_opcode(wn_tp) == OPC_DO_LOOP
	  && SYMBOL(wn) == SYMBOL(WN_index(wn_tp)))
	return TRUE; 
    if (def_list->Loop_stmt() == 0x0) {
      for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
	WN* def = node->Wn(); 
	// Avoid infinite loop on reduction expressions.
	if (Find_Containing_Store(wn) == def)
	  return FALSE;
	if (!Is_Non_Dependent_Expression(def, outerloop))
	  return FALSE; 
      }
    }
  } else {
    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
      WN* def = node->Wn();
      if (Wn_Is_Inside(def, outerloop))
	return FALSE;
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Is_Non_Dependent_Expression
// FUNCTION: Returns TRUE if no load in the expression 'wn' is a function
//   of the index variable of 'outerloop', FALSE if some load is possibly
//   a function of the index variable of 'outerloop'.  
//-----------------------------------------------------------------------

static BOOL Is_Non_Dependent_Expression(WN* wn,
                                        WN* outerloop)
{
  switch (WN_operator(wn)) {
  case OPR_LDID:
  case OPR_ILOAD:
  case OPR_IO:
  case OPR_RETURN:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    if (!Is_Non_Dependent_Load(wn, outerloop))
      return FALSE;
  }

  for (INT kid = 0; kid < WN_kid_count(wn); kid++)
    if (!Is_Non_Dependent_Expression(WN_kid(wn, kid), outerloop))
      return FALSE;
  return TRUE;
}
//-----------------------------------------------------------------------
// NAME: SNL_Legal_Perm_Bounds
// FUNCTION: Returns TRUE if the 'stack' of loops can be permuted according 
//   to the 'permutation' of length 'nloops' without rewriting the bounds
//   of any of the loops.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL SNL_Legal_Perm_Bounds(DOLOOP_STACK* stack,  
                                  INT permutation[],
                                  INT nloops)
{
  INT first_in_stack = stack->Elements() - nloops;  
  for (INT i = 0; i < nloops; i++) {
    WN* wn_iloop = stack->Bottom_nth(first_in_stack + permutation[i]);  
    for (INT j = i; j < nloops; j++) {
      if (permutation[j] < permutation[i]) { 
        WN* wn_jloop = stack->Bottom_nth(first_in_stack + permutation[j]);  
        if (!Is_Non_Dependent_Expression(WN_start(wn_iloop), wn_jloop))
	  return FALSE;
        if (UBvar(WN_end(wn_iloop)) == NULL)
	  return FALSE;  
        if (!Is_Non_Dependent_Expression(UBexp(WN_end(wn_iloop)), wn_jloop))
	  return FALSE; 
      }
    }
  } 
  return TRUE;
} 

//-----------------------------------------------------------------------
// NAME: SNL_Legal_Perm_Scalar 
// FUNCTION: Returns TRUE if the scalar dependences on the node 'wn' are
//   still legal after permutaing the 'stack' of loops according to the 
//   'permutation' of length 'nloops'.  Returns FALSE otherwise.  
// 'has_removable_branch' indicates whether to ignore if-conditions in 
// the loop body.
//-----------------------------------------------------------------------

static BOOL SNL_Legal_Perm_Scalar(DOLOOP_STACK* stack, 
				  WN* wn,
                                  INT permutation[],
                                  INT nloops,
				  BOOL has_removable_branch) 
{
  if (WN_operator(wn) == OPR_STID) {
    if (dg->Get_Vertex(wn))
      return FALSE;
    WN* wn_loop = stack->Bottom_nth(stack->Elements() - 1); 
    WN* wn_tp = 0;
    for (wn_tp = wn_loop; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp))
      if (WN_opcode(wn_tp) == OPC_DO_LOOP
          && SYMBOL(wn) == SYMBOL(WN_index(wn_tp)))
        return TRUE;
    for (wn_tp = wn; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp))
      if (WN_opcode(wn_tp) == OPC_IF) 
	break;
    if (wn_tp != NULL && (!has_removable_branch || (WN_operator(wn_tp) != OPR_IF)))
      return FALSE; 
    return TRUE; 
  } else if (WN_operator(wn) == OPR_LDID) {
    if (dg->Get_Vertex(wn))
      return FALSE;
    if (red_manager != NULL && red_manager->Which_Reduction(wn) != RED_NONE)
      return TRUE; 
    WN* wn_loop = stack->Bottom_nth(stack->Elements() - 1); 
    for (WN* wn_tp = wn_loop; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp))
      if (WN_opcode(wn_tp) == OPC_DO_LOOP
          && SYMBOL(wn) == SYMBOL(WN_index(wn_tp)))
        return TRUE;
    DEF_LIST *def_list = du->Ud_Get_Def(wn);
    if (def_list == NULL)
      return TRUE;
    if (def_list->Incomplete())
      return FALSE;
    if (def_list->Loop_stmt() == NULL)
      return TRUE; 
    INT kernel_section = SNL_Kernel_Section(def_list->Loop_stmt(), stack,
      permutation, nloops); 
    if (kernel_section == KERNEL_MIDDLE || kernel_section == KERNEL_INNER)
      return FALSE;  
  }
  return TRUE;    
}

//-----------------------------------------------------------------------
// NAME: SNL_Legal_Perm_Scalars 
// FUNCTION: Returns TRUE if the scalar dependences on the tree rooted at 
//   'wn' are still legal after permuting the 'stack' of loops according 
//   to the 'permutation' of length 'nloops'.  Returns FALSE otherwise.  
// 'has_removable_branch' indicates whether to ignore if-conditions in 
// the loop body.
//-----------------------------------------------------------------------

static BOOL SNL_Legal_Perm_Scalars(DOLOOP_STACK* stack, 
				  WN* wn,
                                  INT permutation[],
                                  INT nloops,
				  BOOL has_removable_branch)
{
  if (!SNL_Legal_Perm_Scalar(stack, wn, permutation, nloops, has_removable_branch)) 
    return FALSE;
  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN* wn_tp = WN_first(wn); wn_tp != NULL; wn_tp = WN_next(wn_tp))
      if (!SNL_Legal_Perm_Scalars(stack, wn_tp, permutation, nloops, has_removable_branch))
        return FALSE;
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) 
      if (!SNL_Legal_Perm_Scalars(stack, WN_kid(wn, i), permutation, nloops, has_removable_branch))
        return FALSE;
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: SNL_Depv_Is_Permutable 
// FUNCTION: Return TRUE if the 'array_index'th dependence vector in the
//   'depv_array' is still lexicographically positive after the 'stack'
//   of loops has the 'permutation' of length 'nloops' applied to it. 
//   Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL SNL_Depv_Is_Permutable(DEPV_ARRAY* depv_array,
                                   INT array_index,
				   DOLOOP_STACK* stack,
				   INT permutation[],
                                   INT nloops)  
{
  DEPV* depv = depv_array->Depv(array_index);
  INT depv_lb = depv_array->Num_Unused_Dim(); 
  INT depv_ub = depv_array->Num_Unused_Dim() + depv_array->Num_Dim() - 1; 
  INT loop_lb = Do_Loop_Depth(stack->Bottom_nth(0)); 
  INT loop_ub = loop_lb + nloops - 1; 
  INT i;
  for (i = loop_lb; i < depv_lb; i++) 
    if (permutation[i - loop_lb] != i - loop_lb)
      return FALSE; 
  for (i = depv_ub + 1; i <= loop_ub; i++) 
    if (permutation[i - loop_lb] != i - loop_lb) 
      return FALSE; 
  for (i = depv_lb; i <= depv_ub; i++) {
    INT dep_index = i - depv_lb;  
    if (i >= loop_lb && i <= loop_ub) 
      dep_index = permutation[i - loop_lb] + loop_lb - depv_lb;  
    DIRECTION dir = DEP_Direction(DEPV_Dep(depv, dep_index)); 
    if (dir == DIR_POS)  
      return TRUE; 
    if (dir == DIR_NEG || dir == DIR_POSNEG || dir == DIR_NEGEQ 
      || dir == DIR_STAR)
      return FALSE; 
   }
   return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Legal_Perm_Array 
// FUNCTION: For the node 'wn' return TRUE if its array dependences allow 
//   the 'stack' of loops to be permuted according to the 'permutation' of 
//   length 'nloops'.  Return FALSE otherwise.  
// NOTE: The 'edge_table' is an intermediate structure used to keep us from 
//   double checking of edges between nodes.  
//-----------------------------------------------------------------------

static BOOL SNL_Legal_Perm_Array(DOLOOP_STACK* stack,
				 WN* wn,
                                 INT permutation[],
                                 INT nloops, 
                                 HASH_TABLE<EINDEX16,INT>* edge_table,
				 BOOL extended_test)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  OPERATOR opr = WN_operator(wn);
  if (opr != OPR_ILOAD && opr != OPR_ISTORE && opr != OPR_LDID 
      && opr != OPR_STID)
    return TRUE; 
  VINDEX16 v = dg->Get_Vertex(wn);
  if (v == 0) { 
    if (opr == OPR_LDID || opr == OPR_STID)
      return TRUE; 
    else
      return FALSE;
  }
  EINDEX16 e = 0; 
  if (extended_test) { 
    WN* wn_enclosing = Enclosing_Loop(wn); 
    KERNEL_SECTION ks_enclosing = SNL_Kernel_Section(wn_enclosing, 
      stack, permutation, nloops);
    WN* wn_enclosing_body = Enclosing_Loop_Body(wn);
    if (wn_enclosing_body == NULL) 
      return (ks_enclosing == KERNEL_NONE || ks_enclosing == KERNEL_OUTER);
    KERNEL_SECTION ks_enclosing_body = SNL_Kernel_Section(wn_enclosing_body, 
      stack, permutation, nloops);
    if (ks_enclosing == KERNEL_NONE || ks_enclosing == KERNEL_OUTER) {
      for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
	WN* wn_loop = Enclosing_Loop(dg->Get_Wn(dg->Get_Source(e))); 
	KERNEL_SECTION ks_enclosing = SNL_Kernel_Section(wn_loop, stack, 
	  permutation, nloops);
	if (!(ks_enclosing == KERNEL_NONE || ks_enclosing == KERNEL_OUTER))
	  return FALSE;
      }
      for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
	WN* wn_loop = Enclosing_Loop(dg->Get_Wn(dg->Get_Sink(e))); 
	KERNEL_SECTION ks_enclosing = SNL_Kernel_Section(wn_loop, stack, 
	  permutation, nloops);
	if (!(ks_enclosing == KERNEL_NONE || ks_enclosing == KERNEL_OUTER))
	  return FALSE;
      }
      return TRUE;
    }
    if (ks_enclosing_body != KERNEL_INNER)
      return FALSE; 
  } 
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    if (edge_table->Find(e))
      continue;
    edge_table->Enter(e, 1);
    DEPV_ARRAY* depv_array = dg->Depv_Array(e);
    for (INT i = 0; i < depv_array->Num_Vec(); i++)
      if (!SNL_Depv_Is_Permutable(depv_array, i, stack, permutation, nloops))
	return FALSE;
  }
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    if (edge_table->Find(e))
      continue;
    edge_table->Enter(e, 1);
    DEPV_ARRAY* depv_array = dg->Depv_Array(e);
    for (INT i = 0; i < depv_array->Num_Vec(); i++)
      if (!SNL_Depv_Is_Permutable(depv_array, i, stack, permutation, nloops))
	return FALSE;
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: SNL_Legal_Perm_Arrays 
// FUNCTION: For the tree rooted at 'wn' return TRUE if all of the array 
//   dependences allow the 'stack' of loops to be permuted according to 
//   the 'permutation' of length 'nloops'.  Return FALSE otherwise.  
// NOTE: The 'edge_table' is an intermediate structure used to keep us from 
//   double checking of edges between nodes.  
//-----------------------------------------------------------------------

extern BOOL SNL_Legal_Perm_Arrays(DOLOOP_STACK* stack, 
				  WN* wn,
                                  INT permutation[],
                                  INT nloops, 
				  HASH_TABLE<EINDEX16,INT>* edge_table,
				  BOOL extended_test)
{
  if (!SNL_Legal_Perm_Array(stack, wn, permutation, nloops, edge_table, 
      extended_test))
    return FALSE;

  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN* wn_tp = WN_first(wn); wn_tp != NULL; wn_tp = WN_next(wn_tp))
      if (!SNL_Legal_Perm_Arrays(stack, wn_tp, permutation, nloops, edge_table, 	  extended_test))
        return FALSE;
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      if (!SNL_Legal_Perm_Arrays(stack, WN_kid(wn, i), permutation, nloops, 
	  edge_table, extended_test))
        return FALSE;
    }
  }
  return TRUE;
} 

//-----------------------------------------------------------------------
// NAME: SNL_Good_Perm_Loops 
// FUNCTION: Returns TRUE if no loop in the 'permutation' of 'nloops' 
//   (except unpermuted loops at the ends of those sections) is a bad loop
//   or crosses an MP region.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL SNL_Good_Perm_Loops(DOLOOP_STACK* stack,
			        INT permutation[],
                                INT nloops)
{
  INT first_in_stack = stack->Elements() - nloops; 
  INT i;
  for (i = 0; i < nloops; i++) 
    if (permutation[i] != i)
      break;
  INT low_index = i;
  if (low_index == nloops) 
    return TRUE;
  for (i = nloops - 1; i >= 0; i--)
    if (permutation[i] != i) 
      break;
  INT high_index = i;
  for (i = low_index; i <= high_index; i++) {
    WN* wn_loop = stack->Bottom_nth(first_in_stack + i);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    if (dli->Has_Bad_Mem) 
      return FALSE; 
  }
  WN* wn_start = stack->Bottom_nth(first_in_stack + high_index); 
  WN* wn_stop = stack->Bottom_nth(first_in_stack + low_index); 
  for (WN* wn = wn_start; ; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_REGION && Is_Mp_Region(wn))
      return FALSE; 
    if (wn == wn_stop)
      break;
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Parallel_Serial_Order_OK
// FUNCTION: Returns TRUE if there is no serial loop outside a parallel
//  loop as a result of doing this interchange.  Returns FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL SNL_Parallel_Serial_Order_OK(DOLOOP_STACK* stack,
                                         INT permutation[],
                                         INT nloops)
{
  BOOL found_serial = FALSE;
  INT first_in_stack = stack->Elements() - nloops; 
  for (INT i = 0; i < nloops; i++) {
    WN* wn_loop = stack->Bottom_nth(first_in_stack + permutation[i]);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
    if (dli->Mp_Info != NULL) {
      if (found_serial)
        return FALSE;
    } else {
      found_serial = TRUE;
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: SNL_Legal_Permutation 
// FUNCTION: Returns TRUE if the SNL defined between loops 'outer_loop' 
//   and 'inner_loop' which consists of 'nloops' loops can be legally 
//   permuted with specified 'permutation'.  Returns FALSE otherwise. 
// NOTE: This test is more conservative than the one used in Phase 2
//   LNO analysis.  We may wish to extend it later.  
//-----------------------------------------------------------------------

extern BOOL SNL_Legal_Permutation(WN* outer_loop, 
				  WN* inner_loop, 
				  INT permutation[], 
				  INT nloops,
				  BOOL has_removable_branch)
{
  dg = Array_Dependence_Graph;
  du = Du_Mgr;
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(inner_loop, &stack);
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(inner_loop);
  INT first_in_stack = dli_inner->Depth - nloops + 1;
  INT i;
  for (i = 0; i < nloops; i++)
    if (permutation[i] != i)
      break;
  if (i == nloops) 
    return TRUE; 
  WN* wn_outer_loop = stack.Bottom_nth(first_in_stack + i);
  for (i = nloops - 1; i >= 0; i--)
    if (permutation[i] != i) 
      break; 
  WN* wn_inner_loop = stack.Bottom_nth(first_in_stack + i);  
  if (!SNL_Parallel_Serial_Order_OK(&stack, permutation, nloops))
    return FALSE;
  if (!SNL_Good_Perm_Loops(&stack, permutation, nloops))
    return FALSE;  
  if (!SNL_Legal_Perm_Bounds(&stack, permutation, nloops))
    return FALSE; 
  if (!SNL_Legal_Perm_Scalars(&stack, WN_do_body(inner_loop), permutation, 
      nloops, has_removable_branch))
    return FALSE; 
  if (!Sandwiched_Code_Sinkable_In(wn_outer_loop, wn_inner_loop, du))
    return FALSE; 
  INT hash_table_size = MIN(dg->Get_Edge_Count(), 512);
  HASH_TABLE<EINDEX16,INT> edge_table(hash_table_size, &LNO_local_pool);
  if (!SNL_Legal_Perm_Arrays(&stack, outer_loop, permutation, nloops, 
      &edge_table, TRUE))
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Tlog_Lego_Interchange 
// FUNCTION: Print information to Tlog which describes the lego interchange
//   which has been performed. 
//-----------------------------------------------------------------------

static void Tlog_Lego_Interchange(WN* outer_loop,
                                  INT permutation[],
                                  INT nloops)
{
   char tlog_instring[TLOG_STRING_LENGTH];
   char tlog_outstring[TLOG_STRING_LENGTH];
   INT char_count = 0;
   char_count += sprintf(tlog_instring + char_count, "(");
   INT i;
   for (i = 0; i < nloops; i++) {
     const char *name = 
	WB_Whirl_Symbol(SNL_Get_Inner_Snl_Loop(outer_loop, i + 1));
     if (strlen(tlog_instring) + strlen(name) + 1 < TLOG_STRING_LENGTH)  
       char_count += sprintf(tlog_instring + char_count, "%s", name);
     if (i < nloops - 1 && strlen(tlog_instring) + 2 < TLOG_STRING_LENGTH)
       char_count += sprintf(tlog_instring + char_count, ",");
   }
   if (strlen(tlog_instring) + 2 < TLOG_STRING_LENGTH)
     char_count += sprintf(tlog_instring + char_count, ")");
   char_count = 0;
   char_count += sprintf(tlog_outstring + char_count, "(");
   for (i = 0; i < nloops; i++) {
     const char *name = WB_Whirl_Symbol(SNL_Get_Inner_Snl_Loop(outer_loop, 
       permutation[i] + 1));
     if (strlen(tlog_outstring) + strlen(name) + 1 < TLOG_STRING_LENGTH)
       char_count += sprintf(tlog_outstring + char_count, "%s", name);
     if (i < nloops - 1 && strlen(tlog_outstring) + 2 < TLOG_STRING_LENGTH)
       char_count += sprintf(tlog_outstring + char_count, ",");
   }
   if (strlen(tlog_outstring) + 2 < TLOG_STRING_LENGTH)
     char_count += sprintf(tlog_outstring + char_count, ")");
     char *tmp_char = (char *) WB_Whirl_Symbol(outer_loop);
   Generate_Tlog("LNO", "lego_interchange", 
     Srcpos_To_Line(WN_linenum(outer_loop)), tmp_char, 
     tlog_instring, tlog_outstring, "");
}

//-----------------------------------------------------------------------
// NAME: SNL_Lift_Lego_Tile_Loops_Once 
// FUNCTION: Given the SNL between 'outer_loop' and 'inner_loop', con-
//   struct a single permutation which attempts to move all of the loops
//   with 'Is_Outer_Lego_Tile' set to the outermost position.  If this 
//   permutation fails, none of the loops are permuted.  Return TRUE 
//   if this was successful, FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL SNL_Lift_Lego_Tile_Loops_Once(WN* outer_loop, 
					  WN* inner_loop)
{
  BOOL successful = FALSE; 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(outer_loop);
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(inner_loop);
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
    &LNO_local_pool);
  Hoist_Statements(outer_loop, du);
  DOLOOP_STACK rebuild_stack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(outer_loop), &rebuild_stack);
  LNO_Build_Access(outer_loop, &rebuild_stack, &LNO_default_pool); 
  Build_Doloop_Stack(inner_loop, stack);
  INT nloops = dli_inner->Depth - dli_outer->Depth + 1;  
  INT first_in_stack = dli_inner->Depth - nloops + 1; 
  INT* permutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool); 
  INT j = 0; 
  INT i;
  for (i = first_in_stack; i < stack->Elements(); i++) { 
    WN* wn_loop = stack->Bottom_nth(i); 
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop); 
    if (dli_loop->Is_Outer_Lego_Tile) 
      permutation[j++] = i - first_in_stack; 
  }
  for (i = first_in_stack; i < stack->Elements(); i++) { 
    WN* wn_loop = stack->Bottom_nth(i);   
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);  
    if (!dli_loop->Is_Outer_Lego_Tile) 
      permutation[j++] = i - first_in_stack; 
  }
  if (SNL_Legal_Permutation(outer_loop, inner_loop, permutation, nloops, FALSE)) {
    if (LNO_Tlog)
      Tlog_Lego_Interchange(outer_loop, permutation, nloops);
    SNL_INV_Permute_Loops(outer_loop, permutation, nloops, TRUE);
    successful = TRUE; 
  } 
  CXX_DELETE_ARRAY(permutation, &LNO_local_pool); 
  return successful; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Lift_Lego_Tile_Loops_Shifts
// FUNCTION:  Given the SNL between 'outer_loop' and 'inner_loop', con-
//   struct a series of shift permutations which attempt to move all 
//   of the loops with 'Is_Outer_Lego_Tile' set to the outermost positions. 
// NOTE: Since this makes a sequence of shifts, it is not an all-or- 
//   nothing transformation like SNL_Lift_Lego_Tile_Loops_Once(). 
//-----------------------------------------------------------------------

static void SNL_Lift_Lego_Tile_Loops_Shifts(WN* outer_loop, 
					    WN* inner_loop) 
{
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(outer_loop);
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(inner_loop);
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
    &LNO_local_pool);
  Hoist_Statements(outer_loop, du);
  DOLOOP_STACK rebuild_stack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(outer_loop), &rebuild_stack);
  LNO_Build_Access(outer_loop, &rebuild_stack, &LNO_default_pool); 
  Build_Doloop_Stack(inner_loop, stack);
  INT nloops = dli_inner->Depth - dli_outer->Depth + 1;
  INT first_in_stack = dli_inner->Depth - nloops + 1;
  INT* permutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);
  for (INT i = 0; i < nloops; i++) { 
    WN* wn_loop = stack->Bottom_nth(first_in_stack + i); 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
    if (dli->Is_Outer_Lego_Tile) 
      continue;
    INT j = i + 1;
    while (TRUE) {  
      for (; j < nloops; j++) { 
        WN* wn_next = stack->Bottom_nth(first_in_stack + j);  
        DO_LOOP_INFO* dli_next = Get_Do_Loop_Info(wn_next); 
	if (dli_next->Is_Outer_Lego_Tile) 
	  break;
      }
      if (j == nloops) 
	break;
      INT k;
      for (k = 0; k < i; k++) 
	permutation[k] = k;
      permutation[i] = j;
      for (k = i + 1; k <= j; k++) 
	permutation[k] = k - 1;  
      for (k = j + 1; k < nloops; k++)
	permutation[k] = k;   
      if (SNL_Legal_Permutation(outer_loop, inner_loop, permutation, nloops, FALSE)) {
	if (LNO_Tlog)
	  Tlog_Lego_Interchange(outer_loop, permutation, nloops);
	SNL_INV_Permute_Loops(outer_loop, permutation, nloops, TRUE);
	WN* wn_temp = stack->Bottom_nth(first_in_stack + i); 
        for (k = i + 1; k <= j; k++) 
	  stack->Bottom_nth(first_in_stack + k - 1) 
	    = stack->Bottom_nth(first_in_stack + k); 
        stack->Bottom_nth(first_in_stack + j) = wn_temp; 
        outer_loop = stack->Bottom_nth(first_in_stack);    
        inner_loop = stack->Bottom_nth(first_in_stack + nloops - 1);    
	break; 
      }
      j++;  
    } 
  }
  CXX_DELETE_ARRAY(permutation, &LNO_local_pool);
  CXX_DELETE(stack, &LNO_local_pool); 
}

//-----------------------------------------------------------------------
// NAME: SNL_Find_Traverse 
// FUNCTION: Traverse the tree rooted at 'wn_tree', adding SNLs to the 
//  'outer_stack' and 'inner_stack'.  Each SNL is specified as a pair  
//  of loops, an outermost and innermost loop.  The i-th pair is stored
//  as outer_stack->Bottom_nth(i) and inner_stack->Bottom_nth(i).  The 
//  'local_stack' is used for intermediate computations. 
//-----------------------------------------------------------------------

static void SNL_Find_Traverse(WN* wn_tree, 
		     STACK<WN*>* outer_stack, 
	             STACK<WN*>* inner_stack, 
		     STACK<WN*>* local_stack) 
{
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      SNL_Find_Traverse(wn, outer_stack, inner_stack, local_stack);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      SNL_Find_Traverse(WN_kid(wn_tree, i), outer_stack, inner_stack, 
	local_stack);
  }
  switch (WN_opcode(wn_tree)) {
  case OPC_DO_LOOP:
    {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree); 
      if (dli->Is_Inner) { 
        outer_stack->Push(wn_tree); 
        inner_stack->Push(wn_tree);
        local_stack->Push(wn_tree);
      } else if (local_stack->Elements() > 0) {
        INT inner_count = 0; 
        for (INT i = 0; i < local_stack->Elements(); i++) {
          WN* wn = local_stack->Bottom_nth(i); 
          switch (WN_opcode(wn)) {
	  case OPC_DO_LOOP: 
	    {
	      DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn);
	      if (dli_inner->Depth == dli->Depth + 1)
	        inner_count++;   
            }
 	    break; 
	  case OPC_DO_WHILE:
  	  case OPC_WHILE_DO:
  	  case OPC_IF:
	    break;
          } 
	  if (inner_count > 1) 
	    break;
        }
        if (inner_count == 1) {
	  local_stack->Push(wn_tree); 
	  outer_stack->Top_nth(0) = wn_tree; 
        } else {
          local_stack->Clear(); 
        }
      }
    }
    break;
  case OPC_DO_WHILE: 
  case OPC_WHILE_DO:
    local_stack->Clear();  
    break;
  case OPC_IF: 
    INT i;
    for (i = 0; i < local_stack->Elements(); i++) 
      if (Wn_Is_Inside(local_stack->Bottom_nth(i), wn_tree))
	break;
    if (i < local_stack->Elements())
      local_stack->Clear(); 
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_Find 
// FUNCTION: Find all of the SNLs in the tree rooted at 'wn_root'. Each 
//   SNL is stored as a pair of loops.  The i-th SNL has its outermost 
//   loop stored in 'outer_stack->Bottom_nth(i)' and its inner loop 
//   stored in 'inner_stack->Bottom_nth(i)'.  
//-----------------------------------------------------------------------

extern void SNL_Find(WN* wn_root,
                     STACK<WN*>* outer_stack,
                     STACK<WN*>* inner_stack)
{
  STACK<WN*> local_stack(&LNO_local_pool); 
  SNL_Find_Traverse(wn_root, outer_stack, inner_stack, &local_stack);  
}

//-----------------------------------------------------------------------
// NAME: Lego_Interchange 
// FUNCTION: Attempt to interchange all outer lego tiles outermost in
//   the function 'wn_root'. 
//-----------------------------------------------------------------------

extern void Lego_Interchange(WN* wn_root)
{
  dg = Array_Dependence_Graph;
  du = Du_Mgr;
  STACK<WN*> outer_stack(&LNO_local_pool);
  STACK<WN*> inner_stack(&LNO_local_pool);
  SNL_Find(wn_root, &outer_stack, &inner_stack);  
  FmtAssert(outer_stack.Elements() == inner_stack.Elements(), 
    ("Unmatched outer and inner stacks while finding SNLs")); 
  for (INT i = 0; i < outer_stack.Elements(); i++) {
    WN* outer_loop = outer_stack.Bottom_nth(i); 
    WN* inner_loop = inner_stack.Bottom_nth(i); 
    if (outer_loop == inner_loop)
      continue;
    // if (!SNL_Lift_Lego_Tile_Loops_Once(outer_loop, inner_loop))  
      SNL_Lift_Lego_Tile_Loops_Shifts(outer_loop, inner_loop); 
  }
}  

// Forward declaration.
static void Lego_Peel_Traverse(WN* wn_tree);

//-----------------------------------------------------------------------
// NAME: Lego_Block_Peel_Traverse  
// FUNCTION: Call Lego_Peel_Traverse for wn_tree, 
//  where wn_tree is a kid of a BLOCK node.
//  If some pre-peel happens then process code that gets added before
//  this node.  post-peel is processed automatically as part of the
//  forward walk.
//-----------------------------------------------------------------------

static void Lego_Block_Peel_Traverse(WN* wn_tree) {
  Is_True (wn_tree && LWN_Get_Parent(wn_tree) &&
           WN_opcode(LWN_Get_Parent(wn_tree)) == OPC_BLOCK,
           ("Incorrect arguments to Lego_Block_Peel_Traverse"));

  WN* wn_prev = WN_prev(wn_tree);
  Lego_Peel_Traverse(wn_tree);
  if (wn_prev != WN_prev(wn_tree)) {
    // some pre-peel got added just before wn. Process it.
    // Find the first new node. 
    // It is important to process the new nodes in the forward direction,
    // since we are only trying to process the pre-peel, NOT the post-peel.
    // That automatically gets processed as part of the forward walk.
    WN* wn_new;
    if (wn_prev == NULL) wn_new = WN_first(LWN_Get_Parent(wn_tree));
    else wn_new = WN_next(wn_prev);
    // now wn_new is the first new node.
    while (wn_new != wn_tree) {
      Lego_Block_Peel_Traverse(wn_new);
      wn_new = WN_next(wn_new);
    }
  }
}


static const INT PEEL_UNROLL_LIMIT = 20;  

//-----------------------------------------------------------------------
// NAME: Lego_Peel_Traverse  
// FUNCTION: Peel loops for Lego in the tree rooted at 'wn_tree' according 
//  to the informnation in the LEGO_INFO of each DO loop. 
//-----------------------------------------------------------------------

static void Lego_Peel_Traverse(WN* wn_tree)
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    INT statement_count = 0; 
    for (WN* wn = WN_first(WN_do_body(wn_tree)); wn != NULL; 
      wn = WN_next(wn))
      statement_count++;   
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree); 
    LEGO_INFO* lgi = dli->Lego_Info;
    if (lgi != NULL) { 
      INT front_peel = lgi->Front_Peel(); 
      BOOL create_loop;
      if (front_peel > 0) {
        create_loop = (front_peel > 1 &&
                       (statement_count*front_peel > PEEL_UNROLL_LIMIT));
        if (LNO_Verbose) {
	  fprintf(stdout, "Lego Peeling Loop %s, %d iterations\n", 
	    WB_Whirl_Symbol(wn_tree), front_peel);
	  fprintf(TFile, "Lego Peeling Loop %s, %d iterations\n", 
	    WB_Whirl_Symbol(wn_tree), front_peel);
        }
        // for updating RR_Map
        WN* prev_wn = WN_prev(wn_tree);
        Pre_loop_peeling(wn_tree, front_peel, !create_loop);
        Pre_Peel_RR_Map_Update (wn_tree, prev_wn, create_loop);
        if (create_loop) {
          WN* peeled_wn = WN_prev(wn_tree);
          Is_True (peeled_wn && WN_opcode(peeled_wn) == OPC_DO_LOOP,
                   ("Where is the peeled portion?"));
          DO_LOOP_INFO* dli = Get_Do_Loop_Info (peeled_wn);
          LEGO_INFO* li = dli->Lego_Info;
          Is_True (li, ("Where is the lego-info"));
          CXX_DELETE (li, LEGO_pool);
          dli->Lego_Info = NULL;
        }
      }
      INT back_peel = lgi->Back_Peel(); 
      if (back_peel > 0)  {
        create_loop = (back_peel > 1 &&
                       (statement_count*back_peel > PEEL_UNROLL_LIMIT));
        if (LNO_Verbose) {
	  fprintf(stdout, "Lego Peeling Loop %s, %d iterations\n", 
	    WB_Whirl_Symbol(wn_tree), back_peel);
	  fprintf(TFile, "Lego Peeling Loop %s, %d iterations\n", 
	    WB_Whirl_Symbol(wn_tree), back_peel);
        }
        WN* next_wn = WN_next(wn_tree);
        Post_loop_peeling(wn_tree, back_peel, !create_loop);
        Post_Peel_RR_Map_Update (wn_tree, next_wn, create_loop);
        if (create_loop) {
          WN* peeled_wn = WN_next(wn_tree);
          Is_True (peeled_wn && WN_opcode(peeled_wn) == OPC_DO_LOOP,
                   ("Where is the peeled portion?"));
          DO_LOOP_INFO* dli = Get_Do_Loop_Info (peeled_wn);
          LEGO_INFO* li = dli->Lego_Info;
          Is_True (li, ("Where is the lego-info"));
          CXX_DELETE (li, LEGO_pool);
          dli->Lego_Info = NULL;
        }
      }
    }
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
     for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
       Lego_Block_Peel_Traverse(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Lego_Peel_Traverse(WN_kid(wn_tree, i));
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Peel 
// FUNCTION: Carry out the peeling commands indicated in the LEGO_INFO 
//   of each loop in the function 'wn_root'. 
//-----------------------------------------------------------------------

extern void Lego_Peel(WN* wn_root) 
{
  Lego_Peel_Traverse(wn_root); 
}

//-----------------------------------------------------------------------
// NAME: Is_Nested_Do_Across 
// FUNCTION: Returns TRUE if the 'wn_loop' is a nested do across loop, 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Nested_Do_Across(WN* wn_loop)
{
  if (WN_opcode(wn_loop) != OPC_DO_LOOP)
    return FALSE; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->Mp_Info == NULL) 
    return FALSE; 
  return dli->Mp_Info->Nest_Index() == 0 
    && dli->Mp_Info->Nest_Total() > 1;
}  

//-----------------------------------------------------------------------
// NAME: Is_Rectangular_Nested_Doacross 
// FUNCTION: Returns TRUE if the nested do across rooted at 'wn_loop' has
//   a rectangular iteration space, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Rectangular_Nested_Doacross(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  INT nloops = dli->Mp_Info->Nest_Total(); 
  WN* wn_inner_loop = SNL_Get_Inner_Snl_Loop(wn_loop, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_loop, &stack);
  for (INT i = 1; i < nloops; i++) 
    if (!SNL_Is_Invariant(&stack, 0, i))
      return FALSE; 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Redundant_Pragma 
// FUNCTION: Returns TRUE if the 'wn_pragma' node is already contained 
//   in the pragma block of the region 'wn_region', FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Redundant_Pragma(WN* wn_pragma, 
			     WN* wn_region)
{
  OPCODE op = WN_opcode(wn_pragma); 
  FmtAssert(op == OPC_PRAGMA || op == OPC_XPRAGMA, 
    ("Redundant_Pragma: Expected only OPC_[X]PRAGMA nodes"));
 
  switch (WN_pragma(wn_pragma)) {
  case WN_PRAGMA_LOCAL:
  case WN_PRAGMA_SHARED:
  case WN_PRAGMA_LASTLOCAL:
  case WN_PRAGMA_REDUCTION: 
  case WN_PRAGMA_FIRSTPRIVATE: 
  case WN_PRAGMA_END_MARKER:
    {
      WN* wn_first = WN_first(WN_region_pragmas(wn_region)); 
      for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) 
        if (WN_pragma(wn) == WN_pragma(wn_pragma) 
          && WN_st(wn) == WN_st(wn_pragma)
	  && WN_pragma_arg1(wn) == WN_pragma_arg1(wn_pragma))
	    return TRUE; 
      return FALSE;  
    }
  default:
    FmtAssert(FALSE, ("Bad pragma in inner nested do across."));
    return FALSE; 
  }
} 

//-----------------------------------------------------------------------
// NAME: Mp_Extract_Nested_Pragmas 
// FUNCTION: Extract the pragmas from 'wn_inregion', placing them in the 
//   pragma block for 'wn_region' (with the exeception of the DOACROSS 
//   pragma, which is discarded). 
//-----------------------------------------------------------------------

static void Mp_Extract_Nested_Pragmas(WN* wn_region,
				      WN* wn_inregion)
{
  WN* wnn = NULL; 
  WN* wn_pragmas = WN_region_pragmas(wn_region); 
  WN* wn_first = WN_first(WN_region_pragmas(wn_inregion));
  for (WN* wn = wn_first; wn != NULL; wn = wnn) {
    wnn = WN_next(wn);
    LWN_Extract_From_Block(wn);
    if ((WN_opcode(wn) == OPC_PRAGMA || WN_opcode(wn) == OPC_XPRAGMA) 
        && (WN_pragma(wn) == WN_PRAGMA_DOACROSS  
        ||  WN_pragma(wn) == WN_PRAGMA_PDO_BEGIN
        ||  WN_pragma(wn) == WN_PRAGMA_PARALLEL_DO)
	|| Redundant_Pragma(wn, wn_region)) 
      LWN_Delete_Tree(wn);
    else 
      LWN_Insert_Block_After(wn_pragmas, WN_first(wn_pragmas), wn); 
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Remove_Nested_Region 
// FUNCTION: Remove the region 'wn_region' while retaining the code within
//   the region. (Actually stack the regions on 'stack_regions' and delete
//   them laster, since they need to be deleted in reverse order.) 
//-----------------------------------------------------------------------

static void Mp_Remove_Nested_Region(WN* wn_region, 
				    STACK<WN*>* stack_regions)
{
  WN* wnn = NULL; 
  WN* wn_after = wn_region; 
  WN* wn_first = WN_first(WN_region_body(wn_region));
  for (WN* wn = wn_first; wn != NULL; wn = wnn) {
    wnn = WN_next(wn); 
    LWN_Extract_From_Block(wn); 
    LWN_Insert_Block_After(LWN_Get_Parent(wn_region), wn_after, wn); 
    wn_after = wn; 
  }
  LWN_Extract_From_Block(wn_region);
  stack_regions->Push(wn_region);  
}

//-----------------------------------------------------------------------
// NAME: Mp_Region_Under_Loop 
// FUNCTION: Returns the first REGION node in the statement list of the 
//   the loop 'wn_loop'.  Returns NULL if there is none. 
//-----------------------------------------------------------------------

static WN* Mp_Region_Under_Loop(WN* wn_loop)
{
  WN* wn_first = WN_first(WN_do_body(wn_loop)); 
  WN* wn = 0;
  for (wn = wn_first; wn != NULL; wn = WN_next(wn))
    if (WN_opcode(wn) == OPC_REGION)
      break;
  return wn;
}

//-----------------------------------------------------------------------
// NAME: Mp_Compress_Nested_Loop
// FUNCTION: Compress the nested do across loop 'wn_loop' by moving all 
//   of the pragmas of the inner do across loops to the outer MP region
//   and elimiating these inner regions. 
//-----------------------------------------------------------------------

extern void Mp_Compress_Nested_Loop(WN* wn_loop)
{
  WN* wn_inloop = NULL; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  INT region_count = dli->Mp_Info->Nest_Index();  
  INT region_total = dli->Mp_Info->Nest_Total(); 
  FmtAssert(region_count == 0, 
    ("Bad Nest_Index() in outer nested do across"));
  FmtAssert(region_total >= 1,
    ("Bad Nest_Total() in outer nested do across"));
  STACK<WN*> stack_regions(&LNO_local_pool);  
  for (WN* wn_inregion = Mp_Region_Under_Loop(wn_loop); TRUE; 
    wn_inregion = Mp_Region_Under_Loop(wn_inloop)) {
    if (wn_inregion == NULL || WN_opcode(wn_inregion) != OPC_REGION)
      break; 
    wn_inloop = NULL; 
    WN* wn_first = WN_first(WN_region_body(wn_inregion));
    for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) 
      if (WN_opcode(wn) == OPC_DO_LOOP)
	wn_inloop = wn;
    FmtAssert(wn_inloop != NULL && WN_opcode(wn_inloop) == OPC_DO_LOOP, 
      ("Didn't find nested doacross loop as expected."));
    DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inloop);
    region_count++; 
    FmtAssert(dli_inner->Mp_Info->Nest_Index() == region_count, 
      ("Bad Nest_Index() in inner nested do across"));
    FmtAssert(dli_inner->Mp_Info->Nest_Total() == region_total, 
      ("Bad Nest_Total() in inner nested do across"));
    Mp_Extract_Nested_Pragmas(wn_region, wn_inregion);
    Mp_Remove_Nested_Region(wn_inregion, &stack_regions);
  }
  for (INT i = 0; i < stack_regions.Elements(); i++)
    LWN_Delete_Tree(stack_regions.Top_nth(i)); 
} 

//-----------------------------------------------------------------------
// NAME: Permutation_Last
// FUNCTION: Returns the last index in the permutation subsequence in
//   'permutation' which begins with 'first'.
// EXAMPLE: (1 0 2 4 3) has three permutation subsequences (1 0), (2),
//   and (4 3).  In this case:
//     Permutation_Last(0, permutation, 5) returns 1
//     Permutation_Last(2, permutation, 5) returns 2
//     Permutation_Last(3, permutation, 5) returns 4
//-----------------------------------------------------------------------

extern INT Permutation_Last(INT first,
                            INT permutation[],
                            INT nloops)
{
  INT last = permutation[first];
  for (INT i = first; i < nloops; i++) {
    if (permutation[i] > last)
      last = permutation[i];
    if (last == i)
      return last;
  }
  FmtAssert(TRUE, ("last must be in [0..nloops-1]"));
  return nloops;
}

