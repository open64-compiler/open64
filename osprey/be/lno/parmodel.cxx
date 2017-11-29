/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <limits.h>
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
#include "scalar_expand.h"
#include "cxx_memory.h"
#include "reduc.h"
#include "snl_utils.h"
#include "sxlist.h"
#include "snl_dist.h"
#include "permute.h"
#include "sxlimit.h"
#include "parallel.h"
#include "fiz_fuse.h"
#include "ara.h"
#include "snl_deps.h"
#include "lego_util.h"
#include "tile.h"
#include "model.h"
#include "cache_model.h"
#include "config_cache.h"
#include "config_lno.h"
#include "parmodel.h"
#include "wn_mp.h"
#include "call_info.h" 
#include "ipa_lno_cost.h" 

INT PAR_STAT::id_count = 0; 
INT Parallel_Debug_Level = 3; 

//-----------------------------------------------------------------------
// NAME: PAR_STAT 
// FUNCTION: Create a PAR_STAT node with default values for each of the
//   fields. 
//-----------------------------------------------------------------------

PAR_STAT::PAR_STAT()
{
  _next = NULL; 
  _prev = NULL; 
  _parent = NULL; 
  _first = NULL;  
  _last = NULL; 
  _depth = 0;  
  _is_parallel = FALSE; 
  _count = 0;  
  _id = 0;  
  _wn = NULL;  
  _is_cloned = FALSE; 
  _num_estimated_iters = -1;
} 

//-----------------------------------------------------------------------
// NAME: PAR_STAT 
// FUNCTION: Create a PAR_STAT node by cloning the PAR_STAT node 'ps'.
//-----------------------------------------------------------------------

PAR_STAT::PAR_STAT(PAR_STAT* ps)
{
  FmtAssert(ps != NULL, ("Tried to copy from NULL source")); 
  _next = NULL; 
  _prev = NULL; 
  _parent = NULL; 
  _first = NULL;  
  _last = NULL;  
  _is_parallel = ps->_is_parallel; 
  _depth = ps->_depth; 
  _count = ps->_count;
  _id = ps->_id;  
  _wn = ps->_wn; 
  _is_cloned = TRUE; 
  _num_estimated_iters = -1;
}

//-----------------------------------------------------------------------
// NAME: Has_Loop 
// FUNCTION: Returns TRUE if the PAR_STAT tree contains a DO LOOP, FALSE
//   otherwise.
//-----------------------------------------------------------------------

BOOL PAR_STAT::Has_Loop()
{
  if (WN_opcode(_wn) == OPC_DO_LOOP) 
    return TRUE; 
  for (PAR_STAT* ps = _first; ps != NULL; ps = ps->_next)
    if (ps->Has_Loop())
      return TRUE; 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Is_Outer_Loop 
// FUNCTION: Returns TRUE if the PAR_STAT node represents an outermost 
//   loop, FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL PAR_STAT::Is_Outer_Loop()
{
  if (WN_opcode(_wn) != OPC_DO_LOOP)
    return FALSE;
  for (PAR_STAT* ps = _parent; ps != NULL; ps = ps->_parent)
    if (WN_opcode(_wn) == OPC_DO_LOOP)
      return FALSE;
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Is_Inner_Loop 
// FUNCTION: Returns TRUE if the PAR_STAT node represents an innermost 
//   loop, FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL PAR_STAT::Is_Inner_Loop()
{
  if (WN_opcode(_wn) != OPC_DO_LOOP)
    return FALSE; 
  for (PAR_STAT* ps = _first; ps != NULL; ps = ps->_next)
    if (ps->Has_Loop())
      return FALSE; 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Is_Parallel_Enclosed_Loop
// FUNCTION: Returns TRUE if the PAR_STAT tree is a parallel loop or is
//   enclosed within a parallel loop, returns FALSE otherwise.
//-----------------------------------------------------------------------

BOOL PAR_STAT::Is_Parallel_Enclosed_Loop()
{
  for (PAR_STAT* ps = this; ps != NULL; ps = ps->_parent) 
    if (WN_opcode(_wn) == OPC_DO_LOOP && ps->_is_parallel)
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Remove
// FUNCTION: Remove the PAR_STAT node from its PAR_STAT tree. 
//-----------------------------------------------------------------------

void PAR_STAT::Remove()
{
  if (_parent != NULL) {
    if (_parent->_first == this)
      _parent->_first = this->_next; 
    if (_parent->_last == this) 
      _parent->_last = this->_prev; 
  }
  if (_prev != NULL) 
    _prev->_next = this->_next; 
  if (_next != NULL) 
    _next->_prev = this->_prev; 
  _next = NULL; 
  _prev = NULL; 
  _parent = NULL; 
}

//-----------------------------------------------------------------------
// NAME: Make_Parent 
// FUNCTION: Make 'ps_parent' the parent of this node.  If 'first', make
//   it 'ps_parent's first child, otherwise make it 'ps_parent's last child. 
//-----------------------------------------------------------------------

void PAR_STAT::Make_Parent(PAR_STAT* ps_parent, 
			   BOOL first) 
{
  _parent = ps_parent; 
  _depth = 0; 
  if (ps_parent != NULL) {
    _depth = ps_parent->_depth + 1; 
    if (first) { 
      _next = ps_parent->_first; 
      _prev = NULL;
      if (_next != NULL)
        _next->_prev = this;
      if (ps_parent->_last == NULL)
        ps_parent->_last = this;
      ps_parent->_first = this;  
    } else {  
      _prev = ps_parent->_last;  
      _next = NULL; 
      if (_prev != NULL) 
	_prev->_next = this; 
      if (ps_parent->_first == NULL) 
	ps_parent->_first = this; 
      ps_parent->_last = this; 
    } 
  }
}

//-----------------------------------------------------------------------
// NAME: Make_Sibling 
// FUNCTION: Make 'ps_sibling' the sibling of this node.  If 'above' make
//   it 'ps_sibling's immediate older sibling, otherwise make it 'ps_sib-
//   ling's immediate younger sibling. 
//-----------------------------------------------------------------------

void PAR_STAT::Make_Sibling(PAR_STAT* ps_sibling, 
			    BOOL above)
{
  ps_sibling->_parent = _parent; 
  if (above) { 
    if (_parent != NULL && _parent->_first == this) 
      _parent->_first = ps_sibling; 
    if (_prev != NULL) 
      _prev->_next = ps_sibling; 
    ps_sibling->_prev = _prev; 
    ps_sibling->_next = this; 
    _prev = ps_sibling;
  } else { 
    if (_parent != NULL && _parent->_last == this)
      _parent->_last = ps_sibling; 
    if (_next != NULL)
      _next->_prev = ps_sibling; 
    ps_sibling->_prev = this; 
    ps_sibling->_next = _next; 
    _next = ps_sibling; 
  } 
}

//-----------------------------------------------------------------------
// NAME: Find 
// FUNCTION: Find the node in the PAR_STAT tree which has a '_wn' field 
//   equal to 'wn'.  If 'uncloned', look only for an uncloned node.  Other- 
//   wise, return the first node encountered on a depth first search of 
//   the tree. 
//-----------------------------------------------------------------------

PAR_STAT* PAR_STAT::Find(WN* wn,
			 BOOL uncloned)
{
  if (wn == _wn && (!uncloned || !_is_cloned))
    return this; 
  if (_first != NULL) {
    PAR_STAT* ps_first = _first->Find(wn, uncloned); 
    if (ps_first != NULL) 
      return ps_first; 
  } 
  if (_next != NULL) {
    PAR_STAT* ps_next = _next->Find(wn, uncloned); 
    if (ps_next != NULL) 
      return ps_next; 
  } 
  return NULL; 
}

//-----------------------------------------------------------------------
// NAME: Innermost_Sandwiched_Code 
// FUNCTION: Return a pointer to the sandwiched innermost code which will
//   be obtained when the sandwiched code in the PAR_STAT tree but outside
//   the nest represented by 'ps_inner' is distributed out.  If 'above',
//   consider the code above 'ps_inner', otherwise consider the code below 
//   'ps_inner'. 
//-----------------------------------------------------------------------

PAR_STAT* PAR_STAT::Innermost_Sandwiched_Code(PAR_STAT* ps_inner, 
					      BOOL above)
{
  DOLOOP_STACK loop_stack(&LNO_local_pool); 
  Build_Doloop_Stack(ps_inner->_wn, &loop_stack); 

  PAR_STAT* ps = this; 
  PAR_STAT* ps_stat = NULL; 
  INT start_depth = this->_depth; 
  INT end_depth = ps_inner->_depth; 
  for (INT i = start_depth + 1; i <= end_depth; i++) {
    WN* wn_loop = loop_stack.Bottom_nth(i); 
    PAR_STAT* pss =  ps->_first; 
    if (above) { 
      for (ps = pss; ps->_is_cloned || ps->_wn != wn_loop; ps = ps->_next) 
	ps_stat = ps; 
    } else { 
      for (ps = pss; ps->_is_cloned || ps->_wn != wn_loop; ps = ps->_next); 
      PAR_STAT* ps_save = ps; 
      for (ps = ps->_next; ps != NULL; ps = ps->_next)
        ps_stat = ps;  
      ps = ps_save; 
    }  
  }
  return ps_stat; 
}

//-----------------------------------------------------------------------
// NAME: Distribute
// FUNCTION: Transform the PAR_STAT tree by distributing sandwiched code
//   outside of 'ps_inner'.  If 'above' distribute out the code above
//   'ps_inner', otherwise distribute out the code below 'ps_inner'.
//   Return a pointer to the distributed part.
//-----------------------------------------------------------------------

PAR_STAT* PAR_STAT::Distribute(PAR_STAT* ps_inner, 
                               BOOL above)
{
  DOLOOP_STACK loop_stack(&LNO_local_pool); 
  Build_Doloop_Stack(ps_inner->_wn, &loop_stack); 

  PAR_STAT* ps_stat = Innermost_Sandwiched_Code(ps_inner, above);
  if (ps_stat == NULL)
    return NULL; 

  PAR_STAT* ps_dist = CXX_NEW(PAR_STAT(this), &LNO_local_pool); 
  PAR_STAT* ps_old_loop = ps_dist; 
  PAR_STAT* ps = this; 
  INT start_depth = this->_depth; 
  INT end_depth = ps_inner->_depth; 
  for (INT i = start_depth + 1; i <= end_depth; i++) {
    WN* wn_loop = loop_stack.Bottom_nth(i); 
    if (above) {
      PAR_STAT* pss = NULL; 
      for (ps = ps->_first; ps->_is_cloned || ps->_wn != wn_loop; ps = pss) {
        pss = ps->_next; 
        ps->Remove(); 
        ps->Make_Parent(ps_old_loop, FALSE); 
        if (ps == ps_stat) 
          break;
      }
      if (ps == ps_stat) 
        break; 
      PAR_STAT* ps_new_loop = CXX_NEW(PAR_STAT(ps), &LNO_local_pool);
      ps_new_loop->Make_Parent(ps_old_loop, FALSE); 
      ps_old_loop = ps_new_loop; 
    } else { 
      PAR_STAT* pss = NULL; 
      for (ps = ps->_first; ps->_is_cloned || ps->_wn != wn_loop; ps = pss)
        pss = ps->_next;
      PAR_STAT* ps_loop = ps;
      pss = NULL; 
      for (ps = ps ->_next; ps != NULL; ps = pss) {
        pss = ps->_next; 
        ps->Remove(); 
        ps->Make_Parent(ps_old_loop, FALSE); 
        if (ps == ps_stat)
	  break;
      }
      if (ps == ps_stat)
        break; 
      ps = ps_loop; 
      PAR_STAT* ps_new_loop = CXX_NEW(PAR_STAT(ps), &LNO_local_pool);
      ps_new_loop->Make_Parent(ps_old_loop, TRUE);  
      ps_old_loop = ps_new_loop; 
    }
  }
  Make_Sibling(ps_dist, above); 
  return ps_dist;  
}

//-----------------------------------------------------------------------
// NAME: PAR_STAT
// FUNCTION: Create a PAR_STAT tree to represent the SNL rooted at 'wn_tree'
//   consisting of 'nloops' using memory from 'pool'.  If 'ps_parent != NULL', 
//   make this PAR_STAT tree the last child of 'ps_parent'.
//-----------------------------------------------------------------------

PAR_STAT::PAR_STAT(WN* wn_tree, 
		   INT nloops, 
		   MEM_POOL* pool,
		   PAR_STAT* ps_parent)
{
  _next = NULL; 
  _prev = NULL; 
  _first = NULL; 
  _last = NULL; 
  _parent = NULL;
  _num_estimated_iters = -1;
  _depth = WN_opcode(wn_tree) == OPC_DO_LOOP 
    ? Do_Loop_Depth(wn_tree) : Loop_Depth(wn_tree) + 1; 
  _is_parallel = FALSE; 
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree); 
    _count = MAX(dli->Est_Num_Iterations, 1); 
  } else { 
    _count = 0;
  } 
  _wn = wn_tree;
  _id = id_count++; 
  _is_cloned = FALSE; 
  if (ps_parent != NULL) 
    Make_Parent(ps_parent, FALSE); 
  if (WN_opcode(wn_tree) == OPC_DO_LOOP && nloops >= 1) {
    WN* wn_first = WN_first(WN_do_body(wn_tree)); 
    for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) 
      CXX_NEW(PAR_STAT(wn, nloops - 1, pool, this), pool); 
  }       
}

//-----------------------------------------------------------------------
// NAME: Distribute_For_Permutation 
// FUNCTION: Distribute the PAR_STAT tree as if SNL_Distribute_For_Permu-
//   tation() were applied. (See description in snl_dist.cxx). 
//-----------------------------------------------------------------------

PAR_STAT* PAR_STAT::Distribute_For_Permutation(WN* wn_outer,
                                               WN* wn_inner,
                                               INT permutation[],
                                               INT nloops)
{
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  INT last = -1;
  INT outer_index = Do_Loop_Depth(wn_outer);
  PAR_STAT* ps_return = this; 
  for (INT first = 0; first < nloops; first = last + 1) {
    last = Permutation_Last(first, permutation, nloops);
    INT first_depth = outer_index + first;
    INT last_depth = outer_index + last;
    PAR_STAT* ps_outer = Find(stack.Bottom_nth(first_depth), TRUE); 
    PAR_STAT* ps_inner = Find(stack.Bottom_nth(last_depth), TRUE); 
    PAR_STAT* ps_above = ps_outer->Distribute(ps_inner, TRUE);
    if (ps_outer == this && ps_above != NULL)
      ps_return = ps_above; 
    PAR_STAT* ps_below = ps_outer->Distribute(ps_inner, FALSE);
  }
  return ps_return; 
}

//-----------------------------------------------------------------------
// NAME: Distribute_By_Splitting 
// FUNCTION: Distribute the PAR_STAT tree as if SNL_Distribute_By_Split-
//   ting() were applied. (See description in snl_dist.cxx). 
//-----------------------------------------------------------------------

PAR_STAT* PAR_STAT::Distribute_By_Splitting(WN* wn_outer, 
					    WN* wn_inner, 
				            INT nloops, 
					    INT split_depth)
{
  if (wn_outer == NULL || nloops == 0) 
    return this;
  INT outer_depth = Do_Loop_Depth(wn_outer);
  if (split_depth == -1 || split_depth == outer_depth)
    return this; 
  DOLOOP_STACK local_stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &local_stack);
  PAR_STAT* ps_inner = Find(local_stack.Bottom_nth(split_depth), TRUE); 
  PAR_STAT* ps_above = Distribute(ps_inner, TRUE);
  Distribute(ps_inner, FALSE);
  return ps_above != NULL ? ps_above : this; 
}
  
//-----------------------------------------------------------------------
// NAME: Permute_Loops 
// FUNCTION: Transform the PAR_STAT tree as if SNL_Permute_Loops() were 
//   applied.  (See description in snl_trans.cxx).
//-----------------------------------------------------------------------

void PAR_STAT::Permute_Loops(WN* wn_outer, 
			     WN* wn_inner, 
		   	     INT permutation[], 
			     INT nloops)
{
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  PAR_STAT* ps_array = CXX_NEW_ARRAY(PAR_STAT, nloops, &LNO_local_pool); 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  PAR_STAT* ps = this; 
  INT i;
  for (i = 0; i < nloops; i++, ps = ps->_first) {
    for (; ps->_is_cloned || ps->_wn != stack.Bottom_nth(outer_depth + i); 
      ps = ps->_next); 
    ps_array[i] = *ps; 
  } 
  ps = this; 
  for (i = 0; i < nloops; i++, ps = ps->_first) {
    for (; ps->_is_cloned || ps->_wn != stack.Bottom_nth(outer_depth + i); 
      ps = ps->_next); 
    ps->_is_parallel = ps_array[permutation[i]]._is_parallel; 
    ps->_count = ps_array[permutation[i]]._count; 
    ps->_id = ps_array[permutation[i]]._id; 
    ps->_wn = ps_array[permutation[i]]._wn; 
  }
  CXX_DELETE_ARRAY(ps_array, &LNO_local_pool); 
}

//-----------------------------------------------------------------------
// NAME: Parallel_Interchange 
// FUNCTION: Transform the PAR_STAT tree as if Parallel_Interchange() were
//   applied.  (See description in parallel.cxx). 
//-----------------------------------------------------------------------

PAR_STAT* PAR_STAT::Parallel_Interchange(WN* wn_outer,
                                         INT permutation[],
                                         INT nloops,
                                         INT parallel_depth,
					 INT sd_split_depth,
                                         INT split_depth)
{
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  PAR_STAT* ps_return = NULL; 
  PAR_STAT* ps_root = Distribute_By_Splitting(wn_outer, wn_inner, nloops, 
    sd_split_depth); 
  if (ps_return == NULL && ps_root != this) 
    ps_return = ps_root; 
  ps_root = Distribute_For_Permutation(wn_outer, wn_inner, permutation, 
    nloops);
  if (ps_return == NULL && ps_root != this) 
    ps_return = ps_root; 
  ps_root = Distribute_By_Splitting(wn_outer, wn_inner, nloops, split_depth); 
  if (ps_return == NULL && ps_root != this) 
    ps_return = ps_root; 
  Permute_Loops(wn_outer, wn_inner, permutation, nloops);  
  PAR_STAT* ps_parallel_loop = NULL; 
  for (INT i = 0; i < stack.Elements(); i++) {
    WN* wn_loop = stack.Bottom_nth(i); 
    PAR_STAT* ps_loop = Find(wn_loop, TRUE); 
    if (ps_loop != NULL && ps_loop->_depth == parallel_depth) {
      ps_parallel_loop = ps_loop; 
      break; 
    }
  }
  if (ps_parallel_loop != NULL) 
    ps_parallel_loop->_is_parallel = TRUE; 
  return ps_return == NULL ? this : ps_return; 
}

//-----------------------------------------------------------------------
// NAME: Estimate_Cycles  
// FUNCTION: Returns an estimate of the cost of executing one iteration 
//   of the loop 'wn_loop'.
// NOTE: This is a backup for when the REGISTER_MODEL fails.  Right now 
//   we charge one cycle per node. 
//-----------------------------------------------------------------------

static double Estimate_Cycles(WN* wn_loop)
{
  double cost = 0.0; 
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop)); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    cost++; 
  }
  return cost; 
}

//-----------------------------------------------------------------------
// NAME: Machine_Cost_Calls
// FUNCTION: Return the cost of calls which have non-NULL CALL_INFOs. 
//   (This information is obtained from IPA array analysis.)
//-----------------------------------------------------------------------

static double Machine_Cost_Calls(WN* wn_loop) 
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (!dli->Has_Calls)
    return 0;
  INT64 call_cost = 0; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn_call = itr->wn;
    if (WN_operator(wn_call) == OPR_CALL && Has_Execution_Cost(wn_call))
      call_cost += Simple_Execution_Cost(wn_call);
  } 
  return (double) call_cost;
} 


//-----------------------------------------------------------------------
// NAME: Estimate_IF_Cost
// FUNCTION: Returns an estimate of the cost of executing an IF statement,
//   under the assumption that the machine cost is the sum of the cost of
//   the IF test and the larger of the THEN and ELSE blocks.  We don't
//   split the cache cost between the THEN and ELSE blocks because it's too
//   hard--the cache model just doesn't support this.  However, to the
//   extent that we take the THEN and ELSE branches with equal frequency,
//   the actual cache footprint will usually be similar to the modelled
//   footprint so the cache cost error won't be so large.
//-----------------------------------------------------------------------

static double
Estimate_IF_Cost(WN *wn_if, WN *inner_do_loop, WN2INT *se_needed,
                 HASH_TABLE<WN *, BIT_VECTOR *> *invar_table)
{
  Is_True(WN_operator(wn_if) == OPR_IF, ("not an IF"));
  Is_True(WN_operator(inner_do_loop) == OPR_DO_LOOP, ("not a DO_LOOP"));

  REGISTER_MODEL test_model(&LNO_local_pool);

  INT fp_regs_used_out = 0;
  INT int_regs_used_out = 0;
  INT tlb_used_out = 0;

  double return_cost = 0.0;
  test_model.Add_Statement(WN_if_test(wn_if));
  test_model.Evaluate(inner_do_loop, se_needed, invar_table,
                      &return_cost, &fp_regs_used_out, &int_regs_used_out,
                      &tlb_used_out);

  double then_cost = 0.0, else_cost = 0.0;

  if (WN_first(WN_then(wn_if))) {
    REGISTER_MODEL then_model(&LNO_local_pool);
    double then_if_cost = 0.0;

    for (WN *wn = WN_first(WN_then(wn_if)); wn; wn = WN_next(wn)) {
      if (WN_operator(wn) != OPR_IF)
        then_model.Add_Statement(wn);
      else
        then_if_cost += Estimate_IF_Cost(wn, inner_do_loop, se_needed,
	                                 invar_table);
    }

    then_model.Evaluate(inner_do_loop, se_needed, invar_table,
                        &then_cost, &fp_regs_used_out, &int_regs_used_out,
                        &tlb_used_out);

    then_cost += then_if_cost;
  }

  if (WN_first(WN_else(wn_if))) {
    REGISTER_MODEL else_model(&LNO_local_pool);
    double else_if_cost = 0.0;

    for (WN *wn = WN_first(WN_else(wn_if)); wn; wn = WN_next(wn)) {
      if (WN_operator(wn) != OPR_IF)
        else_model.Add_Statement(wn);
      else
        else_if_cost += Estimate_IF_Cost(wn, inner_do_loop, se_needed,
	                                 invar_table);
    }

    else_model.Evaluate(inner_do_loop, se_needed, invar_table,
                        &else_cost, &fp_regs_used_out, &int_regs_used_out,
                        &tlb_used_out);
    else_cost += else_if_cost;
  }

  return_cost += (then_cost > else_cost) ? then_cost : else_cost;

  return return_cost;
} // Estimate_IF_Cost()


//-----------------------------------------------------------------------
// NAME: SNL_Inner_Machine_Cost 
// FUNCTION: Return the cost in machine cycles of executing the innermost 
//   loop of the SNL with outermost loop 'wn_outer' containing 'nloops' 
//   loops. The 'plist' contains a list of scalar expanded variables.  The 
//   'parallel_depth' is the depth of the parallel loop in the transformed 
//   nest.  An estimate of the work for one iteration of the innermost loop
//   is returned in 'work_estimate'. 
//-----------------------------------------------------------------------

static double SNL_Inner_Machine_Cost(WN* wn_total_outer, 
				     INT total_nloops, 
				     WN* wn_outer, 
				     INT nloops, 
				     INT parallel_depth, 
				     SX_PLIST* plist, 
				     double* work_estimate,
				     BOOL include_calls)
{
  extern void  Mark_Invar(WN *region, INT num_loops, DOLOOP_STACK *do_stack,
                          HASH_TABLE<WN *,BIT_VECTOR *> *htable, 
                          MEM_POOL *pool, BOOL outer_only);

  REGISTER_MODEL machine_model(&LNO_local_pool); 
  double loop_cycles = 0.0; // estimated cycles for all but IF nodes
  double if_cycles = 0.0; // estimated cycles for IF nodes
  DYN_ARRAY<WN *> if_list(&LNO_local_pool);
  INT fp_regs_used_out = 0; 
  INT int_regs_used_out = 0;
  INT tlb_used_out = 0;
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  WN* wn_first = WN_first(WN_do_body(wn_inner));
  WN* wn = 0;
  for (wn = wn_first; wn != NULL; wn = WN_next(wn)) {
    if (WN_operator(wn) != OPR_IF)
      machine_model.Add_Statement(wn);
    else
      if_list.AddElement(wn);
  }
  WN2INT* se_needed = CXX_NEW(WN2INT(HASH_SIZE, &LNO_local_pool),
    &LNO_local_pool);
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_inner)); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    OPERATOR opr = WN_operator(wn);
    if (opr != OPR_LDID && opr != OPR_STID)
      continue; 
    TYPE_ID type = opr == OPR_STID 
      ? WN_desc(wn) : WN_rtype(wn);
    SYMBOL symb(wn);
    if (plist != NULL) { 
      SX_PITER ii(plist);
      BOOL is_found = FALSE;
      SX_PNODE *n, *found_n = NULL;
      for (n = ii.First(); n != NULL && !is_found; n = ii.Next()) {
	if (n->Symbol() == symb) {
	  is_found = TRUE;
	  SX_PNODE::STATUS status = n->Transformable(outer_depth);
	  if (status != SX_PNODE::SE_NOT_REQD) 
	    found_n = n;
	}
      }
      if (found_n) 
	se_needed->Enter(wn, 1); 
    } 
  }

  DOLOOP_STACK *do_stack = 
    CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
  Build_Doloop_Stack(wn_inner, do_stack);
  HASH_TABLE<WN *,BIT_VECTOR *> invar_table(500, &LNO_local_pool);
  Mark_Invar(WN_do_body(wn_inner), nloops, do_stack, 
             &invar_table, &LNO_local_pool, FALSE);
  
  machine_model.Evaluate(wn_inner, se_needed, &invar_table, &loop_cycles, 
                         &fp_regs_used_out, &int_regs_used_out, &tlb_used_out);

  for (INT i = 0; i < if_list.Elements(); i++)
    if_cycles += Estimate_IF_Cost(if_list[i], wn_inner, se_needed,
                                  &invar_table);
  loop_cycles += if_cycles;

  if (include_calls) { 
    double call_cycles = Machine_Cost_Calls(wn_inner);
    loop_cycles += call_cycles; 
  } 
  *work_estimate = loop_cycles; 
  if (loop_cycles < 0.0) 
    *work_estimate = Estimate_Cycles(wn_inner); 
  if (wn_total_outer != wn_outer) { 
    for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_DO_LOOP) {
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
        *work_estimate *= (double) dli->Est_Num_Iterations; 
	if (Do_Loop_Depth(wn) == Do_Loop_Depth(wn_total_outer) + total_nloops)
	  break;  
      } 
    }
  }

  WN* wn_last = LWN_Get_Parent(wn_total_outer);  
  for (wn = wn_inner; wn != wn_last; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
       DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
       double multiplier = (double) (Do_Loop_Depth(wn) == parallel_depth 
	 ? (dli->Est_Num_Iterations + NOMINAL_PROCS - 1) / NOMINAL_PROCS
	 : dli->Est_Num_Iterations);  
       loop_cycles *= multiplier; 
    } 
  } 
  return loop_cycles; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Machine_Cost 
// FUNCTION: Return the cost in machine cycles of executing the innermost 
//   loops of all of the inner SNLs within the SNL with outermost loop 
//   'wn_outer' containing 'nloops' loops. The 'plist' contains a list of 
//   scalar expanded variables. The 'parallel_depth' is the depth of the 
//   parallel loop in the transformed nest.  An estimate of the work for 
//   one iteration of the innermost loop is returned in 'work_estimate'.
//-----------------------------------------------------------------------

extern double SNL_Machine_Cost(WN* wn_outer, 
			       INT nloops, 
			       INT parallel_depth, 
			       SX_PLIST* plist, 
			       double* work_estimate,
			       BOOL include_calls)
{
  double local_work_estimate = 0.0; 
  double total_work_estimate = 0.0; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner);  
  if (dli_inner->Is_Inner)
    return SNL_Inner_Machine_Cost(wn_outer, nloops, wn_outer, nloops, 
      parallel_depth, plist, work_estimate, include_calls); 

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(wn_outer, TRUE);  

  double total_cycles = 0; 
  *work_estimate = 0.0; 
  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    if (ffi->Get_Type(i) != Inner)
      continue;
    WN* wn_loop = ffi->Get_Wn(i); 
    INT nnloops = ffi->Get_Depth(i); 
    double local_cycles = SNL_Inner_Machine_Cost(wn_outer, nloops, wn_loop, 
      nnloops, parallel_depth, plist, &local_work_estimate, include_calls); 
    total_work_estimate += local_work_estimate; 
    total_cycles += local_cycles; 
  }
  *work_estimate = total_work_estimate; 
  return total_cycles; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Min_Parallel_Overhead_Cost 
// FUNCTION: Returns a lower bound in parallel overhead cycles for executing 
//   the SNL with outermost loop 'wn_outer' of 'nloops' loops, where the 
//   loop at 'parallel_depth' is made parallel. 
//-----------------------------------------------------------------------

extern double SNL_Min_Parallel_Overhead_Cost(WN* wn_outer, 
					     INT nloops, 
					     INT parallel_depth)
{
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT* small_trips = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool); 
  INT i;
  for (i = outer_depth; i < outer_depth + nloops - 1; i++) {
    WN* wn_loop = stack.Bottom_nth(i); 
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop); 
    small_trips[i - outer_depth] = dli_loop->Est_Num_Iterations; 
  } 
  INT snloops = parallel_depth - outer_depth; 
  for (i = 0; i < snloops; i++) {
    for (INT j = i; j < nloops - 1; j++) {  
      if (small_trips[j] < small_trips[j+1]) {
        INT temp = small_trips[j]; 
	small_trips[j] = small_trips[j+1];
	small_trips[j+1] = temp; 
      }
    }
  }
  double total_cost = (double) LNO_Parallel_Overhead; 
  for (i = 0; i < snloops; i++) 
    total_cost *= (double) small_trips[i];    
  return total_cost; 
}

//-----------------------------------------------------------------------
// NAME: Invariant_Reduction
// FUNCTION: Returns TRUE if the array reduction store 'wn_istore' is 
//   invariant to all of the loops inside the parallel nest enclosing 
//   the PAR_STAT, FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL PAR_STAT::Invariant_Reduction(WN* wn_istore)
{
  WN* wn_array = WN_kid1(wn_istore); 
  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    if (av->Too_Messy)
      return FALSE; 
#ifdef KEY //bug 8434
    if (av->Non_Const_Loops() >= _depth)
      return FALSE; 
#endif
    PAR_STAT* ps = NULL; 
    for (ps = this; ps != NULL; ps = ps->_parent) {
      if (WN_opcode(ps->_wn) == OPC_DO_LOOP 
	  && av->Loop_Coeff(Do_Loop_Depth(ps->_wn)) != 0)
        return FALSE; 
      if (ps->_is_parallel)
        break;
    }
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Reduction_List
// FUNCTION: Returns a list of the reductions in the PAR_STAT tree.
//   Each reduction is represented as the WN * to the STID or ISTORE node
//   for the reduction.  The PAR_STAT tree must represent an innermost
//   loop.  rlist must be empty upon entry to this method.
//-----------------------------------------------------------------------

void PAR_STAT::Reduction_List(REDUCTION_LIST *rlist)
{
  REDUCTION_MANAGER* rm = red_manager; 
  if (rm == NULL) 
    return;

  if (WN_opcode(_wn) == OPC_DO_LOOP) { 
    for (PAR_STAT* ps = _first; ps != NULL; ps = ps->_next)
      ps->Reduction_List(rlist);  // recursive call with non-empty rlist is OK
  } else { 
    LWN_ITER* itr = LWN_WALK_TreeIter(_wn); 
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      WN* wn = itr->wn;
      OPERATOR opr = WN_operator(wn); 
      if (opr == OPR_STID && rm->Which_Reduction(wn) != RED_NONE)
        rlist->AddElement(wn);
      else if (opr == OPR_ISTORE && rm->Which_Reduction(wn) != RED_NONE
          && Invariant_Reduction(wn))
        rlist->AddElement(wn);
    }
  } 
  return;
} 


//-----------------------------------------------------------------------
// NAME: Num_Reductions 
// FUNCTION: Returns the number of scalar (and parallel loop invariant 
//   array) reductions in the PAR_STAT tree which must represent an 
//   innermost loop.
//-----------------------------------------------------------------------

INT PAR_STAT::Num_Reductions()
{
  REDUCTION_LIST rlist(&LNO_local_pool);

  Reduction_List(&rlist);
  return rlist.Elements();
} 

//-----------------------------------------------------------------------
// NAME: Reduction_Cost 
// FUNCTION: Returns the number of cycles needed to compute the partial
//   sum loops for the reductions in any parallel loop in the PAR_STAT 
//   tree.
//-----------------------------------------------------------------------

double PAR_STAT::Reduction_Cost()
{
  REDUCTION_MANAGER* rm = red_manager; 
  if (rm == NULL) 
    return 0.0; 

  double red_cost = 0.0; 
    // there's a problem with this test: if there's a serial DO loop inside
    // a parallel one, we never find any parallel reductions--DRK
  if (Is_Inner_Loop() && Is_Parallel_Enclosed_Loop()) {
    INT red_count = Num_Reductions(); 
    MHD_LEVEL* Cur_Mhd = NULL; 
    for (INT i = MHD_MAX_LEVELS - 1; i >= 0; i--) {
      Cur_Mhd = &Mhd.L[i];
      if (Cur_Mhd->Valid())
        break;
    }
    if (red_count > 0) { 
      double dirty_penalty = (double) Cur_Mhd->Dirty_Miss_Penalty;
      double dirty_bytes = (double) red_count; 
      dirty_bytes *= (double) NOMINAL_PROCS;  
      PAR_STAT* ps = 0;
      for (ps = this; ps != NULL; ps = ps->_next) 
	if (WN_opcode(ps->_wn) == OPC_DO_LOOP && ps->_is_parallel)
	  break;
      for (ps = _parent; ps != NULL; ps = ps->_next) 
	dirty_bytes *= ps->_count; 
      red_cost += dirty_bytes * dirty_penalty; 
    } 
  }
    
  if (_first != NULL)
    red_cost += _first->Reduction_Cost(); 
  if (_next != NULL)
    red_cost += _next->Reduction_Cost(); 
  return red_cost; 
}

//-----------------------------------------------------------------------
// NAME: Sx_Depth
// FUNCTION: Return the depth of the subnest over which we must apply 
//   scalar expansion for permutation and parallelism of the SNL whose 
//   outermost loop is 'wn_outer' and which has 'nloops', to which we 
//   will apply a 'permutation' of 'nloops' and which will be split at
//   depth 'split_depth' for parallelism.  Information about scalar 
//   expanded variables is supplied in 'plist'. 
//-----------------------------------------------------------------------

static INT Sx_Depth(WN* wn_outer,
		    INT permutation[], 
		    INT nloops, 
		    INT split_depth, 
	            SX_PLIST* plist)
{
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT i;
  for (i = 0; i < nloops; i++) 
    if (permutation[i] != i) 
      break; 
  INT sx_perm_depth = outer_depth + i; 
  INT sx_par_depth = Split_Sx_Depth(wn_outer, nloops, plist, split_depth); 
  INT sx_depth = sx_perm_depth;
  if (sx_par_depth != -1 && sx_par_depth < sx_depth)
    sx_depth = sx_par_depth; 
  return sx_depth; 
} 
				 
//-----------------------------------------------------------------------
// NAME: Enter_Scalar_Expandable_Refs
// FUNCTION: For the innermost loop of the SNL with outermost loop 'wn_outer'
//   innermost loop 'wn_inner', which consists of 'nloops' loops, and to 
//   whioch will be applied the 'permutation' of length 'nloops', update the 
//   'arl' to include references to the scalar expandable variables listed 
//   in 'plist'.  The nest is split at 'split_depth' to remove LCDs in 
//   imperfectly nested code for loops above the loop at 'split_depth'.
//-----------------------------------------------------------------------

static void Enter_Scalar_Expandable_Refs(WN* wn_outer, 
					 WN* wn_inner, 
					 INT permutation[], 
					 INT nloops, 
					 INT split_depth, 
					 ARRAY_REF* arl, 
					 SX_PLIST* plist) 
{
  INT outer_depth = Do_Loop_Depth(wn_outer);
  INT sx_depth = Sx_Depth(wn_outer, permutation, nloops, split_depth, plist);
  if (sx_depth >= outer_depth + nloops) 
    return; 
  BOOL* can_be_inner = CXX_NEW_ARRAY(BOOL, nloops, &LNO_local_pool); 
  for (INT i = 0; i < nloops; i++) 
    can_be_inner[i] = TRUE; 
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_inner)); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    OPERATOR opr = WN_operator(wn);
    if (opr != OPR_LDID && opr != OPR_STID)
      continue; 
    TYPE_ID type = opr == OPR_STID 
      ? WN_desc(wn) : WN_rtype(wn);
    SYMBOL symb(wn);
    SX_PITER ii(plist);
    BOOL is_found = FALSE;
    SX_PNODE *n, *found_n = NULL;
    for (n = ii.First(); n != NULL && !is_found; n = ii.Next()) {
      if (n->Symbol() == symb) {
        is_found = TRUE;
        SX_PNODE::STATUS status = n->Transformable(sx_depth, permutation, 
	  nloops);
        if (status == SX_PNODE::SE_REQD) 
          found_n = n;
      }
    }
    if (found_n) 
      arl->Enter_Scalar_Expand(wn, found_n, can_be_inner, nloops); 
  }
}

//-----------------------------------------------------------------------
// NAME: Index_Variable_Ldid
// FUNCTION: If 'wn_node' is an LDID of an index variable of one of the 
//   'nloops' DO loops enclosing 'wn_node', return the ordinal placement 
//   of that DO loop (e.g. 1 is innermost, 2 is next innermost, etc.). 
//   Otherwise, return 0.
//-----------------------------------------------------------------------

static INT Index_Variable_Ldid(WN* wn_node, 
			       INT nloops)
{
  if (WN_operator(wn_node) != OPR_LDID)
    return FALSE; 

  INT loop_count = 0; 
  for (WN* wn = wn_node; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_operator(wn) == OPR_DO_LOOP) {
      loop_count++; 
      if (SYMBOL(WN_index(wn)) == SYMBOL(wn_node))
	return loop_count; 
    } 
    if (loop_count == nloops)
      break; 
  } 
  return 0; 
} 

//-----------------------------------------------------------------------
// NAME: Loop_Index_Count
// FUNCTION: Set the bits in 'bv' corresponding to the 'nloops' innermost
//   DO loops whose index variables are referenced in the expression tree
//   rooted at 'wn_tree'. 
// NOTE: In 'bv', the innermost loop enclosing 'wn_tree' represented by 
//   bit position 0, the next innermost loop by bit position 1, and so on.
//-----------------------------------------------------------------------

static void Loop_Index_Count_Traverse(WN* wn_tree, 
				      INT nloops,
				      BIT_VECTOR* bv)
{ 
  INT loop_position = Index_Variable_Ldid(wn_tree, nloops);
  if (loop_position > 0)
    bv->Set(loop_position - 1);     

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Loop_Index_Count_Traverse(wn, nloops, bv);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Loop_Index_Count_Traverse(WN_kid(wn_tree, i), nloops, bv);
  } 
} 

//-----------------------------------------------------------------------
// NAME: Loop_Index_Count
// FUNCTION: Return the number of loops the 'nloops' innermost loops 
//   which are indexed by the expression tree rooted at 'wn_tree'.
//-----------------------------------------------------------------------

static INT Loop_Index_Count(WN* wn_tree, 
			    INT nloops)
{
  BIT_VECTOR bv(nloops, &LNO_local_pool);
  Loop_Index_Count_Traverse(wn_tree, nloops, &bv);
  return bv.Pop_Count();
} 

//-----------------------------------------------------------------------
// NAME: SNL_Bad_Array_Footprints
// FUNCTION: For the loop 'wn_loop' which is being analyzed inside an 
//   SNL of 'nloops' loops, return in 'bad_read_bytes' an estimate of 
//   the number of bytes due to bad reads and in 'bad_write_bytes' an 
//   estimate of the number of bytes due to bad writes. 
//-----------------------------------------------------------------------

static void SNL_Bad_Array_Footprints(WN* wn_loop, 
				     INT nloops, 
			             INT64 trip_product, 
			             double* bad_read_bytes, 
			             double* bad_write_bytes)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (Is_Bad_Array(wn, nloops)) {
      BOOL is_load = OPCODE_is_load(WN_opcode(wn)); 
      WN* wn_array = is_load ? WN_kid0(wn) : WN_kid1(wn); 
      double bad_bytes = (double) WN_element_size(wn_array); 
      if (bad_bytes < 0)
	bad_bytes = -bad_bytes; 
      bad_bytes *= trip_product;  
      if (is_load)
        *bad_read_bytes += bad_bytes;  
      else 
	*bad_write_bytes += bad_bytes; 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_Inner_Cache_Cost
// FUNCTION: Return the cache miss cost (in cycles) for the innermost SNL 
//   with outermost loop 'wn_outer', innermost loop 'wn_inner', and which 
//   contains 'nloops' loops, to which we are applying the 'permutation' 
//   of length 'nloops' and whose loop (after permutation) at depth 
//   'parallel_depth' will be made parallel.  The list of expandable 
//   scalars is given by 'plist'.  The nest is split at 'split_depth' 
//   to remove LCDs in imperfectly nested code for loops above the loop 
//   at 'split_depth'.
// NOTE: We are ignoring all but the nodes in the innermost loop at 
//   this point because Compute_Footprint() only accounts for these. 
//-----------------------------------------------------------------------

static double SNL_Inner_Cache_Cost(WN* wn_outer,
				   WN* wn_inner, 
                                   INT permutation[],
                                   INT nloops,
                                   INT parallel_depth,
				   INT split_depth, 
				   SX_PLIST* plist,
				   double *est_num_iters)
{
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0; 
  INT outer_depth = Do_Loop_Depth(wn_outer);
  DOLOOP_STACK loop_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &loop_stack);
  ARRAY_REF* arl =
    CXX_NEW(ARRAY_REF(wn_inner, nloops, &LNO_local_pool,NULL), &LNO_local_pool);
  Enter_Scalar_Expandable_Refs(wn_outer, wn_inner, permutation, nloops, 
    split_depth, arl, plist); 
  INT depth = outer_depth + nloops - 1;
  INT* loops = CXX_NEW_ARRAY(INT, depth + 1, &LNO_local_pool);
  INT* iters = CXX_NEW_ARRAY(INT, depth + 1, &LNO_local_pool);
  INT* arl_stripsz = CXX_NEW_ARRAY(INT, depth + 1, &LNO_local_pool);
  mINT64* est_iters = CXX_NEW_ARRAY(mINT64, depth + 1, &LNO_local_pool);
  mINT64* max_iters = CXX_NEW_ARRAY(mINT64, depth + 1, &LNO_local_pool);
  INT* unrolls = CXX_NEW_ARRAY(INT, depth + 1, &LNO_local_pool);
  INT* permute_order = CXX_NEW_ARRAY(INT, depth + 1, &LNO_local_pool);
  INT snloops = nloops - (parallel_depth - outer_depth); 
  INT i;
  for (i = 0; i <= depth; i++) {
    WN* wn_loop = loop_stack.Bottom_nth(i);
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
    arl_stripsz[i] = 1;
    extern INT64 Get_Good_Num_Iters(DO_LOOP_INFO *dli);
    est_iters[i] = Get_Good_Num_Iters(dli_loop);
    max_iters[i] = UNBOUNDED_ITERS;
    unrolls[i] = 1;
    permute_order[i] = i < outer_depth 
      ? i : outer_depth + permutation[i - outer_depth];
  }
  est_iters[outer_depth + permutation[parallel_depth - outer_depth]] =
    (est_iters[outer_depth + permutation[parallel_depth - outer_depth]] 
    + NOMINAL_PROCS - 1) / NOMINAL_PROCS;
  for (i = 0; i <= depth; i++)
    if (est_iters[i] <= 0)
      est_iters[i] = 1; 
  for (i = 0; i < snloops; i++) 
    loops[i] = outer_depth + permutation[i + parallel_depth - outer_depth];
  for (i = 0; i < nloops; i++) 
    iters[i] = est_iters[outer_depth + permutation[i]]; 
  INT stripdepth = depth + 1;
  INT v_first = -1;
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer);
  INT64 outersz = iters[parallel_depth - outer_depth];
  BOOL using_tlb = FALSE;
  INT middle_loop_no = loops[0];
  double clean_cost = 0.0;
  double dirty_cost = 0.0;
  double bad_read_bytes = 0; 
  double bad_write_bytes = 0; 
  INT64 trip_product = 1; 
  for (i = parallel_depth - outer_depth; i < nloops; i++)
    trip_product *= iters[i]; 
  if (arl->Num_Bad() > 0) 
    SNL_Bad_Array_Footprints(wn_inner, nloops, trip_product, 
      &bad_read_bytes, &bad_write_bytes);  
  for (i = 0; i < MHD_MAX_LEVELS; i++) {
    MHD_LEVEL* Cur_Mhd = &Mhd.L[i];
    if (!Cur_Mhd->Valid())
      continue;
    Set_Cache_Model_Statics(i); 
    INT line_size = Cur_Mhd->Line_Size; 
    double clean_penalty = double(Cur_Mhd->Clean_Miss_Penalty)/line_size; 
    double dirty_penalty = double(Cur_Mhd->Dirty_Miss_Penalty)/line_size; 
    COMPUTE_FOOTPRINT_RVAL rval = Compute_Footprint(arl, snloops, loops,
      arl_stripsz, est_iters, max_iters, unrolls, depth, stripdepth,
      permute_order, v_first, outersz, using_tlb, middle_loop_no);
    if (parallel_debug_level >= 3) { 
      rval.Print(stdout); 
      fprintf(stdout, "Evaluating: "); 
      for (INT j = 0; j < snloops - 1; j++) { 
	fprintf(stdout, "v%d=%d", j, 
          iters[parallel_depth - outer_depth + 1 + j]);
	if ( j < snloops - 2)
	  fprintf(stdout, " ");
      }
      fprintf(stdout, "\n"); 
    } 
    INT offset = parallel_depth - outer_depth + 1; 
    double read_bytes = rval.RFormula == NULL
      ? 0.0 : rval.RFormula->Eval(snloops - 1, &iters[offset]);
    read_bytes += bad_read_bytes; 
    double write_bytes = rval.WFormula == NULL
      ? 0.0 : rval.WFormula->Eval(snloops - 1, &iters[offset]);
    write_bytes += bad_write_bytes; 
    clean_cost += read_bytes * clean_penalty;
    dirty_cost += write_bytes * dirty_penalty;
 } 
  double total_cost = clean_cost + dirty_cost;
  for (i = 0; i < parallel_depth - outer_depth; i++) 
    total_cost *= iters[i];
  *est_num_iters = 1.0;
  for (i = 0; i < nloops; i++) 
    *est_num_iters *= iters[i];
  *est_num_iters *= NOMINAL_PROCS;
  return total_cost;
}

//-----------------------------------------------------------------------
// NAME: SNL_Cache_Cost
// FUNCTION: Return the cache miss cost (in cycles) for the SNL with 
//   outermost loop 'wn_outer' containing 'nloops' loops, to which we are 
//   applying the 'permutation' of length 'nloops' and whose loop (after
//   permutation) at depth 'parallel_depth' will be made parallel.
//   The list of expandable scalars is given by 'plist'.  The nest is 
//   split at 'split_depth' to remove LCDs in imperfectly nested code 
//   for loops above the loop at 'split_depth'.
// NOTE: For non-innermost SNLs we are pretending that we can take the 
//   contained inner SNLs, distribute them out, and then sum the cache 
//   costs of the resulting inner SNLs.  This is a gross oversimplifica-
//   tion, but is all right for now, since we don't have time to rewrite 
//   Compute_Footprint() before the release of Mongoose 7.2. 
//-----------------------------------------------------------------------

extern double SNL_Cache_Cost(WN* wn_outer,
                             INT permutation[],
                             INT nloops,
                             INT parallel_depth, 
                             INT split_depth, 
                             SX_PLIST* plist,
		             double *est_num_iters)
{
  double _est_num_iters;
  *est_num_iters = 0.0;

  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner); 
  if (dli_inner->Is_Inner) 
    return SNL_Inner_Cache_Cost(wn_outer, wn_inner, permutation, nloops, 
      parallel_depth, split_depth, plist, est_num_iters);

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(wn_outer, TRUE);  

  double total_cycles = 0; 
  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    if (ffi->Get_Type(i) != Inner)
      continue;
    WN* wn_loop = ffi->Get_Wn(i); 
    INT nnloops = ffi->Get_Depth(i); 
    INT depth_diff = Do_Loop_Depth(wn_loop) - Do_Loop_Depth(wn_outer);
    INT xnloops = nnloops + depth_diff; 
    INT* xpermutation = CXX_NEW_ARRAY(INT, xnloops, &LNO_local_pool);  
    INT j;
    for (j = 0; j < nloops; j++) 
      xpermutation[j] = permutation[j];  
    for (j = nloops; j < xnloops; j++)
      xpermutation[j] = j; 
    WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_loop, nnloops); 
    double local_cycles = SNL_Inner_Cache_Cost(wn_outer, wn_inner, 
      xpermutation, xnloops, parallel_depth, split_depth, plist, 
      &_est_num_iters);
    *est_num_iters += _est_num_iters;
    total_cycles += local_cycles; 
    CXX_DELETE_ARRAY(xpermutation, &LNO_local_pool); 
  }
  return total_cycles; 
}

//-----------------------------------------------------------------------
// NAME: Num_Refs
// FUNCTION: Returns the number of array references in the PAR_STAT tree
//   which must represent an innermost loop. 
//-----------------------------------------------------------------------

INT PAR_STAT::Num_Refs()
{
  INT num_refs = 0; 
  if (WN_opcode(_wn) == OPC_DO_LOOP) {
    for (PAR_STAT* ps = _first; ps != NULL; ps = ps->_next)
      num_refs += ps->Num_Refs(); 
  } else {
    LWN_ITER* itr = LWN_WALK_TreeIter(_wn);
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      OPERATOR opr = WN_operator(itr->wn); 
      if (opr == OPR_ILOAD || opr == OPR_ISTORE)
	num_refs++; 
    } 
  }
  return num_refs; 
} 

//-----------------------------------------------------------------------
// NAME: Loop_Overhead_Cost
// FUNCTION: Returns the number of loop overhead cycles represented by 
//   the PAR_STAT tree. 
// NOTE: In the first version of this function, the loop overhead had 
//   two components: (1) A software pipelining overhead and (2) a dis-
//   tribution overhead. 
//      The software pipelining overhead was based on the cost of 
//   performing windup and winddown of the innermost loop.  The cost 
//   for the nest was given by the formula: 
//      (product of trip counts of all loops except innermost) 
//        * (Mhd.Loop_Overhead_Base + Mhd.Loop_Overhead_Memref * num_refs)
//   where "num_refs" is the number of references in the innermost loop.
//   Typically, Mhd.Loop_Overhead_Base == 18 and Mhd.Loop_Overhead_Memref
//   == 1.  Since this cost is paid once for each execution of the inner-
//   most loop (NOTE: NOT each iteration of the innermost loop), this 
//   component of the cost favored placing loops with large trip counts 
//   as innermost.
//     A problem with using this component of the cost showed up in PV 
//   615485.  Since this cost favors large trip count loops being placed 
//   innermost, if the trip counts of the loops in a nest are different, 
//   a combination like (0-P,1,2) may have a different cost then a com-
//   bination like (0-P,2,1).  This violates an assumption of our model 
//   that the arrangement of loops inside the parallel loop doesn't 
//   matter.  It actually doesn't matter for machine cost (which is in-
//   dependent of loop order) or cache cost (which is set up not to de-
//   pend on the order of the loops inside the parallel loop) or the 
//   parallel overhead and reduction costs (which depend on the parallel 
//   loop and those loops outside it).  
//      This assumption that the arrangement of the loops inside the 
//   parallel loop is an important one because we currently don't test, 
//   for example, both (0-P,1,2) and (0-P,2,1).  If these were to 
//   evaluate to different costs, we might end only evaluating the 
//   higher cost alternative and therefore not choose the best order.
//     Because of this, we've decided to drop the software pipelining
//   cost from the model.  If we decide that it is important to add it 
//   back in, we'll have to evaluate all of the permutation orders. 
//   To save compile time, it is possible to do this in a hierarchical 
//   manner: only the loop overhead cost will need to be evaluated 
//   separately for permutations in the same permutation class.  (By 
//   permutation class, I mean two permutations with the same entries
//   up to and including the parallel loop, which must be in the same
//   position in both cases.  For example, (1-P,0,2) and (1-P,2,0) are
//   in the same permutation class.) 
//      A different problem arises with the distribution overhead.  The
//   idea here is to charge LOOP_CYCLES_PER_ITER cycles for each loop 
//   iteration that is excuted.  It is clear that two permutations in
//   the same permutation class can have different distribution costs.
//   For example, (0-P,1,2) and (0-P,2,1) will have different costs if
//   there is a statement between loops 1 and 2 in the original nest. 
//      However, in this case, there is an choice among the permutations 
//   in the same permutation class that will guarentee the lowest dis-
//   tribution cost for that nest.  This is the permutation whose loops
//   inside the parallel loops have their indices in the lowest possible
//   lexicographic order.  For example, for the permutation class 
//   (2,0-P,x,y,z), the best choice is (2,0-P,1,3,4).  We therefore force
//   this to be the choice that we evaluate for the cost of the permu-
//   tation class.   
//      One other issue worth considering: is it true that if this choice
//   is not legal then no other choice in the same permutation class is 
//   legal?  If not, we may need to re-evaluate whether we can still get 
//   by without evaluating all of the choices. 
//-----------------------------------------------------------------------

double PAR_STAT::Loop_Overhead_Cost()
{
  double loop_cycles = 0.0;
  if (WN_operator(_wn) == OPR_DO_LOOP) {
    double multiplier = LOOP_CYCLES_PER_ITER;
    for (PAR_STAT* ps = this; ps != NULL; ps = ps->_parent) {
      if (WN_operator(ps->_wn) == OPR_DO_LOOP) {
        INT count = ps->_is_parallel
          ? (ps->_count + NOMINAL_PROCS - 1)/NOMINAL_PROCS : ps->_count;
        multiplier *= (double) count;
      }
    }
    loop_cycles += multiplier;
  }
  if (_first != NULL)
    loop_cycles += _first->Loop_Overhead_Cost();
  if (_next != NULL)
    loop_cycles += _next->Loop_Overhead_Cost();
  return loop_cycles;
} 

//-----------------------------------------------------------------------
// NAME: Parallel_Overhead_Cost
// FUNCTION: Returns the number of parallel overhead cycles represented by 
//   the PAR_STAT tree. 
//-----------------------------------------------------------------------

double PAR_STAT::Parallel_Overhead_Cost()
{
  double parallel_cycles = 0;
  if (WN_opcode(_wn) == OPC_DO_LOOP && _is_parallel) { 
      // need to encapsulate this in a class--DRK
#ifdef KEY
    double multiplier = (double) (LNO_Parallel_Overhead +
				NOMINAL_PROCS * LNO_Parallel_per_proc_overhead);
#else
    double multiplier = (double) (LNO_Parallel_Overhead +
                                  NOMINAL_PROCS * 123);
#endif
      // same test as in PAR_STAT::Reduction_Cost(), has same bug--DRK
    if (Is_Inner_Loop() && Is_Parallel_Enclosed_Loop()) {
        // add estimate of cost of combining partial results
      REDUCTION_LIST rlist(&LNO_local_pool);
      BOOL dummy;

      Reduction_List(&rlist);
      if (rlist.Elements() > 0)
        multiplier += MP_Reduction_Combine_Cycles(&rlist, &dummy);
    }
    BOOL first_loop=FALSE;
    for (PAR_STAT* ps = _parent; ps != NULL; ps = ps->_parent)  
      if (WN_opcode(_wn) == OPC_DO_LOOP)
	if (first_loop)
	  first_loop=FALSE;
	else
	  multiplier *= (double) ps->_count; 
    parallel_cycles += multiplier;    
  } 
  if (_first != NULL)
    parallel_cycles += _first->Parallel_Overhead_Cost();
  if (_next != NULL)
    parallel_cycles += _next->Parallel_Overhead_Cost();
  return parallel_cycles;
}

//-----------------------------------------------------------------------
// NAME: Cycle_Count 
// FUNCTION: Returns the number of machine cycles represented by the 
//   PAR_STAT tree.  This models an SNL with outermost loop 'wn_outer' 
//   containing 'nloops' loops.  We apply a 'permutation' of length 
//   'nloops' and parallelize the loop which after transformation is at 
//   'parallel_depth'.  The list of expandable scalars is given by 'plist'.
//   We split the loop nest at 'split_depth' to rid ourselves of LCDs
//   in imperfect code of the nest above the loop at 'split_depth'.
//   The number of machine cycles (which is independent of 'permutation'
//   and so computed elsewhere), is given by 'machine_cycles'.
//   We return in *cache_cycles_per_iter an estimate of the number of cycles
//   spent per iteration due to cache misses; it is returned up the call chain
//   to the constructor for PARALLEL_INFO() to avoid redundant
//   computation (we should move this into DO_LOOP_INFO eventually--DRK).
//   'is_doacross' should be set to TRUE if this is to model the doall
//   part of the cost for a doacross loop with synchronization.
//-----------------------------------------------------------------------

double PAR_STAT::Cycle_Count(WN* wn_outer,
                             INT permutation[],
                             INT nloops,
                             INT parallel_depth,
			     SX_PLIST* plist,
			     INT split_depth, 
			     double machine_cycles,
			     double *cache_cycles_per_iter,
			     BOOL is_doacross)
{
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0; 
  double reduction_cycles = Reduction_Cost();
  double loop_cycles = Loop_Overhead_Cost(); 
  double parallel_cycles = Parallel_Overhead_Cost();
  double est_num_iters;
  double cache_cycles = SNL_Cache_Cost(wn_outer, permutation, nloops, 
    parallel_depth, split_depth, plist, &est_num_iters); 
  Is_True(cache_cycles >= 0.0, 
    ("PAR_STAT::Cycle_Count: Unexpected negative cache cycle count"));
  double total_cycles = machine_cycles + reduction_cycles + parallel_cycles 
    + loop_cycles + cache_cycles; 
  if (parallel_debug_level >= 2) {
    INT outer_depth = Do_Loop_Depth(wn_outer);
    fprintf(stdout, "Permutation = ("); 
    if (!is_doacross)
      for (INT i = 0; i < nloops; i++) {
        fprintf(stdout, "%d%s", permutation[i], 
	  i == parallel_depth - outer_depth ? "-P" : ""); 
        if (i < nloops - 1)
          fprintf(stdout, ","); 
      }
    else
      for (INT i = 0; i < nloops; i++) {
        fprintf(stdout, "%d%s", permutation[i], 
	  parallel_depth == i ? "-X" : ""); 
        if (i < nloops - 1)
          fprintf(stdout, ","); 
      }
    fprintf(stdout, ")\n"); 
    fprintf(stdout, "  Machine cycles =           %13.2f\n", machine_cycles); 
    fprintf(stdout, "  Reduction cycles =         %13.2f\n", reduction_cycles); 
    fprintf(stdout, "  Loop Overhead cycles =     %13.2f\n", loop_cycles); 
    fprintf(stdout, "  Parallel Overhead cycles = %13.2f\n", parallel_cycles); 
    fprintf(stdout, "  Cache cycles =             %13.2f\n", cache_cycles); 
    fprintf(stdout, " *Total cycles =             %13.2f\n", total_cycles); 
  }
    // cache_cycles is per-processor, for executing that proc's portion
    // of the SNL; convert it to per-iteration for serial execution of SNL
  _num_estimated_iters = est_num_iters;
  *cache_cycles_per_iter = (cache_cycles * NOMINAL_PROCS) / est_num_iters;
  return total_cycles; 
}

//-----------------------------------------------------------------------
// NAME: Sanity_Check_Node 
// FUNCTION: Perform sanity checks on the PAR_STAT node.  Return the number 
//   of errors encountered.  Print the errors to the file 'fp'. 
//-----------------------------------------------------------------------

#define PS_ASSERT(a, b) \
  ((a) ? 0 : (fprintf b, fprintf(fp, "\n"), error_count++)) 

INT PAR_STAT::Sanity_Check_Node(FILE* fp)
{
  INT error_count = 0; 
  PAR_STAT* ps_root = 0;
  for (ps_root = this; ps_root->_parent != NULL; 
    ps_root = ps_root->_parent); 
  for (; ps_root->_prev != NULL; ps_root = ps_root->_prev); 
  PS_ASSERT(_next == NULL || _next->_prev == this, 
    (fp, "PAR_STAT: Bad _next pointer 0x%p", this));  
  PS_ASSERT(_prev == NULL || _prev->_next == this, 
    (fp, "PAR_STAT: Bad _prev pointer 0x%p", this));
  if (_parent != NULL) {
    PAR_STAT* ps = 0;
    for (ps = _parent->_first; ps != NULL; ps = ps->_next) 
      if (ps == this) 
        break; 
    PS_ASSERT(ps != NULL, (fp, "PAR_STAT: Bad _parent pointer 0x%p", this));
  } 
  if (_first != NULL) { 
    PS_ASSERT(_last != NULL, 
      (fp, "PAR_STAT: _last not consistent with _first 0x%p", this)); 
    PAR_STAT* pss = NULL; 
    for (PAR_STAT* ps = _first; ps != NULL; pss = ps, ps = ps->_next); 
    PS_ASSERT(pss == _last, 
      (fp, "PAR_STAT: _last not consistent with actual _last 0x%p", this)); 
  } 
  if (_last != NULL) 
    PS_ASSERT(_first != NULL, 
      (fp, "PAR_STAT: _last not consistent with _first 0x%p", this)); 
  PS_ASSERT(WN_opcode(_wn) == OPC_DO_LOOP || _first == NULL && _last == NULL,
    (fp, "PAR_STAT: STATEMENT is not leaf node 0x%p", this));
  INT loop_count = 0; 
  for (PAR_STAT* ps = this; ps != NULL; ps = ps->_parent)
    if (WN_opcode(ps->_wn) == OPC_DO_LOOP)
      loop_count++; 
  loop_count += ps_root->_depth; 
  INT depth = (WN_opcode(_wn) == OPC_DO_LOOP) ? loop_count - 1: loop_count; 
  PS_ASSERT(_depth == depth, (fp, "PAR_STAT: Improper depth 0x%p", this)); 
  PS_ASSERT(WN_opcode(_wn) == OPC_DO_LOOP || !_is_parallel, 
    (fp, "PAR_STAT: _is_parallel set on STATEMENT 0x%p", this)); 
  PS_ASSERT(WN_opcode(_wn) != OPC_DO_LOOP || _count > 0, 
    (fp, "PAR_STAT: _count <= 0 on DO LOOP 0x%p", this));
  PS_ASSERT(_id <= id_count, (fp, "PAR_STAT: Improper _id value 0x%p", this)); 
  PS_ASSERT(!_is_cloned || ps_root->Find(_wn, TRUE) != NULL, 
    (fp, "PAR_STAT: _is_cloned is set, but could not find original 0x%p", 
    this));
  PS_ASSERT(WN_opcode(_wn) == OPC_DO_LOOP || !_is_cloned, 
    (fp, "PAR_STAT: _is_cloned set on STATEMENT 0x%p", this));
  return error_count; 
}

//-----------------------------------------------------------------------
// NAME: Sanity_Check
// FUNCTION: Perform sanity checks on the PAR_STAT tree.  Return the number 
//   of errors encountered.  Print the errors to the file 'fp'. 
//-----------------------------------------------------------------------

INT PAR_STAT::Sanity_Check(FILE* fp)
{
  INT error_count = Sanity_Check_Node(fp); 
  if (_first != NULL)
    error_count += _first->Sanity_Check(fp); 
  if (_next != NULL)
    error_count += _next->Sanity_Check(fp); 
  return error_count; 
}

//-----------------------------------------------------------------------
// NAME: Print 
// FUNCTION: Print a user readable representation of the PAR_STAT tree to
//   'fp'.  Start printing with an indentation of 'ident_count' spaces. 
//-----------------------------------------------------------------------

void PAR_STAT::Print(FILE* fp, 
		     INT indent_count)
{
  if (this == NULL)
    fprintf(fp, "<NULL>\n"); 
  for (INT i = 0; i < indent_count; i++) 
    fprintf(fp, " "); 
  if (WN_opcode(_wn) == OPC_DO_LOOP) 
    fprintf(fp, "%s %d [%d]%s (0x%p) [%d]\n", _is_parallel ? "PR" : "DO", 
      _depth, _id, _is_cloned ? "+" : "", _wn, _count); 
  else  
    fprintf(fp, "ST %d (%d) (0x%p) [%d]\n", _depth, _id, _wn, _count);  
  if (_first != NULL) 
    _first->Print(fp, indent_count + 2); 
  if (_next != NULL) 
    _next->Print(fp, indent_count); 
}
