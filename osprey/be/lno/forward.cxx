/*
  Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.

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

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.forward.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>

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
#include "move.h"
#include "dead.h" 
#include "targ_sim.h"
#include "ipl_lno_util.h" 

static ARRAY_DIRECTED_GRAPH16* dg; 
static DU_MANAGER* du; 
static REDUCTION_MANAGER* rm; 

const INT FS_LOAD_AND_LEAF_LIMIT = 1; 
const INT FS_ARRAY_EXP_LIMIT = 80; 
const INT FS_NODE_HASH_SIZE = 247;
const INT FS_SIBLING_HASH_SIZE = 247;
const INT FS_MAX_STRING_LENGTH = 200;

static WN *Find_MP(WN *wn);

//-----------------------------------------------------------------------
// NAME: Count_Loads 
// FUNCTION: Returns the number of loads in the expression rooted at 'wn'. 
//-----------------------------------------------------------------------

static INT Count_Loads_And_Leafs(WN* wn) 
{
  INT load_count = 0; 
  if (OPCODE_is_load(WN_opcode(wn)) || WN_kid_count(wn) == 0)
    return 1; 
 
  for (INT i = 0; i < WN_kid_count(wn); i++)  
    load_count += Count_Loads_And_Leafs(WN_kid(wn, i)); 
  return load_count; 
} 

//-----------------------------------------------------------------------
// NAME: Fix_Deps_For_Load_Or_Store 
// FUNCTION: Fix the dependences for the load or store 'wn_copy' by making 
//   them equal to those of 'wn_orig'. 
// NOTE: 'loop_ls' stores the lexical positions of all of the nodes 
//   in the original outermost do loop for which we are substituting 
//   scalars. 
// NOTE: 'position' is the position of the copy in the loop. 
//-----------------------------------------------------------------------

static void Fix_Deps_For_Load_Or_Store(WN* wn_orig, 
				       WN* wn_copy, 
				       LS_IN_LOOP* loop_ls,
				       INT position)
{
  dg->Add_Vertex(wn_copy);
  EINDEX16 e = 0;
  HASH_TABLE<WN*,INT> old_nodes(MIN(dg->Get_Edge_Count(), 512), 
    &LNO_local_pool); 
  DOLOOP_STACK source_stack(&LNO_local_pool);  
  VINDEX16 v_orig = dg->Get_Vertex(wn_orig);  
  DOLOOP_STACK copy_stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_copy, &copy_stack);
  for (e = dg->Get_In_Edge(v_orig); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e)); 
    old_nodes.Enter(wn_source, 1); 
    Build_Doloop_Stack(wn_source, &source_stack);
    if (!dg->Add_Edge(wn_source, &source_stack, wn_copy, 
      &copy_stack, loop_ls->In(wn_source) < position)) { 
      LNO_Erase_Dg_From_Here_In(wn_copy, dg); 
      return; 
    } 
    source_stack.Clear(); 
  }
  DOLOOP_STACK sink_stack(&LNO_local_pool);  
  for (e = dg->Get_Out_Edge(v_orig); e != 0; e = dg->Get_Next_Out_Edge(e)) { 
    WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e)); 
    if (old_nodes.Find(wn_sink) != 0)
      continue; 
    Build_Doloop_Stack(wn_sink, &sink_stack); 
    if (!dg->Add_Edge(wn_copy, &copy_stack, wn_sink,  
      &sink_stack, position < loop_ls->In(wn_sink))) { 
      LNO_Erase_Dg_From_Here_In(wn_copy, dg); 
      return; 
    } 
    sink_stack.Clear(); 
  }
}

//-----------------------------------------------------------------------
// NAME: Fix_Access_Arrays_In_Copy_Block
// FUNCTION: Ensure that valid access arrays are built for 'wn_tree' and 
//   all of its ancestors.
//-----------------------------------------------------------------------

static void Fix_Access_Arrays_In_Copy_Block(WN* wn_tree) 
{
  if (WN_operator(wn_tree) == OPR_ARRAY) {
    DOLOOP_STACK copy_stack(&LNO_local_pool);
    Build_Doloop_Stack(wn_tree, &copy_stack);
    LNO_Build_Access_Array(wn_tree, &copy_stack, &LNO_default_pool);
  }
  for (INT i = 0; i < WN_kid_count(wn_tree); i++)
    Fix_Access_Arrays_In_Copy_Block(WN_kid(wn_tree, i));
} 
 
//-----------------------------------------------------------------------
// NAME: Fix_Deps_In_Copy_Block 
// FUNCTION: For the load 'wn_copy' which was cloned from 'wn_orig', 
//   and which replaces the scalar load in the given lexical 'position',
//   create an appropriate access vector and dependence arcs.  
// NOTE: 'loop_ls' stores the lexical positions of all of the nodes 
//   in the original outermost do loop for which we are substituting 
//   scalars. 
//-----------------------------------------------------------------------

static void Fix_Deps_In_Copy_Block(LS_IN_LOOP* loop_ls,  
				   WN* wn_orig, 
				   WN* wn_copy, 
				   INT position) 
{ 
  if (OPCODE_is_load(WN_opcode(wn_orig)) && dg->Get_Vertex(wn_orig)) 
    Fix_Deps_For_Load_Or_Store(wn_orig, wn_copy, loop_ls, position);
  for (INT i = 0; i < WN_kid_count(wn_orig); i++)  
    Fix_Deps_In_Copy_Block(loop_ls, WN_kid(wn_orig, i), WN_kid(wn_copy, i), 
      position); 
} 

//-----------------------------------------------------------------------
// NAME: FS_Is_Inside_If 
// FUNCTION: Returns TRUE if 'wn_use' is nested inside an IF within          
//   'wn_stop', FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL FS_Is_Inside_If(WN* wn_use,
			    WN* wn_stop) 
{
  WN *wn;
  for (wn = wn_use; wn != NULL && wn != wn_stop; 
      wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_IF) 
      return TRUE;
  FmtAssert(wn == wn_stop, ("wn_use was not originally inside wn_stop")); 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: FS_Substitute 
// FUNCTION: Substitute the right-hand-side of 'wn_orig' for every 
//   use that reaches its definition. 
// NOTE: 'loop_ls' stores the lexical positions of all of the nodes 
//   in the original outermost do loop for which we are substituting 
//   scalars. 
//-----------------------------------------------------------------------
static void FS_Substitute(WN* wn_orig, 
			  LS_IN_LOOP* loop_ls) 
{
  REDUCTION_MANAGER* rm = red_manager; 
  USE_LIST *use_list = du->Du_Get_Use(wn_orig);
  USE_LIST_ITER iter(use_list);
  DU_NODE* unode = NULL;
  DU_NODE* unnode_next = NULL; 
  INT sub_count = 0; 
  for (unode = iter.First(); !iter.Is_Empty(); unode = unnode_next) {
    WN* use = unode->Wn();
    unnode_next = iter.Next(); 
    if (WN_operator(use) != OPR_LDID) 
      continue;
    if (FS_Is_Inside_If(use, LWN_Get_Parent(wn_orig)))
      continue; 

    sub_count++; 
    INT position = loop_ls->In(use); 
    INT32 count = 0;
    if (Cur_PU_Feedback) {
      count = WN_MAP32_Get(WN_MAP_FEEDBACK,use);
    }
    if (rm != NULL) { 
      REDUCTION_TYPE red_type = rm->Which_Reduction(use);
      if (red_type != RED_NONE) { 
        WN *wn;
	for (wn = use; wn != NULL; wn = LWN_Get_Parent(wn))
	  if (OPCODE_is_store(WN_opcode(wn)) 
	      && rm->Which_Reduction(wn) == red_type)     
	    break; 
	FmtAssert(wn != NULL, 
	  ("Could not find store to match reduction load."));
	rm->Erase(wn); 
      } 
    } 
    BOOL added_convert = FALSE; 
    WN* wn_copy = Replace_Wnexp_With_Exp_Copy(use, WN_kid0(wn_orig), du,
      &added_convert);
#ifdef KEY
    //bug 12622: for the following case, an implicit CVTL is applied
    //in STID. We need an explicit CVTL instead of CVT when replacing
    //LDIDs with the the ILOAD.
    //
    //  I4I1ILOAD
    // U1STID
    if(added_convert && WN_operator(wn_copy) == OPR_CVT &&
       WN_operator(WN_kid0(wn_orig)) == OPR_ILOAD &&
       MTYPE_bit_size(WN_desc(wn_orig))<MTYPE_bit_size(WN_rtype(WN_kid0(wn_orig)))){
           WN_set_operator(wn_copy, OPR_CVTL);
           WN_set_desc(wn_copy, MTYPE_V);
           WN_cvtl_bits(wn_copy) = MTYPE_bit_size(WN_desc(wn_orig));
      }
#endif
    LWN_Set_Frequency_Tree(wn_copy,count);
    WN* wn_true_copy = added_convert ? WN_kid0(wn_copy) : wn_copy; 
    Fix_Access_Arrays_In_Copy_Block(wn_true_copy); 
    Fix_Deps_In_Copy_Block(loop_ls, WN_kid0(wn_orig), wn_true_copy, position); 
  } 
  if (LNO_Verbose) {
    fprintf(stdout, " Forward Substituting %d occurences of %s in loop %s\n", 
      sub_count, WB_Whirl_Symbol(wn_orig), 
      WB_Whirl_Symbol(Enclosing_Loop(wn_orig)));
    fprintf(TFile, " Forward Substituting %d occurences of %s in loop %s\n", 
      sub_count, WB_Whirl_Symbol(wn_orig), 
      WB_Whirl_Symbol(Enclosing_Loop(wn_orig)));
  } 
  LWN_Extract_From_Block(wn_orig); 
  LWN_Delete_Tree(wn_orig); 
} 

//-----------------------------------------------------------------------
// NAME: BS_Collect_Array 
// FUNCTION: For the array reference 'wn_copy' nested inside the loops in 
//   'copy_stack', add an entry for 'array_stack' and 'position_stack' so 
//   that the dependences will be properly updated.  The 'array_stack' entry 
//   should be the address of the array's whirl node.  The 'position_stack' 
//   entry should be 'position'. 
//-----------------------------------------------------------------------

static void BS_Collect_Array(WN* wn_copy,
                             DOLOOP_STACK* copy_stack,
                             STACK<WN*>* array_stack,
                             STACK<INT>* position_stack,
                             INT position)
{
  dg->Add_Vertex(wn_copy); 
  OPERATOR opr = WN_operator(wn_copy);
  FmtAssert(opr == OPR_ILOAD || opr == OPR_ISTORE || opr == OPR_LDID
    || opr == OPR_STID,  
    ("BS_Collect_Array() not called on OPR_ILOAD(LDID) or OPR_ISTORE(STID)")); 
  INT kid_number = opr == OPR_ILOAD ? 0 : 1;
#ifdef KEY //bug 11113: LNO_Build_Access_Array applies only to arrays
  if((opr == OPR_ILOAD || opr == OPR_ISTORE)
           && WN_operator(WN_kid(wn_copy, kid_number))==OPR_ARRAY)
#endif
    LNO_Build_Access_Array(WN_kid(wn_copy, kid_number), copy_stack,
           &LNO_default_pool);
  array_stack->Push(wn_copy);
  position_stack->Push(position);
}

//-----------------------------------------------------------------------
// NAME: BS_Collect_Arrays 
// FUNCTION: For all array references in the tree rooted at 'wn_copy' 
//   nested inside the loops in 'copy_stack', add an entry for 'array_stack' 
//   and 'position_stack' so that the dependences will be properly updated. 
//   The 'array_stack' entry should be the address of the array's whirl 
//   node.  The 'position_stack' entry should be 'position'. 
//-----------------------------------------------------------------------

static void BS_Collect_Arrays(WN* wn_orig, 
			      WN* wn_copy, 
			      DOLOOP_STACK* copy_stack, 
			      STACK<WN*>* array_stack,
                              STACK<INT>* position_stack, 
			      INT position)
{
  if (dg->Get_Vertex(wn_orig))
    BS_Collect_Array(wn_copy, copy_stack, array_stack, position_stack,
      position); 
  if (WN_opcode(wn_copy) == OPC_BLOCK) {
    WN* wn_old = WN_first(wn_orig); 
    for (WN* wn = WN_first(wn_copy); wn != NULL; wn = WN_next(wn)) {
      BS_Collect_Arrays(wn_old, wn, copy_stack, array_stack, position_stack, 
	position);
      wn_old = WN_next(wn_old);  
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_copy); i++) 
      BS_Collect_Arrays(WN_kid(wn_orig, i), WN_kid(wn_copy, i), copy_stack, 
        array_stack, position_stack, position);
  }
}

//-----------------------------------------------------------------------
// NAME: BS_Replace_Load 
// FUNCTION: Replace the scalar load 'wn_scalar' with a copy of the 
//   the array load 'wn_load'.   
// NOTE: 'loop_ls' stores the lexical positions of all of the nodes 
//   in the original outermost do loop for which we are substituting 
//   scalars. 
// NOTE: The new array reference and its position are saved in the 
//   'array_stack' and 'position_stack'. 
//-----------------------------------------------------------------------

static void BS_Replace_Load(WN* wn_scalar, 
			    WN* wn_load, 
			    LS_IN_LOOP* loop_ls, 
			    STACK<WN*>* array_stack,
			    STACK<INT>* position_stack)
{
  INT position = loop_ls->In(wn_scalar); 
  REDUCTION_TYPE red_type = RED_NONE;  
  if (rm != NULL && rm->Which_Reduction(wn_scalar))
    red_type = rm->Which_Reduction(wn_scalar); 
  INT32 count = 0;
  if (Cur_PU_Feedback) {
    count = WN_MAP32_Get(WN_MAP_FEEDBACK,wn_scalar);
  }
  BOOL added_cvt = FALSE;
  WN* wn_copy = Replace_Wnexp_With_Exp_Copy(wn_scalar,wn_load,du,&added_cvt); 
  LWN_Set_Frequency_Tree(wn_copy,count);
  DOLOOP_STACK copy_stack(&LNO_local_pool);
  WN* wn_true_copy = added_cvt ? WN_kid0(wn_copy) : wn_copy;
  Build_Doloop_Stack(wn_true_copy, &copy_stack);
  BS_Collect_Array(wn_true_copy, &copy_stack, array_stack, position_stack,
                   position); 
  BS_Collect_Arrays(WN_kid0(wn_load), WN_kid0(wn_true_copy), &copy_stack, 
                    array_stack, position_stack, position); 
  if (rm != NULL && red_type != RED_NONE)
    rm->Add_Reduction(wn_true_copy, red_type);  
}
 
//-----------------------------------------------------------------------
// NAME: BS_Replace_Store 
// FUNCTION: Replace the scalar store 'wn_scalar' with a copy of the 
//   the array load 'wn_store'.  
// NOTE: 'loop_ls' stores the lexical positions of all of the nodes 
//   in the original outermost do loop for which we are substituting 
//   scalars. 
// NOTE: The new array reference and its position are saved in the 
//   'array_stack' and 'position_stack'. 
//-----------------------------------------------------------------------

static void BS_Replace_Store(WN* wn_scalar, 
			     WN* wn_store, 
			     LS_IN_LOOP* loop_ls,
			     STACK<WN*>* array_stack,
			     STACK<INT>* position_stack)
{
  INT position = loop_ls->In(wn_scalar);
  REDUCTION_TYPE red_type = RED_NONE; 
  if (rm != NULL && rm->Which_Reduction(wn_scalar))
    red_type = rm->Which_Reduction(wn_scalar);       
  INT32 count = 0;
  if (Cur_PU_Feedback) {
    count = WN_MAP32_Get(WN_MAP_FEEDBACK,wn_scalar);
  }
  WN* wn_copy = Replace_Scalar_Store_With_Array_Store(wn_scalar, wn_store, du);
  LWN_Set_Frequency_Tree(wn_copy, count);
  DOLOOP_STACK copy_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_copy, &copy_stack);
  BS_Collect_Array(wn_copy, &copy_stack, array_stack, position_stack,
    position); 
  BS_Collect_Arrays(WN_kid1(wn_store), WN_kid1(wn_copy), &copy_stack, 
    array_stack, position_stack, position); 
  if (rm != NULL && red_type != RED_NONE) 
    rm->Add_Reduction(wn_copy, red_type);  
}

//-----------------------------------------------------------------------
// NAME: FS_Load_Assigned_on_Loop_Iteration 
// FUNCTION: Returns TRUE if the load 'wn_load' is assigned on the 
//   current iteration of 'wn_loop' by something with a lexical number
//   between min_lex and max_lex, FALSE otherwise. 
//
//   for now since we don't have anti-dependences, conservatively
//   return TRUE if an LDID is assigned by anything in the loop
//-----------------------------------------------------------------------

static BOOL FS_Load_Assigned_on_Loop_Iteration(WN* wn_load, 
			WN* wn_loop, LS_IN_LOOP *loop_ls,
			INT32 min_lex, INT32 max_lex) 
{
  if (WN_operator(wn_load) == OPR_LDID) {
    if (SYMBOL(wn_load) == SYMBOL(WN_start(wn_loop)))
      return FALSE; 
    if (dg->Get_Vertex(wn_load))
      return TRUE; 
    DEF_LIST *def_list = du->Ud_Get_Def(wn_load); 
    if (def_list != NULL) { 
      if (def_list->Incomplete())
	return TRUE; 
      DEF_LIST_ITER iter(def_list); 
      DU_NODE* dnode = NULL;
      for (dnode = iter.First(); !iter.Is_Empty(); dnode = iter.Next()) { 
	WN* def = dnode->Wn(); 
	if (Wn_Is_Inside(def,wn_loop))
	  return TRUE; 
      }
    }
    return FALSE; 
  } 
  if (WN_operator(wn_load) == OPR_ILOAD) {  
    EINDEX16 e = 0;  
    VINDEX16 v = dg->Get_Vertex(wn_load);
    INT depth = Get_Do_Loop_Info(wn_loop)->Depth; 
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      WN* def = dg->Get_Wn(dg->Get_Sink(e));    
      INT def_lex_number = loop_ls->In(def);
      if ((def_lex_number >= min_lex) && (def_lex_number <= max_lex)) {
        if (Wn_Is_Inside(def, wn_loop) 
	  && dg->Depv_Array(e)->One_Equal_Through_Depth(depth))
	return TRUE; 
      }
    } 
    return FALSE; 
  } 
  FmtAssert(0, ("Found a LOAD which was not LDID or ILOAD"));
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: FS_Exp_Assigned_on_Loop_Iteration 
// FUNCTION: Returns TRUE if any load in the expression 'wn_exp' is 
//   assigned on the current iteration of 'wn_loop' by a store that
//   has a lexical number between min_lex and max_lex, FALSE otherwise. 
//
//-----------------------------------------------------------------------

static BOOL FS_Exp_Assigned_on_Loop_Iteration(WN* wn_exp,
                 WN* wn_loop, 
		 LS_IN_LOOP *loop_ls,
		 INT32 min_lex=INT32_MIN, INT32 max_lex=INT32_MAX)
{ 
  if (OPCODE_is_load(WN_opcode(wn_exp)))  
    if (FS_Load_Assigned_on_Loop_Iteration(wn_exp, wn_loop,
						loop_ls,min_lex,max_lex))
      return TRUE; 
  for (INT i = 0; i < WN_kid_count(wn_exp); i++)  
    if (FS_Exp_Assigned_on_Loop_Iteration(WN_kid(wn_exp, i), 
				      wn_loop,loop_ls,min_lex,max_lex))
      return TRUE;
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: FS_Is_In_Do_Loop_Expression 
// FUNCTION: Returns TRUE if 'wn' is part of a do-loop expression, FALSE
//   otherwise.  
//-----------------------------------------------------------------------

static BOOL FS_Is_In_Do_Loop_Expression(WN* wn) 
{ 
  for (WN* wnt = wn; wnt != NULL; wnt = LWN_Get_Parent(wnt)) { 
    if (WN_opcode(wnt) == OPC_BLOCK) 
       return FALSE; 
    if (WN_opcode(wnt) == OPC_DO_LOOP)
       return TRUE; 
  } 
  FmtAssert(0, ("Should have found BLOCK or DO_LOOP"));  
  return FALSE; 
}  

//-----------------------------------------------------------------------
// NAME: Contains_Intrinsic_Op
// FUNCTION: Returns TRUE if the tree rooted at 'wn_tree' has a node 
//   which is an INTRINSIC_OP, otherwise returns FALSE. 
//-----------------------------------------------------------------------

static BOOL Contains_Intrinsic_Op(WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_INTRINSIC_OP)
    return TRUE; 

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (Contains_Intrinsic_Op(wn))
        return TRUE;
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      if (Contains_Intrinsic_Op(WN_kid(wn_tree, i)))
        return TRUE;
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: FS_Worthwhile 
// FUNCTION: Returns TRUE if it possible to forward substitute the 
//   store 'wn_orig' within the loop 'wn_loop' and eliminate it, FALSE 
//   otherwise.    
//-----------------------------------------------------------------------

static BOOL FS_Worthwhile(WN* wn_orig,
		          WN* wn_loop,
			  LS_IN_LOOP* loop_ls)
{
  WN *surrounding_mp=NULL;
  if (Contains_MP) {
    surrounding_mp = Find_MP(wn_orig);
  }
  if (Contains_Dedicated_Preg(wn_orig))
    return FALSE; 
  if (Contains_Intrinsic_Op(wn_orig))
    return FALSE; 
  WN *def_loop = Enclosing_Loop(wn_orig);
  BOOL all_uses_in_same_def_loop = TRUE;
  INT max_lex_use = 0;
  INT count = Count_Loads_And_Leafs(WN_kid0(wn_orig)); 
  if (count > FS_LOAD_AND_LEAF_LIMIT && !Do_Loop_Is_Inner(wn_loop))
    return FALSE;  
  USE_LIST *use_list = du->Du_Get_Use(wn_orig); 
  if (use_list == NULL || use_list->Incomplete() || dg->Get_Vertex(wn_orig))
    return FALSE; 
  USE_LIST_ITER iter(use_list);
  DU_NODE* unode = NULL; 
  BOOL found_inner_use = FALSE; 
  INT use_count = 0; 
#ifdef KEY //bug 11786 count total number of uses
  INT total_use_count = 0;
#endif 
  for (unode = iter.First(); !iter.Is_Empty(); unode = iter.Next()) { 
    WN *use = unode->Wn(); 
    if (WN_operator(use) != OPR_LDID)
      return FALSE; 
    if (Contains_MP) {
      WN *mp = Find_MP(use);
      if (mp != surrounding_mp) {
	return FALSE;
      }
    }
    if (!Wn_Is_Inside(use, wn_loop))
       return FALSE; 

    // Avoid FS for nodes directly under ASM_INPUT in order to preserve type.
    if (WN_operator(LWN_Get_Parent(use)) == OPR_ASM_INPUT)
      return FALSE;

    WN* use_loop = Enclosing_Loop(use); 
    if (use_loop != def_loop) {
      all_uses_in_same_def_loop = FALSE;
    } else {
      max_lex_use = MAX(max_lex_use,loop_ls->In(use));
    }
    if (Wn_Is_Inside(use_loop, wn_loop) && use_loop != wn_loop) 
      found_inner_use = TRUE;  
    DEF_LIST *def_list = du->Ud_Get_Def(use); 
    if (def_list == NULL || def_list->Incomplete() || dg->Get_Vertex(use)) 
      return FALSE;
    if (FS_Is_In_Do_Loop_Expression(use))
      return FALSE;  
    DEF_LIST_ITER iter(def_list); 
    DU_NODE* dnode = NULL;
    INT def_count = 0;
    BOOL substitution_candidate = TRUE;   
    for (dnode = iter.First(); !iter.Is_Empty(); dnode = iter.Next()) { 
      WN* def = dnode->Wn(); 
      if (++def_count > 1 || def != wn_orig) { 
	substitution_candidate = FALSE; 
	break;
      } 
    }
    if (!substitution_candidate) 
      return FALSE;

#ifdef KEY
//bug 14352: Forward substitution requires the "use" is under the subtree
//of LWN_Get_Parent(wn_orig)). Otherwise, it is not legal to substitute
//"use" with the right-hand side of wn_orig.
    WN *tmp=NULL;
    for(tmp = use; tmp!=NULL; tmp=LWN_Get_Parent(tmp))
     if(tmp == LWN_Get_Parent(wn_orig))
        break;
    if(!tmp) return FALSE;
#endif

    if (FS_Is_Inside_If(use, LWN_Get_Parent(wn_orig)))
       return FALSE;
    WN *wn;
    for (wn = use; wn != NULL; wn = LWN_Get_Parent(wn))
      if (WN_operator(wn) == OPR_ARRAY)
	break;
#ifdef KEY //bug 11786: count total number of uses
    total_use_count++;
#endif
    if (wn != NULL) 
      continue;
    use_count++;  
  }
  if (use_count == 0)//no use or all uses under array
    return FALSE; 
  if (count > FS_LOAD_AND_LEAF_LIMIT && found_inner_use) 
    return FALSE; 
#ifdef KEY //bug 11786: let total uses to control code expansion
  if (count > FS_LOAD_AND_LEAF_LIMIT && total_use_count > 1)
#else
  if (count > FS_LOAD_AND_LEAF_LIMIT && use_count > 1) 
#endif
    return FALSE;  
  if (all_uses_in_same_def_loop) {
    if (FS_Exp_Assigned_on_Loop_Iteration(wn_orig, wn_loop,
		loop_ls,loop_ls->In(wn_orig),max_lex_use)) 
      return FALSE;
  } else {
    if (FS_Exp_Assigned_on_Loop_Iteration(wn_orig, wn_loop,loop_ls)) 
      return FALSE;
  }

  return TRUE;  
} 

//-----------------------------------------------------------------------
// NAME: BS_Loop_Within_Equivalence_Class 
// FUNCTION: Returns TRUE if at least one the references in 'scalar_stack'
//   is nested within a loop deeper than 'wn_orig'.  Otherwise, returns 
//   FALSE.  
//-----------------------------------------------------------------------

static BOOL BS_Loop_Within_Equivalence_Class(WN* wn_orig, 
					     STACK<WN*>* scalar_stack)
{
  WN* wn_orig_loop = Enclosing_Loop(wn_orig); 
  for (INT i = 0; i < scalar_stack->Elements(); i++) {
    WN* wn = scalar_stack->Bottom_nth(i);  
    WN* wn_loop = Enclosing_Loop(wn);
    if (wn_loop != wn_orig_loop && Wn_Is_Inside(wn_loop, wn_orig_loop))
      return TRUE;
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: BS_Find_Sibling 
// FUNCTION: Returns a node in the 'region' which is an ancestor of 'wn_try'
//   if such a node exists.  Otherwise, returns NULL.  
//-----------------------------------------------------------------------

static WN* BS_Find_Sibling(WN* wn_try,
			   HASH_TABLE<WN*,INT>* region)
{
  for (WN* wn = wn_try; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (region->Find(wn) != 0)
      return wn;
  return NULL; 
} 

//-----------------------------------------------------------------------
// NAME: BS_Find_Region 
// FUNCTION: Returns TRUE if a 'region' of sibling statements can be found 
//   which include all of the references in the 'scalar_stack'. The last  
//   statement in the 'region' must be the statement containing 'wn_orig'.   
//   Otherwise, returns FALSE. 
//-----------------------------------------------------------------------

static BOOL BS_Find_Region(WN* wn_orig, 
			   STACK<WN*>* scalar_stack, 
			   HASH_TABLE<WN*,INT>* region)
{
  WN* wn_first = LWN_Get_Parent(wn_orig);
  WN* wn_last = LWN_Get_Parent(wn_orig);
  WN *wn;
  for (wn = wn_last; wn != NULL; wn = WN_prev(wn))
    region->Enter(wn, 1);
  for (INT i = 0; i < scalar_stack->Elements(); i++) {
    WN* wn_scalar = scalar_stack->Bottom_nth(i); 
    WN* wn_sibling = BS_Find_Sibling(wn_scalar, region);
    if (wn_sibling == NULL) 
      return FALSE;  
    for (wn = WN_prev(wn_first); wn != NULL; wn = WN_prev(wn))
      if (wn == wn_sibling) 
	break;
    if (wn == wn_sibling) 
      wn_first = wn_sibling; 
  }
  for (wn = WN_prev(wn_first); wn != NULL; wn = WN_prev(wn))
    region->Remove(wn);  
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: BS_Range_Check 
// FUNCTION: Returns TRUE if all assignments to 'wn_orig' lies outside 
//   the 'region', FALSE otherwise.
// NOTE: The 'region' is a list of WHIRL nodes which are statements and 
//   siblings of one another.  
//-----------------------------------------------------------------------

static BOOL BS_Range_Check(WN* wn_orig, 
			   HASH_TABLE<WN*,INT>* region)
{
  EINDEX16 e = 0;
  WN* wn_store = LWN_Get_Parent(wn_orig); 
  VINDEX16 v = dg->Get_Vertex(wn_store);
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn = dg->Get_Wn(dg->Get_Source(e));
    if (wn != wn_store && BS_Find_Sibling(wn, region))
      return FALSE; 
  } 
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn = dg->Get_Wn(dg->Get_Sink(e));
    if (wn != wn_store && BS_Find_Sibling(wn, region))
      return FALSE; 
  } 
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: BS_Crosses_MP_Region 
// FUNCTION: Returns TRUE if at least one the elements in 'scalar_stack'
//   does not belong to the same MP region as 'wn_orig'.  Otherwise, 
//   returns FALSE. 
//-----------------------------------------------------------------------

static BOOL BS_Crosses_MP_Region(WN* wn_orig, 
			         STACK<WN*>* scalar_stack) 
{
  if (!Contains_MP) 
    return FALSE; 
  WN* surrounding_mp = Find_MP(wn_orig); 
  for (INT i = 0; i < scalar_stack->Elements(); i++) 
    if (Find_MP(scalar_stack->Bottom_nth(i)) != surrounding_mp)
      return TRUE;  
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: BS_Is_Index_Variable 
// FUNCTION: Returns TRUE if some entry in the 'scalar_stack' is an 
//   index variable of a do loop. 
//-----------------------------------------------------------------------

static BOOL BS_Is_Index_Variable(STACK<WN*>* scalar_stack)
{
  for (INT i = 0; i < scalar_stack->Elements(); i++) {
    WN* wn_scalar = scalar_stack->Bottom_nth(i);
    for (WN* wn = wn_scalar; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_DO_LOOP 
	  && SYMBOL(WN_index(wn)) == SYMBOL(wn_scalar))
	return TRUE; 
    }
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: BS_Has_If_In_Region 
// FUNCTION: Returns TRUE if any of the elements of 'scalar_stack' is 
//   surrounded by an IF inside the 'region' over which backward 
//   substitution is to take place. 
//-----------------------------------------------------------------------

static BOOL BS_Has_If_In_Region(STACK<WN*>* scalar_stack,
			        HASH_TABLE<WN*,INT>* region)
{
  for (INT i = 0; i < scalar_stack->Elements(); i++) {
    WN* wn_scalar = scalar_stack->Bottom_nth(i); 
    WN* wn_final = BS_Find_Sibling(wn_scalar, region);
    FmtAssert(wn_final != NULL, ("wn_scalar was not in region"));  
    WN *wn;
    for (wn = wn_scalar; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_IF)
	return TRUE; 
      if (wn == wn_final) 
	break;
    }
    FmtAssert(wn == wn_final, ("wn_scalar was not in region"));  
  } 
  return FALSE;     
}  

//-----------------------------------------------------------------------
// NAME: BS_Has_Varying_Access_Array_In_Region
// FUNCTION: Returns TRUE if the access array of 'wn_store' varies inside
//   he loop enclosing the substitution region, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL BS_Has_Varying_Access_Array_In_Region(WN* wn_store) 
{
  WN* wn_enclosing_loop = Enclosing_Loop(wn_store); 
  if (wn_enclosing_loop == NULL) 
    return FALSE;
  WN* wn_array = WN_kid1(wn_store); 
  ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  INT varying_depth = array->Non_Const_Loops() - 1; 
  return (varying_depth >= Do_Depth(wn_enclosing_loop)); 
}

//-----------------------------------------------------------------------
// NAME: BS_Has_Use_In_Subscript
// FUNCTION: Returns TRUE if 'wn_sub' is aliased with some store on the
//   'scalar_stack'; return FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL BS_Has_Use_In_Subscript(WN* wn_sub,
                                    STACK<WN*>* scalar_stack)
{
  for (INT i = 0; i < scalar_stack->Elements(); i++) {
    WN* wn_store = scalar_stack->Bottom_nth(i);
    if (OPCODE_is_store(WN_opcode(wn_store))) {
      LWN_ITER* itr = LWN_WALK_TreeIter(wn_sub);
      for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
        WN* wn = itr->wn;
	OPCODE op = WN_opcode(wn);
        OPERATOR opr = WN_operator(wn);
	if (!OPCODE_is_load(op) && !OPCODE_is_store(op) && opr != OPR_PARM)
	  continue; 
        ALIAS_RESULT result = Aliased(Alias_Mgr, wn, wn_store);
        if (result != NOT_ALIASED)
          return TRUE;
      }
    }
  }
  return FALSE;
}

#define MAX_COEFF 20		// same as in cache_model.cxx

//-----------------------------------------------------------------------
// Returns TRUE if the access array of 'wn_array' has a loop
// index variable coefficient that is greater than MAX_COEFF.
// We need this because cache_model doesn't deal very well with
// linearized references.
//-----------------------------------------------------------------------

static BOOL 
BS_Is_Linearized(WN* wn_array) 
{
  ACCESS_ARRAY* array = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_array);
  INT num_dims = array->Num_Vec();
  for (INT i = 0; i < num_dims; i++) {
    ACCESS_VECTOR* dim = array->Dim(i);
    INT num_loops = dim->Nest_Depth();
    for (INT j = 0; j < num_loops; j++) {
      if (abs(dim->Loop_Coeff(j)) > MAX_COEFF) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: BS_Worthwhile 
// FUNCTION: Returns a stack of scalars to be replaced with array references
//   of the type 'wn_orig' if this replacement is both legal and thought to
//   be profitable.  Otherwise, it returns NULL. 
//-----------------------------------------------------------------------

static STACK<WN*>* BS_Worthwhile(WN* wn_store) 
{
  WN* wn_orig = WN_kid0(wn_store);
  if (!OPCODE_is_load(WN_opcode(wn_orig)) || WN_kid_count(wn_orig) != 0) 
    return NULL;
  DEF_LIST *def_list = du->Ud_Get_Def(wn_orig);
  if (def_list == NULL || def_list->Incomplete() || dg->Get_Vertex(wn_orig))
    return NULL;
  STACK<WN*>* scalar_stack = NULL;
  scalar_stack = Scalar_Equivalence_Class(wn_orig, du, &LNO_default_pool);
  if (scalar_stack == NULL)
    return NULL;
  if (BS_Crosses_MP_Region(wn_orig, scalar_stack)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool); 
    return NULL;
  }
  if (BS_Is_Index_Variable(scalar_stack)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool);
    return NULL;
  } 
  if (!BS_Loop_Within_Equivalence_Class(wn_orig, scalar_stack)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool); 
    return NULL;
  } 
  HASH_TABLE<WN*,INT> region(FS_SIBLING_HASH_SIZE, &LNO_local_pool);
  if (!BS_Find_Region(wn_orig, scalar_stack, &region)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool); 
    return NULL;
  }
  if (!BS_Range_Check(wn_orig, &region)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool); 
    return NULL;
  }
  if (BS_Has_If_In_Region(scalar_stack, &region)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool);
    return NULL;
  }
  if (BS_Has_Varying_Access_Array_In_Region(wn_store)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool);
    return NULL;
  }
  if (BS_Has_Use_In_Subscript(WN_kid1(wn_store), scalar_stack)) {
    CXX_DELETE(scalar_stack, &LNO_default_pool);
    return NULL;
  }
  if (BS_Is_Linearized(WN_kid1(wn_store))) {
    CXX_DELETE(scalar_stack, &LNO_default_pool);
    return NULL;
  }
  return scalar_stack; 
}

//-----------------------------------------------------------------------
// NAME: BS_Matching_Load 
// FUNCTION: Returns TRUE if the load 'wn_load' and the store 'wn_store'
//   refer to the same array element, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL BS_Matching_Load(WN* wn_load, 
                             WN* wn_store) 
{
  if (!OPCODE_is_load(WN_opcode(wn_load))) 
    return FALSE;
  if (WN_kid_count(wn_load) == 0) 
    return FALSE; 
  ACCESS_ARRAY *ac_load = (ACCESS_ARRAY *)
    WN_MAP_Get(LNO_Info_Map, WN_kid0(wn_load));
  ACCESS_ARRAY *ac_store = (ACCESS_ARRAY *)
    WN_MAP_Get(LNO_Info_Map, WN_kid1(wn_store));
  if (DEPV_COMPUTE::Base_Test(wn_load, NULL,wn_store,NULL)
      != DEP_CONTINUE)
    return FALSE;
  if (WN_desc(wn_load) != WN_desc(wn_store))
    return FALSE;
  if (!Equivalent_Access_Arrays(ac_load, ac_store, wn_load, wn_store))
    return FALSE;
  return TRUE; 
}

    
//-----------------------------------------------------------------------
// NAME: BS_Substitute 
// FUNCTION: Replace each of the scalars in 'scalar_stack' with an array
//   reference matching 'wn_store'. Then delete the statement containing 
//   'wn_orig', because it should no longer be needed.  
// NOTE: 'loop_ls' stores the lexical positions of all of the nodes
//   in the original outermost do loop 'wn_loop' for which we are
//   substituting scalars.
//-----------------------------------------------------------------------

static void BS_Substitute(WN* wn_store, 
			  WN* wn_loop, 
	                  STACK<WN*>* scalar_stack, 
			  LS_IN_LOOP* loop_ls)
{
  WN* wn_orig = WN_kid0(wn_store);
  if (LNO_Verbose) {
    INT sub_count = scalar_stack->Elements();
    FmtAssert(sub_count > 0,
      ("Backward substituting less than one occurrence."));
    fprintf(stdout, " Backward Substituting %d occurences of %s in loop %s\n",
      sub_count, WB_Whirl_Symbol(wn_orig), WB_Whirl_Symbol(wn_loop));
    fprintf(TFile, " Backward Substituting %d occurences of %s in loop %s\n",
      sub_count, WB_Whirl_Symbol(wn_orig), WB_Whirl_Symbol(wn_loop));
  }
  WN* wn_load = Create_ILoad_From_IStore(wn_store, du, dg); 
  STACK<WN*>* array_stack = CXX_NEW(STACK<WN*> (&LNO_local_pool),
    &LNO_local_pool);
  STACK<INT>* position_stack = CXX_NEW(STACK<INT> (&LNO_local_pool),
    &LNO_local_pool);
  WN* wn_red_load = NULL; 
  INT i;
  for (i = 0; i < scalar_stack->Elements(); i++) {
    WN* wn = scalar_stack->Bottom_nth(i);
    if (wn == wn_orig)
      continue;
    if (wn_red_load == NULL
	&& WN_operator(wn) == OPR_STID 
	&& BS_Matching_Load(WN_kid0(wn), wn_store)) {
      wn_red_load = wn; 
      continue; 
    } 
    OPCODE op = WN_opcode(wn);
    if (OPCODE_is_store(op) && WN_kid0(wn) != wn_load)
      BS_Replace_Store(wn, wn_store, loop_ls, array_stack, position_stack);
    else if (OPCODE_is_load(op) && LWN_Get_Parent(wn) != wn_store)
      BS_Replace_Load(wn, wn_load, loop_ls, array_stack, position_stack);
  }
  if (wn_red_load != NULL) { 
    LWN_Extract_From_Block(wn_red_load);
    LWN_Delete_Tree(wn_red_load);
  }
  EINDEX16 e = 0; 
  for (i = 0; i < array_stack->Elements(); i++) {
    WN* wn_one = array_stack->Bottom_nth(i);
    VINDEX16 v = dg->Get_Vertex(wn_store);
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      WN* wn_source = dg->Get_Wn(dg->Get_Source(e)); 
      if (wn_source == wn_store)
	continue; 
      if (!OPCODE_is_store(WN_opcode(wn_one)) 
	    && !OPCODE_is_store(WN_opcode(wn_source)))
	continue; 
      if (!dg->Add_Edge(dg->Get_Vertex(wn_source), dg->Get_Vertex(wn_one), 
	Create_DEPV_ARRAY(dg->Depv_Array(e), &LNO_default_pool))) { 
	LNO_Erase_Dg_From_Here_In(wn_store, dg);
        return; 
      } 
    }
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e)); 
      if (wn_sink == wn_store)
	continue; 
      if (!OPCODE_is_store(WN_opcode(wn_one)) 
	    && !OPCODE_is_store(WN_opcode(wn_sink)))
	continue; 
      if (!dg->Add_Edge(dg->Get_Vertex(wn_one), dg->Get_Vertex(wn_sink), 
	Create_DEPV_ARRAY(dg->Depv_Array(e), &LNO_default_pool))) { 
	LNO_Erase_Dg_From_Here_In(wn_store, dg);
        return; 
      } 
    }
    if (OPCODE_is_store(WN_opcode(wn_one))) {
      USE_LIST *use_list = du->Du_Get_Use(wn_store);
      USE_LIST_ITER iter(use_list);
      const DU_NODE* node = NULL;
      for (node = iter.First(); !iter.Is_Empty(); i++, node = iter.Next()) 
	du->Add_Def_Use(wn_one, node->Wn()); 
    }  
  }
  LWN_Extract_From_Block(wn_store);
  LWN_Delete_Tree(wn_store);
  LWN_Delete_Tree(wn_load); 
  for (i = 0; i < array_stack->Elements(); i++) {
    WN* wn_one = array_stack->Bottom_nth(i);
    INT position_one = position_stack->Bottom_nth(i);
    DOLOOP_STACK first_stack(&LNO_local_pool);
    Build_Doloop_Stack(wn_one, &first_stack);
    for (INT j = i; j < array_stack->Elements(); j++) {
      WN* wn_two = array_stack->Bottom_nth(j);
      INT position_two = position_stack->Bottom_nth(j);
      DOLOOP_STACK second_stack(&LNO_local_pool);
      Build_Doloop_Stack(wn_two, &second_stack);
      if (!OPCODE_is_store(WN_opcode(wn_one)) 
	    && !OPCODE_is_store(WN_opcode(wn_two)))
	continue; 
      if (!dg->Add_Edge(wn_one, &first_stack, wn_two,
        &second_stack, position_one < position_two)) { 
        LNO_Erase_Dg_From_Here_In(wn_two, dg);
	return; 
      } 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: FS_Array_Single_Def_Use
// FUNCTION: Returns TRUE if 'wn_def' is the unique definition of 'wn_use'.
//   We assume that 'wn_def' is an ISTORE and 'wn_use' is an ILOAD. 
//-----------------------------------------------------------------------

static BOOL FS_Array_Single_Def_Use(WN* wn_def, 
			            WN* wn_use)
{
  EINDEX16 e = 0;
  VINDEX16 v = dg->Get_Vertex(wn_use);
  BOOL found_def = FALSE; 
  WN* wn_loop_def = Enclosing_Loop(wn_def);
  WN* wn_loop_use = Enclosing_Loop(wn_use);
  if (wn_loop_def == NULL || wn_loop_def != wn_loop_use)
    return FALSE; 
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
    DEPV_ARRAY* dva = dg->Depv_Array(e);
    INT num_dim = dva->Num_Dim();
    for (INT i = 0; i < dva->Num_Vec(); i++) {
      DEPV* depv = dva->Depv(i);
      INT j;
      for (j = 0; j < num_dim; j++)
        if (DEP_Direction(DEPV_Dep(depv, j)) == DIR_POS)
          break;
      if (j < num_dim)
        continue;
      if (wn_source == wn_def) {
	found_def = TRUE; 
	continue; 
      }
      return FALSE; 
    }  
  }
  return found_def; 
} 

//-----------------------------------------------------------------------
// NAME: FS_Condition
// FUNCTION: Returns the innermost node enclosing 'wn_node' which repre-   
//   sents a structured condition node.  Returns NULL if there is none. 
//   If 'which_branch' to 1 if the return value is an IF node and the 
//   node 'wn_node' is under the THEN branch or to 2 if it is an IF node 
//   and the node 'wn_node' is under the ELSE branch.  Otherwise 'which_
//   branch' is set to 0.
//-----------------------------------------------------------------------

static WN* FS_Condition(WN* wn_node,
			INT* which_branch)
{
  WN* wnn = NULL; 
  *which_branch = 0;  
  WN *wn;
  for (wn = wn_node; wn != NULL; wn = LWN_Get_Parent(wn)) {
    switch (WN_opcode(wn)) {
    case OPC_IF: 
      if (wnn == WN_then(wn))
	*which_branch = 1; 
      else if (wnn == WN_else(wn))
	*which_branch = 2; 
      return wn; 
    case OPC_DO_LOOP:
    case OPC_DO_WHILE: 
    case OPC_WHILE_DO: 
      return wn; 
    } 
    wnn = wn; 
  } 
  return wn; 
} 

//-----------------------------------------------------------------------
// NAME: FS_Array_Worthwhile
// FUNCTION: Returns TRUE if there is a non-empty set of ILOADs into which 
//   we can forward substitute the ISTORE 'wn_def'.  If so, 'st_use' gives
//   the list of such ILOADs.     
//-----------------------------------------------------------------------

static BOOL FS_Array_Worthwhile(WN* wn_def,
				STACK<WN*>* st_use,
				LS_IN_LOOP* loop_ls)
{
  WN* wn_loop = Enclosing_Do_Loop(wn_def); 
  if (wn_loop == NULL) 
    return FALSE; 
  if (Do_Loop_Has_Gotos(wn_loop)) {
    return FALSE;
  }
  WN* wn_surrounding_mp = NULL;
  if (Contains_MP) 
    wn_surrounding_mp = Find_MP(wn_def);
  if (Contains_Dedicated_Preg(wn_def))
    return FALSE;
  if (Contains_Intrinsic_Op(wn_def))
    return FALSE; 
  INT count = Node_Count(WN_kid0(wn_def)); 
  if (count > FS_ARRAY_EXP_LIMIT) 
    return FALSE; 
  EINDEX16 e = 0;
  VINDEX16 v_def = dg->Get_Vertex(wn_def);
  if (v_def == 0)
    return FALSE; 
  STACK<WN*> fs_stack(&LNO_local_pool); 
  INT loop_depth = Do_Loop_Depth(wn_loop); 
  INT max_lex_use = 0;
  for (e = dg->Get_Out_Edge(v_def); e != 0; e = dg->Get_Next_Out_Edge(e)) { 
    WN* wn_use = dg->Get_Wn(dg->Get_Sink(e));
    max_lex_use = MAX(max_lex_use,loop_ls->In(wn_use));
  } 
  for (e = dg->Get_Out_Edge(v_def); e != 0; e = dg->Get_Next_Out_Edge(e)) { 
    WN* wn_use = dg->Get_Wn(dg->Get_Sink(e));
    if (Contains_MP && Find_MP(wn_use) != wn_surrounding_mp)
        return FALSE;
    if (Wn_Is_Inside(wn_use, wn_def))
      continue; 
    if (!BS_Matching_Load(wn_use, wn_def))
      continue; 
    if (!FS_Array_Single_Def_Use(wn_def, wn_use))
      continue; 
    INT which_branch_def = -1; 
    INT which_branch_use = -1; 
    if (FS_Condition(wn_def, &which_branch_def) 
	!= FS_Condition(wn_use, &which_branch_use))
      continue; 
    if (which_branch_def != which_branch_use)
      continue; 
    if (FS_Exp_Assigned_on_Loop_Iteration(wn_def, wn_loop, loop_ls, 
      loop_ls->In(wn_def), max_lex_use))
      continue; 
    st_use->Push(wn_use); 
  }
  return st_use->Elements() >= 1;
} 

//-----------------------------------------------------------------------
// NAME: FS_Array_Substitute
// FUNCTION: Forward substitutes the ISTORE 'wn_def' in the ILOAD 'wn_use'.
//   The 'loop_ls' is used to calculate relative positions of nodes. 
//-----------------------------------------------------------------------

static void FS_Array_Substitute(WN* wn_def, 
				WN* wn_use,
				LS_IN_LOOP* loop_ls)
{ 
  DU_MANAGER* du = Du_Mgr; 
  REDUCTION_MANAGER* rm = red_manager; 
  INT32 count = 0;
  if (LNO_Verbose) {
    char buffer[FS_MAX_STRING_LENGTH]; 
    WB_Dep_Symbol(wn_def, buffer, FS_MAX_STRING_LENGTH - 1); 
    fprintf(stdout, " Forward Substituting Array %s in loop %s\n", 
      buffer, WB_Whirl_Symbol(Enclosing_Loop(wn_def)));
    fprintf(TFile, " Forward Substituting Array %s in loop %s\n", 
      buffer, WB_Whirl_Symbol(Enclosing_Loop(wn_def)));
  } 
  if (Cur_PU_Feedback) 
    count = WN_MAP32_Get(WN_MAP_FEEDBACK, wn_use);
  INT position = loop_ls->In(wn_use);
  if (rm != NULL) { 
    REDUCTION_TYPE red_type = rm->Which_Reduction(wn_use);
    if (red_type != RED_NONE) { 
      WN *wn;
      for (wn = wn_use; wn != NULL; wn = LWN_Get_Parent(wn))
	if (OPCODE_is_store(WN_opcode(wn)) 
	    && rm->Which_Reduction(wn) == red_type)     
	  break; 
      FmtAssert(wn != NULL, 
	("Could not find store to match reduction load."));
      rm->Erase(wn); 
    } 
  } 
  BOOL added_convert = FALSE;
  WN* wn_copy = Replace_Wnexp_With_Exp_Copy(wn_use, WN_kid0(wn_def), du,
    &added_convert); 
  LWN_Set_Frequency_Tree(wn_copy, count);
  WN* wn_true_copy = added_convert ? WN_kid0(wn_copy) : wn_copy; 
  INT count1 = Node_Count(WN_kid0(wn_def)); 
  INT count2 = Node_Count(wn_true_copy); 
  FmtAssert(count1 == count2, 
    ("FS_Array_Substitute: Counts do not match"));
  Fix_Access_Arrays_In_Copy_Block(wn_true_copy); 
  Fix_Deps_In_Copy_Block(loop_ls, WN_kid0(wn_def), wn_true_copy, position); 
}

//-----------------------------------------------------------------------
// NAME: AS_Traverse 
// FUNCTION: Traverses the tree starting at 'wn', performing forward 
//   and backward substitution.
// NOTE: 'loop_ls' stores the lexical positions of all of the nodes 
//   in the original outermost do loop for which we are substituting 
//   scalars. 
//-----------------------------------------------------------------------

static BOOL AS_Traverse(WN *wn, 
			LS_IN_LOOP* loop_ls) 
{ 
  BOOL substituted = FALSE; 
  switch (WN_operator(wn)) { 
  case OPR_BLOCK: 
    {
      WN* wn_tpp = NULL; 
      for (WN* wn_tp = WN_first(wn); wn_tp != NULL; wn_tp = wn_tpp) {
        wn_tpp = WN_next(wn_tp); 
        if (AS_Traverse(wn_tp, loop_ls)) 
	  substituted = TRUE;  
      } 
    }
    break;
  case OPR_DO_LOOP:
    { // Entering a scope for the creation of LS_IN_LOOP   
    if (loop_ls == NULL) 
      loop_ls = CXX_NEW(LS_IN_LOOP(wn, dg, &LNO_local_pool, TRUE), 
	&LNO_local_pool); 
    substituted = AS_Traverse(WN_do_body(wn), loop_ls);  
    loop_ls = NULL; 
    } // Closing a scope for the deletion of LS_IN_LOOP 
    break;
  case OPR_FUNC_ENTRY: 
    substituted = AS_Traverse(WN_func_body(wn), loop_ls); 
    break;
  case OPR_IF:
    {
      BOOL sub1 = AS_Traverse(WN_then(wn), loop_ls); 
      BOOL sub2 = AS_Traverse(WN_else(wn), loop_ls);
      substituted = sub1 || sub2; 
    }
    break;
  case OPR_DO_WHILE: 
  case OPR_WHILE_DO: 
    substituted = AS_Traverse(WN_while_body(wn), loop_ls);
    break;
  case OPR_STID:
    if (LNO_Forward_Substitution) { 
      WN* wn_loop = Enclosing_Loop(wn); 
      if (!(wn_loop != NULL 
	   && WN_operator(wn_loop) == OPR_DO_LOOP
	   && Do_Loop_Is_Good(wn_loop) &&
	   !Do_Loop_Has_Gotos(wn_loop)))
	break; 
      if (FS_Worthwhile(wn, wn_loop, loop_ls)) { 
	FS_Substitute(wn, loop_ls);
	substituted = TRUE;
      }
    }  
    break;
  case OPR_ISTORE:
    if (LNO_Backward_Substitution) {
      WN* wn_loop = Enclosing_Loop(wn); 
      if (wn_loop != NULL 
	  && WN_operator(wn_loop) == OPR_DO_LOOP
	  && Do_Loop_Is_Good(wn_loop) &&
	     !Do_Loop_Has_Gotos(wn_loop)) { 
	STACK<WN*>* scalar_stack = BS_Worthwhile(wn);
	if (scalar_stack != NULL) { 
	  BS_Substitute(wn, wn_loop, scalar_stack, loop_ls);
	  substituted = TRUE; 
	  break; 
	}
      } 
    } 
    if (LNO_Forward_Substitution) {
      STACK<WN*> st_use(&LNO_local_pool);  
      if (FS_Array_Worthwhile(wn, &st_use, loop_ls)) {
        if (st_use.Elements() == 1) { 
	  for (INT i = 0; i < st_use.Elements(); i++) 
	    FS_Array_Substitute(wn, st_use.Bottom_nth(i), loop_ls); 
	  substituted = TRUE; 
	  VINDEX16 v = dg->Get_Vertex(wn); 
	  if (v != 0)
	    Process_Store(wn, v, dg); 
	} 
      } 
    } 
  }
  return substituted;  
} 

//-----------------------------------------------------------------------
// NAME: Array_Substitution 
// FUNCTION: Forward substitute references for the function 'func_nd'.  
//-----------------------------------------------------------------------

extern void Array_Substitution(WN* func_nd)
{
  LS_IN_LOOP* loop_ls = NULL; 
  if (!LNO_Forward_Substitution && !LNO_Backward_Substitution) 
    return; 
  if (LNO_Verbose) { 
    fprintf(stdout, "Applying Array Substitution\n"); 
    fprintf(TFile, "Applying Array Substitution\n"); 
  } 

  dg = Array_Dependence_Graph; 
  du = Du_Mgr;
  rm = red_manager;  
  (void) AS_Traverse(func_nd, loop_ls); 
  if (LNO_Verbose) { 
    fprintf(stdout, "Array Substitution Complete\n"); 
    fprintf(TFile, "Array Substitution Complete\n"); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Crosses_Regions
// FUNCTION: Returns TRUE if 'wn1' and 'wn2' are nested inside a different 
//   number of regions, returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Crosses_Regions(WN* wn1, WN* wn2) 
{
  INT region_count1 = 0; 
  WN *wn;
  for (wn = wn1; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_REGION) 
      region_count1++; 
  INT region_count2 = 0; 
  for (wn = wn2; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_REGION) 
      region_count2++; 
  return region_count1 != region_count2; 
} 

//-----------------------------------------------------------------------
// NAME: Contains_ILoad_Without_Vertex
// FUNCTION: Returns TRUE if the expression rooted at 'wn_tree' contains 
//   a ILOAD without a vertex.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Contains_ILoad_Without_Vertex(WN* wn_tree)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  if (WN_operator(wn_tree) == OPR_ILOAD && !dg->Get_Vertex(wn_tree))
    return TRUE; 
  for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
    if (Contains_ILoad_Without_Vertex(WN_kid(wn_tree, i)))
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Exceeds_FS_Limit
// FUNCTION: Return TRUE if we should not forward substitute because the 
//   defining expression 'wn_node' is too large.  FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Exceeds_FS_Limit(WN* wn_node)
{
  const INT FS_NODE_COUNT = 100; 
  const INT FS_SYMBOL_COUNT = 10; 
  if (Node_Count(wn_node, FS_NODE_COUNT, FALSE) > FS_NODE_COUNT)
    return TRUE; 
  if (Node_Count(wn_node, FS_SYMBOL_COUNT, TRUE) > FS_SYMBOL_COUNT)
    return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Forward_Substitutable
// FUNCTION: Returns TRUE if the 'wn_ldid' has a unique definition which
//   can be forward substituted for it.  Returns FALSE not, or if we are
//   not sure.
//-----------------------------------------------------------------------

extern WN* Forward_Substitutable(WN* wn_ldid,
                                 DU_MANAGER* du)
{
  if (WN_operator(wn_ldid) != OPR_LDID)
    return NULL;
  if (ST_class(WN_st(wn_ldid)) == CLASS_PREG
      && WN_offset(wn_ldid) <= Last_Dedicated_Preg_Offset)
    return NULL;
  DEF_LIST *def_list = du->Ud_Get_Def(wn_ldid);
  if (def_list == NULL || def_list->Incomplete())
    return NULL;
  DEF_LIST_ITER iter(def_list);
  WN* wn_def = NULL;
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn = node->Wn();
    if (wn_def != NULL)
      return NULL;
    wn_def = wn;
  }
  if (WN_operator(wn_def) != OPR_STID
      || SYMBOL(wn_ldid) != SYMBOL(wn_def))
    return NULL;
  if (Contains_Dedicated_Preg(wn_def))
    return NULL;
  WN* wn_first = wn_def;
  WN* wn_last = Find_Sibling_Containing(wn_first, wn_ldid);
  if (wn_last == NULL) 
    return NULL; 
  if (Exceeds_FS_Limit(WN_kid0(wn_def)))    
    return NULL; 
  if (Maybe_Assigned_Exp(WN_kid0(wn_def), wn_first, wn_last))
    return NULL;
  if (Crosses_Regions(wn_def, wn_ldid))
    return NULL; 
  WN* wn_enclosing_loop = Enclosing_Proper_Do_Loop(wn_ldid);
  if (wn_enclosing_loop != NULL && Do_Loop_Is_Good(wn_enclosing_loop) 
      && Contains_ILoad_Without_Vertex(wn_def))
    return NULL; 
  return wn_def;
}

//-----------------------------------------------------------------------
// NAME: Fix_Deps_For_Load
// FUNCTION: Copy the dependences from 'wn_orig' to 'wn_copy', building 
//   an access array for 'wn_copy's ARRAY node, if it exists.  
//-----------------------------------------------------------------------

static void Fix_Deps_For_Load(WN* wn_orig,
			      WN* wn_copy)
{
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  OPERATOR op_orig = WN_operator(wn_orig);        
  OPERATOR op_copy = WN_operator(wn_copy);        
  FmtAssert(op_orig == op_copy 
    && (op_orig == OPR_LDID || op_orig == OPR_ILOAD),
    ("Fix_Deps_For_Load: Call with improper arguments"));
  DOLOOP_STACK copy_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_copy, &copy_stack);
  dg->Add_Vertex(wn_copy); 
  HASH_TABLE<WN*,INT> old_nodes(MIN(dg->Get_Edge_Count(), 512),
    &LNO_local_pool);
  EINDEX16 e = 0; 
  DOLOOP_STACK source_stack(&LNO_local_pool); 
  VINDEX16 v_orig = dg->Get_Vertex(wn_orig);
  for (e = dg->Get_In_Edge(v_orig); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
    old_nodes.Enter(wn_source, 1);
    Build_Doloop_Stack(wn_source, &source_stack);
    if (!dg->Add_Edge(wn_source, &source_stack, wn_copy,
      &copy_stack, Is_Lex_Before(wn_source, wn_orig))) { 
      LNO_Erase_Dg_From_Here_In(wn_copy, dg);
      return; 
    } 
    source_stack.Clear();
  }
  DOLOOP_STACK sink_stack(&LNO_local_pool);
  for (e = dg->Get_Out_Edge(v_orig); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
    if (old_nodes.Find(wn_sink) != 0)
      continue;
    Build_Doloop_Stack(wn_sink, &sink_stack);
    if (!dg->Add_Edge(wn_copy, &copy_stack, wn_sink,
      &sink_stack, Is_Lex_Before(wn_orig, wn_sink))) { 
      LNO_Erase_Dg_From_Here_In(wn_copy, dg);
      return; 
    } 
    sink_stack.Clear();
  }
}

//-----------------------------------------------------------------------
// NAME: Fix_Exp_Deps 
// FUNCTION: Clone the dependences of from the expression rooted at 'wn_orig' 
//   to the expression rooted at 'wn_copy'.  Build access arrays for 'wn_copy'
//   as needed.  Note that the dependences will not necessarily be identical
//   because 'wn_orig' and 'wn_copy' may have nodes at different loop levels. 
//-----------------------------------------------------------------------

static void Fix_Exp_Deps(WN* wn_orig, 
			 WN* wn_copy)
{ 
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  OPCODE op_orig = WN_opcode(wn_orig);        
  OPCODE op_copy = WN_opcode(wn_copy);        
  FmtAssert(op_orig == op_copy && OPCODE_is_expression(op_orig), 
    ("Fix_Exp_Deps: Opcodes do not match or not an expression")); 

  if (dg->Get_Vertex(wn_orig)) {
    Fix_Deps_For_Load(wn_orig, wn_copy); 
  } else if (WN_operator(wn_copy) == OPR_ILOAD) {
    WN* wn_loop = Enclosing_Proper_Do_Loop(wn_copy); 
    if (wn_loop != NULL && Do_Loop_Is_Good(wn_loop) && 
			!Do_Loop_Has_Gotos(wn_loop))
      dg->Add_Vertex(wn_copy); 
  } 
  for (INT i = 0; i < WN_kid_count(wn_orig); i++) {
    WN* wn_new_orig = WN_kid(wn_orig, i);     
    WN* wn_new_copy = WN_kid(wn_copy, i);     
    Fix_Exp_Deps(wn_new_orig, wn_new_copy); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Dead_Stid
// FUNCTION: Returns TRUE if 'wn_def' is a STID with no uses, FALSE if it
//   has uses, or we aren't sure. 
//-----------------------------------------------------------------------

static BOOL Dead_Stid(WN* wn_def)
{
  DU_MANAGER* du = Du_Mgr; 
  OPERATOR opr = WN_operator(wn_def);
  if (opr != OPR_STID) 
    return FALSE; 
  USE_LIST* use_list = du->Du_Get_Use(wn_def);  
  if (use_list == NULL) 
    return TRUE; 
  if (use_list->Incomplete())
    return FALSE; 
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = iter.First();
  if (node == NULL) 
    return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Forward_Substitute_Ldids
// FUNCTION: Forward substitutes each LDID in 'wn_exp' which has a single
//   definition which is simple enough.  Rebuild the access vectors of the
//   immediately enclosing loop or when an ldid is forward substituted.
//-----------------------------------------------------------------------

extern void Forward_Substitute_Ldids(WN* wn_exp,
                                     DU_MANAGER* du)
{
  FmtAssert(OPCODE_is_expression(WN_opcode(wn_exp)),
   ("wn_exp must be expression"));

  if (WN_operator(wn_exp) == OPR_LDID) {
    WN* wn_def = Forward_Substitutable(wn_exp, du);
    if (wn_def != NULL) {
      if (LNO_Verbose) { 
	fprintf(stdout, "FS: Forward substituting %s at 0x%p\n",
	  WB_Whirl_Symbol(wn_def), wn_def);
	fprintf(TFile, "FS: Forward substituting %s at 0x%p\n",
	  WB_Whirl_Symbol(wn_def), wn_def);
      } 
      WN* wn_orig = WN_kid0(wn_def); 
      WN* wn_copy = LWN_Copy_Tree(wn_orig);
      LWN_Copy_Def_Use(wn_orig, wn_copy, du); 
      WN* wn_parent = LWN_Get_Parent(wn_exp); 
      INT k;
      for (k = 0; k < WN_kid_count(wn_parent); k++) 
	if (WN_kid(wn_parent, k) == wn_exp)
	  break; 
      WN_kid(wn_parent, k) = wn_copy; 
      LWN_Set_Parent(wn_copy, wn_parent); 
      Fix_Access_Arrays_In_Copy_Block(wn_copy);
      Fix_Exp_Deps(wn_orig, wn_copy); 
      LWN_Delete_Tree(wn_exp); 
      if (Dead_Stid(wn_def)) { 
	LWN_Extract_From_Block(wn_def); 
        LWN_Delete_Tree(wn_def); 
      } 
      WN *wn;
      for (wn = wn_copy; wn != NULL; wn = LWN_Get_Parent(wn))
        if (WN_opcode(wn) == OPC_DO_LOOP || WN_opcode(wn) == OPC_IF)
          break;
      DOLOOP_STACK dostack(&LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn), &dostack);
      LNO_Build_Access(wn, &dostack, &LNO_default_pool);
      return; 
    }
  }

  for (INT i = 0; i < WN_kid_count(wn_exp); i++)
    Forward_Substitute_Ldids(WN_kid(wn_exp, i), du);
}

// return the MP loop/region surrounding wn, NULL if there is not
static WN *Find_MP(WN *wn)
{
  while (wn && !Is_Mp_Region(wn) && 
	  ((WN_opcode(wn) != OPC_DO_LOOP) || !Do_Loop_Is_Mp(wn))) {
    wn = LWN_Get_Parent(wn);
  }
  return wn;
}
