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


// -*-C++-*-

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <alloca.h>
#include "pu_info.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "config.h"
#include "debug.h"
#include "glob.h"
#include "lnopt_main.h"
#include "snl_utils.h" 
#include "reduc.h" 
#include "small_trips.h" 
#include "wind_down.h"
#include "forward.h" 

#define SPL_NO_SPLIT 	0 
#define SPL_SPLIT_INNER 1
#define SPL_SPLIT_ALL	2 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Is_Tile_Plus_Constant 
// FUNCTION: Returns TRUE if 'wn_exp' has the form 'wn_tile_symbol + 
//   const', FALSE otherwise. If TRUE, 'wn_tile_size' is set to the 
//   value of 'const + 1'.  
//-----------------------------------------------------------------------

static BOOL SNL_SPL_Is_Tile_Plus_Constant(WN* wn_exp,
					  SYMBOL wn_tile_symbol,  
					  INT* wn_tile_size) 
{ 
  if (WN_operator(wn_exp) == OPR_LDID
    && SYMBOL(wn_exp) == wn_tile_symbol) { 
    *wn_tile_size = 1; 
    return TRUE; 
  } 
  if (WN_operator(wn_exp) != OPR_ADD)
    return FALSE;
  WN* wn_constant = NULL;  
  if (WN_operator(WN_kid0(wn_exp)) == OPR_LDID 
    && SYMBOL(WN_kid0(wn_exp)) == wn_tile_symbol)
    wn_constant = WN_kid1(wn_exp); 
  else if (WN_operator(WN_kid1(wn_exp)) == OPR_LDID 
    && SYMBOL(WN_kid1(wn_exp)) == wn_tile_symbol) 
    wn_constant = WN_kid0(wn_exp); 
  if (wn_constant == NULL) 
    return FALSE; 
  if (WN_operator(wn_constant) != OPR_INTCONST) 
    return FALSE; 
  *wn_tile_size = WN_const_val(wn_constant) + 1;  
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Is_Multiple
// FUNCTION: Returns TRUE if 'wn_tree' is a multiple of 'tile_const', 
//   FALSE if we don't know or it isn't easy to determine this. 
//-----------------------------------------------------------------------

static BOOL Is_Multiple(WN* wn_tree, 
			INT tile_const) 
{ 
  if (tile_const == 1) 
    return TRUE; 
  if (WN_operator(wn_tree) == OPR_INTCONST) { 
    INT tree_const = WN_const_val(wn_tree); 
    return tree_const % tile_const == 0; 
  } 
  if (WN_operator(wn_tree) == OPR_MPY) { 
    if (Is_Multiple(WN_kid0(wn_tree), tile_const)  
        || Is_Multiple(WN_kid1(wn_tree), tile_const))
      return TRUE; 
  } 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Loop_Is_Inner_Tile 
// FUNCTION: Returns an outer tile loop if 'wn_loop' has the form of an 
//   inner tile loop, NULL otherwise.  If TRUE, 'wn_tile_symbol' is set to 
//   the symbol of the tile loop, and 'wn_tile_size' is set to the tile size. 
// NOTES: The form of an inner tile loop is: 
//   do i = tile_i, min(tile_i + const1, const2), 1  
//-----------------------------------------------------------------------

extern WN* SNL_SPL_Loop_Is_Inner_Tile(WN* wn_loop,
				      INT* tile_size) 
{
  // Must be good. 
  if (!Do_Loop_Is_Good(wn_loop) || Do_Loop_Has_Gotos(wn_loop))
    return NULL; 

  // Check the form of the start. 
  BOOL need_lb_fs = FALSE; 
  WN* wn_outer = NULL; 
  DU_MANAGER* du = Du_Mgr; 
  WN* wn_tile_load = WN_kid0(WN_start(wn_loop));
  if (WN_operator(wn_tile_load) != OPR_LDID) 
    return NULL; 
  WN* wn_first = LWN_Get_Parent(wn_loop);
  WN* wn = 0;
  for (wn = wn_first; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP 
        && SYMBOL(wn_tile_load) == SYMBOL(WN_index(wn)))
      break; 
  if (wn == NULL) {
    WN* wn_fs_tile_def = Forward_Substitutable(wn_tile_load, du);
    if (wn_fs_tile_def != NULL) { 
      wn_tile_load = WN_kid0(wn_fs_tile_def); 
      if (WN_operator(wn_tile_load) != OPR_LDID) 
        return NULL; 
      WN* wn_first = LWN_Get_Parent(wn_loop);
      WN* wn = 0;
      for (wn = wn_first; wn != NULL; wn = LWN_Get_Parent(wn)) 
	if (WN_opcode(wn) == OPC_DO_LOOP 
	    && SYMBOL(wn_tile_load) == SYMBOL(WN_index(wn)))
	  break;
      if (wn == NULL) 
	return NULL; 
      wn_outer = wn; 
      need_lb_fs = TRUE; 
    }
  } else { 
    wn_outer = wn; 
  }  
  if (wn_outer == NULL)
    return NULL; 
  SYMBOL wn_tile_sym(wn_tile_load);

  // Check the form of the stop expression.  
  BOOL need_ub_fs = FALSE; 
  Upper_Bound_Standardize(WN_end(wn_loop)); 
  WN* wn_le = WN_end(wn_loop); 
  FmtAssert(WN_operator(wn_le) == OPR_LE, 
    ("Did not standardize inner tile loop test.")); 
  WN* wn_ind = WN_kid0(wn_le);
  if (WN_operator(wn_ind) != OPR_LDID)
    return NULL;
  WN* wn_min = WN_kid1(wn_le);
  if (WN_operator(wn_min) == OPR_LDID) { 
    WN* wn_min_def = Forward_Substitutable(wn_min, du); 
    if (wn_min_def != NULL) {
      wn_min = WN_kid0(wn_min_def); 
      need_ub_fs = TRUE; 
    } 
  }  
  if (WN_operator(wn_min) != OPR_MIN) 
    return NULL;
  INT tile_const = 0; 
  WN* wn_tile_ub = NULL;  
  if (SNL_SPL_Is_Tile_Plus_Constant(WN_kid0(wn_min), wn_tile_sym, &tile_const))
    wn_tile_ub = LWN_Copy_Tree(WN_kid1(wn_min), TRUE, LNO_Info_Map);
  else if (SNL_SPL_Is_Tile_Plus_Constant(WN_kid1(wn_min), wn_tile_sym,  
      &tile_const))
    wn_tile_ub = LWN_Copy_Tree(WN_kid0(wn_min), TRUE, LNO_Info_Map);
  if (wn_tile_ub == NULL) 
    return NULL;

  // Check the form of the step. 
  INT wn_step = Step_Size(wn_loop); 
  if (wn_step != 1)    
    return NULL;

  // Check the outer tile step 
  if (!Do_Loop_Is_Good(wn_outer) || Do_Loop_Has_Gotos(wn_outer))
    return NULL; 
  BOOL need_outer_step_fs = FALSE; 
  Upper_Bound_Standardize(WN_end(wn_outer));
  WN* wn_outer_step = Loop_Step(wn_outer); 
  if (WN_operator(wn_outer_step) == OPR_LDID) { 
    WN* wn_step_def = Forward_Substitutable(wn_outer_step, du); 
    if (wn_step_def != NULL) { 
      need_outer_step_fs = TRUE;  
      wn_outer_step = WN_kid0(wn_step_def); 
    }
  }    
  if (!Is_Multiple(wn_outer_step, tile_const))
    return FALSE; 

  // Perform forward substitution as needed 
  if (need_lb_fs)
    Forward_Substitute_Ldids(WN_kid0(WN_start(wn_loop)), du);
  if (need_ub_fs)
    Forward_Substitute_Ldids(WN_kid1(WN_end(wn_loop)), du);
  if (need_outer_step_fs)
    Forward_Substitute_Ldids(Loop_Step(wn_outer), du);

  *tile_size = tile_const; 
  return wn_outer; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Build_Loop_List 
// FUNCTION: For the list of nodes between 'wn_first' and 'wn_last' build 
//   three stacks 'inner_do_stack', 'outer_do_stack', and 'tile_size_stack'.
//   The i-th entry in each of these stacks corresponds to a pair of inner 
//   and outer tile loops and the tile size. 'split_flag' controls which 
//   loops are candidates to be stacked: 
//     split_flag == SPL_SPLIT_ALL => stack all tile pairs 
//     split_flag != SPL_SPLIT_ALL => stack only combinations when the 
//                                    inner loop is innermost.  
//-----------------------------------------------------------------------

static void SNL_SPL_Build_Loop_List(WN* wn_first, 
				    WN* wn_last,  
				    DOLOOP_STACK* inner_do_stack, 
				    DOLOOP_STACK* outer_do_stack, 
				    STACK<INT>* tile_size_stack, 
				    INT split_flag)
{
  if (wn_first == NULL) 
    return; 

  for (WN* wn = wn_first; ; wn = WN_next(wn)) { 
    switch(WN_opcode(wn)) { 
    case OPC_DO_LOOP:
      {
        if (split_flag == SPL_SPLIT_ALL || Get_Do_Loop_Info(wn)->Is_Inner) {
          SYMBOL wn_tile_symbol(WN_start(wn)); 
          INT tile_size = 0;  
          WN* wn_outer = SNL_SPL_Loop_Is_Inner_Tile(wn, &tile_size); 
	  if (wn_outer != NULL) {
	    inner_do_stack->Push(wn); 
	    outer_do_stack->Push(wn_outer);
	    tile_size_stack->Push(tile_size); 
	  }
          if (Get_Do_Loop_Info(wn)->Is_Inner)  
            return; 
        }          
        WN* wn_newfirst = WN_first(WN_do_body(wn)); 
        WN* wn_newlast = WN_last(WN_do_body(wn)); 
        SNL_SPL_Build_Loop_List(wn_newfirst, wn_newlast, inner_do_stack, 
	  outer_do_stack, tile_size_stack, split_flag);
      }
      break;
    case OPC_IF: 
      SNL_SPL_Build_Loop_List(WN_first(WN_then(wn)), WN_last(WN_then(wn)),
	inner_do_stack, outer_do_stack, tile_size_stack, split_flag);    
      SNL_SPL_Build_Loop_List(WN_first(WN_else(wn)), WN_last(WN_else(wn)),
	inner_do_stack, outer_do_stack, tile_size_stack, split_flag);    
      break;
    case OPC_DO_WHILE: 
    case OPC_WHILE_DO:
      SNL_SPL_Build_Loop_List(WN_first(WN_while_body(wn)), 
	WN_last(WN_while_body(wn)), inner_do_stack, outer_do_stack, 
	tile_size_stack, split_flag);
      break;
    }  
    if (wn == wn_last) 
      break; 
  } 
}

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Sort_Stack 
// FUNCTION: Sorts the three stacks 'outer_do_stack', 'inner_do_stack', 
//   and 'tile_size_stack' using the value of 'outer_do_stack' as the key.  
//-----------------------------------------------------------------------

static void SNL_SPL_Sort_Stack(DOLOOP_STACK* outer_do_stack, 
			       DOLOOP_STACK* inner_do_stack,
			       STACK<INT>* tile_size_stack) 
{
  for (INT i = 0; i < outer_do_stack->Elements(); i++) { 
    INT compare_tag = (INT)(INTPTR)outer_do_stack->Bottom_nth(i); 
    for (INT j = i + 1; j < outer_do_stack->Elements(); j++) { 
      INT current_tag = (INT)(INTPTR)outer_do_stack->Bottom_nth(j); 
      if (current_tag < compare_tag) { 
	WN* outer_loop = outer_do_stack->Bottom_nth(i); 
	outer_do_stack->Bottom_nth(i) = outer_do_stack->Bottom_nth(j); 
	outer_do_stack->Bottom_nth(j) = outer_loop; 
	WN* inner_loop = inner_do_stack->Bottom_nth(i); 
	inner_do_stack->Bottom_nth(i) = inner_do_stack->Bottom_nth(j); 
	inner_do_stack->Bottom_nth(j) = inner_loop; 
	INT tile_size = tile_size_stack->Bottom_nth(i); 
	tile_size_stack->Bottom_nth(i) = tile_size_stack->Bottom_nth(j); 
	tile_size_stack->Bottom_nth(j) = tile_size; 
      } 
    } 
  }  
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Get_Tile_Loops 
// FUNCTION: Given the sorted three stacks 'outer_do_stack', 'inner_do_
//   stack', and 'tile_size_stack', set 'outer_tile_loop' to the first 
//   outer tile loop and 'inner_tile_stack' to the corresponding inner 
//   tiles. Set 'tile_size' to the common tile size of these loops. 
//   Also update 'outer_do_stack', 'inner_do_stack', and 'tile_size_stack'
//   by removing all entries corresponding to this 'outer_tile_loop' and  
//   the inner tiles on 'inner_tile_stack'. 
//-----------------------------------------------------------------------

static void SNL_SPL_Get_Tile_Loops(DOLOOP_STACK* outer_do_stack, 
  				   DOLOOP_STACK* inner_do_stack, 
				   STACK<INT>* tile_size_stack, 
  				   WN** outer_tile_loop, 
      				   DOLOOP_STACK* inner_tile_stack,
				   INT* tile_size) 
{
  *outer_tile_loop = NULL; 
  inner_tile_stack->Clear(); 
  if (outer_do_stack->Elements() == 0) 
    return;
  *outer_tile_loop = outer_do_stack->Top(); 
  *tile_size = tile_size_stack->Top();  
  while (outer_do_stack->Elements() > 0 
      && outer_do_stack->Top() == *outer_tile_loop) {
    inner_tile_stack->Push(inner_do_stack->Top());
    outer_do_stack->Pop(); 
    inner_do_stack->Pop(); 
    FmtAssert(tile_size_stack->Top() == *tile_size, 
      ("SNL_SPL_Get_Tile_Loops(): Inner tiles with nonmatching tile sizes"));
    tile_size_stack->Pop(); 
  }  
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Make_Inner_Tile_Stack 
// FUNCTION: The tree starting at 'wn_root' has a set of inner tile loops 
//   in 'stack'.  The tree 'cp_wn_root' is a copy of the tree 'wn_root'.
//   Initially 'cp_stack' is the same size as 'stack' but has NULL elements.  
//   Replace the tile loops in 'cp_stack' with the corresponding tile loops 
//    in 'cp_wn_root'. 
//-----------------------------------------------------------------------

static void SNL_SPL_Make_Inner_Tile_Stack(WN* wn_root, 
                                          DOLOOP_STACK* stack,
                                          WN* cp_wn_root,
                                          DOLOOP_STACK* cp_stack)
{
  if (wn_root == NULL) 
    return; 

  switch (WN_opcode(wn_root)) { 
  case OPC_DO_LOOP: 
    {
      INT i;
      for (i = 0; i < stack->Elements(); i++)  
        if (stack->Bottom_nth(i) == wn_root) 
	  break; 
      if (i < stack->Elements()) 
        cp_stack->Bottom_nth(i) = cp_wn_root; 
      SNL_SPL_Make_Inner_Tile_Stack(WN_do_body(wn_root), stack, 
        WN_do_body(cp_wn_root), cp_stack); 
    }
    break; 
  case OPC_IF: 
    SNL_SPL_Make_Inner_Tile_Stack(WN_then(wn_root), stack, 
      WN_then(cp_wn_root), cp_stack); 
    SNL_SPL_Make_Inner_Tile_Stack(WN_else(wn_root), stack, 
      WN_else(cp_wn_root), cp_stack); 
    break; 
  case OPC_DO_WHILE: 
  case OPC_WHILE_DO:
    SNL_SPL_Make_Inner_Tile_Stack(WN_while_body(wn_root),
      stack, WN_while_body(cp_wn_root), cp_stack);
    break;
  case OPC_BLOCK:
    WN* cp_wn = WN_first(cp_wn_root);  
    for (WN* wn = WN_first(wn_root); wn != NULL; wn = WN_next(wn)) { 
      SNL_SPL_Make_Inner_Tile_Stack(wn, stack, cp_wn, 
 	cp_stack);  
      cp_wn = WN_next(cp_wn);
    }
    break;  
  }   
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Copy_Inner_Tile_Stack 
// FUNCTION: The tree starting at 'outer_tile_loop' has a set of inner tile 
//   loops in 'stack'.  The tree 'cp_outer_tile_loop' is a copy of the tree 
//   'outer_tile_loop'. Fill in 'cp_inner_tile_stack' with the tile loops 
//   which correspond to the ones in 'inner_tile_stack' but appear in 
//   'cp_outer_tile_loop'. 
//-----------------------------------------------------------------------

static void SNL_SPL_Copy_Inner_Tile_Stack(WN* outer_tile_loop, 
					  DOLOOP_STACK* inner_tile_stack, 
					  WN* cp_outer_tile_loop, 
				 	  DOLOOP_STACK* cp_inner_tile_stack)
{ 
  WN* null_addr = NULL; 
  cp_inner_tile_stack->Clear(); 
  for (INT i = 0; i < inner_tile_stack->Elements(); i++) 
    cp_inner_tile_stack->Push(null_addr); 
  SNL_SPL_Make_Inner_Tile_Stack(outer_tile_loop, inner_tile_stack, 
    cp_outer_tile_loop, cp_inner_tile_stack);  
}  

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Fix_First_Outer_Loop_Limits 
// FUNCTION: Change the limits on the original outer tile loop 
//   'outer_tile_loop' from: 
//     do tile_i = lb, ub, tile_size  
//   to: 
//     do tile_i = lb, ub - (tile_size + 1), step 
//-----------------------------------------------------------------------

static void SNL_SPL_Fix_First_Outer_Loop_Limits(WN* outer_tile_loop,
						INT tile_size)
{ 
  SYMBOL sym_index(WN_start(outer_tile_loop)); 
  INT64 outer_step = tile_size - 1; 
  WN* diff = LWN_Make_Icon(sym_index.Type, outer_step); 
  OPCODE sub_opc = OPCODE_make_op(OPR_SUB, sym_index.Type, MTYPE_V); 
  WN* new_ub = LWN_CreateExp2(sub_opc, WN_kid1(WN_end(outer_tile_loop)), 
    diff); 
  WN_kid1(WN_end(outer_tile_loop)) = new_ub; 
  LWN_Set_Parent(WN_kid1(WN_end(outer_tile_loop)), WN_end(outer_tile_loop));
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Fix_Second_Outer_Loop_Limits 
// FUNCTION: Change the limits on the copy of the outer tile loop 
//   'cp_outer_tile_loop' which is a copy of 'outer_tile_loop' from: 
//     do tile_i_new = lb, ub, tile_size  
//   to: 
//     do tile_i_new = tile_i, ub, tile_size  
//-----------------------------------------------------------------------

static void SNL_SPL_Fix_Second_Outer_Loop_Limits(WN* outer_tile_loop, 
						 WN* cp_outer_tile_loop,
					         INT tile_size)
{ 
  TYPE_ID wtype = WN_desc(WN_start(cp_outer_tile_loop)); 
  WN* newbegin = WN_start(cp_outer_tile_loop);
  LWN_Delete_Tree(WN_kid0(newbegin));
  WN_kid0(newbegin) = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, wtype, wtype),
    WN_start(outer_tile_loop));
  LWN_Copy_Frequency(newbegin,outer_tile_loop);
  LWN_Set_Parent(WN_kid0(newbegin), newbegin);
  Fix_Do_Du_Info(newbegin, NULL, FALSE, outer_tile_loop, 0);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(cp_outer_tile_loop);
  dli->LB->Too_Messy = TRUE;
  dli->Est_Num_Iterations = tile_size / 2;
  dli->Num_Iterations_Symbolic = FALSE;    
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Fix_Inner_Loop_Limits 
// FUNCTION: Change the limits on the inner tile loops in the stack: 
//   'inner_tile_stack' from:  
//     do i = tile_i, min(tile_i + tile_size - 1, ub), 1 
//   to: 
//     do i = tile_i, tile_i + tile_size - 1, 1 
//   if 'want_normal' is TRUE and to: 
//     do i = tile_i, ub, 1      
//   if 'want_normal' is FALSE.  
//-----------------------------------------------------------------------

static void SNL_SPL_Fix_Inner_Loop_Limits(DOLOOP_STACK* inner_tile_stack, 
					  BOOL want_normal) 
{
  for (INT i = 0; i < inner_tile_stack->Elements(); i++) { 
    WN* wn_inner = inner_tile_stack->Bottom_nth(i); 
    WN* wn_min = WN_kid1(WN_end(wn_inner)); 
    FmtAssert(WN_operator(wn_min) == OPR_MIN, 
      ("Could not find MIN in test of inner tile loop."));
    INT wn_const = 0;  
    SYMBOL wn_tile_sym(WN_kid0(WN_start(wn_inner))); 
    WN* wn_normal = NULL;  
    WN* wn_last = NULL; 
    if (SNL_SPL_Is_Tile_Plus_Constant(WN_kid0(wn_min), wn_tile_sym, 
        &wn_const)) { 
      wn_normal = WN_kid0(wn_min);
      wn_last = WN_kid1(wn_min);  
    } else if (SNL_SPL_Is_Tile_Plus_Constant(WN_kid1(wn_min), wn_tile_sym, 
	&wn_const)) { 
      wn_normal = WN_kid1(wn_min);
      wn_last = WN_kid0(wn_min);
    } 
    WN* wn_want = want_normal ? wn_normal : wn_last;  
    FmtAssert(wn_normal != NULL, 
      ("Could not find normal branch in inner tile loop."));
    WN* wn_new = LWN_Copy_Tree(wn_want, TRUE, LNO_Info_Map);  
    LWN_Copy_Def_Use(wn_want, wn_new, Du_Mgr); 
    LWN_Copy_Frequency_Tree(wn_new,WN_kid1(WN_end(wn_inner)));
    LWN_Delete_Tree(WN_kid1(WN_end(wn_inner))); 
    WN_kid1(WN_end(wn_inner)) = wn_new; 
    LWN_Set_Parent(wn_new, WN_end(wn_inner));
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_inner);
    if (want_normal) {  
      dli->Est_Num_Iterations = wn_const; 
      dli->Num_Iterations_Symbolic = FALSE;  
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Propagate_Tiled_Lower_Bound 
// FUNCTION: If the upper and lower bounds of 'outer_tile_loop' are 
//   constant, change the initial value of 'inner_tile_loop' from 
//   to a constant value, namely: 
//	lower_bound + iteration_count * step_size 
//   of the 'outer_tile_loop'.  
//-----------------------------------------------------------------------

static void SNL_SPL_Propagate_Tiled_Lower_Bound(WN* outer_tile_loop, 
						WN* inner_tile_loop)
{
  WN* lower_bound = WN_kid0(WN_start(outer_tile_loop));
  WN* upper_bound = SNL_UBexp(WN_end(outer_tile_loop));
  WN* step = Loop_Step(outer_tile_loop); 
  if (WN_operator(lower_bound) == OPR_INTCONST
      && WN_operator(upper_bound) == OPR_INTCONST
      && WN_operator(step) == OPR_INTCONST) {
    INT64 iters = Iterations(outer_tile_loop, &LNO_local_pool);
    INT64 lb = WN_const_val(lower_bound);
    INT64 ub = WN_const_val(upper_bound);
    INT64 u = Step_Size(outer_tile_loop); 
    INT64 wdlb = lb + iters * u;
    WN* wd_lower_bound = LWN_Copy_Tree(lower_bound, TRUE, LNO_Info_Map);
    LWN_Copy_Frequency(wd_lower_bound,WN_start(inner_tile_loop));
    LWN_Delete_Tree(WN_kid0(WN_start(inner_tile_loop)));
    WN_kid0(WN_start(inner_tile_loop)) = wd_lower_bound;
    LWN_Set_Parent(wd_lower_bound, WN_start(inner_tile_loop));
    WN_const_val(wd_lower_bound) = wdlb;
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Split_Tile_Sets 
// FUNCTION: Given an outer tile loop 'outer_tile_loop' and its corres- 
//   ponding iner tile loops on 'inner_tile_stack', change the original: 
//     do tile_i = lb, ub, tile_size 
//       do i = tile_i, min(tile_i + tile_size - 1, ub), 1 
//   to: 
//     do tile_i = lb, ub - (tile_size - 1), tile_size 
//       do i = tile_i, tile_i + tile_size - 1, 1 
//     do tile_i_new = tile_i, ub, tile_size  
//       do i = tile_i_new, ub, 1 
//   Intervening loops and multiple inner tiles for a single outer tile 
//   are handled.  The index variable 'tile_i_new' will have a name with 
//   the given 'prefix[]'.  If 'cache_annotate' is TRUE, we are creating a 
//   cache winddown loop, otherwise we are creating a winddown loop for an 
//   MP loop with interleaved scheduling.
//-----------------------------------------------------------------------

static BOOL SNL_SPL_Split_Tile_Sets(WN* outer_tile_loop, 
				    DOLOOP_STACK* inner_tile_stack,
				    INT tile_size, 
				    const char prefix[], 
				    BOOL cache_annotate) 
{ 
   ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
   DOLOOP_STACK cp_inner_tile_stack(&LNO_local_pool); 

   // Copy the original loop. 
   WN* cp_outer_tile_loop = LWN_Copy_Tree(outer_tile_loop, TRUE, LNO_Info_Map); 
   SNL_SPL_Copy_Inner_Tile_Stack(outer_tile_loop, inner_tile_stack, 
     cp_outer_tile_loop, &cp_inner_tile_stack); 
   
   // Adjust the frequency
   if (Cur_PU_Feedback) {
     INT64 size = tile_size;
     INT32 loop_count = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_start(outer_tile_loop));
     INT32 trip_count = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_step(outer_tile_loop));
     if (loop_count>0) {
       INT32 trip = MAX(1,trip_count/loop_count);
       float ratio = 1.0/(float) trip;
       float ratio1 = 1.0-ratio;
       INT32 adjust= size*loop_count;
       LWN_Scale_Frequency_Tree(WN_do_body(outer_tile_loop),ratio1);
       LWN_Scale_Frequency(WN_step(outer_tile_loop),ratio1);

       LWN_Scale_Frequency_Tree(WN_do_body(cp_outer_tile_loop),ratio);
       LWN_Scale_Frequency(WN_step(cp_outer_tile_loop),ratio);
     }
   }

   // Put the copy in the program tree. 
   LWN_Insert_Block_After(LWN_Get_Parent(outer_tile_loop), 
     outer_tile_loop, cp_outer_tile_loop); 

   // Update the dependence information. 
   WN* wn_holder[2];
   wn_holder[0] = outer_tile_loop; 
   wn_holder[1] = cp_outer_tile_loop; 
   if (!dg->Add_Deps_To_Copy_Block(outer_tile_loop, cp_outer_tile_loop, TRUE)) {
     LWN_Update_Dg_Delete_Tree(cp_outer_tile_loop, dg);
     LWN_Delete_Tree(cp_outer_tile_loop); 
     return FALSE; 
   }
   INT depth = Do_Depth(outer_tile_loop); 
   HASH_TABLE<VINDEX16,VINDEX16>
     hash_table(MIN(dg->Get_Vertex_Count(), 512), &LNO_local_pool);
   Wind_Down_Dep_V(outer_tile_loop, cp_outer_tile_loop, &hash_table, dg);
   if (!Wind_Down_Dep_E(&hash_table, Do_Depth(outer_tile_loop), dg)) { 
     LWN_Update_Dg_Delete_Tree(cp_outer_tile_loop, dg);
     LWN_Delete_Tree(cp_outer_tile_loop); 
     return FALSE; 
   } 

   // Update the reduction information.  
   if (red_manager) 
     red_manager->Unroll_Update(wn_holder, 2);

   // Update the UD information. 
   Unrolled_DU_Update(wn_holder, 2, Do_Loop_Depth(outer_tile_loop) - 1, 
     TRUE, FALSE);

   // Replace the index variable in the new loop.        
   Replace_Index_Variable(outer_tile_loop, cp_outer_tile_loop, prefix); 
 
   // Update the loop limits on the outer do loops. 
   SNL_SPL_Fix_First_Outer_Loop_Limits(outer_tile_loop, tile_size); 
   SNL_SPL_Fix_Second_Outer_Loop_Limits(outer_tile_loop, cp_outer_tile_loop,
     tile_size); 
   if (cache_annotate) 
     Set_Winddown_Annotations(cp_outer_tile_loop, 1, EST_REGISTER_USAGE(), 
       TRUE); 
   
   // Update the loop limits on the inner do loops. 
   SNL_SPL_Fix_Inner_Loop_Limits(inner_tile_stack, 1); 
   SNL_SPL_Fix_Inner_Loop_Limits(&cp_inner_tile_stack, 0); 

   // Eliminate the copied outer tile loop, if it is unity trip. 
   SNL_REGION region(cp_outer_tile_loop, cp_outer_tile_loop); 
   SNL_SPL_Propagate_Tiled_Lower_Bound(outer_tile_loop, cp_outer_tile_loop); 
   if (Iterations(cp_outer_tile_loop, &LNO_local_pool) == 1)  
     Remove_Unity_Trip_Loop(cp_outer_tile_loop, FALSE, &(region.First), 
       &(region.Last), Array_Dependence_Graph, Du_Mgr);  

   // Fix the doloop info.
   SNL_Rebuild_Access_Arrays(outer_tile_loop);
   for (WN* wn = region.First; ; wn = WN_next(wn)) {  
     SNL_Rebuild_Access_Arrays(wn);
     if (wn == region.Last) 
       break;
   }

   // Record the optimization. 
   if (LNO_Verbose) {
     fprintf(stdout, "Splitting cache tiles: (");
     INT i;
     for (i = 0; i < inner_tile_stack->Elements(); i++) { 
       fprintf(stdout, "0x%p", inner_tile_stack->Bottom_nth(i));  
       if (i < inner_tile_stack->Elements() - 1)
	 fprintf(stdout, ",");
     }
     fprintf(stdout, ")\n");
     fprintf(TFile, "Splitting cache tiles: (");
     for (i = 0; i < inner_tile_stack->Elements(); i++) { 
       fprintf(TFile, "0x%p", inner_tile_stack->Bottom_nth(i));  
       if (i < inner_tile_stack->Elements() - 1)
	 fprintf(TFile, ",");
     }
     fprintf(TFile, ")\n");
   }
   return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Split_Inner_Tile_Loop 
// FUNCTION: Perform the optimization described in 'SNL_SPL_Split_Tile_Sets'
//   on the pair of outer and inner loops 'wn_outer' and 'wn_inner' which 
//   have tile size 'tile_size'.  The name of the index variable of the wind 
//   down loop will have the given 'prefix'.  If 'cache_annotate' is TRUE, 
//   we are creating a cache winddown loop, otherwise we are creating a 
//   winddown loop for an MP loop with interleaved scheduling.
//-----------------------------------------------------------------------

extern BOOL SNL_SPL_Split_Inner_Tile_Loop(WN* wn_outer, 
					  WN* wn_inner,
					  INT tile_size, 
					  const char prefix[],
					  BOOL cache_annotate) 
{
  DOLOOP_STACK inner_do_stack(&LNO_local_pool); 
  inner_do_stack.Push(wn_inner); 
  return SNL_SPL_Split_Tile_Sets(wn_outer, &inner_do_stack, tile_size, 
    prefix, cache_annotate); 
} 

//-----------------------------------------------------------------------
// NAME: SNL_SPL_Split_Inner_Tile_Loops 
// FUNCTION: Perform the optimization described in 'SNL_SPL_Split_Tile_Sets'
//   on the region between 'wn_first' and 'wn_last' inclusive.  The flag 
//   'split_flag' controls the extent of the optimization.  Possible values 
//   are: 
// 	SPL_NO_SPLIT => do not perform this optimization 
// 	SPL_SPLIT_INNER => perform this only when the inner tile loop 
//	  is an innermost loop
//	SPL_SPLIT_ALL => perform this optimization whenever possible  
//  The name of the index variable of the wind down loop will have the 
//  given 'prefix'.  If 'cache_annotate' is TRUE, we are creating a cache 
//  winddown loop, otherwise we are creating a winddown loop for an MP 
//  loop with interleaved scheduling.
//-----------------------------------------------------------------------

extern void SNL_SPL_Split_Inner_Tile_Loops(WN* wn_first, 
					   WN* wn_last, 
				           INT split_flag,
					   const char prefix[],
					   BOOL cache_annotate) 
{
  WN* outer_tile_loop; 
  INT tile_size = 0; 
  DOLOOP_STACK inner_do_stack(&LNO_local_pool); 
  DOLOOP_STACK outer_do_stack(&LNO_local_pool); 
  DOLOOP_STACK inner_tile_stack(&LNO_local_pool); 
  STACK<INT> tile_size_stack(&LNO_local_pool); 
  if (split_flag == SPL_NO_SPLIT) 
    return;  

  inner_do_stack.Clear(); 
  outer_do_stack.Clear(); 
  SNL_SPL_Build_Loop_List(wn_first, wn_last, &inner_do_stack, &outer_do_stack, 
    &tile_size_stack, split_flag);
  SNL_SPL_Sort_Stack(&outer_do_stack, &inner_do_stack, &tile_size_stack); 
  while (outer_do_stack.Elements() != 0) {  
    SNL_SPL_Get_Tile_Loops(&outer_do_stack, &inner_do_stack, &tile_size_stack, 
      &outer_tile_loop, &inner_tile_stack, &tile_size); 
    if (LNO_Split_Tiles_Size > 0 && tile_size > LNO_Split_Tiles_Size) 
      continue; 
    BOOL success = SNL_SPL_Split_Tile_Sets(outer_tile_loop, 
      &inner_tile_stack, tile_size, prefix, cache_annotate); 
    if (!success) 
      return; 
  } 
}     

