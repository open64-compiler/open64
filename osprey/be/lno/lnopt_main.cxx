/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//
// -*-C++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: lnopt_main.c
 * $Revision: 1.32 $
 * $Date: 05/05/26 10:27:49-07:00 $
 * $Author: kannann@iridot.keyresearch $
 * $Source: be/lno/SCCS/s.lnopt_main.cxx $
 *
 * Revision history:
 *  14-Sep-94 - Original Version 
 *
 * Description:
 *
 * The main part of the Whirl Loop Nest Optimizer
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "defs.h"
#include "config_cache.h"
#include "config_list.h"
#include "config_lno.h"
#include "config_opt.h"
#include "erbe.h"
#include "glob.h"		    /* Irb_File_Name, Cur_PU_Name */
#include "wn.h"
#include "timing.h"
#include "wn_simp.h"
#include "ir_reader.h"
#include "lnoptimizer.h"
#include "opt_du.h"			/* Du_Built() */
#include "wn_pragmas.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "dep_graph.h"
#include "fission.h"
#include "fusion.h"
#include "ff_utils.h"
#include "ff_pragmas.h"
#include "fiz_fuse.h"
#include "fis_gthr.h"
#include "inner_fission.h"
#include "snl.h"
#include "prefetch.h"
#include "reduc.h"
#include "soe.h"
#include "cond.h"
#include "lnopt_main.h"
#include "config.h"
#include "be_util.h"
#include "aequiv.h"
#include "sclrze.h"
#include "dead.h"
#include "minvariant.h"
#include "outer.h"
#include "lego.h"
#include "lego_util.h"
#include "lego_opts.h"
#include "cxx_graph.h"
#include "model.h"
#include "forward.h"
#include "debug.h"
#include "cse.h"
#include "stblock.h"
#include "strtab.h"
#include "reverse.h"
#include "tile.h"
#include "permute.h"
#include "lego_skew.h"
#include "array_bounds.h"
#include "small_trips.h"
#include "parallel.h"
#include "ara.h"
#include "doacross.h"
#include "autod.h"
#include "parids.h"
#include "call_info.h"
#include "ifminmax.h"
#include "shackle.h"
#include "ipa_lno_info.h"
#include "ipa_lno_file.h"
#include "ipa_lno_summary.h"
#include "ipa_section.h"
#include "lnodriver.h"
#include "ipa_lno_read.h"
#include "wn_lower.h"
#include "array_copy.h"

extern WN *Convert_Intrinsic_To_Alloca_Dealloca (WN *wn);
extern BOOL Phase_123(PU_Info* current_pu, WN* func_nd, 
		      BOOL do_fiz_fuse, BOOL do_phase25,
                      BOOL do_inner_fission);

typedef STACK<WN *> STACK_OF_WN;
  /* ====================================================================
   *
   * Loop Nest Optimizer - Return an optimized tree
   *
   * ====================================================================
   */

  // Each statement/hcf maps a pointer to its parent
  WN_MAP Parent_Map;

  // A map to keep track of deleted loop (because of unroll, etc)
  HASH_TABLE<WN*,BOOL> *Deleted_Loop_Map;

  // Each array maps an ACCESS_ARRAY
  // Each do loop maps a BOUNDS
  // Each ldid maps an integer giving an id if it's the base of a scalar
  // expanded array (TODO: This is a hack until preopt is done).
  WN_MAP LNO_Info_Map;

  // Each load/store maps to vertex in the array dependence graph
  // Each load/store maps to vertex in CG's array dependence graph
  // (only of the above two is alive at any given time)
  WN_MAP Array_Dependence_Map;

  // Map pre-computation use to its def.
  WN_MAP LNO_Precom_Map;

  // Each statement maps to vertex in the statement dependence graph
  WN_MAP Stmt_Dependence_Map;

  // Whether to do aggressive loop fusion to bring defs and uses in the same loop.
  BOOL Do_Aggressive_Fuse;

  MEM_POOL LNO_default_pool;
  MEM_POOL LNO_local_pool;
  static BOOL lno_mempool_initialized = FALSE;
  FILE* LNO_Analysis;
  BOOL LNO_Tlog=FALSE;

  DU_MANAGER *Du_Mgr;
  ALIAS_MANAGER *Alias_Mgr;
  REDUCTION_MANAGER *red_manager;
  ARRAY_DIRECTED_GRAPH16 *Array_Dependence_Graph;  // LNO dependence graph
  INT snl_debug = 0; 
  FILE *STDOUT;
  BOOL Contains_MP = FALSE;

  BOOL LNO_enabled = TRUE;

  PERMUTATION_ARRAYS *Permutation_Arrays;

  void Eliminate_Zero_Mult(WN *wn, ARRAY_DIRECTED_GRAPH16 *dep_graph);
  extern void Pad_Degenerates();
  extern void Pad_First_Dim_Degenerates(WN *func_nd);

  extern BOOL Build_Array_Dependence_Graph (WN* func_nd);
  extern void Build_CG_Dependence_Graph (WN* func_nd);
  extern void Build_CG_Dependence_Graph (ARRAY_DIRECTED_GRAPH16*);

  WN* Current_Func_Node = NULL; 
  BOOL  LNO_Allow_Delinearize = TRUE; // this is true for lno but not
				      // ipl

static BOOL
Unroll_before_Factorize(WN *wn);

static 
void Outer_Unroll_For_Factorization(WN *func_nd, STACK_OF_WN *inner_do_stack);

extern void
Unroll_Loop_By_Trip_Count(WN* outerloop, INT u);

// How many iterations in a loop? Returns -1 if not constant
static INT64 
Num_Iters(WN* loop)
{
  INT64 stepsz = Step_Size(loop);
  if (stepsz == 0) {
    return -1;
  }


  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
  if (dli->LB->Num_Vec() > 1 || dli->UB->Num_Vec() > 1) {
    return -1;
  }
  ACCESS_VECTOR* ub = dli->UB->Dim(0);
  ACCESS_VECTOR* lb = dli->LB->Dim(0);
   
  INT64 rval = -1;
  MEM_POOL_Push(&LNO_local_pool);
  ACCESS_VECTOR* sum = Add(lb, ub, &LNO_local_pool);
  if (sum->Is_Const()) {
    if (stepsz < 0) {
      stepsz = -stepsz;
    }
    rval = sum->Const_Offset >= 0 ? (sum->Const_Offset + stepsz)/stepsz : 0;
  }
  MEM_POOL_Pop(&LNO_local_pool);
  return rval;
}

void Lower_To_Memlib_Walk(WN *block, WN *wn)
{
  OPCODE opc=WN_opcode(wn);
  
  if (!OPCODE_is_scf(opc))
    return;
  else if (opc==OPC_DO_LOOP) {
    if (Do_Loop_Is_Good(wn) && Do_Loop_Is_Inner(wn) && !Do_Loop_Has_Calls(wn)
        && !Do_Loop_Has_Gotos(wn)) {
       WN* result = Lower_Memlib(NULL, wn, LOWER_TO_MEMLIB, Alias_Mgr);
       if (WN_opcode(result) != OPC_DO_LOOP)
       {
         LWN_Insert_Block_Before(block,wn,result);
         LWN_Parentize(block);
         LWN_Delete_DU(wn);
         LWN_Delete_Tree(wn);
       }
    } else
      Lower_To_Memlib_Walk(block,WN_do_body(wn));
  } else if (opc==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      Lower_To_Memlib_Walk(wn, stmt); 
      stmt=next_stmt;
    } 
  else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Lower_To_Memlib_Walk(block,WN_kid(wn,kidno));
    }
}

// this is in common/com/config_opt.cxx
extern UINT32 OPT_Lower_To_Memlib;

void  LNO_Lower_Memlib(WN *wn)
{
  if (OPT_Lower_To_Memlib == 2)
  {
    Lower_To_Memlib_Walk(NULL, wn);
  }
}


#define MAX_INNER_LOOPS 3

static INT
Num_Inner_Loops(WN* loop)
{
  INT max_inner_loops = 0;
  for (LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(loop));
       itr; 
       itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_DO_LOOP) {
      INT num_inner_loops = 1;
      while ((wn = LWN_Get_Parent(wn)) != loop) {
        if (WN_operator(wn) == OPR_DO_LOOP) {
          num_inner_loops++;
        }
      }
      if (num_inner_loops > max_inner_loops) {
        max_inner_loops = num_inner_loops;
#ifdef KEY
        if (max_inner_loops > MAX_INNER_LOOPS) {
          return max_inner_loops; //stop
        }
#else
        if (max_inner_loops >= MAX_INNER_LOOPS) {
          return MAX_INNER_LOOPS;
        }

#endif
      }
    }
  }
  return max_inner_loops;
}


#ifdef KEY
// Count the number of basic operations
static
INT64 Loop_Size(WN* wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_leaf(opcode))
    return 1;
  else if (OPCODE_is_load(opcode))
    return 1;
  else if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    INT64 count = 0;
    while (kid) {
      count += Loop_Size(kid);
      kid = WN_next(kid);
    }
    return count;
  } else if (opcode == OPC_DO_LOOP) {
    INT64 count = Loop_Size(WN_start(wn));
    count += Loop_Size(WN_end(wn));
    INT64 count1 = Loop_Size(WN_do_body(wn));
    count1 += Loop_Size(WN_step(wn));
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (dli) {
      count1 *= MAX(1,dli->Est_Num_Iterations);
    }
    return (count+count1);
  }

  OPERATOR oper = OPCODE_operator(opcode);
  
  INT64 count = 0;
  INT kid_cnt = WN_kid_count(wn);

  if ((oper == OPR_TRUNC) || (oper == OPR_RND) ||
      (oper == OPR_CEIL) || (oper == OPR_FLOOR) || (oper == OPR_INTRINSIC_OP)) {
    count++;
  } else if ((oper == OPR_REALPART) || (oper == OPR_IMAGPART) ||
	     (oper == OPR_PARM) || (oper == OPR_PAREN)) {
    // no-ops
  } else if (OPCODE_is_expression(opcode) && (oper != OPR_CONST)) {
    if ((oper == OPR_MAX) || (oper == OPR_MIN) || 
	(oper == OPR_ADD) || (oper == OPR_SUB) || (oper == OPR_MPY) ||
	(oper == OPR_NEG))
      count++;
    else if ((oper == OPR_DIV || oper == OPR_SQRT))
      count = count + 10;
    
  } else if (OPCODE_is_store(opcode)) {
    count++;
    kid_cnt = kid_cnt - 1;
  } else if ((oper == OPR_CALL) || (oper == OPR_PURE_CALL_OP)) {
    count = count + LNO_Full_Unrolling_Loop_Size_Limit;
  }
  
  for (INT kidno=0; kidno<kid_cnt; kidno++) {
    WN *kid = WN_kid(wn,kidno);
    count += Loop_Size(kid);
  }

  return count;
  
}
// Count the number of basic operations in triangle loop
static
INT64 Loop_Size_Triangle(WN* wn, double trip_count)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_leaf(opcode))
    return 1;
  else if (OPCODE_is_load(opcode))
    return 1;
  else if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    INT64 count = 0;
    while (kid) {
      count += Loop_Size_Triangle(kid, trip_count);
      kid = WN_next(kid);
    }
    return count;
  } else if (opcode == OPC_DO_LOOP) {
    INT64 count = Loop_Size_Triangle(WN_start(wn), trip_count/2);
    count += Loop_Size_Triangle(WN_end(wn), trip_count/2);
    INT64 count1 = Loop_Size_Triangle(WN_do_body(wn), trip_count/2);
    count1 += Loop_Size_Triangle(WN_step(wn), (trip_count+1)/2);
    count1 *= MAX(1,trip_count);
    return (count+count1);
  }

  OPERATOR oper = OPCODE_operator(opcode);
  
  INT64 count = 0;
  INT kid_cnt = WN_kid_count(wn);

  if ((oper == OPR_TRUNC) || (oper == OPR_RND) ||
      (oper == OPR_CEIL) || (oper == OPR_FLOOR) || (oper == OPR_INTRINSIC_OP)) {
    count++;
  } else if ((oper == OPR_REALPART) || (oper == OPR_IMAGPART) ||
	     (oper == OPR_PARM) || (oper == OPR_PAREN)) {
    // no-ops
  } else if (OPCODE_is_expression(opcode) && (oper != OPR_CONST)) {
    if ((oper == OPR_MAX) || (oper == OPR_MIN) || 
	(oper == OPR_ADD) || (oper == OPR_SUB) || (oper == OPR_MPY) ||
	(oper == OPR_NEG))
      count++;
    else if ((oper == OPR_DIV || oper == OPR_SQRT))
      count = count + 10;
    
  } else if (OPCODE_is_store(opcode)) {
    count++;
    kid_cnt = kid_cnt - 1;
  } else if ((oper == OPR_CALL) || (oper == OPR_PURE_CALL_OP)) {
    count = count + LNO_Full_Unrolling_Loop_Size_Limit;
  }
  
  for (INT kidno=0; kidno<kid_cnt; kidno++) {
    WN *kid = WN_kid(wn,kidno);
    count += Loop_Size_Triangle(kid, trip_count);
  }

  return count;
  
}

static BOOL Has_Negative_Offset_Preg(WN *tree)
{
 if(WN_operator(tree)==OPR_BLOCK){
  for(WN *stmt = WN_first(tree); stmt; stmt=WN_next(stmt)){
     if(Has_Negative_Offset_Preg(stmt))
       return TRUE;
  }
  return FALSE;
 }
 if(WN_operator(tree)==OPR_LDID || WN_operator(tree)==OPR_STID){
   if (ST_class(WN_st(tree)) == CLASS_PREG &&
       WN_offset(tree) < 0) return TRUE;
 }
 for(INT kidno=0; kidno<WN_kid_count(tree); kidno++){
   if(Has_Negative_Offset_Preg(WN_kid(tree,kidno)))
    return TRUE;
 }
 return FALSE;
} 
#endif

//-----------------------------------------------------------------------
// peel the 2D triangle into 2 piece and fully unroll the 
// first part
// will do the following transformation
// for i = 1, n
//   for j = 1, i
//     S;
//---- will be changed to ----->
// switch(n)
// {
// case 1:
//   for i = 1, 1
//     for j = 1, j
//       S;
//       break;
// ....
// case m-1:
//   for i = 1, m-1
//     for j = 1, j
//       S;
//       break;
// default:
//   if (n >=m)
//   {
//     for i = 1, m
//       for j = 1, j
//         S;
//         break;
//     for i = m+1, n
//       for j = 1, j
//         S;
//         break;
//   }
// }
//-----------------------------------------------------------------------
BOOL Peel_2D_Triangle_Loops(WN* outer_loop)
{
  if ( !LNO_Peel_2D_Triangle_LOOP )
    return FALSE;
  Is_True((WN_opcode(outer_loop) == OPC_DO_LOOP) ,("outer_loop must be a do loop"));

  // don't do the transformation in the outermost loop
  if (!Do_Loop_Depth(outer_loop)) 
    return FALSE;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  WN* inner = Find_Next_Innermost_Do(outer_loop);
  Is_True((inner), ("inner must exist"));
  WN* inner_ubexp = UBexp(WN_end(inner));

  // the init value should be a const.
  WN* outer_start_v = WN_kid0(WN_start(outer_loop));
  WN* inner_start_v = WN_kid0(WN_start(inner));
  if( !outer_start_v || !inner_start_v)
    return FALSE;
  if ((WN_operator(outer_start_v) != OPR_CONST && 
       WN_operator(outer_start_v) != OPR_INTCONST) ||       
      (WN_operator(inner_start_v) != OPR_CONST &&
       WN_operator(inner_start_v) != OPR_INTCONST))
    return FALSE;
  
  // the compare operation should be OPR_LE
  if (WN_operator(WN_end(outer_loop)) != OPR_LE ||
      WN_operator(WN_end(inner)) != OPR_LE )
    return FALSE;
  // the loop step is a constant
  WN* outer_step_v = WN_kid1(WN_kid0(WN_step(outer_loop)));
  WN* inner_step_v = WN_kid1(WN_kid0(WN_step(inner)));
  if ((WN_operator(outer_step_v) != OPR_CONST && 
       WN_operator(outer_step_v) != OPR_INTCONST) ||       
      (WN_operator(inner_step_v) != OPR_CONST &&
       WN_operator(inner_step_v) != OPR_INTCONST))
    return FALSE;
  
  //  check if the loop is a triangle loop
  DEF_LIST *defs = Du_Mgr->Ud_Get_Def(inner_ubexp);
  DEF_LIST_ITER iter(defs);
  int num=0;
  for (const DU_NODE *node=iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN *def = (WN *) node->Wn();
    if (def != WN_start(outer_loop) && def != WN_step(outer_loop))
      return FALSE;
    num++;
  }
  if (num != 2)
    return FALSE;

  // now, it's a triangle loop, do the transformation
  WN* outer_info=WN_do_loop_info(outer_loop);
  WN* outer_trip=NULL;
  INT outer_trip_est=100;
  if (outer_info){
    outer_trip = WN_loop_trip(outer_info);
    outer_trip_est = WN_loop_trip_est(outer_info);
  }

  INT Triangle_Peel_Factor = 1;
  //check the size
  for (INT i = 2; i <= LNO_Full_Unrolling_Limit; i++){
    INT loop_size = Loop_Size_Triangle(outer_loop, i);
    if ( loop_size > LNO_Full_Unrolling_Loop_Size_Limit )
      break;
    Triangle_Peel_Factor = i;
  }
  if ( Triangle_Peel_Factor < 2 )
    return FALSE;

  if (LNO_Verbose) {
    fprintf(stdout,"Peel_2D_Triangle_Loops\n");
    fprintf(TFile, "Peel_2D_Triangle_Loops\n");
  }
  INT outer_init_v = WN_const_val(outer_start_v);
  INT outer_loop_step = WN_const_val(outer_step_v);
  INT const_value = (Triangle_Peel_Factor -1 + outer_init_v) * outer_loop_step;
  // The trip count is known
  if (outer_trip && outer_trip_est > Triangle_Peel_Factor) {
    WN* nest_copy = LWN_Copy_Tree(outer_loop, TRUE, LNO_Info_Map); 
    WN* wn_holder[2];
    wn_holder[0] = outer_loop;
    wn_holder[1] = nest_copy;
    if (red_manager) 
      red_manager->Unroll_Update(wn_holder, 2);
    Unrolled_DU_Update((WN**)wn_holder, 2, Do_Loop_Depth(outer_loop)-1, TRUE, FALSE);
    if (!dg->Add_Deps_To_Copy_Block(outer_loop, nest_copy, TRUE)) {
      SNL_DEBUG0(0, "Peel_2D_Triangle_Loops() failed -- continueing");
      LWN_Update_Dg_Delete_Tree(nest_copy, dg);
      LNO_Erase_Dg_From_Here_In(nest_copy, dg);
      LWN_Delete_Tree(nest_copy);
      MEM_POOL_Pop_Unfreeze(&SNL_local_pool);
      return FALSE;
    }
    WN* pblock = LWN_Get_Parent(outer_loop);
    LWN_Insert_Block_Before(pblock, outer_loop, nest_copy);
    LWN_Set_Parent(nest_copy, pblock);

    //set the bounds of the two loop
    WN* tmp;
    TYPE_ID rtype = WN_rtype(tmp);

    tmp = WN_kid1(WN_end(nest_copy));
    WN_const_val(tmp) = const_value ;
    WN* info0 = WN_do_loop_info(nest_copy);
    WN_const_val(WN_loop_trip(info0)) = Triangle_Peel_Factor;
    WN_loop_trip_est(info0) = Triangle_Peel_Factor;
    inner = Find_Next_Innermost_Do(nest_copy);
    WN_loop_trip_est(WN_do_loop_info(inner)) = (Triangle_Peel_Factor + 1) / 2;
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(nest_copy);
    dli->Est_Num_Iterations = Triangle_Peel_Factor;
    dli = Get_Do_Loop_Info(inner);
    dli->Est_Num_Iterations = (Triangle_Peel_Factor + 1) / 2;

    tmp = WN_kid0(WN_start(outer_loop));
    WN_const_val(tmp) = const_value + 1;
    WN* info1 = WN_do_loop_info(outer_loop);
    WN_const_val(WN_loop_trip(info1)) = outer_trip_est - Triangle_Peel_Factor;
    WN_loop_trip_est(info1) = outer_trip_est - Triangle_Peel_Factor;
    inner = Find_Next_Innermost_Do(outer_loop);
    WN_loop_trip_est(WN_do_loop_info(inner)) = (outer_trip_est - Triangle_Peel_Factor)/2;
    dli = Get_Do_Loop_Info(outer_loop);
    dli->Est_Num_Iterations = outer_trip_est - Triangle_Peel_Factor;
    dli = Get_Do_Loop_Info(inner);
    dli->Est_Num_Iterations = (outer_trip_est - Triangle_Peel_Factor + 1) / 2;
    
    return TRUE;
  } else if (outer_trip) {
    return TRUE; 
  }
  // deal with the situation when the trip count is unkown.
  WN *nest_copy, *tmp, *loop_info;
  WN** wn_holder;
  WN* pblock = LWN_Get_Parent(outer_loop);
  WN* position_wn = NULL;
  WN *goto_exp, *lable_exp;
  LABEL_IDX out_lbl;
  New_LABEL(CURRENT_SYMTAB, out_lbl);
  WN* out_lbl_exp = WN_CreateLabel(out_lbl, 0, NULL);
  LWN_Insert_Block_After(pblock, outer_loop, out_lbl_exp);
  LWN_Set_Parent(out_lbl_exp, pblock);
  wn_holder = CXX_NEW_ARRAY(WN*, Triangle_Peel_Factor+1, &SNL_local_pool);
  wn_holder[0] = outer_loop; 

  //make Triangle_Peel_Factor copy of the original
  for (INT i = 1; i <= Triangle_Peel_Factor; i++) {
    nest_copy = LWN_Copy_Tree(outer_loop, TRUE, LNO_Info_Map);
    if (!dg->Add_Deps_To_Copy_Block(outer_loop, nest_copy, TRUE)){
      SNL_DEBUG0(0, "Peel_2D_Triangle_Loops() failed -- continueing");
      LWN_Update_Dg_Delete_Tree(nest_copy, dg);
      LNO_Erase_Dg_From_Here_In(nest_copy, dg);
      LWN_Delete_Tree(nest_copy);
      MEM_POOL_Pop_Unfreeze(&SNL_local_pool);
      return FALSE;
    }
    wn_holder[i]=nest_copy;
  }  
  if (red_manager) 
    red_manager->Unroll_Update(wn_holder, Triangle_Peel_Factor+1);
  Unrolled_DU_Update(wn_holder, Triangle_Peel_Factor+1, Do_Loop_Depth(outer_loop)-1, TRUE, FALSE);

  for (INT i = 1; i <= Triangle_Peel_Factor; i++) {
    nest_copy = wn_holder[i];
    tmp = WN_kid1(WN_end(nest_copy));
    defs = Du_Mgr->Ud_Get_Def(tmp);
    DEF_LIST_ITER d_iter0(defs);
    for (DU_NODE* d_node = d_iter0.First(); !d_iter0.Is_Empty(); d_node=d_iter0.Next()) {
      Du_Mgr->Delete_Def_Use(d_node->Wn(), tmp);
    }
    Du_Mgr->Remove_Use_From_System(tmp);
    tmp = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, WN_rtype(tmp), MTYPE_V), 
			    (i - 1 + outer_init_v) * outer_loop_step); 
    LWN_Delete_Tree(WN_kid1(WN_end(nest_copy)));
    WN_kid1(WN_end(nest_copy)) = tmp;
    LWN_Set_Parent(tmp, WN_end(nest_copy));
    loop_info = WN_do_loop_info(nest_copy);
    WN_kid1(loop_info) = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, WN_rtype(tmp), MTYPE_V), i); 
    WN_set_kid_count(loop_info, 2);
    LWN_Set_Parent(WN_loop_trip(loop_info), loop_info);
    WN_loop_trip_est(loop_info) = i;
    inner = Find_Next_Innermost_Do(nest_copy);
    WN_loop_trip_est(WN_do_loop_info(inner)) = (i + 1) / 2;
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(nest_copy);
    dli->Est_Num_Iterations = i;
    dli->Num_Iterations_Symbolic = 0;
    dli = Get_Do_Loop_Info(inner);
    dli->Est_Num_Iterations = (i + 1) / 2;
    dli->Num_Iterations_Symbolic = 0;

    if ( i == Triangle_Peel_Factor ) 
      break;
      
    // build the switch case
    OPCODE op_eq = OPCODE_make_op(OPR_EQ, Boolean_type, Do_Wtype(outer_loop));
    WN* tmp1 = LWN_Copy_Tree(UBexp(WN_end(outer_loop)));
    defs = Du_Mgr->Ud_Get_Def(UBexp(WN_end(outer_loop)));
    DEF_LIST_ITER d_iter(defs);
    for (DU_NODE* d_node = d_iter.First(); !d_iter.Is_Empty(); d_node=d_iter.Next()) {
      Du_Mgr->Add_Def_Use(d_node->Wn(), tmp1);    
    }
    WN* tmp2 = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, WN_rtype(tmp1), MTYPE_V), i); 
    LABEL_IDX lbl;
    New_LABEL(CURRENT_SYMTAB, lbl);
    WN* truebr_exp = WN_CreateTruebr(lbl, LWN_CreateExp2(op_eq, tmp1, tmp2));
    LWN_Set_Parent(WN_kid0(truebr_exp), truebr_exp);
    LWN_Insert_Block_After(pblock, outer_loop, truebr_exp);
    LWN_Set_Parent(truebr_exp, pblock);
    if (!position_wn)
      position_wn = truebr_exp;
    lable_exp = WN_CreateLabel(lbl, 0, NULL);
    LWN_Insert_Block_After(pblock, position_wn, lable_exp);
    LWN_Set_Parent(lable_exp, pblock);
    LWN_Insert_Block_After(pblock, lable_exp, nest_copy);
    LWN_Set_Parent(nest_copy, pblock);
    goto_exp = WN_CreateGoto(out_lbl);
    LWN_Insert_Block_After(pblock, nest_copy, goto_exp);
    LWN_Set_Parent(goto_exp, pblock);
  }
  LABEL_IDX lbl_default;
  New_LABEL(CURRENT_SYMTAB, lbl_default);
  WN* lable_default_exp = WN_CreateLabel(lbl_default, 0, NULL);
  WN* goto_default = WN_CreateGoto(lbl_default);
  LWN_Insert_Block_After(pblock, position_wn, goto_default);
  LWN_Set_Parent(goto_default, pblock);
  LWN_Insert_Block_Before(pblock, out_lbl_exp, lable_default_exp);
  LWN_Set_Parent(lable_default_exp, pblock);

  tmp = WN_kid0(WN_start(outer_loop));
  WN_const_val(tmp) = const_value+1;
  loop_info = WN_do_loop_info(outer_loop);

  if (WN_loop_trip_est(loop_info))
    WN_loop_trip_est(loop_info) = outer_trip_est - Triangle_Peel_Factor;
  
  //create the if-then-else, split the loop into two pieces.
  OPCODE op_ge = OPCODE_make_op(OPR_GE, Boolean_type, Do_Wtype(outer_loop));

  WN* tmp1 = LWN_Copy_Tree(UBexp(WN_end(outer_loop)));
  defs = Du_Mgr->Ud_Get_Def(UBexp(WN_end(outer_loop)));
  DEF_LIST_ITER d_iter(defs);
  for (DU_NODE* d_node = d_iter.First(); !d_iter.Is_Empty(); d_node=d_iter.Next()) {
    Du_Mgr->Add_Def_Use(d_node->Wn(), tmp1);    
  }
  WN* tmp2 = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, WN_rtype(tmp1), MTYPE_V),
			       Triangle_Peel_Factor); 
  WN* if_exp = LWN_CreateIf(LWN_CreateExp2(op_ge, tmp1, tmp2), 
			    WN_CreateBlock(), WN_CreateBlock());
  LWN_Insert_Block_After(WN_then(if_exp), NULL, wn_holder[Triangle_Peel_Factor]);
  LWN_Set_Parent(wn_holder[Triangle_Peel_Factor], WN_then(if_exp));
  //get outer_loop out of the original parent
  if (WN_first(pblock) == outer_loop) {
    WN_first(pblock) = WN_next(outer_loop);
    WN_prev(WN_first(pblock)) = NULL;
  } else {
    WN_next(WN_prev(outer_loop)) = WN_next(outer_loop);
    WN_prev(WN_next(outer_loop)) = WN_prev(outer_loop);
  }
  LWN_Insert_Block_After(WN_then(if_exp), wn_holder[Triangle_Peel_Factor], outer_loop);
  LWN_Set_Parent(outer_loop, WN_then(if_exp));
  //insert after the switch default lable
  LWN_Insert_Block_After(pblock, lable_default_exp, if_exp);
  LWN_Set_Parent(if_exp, pblock);
  // annotate the if
  BOOL has_regions = (Find_SCF_Inside(if_exp, OPC_REGION) != NULL);
  IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, has_regions), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, if_exp, (void *)ii);
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
  Build_Doloop_Stack(if_exp, stack);
  LNO_Build_If_Access(if_exp, stack);
  //Build_Doloop_Stack(pblock, stack);
  // LNO_Build_Access(pblock, stack, &LNO_local_pool);
  CXX_DELETE(stack, &LNO_local_pool);
  // set the parent_loop has goto
  WN* parent_wn=pblock;
  WN* parent_loop=NULL;
  BOOL current_level=TRUE;
  while (parent_wn){
    if ( WN_opcode(parent_wn) == OPC_DO_LOOP ){
      parent_loop=parent_wn;
      Get_Do_Loop_Info(parent_wn)->Has_Gotos = TRUE;
      if (current_level){
	Get_Do_Loop_Info(parent_wn)->Has_Gotos_This_Level=TRUE;
	current_level = FALSE;
      }
    }
    parent_wn = LWN_Get_Parent(parent_wn);
  }
  Is_True(parent_loop, ("parent_loop is NULL!"));
  SNL_Rebuild_Access_Arrays(parent_loop);
  
  return TRUE;
}

// returns true if any inner loop in wn is fully unrolled.
// 'is_seq_iter' indicates whether caller site is iterating the WHIRLs sequentially.
BOOL
Fully_Unroll_Short_Loops(WN* wn, BOOL is_seq_iter = FALSE)
{
  WN* first;
  WN* last;
  WN* next;
  BOOL unrolled = FALSE;
  OPERATOR oper = WN_operator(wn);
  // This routine can be destructive to 'wn', verify that 'wn' was not deleted.
  Is_True(oper != OPERATOR_UNKNOWN, ("invalid operator"));

  if (oper == OPR_BLOCK) {
    wn = WN_first(wn); 
    while (wn) {
      next = WN_next(wn);
      unrolled  |=  Fully_Unroll_Short_Loops(wn, TRUE);
      wn = next;
    }
    return unrolled;
  }
  else if (oper == OPR_DO_LOOP    &&
           !Do_Loop_Has_Calls(wn) &&
           !Do_Loop_Has_EH_Regions(wn) &&
#ifndef KEY
           (!Do_Loop_Has_Exits(wn) || Do_Loop_Is_Regular(wn)) &&
	   !Do_Loop_Has_Conditional(wn) &&
#else 
           !Do_Loop_Has_Exits(wn) &&
           !Do_Loop_Has_Gotos(wn) &&
#endif
           !Do_Loop_Has_Gotos(wn) &&
           !Do_Loop_Is_Mp(wn)     &&
           !Is_Nested_Doacross(wn) &&
#ifdef KEY
           Num_Inner_Loops(wn) <= MAX_INNER_LOOPS) {
          //bug 10644
          if(Num_Inner_Loops(wn) == MAX_INNER_LOOPS &&
             Is_Invariant_Factorization_Beneficial(wn)) {
             DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
           if(dli && dli->Delay_Full_Unroll==FALSE){
               dli->Delay_Full_Unroll = TRUE;
              return unrolled;              
           }
          }
#else
           Num_Inner_Loops(wn) < MAX_INNER_LOOPS) {
#endif
    INT64 trip_count = Num_Iters(wn);
    if (trip_count == 0
#ifdef KEY
	// bug 3444
	// Why do we have 2 functions to find # of iterations?
        && Iterations(wn, &LNO_local_pool) == 0
#endif
      ) {
      Remove_Zero_Trip_Loop(wn);
      return unrolled;
    }
    if (trip_count >= 1 && trip_count <= LNO_Full_Unrolling_Limit) {
      if (trip_count > 1) {
	WN * wn_prev = WN_prev(wn);
	WN * wn_next = WN_next(wn);
	DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
	WN * wn_body = WN_do_body(wn);
	WN * wn_kid = (wn_body && (WN_operator(wn_body) == OPR_BLOCK)) ? WN_first(wn_body) : NULL;

	// Attempt unrolling if wn has a child loop whose iteration space is compatible
	// with that of wn_prev or wn_next.  Unrolling enables loop fusion.
	if (Do_Aggressive_Fuse) {
	  OPERATOR opr = WN_operator(wn);
	  if (dli && wn_kid && (WN_operator(wn_kid) == opr)
	      && ((wn_prev && (WN_operator(wn_prev) == opr)
		   && WN_has_compatible_iter_space(wn_kid, wn_prev, NULL, NULL, FALSE)
		   && Same_Bounds(wn_prev, wn_kid))
		  || (wn_next && (WN_operator(wn_next) == opr) 
		      && WN_has_compatible_iter_space(wn_kid, wn_next, NULL, NULL, FALSE)
		      && Same_Bounds(wn_next, wn_kid)))) {
	    dli->Required_Unroll = 1;
	  }
	}

#ifdef KEY
        //trip_count already used in calculating Loop_Size(do_loop), so don't mul
        //bug 11954, 11958: Regression caused by not multiplying trip_count, because
        //we need new LNO_Full_Unrolling_Loop_Size_Limit default.
        //TODO: re-investigate here after work bug 10644
	if (!dli || !dli->Required_Unroll) {
	  if (Loop_Size(wn)*trip_count > LNO_Full_Unrolling_Loop_Size_Limit ||
	      //bug 5159:  Loops having PREG with -ve offsets can not be unrolled since they are
	      //ASM output values and duplicating them will break CG assumption.
	      Has_Negative_Offset_Preg(WN_do_body(wn))){
	    //       if (Loop_Size(wn) > LNO_Full_Unrolling_Loop_Size_Limit) {
	    unrolled |= Fully_Unroll_Short_Loops(WN_do_body(wn));
	    return unrolled;
	  }
	}
	static INT count = 0;
	count ++;
	if (LNO_Full_Unroll_Skip_Before > count - 1 ||
	    LNO_Full_Unroll_Skip_After < count - 1 ||
	    LNO_Full_Unroll_Skip_Equal == count - 1) {
	  unrolled |= Fully_Unroll_Short_Loops(WN_do_body(wn));
	  return unrolled;
	}
	if (LNO_Full_Unroll_Outer == FALSE) {
	  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
	  if (!dli || !dli->Required_Unroll) {
	    WN* parent = LWN_Get_Parent(wn);
	    while(parent && WN_operator(parent) != OPR_DO_LOOP &&
		  WN_operator(parent) != OPR_FUNC_ENTRY)
	      parent = LWN_Get_Parent(parent);
	    if (!parent || WN_operator(parent) == OPR_FUNC_ENTRY) {
	      unrolled |= Fully_Unroll_Short_Loops(WN_do_body(wn));
	      return unrolled;	    
	    }
	  }
	}
#endif
        Unroll_Loop_By_Trip_Count(wn, trip_count);
        unrolled = TRUE;
        // Du_Sanity_Check(Current_Func_Node);
      }
      WN * next_wn = WN_next(wn);
      Remove_Unity_Trip_Loop(wn, TRUE, &first, &last, NULL, Du_Mgr);
      // Du_Sanity_Check(Current_Func_Node);
      // If caller is sequentially visiting the WHIRLs, the 'next_wn'
      // will be visited in the caller site.  if the 'next_wn' is the
      // same as 'first', we should not recursively invoke the routine
      // 'Fully_Unroll_Short_Loops', otherwise if 'first' is deleted,
      // the caller site will visit the deleted wn.
      if (!is_seq_iter || (first != next_wn)) {
	wn = first; 

	while (wn) {
	  next = WN_next(wn);
	  unrolled |= Fully_Unroll_Short_Loops(wn, TRUE);
	  if (wn == last) {
	    break;
	  }
	  wn = next;
	}
      }

      return unrolled;
    }
  }
  if (OPERATOR_is_scf(oper)) {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      unrolled |= Fully_Unroll_Short_Loops(WN_kid(wn, kidno));
    }
  }

  return unrolled;
}

//-----------------------------------------------------------------------
// NAME: Parallel_And_Padding_Phase 
// FUNCTION: Apply parallel and padding optimizations as requested by 
//   flags
//-----------------------------------------------------------------------

extern void Parallel_And_Padding_Phase(PU_Info* current_pu, 
		    		       WN* func_nd)
{
  Mark_Critical_Section_Loops(func_nd);
  Mark_Threadprivate_Loops(func_nd);
  IPA_LNO_Evaluate_Call_Infos(func_nd);

  if (Run_autopar && LNO_Run_AP > 0 &&
      !Get_Trace(TP_LNOPT2, TT_LNO_NO_TRANSPOSE) &&
      !Get_Trace(TP_LNOPT2, TT_LNO_NO_AUTO_PARALLEL)) {
      Mark_Auto_Parallelizable_Loops(func_nd); 
      Transpose_For_MP(func_nd);
  }

  if (!Get_Trace(TP_LNOPT2, TT_LNO_NO_PAD) && LNO_Local_Pad_Size != 0) { 
    Pad_First_Dim_Degenerates(func_nd);
    // Only pad degenerates if -LNO:local_pad_size was not specified
    if (LNO_Local_Pad_Size == (UINT32)-1) {
      Pad_Degenerates();
    }
  } 

  Doacross_Init(func_nd);

  Auto_Parallelization(current_pu, func_nd); 
  if (LNO_Autodist) {
    Automatic_Data_Distribute(func_nd);
  }
  Mp_Tile(func_nd); 

  Doacross_Finish();
  IPA_LNO_Unevaluate_Call_Infos(func_nd);

}

BOOL Run_autopar_save; 
INT last_loop_num;

#ifdef KEY
static BOOL Skip_Simd;
static BOOL Skip_HoistIf;
static BOOL Skip_SVR;
static BOOL Skip_Unswitch;
#endif /* KEY */
extern WN * Lnoptimizer(PU_Info* current_pu, 
			WN *func_nd , DU_MANAGER *du_mgr,
			ALIAS_MANAGER *alias_mgr)
{
  extern BOOL Run_lno;
  STDOUT = stdout;
  last_loop_num = 0;

  MEM_POOL_Initialize(&ARA_memory_pool, "ARA_memory_pool", FALSE);
  MEM_POOL_Push(&ARA_memory_pool);

#ifdef KEY
  static INT pu_num = 0;
  
  if (pu_num < LNO_Simd_Skip_Before 
      || pu_num > LNO_Simd_Skip_After
      || pu_num == LNO_Simd_Skip_Equal)
    Skip_Simd = TRUE;
  else 
    Skip_Simd = FALSE;
  
  if (pu_num < LNO_HoistIf_Skip_Before 
      || pu_num > LNO_HoistIf_Skip_After
      || pu_num == LNO_HoistIf_Skip_Equal)
    Skip_HoistIf = TRUE;
  else 
    Skip_HoistIf = FALSE;
 
  if (pu_num < LNO_SVR_Skip_Before 
      || pu_num > LNO_SVR_Skip_After
      || pu_num == LNO_SVR_Skip_Equal)
    Skip_SVR = TRUE;
  else 
    Skip_SVR = FALSE;
 
  if (pu_num < LNO_Unswitch_Skip_Before 
      || pu_num > LNO_Unswitch_Skip_After
      || pu_num == LNO_Unswitch_Skip_Equal)
    Skip_Unswitch = TRUE;
  else 
    Skip_Unswitch = FALSE;
  
  if (pu_num < LNO_Skip_Before 
      || pu_num > LNO_Skip_After
      || pu_num == LNO_Skip_Equal)
    LNO_enabled = FALSE;
  else 
    LNO_enabled = TRUE;

  pu_num ++;
#endif /* KEY */
  
  // early exit test
  if ( !Du_Built(du_mgr) ) {

    // need to possibly dump the function
    if (LNO_Prompl) {
      Print_Prompl_Msgs(current_pu, func_nd); 
    } 

#ifdef KEY //just cheat the list_option, don't worry
           // about the compilation time if the uer
           // want to list options
    if(List_Enabled){
      Init_Prefetch_Options(func_nd);
      Mhd.Initialize();
      Mhd.Merge_Options(Mhd_Options);
    }
#endif

    MEM_POOL_Pop(&ARA_memory_pool);
    MEM_POOL_Delete(&ARA_memory_pool);
    LNO_Restore_Configs();
    return func_nd;
  }

  if ((!Run_lno) ||
      (LNO_Opt == 0) ||
      (Get_Trace(TP_LNOPT, TT_LNO_SKIP_LNO))) {
    LNO_enabled = FALSE;
  }

  Run_autopar_save = Run_autopar;
  ROUNDOFF Roundoff_Level_Save = Roundoff_Level;

  Du_Mgr = du_mgr;
  if(Run_autopar)
    WN_Register_Delete_Cleanup_Function(LWN_Delete_SR);
  WN_Register_Delete_Cleanup_Function(LWN_Delete_DU);
  VINDEX16 save_graph_capacity = GRAPH16_CAPACITY; 
  GRAPH16_CAPACITY = LNO_Graph_Capacity;
  Current_Func_Node = func_nd;  
  if (!lno_mempool_initialized) {
    MEM_POOL_Initialize(&LNO_default_pool, "LNO_default_pool", FALSE);
    MEM_POOL_Initialize(&LNO_local_pool, "LNO_local_pool", FALSE);
    MEM_POOL_Initialize(&SNL_local_pool, "SNL_local_pool", FALSE);
    lno_mempool_initialized = TRUE;
  }

  snl_debug = 0;
  if (Get_Trace(TP_LNOPT, TT_LNO_SNL_DEBUG2))
    snl_debug += 2;
  if (Get_Trace(TP_LNOPT, TT_LNO_SNL_DEBUG1))
    snl_debug += 1;

  // Check whether to attempt aggressive loop fusion.
  Do_Aggressive_Fuse = FALSE;
  if ((LNO_Fusion >= 2) && (LNO_Sclrze) && (LNO_Aequiv) && OPT_Scale) {
    Do_Aggressive_Fuse = TRUE;
    int pu_cnt = Current_PU_Count();
    UINT limit = LNO_Aggressive_Fusion_Limit;
    if (limit > 0) {
      if ((pu_cnt + 1) >= limit) {
	Do_Aggressive_Fuse = FALSE;
	if (LNO_Verbose)
	  fprintf(stdout, "Disable aggressive loop fusion for function:%d\n", pu_cnt);	  
      }
    }
  }

  LNO_Allow_Nonlinear = !Get_Trace(TP_LNOPT,TT_LNO_SKIP_NONLIN);
  LNO_Debug_Delinearization = Get_Trace(TP_LNOPT,TT_LNO_DEBUG_DELIN);

  LNO_Allow_Delinearize = TRUE; // this is true for lno but not
				    // for IPL

  LNO_Verbose = Get_Trace(TP_LNOPT, TT_LNO_VERBOSE);

  if (LNO_Verbose) {
    fprintf(stdout, "Lno processing procedure: %s\n", Cur_PU_Name);
    fprintf(TFile, "Lno processing procedure: %s\n", Cur_PU_Name);
  }

  // Initialize prefetch options before Mhd.Initialize
  // since that uses Run_Prefetch
  Init_Prefetch_Options (func_nd);
  
  Mhd.Initialize();
  Mhd.Merge_Options(Mhd_Options);
  if (snl_debug)
    Mhd.Print(TFile);

  MEM_POOL_Push(&LNO_local_pool);
  MEM_POOL_Push(&SNL_local_pool);
  MEM_POOL_Push_Freeze(&LNO_default_pool);

  Alias_Mgr = alias_mgr;
  red_manager = NULL;
  Array_Dependence_Graph = NULL;

  Parent_Map = WN_MAP_Create(&LNO_default_pool);
  Deleted_Loop_Map =CXX_NEW(
         (HASH_TABLE<WN*, BOOL>) (100,&LNO_default_pool),
         &LNO_default_pool);

  WN_SimpParentMap = Parent_Map;   // Let the simplifier know about it
  FmtAssert(Parent_Map != -1,("Ran out of mappings in Lnoptimizer"));
  LNO_Info_Map = WN_MAP_Create(&LNO_default_pool);
  FmtAssert(LNO_Info_Map != -1,("Ran out of mappings in Lnoptimizer"));
  LNO_Precom_Map = WN_MAP_Create(&LNO_default_pool);
  FmtAssert(LNO_Precom_Map != -1,("Ran out of mappings in Lnoptimizer"));
  Array_Dependence_Map = WN_MAP_Create(&LNO_default_pool);
  FmtAssert(Array_Dependence_Map != -1,("Ran out of mappings in Lnoptimizer"));
  Stmt_Dependence_Map = WN_MAP_Create(&LNO_default_pool);
  FmtAssert(Stmt_Dependence_Map != -1,("Ran out of mappings in Lnoptimizer"));
  /* initialize safe speculation map */
  Safe_Spec_Map = WN_MAP_Create(&LNO_default_pool);
  FmtAssert(Safe_Spec_Map != WN_MAP_UNDEFINED,
	    ("Ran out of mappings in Lnoptimizer"));



  Start_Timer ( T_LNOParentize_CU );
  LWN_Parentize (func_nd);
  Stop_Timer ( T_LNOParentize_CU );
  WB_Set_Sanity_Check_Level(WBC_DU_ONLY); 


#ifdef Is_True_On
  Du_Sanity_Check(func_nd);
#endif

  if (snl_debug) {
    fprintf(TFile, "AFTER PREOPT:\n");
    Dump_WN(func_nd, TFile, snl_debug);
  }

  BOOL simp_state_save = WN_Simplifier_Enable(TRUE);
  BOOL simp_fold_iload_save = WN_Simp_Fold_ILOAD;
  WN_Simp_Fold_ILOAD = FALSE;

#ifdef _NEW_SYMTAB
  if (!LNO_Run_Lego_Set) LNO_Run_Lego = PU_mp_needs_lno(Get_Current_PU());
#else
  if (!LNO_Run_Lego_Set) LNO_Run_Lego = SYMTAB_mp_needs_lno(Current_Symtab);
#endif

  extern BOOL Run_lno;
  // from this point on:
  //    Run_lno  == TRUE --> do LNO optimizations
  //    LNO_Run_Lego == TRUE --> do lego lowering
  //    None TRUE is an error, unless explicitly overridden
  Is_True (Run_lno || LNO_Run_Lego || LNO_Run_Lego_Set 
	   || Run_autopar && LNO_Run_AP > 0,
           ("LNO on PU %s: Run_lno == FALSE, and no distr-arrays\n",
            Cur_PU_Name));
  // LNO_Analysis=fopen("LNO_Analysis","w");
  if ( List_Cite ) {
    LNO_Analysis=Lst_File;
  }
  if (!Run_lno && !LNO_Run_Lego && !(Run_autopar && LNO_Run_AP > 0)) 
    goto return_point;

  {
    Permutation_Arrays = CXX_NEW(PERMUTATION_ARRAYS(&LNO_default_pool),
      &LNO_default_pool);

    if (LNO_Run_Lego && !LNO_enabled) {
      // do just basic lowering
      Lego_OZero_Driver(current_pu, func_nd);
      goto return_point;
    }
    // Skipping lno?  If so, quit.
    if (Get_Trace(TP_LNOPT, TT_LNO_SKIP_LNO)) {
      goto return_point;
    } 

    SAC sac(func_nd); 
    sac.Perform_Structure_Split_Opt();

    // mark do loops
    // doesn't affect access vectors
    BOOL has_do_loops = Mark_Code(func_nd, TRUE, TRUE);

    // Mark_Code may have disabled -pfa (e.g. due to directive)
    if (!LNO_enabled && !Run_autopar && !Run_lno) {
      goto return_point;
    }

    if (LNO_Run_call_info) {
      Call_Info_Walk(func_nd);
      Mark_Code(func_nd, FALSE, TRUE);  
    }
  
    // From this point on, do both regular LNO and Lego-lowering
    if (LNO_Run_Lego) {
      Lego_PU_Init ();
      Lego_Read_Pragmas (func_nd);
      void Lego_Fix_Local(WN *func_nd);
      if (LNO_Run_Lego_Localizer) Lego_Fix_Local(func_nd);
      void Lego_Fix_IO(WN *func_nd, BOOL *has_do_loops);
      Lego_Fix_IO(func_nd,&has_do_loops);
    }

    // Enumerate loops for debugging purpose.
    Enum_loops(func_nd);  
  
    // Build and map all access arrays
    Start_Timer ( T_LNOAccess_CU );
    LNO_Build_Access(func_nd, &LNO_default_pool);  
    Stop_Timer ( T_LNOAccess_CU );
    if (LNO_Verbose) {
      LNO_Print_Access(TFile,func_nd);  
    }

    if (Run_autopar && LNO_enabled)
      IPA_LNO_Map_Calls(func_nd, IPA_LNO_File);
  
    if (!has_do_loops) {
      goto return_point;  // no do loops, no point in continuing
    }

    if (LNO_Full_Unrolling_Limit != 0) {
      Fully_Unroll_Short_Loops(func_nd);
    }

#ifdef KEY 
    if (LNO_Build_Scalar_Reductions || Roundoff_Level >= ROUNDOFF_ASSOC) {
#else
    if (Roundoff_Level >= ROUNDOFF_ASSOC) {
#endif
      red_manager = CXX_NEW 
          (REDUCTION_MANAGER(&LNO_default_pool), &LNO_default_pool);
      red_manager->Build(func_nd,TRUE,FALSE); // build scalar and array reductions
    }
  
    // Get rid of inconsistent control flow
    if (Eliminate_Dead_SCF(func_nd,LWN_Delete_Tree)) {
      Mark_Code(func_nd, FALSE, TRUE);  
	  // remark because elimination may have changed things
    }
  
    if (LNO_Opt > 0) {
      // try to move conditionals outside of loops
      if (!Get_Trace(TP_LNOPT2, TT_SHACKLE_ONLY) && !LNO_Shackle)
        Hoist_Conditionals(func_nd);
    }
  
    // Build the array dependence graph
    Start_Timer ( T_LNOBuildDep_CU );
      
    if (Liberal_Ivdep && Cray_Ivdep) {
      DevWarn("Both Liberal_Ivdep and Cray_Ivdep set, Liberal_Ivdep ignored");
    }
    
    BOOL LNO_skip=FALSE;
  
    // skip_it, skip_before, skip_after function count specified
    if ( Query_Skiplist ( Optimization_Skip_List, Current_PU_Count() ) )
    {
      if ( Show_Progress ) {
        ErrMsg ( EC_Skip_PU, " LNO", Current_PU_Count(), Cur_PU_Name );
      }
      LNO_skip=TRUE;
    }

    if ((LNO_Opt == 0 || LNO_skip || !LNO_enabled) && !(Run_autopar && LNO_Run_AP > 0)) {
      GRAPH16_CAPACITY = save_graph_capacity; 
      Build_CG_Dependence_Graph (func_nd);
      Stop_Timer ( T_LNOBuildDep_CU );
  
      if (!Get_Trace(TP_LNOPT, TT_LNO_GUARD)) {
        Guard_Dos(func_nd); // put guards around all the do statments
      }
      goto return_point;
    }
    else {
      BOOL graph_is_ok = Build_Array_Dependence_Graph (func_nd);
      Stop_Timer ( T_LNOBuildDep_CU );
      if (!graph_is_ok) 
        goto return_point;
    }

    if (!LNO_enabled && (Run_autopar && LNO_Run_AP > 0)) {
      LWN_Process_FF_Pragmas(func_nd); 
      Parallel_And_Padding_Phase(current_pu, func_nd); 
      goto return_point; 
    } 
  
    LNO_Tlog = Get_Trace ( TP_PTRACE1, TP_PTRACE1_LNO );
  
    Hoist_Varying_Lower_Bounds(func_nd); 
#ifndef TARG_X8664
    // Move If_MinMax() after Array_Substutution() so that
    // new peeling heuiristic could kick-in. Only do this
    // for X8664.
    If_MinMax(func_nd);
#endif
    Dead_Store_Eliminate_Arrays(Array_Dependence_Graph);
    Array_Substitution(func_nd);
#ifdef TARG_X8664
    If_MinMax(func_nd);
#endif
    Reverse_Loops(func_nd);

   
    if (Roundoff_Level >= ROUNDOFF_ASSOC) {
      // array reductions
      red_manager->Build(func_nd,FALSE,TRUE,Array_Dependence_Graph);
      if (Eager_Level >= 4) {
        Eliminate_Zero_Mult(func_nd, Array_Dependence_Graph);
      }
    }

    // Scalarize the invariants
    if (LNO_Sclrze) {
      Scalarize_Arrays(Array_Dependence_Graph,0,1,red_manager, func_nd);
    }
  
    // Mark parallel loops before fusion so fusion will not
    // fuse parallel and serial loops.
    //  gwe? LNO_entry()
    if (Run_autopar && LNO_Run_AP > 0 &&
        !Get_Trace(TP_LNOPT2, TT_LNO_NO_TRANSPOSE) &&  // gwe? need this?
        !Get_Trace(TP_LNOPT2, TT_LNO_NO_AUTO_PARALLEL)) {
        IPA_LNO_Evaluate_Call_Infos(func_nd);
        Mark_Auto_Parallelizable_Loops(func_nd); 
        Transpose_For_MP(func_nd);  // gwe? want this here?
        IPA_LNO_Unevaluate_Call_Infos(func_nd);
    }
#ifdef KEY
    if (LNO_Run_Unswitch && !Skip_Unswitch && Loop_Unswitch_SCF(func_nd)) {
      // remark because unswitch may have changed things
      Mark_Code(func_nd, FALSE, TRUE);  
    }
#endif
#ifdef TARG_X8664
    if (LNO_Run_Simd > 0)
      Mark_Auto_Vectorizable_Loops(func_nd);
#endif
  
    // This must be done before LWN_Process_FF_Pragmas because we rely
    // on the inline markers
    Perform_Structure_Array_Copy_Opt(func_nd);

    // Process pragmas
    if (!LNO_Ignore_Pragmas) {
      Fission_Init();
      Fusion_Init();
      LWN_Process_FF_Pragmas(func_nd);
    }
  
    Canonicalize_Unsigned_Loops(func_nd); 


    BOOL do_ara = ((Get_Trace(TP_LNOPT2,TT_LNO_RUN_ARA) 
      || Run_autopar && LNO_Run_AP > 0)
      && Get_Trace(TP_LNOPT2, TT_LNO_NO_AUTO_PARALLEL));
    if (do_ara) 
      Perform_ARA_and_Parallelization(current_pu, func_nd);
  
    if (LNO_Run_Lego) {
      Lego_Skew_Indices(func_nd); 
      Lego_Compute_Tile_Peel (func_nd);
    }
  
    Lego_Tile(func_nd, FALSE); 
    if (LNO_Run_Lego) {
      if (!Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_INTERCHANGE))
        Lego_Interchange(func_nd);
      extern void RR_Map_Setup (WN* func_wn);
      RR_Map_Setup(func_nd);
      Lego_Peel(func_nd);  
    }

    BOOL early_exit = FALSE; 
    {
      BOOL do_fiz_fuse = !Get_Trace(TP_LNOPT,TT_LNO_SKIP_FIZ_FUSE);
      BOOL do_p25 = !Get_Trace(TP_LNOPT,TT_LNO_SKIP_GS);
      BOOL do_inner_fission = !Get_Trace(TP_LNOPT,TT_LNO_SKIP_INNER_FISSION);
  
      Fission_Init();
      Fusion_Init();
  
      WB_Set_Sanity_Check_Level(WBC_FULL_SNL); 
  
      if (!Get_Trace(TP_LNOPT, TT_LNO_NORENAME))
#ifdef KEY
#ifdef TARG_X8664
	// Bug 4203 - this is probably exposing some register allocation bug 
	// for m32. Hide it for now.
	if (!Skip_SVR && LNO_SVR && (LNO_SVR_Phase1 || Is_Target_32bit()))
#else
	if (!Skip_SVR && LNO_SVR && LNO_SVR_Phase1)
#endif
#endif  
       {
#ifdef KEY
       // Bug 8628 -- turn off the red_manager to let the reduction variable
       // be renamed for SVR_PHASE1
	if(red_manager)
	  red_manager->Erase(func_nd);
#endif
        if (Scalar_Variable_Renaming(func_nd))
          LNO_Build_Access(func_nd,&LNO_default_pool);
#ifdef KEY
        // Bug 8628 -- rebuild the red_manager if necessary
	if(red_manager){
	  red_manager->Build(func_nd, TRUE, FALSE);//scalar
          if (Roundoff_Level >= ROUNDOFF_ASSOC)//array only if roundoff>=2
            red_manager->Build(func_nd,FALSE,TRUE,Array_Dependence_Graph);
        }
#endif
       }          
      early_exit = Phase_123(current_pu, func_nd, do_fiz_fuse, do_p25, 
        do_inner_fission);
  
      Fission_Finish();
      Fusion_Finish();
    }
    if (early_exit) 
      goto return_point; 

    if (Do_Aggressive_Fuse) {
      if (LNO_Sclrze) {
	// Rebuild Array_Dependence_Graph
	Array_Dependence_Graph->Erase_Graph();
	Build_Array_Dependence_Graph(func_nd);	
	// Run scalarization before prefetch to avoid prefetches that are not needed.	
	Scalarize_Arrays(Array_Dependence_Graph,1,0,red_manager, func_nd);
      }
    }
    
    // Driver determines whether to run prefetching or not,
    // based on the options.
    Prefetch_Driver (func_nd, Array_Dependence_Graph);
  
    // Scalarize the variants
    if (!Do_Aggressive_Fuse) {
      if (LNO_Sclrze) 
	Scalarize_Arrays(Array_Dependence_Graph,1,0,red_manager, NULL);
    }

    if (LNO_Aequiv) { 
      AEQUIV aequiv(func_nd,Array_Dependence_Graph);
      aequiv.Equivalence_Arrays();
    }

    if (!Get_Trace(TP_LNOPT, TT_LNO_GUARD)) {
      Guard_Dos(func_nd); // put guards around all the do statments
      if (LNO_Minvar) {
        Minvariant_Removal(func_nd, Array_Dependence_Graph);
      }
    }
    
    if (!LNO_Ignore_Pragmas) {
      LNO_Insert_Pragmas(func_nd);
    }
    
#ifdef Is_True_On
    MP_Sanity_Check_Func(func_nd);
    LNO_Check_Graph(Array_Dependence_Graph);
#endif
  
    if (Get_Trace(TP_LNOPT,TT_LNO_DEP2) || 
        Get_Trace(TP_LNOPT,TT_LNO_DEP)) {
      fprintf(TFile, "%sLNO dependence graph (after transformation)\n%s",
                      DBar, DBar);
      Array_Dependence_Graph->Print(TFile);
    }

    // Use the array dependence graph to build cg's dependence graph
    Build_CG_Dependence_Graph (Array_Dependence_Graph);
  
    if (LNO_Cse && (Roundoff_Level >= ROUNDOFF_ASSOC)) { 
      Inter_Iteration_Cses(func_nd);
    }

     
#ifdef KEY // bug 10644
    //perform factorization only when the any loop is guarenteed to be executed
    //at least once

    if(!Get_Trace(TP_LNOPT, TT_LNO_GUARD) && LNO_Invariant_Factorization 
      && Roundoff_Level >= ROUNDOFF_ASSOC){
      BOOL unrolled = Unroll_before_Factorize(func_nd);
      BOOL graph_is_ok = TRUE;
      STACK_OF_WN *unroll_and_jammed_loops = CXX_NEW
          (STACK_OF_WN(&LNO_default_pool),&LNO_default_pool);
      // Rebuild the array depenedence graph
      if (unrolled && Array_Dependence_Graph)
      { 
        Array_Dependence_Graph->Erase_Graph();
        graph_is_ok =  Build_Array_Dependence_Graph (func_nd);
        if (graph_is_ok)
        { 
	  // rebuild scalar reductions
          if(red_manager){
            red_manager->Erase(func_nd);
             red_manager->Build(func_nd, TRUE, FALSE);//scalar
	  }
          Outer_Unroll_For_Factorization(func_nd, unroll_and_jammed_loops); 

          // if we unrolled and jammed then rebuild the dependence graph.

          if (unroll_and_jammed_loops->Elements() != 0)
	  { 
            Array_Dependence_Graph->Erase_Graph();
            graph_is_ok =  Build_Array_Dependence_Graph (func_nd);
	    // rebuild scalar reductions
            if(red_manager){
               red_manager->Erase(func_nd);
               red_manager->Build(func_nd, TRUE, FALSE);//scalar
	    }
          } 
	} 
      }
      if (graph_is_ok)
      { 
        BOOL rebuild_dg = Invariant_Factorization(func_nd);
        if (rebuild_dg)
        {
          Array_Dependence_Graph->Erase_Graph();
          graph_is_ok =  Build_Array_Dependence_Graph (func_nd);
          if (graph_is_ok)
	  { 
             Minvariant_Removal(func_nd, Array_Dependence_Graph);
          }
	}
        for(INT ii=0; ii<unroll_and_jammed_loops->Elements(); ii++){
            WN *loop = unroll_and_jammed_loops->Bottom_nth(ii);
            INT64 trip_count = Num_Iters(loop);
            if (trip_count >= 1 && trip_count <= LNO_Full_Unrolling_Limit) 
            { 
	       WN *first, *last;
               Unroll_Loop_By_Trip_Count(loop,trip_count);
               Remove_Unity_Trip_Loop(loop, TRUE, &first, &last, NULL, Du_Mgr);
            }
	}
      }
    }
#endif
    if (Get_Trace(TP_LNOPT,TT_LNO_DEP2) || 
        Get_Trace(TP_LNOPT,TT_LNO_DEP)) {
      fprintf(TFile, "%sLNO dep graph for CG, after LNO\n%s", DBar, DBar);
      Current_Dep_Graph->Print(TFile);
      fprintf(TFile, "%s", DBar);
    }
  
  }
return_point:

  if (Alloca_Dealloca_On && PU_has_alloca(Get_Current_PU())) {
    Convert_Intrinsic_To_Alloca_Dealloca (func_nd);
  }

#ifndef _NEW_SYMTAB
  if (LNO_Mem_Sim) {
    void Instrument_Mem_Sim (WN *wn);
    Instrument_Mem_Sim (func_nd);
  }
#endif
  Run_autopar = Run_autopar_save;
  Roundoff_Level = Roundoff_Level_Save;

  // Do the lowering of Lego pragmas after everything else in LNO.
  if (LNO_Run_Lego) {
    Lego_Lower_Pragmas (func_nd);
    Lego_PU_Fini ();
    // be driver may look at this bit after LNO is run
    // Reset_SYMTAB_mp_needs_lno(Current_Symtab);
  }
  WN_Simplify_Tree(func_nd);

  if (LNO_Analysis)
    Lisp_Loops(func_nd, LNO_Analysis); 

  WB_Set_Sanity_Check_Level(WBC_DISABLE); 
  WN_Simplifier_Enable(simp_state_save);
  WN_Simp_Fold_ILOAD = simp_fold_iload_save;
  WN_Remove_Delete_Cleanup_Function(LWN_Delete_LNO_dep_graph);

  if (red_manager) CXX_DELETE(red_manager,&LNO_default_pool);
  if (Array_Dependence_Graph) {
    CXX_DELETE(Array_Dependence_Graph,&LNO_default_pool);
    Array_Dependence_Graph = NULL;
  }

  // Free up the mappings
  WN_MAP_Delete(Parent_Map);
  // Let the simplifier know about it
  WN_SimpParentMap = WN_MAP_UNDEFINED;
  WN_MAP_Delete(LNO_Info_Map);
  WN_MAP_Delete(LNO_Precom_Map);
  WN_MAP_Delete(Stmt_Dependence_Map);
  WN_MAP_Delete(Array_Dependence_Map);
  WN_MAP_Delete(Safe_Spec_Map);
  Safe_Spec_Map = WN_MAP_UNDEFINED;

  MEM_POOL_Pop(&LNO_local_pool);
  MEM_POOL_Pop(&SNL_local_pool);
  MEM_POOL_Pop_Unfreeze(&LNO_default_pool);

  if(Run_autopar)
    WN_Remove_Delete_Cleanup_Function(LWN_Delete_SR);
  WN_Remove_Delete_Cleanup_Function(LWN_Delete_DU);
  GRAPH16_CAPACITY = save_graph_capacity; 

  if (LNO_Verbose) {
    fprintf(stdout, "Lno DONE processing procedure: %s\n", Cur_PU_Name);
    fprintf(TFile, "Lno DONE processing procedure: %s\n", Cur_PU_Name);
  }
  // fclose(LNO_Analysis); 
  MEM_POOL_Pop(&ARA_memory_pool);
  MEM_POOL_Delete(&ARA_memory_pool);
  LNO_Restore_Configs();
  return func_nd;
}


/***********************************************************************
 *
 * Build the appropriate dependence graph. 
 * Return TRUE if the graph was OK, FALSE otherwise.
 *
 ***********************************************************************/
extern BOOL Build_Array_Dependence_Graph (WN* func_nd) {

  Array_Dependence_Graph = 
    CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,Array_Dependence_Map,
                                   DEPV_ARRAY_ARRAY_GRAPH), &LNO_default_pool);
  BOOL graph_ok=Array_Dependence_Graph->Build(func_nd,&LNO_default_pool);
  WB_Set_Sanity_Check_Level(WBC_DU_AND_ARRAY); 
  WN_Register_Delete_Cleanup_Function(LWN_Delete_LNO_dep_graph);

  // Is_True(graph_ok,("Overflow building dependence graph"));
  if (!graph_ok) return FALSE;
  if (Get_Trace(TP_LNOPT,TT_LNO_DEP2) || 
      Get_Trace(TP_LNOPT,TT_LNO_DEP)) {
    fprintf(TFile, "%sLNO dependence graph (before transformation)\n%s",
            DBar, DBar);
    Array_Dependence_Graph->Print(TFile);
  }
  return TRUE;
}

/***********************************************************************
 *
 * Build dependence graph for CG from scratch.
 *
 ***********************************************************************/
extern void Build_CG_Dependence_Graph (WN* func_nd) {
  if (!Current_Dep_Graph) {
    Current_Dep_Graph = CXX_NEW(ARRAY_DIRECTED_GRAPH16(100, 500,
    	WN_MAP_DEPGRAPH, DEP_ARRAY_GRAPH), Malloc_Mem_Pool);
    Set_PU_Info_depgraph_ptr(Current_PU_Info,Current_Dep_Graph);
    Set_PU_Info_state(Current_PU_Info,WT_DEPGRAPH,Subsect_InMem);
  }
  // Build cg's dependence graph from scratch
  BOOL graph_ok=Current_Dep_Graph->Build(func_nd);
#ifndef KEY
  Is_True(graph_ok,("Overflow converting to cg dependence graph"));
#else
#ifdef Is_True_On 
  if (!graph_ok)
    DevWarn("Overflow converting to cg dependence graph");
#endif
#endif
  if (!graph_ok) Current_Dep_Graph->Erase_Graph();

  if (graph_ok) {
    if (Get_Trace(TP_LNOPT,TT_LNO_DEP2) || 
        Get_Trace(TP_LNOPT,TT_LNO_DEP)) {
      fprintf(TFile, "%sLNO dep graph for CG, after LNO\n%s", DBar, DBar);
      Current_Dep_Graph->Print(TFile);
      fprintf(TFile, "%s", DBar);
    }
  }
  WN_Register_Delete_Cleanup_Function(LWN_Delete_CG_dep_graph);
}

/***********************************************************************
 *
 * Use LNO Array_Dependence_Graph to build dependence graph for CG.
 *
 ***********************************************************************/
extern void Build_CG_Dependence_Graph (ARRAY_DIRECTED_GRAPH16*
                                       Array_Dependence_Graph) {
  // Use the array dependence graph to build cg's dependence graph
  if (!Current_Dep_Graph) {
    Current_Dep_Graph = CXX_NEW(ARRAY_DIRECTED_GRAPH16(100, 500,
    	WN_MAP_DEPGRAPH, DEP_ARRAY_GRAPH), Malloc_Mem_Pool);
    Set_PU_Info_depgraph_ptr(Current_PU_Info,Current_Dep_Graph);
    Set_PU_Info_state(Current_PU_Info,WT_DEPGRAPH,Subsect_InMem);
  }
  BOOL graph_ok=Current_Dep_Graph->Build(Array_Dependence_Graph);
  Is_True(graph_ok,("Overflow converting to cg dependence graph"));
  if (!graph_ok) Current_Dep_Graph->Erase_Graph();
  WN_Register_Delete_Cleanup_Function(LWN_Delete_CG_dep_graph);
}


void EST_REGISTER_USAGE::Set_Est_Regs(
INT fp_est, INT fp_regs_available,
INT int_est, INT int_regs_available,
INT tlb_est, INT tlb_available)
{
  _fp_est = fp_est;
  _int_est = int_est;
  _tlb_est = tlb_est;
  _fits = (_fp_est >= 0 && fp_regs_available >= 0 &&
            fp_est <= fp_regs_available &&
           _int_est >= 0 && int_regs_available >= 0 &&
            int_est <= int_regs_available &&
           _tlb_est >= 0 && tlb_available >= 0 &&
            tlb_est <= tlb_available);
  _no_fit = (_fp_est >= 0 && fp_regs_available >= 0 &&
             fp_est > fp_regs_available ||
             _int_est >= 0 && int_regs_available >= 0 &&
             int_est > int_regs_available);
}

void EST_REGISTER_USAGE::Print(FILE* f)
{
  fprintf(f, "fp est=%d, int est=%d, tlb est=%d <%s>", _fp_est, _int_est,
	_tlb_est,
	_fits ? "FITS" : _no_fit ? "NO FIT" : "DON'T KNOW IF FITS");
}

void DO_LOOP_INFO::Print(FILE *fp, INT indentation)
{
  char buf[80];
  INT i;

  for (i = 0; i < indentation && i < 79; i++)
    buf[i] = ' ';
  buf[i] = '\0';

  if (Has_Calls) fprintf(fp,"%sIt has calls \n", buf);
#ifdef KEY //bug 14284
  if (Has_Nested_Calls) fprintf(fp,"%sIt has calls to nested functions \n", buf);
#endif
  if (Has_Unsummarized_Calls) fprintf(fp,"%sIt has unsummarized calls \n", buf);
  if (Has_Unsummarized_Call_Cost) 
	fprintf(fp,"%sIt has unsummarized call cost \n", buf);
  if (Has_Threadprivate) 
        fprintf(fp,"%sIt has THREADPRIVATE variables \n", buf);
  if (Has_Gotos) fprintf(fp,"%sIt has non-DO or non-IF control flow\n", buf);
  if (Has_Gotos_This_Level) fprintf(fp,"%sIt has non-DO or non-IF control flow to this level\n", buf);
  if (Has_Exits) fprintf(fp,"%sIt has exits\n", buf);
  if (Has_Bad_Mem) fprintf(fp,"%sIt has bad memory references \n", buf);
  if (Has_Barriers) fprintf(fp,"%sIt has barriers\n", buf);
  if (Cannot_Interchange) fprintf(fp,"%sPragma says can't interchange\n", buf);
  if (Cannot_Block) fprintf(fp,"%sPragma says can't block\n", buf);
  if (Pragma_Cannot_Concurrentize) 
    fprintf(fp,"%sPragma says can't concurrentize\n", buf);
  if (Pragma_Prefer_Concurrentize)
    fprintf(fp,"%sPragma says prefer to concurrentize this loop\n", buf); 
  if (Serial_Version_of_Concurrent_Loop)
    fprintf(fp, "%sLoop is in serial version of parallel loop\n", buf); 
  if (Auto_Parallelized) 
    fprintf(fp,"%sAuto Parallelized\n", buf); 
  if (Required_Unroll) fprintf(fp,"%sPragma requires %d unrolls\n", buf,
                               Required_Unroll);
  BOOL required_blks = FALSE;
  for (i = 0; i < MHD_MAX_LEVELS; i++)
    if (Required_Blocksize[i] >= 0)
      required_blks = TRUE;
  if (required_blks) {
    fprintf(fp,"%sPragma requires blocksizes of", buf);
    for (i = 0; i < MHD_MAX_LEVELS; i++)
      if (Required_Blocksize[i] >= 0)
        fprintf(fp," L%d=%d", i+1, Required_Blocksize[i]);
    fprintf(fp,"\n");
  }
  if (Blockable_Specification)
    fprintf(fp,"%sThe %d loops from here in are blockable, says a pragma\n",
            buf, Blockable_Specification);
  if (Permutation_Spec_Count > 0) {
    fprintf(fp,"%sThe permutation requested in a pragma, from here in:", buf);
    for (INT i = 0; i < Permutation_Spec_Count; i++)
      fprintf(fp," %d",Permutation_Spec_Array[i]);
    fprintf(fp,"\n");
  }
    
  if (Required_Unroll) fprintf(fp,"%sPragma requires %d unrolls\n", buf,
                               Required_Unroll);
  if (_wind_down_flags) {
    fprintf(fp, "%s_wind_down_flags=", buf);
    if (_wind_down_flags&CWD) fprintf(fp, "<cache winddown>");
    if (_wind_down_flags&RWD) fprintf(fp, "<reg winddown>");
    if (_wind_down_flags&ICWD) fprintf(fp, "<in cache winddown>");
    if (_wind_down_flags&IRWD) fprintf(fp, "<in reg winddown>");
    if (_wind_down_flags&UNIMPORTANT) fprintf(fp, "<generally unimportant>");
    fprintf(fp, "\n");
  }
  if (Est_Register_Usage.Fits() || Est_Register_Usage.Does_Not_Fit() ||
      Est_Register_Usage.Est_Fp_Regs() >= 0 ||
      Est_Register_Usage.Est_Int_Regs() >= 0) {
    fprintf(fp, "%sEst_Register_Usage: ", buf);
    Est_Register_Usage.Print(fp);
    fprintf(fp, "\n");
  }
  if (Is_Inner)
    fprintf(fp,"%sIs_Inner is %d \n", buf, Is_Inner);
  if (Is_Backward)
    fprintf(fp,"%sIs_Backward is %d \n", buf, Is_Backward);
  if (Is_Outer_Lego_Tile)
    fprintf(fp,"%sIs_Outer_Lego_Tile is %d \n", buf, Is_Outer_Lego_Tile);
  if (Is_Inner_Lego_Tile)
    fprintf(fp,"%sIs_Inner_Lego_Tile is %d \n", buf, Is_Inner_Lego_Tile);
  if (Is_Processor_Tile)
    fprintf(fp,"%sIs_Processor_Tile is %d \n", buf, Is_Processor_Tile);
  if (Suggested_Parallel)
    fprintf(fp,"%sSuggested_Parallel is %d \n", buf, Suggested_Parallel);
  if (Parallelizable)
    fprintf(fp,"%sParallelizable is %d \n", buf, Parallelizable);
#ifdef KEY
  if (Vectorizable)
    fprintf(fp,"%sVectorizable is %d \n", buf, Vectorizable);
#endif
  if (Last_Value_Peeled)
    fprintf(fp,"%sLast Value Peeled is %d \n", buf, Last_Value_Peeled); 
  if (Not_Enough_Parallel_Work)
    fprintf(fp,"%sNot_Enough_Parallel_Work is %d \n", buf, 
      Not_Enough_Parallel_Work); 
  if (Inside_Critical_Section)
    fprintf(fp, "%sInside_Critical_Section is %d \n", buf,
      Inside_Critical_Section);
  if (Is_Doacross) {
    fprintf(fp, "Is_Doacross is 1 \n");
    fprintf(fp, "Doacross_Tile_Size is %d \n", Doacross_Tile_Size);
    fprintf(fp, "Sync_Distances[0] = %d \n", Sync_Distances[0]);
    fprintf(fp, "Sync_Distances[1] = %d \n", Sync_Distances[1]);
  }
  if (Is_Ivdep) fprintf(fp,"Is_Ivdep is 1 \n");
  if (Is_Concurrent_Call) fprintf(fp,"%sIs_Concurrent_Call is 1 \n",buf);
  if (Concurrent_Directive) fprintf(fp,"%sConcurrent_Directive is 1 \n",buf);
  if (Work_Estimate != 0)
    fprintf(fp,"%sWork_Estimate is %g \n", buf, Work_Estimate);
  if (Lego_Mp_Key_Lower != 0 || Lego_Mp_Key_Upper != 0)
    fprintf(fp,"%sLego_Mp_Tile Key [%d:%d;%d] \n", buf, 
      Lego_Mp_Key_Lower, Lego_Mp_Key_Upper, Lego_Mp_Key_Depth); 
  if (Lego_LB_Symbols) {
    for (INT i=0; i<Lego_Mp_Key_Upper-Lego_Mp_Key_Lower+1; i++)
      Lego_LB_Symbols[i].Print(fp);
  }
  fprintf(fp,"%sDepth is %d \n", buf, Depth);
  if (Lego_Info != NULL) { 
    fprintf(fp,"%sLego Info is: \n", buf); 
    Lego_Info->Print(fp); 
  }
  if (Mp_Info != NULL) {
    fprintf(fp,"%sMp Info is: \n", buf); 
    Mp_Info->Print(fp); 
  }
  fprintf(fp,"%sThe lb is ", buf);
  if (LB) LB->Print(fp,TRUE);
  else fprintf(fp, "<null>\n");
  fprintf(fp,"%sThe ub is ", buf);
  if (UB) UB->Print(fp,TRUE);
  else fprintf(fp, "<null>\n");
  fprintf(fp,"%sThe step is ", buf);
  if (Step) {
    Step->Print(fp);
    fprintf(fp,"\n");
  }
  else
    fprintf(fp, "<null>\n");
  fprintf(fp,"%sWe estimate this loop has %lld iterations.  %s\n",
	  buf, Est_Num_Iterations, Num_Iterations_Symbolic?"<symbolic>":"");
  if (Est_Max_Iterations_Index != -1) 
    fprintf(fp,"%sThis loop has at most %lld iterations due to array bounds.\n",
          buf, Est_Max_Iterations_Index); 
  if (Is_Inner_Tile) 
    if (Tile_Size > 0)
      fprintf(fp, "%sLoop is an inner tile with tile size of %d.\n", 
        buf, Tile_Size); 
    else 
      fprintf(fp, "%sLoop is an inner tile with unknown tile size.\n", 
        buf); 
  if (Is_Outer_Tile) 
    fprintf(fp, "%sLoop is an outer tile.\n", buf); 
  if (ARA_Info != NULL)
    ARA_Info->WB_Print(fp); 
}

static void Fiz_Fuse_Phase(WN* body, FIZ_FUSE_INFO *ffi)
{
  FmtAssert(WN_opcode(body) == OPC_BLOCK, ("Bad block to Fiz_Fuse_Phase()"));

  WN* next_wn = NULL;
  for (WN* wn = WN_first(body); wn; wn = next_wn) {
    next_wn = WN_next(wn);
    OPCODE	opc = WN_opcode(wn);

    switch (opc) {
     case OPC_DO_LOOP:
      if (Do_Loop_Is_Mp(wn)) {
        Fiz_Fuse_Phase(WN_do_body(wn),ffi);
      } else {
        MEM_POOL_Push(&LNO_local_pool);
        *ffi += *Fiz_Fuse(wn,ffi,&LNO_default_pool);
        MEM_POOL_Pop(&LNO_local_pool);
      }
      break;
     case OPC_REGION:
       Fiz_Fuse_Phase(WN_region_body(wn),ffi);
       break;
     case OPC_IF:
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      (void)If_While_Region_Fiz_Fuse(wn, ffi, &LNO_default_pool);
      break;
    }
  }
}

// call Fiz_Fuse on outermost loops, then call SNL_Transform
// on the SNLs found.
extern BOOL Phase_123(PU_Info* current_pu, WN* func_nd, 
		      BOOL do_fiz_fuse, BOOL do_phase25,
                      BOOL do_inner_fission)
{
#ifdef Is_True_On
  if (LNO_Verbose) {
    fprintf(stdout, "Sanity check on 123 entry\n");
    fflush(stdout);
  }
  LWN_Check_Parentize(func_nd);
  SNL_Sanity_Check_Func(func_nd);
  MP_Sanity_Check_Func(func_nd);
  LNO_Check_Graph(Array_Dependence_Graph);
  if (LNO_Verbose) {
    fprintf(stdout, "Sanity check on 123 entry complete\n");
    fflush(stdout);
  }
#endif

  if (snl_debug >= 3) {
    WN* f2 = WN_Simplify_Tree(func_nd);
    Is_True(f2 == func_nd, ("Bug in simplification test code"));
    LWN_Parentize(func_nd);
  }

  FIZ_FUSE_INFO *ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_default_pool),&LNO_default_pool);

  FIZ_FUSE_INFO *new_ffi=
      CXX_NEW(FIZ_FUSE_INFO(&LNO_default_pool),&LNO_default_pool);

  FIZ_FUSE_INFO *outer_ffi=
      CXX_NEW(FIZ_FUSE_INFO(&LNO_default_pool),&LNO_default_pool);

  if (do_fiz_fuse) {
    Fiz_Fuse_Phase(WN_func_body(func_nd), ffi);
#ifdef TARG_X8664
    SNL_Lite_Phase(func_nd);
#endif

#ifdef Is_True_On
    MP_Sanity_Check_Func(func_nd);
    LNO_Check_Graph(Array_Dependence_Graph);
#endif

    outer_ffi->Build(func_nd);
    if (LNO_Test_Dump)
      for (INT i = 0; i < outer_ffi->Num_Snl(); i++)
        outer_ffi->Print(i,TFile);

#ifdef Is_True_On
    outer_ffi->Check();
#endif
    if (LNO_Run_Outer && LNO_Fusion!=0)
      Outer_Loop_Fusion_Phase(func_nd, outer_ffi);

    new_ffi->Build(func_nd);
    if (LNO_Test_Dump)
      for (INT i = 0; i < new_ffi->Num_Snl(); i++)
        new_ffi->Print(i,TFile);

#ifdef Is_True_On
    new_ffi->Check();
#endif
  }

  if (do_fiz_fuse) {

#ifdef Is_True_On
    if (LNO_Verbose) {
      fprintf(stdout, "Sanity check after fiz_fuse phase\n");
      fflush(stdout);
    }
    LWN_Check_Parentize(func_nd);
    SNL_Sanity_Check_Func(func_nd);
    MP_Sanity_Check_Func(func_nd);
    LNO_Check_Graph(Array_Dependence_Graph);
    if (LNO_Verbose) {
      fprintf(stdout, "Sanity check after fiz_fuse phase complete\n");
      fflush(stdout);
    }
#endif
  }

  Hoist_Messy_Bounds(func_nd); 
  Finalize_Loops(func_nd); 
  Parallel_And_Padding_Phase(current_pu, func_nd);
  // Run the shackling phase
  SHACKLE_Phase(func_nd);

  SNL_Phase(func_nd);
   

//  void Hoist_Outer_Invar(WN *func_nd);
//  Hoist_Outer_Invar(func_nd);

#ifdef Is_True_On
  if (LNO_Verbose) {
    fprintf(stdout, "Sanity check after phase 2\n");
    fflush(stdout);
  }
  LWN_Check_Parentize(func_nd);
  SNL_Sanity_Check_Func(func_nd);
  MP_Sanity_Check_Func(func_nd);
  LNO_Check_Graph(Array_Dependence_Graph);
  if (LNO_Verbose) {
    fprintf(stdout, "Sanity check after phase 2 complete\n");
    fflush(stdout);
  }
#endif

  if (Get_Trace(TP_LNOPT2, TT_SHACKLE_ONLY)
      || Get_Trace(TP_LNOPT2, TT_TILE_ONLY))
    return TRUE; 

  if (do_phase25 && LNO_Fission!=0 && LNO_Gather_Scatter!=0) {
    
    if (!Get_Trace(TP_LNOPT, TT_LNO_NORENAME))
      // rename after phase 2
#ifdef KEY
      if (!Skip_SVR && LNO_SVR)
#endif
      if (Scalar_Variable_Renaming(func_nd))
        LNO_Build_Access(func_nd,&LNO_default_pool);

    Fiss_Gather_Loop(func_nd, Array_Dependence_Graph);

#ifdef Is_True_On
    if (LNO_Verbose) {
      fprintf(stdout, "Sanity check after phase 2.5\n");
      fflush(stdout);
    }
    LWN_Check_Parentize(func_nd);
    SNL_Sanity_Check_Func(func_nd);
    MP_Sanity_Check_Func(func_nd);
    LNO_Check_Graph(Array_Dependence_Graph);
    if (LNO_Verbose) {
      fprintf(stdout, "Sanity check after phase 2.5 complete\n");
      fflush(stdout);
    }
#endif
  }

#ifndef KEY
  if ((do_inner_fission || LNO_Run_Vintr==TRUE) && LNO_Fission!=0)
#else
  if ((do_inner_fission || LNO_Run_Vintr > 0) && LNO_Fission!=0)
#endif
    if (!Get_Trace(TP_LNOPT, TT_LNO_NORENAME))
      // rename after phase 2
#ifdef KEY
      if (!Skip_SVR && LNO_SVR)
#endif
      if (Scalar_Variable_Renaming(func_nd))
        LNO_Build_Access(func_nd,&LNO_default_pool);

  if (do_inner_fission && LNO_Fission!=0) {

    void Inner_Fission(WN* func_nd,
                       ARRAY_DIRECTED_GRAPH16* Array_Dependence_Graph);
    Inner_Fission(func_nd,Array_Dependence_Graph);
  }

  LNO_Lower_Memlib(func_nd);

#ifdef TARG_X8664
  void Simd_Phase(WN* func_nd);
  if (LNO_Run_Simd && LNO_Run_Simd_Set && !Skip_Simd && Is_Target_SSE2())
  {
    Array_Substitution(func_nd);
    Simd_Phase(func_nd);
  }
  void HoistIf_Phase(WN* func_nd);
  if (LNO_Run_hoistif==TRUE && !Skip_HoistIf)
    HoistIf_Phase(func_nd);
#endif /* KEY */
//Sicortex bug 5073: Do an additional pass of array substutution. We need
//to rebuild reduction manager because reduction arrays may be replaced.
#ifdef KEY
#ifdef TARG_MIPS
Array_Substitution(func_nd);
if(red_manager){
  red_manager->Erase(func_nd);
  red_manager->Build(func_nd, TRUE, FALSE);//scalar
  if (Roundoff_Level >= ROUNDOFF_ASSOC) //array
   red_manager->Build(func_nd,FALSE,TRUE,Array_Dependence_Graph);
 }
#endif
#endif

  void Vintrinsic_Fission_Phase(WN* func_nd);
#ifndef KEY
  if (LNO_Run_Vintr==TRUE)
#else
  if (LNO_Run_Vintr)
#endif
    Vintrinsic_Fission_Phase(func_nd);
  Finalize_Loops(func_nd); 

#ifdef Is_True_On
    if (LNO_Verbose) {
      fprintf(stdout, "Sanity check after inner_fission phase\n");
      fflush(stdout);
    }
    LWN_Check_Parentize(func_nd);
    SNL_Sanity_Check_Func(func_nd);
    MP_Sanity_Check_Func(func_nd);
    LNO_Check_Graph(Array_Dependence_Graph);
    if (LNO_Verbose) {
      fprintf(stdout, "Sanity check after inner_fission phase complete\n");
      fflush(stdout);
    }
#endif
  return FALSE; 
}

// NB: the two constructors for DO_LOOP_INFO need to remain in sync.
DO_LOOP_INFO::DO_LOOP_INFO(MEM_POOL *pool, ACCESS_ARRAY *lb, ACCESS_ARRAY *ub,
	ACCESS_VECTOR *step, BOOL has_calls, BOOL has_nested_calls, BOOL has_unsummarized_calls,
	BOOL has_unsummarized_call_cost, BOOL has_gotos, BOOL has_exits, 
	BOOL has_gotos_this_level,BOOL is_inner)
{
    _pool = pool;
    _id = 0;
    LB = lb;
    UB = ub;
    Step = step;
    Has_Calls = has_calls;
    Has_Nested_Calls = has_nested_calls;
    Has_Unsummarized_Calls = has_unsummarized_calls;
    Has_Unsummarized_Call_Cost = has_unsummarized_call_cost;
    Has_Threadprivate = FALSE; 
    Has_Gotos = has_gotos;
    Has_Conditional = FALSE;
    Has_Gotos_This_Level = has_gotos_this_level;
    Has_Exits = has_exits;
    Has_EH_Regions = FALSE;
    Is_Inner = is_inner;
    Has_Bad_Mem = FALSE;
    Has_Barriers = FALSE;
    Multiversion_Alias = FALSE;
    Loop_Vectorized = FALSE;
    Loop_Align_Peeled = FALSE;
    Is_Ivdep = FALSE;
    Is_Concurrent_Call = FALSE;
    Concurrent_Directive = FALSE;
    No_Fission = FALSE;
    No_Fusion = FALSE; 
    Aggressive_Inner_Fission = FALSE;
    Depth = 0;
    _wind_down_flags = 0;
    Est_Num_Iterations = -1;
    Est_Max_Iterations_Index = -1;
    // Est_Register_Usage has it's own constructor
    // Matching comment (provides a hint for tkdiff).
    Num_Iterations_Symbolic = TRUE;
    Num_Iterations_Profile = FALSE;
    Guard = NULL;
    Cannot_Interchange = FALSE;
    Cannot_Block = FALSE;
    Pragma_Cannot_Concurrentize = FALSE; 
    Pragma_Prefer_Concurrentize = FALSE; 
    Serial_Version_of_Concurrent_Loop = FALSE; 
    Auto_Parallelized = FALSE; 
    Required_Unroll = 0;
    Prefer_Fuse = 0;
    Has_Precom_Def = 0;
    Has_Precom_Use = 0;
    Is_Precom_Init = 0;
    Sclrze_Dse = 0;
    for (INT i = 0; i < MHD_MAX_LEVELS; i++)
      Required_Blocksize[i] = -1;
    Permutation_Spec_Array = NULL;
    Permutation_Spec_Count = 0;
    // Matching comment (provides a hint for tkdiff).
    Blockable_Specification = 0;
    Is_Inner_Tile = FALSE; 
    Is_Outer_Tile = FALSE; 
    Tile_Size = 0;  
    Is_Backward = FALSE; 
    Is_Outer_Lego_Tile = FALSE; 
    Is_Inner_Lego_Tile = FALSE; 
    Is_Processor_Tile = FALSE; 
    Suggested_Parallel = FALSE; 
    Is_Doacross = FALSE; 
    Doacross_Tile_Size = 0; 
    Sync_Distances[0] = NULL_DIST; 
    Sync_Distances[1] = NULL_DIST; 
    Parallelizable = FALSE; 
#ifdef KEY
    Vectorizable = FALSE; 
    Delay_Full_Unroll = FALSE;
#endif
    Last_Value_Peeled = FALSE; 
    Not_Enough_Parallel_Work = FALSE; 
    Inside_Critical_Section = FALSE;
    Work_Estimate = 0.0; 
    Lego_Mp_Key_Lower = 0; 
    Lego_Mp_Key_Upper = 0; 
    Lego_Mp_Key_Depth = 0; 
    Lego_LB_Symbols = NULL;
    Lego_Info = NULL; 
    Mp_Info = NULL; 
    ARA_Info = NULL; 
}

// Until 7.3, when we create a copy constructor for the ARA_LOOP_INFO, 
// Use this mechanism to indicate that it's OK to copy a DO_LOOP_INFO 
// with an ARA_LOOP_INFO, because we are going to discard the ARA in-    
// formation soon. Right now, this should only happen if we are doing
// last value peeling. 

static INT last_value_peeling = FALSE; 

extern void Last_Value_Peeling_On()
{ 
  last_value_peeling = TRUE; 
} 

extern void Last_Value_Peeling_Off()
{ 
  last_value_peeling = FALSE; 
} 

extern BOOL Last_Value_Peeling()
{ 
  return last_value_peeling; 
} 

// NB: the two constructors for DO_LOOP_INFO need to remain in sync.
DO_LOOP_INFO::DO_LOOP_INFO(DO_LOOP_INFO *dli, MEM_POOL *pool)
{
    _pool = pool;
    _id = 0;
    if (dli->LB) LB = CXX_NEW(ACCESS_ARRAY(dli->LB,pool),pool);
    if (dli->UB) UB = CXX_NEW(ACCESS_ARRAY(dli->UB,pool),pool);
    if (dli->Step) Step = CXX_NEW(ACCESS_VECTOR(dli->Step,pool),pool);
    Has_Calls = dli->Has_Calls;
    Has_Nested_Calls = dli->Has_Nested_Calls;
    Has_Unsummarized_Calls = dli->Has_Unsummarized_Calls;
    Has_Unsummarized_Call_Cost = dli->Has_Unsummarized_Call_Cost;
    Has_Threadprivate = dli->Has_Threadprivate; 
    Has_Gotos = dli->Has_Gotos;
    Has_Conditional = dli->Has_Conditional;
    Has_Gotos_This_Level = dli->Has_Gotos_This_Level;
    Has_Exits = dli->Has_Exits;
    Has_EH_Regions = dli->Has_EH_Regions;
    Is_Inner = dli->Is_Inner;
    Has_Bad_Mem = dli->Has_Bad_Mem;
    Has_Barriers = dli->Has_Barriers;
    Multiversion_Alias = dli->Multiversion_Alias;
    Loop_Vectorized = dli->Loop_Vectorized;
    Loop_Align_Peeled = dli->Loop_Align_Peeled;
    Is_Ivdep = dli->Is_Ivdep;
    Is_Concurrent_Call = dli->Is_Concurrent_Call;
    Concurrent_Directive = dli->Concurrent_Directive;
    No_Fission = dli->No_Fission;
    No_Fusion = dli->No_Fusion;
    Aggressive_Inner_Fission = dli->Aggressive_Inner_Fission;
    Depth = dli->Depth;
    _wind_down_flags = 0;
    Est_Num_Iterations = dli->Est_Num_Iterations;
    Est_Max_Iterations_Index = dli->Est_Max_Iterations_Index;
    Est_Register_Usage = dli->Est_Register_Usage;
    // Matching comment (provides a hint for tkdiff).
    Num_Iterations_Symbolic = dli->Num_Iterations_Symbolic;
    Num_Iterations_Profile = dli->Num_Iterations_Profile;
    Guard = dli->Guard;
    Cannot_Interchange = dli->Cannot_Interchange;
    Cannot_Block = dli->Cannot_Block;
    Pragma_Cannot_Concurrentize = dli->Pragma_Cannot_Concurrentize; 
    Pragma_Prefer_Concurrentize = dli->Pragma_Prefer_Concurrentize; 
    Serial_Version_of_Concurrent_Loop = dli->Serial_Version_of_Concurrent_Loop;
    Auto_Parallelized = dli->Auto_Parallelized; 
    Required_Unroll = dli->Required_Unroll;
    Prefer_Fuse = dli->Prefer_Fuse;
    Has_Precom_Def = dli->Has_Precom_Def;
    Has_Precom_Use = dli->Has_Precom_Use;
    Is_Precom_Init = dli->Is_Precom_Init;
    Sclrze_Dse = dli->Sclrze_Dse;
    for (INT i = 0; i < MHD_MAX_LEVELS; i++)
      Required_Blocksize[i] = dli->Required_Blocksize[i];
    Permutation_Spec_Array = NULL;  
    Permutation_Spec_Count = dli->Permutation_Spec_Count;
    if (Permutation_Spec_Count > 0) {
      Permutation_Spec_Array = CXX_NEW_ARRAY(INT, Permutation_Spec_Count, 
	pool); 
      for (INT i = 0; i < Permutation_Spec_Count; i++) 
	Permutation_Spec_Array[i] = dli->Permutation_Spec_Array[i]; 
    }
    // Matching comment (provides a hint for tkdiff).
    Blockable_Specification = dli->Blockable_Specification;
    Is_Inner_Tile = dli->Is_Inner_Tile; 
    Is_Outer_Tile = dli->Is_Outer_Tile; 
    Tile_Size = dli->Tile_Size; 
    Is_Backward = dli->Is_Backward;
    Is_Outer_Lego_Tile = dli->Is_Outer_Lego_Tile;
    Is_Inner_Lego_Tile = dli->Is_Inner_Lego_Tile;
    Is_Processor_Tile = dli->Is_Processor_Tile;
    Suggested_Parallel = dli->Suggested_Parallel; 
    Is_Doacross = dli->Is_Doacross; 
    Doacross_Tile_Size = dli->Doacross_Tile_Size; 
    Sync_Distances[0] = dli->Sync_Distances[0]; 
    Sync_Distances[1] = dli->Sync_Distances[1]; 
    Parallelizable = dli->Parallelizable; 
#ifdef KEY
    Vectorizable = dli->Vectorizable; 
    Delay_Full_Unroll = dli->Delay_Full_Unroll; 
#endif
    Last_Value_Peeled = dli->Last_Value_Peeled; 
    Not_Enough_Parallel_Work = dli->Not_Enough_Parallel_Work; 
    Inside_Critical_Section = dli->Inside_Critical_Section;
    Work_Estimate = dli->Work_Estimate; 
    Lego_Mp_Key_Lower = dli->Lego_Mp_Key_Lower; 
    Lego_Mp_Key_Upper = dli->Lego_Mp_Key_Upper; 
    Lego_Mp_Key_Depth = dli->Lego_Mp_Key_Depth; 
    if (dli->Lego_LB_Symbols) {
      INT nloops = Lego_Mp_Key_Upper - Lego_Mp_Key_Lower + 1;
      Lego_LB_Symbols = CXX_NEW_ARRAY (SYMBOL, nloops, LEGO_pool);
      for (INT i=0; i<nloops; i++) {
        Lego_LB_Symbols[i] = dli->Lego_LB_Symbols[i];
      }
    }
    else Lego_LB_Symbols = dli->Lego_LB_Symbols;
    Lego_Info = NULL; 
    if (dli->Lego_Info != NULL) 
      Lego_Info = CXX_NEW(LEGO_INFO(dli->Lego_Info, LEGO_pool), LEGO_pool); 
    Mp_Info = NULL; 
    if (dli->Mp_Info != NULL) 
      Mp_Info = CXX_NEW(MP_INFO(dli->Mp_Info), pool); 
    ARA_Info = NULL; 
    if (dli->ARA_Info != NULL) {
      if (Last_Value_Peeling())
        Last_Value_Peeled = TRUE; 
      else 
        DevWarn("No copy constructor for ARA_Info"); 
    } 
}

void REGION_INFO::Print(FILE* fp)
{ 
  if (this == NULL) { 
    fprintf(fp, "<NULL>\n"); 
    return; 
  } 
  if (_auto_parallelized)
    fprintf(fp, "Auto Parallelized\n"); 
  else 
    fprintf(fp, "User Created\n"); 
} 

static INT Check_Vertices_Traverse(WN* wn_tree)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT error_count = 0; 
  OPERATOR opr = WN_operator(wn_tree);
  if (opr == OPR_ILOAD || opr == OPR_ISTORE) { 
    if (dg->Get_Vertex(wn_tree) == 0) { 
      BOOL found_block = FALSE; 
      WN *wn;
      for (wn = wn_tree; wn != NULL; wn = LWN_Get_Parent(wn)) {
	if (WN_operator(wn) == OPR_BLOCK)
	  found_block = TRUE; 
	if (found_block && WN_operator(wn) == OPR_DO_LOOP)  
	  break; 
      } 
      if (Do_Loop_Is_Good(wn)) { 
	error_count++;
	fprintf(stdout, "0x%p missing vertex\n", wn_tree);
      } 
    } 
  } 
  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      error_count += Check_Vertices_Traverse(wn);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      error_count += Check_Vertices_Traverse(WN_kid(wn_tree, i));
  }
  return error_count;  
} 

extern INT cv()
{
  return Check_Vertices_Traverse(Current_Func_Node);
}

extern INT cl()
{  
  INT error_count = 0; 
  LWN_ITER* itr = LWN_WALK_TreeIter(Current_Func_Node);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn);
    if (def_list == NULL)
      continue;
    WN* wn_loop = def_list->Loop_stmt();
    if (wn_loop == NULL)
      continue; 
    WN *wnn;
    for (wnn = wn; wnn != NULL; wnn = LWN_Get_Parent(wnn))
      if (WN_operator(wnn) == OPR_DO_LOOP && wnn == wn_loop) 
	break; 
    if (wnn == NULL) { 
      error_count++; 
      fprintf(stdout, "0x%p bad loop stmt 0x%p\n", wn, wn_loop);
    } 
  }
  return error_count;
}  
// the map to keep track of the deleted loops (because of unroll, etc)
extern HASH_TABLE<WN*, BOOL> *Deleted_Loop_Map;

static BOOL
Unroll_before_Factorize(WN *func_nd)
{ 
  if (!LNO_New_Invariant_Factorization) return FALSE; 

   //identify loops where full unrolling is delayed
   STACK_OF_WN *inner_do_stack = CXX_NEW
          (STACK_OF_WN(&LNO_default_pool),&LNO_default_pool);
   for (LWN_ITER* itr = LWN_WALK_TreeIter(func_nd);
        itr;
        itr = LWN_WALK_TreeNext(itr)){
         WN* wn = itr->wn;
         if (WN_operator(wn) == OPR_DO_LOOP){
            DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
            if (dli->Delay_Full_Unroll == TRUE)
  	    {
              Array_Substitution(wn); 
              for (LWN_ITER* itr2 = LWN_WALK_TreeIter(WN_do_body(wn));
                             itr2;
                   itr2 = LWN_WALK_TreeNext(itr2)){
                   WN* wn = itr2->wn;
                   if (WN_operator(wn) == OPR_DO_LOOP){
                       DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
                       if(dli && dli->Is_Inner)
                          inner_do_stack->Push(wn);
		   }
	      }
	    }
	 }
   }

  BOOL unrolled = FALSE; 
  for(INT ii=0; ii<inner_do_stack->Elements(); ii++){
     WN *loop = inner_do_stack->Bottom_nth(ii);

     // Unroll the inner loops.  Currently we will unroll 2 levels.
     INT current_level=1;
     while (current_level < MAX_INNER_LOOPS)
     { 
        if (Deleted_Loop_Map->Find(loop)) break;
        WN *parent = LWN_Get_Parent(loop);
        WN *outer_loop = Enclosing_Do_Loop(parent);
        if (outer_loop == NULL) break;
        WN *loopbody = WN_do_body(loop);
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
        unrolled |= Fully_Unroll_Short_Loops(loop);
        loop = outer_loop;
        current_level++;
     }
  }

  return unrolled; 
}

static WN *
Unroll_and_Jam(WN *loop, INT ufactor)
{ 

  INT nloops = 1 + Num_Inner_Loops(loop);

  if (!Fully_Permutable_Permutation(loop, nloops)) return NULL; 

  DO_LOOP_INFO *dli = Get_Do_Loop_Info(loop);
  SNL_NEST_INFO ni(loop, nloops, &LNO_default_pool, TRUE);

  // before calling the function SNL_Regtile_Loop, the condition that
  // all the scalars must be expanded should be satisfied. 
  if ( !ni.All_Var_Expandable(nloops) )
    return NULL;
  INT outer = Do_Depth(loop);

  EST_REGISTER_USAGE est_register_usage =
      Get_Do_Loop_Info(loop)->Est_Register_Usage;
  SNL_REGION ujm = SNL_Regtile_Loop(loop,ufactor,nloops, FALSE, est_register_usage, &ni.Privatizability_Info(), outer, TRUE, NULL, NULL);
  ARRAY_DIRECTED_GRAPH16*       dg = Array_Dependence_Graph;
  Renumber_Loops(ujm.First, ujm.Last, dg);
  // remove the unity count outer loop. 

  WN *first, *last;

  Remove_Unity_Trip_Loop(loop, TRUE, &first, &last, NULL, Du_Mgr);

  return first; 
} 

void
Outer_Unroll_For_Factorization(WN *func_nd, STACK_OF_WN *loops)
{ 
   //identify loops where full unrolling is delayed
   STACK_OF_WN *inner_do_stack = CXX_NEW
          (STACK_OF_WN(&LNO_default_pool),&LNO_default_pool);
   for (LWN_ITER* itr = LWN_WALK_TreeIter(func_nd);
        itr;
        itr = LWN_WALK_TreeNext(itr)){
         WN* wn = itr->wn;
         if (WN_operator(wn) != OPR_DO_LOOP) continue; 

         DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
	 if (dli->Delay_Full_Unroll != TRUE) continue;

         // Get the outermost loop. 
         WN *outermost = NULL;
         WN *outer = wn;
         while (outer = LWN_Get_Parent(outer))
         { 
           OPCODE opc = WN_opcode(outer);
           if (OPC_DO_LOOP == opc)
           {
             outermost = outer;
             continue;
	   }
           if (OPC_BLOCK == opc)
             continue;
           break;
	 }
         if (outermost == NULL) continue;
         INT64 trip_count = Num_Iters(outermost);
         if (trip_count >= 1 && trip_count <= LNO_Full_Unrolling_Limit) 
         { 
           WN *unrolled_loop =  Unroll_and_Jam(outermost, trip_count);
           if (unrolled_loop)
             loops->Push(unrolled_loop);
         }
   }
}






