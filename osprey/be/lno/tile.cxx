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


// -*-C++-*-

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <alloca.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "snl.h"
#include "lnopt_main.h" 
#include "config_targ.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "cxx_graph.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "wintrinsic.h"
#include "scalar_expand.h"

#include "strtab.h" 
#include "dvector.h" 
#include "small_trips.h"

#include "lego_gen.h"
#include "lego_opts.h"
#include "lego_util.h"
#include "lego_pragma.h"

#include "ir_reader.h"
#include "permute.h"
#include "snl_utils.h"
#include "move.h"
#include "debug.h"
#include "tlog.h"
#include "wn_pragmas.h"
#include "wintrinsic.h"
#include "soe.h"
#include "cond.h"
#include "parmodel.h"
#include "doacross.h"
#include "ff_utils.h"
#include "parallel.h"
#include "split_tiles.h"
#include "wn_mp.h"
#include "fb_whirl.h"

#define LEGO_BOGUS_VALUE 100
#define LEGO_NAME_LENGTH 256
#define TLOG_STRING_LENGTH 1000

enum LMT_VALUE {LMT_LEGO, LMT_MP, LMT_LEGO_MP}; 

static void Lego_Tile_Traverse(WN* wn_tree, BOOL LNO_Ozero);
static void Mp_Tile_Traverse(WN* wn_tree);
static void Lego_Mp_Tile_Traverse(WN* wn_tree, BOOL LNO_Ozero);

//-----------------------------------------------------------------------
// NAME: Lego_Build_DU_For_Ldids 
// FUNCTION: Add DU information for the tree of nodes rooted at 'wn_ldid'.
//   Make LDIDs from 'wn_def' to those LDIDs which have symbol 'sym_ldid'.
//   If 'do_alias', duplicate the alias information from the 'wn_def' to 
//   each LDID for which we are making DU chains.
//-----------------------------------------------------------------------

static void Lego_Build_DU_For_Ldids(WN* wn_ldid, 
				    SYMBOL sym_ldid, 
				    WN* wn_def,
				    WN* loop, 
				    BOOL do_alias) 
{
  DU_MANAGER* du = Du_Mgr; 
  if (WN_operator(wn_ldid) == OPR_LDID
      && SYMBOL(wn_ldid) == sym_ldid) {
    du->Add_Def_Use(wn_def, wn_ldid); 
    DEF_LIST *def_list = du->Ud_Get_Def(wn_ldid); 
    def_list->Set_loop_stmt(loop); 
    if (do_alias)
      Duplicate_alias_info(Alias_Mgr, wn_def, wn_ldid); 
  }

  if (WN_opcode(wn_ldid) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_ldid); wn != NULL; wn = WN_next(wn))
      Lego_Build_DU_For_Ldids(wn, sym_ldid, wn_def, loop, do_alias);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_ldid); i++)
      Lego_Build_DU_For_Ldids(WN_kid(wn_ldid, i), sym_ldid, wn_def, loop, 
        do_alias);
  }
}

//-----------------------------------------------------------------------
// NAME: Processor_Update_Inner_Tile 
// FUNCTION: Insert the bound code in 'wn_bound' between 'outer_loop' 
//   and 'loop', and change the lower bound, upper bound, and step of 
//   the 'loop' to 'new_lb', new_ub', and 'new_step'.  These variables
//   are functions of 'pid', the index of 'outer_loop'. 
//-----------------------------------------------------------------------

static void Processor_Update_Inner_Tile(WN* wn_bound, 
  				        WN* loop, 
				        WN* outer_loop, 
				        SYMBOL* new_lb, 
				        SYMBOL* new_ub, 
			                SYMBOL* new_step,
				        SYMBOL* pid) 
{
  DU_MANAGER* du = Du_Mgr; 
  // Get a stack of definitions of lower bounds, upper bounds, and steps
  // for the inner tile loop. 
  DOLOOP_STACK lb_stack(&LNO_local_pool); 
  Lego_Find_Nodes(OPR_STID, new_lb, wn_bound, &lb_stack);  
  DOLOOP_STACK ub_stack(&LNO_local_pool); 
  Lego_Find_Nodes(OPR_STID, new_ub, wn_bound, &ub_stack); 
  DOLOOP_STACK step_stack(&LNO_local_pool);
  if (new_step != NULL) 
    Lego_Find_Nodes(OPR_STID, new_step, wn_bound, &step_stack);
  FmtAssert(lb_stack.Elements() > 0 && ub_stack.Elements() > 0,  
    ("Could not find STIDs to lower and/or upper bounds."));  

  // Move statements defining lower bounds, upper bounds, and steps for 
  // the inner tile loop into the program tree.  
  WN* wnn = NULL;
  for (WN* wn = WN_first(wn_bound); wn != NULL; wn = wnn) {
    wnn = WN_next(wn);
    LWN_Extract_From_Block(wn); 
    LWN_Insert_Block_Before(LWN_Get_Parent(loop), loop, wn); 
    Lego_Build_DU_For_Ldids(wn, *pid, WN_start(outer_loop), outer_loop, 
      TRUE);  
    Lego_Build_DU_For_Ldids(wn, *pid, WN_step(outer_loop), outer_loop, 
      FALSE);  
  }
  LWN_Delete_Tree(wn_bound);

  // Update the lower bound, upper bound, and step information for 
  // the inner tile loop, as well as the DU information. 
  WN* wn_stid_lb = lb_stack.Bottom_nth(0); 
  SYMBOL sym_stid_lb(wn_stid_lb);
  WN* wn_stid_ub = ub_stack.Bottom_nth(0); 
  SYMBOL sym_stid_ub(wn_stid_ub);
  TYPE_ID wtype_lb = sym_stid_lb.Type;
  TYPE_ID wtype_ub = sym_stid_ub.Type; 
  OPCODE opld_lb = OPCODE_make_op(OPR_LDID, wtype_lb, wtype_lb);
  OPCODE opld_ub = OPCODE_make_op(OPR_LDID, wtype_ub, wtype_ub);
  WN* lb_ldid = LWN_CreateLdid(opld_lb, wn_stid_lb);  
  INT i;
  for (i = 0; i < lb_stack.Elements(); i++) 
    du->Add_Def_Use(lb_stack.Bottom_nth(i), lb_ldid); 
  Replace_Wnexp_With_Exp_Copy(WN_kid0(WN_start(loop)), lb_ldid, du);   
  LWN_Delete_Tree(lb_ldid); 
  WN* ub_ldid = LWN_CreateLdid(opld_ub, wn_stid_ub);  
  for (i = 0; i < ub_stack.Elements(); i++) 
    du->Add_Def_Use(ub_stack.Bottom_nth(i), ub_ldid); 
  Replace_Wnexp_With_Exp_Copy(UBexp(WN_end(loop)), ub_ldid, du);   
  LWN_Delete_Tree(ub_ldid); 
  if (new_step != NULL) {
    WN* wn_stid_st = step_stack.Bottom_nth(0);
    SYMBOL index = SYMBOL(WN_start(loop));
    WN* step_add = WN_kid0(WN_step(loop));
    WN* step_exp = WN_operator(WN_kid0(step_add)) == OPR_LDID
      && SYMBOL(WN_kid0(step_add)) == index
      ? WN_kid1(step_add) : WN_kid0(step_add);
    if (WN_operator(WN_kid0(wn_stid_st)) == OPR_INTCONST) {
      Replace_Wnexp_With_Exp_Copy(step_exp, WN_kid0(wn_stid_st), du);
      LWN_Delete_Tree(wn_stid_st);
    } else {  
      SYMBOL sym_stid_st(wn_stid_st);
      TYPE_ID wtype_st = sym_stid_st.Type; 
      OPCODE opld_st = OPCODE_make_op(OPR_LDID, wtype_st, wtype_st);
      WN* step_ldid = LWN_CreateLdid(opld_st, wn_stid_st);  
      for (i = 0; i < step_stack.Elements(); i++) 
	du->Add_Def_Use(step_stack.Bottom_nth(i), step_ldid); 
      Replace_Wnexp_With_Exp_Copy(step_exp, step_ldid, du);   
      LWN_Delete_Tree(step_ldid); 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Is_Versioned_Mp_Region
// FUNCTION: Return TRUE if 'wn_loop' is a versioned MP region, FALSE if it
//   is not.
//-----------------------------------------------------------------------

static BOOL Is_Versioned_Mp_Region(WN* wn_region)
{
  if (LWN_Get_Parent(wn_region) != NULL
      && LWN_Get_Parent(LWN_Get_Parent(wn_region)) != NULL
      && WN_opcode(LWN_Get_Parent(LWN_Get_Parent(wn_region))) == OPC_IF
      && WN_Is_If_MpVersion(LWN_Get_Parent(LWN_Get_Parent(wn_region))))
    return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Is_Versioned_Mp_Loop
// FUNCTION: Return TRUE if 'wn_loop' is a versioned MP loop, FALSE if it
//   is not.
//-----------------------------------------------------------------------

extern BOOL Is_Versioned_Mp_Loop(WN* wn_loop)
{
  for (WN* wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_operator(wn) == OPR_REGION
        && Is_Versioned_Mp_Region(wn))
      return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Processor_Update_Outer_Tile 
// FUNCTION: Update the lower bound, upper bound, and step information
//   for the outermost tile loop 'outer_loop' which was cloned from the 
//   loop 'loop'. 
//-----------------------------------------------------------------------

static void Processor_Update_Outer_Tile(WN* outer_loop, 
				        WN* loop, 
				        SYMBOL* pid, 
					BOOL lego_tile, 
					BOOL negative_stride) 
{  
  // If original loop was MP, transfer it's MP flag to the outer tile. 
  DU_MANAGER* du = Du_Mgr;   
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(outer_loop);
  if (dli->Mp_Info != NULL) {
    dli_outer->Mp_Info = dli->Mp_Info; 
    dli->Mp_Info = NULL; 
  }

  // Update the lower bound, upper bound, and step information for 
  // the outer tile loop, as well as the DU information. 
  TYPE_ID wtype = Do_Wtype(loop);
  WN* wn_zero = LWN_Make_Icon(Promote_Type(wtype), 0); 
  if (negative_stride)
    Replace_Wnexp_With_Exp_Copy(UBexp(WN_end(outer_loop)), wn_zero, du); 
  else 
    Replace_Wnexp_With_Exp_Copy(WN_kid0(WN_start(outer_loop)), wn_zero, du); 
  LWN_Delete_Tree(wn_zero);
  SYMBOL outer_index = SYMBOL(WN_start(outer_loop));
  WN* outer_step_add = WN_kid0(WN_step(outer_loop));
  WN* outer_step_exp = WN_operator(WN_kid0(outer_step_add)) 
    == OPR_LDID && SYMBOL(WN_kid0(outer_step_add)) == outer_index
    ? WN_kid1(outer_step_add) : WN_kid0(outer_step_add);
  WN* wn_stride = LWN_Make_Icon(Promote_Type(wtype), negative_stride ? -1 : 1);
  Replace_Wnexp_With_Exp_Copy(outer_step_exp, wn_stride, du); 
  LWN_Delete_Tree(wn_stride);
  DOLOOP_STACK pid_stack(&LNO_local_pool);
  Lego_Find_Nodes(OPR_LDID, pid, WN_do_body(outer_loop), &pid_stack);  
  INT i;
  for (i = 0; i < pid_stack.Elements(); i++) {
    WN* wn_use = pid_stack.Bottom_nth(i);
    du->Add_Def_Use(WN_start(outer_loop), wn_use); 
    du->Add_Def_Use(WN_step(outer_loop), wn_use); 
  }
  for (i = 0; i < pid_stack.Elements(); i++) {
    WN* wn_use =  pid_stack.Bottom_nth(i);
    DEF_LIST *def_list = du->Ud_Get_Def(wn_use); 
    def_list->Set_loop_stmt(outer_loop); 
  }
  WN* num_threads = NULL; 
  if (lego_tile)
    if (dli->Lego_Info->Dynamic_Affinity()) {
      WN* wn_numthreads_code = NULL;
      WN* wn = 0;
      for (wn = outer_loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
        if (WN_opcode(wn) == OPC_DO_LOOP) {
	  DO_LOOP_INFO* dli_wn = Get_Do_Loop_Info(wn);
	  if (dli_wn->Mp_Info != NULL && dli_wn->Mp_Info->Nest_Index() == 0)
	    break;
        }
      }
      FmtAssert(wn != NULL, ("Could not find outermost doacross loop"));
      WN* wn_outer_doacross = wn;  
      num_threads = Numprocs(dli->Lego_Info->Array()->St(), 
                             dli->Lego_Info->Dim_Num(),
                             Do_Depth(wn_outer_doacross) > 0, 
                             &wn_numthreads_code); 
      WN* wnn = NULL;
      for (wn = WN_first(wn_numthreads_code); wn != NULL; wn = wnn) {
	wnn = WN_next(wn);
	LWN_Extract_From_Block(wn);
	LWN_Insert_Block_Before(LWN_Get_Parent(wn_outer_doacross), 
          wn_outer_doacross, wn);
        LWN_Copy_Linenumber(wn_outer_doacross,wn);

        // Numprocs may create a conditional, in which case we need to
        // build if-info
        //
        if (WN_opcode(wn) == OPC_IF) {
          IF_INFO *ii=CXX_NEW (IF_INFO(&LNO_default_pool,
	    Find_SCF_Inside(wn, OPC_DO_LOOP) != NULL,
            Find_SCF_Inside(wn, OPC_REGION) != NULL), &LNO_default_pool);
          WN_MAP_Set(LNO_Info_Map,wn,(void *)ii);
          DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                        &LNO_local_pool);
          Build_Doloop_Stack(wn, stack);
          LNO_Build_If_Access(wn, stack);
          CXX_DELETE(stack, &LNO_local_pool);
        }
      }
      LWN_Delete_Tree(wn_numthreads_code);
    } else {
      num_threads = Get_Numthreads_Ldid(dli->Lego_Info);
    }
  else if (dli_outer->Mp_Info->Is_Pdo()) { 
    if (Is_Versioned_Mp_Loop(outer_loop))
      num_threads = Get_Runtime_Cur_Numthreads_Ldid(); 
    else 
      num_threads = Get_Runtime_Cur_Numthreads_Func(outer_loop); 
  } else  
    num_threads = Get_Frozen_Numthreads_Ldid(outer_loop); 
  TYPE_ID desc = WN_rtype(num_threads); 
  OPCODE subop = OPCODE_make_op(OPR_SUB, desc, MTYPE_V);
  WN* wn_one = LWN_Make_Icon(Promote_Type(wtype), 1);
  WN* wn_ub = LWN_CreateExp2(subop, num_threads, wn_one);
  WN* wn1;
  WN* wn_parent;
  if (negative_stride)
    wn1 = WN_kid0(WN_start(outer_loop));
  else 
    wn1 = UBexp(WN_end(outer_loop));
  wn_parent = LWN_Get_Parent(wn1);
  if (WN_desc(wn_parent) != WN_rtype(wn_ub)) 
    wn_ub = LWN_Integer_Cast(wn_ub, WN_desc(wn_parent), WN_rtype(wn_ub));
  for (i=0; i<WN_kid_count(wn_parent); i++)
    if (WN_kid(wn_parent,i)==wn1) {
      WN_kid(wn_parent, i) =  wn_ub;
      LWN_Set_Parent(wn_ub, wn_parent);
      LWN_Delete_Tree(wn1);
      break;
    }
  if (negative_stride) {
    OPCODE op = WN_opcode(WN_end(outer_loop));
    OPCODE op_inv = OPCODE_make_op(OPR_GE, OPCODE_rtype(op), OPCODE_desc(op));
    WN_set_opcode(WN_end(outer_loop), op_inv);
  }
}

//-----------------------------------------------------------------------
// NAME: Has_Calls 
// FUNCTION: Returns TRUE if the code rooted at 'wn_tree' contains calls,
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Has_Calls(WN* wn_tree)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_tree);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (OPCODE_is_call(WN_opcode(wn)))
      return TRUE;
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Has_Live_Out_Index_Variable
// FUNCTION: Return TRUE if 'wn_loop's index variable is used outside the
//   loop (or if we are not sure).  Return FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Has_Live_Out_Index_Variable(WN* wn_loop)
{
  USE_LIST *use_list = Du_Mgr->Du_Get_Use(WN_start(wn_loop)); 
  if (use_list == NULL || use_list->Incomplete())
    return TRUE; 
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_use = node->Wn();
    if (!Wn_Is_Inside(wn_use, wn_loop))
      return TRUE; 
  } 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Index_Variable_Is_Last_Local
// FUNCTION: Return TRUE if the index variable of the loop 'wn_loop' is 
//   LASTLOCAL in the immediately enclosing MP region, return FALSE 
//   otherwise.    
// NOTE: The case where the index variable is marked as LASTLOCAL, but 
//   is not used outside the loop is considered as NOT being last local.
//-----------------------------------------------------------------------

static BOOL Index_Variable_Is_Last_Local(WN* wn_loop)
{
  if (!Do_Loop_Is_Mp(wn_loop))
    return FALSE; 
  WN* wn_start = WN_start(wn_loop);
  if (!Has_Live_Out_Index_Variable(wn_loop))
    return FALSE; 
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  WN* wn_first = WN_first(WN_region_pragmas(wn_region));
  SYMBOL sym_index(WN_index(wn_loop));
  for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) { 
    if ((WN_opcode(wn) == OPC_PRAGMA || WN_opcode(wn) == OPC_XPRAGMA)
        && WN_pragma(wn) == WN_PRAGMA_LASTLOCAL   
	&& WN_st(wn) == sym_index.St() 
	&& WN_pragma_arg1(wn) == sym_index.WN_Offset())
      return TRUE; 
  } 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Initialize_Doacross_Last_Local_Index
// FUNCTION: Put a copy of the WN_start() initialization of 'wn_loop' 
//   immediately before 'wn_loop'. 
//-----------------------------------------------------------------------

static WN* Initialize_Doacross_Last_Local_Index(WN* wn_loop)
{ 
  WN* wn_start = WN_start(wn_loop);
  WN* wn_copy = LWN_Copy_Tree(wn_start, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(WN_kid0(wn_start), WN_kid0(wn_copy), Du_Mgr);
  Copy_alias_info(Alias_Mgr, wn_start, wn_copy);
  USE_LIST *ul_start = Du_Mgr->Du_Get_Use(wn_start);
  if (ul_start != NULL) { 
    if (ul_start->Incomplete())
      Du_Mgr->Du_Set_Incomplete(wn_copy);
    USE_LIST_ITER iter(ul_start);
    const DU_NODE* node = NULL;
    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) { 
      WN* wn_use = node->Wn();
      if (!Wn_Is_Inside(wn_use, wn_loop))
        Du_Mgr->Add_Def_Use(wn_copy, wn_use);
    } 
  } 
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, wn_copy);
  return wn_copy;
} 

//-----------------------------------------------------------------------
// NAME: Initialize_Pdo_Last_Local_Index
// FUNCTION: Initialize the last local value of 'wn_loop' by putting: 
//   IF (trip_count .LT. 0) THEN 
//     call mp_my_threadnum()
//     IF (my_thread_num .EQ. 0) THEN 
//       i = lower_bound
//     ENDIF 
//   ENDIF    
//   before 'wn_loop'.
//-----------------------------------------------------------------------

static WN* Initialize_Pdo_Last_Local_Index(WN* wn_loop)
{ 
  // Make an IF test with "trip_count < 0" 
  WN* wn_trip_count = Trip_Count(wn_loop);
  TYPE_ID index_type = Promote_Type(Do_Wtype((WN *) wn_loop));
  WN* wn_zero = LWN_Make_Icon(index_type, 0);
  OPCODE op_lt = OPCODE_make_op(OPR_LT, Boolean_type, index_type);
  WN* wn_trip_test = LWN_CreateExp2(op_lt, wn_trip_count, wn_zero);
  WN* wn_if = LWN_CreateIf(wn_trip_test, WN_CreateBlock(), WN_CreateBlock()); 
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_region), wn_region, wn_if);
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_loop));
  IF_INFO* ii_trip =
    CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii_trip);
  DOLOOP_STACK stk(&LNO_local_pool);
  Build_Doloop_Stack(wn_if, &stk);
  LNO_Build_If_Access(wn_if, &stk);

  // Make an IF test with "mp_my_threadnum == 0 
  PREG_NUM rreg1, rreg2;
  OPCODE op_call = OPCODE_make_op(OPR_CALL, MTYPE_I4, MTYPE_V);
  WN* wn_call = WN_Create(op_call, 0);
  WN_Set_Linenum(wn_call, WN_Get_Linenum(wn_loop));
  WN_st_idx(wn_call) = ST_st_idx(distr_st_entries[mp_my_threadnum]);
  Set_Runtime_Call_Side_Effects(wn_call);
  LWN_Insert_Block_Before(WN_then(wn_if), NULL, wn_call);
  ST* rst = Find_Return_Registers (Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, 
    ("Initialize_Pdo_Last_Local_Index: Bad MTYPE_I4 return regs"));
  SYMBOL preg = Create_Preg_Symbol ("$myid", MTYPE_I4);
  WN* wn_myid = WN_CreateLdid (OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I4),
    rreg1, rst, Be_Type_Tbl(MTYPE_I4));
  Create_alias(Alias_Mgr, wn_myid);
  Du_Mgr->Add_Def_Use(wn_call, wn_myid);
  OPCODE op_eq = OPCODE_make_op(OPR_EQ, Boolean_type, MTYPE_I4);
  WN* wn_zero_copy = LWN_Make_Icon(MTYPE_I4, 0);
  WN* wn_thread_test = LWN_CreateExp2(op_eq, wn_myid, wn_zero_copy);
  WN* wn_thread_if = LWN_CreateIf(wn_thread_test, WN_CreateBlock(), 
    WN_CreateBlock());
  LWN_Insert_Block_After(LWN_Get_Parent(wn_call), wn_call, wn_thread_if);
  WN_Set_Linenum(wn_thread_if, WN_Get_Linenum(wn_loop));
  IF_INFO* ii_thread =
    CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_thread_if, (void *) ii_thread);
  DOLOOP_STACK stk_thread(&LNO_local_pool);
  Build_Doloop_Stack(wn_thread_if, &stk_thread);
  LNO_Build_If_Access(wn_thread_if, &stk_thread);

  // Assign LB of loop its value  
  WN* wn_init = Initialize_Doacross_Last_Local_Index(wn_loop);
  LWN_Extract_From_Block(wn_init);
  LWN_Insert_Block_Before(WN_then(wn_thread_if), NULL, wn_init);
  return wn_if; 
} 

//-----------------------------------------------------------------------
// NAME: Initialize_Last_Local_Index
// FUNCTION: 
//-----------------------------------------------------------------------

static void Initialize_Last_Local_Index(WN* wn_loop)
{ 
  DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
  FmtAssert(dli_loop->Mp_Info != NULL, 
    ("Initialize_Last_Local_Index: Expecting an MP loop"));
  if (dli_loop->Mp_Info->Is_Pdo())
    Initialize_Pdo_Last_Local_Index(wn_loop);
  else 
    Initialize_Doacross_Last_Local_Index(wn_loop);
} 

//-----------------------------------------------------------------------
// NAME: Processor_2D_Tile_Loop 
// FUNCTION:  Processor tile (pseudo-lower) the 'loop' according to the 
//   information in the LEGO_INFO.  Memory for data structures created 
//   during the tiling is allocated from 'pool'.  A 2-level loop is 
//   created of the form: 
//	do lego_tile = 0, P-1 
//        lb = lower_bound_expression(lego_tile) 
//        ub = upper_bound_expression(lego_tile) 
//        do i = lb, ub 
//	    <loop body> 
//        end do 
//	end do 
//-----------------------------------------------------------------------

static WN* Processor_2D_Tile_Loop(WN* loop, 
			          MEM_POOL *pool, 
				  BOOL lego_tile)
{
  // Find the root of the program tree.
  WN* wn_root = NULL; 
  DU_MANAGER* du = Du_Mgr; 
  for (WN* wn = loop; wn != NULL; wn_root = wn, wn = LWN_Get_Parent(wn));
  FmtAssert(wn_root != NULL, ("Could not find program tree root."));  

  if (Index_Variable_Is_Last_Local(loop))
    Initialize_Last_Local_Index(loop);

  // Get the next unique tiling identifier 
  INT tiling_key = Get_New_Lego_Mp_Tile_Key(); 

  // Generate new inner loop bounds. 
  SYMBOL* new_lb = NULL; 
  SYMBOL* new_ub = NULL; 
  SYMBOL* new_step = NULL; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop); 
  SYMBOL oldsym(WN_index(loop)); 
  INT required_length = strlen(oldsym.Name()) + 10; 
  char* Str_Buf = CXX_NEW_ARRAY(char, required_length, &LNO_local_pool); 
  if (lego_tile) 
    sprintf(Str_Buf, "$dsmtile0%s", oldsym.Name()); 
  else 
    sprintf(Str_Buf, "$datile0%s", oldsym.Name());
  SYMBOL* pid = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, Do_Wtype(loop))), 
    &LNO_default_pool);
  if (lego_tile)
    dli->Lego_Info->Set_Pid0(pid);
  else 
    dli->Mp_Info->Set_Pid0(pid); 
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(loop, &stack);
  if (Bound_Is_Too_Messy(dli->LB))
    Hoist_Lower_Bound(loop, &stack, &LNO_default_pool); 
  if (Bound_Is_Too_Messy(dli->UB))
    Hoist_Upper_Bound(loop, &stack, &LNO_default_pool); 
  WN* wn_bound = Generate_Bounds(loop, &new_lb, &new_ub, &new_step); 
  FmtAssert(new_lb != NULL && new_ub != NULL, 
    ("Did not generate new lower and/or upper bound"));  

  // Create basic form for pair of tiled loops. 
  WN* outer_loop = NULL; 
  SNL_INV_CACHE_BLOCK_REASON reason = lego_tile ? SNL_INV_LEGO_TILE :
    SNL_INV_MP_TILE;
  INT64 old_est_iters = dli->Est_Num_Iterations;  
  outer_loop = Tile_Loop(loop, LEGO_BOGUS_VALUE, 0, reason, pid, pool); 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(outer_loop);
  dli_outer->Est_Num_Iterations = NOMINAL_PROCS; 
  dli->Est_Num_Iterations = old_est_iters;  
  dli->Tile_Size = 0; 
  if (Has_Calls(wn_bound)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(outer_loop); 
    dli->Has_Calls = TRUE; 
  }

  // Update the lower bound, upper bound, and step information for 
  // the inner tile loop, as well as the DU information. 
  Processor_Update_Inner_Tile(wn_bound, loop, outer_loop, new_lb, new_ub,
    new_step, pid); 

  // Update the lower bound, upper bound, and step information for 
  // the outer tile loop, as well as the DU information. 
  Processor_Update_Outer_Tile(outer_loop, loop, pid, lego_tile, 
    dli->Lego_Info != NULL && dli->Lego_Info->Stride() < 0); 

  // Update Is_Outer_Lego_Tile bit. 
  dli_outer->Is_Processor_Tile = TRUE; 
  if (lego_tile) {
    dli_outer->Is_Outer_Lego_Tile = TRUE;
    dli->Is_Inner_Lego_Tile = TRUE; 
  }
  dli_outer->Lego_Mp_Key_Lower = tiling_key; 
  dli_outer->Lego_Mp_Key_Upper = tiling_key; 
  dli_outer->Lego_Mp_Key_Depth = 0; 
  dli_outer->Suggested_Parallel = dli->Suggested_Parallel; 
  dli->Lego_Mp_Key_Lower = tiling_key; 
  dli->Lego_Mp_Key_Upper = tiling_key; 
  dli->Lego_Mp_Key_Depth = 1; 
  dli->Suggested_Parallel = FALSE; 

  // Inhibit optimizations on outer tile loop 
  dli_outer->No_Fission = TRUE;
  dli_outer->No_Fusion = TRUE;
  dli_outer->Cannot_Interchange = TRUE;
  dli_outer->Cannot_Block = TRUE;
  dli_outer->Required_Unroll = 1;
  dli_outer->Pragma_Cannot_Concurrentize = dli->Pragma_Cannot_Concurrentize; 
  dli_outer->Inside_Critical_Section = dli->Inside_Critical_Section;
  dli_outer->Has_Threadprivate = dli->Has_Threadprivate; 
  dli_outer->Serial_Version_of_Concurrent_Loop 
    = dli->Serial_Version_of_Concurrent_Loop;

  // Fix the access vectors of the tiled loops. 
  DOLOOP_STACK dostack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(outer_loop), &dostack);
  LNO_Build_Access(outer_loop, &dostack, &LNO_default_pool);
  BOOL negative_stride = dli->Lego_Info && dli->Lego_Info->Stride() < 0;
  Hoist_Iload_Ldid_Upper_Bound_One_Level(outer_loop,negative_stride);

  if (Cur_PU_Feedback) {
    LNO_FB_MP_Tile(outer_loop, NOMINAL_PROCS, loop);
  }

  // Print informative messages. 
  if (LNO_Verbose) {
    fprintf(stdout, "2D Tile (%s) -> (%s,%s)\n", WB_Whirl_Symbol(loop), 
      WB_Whirl_Symbol(outer_loop), WB_Whirl_Symbol(loop)); 
    fprintf(TFile, "2D Tile (%s) -> (%s,%s)\n", WB_Whirl_Symbol(loop), 
      WB_Whirl_Symbol(outer_loop), WB_Whirl_Symbol(loop)); 
  }
  if (LNO_Tlog) {
    INT required_length = strlen(WB_Whirl_Symbol(loop)) + 3; 
    char* tlog_instring = CXX_NEW_ARRAY(char, required_length, 
      &LNO_local_pool); 
    required_length = strlen(WB_Whirl_Symbol(outer_loop)) 
      + strlen(WB_Whirl_Symbol(loop)) + 4; 
    char* tlog_outstring = CXX_NEW_ARRAY(char, required_length, 
      &LNO_local_pool); 
    sprintf(tlog_instring, "(%s)", (char *) WB_Whirl_Symbol(loop));
    sprintf(tlog_outstring, "(%s,%s)", (char *) WB_Whirl_Symbol(outer_loop), 
      (char *) WB_Whirl_Symbol(loop));
    Generate_Tlog("LNO", "lego_mp_tile", Srcpos_To_Line(WN_linenum(loop)),
      (char *) WB_Whirl_Symbol(loop), tlog_instring, tlog_outstring, "");  
  }
   
  return outer_loop; 
} // Processor_2D_Tile_Loop()

//-----------------------------------------------------------------------
// NAME: Processor_3D_Tile_Loop 
// FUNCTION:  Processor tile (pseudo-lower) the 'loop' according to the 
//   information in the LEGO_INFO.  Memory for data structures created 
//   during the tiling is allocated from 'pool'.  A 3-level loop is 
//   created of the form: 
//	do lego_tile0 = 0, P-1 
//        lb0 = lower_bound_expression0(lego_tile0) 
//        ub0 = upper_bound_expression0(lego_tile0) 
//        do lego_tile1 = lb0, ub0
//	    lb1 = lower_bound_expression1(lego_tile1)   
//	    ub1 = upper_bound_expression1(lego_tile1) 
//	    do lego_tile2 = lb1, ub1   
//	      <loop body>
//	    end do  
//        end do 
//	end do 
//-----------------------------------------------------------------------

static WN* Processor_3D_Tile_Loop(WN* loop,
                                  MEM_POOL *pool, 
				  BOOL lego_tile)
{
  // Find the root of the program tree.
  WN* wn_root = NULL;
  for (WN* wn = loop; wn != NULL; wn_root = wn, wn = LWN_Get_Parent(wn));
  FmtAssert(wn_root != NULL, ("Could not find program tree root."));

  // Get the next unique tiling identifier 
  INT tiling_key = Get_New_Lego_Mp_Tile_Key(); 

  // Generate new loop bounds
  SYMBOL* new_out_lb = NULL;
  SYMBOL* new_out_ub = NULL;
  SYMBOL* new_out_step = NULL;
  SYMBOL* new_in_lb = NULL;
  SYMBOL* new_in_ub = NULL;
  SYMBOL* new_in_step = NULL;
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
  SYMBOL oldsym(WN_index(loop));
  SYMBOL* pid_in = NULL; 
  SYMBOL* pid_out = NULL;
  INT required_length = strlen(oldsym.Name()) + strlen("$dsmtile0") + 1;
  char* Str_Buf = CXX_NEW_ARRAY(char, required_length, &LNO_local_pool); 
  if (lego_tile) {
    sprintf(Str_Buf, "$dsmtile0%s", oldsym.Name());
    pid_out = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, Do_Wtype(loop))), 
      &LNO_default_pool);
    dli->Lego_Info->Set_Pid0(pid_out);
    sprintf(Str_Buf, "$dsmtile1%s", oldsym.Name());
    pid_in = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, Do_Wtype(loop))), 
      &LNO_default_pool);
    dli->Lego_Info->Set_Pid1(pid_in);
  } else {
    sprintf(Str_Buf, "$da_tile0%s", oldsym.Name());
    pid_out = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, Do_Wtype(loop))), 
      &LNO_default_pool);
    dli->Mp_Info->Set_Pid0(pid_out);
    sprintf(Str_Buf, "$da_tile1%s", oldsym.Name());
    pid_in = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, Do_Wtype(loop))), 
      &LNO_default_pool);
    dli->Mp_Info->Set_Pid1(pid_in);
  }
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(loop, &stack);
  if (Bound_Is_Too_Messy(dli->LB))
    Hoist_Lower_Bound(loop, &stack, &LNO_default_pool);  
  if (Bound_Is_Too_Messy(dli->UB))
    Hoist_Upper_Bound(loop, &stack, &LNO_default_pool);
  WN* wn_out_bound = Generate_Bounds(loop, &new_out_lb, &new_out_ub, 
    &new_out_step, 0);
  FmtAssert(new_out_lb != NULL && new_out_ub != NULL,
    ("Did not generate new outer lower and/or upper bound"));
  WN* wn_in_bound = Generate_Bounds(loop, &new_in_lb, &new_in_ub, 
    &new_in_step, 1);
  FmtAssert(new_in_lb != NULL && new_in_ub != NULL,
    ("Did not generate new inner lower and/or upper bound"));

  // Tile the loop twice. 
  WN* outer_loop = NULL; 
  SNL_INV_CACHE_BLOCK_REASON reason = lego_tile ? SNL_INV_LEGO_TILE :
    SNL_INV_MP_TILE; 
  INT64 old_est_iters = dli->Est_Num_Iterations; 
  outer_loop = Tile_Loop(loop, LEGO_BOGUS_VALUE, 0, reason, pid_out, pool);
  WN* inner_loop = NULL; 
  inner_loop = Tile_Loop(loop, LEGO_BOGUS_VALUE/2, 0, reason, pid_in, 
    pool);
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(outer_loop); 
  dli_outer->Est_Num_Iterations = NOMINAL_PROCS;
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(inner_loop); 
  dli_inner->Est_Num_Iterations = NOMINAL_PROCS;
  dli->Est_Num_Iterations = old_est_iters;  
  dli->Tile_Size = 0; 

  // Update the lower bound, upper bound, and step information for 
  // the two inner tile loop, as well as the DU information. 
  Processor_Update_Inner_Tile(wn_out_bound, inner_loop, outer_loop, 
    new_out_lb, new_out_ub, new_out_step, pid_out); 
  Processor_Update_Inner_Tile(wn_in_bound, loop, inner_loop, 
    new_in_lb, new_in_ub, new_in_step, pid_in); 

  // Update the lower bound, upper bound, and step information for 
  // the inner tile loop, as well as the DU information. 
  Processor_Update_Outer_Tile(outer_loop, loop, pid_out, lego_tile,
    dli->Lego_Info != NULL && dli->Lego_Info->Stride() < 0); 

  // Update the Is_Outer_Lego_Tile bits. 
  dli_outer->Is_Processor_Tile = TRUE;
  if (lego_tile) {
    dli_outer->Is_Outer_Lego_Tile = TRUE;
    dli_inner->Is_Outer_Lego_Tile = TRUE;
    dli->Is_Inner_Lego_Tile = TRUE; 
  }
  dli_outer->Lego_Mp_Key_Depth = 0; 
  dli_outer->Lego_Mp_Key_Lower = tiling_key;  
  dli_outer->Lego_Mp_Key_Upper = tiling_key;  
  dli_outer->Suggested_Parallel = dli->Suggested_Parallel; 
  dli_inner->Lego_Mp_Key_Depth = 1; 
  dli_inner->Lego_Mp_Key_Lower = tiling_key;  
  dli_inner->Lego_Mp_Key_Upper = tiling_key;  
  dli->Lego_Mp_Key_Depth = 2; 
  dli->Lego_Mp_Key_Lower = tiling_key;  
  dli->Lego_Mp_Key_Upper = tiling_key;   
  dli->Suggested_Parallel = FALSE; 

  dli_outer->No_Fission = TRUE;
  dli_outer->No_Fusion = TRUE;
  dli_outer->Cannot_Interchange = TRUE;
  dli_outer->Cannot_Block = TRUE;
  dli_outer->Required_Unroll = 1;
  dli_outer->Pragma_Cannot_Concurrentize = dli->Pragma_Cannot_Concurrentize; 
  dli_outer->Inside_Critical_Section = dli->Inside_Critical_Section;
  dli_outer->Has_Threadprivate = dli->Has_Threadprivate;
  dli_outer->Serial_Version_of_Concurrent_Loop  
    = dli->Serial_Version_of_Concurrent_Loop;
  dli_inner->No_Fission = TRUE;
  dli_inner->No_Fusion = TRUE;
  dli_inner->Cannot_Interchange = TRUE;
  dli_inner->Cannot_Block = TRUE;
  dli_inner->Required_Unroll = 1;
  dli_inner->Pragma_Cannot_Concurrentize = dli->Pragma_Cannot_Concurrentize; 
  dli_inner->Has_Threadprivate = dli->Has_Threadprivate;
  dli_inner->Serial_Version_of_Concurrent_Loop  
    = dli->Serial_Version_of_Concurrent_Loop;

  // Fix the access vectors of the tiled loops. 
  DOLOOP_STACK dostack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(outer_loop), &dostack);
  LNO_Build_Access(outer_loop, &dostack, &LNO_default_pool);
  BOOL negative_stride = dli->Lego_Info && dli->Lego_Info->Stride() < 0;
  Hoist_Iload_Ldid_Upper_Bound_One_Level(outer_loop,negative_stride);
 
  if (Cur_PU_Feedback) {
    LNO_FB_MP_Tile(inner_loop, 1, loop);
    LNO_FB_MP_Tile(outer_loop, 1, inner_loop);
  }

  // Print informative messages
  if (LNO_Verbose) {
    fprintf(stdout, "3D Tile (%s) -> (%s,%s,%s)\n", WB_Whirl_Symbol(loop),
      WB_Whirl_Symbol(outer_loop), WB_Whirl_Symbol(inner_loop), 
      WB_Whirl_Symbol(loop));
    fprintf(TFile, "3D Tile (%s) -> (%s,%s,%s)\n", WB_Whirl_Symbol(loop),
      WB_Whirl_Symbol(outer_loop), WB_Whirl_Symbol(inner_loop), 
      WB_Whirl_Symbol(loop));
  }
  if (LNO_Tlog) {
    INT required_length = strlen(WB_Whirl_Symbol(loop)) + 3; 
    char* tlog_instring = CXX_NEW_ARRAY(char, required_length, 
      &LNO_local_pool); 
    required_length = 5 + strlen(WB_Whirl_Symbol(outer_loop)) 
      + strlen(WB_Whirl_Symbol(inner_loop)) + strlen(WB_Whirl_Symbol(loop));
    char* tlog_outstring = CXX_NEW_ARRAY(char, required_length, 
      &LNO_local_pool); 
    sprintf(tlog_instring, "(%s)", WB_Whirl_Symbol(loop));
    sprintf(tlog_outstring, "(%s,%s,%s)", WB_Whirl_Symbol(outer_loop), 
      WB_Whirl_Symbol(inner_loop), WB_Whirl_Symbol(loop));
    Generate_Tlog("LNO", "lego_mp_tile", Srcpos_To_Line(WN_linenum(loop)),
      (char *) WB_Whirl_Symbol(loop), tlog_instring, tlog_outstring, "");  
  }
 
  return outer_loop; 
}

//-----------------------------------------------------------------------
// NAME: Lego_Tile_Single_Loop
// FUNCTION:  Processor tile (pseudo-lower) the 'loop' according to the
//   information in the LEGO_INFO.  Memory for data structures created
//   during the tiling is allocated from 'pool'.
//-----------------------------------------------------------------------

extern WN* Lego_Tile_Single_Loop(WN* loop,
                                 MEM_POOL *pool)
{
  Is_True(Loop_Bounds_Simple(loop),
    ("Lego tiling cannot generate code for loop %s with complex bounds",
    ST_name(WN_st(WN_index(loop)))));
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
  LEGO_INFO* lego_info = dli->Lego_Info; 
  Is_True(lego_info, ("Generate_Bounds passed empty LEGO_INFO"));
  SYMBOL *array_sym = lego_info->Array();
  Is_True(array_sym != NULL, 
    ("Lego_Tile_Single_Loop: Could not find array sym for loop %s", 
    ST_name(WN_st(WN_index(loop)))));
  if (lego_info->Dynamic_Affinity())
    return Processor_3D_Tile_Loop(loop, pool, TRUE); 
  DISTR_ARRAY *dact = Lookup_DACT(array_sym->St());
  DISTR_INFO *dinfo = dact->Dinfo();
  Is_True(dact != NULL, 
    ("Lego_Tile_Single_Loop: No DACT for array %s in LEGO_INFO",
    ST_name(array_sym->St())));
  INT curr_dim = lego_info->Dim_Num();
  INT num_dim = dinfo->Num_Dim();
  Is_True((curr_dim >= 0) && (curr_dim < num_dim),
    ("Lego_Tile_Single_Loop: Bad dimension (%d) in LEGO_INFO, 0..%d expected",
     curr_dim, num_dim-1));
  switch (dact->Get_Dim(curr_dim)->Distr_Type()) {
    case DISTRIBUTE_BLOCK: 
      return Processor_2D_Tile_Loop(loop, pool, TRUE); 
    case DISTRIBUTE_CYCLIC_CONST: 
      if (dact->Get_Dim(curr_dim)->Chunk_Const_Val() == 1)
        return Processor_2D_Tile_Loop(loop, pool, TRUE);
      else 
	return Processor_3D_Tile_Loop(loop, pool, TRUE);
    case DISTRIBUTE_CYCLIC_EXPR: 
      return Processor_3D_Tile_Loop(loop, pool, TRUE); 
    case DISTRIBUTE_STAR: 
      return NULL; 
  }
  return NULL;  
}

//-----------------------------------------------------------------------
// NAME: Create_Array_Load 
// FUNCTION: Create a 1-dimensional array reference to the 'index-th' 
//   element of the 'st_array' which has 'element_count' elements of 
//   size 'element_size' with machine type 'mtype'.  
//-----------------------------------------------------------------------

static WN* Create_Array_Load(ST* st_array,
		             TYPE_ID mtype,  
			     INT index, 
			     INT element_size, 
			     INT element_count)
{
  TY_IDX ty = Be_Type_Tbl(mtype); 
  TY_IDX ty_ptr = Make_Pointer_Type(Be_Type_Tbl(mtype));
  TY_IDX arr_ty_ptr = Make_Pointer_Type(ST_type(st_array));
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_lda = WN_CreateLda(op_lda, 0, arr_ty_ptr, st_array);
  WN* wn_size = LWN_Make_Icon(mtype, element_count);
  WN* wn_index = LWN_Make_Icon(mtype, index);
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  WN* wn_array = WN_Create(op_array, 3);
  WN_element_size(wn_array) = element_size;
  WN_array_base(wn_array) = wn_lda;
  WN_array_index(wn_array, 0) = wn_index;
  WN_array_dim(wn_array, 0) = wn_size;
  LWN_Parentize(wn_array); 
  OPCODE op_iload = OPCODE_make_op(OPR_ILOAD, mtype, mtype);
  WN* wn_iload = LWN_CreateIload(op_iload, 0, ty, ty_ptr, wn_array);
  Create_lda_array_alias(Alias_Mgr, wn_lda, wn_iload);
  return wn_iload;
} 

//-----------------------------------------------------------------------
// NAME: Create_Array_Store 
// FUNCTION: Create a 1-dimensional store of 'wn_value' to the array 
//   reference to the 'index-th' element of the 'st_array' which has 
//   'element_count' elements of size 'element_size' with machine type 
//   'mtype'.  
//-----------------------------------------------------------------------

extern WN* Create_Array_Store(ST* st_array,
                             TYPE_ID mtype,   
                             INT index,   
                             INT element_size,   
                             INT element_count, 
			     WN* wn_value)
{
  TY_IDX ty = Be_Type_Tbl(mtype); 
  TY_IDX ty_ptr = Make_Pointer_Type(Be_Type_Tbl(mtype));
  TY_IDX arr_ty_ptr = Make_Pointer_Type(ST_type(st_array));
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_lda = WN_CreateLda(op_lda, 0, arr_ty_ptr, st_array);
  WN* wn_size = LWN_Make_Icon(mtype, element_count);
  WN* wn_index = LWN_Make_Icon(mtype, index);
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  WN* wn_array = WN_Create(op_array, 3);
  WN_element_size(wn_array) = element_size;
  WN_array_base(wn_array) = wn_lda;
  WN_array_index(wn_array, 0) = wn_index;
  WN_array_dim(wn_array, 0) = wn_size;
  LWN_Parentize(wn_array); 
  OPCODE op_istore = OPCODE_make_op(OPR_ISTORE, MTYPE_V, mtype);
  WN* wn_istore = LWN_CreateIstore(op_istore, 0, ty_ptr, wn_value, wn_array);
  Create_lda_array_alias(Alias_Mgr, wn_lda, wn_istore);
  return wn_istore;
}

//-----------------------------------------------------------------------
// NAME: Mp_Layout_Load_Pids 
// FUNCTION: Load pids into the MP_INFOs of each of loops in the nested 
//   doacross with outer loop 'wn_loop' and depth 'tile_count'.  These 
//   pids will be used to hold the number of processors assigned to each
//   loop in the nested doacross. 
//-----------------------------------------------------------------------

static void Mp_Layout_Load_Pids(WN* wn_loop, 
				INT tile_count)
{
  char Str_Buf[256];
  for (INT i = 0; i < tile_count; i++) {
    WN* wn_load_loop = SNL_Get_Inner_Snl_Loop(wn_loop, i + 1); 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_load_loop); 
    TYPE_ID type = Do_Wtype((WN*) wn_load_loop); 
    sprintf(Str_Buf, "$layout_pid%d", WN_map_id(wn_load_loop));
    SYMBOL* sym_pid = 
      CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), &LNO_default_pool);
    dli->Mp_Info->Set_Nest_Layout(sym_pid); 
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Layout_Copy_Out_Layout 
// FUNCTION: Create statements which copy the values of the layout 
//   array 'st_layout' into local variables.  Code is added to the 
//   'wn_bounds_code' block. 
//-----------------------------------------------------------------------

static void Mp_Layout_Copy_Out_Layout(WN* wn_outer_loop, 
				      INT tile_count, 
				      ST* st_layout, 
			              WN* wn_bounds_code, 
				      STACK<WN*>* dep_stack) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT64 linenum = WN_Get_Linenum(wn_outer_loop);
  WN* wn_inner_loop = SNL_Get_Inner_Snl_Loop(wn_outer_loop, tile_count);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner_loop, &stack);
  TY_IDX ty_i8 = Be_Type_Tbl(MTYPE_I8);
  TY_IDX ty_i8_ptr = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  INT i;
  for (i = 0; i < stack.Elements(); i++)
    if (stack.Bottom_nth(i) == wn_outer_loop)
      break;
  for (INT j = 0; i < stack.Elements(); j++, i++) {
    WN* wn_loop = stack.Bottom_nth(i);
    WN* wn_stid = WN_start(wn_loop);
    WN* wn_iload = Create_Array_Load(st_layout, MTYPE_I8, j, 8, tile_count);
    if (Do_Depth(wn_outer_loop) > 0) 
      dg->Add_Vertex(wn_iload); 
    dep_stack->Push(wn_iload); 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    SYMBOL* sym_stid = dli->Mp_Info->Nest_Layout(); 
    WN* wn_iload_cast = wn_iload; 
    if (sym_stid->Type != WN_rtype(wn_iload))
      wn_iload_cast = LWN_Integer_Casts(wn_iload, sym_stid->Type, 
        WN_rtype(wn_iload));
    WN* wn_new_stid = AWN_StidIntoSym(sym_stid, wn_iload_cast);
    WN_Set_Linenum(wn_new_stid, linenum);
    LWN_Insert_Block_Before(wn_bounds_code, NULL, wn_new_stid);
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Layout_Call
// FUNCTION: Create the call to '_dsm_Processor_Layout()' for the
//   nested doacross loop 'wn_loop' of depth 'tile_count'.  Place this
//   call in the block 'wn_bounds_code'. The symbols of the onto array 
//   and the layout array are 'st_onto' and 'st_layout'.  Returns WN* 
//   to create call node. 
//-----------------------------------------------------------------------

static WN* Mp_Layout_Call(WN* wn_loop, 
		          INT tile_count, 
	                  ST* st_onto, 
		          ST* st_layout, 
		          WN* wn_bounds_code)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  INT64 linenum = WN_Get_Linenum(wn_loop);
  WN_Set_Linenum(wn_bounds_code, linenum);
  TY_IDX ty_i8 = Be_Type_Tbl(MTYPE_I8);
  TY_IDX ty_i8_ptr = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));

  // Create the call.
  OPCODE op_call = OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V);
  WN* wn_call = WN_Create(op_call, 4);
  WN_st_idx(wn_call) = ST_st_idx(distr_st_entries[Processor_Layout]);
  WN_Set_Call_Parm_Mod(wn_call);
  WN_Set_Call_Parm_Ref(wn_call);
  WN_Set_Linenum(wn_call, linenum);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  if (Do_Depth(wn_loop) > 0)
    dg->Add_Vertex(wn_call); 

  // Kid 0: number of threads
  WN* wn_num_threads = dli->Mp_Info->Is_Pdo()
    ? (Is_Versioned_Mp_Loop(wn_loop))
    ? Get_Runtime_Cur_Numthreads_Ldid()
    : Get_Runtime_Cur_Numthreads_Func(wn_loop)
    : Get_Frozen_Numthreads_Ldid(wn_loop);
  TYPE_ID type = WN_rtype(wn_num_threads);
  if (type != MTYPE_I8)
    wn_num_threads = LWN_Integer_Casts(wn_num_threads, MTYPE_I8, type);
  WN* wn_parm = WN_CreateParm(MTYPE_I8, wn_num_threads, Be_Type_Tbl(MTYPE_I8),
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_num_threads, wn_parm);
  WN_kid(wn_call, 0) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);

  // Arg 1: Number of loops in nested doacross
  WN* wn_tile_count = LWN_Make_Icon(MTYPE_I8, tile_count);
  type = WN_rtype(wn_tile_count);
  if (type != MTYPE_I8)
    wn_tile_count = LWN_Integer_Casts(wn_tile_count, MTYPE_I8, type);
  wn_parm = WN_CreateParm(MTYPE_I8, wn_tile_count, Be_Type_Tbl(MTYPE_I8),
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_tile_count, wn_parm);
  WN_kid(wn_call, 1) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);

  // Arg 2: Onto array
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_onto = WN_CreateLda(op_lda, 0, ty_i8_ptr, st_onto);
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(st_onto);
#else
  Set_ST_addr_taken_passed(st_onto);
#endif
  wn_parm = WN_CreateParm(Pointer_type, wn_onto, ty_i8_ptr,
    WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(wn_onto, wn_parm);
  WN_kid(wn_call, 2) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);

  // Arg 3: Layout array
  op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_layout = WN_CreateLda(op_lda, 0, ty_i8_ptr, st_layout);
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(st_layout);
#else
  Set_ST_addr_taken_passed(st_layout);
#endif
  wn_parm = WN_CreateParm(Pointer_type, wn_layout, ty_i8_ptr,
    WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(wn_layout, wn_parm);
  WN_kid(wn_call, 3) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);

  // Insert into the wn_bounds_code block.
  LWN_Insert_Block_Before(wn_bounds_code, WN_first(wn_bounds_code), wn_call);
  return wn_call; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Layout_Copy_In_Onto 
// FUNCTION: Adds a set of array assignments to the 'wn_bounds_code' which 
//   initialize the 'tile_count' elements of the 'st_onto' array. 
// NOTE: Right now, these are all initialized to 0, which is the default.
//   Later, we will have to get the values from the ONTO clause.  
//-----------------------------------------------------------------------

static void Mp_Layout_Copy_In_Onto(WN* wn_loop, 
				   INT tile_count, 
				   ST* st_onto, 
			           WN* wn_bounds_code, 
				   STACK<WN*>* dep_stack)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  INT* onto_consts = CXX_NEW_ARRAY(INT, tile_count, &LNO_local_pool); 
  INT onto_count = 0; 
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  WN* wn_first = WN_first(WN_region_pragmas(wn_region));
  for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) {
    if (WN_opcode(wn) == OPC_XPRAGMA && WN_pragma(wn) == WN_PRAGMA_ONTO) {
      FmtAssert(WN_operator(WN_kid0(wn)) == OPR_INTCONST,
        ("Parameter to ONTO clause is not constant."));
      onto_consts[onto_count++] = WN_const_val(WN_kid0(wn));
    }
  }
  FmtAssert(onto_count == 0 || onto_count == tile_count, 
    ("Wrong number of onto pragmas in region"));  
  for (INT i = tile_count - 1; i >= 0; i--) {
    WN* wn_value = onto_count == 0 ? LWN_Make_Icon(MTYPE_I8, 0) 
      : LWN_Make_Icon(MTYPE_I8, onto_consts[i]); 
    WN* wn_istore = Create_Array_Store(st_onto, MTYPE_I8, i, 8, 
      tile_count, wn_value);
    LWN_Insert_Block_Before(wn_bounds_code, WN_first(wn_bounds_code), 
      wn_istore);
    if (Do_Depth(wn_loop) > 0) 
      dg->Add_Vertex(wn_istore);
    dep_stack->Push(wn_istore); 
  } 
}

//-----------------------------------------------------------------------
// NAME: Mp_Insert_Bounds_Code 
// FUNCTION: Insert the 'wn_bounds_code' into the nested do across loop        
//   'wn_loop'. If 'inside' then insert it inside 'wn_loop', otherwise, 
//   insert it outside 'wn_loop'. 
//-----------------------------------------------------------------------

static void Mp_Insert_Bounds_Code(WN* wn_loop, 
				  WN* wn_bounds_code,
				  BOOL inside)
{
  
  if (inside) { 
    WN* wnn = NULL; 
    WN* wn = 0;
    for (wn = WN_first(wn_bounds_code); wn != NULL; 
      wnn = wn, wn = WN_next(wn));
    for (wn = wnn; wn != NULL; wn = wnn) {
      wnn = WN_prev(wn);
      LWN_Extract_From_Block(wn);
      LWN_Insert_Block_Before(WN_do_body(wn_loop), 
	WN_first(WN_do_body(wn_loop)), wn);
    }
  } else {
    WN* wnn = NULL; 
    for (WN* wn = WN_first(wn_bounds_code); wn != NULL; wn = wnn) {
      wnn = WN_next(wn);
      LWN_Extract_From_Block(wn);
      LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, wn); 
    }
  }
  LWN_Delete_Tree(wn_bounds_code);
}

//-----------------------------------------------------------------------
// NAME: Mp_Fix_Deps 
// FUNCTION: Build access arrays for all of the nodes on 'dep_stack', 
//   and add self-dependent arcs for all of the OPR_ISTORE on the 'dep_stack'.
//-----------------------------------------------------------------------

static void Mp_Fix_Deps(STACK<WN*>* dep_stack)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  for (INT i = 0; i < dep_stack->Elements(); i++) {
    WN* wn_inode = dep_stack->Bottom_nth(i); 
    OPERATOR opr = WN_operator(wn_inode); 
    WN* wn_array = opr == OPR_ILOAD ? WN_kid0(wn_inode) : WN_kid1(wn_inode);  
    DOLOOP_STACK st_stack(&LNO_local_pool);  
    Build_Doloop_Stack(wn_inode, &st_stack); 
    LNO_Build_Access_Array(wn_array, &st_stack, &LNO_default_pool); 
    if (opr == OPR_ISTORE && Do_Depth(wn_inode) > 0) { 
      if (!dg->Add_Edge(wn_inode, &st_stack, wn_inode, &st_stack, FALSE))
	LNO_Erase_Dg_From_Here_In(wn_inode, dg);
    } 
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Fix_Ref_Array_Aliases
// FUNCTION: Add alias information for the PARM nodes in the tree rooted 
//   at 'wn_call' which are parents of LDA nodes of 'st_alias' by cloning 
//   alias information from an OPR_ILOAD or OPR_ISTORE which is the grand-
//   parent of an LDA with the same 'st_alias' and occurs in the tree rooted 
//   at 'wn_bounds_code'.  
//-----------------------------------------------------------------------

static void Mp_Fix_Ref_Array_Aliases(WN* wn_call, 
				     WN* wn_bounds_code, 
				     ST* st_alias)
{
  WN* wn = NULL; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_bounds_code); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    if (WN_operator(itr->wn) == OPR_LDA
	&& WN_st(itr->wn) == st_alias 
	&& WN_operator(LWN_Get_Parent(itr->wn)) != OPR_PARM) {
      wn = itr->wn; 
      break;  
    }
  }
  FmtAssert(wn != NULL, 
    ("Mp_Fix_Ref_Array_Aliases: Could not find LDA of array node"));
  WN* wn_inode = LWN_Get_Parent(LWN_Get_Parent(wn)); 
  FmtAssert(wn_inode != NULL && (WN_operator(wn_inode)
    == OPR_ILOAD || WN_operator(wn_inode) == OPR_ISTORE), 
    ("Mp_Fix_Ref_Array_Aliases: Could not find array node"));
  wn = NULL; 
  itr = LWN_WALK_TreeIter(wn_call); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    if (WN_operator(itr->wn) == OPR_LDA
	&& WN_st(itr->wn) == st_alias 
	&& WN_operator(LWN_Get_Parent(itr->wn)) == OPR_PARM) {
      wn = itr->wn; 
      break;  
    }
  }
  FmtAssert(wn != NULL, 
    ("Mp_Fix_Ref_Array_Aliases: Could not find LDA of PARM node")); 
  WN* wn_parm = LWN_Get_Parent(wn);  
  FmtAssert(wn_parm != NULL && WN_operator(wn_parm) == OPR_PARM, 
    ("Mp_Fix_Ref_Array_Aliases: Could not find PARM node"));
  Copy_alias_info(Alias_Mgr, wn_inode, wn_parm); 
}

//-----------------------------------------------------------------------
// NAME: Is_Orphaned_Pdo
// FUNCTION: Returns TRUE if the loop 'wn_loop' is an orphaned PDO,
//   FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Is_Orphaned_Pdo(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->Mp_Info == NULL)
    return FALSE;
  if (!dli->Mp_Info->Is_Pdo())
    return FALSE;
  for (WN* wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_operator(wn) == OPR_REGION) {
      WN* wn_first = WN_first(WN_region_pragmas(wn));
      if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA
          && WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN)
        return FALSE;
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Mp_Localize_Onto_and_Layout
// FUNCTION: For the loop 'wn_loop', localize the arrays with ST*'s 
//   'st_onto' and 'st_layout' if these arrays are within a PD inside a
//   parallel region.
//-----------------------------------------------------------------------

static void Mp_Localize_Onto_and_Layout(WN* wn_loop,
					ST* st_onto, 
			   		ST* st_layout)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->Mp_Info->Is_Pdo() && !Is_Orphaned_Pdo(wn_loop)) {
    WN* wn = 0;
    for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_REGION) {
	WN* wn_first = WN_first(WN_region_pragmas(wn));
	if (wn_first != NULL 
	    && WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN)
	  break;
      }
    } 
    FmtAssert(wn != NULL, ("Mp_Layout_Code: Could not find parallel region"));
    Add_Pragma_To_MP_Region(wn, st_layout, 0, WN_PRAGMA_LOCAL);
    Add_Pragma_To_MP_Region(wn, st_onto, 0, WN_PRAGMA_LOCAL);
  }
} 
 
//-----------------------------------------------------------------------
// NAME: Mp_Layout_Code
// FUNCTION: Add code before 'wn_loop' of depth 'tile_count' which will 
//   compute the processor layout from the 'st_onto' array and place it 
//   in the 'st_layout' array.  
//-----------------------------------------------------------------------

static void Mp_Layout_Code(WN* wn_loop, 
			   INT tile_count, 
			   ST** st_onto, 
			   ST** st_layout) 
{
  char Str_Buf[256];
  STACK<WN*> dep_stack(&LNO_local_pool); 
  sprintf(Str_Buf, "onto%d", WN_map_id(wn_loop));  
  *st_onto = Create_Local_Array_ST(Str_Buf, Be_Type_Tbl(MTYPE_I8), 
    tile_count);
  sprintf(Str_Buf, "layout%d", WN_map_id(wn_loop));  
  *st_layout = Create_Local_Array_ST(Str_Buf, Be_Type_Tbl(MTYPE_I8), 
    tile_count);
  WN* wn_bounds_code = WN_CreateBlock(); 
  Mp_Layout_Load_Pids(wn_loop, tile_count); 
  Mp_Layout_Copy_Out_Layout(wn_loop, tile_count, *st_layout, wn_bounds_code,
    &dep_stack); 
  WN* wn_call = Mp_Layout_Call(wn_loop, tile_count, *st_onto, *st_layout, 
    wn_bounds_code);
  Mp_Layout_Copy_In_Onto(wn_loop, tile_count, *st_onto, wn_bounds_code, 
    &dep_stack);   
  Mp_Fix_Ref_Array_Aliases(wn_call, wn_bounds_code, *st_onto); 
  Mp_Fix_Ref_Array_Aliases(wn_call, wn_bounds_code, *st_layout); 
  Mp_Insert_Bounds_Code(wn_loop, wn_bounds_code, FALSE); 
  Mp_Fix_Deps(&dep_stack); 
  Mp_Localize_Onto_and_Layout(wn_loop, *st_onto, *st_layout);
}

//-----------------------------------------------------------------------
// NAME: Mp_Layout_Lego_Layout                                   
// FUNCTION: Create statements to copy the layout for the nested        
//   doacross loop 'wn_outer_loop' of depth 'tile_count' from the 
//   appropriate processor counts of the corresponding distributed array
//   for that loop.  Code is added to the 'wn_bounds_code' block.
//-----------------------------------------------------------------------

static void Mp_Layout_Lego_Layout(WN* wn_outer_loop, 
				  INT tile_count, 
			          ST* st_layout, 
			          WN* wn_bounds_code,
			          STACK<WN*>* dep_stack) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT64 linenum = WN_Get_Linenum(wn_outer_loop);
  WN* wn_inner_loop = SNL_Get_Inner_Snl_Loop(wn_outer_loop, tile_count);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner_loop, &stack);
  TY_IDX ty_i8 = Be_Type_Tbl(MTYPE_I8);
  TY_IDX ty_i8_ptr = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  INT outer_depth = Do_Loop_Depth(wn_outer_loop); 
  for (INT i = outer_depth; i < stack.Elements(); i++) {
    WN* wn_loop = stack.Bottom_nth(i);
    WN* wn_stid = WN_start(wn_loop);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    WN* wn_ldid = NULL; 
    if (dli->Lego_Info->Dynamic_Affinity()) {
      WN* wn_numthreads_code = NULL; 
      wn_ldid = Numprocs(dli->Lego_Info->Array()->St(), 
                         dli->Lego_Info->Dim_Num(),
                         Do_Depth(wn_outer_loop) > 0, 
                         &wn_numthreads_code); 
      WN* wnn = NULL;
      for (WN* wn = WN_first(wn_numthreads_code); wn != NULL; wn = wnn) {
	wnn = WN_next(wn);
	LWN_Extract_From_Block(wn);
	LWN_Insert_Block_Before(LWN_Get_Parent(wn_outer_loop), 
	  wn_outer_loop, wn);
        LWN_Copy_Linenumber(wn_outer_loop,wn);

        // Numprocs may create a conditional, in which case we need to
        // build if-info
        //
        if (WN_opcode(wn) == OPC_IF) {
          IF_INFO *ii=CXX_NEW (IF_INFO(&LNO_default_pool,
            Find_SCF_Inside(wn, OPC_DO_LOOP) != NULL,
            Find_SCF_Inside(wn, OPC_REGION) != NULL), &LNO_default_pool);
          WN_MAP_Set(LNO_Info_Map,wn,(void *)ii);
          DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                        &LNO_local_pool);
          Build_Doloop_Stack(wn, stack);
          LNO_Build_If_Access(wn, stack);
          CXX_DELETE(stack, &LNO_local_pool);
        }
      }
      LWN_Delete_Tree(wn_numthreads_code);
    } else {
      wn_ldid = Get_Numthreads_Ldid(dli->Lego_Info);
    }
    WN* wn_istore = Create_Array_Store(st_layout, MTYPE_I8, i - outer_depth, 
      8, tile_count, wn_ldid); 
    if (Do_Depth(wn_outer_loop) > 0) 
      dg->Add_Vertex(wn_istore); 
    dep_stack->Push(wn_istore); 
    WN_Set_Linenum(wn_istore, linenum);
    LWN_Insert_Block_Before(wn_bounds_code, NULL, wn_istore);
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Layout_Code 
// FUNCTION: Add code before 'wn_loop' of depth 'tile_count' which will 
//   compute the processor layout from the LEGO_INFOs of the loops in the 
//   nested doacross and place it in the 'st_layout' array.  
//-----------------------------------------------------------------------

static void Lego_Layout_Code(WN* wn_loop,
			     INT tile_count, 
			     ST** st_layout)
{
  char Str_Buf[256];
  STACK<WN*> dep_stack(&LNO_local_pool); 
  sprintf(Str_Buf, "layout%d", WN_map_id(wn_loop));
  *st_layout = Create_Local_Array_ST(Str_Buf, Be_Type_Tbl(MTYPE_I8),
    tile_count);
  WN* wn_bounds_code = WN_CreateBlock();
  Mp_Layout_Lego_Layout(wn_loop, tile_count, *st_layout, wn_bounds_code,
    &dep_stack);
  Mp_Insert_Bounds_Code(wn_loop, wn_bounds_code, FALSE);
  Mp_Fix_Deps(&dep_stack); 
}

//-----------------------------------------------------------------------
// NAME: Add_Condition 
// FUNCTION: Accumulate information about the 'wn_cond' in 'info', and 
//   CAND it with the existing condition under 'wn_if' if 'wn_cond' is 
//   not redundant.  Returns FALSE if the condition was proved to be 
//   redundant, TRUE otherwise.  
//-----------------------------------------------------------------------

extern BOOL Add_Condition(COND_BOUNDS_INFO* info, 
			  WN* wn_cond,
			  WN* wn_if)
{
  if (Redundant_Condition(info, wn_cond, wn_if))
    return FALSE; 
  WN* wn_total_cond = WN_if_test(wn_if); 
  OPCODE op_cand = OPCODE_make_op(OPR_CAND, Boolean_type, MTYPE_V); 
  wn_total_cond = LWN_CreateExp2(op_cand, wn_total_cond, wn_cond); 
  WN_if_test(wn_if) = wn_total_cond; 
  LWN_Parentize(wn_if);
  DOLOOP_STACK stack2(&LNO_local_pool); 
  Build_Doloop_Stack(wn_if, &stack2); 
  LNO_Build_If_Access(wn_if, &stack2); 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Prune_Redundant_Trues 
// FUNCTION: Apply the identity X .CAND. 1 == X to simplify the expression
//   'wn_cond'.
// NOTE: We can get rid of this function when the simplifier handles 
//   short-circuited ANDs (i.e CANDs).  
//-----------------------------------------------------------------------

static void Prune_Redundant_Trues(WN* wn_cond)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr;
  for (INT i = 0; i < WN_kid_count(wn_cond); i++) 
    Prune_Redundant_Trues(WN_kid(wn_cond, i)); 

  if (WN_operator(wn_cond) == OPR_CAND) {
    WN* wn_original = NULL; 
    if (WN_operator(WN_kid0(wn_cond)) == OPR_INTCONST
        && WN_const_val(WN_kid0(wn_cond)) == 1) {
      wn_original = WN_kid1(wn_cond); 
    } else if (WN_operator(WN_kid1(wn_cond)) == OPR_INTCONST
        && WN_const_val(WN_kid1(wn_cond)) == 1) {
      wn_original = WN_kid0(wn_cond); 
    } 
    if (wn_original != NULL) { 
      WN* wn_copy = LWN_Copy_Tree(wn_original); 
      LWN_Copy_Def_Use(wn_original, wn_copy, du); 
      dg->Add_Deps_To_Copy_Block(wn_original, wn_copy, FALSE); 
      WN* wn_old_cond = wn_cond; 
      WN* wn_parent = LWN_Get_Parent(wn_cond); 
      INT i;
      for (i = 0; i < WN_kid_count(wn_parent); i++) 
	if (WN_kid(wn_parent, i) == wn_cond)
	  break; 
      WN_kid(wn_parent, i) = wn_copy; 
      LWN_Set_Parent(wn_copy, wn_parent); 
      LWN_Delete_Tree(wn_old_cond); 
    } 
  } 
}

//-----------------------------------------------------------------------
// NAME: Mp_Retained_Pragma 
// FUNCTION: Returns TRUE if the node 'wn_pragma' should be retained 
//   while serializing parallel code, FALSE otherwise. The boolean 
//   'auto_parallel' is TRUE if the pragma was created by auto parallel-
//   ization, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Mp_Retained_Pragma(WN* wn_pragma, 
			       BOOL auto_parallel)
{
  FmtAssert(WN_opcode(wn_pragma) == OPC_PRAGMA 
    || WN_opcode(wn_pragma) == OPC_XPRAGMA, 
    ("Mp_Retained_Pragma: Argument not a pragma node")); 
  switch (WN_pragma(wn_pragma)) { 
  case WN_PRAGMA_CRITICAL_SECTION_BEGIN:
  case WN_PRAGMA_CRITICAL_SECTION_END:
  case WN_PRAGMA_BARRIER:
  case WN_PRAGMA_ENTER_GATE:
  case WN_PRAGMA_EXIT_GATE:
  case WN_PRAGMA_INDEPENDENT_BEGIN:
  case WN_PRAGMA_INDEPENDENT_END:
    return TRUE; 
  case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
  case WN_PRAGMA_SINGLE_PROCESS_END:
    return !auto_parallel;  
  default: 
    return FALSE; 
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Basic_Parallel_Construct
// FUNCTION: Returns TRUE if 'wn_region' represents a "basic parallel 
//   construct", i.e. a place at which we spawn a group of parallel 
//   threads of execution. 
//-----------------------------------------------------------------------

static BOOL Mp_Basic_Parallel_Construct(WN* wn_region)
{
  if (WN_opcode(wn_region) == OPC_REGION) { 
    WN* wn_first = WN_first(WN_region_pragmas(wn_region));
    if (wn_first == NULL)
      return FALSE; 
    if (WN_opcode(wn_first) == OPC_PRAGMA) { 
      switch(WN_pragma(wn_first)) { 
      case WN_PRAGMA_PARALLEL_BEGIN: 
      case WN_PRAGMA_DOACROSS:
      case WN_PRAGMA_PARALLEL_DO: 
        return TRUE; 
      } 
    } 
  }
  return FALSE; 
}  

//-----------------------------------------------------------------------
// NAME: Mp_Disable_Opts_On_Internal_Regions
// FUNCTION: Disables pseudo-lowering on all MP do loops in the tree rooted 
//   at 'wn_tree' nested inside basic parallel regions (where the 
//   'parallel_level' is greater than 0). 
//-----------------------------------------------------------------------

static void Mp_Disable_Opts_On_Internal_Regions(WN* wn_tree, 
						INT parallel_level)
{
  if (WN_opcode(wn_tree) == OPC_REGION) {
    if (Mp_Basic_Parallel_Construct(wn_tree)) { 
      for (INT i = 0; i < WN_kid_count(wn_tree); i++) {
	Mp_Disable_Opts_On_Internal_Regions(WN_kid(wn_tree, i), 
	  Mp_Basic_Parallel_Construct(wn_tree) 
	    ? parallel_level + 1 : parallel_level);
      } 
    } 
  } 

  if (WN_opcode(wn_tree) == OPC_DO_LOOP && parallel_level >= 1
      && Do_Loop_Is_Mp(wn_tree)) { 
    DO_LOOP_INFO* dli_tree = Get_Do_Loop_Info(wn_tree); 
    dli_tree->Mp_Info->Disable_Plowering(); 
  } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Mp_Disable_Opts_On_Internal_Regions(wn, parallel_level); 
  } else {  
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Mp_Disable_Opts_On_Internal_Regions(WN_kid(wn_tree, i), 
	parallel_level);
  }  
} 

//-----------------------------------------------------------------------
// NAME: Mp_Extract_Parallel_Directives
// FUNCTION: Extract directives and pdo regions from the parallel region 
//   'wn_tree' to make it serial code. The boolean 'auto_parallel' is TRUE
//   if the directives were created by auto parallelization, FALSE otherwise. 
//-----------------------------------------------------------------------

static void Mp_Extract_Parallel_Directives(WN* wn_tree, 
					   BOOL auto_parallel, 
					   INT parallel_level)
{
  if (WN_opcode(wn_tree) == OPC_REGION) {
    if (Mp_Basic_Parallel_Construct(wn_tree)) { 
      if (parallel_level >= 1) 
        return; 
      for (INT i = 0; i < WN_kid_count(wn_tree); i++)
	Mp_Extract_Parallel_Directives(WN_kid(wn_tree, i), auto_parallel, 
	  parallel_level + 1);
    } else if (Is_Mp_Region(wn_tree)) {  
      WN* wnn = NULL; 
      BOOL has_retained_pragma = FALSE; 
      WN* wn_first = WN_first(WN_region_pragmas(wn_tree));
      for (WN* wn = wn_first; wn != NULL; wn = wnn) { 
	wnn = WN_next(wn); 
        if (Mp_Retained_Pragma(wn, auto_parallel)) {
	  has_retained_pragma = TRUE; 
	} else { 
	  LWN_Extract_From_Block(wn); 
	  LWN_Delete_Tree(wn); 
	} 
      }  
      if (!has_retained_pragma) {  
	WN* wn_new_tree = WN_first(WN_region_body(wn_tree)); 
	WN* wnn = NULL; 
	for (WN* wn = wn_new_tree; wn != NULL; wn = wnn) {  
	  wnn = WN_next(wn); 
	  LWN_Extract_From_Block(wn); 
	  LWN_Insert_Block_Before(LWN_Get_Parent(wn_tree), wn_tree, wn);
	  Mp_Extract_Parallel_Directives(wn, auto_parallel, parallel_level); 
	} 
	LWN_Extract_From_Block(wn_tree);
	LWN_Delete_Tree(wn_tree);
	return; 
      } 
    } 
  }  

  if ((WN_opcode(wn_tree) == OPC_PRAGMA || WN_opcode(wn_tree) == OPC_XPRAGMA)
      && (WN_pragmas[WN_pragma(wn_tree)].users & PUSER_MP)
      && !Mp_Retained_Pragma(wn_tree, auto_parallel)) {
    LWN_Extract_From_Block(wn_tree); 
    LWN_Delete_Tree(wn_tree);
    return;
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    WN* wnn = NULL; 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = wnn) {
      wnn = WN_next(wn); 
      Mp_Extract_Parallel_Directives(wn, auto_parallel, parallel_level);  
    }
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Mp_Extract_Parallel_Directives(WN_kid(wn_tree, i), auto_parallel,
	parallel_level);
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Retained_Region
// FUNCTION: Return TRUE if the REGION rooted at 'wn_region' will be 
//   retained after stripping out parallel code to create a serial version.
//   The BOOL 'auto_parallel' is true if we are performing automatic 
//   parallelization. 
//-----------------------------------------------------------------------
      
static BOOL Mp_Retained_Region(WN* wn_region,
			       BOOL auto_parallel)
{ 
  FmtAssert(WN_opcode(wn_region) == OPC_REGION, 
    ("Mp_Retained_Region(): Expecting a REGION node"));
  WN* wn_first = WN_first(WN_region_pragmas(wn_region));
  if (WN_opcode(wn_first) == OPC_PRAGMA) {
    switch(WN_pragma(wn_first)) {
    case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
      return !auto_parallel; 
    default: 
      return TRUE; 
    } 
  }
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Mp_Trip_Count 
// FUNCTION: Return the trip count of the MP loop 'wn_loop'.  
//-----------------------------------------------------------------------

static WN* Mp_Trip_Count(WN* wn_loop)
{
  DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop); 
  INT nloops = dli_loop->Mp_Info->Nest_Total(); 
  if (!Fully_Permutable_Permutation(wn_loop, nloops))
    return NULL; 
  WN* wn_trip = NULL; 
  for (WN* wn = wn_loop; wn != NULL; wn = Next_SNL_Loop(wn)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    FmtAssert(dli->Mp_Info != NULL, ("Mp_Trip_Count: Expecting MP loop")); 
    WN* wn_local_trip = Trip_Count(wn);
    if (wn_local_trip == NULL) {
      LWN_Delete_Tree(wn_trip); 
      return NULL;  
    } 
    if (wn_trip == NULL) {
      wn_trip = wn_local_trip; 
    } else { 
      TYPE_ID type = Max_Wtype(WN_rtype(wn_trip), WN_rtype(wn_local_trip));
      wn_trip = AWN_Mpy(type, wn_trip, wn_local_trip); 
    } 
    if (dli->Mp_Info->Nest_Index() + 1 == dli->Mp_Info->Nest_Total())
      break; 
  } 
  return wn_trip; 
}  

//-----------------------------------------------------------------------
// NAME: Mp_Want_Version_Mp_Loop
// FUNCTION: Returns TRUE if the DOACROSS loop 'wn_loop' showed be ver-
//   sioned in LNO, returns FALSE otherwise.  If 'test_already', return
//   FALSE if we have already versioned.
//-----------------------------------------------------------------------

extern BOOL Mp_Want_Version_Loop(WN* wn_loop,
                                 BOOL test_already)
{
  if (!LNO_Version_Mp_Loops)
    return FALSE;

  if (test_already && Is_Versioned_Mp_Loop(wn_loop))
    return FALSE;

  INT construct_count = 0; 
  for (WN* wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (Mp_Basic_Parallel_Construct(wn))
      construct_count++; 
  if (construct_count > 1)
    return FALSE; 

  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->Mp_Info->Is_Pdo() && !Is_Orphaned_Pdo(wn_loop))
    return FALSE;

  // Return if code sandwiched between REGION and DOACROSS
  // May not be able to create trip count expression
  if (WN_prev(wn_loop) && !Is_Orphaned_Pdo(wn_loop))
    return FALSE;

  // Must Be OK.  Passed all tests.
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Mp_Version_Loop 
// FUNCTION: Version the mp loop 'wn_loop' into a parallel and scalar copy.
//-----------------------------------------------------------------------

static WN* Mp_Version_Loop(WN* wn_loop) 
{
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 
  if (!Mp_Want_Version_Loop(wn_loop, TRUE))
    return NULL;

  // Determine whether auto or manual parallel. 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  BOOL is_auto_parallel = dli->Auto_Parallelized; 
  BOOL is_pdo = dli->Mp_Info->Is_Pdo();
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop)); 
  BOOL is_omp = WN_pragma_omp(WN_first(WN_region_pragmas(wn_region))); 
  INT nest_total = dli->Mp_Info->Nest_Total(); 
 
  // Create the scalar copy. 
  TYPE_ID type = Promote_Type(Do_Wtype((WN *) wn_loop));
  WN* wn_block = LWN_Get_Parent(wn_loop); 
  WN* wn_first = WN_first(WN_region_pragmas(wn_region));
  FmtAssert(wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA,
    ("Mp_Version_Loop: Missing PRAGMA in MP region")); 
  switch (WN_pragma(wn_first)) {
  case WN_PRAGMA_DOACROSS:  
  case WN_PRAGMA_PARALLEL_DO:  
  case WN_PRAGMA_PDO_BEGIN: 
    break;
  default: 
    FmtAssert(FALSE, ("Mp_Version_Loop: Not an MP loop")); 
  }
  WN* wn_parent = LWN_Get_Parent(wn_region);
  WN_MAP version_map = WN_MAP_Create(&LNO_local_pool);
  WN* wn_copy = LWN_Copy_Tree(wn_block, TRUE, LNO_Info_Map, TRUE, version_map);
  BOOL all_internal = WN_Rename_Duplicate_Labels(wn_block, wn_copy,
                        Current_Func_Node, &LNO_local_pool);
  Is_True(all_internal, ("external labels renamed"));

  // Clone the dependences for the scalar copy. 
  WN* wn_array[2]; 
  wn_array[0] = wn_block; 
  wn_array[1] = wn_copy; 
  Unrolled_DU_Update(wn_array, 2, Do_Loop_Depth(wn_loop) - 1, TRUE, FALSE); 
  dg->Versioned_Dependences_Update(wn_block, wn_copy, Do_Loop_Depth(wn_loop),
    version_map);
  WN_MAP_Delete(version_map); 
  if (rm != NULL) 
    rm->Unroll_Update(wn_array, 2); 

  // Delete MP and LEGO info from the scalar clone. 
  INT nest_count = 0; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_copy); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    if (WN_opcode(itr->wn) == OPC_DO_LOOP) {
      WN* wn_loop_copy = itr->wn; 
      DO_LOOP_INFO* dli_copy = Get_Do_Loop_Info(wn_loop_copy); 
      WN* wn_delete_loop = wn_loop_copy; 
      DO_LOOP_INFO* dli_delete = Get_Do_Loop_Info(wn_delete_loop);  
      CXX_DELETE(dli_delete->Mp_Info, &LNO_default_pool); 
      dli_delete->Mp_Info = NULL; 
      dli_delete->Serial_Version_of_Concurrent_Loop = TRUE; 
      if (dli_delete->Lego_Info != NULL) {
	DISTR_ARRAY* dact = 
	    Lookup_DACT(dli_delete->Lego_Info->Array()->St()); 
	if (dact == NULL || !dact->Dinfo()->IsReshaped()) {
	  CXX_DELETE(dli_delete->Lego_Info, LEGO_pool); 
	  dli_delete->Lego_Info = NULL; 
	}
      }
      if (++nest_count >= nest_total)
	break;
    }
  }
  WN* wn_prev = WN_prev(wn_region); 
  LWN_Extract_From_Block(wn_region);
 
  // Start with a condition of .TRUE. for the version test  
  WN* wn_total_cond = LWN_Make_Icon(Boolean_type, 1); 
  WN* wn_if = LWN_CreateIf(wn_total_cond, WN_CreateBlock(), wn_copy);
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_region); 
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_loop));
  IF_INFO *ii =
    CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, TRUE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  WN_Set_If_MpVersion(wn_if); 
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_default_pool), 
    &LNO_default_pool);
  Build_Doloop_Stack(wn_if, stack);
  LNO_Build_If_Access(wn_if, stack);
  LWN_Insert_Block_After(wn_parent, wn_prev, wn_if);
  Mp_Disable_Opts_On_Internal_Regions(wn_block, 0); 
  Mp_Extract_Parallel_Directives(wn_copy, is_auto_parallel, 1); 
  if (is_omp && !is_auto_parallel) { 
    // Commented by csc. 2002/11/14
    // TODO: make the versioning mechanism working.
/*    WN* wn_begin_spr = Gen_OMP_Begin_SPR(MPP_PARALLEL_DO);
    LWN_Insert_Block_After(wn_copy, NULL, wn_begin_spr); 
    WN* wn_end_spr = Gen_OMP_End_SPR(MPP_PARALLEL_DO); 
    LWN_Insert_Block_Before(wn_copy, NULL, wn_end_spr); 
    if (Enclosing_Do_Loop(wn_copy) != NULL) { 
      dg->Add_Vertex(wn_begin_spr); 
      dg->Add_Vertex(wn_end_spr); 
    } 
 */
  } 

  // Walk the pragma list, and add user defined conditions 
  COND_BOUNDS_INFO *info =
       CXX_NEW(COND_BOUNDS_INFO(&LNO_local_pool), &LNO_local_pool);
  info->Collect_Outer_Info(wn_parent); 
  WN* wnn = NULL; 
  wn_first = WN_first(WN_region_pragmas(wn_region)); 
  for (WN* wn = wn_first; wn != NULL; wn = wnn) {
    wnn = WN_next(wn); 
    if (WN_opcode(wn) == OPC_XPRAGMA && WN_pragma(wn) == WN_PRAGMA_IF) { 
      WN* wn_cond = LWN_Copy_Tree(WN_kid0(wn)); 
      LWN_Copy_Def_Use(WN_kid0(wn), wn_cond, du); 
      dg->Add_Deps_To_Copy_Block(WN_kid0(wn), wn_cond, FALSE); 
      if (!Add_Condition(info, wn_cond, wn_if)) 
	LWN_Delete_Tree(wn_cond); 
      LWN_Extract_From_Block(wn); 
      LWN_Delete_Tree(wn); 
    } 
  } 

  // Add a condition for the trip count. 
  WN* wn_trip_count = Mp_Trip_Count(wn_loop);  
  if (wn_trip_count != NULL) { 
    TYPE_ID index_type = Promote_Type(Do_Wtype((WN *) wn_loop));
    WN* wn_one = LWN_Make_Icon(index_type, 1); 
    OPCODE op_gt = OPCODE_make_op(OPR_GT, Boolean_type, index_type); 
    WN* wn_trip_test = LWN_CreateExp2(op_gt, wn_trip_count, wn_one); 
    if (!Add_Condition(info, wn_trip_test, wn_if))
      LWN_Delete_Tree(wn_trip_test); 
  } 

  // Add a condition to determine if we are already in a parallel region. 
  OPCODE op_intrinsic = OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V);
  WN* wn_not_parallel = NULL; 
  if (is_pdo) { 
    WN* wn_intrinsic = WN_Create_Intrinsic(op_intrinsic,
      INTRN_OMP_DO_WORKSHARING, 0, NULL);
    LWN_Parentize(wn_intrinsic);
    wn_not_parallel = wn_intrinsic;
  } else {
    WN* wn_intrinsic = WN_Create_Intrinsic(op_intrinsic,
      INTRN_MP_IN_PARALLEL_REGION, 0, NULL);
    LWN_Parentize(wn_intrinsic);
    OPCODE op_lnot = OPCODE_make_op(OPR_LNOT, Boolean_type, MTYPE_V);
    wn_not_parallel = LWN_CreateExp1(op_lnot, wn_intrinsic);
  }
  Add_Condition(info, wn_not_parallel, wn_if);
  Prune_Redundant_Trues(WN_if_test(wn_if)); 

  if (Cur_PU_Feedback) {
    Update_Guarded_Do_FB(wn_if, wn_loop, Cur_PU_Feedback);
  }

  return wn_copy; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Want_Version_Parallel_Region
// FUNCTION: Returns TRUE if we want to version the parallel region
//   'region' in LNO, FALSE if we do not.  If 'test_already', return
//   FALSE if we have already versioned.
//-----------------------------------------------------------------------

static BOOL Mp_Want_Version_Parallel_Region(WN* wn_region,
                                            BOOL test_already)
{

  if (!LNO_Version_Mp_Loops)
    return FALSE;

  if (test_already && Is_Versioned_Mp_Region(wn_region))
    return FALSE;

  INT construct_count = 0; 
  for (WN* wn = wn_region; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (Mp_Basic_Parallel_Construct(wn))
      construct_count++; 
  if (construct_count > 1)
    return FALSE; 

  // find out if this is a region for sync_doacross loop
  // turn off versioning if it is
  WN* pragmas=WN_region_pragmas(wn_region);
  WN* next_wn=WN_first(pragmas);
  while (next_wn) {
    if (WN_opcode(next_wn)==OPC_PRAGMA)
      if ((WN_PRAGMA_ID)WN_pragma(next_wn)==WN_PRAGMA_SYNC_DOACROSS) {
        return FALSE;
      }
    next_wn=WN_next(next_wn);
  }

  // Must Be OK.  Passed all tests.
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Mp_Delete_Outer_Mp_Lego_Info
// FUNCTION: Deletes the MP and LEGO info for any loop inside 'wn_tree' 
//   but outside any basic parallel construct. 
//-----------------------------------------------------------------------

static void Mp_Delete_Outer_Mp_Lego_Info(WN* wn_tree) 
{ 
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) { 
    DO_LOOP_INFO* dli_tree = Get_Do_Loop_Info(wn_tree);  
    CXX_DELETE(dli_tree->Mp_Info, &LNO_default_pool); 
    dli_tree->Mp_Info = NULL; 
    dli_tree->Serial_Version_of_Concurrent_Loop = TRUE; 
    if (dli_tree->Lego_Info != NULL) {
      DISTR_ARRAY* dact = Lookup_DACT(dli_tree->Lego_Info->Array()->St()); 
      if (dact == NULL || !dact->Dinfo()->IsReshaped()) {
        CXX_DELETE(dli_tree->Lego_Info, LEGO_pool); 
        dli_tree->Lego_Info = NULL; 
      }
    }
  } 

  if (Mp_Basic_Parallel_Construct(wn_tree))
    return; 

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Mp_Delete_Outer_Mp_Lego_Info(wn); 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Mp_Delete_Outer_Mp_Lego_Info(WN_kid(wn_tree, i)); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Mp_Version_Parallel_Region  
// FUNCTION: Version the parallel region 'wn_region' into a parallel and 
//   scalar copy.
//-----------------------------------------------------------------------

static WN* Mp_Version_Parallel_Region(WN* wn_region) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 
  REDUCTION_MANAGER* rm = red_manager; 

  if (!Mp_Want_Version_Parallel_Region(wn_region, TRUE))
    return NULL; 

  // Determine whether auto or manual parallel.
  REGION_INFO* rgi = Get_Region_Info(wn_region); 
  BOOL is_auto_parallel = rgi != NULL && rgi->Auto_Parallelized();  
  BOOL is_omp = WN_pragma_omp(WN_first(WN_region_pragmas(wn_region))); 
 
  // Create the scalar copy. 
  WN* wn_block = WN_region_body(wn_region);
  WN* wn_parent = LWN_Get_Parent(wn_region); 
  WN* wn_prev = WN_prev(wn_region); 
  WN_MAP version_map = WN_MAP_Create(&LNO_local_pool);
  WN* wn_copy = LWN_Copy_Tree(wn_block, TRUE, LNO_Info_Map, TRUE, 
    version_map); 
  BOOL all_internal = WN_Rename_Duplicate_Labels(wn_block, wn_copy,
                        Current_Func_Node, &LNO_local_pool);
  Is_True(all_internal, ("external labels renamed"));

  // Clone the dependences for the scalar copy. 
  WN* wn_array[2]; 
  wn_array[0] = wn_block; 
  wn_array[1] = wn_copy; 
  Unrolled_DU_Update(wn_array, 2, Do_Depth(wn_region), TRUE, FALSE); 
  dg->Versioned_Dependences_Update(wn_block, wn_copy, Do_Depth(wn_region) + 1,
    version_map);
  WN_MAP_Delete(version_map); 
  if (rm != NULL) 
    rm->Unroll_Update(wn_array, 2); 

  // Delete MP and LEGO info from the scalar clone. 
  Mp_Delete_Outer_Mp_Lego_Info(wn_copy); 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_copy); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    if (WN_opcode(itr->wn) == OPC_DO_LOOP) {
    }
  }
  LWN_Extract_From_Block(wn_region);
 
  // Start with a condition of .TRUE. for the version test  
  WN* wn_total_cond = LWN_Make_Icon(Boolean_type, 1); 
  WN* wn_if = LWN_CreateIf(wn_total_cond, WN_CreateBlock(), wn_copy);
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_region); 
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_region));
  BOOL has_do = Find_SCF_Inside(wn_region, OPC_DO_LOOP) != NULL; 
  IF_INFO *ii =
    CXX_NEW(IF_INFO(&LNO_default_pool, has_do, TRUE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  WN_Set_If_MpVersion(wn_if); 
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_default_pool), 
    &LNO_default_pool);
  Build_Doloop_Stack(wn_if, stack);
  LNO_Build_If_Access(wn_if, stack);
  LWN_Insert_Block_After(wn_parent, wn_prev, wn_if);
  Mp_Disable_Opts_On_Internal_Regions(wn_block, 0);
  Mp_Extract_Parallel_Directives(wn_copy, is_auto_parallel, 1); 
  if (is_omp && !is_auto_parallel) { 
	  // commented by csc. 2002/11/14
	  // TODO: make the versioning mechanism working
/*
    WN* wn_begin_spr = Gen_OMP_Begin_SPR(MPP_PARALLEL_REGION);
    LWN_Insert_Block_After(wn_copy, NULL, wn_begin_spr); 
    WN* wn_end_spr = Gen_OMP_End_SPR(MPP_PARALLEL_REGION); 
    LWN_Insert_Block_Before(wn_copy, NULL, wn_end_spr); 
    if (Enclosing_Do_Loop(wn_copy) != NULL) { 
      dg->Add_Vertex(wn_begin_spr); 
      dg->Add_Vertex(wn_end_spr); 
    } 
 */
  } 

  // Walk the pragma list, and add user defined conditions 
  COND_BOUNDS_INFO *info =
       CXX_NEW(COND_BOUNDS_INFO(&LNO_local_pool), &LNO_local_pool);
  info->Collect_Outer_Info(wn_parent); 
  WN* wnn = NULL; 
  WN* wn_first = WN_first(WN_region_pragmas(wn_region)); 
  for (WN* wn = wn_first; wn != NULL; wn = wnn) {
    wnn = WN_next(wn); 
    if (WN_opcode(wn) == OPC_XPRAGMA && WN_pragma(wn) == WN_PRAGMA_IF) { 
      WN* wn_cond = LWN_Copy_Tree(WN_kid0(wn)); 
      LWN_Copy_Def_Use(WN_kid0(wn), wn_cond, du); 
      dg->Add_Deps_To_Copy_Block(WN_kid0(wn), wn_cond, FALSE); 
      if (!Add_Condition(info, wn_cond, wn_if)) 
	LWN_Delete_Tree(wn_cond); 
      LWN_Extract_From_Block(wn); 
      LWN_Delete_Tree(wn); 
    } 
  } 

  if (is_auto_parallel) { 
    WN* wn_loop = Find_SCF_Inside(wn_region, OPC_DO_LOOP); 
    WN* wn_trip_count = Trip_Count(wn_loop);
    TYPE_ID index_type = Promote_Type(Do_Wtype((WN *) wn_loop));
    WN* wn_one = LWN_Make_Icon(index_type, 1);
    OPCODE op_gt = OPCODE_make_op(OPR_GT, Boolean_type, index_type);
    WN* wn_trip_test = LWN_CreateExp2(op_gt, wn_trip_count, wn_one);
    if (!Add_Condition(info, wn_trip_test, wn_if))
      LWN_Delete_Tree(wn_trip_test);
  } 

  // Add a condition to determine if we are already in a parallel region. 
  OPCODE op_intrinsic = OPCODE_make_op(OPR_INTRINSIC_OP, MTYPE_I4, MTYPE_V);
  WN* wn_intrinsic = WN_Create_Intrinsic(op_intrinsic, 
    INTRN_MP_IN_PARALLEL_REGION, 0, NULL);  
  LWN_Parentize(wn_intrinsic); 
  OPCODE op_lnot = OPCODE_make_op(OPR_LNOT, Boolean_type, MTYPE_V);
  WN* wn_not_parallel = LWN_CreateExp1(op_lnot, wn_intrinsic); 
  Add_Condition(info, wn_not_parallel, wn_if);
  Prune_Redundant_Trues(WN_if_test(wn_if)); 
  return wn_copy; 
}

//-----------------------------------------------------------------------
// NAME: Innermost_Doacross_Nest_Loop
// FUNCTION: Returns the innermost loop of the lowered doacross loop
//   'wn_outer'.
//-----------------------------------------------------------------------

static WN* Innermost_Doacross_Nest_Loop(WN* wn_outer)
{
  WN* wn_inner = wn_outer;
  DO_LOOP_INFO* dli = NULL; 
  WN* wn = 0;
  for (wn = wn_outer; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      dli = Get_Do_Loop_Info(wn); 
      if (dli->Lego_Mp_Key_Depth == 0)
	break; 
    }
  }
  FmtAssert(wn != NULL, 
    ("Innermost_Doacross_Nest_Loop: Could not find level 0 loop")); 
  INT lower = dli->Lego_Mp_Key_Lower; 
  INT upper = dli->Lego_Mp_Key_Upper;           
  for (wn = wn_outer; wn != NULL; wn = Next_SNL_Loop(wn)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (dli->Lego_Mp_Key_Lower == 0 || dli->Lego_Mp_Key_Lower < lower 
	|| dli->Lego_Mp_Key_Upper > upper)
      break;
    wn_inner = wn;
  }
  return wn_inner;
}

//-----------------------------------------------------------------------
// NAME: Mp_Tile_Single_Loop
// FUNCTION:  Processor tile (pseudo-lower) the 'loop' according to the
//   information in the MP_INFO.  Memory for data structures created
//   during the tiling is allocated from 'pool'.
//-----------------------------------------------------------------------

static WN* Mp_Tile_Single_Loop(WN* loop,
			       BOOL LNO_Ozero, 
                               MEM_POOL *pool)
{
  WN* wn_new_loop = loop; 
  Is_True(Upper_Bound_Standardize(WN_end(loop), TRUE), 
    ("Tried to MP tile a loop with non-standard upper bound."));
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
  if (dli->Lego_Info != NULL)
    return Lego_Tile_Single_Loop(loop, pool);  

  Is_True (dli->Mp_Info, ("Mp_Tile_Single_Loop(): NULL Mp_Info"));
  if (LNO_Ozero || !LNO_Pseudo_Lower && dli->Mp_Info->Nest_Total() <= 1
      || dli->Mp_Info->Plower_Disabled())
    return wn_new_loop; 

  switch (dli->Mp_Info->Sched_Type()) { 
  case MP_SCHED_SIMPLE: 
    wn_new_loop = Processor_2D_Tile_Loop(loop, pool, FALSE);     
    break; 
  case MP_SCHED_INTERLEAVE: 
    wn_new_loop = Processor_3D_Tile_Loop(loop, pool, FALSE);     
    break; 
  case MP_SCHED_DYNAMIC: 
  case MP_SCHED_GSS: 
  case MP_SCHED_RUNTIME:
  default: 
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(loop); 
    dli->No_Fission = TRUE;
    dli->No_Fusion = TRUE;
    dli->Cannot_Interchange = TRUE;
    dli->Cannot_Block = TRUE;
    dli->Required_Unroll = 1;
    break; 
  }
  return wn_new_loop;   
}    

//-----------------------------------------------------------------------
// NAME: Mp_Coordinate_Copy_Out_Coordinates 
// FUNCTION: Create statements which copy the values of the indexing 
//   array 'st_coordinates' into local variables and propagate those values 
//   where they are needed in the nested 'tile_count' deep nested doacross 
//   loop 'wn_outer_loop'.  Code is added to the 'wn_bounds_code' block. 
//-----------------------------------------------------------------------

static void Mp_Coordinate_Copy_Out_Coordinates(WN* wn_outer_loop, 
			                       INT tile_count, 
				               ST* st_coordinates, 
			                       WN* wn_bounds_code,
					       STACK<WN*>* dep_stack) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 

  INT64 linenum = WN_Get_Linenum(wn_outer_loop);
  WN* wn_inner_tile_loop = SNL_Get_Inner_Snl_Loop(wn_outer_loop, tile_count);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner_tile_loop, &stack);
  TY_IDX ty_i8 = Be_Type_Tbl(MTYPE_I8); 
  TY_IDX ty_i8_ptr = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  INT i;
  for (i = 0; i < stack.Elements(); i++)
    if (stack.Bottom_nth(i) == wn_outer_loop)
      break;
  for (INT j = 0; i < stack.Elements(); j++, i++) {
    WN* wn_loop = stack.Bottom_nth(i); 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_outer_loop); 
    WN* wn_pattern = WN_start(wn_loop);
    WN* wn_iload = Create_Array_Load(st_coordinates, MTYPE_I8, 
      j, 8, tile_count);  
    dg->Add_Vertex(wn_iload); 
    dep_stack->Push(wn_iload); 
    SYMBOL sym_stid(wn_pattern); 
    WN* wn_iload_cast = wn_iload;
    if (sym_stid.Type != MTYPE_I8)
      wn_iload_cast = LWN_Integer_Casts(wn_iload_cast, sym_stid.Type, MTYPE_I8);
    WN* wn_new_stid = AWN_StidIntoSym(&sym_stid, wn_iload_cast);
    WN_Set_Linenum(wn_new_stid, linenum);
    LWN_Insert_Block_Before(wn_bounds_code, NULL, wn_new_stid);
    STACK<WN*> nstack(&LNO_local_pool);
    Lego_Find_Nodes(OPR_LDID, sym_stid, WN_do_body(wn_loop), &nstack);  
    for (INT j = 0; j < nstack.Elements(); j++) {
      WN* wn_ldid = nstack.Bottom_nth(j); 
      du->Remove_Use_From_System(wn_ldid); 
      du->Add_Def_Use(wn_new_stid, wn_ldid); 
      DEF_LIST *def_list = du->Ud_Get_Def(wn_ldid);
      def_list->Set_loop_stmt(NULL);
    }
  }
  Add_Pragma_To_MP_Region(wn_outer_loop, st_coordinates, 0, WN_PRAGMA_LOCAL); 
}

//-----------------------------------------------------------------------
// NAME: Mp_Coordinate_Call 
// FUNCTION: Create the call to '_dsm_Processor_Coordinates()' for the 
//   nested doacross loop 'wn_loop' of depth 'tile_count'.  Place this 
//   call in the block 'wn_bounds_code'. The symbols of the layout array and 
//   the coordinate array are 'st_layout' and 'st_coordinates'.  Returns
//   WN* to created call node. 
//-----------------------------------------------------------------------

static WN* Mp_Coordinate_Call(WN* wn_loop,         
                              INT tile_count,
			      ST* st_layout,    
			      ST* st_coordinates, 
                              WN* wn_bounds_code)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr;

  INT64 linenum = WN_Get_Linenum(wn_loop);
  WN_Set_Linenum(wn_bounds_code, linenum);
  TY_IDX ty_i8 = Be_Type_Tbl(MTYPE_I8); 
  TY_IDX ty_i8_ptr = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));

  // Create assignments for the iteration variables
  // Create the call. 
  OPCODE op_call = OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V);
  WN* wn_call = WN_Create(op_call, 4);
  WN_st_idx(wn_call) = ST_st_idx(distr_st_entries[Processor_Coordinates]);
  WN_Set_Call_Parm_Mod(wn_call);
  WN_Set_Call_Parm_Ref(wn_call);
  WN_Set_Linenum(wn_call, linenum);
  dg->Add_Vertex(wn_call);

  // Arg 0: Layout array
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_layout = WN_CreateLda(op_lda, 0, ty_i8_ptr, st_layout); 
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(st_layout);
#else
  Set_ST_addr_taken_passed(st_layout);
#endif
  WN* wn_parm = WN_CreateParm(Pointer_type, wn_layout, ty_i8_ptr, 
    WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(wn_layout, wn_parm);
  WN_kid(wn_call, 0) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);
 
  // Arg 1: Number of loops in nested doacross 
  WN* wn_tile_count = LWN_Make_Icon(MTYPE_I8, tile_count);
  TYPE_ID type = WN_rtype(wn_tile_count);
  if (type != MTYPE_I8)
    wn_tile_count = LWN_Integer_Casts(wn_tile_count, MTYPE_I8, type);
  wn_parm = WN_CreateParm(MTYPE_I8, wn_tile_count, Be_Type_Tbl(MTYPE_I8),
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_tile_count, wn_parm);
  WN_kid(wn_call, 1) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);

  // Arg 2: My thread id 
  SYMBOL sym_index(WN_start(wn_loop)); 
  WN* wn_mythreadid = AWN_LdidSym(&sym_index); 
  type = WN_rtype(wn_mythreadid);
  WN* wn_mythreadid_cast = wn_mythreadid; 
  if (type != MTYPE_I8)
    wn_mythreadid_cast = LWN_Integer_Casts(wn_mythreadid, MTYPE_I8, type);
  wn_parm = WN_CreateParm(MTYPE_I8, wn_mythreadid_cast, Be_Type_Tbl(MTYPE_I8),
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_mythreadid_cast, wn_parm);
  WN_kid(wn_call, 2) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);
  du->Add_Def_Use(WN_start(wn_loop), wn_mythreadid);
  du->Add_Def_Use(WN_step(wn_loop), wn_mythreadid); 
  DEF_LIST *def_list = du->Ud_Get_Def(wn_mythreadid);
  def_list->Set_loop_stmt(wn_loop);
   
  // Arg 3: Coordinate array 
  op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_coordinate = WN_CreateLda(op_lda, 0, ty_i8_ptr, st_coordinates); 
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(st_coordinates);
#else
  Set_ST_addr_taken_passed(st_coordinates);
#endif
  wn_parm = WN_CreateParm(Pointer_type, wn_coordinate, ty_i8_ptr, 
    WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(wn_coordinate, wn_parm);
  WN_kid(wn_call, 3) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);

  // Insert into the wn_bounds_code block.
  LWN_Insert_Block_Before(wn_bounds_code, WN_first(wn_bounds_code), wn_call);
  return wn_call; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Collapse_Coordinates 
// FUNCTION: Collapses the indexing in the nested doacross loop 'wn_loop'
//   of depth 'tile_count'.  
//-----------------------------------------------------------------------

static void Mp_Collapse_Coordinates(WN* wn_loop, 
				    INT tile_count,
				    ST* st_layout, 
				    ST* st_coordinates) 
{ 
  STACK<WN*> dep_stack(&LNO_local_pool); 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  WN* wn_bounds_code = WN_CreateBlock();
  Mp_Coordinate_Copy_Out_Coordinates(wn_loop, tile_count, st_coordinates, 
    wn_bounds_code, &dep_stack); 
  WN* wn_call = Mp_Coordinate_Call(wn_loop, tile_count, st_layout, 
    st_coordinates, wn_bounds_code); 
  Mp_Fix_Ref_Array_Aliases(wn_call, LWN_Get_Parent(wn_loop), st_layout); 
  Mp_Fix_Ref_Array_Aliases(wn_call, wn_bounds_code, st_coordinates); 
  Mp_Insert_Bounds_Code(wn_loop, wn_bounds_code, TRUE); 
  Mp_Fix_Deps(&dep_stack); 
} 

//-----------------------------------------------------------------------
// NAME: Direction_Union 
// FUNCTION: Returns the union of directions 'dir1' and 'dir2'.  
//-----------------------------------------------------------------------

static DIRECTION Direction_Union(DIRECTION dir1, 
				 DIRECTION dir2)
{
  DEP dep1 = DEP_SetDirection(dir1);  
  DEP dep_union = DEP_UnionDirection(dep1, dir2); 
  return DEP_Direction(dep_union);  
}
 
//-----------------------------------------------------------------------
// NAME: Depv_Collapse 
// FUNCTION: Returns a DEPV* with 'num_dim' components, which a collapsed
//   version of 'dv' in which the 'tile_count' components starting at 
//   'start_index' have been collapsed into a single component at 
//   'start_index'. 
//-----------------------------------------------------------------------

static DEPV* Depv_Collapse(DEPV* dv, 
			   INT num_dim,   
			   INT start_index, 
			   INT tile_count, 
			   MEM_POOL* pool)
{
  FmtAssert(start_index >= 0 && start_index <= num_dim - 1, 
    ("Bad indexing of DEPV in tile loop collapse"));
  FmtAssert(tile_count >= 0 && start_index + tile_count - 1 <= num_dim - 1, 
    ("Bad indexing of DEPV in tile loop collapse"));
  DIRECTION dir_new; 
  BOOL dir_new_defined = FALSE; 
  INT i;
  for (i = start_index; i <= start_index + tile_count - 1; i++) { 
    DIRECTION dir = DEP_Direction(DEPV_Dep(dv, i));
    if (dir & DIR_POS) {
      if (dir_new_defined) { 
        dir_new = Direction_Union(dir_new, DIR_POS);
      } else { 
	dir_new = DIR_POS;
	dir_new_defined = TRUE; 
      } 
    } 
    if (dir & DIR_NEG) {
      if (dir_new_defined) { 
        dir_new = Direction_Union(dir_new, DIR_NEG);  
      } else { 
	dir_new = DIR_NEG; 
	dir_new_defined = TRUE;
      } 
    } 
    if (!(dir & DIR_EQ))
      break; 
  } 
  if (i == start_index + tile_count) { 
    if (dir_new_defined) { 
      dir_new = Direction_Union(dir_new, DIR_EQ);
    } else { 
      dir_new = DIR_EQ; 
    } 
  } 
  DEPV* dv_new = DEPV_Create(pool, num_dim); 
  for (i = 0; i < start_index; i++) 
    DEPV_Dep(dv_new, i) = DEPV_Dep(dv, i);
  DEPV_Dep(dv_new, start_index) = DEP_SetDirection(dir_new);
  for (i = start_index + 1; i < num_dim; i++) 
    DEPV_Dep(dv_new, i) = DEPV_Dep(dv, i + tile_count - 1);  
  return dv_new;
} 
	   
//-----------------------------------------------------------------------
// NAME: Depv_Already_On_List 
// FUNCTION: Returns TRUE if 'dv_new' is already on the list 'dvl_old', 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Depv_Already_On_List(DEPV_LIST* dvl_old, 
				 DEPV* dv_new)
{
  DEPV_ITER iter(dvl_old);
  for (DEPV_NODE* node=iter.First(); !iter.Is_Empty(); node=iter.Next()) {
    DEPV *Depv = node->Depv;
    INT i;
    for (i = 0; i < dvl_old->Num_Dim(); i++) 
      if (DEPV_Dep(dv_new, i) != DEPV_Dep(Depv, i))
        break; 
    if (i == dvl_old->Num_Dim())
      return TRUE; 
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Collapse_Dependences 
// FUNCTION: Collapses the dependences inside the tiled nested doacross,
//   which currently has 'tile_count' tile loops, 'wn_loop' being the 
//   outermost of these. 
//-----------------------------------------------------------------------

static void Mp_Collapse_Dependences(WN* wn_loop, 
				    INT tile_count)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT local_count = 0; 
  INT bad_mem_count = 0; 
  for (WN* wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    if (++local_count > tile_count)
      break;
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    if (dli->Has_Bad_Mem)
      bad_mem_count++;
  }
  FmtAssert(bad_mem_count == 0 || bad_mem_count == tile_count, 
    ("Should either be no or all bad mem loops."));
  LWN_ITER* iter_first = LWN_WALK_TreeIter(WN_do_body(wn_loop)); 
  for (LWN_ITER *iter = iter_first; iter; iter = LWN_WALK_TreeNext(iter)) {
    WN* wn = iter->wn; 
    OPCODE op = WN_opcode(wn);
    if (!OPCODE_is_load(op) && !OPCODE_is_store(op) && !OPCODE_is_call(op))
      continue;
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0)
      continue;
    EINDEX16 e_next = 0; 
    for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = e_next) {
      e_next = dg->Get_Next_Out_Edge(e);
      VINDEX16 vSink = dg->Get_Sink(e);
      WN* wnSink = dg->Get_Wn(vSink);
      if (!Wn_Is_Inside(wnSink, wn_loop))
        continue;
      DEPV_ARRAY* dva = dg->Depv_Array(e);
      if (bad_mem_count == tile_count) { 
 	dva->Remove_Unused_Dim(tile_count - 1); 	
      } else { 
	DEPV_LIST dl_out(dva->Num_Dim() - tile_count + 1, 
	  dva->Num_Unused_Dim(), &LNO_local_pool);
	for (INT i = 0; i < dva->Num_Vec(); i++) {
	  DEPV* dv_old = dva->Depv(i);
	  INT start_index = Do_Depth(wn_loop) - dva->Num_Unused_Dim(); 
	  DEPV* dv_new = Depv_Collapse(dv_old, dva->Num_Dim() - tile_count + 1, 
	    start_index, tile_count, &LNO_local_pool); 
	  if (!Depv_Already_On_List(&dl_out, dv_new))
	    dl_out.Append(CXX_NEW(DEPV_NODE(dv_new), &LNO_local_pool));
        }
	DEPV_ARRAY* dva_new = Create_DEPV_ARRAY(&dl_out, dg->Pool());  
	Delete_DEPV_ARRAY(dva, dg->Pool());
	dg->Set_Depv_Array(e, dva_new);
      } 
    } 
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Collapse_Loop_Statements 
// FUNCTION: Make all loop statements pointing to the 'tile_count' loops
//   starting at 'wn_loop' point to 'wn_loop' (since the other tile loops 
//   will be collapsed into 'wn_loop'.)  
//-----------------------------------------------------------------------

static void Mp_Collapse_Loop_Statements(WN* wn_loop, 
				        INT tile_count) 
{
  DU_MANAGER* du = Du_Mgr; 
  WN* wn_inner_tile_loop = SNL_Get_Inner_Snl_Loop(wn_loop, tile_count);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner_tile_loop, &stack);
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    OPERATOR opr = WN_operator(wn);
    // Only LDIDs have loop statements. 
    if (opr != OPR_LDID) 
      continue; 
    // LDIDs under OPC_IO_ITEMs do not have loop statements. 
    WN* wnn = 0;
    for (wnn = wn; wnn != NULL; wnn = LWN_Get_Parent(wnn)) 
      if (WN_opcode(wnn) == OPC_IO_ITEM)
	break; 
    if (wnn != NULL)
      continue; 
    WN* loop_stmt = du->Ud_Get_Def(wn)->Loop_stmt();
    for (INT i = 1; i < stack.Elements(); i++) {
      if (loop_stmt == stack.Bottom_nth(i)) {
	du->Ud_Get_Def(wn)->Set_loop_stmt(wn_loop); 
	break;
      }
    }
  }
} 

//-----------------------------------------------------------------------
// NAME: Remove_Dead_Single_Defs 
// FUNCTION: Remove all statements which define LDIDs in 'wn_exp' uniquely,
//   and do not define anything else.  
//-----------------------------------------------------------------------

static void Remove_Dead_Single_Defs(WN* wn_exp)
{
  DU_MANAGER* du = Du_Mgr;   
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_exp);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_LDID) {
      DEF_LIST* def_list = du->Ud_Get_Def(wn);
      if (def_list == NULL)
	continue; 
      if (def_list->Incomplete())
	continue;
      WN* wn_single_def = NULL;
      const DU_NODE* node = NULL;
      INT i = 0;
      DEF_LIST_ITER iter1(def_list);
      for (node = iter1.First(); !iter1.Is_Empty(); i++, node = iter1.Next()) {
        WN* def = node->Wn();
        if (i == 0) {
	  wn_single_def = def; 
	} else {
	  wn_single_def = NULL;
	  break;
	}
      }
      if (wn_single_def == NULL)
	continue; 
      node = NULL; 
      i = 0;
      USE_LIST* use_list = du->Du_Get_Use(wn_single_def);
      if (use_list == NULL)
	continue; 
      if (use_list->Incomplete())
	continue;
      USE_LIST_ITER iter2(use_list);
      for (node = iter2.First(); !iter2.Is_Empty(); i++, node = iter2.Next()) {
        if (i > 0) {
          wn_single_def = NULL;
          break;
        }
      } 
      if (wn_single_def == NULL)
	continue;  
      du->Delete_Def_Use(wn_single_def, wn); 
      LWN_Delete_Tree(wn_single_def);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Find_Lego_Base_Exp 
// FUNCTION: Returns the first node encountered in a walk of 'wn_exp' 
//   which is a OPR_MPY, OPR_LDID, or OPR_ILOAD. 
// NOTE: By design, this will be either the number of processors in a 
//   dimension of the processor grid, or a product of the number of 
//   processors in several dimensions of the processor grid. 
//-----------------------------------------------------------------------

static WN* Find_Lego_Base_Exp(WN* wn_exp)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_exp); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    OPERATOR opr = WN_operator(wn);
    if (opr == OPR_MPY || opr == OPR_LDID || opr == OPR_ILOAD)
      return wn;
  }
  FmtAssert(FALSE, ("Could not find base of lego expression"));
  return NULL; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Collapse_Loop_Heads 
// FUNCTION: Remove all but one of the 'tile_count' tiled loops created for 
//   the nested doacross loop 'wn_outer_loop', namely 'wn_outer_loop' 
//   itself.  
//-----------------------------------------------------------------------

static void Mp_Collapse_Loop_Heads(WN* wn_outer_loop, 
			           INT tile_count, 
				   INT tiling_depth[],
			           BOOL negative_stride) 
{
  DU_MANAGER* du = Du_Mgr;
  INT nloops = 0; 
  INT i;
  for (i = 0; i < tile_count; i++)  
    nloops += tiling_depth[i]; 
  WN* wn_inner_tile_loop = SNL_Get_Inner_Snl_Loop(wn_outer_loop, tile_count);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner_tile_loop, &stack);
  for (i = 0; i < stack.Elements(); i++) 
    if (stack.Bottom_nth(i) == wn_outer_loop) 
      break;
  DO_LOOP_INFO* dli_outer_loop = Get_Do_Loop_Info(wn_outer_loop); 
  for (i++; i < stack.Elements(); i++) {
    WN* wn_loop = stack.Bottom_nth(i); 
    WN* wnn = NULL; 
    for (WN* wn = WN_first(WN_do_body(wn_loop)); wn != NULL; wn = wnn) {
      wnn = WN_next(wn); 
      LWN_Extract_From_Block(wn);
      LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, wn); 
    }
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    if (dli->Is_Outer_Lego_Tile) {
      WN* wn_old_root = negative_stride ? WN_kid0(WN_start(wn_outer_loop))
	: UBexp(WN_end(wn_outer_loop));
      WN* wn_old_base = Find_Lego_Base_Exp(wn_old_root);
      WN* wn_parent = LWN_Get_Parent(wn_old_base);
      WN* wn_new_root = negative_stride ? WN_kid0(WN_start(wn_loop)) 
	: UBexp(WN_end(wn_loop)); 
      WN* wn_new_base = Find_Lego_Base_Exp(wn_new_root); 
      WN* wn_factor = LWN_Copy_Tree(wn_new_base); 
      LWN_Copy_Def_Use(wn_new_base, wn_factor, du);
      WN* wn_mul = AWN_Mpy(Promote_Type(Do_Wtype(wn_outer_loop)), 
	wn_factor, wn_old_base); 
      WN_kid0(wn_parent) = wn_mul; 
      LWN_Parentize(wn_parent); 
    }
    Remove_Dead_Single_Defs(WN_end(wn_loop));
    LWN_Extract_From_Block(wn_loop);
    LWN_Delete_Tree(wn_loop);
    dli_outer_loop->Lego_Mp_Key_Upper++; 
  }
  wn_inner_tile_loop = SNL_Get_Inner_Snl_Loop(wn_outer_loop, 
    nloops - (tile_count - 1));
  DOLOOP_STACK depth_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner_tile_loop, &depth_stack);
  for (i = 0; i < stack.Elements(); i++)
    if (depth_stack.Bottom_nth(i) == wn_outer_loop)
      break;
  for (i++; i < depth_stack.Elements(); i++) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(depth_stack.Bottom_nth(i));
    dli->Depth -= tile_count - 1;
  }
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_inner_tile_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      dli->Depth -= tile_count - 1;
    }
  } 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer_loop); 
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_outer_loop));
  WN* wn_first = WN_first(WN_region_pragmas(wn_region));
  FmtAssert(wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA, 
    ("Mp_Collapse_Loop_Heads: Cannot find doacross/pdo pragma")); 
  FmtAssert(wn_first != NULL && WN_pragma(wn_first) == WN_PRAGMA_DOACROSS
    || WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_DO 
    || WN_pragma(wn_first) == WN_PRAGMA_PDO_BEGIN, 
    ("Mp_Collapse_Loop_Heads: Cannot find doacross/pdo pragma"));
  WN_pragma_arg2(wn_first) = 1;  
  dli_outer->Mp_Info->Set_Nest_Total(1); 
}
 
//-----------------------------------------------------------------------
// NAME: Mp_Collapse_Cleanup 
// FUNCTION: Rebuild access vectors and do other miscellaneous cleanup 
//   for nested doacross loops. 
//-----------------------------------------------------------------------

static void Mp_Collapse_Cleanup(WN* wn_loop) 
{
  // Patch up the nested doacross index variable 
  char Str_Buf[256];  
  DU_MANAGER* du = Du_Mgr; 
  TYPE_ID wtype = WN_desc(WN_start(wn_loop));
  SYMBOL sym_old(WN_st(WN_index(wn_loop)), WN_offset(WN_index(wn_loop)), 
    wtype);
  sprintf(Str_Buf, "$danest%d", WN_map_id(wn_loop));  
  SYMBOL sym_new = Create_Preg_Symbol(Str_Buf, wtype); 
  Replace_Symbol(WN_index(wn_loop), sym_old, sym_new, NULL, NULL); 
  Replace_Symbol(WN_start(wn_loop), sym_old, sym_new, NULL, NULL); 
  Replace_Symbol(WN_end(wn_loop), sym_old, sym_new, NULL, NULL); 
  Replace_Symbol(WN_step(wn_loop), sym_old, sym_new, NULL, NULL); 
  USE_LIST *use_list = du->Du_Get_Use(WN_start(wn_loop));
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_use = node->Wn();
    Replace_Symbol(wn_use, sym_old, sym_new, NULL, NULL);
  } 

  /* Patch up outer loop symbols for inner lego tile loops
  INT loop_count = 0; 
  for (WN* wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    if (dli->Is_Inner_Lego_Tile) {
      if (++loop_count > tile_count)
	break; 
      SYMBOL* sym_local = CXX_NEW(SYMBOL(sym_new), &LNO_default_pool); 
      dli->Lego_Info->Set_Pid0(sym_local); 
    }
  }
  FmtAssert(loop_count == 0 || loop_count == tile_count, 
    ("Incorrect number of inner lego tile loops")); 
  */

  // Rebuild Access Vectors 
  DOLOOP_STACK rebuild_stack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(wn_loop), &rebuild_stack);
  LNO_Build_Access(wn_loop, &rebuild_stack, &LNO_default_pool);
} 

//-----------------------------------------------------------------------
// NAME: Mp_Collapse_Tile_Loops 
// FUNCTION: Collapse the multiple tile loops generated during the tiling 
//   of a nested doacross into a single tile loop.  The outermost tile loop
//   is 'wn_loop' and the number of loops in the nested doacross is  
//   'tile_count'.  
//-----------------------------------------------------------------------

static void Mp_Collapse_Tile_Loops(WN* wn_loop,      
				   INT tile_count,
				   INT tiling_depth[], 
				   ST* st_layout, 
				   BOOL negative_stride) 
{
  char Str_Buf[256]; 
  ST* st_coordinates = NULL; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  sprintf(Str_Buf, "indices%d", WN_map_id(wn_loop));  
  st_coordinates = Create_Local_Array_ST(Str_Buf, Be_Type_Tbl(MTYPE_I8), 
    tile_count);
  Mp_Collapse_Coordinates(wn_loop, tile_count, st_layout, st_coordinates); 
  Mp_Collapse_Dependences(wn_loop, tile_count);
  Mp_Collapse_Loop_Statements(wn_loop, tile_count);
  Mp_Collapse_Loop_Heads(wn_loop, tile_count, tiling_depth, negative_stride); 
  Mp_Collapse_Cleanup(wn_loop); 
}

//-----------------------------------------------------------------------
// NAME: Mp_Nested_Last_Thread
// FUNCTION: Update the last thread information for the nested doacross 
//   loop 'wn_outer_loop' of depth 'tile_count'. 
//-----------------------------------------------------------------------

static void Mp_Nested_Last_Thread(WN* wn_outer_loop, 
				  INT tile_count,
				  INT tiling_depth[], 
				  DU_MANAGER* du)
{
  char Str_Buf[256];
  INT nloops = 0; 
  for (INT i = 0; i < tile_count; i++)
    nloops += tiling_depth[i]; 
  INT64 linenum = WN_Get_Linenum(wn_outer_loop);
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_outer_loop));
  WN* wn_first = WN_first(WN_region_pragmas(wn_region)); 
  WN* wnn = NULL; 
  WN* wn_exp = NULL; 
  WN* wn_lastthread = NULL; 
  OPCODE opand = OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V);
  for (WN* wn = wn_first; wn != NULL; wn = wnn) {
    wnn = WN_next(wn); 
    if (WN_opcode(wn) == OPC_PRAGMA && WN_pragma(wn) == WN_PRAGMA_LASTTHREAD) {
      SYMBOL sym_lastthread(WN_st(wn), WN_pragma_arg1(wn), MTYPE_I4); 
      if (wn_exp == NULL)
        wn_exp = wn_lastthread;   
      wn_lastthread = AWN_LdidSym(&sym_lastthread);  
      WN* wn_stid = Find_Node(sym_lastthread, wn_outer_loop); 
      if (WN_operator(wn_stid) == OPR_LDA)
        wn_stid = LWN_Get_Parent(wn_stid); 
      du->Add_Def_Use(wn_stid, wn_lastthread); 
      Copy_alias_info(Alias_Mgr, wn_stid, wn_lastthread); 
      if (wn_exp != NULL) 
        wn_exp = LWN_CreateExp2(opand, wn_exp, wn_lastthread); 
      LWN_Extract_From_Block(wn);
      LWN_Delete_Tree(wn); 
    }
  }
  FmtAssert(wn_exp != NULL, ("Didn't find any LASTTHREAD pragmas")); 
  sprintf(Str_Buf, "$da_is_last%d", WN_map_id(wn_outer_loop));
  SYMBOL* is_last = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I4)),
    &LNO_default_pool);
  Add_Pragma_To_MP_Region(wn_outer_loop, is_last->St(), 
    is_last->WN_Offset(), WN_PRAGMA_LASTTHREAD);
  Add_Pragma_To_MP_Region(wn_outer_loop, is_last->St(), 
    is_last->WN_Offset(), WN_PRAGMA_LOCAL);
  wn_exp = AWN_StidIntoSym(is_last, wn_exp); 
  Create_local_alias(Alias_Mgr, wn_exp);
  WN* wn_inner_loop = 
     SNL_Get_Inner_Snl_Loop(wn_outer_loop, nloops - (tile_count - 1));  
  LWN_Insert_Block_Before(WN_do_body(wn_inner_loop), 
    WN_first(WN_do_body(wn_inner_loop)), wn_exp);
  Hoist_Statement(wn_exp, Hoistable_Statement(wn_exp, du));
  WN_Set_Linenum(wn_exp, linenum);
  du->Add_Def_Use(wn_exp, Return_Node(Current_Func_Node));
} 

//-----------------------------------------------------------------------
// NAME: Mp_Remove_Onto_Pragmas 
// FUNCTION: Remove the ONTO pragmas from the region associated with 
//   'wn_loop'. 
//-----------------------------------------------------------------------

static void Mp_Remove_Onto_Pragmas(WN* wn_loop)
{
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop)); 
  WN* wn_first = WN_first(WN_region_pragmas(wn_region));
  WN* wnn = NULL; 
  for (WN* wn = wn_first; wn != NULL; wn = wnn) { 
    wnn = WN_next(wn); 
    if (WN_opcode(wn) == OPC_XPRAGMA && WN_pragma(wn) == WN_PRAGMA_ONTO) {
      LWN_Extract_From_Block(wn); 
      LWN_Delete_Tree(wn);
    } 
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Tiling_Depth 
// FUNCTION: Returns the number of loops in a tile when the loop 'wn_loop'
//   is tiled according to its LEGO_INFO.  
//-----------------------------------------------------------------------

static INT Lego_Tiling_Depth(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  LEGO_INFO* lego_info = dli->Lego_Info;
  if (lego_info->Dynamic_Affinity())
    return 3; 
  SYMBOL *array_sym = lego_info->Array();
  FmtAssert(array_sym != NULL, ("No array symbol on lego loop"));
  DISTR_ARRAY *dact = Lookup_DACT(array_sym->St());
  INT curr_dim = lego_info->Dim_Num();
  switch (dact->Get_Dim(curr_dim)->Distr_Type()) {
  case DISTRIBUTE_BLOCK:
    return 2; 
  case DISTRIBUTE_CYCLIC_CONST:
    return dact->Get_Dim(curr_dim)->Chunk_Const_Val() == 1 ? 2 : 3;
  case DISTRIBUTE_CYCLIC_EXPR:
    return 3; 
  default:
    FmtAssert(FALSE, ("Do not currently handle this sched type"));
    return -1; 
  }
}
    
//-----------------------------------------------------------------------
// NAME: Mp_Tiling_Depth 
// FUNCTION: Returns the number of loops in a tile when the loop 'wn_loop'
//   is tiled with the sched type in its MP_INFO. 
//-----------------------------------------------------------------------

static INT Mp_Tiling_Depth(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  if (dli->Lego_Info != NULL) 
    return Lego_Tiling_Depth(wn_loop); 
  switch (dli->Mp_Info->Sched_Type()) {
  case MP_SCHED_SIMPLE: 
    return 2;
  case MP_SCHED_INTERLEAVE:
    return 3;
  case MP_SCHED_DYNAMIC:
  case MP_SCHED_GSS:
  case MP_SCHED_RUNTIME:
  default: 
    FmtAssert(FALSE, ("Mp lowerer should handle these sched types."));
    return -1; 
  }
}

//-----------------------------------------------------------------------
// NAME: Repair_Bad_Dependences 
// FUNCTION: Repair the lexicographically negative dependences incident on
//   nodes inside the loop 'wn_loop' by recomputing them. 
//-----------------------------------------------------------------------

extern void Repair_Bad_Dependences(WN* wn_loop) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  LS_IN_LOOP* loop_ls = CXX_NEW(LS_IN_LOOP(wn_loop, dg, &LNO_local_pool, TRUE),
    &LNO_local_pool);
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0) 
      continue;
    DOLOOP_STACK wn_stack(&LNO_local_pool);
    Build_Doloop_Stack(wn, &wn_stack);
    EINDEX16 e = 0;
    DOLOOP_STACK source_stack(&LNO_local_pool);
    STACK<WN*> stack_of_source_wn(&LNO_local_pool);
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      if (Is_Lexpos(dg->Depv_Array(e)))
	continue; 
      WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
      stack_of_source_wn.Push(wn_source);
    }
    INT i;
    for (i=0; i<stack_of_source_wn.Elements(); i++) {
      VINDEX16 v_source=dg->Get_Vertex(stack_of_source_wn.Top_nth(i));
      e=dg->Get_Edge(v_source,v);
      dg->Delete_Array_Edge(e);   
      e=dg->Get_Edge(v,v_source);
      if (e != 0)
	dg->Delete_Array_Edge(e);
    }
    for (i=0; i<stack_of_source_wn.Elements(); i++) {
      WN* wn_source=stack_of_source_wn.Top_nth(i);
      Build_Doloop_Stack(wn_source, &source_stack);
      if (!dg->Add_Edge(wn_source, &source_stack, wn,
	&wn_stack, loop_ls->In(wn_source) < loop_ls->In(wn)))
	LNO_Erase_Dg_From_Here_In(wn, dg);
      source_stack.Clear();
    }
    DOLOOP_STACK sink_stack(&LNO_local_pool);
    STACK<WN*> stack_of_sink_wn(&LNO_local_pool);
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      if (Is_Lexpos(dg->Depv_Array(e)))
	continue; 
      WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
      stack_of_sink_wn.Push(wn_sink);
    }
    for (i=0; i<stack_of_sink_wn.Elements(); i++) {
      VINDEX16 v_sink=dg->Get_Vertex(stack_of_sink_wn.Top_nth(i));
      e=dg->Get_Edge(v,v_sink);
      dg->Delete_Array_Edge(e);
      e=dg->Get_Edge(v_sink,v);
      if (e != 0)
	dg->Delete_Array_Edge(e);
    }
    for (i=0; i<stack_of_sink_wn.Elements(); i++) {
      WN* wn_sink=stack_of_sink_wn.Top_nth(i);
      Build_Doloop_Stack(wn_sink, &sink_stack);
      if (!dg->Add_Edge(wn, &wn_stack, wn_sink,
	&sink_stack, loop_ls->In(wn) < loop_ls->In(wn_sink)))
	LNO_Erase_Dg_From_Here_In(wn, dg);
      sink_stack.Clear();
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Permute_Nested_Loops 
// FUNCTION: Permute the tiled nested doacross loop 'wn_outer_loop' so that 
//   the outer tile loops are outside innermore tile loops.  The number of 
//   loop in the original nested doacross is 'tile_count'.  Each of these 
//   loops is tiled into 'tiling_depth' loops.   
//-----------------------------------------------------------------------

static WN* Mp_Permute_Nested_Loops(WN* wn_outer_loop,
				   INT tile_count, 
				   INT tiling_depth[]) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;

  INT* first_loops = CXX_NEW_ARRAY(INT, tile_count, &LNO_local_pool); 
  INT nloops = 0; 
  INT i;
  for (i = 0; i < tile_count; i++) {
    first_loops[i] = nloops; 
    nloops += tiling_depth[i]; 
  }
  INT* permutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);  
  INT max_tiling_depth = 0;
  for (i = 0; i < tile_count; i++)
    if (tiling_depth[i] > max_tiling_depth)
      max_tiling_depth = tiling_depth[i];  
  INT pindex = 0; 
  for (i = 0; i < max_tiling_depth - 1; i++)
    for (INT j = 0; j < tile_count; j++)
      if (i < tiling_depth[j] - 1)   
        permutation[pindex++] = first_loops[j] + i; 
  for (i = 0; i < tile_count; i++)
    permutation[pindex++] = first_loops[i] + tiling_depth[i] - 1; 
  WN* wn_inner_loop = SNL_Get_Inner_Snl_Loop(wn_outer_loop, nloops); 
  if (SNL_Legal_Permutation(wn_outer_loop, wn_inner_loop, 
      permutation, nloops, FALSE)) {
    wn_outer_loop = SNL_INV_Permute_Loops(wn_outer_loop, permutation, 
      nloops, TRUE);
  } else {
    pindex = 0; 
    for (i = 0; i < tile_count; i++)
      permutation[pindex++] = first_loops[i]; 
    for (i = 0; i < tile_count; i++) 
      for (INT j = 1; j < tiling_depth[i]; j++)
        permutation[pindex++] = first_loops[i] + j; 
    wn_outer_loop = SNL_INV_Permute_Loops(wn_outer_loop, permutation, 
      nloops, FALSE);
    Repair_Bad_Dependences(wn_outer_loop); 
  }
  return wn_outer_loop; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Tile_Nested_Loop
// FUNCTION: Processor tile (pseudo-lower) the nested do across loop nest
//   'loop' according to the information in the MP_INFOs of the enclosed 
//   loops.  Return a WN* to the outermost loop in the nest.  
//-----------------------------------------------------------------------

static WN* Mp_Tile_Nested_Loop(WN* loop,
                               MEM_POOL *pool)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DU_MANAGER* du = Du_Mgr;

  ST* st_onto = NULL; 
  ST* st_layout = NULL;  
  WN* wn_new_loop = NULL; 
  WN* wn_outer_loop = NULL; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop); 
  INT tile_count = dli->Mp_Info->Nest_Total(); 
  INT* tiling_depth = CXX_NEW_ARRAY(INT, tile_count, &LNO_local_pool);  
  WN* wn = loop;
  INT i;
  for (i = 0; i < tile_count; i++) {
    tiling_depth[i] = Mp_Tiling_Depth(wn); 
    wn = Find_Next_Innermost_Do(wn); 
  }
  BOOL lego_loop = dli->Lego_Info != NULL; 
  Hoist_And_Sink_For_Nested_Doacross(loop, dg, du);
  if (lego_loop) 
    Lego_Layout_Code(loop, tile_count, &st_layout); 
  else 
    Mp_Layout_Code(loop, tile_count, &st_onto, &st_layout); 
  wn = loop; 
  WN* wnn = NULL; 
  for (i = 0; i < tile_count; i++, wn = wnn) {
    wnn = Find_Next_Innermost_Do(wn);  
    wn_new_loop = Mp_Tile_Single_Loop(wn, FALSE, pool); 
    if (wn_outer_loop == NULL) 
      wn_outer_loop = wn_new_loop; 
  }
  BOOL negative_stride = lego_loop && dli->Lego_Info->Stride() < 0; 
  Mp_Permute_Nested_Loops(wn_outer_loop, tile_count, tiling_depth); 
  Mp_Collapse_Tile_Loops(wn_outer_loop, tile_count, tiling_depth, 
    st_layout, negative_stride); 
  Mp_Nested_Last_Thread(wn_outer_loop, tile_count, tiling_depth, du); 
  Mp_Remove_Onto_Pragmas(wn_outer_loop); 
  return wn_outer_loop; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Extract_Bogus_Do_Across
// FUNCTION: Remove the do_across information from the loop 'wn_loop'
//   because it is nested inside another doacross and it was not part
//   of a nested doacross.
//-----------------------------------------------------------------------

static void Mp_Extract_Bogus_Do_Across(WN* wn_loop)
{
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  LWN_Extract_From_Block(LWN_Get_Parent(wn_loop), wn_loop);
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_region),
    wn_region, wn_loop);
  LWN_Extract_From_Block(LWN_Get_Parent(wn_region), wn_region);
  LWN_Delete_Tree(wn_region);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  CXX_DELETE(dli->Mp_Info, &LNO_default_pool);
  dli->Mp_Info = NULL;
}

//-----------------------------------------------------------------------
// NAME: Mp_Extract_Bogus_Do_Acrosses
// FUNCTION: Remove the do_across information from the do_across loops
//   nested within 'wn_loop', which are not part of a nested doacross.
//-----------------------------------------------------------------------

static void Mp_Extract_Bogus_Do_Acrosses(WN* wn_loop)
{
  FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP,
    ("Root doacross is not a DO loop.\n"));
  DO_LOOP_INFO* dli_root = Get_Do_Loop_Info(wn_loop);
  FmtAssert(dli_root->Mp_Info != NULL, ("Root doacross is not an MP loop.\n"));
  if (dli_root->Mp_Info->Nest_Index() != 0)
    return;
  DOLOOP_STACK stack_doacross(&LNO_local_pool);
  stack_doacross.Clear();
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
      if (dli->Mp_Info != NULL && dli->Depth > dli_root->Depth
        + dli->Mp_Info->Nest_Index())
        stack_doacross.Push(wn);
    }
  }
  for (INT i = stack_doacross.Elements() - 1; i >= 0; i--)
    Mp_Extract_Bogus_Do_Across(stack_doacross.Bottom_nth(i));
}

//-----------------------------------------------------------------------
// NAME: Standardize_For_Tiling 
// FUNCTION: Standardizes the loop 'wn_loop' for tiling.  Returns TRUE 
//   if it was possible to do this, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Standardize_For_Tiling(WN* wn_loop) 
{
  BOOL return_value = TRUE; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  if (dli->Mp_Info != NULL) {
    INT loop_count = 0; 
    INT total_count = dli->Mp_Info->Nest_Total(); 
    for (WN* wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      FmtAssert(loop_count == dli->Mp_Info->Nest_Index(), 
	("Bad indexing within nested doacross"));
      FmtAssert(total_count == dli->Mp_Info->Nest_Total(), 
	("Bad indexing within nested doacross"));
      WN* wn_step = Loop_Step(wn); 
      if (WN_operator(wn_step) != OPR_INTCONST
	|| WN_const_val(wn_step) != 1) 
	return_value = FALSE; 
      else if (!Upper_Bound_Standardize(WN_end(wn), TRUE)) 
	return_value = FALSE; 
      if (++loop_count >= total_count)
	break;
    }
  } else {
    WN* wn_step = Loop_Step(wn_loop);
    if (WN_operator(wn_step) != OPR_INTCONST
      || WN_const_val(wn_step) != 1)
      return_value = FALSE;
    else if (!Upper_Bound_Standardize(WN_end(wn_loop)))
      return_value = FALSE;
  }
  return return_value; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Convert_To_Single_Loop 
// FUNCTION: Convert the nested doacross loop into a single doacross loop.
//-----------------------------------------------------------------------

static void Mp_Convert_To_Single_Loop(WN* wn_loop)
{
  WN* wn = 0;
  for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_REGION)
      break;
  FmtAssert(wn != NULL, ("Could not find region"));
  WN* wn_first = WN_first(WN_region_pragmas(wn)); 
  WN* wnn = NULL; 
  for (wn = wn_first; wn != NULL; wn = wnn) {
    wnn = WN_next(wn); 
    if (WN_opcode(wn) == OPC_XPRAGMA && WN_pragma(wn) == WN_PRAGMA_ONTO) {
      LWN_Extract_From_Block(wn); 
      LWN_Delete_Tree(wn); 
    }
  }
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  INT loop_count = 0;
  INT total_count = dli->Mp_Info->Nest_Total(); 
  for (wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    FmtAssert(loop_count == dli->Mp_Info->Nest_Index(),
      ("Bad indexing within nested doacross"));
    FmtAssert(total_count == dli->Mp_Info->Nest_Total(), 
      ("Bad indexing within nested doacross"));
    CXX_DELETE(dli->Lego_Info, LEGO_pool);
    dli->Lego_Info = NULL;
    if (wn != wn_loop) {
      CXX_DELETE(dli->Mp_Info, &LNO_default_pool);
      dli->Mp_Info = NULL;
    }
    if (++loop_count >= total_count)
      break;
  }  
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  wn_first = WN_first(WN_region_pragmas(wn_region));
  FmtAssert(wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA,
    ("Mp_Collapse_Loop_Heads: Cannot find doacross/pdo pragma")); 
  FmtAssert(wn_first != NULL && WN_pragma(wn_first) == WN_PRAGMA_DOACROSS
    || WN_pragma(wn_first) == WN_PRAGMA_PDO_BEGIN, 
    ("Mp_Collapse_Loop_Heads: Cannot find doacross/pdo pragma"));
  WN_pragma_arg2(wn_first) = 1; 
  dli->Mp_Info->Set_Nest_Total(1);
}

//-----------------------------------------------------------------------
// NAME: SNL_Legal_Tile_Scalars
// FUNCTION: Returns TRUE if the scalars in 'wn_loop' do not inhibit 
//   arbitrary cyclic and block cyclic tiling.  Returns FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL SNL_Legal_Tile_Scalars(WN* wn_loop)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 

  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_STID) {
      if (dg->Get_Vertex(wn))
        return FALSE;
      for (WN* wn_tp = wn; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp))
        if (WN_opcode(wn_tp) == OPC_DO_LOOP
            && SYMBOL(wn) == SYMBOL(WN_index(wn_tp)))
          return TRUE;
      USE_LIST *use_list = du->Du_Get_Use(wn);
      if (use_list == NULL)
        return TRUE;
      if (use_list->Incomplete())
        return FALSE;
      USE_LIST_ITER iter(use_list);
      const DU_NODE* node = NULL;
      for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
        WN* wn_use = node->Wn();
        WN* wn = NULL;
        for (wn = wn_use; wn != NULL; wn = LWN_Get_Parent(wn))
          if (wn == wn_loop)
            break;
        if (wn == NULL)
          return FALSE;
      }
      return TRUE;
    } else if (WN_operator(wn) == OPR_LDID) {
      if (dg->Get_Vertex(wn))
        return FALSE;
      for (WN* wn_tp = wn; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp))
        if (WN_opcode(wn_tp) == OPC_DO_LOOP
            && SYMBOL(wn) == SYMBOL(WN_index(wn_tp)))
          return TRUE;
      DEF_LIST *def_list = du->Ud_Get_Def(wn);
      if (def_list == NULL)
        continue;
      if (def_list->Incomplete())
        return FALSE;
      WN* wn_loop_stmt = def_list->Loop_stmt();
      if (wn_loop_stmt == wn_loop)
        return FALSE;
      return TRUE;
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: SNL_Depv_Is_LCD
// FUNCTION: Returns TRUE if the 'array_index'th dependence in 'depv_array'
//   is carried by 'wn_loop', FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL SNL_Depv_Is_LCD(DEPV_ARRAY* depv_array,
                            INT array_index,
                            WN* wn_loop)
{
  DEPV* depv = depv_array->Depv(array_index);
  INT loop_depth = Do_Loop_Depth(wn_loop);
  loop_depth -= depv_array->Num_Unused_Dim();
  for (INT i = 0; i < loop_depth; i++) {
    DIRECTION dir = DEP_Direction(DEPV_Dep(depv, i));
    if (dir == DIR_POS)
      return FALSE;
  }
  DIRECTION dir = DEP_Direction(DEPV_Dep(depv, loop_depth));
  return (dir != DIR_EQ);
}


//-----------------------------------------------------------------------
// NAME: SNL_Legal_Tile_Arrays
// FUNCTION: Returns TRUE if the array references in 'wn_loop' do not
//   inhibit arbitrary cyclic and block cyclic tiling.  Returns FALSE
//   otherwise.
//-----------------------------------------------------------------------

static BOOL SNL_Legal_Tile_Arrays(WN* wn_loop,
                                  HASH_TABLE<EINDEX16,INT>* edge_table)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    OPERATOR opr = WN_operator(wn);
    if (opr == OPR_ILOAD || opr == OPR_ISTORE
        || opr == OPR_LDID || opr == OPR_STID) {
      VINDEX16 v = dg->Get_Vertex(wn);
      if (v == 0 && (opr == OPR_LDID || opr == OPR_STID))
        continue;
      EINDEX16 e = 0;
      for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
        if (edge_table->Find(e))
          continue;
        edge_table->Enter(e, 1);
        DEPV_ARRAY* depv_array = dg->Depv_Array(e);
        for (INT i = 0; i < depv_array->Num_Vec(); i++)
          if (SNL_Depv_Is_LCD(depv_array, i, wn_loop))
            return FALSE;
      }
      for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
        if (edge_table->Find(e))
          continue;
        edge_table->Enter(e, 1);
        DEPV_ARRAY* depv_array = dg->Depv_Array(e);
        for (INT i = 0; i < depv_array->Num_Vec(); i++)
          if (SNL_Depv_Is_LCD(depv_array, i, wn_loop))
            return FALSE;
      }
    }
  }
  return TRUE;
}


//-----------------------------------------------------------------------
// NAME: Lego_Tile_Legal
// FUNCTION: Returns TRUE if 'wn_loop' can be lego-tiled according to
//   the stated distribution in its LEGO_INFO, FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Lego_Tile_Legal(WN* wn_loop)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  LEGO_INFO* lego_info = dli->Lego_Info;
  SYMBOL *array_sym = lego_info->Array();
  if (array_sym == NULL)
    return FALSE;
  INT hash_table_size = MIN(dg->Get_Edge_Count(), 512);
  HASH_TABLE<EINDEX16,INT> edge_table(hash_table_size, &LNO_local_pool);
  if (lego_info->Dynamic_Affinity()) {
    if (dli->Has_Bad_Mem)
      return FALSE;
    if (!SNL_Legal_Tile_Scalars(wn_loop))
      return FALSE;
    if (!SNL_Legal_Tile_Arrays(wn_loop, &edge_table))
      return FALSE;
    return TRUE;
  }
  DISTR_ARRAY *dact = Lookup_DACT(array_sym->St());
  INT curr_dim = lego_info->Dim_Num();
  switch (dact->Get_Dim(curr_dim)->Distr_Type()) {
  case DISTRIBUTE_BLOCK:
    return TRUE;
  case DISTRIBUTE_CYCLIC_CONST:
  case DISTRIBUTE_CYCLIC_EXPR:
    {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
      if (dli->Has_Bad_Mem)
        return FALSE;
      if (!SNL_Legal_Tile_Scalars(wn_loop))
        return FALSE;
      if (!SNL_Legal_Tile_Arrays(wn_loop, &edge_table))
        return FALSE;
      return TRUE;
    }
  default:
    return FALSE;
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Tile_Loop 
// FUNCTION: If legal, tile the 'wn_loop' according to the information in 
//   that loop's LEGO_INFO. 
//-----------------------------------------------------------------------

static WN* Lego_Tile_Loop(WN* wn_loop, 
                          MEM_POOL *pool)
{
  if (!Standardize_For_Tiling(wn_loop) || !Lego_Tile_Legal(wn_loop)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    CXX_DELETE(dli->Lego_Info, LEGO_pool);
    dli->Lego_Info = NULL;
    return wn_loop;
  } 
  WN* wn_return = Lego_Tile_Single_Loop(wn_loop, pool);
  return wn_return; 
}

//-----------------------------------------------------------------------
// NAME: Mp_Want_Freeze_Threads
// FUNCTION: Returns TRUE if we want to freeze the number of threads used
//   to execute a parallel loop before the loop, FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Mp_Want_Freeze_Threads(WN* wn_loop,
                                   BOOL LNO_Ozero)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  return !dli->Mp_Info->Is_Pdo() && dli->Lego_Info == NULL
    && !dli->Mp_Info->Plower_Disabled() 
    && (LNO_Pseudo_Lower || dli->Mp_Info->Nest_Total() > 1)
    && !(LNO_Ozero && dli->Mp_Info->Nest_Total() == 1);
}

//-----------------------------------------------------------------------
// NAME: Mp_Want_Freeze_Cur_Threads
// FUNCTION: Returns TRUE if we want to freeze the current number of threads
//   used to execute a PDO 'wn_loop' before the loop, FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Mp_Want_Freeze_Cur_Threads(WN* wn_loop,
                                       BOOL LNO_Ozero)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (!LNO_Pseudo_Lower && dli->Mp_Info->Nest_Total() > 1)
    return FALSE;
  if (LNO_Ozero && dli->Mp_Info->Nest_Total() == 1)
    return FALSE;
  if (dli->Lego_Info != NULL)
    return FALSE;
  if (dli->Mp_Info->Plower_Disabled())
    return FALSE;
  if (!dli->Mp_Info->Is_Pdo())
    return FALSE;
  if (Is_Orphaned_Pdo(wn_loop)) {
    if (!Mp_Want_Version_Loop(wn_loop, FALSE))
      return TRUE;
  } else {
    WN* wn = 0;
    for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_operator(wn) == OPR_REGION) {
        WN* wn_first = WN_first(WN_region_pragmas(wn));
        if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA
            && WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN)
          break;
      }
    }
    WN* wn_region = wn;
    if (!Mp_Want_Version_Parallel_Region(wn_region, FALSE))
      return TRUE;
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Mp_Optimize_Interleaved_Loop
// FUNCTION: Optimize the tiled version of an MP interleave scheduled  
//   loop 'wn_loop' applying tile splitting. 
//-----------------------------------------------------------------------

static BOOL Mp_Optimize_Interleaved_Loop(WN* wn_loop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DU_MANAGER* du = Du_Mgr;

  WN* wn_middle = Next_SNL_Loop(wn_loop); 
  if (wn_middle == NULL) 
    return FALSE; 
  WN* wn_inner = Next_SNL_Loop(wn_middle); 
  if (wn_inner == NULL) 
    return FALSE; 
  INT tile_size = 0; 
  WN* wn_outer = SNL_SPL_Loop_Is_Inner_Tile(wn_inner, &tile_size); 
  if (wn_outer == NULL) 
    return FALSE; 
  SNL_SPL_Split_Inner_Tile_Loop(wn_outer, wn_inner, tile_size, "$ispl_", 
    FALSE);
  WN* wn_first = NULL; 
  WN* wn_last = NULL; 
  if (Iterations(wn_inner, &LNO_local_pool) == 1)
     Remove_Unity_Trip_Loop(wn_inner, TRUE, &wn_first, &wn_last, dg, du); 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Mp_Optimize_Interleaved_Loop_Traverse
// FUNCTION: Traverse the tree 'wn_tree' looking for MP interleave scheduled
//   loops to optimize with tile splitting. 
//-----------------------------------------------------------------------

static void Mp_Optimize_Interleaved_Loop_Traverse(WN* wn_tree)
{ 
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) { 
    DO_LOOP_INFO* dli_tree = Get_Do_Loop_Info(wn_tree); 
    if (dli_tree->Mp_Info 
	&& dli_tree->Mp_Info->Sched_Type() == MP_SCHED_INTERLEAVE)
      Mp_Optimize_Interleaved_Loop(wn_tree); 
  } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Mp_Optimize_Interleaved_Loop_Traverse(wn); 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Mp_Optimize_Interleaved_Loop_Traverse(WN_kid(wn_tree, i)); 
  } 
}

//-----------------------------------------------------------------------
// NAME: Mp_Optimize_Interleaved_Loops
// FUNCTION: Optimize all MP interleave scheduled loops in the tree rooted
//   at 'wn_loop'. 
//-----------------------------------------------------------------------

static void Mp_Optimize_Interleaved_Loops(WN* wn_loop)
{
  Mp_Optimize_Interleaved_Loop_Traverse(wn_loop); 
}

//-----------------------------------------------------------------------
// NAME: Mp_Tile_Loop
// FUNCTION: Processor tile (pseudo-lower) the 'wn_loop' according to the
//   information in the MP_INFO.  Memory for data structures created
//   during the tiling is allocated from 'pool'. Set 'LNO_Ozero' to 
//   to TRUE if you are tiling at -O0, at -O3 set it to FALSE.
//-----------------------------------------------------------------------

extern WN* Mp_Tile_Loop(WN* wn_loop, 
                        BOOL LNO_Ozero, 
                        MEM_POOL *pool,
			LMT_VALUE lmt_traverse)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (!Standardize_For_Tiling(wn_loop)) {
    if (dli->Mp_Info->Nest_Total() > 1) {
      Mp_Compress_Nested_Loop(wn_loop); 
      Mp_Convert_To_Single_Loop(wn_loop);
    }
    return wn_loop;
  } 

  // At this point, we beieve that the MP library can support will 
  // handle nested parallelism, even if "handling" means ignoring it 
  // without producing errors. 
  //
  // if (!LNO_Ozero)
  //  Mp_Extract_Bogus_Do_Acrosses(wn_loop);

  WN* wn_return = NULL; 
  INT nest_depth = dli->Mp_Info->Nest_Total(); 
  // modified by csc. 2002/11/14
  // The original MP framework nolonger needed.
  // TODO: desert or replace it 
  BOOL want_freeze_threads = FALSE;
  BOOL want_freeze_cur_threads = FALSE;
//  BOOL want_freeze_threads = Mp_Want_Freeze_Threads(wn_loop, LNO_Ozero);
//  BOOL want_freeze_cur_threads = Mp_Want_Freeze_Cur_Threads(wn_loop,
//    LNO_Ozero);
  if (nest_depth > 1) {
    Mp_Compress_Nested_Loop(wn_loop);
    WN* wn_scalar_loop = Mp_Version_Loop(wn_loop);
    if (wn_scalar_loop != NULL) {
      switch (lmt_traverse) { 
      case LMT_LEGO:
        Lego_Tile_Traverse(wn_scalar_loop, LNO_Ozero); 
	break;
      case LMT_MP:
        Mp_Tile_Traverse(wn_scalar_loop); 
	break;
      case LMT_LEGO_MP:
        Lego_Mp_Tile_Traverse(wn_scalar_loop, LNO_Ozero); 
	break;
      } 
    } 
    if (want_freeze_threads)
      Freeze_Numthreads_Ldid(wn_loop);
    if (want_freeze_cur_threads)
      Freeze_Cur_Numthreads_Func(wn_loop);
    wn_return = Mp_Tile_Nested_Loop(wn_loop, pool);
  } else {
    WN* wn_scalar_loop = Mp_Version_Loop(wn_loop);
    if (wn_scalar_loop != NULL) {
      switch (lmt_traverse) { 
      case LMT_LEGO:
        Lego_Tile_Traverse(wn_scalar_loop, LNO_Ozero); 
	break;
      case LMT_MP:
        Mp_Tile_Traverse(wn_scalar_loop); 
	break;
      case LMT_LEGO_MP:
        Lego_Mp_Tile_Traverse(wn_scalar_loop, LNO_Ozero); 
	break;
      } 
    } 
    if (want_freeze_threads)
      Freeze_Numthreads_Ldid(wn_loop);
    if (want_freeze_cur_threads)
      Freeze_Cur_Numthreads_Func(wn_loop); 
    wn_return = Mp_Tile_Single_Loop(wn_loop, LNO_Ozero, pool); 
  }
  Mp_Optimize_Interleaved_Loops(wn_return); 
  return wn_return;  
}

//-----------------------------------------------------------------------
// NAME: Traverse_Update
// FUNCTION: Return the OPC_BLOCK node at which to resume a traversal 
//   after lego or mp tiling a loop 'wn_loop'.
//-----------------------------------------------------------------------

static WN* Traverse_Update(WN* wn_loop)
{ 
  FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP, 
    ("Traverse_Update: Expecting a do loop"));
  DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
  INT lower = dli_loop->Lego_Mp_Key_Lower; 
  INT upper = dli_loop->Lego_Mp_Key_Upper; 
  if (lower == 0 || upper == 0)
    return WN_do_body(wn_loop);  
  WN* wn_inner_loop = wn_loop; 
  INT nloops = SNL_Loop_Count(wn_loop);
  for (INT i = 1; i <= nloops; i++) { 
    WN* wn = SNL_Get_Inner_Snl_Loop(wn_loop, i);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (dli->Lego_Mp_Key_Lower == 0 || dli->Lego_Mp_Key_Upper == 0) 
      break; 
    if (dli->Lego_Mp_Key_Lower < lower || dli->Lego_Mp_Key_Upper > upper)
      break;
    wn_inner_loop = wn; 
  } 
  return WN_do_body(wn_inner_loop);
} 

//-----------------------------------------------------------------------
// NAME: Lego_Mp_Tile_Traverse
// FUNCTION: Processor tile (pseudo-lower) all of the loops in the tree
//   rooted at 'wn_tree' according to the information in the 'LEGO_INFO'
//   and 'MP_INFO' of each do loop.
//-----------------------------------------------------------------------

static void Lego_Mp_Tile_Traverse(WN* wn_tree, 
			          BOOL LNO_Ozero)
{
  DU_MANAGER* du = Du_Mgr;

  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    WN* wn_return = NULL; 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Mp_Info != NULL) 
      wn_return = Mp_Tile_Loop(wn_tree, LNO_Ozero, 
        &LNO_default_pool, LMT_LEGO_MP);
    else if (dli->Lego_Info != NULL && !dli->Is_Inner_Lego_Tile) 
      wn_return = Lego_Tile_Loop(wn_tree, &LNO_default_pool);
    if (wn_return != NULL) 
      Hoist_Statements(wn_return, du);
    if (wn_return != NULL)
      wn_tree = Traverse_Update(wn_return);
  }

  if (WN_opcode(wn_tree) == OPC_REGION) { 
    WN* wn_first = WN_first(WN_region_pragmas(wn_tree));
    if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA 
        && WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN) {
      WN* wn_scalar_region = Mp_Version_Parallel_Region(wn_tree); 
      if (wn_scalar_region != NULL) 
        Lego_Mp_Tile_Traverse(wn_scalar_region, LNO_Ozero); 
    }
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    WN* wnn = NULL;
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = wnn) {
      wnn = WN_next(wn);
      Lego_Mp_Tile_Traverse(wn, LNO_Ozero);
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Lego_Mp_Tile_Traverse(WN_kid(wn_tree, i), LNO_Ozero);
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Mp_Tile
// FUNCTION: Processor tile (pseudo-lower) all of the loops with LEGO_INFO
//   and/or MP_INFO according to the information in those structures.
//-----------------------------------------------------------------------

extern void Lego_Mp_Tile(WN* wn_root, 
			 BOOL LNO_Ozero)
{
  Lego_Mp_Tile_Traverse(wn_root, LNO_Ozero);
  if (Eliminate_Dead_SCF(wn_root, LWN_Delete_Tree))
    Mark_Code(wn_root, FALSE, FALSE);
}

//-----------------------------------------------------------------------
// NAME: Has_Lego_Mp_Loops
// FUNCTION: Returns TRUE if the tree rooted at 'wn_tree' has at least one
//   do loop with both a Lego_Info and an Mp_Info, returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Has_Lego_Mp_Loops(WN* wn_tree)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_tree);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (dli->Mp_Info != NULL && dli->Lego_Info != NULL)
        return TRUE; 
    }
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Lego_Tile_Traverse
// FUNCTION: Processor tile (pseudo-lower) all of the loops in the tree
//   rooted at 'wn_tree' according to the information in the 'LEGO_INFO'
//   of each do loop.
//-----------------------------------------------------------------------

static void Lego_Tile_Traverse(WN* wn_tree, 
			       BOOL LNO_Ozero)
{
  DU_MANAGER* du = Du_Mgr;

  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    WN* wn_return = NULL; 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Mp_Info != NULL && dli->Lego_Info != NULL) 
      wn_return = Mp_Tile_Loop(wn_tree, LNO_Ozero, 
        &LNO_default_pool, LMT_LEGO);
    else if (dli->Lego_Info != NULL && !dli->Is_Inner_Lego_Tile) 
      wn_return = Lego_Tile_Loop(wn_tree, &LNO_default_pool);
    if (wn_return != NULL) 
      Hoist_Statements(wn_return, du);
    if (wn_return != NULL)
      wn_tree = Traverse_Update(wn_return);
  }

  if (WN_opcode(wn_tree) == OPC_REGION) { 
    WN* wn_first = WN_first(WN_region_pragmas(wn_tree));
    if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA 
	&& WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN 
	&& Has_Lego_Mp_Loops(wn_tree)) {
      WN* wn_scalar_region = Mp_Version_Parallel_Region(wn_tree); 
      if (wn_scalar_region != NULL) 
        Lego_Tile_Traverse(wn_scalar_region, LNO_Ozero); 
    }
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    WN* wnn = NULL;
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = wnn) {
      wnn = WN_next(wn);
      Lego_Tile_Traverse(wn, LNO_Ozero);
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Lego_Tile_Traverse(WN_kid(wn_tree, i), LNO_Ozero);
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Tile
// FUNCTION: Processor tile (pseudo-lower) all of the loops with LEGO_INFO
//   according to the information in those structures.
//-----------------------------------------------------------------------

extern void Lego_Tile(WN* wn_root,
                      BOOL LNO_Ozero)
{
  Lego_Tile_Traverse(wn_root, LNO_Ozero);
  if (Eliminate_Dead_SCF(wn_root, LWN_Delete_Tree))
    Mark_Code(wn_root, FALSE, FALSE);
}

//-----------------------------------------------------------------------
// NAME: Auto_Mp_Tile_Traverse
// FUNCTION: Processor tile (pseudo-lower) all of the loops in the tree
//   rooted at 'wn_tree' which were marked for parallelization by the 
//   auto-parallelizer. 
//-----------------------------------------------------------------------

static void Mp_Tile_Traverse(WN* wn_tree) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 
  REDUCTION_MANAGER* rm = red_manager; 

  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    WN* wn_return = NULL; 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Mp_Info != NULL && !dli->Is_Processor_Tile)  
      wn_return = Mp_Tile_Loop(wn_tree, FALSE, &LNO_default_pool, LMT_MP);
    if (wn_return != NULL) {
      Hoist_Statements(wn_return, du);
      WN* wn_save_tree = LWN_Get_Parent(Traverse_Update(wn_return));
      if (dli->Is_Doacross) 
        Parallelize_Doacross_Loop(
		wn_return, wn_tree, dli->Doacross_Tile_Size,
		dli->Sync_Distances, dg, du);
      wn_tree = WN_do_body(wn_save_tree); 
    }
  }

  if (WN_opcode(wn_tree) == OPC_REGION) { 
    WN* wn_first = WN_first(WN_region_pragmas(wn_tree));
    if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA 
        && WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN) {
      WN* wn_scalar_region = Mp_Version_Parallel_Region(wn_tree); 
      if (wn_scalar_region != NULL) 
        Mp_Tile_Traverse(wn_scalar_region); 
    }
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    WN* wnn = NULL;
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = wnn) {
      wnn = WN_next(wn);
      Mp_Tile_Traverse(wn);
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Mp_Tile_Traverse(WN_kid(wn_tree, i));
  }
}

//-----------------------------------------------------------------------
// NAME: Mp_Tile
// FUNCTION: Processor tile (pseudo-lower) all of the loops which have been
//   marked for parallelization.
//-----------------------------------------------------------------------

extern void Mp_Tile(WN* wn_root) 
{
	// modified by csc. 2002/11/14
	// The MP_Tile cause big trouble. Turn off it.
	// TODO: Check whether the Mp_Tile is needed for SMP.
	return;
  Mp_Tile_Traverse(wn_root); 
  if (Eliminate_Dead_SCF(wn_root, LWN_Delete_Tree))
    Mark_Code(wn_root, FALSE, FALSE);
}  

