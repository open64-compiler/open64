/*
 *  Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: be/lno/SCCS/s.move.cxx $ $Revision: 1.9 $";
#endif /* _KEEP_RCS_ID */

#include <alloca.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "lnoutils.h"
#include "config.h"
#include "config_cache.h"
#include "config_lno.h"
#include "lnopt_main.h"
#include "stab.h"
#include "targ_const.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "strtab.h"
#include "targ_sim.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "wintrinsic.h"
#include "lno_bv.h"
#include "region_util.h"
#include "lego_gen.h"
#include "snl_utils.h"
#include "dep_graph.h"
#include "wn_pragmas.h"
#include "ff_utils.h"
#include "move.h"
#include "w2op.h"
#include "ipa_lno_util.h"
#include "call_info.h"
#include "debug.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "ara_utils.h" 

//-----------------------------------------------------------------------
// NAME: Hoist_Statement 
// FUNCTION: Hoist the statement 'wn_stat' so that it is enclosed by a loop 
//   of level 'hoist_level'.  If 'hoist_level' is -1, hoist 'wn_stat' out of 
//   all loops. 
//-----------------------------------------------------------------------

extern void Hoist_Statement(WN* wn_stat,
                            INT hoist_level)
{
  if (Loop_Depth(wn_stat) == hoist_level)
    return;

  WN* place_loop = NULL; 
  for (WN* wn = wn_stat; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
      if (dli->Depth == hoist_level + 1) {
        place_loop = wn;
        break;
      }
    }
  }
  FmtAssert(place_loop != NULL, ("Could not find place loop."));
  FmtAssert(WN_opcode(place_loop) == OPC_DO_LOOP, 
    ("Did not find do loop of correct level")); 
  wn_stat = LWN_Extract_From_Block(wn_stat);
  LWN_Insert_Block_Before(LWN_Get_Parent(place_loop), place_loop, wn_stat);
}

//-----------------------------------------------------------------------
// NAME: Single_Definition_Uses
// FUNCTION: Returns TRUE if all of the uses of the definition 'wn_stid'
//   have only one definition (which must be 'wn_stid'). Returns FALSE 
//   otherwise.   
//-----------------------------------------------------------------------

static BOOL Single_Definition_Uses(WN* wn_stid) 
{
  USE_LIST *use_list = Du_Mgr->Du_Get_Use(wn_stid);
  if (use_list == NULL)
    return TRUE; 
  if (use_list->Incomplete())
    return FALSE; 
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node1 = NULL;
  for (node1 = iter.First(); !iter.Is_Empty(); node1 = iter.Next()) {
    WN* wn_use = node1->Wn();
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_use);
    if (def_list->Incomplete())
      return FALSE; 
    DEF_LIST_ITER iter(def_list);
    const DU_NODE* node2 = NULL;
    for (node2 = iter.First(); !iter.Is_Empty(); node2 = iter.Next()) {
      WN* wn_def = node2->Wn();
      if (wn_def != wn_stid) 
        return FALSE; 
    }
  }
  return TRUE;
}   

//-----------------------------------------------------------------------
// NAME: Possibly_Used_Outside_Program_Unit 
// FUNCTION: Returns TRUE of the definition 'wn_stid' is possibly used 
//   outside of the program unit, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Possibly_Used_Outside_Program_Unit(WN* wn_stid) 
{
  USE_LIST *use_list = Du_Mgr->Du_Get_Use(wn_stid);
  if (use_list == NULL)
    return FALSE;
  if (use_list->Incomplete())
    return TRUE;
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node1 = NULL;
  for (node1 = iter.First(); !iter.Is_Empty(); node1 = iter.Next()) {
    WN* wn_use = node1->Wn();
    if (WN_opcode(wn_use) == OPC_RETURN
#ifdef KEY
  	|| WN_opcode(wn_use) == OPC_GOTO_OUTER_BLOCK
#endif
       )
      return TRUE; 
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Region_Depth  
// FUNCTION: Returns the depth of the loop in which the region enclosing
//   'wn_stid' is nested.  Returns -1 if 'wn_stid' is not nested within
//   any region or loop.  
//-----------------------------------------------------------------------

static INT Region_Depth(WN* wn_stid) 
{
  for (WN* wn = wn_stid; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_REGION)
      return Loop_Depth(wn); 
  return -1; 
}

//-----------------------------------------------------------------------
// NAME: May_Be_Same_Memory 
// FUNCTION: Returne TRUE if 'wn_stid' and 'wn' may refer to the same 
//   memory, FALSE otherwise. 
// NOTE: Be very careful about exporting this function.  It should only 
//   work within the context of the testing of DU chains and special 
//   calls handled in the context of LEGO/MP hoisting. 
//-----------------------------------------------------------------------

static BOOL May_Be_Same_Memory(WN* wn_stid, WN* wn)
{
  if (WN_operator(wn_stid) == OPR_LDA) 
    return WN_operator(wn) == OPR_STID 
	&& SYMBOL(wn_stid) == SYMBOL(wn);
  if (!OPCODE_is_store(WN_opcode(wn)))
    return FALSE;
  if (!Valid_alias(Alias_Mgr, wn))
    return TRUE;
  if (Aliased(Alias_Mgr, wn_stid, wn) == NOT_ALIASED) 
    return FALSE;
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Initial_Hoist_Place 
// FUNCTION: Returns the node past which it is impossible to hoist 'wn_stat'
//   regardless of DU chains. 
//-----------------------------------------------------------------------

extern WN* Initial_Hoist_Place(WN* wn_stat)
{
  WN* wn_return = WN_func_body(Current_Func_Node); 
  WN *wn;
  for (wn = wn_stat; wn != wn_return; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_REGION)
      break;
  if (WN_opcode(wn) == OPC_REGION)
    wn_return = wn; 
  return wn_return;
}

//-----------------------------------------------------------------------
// NAME: Hoist_Merge
// FUNCTION: Returns the deepest node in the parent chain connecting 'wn1'
//   and 'wn2'.  Returns NULL if there is no parent chain connecting 'wn1' 
//   and 'wn2'.
//-----------------------------------------------------------------------

extern WN* Hoist_Merge(WN* wn1, WN* wn2)
{
  if (Wn_Is_Inside(wn1, wn2))
    return wn1;
  if (Wn_Is_Inside(wn2, wn1))
    return wn2;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Scalar_Store_Hoist_Place 
// FUNCTION: Returns the node past which 'wn_stid' cannot be hoisted 
//  because its value is potentially used.
//-----------------------------------------------------------------------

static WN* Scalar_Store_Hoist_Place(WN* wn_stid) 
{
  if (!Single_Definition_Uses(wn_stid))
    return NULL;
  if (Possibly_Used_Outside_Program_Unit(wn_stid))
    return NULL; 
  WN* wn_return = Initial_Hoist_Place(wn_stid);
  WN* wn_outer_loop = NULL; 
  for (WN* wn = wn_stid; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP)
      wn_outer_loop = wn;  
  if (wn_outer_loop == NULL) 
    return NULL; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_outer_loop); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (wn == wn_stid || !May_Be_Same_Memory(wn_stid, wn))
      continue; 
    wn_return = Hoist_Merge(Common_Ancestor(wn, wn_stid), wn_return); 
    if (wn_return == NULL)
      return NULL;
  } 
  return wn_return; 
}

//-----------------------------------------------------------------------
// NAME: Hoist_Place 
// FUNCTION: Returns the node above which the node 'wn_stat' cannot be 
//   hoisted. 
//-----------------------------------------------------------------------

extern WN* Hoist_Place(WN* wn_stat,
                       DU_MANAGER* du)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  WN* wn_return = NULL; 
  OPERATOR opr = WN_operator(wn_stat); 
  switch (opr) {
  case OPR_LDID: {
    if (dg == NULL || dg->Get_Vertex(wn_stat))
      return NULL; 
    wn_return = Initial_Hoist_Place(wn_stat);  
    DEF_LIST *def_list = du->Ud_Get_Def(wn_stat);
    if (def_list == NULL || def_list->Incomplete())
      return NULL;
    if (def_list->Loop_stmt() != NULL)  
      wn_return = Hoist_Merge(def_list->Loop_stmt(), wn_return);
    DEF_LIST_ITER iter(def_list);
    const DU_NODE* node = NULL;
    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
      WN* wn_def = node->Wn();
      wn_return = Hoist_Merge(Common_Ancestor(wn_def, wn_stat), wn_return);
      if (wn_return == NULL)
	return NULL; 
    }
    return wn_return;
  }
  case OPR_STID: 
    {
      if (dg == NULL || dg->Get_Vertex(wn_stat))
        return NULL; 
      if (ST_class(WN_st(wn_stat)) == CLASS_PREG
	  && Preg_Is_Dedicated(WN_offset(wn_stat)))
	return NULL; 
      wn_return = Hoist_Place(WN_kid0(wn_stat), du);
      if (wn_return == NULL) 
        return NULL; 
      WN* wn_store_hoist = Scalar_Store_Hoist_Place(wn_stat);
      if (wn_store_hoist == NULL)
        return NULL; 
      wn_return = Hoist_Merge(wn_store_hoist, wn_return);
      return wn_return;
    }
  case OPR_DIV:
  case OPR_REM:
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MPY:
  case OPR_MIN:
  case OPR_MAX:
  case OPR_NEG:
  case OPR_LAND:
  case OPR_LIOR: 
  case OPR_CAND:
  case OPR_CIOR: 
  case OPR_MOD:
  case OPR_EQ: 
  case OPR_NE:
  case OPR_GE:
  case OPR_GT:
  case OPR_LE:
  case OPR_LT:
  case OPR_CVT: 
  case OPR_TRUNC: 
  case OPR_INTRINSIC_CALL:
  case OPR_INTRINSIC_OP: 
    {
      if (!((opr == OPR_DIV || opr == OPR_REM || opr == OPR_MOD)    
          && Safe_Spec_Map != WN_MAP_UNDEFINED
	  && WN_MAP_Get(Safe_Spec_Map, (WN*) wn_stat)
          || Statically_Safe_Node(wn_stat)))
        return NULL;  
      wn_return = Initial_Hoist_Place(wn_stat); 
      for (INT i = 0; i < WN_kid_count(wn_stat); i++) {
        WN* wn_hoist = Hoist_Place(WN_kid(wn_stat, i), du);
        if (wn_hoist == NULL)
          return NULL; 
        wn_return = Hoist_Merge(wn_hoist, wn_return);
        Is_True(wn_return != NULL, ("Expected a node in the parent chain"));
        if (wn_return == NULL)
          return NULL; 
      }
      return wn_return;
    }
  case OPR_PARM:
    if (WN_Parm_By_Value(wn_stat)) 
      return Hoist_Place(WN_kid0(wn_stat), du);
    if (WN_Parm_By_Reference(wn_stat)) 
      return Scalar_Store_Hoist_Place(WN_kid0(wn_stat));  
    FmtAssert(FALSE, ("Parameter must be by value or by reference."));
  case OPR_CALL: 
#ifdef KEY
  case OPR_PURE_CALL_OP:
#endif
    {
      if (!Special_Lego_Or_Mp_Call(WN_st(wn_stat)) 
        && (WN_Call_Never_Return(wn_stat) || WN_Call_Non_Data_Mod(wn_stat) 
        || WN_Call_Non_Data_Ref(wn_stat) || WN_Call_Non_Parm_Mod(wn_stat) 
        || WN_Call_Non_Parm_Ref(wn_stat) || WN_Call_Parm_Mod(wn_stat)))
        return wn_stat; 
      USE_LIST *use_list = du->Du_Get_Use(wn_stat);
      if (use_list == NULL || use_list->Incomplete())
	return wn_stat; 
      USE_LIST_ITER iter(use_list);
      const DU_NODE* node = NULL;
      for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
	WN* wn_use = node->Wn();
	if (OPCODE_has_sym(WN_opcode(wn_use)) 
	    && ST_class(WN_st(wn_use)) == CLASS_PREG 
	    && Preg_Is_Dedicated(WN_offset(wn_use)))
	  return wn_stat; 
      } 
      WN* wn_return = Initial_Hoist_Place(wn_stat); 
      for (INT i = 0; i < WN_kid_count(wn_stat); i++) {       
        WN* wn_hoist = Hoist_Place(WN_kid(wn_stat, i), du);
          if (wn_hoist == NULL) 
          return NULL; 
        wn_return = Hoist_Merge(wn_hoist, wn_return);
        Is_True(wn_return != NULL, ("Expected a node in the parent chain"));
        if (wn_return == NULL)
          return NULL; 
      }
      return wn_return;
    }
  case OPR_INTCONST: 
  case OPR_CONST: 
    return Initial_Hoist_Place(wn_stat);
  default:
    return NULL;
  }
}

//-----------------------------------------------------------------------
// NAME: Hoistable_Statement 
// FUNCTION: Returns the level of the loop above which 'wn_stat' cannot be
//   hoisted.  Returns -1 if 'wn_stat' can be hoisted out of all loops. 
//-----------------------------------------------------------------------

extern INT Hoistable_Statement(WN* wn_stat, 
			       DU_MANAGER* du)
{
  WN* wn_place = Hoist_Place(wn_stat, du);
  if (wn_place == NULL)
    return Loop_Depth(wn_stat);
  for (WN* wn = wn_place; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      return Do_Depth(wn);
  return -1;
}

//-----------------------------------------------------------------------
// NAME: Hoistable_Place 
// FUNCTION: Returns the statement above which 'wn_stat' can be hoisted. 
//   Returns NULL if it must be left in place. 
//-----------------------------------------------------------------------

extern WN* Hoistable_Place(WN* wn_stat, 
			   DU_MANAGER* du)
{
  WN* wn_place = Hoist_Place(wn_stat, du);
  if (wn_place == NULL)
    return NULL;
  WN* wn_return = NULL; 
  for (WN* wn = wn_stat; wn != wn_place; wn = LWN_Get_Parent(wn))
    if (WN_opcode(LWN_Get_Parent(wn)) == OPC_BLOCK)
       wn_return = wn;
  return wn_return;
}

//-----------------------------------------------------------------------
// NAME: Hoist_Statements
// FUNCTION: Hoist all of the statements which are sandwiched code in 
//   'wn_outer_loop' and can be legally hoisted out of the loop. 
//-----------------------------------------------------------------------

extern void Hoist_Statements(WN* wn_outer_loop,
                             DU_MANAGER* du)
{
  WN* next_stat = NULL;
  WN* first_stat = WN_first(WN_do_body(wn_outer_loop));
  INT hoist_level = -1;
  for (WN* stat = first_stat; stat != NULL; stat = next_stat) {
    next_stat = WN_next(stat);
    if (WN_opcode(stat) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(stat);
      if (!dli->Is_Inner) 
        Hoist_Statements(stat, du);
    } else {
      hoist_level = Hoistable_Statement(stat, du);
      if (hoist_level < Loop_Depth(stat)) 
        Hoist_Statement(stat, hoist_level);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Get_Statement 
// FUNCTION: Returns the statement of which 'wn_node' is a part. 
//-----------------------------------------------------------------------

static WN* Get_Statement(WN* wn_node)
{
  if (WN_opcode(wn_node) == OPC_FUNC_ENTRY)
    return wn_node;  
  WN *wn;
  for (wn = wn_node; wn != NULL; wn = LWN_Get_Parent(wn))
    if (LWN_Get_Parent(wn) != NULL
        && WN_opcode(LWN_Get_Parent(wn)) == OPC_BLOCK)
      return wn;
  FmtAssert(wn != NULL, ("Could not find enclosing block"));
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: New_Lowest_Statement
// FUNCTION: Returns the lowest statement for statements 'wn_one' and
//   'wn_two'.
//-----------------------------------------------------------------------

static WN* New_Lowest_Statement(WN* wn_one,
                                WN* wn_two)
{
  WN* wn_common = Common_Ancestor(wn_one, wn_two);
  if (WN_opcode(wn_common) != OPC_BLOCK)
    return wn_common;
  for (WN* wn = WN_first(wn_common); wn != NULL; wn = WN_next(wn)) {
    if (Wn_Is_Inside(wn_one, wn))
      return wn;
    if (Wn_Is_Inside(wn_two, wn))
      return wn;
  }
  FmtAssert(TRUE, ("New_Lowest_Statement: Didn't find lowest statement"));
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Hoist_Point
// FUNCTION: Returns the statement above which 'wn_stat' must be hoisted
//   so that none of its uses appear after it in control flow. 
//-----------------------------------------------------------------------

static WN* Hoist_Point(WN* wn_stat,
		       DU_MANAGER* du)
{
  WN* wn_lowest_statement = wn_stat;
  INT lowest_loop_depth = Loop_Depth(wn_stat); 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_stat);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn_def = itr->wn;
    USE_LIST *use_list = du->Du_Get_Use(wn_def);
    if (use_list == NULL) 
      continue; 
    USE_LIST_ITER iter(use_list);
    const DU_NODE* node = NULL;
    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
      WN* wn_use = node->Wn();
      WN* wn_statement = Get_Statement(wn_use);
      if (wn_statement == wn_lowest_statement)
	continue; 
      INT current_loop_depth = Loop_Depth(wn_statement); 
      if (current_loop_depth < lowest_loop_depth) {
	wn_lowest_statement = wn_statement; 
        lowest_loop_depth = current_loop_depth; 
      } else if (current_loop_depth == lowest_loop_depth) {
        WN* wn_new_statement = New_Lowest_Statement(wn_statement,
          wn_lowest_statement);
        if (wn_new_statement != wn_lowest_statement) {
          wn_lowest_statement = wn_new_statement;
          lowest_loop_depth = Loop_Depth(wn_new_statement);
        }
      } 
    }
  }
  return wn_lowest_statement; 
}

//-----------------------------------------------------------------------
// NAME: Hoist_Necessary_Code_Up 
// FUNCTION: Hoist all of the code between 'wn_sunk' and the beginning
//   of its block so that any variable defined in these statements 
//   appears before it is used.  
//-----------------------------------------------------------------------

extern void Hoist_Necessary_Code_Up(WN* wn_sunk, 
				    DU_MANAGER* du)
{
  WN* wnn = NULL;
  for (WN* wn = wn_sunk; wn != NULL; wn = wnn) {
    wnn = WN_prev(wn);
    WN* wn_hoist_place = Hoist_Point(wn, du);
    if (wn_hoist_place != wn) {
      LWN_Extract_From_Block(LWN_Get_Parent(wn), wn);
      LWN_Insert_Block_Before(LWN_Get_Parent(wn_hoist_place), 
	wn_hoist_place, wn);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Is_Inside_Store 
// FUNCTION: Returns TRUE if 'wn_node' is inside a store, FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Is_Inside_Store(WN* wn_node)
{
  for (WN* wn = wn_node; wn != NULL; wn = LWN_Get_Parent(wn))
    if (OPCODE_is_store(WN_opcode(wn)))
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Maybe_Assigned  
// FUNCTION: Returns TRUE if the variable indicated by 'wn_symbol' may be 
//   assigned within the code between 'wn_first' and 'wn_last' inclusive.
//   Returns FALSE otherwise.
// NOTE: 'wn_first' and 'wn_last must be siblings in a block.  
//-----------------------------------------------------------------------

static BOOL Maybe_Assigned(WN* wn_symbol,
			   WN* wn_first, 
			   WN* wn_last)
{
  WN* wn_alias = wn_symbol;
  if (WN_operator(wn_symbol) == OPR_PARM)
    wn_symbol = WN_kid0(wn_symbol);
  BOOL is_preg = ST_class(WN_st(wn_symbol)) == CLASS_PREG; 
  BOOL is_inside_store = Is_Inside_Store(wn_symbol); 
  for (WN* wn_stat = wn_first; wn_stat != NULL; wn_stat = WN_next(wn_stat)) {
    LWN_ITER* itr = LWN_WALK_TreeIter(wn_stat); 
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      WN* wn = itr->wn;
      if (WN_operator(wn) == OPR_LDA 
        && wn != wn_symbol && SYMBOL(wn) == SYMBOL(wn_symbol)
	&& (is_inside_store || Is_Inside_Store(wn)))
        return TRUE; 
      if (OPCODE_is_store(WN_opcode(wn))  
        && (is_preg 
        || OPCODE_has_sym(WN_opcode(wn)) && ST_class(WN_st(wn)) == CLASS_PREG)
	&& (!OPCODE_has_sym(WN_opcode(wn)) || SYMBOL(wn) != SYMBOL(wn_symbol)))
	continue; 
      if (OPCODE_is_store(WN_opcode(wn)) && Valid_alias(Alias_Mgr, wn)
	&& wn != wn_alias && Aliased(Alias_Mgr, wn, wn_alias) != NOT_ALIASED)
	return TRUE; 
      if (OPCODE_is_call(WN_opcode(wn))) { 
	if (Has_Call_Info(wn)) {
	  POINTS_TO* pt_symbol = Points_To(wn_symbol, &LNO_local_pool);
	  ARA_LOOP_INFO* ali_call = Get_Call_Info(wn)->Call_Ara_Info();
          INT i;
	  for (i = 0; i < ali_call->MAY_DEF().Elements(); i++) { 
	    ARA_REF* ara_call = ali_call->MAY_DEF().Bottom_nth(i);
            POINTS_TO* pt_call = Points_To(ara_call, &LNO_local_pool);
	    if (Alias_Mgr->Aliased(pt_call, pt_symbol))
	      return TRUE; 
	    if (SYMBOL(wn_symbol) == ara_call->Array())
	      return TRUE; 
	  } 
	  for (i = 0; i < ali_call->SCALAR_MAY_DEF().Elements(); i++) { 
	    SCALAR_STACK& stk = (SCALAR_STACK&) ali_call->SCALAR_MAY_DEF();
	    SCALAR_NODE* sn = stk.Bottom_nth(i);
	    POINTS_TO* pt_sn = Points_To(sn, &LNO_local_pool); 
	    if (Alias_Mgr->Aliased(pt_sn, pt_symbol))
	      return TRUE; 
	    if (SYMBOL(wn_symbol) == stk.Bottom_nth(i)->_scalar)
	      return TRUE; 
	  } 
	} else { 
	  if (Aliased_with_region(Alias_Mgr, wn, wn_alias, WRITE) 
	      != NOT_ALIASED)
	    return TRUE; 
	} 
        return FALSE; 
      } 
    } 
    if (wn_stat == wn_last) 
      return FALSE;
  }
  FmtAssert(FALSE, ("wn_first and wn_last were not siblings")); 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Is_In_Range
// FUNCTION: Returns TRUE if 'wn_node' is in the range of statements from 
//   'wn_first' to 'wn_last' inclusive.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_In_Range(WN* wn_node, 
			WN* wn_first,
			WN* wn_last)
{ 
  WN *wn;
  for (wn = wn_node; wn != NULL; wn = LWN_Get_Parent(wn))  
    if (LWN_Get_Parent(wn) == LWN_Get_Parent(wn_first))
      break; 
  if (wn == NULL) 
    return FALSE; 
  if (wn == wn_first || wn == wn_last)
    return TRUE; 
  WN* wn_sibling = wn; 
  for (wn = wn_sibling; wn != NULL; wn = WN_next(wn)) {
    if (wn == wn_first)
      return FALSE; 
    if (wn == wn_last) 
      return TRUE; 
  } 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Maybe_Assigned_Exp_Traverse 
// FUNCTION: Returns TRUE if any node in 'wn_exp' may be assigned in the 
//   code segment between 'wn_first' and 'wn_last' inclusive. 
//-----------------------------------------------------------------------

static BOOL Maybe_Assigned_Exp_Traverse(WN* wn_exp,
                                        WN* wn_first,
                                        WN* wn_last)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  FmtAssert(OPCODE_is_expression(WN_opcode(wn_exp)), 
    ("wn_exp must be an expression"));

  switch (WN_operator(wn_exp)) {
  case OPR_LDA:
    return FALSE; 
  case OPR_LDID: 
    return Maybe_Assigned(wn_exp, wn_first, wn_last);
  case OPR_INTCONST: 
  case OPR_CONST:
    return FALSE;
  case OPR_ILOAD: 
    {
      EINDEX16 e = 0; 
      if (dg == NULL)
        return TRUE; 
      VINDEX16 v = dg->Get_Vertex(wn_exp);
      if (v == 0) {
        for (WN* wnn = wn_first; wnn != NULL; wnn = WN_next(wnn)) { 
	  LWN_ITER* itr = LWN_WALK_TreeIter(wnn);
	  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
	    WN* wn = itr->wn; 
#ifdef KEY
	    // Bug 4426 - address is passed as a parameter to call 
	    // and call does not have a valid alias mapping. Also, the call
	    // uses dope vector to pass the address of the array so it is not
	    // immediately obvious how we can match 'wn' and 'wn_exp'. 
	    if (WN_operator(wn) == OPR_CALL && !Valid_alias(Alias_Mgr, wn))
	      return TRUE;
#endif
	    if (Valid_alias(Alias_Mgr, wn) 
	        && !OPCODE_is_load(WN_opcode(wn))
	        && Aliased(Alias_Mgr, wn, wn_exp) != NOT_ALIASED)
	      return TRUE; 
	  } 
	  if (wnn == wn_last) 
	    break; 
        } 
      } else { 
        for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
	  WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
	  if (Is_In_Range(wn_source, wn_first, wn_last))
	    return TRUE; 
        } 
        for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
	  WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
	  if (Is_In_Range(wn_sink, wn_first, wn_last))
	    return TRUE; 
        } 
      } 
      for (INT i = 0; i < WN_kid_count(wn_exp); i++)
        if (Maybe_Assigned_Exp_Traverse(WN_kid(wn_exp, i), wn_first, wn_last))
          return TRUE; 
      return FALSE; 
    }
  case OPR_PARM:
    if (WN_Parm_By_Value(wn_exp)) 
      return Maybe_Assigned_Exp_Traverse(WN_kid0(wn_exp), wn_first, wn_last);  
    return TRUE; 
  case OPR_DIV:
  case OPR_REM:
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MPY:
  case OPR_MIN:
  case OPR_MAX:
  case OPR_NEG:
  case OPR_LAND:
  case OPR_LIOR:
  case OPR_CAND:
  case OPR_CIOR: 
  case OPR_MOD:
  case OPR_EQ:
  case OPR_NE:
  case OPR_GE:
  case OPR_GT:
  case OPR_LE:
  case OPR_LT:
  case OPR_CVT:
  case OPR_TRUNC:
  case OPR_INTRINSIC_CALL:
  case OPR_INTRINSIC_OP:
  case OPR_ARRAY: 
#ifdef KEY
  case OPR_PURE_CALL_OP:
#endif
    {
      for (INT i = 0; i < WN_kid_count(wn_exp); i++)
        if (Maybe_Assigned_Exp_Traverse(WN_kid(wn_exp, i), wn_first, wn_last))
	  return TRUE; 
      return FALSE; 
    }
  default: 
    return TRUE; 
  } 
}

//-----------------------------------------------------------------------
// NAME: Maybe_Assigned_Exp 
// FUNCTION: Returns TRUE if any node in 'wn_exp' may be assigned in the 
//   code segment between 'wn_first' and 'wn_last' inclusive. 
//-----------------------------------------------------------------------

extern BOOL Maybe_Assigned_Exp(WN* wn_exp,
                               WN* wn_first,
                               WN* wn_last)
{
  return Maybe_Assigned_Exp_Traverse(wn_exp, wn_first, wn_last);
}

//-----------------------------------------------------------------------
// NAME: Trip_One_Loop 
// FUNCTION: Returns TRUE if 'wn_loop' will always execute at least once, 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Trip_One_Loop(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  if (dli->Is_Processor_Tile)
    return TRUE; 
  if (Iterations(wn_loop, &LNO_local_pool) > 0)
    return TRUE; 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Loop_Dominates_Uses 
// FUNCTION: Returns TRUE if all of the uses of the definition 'wn_stat' 
//   are dominated by the loop entry point 'wn_loop'.  
//-----------------------------------------------------------------------

static BOOL Loop_Dominates_Uses(WN* wn_stat, 
				WN* wn_loop, 
				DU_MANAGER* du)
{
  USE_LIST *use_list = du->Du_Get_Use(wn_stat);
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
      if (wn == WN_do_body(wn_loop))
	break;
    if (wn == NULL)
      return FALSE;
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Find_Sibling_Containing 
// FUNCTION: Returns the sibling of 'wn_stat' after or including 'wn_stat'
//   which contains 'wn_contained' if there is one, NULL otherwise. 
//-----------------------------------------------------------------------

extern WN* Find_Sibling_Containing(WN* wn_stat, 
				   WN* wn_contained)
{
  for (WN* wn = wn_stat; wn != NULL; wn = WN_next(wn)) {
    for (WN* wnn = wn_contained; wnn != NULL; wnn = LWN_Get_Parent(wnn))
      if (wnn == wn)
	return wn;
  }
  return NULL; 
} 

//-----------------------------------------------------------------------
// NAME: Sinkable_Into_Loop 
// FUNCTION: Returns TRUE if the subtree 'wn_test' of 'wn_stat' has all 
//   the necessary properties to the sunk into the 'wn_sink_loop'.  
//   Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Sinkable_Into_Loop(WN* wn_stat, 
			       WN* wn_test,
			       WN* wn_sink_loop, 
			       DU_MANAGER* du)
{
  WN* wn_sibling = Find_Sibling_Containing(wn_stat, wn_sink_loop);
  FmtAssert(wn_sibling != NULL, ("Could not find a sink loop sibling."));  
  switch (WN_operator(wn_test)) {
  case OPR_LDID: 
    return !Maybe_Assigned(wn_test, WN_next(wn_stat), wn_sibling); 
  case OPR_STID:
    if (ST_class(WN_st(wn_test)) != CLASS_PREG) {
      WN* wn_enclosing = Enclosing_Loop(wn_stat); 
      for (WN* wn = wn_sink_loop; wn != wn_enclosing; wn = LWN_Get_Parent(wn))
	if (WN_opcode(wn) == OPC_DO_LOOP && !Trip_One_Loop(wn))
	  return FALSE; 
    } else if (Preg_Is_Dedicated(WN_offset(wn_test))) { 
      return FALSE; 
    } 
    if (Maybe_Assigned(wn_test, WN_next(wn_stat), wn_sibling))
      return FALSE;
    if (!Loop_Dominates_Uses(wn_stat, wn_sink_loop, du))
      return FALSE;
    return TRUE; 
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MPY:
  case OPR_DIV:
  case OPR_MIN:
  case OPR_MAX:
  case OPR_NEG:
  case OPR_EQ: 
  case OPR_INTRINSIC_CALL:
  case OPR_INTRINSIC_OP:
    {
      for (INT i = 0; i < WN_kid_count(wn_test); i++) 
        if (!Sinkable_Into_Loop(wn_stat, WN_kid(wn_test, i), wn_sink_loop, du))
	  return FALSE; 
      return TRUE; 
    }
  case OPR_PARM:
    if (WN_Parm_By_Value(wn_test)) 
      return Sinkable_Into_Loop(wn_stat, WN_kid0(wn_test), wn_sink_loop, du);
    else if (WN_Parm_By_Reference(wn_test)) {
      if (Maybe_Assigned(wn_test, WN_next(wn_stat), wn_sibling))
        return FALSE;
      else if (!Loop_Dominates_Uses(wn_stat, wn_sink_loop,du))
        return FALSE;
      else
	return TRUE;
    }
    FmtAssert(FALSE, ("Parameter must be by value or by reference."));
  case OPR_CALL: 
#ifdef KEY
  case OPR_PURE_CALL_OP:
#endif
    {
      if (!Special_Lego_Or_Mp_Call(WN_st(wn_test)) 
        && (WN_Call_Never_Return(wn_test) || WN_Call_Non_Data_Mod(wn_test) 
        || WN_Call_Non_Data_Ref(wn_test) || WN_Call_Non_Parm_Mod(wn_test) 
        || WN_Call_Non_Parm_Ref(wn_test) || WN_Call_Parm_Mod(wn_test)))
        return FALSE;
      for (INT i = 0; i < WN_kid_count(wn_test); i++) 
        if (!Sinkable_Into_Loop(wn_stat, WN_kid(wn_test, i), wn_sibling, du))
	  return FALSE; 
      return TRUE; 
    }
  case OPR_INTCONST:
    return TRUE;
  case OPC_BLOCK:
    {
      for (WN* wnn = WN_first(wn_test); wnn != NULL; wnn = WN_next(wnn))
        if (!Sinkable_Into_Loop(wn_stat, wnn, wn_sink_loop, du))
	  return FALSE; 
      return TRUE;  
    }
  case OPC_IF: 
    if (!Sinkable_Into_Loop(wn_stat, WN_if_test(wn_test), wn_sink_loop, du))
      return FALSE;
    if (!Sinkable_Into_Loop(wn_stat, WN_then(wn_test), wn_sink_loop, du))
      return FALSE;
    if (!Sinkable_Into_Loop(wn_stat, WN_else(wn_test), wn_sink_loop, du))
      return FALSE;
    return TRUE; 
  default:
    return FALSE;  
  }
}

//-----------------------------------------------------------------------
// NAME: All_Uses_Outside_Of_Loop 
// FUNCTION: Returns TRUE if all of the uses of the definition 'wn_stat' 
//   are outside the loop 'wn_loop'.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL All_Uses_Outside_Of_Loop(WN* wn_stat, 
				     WN* wn_loop, 
				     DU_MANAGER* du)
{
  USE_LIST *use_list = du->Du_Get_Use(wn_stat);
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {  
    WN* wn_use = node->Wn(); 
    WN* wn = NULL; 
    for (wn = wn_use; wn != NULL; wn = LWN_Get_Parent(wn))
      if (wn == wn_loop)
	break;
    if (wn != NULL) 
      return FALSE;  
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Sinkable_Out_Of_Loop
// FUNCTION: Returns TRUE if the subtree 'wn_test' of 'wn_stat' has all 
//   the necessary properties to the sunk out of the 'wn_sink_loop'.  
//   Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Sinkable_Out_Of_Loop(WN* wn_stat, 
			         WN* wn_test,
			         WN* wn_sink_loop, 
			         DU_MANAGER* du)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  switch (WN_operator(wn_test)) {
  case OPR_LDID:
    return !Maybe_Assigned(wn_test, wn_sink_loop, wn_sink_loop); 
  case OPR_STID:
    {
      if (!All_Uses_Outside_Of_Loop(wn_stat, wn_sink_loop, du))
        return FALSE;
      for (INT i = 0; i < WN_kid_count(wn_test); i++) 
        if (!Sinkable_Out_Of_Loop(wn_stat, WN_kid(wn_test, i), wn_sink_loop, du))
	  return FALSE; 
      return TRUE; 
    }
  case OPR_ILOAD: 
    {
      EINDEX16 e = 0; 
      if (dg == NULL)
        return TRUE;
      VINDEX16 v = dg->Get_Vertex(wn_test);
      if (v == 0) 
        return TRUE;  
      for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
        WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
        if (Wn_Is_Inside(wn_source, wn_sink_loop)) 
	  return FALSE; 
      } 
      for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
        WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
        if (Wn_Is_Inside(wn_sink, wn_sink_loop)) 
	  return FALSE; 
      } 
      // Fall through is intentional. 
    }
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MPY:
  case OPR_DIV:
  case OPR_MIN:
  case OPR_MAX:
  case OPR_NEG:
  case OPR_INTRINSIC_CALL:
  case OPR_INTRINSIC_OP:
  case OPR_ARRAY: 
    {
      for (INT i = 0; i < WN_kid_count(wn_test); i++) 
        if (!Sinkable_Out_Of_Loop(wn_stat, WN_kid(wn_test, i), wn_sink_loop, du))
	  return FALSE; 
      return TRUE; 
    }
  case OPR_PARM:
    if (WN_Parm_By_Value(wn_test)) 
      return Sinkable_Out_Of_Loop(wn_stat, WN_kid0(wn_test), wn_sink_loop, du);
    if (WN_Parm_By_Reference(wn_test))
#ifdef KEY //bug 14207
      return !Maybe_Assigned(WN_kid0(wn_test), wn_sink_loop, wn_sink_loop);
#else
      return !Maybe_Assigned(WN_kid0(wn_test), WN_next(wn_stat), wn_sink_loop);
#endif 
    FmtAssert(FALSE, ("Parameter must be by value or by reference."));
  case OPR_CALL: 
#ifdef KEY
  case OPR_PURE_CALL_OP:
#endif
    {
      if (!Special_Lego_Or_Mp_Call(WN_st(wn_test)) 
        && (WN_Call_Never_Return(wn_test) || WN_Call_Non_Data_Mod(wn_test) 
        || WN_Call_Non_Data_Ref(wn_test) || WN_Call_Non_Parm_Mod(wn_test) 
        || WN_Call_Non_Parm_Ref(wn_test) || WN_Call_Parm_Mod(wn_test)))
        return FALSE;
      for (INT i = 0; i < WN_kid_count(wn_test); i++) 
        if (!Sinkable_Out_Of_Loop(wn_stat, WN_kid(wn_test, i), wn_sink_loop, du))
	  return FALSE; 
      return TRUE; 
    }
  case OPR_INTCONST:
    return TRUE;
  default:
    return FALSE;  
  }
}

//-----------------------------------------------------------------------
// NAME: Statement_Sinkable_Out_Of_Loop
// FUNCTION: Returns TRUE if 'wn_stat' is sinkable out of the loop 'wn_loop'.
//   Returns FALSE if not, or if we can't prove it. 
//-----------------------------------------------------------------------

extern BOOL Statement_Sinkable_Out_Of_Loop(WN* wn_stat, 
				 WN* wn_loop)
{
  DU_MANAGER* du = Du_Mgr; 
  FmtAssert(WN_opcode(LWN_Get_Parent(wn_stat)) == OPC_BLOCK, 
    ("Sinkable_Out_Of_Loop: First arg must be a statement")); 
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_stat, &stack);
  INT outer_depth = Do_Loop_Depth(wn_loop);
  WN* wn_inner_loop = stack.Bottom_nth(stack.Elements() - 1); 
  INT inner_depth = Do_Loop_Depth(wn_inner_loop); 
  for (INT i = outer_depth; i <= inner_depth; i++) { 
    WN* wn_loop = stack.Bottom_nth(i); 
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
    if (dli_loop->Has_Gotos)
      return FALSE; 
    if (!Upper_Bound_Standardize(WN_end(wn_loop), TRUE))
      return FALSE; 
    for (INT j = i + 1; j <= inner_depth; j++) 
      if (!SNL_Is_Invariant(&stack, i, j))
	return FALSE; 
  } 
  return Sinkable_Out_Of_Loop(wn_stat, wn_stat, wn_loop, du); 
} 

//-----------------------------------------------------------------------
// NAME: Sandwiched_Code_Sinkable_In 
// FUNCTION: Returns TRUE if the sandwiched code within 'wn_outer_loop' 
//   and 'wn_sink_loop' can be sunk inside 'wn_sink_loop'.  Returns 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Sandwiched_Code_Sinkable_In(WN* wn_outer_loop,
                                        WN* wn_sink_loop,
                                        DU_MANAGER* du)
{
  WN* wn_sink_block = WN_do_body(wn_sink_loop);
  if (WN_next(wn_sink_loop))
    return FALSE; 
  for (WN* wn_loop = wn_sink_loop; wn_loop != wn_outer_loop;
    wn_loop = LWN_Get_Parent(LWN_Get_Parent(wn_loop))) {
    WN* wnn = NULL;
    for (WN* wn = WN_prev(wn_loop); wn != NULL; wn = wnn) {
      wnn = WN_prev(wn);
      if (!Sinkable_Into_Loop(wn, wn, wn_sink_loop, du)) 
        return FALSE; 
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Sink_Sandwiched_Code_In 
// FUNCTION: Sink the sandwiched code between 'wn_outer_loop' and 
//   'wn_sink_loop' inside 'wn_sink_loop'. Returns the first statement
//   sunk.
//-----------------------------------------------------------------------

extern WN* Sink_Sandwiched_Code_In(WN* wn_outer_loop,
			           WN* wn_sink_loop) 
{
  WN* wn_first = NULL;
  WN* wn_sink_block = WN_do_body(wn_sink_loop); 
  for (WN* wn_loop = wn_sink_loop; wn_loop != wn_outer_loop;   
    wn_loop = LWN_Get_Parent(LWN_Get_Parent(wn_loop))) {
    WN* wnn = NULL; 
    for (WN* wn = WN_prev(wn_loop); wn != NULL; wn = wnn) {
      wnn = WN_prev(wn);
      if (wn_first == NULL)
	wn_first = wn; 
      LWN_Extract_From_Block(LWN_Get_Parent(wn), wn);
      LWN_Insert_Block_After(wn_sink_block, NULL, wn);  
    }
  }
  return wn_first;
}

//-----------------------------------------------------------------------
// NAME: Hoist_Out_Nested_Statements 
// FUNCTION: Hoist the statements within the loop 'wn_loop' out of the loop 
//   'wn_hoist_loop'. 
//-----------------------------------------------------------------------

static void Hoist_Out_Nested_Statements(WN* wn_loop, 
				        WN* wn_hoist_loop, 
				        DU_MANAGER* du)
{
  WN* wnn = NULL; 
  WN* wn_inner_loop = NULL; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->Is_Inner)
    return; 
  for (WN* wn = WN_first(WN_do_body(wn_loop)); wn != NULL; wn = wnn) {
    wnn = WN_next(wn);
    FmtAssert(WN_opcode(wn) != OPC_REGION, 
      ("MP regions should be filtered out by now."));
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      Hoist_Out_Nested_Statements(wn, wn_hoist_loop, du);
      return; 
    } 
    INT hoist_level = Hoistable_Statement(wn, du);
    if (hoist_level < Loop_Depth(wn))
      Hoist_Statement(wn, hoist_level); 
  } 
}

//-----------------------------------------------------------------------
// NAME: Sandwiched_Code_Sinkable_Out 
// FUNCTION: Returns TRUE if the sandwiched code inside 'wn_loop' can be  
//   and can be sunk outside of 'wn_sink_loop'.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Sandwiched_Code_Sinkable_Out(WN* wn_loop, 
					 WN* wn_sink_loop, 
					 DU_MANAGER* du)
{
  WN* wn_inner_loop = NULL;
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->Is_Inner)
    return TRUE;
  WN* wnn = NULL; 
  WN *wn;
  for (wn = WN_first(WN_do_body(wn_loop)); wn != NULL; wn = wnn) {
    WN* wnn = WN_next(wn); 
    FmtAssert(WN_opcode(wn) != OPC_REGION, 
      ("MP regions should be filtered out by now."));
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      wn_inner_loop = wn;
      break;
    }
  }
  for (wn = WN_next(wn_inner_loop); wn != NULL; wn = WN_next(wn)) 
    if (!Statement_Sinkable_Out_Of_Loop(wn, wn_loop))
      return FALSE; 
  return Sandwiched_Code_Sinkable_Out(wn_inner_loop, wn_sink_loop, du); 
} 

//-----------------------------------------------------------------------
// NAME: Generate_Guard_Test 
// FUNCTION: Generate a guard test corresponding to the loop 'wn_loop' 
//   and place it in 'wn_block'.  Return a WN* to the THEN block of the 
//   the IF test that was created for the guard test. 
//-----------------------------------------------------------------------

static WN* Generate_Guard_Test(WN* wn_loop, 
			       WN* wn_block, 
			       DU_MANAGER* du)
{
  if (Trip_One_Loop(wn_loop))
    return wn_block; 
  INT outer_depth = Do_Loop_Depth(wn_loop) - 1;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  WN_MAP version_map = WN_MAP_Create(&LNO_local_pool);
  WN* wn_cp_end = LWN_Copy_Tree(UBexp(WN_end(wn_loop)), TRUE, LNO_Info_Map, 
    TRUE, version_map);
  LWN_Copy_Def_Use(UBexp(WN_end(wn_loop)), wn_cp_end, du); 
  if (outer_depth >= 0)
    dg->Versioned_Dependences_Update(UBexp(WN_end(wn_loop)), wn_cp_end, 
      outer_depth, version_map); 
  WN_MAP_Delete(version_map);
  version_map = WN_MAP_Create(&LNO_local_pool);
  WN* wn_cp_start = LWN_Copy_Tree(WN_kid0(WN_start(wn_loop)), TRUE, 
    LNO_Info_Map, TRUE, version_map);
  LWN_Copy_Def_Use(WN_kid0(WN_start(wn_loop)), wn_cp_start, du); 
  if (outer_depth >= 0)
    dg->Versioned_Dependences_Update(WN_kid0(WN_start(wn_loop)), wn_cp_start, 
      outer_depth, version_map);
  WN_MAP_Delete(version_map);
  OPCODE opge = OPCODE_make_op(OPR_GE, Boolean_type, 
    Promote_Type(Do_Wtype(wn_loop)));
  WN* wn_cond = LWN_CreateExp2(opge, wn_cp_end, wn_cp_start);
  WN* wn_if_block = WN_CreateBlock(); 
  WN* wn_if = LWN_CreateIf(wn_cond, wn_if_block, WN_CreateBlock());
  LWN_Insert_Block_After(wn_block, NULL, wn_if);     
  return wn_if_block; 
}

//-----------------------------------------------------------------------
// NAME: Generate_Sink_Out_Code 
// FUNCTION: Add to the block 'wn_block' the statements within 'wn_loop' 
//   which can be sunk out of 'wn_sink_loop', together with appropriate 
//   guard tests.  If 'test_legality', test for legality and sink only 
//   those statements which are proved to be sinkable.
//-----------------------------------------------------------------------

static void Generate_Sink_Out_Code(WN* wn_loop,
  				   WN* wn_sink_loop, 
				   INT max_depth, 
				   WN* wn_block, 
				   BOOL test_legality, 
				   DU_MANAGER* du) 
{
  WN* wn_inner_loop = NULL;
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->Is_Inner || max_depth >= 0 && Do_Depth(wn_loop) >= max_depth)
    return;
  WN *wn;
  for (wn = WN_first(WN_do_body(wn_loop)); wn != NULL; wn = WN_next(wn)) { 
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      wn_inner_loop = wn; 
      break;
    }
  }
  WN* wn_insert_block = Generate_Guard_Test(wn_loop, wn_block, du); 
  if (WN_next(wn_inner_loop) != NULL) {
    WN* wn_first = WN_next(wn_inner_loop);
    WN *wnn;
    for (wnn = wn_first; WN_next(wnn) != NULL; wnn = WN_next(wnn));
    for (wn = wnn; wn != wn_inner_loop; wn = wnn) {
      wnn = WN_prev(wn);
      if (!test_legality || Statement_Sinkable_Out_Of_Loop(wn, wn_sink_loop)) { 
	LWN_Extract_From_Block(LWN_Get_Parent(wn), wn);
	LWN_Insert_Block_After(wn_insert_block, NULL, wn);
      } 
    }
  }
  Generate_Sink_Out_Code(wn_inner_loop, wn_sink_loop, max_depth, 
    wn_insert_block, test_legality, du); 
}

//-----------------------------------------------------------------------
// NAME: Simplify_Sink_Out_Code 
// FUNCTION: Simplify the code in 'wn_block' by removing redundant 'IFs'. 
//-----------------------------------------------------------------------

static void Simplify_Sink_Out_Code(WN* wn_block)
{
  WN* wnn = NULL; 
  if (wn_block == NULL) 
    return; 
  for (WN* wn = WN_first(wn_block); wn != NULL; wn = wnn) {
    wnn = WN_next(wn); 
    if (WN_opcode(wn) == OPC_IF) {
      Simplify_Sink_Out_Code(WN_then(wn)); 
      Simplify_Sink_Out_Code(WN_else(wn));
      if (WN_first(WN_then(wn)) == NULL && WN_first(WN_else(wn)) == NULL) {
        LWN_Extract_From_Block(LWN_Get_Parent(wn), wn);
	LWN_Delete_Tree(wn); 
      }
    }
  } 
}

//-----------------------------------------------------------------------
// NAME: Generate_If_Accesses 
// FUNCTION: Generate access information for the IF nodes in the tree 
//   rooted at 'wn_node'.  
//-----------------------------------------------------------------------

static void Generate_If_Accesses(WN* wn_node)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_node); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_opcode(wn) == OPC_IF) {
      IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE), 
        &LNO_default_pool);
      WN_MAP_Set(LNO_Info_Map, wn, (void *) ii);
      DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), 
	&LNO_local_pool);
      Build_Doloop_Stack(wn, stack);
      LNO_Build_If_Access(wn, stack);
      CXX_DELETE(stack, &LNO_local_pool);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Delete_Deps 
// FUNCTION: Deletes the dependences incident on the node 'wn'. 
//-----------------------------------------------------------------------

static void Delete_Deps(WN* wn, 
		        ARRAY_DIRECTED_GRAPH16* dg)
{
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  VINDEX16 v = dg->Get_Vertex(wn);
  EINDEX16 e = 0;
  EINDEX16 enext = 0;
  for (e = dg->Get_In_Edge(v); e != 0; e = enext) {
    enext = dg->Get_Next_In_Edge(e);
    dg->Delete_Array_Edge(e);
  }
  for (e = dg->Get_Out_Edge(v); e != 0; e = enext) {
    enext = dg->Get_Next_Out_Edge(e);
    dg->Delete_Array_Edge(e);
  }
  dg->Delete_Vertex(v);
}

//-----------------------------------------------------------------------
// NAME: Recompute_Deps
// FUNCTION: Recompute the dependences for the node 'wn'. The LS_IN_LOOP
//   'ls_loop' is derived for a loop enclosing 'wn' if there is one, 
//   otherwise it is NULL.  
//-----------------------------------------------------------------------
static void Recompute_Deps(WN* wn,
			   LS_IN_LOOP* ls_loop, 
                           ARRAY_DIRECTED_GRAPH16* dg) 
{
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  DOLOOP_STACK wn_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn, &wn_stack);
#ifdef KEY //bug 11113
  typedef STACK<WN *> WN_STACK;
  WN_STACK *tmp_stack =
        CXX_NEW(WN_STACK(&LNO_local_pool),&LNO_local_pool);
#endif
  EINDEX16 e = 0;
  DOLOOP_STACK source_stack(&LNO_local_pool);
  VINDEX16 v = dg->Get_Vertex(wn);
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
#ifdef KEY
    tmp_stack->Push(wn_source); //bug 11113: collect sources
#else
    Build_Doloop_Stack(wn_source, &source_stack);
#endif
    dg->Delete_Array_Edge(e);
    EINDEX16 ec = 0;
    for (ec = dg->Get_Out_Edge(v); ec != 0; ec = dg->Get_Next_Out_Edge(ec)) {
      WN* wn_sink = dg->Get_Wn(dg->Get_Sink(ec));
      if (wn_sink == wn_source)
	break;
    }
    if (ec != 0)
      dg->Delete_Array_Edge(ec);
#ifndef KEY //bug 11113
    if (!dg->Add_Edge(wn_source, &source_stack, wn,
         &wn_stack, ls_loop->In(wn_source) < ls_loop->In(wn)))
      LNO_Erase_Dg_From_Here_In(wn, dg);
    source_stack.Clear();
#endif
  }

#ifdef KEY //bug 11113: build incoming edges only after all incoming edges removed 
 while(tmp_stack->Elements() != 0){
   WN *wn_source = tmp_stack->Pop();
   Build_Doloop_Stack(wn_source, &source_stack);
  if (!dg->Add_Edge(wn_source, &source_stack, wn,
      &wn_stack, ls_loop->In(wn_source) < ls_loop->In(wn)))
        LNO_Erase_Dg_From_Here_In(wn, dg);
    source_stack.Clear();
 }
#endif   
  DOLOOP_STACK sink_stack(&LNO_local_pool);
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
#ifdef KEY
    tmp_stack->Push(wn_sink); //bug 11113: collect sinks
#else
    Build_Doloop_Stack(wn_sink, &sink_stack);
#endif
    dg->Delete_Array_Edge(e);
    EINDEX16 ec = 0;
    for (ec = dg->Get_In_Edge(v); ec != 0; ec = dg->Get_Next_In_Edge(ec)) {
      WN* wn_source = dg->Get_Wn(dg->Get_Source(ec));
      if (wn_source == wn_sink)
	break;
    }
    if (ec != 0)
      dg->Delete_Array_Edge(ec);
#ifndef KEY //bug 11113
    if (!dg->Add_Edge(wn, &wn_stack, wn_sink,
      &sink_stack, ls_loop->In(wn) < ls_loop->In(wn_sink)))
      LNO_Erase_Dg_From_Here_In(wn, dg);
      sink_stack.Clear();
#endif
  }
#ifdef KEY //bug 11113: build outgoing edges only after all outgoing edges deleted
 while(tmp_stack->Elements() != 0){
   WN *wn_sink = tmp_stack->Pop();
   Build_Doloop_Stack(wn_sink, &sink_stack);
   if (!dg->Add_Edge(wn, &wn_stack, wn_sink,
      &sink_stack, ls_loop->In(wn) < ls_loop->In(wn_sink)))
      LNO_Erase_Dg_From_Here_In(wn, dg);
      sink_stack.Clear();
 }
#endif
}

//-----------------------------------------------------------------------
// NAME: Recompute_Deps_For_Tree
// FUNCTION: Recompute the dependences for all of the nodes in the tree  
//   rooted at 'wn_tree'.  The LS_IN_LOOP ls_loop' is derived for a loop  
//   enclosing 'wn' if there is one, otherwise it is NULL. 
//-----------------------------------------------------------------------

static void Recompute_Deps_For_Tree(WN* wn_tree, 
                                    LS_IN_LOOP* ls_loop,
                                    ARRAY_DIRECTED_GRAPH16* dg)         
{
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  if (WN_opcode(wn_tree) == OPC_DO_LOOP && Do_Depth(wn_tree) == 0) {
    LS_IN_LOOP* ls_loop_new = CXX_NEW(LS_IN_LOOP(wn_tree, dg, &LNO_local_pool, 
      TRUE), &LNO_local_pool);
    if (WN_opcode(wn_tree) == OPC_BLOCK) { 
      for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) 
	Recompute_Deps_For_Tree(wn, ls_loop_new, dg);
    } else { 
      for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
	Recompute_Deps_For_Tree(WN_kid(wn_tree, i), ls_loop_new, dg);
    } 
    CXX_DELETE(ls_loop_new, &LNO_local_pool); 
  }
  if (dg->Get_Vertex(wn_tree)) 
    if (Enclosing_Do_Loop(wn_tree) == NULL) 
      Delete_Deps(wn_tree, dg); 
    else 
      Recompute_Deps(wn_tree, ls_loop, dg);
  if (WN_opcode(wn_tree) == OPC_BLOCK) 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) 
      Recompute_Deps_For_Tree(wn, ls_loop, dg); 
  else 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Recompute_Deps_For_Tree(WN_kid(wn_tree, i), ls_loop, dg); 
} 

//-----------------------------------------------------------------------
// NAME: Insert_Sink_Code
// FUNCTION: Insert the sink code in 'wn_block' after the loop 'wn_loop'. 
//-----------------------------------------------------------------------

static void Insert_Sink_Code(WN* wn_block, 
			     WN* wn_loop)
{
  if (WN_first(wn_block) != NULL) { 
    WN* wn_first = WN_first(wn_block); 
    WN *wnn;
    for (wnn = wn_first; WN_next(wnn) != NULL; wnn = WN_next(wnn));
    for (WN* wn = wnn; wn != NULL; wn = wnn) {
      wnn = WN_prev(wn);
      LWN_Extract_From_Block(LWN_Get_Parent(wn), wn);
      LWN_Insert_Block_After(LWN_Get_Parent(wn_loop), wn_loop, wn);
    }
  }
  LWN_Delete_Tree(wn_block); 
}

//-----------------------------------------------------------------------
// NAME: Fix_Accesses_And_Deps 
// FUNCTION: Fix the access arrays and dependences for the sink code which 
//   was inserted after 'wn_loop' but before 'wn_after'.  
//-----------------------------------------------------------------------

static void Fix_Accesses_And_Deps(WN* wn_loop, 
				  WN* wn_after,
				  ARRAY_DIRECTED_GRAPH16* dg)
{
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  LS_IN_LOOP* ls_loop = NULL; 
  if (WN_next(wn_loop) != NULL) {
    WN* wn_encl_loop = Enclosing_Loop(WN_next(wn_loop)); 
    if (wn_encl_loop != NULL) 
      ls_loop = CXX_NEW(LS_IN_LOOP(wn_encl_loop, dg, &LNO_local_pool,
        TRUE), &LNO_local_pool);
  } 
  for (WN* wn = WN_next(wn_loop); wn != wn_after; wn = WN_next(wn)) {
    Generate_If_Accesses(wn);
    Recompute_Deps_For_Tree(wn, ls_loop, dg); 
  }
  if (ls_loop != NULL)
    CXX_DELETE(ls_loop, &LNO_local_pool);
} 

//-----------------------------------------------------------------------
// NAME: Sink_Out_Sandwiched_Statement 
// FUNCTION: Sink the sandwiched statement 'wn_stat' out of the loop 
//   'wn_sink_loop'. 
//-----------------------------------------------------------------------

extern void Sink_Out_Sandwiched_Statement(WN* wn_stat, 
					  WN* wn_sink_loop)
{
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  FmtAssert(WN_opcode(LWN_Get_Parent(wn_stat)) == OPC_BLOCK,
    ("Sink_Out_Sandwiched_Statement: First arg must be a statement"));
  DOLOOP_STACK do_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_stat, &do_stack); 
  INT i;
  for (i = 0; i < do_stack.Elements(); i++) 
    if (do_stack.Bottom_nth(i) == wn_sink_loop)
      break; 
  WN* wn_block = WN_CreateBlock(); 
  WN* wn_new_block = wn_block; 
  for (INT j = i; j < do_stack.Elements(); j++) {
    WN* wn_loop = do_stack.Bottom_nth(j); 
    wn_new_block = Generate_Guard_Test(wn_loop, wn_new_block, du); 
  }   
  LWN_Extract_From_Block(wn_stat);
  WN* wn_after = WN_next(wn_sink_loop);
  LWN_Insert_Block_After(wn_new_block, NULL, wn_stat);
  LWN_Insert_Block_After(LWN_Get_Parent(wn_sink_loop), wn_sink_loop, wn_block);
  Fix_Accesses_And_Deps(wn_sink_loop, wn_after, dg); 
}  

//-----------------------------------------------------------------------
// NAME: SNL_Sink_Out_Sandwiched_Statements 
// FUNCTION: Sink the sandwiched code out of the SNL whose outermost loop
//   is 'wn_loop' and which consists of 'nloops' loops.  If 'test_legality',
//   test for legality and sink only those statements which are proved to 
//   be sinkable.
//-----------------------------------------------------------------------

extern void SNL_Sink_Out_Sandwiched_Statements(WN* wn_loop,
					       INT nloops, 
					       BOOL test_legality, 
  				               ARRAY_DIRECTED_GRAPH16* dg,
				               DU_MANAGER* du)
{
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  if (wn_loop == NULL) 
    return; 
  WN* wn_block = WN_CreateBlock(); 
  WN* wn_after = WN_next(wn_loop);  
  INT outer_depth = Do_Loop_Depth(wn_loop); 
  INT inner_depth = outer_depth + nloops - 1; 
  Generate_Sink_Out_Code(wn_loop, wn_loop, inner_depth, wn_block, 
    test_legality, du);
  Simplify_Sink_Out_Code(wn_block);
  Insert_Sink_Code(wn_block, wn_loop); 
  Fix_Accesses_And_Deps(wn_loop, wn_after, dg); 
}

//-----------------------------------------------------------------------
// NAME: Hoist_And_Sink_For_Nested_Doacross 
// FUNCTION: Hoist and sink sandwiched code out of the nested doacross
//   loop 'wn_loop'. 
//-----------------------------------------------------------------------

extern void Hoist_And_Sink_For_Nested_Doacross(WN* wn_loop,
					       ARRAY_DIRECTED_GRAPH16* dg,
				               DU_MANAGER* du)
{
  FmtAssert(dg != NULL, ("Could not find dependence graph"));
  FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP, ("Not a do loop")); 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  FmtAssert(dli->Mp_Info != NULL, ("Not an MP loop"));
  FmtAssert(dli->Mp_Info->Nest_Index() == 0, 
    ("Not an outer nested doacross"));
  FmtAssert(dli->Mp_Info->Nest_Total() > 1, 
    ("Not a nested doacross"));
  Hoist_Out_Nested_Statements(wn_loop, wn_loop, du);
  INT nloops = dli->Mp_Info->Nest_Total();
  SNL_Sink_Out_Sandwiched_Statements(wn_loop, nloops, FALSE, dg, du);
}
