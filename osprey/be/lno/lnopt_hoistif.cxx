/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

//
// 	LNO Hoist If (Loop Idiom Recognition and Optimization)
//      ------------------------------------------------------
//
//
//      Identify the following loop idiom and remove redundant loops.
//         do i = lb, ub
//           if (i == winner && G(i))
//             F(i)
//           endif
//           if (i == winner2 && H(i))
//             I(i)
//           endif
//           .
//           . { any number of such if-statements }
//           .
//         enddo
//      Replace with:
//         if (winner >= lb && winner <= ub && G(winner))
//           F(winner)
//         endif
//         if (winner2 >= lb && winner2 <= ub && H(winner2))
//           I(winner2)
//         endif
//         .
//         . { The rest of the if-statements expanded similarly }
//         .
//      F, G, H and I are functions without side-effects.
//
//      We use a threshold (user-controlled) to decide when it is 
//      useful to do this optimization. The threshold refers to the 
//      number of if-statements that is contained inside the loop.
//      The threshold is 1 by default.
//
//      TODO: In the presence of multiple such if-statements, it is 
//            necessary to do dependence-analysis. Right now, we expand
//            them in the order of their appearance inside the loop.
//            Analysis is intricate because it involves if-statements
//            rather than plain statements. The run-time order of the 
//            statements may be non-deterministic at compile-time.

#include "defs.h"
#include "glob.h"
#include "wn.h"
#include "wn_map.h"
#include "lwn_util.h"
#include "ff_utils.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "opt_du.h"
#include "dep_graph.h"
#include "ir_reader.h"             // for fdump_tree

static MEM_POOL HoistIf_default_pool;	// Hoist If private mem pool

typedef STACK<WN*> STACK_OF_WN;

static BOOL debug_hoistif;

static void
HoistIf_Update_Use_List (WN* stmt, WN* tmp, WN* innerloop)
{
  // Get uses outside loop and update use_list
  // Get uses inside tmp and update use_list
  USE_LIST* use_list=Du_Mgr->Du_Get_Use(stmt);
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_use = node->Wn();
    if (!Wn_Is_Inside(wn_use, innerloop))
      Du_Mgr->Add_Def_Use(tmp, wn_use);
    else
      Du_Mgr->Add_Def_Use(tmp, tmp);
  }
}

static void
HoistIf_Copy_Def_Use (WN* stmt, WN* tmp, SYMBOL sym, WN* innerloop)
{
  if (WN_operator(stmt) == OPR_LDID) {
    SYMBOL currentsym = SYMBOL(stmt);
    if (currentsym != sym) {
      LWN_Copy_Def_Use(stmt, tmp, Du_Mgr);
      DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(tmp);
      if (def_list->Loop_stmt() == innerloop)
	def_list->Set_loop_stmt(NULL);
    }
  } else if (WN_operator(stmt) == OPR_BLOCK) {
    WN* kid_stmt = WN_first(stmt);
    WN* kid_tmp = WN_first(tmp);
    while(kid_stmt) {
      HoistIf_Copy_Def_Use(kid_stmt, kid_tmp, sym, innerloop); 
      kid_stmt = WN_next(kid_stmt);
      kid_tmp = WN_next(kid_tmp);
    }
  } else {
    if (WN_operator(stmt) == OPR_STID) {
      SYMBOL currentsym = SYMBOL(stmt);
      if (currentsym != sym)
	HoistIf_Update_Use_List(stmt, tmp, innerloop);
    }

    for (INT kid = 0; kid < WN_kid_count(stmt); kid ++)
      HoistIf_Copy_Def_Use(WN_kid(stmt, kid), WN_kid(tmp, kid), sym, 
			   innerloop);
  }
}

static BOOL
Is_Cmp_Eq_IV (WN* cmp, SYMBOL symindex) 
{
  if (WN_operator(cmp) == OPR_CAND) {
    WN* kid0 = WN_kid0(cmp);
    WN* kid1 = WN_kid1(cmp);

    if (Is_Cmp_Eq_IV(kid0, symindex) ||
	Is_Cmp_Eq_IV(kid1, symindex))
      return TRUE;
    
    return FALSE;
  } else if (WN_operator(cmp) == OPR_EQ) {
    WN* opnd0 = WN_kid0(cmp);
    WN* opnd1 = WN_kid1(cmp);
    
    if (WN_operator(opnd0) == OPR_LDID && WN_operator(opnd1) == OPR_LDID) {
      SYMBOL sym0 = SYMBOL(opnd0);
      SYMBOL sym1 = SYMBOL(opnd1);      
      if (symindex == sym0 || symindex == sym1)
	return TRUE;
    } else if ((WN_operator(opnd0) == OPR_SUB ||
		WN_operator(opnd0) == OPR_ADD) &&
	       WN_operator(opnd1) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd0);
      WN* tmp_opnd1 = WN_kid1(opnd0);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return FALSE;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (!found_index)
	return FALSE;      
      else
	return TRUE;
    } else if ((WN_operator(opnd1) == OPR_SUB ||
		WN_operator(opnd1) == OPR_ADD) &&
	       WN_operator(opnd0) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd1);
      WN* tmp_opnd1 = WN_kid1(opnd1);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return FALSE;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (!found_index)
	return FALSE;      
      else 
	return TRUE;
    } else {
      BOOL found_index = FALSE;
      if (WN_operator(opnd0) == OPR_LDID) {
	SYMBOL sym0 = SYMBOL(opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }
      if (WN_operator(opnd1) == OPR_LDID) {
	SYMBOL sym1 = SYMBOL(opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (!found_index)
	return FALSE;
      else
	return TRUE;
    }
  } 
  return FALSE;
}

static BOOL
compared_operand_has_index (WN *opnd, SYMBOL symindex)
{
  if (WN_operator(opnd) == OPR_LDID) {
    SYMBOL sym = SYMBOL(opnd);
    if (sym == symindex)
      return TRUE;

  } else
    for (INT kid = 0; kid < WN_kid_count(opnd); kid ++)
      if (compared_operand_has_index (WN_kid(opnd, kid), symindex))
	return TRUE;

  return FALSE;
}

static BOOL 
Is_HoistIf_Amenable (WN* stmt, WN* innerloop)
{
  if (WN_operator(stmt) != OPR_IF)
    return FALSE;

  if (WN_first(WN_kid2(stmt)) != NULL) // else block should be empty
    return FALSE;

  WN* kid0 = WN_kid0(stmt);
  if (WN_operator(kid0) != OPR_EQ &&
      WN_operator(kid0) != OPR_CAND)
    return FALSE;

  SYMBOL symindex = SYMBOL(WN_index(innerloop));
  if (WN_operator(kid0) == OPR_EQ) {
    WN* opnd0 = WN_kid0(kid0);
    WN* opnd1 = WN_kid1(kid0);
    
    if (WN_operator(opnd0) == OPR_LDID && WN_operator(opnd1) == OPR_LDID) {
      SYMBOL sym0 = SYMBOL(opnd0);
      SYMBOL sym1 = SYMBOL(opnd1);      
      if (symindex != sym0 && symindex != sym1)
	return FALSE;
    } else if ((WN_operator(opnd0) == OPR_SUB ||
		WN_operator(opnd0) == OPR_ADD) &&
	       WN_operator(opnd1) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd0);
      WN* tmp_opnd1 = WN_kid1(opnd0);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return FALSE;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
	if (found_index && compared_operand_has_index(tmp_opnd1, symindex))
	  return FALSE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
	if (found_index && compared_operand_has_index(tmp_opnd0, symindex))
	  return FALSE;
      }
      if (!found_index)
	return FALSE; 
    } else if ((WN_operator(opnd1) == OPR_SUB ||
		WN_operator(opnd1) == OPR_ADD) &&
	       WN_operator(opnd0) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd1);
      WN* tmp_opnd1 = WN_kid1(opnd1);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return FALSE;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
	if (found_index && compared_operand_has_index(tmp_opnd1, symindex))
	  return FALSE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
	if (found_index && compared_operand_has_index(tmp_opnd0, symindex))
	  return FALSE;
      }
      if (!found_index)
	return FALSE;            
    } else {
      BOOL found_index = FALSE;
      if (WN_operator(opnd0) == OPR_LDID) {
	SYMBOL sym0 = SYMBOL(opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
	if (found_index && compared_operand_has_index(opnd1, symindex))
	  return FALSE;
      }
      if (WN_operator(opnd1) == OPR_LDID) {
	SYMBOL sym1 = SYMBOL(opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
	if (found_index && compared_operand_has_index(opnd0, symindex))
	  return FALSE;
      }
      if (!found_index)
	return FALSE;
    }
  } else {
    // kid0 is OPR_CAND; traverse all kids to see which one is a OPR_EQ
    // that has a reference to loop index
    if (!Is_Cmp_Eq_IV(kid0, symindex))
      return FALSE;
  }

  return TRUE;
}

static void
HoistIf_Delete_Def_Use (WN *wn_tree, SYMBOL sym) 
{
  if (WN_operator(wn_tree) == OPR_LDID) {
    SYMBOL currentsym = SYMBOL(wn_tree);
    if (currentsym == sym)
      return;
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_tree);
    DEF_LIST_ITER iter(def_list);
    const DU_NODE *node = iter.First();
    Is_True(!iter.Is_Empty(),("Empty def list in HoistIf_Delete_Def_Use"));
    for(; !iter.Is_Empty();node=iter.Next()){
      WN *def = (WN *) node->Wn();
      Du_Mgr->Delete_Def_Use(def,wn_tree);
    }
  }
  for (INT i = 0; i < WN_kid_count(wn_tree); i++)
    HoistIf_Delete_Def_Use(WN_kid(wn_tree, i), sym);
}

// Assumes: loop bounds are of the form [lb, ub]
static void
Replace_Equality_Check(WN* stmt, WN* cmp, WN* cmp_value, WN* innerloop)
{
  WN* parent = LWN_Get_Parent(cmp);
  WN* lb = LWN_Copy_Tree(WN_kid0(WN_start(innerloop)), TRUE, LNO_Info_Map);
  WN* ub = LWN_Copy_Tree(WN_kid1(WN_end(innerloop)), TRUE, LNO_Info_Map);  
  WN* copy1 = LWN_Copy_Tree(cmp_value, TRUE, LNO_Info_Map);
  WN* copy2 = LWN_Copy_Tree(cmp_value, TRUE, LNO_Info_Map);
  INT kid;
  OPCODE op_cand = OPCODE_make_op(OPR_CAND, Boolean_type, MTYPE_V);
  TYPE_ID index_type = WN_rtype(WN_end(innerloop));
  OPCODE op_le = OPCODE_make_op(OPR_LE, index_type, index_type);
  OPCODE op_ge = OPCODE_make_op(OPR_GE, index_type, index_type);
  WN* opnd0; 
  WN* opnd1; 

  LWN_Copy_Def_Use(WN_kid0(WN_start(innerloop)), lb, Du_Mgr);
  LWN_Copy_Def_Use(WN_kid1(WN_end(innerloop)), ub, Du_Mgr);
  LWN_Copy_Def_Use(cmp_value, copy1, Du_Mgr);
  LWN_Copy_Def_Use(cmp_value, copy2, Du_Mgr);
  LWN_Copy_Linenumber(WN_kid0(WN_start(innerloop)), lb);
  LWN_Copy_Linenumber(WN_kid1(WN_end(innerloop)), ub);
  LWN_Copy_Linenumber(cmp_value, copy1);
  LWN_Copy_Linenumber(cmp_value, copy2);

  opnd0 = LWN_CreateExp2(op_le, copy1, ub);
  opnd1 = LWN_CreateExp2(op_ge, copy2, lb);

  for (kid = 0; kid < WN_kid_count(parent); kid ++)
    if (WN_kid(parent, kid) == cmp)
      break;

  WN_kid(parent, kid) = LWN_CreateExp2(op_cand, opnd0, opnd1);
  LWN_Parentize(WN_kid(parent, kid));
  LWN_Set_Parent(WN_kid(parent, kid), parent);
				       
  return;
}

static void 
HoistIf_Replace_Symbol (WN* node, SYMBOL sym, WN* ldid, WN* innerloop)
{
  if (WN_operator(node) == OPR_LDID) {
    SYMBOL currsym = SYMBOL(node);
    if (currsym == sym) {
      WN* parent = LWN_Get_Parent(node);
      INT kid;
      for (kid = 0; kid < WN_kid_count(parent); kid ++)
        if (WN_kid(parent, kid) == node)
          break;
      WN* copy_ldid = LWN_Copy_Tree(ldid, TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use(ldid, copy_ldid, Du_Mgr);
      LWN_Copy_Linenumber(ldid, copy_ldid);
      {
        SYMBOL symindex = SYMBOL(WN_index(innerloop));
        HoistIf_Delete_Def_Use(WN_kid(parent, kid), symindex);
      }
      WN_kid(parent, kid) = copy_ldid;
      LWN_Set_Parent(copy_ldid, parent);
    }
  } else if (WN_operator(node) == OPR_BLOCK) {
    WN* kid = WN_first(node);
    // Recurse
    while(kid) {
      HoistIf_Replace_Symbol(kid, sym, ldid, innerloop);
      kid = WN_next(kid);
    }
  }
  // recurse
  for (INT i = 0; i < WN_kid_count(node); i ++)
    HoistIf_Replace_Symbol(WN_kid(node, i), sym, ldid, innerloop);
}

static void
Replace_IV_Ref (WN* stmt, WN* cmp_value, WN* innerloop)
{
  // Traverse all nodes in stmt, and replace all references to loop index
  // by cmp_value.
  HoistIf_Replace_Symbol(stmt, WN_index(innerloop), cmp_value, innerloop);
}

static WN* 
Find_Compare_Value (WN* node, WN* innerloop)
{
  SYMBOL symindex = SYMBOL (WN_index(innerloop));
  WN* opnd0 = WN_kid0(node);
  WN* opnd1 = WN_kid1(node);
  
  if (WN_operator(opnd0) == OPR_LDID &&
      WN_operator(opnd1) == OPR_LDID) {
    SYMBOL sym0(opnd0);
    SYMBOL sym1(opnd1);
    
    if (sym0 == symindex)
      return opnd1;
    else if (sym1 == symindex)
      return opnd0;
    else
      return NULL;
  } else if ((WN_operator(opnd0) == OPR_SUB ||
	      WN_operator(opnd0) == OPR_ADD) &&
	     WN_operator(opnd1) == OPR_LDID) {
    WN* tmp_opnd0 = WN_kid0(opnd0);
    WN* tmp_opnd1 = WN_kid1(opnd0);
    SYMBOL sym0, sym1;
    
    FmtAssert(WN_operator(tmp_opnd0) == OPR_LDID ||
	      WN_operator(tmp_opnd1) == OPR_LDID,
	      ("Find_Compare_Value: Handle this"));
    if (WN_operator(tmp_opnd0) == OPR_LDID) {
      sym0 = SYMBOL(tmp_opnd0);
      if (sym0 == symindex) {
	WN* opnd1_tmp = LWN_Copy_Tree(opnd1, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(opnd1, opnd1_tmp, Du_Mgr);
	LWN_Copy_Linenumber(opnd1, opnd1_tmp);
	WN* tmp_opnd1_tmp = LWN_Copy_Tree(tmp_opnd1, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(tmp_opnd1, tmp_opnd1_tmp, Du_Mgr);
	LWN_Copy_Linenumber(tmp_opnd1, tmp_opnd1_tmp);
	if (WN_operator(opnd0) == OPR_SUB) {
	  OPCODE op_add = 
	    OPCODE_make_op(OPR_ADD, WN_rtype(opnd0), WN_desc(opnd0));
	  return LWN_CreateExp2(op_add, opnd1_tmp, tmp_opnd1_tmp);
	} 
	OPCODE op_sub = 
	  OPCODE_make_op(OPR_SUB, WN_rtype(opnd0), WN_desc(opnd0));
	return LWN_CreateExp2(op_sub, opnd1_tmp, tmp_opnd1_tmp);
      }
    }
    if (WN_operator(tmp_opnd1) == OPR_LDID) {
      sym1 = SYMBOL(tmp_opnd1);
      if (sym1 == symindex) {
	WN* opnd1_tmp = LWN_Copy_Tree(opnd1, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(opnd1, opnd1_tmp, Du_Mgr);
	LWN_Copy_Linenumber(opnd1, opnd1_tmp);
	WN* tmp_opnd0_tmp = LWN_Copy_Tree(tmp_opnd0, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(tmp_opnd0, tmp_opnd0_tmp, Du_Mgr);
	LWN_Copy_Linenumber(tmp_opnd0, tmp_opnd0_tmp);
	if (WN_operator(opnd0) == OPR_SUB) {
	  OPCODE op_add = 
	    OPCODE_make_op(OPR_ADD, WN_rtype(opnd0), WN_desc(opnd0));
	  return LWN_CreateExp2(op_add, opnd1_tmp, tmp_opnd0_tmp);
	} 
	OPCODE op_sub = 
	  OPCODE_make_op(OPR_SUB, WN_rtype(opnd0), WN_desc(opnd0));
	return LWN_CreateExp2(op_sub, opnd1_tmp, tmp_opnd0_tmp);
      }
    }
  } else if ((WN_operator(opnd1) == OPR_SUB ||
	      WN_operator(opnd1) == OPR_ADD) &&
	     WN_operator(opnd0) == OPR_LDID) {
    WN* tmp_opnd0 = WN_kid0(opnd1);
    WN* tmp_opnd1 = WN_kid1(opnd1);
    SYMBOL sym0, sym1;
    
    FmtAssert(WN_operator(tmp_opnd0) == OPR_LDID ||
	      WN_operator(tmp_opnd1) == OPR_LDID,
	      ("Find_Compare_Value: Handle this"));
    if (WN_operator(tmp_opnd0) == OPR_LDID) {
      sym0 = SYMBOL(tmp_opnd0);
      if (sym0 == symindex) {
	WN* opnd0_tmp = LWN_Copy_Tree(opnd0, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(opnd0, opnd0_tmp, Du_Mgr);
	LWN_Copy_Linenumber(opnd0, opnd0_tmp);
	WN* tmp_opnd1_tmp = LWN_Copy_Tree(tmp_opnd1, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(tmp_opnd1, tmp_opnd1_tmp, Du_Mgr);
	LWN_Copy_Linenumber(tmp_opnd1, tmp_opnd1_tmp);
	if (WN_operator(opnd1) == OPR_SUB) {
	  OPCODE op_add = 
	    OPCODE_make_op(OPR_ADD, WN_rtype(opnd1), WN_desc(opnd1));
	  return LWN_CreateExp2(op_add, opnd0_tmp, tmp_opnd1_tmp);
	} 
	OPCODE op_sub = 
	  OPCODE_make_op(OPR_SUB, WN_rtype(opnd1), WN_desc(opnd1));
	return LWN_CreateExp2(op_sub, opnd0_tmp, tmp_opnd1_tmp);
      }
    }
    if (WN_operator(tmp_opnd1) == OPR_LDID) {
      sym1 = SYMBOL(tmp_opnd1);
      if (sym1 == symindex) {
	WN* opnd0_tmp = LWN_Copy_Tree(opnd0, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(opnd0, opnd0_tmp, Du_Mgr);
	LWN_Copy_Linenumber(opnd0, opnd0_tmp);
	WN* tmp_opnd0_tmp = LWN_Copy_Tree(tmp_opnd0, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(tmp_opnd0, tmp_opnd0_tmp, Du_Mgr);
	LWN_Copy_Linenumber(tmp_opnd0, tmp_opnd0_tmp);
	if (WN_operator(opnd1) == OPR_SUB) {
	  OPCODE op_add = 
	    OPCODE_make_op(OPR_ADD, WN_rtype(opnd1), WN_desc(opnd1));
	  return LWN_CreateExp2(op_add, opnd0_tmp, tmp_opnd0_tmp);
	} 
	OPCODE op_sub = 
	  OPCODE_make_op(OPR_SUB, WN_rtype(opnd1), WN_desc(opnd1));
	return LWN_CreateExp2(op_sub, opnd0_tmp, tmp_opnd0_tmp);
      }
    }
  } else {
    if (WN_operator(opnd0) == OPR_LDID) {
      SYMBOL sym0 = SYMBOL(opnd0);
      if (sym0 == symindex)
	return opnd1;
    }
    if (WN_operator(opnd1) == OPR_LDID) {
      SYMBOL sym1 = SYMBOL(opnd1);
      if (sym1 == symindex)
	return opnd0;
    }
  }
  return NULL;
}

static WN* 
Find_Compare_IV_Recurse (WN* cmp, SYMBOL symindex)
{
  if (WN_operator(cmp) == OPR_CAND) {
    WN* kid0 = Find_Compare_IV_Recurse(WN_kid0(cmp), symindex);

    if (kid0)
      return kid0;
    else
      return Find_Compare_IV_Recurse(WN_kid1(cmp), symindex);
  } else if (WN_operator(cmp) == OPR_EQ) {
    WN* opnd0 = WN_kid0(cmp);
    WN* opnd1 = WN_kid1(cmp);
    
    if (WN_operator(opnd0) == OPR_LDID && WN_operator(opnd1) == OPR_LDID) {
      SYMBOL sym0 = SYMBOL(opnd0);
      SYMBOL sym1 = SYMBOL(opnd1);      
      if (symindex == sym0 || symindex == sym1)
	return cmp;
    } else if ((WN_operator(opnd0) == OPR_SUB ||
		WN_operator(opnd0) == OPR_ADD) &&
	       WN_operator(opnd1) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd0);
      WN* tmp_opnd1 = WN_kid1(opnd0);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return NULL;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (found_index)
	return cmp;      
    } else if ((WN_operator(opnd1) == OPR_SUB ||
		WN_operator(opnd1) == OPR_ADD) &&
	       WN_operator(opnd0) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd1);
      WN* tmp_opnd1 = WN_kid1(opnd1);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return NULL;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (found_index)
	return cmp;            
    } else {
      BOOL found_index = FALSE;
      if (WN_operator(opnd0) == OPR_LDID) {
	SYMBOL sym0 = SYMBOL(opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }
      if (WN_operator(opnd1) == OPR_LDID) {
	SYMBOL sym1 = SYMBOL(opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (found_index)
	return cmp;
    }
  }
  return NULL;
}

static WN*
Find_Compare_IV (WN* node, WN* innerloop)
{
  SYMBOL symindex = SYMBOL(WN_index(innerloop));

  if (WN_operator(node) == OPR_EQ) {
    WN* opnd0 = WN_kid0(node);
    WN* opnd1 = WN_kid1(node);

    if (WN_operator(opnd0) == OPR_LDID && WN_operator(opnd1) == OPR_LDID) {
      SYMBOL sym0 = SYMBOL(opnd0);
      SYMBOL sym1 = SYMBOL(opnd1);      
      if (symindex == sym0 || symindex == sym1)
	return node;
    } else if ((WN_operator(opnd0) == OPR_SUB ||
		WN_operator(opnd0) == OPR_ADD) &&
	       WN_operator(opnd1) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd0);
      WN* tmp_opnd1 = WN_kid1(opnd0);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return NULL;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (found_index)
	return node;      
    } else if ((WN_operator(opnd1) == OPR_SUB ||
		WN_operator(opnd1) == OPR_ADD) &&
	       WN_operator(opnd0) == OPR_LDID) {
      WN* tmp_opnd0 = WN_kid0(opnd1);
      WN* tmp_opnd1 = WN_kid1(opnd1);
      SYMBOL sym0, sym1;
      BOOL found_index = FALSE;

      if (WN_operator(tmp_opnd0) != OPR_LDID &&
	  WN_operator(tmp_opnd1) != OPR_LDID)
	return NULL;
      if (WN_operator(tmp_opnd0) == OPR_LDID) {
	sym0 = SYMBOL(tmp_opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }  
      if (WN_operator(tmp_opnd1) == OPR_LDID) {
	sym1 = SYMBOL(tmp_opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (found_index)
	return node;            
    } else {
      BOOL found_index = FALSE;
      if (WN_operator(opnd0) == OPR_LDID) {
	SYMBOL sym0 = SYMBOL(opnd0);
	if (sym0 == symindex)
	  found_index = TRUE;
      }
      if (WN_operator(opnd1) == OPR_LDID) {
	SYMBOL sym1 = SYMBOL(opnd1);
	if (sym1 == symindex)
	  found_index = TRUE;
      }
      if (found_index)
	return node;
    }
  } else {
    FmtAssert(WN_operator(node) == OPR_CAND, 
              ("LNO_HoistIf: Invalid node passed to Find_Compare_IV"));
    return Find_Compare_IV_Recurse(node, symindex);
  }
}

static void 
HoistIf_Optimize (WN* stmt, STACK_OF_WN *hoistifed_stmts, WN* innerloop)
{
  WN* tmp;
  WN* stmt_tmp1;
  WN* stmt_tmp2;
  WN* cmp;
  WN* cmp_value;

  tmp = LWN_Copy_Tree(stmt, TRUE, LNO_Info_Map);
  LWN_Copy_Linenumber(stmt, tmp);
  {
    SYMBOL sym = SYMBOL(WN_index(innerloop));
    HoistIf_Copy_Def_Use(stmt, tmp, sym, innerloop);
  }
  LWN_Parentize(tmp);

  FmtAssert(WN_operator(tmp) == OPR_IF, 
            ("LNO_HoistIf: Invalid statement passed to HoistIf_Optimize"));
  
  // First kid is the condition evaluation. It is of the form:
  //   cond0 && cond1 && cond2 ... condn
  FmtAssert(WN_operator(WN_kid0(tmp)) == OPR_CAND || 
  	    WN_operator(WN_kid0(tmp)) == OPR_EQ,
	    ("LNO_HoistIf: Invalid condition passed to HoistIf_Optimize"));
  
  // It is assumed that (only) one of the conditions evaluate:
  // <loop_index_variable> == <compared_value>
  // Find this type of condition in the first kid.
  cmp = Find_Compare_IV(WN_kid0(tmp), innerloop);
  FmtAssert(WN_operator(cmp) == OPR_EQ, 
            ("LNO_FLOW: Invalid condition passed to HoistIf_Optimize"));
  cmp_value = Find_Compare_Value(cmp, innerloop);
  
  // We need to replace conditions of this kind with:
  // <compared_value> >= lb && <compared_value> <= ub 
  Replace_Equality_Check(tmp, cmp, cmp_value, innerloop);
  
  // Also, we should replace all references to <loop_index_variable> to:
  // <compared_value>.
  Replace_IV_Ref(tmp, cmp_value, innerloop);

  {
    // delete old node def-use
    SYMBOL sym = SYMBOL(WN_index(innerloop));
    if (WN_operator(cmp_value) != OPR_LDID && 
	cmp_value != WN_kid0(cmp) && cmp_value != WN_kid1(cmp)) {
      // New node was created; delete the new node because copy
      // has already been made. 
      HoistIf_Delete_Def_Use(cmp_value, sym);
      LWN_Delete_Tree(cmp_value);
    }
    HoistIf_Delete_Def_Use(cmp, sym);
    LWN_Delete_Tree(cmp);    
  }

  LWN_Copy_Linenumber(innerloop, tmp);

  hoistifed_stmts->Push(tmp);

  return;
}

// Hoist If optimize loops
static INT 
HoistIf (WN* innerloop)
{  
  if (//!Do_Loop_Is_Good(innerloop) || 
      Do_Loop_Has_Calls(innerloop) || Do_Loop_Has_Gotos(innerloop)) {
    if (debug_hoistif) {
      printf("(%s:%d) ", 
	     Src_File_Name, 
	     Srcpos_To_Line(WN_Get_Linenum(innerloop)));
      printf("Loop has calls or Gotos. Not suitable for hoistif opt.\n");
    }
    Is_True(0, ("Bad loop passed to HoistIf().\n"));
    return 0;
  }
  if (!Do_Loop_Is_Inner(innerloop)) {
    if (debug_hoistif) {
      printf("(%s:%d) ", 
	     Src_File_Name, 
	     Srcpos_To_Line(WN_Get_Linenum(innerloop)));
      printf("Loop is not innermost. Loop was not vectorized.\n");
    }
    Is_True(0, ("Non-innermost loop passed to HoistIf().\n"));
    return 0;
  }  
 
  Upper_Bound_Standardize(WN_end(innerloop), TRUE);

  WN* stmt;
  UINT stmt_count=0;
  WN* body=WN_do_body(innerloop);

  for (stmt = WN_first(body); stmt; stmt = WN_next(stmt)) {
    if (!Is_HoistIf_Amenable(stmt, innerloop)) {
      if (debug_hoistif) {
	printf("(%s:%d) ", 
	       Src_File_Name,
	       Srcpos_To_Line(WN_Get_Linenum(innerloop)));
	printf("Loop is not amenable to hoistif optimization.\n");
      }
      return 0;
    }
    stmt_count ++;
    if (stmt_count > LNO_HoistIf_Threshold) {
      if (debug_hoistif) {
	printf("(%s:%d) ", 
	       Src_File_Name,
	       Srcpos_To_Line(WN_Get_Linenum(innerloop)));
	printf("Loop has too many stmts for Hoist If opt.\n");
      }      
      return 0;
    }
  }

  // if the loop index var is live at exit and cannot be finalized,
  // we will not do hoist if optimization.
  if (Index_Variable_Live_At_Exit(innerloop)) {
  
    if (Upper_Bound_Standardize(WN_end(innerloop),TRUE)==FALSE) {
      if (debug_hoistif) {
	printf("(%s:%d) ", 
	       Src_File_Name, 
	       Srcpos_To_Line(WN_Get_Linenum(innerloop)));
	printf("Loop upper bound can not be std. ");
	printf("Loop is not hoist if optimized.\n");
      }
      return 0;
    }
    Finalize_Index_Variable(innerloop,FALSE);
    scalar_rename(WN_start(innerloop));
  }  

  // At this point, all loop statements are Hoist If optimizable.
  // Assume loop index variable is 'i', loop bounds [lb, ub].
  // Note the bounds are inclusive.
  // Each statement is of the form:
  //   if (i == <var> && ... g(i) )
  //      = f(i); 
  //   // f and g are functions of i.
  // Tranform to:
  //   if (<var> >= lb &&
  //       <var> <= ub && ... g(<var>) )
  //      = f(<var>)
  STACK_OF_WN *hoistif_stmts =
    CXX_NEW(STACK_OF_WN(&HoistIf_default_pool),
	    &HoistIf_default_pool);
  for (stmt = WN_first(body); stmt; stmt = WN_next(stmt)) {
    HoistIf_Optimize (stmt, hoistif_stmts, innerloop);
  }  
  
  // Now, pop each statement and stick it before this loop.
  if (hoistif_stmts->Elements() == 0) {
    if (debug_hoistif) {
      printf("(%s:%d) ", 
	     Src_File_Name,
	     Srcpos_To_Line(WN_Get_Linenum(innerloop)));
      printf("Loop has no Hoist If opt stmt.\n");
    }      
    return 0;
  }
  for (INT i=hoistif_stmts->Elements()-1; i >= 0; i--) {
    WN* stmt = hoistif_stmts->Top_nth(i);
    
    LWN_Insert_Block_Before(LWN_Get_Parent(innerloop), 
			    innerloop, stmt);
    LWN_Parentize(stmt);
    LWN_Set_Parent(stmt, LWN_Get_Parent(innerloop));
  }
  if (debug_hoistif) {
    printf("(%s:%d) ", 
      Src_File_Name,
      Srcpos_To_Line(WN_Get_Linenum(innerloop)));
      printf("Hoist If succeeded on loop.\n");
    }      
  return 1;
}

static void HoistIf_Walk(WN* wn) {
  OPCODE opc=WN_opcode(wn);

  if (!OPCODE_is_scf(opc)) 
    return;
  else if (opc==OPC_DO_LOOP) {
    if (//Do_Loop_Is_Good(wn) && 
	Do_Loop_Is_Inner(wn) && !Do_Loop_Has_Calls(wn)
	&& !Do_Loop_Is_Mp(wn) && !Do_Loop_Has_Gotos(wn)) {
      if (HoistIf(wn)) {
	WN* parent_loop = LWN_Get_Parent(wn);
	if (WN_opcode(parent_loop) == OPC_BLOCK)
	  parent_loop = LWN_Get_Parent(parent_loop);
	if (WN_opcode(parent_loop) == OPC_DO_LOOP) {
	  // Update Do loop info of parent is required.
	  BOOL parent_has_another_inner_loop = FALSE;
	  for (WN* tmp = WN_first(WN_do_body(parent_loop)); 
	       tmp; tmp = WN_next(tmp)) {
	    if (WN_opcode(tmp) == OPC_DO_LOOP && tmp != wn) {
	      parent_has_another_inner_loop = TRUE;
	      break;
	    }
	  }
	  if (!parent_has_another_inner_loop) {
	    DO_LOOP_INFO* dli = Get_Do_Loop_Info(parent_loop);
	    dli->Is_Inner = TRUE;
	  }
	}
        LWN_Delete_Tree(wn);
      }
    } else
      HoistIf_Walk(WN_do_body(wn));
  } else if (opc==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      HoistIf_Walk(stmt);
      stmt=next_stmt;
    }
  else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      HoistIf_Walk(WN_kid(wn,kidno));
    }
}

void HoistIf_Phase(WN* func_nd) {

  MEM_POOL_Initialize(&HoistIf_default_pool,"HoistIf_default_pool",FALSE);
  MEM_POOL_Push(&HoistIf_default_pool);
  
  debug_hoistif = Get_Trace(TP_LNOPT, TT_LNO_DEBUG_HOISTIF);
  if (debug_hoistif) {
    fprintf(TFile, "=======================================================================\n");
    fprintf(TFile, "LNO: \"WHIRL tree before hoist if phase\"\n");
    fdump_tree (TFile, func_nd);
  }
  HoistIf_Walk(func_nd);
  if (debug_hoistif) {
    fprintf(TFile, "=======================================================================\n");
    fprintf(TFile, "LNO: \"WHIRL tree after hoist if phase\"\n");
    fdump_tree (TFile, func_nd);
  }

  MEM_POOL_Pop(&HoistIf_default_pool);
  MEM_POOL_Delete(&HoistIf_default_pool);

}
