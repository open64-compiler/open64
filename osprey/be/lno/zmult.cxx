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


//                     Zero Multiplication
//                     -------------------
//
// Description:
//
// Given loops like
//
//	do i
//	  if (x == 0) 
//	    do j
//	      ... += x*
//
// we get rid of the if statement
//
//	This is useful because 
//	  1) It allows us to block the i loop
//
//	This is harmful if x frequently equals zero, but it's necessary to
//	get good linpack performance
//	(i.e. we admit this is our concession to bad sportsmanship)
//
/* ====================================================================
 * ====================================================================
 *
 * Module: zmult.cxx
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Zero multiplication
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source$ $Revision$";

#include <sys/types.h>
#include "lnopt_main.h"
#include "dep_graph.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "reduc.h"
#include "config.h"
#include "targ_const.h"

static BOOL Process_If(WN *if_wn, ARRAY_DIRECTED_GRAPH16 *dep_graph);
static BOOL Is_Zero(WN *wn);
static BOOL Equivalent_Load(WN *wn1, WN *wn2, WN *statement,
	ARRAY_DIRECTED_GRAPH16 *dep_graph);
static WN *Get_Single_Real_Statement(WN *block);
static BOOL Block_Is_Empty(WN *block);

void Eliminate_Zero_Mult(WN *wn, ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  Is_True(Eager_Level >= 4,("Eliminate_Zero_Mult causes speculation "));
  OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      Eliminate_Zero_Mult(kid,dep_graph);
      kid = WN_next(kid);
    }
    return;
  }

  if (opcode == OPC_IF) {
    if (Process_If(wn,dep_graph)) {
      return;
    }
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    Eliminate_Zero_Mult(kid,dep_graph);
  }
}

// Process a single if
// return TRUE if we got rid of it
static BOOL Process_If(WN *if_wn, ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  WN *parent = LWN_Get_Parent(if_wn);

  // only worth while if there is an outer do loop 
  if ((WN_opcode(parent) != OPC_BLOCK) ||
      (WN_opcode(LWN_Get_Parent(parent)) != OPC_DO_LOOP)) {
    return FALSE;
  }
  if (!Do_Loop_Is_Good(LWN_Get_Parent(parent)) || 
	Do_Loop_Has_Gotos(LWN_Get_Parent(parent))) {
    return FALSE;
  }

  WN *test = WN_if_test(if_wn);
  OPERATOR oper = WN_operator(test);
  WN *multiplier = NULL;
  WN *loop = NULL;

  if (oper == OPR_NE) {
    if (Is_Zero(WN_kid0(test))) {
      multiplier = WN_kid1(test);
    } else if (Is_Zero(WN_kid1(test))) {
      multiplier = WN_kid0(test);
    } else {
      return FALSE;  // not comparing with zero
    }

    // else must be empty, then must contain a loop and nothing else
    if (!Block_Is_Empty(WN_else(if_wn))) {
      return FALSE;
    }
    loop = Get_Single_Real_Statement(WN_then(if_wn));
    if (!loop || WN_opcode(loop) != OPC_DO_LOOP) {
      return FALSE;
    }
    if (!Do_Loop_Is_Good(loop) || Do_Loop_Has_Gotos(loop)) {
      return FALSE;
    }
  } else if (oper == OPR_EQ) {
    if (Is_Zero(WN_kid0(test))) {
      multiplier = WN_kid1(test);
    } else if (Is_Zero(WN_kid1(test))) {
      multiplier = WN_kid0(test);
    } else {
      return FALSE;  // not comparing with zero
    }
    // then must be empty, else must contain a loop and nothing else
    if (!Block_Is_Empty(WN_then(if_wn))) {
      return FALSE;
    }
    loop = Get_Single_Real_Statement(WN_else(if_wn));
    if (!loop || WN_opcode(loop) != OPC_DO_LOOP) {
      return FALSE;
    }
    if (!Do_Loop_Is_Good(loop) || Do_Loop_Has_Gotos(loop)) {
      return FALSE;
    }
  } else {
    return FALSE;
  }

  // at this point, we know we're comparing multiplier to zero outside of loop

  // check that multiplier is a single load
  if (!OPCODE_is_load(WN_opcode(multiplier))) {
    return FALSE;
  }

  // check that the do loop has one statement, and it's an addition 
  // redution
  WN *body = WN_do_body(loop);
  WN *statement = Get_Single_Real_Statement(body);
  if (!statement || !OPCODE_is_store(WN_opcode(statement))) {
    return FALSE;
  }

  if (!red_manager || (red_manager->Which_Reduction(statement) != RED_ADD)) {
   return FALSE;
  }

  WN *add = WN_kid0(statement);
  Is_True((WN_operator(add) == OPR_ADD) ||
          (WN_operator(add) == OPR_SUB),
	  ("Non add in Process_If"));
  WN *loop_mult = NULL;
  if (WN_operator(WN_kid0(add)) == OPR_MPY) {
    loop_mult = WN_kid0(add);
  } else if (WN_operator(WN_kid1(add)) == OPR_MPY) {
    loop_mult = WN_kid1(add);
  } else {
    return FALSE;
  }

  if (Equivalent_Load(multiplier,WN_kid0(loop_mult),statement,dep_graph) ||
      Equivalent_Load(multiplier,WN_kid1(loop_mult),statement,dep_graph)) {
    // Remove the if
    LWN_Insert_Block_Before(parent,if_wn,LWN_Extract_From_Block(loop));
    LWN_Delete_Tree(if_wn);
    return TRUE;
  }
  return FALSE;
}


// Does this instruction represent the constant zero
static BOOL Is_Zero(WN *wn)
{
  OPERATOR oper = WN_operator(wn);
  if (oper == OPR_INTCONST) {
    return (WN_const_val(wn) == 0);
  } else if (oper == OPR_CONST) {
    TCON t = STC_val(WN_st(wn));
    switch(TCON_ty(t)) {
      case MTYPE_F4:
#ifdef TCON_R4_IS_DOUBLE
	return t.vals.dval == 0;
#else
	return t.vals.fval == 0;
#endif
      case MTYPE_F8:
	return t.vals.dval == 0;
      default:
	return FALSE;
    }
  }
  return FALSE;
}

// Are these two wns loads to the same location,
// always conservative to say no
// It's a given that wn1 is directly outside a loop containing wn2
// and that the only statement in the loop is statement
static BOOL Equivalent_Load(WN *wn1, WN *wn2, WN *statement,
	ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  OPCODE opcode1 = WN_opcode(wn1);
  OPCODE opcode2 = WN_opcode(wn2);

  if (opcode1 != opcode2) {
    return FALSE;
  }
  if (!OPCODE_is_load(opcode1)) {
    return FALSE;
  }

  if (OPCODE_operator(opcode1) == OPR_LDA) {
    return ((WN_offset(wn1) == WN_offset(wn2)) &&
	    (ST_base(WN_st(wn1)) == ST_base(WN_st(wn2))) &&
	    (ST_ofst(WN_st(wn1)) == ST_ofst(WN_st(wn2))));
  } else if (OPCODE_operator(opcode1) == OPR_LDID) {
    if ((WN_offset(wn1) != WN_offset(wn2)) ||
	    (ST_base(WN_st(wn1)) != ST_base(WN_st(wn2))) ||
	    (ST_ofst(WN_st(wn1)) != ST_ofst(WN_st(wn2)))) {
      return FALSE;
    }
    // make sure wn2 doesn't vary inside the region of wn1
    // it only can if it's dependent on statement
    DEF_LIST *defs = Du_Mgr->Ud_Get_Def(wn2);
    DEF_LIST_ITER iter(defs);
    if (defs->Incomplete()) return FALSE;
    for(const DU_NODE *node=iter.First();!iter.Is_Empty();node=iter.Next()){
      WN *def = (WN *) node->Wn();
      if (def == statement) {
	return FALSE;
      }
    }
  } else if (OPCODE_operator(opcode1) == OPR_ILOAD) {
    WN *array1 = WN_kid0(wn1);
    WN *array2 = WN_kid0(wn2);

    // are they the same reference
    ACCESS_ARRAY *aa1 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array1);
    ACCESS_ARRAY *aa2 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array2);
    if (!aa1 || !aa2) {
      return FALSE;
    }
    if (!(*aa1 == *aa2)) {
      return FALSE;
    }

    VINDEX16 v1 = dep_graph->Get_Vertex(wn1);
    VINDEX16 v2 = dep_graph->Get_Vertex(wn2);
    if (!v1 || !v2) {
      return FALSE;
    }

    // does the reference vary in the loop (the only way it can
    // is if the single statement in the loop is a dependent store)
    VINDEX16 store_v = dep_graph->Get_Vertex(statement);
    if (store_v && dep_graph->Get_Edge(store_v,v2)) {
      return FALSE;
    }
  } else {
    return FALSE;
  }
  return TRUE;
}

  
// If this block has one non-useless statement, return it
// If it has none or more than one, return NULL
static WN *Get_Single_Real_Statement(WN *block)
{
  INT count = 0;
  WN *statement = NULL;
  WN *tmp = WN_first(block);
  while (tmp) {
    if (!OPCODE_is_not_executable(WN_opcode(tmp))) {
      count++;
      if (count > 1) {
	return NULL;
      }
      statement = tmp;
    }
    tmp = WN_next(tmp);
  }
  return (statement);
}

// return TRUE if this block is empty (all its opcodes are not executable
static BOOL Block_Is_Empty(WN *block)
{
  WN *tmp = WN_first(block);
  while (tmp) {
    if (!OPCODE_is_not_executable(WN_opcode(tmp))) {
      return FALSE;
    }
    tmp = WN_next(tmp);
  }
  return TRUE;
}

