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


//-*-c++-*-
//                     Reductions
//                     ----------
//
// Description:
//
//	Mark reductions.  We use an algorithm similar to SUIF's and Polaris's.
//	We look at each store in the program.  It is a reduction if
//	it's of the form a(x) = a(x) op ..., where op is a reducion operation,
//	x is anything or nothing and nothing in 'x' aliases 'a' (although 
//	'a' may be aliased with anything in '...').
//
//	If it is a reduction, we annotation both the load and the store
//	with the type.
//

/* ====================================================================
 * ====================================================================
 *
 * Module: redic.cxx
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:15-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.reduc.cxx $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Model time taken by inner loops
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.reduc.cxx $ $Revision: 1.5 $";

#include <sys/types.h>
#include "reduc.h"
#include "stab.h"
#include "lnoutils.h"
#include "lwn_util.h"
#include "lnopt_main.h"

OPERATOR REDUCTION_TYPE_to_OPERATOR(REDUCTION_TYPE red_type)
{
  switch (red_type) {
  case RED_ADD:
    return OPR_ADD;
  case RED_MPY:
    return OPR_MPY;
  case RED_MIN:
    return OPR_MIN;
  case RED_MAX:
    return OPR_MAX;
  case RED_NONE:
    break;
  default:
    Fail_FmtAssertion("REDUCTION_TYPE_to_OPERATOR: bogus red_type %d",
                      red_type);
  }
  return OPERATOR_UNKNOWN;
}

// Find all the stores
void REDUCTION_MANAGER::Build(WN *wn,BOOL build_scalar, BOOL build_array,
			class ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  _build_scalar = build_scalar;
  _build_array = build_array;
  Is_True((!build_array) || dep_graph,
	("Null dep_graph in REDUCTION_MANAGER::Build"));
  _dep_graph = dep_graph;
  Build(wn);
}

void REDUCTION_MANAGER::Build(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);

  if (OPCODE_is_store(opcode)) {
    OPERATOR oper = OPCODE_operator(opcode);
    if ((_build_scalar && (oper == OPR_STID)) ||
	(_build_array && (oper != OPR_STID))) {
      Check_Store(wn);
    }
  } else if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Build(kid);
      kid = WN_next(kid);
    }
  } else if (OPCODE_is_scf(opcode)) {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      Build(kid);
    }
  }
}

// Process a store
void REDUCTION_MANAGER::Check_Store(WN *store)
{
  // first check that the operation is a reduction type
  REDUCTION_TYPE type = RED_NONE;

  WN *rhs = WN_kid0(store);
  OPERATOR oper = WN_operator(rhs);

  switch(oper) {
    case OPR_ADD: case OPR_SUB:
      type = RED_ADD;
      break;
    case OPR_MPY: 
#ifdef KEY
    case OPR_DIV:
#endif
      type = RED_MPY;
      break;
    case OPR_MAX:
      type = RED_MAX;
      break;
    case OPR_MIN:
      type = RED_MIN;
      break;
    default: return; // not a reduction operator
  }

  // next check that the lhs is equivalent to one of the children of the 
  // right hand side (if it's a minus, it has to be equivalent to the first
  // child of the right hand side)

  WN *match_ld; // the wn of the kid that matches the store

  match_ld = Find_Match(store,WN_opcode(rhs),rhs);
  if (match_ld)  {
    if (!Self_Dependent_Store(store)) {
      WN_MAP32_Set(_map,store,(INT32) type);
      WN_MAP32_Set(_map,match_ld,(INT32) type);
    }
  }
}

// Find a load to the same location as store, keep looking as deep as
// possible
WN *REDUCTION_MANAGER::Find_Match(WN *store,OPCODE rhs_opcode, WN *rhs) {
  WN *kid0 = WN_kid0(rhs);

#ifndef KEY
  if (OPCODE_operator(rhs_opcode) == OPR_SUB) {
#else
  if (OPCODE_operator(rhs_opcode) == OPR_SUB || 
      OPCODE_operator(rhs_opcode) == OPR_DIV) {
#endif
    if (Opcode_Match(WN_opcode(kid0), rhs_opcode)) { // recurse
      return Find_Match(store,rhs_opcode,kid0);
    } else if (Match(store,kid0)) {
      return kid0;
    } else {
      return NULL;
    }
  } else {
    if (Opcode_Match(WN_opcode(kid0), rhs_opcode)) { // recurse
      WN *result = Find_Match(store,rhs_opcode,kid0);
      if (result) {  // found a match
	return result;
      }
    }
    if (Match(store,kid0)) {
      return kid0;
    }
    WN *kid1 = WN_kid1(rhs);
    if (Opcode_Match(WN_opcode(kid1), rhs_opcode)) { // recurse
      WN *result = Find_Match(store,rhs_opcode,kid1);
      if (result) {  // found a match
	return result;
      }
    }
    if (Match(store,kid1)) {
      return kid1;
    }
  }
  return NULL;
}


// is value a load to the same location we're storing in store
BOOL REDUCTION_MANAGER::Match(WN *store, WN *value) const
{
  OPERATOR st_oper = WN_operator(store);
  OPERATOR value_oper = WN_operator(value);

  if (st_oper == OPR_STID) {  // value has to be an ldid of the same loc
    return((value_oper == OPR_LDID) && 
	   (WN_offset(store) == WN_offset(value)) &&
	   (ST_base(WN_st(store)) == ST_base(WN_st(value))) &&
	   (ST_ofst(WN_st(store)) == ST_ofst(WN_st(value))));
  } else if (st_oper == OPR_ISTORE) {
    return((value_oper == OPR_ILOAD) && 
	   (WN_offset(store) == WN_offset(value)) &&
	   Equiv(WN_kid1(store),WN_kid0(value)));
  } else {
    return (FALSE); // we don't do the other types of stores for now
  }
}

// are the two expressions subtrees equivalent
BOOL REDUCTION_MANAGER::Equiv(WN *wn1, WN *wn2) const
{
  if (!WN_Equiv(wn1,wn2)) return(FALSE);
  for (INT kidno=0; kidno<WN_kid_count(wn1); kidno++) {
    if (!Equiv(WN_kid(wn1,kidno),WN_kid(wn2,kidno))) {
      return(FALSE);
    }
  }
  return(TRUE);
}

// is the store self dependent
// i.e. is it something like a[a[i]] = ...
BOOL REDUCTION_MANAGER::Self_Dependent_Store(WN *store) const
{
  OPERATOR oper = WN_operator(store);
  if (oper == OPR_STID) {
    return(FALSE);
  } else if (oper == OPR_ISTORE) {
    if (Unmapped_Vertices(WN_kid1(store))) {
      return(TRUE); // possilby self dependent
    }
    VINDEX16 store_vertex = _dep_graph->Get_Vertex(store);
    if (!store_vertex) {
      return(TRUE);
    }
    EINDEX16 e = _dep_graph->Get_In_Edge(store_vertex);
    while (e) {
      WN *wn = _dep_graph->Get_Wn( _dep_graph->Get_Source(e));
      if ((wn != store) && Is_Descendent_Of_Store_Address(store,wn)) {
	return(TRUE);
      }
      e = _dep_graph->Get_Next_In_Edge(e);
    }
    e = _dep_graph->Get_Out_Edge(store_vertex);
    while (e) {
      WN *wn = _dep_graph->Get_Wn( _dep_graph->Get_Sink(e));
      if ((wn != store) && Is_Descendent_Of_Store_Address(store,wn)) {
	return(TRUE);
      }
      e = _dep_graph->Get_Next_Out_Edge(e);
    }
  } else {
    return(TRUE);
  }
  return(FALSE);
}

// are there any loads here that aren't mapped in the dependence graph
BOOL REDUCTION_MANAGER::Unmapped_Vertices(WN *wn) const
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_load(opcode) && (OPCODE_operator(opcode) != OPR_LDID)) {
    if (!_dep_graph->Get_Vertex(wn)) {
      return(TRUE);
    }
  } 
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    if (Unmapped_Vertices(kid)) {
      return(TRUE);
    }
  }
  return(FALSE);
}

// Is wn a descendant of the store's address
BOOL REDUCTION_MANAGER::Is_Descendent_Of_Store_Address(WN *store,WN *wn) const
{
  while (OPCODE_is_expression(WN_opcode(wn))) {
    if (wn == WN_kid1(store)) {
      return(TRUE);
    }
    wn = LWN_Get_Parent(wn);
  }
  return FALSE;
}

void REDUCTION_MANAGER::Erase_Node(WN* wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_store(opcode) || OPCODE_is_load(opcode)) {
    if (Which_Reduction(wn) != RED_NONE) {
      WN_MAP32_Set(_map,wn,(INT32) RED_NONE);
    }
  }
}

// Erase all the reductions in the tree rooted at wn
void REDUCTION_MANAGER::Erase(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Erase(kid);
      kid = WN_next(kid);
    }
    return;
  } else {
    Erase_Node(wn);
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Erase(WN_kid(wn,kidno));
    }
  }
}

void REDUCTION_MANAGER::Unroll_Update_Rec(WN **bodies, UINT u)
{
  if (bodies[0]) {
    OPCODE	opc = WN_opcode(bodies[0]);
    OPERATOR	opr = OPCODE_operator(opc);

    if (OPCODE_is_store(opc) || OPCODE_is_load(opc)) {
      REDUCTION_TYPE type = Which_Reduction(bodies[0]);
      if (type != RED_NONE) {
	for (INT i=1; i<u; i++) {
	  WN_MAP32_Set(_map,bodies[i],(INT32) type);
	}
      }
    }

    if (opr == OPR_BLOCK) {
      WN **new_bodies = CXX_NEW_ARRAY(WN *,u,&LNO_local_pool);
      for (INT i=0; i<u; i++) {
        new_bodies[i] = WN_first(bodies[i]);
      }
      while (new_bodies[0]) {
        Unroll_Update_Rec(new_bodies, u);
        for (INT i=0; i<u; i++) {
          new_bodies[i] = WN_next(new_bodies[i]);
        }
      }
    } else if (WN_kid_count(bodies[0])) {
      WN **new_bodies = CXX_NEW_ARRAY(WN *,u,&LNO_local_pool);
      for (INT kidno=0; kidno<WN_kid_count(bodies[0]); kidno++) {
        for (INT i=0; i<u; i++) {
          new_bodies[i] = WN_kid(bodies[i],kidno);
        }
        Unroll_Update_Rec(new_bodies, u);
      }
    }
  }
}

void REDUCTION_MANAGER::Unroll_Update(WN **bodies, UINT u)
{
  MEM_POOL_Push(&LNO_local_pool);
  Unroll_Update_Rec(bodies,u);
  MEM_POOL_Pop(&LNO_local_pool);
}


