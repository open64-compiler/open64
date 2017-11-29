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
//     Routines to mark reduction operators.  Given a statement
//     'a(x) = a(x) + ...', where x can be anything.  We map the annotation
//     number for '+' on both the load and store.  These routines are based
//     on the SUIF and Polaris papers.
//
//     These routines assume the dependence graph and def-use chains are good.
//
//
//
// Exported types and functions:
//
//	enum REDUCTION_TYPE 
//
//		RED_NONE: not a reduction
//		RED_ADD: an addition (or subtraction) reduction
//		RED_MPY: a multiplication reduction
//		RED_MIN: a minimization reduction
//	        RED_MAX: a maximiation redution
//
//	REDUCTION_MANAGER
//
//	    REDUCTION_MANAGER(MEM_POOL *pool)
//	
//		Manage the reductions.  
//
//	    ~REDUCTION_MANAGER()
//
//	    REDUCTION_TYPE Which_Reduction(WN *wn)
//
//		Given that wn is a load or a store.  Is it a reduction load
//		or store, and if so, which type?
//
//	    void Build(WN *wn, BOOL build_scalar, BOOL build_array,
//			ARRAY_DIRECTED_GRAPH16 *dep_graph=0)
//
//		Mark all the reductions in the tree rooted at wn.
//		If build_scalar, mark scalar reductions (STID)
//		If build_array, mark array reductions
//		It is possible to bulid both
//		If build_array, dep_graph must point to the array dependence
//		graph
//
//	    void Unroll_Update(WN **bodies, UINT u)
//
//	        Fix the reduction manager after unrolling.
//
//	    void Add_Reduction(WN *wn, REDUCTION_TYPE red_type) 
//
//		Add a reduction for 'wn' of type 'red_type' to the 
//  		reduction manager. 
//
//	    void Erase(WN *wn)
//
//		Erase all the reductions in the tree rooted at wn
//


/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef RED_RCS_ID
#define RED_RCS_ID
#ifdef _KEEP_RCS_ID
static char *reduc_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */
#endif

#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef cxx_memory_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef opcode_INCLUDED
#include "opcode.h"
#endif
#ifndef reduction_INCLUDED
#include "reduction.h"
#endif

#ifndef REDUC_DECLARE
#define REDUC_DECLARE

extern OPERATOR REDUCTION_TYPE_to_OPERATOR(REDUCTION_TYPE red_type);

class REDUCTION_MANAGER {
  WN_MAP _map;
  MEM_POOL *_pool;
  class ARRAY_DIRECTED_GRAPH16 *_dep_graph;
  BOOL _build_scalar;
  BOOL _build_array;
public:
  REDUCTION_MANAGER(MEM_POOL *pool) {
    _dep_graph = NULL;
    _pool = pool;
    _map = WN_MAP32_Create(_pool);
    FmtAssert(_map != -1,("Ran out of mappings in REDUCTION_MANAGER"));
  }
  ~REDUCTION_MANAGER() {
    WN_MAP_Delete(_map);
  }

  REDUCTION_TYPE Which_Reduction(WN *wn) { 
    return (REDUCTION_TYPE) WN_MAP32_Get(_map,wn); 
  };
  void Build(WN *wn, BOOL build_scalar, BOOL build_array,
			class ARRAY_DIRECTED_GRAPH16 *dep_graph=0);
  void Erase(WN *wn);
  void Erase_Node(WN *wn);
  void Unroll_Update(WN **bodies, UINT u);
  void Unroll_Update_Rec(WN **bodies, UINT u);
  void Add_Reduction(WN* wn, REDUCTION_TYPE red_type) {
    WN_MAP32_Set(_map, wn, red_type); 
  }; 
private:
  BOOL Opcode_Match(OPCODE op1, OPCODE op2) {
    if (op1 == op2) return TRUE;
    if (OPCODE_rtype(op1) != OPCODE_rtype(op2)) return FALSE;
    if (OPCODE_desc(op1) != OPCODE_desc(op2)) return FALSE;
    OPERATOR opr1 = OPCODE_operator(op1);
    OPERATOR opr2 = OPCODE_operator(op2);
    if ((opr1 == OPR_ADD) && (opr2 == OPR_SUB)) return TRUE;
    if ((opr2 == OPR_ADD) && (opr1 == OPR_SUB)) return TRUE;
    return FALSE;
  }
  void Build(WN *wn);
  void Check_Store(WN *store);
  BOOL Match(WN *store, WN *value) const;
  BOOL Equiv(WN *wn1, WN *wn2) const;
  BOOL Self_Dependent_Store(WN *store) const;
  BOOL Unmapped_Vertices(WN *wn) const;
  BOOL Is_Descendent_Of_Store_Address(WN *store,WN *wn) const;
  WN *Find_Match(WN *store,OPCODE rhs_opcode, WN *rhs);
};



#endif  // REDUC_DECLARE

