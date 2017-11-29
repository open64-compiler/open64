/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


//                     Hoisting of outer loop invariant expressions
//                     -----------------------------------------------
//
// Description:
//
//	Given something like
//
//	   do i = 1,n
//	     do j = 1,n
//	       b(j)*c(j)
//  
//	generate
//	     do j = 1,n
//	       t(j) = b(j)*c(j)
//	   do i = 1,n
//	     do j = 1,n
//	       t(j)
//
// also hoists inner invariant expressions as well
//
/* ====================================================================
 * ====================================================================
 *
 * Module: oinvar.cxx
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  30-12-96 - Original Version
 *
 * Description: Hositing of outer loop invariant expressions
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
#include "lnoutils.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "lno_bv.h"
#include "whirl2src.h"
#include "fiz_fuse.h"
#include "fission.h"
#include "snl_utils.h"
#include "scalar_expand.h"
#include "small_trips.h"
#include "soe.h"
#include "cond.h"

static void Transform_Expression(BIT_VECTOR *bv, WN *exp, DOLOOP_STACK *do_stack, INT num_loops, 
		INT outer_reg_tile, BOOL can_tile);
static BOOL Process_Load(WN *load, BIT_VECTOR *result,DOLOOP_STACK *do_stack, 
	INT num_elements,HASH_TABLE<WN *,BIT_VECTOR *> *htable, MEM_POOL *pool, BOOL outer_only);
static BOOL Compatible_Expressions(BIT_VECTOR *input, BIT_VECTOR *output, BOOL outer_only);
extern WN *Split_Using_Preg(WN* stmt, WN* op,
                   ARRAY_DIRECTED_GRAPH16* dep_graph, BOOL recursive);
static WN *Fission_Statement(WN *statement, DOLOOP_STACK *do_stack, INT num_loops);
static BOOL Contains_Work(WN *exp);
static BOOL Contains_Varying_Indirect_Load(WN *exp);
static BOOL Contains_Indirect_Load(WN *exp);
static BOOL Contains_Load(WN *exp);
static void Scalar_Expansion_Tile(DOLOOP_STACK *do_stack,INT num_loops,
	BIT_VECTOR *used_loops);
extern INT SNL_INV_Compute_Tile_Size(INT depth);
void Mark_Invar(WN *region, INT num_loops, DOLOOP_STACK *do_stack,
	HASH_TABLE<WN *,BIT_VECTOR *> *htable, MEM_POOL *pool, BOOL outer_only);
static BIT_VECTOR *Mark_Expression(WN *wn, INT num_loops, DOLOOP_STACK *do_stack,
		HASH_TABLE<WN *,BIT_VECTOR *> *htable, MEM_POOL *pool, BOOL outer_only);
static INT First_Invariant(BIT_VECTOR *bv,INT num_loops);
static void Hoist_Inner_Invar(BIT_VECTOR *bv,WN *exp, DOLOOP_STACK *do_stack, INT num_loops);
static BOOL Sufficient_Iterations(BIT_VECTOR *bv, DOLOOP_STACK *stack,
					INT num_loops) ;
typedef STACK<WN *> STACK_OF_WN;

// TODO: a max size to prevent sorts from being too expensive
// unlikely we'll ever hit this
#define MAX_SIZE 50

// Avoid doing this optimization on small trip count looops
// since the overhead might be larger than the gain
#define MIN_ITERATIONS 8

// A class of wns and their associated bit vectors
// Also store the parent of the wn, and the kidno of the
// parent that gives the wn
class WN_BV
{
public:
  WN *wn;
  WN *parent;
  INT kid_pos;
  BIT_VECTOR *bv;
  WN_BV(WN *w, BIT_VECTOR *b) {  
    wn=w; 
    bv=b;
  };
  WN_BV(WN *w, BIT_VECTOR *b, WN *par, INT kp) {  
    wn=w; 
    bv=b;
    parent = par;
    kid_pos = kp;
  };
  WN_BV() {}
} ;

typedef STACK<WN_BV> WN_BV_STACK;

static void Remove_Invar_Duplicates(WN_BV_STACK* invar_stack);
static void Prune_Invar_Memops(WN_BV_STACK* invar_stack, BOOL inner_invar);
static void Sort_Invar_Stack(WN_BV_STACK *invar_stack, INT num_loops);
static void Gather_Invar(WN *inner, INT num_loops, DOLOOP_STACK *do_stack,
			HASH_TABLE<WN *,BIT_VECTOR *> *htable, WN_BV_STACK *invar_stack);

// Hoist all the invariants in the inner loop wn_inner
// The loop outer_reg_tile is going to be register tiled
// so we must be certain to put all the ALLOCAs outside this loop
//
// Only do scalar expansion limiting if can_tile is true
void Hoist_Outer_Invar(WN *wn_inner, INT num_loops, INT outer_reg_tile, BOOL can_tile)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn_inner);
  if (!dli->Num_Iterations_Symbolic && 
	dli->Est_Num_Iterations < MIN_ITERATIONS) {
    return;
  }

  MEM_POOL_Push(&LNO_local_pool);
  if (LNO_Verbose) { 
    fprintf(stdout, 
      "# Hoisting outer invariants from loop on line %d (begin)\n", 
      Srcpos_To_Line(WN_linenum(wn_inner))); 
    fprintf(TFile, 
      "# Hoisting outer invariants from loop on line %d (begin)\n", 
      Srcpos_To_Line(WN_linenum(wn_inner))); 
  } 
  DOLOOP_STACK *do_stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
					&LNO_local_pool);
  WN_BV_STACK *invar_stack = CXX_NEW(WN_BV_STACK(&LNO_local_pool),
					&LNO_local_pool);
  {
    HASH_TABLE<WN *,BIT_VECTOR *> htable(500,&LNO_local_pool);
    Build_Doloop_Stack(wn_inner,do_stack);
    Mark_Invar(WN_do_body(wn_inner),num_loops,do_stack,&htable,
      &LNO_local_pool,FALSE);
    Gather_Invar(WN_do_body(wn_inner),num_loops,do_stack,&htable,invar_stack);
    Remove_Invar_Duplicates(invar_stack);
    Prune_Invar_Memops(invar_stack, outer_reg_tile == num_loops);
    Sort_Invar_Stack(invar_stack,num_loops);

    for (INT i=0; i<invar_stack->Elements(); i++) {
      WN *wn = invar_stack->Bottom_nth(i).wn;
      BIT_VECTOR *bv = invar_stack->Bottom_nth(i).bv;
      if (bv->Test(bv->Size()-1)) {
	if (outer_reg_tile == num_loops && Contains_Indirect_Load(wn))
		Hoist_Inner_Invar(bv,wn,do_stack,num_loops);
      } else {
	if (Sufficient_Iterations(bv,do_stack,num_loops)) {
          Transform_Expression(bv,wn,do_stack,num_loops,outer_reg_tile,
	    can_tile);
        }
      }
    }
  }
  if (LNO_Verbose) { 
    fprintf(stdout, 
      "# Hoisting outer invariants from loop on line %d (end)\n", 
      Srcpos_To_Line(WN_linenum(wn_inner))); 
    fprintf(TFile, 
      "# Hoisting outer invariants from loop on line %d (end)\n", 
      Srcpos_To_Line(WN_linenum(wn_inner))); 
  } 
  MEM_POOL_Pop(&LNO_local_pool);
}


// Remove duplicate copies of equivalent WHIRL
// expressions from the invariant list
//
static void 
Remove_Invar_Duplicates(WN_BV_STACK* invar_stack)
{
  for (INT i = 0; i < invar_stack->Elements(); ++i) {
    WN* wn = invar_stack->Bottom_nth(i).wn;
    for (INT j = i+1; j < invar_stack->Elements(); ++j) {
      if (Tree_Equiv(wn, invar_stack->Bottom_nth(j).wn)) {
        invar_stack->Bottom_nth(j) = invar_stack->Top();
        invar_stack->Pop();
        --j;
      }
    }
  }
}


// This is a quick hack for 7.3
// It should be fixed to use latencies from targ_info
//
static inline BOOL
Is_Very_Expensive_Expression(const WN* wn)
{
  OPERATOR opr = WN_operator(wn);

  return (opr == OPR_DIV ||
          opr == OPR_DIVREM ||
          opr == OPR_RECIP ||
          opr == OPR_SQRT ||
          opr == OPR_RSQRT
#ifdef TARG_X8664
          || opr == OPR_ATOMIC_RSQRT
#endif
	  );
}

#ifdef KEY
static BOOL Operands_All_Invariant(WN *wn)
{
 WN *do_loop = Enclosing_Do_Loop(wn);
 for (INT i = 0; i < WN_kid_count(wn); i++){
  WN *kid = WN_kid(wn,i);
  if(!WN_operator(kid)==OPR_LDID || 
    !Is_Loop_Invariant_Exp(kid, do_loop)) 
   return FALSE;
 }
 return TRUE;
}
#endif

// Count the number of ILOADs and bytes actually moved
// Special handling is needed for complex types:
// ILOAD of complex types actually generates two loads,
// unless its parent is OPR_REALPART or OPR_IMAGPART
// 
// Very expensive operations override memory issues
//
static BOOL
Expr_Should_Always_Be_Hoisted(WN* wn, 
                              DYN_ARRAY<WN*>& ld_array,
                              INT* num_loads, 
                              INT* num_bytes)
{
  if (WN_operator(wn) == OPR_ILOAD) {

    INT loads = 1;
    TYPE_ID rtype = WN_rtype(wn);

    // check if we are touching only one part of a complex number
    // if we need the whole complex number, count it as two loads
    if (MTYPE_is_complex(rtype)) {
      WN* parent = LWN_Get_Parent(wn);
      OPERATOR parent_opr = WN_operator(parent);
      if (parent_opr == OPR_REALPART || parent_opr == OPR_IMAGPART) {
        wn = parent;
        rtype = WN_rtype(wn);
      }
      else {
        loads = 2;
      }
    }

    // check for duplicate ILOADs
    for (INT i = 0; i < ld_array.Elements(); ++i) {
      if (Tree_Equiv(wn, ld_array[i])) {
        return FALSE;
      }
    }

    // if not found, add ILOAD to the list
    ld_array.AddElement(wn);
        
    INT bytes = MTYPE_byte_size(rtype);

    (*num_loads) += loads;
    (*num_bytes) += bytes;
  }

//bug 14132 : Even though an expression is very expensive, if all
//its operands are invariant to the loop, the optimizer will hoist
//the expression outside the loop. And thus beyond the consideration.
#ifdef KEY
  else if (Is_Very_Expensive_Expression(wn) && 
                !Operands_All_Invariant(wn)){ 
#else
  else if (Is_Very_Expensive_Expression(wn)) {
#endif
    return TRUE;
  }

  else {
    for (INT i = 0; i < WN_kid_count(wn); ++i) {
      if (Expr_Should_Always_Be_Hoisted(WN_kid(wn,i), 
                                        ld_array, 
                                        num_loads, 
                                        num_bytes)) {
        return TRUE;
      }
    }
  }

  return FALSE;
}
                 
  
// Remove invariant expressions whose hoisting would increase either
// a) the number of loads in the inner loop, or
// b) the number of bytes actually read from memory
// 
// a) can happen when we have a complex number that has one part
//    that is outer invariant and one part that is constant.
//    after such a pair is hoisted, the constant part will be
//    lost and require and additional load
//
// b) can happen when we hoist a convert from a smaller to a 
//    larger data type. for example, after hoisting an F4 to F8
//    conversion, we'll have double loads in the loop, compared
//    to the original single load and convert in register.
// 
// Case b) was actually encountered in spectral.f90 code (656837).
// Case a) was contrived by a minor modification of the same code.
//
// If the expression is inner invariant (same test is performed as
// when Hoist_Inner_Invar is called) memory cost is not an issue.
//
// If the expression includes "really" expensive ops, 
// we will ignore possible increase in memmory traffic
//
static void 
Prune_Invar_Memops(WN_BV_STACK* invar_stack, BOOL inner_invar)
{
  for (INT i = 0; i < invar_stack->Elements(); ++i) {

    WN* wn = invar_stack->Bottom_nth(i).wn;
    BIT_VECTOR* bv = invar_stack->Bottom_nth(i).bv;

    // skip expressions that will go through Hoist_Inner_Invar
    // because they won't be scalar expanded
    if (inner_invar && bv->Test(bv->Size()-1) && Contains_Indirect_Load(wn)) {
      continue;
    }

    INT loads_after_hoisting = (WN_operator(wn) == OPR_COMPLEX ? 2 : 1);
    INT bytes_after_hoisting = MTYPE_byte_size(WN_rtype(wn));
    
    DYN_ARRAY<WN*> loads(&LNO_local_pool);
    INT loads_before_hoisting = 0;
    INT bytes_before_hoisting = 0;
    
    // if hoisting of this expression would increase memory traffic
    // delete it from the list of invariants
    if (!Expr_Should_Always_Be_Hoisted(wn, 
                                       loads,
                                       &loads_before_hoisting,
                                       &bytes_before_hoisting)
        &&
        (loads_after_hoisting > loads_before_hoisting ||
         bytes_after_hoisting > bytes_before_hoisting)) {
      invar_stack->Bottom_nth(i) = invar_stack->Top();
      invar_stack->Pop();
      --i;
    }
  }
}


// Sort the order in which we process invariant expressions
// We want things that are invariant in the outermost loops first.
// That's because we only scalar expand across the invariant loops
// So, we want to be sure that when we process the second expression,
// it isn't invariant across the ALLOCA created by the first expression
//
// This is necessary for correctness
static void Sort_Invar_Stack(WN_BV_STACK *invar_stack,INT num_loops)
{
  for (INT i=0; i<invar_stack->Elements(); i++) {
    BIT_VECTOR *bvi = invar_stack->Bottom_nth(i).bv;
    INT min_val = First_Invariant(bvi,num_loops);
    INT min_pos = i;
    for (INT j=i+1; j<invar_stack->Elements(); j++) {
      BIT_VECTOR *bvj = invar_stack->Bottom_nth(j).bv;
      INT val = First_Invariant(bvj,num_loops);
      if (val < min_val) {
	min_val = val;
	min_pos = j;
      }
    }
    if (min_pos != i) {
      WN *tmp_wn = invar_stack->Bottom_nth(i).wn;
      invar_stack->Bottom_nth(i).wn = invar_stack->Bottom_nth(min_pos).wn;
      invar_stack->Bottom_nth(i).bv = invar_stack->Bottom_nth(min_pos).bv;
      invar_stack->Bottom_nth(min_pos).wn = tmp_wn;
      invar_stack->Bottom_nth(min_pos).bv = bvi;
    }
  }
}
      


// Mark every expression in the loop with a BIT_VECTOR
// position 0 of each bit vector corresponds to the outermost loop
// bit 'i' is set if the expression is invariant in the 'i' loop and 
// distributable across the 'i' loop
// In addition, the expression must not be inside a conditional
// a null bit vector implies that the expression is not invariant
//
// If outer only, only consider expressions invariant in an outer loop
void Mark_Invar(WN *wn_inner, INT num_loops, DOLOOP_STACK *do_stack,
	HASH_TABLE<WN *,BIT_VECTOR *> *htable, MEM_POOL *pool, BOOL outer_only)
{
  OPCODE opcode = WN_opcode(wn_inner);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn_inner);
    while (kid) {
      WN *next = WN_next(kid);
      Mark_Invar(kid,num_loops,do_stack,htable,pool,outer_only);
      kid = next;
    }
  } else if (OPCODE_is_expression(opcode)) {
    Mark_Expression(wn_inner,num_loops,do_stack,htable,pool,outer_only);
  } else if (OPCODE_is_store(opcode)) {
    Mark_Invar(WN_kid0(wn_inner),num_loops,do_stack,htable,pool,outer_only);
    if (WN_kid_count(wn_inner) == 2) {
      Mark_Invar(WN_kid1(wn_inner),num_loops,do_stack,htable,pool,outer_only);
    }
  }
}

static BIT_VECTOR *Mark_Expression(WN *wn, INT num_loops, DOLOOP_STACK *do_stack,
		HASH_TABLE<WN *,BIT_VECTOR *> *htable, MEM_POOL *pool, BOOL outer_only)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_load(opcode)) {
    BIT_VECTOR *result = CXX_NEW(BIT_VECTOR(num_loops,pool),pool);
    for (INT i=0; i<num_loops; i++) result->Set(i);
    BOOL is_clean = Process_Load(wn,result,do_stack,num_loops,htable,pool,outer_only);
    if (is_clean) {
      if (Contains_Work(wn)) {
        htable->Enter(wn,result);
      }
    }
    return result;
  } else if (OPCODE_operator(opcode) == OPR_INTCONST || 
	     OPCODE_operator(opcode) == OPR_CONST) {
    BIT_VECTOR *result = CXX_NEW(BIT_VECTOR(num_loops,pool),pool);
    for (INT i=0; i<num_loops; i++) result->Set(i);
    return result;
  } else if (WN_kid_count(wn)) {
    BIT_VECTOR *result = Mark_Expression(WN_kid0(wn),num_loops,do_stack,htable,pool,outer_only);
    if (WN_operator(WN_kid0(wn)) == OPR_PARM && 
	  !WN_Parm_By_Value(WN_kid0(wn))) {
      return NULL;
    }

    for (INT i=1; i<WN_kid_count(wn); i++) {
      BIT_VECTOR *new_bv = 
	Mark_Expression(WN_kid(wn,i),num_loops,do_stack,htable,pool,outer_only);
      if (!Compatible_Expressions(new_bv,result,outer_only) ||
          !(WN_operator(WN_kid0(wn)) != OPR_PARM || WN_Parm_By_Value(wn))) {
        INT j;
	for (j=1; j<i; j++) {
          Mark_Expression(WN_kid(wn,j),num_loops,do_stack,htable,pool,outer_only);
	}
        for (j=i+1; j<WN_kid_count(wn); j++) {
          BIT_VECTOR *bv = Mark_Expression(WN_kid(wn,j),num_loops,do_stack,htable,pool,outer_only);
	}
	return NULL;
      }
    }
    if (Contains_Work(wn)) {
      htable->Enter(wn,result);
    }
    return result;
  }
  return NULL;
}




// Hoist all the outer invariant expressions underneath wn_inner in the num_loops deep loop
static void Gather_Invar(WN *wn_inner, INT num_loops, DOLOOP_STACK *do_stack,
			HASH_TABLE<WN *,BIT_VECTOR *> *htable, 
		        WN_BV_STACK *invar_stack)
{

  OPCODE opcode = WN_opcode(wn_inner);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn_inner);
    while (kid) {
      WN *next = WN_next(kid);
      Gather_Invar(kid,num_loops,do_stack,htable,invar_stack);
      kid = next;
    }
  } else if (OPCODE_is_expression(opcode)) {
    BIT_VECTOR *bv = htable->Find(wn_inner);
    if (bv) {
      if (bv->Pop_Count()) { // invariant somewhere
        if (invar_stack->Elements() >= MAX_SIZE) 
	  return;
	if (OPCODE_operator(opcode) != OPR_PARM) {
            invar_stack->Push(WN_BV(wn_inner,bv));
        } else {
            invar_stack->Push(WN_BV(WN_kid0(wn_inner),bv));
        }
      }
    } else {
      for (INT kidno=0; kidno<WN_kid_count(wn_inner); kidno++) {
	Gather_Invar(WN_kid(wn_inner,kidno),num_loops,do_stack,htable,
          invar_stack);
      }
    }
  } else if (OPCODE_is_store(opcode)) {
    Gather_Invar(WN_kid0(wn_inner),num_loops,do_stack,htable,invar_stack);
    if (WN_kid_count(wn_inner) == 2) {
      Gather_Invar(WN_kid1(wn_inner),num_loops,do_stack,htable,invar_stack);
    }  
  }
}



// are the two expressions compatible
// in order to be compatible there must exist a loop in which
// both expressions are invariant
// If outer_only is TRUE, that loop must be an outer loop
//
// if they are compatible, ouput[i] is anded with input[i] for all i
// (i.e. output[i] is invariant iff both input[i] and output[i] were invariant)
static BOOL Compatible_Expressions(BIT_VECTOR *input, BIT_VECTOR *output, BOOL outer_only)
{
  if (!input || !output) return FALSE;
  INT num_match=0;
  INT i;
  for (i=0; i<input->Size()-1; i++) {
    if (input->Test(i) && output->Test(i)) num_match++;
  }
  if (!outer_only && input->Test(i) && output->Test(i)) num_match++;
  if (num_match) {
    for (INT i=0; i<input->Size(); i++) {
      if (!input->Test(i)) output->Reset(i);
    }
    return TRUE;
  }
  return FALSE;
}
    
// in what loops is the load both invariant and distributable
// bit vectors has been inited to all invariant, so reset the non-invariant
// positions
//
// return TRUE if the load is "clean"
static BOOL Process_Load(WN *load, BIT_VECTOR *result,DOLOOP_STACK *do_stack, 
	INT num_elements,HASH_TABLE<WN *,BIT_VECTOR *> *htable, MEM_POOL *pool, BOOL outer_only)
{
  OPCODE opcode = WN_opcode(load);
  WN *inner_loop=do_stack->Top_nth(0);

  // for ldids, we just care about which loops the thing is invariant in
  if (OPCODE_operator(opcode) == OPR_LDID) {
    // special case loop variables
    SYMBOL load_sym(load);
    for (INT i=0; i<num_elements; i++) {
      INT offset = do_stack->Elements() - num_elements;
      SYMBOL loop_sym(WN_index(do_stack->Bottom_nth(i+offset)));
      if (loop_sym == load_sym) {
	result->Reset(i);
	return TRUE;
      }
    }
    DEF_LIST *defs = Du_Mgr->Ud_Get_Def(load);
    if (defs) {
      DEF_LIST_ITER iter(defs);
      for(DU_NODE *node=iter.First(); !iter.Is_Empty(); node=iter.Next()) {
        WN *def = node->Wn();
        WN *def_loop = LNO_Common_Loop(def,inner_loop);
	INT i=0;
	while (i<num_elements && do_stack->Top_nth(i) != def_loop) {
	  i++;
        }
	for (INT j=i; j<num_elements; j++) {
	  result->Reset(num_elements-1-j);
	}
      }
    }
  } else {
    // First check for invariance of the address

    WN *array = WN_kid0(load);
    if (WN_operator(array) == OPR_ADD) {
      if (WN_operator(WN_kid0(array)) == OPR_INTCONST) {
	array = WN_kid1(array);
      } else if (WN_operator(WN_kid1(array)) == OPR_INTCONST) {
	array = WN_kid0(array);
      }
    }
    if (WN_operator(array) != OPR_ARRAY) {
      for (INT i=0; i<num_elements; i++) result->Reset(i);
      return FALSE;

    }
    ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array);
    BOOL messy = aa->Too_Messy;
    if (!messy) {
      for (INT i=0; i<aa->Num_Vec() ; i++) {
        ACCESS_VECTOR *av = aa->Dim(i);
	if (av->Too_Messy) messy = TRUE;
      }
    }
    // Recurse on messy access vectors, we can do this for all access vectors,
    // but it's faster to use the access vectors when they're clean
    if (messy) {
      BIT_VECTOR *tmp = Mark_Expression(array,num_elements,do_stack,htable,pool,outer_only);
      if (tmp) {
        for (INT i=0; i<num_elements; i++) {
	  if (!tmp->Test(i)) result->Reset(i);
	}
      } else {
        for (INT i=0; i<num_elements; i++) result->Reset(i);
        return FALSE;
      }
    } else { 
      for (INT j=0; j<aa->Num_Vec() ; j++) {
        ACCESS_VECTOR *av = aa->Dim(j);
        INT bad_symbolics = av->Non_Const_Loops() + num_elements - av->Nest_Depth();
        INT i;
        for (i=0; i<bad_symbolics; i++) {
	  result->Reset(i);
        }
        INT i0 = av->Nest_Depth()-num_elements;
        for (i=MAX(0,bad_symbolics); i<num_elements; i++) {
	  if (av->Loop_Coeff(i+i0)) {
	    result->Reset(i);
          }
        }
      }
    }

    // now check for invariance of the value and for distributability
    VINDEX16 v = Array_Dependence_Graph->Get_Vertex(load);
    if (!v) {
      for (INT i=0; i<num_elements; i++) result->Reset(i);
      return FALSE;
    }
    for (EINDEX16 e = Array_Dependence_Graph->Get_In_Edge(v); e; 
				e=Array_Dependence_Graph->Get_Next_In_Edge(e)) {
      DEPV_ARRAY *da = Array_Dependence_Graph->Depv_Array(e);
      INT maxlevel = da->Max_Level();
      INT i0 = da->Num_Dim() + da->Num_Unused_Dim();
      for (INT i=0; i<=MIN(maxlevel-i0+num_elements,num_elements-1); i++) {
	result->Reset(i);
      }
    }
  }
  return TRUE;
}



// Does this load load a different address in different iterations
// of the inner loop
// If we return FALSE too much, we'll limit hoisting, which is 
// conservative
static BOOL Varying_Load(WN *load)
{
  WN *array = WN_kid0(load);
  if (WN_operator(array) == OPR_ADD) {
    if (WN_operator(WN_kid0(array)) == OPR_INTCONST) {
      array = WN_kid1(array);
    } else if (WN_operator(WN_kid1(array)) == OPR_INTCONST) {
      array = WN_kid0(array);
    }
  }
  if (WN_operator(array) != OPR_ARRAY) {
    return FALSE;
  }
  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array);
  if (!aa->Too_Messy) {
    for (INT i=0; i<aa->Num_Vec() ; i++) {
      ACCESS_VECTOR *av = aa->Dim(i);
      if (av->Too_Messy) return FALSE;
    }
  }
  for (INT i=0; i<aa->Num_Vec() ; i++) {
    ACCESS_VECTOR *av = aa->Dim(i);
    if (av->Loop_Coeff(av->Nest_Depth()-1)) {
      return TRUE;
    }
  }
  return FALSE;
}


static INT First_Invariant(BIT_VECTOR *bv,INT num_loops)
{
  INT first_invariant=0;
  while (first_invariant<num_loops-1 && !bv->Test(first_invariant)) first_invariant++;
  return first_invariant;
}

// Is loop perfectly nested in the num_outer DO loops that surround it
static BOOL Perfectly_Nested(WN *loop, INT num_outer)
{
  if (num_outer == 0) return TRUE;
  if (WN_next(loop) || WN_prev(loop)) return FALSE;
  WN *new_loop = LWN_Get_Parent(LWN_Get_Parent(loop));
  OPCODE opc = WN_opcode(new_loop);
  if (opc != OPC_DO_LOOP) return FALSE;
  return Perfectly_Nested(new_loop,num_outer-1);
}


// do the transformation
static void Transform_Expression(BIT_VECTOR *bv, WN *exp, DOLOOP_STACK *do_stack, INT num_loops,
				INT outer_reg_tile, BOOL can_tile)
{
  INT first_invariant=First_Invariant(bv,num_loops);
  first_invariant = MIN(first_invariant,outer_reg_tile);
  num_loops = num_loops-first_invariant; // skip outer variant loops


  MEM_POOL_Push(&LNO_local_pool);

  BIT_VECTOR *used_loops = CXX_NEW(BIT_VECTOR(num_loops,&LNO_local_pool),
				&LNO_local_pool);
  INT used_loops_offset = bv->Size()-num_loops;
  INT i;
  for (i=0; i<num_loops; i++) {
    if (bv->Test(i+used_loops_offset)) {
      used_loops->Reset(i);
    } else {
      used_loops->Set(i);
    }
  }

  // Tile to limit scalar expansion if possible
  if (can_tile && Perfectly_Nested(do_stack->Top_nth(0),num_loops-1)) {
    Scalar_Expansion_Tile(do_stack,num_loops,used_loops);
  }

  // split off the expression
  WN *new_statement = Split_Using_Preg(LWN_Get_Statement(exp),
	exp,Array_Dependence_Graph, FALSE);
  WN *rhs = WN_kid0(new_statement);

  // Do the scalar expansion 
  WN **loops = CXX_NEW_ARRAY(WN *,num_loops,&LNO_local_pool);
  INT *order = CXX_NEW_ARRAY(INT,num_loops,&LNO_local_pool);
  INT *strip_sizes = CXX_NEW_ARRAY(INT,num_loops,&LNO_local_pool);
  for (i=0; i<num_loops; i++) {
    loops[num_loops-i-1] = do_stack->Top_nth(i);
    order[i] = i;
    strip_sizes[i] = 0;
  }
  Scalar_Expand(do_stack->Top_nth(num_loops-1),
    do_stack->Top_nth(num_loops-1), new_statement, SYMBOL(new_statement), 
    loops, order, num_loops, TRUE, FALSE, FALSE, NULL, used_loops);

  // Scalar expansion has killed new statement, find the equivalent
  new_statement = LWN_Get_Parent(rhs);

  // do the fission
  WN *new_outer_loop = Fission_Statement(new_statement,do_stack,num_loops);

  // get rid of the uneeded loops
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
  WN *tmp = rhs;
  for (i=0; i<num_loops; i++) {
    tmp = LWN_Get_Parent(tmp);
    while (WN_opcode(tmp) != OPC_DO_LOOP) tmp = LWN_Get_Parent(tmp);
    if (!used_loops->Test(num_loops-1-i)) {
      stack->Push(tmp);
    }
  }
  Is_True(new_outer_loop == tmp,("Internal error in Transform_Expression"));
  SNL_Finalize_Loops(new_outer_loop,stack,Array_Dependence_Graph,Du_Mgr);


  MEM_POOL_Pop(&LNO_local_pool);
}

// Hoist an inner invariants
// Hoist them through all the inner invariant loops but don't go through any variant
// loops, there is no point in doing scalar expansion here
//
// Only do this if we're not register tiling, since register tiling
// can not deal with results of this optimization
//
// Only hoist if there is an indirect load in the expression (checked by caller)
// For inner invariants there is no point in hoisting other things because WOPT
// will do it better

static void Hoist_Inner_Invar(BIT_VECTOR *bv,WN *exp, DOLOOP_STACK *do_stack, INT num_loops)
{
  INT i=num_loops-1;
  while (i>=0 && bv->Test(i)) i--;
  num_loops = num_loops-i-1;
  WN *new_statement = Split_Using_Preg(LWN_Get_Statement(exp),
	exp,Array_Dependence_Graph, FALSE);
  WN *new_outer_loop = Fission_Statement(new_statement,do_stack,num_loops);

  MEM_POOL_Push(&LNO_local_pool);
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
  WN *tmp = new_statement;
  for (i=0; i<num_loops; i++) {
    while (WN_opcode(tmp) != OPC_DO_LOOP) tmp = LWN_Get_Parent(tmp);
    stack->Push(tmp);
    tmp = LWN_Get_Parent(tmp);
  }
  SNL_Finalize_Loops(new_outer_loop,stack,Array_Dependence_Graph,Du_Mgr);
  MEM_POOL_Pop(&LNO_local_pool);
}


// Fission the loops one level at a time
// statement is the statement we want to fission out
// fission_loop is the inner loop
//
// return the loop we have fissioned out
//
static WN *Fission_Statement(WN *statement, DOLOOP_STACK *do_stack, INT num_loops)
{
  WN *fission_loop = do_stack->Top_nth(0);
  for (INT i=0; i<num_loops; i++) {
    MEM_POOL_Push(&LNO_local_pool);
    {
      DYN_ARRAY<FF_STMT_LIST> loop_list(&LNO_local_pool);
      loop_list.Newidx(); loop_list[0].Init(NULL);
      loop_list.Newidx(); loop_list[1].Init(NULL);

      WN *tmp = WN_first(WN_do_body(fission_loop));
      while (tmp) {
	if (tmp == statement) {
	  loop_list[1].Append(tmp,&LNO_local_pool);
        } else {
	  loop_list[0].Append(tmp,&LNO_local_pool);
        }
	tmp = WN_next(tmp);
      }
      Separate_And_Update(fission_loop,loop_list,1,1);
      WN* new_loop = WN_next(fission_loop); 
    }
    MEM_POOL_Pop(&LNO_local_pool);

    if (i != num_loops -1) {
      // prepare for next round, the loop we're fissioning is one loop out
      fission_loop = LWN_Get_Parent(fission_loop);
      while (WN_opcode(fission_loop) != OPC_DO_LOOP) fission_loop = LWN_Get_Parent(fission_loop);

    }
    // the statement we want to keep by itself is the loop surrounding statement
    statement = LWN_Get_Parent(statement);
    while (WN_opcode(statement) != OPC_DO_LOOP) statement = LWN_Get_Parent(statement);

    // flip the the two loops
    // This is necessary so that the split statement comes before the main body
    // Why not just reverse the entries in loop_list?
    //   This way the core gets the same wn's for the do loops as it had before
    //   This is necessary for the rest of phase 2 to work
    WN *other_loop = WN_prev(statement);
    Is_True(WN_opcode(other_loop) == OPC_DO_LOOP,("Fissioned loops not adjacent"));
    WN *parent = LWN_Get_Parent(statement);
    WN *prev = WN_prev(other_loop);
    other_loop = LWN_Extract_From_Block(other_loop);
    statement = LWN_Extract_From_Block(statement);
    LWN_Insert_Block_After(parent,prev,other_loop);
    LWN_Insert_Block_After(parent,prev,statement);

  }


  return statement;
}

static BOOL Contains_FP_Non_Load(WN *exp)
{
  OPCODE opc = WN_opcode(exp);
  OPERATOR oper = OPCODE_operator(opc);
  if (MTYPE_float(OPCODE_rtype(opc))) {
    if (!OPCODE_is_load(opc) &&
	(oper != OPR_CONST) &&
	(oper != OPR_PARM) &&
        (oper != OPR_REALPART) &&
	(oper != OPR_IMAGPART) &&
	(oper != OPR_COMPLEX) &&
	(oper != OPR_PAREN)) {
      return TRUE;
    }
  } 
  for (INT kidno=0; kidno<WN_kid_count(exp); kidno++) {
    if (Contains_FP_Non_Load(WN_kid(exp,kidno))) {
      return TRUE;
    }
  }
  return FALSE;
}



// Is there useful work here
// The current definition is 
// 1. floating point non-load, non-paren,non-parm
//  or
// 2. a non-add/sub/neg over a variant indirect load 
// 3. a variant indirect load in some kid, and any load in some other kid
//
// We call this to avoid hoisting addressing expressions that will
// be simplified away
static BOOL Contains_Work(WN *exp)
{
  OPCODE opc = WN_opcode(exp);
  OPERATOR oper = OPCODE_operator(opc);
  if ( (oper == OPR_CONST) ||
      (oper == OPR_LDA)) {
    return FALSE;
  }
  if (oper == OPR_ILOAD) {
    return Contains_Work(WN_kid0(exp));
  }
  if (oper == OPR_PAREN || oper == OPR_PARM ||
      oper == OPR_REALPART || oper == OPR_IMAGPART) {
    return Contains_Work(WN_kid0(exp));
  }
  if (!OPCODE_is_load(opc) && MTYPE_float(OPCODE_rtype(opc)) &&
       (OPCODE_operator(opc) != OPR_COMPLEX)) return TRUE;
  BOOL non_add_non_load = (!OPCODE_is_load(opc) &&
		       oper != OPR_ADD && oper != OPR_SUB && oper != OPR_NEG);
  BOOL contains_indirect=FALSE;
  BOOL num_kids_with_loads=0;
  for (INT kidno=0; kidno<WN_kid_count(exp); kidno++) {
    if (Contains_FP_Non_Load(WN_kid(exp,kidno))) {
      return TRUE;
    }
    if (Contains_Varying_Indirect_Load(WN_kid(exp,kidno))) {
      if (non_add_non_load) return TRUE;
      contains_indirect=TRUE;
      num_kids_with_loads++;
    } else if (Contains_Load(WN_kid(exp,kidno))) {
      num_kids_with_loads++;
    }
  }
  if (num_kids_with_loads >= 2 && contains_indirect) return TRUE;
  return FALSE;
}


// Does this contain an indirect load that varies
// in the innermost loop
static BOOL Contains_Varying_Indirect_Load(WN *exp)
{
  OPCODE opc = WN_opcode(exp);
  if (OPCODE_is_load(opc) && 
      (OPCODE_operator(opc) != OPR_LDID)) {
    if (Varying_Load(exp)) {
      return TRUE;
    }
  }
  for (INT kidno=0; kidno<WN_kid_count(exp); kidno++) {
    if (Contains_Varying_Indirect_Load(WN_kid(exp,kidno))) {
      return TRUE;
    }
  }
  return FALSE;
}

static BOOL Contains_Indirect_Load(WN *exp)
{
  OPCODE opc = WN_opcode(exp);
  if (OPCODE_is_load(opc) && 
      (OPCODE_operator(opc) != OPR_LDID)) {
    return TRUE;
  }
  for (INT kidno=0; kidno<WN_kid_count(exp); kidno++) {
    if (Contains_Indirect_Load(WN_kid(exp,kidno))) {
      return TRUE;
    }
  }
  return FALSE;
}

static BOOL Contains_Load(WN *exp)
{
  OPCODE opc = WN_opcode(exp);
  if (OPCODE_is_load(opc)) {
    return TRUE;
  }
  for (INT kidno=0; kidno<WN_kid_count(exp); kidno++) {
    if (Contains_Load(WN_kid(exp,kidno))) {
      return TRUE;
    }
  }
  return FALSE;
}




// Tile the loops we're scalar expanding to avoid creating tmps that
// are too large
static void Scalar_Expansion_Tile(DOLOOP_STACK *do_stack,INT num_loops,
					BIT_VECTOR *used_loops) {
  INT num_used_loops = used_loops->Pop_Count();
  INT tile_size = SNL_INV_Compute_Tile_Size(num_used_loops);
  WN *outer_loop = do_stack->Top_nth(num_loops-1);
  INT *permutation = CXX_NEW_ARRAY(INT,num_loops,&LNO_local_pool);
  for (INT i=0; i<num_loops; i++) {
    if (used_loops->Test(i)) {
      WN *loop = do_stack->Top_nth(num_loops-i-1);
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(loop);
      if (dli->Num_Iterations_Symbolic || dli->Est_Num_Iterations > tile_size) {
        if (!dli->Is_Inner_Tile || dli->Tile_Size > tile_size) {
          SYMBOL oldsym(WN_index(loop));
	  char *Str_Buf = 
	    CXX_NEW_ARRAY(char,18+strlen(oldsym.Name()),&LNO_local_pool);
	  sprintf(Str_Buf,"oinvar_tile_%s",oldsym.Name());
	  SYMBOL *sym_pid = CXX_NEW(
	       SYMBOL(Create_Preg_Symbol(Str_Buf, Do_Wtype(loop))), 
				&LNO_default_pool);
	  WN* newloop = Tile_Loop(loop,tile_size,0,SNL_INV_SE_ONLY,sym_pid,
		&LNO_local_pool);
	  permutation[0] = i;
          for (INT j=1; j<=i; j++) {
	    permutation[j] = j-1;
	  }
          SNL_INV_Permute_Loops(outer_loop,permutation,i+1,TRUE);
        }
      }
    }
  }
}


// If there is a series of loads being added/multiplied,mined or maxed,
// Sort the series based on invariance so that things with the
// same invariance are next to each other


class WNS_BV
{
public:
  STACK_OF_WN *stack;
  BIT_VECTOR *bv;
  WNS_BV(STACK_OF_WN *wns, BIT_VECTOR *b) { stack = wns; bv=b;};
};

typedef STACK<WNS_BV> WNS_BV_STACK;

enum EQUIVALENCE_TYPE { EQ_NONE=0, EQ_ADD, EQ_MPY, EQ_MIN, EQ_MAX };
static EQUIVALENCE_TYPE Sort_Invar_Expression(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable);
static void Sort_Equivalence_Class(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable);
static void Sort_Equivalence_Class_Rec(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable,
					WN_BV_STACK *wn_bv_stack);
static void Sort_Invar_Expressions_Rec(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable);
static BOOL sorted_something;

// If there is a series of loads being added/multiplied,mined or maxed,
// Sort the series based on invariance so that things with the
// same invariance are next to each other
//
// return TRUE if anything was sorted
// if returning TRUE, htable is invalid
BOOL Sort_Invar_Expressions(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable)
{
  sorted_something = FALSE;
  Sort_Invar_Expressions_Rec(wn,htable);
  return sorted_something;
}

static void Sort_Invar_Expressions_Rec(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      WN *next = WN_next(kid);
      Sort_Invar_Expressions_Rec(kid,htable);
      kid = next;
    }
  } else if (OPCODE_is_expression(opcode)) {
    if (Sort_Invar_Expression(wn,htable) != EQ_NONE) {
      Sort_Equivalence_Class(wn,htable);
    }
  } else if (OPCODE_is_store(opcode)) {
    if (Sort_Invar_Expression(WN_kid0(wn),htable) != EQ_NONE) {
      Sort_Equivalence_Class(WN_kid0(wn),htable);
    }
  } else if (opcode == OPC_DO_LOOP) {
    Sort_Invar_Expressions_Rec(WN_do_body(wn),htable);
  }
}




// Sort all the equiavlence classes in an expreesion
static EQUIVALENCE_TYPE Sort_Invar_Expression(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable)
{
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_ADD) || (oper == OPR_MPY) || 
      (oper == OPR_MIN) || (oper == OPR_MAX)) {
    EQUIVALENCE_TYPE eq;
    if (oper == OPR_ADD) eq = EQ_ADD;
    else if (oper == OPR_MPY) eq = EQ_MPY;
    else if (oper == OPR_MAX) eq = EQ_MAX;
    else if (oper == OPR_MIN) eq = EQ_MIN;
    EQUIVALENCE_TYPE eq0 = Sort_Invar_Expression(WN_kid0(wn),htable);
    EQUIVALENCE_TYPE eq1 = Sort_Invar_Expression(WN_kid1(wn),htable);
    BOOL match0 = (opcode==WN_opcode(WN_kid0(wn)));
    BOOL match1 = (opcode==WN_opcode(WN_kid1(wn)));
    if (match0 && match1) return eq;
    if (!match0) {
      if (eq0 != EQ_NONE) {
	Sort_Equivalence_Class(WN_kid0(wn),htable);
      }
    }
    if (!match1) {
      if (eq1 != EQ_NONE) {
	Sort_Equivalence_Class(WN_kid1(wn),htable);
      }
    }
    if (match0 || match1) { 
      return eq;
    }
    return EQ_NONE;
  } 
  return EQ_NONE;
}

// stack the tree so that the non-loads are on the left
static void Left_Justify(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (WN_opcode(WN_kid0(wn)) == opcode) {
    Left_Justify(WN_kid0(wn));
  }  
  if (WN_opcode(WN_kid1(wn)) == opcode) {
    Left_Justify(WN_kid1(wn));
  }  
  if (opcode == WN_opcode(WN_kid1(wn)) &&
      opcode != WN_opcode(WN_kid0(wn))) {
    WN *tmp = WN_kid0(wn);
    WN_kid0(wn) = WN_kid1(wn);
    WN_kid1(wn) = tmp;
  }
}


// Sort all elements of the equivalence class rooted at wn
static void Sort_Equivalence_Class(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable)
{
  MEM_POOL_Push(&LNO_local_pool);
  WN_BV_STACK  *wn_bv_stack = CXX_NEW(WN_BV_STACK(&LNO_local_pool),&LNO_local_pool);
  Left_Justify(wn);
  Sort_Equivalence_Class_Rec(wn,htable,wn_bv_stack);

  INT elements = wn_bv_stack->Elements();

  // TODO: give up on really big cases (more than MAX_SIZE)
  // the algorithm goes n^2, and we'll almost never see these cases
  if (elements <= 2 || elements >= MAX_SIZE) {
    MEM_POOL_Pop(&LNO_local_pool);
    return;
  } 

  // at this point, wn_bv_stack gives all the wns in the order in which
  // they appear in the expression tree
  //
  // now group all the WNs into buckets, each bucket corresponds to
  // a unique bit vector, 
  // only put in a bucket, bit vectors with a Pop_Count
  // the other ones don't have invariant terms so there is no point in sorting them
  WNS_BV_STACK *sorted_stack = CXX_NEW(WNS_BV_STACK(&LNO_local_pool),&LNO_local_pool);
  INT i;
  for (i=0; i<elements; i++) {
    BIT_VECTOR *bvi = wn_bv_stack->Bottom_nth(i).bv;
    if (bvi && bvi->Pop_Count()) {
      WN *wni = wn_bv_stack->Bottom_nth(i).wn;
      BOOL found = FALSE;
      for (INT j=0; j<sorted_stack->Elements() && !found; j++) {
        BIT_VECTOR *bvj = sorted_stack->Bottom_nth(j).bv;
        if (bvi->Intersects(bvj)) {
	  *bvi &= *bvj;
	  found = TRUE;
	  sorted_stack->Bottom_nth(j).stack->Push(wni);
        }
      }
      if (!found) {
	BIT_VECTOR *new_bv = CXX_NEW(BIT_VECTOR(bvi->Size(),&LNO_local_pool),&LNO_local_pool);
	*new_bv = *bvi;
        sorted_stack->Push(WNS_BV(CXX_NEW(STACK_OF_WN(&LNO_local_pool),&LNO_local_pool),
		new_bv));
	sorted_stack->Top_nth(0).stack->Push(wni);
      }
    }
  }

  // sort the buckets based on the number of elements in the bucket
  // after this step, sorted stack will give all the wns in an order
  // sorted by bit vectors, with the most commn bit vectors first
  INT sort_elements = sorted_stack->Elements();
  if (sort_elements == 0) {
    MEM_POOL_Pop(&LNO_local_pool);
    return;  
  } 
  for (i=0; i<sort_elements; i++) {
    INT max = sorted_stack->Bottom_nth(i).stack->Elements();
    INT max_pos = i;
    for (INT j=i+1; j<sort_elements; j++) {
      INT ej = sorted_stack->Bottom_nth(j).stack->Elements();
      if (ej > max) {
	max = ej;
	max_pos = j;
      }
    }
    if (max_pos != i) {
      STACK_OF_WN *tmp = sorted_stack->Bottom_nth(i).stack;
      sorted_stack->Bottom_nth(i).stack =  
	sorted_stack->Bottom_nth(max_pos).stack;
      sorted_stack->Bottom_nth(max_pos).stack =  tmp;
    }
  }

  // reorder all the wns in wn_bv_stack according to the order
  // in sorted_stack
  INT bucket = 0;
  INT bucket_pos = 0;
  STACK_OF_WN *sorted_wn = sorted_stack->Bottom_nth(bucket).stack;
      
  BOOL done = FALSE;
  for (i=0; i<elements && !done; i++) {
    WN_BV *wn_bvi = &wn_bv_stack->Bottom_nth(i);
    if (bucket_pos == sorted_wn->Elements()) {
      bucket++;
      if (bucket < sorted_stack->Elements()) {
        bucket_pos = 0;
        sorted_wn = sorted_stack->Bottom_nth(bucket).stack;
	if (sorted_wn->Elements() <= 1) done = TRUE;
      } else {
	done = TRUE;
      }
    }
    if (!done) {
      WN *parenti = wn_bvi->parent;
      INT posi = wn_bvi->kid_pos;
      WN *wni = WN_kid(parenti,posi); // this may be different
				    // than wn_bvi->wn because
				    // of an earlier swap

      WN *wnj = sorted_wn->Bottom_nth(bucket_pos);
      if (wni != wnj) {
        WN *parentj = LWN_Get_Parent(wnj);
        INT posj = 0;
        if (wnj = WN_kid1(parentj)) posj = 1;

        WN_kid(parenti,posi) = wnj;
        LWN_Set_Parent(wnj,parenti);
        WN_kid(parentj,posj) = wni;
        LWN_Set_Parent(wni,parentj);
	sorted_something = TRUE;
      }
      bucket_pos++;
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
}



static void Sort_Equivalence_Class_Rec(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *htable,
					WN_BV_STACK *wn_bv_stack)
{
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);

  if (opcode==WN_opcode(WN_kid0(wn))) {
    Sort_Equivalence_Class_Rec(WN_kid0(wn),htable,wn_bv_stack);
  } else {
    BIT_VECTOR *bv = htable->Find(WN_kid0(wn));
    wn_bv_stack->Push(WN_BV(WN_kid0(wn),bv,wn,0));
  }

  if (opcode==WN_opcode(WN_kid1(wn))) {
    Sort_Equivalence_Class_Rec(WN_kid1(wn),htable,wn_bv_stack);
  } else {
    BIT_VECTOR *bv = htable->Find(WN_kid1(wn));
    wn_bv_stack->Push(WN_BV(WN_kid1(wn),bv,wn,1));
  }

}

// Is the product of the invariant iteration counts sufficiently big
static BOOL Sufficient_Iterations(BIT_VECTOR *bv, DOLOOP_STACK *stack,
					INT num_loops) 
{
  INT prod=1;
  for (INT i=0; i<num_loops; i++) {
    if (bv->Test(bv->Size()-1-i)) {
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(stack->Top_nth(i));
      if (dli->Num_Iterations_Symbolic) {
	return TRUE;
      } else {
	prod = prod * dli->Est_Num_Iterations;
      }
    }
  }
  return prod >= MIN_ITERATIONS;
}

