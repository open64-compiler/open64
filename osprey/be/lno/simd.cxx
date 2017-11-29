/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008 Pathscale, LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
//                  	LNO Vectorization
//                  	-----------------
//

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: be/lno/SCCS/s.simd.cxx $ $Revision: 1.244 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "config_asm.h"         // Temp_Symbol_Prefix
#include "glob.h"
#include "wn.h"
#include "wn_map.h"
#include "cxx_memory.h"
#include "lwn_util.h"
#include "ff_utils.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "scalar_expand.h"
#include "fission.h"
#include "opt_du.h"
#include "dep_graph.h"
#include "reduc.h"
#include "snl.h"
#include "name.h"
#include "inner_fission.h"
#include "lno_scc.h"
#include "config_targ.h"
#include "ir_reader.h"             // for fdump_tree
#include "wn_simp.h"               // for WN_Simplify_Tree
#include "const.h"		   // for New_Const_Sym
#include "data_layout.h"	   // for Stack_Alignment
#include "cond.h"                  // for Guard_A_Do
#include "config_opt.h"            // for Align_Unsafe
#include "be_util.h"               // for Current_PU_Count()
#include "region_main.h" 	   // for creating new region id.
#include "lego_util.h"             // for AWN_StidIntoSym, AWN_Add
#include "minvariant.h"            // for Minvariant_Removal
#include "simd_util.h"
#include "small_trips.h"           // for Remove_Unity_Trip_Loop

#define simd_util_INCLUDED
//#include "simd_util.h"

#include <vector>
#include <set>
#include <map>
#include <list>
#include <deque>

#define ABS(a) ((a<0)?-(a):(a))
#define BINARY_OP(opr) (opr == OPR_ADD || opr == OPR_SUB || opr == OPR_MPY || opr == OPR_SHL)

static void WN_collect_iloads_nr(std::list<WN *> *wn_list, WN *wn);
BOOL debug;
BOOL under_if = FALSE;  // sub-expression of an if-statement,
                        // also indicates if if-vectorization is turned on.
BOOL nested_if = FALSE; // sub-expression of a nested-if-statement

extern WN *Split_Using_Preg(WN* stmt, WN* simd_op,
                   ARRAY_DIRECTED_GRAPH16* dep_graph,
                   BOOL recursive=TRUE);
typedef STACK<WN*> STACK_OF_WN;
typedef HASH_TABLE<WN*,VINDEX16> WN2VINDEX;
typedef HASH_TABLE<WN*,UINT> WN2UINT;
typedef HASH_TABLE<WN*,INT> WN2INT;
typedef DYN_ARRAY<UINT> UINT_DYN_ARRAY;

#define ESTIMATED_SIZE 100	// used to initialized hash table, etc.
#define Iteration_Count_Threshold 10 // threshold to determine if a loop
                                     // has too few a number of iterations

extern REDUCTION_MANAGER *red_manager;	// LNO reduction manager
extern MEM_POOL SNL_local_pool;		// SNL private mem pool
static MEM_POOL SIMD_default_pool;	// simd private mem pool
static ARRAY_DIRECTED_GRAPH16 *adg;	// PU array dep. graph
// Do not disturb the external reduction manager and we only care about scalar
// reductions.
static REDUCTION_MANAGER *simd_red_manager;	
static REDUCTION_MANAGER *curr_simd_red_manager;	

static void Simd_Mark_Code (WN* wn);
static TYPE_ID Simd_Get_Vector_Type(WN *istore);

static INT Last_Vectorizable_Loop_Id = 0;
SIMD_VECTOR_CONF Simd_vect_conf;

typedef struct {
    INT unroll_times;
    INT add_to_base;
    INT base_incr;
    WN  *innerloop;
    WN  *vec_index_preg_store;
    TYPE_ID index_type;
} UNROLL_PARAMS;

static WN_MAP unroll_map; 

// Some functions in this module are called in different contexts:
//
//   - SC_SIMD: The functions are called during SIMD phase. The loop
//        being vectorized should be in the innermost possition. 
//
//   - SC_LOOP_MODELING: The functions are called during the loop modeling time
//       (i.e. LOOP_MODEL::Model() is called). Some functions in this module will 
//       be called to see if a loop, possiblly outer loop, is legal and beneficial
//       to be vectorized when the loop shifts to innermost possition.
// 
//       Since the loop is not necessarily in the innermost possition at the time
//       the functions are called. These functions must envision the situations
//       when the loop is moved to innermost position.
//
//   - SC_OTHER: other misc situations. We conservatively assume that the loop
//       being vectorized is already in the innermost position.
//
typedef enum {
    SC_INVALID = 0,
    SC_SIMD = 1,
    SC_LOOP_MODELING = 2, 
    SC_OTHER = 3,
} SIMD_CONTEXT;

static SIMD_CONTEXT simd_context = SC_INVALID;

// Return TRUE iff there are too few iterations to generate a single 
// vectorized iteration.
//
// One interesting snippet to challenge this function is following:
//
//     float f[]; double d[];
//     for (i = 0; i < 2; i++) { d[i] = (double)f[i]; }
//
// This func should not be folled by "f[i]". Currently, it is ok because 
// "(double)f[i]" instead of "f[i]" is considered as vectorizable expr.
// 
static BOOL Too_Few_Iterations (WN* loop, SCALAR_REF_STACK* vect_exprs) 
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info (loop);
  if (dli->Est_Num_Iterations >= Simd_vect_conf.Get_Vect_Byte_Size ()) 
    return FALSE;

  SIMD_EXPR_MGR expr_mgr (loop, &SIMD_default_pool);
  expr_mgr.Convert_From_Lagacy_Expr_List (vect_exprs);
  return expr_mgr.Get_Max_Vect_Len () > dli->Est_Num_Iterations;
}
     
// Bug 10136: use a stack to count the number of different
// invariant operands
static void Count_Invariant(STACK_OF_WN *invars, WN *ops)
{
  for(INT i=0; i<invars->Elements(); i++){
     WN *tmp = invars->Top_nth(i);
     if(Tree_Equiv(tmp, ops)) return;
  }
  invars->Push(ops); //new one
}

// simd_2 : examine all scalar reads and writes and do the following
//	1. create name to bit position mappings for new symbol names
//	2. for STID, check if it is scalar expandable
extern  UINT simd_2(
	WN* loop,		// enclosing loop
	SCALAR_STACK* scalar_reads,	// read refs to be examined
	SCALAR_STACK* scalar_writes,	// write refs to be examined
	BINARY_TREE<NAME2BIT> *mapping_dictionary,
		// dictionary to be updated which records mapping from
		// symbol names to bit positions
	FF_STMT_LIST& expandable_ref_list)
		// list contains all expandable refs after simd_2
{
  
  UINT bit_position=0;

  SCALAR_STACK *scalar_ref_list[2];
  scalar_ref_list[0]=scalar_reads;
  scalar_ref_list[1]=scalar_writes;

  // look at both reads and writes
  for (INT i=0; i<2; i++) {

    for (INT j=0; j<scalar_ref_list[i]->Elements(); j++) {
  
      WN* scalar_ref=scalar_ref_list[i]->Bottom_nth(j)->Bottom_nth(0)->Wn;
      NAME2BIT temp_map;

      temp_map.Set_Symbol(scalar_ref);

      // create and enter new name to bit_position mapping if it has
      // not been created
      // also check if it is scalar expandable if the ref is a STID
      const BINARY_TREE_NODE<NAME2BIT> *tree_node;
      if (mapping_dictionary->Find(temp_map)==NULL) {

        if (LNO_Test_Dump) {
          temp_map.Get_Symbol().Print(stdout);
          printf("\t\tat bit %d\n", bit_position);
        }
        temp_map.Set_Bit_Position(bit_position);
        mapping_dictionary->Enter(temp_map);
      }

      if (i==1) {
        SE_RESULT se_result = Scalar_Expandable(scalar_ref,loop, Du_Mgr);
        if (!Get_Trace(TP_LNOPT2, TT_LNO_DISABLE_SEFIN) 
            && se_result != SE_NONE || se_result == SE_EASY)
          expandable_ref_list.Append(scalar_ref,&SIMD_default_pool);
      }

      bit_position++;
    }
  }
  return bit_position;
}

static BOOL is_vectorizable_op (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc) {

  switch (opr) {
  case OPR_SELECT:
    if (MTYPE_is_float(rtype))
      return TRUE;
    else
      return FALSE;
  case OPR_EQ: case OPR_NE: 
  case OPR_LT: case OPR_GT: case OPR_LE: case OPR_GE:
    if (MTYPE_is_float(desc) && MTYPE_is_integral(rtype))
      return TRUE;
    else {
      if (under_if && 
	  (opr == OPR_NE && MTYPE_is_integral(desc) && MTYPE_is_integral(rtype)))
	return TRUE;
      return FALSE;
    }
  case OPR_TRUNC:
    if (rtype == MTYPE_I4 && desc == MTYPE_F4)
      return TRUE;
    else
      return FALSE;
  case OPR_CVT:
    if ((rtype == MTYPE_F8 || rtype == MTYPE_F4) && 
	(desc == MTYPE_I4 || desc == MTYPE_F4))
      return TRUE;
    else
      return FALSE;
  case OPR_INTRINSIC_OP:
    return TRUE;
  case OPR_PAREN:    
    return TRUE;
  case OPR_ABS:
    if (rtype == MTYPE_F4 || rtype == MTYPE_F8)
      return TRUE;
    else
      return FALSE;
  // BUG 5701: vectorize NEG for integers
  case OPR_NEG:
    if (rtype == MTYPE_C4)//why C4 is not good?
      return FALSE;
    else
      return TRUE;
  case OPR_ADD:
  case OPR_SUB:
    return TRUE;
  case OPR_MPY:
    if (rtype == MTYPE_F8 || rtype == MTYPE_F4)
      return TRUE;
    else if (rtype == MTYPE_I4) {
	    // I2MPY followed by I2STID is actually I4MPY followed by I2STID 
	    // We will distinguish between I4MPY and I2MPY in Is_Well_Formed_Simd
      return TRUE;
    } else if (Simd_vect_conf.Is_SSE3() && 
               (rtype == MTYPE_C4 || rtype == MTYPE_C8)) {
      // TODO: explain why requires SSE3
      return TRUE;      
    }

    return FALSE;

  case OPR_DIV:
    if (rtype == MTYPE_F8 || rtype == MTYPE_F4 || 
        (rtype == MTYPE_C4 && Simd_vect_conf.Is_SSE3()))
      return TRUE;
    else
      return FALSE;

  case OPR_MAX:
  case OPR_MIN:
    if (rtype == MTYPE_F4 || rtype == MTYPE_F8 || rtype == MTYPE_I4)
      return TRUE;
    else
      return FALSE;
  case OPR_BAND:
  case OPR_SHL:
  case OPR_BXOR:
    if (MTYPE_is_integral(rtype))
      return TRUE;
    else
      return FALSE;    
  case OPR_SQRT:
    if (rtype == MTYPE_F4 || rtype == MTYPE_F8)
      return TRUE;
    else
      return FALSE;
  case OPR_RSQRT:
#ifdef TARG_X8664
  case OPR_ATOMIC_RSQRT:
#endif
    if (rtype == MTYPE_F4)
      return TRUE;
    else
      return FALSE;
//BUG 10136: allows F8RECIP to be vectorized here, and V16F8RECIP
//           will be lowered down to DIV after LNO
  case OPR_RECIP:
    if (rtype == MTYPE_F4 || rtype == MTYPE_F8)
      return TRUE;
    else
      return FALSE;

  case OPR_PARM:
    return TRUE;
  default:
    return FALSE;
  }  
}


/*Determine whether an if-statement meets criteria for vectorization.
  The if-statement can be one of the following forms:
  if (x != 0) { single_istore_statement } else {empty_body}
  if (x != 0) { if (y !=0) {single_istore_statement} else {empty_body}} else {empty_body}
  The check on whether the sub-expressions of if-statement are vectorizable
  is done as for other statements by Gather_Vectorizable_Ops_Helper.
  To extend the vectorization of if-statements to include triple-nested 
  statements or more, consider using a bitmask in place of under_if/nested_if.
*/
BOOL Is_Vectorizable_If(WN* wn, BOOL inner) {
  BOOL safe;
  
  FmtAssert(WN_operator(wn) == OPR_IF, ("Not an IF statement\n"));
  WN *if_test = WN_if_test(wn);
  WN *if_then = WN_then(wn);
  WN *if_else = WN_else(wn);

  FmtAssert(WN_operator(if_else) == OPR_BLOCK, ("Else part of IF is not a block"));
  if (WN_first(if_else) != NULL)
    //non-empty else-clause
    return FALSE;

  // if-condition should be of the form (expr != 0)
  if (WN_operator(if_test) != OPR_NE)
    return FALSE;
  if (!((WN_operator(WN_kid1(if_test)) == OPR_INTCONST) && 
	WN_const_val(WN_kid1(if_test)) == 0))
    return FALSE;

  FmtAssert(WN_operator(if_then) == OPR_BLOCK, ("Then part of IF is not a block"));
  if (WN_first(if_then) != WN_last(if_then))
    //multiple statements in the body
    return FALSE; 

  //Nested-if: if (cond) {if (inner-cond) {...}}
  if (WN_operator(WN_first(if_then)) == OPR_IF) {
    if (inner)
      //triply nested-if.
      return FALSE;
    if (!Is_Vectorizable_If(WN_first(if_then), TRUE))
      return FALSE;
    //Handle nested-ifs only if the vector lengths of cond and inner-cond match,
    //since these two conditions will be fused for vectorization.
    WN *nested_if_test = WN_if_test(WN_first(if_then));
    int vlength_cond = Simd_Get_Vector_Type(if_test);
    int vlength_inner_cond = Simd_Get_Vector_Type(nested_if_test);
    if (vlength_cond != vlength_inner_cond)
      return FALSE;
    //Vector length must be V16I8 in order to use SSE4.1 instruction pcmpeqq and pblendvb
    if (vlength_cond != MTYPE_V16I8)
      return FALSE;  
    // The nested-if-condition must not have side effects, since its evaluation is speculative.
    if (WN_has_side_effects(nested_if_test))
	return FALSE;

    //Check if iloads in nested-if-condition are safe to speculate.
    std::list<WN *> outer_loads, inner_loads;
    //Collect iloads in the outer and inner if-conditions.
    WN_collect_iloads(&outer_loads, if_test); 
    //Collect iloads non-recursively since a match for the outer-most iload implies a match for any inner iloads.
    WN_collect_iloads_nr(&inner_loads, nested_if_test);
    std::list<WN *>::iterator outer_ld_iter, inner_ld_iter;
    for (inner_ld_iter = inner_loads.begin(); inner_ld_iter != inner_loads.end(); ++inner_ld_iter) {
      WN *inner_iload = *inner_ld_iter;
      safe = FALSE;
      //For a speculative iload in the inner if-condition to be safe, 
      //it must also be present in the outer if-condition.
      //The if-condition contains a single condition, so no need to check for short-circuit.
      for (outer_ld_iter = outer_loads.begin(); outer_ld_iter != outer_loads.end(); ++outer_ld_iter) {
	WN *outer_iload = *outer_ld_iter;
	if (Tree_Equiv(inner_iload, outer_iload)) {
	  safe = TRUE;
	  break;
	}
      }
      if (!safe)
	return FALSE;
    }
  }
  else {
    //the single statement in the if-body should be an istore.
    if (WN_operator(WN_first(if_then)) != OPR_ISTORE)
      return FALSE;
    //the RHS of the istore must not have side-effects because after vectorization, 
    //the RHS is always evaluated regardless of the if-condition.
    if (WN_has_side_effects(WN_kid0(WN_first(if_then))))
	return FALSE;

    //Check if iloads in if-body are safe to speculate.
    WN *outer_if_test = if_test;
    if(inner) {
      // This is a nested-if. Check for safe speculation must be done against outermost if-condition.
      WN *loop = Enclosing_Do_Loop(if_test);
      outer_if_test = WN_if_test(WN_first((WN_do_body(loop)))); 
    }
    std::list<WN *> outer_loads, then_loads;
    //Collect iloads in the outer if-condition.
    WN_collect_iloads(&outer_loads, outer_if_test); 
    //Collect iloads from RHS of the istore in the if-body.
    WN_collect_iloads_nr(&then_loads, WN_kid0(WN_first(if_then))); 
    std::list<WN *>::iterator outer_ld_iter, then_ld_iter;
    for (then_ld_iter = then_loads.begin(); then_ld_iter != then_loads.end(); ++then_ld_iter) {
      WN *then_iload = *then_ld_iter;
      safe = FALSE;
      //For a speculative iload in the if-body to be safe, 
      //it must also be present in the outer if-condition.
      for (outer_ld_iter = outer_loads.begin(); outer_ld_iter != outer_loads.end(); ++outer_ld_iter) {
	WN *outer_iload = *outer_ld_iter;
	if (Tree_Equiv(then_iload, outer_iload)) {
	  safe = TRUE;
	  break;
	}
      }
      if (!safe)
	return FALSE;
    }
    //Check whether the istore in the if-body is safe to speculate.
    WN *istore = WN_first(if_then);
    //For the speculative istore (in the if-body) to be safe, 
    //there must be an iload from the same address present in the outer if-condition.
    safe = FALSE;
    for (outer_ld_iter = outer_loads.begin(); outer_ld_iter != outer_loads.end(); ++outer_ld_iter) {
      WN *outer_iload = *outer_ld_iter;
      if (Tree_Equiv(WN_kid1(istore), WN_kid0(outer_iload))) {
	if ((WN_offset(istore) == WN_offset(outer_iload)) &&
	    (WN_ty(istore) == WN_load_addr_ty(outer_iload)) &&
	    (WN_desc(istore) == WN_desc(outer_iload))) {
	  safe = TRUE;
	  break;
	}
      }
    }
    if (!safe)
      return FALSE;
  }
  //Vector length of condition and body of if-statement must match
  int vlength_cond = Simd_Get_Vector_Type(if_test);
  int vlength_body = Simd_Get_Vector_Type(WN_first(if_then));
  if (vlength_cond != vlength_body)
    return FALSE;
  if (vlength_cond != MTYPE_V16I8)
    return FALSE;
    
  WN *if_parent =LWN_Get_Parent(wn);
  if (WN_operator(if_parent) != OPR_BLOCK)
    return FALSE;

  return TRUE;
}

extern WN *find_loop_var_in_simple_ub(WN* loop); // defined in vintr_fission.cxx

typedef enum {
  Invariant=0,	
  Reference=1,
  Simple=2,
  Complex=3
} SIMD_OPERAND_KIND;

static SIMD_OPERAND_KIND simd_operand_kind(WN* wn, WN* loop) {
  OPERATOR opr=WN_operator(wn);

  if (opr==OPR_PARM) {
    if (WN_Parm_By_Reference(wn))
      return Reference;
    wn=WN_kid0(wn);
    opr=WN_operator(wn);
  }

  // Recognize 2 operand invariant expressions.
  // Should eventually be generalized to any 2 operand operation.
  if (BINARY_OP(opr)) {
    if ((simd_operand_kind(WN_kid0(wn), loop) == Invariant) &&
        (simd_operand_kind(WN_kid1(wn), loop) == Invariant))
      return Invariant;
  }

  if (opr==OPR_CONST || opr==OPR_INTCONST) {
    return Invariant;
  } else if (opr==OPR_LDA) {
    return Reference;
  } else if (opr==OPR_LDID) {
    SYMBOL symbol1(wn);
    SYMBOL symbol2(WN_index(loop));
    if (symbol1==symbol2)
      return Complex;
    DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(wn);
    WN* body=WN_do_body(loop);
    DEF_LIST_ITER d_iter(def_list);
    for (DU_NODE* dnode=d_iter.First(); !d_iter.Is_Empty();
                  dnode=d_iter.Next()) {
      WN* def=dnode->Wn();
      WN* stmt=Find_Stmt_Under(def,body);
      if (stmt!=NULL)
        return Complex;
    }
    return Invariant;
  } else if (opr==OPR_ILOAD) {
    if (WN_kid_count(wn) != 1 || WN_offset(wn) != 0  ||
        WN_operator(WN_kid0(wn)) != OPR_ARRAY)
      return Complex;

    ACCESS_ARRAY* aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,WN_kid0(wn));

    if (aa->Too_Messy)
      return Complex;

    ACCESS_VECTOR* av;
    INT loopno=Do_Loop_Depth(loop);

    BOOL seen_non_zero=FALSE;
    for (INT i=0; i<aa->Num_Vec(); i++) {
      av=aa->Dim(i);
      if (av->Too_Messy || av->Non_Lin_Symb)
        return Complex;
      if ((av->Non_Const_Loops() > loopno))
        return Complex;
      if (av->Loop_Coeff(loopno)!=0 && i != aa->Num_Vec()-1)
	return Reference;
      if (av->Loop_Coeff(loopno)!=0)
        if (seen_non_zero) // cannot have two non-zero
          return Complex;
        else
          seen_non_zero=TRUE;
    }
    if (!seen_non_zero)
      return Invariant;
    return Simple;
  } else if (opr==OPR_ISTORE) {
    if (WN_offset(wn) != 0  ||
        WN_operator(WN_kid1(wn)) != OPR_ARRAY)
      return Complex;

    ACCESS_ARRAY* aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,WN_kid1(wn));

    if (aa->Too_Messy)
      return Complex;

    ACCESS_VECTOR* av;
    INT loopno=Do_Loop_Depth(loop);

    BOOL seen_non_zero=FALSE;
    for (INT i=0; i<aa->Num_Vec(); i++) {
      av=aa->Dim(i);
      if (av->Too_Messy || av->Non_Lin_Symb)
        return Complex;
      if ((av->Non_Const_Loops() > loopno))
        return Complex;
      if (av->Loop_Coeff(loopno)!=0 && i != aa->Num_Vec()-1)
	return Reference;
      if (av->Loop_Coeff(loopno)!=0)
        if (seen_non_zero) // cannot have two non-zero
          return Complex;
        else
          seen_non_zero=TRUE;
    }
    if (!seen_non_zero)
      return Invariant;
    return Simple;
  }

  return Complex;  
}


// bug 8931 : prototype first
BOOL Is_Vectorizable_Intrinsic(WN *wn);

//whether or not it is used for array addresses calculation.
static BOOL Is_Under_Array(WN *wn)
{
    WN* parent = LWN_Get_Parent(wn);
    while(parent && WN_operator(parent) != OPR_DO_LOOP) {
      if (WN_operator(parent) == OPR_ARRAY)
        return TRUE;
      parent = LWN_Get_Parent(parent);
    }
  return FALSE;
}

/* Are the loads and stores in this tree node all unaligned ?
 * Better said, are we going to mark down all the vector loads and vector 
 * stores as unaligned, because we can not determine alignment at compile time.
 * If so, then it may not be that useful to do SIMD after all.
 * Right now, we consider only formal parameters in this category. 
 * Under certain conditions, we override this heuristic.
 * (1) If there is one or more OPR_RECIP in the loop body
 * (2) If the SIMD operand size is < 8 bytes.
 */
static BOOL Simd_Benefit (WN* wn) {

  int store_granular_size = (LNO_Iter_threshold) ? 8 : 4;

  if (LNO_Loop_Model_Simd) {
    // vector-length should be >= 2
    store_granular_size = Simd_vect_conf.Get_Vect_Byte_Size () / 2;
  }

  if (LNO_Run_Simd == 0)
    return FALSE;
  else if (LNO_Run_Simd == 2)
    return TRUE;

  if (wn == NULL)
    return FALSE;

  OPERATOR opr = WN_operator(wn);

  // Bug 5582: If the CVT is for array address calculation and can not be vectorized
  // we can not say CVT is beneficial. Let others(e.g. alignment) to make decision.
  if (opr == OPR_CVT && 
     (!Is_Under_Array(wn) || is_vectorizable_op(opr, WN_rtype(wn), WN_desc(wn)))) 
   return TRUE;

    if((opr == OPR_RECIP && WN_rtype(wn) == MTYPE_F4) || 
        opr == OPR_SQRT || opr == OPR_TRUNC)
    return TRUE;

 //Bug 8931 : according to (1), this routine is not just for alignment. It also
 //           contains beneficial info. So, I think if a loop contains a intrinsic
 //           that can be vectorized, it is also ok if OPT:fast_math=on or simd=2.
   if(Is_Vectorizable_Intrinsic(wn))
    return TRUE;

  if (OPCODE_is_store(WN_opcode(wn)) &&
      (MTYPE_byte_size(WN_desc(wn)) <= store_granular_size ||
       MTYPE_is_complex(WN_desc(wn)) || opr == OPR_STID))
    return TRUE;

  if (WN_operator(wn) == OPR_ARRAY &&
      WN_has_sym(WN_array_base(wn)) &&
      // can not align if array base is a pointer: bug 8595
      WN_operator(WN_array_base(wn)) != OPR_LDID &&
      ST_sclass(WN_st(WN_array_base(wn))) != SCLASS_FORMAL)
    return TRUE;

  if (WN_opcode(wn) == OPC_BLOCK) 
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      if (Simd_Benefit(stmt))
	return TRUE;
      stmt=next_stmt;
    }

  for (UINT kidno = 0; kidno < WN_kid_count(wn); kidno ++) {
    if (Simd_Benefit(WN_kid(wn, kidno)))
      return TRUE;
  }

  return FALSE;
}

//***********************************************************************
//Bug 9143 : merge two similar functions into one. This is essentially
// to say if the array is aligned, operations on double-precision value
// can also be benefited from simd
//
// NOTE: the original Is_Vectorization_Beneficial is commented out but
//       still there for comparison
//***********************************************************************
extern BOOL Is_Vectorization_Beneficial (WN* wn)
{
  return Simd_Benefit(wn);
}

static BOOL Is_Vectorizable_Tree (WN* tree)
{
   if(tree == NULL) return TRUE; //no kid2 is fine

   OPERATOR opr = WN_operator(tree);
   if(opr == OPR_ILOAD || opr == OPR_LDID ||
      opr == OPR_CONST || opr == OPR_INTCONST)
    return TRUE; 

  if (!is_vectorizable_op(opr, WN_rtype(tree), WN_desc(tree)))
    return FALSE;

  if (WN_kid_count(tree) > 2)
    return FALSE;

  for(INT i=0; i<WN_kid_count(tree); i++){
    if(!Is_Vectorizable_Tree(WN_kid(tree, i)))
      return FALSE;
  } 
  return TRUE;
}

static BOOL Array_Subscript_Uses_IV (WN *wn, SYMBOL loop_var)
{
  if (WN_operator(wn) == OPR_LDID) {
    SYMBOL symbol(wn);
    if (symbol == loop_var)
      return TRUE;
  }
  for (INT kid = 0; kid < WN_kid_count(wn); kid ++)
    if (Array_Subscript_Uses_IV(WN_kid(wn, kid), loop_var))
      return TRUE;
  return FALSE;
}

// Bug 4932 - Accesses like e[c[j]][j] in a j-loop should not be 
// vectorized but accesses like e[c[i][j] in a j-loop can be vectorized.
// Unfortunately, the loop coefficients are not computed for such cases.
// The access vector is marked too messy but we have to distinguish
// these two cases.      
static BOOL Identify_Messy_Array_Subscript (WN* array, WN* loop, 
					    ACCESS_ARRAY* aa, 
					    INT i /* current dim */)
{
  WN* index;
  if (WN_num_dim(array) == aa->Num_Vec()) {
    Is_True(i + 1 + WN_num_dim(array) < WN_kid_count(array), ("NYI"));
    if (i + 1 + WN_num_dim(array) < WN_kid_count(array)) {
      index = WN_kid(array, i + 1 + WN_num_dim(array));
      SYMBOL symbol(WN_index(loop));
      if (Array_Subscript_Uses_IV(index, symbol))
	return TRUE;
    }
  }		
  return FALSE;
}

// two names for diagnostics
static const char *non_unit_stride;
static char *non_vect_op;

// If not possible to determine, we will do runtime checking
// Bug 6606 - Additional checks for assumed shape arrays (ARRAY with -ve
// element size) when identifying vectorizable loads and stores :
// (1) if the WN offset > 0, then it can not be vectorized (this is a
//     practical observation that speeds up the check) and,
// (2) Bug 10379: loading an array of structure is sure to be non-contiguous
//                if the structure has more than one field
static BOOL Possible_Contiguous_Dope(WN *wn)
{   
   if(WN_element_size(wn) < 0 &&
      WN_operator(WN_array_base(wn)) == OPR_LDID){ 
    if (WN_offset(LWN_Get_Parent(wn)) > 0) 
       return FALSE; // case (1)
   TY_IDX ty_high = WN_ty(WN_array_base(wn));
   if(TY_kind(ty_high) == KIND_POINTER){
     TY_IDX ty_point_to = TY_pointed(ty_high);
   if(TY_kind(ty_point_to) == KIND_ARRAY){
     TY_IDX ty_ele = TY_etype(ty_point_to);
    if(TY_kind(ty_ele) == KIND_STRUCT){
      UINT fld_id = 0;
      if (!FLD_last_field(FLD_get_to_field(ty_ele, 1, fld_id)))
        return FALSE; //case 2
     } // end kind_structure
    }//end kind_array
   }// end kind_pointer
  }
 return TRUE;
}

//  Returns TRUE if all uses in the expression 'wn' are invariant 
//  w.r.t the <loop> when it shifts to the innermost position.
//
//  This function assume that it is legal to move the <loop> in the innermost pos
//
//  The <innermost> is the current innermost loop. If <loop> is already in 
//  the innermost position, <loop> should be equal to <innermost>.
//
static BOOL Is_Loop_Invariant_Helper (WN* wn, WN* loop, WN* innermost);
static BOOL Is_Loop_Invariant (WN* wn, WN* loop, WN* innermost)
{
  if (loop == innermost) {
    return Is_Loop_Invariant_Exp (wn, loop); 
  }

  return Is_Loop_Invariant_Helper (wn, loop, innermost);
}

// Helper function of Is_Loop_Invariant_Helper(). 
// Returns true if:
//    - the wn is inside innermost loop, or 
//    - it is init and fini expression of the loop being examined. 
// Returns false if: 
//     - the wn is in imperfect part in the loop nests. Does not matter it is imperfect part which loop. 
//     - not inside the loop at all
// Consider a general case:
//  wn0   
//  loop1 (i = init; i <= fini; i++) {
//       wn1 
//       loop 2{ 
//          wn 2
//           loop 3 {
//              wn3
//           }
//      }
//  }
//  - Is_WN_Inside_Loop(wn3, loop1, loop3/*innermost*/) returns true
//  - Is_WN_Inside_Loop(wn2, loop1, loop3/*innermost*/) returns false 
//  - Is_WN_Inside_Loop(wn1, loop1, loop3/*innermost*/) returns false 
//  - Is_WN_Inside_Loop(wn0, loop1, loop3/*innermost*/) returns false 
//  - Is_WN_Inside_loop(<wn-"init"/wn-"fini", loop1, loop3) return TRUE since it is part of loop construct. 
static BOOL Is_WN_Inside_Loop (WN* wn, WN* loop, WN* innermost)
{
    Is_True (WN_operator (loop) == OPR_DO_LOOP && 
             WN_operator (innermost) == OPR_DO_LOOP, ("invalid input"));

    for (WN* ancestor = wn; ancestor; ancestor = LWN_Get_Parent(ancestor)) {
        if (ancestor == WN_do_body (innermost))
	  // <wn> is inside <innermost> loop.
	  return TRUE;

        if ((ancestor == WN_index(loop)) || (ancestor == WN_start(loop)) || 
	    (ancestor == WN_end(loop)) || (ancestor == WN_step(loop)))
	  // <wn> is inside one of the components of <loop> other than its body.
	  return TRUE;

	if (ancestor == loop) 
	  // <wn> is in the body of <loop> either in its imperfect part or 
	  // as part of another loop nested inside <loop>.
	  return FALSE;
    }
    //<wn> is outside <loop>
    return FALSE;
}

static BOOL Is_Loop_Invariant_Helper (WN* wn, WN* loop, WN* innermost)
{
    ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
    if (!dg) return FALSE;

    OPERATOR opr = WN_operator(wn); 

    if (OPERATOR_is_call (opr)) {
        // we should not come across a call.
        return FALSE;
    }

    if (opr == OPR_ILOAD) {
        VINDEX16 v = dg->Get_Vertex(wn); 
        if (v == 0) 
            return FALSE; 

        EINDEX16 e = 0; 
        for (e = dg->Get_In_Edge(v); e; e = dg->Get_Next_In_Edge(e)) { 
            VINDEX16 v_source = dg->Get_Source(e);
            WN* wn_source = dg->Get_Wn(v_source);
            if (Is_WN_Inside_Loop (wn_source, loop, innermost))
	            return FALSE; 
        }

        for (INT kid = 0; kid < WN_kid_count(wn); kid++) {
            if (!Is_Loop_Invariant_Helper (WN_kid(wn, kid), loop, innermost))
	            return FALSE; 
        }
        return TRUE; 
    } else if (opr == OPR_INTRINSIC_OP || opr == OPR_PURE_CALL_OP) { 
        for (INT i = 0; i < WN_kid_count(wn); i++) {
            WN* wn_parm_node = WN_kid(wn, i);
            if (WN_Parm_By_Reference(wn_parm_node))
                return FALSE; 

            WN* wn_parameter = WN_kid0(wn_parm_node); 
            if (!Is_Loop_Invariant_Helper (wn_parameter, loop, innermost))
                return FALSE; 
        }
        return TRUE;  
    } else if (opr == OPR_LDID) {
        DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
	if (!def_list || def_list->Incomplete())
	  return FALSE;
        DEF_LIST_ITER iter(def_list);
        const DU_NODE* node = NULL;
        for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
            WN* def = node->Wn();
            if (Is_WN_Inside_Loop (def, loop, innermost))
                return FALSE;
        }
        return TRUE; 
    } else {
        if (!Statically_Safe_Node(wn)) 
            return FALSE; 

        for (INT kid = 0; kid < WN_kid_count(wn); kid++)
            if (!Is_Loop_Invariant_Helper (WN_kid(wn, kid), loop, innermost))
                return FALSE;
    }
    return TRUE; 
}


// Helper function of Unit_Stride_Reference. Do *NOT* call this function 
// directly.
//
static BOOL Unit_Stride_Reference_Helper(
                WN *wn, 
                WN *loop, 
                WN *innermost,
                BOOL in_simd)
{

    BOOL ok = TRUE;

    if (WN_opcode(wn) == OPC_BLOCK){
      WN* kid = WN_first (wn);
      while (kid) {
        if(!Unit_Stride_Reference_Helper(kid, loop, innermost, in_simd))
           return FALSE;
        kid = WN_next(kid);
    } // end while
      return TRUE; // I think this is not necessary
    }// endif

    if(WN_operator(wn) == OPR_ARRAY && 
       (in_simd || !Is_Loop_Invariant(wn, loop, innermost))){
      
      ACCESS_ARRAY* aa = (ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map, wn);
      ACCESS_VECTOR* av;
      INT loopno = Do_Loop_Depth(loop);
      
      for (INT i = 0; i < aa->Num_Vec(); i ++) {
        av = aa->Dim(i); //for each diemnsion     
       if (av->Loop_Coeff(loopno)!=0 && i != aa->Num_Vec()-1)
         ok = FALSE;
       if (i == aa->Num_Vec()-1 && av->Loop_Coeff(loopno) != 1 &&
          av->Loop_Coeff(loopno) != -1)
          ok = FALSE;
       // There is no instruction to shuffle I1/U1.
       if (i == aa->Num_Vec()-1 && av->Loop_Coeff(loopno) == -1 &&
           ABS(WN_element_size(wn)) == 1)
           ok = FALSE;
       // Bug 4984 - non-linear symbols in the last dimension (access[i+i*l]).
       if (i == aa->Num_Vec()-1 && av->Contains_Non_Lin_Symb())
           ok = FALSE;
       // bug 9858 : a(i+(i*(i+1))/2) -- array is too messy and contains the index variable
       //            we can not vectorize it due to non-unit stride
       if (av->Too_Messy &&
          Identify_Messy_Array_Subscript(wn, loop, aa, i))
          ok = FALSE;
          if(!ok) break;
        } // end for each dimension
  
       if (ok && PU_src_lang(Get_Current_PU()) == PU_F90_LANG)
           ok = Possible_Contiguous_Dope(wn); // Bug 6606, 10379
         if(!ok){ 
           if(in_simd && (debug || LNO_Simd_Verbose)){
            if(WN_has_sym(WN_array_base(wn))){
             SYMBOL array_symbol(WN_array_base(wn));
            non_unit_stride = array_symbol.Name();
            }
           }           
            return FALSE;
         } /// ok
      }// end if array

    for (UINT kidno = 0; kidno < WN_kid_count(wn); kidno ++) {
       if(!Unit_Stride_Reference_Helper(WN_kid(wn, kidno), 
                                        loop, innermost, in_simd)) {
          return FALSE;
       }
     }

   return TRUE;
}

static BOOL Unit_Stride_Reference (
                WN *wn, 
                WN *loop, 
                BOOL in_simd)
{
   FmtAssert (simd_context == SC_SIMD && in_simd && Do_Loop_Is_Inner (loop) ||
              simd_context == SC_OTHER && Do_Loop_Is_Inner (loop) ||
              simd_context == SC_LOOP_MODELING, 
              ("impossible"));
    
    WN* innermost = loop;
    if (!Do_Loop_Is_Inner (loop)) {
       innermost = SNL_Innermost_Do (loop);
    }

    return Unit_Stride_Reference_Helper (wn, loop, innermost, in_simd);
}

static void Report_Non_Vectorizable_Op(WN *wn)
{
  if(non_vect_op) return; //already reported by sub-ops

  if(debug || LNO_Simd_Verbose){
  if(WN_operator(wn) == OPR_PAREN)// report child
    non_vect_op = OPCODE_name(WN_opcode(WN_kid0(wn)));
  else non_vect_op = OPCODE_name(WN_opcode(wn));
 } 
}

//whether the statement is going to be unrolled, if vectorized
static BOOL Is_Unroll_Statement(WN *stmt, WN *body)
{
  for(WN *tmp=WN_first(body); tmp; tmp=WN_next(tmp)){
    if(WN_operator(tmp)==OPR_ISTORE || WN_operator(tmp)==OPR_STID){
     if(MTYPE_byte_size(WN_desc(stmt)) > MTYPE_byte_size(WN_desc(tmp)))
       return TRUE;
    }
  }
  return FALSE;
}


// Bug 5208
// We support only one induction type per loop. To support more than one type,
// we need to split the loop or generate multiple vec_index_preg and incr 
// stores. We will screen out such loops.
INT Induction_Type_Size;
BOOL Induction_Seen;
BOOL Inconsistent_Induction;


static BOOL Is_Well_Formed_Simd ( WN* wn, WN* loop)
{
  // for vectorizing copies
  if (WN_operator(wn) == OPR_ILOAD) {
    if (WN_operator(LWN_Get_Parent(wn)) == OPR_ISTORE)
      return TRUE;
    else
      return FALSE;
  }
  
  WN* parent = LWN_Get_Parent(wn);
  WN* kid0 = WN_kid0(wn);
  WN* kid1 = WN_kid1(wn);

  if (WN_kid_count(wn) > 2 && WN_operator(wn) != OPR_SELECT)
    return FALSE;

  if (WN_operator(wn) == OPR_SELECT) {
    if (!OPCODE_is_compare(WN_opcode(kid0)) ||
	!MTYPE_is_float(WN_desc(kid0)) || 
	!MTYPE_is_integral(WN_rtype(kid0)))
      return FALSE;
    kid0 = WN_kid1(wn);
    kid1 = WN_kid2(wn);
  }
 
  // For all vectorized versions of the shift-left operation psll(w/d/q/dq), 
  // each  w/d/q/dq in the first operand is left shifted by the same number 
  // of bits given by the second argument. Hence for a scalar shift in a 
  // loop to be vectorized, the second operand to the shift must be a loop invariant. 
  if (WN_operator(wn) == OPR_SHL) {
    SIMD_OPERAND_KIND shl_op_kind = simd_operand_kind(WN_kid1(wn), LWN_Get_Parent(WN_do_body(loop)));
    if (shl_op_kind != Invariant)
      return FALSE;
    // cannot vectorize a 128-bit or 8-bit shift since there is no corresponding vectorized instruction.
    if (WN_rtype(wn) == MTYPE_I16 || WN_rtype(wn) == MTYPE_U16 ||
	WN_rtype(wn) == MTYPE_I1  || WN_rtype(wn) == MTYPE_U1)
      return FALSE;
  }

  if (OPCODE_is_compare(WN_opcode(wn)) && (WN_operator(parent) != OPR_SELECT)
      && (!under_if && WN_operator(parent) != OPR_IF))
    return FALSE;

  //Bug 10148: don't vectoorize F8RECIP if it is MPY's child
  //Bug 13853: V16F8RECIP will be transformed to vector division in code generation
  //           And this has been observed good or no harm to intel platforms and
  //           Barcelona
  if(!Is_Target_EM64T() && 
     !Is_Target_Core() && 
     !Is_Target_Wolfdale() &&
     !Is_Target_Barcelona() &&
     !Is_Target_Orochi() &&
     WN_operator(wn) == OPR_RECIP && WN_rtype(wn) == MTYPE_F8
     && WN_operator(parent) == OPR_MPY)
   return FALSE; 
    
  if (!LNO_Simd_Reduction) {
    if (WN_operator(parent) == OPR_STID)
      return FALSE;

    if (WN_operator(kid0) == OPR_LDID &&
	(WN_desc(kid0) == MTYPE_I1 ||
	 WN_desc(kid0) == MTYPE_I2 ||
        WN_desc(parent) == MTYPE_I1 ||
        WN_desc(parent) == MTYPE_I2))
      return FALSE;

   if (kid1 && WN_operator(kid1) == OPR_LDID &&
       (WN_desc(kid1) == MTYPE_I1 ||
        WN_desc(kid1) == MTYPE_I2 ||
        WN_desc(parent) == MTYPE_I1 ||
        WN_desc(parent) == MTYPE_I2))	 
     return FALSE;
  
  }

  if (WN_operator(wn) == OPR_MPY && WN_rtype(wn) == MTYPE_I4 &&
      WN_desc(parent) != MTYPE_I2){
      if((WN_operator(kid0) == OPR_INTCONST && WN_const_val(kid0) == 2  &&
      WN_operator(kid1) == OPR_ILOAD && WN_operator(WN_kid0(kid1)) == OPR_ARRAY)
      ||(WN_operator(kid1) == OPR_INTCONST && WN_const_val(kid1) == 2  &&
      WN_operator(kid0) == OPR_ILOAD && WN_operator(WN_kid0(kid0)) == OPR_ARRAY)
      ); //bug 5844: 2*b[i] should be fine
     else return FALSE;
  }
  
  // F4 REDUCE_MPY and REDUCE_ADD are known to be inaccurate with other 
  // compilers also. This is true for Opteron and EM64T.
  // (Bug 4636 - lapack failure with -msse3 is attributable to difference
  // in association introduced due to REDUCE style operations. SSE3 instructions
  // show more variability due to roundoff than their SSE2 counterparts).
  // Bug 4426 - associativity problem shows up with F8 type also.
  if (MTYPE_is_float(WN_desc(parent)) && WN_operator(parent) == OPR_STID && 
      LNO_Run_Simd != 2 && Roundoff_Level == ROUNDOFF_NONE &&
      // bug 2456 - scalar expansion test may pass down non-reduction 
      // statements and we don't care if OPR_MPY/ADD is in the middle of the 
      // expression.
      ((WN_operator(wn) == OPR_MPY &&
        curr_simd_red_manager->Which_Reduction(parent) == RED_MPY) ||
       (WN_operator(wn) == OPR_ADD &&
        curr_simd_red_manager->Which_Reduction(parent) == RED_ADD)))
    return FALSE;

  // bug 8766: does not yet know how to do simd for a reduction loop inside a
  //	       parallel region
  if (Do_Loop_Is_Mp(loop) && 
      WN_operator(parent) == OPR_STID && curr_simd_red_manager != NULL &&
      curr_simd_red_manager->Which_Reduction(parent) != RED_NONE)
    return FALSE;
     
  if (WN_operator(parent) != OPR_ISTORE && WN_operator(parent) != OPR_STID &&
      !(under_if && WN_operator(parent) == OPR_IF) && 
      !is_vectorizable_op(WN_operator(parent), 
			  WN_rtype(parent), WN_desc(parent)))
    return FALSE;

  if (WN_operator(kid0) == OPR_ILOAD) {
    WN* array0 = WN_kid0(kid0);
    if (WN_operator(array0) == OPR_ARRAY &&
	WN_operator(WN_kid0(array0)) != OPR_LDID &&
	WN_operator(WN_kid0(array0)) != OPR_LDA) {
      // Bug 5057 - tolerate base addresses of the form (+ const LDID).
      // Bug 6649 - vectorize things like struct[index].array[loop_index]
      //            where is base is const + ARRAY
      if (WN_operator(WN_kid0(array0)) == OPR_ADD) {
	WN* opnd0 = WN_kid0(WN_kid0(array0));
	WN* opnd1 = WN_kid1(WN_kid0(array0));
	if (((WN_operator(opnd0) == OPR_LDID || WN_operator(opnd0) == OPR_ARRAY) &&
	     WN_operator(opnd1) == OPR_INTCONST) ||
	    ((WN_operator(opnd1) == OPR_LDID||WN_operator(opnd0) == OPR_ARRAY) &&
	     WN_operator(opnd0) == OPR_INTCONST))
	  ;
	else
	  return FALSE;
      } else
	return FALSE;
    }
  }
  if (WN_kid_count(wn) > 1 && 
      WN_operator(kid1) == OPR_ILOAD) {
    WN* array1 = WN_kid0(kid1);
    if (WN_operator(array1) == OPR_ARRAY &&
	WN_operator(WN_kid0(array1)) != OPR_LDID &&
	WN_operator(WN_kid0(array1)) != OPR_LDA) {
      // Bug 5057 - tolerate base addresses of the form (+ const LDID).
      // Bug 6649 - vectorize things like struct[index].array[loop_index]
      //            where is base is const + ARRAY
      if (WN_operator(WN_kid0(array1)) == OPR_ADD) {
	WN* opnd0 = WN_kid0(WN_kid0(array1));
	WN* opnd1 = WN_kid1(WN_kid0(array1));
	if (((WN_operator(opnd0) == OPR_LDID || WN_operator(opnd0) == OPR_ARRAY) &&
	     WN_operator(opnd1) == OPR_INTCONST) ||
	    ((WN_operator(opnd1) == OPR_LDID || WN_operator(opnd0) == OPR_ARRAY) &&
	     WN_operator(opnd0) == OPR_INTCONST))
	  ;
	else
	  return FALSE;
      } else
	return FALSE;
    }
  }

  //bug 14155: we don't know how to unroll the statement, so give up for now
  if(WN_operator(kid0) == OPR_ILOAD && WN_operator(WN_kid0(kid0)) == OPR_ARRAY){
    WN * stmt = Find_Stmt_Under(wn, WN_do_body(loop));
    if(stmt && WN_operator(stmt)==OPR_STID && Is_Unroll_Statement(stmt, WN_do_body(loop))){
     if(WN_element_size(WN_kid0(kid0)) != MTYPE_byte_size(WN_desc(stmt)))
      return FALSE;
    }
  }
  if(kid1 && WN_operator(kid1) == OPR_ILOAD && WN_operator(WN_kid0(kid1)) == OPR_ARRAY){
    WN * stmt = Find_Stmt_Under(wn, WN_do_body(loop));
    if(stmt && WN_operator(stmt)==OPR_STID && Is_Unroll_Statement(stmt, WN_do_body(loop))){
     if(WN_element_size(WN_kid0(kid1)) != MTYPE_byte_size(WN_desc(stmt)))
      return FALSE;
    }
  }
 
  if(!Is_Vectorizable_Tree(kid0)||!Is_Vectorizable_Tree(kid1))
   return FALSE;

  // Two invariant operands
  if (WN_kid_count(wn) == 2 &&
      ((WN_operator(kid0) == OPR_CONST || WN_operator(kid0) == OPR_INTCONST) &&
       (WN_operator(kid1) == OPR_CONST || WN_operator(kid1) == OPR_INTCONST)))
    return FALSE;  

  if (WN_operator(kid0) == OPR_LDID) {
    SYMBOL symbol1(kid0);
    SYMBOL symbol2(WN_index(loop));
    if (symbol1 == symbol2) {
      // Bug 7255 - induction loop in MP region needs special treatment.
      if (Do_Loop_Is_Mp(loop)) 
	return FALSE;
      INT Type_Size = MTYPE_byte_size(WN_rtype(wn));
      if (WN_operator(wn) == OPR_CVT) 
	Type_Size = MTYPE_byte_size(WN_desc(wn));
      if (Induction_Seen &&
	  Type_Size != Induction_Type_Size) {
	Inconsistent_Induction = TRUE;
	return FALSE;
      } 
      Induction_Seen = TRUE;
      Induction_Type_Size = Type_Size;
    }
  }

  if (kid1 && WN_operator(kid1) == OPR_LDID) {
    SYMBOL symbol1(kid1);
    SYMBOL symbol2(WN_index(loop));
    if (symbol1 == symbol2) { 
      // Bug 7255 - induction loop in MP region needs special treatment.
      if (Do_Loop_Is_Mp(loop)) 
	return FALSE;
      INT Type_Size = MTYPE_byte_size(WN_rtype(wn));
      if (WN_operator(wn) == OPR_CVT) 
	Type_Size = MTYPE_byte_size(WN_desc(wn));
      if (Induction_Seen &&
	  Type_Size != Induction_Type_Size) {
	Inconsistent_Induction = TRUE;
	return FALSE;
      }
      Induction_Seen = TRUE;
      Induction_Type_Size = Type_Size;
    }
  }

  if ((WN_operator(kid0) == OPR_ILOAD && WN_field_id(kid0) != 0) ||
      (kid1 && WN_operator(kid1) == OPR_ILOAD && WN_field_id(kid1) != 0) ||
      (WN_operator(parent) == OPR_ISTORE && WN_field_id(parent) != 0))
    return FALSE;
     
  // Can not vector copy different sized arrays. 
  // The elements are not contiguous
  WN* stmt = parent; // bug 2336 - trace up the correct type
  while(stmt && !OPCODE_is_store(WN_opcode(stmt)) && 
	WN_operator(stmt) != OPR_DO_LOOP){
    stmt = LWN_Get_Parent(stmt);
  }    
  if (stmt && WN_operator(stmt) != OPR_DO_LOOP &&
      (WN_operator(kid0) == OPR_ILOAD && WN_rtype(kid0) != WN_desc(kid0) &&
       WN_desc(kid0) != WN_desc(stmt)) ||
      (kid1 && WN_operator(kid1) == OPR_ILOAD && 
       WN_rtype(kid1) != WN_desc(kid1) &&
       WN_desc(kid1) != WN_desc(stmt)))
    return FALSE;

  // Bug 568
  // For Fortran loops that copy array sections, the lowerer may return
  // fields in structure without a field Id. Also, we can not
  // rely on the offset field in the ISTORE which may be 0 (first field in
  // a structure). To work around the problem, we compare the size of the 
  // array nodes.
  // Revised for bug 4554 after 2.0 change for bug 3359.
  if (WN_operator(wn) != OPR_INTRINSIC_OP &&
      //bug 13853 : should not apply this to things related to select 
      !OPCODE_is_compare(WN_opcode(wn))   && 
      WN_operator(wn) != OPR_SELECT) {
    INT oper_size = -1, opnd_size = -1;
    WN* address;

    if (WN_operator(parent) == OPR_ISTORE) {
      address = WN_kid1(parent);
      if (WN_operator(address) != OPR_ARRAY)
	return FALSE;
      else {
	if (WN_element_size(address) > 8)
	  return FALSE;
	else 
	  oper_size = ABS(WN_element_size(address));
      } 
    } else {
      oper_size = MTYPE_byte_size(WN_rtype(wn));
      if (WN_rtype(wn) == MTYPE_V)
	oper_size = MTYPE_byte_size(WN_desc(wn));
    }
    
    for (INT kid_count = 0; kid_count < WN_kid_count(wn); 
	 kid_count ++) {
      WN* kid = WN_kid(wn, kid_count);

      if (WN_operator(kid) == OPR_ILOAD) {
	address = WN_kid0(kid);
	if (WN_operator(address) != OPR_ARRAY)
	  return FALSE;
	else {
	  if (WN_element_size(address) > 8)
	    return FALSE;
	  else 
	    opnd_size = ABS(WN_element_size(address));
	} 
      } else {
	opnd_size = MTYPE_byte_size(WN_rtype(kid));
	if (WN_rtype(kid) == MTYPE_V)
	  opnd_size = MTYPE_byte_size(WN_desc(kid));
      }
      
      if (opnd_size != oper_size && WN_operator(wn) != OPR_PARM &&
	  WN_operator(wn) != OPR_CVT && WN_operator(wn) != OPR_TRUNC)
	return FALSE;
      if (WN_operator(wn) == OPR_CVT || WN_operator(wn) == OPR_TRUNC) {
	INT rsize = MTYPE_byte_size(WN_rtype(wn));
	INT dsize = MTYPE_byte_size(WN_desc(wn));
	if (rsize != oper_size || dsize != opnd_size)
	  return FALSE;
      }
    }
  }

  // Bug 2962
  // Can not vectorize "a[i].b = " ; this is not caught by the other checks 
  // because sometimes field id is not set and even if set it may be zero.
  if (WN_operator(parent) == OPR_ISTORE && 
      WN_operator(WN_kid1(parent)) == OPR_ARRAY &&
      ABS(WN_element_size(WN_kid1(parent))) != 
      MTYPE_byte_size(WN_desc(parent)))
    return FALSE;

  return TRUE;
}

static WN* Find_Do_Body (WN* simd_op)
{
  WN* parent = LWN_Get_Parent(LWN_Get_Parent(simd_op));
  WN* body = LWN_Get_Parent(simd_op);

  while (parent) {
    if (WN_operator(parent) == OPR_DO_LOOP)
      break;
    parent = LWN_Get_Parent(parent);
    body = LWN_Get_Parent(body);
  }
  return body;
}

static BOOL is_vectorizable_op_stmt(WN* stmt, WN* loop) {

  OPERATOR opr=WN_operator(stmt);
  if (opr==OPR_STID || opr==OPR_ISTORE) {
    WN* rhs=WN_kid0(stmt);
    opr=WN_operator(rhs);
    TYPE_ID rtype = WN_rtype(rhs);
    TYPE_ID desc = WN_desc(rhs);
    if (is_vectorizable_op(opr, rtype, desc)) {
      if (Is_Well_Formed_Simd(rhs, loop)) {
	return TRUE;
      }
    }
  }
  return FALSE;
}

static UINT_DYN_ARRAY* simd_fis_merge_scc_to_form_new_loop(
        UINT            total_scc,      // total number of SCCs
        FF_STMT_LIST*      scc,            // list of statements for SCCs
        UINT*		scc_size,	// size of each scc
        WN*             loop,           // loop enclosing the SCCs
        SCC_DIRECTED_GRAPH16 *scc_dep_g // SCC dependence graph
        )
{

 
  // store sccs chosen as seeds to form new loops
  UINT_DYN_ARRAY *seed_scc=CXX_NEW(UINT_DYN_ARRAY(&SIMD_default_pool),
                    &SIMD_default_pool);

  // the queues for SCC available to be merged
  // scc_queue[0] stores simd scc
  // scc_queue[1] stores non_simd scc
  INT* scc_queue[2];
  UINT head0, head1, tail0, tail1;  // heads and tails of scc_queue

  INT scc_remained=total_scc;
  UINT simd=0;
  UINT non_simd=1;

  UINT i;
  for (i=0; i<2; i++) {
    scc_queue[i]= CXX_NEW_ARRAY(INT,total_scc+1,&SIMD_default_pool);
  }
  head0=tail0=0;
  head1=tail1=0;

  // initially, only those SCCs without any predecessor are available
  for (i=1; i<=total_scc; i++) {

    if (scc_size[i]>0 && scc_dep_g->Get_In_Edge(i)==0) {
      // scc_size could be 0 if the single assignment stmt is removed
      // after copy_propagation
      if (scc_size[i]==1) {
        WN* stmt=scc[i].Head()->Get_Stmt();
        if (is_vectorizable_op_stmt(stmt,loop))
          scc_queue[simd][head0++]=i;
        else
          scc_queue[non_simd][head1++]=i;
      } else
          scc_queue[non_simd][head1++]=i;
    } else if (scc_size[i]==0)
      scc_remained--;
  }

  INT kind=simd;
  INT last_loop_kind=simd;
  WN* body=WN_do_body(loop);
  UINT entry_loop_id = seed_scc->Newidx();
  BOOL entry = TRUE;
  while (1) {
    UINT current_scc;
    if (kind==simd && head0!=tail0) {
      current_scc=scc_queue[simd][tail0++];

      if (entry) {
	entry = FALSE;
	(*seed_scc)[entry_loop_id]=current_scc;
      } else {
	if (last_loop_kind!= simd) {
	  UINT loop_id=seed_scc->Newidx();
	  (*seed_scc)[loop_id]=current_scc;
	} else {
	  scc[(*seed_scc)[seed_scc->Lastidx()]].Append_List(&scc[current_scc]);
	}
      }
      last_loop_kind=simd;
      scc_remained--;
    } else if (kind==non_simd && head1!=tail1) {
      current_scc=scc_queue[non_simd][tail1++];

      if (entry) {
	entry = FALSE;
	(*seed_scc)[entry_loop_id]=current_scc;
      } else {
	if (last_loop_kind!=non_simd) {
	  UINT loop_id=seed_scc->Newidx();
	  (*seed_scc)[loop_id]=current_scc;
	} else {
	  scc[(*seed_scc)[seed_scc->Lastidx()]].Append_List(&scc[current_scc]);
	}
      }
      last_loop_kind=non_simd;
      scc_remained--;
    } else {
      if (head0!=tail0)
        kind=simd;
      else if (head1!=tail1)
        kind=non_simd;
      else
        break;
      continue;
    }

    // remove all out-edges of scc and put new candidate SCCs in queue
    EINDEX16 e=scc_dep_g->Get_Out_Edge(current_scc);
    while (e) {
  
      VINDEX16 v=scc_dep_g->Get_Sink(e);
      scc_dep_g->Delete_Edge(e);
      if (scc_dep_g->Get_In_Edge(v)==0) {
        if (scc_size[v]==1) {
          WN* stmt=scc[v].Head()->Get_Stmt();
          if (is_vectorizable_op_stmt(stmt,loop))
            scc_queue[simd][head0++]=v;
          else
            scc_queue[non_simd][head1++]=v;
        } else
          scc_queue[non_simd][head1++]=v;
      }
      e=scc_dep_g->Get_Next_Out_Edge(e);
    }
  }
  FmtAssert(scc_remained==0,("Merging not finished in simd phase"));
  return seed_scc;
}

static void simd_fis_separate_loop_and_scalar_expand(
   UINT_DYN_ARRAY* new_loops,
   FF_STMT_LIST* scc,
   WN* loop,
   FF_STMT_LIST& expandable_ref_list)
{
  WN* body=WN_do_body(loop);
  UINT total_loops=new_loops->Lastidx()+1;
  UINT *loop_size=CXX_NEW_ARRAY(UINT,total_loops,&SIMD_default_pool);
  // hash table which maps a statement to a result loop (id)
  WN2INT *stmt_to_loop=
  CXX_NEW(WN2INT(ESTIMATED_SIZE, &SIMD_default_pool),
	  &SIMD_default_pool);

  BOOL fission_ok = (total_loops>1);
  UINT i;
  for (i=0; i<total_loops; i++) {

    UINT seed_scc=(*new_loops)[i];
    UINT total_stmt=0;
    FF_STMT_ITER s_iter(&scc[seed_scc]);
    for (FF_STMT_NODE* stmt_node=s_iter.First(); !s_iter.Is_Empty();
      stmt_node=s_iter.Next()) {
      WN* stmt=stmt_node->Get_Stmt();
      stmt_to_loop->Enter(stmt,i);
      LWN_Insert_Block_Before(body,NULL,LWN_Extract_From_Block(stmt));
      total_stmt++;
    }
    loop_size[i]=total_stmt;

  }

  if (total_loops>=1) { // used to be > 1 09/14/2003
    BOOL has_calls_or_gotos_or_inner_loops = FALSE;
    DO_LOOP_INFO* loop_info=Get_Do_Loop_Info(loop, FALSE);
    if (loop_info->Has_Calls || loop_info->Has_Gotos || !loop_info->Is_Inner) {
      has_calls_or_gotos_or_inner_loops = TRUE;
    }

    BOOL need_expansion = FALSE; 
    BOOL need_finalization = FALSE; 
    STACK<WN*> se_stack(&SIMD_default_pool);
    STACK<BOOL> finalize_stack(&SIMD_default_pool);
    FF_STMT_ITER r_iter(&expandable_ref_list);
    for (FF_STMT_NODE* ref_node=r_iter.First(); !r_iter.Is_Empty();
        ref_node=r_iter.Next()) {
        WN* ref=ref_node->Get_Stmt();
        WN* stmt0=Find_Stmt_Under(ref,body);
	WN* wn_eq_loop = NULL; 
        STACK<WN*>* equivalence_class=
          Scalar_Equivalence_Class(ref, Du_Mgr, &SIMD_default_pool,
	    TRUE, &wn_eq_loop);
        BOOL expand = FALSE;
        BOOL finalize = FALSE;
        while (!equivalence_class->Is_Empty() && !expand) {
          WN* ref1=equivalence_class->Pop();
          WN* stmt1=Find_Stmt_Under(ref1,body);
          if (1) { // 09/14/2003 if (stmt_to_loop->Find(stmt0)!=stmt_to_loop->Find(stmt1)) {
            expand = TRUE;
  	    need_expansion = TRUE; 
            if (wn_eq_loop != NULL) {
	      finalize = TRUE; 
	      need_finalization = TRUE; 
	    } 
	  }
        }
        // cannot do expansion right away because it will
        // destroy stmt_to_loop mapping
        if (expand) {
          se_stack.Push(ref);
	  finalize_stack.Push(finalize); 
	}
    }
    WN* guard_tests[1];
    guard_tests[0] = NULL;
    if (need_finalization)
      SE_Guard_Tests(loop, 1, guard_tests, Do_Loop_Depth(loop));
    for (i=0; i<se_stack.Elements(); i++) {
      WN* wn_ref = se_stack.Top_nth(i); 
      SYMBOL sym(wn_ref); 
      INT dummy[1]={0};
      BOOL finalize = finalize_stack.Top_nth(i); 
      Scalar_Expand(loop, loop, NULL, sym, &loop, dummy, 1, FALSE, 
		    finalize, FALSE, guard_tests);
    }

    WN* tmp_loop1=loop;
    WN** wn_starts=CXX_NEW_ARRAY(WN*, total_loops, &SIMD_default_pool);
    WN** wn_ends=CXX_NEW_ARRAY(WN*, total_loops, &SIMD_default_pool);
    WN** wn_steps=CXX_NEW_ARRAY(WN*, total_loops, &SIMD_default_pool);
    WN** new_loops=CXX_NEW_ARRAY(WN*, total_loops, &SIMD_default_pool);

    wn_starts[0]=WN_kid0(WN_start(tmp_loop1));
    wn_ends[0]=WN_end(tmp_loop1);
    wn_steps[0]=WN_kid0(WN_step(tmp_loop1));
    new_loops[0]=loop;
    WN* stmt=WN_first(body);

    for (i=0; i<total_loops-1; i++) {
  
      INT size=loop_size[i];

      for (INT j=0; j<size; j++)
        stmt=WN_next(stmt);

      WN* tmp_loop2;

      Separate(tmp_loop1, WN_prev(stmt), 1, &tmp_loop2);
      LWN_Parentize(tmp_loop2);
      DO_LOOP_INFO* new_loop_info =
        CXX_NEW(DO_LOOP_INFO(loop_info,&LNO_default_pool), &LNO_default_pool);
      Set_Do_Loop_Info(tmp_loop2,new_loop_info);
      if (has_calls_or_gotos_or_inner_loops) {
        // should check gotos and calls when they are allowed to be in
        // loops handled by simd phase
      }
      wn_starts[i+1]=WN_kid0(WN_start(tmp_loop2));
      wn_ends[i+1]=WN_end(tmp_loop2);
      wn_steps[i+1]=WN_kid0(WN_step(tmp_loop2));
      new_loops[i+1]=tmp_loop2;

      tmp_loop1=tmp_loop2;
    }

    Fission_DU_Update(Du_Mgr,red_manager,wn_starts,wn_ends,wn_steps,
		      total_loops,new_loops);
    for (i=0; i<total_loops-1; i++)
      scalar_rename(LWN_Get_Parent(wn_starts[i]));

    adg->Fission_Dep_Update(new_loops[0],total_loops);
  }
}

typedef enum { V16I1, V16I2, V16I4, V16I8, V16C8, INVALID } SIMD_KIND;
#define V16F4 V16I4
#define V16F8 V16I8
#define V16C4 V16F8
INT Vec_Unit_Size[6] = { 1, 2, 4, 8, 16, -1 };

static SIMD_KIND 
Find_Simd_Kind ( STACK_OF_WN *vec_simd_ops )
{
  SIMD_KIND smallest_kind = INVALID;

  //Should not check under_if in conjunction with OPR_IF here
  //since Find_Simd_Kind is not called in the context of a single
  //expression but a stack of vectorizable nodes.

  for (INT i=0; i<vec_simd_ops->Elements(); i++){
    WN* simd_op=vec_simd_ops->Top_nth(i);

    WN* istore=LWN_Get_Parent(simd_op);
    // bug 2336 - trace up the correct type
    while(istore && !OPCODE_is_store(WN_opcode(istore)) && 
	  (WN_operator(istore) != OPR_IF) && 
	  WN_operator(istore) != OPR_DO_LOOP)
      istore = LWN_Get_Parent(istore);
    FmtAssert(!istore || 
	      WN_operator(istore) == OPR_DO_LOOP ||
	      (WN_operator(istore) == OPR_IF) || 
	      OPCODE_is_store(WN_opcode(istore)), ("NYI"));	
    TYPE_ID type;
    if (WN_desc(istore) == MTYPE_V) 
      type = WN_rtype(istore);
    else 
      type = WN_desc(istore);

    if (WN_operator(istore) == OPR_IF) {
    //simd_op is part of the if-condition. 
      type = WN_rtype(simd_op);
      if (WN_operator(simd_op) == OPR_NE) {
	//We're assuming that OPR_NE is the root of the condition
	//and none of its sub-expr will contain OPR_NE. 
	//Further work: We need to ensure that this is indeed the case.
	type = WN_desc(simd_op);
      }
    }

    switch(type) {
    case MTYPE_C4:
      if (smallest_kind > V16C4)
	 smallest_kind = V16C4;
      break;
    case MTYPE_C8:
      if (smallest_kind > V16C8)
	 smallest_kind = V16C8;
      break;
    case MTYPE_F4:
      if (smallest_kind > V16F4)
	smallest_kind = V16F4;
      break;
    case MTYPE_F8:
      if (smallest_kind > V16F8)
	smallest_kind = V16F8;
      break;
    case MTYPE_I1: case MTYPE_U1:
      smallest_kind = V16I1;
      break;
    case MTYPE_I2: case MTYPE_U2:
      if (smallest_kind > V16I2)
	smallest_kind = V16I2;
      break;
    case MTYPE_I4: case MTYPE_U4:
      if (smallest_kind > V16I4)
	smallest_kind = V16I4;
      break;
    case MTYPE_I8: case MTYPE_U8:
      if (smallest_kind > V16I8)
	smallest_kind = V16I8;
      break;
    default:
      smallest_kind = INVALID;
      break;
    }
    if (smallest_kind == INVALID)
      break;
  }
  return smallest_kind;
}

BOOL Is_Vectorizable_Intrinsic (WN *wn) 
{
  INTRINSIC intrn = WN_intrinsic(wn);
  
  if (intrn == INTRN_SUBSU2 ||
      intrn == INTRN_F4SIGN ||
      intrn == INTRN_F8SIGN )
    return TRUE;

  if (!OPT_Fast_Math || Is_Target_32bit())
    return FALSE;

  switch (intrn) {
  case INTRN_F4EXP:
  case INTRN_F8EXP:
  case INTRN_F4LOG:
  case INTRN_F8LOG:
  case INTRN_F4SIN:
  case INTRN_F8SIN:
  case INTRN_F4COS:
  case INTRN_F8COS:
  case INTRN_F4EXPEXPR:
  case INTRN_F8EXPEXPR:
  case INTRN_F8SINH:
  case INTRN_F8COSH:
  case INTRN_F4LOG10:
  case INTRN_F8LOG10:
    return TRUE;
  default:
    return FALSE;
  }  
}


BOOL Gather_Vectorizable_Ops(
  WN* wn, SCALAR_REF_STACK* simd_ops, MEM_POOL *pool, WN *loop);

BOOL Gather_Vectorizable_Ops_Helper(
  WN* wn, SCALAR_REF_STACK* simd_ops, MEM_POOL *pool, WN *loop)
{
  if (WN_opcode(wn) == OPC_BLOCK){
    WN* kid = WN_first (wn);
    while(kid){
      if (!Gather_Vectorizable_Ops(kid,simd_ops,pool,loop))
	return FALSE;
      kid = WN_next(kid);
    }
    return TRUE;
  }
    
  OPERATOR opr=WN_operator(wn);  
  TYPE_ID rtype = WN_rtype(wn);
  TYPE_ID desc = WN_desc(wn);
  
  // Recognize 2 operand invariant sub-expression
  // and do not push it onto the stack of vectorizable operations. 
  // Should eventually be generalized to prevent any 2 operand invariant
  // from being vectorized.
  WN* body_parent = LWN_Get_Parent(WN_do_body(loop));
  if (BINARY_OP(opr) &&
      simd_operand_kind(WN_kid0(wn), body_parent) == Invariant &&
      simd_operand_kind(WN_kid1(wn), body_parent) == Invariant)
    if (is_vectorizable_op(WN_operator(wn), WN_rtype(wn), WN_desc(wn))) {
      WN* parent = LWN_Get_Parent(wn);
      // Invariant children of a store need to be vectorized as they will not be replicated.
      if (parent && !OPCODE_is_store(WN_opcode(parent)))
 	return TRUE;
    }
  if (opr == OPR_REGION){
    Report_Non_Vectorizable_Op(wn);
    return FALSE;
  }
  if (WN_operator(wn) == OPR_IF) {
    if (!Simd_vect_conf.Is_SSE41() || !LNO_Simd_Vect_If) {
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
    if(!Is_Vectorizable_If(wn, FALSE)) { 
      //ok to always pass FALSE. 
      //Is_Vectorizable_If will correctly pass TRUE for inner-if on recursive call.
      return FALSE;
    }
    if (!under_if)
      under_if = TRUE;
    else
      nested_if = TRUE;
  }

  if (is_vectorizable_op(opr, rtype, desc)){
    if ((opr != OPR_INTRINSIC_OP && 
	 Is_Well_Formed_Simd(wn, loop)) ||
	(opr == OPR_INTRINSIC_OP && 
	 Is_Vectorizable_Intrinsic(wn))) {
      SCALAR_REF scalar_ref(wn,0);
      simd_ops->Push(scalar_ref);
    }else{//not well formed
      // If the 'wn' is inside a OPR_ARRAY, then it is
      // not vectorizable but we do not abort vectorization.
      if(Is_Under_Array(wn)) return TRUE;
      
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
  } else if(OPCODE_is_store(WN_opcode(LWN_Get_Parent(wn)))&& 
             WN_operator(wn) != OPR_ARRAY){
            Report_Non_Vectorizable_Op(wn);
            return FALSE; // op is not vectorizable
          }

  // Bug 2986
  if (opr == OPR_CVT && !is_vectorizable_op(opr, rtype, desc)){
    // If the CVT is inside a OPR_ARRAY, then it is
    // not vectorizable but we do not abort vectorization.
    if(Is_Under_Array(wn)) return TRUE;
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
   }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++){
    WN* kid = WN_kid(wn,kidno);
    if (!Gather_Vectorizable_Ops(kid,simd_ops,pool,loop)) {
      Report_Non_Vectorizable_Op(kid);
      return FALSE;
    }
  }

  // Bug 3011 - If 'wn' is a reduction statement then it should not be used
  // more than once (except in reductions on the same variable) inside this 
  // loop body.
  if (WN_operator(wn) == OPR_STID && curr_simd_red_manager &&
      curr_simd_red_manager->Which_Reduction(wn) != RED_NONE) {
    // Bug 4061 - vectorization of reduction loops involving complex types
    // is not yet supported.
    if(MTYPE_is_complex(WN_desc(wn))){
       Report_Non_Vectorizable_Op(wn);
       return FALSE;
    }

    // 'wn' is a reduction statement.
    // If there is more than one use for this definition inside this loop 
    // then do not vectorize.
    if (!Du_Mgr) {
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
    USE_LIST* use_list=Du_Mgr->Du_Get_Use(wn);
    if (!use_list) {
      return FALSE;
    }
    WN *body = WN_do_body(loop);
    USE_LIST_ITER uiter(use_list);
    INT num_reuse = 0;
    for (DU_NODE* u = uiter.First(); !uiter.Is_Empty(); u=uiter.Next()) {
      WN* use=u->Wn();
      if (Wn_Is_Inside(use, loop)) {
	WN* stmt = Find_Stmt_Under(use, body);
	if (curr_simd_red_manager->Which_Reduction(stmt) == RED_NONE ||
	    (WN_operator(stmt) == OPR_STID && 
	     (WN_st(stmt) != WN_st(wn) || 
	      WN_store_offset(stmt) != WN_store_offset(wn)))){
          Report_Non_Vectorizable_Op(wn);
	  return FALSE;	 
         }   
      }
      // Bug 6248 - can not vectorize if there are multiple uses in the 
      // reduction statement.
      if (Wn_Is_Inside(use, wn)) {
	if (num_reuse > 0){
          Report_Non_Vectorizable_Op(wn);
	  return FALSE;
         }
	else num_reuse ++;
      }	
    }
  }

  // Bug 2952 - use before def of a loop variant scalar that is not 
  // involved in a reduction.
  if (WN_operator(wn) == OPR_STID && curr_simd_red_manager &&
      curr_simd_red_manager->Which_Reduction(wn) == RED_NONE) {
    if (!Du_Mgr) {
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
    USE_LIST* use_list=Du_Mgr->Du_Get_Use(wn);
    if (!use_list || use_list->Incomplete()) { 
      //bug 12536 - conservative if incomplete
      Report_Non_Vectorizable_Op(wn);
      return FALSE;                            
    }
    USE_LIST_ITER uiter(use_list);
    for (DU_NODE* u = uiter.First(); !uiter.Is_Empty(); u=uiter.Next()) {
      WN* use=u->Wn();
      WN* body = WN_do_body(loop);
      WN* stmt = Find_Stmt_Under(use, body);

   //BUG 9957: if the use (LDID) is under a XPRAGMA of copyin_bound
   //          we don't vectorize.
   //NOTE: This copyin_bound xpragma and maybe other pragma are generated
   //      by inlining. Don't know whether they are useful for non-omp code
   //      Therefore, I just hide vectorization for this specific case.
   //TODO: investigate whether these pragmas can be removed before simd
      if(stmt && WN_operator(stmt)==OPR_XPRAGMA && 
            WN_pragma(stmt) == WN_PRAGMA_COPYIN_BOUND && 
            WN_kid0(stmt) == use) {
        Report_Non_Vectorizable_Op(wn);
        return FALSE;
      } 

      WN* loop_stmt = WN_first(body);
      for (; loop_stmt; loop_stmt = WN_next(loop_stmt)) {
	if (loop_stmt == stmt){
          Report_Non_Vectorizable_Op(wn);
	  return FALSE;
         }
	if (loop_stmt == wn)
	  break;
      }
    }
  }

  // Bug 3875 - Also, the STID should not be used to compute address from a
  // ARRAY node.
  if (WN_operator(wn) == OPR_STID) {
    if (!Du_Mgr) {
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
    USE_LIST* use_list=Du_Mgr->Du_Get_Use(wn);
    if (!use_list) {
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
    USE_LIST_ITER uiter(use_list);
    for (DU_NODE* u = uiter.First(); !uiter.Is_Empty(); u=uiter.Next()) {
      WN* use=u->Wn();
      if (Wn_Is_Inside(use, loop)) {
	WN* parent = LWN_Get_Parent(use);
	while(parent && 
	      !OPCODE_is_load(WN_opcode(parent)) &&
	      !OPCODE_is_store(WN_opcode(parent))) {
	  if (WN_operator(parent) == OPR_ARRAY){
             Report_Non_Vectorizable_Op(wn);
             return FALSE;
           }
	  parent = LWN_Get_Parent(parent);
	}
      }    
    }
  }

  // Bug 4971 - result of a STID is never used inside the loop. So you wonder
  // why a loop. 
  if (WN_operator(wn) == OPR_STID && curr_simd_red_manager &&
      curr_simd_red_manager->Which_Reduction(wn) == RED_NONE) {
    if (!Du_Mgr) {
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
    USE_LIST* use_list=Du_Mgr->Du_Get_Use(wn);
    if (!use_list) {
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
    }
    USE_LIST_ITER uiter(use_list);
    BOOL used_in_loop = FALSE;
    for (DU_NODE* u = uiter.First(); !uiter.Is_Empty() && !used_in_loop; 
	 u=uiter.Next()) {
      WN* use=u->Wn();
      if (Wn_Is_Inside(use, loop))
	used_in_loop = TRUE;

      // A non-reduction statement that has a re-use in itself should 
      // not be vectorized. An example is bug 5296 (x = (x + a[i])*y;).
      if (Wn_Is_Inside(use, wn)){
        Report_Non_Vectorizable_Op(wn);
	return FALSE;
       }
    }
    if (used_in_loop == FALSE){
      Report_Non_Vectorizable_Op(wn);
      return FALSE;
      }
  }
  
  // Bug 2612
  if (WN_operator(wn) == OPR_ISTORE) {
    WN* stmt_next = WN_next(wn);
    while(stmt_next) {
      if (WN_operator(stmt_next) == OPR_ISTORE &&
	  WN_Simp_Compare_Trees(WN_kid1(wn), WN_kid1(stmt_next)) == 0 &&
	  ABS(WN_offset(wn) - WN_offset(stmt_next)) <= 
	  MTYPE_byte_size(WN_desc(wn)) && 
	  WN_offset(wn) != WN_offset(stmt_next)){
        Report_Non_Vectorizable_Op(wn);
	return FALSE;	
      }
      stmt_next = WN_next(stmt_next);
    }    
  }

  if (WN_operator(wn) == OPR_IF) {
    // Done with processing if statement
    if (!nested_if)
      under_if = FALSE;
    else
      nested_if = FALSE;
  }
  return TRUE;
}

BOOL Gather_Vectorizable_Ops(
  WN* wn, SCALAR_REF_STACK* simd_ops, MEM_POOL *pool, WN *loop) {
  if (!Gather_Vectorizable_Ops_Helper (wn, simd_ops, pool, loop)) {
    Report_Non_Vectorizable_Op (wn);
    return FALSE;
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Find_Nodes
// FUNCTION: Find all of the nodes in the tree rooted at 'wn_tree' with the
//    symbol 'sym' iand OPERATOR type 'opr' and push their addresses on the
//    'stack'.
//-----------------------------------------------------------------------

static void Find_Nodes(OPERATOR opr,
		       SYMBOL sym,
		       WN* wn_tree,
		       STACK<WN*>* stack)
{
  if (WN_operator(wn_tree) == opr) {
    SYMBOL newsym = SYMBOL(wn_tree);
    if (newsym == sym)
      stack->Push(wn_tree);
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Find_Nodes(opr, sym, wn, stack);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Find_Nodes(opr, sym, WN_kid(wn_tree, i), stack);
  }
}

static void Add_Vertices(WN *wn_tree)
{
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Add_Vertices(wn);
  } else {
    if (OPCODE_is_load(WN_opcode(wn_tree)) || 
	OPCODE_is_store(WN_opcode(wn_tree)))
      adg->Add_Vertex(wn_tree);
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Add_Vertices(WN_kid(wn_tree, i));
  }
}

static void Delete_Def_Use (WN *wn_tree) 
{
  if (WN_operator(wn_tree) == OPR_LDID) {
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_tree);
    if(def_list==NULL) return; //nothing to delete
    DEF_LIST_ITER iter(def_list);
    const DU_NODE *node = iter.First();
    const DU_NODE *next;
    Is_True(!iter.Is_Empty(),("Empty def list in Delete_Def_Use"));
    for(next = iter.Next(); node; node=next, next=iter.Next()){
      WN *def = (WN *) node->Wn();
      Du_Mgr->Delete_Def_Use(def,wn_tree);
    }
  }
  for (INT i = 0; i < WN_kid_count(wn_tree); i++)
    Delete_Def_Use(WN_kid(wn_tree, i));
}

static void Copy_Def_Use (WN *from_tree, 
			  WN *to_tree, 
			  SYMBOL sym, 
			  BOOL no_synch)
{
  FmtAssert(WN_operator(from_tree) == WN_operator(to_tree) || 
	    no_synch, 
	    ("from_tree and to_tree not in synch"));
  if (WN_operator(from_tree) != WN_operator(to_tree) &&
      // After we adjust loop bounds, we can get different cmp operators 
      !(WN_operator(from_tree) == OPR_LT &&
        WN_operator(to_tree) == OPR_LE))
    return;

  if (WN_operator(from_tree) == OPR_LDID) {
    SYMBOL currsym = SYMBOL(from_tree);
    FmtAssert(SYMBOL(to_tree) == currsym,
	      ("from_tree and to_tree have different symbols"));    
    // Just care for external edges if any.
    // Index variable will be updated later with internal edges 
    // inside the caller.
    if (currsym != sym) { 
      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(from_tree);
      DEF_LIST_ITER iter(def_list);
      const DU_NODE *node = iter.First();
      for(; !iter.Is_Empty();node=iter.Next()){
	WN *def = (WN *) node->Wn();
	Du_Mgr->Add_Def_Use(def, to_tree);
	DEF_LIST *def_list_to = Du_Mgr->Ud_Get_Def(to_tree); 
	DEF_LIST *def_list_from = Du_Mgr->Ud_Get_Def(from_tree); 
	def_list_to->Set_loop_stmt(def_list_from->Loop_stmt()); 
      }
    }    
  }
  // recurse
  for (INT i = 0; i < WN_kid_count(from_tree); i ++) {
    Copy_Def_Use(WN_kid(from_tree, i), WN_kid(to_tree, i), sym, no_synch);
  }    
}

static void 
Simd_Replace_With_Constant(WN *copy, SYMBOL sym, WN *cons, TYPE_ID index_type)
{
  FmtAssert(WN_operator(cons) == OPR_INTCONST, ("Handle this"));
  if (WN_operator(copy) == OPR_LDID) {
    SYMBOL currsym = SYMBOL(copy);
    if (currsym == sym) { 
      WN *parent = LWN_Get_Parent(copy);
      INT kid;
      for (kid = 0; kid < WN_kid_count(parent); kid ++)
	if (WN_kid(parent, kid) == copy)
	  break;
      OPCODE intconst_opc= 
	OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);   
      WN_kid(parent, kid) = 
	WN_CreateIntconst(intconst_opc, WN_const_val(cons));
      LWN_Set_Parent(WN_kid(parent, kid), parent);
    }
  }
  // recurse
  for (INT i = 0; i < WN_kid_count(copy); i ++) {
    Simd_Replace_With_Constant(WN_kid(copy, i), sym, cons, index_type);
  }    
  return;
}

// Look for all the symbols used in src and for each of them 
// update the use-def for uses in dest. Additionally, if flag is set
// then ignore those symbols in src that are identical to 'symbol'.
static void Update_Symbol_Use_Def (WN *src, WN *dest, SYMBOL symbol, BOOL flag)
{
  if (WN_operator(src) == OPR_LDID) {
    SYMBOL currsym = SYMBOL(src);
    if (!flag || currsym != symbol) { 

      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(src);
      DEF_LIST_ITER iter(def_list);
      const DU_NODE *node = iter.First();
      Is_True(!iter.Is_Empty(),("Empty def list in Update_Symbol_Use_Def"));

      // Find all locations inside dest that use 'symbol'
      // Then update use-def for all the uses.
      DOLOOP_STACK sym_stack(&LNO_local_pool);
      Find_Nodes(OPR_LDID, currsym, dest, &sym_stack);  
      for(; !iter.Is_Empty();node=iter.Next()){
	for (INT k = 0; k < sym_stack.Elements(); k++) {
	  WN* wn_use = sym_stack.Bottom_nth(k);

	  WN *def = (WN *) node->Wn();
	  Du_Mgr->Add_Def_Use(def, wn_use);
	  DEF_LIST *def_list_to = Du_Mgr->Ud_Get_Def(wn_use); 
	  DEF_LIST *def_list_from = Du_Mgr->Ud_Get_Def(src); 
	  def_list_to->Set_loop_stmt(def_list_from->Loop_stmt()); 
	}
      }    
    }    
  } else {
    for (INT i = 0; i < WN_kid_count(src); i ++)
      Update_Symbol_Use_Def(WN_kid(src, i), dest, symbol, flag);
  }  
}

// Use SCC analysis to find out any loop-carried dependencies.
BOOL Analyse_Dependencies(WN* innerloop) 
{
  REDUCTION_MANAGER *depanal_red_manager = 0;

  WN* body=WN_do_body(innerloop);
  WN* stmt;
  // main statement dependence graph for statements in the loop
  SCC_DIRECTED_GRAPH16 *dep_g_p =
    CXX_NEW(SCC_DIRECTED_GRAPH16(ESTIMATED_SIZE,ESTIMATED_SIZE),
    &SIMD_default_pool);

  // hash table which associates the statements in the loop and vertices in the
  // above dependence graph 'dep_g_p'
  WN2VINDEX *stmt_to_vertex=
  CXX_NEW(WN2VINDEX(ESTIMATED_SIZE, &SIMD_default_pool),
    &SIMD_default_pool);

  SCALAR_REF_STACK *simd_ops =
        CXX_NEW(SCALAR_REF_STACK(&SIMD_default_pool),
        &SIMD_default_pool);

  if (LNO_Simd_Reduction) {
    depanal_red_manager = CXX_NEW 
      (REDUCTION_MANAGER(&SIMD_default_pool), &SIMD_default_pool);
    depanal_red_manager->Build(innerloop,TRUE,FALSE); // build scalar reductions
    curr_simd_red_manager = depanal_red_manager;
  }

  for (stmt=WN_first(body); stmt; stmt=WN_next(stmt)) {
    Gather_Vectorizable_Ops(stmt,simd_ops,&SIMD_default_pool, innerloop) ;
  }

  if (LNO_Simd_Reduction && depanal_red_manager) {
    curr_simd_red_manager = simd_red_manager;
  }

  if (simd_ops->Elements()==0) { // no simd op in this loop
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    if (depanal_red_manager)
      CXX_DELETE(depanal_red_manager,&SIMD_default_pool);
    return TRUE;
  }

  STACK_OF_WN *vec_simd_ops=
    CXX_NEW(STACK_OF_WN(&SIMD_default_pool),&SIMD_default_pool);

  for (INT i=0; i<simd_ops->Elements(); i++) {

    WN* simd_op=simd_ops->Top_nth(i).Wn;
    WN* stmt=simd_op;
    WN* stmt1;
    BOOL under_scf=FALSE;
    while ((stmt1=LWN_Get_Parent(stmt))!=body) {
      stmt=stmt1;
      if (WN_opcode(stmt)==OPC_BLOCK) {
        under_scf=TRUE;
        break;
      }
    }
    if (under_scf)
      continue;
    TYPE_ID rtype = WN_rtype(simd_op);
    TYPE_ID desc = WN_desc(simd_op);
    UINT kid_no;
    BOOL splitted=FALSE;

    for (kid_no=0; kid_no<WN_kid_count(simd_op); kid_no++) {
      WN* tmp=WN_kid(simd_op,kid_no);
      tmp = Split_Using_Preg(stmt,tmp,adg,FALSE);
      FmtAssert(WN_operator(tmp)==OPR_STID,
		("Expecting STID after splitting"));
      USE_LIST* use_list=Du_Mgr->Du_Get_Use(tmp);
      DU_NODE* node=use_list->Head();
      FmtAssert(use_list->Tail()==node, ("Too many uses after splitting"));
      splitted=TRUE;
    }
    if (!splitted)
      continue;

    vec_simd_ops->Push(simd_op);

    WN_OFFSET offset=WN_offset(WN_prev(stmt));

    WN *simd_root = Split_Using_Preg(stmt,simd_op,adg,FALSE);
    FmtAssert(WN_operator(simd_root)==OPR_STID,
      ("Expecting STID after splitting"));
    USE_LIST* use_list=Du_Mgr->Du_Get_Use(simd_root);
    DU_NODE* node=use_list->Head();
    FmtAssert(use_list->Tail()==node, ("Too many uses after splitting"));
    WN* use=node->Wn();
    // FP and INT registers can not have same (preg) offset.
    // That situation comes up when we vectorize CVT.
    //WN_offset(simd_root)=WN_offset(use)=offset;
  }

  if (vec_simd_ops->Elements()==0) {
    // no vecorizable op in this loop
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    if (depanal_red_manager)
      CXX_DELETE(depanal_red_manager,&SIMD_default_pool);
    return TRUE;
  }

  REF_LIST_STACK* writes = CXX_NEW(REF_LIST_STACK(&SIMD_default_pool),
        &SIMD_default_pool);
  REF_LIST_STACK* reads = CXX_NEW(REF_LIST_STACK(&SIMD_default_pool),
        &SIMD_default_pool);

  SCALAR_STACK* scalar_writes = CXX_NEW(SCALAR_STACK(&SIMD_default_pool),
        &SIMD_default_pool);
  SCALAR_STACK* scalar_reads = CXX_NEW(SCALAR_STACK(&SIMD_default_pool),
        &SIMD_default_pool);
  SCALAR_REF_STACK* params = CXX_NEW(SCALAR_REF_STACK(&SIMD_default_pool),
        &SIMD_default_pool);

  // stack used in collecting references
  DOLOOP_STACK *stack1=CXX_NEW(DOLOOP_STACK(&SIMD_default_pool),
                              &SIMD_default_pool);
  Build_Doloop_Stack(innerloop, stack1);

  // gather again after simd ops are splitted out of old stmts
  Init_Ref_Stmt_Counter();
  INT32 gather_status = 0;
  for (stmt=WN_first(body); stmt && gather_status!= -1; stmt=WN_next(stmt)) {
    gather_status=New_Gather_References(stmt,writes,reads,stack1,
        scalar_writes,scalar_reads,
        params,&SIMD_default_pool) ;
  }
  if (gather_status == -1) {
    DevWarn("Error in gathering references");
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    if (depanal_red_manager)
      CXX_DELETE(depanal_red_manager,&SIMD_default_pool);
    return TRUE;
  }

  for (stmt=WN_first(body); stmt; stmt=WN_next(stmt)) {
    VINDEX16 v=dep_g_p->Add_Vertex();
    if (v==0) {
      DevWarn("Statement dependence graph problem");
      CXX_DELETE(dep_g_p, &SIMD_default_pool);
      if (depanal_red_manager)
        CXX_DELETE(depanal_red_manager,&SIMD_default_pool);
      return TRUE;
    }
    stmt_to_vertex->Enter(stmt, v);
  }
  // a dictionary used for looking up the bit position for a symbol
  BINARY_TREE<NAME2BIT> *mapping_dictionary = 
    CXX_NEW(BINARY_TREE<NAME2BIT>(&SIMD_default_pool),
    &SIMD_default_pool);

  // list of references that use scalar-expandable variables
  FF_STMT_LIST expandable_ref_list;

  // step 2: examine all reads and writes and do the following
  //		1. classify them as scalar or array
  //		2. create name to bit position mappings for new symbol names
  //		3. if the ref is STID, check if it is scalar expandable
  UINT sym_count=simd_2(innerloop, scalar_reads, scalar_writes,
			mapping_dictionary, expandable_ref_list);

  // we also need to have a set of expandable scalars
  BIT_VECTOR Expandable_Scalar_Set(sym_count, &SIMD_default_pool);

  // now look at all references in 'expandable_ref_list' and set the
  // corresponding bit in 'Expandable_Scalar_Set'
  FF_STMT_ITER e_iter(&expandable_ref_list);
  for (FF_STMT_NODE* ref_node=e_iter.First(); !e_iter.Is_Empty();
      ref_node=e_iter.Next()) {
      NAME2BIT temp_map;
      temp_map.Set_Symbol(ref_node->Get_Stmt());
      Expandable_Scalar_Set.Set(mapping_dictionary->Find(temp_map)->
               Get_Data()->Get_Bit_Position());
  }

  if (LNO_Test_Dump) {
    printf("Expandable_Scalar_Set=\n");
    Expandable_Scalar_Set.Print(stdout);
  }

  WN_MAP sdm=WN_MAP_Create(&SIMD_default_pool);
  ARRAY_DIRECTED_GRAPH16 *sdg =
    CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,sdm,LEVEL_ARRAY_GRAPH),
      &SIMD_default_pool);

  for (stmt = WN_first(body); stmt; stmt = WN_next(stmt)) {
    if (!Map_Stmt_To_Level_Graph(stmt,sdg)) {
      FmtAssert(0, ("Error in mapping stmt to level graph\n"));
      CXX_DELETE(dep_g_p, &SIMD_default_pool);
      CXX_DELETE(sdg, &SIMD_default_pool);
      WN_MAP_Delete(sdm);
      if (depanal_red_manager)
        CXX_DELETE(depanal_red_manager,&SIMD_default_pool);
      return TRUE;
    }
  }

  BOOL status=Generate_Scalar_Dependence_For_Statement_Dependence_Graph(
    innerloop, scalar_reads, scalar_writes, params, sdg, depanal_red_manager,
    &Expandable_Scalar_Set, mapping_dictionary);

  if (depanal_red_manager)
    CXX_DELETE(depanal_red_manager,&SIMD_default_pool);

  if (status==FALSE) {
    DevWarn("Statement dependence graph problem");
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    CXX_DELETE(sdg, &SIMD_default_pool);
    WN_MAP_Delete(sdm);
    return TRUE;
  }

  status=Generate_Array_Dependence_For_Statement_Dependence_Graph(
    innerloop, reads, writes, sdg, red_manager, adg);
  if (status==FALSE) {
    DevWarn("Statement dependence graph problem");
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    CXX_DELETE(sdg, &SIMD_default_pool);
    WN_MAP_Delete(sdm);
    return TRUE;
  }

  // dep_g_p would not overflow if sdg did not overflow so no checking
  // is needed

  EINDEX16 e=sdg->Get_Edge();
  while (e) {
    WN* source=sdg->Get_Wn(sdg->Get_Source(e));
    WN* sink=sdg->Get_Wn(sdg->Get_Sink(e));
    if (LWN_Get_Parent(source) == body || LWN_Get_Parent(sink) == body)
      // add edges only if the source and sink are immediate children
      dep_g_p->Add_Unique_Edge(
        stmt_to_vertex->Find(source),
        stmt_to_vertex->Find(sink));
    e=sdg->Get_Next_Edge(e);

  }

  // ac_g is the acyclic condensation graph of dep_g_p
  // it stores dependence relations between SCCs
  SCC_DIRECTED_GRAPH16 *ac_g;
  ac_g = dep_g_p->Acyclic_Condensation(&SIMD_default_pool);

  VINDEX16 total_scc = dep_g_p->Get_Scc_Count();

  // scc[i] is a list of statemens in i-th SCC
  FF_STMT_LIST *scc;
  scc = CXX_NEW_ARRAY(FF_STMT_LIST, total_scc+1, &SIMD_default_pool);

  UINT *scc_size=CXX_NEW_ARRAY(UINT, total_scc+1, &SIMD_default_pool);

  for (INT i=1; i<=total_scc; i++) {
    scc_size[i]=0;
  }

  // Append statements to the statement list of proper SCC
  for (stmt = WN_first(WN_do_body(innerloop)); stmt; stmt = WN_next(stmt)) {
    VINDEX16 scc_id;
    scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
    scc_size[scc_id]++;
  }

  for (INT i=0; i<vec_simd_ops->Elements(); i++) {
    WN* simd_op=vec_simd_ops->Top_nth(i);
    stmt=Find_Stmt_Under(simd_op,body);
    VINDEX16 scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
    if (scc_size[scc_id]!=1) {
      CXX_DELETE(dep_g_p, &SIMD_default_pool);
      CXX_DELETE(sdg, &SIMD_default_pool);
      WN_MAP_Delete(sdm);
      return TRUE;
    }
  }
  
  CXX_DELETE(dep_g_p, &SIMD_default_pool);
  CXX_DELETE(sdg, &SIMD_default_pool);
  WN_MAP_Delete(sdm);
  return FALSE;
}

static BOOL Loop_Has_Asm (WN* loop)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* node = itr->wn;
    if (WN_operator(node) == OPR_ASM_STMT)
      return TRUE;
  }

  return FALSE;
}


//------------------------------------------------------------
// Bug 5880: whether is wn sub-tree contsains a vectorizable 
//           intrinsic or not
//------------------------------------------------------------
static BOOL Contain_Vectorizable_Intrinsic(WN *wn)
{

    OPERATOR opr=WN_operator(wn);

    if(opr == OPR_INTRINSIC_OP &&
         Is_Vectorizable_Intrinsic(wn))
     return TRUE;

  if (WN_opcode(wn) == OPC_BLOCK){
    WN* kid = WN_first (wn);
    while (kid) {
      if(Contain_Vectorizable_Intrinsic(kid))
       return TRUE;
      kid = WN_next(kid);
    }
    return FALSE;
   }

    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN* kid = WN_kid(wn,kidno);
      if(Contain_Vectorizable_Intrinsic(kid))
      return TRUE;
    }
    return FALSE;
}

//--------------------------------------------------------
// Bug 5880 : 
// this is used to disable blocking if the loop contain
// vectorizable intrinsic ops and vintr is aggressive
//-------------------------------------------------------- 
extern BOOL Is_Aggressive_Vintr_Loop(WN* innerloop)
{
   if (LNO_Run_Vintr < 2)
     return FALSE; // only at level 2

   if (Loop_Has_Asm(innerloop))
     return FALSE;

   if (WN_opcode(innerloop) != OPC_DO_LOOP ||
      !Do_Loop_Is_Good(innerloop) ||
      Do_Loop_Has_Calls(innerloop) ||
      Do_Loop_Has_Gotos(innerloop) ||
      Do_Loop_Is_Mp(innerloop) ||
      !Do_Loop_Is_Inner(innerloop))
     return FALSE;

    WN* body = WN_do_body(innerloop);
    return Contain_Vectorizable_Intrinsic(body);
}

/*
 * Copy the use-def relations from a given loop body to its copy wn node 
 */
static void Simd_Copy_Def_Use_For_Loop_Body(WN* vbody, WN *pbody, SYMBOL index)
{
    WN *vstmt, *pstmt;

    Is_True((WN_opcode(vbody) == OPC_BLOCK && WN_opcode(pbody) == OPC_BLOCK),
            ("This function only works for DO_LOOP's loop body"));

    for (vstmt=WN_first(vbody), pstmt=WN_first(pbody);
            vstmt != NULL && pstmt != NULL;
            vstmt=WN_next(vstmt), pstmt=WN_next(pstmt))
        Copy_Def_Use(vstmt, pstmt, index, FALSE/*synch*/);

    for (vstmt=WN_first(vbody), pstmt=WN_first(pbody);
            vstmt != NULL && pstmt != NULL;
            vstmt=WN_next(vstmt), pstmt=WN_next(pstmt))
    {
        if (WN_operator(vstmt) != OPR_PRAGMA &&
                WN_operator(pstmt) != OPR_PRAGMA )
            LWN_Copy_Def_Use(WN_kid0(vstmt),WN_kid0(pstmt), Du_Mgr);
    }

    for (vstmt=WN_first(vbody), pstmt=WN_first(pbody);
            vstmt != NULL && pstmt != NULL;
            vstmt=WN_next(vstmt), pstmt=WN_next(pstmt)){

        if (WN_operator(vstmt) == OPR_STID) {
            USE_LIST* use_list=Du_Mgr->Du_Get_Use(vstmt);
            USE_LIST_ITER uiter(use_list);
            DOLOOP_STACK sym_stack(&LNO_local_pool);
            SYMBOL symbol(vstmt);
            Find_Nodes(OPR_LDID, symbol, pbody,&sym_stack);
            for (INT j = 0; j < sym_stack.Elements(); j++) {
                WN* wn_use =  sym_stack.Bottom_nth(j);
                DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_use);
                def_list->Set_loop_stmt(LWN_Get_Parent(pbody));
            }
            if (use_list->Incomplete()) {
                Du_Mgr->Create_Use_List(pstmt);
                Du_Mgr->Du_Get_Use(pstmt)->Set_Incomplete();
                continue;
            }
            for (DU_NODE* u=uiter.First(); !uiter.Is_Empty(); u=uiter.Next()) {
                WN* use = u->Wn();
                Du_Mgr->Add_Def_Use(pstmt, use);
            }
        }
    }
}
//copy from vloop to ploop
static void Simd_Copy_Def_Use_For_Loop_Stmt(WN* vloop, WN *ploop)
{

    SYMBOL index(WN_index(vloop));

    WN *vbody = WN_do_body(vloop);
    WN *pbody = WN_do_body(ploop);
    Simd_Copy_Def_Use_For_Loop_Body(vbody, pbody, index);
}

static BOOL SIMD_Is_Vectorizable_Loop (WN* vect_loop, WN* body) 
{
  if (LNO_Run_Simd == 0)
    return FALSE;

  if (Loop_Has_Asm(vect_loop))
    return FALSE;

  if (WN_opcode(vect_loop) != OPC_DO_LOOP ||
      !Do_Loop_Is_Good(vect_loop) ||
      Do_Loop_Has_Calls(vect_loop) ||
      Do_Loop_Has_Gotos(vect_loop) ||
      Do_Loop_Is_Mp(vect_loop)) {
    return FALSE;
  }

  WN* stmt;
  MEM_POOL SIMD_tmp_pool;
  MEM_POOL_Initialize(&SIMD_tmp_pool,"SIMD_tmp_pool",FALSE);
  MEM_POOL_Push(&SIMD_tmp_pool);

  SCALAR_REF_STACK *simd_ops =
    CXX_NEW(SCALAR_REF_STACK(&SIMD_tmp_pool),
	    &SIMD_tmp_pool);

  BOOL save_simp_state = WN_Simplifier_Enable(FALSE);
  BOOL innermost_loop  = TRUE;

  // In the case of outer loop vectorization, body is not the loop body of vect_loop.
  if (simd_context == SC_LOOP_MODELING 
          && LWN_Get_Parent(body) != vect_loop) {
      innermost_loop = FALSE;
  }

  Simd_Mark_Code(WN_do_body(vect_loop)); 
  WN_Simplifier_Enable(save_simp_state);

  if (LNO_Simd_Reduction) {
    WN* func_nd = LWN_Get_Parent(vect_loop);
    while(func_nd && WN_opcode(func_nd) != OPC_FUNC_ENTRY)
      func_nd = LWN_Get_Parent(func_nd);
    simd_red_manager = CXX_NEW 
      (REDUCTION_MANAGER(&SIMD_tmp_pool), &SIMD_tmp_pool);
    simd_red_manager->Build(func_nd,TRUE,FALSE); // build scalar reductions
    curr_simd_red_manager = simd_red_manager;
  }

  Induction_Seen = FALSE;
  BOOL _stop = FALSE;
  for (stmt=WN_first(body); stmt; stmt=WN_next(stmt))
    if (!Gather_Vectorizable_Ops(stmt, simd_ops,&SIMD_tmp_pool, vect_loop)){
        _stop = TRUE;
        break;
    }
  
  if (LNO_Simd_Reduction && simd_red_manager)
    CXX_DELETE(simd_red_manager,&SIMD_tmp_pool);

   //Bug 6963: Loop invariant array reference(loads) is permitted at this time
   // Simd will move these kind of array references out of the loop.
   BOOL move_invar = (!Get_Trace(TP_LNOPT, TT_LNO_GUARD) && LNO_Minvar);
   if(!_stop && !Unit_Stride_Reference(body, vect_loop, !move_invar))
     _stop = TRUE;

  if(_stop){
      MEM_POOL_Pop(&SIMD_tmp_pool);
      MEM_POOL_Delete(&SIMD_tmp_pool);
      return FALSE;
  }
  
  // Dependence Analysis
  WN* loop_copy = LWN_Copy_Tree(vect_loop, TRUE, LNO_Info_Map);

  if (!innermost_loop) {
      WN_do_body(loop_copy)= LWN_Copy_Tree(body, TRUE, LNO_Info_Map);
      LWN_Set_Parent(WN_do_body(loop_copy), loop_copy);
  }

  DO_LOOP_INFO* dli=Get_Do_Loop_Info(vect_loop);
  DO_LOOP_INFO* new_loop_info =
    CXX_NEW(DO_LOOP_INFO(dli,&LNO_default_pool), &LNO_default_pool);
  Set_Do_Loop_Info(loop_copy, new_loop_info);
  adg=Array_Dependence_Graph;

  if (innermost_loop) {
      if (!adg->Add_Deps_To_Copy_Block(vect_loop, loop_copy, TRUE)) {
          LNO_Erase_Dg_From_Here_In(loop_copy, adg);
          MEM_POOL_Pop(&SIMD_tmp_pool);
          MEM_POOL_Delete(&SIMD_tmp_pool);
          return FALSE;
      }
  } else {
      if (!adg->Add_Deps_To_Copy_Block(WN_start(vect_loop), WN_start(loop_copy), TRUE)
              || !adg->Add_Deps_To_Copy_Block(WN_end(vect_loop), WN_end(loop_copy), TRUE)
              || !adg->Add_Deps_To_Copy_Block(WN_step(vect_loop), WN_step(loop_copy), TRUE)
              || !adg->Add_Deps_To_Copy_Block(body, WN_do_body(loop_copy), TRUE)) {
          LNO_Erase_Dg_From_Here_In(loop_copy, adg);
          MEM_POOL_Pop(&SIMD_tmp_pool);
          MEM_POOL_Delete(&SIMD_tmp_pool);
          return FALSE;
      }
  }


  Copy_Def_Use(WN_start(vect_loop), WN_start(loop_copy),
                WN_index(vect_loop), FALSE /* synch */);
  Copy_Def_Use(WN_end(vect_loop), WN_end(loop_copy),
                WN_index(vect_loop), FALSE /* synch */);
  if (innermost_loop) {
      Simd_Copy_Def_Use_For_Loop_Stmt(vect_loop, loop_copy);
  } else {
      Simd_Copy_Def_Use_For_Loop_Body(body, WN_do_body(loop_copy), SYMBOL(WN_index(vect_loop)));
  }

  MEM_POOL_Initialize(&SIMD_default_pool,"SIMD_default_pool",FALSE);
  MEM_POOL_Push(&SIMD_default_pool);

  BOOL Has_Dependencies = Analyse_Dependencies(loop_copy);

  LNO_Erase_Dg_From_Here_In(loop_copy, adg);
  LWN_Update_Def_Use_Delete_Tree(loop_copy, Du_Mgr);

  MEM_POOL_Pop(&SIMD_default_pool);
  MEM_POOL_Delete(&SIMD_default_pool);

  MEM_POOL_Pop(&SIMD_tmp_pool);
  MEM_POOL_Delete(&SIMD_tmp_pool);

  return !Has_Dependencies;
}

extern BOOL Is_Vectorizable_Inner_Loop (WN* innerloop) {

    SIMD_CONTEXT sc_save = simd_context; 
    simd_context = SC_OTHER; 

    BOOL res;
    if (!Do_Loop_Is_Inner (innerloop)) {
        // Not applicable
        //
        res =FALSE;
    } else {
        res = SIMD_Is_Vectorizable_Loop (innerloop, WN_do_body (innerloop));
    }

    simd_context = sc_save;    
    return res;
}

// return TRUE iff it is legal to vectorize <loop> if it were in the 
//  innermost position.
//
// NOTE: It is up to the caller to determine if it is legal to move <loop>
//   into innermost position
//
extern BOOL Is_Vectorizable_Outer_Loop (WN* loop) {

    Is_True (WN_operator (loop) == OPR_DO_LOOP, ("invalid input"));

    if (Do_Loop_Is_Inner (loop)) {
        // not applicable 
        return FALSE;
    }

    WN* innermost = SNL_Innermost_Do (loop);
    if (!Do_Loop_Is_Inner (innermost)) {
        // there is a loop inside the SNL-sense innermost loop, give up.
        // 
        return FALSE;
    }

    SIMD_CONTEXT sc_save = simd_context;
    simd_context = SC_LOOP_MODELING;

    BOOL res = SIMD_Is_Vectorizable_Loop (loop, WN_do_body (innermost));
    
    simd_context = sc_save;
    return res;
}

extern void Mark_Auto_Vectorizable_Loops (WN* wn)
{
  simd_context = SC_OTHER;

  OPCODE opc=WN_opcode(wn);

  if (!OPCODE_is_scf(opc)) 
    return;
  else if (opc==OPC_DO_LOOP) {
    if (Do_Loop_Is_Good(wn) && Do_Loop_Is_Inner(wn) && !Do_Loop_Has_Calls(wn)
	&& !Do_Loop_Is_Mp(wn) && !Do_Loop_Has_Gotos(wn)) {
      if (SIMD_Is_Vectorizable_Loop (wn, WN_do_body (wn))) {
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn, FALSE);
	dli->Vectorizable = TRUE;
      }	
    } else
      Mark_Auto_Vectorizable_Loops(WN_do_body(wn));
  } else if (opc==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      Mark_Auto_Vectorizable_Loops(stmt);
      stmt=next_stmt;
    }
  else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Mark_Auto_Vectorizable_Loops(WN_kid(wn,kidno));
    }

    simd_context = SC_INVALID;
}

/* To facilitate vectorization, convert all 
 * ISTORE of CONST/LDID/ILOAD into ISTORE of PAREN of CONST/LDID/ILOAD 
 * PAREN nodes are later converted into NOPs. So, there are no new 
 * instructions generated, but vectorizer can assume PAREN is a 
 * vectorizable op and proceed. */
static void Simd_Mark_Code (WN* wn) 
{
  if ((WN_operator(wn) == OPR_ILOAD && 
       WN_operator(WN_kid0(wn)) == OPR_ARRAY) ||
      WN_operator(wn) == OPR_LDID || 
      WN_operator(wn) == OPR_CONST || 
      WN_operator(wn) == OPR_INTCONST) {
    WN* parent = LWN_Get_Parent(wn);
    if (((WN_operator(parent) == OPR_ISTORE &&
	  WN_operator(WN_kid1(parent)) == OPR_ARRAY) ||
	 WN_operator(parent) == OPR_STID) &&
	WN_desc(parent) != MTYPE_M &&
	WN_desc(parent) != MTYPE_C4 && WN_desc(parent) != MTYPE_C8 &&
	! MTYPE_is_vector(WN_desc(parent))) {
      TYPE_ID desc = WN_rtype(wn);
      OPCODE paren_opc;
      if (!MTYPE_is_float(desc) && MTYPE_is_unsigned(desc)) {
	switch(desc) {
	case MTYPE_U1: desc = MTYPE_I1; break;
	case MTYPE_U2: desc = MTYPE_I2; break;
	case MTYPE_U4: desc = MTYPE_I4; break;
	case MTYPE_U8: desc = MTYPE_I8; break;
	}
      }
      paren_opc = OPCODE_make_op(OPR_PAREN, desc, MTYPE_V);
      WN* new_parent;
      if (WN_operator(wn) == OPR_CONST) {
	new_parent = WN_Create(OPR_PAREN, desc, MTYPE_V, 1);
	WN_kid0(new_parent) = wn;
      } else 
	new_parent = 
	  LWN_CreateExp1(paren_opc, WN_kid(parent, 0));
      WN_kid0(parent) = new_parent;
      LWN_Parentize(parent);
    }
  }
  // Recurse
  if (WN_opcode(wn)==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      Simd_Mark_Code(stmt);
      stmt=next_stmt;
    }
  else 
    for (INT kid = 0; kid < WN_kid_count(wn); kid ++)
      Simd_Mark_Code(WN_kid(wn, kid));
}

static INT Simd_Compute_Best_Align (INT offset, INT fn, INT size)
{
  INT A0, A;

  A0 = offset;
  A = (A0 + fn*size)%16;
  return (A == 0 ? A : ((16 - A)/size));
}

// Have we created a vector type preg to create unroll copies for the use of 
// induction variable. 
// So far, the maximum unroll times is 128/8 = 16 times
BOOL vec_unroll_preg_created[16]; 
WN *vec_unroll_preg_store[16]; 

// Descend unroll copy and update the index into the last dimension for all 
// arrays. Assumes all operators inside WN copy are vectorizable.
static void 
Create_Unroll_Copy(WN* copy, INT add_to_base, 
		   WN* orig, TYPE_ID index_type,
		   WN* vec_preg_incr, WN* loop)
{
  FmtAssert(WN_operator(copy) == WN_operator(orig), ("Handle this"));
  OPCODE add_opc= OPCODE_make_op(OPR_ADD,index_type, MTYPE_V);
  OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);
  INT aa_num, dim_max;
  WN* array_index;

  if (WN_operator(copy) == OPR_ARRAY) {
    INT kid = WN_num_dim(copy)<<1;
    array_index = WN_kid(copy, kid);
    WN_kid(copy, kid) = 
      LWN_CreateExp2(add_opc, array_index, 
		     WN_CreateIntconst(intconst_opc, add_to_base));
    // Look at WN structure for OPR_ARRAY
    dim_max = WN_num_dim(copy);
    for (aa_num = 0; aa_num < dim_max - 1; aa_num ++) {
      LWN_Copy_Def_Use(WN_kid(orig, aa_num + dim_max + 1), 
		       WN_kid(copy, aa_num + dim_max + 1),
		       Du_Mgr);    
    }
    // When a CVT operation is involved, the array element may not be the 
    // same size as the add_to_base factor used to unroll. In such cases, we
    // need to set back the alignment from the load or the store. (Bug 5294)
    if (ABS(WN_element_size(copy)) != add_to_base) {
      WN* parent = LWN_Get_Parent(copy);
      FmtAssert(WN_operator(parent) == OPR_ILOAD ||
		WN_operator(parent) == OPR_ISTORE, ("NYI"));      
      TY_IDX ty_parent;
      if (WN_operator(parent) == OPR_ILOAD)
	ty_parent = TY_pointed(WN_load_addr_ty(parent));
      else
	ty_parent = TY_pointed(WN_ty(parent));
      Set_TY_align (ty_parent, 
		    ABS(WN_element_size(copy)) ?
		    TY_log_base2(ABS(WN_element_size(copy))) :
		    8 /* non-16-byte aligned */);
      TY_IDX ty_idx = 0;
      TY &ty = New_TY (ty_idx);      
      TY_Init (ty, Pointer_Size, KIND_POINTER, Pointer_Mtype, 
	       Save_Str ("anon_ptr."));
      Set_TY_pointed (ty, ty_parent);
      if (WN_operator(parent) == OPR_ILOAD)
	WN_set_load_addr_ty (parent, ty_idx);
      else
	WN_set_ty (parent, ty_idx);
    }
    return;
  } 
  else if (WN_operator(copy) == OPR_LDID && vec_preg_incr) {
    SYMBOL sym1(copy);
    SYMBOL sym2(vec_preg_incr);
    if (sym1 == sym2) {
      FmtAssert(MTYPE_is_vector(WN_desc(copy)), ("Handle this case"));

      TYPE_ID vec_type = WN_desc(copy);
      INT unroll_type;

      FmtAssert((add_to_base < 16),
              ("Loop unrolled more than 16 times, need to expand vec_unroll_preg_created array"));

      unroll_type = add_to_base;

      if (!vec_unroll_preg_created[unroll_type]) {
	WN* body = WN_do_body(loop);
	// Create the const (..., add_to_base, add_to_base, ...) in a vector preg
	TCON unroll_const_tcon = Host_To_Targ(MTYPE_I4, add_to_base);
	ST* unroll_const_symbol = 
	  New_Const_Sym (Enter_tcon(unroll_const_tcon),
			 Be_Type_Tbl(MTYPE_I4));
	WN* unroll_const = 
	  WN_CreateConst (OPR_CONST, vec_type, MTYPE_V, 
			  unroll_const_symbol);
	SYMBOL vec_unroll_symbol;
	WN* loop_enclosing_block = loop;

	vec_unroll_symbol = 
	  Create_Preg_Symbol(sym1.Name(), vec_type);
	vec_unroll_preg_store[unroll_type] = 
	  AWN_StidIntoSym(&vec_unroll_symbol, unroll_const);
	while (WN_operator(loop_enclosing_block) != OPR_BLOCK)
	  loop_enclosing_block = 
	    LWN_Get_Parent(loop_enclosing_block);
	LWN_Insert_Block_Before(loop_enclosing_block, loop, 
				vec_unroll_preg_store[unroll_type]);
	WN_Set_Linenum ( vec_unroll_preg_store[unroll_type], 
			 WN_Get_Linenum(loop) );	
	LWN_Parentize(vec_unroll_preg_store[unroll_type]);
	LWN_Set_Parent(vec_unroll_preg_store[unroll_type], 
		       loop_enclosing_block);
	vec_unroll_preg_created[unroll_type] = TRUE;	
      }

      // Use the vector preg created to update the use of the induction variable.
      SYMBOL vec_unroll_preg_symbol(vec_unroll_preg_store[unroll_type]);
      WN *use_vec_unroll_preg = AWN_LdidSym(&vec_unroll_preg_symbol);
      WN* parent = LWN_Get_Parent(copy);
      INT kid = 0;
      while(WN_kid(parent, kid) != copy && kid < WN_kid_count(parent))
	kid++;
      FmtAssert(WN_kid(parent, kid), ("Handle this"));
      WN_kid(parent, kid) = 
	AWN_Add(vec_type, use_vec_unroll_preg, copy);
      Du_Mgr->Add_Def_Use(vec_unroll_preg_store[unroll_type],
			  use_vec_unroll_preg);
      LWN_Parentize(parent);
    }
  }

  // Recurse
  for (INT kid = 0; kid < WN_kid_count(copy); kid ++)
    Create_Unroll_Copy(WN_kid(copy, kid), add_to_base, 
		       WN_kid(orig, kid), index_type, vec_preg_incr, loop);
  
  return;
}

// Rename all reduction statements involving the variable st, at old_offset, with
// new offset.
static void Rename_Reductions_Rec (WN_OFFSET orig_offset, ST *orig_st,
				   WN_OFFSET offset, ST *st, 
				   WN *node, TYPE_ID vmtype)
{
  if (WN_operator(node) == OPR_LDID && WN_st(node) == orig_st &&
      WN_load_offset(node) == orig_offset) {
    WN_set_desc(node, vmtype);
    WN_set_rtype(node, vmtype);
    WN_load_offset(node) = offset;
    WN_st_idx(node) = ST_st_idx(st);
  } 
  else if (WN_operator(node) == OPR_STID && WN_st(node) == orig_st &&
	   WN_store_offset(node) == orig_offset) {
    WN_set_desc(node, vmtype);
    WN_store_offset(node) = offset;
    WN_st_idx(node) = ST_st_idx(st);
  }
  for (INT kid = 0; kid < WN_kid_count(node); kid ++)
    Rename_Reductions_Rec(orig_offset, orig_st, 
			  offset, st, WN_kid(node, kid), vmtype);
}

static void Rename_Other_Reductions (WN_OFFSET orig_offset, ST *orig_st,
				     WN_OFFSET offset,
				     WN *start, WN *last, 
				     TYPE_ID vmtype) 
{
  WN *stmt = start;
  ST *st = WN_st(last);

  while (stmt != last) {
    Rename_Reductions_Rec(orig_offset, orig_st, offset, st, stmt, vmtype);
    stmt = WN_next(stmt);
  }
}

// Is stmt the last reduction statement involving that symbol in the block
// containing stmt.
static BOOL Is_Last_Red_Stmt ( WN *stmt )
{
  ST* st = WN_st(stmt);
  WN_OFFSET offset = WN_offset(stmt);
  WN* curr_stmt = WN_next(stmt);

  while(curr_stmt) {
    if (WN_operator(curr_stmt) == OPR_STID && WN_st(curr_stmt) == st &&
	WN_store_offset(curr_stmt) == offset)
      return FALSE;
    curr_stmt = WN_next(curr_stmt);
  }
  return TRUE;
}

static void Create_Stride1_Condition_If_Required (WN *array_base, 
						  WN **if_noncontig)
{
  TYPE_ID mtype = Is_Target_32bit() ? MTYPE_I4 : MTYPE_I8;
  OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,mtype, MTYPE_V);
  WN *value = WN_CreateIntconst(intconst_opc, 1);
  WN *stride;
  TY_IDX ty_dope = ST_type(WN_st(array_base));

  // bug 10379: we use the stride_multiplier directly to construct
  // the checking condition if the base is not in a dope structure
  if (TY_kind(ty_dope) != KIND_STRUCT || 
     strncmp(TY_name(ty_dope), ".dope.", 6) != 0){
     WN *array = LWN_Get_Parent(array_base);
     WN *inner_str_m = WN_kid(array, WN_num_dim(array));
    stride = LWN_Copy_Tree(inner_str_m, TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(inner_str_m, stride, Du_Mgr);
    WN_set_rtype(stride, mtype);
     if (*if_noncontig)
       *if_noncontig = WN_CAND( *if_noncontig, WN_EQ( mtype, stride, value ) );
     else
       *if_noncontig = WN_EQ( mtype, stride, value );
    return;
  }
  FLD_ITER fld_iter_dope = Make_fld_iter(TY_fld(ty_dope));
  while(!FLD_last_field(fld_iter_dope)) fld_iter_dope++;
  FLD_HANDLE fld_dims(fld_iter_dope);
  UINT64 dims_offset = FLD_ofst(fld_dims);
  TY_IDX ty_dims = FLD_type(fld_dims);
  if (TY_kind(ty_dims) != KIND_ARRAY) return;
  TY_IDX ty_dope_bnd = TY_etype(ty_dims);
  if (TY_kind(ty_dope_bnd) != KIND_STRUCT) return;
  FLD_ITER fld_iter_dope_bnd = Make_fld_iter(TY_fld(ty_dope_bnd));
  while(!FLD_last_field(fld_iter_dope_bnd)) fld_iter_dope_bnd++; 
  FLD_HANDLE fld_dope_bnd(fld_iter_dope_bnd);
  UINT64 str_offset = FLD_ofst(fld_dope_bnd);	
  TY_IDX ty_dope_bnd_str = FLD_type(fld_dope_bnd);
  if (TY_kind(ty_dope_bnd_str) != KIND_SCALAR) return;
  stride = LWN_Copy_Tree(array_base, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(array_base, stride, Du_Mgr);
  WN_set_rtype(stride, mtype);
  WN_set_desc(stride, mtype);
  WN_offset(stride) = dims_offset + str_offset;
  if (*if_noncontig) 
    *if_noncontig = WN_CAND( *if_noncontig, WN_EQ( mtype, stride, value ) );
  else          
    *if_noncontig = WN_EQ( mtype, stride, value );
  return;
}

//-----------------------------------------------------------------------
// NAME: Version_Loop
// FUNCTION: Create two identical versions of 'wn_loop' under a bogus if
//   test.  
//-----------------------------------------------------------------------

static WN* Version_Loop(WN* wn_loop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  REDUCTION_MANAGER* rm = red_manager;
  WN_MAP version_map = WN_MAP_Create(&LNO_local_pool);
  WN* wn_copy = LWN_Copy_Tree(wn_loop, TRUE, LNO_Info_Map, TRUE, version_map);
  BOOL all_internal = WN_Rename_Duplicate_Labels(wn_loop, wn_copy,
    Current_Func_Node, &LNO_local_pool);
  Is_True(all_internal, ("external labels renamed"));

  // Clone the dependences for the scalar copy.
  WN* wn_array[2];
  wn_array[0] = wn_loop;
  wn_array[1] = wn_copy;
  Unrolled_DU_Update(wn_array, 2, Do_Loop_Depth(wn_loop) - 1, TRUE, FALSE);
  dg->Versioned_Dependences_Update(wn_loop, wn_copy, Do_Loop_Depth(wn_loop),
    version_map);
  WN_MAP_Delete(version_map);
  if (rm != NULL)
    rm->Unroll_Update(wn_array, 2);

  // Start with a condition of .TRUE. for the version test
  WN* wn_total_cond = LWN_Make_Icon(Boolean_type, 1);
  LWN_Extract_From_Block(wn_loop);
  WN* wn_if = LWN_CreateIf(wn_total_cond, WN_CreateBlock(), WN_CreateBlock());
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_loop);
  LWN_Insert_Block_After(WN_else(wn_if), NULL, wn_copy);
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_loop));
  IF_INFO *ii =
    CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, FALSE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_default_pool),
    &LNO_default_pool);
  Build_Doloop_Stack(wn_if, stack);
  LNO_Build_If_Access(wn_if, stack);
  return wn_if;
}

//-----------------------------------------------------------------------
// NAME: Version_Region
// FUNCTION: Create two identical versions of 'region' under a bogus if
//   test. Update the region id of the cloned region.
//-----------------------------------------------------------------------

static WN* Version_Region(WN* region, WN *wn_loop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  REDUCTION_MANAGER* rm = red_manager;
  WN_MAP version_map = WN_MAP_Create(&LNO_local_pool);
  WN* region_copy = LWN_Copy_Tree(region, TRUE, LNO_Info_Map, TRUE, version_map);
  BOOL all_internal = WN_Rename_Duplicate_Labels(region, region_copy,
    Current_Func_Node, &LNO_local_pool);
  Is_True(all_internal, ("external labels renamed"));

  // Clone the dependences for the scalar copy.
  WN* wn_array[2];
  wn_array[0] = region;
  wn_array[1] = region_copy;
  Unrolled_DU_Update(wn_array, 2, Do_Loop_Depth(wn_loop) - 1, TRUE, FALSE);
  dg->Versioned_Dependences_Update(region, region_copy, Do_Loop_Depth(wn_loop),
    version_map);
  WN_MAP_Delete(version_map);
  if (rm != NULL)
    rm->Unroll_Update(wn_array, 2);

  // Start with a condition of .TRUE. for the version test
  WN* wn_total_cond = LWN_Make_Icon(Boolean_type, 1);
  LWN_Extract_From_Block(region);
  WN* wn_if = LWN_CreateIf(wn_total_cond, WN_CreateBlock(), WN_CreateBlock());
  LWN_Insert_Block_After(WN_then(wn_if), NULL, region);
  LWN_Insert_Block_After(WN_else(wn_if), NULL, region_copy);
  WN_Set_Linenum(wn_if, WN_Get_Linenum(region));
  IF_INFO *ii =
    CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, TRUE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_default_pool),
    &LNO_default_pool);
  Build_Doloop_Stack(wn_if, stack);
  LNO_Build_If_Access(wn_if, stack);
  return wn_if;
}

static BOOL Simd_Pre_Analysis(WN *innerloop, char *verbose_msg)
{
   //general testing according do loop information
   DO_LOOP_INFO *dli = Get_Do_Loop_Info(innerloop);
   if(!dli){
     sprintf(verbose_msg, "Loop info was not set.");
     return FALSE;
   }

   if(dli->Has_Bad_Mem || dli->Has_Calls || dli->Has_Gotos || dli->Has_Exits){
    sprintf(verbose_msg, "Loop has calls or Gotos");
    return FALSE;
   }

   if(!dli->Is_Inner){
    sprintf(verbose_msg, "Loop is not innermost");
    return FALSE;
   }
  
  // Bug 3784
  // Check for useless loops (STID's use_list is empty) of the form
  // do i
  //   x = a[i]
  // enddo
  // and screen them out from vectorizer.
   for (WN *stmt=WN_first(WN_do_body(innerloop)); stmt; stmt=WN_next(stmt)) {
    if (WN_operator(stmt) == OPR_STID) {
       USE_LIST* use_list=Du_Mgr->Du_Get_Use(stmt);
      if (!use_list) {
          sprintf(verbose_msg, "Loop has missing use_list.");
          return FALSE;
        }
      }
   }

  // if the loop index var is live at exit and cannot be finalized,
  // we will not vectorize
  if (Index_Variable_Live_At_Exit(innerloop)) {
    // Bug 5139 - The Loop index variable is probably a LAST PRIVATE
    // variable and vectorizing this loop here will cause MP lowerer
    // to not write out the loop index variable from the last iteration.
    if (Do_Loop_Is_Mp(innerloop) && !Early_MP_Processing){
      sprintf(verbose_msg, "Index variale lives at the exit of OpenMP loop.");
      return FALSE;
    }

    if (!Upper_Bound_Standardize(WN_end(innerloop),TRUE)){
      sprintf(verbose_msg, "Loop upper bound can not be std.");
      return FALSE;
    }
    
    Finalize_Index_Variable(innerloop,FALSE);
    scalar_rename(WN_start(innerloop));
  }

  if (Loop_Has_Asm(innerloop)) {
    sprintf(verbose_msg, "Loop has inline assembly.");
    return FALSE;
  }

  // if the loop upper bound is too complicated, we will not vectorize
  if (find_loop_var_in_simple_ub(innerloop)==NULL){
    sprintf(verbose_msg, "Loop upper bound too complicated.");
    return FALSE;
  }

 // Bug 5730 - OMP DO LOOP that are REDUCTIONs should not be vectorized
  // because of lack of functionality in the OMP lowerer.
  if (Do_Loop_Is_Mp(innerloop) && !Early_MP_Processing) {
    WN *enclosing_parallel_region, *region_pragma;
    BOOL reduction = FALSE, pdo = FALSE;
    enclosing_parallel_region = LWN_Get_Parent(innerloop);
    while(enclosing_parallel_region &&
          WN_operator(enclosing_parallel_region) != OPR_REGION)
      enclosing_parallel_region =
        LWN_Get_Parent(enclosing_parallel_region);
    if (PU_cxx_lang(Get_Current_PU()) &&
        Is_Eh_Or_Try_Region(enclosing_parallel_region))
      enclosing_parallel_region =
        LWN_Get_Parent(LWN_Get_Parent(enclosing_parallel_region));
    FmtAssert(enclosing_parallel_region, ("NYI"));
    region_pragma = WN_first(WN_region_pragmas(enclosing_parallel_region));
    while(region_pragma && (!reduction || !pdo)) {
      if (WN_pragma(region_pragma) == WN_PRAGMA_REDUCTION)
        reduction = TRUE;
      else if (WN_pragma(region_pragma) == WN_PRAGMA_PDO_BEGIN)
        pdo = TRUE;
      region_pragma = WN_next(region_pragma);
    }//end while
    if (pdo && reduction) {
     sprintf(verbose_msg, "Reduction loop in a DO region.");
     return FALSE;
    }
  }//end if

  //bug 9141 improve simd diagnostics, also split unit stride checking out of gathering simd ops
  non_unit_stride = "unknown";
  if (!Unit_Stride_Reference(WN_do_body(innerloop), innerloop, TRUE)) {
    sprintf(verbose_msg, "Non-contiguous array \"%s\" reference exists.", 
                                                non_unit_stride);
    return FALSE;
   }

   if(!Simd_Benefit(WN_do_body(innerloop))){
      sprintf(verbose_msg, "Vectorization is not likely to be beneficial (try -LNO:simd=2 to vectorize it).");
      return FALSE;
    }   
   return TRUE;
}

//global variables that simd analysis is about to set
//for vectorization phase
static STACK_OF_WN *vec_simd_ops;
static INT *simd_operand_invariant[3];
static BOOL *simd_op_last_in_loop;
static SIMD_KIND *simd_op_kind;
static TYPE_ID index_type;
static BOOL needs_scalar_expansion;

static BOOL SA_Set_SimdOps_Info1(WN* body,
                                 SCALAR_REF_STACK *simd_ops,
                                 char *verbose_msg)
{
  STACK_OF_WN *invariant_ops =
     CXX_NEW(STACK_OF_WN(&SIMD_default_pool),&SIMD_default_pool);
  INT invariant_operands = 0;
  INT curr_num_simd = 0;
  WN* simd_op;

  for (INT i=0; i<simd_ops->Elements(); i++){
    simd_op = simd_ops->Top_nth(i).Wn;   
    WN* stmt=simd_op;
    WN* stmt1;
    BOOL under_scf=FALSE;
    while((stmt1=LWN_Get_Parent(stmt)) != body){
       stmt = stmt1;
       if (WN_opcode(stmt)==OPC_BLOCK){
         if (!(LNO_Simd_Vect_If && Simd_vect_conf.Is_SSE41()) || (WN_opcode(LWN_Get_Parent(stmt)) != OPC_IF))
           under_scf=TRUE;
         break;
      }
    }     
    if (under_scf)
      continue;
    TYPE_ID rtype = WN_rtype(simd_op);
    TYPE_ID desc = WN_desc(simd_op);
    // CHANGED
    if ((LNO_Simd_Vect_If && Simd_vect_conf.Is_SSE41()) && (WN_operator(LWN_Get_Parent(simd_op)) == OPR_IF))
      under_if = TRUE;
    FmtAssert(is_vectorizable_op(WN_operator(simd_op), rtype, desc),
              ("Handle this piece"));
    if (!is_vectorizable_op(WN_operator(simd_op), rtype, desc))
      continue; //will never happen due to the above assert
    if ((LNO_Simd_Vect_If && Simd_vect_conf.Is_SSE41()) && (WN_operator(LWN_Get_Parent(simd_op)) == OPR_IF) && (under_if))
      under_if = FALSE;
    for (INT kid_no=0; kid_no<WN_kid_count(simd_op); kid_no++){
      WN* tmp=WN_kid(simd_op,kid_no);
      SIMD_OPERAND_KIND kind=simd_operand_kind(tmp,LWN_Get_Parent(body));

      //bug 10136: we don't count constants
      if(kind==Invariant && WN_operator(tmp) == OPR_LDID)
         Count_Invariant(invariant_ops, tmp);

      if (kind == Invariant ||
          (kind == Simple &&
           (WN_operator(tmp) == OPR_CONST ||
            WN_operator(tmp) == OPR_INTCONST)))
        simd_operand_invariant[kid_no][curr_num_simd] = 1;
      else
        simd_operand_invariant[kid_no][curr_num_simd] = 0;
    }
    curr_num_simd ++;
    vec_simd_ops->Push(simd_op);

    if (WN_rtype(simd_op) != MTYPE_V &&
        WN_rtype(simd_op) != MTYPE_F8 && WN_rtype(simd_op) != MTYPE_I8) {
      invariant_operands = -1;
    }
    else if (WN_desc(simd_op) != MTYPE_V &&
             WN_desc(simd_op) != MTYPE_F8 && WN_desc(simd_op) != MTYPE_I8) {
      invariant_operands = -1;
    }
   }

  if (vec_simd_ops->Elements()==0){
      sprintf(verbose_msg, "Loop has 0 vectorizable ops.\n");
      return FALSE;
  }
  if(invariant_operands != -1)
     invariant_operands = invariant_ops->Elements();

   CXX_DELETE(invariant_ops, &SIMD_default_pool);

  if ((Is_Target_64bit() && invariant_operands >= 16) ||
      (Is_Target_32bit() && invariant_operands >= 8)) {
      sprintf(verbose_msg, "Loop has too many loop invariants.");
      return FALSE;
    }
  return TRUE;  
}

static void SA_Set_SimdOps_Info2()
{
 // For all SIMD ops that belong to same loop, we need to call Find_Simd_Kind
  // to find what the combination SIMD Kind is.
  typedef HASH_TABLE<WN*, WN**> HTABLE_TYPE;
  HTABLE_TYPE *hash_table =
    CXX_NEW(HTABLE_TYPE(vec_simd_ops->Elements(),
                        &LNO_local_pool), &LNO_local_pool);
  INT max_size = 0;
  simd_op_last_in_loop =
    CXX_NEW_ARRAY(BOOL, vec_simd_ops->Elements(),&LNO_local_pool);
  simd_op_kind =
    CXX_NEW_ARRAY(SIMD_KIND, vec_simd_ops->Elements(),&LNO_local_pool);

  WN *istore, *simd_op;
  for (INT i=0; i < vec_simd_ops->Elements(); i++){
    simd_op=vec_simd_ops->Top_nth(i);

    istore=LWN_Get_Parent(simd_op);
    WN* new_body=Find_Do_Body(istore);
    WN* new_loop=LWN_Get_Parent(new_body);
    WN **newwn = CXX_NEW_ARRAY(WN*,vec_simd_ops->Elements(),&LNO_local_pool);
    INT num = 1, j;
    BOOL found = FALSE;
    WN* found_eq = NULL;
    INT found_eq_loc;

    // Initialize
    newwn[0] = simd_op;
    simd_op_kind[i] = INVALID;
    simd_op_last_in_loop[i] = TRUE;

    // If simd_op belongs to one of earlier simd ops' hash entry then,
    // set hash table equivalent to previous entry and skip this entry.
    for (j = i - 1; j >= 0 && !found; j --) {
      WN **checkwn = hash_table->Find(vec_simd_ops->Top_nth(j));
     for (INT k = 0; k < max_size && !found; k++) {
        if (checkwn[k] == simd_op) {
          found = TRUE;
          found_eq = vec_simd_ops->Top_nth(j);
          found_eq_loc = j;
        }
      }
    }
    if (found_eq) {
      hash_table->Enter(simd_op, hash_table->Find(found_eq));
      simd_op_kind[i] = simd_op_kind[found_eq_loc];
      simd_op_last_in_loop[found_eq_loc] = FALSE;
      continue;
    }

    // Look for simd ops that belong to same loop.
    for (j=i+1; j<vec_simd_ops->Elements(); j++) {
      WN *simd_op_next=vec_simd_ops->Top_nth(j);

      WN* istore_next=LWN_Get_Parent(simd_op_next);
      WN* new_body_next = Find_Do_Body(istore_next);
      WN* new_loop_next=LWN_Get_Parent(new_body_next);

      if (new_loop == new_loop_next)
        newwn[num++] = simd_op_next;
    }
    hash_table->Enter(simd_op, newwn);
    if (max_size < num)
      max_size = num;

    // At this point, the hash table entry op for this simd_op has all
    // the simd ops that are inside the same loop. Calculate simd_kind
    // for this simd_op
    STACK_OF_WN *vec_simd_ops_tmp=
      CXX_NEW(STACK_OF_WN(&SIMD_default_pool),&SIMD_default_pool);
    for (j = 0; j < num; j++)
      vec_simd_ops_tmp->Push(newwn[j]);
    simd_op_kind[i] = Find_Simd_Kind(vec_simd_ops_tmp);
  }
}


static BOOL SA_Loop_Has_Dependence_Cycles(WN *innerloop, char *verbose_msg)
{
  DO_LOOP_INFO* dli=Get_Do_Loop_Info(innerloop);
  WN* loop_copy = LWN_Copy_Tree(innerloop, TRUE, LNO_Info_Map);
  // Set loop info for copy loop
  DO_LOOP_INFO* new_loop_info =
    CXX_NEW(DO_LOOP_INFO(dli,&LNO_default_pool), &LNO_default_pool);
  Set_Do_Loop_Info(loop_copy, new_loop_info);
  if (!adg->Add_Deps_To_Copy_Block(innerloop, loop_copy, TRUE)) {
    LNO_Erase_Dg_From_Here_In(loop_copy, adg);
    sprintf(verbose_msg, "Too many edges in Dependence graph.");
    return TRUE;
  }
  Copy_Def_Use(WN_start(innerloop), WN_start(loop_copy),
                WN_index(innerloop), FALSE /* synch */);
  Copy_Def_Use(WN_end(innerloop), WN_end(loop_copy),
                WN_index(innerloop), FALSE /* synch */);
  Simd_Copy_Def_Use_For_Loop_Stmt(innerloop, loop_copy);

  if (Analyse_Dependencies(loop_copy)) {
    LNO_Erase_Dg_From_Here_In(loop_copy, adg);
    sprintf(verbose_msg, "Loop has dependencies.");
    LWN_Update_Def_Use_Delete_Tree(loop_copy, Du_Mgr);
    return TRUE;
  }    
  LNO_Erase_Dg_From_Here_In(loop_copy, adg);
  LWN_Update_Def_Use_Delete_Tree(loop_copy, Du_Mgr);
  return FALSE;
}

static void SA_Version_F90_Loops_For_Contiguous(WN *innerloop)
{
    WN *array_base, *simd_op;
    WN *if_noncontig = NULL;
    STACK_OF_WN *noncontig_array_dope=
        CXX_NEW(STACK_OF_WN(&SIMD_default_pool),&SIMD_default_pool);
    for (INT i=vec_simd_ops->Elements()-1; i >= 0; i--){
      simd_op=vec_simd_ops->Top_nth(i);
      for (INT kid = 0; kid < WN_kid_count(simd_op); kid ++){
        WN* opnd = WN_kid(simd_op, kid);
        if (WN_operator(opnd) == OPR_ILOAD &&
            WN_operator(WN_kid0(opnd)) == OPR_ARRAY &&
            WN_element_size(WN_kid0(opnd)) < 0 &&
            WN_operator(WN_array_base(WN_kid0(opnd))) == OPR_LDID) {
          array_base = WN_array_base(WN_kid0(opnd));
          BOOL entered = FALSE;
          for (INT id = 0; id < noncontig_array_dope->Elements(); id ++){
            if (SYMBOL(noncontig_array_dope->Bottom_nth(id)) ==
                SYMBOL(array_base)) {
              entered = TRUE;
              break;
            }
          }
          if (entered) continue;
          Create_Stride1_Condition_If_Required(array_base, &if_noncontig);
          noncontig_array_dope->Push(array_base);
        }
      }
      WN *parent = LWN_Get_Parent(simd_op);
      if (WN_operator(parent) == OPR_ISTORE &&
          WN_operator(WN_kid1(parent)) == OPR_ARRAY &&
          WN_element_size(WN_kid1(parent)) < 0 &&
          WN_operator(WN_array_base(WN_kid1(parent))) == OPR_LDID) {
        array_base = WN_array_base(WN_kid1(parent));
        BOOL entered = FALSE;
        for (INT id = 0; id < noncontig_array_dope->Elements(); id ++) {
          if (SYMBOL(noncontig_array_dope->Bottom_nth(id)) ==
              SYMBOL(array_base)) {
            entered = TRUE;
            break;
          }
        }
        if (entered) continue;
        Create_Stride1_Condition_If_Required(array_base, &if_noncontig);
        noncontig_array_dope->Push(array_base);
      }
    }
    if (if_noncontig) {
      if (Do_Loop_Is_Mp(innerloop)){
        // Bug 7258 - when loop is inside MP region, clone the outer region
        WN* enclosing_parallel_region;
        enclosing_parallel_region = LWN_Get_Parent(innerloop);
        while(enclosing_parallel_region &&
              WN_operator(enclosing_parallel_region) != OPR_REGION)
          enclosing_parallel_region =
            LWN_Get_Parent(enclosing_parallel_region);
        if (PU_cxx_lang(Get_Current_PU()) &&
            Is_Eh_Or_Try_Region(enclosing_parallel_region))
          enclosing_parallel_region =
            LWN_Get_Parent(LWN_Get_Parent(enclosing_parallel_region));
        WN *stmt_before_region = WN_prev(enclosing_parallel_region);
        FmtAssert(stmt_before_region, ("NYI"));
        WN *parent_block = LWN_Get_Parent(enclosing_parallel_region);
        WN *wn_if = Version_Region(enclosing_parallel_region, innerloop);
        WN_if_test(wn_if) = if_noncontig;
        LWN_Insert_Block_After(parent_block, stmt_before_region, wn_if);
        LWN_Parentize(wn_if);
      } else {
        WN *stmt_before_loop = WN_prev(innerloop);
        WN *parent_block = LWN_Get_Parent(innerloop);
        WN *wn_if = Version_Loop(innerloop);
        WN_if_test(wn_if) = if_noncontig;
        LWN_Insert_Block_After(parent_block, stmt_before_loop, wn_if);
        LWN_Parentize(wn_if);
      }
   }//end if_noncontig
} 


static BOOL Simd_Analysis(WN *innerloop, char *verbose_msg)
{

  WN *stmt;
  WN *body = WN_do_body(innerloop);
  needs_scalar_expansion = FALSE; //tempora global
  //set index type
  index_type=WN_rtype(WN_end(innerloop));
  
  //GATHER all ops in innerloop that can and should be vectorized
  non_vect_op = NULL; //opcode may not have name
  Induction_Seen = FALSE;
  Inconsistent_Induction = FALSE;
  SCALAR_REF_STACK *simd_ops =
        CXX_NEW(SCALAR_REF_STACK(&SIMD_default_pool),&SIMD_default_pool);
  for (stmt=WN_first(body); stmt; stmt=WN_next(stmt)){
    if (!Gather_Vectorizable_Ops(stmt, simd_ops, &SIMD_default_pool, innerloop)){
        if (!Inconsistent_Induction){
          if(non_vect_op==NULL) 
             Report_Non_Vectorizable_Op(stmt); //report the opcode name of the stmt
           sprintf(verbose_msg, "Expression rooted at op \"%s\"(line %d) is not vectorizable.", 
                                 non_vect_op, Srcpos_To_Line(WN_Get_Linenum(stmt)));
         }
        else sprintf(verbose_msg, "Induction loop has to be split");      
      return FALSE;
    }//end !Gather
  }//end for

  if (simd_ops->Elements()==0) {//no simd op in this loop
      sprintf(verbose_msg, "Loop has 0 vectorizable ops.");
      return FALSE;
  }

  if (Too_Few_Iterations (innerloop, simd_ops)) {
      sprintf(verbose_msg, "Too few iterations.");
      return FALSE;
  }

  //WHETHER scalar expansion is required    
  for(stmt=WN_first(body); stmt && curr_simd_red_manager; stmt=WN_next(stmt)){
    if (WN_operator(stmt) == OPR_STID &&
        curr_simd_red_manager->Which_Reduction(stmt) == RED_NONE){
      STACK<WN*>* equivalence_class=
        Scalar_Equivalence_Class(stmt, Du_Mgr,&LNO_local_pool);
      if (!equivalence_class){ //scalar expansion required
        if(LNO_Run_Simd == 2){ //currently only under -LNO:simd=2
          needs_scalar_expansion = TRUE;
          break;
        }
        sprintf(verbose_msg, "Vectorization requires scalar expansion(implemented under -LNO:simd=2).");
        return FALSE;
      }
    }
  }

  //SET vect_simd_op and simd_operand_invariant  
  vec_simd_ops = CXX_NEW(STACK_OF_WN(&SIMD_default_pool),&SIMD_default_pool);
  for(INT ii=0; ii<3; ii++){
    simd_operand_invariant[ii] =
                     CXX_NEW_ARRAY(INT,simd_ops->Elements(),&SIMD_default_pool);
  }
  if(!SA_Set_SimdOps_Info1(body, simd_ops, verbose_msg))
    return FALSE;

    // Do not vectorize loops with complex type invariants : loop invariants
  // will be hoisted out of loop and may be assigned to registers and
  // vcast_complex skips the STID, or LDID from preg_C8 (need further work in
  // back-end to enable this). If vcast_complex skips the STID to the
  // invariant, then we would be left with an inconsistent state (store to
  // invariant C8 will be lowered to 2 F8 type pregs and we would be expecting
  // a V16C8 128-bit value inside the loop).
  for (INT i=vec_simd_ops->Elements()-1; i >= 0; i--) {
    WN *simd_op=vec_simd_ops->Top_nth(i);
    if (WN_rtype(simd_op) != MTYPE_C8 && WN_desc(simd_op) != MTYPE_C8)
      continue;
    if (simd_operand_invariant[0][vec_simd_ops->Elements()-i-1] == 1 ||
        (WN_kid_count(simd_op) >= 2 &&
         simd_operand_invariant[1][vec_simd_ops->Elements()-i-1] == 1) ||
        (WN_kid_count(simd_op) >= 3 &&
         simd_operand_invariant[2][vec_simd_ops->Elements()-i-1] == 1)) {
        sprintf(verbose_msg, "Loop has C8 type invariant.\n");
        return FALSE;
      }
  }//end for

  //DEPENDENCE analysis
  if(SA_Loop_Has_Dependence_Cycles(innerloop, verbose_msg))
    return FALSE;
  
  //Bug 6644 - version loop if non-contiguous arrays may exist.
  if (PU_src_lang(Get_Current_PU()) == PU_F90_LANG)
     SA_Version_F90_Loops_For_Contiguous(innerloop);

  REF_LIST_STACK* writes = CXX_NEW(REF_LIST_STACK(&SIMD_default_pool),
        &SIMD_default_pool);
  REF_LIST_STACK* reads = CXX_NEW(REF_LIST_STACK(&SIMD_default_pool),
        &SIMD_default_pool);

  SCALAR_STACK* scalar_writes = CXX_NEW(SCALAR_STACK(&SIMD_default_pool),
        &SIMD_default_pool);
  SCALAR_STACK* scalar_reads = CXX_NEW(SCALAR_STACK(&SIMD_default_pool),
        &SIMD_default_pool);
  SCALAR_REF_STACK* params = CXX_NEW(SCALAR_REF_STACK(&SIMD_default_pool),
        &SIMD_default_pool);

  DOLOOP_STACK *stack1=CXX_NEW(DOLOOP_STACK(&SIMD_default_pool),
                              &SIMD_default_pool);
  Build_Doloop_Stack(innerloop, stack1);

  // gather again after simd ops are splitted out of old stmts
  Init_Ref_Stmt_Counter();
  INT32 gather_status = 0;
  for (stmt=WN_first(body); stmt && gather_status != -1; stmt=WN_next(stmt)){
    gather_status=New_Gather_References(stmt,writes,reads,stack1,
        scalar_writes,scalar_reads,
        params,&SIMD_default_pool) ;
  }
  if (gather_status == -1) {
      sprintf(verbose_msg, "Error in gathering phase.");
      return FALSE;
  }

  // main statement dependence graph for statements in the loop
  SCC_DIRECTED_GRAPH16 *dep_g_p =
    CXX_NEW(SCC_DIRECTED_GRAPH16(ESTIMATED_SIZE,ESTIMATED_SIZE),&SIMD_default_pool);
  // hash table which associates the statements in the loop and vertices in the
  // above dependence graph 'dep_g_p'
  WN2VINDEX *stmt_to_vertex=
  CXX_NEW(WN2VINDEX(ESTIMATED_SIZE, &SIMD_default_pool), &SIMD_default_pool);

  for (stmt=WN_first(body); stmt; stmt=WN_next(stmt)){
    VINDEX16 v=dep_g_p->Add_Vertex();
    if (v==0){
      sprintf(verbose_msg, "Statement dependence graph problem.");
      CXX_DELETE(dep_g_p, &SIMD_default_pool);
      return FALSE;
     }
    stmt_to_vertex->Enter(stmt, v); //vertex_stmt mapping
  }
  // a dictionary used for looking up the bit position for a symbol
  BINARY_TREE<NAME2BIT> *mapping_dictionary =
     CXX_NEW(BINARY_TREE<NAME2BIT>(&SIMD_default_pool), &SIMD_default_pool);

  // list of references that use scalar-expandable variables
  FF_STMT_LIST expandable_ref_list;

  // step 2: examine all reads and writes and do the following
  //            1. classify them as scalar or array
  //            2. create name to bit position mappings for new symbol names
  //            3. if the ref is STID, check if it is scalar expandable
  UINT sym_count=simd_2(innerloop, scalar_reads, scalar_writes,
                        mapping_dictionary, expandable_ref_list);
  // we also need to have a set of expandable scalars
  BIT_VECTOR Expandable_Scalar_Set(sym_count, &SIMD_default_pool);

  // now look at all references in 'expandable_ref_list' and set the
  // corresponding bit in 'Expandable_Scalar_Set'
  FF_STMT_ITER e_iter(&expandable_ref_list);
  for (FF_STMT_NODE* ref_node=e_iter.First(); !e_iter.Is_Empty();
      ref_node=e_iter.Next()) {
      NAME2BIT temp_map;
      temp_map.Set_Symbol(ref_node->Get_Stmt());
      Expandable_Scalar_Set.Set(mapping_dictionary->Find(temp_map)->
               Get_Data()->Get_Bit_Position());
  }

  if (LNO_Test_Dump) {
    printf("Expandable_Scalar_Set=\n");
    Expandable_Scalar_Set.Print(stdout);
  }

  WN_MAP sdm=WN_MAP_Create(&SIMD_default_pool);
  ARRAY_DIRECTED_GRAPH16 *sdg =
    CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,sdm,LEVEL_ARRAY_GRAPH),
      &SIMD_default_pool);

  for (stmt = WN_first(body); stmt; stmt = WN_next(stmt)){
    if (!Map_Stmt_To_Level_Graph(stmt,sdg)){
      FmtAssert(0, ("Error in mapping stmt to level graph\n"));
      CXX_DELETE(dep_g_p, &SIMD_default_pool);
      CXX_DELETE(sdg, &SIMD_default_pool);
      WN_MAP_Delete(sdm);
      sprintf(verbose_msg, "Error in mapping stmt to level graph.");
      return FALSE;
    }
  }

  BOOL status=Generate_Scalar_Dependence_For_Statement_Dependence_Graph(
    innerloop, scalar_reads, scalar_writes, params, sdg, red_manager,
    &Expandable_Scalar_Set, mapping_dictionary);
  if (status==FALSE) {
    sprintf(verbose_msg, "Statement dependence graph problem.");
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    CXX_DELETE(sdg, &SIMD_default_pool);
    WN_MAP_Delete(sdm);
    return FALSE;
  }

  status=Generate_Array_Dependence_For_Statement_Dependence_Graph(
    innerloop, reads, writes, sdg, red_manager, adg);
  if (status==FALSE) {
    sprintf(verbose_msg, "Statement dependence graph problem.");
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    CXX_DELETE(sdg, &SIMD_default_pool);
    WN_MAP_Delete(sdm);
    return FALSE;
   }


  EINDEX16 e=sdg->Get_Edge();
  while (e) {
    WN* source=sdg->Get_Wn(sdg->Get_Source(e));
    WN* sink=sdg->Get_Wn(sdg->Get_Sink(e));
    if (LWN_Get_Parent(source) == body || LWN_Get_Parent(sink) == body)
      // add edges only if the source and sink are immediate children
      dep_g_p->Add_Unique_Edge(
        stmt_to_vertex->Find(source),
        stmt_to_vertex->Find(sink));
    e=sdg->Get_Next_Edge(e);

  }

  // ac_g is the acyclic condensation graph of dep_g_p
  // it stores dependence relations between SCCs
  SCC_DIRECTED_GRAPH16 *ac_g;
  ac_g = dep_g_p->Acyclic_Condensation(&SIMD_default_pool);

  VINDEX16 total_scc = dep_g_p->Get_Scc_Count();

  // scc[i] is a list of statemens in i-th SCC
  FF_STMT_LIST *scc;
  scc = CXX_NEW_ARRAY(FF_STMT_LIST, total_scc+1, &SIMD_default_pool);

  UINT *scc_size=CXX_NEW_ARRAY(UINT, total_scc+1, &SIMD_default_pool);

  for (INT i=1; i<=total_scc; i++)
    scc_size[i]=0;

  // Append statements to the statement list of proper SCC
  for (stmt = WN_first(WN_do_body(innerloop)); stmt; stmt = WN_next(stmt)) {
    VINDEX16 scc_id;
    scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
    scc[scc_id].Append(stmt, &SIMD_default_pool);
    scc_size[scc_id]++;
  }
  if (LNO_Test_Dump)
    for (INT i=1; i<=total_scc; i++) {

      printf("Simd:scc %d:", i);
      FF_STMT_ITER s_iter(&scc[i]);
      INT j=0;
      for (FF_STMT_NODE *stmt_node=s_iter.First(); !s_iter.Is_Empty();
           stmt_node=s_iter.Next()) {
          stmt=stmt_node->Get_Stmt();
          Dump_WN(stmt,stdout,TRUE,4,4);
        j++;
      }
      printf(" has %d stmts\n", j);
   }

  if (total_scc==1 && scc_size[1]>1) {
    CXX_DELETE(ac_g, &SIMD_default_pool);
    CXX_DELETE(dep_g_p, &SIMD_default_pool);
    CXX_DELETE(sdg, &SIMD_default_pool);
    WN_MAP_Delete(sdm);
    sprintf(verbose_msg, "Loop has to be split.");
    return FALSE;
  }

  UINT_DYN_ARRAY* new_loops;
  WN *simd_op;

  new_loops=simd_fis_merge_scc_to_form_new_loop(total_scc,scc,scc_size,
    innerloop,ac_g);

  // new_loops[i] is the i-th seed SCC
  if (LNO_Run_Simd != 2 && new_loops->Lastidx() != 0) {
    // If there are super vectors in the loop then it may still be okay to
    // vectorize (bug 1544)
    // TODO_1.2: compute the actual overhead due to scalar expansion
    // and offset that with the benefit of vectorization, to decide
    // whether to proceed with vectorization. Too much scalar expansion
    // may kill. This is not relevant to this bug.
    BOOL super_vector = FALSE;
    for (INT i=0; i<vec_simd_ops->Elements() && !super_vector; i++) {
      simd_op=vec_simd_ops->Top_nth(i);
      if (OPCODE_is_compare(WN_opcode(simd_op)) &&
          MTYPE_is_size_double(WN_desc(simd_op)))
        continue;
      if (WN_rtype(simd_op) != MTYPE_V &&
          WN_rtype(simd_op) != MTYPE_F8 && WN_rtype(simd_op) != MTYPE_I8)
        super_vector = TRUE;
      else if (WN_desc(simd_op) != MTYPE_V &&
               WN_desc(simd_op) != MTYPE_F8 && WN_desc(simd_op) != MTYPE_I8)
        super_vector = TRUE;
    }

    if (!super_vector) {
      CXX_DELETE(dep_g_p, &SIMD_default_pool);
      CXX_DELETE(ac_g, &SIMD_default_pool);
      CXX_DELETE(sdg, &SIMD_default_pool);
      WN_MAP_Delete(sdm);
      sprintf(verbose_msg,"Loop has to be scalar-expanded.");
      return FALSE;
    }
  }

  // at this point, we know that the loop can be vectorized
  // reorder the loop statement according to their dependencies
  toplogical_reordering(innerloop, 1, adg);

  // separate the loop and expand scalars which is expandable and has
  // references in different fissions loops
  if (needs_scalar_expansion)
    simd_fis_separate_loop_and_scalar_expand(new_loops,scc, innerloop,
                                             expandable_ref_list);

  // For all SIMD ops that belong to same loop, we need to call Find_Simd_Kind
  // to find what the combination SIMD Kind is.
  SA_Set_SimdOps_Info2();

  CXX_DELETE(dep_g_p, &SIMD_default_pool);
  CXX_DELETE(ac_g, &SIMD_default_pool);
  CXX_DELETE(sdg, &SIMD_default_pool);
  WN_MAP_Delete(sdm);

 return TRUE;
}  
 
//determine the number of iters to peel for simd alignment
static INT Simd_Align_Best_Peel(STACK_OF_WN *vec_simd_ops, SIMD_KIND *simd_op_kind,
                          INT **simd_op_best_align, WN *innerloop)
{
    INT peel_benefit[16], peel;
    INT best_peel = 0, best_benefit = 0;
    for (peel = 0; peel < 16; peel ++) {
      peel_benefit[peel] = -1;
      for (INT j=vec_simd_ops->Elements()-1; j >= 0; j--) {
        WN* simd_op=vec_simd_ops->Top_nth(j);

        if (simd_op_kind[j] == INVALID || 
            innerloop != LWN_Get_Parent(Find_Do_Body(simd_op)))
          continue; 

        for(INT k=0; k<4; k++)
        if (simd_op_best_align[k][j] == peel)
          peel_benefit[peel] ++;
      } //end j 
    }//end peel

    for (peel = 0; peel < 16; peel ++) {
      if (peel_benefit[peel] > best_benefit) {
        best_benefit = peel_benefit[peel];
        best_peel = peel;
      }
    }
   return best_peel;
}

//whether the upper is not known at compile time
static BOOL Simd_Align_UB_Variable(WN *innerloop)
{
  WN* end = WN_end(innerloop);
  SYMBOL loop_index(WN_index(innerloop));
  if (WN_kid_count(end) != 2)
    return TRUE;
  else if (WN_operator(WN_kid0(end)) == OPR_LDID &&
           loop_index == SYMBOL(WN_kid0(end))) {
    if (WN_operator(WN_kid1(end)) != OPR_INTCONST)
       return TRUE;
  } else if (WN_operator(WN_kid1(end)) == OPR_LDID &&
             loop_index == SYMBOL(WN_kid1(end))) {
    if (WN_operator(WN_kid0(end)) != OPR_INTCONST)
      return TRUE;
  }
 return FALSE;
}

//to determine whether or not an array reference can be aligned,
//and if yes, the num_of_iters to peel for this alignment
static INT Simd_Align_Analysis(INT init_align, WN *load_store,
                                     WN *simd_op, INT size,
                                     SIMD_KIND simd_kind, 
                                     WN *innerloop, BOOL is_store)
{
      INT alignment = init_align;
      WN *istore = is_store?load_store:LWN_Get_Parent(simd_op);
      TYPE_ID index_type=WN_rtype(WN_end(innerloop));
      WN *array0 = is_store?WN_kid1(load_store):WN_kid0(load_store);
      ACCESS_ARRAY* aa0=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,array0);
      WN *copy = LWN_Copy_Tree(WN_kid(array0, WN_kid_count(array0) - 1),
                               TRUE, LNO_Info_Map);
      WN *start = LWN_Copy_Tree(WN_kid0(WN_start(innerloop)),
                                TRUE, LNO_Info_Map);
      SYMBOL symbol(WN_index(innerloop));
      BOOL const_lb =
        WN_operator(WN_kid0(WN_start(innerloop))) == OPR_INTCONST;
      if (!const_lb) {
        if (WN_operator(WN_kid0(WN_start(innerloop))) == OPR_LDID) {
          SYMBOL symnew(WN_kid0(WN_start(innerloop)));
          // Replace_Symbol copies def_use
          Replace_Symbol(copy, symbol, symnew, WN_kid0(WN_start(innerloop)));
          //Replace_Symbol(copy, symbol, symnew, NULL);
        }
      } else {
        if (WN_operator(copy) == OPR_LDID) {
          SYMBOL sym(copy);
          if (sym == symbol) {
            OPCODE intconst_opc=
              OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);
            copy = WN_CreateIntconst(intconst_opc,
                                WN_const_val(WN_kid0(WN_start(innerloop))));
          }
        } else
          Simd_Replace_With_Constant(copy, symbol,
                                     WN_kid0(WN_start(innerloop)),
                                     index_type);
      }
      copy = WN_Simplify_Tree(copy);
      if(WN_operator(WN_array_base(array0))==OPR_LDID) // maybe different
       // array base is a pointer, can not align : bug 8595
        alignment = -2;
      else if (WN_operator(copy) != OPR_INTCONST)
        alignment = -2;
      else if (!WN_has_sym(WN_array_base(array0))) // may be different
        // TODO: compute alignment for cases like a.b[i] (bug 1703)
        alignment = -2;
      else if (!is_store && ((WN_operator(simd_op) == OPR_CVT &&
                MTYPE_byte_size(WN_rtype(simd_op)) !=
                MTYPE_byte_size(WN_desc(simd_op))) ||
               (WN_operator(simd_op) == OPR_TRUNC &&
                MTYPE_byte_size(WN_rtype(simd_op)) !=
                MTYPE_byte_size(WN_desc(simd_op)))))
        // related to Bug 2665 but faults at run-time
        alignment = -2;
      else if (!is_store && Vec_Unit_Size[simd_kind] != MTYPE_byte_size(WN_desc(load_store)))
        alignment = -2;
      else {
        if (aa0->Dim(aa0->Num_Vec()-1)->Loop_Coeff(Do_Loop_Depth(innerloop))==
            -1)
          copy = LWN_CreateExp2(OPCODE_make_op(OPR_SUB,Mtype_TransferSign(MTYPE_I4, index_type), MTYPE_V),
                                copy,
                                WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST,
                                                                 index_type,
                                                                 MTYPE_V),
                                                  (16/ABS(WN_element_size(array0)))-1));
        INT fn = WN_const_val(copy);
        // Compute A0 (alignment of the base of the array).
        WN *array_base = WN_array_base(array0); //may be different for store
        ST *st = WN_st(array_base);

        TY_IDX ty_iload0;
        ST *base_st; INT64 offset;
        Base_Symbol_And_Offset(WN_st(array_base),
                               &base_st, &offset);

        BOOL var_base = WN_operator(array_base) != OPR_LDA;
        if (!var_base)
          offset += WN_lda_offset(array_base);
        offset += WN_offset(load_store); // bug 2612 --- may be different for store

        if(!is_store || !var_base){ // load should always do this
          ty_iload0 = ST_type(base_st);
          alignment = Simd_Compute_Best_Align(offset, fn, size);
          Set_TY_align_exp (ty_iload0, 4);
           // ARRAYs within COMMON blocks that are not padded to align
           Base_Symbol_And_Offset(WN_st(array_base),
                               &base_st, &offset);
           if (ST_sclass(base_st) == SCLASS_COMMON && offset%16 != 0)
             alignment = -2;

           // Fortran Equivalenced arrays should not be aligned
           if (ST_is_equivalenced(st))
             alignment = -2;
        }else{//store does this when var_base. See bug 8112 
           ty_iload0 = WN_ty(istore); // istore ?
           if (TY_kind(ty_iload0) == KIND_POINTER)
             ty_iload0 = TY_pointed(ty_iload0);
           else ty_iload0 = MTYPE_To_TY(MTYPE_I1); // dummy
        }
        // Check for array bases that are pointer variables assigned to registers.
        // There is no way to align such arrays.
        if (ST_sclass(base_st) == SCLASS_REG)
          alignment = -2;

        // If base address was a memory invariant hoisted out of the loop,
        // it is not possible to know the base alignment without looking at
        // ud chain.
        if (strncmp(ST_name(base_st), 
                    Temp_Symbol_Prefix "_misym",
                    sizeof(Temp_Symbol_Prefix "_misym") - 1) == 0)
           alignment = -2;

        if (WN_num_dim(array0) >= 2 &&
            (!WN_kid(array0, WN_num_dim(array0)) ||
             WN_operator(WN_kid(array0, WN_num_dim(array0))) != OPR_INTCONST ||
             (WN_const_val(WN_kid(array0, WN_num_dim(array0)))*
              // TODO: Element size can be computed more reliably by following
              // the pointed-to type, but we will just use the desc type of the
              // ISTORE.
              MTYPE_byte_size(WN_desc(istore) == MTYPE_V ? //istore is parent
                              WN_rtype(istore) : WN_desc(istore)))%16 != 0))
          alignment = -2;
        if (alignment == -2 ||
            (TY_kind(ST_type(st)) == KIND_STRUCT &&
             strncmp(TY_name(ST_type(st)),".dope.",6) == 0) ||
            (TY_kind(ST_type(st)) == KIND_POINTER && !Align_Unsafe &&
             (!ST_pt_to_unique_mem(st) || ST_is_temp_var(st) ||
              ST_pt_to_compiler_generated_mem(st))))
          ; // Do nothing
        else if (TY_kind(ST_type(st)) == KIND_POINTER) {
          TY_IDX ty = TY_pointed(ST_type(st));
          Set_TY_align_exp(ty, 4);
          Set_TY_pointed(ST_type(st), ty);
        }
        else if (base_st->sym_class != CLASS_BLOCK &&
            ST_sclass(st) != SCLASS_FORMAL &&
            ST_sclass(st) != SCLASS_FORMAL_REF)
          Set_ST_type(base_st, ty_iload0);
        else if (ST_sclass(st) != SCLASS_AUTO &&
                 ST_sclass(st) != SCLASS_EXTERN &&
                 ST_sclass(st) != SCLASS_FORMAL &&
                 ST_sclass(st) != SCLASS_FORMAL_REF) {
          TY_IDX st_ty_idx = ST_type(st);
          Set_TY_align_exp(st_ty_idx, 4);
          Set_ST_type(st, st_ty_idx);
          Set_STB_align(base_st, 16);
          Simd_Reallocate_Objects = TRUE;
        } else if (ST_sclass(st) == SCLASS_AUTO &&
                   Stack_Alignment() == 16 &&
                   ST_level(st) == Current_scope) {
          TY_IDX st_ty_idx = ST_type(st);
          Set_TY_align_exp(st_ty_idx, 4);
          Set_ST_type(st, st_ty_idx);
        }
        if (alignment == -2 ||
            (TY_kind(ST_type(st)) == KIND_STRUCT &&
             strncmp(TY_name(ST_type(st)),".dope.",6) == 0) ||
            (TY_kind(ST_type(st)) == KIND_POINTER && !Align_Unsafe &&
             (!ST_pt_to_unique_mem(st) || ST_is_temp_var(st) ||
              ST_pt_to_compiler_generated_mem(st))))
          alignment = -2;
        else if (ST_sclass(st) == SCLASS_AUTO &&
                 (ST_level(st) != Current_scope ||
                  Stack_Alignment() != 16))
          alignment = -2;
        else if (ST_sclass(st) == SCLASS_FORMAL ||
                 ST_sclass(st) == SCLASS_FORMAL_REF)
          alignment = -2;
        else if (base_st->sym_class == CLASS_BLOCK &&
                 alignment < 0) // Bug 2322
          alignment = 0; // we have just aligned this block
      }
  return alignment;
}

//align iloads and istores
static void Simd_Align_Load_Store(WN *load_store, BOOL is_load)
{   
        TY_IDX ty_load_store = TY_pointed(is_load?
                       WN_load_addr_ty(load_store):WN_ty(load_store));
        TY_IDX ty_idx = 0; 
        TY &ty = New_TY (ty_idx);
        Set_TY_align (ty_load_store, 16);

        TY_Init (ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
                 Save_Str ("anon_ptr."));
        Set_TY_pointed (ty, ty_load_store);
        if(is_load)
          WN_set_load_addr_ty (load_store, ty_idx);
        else WN_set_ty (load_store, ty_idx);
}

//driver to align array references (iload and istores)
static void Simd_Align_Array_References( STACK_OF_WN *vec_simd_ops, 
                               SIMD_KIND *simd_op_kind,
                               INT **simd_op_best_align,
                               INT best_peel,
                               WN *innerloop)
{
    for (INT j=vec_simd_ops->Elements()-1; j >= 0; j--) {
      WN* simd_op=vec_simd_ops->Top_nth(j);

      if (innerloop != LWN_Get_Parent(Find_Do_Body(simd_op)) ||
          simd_op_kind[j] == INVALID)
          continue;

      WN *load_store[4];
      load_store[0]= WN_kid0(simd_op);
      load_store[1]= WN_kid_count(simd_op)>1 ? WN_kid1(simd_op):NULL;
      load_store[2]= WN_kid_count(simd_op)>2 ? WN_kid2(simd_op):NULL;
      load_store[3]= LWN_Get_Parent(simd_op);
      if (WN_operator(simd_op) == OPR_INTRINSIC_OP)
        for(INT k=0; k< 3; k++)
          if(load_store[k]){
            FmtAssert(WN_operator(load_store[k]) == OPR_PARM, ("NYI"));         
            load_store[k] = WN_kid0(load_store[k]);
          }
        
       for(INT k=0; k<4; k++)
          if(simd_op_best_align[k][j] == best_peel)
            Simd_Align_Load_Store(load_store[k], k!=3);
   } //end for j
}

static void Simd_Update_Index_Def_Use(WN *loop,WN *depend_loop, WN *what, SYMBOL sym)
{
    DOLOOP_STACK sym_stack(&LNO_local_pool);
    Find_Nodes(OPR_LDID, sym, what, &sym_stack);
    for (int k = 0; k < sym_stack.Elements(); k++) {
      WN* wn_use = sym_stack.Bottom_nth(k);
      //Delete_Def_Use(wn_use); //bug 5945: original dependence no longer holds
      Du_Mgr->Add_Def_Use(WN_start(depend_loop), wn_use);
      Du_Mgr->Add_Def_Use(WN_step(depend_loop), wn_use);
    }
    for (int k = 0; k < sym_stack.Elements(); k++) {
      WN* wn_use =  sym_stack.Bottom_nth(k);
      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_use);
      def_list->Set_loop_stmt(loop);
    }
}

static void Simd_Update_Loop_Info(WN *loop, WN *orig_loop,DO_LOOP_INFO *dli, BOOL set_unimportant)
{
    DO_LOOP_INFO* new_dli =
      CXX_NEW(DO_LOOP_INFO(dli,&LNO_default_pool), &LNO_default_pool);
    Set_Do_Loop_Info(loop, new_dli);

    //Update def use for loop index related
    SYMBOL symbol(WN_index(loop));
    Simd_Update_Index_Def_Use(loop, loop,WN_end(loop),symbol);
    Simd_Update_Index_Def_Use(loop, loop,WN_do_body(loop),symbol);
    Simd_Update_Index_Def_Use(loop, loop,WN_step(loop),symbol);
    Simd_Update_Index_Def_Use(loop, orig_loop,WN_start(loop),symbol);
    // Set Unimportant flag in loop_info.
    if (WN_kid_count(loop) == 6 && set_unimportant) {
      WN *loop_info = WN_do_loop_info(loop);
      WN_Set_Loop_Unimportant_Misc(loop_info);
      DO_LOOP_INFO *dli_p = Get_Do_Loop_Info(loop);
      dli_p->Set_Generally_Unimportant();
    }
}

static WN *Simd_Align_Generate_Peel_MV_Loops(WN *vloop, DO_LOOP_INFO *dli, ST *first_st)
{
  WN *wn_if = NULL;
  if (first_st &&
      LNO_Simd_peel_align) {
    SYMBOL symbol(WN_index(vloop));
    OPCODE ld_opc = WN_opcode(UBvar(WN_end(vloop)));
    OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
    WN *wn_lda = WN_CreateLda(op_lda, 0,
                              Make_Pointer_Type(ST_type(first_st)),
                              first_st);

    TYPE_ID exp_type = OPCODE_rtype(ld_opc);
    WN *align_cond = NULL;
    WN *wn_align_val = LWN_Make_Icon(exp_type, 15);
    OPCODE and_opc = OPCODE_make_op(OPR_BAND, exp_type, MTYPE_V);
    WN *wn_and = LWN_CreateExp2(and_opc, wn_lda, wn_align_val);
    WN *wn_zero = LWN_Make_Icon(exp_type, 0);
    OPCODE  opeq = OPCODE_make_op(OPR_NE, exp_type, exp_type); 
    align_cond = LWN_CreateExp2(opeq, wn_and, wn_zero);

    WN *stmt_before_loop = WN_prev(vloop);
    WN *parent_block = LWN_Get_Parent(vloop);
    wn_if = Version_Loop(vloop);
    WN_if_test(wn_if) = align_cond;
    LWN_Insert_Block_After(parent_block, stmt_before_loop, wn_if);
    LWN_Parentize(wn_if);
  }

  return wn_if;
}

//generate peeled loop for alignment
static void Simd_Align_Generate_Peel_Loop(WN *vloop, INT best_peel, DO_LOOP_INFO *dli)
{
    TYPE_ID index_type=WN_rtype(WN_end(vloop));
    OPCODE add_opc = OPCODE_make_op(OPR_ADD,index_type, MTYPE_V);
    OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);
    WN *start1 =  LWN_Copy_Tree(WN_kid0(WN_start(vloop)),TRUE, LNO_Info_Map);
    WN *start2 =  LWN_Copy_Tree(WN_kid0(WN_start(vloop)),TRUE, LNO_Info_Map);
    WN *pub = LWN_CreateExp2(add_opc,
                     WN_CreateIntconst(intconst_opc, best_peel-1),start1);
    WN *vlb = LWN_CreateExp2(add_opc,
                     WN_CreateIntconst(intconst_opc, best_peel),start2);

    // Update use=def for newly created loop bounds.
    Update_Symbol_Use_Def(WN_kid0(WN_start(vloop)), pub,
                          WN_index(vloop), FALSE);
    Update_Symbol_Use_Def(WN_kid0(WN_start(vloop)), vlb,
                          WN_index(vloop), FALSE);

    WN *ploop = LWN_Copy_Tree(vloop, TRUE, LNO_Info_Map);

    Copy_Def_Use(WN_start(vloop), WN_start(ploop),
                 WN_index(vloop), FALSE /* synch */);
    Simd_Copy_Def_Use_For_Loop_Stmt(vloop, ploop);

    // Update loop upper bound for peeled loop.
    WN *loop_end = WN_end(ploop);
    // Delete last loop end def-use (we are going to modify)
    LWN_Update_Def_Use_Delete_Tree(loop_end, Du_Mgr);
    WN_kid1(loop_end) = pub;
    LWN_Set_Parent(WN_end(ploop),ploop);
    LWN_Parentize(WN_end(ploop));

    // Adjust loop lower bound for vectorizable loop
    WN *start_vloop = WN_start(vloop);
    WN_kid0(start_vloop) = vlb;

    LWN_Set_Parent(WN_kid0(start_vloop),start_vloop);
    LWN_Set_Parent(start_vloop,vloop);

    // Set loop info for peeled loop
    Simd_Update_Loop_Info(ploop,vloop, dli,TRUE);
    
    // Now, insert the peeled loop before the vectorizable innerloop.
    LWN_Insert_Block_Before(LWN_Get_Parent(vloop),vloop,ploop);

    // Parentize both loops
    LWN_Parentize(vloop);
    LWN_Parentize(ploop);
    LWN_Set_Parent(ploop, LWN_Get_Parent(vloop));
    // Add any new vertices and update dep info.
    Add_Vertices(WN_do_body(ploop));
    adg->Fission_Dep_Update(ploop, 1);
    adg->Fission_Dep_Update(vloop, 1);

    dli->Loop_Align_Peeled = TRUE;
    DO_LOOP_INFO* peel_dli = Get_Do_Loop_Info (ploop);
    peel_dli->Loop_Align_Peeled = TRUE;
}

static INT Simd_Count_Good_Vector(STACK_OF_WN *vec_simd_ops, SIMD_KIND *simd_op_kind)
{
  INT good_vector=0;
  for (INT i=0; i<vec_simd_ops->Elements(); i++) {
    WN *simd_op=vec_simd_ops->Top_nth(i);
    if (simd_op_kind[i] == INVALID)
      continue;
    if (OPCODE_is_compare(WN_opcode(simd_op)) &&
        MTYPE_is_size_double(WN_desc(simd_op)))
      continue;
    if (WN_rtype(simd_op) != MTYPE_V &&
        WN_rtype(simd_op) != MTYPE_C8 &&
        WN_rtype(simd_op) != MTYPE_F8 && WN_rtype(simd_op) != MTYPE_I8)
      good_vector ++;
    else if (WN_desc(simd_op) != MTYPE_V &&
             WN_rtype(simd_op) != MTYPE_C8 &&
             WN_desc(simd_op) != MTYPE_F8 && WN_desc(simd_op) != MTYPE_I8)
      good_vector ++;
  }
 return good_vector;
}

//create a copy of the loop for remainder
static WN *Simd_Create_Remainder_Loop(WN *innerloop)
{ //bug 11057 -- yet to find a way to simply this routine
  WN *remainderloop =  LWN_Copy_Tree(innerloop, TRUE, LNO_Info_Map);
  if (!adg->Add_Deps_To_Copy_Block(innerloop, remainderloop, TRUE))
      FmtAssert(FALSE, ("Probably too many edges in dependence graph."));
      // If the remainder loop were to be added, the loop ends in innerloop
      // before xformation would be identical to the loop end for the
      // remainder loop
  Copy_Def_Use(WN_end(innerloop), WN_end(remainderloop),
               WN_index(innerloop), FALSE /* synch */);
   Simd_Copy_Def_Use_For_Loop_Stmt(innerloop, remainderloop);
  
 return remainderloop; 
} 

// Simd_Handle_Negative_Coefficient_Helper() is helper function of 
// Simd_Handle_Negative_Coefficient(). This function is to handle 
// the vectorizable expression like "(double)a[i]" where a[i] is 
// either 4 byte integer or floating point.
//
static void Simd_Handle_Negative_Coefficient_Helper(
  WN *parent,     // shffle's parent
  INT which_kid,  // which kid 
  WN *array,      // array to shuffle
  WN *loop,       // the loop
  BOOL no_shuffle) {

  // step 1: derive vector length etc
  //

  // This func is supposed to be called only by Simd_Handle_Negative_Coefficient.
  //
  Is_True (WN_operator(parent) == OPR_CVT, ("wrong caller"));

  INT vect_len = 16/MTYPE_byte_size (WN_rtype(parent)); 
  Is_True (vect_len == 2 && WN_element_size(array) == 4, 
           ("For now, this func only handle F8I4CVT and F8F4CVT"));

  ACCESS_ARRAY* aa = (ACCESS_ARRAY*)WN_MAP_Get (LNO_Info_Map,array);
  Is_True (aa->Dim(aa->Num_Vec()-1)->Loop_Coeff(Do_Loop_Depth(loop))==-1,
           ("loop coefficient is not -1"));

  WN *opnd = LWN_Get_Parent(array);
  TYPE_ID vect_ty = MTYPE_is_float (WN_desc(opnd)) ? MTYPE_V8F4 : MTYPE_V8I4;

  // step 2: adjust array index. e.g. If the vectorizable expression is 
  //  "(double)a[i]" where sizeof(a[i]) = 4, the index need to subtract by 
  //  "vector_length - 1". In this case, vect-len = 2, so, the ultimate
  //  vectorized expression is like "F16F8V8I4CVT shuffle ((*(V8I4*)&a[i-1])).
  //
  TYPE_ID idx_ty = WN_rtype(WN_end(loop));
  OPCODE adjust = OPCODE_make_op (OPR_INTCONST, idx_ty, MTYPE_V);
  OPCODE sub_opc = OPCODE_make_op (OPR_SUB,
                    Mtype_TransferSign (MTYPE_I4, idx_ty), MTYPE_V);

  WN* orig_idx = WN_array_index (array, WN_num_dim(array)-1);
  WN_array_index (array, WN_num_dim(array)-1) = 
          LWN_CreateExp2 (sub_opc, orig_idx,
                          WN_CreateIntconst(adjust, vect_len-1));

  LWN_Parentize (array);
  if (!no_shuffle) {
    WN_kid (parent, which_kid) = 
      LWN_CreateExp1 (OPCODE_make_op(OPR_SHUFFLE, vect_ty, vect_ty),
                             WN_kid(parent, which_kid));
    // "0" means reverse vector elements. As of I write this note, 
    // CG doesn't respect this parameter -- it bindly reverses elements 
    // regardless WN_offset() is 0 or not.
    //
    // Since the vector involved here is shorter (8 byte) than underlying 
    // machine is providing, care must be take by CG to only swap elements 
    // 0 and 1, instead of all four elements.
    //
    WN_offset (WN_kid(parent, which_kid)) = 0; 
  }

  LWN_Parentize(parent);
}

//handle negative loop coefficient
static void Simd_Handle_Negative_Coefficient(
                                      WN *parent,/*shffle's parent*/
                                      INT which_kid,/*which kid ?*/
                                      WN *array,/*array to shuffle*/
                                      WN *loop, /* the loop */
                                      BOOL no_shuffle)
{
  FmtAssert(WN_element_size(array), ("NYI"));

  ACCESS_ARRAY* aa = (ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,array);
  if (aa->Dim(aa->Num_Vec()-1)->Loop_Coeff(Do_Loop_Depth(loop)) != -1)
    return;

  TYPE_ID res_ty = WN_rtype (parent);
  TYPE_ID desc_ty = WN_desc (parent);
  if (WN_operator (parent) == OPR_CVT && 
      MTYPE_is_float(res_ty) && 
      MTYPE_byte_size(res_ty) != MTYPE_byte_size(desc_ty)) {
    if (MTYPE_byte_size(res_ty) == 8 && MTYPE_byte_size(desc_ty) == 4) {
      Simd_Handle_Negative_Coefficient_Helper (parent, which_kid, array, 
                                               loop, no_shuffle);
      return;
    } else {
      FmtAssert (FALSE, ("Don't know how to handle %s", 
                 OPCODE_name (WN_opcode(parent))));
    }
  }

  INT incr = 16/ABS(WN_element_size(array));
    
  {
      TYPE_ID vector_type;
      WN *opnd = LWN_Get_Parent(array);
      switch(ABS(WN_element_size(array))) {
      case 1: vector_type = MTYPE_V16I1; break;
      case 2: vector_type = MTYPE_V16I2; break;
      case 4:
              if (MTYPE_is_float(WN_desc(opnd)))
                vector_type = MTYPE_V16F4;
              else
                vector_type = MTYPE_V16I4;
              break;
      case 8:
              if (MTYPE_is_float(WN_desc(opnd)))
                vector_type = MTYPE_V16F8;
              else
                vector_type = MTYPE_V16I8;
              break;
      default: FmtAssert(FALSE, ("NYI"));
      }//end switch
    TYPE_ID index_type=WN_rtype(WN_end(loop));
    OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);
    OPCODE sub_opc= OPCODE_make_op(OPR_SUB,Mtype_TransferSign(MTYPE_I4, index_type), MTYPE_V); 
    WN* index = WN_kid(array, WN_num_dim(array)<<1);
    WN_kid(array, WN_num_dim(array)<<1) =
       LWN_CreateExp2(sub_opc, index,
                      WN_CreateIntconst(intconst_opc, incr-1));
    LWN_Parentize(array); //up tp now is fine, we just create array
    if(no_shuffle == FALSE){
        WN_kid(parent, which_kid) = //shuffle needed
              LWN_CreateExp1(OPCODE_make_op(OPR_SHUFFLE, vector_type,
                                           vector_type),
                             WN_kid(parent, which_kid));
        WN_offset(WN_kid(parent, which_kid)) = 0 /* Reverse */;
    }//end if no_shuffle = FALSE
    LWN_Parentize(parent);
  }
}

static void Simd_Add_Shuffle_For_Negative_Coefficient(WN* simd_op, WN *loop)
{
  //handle kids
  for (INT kid = 0; kid < WN_kid_count(simd_op); kid ++){
    WN *opnd = WN_kid(simd_op, kid);
    if (WN_operator(opnd) == OPR_ILOAD && 
            WN_operator(WN_kid0(opnd)) == OPR_ARRAY)
       Simd_Handle_Negative_Coefficient(simd_op, kid, WN_kid0(opnd), loop, FALSE);
  }
  //handle parent
  WN *parent = LWN_Get_Parent(simd_op);
  if(WN_operator(parent) == OPR_ISTORE && 
      WN_operator(WN_kid1(parent)) == OPR_ARRAY){
      //if we store a constant or invariant, we don't need shuffle
      BOOL no_shuffle = (WN_operator(WN_kid0(parent)) == OPR_PAREN && //all under paren
              ((WN_operator(WN_kid0(WN_kid0(parent))) == OPR_LDID &&  //ldid but not ind var
                SYMBOL(WN_kid0(WN_kid0(parent))) != SYMBOL(WN_index(loop))) ||
               WN_operator(WN_kid0(WN_kid0(parent))) == OPR_INTCONST ||//constants
               WN_operator(WN_kid0(WN_kid0(parent))) == OPR_CONST));
     Simd_Handle_Negative_Coefficient(parent,0,WN_kid1(parent), loop, no_shuffle);
  }        
}

static TYPE_ID Simd_Get_Vector_Type(WN *istore)
{
   TYPE_ID vmtype, type;
   if (!OPCODE_is_store(WN_opcode(istore))){
      // bug 2336 - trace up the correct type
      WN* stmt = istore;
      while(stmt && !OPCODE_is_store(WN_opcode(stmt)) &&
            WN_operator(stmt) != OPR_DO_LOOP &&
            // Bug 5225 - trace up should stop at a CVT or a TRUNC.
            WN_operator(stmt) != OPR_CVT &&
	    WN_operator(stmt) != OPR_IF &&
            WN_operator(stmt) != OPR_TRUNC) {
        stmt = LWN_Get_Parent(stmt);
      }
      if (!stmt || WN_operator(stmt) == OPR_DO_LOOP)
        type = WN_rtype(istore); //use parent's desc
      else {
	if(WN_operator(stmt) == OPR_IF)
	  // istore is (part of) the if-condition since if-body will have ISTORE as parent stmt.
	  // This returns desc of OPR_NE (root of if-condition). 
	  if (WN_operator(istore) == OPR_IF)
	    type = WN_desc(WN_kid0(istore));
	  else
	    type = WN_desc(istore);
	else
	  type = WN_desc(stmt); //use store's desc
      }
    } else type = WN_desc(istore);//parent is a store
    switch(type) {
      case MTYPE_V16C8: case MTYPE_C8:
        vmtype = MTYPE_V16C8;
        break;
      case MTYPE_V16C4: case MTYPE_C4:
        vmtype = MTYPE_V16C4;
        break;
      case MTYPE_V16F4: case MTYPE_F4:
        vmtype = MTYPE_V16F4;
        break;
      case MTYPE_V16F8: case MTYPE_F8:
        vmtype = MTYPE_V16F8;
        break;
      case MTYPE_V16I1: case MTYPE_I1:
      case MTYPE_U1:
        vmtype = MTYPE_V16I1;
        break;
      case MTYPE_V16I2: case MTYPE_I2:
      case MTYPE_U2:
        vmtype = MTYPE_V16I2;
        break;
      case MTYPE_V16I4: case MTYPE_I4:
      case MTYPE_U4:
        vmtype = MTYPE_V16I4;
        break;
      case MTYPE_V16I8: case MTYPE_I8:
      case MTYPE_U8:
        vmtype = MTYPE_V16I8;
        break;
    default:
      DevWarn("Unexpected type in Simd_Get_Vector_Type");
    }
  return vmtype;
}


// When vectorizing constants and invariants, care must be taken to appropriately 
// vectorize the second operand of OPR_SHL. Most constants/invariant can be vectorized 
// by replicating them in each b/w/d/q of the xmm register as per the type of the vector.
// In the case of packed shift left (psllw/d/q), the second operand must always be 
// loaded into the lower 64-bits of the 128-bit xmm reg or memory.  Note that if the 
// second argument is a constant it can be placed in a 1 byte immediate if it fits. 
// But the first option has been chosen because it fits easier with the existing framework.

static WN* Simd_Vectorize_Shift_Left_Amt (WN* const_wn, 
                                          WN *istore,  //parent of simd_op
                                          WN *simd_op) //const_wn's parent
{
  Is_True (WN_operator(simd_op) == OPR_SHL && WN_kid1(simd_op) == const_wn, 
           ("input WN isn't SHL"));

    WN* shift_amt = WN_Intconst (MTYPE_I8, WN_const_val (const_wn));
    WN* res = LWN_CreateExp1 (OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I8, MTYPE_I8), 
                              shift_amt);
    return res;
}

static WN *Simd_Vectorize_Constants(WN *const_wn,//to be vectorized 
                                    WN *istore,  //parent of simd_op
                                    WN *simd_op) //const_wn's parent
{
   FmtAssert(const_wn && (WN_operator(const_wn)==OPR_INTCONST ||
             WN_operator(const_wn)==OPR_CONST),("not a constant operand"));

   if (WN_operator(simd_op) == OPR_SHL && WN_kid1(simd_op) == const_wn) {
        return Simd_Vectorize_Shift_Left_Amt (const_wn, istore, simd_op);
   }

   TYPE_ID type;
   TCON tcon;
   ST *sym;
   if (WN_desc(istore) == MTYPE_V)
       type = WN_rtype(istore);
   else
       type = WN_desc(istore);

   if (WN_operator(simd_op) == OPR_CVT || WN_operator(simd_op) == OPR_TRUNC)
     type = WN_rtype(const_wn);

   if ((LNO_Simd_Vect_If && Simd_vect_conf.Is_SSE41()) && (WN_operator(istore) == OPR_IF)) {
     FmtAssert(WN_operator(simd_op) == OPR_NE, ("Condition of OPC_IF must be rooted at OPR_NE"));
     //Match the vector type of parent simd_op.
     //We know that this constant is a zero and so its vectorized version 
     //can be made to match the size of the vectorized parent.
     type = WN_desc(simd_op); 
   }

   if (WN_operator(simd_op) == OPR_PARM &&
          WN_operator(istore) == OPR_INTRINSIC_OP &&
          WN_intrinsic(istore) == INTRN_SUBSU2) {
        type = WN_desc(LWN_Get_Parent(istore));
    }

    WN* orig_const_wn = const_wn;

    switch (type) {
     case MTYPE_F4: case MTYPE_V16F4:
          WN_set_rtype(const_wn, MTYPE_V16F4);
          break;
     case MTYPE_F8: case MTYPE_V16F8:
          WN_set_rtype(const_wn, MTYPE_V16F8);
          break;
     case MTYPE_C4: case MTYPE_V16C4:
          WN_set_rtype(const_wn, MTYPE_V16C4);
          break;

     case MTYPE_U1: case MTYPE_I1: case MTYPE_V16I1:
          const_wn = 
            LWN_CreateExp1 (OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I1, MTYPE_I1), 
                            orig_const_wn);
          break;

     case MTYPE_U2: case MTYPE_I2: case MTYPE_V16I2:
          const_wn = 
            LWN_CreateExp1 (OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I2, MTYPE_I2), 
                            orig_const_wn);
          break;

     case MTYPE_U4: case MTYPE_I4: case MTYPE_V16I4:
          const_wn = 
            LWN_CreateExp1 (OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I4, MTYPE_I4), 
                            orig_const_wn);
          break;

     case MTYPE_U8: case MTYPE_I8: case MTYPE_V16I8:
          const_wn = 
            LWN_CreateExp1 (OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I8, MTYPE_I8), 
                            orig_const_wn);
          break;

     } // end switch
      
    return const_wn;
}

static WN *Simd_Vectorize_Invariants(WN *inv_wn, 
                                     WN *istore,
                                     WN *simd_op)
{
  TYPE_ID desc = WN_desc(inv_wn);
  TYPE_ID type;
  if (WN_desc(istore) == MTYPE_V)
      type = WN_rtype(istore);
  else
      type = WN_desc(istore);

  OPERATOR opr = WN_operator(inv_wn);   
  if (WN_operator(simd_op) == OPR_CVT || WN_operator(simd_op) == OPR_TRUNC) {
    if (WN_operator(simd_op) == OPR_CVT && (BINARY_OP(opr)))
	type = WN_rtype(inv_wn);
    else
      type = desc;
  }

   switch (type) {
     case MTYPE_V16C8: case MTYPE_C8:
          // We need not replicate this load, but we do set the types
          // to mean there are two F8 quantities.
          WN_set_rtype(inv_wn, MTYPE_V16C8);
          WN_set_desc(inv_wn, MTYPE_V16C8);
          break;
     case MTYPE_V16C4: case MTYPE_C4:
          WN_set_rtype(inv_wn, MTYPE_F8);
          WN_set_desc(inv_wn, MTYPE_F8);
          inv_wn = //replicate
            LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16C4, MTYPE_F8),
                           inv_wn);
          break;
     case MTYPE_V16F4: case MTYPE_F4:
          inv_wn =
            LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16F4, MTYPE_F4),
                           inv_wn);
          break;
     case MTYPE_V16F8: case MTYPE_F8:
          inv_wn =
            LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16F8, MTYPE_F8),
                           inv_wn);
          break;
     case MTYPE_V16I1: case MTYPE_U1: case MTYPE_I1:
          inv_wn =
            LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I1, MTYPE_I1),
                           inv_wn);
          break;
     case MTYPE_V16I2: case MTYPE_U2: case MTYPE_I2:
          if (WN_operator(simd_op) == OPR_SHL && WN_kid1(simd_op) == inv_wn) {
	    WN* cvt_wn = 
	      LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_I8, MTYPE_I2),
			     inv_wn);
	    inv_wn =
	      LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I8, MTYPE_I8),
			     cvt_wn);
	  }
	  else
	    inv_wn =
	      LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I2, MTYPE_I2),
			     inv_wn);
          break;
     case MTYPE_V16I4: case MTYPE_U4: case MTYPE_I4:
          if (WN_operator(simd_op) == OPR_SHL && WN_kid1(simd_op) == inv_wn) {
	    WN* cvt_wn = 
	      LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_I8, MTYPE_I4),
			     inv_wn);
	    inv_wn =
	      LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I8, MTYPE_I8),
			     inv_wn);
	  }
	  else
	    inv_wn =
	      LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I4, MTYPE_I4),
			     inv_wn);
          break;
     case MTYPE_V16I8: case MTYPE_U8: case MTYPE_I8:
          inv_wn =
            LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, MTYPE_V16I8, MTYPE_I8),
                           inv_wn);
          break;
        }//end switch
   return inv_wn;
}

static void Simd_Vectorize_Intrinsics(WN *simd_op)
{
      if (WN_intrinsic(simd_op) == INTRN_SUBSU2) {
        WN_intrinsic(simd_op) = INTRN_SUBSV16I2;
        WN_set_rtype(WN_kid0(simd_op), MTYPE_V16I2);
        WN_set_rtype(WN_kid1(simd_op), MTYPE_V16I2);
      } else {
        INTRINSIC intrn = WN_intrinsic(simd_op);
        switch(intrn) {

        case INTRN_F8SIGN:
          WN_intrinsic(simd_op) = INTRN_SIGNV16F8;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F8);
          WN_set_rtype(WN_kid1(simd_op), MTYPE_V16F8);
          break;

       case INTRN_F4SIGN:
          WN_intrinsic(simd_op) = INTRN_SIGNV16F4;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F4);
          WN_set_rtype(WN_kid1(simd_op), MTYPE_V16F4);
          break;

        case INTRN_F4EXPEXPR:
          WN_intrinsic(simd_op) = INTRN_V16F4EXPEXPR;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F4);
          break;

        case INTRN_F4EXP:
          WN_intrinsic(simd_op) = INTRN_V16F4EXP;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F4);
          break;
        case INTRN_F8EXP:
          WN_intrinsic(simd_op) = INTRN_V16F8EXP;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F8);
          break;
        case INTRN_F4LOG:
          WN_intrinsic(simd_op) = INTRN_V16F4LOG;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F4);
          break;
        case INTRN_F8LOG:
          WN_intrinsic(simd_op) = INTRN_V16F8LOG;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F8);
          break;
        case INTRN_F4LOG10:
          WN_intrinsic(simd_op) = INTRN_V16F4LOG10;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F4);
          break;
        case INTRN_F8LOG10:
          WN_intrinsic(simd_op) = INTRN_V16F8LOG10;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F8);
          break;
        case INTRN_F4SIN:
          WN_intrinsic(simd_op) = INTRN_V16F4SIN;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F4);
          break;
        case INTRN_F8SIN:
          WN_intrinsic(simd_op) = INTRN_V16F8SIN;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F8);
          break;
        case INTRN_F4COS:
          WN_intrinsic(simd_op) = INTRN_V16F4COS;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F4);
          break;        
        case INTRN_F8COS:
          WN_intrinsic(simd_op) = INTRN_V16F8COS;
          WN_set_rtype(WN_kid0(simd_op), MTYPE_V16F8);
          break;
        }
      }
}

static INT Simd_Unroll_Times_By_SimdKind(SIMD_KIND simd_kind)
{
    INT vect= 1; //default no unrolling
    switch(simd_kind) {
     case V16I1: vect = 16; break;
     case V16I2: vect = 8;  break;
     case V16F4: vect = 4;  break;
     case V16F8: vect = 2;  break; 
     default:    vect = 1;  break;
    }
    return vect;
}

static INT Simd_Unroll_Times_By_VectorType(TYPE_ID vmtype)
{
   INT vect = 1;
   switch (vmtype){    
     case MTYPE_V16C4: case MTYPE_V16I8: case MTYPE_V16F8: vect = 2; break;
     case MTYPE_V16I4: case MTYPE_V16F4: vect = 4; break;
     case MTYPE_V16I2: vect = 8; break;
     case MTYPE_V16I1: vect = 16;break;
     default: vect=1;break;
   }
   return vect;
}

static void Simd_Update_Copy_Array_Index(WN *copy, WN *orig, 
                                         INT add_to_base, TYPE_ID index_type)
{
   OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);
   OPCODE add_opc= OPCODE_make_op(OPR_ADD,index_type, MTYPE_V);
   INT kid_for0 = WN_num_dim(copy)<<1;
   WN_kid(copy, kid_for0) =
          LWN_CreateExp2(add_opc, WN_kid(copy, kid_for0),
                         WN_CreateIntconst(intconst_opc, add_to_base));

   // Look at WN structure for OPR_ARRAY       
   for (INT aa_num = 0; aa_num < WN_num_dim(copy) - 1; aa_num ++){
      INT dim = aa_num + WN_num_dim(copy) + 1;
      LWN_Copy_Def_Use(WN_kid(orig,dim), WN_kid(copy,dim),Du_Mgr);
   }
}

static void Simd_Unroll_Statement( INT unroll_times, INT add_to_base,
                                   WN *istore, WN *vec_index_preg_store,
                                   WN *innerloop, TYPE_ID index_type)
{
    WN *copy_simd_op, *copy, *iload_copy=NULL, *origA=NULL;

    for (INT k = 1, sum = add_to_base; k < unroll_times; k ++){
      copy = LWN_Copy_Tree(istore, TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use(WN_kid0(istore),WN_kid0(copy), Du_Mgr);
      LWN_Copy_Def_Use(WN_kid1(istore),WN_kid1(copy), Du_Mgr);
      LWN_Set_Parent(copy, LWN_Get_Parent(istore));
      copy_simd_op = WN_kid0(copy);

      for(INT k=0; k < WN_kid_count(copy_simd_op); k++){
        iload_copy = WN_kid0(WN_kid(WN_kid0(copy),k)); //to trace down to the array node
        origA      = WN_kid0(WN_kid(WN_kid0(istore),k));
        if(iload_copy && WN_operator(iload_copy) == OPR_ARRAY){//self
           Simd_Update_Copy_Array_Index(iload_copy, origA, add_to_base, index_type);
        }
        else if(iload_copy && WN_operator(iload_copy) == OPR_ILOAD &&
                 WN_operator(WN_kid0(iload_copy)) == OPR_ARRAY &&
                 WN_operator(LWN_Get_Parent(iload_copy)) == OPR_SHUFFLE){
           iload_copy = WN_kid0(iload_copy); //ARRAY node
           origA = WN_kid0(origA);
           Simd_Update_Copy_Array_Index(iload_copy, origA, -add_to_base, index_type);
        }
        else if(iload_copy && WN_operator(iload_copy) == OPR_SHUFFLE &&
                 WN_operator(WN_kid0(iload_copy)) == OPR_ILOAD &&
                 WN_operator(WN_kid0(WN_kid0(iload_copy))) == OPR_ARRAY) {
           iload_copy = WN_kid0(WN_kid0(iload_copy)); // ARRAY node
           origA = WN_kid0(WN_kid0(origA));
           Simd_Update_Copy_Array_Index(iload_copy, origA, -add_to_base, index_type);
        } 
        else if(iload_copy) //Bug 2233
            Create_Unroll_Copy(WN_kid(WN_kid0(copy), k), add_to_base,
                           WN_kid(WN_kid0(istore), k), index_type,
                           vec_index_preg_store, innerloop);
     }//END kid handling
       
      //Now handle ISTORE's array
      ACCESS_ARRAY* aa = (ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,
                                                   WN_kid1(copy));
      if (aa->Dim(aa->Num_Vec()-1)->Loop_Coeff(Do_Loop_Depth(innerloop))==-1)
        add_to_base = -add_to_base;
      Simd_Update_Copy_Array_Index(WN_kid1(copy), WN_kid1(istore), add_to_base, index_type);
      add_to_base = ABS(add_to_base);
      // Parentize copy
      LWN_Parentize(copy);

      // Now, insert the new copy of the istore at the end of current loop
      // current loop should not be empty since we are performing vectorization on it
      WN *body = WN_do_body(innerloop);
      Is_True((body && WN_last(body)),
              ("Loop body should not be empty for unrolling"));
      if (vec_index_preg_store != NULL &&
              SYMBOL(WN_last(body)) == SYMBOL(vec_index_preg_store)) {
          Is_True((WN_first(body) != WN_last(body)),
                  ("Loop body was empty before we inserted an increment operation in Simd_Vectorize_Induction_Variables"));
          LWN_Insert_Block_After(body, WN_prev(WN_last(body)), copy);
      } else {
          LWN_Insert_Block_After(body, WN_last(body),copy);
      }

      // Add the vertices of copy to array dependence graph.
      Add_Vertices(copy);

      // Update array index increment for the next time around.
      add_to_base += sum;
    }
    // Add def use for newly added statements in innerloop
    SYMBOL symbol(WN_index(innerloop));
    DOLOOP_STACK sym_stack(&LNO_local_pool);
    INT k;
    Find_Nodes(OPR_LDID, symbol, WN_do_body(innerloop), &sym_stack);
    for (k = 0; k < sym_stack.Elements(); k++) {
      WN* wn_use = sym_stack.Bottom_nth(k);
      Du_Mgr->Add_Def_Use(WN_start(innerloop), wn_use);
      Du_Mgr->Add_Def_Use(WN_step(innerloop), wn_use);
    }
   for (k = 0; k < sym_stack.Elements(); k++) {
      WN* wn_use =  sym_stack.Bottom_nth(k);
      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_use);
      def_list->Set_loop_stmt(innerloop);
    }
}

// unroll statements that are necessary according to their appearance
// in the loop to keep the dependencies
static void Simd_Unroll_Necessary_Loop_Statements(WN *wn)
{
    Is_True((WN_opcode(wn) == OPC_BLOCK),
            ("This function should only be called with do loop body"));
    BOOL changed = FALSE; 

    do {
        changed = FALSE;
        WN *kid = WN_first(wn);
        while (kid) {
            if (WN_MAP_Get(unroll_map, kid) != NULL) {
                // we need to unroll this wn
                UNROLL_PARAMS *params = (UNROLL_PARAMS*) WN_MAP_Get(unroll_map, kid);
                if (params->unroll_times > 1) {
                    Simd_Unroll_Statement(2, params->add_to_base, kid, params->vec_index_preg_store,
                            params->innerloop, params->index_type);
                    params->unroll_times -= 1;
                    params->add_to_base += params->base_incr;
                } else {
                    WN_MAP_Set(unroll_map, kid, NULL);
                }
                changed = TRUE;
            }
            kid = WN_next(kid);
        }
    } while(changed);
}

static BOOL Simd_Good_Reduction_Load(WN *innerloop, WN *load)
{
  WN* stmt = Find_Stmt_Under(load,WN_do_body(innerloop));
  FmtAssert(stmt && (curr_simd_red_manager || WN_operator(stmt) != OPR_STID),("NYI"));
  if (WN_operator(stmt) == OPR_STID &&
        curr_simd_red_manager->Which_Reduction(stmt) != RED_NONE &&
        WN_st(load) == WN_st(stmt) && WN_offset(load) == WN_offset(stmt))
   return TRUE;
  else return FALSE;
}

static WN* Simd_Vectorize_Scalar_Reduction(WN *red_load, WN *red_store,
                                           WN *simd_op,  WN *innerloop,
                                           TYPE_ID vmtype, INT kid)
{
  WN_OFFSET orig_offset = WN_load_offset(red_load);
  ST *orig_st = WN_st(red_load);

  WN* tmp = red_load;
  WN* copy = LWN_Copy_Tree(tmp, TRUE, LNO_Info_Map);
  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(tmp);
  WN* loop = def_list->Loop_stmt();
  if(loop == innerloop)
     def_list->Set_loop_stmt(NULL);
  Du_Mgr->Delete_Def_Use(innerloop,tmp);
  Du_Mgr->Delete_Def_Use(red_store, tmp);
  LWN_Copy_Def_Use(tmp, copy, Du_Mgr);
  if (WN_operator(simd_op) != OPR_MAX && WN_operator(simd_op) != OPR_MIN)
      Delete_Def_Use(tmp);

  WN* last_tmp = tmp;
  TYPE_ID desc = WN_desc(tmp);
  TYPE_ID rtype = WN_rtype(tmp);
  WN_set_desc(tmp, vmtype);
  WN_set_rtype(tmp, vmtype);

  WN* last_op = simd_op;
  WN *parent = LWN_Get_Parent(last_op);
  while(parent && parent != red_store) {
        parent = LWN_Get_Parent(parent);
        last_op = LWN_Get_Parent(last_op); // lags istore by 1 level
  }
  FmtAssert(parent==red_store && WN_operator(red_store) == OPR_STID, ("NYI"));
 tmp = Split_Using_Preg(red_store, tmp, adg, FALSE);
  WN_OFFSET offset = WN_store_offset(tmp);
  if (WN_operator(simd_op) != OPR_MAX && WN_operator(simd_op) != OPR_MIN)
       LWN_Delete_Tree(last_tmp);
  else{
       WN_set_desc(last_tmp, desc);
       WN_set_rtype(last_tmp, rtype);
  }


  TCON tcon; INT value; float valuefp; double valuedp;
  ST* st; WN *inv_node;
  if (WN_operator(simd_op) == OPR_ADD ||
         WN_operator(simd_op) == OPR_SUB){
         value = 0; valuefp = 0.0F; valuedp = 0.0;
  }else if (WN_operator(simd_op) == OPR_MPY ||
                     WN_operator(simd_op) == OPR_DIV) {
         value = 1; valuefp = 1.0F; valuedp = 1.0;
  }
  if (WN_operator(simd_op) == OPR_ADD ||
      WN_operator(simd_op) == OPR_SUB ||
      WN_operator(simd_op) == OPR_DIV ||
      WN_operator(simd_op) == OPR_MPY) {
         if (!MTYPE_is_integral(desc)) {
            if (desc == MTYPE_F4)
               tcon = Host_To_Targ_Float_4 (MTYPE_F4, valuefp);
            else
               tcon = Host_To_Targ_Float (MTYPE_F8, valuedp);
            }
         else
            tcon = Host_To_Targ(MTYPE_I4, value);
         st = New_Const_Sym (Enter_tcon (tcon), Be_Type_Tbl(desc));
         inv_node = WN_CreateConst (OPR_CONST, vmtype, MTYPE_V, st);
   } else{
           inv_node = LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE, vmtype,
                                    Mtype_TransferSign(MTYPE_I4, desc)),last_tmp);
     }
   WN_kid0(tmp) = inv_node;
 //Hoist the new preg STID above this loop.
  LWN_Extract_From_Block(tmp);

  if(!Do_Loop_Is_Mp(innerloop))
     LWN_Insert_Block_Before(LWN_Get_Parent(innerloop), innerloop, tmp);
  else {
         // Bug 4850 - For DO loops inside a PARALLEL region,
         // move this initialization to before the region containing
         // this vectorized loop.
         WN* enclosing_parallel_region = LWN_Get_Parent(innerloop);
         while(enclosing_parallel_region &&
                    WN_operator(enclosing_parallel_region) != OPR_REGION)
              enclosing_parallel_region = LWN_Get_Parent(enclosing_parallel_region);
         FmtAssert(enclosing_parallel_region, ("NYI"));
         if (WN_pragma(WN_first(WN_region_pragmas(
                   enclosing_parallel_region))) !=
                  WN_PRAGMA_PARALLEL_DO)
              LWN_Insert_Block_Before(LWN_Get_Parent(
                  enclosing_parallel_region),
                                        enclosing_parallel_region, tmp);
          else
                LWN_Insert_Block_Before(LWN_Get_Parent(innerloop),
                                        innerloop, tmp);
   }
   LWN_Parentize(tmp);
   last_tmp = tmp;

   WN_set_rtype(simd_op, vmtype);
   WN_set_rtype(last_op, vmtype);
   tmp = Split_Using_Preg(red_store, last_op, adg, FALSE);
   WN_store_offset(tmp) = offset;
   WN_load_offset(WN_kid0(red_store)) = offset;
   WN_set_rtype(simd_op, rtype);
   WN_set_rtype(last_op, rtype);
   // Move the new preg STID below this loop.
   LWN_Extract_From_Block(red_store);
   if (!Do_Loop_Is_Mp(innerloop))
        LWN_Insert_Block_After(LWN_Get_Parent(innerloop),
                                     innerloop, red_store);
  else{
        // Bug 4850 - For DO loops inside a PARALLEL region,
        // move this reduction to after the region containing
        // this vectorized loop.
        WN* enclosing_parallel_region = LWN_Get_Parent(innerloop);
        while(enclosing_parallel_region &&
                    WN_operator(enclosing_parallel_region) != OPR_REGION)
             enclosing_parallel_region = LWN_Get_Parent(enclosing_parallel_region);
        FmtAssert(enclosing_parallel_region, ("NYI"));

        // Bugs 5150, 4850
        WN *region_pragma =
                WN_first(WN_region_pragmas(enclosing_parallel_region));
        while(region_pragma &&
                    WN_pragma(region_pragma) != WN_PRAGMA_REDUCTION)
                region_pragma = WN_next(region_pragma);
        if (!region_pragma)
                LWN_Insert_Block_After(LWN_Get_Parent(
                  enclosing_parallel_region),
                                       enclosing_parallel_region, red_store);
        else
                LWN_Insert_Block_After(LWN_Get_Parent(innerloop),
                                       innerloop, red_store);
       }

  // Create a REDUCE operation on this STID
  OPERATOR opr;
  OPERATOR s_opr = WN_operator(simd_op);
  desc = WN_desc(red_store);
  if (MTYPE_is_unsigned(desc)) desc = MTYPE_complement(desc);//bug 2625
  switch(WN_operator(simd_op)) {
    case OPR_ADD: opr = OPR_REDUCE_ADD; break;
    case OPR_SUB: opr = OPR_REDUCE_ADD; s_opr = OPR_ADD; break;
    case OPR_MPY: opr = OPR_REDUCE_MPY; break;
    case OPR_DIV: opr = OPR_REDUCE_MPY; s_opr = OPR_MPY; break;
    case OPR_MAX: opr = OPR_REDUCE_MAX; break;
    case OPR_MIN: opr = OPR_REDUCE_MIN; break;
    default: FmtAssert(FALSE, ("NYI"));
  }
  if (MTYPE_is_integral(desc) && MTYPE_byte_size(desc) < 4)
      desc = MTYPE_I4;
  WN* reduce = LWN_CreateExp2(OPCODE_make_op(s_opr, WN_rtype(simd_op),WN_desc(simd_op)),
                             LWN_CreateExp1(OPCODE_make_op(opr, desc, vmtype), WN_kid0(red_store)),
                             copy);
 LWN_Parentize(reduce);
  LWN_Set_Parent(reduce, red_store);
  WN_kid0(red_store) = reduce;

  // Update the use-def
  Du_Mgr->Add_Def_Use(last_tmp, WN_kid0(WN_kid1((reduce))));
  Du_Mgr->Add_Def_Use(last_tmp, WN_kid(simd_op, kid));
  Du_Mgr->Add_Def_Use(tmp, WN_kid(simd_op, kid));
  Du_Mgr->Add_Def_Use(tmp, WN_kid0(WN_kid1(reduce)));
  def_list = Du_Mgr->Ud_Get_Def(WN_kid(simd_op, kid));
  def_list->Set_loop_stmt(innerloop);

  // Rename all other reduction statement involving this variable now.
  Rename_Other_Reductions(orig_offset, orig_st, offset,
                          WN_first(WN_do_body(innerloop)), tmp, vmtype);

 return red_store;
}

static WN *Simd_Vectorize_Induction_Variables(WN *operand, WN *simd_op, WN *innerloop,
                                               TYPE_ID vmtype, STACK_OF_WN *vec_simd_ops,
                                               SIMD_KIND *simd_op_kind, SIMD_KIND simd_kind)

{
  SYMBOL vec_index_symbol;
  SYMBOL vec_loop_incr_symbol;
  WN *vec_index_preg_store = NULL; //to return
  WN *vec_loop_incr_preg_store;
  WN *incr_vec_index_symbol;
  TYPE_ID prog_const_type;
  SYMBOL symbol(operand);

  BOOL const_lb = FALSE;
  INT const_val = 0;
  if(WN_operator(WN_kid0(WN_start(innerloop))) == OPR_INTCONST){
      const_lb = TRUE;
      const_val = WN_const_val(WN_kid0(WN_start(innerloop)));
  }

  INT vec_unit;
  BOOL shorter_type = FALSE;
  TYPE_ID scalar_type;

  if (WN_desc(simd_op) == MTYPE_V)
      scalar_type = WN_rtype(simd_op);
  else
      scalar_type = WN_desc(simd_op);
  switch(vmtype){
      case MTYPE_V16I1:
         prog_const_type = vmtype; break;
      case MTYPE_V16I2:
         prog_const_type = vmtype;
         if (MTYPE_byte_size(scalar_type) < 2)
                shorter_type = TRUE;
         break;
      case MTYPE_V16I4:
         prog_const_type = vmtype;
         if (MTYPE_byte_size(scalar_type) < 4)
                 shorter_type = TRUE;
         break;
      case MTYPE_V16I8:
         prog_const_type = vmtype;
         if (MTYPE_byte_size(scalar_type) < 8)
            shorter_type = TRUE;
        break;
       case MTYPE_V16F4:
         prog_const_type = MTYPE_V16I4;
         if (MTYPE_byte_size(scalar_type) < 4)
              shorter_type = TRUE;
         break;
       case MTYPE_V16F8:
         prog_const_type = MTYPE_V16I8;
         if (MTYPE_byte_size(scalar_type) < 8)
            shorter_type = TRUE;
         break;
       default: FmtAssert(FALSE, ("NYI"));
   }//end switch

   if (shorter_type) {
     switch(scalar_type) {
        case MTYPE_U1:
        case MTYPE_I1: prog_const_type = MTYPE_V16I1; break;
        case MTYPE_U2:
        case MTYPE_I2: prog_const_type = MTYPE_V16I2; break;
        case MTYPE_U4:
        case MTYPE_I4: prog_const_type = MTYPE_V16I4; break;
        case MTYPE_U8:
        case MTYPE_I8: prog_const_type = MTYPE_V16I8; break;
        case MTYPE_F4: prog_const_type = MTYPE_V16I4; break;
        default: FmtAssert(FALSE, ("NYI"));
      }
    }

   switch (simd_kind) {
     case V16I1: vec_unit = 16; break;
     case V16I2: vec_unit = 8; break;
     case V16I4: vec_unit = 4; break;
     case V16I8: vec_unit = 2; break;
     case V16C8: vec_unit = 1; break;
     default: FmtAssert(FALSE, ("NYI"));
   }
  TCON prog_const_tcon = Create_Simd_Prog_Const(prog_const_type,
                                                 const_val);
   ST* prog_const_symbol =
                New_Const_Sym (Enter_tcon(prog_const_tcon),
                              Be_Type_Tbl(prog_const_type));
   WN* prog_const =
                    WN_CreateConst (OPR_CONST, prog_const_type, MTYPE_V,
                                    prog_const_symbol);

    // Create the const (..., vec, vec, vec) in a vector preg
    TCON loop_incr_const_tcon = Host_To_Targ(MTYPE_I4, vec_unit);
    ST* loop_incr_const_symbol =
                    New_Const_Sym (Enter_tcon(loop_incr_const_tcon),
                                   Be_Type_Tbl(MTYPE_I4));
    WN* loop_incr_const =
                    WN_CreateConst (OPR_CONST, prog_const_type, MTYPE_V,
                                    loop_incr_const_symbol);
    vec_index_symbol=
                    Create_Preg_Symbol(symbol.Name(), prog_const_type);
    vec_loop_incr_symbol =
                    Create_Preg_Symbol(symbol.Name(), prog_const_type);

    if (const_lb)
       vec_index_preg_store =
                      AWN_StidIntoSym(&vec_index_symbol, prog_const);
    else {
          WN* lb = LWN_Copy_Tree(WN_kid0(WN_start(innerloop)));
          LWN_Copy_Def_Use(WN_kid0(WN_start(innerloop)), lb, Du_Mgr);
          WN* lb_replicate =
                      LWN_CreateExp1(OPCODE_make_op(OPR_REPLICATE,
                                                    prog_const_type,
          WN_desc(lb)==MTYPE_V?WN_rtype(lb):WN_desc(lb)), lb);
          vec_index_preg_store =
                      AWN_StidIntoSym(&vec_index_symbol,
                                      AWN_Add(prog_const_type, prog_const,
                                              lb_replicate));
    }
    LWN_Parentize(vec_index_preg_store);
    vec_loop_incr_preg_store =
                    AWN_StidIntoSym(&vec_loop_incr_symbol, loop_incr_const);
  WN* loop_enclosing_block = innerloop;
    while (WN_operator(loop_enclosing_block) != OPR_BLOCK)
          loop_enclosing_block =
                      LWN_Get_Parent(loop_enclosing_block);
    LWN_Insert_Block_Before(loop_enclosing_block, innerloop,
                                          vec_index_preg_store);
    WN_Set_Linenum ( vec_index_preg_store,
                                   WN_Get_Linenum(innerloop) );
    LWN_Insert_Block_Before(loop_enclosing_block, innerloop,
                                          vec_loop_incr_preg_store);
    WN_Set_Linenum ( vec_loop_incr_preg_store,
                                   WN_Get_Linenum(innerloop) );
    // Increment vec_index for next time around the loop.
    WN* use_vec_incr_loop = AWN_LdidSym(&vec_loop_incr_symbol);
    WN* use_vec_index_symbol = AWN_LdidSym(&vec_index_symbol);
    incr_vec_index_symbol =
                    AWN_StidIntoSym(&vec_index_symbol,
                                    AWN_Add(prog_const_type,
                                            use_vec_index_symbol,
                                            use_vec_incr_loop));
    LWN_Insert_Block_After(WN_do_body(innerloop),
                                         WN_last(WN_do_body(innerloop)),
                                         incr_vec_index_symbol);
    WN_Set_Linenum ( incr_vec_index_symbol,
                                   WN_Get_Linenum(innerloop) );
    Du_Mgr->Add_Def_Use(vec_loop_incr_preg_store,
                                      use_vec_incr_loop);
    Du_Mgr->Add_Def_Use(vec_index_preg_store,
                                      use_vec_index_symbol);
    Du_Mgr->Add_Def_Use(incr_vec_index_symbol,
                                      use_vec_index_symbol);

  for (INT i=0; i<vec_simd_ops->Elements(); i++){
     WN *cur_simd_op=vec_simd_ops->Top_nth(i);
      if (simd_op_kind[i] == INVALID ||
             Enclosing_Do_Loop(cur_simd_op) != innerloop) //only for current loop
         continue;
      for (INT kid = 0; kid < WN_kid_count(cur_simd_op); kid ++){
       WN *operand1 = WN_kid(cur_simd_op, kid);
        if (WN_operator(operand1)==OPR_LDID &&
               SYMBOL(operand1) == SYMBOL(WN_index(innerloop))){
            Delete_Def_Use(operand1);
            WN_st_idx(operand1)=ST_st_idx(vec_index_symbol.St());
            WN_offset(operand1)=vec_index_symbol.WN_Offset();
            WN_set_desc(operand1, prog_const_type);
            WN_set_rtype(operand1, prog_const_type);
            Du_Mgr->Add_Def_Use(vec_index_preg_store, operand1);
            Du_Mgr->Add_Def_Use(incr_vec_index_symbol, operand1);
        }
      }
   }
    return vec_index_preg_store;
}

static void Simd_Vectorize_Load_And_Equilvalent(WN *load, WN *innerloop, TYPE_ID vmtype)
{
   STACK<WN*>* equivalence_class= //find the load's equivalence class
                Scalar_Equivalence_Class(load, Du_Mgr,&LNO_local_pool);
   if (!equivalence_class){
       equivalence_class = CXX_NEW(STACK<WN*>(&LNO_local_pool), &LNO_local_pool);
       equivalence_class->Push(load); //just itself?
   }

   SYMBOL symbol(load);
   SYMBOL new_symbol=
              Create_Preg_Symbol(symbol.Name(), vmtype);
   for (INT i=0; i<equivalence_class->Elements(); i++) {
      WN* scalar_ref=equivalence_class->Top_nth(i);

      // Bug 3077 - Do not rename references outside the loop.
      // Temporaries that are live-out of this loop will be caught by
      // the scalar expansion test in the begining of this module.
      // These temporaries require scalar expansion (if this loop is
      // vectorized). So, we can safely rename all references inside
      // this loop.
      if (!Wn_Is_Inside(scalar_ref, innerloop))
          continue;

      WN_st_idx(scalar_ref)=ST_st_idx(new_symbol.St());
      WN_offset(scalar_ref)=new_symbol.WN_Offset();
      WN_set_desc(scalar_ref, vmtype);
      if (WN_operator(scalar_ref) != OPR_STID)
              WN_set_rtype(scalar_ref, vmtype);
    }
   CXX_DELETE(equivalence_class, &LNO_local_pool);
}

/*
   Vectorize an if-statement. 
   Vectorization of if-statement uses the following SSE4.1 operations:
   1. pcmpeqq: evaluates pairs of if-conditions.
   2. pblendvb: represents the vectorized if-body by selecting the value 
      to store depending on the result of the pcmpeqq.
   
   By the time a vectorizable if statement is passed to 
   Simd_Vectorize_If, its sub-expressions have already been vectorized.

  Original           Input to                 Output of 
                     Simd_Vectorize_If        Simd_Vectorize_If
  if                 if                             vexpr
      expr               vexpr                      v0
      0                  v0                       veq
    ne                 vne                        vkid0
  then        ===>   then             ===>        kid1
      kid0               vkid0                  vblend   
      kid1               kid1                   kid1
    istore             vistore                vistore
*/
static void Simd_Vectorize_If(WN *simd_op) {
  WN *if_test, *blend_mask, *then_body, *then_istore, *blend_source, 
    *blend_dest_array, *blend_dest, *blend_parent, *blend, *loop_block,
    *inner_if_test, *blend_mask_kid0, *blend_mask_kid1;
  BOOL nested = FALSE;
  BOOL special = FALSE;

  //step 1: verify that form of the if-statement is as expected.
  if_test = WN_if_test(simd_op); 
  then_body = WN_then(simd_op);
  FmtAssert(WN_operator(if_test) == OPR_NE, 
	    ("Unexpected condition in IF-expression\n"));
  FmtAssert(WN_rtype(if_test) == MTYPE_V16I8, 
	    ("Unexpected vector type of if-condition"));
  FmtAssert(WN_operator(then_body) == OPR_BLOCK, 
	    ("body of if must be a block\n "));
  then_istore = WN_first(then_body);

  //step 2: check for nested-if statement and if so access the istore in the inner body.
  if (WN_operator(then_istore) == OPR_IF) {
    nested = TRUE;
    inner_if_test = WN_if_test(then_istore);
    FmtAssert(WN_operator(WN_then(then_istore)) == OPR_BLOCK, 
	      ("body of if must be a block\n "));
    then_istore = WN_first(WN_then(then_istore));
    FmtAssert(WN_operator(inner_if_test) == OPR_NE, 
	      ("Unexpected condition in nested-IF-expression\n"));
  }
  FmtAssert(WN_operator(then_istore) == OPR_ISTORE, 
	    ("if-body must be an istore\n "));

  if (!nested) {
    //step 3: construct blend mask from the if-condition by
    //replacing OPR_NE with OPR_EQ (generates pcmpeqq).
    blend_mask = LWN_Copy_Tree(if_test);
    LWN_Copy_Def_Use(if_test, blend_mask, Du_Mgr);
    FmtAssert(WN_rtype(blend_mask) == MTYPE_V16I8, ("Unexpected rtype for vectorized NE"));
    WN_set_operator(blend_mask, OPR_EQ); 
    WN_desc(blend_mask) == MTYPE_V16I8;
  }
  else {
    // Nested-if:
    WN *testval1, *testval2, *testval2kid1, *testval1kid1, *shlkid1, *shlkid2, 
       *mask1, *mask2, *bitmask, *bitmask_copy;

    testval1 = WN_kid0(if_test); // represents vexpr1 
    testval2 = WN_kid0(inner_if_test); //represents vexpr2 

    /*
      Step 4: Check if nested-if conditions are of a special form (shown below in original form):
      if (x & (1 << c1))  // extract single bit
        if (x & (1 << c2))  // extract another single bit
          s1; 
    */
    if (WN_Equiv(testval1, testval2) && WN_operator(testval1) == OPR_BAND) {
      if (Tree_Equiv(WN_kid0(testval1), WN_kid0(testval2))) {
	testval1kid1 = WN_kid1(testval1);
	testval2kid1 = WN_kid1(testval2);
	// the sub-expression (1 << c1) is an invariant, so its vectorized form will have OPR_REPLICATE
	if (WN_Equiv(testval1kid1, testval2kid1) && WN_operator(testval1kid1) == OPR_REPLICATE) {
	  shlkid1 = WN_kid0(testval1kid1);
	  shlkid2 = WN_kid0(testval2kid1);
	  if (WN_Equiv(shlkid1, shlkid2) && WN_operator(shlkid1) == OPR_SHL) {
	    if (WN_Equiv(WN_kid0(shlkid1), WN_kid0(shlkid2))) {
	      if (WN_operator(WN_kid0(shlkid1)) == OPR_INTCONST && 
		  WN_const_val(WN_kid0(shlkid1)) == 1) {
		special = TRUE;
	      }
	    }
	  }
	}
      }
    }
    if (special) {
      /* Step 5: Construct blend mask for special if-condition by 
         converting it to the following equivalent (shown below in original form):
	 if ((x & (1<<c1 |1<<c2)) == (1<<c1 | 1<<c2)) // extract the two bits simultaneously
	 and then vectorize.
      */
      blend_mask = LWN_Copy_Tree(if_test);
      LWN_Copy_Def_Use(if_test, blend_mask, Du_Mgr);

      //bitmask = por(testval1kid1, testval2kid1);
      mask1 = LWN_Copy_Tree(testval1kid1);
      LWN_Copy_Def_Use(testval1kid1, mask1, Du_Mgr);
      mask2 = LWN_Copy_Tree(testval2kid1);
      LWN_Copy_Def_Use(testval2kid1, mask2, Du_Mgr);
      //bitmask = (1<<c1|1<<c2, 1<<c1|1<<c2)
      bitmask = WN_CreateExp2(OPR_BIOR, WN_rtype(mask1), MTYPE_V, 
			      mask1, mask2);

      // make extract = pand(WN_kid0(testval1, bitmask));
      // extract = ((x[i] & (1<<c1|1<<c2)), (x[i+1] & (1<<c1|1<<c2)))
      WN_kid1(WN_kid0(blend_mask)) = bitmask;
      // blend_mask = pcmpeqq(extract, bitmask);
      bitmask_copy = LWN_Copy_Tree(bitmask);
      LWN_Copy_Def_Use(bitmask, bitmask_copy, Du_Mgr);
      WN_kid1(blend_mask) = bitmask_copy;
      WN_set_operator(blend_mask, OPR_EQ); 
    }
    else {
      //Step 6: Construct blend mask for non-special nested if-conditions by
      //fusing the two condtions with a por.
      /*
                          Input to               Output of 
        Original          Simd_Vectorize_If      Simd_Vectorize_If
        if                if            
            expr1             vexpr1                     vexpr1
            0                 v0                         v0  
          ne                vne                        veq 
        then         ==>  then             ==>           vexpr2
          if                if                           v0
              expr2             vexpr2                 veq 
              0                 v0                   vor
            ne                vne                    vkid0
          then              then                     kid1
              kid0              vkid0              vblend
              kid1              kid1               kid1
            istore            vistore            vistore
      */

      blend_mask_kid0 = LWN_Copy_Tree(if_test);
      LWN_Copy_Def_Use(if_test, blend_mask_kid0, Du_Mgr);
      WN_set_operator(blend_mask_kid0, OPR_EQ); 
      
      blend_mask_kid1 = LWN_Copy_Tree(inner_if_test);
      LWN_Copy_Def_Use(inner_if_test, blend_mask_kid1, Du_Mgr);
      WN_set_operator(blend_mask_kid1, OPR_EQ); 

      blend_mask = WN_CreateExp2(OPR_BIOR, WN_rtype(blend_mask_kid1), MTYPE_V, 
				 blend_mask_kid0, blend_mask_kid1);
    }
  }

  //Step 7: Construct kid1 of blend from LHS of the istore statement.
  blend_source = WN_kid0(then_istore); 

  //Step 8: Construct kid2 of blend from RHS of the istore statement.
  blend_dest_array = LWN_Copy_Tree(WN_kid1(then_istore)); 
  LWN_Copy_Def_Use(WN_kid1(then_istore), blend_dest_array, Du_Mgr); 
  blend_dest = WN_Iload(WN_desc(then_istore), WN_offset(then_istore), 
			Make_Pointer_Type(MTYPE_To_TY(WN_desc(then_istore))), 
			blend_dest_array, WN_field_id(then_istore));  

  blend_parent = then_istore;
  if(special) {
    blend = WN_CreateExp3(OPR_SELECT, MTYPE_V16I1, MTYPE_V16I1,
			  blend_dest, blend_source, blend_mask);   
  }
  else {
    blend = WN_CreateExp3(OPR_SELECT, MTYPE_V16I1, MTYPE_V16I1,
			  blend_source, blend_dest, blend_mask);   
  }

  //Step 9: Store result of blend is stored in the LHS of the istore
  WN_kid0(then_istore) = blend;
  LWN_Set_Parent(blend, blend_parent);
  LWN_Parentize(blend);

  loop_block = LWN_Get_Parent(simd_op);
  LWN_Set_Parent(blend_parent, loop_block);

  //Step 10: Replace the if-statement with the istore expression containing the blend-tree
  if (simd_op == WN_first(loop_block))
    WN_first(loop_block) = blend_parent;
  else {
    WN_next(WN_prev(simd_op)) = blend_parent;
    WN_prev(blend_parent) = WN_prev(simd_op);
  }
  if (simd_op == WN_last(loop_block)) {
    WN_last(loop_block) = blend_parent;
    WN_next(blend_parent) = NULL;
  }
  else {
    WN_next(blend_parent) = WN_next(simd_op);
    WN_prev(WN_next(simd_op)) = blend_parent;
  }
  return;
}

static void Simd_Vectorize_SimdOp_And_Kids(WN *simd_op, TYPE_ID vmtype, BOOL *invarkid)
{

  if (WN_operator(simd_op) != OPR_CVT && WN_operator(simd_op) != OPR_TRUNC){
    for(INT kid=0; kid<WN_kid_count(simd_op); kid++){
      if(invarkid[kid]) continue; //invariant and ldid already done
      WN *operand = WN_kid(simd_op, kid);
      if (WN_operator(simd_op) == OPR_INTRINSIC_OP) {
        FmtAssert(WN_operator(operand) == OPR_PARM, ("NYI"));
        operand = WN_kid0(operand);
      }
      if(WN_operator(operand) == OPR_SHUFFLE) //shuffle done, go to the iload
        operand = WN_kid0(operand);

      if(WN_desc(operand) != MTYPE_V && !MTYPE_is_vector(WN_desc(operand)))
         WN_set_desc(operand, vmtype);
      if(WN_rtype(operand) != MTYPE_V && !MTYPE_is_vector(WN_rtype(operand)))
        WN_set_rtype(operand, vmtype);
    }//end kids

    if(OPCODE_is_compare(WN_opcode(simd_op))){
      if (vmtype == MTYPE_V16F4)
        WN_set_rtype (simd_op, MTYPE_V16I4);
      else WN_set_rtype (simd_op, MTYPE_V16I8);
      WN_set_desc(simd_op, vmtype);
    }else if (!MTYPE_is_vector(WN_rtype(simd_op)))
        WN_set_rtype (simd_op, vmtype);
  }else{//CVT
    TYPE_ID vec_rtype, vec_desc;
    switch(WN_desc(simd_op)) {
      case MTYPE_I4: vec_desc = MTYPE_V16I4; break;
      case MTYPE_F4: vec_desc = MTYPE_V16F4; break;
     default: FmtAssert(FALSE, ("NYI"));
    }
    switch(WN_rtype(simd_op)) {
      case MTYPE_F8:
        vec_rtype = MTYPE_V16F8;
        if (vec_desc == MTYPE_V16I4) // bug 7334
           vec_desc = MTYPE_V8I4;
        else vec_desc = MTYPE_V8F4;
        break;
      case MTYPE_F4: vec_rtype = MTYPE_V16F4; break;
      case MTYPE_I4: vec_rtype = MTYPE_V16I4; break;
      default: FmtAssert(FALSE, ("NYI"));
     }
     WN_set_rtype(simd_op, vec_rtype);
     WN_set_desc(simd_op, vec_desc);
     if(!invarkid[0]){
         WN *operand0 = WN_kid0(simd_op);
         if(WN_operator(operand0)==OPR_SHUFFLE)
           operand0 = WN_kid0(operand0); //shuffle, down a level
         if (!MTYPE_is_vector(WN_rtype(operand0)))
            WN_set_rtype(operand0, vec_desc);
         if(!MTYPE_is_vector(WN_desc(operand0)) && WN_desc(operand0) != MTYPE_V)
            WN_set_desc(operand0, vec_desc);
    }
  }//end CVT

  WN *istore = LWN_Get_Parent(simd_op);
  if(WN_operator(istore) == OPR_SHUFFLE)
     istore =  LWN_Get_Parent(istore); //up one level

  if(WN_operator(istore) == OPR_IF) {
    if (WN_operator(LWN_Get_Parent(istore)) == OPR_BLOCK &&
	WN_operator(LWN_Get_Parent(LWN_Get_Parent(istore))) == OPR_IF) {
      // This is the inner if of a nested-if. Do nothing here as 
      // Simd_Vectorize_SimdOp_And_Kids on the parent will vectorize this inner if.
      // LWN_Get_Parent(LWN_Get_Parent(istore)) is the parent if.
    }
    else
      Simd_Vectorize_If(istore);
  }

  if (WN_operator(istore) != OPR_STID && WN_operator(istore) != OPR_CVT &&
        WN_operator(istore) != OPR_TRUNC &&
        !OPCODE_is_compare(WN_opcode(istore))) {
      if (WN_desc(istore) != MTYPE_V)
        WN_set_desc(istore, vmtype);
      if (WN_rtype(istore) != MTYPE_V)
        WN_set_rtype(istore, vmtype);
    }

  //vectorize intrinsic - reset intrinsic kind
    if (WN_operator(simd_op) == OPR_INTRINSIC_OP)
      Simd_Vectorize_Intrinsics(simd_op);
}

// This is a kludge to SIMD code generation. The purpose is to simplify the LB 
// and UB of the vectorized loop and the remainder loop.  It is better 
// to directly generate simple LB/Ub in Simd_Finalize_Loops(). Unfornately, 
// this function was pooly written. It is lot easier simplify the LB/UB by 
// post-processing them.
// 
//  Suppose the loop being vectorized is:
//   for (int i = 0; i < n; i++) {...}
//
//  Before calling this function, the vectorized loop and the remainder loop were: 
//   vectorized loop: for (i = 0; i <= n/f*f - 1; n+=f)
//   remainder loop:  for (i = n/f*f; i < n; i++)
//   where the <f> is unrolling factor (i.e. the vector length)
//
//  They are following after calling this func:
//   vectorized loop: for (i = 0; i <= n-f; i+=f)
//   remainder loop:  for (i'= i; i' < n; i'++)
//
static BOOL Simd_Simplify_LB_UB (WN* vect_loop, WN* remainder, INT vect)
{
  if (Do_Loop_Is_Unsigned (vect_loop))
    return FALSE;

  // This condition is not expected to be true with current implementation since
  // loop being vectorized is 'standardized' at very early stage. But we perform
  // this safety check just in case things change in the future.
  if (remainder && Index_Variable_Live_At_Exit(remainder))
    return FALSE;

  WN* loop_idx = WN_index(vect_loop);  
  WN* loop_end = WN_end (vect_loop);
  WN* loop_start = WN_start (vect_loop);

  // step 1.a: check to see if :
  //  - loop_end is in the form of "idx < UB" or "idx <= UB", and 
  //  - loop index's initial value is 0.
  //
  if (!WN_has_sym (WN_kid0(loop_end)) || 
      SYMBOL(WN_kid0(loop_end)) != SYMBOL (loop_idx) ||
      (WN_operator (loop_end) != OPR_LT && WN_operator(loop_end) != OPR_LE) ||
      (WN_operator (WN_kid0(loop_start)) != OPR_INTCONST || 
       WN_const_val (WN_kid0(loop_start)) != 0)) {
    return FALSE;    
  }

  // if the UB is already a constant, no room for improvement
  //
  if (WN_operator (WN_kid1(loop_end)) == OPR_INTCONST) {
    return FALSE;
  }

  // step 1.b: check to see if the UB matches the pattern "<= tc/vect * vect - 1"
  // or "tc/vect * vect"
  WN* TC = NULL;
  WN* UB = WN_kid1(loop_end);
  if (WN_operator (loop_end) == OPR_LE) {
    BOOL match = FALSE;
    if (WN_operator (UB) == OPR_SUB && 
        WN_operator (WN_kid1(UB)) == OPR_INTCONST &&
        WN_const_val (WN_kid1(UB)) == 1) {
      match = TRUE;
    } else if (WN_operator (UB) == OPR_ADD && 
               WN_operator (WN_kid1(UB)) == OPR_INTCONST && 
               WN_const_val (WN_kid1(UB)) == -1) {
      match = TRUE;
    }
    if (!match) 
      return FALSE;
    TC = WN_kid0 (UB);
  } else if (WN_operator (loop_end) == OPR_LT) {
    TC = UB;
  } else {
    // the operator is supposed to be either < or <=
    return FALSE;
  }

  if (WN_operator (TC) != OPR_MPY || 
      WN_operator (WN_kid1(TC)) != OPR_INTCONST ||
      WN_const_val (WN_kid1(TC)) != vect) {
    return FALSE;
  } else {
    TC = WN_kid0(TC);
  }

  if (WN_operator (TC) != OPR_DIV || 
      WN_operator (WN_kid1(TC)) != OPR_INTCONST ||
      WN_const_val (WN_kid1(TC)) != vect) {
    return FALSE;
  } else {
    TC = WN_kid0(TC);
  }

  // Delete  the whole WHIRL tree of WN_kid1(WN_end(loop)) except the 
  // subtree rooted at <TC> as <TC> will be used in the following steps.
  {
    WN* dummy = WN_CreateIntconst (OPC_I4INTCONST, 0);
    WN* parent = LWN_Get_Parent (TC);
    BOOL replaced = FALSE;
    for (INT i = 0; i < WN_kid_count (parent); i++) {
      if (WN_kid(parent, i) == TC) {
        WN_kid(parent, i) = dummy;
        replaced = TRUE;
        break;
      }
    }
    Is_True (replaced, ("parentization must be wrong"));
    LWN_Delete_Tree (WN_kid1(loop_end));
  }
  
  // step 2: replace the UB (of the vectorized loop) to "i <= TC - vect"
  //    or "i < TC - vect + 1".
  //
  //   More intuitively, if the original loop (before vectorization) is:
  //      for (i = 0; i <= n-1; i++) {...}
  //
  //   Suppose unrolling factor is 4; this loop will be transformed into 
  //   following by Simd_Finalize_Loops():
  //      for (i = 0; i <= n/4 * 4 -1 ; i++) {...}
  //  
  //   This function will adjust the upper bounder into:
  //      for (i = 0; i <= n - 4 ; i++) {...}
  //
  {
    OPCODE sub_opc= OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
    OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST, index_type, MTYPE_V);
    INT64 const_val = (WN_operator (loop_end) == OPR_LE) ? vect : vect - 1;
    WN* new_UB = LWN_CreateExp2 (sub_opc, TC,
			         WN_CreateIntconst (intconst_opc, const_val));
    WN_kid1(loop_end) = new_UB;
    LWN_Parentize (loop_end);
  }
  
  if (!remainder)
    return TRUE;

  if (Do_Loop_Is_Mp (vect_loop)) {
    // Keep the loop index private if it is flagged Do_Loop_Is_Mp().
    // 
    return TRUE;
  }

  // Step 3: replace the init statement of the remainder loop from 
  //  "loop_idx = complicate_expr" to identity "loop_idx = loop_idx".
  //
  {
    WN* start_r = WN_start (remainder);
    WN* start = WN_start (vect_loop);
    Is_True (WN_operator (start_r) == OPR_STID && 
             WN_operator(start) == OPR_STID, 
             ("criteria isn't met"));

    OPCODE ld_opc = OPCODE_make_op (OPR_LDID, Promote_Type(WN_desc(start)), WN_desc(start));
    WN* ld_idx = LWN_CreateLdid (ld_opc, WN_start(vect_loop));

    Delete_Def_Use (WN_kid0(start_r));
    LWN_Delete_Tree (WN_kid0(start_r));
    
    WN_kid0(start_r) = ld_idx;
    Du_Mgr->Add_Def_Use (WN_start (vect_loop), ld_idx);
    Du_Mgr->Add_Def_Use (WN_step (vect_loop), ld_idx);
    LWN_Parentize (start_r);

  }

  // step 4: rename the loop index of the remainder loop
  scalar_rename (WN_start (remainder));

  return TRUE;  
}

// Simd_Remove_Unity_Remainder() is to remove the remainder loop if the number
// of its iteration is one or at most one.
//
// In the case when the iteration number is provably one, the remainder loop
// construct is removed; otherwise, we guard the remainder loop with 
// a condition and remove the loop construct.
//
// return TRUE iff loop construct is removed.
//
static BOOL Simd_Remove_Unity_Remainder 
    (WN* remainder, BOOL provable_unity, BOOL one_iter_at_most) {
    
    if (!LNO_Simd_Rm_Unity_Remainder) 
        return FALSE;

    // case 1: the remainder loop may have more than one iterations
    //
    if (!provable_unity && !one_iter_at_most) {
        return FALSE;
    }

    INT64 src_pos = WN_Get_Linenum (remainder);

    // case 2: the remainder loop is provable unity.
    //
    if (provable_unity) {
      WN* wn_first_dumy, *wn_last_dumy;
      Remove_Unity_Trip_Loop (remainder, FALSE, &wn_first_dumy, 
                              &wn_last_dumy, adg, Du_Mgr, FALSE);
      if (debug || LNO_Simd_Verbose) {
        printf ("SIMD: (%s:%d) remove unity remainder loop\n", 
                 Src_File_Name, Srcpos_To_Line(src_pos));
      }
      return TRUE;
    }

    // case 3: The remainder loop ain't provable. But, compiler know it 
    // has at most one iteration.
    //
    // Suppose the remander loop is "for (i = LB; i <= UB; i++) {}". 
    // We guard the loop with "if (LB <= UB)" resulting the as following:
    //     if (LB <= UB) {
    //        for (....) {}    
    //     }
    // Then Remove_Unity_Trip_Loop() is called to get rid of the loop 
    // construct. 
    //
    
    // step 1: construct the test-condition
    // 
    WN* test = WN_end(remainder);
    if (SYMBOL (WN_kid0(test)) != SYMBOL(WN_index (remainder))) {
        // Oops, the condition isn't in the form of "idx < UB"
        return FALSE;
    }

    WN* cond = LWN_Copy_Tree (test);
    LWN_Copy_Def_Use (test, cond, Du_Mgr);
    LWN_Copy_Frequency_Tree (cond, test);

    // change "idx < UB" to "LB < UB". 
    //
    Replace_Ldid_With_Exp_Copy (WN_index (remainder), cond, 
                                WN_kid0(WN_start (remainder)), Du_Mgr);
                                
    // step 2: create empty else-clause
    //
    WN* else_clause = WN_CreateBlock ();

    // step 3: create then-clause, and put the remainder in the then clause
    //
    WN* then_clause = WN_CreateBlock ();
    WN* insert_after = WN_prev (remainder);
    WN* insert_block = LWN_Get_Parent(remainder);
    FmtAssert((WN_operator(insert_block) == OPR_BLOCK), 
      ("weird tree encountered"));
    LWN_Extract_From_Block (remainder);
    LWN_Insert_Block_Before (then_clause, NULL, remainder);

    // step 4: create the if-construct
    //
    WN* if_stmt = LWN_CreateIf (cond, then_clause, else_clause);
    LWN_Insert_Block_After (insert_block, insert_after, if_stmt);

    // step 5: create IF_INFO for the if-construct
    //
    {
        IF_INFO *ii = CXX_NEW (IF_INFO(&LNO_default_pool, 
                               TRUE, // contain DO loop
                               Find_SCF_Inside (remainder, OPC_REGION) != NULL), 
                               &LNO_default_pool);
        WN_MAP_Set (LNO_Info_Map, if_stmt, (void *)ii);

        DOLOOP_STACK *stack;
        stack = CXX_NEW (DOLOOP_STACK (&LNO_local_pool), &LNO_local_pool);
        Build_Doloop_Stack(if_stmt, stack);
        LNO_Build_If_Access(if_stmt, stack);
        CXX_DELETE(stack, &LNO_local_pool);
    }

    // step 6: remove the unity loop
    //
    {
        WN* dummy_first, *dummy_last;
        Remove_Unity_Trip_Loop (remainder, FALSE, &dummy_first, &dummy_last, 
                                adg, Du_Mgr, FALSE);
    }

    if (debug || LNO_Simd_Verbose) {
        printf ("SIMD: (%s:%d) remainder loop is changed to if-stmt\n", 
                 Src_File_Name, Srcpos_To_Line(src_pos));
    }

    return TRUE;
}

static void Simd_Finalize_Loops(WN *innerloop, WN *remainderloop, INT vect, WN *reduction_node)
{

    SYMBOL symbol(WN_index(innerloop));
    OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);
    OPCODE add_opc= OPCODE_make_op(OPR_ADD,index_type, MTYPE_V);
    OPCODE sub_opc= OPCODE_make_op(OPR_SUB,Mtype_TransferSign(MTYPE_I4, index_type), MTYPE_V);
    OPCODE div_opc = OPCODE_make_op(OPR_DIV,index_type, MTYPE_V);
    OPCODE cmp_opc = WN_opcode(WN_end(innerloop));
    OPERATOR opr = OPCODE_operator(cmp_opc);
    OPCODE new_cmp_opc = OPCODE_make_op(OPR_LE,
                                        WN_rtype(WN_end(innerloop)),
                                        WN_desc(WN_end(innerloop)));
    FmtAssert((opr == OPR_LE || opr == OPR_LT ||
               opr == OPR_GE || opr == OPR_GT), ("NYI"));
    if (opr == OPR_GE || opr == OPR_GT) {
      opr = (opr == OPR_GE) ? OPR_LE: OPR_LT;
      // Bug 4566 - Do not let the simplifier change the LE back to GE because
      // we rely on the order (loop_variable <= upper_bound).
      BOOL save_simp_state = WN_Simplifier_Enable(FALSE);
      WN_end(innerloop) =
        LWN_CreateExp2(new_cmp_opc,
                       WN_kid1(WN_end(innerloop)),
                       WN_kid0(WN_end(innerloop)));
      WN_Simplifier_Enable(save_simp_state);
    }
    
    WN *step;
    WN *add = WN_kid0(WN_step(innerloop));
    WN *loop_end = WN_end(innerloop);
    WN *loop_end_tmp, *loop_end_tmp_rloop;
    OPCODE mpy_opc = OPCODE_make_op(OPR_MPY,index_type, MTYPE_V);
    WN *loop_start = LWN_Copy_Tree(WN_start(innerloop), TRUE, LNO_Info_Map);
    WN *loop_start_tmp =
      LWN_Copy_Tree(WN_start(innerloop), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(WN_kid0(WN_start(innerloop)),
                     WN_kid0(loop_start), Du_Mgr);
    LWN_Copy_Def_Use(WN_kid0(WN_start(innerloop)),
                     WN_kid0(loop_start_tmp), Du_Mgr);
    WN *loop_start_rloop =
      LWN_Copy_Tree(WN_start(innerloop), TRUE, LNO_Info_Map);
    WN *loop_start_rloop_tmp =
      LWN_Copy_Tree(WN_start(innerloop), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(WN_kid0(WN_start(innerloop)),
                     WN_kid0(loop_start_rloop), Du_Mgr);
    LWN_Copy_Def_Use(WN_kid0(WN_start(innerloop)),
                     WN_kid0(loop_start_rloop_tmp), Du_Mgr);

    // Adjust loop upper bound for vectorized loop
    WN *loop_index = find_loop_var_in_simple_ub(innerloop);
    WN *tmp = LWN_Get_Parent(loop_index);
    if (tmp == loop_end)
      tmp = loop_index;
    else{
      while (LWN_Get_Parent(tmp)!=loop_end)
        tmp=LWN_Get_Parent(tmp);
    }
    BOOL rloop_needed = TRUE; // Is the remainder loop needed?
    if (WN_kid0(loop_end)==tmp) {
      if (opr == OPR_LT) {
        // Adjust loop upper bound so we always have a [lb,ub]
        // rather than a [lb,ub) bound
        WN_kid1(loop_end) =
          LWN_CreateExp2(add_opc,
            WN_kid1(loop_end),
            WN_CreateIntconst(intconst_opc, -1));
        //Bug 10707: should not create a new expression for WN_end(innerloop)
        //here, otherwise (1) loop_end and WN_end(innerloop) point to different
        //things and thus causes WN_end(innerloop) not updated; (2) we need
        //to turn off simplifier around here; (3) the original comparison node
        //may not be released until the end of BE even though not used.
        WN_set_opcode(loop_end,new_cmp_opc);
      }
      loop_end_tmp = LWN_Copy_Tree(WN_end(innerloop), TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use(WN_kid1(WN_end(innerloop)),
                       WN_kid1(loop_end_tmp), Du_Mgr);
      loop_end_tmp_rloop =
        LWN_Copy_Tree(WN_end(innerloop), TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use(WN_kid1(WN_end(innerloop)),
                       WN_kid1(loop_end_tmp_rloop), Du_Mgr);
      // Delete last loop end def-use (we are going to modify)
      LWN_Update_Def_Use_Delete_Tree(loop_end, Du_Mgr);
      if (WN_operator(WN_kid1(loop_end)) == OPR_INTCONST &&
          WN_operator(WN_kid0(loop_start)) == OPR_INTCONST) {
        // check if the trip count is exactly divisible by vect
        if ((WN_const_val(WN_kid1(loop_end)) -
             WN_const_val(WN_kid0(loop_start)) + 1)%vect == 0)
          rloop_needed = FALSE;
      }
      else if (WN_operator(WN_kid1(loop_end)) != OPR_INTCONST &&
               WN_operator(WN_kid0(loop_start)) != OPR_INTCONST) {
        WN* tmp1 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid1(loop_end)));
        WN* tmp2 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid0(loop_start)));
        if (WN_operator(tmp1) == OPR_INTCONST &&
            WN_operator(tmp2) == OPR_INTCONST) {
          if ((WN_const_val(tmp1) - WN_const_val(tmp2) + 1)%vect == 0)
            rloop_needed = FALSE;
        }
      }
      else if (WN_operator(WN_kid1(loop_end)) != OPR_INTCONST &&
               WN_operator(WN_kid0(loop_start)) == OPR_INTCONST) {
        WN* tmp1 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid1(loop_end)));
        if (WN_operator(tmp1) == OPR_INTCONST) {
          if ((WN_const_val(tmp1) - WN_const_val(WN_kid0(loop_start)) + 1)%vect == 0)
            rloop_needed = FALSE;
        }
      }
      else if (WN_operator(WN_kid1(loop_end)) == OPR_INTCONST &&
               WN_operator(WN_kid0(loop_start)) != OPR_INTCONST) {
        WN* tmp2 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid0(loop_start)));
        if (WN_operator(tmp2) == OPR_INTCONST) {
          if ((WN_const_val(WN_kid1(loop_end)) -
               WN_const_val(tmp2) + 1)%vect == 0)
            rloop_needed = FALSE;
        }
      }

      INT64 add_value = -1;
      if(MTYPE_is_unsigned(WN_desc(loop_end))) {
        // loop end is unsigned comparison.
        // i <= start + (((end - start + 1) / vect) * vect) -1
        // suppose start is 0, end is 2, vect is 4, original loop is
        // for( i = 0; i < 2; i++)
        // after this transformation 
        // for(i = 0, i <= -1 i++), if comparion is unsigned then
        // this loop is incorrect.
        // the fix is change loop end to 
        // i < start + (((end - start + 1) / vect) * vect)
        // 
        // start <= end guard should already insterted in STD_Canonicalize_Upper_Bound
        add_value = 0;
      }
      
      WN_kid1(loop_end) =            //old loop end 
        LWN_CreateExp2(add_opc,
          LWN_CreateExp2(add_opc,
            LWN_CreateExp2(mpy_opc,
              LWN_CreateExp2(div_opc,
                LWN_CreateExp2(add_opc,
                  LWN_CreateExp2(sub_opc,
                               WN_kid1(loop_end_tmp),
                               WN_kid0(loop_start)),
                  WN_CreateIntconst(intconst_opc, 1)),
                WN_CreateIntconst(intconst_opc, vect)),
              WN_CreateIntconst(intconst_opc, vect)),
            WN_CreateIntconst(intconst_opc, add_value)),
          WN_kid0(loop_start_tmp));
      
      if(MTYPE_is_unsigned(WN_desc(loop_end))) {
        OPCODE lt_cmp_opc = OPCODE_make_op(OPR_LT,
                                        WN_rtype(loop_end), WN_desc(loop_end));
        WN_set_opcode(loop_end,lt_cmp_opc);
      }

      // Adjust loop lower bound for serial remainder loop (if any)
      WN *start = WN_start(remainderloop);
      WN_kid0(start) =
        LWN_CreateExp2(add_opc,
          LWN_CreateExp2(mpy_opc,
            LWN_CreateExp2(div_opc,
              LWN_CreateExp2(add_opc,
                LWN_CreateExp2(sub_opc,
                               WN_kid1(loop_end_tmp_rloop),
                               WN_kid0(loop_start_rloop)),
                WN_CreateIntconst(intconst_opc, 1)),
              WN_CreateIntconst(intconst_opc, vect)),
            WN_CreateIntconst(intconst_opc, vect)),
          WN_kid0(loop_start_rloop_tmp));
    }
    else {
      if (opr == OPR_LT) {
        // Adjust loop upper bound so we always have a [lb,ub]
        // rather than a [lb,ub) bound
        WN_kid0(loop_end) =
          LWN_CreateExp2(add_opc,
            WN_kid0(loop_end),
            WN_CreateIntconst(intconst_opc, -1));
        //Bug 10707: should not create a new expression for WN_end(innerloop)
        //here, otherwise (1) loop_end and WN_end(innerloop) point to different
        //things and thus causes WN_end(innerloop) not updated; (2) we need
        //to turn off simplifier around here; (3) the original comparison node
        //may not be released until the end of BE even though not used.
        WN_set_opcode(loop_end,new_cmp_opc);
      }
      loop_end_tmp = LWN_Copy_Tree(WN_end(innerloop), TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use(WN_kid0(WN_end(innerloop)),
                       WN_kid0(loop_end_tmp), Du_Mgr);
      loop_end_tmp_rloop =
        LWN_Copy_Tree(WN_end(innerloop), TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use(WN_kid0(WN_end(innerloop)),
                       WN_kid0(loop_end_tmp_rloop), Du_Mgr);
      // Delete last loop end def-use (we are going to modify)
      LWN_Update_Def_Use_Delete_Tree(loop_end, Du_Mgr);
      if (WN_operator(WN_kid0(loop_end)) == OPR_INTCONST &&
          WN_operator(WN_kid0(loop_start)) == OPR_INTCONST) {
        // check if the trip count is exactly divisible by vect
        if ((WN_const_val(WN_kid0(loop_end)) -
             WN_const_val(WN_kid0(loop_start)) + 1)%vect == 0)
          rloop_needed = FALSE;
      }
      else if (WN_operator(WN_kid0(loop_end)) != OPR_INTCONST &&
               WN_operator(WN_kid0(loop_start)) != OPR_INTCONST) {
        WN* tmp1 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid0(loop_end)));
        WN* tmp2 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid0(loop_start)));
        if (WN_operator(tmp1) == OPR_INTCONST &&
            WN_operator(tmp2) == OPR_INTCONST) {
          if ((WN_const_val(tmp1) - WN_const_val(tmp2) + 1)%vect == 0)
            rloop_needed = FALSE;
        }
      }
      else if (WN_operator(WN_kid0(loop_end)) != OPR_INTCONST &&
               WN_operator(WN_kid0(loop_start)) == OPR_INTCONST) {
        WN* tmp1 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid0(loop_end)));
        if (WN_operator(tmp1) == OPR_INTCONST) {
          if ((WN_const_val(tmp1) - WN_const_val(WN_kid0(loop_start)) + 1)%vect == 0)
            rloop_needed = FALSE;
        }
      }
      else if (WN_operator(WN_kid0(loop_end)) == OPR_INTCONST &&
               WN_operator(WN_kid0(loop_start)) != OPR_INTCONST) {
        WN* tmp2 = WN_Simplify_Tree(LWN_Copy_Tree(WN_kid0(loop_start)));
        if (WN_operator(tmp2) == OPR_INTCONST) {
          if ((WN_const_val(WN_kid0(loop_end)) -
               WN_const_val(tmp2) + 1)%vect == 0)
            rloop_needed = FALSE;
        }
      }

      INT64 add_value = -1;
      if(MTYPE_is_unsigned(WN_desc(loop_end))) {
        add_value = 0;
      }
      WN_kid1(loop_end) =
        LWN_CreateExp2(add_opc,
          LWN_CreateExp2(add_opc,
            LWN_CreateExp2(mpy_opc,
              LWN_CreateExp2(div_opc,
                LWN_CreateExp2(add_opc,
                  LWN_CreateExp2(sub_opc,
                                 WN_kid0(loop_end_tmp),
                                 WN_kid0(loop_start)),
                  WN_CreateIntconst(intconst_opc, 1)),
                WN_CreateIntconst(intconst_opc, vect)),
              WN_CreateIntconst(intconst_opc, vect)),
            WN_CreateIntconst(intconst_opc, add_value)),
          WN_kid0(loop_start_tmp));
      if(MTYPE_is_unsigned(WN_desc(loop_end))) {
        OPCODE lt_cmp_opc = OPCODE_make_op(OPR_LT,
                                        WN_rtype(loop_end), WN_desc(loop_end));
        WN_set_opcode(loop_end,lt_cmp_opc);
      }
      
      // Adjust loop lower bound for serial remainder loop (if any)
      WN *start = WN_start(remainderloop);
      WN_kid0(start) =
        LWN_CreateExp2(add_opc,
          LWN_CreateExp2(mpy_opc,
            LWN_CreateExp2(div_opc,
              LWN_CreateExp2(add_opc,
                LWN_CreateExp2(sub_opc,
                               WN_kid0(loop_end_tmp_rloop),
                               WN_kid0(loop_start_rloop)),
                WN_CreateIntconst(intconst_opc, 1)),
              WN_CreateIntconst(intconst_opc, vect)),
            WN_CreateIntconst(intconst_opc, vect)),
          WN_kid0(loop_start_rloop_tmp));
    }

    BOOL remainder_is_unity = FALSE;

    // Bug 2516 - eliminate redundant remainder loop if it is possible to
    // simplify the symbolic (non-constant) expression (loop_end - loop_start).
    // This loop should be eliminated later on but there is no point in
    // creating a redundant loop if we can prove that the remainder loop is
    // not needed. After this change, the bug is either hidden or we have
    // eliminated grounds for error (probably some later phase is confused by
    // the loop bounds).
    {
      WN* rloop_start = LWN_Copy_Tree(WN_start(remainderloop));
      WN* rloop_end = LWN_Copy_Tree(WN_end(remainderloop));
      WN* diff = LWN_CreateExp2(sub_opc,
                                WN_kid1(rloop_end),
                                WN_kid0(rloop_start));
      WN* simpdiff = WN_Simplify_Tree(diff);
      if (WN_operator(simpdiff) == OPR_INTCONST) {
          if (WN_const_val(simpdiff) < 0)
            rloop_needed = FALSE;
         
          if (WN_const_val(simpdiff) == 0)
            remainder_is_unity = TRUE;
      }
    }

    // Update def use for new loop end for innerloop
    Simd_Update_Index_Def_Use(innerloop,innerloop,WN_end(innerloop), symbol);
    LWN_Set_Parent(WN_end(innerloop),innerloop);
    LWN_Parentize(WN_end(innerloop));

    // Adjust loop stride for vectorized loop
    if  (WN_operator(WN_kid0(add)) == OPR_INTCONST)
      step = WN_kid0(add);
    else if  (WN_operator(WN_kid1(add)) == OPR_INTCONST)
      step = WN_kid1(add);
    else
      FmtAssert(FALSE, ("Handle this"));
    if (WN_const_val(step)!= 1)
      FmtAssert(FALSE, ("Handle this"));
    WN_const_val(step) = vect;
    Simd_Update_Index_Def_Use(innerloop, innerloop,WN_step(innerloop), symbol);

    WN *start = WN_start(remainderloop);
    LWN_Set_Parent(WN_kid0(start),start);
    LWN_Set_Parent(start,remainderloop);
    LWN_Parentize(start);
    LWN_Parentize(innerloop);
    //Set loop info for remainder loop, defer setting unimportant flags
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(innerloop);
    Simd_Update_Loop_Info(remainderloop, innerloop,dli,FALSE);

    // Now, insert the remainder loop after the vectorized innerloop.
    if (rloop_needed){
       if(!Do_Loop_Is_Mp(innerloop)){
          if(reduction_node)
               LWN_Insert_Block_After(LWN_Get_Parent(innerloop),
                               reduction_node,remainderloop);
          else
               LWN_Insert_Block_After(LWN_Get_Parent(innerloop),
                                 innerloop,remainderloop);

          LWN_Parentize(remainderloop);
          LWN_Set_Parent(remainderloop, LWN_Get_Parent(innerloop));
       }else { //inner loop is an mp loop
          WN* enclosing_parallel_region = LWN_Get_Parent(innerloop);
          while(enclosing_parallel_region &&
                WN_operator(enclosing_parallel_region) != OPR_REGION)
            enclosing_parallel_region =
              LWN_Get_Parent(enclosing_parallel_region);
          FmtAssert(enclosing_parallel_region, ("Simd: MP loop has no enclosing region!"));

          // Bug 4930 - Do not create a SINGLE region for a PARALLEL DO loop.
          if(WN_pragma(WN_first(WN_region_pragmas(enclosing_parallel_region))) ==
                    WN_PRAGMA_PARALLEL_DO)
            LWN_Insert_Block_After(LWN_Get_Parent(enclosing_parallel_region),
                                 enclosing_parallel_region,remainderloop);
          else{
            // Bug 4884 - Place the remainderloop inside a new REGION marked
          // with a SINGLE pragma.
          WN *body,*pragmas,*exits,*region;

          /* create region on current remainderloop */
          body    = WN_CreateBlock ();
          pragmas = WN_CreateBlock ();
          exits   = WN_CreateBlock ();
          region  = WN_CreateRegion (REGION_KIND_MP,
                                     body,
                                     pragmas,
                                     exits,
                                     -1,
                                     0);
          WN* pragma =
            WN_CreatePragma(WN_PRAGMA_SINGLE_PROCESS_BEGIN,
                            (ST_IDX)NULL, 0, 0);
          WN_set_pragma_omp(pragma);
          LWN_Insert_Block_After(pragmas,NULL,pragma);
          // Bug 5150: Check if the enclosing_parallel_region has NOWAIT
          // directive and (only) if it does, transfer the pragma to the
          // remainder loop. There is an optimization opportunity here to
          // insert a NOWAIT clause if we correctly identify all possible
          // situations but we need to be semantically correct first.
          WN* region_pragma =
            WN_first(WN_region_pragmas(enclosing_parallel_region));
          while(region_pragma && WN_pragma(region_pragma) != WN_PRAGMA_NOWAIT)
            region_pragma = WN_next(region_pragma);
          if (region_pragma && WN_pragma(region_pragma) == WN_PRAGMA_NOWAIT) {
            WN* nowait_pragma =
              WN_CreatePragma (WN_PRAGMA_NOWAIT,
                               (ST_IDX) NULL, 0, 0);
            WN_set_pragma_omp(nowait_pragma);
            LWN_Insert_Block_After(pragmas,pragma,nowait_pragma);
            pragma = nowait_pragma;
          }
          WN* pragma_end =
            WN_CreatePragma(WN_PRAGMA_END_MARKER,
                            (ST_IDX)NULL, 0, 0);
          WN_set_pragma_omp(pragma_end);
          LWN_Insert_Block_After(pragmas,pragma,pragma_end);
          LWN_Insert_Block_After(LWN_Get_Parent(enclosing_parallel_region),
                                 enclosing_parallel_region, region);
          LWN_Insert_Block_After(body,NULL,remainderloop);
          LWN_Set_Parent(remainderloop, body);
          LWN_Parentize(region);
          LWN_Set_Parent(region, LWN_Get_Parent(enclosing_parallel_region));
          // Create a new region id for the new "region".
          // Code from REGION_clone in be/region/region_util.cxx.
          RID *o_rid, *n_rid, *p_rid;
          o_rid = REGION_get_rid(enclosing_parallel_region);
          n_rid = RID_Create(WN_region_id(region), RID_level(o_rid), region);
          /* copy some flags over, set others */
          RID_level(n_rid) = RID_level(o_rid);
          RID_type(n_rid) = RID_type(o_rid);
          RID_depth(n_rid) = RID_depth(o_rid);
          RID_srcpos(n_rid) = WN_Get_Linenum(region);
          RID_bounds_exist(n_rid) = REGION_BOUND_UNKNOWN;
          RID_has_return(n_rid) = REGION_NO_RETURN;
          RID_num_exits(n_rid) = RID_num_exits(o_rid);
          RID_eh_range_ptr(n_rid) = RID_eh_range_ptr(o_rid);
          /* connect to RID to WHIRL */
          WN_MAP_Set(RID_map, region, (void *)n_rid);
          RID_rwn(n_rid) = region;
          p_rid = RID_parent(o_rid);
          RID_Add_kid(n_rid, p_rid);
         }
      }//end mp loop
  
      // Reset the bounds info for the remainderloop and the main innerloop.
      DO_LOOP_INFO *dli_m = Get_Do_Loop_Info(innerloop);
      DO_LOOP_INFO *dli_r = Get_Do_Loop_Info(remainderloop);
      INT num_bounds_m = Num_Lower_Bounds(innerloop, dli_m->Step);
      INT num_bounds_r = Num_Lower_Bounds(remainderloop, dli_r->Step);
      DOLOOP_STACK stack_m(&SIMD_default_pool);
      DOLOOP_STACK stack_r(&SIMD_default_pool);
      CXX_DELETE(dli_m->UB, dli_m->UB->Pool());
      CXX_DELETE(dli_r->LB, dli_r->LB->Pool());
      Build_Doloop_Stack(innerloop, &stack_m);
      Build_Doloop_Stack(remainderloop, &stack_r);
      dli_r->LB =
        CXX_NEW(ACCESS_ARRAY(num_bounds_r,stack_r.Elements(),
                             &LNO_default_pool),
                &LNO_default_pool);
      dli_r->LB->Set_LB(WN_kid0(WN_start(remainderloop)), &stack_r,
                        dli_r->Step->Const_Offset);
      if (dli_r->Est_Num_Iterations >= vect) {
        dli_r-> Est_Num_Iterations = vect - 1;
      }

      dli_m->UB = CXX_NEW(ACCESS_ARRAY(num_bounds_m,stack_m.Elements(),
                                       &LNO_default_pool),
                          &LNO_default_pool);
      dli_m->UB->Set_UB(WN_end(innerloop), &stack_m);
      
      // Set Unimportant flag in loop_info.
      if (WN_kid_count(remainderloop) == 6) {
        WN *loop_info = WN_do_loop_info(remainderloop);
        WN_Set_Loop_Unimportant_Misc(loop_info);
        dli_r->Set_Generally_Unimportant();
      }

   }else {//remainder loop is not needed, dependences should be removed first
      Delete_Def_Use(WN_start(remainderloop));
      Delete_Def_Use(WN_end(remainderloop)); //bug 12291, 12295
      WN *r_body = WN_do_body(remainderloop);
      WN *r_stmt;
      for (r_stmt=WN_first(r_body); r_stmt != NULL; r_stmt=WN_next(r_stmt))
         Delete_Def_Use(r_stmt); 
      LNO_Erase_Dg_From_Here_In(remainderloop,adg);
   }
   
    Simd_Simplify_LB_UB (innerloop, rloop_needed ? remainderloop : NULL, vect);
    adg->Fission_Dep_Update(innerloop, 1);

    if (rloop_needed && (remainder_is_unity || vect == 2) && 
       LNO_Simd_Rm_Unity_Remainder) {
      Simd_Remove_Unity_Remainder (remainderloop, remainder_is_unity, TRUE); 
    }
}

static ST *Build_Sym_Queues( STACK_OF_WN *vec_simd_ops,
                             std::set<ST*>& counted_load_store_sts,
                             std::map<ST*,std::deque<WN*> >& symbol_wn_map )
{
  INT num_loads = 0;

  if (!LNO_Simd_peel_align)
    return NULL;

  for (INT i=0; i<vec_simd_ops->Elements(); i++){
    SIMD_KIND simd_kind = simd_op_kind[i];

    if (simd_kind == INVALID)
      continue;

    WN *simd_op = vec_simd_ops->Top_nth(i);
    if ((simd_kind == V16I8) && (WN_rtype(simd_op) == MTYPE_F8)) {
      for (INT kid = 0; kid < WN_kid_count(simd_op); kid ++) {
        WN *wn = WN_kid(simd_op, kid);
        if ((WN_operator(wn) == OPR_LDID) || (WN_operator(wn) == OPR_STID)) {
          ST *st = WN_st(wn);
          if ((st != NULL) && (ST_class(st) != CLASS_PREG)) {
            if (symbol_wn_map[st].empty()) {
              counted_load_store_sts.insert(st);
            } 
            symbol_wn_map[st].push_front(wn);
          }
        }
      }
    }
  }

  // Next stop: Prune the sym queues down to a single entry or no entries.
  ST *largest_st = NULL;
  std::set<ST*>::const_iterator counted_load_store_sts_it;
  for (counted_load_store_sts_it = counted_load_store_sts.begin();
       counted_load_store_sts_it != counted_load_store_sts.end();
       ++counted_load_store_sts_it) {
    ST *st = *counted_load_store_sts_it;
    num_loads += symbol_wn_map[st].size();
    if (largest_st == NULL)    
      largest_st = st;
    else if (symbol_wn_map[largest_st].size() > symbol_wn_map[st].size())
      largest_st = st;
  }

  // now remove all the smaller st maps, and do not worry about the race
  // on the largest, it is sufficient to take the first one
  for (counted_load_store_sts_it = counted_load_store_sts.begin();
       counted_load_store_sts_it != counted_load_store_sts.end();
       ++counted_load_store_sts_it) {
    ST *st = *counted_load_store_sts_it;
    if (st != largest_st)
      symbol_wn_map[st].clear();
  }

  // Now the final check, what is the ratio of our chosen syms loads vs the total
  if (largest_st != NULL) {
    INT best_size = symbol_wn_map[largest_st].size();
    if (best_size < 3) {
      // Heuristic: Do not multiversion a vector loop with fewer than 3 
      // alignable loads.
      symbol_wn_map[largest_st].clear();
      largest_st = 0;
    } else if ((num_loads / best_size) > 3) {
      // Hueristic: If alignable loads are less than 1/3 of all loads, do
      // not multiversion the vector loop.
      symbol_wn_map[largest_st].clear();
      largest_st = 0;
    }
  } else {
    largest_st = 0;
  }
  return largest_st;
}

static void Clear_Sym_Queues( STACK_OF_WN *vec_simd_ops,
                              std::set<ST*>& counted_load_store_sts,
                              std::map<ST*,std::deque<WN*> >& symbol_wn_map )
{
  std::set<ST*>::const_iterator counted_load_store_sts_it;
  for (counted_load_store_sts_it = counted_load_store_sts.begin();
       counted_load_store_sts_it != counted_load_store_sts.end();
       ++counted_load_store_sts_it) {
    ST *st = *counted_load_store_sts_it;
    symbol_wn_map[st].clear();
  }
}

// Vectorize an innerloop
static INT Simd(WN* innerloop)
{
  std::map<ST*,std::deque<WN*> > sym_wn_map;
  std::set<ST*> counted_load_store_sts;

  if (!Simd_vect_conf.Arch_Has_Vect ())
    return 0;

  INT good_vector = 0;

  //pre_analysis to filter out loops that can not be vectorized
  char verbose_msg[128];
  if(!Simd_Pre_Analysis(innerloop, verbose_msg)){
     if (debug || LNO_Simd_Verbose){
      printf("(%s:%d) %s Loop was not vectorized.\n", Src_File_Name,
             Srcpos_To_Line(WN_Get_Linenum(innerloop)), verbose_msg);
     }
    return 0;
  }

  {//debug purpose only, may not be remapped to the source code
    Last_Vectorizable_Loop_Id ++;
    if (Last_Vectorizable_Loop_Id < LNO_Simd_Loop_Skip_Before ||
	Last_Vectorizable_Loop_Id > LNO_Simd_Loop_Skip_After ||
	Last_Vectorizable_Loop_Id == LNO_Simd_Loop_Skip_Equal) {
      fprintf (stderr, "SIMD: loop (%s:%d) of PU:%d is skipped\n", 
               Src_File_Name, Srcpos_To_Line(WN_Get_Linenum(innerloop)),
               Current_PU_Count ());
      return 0;
    }
  }
 
  MEM_POOL_Push(&SIMD_default_pool);
  {
  DO_LOOP_INFO* dli=Get_Do_Loop_Info(innerloop);
  WN *stmt, *simd_op, *istore;
  WN *body = WN_do_body(innerloop);
  BOOL save_simp_state = WN_Simplifier_Enable(FALSE);
  Simd_Mark_Code(WN_do_body(innerloop)); 
  WN_Simplifier_Enable(save_simp_state);

  if(!Simd_Analysis(innerloop,verbose_msg)){
    MEM_POOL_Pop(&SIMD_default_pool);
    if (debug || LNO_Simd_Verbose){
      printf("(%s:%d) %s Loop was not vectorized.\n", Src_File_Name,
             Srcpos_To_Line(WN_Get_Linenum(innerloop)), verbose_msg);
     }
    return 0;
  }

//START: Alignment Module
  INT *simd_op_best_align[4];
  for(INT k=0; k<4; k++)
    simd_op_best_align[k] = 
      CXX_NEW_ARRAY(INT,vec_simd_ops->Elements(),&SIMD_default_pool);
  BOOL ubound_variable = Simd_Align_UB_Variable(innerloop);

  INT num_loads = 0;
  ST *first_st = NULL;
  if(dli->Loop_Align_Peeled == FALSE){
    first_st = Build_Sym_Queues(vec_simd_ops, 
                                 counted_load_store_sts, 
                                 sym_wn_map);
    if (first_st)
      num_loads = sym_wn_map[first_st].size();
  }
  
  for (INT i=vec_simd_ops->Elements()-1; i >= 0; i--) {
    simd_op=vec_simd_ops->Top_nth(i);

    SIMD_KIND simd_kind = simd_op_kind[i];
    for(INT k=0; k<4; k++)
        simd_op_best_align[k][i]=-1;

    if (simd_kind == INVALID)
      continue;
    
    WN *load_store[4];
    load_store[0]=WN_kid0(simd_op);
    load_store[1] =WN_kid_count(simd_op)>1 ? WN_kid1(simd_op):NULL;
    load_store[2]=WN_kid_count(simd_op)>2 ? WN_kid2(simd_op):NULL;
    load_store[3]=LWN_Get_Parent(simd_op);
    WN* innerloop=LWN_Get_Parent(Find_Do_Body(simd_op));
    INT size = Vec_Unit_Size[simd_kind];
    if (WN_operator(simd_op) == OPR_INTRINSIC_OP)
     for(INT k=0; k<3; k++)
       if(load_store[k]){
          FmtAssert(WN_operator(load_store[k]) == OPR_PARM, ("NYI"));
          load_store[k] = WN_kid0(load_store[k]);
       }
    INT second_indx = vec_simd_ops->Elements()-i-1;
    for(INT k=0; k<3; k++){
      if (load_store[k]==NULL || simd_operand_invariant[k][second_indx] == 1)
        simd_op_best_align[k][i] = -2;
    
      if (simd_op_best_align[k][i] != -2 && 
	WN_operator(load_store[k]) == OPR_ILOAD)
        simd_op_best_align[k][i] =
                          Simd_Align_Analysis(simd_op_best_align[k][i], 
                             load_store[k], simd_op, size, simd_kind, innerloop,FALSE);
    }
    if (WN_operator(load_store[3]) != OPR_ISTORE)
      continue;
     simd_op_best_align[3][i] =
                       Simd_Align_Analysis(simd_op_best_align[3][i],
                          load_store[3], simd_op, size, simd_kind, innerloop,TRUE);
  }
  //align iloads and istores according to simd_op_best_align
  for (INT i=vec_simd_ops->Elements()-1; i >= 0; i--) { 
    simd_op=vec_simd_ops->Top_nth(i);

    if (simd_op_kind[i] == INVALID || !simd_op_last_in_loop[i])
      continue;

    WN* innerloop=LWN_Get_Parent(Find_Do_Body(simd_op));
    INT best_peel = Simd_Align_Best_Peel(vec_simd_ops, simd_op_kind,
                                   simd_op_best_align, innerloop);
    
    if(best_peel==0)
            Simd_Align_Array_References(vec_simd_ops,simd_op_kind, //align iloads and istores
                       simd_op_best_align,best_peel,innerloop);
    if (best_peel <= 0 || ubound_variable)
       continue;
     //best_peel > 0 && !ubound_variable -- bug 2840: don't peel and align ub var
     Simd_Align_Generate_Peel_Loop(innerloop, best_peel, dli);     
     Simd_Align_Array_References(vec_simd_ops,simd_op_kind, //align iloads and istores
                     simd_op_best_align,best_peel,innerloop);
  }

  // Emit multiversion loops for peeled alignment
  if (LNO_Simd_peel_align && (dli->Loop_Align_Peeled == FALSE)) {
    if (num_loads) {
      WN *wn_if = Simd_Align_Generate_Peel_MV_Loops(innerloop, dli, first_st);
      WN *peel_loop = WN_first(WN_then(wn_if));

      Simd_Align_Generate_Peel_Loop(peel_loop, 1, dli);
      innerloop = WN_first(WN_else(wn_if));
      dli = Get_Do_Loop_Info (innerloop);
      // prevent processing this loop complex again the same way
      dli->Loop_Align_Peeled = TRUE;

      // now simd-ize the new mv loop pair
      if (Simd(peel_loop)) {
        WN *loop_info = WN_do_loop_info(peel_loop);
        WN_Set_Vectorized(loop_info);
        WN_Set_Align_Peeled(loop_info);
        if (Simd(innerloop)) {
          loop_info = WN_do_loop_info(innerloop);
          WN_Set_Vectorized(loop_info);
        }
      }
      dli->Loop_Align_Peeled = FALSE;
      Clear_Sym_Queues(vec_simd_ops, counted_load_store_sts, sym_wn_map);
      MEM_POOL_Pop(&SIMD_default_pool);
      return 1;
    }
  }

//END: Alignment Module

#ifdef Is_True_On //internal debug purpose
  if (debug || LNO_Simd_Verbose)
    good_vector = Simd_Count_Good_Vector(vec_simd_ops, simd_op_kind);
#endif

//START: Vectorization Module 
  WN* reduction_node= NULL;
  WN *vec_index_preg_store = NULL;

  unroll_map = WN_MAP_Create(&SIMD_default_pool);

  for (INT i=vec_simd_ops->Elements()-1; i >= 0; i--){
    simd_op=vec_simd_ops->Top_nth(i); 

    // If we are unable to find combination SIMD Kind for this simd_op
    // then skip.
    SIMD_KIND simd_kind = simd_op_kind[i];
    if (simd_kind == INVALID)
      continue;
    
    istore=LWN_Get_Parent(simd_op);

    WN *innerloop = Enclosing_Do_Loop(simd_op); 
    WN *remainderloop = NULL;

    if(simd_op_last_in_loop[i]) //create a remainder loop
        remainderloop = Simd_Create_Remainder_Loop(innerloop);

    //get vector type according to istore
    TYPE_ID vmtype = Simd_Get_Vector_Type(istore);
    //add shuffle ops for cases of negative coefficient 
    Simd_Add_Shuffle_For_Negative_Coefficient(simd_op,innerloop);

    BOOL invarkid[3]; //record invariant kids
    invarkid[0]=invarkid[1]=invarkid[2]=FALSE;
    for (INT kid = 0; kid < WN_kid_count(simd_op); kid ++) {
      WN* inv_node;

      if (simd_operand_invariant[kid][vec_simd_ops->Elements()-i-1] == 1 ||
	  WN_operator(WN_kid(simd_op, kid)) == OPR_LDID) {//invariant or LDID
        invarkid[kid] = TRUE;     
        inv_node = WN_kid(simd_op,kid);
      if (WN_operator(inv_node) == OPR_PARM) {
	inv_node = WN_kid0(inv_node);
	if (WN_operator(inv_node) == OPR_REPLICATE) continue;
      }

      //we handle invariant kid first here
      if(simd_operand_invariant[kid][vec_simd_ops->Elements()-i-1] == 1){
     
      if (WN_operator(inv_node) == OPR_CONST ||
	  WN_operator(inv_node) == OPR_INTCONST){//constant
          //bug 5844, 12348: handle I4MPY here -- kid const must be 2
          if(WN_operator(inv_node) == OPR_INTCONST && WN_const_val(inv_node)==2 &&
             (WN_opcode(simd_op)==OPC_I4MPY || WN_opcode(simd_op)==OPC_V16I4MPY)){
            inv_node = LWN_Copy_Tree(WN_kid(simd_op,1-kid), TRUE, LNO_Info_Map);
            LWN_Copy_Def_Use(WN_kid(simd_op,1-kid), inv_node, Du_Mgr);
            LWN_Copy_Frequency_Tree(inv_node, WN_kid(simd_op,1-kid));
            WN_set_opcode(simd_op, OPCODE_make_op(OPR_ADD, WN_rtype(simd_op), MTYPE_V));
            LWN_Set_Parent(inv_node, simd_op);
            invarkid[kid] = FALSE; //to vectorize this copy later            
          }else{
	  if(MTYPE_is_vector(WN_rtype(inv_node))) continue; //next kid
          inv_node = Simd_Vectorize_Constants(inv_node, istore, simd_op);
        }
      }
      else//real invariant (non-constant) 
         inv_node = Simd_Vectorize_Invariants(inv_node, istore, simd_op);

      if (WN_operator(WN_kid(simd_op, kid)) == OPR_PARM){
        WN_kid0(WN_kid(simd_op, kid)) = inv_node; //down one level
        LWN_Set_Parent(inv_node, WN_kid(simd_op, kid));
      } else
        WN_kid(simd_op, kid) = inv_node;

      LWN_Set_Parent(WN_kid(simd_op, kid), simd_op);
      LWN_Parentize(WN_kid(simd_op, kid));
     }else if(Simd_Good_Reduction_Load(innerloop, WN_kid(simd_op, kid))){
       //handle reduction
       WN* stmt = Find_Stmt_Under(simd_op,WN_do_body(innerloop));
       if (Is_Last_Red_Stmt(stmt)) //the last handles all
          reduction_node =
                     Simd_Vectorize_Scalar_Reduction(WN_kid(simd_op, kid), stmt,
                                       simd_op,  innerloop, vmtype, kid);
     }else{ 
       WN* operand = WN_kid(simd_op, kid);
       // If we have already renamed this kid then skip.
       if (MTYPE_is_vector(WN_desc(operand))) continue;

       if (SYMBOL(operand) == SYMBOL(WN_index(innerloop))){ //induction variable
          vec_index_preg_store = Simd_Vectorize_Induction_Variables(
                                         operand, simd_op, innerloop,
                                         vmtype, vec_simd_ops,
                                         simd_op_kind, simd_kind);
        }else //all other ldid
           Simd_Vectorize_Load_And_Equilvalent(operand, innerloop, vmtype);
    }
   }
  }//end handling kids
  
  //Main phase. Here we use invarkid to guard not resetting kids handled above
  Simd_Vectorize_SimdOp_And_Kids(simd_op, vmtype, invarkid);
  INT vect = Simd_Unroll_Times_By_SimdKind(simd_kind); //loop unroll time

 //Unroll statements for types that require unrolling loop other than "vect" times
  INT stmt_unroll = Simd_Unroll_Times_By_VectorType(vmtype);
  INT unroll_times = vect/stmt_unroll; //copies of statement needed
  INT add_to_base = unroll_times>1?vect/unroll_times:0; //dim index incr
  
  if(unroll_times > 1 && WN_operator(istore) == OPR_ISTORE) {
      //record unroll parameters for now, use them later
      //so that we don't depend on which direction we use to perform vectorization for the loop statements
      UNROLL_PARAMS *params = TYPE_MEM_POOL_ALLOC(UNROLL_PARAMS, &SIMD_default_pool);

      params->unroll_times = unroll_times;
      params->add_to_base = add_to_base;
      params->base_incr   = add_to_base;
      params->innerloop = innerloop;
      params->vec_index_preg_store = vec_index_preg_store;
      params->index_type = index_type;

      WN_MAP_Set(unroll_map, istore, params);
  }
 
  //Finalize innerloop and remainderloop
   if (simd_op_last_in_loop[i])
      Simd_Finalize_Loops(innerloop, remainderloop, vect, reduction_node);
  }

  Simd_Unroll_Necessary_Loop_Statements(WN_do_body(innerloop));

  dli->Loop_Vectorized = TRUE;

  WN_MAP_Delete(unroll_map);
 }
 MEM_POOL_Pop(&SIMD_default_pool);

  if (debug || LNO_Simd_Verbose) {
    printf("(%s:%d) LOOP WAS VECTORIZED.\n", 
	   Src_File_Name, 
	   Srcpos_To_Line(WN_Get_Linenum(innerloop)));
#ifdef Is_True_On
    printf("Loop has %d super vectors\n", good_vector);
#endif
  }

  return 1;
}

static void Simd_Walk(WN* wn) {
  OPCODE opc=WN_opcode(wn);

  if (!OPCODE_is_scf(opc)) 
    return;
  else if (opc==OPC_DO_LOOP) {
    if (Do_Loop_Is_Good(wn) && Do_Loop_Is_Inner(wn) && !Do_Loop_Has_Calls(wn)
	&& !Do_Loop_Has_Gotos(wn)) {
      if (Simd(wn)) {
        Simd_Align = TRUE;
        WN *loop_info = WN_do_loop_info(wn);
        WN_Set_Vectorized(loop_info);
      }
    } else
      Simd_Walk(WN_do_body(wn));
  } else if (opc==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      Simd_Walk(stmt);
      stmt=next_stmt;
    }
  else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Simd_Walk(WN_kid(wn,kidno));
    }
}

void Simd_Phase(WN* func_nd) {

  MEM_POOL_Initialize(&SIMD_default_pool,"SIMD_default_pool",FALSE);
  MEM_POOL_Push(&SIMD_default_pool);

  simd_context = SC_SIMD;

  adg=Array_Dependence_Graph;

  debug = Get_Trace(TP_LNOPT, TT_LNO_DEBUG_SIMD);
  if (debug) {
    fprintf(TFile, "=======================================================================\n");
    fprintf(TFile, "LNO: \"WHIRL tree before simd phase\"\n");
    fdump_tree (TFile, func_nd);
  }
  Simd_Reallocate_Objects = FALSE;  
  Last_Vectorizable_Loop_Id = 0; // Initialize per PU
  if (LNO_Simd_Reduction) {
    simd_red_manager = CXX_NEW 
      (REDUCTION_MANAGER(&SIMD_default_pool), &SIMD_default_pool);
    simd_red_manager->Build(func_nd,TRUE,FALSE); // build scalar reductions
    curr_simd_red_manager = simd_red_manager;
  }
  // Remove (Hoist) memory invariants to aid the vectorizer (bug 5058).
  // When doing memory invariant removal for SIMD, use a temporary instead of 
  // a preg. The reason for doing this is that a following scalar expansion 
  // may incorrectly replace the instances of the memory invariants outside
  // the loop (an example is attached to bug 6606).
  Minvariant_Removal_For_Simd = TRUE;
  if (!Get_Trace(TP_LNOPT, TT_LNO_GUARD) && LNO_Minvar) {
    // If invariants are hoisted, have to guard the loops. This is similar
    // to the other call in lnopt_main.
    Guard_Dos(func_nd);
    Minvariant_Removal(func_nd, Array_Dependence_Graph);
    // Rebuild reduction information after minvariant removal.
    if (curr_simd_red_manager) 
      curr_simd_red_manager->Build(func_nd, TRUE, TRUE, adg);
  }
  Minvariant_Removal_For_Simd = FALSE;
  Simd_Walk(func_nd);
  if (debug) {
    fprintf(TFile, "=======================================================================\n");
    fprintf(TFile, "LNO: \"WHIRL tree after simd phase\"\n");
    fdump_tree (TFile, func_nd);
  }
  if (LNO_Simd_Reduction && simd_red_manager)
    CXX_DELETE(simd_red_manager,&SIMD_default_pool);
  simd_context = SC_INVALID;

  MEM_POOL_Pop(&SIMD_default_pool);
  MEM_POOL_Delete(&SIMD_default_pool);

}

// IPA does not pad common blocks that participate in I/O. The base address
// of the common block is passed to Fortran I/O statement and hence any I/O
// involving these blocks will be incorrect if IPA were to pad these blocks.
// Alignment is an important issue for vectorization (on Opteron) and hence we 
// have disabled vectorization of operations on ARRAYs inside COMMON blocks
// that are not padded to align at 16-bytes.
// Array copies appear as ISTOREs of ILOADs. If they are converted to ISTORE
// of PAREN of ILOAD, then the PAREN can be treated like any other vectorizable
// op. PAREN is later converted to NOP so, there is no overhead in this 
// transformation but it enables vectorization without a different routine.

// Notes on bug fixes:
// Bug 3617 : Num_Vec() from ACCESS_ARRAY may not be in synch with
// WN_num_dim(array) dues to delinearization. If we were to access different
// kids in array, WN_num_dim(array) is the reliable source to find #kids.
 
// Collect the indirect loads in a whirl tree. 
// Does not recursively inspect kids of iloads.
static void WN_collect_iloads_nr(std::list<WN *> *wn_list, WN *wn)
{ 
  if (!wn_list || !wn) return;

  if (OPCODE_operator(WN_opcode(wn))==OPR_ILOAD)
    wn_list->push_back(wn);
  else
  {
    int i;
    for (i = 0; i < WN_kid_count(wn); i++)
    { 
      WN *kid = WN_kid(wn,i);
      WN_collect_iloads_nr(wn_list,kid);
    } 
  } 
} 
