/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- */
//
// Exported functions:
//
//     BOOL IPL_Mark_Code(WN *func_nd)
//
//
//     Mark all the do loops and all the IFs in the function using 
//     the DO_LOOP_INFO_BASE data structure and the IPL_info_map map. 
//     Use IPL_loop_pool to store the annotations.
//
//     void IPL_Build_Access_Vectors(WN* func_nd)
//
//     Build access vectors for all the loops and array expressions
//     that occur in WHIRL
//
//--------------------------------------------------------------------------
#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>                        // Elf64_Word
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>                        // Elf64_Word
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "tracing.h"
#include "loop_info.h"
#include "if_info.h"
#include "soe.h"
#include "opt_alias_interface.h"
#include "ipl_main.h"
#include "ir_reader.h" // for dump tree
#include "cxx_hash.h"
#include "lwn_util.h"
#include "be_util.h"
#include "ipl_summary.h"
#include "ipl_summarize.h"

#ifdef SHARED_BUILD
mINT32
SYSTEM_OF_EQUATIONS::_work[SOE_MAX_WORK_ROWS][SOE_MAX_WORK_COLS];
mINT64 
SYSTEM_OF_EQUATIONS::_work_const[SOE_MAX_WORK_ROWS];
#endif
 
struct ALIAS_MANAGER *Ipl_Al_Mgr;
struct DU_MANAGER *Ipl_Du_Mgr;

MEM_POOL IPL_loop_pool;
MEM_POOL IPL_local_pool;
WN_MAP IPL_info_map;
WN_MAP IPL_reduc_map;
static BOOL trace_section = FALSE;

typedef HASH_TABLE<char*, ST*> ST_TBL;
ST_TBL *ST_node_tbl;

static void
Mark_Code(WN* wn, STACK_OF_WN *stack, DLI_BASE_STACK *dlistack,
	       IF_STACK *if_stack, mUINT8 depth);
static  void
IPL_Build_Access_Vectors(WN* wn, DOLOOP_STACK *stack, MEM_POOL *pool);
static WN*

Find_Match(WN *store,OPCODE rhs_opcode, WN *rhs);

static BOOL
Self_Dependent_Store(WN* store);

static void 
Check_Reduction(WN* store);

static BOOL 
Match(WN *store, WN *value);

static BOOL
Equiv(WN *wn1, WN *wn2);

static void 
IPL_Print_One_Access(FILE *fp, WN* wn);

static void 
IPL_Print_Access(FILE *fp, WN* wn);

// ------------------------------------------------------------------
// For a given ST mark its SUMMARY_SYMBOL if it is a formal parameter
// ------------------------------------------------------------------
extern void
Mark_formal_summary_symbol(ST* s)
{
  if (ST_sclass(s) == SCLASS_FORMAL ||
      ST_sclass(s) == SCLASS_FORMAL_REF) {
    INT32 idx = Summary->Get_symbol_index(s);
    SUMMARY_SYMBOL* sym = Summary->Get_symbol(idx);
    sym->Set_used_in_array_section();
  }
}

//------------------------------------------------------------------
// check if opcodes match
//------------------------------------------------------------------
static BOOL
Opcode_Match(OPCODE op1, OPCODE op2) 
{
  if (op1 == op2) return TRUE;
  if (OPCODE_rtype(op1) != OPCODE_rtype(op2)) return FALSE;
  if (OPCODE_desc(op1) != OPCODE_desc(op2)) return FALSE;
  OPERATOR opr1 = OPCODE_operator(op1);
  OPERATOR opr2 = OPCODE_operator(op2);
  if ((opr1 == OPR_ADD) && (opr2 == OPR_SUB)) return TRUE;
  if ((opr2 == OPR_ADD) && (opr1 == OPR_SUB)) return TRUE;
  return FALSE;
}

//------------------------------------------------------------------
// stolen from lnoutils
//------------------------------------------------------------------
#ifdef SHARED_BUILD
extern
#else
static
#endif
WN* UBvar(WN* end)
{
  WN* wn_index = NULL; 
  OPERATOR opr = WN_operator(end);
  switch (opr) {
   case OPR_LE:
   case OPR_LT:
    wn_index = WN_kid0(end);
    break;
   case OPR_GE:
   case OPR_GT:
    wn_index = WN_kid1(end);
    break;
   default: 
    return NULL; 
  }
  if (WN_operator(wn_index) != OPR_LDID) 
    return NULL; 
  return wn_index; 
}

static BOOL 
Loop_pool_initialized = FALSE;
//------------------------------------------------------------------
// initalize maps and mempools needed 
//------------------------------------------------------------------
extern void
IPL_Initialize_Par_Code()
{
  trace_section = Get_Trace(TP_IPL, TT_IPL_SECTION);

  IPL_info_map  = WN_MAP_Create(&IPL_loop_pool);
  IPL_reduc_map = WN_MAP32_Create(&IPL_loop_pool);
  if (!Loop_pool_initialized)
    {
      MEM_POOL_Initialize(&IPL_loop_pool, "ipl loop pool", 0);
      MEM_POOL_Initialize(&IPL_local_pool, "ipl local pool", 0);
      Loop_pool_initialized = TRUE;
    }

  MEM_POOL_Push(&IPL_loop_pool);
  MEM_POOL_Push(&IPL_local_pool);

  
  ST_node_tbl = (ST_TBL*)CXX_NEW(ST_TBL(12, &IPL_loop_pool), &IPL_loop_pool);

}

//------------------------------------------------------------------
// Pop the mempools
//------------------------------------------------------------------
extern void 
IPL_Finalize_Par_Code()
{
  MEM_POOL_Pop(&IPL_local_pool);
  MEM_POOL_Pop(&IPL_loop_pool);
  WN_MAP_Delete(IPL_info_map);
  WN_MAP_Delete(IPL_reduc_map);
}

//------------------------------------------------------------------
// Mark the code with access vectors and reductions
//------------------------------------------------------------------
void
IPL_Mark_Code(WN* func_nd)
{
  mUINT8 depth = 0;

  DLI_BASE_STACK *dlistack = 
    CXX_NEW(DLI_BASE_STACK(&IPL_loop_pool),&IPL_loop_pool); 

  STACK_OF_WN *stack =
    CXX_NEW(STACK_OF_WN(&IPL_loop_pool),&IPL_loop_pool);

  IF_STACK *if_stack = 
    CXX_NEW(IF_STACK(&IPL_loop_pool),&IPL_loop_pool); 

  Mark_Code(func_nd, stack, dlistack, if_stack, depth);

}

//------------------------------------------------------------------
// func_nd = PU
// depth = depth of the loop
// stack = WNs of all the outer do loops
// dlistack = stack of outer do loop infos
// if stack = stack of outer ifs
// depth = max depth of do loops inside you
//------------------------------------------------------------------
static 
void Mark_Code(WN* wn, 
               STACK_OF_WN* stack, 
	       DLI_BASE_STACK* dlistack, 
               IF_STACK* if_stack,
	       mUINT8 depth)
{
  FmtAssert (wn, ("NULL node encountered in Mark_Code\n"));

  WN* kid;
  DO_LOOP_INFO_BASE *dli;
  OPERATOR oper = WN_operator(wn);

  switch (oper) {

    case OPR_BLOCK:
      kid = WN_last (wn);
      while (kid) {
        Mark_Code(kid, stack, dlistack, if_stack, depth);
        kid = WN_prev(kid);
      }
      break;
      
    case OPR_DO_LOOP: {
      INT i;

      dli = (DO_LOOP_INFO_BASE*) WN_MAP_Get(IPL_info_map, wn);

      if (!dli) {
        dli = CXX_NEW(DO_LOOP_INFO_BASE(&IPL_loop_pool), &IPL_loop_pool);
        dli->Set_depth(depth);
        WN_MAP_Set(IPL_info_map,wn,(void *)dli);
      }
      else {
        dli->Set_is_inner_loop();
      }

      ++depth;
	
      // make sure that the rest of the loops are not considered inner
      for (i = 0; i < dlistack->Elements(); ++i) {
        dlistack->Bottom_nth(i)->Reset_is_inner_loop();
      }
      
      // if there are ifs, then mark them as containing do loops
      for (i = 0; i < if_stack->Elements(); ++i) {
        if_stack->Bottom_nth(i)->Contains_Do_Loops = TRUE;
      }
      
      stack->Push(wn);
      dlistack->Push(dli);
    } 
    break;

    case OPR_IF: {
      IF_INFO *if_info = (IF_INFO*) WN_MAP_Get(IPL_info_map, wn);
      if (!if_info) {
        if_info = CXX_NEW(IF_INFO(&IPL_loop_pool, FALSE,FALSE),&IPL_loop_pool);
        WN_MAP_Set(IPL_info_map, wn, (void*) if_info);
      }
      else {
        if_info->Contains_Do_Loops = FALSE;
      }
      if_stack->Push(if_info);
    } 
    break;

    case OPR_IO:
    case OPR_CALL:
    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER: {
      for (INT i = 0; i < dlistack->Elements(); ++i) {
        dlistack->Bottom_nth(i)->Set_has_calls();
      }
    } 
    break;

    case OPR_DO_WHILE:
    case OPR_WHILE_DO:
    case OPR_COMPGOTO: { 
      for (INT i = 0; i < dlistack->Elements(); i++) {
	dlistack->Bottom_nth(i)->Set_has_gotos();
      }
    } 
    break;

    default: {
      if (OPERATOR_is_non_scf(oper)) {
        for (INT i = 0; i < dlistack->Elements(); i++) {
          dlistack->Bottom_nth(i)->Set_has_gotos();
        }
      }
      if (OPERATOR_is_store(oper)) {
        Check_Reduction(wn);
      }
    } 
    break;
  }

  for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    INT64 constval;
    kid = WN_kid(wn, kidno);
    if (WN_operator(kid) == OPR_LDID && Wn_Is_Intconst(kid, &constval)) {
      WN_kid(wn, kidno) = WN_Intconst (WN_rtype(kid), constval);
      LWN_Set_Parent (WN_kid(wn, kidno), wn);
      IPA_WN_DELETE_Tree (Current_Map_Tab, kid);
    }
    else {
      Mark_Code(kid,stack,dlistack,if_stack,depth);
    }
  }      

  if (oper == OPR_DO_LOOP) {
    stack->Pop();
    dlistack->Pop();
    depth--;
  }
  else if (oper == OPR_IF) {
    if_stack->Pop();
  }
}

//-------------------------------------------------------------
// Build the access vectors attach them to nodes
//-------------------------------------------------------------
extern  void
IPL_Build_Access_Vectors(WN* func_nd)
{
  

  MEM_POOL_Push(&IPL_local_pool);

  DOLOOP_STACK *stack =
    CXX_NEW(DOLOOP_STACK(&IPL_local_pool),&IPL_local_pool);
  IPL_Build_Access_Vectors(func_nd,stack,&IPL_loop_pool);

  CXX_DELETE(stack,&IPL_local_pool);

  MEM_POOL_Pop(&IPL_local_pool);
  if (Get_Trace(TP_IPL, TT_IPL_VERBOSE))
    IPL_Print_Access(stderr, func_nd);
}

//-------------------------------------------------------------
// build the access vector information for the bounds of the
// do loop
//-------------------------------------------------------------
static void
IPL_Build_Do_Access(WN* wn, DOLOOP_STACK *stack)
{

  DO_LOOP_INFO_BASE *dli = (DO_LOOP_INFO_BASE *) WN_MAP_Get(IPL_info_map,wn);

  FmtAssert(dli,("Unmapped DO loop in IPL_Build_Do_Access"));

  MEM_POOL *pool=dli->Pool();

  // first the step
  ACCESS_VECTOR *step = 
    CXX_NEW(ACCESS_VECTOR(stack->Elements(),pool),pool);

  stack->Push(wn);

  dli->Set_step(step);
  dli->Set_lb(NULL);
  dli->Set_ub(NULL);
  WN *s = WN_step(wn);
  if (WN_operator(s) != OPR_STID) {
    step->Too_Messy = TRUE;
    goto return_point;
  }
  { 
    s = WN_kid(s,0);
    INT8 sign;
    if (WN_operator(s) == OPR_ADD) {
      sign = 1;
    } else if (WN_operator(s) == OPR_SUB) {
      sign = -1;
    } else {
      step->Too_Messy = TRUE;
      goto return_point;
    }

    // one of s's children is the loop variable, the other is the step
    // we don't know which
    BOOL found_step = FALSE;
    SYMBOL doloop(WN_index(wn));
    if (WN_operator(WN_kid(s,0)) == OPR_LDID) {
      WN*kid = WN_kid(s,0);
      SYMBOL symbol(kid);
      Is_True(symbol.St(),("Null symbol in IPL_Build_Do_Access"));
      if (symbol == doloop) {
	  step->Set(WN_kid(s,1),stack,sign,0);
	  found_step = TRUE;
      }
    }

    if (!found_step && WN_operator(WN_kid(s,1)) == OPR_LDID) {
      SYMBOL symbol(WN_kid(s,1));
      if (symbol == doloop) {
	  step->Set(WN_kid(s,0),stack,sign,0);
	  found_step = TRUE;
      }
    }
    if (!found_step) {
      step->Too_Messy = TRUE;
      goto return_point;
    }
    if (!step->Is_Const()) {  // can't get a bound if we don't know sign of step
      goto return_point;
    }

    // Next the lower bound
    // First, how many bounds

    WN *l = WN_start(wn);

    Is_True(WN_operator(l) == OPR_STID,
      ("IPL_Build_Do_Access::LB of do loop is not a stid"));

    INT num_bounds = Num_Lower_Bounds(wn, step); 



    ACCESS_ARRAY *lb = 
      CXX_NEW(ACCESS_ARRAY(num_bounds,stack->Elements(),pool),pool);


    dli->Set_lb(lb);

    // now actually do the building
    dli->Get_lb()->Set_LB(WN_kid(l,0),stack,step->Const_Offset);

    // Now the upper bound
    // First, how many bounds
    WN *u = WN_end(wn);

    Is_True(OPCODE_is_compare(WN_opcode(u)),
      ("IPL_Build_Do_Access::UB of do loop is not a compare"));

    num_bounds = Num_Upper_Bounds(wn);



    // now actually do the building
    ACCESS_ARRAY *ub = 
      CXX_NEW(ACCESS_ARRAY(num_bounds,stack->Elements(),pool),pool);
    dli->Set_ub(ub);
    dli->Get_ub()->Set_UB(u,stack);

    WN* wn_var = UBvar(WN_end(wn));

    // Swap the lower bound and upper bound if the step is negative
    if (step->Is_Const() && (step->Const_Offset < 0)) {

      ACCESS_ARRAY *ub = dli->Get_ub();
      dli->Set_ub(dli->Get_lb());
      dli->Set_lb(ub);
    }

    // check for the weird degenerate case for (i =1; i<N; i--)

    if (step->Is_Const()) {
      if ((step->Const_Offset > 0)) {
	for (INT i=0; i<dli->Get_ub()->Num_Vec(); i++) {
	  ACCESS_VECTOR *ub = dli->Get_ub()->Dim(i);
	  if (ub->Loop_Coeff(dli->Get_depth()) < 0) {
	    dli->Get_lb()->Too_Messy = TRUE;
	    dli->Get_ub()->Too_Messy = TRUE;
	    step->Too_Messy = TRUE;
	    break;
	  }
	}
      } else {
	for (INT i=0; i<dli->Get_lb()->Num_Vec(); i++) {
	  ACCESS_VECTOR *lb = dli->Get_lb()->Dim(i);
	  if (lb->Loop_Coeff(dli->Get_depth()) > 0) {
	    dli->Get_lb()->Too_Messy = TRUE;
	    dli->Get_ub()->Too_Messy = TRUE;
	    step->Too_Messy = TRUE;
	    break;
	  }
	}
      }
    }
  } 

return_point:

  stack->Pop();

  if (!dli->Get_lb()) {
    ACCESS_ARRAY *lb=
    	CXX_NEW(ACCESS_ARRAY(1,stack->Elements()+1,pool),pool);
    dli->Set_lb(lb);
    lb->Too_Messy = TRUE;
  }
  if (!dli->Get_ub()) {
    ACCESS_ARRAY *ub =
    	CXX_NEW(ACCESS_ARRAY(1,stack->Elements()+1,pool),pool);
    dli->Set_ub(ub);
    ub->Too_Messy = TRUE;
  }
}

//-------------------------------------------------------------
// build the access vector for the array reference
//-------------------------------------------------------------
static void
IPL_Build_Access_Array(WN *wn, DOLOOP_STACK *stack, MEM_POOL *pool)
{
  ACCESS_ARRAY *array = 
    CXX_NEW(ACCESS_ARRAY(WN_num_dim(wn),stack->Elements(),
			 pool),pool);
  array->Set_Array(wn,stack);

  WN_MAP_Set(IPL_info_map,wn,(void *)array);

  if (stack->Elements() == 0 && trace_section)
      fprintf(stderr, "NULL elements in the stack, hence NO do loops \n");

}

//-------------------------------------------------------------
// build access vector information for IFs
//-------------------------------------------------------------
static void
IPL_Build_If_Access(WN *wn, DOLOOP_STACK *stack)
{

  Is_True(WN_opcode(wn) == OPC_IF,("Non-if in IPL_Build_If_Access"));

  IF_INFO *info = (IF_INFO *) WN_MAP_Get(IPL_info_map,wn);
  Is_True(info,("Unmapped IF loop in IPL_Build_IF_Access"));

  MEM_POOL *pool=info->Pool();

  WN *compare = WN_if_test(wn);
  BOOL top_level_not;

  if (WN_operator(compare) == OPR_LNOT) {
    top_level_not = TRUE;
    compare = WN_kid0(compare);
  } else {
    top_level_not = FALSE;
  }

  if (WN_operator(compare) == OPR_LIOR) {
    INT num_lors = Num_Liors(compare);
    info->Condition = 
      CXX_NEW(ACCESS_ARRAY(num_lors+1,stack->Elements(),pool),pool);
    info->Condition_On_Then = top_level_not;
    info->Condition->Set_IF(compare,stack,TRUE,FALSE,0);
  } else {
    INT num_lands = Num_Lands(compare);
    info->Condition = 
      CXX_NEW(ACCESS_ARRAY(num_lands+1,stack->Elements(),pool),pool);
    info->Condition_On_Then = !top_level_not;
    info->Condition->Set_IF(compare,stack,FALSE,TRUE,0);
  } 
}



//-------------------------------------------------------------
// walk the pu, build the access vectors
//-------------------------------------------------------------
static  void
IPL_Build_Access_Vectors(WN* wn, DOLOOP_STACK *stack, MEM_POOL *pool)
{
  WN* kid;

  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      IPL_Build_Access_Vectors(kid,stack,pool);
      kid = WN_next(kid);
    }
    return;
  } 

  if (WN_opcode(wn) == OPC_DO_LOOP) {
    IPL_Build_Do_Access(wn,stack);
    stack->Push(wn);
  } 
  else if (WN_operator(wn) == OPR_ARRAY) {
    IPL_Build_Access_Array(wn,stack,pool);
  } else if (WN_opcode(wn) == OPC_IF) {
    IPL_Build_If_Access(wn,stack);
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    IPL_Build_Access_Vectors(kid,stack,pool);
  }

  if (WN_opcode(wn) == OPC_DO_LOOP) {
    stack->Pop();
  }
}

static void Mark_Formals_In_Tree(WN* wn_tree)
{
  if (OPCODE_has_sym(WN_opcode(wn_tree)))
    Mark_formal_summary_symbol(WN_st(wn_tree)); 
  for (INT i = 0; i < WN_kid_count(wn_tree); i++)
    Mark_Formals_In_Tree(WN_kid(wn_tree, i));
} 


static void Mark_Formals_In_Reduction_Increment(WN* wn_stid, 
					        WN* wn_ldid)
{
  if (WN_operator(wn_stid) != OPR_STID 
      && WN_operator(wn_ldid) != OPR_LDID)
    return; 
  WN* wn_redop = WN_kid0(wn_stid);
  if (WN_kid_count(wn_redop) != 2)
    return; 
  WN* wn_left = WN_kid0(wn_redop);
  WN* wn_right = WN_kid1(wn_redop);
  if (wn_left != wn_ldid && wn_right != wn_ldid)
    return;
  WN* wn_tree = (wn_left == wn_ldid) ? wn_right : wn_left; 
  Mark_Formals_In_Tree(wn_tree);
} 

//-------------------------------------------------------------
// check for reductions
//-------------------------------------------------------------
static void 
Check_Reduction(WN* store)
{

  // for STIDS always enter the symbol
  // keep these STs in a list to be used to determine if 
  // we need to store the associated control flow for this
  if (WN_operator(store) == OPR_STID)
    ST_node_tbl->Enter(ST_name(WN_st(store)), WN_st(store));

// first check that the operation is a reduction type
  REDUCTION_TYPE type = RED_NONE;

  WN *rhs = WN_kid0(store);
  OPERATOR oper = WN_operator(rhs);

  switch(oper) {
    case OPR_ADD: case OPR_SUB:
      type = RED_ADD;
      break;
    case OPR_MPY: 
      type = RED_MPY;
      break;
    case OPR_MAX:
      type = RED_MAX;
      break;
    case OPR_MIN:
      type = RED_MIN;
      break;
    default: 
      return; // not a reduction operator
  }
  // next check that the lhs is equivalent to one of the children of the 
  // right hand side (if it's a minus, it has to be equivalent to the first
  // child of the right hand side)

  WN *match_ld; // the wn of the kid that matches the store

  match_ld = Find_Match(store,WN_opcode(rhs),rhs);
  if ((match_ld) && (WN_operator(store) == OPR_ISTORE))
    {
      if (!Self_Dependent_Store(store)) {
	//printf("Found a Reduction \n");
	// dump_tree(store);
	WN_MAP32_Set(IPL_reduc_map,store,(INT32) type);
	WN_MAP32_Set(IPL_reduc_map,match_ld,(INT32) type);
      }
    }
  else if (match_ld)
    {
      // check if the parent of this is a do loop and that
      // this is the increment part of a do loop
      WN* parent_wn = LWN_Get_Parent(store);
      if (!(WN_operator(parent_wn) == OPR_DO_LOOP &&
	  WN_step(parent_wn) == store))
	{
	  WN_MAP32_Set(IPL_reduc_map,store,(INT32) type);
	  WN_MAP32_Set(IPL_reduc_map,match_ld,(INT32) type);
          Mark_Formals_In_Reduction_Increment(store, match_ld);
	}
    }
}


//-------------------------------------------------------------
// do we need information about this scalar?
//-------------------------------------------------------------
BOOL Record_scalar_flow(WN* stid)
{
  FmtAssert((WN_operator(stid) == OPR_STID),( "expecting OPR_STID \n"));

  if (ST_node_tbl->Find(ST_name(WN_st(stid))))
      {
	return TRUE;
      }
  return FALSE;
}

//-------------------------------------------------------------
// is value a load to the same location we're storing in store
//-------------------------------------------------------------
static BOOL 
Match(WN *store, WN *value)
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

//-------------------------------------------------------------------
// Find a load to the same location as store, keep looking as deep as
// possible
//-------------------------------------------------------------------
static WN*
Find_Match(WN *store,OPCODE rhs_opcode, WN *rhs) {
  WN *kid0 = WN_kid0(rhs);

  if (OPCODE_operator(rhs_opcode) == OPR_SUB) {
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

//-------------------------------------------------------------------
// self dependent store of the form a(a(i)) = ... 
//-------------------------------------------------------------------
static BOOL 
Self_Dependent_Store(WN* store)
{
  WN* wn1, *wn2;
  WN* lhs = WN_kid1(store);
  ALIAS_RESULT result;
  wn1 = WN_kid1(store);
  if (WN_operator(wn1) == OPR_ARRAY)
    {
      wn2 = WN_array_base(wn1);
      if ((!WN_operator(wn1) == OPR_LDID) ||
	  (!WN_operator(wn1) == OPR_LDA))
	{
	  for (INT kidno=0; kidno<WN_kid_count(wn2); kidno++) 
	    {
	      WN* wn3 = WN_kid(wn2, kidno);
	      //printf("------------dumping wn1 \n--------------");
	      // dump_tree(wn1);
	      //printf("------------dumping wn2 \n--------------");
	      // dump_tree(wn2);
	      result =  Aliased(Ipl_Al_Mgr, wn1, wn3);
	      // printf("result = %d \n", result);
	      if (!(result == NOT_ALIASED))
		return TRUE;
	    }
	  return FALSE;
	}
    }
  return FALSE;
}

//-------------------------------------------------------------------
// are the two expressions subtrees equivalent
//-------------------------------------------------------------------
static BOOL
Equiv(WN *wn1, WN *wn2)
{
  if (!WN_Equiv(wn1,wn2)) return(FALSE);
  for (INT kidno=0; kidno<WN_kid_count(wn1); kidno++) {
    if (!Equiv(WN_kid(wn1,kidno),WN_kid(wn2,kidno))) {
      return(FALSE);
    }
  }
  return(TRUE);
}

//-------------------------------------------------------------
// print the access vectors and the loop information for a 
// routine
//-------------------------------------------------------------
static void 
IPL_Print_Access(FILE *fp, WN* wn)
{
 WN *kid;

 if (!wn) return;

 if (OPCODE_is_leaf(WN_opcode(wn))) return;

 if (WN_opcode(wn) == OPC_BLOCK) {
   kid = WN_first (wn);
   while (kid) {
     IPL_Print_Access(fp,kid);
      kid = WN_next(kid);
    }
    return;
  } 

  IPL_Print_One_Access(fp, wn); 

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    IPL_Print_Access(fp,kid);
  }
}

//-------------------------------------------------------------
// Print the access information for 1 whirl node
//-------------------------------------------------------------
static void 
IPL_Print_One_Access(FILE *fp, WN* wn)
{
  if (WN_opcode(wn) == OPC_DO_LOOP) 
    {
      DO_LOOP_INFO_BASE *dli = (DO_LOOP_INFO_BASE *)
	WN_MAP_Get(IPL_info_map,wn);
      fprintf(fp,"The do loop info is \n"); dli->Print(fp);
    } 
  else if (WN_operator(wn) == OPR_ARRAY) 
    {
      ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(IPL_info_map,wn);
      if (array) 
	{
	  if (trace_section)
	    fprintf(fp,"The array is \n"); array->Print(fp);
	} 
      else 
	{
	  if (trace_section)
	    fprintf(fp,"Null array \n");
	} 
    }
}

//----------------------------------------------------------------
// print the loop information
//----------------------------------------------------------------
void
DO_LOOP_INFO_BASE::Print(FILE *fp, INT indentation)
{
  char buf[80];
  INT i;

  for (i = 0; i < indentation && i < 79; i++)
    buf[i] = ' ';
  buf[i] = '\0';

  if (Has_calls()) 
      fprintf(fp,"%sIt has calls \n", buf);
  if (Has_gotos())
      fprintf(fp,"%sIt has non-DO or non-IF controlflow\n", buf);
  if (Is_inner_loop()) 
      fprintf(fp,"%sIs an inner loop \n", buf);
  fprintf(fp, "%sDepth is %d \n", buf, Get_depth());

  fprintf(fp,"%sThe lb is ", buf);
  if (Get_lb()) 
      Get_lb()->Print(fp,TRUE);
  else 
      fprintf(fp, "<null>\n");

  fprintf(fp,"%sThe ub is ", buf);

  if (Get_ub()) 
      Get_ub()->Print(fp,TRUE);
  else 
      fprintf(fp, "<null>\n");
  fprintf(fp,"%sThe step is ", buf);

  if (Get_step()) {
      Get_step()->Print(fp);
      fprintf(fp,"\n");
  }
  else
      fprintf(fp, "<null>\n");

}

//------------------------------------------------------------
// Walk the pruned control flow graph and 
// 1) project sections onto loops as far out as possible
// 2) Union sections if under the same control flow
// 3) Intersect sections in if then else parts
//------------------------------------------------------------

extern "C" void Print_DO_LOOP_INFO_BASE (FILE *fp, DO_LOOP_INFO_BASE *b)
{
  b->Print(fp);
}



