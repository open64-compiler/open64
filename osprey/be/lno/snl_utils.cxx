/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_utils_CXX      "snl_utils.cxx"
static char *rcs_id =   snl_utils_CXX "$Revision: 1.6 $";

#include <sys/types.h>
#include "snl.h"

#include "targ_sim.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "dep_graph.h"
#include "wintrinsic.h"
#include "opt_du.h"
#include "ff_utils.h"
#include "config_targ.h" 
#include "ipa_lno_util.h"

#ifdef Is_True_On
extern char* Cur_PU_Name;
#endif

MEM_POOL SNL_local_pool;

static ARRAY_DIRECTED_GRAPH16* dg;

static void sc_print_debug_stop_here()
{
}

#ifdef Is_True_On
#define SC_PRINT(a,b) ((void) ( (a) ? 0 : \
                                (printf b , printf("\n"), \
                                 printf("<subroutine %s>\n", Cur_PU_Name), \
                                 fflush(stdout), \
                                 sc_print_debug_stop_here(), 0)))

#define SC_ASSERT SC_PRINT
/*#define SC_ASSERT FmtAssert*/

static void Check_Zero_Linear_Coefficients(WN* wn_ref, ACCESS_VECTOR *av)
{
  INTSYMB_ITER lin_iter(av->Lin_Symb);
  for (INTSYMB_NODE *lin_node=lin_iter.First(); !lin_iter.Is_Empty();
    lin_node = lin_iter.Next())
    SC_ASSERT(lin_node->Coeff != 0, 
      ("Access vector for %p has 0 linear coefficient", wn_ref));  
}

#endif 

static INT Check_Depth(WN* wn)
{
  if (wn == NULL)
    return -1;
  INT depth = Check_Depth(LWN_Get_Parent(wn));
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    depth++;
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    FmtAssert(dli->Depth == depth,
              ("DO confusion: %d %d (loop %s: %p)",
               dli->Depth, depth, SYMBOL(WN_index(wn)).Name(), wn));
    return depth;
  }
  else
    return depth;
}

void Print_Do_Stack(FILE* f, const DOLOOP_STACK *stack)
{
  fprintf(f, "dostack:");
  for (INT i = 0; i < stack->Elements(); i++) {
    WN* indx = WN_index(stack->Bottom_nth(i));
    fprintf(f, " <|%s,%lld,%d|>",
	    ST_name(WN_st(indx)),
	    ST_ofst(WN_st(indx)),
	    WN_offset(indx));
  }
  fprintf(f, "\n");
  fflush(f);
}

// Mono()
//
// Requires an expression, or a statement initializing some symbol.
// Returns that the expression is either invariant in the symbol,
// increasing, decreasing, or something uglier.  It assumes that
// the symbol itself is the only value that changes with the symbol:
// no loads, stores, calls, etc have a value that depends upon the
// symbol or changes the symbol.  This assumption is valid because of
// the checks made in SNL_Find_Nest.

static SNL_MONO mono_union(SNL_MONO a, SNL_MONO b)
{
  switch (a) {
   default:
    FmtAssert(0, ("Impossible SNL_MONO TYPE %d", a));
   case SNL_MONO_INVARIANT:
    return b;
   case SNL_MONO_INC:
    if (b == SNL_MONO_INVARIANT || b == SNL_MONO_INC)
      return SNL_MONO_INC;
    else
      return SNL_MONO_OTHER;
   case SNL_MONO_DEC:
    if (b == SNL_MONO_INVARIANT || b == SNL_MONO_DEC)
      return SNL_MONO_DEC;
    else
      return SNL_MONO_OTHER;
   case SNL_MONO_OTHER:
    return SNL_MONO_OTHER;
  }
}

SNL_MONO Mono(WN* wn, SYMBOL symbol, BOOL neg)
{
  SNL_MONO	m;
  INT64		c;
  INT		kid;

  OPCODE	opc = WN_opcode(wn);
  OPERATOR	opr = OPCODE_operator(opc);

  FmtAssert(OPCODE_is_expression(opc) || opr == OPR_STID,
	    ("Odd opcode %s passed to Mono()", OPCODE_name(opc)));

  switch (opr) {
   default:
    Is_True(0, ("unknown opr %s [no problem -- just implement it!]",
                OPERATOR_name(opr)));
    m = SNL_MONO_OTHER;
    break;

   case OPR_IDNAME:
    Is_True(0, ("strange opr %s for Mono()",
                  OPERATOR_name(opr)));
    m = SNL_MONO_OTHER;
    break;

   case OPR_INTRINSIC_OP: 
    {
      m = SNL_MONO_OTHER;
      switch (WN_intrinsic(wn)) {
       case INTRN_I4DIVFLOOR:
       case INTRN_I8DIVFLOOR:
       case INTRN_U4DIVFLOOR:
       case INTRN_U8DIVFLOOR:
       case INTRN_I4DIVCEIL:
       case INTRN_I8DIVCEIL:
       case INTRN_U4DIVCEIL:
       case INTRN_U8DIVCEIL:
	FmtAssert(WN_operator(WN_kid0(wn)) == OPR_PARM &&
		  WN_operator(WN_kid1(wn)) == OPR_PARM,
		  ("Children of intrinsic op must be parms"));
        if (WN_operator(WN_kid0(WN_kid1(wn))) == OPR_INTCONST) {
          m = Mono(WN_kid0(WN_kid0(wn)), symbol);
          if (WN_const_val(WN_kid0(WN_kid1(wn))) < 0)
            neg = !neg;
        }
      }
    }
    break;

   case OPR_ILOAD:
   case OPR_MLOAD:
   case OPR_ARRAY:
   case OPR_COMPLEX:
   case OPR_ABS:
   case OPR_SQRT:
   case OPR_RECIP:
   case OPR_RSQRT:
#ifdef TARG_X8664
   case OPR_ATOMIC_RSQRT:
#endif
   case OPR_REALPART:
   case OPR_IMAGPART:
   case OPR_RND:
   case OPR_TRUNC:
   case OPR_BNOT:
   case OPR_LNOT:
   case OPR_MOD:
   case OPR_REM:
   case OPR_EQ:
   case OPR_NE:
   case OPR_GE:
   case OPR_GT:
   case OPR_LE:
   case OPR_LT:
   case OPR_BAND:
   case OPR_BIOR:
   case OPR_BXOR:
   case OPR_LAND:
   case OPR_LIOR:
   case OPR_CAND:
   case OPR_CIOR:
   case OPR_LDA:
    for (kid = 0; kid < WN_kid_count(wn); kid++) {
      FmtAssert(Mono(WN_kid(wn,kid), symbol) == SNL_MONO_INVARIANT,
		("operator %s depends upon %s, violating assumptions",
		 OPERATOR_name(opr), symbol.Name()));
    }
    m = SNL_MONO_OTHER;
    break;

   case OPR_STID:
   case OPR_PAREN:
   case OPR_TAS:
   case OPR_CVT:
   case OPR_CVTL:
   case OPR_CEIL:
   case OPR_FLOOR:
   case OPR_PARM:                // PARM for intrinsics
    m = Mono(WN_kid0(wn), symbol);
    break;

   case OPR_NEG:
    m = Mono(WN_kid0(wn), symbol, TRUE);
    break;

   case OPR_ADD:
   case OPR_MAX:
   case OPR_MIN:
    m = mono_union(Mono(WN_kid0(wn), symbol), Mono(WN_kid1(wn), symbol));
    break;

   case OPR_SUB:
    m = mono_union(Mono(WN_kid0(wn), symbol), Mono(WN_kid1(wn), symbol, TRUE));
    break;

   case OPR_MPY:
   case OPR_DIV:
    if (opr == OPR_MPY &&
	WN_operator(WN_kid0(wn)) == OPR_INTCONST) {
      c = WN_const_val(WN_kid0(wn));
      if (c == 0)
	m = SNL_MONO_INVARIANT;
      else if (c > 0)
	m = Mono(WN_kid1(wn), symbol);
      else
	m = Mono(WN_kid1(wn), symbol, TRUE);
    }
    else if (WN_operator(WN_kid1(wn)) == OPR_INTCONST) {
      c = WN_const_val(WN_kid1(wn));
      if (c == 0)
	m = SNL_MONO_INVARIANT;
      else if (c > 0)
	m = Mono(WN_kid0(wn), symbol);
      else
	m = Mono(WN_kid0(wn), symbol, TRUE);
    }
    else {
      if (Mono(WN_kid0(wn), symbol) == SNL_MONO_INVARIANT &&
	  Mono(WN_kid1(wn), symbol) == SNL_MONO_INVARIANT)
	m = SNL_MONO_INVARIANT;
      else
	m = SNL_MONO_OTHER;
    }
    break;

   case OPR_SHL:
   case OPR_ASHR:
   case OPR_LSHR:
    if (Mono(WN_kid1(wn), symbol) == SNL_MONO_INVARIANT)
      m = Mono(WN_kid0(wn), symbol);
    else
      m = SNL_MONO_OTHER;
    break;

   case OPR_LDID:
    if (symbol == SYMBOL(wn))
      m = SNL_MONO_INC;
    else
      m = SNL_MONO_INVARIANT;
    break;

   case OPR_INTCONST:
   case OPR_CONST:
    return SNL_MONO_INVARIANT;
  }

  if (neg) {
    if (m == SNL_MONO_INC)
      m = SNL_MONO_DEC;
    else if (m == SNL_MONO_DEC)
      m = SNL_MONO_INC;
  }

  return m;
}


BOOL Is_Lexpos(DEPV_ARRAY* dv)
{
  for (INT v = 0; v < dv->Num_Vec(); v++)
    if (!Is_Lexpos(dv->Depv(v), dv->Num_Dim()))
      return FALSE;
  return TRUE;
}

//--------------------------------------------------------------------
// General utilities.
//--------------------------------------------------------------------

void Increase_By(WN* wn, INT c, WN* parent, INT kid)
{
  FmtAssert(wn, ("Bad wn for Increase_By"));

  OPCODE	opc = WN_opcode(wn);
  OPERATOR	opr = OPCODE_operator(opc);

  if (opr == OPR_STID) {
    parent = wn;
    kid = 0;
    wn = WN_kid0(wn);
    opc = WN_opcode(wn);
    opr = OPCODE_operator(opc);
  }

  if (parent == NULL) {
    parent = LWN_Get_Parent(wn);
    FmtAssert(parent, ("Missing parent in Increase_By"));
  }

  if (kid < 0) {
    for (kid = 0; kid < WN_kid_count(parent); kid++)
      if (WN_kid(parent,kid) == wn)
	break;
    FmtAssert(kid < WN_kid_count(parent),
	      ("Missing kid: op=%d kc=%d", WN_opcode(wn), WN_kid_count(wn)));
  }

  switch (opr) {
   case OPR_INTCONST:
    WN_const_val(wn) += c;
    break;
   case OPR_MAX:
   case OPR_MIN:
    Increase_By(WN_kid0(wn), c, wn, 0);
    Increase_By(WN_kid1(wn), c, wn, 1);
    break;
   case OPR_ADD:
   case OPR_SUB:
    if (WN_operator(WN_kid1(wn)) == OPR_INTCONST) {
      if (opr == OPR_ADD)
	WN_const_val(WN_kid1(wn)) += c;
      else      
	WN_const_val(WN_kid1(wn)) -= c;
    }
    else
      Increase_By(WN_kid0(wn), c, wn, 0);
    break;
   default:
    FmtAssert(OPCODE_is_expression(opc),
	      ("Bad opcode %s to Increase_By()", OPCODE_name(opc)));

    TYPE_ID	wtype = OPCODE_rtype(opc);
    OPCODE	add = OPCODE_make_op(OPR_ADD, wtype, MTYPE_V);
    WN* exp = LWN_CreateExp2(add, wn, LWN_Make_Icon(wtype, c));
    LWN_Copy_Frequency_Tree(exp, wn);
    LWN_Set_Parent(exp, parent);
    WN_kid(parent,kid) = exp;
  }
}

// Take statements off block before (after) wn and make a new block with
// only those statements, in the same order.  Parents must be correct.
// If none, return NULL.

WN* LWN_Create_Block_From_Stmts_Above(WN* wn)
{
  // start with prev_executable and take the prev back from there
  WN*	parent = LWN_Get_Parent(wn);
  FmtAssert(parent, ("wn_create_block_from_stmts_above() requires parents"));

  WN*	block = WN_CreateBlock();
  WN*	pprev = NULL;
  for (WN* prev = WN_prev_executable(wn); prev; prev = pprev) {
    pprev = WN_prev(prev);
    LWN_Extract_From_Block(parent, prev);
    LWN_Insert_Block_After(block, NULL, prev);
  }

  return block;
}

WN* LWN_Create_Block_From_Stmts_Below(WN* wn)
{
  // start with next_executable and take the next up from there
  WN*	parent = LWN_Get_Parent(wn);
  FmtAssert(parent, ("create_block_from_stmts_below() requires parents"));

  WN*	block = WN_CreateBlock();
  WN*	nnext = NULL;
  for (WN* next = WN_next_executable(wn); next; next = nnext) {
    nnext = WN_next(next);
    LWN_Extract_From_Block(parent, next);
    LWN_Insert_Block_Before(block, NULL, next);
  }

  return block;
}

// iterations computes the number of iterations in this loop.  If it cannot
// figure it out (non-constant) then return -1.

INT64 Iterations(WN* loop,
		 MEM_POOL* pool) 
{
  INT64	lb = 0;
  INT64	ub = 0;
  BOOL	is_const_lb;
  BOOL	is_const_ub;
  WN*	wnconst;

  if (!Upper_Bound_Standardize(WN_end(loop), TRUE))
    return -1; 

  FmtAssert(WN_opcode(loop) == OPC_DO_LOOP, ("Bad parameter to Iterations()"));

  INT  stepsz = Step_Size(loop);
  if (stepsz == 0)
    return -1;

  // easy to handle if one or the other is constant

  wnconst = WN_kid0(WN_start(loop));
  is_const_lb = WN_operator(wnconst) == OPR_INTCONST;
  if (is_const_lb)
    lb = WN_const_val(wnconst);

  BOOL is_ne;
  wnconst = SNL_UBexp(WN_end(loop), &is_ne);
  is_const_ub = WN_operator(wnconst) == OPR_INTCONST;
  if (is_const_ub) {
    ub = WN_const_val(wnconst);
    if (is_ne)
      ub--;
  }

  if (is_const_lb && is_const_ub) {
    if (stepsz < 0) {
      lb = -lb;
      ub = -ub;
      stepsz = -stepsz;
    }
    return lb <= ub ? (ub - lb + stepsz)/stepsz: 0;
  }

  // can build access vectors and see if the difference is constant
  // use a bit of local memory; that's ok.
  // Lie and say this is the only loop on the stack, since same answer.

  DOLOOP_STACK stack(pool);
  stack.Push(loop);

  if (WN_operator(WN_kid0(WN_start(loop))) == OPR_MAX)
    return -1;
  if (WN_operator(SNL_UBexp(WN_end(loop),NULL)) == OPR_MIN)
    return -1;

  INT rval = -1;
  // Assumes step is 1
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop); 
  ACCESS_VECTOR *av = Add(dli->LB->Dim(0), dli->UB->Dim(0), pool);
  if (av->Is_Const()) {
    if (stepsz < 0) {
      av->Const_Offset = -av->Const_Offset;
      stepsz = -stepsz;
    }
    rval = av->Const_Offset >= 0 ? (av->Const_Offset + stepsz)/stepsz : 0;
  }
  CXX_DELETE(av, pool);

  return rval;
}

extern WN* Find_Next_Innermost_Do_In_Block(WN* wn_block)
{
  for (WN* wn = WN_first(wn_block); wn != NULL; wn = WN_next(wn)) {
    if (WN_opcode(wn) == OPC_REGION) {
      WN* wn_inside = Find_Next_Innermost_Do_In_Block(WN_region_body(wn));
      return wn_inside;
    } 
    if (WN_opcode(wn) == OPC_DO_LOOP)
      return wn;
  }
  return NULL;
}

extern WN* Find_Next_Innermost_Do(WN* loop)
{
  return Find_Next_Innermost_Do_In_Block(WN_do_body(loop));
}

DOLOOP_STACK* Copy_Dostack(const DOLOOP_STACK& stack, MEM_POOL* pool)
{
  DOLOOP_STACK* newstack = CXX_NEW(DOLOOP_STACK(pool), pool);
  for (INT i = 0; i < stack.Elements(); i++)
    newstack->Push(stack.Bottom_nth(i));
  return newstack;
}

//-----------------------------------------------------------------------
// NAME: Valid_SNL_Region 
// FUNCTION: Returns TRUE if 'region' is a valid SNL region, FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Valid_SNL_Region(SNL_REGION region)
{
  if (region.First == NULL && region.Last == NULL)
    return TRUE; 
  if (region.First == NULL || region.Last == NULL)
    return FALSE; 
  for (WN* wn = region.First; wn != NULL; wn = WN_next(wn))
    if (wn == region.Last)
      return TRUE; 
  return FALSE; 
} 

#ifdef Is_True_On

struct SANITY_CHECK_RVAL {
  mBOOL has_loops;
  mBOOL has_regions;
  mBOOL has_calls;
  mBOOL has_io;
  WN*   has_bad_mem;

  SANITY_CHECK_RVAL() :
    has_loops(FALSE), has_regions(FALSE), has_calls(FALSE),
    has_bad_mem(NULL), has_io(FALSE) {}
  void operator += (const SANITY_CHECK_RVAL r) {
    if (r.has_loops)
      has_loops = TRUE;
    if (r.has_regions)
      has_regions = TRUE;
    if (r.has_calls)
      has_calls = TRUE;
    if (r.has_bad_mem)
      has_bad_mem = r.has_bad_mem;
    if (r.has_io)
      has_io = TRUE;
  }
};

static SANITY_CHECK_RVAL SNL_Sanity_Check_Block(WN*, INT depth);

static SANITY_CHECK_RVAL SNL_Sanity_Check_Loop(WN* wn, INT depth)
{
  FmtAssert(wn, ("Missing loop for sanity check"));
  FmtAssert(WN_opcode(wn) == OPC_DO_LOOP,
	    ("Bad opcode %d for sanity check of block", WN_opcode(wn)));

  const INT indexnamesz = 64;
  char      indexname[indexnamesz];
  (SYMBOL(WN_index(wn))).Name(indexname,indexnamesz);

  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
  SC_ASSERT(dli->LB && dli->UB && dli->Step,
	    ("sanity check failed: Missing LB, UB or Step (%p)", wn));

  if (!dli->LB->Too_Messy) {
    for (INT i = 0; i < dli->LB->Num_Vec(); i++) {
      ACCESS_VECTOR* av = dli->LB->Dim(i);
      if (!av->Too_Messy) {
        INT lval = av->Loop_Coeff(dli->Depth);
        SC_ASSERT(lval < 0,
                  ("sanity check failed: lbval=%d depth=%d index=%s (%p)",
                   lval, dli->Depth, indexname, wn));
	Check_Zero_Linear_Coefficients(wn, av); 
      }
    }
  }
  if (!dli->UB->Too_Messy) {
    for (INT i = 0; i < dli->UB->Num_Vec(); i++) {
      ACCESS_VECTOR* av = dli->UB->Dim(i);
      if (!av->Too_Messy) {
        INT uval = av->Loop_Coeff(dli->Depth);
        SC_ASSERT(uval > 0,
                  ("sanity check failed: ubval=%d depth=%d index=%s (%p)",
                   uval, dli->Depth, indexname, wn));
	Check_Zero_Linear_Coefficients(wn, av); 
      }
    }
  }

  SANITY_CHECK_RVAL r = SNL_Sanity_Check_Block(WN_do_body(wn), depth+1);

  if (!r.has_io) {
    SC_ASSERT((r.has_loops == FALSE) == (dli->Is_Inner != FALSE),
              ("sanity check failed: index=%s (%p): has_loops: %d %d",
               indexname, wn, r.has_loops, dli->Is_Inner));
    SC_ASSERT(depth+1 == dli->Depth,
              ("sanity check failed: index=%s (%p): depth: %d %d",
               indexname, wn, depth+1, dli->Depth));

    SC_ASSERT((dli->Has_Bad_Mem != 0) == (r.has_bad_mem != NULL) ||
              (dli->Has_Bad_Mem && (dli->Has_Calls || dli->Has_Gotos)),
              ("sanity check failed: index=%s (%p): has_bad_mem: %d %p",
               indexname, wn, dli->Has_Bad_Mem, r.has_bad_mem));
  }

  r.has_loops = TRUE;
  WN* bad;
  if (bad = SNL_Sanity_Check_Exp(WN_start(wn)))
    r.has_bad_mem = bad;
  if (bad = SNL_Sanity_Check_Exp(WN_end(wn)))
    r.has_bad_mem = bad;
  if (bad = SNL_Sanity_Check_Exp(WN_step(wn)))
    r.has_bad_mem = bad;

  return r;
}

static SANITY_CHECK_RVAL SNL_Sanity_Check_If(WN* wn, INT depth)
{
  FmtAssert(wn && WN_opcode(wn) == OPC_IF, ("Bad if for sanity check"));

  SANITY_CHECK_RVAL r;

  WN* bad;
  if (bad = SNL_Sanity_Check_Exp(WN_if_test(wn)))
    r.has_bad_mem = bad;
  SANITY_CHECK_RVAL rt = SNL_Sanity_Check_Block(WN_then(wn), depth);
  SANITY_CHECK_RVAL re = SNL_Sanity_Check_Block(WN_else(wn), depth);

  IF_INFO* ii = Get_If_Info(wn, TRUE);
  FmtAssert(ii, ("Missing IF info annotation"));
  FmtAssert(ii->Condition, ("Missing If Condition"));
  if (!rt.has_io) {
    SC_ASSERT((rt.has_loops||re.has_loops) == (ii->Contains_Do_Loops!=FALSE),
              ("Bad if info: has_loops disagreement: %d %d %d %p",
               rt.has_loops, re.has_loops, ii->Contains_Do_Loops, wn));
    SC_ASSERT((rt.has_regions||re.has_regions) == (ii->Contains_Regions!=FALSE),
              ("Bad if info: has_regions disagreement: %d %d %d %p",
               rt.has_regions, re.has_regions, ii->Contains_Regions, wn));
  }

  r += rt;
  r += re;
  return r;
}

// return number of bad memrefs
WN* SNL_Sanity_Check_Exp(WN* wn)
{
#ifdef KEY
  if (WN_operator(wn) == OPR_ASM_STMT)
    return NULL;
#endif /* KEY */
  FmtAssert(wn, ("Null wn in SNL_Sanity_Check_Exp"));

  WN*           rval = NULL;
  OPCODE	opc = WN_opcode(wn);
  OPERATOR	opr = OPCODE_operator(opc);

  FmtAssert(!OPCODE_is_scf(opc) && opc != OPC_BLOCK,
	    ("problem in SNL_Sanity_Check, op=%d\n", opc));

  if (OPCODE_is_store(opc) || OPCODE_is_load(opc)) {
    if (OPCODE_has_1ty(opc) && TY_is_volatile(WN_ty(wn))) {
      rval = wn;
    } else if (OPCODE_has_2ty(opc)&&(TY_is_volatile(WN_ty(wn)) ||
		TY_is_volatile(WN_load_addr_ty(wn)))) {
      rval = wn;
    }
  }

  if (opr == OPR_STID) {
    USE_LIST* ul = Du_Mgr->Du_Get_Use(wn);
    if ((!ul || (!ul->Incomplete() && ul->Len() == 0)) &&
        !((ST_class(WN_st(wn))==CLASS_PREG)
           && Preg_Is_Dedicated(WN_offset(wn)))) {
      DevWarn("sanity check warning(%s): missing uses for def (%p) of %s <%s>\n",
              Cur_PU_Name, wn, SYMBOL(wn).Name(),
              ul ? "empty list" : "missing list");
    }
  }
  else if (opr == OPR_LDID) {
    DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn);
    if (!dl || dl->Len() == 0) {
      DevWarn("sanity check warning(%s): missing defs for use (%p) of %s <%s> <id %d:%d>\n",
	      Cur_PU_Name, wn, SYMBOL(wn).Name(),
	      dl ? "empty list" : "missing list", OPCODE_mapcat(WN_opcode(wn)), 
	      WN_map_id(wn));
    }
  }
  else if (opr == OPR_INTRINSIC_OP) {
    FmtAssert(WN_intrinsic(wn) >= INTRINSIC_FIRST &&
              WN_intrinsic(wn) <= INTRINSIC_LAST,
              ("Sanity check failed: Bad intrinsic number %d for opcode %s",
               WN_intrinsic(wn), OPCODE_name(WN_opcode(wn))));
  }
  else if (OPCODE_is_load(opc) || OPCODE_is_store(opc) ||
	   OPCODE_is_call(opc)) {
    ::dg = Array_Dependence_Graph;
    if (!dg->Get_Vertex(wn))
      rval = wn;
  } else if (OPCODE_operator(opc) == OPR_ARRAY) {
    ::dg = Array_Dependence_Graph;
    WN* parent=LWN_Get_Parent(wn);
    if (WN_operator(parent) == OPR_PARM) {
      if (!dg->Get_Vertex(LWN_Get_Parent(parent))) {
        rval = wn;
      }
    }
    ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn); 
    if (aa != NULL && !aa->Too_Messy) {
      for (INT i = 0; i < aa->Num_Vec(); i++) {
        ACCESS_VECTOR* av = aa->Dim(i);
        if (av->Too_Messy)
          continue;
        Check_Zero_Linear_Coefficients(wn, av); 
      }
    }
  }

  for (INT k = 0; k < WN_kid_count(wn); k++) {
    WN* bad = SNL_Sanity_Check_Exp(WN_kid(wn,k));
    if (bad)
      rval = bad;
  }

  return rval;
}

static SANITY_CHECK_RVAL SNL_Sanity_Check_Block(WN* body, INT depth)
{
  WN* bad;

  FmtAssert(body, ("Missing body for sanity check"));
  FmtAssert(WN_opcode(body) == OPC_BLOCK,
	    ("Bad opcode %d for sanity check of block", WN_opcode(body)));

  SANITY_CHECK_RVAL rval;

  for (WN* wn = WN_first(body); wn; wn = WN_next(wn)) {
    OPCODE	opc = WN_opcode(wn);

    switch (opc) {
     case OPC_DO_LOOP:
      rval += SNL_Sanity_Check_Loop(wn, depth);
      break;
     case OPC_IF:
      rval += SNL_Sanity_Check_If(wn, depth);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      rval += SNL_Sanity_Check_Block(WN_while_body(wn), depth);
      if (bad = SNL_Sanity_Check_Exp(WN_while_test(wn)))
        rval.has_bad_mem = bad;
      break;
     case OPC_IO:
      // Not sanity checking the children at all.
      rval.has_calls = TRUE;
      rval.has_io = TRUE;
      break;
     case OPC_COMPGOTO:
      if (bad = SNL_Sanity_Check_Exp(WN_kid(wn,0)))
        rval.has_bad_mem = bad;
      break;
     case OPC_REGION:
      rval += SNL_Sanity_Check_Block(WN_region_body(wn), depth);
      rval.has_regions = TRUE;
      break;
     default:
      if (OPCODE_is_call(opc))
	rval.has_calls = TRUE;
      if (bad = SNL_Sanity_Check_Exp(wn))
        rval.has_bad_mem = bad;
      break;
    }
  }

  return rval;
}

void SNL_Sanity_Check_Region(SNL_REGION region)
{

  if (!Valid_SNL_Region(region)) { 
    DevWarn("SNL_Sanity_Check_Region: Invalid SNL region %p->%p", 
      region.First, region.Last); 
    return;
  } 

  if (region.First == NULL && region.Last == NULL) 
    return; 

  INT depth = Check_Depth(LWN_Get_Parent(region.First));

  for (WN* wn = region.First; wn; wn = wn==region.Last ? NULL : WN_next(wn)) {
    OPCODE	opc = WN_opcode(wn);

    switch (opc) {
     case OPC_DO_LOOP:
      SNL_Sanity_Check_Loop(wn, depth);
      break;
     case OPC_IF:
      SNL_Sanity_Check_If(wn, depth);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      SNL_Sanity_Check_Block(WN_while_body(wn), depth);
      (void) SNL_Sanity_Check_Exp(WN_while_test(wn));
      break;
     case OPC_IO:
      break;
     case OPC_COMPGOTO:
      break;
     default:
      (void) SNL_Sanity_Check_Exp(wn);
      break;
    }
  }
}

void SNL_Sanity_Check_Block(WN* wn)
{
  SNL_Sanity_Check_Block(wn, Do_Depth(wn));
}

void SNL_Sanity_Check_Func(WN* func_nd)
{
  dg = Array_Dependence_Graph; 
  SNL_Sanity_Check_Block(WN_func_body(func_nd), -1);
  Du_Sanity_Check(func_nd);
  FB_Sanity_Check(func_nd);
}

void SNL_Sanity_Check_Body(WN* wn)
{
  SNL_Sanity_Check_Block(wn, Do_Depth(wn));
}

void SNL_Sanity_Check_Loop(WN* loop)
{
  SNL_Sanity_Check_Loop(loop, Do_Depth(loop)-1);
}

void SNL_Sanity_Check_If(WN* wn)
{
  SNL_Sanity_Check_If(wn, Do_Depth(wn));
}

#endif

struct RENUMBER_INFO {
  INT	loops_inside;
  INT	missing_dg_nodes;
  RENUMBER_INFO() : loops_inside(0), missing_dg_nodes(0) {}
  RENUMBER_INFO& operator += (const RENUMBER_INFO& a) {
    loops_inside += a.loops_inside;
    missing_dg_nodes += a.missing_dg_nodes;
    return *this;
  }
};

// return number of unmapped nodes
INT Renumber_Exp(WN* wn)
{
  INT           ans = 0;
  OPCODE        op = WN_opcode(wn);
  OPERATOR      oper = OPCODE_operator(op);

  if (((OPCODE_is_load(op) && OPCODE_operator(op) != OPR_LDID) ||
       (OPCODE_is_store(op) && OPCODE_operator(op) != OPR_STID) ||
       OPCODE_is_call(op)) &&
      !dg->Get_Vertex(wn))
    ans++;
  if (op == OPC_IO || oper == OPR_FORWARD_BARRIER 
    || oper == OPR_BACKWARD_BARRIER) 
    ans++; 
  for (INT k = 0; k < WN_kid_count(wn); k++)
    ans += Renumber_Exp(WN_kid(wn,k));
  return ans;
}

// return number of loops inside
static RENUMBER_INFO Renumber_Loops(WN* first, WN* last, INT depth)
{
  RENUMBER_INFO info;

  if (first == NULL)
    return info;

  for (WN* wn = first; ; wn = WN_next(wn)) {
    FmtAssert(wn, ("last not found for first in Renumber_Loops()"));
    switch (WN_opcode(wn)) {
     case OPC_IF:
      info.missing_dg_nodes += Renumber_Exp(WN_if_test(wn));
      info += Renumber_Loops(WN_first(WN_then(wn)),
			     WN_last(WN_then(wn)),
			     depth);
      info += Renumber_Loops(WN_first(WN_else(wn)),
			     WN_last(WN_else(wn)),
			     depth);
      break;
     case OPC_DO_LOOP:
      {
        RENUMBER_INFO info2;
        info2.missing_dg_nodes += Renumber_Exp(WN_start(wn));
        info2.missing_dg_nodes += Renumber_Exp(WN_end(wn));
        info2.missing_dg_nodes += Renumber_Exp(WN_step(wn));
        info2 +=  Renumber_Loops(WN_first(WN_do_body(wn)),
				   WN_last(WN_do_body(wn)),
				   depth+1);
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
        dli->Is_Inner = info2.loops_inside == 0;
        dli->Depth = depth + 1;
	if (info2.missing_dg_nodes != 0)
          dli->Has_Bad_Mem = TRUE; 
        info += info2;
        info.loops_inside++;
      }
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      info.missing_dg_nodes += Renumber_Exp(WN_while_test(wn));
      info += Renumber_Loops(WN_first(WN_while_body(wn)),
			     WN_last(WN_while_body(wn)),
			     depth);
      break;
     case OPC_REGION: 
       info += Renumber_Loops(WN_first(WN_region_exits(wn)), 
			      WN_last(WN_region_exits(wn)), depth);
       info += Renumber_Loops(WN_first(WN_region_pragmas(wn)), 
			      WN_last(WN_region_pragmas(wn)), depth);
       info += Renumber_Loops(WN_first(WN_region_body(wn)), 
			      WN_last(WN_region_body(wn)), depth);
       break; 
     default:
      info.missing_dg_nodes += Renumber_Exp(wn);
      break;
    }
    if (wn == last)
      break;
  }

  return info;
}

INT Renumber_Loops(WN* first, WN* last, ARRAY_DIRECTED_GRAPH16* dg)
{
  ::dg = (dg == NULL) ? Array_Dependence_Graph : dg;

  FmtAssert(first && last, ("bad call to Renumber_Loops()"));
  FmtAssert(LWN_Get_Parent(first) && LWN_Get_Parent(last),
            ("bad call to Renumber_Loops()"));

  INT depth = Check_Depth(LWN_Get_Parent(first));
  RENUMBER_INFO info = Renumber_Loops(first, last, depth);
  if (info.missing_dg_nodes)
    Unmapped_Vertices_Here_Out(LWN_Get_Parent(first));

  return info.loops_inside;
}

void Dump_WN(SNL_REGION region, FILE* f, INT i1, INT i2, INT i3,
	     WN** wnp, WN* wn2, ARRAY_DIRECTED_GRAPH16* dg)
{
  for (WN* wn = region.First; wn; wn = wn==region.Last ? NULL : WN_next(wn))
    Dump_WN(wn, f, i1, i2, i3, dg, wnp, wn2);
}

static void SNL_Add_Du_To_Index_Ldid_Ldid(WN* loop, WN* ldid, DU_MANAGER* du,
                                          BOOL code_in_loop)
{
  du->Add_Def_Use(WN_start(loop), ldid);
  du->Add_Def_Use(WN_step(loop), ldid);
  DEF_LIST* dl = du->Ud_Get_Def(ldid);
  dl->Set_loop_stmt(code_in_loop ? loop : NULL);
}

void SNL_Add_Du_To_Index_Ldid(WN* loop, WN* exp, DU_MANAGER* du,
                              BOOL code_in_loop)
{
  if (WN_operator(exp) == OPR_LDID &&
      SYMBOL(WN_index(loop)) == SYMBOL(exp)) {
    SNL_Add_Du_To_Index_Ldid_Ldid(loop, exp, du, code_in_loop);
    FmtAssert(du->Ud_Get_Def(exp), ("failed to add!"));
  }

  if (WN_opcode(exp) == OPC_BLOCK) {
    for (WN* w = WN_first(exp); w; w = WN_next(w))
      SNL_Add_Du_To_Index_Ldid(loop, w, du, code_in_loop);
  }
  else {
    for (INT i = 0; i < WN_kid_count(exp); i++)
      SNL_Add_Du_To_Index_Ldid(loop, WN_kid(exp,i), du, code_in_loop);
  }
}

void SNL_Change_Du_To_Index_Ldid(WN* loop, WN* exp, DU_MANAGER* du,
                                 BOOL code_in_loop)
{
  if (WN_operator(exp) == OPR_LDID &&
      SYMBOL(WN_index(loop)) == SYMBOL(exp)) {
    du->Remove_Use_From_System(exp);
    SNL_Add_Du_To_Index_Ldid_Ldid(loop, exp, du, code_in_loop);
    FmtAssert(du->Ud_Get_Def(exp), ("failed to add!"));
  }

  if (WN_opcode(exp) == OPC_BLOCK) {
    for (WN* w = WN_first(exp); w; w = WN_next(w))
      SNL_Change_Du_To_Index_Ldid(loop, w, du, code_in_loop);
  }
  else {
    for (INT i = 0; i < WN_kid_count(exp); i++)
      SNL_Change_Du_To_Index_Ldid(loop, WN_kid(exp,i), du, code_in_loop);
  }
}

void SNL_Change_Du_Pointer(WN* oldptr, WN* ptr, WN* wn, DU_MANAGER* du)
{
  OPCODE	opc = WN_opcode(wn);

  if (opc == OPC_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      SNL_Change_Du_Pointer(oldptr, ptr, w, du);
  }
  else {
    if (OPCODE_operator(opc) == OPR_LDID) {
      DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn);
      if (dl && dl->Loop_stmt() == oldptr)
	dl->Set_loop_stmt(ptr);
    }
    for (INT i = 0; i < WN_kid_count(wn); i++)
      SNL_Change_Du_Pointer(oldptr, ptr, WN_kid(wn,i), du);
  }
}

void SNL_Fix_Index_Pointers(WN* loop, WN* wn)
{
  OPCODE	opc = WN_opcode(wn);

  if (opc == OPC_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      SNL_Fix_Index_Pointers(loop, w);
  }
  else {
    if (OPCODE_operator(opc) == OPR_LDID &&
	SYMBOL(wn) == SYMBOL(WN_index(loop))) {
      DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn);
      if (dl)
	dl->Set_loop_stmt(loop);
    }
    for (INT i = 0; i < WN_kid_count(wn); i++)
      SNL_Fix_Index_Pointers(loop, WN_kid(wn,i));
  }
}

void SNL_Print_Ldid_Pointers(WN* wn)
{
  OPCODE	opc = WN_opcode(wn);

  if (opc == OPC_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      SNL_Print_Ldid_Pointers(w);
  }
  else {
    if (OPCODE_operator(opc) == OPR_LDID) {
      DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn);
      if (dl == NULL)
	printf("%p <missing deflist>\n", wn);
      else {
	WN* loop = dl->Loop_stmt();
	printf("%p %p %s\n", wn, loop,
	       loop ? SYMBOL(WN_index(loop)).Name() : "<none>");
      }
    }
    for (INT i = 0; i < WN_kid_count(wn); i++)
      SNL_Print_Ldid_Pointers(WN_kid(wn,i));
  }
}

WN* Find_Use_In_Exp(WN* exp, const SYMBOL& sym)
{
  if (WN_operator(exp) == OPR_LDID) {
    if (sym == SYMBOL(exp)) {
      return exp;
    } 
  }

  for (INT k = 0; k < WN_kid_count(exp); k++) {
    WN* rv = Find_Use_In_Exp(WN_kid(exp,k), sym);
    if (rv)
      return rv;
  }

  return NULL;
}


const DEF_LIST* Find_Def_List_In_Exp(WN* exp, const SYMBOL& sym)
{
  const DEF_LIST* l = NULL;
  WN* wn = Find_Use_In_Exp(exp, sym);
  if (wn) {
    l = Du_Mgr->Ud_Get_Def(wn);
    FmtAssert(l, ("Missing def list for %s", sym.Name()));
  }
  return l;
}

WN* SNL_Copy_Exp(WN* wn)
{
  WN* new_wn = LWN_Copy_Tree(wn, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(wn, new_wn, Du_Mgr);
  return new_wn;
}

// TODO: These two are not used in very many places -- it should be used
// instead of WN_prev(wn) == NULL tests -- snl_nests.cxx and trans
// for example are way to conservative in what is called a perfect
// nest.  Also, should this be in lnoutils??

// Good_Do_Next_Innermost1(): private to Good_Do_Next_Innermost
//	Examine a single WN (must be a block), returning
//	    0 if not a loop anywhere recursively inside.  Don't overwrite
//		unique_answer.
//	    1 if not a loop anywhere recursively inside, except
//		exactly one DO_LOOP (not looking inside that DO_LOOP).
//	 	Return the loop in unique_answer
//	   -1 otherwise (too ugly).  May trash unique_answer.
//
// If you pass a DO node, the last parameter tells whether that counts or not.
//
// Unstructured control flow is not checked, but that's fine, because
// annotations on DO_LOOPs are checked.
//
// Note that DO(IF(DO)) is not acceptable.

static INT Good_Do_Next_Innermost1(WN* blk, WN** unique_answer)
{
  FmtAssert(WN_opcode(blk) == OPC_BLOCK, ("expected block"));

  BOOL do_seen = FALSE;
  INT  val;

  for (WN* wn = WN_first(blk); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_IF:
      if ((val=Good_Do_Next_Innermost1(WN_then(wn), unique_answer)) != 0 ||
          (val=Good_Do_Next_Innermost1(WN_else(wn), unique_answer)) != 0) {
        SNL_DEBUG1(3, "Inside if val=%d", val);
        return -1;
      }
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      if ((val=Good_Do_Next_Innermost1(WN_then(wn), unique_answer)) != 0) {
        SNL_DEBUG1(3, "Inside do-while val=%d", val);
        return -1;
      }
      break;
     case OPC_DO_LOOP:
      if (do_seen) {
	SNL_DEBUG1(3, "Loop %s is second seen", SYMBOL(WN_index(wn)).Name());
	return -1;
      }
      if (!Do_Loop_Is_Good(wn) || Do_Loop_Has_Gotos(wn)) {
	SNL_DEBUG1(3, "Loop %s is bad!", SYMBOL(WN_index(wn)).Name());
	return -1;
      }
      *unique_answer = wn;
      do_seen = TRUE;
      break;
    }
  }

  return do_seen ? 1 : 0;
}

WN* Good_Do_Next_Innermost(WN* wn)
{
  FmtAssert(WN_opcode(wn) == OPC_DO_LOOP, ("expected block"));

  WN* answer;
  INT val = Good_Do_Next_Innermost1(WN_do_body(wn), &answer);
  return (val == 1) ? answer : NULL;
}

static BOOL Wn_Intrinsic_Is_Floor(WN* wn)
{
  INT32 intr = WN_intrinsic(wn);

  return intr == INTRN_I4DIVFLOOR ||
         intr == INTRN_I8DIVFLOOR ||
         intr == INTRN_U4DIVFLOOR ||
         intr == INTRN_U8DIVFLOOR;
}

static BOOL Wn_Intrinsic_Is_Ceil(WN* wn)
{
  INT32 intr = WN_intrinsic(wn);

  return intr == INTRN_I4DIVCEIL ||
         intr == INTRN_I8DIVCEIL ||
         intr == INTRN_U4DIVCEIL ||
         intr == INTRN_U8DIVCEIL;
}

static void SNL_Optimize_Bounds(WN* wn)
{
  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      SNL_Optimize_Bounds(w);
  }
  else if (opr == OPR_DO_LOOP) {
    // just examine the upper bound.  Replace
    //        x <= a divfloor b        ----->     x*b <= a
    //        x >= a divceil  b        ----->     x*b <= a
    // b positive

    WN*       end = WN_end(wn);
    OPCODE    opend = WN_opcode(end);
    OPERATOR  oprend = OPCODE_operator(opend);

    BOOL less = (oprend == OPR_LE || oprend == OPR_LT);
    while (less || (oprend == OPR_GE || oprend == OPR_GT)) {
      WN*     l = WN_kid0(end);
      WN*     r = WN_kid1(end);
      OPCODE  lop = WN_opcode(l);
      OPCODE  rop = WN_opcode(r);
      if (OPCODE_operator(rop) == OPR_INTRINSIC_OP &&
          ((Wn_Intrinsic_Is_Floor(r) && less) ||
           (Wn_Intrinsic_Is_Ceil(r) && !less)) &&
          WN_operator(WN_kid1(r)) == OPR_INTCONST &&
          WN_const_val(WN_kid1(r)) > 0) {
        WN* rl = WN_kid0(r);
        WN* rr = WN_kid1(r);
        OPCODE  mulop = OPCODE_make_op(OPR_MPY, OPCODE_rtype(rop), MTYPE_V);
        WN_Delete(r);
        WN_kid0(end) = LWN_CreateExp2(mulop, l, rr);
        WN_kid1(end) = rl;
        LWN_Set_Parent(WN_kid0(end), end);
        LWN_Set_Parent(WN_kid1(end), end);
      }
      else if (OPCODE_operator(lop) == OPR_INTRINSIC_OP &&
          ((Wn_Intrinsic_Is_Floor(l) && less) ||
           (Wn_Intrinsic_Is_Ceil(l) && !less)) &&
          WN_operator(WN_kid1(l)) == OPR_INTCONST &&
          WN_const_val(WN_kid1(l)) > 0) {
        WN* ll = WN_kid0(l);
        WN* lr = WN_kid1(l);
        OPCODE  mulop = OPCODE_make_op(OPR_MPY, OPCODE_rtype(lop), MTYPE_V);
        WN_Delete(l);
        WN_kid0(end) = ll;
        WN_kid1(end) = LWN_CreateExp2(mulop, r, lr);
        LWN_Set_Parent(WN_kid0(end), end);
        LWN_Set_Parent(WN_kid1(end), end);
      }
      else
        break;
    }

    // try loops further in

    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (!dli->Is_Inner)
      SNL_Optimize_Bounds(WN_do_body(wn));
  }
  else {  
    for (INT i = 0; i < WN_kid_count(wn); i++)
      SNL_Optimize_Bounds(WN_kid(wn,i));
  }
}

void SNL_Optimize_Bounds(SNL_REGION region)
{
  for (WN* wn = region.First; wn; wn = wn==region.Last ? NULL : WN_next(wn))
    SNL_Optimize_Bounds(wn);
}

WN*& SNL_UBexp(WN* snl_end, BOOL* ne)
{
  OPERATOR opr = WN_operator(snl_end);

  switch (opr) {
   default:
    FmtAssert(0, ("Bad op %d for SNL_UBexp", WN_opcode(snl_end)));
   case OPR_GE:
    if (ne) *ne = FALSE;
    FmtAssert(WN_operator(WN_kid1(snl_end)) == OPR_LDID, 
      ("Does not have LDID on opposite side of WN_end")); 
    return WN_kid0(snl_end);
   case OPR_GT:
    if (ne) *ne = TRUE;
    FmtAssert(WN_operator(WN_kid1(snl_end)) == OPR_LDID, 
      ("Does not have LDID on opposite side of WN_end")); 
    return WN_kid0(snl_end);
   case OPR_LE:
    if (ne) *ne = FALSE;
    FmtAssert(WN_operator(WN_kid0(snl_end)) == OPR_LDID, 
      ("Does not have LDID on opposite side of WN_end")); 
    return WN_kid1(snl_end);
   case OPR_LT:
    if (ne) *ne = TRUE;
    FmtAssert(WN_operator(WN_kid0(snl_end)) == OPR_LDID, 
      ("Does not have LDID on opposite side of WN_end")); 
    return WN_kid1(snl_end);
  }
}

WN*& SNL_UBvar(WN* snl_end)
{
  OPERATOR opr = WN_operator(snl_end);

  switch (opr) {
   default:
    FmtAssert(0, ("Bad op %d for SNL_UBvar", WN_opcode(snl_end)));
   case OPR_LE:
   case OPR_LT:
    return WN_kid0(snl_end);
   case OPR_GE:
   case OPR_GT:
    return WN_kid1(snl_end);
  }
}

extern WN* SNL_Get_Inner_Snl_Loop(WN* outer, INT nloops)
{
  Is_True(nloops > 0, ("Bad nloops for SNL_Get_Inner_Snl_Loop()"));
  WN* wn = outer;
  for (INT curnloops = nloops; curnloops > 1; curnloops--) {
    for (wn = WN_first(WN_do_body(wn)); wn; wn = WN_next(wn))
#ifndef KEY      
      if (WN_opcode(wn) == OPC_DO_LOOP || WN_opcode(wn) == OPC_REGION)
        break;
    FmtAssert(wn != NULL, ("Sequencing error"));
    if (WN_opcode(wn) == OPC_REGION) {
       for (wn = WN_first(WN_region_body(wn)); wn; wn = WN_next(wn)) 
         if (WN_opcode(wn) == OPC_DO_LOOP)
           break;
    }
#else //KEY Bug 10700
    {
     if (WN_opcode(wn) == OPC_REGION) {
       for (WN *tmp = WN_first(WN_region_body(wn)); tmp; tmp = WN_next(tmp))
         if (WN_opcode(tmp) == OPC_DO_LOOP){
           wn = tmp;
           break;
         }
      } // end if region
       if (WN_opcode(wn) == OPC_DO_LOOP)
        break;
     }
#endif
    FmtAssert(wn, ("SNL_Get_Inner_Snl_Loop() called with bad parameters"));
  }
  return wn;
}

extern BOOL SNL_Is_Non_Varying_Access_Array(ACCESS_ARRAY* aa,
                                            INT outer_depth)
{
  for (INT dim = 0; dim < aa->Num_Vec(); dim++) {
    ACCESS_VECTOR* av = aa->Dim(dim);
    if (av->Too_Messy ||
        av->Contains_Non_Lin_Symb() ||
        av->Non_Const_Loops() > outer_depth) {
      return FALSE;
    }
  }
  return TRUE;
}

typedef enum {
  RELOP_LT,
  RELOP_LE,
  RELOP_GT,
  RELOP_GE,
  RELOP_EQ,
  RELOP_NE,
  ARITH_SUB,
  ARITH_ADD,
  NULL_OPR
} NT_OPR;

typedef enum {
  EXPR_TERM,
  EXPR_VAL,
  EXPR_SYM,
  EXPR_OPR,
  EXPR_EMPTY
} SNL_STACK_ATTR;

typedef struct SNL_EXPR_STACK_INFO_s {
  SNL_STACK_ATTR attr;
  SYMBOL         var;
  INT            val;
  INT            depth;
  NT_OPR         opr;
  WN*            wn;
} SNL_EXPR_STACK_INFO;

INT SNL_Evaluate_ExprNT(SNL_EXPR_STACK_INFO *if_stack,
                        UINT8 if_stack_size,
                        UINT8 i,
                        UINT8 key_idx,
                        UINT8 seed,
                        SNL_EXPR_STACK_INFO *sub_expr_stack,
                        UINT8 sub_stack_size,
                        UINT8 j);

INT SNL_Evaluate_ExprTerm(SNL_EXPR_STACK_INFO *if_stack,
                          UINT8 if_stack_size,
                          UINT8 i,
                          UINT8 key_idx,
                          UINT8 seed,
                          SNL_EXPR_STACK_INFO *sub_expr_stack,
                          UINT8 sub_stack_size,
                          UINT8 j)
{
  INT rhs, lhs, val;

  switch(if_stack[i].opr) {
  case RELOP_GT:
  case RELOP_GE:
  case RELOP_LT:
  case RELOP_LE:
  case RELOP_EQ:
  case RELOP_NE:
    // Bin op: lhs
    lhs = SNL_Evaluate_ExprNT(if_stack,
                              if_stack_size,
                              i+1,
                              key_idx,
                              seed,
                              sub_expr_stack,
                              sub_stack_size,
                              j);
    // Bin op: rhs
    rhs = SNL_Evaluate_ExprNT(if_stack,
                              if_stack_size,
                              i+2,
                              key_idx,
                              seed,
                              sub_expr_stack,
                              sub_stack_size,
                              j);

    // Now evaluate the expression.
    switch(if_stack[i].opr) {
    case RELOP_GT:
      val = (lhs > rhs);
      break;

    case RELOP_GE:
      val = (lhs >= rhs);
      break;

    case RELOP_LT:
      val = (lhs < rhs);
      break;

    case RELOP_LE:
      val = (lhs <= rhs);
      break;

    case RELOP_EQ:
      val = (lhs == rhs);
      break;

    case RELOP_NE:
      val = (lhs != rhs);
      break;
    }
    break;

  case ARITH_ADD:
  case ARITH_SUB:
    // Bin op: lhs
    lhs = SNL_Evaluate_ExprNT(if_stack,
                              if_stack_size,
                              i+1,
                              key_idx,
                              seed,
                              sub_expr_stack,
                              sub_stack_size,
                              j);
    // Bin op: rhs
    rhs = SNL_Evaluate_ExprNT(if_stack,
                              if_stack_size,
                              i+2,
                              key_idx,
                              seed,
                              sub_expr_stack,
                              sub_stack_size,
                              j);

     
    if (if_stack[i].opr == ARITH_ADD)
      val = lhs + rhs;  
    else if (if_stack[i].opr == ARITH_ADD)
      val = lhs - rhs;  

    break;

  default:
    val = 1;
    break;
  }

  return val;
}

INT SNL_Evaluate_ExprNT(SNL_EXPR_STACK_INFO *if_stack,
                        UINT8 if_stack_size,
                        UINT8 i,
                        UINT8 key_idx,
                        UINT8 seed,
                        SNL_EXPR_STACK_INFO *sub_expr_stack,
                        UINT8 sub_stack_size,
                        UINT8 j)
{
  INT val = 0;

  switch(if_stack[i].attr) {
  case EXPR_VAL:
    val = if_stack[i].val;
    break;

  case EXPR_SYM:
    if (key_idx == i) {
      // Here is where we substitute the induction variable
      // with the evaluation of the given sub expression.
      val = SNL_Evaluate_ExprTerm(sub_expr_stack,
                                  sub_stack_size,
                                  j,
                                  -1,
                                  seed,
                                  sub_expr_stack,
                                  sub_stack_size,
                                  j);
    } else {
      // For expressions where we will substitute via a subexpression,
      // the symbol values are 1, else we use the seed value.
      if (key_idx == -2)
        val = seed;
      else
        val = 1;
    }
    break;
  case EXPR_TERM:
    val = SNL_Evaluate_ExprTerm(if_stack,
                                if_stack_size,
                                i,
                                key_idx,
                                seed,
                                sub_expr_stack,
                                sub_stack_size,
                                j);
    break;
  default:
    break;
  }

  return val;
}

// This works like a recursive decent parser.
BOOL SNL_Evaluate_ExprStack(SNL_EXPR_STACK_INFO *if_stack,
                            UINT8 if_stack_size, 
                            UINT8 key_idx,
                            UINT8 seed,
                            UINT8 num_vars,
                            SNL_EXPR_STACK_INFO *sub_expr_stack,
                            UINT8 sub_stack_size)
{
  UINT8 i, j;
  INT val;
  
  // Only solve for single variable solutions, we are substituting for 
  // the induction variable.
  if (num_vars > 1) {
    return FALSE;
  }

  i = 0;
  j = 0;
  val = 1;
  if (if_stack[i].attr == EXPR_OPR) {
    // There will only be 1 RELOP followed by a lhs and rhs
    val = SNL_Evaluate_ExprTerm(if_stack,
                                if_stack_size,
                                i,
                                key_idx,
                                seed,
                                sub_expr_stack,
                                sub_stack_size,
                                j);
  }
  return (val == 0);
}

SNL_STACK_ATTR configure_expr_wn(WN *wn,
                                 UINT8 *num_syms, 
                                 NT_OPR *opr, 
                                 INT *val, 
                                 SYMBOL *sym)
{
  SNL_STACK_ATTR attr;
  INT cur_syms = *num_syms;

  *opr = NULL_OPR;
  switch (WN_operator(wn)) {
  case OPR_LDID:
     attr = EXPR_SYM;
     cur_syms++;
     *num_syms = cur_syms;
     *sym = SYMBOL(wn);
     break;
  case OPR_GT:
     *opr = RELOP_GT;
     attr = EXPR_OPR;
     break;
  case OPR_GE:
     *opr = RELOP_GE;
     attr = EXPR_OPR;
     break;
  case OPR_LT:
     *opr = RELOP_LT;
     attr = EXPR_OPR;
     break;
  case OPR_LE:
     *opr = RELOP_LE;
     attr = EXPR_OPR;
     break;
  case OPR_EQ:
     *opr = RELOP_EQ;
     attr = EXPR_OPR;
     break;
  case OPR_NE:
     *opr = RELOP_NE;
     attr = EXPR_OPR;
     break;
  case OPR_ADD:
     attr = EXPR_TERM;
     *opr = ARITH_ADD;
     break;
  case OPR_SUB:
     attr = EXPR_TERM;
     *opr = ARITH_SUB;
     break;
  case OPR_INTCONST:
     attr = EXPR_VAL;
     *val = WN_const_val(wn);
     break;
  default:
     attr = EXPR_EMPTY;
     break;
  }

  return attr;
}

SNL_EXPR_STACK_INFO *SNL_Walk_ExpTree(WN *expr, 
                                      UINT8 *num_syms,
                                      UINT8 *expr_depth,
                                      UINT8 *stack_idx )
{
  SNL_EXPR_STACK_INFO *cur_stack;
  UINT8 i, j;
  UINT8 depth = *expr_depth;
  INT num_relops = 0;
  INT idx = *stack_idx;
  BOOL simple_exp = TRUE;

  cur_stack = CXX_NEW_ARRAY(SNL_EXPR_STACK_INFO, 20, &LNO_local_pool);

  // First we add the expr node itself
  // load expr stack
  cur_stack[idx].attr = 
    configure_expr_wn(expr, 
                      num_syms,
                      &cur_stack[idx].opr,
                      &cur_stack[idx].val,
                      &cur_stack[idx].var);
  cur_stack[idx].depth = 0;
  if (cur_stack[idx].attr == EXPR_OPR)
    num_relops++;

  cur_stack[idx++].wn = expr;

  // Now walk the sub tree
  for (i = 0; i < WN_kid_count(expr); i++) {
    WN *d1_wn = WN_kid(expr,i);

    // load expr stack
    cur_stack[idx].attr = 
      configure_expr_wn(d1_wn, 
                        num_syms,
                        &cur_stack[idx].opr,
                        &cur_stack[idx].val,
                        &cur_stack[idx].var);
    cur_stack[idx].depth = 1;
    if (cur_stack[idx].attr == EXPR_OPR)
      num_relops++;

    cur_stack[idx++].wn = d1_wn;

    // Record depth
    if (depth == 0) depth++;

    // Now walk the inner sub tree
    for (j = 0; j < WN_kid_count(d1_wn); j++) {
      WN *d2_wn = WN_kid(d1_wn,j);
 
      // load expr stack
      cur_stack[idx].attr = 
        configure_expr_wn(d2_wn, 
                          num_syms,
                          &cur_stack[idx].opr, 
                          &cur_stack[idx].val, 
                          &cur_stack[idx].var);
      cur_stack[idx].depth = 2;
      if (cur_stack[idx].attr == EXPR_OPR)
        num_relops++;

      cur_stack[idx++].wn = d2_wn;

      // record depth
      if (depth == 1) depth++;

      // depth limit is 2
      if (WN_kid_count(d2_wn) > 0) {
        simple_exp = FALSE;
      }
    }
  }
  *expr_depth = depth;
  *stack_idx = idx;

  // For now do not support compound expressions. 
  if (num_relops > 1)
    simple_exp = FALSE;

  // Now check the result to see if any unmapped WN's were seen
  for (i = 0; i < idx; i++) {
    if (cur_stack[i].attr == EXPR_EMPTY) {
      simple_exp = FALSE; 
      break;
    }
  }

  if (simple_exp == FALSE) {
    CXX_DELETE_ARRAY(cur_stack, &LNO_local_pool);
    cur_stack = NULL;
  }

  return cur_stack;
}

//
// A return state of TRUE means one of:
//
//  a.) Either the if_test or the loop_test has a non trivial expression
//  b.) The depth of if_test is not equivalent to loop_test's depth
//  c.) The expression was still consistent when evaluated.
//
// A return state of FALSE means the evaluated expression with
// the applied constraint of first or last iteration exclusion produced
// an inconsistent expression.
//
extern BOOL SNL_Compare_Logic(WN *bound_exp, 
                        WN *bound_var, 
                        WN *loop,
                        WN *stmt, 
                        BOOL is_last)
{
  SNL_EXPR_STACK_INFO *if_expr_stack;
  UINT8 if_stack_idx = 0;
  UINT8 num_if_syms = 0;
  UINT8 induc_loc;
  WN *UB_wn =  bound_exp;
  WN *UB_var = bound_var;
  WN *LB_wn =  WN_kid0(WN_start(loop));
  WN *induc = WN_index(loop);
  SYMBOL induction_var;
  BOOL stmt_contains_induc = FALSE;
  BOOL simple_exp = TRUE;
  UINT8 i, j;
  UINT8 if_test_depth = 0;

  // Now test the init value, it needs to be positive, the 
  // transformation test told us the the trip was positive and 
  // the stride was 1.
  if (WN_operator(LB_wn) == OPR_INTCONST) {
    if (WN_const_val(LB_wn) < 0)
      return TRUE;
  } else {
    return TRUE;
  }

  // Must have an induction var
  if (induc)
    induction_var = SYMBOL(induc);
  else 
    return TRUE;

  // First configure the attributes of the if_test tree, depth, size, 
  // and load the expr stack with the info about each wn in the tree.
  WN *if_test = WN_if_test(stmt);
  if (WN_else_is_empty(stmt) == FALSE)
    return TRUE;
  if_expr_stack = SNL_Walk_ExpTree(if_test, 
                                   &num_if_syms, 
                                   &if_test_depth,
                                   &if_stack_idx);
  if (if_expr_stack == NULL)
    return TRUE;

  // Now determine if the induction var is referenced 
  // in the if_test.
  for (i = 0; i < if_stack_idx; i++) {
    if (if_expr_stack[i].attr == EXPR_SYM) {
      if (if_expr_stack[i].var == induction_var) {
        induc_loc = i;
        stmt_contains_induc = TRUE;
        break;
      }
    }
  }

  // The if_test must contain a reference to the induction var
  if (stmt_contains_induc == FALSE) {
    CXX_DELETE_ARRAY(if_expr_stack, &LNO_local_pool);
    return TRUE;
  }

  // We do more work on checking the upper boundary test against the
  // if_test expr than we would for checking for a constant value
  // exclusion in an induction var comparison.
  if ((is_last) && (induc)) {
    SNL_EXPR_STACK_INFO *lc_expr_stack;
    UINT8 lc_stack_idx = 0;
    UINT8 num_lc_syms = 0;
    UINT8 loop_test_depth = 0;
    BOOL not_similar = FALSE;
    BOOL bound_includes = TRUE;
  
    // Now configure the attributes of the loop_test tree, depth, size, 
    // and load the expr stack with the info about each wn in the tree.
    WN *loop_test = WN_end(loop);
   
    // Does the upper bound test include the bound_exp or exclude it?
    // If it include it, we use the bound_exp unmodified later, else
    // if excluded, we alter the value by -1 to include the last iteration
    // in a copy of the expression.
    if (WN_operator(loop_test) == OPR_LT)
      bound_includes = FALSE;

    // Now build the loop test expression stack
    lc_expr_stack = SNL_Walk_ExpTree(loop_test, 
                                     &num_lc_syms, 
                                     &loop_test_depth,
                                     &lc_stack_idx);
    if (lc_expr_stack == NULL)
      return TRUE;

    // Trees are not equivalent, cannot compare
    if (if_test_depth != loop_test_depth) {
      CXX_DELETE_ARRAY(if_expr_stack, &LNO_local_pool);
      CXX_DELETE_ARRAY(lc_expr_stack, &LNO_local_pool);
      return TRUE;
    }

    // The first constraint is that the number of symbols
    // existing in the two expressions is the same
    if (num_if_syms == num_lc_syms) {
      // Since the induction var was seen, we need to ensure that 
      // for each var in the if test, there is a matching var
      // in the lc test.
      for (i = 0; i < if_stack_idx; i++) {
        SYMBOL if_sym;
        if (if_expr_stack[i].attr == EXPR_SYM) {
          BOOL found_sym = FALSE;
          if_sym = if_expr_stack[i].var; 
          for (j = 0; j < lc_stack_idx; j++) {
            if (lc_expr_stack[j].attr == EXPR_SYM) {
              if (if_sym == lc_expr_stack[j].var) {
                found_sym = TRUE;
                break;
              }
            }
          }

          // If the expressions are not similar we cannot compare
          if (found_sym == FALSE) {
            not_similar = TRUE;
            break;
          }
        }
      }

      // Now if the two expressions are similar, locate the induction
      // var in the if_test and reform the if_test expression stack
      // substituting the induction var with the bound_exp.
      if (not_similar == FALSE) {
        SNL_EXPR_STACK_INFO *bound_exp_stack;
        UINT8 bound_stack_idx = 0;
        UINT8 num_bound_syms = 0;
        UINT8 bound_depth = 0;
        BOOL is_consistent = TRUE;

        // Substitute the bound_exp by one which will include the last iter.
        if (bound_includes == FALSE) {
          bound_exp = LWN_Copy_Tree(bound_exp, FALSE, LNO_Info_Map);
          Increase_By(bound_exp, -1, bound_exp, 0);
        }

        // First form an expression stack with bound_exp
        bound_exp_stack = SNL_Walk_ExpTree(bound_exp, 
                                           &num_bound_syms, 
                                           &bound_depth,
                                           &bound_stack_idx);
        if (bound_exp_stack == NULL) {
          CXX_DELETE_ARRAY(if_expr_stack, &LNO_local_pool);
          CXX_DELETE_ARRAY(lc_expr_stack, &LNO_local_pool);
          return TRUE;
        }

        // Now there are only num_if_syms - 1 vars in the if_expr_stack,
        // evaluate the contents to see if it can logically
        // ever be true.  If not, we have made it inconsistent.
        if (SNL_Evaluate_ExprStack(if_expr_stack,
                                   if_stack_idx,
                                   induc_loc,
                                   0,
                                   num_if_syms - 1,
                                   bound_exp_stack,
                                   bound_stack_idx))
          is_consistent = FALSE;

        // Now clean up
        CXX_DELETE_ARRAY(bound_exp_stack, &LNO_local_pool);
        CXX_DELETE_ARRAY(if_expr_stack, &LNO_local_pool);
        CXX_DELETE_ARRAY(lc_expr_stack, &LNO_local_pool);

        // Now see if we made the if_test inconsistant
        if (!is_consistent)
          return FALSE;
      }
    }
  } else if (is_last == FALSE) {
    BOOL is_consistent = TRUE;
    INT seed = WN_const_val(LB_wn); 
    // Detect first iteration exclusion.  Note: This is greatly similar
    // to what is needed for loop splitting, perhaps this test can be used
    // to find those cases.
    if (SNL_Evaluate_ExprStack(if_expr_stack,
                               if_stack_idx,
                               -2,
                               seed,
                               num_if_syms,
                               NULL,
                               0))
      is_consistent = FALSE;

    CXX_DELETE_ARRAY(if_expr_stack, &LNO_local_pool);
    // Now see if we made the if_test inconsistant
    if (!is_consistent)
      return FALSE;
  }

  return TRUE;
}

extern BOOL SNL_Is_Invariant(DOLOOP_STACK *stack,
                        INT d,
                        INT dd)
{
  DO_LOOP_INFO* dli_dd = Get_Do_Loop_Info(stack->Bottom_nth(dd));

  ACCESS_ARRAY* aalb = dli_dd->LB;
  ACCESS_ARRAY* aaub = dli_dd->UB;
  INT outer_depth = Do_Loop_Depth(stack->Bottom_nth(d));

  if (SNL_Is_Non_Varying_Access_Array(aalb, outer_depth)) {
    for (INT dimlb = aalb->Num_Vec() - 1; dimlb >= 0; dimlb--) {
      ACCESS_VECTOR* avlb = aalb->Dim(dimlb);
      if (avlb->Loop_Coeff(d))
        return FALSE;
    }
  } else if (!Is_Loop_Invariant_Exp(WN_start(stack->Bottom_nth(dd)),
      stack->Bottom_nth(d))) {
    return FALSE;
  } 

  if (SNL_Is_Non_Varying_Access_Array(aaub, outer_depth)) {
    for (INT dimub = aaub->Num_Vec() - 1; dimub >= 0; dimub--) {
      ACCESS_VECTOR* avub = aaub->Dim(dimub);
      if (avub->Loop_Coeff(d))
        return FALSE;
    }
  } else {
    WN* wn = UBexp(WN_end(stack->Bottom_nth(dd)));
    // Have to give up if loop is not in right form.  
    if (wn == NULL)
      return FALSE; 
    if (!Is_Loop_Invariant_Exp(wn, stack->Bottom_nth(d)))
      return FALSE;
  }

  return TRUE;
}

extern INT SNL_Loop_Count(WN* wn_snl)
{ 
  FmtAssert(WN_opcode(wn_snl) == OPC_DO_LOOP,
    ("SNL_Loop_Count: Expected a DO loop"));
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_snl);
  if (dli->Is_Inner)
    return 1;
  INT do_count = 0; 
  INT loop_count = 1;
  for (WN* wn = WN_first(WN_do_body(wn_snl)); wn != NULL; wn = WN_next(wn)) { 
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      do_count++; 
      if (do_count > 1)
        return 1;
      loop_count = SNL_Loop_Count(wn) + 1;
    } 
  } 
  return loop_count; 
} 

extern WN* SNL_Innermost_Do(WN* wn_outer)
{
  FmtAssert(WN_opcode(wn_outer) == OPC_DO_LOOP,
    ("SNL_Innermost_Do: Expected a DO loop"));
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_outer);
  if (dli->Is_Inner)
    return wn_outer; 
  INT do_count = 0;
  WN* wn_inner = NULL; 
  for (WN* wn = WN_first(WN_do_body(wn_outer)); wn != NULL; wn = WN_next(wn)) { 
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      do_count++; 
      if (do_count > 1)
	return wn_outer; 
      wn_inner = wn; 
    }
  }
  return SNL_Innermost_Do(wn_inner);
} 

//-----------------------------------------------------------------------
// NAME: Is_Inner_SNL 
// FUNCTION: Returns the number of loops in the inner SNL rooted at 'wn_loop',
//   if 'wn_loop' is indeed an inner SNL, otherwise returns 0.
//-----------------------------------------------------------------------

extern INT Is_Inner_SNL(WN* wn_loop)  
{
  switch (WN_opcode(wn_loop)) { 
  case OPC_DO_LOOP:
    {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
      if (dli->Is_Inner)
        return 1;
    }
    break; 
  case OPC_IF:
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    return 0;
  case OPC_BLOCK:
  case OPC_REGION:
    break;
  default: 
    return -1; 
  } 

  INT kid_depth = -1; 
  if (WN_opcode(wn_loop) == OPC_BLOCK) {  
    for (WN* wn = WN_first(wn_loop); wn != NULL; wn = WN_next(wn)) { 
      INT local_kid_depth = Is_Inner_SNL(wn);
      if (local_kid_depth == 0 || kid_depth != -1 && local_kid_depth != -1) 
        return 0;
      if (local_kid_depth > 0)
        kid_depth = local_kid_depth; 
    } 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_loop); i++) { 
      WN* wn = WN_kid(wn_loop, i); 
      INT local_kid_depth = Is_Inner_SNL(wn);
      if (local_kid_depth == 0 || kid_depth != -1 && local_kid_depth != -1) 
        return 0;
      if (local_kid_depth > 0)
        kid_depth = local_kid_depth; 
    }
  }
  return WN_opcode(wn_loop) == OPC_DO_LOOP ? kid_depth + 1 : kid_depth ; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Upper_Bound_Standardize 
// FUNCTION: For the SNL wth outermost loop 'wn_outer' consisting of
//   'nloops' loops, attempt to standardize the upper bounds of each
//   loop in that SNL.
//-----------------------------------------------------------------------

extern void SNL_Upper_Bound_Standardize(WN* wn_outer,
                                        INT nloops)
{
  INT outer_depth = Do_Loop_Depth(wn_outer);
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  for (INT i = outer_depth; i < outer_depth + nloops; i++) {
    WN* wn_loop = stack.Bottom_nth(i);
    Upper_Bound_Standardize(WN_end(wn_loop), TRUE);
  }
}

//-----------------------------------------------------------------------
// NAME: Need_Fix_Array_Deps_On_Index_Variable
// FUNCTION: Returns TRUE if the index variable for loop 'wn_loop' is 
//   aliased with an array element, FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Need_Fix_Array_Deps_On_Index_Variable(WN* wn_loop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  WN* wn_start = WN_start(wn_loop); 
  return dg->Get_Vertex(wn_start); 
} 
   
//-----------------------------------------------------------------------
// NAME: Can_Fix_Array_Deps_On_Index_Variable
// FUNCTION: Returns TRUE if it is possible to fix the index variable 
//   of 'wn_loop', which is assumed aliased with an array element.  
//   Returns FALSE otherwise.
// NOTE: If the index variable is aliased to the array element inside 
//   the loop it is not fixable, otherwise it is fixable.
//-----------------------------------------------------------------------

static BOOL Can_Fix_Array_Deps_On_Index_Variable(WN* wn_loop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  VINDEX16 v = dg->Get_Vertex(WN_start(wn_loop)); 
  EINDEX16 e = 0; 
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn = dg->Get_Wn(dg->Get_Source(e)); 
    if (Wn_Is_Inside(wn, wn_loop))
      return FALSE; 
  } 
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn = dg->Get_Wn(dg->Get_Sink(e)); 
    if (Wn_Is_Inside(wn, wn_loop))
      return FALSE; 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Fix_Array_Deps_On_Index_Variable
// FUNCTION: Fix the array dependences on the index variable of 'wn_loop'
//   by finalizing and renaming the index variable and updating array 
//   dependences appropriately.
//-----------------------------------------------------------------------

static BOOL Fix_Array_Deps_On_Index_Variable(WN* wn_loop)
{
  DU_MANAGER* du = Du_Mgr;  
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  Finalize_Index_Variable(wn_loop); 
  WN* wn_new = WN_next(wn_loop); 
  scalar_rename(WN_start(wn_loop)); 
  dg->Add_Vertex(wn_new); 
  EINDEX16 e = 0; 
  VINDEX16 v = dg->Get_Vertex(WN_start(wn_loop)); 
  VINDEX16 v1 = dg->Get_Vertex(wn_new); 
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    VINDEX16 v2 = dg->Get_Source(e);
    if (!dg->Add_Edge(v1, v2, Create_DEPV_ARRAY(dg->Depv_Array(e), 
        &LNO_default_pool))) { 
      LNO_Erase_Dg_From_Here_In(wn_new, dg); 
      LNO_Erase_Dg_From_Here_In(dg->Get_Wn(dg->Get_Source(e)), dg); 
      return FALSE; 
    } 
  } 
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    VINDEX16 v2 = dg->Get_Sink(e);
    if (!dg->Add_Edge(v2, v1, Create_DEPV_ARRAY(dg->Depv_Array(e), 
        &LNO_default_pool))) { 
      LNO_Erase_Dg_From_Here_In(wn_new, dg); 
      LNO_Erase_Dg_From_Here_In(dg->Get_Wn(dg->Get_Sink(e)), dg); 
      return FALSE; 
    } 
  }
  USE_LIST *use_list = du->Du_Get_Use(WN_start(wn_loop)); 
  const DU_NODE* node = NULL;
  USE_LIST_ITER iter(use_list);
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn = node->Wn(); 
    if (WN_operator(wn) == OPR_LDID 
	&& SYMBOL(wn) == SYMBOL(WN_start(wn_loop))) { 
      VINDEX16 v = dg->Get_Vertex(wn); 
      if (v != 0) { 
	while (e = dg->Get_Out_Edge(v))  
	  dg->Remove_Edge(e);    
	while (e = dg->Get_In_Edge(v))  
	  dg->Remove_Edge(e); 
	dg->Delete_Vertex(v); 
      } 
    } 
  }    
  v = dg->Get_Vertex(WN_start(wn_loop)); 
  while (e = dg->Get_Out_Edge(v))  
    dg->Remove_Edge(e);    
  while (e = dg->Get_In_Edge(v))  
    dg->Remove_Edge(e); 
  dg->Delete_Vertex(v); 
  v = dg->Get_Vertex(WN_step(wn_loop)); 
  while (e = dg->Get_Out_Edge(v))  
    dg->Remove_Edge(e);    
  while (e = dg->Get_In_Edge(v))  
    dg->Remove_Edge(e); 
  dg->Delete_Vertex(v); 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Fix_Array_Deps_On_Index_Variable
// FUNCTION: Fix all of the index variables in the SNL 'wn_outer' with  
//   'nloops' loops which are aliased to array elements, insofar as this 
//   is possible.  Return FALSE if we ran out of dependence edges while 
//   attempting this, TRUE otherwise. 
//-----------------------------------------------------------------------

extern BOOL SNL_Fix_Array_Deps_On_Index_Variable(WN* wn_outer,
						 INT nloops)
{ 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) { 
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      if (Need_Fix_Array_Deps_On_Index_Variable(wn)  
	  && Can_Fix_Array_Deps_On_Index_Variable(wn))  
	if (!Fix_Array_Deps_On_Index_Variable(wn))      
	  return FALSE; 
    } 
  } 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Next_SNL_Loop
// FUNCTION: Returns the next innermost loop in the SNL 'wn_outer', NULL
//   if there is none.
//-----------------------------------------------------------------------

extern WN* Next_SNL_Loop(WN* wn_outer)
{
  WN *wn;
  for (wn = WN_first(WN_do_body(wn_outer)); wn != NULL; wn = WN_next(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      break;
  return wn;
}
          
//-----------------------------------------------------------------------
// NAME: Permutation_To_Unimodular
// FUNCTION: Returns a unimodular matrix equivalent to the 'permutation'
//   of length 'nloops'.
//-----------------------------------------------------------------------

extern IMAT* Permutation_To_Unimodular(INT permutation[],
                                       INT nloops)
{
  IMAT* unimodular = CXX_NEW(IMAT(nloops, nloops, &LNO_local_pool),
    &LNO_local_pool);
  for (INT i = 0; i < nloops; i++)
    for (INT j = 0; j < nloops; j++)
      (*unimodular)(i,j) = j == permutation[i] ? 1 : 0;
  return unimodular;
}

//-----------------------------------------------------------------------
// NAME: Unimodular_To_Permutation
// FUNCTION: Returns a permutation equivalent to the 'unimodular' matrix,
//   if it is actually a permutation matrix.  Otherwise returns NULL.
//-----------------------------------------------------------------------

extern INT* Unimodular_To_Permutation(IMAT* unimodular)
{
  if (unimodular->Rows() != unimodular->Cols())
    return NULL;
  INT nloops = unimodular->Rows();
  INT* permutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);
  INT i;
  for (i = 0; i < nloops; i++)
    permutation[i] = -1;
  for (i = 0; i < nloops; i++) {
    BOOL local_index = -1;
    for (INT j = 0; j < nloops; j++) {
      if ((*unimodular)(i,j) != 0) {
        if (local_index == -1 && (*unimodular)(i,j) == 1) {
          local_index = j;
        } else {
          CXX_DELETE_ARRAY(permutation, &LNO_local_pool);
          return NULL;
        }
      }
    }
    permutation[i] = local_index;
  }
  if (!Is_Permutation_Vector(permutation, nloops)) {
    CXX_DELETE_ARRAY(permutation, &LNO_local_pool);
    return NULL;
  }
  return permutation;
}

//-----------------------------------------------------------------------
// SNL_TILE_INFO member functions
//-----------------------------------------------------------------------

SNL_TILE_INFO::SNL_TILE_INFO(INT nloops, INT strips,
                             const INT* iloop, const INT* stripsz,
                             const INT* striplevel,
                             SNL_INV_CACHE_BLOCK_REASON reason[],
                             MEM_POOL* pool)
: _pool(pool), _l(nloops, strips, pool), _k(Lcm(stripsz, strips)),
_kht(strips, nloops, pool), _rectangular(TRUE)
{
  _l.D_Zero();
  _kht.D_Zero();
  for (INT j = 0; j < strips; j++) {
    INT i = iloop[j];

    Is_True(i < nloops && i >= 0, ("strip specified loop outside of range"));
    Is_True(_k%stripsz[j] == 0, ("strip size is %d, _k=%d", stripsz[i], _k));
    _l(i,j) = stripsz[j];
    _kht(j,i) = _k/stripsz[j];

    _stripsz[j] = stripsz[j];
    _striplevel[j] = striplevel[j];
    _iloop[j] = iloop[j];
    _reason[j] = reason[j];
  }
}

void SNL_TILE_INFO::Print(FILE* f) const
{
  Is_True(_rectangular, ("Don't know how to print non-rectangular tile"));
  if (_rectangular) {
    fprintf(f, "tile info (strips=%d)", _l.Cols());
    for (INT j = 0; j < _l.Cols(); j++)
      fprintf(f, "<%d,%d>", _iloop[j], _stripsz[j]);
    fprintf(f, "\n");
    fprintf(f, "_k = %d\n", _k);
    fprintf(f, "_l matrix: \n");
    _l.Print(f);
    fprintf(f, "_kht matrix: \n");
    _kht.Print(f);
  }
}

//-----------------------------------------------------------------------
// Bounds info generation functions
//-----------------------------------------------------------------------

static WN* generate_tree_add(WN*                tree,
                             SYMBOL             symbol,
                             INT                coeff,
                             TYPE_ID            wtype,
                             WN*                alias_wn)
{
  // create wn for id and cast to correct type

  OPCODE op = OPCODE_make_op(OPR_LDID, symbol.Type, symbol.Type);
  TY_IDX    ty = Be_Type_Tbl(symbol.Type);
  WN*    ld;

  if (alias_wn)
    ld = LWN_CreateLdid(op, alias_wn);
  else
    // TODO OK: Only DO's have this, and we fix up DO information later,
    // so it's okay.
    ld = WN_CreateLdid(op, symbol.WN_Offset(), symbol.St(), ty);

  WN*    wn = LWN_Int_Type_Conversion(ld, wtype);

  // multiply by coefficient

  if (coeff != 1) {
    OPCODE mul_op = OPCODE_make_op(OPR_MPY, wtype, MTYPE_V);
    wn = LWN_CreateExp2(mul_op, wn, LWN_Make_Icon(wtype, coeff));
  }

  // add to previous tree

  if (tree) {
    OPCODE add = OPCODE_make_op(OPR_ADD, wtype, MTYPE_V);
    wn = LWN_CreateExp2(add, tree, wn);
  }

  // put in DU information, if any.

  DEF_LIST* def_list = alias_wn ? Du_Mgr->Ud_Get_Def(alias_wn) : NULL;

  if (def_list) {
    DEF_LIST_CONST_ITER iter(def_list);
    for (const DU_NODE* n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
      WN* def = n->Wn();
      Du_Mgr->Add_Def_Use(def, ld);
    }
    DEF_LIST* def_list2 = Du_Mgr->Ud_Get_Def(ld);
    FmtAssert(def_list2, ("Impossible missing deflist"));
    def_list2->Set_loop_stmt(def_list->Loop_stmt());
  }

  return wn;
}

extern WN* generate_tree_from_row(const mINT32*         m,
                                  SNL_TRANS_INDEX_DATA* td,
                                  INT64                 c,
                                  TYPE_ID               wtype,
                                  INT                   part)
{
  INT i;
  INT e = 0;

  WN* tree = c ? LWN_Make_Icon(wtype, c) : NULL;

  if (part & UCTILE_T) {
    for (i = 0; i < td->t_nloops; i++, e++) {
      if (m[e])
        tree = generate_tree_add(tree, td->tdata[i].symbol, m[e], wtype,
                                 td->tdata[i].alias_wn);
    }
  }
  if (part & UCTILE_I) {
    for (i = 0; i < td->i_nloops; i++, e++) {
      if (m[e])
        tree = generate_tree_add(tree, td->idata[i].post_symbol, m[e], wtype,
                                 td->idata[i].post_alias_wn);
    }
  }
  if (part & UCTILE_O) {
    for (i = 0; i < td->o_nloops; i++, e++) {
      if (m[e])
        tree = generate_tree_add(tree, td->odata[i].symbol, m[e], wtype,
                                 td->odata[i].alias_wn);
    }
  }

  return tree ? tree : LWN_Make_Icon(wtype, 0);
}

extern WN* generate_tree_from_bounds_info_row(const mINT32*     row,
                                              mINT64            con,
                                              BOOL              le,
                                              const SNL_BOUNDS_SYMBOL_LIST*vi)
{
  DU_MANAGER*                   du = Du_Mgr;

  INT                           i;
  const SNL_BOUNDS_SYMBOL_NODE* n;
  WN*                           tree = NULL;
  TYPE_ID                       wtype = MTYPE_V;
  SNL_BOUNDS_SYMBOL_CONST_ITER  avi1(vi);

  for (i = 0, n = avi1.First(); !avi1.Is_Empty(); i++, n = avi1.Next())
    if (row[i])
      wtype = Max_Wtype(wtype, n->Symbol.Type);

  SNL_BOUNDS_SYMBOL_CONST_ITER  avi2(vi);
  for (i = 0, n = avi2.First(); !avi2.Is_Empty(); i++, n = avi2.Next()) {
    if (row[i])
      tree = generate_tree_add(tree, n->Symbol, row[i], wtype, n->Alias_Wn);
  }

  FmtAssert(tree, ("Zero row in conditionals??"));

  OPCODE op = OPCODE_make_op(le ? OPR_LE : OPR_EQ, Boolean_type, wtype);
  tree = LWN_CreateExp2(op, tree, LWN_Make_Icon(wtype, con));
  return tree;
}

//-----------------------------------------------------------------------
// SNL_TRANS_INDEX_DATA member functions
//-----------------------------------------------------------------------

void SNL_TRANS_INDEX_DATA::Print(FILE* f) const
{
  INT i;

  fprintf(f, "SNL_TRANS_INDEX_DATA printout:\n");

  for (i = 0; i < t_nloops; i++)
    fprintf(f, "tdata %d: %s\n", i, tdata[i].symbol.Name());

  for (i = 0; i < o_nloops; i++)
    fprintf(f, "odata %d: %s\n", i, odata[i].symbol.Name());

  for (i = 0; i < i_nloops; i++) {
    fprintf(f, "idata %d: %s (renamed to %s) -> ",
            i, idata[i].pre_symbol.Name(), idata[i].post_symbol.Name());
    fprintf(f, " newcode:");
    Dump_WN(idata[i].newcode, f, 1, 0, 2);
    fprintf(f, " lbtest:");
    Dump_WN(idata[i].lbtest, f, 1, 0, 2);
    fprintf(f, " ubtest:");
    Dump_WN(idata[i].ubtest, f, 1, 0, 2);
  }
}

SNL_TRANS_INDEX_DATA::~SNL_TRANS_INDEX_DATA()
{
  INT i;
  for (i = 0; i < i_nloops; i++) {
    if (idata[i].newcode) {
      LWN_Delete_Tree(idata[i].newcode);
      idata[i].newcode = NULL;
    }
    if (idata[i].lbtest) {
      LWN_Delete_Tree(idata[i].lbtest);
      idata[i].lbtest = NULL;
    }
    if (idata[i].ubtest) {
      LWN_Delete_Tree(idata[i].ubtest);
      idata[i].ubtest = NULL;
    }
  }

  for (i = 0; i < i_nloops; i++)
    if (idata[i].post_alias_wn != NULL &&
        idata[i].pre_alias_wn != idata[i].post_alias_wn)
      WN_Delete(idata[i].post_alias_wn);
  for (i = 0; i < o_nloops; i++)
    WN_Delete(odata[i].alias_wn);

  if (tdata)
    CXX_DELETE_ARRAY(tdata, pool);
  if (idata)
    CXX_DELETE_ARRAY(idata, pool);
  if (odata)
    CXX_DELETE_ARRAY(odata, pool);
}

static INT ut_body_exp_pre(WN* wn, SNL_TRANS_INDEX_DATA* td)
{
  if (WN_operator(wn) == OPR_LDID) {
    for (INT d = 0; d < td->i_nloops; d++) {
      if (td->idata[d].pre_symbol == SYMBOL(wn)) {
        FmtAssert(td->idata[d].newcode, ("Missing newcode"));
        (void) Replace_Wnexp_With_Exp_Copy(wn, td->idata[d].newcode,
          Du_Mgr);
        return td->idata[d].max_used_depth;
      }
    }
    return -1;
  }
  else {
    INT rv = -1;

    for (INT kid = 0; kid < WN_kid_count(wn); kid++) {
      INT nrv = ut_body_exp_pre(WN_kid(wn,kid), td);
      if (rv < nrv)
        rv = nrv;
    }
    return rv;
  }
}

SNL_TRANS_INDEX_DATA::SNL_TRANS_INDEX_DATA(const IMAT*          u,
                                           const IMAT*          uinv,
                                           const IMAT*          kht,
                                           const SNL_BOUNDS_INFO*bi,
                                           DOLOOP_STACK*        stack,
                                           INT                  first_in_stack,
                                           SNL_TILE_INFO*       ti,
                                           MEM_POOL*            p)
{
  pool = p;

  INT v;

  // sanity checking
  if (u) {
    Is_True(uinv, ("if u supplied, then so should uinv"));
    Is_True(u->Rows() == u->Cols() &&
            uinv->Rows() == uinv->Cols() &&
            u->Rows() == uinv->Rows(), ("bad u, uinv dimensionality"));
    Is_True(kht == NULL || kht->Cols() == u->Rows(),
            ("u(%d,%d) yet kht(%d,%d)",
             u->Rows(), u->Cols(), kht->Rows(), kht->Cols()));
  }
  else {
    Is_True(kht, ("transformation required!"));
  }

  // compute and allocate members

  t_nloops = kht ? kht->Rows() : 0;
  i_nloops = kht ? kht->Cols() : u->Rows();
  o_nloops = bi->Bounds().Num_Vars() - i_nloops;

  tdata = t_nloops ? CXX_NEW_ARRAY(TDATA, t_nloops, p) : NULL;
  idata = i_nloops ? CXX_NEW_ARRAY(IDATA, i_nloops, p) : NULL;
  odata = o_nloops ? CXX_NEW_ARRAY(ODATA, o_nloops, p) : NULL;

  const SNL_BOUNDS_SYMBOL_LIST* sl = &bi->Var_Info();
  const SNL_BOUNDS_SYMBOL_NODE* sld = sl->Head();

  // step 1: fill in idata, pre transformation

  for (v = 0; v < i_nloops; v++) {
    FmtAssert(sld, ("Missing sld"));
    idata[v].pre_symbol = sld->Symbol;
    idata[v].pre_alias_wn = sld->Alias_Wn;
    sld = sld->Next();
  }

  // step 2: fill in odata

  for (v = 0; v < o_nloops; v++) {
    FmtAssert(sld, ("Missing sld"));
    odata[v].symbol = sld->Symbol;
    // these have a habit of disappearing, so copy and delete later
    OPCODE op = OPCODE_make_op(OPR_LDID, sld->Symbol.Type, sld->Symbol.Type);
    odata[v].alias_wn = LWN_CreateLdid(op, sld->Alias_Wn);
    DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(sld->Alias_Wn);
    if (deflist) {
      DEF_LIST_ITER iter(deflist);
      for(const DU_NODE *n=iter.First(); !iter.Is_Empty(); n=iter.Next()) {
        WN *def = (WN *)n->Wn();
        Du_Mgr->Add_Def_Use(def, odata[v].alias_wn);
      }
    }
    sld = sld->Next();
  }

  // step 3: fill in idata, post-transformation

  for (v = 0; v < i_nloops; v++) {
    idata[v].post_symbol = idata[v].pre_symbol;     // can be overwritten below 
    idata[v].post_alias_wn = idata[v].pre_alias_wn; // can be overwritten below
    idata[v].max_used_depth = -1;
    idata[v].newcode = NULL;
    idata[v].lbtest = NULL;
    idata[v].ubtest = NULL;
  }

  if (u) {
    for (v = 0; v < i_nloops; v++) {
      TYPE_ID   wtype = MTYPE_V;
      INT       first_non_zero = -1;

      for (INT j = 0; j < i_nloops; j++) {
        if ((*u)(v,j)) {
          wtype = Max_Wtype(wtype, idata[j].pre_symbol.Type);
          if (first_non_zero == -1)
            first_non_zero = j;
        }
      }

      Is_True(first_non_zero > -1 || wtype != MTYPE_V,
              ("Bad type for new index variable"));

      if (first_non_zero != v || wtype != idata[v].pre_symbol.Type) {
        // non-skew transformation or type change -> give a new name
        // make the name related to outermost variable name, anyway

        const INT  symsz = 64;
        char       buf[symsz+6];

        strcpy(buf, "$fake_");
        idata[first_non_zero].pre_symbol.Name(buf+6, symsz);
        idata[v].post_symbol = Create_Preg_Symbol(buf, wtype);
        idata[v].post_alias_wn = NULL;
      }
    }

    // Build temporary list of replacement nodes
    // Need to get all symbols for post before calling generate_tree ...
    // Note that the DU information is lacking.  That's why once the
    // post indices are made, we have to call Include_Post_Index().

    INT v;
    for (v = 0; v < i_nloops; v++) {
      idata[v].newcode = generate_tree_from_row(&(*uinv)(v,0), this, 0,
                                                idata[v].post_symbol.Type,
                                                UCTILE_I);
      INT i;
      for (i = i_nloops-1; i >= 0; i--) {
        if ((*uinv)(v,i)) {
          idata[v].max_used_depth = i;
          break;
        }
      }
      Is_True(i >= 0, ("Impossible u^(-1)"));
    }

    // for imperfect nests

    for (v = 0; v < i_nloops; v++) {
      WN*       loop = stack->Bottom_nth(first_in_stack+v);
      Upper_Bound_Standardize(WN_end(loop));
      WN*       lb = LWN_Copy_Tree(WN_kid0(WN_start(loop)), TRUE, LNO_Info_Map);      WN*       ub = LWN_Copy_Tree(SNL_UBexp(WN_end(loop)), TRUE, LNO_Info_Map);
      TYPE_ID   wtype = Do_Wtype(loop);
      OPCODE    opld = OPCODE_make_op(OPR_LDID, wtype, wtype);
      OPCODE    opeq = OPCODE_make_op(OPR_EQ, Boolean_type, wtype);

      WN*       ld1 = LWN_CreateLdid(opld, WN_step(loop));
      WN*       ld2 = LWN_CreateLdid(opld, WN_step(loop));
      SNL_Add_Du_To_Index_Ldid(loop, ld1, Du_Mgr, TRUE);
      SNL_Add_Du_To_Index_Ldid(loop, ld2, Du_Mgr, TRUE);
      idata[v].lbtest = LWN_CreateExp2(opeq, ld1, lb);
      idata[v].ubtest = LWN_CreateExp2(opeq, ld2, ub);
      ut_body_exp_pre(idata[v].lbtest, this);
      ut_body_exp_pre(idata[v].ubtest, this);
    }
  }

  // step 4: fill in tdata, which requires the post_symbol from step 3.

  for (v = 0; v < t_nloops; v++) {
    TYPE_ID type = MTYPE_V;
    for (INT j = 0; j < i_nloops; j++)
      if ((*kht)(v,j))
        type = Max_Wtype(type, idata[j].post_symbol.Type);
    Is_True(type != MTYPE_V, ("Bad type for new tile index variable"));

    char buffer[100];
    sprintf(buffer, "$tile.%d.%d", ti->Striplevel(v), ti->Iloop(v));
    tdata[v].symbol = Create_Preg_Symbol(buffer, type);
    tdata[v].alias_wn = NULL;   // no alias information for a preg
  }
}

// td can be null if we're not worried about updating the 'other' (e.g. N)
// variables.  Recursive means explore bodies as well.  only_in_nloops
// tells us to delete the defs from the system, essentially assuming the
// index var is only read and written within the loop.  Setting it to
// 1 tells us only to do that to the first depth of for loops within wn.
// Setting to 2 tells us to get all loops nested directly within any loop
// in wn.

static void Fix_Do_Du_Info_X(WN* wn, SNL_TRANS_INDEX_DATA* td,
                             BOOL recursive, WN* loops,
                             INT only_in_nloops)
{
  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_DO_LOOP && only_in_nloops-- > 0) {
    Du_Mgr->Remove_Def_From_System(WN_start(wn));
    Du_Mgr->Remove_Def_From_System(WN_step(wn));
  }

  if (opr == OPR_LDID) {
    SYMBOL s(wn);
    WN* loop = NULL;
    if (loops) {
      for (loop = loops; loop; loop = LWN_Get_Parent(loop)) {
        if (WN_opcode(loop) == OPC_DO_LOOP && s == SYMBOL(WN_index(loop)))
          break;
      }
    }
    else {
      WN* child = NULL;
      for (loop = wn; loop; loop = LWN_Get_Parent(loop)) {
        if (WN_opcode(loop) == OPC_DO_LOOP && s == SYMBOL(WN_index(loop))) {
          if (child == WN_start(loop))
            loop = NULL;  // for the lower bound, the value isn't defined here!
          break;
        }
        child = loop;
      }
    }
    if (loop) {
      // du information
      Du_Mgr->Remove_Use_From_System(wn);
      WN* def1 = WN_start(loop);
      WN* def2 = WN_step(loop);
      Du_Mgr->Add_Def_Use(def1, wn);
      Du_Mgr->Add_Def_Use(def2, wn);
      DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn);
      FmtAssert(dl, ("Missing deflist"));
      if (loops == NULL)
        dl->Set_loop_stmt(loop);
      else {
        dl->Set_loop_stmt(NULL);
        while (loop) {
          WN *w;
          for (w = wn; w && w != loop; w = LWN_Get_Parent(w))
            ;
          if (w == loop) {
            dl->Set_loop_stmt(loop);
            break;
          }
          do {
            loop = LWN_Get_Parent(loop);
          } while(loop && WN_opcode(loop) != OPC_DO_LOOP);
        }
      }

      // alias information.  This actually inserts alias information.
      // TODO: assuming Aliased() assigns ids.
      if (Aliased(Alias_Mgr, def1, def2) != SAME_LOCATION) {
        const INT bufsz = 32;
        char      buf[bufsz];
        FmtAssert(0, ("Bad defs for %s %s",
                      SYMBOL(def1).Name(buf, bufsz), SYMBOL(def2).Name()));
      }
      Copy_alias_info(Alias_Mgr, def1, wn);
    }
    else if (td) {
      INT i;
      for (i = 0; i < td->o_nloops; i++) {
        if (s == td->odata[i].symbol)
          break;
      }
      if (i < td->o_nloops) {
        DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(td->odata[i].alias_wn);
        DEF_LIST_ITER iter(deflist);
        for(const DU_NODE *n=iter.First(); !iter.Is_Empty(); n=iter.Next()) {
          WN *def = (WN *)n->Wn();
          Du_Mgr->Add_Def_Use(def, wn);
        }
        DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn);
        FmtAssert(dl, ("Missing deflist"));
        dl->Set_loop_stmt(loop);
      }
    }
  }

  if (opr == OPR_BLOCK) {
    if (recursive) {
      for (WN* w = WN_first(wn); w; w = WN_next(w))
        Fix_Do_Du_Info_X(w, td, recursive, loops, only_in_nloops);
    }
  }
  else {
    for (INT k = WN_kid_count(wn) - 1; k >= 0; k--)
      Fix_Do_Du_Info_X(WN_kid(wn,k), td, recursive, loops, only_in_nloops);
  }

  if (opr == OPR_DO_LOOP) {
    FmtAssert(Du_Mgr->Du_Get_Use(WN_start(wn)),
              ("Didn't find uses for WN_index=%p", WN_index(wn)));
    FmtAssert(Du_Mgr->Du_Get_Use(WN_step(wn)),
              ("Didn't find uses for WN_step=%p", WN_step(wn)));
  }
}

// Fix_Do_Du_Info:
//
//  wn:       The code we are fixing up.
//  td:       Pass in NULL to not fix up DU information for non-index
//            variables in the bounds.  Otherwise pass in the td.
//  recursive:If true, fix up information inside the do body as well,
//            and recursively inside that.  Otherwise, just operate on
//            the bounds of this wn.
//  loops:    If NULL, fix up all index variables that appear within
//            WN.  E.g. if inside wn there is a use of "i", and there is
//            some ancestor that is a DO "i", tnen make i's DU information
//            point to that only.  If not null, then it's not the ancestors
//            of the reference, but loops and its ancestors.  This is useful
//            when wn is not part of the PU but just some where we created,
//            but more typically, just pass in NULL.
//  only_in_nloops: How deep may we assume that the index variable is only
//            read and written within the loop.  E.g. 0 means never assume
//            that.  1 means assume that for wn only.  2 means assume that
//            for wn and every loop nested 1 inside wn ... .  Notice that
//            if this value is non-zero, then this code will delete
//            all DU information for each loop it sees, and then expect to
//            see every reference to that index variable and rebuild the
//            information.  If a DO with uses of the index only inside
//            is being renamed, then you pass in only_in_loops=1 so that
//            you remove any stale DU information and recompute it exactly.

extern void Fix_Do_Du_Info(WN* wn, SNL_TRANS_INDEX_DATA* td,
                           BOOL recursive, WN* loops,
                           INT  only_in_nloops)
{
  Fix_Do_Du_Info_X(wn, td, recursive, loops, only_in_nloops);
}

//-----------------------------------------------------------------------
// NAME: SNL_Rebuild_Access_Arrays
// FUNCTION: Rebuild the access arrays for the 'loop' and all enclosed
//   loops and array references, as well as renumbering the loops.
//-----------------------------------------------------------------------

extern void SNL_Rebuild_Access_Arrays(WN* wn_outerloop)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DOLOOP_STACK shortstack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(wn_outerloop), &shortstack);
  Renumber_Loops(wn_outerloop, wn_outerloop, dg);
  LNO_Build_Access(wn_outerloop, &shortstack, &LNO_default_pool);
}

