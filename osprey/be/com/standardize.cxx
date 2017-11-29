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



#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "stab.h"
#include "wn.h"
#include "config_targ.h"
#include "wn_simp.h"
#include "wio.h"
#include "targ_const.h"
#include "const.h"
#include "strtab.h"
#include "config.h"
#include "wn_util.h"
#include "fb_whirl.h"

/*-----------------------------------------------------------------------
// NAME: WN_Symbol_Count 
// FUNCTION: For the tree rooted at 'wn', return the number of nodes with 
//   the given 'symbol' and 'offset'.  
//---------------------------------------------------------------------*/

static INT WN_Symbol_Count(WN *wn,              
                           ST_IDX symbol, 
			   WN_OFFSET offset)
{
  INT k = 0; 
  INT rval = 0; 
  rval = (WN_operator(wn) == OPR_LDID &&
	  symbol == WN_st_idx(wn) && offset == WN_offset(wn)) 
    ? 1 : 0;
  for (k = 0; k < WN_kid_count(wn); k++)
    rval += WN_Symbol_Count(WN_kid(wn,k), symbol, offset);
  return rval;
}

/*-----------------------------------------------------------------------
// NAME: WN_Flip_Le_And_Ge 
// FUNCTION: Flip the left and right hand sides of an expression with 
//   OPR_GE, OPR_LE, OPR_GT, or OPR_LT as the operator of the expression 
//   root 'wn'.  
//---------------------------------------------------------------------*/

static void WN_Flip_Le_And_Ge(WN *wn)
{
  OPCODE    opc;
  OPERATOR  opr; 

  opc = WN_opcode(wn);
  opr = OPCODE_operator(opc);
  switch (opr) {
   case OPR_GE: opr = OPR_LE; break;
   case OPR_LE: opr = OPR_GE; break;
   case OPR_GT: opr = OPR_LT; break;
   case OPR_LT: opr = OPR_GT; break;
   default: FmtAssert(0, ("Bad call to Flip_Le_And_Ge")); break;
  }

  WN_set_opcode(wn, OPCODE_make_op(opr, OPCODE_rtype(opc), OPCODE_desc(opc)));
}

/*-----------------------------------------------------------------------
// NAME: WN_Solve_For 
// FUNCTION: For the expression 'wn_top', solve this in terms of the 
//   variable given by 'symbol' and 'offset'.  Return TRUE if we knew 
//   how to solve, FALSE otherwise. 
//---------------------------------------------------------------------*/

static BOOL WN_Solve_For(WN *wn_top, 
  		         ST_IDX symbol,
		         WN_OFFSET offset)
{
  BOOL ok = FALSE;
  BOOL use_ceil = FALSE;
  INT lcount = 0; 
  INT rcount = 0;
  INT v = 0; 
  INTRINSIC intrnceil = INTRINSIC_NONE;
  INTRINSIC intrnfloor = INTRINSIC_NONE;
  WN *l = NULL; 
  WN *r = NULL; 
  WN *wn0 = NULL; 
  WN *ll = NULL;  
  WN *lr = NULL;  
  OPCODE lopc; 
  OPCODE negop;
  OPCODE newopc; 
  OPCODE lropc;
  OPERATOR lopr; 
  TYPE_ID  type;
  OPERATOR opr_base; 
   
  opr_base = WN_operator(wn_top);
  FmtAssert(opr_base == OPR_GT || opr_base == OPR_LT || opr_base == OPR_LE
    || opr_base == OPR_GE, ("Solve_For() called with bad RELOP"));
 
  lcount = WN_Symbol_Count(WN_kid0(wn_top), symbol, offset);
  rcount = WN_Symbol_Count(WN_kid1(wn_top), symbol, offset);
  if (!(lcount == 1 && rcount == 0) && !(lcount == 0 && rcount == 1)) {
    return FALSE;
  }

  /* put variable on lhs for the moment */
  if (rcount) {
    WN_Flip_Le_And_Ge(wn_top);
    wn0 = WN_kid0(wn_top);
    WN_kid0(wn_top) = WN_kid1(wn_top);
    WN_kid1(wn_top) = wn0;
  }

  l = WN_kid0(wn_top);
  r = WN_kid1(wn_top);
  while (1) {
    /* 
     * invariant at this location: the index is somewhere on the left (l))
     * invariant at this location: l and r will be wn_top's kids
     */
  
    lopc = WN_opcode(l);
    lopr = OPCODE_operator(lopc);

    /* have we successfully solved for the index variable? */
    if (OPCODE_is_load(lopc)) {
      ok = TRUE;
      break;
    }

    if (lopr == OPR_NEG) {
      WN_Flip_Le_And_Ge(wn_top);
      type = WN_rtype(r);
      negop = OPCODE_make_op(OPR_NEG, type, MTYPE_V);
      r = WN_CreateExp1(negop, r);
      l = WN_kid0(l);
      continue;
    }

    /* A CVT of an expression containing the index varible
     * or any other single kid node is bad news at this point.
     */
    if (lopr == OPR_CVT || WN_kid_count(l) == 1)
      return FALSE;

    lcount = WN_Symbol_Count(WN_kid0(l), symbol, offset);
    rcount = WN_Symbol_Count(WN_kid1(l), symbol, offset);

    Is_True((lcount == 1 && rcount == 0) ||
            (lcount == 0 && rcount == 1),
            ("Impossible: Counts messed up %d %d", lcount, rcount));

    if (rcount) {
      if (lopr == OPR_SUB) {
        /* 
         * in order to commute below, must change sign and mul right size
         * through by -1.
	 */ 
        WN_Flip_Le_And_Ge(wn_top);
        type = WN_rtype(r);
        negop = OPCODE_make_op(OPR_NEG, type, MTYPE_V);
        r = WN_CreateExp1(negop, r);
      }
      else if (lopr != OPR_ADD && lopr != OPR_MPY) /* commutative */
        break;
      wn0 = WN_kid0(l);
      WN_kid0(l) = WN_kid1(l);
      WN_kid1(l) = wn0;
    }

    ll = WN_kid0(l);
    lr = WN_kid1(l);

    if (lopr == OPR_MPY) {
      type = OPCODE_rtype(lopc);

      /* the intrinsics */ 
      switch (type) {
       case MTYPE_I4:
        intrnceil = INTRN_I4DIVCEIL; intrnfloor = INTRN_I4DIVFLOOR; break;
       case MTYPE_I8:
        intrnceil = INTRN_I8DIVCEIL; intrnfloor = INTRN_I8DIVFLOOR; break;
       case MTYPE_U4:
        intrnceil = INTRN_U4DIVCEIL; intrnfloor = INTRN_U4DIVFLOOR; break;
       case MTYPE_U8:
        intrnceil = INTRN_U8DIVCEIL; intrnfloor = INTRN_U8DIVFLOOR; break;
       default:
        goto out;        /* escape before we change any code. */
      }

      /* 
       * rhs of mul must be a const so we know if we are dividing
       * through by a positive or negative.  Besides, it fits the
       * expected normalized pattern.
       */ 

      lropc = WN_opcode(lr);
      if (OPCODE_operator(lropc) != OPR_INTCONST)
        break;

      v = WN_const_val(lr);
      if (v < 0) {
        WN_Flip_Le_And_Ge(wn_top);
        WN_const_val(lr) = -v;
        negop = OPCODE_make_op(OPR_NEG, OPCODE_rtype(lropc), MTYPE_V);
        r = WN_CreateExp1(negop, r);
      }

      use_ceil = WN_operator(wn_top) == OPR_GE 
        || WN_operator(wn_top) == OPR_LT;
      newopc = OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V);

      r = WN_CreateExp2(newopc, r, lr);
      WN_intrinsic(r) = use_ceil ? intrnceil : intrnfloor;
      WN_Delete(l);
      l = ll;
    }
    else if (lopr == OPR_ADD || lopr == OPR_SUB) {
      WN_kid0(l) = r;
      WN_kid1(l) = lr;
      r = l;
      l = ll;
      WN_set_opcode(r, OPCODE_make_op(lopr == OPR_ADD ? OPR_SUB : OPR_ADD,
                                      OPCODE_rtype(lopc), OPCODE_desc(lopc)));
    }
    else
      return FALSE;
  }

 out:

  WN_kid0(wn_top) = l;
  WN_kid1(wn_top) = r;

  return ok;
}

/*-----------------------------------------------------------------------
// NAME: WN_Copy_Frequency_Tree 
// FUNCTION: Copy the frequency information from 'wn_from' to 'wn'.
//
// Hmm... This function looks like it puts feedback frequencies on
// expression nodes, which isn't useful and really isn't allowed
// (although it isn't known to cause problems at present). Why does
// this function need to be written this way? -- rkennedy
//---------------------------------------------------------------------*/

static void WN_Copy_Frequency_Tree(const WN *wn, const WN *wn_from)
{
  WN_ITER *wniter; 
  INT32 count = 0; 

  if (Cur_PU_Feedback) {
    count = WN_MAP32_Get(WN_MAP_FEEDBACK, wn_from);
    wniter = WN_WALK_TreeIter((WN *) wn );
    while (wniter) {
      WN *cur = wniter->wn;
      wniter = WN_WALK_TreeNext(wniter);
      WN_MAP32_Set(WN_MAP_FEEDBACK, cur, count);
    }
  }
}

/*-----------------------------------------------------------------------
// NAME: WN_Upper_Bound_Standardize 
// FUNCTION: For the do loop 'doloop' with upper bound 'ub', put that 
//   bound in standard form, if possible.  If 'ok_to_fail', return 
//   FALSE if we can't standardize the upper bound, otherwise assert. 
//   Return TRUE if we can standardize. 
//---------------------------------------------------------------------*/

extern "C" BOOL
WN_Upper_Bound_Standardize(WN   *doloop, 
			   WN   *ub, 
			   BOOL  ok_to_fail)
{
  OPCODE opc; 
  OPERATOR opr; 
  TYPE_ID desc; 
  OPCODE subop; 
  OPCODE intconst_opc; 
  WN *ub1 = NULL; 
  WN *wn_const = NULL;
  BOOL ok = FALSE; 
  WN *wn_tmp = NULL;  

  FmtAssert(WN_opcode(doloop) == OPC_DO_LOOP, ("Bad ub passed"));
  if (WN_operator(WN_end(doloop)) == OPR_GT) {
    WN_Flip_Le_And_Ge(ub); 
    wn_tmp = WN_kid0(ub); 
    WN_kid0(ub) = WN_kid1(ub); 
    WN_kid1(ub) = wn_tmp; 
  }
  if (WN_operator(WN_end(doloop)) == OPR_LT) {
    /* change i < b to i <= b-1 */
    desc = WN_desc(WN_end(doloop));
    WN_set_opcode(ub, OPCODE_make_op(OPR_LE, 
      WN_rtype(WN_end(doloop)), desc));
    subop = OPCODE_make_op(OPR_SUB, desc, MTYPE_V);
    intconst_opc = OPCODE_make_op(OPR_INTCONST, desc, MTYPE_V);
    wn_const = WN_CreateIntconst(intconst_opc, 1);
    ub1 = WN_CreateExp2(subop, WN_kid1(ub), wn_const); 
    ub1 = WN_Simplify_Tree(ub1); 
    WN_kid1(ub) = ub1;
    WN_Copy_Frequency_Tree(ub1, ub);
  }
  WN_kid1(WN_end(doloop)) = WN_Simplify_Tree(WN_kid1(WN_end(doloop))); 
  ok = WN_Solve_For(WN_end(doloop), WN_st_idx(WN_index(doloop)),
    WN_offset(WN_index(doloop)));
  opc = WN_opcode(ub = WN_end(doloop));
  opr = OPCODE_operator(opc);
#ifndef KEY
  FmtAssert(opr == OPR_LT || opr == OPR_LE,
            ("surprise operator %s returned from WN_Solve_For()",
             OPCODE_name(opc)));
#else
  FmtAssert(opr == OPR_GE || opr == OPR_LE,
            ("surprise operator %s returned from WN_Solve_For()",
             OPCODE_name(opc)));
#endif
  if (ok == FALSE) {
    FmtAssert(ok_to_fail, 
      ("Upper_Bound_Standardize() could not solve for induction variable")); 
    return FALSE;
  }
  WN_kid1(WN_end(doloop)) = WN_Simplify_Tree(WN_kid1(WN_end(doloop))); 
  return ok;
}

