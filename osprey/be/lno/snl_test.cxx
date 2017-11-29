/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// -*-C++-*-

/** $Revision: 1.9 $
*** $Date: 05/06/29 17:55:11-07:00 $
*** $Author: fchow@fluorspar.internal.keyresearch.com $
*** $Source: be/lno/SCCS/s.snl_test.cxx $
**/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_test_CXX      "snl_test.cxx"
const static char *rcs_id =   snl_test_CXX "$Revision: 1.9 $";

#include <sys/types.h>
#include <alloca.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "model.h"
#include "snl.h"
#include "lwn_util.h"
#include "wn_simp.h"
#include "opt_du.h"
#include "config_lno.h"
#include "config_targ.h"
#include "config_targ_opt.h"
#include "errors.h"
#include "erbe.h"
#include "erglob.h"
#include "tlog.h"
#include "lego.h" 
#include "fiz_fuse.h"
#include "array_bounds.h"
#include "small_trips.h" 
#include "config.h"
#include "split_tiles.h"

// Bug 6010: an upper bound for the number of nests in a procedure
//           to unroll outmost loop, to reduce memory requirement 
#ifdef KEY
#define MAX_NESTS_IN_FU 450
#endif
static void SNL_Optimize_Bounds_With_Access_Vectors(WN* wn_loop, 
					    	    DU_MANAGER* du);



//-----------------------------------------------------------------------
// NAME: Inner_Loop_Is_Trapezoidal
// FUNCTION: Returns TRUE if some loop inside the loop 'wn' is trapezoidal,
//   FALSE otherwise.
// NOTE: 'ni' is used to find what loops are inside 'wn'.
//-----------------------------------------------------------------------

static BOOL Inner_Loop_Is_Trapezoidal(WN* wn, SNL_NEST_INFO *ni)
{
  DOLOOP_STACK* stack = &(ni->Dostack());
  INT i;
  for (i = 0; i < stack->Elements(); i++)
    if (wn == stack->Bottom_nth(i))
      break;
  FmtAssert(i < stack->Elements(), ("Could not find loop in stack."));
  for (i++; i < stack->Elements(); i++)
    if (Loop_Is_Trapezoidal(stack->Bottom_nth(i), Array_Dependence_Graph, 
         Du_Mgr))
       return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Est_Num_Iters_Suspect
// FUNCTION: Returns TRUE if the estimated number of iterations given in
//   the DO_LOOP_INFOs of the loops in the 'stack' with entries from 0 to
//   'depth' are suspect, and the symbolic number of iterations should be
//   used instead.  Returns FALSE otherwise.
// NOTES: This is done because interchanging a pair of triangular loops
//   "i" and "j" where "i" is long and "j" is short may not result in
//   in short "j" outer loop and a long "i" inner loop.  For example,
//   consider:
//      do i = 1, 500, 1
//        do j = i, i + 2, 1
//   Also note that this code assumes that we interchange at most the
//   the two innermost loops when we have a general (as opposed to
//   invariant) nest.
//-----------------------------------------------------------------------

#define EST_CONSTANT    2

static BOOL Est_Num_Iters_Suspect(DOLOOP_STACK* stack)
{
  INT depth = Do_Depth(stack->Bottom_nth(stack->Elements() - 1)); 

  // If terms are already symbolic, they cannot be suspect.
  INT i;
  for (i = depth - 1; i <= depth; i++) {
    WN* wn_loop = stack->Bottom_nth(i);
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
    if (dli_loop->Est_Num_Iterations != LNO_Num_Iters)
      break;
  }
  if (i == depth + 1) {
    if (snl_debug >= 3)
      fprintf(TFile, "!SUSPECT: Values already symbolic\n");
    return FALSE;
  }

  // No reason to suspect invariant loops
  BOOL trapezoidal = FALSE;
  WN* wn_loop = stack->Bottom_nth(depth);
  DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
  ACCESS_ARRAY* array_LB = dli_loop->LB;
  for (i = 0; !trapezoidal && i < array_LB->Num_Vec(); i++)
    for (INT j = 0; !trapezoidal && j < depth; j++)
      if (array_LB->Dim(i)->Loop_Coeff(j) != 0)
        trapezoidal = TRUE;
  if (!trapezoidal) {
    ACCESS_ARRAY* array_UB = dli_loop->UB;
    for (i = 0; !trapezoidal && i < array_UB->Num_Vec(); i++)
      for (INT j = 0; !trapezoidal && j < depth; j++)
        if (array_UB->Dim(i)->Loop_Coeff(j) != 0)
          trapezoidal = TRUE;
    if (!trapezoidal) {
      if (snl_debug >= 3)
        fprintf(TFile, "!SUSPECT: Nest not trapezoidal\n");
      return FALSE;
    }
  }

  // Allow only constant outer bound loops
  WN* wn_outer_loop = stack->Bottom_nth(depth-1);
  DO_LOOP_INFO* dli_outer_loop = Get_Do_Loop_Info(wn_outer_loop);
  ACCESS_ARRAY* array_outer_LB = dli_outer_loop->LB;
  if (array_outer_LB->Num_Vec() != 1) {
    if (snl_debug >= 3)
      fprintf(TFile, "SUSPECT: Outer loop LB not unique\n");
    return TRUE;
  }
  if (array_outer_LB->Dim(0)->Contains_Non_Lin_Symb()
      || array_outer_LB->Dim(0)->Contains_Lin_Symb()) {
    if (snl_debug >= 3)
      fprintf(TFile, "SUSPECT: Outer loop LB contains symbol\n");
    return TRUE; 
  }
  INT prod = 0;
  INT j;
  for (j = 0; j < depth - 1; j++) {
    WN* wn_loop = stack->Bottom_nth(j);
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
    prod += dli_loop->Est_Num_Iterations
      * array_outer_LB->Dim(0)->Loop_Coeff(j);
  }
  INT outer_LB = prod - array_outer_LB->Dim(0)->Const_Offset;
  ACCESS_ARRAY* array_outer_UB = dli_outer_loop->UB;
  if (array_outer_UB->Num_Vec() != 1) {
    if (snl_debug >= 3)
      fprintf(TFile, "SUSPECT: Outer loop UB not unique\n");
    return TRUE;
  }
  if (array_outer_UB->Dim(0)->Contains_Non_Lin_Symb()
      || array_outer_UB->Dim(0)->Contains_Lin_Symb()) {
    if (snl_debug >= 3)
      fprintf(TFile, "SUSPECT: Outer loop UB contains symbol\n");
    return TRUE; 
  }
  prod = 0; 
  for (j = 0; j < depth - 1; j++) {
    WN* wn_loop = stack->Bottom_nth(j);
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
    prod += dli_loop->Est_Num_Iterations
      * array_outer_UB->Dim(0)->Loop_Coeff(j);
  }
  INT outer_UB = array_outer_UB->Dim(0)->Const_Offset - prod;
  if (outer_UB < outer_LB) {
    if (snl_debug >= 3)
      fprintf(TFile, "SUSPECT: Null range in outer loop\n");
    return TRUE; 
  }
 
  // Inner loop bounds must be of the form c1 * outer_loop_var + c2
  // where c1 and c2 are constant.

  // Suspect inner loops with linear or nonlinear terms.
  WN* wn_inner_loop = stack->Bottom_nth(depth);
  DO_LOOP_INFO* dli_inner_loop = Get_Do_Loop_Info(wn_inner_loop);
  ACCESS_ARRAY* array_inner_LB = dli_inner_loop->LB;
  ACCESS_ARRAY* array_inner_UB = dli_inner_loop->UB;
  for (i = 0; i < array_inner_LB->Num_Vec(); i++)
    if (array_inner_LB->Dim(i)->Contains_Non_Lin_Symb()
      || array_inner_LB->Dim(i)->Contains_Lin_Symb())
      break;
  if (i < array_inner_LB->Num_Vec()) {
    if (snl_debug >= 3)
      fprintf(TFile, "SUSPECT: Inner loop LB contains symbol\n");
    return TRUE;
  }
  for (i = 0; i < array_inner_UB->Num_Vec(); i++)
    if (array_inner_UB->Dim(i)->Contains_Non_Lin_Symb()
      || array_inner_UB->Dim(i)->Contains_Lin_Symb())
      break;
  if (i < array_inner_UB->Num_Vec()) {
    if (snl_debug >= 3)
      fprintf(TFile, "SUSPECT: Inner loop UB contains symbol\n");
    return TRUE;
  }

  // Develop lower and upper bounds for inner loop.
  BOOL found_min = FALSE;
  BOOL found_max = FALSE;
  INT min_value = 0;
  INT max_value = 0;
  for (i = 0; i < array_inner_LB->Num_Vec(); i++) {
    prod = 0;
    for (INT j = 0; j < depth - 1; j++) {
      WN* wn_loop = stack->Bottom_nth(j);
      DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
      prod += dli_loop->Est_Num_Iterations
        * array_inner_LB->Dim(i)->Loop_Coeff(j);
    }
    INT c0 = array_inner_LB->Dim(i)->Loop_Coeff(depth-1);
    INT c1 = array_inner_LB->Dim(i)->Loop_Coeff(depth);
    INT k0 = array_inner_LB->Dim(i)->Const_Offset;
    INT value = (k0 - c0 * outer_LB - prod) / c1;
    if (!found_min) {
      min_value = value;
      found_min = TRUE;
    } else if (value < min_value)
      min_value = value;
    value = (k0 - c0 * outer_UB - prod) / c1;
    if (value < min_value)
      min_value = value;
  }
  for (i = 0; i < array_inner_UB->Num_Vec(); i++) {
    INT prod = 0;
    for (INT j = 0; j < depth - 1; j++) {
      WN* wn_loop = stack->Bottom_nth(j);
      DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
      prod += dli_loop->Est_Num_Iterations
        * array_inner_UB->Dim(i)->Loop_Coeff(j);
    }
    INT c0 = array_inner_UB->Dim(i)->Loop_Coeff(depth-1);
    INT c1 = array_inner_UB->Dim(i)->Loop_Coeff(depth);
    INT k0 = array_inner_UB->Dim(i)->Const_Offset;
    INT value = (k0 - c0 * outer_UB - prod) / c1;
    if (!found_max) {
      max_value = value;
      found_max = TRUE;
    } else if (value > max_value)
      max_value = value;
    value = (k0 - c0 * outer_LB - prod) / c1;
    if (value > max_value)
      max_value = value;
  }

  if (snl_debug >= 3) {
    fprintf(TFile, "outer LB: %d\n", outer_LB);
    fprintf(TFile, "outer UB: %d\n", outer_UB);
    fprintf(TFile, "min value inner loop: %d\n", min_value);
    fprintf(TFile, "max value inner loop: %d\n", max_value);
  }

  INT est_iterations_inner = dli_inner_loop->Est_Num_Iterations;
  if (max_value - min_value > EST_CONSTANT * est_iterations_inner) {
    if (snl_debug >= 3)
      fprintf(TFile,
        "SUSPECT: Trapezoidal interchange does not preserve loop lengths\n");
    return TRUE;
  }
  if (snl_debug >= 3)
    fprintf(TFile,
        "!SUSPECT: Trapezoidal interchange preserves loop lengths\n");
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Find_Kernel_Stack_Nest_Traverse
// FUNCTION: Traverse the tree rooted at 'wn_tree' and push onto the 
//   'kernel_stack' the addresses of any loops which are not already 
//   on the stack and which are innermost SNLs with exactly 'loop_count' 
//   loops. 
//-----------------------------------------------------------------------

static void Find_Kernel_Stack_Nest_Traverse(WN* wn_tree,
                                            INT loop_count,
                                            DOLOOP_STACK* kernel_stack)
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Is_Inner && dli->Depth + 1 >= loop_count) {
      INT local_count = 0;
      WN *wn;
      for (wn = wn_tree; wn != NULL; wn = LWN_Get_Parent(wn)) {
        if (WN_opcode(wn) == OPC_DO_LOOP)
          local_count++;
        if (local_count == loop_count)
          break;
      }
      FmtAssert(wn != NULL,
        ("Find_Kernel_Stack_Nest_Traverse: Could not find loop"));
      if (SNL_Loop_Count(wn) == loop_count) {
        INT i;
        for (i = 0; i < kernel_stack->Elements(); i++)
          if (kernel_stack->Bottom_nth(i) == wn)
            break;
        if (i == kernel_stack->Elements())
          kernel_stack->Push(wn);
      }
      return;
    }
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Find_Kernel_Stack_Nest_Traverse(wn, loop_count, kernel_stack);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Find_Kernel_Stack_Nest_Traverse(WN_kid(wn_tree, i), loop_count,
        kernel_stack);
  }
}

//-----------------------------------------------------------------------
// NAME: Find_Kernel_Stack
// FUNCTION: Add to 'kernel_stack' all innermost SNLs in the 'rg_kernel' 
//   with exactly 'loop_count' loops.
//-----------------------------------------------------------------------

static void Find_Kernel_Stack(SNL_REGION* rg_kernel,
                              INT loop_count,
                              DOLOOP_STACK* kernel_stack)
{
  // FmtAssert(FALSE, ("Looking for test cases"));
  for (WN* wn = rg_kernel->First; wn != NULL; wn = WN_next(wn)) {
    Find_Kernel_Stack_Nest_Traverse(wn, loop_count, kernel_stack);
    if (wn == rg_kernel->Last)
      break;
  }
}

// 797054: Make the number of cache strips visible to SE_CT_New_Tile_Infos
// in sxlimit.cxx. SNL_TILE_INFO* t in SNL_INV_Transforms is sometimes NULL,
// and we may end up creating loop nests deeper than 15, which is the
// maximum allowed for dependence vectors (nenad, 2000/08/23).

INT Num_Cache_Strips;


static SNL_REGION Do_Automatic_Transformation(WN* wn,
                                              INT nloops,
                                              SNL_NEST_INFO* ni,
                                              BOOL* changed)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;

  *changed = FALSE;

  // For an equivalent nloops, we always prefer constant bounds.
  // TODO OK: not skewing.  So for now the deepest nest is the best, period.

  SNL_REGION region(wn,wn);
  BOOL trying_invariant = (ni->Nloops_Invariant() >= ni->Nloops_General());

  SNL_ANAL_INFO sinfo(ni, !trying_invariant, dg, &LNO_default_pool);
  if (snl_debug >= 3)
    ni->Print(TFile);
  if (snl_debug >= 2)
    sinfo.Print(TFile);

  SNL_DEP_MATRIX smat(sinfo.Body_Deps(), &LNO_default_pool);
  SNL_DEP_MATRIX smati(sinfo.Imperfect_Deps(), &LNO_default_pool);
  if (snl_debug >= 3) {
    sinfo.Print(TFile);

    fprintf(TFile, "summarized dependences: body\n");
    if (sinfo.Body_Deps().All_Stars())
      fprintf(TFile, "<all stars>\n");
    else
      smat.Print(TFile);

    fprintf(TFile, "summarized dependences: imperfect\n");
    if (sinfo.Imperfect_Deps().All_Stars())
      fprintf(TFile, "<all stars>\n");
    else
      smati.Print(TFile);
  }

  BOOL imperfect_stuff_is_blockable = FALSE;
  BOOL imperfect_stuff_is_distributable = 
    (sinfo.Above_Is_Distributable() && sinfo.Below_Is_Distributable());


  INT i;
  for (i = 0; i < smat.Nloops(); i++) {
    if (i < smat.Nloops()-1 &&
        smat.Is_Fully_Permutable(i, smat.Nloops()-1) &&
        !sinfo.Body_Deps().All_Stars()) {
      // if dependences are good, break.  The test is that the imperfect
      // stuff can be delt with by blocking or distribution.
      imperfect_stuff_is_blockable =
        (smati.Is_Fully_Permutable(i, smat.Nloops()-1) &&
         !sinfo.Imperfect_Deps().All_Stars());
      if (imperfect_stuff_is_blockable || imperfect_stuff_is_distributable)
        break;
    }
      
    INT depth = ni->Depth_Inner() - smat.Nloops() + 1 + i;
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(ni->Dostack().Bottom_nth(depth));

    // dependences are good, because pragmas say so
    if (dli->Blockable_Specification == smat.Nloops() - i) {
      // TODO: untested.  ugly warning message.  But worst of all,
      // after the transformation there will be some negative dependences.
      // Will that confuse CG?  Right here, shouldn't we remove bad
      // dependences?  All dependences?  What are the semantics, as far
      // as updating the dependence graph.  Probably, just to minimally
      // prune all dependences to be lexpos after transformation.
      // Could do that in snl_trans.cxx
      if ( LNO_Apply_Illegal_Transformation_Directives ) {
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_String_Advisory,
                     WN_Get_Linenum(ni->Dostack().Bottom_nth(depth)),
                     "BLOCK",        // TODO: better format??  Correct name??
                     "Seemingly illegal blocking being performed.");
        if (!imperfect_stuff_is_distributable)
          imperfect_stuff_is_blockable = TRUE;
        break;
      }
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String,
                   WN_Get_Linenum(ni->Dostack().Bottom_nth(depth)),
                   "BLOCK",        // TODO: better format??  Correct name??
                   "Seemingly illegal blocking pragma ignored");
    }
    
    // a permutation has been specified all the way to the innermost
    // loop.  We will pay attention if we are allowed to ignore illegal
    // transformations.  TODO OK: this skips pragmas that are illegal and
    // don't specify permutations all the way in.  Better would be to
    // see the legality further in.  Also, all the same issues as
    // directly above.
    if (dli->Permutation_Spec_Count > 0 
	&& dli->Permutation_Spec_Count == smat.Nloops() - i) {
      if ( LNO_Apply_Illegal_Transformation_Directives ) {
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_String_Advisory,
                     WN_Get_Linenum(ni->Dostack().Bottom_nth(depth)),
                     "INTERCHANGE",    // TODO: better format??  Correct name??
                     "Seemingly illegal permutation being performed.");
        if (!imperfect_stuff_is_distributable)
          imperfect_stuff_is_blockable = TRUE;
        break;
      }
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String,
                   WN_Get_Linenum(ni->Dostack().Bottom_nth(depth)),
                   "INTERCHANGE", // TODO: better format??  Correct name??
                   "Seemingly illegal permutation pragma ignored");
    }
    
    if (!LNO_Analysis)
      continue;

    // for the analysis file, say when a loop in an SNL cannot be
    // transformed because dependences are not fully permutable.
    // (DEPENDENCE_PROBLEMS problem-edge problem-edge ...) where
    // a problem-edge is ((lineno-of-ruined-loop lineno-of-ruined-loop ...)
    //                    edge-src-line edge-sink-line)

    if (i == 0)
      fprintf(LNO_Analysis, "    (DEPENDENCE_PROBLEMS");
    for (INT j = sinfo.Body_Deps().Bad_Deps().Lastidx(); j >= 0; j--) {
      if (sinfo.Body_Deps().Bad_Deps()[j].loop == i) {
        WN* src;
        WN* snk;
        EINDEX16 e = sinfo.Body_Deps().Bad_Deps()[j].e;
        if (e == 0) {
          src = NULL;
          snk = NULL;
          // TODO: zero edges only arise from a missing vertex, and I
          // thought we had a different message for bad_mem than
          // merely a DEPENDENCE_PROBLEMS.
          DevWarn("not actually a dependence problem.");
        }
        else {
          src = dg->Get_Wn(dg->Get_Source(e));
          snk = dg->Get_Wn(dg->Get_Sink(e));
          if (src == NULL || snk == NULL)
            DevWarn("Missing wn mapping for vertex");
        }

        fprintf(LNO_Analysis, " ((");
        for (INT ii = 0; ii <= i; ii++) {
          INT loop_depth = ni->Depth_Inner() - nloops + 1 + ii;
          WN* lp = ni->Dostack().Bottom_nth(loop_depth);
          fprintf(LNO_Analysis, "%s%d",
                  ii==0 ? "" : " ", Srcpos_To_Line(WN_Get_Linenum(lp)));
        }
        INT  srcline = src ? Srcpos_To_Line(LWN_Get_Linenum(src)) : -1;
        INT  snkline = src ? Srcpos_To_Line(LWN_Get_Linenum(snk)) : -1;
        fprintf(LNO_Analysis, ") %d %d)", srcline, snkline);
      }
    }

    if (!imperfect_stuff_is_distributable) {
      for (INT j = sinfo.Imperfect_Deps().Bad_Deps().Lastidx(); j >= 0; j--) {
        if (sinfo.Imperfect_Deps().Bad_Deps()[j].loop == i) {
          WN* src;
          WN* snk;
          EINDEX16 e = sinfo.Imperfect_Deps().Bad_Deps()[j].e;
          if (e == 0) {
            src = NULL;
            snk = NULL;
            // TODO: zero edges only arise from a missing vertex, and I
            // thought we had a different message for bad_mem than
            // merely a DEPENDENCE_PROBLEMS.
            DevWarn("not actually a dependence problem.");
          }
          else {
            src = dg->Get_Wn(dg->Get_Source(e));
            snk = dg->Get_Wn(dg->Get_Sink(e));
            if (src == NULL || snk == NULL)
              DevWarn("Missing wn mapping for vertex");
          }

          fprintf(LNO_Analysis, " ((");
          for (INT ii = 0; ii <= i; ii++) {
            INT loop_depth = ni->Depth_Inner() - nloops + 1 + ii;
            WN* lp = ni->Dostack().Bottom_nth(loop_depth);
            fprintf(LNO_Analysis, "%s%d",
                    ii==0 ? "" : " ", Srcpos_To_Line(WN_Get_Linenum(lp)));
          }
          INT  srcline = src ? Srcpos_To_Line(LWN_Get_Linenum(src)) : -1;
          INT  snkline = src ? Srcpos_To_Line(LWN_Get_Linenum(snk)) : -1;
          fprintf(LNO_Analysis, ") %d %d)", srcline, snkline);
        }
      }
    }
  }

  if (LNO_Analysis && i > 0)
    fprintf(LNO_Analysis, ")\n");

  if (smat.Nloops() - i < 2 || sinfo.Body_Deps().All_Stars() ||
      !(imperfect_stuff_is_distributable ||
        imperfect_stuff_is_blockable)) {
    if (LNO_Run_Oinvar) {
      WN* innerloop = ni->Dostack().Bottom_nth(ni->Depth_Inner());
      extern void Hoist_Outer_Invar(WN *wn_inner, INT num_loops,
					INT outer_reg_tile,BOOL can_tile);
      Hoist_Outer_Invar(innerloop, ni->Nloops_Invariant(),
				   ni->Nloops_Invariant(),FALSE);
    }
    SNL_DEBUG0(1, "Dependences are uncooperative---nest untransformable");
    if (LNO_Tlog) {
      WN* wn = ni->Dostack().Bottom_nth(ni->Depth_Inner()-(ni->Nloops()-1));
      char buffer[20];
      sprintf(buffer,"level=%d",ni->Nloops());
      SRCPOS srcpos=WN_Get_Linenum(wn);
      Generate_Tlog("LNO","snl", Srcpos_To_Line(srcpos),
                ST_name(WN_st(WN_index(wn))),
                buffer, "", "SNL_FAILURES generic dependence problem");
    }

    if (LNO_Analysis) {
      for (INT i = ni->Nloops() - 1; i >= 0; i--) {
        WN* wn = ni->Dostack().Bottom_nth(ni->Depth_Inner() - i);
        if (i == ni->Nloops() - 1)
	  fprintf(LNO_Analysis, "    (SNL_FAILURES  ");
	else
	  fprintf(LNO_Analysis, "                   ");
	fprintf(LNO_Analysis, "(%d \"generic dependence problem\")",
	        Srcpos_To_Line(WN_Get_Linenum(wn)));
	fprintf(LNO_Analysis, "%s\n", (i == 0) ? ")" : "");
      }
    }
    if (LNO_Verbose) {
      printf("Line %d: SNL nest not transformable\n",
        Srcpos_To_Line(WN_Get_Linenum(ni->Dostack().
          Bottom_nth(ni->Depth_Inner() - nloops + 1))));
    }
    if (!Valid_SNL_Region(region))
      DevWarn("Do_Automatic_Transformation: Invalid SNL_REGION [0x%p,0x%p]",
        region.First, region.Last);
    return region;
  }

  if (i) {
    FmtAssert(wn == ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1),
              ("Failed sanity check"));
    ni->Exclude_Outer_Loops(i);
    nloops = trying_invariant ? ni->Nloops_Invariant() : ni->Nloops_General();
    wn = ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1);
    FmtAssert(wn == ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1),
              ("Failed sanity check"));

    // note: ok, since the dependences computed above are conservative
    if (!trying_invariant && nloops <= ni->Nloops_Invariant())
      trying_invariant = TRUE;
  }

  BOOL* can_be_inner = CXX_NEW_ARRAY(BOOL, ni->Depth_Inner() + 1,
				     &LNO_default_pool);
  BOOL* can_be_unrolled = CXX_NEW_ARRAY(BOOL, ni->Depth_Inner() + 1,
				     &LNO_default_pool);

  WN* innerloop = ni->Dostack().Bottom_nth(ni->Depth_Inner());

  INT outermost_can_be_tiled = ni->Depth_Inner() - nloops + 1;
  INT outermost_can_be_inner;
  INT outermost_can_be_unrolled;

  BOOL can_interchange = LNO_Interchange;
  INT loop_count = 0; 
  for (WN* lp = innerloop; lp != NULL; lp = LWN_Get_Parent(lp)) {
    if (WN_opcode(lp) == OPC_DO_LOOP) {
      loop_count++; 
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(lp);
      if (dli->Cannot_Interchange)
        can_interchange = FALSE;
      if (loop_count >= nloops)
	break; 
    }
  }
  if (can_interchange)
    can_interchange = !Est_Num_Iters_Suspect(&(ni->Dostack())); 

  // TODO: note that for general nests, we can tile as much as we want
  // but interchange is only limited to the innermost two loops.  An improvement
  // would be to inform the model that in certain cases it can only choose
  // one loop to unroll.  Meanwhile, this works perfectly for variant loops
  // when the innermost two are the two that would be chosen for register
  // blocking and placing innermost.

  if (can_interchange) {
    if (trying_invariant) {
      outermost_can_be_inner = outermost_can_be_tiled;
      outermost_can_be_unrolled = outermost_can_be_tiled;
    }
    else {
      // Only allow unrolling of the second-to-the-innermost.  Also, allow
      // cache blocking of that loop as well if perfectly nested.

      if ((ni->Above_Is_Distributable() ||
           WN_prev_executable(innerloop) == NULL) &&
          (ni->Below_Is_Distributable() ||
           WN_next_executable(innerloop) == NULL)) 
        outermost_can_be_inner = ni->Depth_Inner() - 1;
      else
        outermost_can_be_inner = ni->Depth_Inner();
      outermost_can_be_unrolled = ni->Depth_Inner() - 1;
    }
  }
  else {
    outermost_can_be_inner = ni->Depth_Inner();
    if (trying_invariant)
      outermost_can_be_unrolled = outermost_can_be_tiled;
    else
      outermost_can_be_unrolled = ni->Depth_Inner() - 1;
  }

  for (i = 0; i <= ni->Depth_Inner(); i++) {
    can_be_inner[i] = (i >= outermost_can_be_inner);
    can_be_unrolled[i] = (i >= outermost_can_be_unrolled);
  }

  LOOP_MODEL lm;
  {
    HASH_TABLE<WN *,BIT_VECTOR *> htable(500,&LNO_local_pool);
    if (LNO_Run_Oinvar && trying_invariant) {
      extern void Mark_Invar(WN *region, INT num_loops, DOLOOP_STACK *do_stack,
	      HASH_TABLE<WN *,BIT_VECTOR *> *htable, MEM_POOL *pool, BOOL outer_only);

      DOLOOP_STACK *do_stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
					      &LNO_local_pool);
      Build_Doloop_Stack(innerloop,do_stack);
      Mark_Invar(WN_do_body(innerloop),nloops,do_stack,&htable,&LNO_local_pool,FALSE);
      extern BOOL Sort_Invar_Expressions(WN *wn, 
			HASH_TABLE<WN *,BIT_VECTOR *> *htable);
      if (Roundoff_Level >= ROUNDOFF_ASSOC &&
	  Sort_Invar_Expressions(innerloop,&htable)) {
        HASH_TABLE<WN *,BIT_VECTOR *> htable2(500,&LNO_local_pool);
        Mark_Invar(WN_do_body(innerloop),nloops,do_stack,
				&htable2,&LNO_local_pool,FALSE);
        lm.Model(innerloop, can_be_inner, can_be_unrolled,
  		outermost_can_be_tiled, 
		dg, &ni->Privatizability_Info(), nloops,&htable2);
      } else {
        lm.Model(innerloop, can_be_inner, can_be_unrolled,
  		outermost_can_be_tiled, 
		dg, &ni->Privatizability_Info(), nloops,&htable);
      }
    } else {
      lm.Model(innerloop, can_be_inner, can_be_unrolled,
  		outermost_can_be_tiled, 
		dg, &ni->Privatizability_Info(), nloops,NULL);
    }
  }

  CXX_DELETE_ARRAY(can_be_inner, &LNO_default_pool);
  CXX_DELETE_ARRAY(can_be_unrolled, &LNO_default_pool);

  INT outermost;
  for (outermost = 0; outermost < ni->Depth_Inner(); outermost++) {
    if (lm.New_Order(outermost) != outermost ||
	(lm.Block_Number(outermost) != 1 && !LNO_Outer_Unroll_Model_Only) ||
        (lm.Nstrips() != 0 && lm.Stripdepth() == outermost))
      break;
  }

  EST_REGISTER_USAGE reg_usage;
  reg_usage.Set_Est_Regs(lm.Num_Fp_Regs(), Target_FPRs,
			 lm.Num_Int_Regs(), Target_INTRs,
			 lm.Num_TLB(), Mhd.L[0].TLB_Entries);

  if (outermost >= ni->Depth_Inner()) {
    // the model likes things the way they are
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(innerloop);
    dli->Est_Register_Usage = reg_usage;
    if (LNO_Verbose) {
      printf("Line %d: SNL not transforming nest\n",
             Srcpos_To_Line(WN_Get_Linenum(ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1))));
    }

    // Run outer loop invariance algorithm
    if (LNO_Run_Oinvar) {
      extern void Hoist_Outer_Invar(WN *wn_inner, INT num_loops,
				INT outer_reg_tile, BOOL can_tile);
      Hoist_Outer_Invar(innerloop, ni->Nloops_Invariant(),
				   ni->Nloops_Invariant(),TRUE);
    }
    if (!Valid_SNL_Region(region))
      DevWarn("Do_Automatic_Transformation: Invalid SNL_REGION [0x%p,0x%p]",
        region.First, region.Last);
    if (!Valid_SNL_Region(region))
      DevWarn("Do_Automatic_Transformation: Invalid SNL_REGION [0x%p,0x%p]",
	region.First, region.Last);
    return region;
  }

  FmtAssert(outermost >= ni->Depth_Inner() - nloops + 1,
            ("Transforming outside nloops???"));

  if (outermost > ni->Depth_Inner() - nloops + 1) {
    INT i = outermost - ni->Depth_Inner() + nloops - 1; // exclude count
    FmtAssert(wn == ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1),
              ("Failed sanity check"));
    ni->Exclude_Outer_Loops(i);
    FmtAssert(nloops - i == (trying_invariant ? ni->Nloops_Invariant() : ni->Nloops_General()), ("Attempted transformation too deep?"));
    nloops = trying_invariant ? ni->Nloops_Invariant() : ni->Nloops_General();
    wn = ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1);

    // note: ok, since the dependences computed above are conservative
    if (!trying_invariant && nloops <= ni->Nloops_Invariant())
      trying_invariant = TRUE;
  }

  INT   order[SNL_MAX_LOOPS];  // original permutation order 
  INT   regstripsz[SNL_MAX_LOOPS]; // outer unroll factors
  BOOL  regstripping = FALSE; // actually doing outer unroll 
  BOOL  cachestripping = (lm.Nstrips() != 0); // actually doing cache block 
  BOOL  reordering = FALSE; // actually doing permutation 

  for (i = outermost; i <= ni->Depth_Inner(); i++) {
    order[i-outermost] = lm.New_Order(i)-outermost;
    if (lm.New_Order(i) != i)
      reordering = TRUE;
    WN* my_loop = ni->Dostack().Bottom_nth(i);
    if (!LNO_Outer_Unroll_Model_Only
        && (LNO_Trapezoidal_Outer_Unroll
        || !Inner_Loop_Is_Trapezoidal(my_loop, ni))) {
      regstripsz[i-outermost] = lm.Block_Number(order[i-outermost]+outermost);
      if (i-outermost < nloops-1 && regstripsz[i-outermost] != 1)
        regstripping = TRUE;
    }
    else
      regstripsz[i-outermost] = 1;
  }
  FmtAssert(ni->Depth_Inner() + 1 <= outermost + nloops,
	    ("Transformed loops outside of innermost nloops"));

  // There's a small problem with cache stripping.
  // With single-level blocking, the cache model arranges outer strips
  // (loops outside the middle loop) in source order.  In the invariant
  // case, it doesn't matter, but in the general case, it's good that the
  // strips come out ordered, since an implicit interchange of an imperfectly
  // nested loop might otherwise result.  But in multi-level blocking, there's
  // no guarantee about order of strips.  So we reorder them so that for
  // strip s, if there is a strip ss such that loop[ss] > loop[s], then
  // either ss > s or there is some sss such that sss>s and
  // loop[sss] == loop[ss].  That is, the rightmost occurance of loop[s]
  // is always left of the rightmost occurance of loop[ss] when
  // loop[ss] > loop[s].
  // This should prevent implicit interchanges.
  // Reordering strips doesn't affect L1 cache behavior.  It might hurt
  // L2 or further out cache behavior, though.  That's why we use an
  // algorithm that reorders as little as possible.  E.g. sorting would be
  // very bad.  Imagine transforming (primed loops are the strip loops)
  //               do j'' do i' do j' do i do j
  // to
  //               do i' do j'' do j' do i do j
  // That's very different!  So we don't do a gratuitous interchange.
  // In the above, if the source order was 'do i do j', then you can see
  // that all is well and we do no permutation of the strips.

  if (!trying_invariant) {
    UINT32   seen = 0;    // bitvector 1<<i set if loop i is placed rightmost

    for (INT s_cur = lm.Nstrips() - 1; s_cur > 0; s_cur--) {
      // Our goal is (FORALL s<s'), iloop[s] < iloop[s'], but if only if
      // there does not exist s''>s: iloop[s'']==iloop[s] and there does
      // not exists s'''>s': iloop[s''']==iloop[s'].  That is, the last
      // occurances of each iloop[s] must be in order.  That prevents
      // implicit interchanges.
      //   So the algorithm is this: start at the end.  Pick the value
      // we want there.  That's the last occurence of the largest iloop[s]
      // value.  Swap that down to here.  Now for s_cur one less, do the
      // same thing, skipping over larger values that exists beyond s_cur.
      // So, for example, if we have 2 1 2 0 2, we start with the last element.
      // The last 2 is indeed the largest value, and the rightmost occurence
      // of it.  Now we decrement s_cur, which puts us on the 0.  Looking left,
      // we see a 2, but that's not a rightmost 2, so we ignore it.  We then
      // see a 1.  That's promising.  We see another 2, not the rightmost.
      // The 1 is the largest legal one to move over, so we shift it over. Etc.

      if (seen & (1<<lm.Iloop()[s_cur]))
        continue;

      // look left for the biggest previously unseen loop number.
      INT s_biggest = s_cur;
      INT s;
      for (s = s_cur - 1; s >= 0; s--) {
        if (lm.Iloop()[s] > lm.Iloop()[s_biggest] &&
            (seen & (1 << lm.Iloop()[s])) == 0)
          s_biggest = s;
      }

      Is_True((seen & (1 << lm.Iloop()[s_biggest])) == 0, ("Bug"));
      seen |= (1 << lm.Iloop()[s_biggest]);

      if (s_biggest == s_cur)
        continue;

      // TODO: get rid of DevWarn: situation okay
      DevWarn("Unusual double blocking: ok, but verify with LNO group");

      // found, so copy from s_biggest to s_cur and shift the other data over.
      INT tmp_iloop = lm.Iloop()[s_cur];
      INT tmp_stripsz = lm.Stripsz()[s_cur];
      INT tmp_striplevel = lm.Striplevel()[s_cur];
      for (s = s_cur; s > s_biggest; s--) {
        lm.Iloop()[s] = lm.Iloop()[s-1];
        lm.Stripsz()[s] = lm.Stripsz()[s-1];
        lm.Striplevel()[s] = lm.Striplevel()[s-1];
      }
      lm.Iloop()[s_biggest] = tmp_iloop;
      lm.Stripsz()[s_biggest] = tmp_stripsz;
      lm.Striplevel()[s_biggest] = tmp_striplevel;
    }
  }

  // Likely that this code inside the if is redundant. 
  if (nloops != ni->Depth_Inner() + 1 - outermost) {
    FmtAssert(wn == ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1),
              ("Failed sanity check"));
    ni->Exclude_Outer_Loops(nloops - (ni->Depth_Inner() + 1 - outermost));
    nloops = ni->Depth_Inner() + 1 - outermost;
    wn = ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1);
    FmtAssert(wn == ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1),
              ("Failed sanity check"));
    // note: this is ok, since the dependences computed above are conservative
    if (!trying_invariant && nloops <= ni->Nloops_Invariant())
      trying_invariant = TRUE;
  }

  if (LNO_Verbose) {
    INT outerdepth = ni->Depth_Inner() + 1 - nloops;

    printf("Line %d:", Srcpos_To_Line(WN_Get_Linenum(ni->Dostack().Bottom_nth(outerdepth))));
    printf(" [");
    for (i = 0; i < nloops; i++) {
      INT depth = i + outerdepth;
      if (i > 0)
        printf(",");
      printf("%s", SYMBOL(WN_index(ni->Dostack().Bottom_nth(depth))).Name());
    }
    printf(" --> ");
    for (i = 0; i < nloops; i++) {
      INT depth1 = order[i] + outerdepth;
      if (i > 0)
        printf(",");
      printf("%s",
             SYMBOL(WN_index(ni->Dostack().Bottom_nth(depth1))).Name());
      if (regstripsz[i] > 1)
        printf("(u=%d)", regstripsz[i]);
    }
    if (cachestripping) {
      printf(" block%s ", lm.Nstrips() > 1 ? "s" : "");
      for (i = 0; i < lm.Nstrips(); i++) {
        INT depth = order[lm.Iloop(i) - outerdepth] + outerdepth;
        if (i > 0)
          printf(",");
        printf("%s(%d)[L%d]",
               SYMBOL(WN_index(ni->Dostack().Bottom_nth(depth))).Name(),
               lm.Stripsz(i), lm.Striplevel(i));
      }
      INT d = order[lm.Stripdepth() - outerdepth] + outerdepth;
      printf(" outside %s",
             SYMBOL(WN_index(ni->Dostack().Bottom_nth(d))).Name());
    }
    printf("]\n");
  }
  if (snl_debug >= 1) {
    INT outerdepth = ni->Depth_Inner() + 1 - nloops;

    fprintf(TFile, "Line %d:", Srcpos_To_Line(WN_Get_Linenum(ni->Dostack().Bottom_nth(outerdepth))));
    fprintf(TFile, " [");
    for (i = 0; i < nloops; i++) {
      INT depth = i + outerdepth;
      if (i > 0)
        fprintf(TFile, ",");
      fprintf(TFile,
              "%s", SYMBOL(WN_index(ni->Dostack().Bottom_nth(depth))).Name());
    }
    fprintf(TFile, " --> ");
    for (i = 0; i < nloops; i++) {
      INT depth1 = order[i] + outerdepth;
      if (i > 0)
        fprintf(TFile, ",");
      fprintf(TFile, "%s",
              SYMBOL(WN_index(ni->Dostack().Bottom_nth(depth1))).Name());
      if (regstripsz[i] > 1)
        fprintf(TFile, "(u=%d)", regstripsz[i]);
    }
    if (cachestripping) {
      fprintf(TFile, " block%s ", lm.Nstrips() > 1 ? "s" : "");
      for (i = 0; i < lm.Nstrips(); i++) {
        INT depth = order[lm.Iloop(i) - outerdepth] + outerdepth;
        if (i > 0)
          fprintf(TFile, ",");
        fprintf(TFile, "%s(%d)[L%d]",
                SYMBOL(WN_index(ni->Dostack().Bottom_nth(depth))).Name(),
                lm.Stripsz(i), lm.Striplevel(i));
      }
      INT d = order[lm.Stripdepth() - outerdepth] + outerdepth;
      fprintf(TFile, " outside %s",
              SYMBOL(WN_index(ni->Dostack().Bottom_nth(d))).Name());
    }
    fprintf(TFile, "]\n");
  }

  if (!reordering && !cachestripping && !regstripping) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(innerloop);
    dli->Est_Register_Usage = reg_usage;
    if (!Valid_SNL_Region(region))
      DevWarn("Do_Automatic_Transformation: Invalid SNL_REGION [0x%p,0x%p]",
	region.First, region.Last);
    return region;
  }

  // Put the ordering into orderp, regstripping into regstripszp

  INT* orderp = reordering ? order : NULL;
  INT* regstripszp = regstripping ? regstripsz : NULL;

  // Put the cache blocking into ti.

  Num_Cache_Strips = 0;
  SNL_TILE_INFO* ti = NULL;
  if (cachestripping) {
    SNL_INV_CACHE_BLOCK_REASON* reason 
      = CXX_NEW_ARRAY(SNL_INV_CACHE_BLOCK_REASON, lm.Nstrips(), 
        &LNO_default_pool);
    for (INT i = 0; i < lm.Nstrips(); i++) {
      lm.Iloop()[i] -= lm.Stripdepth();
      reason[i] = SNL_INV_TILE_ONLY; 
    }
    ti = CXX_NEW(SNL_TILE_INFO(ni->Depth_Inner() + 1 - lm.Stripdepth(),
                               lm.Nstrips(), lm.Iloop(), lm.Stripsz(),
			       lm.Striplevel(), reason, 
			       &LNO_default_pool), &LNO_default_pool);
    Num_Cache_Strips = lm.Nstrips();
  }

  //
  // EXPLANATORY NOTE: 
  // There are three cases of interest below.  The most important is 
  // the first, in which all of the tile loops are put on the outside 
  // when the SNL is transformed.  In the other two cases, a tile loop 
  // may be placed inside the transformed SNL. It might have been simpler   
  // to always place the tile loops on the outside, but the treatment 
  // below more accurately mirrors the current behavior of the model. 
  //
  // The order of transformations in SNL_INV_Transforms() is: 
  //  (1) Scalar expansion and distribution.  
  //  (2) Permutation. 
  //  (3) Cache tiling.  
  //  (4) Register tiling. 
  //  In case #1 (ti->Nloops() == nloops), all of the transformations 
  //    are done in one call to SNL_INV_Transforms(). 
  //  In case #2 (not case #1, but no register tiling), we perform the 
  //    permutation in the first call to SNL_INV_Transforms(), then 
  //    the cache tiling in the second call.  
  //  In case #3 (the rest), we perform permutation and register tiling 
  //    of the loops which will be outerside the outermost cache tile 
  //    in the first call to SNL_INV_Transforms(), and then per-
  //    form the cache tiling and register tiling of loops inside the 
  //    cache tile loops in the second call. 
  //  In cases #2 and #3, we may actually execute a series of calls to 
  //    SNL_INV_Transforms().  The first call to SNL_INV_Transforms
  //    Transforms() potentially creates several SNLs, and we want to 
  //    apply the cache (and possibly register tiling) to all of these. 
  //
  //    Right now, the only way I know that this could happen is when  
  //    we register tile on the first call (case #3).  In this case, 
  //    we may create windown loops which also need to be cache and 
  //    register tiled.  One might think that the windown loops are 
  //    not important, but when the trip count of a register tiled 
  //    loop is small, most of the work occurs in the winddown loops. 
  //    Thus, we apply our "second part" optimizations to these loops
  //    as well.  To do this, we keep track of the "kernel" of trans-
  //    formable loops "rg_kernel" , which we pass to the series of 
  //    second part calls to SNL_INV_Transforms().  This kernel 
  //    excludes scalar-expansion-only tiles and distributed sections 
  //    that may have been generated during the first call to SNL_
  //    Invariant_Transforms().  
  //   
  //    A bug (pv 589255) exposed an error in the original implementation 
  //    of this algorithm.  It turns out that the loops 'rg_kernel' are 
  //    not necessarily all SNLs.  To fix problems with this, I added the
  //    the Find_Kernel_Stack() routine.  This routine searches for a set
  //    of innermost SNLs over which we will apply the second pass of    
  //    SNL_INV_Transform() (which is done in cases #2 and #3).  (RJC)

  if (trying_invariant) {
#ifdef Is_True_On
    if (snl_debug >= 3) {
      fprintf(TFile, "Beginning invariant transformation\n");
    }
#endif
    BOOL want_se_and_dist = orderp != NULL || ti != NULL && ti->Strips() > 0
      || regstripsz && (ni->Privatizability_Info().Lcd_Depth() != -1
      || (ni->Privatizability_Info().Must_Finalize())); 
    DOLOOP_STACK* stack = &ni->Dostack();
    INT first_in_stack = ni->Depth_Inner() - nloops + 1;
    WN* wn_outer = stack->Bottom_nth(first_in_stack);
    SNL_REGION rg_kernel(wn_outer, wn_outer); 
    if (ti == NULL || ti->Nloops() == nloops) {
      region = SNL_INV_Transforms(wn_outer, orderp, ni, nloops, ti, 
        regstripszp, reg_usage, want_se_and_dist, NULL, LNO_Run_Oinvar,
        &rg_kernel);
    }
    else if (!regstripszp) {
      region = SNL_INV_Transforms(wn_outer, orderp, ni, nloops, NULL, 
        NULL, reg_usage, want_se_and_dist, NULL,FALSE, &rg_kernel);
      DOLOOP_STACK kernel_stack(&LNO_local_pool);
      Find_Kernel_Stack(&rg_kernel, ti->Nloops(), &kernel_stack);
      for (i = 0; i < kernel_stack.Elements(); i++) {
        WN* wn_new_outer = kernel_stack.Bottom_nth(i);
        WN* wn_new_inner = SNL_Get_Inner_Snl_Loop(wn_new_outer, ti->Nloops());
        ni->Privatizability_Info().Update_Reduction_Loop_Stmts(wn_new_inner);
        SNL_REGION rg_local = rg_kernel;
	SNL_REGION rg_local_kernel;
        BOOL might_reset_first = wn_new_outer == region.First; 
        BOOL might_reset_last = wn_new_outer == region.Last; 
	rg_local = SNL_INV_Transforms(wn_new_outer, NULL, ni, ti->Nloops(),
	  ti, NULL, EST_REGISTER_USAGE(), FALSE, NULL, LNO_Run_Oinvar, 
	  &rg_local_kernel);
	if (might_reset_first && rg_local.First != region.First)
	  region.First = rg_local.First; 
	if (might_reset_last && rg_local.Last != region.Last)
	  region.Last = rg_local.Last; 
      }
    } else {
      INT rg1[SNL_MAX_LOOPS];
      INT rg2[SNL_MAX_LOOPS];
      INT i;
      for (i = 0; i < nloops-ti->Nloops(); i++)
	rg1[i] = regstripsz[i];
      for ( ; i < nloops; i++)
	rg2[i-(nloops-ti->Nloops())] = regstripsz[i], rg1[i] = 1;
      region = SNL_INV_Transforms(wn_outer, orderp, ni, nloops, NULL, 
	rg1, reg_usage, want_se_and_dist, NULL,FALSE, &rg_kernel);
      DOLOOP_STACK kernel_stack(&LNO_local_pool);
      Find_Kernel_Stack(&rg_kernel, ti->Nloops(), &kernel_stack);
      for (i = 0; i < kernel_stack.Elements(); i++) {
        WN* wn_new_outer = kernel_stack.Bottom_nth(i);
        WN* wn_new_inner = SNL_Get_Inner_Snl_Loop(wn_new_outer, ti->Nloops());
        ni->Privatizability_Info().Update_Reduction_Loop_Stmts(wn_new_inner);
        SNL_REGION rg_local = rg_kernel;
        SNL_REGION rg_local_kernel;
        BOOL might_reset_first = wn_new_outer == region.First; 
        BOOL might_reset_last = wn_new_outer == region.Last; 
        rg_local = SNL_INV_Transforms(wn_new_outer, NULL, ni, ti->Nloops(),
          ti, rg2, EST_REGISTER_USAGE(), FALSE, NULL, LNO_Run_Oinvar, 
	  &rg_local_kernel);
	if (might_reset_first && rg_local.First != region.First)
	  region.First = rg_local.First; 
	if (might_reset_last && rg_local.Last != region.Last)
	  region.Last = rg_local.Last; 
      }
    }
    SNL_SPL_Split_Inner_Tile_Loops(region.First, region.Last, 
      LNO_Split_Tiles, "$spl_", TRUE); 
    Remove_Useless_Loops(&region); 

#ifdef Is_True_On
    if (snl_debug) {
      fprintf(TFile, "WN DUMP AFTER AUTOMATIC INVARIANT TRANSFORMATION\n");
      Dump_WN(region, TFile, snl_debug);
      fprintf(TFile, "END WN DUMP AFTER AUTOMATIC INVARIANT TRANSFORMATION\n");
    }
    SNL_Sanity_Check_Region(region);
#endif

    *changed = TRUE;
    if (!Valid_SNL_Region(region))
      DevWarn("Do_Automatic_Transformation: Invalid SNL_REGION [0x%p,0x%p]",
	region.First, region.Last);
    return region;
  }

  {
    // a general transformation

#ifdef Is_True_On
    if (snl_debug >= 3) {
      fprintf(TFile, "Beginning general transformation\n");
    }
#endif
    IMAT uu(nloops, nloops, &LNO_default_pool);
    for (INT i = 0; i < nloops; i++)
      for (INT j = 0; j < nloops; j++)
	uu(i,j) = (i == order[j]);

    BOOL failed = FALSE; 
    region = SNL_GEN_Protect_Nest_With_Conditionals(ni, &failed);
    if (failed) {
      if (!Valid_SNL_Region(region))
	DevWarn("Do_Automatic_Transformation: Invalid SNL_REGION [0x%p,0x%p]",
	  region.First, region.Last);
      return region; 
    } 

    BOOL has_lcd = ni->Privatizability_Info().Lcd_Depth() != -1;
    SNL_REGION dregion = SNL_GEN_Distribution(
      ni->Dostack().Bottom_nth(ni->Depth_Inner() - nloops + 1), &uu, ti, 
      ni->Nloops_General(), has_lcd, &ni->Privatizability_Info().Plist,
      ni->Above_Is_Distributable(), ni->Below_Is_Distributable());

    DOLOOP_STACK* stack = &ni->Dostack();

    INT firstin = ni->Depth_Inner() - nloops + 1;
    if (region.First == stack->Bottom_nth(firstin))
      region.First = dregion.First;
    if (region.Last == stack->Bottom_nth(firstin))
      region.Last = dregion.Last;

    if (reordering || ti) {
      IMAT* uup = reordering ? &uu : NULL;
      SNL_REGION ctregion;
      if (uup == NULL || ti == NULL || nloops == ti->Nloops())
	ctregion = SNL_GEN_U_Ctiling(ni->Dostack().Bottom_nth(firstin), nloops, 
          uup, ti, ni->Bi(), &(ni->Privatizability_Info().Plist), reg_usage, 
          TRUE);
      else {
	// in case you want to do a permutation, and then tile only some
	// of the inner loops.
	ctregion = SNL_GEN_U_Ctiling(ni->Dostack().Bottom_nth(firstin), nloops, 
          uup, NULL, ni->Bi(), &(ni->Privatizability_Info().Plist), reg_usage,
          TRUE);
	(void) SNL_GEN_U_Ctiling(ni->Dostack().Bottom_nth(firstin), nloops, 
	  NULL, ti, ni->Bi(), &(ni->Privatizability_Info().Plist), 
          EST_REGISTER_USAGE(), TRUE);
      }
      if (region.First == stack->Bottom_nth(firstin))
	region.First = ctregion.First;
      if (region.Last == stack->Bottom_nth(firstin))
	region.Last = ctregion.Last;
    }

    if (regstripsz[nloops-2] > 1) {
      SNL_REGION regregion = SNL_GEN_2D_Regtile(ni, regstripsz[nloops-2]);
      if (region.First == stack->Bottom_nth(firstin+nloops-2))
	region.First = regregion.First;
      if (region.Last == stack->Bottom_nth(firstin+nloops-2))
	region.Last = regregion.Last;
    }
    SNL_SPL_Split_Inner_Tile_Loops(region.First, region.Last, 
      LNO_Split_Tiles, "$spl_", TRUE); 
    Remove_Useless_Loops(&region); 

#ifdef Is_True_On
    if (snl_debug) {
      fprintf(TFile, "WN DUMP AFTER AUTOMATIC GENERAL TRANSFORMATION\n");
      Dump_WN(region, TFile, snl_debug);
      fprintf(TFile, "END WN DUMP AFTER AUTOMATIC GENERAL TRANSFORMATION\n");
    }
    SNL_Sanity_Check_Region(region);
#endif

    *changed = TRUE;
    if (!Valid_SNL_Region(region))
      DevWarn("Do_Automatic_Transformation: Invalid SNL_REGION [0x%p,0x%p]",
        region.First, region.Last);
     return region;
  }
}

//-----------------------------------------------------------------------
// NAME: Make_Edge_Blockable
// FUNCTION: Make the DEPV_ARRAY 'dva' for the given 'edge' blockable in 
//   the loops between 'start_depth' and 'stop_depth', inclusive.  Return 
//   TRUE if it was possible to do this, FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Make_Edge_Blockable(EINDEX16 edge, 
				DEPV_ARRAY* dva, 
                                INT start_depth, 
                                INT stop_depth)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DEPV_LIST dl(dva, &LNO_local_pool);
  DEPV_LIST dl_block(dl.Num_Dim(), dl.Num_Unused_Dim(), &LNO_local_pool);
  dl.Blockable_Part(&LNO_local_pool, &dl_block, dl.Num_Unused_Dim(), 
    dl.Num_Dim(), start_depth, stop_depth); 
  DEPV_ARRAY* dva_new = Create_DEPV_ARRAY(&dl_block, dg->Pool());
  if (dva_new != NULL) { 
    Delete_DEPV_ARRAY(dva, dg->Pool());
    dg->Set_Depv_Array(edge, dva_new);
  } 
  return dva_new != NULL; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Fix_Blockable_Dependendences_Traverse
// FUNCTION: For the tree of nodes starting at 'wn_tree', fix the depen-
//   dences of those nodes in the first 'nloops' loops of the loop nest 
//   starting with loop 'wn_outer', making them fully permutable.  Return
//   TRUE if it was possible to do this, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL SNL_Fix_Blockable_Dependendences_Traverse(WN* wn_outer, 
						      INT nloops, 
						      WN* wn_tree) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT start_depth = Do_Loop_Depth(wn_outer); 
  INT stop_depth = start_depth + nloops - 1; 
 
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) { 
    if (Do_Loop_Depth(wn_tree) >= Do_Loop_Depth(wn_outer) + nloops)
      return TRUE; 
  } else if (dg->Get_Vertex(wn_tree)) { 
    VINDEX16 v = dg->Get_Vertex(wn_tree); 
    EINDEX16 enext = 0; 
    for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = enext) {
      enext = dg->Get_Next_Out_Edge(e);
      DEPV_ARRAY* dva = dg->Depv_Array(e);
      if (dva->Is_Blockable(start_depth, stop_depth))
        continue; 
      if (!Make_Edge_Blockable(e, dva, start_depth, stop_depth))
        return FALSE;  
    } 
  } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))  
      SNL_Fix_Blockable_Dependendences_Traverse(wn_outer, nloops, wn); 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      SNL_Fix_Blockable_Dependendences_Traverse(wn_outer, nloops, 
        WN_kid(wn_tree, i)); 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Fix_Blockable_Dependendences
// FUNCTION: For the first 'nloops' loops of the SNL with outermost loop
//   'wn_outer', fix the dependences on these loops so that they are 
//   fully permutable.  Return TRUE if it was possible to do this, FALSE
//   otherwise. 
//-----------------------------------------------------------------------

static BOOL SNL_Fix_Blockable_Dependendences(WN* wn_outer, 
					     INT nloops)
{ 

  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  return SNL_Fix_Blockable_Dependendences_Traverse(wn_outer, nloops, 
    wn_outer); 
} 

//-----------------------------------------------------------------------
// NAME: Fix_Blockable_Dependences
// FUNCTION: Scan the SNL of 'nloops' loops with outermost loop 'wn_outer',
//   and honor the 'Blockable_Specification' on the DO_LOOP_INFOs on each
//   loop in the SNL.  Return TRUE if it was possible to do this, FALSE
//   otherwise.  
//-----------------------------------------------------------------------

static BOOL Fix_Blockable_Dependences(WN* wn_outer,  
				      INT nloops)
{
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  for (INT i = 0; i < stack.Elements(); i++) { 
    WN* wn_loop = stack.Bottom_nth(i);
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop); 
    if (dli_loop->Blockable_Specification > 0) {
      INT block_nloops = dli_loop->Blockable_Specification; 
      if (!SNL_Fix_Blockable_Dependendences(wn_loop, block_nloops))
	return FALSE;    
    } 
  } 
  return TRUE; 
} 

static void SNL_Transform_Min(WN* wn,
		              INT nloops)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  WN* wn_original = wn; 
  INT nloops_original = nloops; 
  if (wn == NULL) 
    return; 

  // Remove non-permutable dependences, if BLOCKABLE directive was present
  if (!Fix_Blockable_Dependences(wn, nloops))
    return; 
  // Attempt to fix index variables which have array dependences
  if (!SNL_Fix_Array_Deps_On_Index_Variable(wn, nloops))
    return; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn, nloops); 
  WN* wn_ok = NULL; 
  WN *wnn;
  for (wnn = wn_inner; wnn != NULL; wnn = LWN_Get_Parent(wnn)) { 
    if (WN_opcode(wnn) == OPC_DO_LOOP) {  
      if (Need_Fix_Array_Deps_On_Index_Variable(wnn))
        break;  
      wn_ok = wnn;
      if (wn_ok == wn) 
	break; 
    }
  } 
  if (wn_ok == NULL)
    return; 
  nloops -= Do_Loop_Depth(wn_ok) - Do_Loop_Depth(wn); 
  wn = wn_ok; 

  // This should never happen in real code, but do the right thing
  // (look only at inner loops) if it does happen.

  while (nloops > SNL_MAX_LOOPS) {
    nloops--;
    wn = Good_Do_Next_Innermost(wn);
    FmtAssert(wn, ("Can't find loop in deep loop nest"));
  }

  while (nloops >= 2 && wn != NULL 
    && (!Do_Loop_Is_Good(wn) || Do_Loop_Has_Calls(wn) ||
	 Do_Loop_Has_Gotos(wn))) {
    nloops--;
    wn = Good_Do_Next_Innermost(wn);
    // FmtAssert(wn, ("Can't find loop in deep loop nest"));
  }

  WN* wn_innermost_legotile = NULL;  
  for (WN* wn_lego = wn; wn_lego != NULL; 
    wn_lego = Good_Do_Next_Innermost(wn_lego)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_lego); 
    if (dli->Is_Outer_Lego_Tile) 
      wn_innermost_legotile = wn_lego; 
  } 
  if (wn_innermost_legotile != NULL) {
    WN* wn_new = Good_Do_Next_Innermost(wn_innermost_legotile);
    if (wn_new == NULL) 
      return;  
    nloops = nloops - (Do_Depth(wn_new) - Do_Depth(wn)); 
    wn = wn_new; 
  }
  if (wn == NULL) 
    return; 
 
  WN* parent_wn=LWN_Get_Parent(LWN_Get_Parent(wn));
  if (parent_wn && (WN_opcode(parent_wn) == OPC_DO_LOOP)) {
    SNL_NEST_INFO ni(parent_wn, wn, nloops, &LNO_default_pool);
  }
  else if (parent_wn && WN_opcode(parent_wn) == OPC_IF &&
      Get_Do_Loop_Info(wn)->Multiversion_Alias) {
    WN *parent_wn2 = LWN_Get_Parent(LWN_Get_Parent(parent_wn));
    if (parent_wn2 && (WN_opcode(parent_wn2) == OPC_DO_LOOP)) {
      SNL_NEST_INFO ni(parent_wn2, wn, nloops, &LNO_default_pool);
    }
  } 
}

extern void Fully_Unroll_Short_Loops(WN *);
extern BOOL Peel_2D_Triangle_Loops(WN* outer_loop);
static void SNL_Transform(WN* wn,
		          INT nloops)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  WN* wn_original = wn; 
  INT nloops_original = nloops; 
  if (wn == NULL) 
    return; 

  // Remove non-permutable dependences, if BLOCKABLE directive was present
  if (!Fix_Blockable_Dependences(wn, nloops))
    return; 
  // Attempt to fix index variables which have array dependences
  if (!SNL_Fix_Array_Deps_On_Index_Variable(wn, nloops))
    return; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn, nloops); 
  WN* wn_ok = NULL; 
  WN *wnn;
  for (wnn = wn_inner; wnn != NULL; wnn = LWN_Get_Parent(wnn)) { 
    if (WN_opcode(wnn) == OPC_DO_LOOP) {  
      if (Need_Fix_Array_Deps_On_Index_Variable(wnn))
        break;  
      wn_ok = wnn;
      if (wn_ok == wn) 
	break; 
    }
  } 
  if (wn_ok == NULL)
    return; 
  nloops -= Do_Loop_Depth(wn_ok) - Do_Loop_Depth(wn); 
  wn = wn_ok; 

  // This should never happen in real code, but do the right thing
  // (look only at inner loops) if it does happen.

  while (nloops > SNL_MAX_LOOPS) {
    nloops--;
    wn = Good_Do_Next_Innermost(wn);
    FmtAssert(wn, ("Can't find loop in deep loop nest"));
  }

  while (nloops >= 2 && wn != NULL 
    && (!Do_Loop_Is_Good(wn) || Do_Loop_Has_Calls(wn) ||
	 Do_Loop_Has_Gotos(wn))) {
    nloops--;
    wn = Good_Do_Next_Innermost(wn);
    // FmtAssert(wn, ("Can't find loop in deep loop nest"));
  }

  WN* wn_innermost_legotile = NULL;  
  for (WN* wn_lego = wn; wn_lego != NULL; 
    wn_lego = Good_Do_Next_Innermost(wn_lego)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_lego); 
    if (dli->Is_Outer_Lego_Tile) 
      wn_innermost_legotile = wn_lego; 
  } 
  if (wn_innermost_legotile != NULL) {
    WN* wn_new = Good_Do_Next_Innermost(wn_innermost_legotile);
    if (wn_new == NULL) 
      return;  
    nloops = nloops - (Do_Depth(wn_new) - Do_Depth(wn)); 
    wn = wn_new; 
  }
  if (wn == NULL) 
    return; 
 
  if (nloops <= 1) {
    if (LNO_Analysis) {
      fprintf(LNO_Analysis,"    (NESTING_DEPTH %d)\n", nloops);
      fprintf(LNO_Analysis,"    (LINE_POS %d)\n",
              Srcpos_To_Line(WN_Get_Linenum(wn)));
    }
    if (nloops == 1 && LNO_Run_Oinvar) {
      if (Roundoff_Level >= ROUNDOFF_ASSOC) {
        MEM_POOL_Push(&LNO_local_pool);
        {
          HASH_TABLE<WN *,BIT_VECTOR *> htable(500,&LNO_local_pool);
          extern void Mark_Invar(WN *region, INT num_loops, 
		DOLOOP_STACK *do_stack, HASH_TABLE<WN *,BIT_VECTOR *> *htable, 
		MEM_POOL *pool, BOOL outer_only);
          extern BOOL Sort_Invar_Expressions(WN *wn, 
			HASH_TABLE<WN *,BIT_VECTOR *> *htable);

          DOLOOP_STACK *do_stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
					      &LNO_local_pool);
          Build_Doloop_Stack(wn,do_stack);
          Mark_Invar(WN_do_body(wn),nloops,do_stack,&htable,&LNO_local_pool,
					FALSE);
          Sort_Invar_Expressions(wn,&htable);
        }
        MEM_POOL_Pop(&LNO_local_pool);
      }
      extern void Hoist_Outer_Invar(WN *wn_inner, INT num_loops,
				INT outer_reg_tile, BOOL can_tile);
      Hoist_Outer_Invar(wn, 1,1,TRUE);
    }


    return;
  }

  for (wnn = wn; wnn != NULL; wnn = Good_Do_Next_Innermost(wnn)) 
    SNL_Optimize_Bounds_With_Access_Vectors(wnn, Du_Mgr);

  SNL_NEST_INFO ni(wn, nloops, &LNO_default_pool, TRUE);

  if (LNO_Analysis) {
    fprintf(LNO_Analysis,"    (NESTING_DEPTH %d)\n",nloops);
    fprintf(LNO_Analysis,"    (LINE_POS");
    for (INT i = 0; i < nloops; i++) {
      INT stack_pos = ni.Depth_Inner()-nloops+1+i;
      fprintf(LNO_Analysis," %d",
	 Srcpos_To_Line(WN_Get_Linenum(ni.Dostack().Bottom_nth(stack_pos))));
    }
    fprintf(LNO_Analysis,")\n");
  }

  if (!ni.Innermost()) {
    SNL_DEBUG2(1, "Nest at line %d (nloops = %d): inner loop not innermost",
	       Srcpos_To_Line(WN_Get_Linenum(wn)), nloops);
    if (LNO_Analysis) {
      fprintf(LNO_Analysis,
	"    (SNL_FAILURES  (%d \"inner loop not innermost\"))\n",
	Srcpos_To_Line(WN_Get_Linenum(wn)));
    }
    SNL_Optimize_Bounds(SNL_REGION(wn, wn));
    return;
  }

  INT nloops_g = ni.Nloops_General();
  INT nloops_i = ni.Nloops_Invariant();
  INT nloops_t = ni.Nloops_Transformable();

  while (nloops >= 2 && nloops > nloops_g && nloops > nloops_i ) {
    nloops--;
    ni.Exclude_Outer_Loops(1);
    nloops_g = ni.Nloops_General();
    nloops_i = ni.Nloops_Invariant();
    if (LNO_Analysis || LNO_Tlog) {
      const char* message;
      char* scalar_message = NULL;
      char  buf[32];           // enough space for its use
      switch (ni.Problem(ni.Depth_Inner() - nloops).Problem) {
       case SNL_LOOP_PROBLEM_NONE:
        SNL_DEBUG0(1, "Unknown reason why loop is not transformable");
        message = "unknown reason";     // could instead be NULL
	break;
       case SNL_LOOP_PROBLEM_DISTRIBUTION:
        message = "imperfect nest undistributable";
	break;
       case SNL_LOOP_PROBLEM_SCALAR: {
         SYMBOL var = ni.Problem(ni.Depth_Inner() - nloops).Var;
         const char* name = var.Name();
         scalar_message = (char*) alloca(strlen(name) + 25);
         sprintf(scalar_message, "%s unexpandable scalar", name);
        }
	break;
       case SNL_LOOP_PROBLEM_LOOP: {
         WN* wn = ni.Problem(ni.Depth_Inner() - nloops).Wn;
         Is_True(wn, ("missing wn for loop problem"));
         sprintf(buf, "%d loop of bad form",
                 Srcpos_To_Line(WN_Get_Linenum(wn)));
         message = buf;
        }
	break;
       case SNL_LOOP_PROBLEM_INNER_MIGHT_NOT_GO:
        message = "can't be certain that all loops further in execute";
	break;
       case SNL_LOOP_PROBLEM_INNER_DOES_NOT_GO:
        message = "some loop further in does not execute";
	break;
       default:
        message = NULL;
	Is_True(0, ("Missing excuse"));
      }
      if (scalar_message) message = scalar_message;
      if (message) {
        if (LNO_Analysis)
          fprintf(LNO_Analysis,
                "    (SNL_FAILURES  (%d \"%s\"))\n",
                Srcpos_To_Line(WN_Get_Linenum(wn)), message);
        if (LNO_Tlog) {
          char tlog_buf[256];
          if (strlen(message)>=256)
            sprintf(tlog_buf,"SNL_FAILURES message too long");
          else
            sprintf(tlog_buf,"SNL_FAILURES %s", message);
          Generate_Tlog("LNO","snl", Srcpos_To_Line(WN_Get_Linenum(wn)),
                ST_name(WN_st(WN_index(wn))),
                "", "", tlog_buf);
        }
      }
    }
    wn = Good_Do_Next_Innermost(wn);
    FmtAssert(wn, ("Can't find loop in loop nest"));
  }

  if (nloops <= 1) {
    if (LNO_Verbose) {
     printf("SNL not transforming nest,");
     printf(" transformable depth is less than two.\n");
    }
    if (nloops_original == 2 && nloops_t == 2)
    {
      if (Peel_2D_Triangle_Loops(wn_original))
      {
          // let the later phase Unroll_before_Factorize to
          // unroll these loops
          WN* parent_original=LWN_Get_Parent(wn_original);
          while (parent_original && 
            WN_operator(parent_original) != OPR_FUNC_ENTRY &&
            WN_operator(parent_original) != OPR_DO_LOOP)
            parent_original = LWN_Get_Parent(parent_original);

          if (WN_operator(parent_original) == OPR_DO_LOOP)
          {
            DO_LOOP_INFO *dli = Get_Do_Loop_Info(parent_original);
            if (dli) dli->Delay_Full_Unroll = TRUE;
          }      
      }    
      return;
    }
    SNL_Optimize_Bounds(SNL_REGION(wn, wn));
    SNL_Transform(Find_Next_Innermost_Do(wn_original), nloops_original - 1); 
    return;
  }

  SNL_DEBUG2(1, "SNL FOUND A NEST AT LINE %d (nloops = %d):\n",
	     Srcpos_To_Line(WN_Get_Linenum(wn)), ni.Nloops());

  if (snl_debug >= 2) {
    Dump_WN(wn, TFile, snl_debug, 2, 2,
            Array_Dependence_Graph, NULL, NULL);
    fprintf(TFile, "END OF FOUND NEST\n");
    if (snl_debug >= 3) {
      WN* f = NULL;
      for (WN* p = wn; p; p = LWN_Get_Parent(p))
        f = p;
      Print_Def_Use(f, TFile);
    }
  }


  SNL_DEBUG2(1, "max loops in transformation: invariant %d, general %d\n",
	     ni.Nloops_Invariant(), ni.Nloops_General());

  FmtAssert(nloops >= 2 &&
            (nloops == ni.Nloops_Invariant() || nloops == ni.Nloops_General()),
            ("This is what loop above was for"));

  if (snl_debug >= 3) {
    LNO_Print_Access(TFile, wn);
    Array_Dependence_Graph->Print(TFile);
  }

  SNL_REGION    region;
  BOOL          changed;

#ifdef KEY 
  // Bug 6420 - After forward substitution from HMB, loop bounds may become
  // compile-time constants. In that event, we should update the estimated
  // number of iterations. Otherwise, the models will assume an estimated
  // iteration count of 100 (without feedback), and produce unroll factors 
  // greater than max iteration possible. This behavior is incorrect. When 
  // Est_Num_Iterations can be updated, this will also produce a better model
  // for the loop. The reason this bug got exposed is because of 
  // -WOPT:prop_dope=off. Now, pre-optimizer does not propagate dope vectors
  // including the ones in loop upper bounds. So, propagating constant loop
  // upper bounds (dope vector extent is a compile-time constant) is left to 
  // forward substitution in hoist messy bounds.
  for (INT i = 0; i < ni.Dostack().Elements(); i ++) {
    WN*           loop = ni.Dostack().Bottom_nth(i);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
    DOLOOP_STACK stack(&LNO_local_pool);
    Build_Doloop_Stack(loop, &stack);
    dli->Set_Est_Num_Iterations(&stack);
  }
#endif
  region = Do_Automatic_Transformation(wn, nloops, &ni, &changed);

  if (snl_debug) {
    if (changed && region.First) {
      fprintf(TFile, "\nDump After Transformation:\n");
      WN *rwn;
      for (rwn = region.First; rwn; rwn = WN_next(rwn)) {
        Dump_WN(rwn, TFile, snl_debug);
        if (rwn == region.Last)
          break;
      }
      FmtAssert(rwn == region.Last, ("Bad region"));
      fprintf(TFile, "\n");

      if (snl_debug) {
        WN* f = NULL;
        for (WN* p = region.First; p; p = LWN_Get_Parent(p))
          f = p;
        Print_Def_Use(f, TFile);
      }
    }
    else {
      SNL_DEBUG0(1, "\nNo transformation applied.\n\n");
    }
  }

  SNL_Optimize_Bounds(region);
  return;
}

extern void SNL_Phase(WN* func_nd)
{
  if (Get_Trace(TP_LNOPT, TT_LNO_SKIP_PH2))
    return; 
  if (Get_Trace(TP_LNOPT2, TT_SHACKLE_ONLY))
    return;
  if (Get_Trace(TP_LNOPT2, TT_TILE_ONLY)) {
    LNO_Run_Oinvar = FALSE; 
    LNO_Outer_Unroll = 1; 
  }   

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd);

// Bug 6010: if the number of nests in a procedure exceeds
// the limits, unrolling may require a large compilation time
// memory. Setting to 1 will not directly affect any major LNO
// optimizations 
#ifdef KEY
  if(ffi->Num_Snl() > MAX_NESTS_IN_FU)
    LNO_Outer_Unroll = 1;
#endif

  if (LNO_Test_Dump)
    for (INT i = 0; i < ffi->Num_Snl(); i++)
      ffi->Print(i, TFile);

  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    WN* wn = ffi->Get_Wn(i);
    INT nloops = ffi->Get_Depth(i);
    if (nloops < 1 || ffi->Get_Type(i) != Inner)
      continue;
    if (LNO_Analysis)
      fprintf(LNO_Analysis, "(LNO_SNL\n");
    SNL_Upper_Bound_Standardize(wn, nloops); 
    Hoist_Bounds_One_Level(wn); 
    SNL_Transform(wn, nloops);
    if (LNO_Analysis)
      fprintf(LNO_Analysis,")\n");
  }
}

#ifdef TARG_X8664
extern void SNL_Lite_Phase(WN* func_nd)
{
  if (Get_Trace(TP_LNOPT, TT_LNO_SKIP_PH2))
    return; 
  if (Get_Trace(TP_LNOPT2, TT_SHACKLE_ONLY))
    return;
  if (PU_has_mp (Get_Current_PU ()) || Early_MP_Processing ||
      PU_mp(Get_Current_PU ()))
    return;
  if (Get_Trace(TP_LNOPT2, TT_TILE_ONLY)) {
    LNO_Run_Oinvar = FALSE; 
    LNO_Outer_Unroll = 1; 
  }

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd);

// Bug 6010: if the number of nests in a procedure exceeds
// the limits, unrolling may require a large compilation time
// memory. Setting to 1 will not directly affect any major LNO
// optimizations 
#ifdef KEY
  if(ffi->Num_Snl() > MAX_NESTS_IN_FU)
    LNO_Outer_Unroll = 1;
#endif

  if (LNO_Test_Dump)
    for (INT i = 0; i < ffi->Num_Snl(); i++)
      ffi->Print(i, TFile);

  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    WN* wn = ffi->Get_Wn(i);
    INT nloops = ffi->Get_Depth(i);
    if (nloops < 1 || ffi->Get_Type(i) != Inner)
      continue;
    if (LNO_Analysis)
      fprintf(LNO_Analysis, "(LNO_SNL\n");
    SNL_Upper_Bound_Standardize(wn, nloops); 
    SNL_Transform_Min(wn, nloops);
    if (LNO_Analysis)
      fprintf(LNO_Analysis,")\n");
  }
}
#endif

//-----------------------------------------------------------------------
// NAME: RED_CLASS 
// FUNCTION: An enumeration type with one of three values: 
//   RED_ACCESS: Redundant access array
//   RED_BOUND: Redundant terms in WHIRL expression 
//   RED_NEITHER: Neither RED_ACCESS nor RED_BOUND. 
//-----------------------------------------------------------------------

enum RED_CLASS {RED_ACCESS, RED_BOUND, RED_NEITHER}; 

//-----------------------------------------------------------------------
// NAME: SNL_Bound_Lin_Symb_Worth_Optimizing 
// FUNCTION: Returns a RED_CLASS indicating whether the linear part of an 
//   access vector 'av' or the corresponding part in the bounds expression 
//   'wn_exp' is redundant (or neither). 
//-----------------------------------------------------------------------

static RED_CLASS SNL_Bound_Lin_Symb_Worth_Optimizing(WN* wn_exp, 
					             ACCESS_VECTOR* av)
{
  if (av->Lin_Symb == NULL)
    return RED_NEITHER; 
  INTSYMB_CONST_ITER iter(av->Lin_Symb);
  const INTSYMB_NODE* first = iter.First();
  for (const INTSYMB_NODE* node = first; !iter.Is_Empty(); node = iter.Next()) {
    if (node->Coeff == 0) {
      DevWarn("Access vector has zero coefficient linear symbol");
      return RED_ACCESS; 
    }
    INT count = Symbol_Count(wn_exp, node->Symbol);
    if (count == 0) { 
      DevWarn("Access vector has redundant linear symbol");
      return RED_ACCESS; 
    }
    if (count > 1)  
      return RED_BOUND;
  }
  return RED_NEITHER;  
}
    
//-----------------------------------------------------------------------
// NAME: SNL_Bound_Non_Lin_Symb_Worth_Optimizing 
// FUNCTION: Returns a RED_CLASS indicating whether the nonlinear part of an 
//   access vector 'av' or the corresponding part in the bounds expression 
//   'wn_exp' is redundant (or neither). 
//-----------------------------------------------------------------------

static RED_CLASS SNL_Bound_Non_Lin_Symb_Worth_Optimizing(WN* wn_exp, 
					                 ACCESS_VECTOR* av)
{
  if (av->Non_Lin_Symb == NULL)
    return RED_NEITHER; 
  SUMPROD_CONST_ITER iter(av->Non_Lin_Symb);
  const SUMPROD_NODE* first = iter.First(); 
  for (const SUMPROD_NODE* node = first; !iter.Is_Empty(); node = iter.Next()) {
    if (node->Coeff == 0) {
      DevWarn("Access vector has zero coefficient nonlinear symbol");
      return RED_ACCESS;
    }
    SYMBOL_CONST_ITER sitr(node->Prod_List);
    const SYMBOL_NODE* f = sitr.First();
    for (const SYMBOL_NODE* node = f; !sitr.Is_Empty(); node = sitr.Next()) {
      INT count = Symbol_Count(wn_exp, node->Symbol); 
      if (count == 0) {
        DevWarn("Access vector has redundant nonlinear symbol");
        return RED_ACCESS; 
      }
      if (count > 1)
	return RED_BOUND;
    }
  } 
  return RED_NEITHER; 
}

//-----------------------------------------------------------------------
// NAME: Enclosing_Loop_At_Depth 
// FUNCTION: Returns the OPC_DO_LOOP which is an ancestor of 'wn_exp' and 
//   has the specified 'depth'.  The indicated loop may be 'wn_exp' itself.
//   Returns NULL if no such loop exists.   
//-----------------------------------------------------------------------

static WN* Enclosing_Loop_At_Depth(WN* wn_exp, 
				   INT depth)
{
  for (WN* wn = wn_exp; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP && Do_Loop_Depth(wn) == depth)
      return wn;
  return NULL; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Bound_Loop_Coeff_Worth_Optimizing 
// FUNCTION: Returns a RED_CLASS indicating whether the loop coefficent 
//   part of an access vector 'av' or the corresponding part in the bounds 
//   expression 'wn_exp' is redundant (or neither).
//-----------------------------------------------------------------------

static RED_CLASS SNL_Bound_Loop_Coeff_Worth_Optimizing(WN* wn_exp, 
						       ACCESS_VECTOR* av,
						       INT loop_depth)
{
  for (INT j = 0; j < loop_depth; j++) {
    WN* wn_jloop = Enclosing_Loop_At_Depth(wn_exp, j);
    INT count = Symbol_Count(wn_exp, SYMBOL(WN_index(wn_jloop)));
    if (count == 0 && av->Loop_Coeff(j) > 0) { 
      DevWarn("Access vector has redundant index");
      return RED_ACCESS; 
    }
    if (count > 1)  
      return RED_BOUND;
  }
  return RED_NEITHER;  
}

//-----------------------------------------------------------------------
// NAME: ACOPR_CLASS 
// FUNCTION: An enumeration type which indicates one of the following: 
//   ACOPR_MAX: The operator for the access array is a MAX operator 
//   ACOPR_MIN: The operator for the access array is a MIN operator 
//   ACOPR_ONE: No MAX or MIN, access array must have only one vector
//   ACOPR_INVALID: Can't classify the operator for the access array as 
//     either a MAX or MIN operator.  
//-----------------------------------------------------------------------

enum ACOPR_CLASS {ACOPR_MAX, ACOPR_MIN, ACOPR_ONE, ACOPR_INVALID};   

//-----------------------------------------------------------------------
// NAME: SNL_Access_Operator 
// FUNCTION: Returns an ACOPR_CLASS indicating if the bounds expression 
//   'wn_exp' should have an MAX or MIN type access array. 
//-----------------------------------------------------------------------

static ACOPR_CLASS SNL_Access_Operator(WN* wn_exp)
{
  INT max_count = Num_Maxs(wn_exp);
  INT min_count = Num_Mins(wn_exp);
  if (max_count > 0 && min_count == 0) 
    return ACOPR_MAX; 
  if (min_count > 0 && min_count == 0)
    return ACOPR_MIN;
  if (min_count == 0 && max_count == 0) 
    return ACOPR_ONE;    
  return ACOPR_INVALID;
}

//-----------------------------------------------------------------------
// NAME: SNL_Bound_Worth_Optimizing 
// FUNCTION: Returns TRUE if the access array 'aa' is in a simpler form 
//   that 'wn_exp', FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL SNL_Bound_Worth_Optimizing(WN* wn_exp, 
				       ACCESS_ARRAY* aa,
				       INT loop_depth) 
{
  if (Bound_Is_Too_Messy(aa))
    return FALSE;
  ACOPR_CLASS acopr = SNL_Access_Operator(wn_exp);
  if (acopr == ACOPR_INVALID)
    return FALSE;
  if (acopr == ACOPR_ONE && aa->Num_Vec() > 1)
    return FALSE;  
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    RED_CLASS red = SNL_Bound_Loop_Coeff_Worth_Optimizing(wn_exp, av, 
      loop_depth);
    if (red == RED_ACCESS)
      return FALSE;
    if (red == RED_BOUND)
      return TRUE;
    red = SNL_Bound_Lin_Symb_Worth_Optimizing(wn_exp, av);
    if (red == RED_ACCESS)
      return FALSE;
    if (red == RED_BOUND)
      return TRUE;
    red = SNL_Bound_Non_Lin_Symb_Worth_Optimizing(wn_exp, av);
    if (red == RED_ACCESS)
      return FALSE;
    if (red == RED_BOUND)
      return TRUE;
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: SNL_LB_Worth_Optimizing 
// FUNCTION: Returns TRUE if the access array for the lower bound of the  
//   loop 'wn_loop' is in a potentially simpler form than the lower bound
//   expression in WN_start(wn_loop).  Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL SNL_LB_Worth_Optimizing(WN* wn_loop) 
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  ACCESS_ARRAY* aa = dli->LB; 
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    if (av->Loop_Coeff(i) > 0) 
      return FALSE;
  }
  return SNL_Bound_Worth_Optimizing(WN_start(wn_loop), aa,
    Do_Loop_Depth(wn_loop));
}

//-----------------------------------------------------------------------
// NAME: SNL_UB_Worth_Optimizing 
// FUNCTION: Returns TRUE if the access array for the upper bound of the  
//   loop 'wn_loop' is in a potentially simpler form than the upper bound
//   expression in WN_end(wn_loop).  Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL SNL_UB_Worth_Optimizing(WN* wn_loop) 
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  ACCESS_ARRAY* aa = dli->UB; 
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    if (av->Loop_Coeff(i) < 0) 
      return FALSE;
  }
  if (!Upper_Bound_Standardize(WN_end(wn_loop), TRUE))
    return FALSE; 
  if (UBexp(WN_end(wn_loop)) == NULL)
    return FALSE; 
  return SNL_Bound_Worth_Optimizing(UBexp(WN_end(wn_loop)), aa, 
    Do_Loop_Depth(wn_loop));
}

//-----------------------------------------------------------------------
// NAME: SNL_Opr 
// FUNCTION: Returns a binary expression with operator 'opr' and kids 
//  'wn_exp1' and 'wn_exp2'.   
//-----------------------------------------------------------------------

static WN* SNL_Opr(OPERATOR opr, 
		   WN* wn_exp1, 
	           WN* wn_exp2)
{
  if (wn_exp1 == NULL)
    return wn_exp2;
  if (wn_exp2 == NULL) 
    return wn_exp1; 
  TYPE_ID type = Max_Wtype(WN_rtype(wn_exp1), WN_rtype(wn_exp2));
  OPCODE op = OPCODE_make_op(opr, Promote_Type(type), MTYPE_V);
  return LWN_CreateExp2(op, wn_exp1, wn_exp2);
}

//-----------------------------------------------------------------------
// NAME: SNL_Access_Index_Section 
// FUNCTION: Returns WHIRL code which represents the loop coefficent section
//   of the access vector 'av' multiplied by the 'direction'. We use 'wn_exp'
//   to obtain DU information. 
//-----------------------------------------------------------------------

static WN* SNL_Access_Index_Section(WN* wn_exp, 
				    ACCESS_VECTOR* av, 
				    INT direction,
				    INT loop_depth,  
				    DU_MANAGER* du)
{
  FmtAssert(direction == 1 || direction == -1, ("Invalid direction value"));
  WN* wn_result = NULL; 
  for (INT i = 0; i < loop_depth; i++) {
    if (av->Loop_Coeff(i) != 0) {
      SYMBOL sym_index = SYMBOL(WN_index(Enclosing_Loop_At_Depth(wn_exp, i)));
      WN* wn_index = Find_Node(sym_index, wn_exp);
      WN* wn_index_copy = LWN_Copy_Tree(wn_index); 
      LWN_Copy_Def_Use(wn_index, wn_index_copy, du);
      WN* wn_const = LWN_Make_Icon(WN_rtype(wn_index_copy), 
	direction * av->Loop_Coeff(i)); 
      WN* wn_prod = SNL_Opr(OPR_MPY, wn_const, wn_index_copy);
      wn_result = SNL_Opr(OPR_ADD, wn_result, wn_prod); 
    }
  } 
  return wn_result; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Access_Linear_Section 
// FUNCTION: Returns WHIRL code which represents the linear section of the
//   access vector 'av' multiplied by the 'direction'. We use 'wn_exp' to 
//   obtain DU information. 
//-----------------------------------------------------------------------

static WN* SNL_Access_Linear_Section(WN* wn_exp, 
				    ACCESS_VECTOR* av, 
				    INT direction, 
				    DU_MANAGER* du)
{
  FmtAssert(direction == 1 || direction == -1, ("Invalid direction value")); 
  if (av->Lin_Symb == NULL)
    return NULL;
  WN* wn_result = NULL; 
  INTSYMB_CONST_ITER iter(av->Lin_Symb);
  const INTSYMB_NODE* first = iter.First();
  for (const INTSYMB_NODE* node = first; !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_index = Find_Node(node->Symbol, wn_exp);
    WN* wn_index_copy = LWN_Copy_Tree(wn_index); 
    LWN_Copy_Def_Use(wn_index, wn_index_copy, du);
    WN* wn_const = LWN_Make_Icon(WN_rtype(wn_index_copy), 
      direction * node->Coeff);  
    WN* wn_prod = SNL_Opr(OPR_MPY, wn_const, wn_index_copy);
    wn_result = SNL_Opr(OPR_ADD, wn_result, wn_prod); 
  } 
  return wn_result; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Access_Nonlinear_Section 
// FUNCTION: Returns WHIRL code which represents the nonlinear section of the
//   access vector 'av' multiplied by the 'direction'. We use 'wn_exp' to 
//   obtain DU information. 
//-----------------------------------------------------------------------

static WN* SNL_Access_Nonlinear_Section(WN* wn_exp, 
				        ACCESS_VECTOR* av, 
				        INT direction, 
				        DU_MANAGER* du)
{
  FmtAssert(direction == 1 || direction == -1, ("Invalid direction value")); 
  if (av->Non_Lin_Symb == NULL)
    return NULL; 
  WN* wn_result = NULL;
  SUMPROD_CONST_ITER iter(av->Non_Lin_Symb);
  const SUMPROD_NODE* first = iter.First();
  for (const SUMPROD_NODE* node = first; !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_prod = NULL; 
    SYMBOL_CONST_ITER sitr(node->Prod_List);
    const SYMBOL_NODE* f = sitr.First();
    for (const SYMBOL_NODE* snode = f; !sitr.Is_Empty(); snode = sitr.Next()) {
      WN* wn_index = Find_Node(snode->Symbol, wn_exp);  
      WN* wn_index_copy = LWN_Copy_Tree(wn_index);
      LWN_Copy_Def_Use(wn_index, wn_index_copy, du);
      wn_prod = SNL_Opr(OPR_MPY, wn_prod, wn_index);  
    }
    WN* wn_const = LWN_Make_Icon(WN_rtype(wn_prod), direction * node->Coeff);  
    wn_prod = SNL_Opr(OPR_MPY, wn_prod, wn_const); 
    wn_result = SNL_Opr(OPR_ADD, wn_result, wn_prod); 
  }
  return wn_result;  
} 

//-----------------------------------------------------------------------
// NAME: SNL_Optimize_LB_With_Access_Vectors 
// FUNCTION: Optimize the lower bound of the loop 'wn_loop' if the access
//   vectors give a simpler representation than the WHIRL code itself.  
//-----------------------------------------------------------------------

static void SNL_Optimize_LB_With_Access_Vectors(WN* wn_loop, 
						DU_MANAGER* du) 
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  if (!dli->Step->Is_Const())
    return; 
  ACCESS_ARRAY* aa = dli->Step->Const_Offset > 0  ? dli->LB : dli->UB;
  if (Bound_Is_Too_Messy(aa))
    return; 
  WN* wn_exp = WN_kid0(WN_start(wn_loop)); 
  WN* wn_result = NULL; 
  ACOPR_CLASS acopr = SNL_Access_Operator(wn_exp);
  OPERATOR opr = acopr == ACOPR_MAX ? OPR_MAX : OPR_MIN; 
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    WN* wn_local_result = NULL; 
    WN* wn_index = SNL_Access_Index_Section(wn_exp, av, 1, 
      Do_Loop_Depth(wn_loop), du);
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_index);
    WN* wn_linear = SNL_Access_Linear_Section(wn_exp, av, 1, du);
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_linear);
    WN* wn_nonlinear = SNL_Access_Nonlinear_Section(wn_exp, av, 1, du); 
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_nonlinear);
    TYPE_ID index_type = wn_local_result == NULL 
      ? WN_rtype(wn_exp) : WN_rtype(wn_local_result); 
    WN* wn_const = LWN_Make_Icon(index_type, -av->Const_Offset); 
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_const); 
    INT divisor = av->Loop_Coeff(Do_Loop_Depth(wn_loop));
    FmtAssert(divisor <= -1, ("Should have screened out other values"));
    if (divisor < -1) 
      wn_local_result = LWN_CreateDivfloor(WN_rtype(wn_local_result), 
	wn_local_result, LWN_Make_Icon(WN_rtype(wn_local_result), -divisor)); 
    wn_result = SNL_Opr(opr, wn_result, wn_local_result);  
  }
  Replace_Wnexp_With_Exp_Copy(wn_exp, wn_result, du);
  LWN_Delete_Tree(wn_result); 
}
//-----------------------------------------------------------------------
// NAME: SNL_Optimize_UB_With_Access_Vectors 
// FUNCTION: Optimize the upper bound of the loop 'wn_loop' if the access
//   vectors give a simpler representation than the WHIRL code itself.  
//-----------------------------------------------------------------------

static void SNL_Optimize_UB_With_Access_Vectors(WN* wn_loop,
						DU_MANAGER* du) 
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  if (!dli->Step->Is_Const())
    return; 
  ACCESS_ARRAY* aa = dli->Step->Const_Offset > 0  ? dli->UB : dli->LB;
  if (Bound_Is_Too_Messy(aa))
    return;
  if (!Upper_Bound_Standardize(WN_end(wn_loop), TRUE))
    return; 
  if (UBvar(WN_end(wn_loop)) == NULL || UBexp(WN_end(wn_loop)) == NULL)
    return;  
  WN* wn_exp = UBexp(WN_end(wn_loop)); 
  WN* wn_result = NULL; 
  ACOPR_CLASS acopr = SNL_Access_Operator(wn_exp);
  OPERATOR opr = acopr == ACOPR_MAX ? OPR_MAX : OPR_MIN; 
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    WN* wn_local_result = NULL; 
    WN* wn_index = SNL_Access_Index_Section(wn_exp, av, -1, 
      Do_Loop_Depth(wn_loop), du);
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_index);
    WN* wn_linear = SNL_Access_Linear_Section(wn_exp, av, -1, du);
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_linear);
    WN* wn_nonlinear = SNL_Access_Nonlinear_Section(wn_exp, av, -1, du); 
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_nonlinear);
    TYPE_ID index_type = wn_local_result == NULL 
      ? WN_rtype(wn_exp) : WN_rtype(wn_local_result); 
    WN* wn_const = LWN_Make_Icon(index_type, av->Const_Offset); 
    wn_local_result = SNL_Opr(OPR_ADD, wn_local_result, wn_const); 
    INT divisor = av->Loop_Coeff(Do_Loop_Depth(wn_loop));
    FmtAssert(divisor >= 1, 
      ("Should have screened out other values"));
    if (divisor > 1) 
      wn_local_result = LWN_CreateDivfloor(WN_rtype(wn_local_result), 
	wn_local_result, LWN_Make_Icon(WN_rtype(wn_local_result), divisor)); 
    wn_result = SNL_Opr(opr, wn_result, wn_local_result);  
  }
  WN* wn_ldid = LWN_Copy_Tree(UBvar(WN_end(wn_loop)));
  LWN_Copy_Def_Use(UBvar(WN_end(wn_loop)), wn_ldid, du); 
  OPCODE op_orig = WN_opcode(WN_end(wn_loop));
  OPCODE op_le = OPCODE_make_op(OPR_LE, OPCODE_rtype(op_orig),
    OPCODE_desc(op_orig));
  wn_result = LWN_CreateExp2(op_le, wn_ldid, wn_result);
  Replace_Wnexp_With_Exp_Copy(WN_end(wn_loop), wn_result, du);
  LWN_Delete_Tree(wn_result); 
}

//-----------------------------------------------------------------------
// NAME: SNL_Optimize_Bounds_With_Access_Vectors 
// FUNCTION: Optimizes the lower and upper bounds of the loop 'wn_loop' 
//   using the access vectors, if the access vectors have a simpler 
//   representation.     
//-----------------------------------------------------------------------

static void SNL_Optimize_Bounds_With_Access_Vectors(WN* wn_loop, 
					    	    DU_MANAGER* du)
{
  if (SNL_LB_Worth_Optimizing(wn_loop))
    SNL_Optimize_LB_With_Access_Vectors(wn_loop, du);
  if (SNL_UB_Worth_Optimizing(wn_loop))
    SNL_Optimize_UB_With_Access_Vectors(wn_loop, du); 
}

