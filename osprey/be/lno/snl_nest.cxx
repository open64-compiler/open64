/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/**
*** This file implements finding information about singly nested loop nests
*** and finding those eligible for transformation.
***
*** The bounds information in SNL_NEST_INFO is canonicized so
*** that the first variable corresponds to the outermost found loop, etc.
*** The conditionals use the same variables as are used in Bi.
***/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_nest_CXX      "snl_nest.cxx"
const static char *rcs_id =   snl_nest_CXX "$Revision$";

#include <sys/types.h>
#include "fiz_fuse.h"
#include "lwn_util.h"
#include "dep_graph.h"
#include "snl.h"
#include "snl_nest.h"
#include "snl_xbounds.h"
#include "scalar_expand.h"
#include "wn_simp.h"
#include "lnoutils.h"
#include "cond.h"

//-----------------------------------------------------------------------
// SNL_NEST_INFO member functions
//-----------------------------------------------------------------------

void SNL_NEST_INFO::Print(FILE* f) const
{
  fprintf(f, "Nest Info Begin\n");
  fprintf(f, "\t_nloops=%d _nloops_invariant=%d _nloops_general=%d\n",
	  _nloops, _nloops_invariant, _nloops_general);
  fprintf(f, "\t_above_is_distributable=%d _below_is_distributable=%d\n",
	  _above_is_distributable, _below_is_distributable);
  fprintf(f, "\tLoops:");
  for (INT i = _depth_inner - _nloops + 1; i <= _depth_inner; i++) {
    fprintf(f, " ");
    (SYMBOL(WN_index(Dostack().Bottom_nth(i)))).Print(f);
  }
  fprintf(f, "\n");
  if (_bi)
    _bi->Print(f);
  _privatizability_info.Print(f);
  fprintf(f, "Nest Info End\n");
}

SNL_NEST_INFO::~SNL_NEST_INFO()
{
  if (_bi)
    CXX_DELETE(_bi, _pool);
}

// Given that "outer" is the outer loop of an SNL of nloops "nloops",
// return the innermost loop.

// Note that "depth" means the loop number from the outermost in, and it
// also means the number of loops in a nest, which is very different.
// So for this file, don't use depth for the second meaning: use nloops.

//-----------------------------------------------------------------------
// SNL_NEST_INFO::SNL_NEST_INFO implementation
//-----------------------------------------------------------------------

// SNL_Is_Transformable() returns TRUE if the nest
// is transformable in this loop.  The rules are
//
//   o The index variable is not live upon entry to the loop; i.e. it
//     does not appear in the upper bound, lower bound, or step.
//   o The lb, ub access vectors are invariant (except in outer
//     induction variables) through the outer loop.
//   o The step is 1.  If the step is -1, we just change the code.
//   o All modified scalars in this loop are privatizable.
//
// There is one other rule.  The bound constants (e.g. n in i+N) must not
// vary in the loop.  Actually, they cannot vary anywhere inside the
// outer loop.
//
// If it returns false, then a problem report is passed in p.

static BOOL SNL_Is_Transformable(WN* wn, INT outer_depth, DO_LOOP_INFO* dli)
{
  FmtAssert(WN_opcode(wn) == OPC_DO_LOOP, ("is_transformable requires DO"));

  SYMBOL	index_symbol = SYMBOL(WN_index(wn));
  const INT     index_name_sz = 128;
  char		index_name[index_name_sz];
  index_symbol.Name(index_name, index_name_sz);

  if (!Upper_Bound_Standardize(WN_end(wn), TRUE))
    return FALSE; 

  // If index variable is live upon entry, or is live on exit, then
  // this loop nest is not transformable.  Step size must be unit.
  // Bounds must be well behaved.

  BOOL live_at_entry = Index_Variable_Live_At_Entry(wn); 
  BOOL live_at_exit = Index_Variable_Live_At_Exit(wn); 

  if (!live_at_entry && live_at_exit) {
    Finalize_Index_Variable(wn, TRUE); 
    index_symbol = SYMBOL(WN_index(wn));
    index_symbol.Name(index_name, index_name_sz);
  }  

  if (live_at_entry) { 
    SNL_DEBUG2(3, "Loop %s(0x%p) not transformable: live on entry/exit",
               index_name, wn);
    return FALSE;
  }

  if (Step_Size(wn) != 1) {
    SNL_DEBUG1(2, "Index %s has non-unit step", index_name);
    return FALSE;
  }

  if (!SNL_Is_Non_Varying_Access_Array(dli->LB, outer_depth)) { 
    SNL_DEBUG1(3, "Loop %s not transformable: varying bound lb", index_name);
    return FALSE;
  }   
  if (!SNL_Is_Non_Varying_Access_Array(dli->UB, outer_depth)) {  
    SNL_DEBUG1(3, "Loop %s not transformable: varying bound ub", index_name);
    return FALSE; 
  } 

  if (WN_start(wn) == NULL ||
      WN_operator(WN_start(wn)) != OPR_STID ||
      SYMBOL(WN_start(wn)) != index_symbol) {
    FmtAssert(0, ("Loop %s has bad lower bound", index_name));
    return FALSE;
  }

  if (WN_end(wn) == NULL) {
    FmtAssert(0, ("Loop %s (0x%p) has missing upper bound", index_name, wn));
  }

  if (Mono(SNL_UBexp(WN_end(wn)), index_symbol) != SNL_MONO_INVARIANT ||
      WN_operator(SNL_UBvar(WN_end(wn))) != OPR_LDID ||
      SYMBOL(SNL_UBvar(WN_end(wn))) != index_symbol) {
    DevWarn("Loop %s (0x%p) has surprising form for upper bound",
            index_name, wn);
    return FALSE;
  }

  switch (Do_Wtype(wn)) {
   case MTYPE_I8:
   case MTYPE_I4:
   case MTYPE_U8:
   case MTYPE_U4:
    break;
   case MTYPE_I2:
   case MTYPE_I1:
   case MTYPE_U2:
   case MTYPE_U1:
    // TODO OK: maybe one day we'll decide to handle this.
    // Check all the SNL code to make sure they'd be handled properly.
    DevWarn("Not transforming loop because index type too short: lame");
    return FALSE;
   default:
    Is_True(0, ("Loop %s of non-integral type %d", index_name, Do_Wtype(wn)));
    return FALSE;
  }

  return TRUE;
}

SNL_NEST_INFO::SNL_NEST_INFO(WN* outer, INT nloops, MEM_POOL* pool,
			     BOOL inner_only)
: _nloops(0),
  _bi(0),
  _pool(pool),
  _dostack(pool),
  _num_bad(0),
  _above_is_distributable(TRUE),
  _below_is_distributable(TRUE),
  _nloops_general(0),
  _nloops_invariant(0),
  _nloops_transformable(0),
  _depth_inner(0),
  _privatizability_info(pool),
  _problem(NULL)
{
  FmtAssert(nloops <= SNL_MAX_LOOPS,
            ("SNL_NEST_INFO constructor: nloops %d > %d",
            nloops, SNL_MAX_LOOPS));
  FmtAssert(pool != &SNL_local_pool, ("Bad pool for SNL_NEST_INFO()"));

  MEM_POOL_Push(&SNL_local_pool);

  if (nloops == 0 || nloops == 1)
    goto return_point;

  {
    WN* inner = SNL_Get_Inner_Snl_Loop(outer, nloops);
    Build_Doloop_Stack(inner, &_dostack);
    _innermost = Get_Do_Loop_Info(inner)->Is_Inner;
  
    _depth_inner = Dostack().Elements() - 1;
    _num_bad = _depth_inner - Good_Do_Depth(inner);
    _nloops = nloops;
    
    if (inner_only && !_innermost)
      goto return_point;
    
    {
      DO_LOOP_INFO* dli[LNO_MAX_DO_LOOP_DEPTH];
      for (INT d = Dostack().Elements() - 1; d >= 0; d--)
          dli[d] = Get_Do_Loop_Info(Dostack().Bottom_nth(d));
    
      // Ok, so now we start screening.  First, we can only optimize those
      // loops that do not have problems with privatizability.  They must
      // otherwise be transformable, meaning bounds well behaved, etc.
    
      FmtAssert(nloops == Do_Loop_Depth(inner) - Do_Loop_Depth(outer) + 1, 
                ("outer, inner, and nloops not consistent")); 
      _privatizability_info.Make_Sx_Info(outer, nloops); 
      _problem = CXX_NEW_ARRAY(SNL_LOOP_PROBLEM_INFO, _depth_inner+1, pool);
      INT ii;
      for (ii = 0; ii <= _depth_inner; ii++)
        _problem[ii] = SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_NONE);
    
        // The privatizability info tells us the outermost loop as far as
        // scalars are concerned.
    
      const SX_PNODE* pnode = NULL;
      INT outer_scalar_loop = 
        Privatizability_Info().First_Transformable_Depth( &pnode);
      INT outer_transformable_loop=
	Privatizability_Info().First_Transformable_Depth_Reduction(&pnode);
      INT i;
      for (i = 0; i < _nloops; i++) {
        INT d = _depth_inner - i;
        if (d < outer_scalar_loop)
          break;
      }
      for (ii = i; ii < _nloops; ii++) {
        INT d = _depth_inner - ii;
        _problem[d] = SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_SCALAR);
      }

      for (ii = 0; ii < i; ii++) {
        INT d = _depth_inner - ii;
        if (!SNL_Is_Transformable(Dostack().Bottom_nth(d),
                                  _depth_inner - _nloops + 1, dli[d]))
          break;
      }
      _nloops_general = ii;
      _nloops_invariant = ii;

      for (i = 0; i < _nloops; i++) {
        INT d = _depth_inner - i;
        if (d < outer_transformable_loop)
          break;
      }

      for (ii = 0; ii < i; ii++) {
        INT d = _depth_inner - ii;
        if (!SNL_Is_Transformable(Dostack().Bottom_nth(d),
                                  _depth_inner - _nloops + 1, dli[d]))
          break;
      }
      _nloops_transformable = ii;

    
      // How far out are the loop bounds really invariant?
      for (i = 2; i <= _nloops_invariant; i++) {
        INT d = _depth_inner + 1 - i;
        // see if the loops inside d depend on loop d.
        INT dd;
        for (dd = d + 1; dd <= _depth_inner; dd++) {
          if (!SNL_Is_Invariant(&Dostack(), d, dd))
            break;
          }
          if (dd <= _depth_inner)
          break;
      }
    
        // the innermost const nest is from d+1 to inner_depth
      _nloops_invariant = i - 1;
    
      // There are additional requirements for invariant nests and general
      // nests.  Invariant nests must be fully distributable.  For general
      // nests, we must be able to distribute fully on one side and prove that
      // at least one iteration goes.
    
      _above_is_distributable = TRUE;
      _below_is_distributable = TRUE;
    
      INT _nloops_max = _nloops_general > _nloops_invariant
        ? _nloops_general : _nloops_invariant;
      for (i = 2; i <= _nloops_max; i++) {
        INT d = _depth_inner + 1 - i;
        WN* wnd = Dostack().Bottom_nth(d); 
        BOOL above_ok = _above_is_distributable;
        INT dd;
        for (dd = d+1; above_ok && dd <= _depth_inner; dd++) {
          WN* wn1 = Dostack().Bottom_nth(dd-1);
          WN* wn = Dostack().Bottom_nth(dd);
          if (WN_prev_executable(wn) &&
              !SNL_Is_Distributable(wnd, wn1, wn, TRUE))
            above_ok = FALSE;
        }
    
        BOOL below_ok = _below_is_distributable;
        for (dd = d+1; below_ok && dd <= _depth_inner; dd++) {
          WN* wn1 = Dostack().Bottom_nth(dd-1);
          WN* wn = Dostack().Bottom_nth(dd);
          if (WN_next_executable(wn) &&
	      !SNL_Is_Distributable(wnd,wn1, wn, FALSE))
	    below_ok = FALSE;
        }
    
        if (above_ok == FALSE || below_ok == FALSE) {
          if (_nloops_invariant >= i)
            _nloops_invariant = i - 1;
    
          if (i <= _nloops_general) {
            if (above_ok == FALSE && below_ok == FALSE) {
              _nloops_general = i - 1;
              if (_nloops_general < _nloops_invariant)
                _problem[_depth_inner - i + 1] = 
		  SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_DISTRIBUTION);
              break;
            }
            else if (above_ok == FALSE)
              _above_is_distributable = FALSE;
            else
              _below_is_distributable = FALSE;
          }
        }
      }
    
      if (_nloops_general < 2)
        // nothing to do, since we dont use _bi in this case
        ;
      else if (_above_is_distributable && _below_is_distributable) {
        // no requirements about at least one iteration, so just build
        // bounds.
    
        INT outer_depth = _depth_inner + 1 - _nloops_general;
    
        _bi = CXX_NEW(SNL_BOUNDS_INFO(Pool()), Pool());
        _bi->Outermost_Depth() = outer_depth;
        for (INT d = outer_depth; d <= _depth_inner; d++)
          _bi->Collect_Do_Info(Dostack().Bottom_nth(d));
        _bi->Conditionals().Add_Vars(_bi->Bounds().Num_Vars() -
				     _bi->Conditionals().Num_Vars());
        _bi->Canonicize(_nloops_general, &Dostack(), outer_depth);
      }
      else {
        while (1) {
         try_while_again:
          INT inner_depth = _depth_inner;
          INT outer_depth = _depth_inner + 1 - _nloops_general;
          WN* outer = Dostack().Bottom_nth(outer_depth);
    
          // find first loop with a problem.
          INT i;
          for (i = _nloops_general - 2; i >= 0; i--) {
	    INT d = _depth_inner - i;
	    WN* wnd = Dostack().Bottom_nth(d);
	    if ((!_above_is_distributable && WN_prev_executable(wnd)) ||
	        (!_below_is_distributable && WN_next_executable(wnd)))
	      break;
          }
    
          // no problems
          if (i == -1) {
	    _above_is_distributable = _below_is_distributable = TRUE;
            outer_depth = _depth_inner + 1 - _nloops_general;  
	    _bi = CXX_NEW(SNL_BOUNDS_INFO(Pool()), Pool());
	    _bi->Outermost_Depth() = outer_depth;
	    for (INT d = outer_depth; d <= _depth_inner; d++)
	      _bi->Collect_Do_Info(Dostack().Bottom_nth(d));
	    _bi->Conditionals().Add_Vars(_bi->Bounds().Num_Vars() -
				         _bi->Conditionals().Num_Vars());
	    _bi->Canonicize(_nloops_general, &Dostack(), outer_depth);
	    break;
          }
          else if (_nloops_general < 2)
	    break;
    
          SNL_BOUNDS_INFO ebounds(&SNL_local_pool);
          ebounds.Outermost_Depth() = outer_depth;
          ebounds.Collect_Outer_Info(outer);
    
          for (INT d = outer_depth; d <= inner_depth; d++) {
    
	    // Add in an equation that says the loop never executes.  If that
	    // is consistent, then there's a problem.  If the bounds are 
	    // invariant, then add the appropriate conditional outside the 
	    // outer_depth. Otherwise, give up: the new outer depth is one 
	    // in from here.
    
	    WN* wn = Dostack().Bottom_nth(d);
    
            for (INT dimlb = 0; dimlb < dli[d]->LB->Num_Vec(); dimlb++) {
	      ACCESS_VECTOR* avlb = dli[d]->LB->Dim(dimlb);
	      for(INT dimub = 0; dimub < dli[d]->UB->Num_Vec(); dimub++) {
	        ACCESS_VECTOR* avub = dli[d]->UB->Dim(dimub);
	        ACCESS_VECTOR* nox = Difference_Inequality(avlb, avub, d,
						       DIFFERENCE_EXEC_NEVER,
						       &SNL_local_pool);
    
	        ACCESS_VECTOR* yesx = Difference_Inequality(avlb, avub, d,
						  DIFFERENCE_EXEC_ALWAYS,
						  &SNL_local_pool);
    
	        ebounds.Add_Access(nox, FALSE);
	        BOOL is_consistent = ebounds.Bounds().Is_Consistent();
	        ebounds.Bounds().Remove_Last_Le(1);
	        if (!is_consistent)
	          continue;
    
	        // We can enforce at least one iteration by putting an if
	        // outside, if ub and lb are invariant in outer_depth 
	        // through i-1.
    
	        BOOL is_const = TRUE;
	        for (INT dd = outer_depth; dd < d; dd++) {
	          if (nox->Loop_Coeff(dd))
		    is_const = FALSE;
	        }
    
	        if (is_const == FALSE) {
    
	          // With triangular loops, sometimes they go once too often,
	          // or at least the first or last iteration is the only one
	          // where the next inner loop goes zero times.
	          // We can fix this common case by peeling, but let's peel
	          // only if we are positive that the iteration we are peeling
	          // has no executions futher in, so that we can keep it an 
	          // SNL. Keep it simple: Peel the outer
	          // loop only, and only if this is the second to the outermost
	          // loop, and only if no iteratons go in that last iteration,
	          // and only if everything's ok without that last iteration.
	          // If all that works, then we continue on our merry way.
	          // Otherwise, try peeling the first iteration.
    
	          BOOL peeled=0; // 0 = no peel; 1 = peel first; 2 = peel last
    
	          // Try to peel the last iteration.
  
	          if (d == outer_depth + 1 &&
		      dli[outer_depth]->UB->Num_Vec() == 1 &&
		      dli[d]->LB->Num_Vec() == 1 &&
		      dli[d]->UB->Num_Vec() == 1 &&
		        dli[outer_depth]->UB->Dim(0)->Loop_Coeff(outer_depth)
							==1){
    
		    ACCESS_VECTOR* ub = dli[outer_depth]->UB->Dim(0);
    
		    // First, we add a constraint to pretend that the last
		    // iteration is the only one to execute.  Then we demand
		    // that the inner loop goes.  If inconsistent, then there
		    // is no solution, meaning the inner loop can't ever go.  
		    // That's our requirement for peeling.
    
		    ACCESS_VECTOR last_iter(ub, &SNL_local_pool);
		    last_iter.Negate_Me();
		    ebounds.Add_Access(&last_iter, FALSE);
                    ebounds.Add_Access(yesx, FALSE);
		    is_consistent = ebounds.Bounds().Is_Consistent();
		    ebounds.Bounds().Remove_Last_Le(2);
    
		    if (!is_consistent) {
		      // peeling is totally legal.  Now just make sure that
		      // it solves our problem.  That is, if we reduce ub by
		      // one then an iteration is guaranteed to execute, i.e.
		      // it's inconsistent to have no iterations.
    
		      ACCESS_VECTOR ub2(ub, &SNL_local_pool);
		      ub2.Const_Offset--;
		      ebounds.Add_Access(&ub2, FALSE);
		      ebounds.Add_Access(nox, FALSE);
		      is_consistent = ebounds.Bounds().Is_Consistent();
		      ebounds.Bounds().Remove_Last_Le(1);
		      if (!is_consistent)
		        peeled = 2;
		      else
		        ebounds.Bounds().Remove_Last_Le(1);
		    }
	          }
    
	          // Try to peel the first iteration.
    
	          if (peeled == 0 &&
		      d == outer_depth + 1 &&
		      dli[outer_depth]->LB->Num_Vec() == 1 &&
		      dli[d]->LB->Num_Vec() == 1 &&
		      dli[d]->UB->Num_Vec() == 1 &&
		        dli[outer_depth]->LB->Dim(0)->Loop_Coeff(outer_depth)
								== 1) {
      
		    ACCESS_VECTOR* lb = dli[outer_depth]->LB->Dim(0);
    
		    // First, we add a constraint to pretend that the first
		    // iteration is the only one to execute.  Then we demand
		    // that the inner loop goes.  If inconsistent, then there 
		    // is no solution, meaning the inner loop can't ever go.  
		    // That's our requirement for peeling.
    
		    ACCESS_VECTOR first_iter(lb, &SNL_local_pool);
		    first_iter.Negate_Me();
		    ebounds.Add_Access(&first_iter, FALSE);
                    ebounds.Add_Access(yesx, FALSE);
		    is_consistent = ebounds.Bounds().Is_Consistent();
		    ebounds.Bounds().Remove_Last_Le(2);
    
		    if (!is_consistent) {
		      // peeling is totally legal.  Now just make sure that
		      // it solves our problem.  That is, if we increase lb by
		      // one then an iteration is guaranteed to execute, i.e.
		      // it's inconsistent to have no iterations.
    
		      ACCESS_VECTOR lb2(lb, &SNL_local_pool);
		      lb2.Const_Offset--;// correct, because -i <= c
		      ebounds.Add_Access(&lb2, FALSE);
		      ebounds.Add_Access(nox, FALSE);
		      is_consistent = ebounds.Bounds().Is_Consistent();
		      ebounds.Bounds().Remove_Last_Le(1);
		      if (!is_consistent)
		        peeled = 1;
		      else
		        ebounds.Bounds().Remove_Last_Le(1);
		    }
	          }
    
	          if (peeled) {
		    WN* peel_wn = Dostack().Bottom_nth(outer_depth);
		    SNL_DEBUG1(1, "Peeling last iteration of %s",
			       SYMBOL(WN_index(peel_wn)).Name());
		    SNL_Peel_Iteration(peel_wn, peeled==1);
		    continue;
	          }
    
	          _problem[_depth_inner - _nloops_general + 1] =
	             SNL_LOOP_PROBLEM_INFO(
			SNL_LOOP_PROBLEM_INNER_MIGHT_NOT_GO);
	          SNL_DEBUG1(1, "Not enough iterations to use loop %s",
			     SYMBOL(WN_index(wn)).Name());
	          _nloops_general--;
	          goto try_while_again;
	        }
    
	        // Add in a conditional to guarantee loop executes at 
		// least once
    
	        ebounds.Add_Access(yesx, TRUE);
    
	        if (snl_debug >= 2) {
	          SNL_DEBUG0(2, "Added in a conditional: ");
	          ebounds.Print(TFile);
	        }
	      }
	    }
  
	    ebounds.Collect_Do_Info(wn);
	    ebounds.Conditionals().Add_Vars(ebounds.Bounds().Num_Vars() -
				ebounds.Conditionals().Num_Vars());
  
	    if (!ebounds.Bounds().Is_Consistent()) {
	      SNL_DEBUG1(0, "Loop %s appears to never execute.",
		         SYMBOL(WN_index(wn)).Name());
	      _problem[_depth_inner - _nloops_general + 1] =
	        SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_INNER_DOES_NOT_GO);
	      _nloops_general--;
	      goto try_while_again;
	    }
          }
    
          // All's well.
    
          FmtAssert(_bi==NULL, 
			("Confusion in SNL_NEST_INFO::SNL_NEST_INFO()"));
          _bi = CXX_NEW(SNL_BOUNDS_INFO(&ebounds, Pool()), Pool());
    
          _bi->Canonicize(_nloops_general, &Dostack(),
		          _depth_inner + 1 - _nloops_general);
          break;
        }
      }
    
    } 
  }
  return_point:
  MEM_POOL_Pop(&SNL_local_pool);

  if (snl_debug >= 3) {
    fprintf(TFile, "Nest info creation:\n");
    Print(TFile);
  }
}

extern WN *dump_tree(WN*);

SNL_NEST_INFO::SNL_NEST_INFO(WN* outer, WN *inner, INT nloops, MEM_POOL* pool)
: _nloops(0),
  _bi(0),
  _pool(pool),
  _dostack(pool),
  _num_bad(0),
  _above_is_distributable(FALSE),
  _below_is_distributable(FALSE),
  _nloops_general(0),
  _nloops_invariant(0),
  _nloops_transformable(0),
  _depth_inner(0),
  _privatizability_info(pool),
  _problem(NULL)
{
  FmtAssert(nloops <= SNL_MAX_LOOPS,
            ("SNL_NEST_INFO constructor: nloops %d > %d",
            nloops, SNL_MAX_LOOPS));
  FmtAssert(pool != &SNL_local_pool, ("Bad pool for SNL_NEST_INFO()"));

  MEM_POOL_Push(&SNL_local_pool);

  if (nloops < 1)
    goto return_point2;

  if ((OPT_unroll_level != 2) &&
     (!Get_Do_Loop_Info(inner)->Multiversion_Alias))
       goto return_point2;

  {
    Build_Doloop_Stack(inner, &_dostack);
    _innermost = Get_Do_Loop_Info(inner)->Is_Inner;
  
    _depth_inner = Dostack().Elements() - 1;
    _num_bad = _depth_inner - Good_Do_Depth(inner);
    _nloops = nloops;
    
    if (!_innermost)
      goto return_point2;

    if (Do_Loop_Depth(inner) != (Do_Loop_Depth(outer) + 1)) 
      goto return_point2;
    
    {
      DO_LOOP_INFO* dli[LNO_MAX_DO_LOOP_DEPTH];
      for (INT d = Dostack().Elements() - 1; d >= 0; d--)
        dli[d] = Get_Do_Loop_Info(Dostack().Bottom_nth(d));
    
      // Ok, so now we start screening.  First, we can only optimize those
      // loops that do not have problems with privatizability.  They must
      // otherwise be transformable, meaning bounds well behaved, etc.
      // Also we are only considering innermost loop peeling here. No
      // Other transformations are applied in this analysis.
    
      _privatizability_info.Make_Sx_Info(outer, nloops); 
      _problem = CXX_NEW_ARRAY(SNL_LOOP_PROBLEM_INFO, _depth_inner+1, pool);
      INT ii;
      for (ii = 0; ii <= _depth_inner; ii++)
        _problem[ii] = SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_NONE);
    
      // The privatizability info tells us the outermost loop as far as
      // scalars are concerned.
    
      const SX_PNODE* pnode = NULL;
      INT outer_scalar_loop = 
        Privatizability_Info().First_Transformable_Depth( &pnode);
      INT i;
      for (i = 0; i < _nloops; i++) {
        INT d = _depth_inner - i;
        if (d < outer_scalar_loop)
          break;
      }
      for (ii = i; ii < _nloops; ii++) {
        INT d = _depth_inner - ii;
        _problem[d] = SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_SCALAR);
      }

      // Is the loop a transformable candidate
      if (!SNL_Is_Transformable(inner, _depth_inner, dli[_depth_inner]))
          goto return_point2;

      _nloops_general == ii;
      while (1) {
       try_while_again:
        INT inner_depth = _depth_inner;
        INT outer_depth = _depth_inner - 1;
    
	_bi = CXX_NEW(SNL_BOUNDS_INFO(Pool()), Pool());
	_bi->Outermost_Depth() = outer_depth;
	for (INT d = outer_depth; d <= _depth_inner; d++)
	  _bi->Collect_Do_Info(Dostack().Bottom_nth(d));

	_bi->Conditionals().Add_Vars(_bi->Bounds().Num_Vars() -
				     _bi->Conditionals().Num_Vars());
	_bi->Canonicize(_nloops, &Dostack(), inner_depth);
    
        SNL_BOUNDS_INFO ebounds(&SNL_local_pool);
        ebounds.Outermost_Depth() = outer_depth;
        ebounds.Collect_Outer_Info(inner);
    
        //for (INT d = outer_depth; d <= inner_depth; d++) {
        {
          INT d = inner_depth;
   
	  // Add in an equation that says the loop never executes.  If that
	  // is consistent, then there's a problem.  If the bounds are 
	  // invariant, then add the appropriate conditional outside the 
	  // outer_depth. Otherwise, give up: the new outer depth is one 
	  // in from here.
    
	  WN* wn = Dostack().Bottom_nth(d);
    
          for (INT dimlb = 0; dimlb < dli[d]->LB->Num_Vec(); dimlb++) {
	    ACCESS_VECTOR* avlb = dli[d]->LB->Dim(dimlb);
	    for(INT dimub = 0; dimub < dli[d]->UB->Num_Vec(); dimub++) {
	      ACCESS_VECTOR* avub = dli[d]->UB->Dim(dimub);
    
	      BOOL is_consistent = ebounds.Bounds().Is_Consistent();
	      ebounds.Bounds().Remove_Last_Le(1);
	      if (!is_consistent)
	        continue;
    
	      // First test is that the ub and lb are invariant in the outer
              // loop.  TODO: add code to support SNL's with no parent loop.
              if (SNL_Is_Invariant(&Dostack(), outer_depth, inner_depth) ) {
                WN *stmt;
    
	        // Now mine the comp expression the loop and the
                // conditionals in the loop to see if there are
                // then regions which would not be executed if either
                // the first or the last iteration were missing.
	        BOOL peeled=0; // 0 = no peel; 1 = peel first; 2 = peel last
                BOOL exclusion = 1; // the then part of the loop is not peeled.

                // TODO: add support for the other case, inclusion which
                // would do the opposite, peeling the then and omitting
                // it from the loop body. cases are 
                //   if (lb expr match) then ...
                //   if (ub expr match) then ...
    
	        // Test for peeling the last iteration.
	        if (d == outer_depth + 1 &&
		    dli[d]->LB->Num_Vec() == 1 &&
		    dli[d]->UB->Num_Vec() == 1 &&
		    dli[d]->UB->Dim(0)->Loop_Coeff(d) ==1){

                  // Walk the loop lookinf for stmt level if regions
                  for (stmt = WN_first(WN_do_body(inner));
                    stmt; stmt = WN_next(stmt)) {
                    if (WN_operator(stmt) == OPR_IF) {
                      WN *UB_wn = SNL_UBexp(WN_end(inner));
                      WN *UB_var = SNL_UBvar(WN_end(inner));

                      // First try the upper bounds match
                      is_consistent =  SNL_Compare_Logic(UB_wn, UB_var,
                                                         inner, stmt, TRUE);
                      if (!is_consistent) {
		        peeled = 2;
                        break;
                      }

                      // Now try the lower bounds match
                      is_consistent =  SNL_Compare_Logic(NULL, NULL,
                                                         inner, stmt, FALSE);
                      if (!is_consistent) {
		        peeled = 1;
                        break;
                      }
                    }
		  }
	        }
    
	        if (peeled) {
		  WN* peel_wn = Dostack().Bottom_nth(inner_depth);
		  SNL_DEBUG1(1, "Peeling first or last iteration of %s",
		             SYMBOL(WN_index(peel_wn)).Name());
		  SNL_Peel_Iteration_Inner(peel_wn, stmt, exclusion, peeled==1);
	        } else {
	          _problem[_depth_inner] =
	            SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_INNER_MIGHT_NOT_GO);
	          SNL_DEBUG1(1, "Not enough iterations to use loop %s",
			     SYMBOL(WN_index(wn)).Name());
                }
	      }
	    }
	  }
  
	  ebounds.Collect_Do_Info(wn);
	  ebounds.Conditionals().Add_Vars(ebounds.Bounds().Num_Vars() -
	    ebounds.Conditionals().Num_Vars());
        }

        // All's well.
        break;
      }
    }
  }

  return_point2:
  MEM_POOL_Pop(&SNL_local_pool);

  if (snl_debug >= 3) {
    fprintf(TFile, "Nest info creation:\n");
    Print(TFile);
  }
}

void SNL_NEST_INFO::Exclude_Outer_Loops(INT how_many)
{
  FmtAssert(how_many > 0, ("Bad call to Exclude_Outer_Loops(INT)"));

  _nloops -= how_many;
  _nloops_invariant = MIN(_nloops, _nloops_invariant);
  FmtAssert(_nloops >= 1, ("Too many loops being excluded"));

  if (_nloops_general > _nloops) {
    if (_bi)
      _bi->Exclude_Outer_Loops(_nloops_general - _nloops);
    _nloops_general = _nloops;
  }
  if (_nloops_general == _nloops_invariant) {
    _above_is_distributable = TRUE;
    _below_is_distributable = TRUE;
  }
}

BOOL SNL_NEST_INFO::All_Var_Expandable(int nloops)
{
  if ( nloops > _nloops_general && nloops > _nloops_invariant )
    return FALSE;
  return TRUE;
}
