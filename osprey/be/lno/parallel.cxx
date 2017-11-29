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


#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <math.h>
#include <float.h>
#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "stab.h"
#include "targ_const.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "strtab.h"
#include "config.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "wintrinsic.h"
#include "lno_bv.h"
#include "dep_graph.h"
#include "debug.h"
#include "scalar_expand.h"
#include "cxx_memory.h"
#include "reduc.h"
#include "snl_utils.h"
#include "sxlist.h"
#include "snl_dist.h"
#include "permute.h"
#include "sxlimit.h"
#include "parallel.h"
#include "fiz_fuse.h"
#include "ara.h"
#include "snl_deps.h"
#include "lego_util.h"
#include "tile.h"
#include "model.h"
#include "cache_model.h"
#include "config_cache.h"
#include "parmodel.h"
#include "sdlist.h"
#include "doacross.h"
#include "parids.h"
#include "cond.h"
#include "move.h"
#include "tlog.h"
#include "call_info.h"
#include "cross_snl.h"

#define MAX_PARALLEL_NLOOPS 5 

static double Doacross_Cost(WN* wn_outer, INT permutation[], INT nloops,
  INT parallel_depth, SNL_DEP_MATRIX** sdm_inv, BOOL sdm_scl[],
  SX_INFO* sx_info, SD_INFO* sd_info, INT sd_split_depth,
  double machine_cycles, double *cache_cycles_per_iter, double work_estimate,
  INT* doacross_tile_size_p, INT sync_distances[],
  INT* doacross_overhead);
static double Parallel_Cost(WN* wn_outer, INT permutation[], INT nloops,
  INT parallel_depth, INT sd_split_depth, INT split_depth, SX_PLIST* plist,
  double machine_cycles, double *cache_cycles_per_iter);
static INT Parallelizable(WN* wn_outer, INT permutation[], INT nloops,
  INT parallel_depth, SNL_DEP_MATRIX** sdm_inv, BOOL sdm_scl[], 
  SX_INFO* sx_info, SD_INFO* sd_info, INT sd_split_depth);

#ifdef KEY
  INT Last_Apo_Loop_Id = 0;
#endif

//-----------------------------------------------------------------------
// NAME: Cannot_Concurrentize 
// FUNCTION: Returns TRUE if 'wn_loop' cannot be concurrentized because 
//   of a pragma or because it is the serial version of a concurrent loop. 
//-----------------------------------------------------------------------

extern BOOL Cannot_Concurrentize(WN* wn_loop)
{ 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  return dli->Pragma_Cannot_Concurrentize 
    || dli->Serial_Version_of_Concurrent_Loop
    || dli->Inside_Critical_Section
    || dli->Has_Threadprivate; 
} 

//=======================================================================
// CLASS: PARALLEL_INFO (Contains info about a possible loop order for 
//   parallelization)    
// MEMBER FUNCTIONS: Constructor
//=======================================================================

//-----------------------------------------------------------------------
// NAME: PARALLEL_INFO 
// FUNCTION: Create a PARALLEL_INFO of length 'nloops' representing an 
//   SNL which is not permuted or parallelized.
//-----------------------------------------------------------------------

PARALLEL_INFO::PARALLEL_INFO(INT nloops)
{
  _cost = (double) DBL_MAX;
  _parallel_depth = -1;
  _nloops = nloops;
  INT i;
  for (i = 0; i < nloops; i++)
    _permutation[i] = i;
  _int_type = INT_NONE;
  _split_depth = -1; 
  _sd_split_depth = -1; 

  _is_doacross = FALSE;
  _doacross_overhead = 0;
  _doacross_tile_size = 0;
  for (i = 0; i < 2; i++)
    _sync_distances[i] = 0;
}

static WN* Parallel_Loop(PARALLEL_INFO* parallel_info,
			 DOLOOP_STACK* loop_stack);
//-----------------------------------------------------------------------
// NAME: ap_tlog_info
// FUNCTION: output tlog info for auto-parallelization
// PARAMETERS:
//-----------------------------------------------------------------------

static void ap_tlog_info(
  PARALLEL_INFO* parallel_info,
  DOLOOP_STACK* loop_stack,
  const char* message)
{
  char out_string[80];
  WN* wn_parallel = Parallel_Loop(parallel_info, loop_stack); 
  if (wn_parallel == NULL) 
    return; 
  INT nloops = parallel_info->Nloops(); 
  INT inner_depth = loop_stack->Elements() - 1; 
  INT outer_depth = inner_depth - nloops + 1; 
  INT required_length = strlen(WB_Whirl_Symbol(wn_parallel)) + 13; 
  char* in_string = CXX_NEW_ARRAY(char, required_length, &LNO_local_pool);
  sprintf(in_string, "%s %d ", 
    WB_Whirl_Symbol(wn_parallel), Srcpos_To_Line(WN_linenum(wn_parallel))); 
  for (INT i = 0; i < parallel_info->Nloops(); i++) {
    sprintf(&out_string[5*i], "%2d%2s", parallel_info->Permutation(i), 
      parallel_info->Parallel_Depth() == i + outer_depth ?
      (parallel_info->Is_Doacross() ? "-X" : "-P") : "");;
    if (i < parallel_info->Nloops() - 1) 
      sprintf(&out_string[5*i+4], ",");
  } 
  Generate_Tlog("LNO","auto_parallelization",
                Srcpos_To_Line(WN_Get_Linenum(wn_parallel)),
                (char *)WB_Whirl_Symbol(wn_parallel),
                in_string, out_string, message);
}

//-----------------------------------------------------------------------
// NAME: Mark_Parallelizable_Loop
// FUNCTION: Let L be the loop at 'parallel_depth' in the SNL with outer-
//   most loop 'wn_outer' containing 'nloops' loops to which we are apply-
//   ing the 'permutation' of length 'nloops'.  Mark 'L' as parallelizable. 
//-----------------------------------------------------------------------

static void Mark_Parallelizable_Loop(WN* wn_outer, 
			             INT permutation[], 
		                     INT nloops,
			             INT parallel_depth,
				     SNL_DEP_MATRIX** sdm_inv,
				     BOOL sdm_scl[],
				     SX_INFO* sx_info,
				     SD_INFO* sd_info,
				     INT sd_split_depth)

{
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;
  INT split_depth = Parallelizable(wn_outer, permutation, nloops, 
    parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info, sd_split_depth); 
  if (split_depth == Do_Loop_Depth(wn_outer) + nloops) 
    return;  
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT original_depth = outer_depth + permutation[parallel_depth - outer_depth];
  WN* wn_parallel = SNL_Get_Inner_Snl_Loop(wn_outer, original_depth 
    - outer_depth + 1);  
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_parallel); 
  if (!dli->Parallelizable && parallel_debug_level >= 1) {
    fprintf(stdout, "Loop %s at %d is parallelizable\n", 
      WB_Whirl_Symbol(wn_parallel), Srcpos_To_Line(WN_linenum(wn_parallel))); 
    fprintf(TFile, "Loop %s at %d is parallelizable\n", 
      WB_Whirl_Symbol(wn_parallel), Srcpos_To_Line(WN_linenum(wn_parallel))); 
  } 
  dli->Parallelizable = TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: PARALLEL_INFO 
// FUNCTION: Create a PARALLEL_INFO to model transforming the SNL with 
//   outermost loop 'wn_outer' and containing 'nloops' with the loop at 
//   depth 'parallel_depth' being parallel.  The transformation consists
//   of applying a 'permutation' of length 'nloops'.  The type of
//   transformation, either general or invariant, is given by 'int_type'. 
//   The array dependences for the nest are summarized in 'sdm_inv'.  
//   The scalar dependences are summarized in 'sdm_scl'.  Information about 
//   scalars to be expanded is given by 'sx_info'.  Information about scalars
//   which may be distributed out, but aren't necessarily expanded, is given
//   in 'sd_info'.  The loop must be split at 'sd_split_depth' to remove 
//   unexpandable scalars and obtain the stated 'permutation'.  An 
//   estimate of one iteration of the SNL is given in 'work_estimate'. 
//-----------------------------------------------------------------------

PARALLEL_INFO::PARALLEL_INFO(WN* wn_outer, 
			     INT permutation[], 
		             INT nloops, 
			     INT parallel_depth, 
			     INTERCHANGE_TYPE int_type, 
			     SNL_DEP_MATRIX** sdm_inv,
			     BOOL sdm_scl[], 
			     SX_INFO* sx_info,
			     SD_INFO* sd_info, 
			     INT sd_split_depth,
			     double machine_cycles,
			     double work_estimate)
{
  _wn_outer = wn_outer;
  _nloops = nloops; 
  INT i;
  for (i = 0; i < nloops; i++) 
    _permutation[i] = permutation[i];  
  _int_type = int_type; 
  _is_doacross = FALSE;
  _doacross_overhead = 0;
  for (i = 0; i < 2; i++)
    _sync_distances[i] = NULL_DIST;
  _sd_split_depth = sd_split_depth; 
  _split_depth = Parallelizable(wn_outer, permutation, nloops, 
    parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info, _sd_split_depth); 
  BOOL is_doall = (_split_depth != Do_Loop_Depth(wn_outer) + nloops);
  double doall_cost=DBL_MAX;
  double doacross_cost=DBL_MAX;
  double cache_cycles_per_iter;

  // LNO_Run_Doacross==0	do not run doacross
  // LNO_Run_Doacross==1	run doacross after doall failed
  // LNO_Run_Doacross==2	run doall and doacross and pick the best one
  // LNO_Run_Doacross==3	run doall after doacross failed
  // LNO_Run_Doacross==4	run doacross only

  switch (LNO_Run_Doacross) {
    case 0: if (is_doall) {  
              doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
		parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
		machine_cycles, &cache_cycles_per_iter);
	    }
	    break;
    case 1: if (is_doall) {  
              doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
		parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
		machine_cycles, &cache_cycles_per_iter);
	    } else if (LNO_Pseudo_Lower) {
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
		parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
		sd_split_depth, machine_cycles, &cache_cycles_per_iter,
		work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead);
	    }
	    break;
    case 2: if (is_doall)
	      doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
	        parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
	        machine_cycles, &cache_cycles_per_iter);
	    if (LNO_Pseudo_Lower)
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
	        parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
	        sd_split_depth, machine_cycles, &cache_cycles_per_iter,
	        work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead);
	    break;
    case 3: if (LNO_Pseudo_Lower)
            {
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
	        parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
	        sd_split_depth, machine_cycles, &cache_cycles_per_iter,
	        work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead);
	      BOOL is_doacross= (doacross_cost != DBL_MAX);
	      if (!is_doacross && is_doall) {
                doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
		  parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
		  machine_cycles, &cache_cycles_per_iter);
	      }
            }
	    break;
    case 4: if (LNO_Pseudo_Lower)
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
	        parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
	        sd_split_depth, machine_cycles, &cache_cycles_per_iter,
	        work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead);
	    break;
    default: FmtAssert(0,("Invalid -LNO:doacross value"));
  }

  if (doall_cost==DBL_MAX && doacross_cost==DBL_MAX) {  
    // cannot parallelize with either doall or doacross
    _parallel_depth = -1;  
    _work_estimate = 0; 
    _cost = DBL_MAX;
    _is_doacross = FALSE;
    _doacross_tile_size = 0;
    _sync_distances[0] = 0;
    _sync_distances[1] = 0;
    _doacross_overhead = 0;
  } else if (doall_cost<doacross_cost) {  
    // parallelize with doall
    _parallel_depth = parallel_depth;
    _work_estimate = (int) Compute_Work_Estimate(work_estimate,
						 cache_cycles_per_iter);
    _cost = doall_cost;
    _is_doacross = FALSE;
    _doacross_tile_size = 0;
    _sync_distances[0] = 0;
    _sync_distances[1] = 0;
    _doacross_overhead = 0;
  } else {
    // parallelize with doacross
    _parallel_depth = parallel_depth;  
    _work_estimate = (int) Compute_Work_Estimate(work_estimate,
						 cache_cycles_per_iter);
    _cost = doacross_cost;
    _is_doacross = TRUE;
    _split_depth = -1; 
  }
}

//-----------------------------------------------------------------------
// NAME: Compute_Work_Estimate
// FUNCTION: Given the estimate of the number of cycles per iteration
//   of a loop required to perform machine operations, and the number
//   required to service cache misses, compute a conservative estimate of
//   the total number of cycles required per iteration.  The way the
//   estimate is computed is a kludge, intended to bridge the (wide) gap
//   between the machine and cache estimates and the actual values we see
//   in SPEC benchmarks--DRK.
//-----------------------------------------------------------------------

double Compute_Work_Estimate(double machine, double cache)
{
      /* Machine estimate is a lower bound, and is pretty accurate,
         so let it dominate when cache estimate is less.  We always
         cut the cache estimate at least in half. */
  if (machine >= cache)
    return machine + cache / 2.0;

      /* Otherwise adjust cache estimate downward like this: reduce
         the cache estimate by half for each factor of four (plus 1) by
	 which it is larger than the machine estimate.  Then return the sum
	 of the machine and reduced cache estimates.  The result is
	 conservative enough that it generally doesn't cause loops to run
	 in parallel when they shouldn't, but it's better than relying
	 on the machine estimate alone (which is too conservative, and
	 causes some loops to run serially when they should be in
	 parallel).  It's necessary to reduce the cache estimate
	 because it's really an over-estimate of cache costs in most
	 cases, and we want a conservative estimate here to avoid
	 slowdown by over-aggressive parallelization.  The parameters 2
	 and 4 and the form of the function are the result of
	 experimentation on SPEC benchmarks (especially Swim, Mgrid,
	 and Apsi), and looking at the estimated and actual execution
	 times of most of the loops in Mgrid.  All this needs to be
	 revisited in the future--DRK. */
  const double ratio = cache / machine;
  return machine + cache / pow(2.0, 1.0 + log10(ratio) / log10(4.0));
}

//-----------------------------------------------------------------------
// NAME: Parallel_Cost  
// FUNCTION: For the SNL with outermost loop 'wn_outer' conatining 'nloops' 
//   loops, return the cost of parallelizing the 'parallel_depth' loop 
//   after applying the 'permutation'.  The loop is distributed at 'sd_
//   split_depth' to remove unexpandable scalars and obtain the 'permuta-
//   tion'. The loop is distributed at 'split_depth' if this enables 
//   parallelism.  The 'plist' is a list of scalar expandable variables.   
//   The 'machine_cycles' are the number of machine cycles given the 
//   parallel loop at 'parallel_depth'.  cache_cycles_per_iter is the same
//   output parameter as in PAR_STAT::Cycle_Count(); its return value is 0
//   if Cycle_Count() is never called due to parallelization being
//   impossible (note also that it should go away--DRK).
//-----------------------------------------------------------------------

static double Parallel_Cost(WN* wn_outer, 
			    INT permutation[], 
			    INT nloops,
			    INT parallel_depth,
			    INT sd_split_depth, 
			    INT split_depth, 
			    SX_PLIST* plist,
			    double machine_cycles,
			    double *cache_cycles_per_iter)
{
  *cache_cycles_per_iter = 0.0;
  if (parallel_depth == -1) 
    return (double) DBL_MAX; 
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0; 
  PAR_STAT::id_count = 0; 
  PAR_STAT* ps = CXX_NEW(PAR_STAT(wn_outer, nloops, &LNO_local_pool), 
    &LNO_local_pool);
#ifdef Is_True_On
  ps->Sanity_Check(stdout); 
#endif 
  if (parallel_debug_level >= 3) {
    fprintf(stdout, "Before:\n"); 
    ps->Print(stdout); 
  } 
  ps = ps->Parallel_Interchange(wn_outer, permutation, nloops,
    parallel_depth, sd_split_depth, split_depth); 
#ifdef Is_True_On
  ps->Sanity_Check(stdout); 
#endif 
  double cost = ps->Cycle_Count(wn_outer, permutation, nloops, 
    parallel_depth, plist, split_depth, machine_cycles,
    cache_cycles_per_iter);
  if (parallel_debug_level >= 3) {
    ps->Sanity_Check(stdout); 
    fprintf(stdout, "After:\n"); 
    ps->Print(stdout); 
  } 
  return cost; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Finalize_Index_Variables
// FUNCTION: Finalize the index variables for each of the loops contained
//   in the SNL 'wn_outer' of 'nloops' loops. 
//-----------------------------------------------------------------------

static void SNL_Finalize_Index_Variables(WN* wn_outer, 
				         INT nloops)
{
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  for (INT i = outer_depth; i <= inner_depth; i++) { 
    WN* wn_loop = stack.Bottom_nth(i); 
    if (Index_Variable_Live_At_Exit(wn_loop))
      Finalize_Index_Variable(wn_loop, TRUE);  
  } 
}

//-----------------------------------------------------------------------
// NAME: Minimal_Kernel 
// FUNCTION: For the SNL with outermost loop 'wn_outer' and containing 
//   'nloops' loops, check whether each do loop is good and attempt to 
//   standardize and finalize the index variables of the loops of the 
//   SNL, going innermost out.  Returns the outermost loop for which 
//   this was successful.            
//-----------------------------------------------------------------------

extern WN* Minimal_Kernel(WN* wn_outer, 
			  INT nloops)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 
  if (!SNL_Fix_Array_Deps_On_Index_Variable(wn_outer, nloops))
    return NULL;
  WN* wn_new_outer = NULL; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP) 
      continue; 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    if (dli->Pragma_Cannot_Concurrentize)
      break;
    if (dli->Serial_Version_of_Concurrent_Loop)
      break;
    if (dli->Inside_Critical_Section)
      break;
    if (dli->Has_Threadprivate)
      break;
    if (!Do_Loop_Is_Good(wn))
      break; 
    if (!Upper_Bound_Standardize(WN_end(wn), TRUE))
      break; 
    if (Index_Variable_Live_At_Entry(wn))
      break;  
    if (Index_Variable_Live_At_Exit(wn) && dli->Has_Exits)
      break; 
    if (Need_Fix_Array_Deps_On_Index_Variable(wn))
      break; 
    wn_new_outer = wn;
    if (wn == wn_outer) 
      break; 
  }
  if (wn_new_outer == NULL) 
    return NULL; 
  INT lost_loops = Do_Loop_Depth(wn_new_outer) - Do_Loop_Depth(wn_outer);
  INT new_nloops = nloops - lost_loops; 
  SNL_Finalize_Index_Variables(wn_new_outer, new_nloops); 
  SNL_Sink_Out_Sandwiched_Statements(wn_new_outer, new_nloops, TRUE, dg, du); 
  return wn_new_outer; 
}

//-----------------------------------------------------------------------
// NAME: Safe_Depth 
// FUNCTION: For the access array 'aa' which represents a bound of an SNL
//   loop of the given 'depth', return the depth of the outermost loop for
//   which none of the access_vectors are too messy, contain non-linear 
//   symbols, nor contain non-constant loops. 
//-----------------------------------------------------------------------

static INT Safe_Depth(ACCESS_ARRAY* aa, 
		      INT depth)
{
  INT safe_depth = 0; 
  for (INT dim = 0; dim < aa->Num_Vec(); dim++) {
    ACCESS_VECTOR* av = aa->Dim(dim);
    if (av->Too_Messy || av->Contains_Non_Lin_Symb())
      return depth + 1; 
    if (av->Non_Const_Loops() > safe_depth)
      safe_depth = av->Non_Const_Loops(); 
  }
  return safe_depth;
}

//-----------------------------------------------------------------------
// NAME: General_Kernel 
// FUNCTION: For the SNL with outermost loop 'wn_outer' and containing 
//   'nloops' loops, return the outermost loop for which all of the loops
//   inside it (including it) are "good" do loops, have step sizes of 1, 
//   and have safe access vectors (according to the definition in 
//   'Safe_Depth' above.   
//-----------------------------------------------------------------------

static WN* General_Kernel(WN* wn_outer, 
			  INT nloops) 
{
  WN* wn_new_outer = NULL;
  INT safe_depth = Do_Loop_Depth(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  WN* wn = 0;
  for (wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue; 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    INT safe_depth_lb = Safe_Depth(dli->LB, dli->Depth); 
    if (safe_depth_lb > safe_depth)
      safe_depth = safe_depth_lb; 
    INT safe_depth_ub = Safe_Depth(dli->UB, dli->Depth); 
    if (safe_depth_ub > safe_depth)
      safe_depth = safe_depth_ub; 
    if (wn == wn_outer) 
      break;
  }
  for (wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP) 
      continue; 
    if (!Do_Loop_Is_Good(wn) || Do_Loop_Has_Exits(wn))
      break;
    if (Do_Loop_Depth(wn) < safe_depth)
      break; 
    if (Step_Size(wn) != 1) 
      break;
    wn_new_outer = wn;
  }
  return wn_new_outer;
}

//-----------------------------------------------------------------------
// NAME: Choose 
// FUNCTION: Returns the value of Choose(n,m).  The values 'n' and 'm' 
//   must be non-negative integers. 
//-----------------------------------------------------------------------

static INT Choose(INT n, 
		  INT m)
{ 
  FmtAssert(n >= 0 && m >= 0, ("Choose() takes a non-negative arguments"));
  return Factorial(n)/(Factorial(m)*Factorial(n-m));
} 

//-----------------------------------------------------------------------
// NAME: Choice 
// FUNCTION: Returns a bit vector of length 'n' (right-justified) with 
//   'm' bits set.  There are Choose(n,m) such values, and 'p' is an 
//   integer from 0 to Choose(n,m)-1 which indicates which of the bit 
//   vectors we want.
//-----------------------------------------------------------------------

static INT Choice(INT n, 
		  INT m, 
		  INT p)
{
  INT mm = m; 
  INT pp = p; 
  INT result = 0; 
  FmtAssert(n >= 0 && m >= 0, ("Choice() takes non-negative arguments")); 
  FmtAssert(p >= 0 && p < Choose(n,m), ("Invalid Choice() index")); 
  INT digit = 0; 
  for (INT nn = n; nn >= 1; nn--) { 
    INT cross = mm == 0 ? 0 : Choose(nn,mm) - Choose(nn-1,mm-1); 
    digit = mm == 0 ? 0 : pp < cross ? 0 : 1;    
    pp -= digit * cross; 
    result = (result << 1) + digit; 
    mm -= digit;  
  }
  return result; 
} 
    
//-----------------------------------------------------------------------
// NAME: Permutation_Vector 
// FUNCTION: Write the 'permutation' vector of length 'nloops', which has 
//   the 'parallel_loop' at 'parallel_index' in the 'permutation' and the
//   other loops distributed around it.  There are Choice(nloops - 1, 
//   parallel_index) such loops.  The 'choice' is a value from 0 to the 
//   above value - 1 which indicates which of these permutations are 
//   indicated.  
//-----------------------------------------------------------------------

static void Permutation_Vector(INT parallel_index, 
			       INT parallel_loop,  
			       INT choice, 
			       INT nloops, 
			       INT permutation[])
{
  permutation[parallel_index] = parallel_loop; 
  INT bv_choice = Choice(nloops - 1, parallel_index, choice); 
  INT perm_index = parallel_loop == 0 ? 1 : 0;
  INT before_index = 0; 
  INT after_index = parallel_index + 1; 
  for (INT i = nloops - 2; i >= 0; i--) { 
    INT before = (bv_choice >> i) & 1; 
    permutation[before ? before_index++ : after_index++] = perm_index++; 
    if (perm_index == parallel_loop)
      perm_index++;
  }
  Is_True(Is_Permutation_Vector(permutation, nloops),
    ("Not a permutation vector"));
}

//-----------------------------------------------------------------------
// NAME: SNL_Legal_Perm_Deps
// FUNCTION: Returns TRUE if the SNL whose dependences are summarized in 
//   'sdm_body' can be legally permuted with the given 'permutation of 
//   length 'nloops'.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL SNL_Legal_Perm_Deps(SNL_DEP_MATRIX* sdm_body, 
                                INT permutation[],
                                INT nloops) 
{
  if (sdm_body == NULL)
    return FALSE; 

  FmtAssert(sdm_body->Nloops() == nloops, 
    ("Permutation length and dep matrix length do not match"));  

  for (INT d = 0; d < sdm_body->Ndep(); d++) {
    for (INT i = 0; i < sdm_body->Nloops(); i++) {
      SNL_DEP dep = (*sdm_body)(d, permutation[i]);
      if (dep.Unbounded_Min() || dep.Min() < 0)
        return FALSE;
      else if (dep.Min() > 0)
        break;
    }
  }
  return TRUE;
} 

//-----------------------------------------------------------------------
// NAME: General_Permutation 
// FUNCTION: Returns TRUE if all loops in the SNL with outermost loop 
//   'wn_outer' containing 'nloops' loops can be permuted in the given
//   'permutation' by using the access vectors.  Returns FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

extern BOOL General_Permutation(WN* wn_outer, 
				INT permutation[], 
				INT nloops)
{
  WN* wn_new_outer = General_Kernel(wn_outer, nloops);  
  if (wn_new_outer == NULL) 
    return FALSE; 
  INT last = Do_Loop_Depth(wn_new_outer) - Do_Loop_Depth(wn_outer);
  for (INT i = 0; i < last; i++) 
    if (permutation[i] != i)
      return FALSE; 
  return TRUE;  
}

//-----------------------------------------------------------------------
// NAME: Invariant_Permutation 
// FUNCTION: Returns TRUE if all loops in the SNL with outermost loop 
//   'wn_outer' containing 'nloops' loops can be permuted in the given
//   'permutation' without rewriting the loop bounds.  Returns FALSE 
//   otherwise.
//-----------------------------------------------------------------------

extern BOOL Invariant_Permutation(WN* wn_outer,
                                 INT permutation[],
                                 INT nloops) 
{
  if (!General_Permutation(wn_outer, permutation, nloops))
    return FALSE; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT offset = Do_Loop_Depth(wn_outer);  
  for (INT i = 0; i < nloops; i++) { 
    for (INT j = i + 1; j < nloops; j++) { 
      if (permutation[i] > permutation[j] && !SNL_Is_Invariant(&stack, 
          offset + permutation[j], offset + permutation[i]))
        return FALSE; 
    } 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Fully_Permutable_Permutation 
// FUNCTION: Returns TRUE if the loops within the SNL with outermost loop 
//   'wn_outer' consisting of 'nloops' loops can be arbitrarily permuted 
//   without rewriting the loop bounds.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Fully_Permutable_Permutation(WN* wn_outer, 
				         INT nloops)
{
  INT* permutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool); 
  INT i;
  for (i = 0; i < nloops; i++) 
    permutation[i] = i;
  if (!General_Permutation(wn_outer, permutation, nloops))
    return FALSE;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  for (i = 2; i <= nloops; i++) {
    INT d = inner_depth + 1 - i;
    for (INT dd = d + 1; dd <= inner_depth; dd++) 
      if (!SNL_Is_Invariant(&stack, d, dd))
        return FALSE;
  }
  return TRUE;  
}

//-----------------------------------------------------------------------
// NAME: SNL_Perm_Retained_Section
// FUNCTION: Return TRUE if the imperfect code nested between the 
//   'section_depth' loop and the 'section_depth' + 1 loop will be 
//   retained when the 'permutation' of length 'nloops' is applied. 
//   Returns FALSE otherwise. 
// EXAMPLE: For the permutation [2 1 0 3 5 4], we return TRUE for a 
//   'section_depth' of 2, 3, and 5 and FALSE for 0, 1, and 4. 
//-----------------------------------------------------------------------

static BOOL SNL_Perm_Retained_Section(INT section_depth, 
				      INT permutation[], 
				      INT nloops)
{
  INT last = -1; 
  for (INT first = 0; first < nloops; first = last + 1) {
    last = Permutation_Last(first, permutation, nloops);
    if (last == section_depth) 
      return TRUE;  
    if (last > section_depth) 
      return FALSE; 
  } 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Is_Perfectly_Nested 
// FUNCTION: Returns TRUE if the SNL 'wn_outer' of 'nloops' loops, to which 
//   we are applying the 'permutation' of length 'nloops' will have sand-
//   wiched code between the loops at 'depth' and 'depth+1', after the 
//   transformation is applied.  
//-----------------------------------------------------------------------

static BOOL Is_Perfectly_Nested(WN* wn_outer, 
				INT permutation[], 
				INT nloops, 
			        INT depth)
{
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = outer_depth + nloops - 1;  
  FmtAssert(depth >= outer_depth && depth <= inner_depth - 1, 
    ("Is_Perfectly_Nested: Test depth outside allowed range")); 
  WN* wn_level = SNL_Get_Inner_Snl_Loop(wn_outer, depth - outer_depth + 1);  
  return !SNL_Perm_Retained_Section(depth - outer_depth, permutation, nloops)
    || WN_opcode(WN_first(WN_do_body(wn_level))) == OPC_DO_LOOP
    && WN_first(WN_do_body(wn_level)) == WN_last(WN_do_body(wn_level));
}

//-----------------------------------------------------------------------
// NAME: SNL_Perm_Retained_Section
// FUNCTION: Similar to the above, except that the SNL is distributed at 
//   'sd_split_depth' in addition to distributing for the 'permutation'.
//-----------------------------------------------------------------------

static BOOL SNL_Perm_Retained_Section(INT section_depth,
                                      INT permutation[],
                                      INT nloops, 
   				      INT sd_split_depth)
{
  if (sd_split_depth == -1)
    return SNL_Perm_Retained_Section(section_depth, permutation, nloops);
  return section_depth >= sd_split_depth 
    && SNL_Perm_Retained_Section(section_depth, permutation, nloops);
}

//-----------------------------------------------------------------------
// NAME: SNL_Dir_Cannot_Interchange
// FUNCTION: Returns TRUE if the SNL with outermost loop 'wn_outer' con-
//   sisting of 'nloops' cannot be permuted according to 'permutation' 
//   because of a NO INTERCHANGE directive.  Returns FALSE otherwise.   
//-----------------------------------------------------------------------

static BOOL SNL_Dir_Cannot_Interchange(WN* wn_outer, 
				       INT permutation[], 
				       INT nloops)
{
  if (!LNO_Interchange) 
    return TRUE; 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  for (INT i = outer_depth; i < outer_depth + nloops; i++) { 
    WN* wn_loop = stack.Bottom_nth(i);
    DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop); 
    if (dli_loop->Cannot_Interchange 
        && permutation[i - outer_depth] != i - outer_depth)
      return TRUE; 
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Inv_Dep_Info 
// FUNCTION: For the SNL with outermost loop 'wn_outer' which contains 
//   'nloops' loops, return an array of SNL_DEP_MATRIXes which summarizes 
//   the dependences in the body of the SNL which do NOT depend on the 
//   permutation order.  If 'check_privates', then exclude those depen-
//   dences which arise from privatizable arrays. 
//-----------------------------------------------------------------------

extern SNL_DEP_MATRIX** Inv_Dep_Info(WN* wn_outer, 
                                     INT nloops, 
				     BOOL check_privates,
				     BOOL definitely)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 

  // Create hash table of privatizable variables. 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT v_hash_table_size = MAX(MIN(dg->Get_Vertex_Count(), 512), 5);
  HASH_TABLE<WN*,INT> priv_table(v_hash_table_size, &LNO_local_pool);  
  if (check_privates) { 
    LWN_ITER* itr = LWN_WALK_TreeIter(wn_outer); 
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      WN* wn = itr->wn;
      OPERATOR opr = WN_operator(wn);
      if (opr != OPR_ILOAD && opr != OPR_ISTORE && opr != OPR_LDID
	  && opr != OPR_STID)
	continue;  
      VINDEX16 v = dg->Get_Vertex(wn);
      if (v == 0 && (opr == OPR_LDID || opr == OPR_STID))
	continue; 
      INT i;
      for (i = Do_Loop_Depth(wn_outer); i < stack.Elements(); i++) {
	WN* wn_loop = stack.Bottom_nth(i); 
	if (Is_Privatizable_With_Context(wn_loop, wn, definitely))
	    break;
      }
      if (i < stack.Elements())
	priv_table.Enter(wn, 1);
    }
  } 

  // Create SNL_DEP_INFO, excluding deps of privatizable variables. 
  INT i;
  for (i = 0; i < stack.Elements(); i++) 
    if (Do_Loop_Is_Good(stack.Bottom_nth(i)) && 
	!Do_Loop_Has_Exits(stack.Bottom_nth(i)))
      break; 
  INT nbad = i; 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  SNL_DEP_INFO** sdi_list = CXX_NEW_ARRAY(SNL_DEP_INFO*, inner_depth 
    - outer_depth + 1, &LNO_local_pool); 
  for (i = outer_depth; i <= inner_depth; i++)
    sdi_list[i - outer_depth] = CXX_NEW(SNL_DEP_INFO(outer_depth - nbad,
      i - outer_depth + 1, nbad, stack, &LNO_local_pool), &LNO_local_pool); 
  EINDEX16 e = 0; 
  INT e_hash_table_size = MIN(dg->Get_Edge_Count(), 512);
  HASH_TABLE<EINDEX16,INT> edge_table(e_hash_table_size, &LNO_local_pool);  
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_outer); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    INT wn_source_depth = Loop_Depth(wn); 
    REDUCTION_TYPE rt_source = rm != NULL ? rm->Which_Reduction(wn) : RED_NONE; 
    OPERATOR opr = WN_operator(wn);
    if (opr != OPR_ILOAD && opr != OPR_ISTORE && opr != OPR_LDID
        && opr != OPR_STID)
      continue; 
    BOOL Is_Wn_Array = opr == OPR_ILOAD || opr == OPR_ISTORE; 
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0 
#ifndef KEY // bug 7451
        && (opr == OPR_LDID || opr == OPR_STID)
#endif
	)
      continue; 
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      if (edge_table.Find(e))
        continue;
      edge_table.Enter(e, 1);
      WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
      OPERATOR opr_sink = WN_operator(wn_sink); 
      BOOL Is_Wn_Sink_Array = opr_sink == OPR_ILOAD || opr_sink == OPR_ISTORE; 
      INT wn_sink_depth = Loop_Depth(wn_sink); 
      REDUCTION_TYPE rt_sink = rm != NULL ? rm->Which_Reduction(wn_sink)
	: RED_NONE; 
      if (!Wn_Is_Inside(wn_sink, wn_outer))
        continue; 
      INT list_index = wn_source_depth < wn_sink_depth 
        ? wn_source_depth - outer_depth : wn_sink_depth - outer_depth; 
      if (list_index > nloops - 1)
        list_index = nloops - 1;  
      if (!priv_table.Find(wn) 
	  && (rt_source == RED_NONE || rt_source != rt_sink)) {
        if (!sdi_list[list_index]->All_Stars())
          sdi_list[list_index]->Enter(dg->Depv_Array(e), e, TRUE); 
      }
    }
  } 

  // Return a SNL_DEP_MATRIX corresponding to the SNL_DEP_INFO. 
  SNL_DEP_MATRIX** sdm_list = CXX_NEW_ARRAY(SNL_DEP_MATRIX*, inner_depth
    - outer_depth + 1, &LNO_local_pool); 
  for (i = 0; i < inner_depth - outer_depth + 1; i++) {
    if (sdi_list[i]->All_Stars()) { 
      sdm_list[i] = NULL; 
    } else { 
      SNL_DEP_MATRIX* sdm_local = CXX_NEW(SNL_DEP_MATRIX(*sdi_list[i], 
	&LNO_local_pool), &LNO_local_pool); 
      sdm_list[i] = sdm_local; 
    } 
  } 
  return sdm_list; 
} 

//-----------------------------------------------------------------------
// NAME: Invariant_Red_Depth 
// FUNCTION: Let 'wn_array' be an OPR_ARRAY reduction node in the SNL with 
//   outermost loop 'wn_outer' containing 'nloops' loops, to which we are 
//   applying the 'permutation' of 'nloops' loops.  Return the outermost 
//   depth that we may parallelize due to this reduction.  
//-----------------------------------------------------------------------

static INT Invariant_Red_Depth(WN* wn_array, 
			       WN* wn_outer, 
			       INT permutation[],
                               INT nloops)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 
  FmtAssert(rm != NULL, ("Test requires reduction manager")); 

  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = Loop_Depth(wn_array); 
  INT invariant_depth = outer_depth; 
  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    if (av->Too_Messy)
      return outer_depth + nloops; 
    if (inner_depth >= outer_depth + nloops) {
      INT j;
      for (j = inner_depth; j >= outer_depth + nloops; j--)
	if (av->Loop_Coeff(j) != 0)
	  break;
      if (j + 1 > invariant_depth)
	invariant_depth = j + 1; 
    } 
    INT j;
    for (j = outer_depth + nloops - 1; j >= outer_depth; j--) 
      if (av->Loop_Coeff(outer_depth + permutation[j - outer_depth]) != 0) 
	break; 
    if (j + 1 > invariant_depth)
      invariant_depth = j + 1;  
  } 
  WN* wn_red = LWN_Get_Parent(wn_array); 
  EINDEX16 e = 0;
  VINDEX16 v = dg->Get_Vertex(wn_red);
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
    if (rm->Which_Reduction(wn_source) == rm->Which_Reduction(wn_red))
      continue; 
    WN* wn_common = LNO_Common_Loop(wn_red, wn_source); 
    if (wn_common == NULL) 
      continue; 
    DEPV_ARRAY* dv = dg->Depv_Array(e);
    INT lcd_depth = dv->Loop_Carrying_Dependence();
    if (lcd_depth + 1 > invariant_depth)
      invariant_depth = lcd_depth == -1 ? Do_Loop_Depth(wn_common) + 1
	: lcd_depth + 1; 
  }
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
    if (rm->Which_Reduction(wn_sink) == rm->Which_Reduction(wn_red))
      continue; 
    WN* wn_common = LNO_Common_Loop(wn_red, wn_sink); 
    if (wn_common == NULL) 
      continue; 
    DEPV_ARRAY* dv = dg->Depv_Array(e);
    INT lcd_depth = dv->Loop_Carrying_Dependence();
    if (lcd_depth + 1 > invariant_depth)
      invariant_depth = lcd_depth == -1 ? Do_Loop_Depth(wn_common) + 1
	: lcd_depth + 1; 
  } 
  return invariant_depth; 
}

//-----------------------------------------------------------------------
// NAME: Red_Dep_Info 
// FUNCTION: For the SNL with outermost loop 'wn_outer' which contains 
//   'nloops' loops, return an array of SNL_DEP_MATRIXes which summarizes 
//   the dependences in the body of the SNL which DO depend on the 
//   permutation order (notably array reductions).  The 'parallel_depth' 
//   is the depth of the parallel loop in the transformed nest. 
//-----------------------------------------------------------------------

static SNL_DEP_MATRIX** Red_Dep_Info(WN* wn_outer, 
				     INT permutation[], 
                                     INT nloops, 
				     INT parallel_depth,
				     BOOL check_privates, 
				     BOOL definitely)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 

  // Create hash table of privatizable variables. 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT v_hash_table_size = MAX(MIN(dg->Get_Vertex_Count(), 512), 5);
  HASH_TABLE<WN*,INT> priv_table(v_hash_table_size, &LNO_local_pool);  
  if (check_privates) { 
    LWN_ITER* itr = LWN_WALK_TreeIter(wn_outer); 
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      WN* wn = itr->wn;
      OPERATOR opr = WN_operator(wn);
      if (opr != OPR_ILOAD && opr != OPR_ISTORE && opr != OPR_LDID
	  && opr != OPR_STID)
	continue;  
      VINDEX16 v = dg->Get_Vertex(wn);
      if (v == 0 && (opr == OPR_LDID || opr == OPR_STID))
	continue; 
      INT i;
      for (i = Do_Loop_Depth(wn_outer); i < stack.Elements(); i++) {
	WN* wn_loop = stack.Bottom_nth(i); 
	if (Is_Privatizable_With_Context(wn_loop, wn, definitely))
	  break;
      }
      if (i < stack.Elements())
	priv_table.Enter(wn, 1);
    }
  } 

  // Create SNL_DEP_INFO for reduction variables. 
  INT i;
  for (i = 0; i < stack.Elements(); i++) 
    if (Do_Loop_Is_Good(stack.Bottom_nth(i)) && 
	!Do_Loop_Has_Exits(stack.Bottom_nth(i)))
      break; 
  INT nbad = i; 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  SNL_DEP_INFO** sdi_list = CXX_NEW_ARRAY(SNL_DEP_INFO*, inner_depth 
    - outer_depth + 1, &LNO_local_pool); 
  for (i = outer_depth; i <= inner_depth; i++)
    sdi_list[i - outer_depth] = CXX_NEW(SNL_DEP_INFO(outer_depth - nbad,
      i - outer_depth + 1, nbad, stack, &LNO_local_pool), &LNO_local_pool); 
  EINDEX16 e = 0; 
  INT e_hash_table_size = MIN(dg->Get_Edge_Count(), 512);
  HASH_TABLE<EINDEX16,INT> edge_table(e_hash_table_size, &LNO_local_pool);  
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_outer); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (priv_table.Find(wn))
      continue; 
    INT wn_source_depth = Loop_Depth(wn); 
    REDUCTION_TYPE rt_source = rm != NULL ? rm->Which_Reduction(wn) : RED_NONE;
    if (rt_source == RED_NONE)
      continue; 
    OPERATOR opr = WN_operator(wn);
    if (opr != OPR_ILOAD && opr != OPR_ISTORE) 
      continue; 
    WN* wn_array = opr == OPR_ILOAD ? WN_kid0(wn) : WN_kid1(wn);
    if (Invariant_Red_Depth(wn_array, wn_outer, permutation, nloops) 
	<= parallel_depth)
      continue; 
    VINDEX16 v = dg->Get_Vertex(wn);
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      if (edge_table.Find(e))
        continue;
      edge_table.Enter(e, 1);
      WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
      OPERATOR opr_sink = WN_operator(wn_sink); 
      if (opr_sink != OPR_ILOAD && opr_sink != OPR_ISTORE)
	continue; 
      INT wn_sink_depth = Loop_Depth(wn_sink); 
      REDUCTION_TYPE rt_sink = rm != NULL ? rm->Which_Reduction(wn_sink)
	: RED_NONE; 
      if (rt_sink == RED_NONE)
        continue;
      if (rt_source != rt_sink)
	continue; 
      if (!Wn_Is_Inside(wn_sink, wn_outer))
        continue; 
      INT list_index = wn_source_depth < wn_sink_depth 
        ? wn_source_depth - outer_depth : wn_sink_depth - outer_depth; 
      if (list_index > nloops - 1)
        list_index = nloops - 1;  
      if (!sdi_list[list_index]->All_Stars())
        sdi_list[list_index]->Enter(dg->Depv_Array(e), e, TRUE); 
    }
  } 

  // Return a SNL_DEP_MATRIX corresponding to the SNL_DEP_INFO. 
  SNL_DEP_MATRIX** sdm_list = CXX_NEW_ARRAY(SNL_DEP_MATRIX*, inner_depth
    - outer_depth + 1, &LNO_local_pool); 
  for (i = 0; i < inner_depth - outer_depth + 1; i++) {
    if (sdi_list[i]->All_Stars()) { 
      sdm_list[i] = NULL; 
    } else { 
      SNL_DEP_MATRIX* sdm_local = CXX_NEW(SNL_DEP_MATRIX(*sdi_list[i], 
	&LNO_local_pool), &LNO_local_pool); 
      sdm_list[i] = sdm_local; 
    } 
  } 
  return sdm_list; 
} 
        
//-----------------------------------------------------------------------
// NAME: Is_Legal_Permutation 
// FUNCTION: For the SNL with outermost loop 'wn_outer' contating 'nloops'
//   loops return the appropriate interchange type: 
//    INT_NONE: It is not legal to apply either general or invariant 
//      interchange to this 'permutation' of length 'nloops'. 
//    INT_INVARIANT: It is legal to apply invariant interchange to 
//      this 'permutation' of length 'nloops'.
//    INT_GENERAL: It is legal to apply general interchange to this 
//      'permutation' of length 'nloops'.  
//   Also: 'sd_Parallelizablesplit_depth' is set with the depth at which 
//   we must apply distribution to remove unexpandable scalars. 
// NOTES: 
//   (1) If both invariant and general interchange can be performed, 
//       we prefer invariant interchange.
//   (2) The 'sx_info' contains information relating to expanding scalars. 
//   (3) The 'sd_info' contains information relating to distributing out 
//       unexpandable scalars. 
//   (4) The 'sdm_inv' summarizes array dependence information. 
//-----------------------------------------------------------------------

static INTERCHANGE_TYPE Is_Legal_Permutation(WN* wn_outer, 
				 	     INT permutation[], 
				 	     INT nloops,
				             SX_INFO* sx_info, 
					     SD_INFO* sd_info, 
					     SNL_DEP_MATRIX** sdm_inv, 
					     INT* sd_split_depth, 
					     BOOL is_fully_permutable) 
{ 
  // Record parallel debug level. 
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;

  // The identity permutation is always legal. 
  INT i;
  for (i = 0; i < nloops; i++)
    if (permutation[i] != i) 
      break;
  if (i == nloops) 
    return is_fully_permutable ? INT_PERMUTABLE : INT_INVARIANT;  

  if (SNL_Dir_Cannot_Interchange(wn_outer, permutation, nloops)) {
    if (parallel_debug_level >= 1)
      fprintf(stdout, "  Directive prevents interchange.\n");
    return INT_NONE; 
  } 

  // Test for scalar expandability. 
  SE_STATUS se_result = SNL_Is_Scalar_Expandable(wn_outer, permutation, 
      nloops, sx_info, FALSE);
  if (se_result == SE_FALSE) {
    if (parallel_debug_level >= 1)
      fprintf(stdout, "  Could not scalar expand.\n"); 
    return INT_NONE; 
  } 

  // Test for distribution to remove unexpandable scalars. 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  *sd_split_depth = -1; 
  if (se_result == SE_MAYBE) {
    *sd_split_depth = SNL_Bad_Scalars_Are_Distributable(wn_outer, 
      permutation, nloops, sx_info, sd_info);
    if (*sd_split_depth == outer_depth + nloops) {
      if (parallel_debug_level >= 1)
	fprintf(stdout, "  Could not remove unexpandable scalars.\n"); 
      return INT_NONE; 
    } 
  }

  // Test for distributability. 
  if (!SNL_Permutation_Is_Distributable(wn_outer, permutation, nloops)) {
    if (parallel_debug_level >= 1) 
      fprintf(stdout, "  Could not distribute.\n"); 
    return INT_NONE;
  } 

  // Test for legal permutation vector. 
  for (i = 0; i < nloops; i++) { 
    if (SNL_Perm_Retained_Section(i, permutation, nloops, *sd_split_depth)
        && !SNL_Legal_Perm_Deps(sdm_inv[i], permutation, i + 1)) {
      if (parallel_debug_level >= 1)
	fprintf(stdout, "  Loop-carried dependence.\n"); 
      return INT_NONE; 
    } 
  }

  // Test for legal permutation bounds. 
  if (is_fully_permutable)
    return INT_PERMUTABLE; 
  if (Invariant_Permutation(wn_outer, permutation, nloops))
    return INT_INVARIANT; 
  if (General_Permutation(wn_outer, permutation, nloops))
    return INT_GENERAL; 
  return INT_NONE; 
}

//-----------------------------------------------------------------------
// NAME: Print_Permutation_Vector 
// FUNCTION: Print the 'permutation' vector of length 'nloops' on the file 
//   'fp', marking the parallel loop at depth 'parallel_depth' with "P".  
//-----------------------------------------------------------------------

static void Print_Permutation_Vector(FILE* fp, 
				     INT permutation[], 
				     INT nloops, 
				     INT parallel_depth,
				     BOOL is_doacross)
{
  fprintf(fp, "Testing permutation ["); 
  if (!is_doacross)
    for (INT l = 0; l < nloops; l++) {
      fprintf(fp, "%d%s", permutation[l], l == parallel_depth ? "-P" : ""); 
      if (l < nloops - 1)
        fprintf(fp, ","); 
    }
  else
    for (INT l = 0; l < nloops; l++) {
      fprintf(fp, "%d%s", permutation[l], l == parallel_depth ? "-X" : ""); 
      if (l < nloops - 1)
        fprintf(fp, ","); 
    } 
  fprintf(fp, "]\n"); 
}
 
//-----------------------------------------------------------------------
// NAME: Is_Legal_Permutation_Class 
// FUNCTION: Attempts to find a legal permutation in the set of permuta-
//   tions for which 'permutation' is a representative.  If so, returns
//   an INTERCHANGE_TYPE different from INT_NONE, and sets the value of 
//   'permutation' to that legal permutation.  Otherwise, returns INT_NONE. 
// EXAMPLE: If passed the permutation [0 2 1 3 4], with 'parallel_depth' 
//   - 'Do_Loop_Depth(wn_outer)' == 2, we should try the following per-
//   mutations: 
//     [0 2 1 3 4], [0 2 1 4 3], [2 0 1 3 4], and [2 0 1 4 3].  
//-----------------------------------------------------------------------

static INTERCHANGE_TYPE Is_Legal_Permutation_Class(WN* wn_outer,
                                                   INT permutation[],
                                                   INT nloops,
						   INT parallel_depth, 
                                                   SX_INFO* sx_info,
                                                   SD_INFO* sd_info,
                                                   SNL_DEP_MATRIX** sdm_inv,
                                                   INT* sd_split_depth,
						   BOOL print_message, 
						   BOOL is_fully_permutable)
{
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT parallel_index = parallel_depth - outer_depth; 
  INT aloops = parallel_index; 
  INT bloops = nloops - parallel_index - 1; 
  INT* aperm = CXX_NEW_ARRAY(INT, aloops, &LNO_local_pool); 
  INT* bperm = CXX_NEW_ARRAY(INT, bloops, &LNO_local_pool); 
  INT* nperm = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool); 
  nperm[parallel_index] = permutation[parallel_index]; 
  INT i;
  for (i = 0; i < Factorial(aloops); i++) { 
    Permutation(i, aloops, aperm); 
    INT j;
    for (j = 0; j < aloops; j++) 
      nperm[j] = permutation[aperm[j]]; 
    for (j = 0; j < Factorial(bloops); j++) { 
      Permutation(j, bloops, bperm); 
      for (INT k = 0; k < bloops; k++) 
        nperm[k + aloops + 1] = permutation[bperm[k] + aloops + 1]; 
      if (print_message) { 
	Print_Permutation_Vector(stdout, nperm, nloops, parallel_index, FALSE); 
	Print_Permutation_Vector(TFile, nperm, nloops, parallel_index, FALSE); 
      } 
      INTERCHANGE_TYPE int_type = Is_Legal_Permutation(wn_outer, nperm, 
	nloops, sx_info, sd_info, sdm_inv, sd_split_depth, 
	is_fully_permutable);
      if (int_type != INT_NONE) { 
	for (INT l = 0; l < nloops; l++) 
	  permutation[l] = nperm[l]; 
	return int_type; 
      }
    }
  }
  return INT_NONE; 
}

//-----------------------------------------------------------------------
// NAME: Parallel_Interchange 
// FUNCTION: Interchange the SNL with outermost loop 'wn_outer' containing 
//   'nloops' loops and list of expandable scalars 'sx_info' according to the
//   given 'permutation'.  Apply the type of permutation given by 'int_type'.
//   Return the new outermost loop in the SNL. 
// The parallel_interchange actually takes three steps: 
//   (1) Distribution at 'sd_split_level' to remove unexpandable scalars. 
//   (2) Distribute and permute the loops to get the desired 'permutation' 
//       of 'nloops' loops. 
//   (3) Distribute at 'split_level' to remove scalar and array dependences
//       which inhibit parallelization. 
// NOTES: Assumes that the permutation is legal. 
//-----------------------------------------------------------------------

static WN* Parallel_Interchange(WN* wn_outer,       
                                INT permutation[],
                                INT nloops, 
				SD_INFO* sd_info, 
		                SX_INFO* sx_info, 
				INTERCHANGE_TYPE int_type, 
				INT sd_split_depth, 
				INT split_depth, 
				DOLOOP_STACK* stack)
{
  INT outer_depth = Do_Loop_Depth(wn_outer);
  if (split_depth == outer_depth && Identity_Permutation(permutation, nloops))
    return wn_outer;
 
  BOOL sinv = int_type == INT_PERMUTABLE; 
  BOOL inv = int_type == INT_PERMUTABLE || int_type == INT_INVARIANT; 
  BOOL sd_ignore_illegals = sd_split_depth > 0 && sd_split_depth 
    < outer_depth + nloops; 
  BOOL ignore_illegals = split_depth > 0 && split_depth 
    < outer_depth + nloops; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  SNL_Scalar_Expand_For_Splitting(wn_outer, wn_inner, sd_split_depth, 
    &sx_info->Plist, &sd_info->Plist, sinv, sd_ignore_illegals, FALSE); 
  SNL_Scalar_Expand(wn_outer, wn_inner, permutation, nloops, 
    sx_info, sinv, sd_ignore_illegals, FALSE); 
  SNL_Scalar_Expand_For_Splitting(wn_outer, wn_inner, split_depth, 
    &sx_info->Plist, &sd_info->Plist, sinv, ignore_illegals, FALSE); 
  SNL_Distribute_By_Splitting(wn_outer, wn_inner, nloops, sd_split_depth, 
    stack); 
  SNL_Distribute_For_Permutation(wn_outer, wn_inner, permutation, nloops, 
    stack);
  SNL_Distribute_By_Splitting(wn_outer, wn_inner, nloops, split_depth, 
    stack);
  WN* wn_new_outer = SNL_Permute_Loops(wn_outer, wn_inner, permutation, 
    nloops, inv, FALSE); 
  return wn_new_outer; 
} 

//-----------------------------------------------------------------------
// NAME: Splittable
// FUNCTION: Returns the level of the loop in the SNL with outermost loop 
//   'wn_outer' and 'nloops' loops which can be retained in the parallel-
//   ized code after splitting.  Assume that 'split_depth' is the maximum
//   allowable distribution level.  Return  Do_Loop_Depth(wn_outer) + 
//   nloops' if splitting does not enable parallelization.  Information 
//   about array dependences is stored in 'sx_info'.  Information about 
//   scalar dependences is stored in 'sd_info'.  The BOOL 'is_privatizable'
//   is TRUE if we know that all of the scalars can be privatized for 
//   parallelization.
// NOTES: Each scalar may be handled in one of three ways: privatized, 
//   expanded, or removed by distribution.  Arrays are handled only by 
//   distribution.
//-----------------------------------------------------------------------

static INT Splittable(WN* wn_outer, 
		      INT split_depth, 
		      INT nloops, 
		      SX_INFO* sx_info,
		      SD_INFO* sd_info, 
		      BOOL is_privatizable) 
{
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack); 
  
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT best_split_depth = split_depth; 
  if (!is_privatizable || split_depth > outer_depth) { 
    SE_STATUS se_status = SNL_Is_Scalar_Expandable(wn_outer, NULL, nloops, 
      sx_info, FALSE);
    if (se_status == SE_FALSE) 
      return outer_depth + nloops; 
    if (se_status == SE_MAYBE) {
      INT upper_range = sd_info->Distribution_Range(outer_depth, sx_info);
      if (upper_range + 1 > best_split_depth)
	best_split_depth = upper_range + 1; 
      if (best_split_depth >= outer_depth + nloops)
        return outer_depth + nloops;
    }
  } 
  if (best_split_depth <= outer_depth)
    return -1;                 
  WN* wn_split = stack.Bottom_nth(best_split_depth); 
  if (!SNL_Is_Distributable(wn_outer, wn_outer, wn_split, TRUE))
    return outer_depth + nloops; 
  if (!SNL_Is_Distributable(wn_outer, wn_outer, wn_split, FALSE))
    return outer_depth + nloops; 
  return best_split_depth; 
}

//-----------------------------------------------------------------------
// NAME: Parallelizable_At_Depth 
// FUNCTION: For the SNL with outermost loop 'wn_outer' containg 'nloops'
//   loops, returns a 'split_depth' which determines if the loop at depth i
//   'parallel_depth' is parallelizable after the 'permutation' is applied.  
//   The 'split_depth' will satisfy:   
//     split_depth >= Do_Loop_Depth(wn_outer) 
//     split_depth <= Do_Loop_Depth(wn_outer) + nloops
//   If 'split_depth == Do_Loop_Depth(wn_outer) + nloops', it is not pos-
//     sible to parallelize this permutation of loops at 'parallel_depth'.
//   If 'split_depth == Do_Loop_Depth(wn_outer)', it is possible to par- 
//     allelize this permutation of loops at 'parallel_depth' as it is. 
//   If 'split_depth' has some other value between these two extremes, 
//     it is possible to distribute the nest into two nests: 
//       [Do_Loop_Depth(wn_outer) .. split_depth - 1] and 
//       [split_depth .. Do_Loop_Depth(wn_outer) + nloops - 1] 
//     and parallelize the second of the two nests.  
//  NOTES:             
//   (1) Information about array dependences is given in 'sdm_body'. 
//   (2) Information about scalar dependences is given in 'sdm_scl'. 
//   (3) Information about expandable scalars is given in 'sx_info'. 
//   (4) Information about unexpandable scalars is given in 'sd_info'. 
//   (5) To get the desired permutation, the SNL must be distributed at 
//       the level 'sd_split_depth' so that unexpandable scalars can be
//       removed. 
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

static INT Parallelizable_At_Depth(WN* wn_outer, 
				   INT nloops, 
				   INT permutation[], 
				   SNL_DEP_MATRIX** sdm_body, 
				   BOOL sdm_scl[], 
				   SX_INFO* sx_info, 
				   SD_INFO* sd_info, 
				   INT sd_split_depth, 
				   INT parallel_depth)
{
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT outer_depth = Do_Loop_Depth(wn_outer);
  BOOL need_test = FALSE; 
  WN* wn_parallel_loop = stack.Bottom_nth(parallel_depth);
  DO_LOOP_INFO* dli_parallel = Get_Do_Loop_Info(wn_parallel_loop);
  if (dli_parallel->Has_Exits)
    return outer_depth + nloops;
  BOOL is_privatizable = !sdm_scl[permutation[parallel_depth - outer_depth]];
  for (INT k = nloops - 1; k >= parallel_depth - outer_depth; k--) { 
    if (!SNL_Perm_Retained_Section(k, permutation, nloops, sd_split_depth))
      continue;
    if (sdm_body[k] == NULL) {
      if (k == nloops - 1) 
	return outer_depth + nloops; 
      INT split_depth = outer_depth + k + 1; 
      return Splittable(wn_outer, split_depth, nloops, sx_info, sd_info,
        is_privatizable);
    } 
    for (INT i = 0; i < sdm_body[k]->Ndep(); i++) {
      for (INT j = 0; j <= parallel_depth - outer_depth; j++) {
	SNL_DEP dep = (*sdm_body[k])(i, permutation[j]); 
	if (j < parallel_depth - outer_depth) {
	  if (dep.Moreless == SNL_DEP::SNL_DEP_PLUS 
	      || dep.Moreless == SNL_DEP::SNL_DEP_EXACT && dep.Distance > 0) 
	    break; 
	} else { 
	  FmtAssert(j == parallel_depth - outer_depth, 
	    ("Index out of range")); 
	  if (dep.Moreless == SNL_DEP::SNL_DEP_EXACT && dep.Distance == 0) 
	    break;
          if (k == nloops - 1) 
	    return outer_depth + nloops; 
          INT split_depth = outer_depth + k + 1; 
	  return Splittable(wn_outer, split_depth, nloops, sx_info, sd_info,
	    is_privatizable);
	}
      }
    }
  } 
  return Splittable(wn_outer, -1, nloops, sx_info, sd_info, is_privatizable);  
}
   
//-----------------------------------------------------------------------
// NAME: Parallelizable 
// FUNCTION: For the SNL with outermost loop 'wn_outer' containing 'nloops'
//   loops in which we parallelize the loop at depth 'parallel_depth', 
//   return the split_depth, which is the depth of the loop above and 
//   below which we must distribute to make this permutation paralleliza-
//   ble.  Return -1 if no distribution is necessary.  Return the depth 
//   of 'wn_outer' + 'nloops' if it is not possible to make this combi-
//   nation parallel.   
// NOTES: 
//   (1) Information about array dependences is given in 'sdm_inv'. 
//   (2) Information about scalar dependences is given in 'sdm_scl'. 
//   (3) Information about expandable scalars is given in 'sx_info'. 
//   (4) Information about unexpandable scalars is given in 'sd_info'. 
//   (5) To get the desired permutation, the SNL must be distributed at 
//       the level 'sd_split_depth' so that unexpandable scalars can be
//       removed. 
//-----------------------------------------------------------------------

static INT Parallelizable(WN* wn_outer, 
                         INT permutation[], 
		         INT nloops, 
		         INT parallel_depth, 
		         SNL_DEP_MATRIX** sdm_inv,
		         BOOL sdm_scl[], 
		         SX_INFO* sx_info, 
		         SD_INFO* sd_info, 
		         INT sd_split_depth)
{
  SNL_DEP_MATRIX** sdm_red = Red_Dep_Info(wn_outer, permutation, nloops, 
    parallel_depth, TRUE, FALSE); 
  INT inv_split_depth = Parallelizable_At_Depth(wn_outer, nloops, 
    permutation, sdm_inv, sdm_scl, sx_info, sd_info, sd_split_depth, 
    parallel_depth);
  INT red_split_depth = Parallelizable_At_Depth(wn_outer, nloops, 
    permutation, sdm_red, sdm_scl, sx_info, sd_info, sd_split_depth, 
    parallel_depth);
  INT split_depth = inv_split_depth > red_split_depth ? inv_split_depth :
    red_split_depth; 
  return split_depth; 
} 

//-----------------------------------------------------------------------
// NAME: Scl_Dep_Info
// FUNCTION: Returns an array of BOOL of length 'nloops' which indicates 
//  which of the loops in the SNL with outermost loop 'wn_outer' with 
//  'nloops' loops scalars which prevent parallelization.  
//-----------------------------------------------------------------------

static BOOL* Scl_Dep_Info(WN* wn_outer,
                          INT nloops)
{
  BOOL* scl_list = CXX_NEW_ARRAY(BOOL, nloops, &LNO_local_pool); 
  for (INT i = 0; i < nloops; i++) 
    scl_list[i] = FALSE; 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_outer);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn_scalar = itr->wn; 
    OPERATOR opr_scalar = WN_operator(wn_scalar); 
    if (opr_scalar != OPR_LDID && opr_scalar != OPR_STID)
      continue; 
    for (WN* wn = wn_scalar; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_DO_LOOP) {
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
#ifdef KEY //bug 12049: we are only interested in the chunk of 'nloops' 
           //loops with wn_outer as the outermost
        if(dli->Depth  < outer_depth + nloops)
#endif	  
	if (dli->ARA_Info->Is_Problem_Scalar(wn_scalar))
	  scl_list[dli->Depth - outer_depth] = TRUE; 
      }
      if (wn == wn_outer)
	break; 
    }
  }
  return scl_list; 
}      

//-----------------------------------------------------------------------
// NAME: Parallel_Loop 
// FUNCTION: Find the parallel loop indicated by the 'parallel_info' for
//   the SNL whose loops are contained on the stack 'loop_stack' .  
//-----------------------------------------------------------------------

static WN* Parallel_Loop(PARALLEL_INFO* parallel_info,
			 DOLOOP_STACK* loop_stack)
{
  if (parallel_info->Parallel_Depth() < 0)
    return NULL; 
  INT i;
  for (i = 0; i < loop_stack->Elements(); i++) {
    WN* wn_loop = loop_stack->Bottom_nth(i); 
    if (Do_Loop_Depth(wn_loop) == parallel_info->Parallel_Depth())
      break;
  } 
  WN* wn_parallel = loop_stack->Bottom_nth(i); 
  return wn_parallel; 
}

//-----------------------------------------------------------------------
// NAME: Print_Parallel_Loop 
// FUNCTION: Print a message about the parallel loop indicated by 'para-
//  llel_info' for the SNL whose loops are contained on the stack 'loop_
//  stack' on the file with descriptor 'fp'.  
//-----------------------------------------------------------------------

static void Print_Parallel_Loop(FILE* fp, 
				PARALLEL_INFO* parallel_info,
				DOLOOP_STACK* loop_stack)
{
  WN* wn_parallel = Parallel_Loop(parallel_info, loop_stack); 
  if (wn_parallel == NULL) 
    return; 
  INT nloops = parallel_info->Nloops(); 
  INT inner_depth = loop_stack->Elements() - 1; 
  INT outer_depth = inner_depth - nloops + 1; 
  fprintf(fp, "Auto Parallelizing Loop %s at %d ", 
    WB_Whirl_Symbol(wn_parallel), Srcpos_To_Line(WN_linenum(wn_parallel))); 
  fprintf(fp, "using (");
  for (INT i = 0; i < parallel_info->Nloops(); i++) {
    fprintf(fp, "%d%s", parallel_info->Permutation(i), 
      parallel_info->Parallel_Depth() == i + outer_depth ?
      (parallel_info->Is_Doacross() ? "-X" : "-P") : "");;
    if (i < parallel_info->Nloops() - 1) 
      fprintf(fp, ",");
  } 
  fprintf(fp, ")\n"); 
}

//-----------------------------------------------------------------------
// NAME: Outermore_Parallel_Construct 
// FUNCTION: Returns TRUE if some parallel construct encloses the loop 
//   'wn_loop' or if 'check_suggested' 'wn_loop' has the 'Suggested_Parallel' 
//   bit set in the DO_LOOP_INFO.
//-----------------------------------------------------------------------

extern BOOL Outermore_Parallel_Construct(WN* wn_loop, 
				         BOOL check_suggested) 
{
  if (Do_Loop_Is_Mp(wn_loop))
    return FALSE; 

  for (WN* wn = LWN_Get_Parent(wn_loop); wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (check_suggested && dli->Suggested_Parallel) 
	return TRUE;
    }
    if (Is_Mp_Region(wn))
      return TRUE; 
 }
 return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Outermore_Parallel_Construct_Or_Lego_Loop 
// FUNCTION: Returns TRUE if some construct outside of 'wn_loop' is a 
//   parallel construct. 
// NOTE: Also exclude lego tiled loops, at least for now. 
//-----------------------------------------------------------------------

extern BOOL Outermore_Parallel_Construct_Or_Lego_Loop(WN* wn_loop) 
{
  if (Do_Loop_Is_Mp(wn_loop))
    return FALSE; 

  for (WN* wn = LWN_Get_Parent(wn_loop); wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (dli->Is_Outer_Lego_Tile || dli->Is_Inner_Lego_Tile)
	return TRUE;  
      if (dli->Suggested_Parallel) 
	return TRUE;
    }
    if (Is_Mp_Region(wn))
      return TRUE; 
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Innermore_Parallel_Loop 
// FUNCTION: Return TRUE if 'wn_loop' or any loop enclosed by it is already
//   a parallel loop.  If 'check_suggested', include loops for which the 
//   'Suggested_Parallel' bit is set in the DO_LOOP_INFO.
//-----------------------------------------------------------------------

extern BOOL Innermore_Parallel_Loop(WN* wn_loop,
				    BOOL check_suggested)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (check_suggested && dli->Suggested_Parallel) 
	return TRUE; 
      if (Do_Loop_Is_Mp(wn))
        return TRUE; 
    }
  }
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Innermore_Parallel_Or_Lego_Loop 
// FUNCTION: Return TRUE if 'wn_loop' or any loop enclosed by it is already
//   a parallel loop. 
// NOTE: Also exclude lego tiled loops, at least for now. 
//-----------------------------------------------------------------------

extern BOOL Innermore_Parallel_Or_Lego_Loop(WN* wn_loop)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (dli->Is_Outer_Lego_Tile || dli->Is_Inner_Lego_Tile)
	return TRUE;  
      if (Do_Loop_Is_Mp(wn))
        return TRUE; 
    }
  }
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_All_Parallelizable 
// FUNCTION: Return TRUE if all of the loops in the SNL with outermost 
//   loop 'wn_outer' containing 'nloops' loops are already mark as paral-
//   lelizable, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL SNL_All_Parallelizable(WN* wn_outer, 
				   INT nloops) 
{
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (!dli->Parallelizable) 
	return FALSE; 
      if (wn == wn_outer) 
	return TRUE; 
    } 
  } 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Parallel_Directive_Class
// FUNCTION: Returns TRUE if some loop in the SNL whose outer loop is 
//   'wn_outer_loop' and which contains 'nloops' loops has a PREFER 
//   CONCURRENTIZE directive, FALSE otherwise.  More importantly, sets
//   'par_dir[i]' to PD_NO_CONCURRENT if it has a NO CONCURRENTIZE 
//   directive, to PD_PREFER_CONCURRENT if it has a PREFER CONCURREN-
//   TIZE directive, and to PD_NONE if it has neither. 
//-----------------------------------------------------------------------

static BOOL Parallel_Directive_Class(WN* wn_outer_loop, 
				     INT nloops, 
				     PAR_DIR_TYPE par_dir[]) 
{
  INT outer_depth = Do_Loop_Depth(wn_outer_loop); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer_loop, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  BOOL has_prefer_parallel = FALSE; 
  for (INT i = 0; i < nloops; i++) { 
    WN* wn_loop = stack.Bottom_nth(outer_depth + i); 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
    if (dli->Pragma_Cannot_Concurrentize) 
      par_dir[i] = PD_NO_CONCURRENT; 
    else if (dli->Pragma_Prefer_Concurrentize)
      par_dir[i] = PD_PREFER_CONCURRENT; 
    else 
      par_dir[i] = PD_NONE; 
    if (dli->Pragma_Prefer_Concurrentize)
      has_prefer_parallel = TRUE;  
  }
  return has_prefer_parallel; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Inner_Exit_Count
// FUNCTION: Return the number of loops that should be discarded from 
//   the inner SNL because these we cannot interchange or distribute 
//   across a region with exits. 
//-----------------------------------------------------------------------

static INT SNL_Inner_Exit_Count(WN* wn_outer, 
				INT nloops)
{ 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  INT loop_count = 0; 
  INT exit_count = 0; 
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) { 
    if (WN_operator(wn) == OPR_DO_LOOP) { 
      loop_count++; 
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
      if (dli->Has_Exits)
	exit_count = loop_count; 
    } 
    if (wn == wn_outer)
      break; 
  } 
  return exit_count; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Auto_Parallelization 
// FUNCTION: For the SNL with outermost loop 'wn_outer' containing 'nloops',
//   apply scalar expansion, distribution, and permutation to obtain a best
//   parallelizable loop.    
//-----------------------------------------------------------------------

static void SNL_Auto_Parallelization(WN* wn_outer, 
				     INT nloops, 
			             BOOL mark_only)
{
  // Set parallel debug level
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;

  // Nothing to do if already outside or inside parallel loop.
  if (Outermore_Parallel_Construct_Or_Lego_Loop(wn_outer) 
	|| Innermore_Parallel_Or_Lego_Loop(wn_outer))
    return; 

  // Discard loops outside minimal kernel. 
  WN* wn_new_outer = Minimal_Kernel(wn_outer, nloops);
  if (wn_new_outer == NULL) 
    return;
  INT new_nloops =  
    nloops - (Do_Loop_Depth(wn_new_outer) - Do_Loop_Depth(wn_outer));   
  
  // Do not include exited loops in SNL
  new_nloops -= SNL_Inner_Exit_Count(wn_new_outer, new_nloops);
  if (new_nloops == 0) 
    return; 

  // Break giant SNLs into SNLs of size MAX_PARALLEL_NLOOPS
  if (new_nloops > MAX_PARALLEL_NLOOPS) { 
    INT outer_depth = Do_Loop_Depth(wn_new_outer);
    WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_new_outer, new_nloops); 
    DOLOOP_STACK stack(&LNO_local_pool); 
    Build_Doloop_Stack(wn_inner, &stack); 
    INT extra_nloops = new_nloops; 
    for (INT i = 0; i < new_nloops; i += MAX_PARALLEL_NLOOPS) { 
      WN* wn_local_outer = stack.Bottom_nth(outer_depth + i); 
      INT local_nloops = extra_nloops >= MAX_PARALLEL_NLOOPS
        ? MAX_PARALLEL_NLOOPS : extra_nloops; 
      SNL_Auto_Parallelization(wn_local_outer, local_nloops, mark_only); 
      extra_nloops -= MAX_PARALLEL_NLOOPS; 
    }
    return; 
  }

  // Data structures to test each permutation for parallelism.
  INT* permutation = CXX_NEW_ARRAY(INT, new_nloops, &LNO_local_pool); 
  PARALLEL_INFO* pi_best = mark_only 
     ? NULL : CXX_NEW(PARALLEL_INFO(new_nloops), &LNO_local_pool); 
  PARALLEL_INFO* pi_best_pref = mark_only 
     ? NULL : CXX_NEW(PARALLEL_INFO(new_nloops), &LNO_local_pool); 

  // Data structures for privatization analysis. 
  ARA_LOOP_INFO *ara_root =
    CXX_NEW(ARA_LOOP_INFO(wn_new_outer, NULL, TRUE), &ARA_memory_pool);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_new_outer);
  ARA_Initialize_Loops(wn_new_outer, ara_root);
  dli->ARA_Info->Walk_Loop(); 

  // Data structures for expandable and non-expandable scalars  
  SX_INFO sx_info(&LNO_local_pool); 
  sx_info.Make_Sx_Info(wn_new_outer, new_nloops, TRUE); 
  SX_PLIST* plist = &sx_info.Plist; 
  SD_INFO sd_info(&LNO_local_pool); 
  sd_info.Make_Sd_Info(wn_new_outer, new_nloops); 
  SD_PLIST* sd_plist = &sd_info.Plist; 
   
  // Data structures for parallel scalar and array dependences 
  BOOL *sdm_scl = Scl_Dep_Info(wn_new_outer, new_nloops); 
  SNL_DEP_MATRIX** sdm_inv = Inv_Dep_Info(wn_new_outer, new_nloops, TRUE, 
    FALSE); 

  // Cleanup privatization structures. 
  if (sdm_inv[new_nloops - 1] == NULL) { 
    ARA_Cleanup(wn_new_outer); 
    return; 
  } 

  // Test for each position of the parallel loop. 
  double machine_cycles = 0.0; 
  double work_estimate = 0.0; 
  double min_parallel_cycles = 0.0; 
  INT outer_depth = Do_Loop_Depth(wn_new_outer); 
  BOOL is_fully_permutable = Fully_Permutable_Permutation(wn_new_outer, 
    new_nloops); 
  SNL_DEP_MATRIX** sdm_inv_np = Inv_Dep_Info(wn_new_outer, new_nloops, 
    is_fully_permutable, TRUE); 
  PAR_DIR_TYPE* par_directive = CXX_NEW_ARRAY(PAR_DIR_TYPE, new_nloops, 
    &LNO_local_pool);
  BOOL par_pref = Parallel_Directive_Class(wn_new_outer, new_nloops, 
    par_directive); 
  INT i;
  for (i = outer_depth; i < outer_depth + new_nloops; i++) {
    INT ii = i - outer_depth; 
    if (!mark_only) { 
      machine_cycles = SNL_Machine_Cost(wn_new_outer, new_nloops, i, plist,
	&work_estimate, TRUE); 
      if (work_estimate == 0.0) 
        DevWarn("Work Estimate for loop %s at %d is 0", 
          WB_Whirl_Symbol(wn_new_outer), Srcpos_To_Line(WN_linenum(wn_new_outer)));
      min_parallel_cycles = SNL_Min_Parallel_Overhead_Cost(wn_new_outer, 
	new_nloops, i);
      if (!par_pref && machine_cycles + min_parallel_cycles >= pi_best->Cost())
	break; 
    }  
    // Test for each possible loop as parallel in that position. 
    for (INT j = 0; j < new_nloops; j++) { 
      if (par_directive[j] == PD_NO_CONCURRENT)
	continue;
      // Test for loops before and after parallel loop in that position.
      for (INT k = 0; k < Choose(new_nloops - 1, ii); k++) { 
	Permutation_Vector(ii, j, k, new_nloops, permutation); 
	INT sd_split_depth = -1; 
	INTERCHANGE_TYPE int_type = Is_Legal_Permutation_Class(wn_new_outer, 
	  permutation, new_nloops, i, &sx_info, &sd_info, sdm_inv_np, 
          &sd_split_depth, !mark_only && parallel_debug_level >= 2,
	  is_fully_permutable);
	if (int_type == INT_NONE)
	  continue; 
        if (mark_only) { 
	  Mark_Parallelizable_Loop(wn_new_outer, permutation, new_nloops, 
            i, sdm_inv, sdm_scl, &sx_info, &sd_info,sd_split_depth);
	  if (SNL_All_Parallelizable(wn_new_outer, new_nloops)) {
	    ARA_Cleanup(wn_new_outer); 
	    return; 
	  }
        } else { 
	  PARALLEL_INFO* pi = CXX_NEW(PARALLEL_INFO(wn_new_outer, permutation, 
	    new_nloops, i, int_type, sdm_inv, sdm_scl, &sx_info, &sd_info, 
	    sd_split_depth, machine_cycles, work_estimate), &LNO_local_pool); 
	  if (pi->Cost() < pi_best->Cost()) 
	    *pi_best = *pi; 
	  if (par_directive[j] == PD_PREFER_CONCURRENT 
	      && pi->Cost() < pi_best_pref->Cost())
	    *pi_best_pref = *pi; 
	}
      } 
    } 
  } 
  if (pi_best_pref != NULL && pi_best_pref->Parallel_Depth() >= 0) 
    *pi_best = *pi_best_pref; 

  // Mark loops for parallelization. 
  if (!mark_only && pi_best->Parallel_Depth() >= 0) { 
    DOLOOP_STACK dist_stack(&LNO_local_pool); 
    DOLOOP_STACK loop_stack(&LNO_local_pool);
    WN* wn_new_inner = SNL_Get_Inner_Snl_Loop(wn_new_outer, new_nloops); 
    Build_Doloop_Stack(wn_new_inner, &loop_stack); 
    WN* wn_kernel = Parallel_Interchange(wn_new_outer, pi_best->Permutation(), 
      new_nloops, &sd_info, &sx_info, pi_best->Int_Type(), 
      pi_best->Sd_Split_Depth(), pi_best->Split_Depth(), &dist_stack); 
    WN* wn_parallel = Parallel_Loop(pi_best, &loop_stack);
    DO_LOOP_INFO* dli_parallel = Get_Do_Loop_Info(wn_parallel); 
    dli_parallel->Suggested_Parallel = TRUE; 
    dli_parallel->Work_Estimate = pi_best->Work_Estimate(); 
    if (pi_best->Is_Doacross()) {
      if (Check_Doacross_Sync_Coverage(wn_parallel, 
	  pi_best->Sync_Distances())) {
        dli_parallel->Is_Doacross = TRUE;
        dli_parallel->Doacross_Tile_Size = pi_best->Doacross_Tile_Size();
        dli_parallel->Sync_Distances[0]= (pi_best->Sync_Distances())[0];
        dli_parallel->Sync_Distances[1]= (pi_best->Sync_Distances())[1];
        dli_parallel->Doacross_Overhead= pi_best->Doacross_Overhead();
        // INT permutation[2]; permutation[0]=1; permutation[1]=0;
        WN* parent_loop=LWN_Get_Parent(LWN_Get_Parent(wn_parallel));
        //SNL_Permute_Loops(parent_loop,wn_parallel,permutation,2,TRUE,FALSE);
      } else {
        // something is wrong with doacross parallelization, bail out
        dli_parallel->Suggested_Parallel = FALSE; 
	dli_parallel->Work_Estimate = 0;
      }
    }

    ARA_Cleanup(wn_new_outer); 

    for (i = 0; i < dist_stack.Elements(); i++) {
      WN* wn_loop = dist_stack.Bottom_nth(i); 
      SNL_Auto_Parallelization(wn_loop, SNL_Loop_Count(wn_loop), mark_only); 
    } 
    if (LNO_Verbose || parallel_debug_level >= 1)  {
      Print_Parallel_Loop(stdout, pi_best, &loop_stack); 
      Print_Parallel_Loop(TFile, pi_best, &loop_stack); 
    }
    if (LNO_Tlog || Get_Trace(TP_PTRACE1, TP_PTRACE1_PARALLEL)) {
      ap_tlog_info(pi_best, &loop_stack, "");
    } 
  } else {
    ARA_Cleanup(wn_new_outer); 
  } 
}
  
//-----------------------------------------------------------------------
// NAME: Check_Suggested_Parallel 
// FUNCTION: Print a message warning if a loop which we suggested to be 
//   parallelized was not parallelized. 
//-----------------------------------------------------------------------

static void Check_Suggested_Parallel(WN* wn_tree)
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree); 
    if (dli->Suggested_Parallel && !Do_Loop_Is_Mp(wn_tree))
      DevWarn("Did NOT auto-parallelize suggested loop %s at %d", 
        WB_Whirl_Symbol(wn_tree), Srcpos_To_Line(WN_linenum(wn_tree)));
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Check_Suggested_Parallel(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Check_Suggested_Parallel(WN_kid(wn_tree, i));
  }
}

//-----------------------------------------------------------------------
// NAME: Mark_Auto_Parallelizable_Loops 
// FUNCTION: Mark the 'Parallelizable' field for all loops parallelizable 
//   in 'func_nd' in some legal permutation within their SNLs.  
//-----------------------------------------------------------------------

extern void Mark_Auto_Parallelizable_Loops(WN* func_nd)
{
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;

  if (parallel_debug_level >= 1) {
    fprintf(stdout, "### Marking Auto-Parallel-Loops (Begin)\n");
    fprintf(TFile, "### Marking Auto-Parallel Loops (Begin)\n");
  }

  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DU_MANAGER* du = Du_Mgr;

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd, TRUE);

  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    if (ffi->Get_Type(i) == Invalid || ffi->Get_Type(i) == Non_SNL)
      continue;
    WN* wn_outer_loop = ffi->Get_Wn(i); 
    INT nloops = ffi->Get_Depth(i); 
    SNL_Upper_Bound_Standardize(wn_outer_loop, nloops); 
    Hoist_Bounds_One_Level(wn_outer_loop);
    SNL_Auto_Parallelization(wn_outer_loop, nloops, TRUE);
  }

  if (LNO_Verbose || parallel_debug_level >= 1) {
    fprintf(stdout, "### Marking Auto-Parallel-Loops (End)\n");
    fprintf(TFile, "### Marking Auto-Parallel Loops (End)\n");
  }
}
//-----------------------------------------------------------------------
// NAME: Mark_Critical_Section_Loops_Traverse
// FUNCTION: Traverse the tree rooted at 'wn_tree' marking the DO_LOOP_INFO
//   of each do loop which is inside a pair of critical section directives.
//   The tree 'wn_tree' is nested within 'critical_count' critical sections.
//-----------------------------------------------------------------------

static void Mark_Critical_Section_Loops_Traverse(WN* wn_tree,
                                                 INT critical_count)
{
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) {
      if (WN_opcode(wn) == OPC_PRAGMA
          && WN_pragma(wn) == WN_PRAGMA_CRITICAL_SECTION_BEGIN) {
        critical_count++;
      } else if (WN_opcode(wn) == OPC_PRAGMA
          && WN_pragma(wn) == WN_PRAGMA_CRITICAL_SECTION_END) {
        critical_count--;
      } else {
        Mark_Critical_Section_Loops_Traverse(wn, critical_count);
      }
    }
  } else {
    if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
      if (critical_count > 0)
        dli->Inside_Critical_Section = TRUE;
    }
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Mark_Critical_Section_Loops_Traverse(WN_kid(wn_tree, i), critical_count);
  }
}

//-----------------------------------------------------------------------
// NAME: Mark_Critical_Section_Loops
// FUNCTION: Mark the DO_LOOP_INFO of each do loop which is nested inside
//   a pair of critical section directives.
//-----------------------------------------------------------------------

extern void Mark_Critical_Section_Loops(WN* func_nd)
{
  Mark_Critical_Section_Loops_Traverse(func_nd, 0);
}

//-----------------------------------------------------------------------
// NAME: Mark_Threadprivate_Loops_Traverse
// FUNCTION: Set 'Has_Threadprivate' on the DO_LOOP_INFO of an loop which 
//   references a THREAD_PRIVATE variable in the tree rooted at 'wn_tree'.
//-----------------------------------------------------------------------

static void Mark_Threadprivate_Loops_Traverse(WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Mark_Threadprivate_Loops_Traverse(wn);
  } else { 
    if (OPERATOR_has_sym(WN_operator(wn_tree)) && WN_st(wn_tree) != NULL 
	&& (ST_base(WN_st(wn_tree)) != WN_st(wn_tree) 
	&& ST_sclass(ST_base(WN_st(wn_tree))) == SCLASS_COMMON
	&& ST_is_thread_private(ST_base(WN_st(wn_tree)))
#ifdef KEY
	// Bug 6652 - after OMP_Prelower (Rename_Threadprivate_COMMON), 
	// thread private common variables are renamed and the
	// new variables do not carry the thread_private property.
	// Use the special symbol name here to catch that case.
	// Perhaps, the other tests are redundant.
	// FYI, call to Rename_Threadprivate_COMMON in OMP_Prelower
	// was added at Pathscale.
        || strncmp(ST_name(WN_st(wn_tree)), "__ppthd_common_", 15) == 0
#endif
	|| ST_is_thread_private(WN_st(wn_tree)))) {
      for (WN* wn = wn_tree; wn != NULL; wn = LWN_Get_Parent(wn)) {
	if (WN_operator(wn) == OPR_DO_LOOP) {
	  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
	  dli->Has_Threadprivate = TRUE; 
	} 
      }
    } 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Mark_Threadprivate_Loops_Traverse(WN_kid(wn_tree, i));
  } 
} 

//-----------------------------------------------------------------------
// NAME: Mark_Threadprivate_Loops
// FUNCTION: Set 'Has_Threadprivate' on the DO_LOOP_INFO of any loop in 
//   'func_nd' which references a THREAD_PRIVATE variable. 
//-----------------------------------------------------------------------

extern void Mark_Threadprivate_Loops(WN* func_nd) 
{
  if (Run_autopar) 
    Mark_Threadprivate_Loops_Traverse(func_nd);
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Evaluate_Call_Infos_Traverse
// FUNCTION: Evaluate all of the CALL_INFOs in the tree rooted at 'wn_tree'.
//-----------------------------------------------------------------------

extern void IPA_LNO_Evaluate_Call_Infos_Traverse(WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_CALL && Has_Call_Info(wn_tree))
    Get_Call_Info(wn_tree)->Evaluate();

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      IPA_LNO_Evaluate_Call_Infos_Traverse(wn);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      IPA_LNO_Evaluate_Call_Infos_Traverse(WN_kid(wn_tree, i));
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Evaluate_Call_Infos
// FUNCTION: Evaluate all of the CALL_INFOs in the 'func_nd'. 
//-----------------------------------------------------------------------

extern void IPA_LNO_Evaluate_Call_Infos(WN* func_nd)
{
  if (!LNO_IPA_Enabled)
    return; 
  IPA_LNO_Evaluate_Call_Infos_Traverse(func_nd);
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Unevaluate_Call_Infos_Traverse
// FUNCTION: Unevaluate all of the CALL_INFOs in the tree rooted at 'wn_tree'.
//-----------------------------------------------------------------------

extern void IPA_LNO_Unevaluate_Call_Infos_Traverse(WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_CALL && Has_Call_Info(wn_tree))
    Get_Call_Info(wn_tree)->Unevaluate();

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      IPA_LNO_Unevaluate_Call_Infos_Traverse(wn);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      IPA_LNO_Unevaluate_Call_Infos_Traverse(WN_kid(wn_tree, i));
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Unevaluate_Call_Infos
// FUNCTION: Unevaluate all of the CALL_INFOs in the 'func_nd'. 
//-----------------------------------------------------------------------

extern void IPA_LNO_Unevaluate_Call_Infos(WN* func_nd)
{
  if (!LNO_IPA_Enabled)
    return; 
  IPA_LNO_Unevaluate_Call_Infos_Traverse(func_nd);
} 

//-----------------------------------------------------------------------
// NAME: Auto_Parallelization 
// FUNCTION: Perform automatic parallelization on the tree of loops rooted
//   at 'func_nd'. 
//-----------------------------------------------------------------------

extern void Auto_Parallelization(PU_Info* current_pu, 
				 WN* func_nd)
{
#ifdef KEY
  static INT pu_num = -1;
  pu_num ++;
  if (pu_num > LNO_Apo_Skip_After ||
      pu_num < LNO_Apo_Skip_Before ||
      pu_num == LNO_Apo_Skip_Equal)
    return;

  Last_Apo_Loop_Id = 0; // initialize per PU
#endif

  extern BOOL running_cross_loop_analysis;

  if (!(Run_autopar && LNO_Run_AP > 0) 
      || Get_Trace(TP_LNOPT2, TT_LNO_NO_AUTO_PARALLEL)) {
    Annotate_For_Mp_Lowering(current_pu, func_nd);
    if (LNO_Prompl)
      Print_Prompl_Msgs(current_pu, func_nd);
    return;
  } 


#ifdef KEY //Bug 9731: update array access information before apo
           //Bug 9770: move this rebuild to here to skip non-autopar
  LNO_Build_Access(func_nd, &LNO_default_pool);
#endif

  MEM_POOL_Push(&LNO_local_pool); 
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0; 


  if (parallel_debug_level >= 1) {
    fprintf(stdout, "### Auto-parallelization (Begin)\n"); 
    fprintf(TFile, "### Auto-parallelization (Begin)\n"); 
  } 

  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DU_MANAGER* du = Du_Mgr;

  if (LNO_Cross_Loop) {
    Cross_Loop_Cache_Analysis(current_pu, func_nd);
  }

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd, TRUE);

  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    if (ffi->Get_Type(i) == Invalid || ffi->Get_Type(i) == Non_SNL)
      continue; 
    WN* wn_outer_loop = ffi->Get_Wn(i); 
    INT nloops = ffi->Get_Depth(i); 
    SNL_Upper_Bound_Standardize(wn_outer_loop, nloops); 
    Hoist_Bounds_One_Level(wn_outer_loop);
    SNL_Auto_Parallelization(wn_outer_loop, nloops, FALSE);
  }

  Perform_ARA_and_Parallelization(current_pu, func_nd); 
  Check_Suggested_Parallel(func_nd); 

  if (LNO_Verbose || parallel_debug_level >= 1) {
    fprintf(stdout, "### Auto-parallelization (End)\n"); 
    fprintf(TFile, "### Auto-parallelization (End)\n"); 
  } 
  MEM_POOL_Pop(&LNO_local_pool); 
}

//-----------------------------------------------------------------------
// NAME: Doacross_Cost  
// FUNCTION: For the SNL with outermost loop 'wn_outer' conatining 'nloops' 
//   loops, return the cost of parallelizing the 'parallel_depth' loop 
//   at 'parallel_depth' with synchronization
//   (aka real doacross parallelization) after applying the 'permutation'.
//
//   The array dependences for the nest are summarized in 'sdm_inv'.  
//   The scalar dependences are summarized in 'sdm_scl'.  Information about 
//   scalars to be expanded is given by 'sx_info'.  Information about scalars
//   which may be distributed out, but aren't necessarily expanded, is given
//   in 'sd_info'.  The loop must be split at 'sd_split_depth' to remove 
//   unexpandable scalars and obtain the stated 'permutation'.
//   The 'machine_cycles' are the number of machine cycles given the 
//   parallel loop at 'parallel_depth'.  'cache_cycles_per_iter' is as per
//   Parallel_Cost() (note that it should go away--DRK).
//
//   'sync_distances[2]' is returned and contains the synchronization
//   vector information.
//-----------------------------------------------------------------------

static double Doacross_Cost(WN* wn_outer, 
			    INT permutation[], 
			    INT nloops,
			    INT parallel_depth,
			    SNL_DEP_MATRIX** sdm_inv,
                            BOOL sdm_scl[],
                            SX_INFO* sx_info,
                            SD_INFO* sd_info,
			    INT sd_split_depth,
			    double machine_cycles,
			    double *cache_cycles_per_iter,
			    double work_estimate,
			    INT* doacross_tile_size_p,
			    INT sync_distances[],
			    INT* doacross_overhead_p)
{
  *cache_cycles_per_iter = 0.0;

  INT outer_depth=Do_Loop_Depth(wn_outer);

  // test for illegal cases
  if (parallel_depth < outer_depth)
    return (double)DBL_MAX;

  if (parallel_depth >= outer_depth+nloops-1)
    return DBL_MAX;

  // the doacross loop and the immediate inner loop has to be
  // perfectly nested after permutation
  if (!Is_Perfectly_Nested(wn_outer, permutation, nloops, parallel_depth))
    return DBL_MAX;

  MEM_POOL_Push(&LNO_local_pool);

  // the immediate inner loop of the doacross loop after permutation
  // has to be invariant
  DOLOOP_STACK* loop_stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), 
    &LNO_local_pool);
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  Build_Doloop_Stack(wn_inner, loop_stack); 
  INT outer=MIN(outer_depth+permutation[parallel_depth-outer_depth],
                outer_depth+permutation[parallel_depth-outer_depth+1]);
  INT inner=MAX(outer_depth+permutation[parallel_depth-outer_depth],
                outer_depth+permutation[parallel_depth-outer_depth+1]);
  if (!SNL_Is_Invariant(loop_stack, outer, inner)) {
    MEM_POOL_Pop(&LNO_local_pool);
    return DBL_MAX;
  }

  // now, if we ignore the invariant array dependences, is this loop
  // still parallelizable?
  SNL_DEP_MATRIX** sdm_red = Red_Dep_Info(wn_outer, permutation, nloops,
    parallel_depth, TRUE, FALSE);
  INT red_split_depth = Parallelizable_At_Depth(wn_outer, nloops,
    permutation, sdm_red, sdm_scl, sx_info, sd_info, sd_split_depth,
    parallel_depth);

  MEM_POOL_Pop(&LNO_local_pool);

  // give it up if still not parallelizable
  if (red_split_depth == Do_Loop_Depth(wn_outer) + nloops)
    return DBL_MAX;

  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;

  if (parallel_debug_level >= 2)
    Print_Permutation_Vector(stdout,permutation,nloops,parallel_depth,TRUE); 

  // now parallelize assuming doacross (with synch)
  // first we compute the needed synchronization vectors (2 of them)
  BOOL *retained=CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);
  for (INT i=0; i<nloops; i++)
    retained[i]=SNL_Perm_Retained_Section(i,permutation,nloops);
  Compute_Sync_Distances(wn_outer,nloops,permutation,parallel_depth,
			 sdm_inv,retained,sync_distances);

  // based on the synchronization vectors, compute the tile size
  // which determines the granularity of synchronization
  INT doacross_tile_size=
	Get_Doacross_Tile_Size(sync_distances,wn_outer, permutation, nloops,
			       parallel_depth,NOMINAL_PROCS,work_estimate);
  (*doacross_tile_size_p) = doacross_tile_size;

  // in order to estimate the total cost of doacross execution
  // we need the doall execution cost
  double doall_cycle;
  {
    MEM_POOL_Push(&LNO_local_pool);
    PAR_STAT::id_count = 0; 
    PAR_STAT* ps = CXX_NEW(PAR_STAT(wn_outer, nloops, &LNO_local_pool), 
      &LNO_local_pool);
    ps = ps->Parallel_Interchange(wn_outer, permutation, nloops,
      parallel_depth, sd_split_depth, red_split_depth); 
    doall_cycle = ps->Cycle_Count(wn_outer, permutation, nloops, 
      parallel_depth, &sx_info->Plist, red_split_depth, machine_cycles,
      cache_cycles_per_iter, TRUE);
    MEM_POOL_Pop(&LNO_local_pool);
  }

  // compute the initial delay cost if skewing is necessary
  double doacross_delay_cycle=
	Compute_Doacross_Delay_Cycle(wn_outer, permutation, parallel_depth,
				     NOMINAL_PROCS, doacross_tile_size,
				     sync_distances, machine_cycles);
  // compute the synch overhead
  double doacross_sync_cycle=
	Compute_Doacross_Sync_Cycle(wn_outer, permutation, parallel_depth,
				     doacross_tile_size, sync_distances);

  // total cost is the sum of doall cost, delay, and sync overhead

  double cost;
  if (doacross_delay_cycle == DBL_MAX)
    cost = DBL_MAX;
  else
    cost = doall_cycle + doacross_delay_cycle + doacross_sync_cycle;

  (*doacross_overhead_p) = int(doacross_delay_cycle + doacross_sync_cycle);
  if (parallel_debug_level >= 2) {
    printf("  sync vectors =              ");
    if (sync_distances[0]!= NULL_DIST)
      printf("(%d -1) ",sync_distances[0]);
    if (sync_distances[1]!= NULL_DIST)
      printf("(%d 1)",sync_distances[1]);
    printf("\n");
    if (doacross_delay_cycle == DBL_MAX) {
      printf("  delay cycles =             inf\n");
      printf("  sync cycles =              inf\n");
      printf(" *doacross cycles =          inf\n");
    } else {
      printf("  delay cycles =             %13.2f\n", doacross_delay_cycle);
      printf("  sync cycles =              %13.2f\n", doacross_sync_cycle);
      printf(" *doacross cycles =          %13.2f\n", cost);
    }
  }

  return cost;
}

//-----------------------------------------------------------------------
// NAME: Is_Privatizable_With_Context
// FUNCTION:
//	Check privatizability and make sure the sources and sinks
//	of this node are also privatizable.
// ARGUMENTS:
//	loop -- the loop in which 'wn' might be privatizable
//	wn --	the references to be privatized
//	definitely	-- to be passed to the Is_Privatizable() function
//-----------------------------------------------------------------------
extern BOOL Is_Privatizable_With_Context(
			WN* loop,
			WN* wn,
			BOOL definitely)
{
  
  ARA_LOOP_INFO* ara_loop_info=Get_Do_Loop_Info(loop)->ARA_Info;
  if (!ara_loop_info)
    return FALSE;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  if (ara_loop_info->Is_Privatizable(wn,definitely)) {
    VINDEX16 v=dg->Get_Vertex(wn);
    if (v==0)
      return FALSE;
    EINDEX16 e;
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
      if (!Wn_Is_Inside(wn_sink,loop))
	continue;
      if (ara_loop_info->Is_Privatizable(wn_sink,definitely))
	  continue;
      else {
	  return FALSE;
      }
    }
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
      if (!Wn_Is_Inside(wn_source,loop))
	continue;
      if (ara_loop_info->Is_Privatizable(wn_source,definitely))
	  continue;
      else {
	  return FALSE;
      }
    }
    return TRUE;
  } else
    return FALSE;
}


#include "cross_parallel.cxx"
