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


// this file is -*- C++ -*-  Really!


static double Parallel_Cost(WN* wn_outer, 
		     INT permutation[], 
		     INT nloops,
		     INT parallel_depth,
		     INT sd_split_depth, 
		     INT split_depth, 
		     SX_PLIST* plist,
		     double machine_cycles,
		     double *cache_cycles_per_iter,
		     double *loop_cycles,
		     double *reduction_cycles,
		     double *parallel_cycles,
		     double *cache_cycles)
{
  *cache_cycles_per_iter = 0.0;
  *loop_cycles = 0.0;
  *reduction_cycles = 0.0;
  *parallel_cycles = 0.0;
  *cache_cycles = 0.0;
    
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
  
  *loop_cycles = ps->Loop_Overhead_Cost();
  *parallel_cycles = ps->Parallel_Overhead_Cost();
  *reduction_cycles = ps->Reduction_Cost();
  *cache_cycles = ps->Num_Estimated_Iters() * (*cache_cycles_per_iter) / NOMINAL_PROCS;

  if (parallel_debug_level >= 3) {
    ps->Sanity_Check(stdout); 
    fprintf(stdout, "After:\n"); 
    ps->Print(stdout); 
  } 

  return cost; 
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
			    INT* doacross_overhead_p,
			    double *loop_cycles,
			    double *reduction_cycles,
			    double *parallel_cycles,
			    double *cache_cycles)
{
  *cache_cycles_per_iter = 0.0;
  *loop_cycles = 0.0;
  *reduction_cycles = 0.0;
  *parallel_cycles = 0.0;
  *cache_cycles = 0.0;

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
    *loop_cycles = ps->Loop_Overhead_Cost();
    *parallel_cycles = ps->Parallel_Overhead_Cost();
    *reduction_cycles = ps->Reduction_Cost();
    *cache_cycles = ps->Num_Estimated_Iters() * (*cache_cycles_per_iter) / NOMINAL_PROCS;
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
			     double work_estimate,
			     BOOL dummy)
{
  _wn_outer = wn_outer;
  _nloops = nloops; 
  INT i;
  for (i = 0; i < nloops; i++) 
    _permutation[i] = permutation[i];  
  _int_type = int_type; 
  _is_doacross = FALSE;
  _doacross_overhead = 0;
  _preferred_concurrent = FALSE;
  for (i = 0; i < 2; i++)
    _sync_distances[i] = NULL_DIST;
  _sd_split_depth = sd_split_depth; 
  _split_depth = Parallelizable(wn_outer, permutation, nloops, 
    parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info, _sd_split_depth); 
  BOOL is_doall = (_split_depth != Do_Loop_Depth(wn_outer) + nloops);
  double doall_cost=DBL_MAX;
  double doacross_cost=DBL_MAX;
  
  // LNO_Run_Doacross==0	do not run doacross
  // LNO_Run_Doacross==1	run doacross after doall failed
  // LNO_Run_Doacross==2	run doall and doacross and pick the best one
  // LNO_Run_Doacross==3	run doall after doacross failed
  // LNO_Run_Doacross==4	run doacross only

  _machine_cycles = machine_cycles;

  switch (LNO_Run_Doacross) {
    case 0: if (is_doall) {  
              doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
		parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
		machine_cycles, &_cache_cycles_per_iter, &_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
	    }
	    break;
    case 1: if (is_doall) {  
              doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
		parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
		machine_cycles, &_cache_cycles_per_iter, &_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
	    } else if (LNO_Pseudo_Lower) {
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
		parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
		sd_split_depth, machine_cycles, &_cache_cycles_per_iter,
		work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead, &_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
	    }
	    break;
    case 2: if (is_doall)
	      doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
	        parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
	        machine_cycles, &_cache_cycles_per_iter,&_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
	    if (LNO_Pseudo_Lower)
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
	        parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
	        sd_split_depth, machine_cycles, &_cache_cycles_per_iter,
	        work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead, &_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
	    break;
    case 3: if (LNO_Pseudo_Lower)
            {
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
	        parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
	        sd_split_depth, machine_cycles, &_cache_cycles_per_iter,
	        work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead, &_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
	      BOOL is_doacross= (doacross_cost != DBL_MAX);
	      if (!is_doacross && is_doall) {
                doall_cost = Parallel_Cost(wn_outer, permutation, nloops,
		  parallel_depth, _sd_split_depth, _split_depth, &sx_info->Plist,
		  machine_cycles, &_cache_cycles_per_iter, &_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
	      }
            }
	    break;
    case 4: if (LNO_Pseudo_Lower)
	      doacross_cost=Doacross_Cost(wn_outer, permutation, nloops,
	        parallel_depth, sdm_inv, sdm_scl, sx_info, sd_info,
	        sd_split_depth, machine_cycles, &_cache_cycles_per_iter,
	        work_estimate, &_doacross_tile_size, _sync_distances,
		&_doacross_overhead, &_loop_cycles,
		&_reduction_cycles, &_parallel_cycles, &_cache_cycles);
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
						 _cache_cycles_per_iter);
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
						 _cache_cycles_per_iter);
    _cost = doacross_cost;
    _is_doacross = TRUE;
    _split_depth = -1; 
  }
}


//-----------------------------------------------------------------------
// NAME: SNL_Parallelization_Costs 
// FUNCTION: For the SNL with outermost loop 'wn_outer' containing 'nloops',
//   apply scalar expansion, distribution, and permutation to obtain a set
//   of parallelization options
//-----------------------------------------------------------------------

void SNL_Parallelization_Costs(WN* wn_outer, INT nloops, PARALLEL_INFO_ST  *pist,
			       double *min_seq_cache_cost, double *min_seq_machine_cost)
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
      SNL_Parallelization_Costs(wn_local_outer, local_nloops, pist, 
			       min_seq_cache_cost, min_seq_machine_cost); 
      extra_nloops -= MAX_PARALLEL_NLOOPS; 
    }
    return; 
  }

  // Data structures to test each permutation for parallelism.
  INT* permutation = CXX_NEW_ARRAY(INT, new_nloops, &LNO_local_pool); 

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

  // also keep track of the sequential cost
  double min_seq_cost = DBL_MAX;
  *min_seq_machine_cost = DBL_MAX;
  *min_seq_cache_cost = DBL_MAX;

  INT outer_depth = Do_Loop_Depth(wn_new_outer); 
  BOOL is_fully_permutable = Fully_Permutable_Permutation(wn_new_outer, 
							  new_nloops); 
  SNL_DEP_MATRIX** sdm_inv_np = Inv_Dep_Info(wn_new_outer, new_nloops, 
					     is_fully_permutable, TRUE); 
  PAR_DIR_TYPE* par_directive = CXX_NEW_ARRAY(PAR_DIR_TYPE, new_nloops, 
					      &LNO_local_pool);
  BOOL par_pref = Parallel_Directive_Class(wn_new_outer, new_nloops, 
					   par_directive); 
  for (INT i = outer_depth; i < outer_depth + new_nloops; i++) {
    INT ii = i - outer_depth; 
    machine_cycles = SNL_Machine_Cost(wn_new_outer, new_nloops, i, plist,
				      &work_estimate, TRUE); 
    if (work_estimate == 0.0) 
      DevWarn("Work Estimate for loop %s at %d is 0", 
	      WB_Whirl_Symbol(wn_new_outer), Srcpos_To_Line(WN_linenum(wn_new_outer)));
    min_parallel_cycles = SNL_Min_Parallel_Overhead_Cost(wn_new_outer, 
							 new_nloops, i);

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
          &sd_split_depth, FALSE,
	  is_fully_permutable);
	if (int_type == INT_NONE)
	  continue; 
      
	PARALLEL_INFO* pi = CXX_NEW(PARALLEL_INFO(wn_new_outer, permutation, 
	    new_nloops, i, int_type, sdm_inv, sdm_scl, &sx_info, &sd_info, 
	    sd_split_depth, machine_cycles, work_estimate, TRUE), &LNO_local_pool); 

	if (pi->Parallel_Depth() >= 0) {
	  // update the minimal sequential execution cost

	  double seq_cost = machine_cycles * NOMINAL_PROCS  + pi->Cache_Cost() * NOMINAL_PROCS;
	  if (seq_cost < min_seq_cost) {
	    min_seq_cost = seq_cost;
	    *min_seq_cache_cost   =  pi->Cache_Cost() * NOMINAL_PROCS;
	    *min_seq_machine_cost =  machine_cycles * NOMINAL_PROCS;
	  }

	  // store the choice
	  pist->Push(pi);

	  if (par_directive[j] == PD_PREFER_CONCURRENT) {
	    pi->Set_Preferred();
	  }
	} else {
	  CXX_DELETE(pi,&LNO_local_pool);
	} 
      } 
    } 
  } 

  if (*min_seq_cache_cost  == DBL_MAX) {
    *min_seq_cache_cost = 0.0;
  }

  if  (*min_seq_machine_cost  == DBL_MAX) {
    *min_seq_machine_cost = 0.0;
  }

  ARA_Cleanup(wn_new_outer); 
}

void PARALLEL_INFO::Print(FILE *file)
{
  Print_Permutation_Vector(file, _permutation, _nloops, _parallel_depth, _is_doacross);
  fprintf(file, "parallel_depth : %d\n", _parallel_depth);
  fprintf(file, "cost : %lf\n", _cost);
  fprintf(file, "rc = %lf lc = %lf pc = %lf mc = %lf cci = %lf cc= %lf\n",
	  _reduction_cycles, _loop_cycles, _parallel_cycles, 
	  _machine_cycles, _cache_cycles_per_iter, _cache_cycles);

  if (_is_doacross) {
    fprintf(file, "doacross overhead = %lf\n", (double) _doacross_overhead);
  }
}
