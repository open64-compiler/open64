/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#define USE_STANDARD_TYPES
#include <vector>
#include "defs.h"
#include "cg.h"
#include "cg_swp.h"
#include "cg_swp_bundle.h"
#include "cg_swp_options.h"
#include "cgprep.h"
#include "errors.h"
#include "tracing.h"
#include "timing.h"
#include "glob.h"    // for Cur_PU_Name
#include "op.h"
#include "bb.h"
#include "cg_swp_target.h"
#include "cg_dep_graph.h"
#include "cg_loop.h"
#include "cg_loop_mii.h"
#include "cgtarget.h"
#include "findloops.h"
#include "ti_res_count.h"
#include "config_asm.h"

static INT loop_index;

// Contains SWP options
//
SWP_OPTIONS SWP_Options;


void SWP_OPTIONS::PU_Configure()
{
  if (Opt_Level == 0) {
    Sched_Direction = 2;
    Heuristics = 2;
  }

  if (!CGTARG_Can_Predicate())
    Enable_While_Loop = FALSE;

  if (!Max_Unroll_Times_Set)
    Max_Unroll_Times = (CG_opt_level > 2) ? 8 : 4;  

  Min_Unroll_Times = std::max(1, Min_Unroll_Times);
  Max_Unroll_Times = std::max(1, Max_Unroll_Times);

  if (Min_Unroll_Times_Set)
    Max_Unroll_Times = Max(Max_Unroll_Times, Min_Unroll_Times);
  
  if (Max_Unroll_Times_Set)
    Min_Unroll_Times = Min(Min_Unroll_Times, Max_Unroll_Times);

  if (!Implicit_Prefetch_Set) {
    // Not all processors implement implicit prefetch -- disable
    // by default on those processors
#if !defined(TARG_MIPS) && !defined(TARG_SL) 
    if (Is_Target_Itanium()) Implicit_Prefetch = FALSE;
#endif
  }
}


void SWP_OP::Print(FILE *fp) const {
  if (op) 
#ifdef TARG_IA64
    fprintf(fp, "[%d] %s scale=%g cycle=%d mod=%d slot=%d trials=%d dir=%s\n",
	    Index(), placed?"placed":"not-placed", scale, cycle, modulo_cycle, 
#else
    fprintf(fp, "[%d](%s) %s scale=%g cycle=%d mod=%d slot=%d trials=%d dir=%s\n",
	    Index(), TOP_Name(OP_code(op)),
	    placed?"placed":"not-placed", scale, cycle, modulo_cycle,
#endif
	    slot, trials,
	    (direction==SWP_TOP_DOWN) ? "top_down" : 
	    ((direction==SWP_BOTTOM_UP) ? "bottom_up" : "unknown"));
  else
    fprintf(fp, "not an SWP op");
}

void SWP_OP_vector::Verify() const {
  for (INT i = 0; i < size(); i++) {
    if (v[i].op)
      FmtAssert(v[i].Index() == i, ("SWP_OP_vector::Verify: v[i].Index() != i"));
  }
}

void SWP_OP_vector::Print(FILE *fp) const {
  for (INT i = 0; i < size(); i++) {
    if (v[i].op) 
      v[i].Print(fp);
  }
  fprintf(TFile, "Invariants: ");
  TN_SET_Print(tn_invariants, fp);
  fprintf(TFile, "\n");
  fprintf(TFile, "Non-rotating: ");
  TN_SET_Print(tn_non_rotating, fp);
  fprintf(TFile, "\n");
}


INT *swp_map_tbl;
INT  swp_map_tbl_max;

SWP_OP_vector::SWP_OP_vector(BB *body, BOOL doloop, MEM_POOL *pool)
{
  OP *op;
  INT max_idx = 0;
  FOR_ALL_BB_OPs(body, op) {
#ifdef TARG_IA64
    max_idx = MAX(max_idx, OP_map_idx(op));
#else
    max_idx = std::max(max_idx, OP_map_idx(op));
#endif
  }
  swp_map_tbl_max = max_idx + 1;
  swp_map_tbl = TYPE_MEM_POOL_ALLOC_N(INT, pool, swp_map_tbl_max);
  INT count = 0;
  FOR_ALL_BB_OPs(body, op) {
    swp_map_tbl[OP_map_idx(op)] = count++;
  }
  const bool trace = Get_Trace(TP_SWPIPE, 2);
  if (trace) {
    for (INT i = 0; i < swp_map_tbl_max; i++)
      fprintf(TFile, "swp_map[%d] = %d\n", i, swp_map_tbl[i]);
  }
  
  INT size = count;
  start = size++;
  stop = size++;
  previous_trials = 0;

#if SWP_USE_STL
  v.insert(v.begin(), size, SWP_OP());
#else
  {
    Is_True(size <= SWP_OPS_LIMIT, 
	    ("SWP_OP_vector: loop has too many (%d) ops.", size));
    v_size = size;
    for (INT i = 0; i < size; i++)
      v[i] = SWP_OP();
  }
#endif

  tn_non_rotating = TN_SET_Create_Empty(Last_TN + 1, pool);
  TN_SET *tn_defs = TN_SET_Create_Empty(Last_TN + 1, pool);
  TN_SET *tn_uses = TN_SET_Create_Empty(Last_TN + 1, pool);
  num_mops = 0;
  num_flops = 0;
  FOR_ALL_BB_OPs(body, op) {
    INT idx = SWP_index(op);
    v[idx].op = op;
    if (OP_memory(op))
      num_mops++;
    if (OP_flop(op))
      num_flops++;
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (TN_is_register(tn) && !TN_is_dedicated(tn))
	tn_defs = TN_SET_Union1D(tn_defs, tn, pool);
      if (TN_is_dedicated(tn))
	tn_non_rotating = TN_SET_Union1D(tn_non_rotating, tn, pool);
    }
    for (INT j = 0; j < OP_opnds(op); j++) {
      TN *tn = OP_opnd(op, j);
      if (TN_is_register(tn) && !TN_is_dedicated(tn))
	tn_uses = TN_SET_Union1D(tn_uses, tn, pool);
      if (TN_is_dedicated(tn))
	tn_non_rotating = TN_SET_Union1D(tn_non_rotating, tn, pool);
    }
    TN *bu_tn = Base_update_tn(op);
    if (bu_tn)
      tn_non_rotating = TN_SET_Union1D(tn_non_rotating, bu_tn, pool);
  }
  // Identify loop invariants!
  tn_invariants = TN_SET_Difference(tn_uses, tn_defs, pool);
  tn_non_rotating = TN_SET_UnionD(tn_non_rotating, tn_invariants, pool);
#ifndef TARG_IA64
  //#ifdef KEY
  FOR_ALL_BB_OPs(body, op) {
    for (INT j = 0; j < OP_opnds(op); j++) {
      TN *tn = OP_opnd(op, j);
      if( TN_is_register(tn) )
	tn_non_rotating = TN_SET_Union1D( tn_non_rotating, tn, pool );
    }
  }
  branch = 0;
  control_predicate_tn = NULL;
#else
  OP *br_op = BB_branch_op(body);
  branch = SWP_index(br_op);
  control_predicate_tn = OP_has_predicate(br_op) ? OP_opnd(br_op, OP_PREDICATE_OPND) : NULL;
#endif
  is_doloop = doloop;
  succeeded = false;
  loop_one_more_time = false;
}

template <class T1>
inline T1 linear_func(T1 x, double alpha, double beta)
{
  return (T1)((double)x * beta + alpha);
}

void SWP_Show_Statistics(const SWP_OP_vector& swp_op_vector, BB *body)
{
  INT nops = 0;
  INT trials = 0;
  for (INT i = 0; i < swp_op_vector.size(); i++) {
    if (swp_op_vector[i].op) {
      nops++;
      trials += swp_op_vector[i].trials;
    }
  }
  const char *banner = "<swps>";
  INT ii = swp_op_vector.ii;
  fprintf(TFile, "%s SWP for PU %s BB %d: %s\n", banner, 
	  Cur_PU_Name ? Cur_PU_Name : "noname", BB_id(body), 
	  (ii == 0) ? "failed" : 
	  ((ii == CG_LOOP_min_ii) ? "optimal" : "non-optimal"));
  fprintf(TFile, "%s  min II: %d\n", banner, swp_op_vector.min_ii);
  fprintf(TFile, "%s  ResMII: %d\n", banner, swp_op_vector.res_min_ii);
  fprintf(TFile, "%s  RecMII: %d\n", banner, swp_op_vector.rec_min_ii);
  fprintf(TFile, "%s  min SL: %d\n", banner, swp_op_vector.min_sl);   
  fprintf(TFile, "%s  found II: %d\n", banner, swp_op_vector.ii);
  fprintf(TFile, "%s  found SL: %d\n", banner, swp_op_vector.sl);
  fprintf(TFile, "%s  found SC: %d\n", banner, swp_op_vector.sc);
  fprintf(TFile, "%s  # ops: %d\n", banner, nops);
  fprintf(TFile, "%s  # trials: %d\n", banner, trials);
  fprintf(TFile, "%s  # total trials: %d\n", banner, trials + swp_op_vector.previous_trials);
  fprintf(TFile, "%s  prep time: %g\n", banner, swp_op_vector.prep_time);
  fprintf(TFile, "%s  sched time: %g\n", banner, swp_op_vector.sched_time);
  fprintf(TFile, "%s  reg alloc time: %g\n", banner, swp_op_vector.reg_alloc_time);
  fprintf(TFile, "%s  code gen time: %g\n", banner, swp_op_vector.code_gen_time);
}


BOOL SWP_Failure(BB *body, SWP_RETURN_CODE code)
{
  // Generate SWP ROTATING KERNEL Annotation
  ROTATING_KERNEL_INFO *info = TYPE_PU_ALLOC(ROTATING_KERNEL_INFO);
  bzero(info, sizeof(ROTATING_KERNEL_INFO));
  ROTATING_KERNEL_INFO_succeeded(info) = FALSE;
  ROTATING_KERNEL_INFO_failure_code(info) = code;
  BB_Add_Annotation(body, ANNOT_ROTATING_KERNEL, (void *)info);
  Reset_BB_rotating_kernel(body);
  return FALSE;
}


// Identify potential hardware/simulator problem
// 
SWP_RETURN_CODE Detect_SWP_Constraints(CG_LOOP &cl, bool trace)
{
  if (SWP_Options.Prep_Only)
    return SWP_PREP_ONLY;

  LOOP_DESCR *loop = cl.Loop();
  BB *body = LOOP_DESCR_loophead(loop);
  OP *op;
  INT op_count = 0;
  INT total_op_count = 0;
  FOR_ALL_BB_OPs(body, op) {
    if (!OP_br(op) && !OP_dummy(op))
      op_count++;
    total_op_count++;

    if (OP_code(op) == TOP_asm) {
      if (trace) 
	fprintf(TFile, "SWP: skip optimization due to TOP_asm.\n");
      return SWP_ASM;
    }

    if (SWP_Options.Enable_Workaround) {
#ifdef TARG_IA64
      if (OP_code(op) == TOP_setf_sig) {
	DevWarn("SWP: skip optimization due to simulation of frcpa");
	if (trace) 
	  fprintf(TFile, "SWP: skip optimization due to simulation of frcpa.\n");
	return SWP_WORKAROUND;
      }
#endif
    }
    
    TN *found_ded_tn = NULL;
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (TN_is_dedicated(tn) &&
	  !TN_is_const_reg(tn) &&
	  REGISTER_Is_Rotating(TN_register_class(tn), TN_register(tn))) {
	found_ded_tn = tn;
	break;
      }
    }
    for (INT j = 0; j < OP_opnds(op); j++) {
      TN *tn = OP_opnd(op, j);
      if (TN_is_dedicated(tn) && 
	  !TN_is_const_reg(tn) && 
	  REGISTER_Is_Rotating(TN_register_class(tn), TN_register(tn))) {
	found_ded_tn = tn;
	break;
      }
    }
    if (found_ded_tn) {
      if (trace) 
	fprintf(TFile, "SWP: skip optimization due to rotating dedicated TN%d.\n",
		TN_number(found_ded_tn));
      return SWP_DEDICATED_ROT_REG;
    }
  }

  if (op_count == 0) 
    return SWP_LOOP_EMPTY;     // don't bother to swp empty loops

  if (total_op_count + SWP_OPS_OVERHEAD > SWP_Options.OPS_Limit)
    return SWP_LOOP_LIMIT;

#ifdef TARG_IA64
  // Disable SWP loops with low feedback trip count
  if (CG_PU_Has_Feedback)
  {
    BBLIST *bb_succs = BB_succs(body);
    if (BBlist_Len(bb_succs) > 1 && BB_freq_fb_based(body)) {
      BBLIST *succ;
      FOR_ALL_BBLIST_ITEMS(bb_succs,succ) {
	if (BB_id(body) == BB_id(BBLIST_item(succ))) {
	  if ((BBLIST_prob(succ) >= 0 && BBLIST_prob(succ) <= 1.0) &&	//test validation first
	      (BBLIST_prob(succ) <= SWP_Options.FB_Prob1/100.0 || 
               BBLIST_prob(succ) <= SWP_Options.FB_Prob2/100.0 && 
	       BB_freq(body) < SWP_Options.FB_Freq && BB_freq(body) > 0)) {
            if (trace) {
              fprintf(TFile, "SWP: skip optimization due to feedback low trip count.\n");
            }
            return SWP_LOW_TRIP_COUNT;
          }
        }
      }
    }    
  }
#endif
    
  return SWP_OK;
}


// prune loop-carried REGOUT dependences on rotating registers
static void
Prune_Regout_Deps(BB *body, TN_SET *non_rotating)
{
  vector<ARC*> arcs_to_delete;
  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    if (_CG_DEP_op_info(op)) {
      for (ARC_LIST *arcs = OP_succs(op); arcs; arcs = ARC_LIST_rest(arcs)) {
        ARC *arc = ARC_LIST_first(arcs);
        // look for loop-carried REGOUT dependences
        if (ARC_kind(arc) == CG_DEP_REGOUT && ARC_omega(arc) > 0) {
          // check that none of the OP results is in the non-rotating set
          bool redundant = true;
          for (INT i = 0; i < OP_results(op); i++) {
            TN *tn = OP_result(op,i);
            if (!TN_is_register(tn) ||
                TN_is_dedicated(tn) ||
                TN_SET_MemberP(non_rotating, tn)) {
              redundant = false;
              break;
            }
          }
          if (redundant) {
            arcs_to_delete.push_back(arc);
          }
        }
      }
    }
  }
  for (size_t i = 0; i < arcs_to_delete.size(); i++) {
    CG_DEP_Detach_Arc(arcs_to_delete[i]);
  }
}


static BOOL Is_Loop_Skipped(void)
/* -----------------------------------------------------------------------
 * Return a boolean that indicates if we should skip pipelining
 * the specified loop.
 * -----------------------------------------------------------------------
 */
{
  const BOOL skip_it = (   loop_index < CG_local_skip_before
			|| loop_index > CG_local_skip_after
			|| loop_index == CG_local_skip_equal);

  if (CG_skip_local_swp) {
    DevWarn("%s swp for loop: index=%d",
	    skip_it ? "Skipping" : "Attempting",
	    loop_index);
  }

  ++loop_index;

  return skip_it;
}


BOOL Perform_SWP(CG_LOOP& cl, SWP_FIXUP_VECTOR& fixup, bool is_doloop)
{
  Set_Error_Phase("Software Pipelining");
  LOOP_DESCR *loop = cl.Loop();
  BB *body = LOOP_DESCR_loophead(loop);
  BB *head = CG_LOOP_prolog;
  BB *tail = CG_LOOP_epilog;
  Is_True(BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1,("can't SWP multi-bb loops."));

  const bool trace = Get_Trace(TP_SWPIPE, 2);
  const bool trace_details = Get_Trace(TP_SWPIPE, 4);
  const bool trace_bundling = Get_Trace(TP_SWPIPE, 0x1000);
  const bool show_result = Get_Trace(TP_SWPIPE, 1);

  if (CG_skip_local_swp && Is_Loop_Skipped()) return FALSE;

  SWP_RETURN_CODE code = Detect_SWP_Constraints(cl, trace);
  if (code != SWP_OK)
    return SWP_Failure(body, code);

  if (trace)
    CG_LOOP_Trace_Loop(loop, "**** Before SWP ****");

  // SWP compile-time tuning parameters
  double max_ii_alpha = SWP_Options.Max_II_Alpha;
  double max_ii_beta  =  SWP_Options.Max_II_Beta;
  double ii_incr_alpha =  SWP_Options.II_Incr_Alpha;
#ifdef TARG_IA64
  double ii_incr_beta =  1.0 + (SWP_Options.II_Incr_Beta - 1.0) / MAX(1,SWP_Options.Opt_Level);
  INT sched_budget = SWP_Options.Budget * MAX(1,SWP_Options.Opt_Level);
#else
  double ii_incr_beta =  1.0 + (SWP_Options.II_Incr_Beta - 1.0) /
    std::max(1,SWP_Options.Opt_Level);
  INT sched_budget = SWP_Options.Budget * std::max(1,SWP_Options.Opt_Level);
#endif

  {
    Start_Timer(T_SWpipe_CU);
    double time0 = Get_User_Time(T_SWpipe_CU);

    CXX_MEM_POOL swp_local_pool("swp pool", FALSE);
    SWP_OP_vector swp_op_vector(body, is_doloop, swp_local_pool());

    // Make sure we have enough non-rotating registers for the loop
    SWP_REG_ASSIGNMENT swp_assign;

#ifdef TARG_IA64
    if (!swp_assign.Enough_Non_Rotating_Registers(swp_op_vector.tn_non_rotating)) {
      // TODO: we might be able to convert some invariants into copy
      // and thus uses the rotating registers.
      return SWP_Failure(body, NON_ROT_REG_ALLOC_FAILED );
    }
#endif
    
    CG_LOOP_rec_min_ii = CG_LOOP_res_min_ii = CG_LOOP_min_ii = 0;

    // invokes CG_DEP_Compute_Graph, deconstructor deletes graph
    CYCLIC_DEP_GRAPH cyclic_graph( body, swp_local_pool()); 

    if (trace)
      CG_DEP_Trace_Graph(body);

#ifdef TARG_IA64
    // prune loop-carried REGOUT dependences on rotating registers
    Prune_Regout_Deps(body, swp_op_vector.tn_non_rotating);

    if (trace)
      CG_DEP_Trace_Graph(body);
#endif

    {
      // Compute CG_LOOP_min_ii.
      MEM_POOL_Push(&MEM_local_pool);
      BOOL ignore_non_def_mem_deps = FALSE;
      CG_LOOP_Make_Strongly_Connected_Components(body, &MEM_local_pool, ignore_non_def_mem_deps); 
      CG_LOOP_Calculate_Min_Resource_II(body, NULL, FALSE /*include pref*/, TRUE /*ignore pref stride*/);
      CG_LOOP_Calculate_Min_Recurrence_II(body, ignore_non_def_mem_deps);

      CG_LOOP_Clear_SCCs(loop);
      MEM_POOL_Pop(&MEM_local_pool);
    }

    double time1 = Get_User_Time(T_SWpipe_CU);

    // Modulo Scheduling
    CG_LOOP_min_ii = std::max(CG_LOOP_min_ii, SWP_Options.Starting_II);
    INT max_ii = (INT)linear_func(CG_LOOP_min_ii, max_ii_alpha, max_ii_beta);

    // update CG_LOOP_min_ii using MinDist
    MinDist mindist(swp_op_vector, swp_op_vector.start, swp_op_vector.stop,
		    swp_op_vector.branch, CG_LOOP_min_ii);

    if (mindist.Found_ii() != CG_LOOP_min_ii) {
      DevWarn("CG_LOOP_min_ii (%d) is different from RecMII (%d) identified by MinDist.",
	      CG_LOOP_min_ii, mindist.Found_ii());
      CG_LOOP_min_ii = mindist.Found_ii();
    }
    
    Modulo_Schedule(swp_op_vector, CG_LOOP_min_ii, max_ii, ii_incr_alpha, ii_incr_beta,
		    sched_budget, trace, trace_details);

    if (!swp_op_vector.succeeded)
      return SWP_Failure(body, MOD_SCHED_FAILED );

    // Bundling
    if (SWP_Options.Enable_Bundling)
      SWP_Bundle(swp_op_vector, trace_bundling);
    else
      SWP_Dont_Bundle(swp_op_vector);

    if (trace)
      swp_op_vector.Print(TFile);

    double time2 = Get_User_Time(T_SWpipe_CU);

#ifdef TARG_IA64
    // Perform Register Allocation to rotating register banks.  The
    // resultant allocation will be in terms of a map from TNs to 
    // positive unbounded locations (swp_assign.reg_allocation), a 
    // lower bound on the number of rotating registers required for
    // the allocation (swp_assign.rotating_reg_used), and a location
    // assigned to swp_assign.control_predicate_loc.
    //
    if (!swp_assign.Allocate_Loop_Variants(swp_op_vector, head, tail)) {
      // failed to allocate loop variants to rotating register banks
      SWP_Undo_Bundle(swp_op_vector, body);
      return SWP_Failure(body, REG_ALLOC_FAILED );
    }
    
    // Reserve rotating registers to cover the ones needed for this loop.
    // Only integer registers can vary the size of the rotating segment,
    // so there is no need to do it for other register classes.
    //
    REGISTER_Reserve_Rotating_Registers(ISA_REGISTER_CLASS_integer, 
		     swp_assign.rotating_reg_used[ISA_REGISTER_CLASS_integer]);
#endif

    double time3 = Get_User_Time(T_SWpipe_CU);

    // Code Generation
    LOOPINFO *info = LOOP_DESCR_loopinfo(loop);
    TN *trip_count_tn = info ? LOOPINFO_trip_count_tn(info) : NULL;
    SWP_Emit(swp_op_vector, swp_assign, trip_count_tn, 
	     head, body, tail,
	     is_doloop, trace);

    fixup.push_back( SWP_FIXUP(head, body, tail, 
			       swp_assign.control_predicate_loc) );

    double time4 = Get_User_Time(T_SWpipe_CU);

    if (trace)
      CG_LOOP_Trace_Loop(loop, "**** After SWP ****");

    // Generate Statistics
    swp_op_vector.prep_time = time1 - time0;
    swp_op_vector.sched_time = time2 - time1;
    swp_op_vector.reg_alloc_time = time3 - time2;
    swp_op_vector.code_gen_time = time4 - time3;
    if (show_result) 
      SWP_Show_Statistics(swp_op_vector, body);

    Stop_Timer(T_SWpipe_CU);
  }

  return TRUE;
}


/* ====================================================================
 *
 * Emit_SWP_Note
 *
 * Emit a loop note to the .s file, ...
 *
 * ====================================================================
 */
void
Emit_SWP_Note(BB *bb, FILE *file)
{
  ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_ROTATING_KERNEL);
  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(ant);
  char prefix[20];

  if (ROTATING_KERNEL_INFO_succeeded(info)) {
    sprintf( prefix, "%s<swps> ", ASM_CMNT_LINE );
    fprintf(file, "%s\n", prefix);
    fprintf(file, "%s%3d cycles per 1 iteration in steady state\n",
	    prefix, ROTATING_KERNEL_INFO_ii(info));
    fprintf(file, "%s%3d pipeline stages\n", 
	    prefix, ROTATING_KERNEL_INFO_stage_count(info));
    fprintf(file, "%s\n", prefix);

    sprintf( prefix, "%s<swps>      ", ASM_CMNT_LINE );

    fprintf(file, "%smin %d cycles required by resources\n",
	    prefix, ROTATING_KERNEL_INFO_res_min_ii(info));
    fprintf(file, "%smin %d cycles required by recurrences\n",
	    prefix, ROTATING_KERNEL_INFO_rec_min_ii(info));
    fprintf(file, "%smin %d cycles required by resources/recurrence\n", 
	    prefix, ROTATING_KERNEL_INFO_min_ii(info)); 
    fprintf(file, "%smin %d cycles (actual %d cycles) required to schedule one iteration\n",
	    prefix, ROTATING_KERNEL_INFO_min_sched_len(info), ROTATING_KERNEL_INFO_sched_len(info));
    fprintf(file, "%s\n", prefix);
    TI_RES_COUNT_Emit_Note(prefix, file, ROTATING_KERNEL_INFO_res_counts(info), 
			   ROTATING_KERNEL_INFO_ii(info));
    fprintf(file, "%s\n", prefix);
  } else {
    sprintf( prefix, "%s<swpf> ", ASM_CMNT_LINE );
    fprintf(file, "%s\n", prefix);
    const char *failure_msg;
    switch (ROTATING_KERNEL_INFO_failure_code(info)) {
    case SWP_PREP_ONLY:
      failure_msg = "disable by -SWP:prep_only";
      break;
    case SWP_ASM:
      failure_msg = "unable to swp a loop containing ASM statements";
      break;
    case SWP_WORKAROUND:
      failure_msg = "disable swp to workaround hardware bugs";
      break;
    case SWP_DEDICATED_ROT_REG:
      failure_msg = "unable to swp a loop with dedicated rotating register binding";
      break;
    case MOD_SCHED_FAILED:
      failure_msg = "unable to find a modulo schedule";
      break;
    case REG_ALLOC_FAILED:
      failure_msg = "not enough rotating register";
      break;
    case NON_ROT_REG_ALLOC_FAILED:
      failure_msg = "not enough non-rotating register";
      break;
    case SWP_LOOP_EMPTY:
      failure_msg = "loop is empty";
      break;
    case SWP_LOOP_LIMIT:
      failure_msg = "loop is too big";
      break;
#ifdef TARG_IA64
    case SWP_LOW_TRIP_COUNT:
      failure_msg = "loop has low trip count";
      break;
#endif
    default:
      Is_True(FALSE, ("unknown SWP RETURN CODE."));
    }
    fprintf(file, "%s %s\n", prefix, failure_msg);
    fprintf(file, "%s\n", prefix);
  }
}
