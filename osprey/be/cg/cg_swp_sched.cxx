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


/*

  Modulo Scheduling

  Implemented by Raymond Lo, Apr 1999.

  The modulo scheduler is based on the paper.  Several modifications
  are made to reduce the searching time for some corner cases.

  Richard Huff,
  "Lifetime-Sensitive Modulo Scheduling",
  in Programming Language Design and Implementation. SIGPLAN, 1993. 
  http://www.cs.cornell.edu/Info/Projects/Bernoulli/home.html#PLDI93-2

*/

#define USE_STANDARD_TYPES
#include "defs.h"
#include <vector>
#include <utility>
#include <iterator>
#include <math.h>
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "mempool.h"
#include "errors.h"
#include "tracing.h"
#include "op.h"
#include "cg_dep_graph.h"
#include "ti_res_res.h"
#include "cg_loop_mii.h"
#ifdef TARG_IA64
#include "cache_analysis.h"
#endif
#if defined(_LP64) && defined(_SGI_COMPILER_VERSION)
/* workaround for a bug in g++ */
#define min(a,b)  ((a < b) ? a : b)
#endif

void MinDist::Print(FILE *fp) const
{
  const int n_col = 16;
  fprintf(fp, "MinDist %dx%d:\n", size(), size());
  fprintf(fp, "     ");
  for (INT j = 0; j < size(); j++) {
    if (j != 0 && j % n_col == 0)
      fprintf(fp, "\n     ");
    fprintf(fp,"%4d", j);
  }
  fprintf(fp, "\n");
  for (INT i = 0; i < size(); i++) {
    fprintf(fp, "%3d: ", i);
    for (INT j = 0; j < size(); j++) {
      if (j != 0 && j % n_col == 0)
	fprintf(fp, "\n     ");
      fprintf(fp,"%4d", mindist[i][j]);
    }
    fprintf(fp, "\n");
  }
}

MinDist::MinDist(const SWP_OP_vector& v, INT start, INT stop, INT branch, INT ii)
{
  int n_ops = v.size();
  mindist_size = n_ops;
  
  INT step = ii;
  INT ii_lower = ii;
  INT ii_try;

  while (1) {
    // ii >= ii_lower
    ii_try = ii_lower;
    while (Compute(v, start, stop, branch, ii_try) > 0) {
      ii_lower = ii_try + 1;
      ii_try += step;
    }
    // Compute(v, start, stop, branch, ii_try) == 0
    // ii >= ii_Lower && ii <= ii_try

    if (ii_lower == ii_try) {
      found_ii = ii_lower;
      return;
    }
    step = MAX(1, step / 2);
  }
}


INT MinDist::Compute(const SWP_OP_vector& v, INT start, INT stop, INT branch, INT ii)
{
  int n_ops = v.size();
  INT i, j, k;
  
  // initialization 
  //
  for (i = 0; i < n_ops; i++) 
    for (j = 0; j < n_ops; j++)
      mindist[i][j] = NEG_INF;

  for (i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op) {
      for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc    = ARC_LIST_first(al);
	// Skip all PREBR dependence because they will be taken care by
	// the SWP stage control predicates
	if (ARC_kind(arc) == CG_DEP_PREBR) 
	  continue;
	OP  *succ = ARC_succ(arc);
	mindist[i][SWP_index(succ)] =
	  MAX(mindist[i][SWP_index(succ)], ARC_latency(arc) - ARC_omega(arc) * ii);
	Is_True(succ == v[SWP_index(succ)].op, ("MinDIST: wrong SWP_index."));
      }
      mindist[start][i] = 0;
      mindist[i][stop] = 0;
#ifdef TARG_IA64
      mindist[i][branch] = MAX(mindist[i][branch], 0);
#endif
    }
  }

  // Floyd's all pairs shortest paths algorithm.
  // It is based on dynamic programming.
  for (k = 0; k < n_ops; k++) 
    for (i = 0; i < n_ops; i++) 
      for (j = 0; j < n_ops; j++) {
#if SWP_DEBUG
	// clearer if mindist values stay at neg_inf
	if (mindist[i][k] != NEG_INF && mindist[k][j] != NEG_INF)
#endif
	  mindist[i][j] = MAX(mindist[i][j], mindist[i][k] + mindist[k][j]);
      }

  for (i = 0; i < n_ops; i++) {
    if (mindist[i][i] > 0) 
      return mindist[i][i];
    else
      mindist[i][i] = 0;
  }
  return 0;
}


//************************************************************************
//   MinLT calculation
//     Assume the first result is the *important* result for all operation
//************************************************************************

class MinLT {
  std::vector<INT>  minlt;
public:
  INT size() const { return minlt.size(); }
  INT operator()(INT i) const { return minlt[i]; }
  void Print(FILE *fp) const;
  MinLT(const SWP_OP_vector& v, INT ii, const MinDist& mindist);
};

void MinLT::Print(FILE *fp) const
{
  fprintf(fp, "MinLT: ");
  for (INT i = 0; i < minlt.size(); i++) {
    if (minlt[i] >= 0) 
      fprintf(fp, "(%d,%d) ", i, minlt[i]);
  }
  fprintf(fp, "\n");
}

MinLT::MinLT(const SWP_OP_vector& v, INT ii, const MinDist& mindist)
  :minlt(v.size(), 0)
{
  for (INT i = 0; i < v.size(); i++) {
    minlt[i] = -1;
    OP *op = v[i].op;
    if (op) {
      minlt[i] = 1;
      for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	if (ARC_kind(arc) == CG_DEP_REGIN) {
	  OP  *succ = ARC_succ(arc);
	  INT succ_idx = SWP_index(succ);
	  if (OP_opnd(succ,ARC_opnd(arc)) == OP_result(op,0)) {
	    INT live_range = ARC_omega(arc) * ii + mindist(i,succ_idx);
	    minlt[i] = MAX(minlt[i], live_range);
	  }
	}
      }
    }
  }
}


//************************************************************************
//   MinAvg calculation
//************************************************************************

INT MinAvg(INT ii, const MinLT& minlt) 
{
  INT minavg = 0;
  for (INT i = 0; i < minlt.size(); i++) {
    if (minlt(i) > 0) 
      minavg += minlt(i);
  }
  return (INT) ceil(minavg/ii);
}


//************************************************************************
//   Slack calculation
//************************************************************************

class Slack {
  std::vector<INT> estart;
  std::vector<INT> lstart;
  INT start;    // the START node index
  INT stop;     // the STOP node index
  bool trace;

public:
  INT Start()           const { return start; }
  INT Stop()            const { return stop; }
  INT Estart(INT i) const     { return estart[i]; }
  INT Lstart(INT i) const     { return lstart[i]; }
  INT operator()(INT i) const { return lstart[i] - estart[i]; }
  void Verify() const;
  void Print(FILE *fp) const;
  void Print(FILE *fp, const SWP_OP_vector& v) const;
  void Set_last_cycle(const SWP_OP_vector& v, INT last_cycle, const MinDist& mindist);
  void Relax_Precedence(const SWP_OP_vector& v, const std::vector<INT>& unplaced, const std::vector<INT>& need_relax, const MinDist& mindist);
  void Update_Slack_From_Placed_Ops(const SWP_OP_vector& v, const MinDist& mindist);
  void Update_Slack_From_Placed_Op(INT candidate, 
				   const SWP_OP_vector& v, const MinDist& mindist);
  Slack(const SWP_OP_vector& v, INT start_idx, INT stop_index, INT ii, const MinDist& m, bool trace);
};

// Verify that forall i 
//   0 <= estart[start] <= estart[i] <= lstart[i] <= lstart[stop]
void Slack::Verify() const 
{
  INT min_cycle = estart[start];
  INT max_cycle = lstart[stop];
  FmtAssert(min_cycle >= 0, ("Slack: min_cycle (%d) < 0", min_cycle));
  for (INT i = 0; i < estart.size(); i++) {
    FmtAssert(estart[i] <= lstart[i],
	      ("Slack: estart (%d) > lstart (%d) for OP %d\n",estart[i], lstart[i], i));
    FmtAssert(min_cycle <= estart[i], 
	      ("Slack: min_cycle (%d) > estart (%d) for OP %d\n", min_cycle, estart[i], i));
    FmtAssert(max_cycle >= lstart[i], 
	      ("Slack: max_cycle (%d) < lstart (%d) for OP %d\n", max_cycle, lstart[i], i));
  }
}

void Slack::Print(FILE *fp) const 
{
  for (INT i = 0; i < estart.size(); i++) {
    fprintf(fp, "[%d] e=%d l=%d s=%d\n", i, estart[i], lstart[i],
	    lstart[i] - estart[i]);
  }
}

// print cycle info too
void Slack::Print(FILE *fp, const SWP_OP_vector& v) const
{
  for (INT i = 0; i < v.size(); i++) {
    fprintf(fp, "[%d] e=%d l=%d s=%d", i, estart[i], lstart[i], 
	lstart[i] - estart[i]);
    if (v[i].placed) 
	fprintf(fp, ", cycle=%d", v[i].cycle);
    fprintf(fp, "\n");
  }
}

void Slack::Set_last_cycle(const SWP_OP_vector& v, INT last_cycle, const MinDist& mindist)
{
  for (INT i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op) {
      estart[i] = mindist(start, i);
      lstart[i] = last_cycle - mindist(i, stop);
    } else {
      estart[i] = 0;
      lstart[i] = 0;
    }
  }
  estart[start] = 0;
  lstart[start] = last_cycle - mindist(start, stop);
  estart[stop] = mindist(start, stop);
  lstart[stop] = last_cycle;
  if (trace) {
	fprintf(TFile, "set_last_cycle:\n");
	Print(TFile, v);
  }
}

void Slack::Relax_Precedence(const SWP_OP_vector& v, const std::vector<INT>& unplaced,
			     const std::vector<INT>& need_relax, const MinDist& mindist)
{
  if (trace) {
	fprintf(TFile, "before relax_precedence:\n");
	Print(TFile, v);
  }
  std::vector<bool> processed(v.size(), false);
  std::vector<INT> need_process;
  {
    for (INT u = 0; u < unplaced.size(); u++) {
      INT i = unplaced[u];
      if (v[i].op)
	need_process.push_back(i);
    }
    for (INT r = 0; r < need_relax.size(); r++) {
      INT i = need_relax[r];
      if (v[i].op)
	need_process.push_back(i);
    }
    while (need_process.size() > 0) {
      INT i = need_process.back();
      need_process.pop_back();
      processed[i] = true;
      ARC_LIST* al;
      for (al = OP_preds(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *pred = ARC_pred(arc);
	INT pred_idx = SWP_index(pred);
	if (!v[pred_idx].placed && !processed[pred_idx]) {
	  processed[pred_idx] = true;
	  need_process.push_back(pred_idx);
	}
      }
      for (al = OP_succs(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *succ = ARC_succ(arc);
	INT succ_idx = SWP_index(succ);
	if (!v[succ_idx].placed && !processed[succ_idx]) {
	  processed[succ_idx] = true;
	  need_process.push_back(succ_idx);
	}
      }
    }
  }

  // The number of unplaced OPs is usually small,
  // therefore process unplaced OPs in the outer loop.
  //
  INT last_cycle = lstart[stop];
  for (INT i=0; i < processed.size(); i++) {
    if (!processed[i]) continue;
    if (trace) {
      fprintf(TFile, "relax_precedence for %d\n", i);
    }
    estart[i] = mindist(start, i);
    lstart[i] = last_cycle - mindist(i, stop);
    for (INT j = 0; j < v.size(); j++) {
      if (v[j].placed) {
	INT cycle = v[j].cycle;
	estart[i] = MAX(estart[i], cycle + mindist(j, i));
	lstart[i] = MIN(lstart[i], cycle - mindist(i, j));
      }
    }
  }
  if (trace) {
	fprintf(TFile, "after relax_precedence:\n");
	Print(TFile, v);
  }
}

void Slack::Update_Slack_From_Placed_Op(INT candidate, 
					const SWP_OP_vector& v, const MinDist& mindist)
{
  INT cycle = v[candidate].cycle;
  if (trace) {
	fprintf(TFile, "update_slack_from_placed_op:\n");
	fprintf(TFile, "candidate = %d, cycle = %d\n", candidate, cycle);
  }
  for (INT i = 0; i < v.size(); i++) {
    if (!v[i].placed) {
      estart[i] = MAX(estart[i], cycle + mindist(candidate, i));
      lstart[i] = MIN(lstart[i], cycle - mindist(i, candidate));
    }
  }
  if (trace) {
	Print(TFile, v);
  }
}

void Slack::Update_Slack_From_Placed_Ops(const SWP_OP_vector& v, const MinDist& mindist)
{
  INT last_cycle = lstart[stop];
  if (trace) {
	fprintf(TFile, "update_slack_from_placed_ops:\n");
	fprintf(TFile, "last_cycle = %d, start = %d, stop = %d\n", last_cycle, start, stop);
  }
  for (INT i = 0; i < v.size(); i++) {
    if (v[i].op && !v[i].placed) {
      estart[i] = mindist(start, i);
      lstart[i] = last_cycle - mindist(i, stop);
      for (INT j = 0; j < v.size(); j++) {
	if (v[j].placed) {
	  INT cycle = v[j].cycle;
	  estart[i] = MAX(estart[i], cycle + mindist(j, i));
	  lstart[i] = MIN(lstart[i], cycle - mindist(i, j));
	}
      }
    }
  }
  if (trace) {
	Print(TFile, v);
  }
}


Slack::Slack(const SWP_OP_vector& v, INT start_idx, INT stop_idx, INT ii, const MinDist &mindist, bool trace)
  :start(start_idx),stop(stop_idx),estart(stop_idx+1,0),lstart(stop_idx+1,0),trace(trace)
{
  // A difference from Huff's paper to reduce backtracking.
  // The schedule length is at least the critical path length+2 and roundup to next ii
  // so each operation on the critical path has three cycles to schedule on.
  INT len = (INT) ceil(((double) mindist(start, stop) + 1) / ii) * ii;
  if (mindist(start, stop) > 8) {
    len = MAX(mindist(start,stop) + 1 + 2 /* slack==2 */, len);
    len = (INT) ceil(((double) len) / ii) * ii;
  }
  Set_last_cycle(v, len-1, mindist);
  if (trace) {
	fprintf(TFile, "slack:\n");
	Print(TFile, v);
  }
}


//************************************************************************
//  Modulo Reservation Table (MRT)
//************************************************************************

class MRT {
  INT ii;
  INT grainy_resources_length;
  TI_RES_RES *resources;

public:
  TI_RES_RES *Res() { return resources; }

  void Reserve_Op_Resources(const SWP_OP& swp_op, INT cycle) {
    TI_RES_RES_Reserve_Resources(resources, OP_code(swp_op.op), cycle);
  }
  void Unreserve_Op_Resources(const SWP_OP& swp_op) {
    TI_RES_RES_Unreserve_Resources(resources, OP_code(swp_op.op), swp_op.cycle);
  }
  bool Resources_Available(const SWP_OP& swp_op, INT cycle) const {
    return TI_RES_RES_Resources_Available(resources, OP_code(swp_op.op), cycle);
  }
  bool Resources_Grainy(const SWP_OP& swp_op) const {
    // return TI_RES_RES_Resources_Grainy(resources, OP_code(swp_op.op));
    return TI_RES_RES_Resources_Length(resources, OP_code(swp_op.op)) >= grainy_resources_length;
  }
  bool Resources_Equivalent(const SWP_OP& sop1, const SWP_OP& sop2) const {
    return
      sop1.op == sop2.op ||
      TI_RES_RES_Resources_Equivalent(resources, OP_code(sop1.op), OP_code(sop2.op));
  }
  bool Resources_Relevant(const SWP_OP& sop1, const SWP_OP& sop2) const {
    if (sop1.cycle < sop2.cycle) 
      return TI_RES_RES_Resources_Relevant(resources, OP_code(sop1.op), OP_code(sop2.op),
					   sop2.cycle - sop1.cycle);
    else
      return TI_RES_RES_Resources_Relevant(resources, OP_code(sop2.op), OP_code(sop1.op),
					   sop1.cycle - sop2.cycle);
  }
  void Verify() const {
    FmtAssert(! TI_RES_RES_Is_Bad_II(resources, ii), ("MRT: bad II."));
  }
  INT Find_Resources_In_Range(INT candidate, const SWP_OP_vector& v, 
			     INT earliest, INT latest, bool top_down) const;
  void Verify2(const SWP_OP_vector& v);
  MRT(const SWP_OP_vector& v, INT _ii, MEM_POOL *pool);
};
#ifdef TARG_IA64
BOOL Violate_forbid_latency(INT candidate, const SWP_OP_vector& v, INT cycle, INT loop_cycle)
{
    // cache conflict! load/load MEMREAD
    if (OP_load(v[candidate].op)) {
      for (INT i=0; i<v.size();i++){
       if (!v[i].op || !v[i].placed) continue;
       if (v[i].cycle != cycle) continue;
       if (!OP_load(v[i].op) && i!=candidate) continue;
       if (Cache_Has_Conflict(v[candidate].op,v[i].op, CG_DEP_MEMREAD))
          return TRUE;
      }
    }
    return FALSE;
}
#endif

INT MRT::Find_Resources_In_Range(INT candidate, const SWP_OP_vector& v, 
				INT earliest, INT latest, bool top_down) const {
  INT incr = top_down ? 1 : -1;
  INT begin = top_down ? earliest : latest;
  INT finish = top_down ? (latest+1) : (earliest-1);
  INT cycle;
  Is_True (earliest <= latest, ("swp sched:  earliest %d > latest %d", earliest, latest));
  for (cycle = begin; cycle != finish; cycle += incr) {
#ifdef TARG_IA64
    if (Violate_forbid_latency(candidate, v, cycle, ii)) continue; 
#endif
    if (Resources_Available(v[candidate], cycle))
      return cycle;
  }
  return cycle;
}


void MRT::Verify2(const SWP_OP_vector& v) 
{
  CXX_MEM_POOL local_mem_pool("MRT verify", FALSE);
  MRT mrt_verify(v, ii, local_mem_pool());
  for (INT i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op && v[i].placed)
      mrt_verify.Reserve_Op_Resources(v[i], v[i].cycle);
  }
  FmtAssert(TI_RES_RES_Equal(Res(), mrt_verify.Res()),
	    ("resource table inconsistent state."));
}


MRT::MRT(const SWP_OP_vector& v, INT _ii, MEM_POOL *pool):ii(_ii)
{
  resources = TI_RES_RES_Alloc(TRUE /*cyclic*/, pool);
  for (INT i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op)
      TI_RES_RES_Has_TOP(resources, OP_code(op));
  }
  TI_RES_RES_Set_BB_Cycle_Count(resources, ii);
  grainy_resources_length = SWP_Options.Grainy_Resources_Length;
}


//************************************************************************
//  The LifeTime modulo scheduler heuristcs:
//   - This heuristics is based on the paper by Richard A. Huff, SIGPLAN'93
//     "Lifetime-Sensitive Modulo Scheduling."
//************************************************************************
class LT_Heuristics {
  
  bool trace;
  bool trace_details;
  bool min_retry;
  std::vector<INT> unplaced;
  std::vector<INT> need_relax;
  INT max_sched_length;
  
  // An operation should schedule 
  //   1) top-down if it has more stretchable inputs than outputs
  //   2) bottom-up if it has less stretchable inputs than outputs
  //   3) schedules closer to the placed operations,
  //       i.e., top-down if more predecessors are placed than successors
  //             bottom-up if more successors are placed than predecessors
  //   4) top-down if both it ties on both the lifetime and the 
  //      placed-operations criteria.
  // Note: it returns top-down if the earliest and latest start is identical.
  bool Sched_Top_Down(INT candidate, SWP_OP_vector& v, const Slack& slack)
  {
    if (slack.Estart(candidate) == slack.Lstart(candidate)) return true;
    if (v[candidate].direction == SWP_TOP_DOWN) return true;
    if (v[candidate].direction == SWP_BOTTOM_UP) return false;
    INT placed_preds = 0;
    INT placed_succs = 0;
    ARC_LIST* al;
    for (al = OP_preds(v[candidate].op) ; al; al = ARC_LIST_rest(al) ) {
      ARC *arc = ARC_LIST_first(al);
      OP *pred = ARC_pred(arc);
      INT pred_idx = SWP_index(pred);
      if (v[pred_idx].placed)
	placed_preds++;
    }
    for (al = OP_succs(v[candidate].op) ; al; al = ARC_LIST_rest(al) ) {
      ARC *arc = ARC_LIST_first(al);
      OP *succ = ARC_succ(arc);
      INT succ_idx = SWP_index(succ);
      if (v[succ_idx].placed)
	placed_succs++;
    }

    // Note: this is a derivation from Huff's paper.  In his paper, an operation's
    // schedule direction may change.  That might cause certain issue slot not
    // searched.  We fixed the direction of scheduling.
    if (placed_preds >= placed_succs) {
      v[candidate].direction = SWP_TOP_DOWN;
      return true;
    } else {
      v[candidate].direction = SWP_BOTTOM_UP;
      return false;
    }
  }

public:
  
  //  Initialize an operation's static properties:
  //    1) priority scale based on critical or grainy resources
  //    2) schedule direction based on stretchable inputs and outputs
  //
  // Note: randomize the priority and the schedule-direction can help
  // to force more backtracking and helps to flush the bugs!  -Raymond
  //
  void Init_SWP_OP_state(SWP_OP_vector& v, INT ii, 
			 const Slack& slack, const MinLT& minlt, const MRT& mrt,
			 bool mop_critical, bool flop_critical)
  {
    INT start = v.start;
    INT stop  = v.stop;
    
    // Place the START and STOP node
    v[start].placed = true;
    v[start].cycle = slack.Estart(start);
    v[stop].placed = true;
    v[stop].cycle = slack.Lstart(stop);
    max_sched_length = slack.Lstart(stop) + SWP_Options.Max_Schedule_Incr;

    for (INT i = 0; i < v.size(); i++) {
      OP *op = v[i].op;
      if (!op) continue;

      v[i].placed = false;
      v[i].cycle = 0;
      v[i].scale = 1.0;
      v[i].trials = 0;

      // Initialize based on resources
      if (mrt.Resources_Grainy(v[i]))
	v[i].scale *= 0.5;

      // critical resource heuristics
      if (mop_critical && OP_memory(op))
	v[i].scale *= 0.1;
      if (flop_critical && OP_flop(op))
	v[i].scale *= 0.2;

      if (SWP_Options.Sched_Direction == 1)
	v[i].direction = SWP_TOP_DOWN;
      else if (SWP_Options.Sched_Direction == 2)
	v[i].direction = SWP_BOTTOM_UP;
      else {
	// Initialize based on lifetime
	INT stretchable_inputs = 0;
	INT stretchable_outputs = 1;
	for (INT j = 0; j < OP_opnds(op); j++) {
	
	  // skip duplicated operands
	  for (INT k = j+1; k < OP_opnds(op); k++) {
	    if (OP_opnd(op, j) == OP_opnd(op, k))
	      goto next_opnd; 
	  }

	  ARC *arc;
	  arc = ARC_LIST_Find_First(OP_preds(op), CG_DEP_REGIN, j);
	  if (arc == NULL)	  // skip invariants
	    goto next_opnd;
	  if (ARC_pred(arc) == op)  // skip self recurrence
	    goto next_opnd;

	  // Determine if input is stretchable?
	  INT pred_op_idx;
	  pred_op_idx = SWP_index(ARC_pred(arc));
	  if (slack.Estart(pred_op_idx) + minlt(pred_op_idx) < 
	      ARC_omega(arc) * ii + slack.Lstart(i))
	    stretchable_inputs++;

	next_opnd: ;
	}

	// TODO: skip predicated definitions from stretchable outputs
	// TODO: skip predicate TNs because there are plenty of predicate registers
	if (stretchable_inputs > stretchable_outputs)
	  v[i].direction = SWP_TOP_DOWN;
	else if (stretchable_inputs < stretchable_outputs)
	  v[i].direction = SWP_BOTTOM_UP;
	else
	  v[i].direction = SWP_UNKOWN;
      }
    }
  }

  //  Choose an operation based on slack * scale.
  //  Return -1 if none is found.
  INT Choose_Op(const SWP_OP_vector& v, const Slack& slack)
  {
    INT candidate = -1;
    double highest_priority = 100000;
    switch (SWP_Options.Heuristics) {
    case 0:
      {
	// based on slack, scaled by critical resources
	//
	INT candidate_lstart = 0;
	INT trials = 0;
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  // so that slack == 0 can still be distinguish by scale!
	  INT pri = (INT) (slack(i) * v[i].scale);
	  if (pri < highest_priority ||
	      pri == highest_priority && 
	      (slack.Lstart(i) < candidate_lstart ||
	       (slack.Lstart(i) == candidate_lstart &&
		v[i].trials < trials))) {
	    highest_priority = pri;
	    candidate_lstart = slack.Lstart(i);
	    trials = v[i].trials;
	    candidate = i;
	  }
	}
      }
#ifdef TARG_IA64
      break;
#endif	
    case 1:
      {
	// operation with smallest Lstart are scheduled first
	//
	INT candidate_slack = 0;
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  INT pri = slack.Lstart(i);
	  if (pri < highest_priority ||
	      pri == highest_priority && slack(i) < candidate_slack) {
	    highest_priority = pri;
	    candidate_slack = slack(i);
	    candidate = i;
	  }
	}
      }
#ifdef TARG_IA64
      break;
#endif
    case 2:
      {
	// operation with largest Estart are scheduled first
	//
	INT last_cycle = slack.Lstart(slack.Stop());
	INT candidate_slack = 0;
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  INT pri = last_cycle - slack.Estart(i);
	  if (pri < highest_priority ||
	      pri == highest_priority && slack(i) < candidate_slack) {
	    highest_priority = pri;
	    candidate_slack = slack(i);
	    candidate = i;
	  }
	}
      }
    }
    return candidate;
  }

  //  Choose An issue cycle
  //   - Return value is a pair<bool, bool>
  //   - The first bool is set to TRUE if precedence constraints might be violated
  //   - The second bool is set to TRUE if resource constraints might be violated.
  std::pair<bool, bool>
  Choose_Issue_Cycle(INT candidate, SWP_OP_vector& v, INT ii, 
		     const Slack& slack, const MRT& mrt)
  {
    v[candidate].trials++;
    v[candidate].placed = true;
    INT earliest = slack.Estart(candidate);
    INT latest   = MIN(earliest + ii - 1, slack.Lstart(candidate));
    bool top_down = Sched_Top_Down(candidate, v, slack);

    Is_True(earliest < 128 * ii, ("SWP Choose_Issue_Cycle: earliest=%d\n", earliest));
    Is_True(latest < 128 * ii, ("SWP Choose_Issue_Cycle: latest=%d\n", latest));

    // If the candidate has previous trial and there is resource for the next slot
    // try to use the next slot instead retry from beginning!
    if (min_retry && v[candidate].trials > 1) {
      INT e = top_down ? MAX(earliest, v[candidate].cycle+1) : earliest;
      INT l = top_down ? latest : MIN(latest, v[candidate].cycle-1);
      if (e <= l) {
	INT cycle = mrt.Find_Resources_In_Range(candidate, v, e, l, top_down);
	if (e <= cycle && cycle <= l) {
	  v[candidate].cycle = cycle;
	  return std::pair<bool,bool>(false, false);
	}
      }
    }
    // else retry the whole range

    INT cycle = mrt.Find_Resources_In_Range(candidate, v, earliest, latest, top_down);
    if (earliest <= cycle && cycle <= latest) {
      v[candidate].cycle = cycle;
      return std::pair<bool,bool>(false, false);
    }
  
    // unable to find available resources in the issue range,
    //  choose an issue slot.
    if (v[candidate].trials == 1)
      v[candidate].cycle = top_down ? earliest : latest;
    else 
      top_down ? ++v[candidate].cycle : --v[candidate].cycle;
    return std::pair<bool,bool>(v[candidate].cycle < earliest || v[candidate].cycle > latest
			   /*has precedence conflicts*/,
			   !mrt.Resources_Available(v[candidate], v[candidate].cycle)
			   /*has resources conflicts*/);
  }

  // Eject operation that violates the precedence constraints with
  // the candidate OP.
  void Eject_Precedence_Conflict_OPs(INT candidate, SWP_OP_vector& v, 
				     const Slack& slack, const MinDist& mindist, MRT& mrt)
  {
    std::insert_iterator<std::vector<INT> > ui(unplaced, unplaced.end());
    std::insert_iterator<std::vector<INT> > ri(need_relax, need_relax.end());
    INT sched_cycle = v[candidate].cycle;
    for (INT i = 0; i < v.size(); i++) {
      if (v[i].placed) {
	INT estart = sched_cycle + mindist(candidate,i);
	INT lstart = sched_cycle - mindist(i, candidate);
	if (v[i].cycle < estart || v[i].cycle > lstart) {
	  *ui = i;
	  v[i].placed = false;
	  if (trace_details)
	    fprintf(TFile, "  eject OP %d due to precedence constraints.\n", i);
	}
      }
      else if (v[i].op) {
	// already unplaced ops may still need to have slack adjusted
	// due to precedence constraints.
	// This may add ops that are already on unplaced list,
	// but relax_precedence handles duplicates okay.
	INT estart = sched_cycle + mindist(candidate,i);
	INT lstart = sched_cycle - mindist(i, candidate);
	if (slack.Estart(i) > estart || slack.Lstart(i) < lstart) {
	  *ri = i;
	  if (trace_details)
	    fprintf(TFile, "  add OP %d to relaxed due to precedence constraints.\n", i);
	}
      }
    }
  }

  //  Eject OPs with reources conflicts
  void Eject_Resources_Conflict_OPs(INT candidate, SWP_OP_vector& v, INT ii, MRT& mrt)
  {
    std::insert_iterator<std::vector<INT> > ins(unplaced, unplaced.end());
    INT sched_cycle = v[candidate].cycle;
    bool ops_unplaced = false;
    for (INT i = 0; i < v.size(); i++) {
      if (v[i].placed &&
	  v[i].op &&
	  (v[i].cycle - sched_cycle) % ii == 0 &&
	  i != candidate &&
	  mrt.Resources_Equivalent(v[i], v[candidate])) {
	*ins = i;
	v[i].placed = false;
	ops_unplaced = true;
	if (trace_details)
	  fprintf(TFile, "  eject OP %d due to equivalent resources.\n", i);
      }
    }
    if (!ops_unplaced) {
      for (INT i = 0; i < v.size(); i++) {
	if (v[i].placed &&
	    v[i].op &&
	    i != candidate &&
	    mrt.Resources_Relevant(v[i], v[candidate])) {
	  *ins = i;
	  v[i].placed = false;
	  ops_unplaced = true;
	  if (trace_details)
	    fprintf(TFile, "  eject OP %d due to relevant resources.\n", i);
	}
      }
    }
    if (!ops_unplaced) {
      // running out of issue slot - a problem with small II
      for (INT i = 0; i < v.size(); i++) {
	if (v[i].placed &&
	    v[i].op &&
	    i != candidate &&
	    ((v[i].cycle - v[candidate].cycle) % ii) == 0) {
	  *ins = i;
	  v[i].placed = false;
	  ops_unplaced = true;
	  if (trace_details)
	    fprintf(TFile, "  eject OP %d due to common resources.\n", i);
	}
      }
    }
    Is_True(ops_unplaced, ("Eject_Resources_Conflict_OPs: cannot eject."));
  }

  //  Update Resources Requirements
  void Update_Resources(INT candidate, SWP_OP_vector& v, MRT& mrt)
  {
    for (INT u = 0; u < unplaced.size(); u++) {
      INT i = unplaced[u];
      if (v[i].op)
	mrt.Unreserve_Op_Resources(v[i]);
    }
    Is_True(mrt.Resources_Available(v[candidate], v[candidate].cycle),
	    ("Update_Reources: cannot eject enough resources for OP %d.", candidate));
    mrt.Reserve_Op_Resources(v[candidate], v[candidate].cycle);
  }

  //  Update Precedence Requirements
  bool Update_Precedence(INT candidate, SWP_OP_vector& v, Slack& slack, const MinDist& mindist, MRT& mrt)
  {
    if (unplaced.size() > 0 || need_relax.size() > 0) 
      slack.Relax_Precedence(v, unplaced, need_relax, mindist);
    slack.Update_Slack_From_Placed_Op(candidate, v, mindist);
    
    INT start = slack.Start();
    INT stop = slack.Stop();
    
    if (!v[start].placed || !v[stop].placed) {
#pragma mips_frequency_hint NEVER
      INT adjustment = MAX(slack.Estart(start) - slack.Lstart(start), 0);
      INT sched_len = MAX(slack.Estart(stop), slack.Lstart(stop) + adjustment);
      if (sched_len > max_sched_length)
	return false;
      if (trace) 
	fprintf(TFile, "Increase sched_len to %d.  Adjust start by %d.\n", sched_len, adjustment); 
      if (adjustment > 0) {
	INT i;
	for (i = 0; i < v.size(); i++) {
	  OP *op = v[i].op;
	  if (op && v[i].placed) {
	    mrt.Unreserve_Op_Resources(v[i]);
	    v[i].cycle += adjustment;
	    if (trace) 
	      fprintf(TFile, "Adjust OP %d to cycle %d\n", i, v[i].cycle);
	  }
	}
	for (i = 0; i < v.size(); i++) {
	  OP *op = v[i].op;
	  if (op && v[i].placed)
	    mrt.Reserve_Op_Resources(v[i], v[i].cycle);
	}
      }
      slack.Set_last_cycle(v, sched_len, mindist);
      // set start/stop cycle before update, so updates are correct
      v[start].placed = true;
      v[start].cycle = slack.Estart(start);
      v[stop].placed = true;
      v[stop].cycle = slack.Lstart(stop);
      slack.Update_Slack_From_Placed_Ops(v, mindist);
    }
    unplaced.erase(unplaced.begin(), unplaced.end());
    need_relax.erase(need_relax.begin(), need_relax.end());
    return true;
  }

  void Print(FILE *fp) {
    if (unplaced.size() > 0) {
      fprintf(fp, "unplaced Ops: ");
      for (INT i = 0; i < unplaced.size(); i++)
	fprintf(fp, "%d ", unplaced[i]);
      fprintf(fp, "\n");
    }
    if (need_relax.size() > 0) {
      fprintf(fp, "need_relax Ops: ");
      for (INT i = 0; i < need_relax.size(); i++)
	fprintf(fp, "%d ", need_relax[i]);
      fprintf(fp, "\n");
    }
  }

  LT_Heuristics(bool t, bool d):trace(t), trace_details(d), min_retry(SWP_Options.Min_Retry) {}
};


void Modulo_Schedule_Verify(SWP_OP_vector &v, INT ii, MRT& mrt)
{
  // Verify Resources
  mrt.Verify2(v);

    // Verify schedule
  for (INT i = 0; i < v.size(); i++) {
    if (v[i].op) {
      FmtAssert(v[i].placed, ("op is not placed."));
      INT sched_cycle = v[i].cycle;
      ARC_LIST *al;
      for (al = OP_preds(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *pred = ARC_pred(arc);
	INT pred_idx = SWP_index(pred);

	FmtAssert(sched_cycle - v[pred_idx].cycle >= ARC_latency(arc) - ARC_omega(arc) * ii,
		  ("OP %d at cycle %d and OP %d at cycle %d violated precedence constraints of %d cycles.",
		   pred_idx, v[pred_idx].cycle, i, v[i].cycle, ARC_latency(arc)));
      }
      for (al = OP_succs(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *succ = ARC_succ(arc);
	INT succ_idx = SWP_index(succ);
	FmtAssert(v[succ_idx].cycle - sched_cycle >= ARC_latency(arc) - ARC_omega(arc) * ii,
		  ("OP %d at cycle %d and OP %d at cycle %d violated precedence constraints of %d cycles.",
		   i, v[i].cycle, succ_idx, v[succ_idx].cycle, ARC_latency(arc)));
      }
    }
  }
}


void Modulo_Schedule_Succeeded(SWP_OP_vector &v,
			       INT ii, INT min_ii, INT max_ii, const MinDist& mindist,
			       MRT& mrt, bool trace)
{
#ifdef Is_True_On
  Modulo_Schedule_Verify(v, ii, mrt);
#endif

  // Produce Statistics
  v.succeeded = true;
  v.min_ii = CG_LOOP_min_ii;
  v.res_min_ii = CG_LOOP_res_min_ii;
  v.rec_min_ii = CG_LOOP_rec_min_ii;
  v.ii = ii;
  v.min_sl = mindist(v.start,v.stop)+1;
  
  // rotate the br to be the last instruction in the modulo schedule.
  INT adjustment = ((v[v.branch].cycle + ii) / ii) * ii - 1 - v[v.branch].cycle;
  INT max_cycle;
  INT min_cycle;
  do {
    min_cycle = 1000;
    max_cycle = 0;
    for (INT i = 0; i < v.size(); i++) {
      v[i].cycle += adjustment;

      // skip br.ctop for do-loop because it can be freely moved in the schedule.
      //  must consider br.wtop because it has dependence on the predicate
      //  computation and can be moved.
      //
      if (v[i].op && (i != v.branch || !v.is_doloop)) {
	min_cycle = MIN(min_cycle, v[i].cycle);
	max_cycle = MAX(max_cycle, v[i].cycle);
      }
      if (v[i].op && OP_dummy(v[i].op))
	v[i].op = NULL;  // remove dummy ops 
    }
    adjustment = -ii;
  } while (min_cycle >= ii);

  v.sc = max_cycle / ii + 1;
  // Include branch for the sched length
  max_cycle = MAX(max_cycle, v[v.branch].cycle);
  v.sl = max_cycle - min_cycle + 1;

  Is_True(min_cycle < ii, ("first operation is not at stage 0."));

  if (!v.is_doloop) {
    v.loop_one_more_time = true;
    // special case for br.wtop:
    //  if the predicate computation of the branch and the branch is in stage 0,
    //  the initial predicate value does not increase the loop execution count by 1.
    INT branch = v.branch;
    OP *br_op = v[branch].op;
    if (v[branch].cycle / ii == 0) 
      v.loop_one_more_time = false;
    if (trace) 
      fprintf(TFile, "while-loop has cmp and branch %s at stage 0.\n",
	      v.loop_one_more_time ? "" : "not");
  }
}


//************************************************************************
//  The standard backtracking SWP algorithm
//************************************************************************
SWP_RETURN_CODE
Modulo_Schedule(SWP_OP_vector &swp_op_vector, INT min_ii, INT max_ii, 
		double incr_alpha, double incr_beta, 
		INT budget, bool trace, bool trace_details)
{
#ifdef Is_True_On
  swp_op_vector.Verify();
#endif
  swp_op_vector.ii = 0;

  bool trace_slack = Get_Trace(TP_SWPIPE, 16);
  if (swp_op_vector.size() >= SWP_Options.OPS_Limit) {
    if (trace)
      fprintf(TFile, "MOD SCHED FAILED: loop too big!");
    return MOD_SCHED_FAILED;
  }

  for (INT ii = min_ii; 
       ii <= max_ii;
       ii = MAX(ii+1, (INT)((ii + incr_alpha) * incr_beta - incr_alpha))) {

    if (trace) 
      fprintf(TFile, "============================\nSWP SCHED with ii %d\n", ii);

    bool mop_critical = ((double) swp_op_vector.num_mops / ii > 
			 2 * SWP_Options.Critical_Threshold / 100.0);
    bool flop_critical = ((double) swp_op_vector.num_flops / ii >
			 2 * SWP_Options.Critical_Threshold / 100.0);
    if (trace) {
      fprintf(TFile, "swp: %d memop is %s critical\n",
	      swp_op_vector.num_mops,  mop_critical ? "" : "not");
      fprintf(TFile, "swp: %d flop is %s critical\n", 
	      swp_op_vector.num_flops, flop_critical ? "" : "not");
    }

    CXX_MEM_POOL local_mem_pool("modulo schedule pool", FALSE);
    MRT mrt(swp_op_vector, ii, local_mem_pool());
    MinDist mindist(swp_op_vector, swp_op_vector.start, swp_op_vector.stop, swp_op_vector.branch, ii);
    Is_True(ii == mindist.Found_ii(), ("Modulo_Schedule: MinDist found a different ii."));

    MinLT minlt(swp_op_vector, ii, mindist);
    Slack slack(swp_op_vector, swp_op_vector.start, swp_op_vector.stop, ii, mindist, trace_slack);
    Slack slack_static(swp_op_vector, swp_op_vector.start, swp_op_vector.stop, ii, mindist, trace_slack);
    LT_Heuristics heur(trace, trace_details);
    heur.Init_SWP_OP_state(swp_op_vector, ii, slack, minlt, mrt,
			   mop_critical, flop_critical);

    slack.Verify();   // Verify precedence constraints
    mrt.Verify();     // Verify sanity of resources allocation

    if (trace) {
      swp_op_vector.Print(TFile);
      mindist.Print(TFile);
      slack.Print(TFile, swp_op_vector);
      minlt.Print(TFile);
    }

    INT num_placed;
    for (num_placed = 0; true; num_placed++) {

#ifdef Is_True_On
      // This is only under is_true_on cause can double compilation time.
      slack.Verify();   // Verify precedence constraints
      mrt.Verify2(swp_op_vector);     // Verify sanity of resources allocation
#endif

      if (trace_details)
	slack.Print(TFile, swp_op_vector);

      INT candidate = heur.Choose_Op(swp_op_vector, slack);  // Get next highest priority op

      if (candidate < 0) {
	Modulo_Schedule_Succeeded(swp_op_vector, ii, min_ii, max_ii, mindist, mrt, trace);
	return MOD_SCHED_SUCCEEDED;
      }
    
      std::pair<bool,bool> issue = heur.Choose_Issue_Cycle(candidate, swp_op_vector, ii, slack, mrt);

      if (SWP_Options.Opt_Level == 0) {
	if (swp_op_vector[candidate].cycle > slack_static.Lstart(candidate) ||
	    swp_op_vector[candidate].cycle < slack_static.Estart(candidate)) {
	  if (trace)
	    fprintf(TFile,
		    "MOD SCHED FAILED: OP %d at cycle %d exceed estart (%d) lstart (%d) range.\n",
		    candidate, swp_op_vector[candidate].cycle,
		    slack_static.Estart(candidate), slack_static.Lstart(candidate));
	  break;
	}
      }
      if (swp_op_vector[candidate].trials > budget) {
	if (trace)
	  fprintf(TFile, "MOD SCHED FAILED: exceed trial budget.\n");
	break;
      }

      if (trace) {
	fprintf(TFile, "placed OP %d at cycle %d estart %d lstart %d\n",
		candidate, swp_op_vector[candidate].cycle,
		slack.Estart(candidate), slack.Lstart(candidate));
      }

      if (issue.second) 
	heur.Eject_Resources_Conflict_OPs(candidate, swp_op_vector, ii, mrt);

      if (issue.first)
	heur.Eject_Precedence_Conflict_OPs(candidate, swp_op_vector, slack, mindist, mrt);
    
      if (trace)
	heur.Print(TFile); // print unplaced OPs

      heur.Update_Resources(candidate, swp_op_vector, mrt);
      
      bool satisfiable = heur.Update_Precedence(candidate, swp_op_vector, slack, mindist, mrt);
      if (!satisfiable) {
	if (trace) {
	  swp_op_vector.Print(TFile);
	  slack.Print(TFile, swp_op_vector);
	  fprintf(TFile, "failed to SWP due to  exceeding max sched length.\n");
	  fprintf(TFile, "current II=%d  max II=%d\n", ii, max_ii);
	}
	break;
      }
    }
    swp_op_vector.previous_trials += num_placed;
  }
  if (trace)
    fprintf(TFile, "MOD SCHED FAILED: cannot find any schedule smaller than maxII cycles.\n");
  return MOD_SCHED_FAILED;
}
