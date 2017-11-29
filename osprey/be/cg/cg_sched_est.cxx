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



#include <stdint.h>
#include <alloca.h>
#include "defs.h"
#include "tracing.h"
#include "cgir.h"
#include "cg_loop.h"
#include "cg_flags.h"
#include "cg_sched_est.h"
#include "cxx_memory.h"
#include "hb_sched.h"

BOOL CG_SCHED_EST_calc_dep_graph = FALSE;
BOOL CG_SCHED_EST_use_locs = FALSE;
INT32 CG_SCHED_EST_call_cost = 100;

inline BOOL tracing(void) { return Get_Trace(TP_SCHED, 0x100); }

#define LATENCY_NOT_COMPUTED ((UINT32)-1)
#define LATENCY_IGNORE ((UINT32)-2)

// =======================================================================
// Return the "latency_to" entry for <op>.
// =======================================================================
static inline 
UINT32 Get_Latency_To(CG_SCHED_EST *se, OP *op)
{
  void *entry = BB_MAP_Get(se->latency_to_map, OP_bb(op));
  DevAssert(entry, ("missing latency_to map for BB:%d", BB_id(OP_bb(op))));
  return (UINT32)(BB_OP_MAP32_Get((BB_OP_MAP)entry, op) - 1);
}

// =======================================================================
// Set the "latency_to" entry in <se> for <op> to <latency>.
// =======================================================================
static inline 
void Set_Latency_To(CG_SCHED_EST *se, OP *op, UINT32 latency)

{
  void *entry = BB_MAP_Get(se->latency_to_map, OP_bb(op));
  DevAssert(entry, ("missing latency_to map for BB:%d", BB_id(OP_bb(op))));
  BB_OP_MAP32_Set((BB_OP_MAP)entry, op, (INT32)(latency + 1));
}

// =======================================================================
// Return the definition of OP_opnd(op, i) that reaches <op>, assuming
// the schedules appended in <se>.  Return NULL if no reaching definition
// found.
// =======================================================================
static 
OP* Find_Reaching_Def(CG_SCHED_EST *se, OP *op, UINT8 i)
{
  TN *opnd = OP_opnd(op, i);
  BB *bb = OP_bb(op);

  if (!TN_is_constant(opnd)) {
    INT32 bb_order = BB_MAP32_Get(se->order, bb);
    DEF_KIND kind;
    OP *defop = TN_Reaching_Value_At_Op(opnd, op, &kind, TRUE);
    if (defop) {
      BB *def_bb = OP_bb(defop);
      INT32 def_order = BB_MAP32_Get(se->order, def_bb);

      if (def_order && def_order <= bb_order &&
	  (def_bb != bb ||
	   OP_Precedes(defop, op))) {
	return defop;
      }
    }
  }

  return NULL;
}

// Forward decl for mutually recursive calls.
static UINT32 Calc_Latency_To(CG_SCHED_EST *se, OP *op);

// =======================================================================
// If <op> is ignored, return 0.  Otherwise return the latency to <op>
// plus <latency>.  Used the value cached in <se> when available, or
// compute and cache if not.
// =======================================================================
static inline 
UINT32 Latency_Thru(CG_SCHED_EST *se, OP *op, INT32 latency)
{
  UINT32 entry = Get_Latency_To(se, op);
  if (entry == LATENCY_IGNORE)
    return 0;
  if (entry == LATENCY_NOT_COMPUTED)
    entry = Calc_Latency_To(se, op);
  if (OP_call(op))
    entry += CG_SCHED_EST_call_cost;
  return entry + latency;
}

// =======================================================================
// Compute the schedule latency to <op>.  Return the new latency and store
// it in <se>.  LATENCY_IGNORE entries are left as is.
// =======================================================================
static UINT32 
Calc_Latency_To(CG_SCHED_EST *se, OP *op)
{
  UINT32 old_entry = Get_Latency_To(se, op);
  UINT32 new_entry = old_entry;

  if (old_entry != LATENCY_IGNORE) {
    UINT8 i;
    INT op_opnds = OP_opnds(op);
    mBOOL *found_pred_for_opnd = (mBOOL *)alloca(op_opnds * sizeof(mBOOL));
    bzero(found_pred_for_opnd, op_opnds * sizeof(mBOOL));

    new_entry = 0;
    if (CG_SCHED_EST_calc_dep_graph || se->use_dep_graph)
    {
      /*
       * Use dep graph if available.
       */
      ARC_LIST *preds;
      for (preds = OP_preds(op); preds; preds = ARC_LIST_rest(preds)) {
	ARC *pred_arc = ARC_LIST_first(preds);
	if (ARC_omega(pred_arc) == 0) {
	  OP *pred = ARC_pred(pred_arc);
	  UINT32 latency = ARC_latency(pred_arc);
	  if (ARC_has_opnd(pred_arc)) {
	    INT iopnd = ARC_opnd(pred_arc);
	    Is_True(iopnd < op_opnds, ("found_pred_for_opnd alloca too small"));
	    found_pred_for_opnd[iopnd] = TRUE;
	  }
	  new_entry = umax(new_entry, Latency_Thru(se, pred, latency));
	}
      }
    }

    /* Note: This finds only REGIN predecessors.  For more accuracy,
     *	     need to find other types of preds (especially those due
     *	     to memory dependences).
     */
    for (i = 0; i < op_opnds; i++) {
      if (!found_pred_for_opnd[i]) {
	OP *pred = Find_Reaching_Def(se, op, i);
	if (pred) {
	  UINT32 latency = CG_DEP_Latency(pred, op, CG_DEP_REGIN, i);
	  new_entry = umax(new_entry, Latency_Thru(se, pred, latency));
	}
      }
    }
    Set_Latency_To(se, op, new_entry);
  }
  return new_entry;
}

// =======================================================================
// Return the estimated critical path length through the appended BBs in
// <se>.
// =======================================================================
static UINT32 
Critical_Path_Len(CG_SCHED_EST *se)
{
  UINT32 max_latency_to = 0;
  BB *bb;

  if (se->latency_to_map_dirty) {
    FOR_ALL_BB_SET_members(se->contents, bb) {
      OP *op;
      BB_OP_MAP lt_map = (BB_OP_MAP)BB_MAP_Get(se->latency_to_map, bb);
      FOR_ALL_BB_OPs(bb, op) {
	/* The -1/+1 are a little confusing.  Using
	 * {get,set}_latency_to is more clear but slower.
	 */
	if (BB_OP_MAP32_Get(lt_map, op)-1 != LATENCY_IGNORE)
	  BB_OP_MAP32_Set(lt_map, op, LATENCY_NOT_COMPUTED+1);
      }
    }
    se->latency_to_map_dirty = FALSE;
  }

  FOR_ALL_BB_SET_members(se->contents, bb) {
    OP *op;

    if (CG_SCHED_EST_calc_dep_graph && !se->use_dep_graph)
      CG_DEP_Compute_Graph(bb, 
			   NO_ASSIGNED_REG_DEPS, 
 			   NON_CYCLIC,
			   NO_MEMREAD_ARCS,
			   INCLUDE_MEMIN_ARCS,
			   NO_CONTROL_ARCS,
			   NULL);

    FOR_ALL_BB_OPs(bb, op)
      max_latency_to = umax(max_latency_to, Latency_Thru(se, op, 0));

    if (CG_SCHED_EST_calc_dep_graph && !se->use_dep_graph)
      CG_DEP_Delete_Graph(bb);
  }

  if (tracing()) {
    fprintf(TFile, "<sched_est>   critical_path_len(BBs ");
    BB_SET_Print(se->contents, TFile);
    fprintf(TFile, ") = %d cycles\n", max_latency_to);
  }

  return max_latency_to;
}

// =======================================================================
// Return the minimum number of cycles dictated by resource usage for <se>.
// =======================================================================
static UINT32 
Resource_Min_Cycles(CG_SCHED_EST *se)
{
  UINT32 result = (UINT32)(TI_RES_COUNT_Min_Cycles(se->res_count) + 0.99);
  if (tracing()) {
    fprintf(TFile, "<sched_est>   resource_min_cycles(BBs ");
    BB_SET_Print(se->contents, TFile);
    fprintf(TFile, ") = %d cycles\n", result);
  }
  return result;
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
void 
CG_SCHED_EST_Print(FILE *fp, CG_SCHED_EST *se)
{
  fprintf(fp, "SCHED_EST(BBs ");
  BB_SET_Print(se->contents, fp);
  if (se->cached_crit_path_len)
    fprintf(fp, ", crit path %d", se->cached_crit_path_len);
  fprintf(fp, ", ");
  TI_RES_COUNT_Print(fp, se->res_count);
  if (se->cached_resource_cycles)
    fprintf(fp, ", res cycles %d", se->cached_resource_cycles);
  fprintf(fp, ", ");
  if (se->sched_cycles)
    fprintf(fp, ", sched cycles %d", se->sched_cycles);  
  fprintf(fp, ")");
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
CG_SCHED_EST*
CG_SCHED_EST_Create_Empty(MEM_POOL *pool, SCHED_EST_TYPE type)
{
  CG_SCHED_EST *se = (CG_SCHED_EST *) MEM_POOL_Alloc(pool, 
						     sizeof(CG_SCHED_EST));

  se->contents = BB_SET_Create_Empty(PU_BB_Count+1, pool);
  se->res_count = TI_RES_COUNT_Alloc(pool);
  se->latency_to_map = BB_MAP_Create();
  se->order = BB_MAP32_Create();
  se->use_dep_graph = (type & SCHED_EST_USE_DEP_GRAPH) ? TRUE : FALSE;
  se->latency_to_map_dirty = FALSE;
  se->sched_cycles = 0;

  return se;
}

CG_SCHED_EST *CG_SCHED_EST_Path_Create(BB_SET *path, BB_MAP bb_ests,
				       MEM_POOL *pool, SCHED_EST_TYPE type)
/* -----------------------------------------------------------------------
 * See "cg_sched_est.h" for interface specification.
 * ----------------------------------------------------------------------- */
{
  BB *bb;
  CG_SCHED_EST *path_se = CG_SCHED_EST_Create_Empty(pool, type);
  
  FOR_ALL_BB_SET_members(path, bb) {
    CG_SCHED_EST *se = (CG_SCHED_EST *)BB_MAP_Get(bb_ests, bb);
    if (!se) {
      se = CG_SCHED_EST_Create(bb, pool, type);
      BB_MAP_Set(bb_ests,bb,se);
    }
    TI_RES_COUNT_Add(path_se->res_count, path_se->res_count, se->res_count);
    if (type == SCHED_EST_FOR_HB) {
      path_se->sched_cycles = (INT)TI_RES_COUNT_Min_Cycles(path_se->res_count);
    } else {
      path_se->sched_cycles += se->sched_cycles;
    }
  }
  return path_se;
}

CG_SCHED_EST *CG_SCHED_EST_Create(BB *bb, MEM_POOL *pool,
				  SCHED_EST_TYPE type)
/* -----------------------------------------------------------------------
 * See "cg_sched_est.h" for interface specification.
 * ----------------------------------------------------------------------- */
{
  OP *op;
  CG_SCHED_EST* se = CG_SCHED_EST_Create_Empty(pool, type);
  BB_MAP_Set(se->latency_to_map, bb, BB_OP_MAP32_Create(bb, pool));
  se->contents = BB_SET_Union1D(se->contents, bb, NULL);
  BB_MAP32_Set(se->order, bb, 1);
  
  if (CG_SCHED_EST_use_locs && LOCS_Enable_Scheduling &&
      type != SCHED_EST_FOR_HB) {

    HB_Schedule *Sched = CXX_NEW(HB_Schedule(), pool);
    Sched->Init(bb, 
		HBS_BEFORE_GRA | HBS_FROM_CGPREP, 
		INT32_MAX,
		NULL, 
		NULL);
    Sched->Schedule_BB(bb, NULL);

    if (BB_last_op(bb)) 
      se->sched_cycles = OP_scycle(BB_last_op(bb)) + 1;
  } else {

    FOR_ALL_BB_OPs(bb, op) {
      BOOL acct_res = TRUE;

      if (type & SCHED_EST_IGNORE_PREFETCH) 
	if (OP_prefetch(op)) 
	  acct_res = FALSE;

      if (type & SCHED_EST_IGNORE_COPY)
	if (OP_copy(op))
	  acct_res = FALSE;

      if (type & SCHED_EST_IGNORE_BRANCH)
	if (OP_br(op))
	  acct_res = FALSE;

      if (type & SCHED_EST_IGNORE_INT_OPS)
	if (!OP_memory(op) && !OP_flop(op))
	  acct_res = FALSE;

      if (type & SCHED_EST_IGNORE_LOH_OPS)
	if (OP_loh(op))
	  acct_res = FALSE;
	  
      if (acct_res)  
	TI_RES_COUNT_Add_Op_Resources(se->res_count, OP_code(op));

    }
  }
  
  se->cached_crit_path_len = 0;
  se->cached_resource_cycles = 0;
  if (tracing()) {
    fprintf(TFile, "<sched_est> creating ");
    CG_SCHED_EST_Print(TFile, se);
    fprintf(TFile, "\n");
  }
 
  return se;
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
void CG_SCHED_EST_Delete(CG_SCHED_EST *se)
{
  BB_MAP_Delete(se->latency_to_map);
  BB_MAP_Delete(se->order);
}

// =======================================================================
// Clone the mappings (including the latency_to BB_OP_MAPs) for the BBs
// in <from> into the mappings of <to>.
// =======================================================================
static void 
Clone_Mappings(CG_SCHED_EST *to, CG_SCHED_EST *from, MEM_POOL *pool)
{
  BB *bb;
  INT32 num_bbs_before = BB_SET_Size(to->contents);
  FOR_ALL_BB_SET_members(from->contents, bb) {
    BB_OP_MAP lt_map = (BB_OP_MAP)BB_MAP_Get(from->latency_to_map, bb);
    INT32 order = BB_MAP32_Get(from->order, bb);
    BB_OP_MAP to_lt_map = BB_OP_MAP32_Create(bb, pool);
    OP *op;
    DevAssert(lt_map, ("missing latency_to map for BB:%d", BB_id(bb)));
    DevAssert(order, ("missing order for BB:%d", BB_id(bb)));
    FOR_ALL_BB_OPs(bb, op)
      BB_OP_MAP32_Set(to_lt_map, op, BB_OP_MAP32_Get(lt_map, op));
    BB_MAP_Set(to->latency_to_map, bb, to_lt_map);
    BB_MAP32_Set(to->order, bb, order);
  }
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
CG_SCHED_EST*
CG_SCHED_EST_Clone(CG_SCHED_EST *se, MEM_POOL *pool)
{
  CG_SCHED_EST *clone = (CG_SCHED_EST *)MEM_POOL_Alloc(pool, sizeof(CG_SCHED_EST));
  clone->contents = BB_SET_Copy(se->contents, pool);
  clone->res_count = TI_RES_COUNT_Alloc(pool);
  TI_RES_COUNT_Add(clone->res_count, se->res_count, clone->res_count);
  clone->latency_to_map = BB_MAP_Create();
  clone->order = BB_MAP32_Create();
  Clone_Mappings(clone, se, pool);
  clone->latency_to_map_dirty = se->latency_to_map_dirty;
  clone->cached_crit_path_len = se->cached_crit_path_len;
  clone->sched_cycles = se->sched_cycles;
  clone->cached_resource_cycles = se->cached_resource_cycles;
  clone->use_dep_graph = se->use_dep_graph;
  return clone;
}

// A few inline routines to provide the illusion of a BB -> float mapping.

inline BB_MAP 
BB_MAPfloat_Create(void) { return BB_MAP32_Create(); }

inline float 
BB_MAPfloat_Get(BB_MAP map, BB *bb)
{
  INT32 entry = BB_MAP32_Get(map, bb);
  return *(float *)(&entry);
}

inline void 
BB_MAPfloat_Set(BB_MAP map, BB *bb, float val)
{
  BB_MAP32_Set(map, bb, *(INT32 *)(&val));
}

// =======================================================================
//  Checks to see if BB_SET <bbs> comprises a region.
// =======================================================================
static BOOL 
Is_Region(BB_SET *bbs, BB *entry, BOOL recursive)
{
  static BB_SET *visiting, *not_visited;
  static BB *orig_entry;
  BBLIST *succs;
  BOOL disqualified = FALSE;

  if (!recursive) {
    MEM_POOL_Push(&MEM_local_nz_pool);
    visiting = BB_SET_Create_Empty(PU_BB_Count+1, &MEM_local_nz_pool);
    not_visited = BB_SET_Copy(bbs, &MEM_local_nz_pool);
    orig_entry = entry;
    if (tracing()) {
      fprintf(TFile, "<sched_est> looking at BBs ");
      BB_SET_Print(bbs, TFile);
      fprintf(TFile, " entry BB:%d\n", BB_id(entry));
    }
  } else {
    BBLIST *preds;
    if (entry == orig_entry) return TRUE;
    FOR_ALL_BB_PREDS(entry, preds) {
      BB *pred = BBLIST_item(preds);
      if (!BB_SET_MemberP(bbs, pred)) {
	if (tracing())
	  fprintf(TFile, "<sched_est> not a region: "
		  "BB:%d has non-region pred BB:%d\n",
		  BB_id(entry), BB_id(pred));
	return FALSE;
      }
    }
    if (BB_SET_MemberP(visiting, entry)) {
      if (tracing())
	fprintf(TFile, "<sched_est> not a region: "
		"internal cycle to BB:%d\n", BB_id(entry));
      return FALSE;
    }
  }

  visiting = BB_SET_Union1D(visiting, entry, NULL);

  FOR_ALL_BB_SUCCS(entry, succs) {
    BB *succ = BBLIST_item(succs);
    if (BB_SET_MemberP(not_visited, succ) && !Is_Region(bbs, succ, TRUE)) {
      disqualified = TRUE;
      break;
    }
  }

  not_visited = BB_SET_Difference1D(not_visited, entry);
  visiting = BB_SET_Difference1D(visiting, entry);

  if (!recursive) {
    if (!disqualified && BB_SET_Size(not_visited) > 0) {
      if (tracing()) {
	fprintf(TFile, "<sched_est> not a region: has disconnected BBs ");
	BB_SET_Print(not_visited, TFile);
	fprintf(TFile, "\n");
      }
      disqualified = TRUE;
    }
    MEM_POOL_Pop(&MEM_local_nz_pool);
  }

  return !disqualified;
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
BOOL 
CG_SCHED_EST_Is_Region(BB_SET *region, BB *entry)
{
  return Is_Region(region, entry, FALSE);
}

// =======================================================================
// Requires: BB_SET_MemberP(region, entry)
//
// Return the average cost of a control-flow path from <entry> through
// <region>, given successor probabilities and BB_MAP <ests> which holds
// the CG_SCHED_EST for each BB in <region> that hasn't been merged with
// another one.
// =======================================================================
static float 
Avg_Cost_Path(BB_SET *region, BB *entry, BB_MAP ests, SCHED_EST_TYPE type)
{
  float avg_cost = 0.0;

  BB *cur_bb;
  FOR_ALL_BB_SET_members(region, cur_bb) {
    CG_SCHED_EST *se = (CG_SCHED_EST *)BB_MAP_Get(ests, cur_bb);
    float local_cost;

    // Adjust the bias-factor when estimating for SWP.
    if ((type & SCHED_EST_FOR_SWP) && Enable_SWP) {

      UINT32 res_cost = CG_SCHED_EST_Resource_Cycles(se);
      UINT32 critical_len = CG_SCHED_EST_Critical_Length(se);
      UINT32 diff = abs(critical_len - res_cost);
      UINT32 num_bbs = BB_SET_Size(region);
      
      // Adjust the bias factor, by calculating the difference (or disparity)
      // between the <res_cost> and <critical_len>, and bias the difference
      // by the number of blocks in the <region>. This skews the schedule
      // slightly and presents a more realistic schedule.
      float bias_factor= (num_bbs > 1) ? (num_bbs - 1.0)/(num_bbs + 7.0) : 0.0;
      local_cost = umax(res_cost, critical_len) - bias_factor * diff;

    } else {

      // For other cases, estimate the cycles as is.
      local_cost = (se) ? (float) CG_SCHED_EST_Cycles(se) : 0.0F;
    }

    // The probability is relative to the entry block.
    avg_cost += BB_freq(cur_bb)/BB_freq(entry) * local_cost;

  }
  if (tracing()) {
    fprintf(TFile, "<sched_est> avg_cost_path(BBs ");
    BB_SET_Print(region, TFile);
    fprintf(TFile, "entry BB:%d) = %g\n", BB_id(entry), avg_cost);
  }

  return avg_cost;
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
float 
CG_SCHED_EST_Avg_Cycles_Thru(BB_SET *region, BB *entry, BB_MAP ests,
			     SCHED_EST_TYPE type)
{
  return Avg_Cost_Path(region, entry, ests, type);
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
void 
CG_SCHED_EST_Ignore_Op(CG_SCHED_EST *se, OP *op)
{
  TOP opc = OP_code(op);
  if (tracing()) {
    fprintf(TFile, "<sched_est> ignoring OP:\n            ");
    Print_OP_No_SrcLine(op);
    fprintf(TFile, "            in ");
    CG_SCHED_EST_Print(TFile, se);
    fprintf(TFile, "\n");
  }
  if (OP_prefetch(op)) 
    TI_RES_COUNT_Subtract_Op_Resources(se->res_count, opc);
  Set_Latency_To(se, op, LATENCY_IGNORE);
  se->cached_resource_cycles = 0;
  se->cached_crit_path_len = 0;
  se->latency_to_map_dirty = TRUE;
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
UINT32 
CG_SCHED_EST_Resource_Cycles(CG_SCHED_EST *se)
{
  if (!se->cached_resource_cycles)
    se->cached_resource_cycles = Resource_Min_Cycles(se);
  return se->cached_resource_cycles;
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
UINT32 
CG_SCHED_EST_Critical_Length(CG_SCHED_EST *se)
{
  if (!se->cached_crit_path_len)
    se->cached_crit_path_len  = Critical_Path_Len(se);
  return se->cached_crit_path_len;
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
UINT32 
CG_SCHED_EST_Cycles(CG_SCHED_EST *se)
{
  if (CG_SCHED_EST_use_locs) {
    return se->sched_cycles;
  } else {
    if (!se->cached_resource_cycles)
      se->cached_resource_cycles = Resource_Min_Cycles(se);
    
    if (!se->cached_crit_path_len)
      se->cached_crit_path_len = Critical_Path_Len(se);
    
    return umax(se->cached_resource_cycles, se->cached_crit_path_len);
  }
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
UINT32 
CG_SCHED_EST_BB_Cycles(BB *bb, SCHED_EST_TYPE type)
{
  UINT32 cycles;
  CG_SCHED_EST *se;
   
  MEM_POOL_Push(&MEM_local_nz_pool);
  se = CG_SCHED_EST_Create(bb, &MEM_local_nz_pool, type);
  cycles = CG_SCHED_EST_Cycles(se);
  CG_SCHED_EST_Delete(se);
  MEM_POOL_Pop(&MEM_local_nz_pool);
  
  return cycles;
}

// =======================================================================
// Copy the mappings for the BBs in <from> to the mappings in <to>
// (note that the latency_to BB_OP_MAPs are not themselves copied).
// The new <order> mappings are incremented by the number of BBs in
// <to> to reflect insertion order.
// =======================================================================
static void 
Append_Mappings(CG_SCHED_EST *to, CG_SCHED_EST *from)
{
  BB *bb;
  INT32 num_bbs_before = BB_SET_Size(to->contents);
  FOR_ALL_BB_SET_members(from->contents, bb) {
    void *lt_map = BB_MAP_Get(from->latency_to_map, bb);
    INT32 order = BB_MAP32_Get(from->order, bb);
    DevAssert(lt_map, ("missing latency_to map for BB:%d", BB_id(bb)));
    DevAssert(order, ("missing order for BB:%d", BB_id(bb)));
    BB_MAP_Set(to->latency_to_map, bb, lt_map);
    BB_MAP32_Set(to->order, bb, order + num_bbs_before);
  }
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
void 
CG_SCHED_EST_Append_Scheds(CG_SCHED_EST *se, CG_SCHED_EST *other_se)
{
  /* TODO: This doesn't really do the right thing if the intersection
   *	   of se->contents and other_se->contents is non-empty.  The
   *	   resource count will be correct, but since we keep only a
   *	   single number (in se->order) to represent the position of
   *	   a given BB in the sequence, the critical path length computation
   *	   get very screwed up.  Also, the se->latency_to map will
   *	   need multiple entries per OP.
   */

  if (tracing()) {
    fprintf(TFile, "<sched_est> appending sched from ");
    CG_SCHED_EST_Print(TFile, other_se);
    fprintf(TFile, "\n            onto sched from ");
    CG_SCHED_EST_Print(TFile, se);
    fprintf(TFile, "\n");
  }
  
  if (!CG_SCHED_EST_use_locs) {
    TI_RES_COUNT_Add(se->res_count, se->res_count, other_se->res_count);
  } else {
    // TODO: currently, we just add the individual HB estimates. We don;t
    // take into account GCM effects. The current sched_est framework doesn't
    // support such mechanism at the moment. Need to revisit.
    se->sched_cycles = se->sched_cycles + other_se->sched_cycles;
  }
  
  Append_Mappings(se, other_se);
  se->contents = BB_SET_UnionD(se->contents, other_se->contents, NULL);
  se->use_dep_graph = se->use_dep_graph && other_se->use_dep_graph;
  se->latency_to_map_dirty = TRUE;
  se->cached_resource_cycles = 0;
  se->cached_crit_path_len = 0;
  if (tracing()) {
    fprintf(TFile, "            yielding ");
    CG_SCHED_EST_Print(TFile, se);
    fprintf(TFile, "\n");
  }
}

// =======================================================================
// See "cg_sched_est.h" for interface specification.
// =======================================================================
float 
CG_SCHED_EST_Region_Cycles(BB_SET *region, BB *entry, BOOL merged,
			   SCHED_EST_TYPE type)
{
  BB_MAP sched_ests = BB_MAP_Create();
  float avg_cycles;
  BB *bb;

  MEM_POOL_Push(&MEM_local_nz_pool);

  if (merged) {
    CG_SCHED_EST *merged_se = CG_SCHED_EST_Create(entry, &MEM_local_nz_pool,
						  SCHED_EST_FOR_IF_CONV);
    FOR_ALL_BB_SET_members(region, bb) {
      if (bb != entry) {
	CG_SCHED_EST *se;
	se = CG_SCHED_EST_Create(bb, &MEM_local_nz_pool,
				 SCHED_EST_FOR_IF_CONV);
	CG_SCHED_EST_Append_Scheds(merged_se, se);
	CG_SCHED_EST_Delete(se);
      }
    }
    avg_cycles = CG_SCHED_EST_Cycles(merged_se);
    CG_SCHED_EST_Delete(merged_se);
  } else {
    FOR_ALL_BB_SET_members(region, bb)
      BB_MAP_Set(sched_ests, bb,
		 CG_SCHED_EST_Create(bb, &MEM_local_nz_pool,
				     SCHED_EST_FOR_IF_CONV));
    avg_cycles = CG_SCHED_EST_Avg_Cycles_Thru(region, entry, sched_ests, type);
    FOR_ALL_BB_SET_members(region, bb)
      CG_SCHED_EST_Delete((CG_SCHED_EST *)BB_MAP_Get(sched_ests, bb));
  }

  MEM_POOL_Pop(&MEM_local_nz_pool);
  BB_MAP_Delete(sched_ests);

  return avg_cycles;
}






