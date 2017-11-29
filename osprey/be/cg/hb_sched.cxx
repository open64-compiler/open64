/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


// =======================================================================
// =======================================================================
//
//  Module: hb_sched.cxx
//  $Revision: 1.50 $
//  $Date: 05/12/05 08:59:07-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.hb_sched.cxx $
//
//  Description:
//  ============
//
//  Hyberblock (HB) Scheduling routines.
//
// =======================================================================
// =======================================================================

#include <alloca.h>
#include <math.h>
#include "defs.h"
#include "config.h"
#include "config_targ_opt.h"
#include "mempool.h"
#include "bb.h"
#include "bb_set.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "glob.h"
#include "tn_map.h"
#include "cg.h"
#include "cg_flags.h"
#include "ercg.h"
#include "cgtarget.h"
#include "cg_vector.h"
#include "dominate.h"
#include "findloops.h"
#include "note.h"
#include "lra.h"
#include "gcm.h"
#include "ti_res.h"
#include "ti_res_res.h"
#include "ti_latency.h"
#include "ti_errors.h"
#include "cg_region.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cxx_memory.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "targ_proc_properties.h"
#include "targ_isa_bundle.h"
#include "ti_bundle.h"
#include "whirl2ops.h"
#include "tag.h"
#ifdef KEY
#include <float.h>	// FLT_MAX
#include "opsch_set.h"
#endif

// ======================================================================
// Declarations (macros, variables)
// ======================================================================
#ifdef KEY
OPSCH_SET *fp_opschs = NULL;
static int OPSCH_count = 0;
static int unsched_prefetch_count = 0;
static int deleted_prefetch_count = 0;

OPSCH **OPSCH_Vec;	// Map OPSCH_id to OPSCH.
int OPSCH_Vec_Count;
#endif

BOOL Trace_HB = FALSE;

static INT BBs_Processed = 0;

// The current cycle in which we are trying to schedule OPs.
static INT Clock;
static INT MAX_Clock;

static void
Print_OPSCH (OP *op, BB_MAP value_map)
{
  OPSCH *opsch = OP_opsch(op, value_map);
  Print_OP_No_SrcLine (op);
  fprintf (TFile, "\t<dfs:%3d cyc:%2d reg:%2d est:%2d lst:%2d succs:%d preds:%d>\n", 
    OPSCH_dfsnum(opsch), OPSCH_scycle(opsch), OPSCH_regcost(opsch),
    OPSCH_estart(opsch), OPSCH_lstart(opsch), 
    OPSCH_num_succs(opsch), OPSCH_num_preds(opsch));

#ifdef KEY
  if (OPSCH_least_constrained_int(opsch) != NULL) {
    fprintf(TFile, " int:%d",
	    OPSCH_num_blockers(OPSCH_least_constrained_int(opsch)));
  }
  if (OPSCH_least_constrained_fp(opsch) != NULL) {
    fprintf(TFile, " fp:%d",
	    OPSCH_num_blockers(OPSCH_least_constrained_fp(opsch)));
  }
#endif
  fprintf (TFile, ">\n");
}

static void
Print_BB_For_HB (BB *bb, BB_MAP value_map)
{
  OP *op;

  fprintf (TFile, "*************** BB:%d ******************\n", BB_id(bb));
  FOR_ALL_BB_OPs_FWD (bb, op) {
    Print_OPSCH (op, value_map);
  }
  fprintf (TFile, "****************************************\n");
}

void
Print_BB_For_HB (std::list<BB*> bblist, BB_MAP value_map)
{
  std::list<BB*>::iterator bbiter;

  fprintf (TFile, "\n********** HyperBlock (HB) ******************\n");
  fprintf (TFile, "******* Contains :");
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbiter) {
    fprintf (TFile, " BB:%d ", BB_id(*bbiter));
  }
  fprintf (TFile, "**********\n");

  CG_DEP_Trace_HB_Graph (bblist);
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbiter) {
    Print_BB_For_HB(*bbiter, value_map);
  }
  fprintf (TFile, "****************************************\n");

}

// ======================================================================
// Check to see if there is a need to reschedule this block. Sometimes
// it's better to reschedule the block with different heuristics (#622253). 
// Currently, we limit it to single-BB loops where the benefits are more
// pronounced.
// ======================================================================
BOOL
Reschedule_BB(BB *bb)
{

  // At the moment, target single_BB loops ONLY.
  if (BB_loop_head_bb(bb) == bb) {
    BBLIST *succ_list;
    FOR_ALL_BB_SUCCS (bb, succ_list) {
      BB *succ_bb = BBLIST_item(succ_list);
      if (succ_bb == bb) return TRUE;
    }
  }

  return FALSE;
}

// ======================================================================
// Check to see if the given HB can be scheduled, i.e prior not SWP'd.
// ======================================================================
BOOL
Can_Schedule_HB(std::list<BB*> hb_blocks)
{

  std::list<BB*>::iterator bb_iter;
  FOR_ALL_BB_STLLIST_ITEMS_FWD (hb_blocks, bb_iter) {
    // if <reschedule> flag is already set, then return FALSE.
    if (BB_scheduled(*bb_iter) && !BB_scheduled_hbs(*bb_iter)) return FALSE;
  }

  return TRUE;
}

INT
Memory_OP_Base_Opndnum (OP *op)
{
#if defined(TARG_X8664) || defined(TARG_LOONGSON)  
  return TOP_Find_Operand_Use( OP_code(op), OU_base );
#else
  INT opnd_num;
  if (OP_store(op) || OP_prefetch(op)) {
    opnd_num = 1;
  }
  else {
    Is_True (OP_load(op), ("OP not a memory OP."));
    opnd_num = 0;
  }
  return opnd_num;
#endif
}


INT
Memory_OP_Offset_Opndnum (OP *op)
{
#if defined(TARG_X8664) || defined(TARG_LOONGSON)  
  return TOP_Find_Operand_Use( OP_code(op), OU_offset );
#else
  INT opnd_num;

  if (OP_store(op) || OP_prefetch(op)) {
    opnd_num = 2;
  }
  else {
    Is_True (OP_load(op), ("OP not a memory OP."));
    opnd_num = 1;
  }
  return opnd_num;
#endif
}

// ======================================================================
// Initialize <regs_map> for the basic block. The defs for each TN
// are counted. Also mark all global TNs as having a register 
// already assigned to them.
// ======================================================================
void HB_Schedule::Init_Register_Map (BB *bb)
{
  OP *op;

  _regs_map = hTN_MAP_Create (&_hb_pool);
  FOR_ALL_BB_OPs_FWD (bb, op) {
    INT i;
    for (i = 0; i < OP_results(op); i++) {
      TN *result_tn = OP_result(op, i);
      REG_ENTRY reginfo;
      REG_ENTRY_ptr(reginfo) =  hTN_MAP_Get (_regs_map, result_tn);
      REG_ENTRY_def_count(reginfo)++;
      if (TN_is_global_reg(result_tn) || TN_is_dedicated(result_tn)) 
	REG_ENTRY_reg_assigned(reginfo) = TRUE;
      hTN_MAP_Set (_regs_map, result_tn, REG_ENTRY_ptr(reginfo));
    }
    for (i = 0; i < OP_opnds(op); i++) {
      TN *opnd_tn = OP_opnd(op,i);
      if (TN_is_constant(opnd_tn)) continue;
      if (TN_is_global_reg(opnd_tn) || TN_is_dedicated(opnd_tn)) {
        REG_ENTRY reginfo;
        REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, opnd_tn);
        REG_ENTRY_reg_assigned(reginfo) = TRUE;
        hTN_MAP_Set (_regs_map, opnd_tn, REG_ENTRY_ptr(reginfo));
      }
    }
  }
}

// ======================================================================
// Estimate the register cost for scheduling <op> next. This cost depends
// on the following factors:
//  - how many registers are available for allocation at this point ?
//  - how many registers are used and freed up by <op> ?
//  - is the number of registers available below a threshold ?
// ======================================================================
void
HB_Schedule::Estimate_Reg_Cost_For_OP (OP *op)
{
  INT cost = 0;
  INT32 local_regs_avail[ISA_REGISTER_CLASS_MAX+1];
  ISA_REGISTER_CLASS cl;
  REG_ENTRY reginfo;

  FOR_ALL_ISA_REGISTER_CLASS(cl) {
    local_regs_avail[cl] = _Cur_Regs_Avail[cl];
  }

  INT i;
  for (i = 0; i < OP_results(op); i++) {
    TN *result_tn = OP_result(op, i);
    // If the result tn is also referenced in the OP, don't consider the 
    // register being freed above the def.
    if (!OP_Refs_TN (op, result_tn)) {
      REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, result_tn);
      if (REG_ENTRY_def_count(reginfo) == 1 &&
	  REG_ENTRY_reg_assigned(reginfo)) 
      {
        cl = TN_register_class(result_tn);
	INT reg_pressure = (2 - local_regs_avail[cl]);
	if (reg_pressure > 0) {
	  cost -= reg_pressure;
	}
	local_regs_avail[cl]++;
      }
    }
  }
  for (i = 0; i < OP_opnds(op); i++) {
    TN *opnd_tn = OP_opnd(op,i);
    if (TN_is_constant(opnd_tn)) continue;
    REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, opnd_tn);
    if (!REG_ENTRY_reg_assigned(reginfo)) {
      // check for an earlier occurence of the opnd_tn.
      BOOL reg_handled = FALSE;
      for (INT j = 0; j < i; j++) {
	if (OP_opnd(op,j) == opnd_tn)
	  reg_handled = TRUE;
      }
      if (!reg_handled) {
        cl = TN_register_class(opnd_tn);
        local_regs_avail[cl]--;
        INT reg_pressure = (2 - local_regs_avail[cl]);
        if (reg_pressure > 0) {
          cost += reg_pressure;
        }
      }
    }
  }   
  OPSCH *opsch = OP_opsch(op, _hb_map);
  OPSCH_regcost(opsch) = cost;
}

// ======================================================================
// Update the number of registers available for allocation after <op> is
// scheduled.
// ======================================================================
void
HB_Schedule::Update_Regs_For_OP (OP *op)
{
  REG_ENTRY reginfo;

  INT i;
  for (i = 0; i < OP_results(op); i++) {
    TN *result_tn = OP_result(op, i);
    REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, result_tn);
    REG_ENTRY_def_count(reginfo)--;
    if (REG_ENTRY_def_count(reginfo) == 0 &&
	REG_ENTRY_reg_assigned(reginfo)) 
    {
      ISA_REGISTER_CLASS cl = TN_register_class(result_tn);
      _Cur_Regs_Avail[cl]++;
      REG_ENTRY_reg_assigned(reginfo) = FALSE;
    }
    hTN_MAP_Set (_regs_map, result_tn, REG_ENTRY_ptr(reginfo));
  }
  for (i = 0; i < OP_opnds(op); i++) {
    TN *opnd_tn = OP_opnd(op,i);
    if (TN_is_constant(opnd_tn)) continue;
    REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, opnd_tn);
    if (!REG_ENTRY_reg_assigned(reginfo)) {
      ISA_REGISTER_CLASS cl = TN_register_class(opnd_tn);
      _Cur_Regs_Avail[cl]--;
#ifdef TARG_X8664
      // mark any excess register pressure by class
      if (HBS_Before_GRA() && HBS_Before_LRA()) {
        if (_Cur_Regs_Avail[cl] == 1) {
          if (BB_regpressure(op->bb,cl) == false)
            Set_BB_regpressure(op->bb, true, cl);
        }
      }
#endif
      REG_ENTRY_reg_assigned(reginfo) = TRUE;
      hTN_MAP_Set (_regs_map, opnd_tn, REG_ENTRY_ptr(reginfo));
    }
  }
}

// ======================================================================
// Return TRUE if <op1> and <op2> are addiu and load/store instructions
// such that the addiu and the load/store can be interchanged.
// ======================================================================
BOOL
Is_Ldst_Addiu_Pair (OPSCH *opsch1, OPSCH *opsch2, OP *op1,OP *op2)
{
  OP *addiu_op;
  OP *ldst_op;
  INT64 multiplier;

  if (((OPSCH_flags(opsch1) | OPSCH_flags(opsch2)) & OPSCH_ADDIU_LDST_PAIR) !=
      OPSCH_ADDIU_LDST_PAIR) 
  {
    return FALSE;
  }

  if (OPSCH_addiu(opsch1)) {
    addiu_op = op1;
    ldst_op = op2;
    multiplier = 1;
  }
  else {
    addiu_op = op2;
    ldst_op = op1;
    multiplier = -1;
  }

  // Check that the result of the addiu is the same as the base of the ldst.
  // Also check that if the memory OP is a store, the source is not the same
  // as the result of the addiu.
  INT base_opndnum = Memory_OP_Base_Opndnum(ldst_op);
#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_LOONGSON)
  if( base_opndnum < 0 ){
    return FALSE;
  }
#endif
  if (OP_result(addiu_op,0 /*???*/) != OP_opnd(ldst_op,base_opndnum) ||
      (OP_store(ldst_op) &&
#ifdef TARG_LOONGSON
       OP_result(addiu_op,0 /*???*/) == OP_opnd(ldst_op,1)
#else
       OP_result(addiu_op,0 /*???*/) == OP_opnd(ldst_op,0)
#endif
       ))
  {
    return FALSE;
  }

#ifdef TARG_LOONGSON
  INT64 addiu_const = TN_value (OP_opnd(addiu_op, 2));
#else
  INT64 addiu_const = TN_value (OP_opnd(addiu_op, 1));
#endif
  INT offset_opndnum = Memory_OP_Offset_Opndnum(ldst_op);
#if defined(TARG_SL)
  if( offset_opndnum < 0 )
    return FALSE;
#endif
  INT64 ldst_const = TN_value (OP_opnd(ldst_op, offset_opndnum));

  return TOP_Can_Have_Immediate (ldst_const + addiu_const*multiplier, OP_code(ldst_op));
}

// ======================================================================
// Change the offset field in load/store OP after it has been moved across
// an addiu OP that defines the base register for the <ldst_op>. The 
// <multiplier> can be either +1 or -1 to indicate direction of movement.
// ======================================================================
void
Fixup_Ldst_Offset (OP *ldst_op, INT64 addiu_const, INT64 multiplier, 
		   HBS_TYPE type)
{
  TN *old_ofst_tn, *ofst_tn;
  INT index;

  index = Memory_OP_Offset_Opndnum (ldst_op);
  if( index < 0 )
    return;
  old_ofst_tn = OP_opnd(ldst_op, index);

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "old: %lld, new: %lld\n", TN_value(old_ofst_tn), 
		      TN_value(old_ofst_tn) + addiu_const * multiplier);
    fprintf (TFile, "offset changed:");
    Print_OP_No_SrcLine (ldst_op);
  }

  ofst_tn = Gen_Literal_TN (TN_value(old_ofst_tn) + addiu_const * multiplier,
			    TN_size(old_ofst_tn));
  Set_OP_opnd (ldst_op, index, ofst_tn);

}

// ======================================================================
// Traverse through the list of scheduled instructions and look for load
// or store OPs that have been moved across corresponding addiu OPs. For
// all such load/store OPs, adjust their offset field.
// ======================================================================
void
HB_Schedule::Adjust_Ldst_Offsets (void)
{
  for (INT i = VECTOR_count(_sched_vector)-1; i >= 0; i--) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    Set_OPSCH_visited (opsch);
    if (!OPSCH_addiu (opsch)) continue;
#ifdef TARG_LOONGSON
    INT64 addiu_const = TN_value (OP_opnd(op,2));
#else
    INT64 addiu_const = TN_value (OP_opnd(op,1));
#endif
    ARC_LIST *arcs;
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
#ifdef KEY
      if( ARC_kind(arc) != CG_DEP_REGIN ){
	continue;
      }
#endif
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch (succ_op, _hb_map);
      if (OPSCH_ldst (succ_opsch) && OPSCH_visited (succ_opsch)) {
	Fixup_Ldst_Offset (succ_op, addiu_const, +1, type());
      }
    }
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
#ifdef KEY
      if (ARC_kind(arc) != CG_DEP_REGANTI){
	// We only care about any REGANTI really.
	// And there may be multiple arcs between two nodes.
	// In that case, the following will update the offset many times.
	// To avoid such cases, we will skip arcs of kind CG_DEP_MISC.
	continue;
      }
#endif /* KEY */
      OPSCH *pred_opsch = OP_opsch (pred_op, _hb_map);
      if (OPSCH_ldst (pred_opsch) && !OPSCH_visited (pred_opsch)) {
	Fixup_Ldst_Offset (pred_op, addiu_const, -1, type());
      }
    }
  }
}

#ifdef KEY

void HB_Schedule::Adjust_Ldst_Offsets( BOOL is_fwd )
{
  for( INT i = is_fwd ? 0 : VECTOR_count(_sched_vector) - 1; 
       is_fwd ? i < VECTOR_count(_sched_vector) : i >= 0; 
       is_fwd ? i++ : i-- ){

    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    Set_OPSCH_visited (opsch);
    if (!OPSCH_addiu (opsch)) continue;
#ifdef TARG_LOONGSON
    INT64 addiu_const = TN_value (OP_opnd(op,2));
#else
    INT64 addiu_const = TN_value (OP_opnd(op,1));
#endif
    ARC_LIST *arcs;

    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      if( ARC_kind(arc) != CG_DEP_REGIN ){
	continue;
      }

      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch (succ_op, _hb_map);
      if (OPSCH_ldst (succ_opsch) && OPSCH_visited (succ_opsch)) {
	Fixup_Ldst_Offset (succ_op, addiu_const, +1, type());
      }
    }

    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);

      if (ARC_kind(arc) != CG_DEP_REGANTI){
	// We only care about any REGANTI really.
	// And there may be multiple arcs between two nodes.
	// In that case, the following will update the offset many times.
	// To avoid such cases, we will skip arcs of kind CG_DEP_MISC.
	continue;
      }

      OPSCH *pred_opsch = OP_opsch (pred_op, _hb_map);
      if (OPSCH_ldst (pred_opsch) && !OPSCH_visited (pred_opsch)) {
	Fixup_Ldst_Offset (pred_op, addiu_const, -1, type());
      }
    }
  }
}

#endif

// ======================================================================
// Set_Resource_Usage
//
// Given an 'op' and a start 'cycle', reserve all the resources needed
// to schedule the op at that cycle. It is assumed that we have already
// verified that the resources are available.
// ======================================================================
void
HB_Schedule::Set_Resource_Usage (OP *op)
{
  INT cycle = OPSCH_scycle (OP_opsch(op, _hb_map));

  TI_RES_RES_Reserve_Resources(_rr_tab, OP_code(op), cycle);
  Clock = cycle;
}

// ======================================================================
// Find_Schedule_Cycle
//
// Find the cycle in which 'op' can be scheduled. Also, update the scycle
// for the OP so that future searches are faster.
// ======================================================================
INT
HB_Schedule::Find_Schedule_Cycle (OP *op, BOOL is_fwd)
{
  OPSCH *opsch = OP_opsch(op, _hb_map);
  INT cycle = OPSCH_scycle(opsch);
  INT cyc;

  if (is_fwd) {
    // keep looking forward till we can schedule the op.
    for (cyc = cycle; cyc <= MAX_Clock; cyc++) {
      if (Check_Resource_Usage (op, cyc)) break;
    }
    FmtAssert (cyc <= MAX_Clock, ("HB_SCHED: no valid cycle for scheduling"));
  } else {
    // Keep looking back till we can schedule the op.
    for (cyc = cycle; cyc >= 0; cyc--) {
      if (Check_Resource_Usage (op, cyc)) break;
    }
    FmtAssert (cyc >= 0, ("HB_SCHED: no valid cycle for scheduling"));
  }

  // update the scycle for the OP.
  OPSCH_scycle(opsch) = cyc;
  return cyc;
}

static INT cur_dfsnum;

// ======================================================================
// Initialize the OPSCH data structure for each OP in <bb>. Identify
// OPs with the OPSCH_addiu, OPSCH_ldst, OPSCH_def_xfer_opnd attributes.
// ======================================================================
static void
Init_OPSCH_For_BB (BB *bb, BB_MAP value_map,  BOOL compute_bitsets,
		   MEM_POOL *pool)
{
  OP *op;
  ARC_LIST *arcs;
  ARC *arc;
  OPSCH *opsch;

#ifdef KEY
  OPSCH_count = 0;
  OPSCH_Vec_Count = BB_length(bb);
  OPSCH_Vec = TYPE_MEM_POOL_ALLOC_N(OPSCH *, pool, OPSCH_Vec_Count + 1);
  fp_opschs = compute_bitsets ?
	        OPSCH_SET_Create_Empty(BB_length(bb), pool) : NULL;
  unsched_prefetch_count = 0;
  deleted_prefetch_count = 0;
#endif

  FOR_ALL_BB_OPs_FWD (bb, op) {
    opsch = TYPE_MEM_POOL_ALLOC (OPSCH, pool);
    BZERO (opsch, sizeof (OPSCH));
    OPSCH_lstart(opsch) = 0x7fff;
    BB_OP_MAP bb_map = (BB_OP_MAP) BB_MAP_Get(value_map, bb);
    BB_OP_MAP_Set (bb_map, op, opsch);
#ifdef KEY
    OPSCH_op(opsch) = op;
    OPSCH_id(opsch) = ++OPSCH_count;
    OPSCH_Vec[OPSCH_id(opsch)] = opsch;
    if (compute_bitsets) {
      OPSCH_ancestors(opsch) = OPSCH_SET_Create_Empty(BB_length(bb), pool);
      OPSCH_descendants(opsch) = OPSCH_SET_Create_Empty(BB_length(bb), pool);
      if (OP_flop(op))
	fp_opschs = OPSCH_SET_Union1D(fp_opschs, opsch, pool);
    }

    // Count number of prefetches.
    if (OP_prefetch(op))
      unsched_prefetch_count++;
#endif
  }

  FOR_ALL_BB_OPs_FWD (bb, op) {
    opsch = OP_opsch(op, value_map);
    // Identify LDST/ADDIU instructions with non-relocatable offsets.
    if (CGTARG_Is_OP_Addr_Incr(op) && 
	!TN_is_sp_reg(OP_result(op,0 /*???*/))) {
	
      BOOL addiu_ok = TRUE;
      for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	arc = ARC_LIST_first(arcs);
	if (ARC_kind(arc) == CG_DEP_REGOUT) {
	  addiu_ok = FALSE;
	  break;
	}
      }
      for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	arc = ARC_LIST_first(arcs);
	if (ARC_kind(arc) == CG_DEP_REGIN) {
	  addiu_ok = FALSE;
	  break;
	}
      }
      if (addiu_ok) Set_OPSCH_addiu (opsch);
    }
    else if (OP_memory(op)) {
      // check if the memory OP has an offset field (i.e. it is not an 
      // indexed load/store/prefx.
      INT offset_opndnum = Memory_OP_Offset_Opndnum (op);
      if ( offset_opndnum >= 0 &&
           TN_has_value(OP_opnd(op,offset_opndnum)) ) {
	Set_OPSCH_ldst (opsch);
      }
    }
#ifdef TARG_MIPS
    if (Is_Target_T5() && OP_xfer(op) && Get_Trace (TP_SCHED, 0x1000)) {
      for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
        arc = ARC_LIST_first(arcs);
	if (ARC_kind(arc) == CG_DEP_REGIN) {
	  OP *pred_op = ARC_pred(arc);
	  OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
	  Set_OPSCH_def_xfer_opnd(pred_opsch);
	}
      }
    }
#endif
  }
}

// ======================================================================
// return TRUE if opsch1 has a larger 'estart' value than opsch2.
// ======================================================================
static BOOL
sort_by_estart (const void *opsch1, const void *opsch2)
{
  return (OPSCH_estart((OPSCH*) opsch1) > OPSCH_estart((OPSCH*) opsch2));
}
 
// ======================================================================
// return TRUE if opsch1 has a smaller 'slack' value than opsch2.
// ======================================================================
static BOOL
sort_by_slack (const void *opsch1, const void *opsch2)
{
  INT slack1 = OPSCH_lstart((OPSCH*) opsch1) - OPSCH_estart((OPSCH*) opsch1);
  INT slack2 = OPSCH_lstart((OPSCH*) opsch2) - OPSCH_estart((OPSCH*) opsch2);
 
  return ((OPSCH*)(INTPTR) slack1 < (OPSCH*)(INTPTR) slack2);
}
 
#ifdef KEY
// ======================================================================
// Find the depth of <op> in the dependence graph and mark it in the
// OPSCH_depth field.
// ======================================================================
static void
Compute_Depth (OP *op, BB_MAP value_map)
{
  OPSCH *opsch = OP_opsch(op, value_map);
  ARC_LIST *arcs;
  INT max_depth = 0;

  Is_True(OPSCH_depth(opsch) <= BB_length(OP_bb(op)),
	  ("Compute_Depth: illegal OP depth value"));

  if (OPSCH_depth(opsch) > 0)	// Depth already assigned.
    return;

  // Visit OP only if all of OP's predeceesors have been visited.
  for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    OP *pred_op = ARC_pred(arc);
    OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
    INT pred_depth = OPSCH_depth(pred_opsch);
    if (pred_depth == 0) {	// Depth 0 means not yet visited.
      return;
    } else {
      max_depth = (pred_depth > max_depth) ? pred_depth : max_depth;
    }
  }

  OPSCH_depth(opsch) = max_depth + 1;

  // Visit successors.
  for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    OP *succ_op = ARC_succ(arc);
    Compute_Depth(succ_op, value_map);
  }
}

// ======================================================================
// qsort comparison function for sorting predecessors.  The sort key is the
// depth of the predecessor in the dependence graph.  We sort in ascending
// order, i.e., lower depths come first.
// ======================================================================
static INT
Compare_Depths (const void *p1, const void *p2)
{
  enum {
    sort_1_before_2 = -1,
    sort_1_after_2  = 1,
    sort_1_same_2   = 0
  };

  INT depth1 = OPSCH_depth(*(OPSCH **)p1);
  INT depth2 = OPSCH_depth(*(OPSCH **)p2);
  Is_True(depth1 > 0 && depth2 > 0, ("Compare_Depths: OP has depth 0"));
  if (depth1 < depth2)
    return sort_1_before_2;
  else if (depth1 > depth2)
    return sort_1_after_2;
  else
    return sort_1_same_2;
}

// ======================================================================
// BLOCKER is an immediate pred/succ of BLOCKEE under forward/backward
// scheduling.  Propagate the least constrained (least blocked) node info
// from BLOCKEE to BLOCKER.
// ======================================================================
static void
Update_Blocker_Info (OPSCH *blocker, OPSCH *blockee)
{
  // Test against blockee.
  if (OP_flop(OPSCH_op(blockee))) {			// fp
    if (OPSCH_least_constrained_fp(blocker) == NULL ||
	(OPSCH_num_blockers(OPSCH_least_constrained_fp(blocker)) >
	 OPSCH_num_blockers(blockee)))
      OPSCH_least_constrained_fp(blocker) = blockee;
  } else {						// int
    if (OPSCH_least_constrained_int(blocker) == NULL ||
	(OPSCH_num_blockers(OPSCH_least_constrained_int(blocker)) >
	 OPSCH_num_blockers(blockee)))
      OPSCH_least_constrained_int(blocker) = blockee;
  }

  // Test against blockee's least constrained.
  if (OPSCH_least_constrained_fp(blocker) == NULL ||	// fp
      (OPSCH_least_constrained_fp(blockee) != NULL &&
       OPSCH_num_blockers(OPSCH_least_constrained_fp(blocker)) >
	 OPSCH_num_blockers(OPSCH_least_constrained_fp(blockee)))) {
    OPSCH_least_constrained_fp(blocker) = OPSCH_least_constrained_fp(blockee);
  }
  if (OPSCH_least_constrained_int(blocker) == NULL ||	// int
      (OPSCH_least_constrained_int(blockee) != NULL &&
       OPSCH_num_blockers(OPSCH_least_constrained_int(blocker)) >
	 OPSCH_num_blockers(OPSCH_least_constrained_int(blockee)))) {
    OPSCH_least_constrained_int(blocker) = OPSCH_least_constrained_int(blockee);
  }
}
#endif

// ======================================================================
// Recursive depth first search of the dep-graph starting at <op>. The 
// OPSCH_dfsnum field is marked as each OP is visited.
// ======================================================================
static void
DFS_Search (OP *op, BB_MAP value_map, BOOL is_fwd)
{
  OPSCH *opsch = OP_opsch (op, value_map);
  ARC_LIST *arcs;

  if (OPSCH_visited(opsch)) return;

#ifdef KEY
  OPSCH *least_constrained_int = NULL;
  OPSCH *least_constrained_fp = NULL;
#endif

  OPSCH_dfsnum(opsch) = cur_dfsnum;
  Set_OPSCH_visited(opsch);
  cur_dfsnum++;

  if (is_fwd) {
    // visit all the successors.
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch (succ_op, value_map);
      if (!OPSCH_visited(succ_opsch)) 
	DFS_Search (succ_op, value_map, TRUE);
#ifdef KEY
      Update_Blocker_Info(opsch, succ_opsch);
#endif
    }
  } else {
    // visit all the predecessors.
#ifdef KEY
    // Heuristic for DFS ordering.  When picking a child to continue the
    // depth-first traversal (in the reverse direction), choose a child with
    // the lowest depth in the dependence graph.  The hope is that this child's
    // sub-DAG would be the least complex and therefore have the least register
    // requirement.  Scheduling this child first (lower in the BB) would tie up
    // the fewest number of temporary registers.  SiCortex 4372.  PathScale
    // 14179.

    if (LOCS_Shallow_Depth) {
      int i = 0;
      OPSCH **opsch_list = (OPSCH **) alloca(OPSCH_num_preds(opsch) *
					     sizeof(OPSCH *));
      for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	OP *pred_op = ARC_pred(arc);
	OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
	// OPSCH_num_preds doesn't include ldst-addiu arcs.
	if (!Is_Ldst_Addiu_Pair(pred_opsch, opsch, pred_op, op))
	  opsch_list[i++] = pred_opsch;
      }
      Is_True(i == OPSCH_num_preds(opsch),
	      ("DFS_Search: incorrect number of preds"));

      qsort(opsch_list, OPSCH_num_preds(opsch), sizeof(OPSCH *),
	    Compare_Depths);

      for (i = 0; i < OPSCH_num_preds(opsch); i++) {
	OPSCH *pred_opsch = opsch_list[i];
	OP *pred_op = OPSCH_op(pred_opsch);
	if (!Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op)) {
	  if (!OPSCH_visited(pred_opsch))
	    DFS_Search(pred_op, value_map, FALSE);
#ifdef KEY
	  Update_Blocker_Info(opsch, pred_opsch);
#endif
	}
      }
    } else
#endif
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch (pred_op, value_map);
      if (!Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op)) {
	if (!OPSCH_visited(pred_opsch)) 
	  DFS_Search (pred_op, value_map, FALSE);
#ifdef KEY
	Update_Blocker_Info(opsch, pred_opsch);
#endif
      }
    }
  }
}

// ======================================================================
// Compute depth first ordering.
// ======================================================================
static void
Compute_DFO (HB_Schedule *sched, BB_MAP value_map, BOOL is_fwd)
{
  INT i;

#ifdef KEY
  OP *op;
  BB *bb = OP_bb(OP_VECTOR_element(sched->ready_vector(), 0));
  FOR_ALL_BB_OPs_FWD(bb, op) {
    if (OP_preds(op) == NULL)
      Compute_Depth(op, value_map);
  }
#endif

  cur_dfsnum = 1;
  for (i = 0; i < VECTOR_count(sched->ready_vector()); i++) {
    DFS_Search (OP_VECTOR_element(sched->ready_vector(), i), value_map,is_fwd);
  }
}

// ======================================================================
// Traverse the OPs in the basic block backwards and build a list of OPs
// that are ready to schedule. 
// Sort the ready vector in decreasing order or 'estart'.
// ======================================================================
void
Priority_Selector::Add_Element_Sorted (VECTOR vector, void *element, VECTOR_ELEMENT_COMPARE comp_func)
{
  INT i;
  INT count = VECTOR_count(vector);
  FmtAssert (count < VECTOR_size(vector), ("VECTOR overflow"));
  for (i = count; i > 0; i--) {
    void *cur_element = VECTOR_element(vector, i - 1);
    void *cur_opsch = OP_opsch((OP*) cur_element, _cur_sched->hb_map());
    void *opsch = OP_opsch((OP*) element, _cur_sched->hb_map());
    if (comp_func(cur_opsch, opsch)) break;
    VECTOR_element(vector, i) = cur_element;
  }
  VECTOR_element(vector, i) = element;
  count++;
  VECTOR_count(vector) = count;
}

// ======================================================================
// Traverse the OPs in the basic block backwards and build a list of OPs
// that are ready to schedule. 
// Sort the ready vector in decreasing order or 'estart'.
// ======================================================================
void
Priority_Selector::Build_Ready_Vector (BB* bb, BOOL is_fwd)
{
  OP *op;

  if (is_fwd) {
    FOR_ALL_BB_OPs_FWD (bb, op) {
      OPSCH *opsch = OP_opsch(op, _cur_sched->hb_map());
      // Add it to the ready vector if there are no successors.
      if (OPSCH_num_preds(opsch) == 0) {
	Add_Element_Sorted (_cur_sched->ready_vector(), op, sort_by_slack);
      }
    }
  } else {
    FOR_ALL_BB_OPs_REV (bb, op) {
      OPSCH *opsch = OP_opsch(op, _cur_sched->hb_map());
      // Add it to the ready vector if there are no successors.
      if (OPSCH_num_succs(opsch) == 0) {
	Add_Element_Sorted (_cur_sched->ready_vector(), op, sort_by_estart);
      }
    }
  }
}

// ======================================================================
// Traverse the OPs in the basic block backwards and build a list of OPs
// that are ready to schedule. 
// Sort the ready vector in decreasing order or 'estart'.
// ======================================================================
void
Priority_Selector::Build_Ready_Vector (std::list<BB*> bblist, BOOL is_fwd)
{

  std::list<BB*>::iterator bb_iter;
  FOR_ALL_BB_STLLIST_ITEMS_FWD (bblist, bb_iter) {
    Build_Ready_Vector (*bb_iter, is_fwd);
  }
}

// ======================================================================
// Return the maximum number of cycles in which a resource is used for
// the OP. This is used to estimate the size of the resource table
// required. It could also be used as a coarse estimate of the latency
// of an OP that has no successors.
// ======================================================================
inline INT
Resource_Cycles_For_OP (OP *op)
{
  return TI_RES_Cycle_Count(OP_code(op));
}

// ======================================================================
// Calculate_Adjust_Latency
// Placeholder to make all latency adjustments (both static/OOO effects).
// ======================================================================
static inline INT
Calculate_Adjust_Latency (ARC *arc)
{
  INT adjust_latency;
  // OOO adjustments necessary (or not)
  BOOL ooo_adjust = (PROC_is_out_of_order() && CG_DEP_Adjust_OOO_Latency);

  // For OOO machine (eg. T5), non-definite memory dependences can be 
  // relaxed to edges with zero latency. The belief is that this can 
  // help avoid creating false dependences with biased critical info.

  adjust_latency = (ooo_adjust && ARC_is_mem(arc) &&
		    !ARC_is_definite(arc)) ? 0 : ARC_latency(arc);

  // Similary, anti-dependences (reg) can be relaxed as well. Since, OOO
  // machines do dynamic renaming, having static estimates due to such
  // dependences can be avoided.
  adjust_latency = (ooo_adjust && ARC_is_reg(arc) &&
		    (ARC_is_anti(arc) || ARC_is_output(arc))) ?
		    0 : adjust_latency;

  return adjust_latency;
}

// ======================================================================
// Do a forward pass and compute the OPSCH data structure for the bb.
// ======================================================================
static void
Compute_Fwd_OPSCH (BB *bb, BB_MAP value_map, INT *max_lstart,
		   BOOL compute_bitsets, MEM_POOL *pool)
{
  OP *op;

  // Initialize the OPSCH_estart and OPSCH_num_succs fields of all OPs.
  // Also compute the max_lstart.
#ifdef KEY
  // Compute OPSCH_ancestors.
#endif
  FOR_ALL_BB_OPs_FWD (bb, op) {
    OPSCH *opsch = OP_opsch(op, value_map);
    INT op_estart = OPSCH_estart(opsch);
    ARC_LIST *arcs;
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch(succ_op, value_map);
      if (!Is_Ldst_Addiu_Pair (opsch, succ_opsch, op, succ_op)) {
        INT cur_estart = Calculate_Adjust_Latency(arc) + op_estart;
        if (OPSCH_estart(succ_opsch) < cur_estart) {
          OPSCH_estart(succ_opsch) = cur_estart;
        }
        OPSCH_num_succs(opsch)++;
#ifdef KEY
	// Compute OPSCH_ancestors.
	if (compute_bitsets) {
	  OPSCH_ancestors(succ_opsch) =
	    OPSCH_SET_UnionD(OPSCH_ancestors(succ_opsch),
			     OPSCH_ancestors(opsch), pool);
	  OPSCH_ancestors(succ_opsch) =
	    OPSCH_SET_Union1D(OPSCH_ancestors(succ_opsch), opsch, pool);
	}
#endif
      }
    }
    *max_lstart = MAX (*max_lstart, op_estart);
  }
}

// ======================================================================
// Do a backward pass and compute the OPSCH data structure for the bb.
// ======================================================================
static void
Compute_Bkwd_OPSCH (BB *bb, BB_MAP value_map, INT max_lstart,
		    BOOL compute_bitsets, MEM_POOL *pool)
{
  OP *op;

  // Initialize the OPSCH_scycle, OPSCH_lstart and OPSCH_num_preds fields of
  // all OPs.
#ifdef KEY
  // Compute OPSCH_descendants.
#endif
  FOR_ALL_BB_OPs_REV (bb, op) {
    OPSCH *opsch = OP_opsch(op, value_map);
    ARC_LIST *arcs;
    OPSCH_scycle(opsch) = Clock;
    if (OPSCH_lstart(opsch) > max_lstart) OPSCH_lstart(opsch) = max_lstart;
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
      if (!Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op)) {
        INT cur_lstart = OPSCH_lstart(opsch) - Calculate_Adjust_Latency(arc);
        if (OPSCH_lstart(pred_opsch) > cur_lstart) {
          OPSCH_lstart(pred_opsch) = cur_lstart;
        }
        OPSCH_num_preds(opsch)++;
#ifdef KEY
	if (compute_bitsets) {
	  // Compute OPSCH_descendants.
	  OPSCH_descendants(pred_opsch) =
	    OPSCH_SET_UnionD(OPSCH_descendants(pred_opsch),
			     OPSCH_descendants(opsch), pool);
	  OPSCH_descendants(pred_opsch) =
	    OPSCH_SET_Union1D(OPSCH_descendants(pred_opsch), opsch, pool);
	}
#endif
      }
    }
  }
}

#ifdef KEY
// ======================================================================
// Compute the number of nodes blocking each node.
// ======================================================================
static void
Compute_OPSCH_Blockers (BB *bb, BB_MAP value_map, BOOL is_fwd, MEM_POOL *pool)
{
  OP *op;

  FOR_ALL_BB_OPs_FWD (bb, op) {
    OPSCH *opsch = OP_opsch(op, value_map);
    if (is_fwd) {
      OPSCH_num_blockers(opsch) = OPSCH_SET_Size(OPSCH_ancestors(opsch)); 
    } else {
      OPSCH_num_blockers(opsch) = OPSCH_SET_Size(OPSCH_descendants(opsch)); 
    }
  }
}
#endif

// ======================================================================
// Given a <bb>, build the OPSCH data structure for it.
// ======================================================================
void
Compute_OPSCH (BB *bb, BB_MAP value_map, MEM_POOL *pool, BOOL compute_bitsets,
	       BOOL is_fwd)
{
  INT max_lstart = 0;

  Init_OPSCH_For_BB (bb, value_map, compute_bitsets, pool);

  Compute_Fwd_OPSCH (bb, value_map, &max_lstart, compute_bitsets, pool);

  Compute_Bkwd_OPSCH (bb, value_map, max_lstart, compute_bitsets, pool);

#ifdef KEY
  if (compute_bitsets)
    Compute_OPSCH_Blockers(bb, value_map, is_fwd, pool);
#endif
}

// ======================================================================
// Given a list of single-entry multiple exit blocks, build the OPSCH data 
// structure for it.
// ======================================================================
void
Compute_OPSCHs (std::list<BB*> bblist, BB_MAP value_map, MEM_POOL *pool,
		BOOL compute_bitsets, BOOL is_fwd)
{
  std::list<BB*>::iterator bb_iter;

  // Initialize all data structures.
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bb_iter) {
    Init_OPSCH_For_BB (*bb_iter, value_map, compute_bitsets, pool);
  }

  // Do a forward pass first.
  INT max_lstart = 0;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bb_iter) {
    Compute_Fwd_OPSCH (*bb_iter, value_map, &max_lstart, compute_bitsets, pool);
  }

  // Do a backward pass.
  std::list<BB*>::reverse_iterator bb_riter;
  FOR_ALL_BB_STLLIST_ITEMS_BKWD(bblist, bb_riter) {
    Compute_Bkwd_OPSCH(*bb_riter, value_map, max_lstart, compute_bitsets, pool);
  }
}

// ======================================================================
// After the <bb> is scheduled, build the BBSCH data structure for GCM phase.
// ======================================================================
void
HB_Schedule::Compute_BBSCH (BB *bb, BBSCH *bbsch)
{
  INT critical_length = 0;

  BBSCH_schedule_length (bbsch) = OP_scycle(BB_last_op(bb)) + 1;
  // computes the longest (or critical) latency for this bb
  // in inverse order
  for (INT i = 0; i < VECTOR_count(_sched_vector); i++) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    if (!critical_length &&
	(OPSCH_lstart(opsch) - OPSCH_estart(opsch)) == 0) { 
      critical_length = OPSCH_scycle(opsch) - Clock;
    }
    if (CGTARG_Is_OP_Barrier(op)) {
      Set_BB_MEM_BARRIER(bbsch);
      if (critical_length) break;
    }
  }

  // block parallelism is computed as the ratio of the number of
  // ops present in this block divided by the longest critical latency
  // the idea is that this would give a rough feel of the amount of 
  // parallelism present in this bb when compared to CGTARG_Peak_Rate
  BBSCH_block_parallelism (bbsch) =  (INT)(VECTOR_count(_sched_vector) != 0) ?
    (mINT16)ceil(VECTOR_count(_sched_vector) / (critical_length + 1.)) : -1;

 if (Cur_Gcm_Type & GCM_MINIMIZE_REGS) {

   GTN_SET *local_set =  GTN_SET_Intersection(BB_live_in(bb), BB_live_out(bb),
					      &_hb_pool);
   local_set = GTN_SET_Union(local_set, BB_live_in(bb), &_hb_pool);
   local_set = GTN_SET_Intersection(local_set, BB_live_def(bb), 
				    &_hb_pool);

   BBSCH_global_regcost (bbsch) = GTN_SET_Alloc_Size(local_set);
 }

}

#ifdef KEY
// ======================================================================
// Drop the remaining unscheduled prefetches because there are no more
// unused issue slots.
// ======================================================================
void
HB_Schedule::Drop_Remaining_Prefetches (BB *bb)
{
  OP *op;

  FOR_ALL_BB_OPs_FWD(bb, op) {
    if (OP_prefetch(op)) {
      OPSCH *opsch = OP_opsch(op, _hb_map);
      if (!OPSCH_scheduled(opsch)) {
	Set_OPSCH_scheduled(opsch);
	OPSCH_scycle(opsch) = Clock;
	VECTOR_Add_Element(_sched_vector, op);
	deleted_prefetch_count++;
      }
    }
  }
}

// ======================================================================
// Recursively update the least constrained node info.
// ======================================================================
void
HB_Schedule::DFS_Update_Least_Constrained(OPSCH *opsch, BOOL is_fwd)
{
  if (One_Set_MemberP(opsch))
    return;

  One_Set_Union1(opsch);

  ARC_LIST *arcs;
  for (arcs = is_fwd ? OP_succs(OPSCH_op(opsch)) : OP_preds(OPSCH_op(opsch));
       arcs != NULL;
       arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    OP *blocked_op = is_fwd ? ARC_succ(arc) : ARC_pred(arc);
    OPSCH *blocked_opsch = OP_opsch(blocked_op, _hb_map);
    DFS_Update_Least_Constrained(blocked_opsch, is_fwd);
    Update_Blocker_Info(opsch, blocked_opsch);
  }
}

// ======================================================================
// OPSCH has just been scheduled.  Go through the nodes blocked by OPSCH and
// reduce their blockage count by one.  Propagate the least constrained node
// info up the DAG.
// ======================================================================
void
HB_Schedule::Update_Least_Constrained (OPSCH *opsch, BOOL is_fwd)
{
  int i;
  OPSCH *blocked_opsch;
  OPSCH_SET *blockees = is_fwd ? OPSCH_descendants(opsch) :
				 OPSCH_ancestors(opsch);

  for (blocked_opsch = OPSCH_SET_Choose(blockees);
       blocked_opsch != OPSCH_SET_CHOOSE_FAILURE;
       blocked_opsch = OPSCH_SET_Choose_Next(blockees, blocked_opsch)) {
    OPSCH_num_blockers(blocked_opsch)--;
    Is_True(OPSCH_num_blockers(blocked_opsch) >= 0,
	    ("Update_Least_Constrained: incorrect num_blockers"));
  }

  Clear_One_Set();
  for (i = 0; i < VECTOR_count(_ready_vector); i++) {
    op *ready_op = OP_VECTOR_element(_ready_vector, i);
    OPSCH *ready_opsch = OP_opsch(ready_op, _hb_map);
    DFS_Update_Least_Constrained(ready_opsch, is_fwd);
  }
}

// ======================================================================
// Return the number of FP OPs in the ready vector.
// ======================================================================
INT32
HB_Schedule::Ready_Vector_Fp_Count ()
{
  int i, count;

  count = 0;
  for (i = 0; i < VECTOR_count(_ready_vector); i++) {
    op *ready_op = OP_VECTOR_element(_ready_vector, i);
    if (OP_flop(ready_op))
      count++;
  }
  return count;
}

// ======================================================================
// Calculate parameters from the current schedule to guide future
// scheduling.
// ======================================================================
void
HB_Schedule::Update_Schedule_Parameters()
{
  _ready_count = VECTOR_count(_ready_vector);
  if (_ready_count == 0)
    return;

#if defined(TARG_X8664)
  if (_scheduled_opschs != NULL)
    _unsched_count = OPSCH_count - OPSCH_SET_Size(_scheduled_opschs);
#endif

  // Count the OP types in the ready vector.
  if (HBS_Balance_Ready_Types()) {
    int ready_fp_count = Ready_Vector_Fp_Count();
    _ready_fp_percentage = (ready_fp_count * 100) / _ready_count;
  }

  // Count the OP types in the unscheduled OPs.
  if (HBS_Balance_Unsched_Types()) {
    OPSCH_SET *unsched_fp_opschs = OPSCH_SET_Difference(fp_opschs,
							_scheduled_opschs,
							&_hb_pool);
    int unsched_fp_count = OPSCH_SET_Size(unsched_fp_opschs);
#if ! defined(TARG_X8664)
    _unsched_count = OPSCH_count - OPSCH_SET_Size(_scheduled_opschs);
#endif
    _unsched_fp_percentage = (unsched_fp_count * 100) / _unsched_count;
  }
}
#endif

// ======================================================================
// Add the selected <op> to the list of scheduled instructions. Update
// various data structures that are affected by this addition.
// ======================================================================
void
HB_Schedule::Add_OP_To_Sched_Vector (OP *op, BOOL is_fwd)
{
  ARC_LIST *arcs;
  OPSCH *opsch = OP_opsch (op, _hb_map);

  Set_OPSCH_scheduled (opsch);

#ifdef KEY
  if (OP_prefetch(op))
    unsched_prefetch_count--;

  if (_scheduled_opschs != NULL)
    _scheduled_opschs = OPSCH_SET_Union1D(_scheduled_opschs, opsch, &_hb_pool);
#endif

  // Adjust the resource table to account for this OP. Change 'Clock' 
  // to be the scycle of this OP.
  if (!OP_dummy(op)) { 
    Set_Resource_Usage (op);
    Update_Regs_For_OP (op);
  }

  // Remove <op> from Ready_Vector.
  VECTOR_Delete_Element (_ready_vector, op);

#ifdef KEY
  if (HBS_Balance_Ready_Types() ||
      HBS_Balance_Unsched_Types())
    Update_Least_Constrained(opsch, is_fwd);
#endif

  INT count = VECTOR_count(_ready_vector);

  // revise the scycle for all OPs in the Ready_Vector.
  for (INT i = 0; i < count; i++) {
    OP *ready_op = OP_VECTOR_element(_ready_vector,i);
    OPSCH *ready_opsch = OP_opsch (ready_op, _hb_map);
    if (is_fwd) {
      OPSCH_scycle(ready_opsch) = MAX (Clock, OPSCH_scycle(ready_opsch));
    } else {
      OPSCH_scycle(ready_opsch) = MIN (Clock, OPSCH_scycle(ready_opsch));
    }
  }

  if (is_fwd) {
    // Add any OPs that are now ready to be scheduled to the Ready_Vector.
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch (succ_op, _hb_map);

      // Adjust the scycle, num_preds if <succ_op> not prior scheduled.

      if (!OPSCH_scheduled(succ_opsch)) {
	INT scycle = Clock + ARC_latency(arc);
	// update the OPSCH_scycle field for the predecessor OP.
	OPSCH_scycle(succ_opsch) = MAX (scycle, OPSCH_scycle(succ_opsch));

#ifdef TARG_IA64
	OPSCH_num_preds(succ_opsch)--;
	if (OPSCH_num_preds(succ_opsch) == 0) {
	  VECTOR_Add_Element (_ready_vector, succ_op);
	}
#else // TARG_IA64
	
	if( !Is_Ldst_Addiu_Pair( opsch, succ_opsch, op, succ_op ) ){
	  FmtAssert( OPSCH_num_preds(succ_opsch) > 0, 
		     ("HBS: invalid count of succs"));
	  
	  OPSCH_num_preds(succ_opsch)--;
	  if( OPSCH_num_preds(succ_opsch) == 0 ){
	    VECTOR_Add_Element (_ready_vector, succ_op);
	  }
	}
	
	if (PROC_has_branch_delay_slot() && OP_br(succ_op)) {
	  // After register allocation, we may end up with a deadlock 
	  // like the one below
	  // [   3] 0x80a6768 :- beq TN110($2) GTN1($0) (lab:.LBB2_main) ;
	  // [   0] 0x80911e8 GTN136($2) :- sltiu TN110($2) (0x1) ;
	  // Here the delay slot instruction and the branch instructions 
	  // are inter-dependent (REG ATI-DEP, PREBR).
	  // To break this deadlock, let go the branch op.
	  if (OPSCH_num_preds(succ_opsch) == 1) {
	    ARC_LIST *arcs_tmp;
	    for (arcs_tmp = OP_succs(succ_op); 
		 arcs_tmp != NULL; 
		 arcs_tmp = ARC_LIST_rest(arcs_tmp)) {
	      ARC *arc_tmp = ARC_LIST_first(arcs_tmp);
	      OP *succ_succ_op = ARC_succ(arc_tmp);
	      if (succ_succ_op && 
		  (OP_bb(succ_op) == OP_bb(succ_succ_op))) {
		if (OP_results(succ_succ_op)) {
		  if (TN_is_register(OP_result(succ_succ_op, 0)) && 
		      TN_is_register(OP_result(succ_succ_op, 0))) {
		    if (TN_register(OP_result(succ_succ_op, 0)) == 
			TN_register(OP_result(succ_succ_op, 0))) {
		      OPSCH_num_preds(succ_opsch)--;
		      VECTOR_Add_Element (_ready_vector, succ_op);
		      break;
		    }
		  }
		}
	      }
	    }	      
	  }
	}
#endif
      }  // if (!OPSCH_scheduled(succ_opsch)) {
    }  // for (arcs = OP_succs(op; ...

#ifdef KEY
    // If current OP is a load/store, check if it has been scheduled before 
    // an addiu. If yes, we need to account for the latency between the 
    // addiu and the current OP.
    if (OPSCH_ldst(opsch)) {
      for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	OP *pred_op = ARC_pred(arc);
	OPSCH *pred_opsch = OP_opsch (pred_op, _hb_map);
	if (!OPSCH_scheduled(pred_opsch)) {
	  INT opndnum = Memory_OP_Base_Opndnum (op);
	  if( opndnum < 0 ){
	    continue;
	  }
	  INT scycle = Clock + CG_DEP_Latency (pred_op, op, CG_DEP_REGIN, opndnum);
	  OPSCH_scycle(pred_opsch) = MAX (scycle, OPSCH_scycle(pred_opsch));
	}
      }
    }
#endif

  } else {
    // Add any OPs that are now ready to be scheduled to the Ready_Vector.
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch (pred_op, _hb_map);

      // Adjust the scycle, num_succs if <pred_op> not prior scheduled.

      if (!OPSCH_scheduled(pred_opsch)) {
	INT scycle = Clock - ARC_latency(arc);
	// update the OPSCH_scycle field for the predecessor OP.
	OPSCH_scycle(pred_opsch) = MIN (scycle, OPSCH_scycle(pred_opsch));
	if (!Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op)) {
	  FmtAssert (OPSCH_num_succs(pred_opsch) != 0, 
		     ("HBS: invalid count of succs"));
	  
	  OPSCH_num_succs(pred_opsch)--;
	  if (OPSCH_num_succs(pred_opsch) == 0) {
	    VECTOR_Add_Element (_ready_vector, pred_op);
	  }
	}
      }
    }

    // If current OP is a load/store, check if it has been scheduled before 
    // an addiu. If yes, we need to account for the latency between the 
    // addiu and the current OP.
    if (OPSCH_ldst(opsch)) {
      for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	OP *succ_op = ARC_succ(arc);
	OPSCH *succ_opsch = OP_opsch (succ_op, _hb_map);
	if (!OPSCH_scheduled(succ_opsch)) {
	  INT opndnum = Memory_OP_Base_Opndnum (op);
#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_LOONGSON)
	  if( opndnum < 0 ){
	    continue;
	  }
#endif
	  INT scycle = Clock - CG_DEP_Latency (succ_op, op, CG_DEP_REGIN, opndnum);
	  OPSCH_scycle(succ_opsch) = MIN (scycle, OPSCH_scycle(succ_opsch));
	}
      }
    }
  }

  VECTOR_Add_Element (_sched_vector, op);

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "Cycle: %d :: ", Clock);
    Print_OP_No_SrcLine (op);
  }
#ifdef KEY
  // Extract scheduling parameters from the schedule in progress.  These
  // parameters will guide future scheduling.
  Update_Schedule_Parameters();
#endif
}

#ifdef KEY
// ======================================================================
// Decide if scheduling CUR_OPSCH will expose more OPs of a type lacking
// in the ready vector.  Return 1 if yes, -1 if no, and 0 if undecided.
// ======================================================================
static int
Is_OP_Better_For_Balance_Ready_Types(OPSCH *cur_opsch, OPSCH *best_opsch, 
				     int ready_count, int ready_fp_percentage)
{
  OPSCH *cur_least_constrained = NULL;
  OPSCH *best_least_constrained = NULL;

  if (ready_count < 4)		// Threshold before activating the heuristic.
    return 0;

  int ready_int_percentage = 100 - ready_fp_percentage;

  if (ready_int_percentage > LOCS_Balance_Ready_Int) {		// Prefer int.
    cur_least_constrained = OPSCH_least_constrained_int(cur_opsch);
    best_least_constrained = OPSCH_least_constrained_int(best_opsch);
  } else if (ready_fp_percentage > LOCS_Balance_Ready_Fp) {	// Prefer fp.
    cur_least_constrained = OPSCH_least_constrained_fp(cur_opsch);
    best_least_constrained = OPSCH_least_constrained_fp(best_opsch);
  } else {
    return 0;
  }

  if (cur_least_constrained == NULL || best_least_constrained == NULL)
    return 0;

  // Compare the number of unscheduled nodes blocking the least constrained
  // node.
  int cur_num = OPSCH_num_blockers(cur_least_constrained);
  int best_num = OPSCH_num_blockers(best_least_constrained);
  if (cur_num < best_num)	// CUR_OPSCH blocks a less constrained node.
    return 1;
  if (cur_num > best_num)	// BEST_OPSCH blocks a less constrained node.
    return -1;
  return 0;
}

// ======================================================================
// Decide if scheduling CUR_OPSCH will expose more OPs of the type that
// make up the majority of the unscheduled OPs.  Return 1 if yes, -1 if
// no, and 0 if undecided.
// ======================================================================
static int
Is_OP_Better_For_Balance_Unsched_Types(OPSCH *cur_opsch, OPSCH *best_opsch, 
				       int unsched_count,
				       int unsched_fp_percentage)
{
  OPSCH *cur_least_constrained = NULL;
  OPSCH *best_least_constrained = NULL;

  if (unsched_count < 10)	// Threshold before activating the heuristic.
    return 0;

  int unsched_int_percentage = 100 - unsched_fp_percentage;

  if (unsched_int_percentage > LOCS_Balance_Unsched_Int) {	// Prefer int.
    cur_least_constrained = OPSCH_least_constrained_int(cur_opsch);
    best_least_constrained = OPSCH_least_constrained_int(best_opsch);
  } else if (unsched_fp_percentage > LOCS_Balance_Unsched_Fp) {	// Prefer fp.
    cur_least_constrained = OPSCH_least_constrained_fp(cur_opsch);
    best_least_constrained = OPSCH_least_constrained_fp(best_opsch);
  } else {
    return 0;
  }

  if (cur_least_constrained == NULL || best_least_constrained == NULL)
    return 0;

  // Compare the number of unscheduled nodes blocking the least constrained
  // node.
  int cur_num = OPSCH_num_blockers(cur_least_constrained);
  int best_num = OPSCH_num_blockers(best_least_constrained);
  Is_True(cur_num < unsched_count,
	  ("Is_OP_Better_For_Balance_Unsched_Types: illegal blocker count"));
  Is_True(best_num < unsched_count,
	  ("Is_OP_Better_For_Balance_Unsched_Types: illegal blocker count"));
  if (cur_num < best_num)	// CUR_OPSCH blocks a less constrained node.
    return 1;
  if (cur_num > best_num)	// BEST_OPSCH blocks a less constrained node.
    return -1;
  return 0;
}
#endif

// ======================================================================
// Compare two OPs to see which one is better for scheduling.
// ======================================================================
BOOL
Priority_Selector::Is_OP_Better (OP *cur_op, OP *best_op)
{
  OPSCH *cur_opsch = OP_opsch(cur_op, _cur_sched->hb_map());
  OPSCH *best_opsch = OP_opsch(best_op, _cur_sched->hb_map());
  INT cur_scycle = OPSCH_scycle(cur_opsch);
  INT best_scycle = OPSCH_scycle(best_opsch);

#ifdef KEY
  // Schedule non-prefetches first.
  if (_cur_sched->HBS_Drop_Unsched_Prefetches()) {
    if (OP_prefetch(cur_op) && !OP_prefetch(best_op) &&
	cur_scycle <= best_scycle)
      return FALSE;
    if (!OP_prefetch(cur_op) && OP_prefetch(best_op) &&
	cur_scycle >= best_scycle)
      return TRUE;
  }
#endif

#if defined(TARG_MIPS) && !(TARG_SL)
  // Schedule definitions of fcc as close as possible to their uses, in order
  // to shorten the fcc's live range to prevent spilling.  Spilling fcc is not
  // possible because there is no load/store instruction for fcc.  Bug 13241.
  if (cur_scycle >= best_scycle &&
      OP_results(cur_op) == 1 &&
      TN_register_class(OP_result(cur_op, 0)) == ISA_REGISTER_CLASS_fcc)
    return TRUE;
  if (best_scycle >= cur_scycle &&
      OP_results(best_op) == 1 &&
      TN_register_class(OP_result(best_op, 0)) == ISA_REGISTER_CLASS_fcc)
    return FALSE;
#endif

   if (_hbs_type & HBS_MINIMIZE_REGS) {
    INT cur_op_better = 0;
    cur_op_better = (cur_scycle - best_scycle);
    if (cur_op_better == 0) {
      cur_op_better = (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
    }
    cur_op_better += (OPSCH_regcost(best_opsch) - OPSCH_regcost(cur_opsch));
    return (cur_op_better > 0);
  }

  BOOL manual_pref = FALSE;
  if (OP_prefetch(cur_op)) {
    WN *pref_wn = Get_WN_From_Memory_OP(cur_op);
    if (pref_wn && WN_pf_manual(pref_wn)) manual_pref = TRUE;
  }

  if (cur_scycle > best_scycle)  {

    // Special case manual prefetch nodes. 
    if (manual_pref) return FALSE;
    else return TRUE;
  }

  if (cur_scycle < best_scycle) return FALSE;

  if (manual_pref) return FALSE;

  // For T5, try to schedule the OPs defining the operands of a terminating
  // branch as far away from the branch as possible.
  if (OPSCH_def_xfer_opnd(cur_opsch) ^ OPSCH_def_xfer_opnd(best_opsch)) {
    return OPSCH_def_xfer_opnd(best_opsch);
  }

#ifdef TARG_X8664

  // for SL2, until we deal with SIMD, we skip this - sunchan

  /* For two load/store operations that access the same array,
     schedule the one load/store the lower address first.

     TODO in beta-5: remove the first checking. Don't have to be vector_op!!!
   */
  if( TOP_is_vector_op( OP_code(cur_op ) ) &&
      TOP_is_vector_op( OP_code(best_op ) ) ){
    if( ( OP_load( cur_op ) && OP_load( best_op ) ) ||
	( OP_store( cur_op ) && OP_store( best_op ) ) ){
      TN* cur_base = NULL;
      TN* cur_ofst = NULL;
      TN* best_base = NULL;
      TN* best_ofst = NULL;

      OP_Base_Offset_TNs( cur_op,  &cur_base,  &cur_ofst );
      OP_Base_Offset_TNs( best_op, &best_base, &best_ofst );

      if( cur_base == best_base &&
	  ( cur_ofst != NULL && best_ofst != NULL ) ){
	if( TN_has_value( cur_ofst )  &&
	    TN_has_value( best_ofst ) ){
	  const bool cur_is_better = TN_value(cur_ofst) < TN_value(best_ofst );
	  return Is_Fwd_Schedule() ?  cur_is_better : !cur_is_better;
	}

	if( TN_is_symbol( cur_ofst )  &&
	    TN_is_symbol( best_ofst ) &&
	    TN_var(cur_ofst) == TN_var(best_ofst) ){
	  const bool cur_is_better = TN_offset( cur_ofst) < TN_offset( best_ofst );
	  return Is_Fwd_Schedule() ? cur_is_better : !cur_is_better;
	}
      }
    }
  }
#endif

#ifdef KEY
  if (_hbs_type & HBS_BALANCE_UNSCHED_TYPES) {
    int status =
      Is_OP_Better_For_Balance_Unsched_Types(cur_opsch, best_opsch, 
					     _cur_sched->Unsched_Count(),
					     _cur_sched->Unsched_Fp_Percentage());
    if (status == 1)  return TRUE;
    if (status == -1) return FALSE;
  }
#endif

  if (_hbs_type & HBS_DEPTH_FIRST) {
    return (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
  }

  if (_hbs_type & HBS_CRITICAL_PATH) {
    INT cur_slack, best_slack;
    cur_slack = OPSCH_lstart(cur_opsch) - OPSCH_estart(cur_opsch);
    best_slack = OPSCH_lstart(best_opsch) - OPSCH_estart(best_opsch);
    if (cur_slack < best_slack) return TRUE;
    if (cur_slack > best_slack) return FALSE;

    if (OPSCH_estart(cur_opsch) > OPSCH_estart(best_opsch)) return TRUE;
    if (OPSCH_estart(cur_opsch) < OPSCH_estart(best_opsch)) return FALSE;
  }

#ifdef KEY
  if (_hbs_type & HBS_BALANCE_READY_TYPES) {
    int status =
      Is_OP_Better_For_Balance_Ready_Types(cur_opsch, best_opsch,
					   _cur_sched->Ready_Count(),
					   _cur_sched->Ready_Fp_Percentage());
    if (status == 1)  return TRUE;
    if (status == -1) return FALSE;
  }
#endif

  return FALSE;
}

// ======================================================================
// Pick an OP to schedule in the delay slot of the terminating branch.
// ======================================================================
OP*
Priority_Selector::Select_OP_For_Delay_Slot (OP *xfer_op)
{

  // If the <xfer_op> has a successor, then that op has to remain in the
  // delay slot. Make sure we return this OP.
  OPSCH *opsch = OP_opsch(xfer_op, _cur_sched->hb_map());
  if (OPSCH_num_succs(opsch) != 0) {
    ARC_LIST *arcs = OP_succs(xfer_op);
    ARC *first_arc = ARC_LIST_first(arcs);
    if (OPSCH_num_succs(opsch) > 1) {
    	ARC *second_arc = ARC_LIST_first(ARC_LIST_rest(arcs));
        DevAssert (ARC_succ(first_arc) == ARC_succ(second_arc), 
		("More than 1 successor for xfer_op"));
    }
    return ARC_succ(first_arc);
  }

  // If this optimization has been disabled or 
  // If we are scheduling to minimize registers, don't put anything 
  // in the delay slot. Trying to spill something in the delay slot 
  // is no fun.
  if (!Enable_Fill_Delay_Slots || 
      (_hbs_type & HBS_MINIMIZE_REGS)) return NULL;

  OP *best_op = NULL;

  for (INT i = VECTOR_count(_cur_sched->ready_vector())-1; i >= 0; i--) 
  {
    OP *cur_op = OP_VECTOR_element (_cur_sched->ready_vector(), i);

    // Don't schedule any dummy OPs or OPs that expand into 
    // more than 1 instruction in the delay slot.
    if (OP_xfer(cur_op) || OP_Real_Ops(cur_op) != 1) continue;

    // Don't put instructions that have hazards in the delay slot.
    if (OP_has_hazard(cur_op)) continue;

#ifndef KEY
    // R10k chip bug workaround: Avoid placing integer mult/div in delay
    // slots of unconditional branches. (see pv516598) for more details.
    if (OP_uncond(xfer_op) && (OP_imul(cur_op) || OP_idiv(cur_op)))
      continue;
#endif

    // Don't put manual prefetches into delay slots as well.
    if (OP_prefetch(cur_op)) {
      WN *pref_wn = Get_WN_From_Memory_OP(cur_op);
      if (pref_wn && WN_pf_manual(pref_wn)) continue;
    }

    // When GCM is calling it, don't try to schedule the <op> that has
    // moved from a different block in the delay slot. frequent observance
    // is that this will unneccessarily restrict further code motion.
    if ((_hbs_type & HBS_FROM_GCM) && OP_moved(cur_op)) continue;

    if (best_op == NULL || Is_OP_Better(cur_op, best_op)) {
      best_op = cur_op;
    }
  }
  return best_op;
}

#ifdef TARG_X8664
// ======================================================================
// Determine how to schedule an OP with preallocated TNs.  Return 1 if it must
// be scheduled now, -1 if it should not be scheduled now, or 0 if it is ok but
// not necessary to schedule it now.
// ======================================================================
int
Priority_Selector::Sched_OP_With_Preallocated_TN (OP *op)
{
  // If OP references a preallocated TN, then determine if OP must be scheduled
  // in the current cycle as follows.  Find PRED_OP that defines the
  // preallocated TN.  Find the OPs that reference the preallocated TN(s)
  // defined by PRED_OP.  These are OP's "siblings".  Schedule OP and the
  // siblings together as a group so that PRED_OP can be scheduled immediately
  // afterwards, thereby shortening the live ranges of the preallocated TNs.
  // (This assumes backwards scheduling.)  Handle three cases for OP:
  //  1)  One of OP's siblings is not ready.  Try not to schedule OP now.
  //  2)  One of OP's siblings is already scheduled.  Must schedule OP now.
  //  3)  All of OP's siblings are ready but none has scheduled.  It is ok but
  //      not necessary to schedule OP now.
  for (int opndnum = 0; opndnum < OP_opnds(op); opndnum++) {
    TN *op_opnd_tn = OP_opnd(op, opndnum);
    if (TN_is_register(op_opnd_tn) &&
	TN_is_preallocated(op_opnd_tn)) {
      ARC_LIST *arcs1, *arcs2;
      for (arcs1 = OP_preds(op); arcs1 != NULL; arcs1 = ARC_LIST_rest(arcs1)) {
	ARC *arc1 = ARC_LIST_first(arcs1);
	OP *pred_op = ARC_pred(arc1);
	if (!OP_Defs_TN(pred_op, op_opnd_tn))
	  continue;
	// PRED_OP defines the preallocated TN referenced by OP.
	for (int i = 0; i < OP_results(pred_op); i++) {
	  TN *pred_result_tn = OP_result(pred_op, i);
	  if (TN_is_register(pred_result_tn) &&
	      TN_is_preallocated(pred_result_tn)) {
	    // Pred's RESULT_TN is a preallocated TN.  For each of OP's
	    // siblings that references RESULT_TN, see if it is ready or
	    // scheduled.
	    for (arcs2 = OP_succs(pred_op);
		 arcs2 != NULL;
		 arcs2 = ARC_LIST_rest(arcs2)) {
	      ARC *arc2 = ARC_LIST_first(arcs2);
	      OP *sib_op = ARC_succ(arc2);
	      // See if it's a sibling OP that references pred's RESULT_TN.
	      if (op != sib_op &&
		  OP_Refs_TN(sib_op, pred_result_tn)) {
		OPSCH *sib_opsch = OP_opsch(sib_op, _cur_sched->hb_map());
		if (OPSCH_scheduled(sib_opsch))
		  return 1;	// Sibling is scheduled.  Must schedule OP now.
		if (!VECTOR_Member_Element(_cur_sched->ready_vector(),
					   (void*) sib_op))
		  return -1;	// Sibling is not ready.  Try not to schedule
				// OP now.
	      }
	    }
	  }
	}
      }
    }
  }
  return 0;
}
#endif

// ======================================================================
// Put the scheduled list of instructions back into the basic block. 
// This is done by emptying the basic block and inserting the scheduled
// instructions back into the basic block.
// ======================================================================
void
HB_Schedule::Put_Sched_Vector_Into_BB (BB *bb, BBSCH *bbsch, BOOL is_fwd)
{
  INT i;
  INT32 cur_cycle = 0;
  INT prefetches_to_delete = 0;
#ifdef KEY
  INT *old_scycle = (INT *) alloca(VECTOR_count(_sched_vector) * sizeof(INT));

  // Find the number of prefetches to be deleted.  These prefetches are located
  // at the end of the schedule and occupy a single cycle by themselves.
  if (HBS_Drop_Unsched_Prefetches()) {
    Is_True(!is_fwd, ("Put_Sched_Vector_Into_BB: prefetch deletion not supported under forward scheduling"));

    OPSCH *prev_prefetch_opsch = NULL;
    for (i = VECTOR_count(_sched_vector) - 1; i >= 0; i--) {
      OP *op = OP_VECTOR_element(_sched_vector, i);
      OPSCH *opsch = OP_opsch(op, _hb_map);
      if (prev_prefetch_opsch != NULL &&
	  OPSCH_scycle(opsch) != OPSCH_scycle(prev_prefetch_opsch)) {
	prefetches_to_delete++;
      }
      if (!OP_prefetch(op))	// No more prefetches to delete.
	break;
      prev_prefetch_opsch = opsch;
    }
  }
#endif

  // Set the OP_scycle field for all the OPs. Also, reset the OPSCH_visited
  // flag. It is used in the Adjust_Ldst_Offsets routine.
  for (i = VECTOR_count(_sched_vector) - 1; i >= 0; i--) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    Reset_OPSCH_visited (opsch);
#ifdef KEY
    old_scycle[i] = OP_scycle(op);	// Save the old scycle.
#endif
#if ! defined(TARG_X8664)
    OP_scycle(op) = (is_fwd) ? OPSCH_scycle(opsch) : OPSCH_scycle(opsch) - Clock;
#else
    OP_scycle(op) = (is_fwd) ?
		      OPSCH_scycle(opsch) :
		      OPSCH_scycle(opsch) - Clock - prefetches_to_delete;
    if (OP_scycle(op) < 0) {
      Is_True(OP_prefetch(op),
	      ("Put_Sched_Vector_Into_BB: incorrect clock calculation"));
      OP_scycle(op) = 0;
    }
    if (OP_scycle(op) > cur_cycle)
      cur_cycle = OP_scycle(op);
#endif
  }

  cur_cycle = OP_scycle(BB_last_op(bb)) + 1;

  // If current cycle estimate is better than <max_sched>, then ONLY dump
  // the Sched_Vector buffer. Otherwise, preserve the previous one.

  if (cur_cycle < _max_sched) {
#ifdef KEY
    Adjust_Ldst_Offsets( is_fwd );
#else
    Adjust_Ldst_Offsets ();
#endif

    if (bbsch != NULL) {
      Compute_BBSCH (bb, bbsch);
    }

    BB_Remove_All(bb);

    // Dump the <sched_vector> backward (or forward) depending on the
    // type of the schedule.
    for (i = (is_fwd) ? 0 : VECTOR_count(_sched_vector) - 1; 
	 (is_fwd) ? i < VECTOR_count(_sched_vector) : i >= 0; 
	 (is_fwd) ? i++ : i--) {
      OP *op = OP_VECTOR_element(_sched_vector, i);
      BB_Append_Op(bb, op);
#ifdef KEY
      // Mark the prefetches deleted by the current schedule.
      if (OP_prefetch(op)) {
	if (prefetches_to_delete > 0) {
	  Set_OP_prefetch_deleted(op);
	  prefetches_to_delete--;
	} else {
	  Reset_OP_prefetch_deleted(op);
	}
      }
#endif
    }
  }
#ifdef KEY
  // Reusing the old schedule.  Restore the old scycle numbers for more
  // accurate asm annotation.
  else {
    for (i = VECTOR_count(_sched_vector) - 1; i >= 0; i--) {
      OP *op = OP_VECTOR_element(_sched_vector, i);
      OP_scycle(op) = old_scycle[i];
    }
  }
#endif
}

// ======================================================================
// Put the scheduled list of instructions back into the basic block. 
// This is done by emptying the basic block and inserting the scheduled
// instructions back into the basic block.
// ======================================================================
void
HB_Schedule::Put_Sched_Vector_Into_HB (std::list<BB*>& bblist)
{
  INT i;

  // Set the OP_scycle field for all the OPs. Also, reset the OPSCH_visited
  // flag. It is used in the Adjust_Ldst_Offsets routine.
  for (i = VECTOR_count(_sched_vector)-1; i >= 0; i--) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    Reset_OPSCH_visited (opsch);
    OP_scycle(op) = OPSCH_scycle(opsch);
  }

  std::list<BB*>::iterator bb_iter;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bb_iter) {
    BB_Remove_All(*bb_iter);
  }

  bb_iter = bblist.begin();
  for (i = 0; i < VECTOR_count(_sched_vector); i++) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    FmtAssert (bb_iter != bblist.end(), ("bb_iter is NULL info"));
    BB_Append_Op(*bb_iter, op);

    // Advance to the next block when noticed an <xfer_op>.
    if (OP_xfer(op)) { bb_iter++; }
  }
}

// ======================================================================
// Allocate a RFlag_Table with <cycles> entries. Initialize the entries.
// ======================================================================
void
HB_Schedule::Init_RFlag_Table (std::list<BB*>& bblist, BOOL is_fwd)
{
  INT rtable_size = 0;
  INT max_resource_cycles = 0;

  _rr_tab = TI_RES_RES_Alloc(FALSE, &_hb_pool);

  std::list<BB*>::iterator bbi;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    OP *op;
    FOR_ALL_BB_OPs_FWD (*bbi, op) {
      INT cur_resource_cycles = Resource_Cycles_For_OP (op);
      if (cur_resource_cycles > max_resource_cycles) {
	max_resource_cycles = cur_resource_cycles;
      }
      INT op_latency = cur_resource_cycles;
      ARC_LIST *arcs;
      for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	if (ARC_latency(arc) > op_latency) {
	  op_latency = ARC_latency(arc);
	}
      }
      rtable_size += op_latency;
    }
  }

  // start scheduling OPs at this <rtable_size>.
  Clock = (is_fwd) ? 0 : rtable_size;
  MAX_Clock = rtable_size;

  // increase table size by the maximum number of resource cycles needed by
  // any OP.
  rtable_size += max_resource_cycles;

  TI_RES_RES_Set_BB_Cycle_Count(_rr_tab, rtable_size);
}

List_Based_Bkwd::List_Based_Bkwd (BB *bb, HB_Schedule *sched, HBS_TYPE type, 
				  MEM_POOL *pool) : 
  Priority_Selector(bb, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bb, this->Is_Fwd_Schedule());

  BOOL traverse_dag = sched->HBS_Depth_First();
#ifdef KEY
  traverse_dag |= sched->HBS_Balance_Ready_Types();
  traverse_dag |= sched->HBS_Balance_Unsched_Types();
#endif

  if (traverse_dag)
    Compute_DFO (sched, sched->hb_map(), this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bb, sched->hb_map());

}

List_Based_Bkwd::List_Based_Bkwd (std::list<BB*> bblist, HB_Schedule *sched, 
				  HBS_TYPE type, MEM_POOL *pool) : 
  Priority_Selector(bblist, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bblist, this->Is_Fwd_Schedule());

  BOOL traverse_dag = sched->HBS_Depth_First();
#ifdef KEY
  traverse_dag |= sched->HBS_Balance_Ready_Types();
  traverse_dag |= sched->HBS_Balance_Unsched_Types();
#endif

  if (traverse_dag)
    Compute_DFO(sched, sched->hb_map(), this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bblist, sched->hb_map());

}

// ======================================================================
// Select an OP to schedule next from the Ready_Vector.
// ======================================================================
void*
Priority_Selector::Get_Next_Element(HB_Schedule *Cur_Sched)
{
#ifdef TARG_X8664
  OP *last_choice_op = NULL;
#endif
  _best_op = NULL;

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "-------------------------------------------\n");
#ifdef KEY
    if (Cur_Sched->HBS_Balance_Ready_Types()) {
      fprintf(TFile, "Ready vector OPs:%d (%d%% fp)\n",
	      Cur_Sched->Ready_Count(), Cur_Sched->Ready_Fp_Percentage());
    }
    if (Cur_Sched->HBS_Balance_Unsched_Types()) {
      fprintf(TFile, "Unscheduled OPs:%d (%d%% fp)\n",
	      Cur_Sched->Unsched_Count(), Cur_Sched->Unsched_Fp_Percentage());
    }
#endif
    fprintf (TFile, "Candidates for Scheduling:\n");
    fprintf (TFile, "-------------------------------------------\n");
  }

  for (INT i = VECTOR_count(Cur_Sched->ready_vector())-1; i >= 0; i--) 
  {
    OP *cur_op = OP_VECTOR_element (Cur_Sched->ready_vector(), i);

    // update the scycle for the <cur_op>.
    if (!OP_dummy(cur_op)) Cur_Sched->Find_Schedule_Cycle (cur_op, FALSE);

    if (Cur_Sched->HBS_Minimize_Regs() && !OP_dummy(cur_op)) {
      Cur_Sched->Estimate_Reg_Cost_For_OP (cur_op);
    }

    if (Trace_HB) {
      #pragma mips_frequency_hint NEVER
      Print_OPSCH (cur_op, Cur_Sched->hb_map());
    }

#ifdef TARG_X8664_TODO
    /* Try to schedule movlpd and movhpd back-to-back.
     */
    const bool is_fwd = Is_Fwd_Schedule();

    if( ( is_fwd  && TOP_is_vector_high_loadstore( OP_code(cur_op) ) ) ||
	( !is_fwd && TOP_is_vector_lo_loadstore( OP_code(cur_op) ) ) ){
      for( ARC_LIST* arcs = is_fwd ? OP_preds(cur_op) : OP_succs(cur_op);
	   arcs != NULL;
	   arcs = ARC_LIST_rest(arcs) ){
	const ARC* arc = ARC_LIST_first(arcs);
	OP* couple = is_fwd ? ARC_pred(arc) : ARC_succ(arc);

	if( ( couple == _last_sched_op ) &&
	    ( ARC_kind(arc) == CG_DEP_MEMOUT ||
	      ARC_kind(arc) == CG_DEP_REGOUT ) ){
	  _last_sched_op = _best_op = cur_op;
	  return (void*)_best_op;
	}
      }
    }
#endif

#ifdef TARG_X8664
    // If scheduling to reduce LRA register pressure, schedule the OPs that
    // have preallocated TNs as close as possible to the copies that move the
    // preallocated TNs to/from regular TNs.  This is needed to shorten the
    // live range of the registers corresponding to the preallocated TNs;
    // otherwise LRA can run out of registers and die.  Bug 6081.
    if (Cur_Sched->HBS_Minimize_Regs()) {
      INT status = Sched_OP_With_Preallocated_TN(cur_op);
      if (status == 1) {		// Must pick cur_op.
	_best_op = cur_op;
	break;
      } else if (status == -1) {	// Try not to pick cur_op.
	last_choice_op = cur_op;
	continue;
      }
    }
#endif

    // Replace the best_op by the cur_op if any of the following is true:
    //   1. best_op is NULL, i.e. cur_op is the first one we have seen.
    //   2. The cur_op is better based on some heuristics.
    if (_best_op == NULL || Is_OP_Better (cur_op, _best_op)) {
      _best_op = cur_op;
    }
  }

#ifdef TARG_X8664
  // Must pick a copy of a preallocated TN if that's the only ready OP.
  if (_best_op == NULL)
    _best_op = last_choice_op;

  _last_sched_op = _best_op;
#endif // KEY

  return (void *) _best_op;
}

// ======================================================================
// Compare two OPs to see which one is better for scheduling.
// ======================================================================
BOOL
List_Based_Fwd::Is_OP_Better (OP *cur_op, OP *best_op)
{
  OPSCH *cur_opsch = OP_opsch(cur_op, _cur_sched->hb_map());
  OPSCH *best_opsch = OP_opsch(best_op, _cur_sched->hb_map());
  INT cur_scycle = OPSCH_scycle(cur_opsch);
  INT best_scycle = OPSCH_scycle(best_opsch);

#if defined(TARG_MIPS) && !defined(TARG_SL)
  // Schedule uses of fcc as close as possible to their definitons, in order
  // to shorten the fcc's live range to prevent spilling.  Spilling fcc is not
  // possible because there is no load/store instruction for fcc.  Bug 13241.
  if (cur_scycle <= best_scycle) {
    for (int opndnum = 0; opndnum < OP_opnds(cur_op); opndnum++) {
      TN *tn = OP_opnd(cur_op, opndnum);
      if (TN_is_register(tn) &&
	  TN_register_class(tn) == ISA_REGISTER_CLASS_fcc)
	return TRUE;
    }
  }
  if (best_scycle <= cur_scycle) {
    for (int opndnum = 0; opndnum < OP_opnds(best_op); opndnum++) {
      TN *tn = OP_opnd(best_op, opndnum);
      if (TN_is_register(tn) &&
	  TN_register_class(tn) == ISA_REGISTER_CLASS_fcc)
	return FALSE;
    }
  }
#endif

  if (_hbs_type & HBS_MINIMIZE_REGS) {
     INT cur_op_better = (best_scycle - cur_scycle);
     if (cur_op_better == 0) {
       cur_op_better = (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
     }
     cur_op_better += (OPSCH_regcost(cur_opsch) - OPSCH_regcost(best_opsch));
     return (cur_op_better > 0);
  }

  if (cur_scycle < best_scycle) return TRUE;

  if (cur_scycle > best_scycle) return FALSE;

  // branch-predict instructions ALWAYS need to be scheduled early but not
  // too early. Need to check if the BB_length doesn't exceed the offset
  // limits imposed by the ISA. It's assumed that about 1/3 nops will be
  // added later, so include the expansion factor. 

  if (OP_branch_predict(cur_op) && 
      ((BB_length(OP_bb(cur_op)) * 1.3 * INST_BYTES) 
       < DEFAULT_BRP_BRANCH_LIMIT)) 
    return TRUE;

#ifdef KEY
  if (_hbs_type & HBS_BALANCE_UNSCHED_TYPES) {
    int status =
      Is_OP_Better_For_Balance_Unsched_Types(cur_opsch, best_opsch, 
					     _cur_sched->Unsched_Count(),
					     _cur_sched->Unsched_Fp_Percentage());
    if (status == 1)  return TRUE;
    if (status == -1) return FALSE;
  }
#endif

  if (_hbs_type & HBS_DEPTH_FIRST) {
    return (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
  }

  if (_hbs_type & HBS_CRITICAL_PATH) {
    INT cur_slack, best_slack;
    cur_slack = OPSCH_lstart(cur_opsch) - OPSCH_estart(cur_opsch);
    best_slack = OPSCH_lstart(best_opsch) - OPSCH_estart(best_opsch);
    if (cur_slack < best_slack) return TRUE;
    if (cur_slack > best_slack) return FALSE;

    if (OPSCH_estart(cur_opsch) > OPSCH_estart(best_opsch)) return TRUE;
    if (OPSCH_estart(cur_opsch) < OPSCH_estart(best_opsch)) return FALSE;
  }

#ifdef TARG_X8664
  /* TODO:
     Now it is seperated for debugging purpose. Once it is stable,
     merge it with its parent. 
   */

  /* Favor load over prefetching. */

  if( OP_memory(cur_op) && OP_prefetch(best_op) ){
    return Is_Fwd_Schedule() ? TRUE : FALSE;
  }

  if( OP_memory(best_op) && OP_prefetch(cur_op) ){
    return Is_Fwd_Schedule() ? FALSE : TRUE;
  }

  /* For two load/store operations that access the same array,
     schedule the one load/store the lower address first.
  */

  if( ( OP_load( cur_op ) && OP_load( best_op ) ) ||
      ( OP_store( cur_op ) && OP_store( best_op ) ) ){
    TN* cur_base = NULL;
    TN* cur_ofst = NULL;
    TN* best_base = NULL;
    TN* best_ofst = NULL;

    OP_Base_Offset_TNs( cur_op,  &cur_base,  &cur_ofst );
    OP_Base_Offset_TNs( best_op, &best_base, &best_ofst );

    if( cur_base == best_base &&
	( cur_ofst != NULL && best_ofst != NULL ) ){
      if( TN_has_value( cur_ofst )  &&
	  TN_has_value( best_ofst ) ){
	const bool cur_is_better = TN_value(cur_ofst) < TN_value(best_ofst );
	return Is_Fwd_Schedule() ?  cur_is_better : !cur_is_better;
      }
      
      if( TN_is_symbol( cur_ofst )  &&
	  TN_is_symbol( best_ofst ) &&
	  TN_var(cur_ofst) == TN_var(best_ofst) ){
	const bool cur_is_better = TN_offset( cur_ofst) < TN_offset( best_ofst );
	return Is_Fwd_Schedule() ? cur_is_better : !cur_is_better;
      }
    }
  }
#endif

#ifdef KEY
  if (_hbs_type & HBS_BALANCE_READY_TYPES) {
    int status =
      Is_OP_Better_For_Balance_Ready_Types(cur_opsch, best_opsch,
					   _cur_sched->Ready_Count(),
					   _cur_sched->Ready_Fp_Percentage());
    if (status == 1)  return TRUE;
    if (status == -1) return FALSE;
  }
#endif

  return FALSE;
}

List_Based_Fwd::List_Based_Fwd (BB *bb, HB_Schedule *sched, HBS_TYPE type, 
				MEM_POOL *pool) : 
  Priority_Selector(bb, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bb, this->Is_Fwd_Schedule());

  BOOL traverse_dag = sched->HBS_Depth_First();
#ifdef KEY
  traverse_dag |= sched->HBS_Balance_Ready_Types();
  traverse_dag |= sched->HBS_Balance_Unsched_Types();
#endif

  if (traverse_dag)
    Compute_DFO(sched, sched->hb_map(), this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bb, sched->hb_map());

}

List_Based_Fwd::List_Based_Fwd (std::list<BB*> bblist, HB_Schedule *sched, 
				HBS_TYPE type, MEM_POOL *pool) : 
  Priority_Selector(bblist, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bblist, this->Is_Fwd_Schedule());

  BOOL traverse_dag = sched->HBS_Depth_First();
#ifdef KEY
  traverse_dag |= sched->HBS_Balance_Ready_Types();
  traverse_dag |= sched->HBS_Balance_Unsched_Types();
#endif

  if (traverse_dag)
    Compute_DFO(sched, sched->hb_map(), this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bblist, sched->hb_map());

}

// ======================================================================
// Select an OP to schedule next from the Ready_Vector.
// ======================================================================
void*
List_Based_Fwd::Get_Next_Element(HB_Schedule *Cur_Sched)
{
  _best_op = NULL;

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "-------------------------------------------\n");
#ifdef KEY
    if (Cur_Sched->HBS_Balance_Ready_Types()) {
      fprintf(TFile, "Ready vector OPs:%d (%d%% fp)\n",
	      Cur_Sched->Ready_Count(), Cur_Sched->Ready_Fp_Percentage());
    }
    if (Cur_Sched->HBS_Balance_Unsched_Types()) {
      fprintf(TFile, "Unscheduled OPs:%d (%d%% fp)\n",
	      Cur_Sched->Unsched_Count(), Cur_Sched->Unsched_Fp_Percentage());
    }
#endif
    fprintf (TFile, "Candidates for Scheduling:\n");
    fprintf (TFile, "-------------------------------------------\n");
  }

  for (INT i = VECTOR_count(Cur_Sched->ready_vector())-1; i >= 0; i--) 
  {
    OP *cur_op = OP_VECTOR_element (Cur_Sched->ready_vector(), i);

    // update the scycle for the <cur_op>.
    if (!OP_dummy(cur_op)) Cur_Sched->Find_Schedule_Cycle (cur_op, TRUE);

    if (Cur_Sched->HBS_Minimize_Regs() && !OP_dummy(cur_op)) {
      Cur_Sched->Estimate_Reg_Cost_For_OP (cur_op);
    }

    if (Trace_HB) {
      #pragma mips_frequency_hint NEVER
      Print_OPSCH (cur_op, Cur_Sched->hb_map());
    }

    // Replace the best_op by the cur_op if any of the following is true:
    //   1. best_op is NULL, i.e. cur_op is the first one we have seen.
    //   2. The cur_op is better based on some heuristics.
    if (_best_op == NULL || Is_OP_Better (cur_op, _best_op)) {
      _best_op = cur_op;
    }
  }

#ifdef KEY
  _last_sched_op = _best_op;
#endif // KEY

  return (void *) _best_op;
}

// ======================================================================
// ::Invoke_Pre_HBS_Phase
// All the scheduling preparatory stuff (eg. separating out the special
// instructions to prolog, epilog blocks, etc..) should be done here,
// before the actual scheduling begins.
// ===================================================================
void
HB_Schedule::Invoke_Pre_HBS_Phase(BB* bb)
{

  OP *op, *prev_op;
  OP *next_op;

#if defined(TARG_X8664) || defined(TARG_SL)
  op = BB_last_op( bb );
  if( op != NULL && OP_cond(op) ){
    /* If a previous test op has been removed, then don't execute this
       function which will set aside the cond jmp op to <_epilog_bb>;
       otherwise, scheduler will give the wrong order so that the
       rflags will not be set up correctly.
    */
    for( op = OP_prev(op); op != NULL; op = OP_prev(op) ){
#ifdef TARG_X8664
      if( TOP_is_change_rflags( OP_code(op) ) ){
#else
	if (1) { // rflags is defined inside targ_info/isa/x8664
#endif
	if( !OP_icmp( op ) )
	  return;
	break;
      }
    }
  }
#endif

  // When we are scheduling before register allocation, we don't want 
  // to schedule SP adjustment OPs in the entry/exit blocks and OPs
  // that are marked with the OP_glue attribute. We also don't want 
  // to schedule COPY instructions for save/restore of callee save 
  // registers. We check for them and move them away to temporary basic 
  // blocks. After we are done scheduling the remaining OPs, we merge the 
  // moved instructions back.

  if (HBS_Before_LRA()) {
    if (BB_entry(bb)) {
      _prolog_bb = Gen_BB_Like (bb);
      for (op = BB_entry_sp_adj_op (bb); op != NULL; op = prev_op) {
        prev_op = OP_prev(op);
        BB_Move_Op_To_Start (_prolog_bb, bb, op);
      }
    }
    if (BB_exit(bb)) {
      _epilog_bb = Gen_BB_Like (bb);
      for (op = BB_exit_sp_adj_op (bb); op != NULL; op = next_op) {
	next_op = OP_next(op);
	BB_Move_Op_To_End (_epilog_bb, bb, op);
      }
    }
    for (op = BB_first_op(bb); op != NULL; op = next_op) {
      if (!OP_copy(op) && !OP_glue(op) && !OP_no_move_before_gra(op) && !OP_access_reg_bank(op)) break;
      next_op = OP_next(op);
      if (_prolog_bb == NULL) {
	_prolog_bb = Gen_BB_Like (bb);
      }
      BB_Move_Op_To_End (_prolog_bb, bb, op);
    }
    for (op = BB_last_op(bb); op != NULL; op = prev_op) {
      prev_op = OP_prev(op);
      if (!OP_copy(op) && !OP_glue(op) && !OP_no_move_before_gra(op) && !OP_access_reg_bank(op)) {
	// check for glue copies before a branch also.
	if (!OP_xfer(op) || 
	    prev_op == NULL || 
	    (!OP_copy(prev_op) && !OP_glue(prev_op) && !OP_no_move_before_gra(op)))
	break;
      }
      if (_epilog_bb == NULL) {
	_epilog_bb = Gen_BB_Like (bb);
      }
      BB_Move_Op_To_Start (_epilog_bb, bb, op);

      // PRE-GCM can sometimes fill the delay slot with a copy op. In such
      // instances, need to move the branch as well so as to avoid later
      // filling of its delay slot.
      if (prev_op && OP_xfer(prev_op))
        BB_Move_Op_To_Start (_epilog_bb, bb, prev_op);
    }
  }
  else {

    // Alloca instructions can't be reordered.
    for (op = BB_first_op(bb); op != NULL; op = next_op) {
      if (!OP_side_effects(op)) break;
      next_op = OP_next(op);
      if (_prolog_bb == NULL) {
	_prolog_bb = Gen_BB_Like (bb);
      }
      BB_Move_Op_To_End (_prolog_bb, bb, op);
    }

    // Special case handling for regions.
    if (BB_rid(bb) != NULL && RID_cginfo(BB_rid(bb)) != NULL) {
      CGRIN *cgrin = RID_cginfo(BB_rid(bb));
      if (CGRIN_first_bb(cgrin) != NULL && 
	  BB_next(CGRIN_first_bb(cgrin)) == NULL) {
	/* only 1 bb */
	/* make sure pregtns don't get moved, even after regalloc */
	for (op = BB_first_op(bb); op != NULL; op = next_op) {
	  if (OP_code(op) != TOP_begin_pregtn) break;
	  next_op = OP_next(op);
	  if (_prolog_bb == NULL) {
	    _prolog_bb = Gen_BB_Like (bb);
	  }
	  BB_Move_Op_To_End (_prolog_bb, bb, op);
	}
      
	for (op = BB_last_op(bb); op != NULL; op = prev_op) {
	  prev_op = OP_prev(op);
	  if (OP_code(op) != TOP_end_pregtn) {
	    // check for glue copies before a branch also.
	    if (!OP_xfer(op) || prev_op == NULL || 
		(OP_code(prev_op) != TOP_end_pregtn))
	      break;
	  }
	  if (_epilog_bb == NULL) {
	    _epilog_bb = Gen_BB_Like (bb);
	  }
	  BB_Move_Op_To_Start (_epilog_bb, bb, op);
	}
      }
    }
  }
}

// ======================================================================
// ::Invoke_Pre_HBB_Phase
// All the scheduling preparatory stuff for hyperblocks (eg. separating out 
// the special instructions to prolog, epilog blocks) should be done here,
// before the actual scheduling begins. The assumption is that this needs
// to be done only for the entry/exit blocks in the hyperblock.
// ===================================================================
void
HB_Schedule::Invoke_Pre_HBB_Phase(std::list<BB*> bblist)
{

  std::list<BB*>::iterator bb_iter;
  std::list<BB*>::reverse_iterator bb_riter;

  bb_iter = bblist.begin();
  bb_riter = bblist.rbegin();

  BB *first_bb = *bb_iter; BB *last_bb = *bb_riter;

  OP *op, *prev_op;
  OP *next_op;

  // When we are scheduling before register allocation, we don't want 
  // to schedule SP adjustment OPs in the entry/exit blocks.
  // We check for them and move them away to temporary basic 
  // blocks. After we are done scheduling the remaining OPs, we merge the 
  // moved instructions back.

  if (HBS_Before_LRA()) {
    if (BB_entry(first_bb)) {
      _prolog_bb = Gen_BB_Like (first_bb);
      for (op = BB_entry_sp_adj_op (first_bb); op != NULL; op = prev_op) {
        prev_op = OP_prev(op);
        BB_Move_Op_To_Start (_prolog_bb, first_bb, op);
      }
    }
    if (BB_exit(last_bb)) {
      _epilog_bb = Gen_BB_Like (last_bb);
      for (op = BB_exit_sp_adj_op (last_bb); op != NULL; op = next_op) {
	next_op = OP_next(op);
	BB_Move_Op_To_End (_epilog_bb, last_bb, op);
      }
    }
  }

  for (op = BB_first_op(first_bb); op != NULL; op = next_op) {
    if (!OP_side_effects(op)) break;
    next_op = OP_next(op);
    if (_prolog_bb == NULL) {
      _prolog_bb = Gen_BB_Like (first_bb);
    }
    BB_Move_Op_To_End (_prolog_bb, first_bb, op);
  }
}

void
HB_Schedule::Invoke_Post_HBS_Phase(BB* bb)
{

  // If we had moved aside any instructions in the prolog or epilog of 
  // the bb, put them back in.
  if (_prolog_bb != NULL) {
    BB_Prepend_All (bb, _prolog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (bb);
  }

  if (_epilog_bb != NULL) {
    BB_Append_All(bb, _epilog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (bb);
  }

}

void
HB_Schedule::Invoke_Post_HBB_Phase(std::list<BB*> bblist)
{

  std::list<BB*>::iterator bb_iter;
  std::list<BB*>::reverse_iterator bb_riter;

  bb_iter = bblist.begin();
  bb_riter = bblist.rbegin();

  BB *first_bb = *bb_iter; BB *last_bb = *bb_riter;

  // If we had moved aside any instructions in the prolog or epilog of 
  // the bb, put them back in.
  if (_prolog_bb != NULL) {
    BB_Prepend_All (first_bb, _prolog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (first_bb);
  }

  if (_epilog_bb != NULL) {
    BB_Append_All(last_bb, _epilog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (last_bb);
  }

}

INT 
HB_Schedule::Calculate_Etime(OP *op) 
{
  OPSCH *opsch = OP_opsch(op, _hb_map);
  return OPSCH_estart(opsch);
}

INT 
HB_Schedule::Calculate_Ltime(OP *op) 
{
  OPSCH *opsch = OP_opsch(op, _hb_map);
  return OPSCH_lstart(opsch);
}

BOOL
HB_Schedule::Can_Schedule_Op (OP *cur_op, INT cur_time)
{
  if (Check_Resource_Usage (cur_op, cur_time)) return TRUE;
  
  return FALSE;
}


void
HB_Schedule::Schedule_Block (BB *bb, BBSCH *bbsch, int scheduling_algorithm)
{
  _sched_vector = VECTOR_Init (BB_length(bb), &_hb_pool);

#ifdef KEY
  _scheduled_opschs = NULL;
  if (HBS_Balance_Unsched_Types() ||
      HBS_Drop_Unsched_Prefetches()) {
    _scheduled_opschs = OPSCH_SET_Create_Empty(BB_length(bb), &_hb_pool);
  }
#endif

  std::list<BB*> blocks;
  blocks.push_back(bb);

#if defined (TARG_X8664) || defined(TARG_LOONGSON)
  Is_True(scheduling_algorithm == 0 || scheduling_algorithm == 1,
    	  ("Schedule_Block: illegal scheduling_algorithm"));
  Init_RFlag_Table( blocks, scheduling_algorithm);
#elif defined (TARG_SL)
  const BOOL org_LOCS_Fwd_Scheduling = LOCS_Fwd_Scheduling;
  if( _hbs_type & HBS_MINIMIZE_REGS ){
    LOCS_Fwd_Scheduling = FALSE;
  }
  Init_RFlag_Table( blocks, LOCS_Fwd_Scheduling );
#elif defined (TARG_IA64)
  Init_RFlag_Table (blocks, TRUE);
#else
  Init_RFlag_Table (blocks, FALSE);
#endif

  Compute_OPSCH(bb, _hb_map, &_hb_pool,
		HBS_Balance_Ready_Types() || HBS_Balance_Unsched_Types(),
		scheduling_algorithm /* 1 is fwd */);

  // Always init, but only use the pressure heuristics with the flags
  Init_Register_Map (bb);

  Priority_Selector *priority_fn;
  Cycle_Selector *cycle_fn;

#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_LOONGSON)
#if defined(TARG_SL) || defined(TARG_MIPS)
  if( LOCS_Fwd_Scheduling ){
#else
  if (scheduling_algorithm == 1) {
#endif
    priority_fn = CXX_NEW( List_Based_Fwd(bb, this, _hbs_type, &_hb_pool),
			   &_hb_pool );
    cycle_fn = CXX_NEW( Fwd_Cycle_Sel(), &_hb_pool );

  } else {
    priority_fn = CXX_NEW( List_Based_Bkwd(bb, this, _hbs_type, &_hb_pool),
			   &_hb_pool );
    cycle_fn = CXX_NEW( Bkwd_Cycle_Sel(), &_hb_pool );
  }
#else // TARG_X8664 || TARG_SL || TARG_MIPS || TARG_LOONGSON
#ifndef TARG_IA64
  if( LOCS_Scheduling_Algorithm == 1 ){
    priority_fn = CXX_NEW( List_Based_Fwd(bb, this, _hbs_type, &_hb_pool),
                           &_hb_pool );
    cycle_fn = CXX_NEW( Fwd_Cycle_Sel(), &_hb_pool );

  } else {
    priority_fn = CXX_NEW( List_Based_Bkwd(bb, this, _hbs_type, &_hb_pool),
                           &_hb_pool );
    cycle_fn = CXX_NEW( Bkwd_Cycle_Sel(), &_hb_pool );
  }
#else
  // Do forward scheduling and cycle selector.
  priority_fn = 
    CXX_NEW(List_Based_Fwd(bb, this, _hbs_type, &_hb_pool), &_hb_pool);
  cycle_fn = CXX_NEW(Fwd_Cycle_Sel(), &_hb_pool);
#endif
#endif



  OP *cur_op;
  OP *xfer_op = BB_xfer_op(bb);

  // If backward schedule and the basic block ends in a control transfer,
  // try to schedule the delay slot (if present) first and then schedule
  // the xfer_op.

  if (!priority_fn->Is_Fwd_Schedule()) {
    if (xfer_op) {
      if (PROC_has_branch_delay_slot()) {
	cur_op = priority_fn->Select_OP_For_Delay_Slot (xfer_op);
	if (cur_op) {
	  Add_OP_To_Sched_Vector(cur_op, priority_fn->Is_Fwd_Schedule());
#if defined(TARG_MIPS) || defined(TARG_LOONGSON) 
	  // Schedule the branch and the delay slot OP in different cycles.
	  Clock--;
	  OPSCH *xfer_opsch = OP_opsch(xfer_op, _hb_map);
	  OPSCH_scycle(xfer_opsch) = MIN(Clock, OPSCH_scycle(xfer_opsch));
#endif
	}
      } 
      Add_OP_To_Sched_Vector(xfer_op, priority_fn->Is_Fwd_Schedule());
    }
  }

  INT cur_time;
  // Now iterate through the rest of the ops.
  for (cur_op = (OP*) priority_fn->Get_Next_Element(this); cur_op != NULL; 
       cur_op = (OP*) priority_fn->Get_Next_Element(this)) {

    if (!OP_dummy(cur_op)) {
      INT etime = Calculate_Etime(cur_op);
      INT ltime = Calculate_Ltime(cur_op);
      cycle_fn->Init(cur_op, etime, ltime);

      for (cur_time = cycle_fn->Get_Cycle(); cur_time >= cycle_fn->Bound(); 
	   cur_time = cycle_fn->Next_Cycle()) {
	if (Can_Schedule_Op(cur_op, cur_time)) break;
      }
    }

    //    Is_True(cur_time < cycle_fn->Bound(),("Invalid cycle boundary, HB_SCHED"));

    // bug fixed for 15264. After this loop, the process of Inserting the scheduled list
    // assume that "Clock" be the smallest scycle of all the scheduled op in this bb. But Clock
    // may not be when if the first few ops are independency. So use min_clock to record the 
    // smallest scycle. 
    INT min_clock;
    min_clock = Clock;
    Add_OP_To_Sched_Vector(cur_op, priority_fn->Is_Fwd_Schedule());
    if (! priority_fn->Is_Fwd_Schedule())
      Clock = MIN(min_clock, Clock);
  }
    
  if( BB_length(bb) != VECTOR_count(_sched_vector) ){
    if( Trace_HB )
      Print_BB_For_HB( bb, hb_map() );
    FmtAssert( false, ("Some ops are not scheduled yet") );
  }

  // Insert the scheduled list of instructions into the bb.
  Put_Sched_Vector_Into_BB (bb, bbsch, priority_fn->Is_Fwd_Schedule() );
#if defined (TARG_SL)
  LOCS_Fwd_Scheduling = org_LOCS_Fwd_Scheduling;
#endif
  // cycle_fn will be deleted when _hb_pool is freed, while priority_fn used malloc pool
  // to initialize its memeber _curbb_list, explicitly delete it to aviod memory leak.
  CXX_DELETE(priority_fn, &_hb_pool);
}

void
HB_Schedule::Schedule_Blocks (std::list<BB*>& bblist)
{
  std::list<BB*>::iterator bbi;
  UINT32 length = 0;

  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    length += BB_length(*bbi);
  }

  _sched_vector = VECTOR_Init (length, &_hb_pool);

#if defined (TARG_X8664)
  _scheduled_opschs = NULL;
  if (HBS_Balance_Unsched_Types() ||
      HBS_Drop_Unsched_Prefetches()) {
    _scheduled_opschs = OPSCH_SET_Create_Empty(length, &_hb_pool);
  }
#else
  _scheduled_opschs = (_hbs_type & HBS_BALANCE_UNSCHED_TYPES) ?
			OPSCH_SET_Create_Empty(length, &_hb_pool) : NULL;
#endif

  Init_RFlag_Table (bblist, TRUE);
  Compute_OPSCHs (bblist, _hb_map, &_hb_pool,
		  HBS_Balance_Ready_Types() || HBS_Balance_Unsched_Types(),
		  TRUE /* fwd */);


  /* TODO: Need to model register usages for hyperblocks soon. 
  if (Cur_Locs_Type & HBS_MINIMIZE_REGS) {
    Init_Register_Map (bb);
  } */

  List_Based_Fwd *priority_fn = 
    CXX_NEW(List_Based_Fwd(bblist, this, _hbs_type, &_hb_pool), &_hb_pool);

  Fwd_Cycle_Sel *cycle_fn = CXX_NEW(Fwd_Cycle_Sel(), &_hb_pool);

  OP *cur_op;
  INT cur_time;

  // Now iterate through the rest of the ops.
  for (cur_op = (OP*) priority_fn->Get_Next_Element(this); cur_op != NULL; 
       cur_op = (OP*) priority_fn->Get_Next_Element(this)) {

    if (!OP_dummy(cur_op)) {
      INT etime = Calculate_Etime(cur_op);
      INT ltime = Calculate_Ltime(cur_op);
      cycle_fn->Init(cur_op, etime, ltime);

      for (cur_time = cycle_fn->Get_Cycle(); cur_time != cycle_fn->Bound(); 
	   cur_time = cycle_fn->Next_Cycle()) {

	if (Can_Schedule_Op(cur_op, cur_time)) break;
      }
    }

    // Is_True(cur_time >= cycle_fn->Bound(), ("Invalid cycle boundary, HB_SCHED"));

    Add_OP_To_Sched_Vector(cur_op, priority_fn->Is_Fwd_Schedule());
  }
    
  // Insert the scheduled list of instructions into the bb.
  Put_Sched_Vector_Into_HB (bblist);

}

// ======================================================================
// HB_Schedule
//
// Algorithm:
//
//    1. Build a list of OPs that is ready to be scheduled. This is
//       the list of OPs with no successors in the dependence graph.
//    2. Select one of the OPs from the ready list based on some
//       heuristics as the next OP to schedule.
//    3. Delete the scheduled OP from the ready list and add any new
//       ones that might be ready now.
//    4. Repeat steps 2. and 3. till all OPs have been scheduled.
//    5. Put the scheduled list of OPs back into the basic block.
// ======================================================================

HB_Schedule::HB_Schedule()
{
  _prolog_bb = NULL;
  _epilog_bb = NULL;

  // Initialize memory pool for use in the scheduling this bb.
  MEM_POOL_Initialize (&_hb_pool, "HB_pool", FALSE);
  MEM_POOL_Initialize (&_hb_map_pool, "HB_map_pool", FALSE);
  MEM_POOL_Push(&_hb_pool);
  MEM_POOL_Push (&_hb_map_pool);

  _hb_map = BB_MAP_Create ();
  Trace_HB = Get_Trace (TP_SCHED, 1);
}

void
HB_Schedule::Init(BB *bb, HBS_TYPE hbs_type, INT32 max_sched,
		  BBSCH *bbsch, mINT8 *regs_avail)
{
  _hbs_type = hbs_type;
  _max_sched = max_sched;
  if (regs_avail) {
    for (INT i = ISA_REGISTER_CLASS_MIN;i <= ISA_REGISTER_CLASS_MAX; i++)   
      _Cur_Regs_Avail[i] = regs_avail[i];
  }

  BB_OP_MAP omap = BB_OP_MAP_Create(bb, &_hb_map_pool);
  BB_MAP_Set(_hb_map, bb, omap);

  _ready_vector = VECTOR_Init (BB_length(bb), &_hb_pool);

#ifdef KEY
  _one_set_counter = 0;
#endif
}

void
HB_Schedule::Init(std::list<BB*> bblist, HBS_TYPE hbs_type, mINT8 *regs_avail)
{
  _hbs_type = hbs_type;
  if (regs_avail) {
    for (INT i = ISA_REGISTER_CLASS_MIN;i <= ISA_REGISTER_CLASS_MAX; i++)   
      _Cur_Regs_Avail[i] = regs_avail[i];
  }

  UINT32 length = 0;
  std::list<BB*>::iterator bbi;

  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    BB_OP_MAP omap = BB_OP_MAP_Create(*bbi, &_hb_map_pool);
    BB_MAP_Set(_hb_map, *bbi, omap);
    length += BB_length(*bbi);
  }

  _ready_vector = VECTOR_Init (length, &_hb_pool);

#ifdef KEY
  _one_set_counter = 0;
#endif
}

void
HB_Schedule::Schedule_BB (BB *bb, BBSCH *bbsch, int scheduling_algorithm)
{
#if defined(TARG_X8664)
  bool clear_minregs_flag = false;
#endif

  /* In some cases, the bb is an empty one */
  if( BB_length(bb) == 0 )
    return;

#if defined(TARG_X8664)
  // Experiment: in prescheduling schedule for reg pressure for unrolled loops
  if (HBS_Before_GRA() && HBS_Before_LRA() && 
      HBS_Depth_First() && !HBS_Minimize_Regs() && 
      ((BB_unrollings(bb) && LOCS_PRE_Enable_Unroll_RegPressure_Sched) || 
       (LOCS_PRE_Enable_General_RegPressure_Sched))) {
    clear_minregs_flag = true;
    _hbs_type |= HBS_MINIMIZE_REGS;
  }
#endif

  Invoke_Pre_HBS_Phase(bb);

  std::list<BB*> bblist;
  bblist.push_back(bb);

  if (CG_DEP_Prune_Dependence &&  // if the flag is turned ON.
      CGTARG_Can_Predicate() &&
      !BB_predicate_promote(bb))   // if the target arch provides predication.
    {
      CG_DEP_Prune_Dependence_Arcs(bblist, PRUNE_PREDICATE_ARCS, FALSE);
      Set_BB_predicate_promote(bb);
    }

  // if there are only zero or one instructions in the basic block,
  // there is nothing to schedule.
  BOOL skip_sched = FALSE;;
  if (CG_skip_local_sched) {
    BBs_Processed++;
    skip_sched =  (BBs_Processed < CG_local_skip_before ||
		   BBs_Processed > CG_local_skip_after ||
		   BBs_Processed == CG_local_skip_equal);
    if (skip_sched)
      fprintf (TFile, "[%d] BB:%d was skipped in HB_Schedule_BB\n", 
	BBs_Processed, BB_id(bb));
    if (!skip_sched)
      fprintf (TFile, "[%d] BB:%d processed in HB_Schedule_BB\n", 
	BBs_Processed, BB_id(bb));
  }

#ifdef KEY
  // -1 means caller didn't specify scheduling algorithm.  Use the
  // LOCS_Scheduling_Algorithm value.
  if (scheduling_algorithm == -1)
    scheduling_algorithm = LOCS_Scheduling_Algorithm;

  if (_hbs_type & HBS_MINIMIZE_REGS ||
      // If scheduling for pre/post-register allocation, scheduling_algorithm
      // will always be 0 or 1.  If scheduling for some other purpose (such as
      // GCM), it can take on other values.  In that case, just set it to 0.
      scheduling_algorithm > 1) {
    scheduling_algorithm = 0;		// backward scheduling
  }
#endif

  if (BB_length(bb) > 0 && !skip_sched) {
    if (BB_length(bb) > 1) {

      CG_DEP_Compute_Graph (
	  bb, 
	  (this->HBS_From_CGPREP()) ? NO_ASSIGNED_REG_DEPS : 
	                               INCLUDE_ASSIGNED_REG_DEPS,
	  NON_CYCLIC,
	  INCLUDE_MEMREAD_ARCS,
	  INCLUDE_MEMIN_ARCS,
#ifdef TARG_IA64
	  Is_Target_Itanium() ? INCLUDE_CONTROL_ARCS : NO_CONTROL_ARCS,
#else
#if defined(TARG_LOONGSON) || defined(TARG_MIPS) && !defined(TARG_SL)
	  // Backward scheduling performs delay slot filling.  Don't add
	  // control flow dependencies because they eliminate all delay slot
	  // candidates.  Bug 11943.
	  scheduling_algorithm == 0 ? NO_CONTROL_ARCS : INCLUDE_CONTROL_ARCS,
#else
	  INCLUDE_CONTROL_ARCS,
#endif
#endif
	  NULL);

      if (Trace_HB) CG_DEP_Trace_Graph (bb);

      Schedule_Block (bb, bbsch, scheduling_algorithm);

      CG_DEP_Delete_Graph (bb);
    }
    Set_BB_scheduled (bb);
    Set_BB_scheduled_hbs (bb);  // scheduled from hbs
    if (Assembly) Add_Scheduling_Note (bb, (void*) bbsch);
  }
  
  Invoke_Post_HBS_Phase(bb);

  // If we are scheduling before GRA, compute an estimate of the registers
  // that will be needed by LRA. No need to compute register request, if
  // HBS is invoked from CGPREP or for the second time within GCM. We model
  // the local register usage counts separately.

  if (HBS_Before_GRA() && !HBS_From_CGPREP()) {

    if (!Get_Trace (TP_SCHED, 0x2000)) {

      // Assumes that <bbsch> is computed fopass      
      mINT8 *fatpoint = (_hbs_type & HBS_FROM_PRE_GCM_SCHED_AGAIN) ?
      BBSCH_local_regcost(bbsch) : LRA_Compute_Register_Request(bb, &_hb_pool);
      if (HBS_From_Pre_GCM_Sched()) {
        Set_BB_local_regcost(bbsch, fatpoint);
      }
    }
  }

#if defined(TARG_X8664)
  if (clear_minregs_flag) {
    _hbs_type &= ~HBS_MINIMIZE_REGS;
  }
#endif
}

void
HB_Schedule::Schedule_HB (std::list<BB*> bblist)
{

  Invoke_Pre_HBB_Phase(bblist);

  if (CG_DEP_Prune_Dependence &&  // if the flag is turned ON.
      CGTARG_Can_Predicate())   // if the target arch provides predication.
    {
      CG_DEP_Prune_Dependence_Arcs(bblist, PRUNE_PREDICATE_ARCS, FALSE);
    }

  CG_DEP_Compute_Region_Graph(bblist, 
			      (this->HBS_From_CGPREP()) ? 
			      NO_ASSIGNED_REG_DEPS : INCLUDE_ASSIGNED_REG_DEPS,
			      INCLUDE_MEMREAD_ARCS,
			      INCLUDE_CONTROL_ARCS);

  Schedule_Blocks (bblist);

  CG_DEP_Delete_Graph (&bblist);

  Invoke_Post_HBB_Phase(bblist);
  std::list<BB*>::iterator bbi;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    Set_BB_scheduled (*bbi);
    Set_BB_scheduled_hbs (*bbi);  // scheduled from hbs
  }

  // if (Assembly) Add_Scheduling_Note (bb, (void*) bbsch);
  
}

HB_Schedule::~HB_Schedule()
{
  BB_MAP_Delete (_hb_map);

  MEM_POOL_Pop (&_hb_pool);
  MEM_POOL_Pop (&_hb_map_pool);
  MEM_POOL_Delete (&_hb_pool);
  MEM_POOL_Delete (&_hb_map_pool);
}
