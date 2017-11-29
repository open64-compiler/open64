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



/* =======================================================================
 * =======================================================================
 *
 *  Module: gcm.cxx
 *  $Revision: 1.34 $
 *  $Date: 05/12/05 08:59:06-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.gcm.cxx $
 *
 *  Description:
 *  ============
 *
 *  Global Code Motion (GCM)
 *
 * =======================================================================
 * =======================================================================
 */

#include <stdint.h>
#include <alloca.h>
#include "defs.h"
#include "config.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "cg.h"
#include "cg_flags.h"
#include "cgprep.h"
#include "ttype.h"
#include "targ_sim.h"
#include "bb.h"
#include "variants.h"
#include "bitset.h"
#include "bb_set.h"
#include "freq.h"
#include "cgtarget.h"
#include "cxx_memory.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "cg_vector.h"
#include "hb_sched.h"
#include "reg_live.h"
#include "gcm.h"
#if defined(TARG_SL)
#include "gcm_licm.h"
#include "loop_dce.h"
#include "loop_rce.h"
#endif
#include "glob.h"
#include "cflow.h"
#include "tn_set.h"
#include "cgemit.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "gra_live.h"
#include "tn_map.h"
#include "cg_sched_est.h"
#include "cg_loop.h"
#include "pf_cg.h"
#include "targ_proc_properties.h"
#include "tag.h"
#ifdef TARG_X8664
#include "lra.h"
#endif

/* This is for the pair struct */
#include <utility>
#include <map>

// cgdriver flags variabes 
BOOL GCM_POST_Spec_Loads = TRUE;
BOOL GCM_PRE_Spec_Loads = TRUE;
#if defined(TARG_SL)
BOOL GCM_Use_Sched_Est = TRUE;
#else
BOOL GCM_Use_Sched_Est = FALSE;
#endif
BOOL GCM_Forw_Circ_Motion = TRUE;
BOOL GCM_POST_Force_Scheduling = FALSE;
BOOL CG_Skip_GCM = FALSE;
INT32 GCM_From_BB = -1;
INT32 GCM_To_BB = -1;
INT32 GCM_Result_TN = -1;
#ifdef KEY
INT32 GCM_BB_Limit = -1;
INT32 cumulative_cand_bb;
#endif

// Internal state flags to store speculative information
#define SPEC_NONE		0x00	// no speculative motion
#define SPEC_EAGER_PTR		0x01	// eager ptr speculation
#define SPEC_EAGER_NULL_PTR	0x02	// eager null ptr speculation
#define SPEC_CIRC_PTR_ABOVE     0x04    // circluar ptr speculation (above)
#define SPEC_CSAFE_PTR          0x08    // safe control speculation 
#define SPEC_DSAFE_PTR          0x10    // safe data speculation
#define SPEC_CDSAFE_PTR         (SPEC_CSAFE_PTR | SPEC_DSAFE_PTR) // safe control (and data) speculation
#define SPEC_PSAFE_PTR          0x20    // safe predicate promotion

#define EAGER_NONE(o)			((o) & SPEC_NONE)
#define Set_EAGER_NONE(o)		((o) |= SPEC_NONE)
#define Reset_EAGER_NONE(o) 		((o) &= ~SPEC_NONE)
#define EAGER_PTR_SPEC(o)		((o) & SPEC_EAGER_PTR)
#define Set_EAGER_PTR_SPEC(o)		((o) |= SPEC_EAGER_PTR)
#define Reset_EAGER_PTR_SPEC(o)		((o) &= ~SPEC_EAGER_PTR)
#define EAGER_NULL_PTR_SPEC(o)		((o) & SPEC_EAGER_NULL_PTR)
#define Set_EAGER_NULL_PTR_SPEC(o)	((o) |= SPEC_EAGER_NULL_PTR)
#define Reset_EAGER_NULL_PTR_SPEC(o)	((o) &= ~SPEC_EAGER_NULL_PTR)
#define CIRC_PTR_SPEC(o)		((o) & SPEC_CIRC_PTR_ABOVE)
#define Set_CIRC_PTR_SPEC(o)	        ((o) |= SPEC_CIRC_PTR_ABOVE)
#define Reset_CIRC_PTR_SPEC(o)	        ((o) &= ~SPEC_CIRC_PTR_ABOVE)
#define CSAFE_PTR_SPEC(o)		((o) & SPEC_CSAFE_PTR)
#define Set_CSAFE_PTR_SPEC(o)	        ((o) |= SPEC_CSAFE_PTR)
#define Reset_CSAFE_PTR_SPEC(o)	        ((o) &= ~SPEC_CSAFE_PTR)
#define DSAFE_PTR_SPEC(o)		((o) & SPEC_DSAFE_PTR)
#define Set_DSAFE_PTR_SPEC(o)	        ((o) |= SPEC_DSAFE_PTR)
#define Reset_DSAFE_PTR_SPEC(o)	        ((o) &= ~SPEC_DSAFE_PTR)
#define CDSAFE_PTR_SPEC(o)		((o) & SPEC_CDSAFE_PTR)
#define Set_CDSAFE_PTR_SPEC(o)	        ((o) |= SPEC_CDSAFE_PTR)
#define Reset_CDSAFE_PTR_SPEC(o)        ((o) &= ~SPEC_CDSAFE_PTR)
#define PSAFE_PTR_SPEC(o)		((o) & SPEC_PSAFE_PTR)
#define Set_PSAFE_PTR_SPEC(o)	        ((o) |= SPEC_PSAFE_PTR)
#define Reset_PSAFE_PTR_SPEC(o)         ((o) &= ~SPEC_PSAFE_PTR)

// control the number of loops processed
INT32 loop_id;

/* TODO: change speculation ratio with a compile time option */
static float speculation_ratio_wfb = 0.35;
static float speculation_ratio_fb = 0.75;

// static variables to keep track of state while performing the GCM phase, includes tracing
// flags as well.
static BOOL Trace_GCM = FALSE;
static BOOL Trace_GCM_Reg_Usage = FALSE;
static BOOL Trace_Fill_Delay_Slots = FALSE;
static BOOL GCM_Internal_Flag = TRUE; // internal flag for extraneous .T dumps
static BOOL Trace_GCM_Dump_IR = FALSE;
static BOOL Trace_GCM_Dump_Preprocess = FALSE;
static BOOL Trace_GCM_Merge_BBs = FALSE;
static BB* GCM_Loop_Prolog; // internal variable to keep track of loop prologue bbs
// static BOOL GCM_PRE_Pass_Enabled = FALSE; // flag enabled if pre_gcm invoked.

static INT32 mispredict, fixed, taken;
static double times;

static BB_MAP bbsch_map;

// Memory pool for LOOP_DESCR 
static MEM_POOL loop_descr_pool;

// Memory pool for BB dom/pdom sets
static MEM_POOL gcm_loop_pool;

static HBS_TYPE cur_hbs_type;	// for saving GCM_Schedule_Region's parameter

// Map and accessors to associate a set of cycles with each BB.
// A loop may contain more than one cycle, so we assign each cycle
// an ID. Then for each BB, we create a set of cycle IDs which identifies
// the cycles that contains that BB.
//
// If a loop has only one cycle we don't create these sets.
// Therefore if the map entry for a BB is NULL, the BB is either not in
// a loop or in only one cycle. Also, if a BB has a map entry than it's
// cycle set is non-empty.
static BB_MAP bb_cycle_set_map;

#define BB_cycle_set(bb)	((BS *)BB_MAP_Get(bb_cycle_set_map, (bb)))
#define Set_BB_cycle_set(bb, bs) (BB_MAP_Set(bb_cycle_set_map, (bb), (bs)))

// This variable is set to TRUE if we want to run cflow again due to
// branch delay-slot filling creating empty basic blocks.
static BOOL Run_Cflow_Delay;
// This variable is set to TRUE if we want to run cflow again after GCM
BOOL Run_Cflow_GCM;
GCM_TYPE Cur_Gcm_Type;

static BOOL Ignore_TN_Dep; // to identify if TN dependences can be ignored
static INT cur_pc = 0; // to hold the pc- value

#if defined(TARG_SL)
// some binary search interfaces
extern BOOL Should_Skip( int, int , int , int );
static bool GCM_Skip_Loop_Binary_Search( int );
static BOOL GCM_Skip_Op_Binary_Search( int );

// LICM binary search on PU level needs to be done by GCM
static BOOL GCM_LICM_Skip_Loop_Binary_Search( int );
static BOOL GCM_LICM_Skip_Op_Binary_Search( int );

// DCE binary search on PU level needs to be done by GCM
static BOOL LOOP_DCE_Skip_Loop_Binary_Search( int );
BOOL LOOP_DCE_Skip_Op_Binary_Search( int );

static BOOL GCM_Skip_Op( OP*, BB*, OP_LIST* );
static void Preprocess_Loop_Head( BB* );
static void Find_Last_Defs_Of_Branch( BB* );
BB* Loop_Is_Zdl( LOOP_DESCR *loop );
static BOOL Loop_Is_Straight_Line( LOOP_DESCR *loop );

// GCM_Merge_Smalll_BBs
static void GCM_Merge_Small_BBs( LOOP_DESCR *loop, MEM_POOL *pool );

static OP_LIST *moved_loads;
static std::vector< std::pair<OP*, OP*> > load_add_pairs;
static std::vector<OP*> last_defs_of_branch;

/* the left un-moved OPs after circular motion. We cannot use OPS to record
 * left_ops, since operations on OPS will change the properties of OP
 */
static std::vector<OP*> left_ops; 
// use mvtc_targets to record the moving locations of mvtc.i of each loop
typedef struct{
  LOOP_DESCR * orig_loop;   // loop where mvtc resides in originally
  OP * mvtc;                // 
  BB * host_bb;             // the BB where mvtc moved to
}Mvtc_Pos;
static std::vector<Mvtc_Pos> mvtc_targets;
static std::vector< LOOP_DESCR* > postprocessed_loops;

/* since circ_above will duplicate OPs, limit the total moves */
static INT32 circ_above_limit = 50;
static INT32 circ_above_moves = 0;

static float circ_above_tc_limit = 10;

/* In gcm, some local TN will be promoted to global TN, this will increase
 * live range, and add possiblity of splling in RA. So I add a heuristic
 * This limit is only for circ_above motion, since the extra spill in loop
 * is serious.
 */
static INT32 local_tn_promotion_limit = 19;
static TN_SET *promoted_tns = NULL;

#define OP_is_loop(o) (OP_code(o)==TOP_loop)
#define OP_is_mvtc(o) ( (OP_code(o)==TOP_mvtc_i) || (OP_code(o)==TOP_mvtc) )

// if we already got a barrier OP before
static BOOL barriered = FALSE;

#endif

// Sort routines must return negative/0/positive according to qsort.
// =======================================================================
// Sort_by_bb_frequency
// returns TRUE if bb1 has higher frequency estimate than bb2.
// =======================================================================
static INT
sort_by_bb_frequency (const void *bb1, const void *bb2)
{
#ifdef KEY
  const BB* A = *(BB**)bb1;  
  const BB* B = *(BB**)bb2;

  if( BB_freq(A) > BB_freq(B) )
    return -1;
  if( BB_freq(A) < BB_freq(B) )
    return 1;

  return BB_id(A) < BB_id(B) ? -1 : 1;

#else
  if (BB_freq((BB *)bb1) > BB_freq((BB *)bb2)) return 1;
  else if (BB_freq((BB *)bb1) < BB_freq((BB *)bb2)) return -1;  
  else return 0;
#endif
}

// =======================================================================
// Sort_by_edge_probability
// returns TRUE if edge bl1 has higher probability
// estimate than bl2. For this to be meaningful, the two edges must be
// out of the same BB.
// =======================================================================
static INT
sort_by_edge_probability (const void *bl1, const void *bl2)
{
  if (BBLIST_prob((BBLIST *)bl1) > BBLIST_prob((BBLIST *)bl2)) return 1;
  else if (BBLIST_prob((BBLIST *)bl1) < BBLIST_prob((BBLIST *)bl2)) return -1;
  else return 0;
}

// =======================================================================
// Sort_by_bb_live_in
// returns TRUE if the number of live variables (weighted by the frequency)
// from bb1 are more than that of bb2. 
// =======================================================================
static INT
sort_by_bb_live_in (const void *bb1, const void *bb2)
{
  UINT8 bb1_count = 0, bb2_count = 0;

  TN* x;
  FOR_ALL_GTN_SET_members(BB_live_in((BB *)bb1), x) bb1_count++;
  FOR_ALL_GTN_SET_members(BB_live_in((BB *)bb2), x) bb2_count++;

  if ((BB_freq((BB *)bb1) * bb1_count) > (BB_freq((BB *)bb2) * bb2_count)) return 1;
  else if ((BB_freq((BB *)bb1) * bb1_count) < (BB_freq((BB *)bb2) * bb2_count)) return -1;
  else return 0;
}

#if !defined(TARG_SL)
// =======================================================================
// Is_BB_Empty
// Check if a basic block has any executable instructions. Return TRUE
// if the block is empty.
// =======================================================================
static BOOL
Is_BB_Empty (BB *bb)
{
  for (OP *op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
    if (OP_Real_Ops(op) != 0) return FALSE;
  }
  return TRUE;
}
#endif

// =======================================================================
// Print_Trace_File
// Common Trace routine
// =======================================================================
static void
Print_Trace_File(OP *cand_op, BB *src_bb, BB *cand_bb, BOOL success)
{
  const char *str = (success) ? "SUCCESS" : "FAIL";
  fprintf (TFile, "%s_GCM(%s): MOVE ",Ignore_TN_Dep ? "POST":"PRE",str);
  Print_OP_No_SrcLine (cand_op);
  fprintf (TFile,"	FROM BB:%d => TO BB:%d:\n", 
	   BB_id(src_bb), BB_id(cand_bb));
}

// =======================================================================
// OP_Is_Expensive
// checks to see if <cur_op> is expensive. These ops are long latency
// ops and the benefit of speculating them are minimal. 
// TODO. need to provide framework in local scheduler which will take
// into account dangling latencies (across basic blocks) for this expensive
// ops. This will help prune the cases further. this is a 7.3 affair.
// need to take into account CGTARG_ option as well.
// =======================================================================
static BOOL
OP_Is_Expensive (OP *cur_op)
{
  return CGTARG_Is_Long_Latency(OP_code(cur_op));
}

// =======================================================================
// First_Inst_Of_BB
// Return the first instruction of the basic block <bb>. Return NULL if
// we are unable to find one. 
// =======================================================================
static OP *
First_Inst_Of_BB (BB *bb)
{
  OP *op;

  if (bb == NULL) return NULL;
  FOR_ALL_BB_OPs_FWD (bb, op) {
    if (OP_dummy(op)) continue;
    return op;
  }
  // If the bb has only dummy ops, look at the following bb.
  if (BB_succs(bb) != NULL) {
    BB *succ_bb = BBLIST_item (BB_succs(bb));
    if (BBlist_Len (BB_preds(succ_bb)) == 1) {
      return First_Inst_Of_BB (succ_bb);
    }
  }
  return NULL;
}

inline BOOL within_bounds(INT num1, INT num2, INT lower_bound,INT upper_bound){
  return (((num1 - num2) < upper_bound) && ((num1 - num2) > lower_bound));
}

// =======================================================================
// OP_Offset_Within_Limit
// Checks to see if the offset field of an memory op lies between the
// lower and upper bound
// =======================================================================
static BOOL 
OP_Offset_Within_Limit(OP *mem_op1, OP *mem_op2, INT lower_bound, 
		       INT upper_bound)
{

  INT offset_opnd_num1 = TOP_Find_Operand_Use(OP_code(mem_op1), OU_offset);
  INT offset_opnd_num2 = TOP_Find_Operand_Use(OP_code(mem_op2), OU_offset);

  TN *offset1, *offset2;

  offset1 = (offset_opnd_num1 < 0) ? NULL : OP_opnd(mem_op1, offset_opnd_num1);
  offset2 = (offset_opnd_num2 < 0) ? NULL : OP_opnd(mem_op2, offset_opnd_num2);

  if (offset1 && offset2 && 
      TN_has_value(offset1) && TN_has_value(offset2)) {
    return within_bounds(TN_value(offset1), 
			 TN_value(offset2), 
			 lower_bound, 
			 upper_bound);
  }
   
  return FALSE;
}

// Bug 8298: Split_BB() splits a basic block into blocks that are 
// (approximately) half or 3/4-th the Split_BB_Length value in 
// length each. Hence we do a (/2) - as the more conservative of the
// two divisions. We retain the (- 60).

#define Large_BB(bb, loop) \
        (LOOP_DESCR_nestlevel((loop)) == 0 && \
        BB_length((bb)) >= (Split_BB_Length/2 - 60))

#ifdef TARG_X8664
// =======================================================================
// Return TRUE if GCM should not change BB because it contains OPs involved
// in GOT computation.
// =======================================================================
static BOOL
Avoid_GOT_BB (BB *bb)
{
  OP *first_op = BB_first_op(bb);
  OP *last_op = BB_last_op(bb);

  return (((first_op != NULL) && OP_computes_got(first_op)) ||
	  ((last_op != NULL) && OP_computes_got(last_op)));
}
#endif

// =======================================================================
// Check_If_Ignore_BB
// Placeholder for all compile speed heuristics. If any of heuristics 
// match, return TRUE (i.e. the block is avoided any further processing).
// =======================================================================
static BOOL
Check_If_Ignore_BB(BB *bb, LOOP_DESCR *loop)
{
  // Avoid processing infrequent basic blocks
  if (BB_freq(bb) < 0.02) 
    return TRUE;

  // Avoid processing large blocks which are not part of any loop. We
  // expect that HBS would have come up with a good schedule.
#ifdef TARG_IA64
  if (LOOP_DESCR_nestlevel(loop) == 0 && BB_length(bb) >= (Split_BB_Length - 60))
#else
  if (Large_BB(bb, loop))
#endif
    return TRUE;


#ifdef TARG_X8664
  // Don't mess with GOT computation.  Bug 14452.
  if (Avoid_GOT_BB(bb))
    return TRUE;
#endif

  return FALSE;
}

// =======================================================================
// Macros to set the limit for applying the pointer speculation
// heurtistics (i.e. Eager_Ptr_Deref_Spec and Null_Ptr_Deref_Spec). 
// TODO: fix an appropriate limit.
// =======================================================================

inline BOOL Similar_Ptr_Offset_ok(OP *cur_op, OP *deref_op) {

  INT cur_offset_num = TOP_Find_Operand_Use(OP_code(cur_op), OU_offset);
  INT deref_offset_num = TOP_Find_Operand_Use(OP_code(deref_op), OU_offset);
  INT deref_base_num = TOP_Find_Operand_Use (OP_code(deref_op), OU_base);
  INT cur_base_num = TOP_Find_Operand_Use(OP_code(cur_op), OU_base);

  TN *deref_base_tn = OP_opnd(deref_op, deref_base_num);
  TN *cur_base_tn = OP_opnd(cur_op, cur_base_num);

  if (cur_offset_num < 0 && deref_offset_num < 0) {
    DEF_KIND kind;
    OP *defop = TN_Reaching_Value_At_Op(cur_base_tn, cur_op, &kind, TRUE);
    if (defop && OP_iadd(defop) && kind == VAL_KNOWN) {
      TN *defop_offset_tn = OP_opnd(defop, 1);
      TN *defop_base_tn = OP_opnd(defop, 2);
      if (defop_base_tn == deref_base_tn && TN_has_value(defop_offset_tn))
	return TRUE;
    }
  } else {

    // #669168: Set the legal offset thresholds based on Opt level.
    // at -O2, range is 32 (-16 .. +16)
    // >= O3, range is 128 (-64 .. +64)

    return (CG_opt_level > 2) ? 
      OP_Offset_Within_Limit(cur_op, deref_op, -64, 64) :
      OP_Offset_Within_Limit(cur_op, deref_op, -16, 16);
  }

  return FALSE;
}

#define Null_Ptr_Offset_ok(op)    OP_Offset_Within_Limit(op, -1, 256)

// =======================================================================
// Similar_Ptr_Addrs_Match
// Return TRUE if the <pred_op> and <succ_op> have conflicting addresses
// assumes that <pred_op> and <succ_op> are memory ops
// a more generic routine needs to be used (eg. a generic version of same_addr
// routine in cg_dep_graph) should be used. If I try to use its current
// version, get into all sorts of dep-graph intrinsic problems.
// =======================================================================
static BOOL
Similar_Ptr_Addrs_Match (OP *pred_op, OP *succ_op)
{

#ifdef TARG_SL
  // C3 load/store has no way to get OU_base/OU_offset. This makes
  // compiler hard to write. So far I just make them dependent.
  if( TOP_is_c3_load(OP_code(pred_op)) ||
      TOP_is_c3_store(OP_code(pred_op))||
      TOP_is_c3_load(OP_code(succ_op)) ||
      TOP_is_c3_store(OP_code(succ_op)) ){
    return TRUE;
  }

  /* one is sl2 load/store, the other is non-sl2 load/store, then
   * they are definitely non-conflicting, otherwise, we just return
   * TRUE conservely.
   */
  if( TOP_is_c2_load(OP_code(pred_op)) || 
      TOP_is_c2_store(OP_code(pred_op)) ) {
    if( !TOP_is_c2_load(OP_code(succ_op)) &&
        !TOP_is_c2_store(OP_code(succ_op)) ) 
      return FALSE;
    else
      return TRUE;
  } else if( TOP_is_c2_load(OP_code(succ_op)) || 
             TOP_is_c2_store(OP_code(succ_op)) ) { 
     if( !TOP_is_c2_load(OP_code(pred_op)) &&
         !TOP_is_c2_store(OP_code(pred_op)) ) 
       return FALSE;
     else 
       return TRUE;
  } 
#endif

  if (OP_unalign_mem(pred_op) || OP_unalign_mem(succ_op))
    return TRUE;// don't know thereby have to make a conservative estimate

  INT pred_base_num = TOP_Find_Operand_Use(OP_code(pred_op), OU_base);
  INT succ_base_num = TOP_Find_Operand_Use(OP_code(succ_op),  OU_base);

#ifdef KEY
  // Don't know anything about the addresses.  Assume they match.  Bug 14376.
  if (pred_base_num < 0 ||
      succ_base_num < 0) {
    return TRUE;
  }
#endif

  TN *pred_base_tn = OP_opnd(pred_op, pred_base_num); 
  TN *succ_base_tn = OP_opnd(succ_op, succ_base_num);

  BOOL identical = FALSE;
  if (	TNs_Are_Equivalent(pred_base_tn, succ_base_tn) &&
	CG_DEP_Mem_Ops_Offsets_Overlap(pred_op, succ_op, &identical))
    return TRUE;

  // if the base register is $0, turn off further analysis. Cannot
  // assume that loads from different offsets of $0 are related.
  // (see pv669168 for more details).
  if (TN_is_zero_reg(pred_base_tn) && TN_is_zero_reg(succ_base_tn))
    return TRUE;

  return FALSE;
}


// =======================================================================
// OP_Has_Restrictions
// Return TRUE if the <op> has a special meaning w.r.t <source_bb> or
// <target_bb> such that it should not be considered as candidate for code 
// motion. For example, stack-adjustment, glue-copy OPs,.. etc are checked
// here.
// =======================================================================
static BOOL 
OP_Has_Restrictions(OP *op, BB *source_bb, BB *target_bb, mINT32 motion_type)
{
  if (CGTARG_Is_OP_Intrinsic(op)) return TRUE;

#ifdef TARG_X8664
  if( OP_icmp(op) )
    return TRUE;

  if( TOP_is_change_rflags( OP_code(op) ) ||
      OP_reads_rflags( op ) )
    return TRUE;
#endif

#ifdef TARG_LOONGSON
  if (OP_icmp(op))
    return TRUE;
#endif

  if (OP_has_hazard(op)) return TRUE;

  if ((cur_hbs_type & HBS_BEFORE_GRA) != 0 && OP_no_move_before_gra(op)) 
    return TRUE;

  // If <OP> accesses rotating register banks, return FALSE.
  if (OP_access_reg_bank(op)) return TRUE;

  if ((BB_entry(source_bb) && BB_entry_sp_adj_op(source_bb) == op) ||
     (BB_exit(source_bb) && BB_exit_sp_adj_op(source_bb) == op))
    return TRUE;

  // SP def OPs are a trouble.
  if (OP_Defs_TN(op, SP_TN) || OP_Defs_Reg(op, REGISTER_CLASS_sp, REGISTER_sp))
    return TRUE;

  // Do extra processing for BRP instructions.
  if (OP_branch_predict(op)) {
    UINT64 num_insts = 0;
    BB *prev_bb;
    for (prev_bb = source_bb; prev_bb && prev_bb != target_bb; 
	 prev_bb = BB_prev(prev_bb)) {
      num_insts += BB_length(prev_bb);
    }
    
    // It's assumed that about 1/3 nops will be added later, so include the
    // expansion factor. The below condition checks that BRP instructions
    // are not scheduled too early, such that they violate the offset
    // restrictions.

    if ((num_insts * 1.3 * INST_BYTES) >= DEFAULT_BRP_BRANCH_LIMIT)
      return TRUE;
  }

  // Even SP ref OPS are a trouble for circular scheduling
  if (motion_type & GCM_CIRC_ABOVE) {
     if (OP_Refs_TN(op,  SP_TN) || 
	 OP_Refs_Reg(op, REGISTER_CLASS_sp, REGISTER_sp)) return TRUE;

     // No need to circular-schedule BRP ops.
     if (OP_branch_predict(op)) return TRUE;

     if (OP_memory(op)) {
       if (OP_no_alias(op) || OP_prefetch(op)) return FALSE;

       // If <PROC> has delayed exception mechanism, either by speculative
       // loads or predication, return FALSE.
       if (PROC_has_delayed_exception() && OP_has_predicate(op)) { 
	 return FALSE;
       } else {

	 // TODO: Need to add more relaxation rules w.r.t memory accesses 
	 // which can be verified as safe. Disallow memory references which 
	 // belong to KIND_ARRAY type since circular scheduling them can lead 
	 // to out-of-bound accesses. Need to change constant operand 
	 // reference by a generic item. 

	 if (TN_is_symbol(OP_opnd(op, 1)) && 
	     ST_class(TN_var(OP_opnd(op, 1))) == CLASS_VAR && 
	     TY_kind(ST_type(TN_var(OP_opnd(op, 1)))) != KIND_ARRAY) 
	   return FALSE;
       }
       return TRUE;
     } 
  }

  // for the PRE-GCM stage, don't let local TN's which are ideal 
  // candidates for peephole opportunities (eg. copy ops) be global TN's.

  if (!Ignore_TN_Dep && (OP_copy(op) || OP_glue(op))) return TRUE;

  //TODO: need to check if this is not too conservative.
  INT i;
  for (i = 0; i < OP_results(op); ++i) {
    TN *result_tn = OP_result(op, i);
    ISA_REGISTER_CLASS result_cl = TN_register_class (result_tn);

        // For Pre-GCM stage, return FALSE, if TN is dedicated or homeable.

    if ((!Ignore_TN_Dep && 
	(TN_is_dedicated(result_tn) || TN_is_gra_homeable(result_tn))) ||

	// For Post-GCM stage (or SWP rotating reg allocated), return FALSE,
	// if TN is rotating register type.

	((Ignore_TN_Dep || TN_is_register(result_tn)) &&
	 REGISTER_Is_Rotating(result_cl, TN_register(result_tn)))) 

      return TRUE;
    }

  // TODO: Need to prune this further. Need to analyze home locations, 
  // analyze any potential overlaps etc. 
  for (i = 0; i < OP_opnds(op); ++i) {
    TN *opnd_tn = OP_opnd(op, i);
    if (TN_is_constant(opnd_tn)) continue;

    ISA_REGISTER_CLASS opnd_cl = TN_register_class (opnd_tn);

    // homeable TNs not included for Pre-GCM phase.
    if ((!Ignore_TN_Dep && TN_is_gra_homeable(opnd_tn)) ||
	
	// SWP rotating register TNs are not included.
	((Ignore_TN_Dep || TN_is_register(opnd_tn)) &&
	 REGISTER_Is_Rotating(opnd_cl, TN_register(opnd_tn)))) 
      return TRUE;
  }

#ifdef TARG_X8664
  // If OP must use or define a specific real register, then don't move it in
  // order to avoid possible conflicts with OPs in the target BB which also use
  // that real register.  For example, for the candidate OP "TN105 = ld32_m",
  // which writes to %rax, and this target BB:
  //
  //		  call		; returns value in %rax
  //  target_BB:  TN100 = %rax	; existing OP in target_bb which also uses %rax
  //		  jne		; xfer_op
  //
  // During the GCM_BEFORE_GRA pass, if GCM moves ld32_m to target_BB, GCM
  // would insert it before "TN100 = %rax", giving bad code since ld32_m kills
  // the previous %rax value.  Bug 9466.
  ASM_OP_ANNOT* asm_info = (OP_code(op) == TOP_asm) ?
    (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op) : NULL;

  for (int i = 0; i < OP_opnds(op); i++) {
    ISA_REGISTER_SUBCLASS subclass = asm_info ?
      ASM_OP_opnd_subclass(asm_info)[i] : OP_opnd_reg_subclass(op, i);
    if (Single_Register_Subclass(subclass) != REGISTER_UNDEFINED)
      return TRUE;
  }
  for (int i = 0; i < OP_results(op); i++) {
    ISA_REGISTER_SUBCLASS subclass = asm_info ?
      ASM_OP_result_subclass(asm_info)[i] : OP_result_reg_subclass(op, i);
    if (Single_Register_Subclass(subclass) != REGISTER_UNDEFINED)
      return TRUE;
  }
#endif

  return FALSE;
}

// =======================================================================
// Can_Do_Safe_Predicate_Movement
// Checks to see if an unsafe <cur_op> can be moved from <src_bb> to <tgt_bb>
// by converting it to safe op by doing instruction predication (i.e. guarding
// it by a control predicate).
// =======================================================================
BOOL
Can_Do_Safe_Predicate_Movement(OP        *cur_op, 
			       BB        *src_bb, 
			       BB        *tgt_bb, 
			       mINT32    motion_type)
{

  // For circular scheduling, need to make sure that the branch predicate for
  // the target block executes under non p0 conditions and has a compare inst
  // defined. 
  if (motion_type & (GCM_CIRC_ABOVE | GCM_SPEC_ABOVE)) {

    // if <cur_op> has an existing non p0 qualifying predicate and is
    // not safe-speculatable, return FALSE.
    // TODO: Add support to allow speculation of predicated instructions
    // as well. 
    if (OP_has_predicate(cur_op) &&
	!TN_is_true_pred(OP_opnd(cur_op, OP_PREDICATE_OPND)) &&
	!CGTARG_Can_Be_Speculative(cur_op)) return FALSE;

    OP *tgt_br_op = BB_branch_op(tgt_bb);
    if (tgt_br_op && OP_has_predicate(tgt_br_op)) {

      // TODO: Check if <src_bb> has a unique predecessor <tgt_bb>. For,
      // more than one predecessor cases, need to compute new predicate
      // expression and allow movement.
      if (!TN_is_true_pred(OP_opnd(tgt_br_op, OP_PREDICATE_OPND)) &&
	  (BB_Unique_Source(src_bb) ==  tgt_bb)) {
	TN *tn1, *tn2;
	OP *cmp_op;
	CGTARG_Analyze_Compare(tgt_br_op, &tn1, &tn2, &cmp_op);

	// if <cmp_op> found and has both result (true/false) predicate
	// registers defined, return TRUE.
	if (cmp_op && cmp_op != tgt_br_op && OP_results(cmp_op) == 2) {
	  TN *r0 = OP_result(cmp_op, 0);
	  TN *r1 = OP_result(cmp_op, 1);

	  // if <cmp_op> has a non p0 qualifying predicate, need to be
	  // conservative, i.e. it has other predicate expressions that can
	  // satisfy the condition. Unless, we can accurately determine
	  // these expressions, be conservative at the moment.

	  if (OP_has_predicate(cmp_op) &&
	      !TN_is_true_pred(OP_opnd(cmp_op, OP_PREDICATE_OPND)))
	    return FALSE;

	  // For GCM phase (before register allocation), we need to guarantee
	  // that (1) either both the TNs are globals, or (2) need to make
	  // sure if they indeed become global TNs, update the GTN sets
	  // accordingly.

	  if (!Ignore_TN_Dep && (!TN_is_global_reg(r0) || !TN_is_global_reg(r1)))
	    return FALSE;

	  if (!TN_is_true_pred(r0) && !TN_is_true_pred(r1)) return TRUE;

	}
      }
    }
  }

  return FALSE;
}

// =======================================================================
// Eager_Ptr_Deref_Spec
// checks to see if a pointer reference in <src> can be moved to <dest>.
// A pointer referencing the same base address with a slight difference in
// the offsets among two references in <src> and <dest> can be speculated.
// This is controlled by <Eager_Ptr_Deref>. This routine is called only
// when <CG_DEP_Mem_Ops_Alias> returns TRUE.
// =======================================================================
static BOOL
#if defined(TARG_SL)
Eager_Ptr_Deref_Spec(OP *deref_op, BB *cur_bb, BB *src_bb, BB *dest_bb, BOOL forw)
#else
Eager_Ptr_Deref_Spec(OP *deref_op, BB *dest_bb, BOOL forw)
#endif
{
#ifdef TARG_SL
  /* SL2 load/store instructions don't have OU_base/OU_offset
   * properties in their operands/results. For now, I just
   * ignore them
   */
  if( TOP_is_c2_load(OP_code(deref_op)) || 
      TOP_is_c2_store(OP_code(deref_op)) )
    return FALSE;  
#endif

  OP *cur_op;
  OP *limit_op;
  BOOL valid_addrs_found = FALSE;

  limit_op = NULL;
  TN *deref_base_tn;
 
  INT dbase_num = TOP_Find_Operand_Use(OP_code(deref_op), OU_base);
  INT doffset_num = TOP_Find_Operand_Use(OP_code(deref_op), OU_offset);

#ifdef KEY
  deref_base_tn = dbase_num >= 0 ? OP_opnd(deref_op, dbase_num) : NULL;
#else
  deref_base_tn = OP_opnd(deref_op, dbase_num);
#endif // KEY

  for (cur_op = (forw) ? BB_last_op(dest_bb) : BB_first_op(dest_bb);
       cur_op && cur_op != limit_op;
       cur_op = (forw) ?  OP_prev(cur_op) : OP_next(cur_op)) {
#ifdef TARG_SL
    /* So far, ignore sl2 load/store */
    if( TOP_is_c2_load(OP_code(deref_op)) || 
        TOP_is_c2_store(OP_code(deref_op)) )
      continue;
#endif

    if (OP_dummy(cur_op)) continue;
   
    // collect all memory references in the <dest> bb 
    
    if (OP_load(cur_op) || OP_store(cur_op)) { 

      INT cbase_num = TOP_Find_Operand_Use(OP_code(cur_op), OU_base);
      INT coffset_num = TOP_Find_Operand_Use(OP_code(cur_op), OU_offset);

#ifdef KEY
      TN *cur_base_tn = cbase_num >= 0 ? OP_opnd(cur_op, cbase_num) : NULL;
#else
      TN *cur_base_tn = OP_opnd(cur_op, cbase_num);
#endif // KEY
      TN *cur_result_tn = OP_load(cur_op) ? OP_result(cur_op, 0) : NULL;

      if (!Similar_Ptr_Addrs_Match(cur_op, deref_op)) {
	if (Similar_Ptr_Offset_ok(cur_op, deref_op)) {
	  
	  if (coffset_num < 0 && doffset_num < 0) {
	    valid_addrs_found = TRUE;
	    break;
	  } else {
	    // Need to check if the OP doesn;t modify the base.

	    BOOL modifies_base = cur_result_tn &&
#ifdef KEY
	      ( cur_base_tn != NULL ) &&
#endif // KEY
	      TNs_Are_Equivalent(cur_result_tn, cur_base_tn);

	    if (!modifies_base && 
		TNs_Are_Equivalent(deref_base_tn, cur_base_tn)){ 
	      valid_addrs_found = TRUE;
	      break;
	    }
	  }
	}
      } else {
	// no need to look further since <deref_op> can't move past
	// <cur_op> anyway.
	valid_addrs_found = FALSE;
	break;
      }
    }

    // if the memory reference is being modified by this <op>, removed it 
    // from the list of valid memory references.

#ifdef TARG_IA64
    BOOL base_redef = FALSE;
    for (INT i = 0; i < OP_results(cur_op); ++i) {
      TN *result = OP_result(cur_op,i);
      if (Ignore_TN_Dep) {
	REGISTER result_reg = TN_register(result);

	// If there was a previous update of <result_reg>, remove it.
	// Sometimes, this may be the first occurence of <result_reg>, so
	// discontinue further.
	if (result_reg == TN_register(deref_base_tn)) 
	  base_redef = TRUE;
      } else {
	if (TNs_Are_Equivalent(result, deref_base_tn)) base_redef = TRUE;
      }
    }

    // No need to look further if there exists a base redef.
    if (base_redef) {
      valid_addrs_found = FALSE;
      break;
    }
#else // TARG_IA64
#ifdef KEY
    if( deref_base_tn != NULL )
#endif // KEY
      {
	BOOL base_redef = FALSE;
	for (INT i = 0; i < OP_results(cur_op); ++i) {
	  TN *result = OP_result(cur_op,i);
	  if (Ignore_TN_Dep) {
	    REGISTER result_reg = TN_register(result);

	    // If there was a previous update of <result_reg>, remove it.
	    // Sometimes, this may be the first occurence of <result_reg>, so
	    // discontinue further.
	    if (result_reg == TN_register(deref_base_tn)) 
	      base_redef = TRUE;
	  } else {
	    if (TNs_Are_Equivalent(result, deref_base_tn)) base_redef = TRUE;
	  }
	}
	
	// No need to look further if there exists a base redef.
	if (base_redef) {
	  valid_addrs_found = FALSE;
	  break;
	}
      }
#endif

    // use CG_DEP_Call_Aliases interface to determine if the
    // deref_op is being read/write in the called procedure. uses info
    // from WOPT alias manager. 
    if (OP_call(cur_op) && 
	!CG_DEP_Can_OP_Move_Across_Call(deref_op, cur_op,forw,Ignore_TN_Dep)) {
      valid_addrs_found = FALSE;
      break;
    }
  }

  // If there exits a valid address, return TRUE.
  if (valid_addrs_found) return TRUE;

  return FALSE;
}

// =======================================================================
// Null_Ptr_Deref_Spec
// checks to see if there is any null pointer reference in path from <src> 
// to <dest>. Null pointer tests are used for checking the bounds or exit
// conditions. Any other valid pointer derefence can be speculated beyond it.
// The test isn't really necessary since this special case is alreday 
// incorporated in the general framework. the main reason is if this condition
// is used in conjunction with other conditions (as is very frequent), we can
// safely eliminate the branch condition (upon valid page references) in the 
// block. This is controlled by <Eager_Null_Ptr_Deref>.
// =======================================================================
static BOOL
Null_Ptr_Deref_Spec(OP *deref_op, BB *src, BB *dest)
{
  REGISTER condition_reg; // reg value which is being compared with zero
  TN *condition_tn;       // tn value which is being compared with zero
  BOOL taken_path;

  OP *branch_op = BB_branch_op(dest);
  // <dest> block doesn't contain any conditional branch
  if (branch_op == NULL || !OP_cond(branch_op)) return FALSE;

  // this condition is too restrictive and should be removed. 
  // if (!Null_Ptr_Offset_ok(deref_op)) return FALSE;

  TN *opnd1, *opnd2;
  INT variant;

  // Invoke the target-independent interface to analyze the branch.
  variant = CGTARG_Analyze_Branch(branch_op, &opnd1, &opnd2);

  // Some branches only have one operand, e.g. mips branch on fcc,
  // these aren't interesting.
  if (opnd2 == NULL) return FALSE;

  Is_True(opnd1, ("expected two operand TNs from CGTARG_Analyze_Branch"));

  // either of the branch operands must be zero to qualify for this case
  // and determine the condition reg whose value is being compared with zero
  if (TN_is_zero(opnd1)) {
    condition_tn = opnd2;
  } else if (TN_is_zero(opnd2)) {
    condition_tn = opnd1;
  } else {
    return FALSE;
  }

  if (Ignore_TN_Dep)
    condition_reg = TN_register(condition_tn);

  // now determine the taken path
  switch (variant) {
    // beq, beql:
  case V_BR_I8EQ:
    taken_path = FALSE;
    break;

    // bne, bnel:
    // bgez, bgezl, bgtz, bgtzl,
    // blez, blezl, bltz, bltzl
  case V_BR_I8NE:
  case V_BR_I8GE:
  case V_BR_I8GT:
  case V_BR_I8LE:
  case V_BR_I8LT:
    taken_path = TRUE;
    break;

  default:
    condition_reg = 0;
    taken_path = FALSE;
  }
  
  // need to make sure that the condition in <dest> is actually the boundary 
  // test condition for "<op> in <src> and that <src> post-dominates <dest>".
  BOOL non_post_dom = BS_MemberP (BB_dom_set(src), BB_id(dest)) &&
		  !BS_MemberP (BB_pdom_set(dest), BB_id(src));
  if (non_post_dom || (dest == BB_prev(src))) { 
	if (taken_path) return FALSE;
  } else {
	if (!taken_path) return FALSE;
  }

#if defined(TARG_X8664) || defined(TARG_LOONGSON)
  const int base_idx = TOP_Find_Operand_Use( OP_code(deref_op),OU_base );
  if( base_idx < 0 )
    return FALSE;
  TN* base_tn = OP_opnd( deref_op, base_idx );
  TN* offset_tn = OP_opnd( deref_op,
			   TOP_Find_Operand_Use( OP_code(deref_op),OU_offset ) );
#else
  // !TARG_X8664
  TN *base_tn = OP_load(deref_op) ? OP_opnd(deref_op, 0) : 
				    OP_opnd(deref_op, 1);

  TN *offset_tn = OP_load(deref_op) ? OP_opnd(deref_op, 1): 
				      OP_opnd(deref_op, 2);
#endif  // TARG_X8664

  // TODO: actually, any positive constant offsets which fit into page 
  // boundary can be considered
  if (Ignore_TN_Dep) {
      REGISTER base_reg = TN_register(base_tn);
      if (base_reg == condition_reg && TN_value(offset_tn) >= 0)
	return TRUE;
  } else {
    if (TN_number(base_tn) == TN_number(condition_tn) && 
	TN_value(offset_tn) >= 0)
      return TRUE;
  }

  return FALSE;
}

// ======================================================================
// Find_Limit_OP
// finds the <limit_op> of <tgt_bb>. The <limit_op> is the last <op> in
// <tgt_bb> the <cur_op> can be moved before. The calculation of <limit_op>
// also depends on whether its' PRE/POST GCM phase.
// ======================================================================
static OP*
Find_Limit_OP(OP *cur_op, BB *cur_bb, BB *src_bb, BB *tgt_bb)
{
  OP *limit_op;

  if (Cur_Gcm_Type & GCM_BEFORE_GRA) 
    limit_op = BB_copy_xfer_op(tgt_bb);
  else 
    limit_op = BB_xfer_op(tgt_bb);

  if (cur_bb == src_bb) 
    limit_op = cur_op;
  else if (cur_bb == tgt_bb)
    limit_op = (limit_op) ? OP_prev(limit_op) : BB_last_op(cur_bb);
  else 
    limit_op = NULL;
 
  return limit_op;
}

// =======================================================================
// Can_Mem_Op_Be_Moved
// checks to see if a <mem_op> can be moved from <src> to <dest>. For 
// speculative movement this is in addition to
// CGTARG_Can_Be_Speculative (cgtarget.c) and is specific to memory ops.
// Other pointer heuristics are applied here before deciding whether it's 
// safe or not. This is controlled by <Enable_Spec_Loads>.
// =======================================================================
static BOOL
Can_Mem_Op_Be_Moved(OP *mem_op, BB *cur_bb, BB *src_bb, BB *dest_bb, 
		    mINT32 motion_type
#if defined(TARG_SL)
, mUINT32 *cur_spec_type
#endif
                   )
{

  if ((Cur_Gcm_Type & GCM_AFTER_GRA) && !GCM_POST_Spec_Loads) 
    return FALSE;

  if ((Cur_Gcm_Type & GCM_BEFORE_GRA) && !GCM_PRE_Spec_Loads)
    return FALSE;
  
  // TODO: first, we filter out the most easy cases (for fast compile time). 
  // I am sure there will be more to filter out (for other motion types as 
  // well) in the future.
#if defined(TARG_SL)
  *cur_spec_type = 0;
#endif
  BOOL forw = motion_type & (GCM_EQUIV_FWD | GCM_SPEC_ABOVE | 
			     GCM_DUP_ABOVE | GCM_CIRC_ABOVE);
  if (motion_type & (GCM_SPEC_ABOVE | GCM_SPEC_BELOW | GCM_CIRC_ABOVE)) {

    // stores can't be speculated
    if (OP_store(mem_op)) 
	return FALSE;
  }

#ifdef TARG_X8664
  /* Do not allow a load operation under -mcmodel=medium to across a
     call, since such load will overwrite %rax that holds the return value.
     (bug#2419)

     TODO ???:
     The right fix should be done inside OP_To_Move(), but <failed_reg_defs>
     is not updated if an op has restrictions.
   */
  if( mcmodel >= MEDIUM &&
      !Ignore_TN_Dep    &&
      forw ){
    BB* prev = BB_prev( src_bb );

    if( prev != NULL &&
	BB_call( prev ) ){
      const TOP top = OP_code(mem_op);
      if( top == TOP_ld8_abs  || top == TOP_ld16_abs ||
	  top == TOP_ld32_abs || top == TOP_ld64_abs )
	return FALSE;
    }
  }
#endif

  // volatile ops shouldn't be touched at all
  if (OP_volatile(mem_op)) return FALSE;

  // prefetches don't alias with anything
  if (OP_prefetch(mem_op)) return TRUE;

  OP *cur_op, *br_op, *limit_op;
  BOOL definite = FALSE, alias = FALSE;
  // we only look for memory dependences here; register dependences and 
  // call dependences will be considered in the later phase only after we 
  // ensure that there aren't any memory dependences.
  limit_op = Find_Limit_OP(mem_op, cur_bb, src_bb, dest_bb);

  BOOL read_read_dep;
  for (cur_op = ((forw && cur_bb != src_bb) || (!forw && cur_bb == src_bb)) ?
       BB_last_op(cur_bb) : BB_first_op(cur_bb);
       cur_op && cur_op != limit_op;
       cur_op = ((forw && cur_bb != src_bb) || (!forw && cur_bb == src_bb)) ?
       OP_prev(cur_op) : OP_next(cur_op)) {

    // dummy ops don't alias with anything
    if (OP_dummy(cur_op)) continue;

    // exclude prefetch ops with zero omegas for circular scheduling.
    if (OP_prefetch(cur_op)) {
      if (Motion_Is_CIRC_ABOVE(motion_type) ) {

	// found PF_POINTER from memop
	WN  *memwn = Get_WN_From_Memory_OP(mem_op);
	PF_POINTER *pf_ptr1 = memwn ?
	  (PF_POINTER *)WN_MAP_Get(WN_MAP_PREFETCH, memwn) : NULL;

	// found PF_POINTER from prefetch
	WN *wn = Get_WN_From_Memory_OP( cur_op);
	PF_POINTER *pf_ptr2 = wn ? (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,wn) : NULL;
	
	if (pf_ptr1 == pf_ptr2)
	  return FALSE;
	
      } else continue;
    }
    
    if (OP_memory(cur_op) 
#ifdef TARG_X8664
	|| OP_load_exe(cur_op)
#endif
	) {
#ifdef TARG_X8664
      read_read_dep = ( OP_load(cur_op) || OP_load_exe(cur_op) ) &&
	( OP_load(mem_op) || OP_load_exe(mem_op) );
#else
      read_read_dep = OP_load(cur_op) && OP_load(mem_op);
#endif

      // No need to process read-read memory dependences
      if (!read_read_dep &&
	  CG_DEP_Mem_Ops_Alias(cur_op, mem_op, &definite)) {
#if !defined(TARG_SL)
	return FALSE;
      }
    }
  }
#else
        // if there is true alias, return FALSE
        if (definite) return FALSE;
        
        // #676123; first, if there exists an alias and <cur_bb> is not equal
        // to <dest_bb>, then there is nothing much other safety tests can do.
        if (cur_bb != dest_bb) return FALSE;

        // Second, if there exists a memory dependence between <cur_op>
        // and <mem_op> and that their base registers are different, there is
        // no way that eager_ptr_deref speculation tests can diagnose further.
        // so, make a conservative estimate and quit.

        if (OP_load(cur_op) || OP_store(cur_op)) {
          TN *to_base_tn = OP_load(cur_op) ?    OP_opnd(cur_op, 0) :
                                                OP_opnd(cur_op, 1);
          TN *from_base_tn = OP_load(mem_op) ?  OP_opnd(mem_op, 0) :
                                                OP_opnd(mem_op, 1);
          if(TN_is_register(to_base_tn) && TN_is_register(from_base_tn)) {
          if (!Ignore_TN_Dep) {
            if (TN_number(to_base_tn) != TN_number(from_base_tn))
              /* we aggressively relax the constraints when doing circular 
               * scheduling
               */
              if( !(Motion_Is_CIRC_ABOVE(motion_type) && (cur_bb == src_bb)) )
                return FALSE;
          } else {
            REGISTER to_base_reg = TN_register(to_base_tn);
            REGISTER from_base_reg = TN_register(from_base_tn);
            if (to_base_reg != from_base_reg) 
              if( !( Motion_Is_CIRC_ABOVE(motion_type) && (cur_bb == src_bb)) )
                return FALSE;
          }
	  }
        }
        // otherwise, there is a memory dependence which may have the
        // possibility of being diagnosed as NO dependence.
        alias |= TRUE;
      }
    }
  }

  // #642858, #641258;
  // If we have reached this point, have convinced ourselves, that there
  // exists no memory ops in <cur_bb> which aliases with <deref_op> with
  // non-matching base addresses.
  if (forw) {

    // now, we do more specific tests concerening <mem_op> w.r.t to <src> and
    // <dest> and see if we can deduce further
    if (GCM_Pointer_Spec &&

        // for now, the pointer specific tests are done after register
        // allocation
        // no need to do specific tests for definite dependences
        // exclude prefetch ops for eager_ptr_deref tests since they don't
        // come under this category anyway.
        (GCM_Eager_Ptr_Deref && !definite && !OP_prefetch(mem_op) &&

        // #676123; Perform eager_ptr_speculation safety tests only when
        // <cur_bb> == <dest_bb> (or target_block), otherwise there is nothing
        // much other safety tests can do.
         (cur_bb == dest_bb) &&

         Eager_Ptr_Deref_Spec(mem_op, cur_bb, src_bb, dest_bb, forw)))
     {
       Set_EAGER_PTR_SPEC(*cur_spec_type);
     }

#if 0
    /* Have to be really careful in relaxing this constraint. This will
       allow circular scheduling of unsafe mem ops. More context analysis
       needs to be done on a global basis (not just the path containing
       the blocks) to determine safeness. */
    if ( Motion_Is_CIRC_ABOVE(motion_type) && (cur_bb == src_bb)) {
      Set_CIRC_PTR_SPEC(*cur_spec_type);
    }
#endif

  }
  if( !alias || EAGER_PTR_SPEC(*cur_spec_type) || CIRC_PTR_SPEC(*cur_spec_type) ) 
    return TRUE;

  return FALSE;
#endif // !TARG_SL
  // #642858, #641258;
  // If we have reached this point, have convinced ourselves, that there
  // exists no memory ops in <cur_bb> which aliases with <deref_op> with
  // non-matching base addresses.

  return TRUE;
}

// =======================================================================
// Can_Inst_Be_Moved
// checks to see if <op> inst_type can be moved or not.
// =======================================================================
static BOOL
Can_Inst_Be_Moved (OP *op, VECTOR succs_vector, INT succ_num)
{
  // If there is only one successor, it is safe to move the <op>.
  if (VECTOR_count(succs_vector) == 1) return TRUE;

  if (!CGTARG_Can_Be_Speculative (op)) return FALSE;

  if (OP_has_hazard(op) || OP_imul(op) || OP_idiv(op)) {
    return FALSE;
  }

  // If the <op> has a result, check if the result-register is live on
  // entry to any of the other successors.
  for (INT i = 0; i < OP_results(op); ++i) {
    TN *result = OP_result(op,i);
    ISA_REGISTER_CLASS result_cl = TN_register_class (result);
    REGISTER result_reg = TN_register (result);
    for (INT i = 0; i < VECTOR_count(succs_vector); i++) {
      if (i == succ_num) continue;
      BBLIST *succ_bl = (BBLIST *) VECTOR_element(succs_vector, i);
      BB *succ_bb = BBLIST_item(succ_bl);
      if (REG_LIVE_Into_BB (result_cl, result_reg, succ_bb)) return FALSE;
    }
  }
  return TRUE;
}

// =======================================================================
// Find_Vacant_Slots_BB
// Determines the vacant slots present in <bb> w.r.t <targ_alignment>
// It dynamically recomputes the <bb> start_pc address from it's prior
// scheduled predecessors and 
// =======================================================================
static INT16
Find_Vacant_Slots_BB(BB *bb, INT targ_alignment)
{
  BBLIST *bblist;
  INT16 vacant_slots = 0;

  BBSCH *bbsch = (BBSCH *)BB_MAP_Get (bbsch_map, bb);
  // ignore if it's a loophead bb. since code motion is never done beyond
  // loop boundaries, it's isn;t necessary to recompute it. 
  // TODO: might need to revisit for bottom-loading (if at all)
  if (!BB_loop_head_bb(bb)) {
    FOR_ALL_BB_PREDS(bb, bblist) {
      BB *pred_bb = BBLIST_item(bblist);
      BBSCH *pred_bbsch = (BBSCH *)BB_MAP_Get (bbsch_map, pred_bb);

      // determine the MAX bb_start pc addresses taking into account
      // the start address and the number of real ops in predecessor bbs
      if (bbsch && pred_bbsch) {
	BBSCH_bb_start_pc(bbsch) = 
                     MAX (BBSCH_bb_start_pc(bbsch),
			  BBSCH_bb_start_pc(pred_bbsch) +
			  BBSCH_num_real_ops(pred_bbsch));
	BB_MAP_Set (bbsch_map, bb, bbsch);
      }
    }
  }
					
  // vacant_slots is used to determine how many vacant_slots are
  // available if at all to align this <bb>.
  vacant_slots = (targ_alignment - ((BBSCH_bb_start_pc(bbsch) + 
				     BBSCH_num_real_ops(bbsch)) %
				    targ_alignment));

  return vacant_slots;
}

// =======================================================================
// Find_OP_For_Delay_Slot
// placeholder to pick the best op from <bb> which can be put in the
// branch delay slot. currently, it just returns the first instruction 
// of the basic block.
// =======================================================================
static OP*
Find_OP_For_Delay_Slot (BB *bb)
{
  return First_Inst_Of_BB (bb);
}

// =======================================================================
// Fill_From_Successor
// checks to see if <xfer_op> can be filled and put in br_delay slot in <bb>
// =======================================================================
static BOOL
Fill_From_Successor (BB *bb, OP *xfer_op, VECTOR succs_vector, INT succ_num)
{
  BBLIST *succ_bl = (BBLIST *) VECTOR_element(succs_vector, succ_num);
  BB *succ_bb = BBLIST_item(succ_bl);
  OP *first_op = Find_OP_For_Delay_Slot (succ_bb); 

  // pv566961. if <succ_bb> is a loophead block and that <bb> and <succ_bb>
  // are enclosed within the same region, avoid filling the delay slot 
  // of the branch. This is due to the fact that region insertion phase 
  // currently doesn't insert empty prelude/postlude blocks to the 
  // currently processed region.

  if ( BB_rid(succ_bb) &&
       BB_id(succ_bb) == BB_id(REGION_First_BB)) return FALSE;

  typedef enum { 
    FILL_NONE,
    FILL_DELETE,
    FILL_MOVE,
    FILL_DUP
  } FILL_TYPE;

  FILL_TYPE fill_type = FILL_NONE;
  if (Is_Delay_Slot_Op (xfer_op, first_op) &&
      !OP_Has_Restrictions(first_op, bb, succ_bb, GCM_DUP_ABOVE)) {
    TOP blikely_opcode;
    BOOL fallthru = (succ_bb == BB_next(bb));
    BOOL single_pred = (BBlist_Len (BB_preds(succ_bb)) == 1);
    if (Can_Inst_Be_Moved (first_op, succs_vector, succ_num)) {
      // For jr instruction we can only move the target instruction. Don't
      // try to copy target instruction since we don't want to change
      // the branch target in the switch table.
      if (single_pred) {
        fill_type = FILL_MOVE;
      }
      else if (!(OP_ijump(xfer_op) && !OP_call(xfer_op))) {
	// deleting the nop in the delay slot can be done only if the 
	// target of the branch is something other than the fallthru bb.
        if (fallthru && VECTOR_count(succs_vector) != 1) {
	  fill_type = FILL_DELETE;
        }
        else {
	  // if it's an unconditional branch with unique successor <succ_bb>
	  // and that its unique predecessor and <succ_bb> are equivalent
	  // means that performing the <FILL_DUP> is unnecessary since the
	  // unconditional branch in <bb> can effectively be eliminated by
	  // cflow. assuming that <unique_pred> are equivalent maybe too
	  // STRICT. need to investigate further.
	  if (OP_br(xfer_op) && !OP_cond(xfer_op) && 
	     (VECTOR_count(succs_vector) == 1) &&
	     (BBlist_Len (BB_preds(bb)) == 1)) {
	    BB *unique_pred = BB_Unique_Predecessor (bb);
	    // kludge to match if the frequencies of the <unique_pred> and
	    // the <succ_bb> are same without recomputing the dominators
	    // and post-dominators (compspeed purposes)
	    if (unique_pred && (BB_freq(unique_pred) == BB_freq(succ_bb))) {
	      fill_type = FILL_NONE;
	      Run_Cflow_Delay = TRUE;
	    }
	    else fill_type = FILL_DUP;
	  }
  	  else fill_type = FILL_DUP;
	}
      }
    }
    else if (!fallthru && 
	     CGTARG_Can_Change_To_Brlikely (xfer_op, &blikely_opcode) &&
	     CGTARG_Use_Brlikely (BBLIST_prob(succ_bl))
	     ) 
    {
      OP_Change_Opcode(xfer_op, blikely_opcode);
      fill_type = (single_pred) ? FILL_MOVE : FILL_DUP;
      if (Trace_Fill_Delay_Slots) {
	#pragma mips_frequency_hint NEVER 
        fprintf (TFile, "DELAY_FILL>   changed to BRLIKELY\n");
      }
    }
  }

  OP *delay_op = BB_last_op(bb);
  BB *from_bb;

  switch (fill_type) {
  case FILL_DELETE:
    if (Trace_Fill_Delay_Slots) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile,"DELAY_FILL> Remove delay-slot op in BB:%d freq = %#.2f\n",
	       BB_id(bb), BB_freq(bb));
      Print_OP_No_SrcLine (delay_op);
    }
    BB_Remove_Op (bb, delay_op);
    break;
  case FILL_MOVE:
    if (Trace_Fill_Delay_Slots) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "DELAY_FILL> Move OP: from BB:%d freq = %#.2f to BB:%d freq = %#.2f\n",
	       BB_id(OP_bb(first_op)), BB_freq(OP_bb(first_op)), 
	       BB_id(bb), BB_freq(bb));
      Print_OP_No_SrcLine (first_op);
    }
    BB_Remove_Op (bb, delay_op);
    from_bb = OP_bb(first_op);
    BB_Move_Op_To_End (bb, OP_bb(first_op), first_op);
    if (Is_BB_Empty (from_bb)) {
      Run_Cflow_Delay = TRUE;
    }
    break;
  case FILL_DUP:
    if (Trace_Fill_Delay_Slots) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "DELAY_FILL>   Copy OP: from BB:%d freq = %#.2f to BB:%d frequency = %#.2f\n",
	       BB_id(OP_bb(first_op)), BB_freq(OP_bb(first_op)), 
	       BB_id(bb), BB_freq(bb));
      Print_OP_No_SrcLine (first_op);
    }
    BB_Remove_Op (bb, delay_op);
    BB_Append_Op (bb, Dup_OP (first_op));
    TN *label_tn = NULL;
    for (INT i = 0; i < OP_opnds(xfer_op); ++i) {
      if (TN_is_label(OP_opnd(xfer_op,i))) {
	label_tn = OP_opnd(xfer_op,i);
	Set_OP_opnd (xfer_op, i, Gen_Adjusted_TN (label_tn, 4));
      }
    }
    FmtAssert (label_tn != NULL, ("Fill_From_Successor: no label in xfer_op"));
    break;
  }
  return (fill_type != FILL_NONE);
}

// ======================================================================
// GCM_Fill_Branch_Delay_Slots
// Try filling branch delay slots with the first instruction of one of 
// the target basic blocks.
// ======================================================================
void
GCM_Fill_Branch_Delay_Slots (void)
{
  // check if this optimization has been disabled.
  if (!GCM_Enable_Fill_Delay_Slots) return;

  Run_Cflow_Delay = FALSE;

  MEM_POOL *pool = &MEM_local_pool;
  L_Save ();

  Trace_Fill_Delay_Slots = Get_Trace (TP_GCM, 0x02);

  // compute live-in sets for registers.
  REG_LIVE_Analyze_Region ();

  for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    BBLIST *bl;

    // needn't process already processed regions
    if ( (BB_rid(bb)) && (RID_level(BB_rid(bb)) >= RL_CGSCHED)) continue;

    // There might be an empty basic block left from an earlier phase.
    // This happens when LRA deletes instructions from a bb. We should
    // run cflow if we detect this case.
    if (Is_BB_Empty (bb)) {
      Run_Cflow_Delay = TRUE;
      continue;
    }

    if (BB_call(bb)) continue;
    OP *delay_op = BB_last_op(bb);
    // check if the last op in bb is a nop.
    if (delay_op == NULL || !OP_noop(delay_op)) continue;
    OP *xfer_op = OP_prev(delay_op);
    if (xfer_op == NULL || !OP_xfer(xfer_op)) continue;
    if (BB_exit(bb)) {

      // At this point we know bb's last two insts are "jr $31; noop".
      // If we haven't already noted we need cflow, see if there is
      // an opportunity to optimize a unconditional branch to a return.
      // There is an opportunity if the predecessor contains an
      // unconditional branch, and it will target the return inst
      // after delay slot filling is complete.
      if (!Run_Cflow_Delay && BB_length(bb) <= 3) {
	FOR_ALL_BB_PREDS (bb, bl) {
	  BB *pred = BBLIST_item(bl);
	  OP *br = BB_branch_op(pred);
	  if (   br 
	      && OP_br(br)
	      && !OP_cond(br)
	      && (BB_length(bb) == 2 || OP_noop(BB_last_op(pred)))
	  ) {
	    Run_Cflow_Delay = TRUE;
	    break;
	  }
	}
      }
      continue;
    }
    INT num_succs = BBlist_Len (BB_succs(bb));
    if (Trace_Fill_Delay_Slots) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "DELAY_FILL> Empty delay slot in BB:%d, succs:%d\n", 
		      BB_id(bb), num_succs);
    }
    // create a vector of all the successors, sorted in the order of 
    // decreasing frequency estimate.
    VECTOR succs_vector = VECTOR_Init (num_succs, pool);
    FOR_ALL_BB_SUCCS (bb, bl) {
      VECTOR_Sorted_Add_Element (succs_vector, (void *)bl, 
				 sort_by_edge_probability);
    }
    for (INT i = 0; i < num_succs; i++) {
      if (Fill_From_Successor (bb, xfer_op, succs_vector, i)) break;
    }
  }

  L_Free ();

  if ((Run_Cflow_Delay || Run_Cflow_GCM) && GCM_Enable_Cflow) {

    /* Filling delay slots may have caused BBs to become empty
     * and possibly resulted in branches to the next BB. So call
     * cflow with only branch optimizations and unreachable block
     * removal enabled. NOTE: the other opts work, but they probably
     * are't profitiable at this stage. so don't waste the compile time.
     */

    CFLOW_Optimize(CFLOW_BRANCH | CFLOW_UNREACHABLE | CFLOW_FILL_DELAY_SLOTS,
		   "CFLOW (from gcm)");
  }

  // Finish with reg_live after cflow so that it can benefit from the info.
  REG_LIVE_Finish ();
}

#if defined(TARG_SL)
// ==================================================================
// Cumulate_Local_TN_Promotion
// Use a TN_SET to record all the local TNs promoted.
// ==================================================================
static void
Cumulate_Local_TN_Promotion( OP *cand_op )
{
  INT32 res_num = OP_results( cand_op );
  INT32 opnd_num = OP_opnds( cand_op );

  for(int i=0; i < res_num; i++ ){
    TN *tn = OP_result( cand_op, i );
    if( !TN_is_constant(tn) && !TN_is_global_reg(tn) && !TN_is_dedicated(tn) ){
      promoted_tns = TN_SET_Union1D( promoted_tns, tn, &gcm_loop_pool );
    }
  }

  for(int i=0; i < opnd_num; i++ ){
    TN *tn = OP_opnd( cand_op, i );
    if( !TN_is_constant(tn) && !TN_is_global_reg(tn) && !TN_is_dedicated(tn) ){
      promoted_tns = TN_SET_Union1D( promoted_tns, tn, &gcm_loop_pool );
    }
  }
}

// ======================================
// OP_Move_Heuristic
//  return TRUE : if it meets the requirements of code motion heuristic
//         FALSE: if not
// ======================================
static BOOL All_OPs_Visited( BB* bb );
static BOOL
OP_Move_Heuristic( OP *cur_op, mINT32 motion_type, BB *src_bb, BB *tgt_bb )
{
  // To make life easier, I won't move the last op of zdl loop's 
  // tail BB. This may happend in Single-BB ZDL.
  // Since this OP has tag, if it's moved, the 'loop'
  // instruction needs to be changed according to many situations
  // Another concern, if all the OPs in zdl body are circ-moved,
  // the loop count needs to decrease by 1. 
  if( OP_has_tag(cur_op) && ( BB_length(src_bb)==1 || All_OPs_Visited(src_bb) ) )
    return FALSE;

  std::vector<OP*>::const_iterator cit = last_defs_of_branch.begin();
  for( ; cit != last_defs_of_branch.end(); cit++ ){
    if( *cit == cur_op ){
      if( Trace_GCM )
        fprintf( TFile, "cur_op is skipped for it is last def of branch" );
      return FALSE;
    }
  }

  if( Motion_Is_CIRC_ABOVE(motion_type) && circ_above_moves > circ_above_limit ){
    if( Trace_GCM )
      fprintf( TFile, "cur_op is skipped for exceeds the limit of circ_above" );
    return FALSE;
  }

  if( Motion_Is_CIRC_ABOVE(motion_type) &&
      TN_SET_Size(promoted_tns) > local_tn_promotion_limit ){
    if( Trace_GCM )
      fprintf( TFile, "cur_op is skipped for exceeds the limit of local tn promotion in circ_above motion" );
    return FALSE;
  }

  return TRUE;
}
#endif // TARG_SL

// ======================================================================
// Can_OP_Move
// returns TRUE if <op> can be moved thru all blocks between <src_bb> and
// <tgt_bb>. 
// ======================================================================
static BOOL
Can_OP_Move(OP *cur_op, BB *src_bb, BB *tgt_bb, BB_SET **pred_bbs, 
	    void *defs[2], void *uses[2], mINT32 motion_type,mUINT8 *spec_type)
{
   BB *cur_bb;
   REGISTER_SET mid_reg_defs[ISA_REGISTER_CLASS_MAX+1], 
		mid_reg_uses[ISA_REGISTER_CLASS_MAX+1];
   GTN_SET *mid_gtn_defs, *mid_gtn_uses;
   TN_SET *mid_tn_defs, *mid_tn_uses;

   // if the requested motion type is speculation and <cur_op> cannot be
   // speculated, return FALSE. need to consider possibility of doing safe
   // speculation (or predication) as well.
   BOOL can_spec = TRUE;
   BOOL safe_spec = FALSE;
   *spec_type = SPEC_NONE;
   if (motion_type & (GCM_SPEC_ABOVE | GCM_SPEC_BELOW | GCM_CIRC_ABOVE)) {
     /* if cur_op is expensive, needn't speculate it. But if the motion type
      * is circ_above, and the OP is a load, then it's benefit to move it
      */ 
     if( !( Motion_Is_CIRC_ABOVE(motion_type) && OP_load(cur_op) ) )
       if (OP_Is_Expensive(cur_op)) 
         return FALSE;
#if defined(TARG_SL)
     if( !CGTARG_Can_Be_Speculative (cur_op) ){
       // So I disabled spec_above/below if cgtarget cannot be speculative,
       if( !Motion_Is_CIRC_ABOVE(motion_type) ) 
         return FALSE;
       else 
	 can_spec = FALSE; 
     }
   } 

   if (Ignore_TN_Dep) {
     REGSET_CLEAR(mid_reg_defs);
     REGSET_CLEAR(mid_reg_uses);
   } else {
     // initialize mid_gtn_uses to be the live-in GTN set.
     mid_gtn_defs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
     mid_gtn_uses = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);

     mid_tn_defs = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);
     mid_tn_uses = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);

     GTN_SET_ClearD (mid_gtn_defs);
     GTN_SET_ClearD (mid_gtn_uses);
   }

   // Second, check for register dependences.
   FOR_ALL_BB_SET_members (*pred_bbs, cur_bb) {

     if (CG_Skip_GCM && BB_id(cur_bb) == GCM_To_BB)
       return FALSE;

     BBLIST *succ_list;
     // check to see if def of cur_op is live out on a successor that's not
     // on the pred_bb's set. 
     // Don't need to check if <cur_bb> == <src_bb>,  since all the successors 
     // of <src_bb> are unnecessary, and the cur_op is moved inside its own
     // BB. [NOTICE]- However, if it's the circular motion, which moves cur_op
     // from the head to tail of single BB (loop body), and also duplicates it 
     // in the loop prolog. That means it increases
     // the loop count by one. the result is incorrect. So we need to check it.
     // TODO: need to maintain a similar list of <succ_bbs> for downward
     // code motion.
     if (cur_bb != src_bb || ( Motion_Is_CIRC_ABOVE(motion_type) && 
                               cur_bb==src_bb) ) {
       for (INT i = 0; i < OP_results(cur_op); ++i) {
	 TN *result = OP_result(cur_op,i);
	 if (CG_Skip_GCM && TN_number(result) == GCM_Result_TN)
	   return FALSE;
	 FOR_ALL_BB_SUCCS(cur_bb, succ_list) {
	   BB *succ_bb = BBLIST_item(succ_list);
	   if (!BB_SET_MemberP(*pred_bbs, succ_bb)) {
	     if (Ignore_TN_Dep) {
	       ISA_REGISTER_CLASS result_cl = TN_register_class (result);
	       REGISTER result_reg = TN_register (result);
	       if (REG_LIVE_Into_BB (result_cl, result_reg, succ_bb) ||
		   
		   // #776729: Sometimes during circular-scheduling, we
		   // insert new blocks. These blocks don't have their
		   // REG_LIVE sets updated, because REG_LIVE can't cope
		   // with interactive updates (fixed-structures, uugghhh !!!)
		   // As a workaround, we also check for GRA_LIVE here.
		   // REAL FIX: To interactively update REG_LIVE sets.

		   (TN_is_global_reg(result) &&
		    GTN_SET_MemberP(BB_live_in(succ_bb), result)))
		 return FALSE;
	     } else {
	       if (TN_is_global_reg(result) &&
		   (GTN_SET_MemberP(BB_live_in(succ_bb), result) ||
		    GTN_SET_MemberP(BB_live_def(succ_bb), result)))
		 return FALSE;
	     }
	   }
	 }
       }
     }

     mUINT32 cur_spec_type;
     // need to check for memory alias dependences here
     if ( OP_memory(cur_op) &&
          !Can_Mem_Op_Be_Moved(cur_op, cur_bb, src_bb, tgt_bb,
                               motion_type, &cur_spec_type))
       return FALSE;

     /* HD - But I like to set Use_Page_Zero aggresively */
     Use_Page_Zero = TRUE;
#else // TARG_SL
     if (!CGTARG_Can_Be_Speculative (cur_op)) {
       // excluding loads for further pruning.
       if (!OP_load(cur_op)) return FALSE;
       else {
	 can_spec = FALSE; // save the state

	 // At this point, <cur_op> is a possible unsafe load. Perform the 
	 // following safety checks in the prescribed order.
	 // (1) Do safe predication first, i.e if there exists a predicate 
	 //     which previously controlled the branch, use the predicate 
	 //     to control it's execution.
	 // (2) Do eager_ptr_speculation next. Check for safety references in 
	 //     the target block and deduce that it's safe to move.
	 // (3) Check for any target-specific speculative loads.
	 // (4) Check for NULL pointer speculation.

	 // if PROC has delayed exception mechanism, do specific tests.
	 // Check to see if instruction can be converted to safe OP using
	 // predication.

	 if (PROC_has_delayed_exception()) {
	   if (GCM_Pointer_Spec && GCM_Predicated_Loads &&
	       Can_Do_Safe_Predicate_Movement(cur_op, src_bb, tgt_bb, motion_type)) {
	     safe_spec = TRUE;
	     Set_PSAFE_PTR_SPEC(*spec_type);
	   }
	 } 

	 // Assumes OP_load only. Invoke eager_ptr_speculation routine 
	 // to check if there exist any valid memory references in 
	 // target_block which match <cur_op>.
	 
	 if (GCM_Pointer_Spec) {
	   if (GCM_Eager_Ptr_Deref && 
	       Eager_Ptr_Deref_Spec(cur_op, tgt_bb, TRUE)) {
	     safe_spec = TRUE;
	     Set_EAGER_PTR_SPEC(*spec_type);
	   }
#ifndef TARG_MIPS
	   else if (GCM_Speculative_Loads && Is_Target_Itanium() &&
		    OP_load(cur_op)) {
	     safe_spec = TRUE;
	     Set_CSAFE_PTR_SPEC(*spec_type);
	   }
#endif
	 }

	 // Check for any NULL ptr speculation cases (MIPS only).
	 if (!PROC_has_delayed_exception() && 
	     Null_Ptr_Deref_Spec(cur_op, src_bb, tgt_bb)) {
	   if (!(GCM_Pointer_Spec && GCM_Eager_Null_Ptr_Deref)) {
	     return FALSE;
	   } else {
	     Set_EAGER_NULL_PTR_SPEC(*spec_type);
	     safe_spec = TRUE;
	   }
	 }
       }
     }
   } // if (motion_type & (GCM_SPEC_ABOVE | GCM_SPEC_BELOW | GCM_CIRC_ABOVE))

   // If memory_op and either it's safe to speculate or has been proven to
   // be safe, by other safety tests, then proceed further.
#ifdef TARG_X8664
   BOOL op_access_mem = OP_memory(cur_op) || OP_load_exe(cur_op);

   /* bug#1470
      An asm instruction could access memory also.
    */
   if( !op_access_mem &&
       OP_code(cur_op) == TOP_asm ){
     ASM_OP_ANNOT* asm_info = (ASM_OP_ANNOT*)OP_MAP_Get(OP_Asm_Map, cur_op);
     for( int i = 0; i < OP_results(cur_op); i++ ){
       if( ASM_OP_result_memory(asm_info)[i] )
	 op_access_mem = TRUE;
     }

     for( int i = 0; i < OP_opnds(cur_op); i++ ){
       if( ASM_OP_opnd_memory(asm_info)[i] )
	 op_access_mem = TRUE;
     }     
   }

   if (op_access_mem && (can_spec || safe_spec)) {
#else
   if (OP_memory(cur_op) && (can_spec || safe_spec)) {
#endif // TARG_X8664
     
     FOR_ALL_BB_SET_members (*pred_bbs, cur_bb) {
     
       if (CG_Skip_GCM && BB_id(cur_bb) == GCM_To_BB)
	 return FALSE;
     
       // need to check for memory alias dependences here
       if (!Can_Mem_Op_Be_Moved(cur_op, cur_bb, src_bb, tgt_bb, motion_type)) 
	 return FALSE;
     
       // detect cases where movement of mem ops past the null ptr test
       // condition can be possible and allow them only when the
       // corresponding flags are true.
       BOOL equiv_fwd  = BS_MemberP (BB_pdom_set(tgt_bb), BB_id(cur_bb)) &&
	                 BS_MemberP (BB_dom_set(cur_bb), BB_id(tgt_bb));
       if (equiv_fwd && Null_Ptr_Deref_Spec(cur_op, src_bb, cur_bb)) {
	 if (!(GCM_Pointer_Spec && GCM_Eager_Null_Ptr_Deref)) {
	   return FALSE;
	 } else {
	   Set_EAGER_NULL_PTR_SPEC(*spec_type);
	   Use_Page_Zero = TRUE;
	 }
       }
     }
   }

   // if <cur_op> can't be speculated (as shown by <can_spec>) and 
   // that the eager_null_ptr test returned FALSE (as shown by <spec_type>)
   // then it's safe to conclude that <cur_op> can't be speculated.
   
   if (!can_spec && 
       !(*spec_type & SPEC_EAGER_NULL_PTR) &&
       !(*spec_type & SPEC_EAGER_PTR) && 
       !(*spec_type & SPEC_CIRC_PTR_ABOVE) &&
       !(*spec_type & SPEC_CSAFE_PTR) &&
       !(*spec_type & SPEC_DSAFE_PTR) &&
       !(*spec_type & SPEC_CDSAFE_PTR) &&
       !(*spec_type & SPEC_PSAFE_PTR))
     return FALSE;

   if (*spec_type & SPEC_EAGER_NULL_PTR) Use_Page_Zero = TRUE;

   if (Ignore_TN_Dep) {
     REGSET_CLEAR(mid_reg_defs);
     REGSET_CLEAR(mid_reg_uses);
   } else {
     // initialize mid_gtn_uses to be the live-in GTN set.
     mid_gtn_defs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
     mid_gtn_uses = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);

     mid_tn_defs = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);
     mid_tn_uses = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);

     GTN_SET_ClearD (mid_gtn_defs);
     GTN_SET_ClearD (mid_gtn_uses);
   }

   // Second, check for register dependences.
   FOR_ALL_BB_SET_members (*pred_bbs, cur_bb) {

     if (CG_Skip_GCM && BB_id(cur_bb) == GCM_To_BB)
       return FALSE;

     BBLIST *succ_list;
     // check to see if def of cur_op is live out on a successor that's not
     // on the pred_bb's set. don't need to check if <cur_bb> == <src_bb> 
     // since all the successors of <src_bb> are unnecessary.
     // TODO: need to maintain a similar list of <succ_bbs> for downward
     // code motion.
     if (cur_bb != src_bb) {
       for (INT i = 0; i < OP_results(cur_op); ++i) {
	 TN *result = OP_result(cur_op,i);
	 if (CG_Skip_GCM && TN_number(result) == GCM_Result_TN)
	   return FALSE;
	 FOR_ALL_BB_SUCCS(cur_bb, succ_list) {
	   BB *succ_bb = BBLIST_item(succ_list);
	   if (!BB_SET_MemberP(*pred_bbs, succ_bb)) {
	     if (Ignore_TN_Dep) {
	       ISA_REGISTER_CLASS result_cl = TN_register_class (result);
	       REGISTER result_reg = TN_register (result);
	       if (REG_LIVE_Into_BB (result_cl, result_reg, succ_bb) ||
		   
		   // #776729: Sometimes during circular-scheduling, we
		   // insert new blocks. These blocks don't have their
		   // REG_LIVE sets updated, because REG_LIVE can't cope
		   // with interactive updates (fixed-structures, uugghhh !!!)
		   // As a workaround, we also check for GRA_LIVE here.
		   // REAL FIX: To interactively update REG_LIVE sets.

		   (TN_is_global_reg(result) &&
		    GTN_SET_MemberP(BB_live_in(succ_bb), result))

#ifdef KEY	   // Continue the above workaround for #776729.  Check
		   // liveness for assigned registers since they are globals
		   // too.  Bug 8726.
		   || (result_reg != REGISTER_UNDEFINED &&
		       GTN_SET_MemberP(BB_live_in(succ_bb),
				       Build_Dedicated_TN(result_cl,
							  result_reg, 0)))
#endif
		   )
		 return FALSE;
	     } else {
	       if (TN_is_global_reg(result) &&
		   (GTN_SET_MemberP(BB_live_in(succ_bb), result) ||
		    GTN_SET_MemberP(BB_live_def(succ_bb), result)))
		 return FALSE;
	     }
	   }
	 }
       }
     }
#endif // TARG_SL

     OP *limit_op;
     limit_op = Find_Limit_OP(cur_op, cur_bb, src_bb, tgt_bb);

     // accumulate all the mid_defs and mid_uses  (register/TN dependences)
     OP *op;
     BOOL forw = motion_type & (GCM_EQUIV_FWD | GCM_SPEC_ABOVE | 
				GCM_DUP_ABOVE | GCM_CIRC_ABOVE);
     for (op = ((forw && cur_bb != src_bb) || (!forw && cur_bb == src_bb)) ?
	    BB_last_op(cur_bb) : BB_first_op(cur_bb);
	  op && op != limit_op;
	  op = ((forw && cur_bb != src_bb) || (!forw && cur_bb == src_bb)) ?
	    OP_prev(op) : OP_next(op)) {

       if (OP_dummy(op)) {
	 if (!CGTARG_Is_OP_Barrier(op)) continue;
	 else if (OP_memory(cur_op)) return FALSE;
       }

       // making sure that we don't check the op with's itself
       if (op == cur_op) continue;

       // making sure that the <call_op> doesn't read/write <cur_op>
       if (OP_call(op) && 
	   !CG_DEP_Can_OP_Move_Across_Call(cur_op, op, forw, Ignore_TN_Dep))
	 return FALSE;

       // accumulate all the mid_defs
       INT i;
       for (i = 0; i < OP_results(op); ++i) {
	 TN *result_tn = OP_result(op,i);
	 if (Ignore_TN_Dep) {
	   REGISTER result_reg = TN_register (result_tn);
	   ISA_REGISTER_CLASS cl = TN_register_class (result_tn);
	   mid_reg_defs[cl] = REGISTER_SET_Union1(mid_reg_defs[cl],result_reg);
	 } else {
	    if (TN_is_global_reg(result_tn)) {
	      mid_gtn_defs = GTN_SET_Union1D(mid_gtn_defs, result_tn,
					    &MEM_local_pool);
	    } else if (cur_bb == src_bb || TN_is_dedicated(result_tn)) {
	      mid_tn_defs = TN_SET_Union1D(mid_tn_defs, result_tn, 
					  &MEM_local_pool);
	    }
	 }
       }

#ifdef KEY
       // Accumulate all clobbers, which are treated like defs.  SiCortex 5044.
       ASM_OP_ANNOT *asm_info = (OP_code(op) == TOP_asm) ?
		      (ASM_OP_ANNOT *) OP_MAP_Get(OP_Asm_Map, op) : NULL;
       if (asm_info) {
	 ISA_REGISTER_CLASS cl;
	 FOR_ALL_ISA_REGISTER_CLASS(cl) {
	   REGISTER_SET clobbers = ASM_OP_clobber_set(asm_info)[cl];
	   if (Ignore_TN_Dep) {
	     mid_reg_defs[cl] = REGISTER_SET_Union(mid_reg_defs[cl], clobbers);
	   } else {
	     REGISTER reg;
	     FOR_ALL_REGISTER_SET_members(clobbers, reg) {
	       TN *ded_tn = Build_Dedicated_TN(cl, reg, 0);
	       mid_gtn_defs = GTN_SET_Union1D(mid_gtn_defs, ded_tn,
					      &MEM_local_pool);
	     }
	   }
	 }
       }
#endif

       // accumulate all the mid_uses
       for (i = 0; i < OP_opnds(op); ++i) {
	 TN *opnd_tn = OP_opnd(op,i);
	 if (TN_is_constant(opnd_tn)) continue;
	 if (Ignore_TN_Dep) {
	   REGISTER opnd_reg = TN_register (opnd_tn);
	   ISA_REGISTER_CLASS cl = TN_register_class (opnd_tn);
	   mid_reg_uses[cl] = REGISTER_SET_Union1(mid_reg_uses[cl], opnd_reg);
	   
	   // In case of circular scheduling, need to prevent speculated
	   // code from executing one extra time, than the condition 
	   // controlling them. This is prevented by converting branch 
	   // operands which control their accesses be converted to definitions
	   // to prevent their movement.
	   if( Motion_Is_CIRC_ABOVE(motion_type) && OP_br(op) )
	     mid_reg_defs[cl] = REGISTER_SET_Union1(mid_reg_defs[cl], opnd_reg);
         }else{
	   if(TN_is_global_reg(opnd_tn)) {
	     mid_gtn_uses = GTN_SET_Union1D(mid_gtn_uses, opnd_tn,
					    &MEM_local_pool);
	   }else if(cur_bb == src_bb || TN_is_dedicated(opnd_tn)) {
	     mid_tn_uses = TN_SET_Union1D(mid_tn_uses, opnd_tn, 
					  &MEM_local_pool);
	   }
	 }
       } // accumulate all the mid_uses
     }
   }

   // if the target of the instruction to be moved is either defined in the
   // middle blocks (output-dependence) OR there are prior uses of the target 
   // in the middle blocks (anti-dependence) OR there are flow- dependences.
   // In reality, these may not be this STRICT and can be relaxed either with
   // TN renaming or inserting predicated ops.

   /* for single BB loop circular motion, no need to check mid-def/mid-use */
   BOOL can_move = (src_bb == tgt_bb && Motion_Is_CIRC_ABOVE(motion_type) ) || 

     // check for register dependences for POST-GCM stage.
     ((Ignore_TN_Dep && !REGSET_INTERSECT((REGSET)defs[0], mid_reg_defs) &&
       !REGSET_INTERSECT((REGSET)defs[0], mid_reg_uses) &&
       !REGSET_INTERSECT((REGSET)uses[0], mid_reg_defs)) ||

      // check for global TN dependences for PRE-GCM stage.
      (!Ignore_TN_Dep && 
       (!GTN_SET_IntersectsP((GTN_SET *)defs[0], mid_gtn_defs) &&
	!GTN_SET_IntersectsP((GTN_SET *)defs[0], mid_gtn_uses) &&
	!GTN_SET_IntersectsP((GTN_SET *)uses[0], mid_gtn_defs)) &&

       (!TN_SET_IntersectsP((TN_SET *)defs[1], mid_tn_defs) &&
	!TN_SET_IntersectsP((TN_SET *)defs[1], mid_tn_uses) &&
	!TN_SET_IntersectsP((TN_SET *)uses[1], mid_tn_defs))));

   return can_move;
}

// ======================================================================
// GTN_Live_Out_From_BB
// Returns TRUE if <opnd_tn> is really live-out from <cand_bb>, i.e. 
// there exists a real use of <opnd_tn> in some successor block. Else,
// return FALSE.
// ======================================================================
static BOOL
GTN_Live_Out_From_BB(BB *cand_bb, TN *use_tn)
{
  BBLIST *succ_list;
  FOR_ALL_BB_SUCCS(cand_bb, succ_list) {
    BB *cur_bb = BBLIST_item(succ_list);
    if (TN_is_global_reg(use_tn) &&
	GTN_SET_MemberP(BB_live_in(cur_bb), use_tn))
      return TRUE;
  }
  
  return FALSE;
}

// ======================================================================
// Update_Live_In_Sets
// The assumption is that <use_tn> is now a no longer a global TN. 
// Therefore, need to update the live-in/live-out sets of all the
// intermediate blocks in <pred_bbs> set to reflect that. 
// ======================================================================
static void
Update_Live_In_Sets(TN *use_tn, BB *src_bb, BB *tgt_bb, BB_SET **pred_bbs)
{

  BB *cur_bb;

  // Check to see if <use_tn> is live-out of any exit from the intermediate
  // blocks in <pred_bbs>.
  
  BOOL tn_use = FALSE;
  FOR_ALL_BB_SET_members (*pred_bbs, cur_bb) {

    if (cur_bb == src_bb) continue;

    BBLIST *succ_list;
    FOR_ALL_BB_SUCCS(cur_bb, succ_list) {
      BB *succ_bb = BBLIST_item(succ_list);
      if ((/* cur_bb != tgt_bb &&  */
	   GTN_SET_MemberP(BB_live_use(cur_bb), use_tn)) ||
	  (!BB_SET_MemberP(*pred_bbs, succ_bb) &&
	  (GTN_SET_MemberP(BB_live_in(succ_bb), use_tn) ||
	   GTN_SET_MemberP(BB_live_def(succ_bb), use_tn)))) {
	tn_use = TRUE;
	break;
      }
    }
  }

  if (!tn_use) {
    FOR_ALL_BB_SET_members(*pred_bbs, cur_bb) {
      if (cur_bb == src_bb || cur_bb == tgt_bb) continue;
    
      // If there exists no use and that it's not live-use, update the
      // live-in/live-out sets of <cur_bb>.
	GRA_LIVE_Remove_Live_In_GTN(cur_bb, use_tn);
	GRA_LIVE_Remove_Live_Out_GTN(cur_bb, use_tn);
    }
  }
}

// ======================================================================
// Update_Live_Use_Counts
// This routine updates the <live_use> counts (if any) for <cur_op> in
// <cur_bb> and updates the <usage_map> structure.
// ======================================================================
static void
Update_Live_Use_Counts(BB *cur_bb, OP *cur_op, hTN_MAP *usage_map)
{
  UINT32 count, *counter, *tn_count;

  for (INT i = 0; i < OP_opnds(cur_op); ++i) {
    TN *opnd_tn = OP_opnd(cur_op, i);
    if (TN_is_constant(opnd_tn)) continue;

    BOOL use_opnd = FALSE;

    // if <opnd_tn> is used in <cur_bb>, or is redef'd (unlikely-case),
    // increment the usage counter.
    if (TN_is_global_reg(opnd_tn) &&
	(GTN_SET_MemberP(BB_live_use(cur_bb), opnd_tn) ||
	 GTN_SET_MemberP(BB_live_def(cur_bb), opnd_tn))) {
      use_opnd = TRUE;
    }

    BBLIST *succ_list;
    FOR_ALL_BB_SUCCS(cur_bb, succ_list) {
      BB *succ_bb = BBLIST_item(succ_list);
      if (TN_is_global_reg(opnd_tn) &&
	  GTN_SET_MemberP(BB_live_in(succ_bb), opnd_tn))
	use_opnd = TRUE;
    }

    // If there exists a use somewhere, increment the count. We are not
    // worried about exact counts, just something more than zero. 
    if (use_opnd) {
      tn_count = (UINT32 *) hTN_MAP_Get (*usage_map, opnd_tn);
      
      count = (tn_count) ? *tn_count : 0;
      counter = (tn_count) ? tn_count : (UINT32 *) malloc(sizeof(UINT32));
      *counter = ++count;
      
      hTN_MAP_Set (*usage_map, opnd_tn, counter);
    }
  }
}

// ======================================================================
// Update_GRA_Live_Sets
// This routine is the placeholder to update all the GRA_LIVE sets as 
// a result of moving <cand_op> from <bb> to <cand_bb>. This needs to be
// updated correctly so that the GRA phase will not be confused. The
// <pred_bbs> is the set containing all BBs between <bb> and <cand_bb>
// whose LIVE sets need to be updated as well.
// ======================================================================
static void
Update_GRA_Live_Sets(OP *cand_op, BB *bb, BB *cand_bb, BB_SET **pred_bbs
#if defined(TARG_SL)
		     , mINT32 motion_type
#endif
		     )
{
  // Update the live info accordingly (PRE-GCM)

  INT i;
  BB *cur_bb;
  BOOL re_def = FALSE;
  hTN_MAP usage_map;
  UINT32 count, *counter, *tn_count;

  MEM_POOL_Push(&MEM_local_pool);
  usage_map = hTN_MAP_Create (&MEM_local_pool);


  // (1) First calculate OP counts for all TNs defined (and referenced) in 
  // <bb>.

  OP *succ_op;
  FOR_ALL_BB_OPs_FWD(bb, succ_op) {
    if (OP_dummy(succ_op)) continue;
    TN *result_tn = NULL;
    for (i = 0; i < OP_results(succ_op); ++i) {
      result_tn = OP_result(succ_op,i);
      tn_count = (UINT32 *) hTN_MAP_Get (usage_map, result_tn);

      count = (tn_count) ? *tn_count : 0;
      counter = (tn_count) ? tn_count : (UINT32 *) alloca(sizeof(UINT32));
      *counter = ++count;
      
      hTN_MAP_Set (usage_map, result_tn, counter);
    }

    for (i = 0; i < OP_opnds(succ_op); ++i) {
      TN *opnd_tn = OP_opnd(succ_op, i);
      if (TN_is_constant(opnd_tn)) continue;
      
      // if <opnd_tn> is a global reg, need to increment its usage 
      // accordingly.
      if (TN_is_global_reg(opnd_tn)) {
	tn_count = (UINT32 *) hTN_MAP_Get (usage_map, opnd_tn);
	
	count = (tn_count) ? *tn_count : 0;
	counter = (tn_count) ? tn_count : (UINT32 *) alloca(sizeof(UINT32));
	*counter = ++count;

	hTN_MAP_Set (usage_map, opnd_tn, counter);
      }
    }
  }

  // (2): Check to see if any of the result ans opnd TNs of <cand_op> are
  // now live-in or live-out because of the movement. Update the Live-Sets
  // accordingly.

  for (i = 0; i < OP_results(cand_op); ++i) {
    BOOL use_before_def = FALSE;

    TN *result = OP_result(cand_op, i);
    FOR_ALL_BB_OPs_FWD(bb, succ_op) {
      if (OP_dummy(succ_op)) continue;
      TN *result_tn = NULL;
      INT j;

      for (j = 0; j < OP_opnds(succ_op); ++j) {
	TN *opnd_tn = OP_opnd(succ_op, j);
	if (TN_is_constant(opnd_tn)) continue;

	// Add it to the GRA Universe set to register it as a global TN.
	if (!re_def && result &&
	    (TN_number(result) == TN_number(opnd_tn))) {
          use_before_def = TRUE;
	  GTN_UNIVERSE_Add_TN(opnd_tn);
	  // This should come only after <opnd_tn> has been added to 
	  // GTN_UNIVERSE
	  GRA_LIVE_Add_Live_Use_GTN(bb, opnd_tn);
	  GRA_LIVE_Add_Live_In_GTN(bb, opnd_tn);
	}
      }

      for (j = 0; j < OP_results(succ_op); ++j) {
	result_tn = OP_result(succ_op, j);
	// if there has been a redefinition of <result_tn>, no need to make
	// any further local use of the result TN's as a global TN.
        // Notice, there may be cond_def OP. 
	if( result_tn && result && 
            TN_number(result) == TN_number(result_tn) 
#if defined(TARG_SL)
            && !OP_cond_def(succ_op) 
#endif
	    ) 
          re_def = TRUE;
      }
    }

    // Remove the <result> TN from <bb>_live_def and add it to 
    // <cand_bb>_live_def set

    if (result) {
      if( TN_is_global_reg(result) ) {
#if defined(TARG_SL)
        if( use_before_def || !re_def )
          GRA_LIVE_Remove_Live_Def_GTN(bb, result);
        else
          DevWarn("Doubly defined, without usage");
#else 
          GRA_LIVE_Remove_Live_Def_GTN(bb, result);
#endif
      }
      GTN_UNIVERSE_Add_TN(result);
      GRA_LIVE_Add_Live_Def_GTN(cand_bb, result);
    }

    // Now update the live-in/defreach-in/live-out/defreach-out sets of all
    // the intermediate BBs as well. There is phase-ordering issue here. This
    // should come only after <result> is made a GTN.

    FOR_ALL_BB_SET_members (*pred_bbs, cur_bb) {
      if( ( cand_bb!=cur_bb 
#if defined(TARG_SL)
	    || Motion_Is_CIRC_ABOVE(motion_type) 
#endif
	    ) && result ) {
	GRA_LIVE_Add_Live_In_GTN(cur_bb, result);
	GRA_LIVE_Add_Defreach_In_GTN(cur_bb, result);
      }

      // check for intermediate uses of any operands TNs of <cand_op> 
      // from the path between <cur_bb> and <cand_bb>.
      if (cur_bb != bb && cur_bb != cand_bb)
	Update_Live_Use_Counts(cur_bb, cand_op, &usage_map);

      tn_count = result ? (UINT32 *) hTN_MAP_Get (usage_map, result) : NULL;
      count = (tn_count) ? *tn_count : 0;

      // Need to update only if <cur_bb != bb> or that there are other defs
      // besides <cand_op> coming out of <bb>.
      if( result && ( cur_bb != bb || count != 0  
#if defined(TARG_SL)
                      || Motion_Is_CIRC_ABOVE(motion_type)
#endif
		      ) ) {
	GRA_LIVE_Add_Live_Out_GTN(cur_bb, result);
	GRA_LIVE_Add_Defreach_Out_GTN(cur_bb, result);
      }
    }
  }

  // (3) Add the corresponding operand uses of <cand_op> to <cand_bb> live_use 
  // set and remove it from <bb> live-use accordingly (i.e. if there are
  // no other succeeding uses)

  for (i = 0; i < OP_opnds(cand_op); ++i) {
    TN *opnd_tn = OP_opnd(cand_op, i);
    if (TN_is_constant(opnd_tn)) continue;

    BOOL single_use = FALSE;

    if (TN_is_global_reg(opnd_tn)) {
      
      // There exists more than one use of <opnd_tn> in <cand_bb>
      if (!GTN_SET_MemberP(BB_live_in(cand_bb), opnd_tn)) {
	single_use = TRUE;
      }
      
      GRA_LIVE_Add_Live_Use_GTN(cand_bb, opnd_tn);

      if (!GTN_SET_MemberP(BB_live_def(cand_bb), opnd_tn))
	GRA_LIVE_Add_Defreach_In_GTN(cand_bb, opnd_tn);

      tn_count = (UINT32 *) hTN_MAP_Get (usage_map, opnd_tn);
      count = (tn_count) ? *tn_count : 0;

      if (count == 0) {
	// There are no more uses of <opnd_tn> in <bb>, so remove it
	// from the live_use set. If <opnd_tn> is not live-out (i.e. last
	// use), remove the live-in entry as well.
	GRA_LIVE_Remove_Live_Use_GTN(bb, opnd_tn);

	if (!GTN_SET_MemberP(BB_live_out(bb), opnd_tn))
	  GRA_LIVE_Remove_Live_In_GTN(bb, opnd_tn);
	Update_Live_In_Sets(opnd_tn, bb, cand_bb, pred_bbs);

	// Sometimes, a local TN becomes a GTN if it has a future
	// use. However, if the use is also moved, the GTN def can be
	// converted back to a local def. check for those cases.
	if (GTN_SET_MemberP(BB_live_def(cand_bb), opnd_tn) &&
	    !GTN_Live_Out_From_BB(cand_bb, opnd_tn)) {
	  UINT32 *opndtn_count = (UINT32 *) hTN_MAP_Get (usage_map, opnd_tn);
	  UINT32 opnd_count = (opndtn_count) ? *opndtn_count : 0;	  
	  if (opnd_count == 0 && single_use) {
	    GRA_LIVE_Remove_Live_Use_GTN(cand_bb, opnd_tn);
	    GRA_LIVE_Remove_Defreach_Out_GTN(cand_bb, opnd_tn);
	    GRA_LIVE_Remove_Live_Out_GTN(cand_bb, opnd_tn);
	  } 
	}
      }
    }
  }

  MEM_POOL_Pop(&MEM_local_pool);
}
		     
// ======================================================================
// Is_OP_Move_Better
// need to take into account all factors (here) in deciding whether 
// <cur_op> is better than <best_op>. The decision is made purely on
// source and target blocks properties. Op's properties are decided later
// once it's chosen that this op is really better
// ======================================================================
static BOOL
Is_OP_Move_Better(OP *cur_op, OP *best_op, mINT32 motion_type)
{

  // TODO: compute the less bottleneck resources in target block. this is 
  // necessary to decide profitability of picking an op

  switch (motion_type) {

  case GCM_EQUIV_FWD:
  case GCM_EQUIV:
  case GCM_SPEC_ABOVE:
  case GCM_CIRC_ABOVE:
	// arbitrary heuristic: used just to test the code
	if (OP_scycle(cur_op) < OP_scycle(best_op)) return TRUE;
	if (OP_scycle(cur_op) >= OP_scycle(best_op)) return FALSE;
	break;
  case GCM_EQUIV_BKWD:
  case GCM_SPEC_BELOW:
	// arbitrary heuristic: used just to test the code
	if (OP_scycle(cur_op) > OP_scycle(best_op)) return TRUE;
	if (OP_scycle(cur_op) <= OP_scycle(best_op)) return FALSE;
	break;
  case GCM_DUP_ABOVE:
  case GCM_DUP_BELOW:
	break;
  default:
	FmtAssert(FALSE, ("unexpected code motion type in GCM phase"));
  }
  return FALSE;
}

#if defined(TARG_SL)
static BOOL
Enough_Circ_Loop_Trip_Count (LOOP_DESCR* l) {

    LOOPINFO* li LOOP_DESCR_loopinfo(l);

    TN* trip_count = NULL; 
    if (li) {
        trip_count = LOOPINFO_trip_count_tn(li); 
    }
    
    if (trip_count && TN_is_constant(trip_count) && 
        TN_has_value(trip_count)) {
        if(TN_value(trip_count)<circ_above_tc_limit) 
          return FALSE;
    }

    return TRUE;
}
#endif

// ======================================================================
// Determine_Motion_Type
// need to take into account all factors (here) in deciding the 
// appropriate motion_type for this <bb> present in <loop>. <bbsch> is used 
// to derive the properties of this basic block
// ======================================================================
static mINT32
Determine_Motion_Type(LOOP_DESCR *loop, BB *bb, BBSCH *bbsch)
{
  mINT32 motion_type;

  // Do forward circular motion
  if (GCM_Forw_Circ_Motion && !BB_CIRC_ABOVE(bbsch) && 
      /* HD - Don't know why the Trace Option is affected to motion type*/
      //!Get_Trace(TP_GCM, 0x0080) &&
      
      // Currently enable circular scheduling during post-GCM stage.
#if !defined(TARG_SL)
      Ignore_TN_Dep &&
#endif
      // Circular scheduling for fully unrolled-loops doesn't make sense.
      (LOOP_DESCR_loophead(loop) == bb) && !BB_unrolled_fully(bb) &&

#if defined(TARG_SL)
      // Circular scheduling for small trip count doesn't make sense
      Enough_Circ_Loop_Trip_Count(loop) &&
#endif

      // Circular scheduling requires a new prolog, make sure we can add one.
      LOOP_DESCR_Can_Retarget_Loop_Entrances(loop) &&

      // TODO: we don't yet know how to handle loophead blocks which
      // contain memory barrier insructions. Avoid processing region blocks.
      // Too complicated.
      !BB_MEM_BARRIER(bbsch) && (BB_rid(bb) == NULL)) {
    motion_type = GCM_CIRC_ABOVE;
    Set_BB_CIRC_ABOVE(bbsch);
  }
  // Do equivalent code motion first since it's always useful and will
  // never result in any degradation.
  else if (!BB_EQUIV_FWD(bbsch) /*&& !Get_Trace(TP_GCM, 0x0020)*/) {
    motion_type = GCM_EQUIV_FWD;
    Set_BB_EQUIV_FWD(bbsch);
  } 
  // Do forward speculation
#if !defined(TARG_SL)
  // TODO: I'll enable this later - TARG_SL
  else if (!BB_SPEC_ABOVE(bbsch) /*&& !Get_Trace(TP_GCM, 0x0040)*/) {
    motion_type = GCM_SPEC_ABOVE;
    Set_BB_SPEC_ABOVE(bbsch);
  } 
#endif
  // Do nothing. 
  else 
    motion_type = GCM_NONE;

  return motion_type;
}

// ======================================================================
// Determine_Candidate_Blocks
// need to take into account all factors (here) in deciding the 
// appropriate set of candidate blocks <cand_bbvector> for this bb for the
// concerned <motion_type>
// ======================================================================
static void
Determine_Candidate_Blocks(BB *bb, LOOP_DESCR *loop, mINT32 motion_type,
			VECTOR *priority_vector, VECTOR *cand_bbvector)
{
  BB *cand_bb;
  for (INT i = 0; i < VECTOR_count(*priority_vector); i++) {
    cand_bb = (BB *)VECTOR_element(*priority_vector, i);

    if (cand_bb == bb) continue;

#ifdef TARG_X8664
    // Don't mess with GOT computation.  Bug 14452.
    if (Avoid_GOT_BB(cand_bb))
      continue;
#endif

    if (BB_scheduled(cand_bb) && !BB_scheduled_hbs(cand_bb)) continue;

    // don't consider empty blocks as they along with branches around them
    // can possibly be eliminated (by cflow)
    if (Is_BB_Empty(cand_bb)) continue;

    // differentiate the cases between equiv_fwd and equiv_bkwd separately.
    BOOL equiv_bkwd =  	BS_MemberP (BB_pdom_set(bb), BB_id(cand_bb)) &&
		     	BS_MemberP (BB_dom_set(cand_bb), BB_id(bb));
    BOOL equiv_fwd  = 	BS_MemberP (BB_pdom_set(cand_bb), BB_id(bb)) &&
			BS_MemberP (BB_dom_set(bb), BB_id(cand_bb));

#ifdef KEY
    /* Fix for bug#1406
       Although <cand_bb> dominates <bb>, and <bb> post-dominates <cand_bb>,
       it does not mean they are really equivalent, if <cand_bb> has two branches,
       and one lead to another loop.
       Should we fix up <equiv_bkwd> here ???
     */
    equiv_fwd = equiv_fwd && BB_Has_One_Succ( cand_bb );
#endif

    BOOL equiv = equiv_fwd && equiv_bkwd;

    // don't try candidate blocks that are above/below a frequency threshold
    // make the knob higher if frequency is feedback- directed
    if ((BB_freq(cand_bb) * (CG_PU_Has_Feedback ?
                             speculation_ratio_fb : speculation_ratio_wfb))
                > BB_freq(bb)) {


        if (!CG_PU_Has_Feedback && !equiv) {

	   // (1) Special case for speculative execution:
	   // where <bb> has a very few instructions and allowing them to
	   // speculate in <cand_bb> (if profitable) will also result
	   // in eliminating the unconditional branch around it. 
	   // Don't want to do this if feedback info is provided.
	 
           if ((BB_branch_op(bb) != NULL) &&
                (BB_Unique_Predecessor(bb) == cand_bb) &&
                (BB_length(bb) < 4)) goto profitable_candidate;
	   else continue;
       } else if (!GCM_Test) continue;
    }

profitable_candidate:
    // if <cand_bb> is a loophead bb comprising of more than one loop, then 
    // it isn't safe to move ops there. pv704881. more pruning needs to be
    // done.
    BOOL multiple_loop = (BB_loop_head_bb(cand_bb) && 
                         BB_in_succs(bb, cand_bb) && 
                         (BB_preds_len(cand_bb) > 2));

    // other metrics like block properties, FU-usage counts etc. also need
    // to be considered here for candidate blocks selections.
    BOOL cond = FALSE;

    switch (motion_type) {
	
    case GCM_EQUIV_FWD:
      cond = equiv_fwd && !multiple_loop;
      break;	

    case GCM_EQUIV_BKWD:
      cond = equiv_bkwd;
      break;

    case GCM_EQUIV:
      cond = equiv;
      break;

      // need to fill the holes here as well
    case GCM_SPEC_ABOVE:
      if (!multiple_loop &&
	  BS_MemberP (BB_dom_set(bb), BB_id(cand_bb)) &&
	  !BS_MemberP (BB_pdom_set(cand_bb), BB_id(bb)))
	cond = TRUE;
      break;

    case GCM_CIRC_ABOVE:
      break;

    case GCM_SPEC_BELOW:
    case GCM_DUP_ABOVE:
    case GCM_DUP_BELOW:

    default:
      FmtAssert(FALSE, ("unexpected code motion type in GCM phase"));
    }
    // add the candidate block if the condition is true and proceed
    if (cond) {
     VECTOR_Add_Element(*cand_bbvector, (void *)cand_bb);
    }
  } 
  // if <motion_type> is forward circular motion (bottom-loading) and the
  // nesting level of the loop is greater than zero, add the unique tail
  // block to the candidate blocks list.
  if( Motion_Is_CIRC_ABOVE(motion_type) ){
    if ((LOOP_DESCR_nestlevel(loop) > 0) && 
	(cand_bb = LOOP_DESCR_Find_Unique_Tail(loop)))
      VECTOR_Add_Element(*cand_bbvector, (void *)cand_bb);
  }
}


#if defined(TARG_SL)
/* Pre-Declaration */
static BOOL Append_Op_To_BB(OP *cand_op, BB *cand_bb, BB *src_bb, mINT32 motion_type, mUINT8 spec_type);

// ========================================================================
// Loop_Is_Zdl
//
// return the prolog if it's a zdl, else return NULL
// ========================================================================
BB* Loop_Is_Zdl( LOOP_DESCR *loop )
{
  BB *head = LOOP_DESCR_loophead( loop );
  BB *prolog = BB_prev( head );
  if( !prolog )
    return NULL;

  while( Is_BB_Empty(prolog) )
    prolog = BB_prev(prolog);

  // Sometimes CFLow will make the CFG ugly,
  // There are one problems in CFlow, it may creates BB like:
  // BB:1
  //   ...
  //   loop 0, tag_xxx
  //   jp .BB:3
  // BB:2
  //   [other BBs not related to this loop]
  // BB:3
  //   ...loop body..
  // .tag_xxx
  //
  // So if I cannot find the prolog through OP_prev(), then have to
  // go create prolog before BB:3, and move 'loop' into that prolog.
  // Only then, can I move code from loop body BEFORE 'loop'
  OP *loop_op = BB_last_op( prolog );
  if( !OP_is_loop(loop_op) ){
    BBLIST *preds_list;
    prolog = NULL;
    FOR_ALL_BB_PREDS( head, preds_list ){
      if( BB_loop_op(BBLIST_item(preds_list)) ){
        if( prolog )
          Is_True( 0, ("multi loop insn meet on a single loop head") );
        prolog = BBLIST_item( preds_list );
      }
    }
  }

  return prolog;
}

// ========================================================================
// Loop_Is_Straight_Line
//
// return true if the loop body is straight line code
// ========================================================================
static BOOL Loop_Is_Straight_Line( LOOP_DESCR *loop )
{
  BB *temp;
  BB_SET *loop_bbs = LOOP_DESCR_bbset(loop);
  FOR_ALL_BB_SET_members( loop_bbs, temp ){
    if( BB_branch_op(temp) || BB_call(temp) || BB_loop_op(temp) )
      return FALSE;
    
  }
  return TRUE;
}
#endif // TARG_SL

// =======================================================================
// Perform_Post_GCM_Steps
// is responsible (if any) for cleanup tasks after performing
// the code motion type as described in <motion_type> on <cand_op>
// success when <TRUE> tells that the code motion has been successful
// otherwise revert some of the changes (eg. ldst/addiu fixups that need
// to be restored to their original form)
// =======================================================================
// ========================================================================
// Perform_Post_GCM_Steps
//
// ========================================================================
static void
Perform_Post_GCM_Steps(BB *bb, BB *cand_bb, OP *cand_op, mINT32 motion_type,
		       mUINT8 spec_type, BB_SET **pred_bbs, 
		       LOOP_DESCR *loop, BOOL success)
{
  BBSCH *bbsch = (BBSCH *)BB_MAP_Get (bbsch_map, bb);
  BBSCH *cand_bbsch = (BBSCH *)BB_MAP_Get (bbsch_map, cand_bb);

  // one post-GCM step is to avoid speculation of two output dependent
  // variables simultaneously.
  // eg. if (cond) x =5; else x=3;
  // to avoid speculation of write of "x" from both paths, need to update
  // the live-in sets after speculating the first one so that suceeding 
  // speculation which computes a new value that is live on exit from the 
  // original target block is avoided.

  if (success) {
    if (motion_type & (GCM_SPEC_ABOVE | GCM_EQUIV_FWD | GCM_CIRC_ABOVE)) {

      INT i;
      if (Ignore_TN_Dep) {
	for (i = 0; i < OP_results(cand_op); ++i) {
	  TN *result = OP_result(cand_op, i);
	  BB *cur_bb;

	  // Update the live info accordingly (POST-GCM)
	  REGISTER result_reg = TN_register(result);
	  ISA_REGISTER_CLASS result_cl = TN_register_class (result);
	  FOR_ALL_BB_SET_members (*pred_bbs, cur_bb) {
	    REG_LIVE_Update(result_cl, result_reg, cur_bb);
	  }
	  REG_LIVE_Update(result_cl, result_reg, bb);
	}
      }
	
      // Extra work needed for GCM_CIRC_ABOVE. 
      // (1) Generate loop prolog blocks (if not already present)
      // (2) Move <cand_op> to the prolog block and insert the predicate 
      //     (if reqd).
      if( Motion_Is_CIRC_ABOVE(motion_type) ) {

	// If <GCM_Loop_Prolog> not already present, generate a new one and 
	// prepend it to the loop.
	if (GCM_Loop_Prolog == NULL) {
#if defined(TARG_SL)
          // ZDL loop already has a prolog, it contains 'loop' instruction as
          // the last instruction. There is a problem, CFlow make some zdl 
          // prolog loose the zdl-prolog flag.
          BOOL need_move_loop_op = FALSE;
          if( BB_zdl_body(bb) || Loop_Is_Zdl(loop) ){
            Is_True( bb == LOOP_DESCR_loophead(loop), 
                     ("the src bb of GCM_CIRC_ABOVEis not loop head") );
            BB* zdl_prolog = Loop_Is_Zdl( loop );
            // There are one problems in CFlow, it may creates BB like:
            // BB:1
            //   ...
            //   loop 0, tag_xxx
            //   jp .BB:3
            // BB:2
            //   [other BBs not related to this loop]
            // BB:3 
            //   ...loop body..
            // .tag_xxx 
            //
            // So if I cannot find the prolog through OP_prev(), then have to
            // go create prolog before BB:3, and move 'loop' into that prolog.
            // Only then, can I move code from loop body BEFORE 'loop'
            if( zdl_prolog )
              GCM_Loop_Prolog = zdl_prolog;
          }

          if( !GCM_Loop_Prolog ){
	    GCM_Loop_Prolog = CG_LOOP_Gen_And_Prepend_To_Prolog(bb, loop);
          }
#else    
	  GCM_Loop_Prolog = CG_LOOP_Gen_And_Prepend_To_Prolog(bb, loop);
#endif // TARG_SL
	  GRA_LIVE_Compute_Liveness_For_BB(GCM_Loop_Prolog);
#ifdef KEY
	  // Need to update register liveness info for newly generated blocks.
	  // There is no procedure to copy liveness info from one block to 
	  // another. Better run REG_LIVE_Analyze_Region once.
	  if (Ignore_TN_Dep) {
	    REG_LIVE_Finish();
	    REG_LIVE_Analyze_Region();
	  }
#endif	
	  if (Trace_GCM) {
#pragma mips_frequency_hint NEVER
	    fprintf (TFile, "GCM: Circular Motion:\n");
	    fprintf (TFile, "GCM: Add New BB:%d before loophead BB:%d\n", 
		     BB_id(GCM_Loop_Prolog), BB_id(bb));
	  }
	} else {
	  if (Trace_GCM) {
#pragma mips_frequency_hint NEVER
	    fprintf (TFile, "GCM: Circular Motion:\n");
	    fprintf (TFile, "GCM: using existing loop prolog BB:%d \n", 
		     BB_id(GCM_Loop_Prolog));
	  }
	}

	// Update the dominator sets appropriately.
	OP *br_op = BB_branch_op(GCM_Loop_Prolog);
	BB *new_bb;
	if (br_op) {
	  new_bb = CG_LOOP_Append_BB_To_Prolog(GCM_Loop_Prolog, bb);
	  GRA_LIVE_Compute_Liveness_For_BB(new_bb);
	  Set_BB_dom_set(new_bb, BS_Create_Empty(2+PU_BB_Count+1, 
						 &gcm_loop_pool));
	  BS_Union1D(BB_dom_set(new_bb), BB_id(new_bb), NULL);
	  BS_UnionD(BB_dom_set(new_bb), BB_dom_set(bb), &gcm_loop_pool);
	    
	  Set_BB_pdom_set(new_bb, BS_Create_Empty(2+PU_BB_Count+1, 
						  &gcm_loop_pool));
	  BS_Union1D(BB_pdom_set(new_bb), BB_id(new_bb), NULL);
	  BS_UnionD(BB_pdom_set(new_bb), BB_pdom_set(bb), &gcm_loop_pool);
	  Run_Cflow_GCM = TRUE;
	}

	OP *dup_op = Dup_OP(cand_op);
#if defined(TARG_SL)
        // No need to duplicate the tags into prolog 
        if( OP_has_tag(dup_op) )
          Reset_OP_has_tag(dup_op); 
        Append_Op_To_BB( dup_op, GCM_Loop_Prolog, bb, motion_type, spec_type);
        *pred_bbs = BB_SET_Union1D(*pred_bbs,GCM_Loop_Prolog,&MEM_local_pool);
#else
	BB_Append_Op(GCM_Loop_Prolog, dup_op);
#endif  // TARG_SL

	if (PSAFE_PTR_SPEC(spec_type)) 
	  Set_OP_opnd(dup_op, OP_PREDICATE_OPND, True_TN);
	//copy the dom/pdom sets to the new_bb
	Set_BB_dom_set(GCM_Loop_Prolog, BS_Create_Empty(2+PU_BB_Count+1, 
							&gcm_loop_pool));
	BS_Union1D(BB_dom_set(GCM_Loop_Prolog), BB_id(GCM_Loop_Prolog), 
		   NULL);
	BS_UnionD(BB_dom_set(GCM_Loop_Prolog), BB_dom_set(bb), 
		  &gcm_loop_pool);
        BS_Difference1D( BB_dom_set(GCM_Loop_Prolog), BB_id(bb) );

	Set_BB_pdom_set(GCM_Loop_Prolog, BS_Create_Empty(2+PU_BB_Count+1, 
							 &gcm_loop_pool));
	BS_Union1D(BB_pdom_set(GCM_Loop_Prolog), BB_id(GCM_Loop_Prolog), 
		   NULL);
	BS_UnionD(BB_pdom_set(GCM_Loop_Prolog), BB_pdom_set(bb), 
		  &gcm_loop_pool);
	
	if (Trace_GCM) {
#pragma mips_frequency_hint NEVER
	  Print_Trace_File(cand_op, bb, GCM_Loop_Prolog, TRUE);
	}
      }

      // Update GRA_LIVE sets for pre-GCM phase.
      if(!Ignore_TN_Dep) 
        Update_GRA_Live_Sets(cand_op, bb, cand_bb, pred_bbs
#if defined(TARG_SL)
			     , motion_type
#endif
			     );
      // since the motion was successful, need to update the info
      // dynamically.
      if (bbsch && cand_bbsch) {
	BBSCH_num_real_ops(bbsch)--;
	BBSCH_num_real_ops(cand_bbsch)++;
	BBSCH_bb_start_pc(bbsch)++;
	BB_MAP_Set (bbsch_map, bb, bbsch);
	BB_MAP_Set (bbsch_map, cand_bb, cand_bbsch);
      }
    }
  } else {
    // sometimes, we move the addiu past the load/store op making the
    // necessary adjustments in the offset and may have to backtrack since
    // it wasn't a profitable code movement. this phase is responsible for
    // adjusting the load/store offsets back to their original form. This
    // is faster than actually calling the dep_graph builder and walking thru
    // the succ arcs.
    if (CGTARG_Is_OP_Addr_Incr(cand_op) &&
	!TN_is_sp_reg(OP_result(cand_op,0 /*???*/))) {
#ifdef TARG_LOONGSON
	INT64 addiu_const = TN_value (OP_opnd(cand_op,2));
#else
	INT64 addiu_const = TN_value (OP_opnd(cand_op,1));
#endif
	OP *succ_op;
	for (succ_op = cand_op;
	     succ_op != NULL;
	     succ_op = OP_next(succ_op)) {
	  if (OP_memory(succ_op)) {
	    // check if the memory OP has an offset field (i.e. it is not
	    // indexed load/store prefx
	    INT offset_opndnum = Memory_OP_Offset_Opndnum (succ_op);
	    INT base_opndnum = Memory_OP_Base_Opndnum (succ_op);
#ifdef TARG_X8664
	    FmtAssert( base_opndnum >= 0, ("NYI") );
#endif
	    if (TN_has_value(OP_opnd(succ_op, offset_opndnum))) {
	      if ((Ignore_TN_Dep && 
		   (TN_register(OP_opnd(succ_op, base_opndnum)) == 
		    TN_register(OP_result(cand_op,0 /*???*/)))) ||
		  
		  (!Ignore_TN_Dep &&
		   (TN_number(OP_opnd(succ_op, base_opndnum)) ==
		    TN_number(OP_result(cand_op,0 /*???*/)))))
		
		Fixup_Ldst_Offset (succ_op, addiu_const, +1, HBS_FROM_GCM);
	    }
	  }
	}
    }
    if (OP_memory(cand_op)) {
      INT offset_opndnum = Memory_OP_Offset_Opndnum (cand_op);
      if (TN_has_value(OP_opnd(cand_op, offset_opndnum))) {
	INT base_opndnum = Memory_OP_Base_Opndnum (cand_op);
#ifdef TARG_X8664
	FmtAssert( base_opndnum >= 0, ("NYI") );
#endif
	OP *succ_op;
	for (succ_op= OP_next(cand_op); 
	     succ_op != NULL; 
	     succ_op = OP_next(succ_op))
	  {
	    if (CGTARG_Is_OP_Addr_Incr(succ_op)) {
	      if ((Ignore_TN_Dep && 
		   (TN_register(OP_opnd(cand_op, base_opndnum)) ==
		    TN_register(OP_result(succ_op,0 /*???*/)))) ||
		  
		  (!Ignore_TN_Dep &&
		   (TN_number(OP_opnd(cand_op, base_opndnum)) ==
		    TN_number(OP_result(succ_op,0 /*???*/)))))
		{
#ifdef TARG_LOONGSON
		  INT64 addiu_const = TN_value (OP_opnd(succ_op,2));
#else
		  INT64 addiu_const = TN_value (OP_opnd(succ_op,1));
#endif
		  Fixup_Ldst_Offset (cand_op, addiu_const, -1, HBS_FROM_GCM);
		  DevWarn ("Memory OP offset adjusted in GCM");
		}
	    }
	  }
      }
    }
  }
}

static void Add_Fail_TNs(OP* cur_op, 
                         TN_SET **pfailed_tn_uses, 
                         TN_SET **pfailed_tn_defs,
                         TN_SET **pfailed_gtn_uses, 
                         TN_SET **pfailed_gtn_defs,
                         REGISTER_SET *failed_reg_uses,
                         REGISTER_SET *failed_reg_defs)
{
  REGISTER_SET 	reg_defs[ISA_REGISTER_CLASS_MAX+1], 
		reg_uses[ISA_REGISTER_CLASS_MAX+1];
  GTN_SET       *gtn_defs, *gtn_uses;
  TN_SET        *tn_defs, *tn_uses;

  if (Ignore_TN_Dep) {
    REGSET_CLEAR (failed_reg_defs);
    REGSET_CLEAR (failed_reg_uses);
  } else {
    tn_defs = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);
    tn_uses = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);
    gtn_defs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
    gtn_uses = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
  }

  // take care of result defs
  INT i;
  for (i = 0; i < OP_results(cur_op); ++i) {
    TN *result_tn = OP_result(cur_op,i);
    if (Ignore_TN_Dep) {
      REGISTER result_reg = TN_register (result_tn);
      ISA_REGISTER_CLASS result_cl = TN_register_class (result_tn);
      reg_defs[result_cl] = REGISTER_SET_Union1(reg_defs[result_cl], result_reg);
    }else{
      if(TN_is_global_reg(result_tn)) 
        gtn_defs = GTN_SET_Union1D(gtn_defs, result_tn, &MEM_local_pool);
      else
        tn_defs = TN_SET_Union1D(tn_defs, result_tn, &MEM_local_pool);
    }
  }

  // check the extra result TNs
#ifdef TARG_SL
  TN_LIST * extra_results = cur_op->extra_result;
  while( extra_results ) {
    TN *result_tn = TN_LIST_first( extra_results );
    if( Ignore_TN_Dep ){
      REGISTER result_reg = TN_register (result_tn);
      ISA_REGISTER_CLASS result_cl = TN_register_class (result_tn);
      reg_defs[result_cl] = REGISTER_SET_Union1(reg_defs[result_cl],
                                                result_reg);
    }else{
      if (TN_is_global_reg(result_tn)) 
        gtn_defs = GTN_SET_Union1D(gtn_defs, result_tn, &MEM_local_pool);
      else
        tn_defs = TN_SET_Union1D(tn_defs, result_tn, &MEM_local_pool);
    }
    extra_results = TN_LIST_rest( extra_results );
  }
#endif
  // take care of opnd uses
  for (i = 0; i < OP_opnds(cur_op); ++i) {
    TN *opnd_tn = OP_opnd(cur_op,i);
    if (TN_is_constant(opnd_tn)) continue;
    if (Ignore_TN_Dep) {
      REGISTER opnd_reg = TN_register (opnd_tn);
      ISA_REGISTER_CLASS opnd_cl = TN_register_class (opnd_tn);
      reg_uses[opnd_cl] = REGISTER_SET_Union1(reg_uses[opnd_cl], opnd_reg);
    }else{
      if(TN_is_global_reg(opnd_tn))
        gtn_uses = GTN_SET_Union1D(gtn_uses, opnd_tn, &MEM_local_pool);
      else
        tn_uses = TN_SET_Union1D(tn_uses, opnd_tn, &MEM_local_pool);
    }
  } 

      // check the extra opnd TNs
#ifdef TARG_SL
  TN_LIST * extra_opnds = cur_op->extra_operand;
  while( extra_opnds ){
    TN* opnd_tn = TN_LIST_first( extra_opnds );
    if (TN_is_constant(opnd_tn)) continue;
    if (Ignore_TN_Dep) {
      REGISTER opnd_reg = TN_register (opnd_tn);
      ISA_REGISTER_CLASS opnd_cl = TN_register_class (opnd_tn);
      reg_uses[opnd_cl] = REGISTER_SET_Union1(reg_uses[opnd_cl],
                                               opnd_reg);
    }else{
      if(TN_is_global_reg(opnd_tn))
        gtn_uses = GTN_SET_Union1D(gtn_uses, opnd_tn, &MEM_local_pool);
      else
        tn_uses = TN_SET_Union1D(tn_uses, opnd_tn, &MEM_local_pool);
    }
    extra_opnds = TN_LIST_rest( extra_opnds );
  }
#endif
  if( Ignore_TN_Dep ){
          REGSET_OR(failed_reg_defs, reg_defs);
          REGSET_OR(failed_reg_uses, reg_uses);
  }else{
    *pfailed_gtn_defs = GTN_SET_Union(*pfailed_gtn_defs, gtn_defs, &MEM_local_pool);
    *pfailed_gtn_uses = GTN_SET_Union(*pfailed_gtn_uses, gtn_uses, &MEM_local_pool);
    *pfailed_tn_defs = TN_SET_Union(*pfailed_tn_defs, tn_defs, &MEM_local_pool);
    *pfailed_tn_uses = TN_SET_Union(*pfailed_tn_uses, tn_uses, &MEM_local_pool);
  }
}

// =======================================================================
// OP_To_Move
// is responsible for picking an <op> to move (dependent on <motion_type>)
// from <bb> to <tgt_bb>. The selection of <op> is dependent on various
// factors. <motion_type> controls the type of code transformations that
// can be performed. <spec_type> returns the type of speculative movement,
// if required.
// =======================================================================
static OP *
OP_To_Move (BB *bb, BB *tgt_bb, BB_SET **pred_bbs, mINT32 motion_type, mUINT8 *spec_type)
{
#if defined(TARG_SL)
  // No need for code motion, if barriered.
  if( barriered )
    return NULL;
#endif

  OP *cur_op;
  OP *best_op = NULL;

  // select a candidate instruction */
  // TODO: use a better heuristic and it should be dependent on the 
  // op's properties, the requested <motion_type> and <bb>, <tgt_bb>
  // properties

  REGISTER_SET 	reg_defs[ISA_REGISTER_CLASS_MAX+1], 
		reg_uses[ISA_REGISTER_CLASS_MAX+1],
		failed_reg_defs[ISA_REGISTER_CLASS_MAX+1], 
		failed_reg_uses[ISA_REGISTER_CLASS_MAX+1];

  GTN_SET       *gtn_defs, *gtn_uses, *failed_gtn_defs, *failed_gtn_uses;
  TN_SET        *tn_defs, *tn_uses, *failed_tn_defs, *failed_tn_uses;

  if (Ignore_TN_Dep) {
    REGSET_CLEAR (failed_reg_defs);
    REGSET_CLEAR (failed_reg_uses);
  } else {

    failed_gtn_defs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
    failed_gtn_uses = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
    failed_tn_defs = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);
    failed_tn_uses = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);

    gtn_defs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
    gtn_uses = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
    tn_defs = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);
    tn_uses = TN_SET_Create_Empty(Last_TN + 1, &MEM_local_pool);

    GTN_SET_ClearD (failed_gtn_uses);
    GTN_SET_ClearD (failed_gtn_defs);
  }

  // need to check if the motion is forward or backward and that we are not
  // moving code across procedure calls (unless full IPA summary is 
  // available) in either direction
  BOOL forw = motion_type & (GCM_EQUIV_FWD | GCM_SPEC_ABOVE | 
			     GCM_DUP_ABOVE | GCM_CIRC_ABOVE);
  OP *call_op = BB_call(bb) ? BB_xfer_op(bb) : NULL;
  for (cur_op = (forw) ? BB_first_op(bb) : BB_last_op(bb); cur_op; 
		cur_op = (forw) ? OP_next(cur_op) : OP_prev(cur_op)) {

    if( Trace_GCM ){
      fprintf( TFile, "best_op: %s, cur_op :", best_op ? "not NULL" : "NULL" );
      Print_OP_No_SrcLine(cur_op);   
    }

    // don't consider dummy or transfer ops
    if( OP_xfer(cur_op) || OP_noop(cur_op)
#if defined(TARG_SL)
	|| OP_is_loop(cur_op) 
#endif
	) {
      if( Trace_GCM ){
        fprintf(TFile, "skipped for OP_xfer or OP_noop \n");
      }
      // Ensure we only rotate once. More than once will need more work
      Add_Fail_TNs( cur_op, &failed_tn_uses, &failed_tn_defs,
                    &failed_gtn_uses, &failed_gtn_defs,
                    failed_reg_uses, failed_reg_defs );
      continue;
    }

    if (CGTARG_Is_OP_Barrier(cur_op)) {
#if defined(TARG_SL)
      if( Trace_GCM ){
        fprintf(TFile, "skipped for it is a barrier \n");
      }
      // If got a barrier, no more code motion. So return NULL now.
      barriered = TRUE;
      return NULL;
#else // TARG_IA64, TARG_X8664, TARG_NVISA
      continue;
#endif
    }

#ifdef TARG_X8664
    // Don't move x87/MMX OPs in order to perserve their ordering relative to
    // EMMS OPs.
    if (OP_x87(cur_op) || OP_mmx(cur_op)) continue;
#endif

    // All real OPs and some dummy OPs which have real operands/results.
    // typo?: if (OP_Real_Ops(cur_op) != 1 || OP_Real_Ops(cur_op) != 0) 
    if (OP_Real_Ops(cur_op) != 1 && OP_Real_Ops(cur_op) != 0) {
      if( Trace_GCM ){
        fprintf(TFile, "skipped for real_op!=1  \n");
      }
      // Ensure we only rotate once. More than once will need more work
      Add_Fail_TNs( cur_op, &failed_tn_uses, &failed_tn_defs,
                    &failed_gtn_uses, &failed_gtn_defs,
                    failed_reg_uses, failed_reg_defs );
      continue;
    }

    // has already been picked before, ignore it now
    if (OP_visited(cur_op)) {
      if( Trace_GCM ){
        fprintf(TFile, "skipped for already visited \n");
      }
      // Ensure we only rotate once. More than once will need more work
      Add_Fail_TNs( cur_op, &failed_tn_uses, &failed_tn_defs,
                    &failed_gtn_uses, &failed_gtn_defs,
                    failed_reg_uses, failed_reg_defs );
      continue;
    }

    // op has a special meaning w.r.t <bb> and therefor shouldn't be 
    // considered as candidate for code movement. these special case
    // applies to all stack adjustment ops, or OPs with glue_code
    // attribute.
    if (OP_Has_Restrictions(cur_op, bb, tgt_bb, motion_type)){ 
      if( Trace_GCM ){
        fprintf(TFile, "skipped for being restricted \n");
      }
      // Ensure we only rotate once. More than once will need more work
      Add_Fail_TNs( cur_op, &failed_tn_uses, &failed_tn_defs,
                    &failed_gtn_uses, &failed_gtn_defs,
                    failed_reg_uses, failed_reg_defs );
      continue;
    }

    if (Ignore_TN_Dep) {
      REGSET_CLEAR(reg_defs);
      REGSET_CLEAR(reg_uses);
    } else {
      GTN_SET_ClearD (gtn_defs);
      GTN_SET_ClearD (gtn_uses);
#if defined(TARG_SL)
      TN_SET_ClearD (tn_defs);
      TN_SET_ClearD (tn_uses);
#endif
    }
    
    BOOL move_better = TRUE;
    if (best_op == NULL || (move_better = 
			Is_OP_Move_Better(cur_op, best_op, motion_type)))
    {
      if (*pred_bbs == NULL) {
       *pred_bbs = BB_SET_Singleton (tgt_bb, &MEM_local_pool);

       /* If <bb> is visited while looking for its predecessors till <tgt_bb>,
        * this implies that <bb> is part of a loop that does not include
        * <tgt_bb>. For this case, don't move instructions. We don't want 
        * to be doing loop invariant code motion.
        */
       if (Motion_Is_CIRC_ABOVE(motion_type)) {
	 *pred_bbs = BB_SET_Union1 (*pred_bbs, bb, &MEM_local_pool);
       } else {
	 if (BB_Add_Ancestors(pred_bbs, bb, bb, &MEM_local_pool)) return NULL;
       }
      }

      // check the result TNs
      INT i;
      for (i = 0; i < OP_results(cur_op); ++i) {
       TN *result_tn = OP_result(cur_op,i);
       if (Ignore_TN_Dep) {
         REGISTER result_reg = TN_register (result_tn);
         ISA_REGISTER_CLASS result_cl = TN_register_class (result_tn);
         reg_defs[result_cl] = REGISTER_SET_Union1(reg_defs[result_cl],
                                                   result_reg);
       } else {
	 if (TN_is_global_reg(result_tn)) 
	   gtn_defs = GTN_SET_Union1D(gtn_defs, result_tn, &MEM_local_pool);
	 else
	   tn_defs = TN_SET_Union1D(tn_defs, result_tn, &MEM_local_pool);
       }
      } // take care of result defs
      // check the extra result TNs
#ifdef TARG_SL
      TN_LIST * extra_results = cur_op->extra_result;
      while ( extra_results ) {
        TN *result_tn = TN_LIST_first( extra_results );
        if ( Ignore_TN_Dep ){
          REGISTER result_reg = TN_register (result_tn);
          ISA_REGISTER_CLASS result_cl = TN_register_class (result_tn);
          reg_defs[result_cl] = REGISTER_SET_Union1(reg_defs[result_cl],
                                                    result_reg);
        } else {
          if (TN_is_global_reg(result_tn)) 
            gtn_defs = GTN_SET_Union1D(gtn_defs, result_tn, &MEM_local_pool);
          else
            tn_defs = TN_SET_Union1D(tn_defs, result_tn, &MEM_local_pool);
        }
        extra_results = TN_LIST_rest( extra_results );
      }
#endif
      // take care of opnd uses
      for(i = 0; i < OP_opnds(cur_op); ++i) {
        TN *opnd_tn = OP_opnd(cur_op,i);
        if (TN_is_constant(opnd_tn)) continue;
        if (Ignore_TN_Dep) {
          REGISTER opnd_reg = TN_register (opnd_tn);
          ISA_REGISTER_CLASS opnd_cl = TN_register_class (opnd_tn);
          reg_uses[opnd_cl] = REGISTER_SET_Union1(reg_uses[opnd_cl],
                                                   opnd_reg);
        } else {
          if (TN_is_global_reg(opnd_tn))
            gtn_uses = GTN_SET_Union1D(gtn_uses, opnd_tn, &MEM_local_pool);
          else
            tn_uses = TN_SET_Union1D(tn_uses, opnd_tn, &MEM_local_pool);
        }
      }
 
      // check the extra opnd TNs
#ifdef TARG_SL
      TN_LIST * extra_opnds = cur_op->extra_operand;
      while( extra_opnds ){
        TN* opnd_tn = TN_LIST_first( extra_opnds );
        if (TN_is_constant(opnd_tn)) continue;
        if (Ignore_TN_Dep) {
          REGISTER opnd_reg = TN_register (opnd_tn);
          ISA_REGISTER_CLASS opnd_cl = TN_register_class (opnd_tn);
          reg_uses[opnd_cl] = REGISTER_SET_Union1(reg_uses[opnd_cl],
                                                   opnd_reg);
        }else{
          if(TN_is_global_reg(opnd_tn))
            gtn_uses = GTN_SET_Union1D(gtn_uses, opnd_tn, &MEM_local_pool);
          else
            tn_uses = TN_SET_Union1D(tn_uses, opnd_tn, &MEM_local_pool);
        }
        extra_opnds = TN_LIST_rest( extra_opnds );
      }
#endif
      // need to check if <cur_op> has any relation with prior unsuccessful 
      // ops in <bb>.
      BOOL succ_intrsct =
        ( Ignore_TN_Dep && !REGSET_INTERSECT(reg_defs, failed_reg_defs) &&
          !REGSET_INTERSECT(reg_defs, failed_reg_uses) &&
          !REGSET_INTERSECT(reg_uses, failed_reg_defs) ||
         (!Ignore_TN_Dep &&
	  (!GTN_SET_IntersectsP(gtn_defs, failed_gtn_defs) &&
	   !GTN_SET_IntersectsP(gtn_defs, failed_gtn_uses) &&
	   !GTN_SET_IntersectsP(gtn_uses, failed_gtn_defs)) &&
	  (!TN_SET_IntersectsP(tn_defs, failed_tn_defs) &&
	   !TN_SET_IntersectsP(tn_defs, failed_tn_uses) &&
	   !TN_SET_IntersectsP(tn_uses, failed_tn_defs))) );

      void *defs[2], *uses[2];
      defs[0] = Ignore_TN_Dep ? (void *)&reg_defs : (void *)gtn_defs;
      uses[0] = Ignore_TN_Dep ? (void *)&reg_uses : (void *)gtn_uses;
      defs[1] = Ignore_TN_Dep ? NULL : (void *)tn_defs;
      uses[1] = Ignore_TN_Dep ? NULL : (void *)tn_uses;
      
      BOOL unsafe = OP_unsafe(cur_op);
      BOOL can_across_call = CG_DEP_Can_OP_Move_Across_Call( cur_op, 
                                 call_op, forw, Ignore_TN_Dep);
#if defined(TARG_SL)
      BOOL meet_heur = OP_Move_Heuristic( cur_op, motion_type, bb, tgt_bb );
#else
      BOOL meet_heur = TRUE;
#endif
      BOOL can_move = Can_OP_Move(cur_op, bb, tgt_bb, pred_bbs, defs, 
                             uses, motion_type, spec_type);
      if( !unsafe && succ_intrsct && can_across_call && meet_heur && can_move ){
	best_op = cur_op;
        if( Trace_GCM )
          fprintf( TFile, " choose cur_op to be best_op \n" );
      }else {
        if (Ignore_TN_Dep) {
          REGSET_OR(failed_reg_defs, reg_defs);
          REGSET_OR(failed_reg_uses, reg_uses);
        } else {
          failed_gtn_defs = GTN_SET_Union(failed_gtn_defs, gtn_defs,
                                          &MEM_local_pool);
          failed_gtn_uses = GTN_SET_Union(failed_gtn_uses, gtn_uses,
                                          &MEM_local_pool);
#if defined(TARG_SL)
          failed_tn_defs = TN_SET_Union(failed_tn_defs, tn_defs, &MEM_local_pool);
          failed_tn_uses = TN_SET_Union(failed_tn_uses, tn_uses, &MEM_local_pool);
#endif
        }

        if( Trace_GCM ){
          fprintf( TFile, " skipped for %s", unsafe ? "unsafe ":" "  );
          fprintf( TFile, " %s", succ_intrsct ? " ":" TN dependence "  );
          fprintf( TFile, " %s", !can_across_call ? " cannot across call ":" "  );
          fprintf( TFile, " %s", !meet_heur ? " not meet heur":" "  );
          fprintf( TFile, " %s", !can_move ? " cannot move":" "  );
          fprintf( TFile, "\n" );
        }
      }
    }

    // compseed metric: now that the <best_op> has been picked among list
    // of possible choices and no further move is better, ideal spot to 
    // quit. Note, that the <bb> is already scheduled. so, the chances that
    // the most profitable candidates are caught early enough are very
    // high.
    else {
      if(best_op && !move_better){
        if( Trace_GCM )
          fprintf( TFile, "take the best op as candidate\n" );
        break;
      }
    }
  }

  return best_op;
}

// =======================================================================
// Adjust_Qualifying_Predicate
// This routine adjust the qualifying predicate of <cand_op> (if required)
// as a result of movement of <cand_op> from <src_bb> to <tgt_bb>. 
// <motion_type> tells the type of transformation performed and <spec_type>
// determines the type of speculation (PSAFE, CSAFE,..).
// 
// =======================================================================
static void
Adjust_Qualifying_Predicate(OP *cand_op, BB *src_bb, BB *tgt_bb, 
			    mINT32 motion_type, mUINT8 spec_type)
{

  if (motion_type & (GCM_CIRC_ABOVE | GCM_SPEC_ABOVE)) {
    // do predicate promotion/assignment. 
    if (PSAFE_PTR_SPEC(spec_type)) {
      OP *tgt_br_op = BB_xfer_op(tgt_bb);
      if (tgt_br_op && OP_has_predicate(tgt_br_op)) {
	TN *tn1, *tn2;
	OP *cmp;
	CGTARG_Analyze_Compare(tgt_br_op, &tn1, &tn2, &cmp);
	BOOL fall_thru = BB_Fall_Thru_Successor(tgt_bb) == src_bb;
	
	// If <!fall_through> set the predicate of <cand_op> to the
	// controlling predicate of <tgt_br_op>.
	if (!fall_thru) {
	  Set_OP_opnd(cand_op, OP_PREDICATE_OPND, 
		      OP_opnd(tgt_br_op, OP_PREDICATE_OPND));
	} else if (cmp && OP_results(cmp) == 2) {

	  // If <fall_through>, then need to determine the exact condition,
	  // depending on whether the branch is on the true or false
	  // predicate. Check the conditions here.

	  BOOL branch_on_true = CGTARG_Branches_On_True(tgt_br_op, cmp);
	  TN *pred_tn;
	  if (branch_on_true) {
	    pred_tn = OP_result(cmp, 1);
	    Set_OP_opnd(cand_op, OP_PREDICATE_OPND, pred_tn);

	    // Set the global reg bit accordingly.
	    if (OP_bb(cmp) != OP_bb(cand_op) && !TN_is_global_reg(pred_tn)) {
	      GTN_UNIVERSE_Add_TN(pred_tn);
	      // TODO: Need to update the live-sets accordingly.
	    }
	  } else {
	    pred_tn = OP_result(cmp, 0);
	    Set_OP_opnd(cand_op, OP_PREDICATE_OPND, pred_tn);

	    // Set the global reg bit accordingly.
	    if (OP_bb(cmp) != OP_bb(cand_op) && !TN_is_global_reg(pred_tn)) {
	      GTN_UNIVERSE_Add_TN(pred_tn);
	      // TODO: Need to update the live-sets accordingly.
	    }
	  }
	}
      }
    }  // PSAFE_PTR_SPEC
  }  // (.. (GCM_CIRC_ABOVE | GCM_SPEC_ABOVE))
}

// =======================================================================
// Dependent_Between_OPs
//
// Decide if op_a and op_b dependent on each other
// =======================================================================
static BOOL Dependent_Between_OPs( OP *op_a, OP *op_b )
{
  int i;
  for( i=0; i < OP_results(op_a); i++ ){
    int j;
    for( j=0; j < OP_results(op_b); j++ ){
      if( TN_number(OP_result(op_a, i)) == TN_number(OP_result(op_b,j)) )
        return TRUE;
    }
    for( j=0; j < OP_opnds(op_b); j++ ){
      if( TN_is_constant( OP_opnd(op_b,j) ) )
        continue;
      if( TN_number(OP_result(op_a, i)) == TN_number(OP_opnd(op_b,j)) )
        return TRUE;
    }
  }   

  for( i=0; i < OP_results(op_b); i++ ){
    int j;
    for( j=0; j < OP_opnds(op_a); j++ ){
      if( TN_is_constant( OP_opnd(op_a,j) ) )
        continue;
      if( TN_number(OP_result(op_b, i)) == TN_number(OP_opnd(op_a,j)) )
        return TRUE;
    }
  } 

  return FALSE;
}

// =======================================================================
// Append_Op_To_BB
// Appends <cand_op> to <bb>. need to consider where to append depending 
// on whether it's PRE/POST GCM phase.
//
// If the cand_op needs to be insert before some limit_op in cand_bb, and 
// there is dependence between cand_op and limit_op, then it cannot be 
// insert there. Then we fail to Append_Op_To_BB
//
// return TRUE  : succeed
//        FALSE : fail
// =======================================================================
static BOOL Append_Op_To_BB(OP *cand_op, BB *cand_bb, BB *src_bb,
                     mINT32 motion_type, mUINT8 spec_type)
{
  OP *limit_op;
  BOOL succeed = TRUE;

#if defined(TARG_SL)
  // Due to CFlow problem, we may got a BB with 'loop' instruction followed
  // by 'jp'
  if( BB_loop_op(cand_bb) )
    limit_op = BB_loop_op(cand_bb);
  else
    limit_op = BB_xfer_op(cand_bb);
#else
    limit_op = BB_xfer_op(cand_bb);
#endif

#ifdef TARG_X8664
  if( limit_op != NULL && OP_cond( limit_op ) ){
    FmtAssert( !TOP_is_change_rflags(OP_code(cand_op)), 
               ("cand_op modifies rflags") );
  }
#endif
  
  {
    if( limit_op ){
      BB_Insert_Op_Before (cand_bb, limit_op, cand_op);
#if defined(TARG_SL)
      // I still insert OP before limit_op even if they have dependence. But I
      // return it FALSE. This way we can use the 'should_skip' variable, and 
      // un-do this insertion in a normalized way.
      if(Dependent_Between_OPs(limit_op, cand_op))
        succeed = FALSE;
#endif
    } else
      BB_Append_Op (cand_bb, cand_op);
  }

  // Adjust the qualifying predicate if required.
  Adjust_Qualifying_Predicate(cand_op, src_bb, cand_bb, motion_type,spec_type);
 
  return succeed; 
}

// =======================================================================
// Adjust_BBSCH
// Accounts for final adjustments to local/global regcosts sets as a 
// result of moving <cand_op> fro <bb> to <cand_bb>. The corresponding
// BBSCH structures are updated as a result of it.
// =======================================================================
static void
Adjust_BBSCH (OP *cand_op, BB *cand_bb, BB *bb,
	      BBSCH *new_cand_bbsch, BBSCH *new_bbsch)
{
  // Account the cost of converting a TN to a GTN or vice-versa.
  if (OP_has_result(cand_op)) {
    TN *result_tn = OP_result(cand_op, 0);
    if (!TN_is_global_reg(result_tn)) {
      BBSCH_global_regcost(new_cand_bbsch)++;
      BBSCH_global_regcost(new_bbsch)++;
#ifndef TARG_MIPS
      BBSCH_local_regcost(new_bbsch)--; 
#endif
    } else {
      BBSCH_global_regcost(new_cand_bbsch)++;
      BBSCH_global_regcost(new_bbsch)--;
    }
  }

  // Now account for operands estimates as well.
  for (INT i = 0; i < OP_opnds(cand_op); ++i) {
    TN *opnd_tn = OP_opnd(cand_op, i);
    if (TN_is_constant(opnd_tn)) continue;
    
    if (TN_is_global_reg(opnd_tn)) {
      if (!GTN_SET_MemberP(BB_live_out(cand_bb), opnd_tn)) {
	BBSCH_global_regcost(new_cand_bbsch)--;
#ifndef TARG_MIPS
	BBSCH_local_regcost(new_cand_bbsch)++;
#endif
      }
      if (!GTN_SET_MemberP(BB_live_out(bb), opnd_tn))
	BBSCH_global_regcost(new_bbsch)--;
    }
  }
}

// =======================================================================
// Is_Schedule_Worse
// Determines if the weighted schedules as a result of code movement is
// worse or not. All the OOO adjustments should be made here.
// =======================================================================
static BOOL
Is_Schedule_Worse(BB *bb, BB *cand_bb, BBSCH *new_bbsch, 
		  BBSCH *new_cand_bbsch, BBSCH *old_bbsch,
		  BBSCH *old_cand_bbsch)
{

  UINT32 new_from_time, new_to_time, old_from_time, old_to_time;
  mINT8  old_from_regcost[ISA_REGISTER_CLASS_MAX+1], 
         old_to_regcost[ISA_REGISTER_CLASS_MAX+1],
         new_from_regcost[ISA_REGISTER_CLASS_MAX+1], 
         new_to_regcost[ISA_REGISTER_CLASS_MAX+1];

  old_from_time = BBSCH_schedule_length(old_bbsch);
  old_to_time = BBSCH_schedule_length(old_cand_bbsch);
  new_from_time = BBSCH_schedule_length(new_bbsch);
  new_to_time = BBSCH_schedule_length(new_cand_bbsch);

  if (Cur_Gcm_Type & GCM_MINIMIZE_REGS) {
    mINT8 *old_from_local_regcost, *old_to_local_regcost,
          *new_from_local_regcost, *new_to_local_regcost;

    old_from_local_regcost = BBSCH_local_regcost(old_bbsch);
    old_to_local_regcost = BBSCH_local_regcost(old_cand_bbsch);
    new_from_local_regcost = BBSCH_local_regcost(new_bbsch);
    new_to_local_regcost = BBSCH_local_regcost(new_cand_bbsch);


    if (Trace_GCM && Trace_GCM_Reg_Usage && GCM_Internal_Flag) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "\n FROM BB:%d => TO BB:%d\n", BB_id(bb), BB_id(cand_bb));
      fprintf (TFile, "\n LOCAL REGISTER COST");
    }

    // TODO: need to update similar ISA_REGISTER_CLASS values for global regs
    // as well.
    /* HD - The original version takes global-cost int the each regcost[i]. This 
     *      is unfair. If a motion make a local TN to global TN, we need to consider
     *      those global TN/Reg separately. -- the same as the TODO above
     */
    INT i;
    FOR_ALL_ISA_REGISTER_CLASS(i) {
#if defined(TARG_SL)
      old_from_regcost[i] = old_from_local_regcost ? old_from_local_regcost[i] : 0;
      old_to_regcost[i] = old_to_local_regcost ? old_to_local_regcost[i] : 0;
      new_from_regcost[i] = new_from_local_regcost ? new_from_local_regcost[i] : 0;
      new_to_regcost[i] = new_to_local_regcost ? new_to_local_regcost[i] : 0;
#else
      old_from_regcost[i] =  BBSCH_global_regcost(old_bbsch) + 
	                     ((old_from_local_regcost) ? old_from_local_regcost[i] : 0);

      old_to_regcost[i] =    BBSCH_global_regcost(old_cand_bbsch) + 
	                     ((old_to_local_regcost) ? old_to_local_regcost[i] : 0);

      new_from_regcost[i] =  BBSCH_global_regcost(new_bbsch) + 
	                     ((new_from_local_regcost) ? new_from_local_regcost[i] : 0);
      
      new_to_regcost[i] =    BBSCH_global_regcost(new_cand_bbsch) + 
	                     ((new_to_local_regcost) ? new_to_local_regcost[i] : 0);
#endif // TARG_SL
      if (Trace_GCM && Trace_GCM_Reg_Usage && GCM_Internal_Flag) {
        #pragma mips_frequency_hint NEVER
        fprintf (TFile, "\nold_from_local_regcost[%d]=%d, old_to_local_regcost[%d]=%d\n",
		 i, (old_from_local_regcost) ? old_from_local_regcost[i] : 0, 
		 i, (old_to_local_regcost) ? old_to_local_regcost[i] : 0);   	
        

        fprintf (TFile, "new_from_local_regcost[%d]=%d, new_to_local_regcost[%d]=%d\n",
		 i, (new_from_local_regcost) ? new_from_local_regcost[i] : 0, 
		 i, (new_to_local_regcost) ? new_to_local_regcost[i] : 0);   	
       
      }
    }
    if (Trace_GCM && Trace_GCM_Reg_Usage && GCM_Internal_Flag) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "\n GLOBAL REGISTER COST");
      fprintf (TFile, "\nold_from_global_regcost=%d, old_to_global_regcost=%d\n",
	       BBSCH_global_regcost(old_bbsch), BBSCH_global_regcost(old_cand_bbsch)); 

      fprintf (TFile, "new_from_global_regcost=%d, new_to_global_regcost=%d\n",
	       BBSCH_global_regcost(new_bbsch), BBSCH_global_regcost(new_cand_bbsch)); 
     }
  }
  
  BOOL equiv_blocks = 	
    BS_MemberP (BB_pdom_set(bb), BB_id(cand_bb)) &&
    BS_MemberP (BB_dom_set(cand_bb), BB_id(bb)) ||
    BS_MemberP (BB_pdom_set(cand_bb), BB_id(bb)) &&
    BS_MemberP (BB_dom_set(bb), BB_id(cand_bb));

  UINT32 branch_penalty;
  float freq_ratio = BB_freq(cand_bb)/BB_freq(bb);

  // OOO adjustments (necessary?)
  if (CG_DEP_Adjust_OOO_Latency && !equiv_blocks && (freq_ratio < 0.4))
    branch_penalty = (UINT32) (freq_ratio * (1 - freq_ratio) * mispredict);
  else
    branch_penalty = 0;

  // make the schedule 10 times worse if GCM_Test is enabled just to
  // invoke more transformations (and testing coverage).
  // Include the branch misprediction penalty when considering OOO effects.
  // This will bias the fall-thru path slightly since the cost of misprediction
  // is high too.

  INT times = (GCM_Test) ? 10 : 1;
  BOOL worsen_schedule =
    (((BB_freq(bb) * new_from_time) + 
      (BB_freq(cand_bb) * new_to_time)) >
     (times * ((BB_freq(bb) * old_from_time) + 
	       (BB_freq(cand_bb) * old_to_time) + 
	       branch_penalty)) ||
     
     // to check that we only do speculation if it's free
     (!equiv_blocks && (new_to_time > (times * (old_to_time + 
						branch_penalty)))));

  // TODO: need to consider register class and consider costs separately
  // for each class.

  BOOL improve_reg_pressure = TRUE;
  if (Cur_Gcm_Type & GCM_MINIMIZE_REGS) {
    for (INT i = ISA_REGISTER_CLASS_MIN; i <= ISA_REGISTER_CLASS_MAX &&
				     improve_reg_pressure; i++) {
      UINT8 delta_from = new_from_regcost[i] - old_from_regcost[i];
      UINT8 delta_to = new_to_regcost[i] - old_to_regcost[i];

#ifdef KEY
      // Implementing the TODO: need to consider register class and 
      // consider costs separately for each class
      improve_reg_pressure =  improve_reg_pressure &&
	(old_from_regcost[i] <= REGISTER_CLASS_info[i].register_count &&
	 old_to_regcost[i] <= REGISTER_CLASS_info[i].register_count &&
	 ((old_from_regcost[i] + delta_from) <= REGISTER_CLASS_info[i].register_count) &&
	 ((old_to_regcost[i] + delta_to) <= REGISTER_CLASS_info[i].register_count));
#else
      improve_reg_pressure =  improve_reg_pressure &&
	(old_from_regcost[i] <= REGISTER_MAX &&
	 old_to_regcost[i] <= REGISTER_MAX &&
	 ((old_from_regcost[i] + delta_from) <= REGISTER_MAX) &&
	 ((old_to_regcost[i] + delta_to) <= REGISTER_MAX));
#endif
    }
  }

  return worsen_schedule || !improve_reg_pressure;
}
	    
// =======================================================================
// Schedule_BB_For_GCM
// calls the hyperblock scheduler (HBS) for single-BBs to derive some of 
// the blocks properties in <bbsch>.
// =======================================================================
static BBSCH *
Schedule_BB_For_GCM (BB *bb, HBS_TYPE hb_type, HB_Schedule **Sched)
{
  BBSCH *bbsch = (BBSCH *)BB_MAP_Get (bbsch_map, bb);

  if (bbsch == NULL) {
    bbsch = TYPE_MEM_POOL_ALLOC (BBSCH, &gcm_loop_pool);
    bzero (bbsch, sizeof (BBSCH));
    Set_BB_SCHEDULE(bbsch); // need to schedule this block
  }

  if (BB_SCHEDULE(bbsch) ) {
    if (!GCM_Use_Sched_Est) {
      if (! *Sched) {
	*Sched = CXX_NEW(HB_Schedule(), &MEM_local_pool);
      }

      (*Sched)->Init(bb, hb_type, INT32_MAX, bbsch, NULL);
      (*Sched)->Schedule_BB(bb, bbsch);
    } else {
      Cur_Gcm_Type |= GCM_USE_SCHED_EST;
      BBSCH_schedule_length(bbsch) = CG_SCHED_EST_BB_Cycles(bb,
							    SCHED_EST_FOR_GCM);
    }
    BB_MAP_Set (bbsch_map, bb, bbsch);
  }
  return bbsch;
}

// =======================================================================
// Visit_BB_Preds
//
// Recursively visit the prececessors of <bb> which are in the set
// <loop_bbs> and mark that they are included in cycle <icycle>. 
// Stop when we reach <head>.
// 
// =======================================================================
static void Visit_BB_Preds(
  BB *bb,
  BB *head,
  BB_SET *loop_bbs,
  INT icycle,
  INT ncycles)
{

  // Create a cycle set for this BB if we haven't already.
  BS *cycle_set = BB_cycle_set(bb);
  if (cycle_set == NULL) {
    cycle_set = BS_Create_Empty(ncycles, &MEM_local_pool);
    Set_BB_cycle_set(bb, cycle_set);
  }

  BS_Union1D(cycle_set, icycle, NULL);

  if (bb == head) return;

  BBLIST *edge;
  FOR_ALL_BB_PREDS(bb, edge) {
    BB *pred = BBLIST_item(edge);
    if (BB_SET_MemberP(loop_bbs, pred)) {
      cycle_set = BB_cycle_set(pred);
      if (!cycle_set || !BS_MemberP(cycle_set, icycle)) {
	Visit_BB_Preds(pred, head, loop_bbs, icycle, ncycles);
      }
    }
  }
}

#if defined(TARG_SL)
/* 
 * Sort BBs in loop (also including the outmost psudo loop, which is the 
 * whole PU), in the order of bottom up. That is in the REVERSE TOPLOGICAL
 * order.
 *
 */
static VECTOR 
Sort_BBs_In_Rev_Top_Order( BB_SET *processed_bbs, LOOP_DESCR *loop )
{
  BB_SET *loop_bbs = LOOP_DESCR_bbset(loop);
  BBLIST* predlist = NULL;
  BBLIST* succlist = NULL;
  BB* pred = NULL;
  BB* succ = NULL;
  
  /* go through the bbvector, in reverse toplogical order. 
   * WE ASSUME THERE IS NO ANY INNER CYCLES, FOR ALL THE INNER CYCLES ARE ALREDY
   * DONE AND TREATED AS BLACK BOX
   */
  VECTOR sorted_bbs;  
  VECTOR working_set;

  sorted_bbs = VECTOR_Init (PU_BB_Count+2, &MEM_local_pool);
  working_set = VECTOR_Init (PU_BB_Count+2, &MEM_local_pool);

  /* Put all the loop bbs, except inner loop bbs,  into working set */
  BB* temp = NULL;
  FOR_ALL_BB_SET_members( loop_bbs, temp ){
    if( !BB_SET_MemberP(processed_bbs, temp) ) 
      VECTOR_Add_Element( working_set, (void*)temp );
  }

  /* We need a pseudo edge to link two seperated segments across inner loop,
   * which is like a hole. [ pls refer to the following 3rd comments ]
   *
   * First: find the immediate preds of inner loop, Second: find the immediate 
   * succs of the inner loop. Third: create edge(s). 
   * Be careful, I need create edges from every preds to every succs. 
   */ 
  VECTOR preds_of_hole = VECTOR_Init (PU_BB_Count+2, &MEM_local_pool);
  VECTOR succs_of_hole = VECTOR_Init (PU_BB_Count+2, &MEM_local_pool);
  for( int i=0; i < VECTOR_count(working_set); i++ ){
    bool is_pred = false;
    bool is_succ = false;
    temp = (BB*)VECTOR_element(working_set,i); 
    /* check if it's immd pred of inner loop */
    FOR_ALL_BB_SUCCS( temp, succlist ){
      succ = BBLIST_item( succlist );
      if( BB_SET_MemberP(processed_bbs, succ) ){
        VECTOR_Add_Element( preds_of_hole, (void*)temp );
        is_pred = true;
        break;
      }
    }  
    /* check if it's immd succ of inner loop */
    FOR_ALL_BB_PREDS( temp, predlist ){
      pred = BBLIST_item( predlist );
      if( BB_SET_MemberP(processed_bbs, pred) ){
        VECTOR_Add_Element( succs_of_hole, (void*)temp );
        is_succ = true;
        break;
      }
    }  
  }
 
  bool changed = false;
  bool first_round = true;
  while( VECTOR_count(working_set) ) {
    /* I need to specially treat a situation ,where nodes form irreducible
     * loop, then no one can be picked as the next rev ordering . 
     * To break this dead circle, I need to pick one randomly, and i decided
     * to choose the one with the biggest BB_id.
     */
    if( !changed && !first_round ){
      /* find the biggest one */
      int biggest_id = BB_id( (BB*)VECTOR_element(working_set, 0) );
      BB* biggest_bb = (BB*)VECTOR_element(working_set, 0);
      for( int j=0; j<VECTOR_count(working_set); j++ ){
        if( BB_id( (BB*)VECTOR_element(working_set,j) ) > biggest_id ) {
          biggest_id = BB_id( (BB*)VECTOR_element(working_set,j) );
          biggest_bb = (BB*)VECTOR_element(working_set,j);
        }
      } 
      /* remove it from working set, and add it to sorted set */
      VECTOR_Add_Element( sorted_bbs, (void*)biggest_bb );
      VECTOR_Delete_Element( working_set, (void*)biggest_bb );
      FOR_ALL_BB_PREDS( biggest_bb, predlist ){
        pred = BBLIST_item( predlist );
        if( BB_SET_MemberP(loop_bbs, pred) && 
            !VECTOR_Member_Element(working_set, (void*)pred) &&
            !VECTOR_Member_Element(sorted_bbs, (void*)pred) ){
          VECTOR_Add_Element( working_set, (void*)pred );
          changed = true;
        }
      }
      /* then go on */
      continue;
    }

    first_round = false;
    changed = false;

    /* If all succs of BB are already sorted, then it can also be sorted 
     * There are some situations we don't care
     * 1. some BBs may have succ not in the loop, for example, the top BB
     *    may be shared by two or more loops
     * 2. if the succ is loop-head, that means this is a back edge, we don't
     *    care back edge
     * 3. since outer loop contains inner loop's BBs, so we don't care about
     *    the inner BBs, they have no effect on this algorithm. But if the
     *    if we take out all the inner loops, then maybe we will get a
     *    hole, and the two segments seperated by the hole have no idea of
     *    which one should be chosen at first, since there may be several 
     *    candidates that meet the requirements. What shall we do?
     *    [Answer] Add pseudo edges from the front segment to behind one. See
     *             above source code for implementation.
     */
    for( int i=0; i < VECTOR_count(working_set); i++ ){

      BB* cand = (BB*)VECTOR_element(working_set,i); 
      bool ready = TRUE;
      BBLIST* succlist;
      BB* succ;

      FOR_ALL_BB_SUCCS( cand, succlist ){
        succ = BBLIST_item( succlist );
        if( BB_SET_MemberP(loop_bbs, succ) &&
            succ != LOOP_DESCR_loophead(loop) &&
            ! BB_SET_MemberP(processed_bbs, succ) ){ 
          if( ! VECTOR_Member_Element(sorted_bbs, (void*)succ) ){
            ready = FALSE;
            break;
          }
        }
      }

      /* If it's a pred of hole, and not all the succs of hole have been 
       * chose, then this pred should be waiting 
       */
      if( VECTOR_Member_Element(preds_of_hole, cand) ){
        /* If a BB is both pred and suss of a hole, that BB should be the only
         * bb in outer loop, so we can choose it. We only consider proper pred.
         */
        if( !VECTOR_Member_Element(succs_of_hole, cand) ) {
          for( int j=0; j < VECTOR_count(succs_of_hole); j++ ){
            void *succ = VECTOR_element(succs_of_hole,j);
            if( !VECTOR_Member_Element(sorted_bbs, succ) ) {
              ready = FALSE;
              break;
            } 
          }
        }
      }

      /* if ready, add it to sorted, remove it from working set,  and add its 
       * preds to working_set 
       */
      if( ready ){
        VECTOR_Add_Element( sorted_bbs, (void*)cand );
        VECTOR_Delete_Element( working_set, (void*)cand );
        changed = true;
        FOR_ALL_BB_PREDS( cand, predlist ){
          pred = BBLIST_item( predlist );
          if( BB_SET_MemberP(loop_bbs, pred) && 
              /* be careful, some preds may be in inner loop */
              !BB_SET_MemberP(processed_bbs, pred) && 
              !VECTOR_Member_Element(working_set, (void*)pred) &&
              !VECTOR_Member_Element(sorted_bbs, (void*)pred) ){
            VECTOR_Add_Element( working_set, (void*)pred );
          }
        }
      }

    } // end of for
  } // end of while
      
  return sorted_bbs;
}

static BOOL All_OPs_Visited( BB* bb )
{
  BOOL visited = TRUE;
  OP*  cur_op;
  FOR_ALL_BB_OPs( bb, cur_op ){
    if( !OP_visited(cur_op) ){
      visited = FALSE;
      break;
    }
  }
  return visited;
}

static void Break_Dependency( OP_LIST * moved_loads, BB* cand_bb )
{
  while( moved_loads /*&& OP_LIST_first(moved_loads)*/ ){
    OP *op = OP_LIST_first( moved_loads );
  
    /* So far, I only treat the one-result load instructions */
    Is_True(OP_results(op) == 1, ("strange load Op with more than one results"));
  
    TN *result_tn = OP_result(op, 0);
    TN* new_tn = Dup_TN_Even_If_Dedicated( result_tn );
    Set_OP_result( op, 0, new_tn );
  
    /* add a move instruction just behind the load */
    OP* copy_op = Mk_OP( TOP_addiu, result_tn, new_tn, Gen_Literal_TN(0,4) );
    Set_OP_copy( copy_op );
    BB_Insert_Op_After( cand_bb, op, copy_op ); 
   
    /* assign the tag */
    if( OP_has_tag(op) ){
      LABEL_IDX tag_idx = 0;
      tag_idx = Get_OP_Tag( op );
      Is_True( tag_idx > 0, ("incorrect tag index") );
      Reset_OP_has_tag( op );
      Set_OP_Tag( copy_op, tag_idx );
    }

    moved_loads = OP_LIST_Delete( op, moved_loads );
  } 

  /* No need to delete the move_loads, since mem pool delete it automatically*/
}

static void Reduce_Loop_Count( LOOP_DESCR *loop, BB* );

#endif // TARG_SL

// =======================================================================
// GCM_For_Loop
// performs code motion within the loop. all inner loops are considered as
// black boxes. The basic blocks within this <loop> are considered in a
// priority order and the code motion transformations done on them, as 
// required.
// =======================================================================
static INT
GCM_For_Loop (LOOP_DESCR *loop, BB_SET *processed_bbs, HBS_TYPE hb_type)
{

  BB *bb;
  RID *rid;
  VECTOR bbvector, cand_bbvector;
  INT num_moves = 0;
  PRIORITY_TYPE priority_type;

  /* this is for binary search */
  INT op_id = 0;

  L_Save();
 
  bb_cycle_set_map = BB_MAP_Create ();

  BB_SET *loop_bbs = LOOP_DESCR_bbset(loop);
  BB *loop_head = LOOP_DESCR_loophead(loop);

  // For real loops...
  if (loop_head) {

    // Find the number of cycles shared by this loop head.
    BBLIST *edge;
    INT ncycles = 0;
    FOR_ALL_BB_PREDS(loop_head, edge) {
      BB *pred = BBLIST_item(edge);
      if (BB_SET_MemberP(loop_bbs, pred)) ++ncycles;
    }

    // If the loop contains more than one cycle, then create a map
    // that identifies which cycles a BB is part of.
    if (ncycles > 1) {
      INT icycle = 0;
      FOR_ALL_BB_PREDS(loop_head, edge) {
	BB *pred = BBLIST_item(edge);
	if (BB_SET_MemberP(loop_bbs, pred)) {
	  Visit_BB_Preds(pred, loop_head, loop_bbs, icycle, ncycles);
	  ++icycle;
	}
      }
    }
  }

  if (Trace_GCM) {
    fprintf( TFile, "GCM_For_Loop: list of bbs:" );
    BB_SET_Print( loop_bbs, TFile ); 
    fprintf( TFile, "\n" );
    /* It's better to dump the pred/succ of all BBs */
    BB* temp = NULL;
    BBLIST * list;
    FOR_ALL_BB_SET_members( loop_bbs, temp ){
      fprintf( TFile, "PREDs of BB[%i] are:", BB_id(temp) );
      FOR_ALL_BB_PREDS(temp, list) {
	BB *pred = BBLIST_item(list);
        fprintf( TFile, " %i, ",BB_id(pred) ); 
      }
      fprintf( TFile, "\nSUCCs of BB[%i] are:", BB_id(temp) );
      FOR_ALL_BB_SUCCS(temp, list) {
	BB *succ = BBLIST_item(list);
        fprintf( TFile, " %i, ",BB_id(succ) ); 
      }
      fprintf( TFile, "\n" );
    }
  }
       
  HBS_TYPE from_hbs_type, to_hbs_type;

  from_hbs_type = to_hbs_type = hb_type;
  bbvector = VECTOR_Init (PU_BB_Count+2, &MEM_local_pool);

  priority_type = SORT_BY_BB_FREQ;

  if (hb_type & (HBS_BEFORE_GRA | HBS_BEFORE_LRA))
    priority_type |= SORT_BY_REG_USAGE;
  else
    priority_type |= SORT_BY_BB_PARALLELISM;

#if defined(TARG_SL)
  bbvector = Sort_BBs_In_Rev_Top_Order( processed_bbs, loop );

  INT count = VECTOR_count(bbvector);

  if (Trace_GCM) {
    fprintf (TFile, "GCM_For_Loop: After Reverse TOP Ordering, list of bbs:\n");
    for (INT i = 0; i < count; i++) {
      BB *bb = (BB *)VECTOR_element(bbvector, i);
      fprintf (TFile, "\tBB:%d\tfreq:%g\n", BB_id(bb), BB_freq(bb));
    }
  }
#else
  /* order the bbs in the loop in decreasing order of frequencies */
  FOR_ALL_BB_SET_members (loop_bbs, bb) {
    /* if bb has already been processed, ignore it. */
    if (BB_SET_MemberP (processed_bbs, bb)) continue;

    /* don't process already scheduled (or SWP) blocks */
    if ( (rid = BB_rid(bb)) && (RID_level(rid) >= RL_CGSCHED)) continue;
    if (BB_scheduled(bb) && !BB_scheduled_hbs(bb)) continue;

    // TODO: need to change sort_by_bb_frequency to a more general purpose 
    // priority function which can fit itself depending on which phase is 
    // calling GCM

    VECTOR_Add_Element (bbvector, (void *)bb);
  }

  VECTOR_Sort (bbvector, sort_by_bb_frequency);

  INT count = VECTOR_count(bbvector);
  /* skip single basic block loops */
  if (count <= 1) return 0;

  if (Trace_GCM) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "GCM_For_Loop: Ordered list of bbs:\n");
    for (INT i = 0; i < count; i++) {
      BB *bb = (BB *)VECTOR_element(bbvector, i);
      fprintf (TFile, "\tBB:%d\tfreq:%g\n", BB_id(bb), BB_freq(bb));
    }
  }
#endif  // TARG_SL

  // compspeed metric: not worthwile looking at more than 300 profitable
  // blocks in any region and 25 blocks in any non-loop region
  // TODO: need to tune this further.
  INT bb_limit = (LOOP_DESCR_nestlevel(loop) == 0) ? 25 : 100;

  /* traverse all the bbs in bbvector */

  BBSCH *old_bbsch = NULL, *old_cand_bbsch = NULL;
  HB_Schedule *Sched = NULL;
  for (INT i = 0; i < count; i++) {

    if (bb_limit-- <= 0) break;

    BB *bb = (BB *)VECTOR_element(bbvector, i);
    /* if bb has already been processed, ignore it. */
    if (BB_SET_MemberP (processed_bbs, bb)) continue;

    /* don't process already scheduled (or SWP) blocks */
    if ( (rid = BB_rid(bb)) && (RID_level(rid) >= RL_CGSCHED)) continue;
    if (BB_scheduled(bb) && !BB_scheduled_hbs(bb)) continue;

    if (Check_If_Ignore_BB(bb, loop)) continue;

    from_hbs_type |= (Ignore_TN_Dep) ? HBS_FROM_POST_GCM_SCHED :
                                        HBS_FROM_PRE_GCM_SCHED;
    to_hbs_type |= (Ignore_TN_Dep) ? HBS_FROM_POST_GCM_SCHED :
                                      HBS_FROM_PRE_GCM_SCHED;
    from_hbs_type |= HBS_FROM_GCM_FROM_BB;

    BBSCH *bbsch = Schedule_BB_For_GCM (bb, from_hbs_type, &Sched);
    if (old_bbsch == NULL) {
      old_bbsch = TYPE_MEM_POOL_ALLOC(BBSCH, &MEM_local_pool);
      bzero (old_bbsch, sizeof (BBSCH));
    }
    bcopy(bbsch, old_bbsch, sizeof (BBSCH));
    Reset_BB_SCHEDULE(bbsch);

    if( Trace_GCM ){
      fprintf(TFile, " - scheduling BB: %d\n", BB_id(bb) );
    }

    if( Trace_GCM_Dump_IR ){
      fprintf(TFile,  "%s   %s  %s", DBar, "before gcm",  DBar);
      Print_BB_No_Srclines(bb);
    }

    // Determine the <motion_type> for this <bb> (in decreasing order of
    // priority).
    mINT32 motion_type;
    while ((motion_type = Determine_Motion_Type(loop, bb, bbsch)) != GCM_NONE){
#if defined(TARG_SL)
      if( Trace_GCM ){
        fprintf( TFile, "motion types are: " );
        if( Motion_Is_CIRC_ABOVE(motion_type) ) 
          fprintf( TFile, " GCM_CIRC_ABOVE" );    
        if( Motion_Is_EQUIV_FWD(motion_type) )
          fprintf( TFile, " GCM_EQUIV_FWD" );
        if( Motion_Is_EQUIV_BKWD(motion_type) )
          fprintf( TFile, " GCM_EQUIV_BKWD" );
        if( Motion_Is_SPEC_ABOVE(motion_type) )
          fprintf( TFile, " GCM_SPEC_ABOVE" );
        if( Motion_Is_SPEC_BELOW(motion_type) )
          fprintf( TFile, " GCM_SPEC_BELOW" );
        if( Motion_Is_DUP_ABOVE(motion_type) ) 
          fprintf( TFile, " GCM_DUP_ABOVE" );
        if( Motion_Is_DUP_BELOW(motion_type) ) 
          fprintf( TFile, " GCM_DUP_BELOW" );
        fprintf( TFile, "\n" );
      }

      load_add_pairs.clear();
      last_defs_of_branch.clear();

      if( Motion_Is_CIRC_ABOVE(motion_type) ){
        circ_above_moves = 0;
        promoted_tns = TN_SET_Create_Empty( Last_TN + 1, &gcm_loop_pool );
        Preprocess_Loop_Head(bb);
        Find_Last_Defs_Of_Branch(bb);
        // init the left_ops recording the left unmoved OPs after circ_move.
        // This is should be done after Preprocess_Loop_Head()
        // NOTE: we can only reduce loop count with straight line loop body
        //       since we cannot copy left OPs to epilog if they contain branch.
        //       So we need to check this when doing Reduce_Loop_Count
        left_ops.clear();
        OP *op;
        FOR_ALL_BB_OPs( bb, op ){
          left_ops.push_back(op);
        }
      }

      /* Need to copy the flags of bbsch to old_bbsch, since Determin_Motion...
       * changes the flags of bbsch
       */
      Set_BB_flags(old_bbsch, BBSCH_flags(bbsch));
      
      cand_bbvector = VECTOR_Init (PU_BB_Count+2, &MEM_local_pool);

      // determine the priority list of candidate blocks (in decreasing 
      // order) which fits the motion type and the source blocks properties

      Determine_Candidate_Blocks(bb, loop, motion_type, &bbvector,
				 &cand_bbvector);
      if( Motion_Is_CIRC_ABOVE(motion_type) ){
        // either no cand or one cand 
        Is_True( VECTOR_count(cand_bbvector) <= 1, 
                 ("circ_above has >1 cand bb") );
      }

      if( Trace_GCM ){
        INT i, count = VECTOR_count(cand_bbvector);
        #ifdef KEY
        FmtAssert(count <= VECTOR_size(cand_bbvector), ("VECTOR overflow"));
        #else
        FmtAssert(count < VECTOR_size(cand_bbvector), ("VECTOR overflow"));
        #endif // KEY
        fprintf( TFile, "current cand BBs: " );
        for (i = 0; i < count; i++) {
          fprintf( TFile, "%d ", BB_id((BB*)VECTOR_element(cand_bbvector,i)) );
        }
        fprintf( TFile, "\n" );
      }
#else
      cand_bbvector = VECTOR_Init (PU_BB_Count+2, &MEM_local_pool);

      // determine the priority list of candidate blocks (in decreasing 
      // order) which fits the motion type and the source blocks properties

      Determine_Candidate_Blocks(bb, loop, motion_type, &bbvector,
				 &cand_bbvector);
#endif // TARG_SL
     
      // Try moving instructions from only a limited number of basic blocks.
      // TODO: Set this limit based on the nesting level and opt-level

      INT cand_bb_limit = GCM_Test ? 30 : 10;
      INT cand_bbcount = VECTOR_count(cand_bbvector);

      // walk thru the <cand_bbvector> list in descending priority order and
      // backwards since it increases the scope of doing code motion across
      // large distances.
      for (INT j = cand_bbcount - 1; j >= 0; j--) {
        OP *cand_op;
        BB_SET *pred_bbs = NULL;
	BB *cand_bb = (BB *)VECTOR_element(cand_bbvector, j);

#if defined(TARG_SL)
        /* This is used to remember all the movde loads, in circular motion */
        moved_loads = NULL;

        // Before motion of each motion type, set barriered to FALSE. 
        // Why set it here? Since before barrier OP, there is still cand_op
        // for different cand_bb. 
        barriered = FALSE;

        BB_SET *bookkeepings = NULL;

        if( BB_in_preds(bb, cand_bb) &&
            BB_preds_len(bb) > 1 &&
            BB_succs_len(cand_bb) > 1 ){
          /* Only need to do code motion on critical edge of circular motion ,
           * and the loop should be the simplest one, like the following 
           * conditionals say. Any other situations will drive me crazy.
           */
          if( !CG_GCM_enable_critical_edge_motion || 
              !Motion_Is_CIRC_ABOVE(motion_type) ||
              BB_preds_len(bb) != 2 ||
              BB_succs_len(cand_bb) != 2 ){
            continue; 
          }
        }
#endif // TARG_SL
        
        if (Large_BB(cand_bb, loop))
           continue;

	if (CG_Skip_GCM) {
	  if (BB_id(bb) == GCM_From_BB && (GCM_To_BB < 0))
	    continue;
	  if (BB_id(cand_bb) == GCM_To_BB && (GCM_From_BB < 0))
	    continue;
	  if (BB_id(bb) == GCM_From_BB && BB_id(cand_bb) == GCM_To_BB)
	    continue;
	}
	  
        if (cand_bb_limit-- <= 0) break;

#ifdef KEY
	// Consider at most GCM_BB_Limit number of candidate bb's.
	if (GCM_BB_Limit != -1 &&
	    cumulative_cand_bb++ >= GCM_BB_Limit)
	  break;
#endif

  	/* don't make the target basic block too large. */
  	if (BB_length(cand_bb) >= (Split_BB_Length - 50)) 
          continue;

	// The target and candidate BBs must be in the same cycles.
	// Skip this candidate if that's not true.
	BS *bb_cycle_set = BB_cycle_set(bb);
	BS *cand_bb_cycle_set = BB_cycle_set(cand_bb);
	if (bb_cycle_set && cand_bb_cycle_set) {
	  if (!BS_EqualP(bb_cycle_set, cand_bb_cycle_set)) 
            continue;
	} else if (bb_cycle_set || cand_bb_cycle_set) {
	    continue;
	}

    	// TODO: we right now consider the motion from <bb> to <cand_bb>
    	// it could be the other way around as well.

	to_hbs_type |= HBS_FROM_GCM_TO_BB;
        BBSCH *cand_bbsch = Schedule_BB_For_GCM (cand_bb, to_hbs_type, &Sched);
        if (old_cand_bbsch == NULL) {
	  old_cand_bbsch = TYPE_MEM_POOL_ALLOC(BBSCH, &MEM_local_pool);
	  bzero (old_cand_bbsch, sizeof (BBSCH));
	}
        bcopy(cand_bbsch, old_cand_bbsch, sizeof (BBSCH));
	Reset_BB_SCHEDULE(cand_bbsch);

	L_Save();
	mUINT8 spec_type;

        // TODO: need to make this a OP_LIST of cand_op's (i.e. aggregrate code
        // movement)
        while ((cand_op = 
                OP_To_Move (bb, cand_bb, &pred_bbs, motion_type, &spec_type)) != NULL) {
          if( Trace_GCM ){
            fprintf( TFile, "op_id:%d, The selected OP:\n", op_id);
            Print_OP_No_SrcLine(cand_op);
          }
#if defined(TARG_SL) 
          /* Need an anchor so that if cand_op needs to be rejected later, it
           * can be put back in its original position.
           */
          OP *position = OP_prev( cand_op );
#endif
	  BB_Remove_Op (bb, cand_op);
	  Set_BB_SCHEDULE(bbsch);
	  from_hbs_type |= (Ignore_TN_Dep) ? HBS_FROM_POST_GCM_SCHED_AGAIN : 
	                                      HBS_FROM_PRE_GCM_SCHED_AGAIN;

	  to_hbs_type |= (Ignore_TN_Dep) ? HBS_FROM_POST_GCM_SCHED_AGAIN : 
	                                    HBS_FROM_PRE_GCM_SCHED_AGAIN;

	  // Append before any terminating branch (if any) or else just 
	  // insert <cand_op> at the end. Also, check if the <cand_op>
	  // itself needs to be adjusted.

	  TN *old_pred_tn = OP_opnd(cand_op, OP_PREDICATE_OPND);
	  BOOL fail = !Append_Op_To_BB( cand_op, cand_bb, bb, 
                                       motion_type, spec_type);
	  Set_BB_SCHEDULE(cand_bbsch);

	  /*  TODO. right now, we reschedule both blocks. need to implement
	      a faster and accurate version of determining if the motion is 
	      profitable, without actually rescheduling both blocks. */
	
	  Set_OP_moved(cand_op);
	  BBSCH *new_bbsch = Schedule_BB_For_GCM (bb, from_hbs_type, &Sched);
	  Set_BB_flags(new_bbsch, BBSCH_flags(old_bbsch));
	  BBSCH *new_cand_bbsch = Schedule_BB_For_GCM(cand_bb, to_hbs_type, &Sched);

	  from_hbs_type &= (Ignore_TN_Dep) ? ~HBS_FROM_POST_GCM_SCHED_AGAIN: 
					      ~HBS_FROM_PRE_GCM_SCHED_AGAIN;

	  to_hbs_type &= (Ignore_TN_Dep) ? ~HBS_FROM_POST_GCM_SCHED_AGAIN: 
					    ~HBS_FROM_PRE_GCM_SCHED_AGAIN;

	  // Account for any fine-grain register estimate corrections 
	  if (Cur_Gcm_Type & GCM_MINIMIZE_REGS)
	    Adjust_BBSCH(cand_op, cand_bb, bb, new_cand_bbsch, new_bbsch);

	  Reset_OP_moved(cand_op);
	  Reset_BB_SCHEDULE(bbsch);
	  Reset_BB_SCHEDULE(cand_bbsch);
	  Set_OP_visited(cand_op);

	  INT targ_alignment = (Align_Instructions) ? Align_Instructions:
	                                              CGTARG_Text_Alignment();
	  targ_alignment /= INST_BYTES; // so word sized
	  INT16 cand_bb_vacant_slots = Find_Vacant_Slots_BB(cand_bb, 
							    targ_alignment);
	  INT16 bb_vacant_slots = Find_Vacant_Slots_BB(bb, targ_alignment);

#if defined(TARG_SL)
          BOOL should_skip = GCM_Skip_Op_Binary_Search(op_id );
          should_skip |= GCM_Skip_Op(cand_op, bb, moved_loads);
          should_skip |= fail;

          if( Trace_GCM && should_skip ){
            fprintf( TFile, "GCM will skip op_id:%d\n", op_id );
          }
#endif
	  /* If schedule is not better, undo the movement, don't worry 
	     about local minima at the moment. 
	     also, check to see if the <bb> which MUST be aligned is now
	     not as a result of code motion */
	  if ((Ignore_TN_Dep && BB_ALIGNED(new_cand_bbsch) && 
	       ((BB_freq(cand_bb)/BB_freq(bb)) < 1.5) && 
	       ((2 * cand_bb_vacant_slots > targ_alignment))) ||

	      Is_Schedule_Worse(bb, cand_bb, new_bbsch, new_cand_bbsch,
				old_bbsch, old_cand_bbsch) 
#if defined(TARG_SL)
              || should_skip
#endif
	      ) {

	    // TODO: need to do similar thing for downward code motion
	    Perform_Post_GCM_Steps(bb, cand_bb, cand_op, motion_type, 
				   spec_type, &pred_bbs, loop, FALSE);
	    BB_Remove_Op (cand_bb, cand_op);
#if defined(TARG_SL)
            if( position ){
	      BB_Insert_Op_After( bb, position, cand_op );
              if( OP_has_tag(position) ){
                LABEL_IDX tag_idx = 0;
                tag_idx = Get_OP_Tag( position );
                Is_True( tag_idx > 0, ("incorrect tag index") );
                Reset_OP_has_tag( position );
                Set_OP_Tag( cand_op, tag_idx );
              }
            } else
#endif
              BB_Prepend_Op( bb, cand_op );
	    Set_OP_opnd(cand_op, OP_PREDICATE_OPND, old_pred_tn);

	    if (Trace_GCM && GCM_Internal_Flag) {
	      #pragma mips_frequency_hint NEVER
	      Print_Trace_File(cand_op, bb, cand_bb, FALSE);
	      fprintf (TFile, "GCM: OLD Schedule length: MOVEDFROM = %d MOVEDTO = %d\n",
		       BBSCH_schedule_length(old_bbsch), BBSCH_schedule_length(old_cand_bbsch));
	      fprintf (TFile, "GCM: NEW Schedule length: MOVEDFROM = %d MOVEDTO = %d\n",
		       BBSCH_schedule_length(new_bbsch), BBSCH_schedule_length(new_cand_bbsch));
	    }

	    Set_BB_SCHEDULE(bbsch);
	    Set_BB_SCHEDULE(cand_bbsch);
#ifdef KEY
	    // Due to the way the control is organized, it is possible that
	    // the bb, and cand_bb never get scheduled again.
	    // see compilation of gcc.c-torture/compile/950922-1.c
	    // There is no harm in re-scheduling because these are the schedule info that
	    // is latest and is going to be passed around to other modules.
	    bbsch = Schedule_BB_For_GCM (bb, from_hbs_type, &Sched);
	    cand_bbsch = Schedule_BB_For_GCM (cand_bb, to_hbs_type, &Sched);
#endif
	  }
	  else {
#if defined(TARG_SL)
            if( Motion_Is_CIRC_ABOVE(motion_type) ){
              circ_above_moves++;
              Cumulate_Local_TN_Promotion(cand_op);
              /* update left_ops */
              std::vector<OP*>::iterator pos;
              pos = find( left_ops.begin(), left_ops.end(), cand_op );
              if( pos != left_ops.end() )
                left_ops.erase( pos );
              else
                Is_True(0, ("the cand_op is not in left_ops") );
            }
#endif
	    num_moves++;
	    Run_Cflow_GCM |= Is_BB_Empty(bb);
#if !defined(TARG_SL)
	    Reset_OP_visited(cand_op);
#endif
	    Reset_BB_scheduled(bb);
	    Reset_BB_scheduled(cand_bb);
            /* the functionality of Reset_OP_visited is to let further scheduling.
             * However, in single-BB loop circular motion, I don't want a second 
             * time motion.
             */
            if( bb != cand_bb )
              Reset_OP_visited(cand_op);

	    Perform_Post_GCM_Steps(bb, cand_bb, cand_op, motion_type,
				   spec_type, &pred_bbs, loop, TRUE);
	    BCOPY(new_bbsch, old_bbsch, sizeof (BBSCH));
	    BCOPY(new_cand_bbsch, old_cand_bbsch, sizeof (BBSCH));
#if defined(TARG_SL)
            if( Motion_Is_CIRC_ABOVE(motion_type) && OP_load(cand_op) )
              moved_loads = OP_LIST_Push( cand_op, moved_loads, &gcm_loop_pool );
#endif
	    if (Trace_GCM) {
	      #pragma mips_frequency_hint NEVER
	      Print_Trace_File(cand_op, bb, cand_bb, TRUE);
	      fprintf (TFile, "GCM: OLD Schedule length: MOVEDFROM = %d MOVEDTO = %d\n",
		       BBSCH_schedule_length(old_bbsch), BBSCH_schedule_length(old_cand_bbsch));
	      fprintf (TFile, "GCM: NEW Schedule length: MOVEDFROM = %d MOVEDTO = %d\n",
		       BBSCH_schedule_length(new_bbsch), BBSCH_schedule_length(new_cand_bbsch));
	    }
          } // else block

	  // sometimes exit blocks remain with their delay slots unfilled
	  // check for those special cases here and reset the flags, if
	  // necessary.
	  if (BB_exit(bb) && BB_length(bb) >= 2) {
	    OP *delay_op = BB_last_op(bb);
	    // check if the last op in bb is a nop
	    if (delay_op == NULL || !OP_noop(delay_op)) Reset_BB_scheduled(bb);
	  }

          op_id++;
	} // OP_To_Move...
	L_Free();

#if !defined(TARG_SL)
      } // for cand_bb
#else
        /* Do the breaking dependency, only needs for load's in circ-sched */
        if( CG_GCM_enable_break_dependence && moved_loads )
          Break_Dependency( moved_loads, cand_bb );
      } // for cand_bb
       
      if( Motion_Is_CIRC_ABOVE(motion_type) ){
        TN_SET_ClearD( promoted_tns );
        // Do Post Processing
        if( CG_GCM_enable_reduce_loop_count && 
            (circ_above_moves > 0) &&
            Loop_Is_Straight_Line(loop) )
          Reduce_Loop_Count( loop, bb );
        // Do Dead Code Elimination, Only needed for circ_above
        if( CG_GCM_enable_dce && 
            circ_above_moves > 0 &&
            !LOOP_DCE_Skip_Loop_Binary_Search(loop_id) )
          GCM_Dead_Code_Elimination( loop, &gcm_loop_pool );
      }
    } // while <motion_type> loop

    // Merge some small BBs
    if( CG_GCM_enable_merge_small_bbs )
      GCM_Merge_Small_BBs( loop, &gcm_loop_pool );

    if( Trace_GCM_Dump_IR ){
      fprintf(TFile,  "%s   %s  %s", DBar, "after gcm",  DBar);
      Print_BB_No_Srclines(bb);
      
      /* Dump the GCM_Loop_Prolog */
      if( GCM_Loop_Prolog ){
        fprintf(TFile,  "%s   %s  %s", DBar, "GCM Prolog, after gcm",  DBar);
        Print_BB_No_Srclines(GCM_Loop_Prolog);
      }
#endif // TARG_SL
    }
  }//for <bb> loop

  BB_MAP_Delete (bb_cycle_set_map);

  if (Sched) {
	CXX_DELETE(Sched, &MEM_local_pool);
  }
  L_Free();

  return num_moves;
}

#if defined(TARG_SL)
// =======================================================================
// Reduce_Loop_Count
//
// Two motivations:
//   (1) reduce the loop count by 1.
//   (2) copy the left_ops to the epilog
// Note: 
//   (a): if left_ops is empty, there should be something wrong, since I don't
//        want to unroll it.
//   (b): need to check that step (1) is only done once for the loop. or else
//        the loop count is wrong. 
//   (c): an epilog can be shared by two nested loops. Fortunately, seems we
//        already create separte epilog before we arrive here.
//   (d): If the left OPs contains branch, then we cannot copy it to epilog,
//        and we cannot reduce loop count by one.
// =======================================================================
static inline INT32 LC_Number( TN * lc_tn );
static void Reduce_Loop_Count( LOOP_DESCR *loop, BB *src_bb )
{
  BB *epilog = NULL;
  BB *bb = NULL;

  if( Trace_GCM )
    fprintf( TFile, "==Reduce_Loop_Count, for BB:%i\n", BB_id(src_bb) );

  std::vector< LOOP_DESCR* >::const_iterator cit_loop;
  cit_loop = postprocessed_loops.begin();
  for( ; cit_loop != postprocessed_loops.end(); cit_loop++ ){
    if( *cit_loop == loop )
      Is_True( 0, ("a loop is postprocessed for two times") );
  }
  postprocessed_loops.push_back( loop );

  // Find the epilog 
  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb) {
    BBLIST *succs;
    FOR_ALL_BB_SUCCS(bb, succs){
      BB *succ = BBLIST_item(succs);
      if( !BB_SET_MemberP(LOOP_DESCR_bbset(loop), succ) ){
        if( epilog && succ != epilog ){
          epilog  = NULL;
          if( Trace_GCM )
            fprintf(TFile, "There are more than one epilogs");
          return;
        }
        epilog = succ;
      }
    }
  }

  if( epilog == NULL ){
    if( Trace_GCM )
      fprintf( TFile, "This is an infinite loop, cannot find epilog" ); 
    return;
  }else{ 
    if( Trace_GCM )
      fprintf( TFile, "epilog is BB:%i\n", BB_id(epilog) ); 
  }

  // Sometimes, the OPs in src_bb are all removed, if src_bb is not same as
  // target bb. 
  if( !left_ops.size() ){
    if( Trace_GCM )
      fprintf( TFile, "all OPs in srb_bb are moved" );
    return ;
  }

  // only go on if the loop is a zdl, since then I can make my life easy 
  BB *prolog = Loop_Is_Zdl( loop );
  if( !prolog )
    return;
  
  BB *mvtc_host_bb = prolog;
  OP *mvtc = NULL;

  /* check if the mvtc is already moved */
  std::vector< Mvtc_Pos >::const_iterator cit;
  cit = mvtc_targets.begin();
  for( ; cit != mvtc_targets.end(); cit++ ){
    if( cit->orig_loop == loop ){
      Is_True( !mvtc, ("there are two same mvtc in mvtc_targets") );
      mvtc_host_bb = cit->host_bb;
      mvtc = cit->mvtc;
    }
  }
  
  // mvtc is not moved yet, so find it in prolog. Take care, since other
  // loop's mvtc may be here too, need to check the $lp number
  if( !mvtc ){
    OP *loop_op = BB_loop_op( prolog );
    Is_True( OP_is_loop(loop_op),
             ("last op of zdl loop prolog is not loop") );
    /* get the $lp id */
    TN *lp_lvl_tn = OP_opnd( loop_op, 0 );
    Is_True( TN_is_constant(lp_lvl_tn), ("lp_idx_tn is not constant") );
    INT32 lp_lvl = TN_value( lp_lvl_tn );
   
    OP *op;
    FOR_ALL_BB_OPs( prolog, op ){
      if( OP_is_mvtc(op) ){
        TN *lp_tn = OP_result( op, 0 );
        INT32 lp_tn_value = LC_Number( lp_tn );      
        /* Don't break if we find the right lp_tn_value, so that I can check */
        if( lp_tn_value == lp_lvl ){
          Is_True( !mvtc, ("There are two mvtc's with the same loop count reg") );
          mvtc = op;
        }
      }
    }
  }

  Is_True( mvtc, ("cannot find the mvtc ") );

  // reduce the loop count by 1 
  if( Trace_GCM ){
    fprintf( TFile, "reducing loop %p count by 1, from:\n", loop );
    Print_OP_No_SrcLine( mvtc );
  }
    
  if( OP_code(mvtc) == TOP_mvtc_i ){
    TN *tn = OP_opnd( mvtc, 0 );
    INT32 lp_val = TN_value( tn );
    // we may got mvtc $lp0, 0
    if( lp_val < 1 )
      return;
    TN *new_tn = Dup_TN_Even_If_Dedicated( tn );
    Set_TN_value( new_tn, lp_val - 1  );
    Set_OP_opnd( mvtc, 0, new_tn );

    if( Trace_GCM ){
      fprintf( TFile, "to:\n" );
      Print_OP_No_SrcLine( mvtc );
    }
  }else{
    Is_True( OP_code(mvtc) == TOP_mvtc, ("incorrect mvtc") );

    TN *opnd_tn = OP_opnd(mvtc, 0);
    TN *new_tn = Dup_TN_Even_If_Dedicated( opnd_tn );
    Set_OP_opnd( mvtc, 0, new_tn );
  
    /* add a add.i instruction just before the mvtc */
    OP *addi_op = Mk_OP( TOP_addiu, new_tn, opnd_tn, Gen_Literal_TN(-1,4) );
    BB_Insert_Op_Before( mvtc_host_bb, mvtc, addi_op ); 

    if( Trace_GCM ){
      fprintf( TFile, "to:\n" );
      Print_OP_No_SrcLine( addi_op );
      Print_OP_No_SrcLine( mvtc );
    }
  }

  if( Trace_GCM ){
    fprintf( TFile, "\n Before copying left OPs, epilog: \n" );
    Print_BB_No_Srclines( epilog );

    fprintf( TFile, "\ncopy the left OPs to BB:%i\n", BB_id(epilog) );
    std::vector<OP*>::const_iterator cit = left_ops.begin();
    for( ; cit != left_ops.end(); cit++ )
      Print_OP_No_SrcLine( *cit );

  }

  // copy the left ops to epilog, need to duplicate OP since BB_Prepend_Op
  // will change the op->bb property
  std::vector<OP*>::reverse_iterator rit = left_ops.rbegin();
  for( ; rit != left_ops.rend(); rit++ ){
    OP *op = Dup_OP( *rit );
    BB_Prepend_Op( epilog, op );
  }

  BB_SET *bbset;
  bbset = BB_SET_Union1D( LOOP_DESCR_bbset(loop), prolog, &gcm_loop_pool );
  bbset = BB_SET_Union1D( bbset, epilog, &gcm_loop_pool );
  BB_REGION region( bbset, &gcm_loop_pool );
  BB_REGION_Recompute_Global_Live_Info( region, TRUE ); 
  
  if( Trace_GCM ){
    fprintf( TFile, "\n After copying left OPs, epilog: \n" );
    Print_BB_No_Srclines( epilog );
  }
}

// =======================================================================
// MVTC_Optimization
//
// For example:
//    ...
//    mvtc.i $lp1, 100
//    loop 1, tag_1
//    ..
//    mvtc.i $lp0, 100
//    loop 0, tag_0
//    ..
// tag_0:
//    ...
// tag_1:
//    ...
// In this example, "mvtc.i $lp0, 100" will be executed for 100 times, and
// this is un-necessary.
//
// Algorithm: We move the mvtc op from the leaf node of the loop tree, upward
//            one level by one level, so that each node only needs to move
//            the immediate child loop node's mvtc to itself.
// =======================================================================

typedef std::vector< int > Vector1D;
typedef std::vector< Vector1D > Vector2D;
static void MVTC_Opt_Internal( LOOP_DESCR *, Vector1D & );

static void MVTC_Optimization()
{
  LOOP_DESCR_Create_Loop_Tree(&loop_descr_pool);
  if( Trace_GCM ){
    LOOP_DESCR_Dump_Loop_Tree();
  }

  for( int i=0; i < VECTOR_count(loop_tree_roots); i++ ){
    LOOP_DESCR *loop = (LOOP_DESCR*)VECTOR_element( loop_tree_roots, i );

    /* loop_num_per_lvl contains how many child zdl's of current node,
     * loop_num_per_lvl[0] : how many 'mvtc.i $lp0, $n' is all its children 
     * loop_num_per_lvl[1] : how many 'mvtc.i $lp1, $n' is all its children 
     * and so on...
     * Iff loop_num_per_lvl[i]=1, can the mvtc of that ith nested loop 
     * can be moved to current loop head.
     */
    Vector1D loop_num_per_lvl;
    loop_num_per_lvl.push_back(0);
    loop_num_per_lvl.push_back(0);
    loop_num_per_lvl.push_back(0);
    loop_num_per_lvl.push_back(0);
    MVTC_Opt_Internal( loop, loop_num_per_lvl );
  }
}


// ========================================================================
// MVTC_Opt_Internal
// 
// It's done depth first. If a mvtc can be moved to upper, it's moved one
// level upper, and residing in its nesting loop's prolog. Then when 
// working in the 2nd nesting loop, the previously moved mvtc, can be moved
// further to the 2nd nesting loop.
// ========================================================================
static inline INT32 LC_Number( TN * lc_tn )
{
  /* This is ugly, but no other better approaches -_- */
  if( lc_tn == LC0_TN )
    return 0;
  if( lc_tn == LC1_TN )
    return 1;
  if( lc_tn == LC2_TN )
    return 2;
  if( lc_tn == LC3_TN )
    return 3;
  Is_True( 0, ("bad loop counter reigster number") );
  return -1;
}

static void MVTC_Opt_Internal( LOOP_DESCR *loop, Vector1D & loop_num )
{
  int i = 0;
  Vector2D children_data;

  /* compute all the children's data , recursively*/
  for( ; i < VECTOR_count( loop->children ); i++ ){
    Vector1D child_data;
    child_data.push_back(0);
    child_data.push_back(0);
    child_data.push_back(0);
    child_data.push_back(0);
    LOOP_DESCR *child_loop = (LOOP_DESCR*)VECTOR_element( loop->children, i );
    MVTC_Opt_Internal( child_loop, child_data );
    children_data.push_back( child_data );
  }

  /* Accumulate the children's record, and increase nested level by one,
   * and 0th level means loop itself.
   * Since loop_num is not initialized before enter this function, it needs to
   * be initialized dynamically according to the children_data.
   */
  for( int j=0; j < children_data.size(); j++ ){
    for( int k=0 ; k < 4; k++ ){
      loop_num[k] += children_data[j][k];
    }
  }

  if( Trace_GCM ){
    fprintf( TFile, "OPTing loop:\n" );
    LOOP_DESCR_Dump_Loop_Brief( loop );
    fprintf( TFile, "it contains: \n" );
    for(int i=0 ; i < 4; i++ ){
      fprintf( TFile, "  %i %ith level child loop\n", loop_num[i], i );
    }
  }

  /* Determine if current loop is zdl'ed, if not, then nothing to do */
  BB *prolog = Loop_Is_Zdl(loop);
  if( !prolog )
    return;

  OP *loop_op = BB_loop_op( prolog );
  Is_True( OP_is_loop(loop_op), ("last op of zdl loop prolog is not loop") );

  // now move the immediate children's mvtc to current loop head, NOTE:
  // there may be more than one mvtc's in child loop head, which are moved
  // from deeper loop. If suitable, they can be moved to current loop head.

  // get the $lp id
  TN *lp_lvl_tn = OP_opnd( loop_op, 0 );
  Is_True( TN_is_constant(lp_lvl_tn), ("lp_idx_tn is not constant") );
  INT32 lp_lvl = TN_value( lp_lvl_tn );

  /* lp_lvl is the lc_index of this loop itself, it's 0 originally */
  loop_num[lp_lvl] += 1;

  for( i=0 ; i < 4; i++ ){
    for( int j=0; j < VECTOR_count(loop->children); j++ ){
      // If parent loop is zdl, the child loop may be a NON-zdl loop. If so
      // skip it.  TODO: But the child's child loop may be zdl, need do later.
      LOOP_DESCR *child_loop = (LOOP_DESCR*)VECTOR_element(loop->children, j);
      BB *child_head = LOOP_DESCR_loophead( child_loop );
      BB *child_prolog = Loop_Is_Zdl( child_loop );
      if( !child_prolog )
        continue;

      Is_True( OP_is_loop(BB_loop_op(child_prolog)),
               ("last op of child zdl loop prolog is not a loop") );

      OP *op;
      FOR_ALL_BB_OPs( child_prolog, op ){
        /* Now, I only move mvtc.i, since mvtc is more complicated */
        if( OP_code(op) == TOP_mvtc_i ){
          TN *lvl_tn = OP_result( op, 0 );
          Is_True( TN_is_dedicated(lvl_tn), ("lvl_tn is not a dedicated tn") );
          INT32 level = LC_Number( lvl_tn );
          if( loop_num[level] == 1 ){
            BB_Remove_Op( child_prolog, op );
            Is_True( !Is_BB_Empty(prolog), ("the loop prolog is empty") );
            BB_Insert_Op_Before( prolog, BB_loop_op(prolog), op );
            if( Trace_GCM ){
              fprintf( TFile, ("following OP is moved from BB:%i to BB:%i\n"),
                       BB_id(child_prolog), BB_id(prolog) );
              Print_OP_No_SrcLine( op );
            }
            /* record the moving of mvtc.i */
            std::vector< Mvtc_Pos >::iterator pos = mvtc_targets.begin();
            for( ; pos != mvtc_targets.end(); pos++ ){
              if( pos->mvtc == op )
                break;
            }
            if( pos == mvtc_targets.end() ){
              Mvtc_Pos record;
              record.orig_loop = child_loop;
              record.mvtc = op;
              record.host_bb = prolog;
              mvtc_targets.push_back(record);
            }else{
              pos->host_bb = prolog;
            }
          }
        }// if( OP_code(o) == ...
      }// FOR_ALL_BB_OPs
    }
  }

  return;
}
#endif // TARG_SL

// =======================================================================
// GCM_Schedule_Region
//
// The main driver for the global code motion (GCM) phase. The hbs_type will
// determine whether the GCM phase is invoked before or after register 
// allocation. The 
// -O2:  perform GCM phase after register allocation.
// -O3:  perform GCM phase both before/after register allocation
// 
// =======================================================================

void GCM_Schedule_Region (HBS_TYPE hbs_type)
{
  LOOP_DESCR *loop_list;
  LOOP_DESCR *outer_loop;
  BB_SET *all_bbs;
  INT max_nestlevel;	/* for use as circuit breaker */
  BB *bb;
  RID *rid;
  INT totalloops = 0;
  INT innerloops = 0;
  INT callloops = 0;
  INT multibbloops = 0;
  INT exitloops = 0;
  INT loopinfoloops = 0;

  hbs_type |= HBS_FROM_GCM;
  cur_hbs_type = hbs_type;
  Run_Cflow_GCM = FALSE;
  if (hbs_type & (HBS_BEFORE_LRA | HBS_BEFORE_GRA)) {
    /* TODO: need to implement pre- GCM phase */
    Ignore_TN_Dep = FALSE;
    Cur_Gcm_Type = GCM_BEFORE_GRA;
    // GCM_PRE_Pass_Enabled = TRUE;
    if (GCM_Min_Reg_Usage) Cur_Gcm_Type |= GCM_MINIMIZE_REGS;

    if (!GCM_PRE_Enable_Scheduling) return;
  } else {
    Ignore_TN_Dep = TRUE;
    Cur_Gcm_Type = GCM_AFTER_GRA;

    if (!GCM_POST_Enable_Scheduling) return;
  }

#ifdef KEY
  cumulative_cand_bb = 0;
#endif

  if (Trace_GCM) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "GCM_For_Region: PU %s\n", Cur_PU_Name);
  }

  Start_Timer (T_GCM_CU);
  Set_Error_Phase ("Global Code Motion");
  Trace_GCM = Get_Trace (TP_GCM, GCM_TRACE_NORMAL);
  Trace_GCM_Dump_IR = Get_Trace( TP_GCM, GCM_TRACE_DUMP_IR );
  Trace_GCM_Dump_Preprocess = Get_Trace( TP_GCM, GCM_TRACE_PREPROCESS );
  Trace_GCM_Merge_BBs = Get_Trace( TP_GCM, GCM_TRACE_MERGE_BBS );

  MEM_POOL_Initialize (&loop_descr_pool, "LOOP_DESCR_pool", FALSE);
  MEM_POOL_Initialize (&gcm_loop_pool, "GCM loop pool", FALSE);
  MEM_POOL_Push(&loop_descr_pool);
  MEM_POOL_Push(&gcm_loop_pool);

  Calculate_Dominators ();
  if (Ignore_TN_Dep) REG_LIVE_Analyze_Region ();
  CGTARG_Compute_Branch_Parameters(&mispredict, &fixed, &taken, &times);

  L_Save();

  loop_list = LOOP_DESCR_Detect_Loops (&loop_descr_pool);

#if defined(TARG_SL)
  /* Do the mvtc.i optimization */
  if( CG_GCM_enable_mvtc_optimization ){
    mvtc_targets.clear();
    MVTC_Optimization();
  }
#endif

  /* Add the whole region as the outermost loop to make sure we do
   * GCM for the basic blocks outside loops. 
   */
  outer_loop = TYPE_L_ALLOC (LOOP_DESCR);
  BB_Mark_Unreachable_Blocks ();
  all_bbs = BB_SET_Create_Empty (PU_BB_Count+2, &loop_descr_pool);
  bbsch_map = BB_MAP_Create ();
  INT16 num_real_ops;
  INT num_of_nops; // no. of nops required if <bb> was aligned
  OP *op;

  // Initial Pass:
  // Determine if each <bb> will be aligned and store the state.
  // Also, restore other <bb> properties in <bbsch> structure as well
  // Ignore SWP blocks and <bbs> which are unreachable
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (BB_unreachable(bb)) continue;

    BBSCH *bbsch = (BBSCH *)BB_MAP_Get (bbsch_map, bb);
    if (bbsch == NULL) {
      bbsch = TYPE_MEM_POOL_ALLOC (BBSCH, &gcm_loop_pool);
      BZERO (bbsch, sizeof (BBSCH));
      Set_BB_SCHEDULE(bbsch); // need to change this flag
    }

    // check to see if <bb> should be aligned. Entry <bb> of a procedure
    // are automatically aligned.
    num_of_nops = Check_If_Should_Align_BB(bb, INST_BYTES * cur_pc);
    if (num_of_nops || BB_entry(bb)) {
      Set_BB_num_align_nops(bbsch, num_of_nops);
      cur_pc = 0;
      Set_BB_ALIGNED(bbsch);
    }

    Set_BB_start_pc(bbsch, cur_pc);
    INT16 num_ops = 0;
    for (op = BB_first_op(bb); op; op = OP_next(op)) {
      num_real_ops = OP_Real_Ops (op);
      cur_pc += num_real_ops;
      num_ops += num_real_ops;
    }
    Set_BB_num_real_ops(bbsch, num_ops);
    BB_MAP_Set (bbsch_map, bb, bbsch);

    if ( (rid = BB_rid(bb)) && (RID_level(rid) >= RL_CGSCHED)) continue;
    if (BB_scheduled(bb) && !BB_scheduled_hbs(bb)) continue;

    all_bbs = BB_SET_Union1D (all_bbs, bb, &loop_descr_pool);
  }

  outer_loop->mem_pool = &loop_descr_pool;
  LOOP_DESCR_bbset(outer_loop) = all_bbs;
  LOOP_DESCR_loophead(outer_loop) = NULL;
  LOOP_DESCR_nestlevel(outer_loop) = 0;
  LOOP_DESCR_num_exits(outer_loop) = 0;
  LOOP_DESCR_next(outer_loop) = NULL;
  LOOP_DESCR_flags(outer_loop) = 0;

  /*  Add this "loop" to the end of our loop_list. Compute max_nestlevel */
  max_nestlevel = 0;
  if (loop_list == NULL) {
    loop_list = outer_loop;
  } 
  else {
    LOOP_DESCR *lastloop = NULL;
    for( LOOP_DESCR *cloop = loop_list; 
         cloop != NULL; 
         cloop = LOOP_DESCR_next(cloop) ) {
      lastloop = cloop;
      if (LOOP_DESCR_nestlevel(cloop) > max_nestlevel)
	max_nestlevel = LOOP_DESCR_nestlevel (cloop);
    }
    LOOP_DESCR_next(lastloop) = outer_loop;
  }

  BB_SET *processed_bbs = BB_SET_Create_Empty (PU_BB_Count+2, &loop_descr_pool);
  INT num_moves = 0;

  /* Determine the loop properties (eg. no. of innerloops, multibb-loops,
     call-loops, exit-loops. This info is used later in deciding the various
     code motion types */
  LOOP_DESCR *cloop;
  for (cloop = loop_list; cloop != NULL; cloop = LOOP_DESCR_next(cloop)) {
    /* skip the last loop (it is the procedure) 
    if (LOOP_DESCR_next(cloop) == NULL) break; */
    totalloops++;
    BB_SET *loop_bbs = LOOP_DESCR_bbset(cloop);
    BB_SET *tmpset = BB_SET_Intersection (processed_bbs, 
					  loop_bbs,
					  &loop_descr_pool);
    if (BB_SET_EmptyP (tmpset)) {
      Set_Inner_Loop(cloop);
      innerloops++;
      if (LOOP_DESCR_loopinfo(cloop) != NULL) loopinfoloops++;
      INT bb_cnt = 0;
      BOOL has_call = FALSE;
      FOR_ALL_BB_SET_members (loop_bbs, bb) {
	bb_cnt++;
	if (BB_call (bb)) has_call = TRUE;
      }
      if (bb_cnt > 1) {
	multibbloops++;
	Set_Multibb_Loop(cloop);
	if (has_call) {
	  callloops++;
	  Set_Call_Loop(cloop);
	}
	if (LOOP_DESCR_num_exits(cloop) > 1) {
	  exitloops++;
	  Set_Exit_Loop(cloop);
	}
      }
    }
  }

  /* When processing outer loops, we ignore bbs that have already been 
   * processed. These correspond to inner loops.
   */
  BB_SET_ClearD(processed_bbs);
#if defined (TARG_SL)
  postprocessed_loops.clear();
#endif
  loop_id=0;
  for (cloop = loop_list; cloop != NULL; cloop = LOOP_DESCR_next(cloop))
  {
#if defined(TARG_SL)
    // only do licm for real loop , and not do it for 'region', since sl2
    // major/minor region brings in lots of troubles.
    if ( CG_GCM_enable_licm && LOOP_DESCR_loophead(cloop) 
        && !BB_rid(LOOP_DESCR_loophead(cloop)) ) {
      if( !GCM_LICM_Skip_Loop_Binary_Search(loop_id) )
        Perform_Loop_Invariant_Code_Motion(cloop, &gcm_loop_pool, Ignore_TN_Dep);
    }

    // only do loop RCE for real loop , and not do it for 'region', since sl2
    // major/minor region brings in lots of troubles.
    if ( CG_GCM_enable_rce &&
        LOOP_DESCR_loophead(cloop) && 
        !BB_rid(LOOP_DESCR_loophead(cloop)) ) {
      LOOP_Redundant_Copy_Elimination( cloop, &gcm_loop_pool );
    }   
#else
    /* Look only at 'CG_opt_level'  deepest levels of nesting. */
    /* TODO: add a knob to vary this */
    if (LOOP_DESCR_nestlevel(cloop) <= (max_nestlevel - CG_opt_level)) 
	continue;
#endif
 
    GCM_Loop_Prolog = NULL;
#if defined(TARG_SL)
    if( !GCM_Skip_Loop_Binary_Search(loop_id) ) 
    {
      num_moves += GCM_For_Loop (cloop, processed_bbs, hbs_type);
      if (Trace_GCM) 
        fprintf(TFile, ".. Loop:%d, is done by GCM\n", loop_id );
    } else {
      if (Trace_GCM) 
        fprintf(TFile, ".. Loop:%d, is skipped by GCM\n", loop_id );
    }
#endif
    processed_bbs = BB_SET_UnionD (processed_bbs, LOOP_DESCR_bbset(cloop), 
				   &loop_descr_pool); 
    loop_id++;
  }

  if (Trace_GCM) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "GCM_For_Loop: Loop characteristics \n");
    fprintf (TFile, "\ttotalloops %d\n", totalloops);
    fprintf (TFile, "\tinnerloops %d\n", innerloops);
    fprintf (TFile, "\tmultibbloops %d\n", multibbloops);
    fprintf (TFile, "\texitloops %d\n", exitloops);
    fprintf (TFile, "\tcallloops %d\n", callloops);
    fprintf (TFile, "\tloopinfoloops %d\n", loopinfoloops);
    fprintf (TFile, "\tNumber of moves: %d\n", num_moves);
  }

  if (Ignore_TN_Dep) REG_LIVE_Finish ();
  BB_MAP_Delete (bbsch_map);
  L_Free();
  Free_Dominators_Memory ();
  MEM_POOL_Pop (&loop_descr_pool);
  MEM_POOL_Pop (&gcm_loop_pool);
  MEM_POOL_Delete (&loop_descr_pool);
  MEM_POOL_Delete (&gcm_loop_pool);

  Stop_Timer (T_GCM_CU);
  Check_for_Dump (TP_GCM, NULL);
}

#if defined (TARG_SL)
// This is for binary search, to determine a loop should be skipped
// by GCM
static bool GCM_Skip_Loop_Binary_Search( int loop_id )
{
  BOOL skip = Should_Skip( CG_GCM_loop_skip_before,
                           CG_GCM_loop_skip_after,
                           CG_GCM_loop_skip_equal,
                           loop_id );
  return skip;
}

static BOOL GCM_Skip_Op_Binary_Search( INT32 op_id )
{
  BOOL skip = Should_Skip( CG_GCM_op_skip_before,
                           CG_GCM_op_skip_after,
                           CG_GCM_op_skip_equal,
                           op_id );
  return skip;
}

static BOOL GCM_LICM_Skip_Loop_Binary_Search( int loop_id )
{
  BOOL skip = Should_Skip( CG_GCM_LICM_loop_skip_before,
                           CG_GCM_LICM_loop_skip_after,
                           CG_GCM_LICM_loop_skip_equal,
                           loop_id );
  return skip;
}

static BOOL GCM_LICM_Skip_Op_Binary_Search( int op_id )
{
  BOOL skip = Should_Skip( CG_GCM_LICM_op_skip_before,
                           CG_GCM_LICM_op_skip_after,
                           CG_GCM_LICM_op_skip_equal,
                           op_id );
  return skip;
}

static BOOL LOOP_DCE_Skip_Loop_Binary_Search( int loop_id )
{
  BOOL skip = Should_Skip( CG_LOOP_DCE_loop_skip_before,
                           CG_LOOP_DCE_loop_skip_after,
                           CG_LOOP_DCE_loop_skip_equal,
                           loop_id );
  return skip;
}

BOOL LOOP_DCE_Skip_Op_Binary_Search( int op_id )
{
  BOOL skip = Should_Skip( CG_LOOP_DCE_op_skip_before,
                           CG_LOOP_DCE_op_skip_after,
                           CG_LOOP_DCE_op_skip_equal,
                           op_id );
  return skip;
}

/* This function is to determine whether a candidate OP should be ignored.
 * for load->incr_offset->alu sequence, we only circularly move load and 
 * incr_offset,  the following sequences are ignored;
 */
static BOOL GCM_Skip_Op( OP* cand_op, BB* src_bb, OP_LIST* moved_loads )
{
  BOOL got_load_and_incr_add = FALSE;
  if( OP_code(cand_op)==TOP_addi || OP_code(cand_op)==TOP_add16_i ||
      OP_code(cand_op)==TOP_addiu ){
    /* Only consider the self increment of base_tn */
    TN *addop_res, *addop_opnd;
    addop_res = OP_result( cand_op, 0 );
    addop_opnd = OP_opnd( cand_op, 0 );
    if( TN_number(addop_res) == TN_number(addop_opnd) ){
      /* cannot change moved_loads, since it will be used later */
      OP_LIST *loads = moved_loads;
      while( loads ){
        OP *load = OP_LIST_first( loads );
        Is_True( OP_load(load), ("OP not a load, in moved_loads") );
//        Is_True( OP_opnds(load)==2, ("add.i has more than two opnds") );
        TN *load_base_tn = OP_opnd(load, 0);
        /* this addi op increments the base address of the moved load OP */
        if( TN_number(load_base_tn) == TN_number(addop_res) ){
          /* need to check if redefined. The corresponding load and addi is 
           * already moved upwards into the prolog and the tail of loop body, 
           * so only needs to check each OP from load to addi in this src_bb 
           */
          BOOL redefine = FALSE;
          OP *temp_op = load;    
          for( ; temp_op && temp_op != cand_op; temp_op = OP_next(temp_op) ){
            INT32 results = OP_results(temp_op);
            for( INT32 i=0; i < results; i++ ){
              if( TN_is_constant(OP_result(temp_op,i)) )
                continue;
              if( TN_number(load_base_tn) == TN_number( OP_result(temp_op,i) ) ){
                redefine = TRUE;
                break;
              }
            } 
          }
          /* If redefined, then skip it. The consquence of this skipping
           * is that later OPs which depends on this addi OP will be skipped 2.
           */
          if( redefine )
            return TRUE;
          else {
            /* the addi arriving here can be moved. If don't return FALSE here,
             * the cand_op will go to the code following that checks whether
             * it uses the TN of load-addi, and will return TRUE
             */
            load_add_pairs.push_back( std::make_pair(load, cand_op) );
            return FALSE;
          } 
        }
        loads = OP_LIST_rest( loads );
      }    
    }
  }
  
  /* If the load-addi pair is already moved then we MUST NOT move the later
   * OPs which use the result of load. Otherwise we do nothing benefitial
   * 
   * What's more, this ensures correctness of loop iterations number, e.g.
   *   .label:
   *         ldw $2, $3
   *         add.i $3, 4
   *         setlt $5, $3, 100
   *         [others]
   *         br.eqz $5, .label
   * If setlt is moved above circularly, with ldw and add.i, then we get:
   *         ldw $2, $3
   *         add.i $3, 4
   *         setlt $5, $3, 100
   *   .label:
   *         [others]
   *         ldw $2, $3
   *         add.i $3, 4
   *         setlt $5, $3, 100
   *         br.eqz $5, .label
   * Now setlt is executed once more than [others], that means [others] is 
   * executed once less than it should.
   */
  for( INT32 i=0; i < OP_opnds(cand_op); i++ ){
    TN *cand_tn = OP_opnd(cand_op, i);
    if( TN_is_constant(cand_tn) )
      continue;

    std::vector< std::pair<OP*, OP*> >::const_iterator cit;
    for( cit = load_add_pairs.begin(); cit != load_add_pairs.end(); cit++ ){
      OP *load = cit->first;
      OP *add = cit->second;
      Is_True( TN_number(OP_opnd(load,0)) == TN_number(OP_result(add,0)), 
               (" result of load is not same as opnd of add ") );
      if( TN_number(cand_tn) == TN_number(OP_opnd(load,0)) ){
        return TRUE;
      } 
    }
  }

  return FALSE;  
}


/* The goal of this function is to make the loop head, the first bb of loop body,
 * more suitable for circular scheduling. For example:
 *   .label:
 *      add16.i $5,4                    # [0]
 *      add16.i $6,4                    # [1]
 *      ldw $13,-4($5)                  # [1]  id:23
 *      ldw $12,-4($6)                  # [2]  id:24
 *tag_0_9:
 *      add $2,$13,$12                  # [5]
 *
 * I want to transform it to:
 *   .label:
 *      ldw $13,0($5)                  # [1]  id:23
 *      ldw $12,0($6)                  # [2]  id:24
 *      add16.i $5,4                    # [0]
 *      add16.i $6,4                    # [1]
 *tag_0_9:
 *      add $2,$13,$12                  # [5]
 *
 * This can use two add16.i's to fill the latency, and make it better for circular
 * scheduling.
 */
typedef enum {
  UPWARD,   /* load move upward, before add.i */
  DOWNWARD  /* addi move downward, after load */
}DIRECTION;

typedef struct
{
  OP *addi_op;
  OP *load_op;
  DIRECTION dir;
} Tuple;
 
static void Preprocess_Loop_Head( BB* head )
{
  /* record all the exchangable pairs */
  std::vector<Tuple> movable_tuples;

  OP* op;
  OP* addi_op;
  FOR_ALL_BB_OPs( head, addi_op ){
    if( OP_code(addi_op)==TOP_addi || OP_code(addi_op)==TOP_add16_i ||
        OP_code(addi_op)==TOP_addiu ){
      Is_True( OP_results(addi_op)==1, ("add.i op has more than one results") );
      TN *base_tn = OP_result(addi_op, 0);
      
      /* We only take the simplest situation, where addi has the same result
       * TN and operand TN. That means it's just a increment of base tn
       */
      if( TN_number(base_tn) != TN_number(OP_opnd(addi_op,0)) )
        break;
 
      /* Find the load, which uses the base_tn */ 
      BOOL redef = FALSE;
      BOOL reuse = FALSE;
      for( op = OP_next(addi_op); op; op = OP_next(op) ){
        if( OP_load(op) && OP_opnds(op)==2 &&  
            TN_number(base_tn) == TN_number(OP_opnd(op,0)) ){
          TN *load_base_tn = OP_opnd(op, 0);
          /* sometimes, two TNs can reference the same tn_number, but TN themselves 
           * are different, so we use TN_number. Actually, this is bad, we should
           * fix it later.
           *
           * Now, we already got the pair of addi-load, without redefinition
           * in between. So we just exchange the location of addi and load
           *
           * Re-definition must be checked carefully. There are some situations:
           * (1) base_tn of add.i is used between add.i and load;
           * (2) base_tn of add.i is re-defined between ....
           * (3) result_tn of load is used between add.i and load, and certainly 
           *     before load, this forbide load to move upwards;
           * (4) result_tn of load is defined between add.i and load, and certainly 
           *     before load, this forbide load to move upwards;
           *
           * To make life easy, I do the the following motions: 
           * (1) move add.i down just before load, if there is no redef and use of
           *     base_tn
           * (2) exchange load and add.i
           *
           * If (1) fails, do the following:
           * (1) move load upward, just after add.i, if there is no use/def of result
           *     tn of load.
           * (2) exchange load and add.i
           */
          if( !redef && !reuse ){
            Tuple t = {addi_op, op, DOWNWARD};
            movable_tuples.push_back( t );
            if( Trace_GCM_Dump_Preprocess ){
              fprintf( TFile, "-- GCM:Preprocess_loop_head exchange the two OPs --\n" );
              Print_OP_No_SrcLine(addi_op);
              Print_OP_No_SrcLine(op);
            }
          }else{
            BOOL predef = FALSE; 
            BOOL preuse = FALSE; 
            TN *res_tn = OP_result( op, 0 );
            OP *prev_op;
            for( prev_op = OP_prev(op); 
                 prev_op && prev_op != addi_op; 
                 prev_op = OP_prev(prev_op) ){
              /* check if the result tn of load is defined between addi and load */
              for( INT32 i=0; i < OP_results(prev_op); i++ ){
                TN *temp_res = OP_result(prev_op, i);
                if( TN_is_constant(temp_res) )
                  continue;
                if( TN_number(temp_res) == TN_number(res_tn) )
                  predef = TRUE;
              } 
              /* check if the result tn of load is used between addi and load */
              for( INT32 i=0; i < OP_opnds(prev_op); i++ ){
                TN *temp_opnd = OP_opnd(prev_op, i);
                if( TN_is_constant(temp_opnd) )
                  continue;
                if( TN_number(temp_opnd) == TN_number(res_tn) )
                  preuse = TRUE;
              } 
            }
            /* move them */
            if( !predef && !preuse ){
              Tuple t = {addi_op, op, UPWARD};
              movable_tuples.push_back( t );
              if( Trace_GCM_Dump_Preprocess ){
                fprintf(TFile, "-- GCM:Preprocess_loop_head exchange the two OPs --\n");
                Print_OP_No_SrcLine(addi_op);
                Print_OP_No_SrcLine(op);
              }
            }
          }

        }
        /* check if the base_tn is changed between addi and load */
        for( INT32 i=0; i < OP_results(op); i++ ){
          TN *temp_res = OP_result(op, i);
          if( TN_is_constant(temp_res) )
            continue;
          if( TN_number(temp_res) == TN_number(base_tn) )
            redef = TRUE;
        } 
        /* check if the base_tn is used between addi and load */
        for( INT32 i=0; i < OP_opnds(op); i++ ){
          TN *temp_opnd = OP_opnd(op, i);
          if( TN_is_constant(temp_opnd) )
            continue;
          if( TN_number(temp_opnd) == TN_number(base_tn) )
            reuse = TRUE;
        } 
      }
    }
  } 

  /* If exchanged the two OPs, need to modify the offset, 
   * (1) get the increment of add.i
   * (2) supplement the increment to the load offset 
   */
  std::vector<Tuple>::const_iterator cit = movable_tuples.begin();
  for( ; cit != movable_tuples.end(); cit++ ){
    /* move them */
    if( cit->dir == UPWARD )
      BB_Move_Op_Before( head, cit->addi_op, head, cit->load_op ); 
    else
      BB_Move_Op_After( head, cit->load_op, head, cit->addi_op ); 
    /* modify the offset */
    TN *incre_tn = OP_opnd( cit->addi_op, 1 );
    INT64 increment = TN_value(incre_tn);
    TN *offset_tn = OP_opnd( cit->load_op, 1 ); 
    INT64 offset = TN_value(offset_tn);
    TN *new_offset_tn = Dup_TN( offset_tn );
    Set_TN_value( new_offset_tn, increment + offset );
    Set_OP_opnd( cit->load_op, 1, new_offset_tn );
  }
}

/* For the following code sequence:
 *   .label:
 *       ...
 *       add.i TN2, TN2, 1
 *       setlt TN1, TN2, TN3
 *       br.nez TN1, label
 * 
 * Or another one:
 *   .label:
 *       ...
 *       add.i TN2 , TN2, 1
 *       ldw TN1, TN2
 *       br.nez TN1, label
 * 
 * If the setlt and ldw, and with the above  add.i, are moved circularly, that 
 * means the definition point of TN1, which controls the loop iteration number, 
 * is executed once more than it should be.
 * So for the most close-to-branch definition OP, it cannot be circ-moved.
 */
static void Find_Last_Defs_Of_Branch( BB* head )
{
  if( Is_BB_Empty(head) )
    return;

  OP *br_op = BB_last_op(head);
  if( br_op && OP_br(br_op) ){
    Is_True( br_op == BB_branch_op(head), ("the two macro of finding branch op is not consistent") );
    /* find the last definition */
    for( int i=0; i < OP_opnds(br_op); i++ ){
      TN *opnd_tn = OP_opnd(br_op, i);
      OP *def_op;
      FOR_ALL_BB_OPs_REV( head, def_op ){
        if( OP_Defs_TN(def_op, opnd_tn) ){
          last_defs_of_branch.push_back(def_op);
          break;
        }
      } 
    }
  }

  return;
}

//=======================================================================
// Relaxed_Topo_Sort
//
// This relaxed topo sort sorts all the BBs even there are inner loop. The 
// existing topological sort method in other files only deal with innermost
// loop strctly. However I need one that is not strict topological
//
//=======================================================================
static void Relaxed_Topo_Sort( LOOP_DESCR *loop, 
                               std::vector<BB*> *sorted, 
                               MEM_POOL *pool )
{
  BB *head = LOOP_DESCR_loophead(loop);
  BB_SET *body = LOOP_DESCR_bbset(loop);
  
  sorted->push_back(head);
  
  std::list<BB*> working_list;
  working_list.push_back(head);

  // visited records the same list of BBs as sorted during the processing, 
  // and we can use 'sorted' only. However it's more convenient to use a
  // BB_SET to search.
  BB_SET *visited = BB_SET_Create_Empty(PU_BB_Count, pool);
  visited = BB_SET_Union1D (visited, head, pool);

  BB *cur;
  while( !working_list.empty() ){
    cur = working_list.front();
    working_list.pop_front();

    BBLIST *s;
    BOOL got_one = FALSE;
    FOR_ALL_BB_SUCCS(cur, s){
      BB* succ = BBLIST_item(s);
      if( !BB_SET_MemberP(body,succ) || BB_SET_MemberP(visited,succ) )
        continue; 
  
      BOOL all_pred_visited = TRUE;
      if(succ != head){
        BBLIST *p;
        FOR_ALL_BB_PREDS(succ, p){
          BB* pred = BBLIST_item (p);
          if( (pred != cur) && !BB_SET_MemberP(visited, pred) ) {
            all_pred_visited = FALSE; 
            break;
          }
        }
      }
  
      if(all_pred_visited){ 
        visited = BB_SET_Union1D (visited, succ, pool);
        sorted->push_back(succ); 
        working_list.push_back(succ);
        got_one = TRUE;
      }
    }

    // if we cannot got one successor in restrict TOPO sort, then we'll add
    // all it's successors. This is why it's name 'relaxed'
    FOR_ALL_BB_SUCCS(cur, s){
      BB* succ = BBLIST_item(s);
      if( !BB_SET_MemberP(body,succ) || BB_SET_MemberP(visited,succ) )
        continue; 
      visited = BB_SET_Union1D (visited, succ, pool);
      sorted->push_back(succ); 
      working_list.push_back(succ);
    } 
  }
}

//=======================================================================
// GCM_Merge_Small_BBs
//
// after gcm, there may be some small BBs, which are in straight line
// if there are no other entry point, then we can shrink the OPs down
// to the last BB (we cannot remove BBs, since they may be used in loop's
// bbset). 
//
// This optimization can reduce the code size because we create a bigger BB
// out of small BBS, and then we can merge some 16bit instructions into 32bit
// 
// We iterate all the BBs, in topo order. If a BB falls through to it
// successor, and there is no other branches arrives between, then  we sink
// the OPs in pred to succ.
//=======================================================================
static void GCM_Merge_Small_BBs( LOOP_DESCR *loop, MEM_POOL *pool )
{
  if( !LOOP_DESCR_loophead(loop) ){
    if( Trace_GCM_Merge_BBs )
      fprintf( TFile, " no loop head, not a real loop, wont merge bbs\n");
    return;
  }

  GRA_LIVE_Init(NULL);

  INT32 move_num = 0;

  std::vector<BB*> sorted; 
  Relaxed_Topo_Sort( loop, &sorted, pool );
  
  std::vector<BB*>::const_iterator cit = sorted.begin();
  if( Trace_GCM_Merge_BBs ){
    fprintf( TFile, " === Begin GCM_Merge_Small_BBs ===\n" );
    fprintf( TFile, " the relaxed topo sort ordering of all BBs is: " );
    for( cit = sorted.begin(); cit != sorted.end(); cit++ )  
      fprintf( TFile, " %i", BB_id(*cit) );
    fprintf( TFile, "\n" );
  }

  cit = sorted.begin();
  for(; cit != sorted.end(); cit++ ){
    BB *cur_bb = *cit;

    BB *fall_thru = NULL;
    if( BB_next(cur_bb) && BB_Find_Succ(cur_bb, BB_next(cur_bb)) )  
      fall_thru = BB_next(cur_bb);

    // check if the pair of BBs are straight line
    if( !fall_thru || BB_succs_len(cur_bb) != 1 || BB_preds_len(fall_thru) != 1 )
      continue;

    // if the last OP is a call, we cannot merget them
    OP *last_op = BB_last_op(cur_bb);
    if( !last_op || OP_call(last_op) )
      continue;

    if( Trace_GCM_Merge_BBs ){
      fprintf( TFile, "before merging:\n" );
      Print_BB_No_Srclines( cur_bb );
      Print_BB_No_Srclines( fall_thru );
    }
  
    // If the last OP of cur BB is branch, only jump is allowed
    // we can remove it directly.
    if( OP_br(last_op) ){
      Is_True( OP_jump(last_op), ("last op in pred BB must be jump") );
      BB_Remove_Op( cur_bb, last_op );
      if( Trace_GCM_Merge_BBs )
        fprintf( TFile, "the last OP is jump, remove it\n" );
    }

    // sink down now.
    OP *op = NULL;
    FOR_ALL_BB_OPs_REV( cur_bb, op ){
      BB_Remove_Op( cur_bb, op );
      BB_Prepend_Op( fall_thru, op );
      move_num++;
    } 

    if( Trace_GCM_Merge_BBs ){
      fprintf( TFile, " after merging:\n");
      Print_BB_No_Srclines( cur_bb );
      Print_BB_No_Srclines( fall_thru );
    }
  }

  if( Trace_GCM_Merge_BBs )
    fprintf( TFile, "total moves:%i\n", move_num );

  GRA_LIVE_Init(NULL);
}
#endif // TARG_SL
