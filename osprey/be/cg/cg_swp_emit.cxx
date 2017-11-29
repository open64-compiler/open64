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


/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_swp_emit.cxx
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/cg_swp_emit.cxx,v $
 *
 *  Description:
 *  ============
 *
 *  Code generation phase after modulo scheduling and register allocation.
 *
 * =======================================================================
 * ======================================================================= */

// ************************************************************************
//
// This file implements the translation of modulo-scheduled and rotating-
// register-allocated code into the CGIR recognized by the rest of CG.
// The following lists of the constraints and possible interactions with
// downstream CG components.
// 
//
// Generation of SWP region:
// 
//  - SWP produces a region of type RID_TYPE_swp. (See be/region/region_util.h)
//  - The region contains a set of BBs that have been scheduled and
//    register allocated.
//  - There is one prolog/epilog block for each entry/exit of the region.
//  - A backpatch in the prolog/epilog blocks is converted into a TN copy.
//  - The TN copies are called the glue code.
//    They are marked OP_glue(). 
//    In the prolog, the result has already been assigned a register.
//    In the epilog, the operand has already been assigned a register.
//    Predicate copy is expanded into a sequence of operations.
//  - The SWP region is considered a black-box.  BBs in the region
//    are not processed by any subsequent optimizations, except
//    bundling in cgemit.cxx.
//  - The prolog/epilog blocks needs further optimizations,
//    but the glue code imposes some restrictions.
//
// GRA interactions:
//
//  - The TNs used in the SWP region are used in the
//    SWP region, used as results in prolog glue copies, or used 
//    as operands in the epilog copies.  Those TNs are not used elsewhere.
//    This is to relieve GRA from worrying about live ranges that span the 
//    boundary of a SWP region.  A consequence is that any invariants
//    must be assigned a new TN inside the SWP loop, and a glue
//    copy is introduced to copy the value of old TN into the new one.
//    Similar requirements for other live-in, live-out TNs are enforced
//    by the conversion of backpatches into glue copies.
//
// HB_SCHED/LRA interactions:
//
//  - The prolog/epilog are allowed to have a mix of glue and non-glue
//    operations, i.e., instructions are partially register allocated.
//    For example, the initialization of predicate registers,
//    the initialization of the loop-counter and epilog-counter are
//    inserted to the prolog block.
//
// EBO interactions:
//
//  - In general glue copies cannot be optimized away, except by GRA preferencing.
//  - EBO may combine a glue copy with other operations, but must preserve
//    the register assignment.
//
// CFLOW interactions:
//
//  - cflow may merge prolog/epilog blocks with their predecessors/successors,
//  - cflow may reorder the prolog/epilog blocks with the restriction that
//    the fall-through epilog block from the SWP region remains in the
//    fall-through path.
//  - cflow should not reorder blocks in the SWP region.
//
// REG_LIVE interaction:
//
//  - REG_LIVE information are needed by postpass GCM, and EBO after scheduling.
//  - If registers do not rotate, REG_LIVE (see reg_live.cxx) can figure out 
//    the register live-in and kill set by examining the SWP-scheduled basic
//    blocks.
//  - If registers rotate, a use of Rx refers to Ry defined in a previous 
//    iteration, it is impossible to determine the register live-in and kill
//    set without annotations.   
//  - We will add a new BB annotation kind:  ANNOT_ROTATING_KERNEL.
//    The info stored in ANNOT_ROTATING_KERNEL are the live-in, kill 
//    REGSET.
//  - We will also a BB_flag: BB_ROTATING_KERNEL to indicate the
//    existence of REGSET annotations.   The flag can be tested
//    using BB_rotating_kernel(bb).
//  - If GRA decides to rename any logical registers in the SWP kernel,
//    GRA must also to update the REGSETs in ANNOT_ROTATING_KERNEL.
//
// ************************************************************************


#define USE_STANDARD_TYPES

#include "defs.h"
#include "cg.h"
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "tracing.h"
#include "op.h"
#include "op_list.h"
#include "bb.h"
#include "cgexp.h"
#include "tn.h"
#include "tn_set.h"
#include "cg_loop.h"
#include "register.h"
#include "cg_swp_target.h"
#include "annotations.h"
#include "ti_res_res.h"
#include "ti_res_count.h"


// The function object returns TRUE when the schedule slot of OP[i] 
// is smaller than that of OP[j].  With bundling no two ops should
// have the same slot number; without bundling all ops with the same
// slot number (== modulo cycle number) will be sorted into
// arbitrary order.
//
struct Order_Op_State_by_Modulo_Cycles {
  const SWP_OP_vector& state;
  bool operator()(INT i, INT j) { 
    return state[i].slot < state[j].slot; 
  }
  Order_Op_State_by_Modulo_Cycles(const SWP_OP_vector& op_state):state(op_state) {}
};


// Rearrange the OPs according to the SWP schedule.
//   - first remove all OPs from the loop body
//   - then insert the OPs based on <modulo-cycle, slot> ordering
//   - also modify their controlling predicate
//  
static void 
SWP_Reorder_OPs(const SWP_OP_vector& op_state,
		const SWP_REG_ASSIGNMENT& reg_assign,
		BB *body, bool trace)
{
  BB_Remove_All(body);  // remove all OPs

  INT         i;
  std::vector<INT> sorted_op;
  for (i = 0; i < op_state.size(); i++) {
    if (op_state[i].op) 
      sorted_op.push_back(i);
  }
  std::sort(sorted_op.begin(), sorted_op.end(), Order_Op_State_by_Modulo_Cycles(op_state));

  INT ii = op_state.ii;
  for (i = 0; i < sorted_op.size(); i++) {
    OP *op =  op_state[sorted_op[i]].op;
    bool is_noop = op_state[sorted_op[i]].is_noop;
    OP_scycle(op) = op_state[sorted_op[i]].cycle;
    if (OP_has_predicate(op) &&
	TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND)) &&
	!is_noop) {
      INT stage = op_state[sorted_op[i]].cycle / ii;
      Set_OP_opnd(op, OP_PREDICATE_OPND, reg_assign.Get_Control_Predicate(stage));
    }
    BB_Append_Op(body, op);
  }

  if (trace) {
    for (i = 0; i < sorted_op.size(); i++) {
      OP *op =  op_state[sorted_op[i]].op;
      fprintf(TFile, "%d: OP %d cycle=%d\n", i, sorted_op[i], op_state[sorted_op[i]].cycle);
    }
  }
}


// Lookup the assigned register TN from the CLASS_REG_PAIR
//
static TN *
Lookup_Register_TN(SWP_REG_ASSIGNMENT::REG2TN_MAP& reg2tn_map, 
		   CLASS_REG_PAIR                  rp,
		   TN                             *tn,
		   bool                            trace)
{
  Is_True(CLASS_REG_PAIR_rclass(rp) >= ISA_REGISTER_CLASS_MIN &&
	  CLASS_REG_PAIR_rclass(rp) <= ISA_REGISTER_CLASS_MAX,
	  ("Lookup_Register_TN: invalid register class %d\n", CLASS_REG_PAIR_rclass(rp)));
	  
  TN *rtn;
  if (reg2tn_map.find(rp) == reg2tn_map.end()) {
    // Register TN not allocated --> Generate a new one
    rtn = Dup_TN(tn);
    Set_TN_class_reg(rtn, rp);
    // Set TN is dedicated.  Otherwise GRA will assign registers again.
#ifndef TARG_IA64
    Set_TN_is_dedicated(rtn);
#endif
    reg2tn_map[rp] = rtn;
    if (trace) {
      fPrint_TN(TFile, "SWP Allocator: Reference to %s", tn);
      fPrint_TN(TFile, " renamed as %s\n", rtn);
    }
  } else
    rtn = reg2tn_map[rp];
  return rtn;
}


// Determine the rotating register assigned for 'tn', assuming unbounded
// rotating register banks.
// The 'adjustment' is the offset in register numbering, when converting 
// from a physical pseudo-register location of the current reference to
// the corresponding (unbounded) rotating register number.
// 
TN *SWP_REG_ASSIGNMENT::Get_Register_TN(TN *tn, INT adjustment) 
{
  Is_True(adjustment >= 0,
	  ("SWP_REG_ASSIGNMENT: Unexpected negative register-number offset."));
#ifdef TARG_IA64
  Is_True(reg_allocation.find(tn) != reg_allocation.end(),
	  ("SWP_REG_ASSIGNMENT: can't locate TN%d.", TN_number(tn)));
#endif
  CLASS_REG_PAIR rp = reg_allocation[tn];
  REGISTER r = CLASS_REG_PAIR_reg(rp);
  ISA_REGISTER_CLASS c = CLASS_REG_PAIR_rclass(rp);
#ifndef TARG_IA64
  c = ISA_REGISTER_CLASS_integer;
#endif
    
  // Assuming a rotating logical register is numbered the same as the
  // corresponding physical pseudo-register location at stage 0, add in
  // the offset to get the logical register number:
  //
  // The correct register computation should be
  //   r = ((r + adjustment) % rotating_reg_avail[c]) + rotating_reg_base[c];
  // The issue is that we don't know the size of the rotating register file
  // at this time, since it is determined based on all loops in a PU, so 
  // delay the modulo computation to a postpass (see SWP_Fixup).
  //
  r = r + adjustment + rotating_reg_base[c];

  // workaround g++ bug:  Set_CLASS_REG_PAIR_reg(rp, r);
  Set_CLASS_REG_PAIR(rp, c, r);

  return Lookup_Register_TN(reg2tn_map, rp, tn, Trace());
}


TN *SWP_REG_ASSIGNMENT::Get_Control_Predicate(INT stage) const
{
  TN *tn = NULL;
#ifdef HAS_ROTATING_REGISTERS
  tn = Gen_Predicate_TN();
  REGISTER r = 
    (REGISTER)(rotating_reg_base[ISA_REGISTER_CLASS_predicate] + 
	       control_predicate_loc + 
	       stage);
  Set_TN_register(tn, r);
  // Set TN is dedicated.  Otherwise GRA will assign registers again.
  Set_TN_is_dedicated(tn);
  if (Trace()) {
    fPrint_TN(TFile, 
	      "SWP Allocator: Control predicate is %s", tn);
    fprintf(TFile, " in stage %d\n", stage);
  }
#endif
  return tn;
}

// Determine the register TN assigned for an invariant TN.
//
TN *SWP_REG_ASSIGNMENT::Get_Non_Rotating_Register_TN(TN *tn)
{
  CLASS_REG_PAIR rp;
  if (reg_allocation.find(tn) != reg_allocation.end())
    rp = reg_allocation[tn];
  else {
    ISA_REGISTER_CLASS rc = TN_register_class(tn);
    REGISTER r;
  /* Try to get one that doesn't need to be saved. */
    r = REGISTER_SET_Choose_Intersection(non_rotating_reg[rc],
                                         REGISTER_CLASS_caller_saves(rc));
    if (r == REGISTER_UNDEFINED ) r = REGISTER_SET_Choose(non_rotating_reg[rc]);
    Is_True(r != REGISTER_UNDEFINED,
	    ("SWP_REG_ASSIGNMENT: run out of non-rotating registers."));
    non_rotating_reg[rc] =  REGISTER_SET_Difference1(non_rotating_reg[rc], r);
    Set_CLASS_REG_PAIR(rp, rc, r);  
    reg_allocation[tn] = rp;
  }
    
  return Lookup_Register_TN(reg2tn_map, rp, tn, Trace());
}
  

// Return TRUE if all loop invariants can be assigned registers
//
bool SWP_REG_ASSIGNMENT::Enough_Non_Rotating_Registers(TN_SET *non_rotating) const
{
  INT reg_needed[ISA_REGISTER_CLASS_MAX+1];
  ISA_REGISTER_CLASS i;
  FOR_ALL_ISA_REGISTER_CLASS(i) {
    reg_needed[i] = 0;
  }

  for (TN *tn = TN_SET_Choose(non_rotating);
       tn != TN_SET_CHOOSE_FAILURE;
       tn = TN_SET_Choose_Next(non_rotating,tn)) {
    ISA_REGISTER_CLASS rc = TN_register_class(tn);
    if (REGISTER_Has_Rotating_Registers(rc)) 
      reg_needed[rc]++;
  }

  FOR_ALL_ISA_REGISTER_CLASS(i) {
    if (REGISTER_SET_Size(non_rotating_reg[i]) < reg_needed[i])
      return false;
  }
  
  return true;
}

  
// Update SWP BB annotation 
//  - the livein and kill REGSET
void SWP_REG_ASSIGNMENT::Update_Annotation(ROTATING_KERNEL_INFO *info)
{
  ISA_REGISTER_CLASS i;
  FOR_ALL_ISA_REGISTER_CLASS(i) {
    // The processing of int register are processed in the fixup
    //
    REGISTER_SET tmp = REGISTER_CLASS_allocatable(i);
    tmp = REGISTER_SET_Difference(tmp, non_rotating_reg[i]);
#ifdef TARG_IA64
//#ifndef KEY
    if (i != ISA_REGISTER_CLASS_integer) {
      // assume all rotating registers are killed 
      // for some reason, rotating reg are not in REGISTER_CLASS_allocatable()?
      tmp = REGISTER_SET_Union(tmp,
			       REGISTER_SET_Range(rotating_reg_base[i],
						  rotating_reg_base[i] + rotating_reg_avail[i] - 1));
    }
#endif

    ROTATING_KERNEL_INFO_live_in(info)[i] = tmp;
    ROTATING_KERNEL_INFO_kill(info)[i] = tmp;
  }
}


// Initialization of SWP_REG_ASSIGNMENT
//
SWP_REG_ASSIGNMENT::SWP_REG_ASSIGNMENT()
{
  ISA_REGISTER_CLASS i;
  FOR_ALL_ISA_REGISTER_CLASS(i) {

#ifdef HAS_ROTATING_REGISTERS
    rotating_reg_base[i] = REGISTER_First_Rotating_Registers(i);
    rotating_reg_avail[i] = REGISTER_Last_Rotating_Registers(i) - 
      REGISTER_First_Rotating_Registers(i) + 1;
    
    // mask out rotating register from non-rotating allocatable set.
    non_rotating_reg[i] =
      REGISTER_SET_Difference_Range(REGISTER_CLASS_allocatable(i),
				    rotating_reg_base[i],
				    rotating_reg_base[i] + rotating_reg_avail[i] - 1);

#else
    non_rotating_reg[i] = REGISTER_CLASS_allocatable(i);
#endif
  }
#ifdef TARG_IA64
  // if the function call other function with integer parameter, 
  // reserve 8 reg from the rotating_reg_avail, or if the rotating 
  // register share a same register with the output registers, function
  // call in this function will get wrong parameter's. 
  // output registers should be already set at the begining of 
  // CG_Generate_Code through functino Convert_WHIRL_To_OPs 

  if ( REGISTER_Number_Stacked_Output (ISA_REGISTER_CLASS_integer) )
    rotating_reg_avail[ISA_REGISTER_CLASS_integer] -= 8 ;
#endif

}


// Generate a glue copy
//
static void
SWP_Add_Glue(TN *result, TN *opnd, BB *bb, bool append)
{
  OPS ops = OPS_EMPTY;
  SWP_Exp_COPY(result, opnd, &ops); 
  OP *op;
  FOR_ALL_OPS_OPs(&ops, op) {
    Set_OP_glue(op);
  }
  if (append)
    BB_Append_Ops(bb, &ops);
  else
    BB_Prepend_Ops(bb, &ops);
}


// Replace the TN in the SWP loop by register-assigned TNs.
//  - A TN in SWP are used in different stages, hence have
//    different register allocated.   Since one TN only has
//    one assignment, it is impossible to keep the same TN.
//    
static void
SWP_Rename_TNs(const SWP_OP_vector& op_state, 
	       SWP_REG_ASSIGNMENT& reg_assign,
	       BB *head, BB *tail)
{
  INT     ii = op_state.ii;
  INT     sc = op_state.sc;
  TN_SET *non_rotating = op_state.tn_non_rotating;
  TN_SET *invariants = op_state.tn_invariants;

  if (SWP_REG_ASSIGNMENT::Trace()) {
    fprintf(TFile, "\nSWP Allocator: TN Renaming\n");
    fprintf(TFile, "--------------------------\n");
  }

  {
    for (TN *tn = TN_SET_Choose(invariants);
	 tn != TN_SET_CHOOSE_FAILURE;
	 tn = TN_SET_Choose_Next(invariants,tn)) {
      if (!TN_is_dedicated(tn)) {
	TN *rtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
	SWP_Add_Glue(rtn, tn, head, true/*append*/ );
      }
    }
  }

  // Rename SWP body; note that we will not rename the special control
  // registers created by SWP_REG_ASSIGNMENT::Get_Control_Predicate, since
  // these are marked as "dedicated".
  //
  for (INT i = 0; i < op_state.size(); i++) {
    OP *op = op_state[i].op;
    if (op) {
      for (INT j = 0; j < OP_opnds(op); j++) {
	TN *tn = OP_opnd(op, j);
	if (TN_is_register(tn) &&
	    !TN_is_dedicated(tn)) {
	  TN *newtn;
#ifdef TARG_IA64
	  if (!TN_SET_MemberP(non_rotating, tn)) {
	    INT ofst = reg_assign.Get_Register_Offset(op_state[i].cycle, ii, OP_omega(op,j));
	    newtn = reg_assign.Get_Register_TN(tn, ofst);
	  } else 
	    newtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
#else
	  if (TN_SET_MemberP(non_rotating, tn)) {
	    newtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
	  } else { 
	    INT ofst = reg_assign.Get_Register_Offset(op_state[i].cycle, ii, OP_omega(op,j));
	    if (SWP_REG_ASSIGNMENT::Trace()) {
	      fprintf(TFile, "op %d: reg ofst for operand tn %d = %d (cycle %d, omega %d)\n", i, TN_number(tn), ofst, op_state[i].cycle, OP_omega(op,j));
	    }
	    newtn = reg_assign.Get_Register_TN(tn, ofst);
	  }
#endif
	  Set_OP_opnd(op, j, newtn);
	}
      }
      for (INT k = 0; k < OP_results(op); k++) {
	TN *tn = OP_result(op, k);
	if (TN_is_register(tn) &&
	    !TN_is_dedicated(tn)) {
	  Is_True(!TN_SET_MemberP(invariants, tn),
		  ("SWP_Rename_Body: result TN%d cannot be an invariant.", TN_number(tn)));
	  TN *newtn;
#ifdef TARG_IA64
	  if (!TN_SET_MemberP(non_rotating, tn)) {
	    INT ofst = reg_assign.Get_Register_Offset(op_state[i].cycle, ii, 0);
	    newtn = reg_assign.Get_Register_TN(tn, ofst);
	  } else
	    newtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
#else
	  if (TN_SET_MemberP(non_rotating, tn)) {
	    newtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
	  } else {
	    INT ofst = reg_assign.Get_Register_Offset(op_state[i].cycle, ii, 0);
	    if (SWP_REG_ASSIGNMENT::Trace()) {
	      fprintf(TFile, "op %d: reg ofst for result tn %d = %d (cycle %d)\n", i, TN_number(tn), ofst, op_state[i].cycle);
	    }
	    newtn = reg_assign.Get_Register_TN(tn, ofst);
	  }
#endif // TARG_IA64
	  Set_OP_result(op, k, newtn);
	}
      }
    }
  }

  CG_LOOP_BACKPATCH *bp;
  // Generate copies for prolog-backpatches.
  for (bp = CG_LOOP_Backpatch_First(head, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    TN *tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    TN *newtn;
    if (!TN_is_dedicated(body_tn)) {
      if (!TN_SET_MemberP(non_rotating, body_tn)) {
	INT omega = CG_LOOP_BACKPATCH_omega(bp);
	INT ofst = reg_assign.Get_Livein_Register_Offset(omega);
	newtn = reg_assign.Get_Register_TN(body_tn, ofst);
      } else
	newtn = reg_assign.Get_Non_Rotating_Register_TN(body_tn);
      SWP_Add_Glue(newtn, tn, head, true/*append*/);
    }
  }

  // Generate copies for epilog-backpatches.
  for (bp = CG_LOOP_Backpatch_First(tail, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    TN *tn = CG_LOOP_BACKPATCH_non_body_tn(bp); 
    TN *newtn;
    if (!TN_is_dedicated(body_tn)) {
      if (!TN_SET_MemberP(non_rotating, body_tn)) {
	INT omega = CG_LOOP_BACKPATCH_omega(bp);
	INT ofst = reg_assign.Get_Liveout_Register_Offset(sc, omega);
#ifndef TARG_IA64
	if (SWP_REG_ASSIGNMENT::Trace()) {
		fprintf(TFile, "reg ofst for epilog tn %d = %d\n", TN_number(body_tn), ofst);
	}
#endif
	newtn = reg_assign.Get_Register_TN(body_tn, ofst);
      } else
	newtn = reg_assign.Get_Non_Rotating_Register_TN(body_tn);
      SWP_Add_Glue(tn, newtn, tail, false/*prepend*/);
    }
  }
} // SWP_Rename_TNs


static TN*
Fixup_Rotating_Register_TN(TN *tn, const SWP_FIXUP &fixup, bool trace)
{
  TN *rtn = tn;

#ifdef HAS_ROTATING_REGISTERS
  // The allocation of rotating registers thus far has been unbounded and
  // based at the appropriate rotating_reg_base.  Now we know how many 
  // registers are assigned to each rotating register bank, and we can
  // adjust all registers such that they are in-sequence within the
  // rotating register bank.  We also adjust predicate registers such 
  // that the special control predicate is located in the n first logical
  // registers (n==stages).
  //
  const ISA_REGISTER_CLASS rc = TN_register_class(tn);

  REGISTER       r = TN_register(tn);

  const REGISTER old_r = r;
  if (REGISTER_Has_Rotating_Registers(rc)) {

    // Invariants will have been allocated at register numbers smaller than
    // the rotating register bank.  The following ignores such registers in
    // the fixup algorithm.
    //
    // WARNING:  REGISTER is an unsigned type. Some of the arithmetic
    // below (in the predicate case) may produce negative intermediate
    // results.  Testing them successfully requires explicit
    // conversions to (int).
    //
    if (r >= REGISTER_First_Rotating_Registers(rc)) {
      if (rc == ISA_REGISTER_CLASS_predicate) {
#ifdef TARG_IA64      	
        //Bug fix:
        //The order of calculate must be guarded!
        
      	if ( ((int)(r - fixup.control_loc)) < 0 ) {
          r += REGISTER_Last_Rotating_Registers(rc) - 
	          REGISTER_First_Rotating_Registers(rc) + 1;
          r -= fixup.control_loc;
      	} else {
	  r -= fixup.control_loc;  // Control lifetime uses first rotating regs
	  if (r < REGISTER_First_Rotating_Registers(rc))
	    r += REGISTER_Last_Rotating_Registers(rc) - 
	            REGISTER_First_Rotating_Registers(rc) + 1;
	  else if (r > REGISTER_Last_Rotating_Registers(rc))
	    r -= REGISTER_Last_Rotating_Registers(rc) - 
	            REGISTER_First_Rotating_Registers(rc) + 1;
        }
#else
	r -= fixup.control_loc;  // Control lifetime uses first rotating regs
	if ((int)r < (int)REGISTER_First_Rotating_Registers(rc))
	  r += REGISTER_Last_Rotating_Registers(rc) - 
	    REGISTER_First_Rotating_Registers(rc) + 1;
	else if ((int)r > (int)REGISTER_Last_Rotating_Registers(rc))
	  r -= REGISTER_Last_Rotating_Registers(rc) - 
	    REGISTER_First_Rotating_Registers(rc) + 1;
      }
      else if (rc == ISA_REGISTER_CLASS_integer) {
	if (r >= REGISTER_First_Rotating_Registers(rc) +
	    REGISTER_Number_Stacked_Rotating(rc)) {
	  r -= REGISTER_Number_Stacked_Rotating(rc);
	}
#endif // TARG_IA64
      }
      else if (rc == ISA_REGISTER_CLASS_integer) {
	if (r >= REGISTER_First_Rotating_Registers(rc) +
	    REGISTER_Number_Stacked_Rotating(rc)) {
	  r -= REGISTER_Number_Stacked_Rotating(rc);
	}
      }
      else if (r > REGISTER_Last_Rotating_Registers(rc)) {
	r -= REGISTER_Last_Rotating_Registers(rc) - 
	  REGISTER_First_Rotating_Registers(rc) + 1;
      }

#ifdef TARG_IA64
      Is_True(r <= REGISTER_Last_Rotating_Registers(rc) &&
	      r >= REGISTER_First_Rotating_Registers(rc),
	      ("cannot wrap around twice."));
#else
      Is_True(r <= REGISTER_Last_Rotating_Registers(rc) &&
	      r >= REGISTER_First_Rotating_Registers(rc),
      ("%d (class %d:%d:%d:%d) => %d: cannot wrap around twice.",
       old_r, rc,
       REGISTER_First_Rotating_Registers(rc),
       REGISTER_Last_Rotating_Registers(rc),
       fixup.control_loc,
       r));
#endif
    }
  }
  if (old_r != r) 
    Set_TN_register(tn, r);

  if (trace && old_r != r) {
    fprintf (TFile, "Rotated register %s;", REGISTER_name(rc, old_r));
    fPrint_TN(TFile, " result is %s\n", tn);
  }
#endif // TARG_IA64
  return rtn;
} // Fixup_Rotating_Register_TN


static void 
SWP_Fixup_Rotating_Registers(BB              *head, 
			     BB              *body, 
			     BB              *tail,
			     const SWP_FIXUP &fixup,
			     bool             trace)
{
  CXX_MEM_POOL local_pool("fixup local pool", FALSE);
  TN_SET *fixed_tn = TN_SET_Create_Empty(Last_TN + 1, local_pool());

  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    for (INT j = 0; j < OP_opnds(op); j++) {
      TN *tn = OP_opnd(op, j);
      if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);
	Set_OP_opnd(op,j, new_tn);
	fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
      }
    }
    for (INT k = 0; k < OP_results(op); k++) {
      TN *tn = OP_result(op, k);
      if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);
	Set_OP_result(op, k, new_tn);
	fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
      }
    }
  }
  FOR_ALL_BB_OPs(head, op) {
    if (OP_glue(op)) {
      for (INT k = 0; k < OP_results(op); k++) {
	TN *tn = OP_result(op, k);
	if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	  TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);	
	  Set_OP_result(op, k, new_tn);
	  fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
	}
      }
    }
  }
  FOR_ALL_BB_OPs(tail, op) {
    if (OP_glue(op)) {
      for (INT j = 0; j < OP_opnds(op); j++) {
	TN *tn = OP_opnd(op, j);
	if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	  TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);
	  Set_OP_opnd(op, j, new_tn);
	  fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
	}
      }
    }
  }
} // SWP_Fixup_Rotating_Registers


void 
SWP_Emit(SWP_OP_vector& op_state,
	 SWP_REG_ASSIGNMENT& reg_assign,
	 TN *trip_count_tn,
	 BB *head, BB *body, BB *tail, 
	 bool is_doloop, bool trace)
{
  if (trace) {
    CG_LOOP_Backpatch_Trace(head, NULL);
    CG_LOOP_Backpatch_Trace(tail, NULL);
  }

  // Create a SWP REGION
  RID *r = RID_Create(New_Region_Id(), 0, NULL);
  RID_has_reg_alloc_Set(r);
  RID_level(r) = RL_CG;
  RID_type(r) = RID_TYPE_swp;
  RID_bounds_exist(r) = REGION_BOUND_UNKNOWN;
  RID_has_return(r) = REGION_NO_RETURN;
  RID_num_exits(r) = 1;
  RID_is_glue_code(r) = FALSE;        
  RID *parent = BB_rid(body);
  RID_parent(r) = parent;
  RID_cginfo(r) = NULL; /* ?? this should have a value */
  if ( parent ) RID_Add_kid(r, parent);

  BB_rid(body) = r;
  Set_BB_reg_alloc(body);
  Set_BB_scheduled(body);
  
  // Rearrange OPs in modulo order
  SWP_Reorder_OPs(op_state, reg_assign, body, trace);

  // Generate br.ctop
  OPS prolog_ops = OPS_EMPTY;
  OPS body_ops = OPS_EMPTY;
  OPS epilog_ops = OPS_EMPTY;

  INT32 prolog_epilog_count = op_state.sc;
  if (!is_doloop && op_state.loop_one_more_time)
    prolog_epilog_count--;

  SWP_Loop_Init_Fini(is_doloop, prolog_epilog_count, &prolog_ops, &body_ops, &epilog_ops);

  BB_Append_Ops(head, &prolog_ops);
  BB_Append_Ops(body, &body_ops);  
  BB_Append_Ops(tail, &epilog_ops);

  Set_BB_mod_pred_rotating_registers(head);
  Set_BB_mod_rotating_registers(tail);

  // Rename body, backpatches and
  // generate glue copies for invariants
  SWP_Rename_TNs(op_state, reg_assign, head, tail);

  {
    // Generate SWP ROTATING KERNEL Annotation
    ROTATING_KERNEL_INFO *info = TYPE_PU_ALLOC(ROTATING_KERNEL_INFO);
    bzero(info, sizeof(ROTATING_KERNEL_INFO));
    reg_assign.Update_Annotation(info);
    
	// Regenerate SWP resource statistics
    TI_RES_COUNT *res_counts = TI_RES_COUNT_Alloc(MEM_pu_pool_ptr);
    // Sum resources from each OP's resource usage.
    OP *op;
    for (INT i = 0; i < op_state.size(); i++) {
      OP *op = op_state[i].op;
      if (op && !op_state[i].is_noop)
	TI_RES_COUNT_Add_Op_Resources(res_counts, OP_code(op));
    }

    // Save SWP statistics
    ROTATING_KERNEL_INFO_succeeded(info) = TRUE;
    ROTATING_KERNEL_INFO_ii(info) = op_state.ii;
    ROTATING_KERNEL_INFO_stage_count(info) = op_state.sc;
    ROTATING_KERNEL_INFO_min_ii(info) = op_state.min_ii;    
    ROTATING_KERNEL_INFO_res_min_ii(info) = op_state.res_min_ii;
    ROTATING_KERNEL_INFO_rec_min_ii(info) = op_state.rec_min_ii;
    ROTATING_KERNEL_INFO_sched_len(info) = op_state.sl;
    ROTATING_KERNEL_INFO_min_sched_len(info) = op_state.min_sl;
    ROTATING_KERNEL_INFO_res_counts(info) = res_counts;

    FOR_ALL_BB_OPs(head, op) {
      if (OP_glue(op)) {
	for (INT k = 0; k < OP_results(op); k++) {
	  TN *tn = OP_result(op, k);
	  if (!TN_is_const_reg(tn))
	    ROTATING_KERNEL_INFO_copyin(info).push_back(tn);
	}
      }
    }
    FOR_ALL_BB_OPs(tail, op) {
      if (OP_glue(op)) {
	for (INT j = 0; j < OP_opnds(op); j++) {
	  TN *tn = OP_opnd(op, j);
	  if (!TN_is_const_reg(tn))
	    ROTATING_KERNEL_INFO_copyout(info).push_back(tn);
	}
      }
    }
#ifdef TARG_IA64
    REGISTER_SET all_non_rotating[ISA_REGISTER_CLASS_MAX+1];
    ISA_REGISTER_CLASS i;
    FOR_ALL_ISA_REGISTER_CLASS(i) {
      all_non_rotating[i] = REGISTER_SET_Difference_Range(REGISTER_CLASS_allocatable(i),
                                                          REGISTER_First_Rotating_Registers(i),
                                                          REGISTER_Last_Rotating_Registers(i));
    }
    FOR_ALL_BB_OPs(body, op) {
      for (INT k = 0; k < OP_results(op); k++) {
        TN *tn = OP_result(op, k);
        if (!TN_is_const_reg(tn) && TN_register(tn) != REGISTER_UNDEFINED){
          ISA_REGISTER_CLASS rc = TN_register_class(tn);      
          REGISTER r = TN_register(tn);
          if(r <= REGISTER_MAX //OSP_43
              && REGISTER_SET_MemberP(all_non_rotating[rc],r)){
            ROTATING_KERNEL_INFO_localdef(info).push_back(tn);
          }
        }
      }
    }
#endif
    BB_Add_Annotation(body, ANNOT_ROTATING_KERNEL, (void *)info);
  }

  if (trace) {
    Print_BB(head);
    Print_BB(body);
    Print_BB(tail);
  }

  if (SWP_Options.Enable_BRP) Gen_SWP_Branch_Predict(body, head, tail);
}


// Postpass SWP Fixup 
//  - it is run after all SWP loop has been processed.
//  - called from Perform_Loop_Optimization
//
void SWP_Fixup(SWP_FIXUP& fixup)
{
  BB *prolog = fixup.prolog;
  BB *body = fixup.body;
  BB *epilog = fixup.epilog;

  SWP_Fixup_Rotating_Registers(prolog, body, epilog, fixup, 
			       SWP_REG_ASSIGNMENT::Trace());

#ifdef HAS_ROTATING_REGISTERS
  // Update the requirement of the configurable int rotating registers
  ANNOTATION *annot = ANNOT_Get(BB_annotations(body), ANNOT_ROTATING_KERNEL);
  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);
  ISA_REGISTER_CLASS rc = ISA_REGISTER_CLASS_integer;
  const REGISTER first_rotating_reg =  REGISTER_First_Rotating_Registers(rc);
  const REGISTER num_rotating_reg = REGISTER_Number_Stacked_Rotating(rc);

  BOOL trace = Get_Trace(TP_SWPIPE, 2);

  for (INT i = 0; i < num_rotating_reg; i++) {
    REGISTER reg = i + first_rotating_reg;
    ROTATING_KERNEL_INFO_live_in(info)[rc] =
      REGISTER_SET_Union1(ROTATING_KERNEL_INFO_live_in(info)[rc], reg);
    ROTATING_KERNEL_INFO_kill(info)[rc] = 
      REGISTER_SET_Union1(ROTATING_KERNEL_INFO_kill(info)[rc], reg);
  }

  if (trace) {
    fprintf(TFile, "Reminder: REGISTER SET number differs from real register by 1.\n");
    FOR_ALL_ISA_REGISTER_CLASS(rc) {
      fprintf(TFile, "SWP annotation: register class %d", rc);
      fprintf(TFile, "\nlivein: ");
      REGISTER_SET_Print(ROTATING_KERNEL_INFO_live_in(info)[rc], TFile);
      fprintf(TFile, "\nkill:   ");
      REGISTER_SET_Print(ROTATING_KERNEL_INFO_kill(info)[rc], TFile);
      fprintf(TFile,"\n");
    }
  }

#ifdef Is_True_On
  // Verify that p63 is not modified in the last cycle
  {
    OP *op;
    INT ii = ROTATING_KERNEL_INFO_ii(info);
    OP *br = BB_branch_op(body);
    INT br_cycle = OP_scycle(br) % ii;
    FOR_ALL_BB_OPs(body, op) {
      if (OP_scycle(op) % ii == br_cycle) {
	for (INT k = 0; k < OP_results(op); k++) {
	  TN *tn = OP_result(op, k);
	  if (TN_is_register(tn)) {
	    if (TN_register_class(tn) == ISA_REGISTER_CLASS_predicate &&
		TN_register(tn) == 64) {
	      Is_True(FALSE, ("SWP_Fixup: p63 is modified."));
	    }
	  }
	}
      }
    }
  }
#endif

#endif
}

