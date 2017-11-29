/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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

#define USE_STANDARD_TYPES
#include <vector>
#include <list>
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "matrix.h"
#include "mempool_allocator.h"
#include "cg.h"
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "cg_swp_target.h"
#include "cgprep.h"
#include "glob.h"    // for Cur_PU_Name
#include "op.h"
#include "cg_loop.h"
#include "cgtarget.h"
#include "ti_si.h"
#include "cg_grouping.h"  // Defines INT32_VECTOR and CG_GROUPING
#include "be_util.h"      // for current_pu_count
#include "cggrp_microsched.h"

static const INT SWP_INVALID_OP_IDX = -1024;

static bool Trace_Swp_Bundling = false;

typedef mempool_allocator<bool>          BOOL_MEMALLOC;
typedef MATRIX<INT32, INT32_MEMALLOC>    INT32_MATRIX;
typedef MATRIX<bool, BOOL_MEMALLOC>      BOOL_MATRIX;
typedef std::list<INT32, INT32_MEMALLOC> INT32_LIST;


// A non-strict weak partial ordering on operations based on dependences,
// hints from the scheduler, heuristics based on the form of bundle 
// templates, and resource constraints.
//
class SLOT_ORDER_CMP
{
private:

  const SWP_OP_vector *_op_state;
  const CG_GROUPING   *_grouping;
  const BOOL_MATRIX   *_dep_order;
  const INT32_VECTOR  *_resource_slack;

  bool _is_loop_variant(TN *tn) const
  {
    return (TN_is_register(tn) && 
	    !TN_is_dedicated(tn) && 
	    !TN_SET_MemberP(_op_state->tn_invariants, tn));
  }
  
  bool _loop_variant_lhs(OP *op) const
  {
    bool is_variant = (OP_results(op) > 0);
    for (INT k = 0; k < OP_results(op); k++)
      is_variant = (is_variant && _is_loop_variant(OP_result(op, k)));
    return is_variant;
  }

  bool _schedule_early(INT i) const 
  {
    return (*_op_state)[i].direction == SWP_TOP_DOWN;
  }

  bool _schedule_late(INT i) const 
  {
    return (*_op_state)[i].direction == SWP_BOTTOM_UP;
  }

public:

  SLOT_ORDER_CMP(const SWP_OP_vector &op_state,
		 const CG_GROUPING   &grouping,
		 const BOOL_MATRIX   &dep_order,
		 const INT32_VECTOR  &resource_slack): 
    _op_state(&op_state), 
    _grouping(&grouping), 
    _dep_order(&dep_order), 
    _resource_slack(&resource_slack)
  {}
  
  bool operator ()(INT32 i, INT32 j) const
  {
    bool cmp = (*_dep_order)(i, j);

    if (!cmp)
    {
      // The order is not enforced by a dependency, so use some heuristics.
      //
      OP * const op1 = (*_op_state)[i].op;
      OP * const op2 = (*_op_state)[j].op;
      const TOP  top1 = OP_code(op1);
      const TOP  top2 = OP_code(op2);

      if (_grouping->is_branch(top1) || _grouping->is_branch(top2))
      {
	// Branches always break a cycle, so order them as late as possible.
	//
	cmp = _grouping->is_branch(top2) && !_grouping->is_branch(top1);
      }
      else if ((OP_l_group(op2) && !OP_l_group(op1))               ||
	       (OP_f_group(op1) && !OP_f_group(op2))               ||
	       ((*_resource_slack)[i] < (*_resource_slack)[j])     || 
#ifdef SWP_BUNDLE_APPLY_LIFETIME_HEURISTICS
	       // apply scheduling early/late heuristics might 
	       // cause inefficient bundling.
	       (_schedule_early(i) && !_schedule_early(j))         ||
	       (_schedule_late(j) && !_schedule_late(i))           ||
	       (_loop_variant_lhs(op2) && !_loop_variant_lhs(op1)) ||
#endif
	       (_grouping->bundling_order(top1) < 
		_grouping->bundling_order(top2)))

      {
	// These heuristics are based on OP attributes (must op be first or
	// last in a group), resource_slack in options of resource use,
	// scheduler heuristics, a desire to schedule stores to loop-variants
	// late in a group (thus making lifetimes smaller for the register 
	// allocator), and the order of ops most commonly seen in the 
	// bundle templates that are available.
	//
	cmp = true;
      }
    }
    return cmp;
  }
}; // SLOT_ORDER_CMP


inline TI_BUNDLE *
TI_BUNDLE_Create(MEM_POOL *pool)
{
  TI_BUNDLE *b = TYPE_MEM_POOL_ALLOC (TI_BUNDLE, pool);
  TI_BUNDLE_bundle_info(b) = TYPE_MEM_POOL_ALLOC(ISA_BUNDLE_INFO, pool);
  TI_BUNDLE_Clear(b);
  return b;
} // TI_BUNDLE_Create


inline void
SWP_Reserve_Bundle_Slot(TI_BUNDLE             *bundle, 
			INT32                  next_slotpos,
			ISA_EXEC_UNIT_PROPERTY op_exec_unit,
			TOP                    top)
{
  TI_BUNDLE_Reserve_Slot(bundle, next_slotpos, op_exec_unit);
  if (Trace_Swp_Bundling)
    fprintf(TFile, "Bundle slot %d = %s\n", next_slotpos, TOP_Name(top));
    
} // SWP_Reserve_Bundle_Slot

static void
SWP_Init_Samecycle_Dep_Order(SWP_OP_vector &op_state, BOOL_MATRIX &dep_order)
{
  // dep_order is initialized to false for all elements.  This routine
  // sets it to true where there is an ordering constraints based on a
  // dependence within the same cycle (cycle is determined by swp_sched).
  //
  for (INT i = 0; i < op_state.size(); i++)
  {
    OP * const op = op_state[i].op;

    if (op != NULL)
    {
      for (ARC_LIST *al = OP_succs(op) ; al != NULL; al = ARC_LIST_rest(al) )
      {
	ARC * const arc = ARC_LIST_first(al);
	OP  * const op2 = ARC_succ(arc);
	const INT   i2 = SWP_index(op2);

	if (op_state[i].cycle == op_state[i2].cycle && ARC_omega(arc) == 0)
	{
	  // Most dependencies should never occur within the same cycle,
	  // but we here maintain all dependencies, except read-read of
	  // non-volatile data.
	  //
	  if (ARC_kind(arc) != CG_DEP_MEMREAD) {
	    dep_order(i, i2) = true;
	    if (Trace_Swp_Bundling) 
	      fprintf(TFile, "  dep_order(%d, %d)\n", i, i2);
	  } 
	} // If in same modulo_cycle
      } // For each arc
    } // If not NULL
  } // For each op
} // SWP_Init_Samecycle_Dep_Order

static void
SWP_Verify_Samecycle_Dep_Order(SWP_OP_vector &op_state)
{
  for (INT i = 0; i < op_state.size(); i++) {
    OP * const op = op_state[i].op;
    if (op != NULL && !op_state[i].is_noop) {
      for (ARC_LIST *al = OP_succs(op) ; al != NULL; al = ARC_LIST_rest(al) ) {
	ARC * const arc = ARC_LIST_first(al);
	OP  * const op2 = ARC_succ(arc);
	const INT   i2 = SWP_index(op2);

	// #787448 REGANTI (i.e read before write) dependences are OK within 
	// a bundle. 
	if (ARC_is_reg(arc) && ARC_is_anti(arc)) continue;

	if (ARC_omega(arc) == 0) {
	  FmtAssert(i == i2 ||
		    op_state[i].cycle != op_state[i2].cycle ||
		    op_state[i].slot < op_state[i2].slot,
		    ("OP%d cycle=%d slot=%d violates dependences with OP%d cycle=%d slot=%d",
		     i, op_state[i].cycle, op_state[i].slot,  
		     i2, op_state[i2].cycle, op_state[i2].slot));
	}
	
      } // For each arc
    } // If not NULL
  } // For each op
} // SWP_Init_Samecycle_Dep_Order


inline INT32
SWP_Num_Remaining_Slots(TI_BUNDLE *bundle)
{
  INT32 i = TI_BUNDLE_slot_count(bundle) - 1;
  while (i >= 0 && !TI_BUNDLE_slot_filled(bundle, i)) --i;
  return (TI_BUNDLE_slot_count(bundle) - (i + 1));
} // SWP_Num_Remaining_Slots


static INT32
SWP_Min_Slot_Count(SWP_OP_vector         &op_state, 
		   INT32_MATRIX::iterator opset,
		   INT32_MATRIX::iterator opset_end,
		   const CG_GROUPING     &grouping)
{
  // Calculate the minimum number of slots necessary for the opset.
  //
  INT32 count = 0;
  for (INT32_MATRIX::iterator it = opset; it < opset_end; ++it)
  {
    if (op_state[*it].op != NULL)
      count += grouping.num_slots(OP_code(op_state[*it].op));
  }
  return count;
} // SWP_Min_Slot_Count


static std::pair<bool, ISA_EXEC_UNIT_PROPERTY>
SWP_Noop_Property(CG_GROUPING &grouping, 
		  TI_BUNDLE   *bundle,
		  INT32        slot_pos,
		  bool         end_of_group)
{
  INT                    preference;
  bool                   found_prop = false;
  ISA_EXEC_UNIT_PROPERTY prop;

  
  // We first try to chose a unit from one of our preferences for the
  // slot, such that we do not overflow our resources.
  //
  for (preference = 0;
       !found_prop && 
	 preference < grouping.no_of_preferred_slot_kinds(slot_pos);
       ++preference)
  {
    prop = grouping.get_preferred_slot_kind(slot_pos, preference);
    if (grouping.avail_resource_units(prop) > 0)
      found_prop = TI_BUNDLE_Slot_Available(bundle, prop, slot_pos);
  }

  // We next try to chose a unit from one of our preferences for the
  // slot, without consideration for resources.
  //
  for (preference = 0;
       !found_prop && 
	 preference < grouping.no_of_preferred_slot_kinds(slot_pos);
       ++preference)
  {
    prop = grouping.get_preferred_slot_kind(slot_pos, preference);
    found_prop = TI_BUNDLE_Slot_Available(bundle, prop, slot_pos);
  }

  return std::pair<bool, ISA_EXEC_UNIT_PROPERTY>(found_prop, prop);
} // SWP_Noop_Property


static INT32
SWP_Append_Noop(SWP_OP_vector &op_state, 
		CG_GROUPING   &grouping, 
		INT32          append_after_op_idx, 
		INT32          next_op_idx, 
		TI_BUNDLE     *bundle,
		INT32          slot_pos)
{
  // Insert a noop and return an idx to it in the op_state.  We assume
  // the bundle has the TI_BUNDLE_Return_Template() set to accomodate
  // any slot filled (at least one slot should be filled).
  //
  Is_True(append_after_op_idx >= 0, 
	  ("Must have a previous op for SWP_Append_Noop"));
  
  // Determine the best ISA_EXEC_UNIT_PROPERTY for the noop, based on the
  // next op we intend to place and our preferences for the slot position
  // in question.
  //
  bool                   found_prop = false;
  ISA_EXEC_UNIT_PROPERTY slot_prop;

  if (next_op_idx != SWP_INVALID_OP_IDX)
  {
    // If at all possible, choose the noop such that we can fit the next
    // op into a subsequent slot.  First call CGTARG_Bundle_Slot_Available()
    // to get the secondary effect of a suitable TI_BUNDLE_Return_Template().
    //
    for (INT preference = 0;
	 !found_prop &&
	   preference < grouping.no_of_preferred_slot_kinds(slot_pos);
	 ++preference)
      {
	slot_prop = grouping.get_preferred_slot_kind(slot_pos, preference);
	if (grouping.avail_resource_units(slot_prop) > 0) {
	  if (slot_pos+1 < TI_BUNDLE_slot_count(bundle)) {
	    ISA_EXEC_UNIT_PROPERTY tmp_slot_prop;
	    TI_BUNDLE_Reserve_Slot(bundle, slot_pos, slot_prop);
	    found_prop = 
	      CGTARG_Bundle_Slot_Available(bundle, 
					   op_state[next_op_idx].op,
					   slot_pos+1,
					   &tmp_slot_prop, // slot prop of next op
					   FALSE,
					   &grouping);
	  }
	}
      }
  }

  if (!found_prop)
  {
    // No suitable noop property (i.e. bundle template) was found to suit
    // a next operation (next_op_idx), so now we try to choose one based on
    // resource usage.
    //
    const std::pair<bool, ISA_EXEC_UNIT_PROPERTY> prop = 
      SWP_Noop_Property(grouping, bundle, slot_pos,
			next_op_idx == SWP_INVALID_OP_IDX && slot_pos > 0);

    found_prop = prop.first;
    slot_prop = prop.second;
  }

  Is_True(found_prop, 
	  ("Cannot find suitable ISA_EXEC_UNIT_PROPERTY for slot %d"
	   " in %s bundle in SWP_Append_Noop", 
	   slot_pos,
	   (ISA_EXEC_Name(TI_BUNDLE_pack_code(bundle)) != NULL? 
	    ISA_EXEC_Name(TI_BUNDLE_pack_code(bundle)):  "unknown")));

  // Create the noop.
  //
  const TOP ntop     = CGTARG_Noop_Top(slot_prop);
  OP       *noop     = Mk_OP(ntop, True_TN, Gen_Literal_TN(0, 4));
  SWP_OP    swp_noop = op_state[append_after_op_idx]; // Similar attributes

  swp_noop.op = noop;
  swp_noop.is_noop = true;
  swp_noop.slot = grouping.absolute_slot_no(slot_pos);

  // Place in the correct BB, but in arbitrary posistion.  The ops
  // will be emitted in correct order at swp emit time.
  //
  op_state.push_back(swp_noop);

  BB_Insert_Op_After(OP_bb(op_state[append_after_op_idx].op),
		     op_state[append_after_op_idx].op,
		     noop);
  OP_scycle(noop) = -1;   // Will be set at swp emit time
  
  SWP_Reserve_Bundle_Slot(bundle, slot_pos, slot_prop, ntop);
  grouping.reserve_resource(slot_prop);
  Set_OP_bundled(noop);
  if (ntop == TOP_nop_m)
    Set_OP_m_unit(noop);

  // The caller may have decided that a group should end at this slot.
  //
  if (TI_BUNDLE_stop_bit(bundle, slot_pos))
    Set_OP_end_group(noop);

  return op_state.size() - 1;
} // SWP_Append_Noop


static void
SWP_Set_Stop_At(CG_GROUPING &grouping, 
		TI_BUNDLE   *bundle,
		OP          *op,
		INT32        slotpos)
{
  const bool start_next_group_in_this_bundle = 
    (slotpos < (TI_BUNDLE_slot_count(bundle) - 1));

  TI_BUNDLE_Reserve_Stop_Bit(bundle, slotpos);
  Set_OP_end_group(op);
  grouping.start_new_group(start_next_group_in_this_bundle);

  if (Trace_Swp_Bundling)
    fprintf(TFile, "<Stop-bit>\n");
}


static void
SWP_Fillup_Bundle(SWP_OP_vector &op_state, 
		  CG_GROUPING   &grouping, 
		  INT32         &prev_op_idx, 
		  TI_BUNDLE     *bundle,
		  INT32          from_slotpos)
{
  Is_True(from_slotpos > 0 && TI_BUNDLE_slot_filled(bundle, from_slotpos - 1),
	  ("Can only call SWP_Fillup_Bundle with partially filled bundle!"));

  // Fill in remaining slots of the bundle with noops, and move the
  // stop bit to the end.
  //
  Reset_OP_end_group(op_state[prev_op_idx].op);

  if (Trace_Swp_Bundling && TI_BUNDLE_stop_bit(bundle, from_slotpos - 1))
    fprintf(TFile, "... moving <Stop-bit> to end of bundle\n");
  TI_BUNDLE_Unreserve_Stop_Bit(bundle, from_slotpos - 1);

  for (INT32 i = from_slotpos; i < TI_BUNDLE_slot_count(bundle); ++i)
  {
    prev_op_idx = SWP_Append_Noop(op_state, 
				  grouping, 
				  prev_op_idx, SWP_INVALID_OP_IDX, bundle, i);
  }
  SWP_Set_Stop_At(grouping, bundle, op_state[prev_op_idx].op,
		  TI_BUNDLE_slot_count(bundle) - 1);

  // TODO: recalculate grouping resources!
} // SWP_Fillup_Bundle


static INT32
SWP_Insert_Stop_Bit(SWP_OP_vector &op_state, 
		    CG_GROUPING   &grouping, 
		    INT32         &op_idx, 
		    TI_BUNDLE     *bundle,
		    INT32          slotpos)
{
  // Return the slot following the one where the stop-bit was set (we may
  // have to introduce noops to find a legal place for a stop bit.  Also
  // updates op_idx to the last one inserted.
  //
  INT32 slot = slotpos;

  Is_True(TI_BUNDLE_slot_filled(bundle, slot),
	  ("Expected filled slot in SWP_Insert_Stop_Bit"));

  if (TI_BUNDLE_stop_bit(bundle, slot))
  {
    // we are done, stop bit is already set!
  }
  else if (TI_BUNDLE_Stop_Bit_Present(bundle))
  {
    SWP_Fillup_Bundle(op_state, grouping, op_idx, bundle, slot + 1);
    slot = TI_BUNDLE_slot_count(bundle) - 1;
  }
  else
  {
    while(!CGTARG_Bundle_Stop_Bit_Available(bundle, slot))
    {
      ++slot;
      op_idx = SWP_Append_Noop(op_state, 
			       grouping, 
			       op_idx, SWP_INVALID_OP_IDX, bundle, slot);
    }

    Is_True (slot < TI_BUNDLE_slot_count(bundle), 
	     ("Could not find position for stop bit in SWP_Insert_Stop_Bit"));

    SWP_Set_Stop_At(grouping, bundle, op_state[op_idx].op, slot);

    // TODO: recalculate grouping resources!
  }
  return slot + 1;
} // SWP_Insert_Stop_Bit


static bool
SWP_Bundle_First_In_Group(SWP_OP_vector &op_state, 
			  CG_GROUPING   &grouping, 
			  TI_BUNDLE     *bundle,
			  INT32          prev_op_idx,
			  INT32          op_idx,
			  INT32          at_slotpos)
{
  // See of the given op must be scheduled first on a group, and if so and
  // it will fit into the slot-position indicated, then set the stop bit
  // as appropriate and place the op in the given position.
  //
  bool       ok = false;
  OP * const op = op_state[op_idx].op;
 
  if (OP_f_group(op))
  {
    OP * const             prev_op = op_state[prev_op_idx].op;
    ISA_EXEC_UNIT_PROPERTY op_exec_unit;

    if (at_slotpos == 0)
    {
      // See of the op can indeed be inserted at this first position.
      //
      ok = CGTARG_Bundle_Slot_Available(bundle, op, at_slotpos, 
					&op_exec_unit, FALSE/*stop_req*/,
					&grouping);
      
      // Determine whether or not we need a stop bit at the end of the 
      // previous bundle (always legal).
      //
      if (!grouping.in_first_bundle_of_group())
      {
	if (Trace_Swp_Bundling)
	  fprintf(TFile, "<Stop-bit>\n");
	Set_OP_end_group(op_state[prev_op_idx].op);
	grouping.start_new_group(true/*in_current_bundle*/);
      }
    }
    else
    {
      // See if the op can be inserted at the given position, given that
      // it must be preceeded by a stop bit.
      //
      ok = CGTARG_Bundle_Slot_Available(bundle, op, at_slotpos, 
					&op_exec_unit, TRUE/*stop_req*/,
					&grouping);
	
      if (ok)
	SWP_Set_Stop_At(grouping, bundle, 
			op_state[prev_op_idx].op, at_slotpos - 1);
    }

    if (ok)
    {
      Set_OP_bundled(op);
      op_state[op_idx].slot = grouping.absolute_slot_no(at_slotpos);
      SWP_Reserve_Bundle_Slot(bundle, at_slotpos, op_exec_unit, OP_code(op));
    }
  }
  return ok;
} // SWP_Bundle_First_In_Group


static bool
SWP_Bundle_Next_In_Group(SWP_OP_vector         &op_state, 
			 CG_GROUPING           &grouping, 
			 TI_BUNDLE             *bundle,
			 INT32                  op_idx,
			 INT32                  next_slotpos,
			 ISA_EXEC_UNIT_PROPERTY preferred_exec_unit)
{
  // See of the given op can be scheduled into the next slot position
  // in the bundle.  It must use the preferred exec unit.
  // 
  //
  OP * const             op = op_state[op_idx].op;
  ISA_EXEC_UNIT_PROPERTY op_exec_unit = ISA_EXEC_Unit_Prop(OP_code(op));
  
  // FOR DEBUGGING:
  // if (grouping.is_branch(OP_code(op)) && preferred_exec_unit & op_exec_unit)
  // {
  //   fprintf(stderr, "Got there!\n");
  // }
  
  const bool ok = ((preferred_exec_unit & op_exec_unit)                     &&
		   (!OP_l_group(op) || 
		    CGTARG_Bundle_Stop_Bit_Available(bundle, next_slotpos)) &&
		   CGTARG_Bundle_Slot_Available(bundle, op, next_slotpos, 
						&op_exec_unit,
						FALSE/*stop_req*/,
						&grouping)                  &&
		   op_exec_unit == preferred_exec_unit);
  if (ok)
  {
    Set_OP_bundled(op);
    op_state[op_idx].slot = grouping.absolute_slot_no(next_slotpos);
    SWP_Reserve_Bundle_Slot(bundle, next_slotpos, op_exec_unit, OP_code(op));
  }

  // Set the stop bit if this op must end the group.
  //
  if (OP_l_group(op))
    SWP_Set_Stop_At(grouping, bundle, op, next_slotpos);

  return ok;
} // SWP_Bundle_Next_In_Group


static bool
SWP_Violates_Dep_Order(BOOL_MATRIX               &dep_order, 
		       INT32_LIST::const_iterator first, 
		       INT32_LIST::const_iterator current)
{
  bool violation = false;
  for (INT32_LIST::const_iterator it = first; 
       it != current && !violation;
       ++it)
  {
    if (dep_order(*it, *current))
      violation = true;
  }
  return violation;
} // SWP_Violates_Dep_Order


static void
SWP_Pack_A_Bundle(SWP_OP_vector &op_state,
		  CG_GROUPING   &grouping,
		  BOOL_MATRIX   &dep_order, 
		  INT32         &prev_op_idx, 
		  TI_BUNDLE     *bundle,
		  INT32_LIST    &ops_list)
{
  // Clears the given bundle, and packs as many ops as possible from 
  // the given ops_list into the bundle.  Once the ops_list becomes empty,
  // a stop-bit will be set in the bundle at the earliest possible slot.
  //
  INT slot = 0;

  grouping.start_new_bundle(bundle);

  for (slot = 0; 
       slot < TI_BUNDLE_slot_count(bundle) && !ops_list.empty();
       ++slot)
  {
    INT32_LIST::iterator placed_op = ops_list.end();

    // Try to place ops in the order they occur in the ops_list,
    // following the preferred exec_unit kind for the current slot.
    //
    for (INT preference = 0;
	 (placed_op == ops_list.end() && 
	  preference < grouping.no_of_preferred_slot_kinds(slot));
	 ++preference)
    {
      ISA_EXEC_UNIT_PROPERTY slot_kind = 
	grouping.get_preferred_slot_kind(slot, preference);

      // Find the first suitable op for this slot position and preferred
      // execution unit.
      //
      for (INT32_LIST::iterator it = ops_list.begin(); 
	   placed_op == ops_list.end() && it != ops_list.end();
	   ++it)
      {
	const TOP top = OP_code(op_state[*it].op);
	const INT remaining_slots = TI_BUNDLE_slot_count(bundle) - slot;

	// If there are enough slots left in this bundle for the op, we
	// do not violate any dependencies w.r.t. ops not yet bundled, and
	// we are not dealing with a branch other than on the last bundle.
	//
	if ((!grouping.is_branch(top) || remaining_slots >= ops_list.size()) &&
	    !SWP_Violates_Dep_Order(dep_order, ops_list.begin(), it)         &&
	    (remaining_slots >= grouping.num_slots(top)))
	{
	  const bool first_in_group = SWP_Bundle_First_In_Group(op_state,
								grouping,
								bundle,
								prev_op_idx,
								*it,
								slot);
	  const bool next_in_group = (!first_in_group &&
				      SWP_Bundle_Next_In_Group(op_state,
							       grouping,
							       bundle,
							       *it,
							       slot,
							       slot_kind));
	  if (first_in_group || next_in_group)
	    placed_op = it;
	}
      } // For each op in the ordered list
    } // For each EXEC_UNIT to be considered for this slot

    // If we found an op for this slot, then erase it from the list of ops,
    // update prev_op_idx, and for ops that have a choice of resources
    // reserve the resource chosen. 
    //
    if (placed_op != ops_list.end())
    {
      const TOP top = OP_code(op_state[*placed_op].op);

      prev_op_idx = *placed_op;
      ops_list.erase(placed_op);
      if (grouping.has_resource_choice(top))
	grouping.reserve_resource(TI_BUNDLE_exec_property(bundle, slot));
    }
    else // Could not find an op for this slot position
    {
      const INT insert_noop_after = 
	(prev_op_idx < 0? ops_list.front() : prev_op_idx);

      prev_op_idx =
	SWP_Append_Noop(op_state, 
			grouping, 
			insert_noop_after, ops_list.front(), bundle, slot);
    }
  } // For each slot, while more ops to be placed and more slots in bundle

  // Set stop bit if we are done with this group
  //
  if (ops_list.empty())
    SWP_Insert_Stop_Bit(op_state, grouping, prev_op_idx, bundle, slot - 1);

} // SWP_Pack_A_Bundle

// The function object used in SWP_Pack_Into_New_Bundles returns TRUE 
// when the schedule slot of OP[i] is smaller than that of OP[j].  
struct Order_Bundled_OPs_By_slot {
  const SWP_OP_vector& state;
  bool operator()(INT i, INT j) { 
    return state[i].slot < state[j].slot; 
  }
  Order_Bundled_OPs_By_slot(const SWP_OP_vector& op_state):state(op_state) {}
};

// use this stucture to backup the <prev> & <next> of each op
struct OP_PARTIAL_BACKUP{
   OP* prev;
   OP* next;
  BOOL valid;
};

void SWP_Delete_Noop(SWP_OP_vector &op_state, 
                 std::vector<INT>    &exist_noops)
{

  for (INT i=0; i<exist_noops.size(); i++){
     INT nop_idx = exist_noops[i];
     //OSP_22
     BB_Remove_Op(op_state[nop_idx].op->bb, op_state[nop_idx].op);
     op_state[nop_idx].op = NULL;
     op_state[nop_idx].is_noop = FALSE;
  }
}

void SWP_Update_OP_slot(SWP_OP_vector &op_state, 
                              OPS          *ops, 
                      const INT  slot_yardstick,
                     INT32         &prev_op_idx)
{
  OP* cur_op;
  INT i = 0;
  FOR_ALL_OPS_OPs(ops, cur_op) {
    if (!OP_noop(cur_op))  // the noop maked in CGGRP_Bundle_SWP_OPS does not 
                          // belong to op_state
    {
      INT idx = SWP_index(cur_op); 
      op_state[idx].slot = slot_yardstick - (OPS_length(ops)-1-i);
      OP_scycle(op_state[idx].op) = 0; // Will be set at swp emit time
      Set_OP_bundled(cur_op);
      // op_state[bundled_ops[i]].slot = slot_yardstick - (bundled_ops.size()-1-i);
    }
    else {

      SWP_OP    swp_noop = op_state[prev_op_idx]; // Similar attributes

      swp_noop.op = cur_op;
      swp_noop.is_noop = true;
      swp_noop.slot = slot_yardstick - (OPS_length(ops)-1-i);
     
      op_state.push_back(swp_noop);
      OP_scycle(cur_op) = -1;   // Will be set at swp emit time
  
      Set_OP_bundled(cur_op);
    }
    i++;
  }
  
  // Update the prev_op_idx
  OP* last_op = OPS_last(ops);
  if (!OP_noop(last_op)) 
     prev_op_idx = SWP_index(last_op);
  else 
     prev_op_idx = op_state.size() - 1 ;   // last op must be the newly pushed noop

}


BOOL SWP_Slot_Helper(SWP_OP_vector &op_state,
			  CG_GROUPING   &grouping, 
			  BOOL_MATRIX   &dep_order,
			  INT32         &prev_op_idx, 
			  TI_BUNDLE     *bundle,
			  INT32_LIST    &ops_list) 
// This function try to insert the split ops(ops_list) into current 
// cycle(op_state[prev_op_idx].modulo_cycle). If success, ops_list will 
// become empty and all swp_op's <slot> will be modified according to
// the new order; if fail, ops_list will be restore to the original value as
// nothing has ever happened.
{

  std::vector<INT> bundled_ops, exist_noops;
  INT slot_yardstick = op_state[prev_op_idx].slot;
  INT32_LIST backup_ops_list = ops_list;

  BOOL int_ld_exist=FALSE;
  BOOL fload_ld_exist=FALSE;

  // Initialize OP_Partial_Backup which is used to back up the <prev> and <next> 
  // of SPLIT op's 

  std::vector<OP_PARTIAL_BACKUP>  OP_Partial_Backup(op_state.size());
  for (INT i=0; i<OP_Partial_Backup.size(); i++) {
      OP_Partial_Backup[i].prev=OP_Partial_Backup[i].next=NULL;
      OP_Partial_Backup[i].valid=FALSE;
  }
  for (INT32_LIST::iterator it = ops_list.begin(); 
		     it != ops_list.end();
		     ++it) {

      OP_Partial_Backup[*it].prev = OP_prev(op_state[*it].op);
      OP_Partial_Backup[*it].next = OP_next(op_state[*it].op);
      OP_Partial_Backup[*it].valid =TRUE;

      // Initialize the op's <scycle> , used in CYCLE_STATE::Add_OP() 
      // when _cyclic is TRUE 
      OP *op = op_state[*it].op;
      OP_scycle(op) = op_state[*it].cycle;

      // take note if integer load and float load exist simultaneously.    
      if (OP_Is_Float_Mem(op)) fload_ld_exist=TRUE;
      if (OP_load(op) && !OP_Is_Float_Mem(op)) int_ld_exist=TRUE;


  }

  // get and sort bundled-ops in current modulo_cycle;
  // Backup <prev> and <next> of each op simultaneously.

  INT low = op_state[prev_op_idx].slot - ISA_MAX_SLOTS * ISA_MAX_ISSUE_BUNDLES + 1 ;
  INT high = op_state[prev_op_idx].slot ; 

  for (INT i = 0; i < op_state.size(); i++) {
    if ( low <= op_state[i].slot && op_state[i].slot <= high && 
        op_state[i].op!=NULL &&
        OP_bundled(op_state[i].op) ) {

      // push back the bundled ops in current cycle
      bundled_ops.push_back(i);

      // Backup each op's <prev> and <next> into OP_Partial_Backup.

      OP_Partial_Backup[i].prev = OP_prev(op_state[i].op);
      OP_Partial_Backup[i].next = OP_next(op_state[i].op);
      OP_Partial_Backup[i].valid = TRUE;
    }
  }

  // sort the op in bundled_ops by their slots
  std::sort(bundled_ops.begin(), bundled_ops.end(), Order_Bundled_OPs_By_slot(op_state));
 

  // take note where there is a noop in bundled_ops.
  for (INT i = 0; i < bundled_ops.size(); i++) {
    INT idx = bundled_ops[i]; 
     // take town the noops
    if (op_state[idx].is_noop)
        exist_noops.push_back(idx);

    // take note if integer load and float load exist simultaneously.
    OP* op = op_state[idx].op;
    if (OP_Is_Float_Mem(op)) fload_ld_exist=TRUE;
    if (OP_load(op) && !OP_Is_Float_Mem(op)) int_ld_exist=TRUE;

  }

  // we do not deal with the case where integer load and float load exist simultaneously.
  if (int_ld_exist && fload_ld_exist) {
    return FALSE;
  }

  // Initialize ops
  OPS ops = OPS_EMPTY;
  for (INT i = 0; i < bundled_ops.size(); i++) {
  OP *op = op_state[bundled_ops[i]].op;
  OP_scycle(op) = op_state[bundled_ops[i]].cycle;
  OPS_Append_Op(&ops, op);
  }

  // try to fix the split ops, if success, ops_list will become empty;
  // else ops_list will be restore to the original value
  BOOL abandoned=FALSE;
  INT split_op_idx;  // split_op is the index of SPLIT OP in op_state
  OP* op;
  while (!ops_list.empty() && abandoned==FALSE ) { 
        split_op_idx = ops_list.front();
        op = op_state[split_op_idx].op;

      if (CGGRP_Bundle_OPS(&ops, op, 0, TRUE)==FALSE) {
        abandoned=TRUE; 
        break;
      }
      else { // success, op has already appended into ops, go on fixing the next split op
        ops_list.erase(ops_list.begin());  
        
        // reset op'bb because it will insert some noop instructions;
        OP *tmp; 
        if (OP_bb(op))
	  FOR_ALL_OPS_OPs(&ops, tmp)
	    tmp->bb = OP_bb(op);
      }
  }

  // If at least one split op abandoned, this means fixing failure, restore op_list as 
  // nothing has ever happened.
  if (abandoned) {
 
    // Restore ops_list
    ops_list = backup_ops_list; 

    // Restore the <prev> & <next> of each op
    for (INT i = 0; i < OP_Partial_Backup.size(); i++) { 
        if (OP_Partial_Backup[i].valid) {
          op_state[i].op->prev = OP_Partial_Backup[i].prev;
          op_state[i].op->next = OP_Partial_Backup[i].next;
        }
    }

    return FALSE;
  }


  // Only when all split ops were inserted, i.e. fixing success , should we update op_state
  if (ops_list.empty()) {

    SWP_Update_OP_slot(op_state, &ops, slot_yardstick, prev_op_idx);


    // Restore the <prev> & <next> of each op
    for (INT i = 0; i < OP_Partial_Backup.size(); i++) { 
        if (OP_Partial_Backup[i].valid) {

          op_state[i].op->prev = OP_Partial_Backup[i].prev;
          op_state[i].op->next = OP_Partial_Backup[i].next;
        }
    }

    //OSP_23
    //New noop generated in SWP_Update_OP_slot should be inserted into BB
    if(op_state.size() > OP_Partial_Backup.size()) {
	  OP* new_noop = op_state[op_state.size()-1].op;
	  BB_Insert_Op_After(OP_bb(new_noop), new_noop->prev, new_noop);
    }

    // invalidate replaced_noops in op_state
    SWP_Delete_Noop(op_state, exist_noops);

    return TRUE;
  }


  return FALSE;
}



static void
SWP_Pack_Into_New_Bundles(SWP_OP_vector &op_state,
			  CG_GROUPING   &grouping, 
			  BOOL_MATRIX   &dep_order,
			  INT32         &prev_op_idx, 
			  TI_BUNDLE     *bundle,
			  INT32_LIST    &ops_list)
{
  // Pack the sequence of ops into a sequence of bundles.  Insert explicit
  // stop bits to indicate grouping of ops into processor cycles.
  //
  while (!ops_list.empty())
  {
    SWP_Pack_A_Bundle(op_state, grouping, dep_order, 
		      prev_op_idx, bundle, ops_list);

    // The following should rarily happen, since when we are done with the
    // ops_list, we would normally already have inserted a stop-bit into
    // the bundle.  However, some schedules may span more bundles than can
    // be scheduled in one group or cycle.
    //
    if (!ops_list.empty() && grouping.new_bundle_will_break_cycle())
    {
      DevWarn("SWP: Bundling group will be split into 2 cycles!");

      if (Trace_Swp_Bundling) {
        for (INT32_LIST::iterator it = ops_list.begin(); 
	        it != ops_list.end();
	        ++it) {
          OP* op = op_state[*it].op;
          TOP top = OP_code(op);
          fprintf(TFile, "PU%d|BB:%d|Freq:%f|Cycle%d|SPLIT_OP%d -- %s    (UNIT_Prop=%d)\n", 
                  Current_PU_Count(), BB_id(OP_bb(op)), BB_freq(OP_bb(op)), 
                  op_state[*it].modulo_cycle, 
                  *it, TOP_Name(top), ISA_EXEC_Unit_Prop(top));
        }
      }

      /* begin the last try */

      OP * op = op_state[ops_list.front()].op;
      if (BB_freq(OP_bb(op))>20 && !OP_flop(op)) {

        if ( SWP_Slot_Helper(op_state, grouping, dep_order,
                               prev_op_idx, bundle, ops_list) ) {
          DevWarn("SWP: Trying to fix split cycle......fixed!");
          if (Trace_Swp_Bundling) {
              fprintf(TFile, "SWP: Trying to fix split cycle......fixed!\n");
          }
        }
        else {
          DevWarn("SWP: Trying to fix split cycle......abandoned!");
          if (Trace_Swp_Bundling) {
              fprintf(TFile, "SWP: Trying to fix split cycle......abandoned!\n");
          }
        }

      }
  
    /* end of the last try*/

      SWP_Insert_Stop_Bit(op_state, grouping, prev_op_idx, 
			  bundle, TI_BUNDLE_slot_count(bundle) - 1);

      // TODO: recalculate reservation table for the next cycle.
      //
    }
  }
} // SWP_Pack_Into_New_Bundles


static void
SWP_Pack_Partially_Into_Prev_Bundle(SWP_OP_vector &op_state,
				    CG_GROUPING   &grouping, 
				    BOOL_MATRIX   &dep_order,
				    INT32         &prev_op_idx, 
				    TI_BUNDLE     *bundle, 
				    INT32          next_avail_slot,
				    INT32_LIST    &ops_list)
{
  // For now, do not do this.  This is really just an optimization 
  // to minimize the number of bundles (i.e. code-size); it does
  // not affect the cycle-count for the loop, other that potentially
  // reducing instruction-cache misses.
  //
  SWP_Fillup_Bundle(op_state, grouping, prev_op_idx, bundle, next_avail_slot);
  SWP_Pack_Into_New_Bundles(op_state,
			    grouping,
			    dep_order,
			    prev_op_idx,
			    bundle,
			    ops_list);
} // SWP_Pack_Partially_Into_Prev_Bundle

    
static void
SWP_Bundle_Next_Cycle(SWP_OP_vector         &op_state,       // in/out
		      CG_GROUPING           &grouping,       // in
		      BOOL_MATRIX           &dep_order,      // in
		      INT32_VECTOR          &resource_slack, // in
		      INT32_MATRIX::iterator opset,          // in
		      INT32_MATRIX::iterator opset_end,      // in
		      INT32                 &prev_op_idx,    // in/out
		      TI_BUNDLE             *current_bundle, // in/out
		      MEM_POOL              *mpool)
{
  // Bundle the operators in the given opset.  The output from bundling 
  // is a "slot" placement for each op in the set, which implicitly
  // determines bundling.  The next_slot and existing bundle information
  // is used for bundling this cycle, and will be updated for bundling
  // the subsequent cycle before returning from this subroutine.
  //
  // The main objective of bundling is to pack each opset into no more
  // than (grouping.max_slots_per_cycle()/ISA_MAX_SLOTS) bundles, in such a 
  // way that no bundle splits occur until after all ops for this cycle.
  //
  // The secondary objective is to produce as few bundles as possible.
  //
  // The bundling must therefore satisfy any dependencies, as well as 
  // resource constraints as imposed by CGTARG_Bundle_Slot_Available().
  //
  INT32_MATRIX::iterator it, it2;

  // Sort in-place the opset elements into the tentative partial order in
  // which we would like to see them allocated to slots, taking all of
  // dependency information, hints from the scheduler, and resource 
  // requirements into account.
  //
  // Simple topographical sort with O(n^2) number of compares.
  //
  SLOT_ORDER_CMP in_order = 
    SLOT_ORDER_CMP(op_state, grouping, dep_order, resource_slack);

  // rearrange for preferred ordering
  for (it = opset; it != opset_end; ++it) {
    for (it2 = it + 1; it2 != opset_end; ++it2)
      if (!in_order(*it, *it2))
	std::swap(*it, *it2);
  }

  // rearrange for correctness
  for (it = opset; it != opset_end; ++it) {
    for (it2 = it + 1; it2 != opset_end; ++it2)
      if (dep_order(*it2, *it))
	std::swap(*it2, *it);
  }

  if (Trace_Swp_Bundling && opset != opset_end)
  {
    fprintf(TFile, "\nSWP Grouping order for cycle %d: ",
	    op_state[*opset].modulo_cycle);
    for (it = opset; it != opset_end; ++it)
      fprintf(TFile, "OP%d %s (%d) ", *it, TOP_Name(OP_code(op_state[*it].op)),
	      op_state[*it].cycle);
    fprintf(TFile, "\n");

    // print each op for debug
    fprintf(TFile,"\n");
    for (it = opset; it != opset_end; ++it)
    Print_OP_No_SrcLine(op_state[*it].op);  //for debug

  }
  
  // Place the ops into bundles using the order dictated by the above sorting 
  // heuristics.  If no valid order is found, then split the ops into
  // multiple groups (i.e. multiple cycles).
  //
  const INT32 num_needed_slots = SWP_Min_Slot_Count(op_state, 
						    opset, opset_end,
						    grouping);
  INT32       num_remaining_slots = SWP_Num_Remaining_Slots(current_bundle);
  INT32       next_avail_slot = (TI_BUNDLE_slot_count(current_bundle) -
				 num_remaining_slots);
  INT32_LIST  ops_list(opset, opset_end, INT32_LIST::allocator_type(mpool));

  // For don't use compressed template, so fill it early to reduce
  // overuse func-unit;
  BOOL fillup_bundle = (num_remaining_slots > 0) ; 

  // if !fillup_bundle with nops,
  // Reserve resources for all ops that have no choice as to the
  // execution unit used.  The remaining available resources will
  // help determine which execution unit to use where there is a choice.
  //

  if (fillup_bundle) 
  {
      // No need to attempt to reuse slots in current_bundle.
      //
      SWP_Fillup_Bundle(op_state, grouping, 
			prev_op_idx, current_bundle, next_avail_slot);
  }

  grouping.clear_reserved();
  for (it = opset; it != opset_end; ++it)
  {
    TOP top = OP_code(op_state[*it].op);
    if (!grouping.has_resource_choice(top))
    {
      const bool ok = grouping.inorder_reserve_resource(top);
      Is_True(ok, ("Running out of resources in SWP_Bundle_Next_Cycle()"));
    }
  }

  if (num_remaining_slots > 0)
  {
    if (fillup_bundle)  {
      SWP_Pack_Into_New_Bundles(op_state, 
				grouping,
				dep_order,
				prev_op_idx, 
				current_bundle, 
				ops_list);
    } else {
      SWP_Pack_Partially_Into_Prev_Bundle(op_state,
					  grouping,
					  dep_order,
					  prev_op_idx, 
					  current_bundle, 
					  next_avail_slot,
					  ops_list);
    }
  } else {
    SWP_Pack_Into_New_Bundles(op_state, 
			      grouping,
			      dep_order,
			      prev_op_idx, 
			      current_bundle, 
			      ops_list);
  }
 

  // The group is packed, with a stop-bit set, and the last bundle is in 
  // current_bundle. The prev_op_idx points to the last op packed into 
  // the current_bundle. SWP_Bundle_Next_In_Group and 
  // SWP_Bundle_First_In_Group have set the "slot" attribute for each 
  // op_state operation.
  //
} // SWP_Bundle_Next_Cycle


void 
SWP_Bundle(SWP_OP_vector& op_state, bool trace)
{
  // Group and bundle the ops in the given op_state such that they can be 
  // issued in one cycle.  
  //
  MEM_POOL bundle_pool;

  Trace_Swp_Bundling = trace;

  MEM_POOL_Initialize(&bundle_pool, "CG_SWP_ALLOCATOR POOL", FALSE);
  MEM_POOL_Push(&bundle_pool);
  {
    // Define a matrix mapping each modulo cycle into the set of ops to be
    // executed at that cycle. The set of ops may have at most 
    // grouping.max_slots_per_cycle() entries, and cycle2op will assert if
    // we go beyond that number of entries for any set.
    //
    const INT32  empty_slot = -1;
    INT32        i;
    CG_GROUPING  grouping(&bundle_pool, SI_resource_count);
    INT32_MATRIX cycle2op(op_state.ii, 
			  grouping.max_slots_per_group(), 
			  empty_slot, 
			  INT32_MATRIX::allocator_type(&bundle_pool));
    INT32_VECTOR numops(op_state.ii,
			0,
			INT32_VECTOR::allocator_type(&bundle_pool));
    INT32_VECTOR resource_slack(op_state.size(), INT32_MAX, 
				INT32_VECTOR::allocator_type(&bundle_pool));

    // First put each op into the first available slot in the cycle2ops map,
    // and while doing so also set the modulo_cycle for each op and calculate
    // its "resource_slack".
    //
    // The resource_slack measures how critical its resource usage is
    // w.r.t. the total number of resources available.  The more critical
    // its resource usage, the smaller its resource_slack is, and the
    // earlier we should try to schedule such ops.
    //
    for (i = 0; i < op_state.size(); ++i)
    {
      if (op_state[i].op != NULL)
      {
	const INT32 mcycle = op_state[i].cycle % op_state.ii;
	op_state[i].modulo_cycle = mcycle;
	cycle2op(mcycle, numops[mcycle]++) = i;
	resource_slack[i] = grouping.resource_slack(OP_code(op_state[i].op));
      }
    }

    // Set up an ordering matrix, based on dependences within each 
    // cycle, where dep_order[i,j] is true iff op_state[i].op 
    // must precede op_state[j].op within the same cycle.
    //
    BOOL_MATRIX dep_order(op_state.size(), op_state.size(), false, 
			  BOOL_MATRIX::allocator_type(&bundle_pool));
    SWP_Init_Samecycle_Dep_Order(op_state, dep_order);
	
    // Next, bundle the ops in each cycle and assign them slot numbers
    // within the group.  The total number of slots used will be recorded 
    // for subsequent register allocation in the form of op_state.ii_slots.
    //
    INT32      prev_op_idx = SWP_INVALID_OP_IDX;
    TI_BUNDLE *current_bundle = TI_BUNDLE_Create(&bundle_pool);

    // Clear the current_bundle and set it to be full, such that the first
    // cycle will start with a new bundle.
    //
    TI_BUNDLE_Clear(current_bundle);
    for (INT slot = 0; slot < TI_BUNDLE_slot_count(current_bundle); ++slot)
      TI_BUNDLE_slot_filled(current_bundle, slot) = true;

    for (INT32 c = 0; c < op_state.ii; c++)
    {
      if (numops[c] > 0)
      {
	MEM_POOL_Push(&bundle_pool);
	SWP_Bundle_Next_Cycle(op_state,
			      grouping,
			      dep_order,
			      resource_slack,
			      cycle2op.pos(c, 0),
			      cycle2op.pos(c, numops[c] - 1) + 1,
			      prev_op_idx,
			      current_bundle,
			      &bundle_pool);
	MEM_POOL_Pop(&bundle_pool);
      }
    }
    
    // Fill in the remaining slots of the last bundle.
    //
    SWP_Fillup_Bundle(op_state, grouping, 
		      prev_op_idx, current_bundle, 
		      (TI_BUNDLE_slot_count(current_bundle) -
		       SWP_Num_Remaining_Slots(current_bundle)));

    op_state.ii_slots = grouping.total_no_of_bundles() * ISA_MAX_SLOTS;

  }
  
  SWP_Verify_Samecycle_Dep_Order(op_state);

  MEM_POOL_Pop(&bundle_pool);
  MEM_POOL_Delete(&bundle_pool);
} // SWP_Bundle


void 
SWP_Dont_Bundle(SWP_OP_vector& op_state)
{
  // For register allocation purposes, assume we have one slot per
  // cycle
  //
  op_state.ii_slots = op_state.ii;
   for (INT i = 0; i < op_state.size(); i++) {
     if (op_state[i].op) {
       op_state[i].modulo_cycle = op_state[i].cycle % op_state.ii;
       op_state[i].slot = op_state[i].modulo_cycle;
     }
   }
} // SWP_Dont_Bundle


void
SWP_Undo_Bundle(SWP_OP_vector& op_state, BB *body)
{
  for (INT i = 0; i < op_state.size(); i++) {
    if (op_state[i].is_noop && op_state[i].op) {
      BB_Remove_Op(body, op_state[i].op);
      op_state[i].op = NULL;
      op_state[i].is_noop = FALSE;
    }
    if (op_state[i].op) {
      Reset_OP_end_group(op_state[i].op);
      Reset_OP_bundled(op_state[i].op);
    }
  }
}
