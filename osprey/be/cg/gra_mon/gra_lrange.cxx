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

//
//  GRA_LRANGE implementation
/////////////////////////////////////
//
//  Thigs that weren't inlined
//
/////////////////////////////////////


//  $Revision: 1.24 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange.cxx $ $Revision: 1.24 $";
#endif

#if defined(__GNUC__)
#include <float.h>	// FLT_MAX
#else
#include <limits.h>
#endif
#include "defs.h"
#include "errors.h"
#include "cgir.h"
#include "tn_map.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cg_spill.h"
#include "cg_flags.h"

#include "gra_bb.h"
#include "gra_trace.h"
#include "gra_lrange.h"
#include "gra_lunit.h"
#include "gra_region.h"
#include "gra_lrange_subuniverse.h"
#include "gra_grant.h"
#include "gra_interfere.h"

#ifdef KEY
// If TRUE, allow reuse of registers at live range boundary basic blocks.
BOOL GRA_optimize_boundary = FALSE;
BOOL GRA_optimize_boundary_set = FALSE;

// If TRUE, prioritize live ranges by reference density.  If FALSE, prioritize
// by the number of references in the live range.
BOOL GRA_prioritize_by_density = FALSE;
BOOL GRA_prioritize_by_density_set = FALSE;
#endif

LRANGE_MGR lrange_mgr;

static INT32 complement_lrange_count, region_lrange_count, local_lrange_count,
	     duplicate_lrange_count;


/////////////////////////////////////
//  called at the start of each GRA invocation
void
LRANGE_MGR::Initialize(void)
{
  one_set_counter = 0;
  tn_map = TN_MAP_Create();
  complement_lrange_count = region_lrange_count = local_lrange_count = 0;
  duplicate_lrange_count = 0;
}

/////////////////////////////////////
//  called at the end of each GRA invocation
void
LRANGE_MGR::Finalize(void)
{
  TN_MAP_Delete(tn_map);
  GRA_Trace_LRANGE_Stats(complement_lrange_count,region_lrange_count,
                         local_lrange_count,duplicate_lrange_count);
}

/////////////////////////////////////
//  Common LRANGE creation stuff.  <type> is the type of LRANGE to create
//  and <rc> is its register class.  Common field are initialized as
//  appropriate.  <size> is how big to make it, which is more easily
//  determined in the clients.
//  Description
LRANGE*
LRANGE_MGR::Create( LRANGE_TYPE type, ISA_REGISTER_CLASS rc, size_t size )
{
  LRANGE* result = (LRANGE*) MEM_POOL_Alloc(GRA_pool,size);
  result->reg = 0;
  result->rc = rc;
  result->type = type;
  result->flags = (LR_FLAG) 0;
  result->mark = 0;
  result->pref = NULL;
  result->pref_priority = 0.0;

  return result;
}

/////////////////////////////////////
// Create and return a new complement LRANGE, corresponding
// to the given <tn>.  Uses the GRA_pool.  The newly created
// LRANGE is entered into the LRANGE_SUBUNIVERSE for the
// complement region (useful only for the representation of
// inteference, which is not exported from gra_lrange.c anyway.)
LRANGE*
LRANGE_MGR::Create_Complement( TN* tn )
{
  LRANGE* result = Create(LRANGE_TYPE_COMPLEMENT,
                                 TN_register_class(tn), sizeof(LRANGE));

  DevAssert(TN_Is_Allocatable(tn),
            ("Invalid TN for register allocation"));

  ++complement_lrange_count;

  result->u.c.tn = tn;
  result->u.c.original_tn = tn;     // Replaced if call from _Create_Duplicate
  result->u.c.first_lunit = NULL;
  // Why +2?  I think because 0 is reserved.
  result->u.c.live_bb_set = BB_SET_Create_Empty(PU_BB_Count+2,GRA_pool);
#ifdef KEY
  result->u.c.internal_bb_set = BB_SET_Create_Empty(PU_BB_Count+2,GRA_pool);
  result->u.c.boundary_bbs = NULL;
#endif
  result->u.c.global_pref_set = NULL;
  gra_region_mgr.Complement_Region()->Add_LRANGE(result);
  TN_MAP_Set(tn_map,tn,result);
  if (TN_is_save_reg(tn))
    result->Tn_Is_Save_Reg_Set();
  if (TN_is_gra_cannot_split(tn))
    result->Cannot_Split_Set();
  return result;
}

/////////////////////////////////////
// Create and return a new region LRANGE for the given <tn>.
// Enter it in <region>'s LRANGE_SUBUNIVERSE.
LRANGE*
LRANGE_MGR::Create_Region( TN* tn, GRA_REGION* region )
{
  LRANGE* result = Create(LRANGE_TYPE_REGION,
                                 TN_register_class(tn), sizeof(LRANGE));
  ++region_lrange_count;
  result->u.r.tn = tn;
  result->u.r.region = region;
  result->u.r.complement_bbs = NULL;
  result->orig_reg = TN_register(tn);
  region->Add_LRANGE(result);
  TN_MAP_Set(tn_map,tn,result);
  return result;
}

/////////////////////////////////////
// Create and return a new local LRANGE for the given <bb> and <cl>
LRANGE*
LRANGE_MGR::Create_Local( GRA_BB* gbb, ISA_REGISTER_CLASS cl )
{
  LRANGE* result = Create(LRANGE_TYPE_LOCAL, cl, sizeof(LRANGE) -
                                   sizeof(result->u.c) + sizeof(result->u.l));
  ++local_lrange_count;
  result->u.l.gbb = gbb;
  result->priority = gbb->Freq() * 2.0F;
  gra_region_mgr.Complement_Region()->Add_LRANGE(result);
  return result;
}

/////////////////////////////////////
// Create a new duplicate of the given <lrange> with a brand new
// GTN.  The intererence information, LUNITs, live_set, etc. are not copied.
LRANGE*
LRANGE_MGR::Create_Duplicate( LRANGE* lrange )
{
  TN* tn;
  LRANGE* result;

  DevAssert(lrange->type == LRANGE_TYPE_COMPLEMENT,
            ("Duplicating a non-COMPLEMENT LRANGE"));

  ++duplicate_lrange_count;
  tn = Dup_TN_Even_If_Dedicated(lrange->Tn());

  Set_TN_spill(tn, TN_spill(lrange->Original_TN()));

  GTN_UNIVERSE_Add_TN(tn);
  result = Create_Complement(tn);
  result->u.c.original_tn = lrange->u.c.original_tn;
  result->flags = (LR_FLAG)(result->flags | (lrange->flags & LRANGE_FLAGS_avoid_ra));
  return result;
}

/////////////////////////////////////
// Return the count of registers that can hold <lrange>.  This
// count is sensitive to whether <lrange> spans a call.  It could
// also be made more accurate for splitting purposes by keeping
// forbidden sets...
INT32 
LRANGE::Candidate_Reg_Count(void) {
  GRA_REGION *region;
  if ( Type() == LRANGE_TYPE_REGION )
    region = Region();
  else region = gra_region_mgr.Complement_Region();
  if ( Spans_A_Call() ) 
    return region->Callee_Saves_Registers_Available_Count(rc);
  else return REGISTER_SET_Size(region->Registers_Available(rc));
}	

/////////////////////////////////////
void 
LRANGE::Add_Live_BB(GRA_BB *gbb) { 
  if (Type() == LRANGE_TYPE_COMPLEMENT)
    u.c.live_bb_set = BB_SET_Union1D(u.c.live_bb_set, gbb->Bb(), GRA_pool);
}

/////////////////////////////////////
void 
LRANGE::Remove_Live_BB(GRA_BB *gbb) { 
  if (Type() == LRANGE_TYPE_COMPLEMENT)
    u.c.live_bb_set = BB_SET_Difference1D(u.c.live_bb_set, gbb->Bb());
}

/////////////////////////////////////
BOOL 
LRANGE::Contains_BB(GRA_BB *gbb) { 
  FmtAssert(Type() == LRANGE_TYPE_COMPLEMENT,
	    ("LRANGE_Contains_BB: LRANGE not a complement LRANGE"));
  return BB_SET_MemberP(u.c.live_bb_set, gbb->Bb());   
}

#ifdef KEY
/////////////////////////////////////
void 
LRANGE::Add_Internal_BB(GRA_BB *gbb) { 
  if (Type() == LRANGE_TYPE_COMPLEMENT)
    u.c.internal_bb_set = BB_SET_Union1D(u.c.internal_bb_set, gbb->Bb(), GRA_pool);
}

/////////////////////////////////////
void 
LRANGE::Remove_Internal_BB(GRA_BB *gbb) { 
  if (Type() == LRANGE_TYPE_COMPLEMENT)
    u.c.internal_bb_set = BB_SET_Difference1D(u.c.internal_bb_set, gbb->Bb());
}

/////////////////////////////////////
BOOL 
LRANGE::Contains_Internal_BB(GRA_BB *gbb) { 
  FmtAssert(Type() == LRANGE_TYPE_COMPLEMENT,
	    ("LRANGE_Contains_BB: LRANGE not a complement LRANGE"));
  return BB_SET_MemberP(u.c.internal_bb_set, gbb->Bb());   
}
#endif

/////////////////////////////////////
void 
LRANGE::Add_Global_Pref(TN *tn) {
  if (u.c.global_pref_set == NULL) {
    u.c.global_pref_set = GTN_SET_Create_Empty(GTN_UNIVERSE_size,
					       GRA_pool);
  }
  u.c.global_pref_set = GTN_SET_Union1D(u.c.global_pref_set, tn, GRA_pool);
}

/////////////////////////////////////
void 
LRANGE::Remove_Global_Pref(TN *tn) {
  u.c.global_pref_set = GTN_SET_Difference1D(u.c.global_pref_set, tn);
}

/////////////////////////////////////
BOOL 
LRANGE::Check_Global_Pref(TN *tn) {
  return Global_Pref_Set() != NULL && GTN_SET_MemberP(Global_Pref_Set(), tn);
}

/////////////////////////////////////
// Add <lunit> to <lrange>'s LUNIT_List, resetting <lunit>'s
// _lrange field.
void 
LRANGE::Add_LUNIT( LUNIT* lunit ) {
  u.c.first_lunit = u.c.first_lunit->Lrange_List_Push(lunit);
  lunit->Lrange_Set(this);
}

/////////////////////////////////////
// Prepare to present the inteference graph neighbors of
// <lrange>, a complement LRANGE.  It becomes "the current
// LRANGE".
void 
LRANGE_MGR::Begin_Complement_Interference(LRANGE *lrange) {
  Is_True(lrange->Type() == LRANGE_TYPE_COMPLEMENT, 
	  ("Not a complement LRANGE"));
  interference_creation_lrange = lrange;
  intf_mgr.Create_Begin(gra_region_mgr.Complement_Region()->Subuniverse(
					(ISA_REGISTER_CLASS)lrange->Rc()));
}

/////////////////////////////////////
// Present the next inteference graph <neighbor> of "the
// current LRANGE".  It is not necessary, but harmless to
// call this function for local LRANGEs.  Note that only one
// direction of the inteference arc is entered, that from the
// current LRANGE to <neighbor>
void 
LRANGE_MGR::Complement_Interference( LRANGE* neighbor ) {
  Is_True(neighbor->Type() == LRANGE_TYPE_COMPLEMENT,
	  ("Not a complement LRANGE"));
  if ( neighbor != interference_creation_lrange )
    intf_mgr.Create_Add_Neighbor(neighbor);
}

/////////////////////////////////////
// Complete the creation of the interference graph neighbors
// of "the current LRANGE".
void 
LRANGE_MGR::End_Complement_Interference( void ) {
  interference_creation_lrange->u.c.neighbors = intf_mgr.Create_End();
}

/////////////////////////////////////
// Initialize a REGION type <lrange> associated with the
// given <region> for creating of interference graph
// neighbors.  Unlike complement LRANGEs, there is no concept
// of a current LRANGE for inteference creation and
// inteferences may be created in any order.
void 
LRANGE::Initialize_Region_Inteference(GRA_REGION* region) {
  u.c.neighbors = intf_mgr.Create_Empty(region->Subuniverse(Rc()));
}

/////////////////////////////////////
// Create an interference arc from <lrange0> to <lrange1> and
// from <lrange1> to <lrange0>.  Both LRANGEs must be in the
// given <region>.
void
LRANGE::Region_Interference( LRANGE*     lrange1, GRA_REGION* region )
{
  LRANGE_SUBUNIVERSE* su = region->Subuniverse(lrange1->Rc());

  if ( this != lrange1 ) {
    u.c.neighbors = u.c.neighbors->Add_Neighbor(lrange1, su);
    lrange1->u.c.neighbors =
      lrange1->u.c.neighbors->Add_Neighbor(this, su);
  }
}

/////////////////////////////////////
// Search for a LUNIT associated with <lrange> and <gbb>.  Return
// TRUE to indicate success, the the found LUNIT returned by
// reference in <lunitp>.
BOOL
LRANGE::Find_LUNIT_For_GBB( const GRA_BB* gbb, LUNIT** lunitp )
{
  LRANGE_LUNIT_ITER iter;

  if ( Type() != LRANGE_TYPE_COMPLEMENT )
    return FALSE;

  for (iter.Init(this); ! iter.Done(); iter.Step())
  {
    LUNIT* lunit = iter.Current();

    if ( lunit->Gbb() == gbb ) {
      *lunitp = lunit;
      return TRUE;
    }
  }

  return FALSE;
}

/////////////////////////////////////
// Delete <neighbor> from <lrange>'s interference graph
// neighbors.  Both LRANGEs must be in <region>.  It is an
// error if <neighbor> is not actually found among the
// neighbors of <lrange>.  Both <lrange> and <neighbor>
// should be complement or region LRANGEs or the function
// will have no effect.  (Local LRANGEs represent their
// neighbors represent their neighbors implicitly based on
// their associated BB.  Compliment LRANGEs represent their
// interference relation with local LRANGEs implicitly via
// their _live_bb_set.
//
// Our splitting algorithm doesn't ever need to be able to
// add neighbors.  This is beasue it will always color one of
// the two halves of the split right away and thus not need
// to keep valid interference arcs either from or to it.  We
// may need to delete after splitting because the remaining
// (deferred) half of the split won't in general interfere
// with all the same nodes as it did before a part was split
// off and colored.
void
LRANGE::Remove_Neighbor( LRANGE*     neighbor, GRA_REGION* region )
{
  //  Local interference is implicit in the BBs in the lrange and thus don't
  //  have to be maintained.
  if ( Type() != LRANGE_TYPE_LOCAL && neighbor->Type() != LRANGE_TYPE_LOCAL) {
    u.c.neighbors = u.c.neighbors->Remove_Neighbor(neighbor,
						   region->Subuniverse(Rc()));
  }
}

/////////////////////////////////////
// Use the live_gbb information to determone if <lrange0> and
// <lrange1> interference graph neighbors?  Since this is really
// coarse interference it is meaningless for REGION LRANGEs and
// will assert if either argument is one.
BOOL
LRANGE::Interferes( LRANGE* lr1 )
{
  DevAssert(Type() != LRANGE_TYPE_REGION && lr1->Type() != LRANGE_TYPE_REGION,
             ("LRANGE_Interferes not valid for REGION LRANGEs."));

  if ( Type() == LRANGE_TYPE_COMPLEMENT ) {
    if ( lr1->Type() == LRANGE_TYPE_COMPLEMENT ) {
#ifdef KEY
      if (GRA_optimize_boundary) {
	// If none of the live BBs interfere, then the LRANGEs don't interfere.
	if (!BB_SET_IntersectsP(u.c.live_bb_set, lr1->u.c.live_bb_set))
	  return FALSE;

	// If some internal BBs interfere, then the LRANGEs interfere.
	if (BB_SET_IntersectsP(u.c.internal_bb_set, lr1->u.c.internal_bb_set))
	  return TRUE;

	// No internal BBs interefere.  Check if the boundary BBs interfere.

	// TODO:  Clean up code and make more efficient.

	// Find the two LRANGEs' boundary BBs.
	BB_SET* this_boundary_bbs = BB_SET_Difference(u.c.live_bb_set,
				      u.c.internal_bb_set, &MEM_local_nz_pool);
	BB_SET* lr1_boundary_bbs = BB_SET_Difference(lr1->u.c.live_bb_set,
				  lr1->u.c.internal_bb_set, &MEM_local_nz_pool);
	// Analyze the intersection of the boundary BBs.
	BB *current =
	      BB_SET_Intersection_Choose(this_boundary_bbs, lr1_boundary_bbs);
	while (current != BB_SET_CHOOSE_FAILURE) {
	  LRANGE_BOUNDARY_BB *boundary_bb0 = Get_Boundary_Bb(current);
	  LRANGE_BOUNDARY_BB *boundary_bb1 = lr1->Get_Boundary_Bb(current);
	  Is_True(boundary_bb0 != NULL && boundary_bb1 != NULL,
		  ("Boundary BB not found"));
	  if (boundary_bb0->Interfere(boundary_bb1))
	    return TRUE;
          current = BB_SET_Intersection_Choose_Next(this_boundary_bbs,
						    lr1_boundary_bbs, current);
	}
	// The boundary BBs don't interfere, so no interference.
	return FALSE;
      } else
#endif
      return BB_SET_IntersectsP(u.c.live_bb_set, lr1->u.c.live_bb_set);
    }
    else {
      return BB_SET_MemberP(u.c.live_bb_set, lr1->u.l.gbb->Bb());
    }
  }
  else if ( lr1->Type() == LRANGE_TYPE_COMPLEMENT ) {
    return BB_SET_MemberP(lr1->u.c.live_bb_set, u.l.gbb->Bb());
  }
  else
    return u.l.gbb->Bb() == lr1->u.l.gbb->Bb();
}

/////////////////////////////////////
void
LRANGE::Calculate_Priority(void)
/////////////////////////////////////
//
//  Notice how our definition of priority differs from the classic Chow
//  definition.  Fred wants to penalize sparsely used live ranges compared
//  to ones with the same number of references in fewer blocks.  We just
//  use the frequency weighted count of memory operations that will have
//  to be added if the LRANGE is spilled.
//
//  Why the difference?  At least in part it is because we handle
//  splitting differently.  Fred splits continuously.  After each LRANGE
//  is allocated to a register, he checks all its neighbors and splits any
//  that become uncolorable.  We skip this step and only split when a
//  coloring attempt fails.  Thus we coloring priorities to reflect the
//  priority of the hightest priority split we could posssibly pick out of
//  the LRANGE.  But we'll see...
//
/////////////////////////////////////
{
  if ( Must_Allocate() )
    priority = FLT_MAX;
  else if ( Type() == LRANGE_TYPE_LOCAL )
    priority = u.l.gbb->Freq() * 2.0F;
  else if ( Type() == LRANGE_TYPE_REGION )
    priority = 0.0;
  else {
    LRANGE_LUNIT_ITER iter;
    float value = 0;
    float sc, rc;

    CGSPILL_Cost_Estimate(Tn(),NULL,&sc,&rc,CGSPILL_GRA);

    for (iter.Init(this); ! iter.Done(); iter.Step())
    {
      LUNIT* lunit = iter.Current();
      float  freq = lunit->Gbb()->Freq();

      // What's the cost of spilling?  If we have a def/use here, the cost is
      // that we will have to spill/restore.  If we have a restores/spills
      // caused by splitting, on the other hand, we won't have to perform
      // them, so that's a benefit.

      if ( lunit->Has_Exposed_Use() && ! lunit->Restore_Above() ) {
        value += (freq * rc);
	GRA_Trace_Split_Add_Priority(lunit->Gbb(), FALSE);
      } else if (!lunit->Has_Exposed_Use() && lunit->Restore_Above() ){
        value -= (freq * rc);
	GRA_Trace_Split_Sub_Priority(lunit->Gbb(), FALSE);	
      }
      if ( lunit->Has_Def() && lunit->Live_Out() &&
	   !lunit->Spill_Below()) {
        value += (freq * sc);
	GRA_Trace_Split_Add_Priority(lunit->Gbb(), TRUE);
      } else if ( ! lunit->Has_Def() && lunit->Spill_Below() ) {
        value -= (freq * sc);
	GRA_Trace_Split_Sub_Priority(lunit->Gbb(), TRUE);
      }
    }
    priority = value;
#ifdef KEY
    // Prioritize live ranges by reference density as in classic Chow.
    if (GRA_prioritize_by_density) {
      UINT32 num_bbs = BB_SET_Size(this->Live_BB_Set());
      priority = priority / num_bbs;
    }

    // Exclude callee-saved registers from normal GRA use by always allocating
    // them to their save TNs.
    if ((GRA_exclude_callee_saved_regs ||
	 GRA_eh_exclude_callee_saved_regs && PU_Has_Exc_Handler ||
	 GRA_fp_exclude_callee_saved_regs && TN_is_float(Tn()))
	&& Tn_Is_Save_Reg()) {
      priority = FLT_MAX;
    }
#endif
  }
}

/////////////////////////////////////
#ifndef KEY
static
#endif
REGISTER_SET
Global_Preferenced_Regs(LRANGE* lrange, GRA_BB* gbb)
/////////////////////////////////////
//
//  return the set of registers for tn's preferenced to this live 
//  range that are live in this block
//
/////////////////////////////////////
{
  REGISTER_SET global_prefs = REGISTER_SET_EMPTY_SET;
  if (lrange->Global_Pref_Set()) {
    for (TN *tn = GTN_SET_Choose(lrange->Global_Pref_Set());
	 tn != GTN_SET_CHOOSE_FAILURE;
	 tn = GTN_SET_Choose_Next(lrange->Global_Pref_Set(), tn)) {
      LRANGE *plrange = lrange_mgr.Get(tn);
#ifdef KEY
      if (GRA_optimize_boundary) {
	// Before making the preferenced TN's register available, check to see
	// if that register is also used by a non-preferenced TN in the BB.  If
	// so, the register cannot be used.
	if (plrange->Contains_BB(gbb) && plrange->Allocated()) {
	  TN *lrange_tn = lrange->Tn();
	  REGISTER reg = plrange->Reg();
	  BOOL used_by_nonpreferenced_tn = FALSE;
	  for (OP *op = BB_first_op(gbb->Bb());
	       op != NULL && !used_by_nonpreferenced_tn;
	       op = OP_next(op)) {
	    // Check source operands.
	    for (int i = OP_opnds(op) - 1; i >= 0; i--) {
	      TN *opnd_tn = OP_opnd(op, i);
	      if (opnd_tn != tn && opnd_tn != lrange_tn) {
		LRANGE *lr = lrange_mgr.Get(opnd_tn);
		if (lr != NULL && lr->Allocated() && lr->Reg() == reg) {
		  used_by_nonpreferenced_tn = TRUE;
		  break;
		}
	      }
	    }
	    // Check result operands.
	    for (int i = OP_results(op) - 1; i >= 0; i--) {
	      TN *result_tn = OP_result(op, i);
	      if (result_tn != tn && result_tn != lrange_tn) {
		LRANGE *lr = lrange_mgr.Get(result_tn);
		if (lr != NULL && lr->Allocated() && lr->Reg() == reg) {
		  used_by_nonpreferenced_tn = TRUE;
		  break;
		}
	      }
	    }
	  }
	  if (! used_by_nonpreferenced_tn)
	    global_prefs = REGISTER_SET_Union1(global_prefs, reg);
	}
      } else
#endif
      if (plrange->Contains_BB(gbb) && plrange->Allocated()) {
	global_prefs = REGISTER_SET_Union1(global_prefs, plrange->Reg());
      }
    }
  }
  return global_prefs;
}

/////////////////////////////////////
// Return the set of registers still allowed for <lrange>.
// This means finding the set of registers such that there is
// no conflict with any already allocated neighbor of
// <lrange>.  <lrange> must belong to <region>.
REGISTER_SET
LRANGE::Allowed_Registers( GRA_REGION* region )
{
  LRANGE_LIVE_GBB_ITER gbb_iter;
  LRANGE_LUNIT_ITER    lunit_iter;
  INTERFERE_ITER       int_iter;
  ISA_REGISTER_CLASS   rc      = Rc();
  REGISTER_SET         allowed = REGISTER_CLASS_allocatable(rc);

#ifdef HAS_STACKED_REGISTERS
  if (REGISTER_Has_Stacked_Registers(rc)) {
    allowed = REGISTER_SET_Difference(allowed, REGISTER_CLASS_stacked(rc));
    //
    // Add in the appropriate used stacked registers if available.  Don't
    // allow non-call-spanning live ranges a chance at the callee saved
    // yet.  The register choosing code will try to allocate a new caller
    // saved before using up a callee saved.
    //
    if (Has_Wired_Register() &&
        REGISTER_Is_Stacked(rc, Reg())) {
      allowed = REGISTER_SET_Union1(allowed, Reg());
    } else if ( Spans_A_Setjmp() ) {
    } else if ( Spans_A_Call() ) {
      REGISTER_SET stacked =
        REGISTER_Get_Stacked_Avail_Set(ABI_PROPERTY_callee, rc);
      allowed = REGISTER_SET_Union(allowed, stacked);
    } else {
      REGISTER_SET stacked =
        REGISTER_Get_Stacked_Avail_Set(ABI_PROPERTY_stacked, rc);
      allowed = REGISTER_SET_Union(allowed, stacked);
    }
  }
#endif

  // if the live range spans an instruction that clobbers rotating registers,
  // disallow rotating registers
  if (Spans_Rot_Reg_Clob()) 
    allowed = REGISTER_SET_Difference(allowed,
				      REGISTER_CLASS_rotating(rc));

  // if the live range spans a setjmp, disallow callee-saved registers
  if (Spans_A_Setjmp() && ! TN_is_save_reg(Tn()))
    allowed = REGISTER_SET_Difference(allowed,
				      REGISTER_CLASS_callee_saves(rc));

#ifdef KEY
  // if PU has label jumped to from nested func, disallow callee-saved registers
  if (PU_Has_Nonlocal_Goto_Target)
    allowed = REGISTER_SET_Difference(allowed,
				      REGISTER_CLASS_callee_saves(rc));
#endif

#ifdef TARG_X8664
  // if the live range spans savexmms pseudo-op, disallow FP parm registers
  if (Spans_Savexmms() && rc == ISA_REGISTER_CLASS_float) {
    INT num_xmms = TN_value(OP_opnd(gra_savexmms_op, 1));
    for (INT i = 1; i <= num_xmms; i++)
      allowed = REGISTER_SET_Difference1(allowed, REGISTER_MIN+(8-i));
  }

  // If a x87 live range spans an MMX OP, disallow all x87 registers.
  if (Spans_mmx_OP() && rc == ISA_REGISTER_CLASS_x87)
    return REGISTER_SET_EMPTY_SET;

  // If a MMX live range spans an x87 OP, disallow all MMX registers.
  if (Spans_x87_OP() && rc == ISA_REGISTER_CLASS_mmx)
    return REGISTER_SET_EMPTY_SET;
#endif

  if (   Type() != LRANGE_TYPE_LOCAL && TN_is_save_reg(Tn())
  ) {
#ifdef TARG_IA64
    // we copied the reg. class (creg) of this copy TN in
    // Init_Callee_Saved_Regs_for_REGION, this confuses TN_save_reg to
    // return a dedicated TN (of lc), then in turn tries to allocate that
    // which is not allowed, eventually, it cause GRA to allocate from RSE
    // We can go ahead allocate registers
    if (TN_save_rclass(Tn()) == REGISTER_CLASS_lc) {
        allowed = REGISTER_SET_Difference(allowed,
                                    REGISTER_CLASS_callee_saves(rc));
    }else {
        REGISTER sv_reg = TN_save_reg(Tn());
        REGISTER_SET singleton = REGISTER_SET_Union1(REGISTER_SET_EMPTY_SET,sv_reg);
        allowed = REGISTER_SET_Intersection(allowed,singleton);
    }
#else
    REGISTER sv_reg = TN_save_reg(Tn());
    REGISTER_SET singleton = REGISTER_SET_Union1(REGISTER_SET_EMPTY_SET,sv_reg);
    allowed = REGISTER_SET_Intersection(allowed,singleton);
#endif
  }

  switch (Type()) {

  case LRANGE_TYPE_LOCAL:
    return
      REGISTER_SET_Difference(allowed, Gbb()->Registers_Used(rc));

  case LRANGE_TYPE_COMPLEMENT:
    gbb_mgr.Clear_One_Set();

    //
    // First visit it's LUNITs.  These may contain allowed_preferences, which
    // are registers that we are permitted to use even if they are already
    // used in their blocks.  The reason for this is that the user is
    // guaranteed to be a local LRANGE with a wired register which is
    // preferenced to <lrange>.  allow registers used by globally preferenced
    // tn's in the block.  we've already guaranteed that there is no conflict
    // between them (though this won't guarantee that the register will be
    // usable as it may be used by another tn outside the preferenced tn's
    // live range but within this tn's live range).
    //
    for (lunit_iter.Init(this); ! lunit_iter.Done(); lunit_iter.Step()) {
      LUNIT* lunit = lunit_iter.Current();
      GRA_BB* gbb = lunit->Gbb();
      REGISTER_SET used = gbb->Registers_Used(rc);
#ifdef KEY
      if (GRA_optimize_boundary) {
	// If <gbb> is a boundary BB, remove from <used> those registers that
	// are unused in the parts of the BB where the lrange is live.
	if (! Contains_Internal_BB(gbb)) {
	  REGISTER reg;
	  LRANGE_BOUNDARY_BB* boundary_bb = Get_Boundary_Bb(gbb->Bb());
	  for (reg = REGISTER_SET_Choose(used);
	       reg != REGISTER_UNDEFINED;
	       reg = REGISTER_SET_Choose_Next(used, reg)) {
	    if (! boundary_bb->Interfere(gbb, rc, reg)) {
	      used = REGISTER_SET_Difference1(used, reg);
	    }
	  }
	}
      }
#endif
      REGISTER_SET allowd_prefs = lunit->Allowed_Preferences();
      
      allowd_prefs = REGISTER_SET_Union(allowd_prefs,
					Global_Preferenced_Regs(this, gbb));
      gbb_mgr.One_Set_Union1(gbb);
      allowed =
        REGISTER_SET_Difference(allowed,
                                REGISTER_SET_Difference(used,allowd_prefs));
    }

    for (gbb_iter.Init(this); ! gbb_iter.Done(); gbb_iter.Step()) {
      GRA_BB* gbb = gbb_iter.Current();

      if ( ! gbb_mgr.One_Set_MemberP(gbb) ) {
	REGISTER_SET prefs = Global_Preferenced_Regs(this, gbb);
	REGISTER_SET used = gbb->Registers_Used(rc);
        allowed = REGISTER_SET_Difference(allowed,
					  REGISTER_SET_Difference(used,
								  prefs));
      }
    }
    if ( Avoid_RA() )
      allowed = REGISTER_SET_Difference1(allowed,TN_register(RA_TN));

    if ( Spans_A_Call() ) {
      return REGISTER_SET_Difference(allowed,
				     REGISTER_CLASS_caller_saves(rc));
    } else {
      return allowed;
    }
  case LRANGE_TYPE_REGION:
    //  This will be a little faster than using the generic itertaion method
    //  and this is probably an important case since we will have to calculate
    //  the allowed registers at least once per LR and each calculation has to
    //  visit all the neighbors.
    //
    for (int_iter.Init(u.r.neighbors, region->Subuniverse(Rc()));
         ! int_iter.Done();
         int_iter.Step() ) {
      LRANGE* nlr = int_iter.Current();

      if ( nlr->Allocated() )
        allowed = REGISTER_SET_Difference1(allowed, nlr->Reg());
    }
    if ( Spans_A_Call() ) {
      return REGISTER_SET_Difference(allowed,
				     REGISTER_CLASS_caller_saves(rc));
    } else {
      return allowed;
    }

  default:
    FmtAssert(FALSE,("Unknown type of LRANGE %d",Type()));
    return REGISTER_SET_EMPTY_SET;
  }
}

#ifdef KEY
/////////////////////////////////////
// Return the set of registers that are reclaimable for <lrange>.  This means
// finding the set of registers that are already allocated to some other lrange
// but are not referenced in the BBs spanned by <lrange>.  <lrange> must belong
// to <region>.
REGISTER_SET
LRANGE::Reclaimable_Registers( GRA_REGION* region )
{
  LRANGE_LIVE_GBB_ITER gbb_iter;
  LRANGE_LUNIT_ITER    lunit_iter;
  INTERFERE_ITER       int_iter;
  ISA_REGISTER_CLASS   rc      = Rc();
  REGISTER_SET         reclaimable = REGISTER_CLASS_allocatable(rc);

#ifdef HAS_STACKED_REGISTERS
  FmtAssert(FALSE,("Reclaimable_Registers: stacked register not implemented"));
#endif

  // if the live range spans an instruction that clobbers rotating registers,
  // disallow rotating registers
  if (Spans_Rot_Reg_Clob()) 
    reclaimable = REGISTER_SET_Difference(reclaimable,
					  REGISTER_CLASS_rotating(rc));

  // if the live range spans a setjmp, disallow callee-saved registers
  if (Spans_A_Setjmp() && ! TN_is_save_reg(Tn()))
    reclaimable = REGISTER_SET_Difference(reclaimable,
					  REGISTER_CLASS_callee_saves(rc));

#ifdef TARG_X8664
  // if the live range spans savexmms pseudo-op, disallow FP parm registers
  if (Spans_Savexmms() && rc == ISA_REGISTER_CLASS_float) {
    INT num_xmms = TN_value(OP_opnd(gra_savexmms_op, 1));
    for (INT i = 1; i <= num_xmms; i++)
      reclaimable = REGISTER_SET_Difference1(reclaimable, REGISTER_MIN+(8-i));
  }
#endif

  if (   Type() != LRANGE_TYPE_LOCAL && TN_is_save_reg(Tn())
  ) {
    REGISTER sv_reg = TN_save_reg(Tn());
    REGISTER_SET singleton = REGISTER_SET_Union1(REGISTER_SET_EMPTY_SET,sv_reg);
    reclaimable = REGISTER_SET_Intersection(reclaimable, singleton);
  }

  switch (Type()) {

  case LRANGE_TYPE_LOCAL:
    FmtAssert(FALSE, ("Reclaimable_Registers: unexpected LRANGE_TYPE_LOCAL"));
    return REGISTER_SET_EMPTY_SET;

  case LRANGE_TYPE_COMPLEMENT:
    for (gbb_iter.Init(this); ! gbb_iter.Done(); gbb_iter.Step()) {
      GRA_BB* gbb = gbb_iter.Current();
      REGISTER_SET used = gbb->Registers_Used(rc);
      REGISTER_SET referenced = gbb->Registers_Referenced(rc);
      // Consider a register reclaimable if it is used and not referenced.
      // Experiment with other criteria in the future.
      reclaimable = REGISTER_SET_Intersection(reclaimable, used);
      reclaimable = REGISTER_SET_Difference(reclaimable, referenced);
    }
    if (Avoid_RA())
      reclaimable = REGISTER_SET_Difference1(reclaimable, TN_register(RA_TN));

    if (Spans_A_Call()) 
      reclaimable = REGISTER_SET_Difference(reclaimable,
					    REGISTER_CLASS_caller_saves(rc));
    return reclaimable;

  case LRANGE_TYPE_REGION:
    FmtAssert(FALSE,
	      ("Reclaimable_Registers: LRANGE_TYPE_REGION not implemented"));
    return REGISTER_SET_EMPTY_SET;

  default:
    FmtAssert(FALSE,("Unknown type of LRANGE %d",Type()));
    return REGISTER_SET_EMPTY_SET;
  }
}
#endif

/////////////////////////////////////
// We've picked <reg> as the register for <lrange>.  Update
// <lrange> and other data structures as appropriate
// (particularly GRA_BBs and GRA_REGIONS.)
void
LRANGE::Allocate_Register( REGISTER r, BOOL reclaim )
{
  LRANGE_LIVE_GBB_ITER live_gbb_iter;
  LRANGE_GLUE_REF_GBB_ITER glue_gbb_iter;

  DevAssert(! Allocated(),("Reallocating a LRANGE register"));
  reg = r;
  flags = (LR_FLAG)(flags | LRANGE_FLAGS_allocated);

  if ( Pref() != NULL )
    Pref()->Allocate_LRANGE(this);

  switch ( Type() ) {
  case LRANGE_TYPE_LOCAL:
    GRA_GRANT_Local_Register(Gbb(),Rc(),r);
    Gbb()->Make_Register_Used((ISA_REGISTER_CLASS) Rc(), r, NULL, reclaim);

#ifdef KEY
    // Update the referenced registers.
    if (GRA_reclaim_register) {
      Gbb()->Make_Register_Referenced(Rc(), r, this);
    }
#endif
    break;
  case LRANGE_TYPE_REGION:
#ifdef KEY
    // Need to call Make_Register_Referenced.
    FmtAssert(!GRA_reclaim_register,
	      ("Allocate_Register: register reclaiming not yet supported "
	       "for LRANGE_TYPE_REGION"));
#endif
    TN_Allocate_Register(Tn(),r);
    Region()->Make_Register_Used(Rc(), r);
    for (glue_gbb_iter.Init(this); ! glue_gbb_iter.Done(); glue_gbb_iter.Step())
    {
      GRA_BB* gbb = glue_gbb_iter.Current();
      gbb->Make_Glue_Register_Used(Rc(),r);
    }
    break;
  case LRANGE_TYPE_COMPLEMENT:
    TN_Allocate_Register(Tn(),r);
    for (live_gbb_iter.Init(this); ! live_gbb_iter.Done(); live_gbb_iter.Step())
    {
      GRA_BB* gbb = live_gbb_iter.Current();
      gbb->Make_Register_Used(Rc(), r, this, reclaim);
    }
#ifdef KEY
    // Update the referenced registers.
    if (GRA_reclaim_register) {
      LRANGE_LUNIT_ITER lunit_iter;
      for (lunit_iter.Init(this); !lunit_iter.Done(); lunit_iter.Step()) {
	LUNIT *lunit = lunit_iter.Current();
	if (Contains_BB(lunit->Gbb()))
	  lunit->Gbb()->Make_Register_Referenced(Rc(), r, this);
      }
    }
#endif
  }

  GRA_Trace_LRANGE_Allocate(this);
}

/////////////////////////////////////
// Return the total number of interference graph neighbors.
INT32
LRANGE::Neighbor_Count(void)
{
  LRANGE_LIVE_GBB_ITER gbb_iter;
  INT32 result;
  ISA_REGISTER_CLASS rc = Rc();

  switch ( Type() ) {
  case LRANGE_TYPE_REGION:
    return u.c.neighbors->Count();
  case LRANGE_TYPE_LOCAL:
    return Gbb()->Local_Lrange_Count(rc) + Gbb()->Global_Live_Lrange_Count(rc)
           - 1;
  case LRANGE_TYPE_COMPLEMENT:
    result = 0;
    for (gbb_iter.Init(this); ! gbb_iter.Done(); gbb_iter.Step()) {
      GRA_BB* gbb = gbb_iter.Current();
      if ( gbb->Region_Is_Complement() )
	result += gbb->Local_Lrange_Count(rc);
      else {
        // FIXIT: horrible hack that penalizes LRANGES that psss through
        // SWP loops.  We need something better here.
        result -= 300;
      }
    }

    return result + u.c.neighbors->Count();
  default:
    FmtAssert(FALSE,("_Neighbor_Count of unknown type of LRANGE"));
    return UNDEFINED;
  }
}

/////////////////////////////////////
// put here because in the header file, gbb_mgr has not yet been defined
GRA_BB *
LRANGE_LIVE_GBB_ITER::Current(void)
{
  return gbb_mgr.Get(current);
}

//  Iterating over complement LRANGEs' inteference graph neighbors
//////////////////////////////////////////////////////////////////////////
//
//  THis is pretty hairy because we have to a fairly complex representation.
//  The global neighbors are all safely tucked away in the _neighbors INTEFERE
//  data structure associated with each complement LRANGE.  But we also need
//  to visit each same register-class local in each block in its LRANGE.  In
//  order to do this, we'll have to walk the BBs and walk the lranges in each
//  BB.
//
//  Notice the trick of representing the states (iterating over
//  globals/locals) with the _step function.  This prevents having to check
//  whether the globals are done each time we step the locals.
//
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Complement_Local_Init( LRANGE_NEIGHBOR_ITER* iter )
/////////////////////////////////////
//
//  Find the next gbb (inclusive to the current) with interfering local
//  LRANGEs.  Set up to loop over the locals in the found gbb.
//
/////////////////////////////////////
{
  LRANGE_LIVE_GBB_ITER*     gbb_iter   = &(iter->live_gbb_iter);
  GRA_BB_LOCAL_LRANGE_ITER* local_iter = &(iter->bb_local_iter);

  for ( ; ! gbb_iter->Done(); gbb_iter->Step()) {
    GRA_BB* gbb = gbb_iter->Current();

    if ( gbb->Region() == gra_region_mgr.Complement_Region() ) {
      local_iter->Init(gbb,iter->rc);
      if ( ! local_iter->Done() ) {
        iter->done = FALSE;
        iter->current = local_iter->Current();
        return;
      }
    }
  }

  //  Couldn't find a bb in the range with local neighbors.
  iter->done = TRUE;
}

/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Complement_Local_Step( LRANGE_NEIGHBOR_ITER* iter )
/////////////////////////////////////
//
//  Step to the next interfering local LRANGE, advancing to a new block if
//  required.
//
/////////////////////////////////////
{
  GRA_BB_LOCAL_LRANGE_ITER* local_iter = &(iter->bb_local_iter);

  local_iter->Step();

  if ( ! local_iter->Done()) {
    //  At least one more in this block.
    iter->current = local_iter->Current();
  }
  else {
    //  Advance to the next block containing at least one conflicting local
    //  live range:
    iter->live_gbb_iter.Step();
    LRANGE_NEIGHBOR_ITER_Complement_Local_Init(iter);
  }
}

/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Complement_Global_Step( LRANGE_NEIGHBOR_ITER* iter )
/////////////////////////////////////
//
//  Step to the next intefering non-local LRANGE.  If there are no more, start
//  stepping locals.
//
/////////////////////////////////////
{
  INTERFERE_ITER* neighbor_iter = &(iter->neighbor_iter);

  neighbor_iter->Step();

  if ( ! neighbor_iter->Done() )
    iter->current = neighbor_iter->Current();
  else {
    //  No more global neighbors.  Set up to loop over the local neighbors.
    iter->step = LRANGE_NEIGHBOR_ITER_Complement_Local_Step;
    LRANGE_NEIGHBOR_ITER_Complement_Local_Init(iter);
  }
}

/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Complement_Init( LRANGE_NEIGHBOR_ITER* iter,
                                      LRANGE*               lrange,
                                      GRA_REGION*           region )
/////////////////////////////////////
//
//  Prepare to iterate over any global neighbors and then over all the locals
//  in all the blocks in the live range.
//
/////////////////////////////////////
{
  LRANGE_LIVE_GBB_ITER* gbb_iter = &(iter->live_gbb_iter);
  INTERFERE             c_neighbors = lrange->Neighbors();
  LRANGE_SUBUNIVERSE* su = region->Subuniverse(lrange->Rc());

  iter->rc = lrange->Rc();

  gbb_iter->Init(lrange);

  if ( c_neighbors->Count() > 0 ) {
    //  We have global neighbors to work on  Set up to step over the
    //  INTEFERE:
    iter->done = FALSE;
    iter->neighbor_iter.Init(c_neighbors,su);
    iter->current = iter->neighbor_iter.Current();
    iter->step = LRANGE_NEIGHBOR_ITER_Complement_Global_Step;
  }
  else {
    //  No global neighbors.  Set up to loop over the local neighbors.
    iter->step = LRANGE_NEIGHBOR_ITER_Complement_Local_Step;
    //  This finds the first local neighbor starting
    //  with the current gbb from _live_gbb_iter:
    LRANGE_NEIGHBOR_ITER_Complement_Local_Init(iter);
  }
}


//  Iterating over REGION LRANGEs' inteference graph neighbors
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Region_Step( LRANGE_NEIGHBOR_ITER* iter )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  iter->neighbor_iter.Step();
  iter->done = iter->neighbor_iter.Done();
  if ( ! iter->done )
    iter->current = iter->neighbor_iter.Current();
}

/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Region_Init( LRANGE_NEIGHBOR_ITER* iter,
                                  LRANGE*               lrange,
                                  GRA_REGION*           region )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  LRANGE_SUBUNIVERSE* su = region->Subuniverse(lrange->Rc());

  iter->step = LRANGE_NEIGHBOR_ITER_Region_Step;
  iter->neighbor_iter.Init(lrange->Neighbors(), su);

  iter->done = iter->neighbor_iter.Done();
  if ( ! iter->done )
    iter->current = iter->neighbor_iter.Current();
}


//  Iterating over LOCAL LRANGEs' inteference graph neighbors
//////////////////////////////////////////////////////////////////////////
//
//  Problems similar to but different from iterating over the neighbors of the
//  complement LRANGEs.  But now we have to visit the locals (skipping
//  the given <lrange> and the neighbors of the GBB itself.
//
//////////////////////////////////////////////////////////////////////////


/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Local_Global_Step( LRANGE_NEIGHBOR_ITER* iter )
/////////////////////////////////////
//
//  Step through the globals for the block of the local on which <iter> was
//  initialized.  We've already stepped the locals if any, so when we are done
//  here, we are really done.
//
/////////////////////////////////////
{
  INTERFERE_ITER* global_iter = &(iter->bb_live_global_iter);

  global_iter->Step();
  iter->done = global_iter->Done();
  if ( ! iter->done )
    iter->current = global_iter->Current();
}

/////////////////////////////////////
inline void
LRANGE_NEIGHBOR_ITER_Local_Global_Init( LRANGE_NEIGHBOR_ITER* iter,
                                        GRA_BB*               gbb,
                                        ISA_REGISTER_CLASS    rc )
/////////////////////////////////////
//
//  Prepare <iter> to step the locals in the given <gbb> and <rc>.
//
/////////////////////////////////////
{
  INTERFERE_ITER* bb_iter = &(iter->bb_live_global_iter);

  bb_iter->Init(gbb->Global_Lranges(rc), gbb->Region()->Subuniverse(rc));
  iter->step = LRANGE_NEIGHBOR_ITER_Local_Global_Step;
  iter->done = bb_iter->Done();
  iter->current = bb_iter->Current();
}

/////////////////////////////////////
inline void
Local_Skip_Self( LRANGE* self, LRANGE_NEIGHBOR_ITER* iter )
/////////////////////////////////////
//
//  If the local iterator if <iter> is pointing at <self> advance it.  Leave
//  _done valid, but no need to set _Current
//
/////////////////////////////////////
{
  GRA_BB_LOCAL_LRANGE_ITER* local_iter = &(iter->bb_local_iter);

  if (! local_iter->Done() && local_iter->Current() == self) {
    local_iter->Step();
  }

  iter->done = local_iter->Done();
}


/////////////////////////////////////
static void
LRANGE_NEIGHBOR_ITER_Local_Local_Step( LRANGE_NEIGHBOR_ITER* iter )
/////////////////////////////////////
//
//  Step through the locals for the block of the local on which <iter> was
//  initialized.  When we are done with locals, start stepping the globals
//  that are live in the block and that want the register class.
//
/////////////////////////////////////
{
  GRA_BB_LOCAL_LRANGE_ITER* local_iter = &(iter->bb_local_iter);

  local_iter->Step();
  Local_Skip_Self(iter->u.l.lrange,iter);

  if ( iter->done ) {
    LRANGE_NEIGHBOR_ITER_Local_Global_Init(iter,iter->u.l.lrange->Gbb(),
                                                iter->rc);
  }
  else
    iter->current = local_iter->Current();
}

/////////////////////////////////////
/*ARGSUSED2*/
static void
LRANGE_NEIGHBOR_ITER_Local_Init( LRANGE_NEIGHBOR_ITER*  iter,
                                 LRANGE*                lrange,
                                 GRA_REGION*            region )
/////////////////////////////////////
//
//  Prepare to step over the interference graph neighbors of a local lrange.
//
/////////////////////////////////////
{
  GRA_BB_LOCAL_LRANGE_ITER* local_iter = &(iter->bb_local_iter);
  GRA_BB* gbb = lrange->Gbb();

  iter->rc = lrange->Rc();
  iter->u.l.lrange = lrange;

  local_iter->Init(gbb,iter->rc);
  Local_Skip_Self(lrange,iter);

  if ( local_iter->Done() ) {
    //  No ohter locals 's of the correct register class for this <gbb>.
    //  Perpare to step the globals are live in the block and want the
    //  register class.
    LRANGE_NEIGHBOR_ITER_Local_Global_Init(iter,gbb,iter->rc);
  }
  else {
    iter->step = LRANGE_NEIGHBOR_ITER_Local_Local_Step;
    iter->done = FALSE;
    iter->current = local_iter->Current();
  }
}


/////////////////////////////////////
// Initlalize <iter> to loop over the interference graph
// neighbors of <lrange> which is a LRANGE in the given
// <region>.
void
LRANGE_NEIGHBOR_ITER::Init( LRANGE*      lrange, GRA_REGION*  region )
{
  switch ( lrange->Type() ) {
  case LRANGE_TYPE_COMPLEMENT:
    LRANGE_NEIGHBOR_ITER_Complement_Init(this,lrange,region);
    return;
  case LRANGE_TYPE_REGION:
    LRANGE_NEIGHBOR_ITER_Region_Init(this,lrange,region);
    return;
  case LRANGE_TYPE_LOCAL:
    LRANGE_NEIGHBOR_ITER_Local_Init(this,lrange,region);
    return;
  default:
    FmtAssert(FALSE,("Unknown LRANGE type"));
  }
}

/////////////////////////////////////
// Make both <clist1> and <clist2> be the list
// c00,..,c0n,c10,...,c1m.  Only one of these two
// LRANGE_CLISTs can continue to be valid after this
// operation.  Choose one and discard the other.
void
LRANGE_CLIST::Append( LRANGE_CLIST* clist1 )
{
  if ( first == NULL )
    *this = *clist1;
  else if ( clist1->first == NULL )
    *clist1 = *this;
  else {
    last->clist_next = clist1->first;
    last = clist1->last;
    clist1->first = first;
  }
}

/////////////////////////////////////
// The _Current LRANGE of <iter> is replaced by
// <lrange>.  This means that _Current is spliced out of
// the LRANGE_CLIST on which <iter> was initialized and
// _lrange is spliced into the list in its place.  This
// is used during splitting.  We need to replace the
// split LRANGE with a new LRANGE representing the part
// that can be colored and splice the old LRANGE out of
// the list.  We use the following two functions to put
// it back into the coloring list at the appropriate place.
void
LRANGE_CLIST_ITER::Replace_Current( LRANGE* lrange )
{
  DevAssert(! Done(), ("Trying to splice into a _Done coloring."));
  if ( clist->first == Current() )
    clist->first = lrange;
  if ( clist->last == Current() )
    clist->last = lrange;
  lrange->clist_next = Current()->clist_next;
  prev->clist_next = lrange;
}

/////////////////////////////////////
// Add <new> just after _Current to the LRANGE_CLIST
// associated with <lrange>.  This is used during
// splitting in order to put the deferred part of the
// split LRANGE back into the coloring list for
// appropriate later consideration.
void
LRANGE_CLIST_ITER::Splice( LRANGE* lrange )
{
  DevAssert(! Done(), ("Trying to splice into a _Done coloring."));
  lrange->clist_next = Current()->clist_next;
  Current()->clist_next = lrange;
  if ( lrange->clist_next == NULL )
    clist->last = lrange;
}

#ifdef KEY
// Insert <lrange> before the current element.  If current element is NULL, add
// to end of list.
void
LRANGE_CLIST_ITER::Push( LRANGE* lrange )
{
  if (Done())
    clist->last = lrange;
  if (clist->first == Current() )
    clist->first = lrange;
  lrange->clist_next = Current();
  prev->clist_next = lrange;
}
#endif

/////////////////////////////////////
// Note that a copy between <lrange0> and <lrange1> in <gbb>
// which we can remove if the two LRANGEs are allocated to the
// same register.
void
LRANGE::Preference_Copy( LRANGE* lrange1, GRA_BB* gbb )
{
  LUNIT*    lunit;
  GRA_PREF* pref0 = Pref();
  GRA_PREF* pref1 = lrange1->Pref();
  GRA_PREF* pref_;

 GRA_Trace_Preference_Copy(this,lrange1,gbb);

  pref_priority += gbb->Freq();
  lrange1->pref_priority += gbb->Freq();

  if ( Find_LUNIT_For_GBB(gbb,&lunit) )
    lunit->Preference_Copy(lrange1);

  if ( lrange1->Find_LUNIT_For_GBB(gbb,&lunit) )
    lunit->Preference_Copy(this);

  if ( pref0 != NULL ) {
    if ( pref1 != NULL )
      pref_ = gra_pref_mgr.UnionD(pref0,pref1);
    else
      pref_ = pref0;
  }
  else if ( pref1 != NULL )
    pref_ = pref1;
  else
    pref_ = gra_pref_mgr.Create();

  pref = pref_;
  lrange1->pref = pref_;

  if ( gbb->Region_Is_Complement() ) {
    if ( Type() == LRANGE_TYPE_REGION )
      lrange_mgr.Add_GBB_With_Glue_Reference(this,gbb);
    if ( lrange1->Type() == LRANGE_TYPE_REGION )
      lrange_mgr.Add_GBB_With_Glue_Reference(lrange1,gbb);
  }
}

/////////////////////////////////////
// Use after aplitting a LRANGE to recompute it's preference
// class and _Preference_Priority.
void
LRANGE::Recompute_Preference(void)
{
  LRANGE_LUNIT_ITER iter;
  float priority = 0.0;

  for (iter.Init(this); ! iter.Done(); iter.Step()) {
    priority += iter.Current()->Pref_Priority();
  }

  pref_priority = priority;

  if ( priority == 0.0 )
    pref = NULL;
}


/////////////////////////////////////
//  Format a description of <lrange> into <buff> to be used for tracing 
// preferences or debugging.
char *
LRANGE::Format( char* buff )
{
  INT count;

  switch ( Type() ) {
  case LRANGE_TYPE_LOCAL:
    count = sprintf(buff,"[LRANGE L rc %d  BB:%d", Rc(), BB_id(Gbb()->Bb()));
    break;
  case LRANGE_TYPE_REGION:
    count = sprintf(buff,"[LRANGE R rc %d TN%d", Rc(), TN_number(Tn()));
    break;
  case LRANGE_TYPE_COMPLEMENT:
    count = sprintf(buff,"[LRANGE C rc %d TN%d", Rc(), TN_number(Tn()));
    break;
  default:
    DevWarn("Invalid LRANGE_TYPE");
    return NULL;
  }
  

  if ( Allocated() ) {
    count += sprintf(buff+count," alloc %s",
                                REGISTER_name(Rc(), Reg()));
  }

  if ( Has_Wired_Register() ) {
    count += sprintf(buff+count," wired");
    if ( ! Allocated() ) {
      count += sprintf(buff+count, " %s", REGISTER_name(Rc(),Reg()));
    }
  }

  if ( Tn_Is_Save_Reg() ) 
    count += sprintf(buff+count," save");

  sprintf(buff+count,"]");
  return buff;
}

#ifdef KEY
/////////////////////////////////////
// Add an already-built boundary BB to the boundary BBs list.
void
LRANGE::Boundary_BBs_Push( LRANGE_BOUNDARY_BB *boundary_bb)
{
  boundary_bb->Next_Set(Boundary_BBs());
  Set_Boundary_BBs(boundary_bb);
  // The boundary BB now belongs to this LRANGE.
  boundary_bb->Lrange_Set(this);
}

/////////////////////////////////////
// Build a new boundary BB and add it to the boundary BBs list.
void
LRANGE::Add_Boundary_BB( GRA_BB* gbb)
{
  LRANGE_BOUNDARY_BB *new_boundary_bb =
    (LRANGE_BOUNDARY_BB*) MEM_POOL_Alloc(GRA_pool, sizeof(LRANGE_BOUNDARY_BB));

  new_boundary_bb->Init(gbb, this);
  new_boundary_bb->Next_Set(Boundary_BBs());
  Set_Boundary_BBs(new_boundary_bb);
}

/////////////////////////////////////
// Update the boundary BBs to reflect now live-in and live-out info.
void
LRANGE::Update_Boundary_BBs(void)
{
  LRANGE_BOUNDARY_BB* p;

  for (p = Boundary_BBs(); p != NULL; p = p->Next()) {
    BOOL need_update = FALSE;
    BOOL new_live_in = p->Is_Live_In();
    BOOL new_live_out = p->Is_Live_Out();

    // See if the live-in and live-out info have changed since the boundary BB
    // was created.  If so, update the boundary BB.
    if (new_live_in && !new_live_out) {
      // live-in
      if (!(p->Start_Index() == 0 && p->End_Index() > 0))
        need_update = TRUE;
    } else if (!new_live_in && new_live_out) {
      // live-out
      if (!(p->Start_Index() > 0 && p->End_Index() == 0))
        need_update = TRUE;
    } else if (!new_live_in && !new_live_out) {
      if (p->Start_Index() > 0 || p->End_Index() > 0) {
        // contained
	if (p->Start_Index() == 0 || p->End_Index() == 0)
	  need_update = TRUE;
      } else {
        // empty
      }
    } else {
      // live-in and live-out
      if (p->Start_Index() > 0 || p->End_Index() > 0) {
        // disjoint
	if (p->Start_Index() == 0 || p->End_Index() == 0)
	  need_update = TRUE;
      } else {
        // pass-thru
      }
    }

    // Update boundary BB if needed.
    if (need_update) {
      p->Init(p->Gbb(), this);
    }
  }
}

/////////////////////////////////////
// Find the boundary BB corresponding to <target_bb>.
LRANGE_BOUNDARY_BB*
LRANGE::Get_Boundary_Bb( BB *target_bb )
{
  LRANGE_BOUNDARY_BB* p;
  for (p = Boundary_BBs(); p != NULL; p = p->Next()) {
    if (p->Gbb()->Bb() == target_bb) {
      Is_True(p->Lrange() == this, ("Boundary BB has wrong LRANGE"));
      return p;
    }
  }
  return NULL;
}

/////////////////////////////////////
// Remove the boundary BB corresponding to <target_bb>.
LRANGE_BOUNDARY_BB*
LRANGE::Remove_Boundary_Bb( BB *target_bb )
{
  LRANGE_BOUNDARY_BB *p, *prev;

  p = Boundary_BBs();
  if (p == NULL)
    return NULL;

  // See if the target is the first in the list.
  if (p->Gbb()->Bb() == target_bb) {
    Set_Boundary_BBs(p->Next());
    return p;
  }

  // The target is not the first on the list.
  prev = p;
  p = p->Next();
  for ( ; p != NULL; prev = p, p = p->Next()) {
    if (p->Gbb()->Bb() == target_bb) {
      prev->Next_Set(p->Next());
      return p;
    }
  }
  return NULL;
}

/////////////////////////////////////
// Return TRUE if the lrange is live-in in this boundary BB.
BOOL
LRANGE_BOUNDARY_BB::Is_Live_In(void)
{
  return gbb->Is_Live_In_LRANGE(lrange);
}

/////////////////////////////////////
// Return TRUE if the lrange is live-out in this boundary BB.
BOOL
LRANGE_BOUNDARY_BB::Is_Live_Out(void)
{
  return gbb->Is_Live_Out_LRANGE(lrange);
}

/////////////////////////////////////
// Determine if the LRANGE intefere with another LRANGE inside a BB.  That
// other LRANGE starts at OP index <other_start> and ends at OP index
// <other_end>, and whose live-in and live-out are given by <other_live_in> and
// <other_live_out>.
BOOL
LRANGE_BOUNDARY_BB::Interfere (BOOL other_live_in, BOOL other_live_out,
			       mUINT16 other_start, mUINT16 other_end)
{
// Return TRUE if a <= x <= b.
#define IN_BETWEEN(x, a, b)	(((a) <= (x)) && ((x) <= (b)))

  const mUINT16 max_index = 0xffff;
  mUINT16 this_start, this_end, other_start1, other_end1;
  BOOL other_is_disjoint = FALSE;

  // No interference if other live range is empty.
  if (!other_live_in && !other_live_out && other_start == 0) {
    Is_True(other_end == 0, ("Bad OP end index in empty live range"));
    return FALSE;
  }

  BOOL this_live_in = Is_Live_In();
  BOOL this_live_out = Is_Live_Out();

  // TODO:  Skip disjoint live ranges for now.  If a boundary lrange is live-in
  // and live-out, consider it pass-thru and thus interfere.
  if (this_live_in && this_live_out) {
    return TRUE;
  }

  // TODO:  We do encounter boundary BBs where the lrange does not appear in
  // the BB.  Originally this function returns FALSE (no interference) for such
  // lrange, but this caused GRA_BB::Make_Register_Used to abort when it
  // detects that the chosen register is already used in the BB.  Decide what
  // to do with such lrange; for now return TRUE.
  // (We do see empty live ranges that are live-in/live-out.)
  if ((start_index == 0 && end_index == 0) &&
      ((!this_live_in && !this_live_out) ||
       (this_live_in && !this_live_out) ||
       (!this_live_in && this_live_out))) {
    // This live range is empty.
    return TRUE;
  }

  // Check for other being pass-thru or disjoint.
  if (other_live_in && other_live_out) {
    if (other_start == 0) {
      // Other is pass-thru.
      Is_True(other_end == 0, ("pass-thru live range cannot have last-use"));
      return TRUE;
    } else if (other_start == other_end) {
      // Other is disjoint but its last-use and the def are in the same insn.
      // This makes the live range look like a pass-thru.
      return TRUE;
    } else {
      Is_True(other_start > other_end, ("disjoint live range corrupted"));
      // Separate out the two live range segments for easier comparison.
      other_is_disjoint = TRUE;
      other_start1 = other_start;
      other_end1 = max_index;
      other_start = 0;
    }
  }

  // Set up the segments for comparision.
  // At this point, this live range is either live-in, live-out, or contained.
  this_start = start_index;
  this_end = this_live_out ? max_index : end_index;

  // The other live range is either live-in, live-out, contained, or has two
  // disjoint segments which we already set up.
  if (!other_is_disjoint && other_live_out) {
    other_end = max_index;
  }

  // Compare the segments.
  if (IN_BETWEEN(this_start, other_start, other_end) ||
      IN_BETWEEN(this_end, other_start, other_end) ||
      IN_BETWEEN(other_start, this_start, this_end))
    return TRUE;

  // Compare the second segment if other is disjoint.
  if (other_is_disjoint) {
    if (IN_BETWEEN(this_start, other_start1, other_end1) ||
	IN_BETWEEN(this_end, other_start1, other_end1) ||
	IN_BETWEEN(other_start1, this_start, this_end))
      return TRUE;
  }

  // No interference.
  return FALSE;
}

/////////////////////////////////////
// Determine if two LRANGEs interefere in a boundary bb.
BOOL
LRANGE_BOUNDARY_BB::Interfere (LRANGE_BOUNDARY_BB *bb1)
{
  LRANGE_BOUNDARY_BB *live_in, *live_out;

  Is_True(lrange->Type() == LRANGE_TYPE_COMPLEMENT, 
	  ("Not a complement LRANGE"));
  Is_True(bb1->Lrange()->Type() == LRANGE_TYPE_COMPLEMENT, 
	  ("Not a complement LRANGE"));
  Is_True(gbb == bb1->Gbb(),
     ("Cannot check boundary interference between two different boundary BBs"));
  
  BOOL bb1_live_in = (bb1->start_index == 0 && bb1->end_index > 0);
  BOOL bb1_live_out = (bb1->start_index > 0 && bb1->end_index == 0);

  return Interfere(bb1_live_in, bb1_live_out, bb1->start_index, bb1->end_index);
}

/////////////////////////////////////
// Determine if LRANGE interfere with the used registers in <gbb>.
BOOL
LRANGE_BOUNDARY_BB::Interfere (GRA_BB *gbb, ISA_REGISTER_CLASS rc, REGISTER reg)
{
  Is_True(lrange->Type() == LRANGE_TYPE_COMPLEMENT, 
	  ("Not a complement LRANGE"));
  return Interfere(gbb->Is_Usage_Live_In(rc, reg),
		   gbb->Is_Usage_Live_Out(rc, reg),
		   gbb->Usage_Start_Index(rc, reg),
		   gbb->Usage_End_Index(rc, reg));
}

/////////////////////////////////////
// Init a boundary BB.  Find the shape of the lrange in the BB.
void
LRANGE_BOUNDARY_BB::Init (GRA_BB *the_gbb, LRANGE *the_lrange)
{
  OP *op;
  mUINT16 op_index, first_ref_index, last_ref_index, *index_ptr;
  BOOL live_in, live_out;
  int i, pass;

  // If TRUE, find the first and last OP that reference TN, respectively.
  BOOL find_first_ref = FALSE;
  BOOL find_last_ref = FALSE;

  gbb = the_gbb;
  lrange = the_lrange;
  TN *tn = lrange->Tn();
  BB *bb = gbb->Bb();
  LUNIT* lunit = gbb_mgr.Split_LUNIT(gbb);

  Is_True(! lrange->Contains_Internal_BB(gbb), ("BB is not a boundary BB"));

  live_in = Is_Live_In();
  live_out = Is_Live_Out();

  // A live-in boundary BB could have been marked live-out by
  // Fix_TN_Live_Out_Info, resulting in both live-in and live-out.
  if (live_in && live_out) {
    // TODO:  Handle the "disjoint" case.  For now, assume live range is
    // pass-thru.
    start_index = 0;
    end_index = 0;
    return;
  } else if (live_in) {
    find_last_ref = TRUE;
  } else if (live_out) {
    find_first_ref = TRUE;
  } else {
    // contained or empty live range
    find_first_ref = TRUE;
    find_last_ref = TRUE;
  }

  // Pass 0 finds the first OP that references the TN.  Pass 1 finds the last
  // OP that references the TN.
  first_ref_index = last_ref_index = 0;
  for (pass = 0; pass < 2; pass++) {
    if (pass == 0) {
      if (!find_first_ref)
	continue;
      op_index = 0;
      op = BB_first_op(bb);
      index_ptr = &first_ref_index;
    } else {
      if (!find_last_ref)
	break;
      op_index = gbb->OPs_Count() + 1;
      op = BB_last_op(bb);
      index_ptr = &last_ref_index;
    }

    while (op != NULL) {
      // First OP in BB has index 1.
      op_index = (pass == 0) ? op_index+1 : op_index-1;

      // Analyze the uses.
      for (i = OP_opnds(op) - 1; i >= 0; i--) {
	TN *opnd_tn = OP_opnd(op, i);
	if (opnd_tn == tn) {
	  *index_ptr = op_index;
	  break;
	}
      }
      if (*index_ptr)	// Quit if found reference.
        break;

      // Analyze the defines.
      for (i = OP_results(op) - 1; i >= 0; i--) {
	TN *result_tn = OP_result(op, i);
	if (result_tn == tn) {
	  *index_ptr = op_index;
	  break;
	}
      }
      if (*index_ptr)	// Quit if found reference.
        break;

      // Next OP.
      op = (pass == 0) ? OP_next(op) : OP_prev(op);
    }

    // Skip the backward traversal if we know TN isn't referenced in the BB.
    if (*index_ptr == 0)
      break;
  }

  // TODO:  The "disjoint" case not yet implemented.
  Is_True(! (live_in && live_out),
	  ("Live-in and live-out boundary BB currently not handled"));

  if (live_in) {
    Is_True(last_ref_index > 0 || (lunit && lunit->Spill_Below()),
	    ("Bad OP index in live-in BB"));
    start_index = 0;
    // last_ref_index is 0 when the TN does not appear in any of the BB's OPs.
    // This happens if the TN is "spill below" inserted by GRA.
    end_index = last_ref_index;
  } else if (live_out) {
    Is_True(first_ref_index > 0 || (lunit && lunit->Restore_Above()),
	    ("Bad OP index in live-out BB"));
    // first_ref_index is 0 when the TN does not appear in any of the BB's OPs.
    // This happens if the TN is "restore above" inserted by GRA.
    start_index = first_ref_index;
    end_index = 0;
  } else {
    // Contained or empty live range.  For empty, first_ref_index and
    // last_ref_index are both 0.  This can happen if the BB is at the edge of
    // the live range created by splitting.
    Is_True((first_ref_index > 0 && last_ref_index > 0) ||
	    (first_ref_index == 0 && last_ref_index == 0),
	    ("Bad OP index for contained or empty live range."));
    start_index = first_ref_index;
    end_index = last_ref_index;
  }
}
#endif
