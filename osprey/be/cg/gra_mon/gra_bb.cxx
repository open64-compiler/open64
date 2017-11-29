/*
 * Copyright 2006, 2007.  QLogic Corporation.  All Rights Reserved.
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

// GRA_BB implementation functions
/////////////////////////////////////
//
//  Description:
//
//      Whatever wasn't inline ended up here.
//
/////////////////////////////////////


//  $Revision: 1.15 $
//  $Date: 05/06/01 16:54:32-07:00 $
//  $Author: tkong@hyalite.keyresearch $
//  $Source: be/cg/gra_mon/SCCS/s.gra_bb.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: be/cg/gra_mon/SCCS/s.gra_bb.cxx $ $Revision: 1.15 $";
#endif

#include "defs.h"
#include "cg.h"
#include "cg_region.h"
#include "mempool.h"
#include "cgir.h"
#include "bb_set.h"
#include "gra_live.h"
#include "gra_bb.h"
#include "gra_lrange.h"
#include "gra_loop.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "gra_region.h"
#include "util.h"
#include "mempool.h"
#include "lra.h"
#include "gra.h"
#include "gra_interfere.h"
#include "gra_trace.h"

GBB_MGR gbb_mgr;
#ifdef TARG_X8664
OP *gra_savexmms_op;
#endif


/////////////////////////////////////
INT 
GRA_BB::Register_Girth( ISA_REGISTER_CLASS rc ) 
{
  INT rr = LRA_Register_Request(bb,rc);
  INT rs = REGISTER_SET_Size(REGISTER_CLASS_allocatable(rc));
#ifndef TARG_X8664  // Don't understand why we should inflate register request.
  if ( rr < GRA_LOCAL_FORCED_MAX(rc) && rr != 0 )
    rr = GRA_LOCAL_FORCED_MAX(rc);
#endif
  return rr <= rs ? rr : rs; 
}

/////////////////////////////////////
// Does this BB come from the complement region?
BOOL
GRA_BB::Region_Is_Complement(void)
{
  return region == gra_region_mgr.Complement_Region();
}

/////////////////////////////////////
// Add <lunit> to <gbb>.  See the iterator GRA_BB_RC_LUNIT_ITER.
void
GRA_BB::Add_LUNIT( LUNIT*  lunit)
{
  ISA_REGISTER_CLASS rc = lunit->Lrange()->Rc();
  lunits[rc] = lunits[rc]->BB_List_Push(lunit);
}

/////////////////////////////////////
// Add <reg> to the set of registers used in the given <gbb> and <rc>.
void
GRA_BB::Make_Register_Used( ISA_REGISTER_CLASS  rc, REGISTER reg,
			    LRANGE* lrange, BOOL reclaim )
{
  region-> Make_Register_Used(rc,reg);
  loop->Make_Register_Used(rc, reg, reclaim);
  registers_used[rc] = REGISTER_SET_Union1(registers_used[rc],reg);

#ifdef KEY
  // Record that REG is owned by LRANGE.
  if (GRA_reclaim_register)
    Set_LRANGE_Owner(rc, reg, lrange);

  if (! GRA_optimize_boundary)
    return;

  // If reclaiming a register, reset its boundary usage info.
  if (reclaim) {
    Usage_Start_Index_Set(rc, reg, 0);
    Usage_End_Index_Set(rc, reg, 0);
    Usage_Live_In_Clear(rc, reg);
    Usage_Live_Out_Clear(rc, reg);
  }

  const mUINT16 max_OP_index = 0xffff;

  // TODO:  Find the exact locations in the BB where the local TN is live.
  // For now mark the register used over the entire BB.  (This may be
  // difficult since these include the GRA grants.)
  if (lrange == NULL) {		// NULL means local LRANGE
    Usage_Start_Index_Set(rc, reg, 1);
    Usage_End_Index_Set(rc, reg, max_OP_index);
    return;
  }

  // Fill in detailed info about register usage in this BB.  If the register
  // isn't used thoughout the BB, it is available to hold other LRANGEs.

  if (lrange->Contains_Internal_BB(this)) {
    // Internal BB.  Make the register used throughout the BB.
    Usage_Live_In_Set(rc, reg);
    Usage_Live_Out_Set(rc, reg);
    Usage_Start_Index_Set(rc, reg, 0);
    Usage_End_Index_Set(rc, reg, 0);
  } else {
    // Boundary BB.  Combine the usage info from this BB and the boundary BB.

    // Describe a start or end index for merging the usages.
    typedef struct {
      mUINT16	index;
      BOOL	is_start_index;
    } usage;

    usage this_usage[4];	// Describe the usage in this BB before merging.
				// Two usage segments (and hence 4 indices) are
				// possible due to the "disjoint" case.
    int usage_segments = 0;	// Number of usage segments originally in this
				// BB.

    usage boundary_usage[2];	// The usage in the boundary BB.
    usage combined_usage[6];	// The merged usage.

    BOOL is_preferenced = FALSE;

#ifdef Is_True_On
    LUNIT* lunit;
    Is_True(lrange->Find_LUNIT_For_GBB(this, &lunit),
	    ("Make_Register_Used: cannot find LUNIT"));
    REGISTER_SET allowed_prefs =
	  REGISTER_SET_Union(lunit->Allowed_Preferences(),
			     Global_Preferenced_Regs(lrange, this));
    is_preferenced = REGISTER_SET_MemberP(allowed_prefs, reg);
#endif

    // Handle BB with no OP.  Treat the usage like a pass-thru.
    if (BB_first_op(Bb()) == NULL) {
      Usage_Live_In_Set(rc, reg);
      Usage_Live_Out_Set(rc, reg);
      Usage_Start_Index_Set(rc, reg, 0);
      Usage_End_Index_Set(rc, reg, 0);
      return;
    }

    // Prepare the start and end indices for merging.  Starts with this BB.
    mUINT16 this_usage_start_index = Usage_Start_Index(rc, reg);
    mUINT16 this_usage_end_index = Usage_End_Index(rc, reg);
    if (Is_Usage_Live_In(rc, reg) && !Is_Usage_Live_Out(rc, reg)) {
      // live-in
      Is_True(this_usage_start_index == 0 && this_usage_end_index > 0,
		("Bad usage index for live-in live range"));
      this_usage[0].index = 0;
      this_usage[0].is_start_index = TRUE;
      this_usage[1].index = this_usage_end_index;
      this_usage[1].is_start_index = FALSE;
      usage_segments = 1;
    } else if (!Is_Usage_Live_In(rc, reg) && Is_Usage_Live_Out(rc, reg)) {
      // live-out
      Is_True(this_usage_start_index > 0 && this_usage_end_index == 0,
		("Bad usage index for live-out live range"));
      this_usage[0].index = this_usage_start_index;
      this_usage[0].is_start_index = TRUE;
      this_usage[1].index = max_OP_index;
      this_usage[1].is_start_index = FALSE;
      usage_segments = 1;
    } else if (Is_Usage_Live_In(rc, reg) && Is_Usage_Live_Out(rc, reg)) {
      if (this_usage_start_index > 0) {
	// disjoint
	Is_True(this_usage_end_index > 0 &&
		this_usage_start_index >= this_usage_end_index,
		("Bad usage index for disjoint live range"));
	this_usage[0].index = 0;
	this_usage[0].is_start_index = TRUE;
	this_usage[1].index = this_usage_end_index;
	this_usage[1].is_start_index = FALSE;

	this_usage[2].index = this_usage_start_index;
	this_usage[2].is_start_index = TRUE;
	this_usage[3].index = max_OP_index;
	this_usage[3].is_start_index = FALSE;
	usage_segments = 2;
      } else {
	// pass-thru
	Is_True(this_usage_end_index == 0,
		("Bad usage index for pass-thru live range"));
	// Register is already used.  Can allocated to the register only if the
	// lrange is preferenced to the register.
	Is_True(is_preferenced,
		("Make_Register_Used: register is already used"));
	return;
      }
    } else if (!Is_Usage_Live_In(rc, reg) && !Is_Usage_Live_Out(rc, reg)) {
      if (this_usage_start_index > 0) {
	// contained
	Is_True(this_usage_end_index > 0 &&
		this_usage_start_index <= this_usage_end_index,
		("Bad usage index for contained live range"));
	this_usage[0].index = this_usage_start_index;
	this_usage[0].is_start_index = TRUE;
	this_usage[1].index = this_usage_end_index;
	this_usage[1].is_start_index = FALSE;
	usage_segments = 1;
      } else {
	// empty.  Live range doesn't appear in BB.  Do nothing.
      }
    }

    // Prepare the indices for the boundary BB.
    LRANGE_BOUNDARY_BB* boundary_bb = lrange->Get_Boundary_Bb(Bb());
    if (boundary_bb->Is_Live_In() && !boundary_bb->Is_Live_Out()) {
      // live-in
      boundary_usage[0].index = 0;
      boundary_usage[0].is_start_index = TRUE;
      if (boundary_bb->End_Index() == 0) {
	// TN is spill below.  The register is used just until the first
	// OP.  To indicate this, reserve the register at the first OP.
	boundary_usage[1].index = 1;
      } else {
	boundary_usage[1].index = boundary_bb->End_Index();
      }
      Is_True(boundary_usage[1].index > 0, ("Bad OP index in live-in"));
      boundary_usage[1].is_start_index = FALSE;
    } else if (!boundary_bb->Is_Live_In() && boundary_bb->Is_Live_Out()) {
      // live-out
      if (boundary_bb->Start_Index() == 0) {
	// TN is restore above.  Reserve the register before the last OP
	// because we insert the restore either before or after the last OP,
	// depending on whether the last OP is a branch.
	boundary_usage[0].index = boundary_bb->Gbb()->OPs_Count();
      } else {
        boundary_usage[0].index = boundary_bb->Start_Index();
      }
      Is_True(boundary_usage[0].index > 0, ("Bad OP index in live-out"));
      boundary_usage[0].is_start_index = TRUE;
      boundary_usage[1].index = max_OP_index;
      boundary_usage[1].is_start_index = FALSE;
    } else if (!boundary_bb->Is_Live_In() && !boundary_bb->Is_Live_Out()) {
      if (boundary_bb->Start_Index() > 0) {
        Is_True(boundary_bb->End_Index() >= boundary_bb->Start_Index(),
		("Bad usage index for contained live range"));
	boundary_usage[0].index = boundary_bb->Start_Index();
	boundary_usage[0].is_start_index = TRUE;
	boundary_usage[1].index = boundary_bb->End_Index();
	boundary_usage[1].is_start_index = FALSE;
      } else {
	// empty.  Live range doesn't appear in BB.  Nothing to merge.
        Is_True(boundary_bb->End_Index() == 0,
		("Bad usage index for empty live range"));
	return;
      }
    } else {
      // live-in and live-out.
#ifdef Is_True_On
      // We could just reserve the register over the entire BB and we're done.
      // Instead, we let the rest of the code run to get more error checking.
      boundary_usage[0].index = 1;
      boundary_usage[0].is_start_index = TRUE;
      boundary_usage[1].index = max_OP_index;
      boundary_usage[1].is_start_index = FALSE;
#else
      // Reserve the register over the entire BB.
      Usage_Live_In_Set(rc, reg);
      Usage_Live_Out_Set(rc, reg);
      Usage_Start_Index_Set(rc, reg, 0);
      Usage_End_Index_Set(rc, reg, 0);
      return;
#endif
    }

    // Merge the two usages.  Order by OP index.
    int combined_i_max = 0;
    {
      int this_i, boundary_i, combined_i;
      int this_i_max = usage_segments * 2;
      int boundary_i_max = 2;

      this_i = boundary_i = combined_i = 0;
      for ( ; this_i < this_i_max && boundary_i < boundary_i_max; ) {
	if (this_usage[this_i].index < boundary_usage[boundary_i].index)
	  combined_usage[combined_i++] = this_usage[this_i++];
	else if (this_usage[this_i].index > boundary_usage[boundary_i].index)
	  combined_usage[combined_i++] = boundary_usage[boundary_i++];
	else {
	  // this_usage[this_i].index == boundary_usage[boundary_i].index.
	  // In this case, accept the end index first, since one usage must end
	  // before the other one starts.
	  if (this_usage[this_i].is_start_index) {
	    combined_usage[combined_i++] = boundary_usage[boundary_i++];
	    combined_usage[combined_i++] = this_usage[this_i++];
	  } else {
	    combined_usage[combined_i++] = this_usage[this_i++];
	    combined_usage[combined_i++] = boundary_usage[boundary_i++];
	  }
	}
      }

      // Done merging in either this_usage or boundary_usage.  Merge in the
      // left overs.
      while (this_i < this_i_max) {
	combined_usage[combined_i++] = this_usage[this_i++];
      }
      while (boundary_i < boundary_i_max) {
	combined_usage[combined_i++] = boundary_usage[boundary_i++];
      }
      combined_i_max = combined_i;

#ifdef Is_True_On
      Is_True(combined_i_max % 2 == 0,
	      ("Make_Register_Used: error in combined usage"));
      // Verify each usage segment is bounded by a start index and an end
      // index, except when the register is preferenced, in which case the
      // usages can overlap.
      if (!is_preferenced) {
	for (combined_i = 0; combined_i < combined_i_max; combined_i++) {
	  // Even means start of segment.  Odd means end of segment.
	  Is_True(combined_usage[combined_i].is_start_index ==
		  (combined_i % 2 == 0),
		  ("Make_Register_Used: error in combined usage"));
	}
      }
#endif
    }

    // Parse the merged usage to see what the new usage looks like.
    {
      BOOL live_in = FALSE;
      BOOL live_out = FALSE;
      mUINT16 start_index = 0;
      mUINT16 end_index = 0;

      if (combined_i_max == 0) {
	// empty
	Is_True(Usage_Start_Index(rc, reg) == 0 &&
		Usage_End_Index(rc, reg) == 0 &&
		!Is_Usage_Live_Out(rc, reg) &&
		!Is_Usage_Live_In(rc, reg),
		("Make_Register_Used: error in combined usage"));
      } else {
	// Detect live-in and live-out.
	if (combined_usage[0].index == 0)
	  live_in = TRUE;
	if (combined_usage[combined_i_max-1].index == max_OP_index)
	  live_out = TRUE;

	// Handle the cases.
	if (live_in && !live_out) {
	  start_index = 0;
	  end_index = combined_usage[combined_i_max-1].index;
	} else if (!live_in && live_out) {
	  start_index = combined_usage[0].index;
	  end_index = 0;
	} else if (live_in && live_out) {
	  // There are at most 2 gaps.
	  if (combined_i_max == 6) {
	    // There are 2 gaps.  Pick the largest gap in order to free up the
	    // register as much as possible.
	    int gap1_size = combined_usage[2].index - combined_usage[1].index;
	    int gap2_size = combined_usage[4].index - combined_usage[3].index;
	    if (gap1_size > gap2_size) {
	      start_index = combined_usage[2].index;
	      end_index = combined_usage[1].index;
	    } else {
	      start_index = combined_usage[4].index;
	      end_index = combined_usage[3].index;
	    }
	  } else if (combined_i_max == 4) {
	    // Only one gap.
	    start_index = combined_usage[2].index;
	    end_index = combined_usage[1].index;
	  } else {
	    Is_True(FALSE,
		    ("Make_Register_Used: error in combined usage"));
	  }
	} else {
	  // !live_in && !live_out
	  start_index = combined_usage[0].index;
	  end_index = combined_usage[combined_i_max-1].index;
	}
      }
      if (live_in)
        Usage_Live_In_Set(rc, reg);
      if (live_out)
        Usage_Live_Out_Set(rc, reg);
      Usage_Start_Index_Set(rc, reg, start_index);
      Usage_End_Index_Set(rc, reg, end_index);
    }
  }
#endif
}


/////////////////////////////////////
REGISTER_SET
GRA_BB::Registers_Used( ISA_REGISTER_CLASS  rc)
{
  if ( region == gra_region_mgr.Complement_Region() )
    return registers_used[rc];
  else {
    REGISTER_SET used = region->Registers_Used(rc);

    if ( region->Has_Call() )
      return REGISTER_SET_Union(used,REGISTER_CLASS_caller_saves(rc));
    else
      return used;
  }
}

#ifdef KEY
/////////////////////////////////////
// Add <reg> to the set of registers referenced in the given <gbb> and <rc>.
void
GRA_BB::Make_Register_Referenced (ISA_REGISTER_CLASS rc, REGISTER reg,
				  LRANGE* lrange )
{
  registers_referenced[rc] = REGISTER_SET_Union1(registers_referenced[rc],reg);
}

/////////////////////////////////////
REGISTER_SET
GRA_BB::Registers_Referenced( ISA_REGISTER_CLASS rc)
{
  FmtAssert(region == gra_region_mgr.Complement_Region(),
	    ("Registers_Referenced: lrange type not implemented"));
  return registers_referenced[rc];
}
#endif

/////////////////////////////////////
// Check if <lrange> in the set of LRANGEs to be spilled above <gbb>.
BOOL
GRA_BB::Spill_Above_Check( LRANGE* lrange )
{
  ISA_REGISTER_CLASS rc = lrange->Rc();
  if (spill_above[rc]) {
    LRANGE_SUBUNIVERSE *su = region->Subuniverse(rc);
    return(LRANGE_SET_MemberPS(spill_above[rc], lrange, su));
  }
  return FALSE;
}

/////////////////////////////////////
// Add <lrange> to the set of LRANGEs to be spilled above <gbb>.
void
GRA_BB::Spill_Above_Set( LRANGE* lr )
{
  ISA_REGISTER_CLASS rc = lr->Rc();
  LRANGE_SUBUNIVERSE *su = region->Subuniverse(rc);
  INT size = su->Count();

  if (!spill_above[rc]) {
    spill_above[rc] = LRANGE_SET_Create_Empty(size, GRA_pool);
  }
  spill_above[rc] = LRANGE_SET_Union1S(spill_above[rc], lr, GRA_pool, su);
}

/////////////////////////////////////
// Remove <lrange> from the set of LRANGEs to be spilled above <gbb>.
void
GRA_BB::Spill_Above_Reset( LRANGE* lr )
{
  ISA_REGISTER_CLASS rc = lr->Rc();
  LRANGE_SUBUNIVERSE *su = region->Subuniverse(rc);

  spill_above[rc] = LRANGE_SET_Difference1DS(spill_above[rc], lr, su);
}


/////////////////////////////////////
// Check if <lrange> in the set of LRANGEs to be restored at the bottom of <gbb>
BOOL
GRA_BB::Restore_Below_Check( LRANGE* lr )
{
  ISA_REGISTER_CLASS rc = lr->Rc();
  if (restore_below[rc]) {
    LRANGE_SUBUNIVERSE *su = region->Subuniverse(rc);
    return(LRANGE_SET_MemberPS(restore_below[rc], lr, su));
  }
  return FALSE;
}

/////////////////////////////////////
// Add <lrange> to the set of LRANGEs to be restored at the bottom of <gbb>.
void
GRA_BB::Restore_Below_Set( LRANGE* lr )
{
  ISA_REGISTER_CLASS rc = lr->Rc();
  LRANGE_SUBUNIVERSE *su = region->Subuniverse(rc);
  INT size = su->Count();

  if (!restore_below[rc]) {
    restore_below[rc] = LRANGE_SET_Create_Empty(size, GRA_pool);
  }
  restore_below[rc] = LRANGE_SET_Union1S(restore_below[rc], lr,
					    GRA_pool, su);
}

/////////////////////////////////////
// Remove <lrange> from the set of LRANGEs to be restored at the
// bottom of <gbb>.
void
GRA_BB::Restore_Below_Reset( LRANGE* lr )
{
  ISA_REGISTER_CLASS rc = lr->Rc();
  LRANGE_SUBUNIVERSE *su = region->Subuniverse(rc);

  restore_below[rc] = LRANGE_SET_Difference1DS(restore_below[rc], lr, su);
}

/////////////////////////////////////
// Called at the start of each GRA invocation.
void
GBB_MGR::Initialize(void)
{
  split_mark_counter = 0;
  one_set_counter = 0;
  wired_local_count = 0;
  alloc_count = 0;
  map = BB_MAP_Create();
  blocks_with_calls = BB_SET_Create_Empty(PU_BB_Count + 2,GRA_pool);
  blocks_with_rot_reg_clob = BB_SET_Create_Empty(PU_BB_Count + 2,GRA_pool);
#ifdef TARG_X8664
  blocks_with_x87_OP = BB_SET_Create_Empty(PU_BB_Count + 2,GRA_pool);
  blocks_with_mmx_OP = BB_SET_Create_Empty(PU_BB_Count + 2,GRA_pool);
#endif
}


/////////////////////////////////////
// Called at the end of each GRA invocation.
void
GBB_MGR::Finalize(void)
{
  BB_MAP_Delete(map);
  GRA_Trace_Wired_Local_Count(wired_local_count);
}


/////////////////////////////////////
// Create and return a new GRA_BB to encapsulate <gbb> a
// GRA_BB associated with the given region.
GRA_BB*
GBB_MGR::Create(BB* bb, GRA_REGION* region)
{
  ISA_REGISTER_CLASS rc;
  GRA_BB* gbb = TYPE_MEM_POOL_ALLOC(GRA_BB,GRA_pool);

  gbb->bb = bb;
  gbb->region = region;
  gbb->split_mark = -1;
  gbb->one_set_mark = -1;
  gbb->Clear_Flags();
  alloc_count++;

  //  Previously allocated regions use fine grained interference rules and
  //  the blocks play a much smaller role.

  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    gbb->registers_used[rc] = REGISTER_SET_EMPTY_SET;
    gbb->glue_registers_used[rc] = REGISTER_SET_EMPTY_SET;
    gbb->lunits[rc] = NULL;
    gbb->local_lrange_count[rc] = 0;
    gbb->local_lranges[rc] = NULL;
    gbb->unpreferenced_wired_lranges[rc] = NULL;
    gbb->spill_above[rc] = NULL;
    gbb->restore_below[rc] = NULL;
#ifdef KEY
    if (GRA_optimize_boundary) {
      gbb->usage_live_in[rc] = REGISTER_SET_EMPTY_SET;
      gbb->usage_live_out[rc] = REGISTER_SET_EMPTY_SET;
      for (int i = 0; i < REGISTER_MAX+1; i++) {
	gbb->usage_start_index[rc][i] = 0;
	gbb->usage_end_index[rc][i] = 0;
      }
    }
    if (GRA_reclaim_register) {
      for (int i = 0; i < REGISTER_MAX+1; i++) {
	gbb->lrange_owner[rc][i] = NULL;
      }
    }
#endif
  }

  BB_MAP_Set(map,bb,(void*) gbb);
  if ( BB_call(bb) ) {
    blocks_with_calls = BB_SET_Union1D(blocks_with_calls,bb,GRA_pool);

    // if call is to setjmp, set setjmp flag in gbb so that live ranges over 
    // it will never be allocated to a callee-saved register; use same means
    // for recognizing setjmp call as in Can_Do_Tail_Calls_For_PU()
    ANNOTATION *callant = ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO);
    CALLINFO *callinfo = ANNOT_callinfo(callant);
    ST *st = CALLINFO_call_st(callinfo);
    if (st != NULL) {
      const char *name = ST_name(st);
      INT len = strlen(name);
      if ((len >= 6 && strcmp(name + len - 6, "setjmp") == 0) ||
	  (len >= 10 && strcmp(name+len-10, "getcontext") == 0)) 
	gbb->Setjmp_Set();
    }
  }
  if (BB_mod_rotating_registers(bb) || BB_mod_pred_rotating_registers(bb))
    blocks_with_rot_reg_clob = BB_SET_Union1D(blocks_with_rot_reg_clob,bb,GRA_pool);
#ifdef TARG_X8664
  if (BB_last_op(bb) && OP_code(BB_last_op(bb)) == TOP_savexmms) {
    gbb->Savexmms_Set();
    gra_savexmms_op = BB_last_op(bb);
  }

  mUINT16 OPs_count = 0;
  for (OP *op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
    OPs_count++;		// Count the number of OPs in the BB.
    if (OP_x87(op))
      gbb->x87_OP_Set();
    else if (OP_mmx(op))
      gbb->mmx_OP_Set();
  }
  gbb->OPs_count = OPs_count;
  if (gbb->x87_OP())
    blocks_with_x87_OP = BB_SET_Union1D(blocks_with_x87_OP, bb, GRA_pool);
  if (gbb->mmx_OP())
    blocks_with_mmx_OP = BB_SET_Union1D(blocks_with_mmx_OP, bb, GRA_pool);
#endif

  return gbb;
}


/////////////////////////////////////
// Create <count> local LRANGEs for registers in <gbb> and
// the given REGISTER_CLASS, <cl>.  See the iterator type
// GRA_BB_RC_LOCAL_LRANGE_ITER, defined below.
void
GRA_BB::Create_Local_LRANGEs(ISA_REGISTER_CLASS rc, INT32 count)
{
  // As a compile speed hack, we'll preallocate the first few locals that are
  // requested.  So they won't need live ranges.  See gra_color for the actual
  // preallocateion.
  //
  count -= GRA_LOCAL_FORCED_MAX(rc);
  if ( count <= 0 )
    return;

  local_lrange_count[rc] += count;

  while ( count-- > 0 ) {
    LRANGE* new_local = lrange_mgr.Create_Local(this,rc);

    local_lranges[rc] =
      local_lranges[rc]->BB_Local_List_Push(new_local);
  }
}


/////////////////////////////////////
// Create and return a new local LRANGE for <gbb> that must be
// allocated the register -- <cl> and <reg>.
LRANGE*
GRA_BB::Create_Wired_LRANGE(ISA_REGISTER_CLASS  rc, REGISTER reg)
{
  LRANGE* result = lrange_mgr.Create_Local(this,rc);

  gbb_mgr.Incr_Wired_Local_Count();
  result->Wire_Register(reg);
  return result;
}

/////////////////////////////////////
// Compute which global (complement) LRANGEs pass through
// this 'gbb'. Save this information away as space effeciently
// as possible.  Must not be called until after after a
// LRANGE for each GTN live in <gbb> has been created.
void
GRA_BB::Create_Global_Interferences(void)
{
  TN* tn;
  ISA_REGISTER_CLASS rc;
  GTN_SET* live;

  MEM_POOL_Push(&MEM_local_nz_pool);

  //  Keeps it from having to be copied and keeps it small:
  live = GTN_SET_Create(Max(GTN_SET_Alloc_Size(BB_live_in(bb)),
                            GTN_SET_Alloc_Size(BB_live_out(bb))),
                        &MEM_local_nz_pool);

  //  Calculate the set of GTNs live in this.  This assumes that any purely
  //  local references have been renamed are are no longer part of any
  //  global live range.  It would be good to check for this somewhere.
  live = GTN_SET_CopyD(live,BB_live_in(bb),&MEM_local_nz_pool);
  live = GTN_SET_IntersectionD(live,BB_defreach_in(bb));
  live = GTN_SET_UnionD(live,BB_live_out(bb),&MEM_local_nz_pool);
  live = GTN_SET_IntersectionD(live,BB_defreach_out(bb));

  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    intf_mgr.Create_Begin(region->Subuniverse(rc));

    for ( tn = GTN_SET_Choose(live);
          tn != GTN_SET_CHOOSE_FAILURE;
          tn = GTN_SET_Choose_Next(live,tn)
    ) {
      if ( TN_register_class(tn) == rc && TN_Is_Allocatable(tn) ) {
        LRANGE* lrange = lrange_mgr.Get(tn);

        if ( lrange->Type() == LRANGE_TYPE_COMPLEMENT ) {

          //  Might not pass our definition of what's a register TN.
          if ( lrange != NULL ) {
            intf_mgr.Create_Add_Neighbor(lrange_mgr.Get(tn));
            GTN_SET_Difference1D(live,tn);
          }
        }
      }
    }
    global_lranges[rc] = intf_mgr.Create_End();
  }

  MEM_POOL_Pop(&MEM_local_nz_pool);
}

/////////////////////////////////////
// Replace <old_lr> with <new_lr> in the set of complement
// LRANGEs that are live in gbb.
void
GRA_BB::Replace_Global_Interference(LRANGE* old_lr, LRANGE* new_lr )
{
  ISA_REGISTER_CLASS rc = old_lr->Rc();

  if (! Region_Is_Complement())
    return;

  DevAssert(old_lr->Type() == LRANGE_TYPE_COMPLEMENT,
            ("Replacing non complement type in a BB's global interference"));
  DevAssert(new_lr->Type() == LRANGE_TYPE_COMPLEMENT,
            ("Replacing non complement type in a BB's global interference"));

  global_lranges[rc] =
    global_lranges[rc]->Replace_Neighbor(old_lr, new_lr,
                               		 region->Subuniverse(rc));
}  


/////////////////////////////////////
// Change all <orig_tn> references in <gbb> to use <new_tn> instead.
void
GRA_BB::Rename_TN_References(TN* orig_tn, TN* new_tn)
{
  GRA_BB_OP_FORWARD_ITER iter;

  for (iter.Init(this); ! iter.Done(); iter.Step()) {
    INT i;
    OP* op = iter.Current();

    for ( i = OP_opnds(op) - 1; i >= 0; --i )
      if ( OP_opnd(op,i) == orig_tn) Set_OP_opnd(op, i, new_tn);

    for ( i = OP_results(op) - 1; i >= 0; --i )
      if ( OP_result(op,i) == orig_tn) Set_OP_result(op, i, new_tn);
  }
}

/////////////////////////////////////
// <lrange> is now live in of <bb>.  Alter <gbb> to reflect
// this by changing the _live_in GTN sets of the underlying BB.
void
GRA_BB::Add_Live_In_LRANGE(LRANGE* lrange)
{
  GRA_LIVE_Add_Live_In_GTN(bb, lrange->Tn());
}

/////////////////////////////////////
// <lrange> is now live out of <bb>.  Alter <gbb> to reflect
// this by changing the _live_out GTN sets of the underlying BB.
void
GRA_BB::Add_Live_Out_LRANGE(LRANGE* lrange)
{
  GRA_LIVE_Add_Live_Out_GTN(bb, lrange->Tn());
}

/////////////////////////////////////
// <lrange> is no longer live in of <bb>.  Alter <gbb>
// to reflect this by changing the _live_in GTN sets of the underlying BB.
void
GRA_BB::Remove_Live_In_LRANGE(LRANGE* lrange)
{
  GRA_LIVE_Remove_Live_In_GTN(bb, lrange->Tn());
}

/////////////////////////////////////
// <lrange> is no longer live out of <bb>.  Alter <gbb>
// to reflect this by changing the _live_out GTN sets of the underlying BB.
void
GRA_BB::Remove_Live_Out_LRANGE(LRANGE* lrange)
{
  GRA_LIVE_Remove_Live_Out_GTN(bb, lrange->Tn());
}

/////////////////////////////////////
// Return the number of live complement LRANGEs for the given
// <gbb> and <rc>.
INT32
GRA_BB::Global_Live_Lrange_Count(ISA_REGISTER_CLASS    rc)
{
  return global_lranges[rc]->Count();
}

/////////////////////////////////////
// Does <gbb> have more than one predecessor block?
BOOL
GRA_BB::Has_Multiple_Predecessors(void)
{
  GRA_BB_FLOW_NEIGHBOR_ITER iter;
  INT count = 0;
  for (iter.Preds_Init(this); ! iter.Done(); iter.Step()) {
    if ( ++count > 1 )
      return TRUE;
  }
  return FALSE;
}

/////////////////////////////////////
// returns true if block for a real region (as opposed to swp region).
BOOL
GRA_BB::Is_Region_Block(BOOL swp_too)
{
  RID* rid = Region()->Rid();
  if (rid && rid != Current_Rid &&
      (RID_type(rid) != RID_TYPE_swp || swp_too == TRUE)) {
    return TRUE;
  }
  return FALSE;
}

/////////////////////////////////////
// return true if this block is the entry to a region
BOOL
GRA_BB::Is_Region_Entry_Block(void)
{
  RID* rid = Region()->Rid();
  if (Is_Region_Block(FALSE) && Bb() == CGRIN_entry(RID_cginfo(rid))) 
    return TRUE;
  return FALSE;
}

/////////////////////////////////////
// determine if block acts as loop epilog or prolog
void 
GRA_BB::Check_Loop_Border(void)
{
  GRA_BB_FLOW_NEIGHBOR_ITER iter;

  //
  // TODO: right now we only look at prologs and epilogs for loop nests.
  // i.e. we don't care about prologs and epilogs for inner loops.
  // when we get prologs and epilogs on all loops, then we may be interested
  // in them as a way to profitably split TN's that have references in outer
  // loops of a nest, but not in its inner loops (in this case, we'd be
  // splitting a TN that's already been colored, and that's not going to
  // happen for a while anyway).
  //
  if (Loop() != NULL) {
    return;
  }

  for (iter.Preds_Init(this); ! iter.Done(); iter.Step()) {
    GRA_BB* pred = iter.Current();
    if (pred->Loop() != NULL) {
      Loop_Epilog_Set();
    }
  }
   
  // a block can be both prolog and epilog
  for (iter.Succs_Init(this); ! iter.Done(); iter.Step()) {
    GRA_BB* succ = iter.Current();
    if (succ->Loop() != NULL) {
      Loop_Prolog_Set();
    }
  }
}

