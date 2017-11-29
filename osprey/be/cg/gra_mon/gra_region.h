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

//  GRA region encapsulation

//  $revision: $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_region.h $

#ifndef GRA_REGION_INCLUDED
#define GRA_REGION_INCLUDED

#ifndef GRA_REGION_RCS_ID
#define GRA_REGION_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_region_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_region.h $ $Revision: 1.2 $";
#endif
#endif

#include "defs.h"
#include "register.h"
#include "mempool.h"
#include "region_util.h"

enum REGION_FLAG {
  REGION_FLAGS_has_call = 0x1,   // Region has a BB with call
};

// GRA associates information with regions and essentially colors 
// each region separately, using preferencing to try to remove data
// copies at the borders.  For GRA's purposes, regions are either
// contigious sets of blocks for which a consistent previous register
// allocation exists (the "previously allocated regions") or the set of
// all blocks not in any prev_alloc_region (the "complement region")
class GRA_REGION {
friend class GRA_REGION_MGR;
  RID* rid;
    //  Region ID associated with the blocks in the region.  This could be
    //  non-unique for the complement.  Do we care?
  LRANGE_SUBUNIVERSE* subuniverse[ISA_REGISTER_CLASS_MAX+1];
    // The subuniverse of non-local LRANGEs associated
    // with the <region> that need a register in the
    // REGISTER_CLASS <rc>.  Note that any local LRANGEs are not
    // included in these subuniverses.  Currently there is no way
    // to access the locals associated with a region directly.
  LRANGE_SUBUNIVERSE* local_subuniverse;
    //  A subuniverse for all the local LRANGEs associated with the region.
    //  We are really just using this as a dynamically sized vector to hold
    //  these guys.  We don't uses sets of them.
  GRA_BB* first_gbb;
    //  Head of internally linked list of BBs associated with the region.
  GRA_BB* last_gbb;
    //  Final BB on internally linked list.
  INT32 lrange_count[ISA_REGISTER_CLASS_MAX+1];
    //  Total number of LRANGEs (local and non-local) associated with the region
    // per ISA_REGISTER_CLASS.
  REGISTER_SET registers_used[ISA_REGISTER_CLASS_MAX+1];
    //  the set of registers of each REGISTER_CLASS 
    //  currently allocated to any LR in this region
  REGISTER_SET registers_available[ISA_REGISTER_CLASS_MAX+1];
    //  the set of registers in each REGISTER_CLASS  available in
    //  this region.  These are the allocatable
    //  registers that have not been preallocated to any
    //  live range that is live thoughout the region.
  GRA_REGION* next;
    //  Head of internally linked list of previously allocated regions.
  REGION_FLAG flags;
public:
  GRA_REGION(void) {}
  ~GRA_REGION(void) {}

  // access member functions
  RID *Rid(void)			{ return rid; }
  LRANGE_SUBUNIVERSE* Subuniverse(ISA_REGISTER_CLASS r) {return subuniverse[r];}
  INT32 Lrange_Count(ISA_REGISTER_CLASS rc) { return lrange_count[rc]; }
  REGISTER_SET Registers_Used(ISA_REGISTER_CLASS rc){return registers_used[rc];}
  void Make_Register_Used(ISA_REGISTER_CLASS rc, REGISTER reg) {
	      registers_used[rc] = REGISTER_SET_Union1(registers_used[rc], reg);
	    }
    // Adds <reg> a register in <rc> to the set of registers used in <region>.
  REGISTER_SET Registers_Available(ISA_REGISTER_CLASS rc){return registers_available[rc];}
  void Make_Register_Unavailable(REGISTER reg, ISA_REGISTER_CLASS rc) {
      registers_available[rc] = REGISTER_SET_Difference1(registers_available[rc],reg);
    }
    // Add <reg> to the set of unavailable registers in <region>
  GRA_BB *First_Gbb(void)		{ return first_gbb; }
  GRA_BB *Last_Gbb(void)		{ return last_gbb; }
  LRANGE_SUBUNIVERSE *Local_Subuniverse(void) { return local_subuniverse; }
  GRA_REGION *Next(void)		{ return next; }
  BOOL Has_Call(void) 			{ return flags & REGION_FLAGS_has_call;}

  // other inlined member functions
  BOOL Is_Loop(void)			{ return TRUE; }
		    // Is this a loop region?
                    // Ugly workaround for now until info cleanly available
                    // from region_util.
  INT32 Callee_Saves_Registers_Available_Count(ISA_REGISTER_CLASS rc) {
      return REGISTER_SET_Size(REGISTER_SET_Difference(registers_available[rc],
                                              REGISTER_CLASS_caller_saves(rc)));
    }
    // Returns the count of callee saves registers available in
    // <region> for LRANGEs of the given REGISTER_CLASS.
  BOOL Is_Prev_Alloc(void) { return rid != NULL && RID_has_reg_alloc(rid); }
    // Was there a previous register allocation for this region?
  BOOL Prev_Alloc_By_GRA(void) { return rid != NULL && RID_was_gra(rid); }
    // Was there a previous register allocation for this region
    // performed by GRA (no? then if previously allocated, must
    // have been by SWP.)
  void Set_GRA_Colored(void)   { if (rid != NULL) {
				   RID_was_gra_Set(rid);
				   RID_has_reg_alloc_Set(rid);
			         }
			       }
    // Record the facts that the region has a previous allocation
    // and that it was performed by GRA.

  // non-inlined member functions
  void Add_GBB( GRA_BB* gbb );
  void Alloc_Subuniverses(void);
  void Add_LRANGE( LRANGE* lrange );
  INT32 Cycle_Count(void);
};


// manages the GRA_REGION package
class GRA_REGION_MGR {
  INT32 _alloc_count;
  GRA_REGION* _first_prev_alloc_region;
  GRA_REGION* _complement_region;
  BOOL _complement_region_created;
  GRA_REGION **_map;		// RIDs with reg allocs -> GRA_REGIONs

  GRA_REGION *Create(RID *rid);
  GRA_REGION *Create_Complement(RID *rid);
  GRA_REGION *Create_Prev_Allocated(RID *rid);
public:
  GRA_REGION_MGR(void) {}
  ~GRA_REGION_MGR(void) {}

  INT32 Alloc_Count(void)		{ return _alloc_count; }
  GRA_REGION* First_Prev_Alloc_Region(void){ return _first_prev_alloc_region; }
  GRA_REGION* Complement_Region(void) 	{ return _complement_region; }

  void Initialize(void);
  void Finalize(void) {}
  GRA_REGION* Get( RID* rid );
};

extern GRA_REGION_MGR gra_region_mgr;


// An iterator type used to iterate over the non-local LRANGEs
// associated with a particular GRA_REGION and REGISTER_CLASS.
class GRA_REGION_RC_NL_LRANGE_ITER {
  LRANGE_SUBUNIVERSE* subuniverse; // Current subuniverse
  INT32               i;           // Current index in subuniverse
public:
  GRA_REGION_RC_NL_LRANGE_ITER(void) {}
  ~GRA_REGION_RC_NL_LRANGE_ITER(void) {}

  void Init( GRA_REGION* region, ISA_REGISTER_CLASS rc) {
				      subuniverse = region->Subuniverse(rc);
				      i = subuniverse->Count() - 1;
				    }
  BOOL Done(void) 		    { return i < 0; }
  LRANGE *Current(void)		    { return subuniverse->Nth_Lrange(i); }
  void Step(void)		    { --i; }
};


// An iterator type used to iterate over all the LRANGEs (local and
// non-local) for a REGISTER_CLASS and GRA_REGION.
class GRA_REGION_RC_LRANGE_ITER {
  GRA_REGION*         _region;         // whose LRANGEs traversed
  ISA_REGISTER_CLASS  _rc;            
  INT32               _i;              // for stepping LRANGEs
  enum {in_locals, 
	in_non_locals, 
	done} 	      _state; 	      // 3 different states
  LRANGE*             _current;        

  void Step_Locals(void);
  void Step_Non_Locals(void);
public:
  GRA_REGION_RC_LRANGE_ITER(void) {}
  ~GRA_REGION_RC_LRANGE_ITER(void) {}

  BOOL Done(void)		{ return _state == done; }
  LRANGE *Current(void)		{ return _current; }
  void Step(void)		{ if (_state == in_locals) 
				    Step_Locals();
				  else Step_Non_Locals(); }

  // non-inlined
  void Init( GRA_REGION* region, ISA_REGISTER_CLASS rc);
    // Prepare to traverse all the LRANGEs associated with <region> and <rc>.
};


// An iterator type used to traverse all the previously allocated
// GRA_REGIONs.  
class GRA_REGION_PREV_ALLOC_ITER {
  GRA_REGION* _current;  // Region in iteration
public:
  GRA_REGION_PREV_ALLOC_ITER(void) {}
  ~GRA_REGION_PREV_ALLOC_ITER(void) {}

  void Init(void)		{_current = gra_region_mgr.First_Prev_Alloc_Region();}
  BOOL Done(void)		{ return _current == NULL; }
  GRA_REGION *Current(void) 	{ return _current; }
  void Step(void)		{ _current = _current->Next(); }
};


// An iterator type used to traverse all the GRA_BBs associated
// with a particular GRA_REGION.  The blocks are visited in order
// of emission.
class GRA_REGION_GBB_ITER {
  GRA_BB* _gbb;
public:
  GRA_REGION_GBB_ITER(void) {}
  ~GRA_REGION_GBB_ITER(void) {}

  void Init(GRA_REGION *region)		{ _gbb = region->First_Gbb(); }
  BOOL Done(void)			{ return _gbb == NULL; }
  GRA_BB* Current(void)			{ return _gbb; }
  void Step(void)			{ _gbb = _gbb->Region_Next(); }
};
#endif

