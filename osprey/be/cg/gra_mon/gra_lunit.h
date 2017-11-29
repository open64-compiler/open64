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

//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lunit.h $
//  Description: GRA Live Units
//
//      This data structure started its life (in the Chow GRA) as an
//      encapsulation of the BBs for the LRANGEs that include them.  It
//      originally served two functions:
//
//          1. represented the set of blocks in the live range via an
//             internally linked list of LUNITs, each pointing to a BB, and
//          2. served as a repository for per BB information about the live
//             range, particularly reference counts, and
//          3. a dense list of the BBs in the LRANGE containing references.
//
//      However, it turns out to be a bad idea to have a list element per BB
//      in each live range, because sometimes there are many very sparse live
//      ranges.  So the function of 1 was supplanted by a BB set per LRANGE
//      (this must have already existed as its intersection serves as the
//      basic interference test in the Chow register allocators.)
//
//      We've retained this data structure primarily because 3 seems to be
//      useful during splitting.  We like having:
//
//          A. a quick way of finding the highest priority block in which
//             the LRANGE is referenced (We can scan the LUNITs for the LRANGE
//             and not have to skip transparent BBs as we would if we used the
//             live BB sets.)
//
//          B. a repository for information about spilling.  (If we decide
//             to spill a LRANGE, we can scan it's LUNITs to find the
//             references.)
//
//      So although LUNITs seem to be in some sense vestigial, we have
//      retained them.

#ifndef GRA_LUNIT_INCLUDED
#define GRA_LUNIT_INCLUDED

#ifndef GRA_LUNIT_RCS_ID
#define GRA_LUNIT_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_lunit_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lunit.h $ $Revision: 1.2 $";
#endif
#endif

#include "defs.h"
#include "register.h"

#include "gra_bb.h"

// forward declarations
class GRA_BB;

enum LU_FLAG {
  LUNIT_FLAGS_spill_listed       =   0x1,
//              Keep track of whether the given <lunit> has been placed on a
//              spill_list.  Exported only to gra_spill.
  LUNIT_FLAGS_spill_below        =   0x2,
//              Keep track of where spill are requred (introduced
//              by live range splitting -- Whole LRANGES may also be silled)
  LUNIT_FLAGS_restore_above      =   0x4,
//              Keep track of where restores are requred (introduced
//              by live range splitting -- Whole LRANGES may also be silled)
  LUNIT_FLAGS_split_defreach_out =   0x8,
//              Property for recording liveness info of a LUNIT within a
//              split live range.  Computed only as required in gra_spill and
//              not generally valid (currently only set on LUNIT's for blocks
//		containing spills).
  LUNIT_FLAGS_split_live_in      =  0x10,
//              Property for recording liveness info of a LUNIT within a
//              split live range.  Computed only as required in gra_spill and
//              not generally valid (currently only set on LUNIT's for blocks
//		containing spills).
  LUNIT_FLAGS_has_exposed_use    =  0x20,
//              Does this LUNIT's TN have an exposed use in it's basic block
//              (before any split loads were added.)
  LUNIT_FLAGS_has_def            =  0x40,
//              Does this LUNIT's TN have a definition in it's basic block?
  LUNIT_FLAGS_spill_below_sticks =  0x80,
//              Used in gra_spill to keep track of special spills added that
//              cannot be moved.
  LUNIT_FLAGS_split_lunit        = 0x100,
//		Lunit created for splitting reference.
  LUNIT_FLAGS_true_reference     = 0x200,
  LUNIT_FLAGS_def_from_home	 = 0x400,
//		Is the definition live out of LUNIT from a load from the
//		home location?
  LUNIT_FLAGS_syncs_with_home    = 0x800,
//		Is the home location sync'd up with the value in the register
//		by this block?
};

class LUNIT;
class LRANGE;
class LUNIT_BB_LIST_ITER;
class LUNIT_LRANGE_LIST_ITER;
class LUNIT_SPILL_LIST_ITER;
class LRANGE_LUNIT_ITER;

extern LUNIT* LUNIT_Create( LRANGE* lrange, GRA_BB* gbb );

class LUNIT {
friend class LRANGE;
friend class LUNIT_BB_LIST_ITER;
friend class LUNIT_LRANGE_LIST_ITER;
friend class LUNIT_SPILL_LIST_ITER;
friend class LRANGE_LUNIT_ITER;
friend LUNIT *LUNIT_Create(LRANGE *, GRA_BB*);
private:
  LRANGE*       lrange;		// used during splitting when we want to rename
				// half of the LRANGE
  GRA_BB*       gbb;
  LUNIT*        bb_list_next;
  LUNIT*        lrange_list_next;
  LUNIT*        spill_list_next;
  float         pref_priority;	// priority of allocating lunit's LRANGE to its
				// preference class in the given block
  REGISTER_SET  allowed_preferences; // set of registers that are allowed for 
			// <lunit> within its block even if they are already 
			// used.  This is because they are used for LRANGEs 
			// with wired registers that are preferenced to 
			// <lunit>'s LRANGE.  It is thus always safe and 
			// desirable to allocate <lunit> to one of these within
			// its block (but perhaps not within other blocks)
  UINT32        flags;
  INT		def_count; // data on TN definitions within a block needed for 
			// global preferencing.
  INT		last_def; // data on TN definitions within a block needed for 
			// global preferencing.
#ifdef TARG_IA64
  BOOL          has_use;
#endif
  LRANGE*	global_pref; // global lr that is a preference candidate in lunit
public:
  LUNIT(void) {}
  ~LUNIT(void) {}

  // access functions
  GRA_BB *Gbb(void)		{ return gbb; }
  LRANGE *Lrange(void)		{ return lrange; }
  void Lrange_Set(LRANGE *lr)	{ lrange = lr; }
  float Pref_Priority(void)	{ return pref_priority; }
  REGISTER_SET Allowed_Preferences(void) { return allowed_preferences; }
  INT Def_Count(void) 		{ return def_count; }
  void Def_Count_Set(INT d)	{ def_count = d; }
  INT Last_Def(void) 		{ return last_def; }
  void Last_Def_Set(INT ld)	{ last_def = ld; }
  LRANGE *Global_Pref(void) 	{ return global_pref; }
  void Global_Pref_Set(LRANGE *lr){ global_pref = lr; }

  // access functions for the flags
  BOOL Has_Exposed_Use(void)	{ return flags & LUNIT_FLAGS_has_exposed_use; }
  void Has_Exposed_Use_Set(void){ flags |= LUNIT_FLAGS_has_exposed_use; }
  BOOL Has_Def(void)	{ return flags & LUNIT_FLAGS_has_def; }
  void Has_Def_Set(void)	{ flags |= LUNIT_FLAGS_has_def; }
#ifdef TARG_IA64
  BOOL Has_Use(void)    {return has_use; }
  void Has_Use_Set(void)  { has_use = TRUE;}
#endif
  BOOL Spill_Below_Sticks(void){ return flags & LUNIT_FLAGS_spill_below_sticks; }
  void Spill_Below_Sticks_Set(void){ flags |= LUNIT_FLAGS_spill_below_sticks; }
  BOOL Split_Lunit(void)	{ return flags & LUNIT_FLAGS_split_lunit; }
  void Split_Lunit_Set(void){ flags |= LUNIT_FLAGS_split_lunit; }
  BOOL True_Reference(void)	{ return flags & LUNIT_FLAGS_true_reference; }
  void True_Reference_Set(void){ flags |= LUNIT_FLAGS_true_reference; }
  BOOL Def_From_Home(void)	{ return flags & LUNIT_FLAGS_def_from_home; }
  void Def_From_Home_Set(void){ flags |= LUNIT_FLAGS_def_from_home; }
  void Def_From_Home_Reset(void){ flags &= ~LUNIT_FLAGS_def_from_home; }
  BOOL Syncs_With_Home(void){ return flags & LUNIT_FLAGS_syncs_with_home; }
  void Syncs_With_Home_Set(void){ flags |= LUNIT_FLAGS_syncs_with_home; }
  void Syncs_With_Home_Reset(void){ flags &= ~LUNIT_FLAGS_syncs_with_home; }
  BOOL Spill_Listed(void)	{ return flags & LUNIT_FLAGS_spill_listed; }
  void Spill_Listed_Set(void){ flags |= LUNIT_FLAGS_spill_listed; }
  void Spill_Listed_Reset(void){ flags &= ~LUNIT_FLAGS_spill_listed; }
  BOOL Restore_Above(void)	{ return flags & LUNIT_FLAGS_restore_above; }
  void Restore_Above_Set(void){ flags |= LUNIT_FLAGS_restore_above; }
  void Restore_Above_Reset(void){ flags &= ~LUNIT_FLAGS_restore_above; }
  BOOL Split_Defreach_Out(void)	{ return flags & LUNIT_FLAGS_split_defreach_out; }
  void Split_Defreach_Out_Set(void){ flags |= LUNIT_FLAGS_split_defreach_out; }
  BOOL Split_Live_In(void)	{ return flags & LUNIT_FLAGS_split_live_in; }
  void Split_Live_In_Set(void){ flags |= LUNIT_FLAGS_split_live_in; }
  BOOL Spill_Below(void)	{ return flags & LUNIT_FLAGS_spill_below; }
  void Spill_Below_Set(void){ flags |= LUNIT_FLAGS_spill_below; }
  void Spill_Below_Reset(void){ flags &= ~LUNIT_FLAGS_spill_below; }

  // inlined member functions
  LUNIT* BB_List_Push(LUNIT* new_elt) { new_elt->bb_list_next = this;
  					return new_elt; }
  LUNIT* Lrange_List_Push(LUNIT* new_elt) { new_elt->lrange_list_next = this;
					    return new_elt; }
  LUNIT* Spill_List_Push(LUNIT *new_elt) { new_elt->spill_list_next = this;
					   return new_elt; }

  // non-inlined member functions
  void Preference_Copy(LRANGE *);
  BOOL Live_Out(void);
  BOOL Live_In(void);
  float Priority(void);
};


// An iterator type for iterating over the elements of the internally
// linked list of LUNITs associated with a particular BB.  Exported only
// to GRA_BB.
class LUNIT_BB_LIST_ITER {
  LUNIT* current;
  LUNIT* next;
public:
  LUNIT_BB_LIST_ITER(void) {}
  ~LUNIT_BB_LIST_ITER(void) {}

  void Init(LUNIT *head)	{ current = head;
  				  if ( current != NULL )
    				    next = current->bb_list_next; }
  BOOL Done(void)		{ return current == NULL; }
  LUNIT *Current(void)		{ return current; }
  void Step(void)		{ current = next;
				  if (next != NULL)
				    next = next->bb_list_next; }
};


// An iterator type for iterating over the elements of the internally
// linked list of LUNITs associated with a particular LRANGE.  To iterate
// over the LUNITs in a LRANGE, see LRANGE_LUNIT_ITER in gra_lrange.h.
class LUNIT_LRANGE_LIST_ITER {
  LUNIT* current;
  LUNIT* next;
public:
  LUNIT_LRANGE_LIST_ITER(void) {}
  ~LUNIT_LRANGE_LIST_ITER(void) {}

  void Init(LUNIT *head)	{ current = head;
  				  if ( current != NULL )
    				    next = current->lrange_list_next; }
  BOOL Done(void)		{ return current == NULL; }
  LUNIT *Current(void)		{ return current; }
  void Step(void)		{ current = next;
				  if (next != NULL)
				    next = next->lrange_list_next; }
};


// An iterator type for iterating over the elements of the internally
// linked list of LUNITs that have been either restored or spilled.  
class LUNIT_SPILL_LIST_ITER {
  LUNIT* current;
public:
  LUNIT_SPILL_LIST_ITER(void) {}
  ~LUNIT_SPILL_LIST_ITER(void) {}

  void Init(LUNIT *head)	{ current = head; }
  BOOL Done(void)		{ return current == NULL; }
  LUNIT *Current(void)		{ return current; }
  void Step(void)		{ current = current->spill_list_next; }
};

#ifndef TARG_IA64
extern LUNIT* LUNIT_Create( LRANGE* lrange, GRA_BB* gbb );
#endif

#endif

