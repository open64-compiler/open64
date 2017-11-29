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

//  GRA Preference classes
/////////////////////////////////////
//
//  Description:
//
//      We can remove register transfers between LRANGEs when the result and
//      the operand of the copy are assigned the same register.  This module
//      provides a data structure that keeps track of sets of LRANGEs that we
//      would like to keep in the same register so that we can remove such
//      copies.  It also provides routines for finding such preferences during
//	GRA_Create().
//
//      These are equivalence classes of LRANGEs, representing the benefit of
//      allocating the same register to each of a set of LRANGES.  This has
//      two rather different applications:
//
//          1. We try to remove copies between COMPLEMENT and region LRANGEs
//             by unifying the preference classes whenever we see a copy
//             between these two types of LRANGEs.
//
//          2. We try to remove copies between dedicated LOCAL and COMPLEMENT
//             LRANGEs in much the same way.  But we preallocate the register
//             to the LOCAL so it will be seen as a preference when the globals
//             in the equivalance class are allocated.
//
//      As we find such copies, we union the preference classes of the operand
//      and result of the copy, thus we call such copies, "preferencing
//      copies".
//
//      Besides being a set of LRANGEs, GRA_PREFs also contain a set of BBs,
//      used to keep track of the blocks in which copies from one member
//      LRANGE to another are made.  This set is used during LRANGE splitting
//      in order to dettermine which of the two resulting LRANGEs should be in
//      the preference class (in some cases both can be though, of course the
//      same register cannot be used throughout the original LRANGE or it
//      would not have been split.)
//
//      There is no need for us to keep track of the unallocated LRANGEs in a
//      preference class (though each of the LRANGEs in the preference class
//      can cheaply find the preference class.)  As each element of the
//      preference class is allocated a register, it is added to a sequence
//      sorted by the count of frequency weighted prefrencing copies of which
//      the LRANGE is either an operand or a result.  An iterator type is
//      provided for the allocated LRANGEs in a preference class in this
//      order.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_pref.h $


#ifndef GRA_PREFERENCE_INCLUDED
#define GRA_PREFERENCE_INCLUDED

#ifndef GRA_PREFERENCE_RCS_ID
#define GRA_PREFERENCE_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_preference_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_pref.h $ $Revision: 1.2 $";
#endif
#endif

#include "defs.h"
#include "tn.h"
#include "mempool.h"
#include "gra_lrange.h"
#include "lrange_list.h"
#include "gra_bb.h"

// Represents an equivalence class of LRANGEs that would all like to be
// allocated to the same register.
class GRA_PREF {
friend class GRA_PREF_MGR;
friend class GRA_PREF_LRANGE_ITER;
  GRA_PREF*         parent;             // Union-find data structure
  INT32             count;              // See Aho, Hopcroft & Ulman
  LRANGE_LIST*      lranges;            // Sorted by freq weighted # of pref
                                        //   copies 

  void Make_Parent_Of(GRA_PREF *child) {
    //  <this> and <child> must initially both be roots.  If they are not
    //  identical, make <this> be the parent of child. Union any
    //  blocks_with_copies from <child> into <this>.  Ditto any allocated
    //  LRANGEs.
    child->parent = this;
    count += child->count;
    Is_True(child->lranges == NULL,
	    ("Union of GRA preferences with some allocated LRANGES"));
  }
  GRA_PREF *Find(void);
public:
  GRA_PREF(void) {}
  ~GRA_PREF(void) {}

  void Allocate_LRANGE( LRANGE* lrange );
};

// Use to iterate over the allocated LRANGEs in a GRA_PREF.  
class GRA_PREF_LRANGE_ITER {
  LRANGE_LIST* current;
public:
  GRA_PREF_LRANGE_ITER(void) {}
  ~GRA_PREF_LRANGE_ITER(void) {}

  void Init(GRA_PREF *pref)	{ 
    // Prepare <iter> to iterate over the allocated LRANGEs in order
    // of the frequency weighted count of preferencing copies that
    // reference the LRANGEs (this frequency weighted count is stored
    // in the LRANGEs themselves and maintained by the LRANGE
    // module.)  Special hack -- if <pref> is NULL, iteration doesn't
    // happen.  This is useful because we use NULL as the value for
    // LRANGEs that aren't preferenced.
			  	  if (pref == NULL)
				    current = NULL;
				  else current = pref->Find()->lranges; }
  BOOL Done(void)		{ return current == NULL; }
  LRANGE *Current(void)		{ return LRANGE_LIST_first(current); }
  void Step(void)		{ current = LRANGE_LIST_rest(current); }
};


// Contains source and destination TNs for the copy that creates the
// preferencing opportunity.
class GRA_PREF_CAND {
  TN* source;	// Source TN of the preferencing copy
  TN* dest;     // Destination TN of the preferencing copy
public:
  GRA_PREF_CAND(void) {}
  ~GRA_PREF_CAND(void) {}

  TN *Source(void)		{ return source; }
  void Source_Set(TN *s)	{ source = s; }
  TN *Dest(void)		{ return dest; }
  void Dest_Set(TN *d)		{ dest = d; }
};


// Contains miminmal liveness information for determining if a preference
// is legal.
class GRA_PREF_LIVE {
  INT last_def;		// last definition of TN seen in BB
  INT num_defs;		// number of definitions of TN in BB
  INT exposed_use;	// true if exposed use of TN at top of BB
public:
  GRA_PREF_LIVE(void) {}
  ~GRA_PREF_LIVE(void) {}

  INT Last_Def(void)		{ return last_def; }
  void Last_Def_Set(INT i)	{ last_def = i; }
  INT Num_Defs(void)		{ return num_defs; }
  void Num_Defs_Set(INT i)	{ num_defs = i; }
  INT Exposed_Use(void)		{ return exposed_use; }
  void Exposed_Use_Set(INT i)	{ exposed_use = i; }
};


class GRA_PREF_MGR {
public:
  GRA_PREF_MGR(void) {}
  ~GRA_PREF_MGR(void) {}

  GRA_PREF* Create(void) { // Create a new GRA_PREF.
    GRA_PREF* pref = TYPE_MEM_POOL_ALLOC(GRA_PREF,GRA_pool);
    pref->lranges = NULL;
    pref->parent = NULL;
    pref->count  = 1;
    return pref;
  }
  GRA_PREF *UnionD(GRA_PREF *pref0, GRA_PREF *pref1);
  GRA_PREF_CAND* CAND_Create(TN* dest, TN* source, MEM_POOL* pool) {
    // Allocate and initialize a GRA_PREF_CAND node
    GRA_PREF_CAND* gpc = TYPE_MEM_POOL_ALLOC(GRA_PREF_CAND, pool);
    gpc->Source_Set(source);
    gpc->Dest_Set(dest);
    return gpc;
  }
  GRA_PREF_LIVE *LIVE_Create(MEM_POOL *pool) {
    // Create and initialize a GRA_PREF_LIVE node
    GRA_PREF_LIVE* gpl = TYPE_MEM_POOL_ALLOC(GRA_PREF_LIVE, pool);
    gpl->Last_Def_Set(0);
    gpl->Num_Defs_Set(0);
    gpl->Exposed_Use_Set(FALSE);
    return gpl;
  }
};

extern GRA_PREF_MGR gra_pref_mgr;

#endif
