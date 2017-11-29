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


/////////////////////////////////////
//
//  Find interesting regions of the control flow graph that will
//  make good candidates for forming hyperblocks.  Loops bodies,
//  of course, are always good candidates.  We also attempt to
//  find other control flow structures that will also make good
//  candidates (e.g. if-then-else constructs).
//
//  When choosing candidate regions, we must make sure that we
//  do not present too large of a graph for consideration by the
//  block selection algorithm.  The reason for this is two fold.
//  First, we do not wish to form a region that is so large that
//  we cannot possibly if convert a large portion of it profitably.
//  Second, compile time is an issue for the path enumeration algorithm,
//  so we must keep the number of nodes that it sees to a reasonably
//  small number.  See the osprey design web page for hyperblocks for
//  more details on this algorithm.
//
//  Exported types:
//
//	HB_CAND_TREE
//	  candidate - hyperblock structure representing the candidate region.
//	  parent - ancestor in tree, i.e. immediately containing region.
//	  kids - list of fully contained candidate regions.
//
//  External routines:
//
//	HB_CAND_TREE_Alloc(MEM_POOL* pool)
//	  allocate and initialize a new structure
//
//	inline HB* HB_CAND_TREE_Candidate(HB_CAND_TREE* hct)
//	inline void HB_CAND_TREE_Candidate_Set(HB_CAND_TREE* hct,
//					       HB* candidate)
//	inline HB_CAND_TREE* HB_CAND_TREE_Parent(HB_CAND_TREE* hct)
//	inline void HB_CAND_TREE_Parent_Set(HB_CAND_TREE* hct,
//					    HB_CAND_TREE* parent)
//	inline list<HB_CAND_TREE*> HB_CAND_TREE_Kids(HB_CAND_TREE* hct)
//	  Routines for accessing/setting members.
//	
//      void HB_Identify_Hammock_Candidates(list<HB_CAND_TREE*> candidates,
//					    BB_MAP hct_entry_map);
//	  Identifies "hammock" regions from the control flow graph.  These
//	  can be nested.
//
//	void HB_Identify_General_Candidates(list<HB_CAND_TREE*> candidates,
//					    BB_MAP hct_entry_map,
//					    INT pass)
//	  Identifies high priority connected regions in the control flow
//	  graph that are single entry, multiple exit that are good candidates
//	  for hyperblock formation.
//
//	void HB_Identify_Candidates_Init()
//	  Initialize data required by candidate region identification 
//	  algorithms.
//
//      INT HB_CAND_TREE_Flags(HB_CAND_TREE* hct)
//	void HB_CAND_TREE_Flags_Reset_All(HB_CAND_TREE* hct)
//	BOOL HB_CAND_TREE_Check_Flag(HB_CAND_TREE* hct, INT flag)
//	void HB_CAND_TREE_Reset_Flag(HB_CAND_TREE* hct, INT flag)
//	  Routines to maniplate tree node flags.
//
//////////////////////////////////////

#ifndef HB_ID_CANDIDATES_H_INCLUDED
#define HB_ID_CANDIDATES_H_INCLUDED

#include <list>
#include "cxx_memory.h"

struct HB_CAND_TREE {
  HB* candidate;
  HB_CAND_TREE* parent;
  std::list<HB_CAND_TREE*> kids;
  INT flags;
};

//
// Flags (properties)
//
#define HCT_FULLY_CONVERTED		0x00000001
#define HCT_SINGLE_BLOCK		0x00000002
#define HCT_SHARED_ENTRY		0x00000004
#define HCT_SHARED_EXIT			0x00000008
#define HCT_UNPROCESSED			0x00000010
#define HCT_ERASE			0x00000020
#define HCT_GENERAL			0x00000040

// Flags (miscellaneous)
#define HCT_VISITED                     0x00000080
#define HCT_CVISITED                    0x00000100


/////////////////////////////////////
inline HB*
HB_CAND_TREE_Candidate(HB_CAND_TREE* hct)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hct->candidate;
}

/////////////////////////////////////
inline void
HB_CAND_TREE_Candidate_Set(HB_CAND_TREE* hct, HB* candidate)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hct->candidate = candidate;
}

/////////////////////////////////////
inline HB_CAND_TREE*
HB_CAND_TREE_Parent(HB_CAND_TREE* hct)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hct->parent;
}

/////////////////////////////////////
inline void
HB_CAND_TREE_Parent_Set(HB_CAND_TREE* hct, HB_CAND_TREE* parent)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hct->parent = parent;
}

/////////////////////////////////////
inline std::list<HB_CAND_TREE*>&
HB_CAND_TREE_Kids(HB_CAND_TREE* hct)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hct->kids;
}

/////////////////////////////////////
inline INT HB_CAND_TREE_Flags(HB_CAND_TREE* hct)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hct->flags;
}

/////////////////////////////////////
inline void HB_CAND_TREE_Flags_Reset_All(HB_CAND_TREE* hct)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hct->flags = 0;
}

/////////////////////////////////////
inline BOOL HB_CAND_TREE_Check_Flag(HB_CAND_TREE* hct, INT flag)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hct->flags & flag;
}

/////////////////////////////////////
inline void HB_CAND_TREE_Reset_Flag(HB_CAND_TREE* hct, INT flag)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hct->flags &= ~flag;
}

/////////////////////////////////////
inline void HB_CAND_TREE_Set_Flag(HB_CAND_TREE* hct, INT flag)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hct->flags |= flag;
}

inline HB_CAND_TREE* HB_CAND_TREE_Alloc(MEM_POOL* pool)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  HB_CAND_TREE* hct = CXX_NEW(HB_CAND_TREE, pool);
  HB_CAND_TREE_Candidate_Set(hct, NULL);
  HB_CAND_TREE_Parent_Set(hct, NULL);
  HB_CAND_TREE_Flags_Reset_All(hct);
  return hct;
}

extern void HB_Identify_Hammock_Candidates(std::list<HB_CAND_TREE*>& candidates,
					   BB_MAP hct_entry_map);
extern void HB_Identify_General_Candidates(std::list<HB_CAND_TREE*>& candidates,
					   BB_MAP hct_entry_map,
					   INT pass);
extern void HB_Identify_Candidates_Init();
extern BOOL Check_BB_For_HB_Suitability(BB* bb, BB* bb_entry);
extern BOOL Check_HB_For_PQS_Suitability(BB_SET *selected_hb, BB *entry);

#endif



