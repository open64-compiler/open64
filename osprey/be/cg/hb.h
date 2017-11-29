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


//
//  Hyperblock formation module
//
/////////////////////////////////////
//
//  Description:
//
//	This module implements an algorithm to choose hyperblocks from
//	multiple control flow paths in a program unit.  The algorithm
//	attempts to choose only those paths that can be profitably
//	executed together (as opposed to full if-conversion).  The
//	algorithm is largely the same as that proposed by Scott Malke
//	in his PhD thesis (section 7.2).  We diverge from it primarily
//	in the fact that we do not tail duplicate unconditionally, and
//	we modify the path priority algorithms if we are dealing with
//	statically generated frequency data.  Here is a brief overview
//	of the algorithm:
//
//	  1. Identify hyperblock candidates - select regions of the CFG
//	     that might be profitably converted into hyperblocks.
//
//	  2. Coalesce backedges - only acyclic graphs can be if converted,
//	     so we make all backedges in a loop body branch to a single
//	     block with an unconditional branch to the loop head (physically,
//	     this step currently happens during step 1 above).
//
//  	  3. Block selection - use heuristics to select blocks for inclusion
//  	     in the hyperblock on a per path basis (I.e. blocks are not
//  	     considered individually).
//
//  	  4. Tail duplication - side entrances to the hyperblock are not
//  	     allowed, so we must duplicate blocks to remove them.
//
//  	  5. If conversion - removal of branches, insertion of predicate
//  	     calculations, and modification of instructions to predicated
//  	     forms.
//
//
//  Exported types:
//
//	HYPER_BLOCK: structure describing a hyperblock.
//
//	  entry - entry block.
//	  flags - informational flags on hyperblock.
//	  blocks - set of bb's making up the hyperblock.  Even though
//		   a hyperblock is logically one block with possibly
//	           multiple exits, we still break it into basic blocks
//		   at the exit branches.  Otherwise, its just too hard
//		   for other CG components to deal with.
//
//
//  Exported globals:
//	HB_bb_map 
//	  Maps bb's to hyperblocks.  Not valid until after block selection.
//
//  Exported functions:
//
//	void HB_Form_Hyperblocks(RID* rid, const BB_REGION& bb_region)
//	  Driver routine for hyperblock formation.
//
//	BB*  HB_Entry(HB* hb)
//	void HB_Entry_Set(HB* hb, BB* entry)
//	BB*  HB_Exit(HB* hb)
//	void HB_Exit_Set(HB* hb, BB* entry)
//	  Return/Set entry/exit block for a given hyperblock.
//
//	BB*  HB_Fall_Thru_Exit(HB* hb)
//	void HB_Fall_Thru_Exit_Set(HB* hb, BB* fall_thru_exit
//	  Return/Set block that the hyperblock may fall thru to if it
//	  does not take a side exit.  This is, more or less, the "join"
//	  point where control flow comes back together at the bottom
//	  of the region of blocks chosen for hyperblock formation.  It
//	  may not exist for all hyperblocks, and there is no requirement
//	  that the hyperblock actually fall thru into it (from the hyperblocks
//	  perspective it can, but it may have multiple blocks that target
//	  it).  This block will terminate every path in the hyperblock for
//	  the purposes of calculating costs, but will not actually be
//        included in the hyperblock itself.
//
//	BB_SET* HB_Blocks(HB* hb)
//	void   HB_Blocks_Set(HB* hb, BB_SET* bbs)
//	void HB_Blocks_Copy(HB* hb, BB_SET* bbs)
//	  Return/Set/Copy BB_SET of BBs in the hyperblock.
//
//	void HB_Add_Block(HB* hb, BB* bb)
//	void HB_Remove_Block(HB* hb, BB* bb)
//	void HB_Add_BB_SET(HB* hb, BB* bb, MEM_POOL* pool)
//	void HB_Intersect_BB_SET(hb, BB_SET* bb_set)
//	  Manipulate contents of bb_set representing the hyperblock.
//	  Doesn't touch HB_bb_map.  We provide these so that we can
//	  use the HB structure to represent potential hyperblocks.
//
//	void HB_Replace_Block(HB* hb, BB* old_bb,new_bb)
//        Replace old_bb with new_bb in the hyperblock. Also update HB_bb_map.
//
//	void HB_Add_Block_And_Map(HB* hb, BB* bb)
//	void HB_Remove_Block_And_Map(HB* hb, BB* bb)
//	  Add/remove a block to bit set and adjust map.
//
//  
//	BOOL HB_Contains_Block(HB* hb, BB* bb)
//	  Returns true if a block is part of a given hyperblock.
//
//	void HB_Add_BBs_And_Map(HB* hb, BB_SET* bbset)
//	void HB_Copy_BBs_And_Map(HB* hb, BB_SET* bbset)
//  	  Add/copy bb's in set to a hyperblock and set up map
//
//	void HB_Remove_Map(HB* hb)
//	  Remove map for all blocks in a partially formed hyperblock that
//	  has been discarded.
//
//	INT HB_Flags(HB* hb)
//	BOOL HB_Flags_Check(HB* hb, INT flag)
//	void HB_Flags_Set(HB* hb, INT flag)
//	void HB_Flags_Clear(HB* hb)
//	  Access flags field of hyperblock.
//
//	HB_Predecessor_Count(HB* hb, BB_MAP& predecessor_count)
//  	  Utility routine to make a map that contains a count of all
//  	  of the predecessors of a block within the hyperblock.
//
//      BB * Force_If_Convert(BB_REGION& bb_region)
//        Fully if-convert a region, if possible. If the conversion happened, the single resulting
//        BB is returned. If it did not happen, NULL is returned. 
//
//      void HB_Remove_Deleted_Blocks(void)
//        remove all deleted blocks from the HB_list list. To be run before trying to schedule.
//
//      void Get_HB_Blocks_List(list<BB *> &blocks, HB* hb)
//        fill in a list of BB's from a hyperblock.
//
//      void Setup_HB_bb_map(void) 
//        recreate the BB->HB mapping 
/////////////////////////////////////
/////////////////////////////////////

#ifndef HB_H_INCLUDED
#define HB_H_INCLUDED

#include <list>
#include "bb.h"
#include "findloops.h"

//
// Some typedefs to make using STL lists easier
//
typedef std::list<BB*> HB_bb_list;

/////////////////////////////////////
//
// Hyperblock list structure.
//
/////////////////////////////////////

struct HB {
  BB* entry;
  BB* exit;
  BB* fall_thru_exit;
  HB_bb_list block_list;
  BB_SET* blocks;
  INT8 flags;
  public:
  void Print(void);
};

extern BB_MAP HB_bb_map;

/////////////////////////////////////
//
// Hyperblock flags.
//
/////////////////////////////////////
#define HB_LOOP_FLAG	0x0001
#define HB_ERASE_FLAG   0x0002
#define HB_SEEN_FLAG    0x0004

/////////////////////////////////////
inline INT
HB_Flags(HB* hb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb->flags;
}

/////////////////////////////////////
inline void
HB_Flags_Clear(HB* hb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->flags = 0;
}

/////////////////////////////////////
inline BOOL
HB_Flags_Check(HB* hb, INT flag)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb->flags & flag;
}

/////////////////////////////////////
inline void
HB_Flags_Set(HB* hb, INT flag)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->flags |= flag;
}

/////////////////////////////////////
inline BB*
HB_Entry(HB* hb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb->entry;
}

/////////////////////////////////////
inline void
HB_Entry_Set(HB* hb, BB* entry)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->entry = entry;
}


/////////////////////////////////////
inline BB*
HB_Exit(HB* hb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb->exit;
}

/////////////////////////////////////
inline void
HB_Exit_Set(HB* hb, BB* exit)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->exit = exit;
}

/////////////////////////////////////
inline BB*
HB_Fall_Thru_Exit(HB* hb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb->fall_thru_exit;
}

/////////////////////////////////////
inline void
HB_Fall_Thru_Exit_Set(HB* hb, BB* fall_thru_exit)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->fall_thru_exit = fall_thru_exit;
}

/////////////////////////////////////
inline BB_SET*
HB_Blocks(HB* hb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb->blocks;
}

/////////////////////////////////////
inline void
HB_Blocks_Set(HB* hb, BB_SET* bb_set)
/////////////////////////////////////
//  See interface description
/////////////////////////////////////
{
  hb->blocks = bb_set;
}

/////////////////////////////////////
inline void
HB_Blocks_Copy(HB* hb, BB_SET* bb_set)
/////////////////////////////////////
//  See interface description
/////////////////////////////////////
{
  hb->blocks = BB_SET_Copy(bb_set, &MEM_pu_pool);
}

/////////////////////////////////////
inline void
HB_Add_Block(HB* hb, BB* bb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->blocks = BB_SET_Union1D(HB_Blocks(hb), bb, &MEM_pu_pool);
}

/////////////////////////////////////
inline void
HB_Add_Block_And_Map(HB* hb, BB* bb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->blocks = BB_SET_Union1D(HB_Blocks(hb), bb, &MEM_pu_pool);
  BB_MAP_Set(HB_bb_map, bb, hb);
}


/////////////////////////////////////
inline void
HB_Add_BB_SET(HB* hb, BB_SET* bb_set, MEM_POOL* pool)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->blocks = BB_SET_UnionD(HB_Blocks(hb), bb_set, pool);
}

/////////////////////////////////////
inline void
HB_Intersect_BB_SET(HB* hb, BB_SET* bb_set)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->blocks = BB_SET_IntersectionD(HB_Blocks(hb), bb_set);
}

/////////////////////////////////////
inline void
HB_Remove_Block(HB* hb, BB* bb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->blocks = BB_SET_Difference1D(HB_Blocks(hb), bb);
}

/////////////////////////////////////
inline void
HB_Remove_Blocks(HB* hb, BB_SET* bbs)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->blocks = BB_SET_DifferenceD(hb->blocks, bbs);
}

/////////////////////////////////////
inline void
HB_Remove_Block_And_Map(HB* hb, BB* bb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb->blocks = BB_SET_Difference1D(HB_Blocks(hb), bb);
  BB_MAP_Set(HB_bb_map, bb, NULL);
}

/////////////////////////////////////
inline BOOL
HB_Contains_Block(HB* hb, BB* bb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return BB_SET_MemberP(HB_Blocks(hb), bb);
}


void Get_HB_Blocks_List(std::list<BB *> &blocks, HB* hb);

/////////////////////////////////////
inline void
HB_Remove_Map(HB* hb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  BB* bb;
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    BB_MAP_Set(HB_bb_map, bb, NULL);
  }
}

inline HB_bb_list * HB_Blocks_List(HB *hb)
{
  return &(hb->block_list);
}

extern HB* HB_Alloc(MEM_POOL* pool);

extern void HB_Init(void);
extern void HB_Form_Hyperblocks(RID* rid, const BB_REGION* bb_region);
//
// Some typedefs to make using STL lists easier
//
typedef std::list<BB*> HB_bb_list;

extern void HB_Predecessor_Count(HB* hb, BB_MAP& predecessor_count);

extern void HB_Add_BBs_And_Map(HB* hb, BB_SET* bbset);
extern void HB_Copy_BBs_And_Map(HB* hb, BB_SET* bbset);

extern MEM_POOL MEM_HB_pool;

extern std::list<HB *> HB_list;

extern float HB_minimum_priority;

//
// Used to control whether or not we update liveness information on a
// per hyperblock basis.  There are some problems with the current 
// interface to the liveness routines in that they don't deal well
// with the "messy" sort of regions that we want analyzed (i.e. the
// exit blocks reenter the region).  For now, we're updating for the
// whole PU.
//
extern BOOL HB_do_local_liveness;

//
// Do we allow any tail duplication to happen at all? 
//
extern BOOL HB_allow_tail_duplication;

extern BOOL HB_did_tail_duplication;


////////////////////////////////////////////////////////////////
// BB * Force_If_Convert(LOOP_DESCR *loop, BOOL allow_multi_bb)
// Fully if-convert a region, if possible. If the conversion happened, the 
// single resulting BB is returned. If it did not happen, NULL is returned. 
// If allow_multi_bb is true, if-conversion is performed if possible, even 
// if multiple BB's will be produced.
////////////////////////////////////////////////////////////////

extern BB * Force_If_Convert(LOOP_DESCR *loop, BOOL allow_multi_bb);

extern void HB_Remove_Deleted_Blocks(void);

extern void Setup_HB_bb_map(void);

#endif
#ifdef KEY
extern BOOL hammock_region;
#endif
