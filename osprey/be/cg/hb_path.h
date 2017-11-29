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
//  Hyperblock path selction module.  Enumerate paths in candidate region
//  and provide data structure for storing and manipulating data about
//  the paths.
//
//  External routines:
//
//	float HB_PATH_Hazard_Multiplier(HB_PATH* hb_path)
//	void  HB_PATH_Hazard_Multiplier_Set(HB_PATH* hb_path,
//	                                    float hazard_multiplier)
//	  Access hazard multiplier for path (indication of presence of
//	  calls or aliased memory references on path).
//
//	float HB_PATH_Probability(HB_PATH* hb_path)
//	void  HB_PATH_Probability_Set(HB_PATH* hb_path, float probability)
//	  Access probability with which the path is executed.
//
//	float HB_PATH_Priority(HB_PATH* hb_path)
//	void  HB_PATH_Priority_Set(HB_PATH* hb_path, float priority)
//	  Access priority of path.
//
//	INT  HB_PATH_Num_Ops(HB_PATH* hb_path)
//	void HB_PATH_Num_Ops_Set(HB_PATH* hb_path, INT num_ops) 
//	  Access count of instructions on path.
//
//	INT  HB_PATH_Num_CBranch_Ops(HB_PATH* hb_path)
//	void HB_PATH_Num_CB_Ops_Set(HB_PATH* hb_path, INT num_ops)
//	  Access count of conditional instructions on path.
//
//	INT  HB_PATH_Num_UCBranch_Ops(HB_PATH* hb_path)
//	void HB_PATH_Num_UCB_Ops_Set(HB_PATH* hb_path, INT num_ops)
//	  Access count of unconditional branch instructions on path.
//
//	INT  HB_PATH_Schedule_Height(HB_PATH* hb_path)
//	  Access height of schedule in terms of number of cycles.
//
//      CG_SCHED_EST* HB_PATH_Sched_Est(HB_PATH* hb_path)
//      void HB_PATH_Sched_Est_Set(HB_PATH* hb_path, CG_SCHED_EST* sched_est)
//	  Get/set schedule estimate for pat.h
//
//	BB_SET* HB_PATH_Blocks(HB_PATH* hb_path)
//	void   HB_PATH_Blocks_Set(HB_PATH* hb_path)
//	  Return/Set BB_SET of BBs in the path.
//
//	void HB_PATH_Add_Block(HB_PATH* hb_path, BB* bb, MEM_POOL* pool)
//	void HB_PATH_Remove_Block(HB_PATH* hb_path, BB* bb, MEM_POOL* pool)
//	void HB_PATH_Add_BB_SET(HB_PATH* hb_path, BB* bb, MEM_POOL* pool)
//	  Manipulate contents of bb_set representing the path.
//
//	BOOL HB_PATH_Contains_Block(HB_PATH* hb_path, BB* bb)
//	  Returns true if a block is part of a given path.
//
//	void HB_PATH_Add_BBs(HB_PATH* hb_path, BB_SET* bbset)
//  	  Add bb's in set to a path and set up map
//

#ifndef HB_PATH_H_INCLUDED
#define HB_PATH_H_INCLUDED

#include "cg_sched_est.h"
#include "bb_set.h"

/////////////////////////////////////
//
//  Path descriptor
//
/////////////////////////////////////

struct HB_PATH {
  float hazard_multiplier;
  float probability;
  float priority;
  INT num_ops;
  INT num_cb_ops;  // no. of conditional branch ops
  INT num_ucb_ops; // no. of unconditional branch ops
  CG_SCHED_EST* sched_est;
  BB_SET* blocks;
};


/////////////////////////////////////
inline float
HB_PATH_Hazard_Multiplier(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->hazard_multiplier;
}

/////////////////////////////////////
inline void
HB_PATH_Hazard_Multiplier_Set(HB_PATH* hb_path, float hazard_multiplier)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb_path->hazard_multiplier = hazard_multiplier;
}

/////////////////////////////////////
inline float
HB_PATH_Probability(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->probability;
}

/////////////////////////////////////
inline void
HB_PATH_Probability_Set(HB_PATH* hb_path, float probability)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb_path->probability = probability;
}


/////////////////////////////////////
inline float
HB_PATH_Priority(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->priority;
}

/////////////////////////////////////
inline void
HB_PATH_Priority_Set(HB_PATH* hb_path, float priority)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb_path->priority = priority;
}


/////////////////////////////////////
inline INT
HB_PATH_Num_Ops(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->num_ops;
}

/////////////////////////////////////
inline void
HB_PATH_Num_Ops_Set(HB_PATH* hb_path, INT num_ops)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb_path->num_ops = num_ops;
}

/////////////////////////////////////
inline INT
HB_PATH_Num_CBranch_Ops(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->num_cb_ops;
}

/////////////////////////////////////
inline void
HB_PATH_Num_CB_Ops_Set(HB_PATH* hb_path, INT num_cb_ops)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb_path->num_cb_ops = num_cb_ops;
}

/////////////////////////////////////
inline INT
HB_PATH_Num_UCBranch_Ops(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->num_ucb_ops;
}

/////////////////////////////////////
inline void
HB_PATH_Num_UCB_Ops_Set(HB_PATH* hb_path, INT num_ucb_ops)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  hb_path->num_ucb_ops = num_ucb_ops;
}

/////////////////////////////////////
inline CG_SCHED_EST*
HB_PATH_Sched_Est(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->sched_est;
}

/////////////////////////////////////
inline void
HB_PATH_Sched_Est_Set(HB_PATH* hb_path, CG_SCHED_EST* sched_est)
/////////////////////////////////////
//  See interface description
/////////////////////////////////////
{
  hb_path->sched_est = sched_est;
}

/////////////////////////////////////
inline INT
HB_PATH_Schedule_Height(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (HB_PATH_Sched_Est(hb_path)) {
    return CG_SCHED_EST_Resource_Cycles(HB_PATH_Sched_Est(hb_path));
  }
  return 0;
}

/////////////////////////////////////
inline BB_SET*
HB_PATH_Blocks(HB_PATH* hb_path)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return hb_path->blocks;
}

/////////////////////////////////////
inline void
HB_PATH_Blocks_Set(HB_PATH* hb_path, BB_SET* bb_set)
/////////////////////////////////////
//  See interface description
/////////////////////////////////////
{
  hb_path->blocks = bb_set;
}

/////////////////////////////////////
inline void
HB_PATH_Add_Block(HB_PATH* hb_path, BB* bb, MEM_POOL* pool)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  BB_SET *blocks = HB_PATH_Blocks(hb_path);
  blocks = BB_SET_Union1D(blocks, bb, pool);
}

/////////////////////////////////////
inline void
HB_PATH_Add_BB_SET(HB_PATH* hb_path, BB_SET* bb_set, MEM_POOL* pool)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  BB_SET *blocks = HB_PATH_Blocks(hb_path);
  blocks = BB_SET_UnionD(blocks, bb_set, pool);
}


/////////////////////////////////////
inline void
HB_PATH_Remove_Block(HB_PATH* hb_path, BB* bb, MEM_POOL* pool)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  BB_SET *blocks = HB_PATH_Blocks(hb_path);
  blocks = BB_SET_Difference1D(blocks, bb);
}

/////////////////////////////////////
inline BOOL
HB_PATH_Contains_Block(HB_PATH* hb_path, BB* bb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return BB_SET_MemberP(HB_PATH_Blocks(hb_path), bb);
}

/////////////////////////////////////
inline HB_PATH*
HB_PATH_Alloc(MEM_POOL* pool)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  HB_PATH* hb_path = TYPE_MEM_POOL_ALLOC(HB_PATH, pool);
  HB_PATH_Blocks_Set(hb_path,BB_SET_Create_Empty(PU_BB_Count+2, pool));
  HB_PATH_Hazard_Multiplier_Set(hb_path, 1.0);
  HB_PATH_Probability_Set(hb_path, 1.0);
  HB_PATH_Priority_Set(hb_path, 0.0);
  HB_PATH_Num_Ops_Set(hb_path, 0);  
  return hb_path;
}


/////////////////////////////////////
//
//  Struct for using sort on list of 
//  paths.
//
/////////////////////////////////////
struct HB_PATH_Priority_Compare {
  BOOL operator () (HB_PATH* p1, HB_PATH* p2) {
    return HB_PATH_Priority(p1) > HB_PATH_Priority(p2);
  }
};

#endif
