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


//
//  If conversion module of hyperblock formation.
//
/////////////////////////////////////
//
//  Algorithm description:
//
//  The if-conversion algorithm that we will use will essentially be the
//  same as that used by Mahlke, et. al. in the IMPACT compiler.  This is
//  a variant of the RK algorithm from Park and Schlansker.  The algorithm
//  groups blocks into equivalence classes based on control dependence.
//  The blocks that are placed in the same equivalence class will be
//  controlled by the same predicate.  The predicates are set in those
//  predecessor blocks that are the source of the control dependences.
//  The algorithm, then, is roughly as follows:
//  
//  	Foreach block in the region being if-converted
//  	   Determine predicate assignment for the block
//  	   Insert predicate calculations for the block
//  
//  Each of these actions will be described below.
//  
//  
//  Predicate Assignment
//  --------------------
//  
//  As was mentioned above, predicates are assigned to blocks based on
//  control dependence equivalence classes.  That is, any two blocks that
//  share precisely the same control dependences (same blocks, same edges)
//  can share the same predicate register.  The IMPACT compiler ignores
//  edges that exit the hyperblock when calculating control dependence
//  (this is the primary variation from Park and Schlansker).  Beyond
//  compile time, the benefits of this are unclear to me.  For the moment,
//  we will include the exiting arcs.  The algorithm for predicate assignment
//  for block X would be as follows:
//  
//  	Calculate control dependences for X
//  	If this set of control dependences has been seen before then
//  	  assign the associated predicate to X
//  	else
//  	  get next predicate and associate it with X's control dependence set
//      endif
//  	change ops in X to predicated form
//  	remove branch if not an exit
//  
//  
//  Insert Predicate Calculations
//  -----------------------------
//  
//  Predicate calculations will have to be placed in each of the blocks that 
//  is a source of a control dependence in the equivalence class to which the
//  predicate is assigned.
//
/////////////////////////////////////
//
//  Externally Visible routines:
//
//	void HB_If_Convert(HB* hb, list<HB_CAND_TREE*>& candidate_regions)
//	  Removes non-exit branches and predicates the instructions in
//	  the blocks selected for the hyperblock. <candidate_regions> is
//        passed to incorporate any dynamic updates to original CFG.
//
//      BOOL HB_Safe_For_If_Conversion(HB *hb)
//        Return TRUE if the hyperblock contains no side entrances. This
//        is used to screen out hyperblock candidates during the simple if-conversion
//        phase of the hyperblock formation. 
//
/////////////////////////////////////

#ifndef HB_IF_CONVERT_H_INCLUDED
#define HB_IF_CONVERT_H_INCLUDED

extern void HB_If_Convert(HB* hb, std::list<HB_CAND_TREE*>& candidate_regions);
extern BOOL HB_Safe_For_If_Conversion(HB* hb);

#endif
