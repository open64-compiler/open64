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
//  Blocks are selected for addition to the hyperblock on a per path
//  basis.  That is, all control flow paths from the entry block of the
//  region to an exit block are enumerated, and each considered for
//  inclusion in the hyperblock as a unit.  The final collection of the
//  blocks in the hyperblock, then, is the union of all blocks along the
//  selected paths.
//  
//  The paths in the region are assigned a priority, and paths are
//  evaluated in priority order.  Factors that are considered when
//  calculating the path's priority are: execution frequency, dependence
//  height, instruction count, and hazard conditions on the path (these
//  include things such as subroutine calls, and unresolvable memory
//  stores).  Execution frequency is used to exclude paths that are
//  infrequently executed.  Dependence height is factored in to reduce the
//  priority of path with relatively large dependence height.  Instruction
//  counts prevents paths with large instruction counts (and hence heavy
//  resource requirements) from having a high priority.  Since the
//  presence of hazardous instructions in the path with reduce the
//  effectiveness of optimization and scheduling, the priority of paths
//  containing them is scaled down significantly.
//  
//  The selection algorithm begins by choosing the highest priority path.
//  Paths are then added to the hyperblock in priority order by
//  considering their characteristics relative to the currently selected
//  hyperblock.  The three factors considered are:
//  
//  	1. The resources required by the new path may not, when
//         combined with the resources already in use by the
//         hyperblock, exceed the estimated available resources.
//  
//  	2. The dependence height of the path may not exceed the
//         dependence height of the highest priority path by more
//         than a predefined fraction.
//  
//  	3. The priority of the path must be within some fraction of
//         the last priority added.  This prevents the addition of
//         paths with disproportionally low priorities.
//  
//  The resultant hyperblock should be able to be scheduled in close to
//  the time required for the highest priority path.
//
//  One thing that we must realize here is that this algorithm is highly
//  dependent upon the presence of feedback.  We must consider alternative
//  heuristics when we only have static frequency analysis available.  We
//  will downplay the importance of frequency and increase the importance
//  of like dependence heights.  If we tend to group things together that
//  have similar critical path lengths, then we reduce the risk of hurting
//  shorter, but frequently executed, paths that we might have otherwise
//  included into the hyperblock based on the statically estimated
//  frequencies.
//
//  External routines:
//
//	BOOL HB_Block_Select(HB* hb, BOOL profitable_ifc)
//	  Driver for block selection algorithm described above.  Returns
//	  TRUE if blocks were selected to form a hyperblock.
//
/////////////////////////////////////

#ifndef HB_BLOCK_SELECT_INCLUDED
#define HB_BLOCK_SELECT_INCLUDED

extern BOOL HB_Block_Select(HB* candidate, BOOL profitable_ifc);

#endif
