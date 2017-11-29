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

//  gra_cflow.h
/////////////////////////////////////
//
//  Description:
//
//      Functions for manipulating the flow graph on behalf of GRA.
//
//  Splitting entry and exit BBs:
//
//      Function entry and exit BBs contain copies from/to the save-TNs for
//      the callee-saves registers.  In the entry BBs, these take the form of
//      copies from dedicated TNs into the save-TNs.  In the exit BBs, these
//      take the form of copies into the dedicated TNs from the save-TNs.  The
//      referenced to the dedicated TNs in these blocks has the effect of
//      preventing a save register from being allocated to any other TN in the
//      entry or exit blocks.  This can cause a number of problems, all of
//      which result in some TN that should be allocated to a save register
//      not getting one.
//
//      In order to improve this situation, we split up the entry/exit blocks
//      before GRA and undo the damage after GRA.  The blocks are split into
//      two parts with new entry blocks containing only the code through the
//      final callee-saves copy (entry blocks) or starting with the first
//      callee-saves copy (exit blocks).  The result is that the wired LRANGEs
//      for the callee-saves registers are allocated in the entry and exit
//      blocks, which is good for preferencing; but not in the code following
//      the entry or exit blocks, which is good for getting callee-saves
//      registers when needed.
//
//          void GRA_Split_Entry_And_Exit_BBs(BOOL is_region)
//            Split the entry blocks up.
//            
//          void GRA_Join_Entry_And_Exit_BBs(void)
//            Undo what GRA_Split_Entry_And_Exit_BBs did.
//
//
//	Miscellaneous functions:
//
//	    void GRA_Add_Call_Spill_Block(BB* bb, BB* succ)
//	      Add block between call and its successor.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:29-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_cflow.h $


#ifndef GRA_CFLOW_INCLUDED
#define GRA_CFLOW_INCLUDED

extern void GRA_Split_Entry_And_Exit_BBs(BOOL is_region);
extern void GRA_Join_Entry_And_Exit_BBs(void);
extern void GRA_Add_Call_Spill_Block(BB *bb, BB* succ);

#endif
