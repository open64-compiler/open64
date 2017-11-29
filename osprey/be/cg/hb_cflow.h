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


#ifndef HB_CFLOW_H_INCLUDED
#define HB_CFLOW_H_INCLUDED
// This file contains interface routines between CFLOW and hyperblock formation
// These routines are mainly to deal with what canb happen when blocks are merged or deleted.


/////////////////////////////////////
//  Replace block old_bb with block new_bb, keeping hyperblock strcutures
//  up to date.
/////////////////////////////////////
extern void HB_CFLOW_Replace_Block(BB *old_bb, BB *new_bb);

////////////////////////////////////////////////////////////////
//
// Test to make sure that merging two BBs would not break the hyperblock data structures
//
//////////////////////////////////////////////////////////////////
BOOL HB_CFLOW_Can_Merge_BBs(BB *bb1, BB *bb2);

////////////////////////////////////////////////////////////////
//
// Remove a block from the hyperblocks list
//
////////////////////////////////////////////////////////////////
extern void HB_CFLOW_Remove_Block(BB *bb);

//
// Also defined in hb.h
//
extern void Setup_HB_bb_map(void);

#endif
