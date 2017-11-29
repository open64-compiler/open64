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
//  Tail duplication module of hyperblock formation.
//
/////////////////////////////////////
//
//  Algorithm description:
//
//  Duplicate blocks in order to remove side entrances to the hyperblock.
//  Blocks will only be duplicated once, regardless of how many side
//  entrances may reach them.
//
//  Ultimately, we'd like to be a little smarter about how we tail duplicate
//  so as to avoid unnecessary impacts on the I-cache.  For now, however,
//  it is not yet clear how best to do this so we are going to tail duplicate
//  unconditionally.
//
/////////////////////////////////////
//
//  Externally visible routines:
//
//	BOOL HB_Tail_Duplicate(HB* hb, BB_MAP duplicate,
//                             BOOL post_tail_duplication)
//	  Perform tail duplication on each hyperblock.
//
/////////////////////////////////////

#ifndef HB_TAIL_DUPLICATION_H_INCLUDED
#define HB_TAIL_DUPLICATION_H_INCLUDED

#include "hb.h"

extern BOOL HB_Tail_Duplicate(HB* hb, BB_MAP duplicate,
	                      HB_bb_list& duplicate_bbs,
                              BOOL post_tail_duplication);

#endif





