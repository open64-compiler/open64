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


//-*-c++-*-
//                     Array Scalarization
//                     -------------------
//
// Description:
//
// 	In loops, convert things that look like
//	do i
//	  a[i] = ...
//	  ... = a[i]
//
//	into
//
//	do i
//	  t = ...
//	  a[i] = t
//	  ... = t
//
//	This is useful because 
//	  1) It gets rid of loads
//	  2) If it gets rid of all the loads to a local array then
//	     the array equivalencing algorithm will get rid of the array
//
//	Because SWP will do 1 as well as we do, we'll only apply this
//	algorithm to local arrays (Although it's trivial to change this).
//
// Exported functions:
//
// void Scalarize_Arrays(ARRAY_DIRECTED_GRAPH16 *dep_graph,
//	BOOL do_variants, BOOL do_invariants)
//
//
//
//


/**
*** $Source$
**/

#ifndef SCLRZE_RCS_ID
#define SCLRZE_RCS_ID
#ifdef _KEEP_RCS_ID
static char *sclrze = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */
#endif

#ifndef SCLRZE_DECLARE

void Scalarize_Arrays(ARRAY_DIRECTED_GRAPH16 *dep_graph,
                      BOOL do_variants, BOOL do_invariants, REDUCTION_MANAGER *red_manager, WN * wn);

#endif  // SCLRZE_DECLARE

