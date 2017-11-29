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


//-*-C++-*-
//                     Memory invariant removal
//                     ------------------------
//
// Description:
//
// 	In loops, convert things that look like
//	do i
//	  a[j] = ...
//	  ... = a[j]
//
//	into
//
//      t = a[j]
//	do i
//	  t = ...
//	  ... = t
//      a[j] = t
//
//      and so on for deeper nested loops.
//
// Exported functions:
//
// void MIR_Update_Dependences(WN* wn_loop, DYN_ARRAY<WN*>* new_uses)
//
//	Update the dependences inside 'wn_loop' on nodes which are
//	descendants of 'new_uses'.
//
// void Minvariant_Removal(WN *func_nd, ARRAY_DIRECTED_GRAPH16 *dg);
//
//      Move memory invariants out of loops as far as possible.
//      Do loops must all execute at least once.
//
// void MIR_Has_Messy_Subscript(WN* wn_loop, INT can_hoist[], 
//      INT lowest_depth[], BOOL initialize)
//
//	For all non-bound references in the loop 'wn_loop', set 
//	'can_hoist[i]' if there is a hoistable messy subscript at the loop
//      at depth 'i'. If 'can_hoist[i] > 0', set 'lowest_depth[i]' to the
//      depth of the outermost loop enclosing a reference hoisted at loop
//      depth 'i'.  If 'initialize' is TRUE, initialize 'can_hoist[]' and
//      'lowest_depth[]'.
//
// void MIR_Hoist_Messy_Subscripts(WN* wn_loop)
//
//	For the loop nest 'wn_loop', hoist all hoistable messy array
//	subscripts inside loops (not in the bounds.)
//

/* ====================================================================
 *
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 * ====================================================================
 */

#ifndef MINVARIANT_RCS_ID
#define MINVARIANT_RCS_ID
#ifdef _KEEP_RCS_ID
static char *minvariant = "$Source$ $Revision$";
#endif
#endif

#ifndef MINVARIANT_DECLARE

class WN;
class ARRAY_DIRECTED_GRAPH16;

extern void Minvariant_Removal(WN *func_nd, ARRAY_DIRECTED_GRAPH16 *dg);
extern void MIR_Has_Messy_Subscript(WN* wn_loop, INT can_hoist[], 
  INT lowest_depth[], BOOL initialize); 
extern void MIR_Hoist_Messy_Subscripts(WN* wn_loop); 
extern void MIR_Update_Dependences(WN* wn_loop, DYN_ARRAY<WN*>* new_uses);

#endif
