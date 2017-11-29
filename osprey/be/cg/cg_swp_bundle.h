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
// ====================================================================
// ====================================================================
//
// Module: cg_swp_bundle.h
//
//   SWP_Bundle: This function will bundle the ops in the op_state,
//   denoting each SWP_OP with a slot number offset from the beginning
//   of the loop body.  Each OP will be marked as OP_end_group() and
//   OP_m_unit as appropriate, and every op will be marked as OP_bundled.
//   New noops will be inserted where necessary to provide legal bundling.
//
//   The noops are inserted at the end of the op_state, and we assume
//   the ops will be sorted on slot position by the emitter to put them
//   in the final bundled order.
// ====================================================================
// ====================================================================
//

#ifndef cg_swp_bundle_INCLUDED
#define cg_swp_bundle_INCLUDED "cg_swp_bundle.h"


void SWP_Bundle(SWP_OP_vector& op_state, bool trace);

void SWP_Dont_Bundle(SWP_OP_vector& op_state);

void SWP_Undo_Bundle(SWP_OP_vector& op_state, BB *body);

#endif // cg_swp_bundle_INCLUDED


