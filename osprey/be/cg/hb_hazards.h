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


/* =======================================================================
 * =======================================================================
 *
 *  Module: hb_hazards.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:25-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.hb_hazards.h $
 *
 *  Description:
 *  ============
 *
 *  Interface to the all hazard detection (machine hazards, bundling etc.)
 *  routines.
 *
 * =======================================================================
 * =======================================================================
 */
#ifndef hb_hazards_INCLUDED
#define hb_hazards_INCLUDED

#include "cgtarget.h"

// Creation and printing of scheduling notes.

typedef struct {
  INT schedule_length;
  INT block_parallelism;
  PRC_INFO prc_info;
  SRCPOS loop_srcpos;	// only valid for loop head BBs
} NOTE_SCHED;

inline OP*
BB_last_real_op(BB *bb) {
  OP *last_op;
  for (last_op = BB_last_op(bb); last_op; last_op = OP_prev(last_op)) {
    if (!OP_dummy(last_op)) break;
  }
  return last_op;
}

// Returns the last real OP preceeding <op>, i.e with ignoring all dummy OPs.
inline OP*
Last_Real_OP(OP *op)
{
  OP *last_real_op;
  for (last_real_op = OP_prev(op); last_real_op; last_real_op = OP_prev(last_real_op)) {
    if (!OP_dummy(last_real_op)) break;
  }

  return last_real_op;
}

extern void Handle_All_Hazards (BB *bb);
extern void Add_Scheduling_Notes_For_Loops ();

extern BOOL Is_Delay_Slot_Op (OP *op, BB *bb);
extern void Add_Scheduling_Note (BB *bb, void *bbsch);

#endif /* hb_hazards_INCLUDED */

