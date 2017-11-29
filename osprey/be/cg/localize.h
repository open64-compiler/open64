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
 *  Module: localize.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:26-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.localize.h $
 *
 *  Description:
 *  ============
 *
 *  Localize any global TNs.
 *  The goal is to avoid the need for GRA by removing all global TNs.
 *  First we make a pass through the bb's to find any global TNs,
 *  then we insert save/restore code in each bb to turn those into local TNs.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef localize_INCLUDED
#define localize_INCLUDED

/* to get the definition of RID. */
#include "region_util.h"

/*
 * Change any global TNs into local TNs
 * within the current REGION or the whole PU if rid is NULL
 */
extern void Localize_Any_Global_TNs ( RID *rid );

/*
 * Check if a dedicated TN is global, and if so, make it local.
 * Our assumptions are that a use of a param reg should be in the
 * entry block, a def of a param reg should be in the call block,
 * the use of a return reg should be in the block following a call block,
 * and the def of a return reg should be in the exit block.
 */
extern void Check_If_Dedicated_TN_Is_Global (TN *tn, BB *current_bb, BOOL def);

/* When using GRA, still want to localize dedicated tns involved in 
 * calls that cross bb's, and replace dedicated TNs involved in 
 * REGION interface with the corresponding allocated TNs from 
 * previously compiled REGIONs.
 */
extern void Localize_or_Replace_Dedicated_TNs (void);

#endif /* localize_INCLUDED */
