/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
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

//  Spill functions for GRA

//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_spill.h $


#ifndef GRA_SPILL_INCLUDED
#define GRA_SPILL_INCLUDED

#ifndef GRA_SPILL_RCS_ID
#define GRA_SPILL_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_spill_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_spill.h $ $Revision: 1.2 $";
#endif
#endif

extern void GRA_Note_Save_Below( LUNIT* lunit );
extern void GRA_Note_Restore_Above( LUNIT* lunit );
extern void GRA_Spill_Initialize(void);
extern void GRA_Note_Spill( LRANGE* lrange );
extern void GRA_Spill(void);
extern void GRU_Fuse(void);

extern void GRA_Remove_Predicates_Save_Restore(void);
#ifdef TARG_IA64
extern void Gen_UNAT_Spills_Entry_And_Exit_BB(void);
#endif
extern float priority_count;

#endif
