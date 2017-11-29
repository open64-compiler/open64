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


/*-*-c++-*-*/
/* ====================================================================
 *
 * Module: region_main.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:58-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.region_main.h $
 *
 * Revision history:
 *  8-NOV-95 dahl - Original Version
 *
 * Description:
 *	Definitions for back end driver. Should only be included by
 *	be/be/driver.c
 * ====================================================================*/

#ifndef	region_main_INCLUDED
#define	region_main_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* Create and push the REGION_mem_pool. Create the region_id map, 
   used to track region info after regions have been lowered. */
extern INT REGION_Initialize(WN *, BOOL);

/* Pop the REGION_mem_pool */
extern void REGION_Finalize(void);

/* Iterate over a REGION tree, finding those REGIONs with no "earlier"
   sub-REGION.  When First or Next returns, iter points to a REGION_CS_ITER
   with "kid" representing the location in the REGION tree which we have just
   finished visiting. This amounts to postorder traversal of the RID tree. */
extern void REGION_CS_ITER_init( REGION_CS_ITER *, WN * );
extern BOOL REGION_CS_NoEarlierSub_While( REGION_CS_ITER * );
extern void REGION_CS_NoEarlierSub_Next ( REGION_CS_ITER * );
extern void REGION_CS_NoEarlierSub_First( REGION_CS_ITER *, WN *, RID_TYPE);
extern void REGION_CS_print( REGION_CS_ITER * );

/* Remove a region from the PU and mark location for insertion later */
extern WN  *REGION_remove_and_mark(WN *, REGION_CS_ITER *);
/* Insert new region at mark left by REGION_remove_and_mark */
extern void REGION_replace_from_mark(WN *, REGION_CS_ITER *);

/* These two phases are called by preopt (wodriver.c) for :p (lno off)	*/
/* and by lno (lnodriver.c) for :l (lno on)				*/
extern WN *Rail(WN *);
extern void Region_Bound_Info(WN *, struct DU_MANAGER *, 
			      struct ALIAS_MANAGER *);

/* generate consistent region boundary sets regardless of optimization level
   code is in be/region/region_bounds.cxx */
extern void Generate_region_boundaries(WN *, struct ALIAS_MANAGER *);

#ifdef __cplusplus
}
#endif
#endif /* region_main_INCLUDED */
