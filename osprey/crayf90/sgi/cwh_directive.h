/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_directive.h
 * $Revision: 1.7 $
 * $Date: 05/09/22 10:54:47-07:00 $
 * $Author: gautam@jacinth.keyresearch $
 * $Source: crayf90/sgi/SCCS/s.cwh_directive.h $
 *
 * Description: Entry points into cwh_directive.c
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_DIRECTIVE_INCLUDED
#define CWH_DIRECTIVE_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_directive.h $";
#endif /* _KEEP_RCS_ID */

extern int parallel_do_count;
  /* marks the next n do loop as parallel loops */

extern WN *
cwh_mp_region(       WN_PRAGMA_ID wn_pragma_id,
                     int threadcount,
                     int datacount,
                     int ontocount,
                     int reductioncount,
                     int chunkcount,
		     int is_omp);


/* Adds all the deferred DO loop directives to the current block */
extern void
cwh_directive_insert_do_loop_directives(void); 

/* Adds a directive to the deferred DO loop list */
extern void
cwh_directive_add_do_loop_directive(WN *directive);

/* add fwd/back barriers, list of names and maybe insert WN between barriers*/

extern void
cwh_directive_barrier_insert(WN *ins, int  args) ;
// Bug 3836
#ifdef KEY
extern void 
cwh_directive_set_PU_flags(BOOL nested);
#endif

#endif /* CWH_DIRECTIVE_INCLUDED */

