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



/*
 * Return Codes used by the compiler phases,
 * used to communicate between the driver and its children.
 */

#ifndef __RCODES_H
#define __RCODES_H

#ifdef INCLUDE_ID
static char *rcodes_h_id="$Header: /proj/osprey/CVS/open64/osprey1.0/include/cmplrs/rcodes.h,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
#endif

/* 
 * Ucode return codes
 */
#define EXEC_UMERGE	255	/* call umerge */

/*
 * Mongoose return codes
 */

#define RC_OKAY			0	/* executed successfully */
#define RC_INTERNAL_ERROR	1	/* a compiler error */
#define RC_USER_ERROR		2	/* a user error */
#define RC_NORECOVER_USER_ERROR	4	/* a user error can't recover from */
#define RC_UNIMPLEMENTED_ERROR	8	/* uses an unimplemented feature */
#define RC_NEED_INLINER		16	/* flag that we need the inliner */
#define RC_SYSTEM_ERROR		32	/* a O/S or environment error */
#define RC_GCC_ERROR		33	/* gcc's error number */
#ifdef KEY
#define RC_GCC_INTERNAL_ERROR	34	/* gcc internal error */
#endif
#define RC_OVERFLOW_ERROR	64	/* an overflow error; try recompiling */

#endif
