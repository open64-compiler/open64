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


#ifndef misalign_INCLUDED
#define misalign_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: misalign.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/misalign.h,v $
 *
 * Revision history:
 *  30-Apr-94 - Original Version
 *
 * Description:
 *
 * Declare an enumerated type and global option variables required by
 * the clients of both irbmem.h and memmodel.h, since we can't seem to
 * get away with an incomplete enumerated type.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *misalign_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

/* Misalignment models -- see memmodel.h for global defaults: */
typedef enum {
  MIS_NOSPEC,	/* No misalignment choice specification */
  MIS_NONE,	/* Assume aligned until proven otherwise */
  MIS_CAST,	/* Base alignment on type, trust casts */
  MIS_TYPED	/* Base alignment on type, ignore casts */
} MISALIGNMENT;

/* Misalignment models -- set up in memmodel.c:Configure_Alignment. */
extern MISALIGNMENT Misalignment_Mem;	/* Load/store misalignment */
extern MISALIGNMENT Misalignment_Parm;	/* Parameter misalignment */
extern MISALIGNMENT Misalignment_Agg;	/* Aggregate misalignment */

#ifdef __cplusplus
}
#endif
#endif /* misalign_INCLUDED */
