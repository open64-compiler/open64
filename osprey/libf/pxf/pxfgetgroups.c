/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libf/pxf/pxfgetgroups.c	92.2	09/15/99 10:41:12"


#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <unistd.h>

#ifdef _UNICOS
#include <sys/types.h>
#include <sys/param.h>
#elif	defined(_LITTLE_ENDIAN)
#include <sys/types.h>
#include <sys/param.h>
#else
#include <limits.h>
#endif

/*
 *  PXFGETGROUPS  -- get supplementary group IDs
 *  (section 4.2.3 of Posix 1003.9-1992)
 *
 *  Call from Fortran:
 *
 *     SUBROUTINE PXFGETGROUPS(IGIDSETSIZE,IGROUPLIST,NGROUPS,IERROR)
 *     INTERGER IGIDSETSIZE,IGROUPLIST(IGIDSETSIZE),NGROUPS,IERROR
 *
 *  Where:
 *
 *  IGIDSETSIZE  is an input integer variable containing the size of
 *               the IGROUPLIST integer array.
 *
 *  IGROUPLIST   is an output integer variable or array element that
 *               will contain a set of supplemental group IDs for the
 *               calling process.
 *
 *  NGROUPS      is an output interger variable that will contain the
 *               number of supplemental group IDs for the calling process.
 *
 *  IERROR       is an output integer variable that will contain
 *               the status:
 *
 *                zero    -  variable was changed.
 *
 *                nonzero - PXFGETGROUPS was not successful.
 *
 *               PXFGETGROUPS may return any of the following error
 *               values:
 *
 *                EINVAL If the IGIDSETSIZE is not equal to zero and is
 *                       is less than the number of supplementary group
 *                       IDs.
 *
 */

#ifdef _UNICOS
void
PXFGETGROUPS(
#else
void
_PXFGETGROUPS(
#endif
	      _f_int *IGIDSETSIZE,
	      _f_int *IGROUPLIST,
	      _f_int *ngroups,
	      _f_int *IERROR
)
{
  gid_t grplistbuf[NGROUPS_MAX];
  int i, groups, gidsetsize;

  gidsetsize = *IGIDSETSIZE;

  /* get groups using a group list buffer of size NGROUPS_MAX */
  if ((groups = getgroups(NGROUPS_MAX, grplistbuf)) == -1) {
    *IERROR = errno;
  } else {
    /* check if user array IGROUPLIST is too small to hold all of the group IDs */
    if ((gidsetsize < groups) && (gidsetsize != 0)) {
      *IERROR = EINVAL;
    } else {

      /* If gidsetsize is zero the user just wants the number of
        * supplemental groups IDs, other wise the group IDs need
        * to be coppied into the IGROUPLIST array and the number of
        * groups assigned to ngroups user variable.
        */

      if (gidsetsize != 0) {
		
	/* copy the group ID elements into IGROUPLIST */
	for (i=0; i < groups; i++) {
	  IGROUPLIST[i] = grplistbuf[i];
	}
      }

      *IERROR = 0;
      *ngroups = groups;
    }
  }
}

#ifndef _UNICOS
void
pxfgetgroups_(
	      _f_int *IGIDSETSIZE,
	      _f_int *IGROUPLIST,
	      _f_int *ngroups,
	      _f_int *IERROR
)
{
  _PXFGETGROUPS(IGIDSETSIZE, IGROUPLIST, ngroups, IERROR);
}
#endif
