/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libf/pxf/pxfisatty.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <unistd.h>

#if defined(_SOLARIS) || defined(_MIPSEB)
#include <stdlib.h>
#endif

#if defined(_UNICOS) || defined(_MIPSEB)
#include <unistd.h>
#endif

/*
 *  PXFISATTY  -- determine if file descriptor corresponds
 *                to file descriptor
 *  (section 4.7.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFISATTY(IFILDES,ISATTY,IERROR)
 *     INTEGER IFILDES,IERROR
 *     LOGICAL ISATTY
 *
 *  Function definition:
 *  The routine PXFISATTY uses isatty(3C) to determine if
 *  IFILDES is a valid file descriptor for a terminal.
 *
 *  Description of arguments:
 *
 *  IFILDES is an input integer variable containing the
 *          file descriptor to be checked for an associated
 *          terminal.
 *
 *  ISATTY  is an output logical variable which is .TRUE.
 *          when IFILDES is a file descriptor with an
 *          associated terminal, otherwise ISATTY is
 *          .FALSE..
 *
 *  IERROR  is an output integer variable for PXFISATTY's
 *          completion status. IERROR may contain:
 *
 *           zero  - PXFISATTY was successful.
 *
 */

#ifdef _UNICOS
void
PXFISATTY(
#else
void
_PXFISATTY(
#endif
	   _f_int *IFILDES,
	   _f_log *ISATTY,
	   _f_int *IERROR
)
{
  *ISATTY = _btol(isatty((int)*IFILDES));
  *IERROR = 0;
}

#ifndef _UNICOS
void
pxfisatty_(
	   _f_int *IFILDES,
	   _f_log *ISATTY,
	   _f_int *IERROR
)
{
  _PXFISATTY(IFILDES,ISATTY,IERROR);
}
#endif
