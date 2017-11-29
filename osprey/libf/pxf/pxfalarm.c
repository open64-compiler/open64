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


#pragma ident "@(#) libf/pxf/pxfalarm.c	92.1	06/29/99 11:36:06"


/*
 *  PXFALARM -- Schedule alarm.
 *  (section 3.4.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFALARM(iseconds, isecleft, ierror)
 *     INTEGER iseconds, isecleft, ierror
 *
 *  Where:
 *
 *	iseconds -  default integer input variable containing
 *	            the number of real-time seconds to wait before
 *	            sending the calling process a SIGALRM signal.
 *	isecleft -  default integer output variable containing
 *	            the number of seconds left until a previous
 *	            request would have generated a SIGALRM signal.
 *	ierror   -  default integer output variable containing
 *	            status:
 *
 *           zero	- PXFALARM was successful.
 *
 *  DESCRIPTION:
 *
 *  The subroutine procedure PXFALARM uses alarm() system call to
 *  wait iseconds before generating SIGALRM signal.  If a previous
 *  PXFALARM has time remaining, isecleft contains the number of
 *  seconds until the SIGALRM would have been generated.
 *
 *  NOTE:
 *
 *  Replace the use of subprocedure ALARM() with the PXFALARM()
 *  interface in Fortran programs for portability.
 *
 *  On UNICOS and UNICOS/mk systems, alarm() can be a subroutine
 *  or a function reference.  On IRIX systems, alarm() is a
 *  function reference that calls a procedure after so many
 *  seconds.
 */

#include <errno.h>
#include <fortran.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <unistd.h>

#ifdef _UNICOS
void
PXFALARM(
#else	/* _UNICOS */
void
_PXFALARM(
#endif	/* _UNICOS */
	_f_int *iseconds,
	_f_int *isecleft,
	_f_int *ierror
)
{
	unsigned int stat;
	*ierror = 0;
	stat = alarm(*iseconds);
	*isecleft = (_f_int) stat;
	return;
}

#ifndef _UNICOS
void
pxfalarm_(
	_f_int *iseconds,
	_f_int *isecleft,
	_f_int *ierror
)
{
	_PXFALARM(iseconds, isecleft, ierror);
}
#endif	/* not _UNICOS */
