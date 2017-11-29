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


#pragma ident "@(#) libf/pxf/ipxfwtermsig.c	92.1	06/29/99 11:36:06"


/*
 *  PXFWTERMSIG  -- Interpret the ISTAT argument as defined in
 *  (section 3.2.1.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     INTEGER FUNCTION IPXFWTERMSIG(istat)
 *     INTEGER istat
 *
 *  Where:
 *
 *	istat -   default integer input variable containing
 *	          the istat argument returned by PXFWAIT or
 *	          PXFWAITPID.
 *
 *  FUNCTION RESULT VALUE:
 *
 *	Default integer value from systemmacro WTERMSIG(*istat)
 *
 *  DESCRIPTION:
 *
 *  The integer function IPXFWTERMSIG returns part of the signal
 *  number that caused the child process to terminate.  Logical
 *  function PXFWIFSIGNALED returns TRUE when the child process has
 *  terminated because of a signal.  IPXFWTERMSIG should be used
 *  only when PXFWIFSIGNALED returns TRUE.
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <fortran.h>

#ifdef _UNICOS
_f_int
IPXFWTERMSIG(
#else	/* _UNICOS */
_f_int
ipxfwtermsig_(
#endif	/* _UNICOS */
	_f_int *istat
)
{
	return((_f_int) WTERMSIG(*istat) );
}
