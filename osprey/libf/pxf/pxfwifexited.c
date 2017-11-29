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


#pragma ident "@(#) libf/pxf/pxfwifexited.c	92.1	06/29/99 11:36:06"


/*
 *  PXFWIFEXITED  -- Interpret the ISTAT argument as defined in
 *  (section 3.2.1.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *     LOGICAL FUNCTION PXFWIFEXITED(istat)
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
 *	Default integer value from system macro WIFEXITED(*istat)
 *
 *  DESCRIPTION:
 *
 *  Logical function PXFWIFEXITED returns the value TRUE if the
 *  child process exited with exit().  IPXFWEXITSTATUS() returns part
 *  of the bits of argument x from exit(x).
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <fortran.h>

#ifdef _UNICOS
_f_log
PXFWIFEXITED(
#else	/* _UNICOS */
_f_log
pxfwifexited_(
#endif	/* _UNICOS */
	_f_int *istat
)
{
	if (WIFEXITED(*istat)) {
		return(_btol(1) );	/* true */
	} else {
		return(_btol(0) );	/* false */
	}
}
