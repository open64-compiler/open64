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


#pragma ident "@(#) libf/pxf/pxfuname.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <string.h>
#include <sys/utsname.h>
#include "pxfstruct.h"
#include "table.h"

/*
 *	PXFUNAME	PXF interface to the uname(2) system call
 *			to return name(s) of current operating system
 *
 *	Call from Fortran:
 *
 *		SUBROUTINE PXFUNAME (JUNAMBUF, IERROR)
 *		INTEGER JUNAMBUF, IERROR
 *
 *      Where:
 *
 *      JUNAMBUF is an input integer variable or array element
 *               containing the pointer to the utsname structure.
 *
 *      IERROR  is an output integer variable that will contain the
 *              status:
 *
 *                Zero    - PXFUNAME is successful, i.e., the
 *                          requested operating system information
 *                          was returned.
 *
 *                Nonzero - PXFUNAME is not successful.
 *
 *                EBADHANDLE If JUNAMBUF is invalid.
 *
 */

#ifdef _UNICOS
void
PXFUNAME(
#else
void
_PXFUNAME(
#endif
	_f_int *JUNAMBUF,	/* handle for struct utsname	*/
	_f_int *IERROR)		/* error status			*/
{
	struct	utsname *utn;
	int	errsts = 0;
	struct pxfhandle pxfhand;

        pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *JUNAMBUF);
        if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_UNAMBUF) {
          *IERROR = EBADHANDLE;
          return;
        }
	
	utn = pxfhand.pxfstructptr;
	if (uname (utn) == -1)
	  errsts = errno;
	*IERROR	= errsts;
} 


#ifndef _UNICOS
void
pxfuname_(
	  _f_int *JUNAMBUF,
	  _f_int *IERROR
)
{
  _PXFUNAME(JUNAMBUF, IERROR);
}
#endif
