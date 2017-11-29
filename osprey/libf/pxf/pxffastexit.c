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

#include <unistd.h>

#pragma ident "@(#) libf/pxf/pxffastexit.c	92.1	06/29/99 11:36:06"


/*
 *  PXFFASTEXIT -- Terminate a process
 *  (section 3.2.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFFASTEXIT(istatus)
 *     INTEGER istatus
 *
 *  Where:
 *
 *	istatus - default integer input variable containing the
 *	          status to be returned to the parent process of
 *	          the process.
 *
 *  PXFFASTEXIT never returns.
 *
 *  DESCRIPTION:
 *
 *  The subroutine procedure PXFFASTEXIT uses _exit() to
 *  terminate a process.
 *
 */

#include <fortran.h>
#include <stdlib.h>

#ifdef _UNICOS
void
PXFFASTEXIT(
#else	/* _UNICOS */
void
_PXFFASTEXIT(
#endif	/* _UNICOS */
	_f_int *istatus
)
{
	_exit(*istatus);
}

#ifndef _UNICOS
void
pxffastexit_(
	_f_int *istatus
)
{
	_PXFFASTEXIT(istatus);
}
#endif	/* not _UNICOS */
