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


#pragma ident "@(#) libf/pxf/pxfisreg.c	92.1	06/29/99 11:36:06"

#include <liberrno.h>
#include <errno.h>
#include <fortran.h>
#include <sys/stat.h>

/*
 *  PXFISREG  -- Test for regular file
 *  (section 5.6.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *     LOGICAL FUNCTION PXFISREG(M)
 *     INTEGER M
 *
 *  Function description:
 *  The logical function PXFISREG checks if a file is a
 *  regular file. The value M should be supplied by
 *  the st_mode component of the stat structure used by
 *  the PXFSTAT routine.
 *
 *  Description of arguments:
 *  M is an input integer variable containing the file mode.
 *
 *  Return values:
 *  If the file is a regular file, PXFISREG returns
 *  a logical true, otherwise a logical false is returned.
 */

#ifdef _UNICOS
_f_int
PXFISREG(
#else
_f_int
_PXFISREG(
#endif
	  _f_int *M
)
{
  return _btol(S_ISREG((int)*M));
}

#ifndef _UNICOS
_f_int
pxfisreg_(
	  _f_int *M
)
{
  return _PXFISREG(M);
}
#endif
