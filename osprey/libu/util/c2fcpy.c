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


#pragma ident "@(#) libu/util/c2fcpy.c	92.1	07/07/99 13:18:33"

#include <fortran.h>
#include <memory.h>

/*
 * _c2fcpy -	Copy a C string to a Fortran character variable.  Pad with
 *		blanks.  If there is insufficient space in f for the string
 *		pointed to by c, -1 is returned.  Otherwise, 0 is returned.
 */
int
_c2fcpy(
char *c,	/* source string */
_fcd f)		/* output string */
{
	int  clen,flen;
	char *fptr;

	clen = strlen(c);
	fptr = _fcdtocp(f);
	flen = _fcdlen(f);

	if (clen > flen)
		return(-1);

	(void)memcpy(fptr,c,clen);			/* copy string */
	(void)memset(&fptr[clen],' ',flen-clen);	/* blank pad */
	return(0);
}
