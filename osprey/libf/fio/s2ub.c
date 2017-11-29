/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/s2ub.c	92.1	06/18/99 18:41:02"
#include <cray/fmtconv.h>
#include <cray/portdefs.h>

#define MXDGTS	64		/* Number of digits in one binary word	*/
#define	MXSIZE	(MXDGTS + 1)	/* Size of one binary word plus blank	*/
#define	DGSIZE	1		/* Size of one binary digit (in bits)	*/


#define UNDP	(MODEUN | MODEDP)

extern oc_func _S2UB;		/* Interface must match oc_func prototype */

/*
 *	_S2UB() Convert Fortran integer variable to binary format.
 *
 *	Entry:
 *		value	Address of logical variable
 *		fca	Address of first unpacked character
 *		mode	Address of mode bits
 *		width	Address of field width
 *		digits	Address of digits field
 *		exp	Unused
 *		scale	Unused
 *
 *	Exit:
 *		result	Points to end of output field
 *
 *	Note:	This routine has the same parameters as S2UI, etc. in
 *		libc.  It handles double-precision values via two calls
 *		to _s2ub if the field width is large enough.
 */

long *
_S2UB(
const void	*value,
long		*fca,
const long	*mode,
const long	*width,
const long	*digits,
const long	*exp,
const long	*scale
)
{
	long	fd, fw, nfd, *ptr;

	fd	= *digits;
	fw	= *width;
	ptr	= fca;

	/* Check if double-precision value and field is large enough. */

	if ((*mode & UNDP) == UNDP && fw > MXSIZE) {

		fw     -= MXSIZE;
		nfd	= fd - MXDGTS;

		if (nfd < 0)
			nfd	= 0;
		else
			if (nfd > fw)
				nfd	= fw;

		ptr	= _s2ub(value, ptr, mode, &fw, &nfd, exp, scale);

		value	= (char *)value + MXSIZE/8;
		fw	= MXSIZE;

		if (fd > MXDGTS)
			fd	= MXDGTS;
	}

	return( _s2ub(value, ptr, mode, &fw, &fd, exp, scale) );
}
