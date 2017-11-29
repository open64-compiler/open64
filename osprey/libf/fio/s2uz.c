/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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



#pragma ident "@(#) libf/fio/s2uz.c	92.1	06/18/99 18:41:02"
#include <fortran.h>
#include <cray/fmtconv.h>
#include <cray/portdefs.h>

#define MXBITS	64		/* Number of bits in a hexadecimal value */
#define MXDGTS	16		/* Number of digits in one hex word	*/
#define	MXSIZE	(MXDGTS + 1)	/* Size of one hex word plus blank	*/
#define	DGSIZE	4		/* Size of one hex digit (in bits)	*/

#define UNDP	(MODEUN | MODEDP)

extern oc_func _S2UZ;		/* Interface must match oc_func prototype */

/*
 *	_S2UZ() Convert Fortran integer variable to hex format.
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
 *		libc.  It does CRAYs implementation of the Z edit
 *		descriptor (automatically masks the field down to the
 *		size specified by the edit descriptor and always does
 *		leading zeroes if the digits field is not set by the
 *		user (digits is 1 and MODE77)).  It also handles double-
 *		precision values via two calls to _s2uz if the field
 *		width is large enough.
 */

long *
_S2UZ(
const void	*value,
long		*fca,
const long	*mode,
const long	*width,
const long	*digits,
const long	*exp,
const long	*scale
)
{
	int64	datum;
#ifdef	_F_INT4
	int32	datum32;
/* KEY: This used to also have __mips */
#if	defined(_F_INT2)
	_f_int2	datum16;
	_f_int1	datum8;
#endif	/* _F_INT2 and mips */
#endif	/* _F_INT4 */
	long	fd, fw, m77, nfd, *ptr;

	fd	= *digits;
	fw	= *width;

#ifdef	_F_INT4
	if ((*mode & MODEHP) != 0)
		datum	= *(_f_int4 *)value;
	else
/* KEY: This used to also have __mips */
#if	defined(_F_INT2)
	if ((*mode & MODEWP) != 0)
		datum	= *(_f_int2 *)value;
	else
	if ((*mode & MODEBP) != 0)
		datum	= *(_f_int1 *)value;
	else
#endif	/* _F_INT2 */
#endif	/* _F_INT4 */
		datum	= *(_f_int8 *)value;

	ptr	= fca;
	m77	= (*mode & MODE77);	/* Fortran 77 mode */

	/* Check if double-precision value and field is large enough. */

	if ((*mode & UNDP) == UNDP && fw > MXSIZE) {

		if (fd == 1 && m77 != 0)
			fd	= fw;

		fw     -= MXSIZE;
		nfd	= fd - MXDGTS;

		if (nfd < 0)
			nfd	= 0;
		else
			if (nfd > fw)
				nfd	= fw;

		if (m77 != 0) {	/* If Fortran 77 mode */
			long	mask, temp;

			temp	= fw * DGSIZE;
			mask	= (temp < MXBITS) ? ((1 << temp) - 1) : ~0;
			datum	= datum & mask;
		}

		ptr	= _s2uz(&datum, ptr, mode, &fw, &nfd, exp, scale);

		datum	= *((int64 *)value + 1);
		fw	= MXSIZE;

		if (fd > MXDGTS)
			fd	= MXDGTS;
	}
	else
		if (fd == 1 && m77 != 0)
			fd	= (fw > MXDGTS) ? MXDGTS : fw;

	if (m77 != 0) {	/* If Fortran 77 mode */
		long	mask, temp;

		temp	= fw * DGSIZE;
		mask	= (temp < MXBITS) ? ((1 << temp) - 1) : ~0;
		datum	= datum & mask;
	}

#ifdef	_F_INT4
	if ((*mode & MODEHP) != 0) {
		datum32	= datum;
		value	= &datum32;
	}
	else
/* KEY: This used to also have __mips */
#if	defined(_F_INT2)
	if ((*mode & MODEWP) != 0) {
		datum16	= datum;
		value	= &datum16;
	}
	else
	if ((*mode & MODEBP) != 0) {
		datum8	= datum;
		value	= &datum8;
	}
	else
#endif	/* _F_INT2 */
#endif	/* _F_INT4 */
		value	= &datum;

	return( _s2uz(value, ptr, mode, &fw, &fd, exp, scale) );
}
