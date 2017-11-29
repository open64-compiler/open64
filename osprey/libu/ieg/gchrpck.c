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


#pragma ident "@(#) libu/ieg/gchrpck.c	92.1	06/25/99 14:37:13"

#include <fortran.h>

/*
 *	G@CHRPCK	Convert a Fortran character descriptor to a word
 *			address, a bit offset and a bit length.  This is
 *			called by the numeric data conversion routines to
 *			convert CHARACTER variables to word addresses with
 *			bit offsets.  This routine is not intended to be
 *			user-callable.
 *
 *	CHARACTER * (*) FCV
 *	INTEGER BLEN, BOFF, WADD
 *
 *	CALL G@CHRPCK(FCV, WADD, BLEN, BOFF)
 *
 *		FCV	Fortran CHARACTER variable
 *		WADD	Address to receive word address of FCV
 *		BLEN	Address to receive length, in bits, of FCV
 *		WADD	Address to receive word offset, in bits, of FCV
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
void
g$chrpck_(
	char	*cd,		/* Fortran character address */
	_f_int8	*waddr,		/* Word address */
	_f_int8	*blen,		/* Length, in bits */
	_f_int8	*woff,		/* Word offset, in bits */
	int	_len)		/* Length of character item, in bytes */
#else
void
G@CHRPCK(
	_fcd	cd,		/* Fortran character descriptor */
	_f_int8	*waddr,		/* Word address */
	_f_int8	*blen,		/* Length, in bits */
	_f_int8	*woff)		/* Word offset, in bits */
#endif
{
	register _f_int8	cp;

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	cp	= (_f_int8) (unsigned long) cd;
#else
	cp	= (_f_int8) _fcdtocp(cd);
#endif
#if	defined(_CRAYMPP) || defined(__mips) || defined(_LITTLE_ENDIAN)
	*waddr	= (cp >> 3) << 3;	/* Convert byte address to word address */
#else
	*waddr	= (cp << 6) >> 6;	/* Convert byte address to word address */
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	*blen	= _len << 3;	/* Set bit length */
	*woff	= (cd - (char *) (unsigned long) *waddr) << 3;	/* Set bit offset */
#else
	*blen	= _fcdlen(cd) << 3;	/* Set bit length */
	*woff	= (_fcdtocp(cd) - (char *) *waddr) << 3;	/* Set bit offset */
#endif

	return;
}
