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


#pragma ident "@(#) libu/util/mpp/_pack.c	92.1	07/07/99 13:23:08"

/* 
 *       _pack     C callable subroutine to pack bytes from a buffer.
 *                  Packing terminates when the byte count is reached.
 *                  The terminating character, if specified, is packed
 *                  into the buffer.
 *
 *	Returns:
 *
 *		>= 0	Number of bytes packed, (including the optional 
 *			terminating byte).
 *		  -1	Argument error detected.
 *
 *		No processing takes place and nb is set to -1 if this routine
 *		is called with bc < 0, called with less than 3 arguments, or
 *		called with an invalid terminating character.
 *
 *	Notes:
 *
 *		The tc argument is required on SPARC systems.
 */
long
_pack(
	long	*up,	/* Buffer containing unpacked data */
	char	*cp,	/* Char buffer to receive packed data (8-bit bytes) */
	long	bc,	/* Number of bytes to pack */
	int	tc)	/* (optional argument) Character to be packed at end of
			 * data (omitted or -1 if no terminating character).  
			 * Valid terminating characters are in the range
			 * 0 <= tc <= O'177. */
{

	int i;
	int narg;

#ifdef	_CRAY
	narg = _numargs();
#else
	narg = 4;		/* assume all arguments are passed */
#endif

	if (narg < 3 || bc < 0)
		return(-1);

	for (i=0; i<bc; i++) {
		cp[i] = up[i];
	}

	if (narg > 3 && tc != -1) {

		if (tc < 0 || tc > 0177)
			return(-1);

		cp[i++] = tc;
	}

	return(i);
}
