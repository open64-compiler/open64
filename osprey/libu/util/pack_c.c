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


#pragma ident "@(#) libu/util/pack_c.c	92.1	07/07/99 13:18:33"

#include <fortran.h>
#include <liberrno.h>

/*
 *	PACK	Pack partial words into full 64-bit words
 *
 *	PACK takes 1-, 2-, 4-, 8-, 16-, or 32-bit right-
 *	justified quantities and concatenates them into a
 *	smaller number of full 64-bit words.
 *
 *	CALL PACK ( P, NBITS, U, COUNT )
 *
 *	On entry:
 *
 *	U	Vector of partial words to be packed
 *
 *	NBITS	Size of quantities in each unpacked word.
 *		NBITS must be a power of two (1, 2, 4, 8,
 *		16, or 32).
 *
 *	COUNT	Number of partial word quantities to pack
 *
 *	On exit:
 *
 *	P	Vector of packed data (64-bit words)
 *
 *
 *	If (COUNT * NBITS) is not a multiple of 64, the final
 *	word in P will be left-justified, zero filled.
 */

void
PACK(
	_f_int	*p,
	_f_int	*nbits,
	_f_int	*u,
	_f_int	*count
)
{
	register int	nb;
	register int	ni;

#ifdef	_UNICOS
	if (_numargs() < 4)
		_lerror(_LELVL_ABORT, FEPCKARG);
#endif

	nb	= *nbits;
	ni	= *count;

	if (nb < 0)
		_lerror(_LELVL_ABORT, FEPCKNEG);

	if (nb == 0)
		_lerror(_LELVL_ABORT, FEPCKPW2);

	if (ni > 0) {
		register short	cpw;	/* Chunks per word */
		register short	remr;	/* Remainder */
		register int	i;
		register int	items;	/* Number of full-word items */
		register long	mask;	/* Mask for each item */
		register long	word;	/* Scratch word */

		cpw	= 64 / nb;		/* Chunks per word */
		remr	= (ni * nb) & 077;
		items	= ((ni * nb) + 63) / 64;/* Round up */
		mask	= (1 << nb) - 1;

		switch (nb) {

			case 32:
#pragma _CRI ivdep
				for (i = 0; i < items; i++) {
					word	= (*u++ & mask) << nb;
					word	= word | (*u++ & mask);
					*p++	= word;
				}
				break;

			case 16:
#pragma _CRI ivdep
				for (i = 0; i < items; i++) {
					word	= (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					*p++	= word;
				}
				break;

			case 8:
#pragma _CRI ivdep
				for (i = 0; i < items; i++) {
					word	= (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					word	= (word << nb) | (*u++ & mask);
					*p++	= word;
				}
				break;

			case 4:
			case 2:
			case 1:
				for (i = 0; i < items; i++) {
					register short	j;

					word	= 0;

#pragma _CRI shortloop
					for (j = 0; j < cpw; j++)
						word	= (word << nb) | (*u++ & mask);

					*p++	= word;
				}
				break;

			default:
				_lerror(_LELVL_ABORT, FEPCKPW2);
				break;

		} /* switch */

		if (remr != 0) {
			p	= p - 1;
			*p	= *p & (-1 << (64 - remr));
		}
	}

	return;
}
