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


#pragma ident "@(#) libu/util/mpp/movbit.c	92.3	10/29/99 21:40:31"

#include <fortran.h>
#include <clibdefs.h>
#include <cray/portdefs.h>

#ifndef _CRAYMPP
#define MOVBITZ movbitz_
#define MOVBIT movbit_
#define STRMOV strmov_
#endif

#ifdef	_CRAYMPP
#define	ZERO	0L
#else
#define	ZERO	0LL
#endif

/*
 *	MOVBIT moves bits from "a" to "b".  MOVBITZ is the same as
 *	MOVBIT, except the bit offsets are zero based.  STRMOV moves
 *	bytes.
 */

#define MASKOFFBYTE 0xfffffffffffff8L
#define BITMASK 0x3fL

/*
 *	The _mask64 function forms a 64-bit mask of '1' bits, left- or
 *	right-justified.  If 0 <= n <= 63, a left-justified mask of '1'
 *	bits, n bits long, is formed.  Otherwise, a right-justified mask
 *	of '1' bits, (128 - n) bits long, is formed.  Value n must be in
 *	the range of 0 to 128.
 */

uint64
_mask64(int n)
{
	if (n <= 0 || n > 128) {
		return (ZERO);
	} else if (n <= 63) {
	/*
	 * turn on all bits, shift in '0's.
	 */
		return ((~ZERO) << (64 - n));
	} else {
		register int	shift;
		/*
		 * turn on all bits, shift in '0's, complement to
		 * get '1's right-justified.
		 */
		shift	= 128 - n;
		/*
		 * some compilers (sgi's for example) don't like to shift
		 * the same number of bits as there are bits in the word
		 * being shifted, so just do it with out shifting.
		 */
		if (shift == 64)
			return (~ZERO);

		return (~((~ZERO) << shift));
        }
} /* _mask64 */

#if	defined(_LITTLE_ENDIAN)
void
MOVBITZ(void *a, int64 *az, int64 *nb, void *b, int64 *bz)
#else
void
MOVBITZ(void *a, _f_int8 *az, _f_int8 *nb, void *b, _f_int8 *bz)
#endif
{
	register short	forward;/* Direction of move */
	register int	af_bit;	/* Bit offset into first word of source */
	register int	bf_bit;	/* Bit offset into first word of destination */
	register int	bl_bit;	/* Bit offset of last bit of destination */
	register int	delta;	/* Different in bit offsets */
	register long	af_byte;/* Byte address of first word of source */
	register long	al_byte;/* Byte address of last word of source */
	register long	bf_byte;/* Byte address of first word of destination */
	register long	bl_byte;/* Byte address of last word of destination */
	register int64	a0;	/* Zero-based source bit offset */
	register int64	b0;	/* Zero-based destination bit offset */
	register int64	n;	/* number of bits to move */
	register uint64	azero;	/* Bit address of start of source */
	register uint64	alast;	/* Bit address of end of source */
	register uint64	blast;	/* Bit address of end of destination */
	register uint64	bzero;	/* Bit address of start of destination*/
	register uint64	first;	/* contents of first word of destination */
	register uint64	last;	/* contents of last word of destination */
	uint64		*afwa;	/* Address of first word of source */
	uint64		*alwa;	/* Address of last word of source */
	uint64		*bfwa;	/* Address of first word of destination */
	uint64		*blwa;	/* Address of last word of destination */
	uint64		*bptr;	/* Temporary pointer for backwards moves */
	uint64		*tptr;	/* Temporary pointer for all moves */

	union {
		void *p;
		uint64 u;
	} ua, ub;

	ua.u = 0;
	ua.p = a;
	ub.u = 0;
	ub.p = b;
	
	a0	= *az;
	b0	= *bz;
	n	= *nb;

	if (n <= 0)
		return;

#if defined(_LITTLE_ENDIAN)
        /* setup the context in a nice straight forward way using byte pointers */
	af_byte = (ua.u + (a0 >> 3)); 
	bf_byte = (ub.u + (b0 >> 3));
	af_bit = a0 & BITMASK;
	bf_bit = b0 & BITMASK;
	al_byte = (ua.u + ((a0 +n -1)>> 3) );
	bl_byte = (ub.u + ((b0 +n -1)>> 3) );
#ifdef KEY /* Bug 11713 */
	/* Initialize otherwise uninitialized variable to prevent adjustment
	 * of final byte, which we never want when called from ia2mips/mips2ia
	 * anyway. If this function is ever called with fractions of a byte,
	 * a more elaborate fix will be needed. */
	blast = 0;
#endif /* KEY Bug 11713 */
	afwa = (uint64 *) af_byte;
	bfwa = (uint64 *) bf_byte;
#else
	/*
	 * Shift "a" and "b" so they can hold the bit offset in the lower 3
	 * bits.
	 */

	azero	= (((uint64) a) << 3) + a0;
	bzero	= (((uint64) b) << 3) + b0;

	/* mask off the byte offset */

	af_byte	= (azero >> 3) & MASKOFFBYTE;
	bf_byte	= (bzero >> 3) & MASKOFFBYTE;

	af_bit	= azero & BITMASK;
	bf_bit	= bzero & BITMASK;
	alast	= azero + n - 1;
	blast	= bzero + n - 1;
	
	al_byte	= (alast >> 3) & MASKOFFBYTE;
	bl_byte	= (blast >> 3) & MASKOFFBYTE;
	afwa	= (uint64 *) af_byte;
	bfwa	= (uint64 *) bf_byte;
#endif

	/* Is the source and destination entirely within one word? */

	if ((al_byte == af_byte) && (bl_byte == bf_byte)) {
		register uint64	temp;

		temp	= (*afwa << af_bit) >> bf_bit;
		*bfwa	= temp & ((_mask64((int) n)) >> bf_bit) |
			  (*bfwa & ~(_mask64((int) n) >> bf_bit));
		return;
	}

	bl_bit	= blast & BITMASK;
	first	= *(uint64 *) bf_byte;
	last	= *(uint64 *) bl_byte;
	blwa	= (uint64 *) bl_byte;
	alwa	= (uint64 *) al_byte;

	/*
	 * The difference in bit positions determines how we execute the
	 * move, so calculate that and store it in delta.
	 */

	delta	= af_bit - bf_bit;

	/*
	 * Determine which way to run the loop.  Under most conditions
	 * the loop will run forward.  The only time it must be run
	 * backwards is if bfwa > afwa and bfwa <= alwa.
	 */	

	forward	= (bf_byte > af_byte && bf_byte <= al_byte) ? 0 : 1;

	if (delta == 0) {	/* words line up bit for bit */
		if (forward) {
			for (tptr = bfwa; tptr <= blwa; tptr++) {
				*tptr	= *afwa++;
			}
		}
		else {
			for (tptr = blwa; tptr >= bfwa; tptr--) {
				*tptr	= *alwa--;
			}
		}
		
	}
	else if (delta > 0) { /* The destination is to the left of the source */
		if (forward) {
			for (tptr = bfwa; tptr <= blwa; tptr++) {
				*tptr	= ((*afwa << delta) |
					(*(afwa + 1) >> (64 - delta)));
				afwa++;
				
			}
		}
		else {
			bptr	= afwa + (blwa - bfwa);
			for (tptr = blwa; tptr >= bfwa; tptr--) {
                                *tptr	= ((*(bptr) << delta) |
                                        (*(bptr+1) >> (64 - delta)));
				bptr--;
			}
		}
	}
	else { /* The destination is to the right of the source */
		delta	= -delta;
		if (forward) {
			for (tptr = bfwa; tptr <= blwa; tptr++) {
				*tptr	= ((*(afwa-1) << (64 - delta)) |
					  (*afwa >> delta));
				afwa++;
			}
		}
		else {
			bptr	= afwa + (blwa - bfwa);
			for (tptr = blwa; tptr >= bfwa; tptr--) {
				*tptr	= ((*(bptr-1) << (64 - delta)) |
					  (*bptr >> delta));
				bptr--;

			}
		}
	}

	/* Now repair the first and last words */

	*blwa	= (*blwa & _mask64(bl_bit+1) ) | (last & ~_mask64(bl_bit+1));
#ifdef	_CRAYMPP	/* Workaround SCC 6.x code gen bug - SPR 709655 */
	*(volatile uint64 *) bfwa	= ((first & _mask64(bf_bit)) | (*bfwa & ~_mask64(bf_bit)));
#else
	*bfwa	= ((first & _mask64(bf_bit)) | (*bfwa & ~_mask64(bf_bit)));
#endif

	return;
}

#if	!defined(_LITTLE_ENDIAN)
/*
 *	This entry is the same as MOVBITZ, only the bit offsets are
 *	one-based.
 */
void
MOVBIT(void *a, _f_int8 *a1, _f_int8 *n, void *b, _f_int8 *b1)
{
	_f_int8	az_temp;
	_f_int8	bz_temp;

	az_temp	= *a1 - 1;
	bz_temp	= *b1 - 1;
	MOVBITZ(a, &az_temp, n, b, &bz_temp);
	return;
}

void
STRMOV(void *a, _f_int8 *az, _f_int8 *nc, void *b, _f_int8 *bz)
{
	_f_int8	az_temp;
	_f_int8	bz_temp;
	_f_int8	n_temp;

	az_temp	= (*az - 1) << 3;
	bz_temp	= (*bz - 1) << 3;
	n_temp	= *nc << 3;
	MOVBITZ(a, &az_temp, &n_temp, b, &bz_temp);
	return;
}

#else
void
MOVBIT(void *a, int64 *a1, int64 *n, void *b, int64 *b1)
{
	int64 az_temp;
	int64 bz_temp;

	az_temp	= *a1 - 1;
	bz_temp	= *b1 - 1;
	MOVBITZ(a, &az_temp, n, b, &bz_temp);
	return;
}

void
STRMOV(void *a, int64 *az, int64 *nc, void *b, int64 *bz)
{
	int64	az_temp;
	int64	bz_temp;
	int64	n_temp;

	az_temp	= (*az - 1) << 3;
	bz_temp	= (*bz - 1) << 3;
	n_temp	= *nc << 3;
	MOVBITZ(a, &az_temp, &n_temp, b, &bz_temp);
	return;
}
#endif
