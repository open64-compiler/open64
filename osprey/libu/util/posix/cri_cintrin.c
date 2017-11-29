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


static char USMID[] = "@(#) libu/util/posix/cri_cintrin.c	92.2	08/02/99 10:41:11";
/*
 * These functions are C implementations of the Cray Standard C intrinsic
 * functions. The descriptions of the functions come from the Cray Standard C
 * Programmer's Reference Manual, SR-2074 3.0.
 * 
 * The Manual states that "Arguments to these functions must be of type short,
 * int, or long, or their signed or unsigned varieties."
 * 
 * We are assumming that the arguments will be int or long, unsigned of each is
 * OK, and that int and long are the same size.
 */

#include <cray/portdefs.h>

/*************************************************************/

#if     !defined(__mips) || \
        (defined(__mips) && !(defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION>= 730))) || \
	(defined(_LITTLE_ENDIAN) && !defined(__sv2))
/*
 * long _dshiftl(x, y, n)  --  The _dshiftl function returns a value generated
 * by a left double-shift of the leftmost n bits of y into the rightmost n bits
 * of x.  If both x and y arguments specify the same word in memory, a rotated
 * shift occurs.
 */

long
_dshiftl(INTRINSICS_PARM_TYPE x, INTRINSICS_PARM_TYPE y, int n)
{
	INTRINSICS_PARM_TYPE	t;

	if (n == 0) {
	/*
	 * no shifting necessary.
	 */
		return x;
	}
	if (n < 0 || n > _BITS_PER_LONG) {
	/*
	 * don't shift when amount to shift is negative.
	 */
		return 0x0L;
	}
	/*
	 * get a mask to get the leftmost n bits of y.
	 *
	 * use it to get the bits from y.
	 *
	 * shift the bits from y n bits.
	 */
	t = _maskl(n);
	t &= y;
	t >>= _BITS_PER_LONG - n;

	/*
	 * shift x n bits.
	 *
	 * or the shifted bits from y with shifted x and return.
	 */
	/*
	 * some compilers (sgi's for example) don't like to shift the same
	 * number of bits as there are bits in the word being shifted. so just
	 * do it with out shifting.
	 */
	x = (n == _BITS_PER_LONG) ? 0x0L : x << n;
	return (x | t);
}	/* _dshiftl */


/*************************************************************/

/*
 * long _dshiftr(x, y, n)  --  The _dshiftr function returns a value generated
 * by a right double-shift of the rightmost n bits of x into the rightmost n
 * bits of y.  If both x and y arguments specify the same word in memory, a
 * rotated shift occurs.
 */

long
_dshiftr(INTRINSICS_PARM_TYPE x, INTRINSICS_PARM_TYPE y, int n)
{
	INTRINSICS_PARM_TYPE	t;

	if (n == 0) {
	/*
	 * no shifting necessary.
	 */
		return y;
	}
	if (n < 0 || n > _BITS_PER_LONG) {
	/*
	 * don't shift when amount to shift is negative.
	 */
		return 0x0L;
	}
	/*
	 * get a mask to get the rightmost n bits of x.
	 *
	 * use it to get the bits from x.
	 *
	 * shift the bits from x n bits.
	 */
	t = _maskr(n);
	t &= x;
	t <<= _BITS_PER_LONG - n;
	/*
	 * shift y n bits.
	 *
	 * or the shifted bits from x with shifted y and return.
	 */
	/*
	 * some compilers (sgi's for example) don't like to shift the same
	 * number of bits as there are bits in the word being shifted. so just
	 * do it with out shifting.
	 */
	y = n == _BITS_PER_LONG ? 0x0L : y >> n;
	return (y | t);
}	/* _dshiftr */

/*************************************************************/

/*
 * long _mask(n) --  The _mask function forms a mask of '1' bits,
 * left-justified or right-justified.  If 0 <= n <= 63, a left-justified mask
 * of '1' bits, n bits long, is formed.  Otherwise, a right-justified mask of
 * '1' bits, (128 - n) bits long, is formed.  Value n must be in the range of 0
 * to 128.
 * 
 * for suns 63 will become 31 and 128 will become 64.
 */

long
_mask(int n)
{

	int	shift;

	/*
	 * don't type cast the return value to long.
	 */

	if (n <= 0 || n > _BITS_PER_LONG * 2) {
		return (0x0L);
	} else if (n <= _BITS_PER_LONG - 1) {
		/*
		 * turn on all bits, shift in '0's.
		 */
		return ((~0x0L) << (_BITS_PER_LONG - n));
	} else {
		/*
		 * turn on all bits, shift in '0's, complement to
		 * get '1's right-justified.
		 */
		shift = _BITS_PER_LONG * 2 - n;
		/*
		 * some compilers (sgi's for example) don't like to shift
		 * the same number of bits as there are bits in the word
		 * being shifted, so just do it with out shifting.
		 */
		if (shift == _BITS_PER_LONG) {
			return (~0x0L);
		}
		return (~((~0x0L) << shift));
	}
}	/* _mask */

/*************************************************************/

/*
 * long _maskl(n) -- The _maskl function returns a left-justified mask of n '1'
 * bits.  Value n must be in the range of 0 to 64 (32 on sun).
 */

long
_maskl(int n)
{
	if (n <= 0 || n > _BITS_PER_LONG)
		return (0x0L);
	return ((~0x0L) << (_BITS_PER_LONG - n));
}	/* _maskl */


/*************************************************************/

/*
 * long _maskr(n) -- The _maskr function returns a right-justified mask
 * of n '1' bits.  Value n must be in the range of 0 to 64 (32 on sun).
 */

long
_maskr(int n)
{
	if (n <= 0 || n > _BITS_PER_LONG)
		return (0x0L);
	/*
	 * some compilers (sgi's for example) don't like to shift the same
	 * number of bits as there are bits in the word being shifted, so
	 * just do it with out shifting.
	 */
	if (n == _BITS_PER_LONG)
		return (~0x0L);
	/*
	 * turn on all bits, shift in n '0's, complement to get n '1's
	 * right-justified.
	 */
	return (~(~0x0L << n));
}	/* _maskr */
#endif

/*************************************************************/

/*
 * int _count(n) -- The _count function returns the number of '1' bits (the
 * population count) in data word n.
 *
 * for suns, the count can never exceed 32.
 */

int
_count(INTRINSICS_PARM_TYPE n)
{
	int	i = 0;

	while (n) {
		if (n & 0x1L)
			i++;
	/*
	 * n should be of a type that is unsigned to avoid shifting in '1's.
	 */
		n >>= 1;
	}
	return (i);
}

/*************************************************************/

#if     !defined(__mips) || \
        (defined(__mips) && !(defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION>= 730))) || \
	(defined(_LITTLE_ENDIAN) && !defined(__sv2))
/*
 * int _leadz(n) -- The _leadz function returns the number of leading '0'
 * bits in data word n.
 *
 * for suns, the count can never exceed 32.
 */

int
_leadz(INTRINSICS_PARM_TYPE opr1)
{
	register int		count;
	INTRINSICS_PARM_TYPE	mask, opr2;


	count	= 0;
	mask	= ~0L >> 16;
	opr2	= opr1 >> 16;

	if (opr2 == 0) {
		count	= 16;
		opr2	= opr1 & mask;
	}

	mask	= mask >> 8;
	opr1	= opr2 >> 8;

	if (opr1 == 0) {
		count	= count + 8;
		opr1	= opr2 & mask;
	}

	mask	= mask >> 4;
	opr2	= opr1 >> 4;

	if (opr2 == 0) {
		count	= count + 4;
		opr2	= opr1 & mask;
	}

	mask	= mask >> 2;
	opr1	= opr2 >> 2;

	if (opr1 == 0) {
		count	= count + 2;
		opr1	= opr2 & mask;
	}

	mask	= mask >> 1;
	opr2	= opr1 >> 1;

	if (opr2 == 0) {
		count	= count + 1;
		opr2	= opr1 & mask;
	}

	if (opr2 == 0)
		count	= count + 1;

	return (count);
}
#endif
