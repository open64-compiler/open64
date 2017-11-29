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


#pragma ident "@(#) libu/ieg/mpp/cray.c	92.1	06/25/99 14:59:36"

#define	CRAYMX	24576	/* CRAY maximum exponent (060000)	*/
#define	CRAYBS	16384	/* CRAY exponent bias (040000)		*/

#define	DIEEEMX	2047	/* IEEE 64-bit maximum exponent (0377)	*/
#define	DIEEEBS	1023	/* IEEE 32-bit exponent bias (0177)	*/
#define	SIEEEMX	255	/* IEEE 32-bit maximum exponent (0377)	*/
#define	SIEEEBS	127	/* IEEE 32-bit exponent bias (0177)	*/

#define	NANMAN	0x3C4E614E3E	/* Nan Mantissa */

/*
 *	_ITH64I	Translate T3D 32-bit Half-precision to CRAY (64-bit)
 */

void
_ITH64I(unsigned long datum, long *lp)
{
	unsigned long	exp, man, nbit, sign;

	nbit	= 1 << 47;
	sign	= (datum >> 31) << 63;
	man	= nbit | ((datum << 41) >> 17);
	exp	= (datum << 33) >> 56;

	if (exp == SIEEEMX) {
		exp	= CRAYMX;

		if (man != nbit)	/* If NaN */
			man	= nbit | NANMAN;	/* '<NaN>' */
	}
	else
		if (exp == 0) {
			man	= 0;
			sign	= 0;
		}
		else
			exp	= exp + (CRAYBS - SIEEEBS + 1);

	exp	= exp << 48;

	*lp	= sign | (exp + man);

	return;
}

/*
 *	_ITF64I	Translate T3D 64-bit REAL to CRAY (64-bit)
 */

void
_ITF64I(unsigned long datum, long *lp)
{
	unsigned long	exp, man, nbit, sbit, sign;

	sbit	= 1 << 63;
	nbit	= 1 << 47;
	sign	= datum & sbit;
	man	= sbit | ((datum << 12) >> 1);
	exp	= (datum << 1) >> 53;

	if (exp == DIEEEMX) {
		exp	= CRAYMX;

		if (man != sbit)	/* If NaN */
			man	= sbit | (NANMAN << 16); /* '<NaN>' << 16 */
	}
	else
		if (exp == 0) {
			man	= 0;
			nbit	= 0;
			sign	= 0;
		}
		else
			exp	= exp + (CRAYBS - DIEEEBS + 1);

	man	= (((man >> 15) + 1) >> 1) | nbit;	/* Round mantissa */
	exp	= exp << 48;

	*lp	= sign | (exp + man);

	return;
}

/*
 *	_ITF128I	Translate T3D 64-bit REAL to CRAY (128-bit)
 */

void
_ITF128I(unsigned long datum, long *lp)
{
	unsigned long	exp, lower, man, nbit, sign;

	nbit	= 1 << 63;
	sign	= datum & nbit;
	man	= nbit | ((datum << 12) >> 1);
	lower	= (datum & 037) << 43;
	exp	= (datum << 1) >> 53;

	if (exp == DIEEEMX) {
		exp	= CRAYMX;

		if (man != nbit) {	/* If NaN */
			man	= nbit | (NANMAN << 16); /* '<NaN>' << 16 */
			lower	= 0;
		}
	}
	else
		if (exp == 0) {
			man	= 0;
			lower	= 0;
			sign	= 0;
		}
		else
			exp	= exp + (CRAYBS - DIEEEBS + 1);

	man	= man >> 16;
	exp	= exp << 48;

	*lp	= sign | (exp + man);
	*(lp+1)	= lower;

	return;
}

/*
 *	_ITSC128I	Translate T3D 32-bit Half-precision Complex to CRAY (2x64-bit)
 */

void
_ITSC128I(unsigned long datum, long *lp)
{
	unsigned long	exp1, exp2, man1, man2, nbit, sign1, sign2;

	nbit	= 1 << 47;
	sign1 	= (datum >> 63) << 63;
	sign2 	= (datum >> 31) << 63;
	man1	= nbit | (((datum <<  9) >> 49) << 25);
	man2	= nbit |  ((datum << 41) >> 17);
	exp1	= (datum <<  1) >> 56;
	exp2	= (datum << 33) >> 56;

	/* Convert real part */

	if (exp1 == SIEEEMX) {
		exp1	= CRAYMX;

		if (man1 != nbit)	/* If NaN */
			man1	= nbit | NANMAN;	/* '<NaN>' */
	}
	else
		if (exp1 == 0) {
			man1	= 0;
			sign1	= 0;
		}
		else
			exp1	= exp1 + (CRAYBS - SIEEEBS + 1);

	/* Convert imaginary part */

	if (exp2 == SIEEEMX) {
		exp2	= CRAYMX;

		if (man2 != nbit)	/* If NaN */
			man2	= nbit | NANMAN;	/* '<NaN>' */
	}
	else
		if (exp2 == 0) {
			man2	= 0;
			sign2	= 0;
		}
		else
			exp2	= exp2 + (CRAYBS - SIEEEBS + 1);

	exp1	= exp1 << 48;
	exp2	= exp2 << 48;

	*lp	= sign1 | (exp1 + man1);
	*(lp+1)	= sign2 | (exp2 + man2);

	return;
}

/*
 *	_ITSC256I	Translate T3D 32-bit Half-precision Complex to CRAY
 *			DOUBLE COMPLEX (2x128-bit)
 */

void
_ITSC256I(unsigned long datum, long *lp)
{
	unsigned long	exp1, exp2, man1, man2, nbit, sign1, sign2;

	nbit	= 1 << 47;
	sign1 	= (datum >> 63) << 63;
	sign2 	= (datum >> 31) << 63;
	man1	= nbit | (((datum <<  9) >> 49) << 25);
	man2	= nbit |  ((datum << 41) >> 17);
	exp1	= (datum <<  1) >> 56;
	exp2	= (datum << 33) >> 56;

	/* Convert real part */

	if (exp1 == SIEEEMX) {
		exp1	= CRAYMX;

		if (man1 != nbit)	/* If NaN */
			man1	= nbit | NANMAN;	/* '<NaN>' */
	}
	else
		if (exp1 == 0) {
			man1	= 0;
			sign1	= 0;
		}
		else
			exp1	= exp1 + (CRAYBS - SIEEEBS + 1);

	/* Convert imaginary part */

	if (exp2 == SIEEEMX) {
		exp2	= CRAYMX;

		if (man2 != nbit)	/* If NaN */
			man2	= nbit | NANMAN;	/* '<NaN>' */
	}
	else
		if (exp2 == 0) {
			man2	= 0;
			sign2	= 0;
		}
		else
			exp2	= exp2 + (CRAYBS - SIEEEBS + 1);

	exp1	= exp1 << 48;
	exp2	= exp2 << 48;

	*lp	= sign1 | (exp1 + man1);
	*(lp+1) = 0;
	*(lp+2)	= sign2 | (exp2 + man2);
	*(lp+3) = 0;

	return;
}
