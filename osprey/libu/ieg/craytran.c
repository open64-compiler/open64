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


#pragma ident "@(#) libu/ieg/craytran.c	92.1	06/25/99 14:37:13"

#include <fortran.h>

#define	CRAYMX	24576	/* CRAY maximum exponent       (060000)	*/
#define	CRAYBS	16384	/* CRAY exponent bias          (040000) */

#define	QIEEEMX	32767	/* IEEE quad maximum exponent  (077777)	*/
#define	QIEEEBS	16383	/* IEEE quad exponent bias     (037777)	*/
#define	DIEEEMX	2047	/* IEEE double maximum exponent (03777)	*/
#define	DIEEEBS	1023	/* IEEE double exponent bias    (01777)	*/
#define	SIEEEMX	255	/* IEEE single maximum exponent  (0377)	*/
#define	SIEEEBS	127	/* IEEE single exponent bias     (0177)	*/

#define	NANMAN	0x3C4E614E3E	/* Nan Mantissa */

#ifdef	__mips
#define	int64	long long
#define	uint64	unsigned long long
#define	int32	int
#define	uint32	unsigned int
#define	ONE	1LL
#ifdef	_LD64
#define	FLOAT128	_f_real16
#else
typedef struct { double _Dreal, _Dimag; } FLOAT128;
#endif
#else
#define	int64	long
#define	uint64	unsigned long
#define	int32	short
#define	uint32	unsigned short
#define	ONE	1L
#define	FLOAT128	_f_real16
#endif

struct info_packet {
	_f_int4	denorm;		/* Flush denorms to zero flag */
	_f_int4	oflows;		/* Count of overflows */
};

#ifdef	__mips
struct info_packet T$IEEE;
#else
#if     defined(_CRAY) && !defined(_CRAYMPP)
#pragma _CRI taskcommon T@IEEE
#endif
struct info_packet T@IEEE;
#endif

/* Configure IEEE exception detection and handling */

#define INEXACT_ENABLE		0	/* Enable detection of INEXACT */
#define INVALID_ENABLE		0	/* Enable detection of INVALID */
#define OVERFLOW_ENABLE		1	/* Enable detection of OVERFLOW */
#define UNDERFLOW_ENABLE	0	/* Enable detection of UNDERFLOW */

/* IEEE exception setting */

#define	set_ieee_inexact()
#define	set_ieee_invalid()
#ifdef	_CRAY
#define	set_ieee_overflow()	(T@IEEE.oflows++)
#else
#define	set_ieee_overflow()	(T$IEEE.oflows++)
#endif
#define	set_ieee_underflow()

/*
 *	CRAY <-> IEEE floating-point conversion routines
 *
 *	IEF32I$		32-bit IEEE to 64-bit CRAY
 *	IEF64I$		64-bit IEEE to 64-bit CRAY
 *	IEF128I$	128-bit IEEE to 64-bit CRAY
 *	IED32I$		32-bit IEEE to 128-bit CRAY
 *	IED64I$		64-bit IEEE to 128-bit CRAY
 *	IED128I$	128-bit IEEE to 128-bit CRAY
 *
 *	IEF32O$		64-bit CRAY to 32-bit IEEE
 *	IEF64O$		64-bit CRAY to 64-bit IEEE
 *	IEF128O$	64-bit CRAY to 128-bit IEEE
 *	IED32O$		128-bit CRAY to 32-bit IEEE
 *	IED64I$		128-bit CRAY to 64-bit IEEE
 *	IED128O$	128-bit CRAY to 128-bit IEEE
 */

union S_box {		/* Container for 32-bit data */
	uint32		sint;
	_f_real4	sflt;
};

union D_box {		/* Container for 64-bit data */
	uint64		dint;
	_f_real8	dflt;
};

union Q_box {		/* Container for 128-bit data */
	uint64		qint[2];
	FLOAT128	qflt;
};

/*
 *	IEF32I$	Translate 32-bit IEEE floating-point to 64-bit CRAY
 *		floating-point
 */

_f_real8
ief32i$_(union S_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	nbit;	/* Normalization bit */
	register uint64	sign;	/* Sign bit */
	register uint64	temp;	/* Temporary */
	union D_box	r;	/* Result */

	temp	= (*datum).sint;
	nbit	= ONE << 47;
	sign	= (temp >> 31) << 63;
	man	= nbit | ((temp << 41) >> 17);
	exp	= (temp >> 23) & SIEEEMX;

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

	r.dint	= sign | (exp + man);

	return (r.dflt);
}

/*
 *	IEF64I$	Translate 64-bit IEEE floating-point to 64-bit CRAY
 *		floating-point
 */

_f_real8
ief64i$_(union D_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	nbit;	/* Normalization bit */
	register uint64	sbit;	/* Bit */
	register uint64	sign;	/* Sign bit */
	register uint64	temp;	/* Temporary */
	union D_box	r;	/* Result */

	temp	= (*datum).dint;
	sbit	= ONE << 63;
	nbit	= ONE << 47;
	sign	= temp & sbit;
	man	= sbit | ((temp << 12) >> 1);
	exp	= (temp >> 52) & DIEEEMX;

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

	r.dint	= sign | (exp + man);

	return (r.dflt);
}

/*
 *	IEF128I$ Translate 128-bit IEEE floating-point to 64-bit CRAY
 *		floating-point
 */

_f_real8
ief128i$_(union Q_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man1;	/* First part of mantissa */
	register uint64	man2;	/* Second part of mantissa */
	register uint64	nbit;	/* Normalization bit */
	register uint64	sbit;	/* Bit */
	register uint64	sign;	/* Sign bit */
	register uint64	temp;	/* Temporary */
	union D_box	r;	/* Result */

	temp	= (*datum).qint[0];
	sbit	= ONE << 63;
	nbit	= ONE << 47;
	sign	= temp & sbit;
	man1	= sbit | ((temp << 16) >> 1);
	man2	= (*datum).qint[1];
	exp	= (temp >> 48) & QIEEEMX;

	if (exp == QIEEEMX) {
		exp	= CRAYMX;

		if (man1 != sbit || man2 != 0)	/* If NaN */
			man1	= sbit | (NANMAN << 16); /* '<NaN>' << 16 */
	}
	else
		if (exp == 0) {
			man1	= 0;
			nbit	= 0;
			sign	= 0;
		}
		else
			exp	= exp + (CRAYBS - QIEEEBS + 1);

	man1	= (((man1 >> 15) + 1) >> 1) | nbit;	/* Round mantissa */
	exp	= exp << 48;

	r.dint	= sign | (exp + man1);

	return (r.dflt);
}

/*
 *	IED32I$	Translate 32-bit IEEE floating-point to 128-bit CRAY
 *		floating-point
 */

FLOAT128
ied32i$_(union S_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	nbit;	/* Normalization bit */
	register uint64	sign;	/* Sign bit */
	register uint64	temp;	/* Temporary */
	union Q_box	r;	/* Result */

	temp	= (*datum).sint;
	nbit	= ONE << 47;
	sign	= (temp >> 31) << 63;
	man	= nbit | ((temp << 41) >> 17);
	exp	= (temp >> 23) & SIEEEMX;

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

	r.qint[0]	= sign | (exp + man);
	r.qint[1]	= 0;

	return (r.qflt);
}

/*
 *	IED64I$	Translate 64-bit IEEE floating-point to 128-bit CRAY
 *		floating-point
 */

FLOAT128
ied64i$_(union D_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	lower;	/* Lower mantissa */
	register uint64	man;	/* (Upper) mantissa */
	register uint64	nbit;	/* Normalization bit */
	register uint64	sign;	/* Sign bit */
	register uint64	temp;	/* Temporary */
	union Q_box	r;	/* Result */

	temp	= (*datum).dint;
	nbit	= ONE << 63;
	sign	= temp & nbit;
	man	= nbit | ((temp << 12) >> 1);
	lower	= (temp & 037) << 43;
	exp	= (temp >> 52) & DIEEEMX;

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

	r.qint[0]	= sign | (exp + man);
	r.qint[1]	= lower;

	return (r.qflt);
}

/*
 *	IED128I$ Translate 128-bit IEEE floating-point to 128-bit CRAY
 *		floating-point
 */

FLOAT128
ied128i$_(union Q_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	lower;	/* Lower mantissa */
	register uint64	man;	/* (Upper) mantissa */
	register uint64	nbit;	/* Normalization bit */
	register uint64	sign;	/* Sign bit */
	register uint64	temp;	/* Temporary */
	union Q_box	r;	/* Result */

	temp	= (*datum).qint[0];
	lower	= (*datum).qint[1];
	nbit	= ONE << 63;
	sign	= temp & nbit;
	man	= nbit | ((temp << 16) >> 1);
	exp	= (temp >> 48) & QIEEEMX;

	if (exp == QIEEEMX) {
		exp	= CRAYMX;

		if (man != nbit && lower != 0) {	/* If NaN */
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
			exp	= exp + (CRAYBS - QIEEEBS + 1);

	lower	= (((man >> 15) & 01) << 63) | (lower >> 1);	/* Move bit */
	lower	= ((lower >> 15) & 01) + (lower >> 16);	/* Round lower */
	man	= (man >> 16) | (exp << 48);
	man	= man + (lower >> 48);	/* Propagate possible carry */
	man	= man | (nbit >> 16);	/* Ensure normalization bit set */
	lower	= lower & 0xffffffffffff;	/* Trim carry, if extant */

	r.qint[0]	= sign | man;
	r.qint[1]	= lower;

	return (r.qflt);
}

/*
 *	IEF32O$	Translate 64-bit CRAY floating-point to 32-bit IEEE
 *		floating-point
 */

_f_real4
ief32o$_(union D_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	sign;	/* Sign */
	register uint64	temp;	/* Temporary */
	register short	oflow;	/* Possible overflow flag */
	union S_box	r;	/* Result */

	temp	= (*datum).dint;
	sign	= (temp >> 63) << 31;
	man	= (temp << 16) >> 39;
	exp	= (temp <<  1) >> 49;
	oflow	= (exp < CRAYMX);
	exp	= exp + (SIEEEBS - CRAYBS - 2);

	if (exp >= SIEEEMX - 1) {
		man	= 0;
		exp	= SIEEEMX << 23;

#if	OVERFLOW_ENABLE
		if (oflow)
			set_ieee_overflow();
#endif
	}
	else
		if (exp < 0) {
			man	= 0;
			exp	= 0;
		}
		else {		/* Round mantissa and position exponent */
			man	= (man + 1) >> 1;
			exp	= exp << 23;
		}

	r.sint	= (uint32) (sign | (exp + man));

	return (r.sflt);
}

/*
 *	IEF64O$	Translate 64-bit CRAY floating-point to 64-bit IEEE
 *		floating-point.
 */

_f_real8
ief64o$_(union D_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	sign;	/* Sign */
	register uint64	temp;	/* Temporary */
	register short	oflow;	/* Possible overflow flag */
	union D_box	r;	/* Result */

	temp	= (*datum).dint;
	sign	= (temp >> 63) << 63;
	man	= (temp << 16) >> 11;
	exp	= (temp <<  1) >> 49;
	oflow	= (exp < CRAYMX);
	exp	= exp + (DIEEEBS - CRAYBS - 2);

	if (exp >= DIEEEMX - 1) {
		man	= 0;
		exp	= (int64)DIEEEMX << 52;

#if	OVERFLOW_ENABLE
		if (oflow)
			set_ieee_overflow();
#endif
	}
	else
		if (exp < 0) {
			man	= 0;
			exp	= 0;
		}
		else
			exp	= exp << 52;

	r.dint	= sign | (exp + man);

	return (r.dflt);
}

/*
 *	IEF128O$ Translate 64-bit CRAY floating-point to 128-bit IEEE
 *		floating-point.
 */

FLOAT128
ief128o$_(union D_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	sign;	/* Sign */
	register uint64	temp;	/* Temporary */
	union Q_box	r;	/* Result */

	temp	= (*datum).dint;
	sign	= (temp >> 63) << 63;
	man	= (temp << 17) >> 16;
	exp	= (temp <<  1) >> 49;

	if (temp != 0) {
		if (exp >= CRAYMX) {
			exp	= QIEEEMX;
			man	= 0;
		}
		else		/* Adjust exponent */
			exp	= exp + (QIEEEBS - CRAYBS - 2);
	}

	r.qint[0]	= sign | ((exp << 48) + man);
	r.qint[1]	= 0;

	return (r.qflt);
}

/*
 *	IED32O$	Translate 128-bit CRAY floating-point to 32-bit IEEE
 *		floating-point.
 */

_f_real4
ied32o$_(union Q_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	sign;	/* Sign */
	register uint64	temp;	/* Temporary */
	register short	oflow;	/* Possible overflow flag */
	union S_box	r;	/* Result */

	temp	= (*datum).qint[0];
	sign	= (temp >> 63) << 31;
	man	= (temp << 16) >> 39;
	exp	= (temp <<  1) >> 49;
	oflow	= (exp < CRAYMX);
	exp	= exp + (SIEEEBS - CRAYBS - 2);

	if (exp >= SIEEEMX - 1) {
		man	= 0;
		exp	= SIEEEMX << 23;

#if	OVERFLOW_ENABLE
		if (oflow)
			set_ieee_overflow();
#endif
	}
	else
		if (exp < 0) {
			man	= 0;
			exp	= 0;
		}
		else {		/* Round mantissa and position exponent */
			man	= (man + 1) >> 1;
			exp	= exp << 23;
		}

	r.sint	= (uint32) (sign | (exp + man));

	return (r.sflt);
}

/*
 *	IED64O$	Translate 128-bit CRAY floating-point to 64-bit IEEE
 *		floating-point
 */

_f_real8
ied64o$_(union Q_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man1;	/* First half of mantissa */
	register uint64	man2;	/* Second half of mantissa */
	register uint64	sign;	/* Sign */
	register uint64	temp;	/* Temporary */
	register short	oflow;	/* Possible overflow flag */
	union D_box	r;	/* Result */

	temp	= (*datum).qint[0];
	man2	= ((*datum).qint[1] << 16) >> 58;
	sign	= (temp >> 63) << 63;
	man1	= (temp << 16) >> 10;
	exp	= (temp <<  1) >> 49;
	oflow	= (exp < CRAYMX);
	exp	= exp + (DIEEEBS - CRAYBS - 2);

	if (exp >= DIEEEMX - 1) {
		man1	= 0;
		exp	= (int64)DIEEEMX << 52;

#if	OVERFLOW_ENABLE
		if (oflow)
			set_ieee_overflow();
#endif
	}
	else
		if (exp < 0) {
			man1	= 0;
			exp	= 0;
		}
		else {		/* Round mantissa and position exponent */
			man1	= ((man1 | man2) + 1) >> 1;
			exp	= exp << 52;
		}

	r.dint	= sign | (exp + man1);

	return (r.dflt);
}

/*
 *	IED128O$ Translate 128-bit CRAY floating-point to 128-bit IEEE
 *		floating-point
 */

FLOAT128
ied128o$_(union Q_box *datum)
{
	register int64	exp;	/* Exponent */
	register uint64	man1;	/* First half of mantissa */
	register uint64	man2;	/* Second half of mantissa */
	register uint64	sign;	/* Sign */
	register uint64	temp;	/* Temporary */
	register short	oflow;	/* Possible overflow flag */
	union Q_box	r;	/* Result */

	temp	= (*datum).qint[0];
	man2	= (*datum).qint[1];
	sign	= (temp >> 63) << 63;
	man1	= (temp << 16) >> 15;
	exp	= (temp <<  1) >> 49;
	oflow	= (exp < CRAYMX);
	exp	= exp + (QIEEEBS - CRAYBS - 2);

	if (exp >= QIEEEMX - 1) {
		man1	= 0;
		man2	= 0;
		exp	= (int64)QIEEEMX << 48;

#if	OVERFLOW_ENABLE
		if (oflow)
			set_ieee_overflow();
#endif
	}
	else
		if (exp < 0) {
			man1	= 0;
			man2	= 0;
			exp	= 0;
		}
		else {		/* Set mantissa and position exponent */
			man1	= man1 | ((man2 >> 47) & 01);
			man2	= man2 << 17;
			exp	= exp << 48;
		}

	r.qint[0]	= sign | (exp + man1);
	r.qint[1]	= man2;

	return (r.qflt);
}
