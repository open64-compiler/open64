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


#pragma ident "@(#) libu/ieg/ieeetran.c	92.3	11/16/99 15:10:31"

#include <fortran.h>
#include <cray/portdefs.h>

#ifndef	__mips
#include <complex.h>	/* Use complex wrapper for 128-bit data */
#endif

/*
 *	Floating-point numbers referenced herein are sized as follows:
 *
 *	IEEE single-precision (S) =  32 bits
 *	IEEE double-precision (D) =  64 bits
 *	IEEE quad-precision   (Q) = 128 bits
 *
 *	32-bit data, when passed in a 64-bit container, is right-justified.
 *
 *	Entry points (scalar, call-by-address):
 *
 *	IEEESS$		IEEE 32-bit to IEEE 32-bit (possibly a no-op)
 *	IEEESD$		IEEE 32-bit to IEEE 64-bit
 *	IEEESQ$		IEEE 32-bit to IEEE 128-bit
 *	IEEEDS$		IEEE 64-bit to IEEE 32-bit
 *	IEEEDD$		IEEE 64-bit to IEEE 64-bit (possibly a no-op)
 *	IEEEDQ$		IEEE 64-bit to IEEE 128-bit
 *	IEEEQS$		IEEE 128-bit to IEEE 32-bit
 *	IEEEQD$		IEEE 128-bit to IEEE 64-bit
 *	IEEEQQ$		IEEE 128-bit to IEEE 128-bit (possibly a no-op)
 *
 *	Complex (2x32-bit) routines:
 *
 *	IEEE2SS$	2xIEEE 32-bit to 2xIEEE 32-bit (possibly a no-op)
 *	IEEE2SD$	2xIEEE 32-bit to 2xIEEE 64-bit
 *	IEEE2DS$	2xIEEE 64-bit to 2xIEEE 32-bit
 *	IEEE2QS$	2xIEEE 128-bit to 2xIEEE 32-bit
 */

/* Numeric conversion constants */

#define	SMASK	(1 << 31) 	/* Sign bit mask (32-bit) */
#define	LMASK	(1ULL << 63) 	/* Sign bit mask (64-bit) */

#define	QIEEEMX	32767		/* IEEE quad maximum exponent  (O'77777)*/
#define	QIEEEBS	16383		/* IEEE quad exponent bias     (O'37777)*/
#define	QES	48		/* IEEE quad exponent shift value	*/
#define	QME	15		/* Ln(base 2) of QIEEEMX		*/
#define	QEMASK	077777		/* IEEE quad exponent mask		*/
#define	QMMASK	0xFFFFFFFFFFFFLL	/* IEEE quad mantissa mask		*/

#define	DIEEEMX	2047		/* IEEE double maximum exponent (O'3777)*/
#define	DIEEEBS	1023		/* IEEE double exponent bias    (O'1777)*/
#define	DES	52		/* IEEE double exponent shift value	*/
#define	DME	11		/* Ln(base 2) of DIEEEMX		*/
#define	DEMASK	DIEEEMX		/* IEEE double exponent mask		*/
#define	DMMASK	0xFFFFFFFFFFFFFLL	/* IEEE double mantissa mask		*/

#define	SIEEEMX	255		/* IEEE single maximum exponent  (O'377)*/
#define	SIEEEBS	127		/* IEEE single exponent bias     (O'177)*/
#define	SES	23		/* IEEE single exponent shift value	*/
#define	SME	8		/* Ln(base 2) of SIEEEMX		*/
#define	SEMASK	SIEEEMX		/* IEEE single exponent mask		*/
#define	SMMASK	0x7FFFFF	/* IEEE single mantissa mask		*/

struct ieee_info_packet {
	_f_int4	denorm;		/* Flush denorms to zero flag */
	_f_int4	oflows;		/* Count of overflows */
};

/*
 *	Endian swap indicator:
 *
 *	0  no swap
 *	1  DEC-style swap
 *	2  Intel-style swap
 */

struct endian_info_packet {
	_f_int4	swap;		/* Endian swap indicator */
};

#ifdef	_CRAYMPP
typedef	float	float_S;
typedef	double	float_D;
typedef	double complex	float_Q;
#elif	defined(__mips) || defined(_LITTLE_ENDIAN)
typedef	float	float_S;
typedef	double	float_D;
#ifdef HAVE_FKIND16
typedef	long double	float_Q;
#endif
#define int64   long long
#define uint64  unsigned long long
#define int32   int
#define uint32  unsigned int
#else
typedef	int	float_S;
typedef	float	float_D;
typedef	double complex	float_Q;
#pragma _CRI taskcommon T@IEEE
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
struct ieee_info_packet	t$ieee_;
struct endian_info_packet	t$endian_;
#else
struct ieee_info_packet	T@IEEE;
#endif

union S_box {		/* Container for 32-bit data */
	int32	sint;
	float_S	sflt;
};

union D_box {		/* Container for 64-bit data */
	int64	dint;
	float_D	dflt;
};

#ifdef HAVE_FKIND16
union Q_box {		/* Container for 128-bit data */
	int64	qint[2];
	float_Q	qflt;
};
#endif

extern uint32 cswap4$_();
extern uint64 cswap8$_();

/* Configure IEEE exception detection */

#define	INEXACT_ENABLE		0	/* Enable detection of INEXACT */
#define	INVALID_ENABLE		0	/* Enable detection of INVALID */
#define	OVERFLOW_ENABLE		1	/* Enable detection of OVERFLOW */
#define	UNDERFLOW_ENABLE	0	/* Enable detection of UNDERFLOW */

/* IEEE exception setting */

#define	set_ieee_inexact()
#define	set_ieee_invalid()
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define	set_ieee_overflow()	(t$ieee_.oflows++)
#else
#define	set_ieee_overflow()	(T@IEEE.oflows++)
#endif
#define	set_ieee_underflow()

/*
 * The following machine-specific function takes an unsigned int32 or int64
 * argument and determines if it is a signaling NaN.  It should not assume
 * that the argument is a NaN.
 */

#define	signaling_S_NaN(word)		0	/* uint32 */
#define	signaling_D_NaN(word)		0	/* uint64 */
#define	signaling_Q_NaN(word1, word2)	0	/* Two uint64's */

/*
 *	Inlined conversion functions
 */

#ifdef	_CRAY
#pragma _CRI inline Convert_S_to_D
#endif

static uint64
Convert_S_to_D(const uint32 input, const short dflush)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */

	sign	= ((uint64) (input & SMASK)) << 32;
	man	= input & SMMASK;
	exp	= (input >> SES) & SEMASK;

	if (exp == SIEEEMX) {	/* If NaN/Overflow input */
		exp	= DIEEEMX;
#if	INVALID_ENABLED
		if (signaling_S_NaN(input))
			set_ieee_invalid();
#endif
	}
	else
		if (exp != 0)
			exp	= exp + (DIEEEBS - SIEEEBS);
		else
			if (man != 0)	/* If denorm input */
				if (dflush) {
					man	= 0;
					exp	= 0;
				}
				else {
					register short	j;

					j	= _leadz(man << (32 + SME));
					man	= man << j;
					exp	= DIEEEBS - SIEEEBS - j;
				}

	man	= man << (DES - SES);
	exp	= exp << DES;

	return (sign | (exp + man));
}

#ifdef	_CRAY
#pragma _CRI inline Convert_S_to_Q
#endif

static uint64	/* Note: second word always zero */
Convert_S_to_Q(const uint32 input, const short dflush)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	sign;	/* Sign */

	sign	= ((uint64) (input & SMASK)) << 32;
	man	= input & SMMASK;
	exp	= (input >> SES) & SEMASK;

	if (exp == SIEEEMX) {	/* If NaN/Overflow input */
		exp	= QIEEEMX;
#if	INVALID_ENABLED
		if (signaling_S_NaN(input))
			set_ieee_invalid();
#endif
	}
	else
		if (exp != 0)
			exp	= exp + (QIEEEBS - SIEEEBS);
		else
			if (man != 0)	/* If denorm input */
				if (dflush) {
					man	= 0;
					exp	= 0;
				}
				else {
					register short	j;

					j	= _leadz(man << (32 + SME));
					man	= man << j;
					exp	= QIEEEBS - SIEEEBS - j;
				}

	man	= man << (QES - SES);
	exp	= exp << QES;

	return (sign | (exp + man));
}

#ifdef	_CRAY
#pragma _CRI inline Convert_D_to_S
#endif

static uint32
Convert_D_to_S(const uint64 input, const short dflush)
{
	register int64	exp;	/* Exponent */
	register uint64	man1;	/* Mantissa */
	register uint64	man2;	/* Mantissa */
	register uint64	round;	/* Round */
	register uint64	sign;	/* Sign */

	sign	= ((uint64) (input & LMASK)) >> 32;
	man1	= input & DMMASK;
	man2	= man1 >> (DES - SES - 1);
	round	= man2 & 1;
	man2	= (man2 >> 1) + round;
	exp	= ((input >> DES) & DEMASK) + (man2 >> SES);
	man2	= man2 & SMMASK;

	if (exp >= DIEEEMX) {	/* If NaN/Overflow input */
		exp	= SIEEEMX;
#if	INVALID_ENABLED
		if (signaling_D_NaN(input))
			set_ieee_invalid();
#endif
		if (man1 != 0 && man2 == 0)
			man2	= 1;	/* Preserve NaNs */
	}
	else
		if (exp != 0) {
			exp	= exp + (SIEEEBS - DIEEEBS);

			if (exp >= SIEEEMX) {	/* If overflow during conversion */
				exp		= SIEEEMX;
				man2		= 0;
#if	OVERFLOW_ENABLED
				set_ieee_overflow();
#endif
			}
			else
				if (exp <= 0) {	/* If underflow */
					register short	i;

					i	= 1 - exp;
					exp	= 0;

					if (dflush || i > (SES + 1)) {
						man2	= 0;
#if	UNDERFLOW_ENABLE
						set_ieee_underflow();
#endif
					}
					else {	/* Denormalize */
						round	= (man2 >> (i - 1)) & 1;
						man2	= ((man2 | (1 << SES)) >> i) + round;
					}
				}
		}
		else
			if (man1 != 0) {	/* If denorm input */
				man2	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}

#if	INEXACT_ENABLE
	if (exp != SIEEEMX && man1 != (man2 << (DES - SES)))
		set_ieee_inexact();
#endif

	exp	= exp << SES;

	return (sign | (exp + man2));
}

#ifdef	_CRAY
#pragma _CRI inline Convert_Q_to_S
#endif

static uint32
Convert_Q_to_S(const uint64 input1, const uint64 input2, const short dflush)
{
	register int64	exp;	/* Exponent */
	register uint64	man1;	/* Mantissa */
	register uint64	man2;	/* Mantissa */
	register uint64	round;	/* Round */
	register uint64	sign;	/* Sign */

	sign	= ((uint64) (input1 & LMASK)) >> 32;
	man1	= input1 & QMMASK;
	man2	= man1 >> (QES - SES - 1);
	round	= man2 & 1;
	man2	= (man2 >> 1) + round;
	exp	= ((input1 >> QES) & QEMASK) + (man2 >> SES);
	man2	= man2 & SMMASK;

	if (exp >= QIEEEMX) {	/* If NaN/Overflow input */
		exp	= SIEEEMX;
#if	INVALID_ENABLED
		if (signaling_Q_NaN(input1, input2))
			set_ieee_invalid();
#endif
		if (man2 == 0 && ((man1 | input2) != 0))
			man2	= 1;	/* Preserve NaNs */
	}
	else
		if (exp != 0) {
			exp	= exp + (SIEEEBS - QIEEEBS);

			if (exp >= SIEEEMX) {	/* If overflow during conversion */
				exp		= SIEEEMX;
				man2		= 0;
#if	OVERFLOW_ENABLED
				set_ieee_overflow();
#endif
			}
			else
				if (exp <= 0) {	/* If underflow */
					register short	i;

					i	= 1 - exp;
					exp	= 0;

					if (dflush || i > (SES + 1)) {
						man2	= 0;
#if	UNDERFLOW_ENABLE
						set_ieee_underflow();
#endif
					}
					else {	/* Denormalize */
						round	= (man2 >> (i - 1)) & 1;
						man2	= ((man2 | (1 << SES)) >> i) + round;
					}
				}
		}
		else
			if (man1 != 0 || input2 != 0) {	/* If denorm input */
				man2	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}

#if	INEXACT_ENABLE
	if (exp != SIEEEMX && man1 != (man2 << (QES - SES)))
		set_ieee_inexact();
#endif

	exp	= exp << SES;

	return ((uint32) sign | (exp + man2));

}

#ifdef	_CRAY
#pragma _CRI inline Flush_S_denorms
#endif

uint32
Flush_S_denorms(const uint32 input)
{
	register int32	exp;	/* Exponent */
	register int32	man;	/* Mantissa */
	register int32	sign;	/* Sign */

	sign	= input & SMASK;
	man	= input & SMMASK;
	exp	= (input >> SES) & SEMASK;

	if (exp == 0 && man != 0) {
#if	UNDERFLOW_ENABLE
		set_ieee_underflow();
#endif
#if	INEXACT_ENABLE
		set_ieee_inexact();
#endif
		return (sign);
	}

	return(input);
}

#ifdef	_CRAY
#pragma _CRI inline Flush_D_denorms
#endif

uint64
Flush_D_denorms(const uint64 input)
{
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	sign;	/* Sign */

	sign	= input & LMASK;
	man	= input & DMMASK;
	exp	= (input >> DES) & DEMASK;

	if (exp == 0 && man != 0) {
#if	UNDERFLOW_ENABLE
		set_ieee_underflow();
#endif
#if	INEXACT_ENABLE
		set_ieee_inexact();
#endif
		return (sign);
	}

	return(input);
}

/*
 *	Floating-point numbers referenced herein are sized as follows:
 *
 *	IEEE single-precision (S) =  32 bits
 *	IEEE double-precision (D) =  64 bits
 *	IEEE quad-precision   (Q) = 128 bits
 *
 *	32-bit data, when passed in a 64-bit container, is right-justified.
 *
 *	Entry points (scalar, call-by-address):
 *
 *	IEEESS$		IEEE 32-bit to IEEE 32-bit (possibly a no-op)
 *	IEEESD$		IEEE 32-bit to IEEE 64-bit
 *	IEEESQ$		IEEE 32-bit to IEEE 128-bit
 *	IEEEDS$		IEEE 64-bit to IEEE 32-bit
 *	IEEEDD$		IEEE 64-bit to IEEE 64-bit (possibly a no-op)
 *	IEEEDQ$		IEEE 64-bit to IEEE 128-bit
 *	IEEEQS$		IEEE 128-bit to IEEE 32-bit
 *	IEEEQD$		IEEE 128-bit to IEEE 64-bit
 *	IEEEQQ$		IEEE 128-bit to IEEE 128-bit (possibly a no-op)
 *
 *	Complex (2x32-bit) routines:
 *
 *	IEEE2SS$	2xIEEE 32-bit to 2xIEEE 32-bit (possibly a no-op)
 *	IEEE2SD$	2xIEEE 32-bit to 2xIEEE 64-bit
 *	IEEE2DS$	2xIEEE 64-bit to 2xIEEE 32-bit
 *	IEEE2QS$	2xIEEE 128-bit to 2xIEEE 32-bit
 */

/*
 *	IEEESS$		Convert IEEE 32-bit to IEEE 32-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_S
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeess$_(
#else
IEEESS$(
#endif
	union S_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register uint32	word;	/* Temporary */
	union S_box	r;	/* Result */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif

	word	= cswap4$_(datum);

	if (dflush) {	/* If flushing denorms to zero */
		word	= Flush_S_denorms(word);
	}

	r.sint	= word;

	return (r.sflt);
}

/*
 *	IEEESD$		Convert IEEE 32-bit to IEEE 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_D
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeesd$_(
#else
IEEESD$(
#endif
	union S_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register uint32	word;	/* Temporary */
	union D_box	r;	/* Result */

	word	= cswap4$_(datum);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif

	r.dint	= Convert_S_to_D(word, dflush);

	return (r.dflt);
}

#ifdef HAVE_FKIND16

/*
 *	IEEESQ$		Convert IEEE 32-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_Q
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeesq$_(
#else
IEEESQ$(
#endif
	union S_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register uint32	word;	/* Temporary */
	union Q_box	r;	/* Result */

	word	= cswap4$_(datum);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif

	r.qint[0]	= Convert_S_to_Q(word, dflush);
	r.qint[1]	= 0;

	return (r.qflt);
}
#endif /* HAVE_FKIND16 */

/*
 *	IEEEDS$		Convert IEEE 64-bit to IEEE 32-bit
 *
 *	Exceptions:
 *
 *	Overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_S
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeeds$_(
#else
IEEEDS$(
#endif
	union D_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register uint64	word;	/* Temporary */
	union S_box	r;	/* Result */

	word	= cswap8$_(datum);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif

	r.sint	= Convert_D_to_S(word, dflush);

	return (r.sflt);
}

/*
 *	IEEEDD$		Convert IEEE 64-bit to IEEE 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_D
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeedd$_(
#else
IEEEDD$(
#endif
	union D_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register uint64	word;	/* Temporary */
	union D_box	r;	/* Result */

#if	defined(__mips)|| defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif
	word	= cswap8$_(datum);

	if (dflush)	/* If flushing denorms to zero */
		word	= Flush_D_denorms(word);

	r.dint	= word;

	return (r.dflt);
}

#ifdef HAVE_FKIND16
/*
 *	IEEEDQ$		Convert IEEE 64-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_Q
#if	defined(__mips)|| defined(_LITTLE_ENDIAN)
ieeedq$_(
#else
IEEEDQ$(
#endif
	union D_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register int64	exp;	/* Exponent */
	register uint64	man1;	/* Mantissa */
	register uint64	man2;	/* Mantissa */
	register uint64	sign;	/* Sign */
	register uint64	word;	/* Temporary */
	union Q_box	r;	/* Result */

	word	= cswap8$_(datum);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif

	man2	= 0;
	sign	= word & LMASK;
	man1	= word & DMMASK;
	exp	= (word >> DES) & DEMASK;

	if (exp == DIEEEMX) {	/* If NaN/Overflow input */
		exp	= QIEEEMX;
#if	INVALID_ENABLED
		if (signaling_D_NaN(word))
			set_ieee_invalid();
#endif
	}
	else
		if (exp != 0)
			exp	= exp + (QIEEEBS - DIEEEBS);
		else
			if (man1 != 0)	/* If denorm input */
				if (dflush) {
					man1	= 0;
					exp	= 0;
				}
				else {
					register short	j;

					j	= _leadz(man1 << DME);
					man1	= man1 << j;
					exp	= QIEEEBS - DIEEEBS - j;
				}

	man2		= _dshiftr(man1, man2, DES - QES);
	man1		= man1 >> (DES - QES);
	exp		= exp << QES;
	r.qint[0]	= sign | (exp + man1);
	r.qint[1]	= man2;

	return (r.qflt);
}

/*
 *	IEEEQS$		Convert IEEE 128-bit to IEEE 32-bit
 *
 *	Exceptions:
 *
 *	Overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_S
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeeqs$_(
#else
IEEEQS$(
#endif
	union Q_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register uint64	word1;	/* Temporaries */
	register uint64	word2;	/* Temporaries */
	union S_box	r;	/* Result */

	word1	= (uint64) ((*datum).qint[0]); /* need to fix */
	word2	= (uint64) ((*datum).qint[1]);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif

	r.sint	= Convert_Q_to_S(word1, word2, dflush);

	return (r.sflt);
}

/*
 *	IEEEQD$		Convert IEEE 128-bit to IEEE 64-bit
 *
 *	Exceptions:
 *
 *	Overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_D
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeeqd$_(
#else
IEEEQD$(
#endif
	union Q_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register int64	exp;	/* Exponent */
	register uint64	man1;	/* Mantissa */
	register uint64	man2;	/* Mantissa */
	register uint64	round;	/* Round */
	register uint64	sign;	/* Sign */
	register uint64	word1;	/* Temporary */
	register uint64	word2;	/* Temporary */
	union D_box	r;	/* Result */

	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif

	sign	= word1 & LMASK;
	man1	= word1 & QMMASK;
	man2	= _dshiftl(man1, word2, (1 + DES - QES));
	round	= man2 & 1;
	man2	= (man2 >> 1) + round;
	exp	= ((word1 >> QES) & QEMASK) + (man2 >> DES);
	man2	= man2 & DMMASK;

	if (exp >= QIEEEMX) {	/* If NaN/Overflow input */
		exp	= DIEEEMX;
#if	INVALID_ENABLED
		if (signaling_Q_NaN(word1, word2))
			set_ieee_invalid();
#endif
		if (man2 == 0 && ((man1 | word2) != 0))
			man2	= 1;	/* Preserve NaNs */
	}
	else
		if (exp != 0) {
			exp	= exp + (DIEEEBS - QIEEEBS);

			if (exp >= DIEEEMX) {	/* If overflow during conversion */
				exp		= DIEEEMX;
				man2		= 0;
#if	OVERFLOW_ENABLED
				set_ieee_overflow();
#endif
			}
			else
				if (exp <= 0) {	/* If underflow */
					register short	i;

					i	= 1 - exp;
					exp	= 0;

					if (dflush || i > (DES + 1)) {
						man2	= 0;
#if	UNDERFLOW_ENABLE
						set_ieee_underflow();
#endif
					}
					else {	/* Denormalize */
						round	= (man2 >> (i - 1)) & 1;
#ifdef	__mips
						man2	= ((man2 | (1LL << DES)) >> i) + round;
#else
						man2	= ((man2 | (1 << DES)) >> i) + round;
#endif
					}
				}
		}
		else
			if (man1 != 0 || word2 != 0) {	/* If denorm input */
				man2	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}

#if	INEXACT_ENABLE
	if (exp != DIEEEMX && man1 != (man2 << (DES - QES)))
		set_ieee_inexact();
#endif

	exp	= exp << DES;
	r.dint	= sign | (exp + man2);

	return (r.dflt);
}

/*
 *	IEEEQQ$		Convert IEEE 128-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_Q
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ieeeqq$_(
#else
IEEEQQ$(
#endif
	union Q_box	*datum)
{
	register short	dflush;	/* Denorm flag */
	register uint64	word1;	/* Temporary */
	register uint64	word2;	/* Temporary */
	union Q_box	r;	/* Result */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	dflush	= t$ieee_.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif
	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);

	if (dflush) {	/* If flushing denorms to zero */
		register int64	exp;	/* Exponent */
		register uint64	man;	/* Mantissa */
		register uint64	sign;	/* Sign */

		sign	= word1 & LMASK;
		man	= word1 & QMMASK;
		exp	= (word1 >> QES) & QEMASK;

		if (exp == 0 && ((man | word2) != 0)) {
			word1	= sign;
			word2	= 0;
#if	UNDERFLOW_ENABLE
			set_ieee_underflow();
#endif
#if	INEXACT_ENABLE
			set_ieee_inexact();
#endif
		}
	}

	r.qint[0]	= word1;
	r.qint[1]	= word2;

	return (r.qflt);
}
#endif /* HAVE_FKIND16 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/*
 *	CSWAPS$		Conditionally do an endian swap on a 32-bit
 *			floating-point number
 *	CSWAPD$		Conditionally do an endian swap on a 64-bit
 *			floating-point number
 *	CSWAPQ$		Conditionally do an endian swap on a 128-bit
 *			floating-point number
 *
 */

float_S
cswaps$_(union S_box *datum)
{
	register short	swap;	/* Endian swap flag */
	register uint32	word;	/* Data word */
	union S_box	r;	/* Result */

	swap	= t$endian_.swap;
	word	= (uint32) ((*datum).sint);

	if (swap != 0) {

		word	= (word << 16) | (word >> 16);	/* Swap words */

		if (swap == 1)	/* If DEC-style, also swap bytes */
			word	= ((word & 0x00FF00FF) << 8) |
				  ((word & 0xFF00FF00) >> 8);	/* Swap bytes */

	}

	r.sint	= word;

	return (r.sflt);
}

float_D
cswapd$_(union D_box *datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Data word */
	union D_box	r;	/* Result */

	swap	= t$endian_.swap;
	word	= (uint64) ((*datum).dint);

	if (swap != 0) {

		word	= (word << 32) | (word >> 32);	/* Swap longwords */

		word	= ((word & 0x0000FFFF0000FFFFLL) << 16) |
			  ((word & 0xFFFF0000FFFF0000LL) >> 16);	/* Swap words */

		if (swap == 1)	/* If DEC-style, also swap bytes */
			word	= ((word & 0x00FF00FF00FF00FFLL) << 8) |
				  ((word & 0xFF00FF00FF00FF00LL) >> 8);
	}

	r.dint	= word;

	return (r.dflt);
}

#ifdef HAVE_FKIND16
float_Q
cswapq$_(union Q_box *datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word1;	/* Data word */
	register uint64	word2;	/* Data word */
	union Q_box	r;	/* Result */

	swap	= t$endian_.swap;
	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);

	if (swap != 0) {
		register uint64	temp;

		temp	= word2;	/* Swap quadwords */

		word2	= (word1 << 32) | (word1 >> 32);
		word1	= (temp  << 32) | (temp  >> 32); /* Swap longwords */


		word2	= ((word2 & 0x0000FFFF0000FFFF) << 16) |
			  ((word2 & 0xFFFF0000FFFF0000) >> 16);
		word1	= ((word1 & 0x0000FFFF0000FFFF) << 16) |
			  ((word1 & 0xFFFF0000FFFF0000) >> 16);	/* Swap words */

		if (swap == 1) {	/* If DEC-style, also swap bytes */
			word2	= ((word2 & 0x00FF00FF00FF00FF) << 8) |
				  ((word2 & 0xFF00FF00FF00FF00) >> 8);
			word1	= ((word1 & 0x00FF00FF00FF00FF) << 8) |
				  ((word1 & 0xFF00FF00FF00FF00) >> 8);
		}
	}

	r.qint[0]	= word1;
	r.qint[1]	= word2;

	return (r.qflt);
}
#endif /* HAVE_FKIND16 */

#endif
