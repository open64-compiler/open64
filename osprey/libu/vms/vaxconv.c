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


#pragma ident "@(#) libu/vms/vaxconv.c	92.2	06/23/99 13:56:22"

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
 *	VAX single-precision (F)  =  32 bits
 *	VAX double-precision (D)  =  64 bits
 *	VAX quad-precision   (G)  = 128 bits
 *
 *	Entry points (scalar, call-by-address):
 *
 *	IS2VF$		IEEE 32-bit to VAX 32-bit
 *	IS2VD$		IEEE 32-bit to VAX 64-bit
 *	IS2VG$		IEEE 32-bit to VAX 128-bit
 *	ID2VF$		IEEE 64-bit to VAX 32-bit
 *	ID2VD$		IEEE 64-bit to VAX 64-bit
 *	ID2VG$		IEEE 64-bit to VAX 128-bit
 *	IQ2VF$		IEEE 128-bit to VAX 32-bit
 *	IQ2VD$		IEEE 128-bit to VAX 64-bit
 *	IQ2VG$		IEEE 128-bit to VAX 128-bit
 *
 *	VF2IS$		VAX 32-bit to IEEE 32-bit
 *	VF2ID$		VAX 32-bit to IEEE 64-bit
 *	VF2IQ$		VAX 32-bit to IEEE 128-bit
 *	VD2IS$		VAX 64-bit to IEEE 32-bit
 *	VD2ID$		VAX 64-bit to IEEE 64-bit
 *	VD2IQ$		VAX 64-bit to IEEE 128-bit
 *	VG2IS$		VAX 128-bit to IEEE 32-bit
 *	VG2ID$		VAX 128-bit to IEEE 64-bit
 *	VG2IQ$		VAX 128-bit to IEEE 128-bit
 */

/* Numeric conversion constants */

#ifdef	__mips
#define	ONE	1LL
#else
#define	ONE	1L
#endif

#define	SMASK	(1 << 31) 	/* Sign bit mask (32-bit) */
#define	LMASK	(ONE << 63) 	/* Sign bit mask (64-bit) */

#define	QIEEEMX	32767		/* IEEE quad maximum exponent  (O'77777)*/
#define	QIEEEBS	16383		/* IEEE quad exponent bias     (O'37777)*/
#define	QES	48		/* IEEE quad exponent shift value	*/
#define	QME	15		/* Ln(base 2) of QIEEEMX		*/
#define	QEMASK	077777		/* IEEE quad exponent mask		*/
#define	QMMASK	0xFFFFFFFFFFFF	/* IEEE quad mantissa mask		*/

#define	DIEEEMX	2047		/* IEEE double maximum exponent (O'3777)*/
#define	DIEEEBS	1023		/* IEEE double exponent bias    (O'1777)*/
#define	DES	52		/* IEEE double exponent shift value	*/
#define	DME	11		/* Ln(base 2) of DIEEEMX		*/
#define	DEMASK	DIEEEMX		/* IEEE double exponent mask		*/
#define	DMMASK	0xFFFFFFFFFFFFF	/* IEEE double mantissa mask		*/

#define	SIEEEMX	255		/* IEEE single maximum exponent  (O'377)*/
#define	SIEEEBS	127		/* IEEE single exponent bias     (O'177)*/
#define	SES	23		/* IEEE single exponent shift value	*/
#define	SME	8		/* Ln(base 2) of SIEEEMX		*/
#define	SEMASK	SIEEEMX		/* IEEE single exponent mask		*/
#define	SMMASK	0x7FFFFF	/* IEEE single mantissa mask		*/

#define	VFEMX	255		/* VAX single maximum exponent	 (O'377)*/
#define	VFEBS	128		/* VAX single exponent bias	 (O'200)*/
#define	VFES	23		/* VAX single exponent shift value	*/
#define	VFME	8		/* Ln(base 2) of VFEMX			*/
#define	VFEMSK	VFEMX		/* VAX single exponent mask		*/
#define	VFMMSK	0x7FFFFF	/* VAX single mantissa mask		*/

#define	VDEMX	255		/* VAX double maximum exponent	 (O'377)*/
#define	VDEBS	128		/* VAX double exponent bias	 (O'200)*/
#define	VDES	55		/* VAX double exponent shift value	*/
#define	VDME	8		/* Ln(base 2) of VDEMX			*/
#define	VDEMSK	VDEMX		/* VAX double exponent mask		*/
#define	VDMMSK	0x7FFFFFFFFFFFFF /* VAX double mantissa mask		*/

#define	VGEMX	2047		/* VAX quad maximum exponent	(O'3777)*/
#define	VGEBS	1024		/* VAX quad exponent bias	(O'2000)*/
#define	VGES	52		/* VAX quad exponent shift value	*/
#define	VGME	11		/* Ln(base 2) of VGEMX			*/
#define	VGEMSK	VGEMX		/* VAX quad exponent mask		*/
#define	VGMMSK	0xFFFFFFFFFFFFF /* VAX quad mantissa mask		*/

struct ieee_info_packet {
	_f_int4	denorm;		/* Flush denorms to zero flag */
	_f_int4	oflows;		/* Count of overflows */
};

struct vax_info_packet {
	_f_int4	oflows;		/* Count of overflows */
};

struct endian_info_packet {
	_f_int4	swap;		/* Perform endian swap (0 no swap; 1 swap) */
};

#ifdef	_CRAYMPP
typedef	float	float_S;
typedef	double	float_D;
typedef	double complex	float_Q;
#elif	defined(__mips)
typedef	float	float_S;
typedef	double	float_D;
typedef	long double	float_Q;
#define int64   long long
#define uint64  unsigned long long
#define int32   int
#define uint32  unsigned int
#else
typedef	int	float_S;
typedef	float	float_D;
typedef	double complex	float_Q;
#pragma _CRI taskcommon T@IEEE
#pragma _CRI taskcommon T@VAX
#endif

#ifdef	__mips
struct ieee_info_packet		t$ieee_;
struct vax_info_packet		t$vax_;
struct endian_info_packet	t$endian_;
#else
struct ieee_info_packet		T@IEEE;
struct vax_info_packet		T@VAX;
struct endian_info_packet	T@ENDIAN;
#endif

union S_box {		/* Container for 32-bit data */
	int32	sint;
	float_S	sflt;
};

union D_box {		/* Container for 64-bit data */
	int64	dint;
	float_D	dflt;
};

union Q_box {		/* Container for 128-bit data */
	int64	qint[2];
	float_Q	qflt;
};

/* Configure IEEE exception detection */

#define	INEXACT_ENABLE		0	/* Enable detection of INEXACT */
#define	INVALID_ENABLE		0	/* Enable detection of INVALID */
#define	OVERFLOW_ENABLE		1	/* Enable detection of OVERFLOW */
#define	UNDERFLOW_ENABLE	0	/* Enable detection of UNDERFLOW */

/* IEEE exception setting */

#define	set_ieee_inexact()
#define	set_ieee_invalid()
#ifdef	__mips
#define	set_ieee_overflow()	(t$ieee_.oflows++)
#else
#define	set_ieee_overflow()	(T@IEEE.oflows++)
#endif
#define	set_ieee_underflow()

/* VAX exception setting */

#ifdef	__mips
#define	set_vax_overflow()	(t$vax_.oflows++)
#else
#define	set_vax_overflow()	(T@VAX.oflows++)
#endif

/*
 * The following machine-specific function takes an unsigned int32 or int64
 * argument and determines if it is a signaling NaN.  It should not assume
 * that the argument is a NaN.
 */

#define	signaling_S_NaN(word)		0	/* uint32 */
#define	signaling_D_NaN(word)		0	/* uint64 */
#define	signaling_Q_NaN(word1, word2)	0	/* Two uint64's */

#define	SWAP32(_U32) {						\
	_U32	= ((_U32 & 0x00FF00FF) << 8) |			\
		  ((_U32 & 0xFF00FF00) >> 8); /* Swap bytes */	\
}

#define	SWAP64(_U64) {							\
	_U64	= ((_U64 & 0x00FF00FF00FF00FF) << 8) |			\
		  ((_U64 & 0xFF00FF00FF00FF00) >> 8); /* Swap bytes */	\
}

/*
 *	IS2VF$		Convert IEEE 32-bit to VAX 32-bit
 *
 *	Special notes:
 *
 *	The domains mostly overlap, though some IEEE denorms can become
 *	valid VAX F floating-point values and some valid IEEE numbers can
 *	overflow the VAX F range.
 */

float_S
#ifdef	__mips
is2vf$_(
#else
IS2VF$(
#endif
	uint32	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint32	word;	/* Temporary */
	register int32	exp;	/* Exponent */
	register uint32	man;	/* Mantissa */
	register uint32 sign;	/* Sign */
	union S_box	r;	/* Result */

	word	= *datum;

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	sign	= word & SMASK;
	man	= word & SMMASK;
	exp	= (word >> SES) & SEMASK;

	if (exp == SIEEEMX) {	/* Check for special values */

		if (man == 0) {	/* If infinity */
			exp	= VFEMX;
			man	= VFMMSK;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= SMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && man == 0)	/* True zero */
		sign	= 0;
	else {
		if (exp == 0) {	/* If denorm */
			register short	j;
			register uint64	temp;

			temp	= (uint64) man;
			j	= _leadz(temp << (32 + SME));
			man	= (man << j) & SMMASK;
			exp	= -j;
		}

		exp	= exp + (1 + VFEBS - SIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			sign	= 0;
		}
		else if (exp > VFEMX) {
			exp	= VFEMX;
			man	= VFMMSK;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp	= exp << VFES;
	r.sint	= sign | (exp + man);

	if (swap)
		SWAP32(r.sint);

	return (r.sflt);
}

/*
 *	IS2VD$		Convert IEEE 32-bit to VAX 64-bit
 *
 *	Special notes:
 *
 *	The domains mostly overlap, though some IEEE denorms can become
 *	valid VAX D floating-point values and some valid IEEE numbers can
 *	overflow the VAX D range.
 */

float_D
#ifdef	__mips
is2vd$_(
#else
IS2VD$(
#endif
	uint32	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint32	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union D_box	r;	/* Result */

	word	= *datum;

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	sign	= ((uint64) (word & SMASK)) << 32;
	man	= ((uint64) (word & SMMASK)) << (VDES - SES);
	exp	= (word >> SES) & SEMASK;

	if (exp == SIEEEMX) {	/* Check for special values */

		if (man == 0) {	/* If infinity */
			exp	= VDEMX;
			man	= VDMMSK;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= LMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && man == 0)	/* True zero */
		sign	= 0;
	else {
		if (exp == 0) {	/* If denorm */
			register short	j;
			register uint64	temp;

			temp	= (uint64) man;
			j	= _leadz(temp << (32 + SME));
			man	= (man << j) & SMMASK;
			exp	= -j;
		}

		man	= man << (VDES - SES);
		exp	= exp + (1 + VDEBS - SIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			sign	= 0;
		}
		else if (exp > VDEMX) {
			exp	= VDEMX;
			man	= VDMMSK;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp	= exp << VDES;
	r.dint	= sign | (exp + man);

	if (swap)
		SWAP64(r.dint);

	return (r.dflt);
}

/*
 *	IS2VG$		Convert IEEE 32-bit to VAX 128-bit
 *
 *	Special notes:
 *
 *	The VAX G domain is a superset of the IEEE 32-bit domain, so all
 *	valid numbers can be converted.
 */

float_Q
#ifdef	__mips
is2vg$_(
#else
IS2VG$(
#endif
	uint32	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint32	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	man2;	/* Mantissa, second word */
	register uint64 sign;	/* Sign */
	union Q_box	r;	/* Result */

	word	= *datum;

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	man2	= 0;
	sign	= ((uint64) (word & SMASK)) << 32;
	man	= ((uint64) (word & SMMASK)) << (VGES - SES);
	exp	= (word >> SES) & SEMASK;

	if (exp == SIEEEMX) {	/* Check for special values */

		if (man == 0) {	/* If infinity */
			exp	= VGEMX;
			man	= VGMMSK;
			man2	= ~0;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= LMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && man == 0)	/* True zero */
		sign	= 0;
	else {
		if (exp == 0) {	/* If denorm */
			register short	j;

			j	= _leadz(man << (32 + SME));
			man	= (man << j) & SMMASK;
			exp	= -j;
		}

		man	= man << (VDES - SES);
		exp	= exp + (1 + VGEBS - SIEEEBS);
	}

	exp		= exp << VGES;
	r.qint[0]	= sign | (exp + man);
	r.qint[1]	= man2;

	if (swap)
		SWAP64(r.qint[0]);

	return (r.qflt);
}

/*
 *	ID2VF$		Convert IEEE 64-bit to VAX 32-bit
 *
 *	Special notes:
 *
 *	The VAX F domain is quite small compared to the IEEE 64-bit domain,
 *	plus there's loss of precision.
 */

float_S
#ifdef	__mips
id2vf$_(
#else
ID2VF$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int32	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint32 sign;	/* Sign */
	union S_box	r;	/* Result */

	word	= *datum;

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	sign	= (word & LMASK) >> 32;
	man	= word & DMMASK;
	exp	= (word >> DES) & DEMASK;

	if (exp == DIEEEMX) {	/* Check for special values */

		if (man == 0) {	/* If infinity */
			exp	= VFEMX;
			man	= VFMMSK;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= SMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && man == 0)	/* True zero */
		sign	= 0;
	else {
		register short	round;

		round	= (man >> ((DES - VFES) - 1)) & 1;
		man	= (man >> (DES - VFES)) + round;
		exp	= exp + (1 + VFEBS - DIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			sign	= 0;
		}
		else if (exp > VFEMX) {
			exp	= VFEMX;
			man	= VFMMSK;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp	= exp << VFES;
	r.sint	= sign | (exp + man);

	if (swap)
		SWAP32(r.sint);

	return (r.sflt);
}

/*
 *	ID2VD$		Convert IEEE 64-bit to VAX 64-bit
 *
 *	Special notes:
 *
 *	The VAX D domain is quite small compared to the IEEE 64-bit domain,
 *	but there is no loss of precision.
 */

float_D
#ifdef	__mips
id2vd$_(
#else
ID2VD$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union D_box	r;	/* Result */

	word	= *datum;

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	sign	= word & LMASK;
	man	= (word & DMMASK) << (VDES - DES);
	exp	= (word >> DES) & DEMASK;

	if (exp == DIEEEMX) {	/* Check for special values */

		if (man == 0) {	/* If infinity */
			exp	= VDEMX;
			man	= VDMMSK;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= LMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && man == 0)	/* True zero */
		sign	= 0;
	else {
		exp	= exp + (1 + VDEBS - DIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			sign	= 0;
		}
		else if (exp > VDEMX) {
			exp	= VDEMX;
			man	= VDMMSK;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp	= exp << VDES;
	r.dint	= sign | (exp + man);

	if (swap)
		SWAP64(r.dint);

	return (r.dflt);
}

/*
 *	ID2VG$		Convert IEEE 64-bit to VAX 128-bit
 *
 *	Special notes:
 *
 *	The domains mostly overlap, though some IEEE denorms can become
 *	valid VAX G floating-point values and some valid IEEE numbers can
 *	overflow the VAX G range.
 */

float_Q
#ifdef	__mips
id2vg$_(
#else
ID2VG$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	man2;	/* Mantissa, second word */
	register uint64 sign;	/* Sign */
	union Q_box	r;	/* Result */

	word	= *datum;

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	man2	= 0;
	sign	= word & LMASK;
	man	= word & DMMASK;
	exp	= (word >> DES) & DEMASK;

	if (exp == DIEEEMX) {	/* Check for special values */

		if (man == 0) {	/* If infinity */
			exp	= VGEMX;
			man	= VGMMSK;
			man2	= ~0;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= LMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && man == 0)	/* True zero */
		sign	= 0;
	else {
		if (exp == 0) {	/* If denorm */
			register short	j;

			j	= _leadz(man << DME);
			man	= (man << j) & DMMASK;
			exp	= -j;
		}

		exp	= exp + (1 + VGEBS - DIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			sign	= 0;
		}
		else if (exp > VGEMX) {
			exp	= VGEMX;
			man	= VGMMSK;
			man2	= ~0;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp		= exp << VGES;
	r.qint[0]	= sign | (exp + man);
	r.qint[1]	= man2;

	if (swap)
		SWAP64(r.qint[0]);

	return (r.qflt);
}

/*
 *	IQ2VF$		Convert IEEE 128-bit to VAX 32-bit
 *
 *	Special notes:
 *
 *	The VAX F domain is quite small compared to the IEEE 128-bit domain,
 *	plus there's loss of precision.
 */

float_S
#ifdef	__mips
iq2vf$_(
#else
IQ2VF$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int32	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	man2;	/* Mantissa, second word */
	register uint32 sign;	/* Sign */
	union S_box	r;	/* Result */

	word	= *datum;
	man2	= *(datum + 1);

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	sign	= (word & LMASK) >> 32;
	man	= word & QMMASK;
	exp	= (word >> QES) & QEMASK;

	if (exp == QIEEEMX) {	/* Check for special values */

		if ((man | man2) == 0) {	/* If infinity */
			exp	= VFEMX;
			man	= VFMMSK;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= SMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && (man | man2) == 0)	/* True zero */
		sign	= 0;
	else {
		register short	round;

		round	= (man >> ((QES - VFES) - 1)) & 1;
		man	= (man >> (QES - VFES)) + round;
		exp	= exp + (1 + VFEBS - QIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			sign	= 0;
		}
		else if (exp > VFEMX) {
			exp	= VFEMX;
			man	= VFMMSK;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp	= exp << VFES;
	r.sint	= sign | (exp + man);

	if (swap)
		SWAP32(r.sint);

	return (r.sflt);
}

/*
 *	IQ2VD$		Convert IEEE 128-bit to VAX 64-bit
 *
 *	Special notes:
 *
 *	The VAX D domain is quite small compared to the IEEE 128-bit domain,
 *	plus there's loss of precision.
 */

float_D
#ifdef	__mips
iq2vd$_(
#else
IQ2VD$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	man2;	/* Mantissa, secord word */
	register uint64 sign;	/* Sign */
	union D_box	r;	/* Result */

	word	= *datum;
	man2	= *(datum  + 1);

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	sign	= word & LMASK;
	man	= (word & QMMASK) << (VDES - QES);
	exp	= (word >> QES) & QEMASK;

	if (exp == QIEEEMX) {	/* Check for special values */

		if ((man | man2) == 0) {	/* If infinity */
			exp	= VDEMX;
			man	= VDMMSK;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= LMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && (man | man2) == 0)	/* True zero */
		sign	= 0;
	else {
		register short	round;

		round	= (man2 >> (1 + 64 - (VDES - QES))) & 1;
		man2	= (man2 >> (64 - (VDES - QES))) + round;
		man	= man + man2;
		exp	= exp + (1 + VDEBS - QIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			sign	= 0;
		}
		else if (exp > VDEMX) {
			exp	= VDEMX;
			man	= VDMMSK;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp	= exp << VDES;
	r.dint	= sign | (exp + man);

	if (swap)
		SWAP64(r.dint);

	return (r.dflt);
}

/*
 *	IQ2VG$		Convert IEEE 128-bit to VAX 128-bit
 *
 *	Special notes:
 *
 *	The VAX D domain is quite small compared to the IEEE 128-bit domain,
 *	but there's no loss of precision.
 */

float_Q
#ifdef	__mips
iq2vg$_(
#else
IQ2VG$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	man2;	/* Mantissa, second word */
	register uint64 sign;	/* Sign */
	union Q_box	r;	/* Result */

	word	= *datum;
	man2	= *(datum  + 1);

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	sign	= word & LMASK;
	man	= word & QMMASK;
	exp	= (word >> QES) & QEMASK;

	if (exp == QIEEEMX) {	/* Check for special values */

		if ((man | man2) == 0) {	/* If infinity */
			exp	= VGEMX;
			man	= VGMMSK;
			man2	= ~0;
		}
		else {		/* Convert NaN to reserved operand */
			sign	= LMASK;
			exp	= 0;
		}
	}
	else if (exp == 0 && (man | man2) == 0)	/* True zero */
		sign	= 0;
	else {
		man	= _dshiftl(man, man2, VGES - QES);
		man2	= man2 << (VGES - QES);
		exp	= exp + (1 + VGEBS - QIEEEBS);

		if (exp <= 0) { /* If underflow */
			exp	= 0;
			man	= 0;
			man2	= 0;
			sign	= 0;
		}
		else if (exp > VGEMX) {
			exp	= VGEMX;
			man	= VGMMSK;
			man2	= ~0;
#if	OVERFLOW_ENABLED
			set_vax_overflow();
#endif
		}
	}

	exp		= exp << VGES;
	r.qint[0]	= sign | (exp + man);
	r.qint[1]	= man2;

	if (swap) {
		SWAP64(r.qint[0]);
		SWAP64(r.qint[1]);
	}

	return (r.qflt);
}

/*
 *	VF2IS$		Convert VAX 32-bit to IEEE 32-bit
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
#ifdef	__mips
vf2is$_(
#else
VF2IS$(
#endif
	uint32	*datum)
{
	register short	dflush;	/* Denorm flag */
	register short	swap;	/* Endian swap flag */
	register uint32	word;	/* Temporary */
	register int32	exp;	/* Exponent */
	register uint32	man;	/* Mantissa */
	register uint32 sign;	/* Sign */
	union S_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
	dflush	= t$ieee_.denorm;
#else
	swap	= T@ENDIAN.swap;
	dflush	= T@IEEE.denorm;
#endif

	word	= *datum;

	if (swap)
		SWAP32(word);

	sign	= word & SMASK;
	man	= word & VFMMSK;
	exp	= (word >> VFES) & VFEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= SIEEEMX;

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VFEMX && man == VFMMSK) { /* If infinity */
		exp	= SIEEEMX;
		man	= 0;
	}
	else {
		exp	= exp - (1 + VFEBS - SIEEEBS);

		if (exp <= 0) { /* If underflow */
			register short	i;

			i	= 1 - exp;
			exp	= 0;

			if (dflush) {
				man	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}
			else { /* Denormalize */
				register short	round;

				round	= (man >> (i - 1)) & 1;
				man	= ((man | (1 << SES)) >> i) + round;
			}
		}
	}

	exp	= exp << SES;
	r.sint	= sign | (exp + man);

	return (r.sflt);
}

/*
 *	VF2ID$		Convert VAX 32-bit to IEEE 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) cannot be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_D
#ifdef	__mips
vf2id$_(
#else
VF2ID$(
#endif
	uint32	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint32	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union D_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	word	= *datum;

	if (swap)
		SWAP32(word);

	sign	= ((uint64) (word & SMASK)) << 32;
	man	= ((uint64) (word & VFMMSK)) << (DES - VFES);
	exp	= (word >> VFES) & VFEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= DIEEEMX;

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VFEMX && man == VFMMSK) { /* If infinity */
		exp	= DIEEEMX;
		man	= 0;
	}
	else
		exp	= exp - (1 + VFEBS - DIEEEBS);

	exp	= exp << DES;
	r.dint	= sign | (exp + man);

	return (r.dflt);
}

/*
 *	VF2IQ$		Convert VAX 32-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) cannot be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_Q
#ifdef	__mips
vf2iq$_(
#else
VF2IQ$(
#endif
	uint32	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint32	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union Q_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	word	= *datum;

	if (swap)
		SWAP32(word);

	sign	= ((uint64) (word & SMASK)) << 32;
	man	= ((uint64) (word & VFMMSK)) << (QES - VFES);
	exp	= (word >> VFES) & VFEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= QIEEEMX;

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VFEMX && man == VFMMSK) { /* If infinity */
		exp	= QIEEEMX;
		man	= 0;
	}
	else
		exp	= exp - (1 + VFEBS - QIEEEBS);

	exp		= exp << QES;
	r.qint[0]	= sign | (exp + man);
	r.qint[1]	= 0;

	return (r.qflt);
}

/*
 *	VD2IS$		Convert VAX 64-bit to IEEE 32-bit
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
#ifdef	__mips
vd2is$_(
#else
VD2IS$(
#endif
	uint64	*datum)
{
	register short	dflush;	/* Denorm flag */
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union S_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
	dflush	= t$ieee_.denorm;
#else
	swap	= T@ENDIAN.swap;
	dflush	= T@IEEE.denorm;
#endif

	word	= *datum;

	if (swap)
		SWAP64(word);

	sign	= (word & LMASK) >> 32;
	man	= word & VDMMSK;
	exp	= (word >> VDES) & VDEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= SIEEEMX;
			man	= man >> (VDES - SES);

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VDEMX && man == VDMMSK) { /* If infinity */
		exp	= SIEEEMX;
		man	= 0;
	}
	else {
		man	= man >> (VDES - SES);
		exp	= exp - (1 + VFEBS - SIEEEBS);

		if (exp <= 0) { /* If underflow */
			register short	i;

			i	= 1 - exp;
			exp	= 0;

			if (dflush || i > (SES + 1)) {
				man	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}
			else { /* Denormalize */
				register short	round;

				round	= (man >> (i - 1)) & 1;
				man	= ((man | (1 << SES)) >> i) + round;
			}
		}
		else if (exp >= SIEEEMX) {
			exp	= SIEEEMX;
			man	= 0;
#if	OVERFLOW_ENABLED
			set_ieee_overflow();
#endif
		}
	}

	exp	= exp << SES;
	r.sint	= sign | (exp + man);

	return (r.sflt);
}

/*
 *	VD2ID$		Convert VAX 64-bit to IEEE 64-bit
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
#ifdef	__mips
vd2id$_(
#else
VD2ID$(
#endif
	uint64	*datum)
{
	register short	dflush;	/* Denorm flag */
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union D_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
	dflush	= t$ieee_.denorm;
#else
	swap	= T@ENDIAN.swap;
	dflush	= T@IEEE.denorm;
#endif

	word	= *datum;

	if (swap)
		SWAP64(word);

	sign	= word & LMASK;
	man	= word & VDMMSK;
	exp	= (word >> VDES) & VDEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= DIEEEMX;
			man	= man >> (VDES - DES);

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VDEMX && man == VDMMSK) { /* If infinity */
		exp	= DIEEEMX;
		man	= 0;
	}
	else {
		man	= man >> (VDES - DES);
		exp	= exp - (1 + VFEBS - DIEEEBS);

		if (exp <= 0) { /* If underflow */
			register short	i;

			i	= 1 - exp;
			exp	= 0;

			if (dflush) {
				man	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}
			else { /* Denormalize */
				register short	round;

				round	= (man >> (i - 1)) & 1;
				man	= ((man | (ONE << DES)) >> i) + round;
			}
		}
	}

	exp	= exp << DES;
	r.dint	= sign | (exp + man);

	return (r.dflt);
}

/*
 *	VD2IQ$		Convert VAX 64-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion (NaN/infinity inputs are converted to NaN/infinity
 *	outputs).
 *
 *	Inexact (loss of precision) cannot be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_Q
#ifdef	__mips
vd2iq$_(
#else
VD2IQ$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union Q_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	word	= *datum;

	if (swap)
		SWAP64(word);

	sign	= word & LMASK;
	man	= word & VDMMSK;
	exp	= (word >> VDES) & VDEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= QIEEEMX;

			if (man == 0)
				man	= 1 << (VDES - QES);
		}
	}
	else if (exp == VDEMX && man == VDMMSK) { /* If infinity */
		exp	= QIEEEMX;
		man	= 0;
	}
	else
		exp	= exp - (1 + VFEBS - QIEEEBS);

	exp		= exp << QES;
	r.qint[1]	= man << (64 - (VDES - QES));
	man		= man >> (VDES - QES);
	r.qint[0]	= sign | (exp + man);

	return (r.qflt);
}

/*
 *	VG2IS$		Convert VAX 128-bit to IEEE 32-bit
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
#ifdef	__mips
vg2is$_(
#else
VG2IS$(
#endif
	uint64	*datum)
{
	register short	dflush;	/* Denorm flag */
	register short	swap;	/* Endian swap flag */
	register uint64	word1;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	man2;	/* Mantissa, second word */
	register uint64 sign;	/* Sign */
	union S_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
	dflush	= t$ieee_.denorm;
#else
	swap	= T@ENDIAN.swap;
	dflush	= T@IEEE.denorm;
#endif

	word1	= datum[0];
	man2	= datum[1];

	if (swap) {
		SWAP64(word1);
		SWAP64(man2);
	}

	sign	= (word1 & LMASK) >> 32;
	man	= word1 & VGMMSK;
	exp	= (word1 >> VGES) & VGEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= SIEEEMX;
			man	= man >> (VGES - SES);

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VGEMX && man == VGMMSK && man2 == ~0) { /* If infinity */
		exp	= SIEEEMX;
		man	= 0;
	}
	else {
		man	= man >> (VGES - SES);
		exp	= exp - (1 + VFEBS - SIEEEBS);

		if (exp <= 0) { /* If underflow */
			register short	i;

			i	= 1 - exp;
			exp	= 0;

			if (dflush || i > (SES + 1)) {
				man	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}
			else { /* Denormalize */
				register short	round;

				round	= (man >> (i - 1)) & 1;
				man	= ((man | (1 << SES)) >> i) + round;
			}
		}
		else if (exp >= SIEEEMX) {
			exp	= SIEEEMX;
			man	= 0;
#if	OVERFLOW_ENABLED
			set_ieee_overflow();
#endif
		}
	}

	exp	= exp << SES;
	r.sint	= sign | (exp + man);

	return (r.sflt);
}

/*
 *	VG2ID$		Convert VAX 128-bit to IEEE 64-bit
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
#ifdef	__mips
vg2id$_(
#else
VG2ID$(
#endif
	uint64	*datum)
{
	register short	dflush;	/* Denorm flag */
	register short	swap;	/* Endian swap flag */
	register uint64	word1;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64	man2;	/* Mantissa, second word */
	register uint64 sign;	/* Sign */
	union D_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
	dflush	= t$ieee_.denorm;
#else
	swap	= T@ENDIAN.swap;
	dflush	= T@IEEE.denorm;
#endif

	word1	= datum[0];
	man2	= datum[1];

	if (swap) {
		SWAP64(word1);
		SWAP64(man2);
	}

	sign	= word1 & LMASK;
	man	= word1 & VGMMSK;
	exp	= (word1 >> VGES) & VGEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0)
			man	= 0;
		else {	/* reserved operand => NaN */
			exp	= DIEEEMX;
			/* man	= man >> (VGES - DES); */

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VGEMX && man == VGMMSK && man2 == ~0) { /* If infinity */
		exp	= DIEEEMX;
		man	= 0;
	}
	else {
		/* man	= man >> (VGES - DES); */
		exp	= exp - (1 + VFEBS - DIEEEBS);

		if (exp <= 0) { /* If underflow */
			register short	i;

			i	= 1 - exp;
			exp	= 0;

			if (dflush) {
				man	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
			}
			else { /* Denormalize */
				register short	round;

				round	= (man >> (i - 1)) & 1;
				man	= ((man | (ONE << DES)) >> i) + round;
			}
		}
		else if (exp >= DIEEEMX) {
			exp	= DIEEEMX;
			man	= 0;
#if	OVERFLOW_ENABLED
			set_ieee_overflow();
#endif
		}
	}

	exp	= exp << DES;
	r.dint	= sign | (exp + man);

	return (r.dflt);
}

/*
 *	VG2IQ$		Convert VAX 128-bit to IEEE 128-bit
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

float_Q
#ifdef	__mips
vg2iq$_(
#else
VG2IQ$(
#endif
	uint64	*datum)
{
	register short	swap;	/* Endian swap flag */
	register uint64	word1;	/* Temporary */
	register uint64	man2;	/* Temporary */
	register int64	exp;	/* Exponent */
	register uint64	man;	/* Mantissa */
	register uint64 sign;	/* Sign */
	union Q_box	r;	/* Result */

#ifdef	__mips
	swap	= t$endian_.swap;
#else
	swap	= T@ENDIAN.swap;
#endif

	word1	= datum[0];
	man2	= datum[1];

	if (swap) {
		SWAP64(word1);
		SWAP64(man2);
	}

	sign	= word1 & LMASK;
	man	= word1 & VGMMSK;
	exp	= (word1 >> VGES) & VGEMSK;

	if (exp == 0) {	/* Check for special values */

		if (sign == 0) {
			man	= 0;
			man2	= 0;
		}
		else {	/* reserved operand => NaN */
			exp	= QIEEEMX;
			man2	= _dshiftr(man, man2, VGES - QES);
			man	= man >> (VGES - QES);

			if (man == 0)
				man	= 1;
		}
	}
	else if (exp == VGEMX && man == VGMMSK && man2 == ~0) { /* If infinity */
		exp	= QIEEEMX;
		man	= 0;
		man2	= 0;
	}
	else {
		man2	= _dshiftr(man, man2, VGES - QES);
		man	= man >> (VGES - QES);
		exp	= exp - (1 + VFEBS - QIEEEBS);
	}

	exp		= exp << QES;
	r.qint[0]	= sign | (exp + man);
	r.qint[1]	= man2;

	return (r.qflt);
}
