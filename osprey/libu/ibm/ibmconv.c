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


#pragma ident "@(#) libu/ibm/ibmconv.c	92.1	09/29/99 20:40:37"

#include <clibdefs.h>
#include <cray/portdefs.h>

extern uint64	_EBCDIC_TO_ASCII[32];
extern uint64	_ASCII_TO_EBCDIC[16];

/* Numeric conversion constants */

#define	SIZE_OF_INT64	64	/* sizeof(int64) in bits		*/
#define	SIZE_OF_INT32	32	/* sizeof(int32) in bits		*/
#define SIGN_SHIFT	(SIZE_OF_INT64 - SIZE_OF_INT32)

#define	LONG_SIGN_MASK	(((uint64) 1) << 63) 	/* 64-bit sign bit mask	*/
#define	SHORT_SIGN_MASK	(((uint32) 1) << 31) 	/* 32-bit sign bit mask	*/

#define	QIEEEMX	32767		/* IEEE quad maximum exponent  (O'77777)*/
#define	QIEEEBS	16383		/* IEEE quad exponent bias     (O'37777)*/
#define	QES	48		/* IEEE quad exponent shift value	*/
#define	QME	15		/* Ln(base 2) of QIEEEMX		*/
#define	QEMASK	QIEEEMX		/* IEEE quad exponent mask		*/
#define	QMMASK	0xFFFFFFFFFFFF	/* IEEE quad mantissa mask (upper part)	*/

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

#define	IBMMX	127		/* IBM maximum exponent          (O'177)*/
#define	IBMBS	64		/* IBM exponent bias             (O'100)*/
#define	IBMSES	24		/* IBM single exponent shift value	*/
#define	IBMDES	56		/* IBM double exponent shift value	*/
#define	IBMQES	IBMDES		/* IBM quad exponent shift value	*/
#define	IME	7		/* Ln(base 2) of IBMMX			*/
#define	IEMASK	IBMMX		/* IBM single exponent mask		*/
#define	ISMASK	0xFFFFFF	/* IBM single mantissa mask		*/
#define	IDMASK	0xFFFFFFFFFFFFFF	/* IBM double mantissa mask	*/
#define	IQMASK	IDMASK		/* IBM quad mantissa mask		*/

/*
 *	Floating-point numbers referenced herein are sized as follows:
 *
 *	Single-precision (S) =  32 bits
 *	Double-precision (D) =  64 bits
 *	Quad-precision   (Q) = 128 bits
 *
 *	32-bit data, when passed in a 64-bit container, is right-justified.
 *
 *	Entry points (scalar, call-by-address):
 *
 *	IBMSS$		IBM 32-bit to IEEE 32-bit
 *	IBMSD$		IBM 32-bit to IEEE 64-bit
 *	IBMSQ$		IBM 32-bit to IEEE 128-bit
 *	IBMDS$		IBM 64-bit to IEEE 32-bit
 *	IBMDD$		IBM 64-bit to IEEE 64-bit
 *	IBMDQ$		IBM 64-bit to IEEE 128-bit
 *	IBMQS$		IBM 128-bit to IEEE 32-bit
 *	IBMQD$		IBM 128-bit to IEEE 64-bit
 *	IBMQQ$		IBM 128-bit to IEEE 128-bit
 *
 *	SSIBM$		IEEE 32-bit to IBM 32-bit
 *	SDIBM$		IEEE 32-bit to IBM 64-bit
 *	SQIBM$		IEEE 32-bit to IBM 128-bit
 *	DSIBM$		IEEE 64-bit to IBM 32-bit
 *	DDIBM$		IEEE 64-bit to IBM 64-bit
 *	DQIBM$		IEEE 64-bit to IBM 128-bit
 *	QSIBM$		IEEE 128-bit to IBM 32-bit
 *	QDIBM$		IEEE 128-bit to IBM 64-bit
 *	QQIBM$		IEEE 128-bit to IBM 128-bit
 *
 *	EB2AS$		EBCDIC to ASCII
 *	AS2EB$		ASCII to EBCDIC
 */

struct large_info_packet {
	short	denorm;		/* Flush denorms to zero flag */
	short	oflows;		/* Count of overflows */
};

struct small_info_packet {
	short	oflows;		/* Count of overflows */
};

#ifdef	__mips
typedef	float		float_S;
typedef	double		float_D;
typedef	long double	float_Q;
#elif	defined(_CRAYMPP)
#include <complex.h>	/* Use complex wrapper for 128-bit data */
typedef	float		float_S;
typedef	double		float_D;
typedef	double complex	float_Q;
#else
#include <complex.h>	/* Use complex wrapper for 128-bit data */
typedef	int		float_S;
typedef	float		float_D;
typedef	double complex	float_Q;
#pragma _CRI taskcommon T@IEEE
#endif

#ifdef	__mips
struct large_info_packet T$IEEE;
#else
struct large_info_packet T@IEEE;
#endif

#ifdef	_CRAY1
#pragma _CRI taskcommon T@IBM
#endif

#ifdef	__mips
struct small_info_packet T$IBM;
#else
struct small_info_packet T@IBM;
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
#define	set_ieee_overflow()	(T$IEEE.oflows++)
#else
#define	set_ieee_overflow()	(T@IEEE.oflows++)
#endif
#define	set_ieee_underflow()

/* IBM exception setting */

#define	set_ibm_inexact()
#define	set_ibm_invalid()
#ifdef	__mips
#define	set_ibm_overflow()	(T$IBM.oflows++)
#else
#define	set_ibm_overflow()	(T@IBM.oflows++)
#endif
#define	set_ibm_underflow()

/*
 * The following machine-specific functions takes uint32 or uint64 arguments
 * and determines if it is a signaling NaN.  It should not assume that the
 * argument is a NaN.
 */

#define	signaling_S_NaN(word)		0	/* uint32 */
#define	signaling_D_NaN(word)		0	/* uint64 */
#define	signaling_Q_NaN(word1, word2)	0	/* Two uint64's */

#ifdef	_CRAY

#define DSHIFTL(x, y, n, type)	_dshiftl(x, y, n)
#define DSHIFTR(x, y, n, type)	_dshiftr(x, y, n)
#define	LEADZ(x)		_leadz(x)

#else

#define _MASKL(n, type)							\
	((~((type) 0)) << ((sizeof(type) << 3) - (n)))

#define _MASKR(n, type)							\
	(~(~((type) 0) << (n)))

#define DSHIFTL(x, y, n, type)						\
	((n) == 0) ? ((type) (x)) :					\
	((n) == (sizeof(type) << 3)) ? ((type) (y)) :			\
	((n) < 0 || (n) > (sizeof(type) << 3)) ? (type) 0 :		\
	((_MASKL((n), type) & ((type) (y))) >> ((sizeof(type) << 3) - (n)) |\
		(((type) (x)) << (n)))

#define DSHIFTR(x, y, n, type)						\
	((n) == 0) ? ((type) (y)) :					\
	((n) == (sizeof(type) << 3)) ? ((type) (x)) :			\
	((n) < 0 || (n) > (sizeof(type) << 3)) ? (type) 0 :		\
	((_MASKR((n), type) & ((type) (x))) << ((sizeof(type) << 3) - (n)) |\
		(((type) (y)) >> (n)))

/*
 * int LEADZ(n) -- The LEADZ function returns the number of leading '0'
 * bits in data word n.
 */

static int
LEADZ(uint64 opr1)
{
	register int	count;
	register uint64	mask, opr2;

	count	= 0;
	mask	= ~0LL >> 32;
	opr2	= opr1 >> 32;

	if (opr2 == 0) {
		count	= 32;
		opr2	= opr1 & mask;
	}

	mask	= mask >> 16;
	opr1	= opr2 >> 16;

	if (opr1 == 0) {
		count	= count + 16;
		opr1	= opr2 & mask;
	}

	mask	= mask >> 8;
	opr2	= opr1 >> 8;

	if (opr2 == 0) {
		count	= count + 8;
		opr2	= opr1 & mask;
	}

	mask	= mask >> 4;
	opr1	= opr2 >> 4;

	if (opr1 == 0) {
		count	= count + 4;
		opr1	= opr2 & mask;
	}

	mask	= mask >> 2;
	opr2	= opr1 >> 2;

	if (opr2 == 0) {
		count	= count + 2;
		opr2	= opr1 & mask;
	}

	mask	= mask >> 1;
	opr1	= opr2 >> 1;

	if (opr1 == 0) {
		count	= count + 1;
		opr1	= opr2 & mask;
	}

	if (opr1 == 0)
		count	= count + 1;

	return (count);
}

#endif
/*
 ***********************************************************************
 *	IBM to IEEE conversion routines
 ***********************************************************************
 */

/*
 *	IBMSS$		Convert IBM 32-bit to IEEE 32-bit
 *
 *	Exceptions:
 *
 *	Overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_S
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMSS$(union S_box *datum)
#else
ibmss$_(union S_box *datum)
#endif
{
	register uint32	word;		/* Temporary */
	register uint32	result;		/* Result */
	register short	dflush;		/* Denorm flag */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union S_box	r;		/* Result */

	word	= (uint32) ((*datum).sint);
#ifdef	__mips
	dflush	= T$IEEE.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif
	result	= word & SHORT_SIGN_MASK;	/* Set sign */
	man	= ((uint64) word) << (SIZE_OF_INT64 - IBMSES); /* lj mantissa */

	if (man != 0) {
		register short	i, j;

		i	= LEADZ(man) + 1;
		j	= SIZE_OF_INT64 - (SES + i);	/* Normalization count */
		man	= (man >> j) & SMMASK;
		exp	= (((word >> IBMSES) & IBMMX) - IBMBS) << 2;
		exp	= exp + SIEEEBS - i;

		if (exp >= SIEEEMX) {	/* If overflow */
			exp	= SIEEEMX;
			man	= 0;

#if	OVERFLOW_ENABLED
			set_ieee_overflow();
#endif
		}
		else if (exp <= 0) {	/* If underflow */
			i	= 1 - exp;
			exp	= 0;

			if (dflush || i > (SES + 1)) {
				man	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
				}
			else {	/* Denormalize */
#if	INEXACT_ENABLE
				register uint64	temp;	/* Discarded bits */

				temp	= (man >> i) << i;

				if (temp != man)	/* If loss of bits */
					set_ieee_inexact();
#endif
				man	= (man | (1 << SES)) >> i;
			}
		}

		result	= result | (exp << SES) | man;
	}

	r.sint	= result;

	return (r.sflt);
}

/*
 *	IBMSD$		Convert IBM 32-bit to IEEE 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_D
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMSD$(union S_box *datum)
#else
ibmsd$_(union S_box *datum)
#endif
{
	register uint32	word;		/* Temporary */
	register uint64	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union D_box	r;		/* Result */

	word	= (uint32) ((*datum).sint);
	result	= ((uint64) (word & SHORT_SIGN_MASK)) << SIGN_SHIFT;/* Sign */
	man	= ((uint64) word) << (SIZE_OF_INT64 - IBMSES); /* lj mantissa */

	if (man != 0) {
		register short	i, j;

		i	= LEADZ(man) + 1;
		j	= SIZE_OF_INT64 - (DES + i);	/* Normalization count */

		if (j >= 0)	/* If not severely denormalized */
			man	= (man >> j) & DMMASK;
		else
			man	= (man << -j) & DMMASK;

		exp	= (((word >> IBMSES) & IBMMX) - IBMBS) << 2;
		exp	= exp + DIEEEBS - i;

		result	= result | (exp << DES) | man;
	}

	r.dint	= result;

	return (r.dflt);
}

/*
 *	IBMSQ$		Convert IBM 32-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_Q
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMSQ$(union S_box *datum)
#else
ibmsq$_(union S_box *datum)
#endif
{
	register uint32	word;		/* Temporary */
	register uint64	result;		/* Upper result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union Q_box	r;		/* Result */

	word	= (uint32) ((*datum).sint);
	result	= ((uint64) (word & SHORT_SIGN_MASK)) << SIGN_SHIFT;/* Sign */
	man	= ((uint64) word) << (SIZE_OF_INT64 - IBMSES); /* lj mantissa */

	if (man != 0) {
		register short	i, j;

		i	= LEADZ(man) + 1;
		j	= SIZE_OF_INT64 - (QES + i);	/* Normalization count */

		if (j >= 0)	/* If not severely denormalized */
			man	= (man >> j) & QMMASK;
		else
			man	= (man << -j) & QMMASK;

		exp	= (((word >> IBMSES) & IBMMX) - IBMBS) << 2;
		exp	= exp + QIEEEBS - i;

		result	= result | (exp << QES) | man;
	}

	r.qint[0]	= result;
	r.qint[1]	= 0;

	return (r.qflt);
}

/*
 *	IBMDS$		Convert IBM 64-bit to IEEE 32-bit
 *
 *	Exceptions:
 *
 *	Overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_S
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMDS$(union D_box *datum)
#else
ibmds$_(union D_box *datum)
#endif
{
	register uint64	word;		/* Temporary */
	register uint32	result;		/* Result */
	register short	dflush;		/* Denorm flag */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union S_box	r;		/* Result */

	word	= (uint64) ((*datum).dint);
#ifdef	__mips
	dflush	= T$IEEE.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif
	result	= (word & LONG_SIGN_MASK) >> SIGN_SHIFT;	/* Set sign */
	man	= word << (SIZE_OF_INT64 - IBMDES);	/* lj mantissa */

	if (man != 0) {
		register short	i, j;
		register int64	round;		/* Round/carry bit */

		i	= LEADZ(man) + 1;
		j	= SIZE_OF_INT64 - (SES + i);	/* Normalization count */

		if (j > 0) {	/* If not severely denormalized */
#if	INEXACT_ENABLE
			register uint64	temp;	/* Discarded bits */

			temp	= (man >> i) << i;

			if (temp != man)	/* If loss of bits */
				set_ieee_inexact();
#endif

			round	= (man >> (j - 1)) & 1;
			man	= (man >> j) + round;
			round	= man >> (SES + 1);	/* Carry to exponent */
			man	= man & SMMASK;
		}
		else {
			man	= (man << -j) & SMMASK;
			round	= 0;
		}

		exp	= (((word >> IBMDES) & IBMMX) - IBMBS) << 2;
		exp	= exp + SIEEEBS + round - i;

		if (exp >= SIEEEMX) {	/* If overflow */
			exp	= SIEEEMX;
			man	= 0;

#if	OVERFLOW_ENABLED
			set_ieee_overflow();
#endif
		}
		else if (exp <= 0) {	/* If underflow */
			i	= 1 - exp;
			exp	= 0;

			if (dflush || i > (SES + 1)) {
				man	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
				}
			else {	/* Denormalize */
#if	INEXACT_ENABLE
				register uint64	temp;	/* Discarded bits */

				temp	= (man >> i) << i;

				if (temp != man)	/* If loss of bits */
					set_ieee_inexact();
#endif
				man	= (man | (1 << SES)) >> i;
			}
		}

		result	= result | (exp << SES) | man;

	}

	r.sint	= result;

	return (r.sflt);
}

/*
 *	IBMDD$		Convert IBM 64-bit to IEEE 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_D
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMDD$(union D_box *datum)
#else
ibmdd$_(union D_box *datum)
#endif
{
	register uint64	word;		/* Temporary */
	register uint64	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union D_box	r;		/* Result */

	word	= (uint64) ((*datum).dint);
	result	= word & LONG_SIGN_MASK;	/* Set sign */
	man	= word << (SIZE_OF_INT64 - IBMDES);	/* lj mantissa */

	if (man != 0) {
		register short	i, j;
		register int64	round;		/* Round/carry bit */

		i	= LEADZ(man) + 1;
		j	= SIZE_OF_INT64 - (DES + i);	/* Normalization count */

		if (j > 0) {	/* If not severely denormalized */
#if	INEXACT_ENABLE
			register uint64	 temp;

			temp	= (man >> i) << i;

			if (temp != man)	/* If loss of bits */
				set_ieee_inexact();
#endif

			round	= (man >> (j - 1)) & 1;
			man	= (man >> j) + round;
			round	= man >> (DES + 1);	/* Carry to exponent */
			man	= man & DMMASK;
		}
		else {
			man	= (man << -j) & DMMASK;
			round	= 0;
		}

		exp	= (((word >> IBMDES) & IBMMX) - IBMBS) << 2;
		exp	= exp + DIEEEBS + round - i;

		result	= result | (exp << DES) | man;
	}

	r.dint	= result;

	return (r.dflt);
}

/*
 *	IBMDQ$		Convert IBM 64-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_Q
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMDQ$(union D_box *datum)
#else
ibmdq$_(union D_box *datum)
#endif
{
	register uint64	word;		/* Temporary */
	register uint64	upper;		/* Upper result */
	register uint64	lower;		/* Lower result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union Q_box	r;		/* Result */

	word	= (uint64) ((*datum).dint);
	lower	= 0;
	upper	= word & LONG_SIGN_MASK;		/* Set sign */
	man	= word << (SIZE_OF_INT64 - IBMDES);	/* lj mantissa */

	if (man != 0) {
		register short	i, j;

		i	= LEADZ(man) + 1;
		j	= SIZE_OF_INT64 - (QES + i);	/* Normalization count */

		if (j > 0) {	/* If not severely denormalized */
			lower	= DSHIFTR(man, lower, j, uint64);
			man	= (man >> j) & QMMASK;
		}
		else
			man	= (man << -j) & QMMASK;

		exp	= (((word >> IBMDES) & IBMMX) - IBMBS) << 2;
		exp	= exp + QIEEEBS - i;

		upper	= upper | (exp << QES) | man;

	}

	r.qint[0]	= upper;
	r.qint[1]	= lower;

	return (r.qflt);
}

/*
 *	IBMQS$		Convert IBM 128-bit to IEEE 32-bit
 *
 *	Exceptions:
 *
 *	Overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Handling of denorms is controlled by the denorm flag.  If set,
 *	denorms are flushed to zero.
 */

float_S
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMQS$(union Q_box *datum)
#else
ibmqs$_(union Q_box *datum)
#endif
{
	register uint64	word1;		/* Temporary */
	register uint64	word2;		/* Temporary */
	register uint32	result;		/* Result */
	register short	dflush;		/* Denorm flag */
	register int64	exp;		/* Exponent */
	register uint64	man1;		/* Mantissa */
	register uint64	man2;		/* Mantissa */
	union S_box	r;		/* Result */

	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);
#ifdef	__mips
	dflush	= T$IEEE.denorm;
#else
	dflush	= T@IEEE.denorm;
#endif
	result	= (word1 & LONG_SIGN_MASK) >> SIGN_SHIFT;	/* Set sign */
	man2	= word2 << (IME + 1);	/* Discard lower exponent */
	man1	= DSHIFTL(word1, man2, SIZE_OF_INT64 - IBMQES, uint64);
	man2	= man2 << (SIZE_OF_INT64 - IBMQES);

	if ((man1 | man2) != 0) {
		register short	i, j;
		register int64	round;	/* Round/carry bit */
#if	INEXACT_ENABLE
		register uint64	temp;	/* Discarded bits */
#endif

		if (man1 != 0)
			i	= LEADZ(man1) + 1;
		else
			i	= SIZE_OF_INT64 + LEADZ(man2) + 1;

		j	= SIZE_OF_INT64 - (SES + i);	/* Normalization count */

		if (j > 0) {	/* If not severely denormalized */

#if	INEXACT_ENABLE
			temp	= DSHIFTR(man1, (uint64) 0, j, uint64) | man2;
#endif
			round	= (man1 >> (j - 1)) & 1;
			man1	= (man1 >> j) + round;
		}
		else {
			round	= DSHIFTL(man1, man2, (1 - j), uint64) & 1;
			man1	= DSHIFTL(man1, man2, -j, uint64) + round;
#if	INEXACT_ENABLE
			temp	= man2 << -j;
#endif
		}

#if	INEXACT_ENABLE
		if (temp != 0)	/* If loss of bits */
			set_ieee_inexact();
#endif

		round	= man1 >> (SES + 1);	/* Carry to exponent */
		man1	= man1 & SMMASK;

		exp	= (((word1 >> IBMQES) & IBMMX) - IBMBS) << 2;
		exp	= exp + SIEEEBS + round - i;

		if (exp >= SIEEEMX) {	/* If overflow */
			exp	= SIEEEMX;
			man1	= 0;

#if	OVERFLOW_ENABLED
			set_ieee_overflow();
#endif
		}
		else if (exp <= 0) {	/* If underflow */
			i	= 1 - exp;
			exp	= 0;

			if (dflush || i > (SES + 1)) {
				man1	= 0;
#if	UNDERFLOW_ENABLE
				set_ieee_underflow();
#endif
				}
			else {	/* Denormalize */
#if	INEXACT_ENABLE
				temp	= DSHIFTR(man1, (uint64) 0, i, uint64) | man2;

				if (temp != 0)	/* If loss of bits */
					set_ieee_inexact();
#endif
				man1	= (man1 | (1 << SES)) >> i;
			}
		}

		result	= result | (exp << SES) | man1;

	}

	r.sint	= result;

	return (r.sflt);
}

/*
 *	IBMQD$		Convert IBM 128-bit to IEEE 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_D
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMQD$(union Q_box *datum)
#else
ibmqd$_(union Q_box *datum)
#endif
{
	register uint64	word1;		/* Temporary */
	register uint64	word2;		/* Temporary */
	register uint64	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man1;		/* Mantissa */
	register uint64	man2;		/* Mantissa */
	union D_box	r;		/* Result */

	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);
	result	= word1 & LONG_SIGN_MASK;	/* Set sign */
	man2	= word2 << (IME + 1);	/* Discard lower exponent */
	man1	= DSHIFTL(word1, man2, SIZE_OF_INT64 - IBMQES, uint64);
	man2	= man2 << (SIZE_OF_INT64 - IBMQES);

	if ((man1 | man2) != 0) {
		register short	i, j;
		register int64	round;	/* Round/carry bit */
#if	INEXACT_ENABLE
		register uint64	temp;	/* Discarded bits */
#endif

		if (man1 != 0)
			i	= LEADZ(man1) + 1;
		else
			i	= SIZE_OF_INT64 + LEADZ(man2) + 1;

		j	= SIZE_OF_INT64 - (DES + i);	/* Normalization count */

		if (j > 0) {	/* If not severely denormalized */

#if	INEXACT_ENABLE
			temp	= DSHIFTR(man1, (uint64) 0, j, uint64) | man2;
#endif
			round	= (man1 >> (j - 1)) & 1;
			man1	= (man1 >> j) + round;
		}
		else {

			round	= DSHIFTL(man1, man2, (1 - j), uint64) & 1;
			man1	= DSHIFTL(man1, man2, -j, uint64) + round;
#if	INEXACT_ENABLE
			temp	= man2 << -j;
#endif
		}

#if	INEXACT_ENABLE
		if (temp != 0)	/* If loss of bits */
			set_ieee_inexact();
#endif

		round	= man1 >> (DES + 1);	/* Carry to exponent */
		man1	= man1 & DMMASK;

		exp	= (((word1 >> IBMDES) & IBMMX) - IBMBS) << 2;
		exp	= exp + DIEEEBS + round - i;

		result	= result | (exp << DES) | man1;

	}

	r.dint	= result;

	return (r.dflt);
}

/*
 *	IBMQQ$		Convert IBM 128-bit to IEEE 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	No denorms can be produced as the result of this conversion.
 */

float_Q
#if	defined(_CRAY) || defined(_CRAYMPP)
IBMQQ$(union Q_box *datum)
#else
ibmqq$_(union Q_box *datum)
#endif
{
	register uint64	word1;		/* Temporary */
	register uint64	word2;		/* Temporary */
	register uint64	upper;		/* Upper result */
	register uint64	lower;		/* Lower result */
	register int64	exp;		/* Exponent */
	register uint64	man1;		/* Mantissa */
	register uint64	man2;		/* Mantissa */
	union Q_box	r;		/* Result */

	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);
	lower	= 0;
	upper	= word1 & LONG_SIGN_MASK;	/* Set sign */
	man2	= word2 << (IME + 1);	/* Discard lower exponent */
	man1	= DSHIFTL(word1, man2, SIZE_OF_INT64 - IBMQES, uint64);
	man2	= man2 << (SIZE_OF_INT64 - IBMQES);

	if ((man1 | man2) != 0) {
		register short	i, j;

		if (man1 != 0)
			i	= LEADZ(man1) + 1;
		else
			i	= SIZE_OF_INT64 + LEADZ(man2) + 1;

		j	= SIZE_OF_INT64 - (QES + i);	/* Normalization count */

		if (j > 0) {	/* If not severely denormalized */
			lower	= DSHIFTR(man1, man2, j, uint64);
			man1	= (man1 >> j) & QMMASK;
		}
		else {
			man1	= DSHIFTL(man1, man2, -j, uint64) & QMMASK;
			lower	= man2 << -j;
		}

		exp	= (((word1 >> IBMQES) & IBMMX) - IBMBS) << 2;
		exp	= exp + QIEEEBS - i;

		upper	= upper | (exp << QES) | man1;

	}

	r.qint[0]	= upper;
	r.qint[1]	= lower;

	return (r.qflt);
}

/*
 ***********************************************************************
 *	IEEE to IBM conversion routines
 ***********************************************************************
 */

/*
 *	SSIBM$		Convert IEEE 32-bit to IBM 32-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_S
#if	defined(_CRAY) || defined(_CRAYMPP)
SSIBM$(union S_box *datum)
#else
ssibm$_(union S_box *datum)
#endif
{
	register uint32	word;		/* Temporary */
	register uint32	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union S_box	r;		/* Result */

	word	= (uint32) ((*datum).sint);
	result	= word & SHORT_SIGN_MASK;	/* Set sign */
	exp	= (word >> SES) & SIEEEMX;	/* Isolate exponent */
	man	= ((uint64) word) << (SIZE_OF_INT64 - SES); /* lj mantissa */

	if ((exp | man) != 0) {		/* If nonzero */
		register short	i;
		register int64	round;	/* Round/carry bit */
#if	INEXACT_ENABLE
		register uint64	temp;	/* Discarded bits */
#endif

		if (exp == SIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_S_NaN(word))
				set_ieee_invalid();
#endif
			result	= result | 0x7FFFFFFF;
			goto done;
		}

		if (exp == 0) {		/* If denorm */
			i	= LEADZ(man);
			man	= man << i;
		}
		else {	/* Set explicit normalization bit */
			i	= 0;
			man	= LONG_SIGN_MASK | (man >> 1);
		}

		exp	= (exp - SIEEEBS) - i;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;
		i	= i + SIZE_OF_INT64 - IBMSES;
		round	= (man >> (i - 1)) & 1;

#if	INEXACT_ENABLE
		temp	= (man >> i) << i;

		if (temp != man)	/* If loss of bits */
			set_ibm_inexact();
#endif

		/*
		 * Round mantissa.  Propagate the round to the exponent,
		 * if necessary, and ensure that the mantissa isn't
		 * cleared as the result of the round.
		 */

		man	= (man >> i) + round;
		exp	= exp << IBMSES;
		exp	= exp + (man & ~ISMASK);
		man	= man & ISMASK;

		if (man == 0)	/* If rounding cleared mantissa */
			man	= LONG_SIGN_MASK >> (i + 3);

		result	= result | exp | man;
	}

done:
	r.sint	= result;

	return (r.sflt);
}

/*
 *	SDIBM$		Convert IEEE 32-bit to IBM 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_D
#if	defined(_CRAY) || defined(_CRAYMPP)
SDIBM$(union S_box *datum)
#else
sdibm$_(union S_box *datum)
#endif
{
	register uint32	word;		/* Temporary */
	register uint64	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union D_box	r;		/* Result */

	word	= (uint32) ((*datum).sint);
	result	= ((uint64) (word & SHORT_SIGN_MASK)) << SIGN_SHIFT;/* Sign */
	exp	= (word >> SES) & SIEEEMX;	/* Isolate exponent */
	man	= ((uint64) word) << (SIZE_OF_INT64 - SES); /* lj mantissa */

	if ((exp | man) != 0) {		/* If nonzero */
		register short	i;

		if (exp == SIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_S_NaN(word))
				set_ieee_invalid();
#endif
			result	= result | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		if (exp == 0) {		/* If denorm */
			i	= LEADZ(man);
			man	= man << i;
		}
		else {	/* Set explicit normalization bit */
			i	= 0;
			man	= LONG_SIGN_MASK | (man >> 1);
		}

		exp	= (exp - SIEEEBS) - i;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;
		i	= i + SIZE_OF_INT64 - IBMDES;
		man	= man >> i;

		result	= result | ((exp << IBMDES) + man);
	}

done:
	r.dint	= result;

	return (r.dflt);
}

/*
 *	SQIBM$		Convert IEEE 32-bit to IBM 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_Q
#if	defined(_CRAY) || defined(_CRAYMPP)
SQIBM$(union S_box *datum)
#else
sqibm$_(union S_box *datum)
#endif
{
	register uint32	word;		/* Temporary */
	register uint64	upper;		/* Upper result */
	register uint64	lower;		/* Lower result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union Q_box	r;		/* Result */

	word	= (uint32) ((*datum).sint);
	lower	= 0;
	upper	= ((uint64) (word & SHORT_SIGN_MASK)) << SIGN_SHIFT;/* Sign */
	exp	= (word >> SES) & SIEEEMX;	/* Isolate exponent */
	man	= ((uint64) word) << (SIZE_OF_INT64 - SES); /* lj mantissa */

	if ((exp | man) != 0) {		/* If nonzero */
		register short	i;

		if (exp == SIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_S_NaN(word))
				set_ieee_invalid();
#endif
			lower	= upper | 0x71FFFFFFFFFFFFFF;
			upper	= upper | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		if (exp == 0) {		/* If denorm */
			i	= LEADZ(man);
			man	= man << i;
		}
		else {	/* Set explicit normalization bit */
			i	= 0;
			man	= LONG_SIGN_MASK | (man >> 1);
		}

		exp	= (exp - SIEEEBS) - i;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;
		i	= i + SIZE_OF_INT64 - IBMQES;
		man	= man >> i;

		upper	= upper | ((exp << IBMQES) + man);
	}

done:
	r.qint[0]	= upper;
	r.qint[1]	= lower;

	return (r.qflt);
}

/*
 *	DSIBM$		Convert IEEE 64-bit to IBM 32-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_S
#if	defined(_CRAY) || defined(_CRAYMPP)
DSIBM$(union D_box *datum)
#else
dsibm$_(union D_box *datum)
#endif
{
	register uint64	word;		/* Temporary */
	register uint32	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union S_box	r;		/* Result */

	word	= (uint64) ((*datum).dint);
	result	= (word & LONG_SIGN_MASK) >> SIGN_SHIFT;	/* Set sign */
	exp	= (word >> DES) & DIEEEMX;	/* Isolate exponent */
	man	= word << (SIZE_OF_INT64 - DES);	/* lj mantissa */

	if ((exp | man) != 0) {		/* If nonzero */
		register short	i;
		register int64	round;	/* Round/carry bit */
#if	INEXACT_ENABLE
		register uint64	temp;	/* Discarded bits */
#endif

		if (exp == DIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_D_NaN(word))
				set_ieee_invalid();
#endif
			result	= result | 0x7FFFFFFF;
			goto done;
		}

		exp	= exp - DIEEEBS;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;

		if (exp > IBMMX) {	/* If overflow on conversion */
#if	OVERFLOW_ENABLE
			set_ibm_overflow();
#endif
			result	= result | 0x7FFFFFFF;
			goto done;
		}

		if (exp < 0) {	/* If underflow */
#if	UNDERFLOW_ENABLE
			set_ibm_underflow();
#endif
			goto done;
		}

		i	= i + SIZE_OF_INT64 - IBMSES;
		man	= LONG_SIGN_MASK | (man >> 1);
		round	= (man >> (i - 1)) & 1;

#if	INEXACT_ENABLE
		temp	= (man >> i) << i;

		if (temp != man)	/* If loss of bits */
			set_ibm_inexact();
#endif

		/*
		 * Round mantissa.  Propagate the round to the exponent,
		 * if necessary, and ensure that the mantissa isn't
		 * cleared as the result of the round.
		 */

		man	= (man >> i) + round;
		exp	= exp << IBMSES;
		exp	= exp + (man & ~ISMASK);
		man	= man & ISMASK;

		if (man == 0)	/* If rounding cleared mantissa */
			man	= LONG_SIGN_MASK >> (i + 3);

		result	= result | exp | man;
	}

done:
	r.sint	= result;

	return (r.sflt);
}

/*
 *	DDIBM$		Convert IEEE 64-bit to IBM 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_D
#if	defined(_CRAY) || defined(_CRAYMPP)
DDIBM$(union D_box *datum)
#else
ddibm$_(union D_box *datum)
#endif
{
	register uint64	word;		/* Temporary */
	register uint64	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union D_box	r;		/* Result */

	word	= (uint64) ((*datum).dint);
	result	= word & LONG_SIGN_MASK;	/* Set sign */
	exp	= (word >> DES) & DIEEEMX;	/* Isolate exponent */
	man	= word << (SIZE_OF_INT64 - DES);	/* lj mantissa */

	if ((exp | man) != 0) {		/* If nonzero */
		register short	i;

		if (exp == DIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_D_NaN(word))
				set_ieee_invalid();
#endif
			result	= result | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		exp	= exp - DIEEEBS;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;

		if (exp > IBMMX) {	/* If overflow on conversion */
#if	OVERFLOW_ENABLE
			set_ibm_overflow();
#endif
			result	= result | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		if (exp < 0) {	/* If underflow */
#if	UNDERFLOW_ENABLE
			set_ibm_underflow();
#endif
			goto done;
		}

		man	= LONG_SIGN_MASK | (man >> 1);
		i	= i + SIZE_OF_INT64 - IBMDES;
		man	= man >> i;

		result	= result | ((exp << IBMDES) + man);
	}

done:
	r.dint	= result;

	return (r.dflt);
}

/*
 *	DQIBM$		Convert IEEE 64-bit to IBM 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_Q
#if	defined(_CRAY) || defined(_CRAYMPP)
DQIBM$(union D_box *datum)
#else
dqibm$_(union D_box *datum)
#endif
{
	register uint64	word;		/* Temporary */
	register uint64	upper;		/* Upper result */
	register uint64	lower;		/* Lower result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union Q_box	r;		/* Result */

	word	= (uint64) ((*datum).dint);
	lower	= 0;
	upper	= word & LONG_SIGN_MASK;	/* Set sign */
	exp	= (word >> DES) & DIEEEMX;	/* Isolate exponent */
	man	= word << (SIZE_OF_INT64 - DES);	/* lj mantissa */

	if ((exp | man) != 0) {		/* If nonzero */
		register short	i;

		if (exp == DIEEEMX) {	/* If overflow/NaN */
#if	INVALID_ENABLE
			if (signaling_D_NaN(word))
				set_ieee_invalid();
#endif
			lower	= upper | 0x71FFFFFFFFFFFFFF;
			upper	= upper | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		exp	= exp - DIEEEBS;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;

		if (exp > IBMMX) {	/* If overflow on conversion */
#if	OVERFLOW_ENABLE
			set_ibm_overflow();
#endif
			lower	= upper | 0x71FFFFFFFFFFFFFF;
			upper	= upper | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		if (exp < 0) {	/* If underflow */
#if	UNDERFLOW_ENABLE
			set_ibm_underflow();
#endif
			goto done;
		}

		man	= LONG_SIGN_MASK | (man >> 1);
		i	= i + SIZE_OF_INT64 - IBMQES;
		man	= man >> i;

		upper	= upper | ((exp << IBMQES) + man);
	}

done:
	r.qint[0]	= upper;
	r.qint[1]	= lower;

	return (r.qflt);
}

/*
 *	QSIBM$		Convert IEEE 128-bit to IBM 32-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_S
#if	defined(_CRAY) || defined(_CRAYMPP)
QSIBM$(union Q_box *datum)
#else
qsibm$_(union Q_box *datum)
#endif
{
	register uint64	word1;		/* Temporary */
	register uint64	word2;		/* Temporary */
	register uint32	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man;		/* Mantissa */
	union S_box	r;		/* Result */

	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);
	result	= (word1 & LONG_SIGN_MASK) >> SIGN_SHIFT;	/* Set sign */
	exp	= (word1 >> QES) & QIEEEMX;	/* Isolate exponent */
	man	= word1 << (SIZE_OF_INT64 - QES);	/* lj mantissa */

	if ((exp | man | word2) != 0) {		/* If nonzero */
		register short	i;
		register int64	round;	/* Round/carry bit */
#if	INEXACT_ENABLE
		register uint64	temp;	/* Discarded bits */
#endif

		if (exp == QIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_Q_NaN(word1, word2))
				set_ieee_invalid();
#endif
			result	= result | 0x7FFFFFFF;
			goto done;
		}

		exp	= exp - QIEEEBS;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;

		if (exp > IBMMX) {	/* If overflow on conversion */
#if	OVERFLOW_ENABLE
			set_ibm_overflow();
#endif
			result	= result | 0x7FFFFFFF;
			goto done;
		}

		if (exp < 0) {	/* If underflow */
#if	UNDERFLOW_ENABLE
			set_ibm_underflow();
#endif
			goto done;
		}

		i	= i + SIZE_OF_INT64 - IBMSES;
		man	= LONG_SIGN_MASK | (man >> 1);
		round	= (man >> (i - 1)) & 1;

#if	INEXACT_ENABLE
		temp	= (man >> i) << i;

		if (temp != man || word2 != 0)	/* If loss of bits */
			set_ibm_inexact();
#endif

		/*
		 * Round mantissa.  Propagate the round to the exponent,
		 * if necessary, and ensure that the mantissa isn't
		 * cleared as the result of the round.
		 */

		man	= (man >> i) + round;
		exp	= exp << IBMSES;
		exp	= exp + (man & ~ISMASK);
		man	= man & ISMASK;

		if (man == 0)	/* If rounding cleared mantissa */
			man	= LONG_SIGN_MASK >> (i + 3);

		result	= result | exp | man;
	}

done:
	r.sint	= result;

	return (r.sflt);
}

/*
 *	QDIBM$		Convert IEEE 128-bit to IBM 64-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_D
#if	defined(_CRAY) || defined(_CRAYMPP)
QDIBM$(union Q_box *datum)
#else
qdibm$_(union Q_box *datum)
#endif
{
	register uint64	word1;		/* Temporary */
	register uint64	word2;		/* Temporary */
	register uint64	result;		/* Result */
	register int64	exp;		/* Exponent */
	register uint64	man1;		/* Upper mantissa */
	register uint64	man2;		/* Lower mantissa */
	union D_box	r;		/* Result */

	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);
	result	= word1 & LONG_SIGN_MASK;	/* Set sign */
	exp	= (word1 >> QES) & QIEEEMX;	/* Isolate exponent */
	man1	= DSHIFTL(word1, word2, SIZE_OF_INT64 - QES, uint64);	/* lj mantissa */
	man2	= word2 << (SIZE_OF_INT64 - QES);

	if ((exp | man1 | man2) != 0) {		/* If nonzero */
		register short	i;
		register int64	round;	/* Round/carry bit */
#if	INEXACT_ENABLE
		register uint64	temp;	/* Discarded bits */
#endif

		if (exp == QIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_Q_NaN(word1, word2))
				set_ieee_invalid();
#endif
			result	= result | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		exp	= exp - QIEEEBS;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;

		if (exp > IBMMX) {	/* If overflow on conversion */
#if	OVERFLOW_ENABLE
			set_ibm_overflow();
#endif
			result	= result | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		if (exp < 0) {	/* If underflow */
#if	UNDERFLOW_ENABLE
			set_ibm_underflow();
#endif
			goto done;
		}

		man1	= LONG_SIGN_MASK | (man1 >> 1);
		i	= i + SIZE_OF_INT64 - IBMDES;
		round	= (man1 >> (i - 1)) & 1;

#if	INEXACT_ENABLE
		temp	= (man >> i) << i;

		if (temp != man || word2 != 0)	/* If loss of bits */
			set_ibm_inexact();
#endif

		/*
		 * Round mantissa.  Propagate the round to the exponent,
		 * if necessary, and ensure that the mantissa isn't
		 * cleared as the result of the round.
		 */

		man1	= (man1 >> i) + round;
		exp	= exp << IBMDES;
		exp	= exp + (man1 & ~IDMASK);
		man1	= man1 & IDMASK;

		if (man1 == 0)	/* If rounding cleared mantissa */
			man1	= LONG_SIGN_MASK >> (i + 3);

		result	= result | exp | man1;
	}

done:
	r.dint	= result;

	return (r.dflt);
}

/*
 *	QQIBM$		Convert IEEE 128-bit to IBM 128-bit
 *
 *	Exceptions:
 *
 *	No overflows or underflows can be produced as the result of this
 *	conversion.
 *
 *	Inexact (loss of precision) can not be produced as the result of
 *	this conversion.
 *
 *	Invalid (signalling NaN) can be produced as the result of this
 *	conversion.
 */

float_Q
#if	defined(_CRAY) || defined(_CRAYMPP)
QQIBM$(union Q_box *datum)
#else
qqibm$_(union Q_box *datum)
#endif
{
	register uint64	word1;		/* Temporary */
	register uint64	word2;		/* Temporary */
	register uint64	upper;		/* Upper result */
	register uint64	lower;		/* Lower result */
	register int64	exp;		/* Exponent */
	register uint64	man1;		/* Upper mantissa */
	register uint64	man2;		/* Lower mantissa */
	union Q_box	r;		/* Result */

	word1	= (uint64) ((*datum).qint[0]);
	word2	= (uint64) ((*datum).qint[1]);
	lower	= 0;
	upper	= word1 & LONG_SIGN_MASK;	/* Set sign */
	exp	= (word1 >> QES) & QIEEEMX;	/* Isolate exponent */
	man1	= DSHIFTL(word1, word2, SIZE_OF_INT64 - QES, uint64);	/* lj mantissa */
	man2	= word2 << (SIZE_OF_INT64 - QES);

	if ((exp | man1 | man2) != 0) {		/* If nonzero */
		register short	i;

		if (exp == QIEEEMX) {	/* If overflow/NaN input */
#if	INVALID_ENABLE
			if (signaling_Q_NaN(word1, word2))
				set_ieee_invalid();
#endif
			lower	= upper | 0x71FFFFFFFFFFFFFF;
			upper	= upper | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		exp	= exp - QIEEEBS;	/* True exponent */
		i	= (exp & 0x3) ^ 0x3;
		exp	= exp + ((i + 1) >> 2);	/* Round exponent up */
		exp	= (exp + (IBMBS << 2) + 0x3) >> 2;

		if (exp > IBMMX) {	/* If overflow on conversion */
#if	OVERFLOW_ENABLE
			set_ibm_overflow();
#endif
			lower	= upper | 0x71FFFFFFFFFFFFFF;
			upper	= upper | 0x7FFFFFFFFFFFFFFF;
			goto done;
		}

		if (exp < 0) {	/* If underflow */
#if	UNDERFLOW_ENABLE
			set_ibm_underflow();
#endif
			goto done;
		}

		man2	= DSHIFTR(man1, man2, 1, uint64);
		man1	= LONG_SIGN_MASK | (man1 >> 1);
		i	= i + SIZE_OF_INT64 - IBMQES;
		man2	= DSHIFTR(man1, man2, i, uint64);

		if (man2 != 0) {
			register int64	exp2;	/* Lower exponent */

			man2	= man2 >> (IME + 1);
			exp2	= (exp + 128 - 14) & IBMMX;
			lower	= upper | (exp2 << IBMQES) | man2;
		}

		man1	= man1 >> i;
		upper	= upper | (exp << IBMQES) | man1;
	}

done:
	r.qint[0]	= upper;
	r.qint[1]	= lower;

	return (r.qflt);
}

/*
 *	EB2AS$		Convert EBCDIC characters to ASCII characters
 */

void
#if	defined(_CRAY) || defined(_CRAYMPP)
EB2AS$(
#else
eb2as$_(
#endif
	uint64	*ebcdic,	/* Word pointer to EBCDIC data */
	uint64	*eoff,		/* Byte offset of start of EBCDIC data (0 - 7) */
	uint64	*ascii,		/* Word pointer to ASCII data */
	uint64	*aoff,		/* Byte offset of start of ASCII data (0 - 7) */
	int64	*chars)		/* Number of characters to convert */
{
	register short	ishft;
	register short	oshft;
	register int	count;
	register uint64	input;
	register uint64	output;
	uint64		*table = _EBCDIC_TO_ASCII;

	count	= *chars;
	ishft	= *eoff;
	oshft	= *aoff;
	output	= 0;

	if (count <= 0 || ishft >= sizeof(int64) || oshft >= sizeof(int64))
		return;

	input	= *ebcdic++;
	ishft	= SIZE_OF_INT64 - (ishft << 3);
	oshft	= SIZE_OF_INT64 - (oshft << 3);

	if (oshft != SIZE_OF_INT64) {	/* If starting in mid-word */
		output	= *ascii;
		output	= output >> oshft;
	}

	do {
		register short	ch;

		ishft	= ishft - 8;
		ch	= (input >> ishft) & 0377;
		ch	= (table[ch >> 3] >> ((ch & 07) << 3)) & 0377;
		output	= (output << 8) | ch;
		count	= count - 1;

		if (ishft == 0) {
			input	= *ebcdic++;
			ishft	= SIZE_OF_INT64;
		}

		oshft	= oshft - 8;

		if (oshft == 0) {
			*ascii++	= output;
			oshft	= SIZE_OF_INT64;
			output	= 0;
		}

	} while (count > 0);

	/* If partial word remains, blend it in */

	if (oshft != 64) {	/* If partial word available */
		output	= DSHIFTL(output, *ascii, oshft, uint64);
		*ascii	= output;
	}

	return;
}

/*
 *	AS2EB$		Convert ASCII characters to EBCDIC characters
 */

void
#if	defined(_CRAY) || defined(_CRAYMPP)
AS2EB$(
#else
as2eb$_(
#endif
	uint64	*ascii,		/* Word pointer to ASCII data */
	uint64	*aoff,		/* Byte offset of start of ASCII data (0 - 7) */
	uint64	*ebcdic,	/* Word pointer to EBCDIC data */
	uint64	*eoff,		/* Byte offset of start of EBCDIC data (0 - 7) */
	int64	*chars)		/* Number of characters to convert */
{
	register short		ishft;
	register short		oshft;
	register int		count;
	register uint64	input;
	register uint64	output;
	uint64		*table = _ASCII_TO_EBCDIC;

	count	= *chars;
	ishft	= *aoff;
	oshft	= *eoff;
	output	= 0;

	if (count <= 0 || ishft >= sizeof(int64) || oshft >= sizeof(int64))
		return;

	input	= *ascii++;
	ishft	= SIZE_OF_INT64 - (ishft << 3);
	oshft	= SIZE_OF_INT64 - (oshft << 3);

	if (oshft != SIZE_OF_INT64) {	/* If starting in mid-word */
		output	= *ebcdic;
		output	= output >> oshft;
	}

	do {
		register short	ch;

		ishft	= ishft - 8;
		ch	= (input >> ishft) & 0177;
		ch	= (table[ch >> 3] >> ((ch & 07) << 3)) & 0377;
		output	= (output << 8) | ch;
		count	= count - 1;

		if (ishft == 0) {
			input	= *ascii++;
			ishft	= SIZE_OF_INT64;
		}

		oshft	= oshft - 8;

		if (oshft == 0) {
			*ebcdic++	= output;
			oshft	= SIZE_OF_INT64;
			output	= 0;
		}

	} while (count > 0);

	/* If partial word remains, blend it in */

	if (oshft != 64) {	/* If partial word available */
		output	= DSHIFTL(output, *ebcdic, oshft, uint64);
		*ebcdic	= output;
	}

	return;
}
