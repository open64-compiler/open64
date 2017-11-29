/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

/* USMID @(#) clibinc/fmath.h	92.3	06/25/99 14:36:12 */

/* The following values are used by the F90 compiler for a set of environment
 * intrinsics.  Some of the values are also used by the libf/intrin library
 * to return a value for some of the array intrinsics: DIGITS, EPSILON, HUGE,
 *  MAXEXPONENT, MINEXPONENT, PRECISION, RADIX, RANGE, TINY.
 * 
 * The F90 compiler needs a complete set of each IEEE and non-IEEE values.
 * Please keep the complete set of values for YMP and for IEEE.
 *
 * YMP/C90/etc:
 * Integer*1 =            2147483647 signed
 * Integer*2 =            2147483647 signed
 * Integer*4 =            2147483647 signed
 * Integer*6 =        70368744177663 46-bit value
 * Integer*8 =   9223372036854775807 signed
 * IEEE MPP and SPARC:
 * Integer*1 =            2147483647 signed
 * Integer*2 =            2147483647 signed
 * Integer*4 =            2147483647 signed
 * Integer*6 =   9223372036854775807 signed
 * Integer*8 =   9223372036854775807 signed
 * IEEE TRITON:
 * Integer*1 =            2147483647 signed
 * Integer*2 =            2147483647 signed
 * Integer*4 =            2147483647 signed
 * Integer*6 =      1125899906842623 50-bit limit - for fastmode only
 * Integer*8 =   9223372036854775807 signed
 */
/* BIT_SIZE returns the number of bits s for an integer of kind n.
 * An integer object consists of s bits in sequence numbered from right to
 * left from zero to (s-1).  Fastmode bitsize is 64 bits on TRITON,ieee.
 */
/* YMP: */
#define BITSIZE_INT1_F90_Y      8   /* Integer (KIND=1) */
#define BITSIZE_INT2_F90_Y     16   /* Integer (KIND=2) */
#define BITSIZE_INT4_F90_Y     32   /* Integer (KIND=4) */
#define BITSIZE_INT6_F90_Y     46   /* Integer (KIND=6), 46-bit value */
#define BITSIZE_INT8_F90_Y     64   /* Integer (KIND=8) */
/* IEEE: */
#define BITSIZE_INT1_F90_IEEE     8   /* Integer (KIND=1) */
#define BITSIZE_INT2_F90_IEEE    16   /* Integer (KIND=2) */
#define BITSIZE_INT4_F90_IEEE    32   /* Integer (KIND=4) */
#define BITSIZE_INT6_F90_IEEE    64   /* Integer (KIND=6) */
#define BITSIZE_INT8_F90_IEEE    64   /* Integer (KIND=8) */
#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define BITSIZE_INT1_F90		BITSIZE_INT1_F90_IEEE
#define BITSIZE_INT2_F90		BITSIZE_INT2_F90_IEEE
#define BITSIZE_INT4_F90		BITSIZE_INT4_F90_IEEE
#define BITSIZE_INT6_F90		BITSIZE_INT6_F90_IEEE
#define BITSIZE_INT8_F90		BITSIZE_INT8_F90_IEEE
#else
#define BITSIZE_INT1_F90		BITSIZE_INT1_F90_Y
#define BITSIZE_INT2_F90		BITSIZE_INT2_F90_Y
#define BITSIZE_INT4_F90		BITSIZE_INT4_F90_Y
#define BITSIZE_INT6_F90		BITSIZE_INT6_F90_Y
#define BITSIZE_INT8_F90		BITSIZE_INT8_F90_Y
#endif

/* DIGITS for integer, real, and double precision is number of significant
 * digits in bits in the model.
 * DIGITS on IEEE is the number of mantissa bits and the implicit bit.
 */
/* YMP: */
#define DIGITS_INT1_F90_Y      7   /* Integer (KIND=1) */
#define DIGITS_INT2_F90_Y     15   /* Integer (KIND=2) */
#define DIGITS_INT4_F90_Y     31   /* Integer (KIND=4) */
#define DIGITS_INT6_F90_Y     46   /* Integer (KIND=6), 46-bit value */
#define DIGITS_INT8_F90_Y     63   /* Integer (KIND=8) */
#define DIGITS_REAL4_F90_Y    24   /* Real (KIND=4)    */
#define DIGITS_REAL8_F90_Y    47   /* Real (KIND=8)    */
#define DIGITS_REAL16_F90_Y   95   /* Real (KIND=16)   */
/* IEEE: */
#define DIGITS_INT1_F90_IEEE      7   /* Integer (KIND=1) */
#define DIGITS_INT2_F90_IEEE     15   /* Integer (KIND=2) */
#define DIGITS_INT4_F90_IEEE     31   /* Integer (KIND=4) */
#if defined (_CRAY1) && defined(_CRAYIEEE)
/* change DIGITS_INT6_F90_IEEE 50 for Integer (KIND=6) to allow
 * values to match as 46 bits across PVP systems
 */
#define DIGITS_INT6_F90_IEEE     46   /* Integer (KIND=6) */
#else
#define DIGITS_INT6_F90_IEEE     63   /* Integer (KIND=6) */
#endif
#define DIGITS_INT8_F90_IEEE     63   /* Integer (KIND=8) */
#define DIGITS_REAL4_F90_IEEE    24   /* Real (KIND=4)    */
#define DIGITS_REAL8_F90_IEEE    53   /* Real (KIND=8)    */
#if	defined(_CRAYMPP)
#define DIGITS_REAL16_F90_IEEE   53   /* Real (KIND=16)   */
#elif	defined(__mips)
#define DIGITS_REAL16_F90_IEEE  107   /* Real (KIND=16)   */
#elif	defined(_LITTLE_ENDIAN) && !defined(__sv2)
#define DIGITS_REAL16_F90_IEEE   64   /* Real (KIND=16)   */
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE)) || \
	(defined(_LITTLE_ENDIAN)  && defined(__sv2))
#define DIGITS_REAL16_F90_IEEE  113   /* Real (KIND=16)   */
#endif

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define DIGITS_INT1_F90		DIGITS_INT1_F90_IEEE
#define DIGITS_INT2_F90		DIGITS_INT2_F90_IEEE
#define DIGITS_INT4_F90		DIGITS_INT4_F90_IEEE
#define DIGITS_INT6_F90		DIGITS_INT6_F90_IEEE
#define DIGITS_INT8_F90		DIGITS_INT8_F90_IEEE
#define DIGITS_REAL4_F90	DIGITS_REAL4_F90_IEEE
#define DIGITS_REAL8_F90	DIGITS_REAL8_F90_IEEE
#define DIGITS_REAL16_F90	DIGITS_REAL16_F90_IEEE
#else
#define DIGITS_INT1_F90		DIGITS_INT1_F90_Y
#define DIGITS_INT2_F90		DIGITS_INT2_F90_Y
#define DIGITS_INT4_F90		DIGITS_INT4_F90_Y
#define DIGITS_INT6_F90		DIGITS_INT6_F90_Y
#define DIGITS_INT8_F90		DIGITS_INT8_F90_Y
#define DIGITS_REAL4_F90	DIGITS_REAL4_F90_Y
#define DIGITS_REAL8_F90	DIGITS_REAL8_F90_Y
#define DIGITS_REAL16_F90	DIGITS_REAL16_F90_Y
#endif

/* EPSILON for real and double precision
 * number that is almost negligible compared to one in the model
 * Real result = (b)**(1-p) = 2**(-46) where p = 47
 * real (KIND=4) = 377534000000000000000
 * real (KIND=8) = 377234000000000000000
 * real (KIND=16) is 2**(p-1) = 2**(-94) where p = 95
 * or 376434000000000000000 0000000000000000000000
 * 
 * HEX values for EPSILON are:
 *
 * YMP:  EPSILON (in hex)                    IEEE: EPSILON (in hex)
 * R*4     3FEA800000000000                |         34000000
 * R*8     3FD3800000000000                | 3CB0000000000000
 * R*16    3FA3800000000000,0              | 3F8F000000000000,0
 *
 * SGI HEX for R*16
 * R*16                                    | 3950000000000000
 *                                         | 0000000000000000
 *
 * LE non-sv2 HEX for R*16
 * R*16                                    | 3C00000000000000
 *                                         | 0000000000000000
 */

/* YMP: */
#define EPSILON_REAL4_F90_Y  0.1192092895507812e-06
#define EPSILON_REAL8_F90_Y  0.1421085471520200e-13
#define EPSILON_REAL16_F90_Y 0.50487097934144755546350628178090e-28L
/* IEEE: */
#define EPSILON_REAL4_F90_IEEE 1.192092895507812500E-07 /* Real(KIND=4)= */
							/* 2**-23 */
#define EPSILON_REAL8_F90_IEEE 2.220446049250313081E-16 /* Real(KIND=8)= */
							/* 2**-52 */
#if	defined(_CRAYMPP)
#define EPSILON_REAL16_F90_IEEE 2.220446049250313081E-16 /* Real(KIND=8)= */
#elif   defined(__mips)
#define EPSILON_REAL16_F90_IEEE 1.2325951644078309459558258832543534800E-32L
#elif   defined(_LITTLE_ENDIAN) && !defined(__sv2)
#define EPSILON_REAL16_F90_IEEE 1.08420217248550443400745E-19L
							/* 2**-63 */
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE)) || \
	(defined(_LITTLE_ENDIAN) && defined(__sv2))
							/* 2**-52 */
#define EPSILON_REAL16_F90_IEEE 1.925929944387235853055977942584927319E-34L
                                              /* Real(KIND=16)=2**-112 */
#endif

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define EPSILON_REAL4_F90	EPSILON_REAL4_F90_IEEE
#define EPSILON_REAL8_F90	EPSILON_REAL8_F90_IEEE
#define EPSILON_REAL16_F90	EPSILON_REAL16_F90_IEEE
#else
#define EPSILON_REAL4_F90	EPSILON_REAL4_F90_Y
#define EPSILON_REAL8_F90	EPSILON_REAL8_F90_Y
#define EPSILON_REAL16_F90	EPSILON_REAL16_F90_Y
#endif

/* HUGE for integer, real and double precision in the model. 
 * Integer result = (r**q)-1
 * Real result = b**emax*(1-b**(-p))
 * 
 * HEX values for HUGE:
 *
 * YMP:  HUGE (in hex)                       IEEE: HUGE (in hex)
 * I*1                   7F                |               7F
 * I*2                 7FFF                |             7FFF
 * I*4             7FFFFFFF                |         7FFFFFFF
 * ----------------------------------------  FOR MPP and SPARC, ieee
 * I*6         3FFFFFFFFFFF                |         7FFFFFFF
 * ----------------------------------------  FOR CRAY-TS,IEEE
 * OLD:
 * I*6         3FFFFFFFFFFF                |    3FFFFFFFFFFFF
 * NEW:
 * I*6         3FFFFFFFFFFF                |     3FFFFFFFFFFF
 * -------------------------------------------------------------------
 * I*8     7FFFFFFFFFFFFFFF                | 7FFFFFFFFFFFFFFF
 * R*4     5FFDFFFFFF000000                |         7F7FFFFF
 * R*8     5FFDFFFFFFFFFFFE                | 7FEFFFFFFFFFFFFF
 * R*16    5FFDFFFFFFFFFFFE                | 7FFEFFFFFFFFFFFF
 *             FFFFFFFFFFFF                | FFFFFFFFFFFFFFFF
 *
 * SGI HEX for R*16
 * R*16                                    | 7FE0000000000000
 *                                         | F930000000000000
 * LARGEST:
 * R*16                                    | 7FEFFFFFFFFFFFFF
 *                                         | 7C8FFFFFFFFFFFFF
 */

/* YMP: */
#define HUGE_INT1_F90_Y              127          /* integer (KIND=1) */
#define HUGE_INT2_F90_Y            32767          /* integer (KIND=2) */
#define HUGE_INT4_F90_Y       2147483647          /* integer (KIND=4) */
#define HUGE_INT6_F90_Y   70368744177663          /* integer (KIND=6) */
						  /* 46-bit */
#define HUGE_INT8_F90_Y   9223372036854775807     /* integer (KIND=8) */
#define HUGE_REAL4_F90_Y  0.13634350882572e+2466  /* 0577757777777600000000 */
#define HUGE_REAL8_F90_Y  0.136343516952426e+2466 /* 0577757777777777777776 */
#define HUGE_REAL16_F90_Y 0.1363435169524269911828730305882e+2466L
                                                  /* 0577757777777777777777 */
                                                  /*       7777777777777776 */
/* IEEE: */
#define HUGE_INT1_F90_IEEE                  127  /* Integer (KIND=1) */
#define HUGE_INT2_F90_IEEE                32767  /* Integer (KIND=2) */
#define HUGE_INT4_F90_IEEE           2147483647  /* Integer (KIND=4) */
#ifdef _WORD32
#define HUGE_INT6_F90_IEEE 9223372036854775807LL /* Integer (KIND=6) */
#define HUGE_INT8_F90_IEEE 9223372036854775807LL /* Integer (KIND=8) */
#elif defined (_CRAY1) && defined(_CRAYIEEE)
/* Change HUGE_INT6_F90_IEEE 1125899906842623L for Integer (KIND=6)
 * to allow values to match as 46 bits across PVP architectures.
 */
#define HUGE_INT6_F90_IEEE      70368744177663L /* integer (KIND=6) */
#define HUGE_INT8_F90_IEEE 9223372036854775807L /* Integer (KIND=8) */
#else
#define HUGE_INT6_F90_IEEE 9223372036854775807L /* Integer (KIND=6) */
#define HUGE_INT8_F90_IEEE 9223372036854775807L /* Integer (KIND=8) */
#endif
#define HUGE_REAL4_F90_IEEE   3.4028234663852886e+38   /* Real (KIND=4) */
#define HUGE_REAL8_F90_IEEE   1.7976931348623158e+308  /* Real (KIND=8) */
#if	defined(_CRAYMPP) || (defined(_LITTLE_ENDIAN) && !defined(__sv2))
#define HUGE_REAL16_F90_IEEE  1.7976931348623158e+308  /* Real (KIND=8) */
#elif	defined(__mips)
/* define HUGE_REAL16_F90_IEEE 1.7976931348623158079372897140530230000E308L */
#define HUGE_REAL16_F90_IEEE 8.98846567431157953864652595394506827E307L
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE)) || \
	(defined(_LITTLE_ENDIAN) && defined(__sv2))
#define HUGE_REAL16_F90_IEEE  1.189731495357231765085759326628007016E+4932L
						/* Real (KIND=16) */
#endif

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define HUGE_INT1_F90		HUGE_INT1_F90_IEEE
#define HUGE_INT2_F90		HUGE_INT2_F90_IEEE
#define HUGE_INT4_F90		HUGE_INT4_F90_IEEE
#define HUGE_INT6_F90		HUGE_INT6_F90_IEEE
#define HUGE_INT8_F90		HUGE_INT8_F90_IEEE
#define HUGE_REAL4_F90		HUGE_REAL4_F90_IEEE
#define HUGE_REAL8_F90		HUGE_REAL8_F90_IEEE
#define HUGE_REAL16_F90		HUGE_REAL16_F90_IEEE
#else
#define HUGE_INT1_F90		HUGE_INT1_F90_Y
#define HUGE_INT2_F90		HUGE_INT2_F90_Y
#define HUGE_INT4_F90		HUGE_INT4_F90_Y
#define HUGE_INT6_F90		HUGE_INT6_F90_Y
#define HUGE_INT8_F90		HUGE_INT8_F90_Y
#define HUGE_REAL4_F90		HUGE_REAL4_F90_Y
#define HUGE_REAL8_F90		HUGE_REAL8_F90_Y
#define HUGE_REAL16_F90		HUGE_REAL16_F90_Y
#endif

/* MAXEXPONENT = emax of real and double precision in the model */
/* YMP: */
#define MAXEXPONENT_F90_Y		8189
#define MAXEXPONENT_REAL4_F90_Y		8189
#define MAXEXPONENT_REAL8_F90_Y		8189
#define MAXEXPONENT_REAL16_F90_Y	8189
/* IEEE: */
#define MAXEXPONENT_REAL4_F90_IEEE	 128
#define MAXEXPONENT_REAL8_F90_IEEE	1024
#if	defined(_CRAYMPP) || defined(__mips)
#define MAXEXPONENT_REAL16_F90_IEEE	1023
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE)) || \
	(defined(_LITTLE_ENDIAN) && defined(__sv2))
#define MAXEXPONENT_REAL16_F90_IEEE     16384
#elif defined(_LITTLE_ENDIAN) && !defined(__sv2)
#define MAXEXPONENT_REAL16_F90_IEEE     16383
#endif

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define MAXEXPONENT_REAL4_F90	MAXEXPONENT_REAL4_F90_IEEE
#define MAXEXPONENT_REAL8_F90	MAXEXPONENT_REAL8_F90_IEEE
#define MAXEXPONENT_REAL16_F90	MAXEXPONENT_REAL16_F90_IEEE
#else
#define MAXEXPONENT_F90		MAXEXPONENT_F90_Y
#define MAXEXPONENT_REAL4_F90	MAXEXPONENT_REAL4_F90_Y
#define MAXEXPONENT_REAL8_F90	MAXEXPONENT_REAL8_F90_Y
#define MAXEXPONENT_REAL16_F90	MAXEXPONENT_REAL16_F90_Y
#endif

/* MINEXPONENT = emin of real and double precision in the model.
 * For mips do not use the following unless the denormal in the
 * second word is handled consistently across all hardware.  The
 * option to speed up the real(16) will not cause the second word
 * to become zero.
 *         MINEXPONENT_REAL16_F90_IEEE	 -967
 * For now, use 1022 - 107 = 915.
 */
/* YMP: */
#define MINEXPONENT_F90_Y		-8188
#define MINEXPONENT_REAL4_F90_Y		-8188
#define MINEXPONENT_REAL8_F90_Y		-8188
#define MINEXPONENT_REAL16_F90_Y	-8188
/* IEEE: */
#define MINEXPONENT_REAL4_F90_IEEE	 -125
#define MINEXPONENT_REAL8_F90_IEEE	-1021
#if	defined(_CRAYMPP)
#define MINEXPONENT_REAL16_F90_IEEE	-1021
#elif   defined(__mips)
#define MINEXPONENT_REAL16_F90_IEEE	 -915
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE)) || \
	(defined(_LITTLE_ENDIAN) && defined(__sv2))
#define MINEXPONENT_REAL16_F90_IEEE    -16381
#elif defined(_LITTLE_ENDIAN) && !defined(__sv2)
#define MINEXPONENT_REAL16_F90_IEEE    -16382
#endif 

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define MINEXPONENT_REAL4_F90	MINEXPONENT_REAL4_F90_IEEE
#define MINEXPONENT_REAL8_F90	MINEXPONENT_REAL8_F90_IEEE
#define MINEXPONENT_REAL16_F90	MINEXPONENT_REAL16_F90_IEEE
#else
#define MINEXPONENT_F90		MINEXPONENT_F90_Y
#define MINEXPONENT_REAL4_F90	MINEXPONENT_REAL4_F90_Y
#define MINEXPONENT_REAL8_F90	MINEXPONENT_REAL8_F90_Y
#define MINEXPONENT_REAL16_F90	MINEXPONENT_REAL16_F90_Y
#endif

/* PRECISION for decimal precision of real and double precision in the model.
 * Result=INT((p-1)*LOG10(b))+k.  K is one if b is an integral power of 10,
 *                                otherwise, k=0.
 */
/* YMP: */
#define PRECISION_REAL4_F90_Y   6                   /* real (KIND=4) */
#define PRECISION_REAL8_F90_Y  13                   /* real (KIND=8) */
#define PRECISION_REAL16_F90_Y 28                   /* real (KIND=16) */
/* IEEE: */
#define PRECISION_REAL4_F90_IEEE    6                  /* real (KIND=4) */
#define PRECISION_REAL8_F90_IEEE   15                  /* real (KIND=8) */
#if	defined(_CRAYMPP)
#define PRECISION_REAL16_F90_IEEE  15                  /* real (KIND=16) */
#elif   defined(__mips)
#define PRECISION_REAL16_F90_IEEE  31                /* real (KIND=16) */
#elif   defined(_LITTLE_ENDIAN) && !defined(__sv2)
#define PRECISION_REAL16_F90_IEEE  18                /* real (KIND=16) */
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE)) || \
	(defined(_LITTLE_ENDIAN) && defined(__sv2))
#define PRECISION_REAL16_F90_IEEE  33                /* real (KIND=16) */
#endif

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define PRECISION_REAL4_F90	PRECISION_REAL4_F90_IEEE
#define PRECISION_REAL8_F90	PRECISION_REAL8_F90_IEEE
#define PRECISION_REAL16_F90	PRECISION_REAL16_F90_IEEE
#else
#define PRECISION_REAL4_F90	PRECISION_REAL4_F90_Y
#define PRECISION_REAL8_F90	PRECISION_REAL8_F90_Y
#define PRECISION_REAL16_F90	PRECISION_REAL16_F90_Y
#endif

/* RADIX for integer, real and double precision in the model. */
/* YMP: */
#define RADIX_F90_Y		2
/* IEEE: */
#define RADIX_F90_IEEE		2
#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define RADIX_F90		RADIX_F90_IEEE
#else
#define RADIX_F90		RADIX_F90_Y		
#endif

/* RANGE for integer, real and double precision, complex and double complex
 * in the model
 * Integer result = INT(LOG10 (huge)) where huge is the largest positive
 *                  integer in the  model of the same kind.
 * Real result = INT(MIN(LOG10 (huge),-LOG10(tiny))) where huge and tiny are
 *               the largest and smallest positive numbers in the model of the
 *               same kind.
 * For mips, do not use 291 until the option to turn off the slower
 * handling of denormals is not used.  Until that time the second word
 * of a small number could change and thus not indicative of the correct
 * significance of the small number.  Save the following for the future:
 *  RANGE_REAL16_F90_IEEE  291
 */
/* YMP: */
#define RANGE_INT1_F90_Y         2
#define RANGE_INT2_F90_Y         4
#define RANGE_INT4_F90_Y         9
#define RANGE_INT6_F90_Y        13
#define RANGE_INT8_F90_Y        18
#define RANGE_REAL_F90_Y      2465
#define RANGE_REAL4_F90_Y     2465
#define RANGE_REAL8_F90_Y     2465
#define RANGE_REAL16_F90_Y    2465
/* IEEE: */
#define RANGE_INT1_F90_IEEE      2
#define RANGE_INT2_F90_IEEE      4
#define RANGE_INT4_F90_IEEE      9
#if defined (_CRAY1) && defined(_CRAYIEEE)
/* change RANGE_INT6_F90_IEEE 15 for Integer (KIND=6) to allow
 * values to match as 46 bits across PVP systems
 */
#define RANGE_INT6_F90_IEEE     13
#else
#define RANGE_INT6_F90_IEEE     18
#endif
#define RANGE_INT8_F90_IEEE     18
#define RANGE_REAL4_F90_IEEE    37
#define RANGE_REAL8_F90_IEEE   307
#if	defined(_CRAYMPP) || defined(_LITTLE_ENDIAN)
#define RANGE_REAL16_F90_IEEE  307
#elif   defined(__mips)
#define RANGE_REAL16_F90_IEEE  275
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE)) || \
	(defined(_LITTLE_ENDIAN) && defined(__sv2))
#define RANGE_REAL16_F90_IEEE  4931
#endif

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define RANGE_INT1_F90		RANGE_INT1_F90_IEEE 	/* Integer (KIND=1) */
#define RANGE_INT2_F90		RANGE_INT2_F90_IEEE 	/* Integer (KIND=2) */
#define RANGE_INT4_F90		RANGE_INT4_F90_IEEE 	/* Integer (KIND=4) */
#define RANGE_INT6_F90		RANGE_INT6_F90_IEEE 	/* Integer (KIND=6) */
#define RANGE_INT8_F90		RANGE_INT8_F90_IEEE 	/* Integer (KIND=8) */
#define RANGE_REAL4_F90		RANGE_REAL4_F90_IEEE	/* real (KIND=4)  */
#define RANGE_REAL8_F90		RANGE_REAL8_F90_IEEE	/* real (KIND=8)  */
#define RANGE_REAL16_F90	RANGE_REAL16_F90_IEEE	/* real (KIND=16) */
#else
#define RANGE_INT1_F90		RANGE_INT1_F90_Y      /* Integer (KIND=1) */
#define RANGE_INT2_F90		RANGE_INT2_F90_Y      /* Integer (KIND=2) */
#define RANGE_INT4_F90		RANGE_INT4_F90_Y      /* Integer (KIND=4) */
#define RANGE_INT6_F90		RANGE_INT6_F90_Y      /* Integer (KIND=6) */
#define RANGE_INT8_F90		RANGE_INT8_F90_Y      /* Integer (KIND=8) */
#define RANGE_REAL_F90		RANGE_REAL_F90_Y     /* real (KIND=4)  */
#define RANGE_REAL4_F90		RANGE_REAL4_F90_Y     /* real (KIND=4)  */
#define RANGE_REAL8_F90		RANGE_REAL8_F90_Y     /* real (KIND=8)  */
#define RANGE_REAL16_F90	RANGE_REAL16_F90_Y    /* real (KIND=16) */
#endif

/* TINY for real and double precision is the smallest number in the model.
 * Result is b**(emin-1)
 *
 * HEX values for TINY:
 *
 * YMP:  TINY (in hex)                       IEEE: TINY (in hex)
 * R*4     2004800000000000                |           800000
 * R*8     2004800000000000                |   10000000000000
 * R*16    2004800000000000                |    1000000000000
 *                        0                |                0
 *
 * SGI HEX for R*16
 * R*16                                    |  6B0000000000000
 *                                         | 0000000000000000
 * Tiny for double-double (real(kind=16) for mips must be less than
 * TINY_REAL16_F90_IEEE 4.008336720017945555992216102700324100E-292L
 * since the use of an exponent in the range of -967 could cause the
 * second word to change and thus the entire value would be incorrect.
 * If the denormals in the second word get handled correctly across
 * all sgi platforms, then change TINY to the larger value and:
 *
 * SGI HEX for R*16
 * R*16                                    |  370000000000000
 *                                         | 0000000000000000
 *
 * For now, use 1022 - 107 = 915 as the minexponent.
 *
 */

/* YMP: */
#define TINY_REAL4_F90_Y   0.73344154702194e-2465 /* 0200044000000000000000 */
#define TINY_REAL8_F90_Y   0.73344154702194e-2465 /* 0200044000000000000000 */
#define TINY_REAL16_F90_Y  0.733441547021938866248564956819e-2465L
                                                   /* 0200044000000000000000 */
                                                   /* 0000000000000000000000 */
/* IEEE: */
#define TINY_REAL4_F90_IEEE   1.1754944E-38 /* 2**(-125-1) */
#define TINY_REAL8_F90_IEEE   2.2250738585072014E-308 /* 2**(-1021-1) */
#if	defined(_CRAYMPP) || defined(_LITTLE_ENDIAN)
#define TINY_REAL16_F90_IEEE  2.2250738585072014E-308 /* 2***(-1021-1) */
#elif	defined(__mips)
#define TINY_REAL16_F90_IEEE 1.8051943758648295760692620811737470000E-276L
#elif	defined(_WORD32) || (defined(_CRAY1) && defined(_CRAYIEEE))
#define TINY_REAL16_F90_IEEE 3.362103143112093506262677817321752603E-4932L
                                                        /* 2**-16382 */
#endif

#if defined(_CRAYMPP) || defined(_WORD32) || defined(__mips) || \
	(defined(_CRAY1) && defined(_CRAYIEEE)) || defined(_LITTLE_ENDIAN)
#define TINY_REAL4_F90	TINY_REAL4_F90_IEEE  /* real (KIND=4) */
#define TINY_REAL8_F90	TINY_REAL8_F90_IEEE  /* real (KIND=8) */
#define TINY_REAL16_F90	TINY_REAL16_F90_IEEE /* real (KIND=16) */
#else
#define TINY_REAL4_F90	TINY_REAL4_F90_Y     /* real (KIND=4) */
#define TINY_REAL8_F90	TINY_REAL8_F90_Y     /* real (KIND=8) */
#define TINY_REAL16_F90	TINY_REAL16_F90_Y    /* real (KIND=16) */
#endif
