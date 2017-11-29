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


/* USMID @(#)5.0_pl/headers/fmath.h	5.1	04/29/99 21:22:31 */
/* The following values are used by the F90 compiler for a set of environment
 * intrinsics.  Some of the values are also used by the libf/intrin library
 * to return a value for some of the array intrinsics: DIGITS, EPSILON, HUGE,
 *  MAXEXPONENT, MINEXPONENT, PRECISION, RADIX, RANGE, TINY.
 * 
 * The F90 compiler needs a complete set of each IEEE and non-IEEE values.
 * Please keep the complete set of values for YMP and for IEEE.
 * YMP/C90/etc:
 * Integer*1 =            2147483647 signed
 * Integer*2 =            2147483647 signed
 * Integer*4 =            2147483647 signed
 * Integer*8 =   9223372036854775807 signed
 * IEEE:
 * Integer*1 =            2147483647 signed
 * Integer*2 =            2147483647 signed
 * Integer*4 =            2147483647 signed
 * Integer*8 =   9223372036854775807 signed
 */
/* BIT_SIZE returns the number of bits s for an integer of kind n.
 * An integer object consists of s bits in sequence numbered from right to
 * left from zero to s-1 .
 */
/* YMP: */
#define BITSIZE_INT1_F90_Y      8
#define BITSIZE_INT2_F90_Y     16
#define BITSIZE_INT4_F90_Y     32
#define BITSIZE_INT8_F90_Y     64
/* IEEE: */
#define BITSIZE_INT1_F90_IEEE     8
#define BITSIZE_INT2_F90_IEEE    16
#define BITSIZE_INT4_F90_IEEE    32
#define BITSIZE_INT8_F90_IEEE    64

#define BITSIZE_INT1_F90	(target_ieee ?                                 \
				BITSIZE_INT1_F90_IEEE : BITSIZE_INT1_F90_Y)
#define BITSIZE_INT2_F90	(target_ieee ?                                 \
				BITSIZE_INT2_F90_IEEE : BITSIZE_INT2_F90_Y)
#define BITSIZE_INT4_F90	(target_ieee ?                                 \
				BITSIZE_INT4_F90_IEEE : BITSIZE_INT4_F90_Y)
#define BITSIZE_INT8_F90	(target_ieee ?                                 \
				BITSIZE_INT8_F90_IEEE : BITSIZE_INT8_F90_Y)

/* DIGITS for integer, real, and double precision is number of significant
 * digits in bits in the model.
 */
/* YMP: */
#define DIGITS_INT1_F90_Y      7   /* Integer (KIND=1) */
#define DIGITS_INT2_F90_Y     15   /* Integer (KIND=2) */
#define DIGITS_INT4_F90_Y     31   /* Integer (KIND=4) */
#define DIGITS_INT8_F90_Y     63   /* Integer (KIND=8) */
#define DIGITS_REAL4_F90_Y    24   /* Real (KIND=4)    */
#define DIGITS_REAL8_F90_Y    47   /* Real (KIND=8)    */
#define DIGITS_REAL16_F90_Y   95   /* Real (KIND=16)   */
/* IEEE: */
#define DIGITS_INT1_F90_IEEE      7   /* Integer (KIND=1) */
#define DIGITS_INT2_F90_IEEE     15   /* Integer (KIND=2) */
#define DIGITS_INT4_F90_IEEE     31   /* Integer (KIND=4) */
#define DIGITS_INT8_F90_IEEE     63   /* Integer (KIND=8) */
#define DIGITS_REAL4_F90_IEEE    24   /* Real (KIND=4)    */
#define DIGITS_REAL8_F90_IEEE    53   /* Real (KIND=8)    */
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define DIGITS_REAL16_F90_IEEE   107  /* Real (KIND=16)   */
#else 
#define DIGITS_REAL16_F90_IEEE   113  /* Real (KIND=16)   */
#endif

#define DIGITS_INT1_F90		(target_ieee ?                                 \
				DIGITS_INT1_F90_IEEE : DIGITS_INT1_F90_Y)
#define DIGITS_INT2_F90		(target_ieee ?                                 \
				DIGITS_INT2_F90_IEEE : DIGITS_INT2_F90_Y)
#define DIGITS_INT4_F90		(target_ieee ?                                 \
				DIGITS_INT4_F90_IEEE : DIGITS_INT4_F90_Y)
#define DIGITS_INT8_F90		(target_ieee ?                                 \
				DIGITS_INT8_F90_IEEE : DIGITS_INT8_F90_Y)
#define DIGITS_REAL4_F90	(target_ieee ?                                 \
				DIGITS_REAL4_F90_IEEE : DIGITS_REAL4_F90_Y)
#define DIGITS_REAL8_F90	(target_ieee ?                                 \
				DIGITS_REAL8_F90_IEEE : DIGITS_REAL8_F90_Y)
#define DIGITS_REAL16_F90	(target_ieee ?                                 \
				DIGITS_REAL16_F90_IEEE : DIGITS_REAL16_F90_Y)

/* EPSILON for real and double precision
 * number that is almost negligible compared to one in the model
 * Real result = (b)**(1-p) = 2**(-47) where p = 48
 * real (KIND=8) = 377224000000000000000
 * real (KIND=16) is 2**(p-1) = 2**(-95) where p = 96
 * or 376424000000000000000 0000000000000000000000
 */
/* YMP: */
#define EPSILON_REAL4_F90_Y  "0.1192092895507812E-06"
#define EPSILON_REAL8_F90_Y  "0.1421085471520200E-13"
#define EPSILON_REAL16_F90_Y "0.50487097934144755546350628178090E-28"
/* IEEE: */
#define EPSILON_REAL4_F90_IEEE  "1.192092895507812500E-07"
#define EPSILON_REAL8_F90_IEEE  "2.220446049250313081E-16"
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define EPSILON_REAL16_F90_IEEE "1.23259516440783094595582588325435348E-32"
#else
#define EPSILON_REAL16_F90_IEEE "1.925929944387235853055977942584927319E-34"
#endif

#define EPSILON_REAL4_F90	(target_ieee ?                                 \
				EPSILON_REAL4_F90_IEEE : EPSILON_REAL4_F90_Y)
#define EPSILON_REAL8_F90	(target_ieee ?                                 \
				EPSILON_REAL8_F90_IEEE : EPSILON_REAL8_F90_Y)
#ifdef _TARGET_OS_MAX
#define EPSILON_REAL16_F90	(target_ieee ?                                 \
				EPSILON_REAL8_F90_IEEE : EPSILON_REAL16_F90_Y)
#else
#define EPSILON_REAL16_F90	(target_ieee ?                                 \
				EPSILON_REAL16_F90_IEEE : EPSILON_REAL16_F90_Y)
#endif

/* BHJ JBL All HUGE values are stored here as strings. They must be converted */
/* to constants using the routine cvrt_str_to_cn in lex.c. This is because of */
/* cross compiler problems with the use of these as "c" constants.            */

/* HUGE for integer, real and double precision in the model. 
 * Integer result = (r**q)-1
 * Real result = b**emax*(1-b**(-p))
 */
/* YMP: */
#define HUGE_INT1_F90_Y              "127"
#define HUGE_INT2_F90_Y            "32767"
#define HUGE_INT4_F90_Y       "2147483647"
#define HUGE_INT8_F90_Y   "9223372036854775807"
#define HUGE_REAL4_F90_Y  "0.13634350882572e+2466"
#define HUGE_REAL8_F90_Y  "0.136343516952426e+2466"
#define HUGE_REAL16_F90_Y "0.1363435169524269911828730305882e+2466"
/* IEEE: */
#define HUGE_INT1_F90_IEEE                  "127"
#define HUGE_INT2_F90_IEEE                "32767"
#define HUGE_INT4_F90_IEEE           "2147483647"
#define HUGE_INT8_F90_IEEE  "9223372036854775807"
#define HUGE_REAL4_F90_IEEE   "3.4028234663852886E+38"
#define HUGE_REAL8_F90_IEEE   "1.7976931348623158E+308"
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define HUGE_REAL16_F90_IEEE  "8.98846567431157953864652595394506827E+307"
#else
#define HUGE_REAL16_F90_IEEE  "1.189731495357231765085759326628007016E+4932"
#endif

#define HUGE_INT1_F90		(target_ieee ?                                 \
				HUGE_INT1_F90_IEEE : HUGE_INT1_F90_Y)
#define HUGE_INT2_F90		(target_ieee ?                                 \
				HUGE_INT2_F90_IEEE : HUGE_INT2_F90_Y)
#define HUGE_INT4_F90		(target_ieee ?                                 \
				HUGE_INT4_F90_IEEE : HUGE_INT4_F90_Y)
#define HUGE_INT8_F90		(target_ieee ?                                 \
				HUGE_INT8_F90_IEEE : HUGE_INT8_F90_Y)
#define HUGE_REAL4_F90		(target_ieee ?                                 \
				HUGE_REAL4_F90_IEEE : HUGE_REAL4_F90_Y)
#define HUGE_REAL8_F90		(target_ieee ?                                 \
				HUGE_REAL8_F90_IEEE : HUGE_REAL8_F90_Y)
#define HUGE_REAL16_F90		(target_ieee ?                                 \
				HUGE_REAL16_F90_IEEE : HUGE_REAL16_F90_Y)

/* MAXEXPONENT = emax of real and double precision in the model */
/* YMP: */
#define MAXEXPONENT_REAL4_F90_Y		8189
#define MAXEXPONENT_REAL8_F90_Y		8189
#define MAXEXPONENT_REAL16_F90_Y	8189
/* IEEE: */
#define MAXEXPONENT_REAL4_F90_IEEE	 128
#define MAXEXPONENT_REAL8_F90_IEEE	1024
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define MAXEXPONENT_REAL16_F90_IEEE     1023
#else
#define MAXEXPONENT_REAL16_F90_IEEE    16384
#endif

#define MAXEXPONENT_REAL4_F90	(target_ieee ?                                 \
			MAXEXPONENT_REAL4_F90_IEEE : MAXEXPONENT_REAL4_F90_Y)
#define MAXEXPONENT_REAL8_F90	(target_ieee ?                                 \
			MAXEXPONENT_REAL8_F90_IEEE : MAXEXPONENT_REAL8_F90_Y)
#define MAXEXPONENT_REAL16_F90	(target_ieee ?                                 \
			MAXEXPONENT_REAL16_F90_IEEE : MAXEXPONENT_REAL16_F90_Y)

/* MINEXPONENT = emin of real and double precision in the model */
/* YMP: */
#define MINEXPONENT_REAL4_F90_Y		-8188
#define MINEXPONENT_REAL8_F90_Y		-8188
#define MINEXPONENT_REAL16_F90_Y	-8188
/* IEEE: */
#define MINEXPONENT_REAL4_F90_IEEE	 -125
#define MINEXPONENT_REAL8_F90_IEEE	-1021
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define MINEXPONENT_REAL16_F90_IEEE      -915
#else
#define MINEXPONENT_REAL16_F90_IEEE    -16381
#endif

#define MINEXPONENT_REAL4_F90	(target_ieee ?                                 \
			MINEXPONENT_REAL4_F90_IEEE : MINEXPONENT_REAL4_F90_Y)
#define MINEXPONENT_REAL8_F90	(target_ieee ?                                 \
			MINEXPONENT_REAL8_F90_IEEE : MINEXPONENT_REAL8_F90_Y)
#define MINEXPONENT_REAL16_F90	(target_ieee ?                                 \
			MINEXPONENT_REAL16_F90_IEEE : MINEXPONENT_REAL16_F90_Y)

/* PRECISION for decimal precision of real and double precision in the model.
 * Result=INT((p-1)*LOG10(b))+k.  K is one if b is an integral power of 10,
 *                                otherwise, k=0.
 */
/* YMP: */
#define PRECISION_REAL4_F90_Y   6 
#define PRECISION_REAL8_F90_Y  13 
#define PRECISION_REAL16_F90_Y 28 
/* IEEE: */
#define PRECISION_REAL4_F90_IEEE    6  
#define PRECISION_REAL8_F90_IEEE   15  
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define PRECISION_REAL16_F90_IEEE  31  
#else
#define PRECISION_REAL16_F90_IEEE  33  
#endif

#define PRECISION_REAL4_F90	(target_ieee ?                                \
			PRECISION_REAL4_F90_IEEE : PRECISION_REAL4_F90_Y)
#define PRECISION_REAL8_F90	(target_ieee ?                                \
			PRECISION_REAL8_F90_IEEE : PRECISION_REAL8_F90_Y)
#define PRECISION_REAL16_F90	(target_ieee ?                                \
			PRECISION_REAL16_F90_IEEE : PRECISION_REAL16_F90_Y)

/* RADIX for integer, real and double precision in the model. */
/* YMP: */
#define RADIX_F90_Y		2
/* IEEE: */
#define RADIX_F90_IEEE		2
#define RADIX_F90		(target_ieee ?                                 \
				RADIX_F90_IEEE : RADIX_F90_Y)

/* RANGE for integer, real and double precision, complex and double complex
 * in the model
 * Integer result = INT(LOG10 (huge)) where huge is the largest positive
 *                  integer in the  model of the same kind.
 * Real result = INT(MIN(LOG10 (huge),-LOG10(tiny))) where huge and tiny are
 *               the largest and smallest positive numbers in the model of the
 *               same kind.
 */
/* YMP: */
#define RANGE_INT1_F90_Y         2
#define RANGE_INT2_F90_Y         4
#define RANGE_INT4_F90_Y         9
#define RANGE_INT8_F90_Y        18
#define RANGE_REAL4_F90_Y     2465
#define RANGE_REAL8_F90_Y     2465
#define RANGE_REAL16_F90_Y    2465
/* IEEE: */
#define RANGE_INT1_F90_IEEE      2
#define RANGE_INT2_F90_IEEE      4
#define RANGE_INT4_F90_IEEE      9
#define RANGE_INT8_F90_IEEE     18
#define RANGE_REAL4_F90_IEEE    37
#define RANGE_REAL8_F90_IEEE   307
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define RANGE_REAL16_F90_IEEE  275
#else
#define RANGE_REAL16_F90_IEEE 4931
#endif

#define RANGE_INT1_F90		(target_ieee ?                                 \
			RANGE_INT1_F90_IEEE : RANGE_INT1_F90_Y)
#define RANGE_INT2_F90		(target_ieee ?                                 \
			RANGE_INT2_F90_IEEE : RANGE_INT2_F90_Y)
#define RANGE_INT4_F90		(target_ieee ?                                 \
			RANGE_INT4_F90_IEEE : RANGE_INT4_F90_Y)
#define RANGE_INT8_F90		(target_ieee ?                                 \
			RANGE_INT8_F90_IEEE : RANGE_INT8_F90_Y)
#define RANGE_REAL4_F90		(target_ieee ?                                 \
			RANGE_REAL4_F90_IEEE : RANGE_REAL4_F90_Y)
#define RANGE_REAL8_F90		(target_ieee ?                                 \
			RANGE_REAL8_F90_IEEE : RANGE_REAL8_F90_Y)
#define RANGE_REAL16_F90	(target_ieee ?                                 \
			RANGE_REAL16_F90_IEEE : RANGE_REAL16_F90_Y)

/* BHJ JBL All TINY values are stored here as strings. They must be converted */
/* to constants using the routine cvrt_str_to_cn in lex.c. This is because of */
/* cross compiler problems with the use of these as "c" constants.            */

/* TINY for real and double precision is the smallest number in the model.
 * Result is b**(emin-1)
 */
/* YMP: */
#if defined(_HOST_OS_UNICOS)
#define TINY_REAL4_F90_Y  "0.73344154702194e-2465"
#define TINY_REAL8_F90_Y  "0.73344154702194e-2465"
#define TINY_REAL16_F90_Y "0.733441547021938866248564956819e-2465"
#else
#define TINY_REAL4_F90_Y  "0.0"
#define TINY_REAL8_F90_Y  "0.0"
#define TINY_REAL16_F90_Y "0.0"
#endif
/* IEEE: */
#define TINY_REAL4_F90_IEEE   "1.1754944E-38"
#define TINY_REAL8_F90_IEEE   "2.2250738585072014E-308"

/* until REAL16 is ready on Cray ieee, this must be the same as REAL8 */
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#define TINY_REAL16_F90_IEEE  "0.1805194375864829576069262081173747E-275"
#else
#define TINY_REAL16_F90_IEEE  "3.362103143112093506262677817321752603E-4932"
#endif

#define TINY_REAL4_F90	(target_ieee ?                                         \
			TINY_REAL4_F90_IEEE : TINY_REAL4_F90_Y)
#define TINY_REAL8_F90	(target_ieee ?                                         \
			TINY_REAL8_F90_IEEE : TINY_REAL8_F90_Y)
#define TINY_REAL16_F90	(target_ieee ?                                         \
			TINY_REAL16_F90_IEEE : TINY_REAL16_F90_Y)
