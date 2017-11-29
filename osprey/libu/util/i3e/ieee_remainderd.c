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


#pragma ident "@(#) libu/util/i3e/ieee_remainderd.c	92.1	07/07/99 14:37:09"

#include <fp.h>
#include <stdio.h>

#ifndef _LD64

extern long double __remainder_d(long double xarg, long double yarg);

/* use 55 bits for the first half and 57 bits for second half */
#define IE128_64_MANT_BTS_P1	48
#define IE128_64_MANT_BTS_P2	 7
#define IE128_64_MANT_BTS_P3	57

#define CALC_DINT()					\
	tmpb = tmptdiva.ldword + two_112.ldword;	\
	tmpb -= two_112.ldword;				\
	if (tmpb > tmptdiva.ldword)			\
		tmpb -= (long double) 1.0;		\
	tmptdiva.ldword = tmpb;

/*
 * Use the following algorithm for x/y rounded quantity:
 *   1.  x/y
 *   2. if(fraction(x/y) = .5 exactly, round to next EVEN number.
 *           0.5 = 0.0, 1.5 + 2.0, 10.5 = 10, etc.
 * Use the following algorithm for the multiply:
 *   ((((x - tdivU *yU) - tdivU * yL) -tdivL * yU) - tdivL * yL)
 */

long double
__remainder_d(long double xarg,
		long double yarg)
{
	union _ieee_ldouble {
		struct { unsigned int sign	: 1;
			 unsigned int exponent	: IEEE_128_EXPO_BITS;
			 unsigned int mantissa_1 : IE128_64_MANT_BTS_P1;
			 unsigned int mantissa_2 : IE128_64_MANT_BTS_P2;
			 unsigned int mantissa_3 : IE128_64_MANT_BTS_P3;
		} parts1;
		long double		ldword;
		unsigned long 		lword[2];
		long			int64[2];
	};
	static union _ieee_ldouble two_112 =
	    {0, 0x406F, 0x0, 0x0, 0x0};
	static union _ieee_ldouble div_112 =
	    {0, 0x3F8F, 0x0, 0x0, 0x0};
	long double	scalet, scaley;
	unsigned long	sign_x = 0X8000000000000000;
	unsigned long	even_x = 0X0000000000000001;
	union _ieee_ldouble x_val, y_val, tdiv;
	union _ieee_ldouble y_up, y_lo, tdiv_up, tdiv_lo, res;

	tdiv.ldword = xarg / yarg;
	x_val.ldword = xarg;
	y_val.ldword = yarg;

	/* check for 2**112 or greater = already integer */
	if ((long) (tdiv.lword[0] & (~sign_x)) < two_112.int64[0]) {
		union _ieee_ldouble tmptdiva, tmptdivb;
		union _ieee_ldouble nearint, evenchk;
		unsigned long	two_64 = 0X403F000000000000;
		long double	tmpb, tmpc;

		nearint.ldword	= 0.0;
		if (((long) tdiv.lword[0] & (~sign_x)) < (long) two_64) {
			/* calculate fraction */
			evenchk.ldword = tdiv.ldword -
				(long double)((long)tdiv.ldword);

			if (tdiv.ldword < 0.0) {
				nearint.int64[1] = (long) (tdiv.ldword - 0.5);
				if ((evenchk.ldword == -0.5) &&
				    ((long) (nearint.lword[1] & even_x) != 0))
					nearint.int64[1] += 1;
			} else {
				nearint.int64[1] = (long) (tdiv.ldword + 0.5);
				if ((evenchk.ldword == 0.5) &&
				   ((long) (nearint.lword[1] & even_x) != 0))
					nearint.int64[1] -= 1;
			}
			tdiv.ldword = (long double) nearint.int64[1];
		} else {
			/* These are values between 2**64 and 2**112.
			 * Calculate DINT(x/y) and use result in
			 * (x/y) - DINT(x/y) to find fraction.
			 */
			tmptdiva.ldword = tdiv.ldword;
			tmptdiva.lword[0] &= (~sign_x);
			CALC_DINT();
			tmptdiva.parts1.sign = tdiv.parts1.sign;

			/* calculate fraction */
			evenchk.ldword = tdiv.ldword - tmptdiva.ldword;

			if (tdiv.ldword < 0.0) {
				/* round to nearest integer */
				tmptdivb.ldword = tdiv.ldword - 0.5;

				/* truncate to a nearest whole number */
				tmptdiva.ldword = tmptdivb.ldword;
				tmptdiva.lword[0] &= (~sign_x);
				CALC_DINT();
				tmptdiva.parts1.sign =
						tmptdivb.parts1.sign;
				tmpc = tmptdiva.ldword;

				/* create DINT(0.5*x) for even number
				 * check using 2*DINT(0.5*x)
				 */
				tmptdiva.ldword = tmpc * 0.5;
				CALC_DINT();
				tmptdiva.parts1.sign =
						tmptdivb.parts1.sign;

				/* check for nearest even whole number */
				if ((evenchk.ldword == -0.5) &&
				    (tmpc != 2.0 * tmptdiva.ldword))
					tmpc += 1.0;
			} else {
				/* round to nearest integer */
				tmptdivb.ldword = tdiv.ldword + 0.5;

				/* truncate to a nearest whole number */
				tmptdiva.ldword = tmptdivb.ldword;
				tmptdiva.lword[0] &= (~sign_x);
				CALC_DINT();
				tmptdiva.parts1.sign =
						tmptdivb.parts1.sign;
				tmpc = tmptdiva.ldword;

				/* create DINT(0.5*x) for even number
				 * check using 2*DINT(0.5*x)
				 */
				tmptdiva.ldword = tmpc * 0.5;
				CALC_DINT();
				tmptdiva.parts1.sign =
						tmptdivb.parts1.sign;

				/* check for nearest even whole number */
				if ((evenchk.ldword == 0.5) &&
				    (tmpc != 2.0 * tmptdiva.ldword))
					tmpc -= 1.0;
			}
			tdiv.ldword = tmpc;
		}
	}

	/* Calculate upper and lower for y and tdiv. */
	y_up.ldword = y_val.ldword;
	tdiv_up.ldword = tdiv.ldword;
	y_up.parts1.mantissa_3 = 0x0;
	tdiv_up.parts1.mantissa_3 = 0x0;
	scalet = 1.0;
	scaley = 1.0;

	/* If tdiv_up exponent < 57, scale up to prevent underflow. */
	if ((long) tdiv.parts1.exponent < 57) {
		tdiv_lo.ldword = (tdiv.ldword * two_112.ldword) -
			(tdiv_up.ldword * two_112.ldword);
		scalet = div_112.ldword;
	} else
		tdiv_lo.ldword = tdiv.ldword - tdiv_up.ldword;

	/* If y_up exponent < 57, scale up to prevent underflow. */
	if ((long) y_val.parts1.exponent < 57) {
		y_lo.ldword = (y_val.ldword * two_112.ldword) -
			(y_val.ldword * two_112.ldword);
		scaley = div_112.ldword;
	} else
		y_lo.ldword = y_val.ldword - y_up.ldword;

	/* algorithm for ieee for x - (x/y)*y. */
	res.ldword = ((((x_val.ldword - (tdiv_up.ldword * y_up.ldword)) -
			(scaley * (tdiv_up.ldword * y_lo.ldword))) -
			(scalet * (tdiv_lo.ldword * y_up.ldword))) -
		(scalet * scaley * (tdiv_lo.ldword * y_lo.ldword)));

	if (res.ldword == 0.0)
		res.parts1.sign = x_val.parts1.sign;

	return(res.ldword);
}
#endif         /* end NOT _LD64 */
