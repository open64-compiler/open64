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


#include "arith.internal.h"

static AR_IEEE_64 P0 = { 0, 0x400, 0x3, 0xc9b0, 0x5d7f, 0xa527 };
static AR_IEEE_64 P1 = { 1, 0x400, 0x9, 0x6989, 0x055f, 0x3e6c };
static AR_IEEE_64 P2 = { 0, 0x400, 0x4, 0xbaa5, 0x10fa, 0x5f82 };
static AR_IEEE_64 P3 = { 1, 0x3ff, 0x0, 0xeed8, 0x47a3, 0x911c };
static AR_IEEE_64 P4 = { 0, 0x3fc, 0x5, 0x8567, 0x3a3b, 0xeba9 };

static AR_IEEE_64 HLF = { 0, 0x3fe, 0, 0, 0, 0 };
static AR_IEEE_64 ONE = { 0, 0x3ff, 0, 0, 0, 0 };

int
ar_isqrt64 (AR_IEEE_64 *a,
			const AR_IEEE_64 *x,
			int roundmode) {

	int res = AR_STAT_OK;
	int expo;
	AR_TYPE int64  = AR_Int_64_S;
	AR_IEEE_64 t, u, y;
 
 	*a = *x;

	/* Test for NaNs and Infs */
	if (x->expo > AR_IEEE64_MAX_EXPO) {
		/* For NaNs and Infs, return same */
		if (!IS_IEEE64_NZ_COEFF(x)) {
			/* infinity */
			if (x->sign)
				res |= AR_STAT_NEGATIVE;
		}
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for zero */
	if (x->expo == 0 && !IS_IEEE64_NZ_COEFF(x)) {
		if (x->sign)
			res |= AR_STAT_NEGATIVE;
		return res | AR_STAT_ZERO;
	}

	/* Test for negative */
	if (x->sign) {
		/* negatives yield quiet NaNs */
		a->expo = AR_IEEE64_MAX_EXPO + 1;
		if (HOST_IS_MIPS) {
			a->coeff0 &= MASKR (AR_IEEE64_C0_BITS - 1);
			if (!IS_IEEE64_NZ_COEFF(a)) {
				a->coeff0 |= 1;
			}
		}
		else {
			a->coeff0 |= (1 << (AR_IEEE64_C0_BITS - 1));
		}
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for denorms (they have zero exponents) */
	if (ar_state_register.ar_denorms_trap &&
		!x->expo && IS_IEEE64_NZ_COEFF(a)) {
		/* operand is a denorm and denorms cause a trap */
		a->expo = AR_IEEE64_MAX_EXPO + 1;
		return res | AR_STAT_UNDEFINED;
	}

	/*  Minimax approx for 1/sqrt(x), on the interval u = (0.5,2.0), 
	    with maximum error = -2.11772E-03 */

	u = *x;

	/* scale value into [0.5,2.0) */
	expo = u.expo & 0x7fe;
	u.expo = (u.expo ^ expo) | 0x3fe;

	/* Approximate 1/sqrt(u) = P0 + u*(P1 + u*(P2 + u*(P3 + u*P4))) */
	ar_ifmul64(&y, &u, (AR_IEEE_64*)&P4, roundmode);
	ar_ifadd64(&y, (AR_IEEE_64*)&P3, &y, roundmode);
	ar_ifmul64(&y, &u,  &y, roundmode);
	ar_ifadd64(&y, (AR_IEEE_64*)&P2, &y, roundmode);
	ar_ifmul64(&y, &u,  &y, roundmode);
	ar_ifadd64(&y, (AR_IEEE_64*)&P1, &y, roundmode);
	ar_ifmul64(&y, &u,  &y, roundmode);
	ar_ifadd64(&u, (AR_IEEE_64*)&P0, &y, roundmode);

	/* scale by 2**(-N) */
	u.expo -= ((expo - 0x3fe)>>1);
	u.sign = 0;

	/* 2 Newton iterations:  u += 0.5*u*(1.0 - u*(u*x)) */
	ar_ifmul64(&y,   &u,  x, roundmode);
	ar_ifmul64(&y,   &u, &y, roundmode);
	ar_ifsub64(&y, (AR_IEEE_64*)&ONE, &y, roundmode);
	ar_ifmul64(&y,   &u, &y, roundmode);
	ar_ifmul64(&y, (AR_IEEE_64*)&HLF, &y, roundmode);
	ar_ifadd64(&u,   &u, &y, roundmode);

	ar_ifmul64(&y,   &u,  x, roundmode);
	ar_ifmul64(&y,   &u, &y, roundmode);
	ar_ifsub64(&y, (AR_IEEE_64*)&ONE, &y, roundmode);
	ar_ifmul64(&y,   &u, &y, roundmode);
	ar_ifmul64(&y, (AR_IEEE_64*)&HLF, &y, roundmode);
	ar_ifadd64(&u,   &u, &y, roundmode);

	/* 1 more Newton iteration:  u = u*x + 0.5*u*x*(1.0 - u*(u*x)) */
	ar_ifmul64(&y,   &u,  x, roundmode);
	ar_ifmul64(&u,   &u, &y, roundmode);
	ar_ifsub64(&u, (AR_IEEE_64*)&ONE, &u, roundmode);
	ar_ifmul64(&u,   &y, &u, roundmode);
	ar_ifmul64(&u, (AR_IEEE_64*)&HLF, &u, roundmode);
	ar_ifadd64(&u,   &y, &u, roundmode);

	/* use the Tuckerman test to get the last bit */

	ar_add_integer((ar_data*)&y, &int64,
				   (ar_data*)&u, &int64, (ar_data*)&AR_const_one, &int64);
	ar_ifmul64(&t, &u, &y, AR_ROUND_ZERO);
	if(ar_ifcmp64(&t, x) & AR_STAT_NEGATIVE)
		u = y;

	ar_subtract_integer((ar_data*)&y, &int64, 0,
				   (ar_data*)&u, &int64, (ar_data*)&AR_const_one, &int64);
	ar_ifmul64(&t, &u, &y, AR_ROUND_ZERO);
	if(ar_ifcmp64(&t, x) & AR_STAT_NEGATIVE)
		*a = u;
	else
		*a = y;

  	return AR_STAT_OK;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: ieee_sqrt.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
