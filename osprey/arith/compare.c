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


/*
 *	Comparison routines
 *
 *	The result of a comparison is to be taken from the status bits
 *	AR_STAT_ZERO and AR_STAT_NEGATIVE, along with the AR_STAT_OVERFLOW
 *      error bit and (for IEEE) the AR_STAT_UNDEFINED bit.
 */

#include "arith.internal.h"


/* Low-level interface */
static int
ar_cfcmp64 (const AR_CRAY_64 *a, const AR_CRAY_64 *b, int roundmode) {

	AR_CRAY_64 diff;

	/* Perform comparison by subtraction; a == b implies (a-b) == 0 */
	return ar_cfsub64 (&diff, a, b);
}


static int
ar_cfcmp128 (const AR_CRAY_128 *a, const AR_CRAY_128 *b) {
	AR_CRAY_128 diff;
	return ar_cfsub128 (&diff, a, b);
}


static int
ar_ifcmp32 (const AR_IEEE_32 *a, const AR_IEEE_32 *b) {

	if (IS_IEEE32_NaN(a) || IS_IEEE32_NaN(b)) {
		/* one of the operands is a NaN - result is unordered */
		return AR_STAT_UNDEFINED;
	}

	/* Test for [+-]0 == [+-]0 */

	if (a->expo == 0 && !IS_IEEE32_NZ_COEFF(a) &&
		b->expo == 0 && !IS_IEEE32_NZ_COEFF(b))
		return AR_STAT_ZERO;

	/* Remaining tests use -Infinity < -finite < +finite < +Infinity */

	if (a->sign ^ b->sign) {
		/* Signs differ; therefore a<b or a>b */
		return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Same signs--test exponents next */

	if (a->expo ^ b->expo) {
		/* Exponents differ; therefore a<b or a>b */
		if(a->expo < b->expo)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Same sign and exponent--test coefficients */

	if (a->coeff0 ^ b->coeff0) {
		if(a->coeff0 < b->coeff0)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff1 ^ b->coeff1) {
		if(a->coeff1 < b->coeff1)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Values are equal */

	return AR_STAT_ZERO;
}


int
ar_ifcmp64 (const AR_IEEE_64 *a, const AR_IEEE_64 *b) {

	if (IS_IEEE64_NaN(a) || IS_IEEE64_NaN(b)) {
		/* one of the operands is a NaN - result is unordered */
		return AR_STAT_UNDEFINED;
	}

	/* Test for [+-]0 == [+-]0 */

	if (a->expo == 0 && !IS_IEEE64_NZ_COEFF(a) &&
		b->expo == 0 && !IS_IEEE64_NZ_COEFF(b))
		return AR_STAT_ZERO;

	/* Remaining tests use -Infinity < -finite < +finite < +Infinity */

	if (a->sign ^ b->sign) {
		/* Signs differ; therefore a<b or a>b */
		return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Same signs--test exponents next */

	if (a->expo ^ b->expo) {
		/* Exponents differ; therefore a<b or a>b */
		if(a->expo < b->expo)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Same sign and exponent--test coefficients */

	if (a->coeff0 ^ b->coeff0) {
		if(a->coeff0 < b->coeff0)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff1 ^ b->coeff1) {
		if(a->coeff1 < b->coeff1)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff2 ^ b->coeff2) {
		if(a->coeff2 < b->coeff2)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff3 ^ b->coeff3) {
		if(a->coeff3 < b->coeff3)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Values are equal */

	return AR_STAT_ZERO;
}


#ifdef __mips
static int
ar_ifcmp128 (const AR_IEEE_128 *a, const AR_IEEE_128 *b)
{
   long double o1,o2;
   o1 = *(long double *) a;
   o2 = *(long double *) b;

   if (IS_MIPS128_NaN((AR_MIPS_128 *)a) || IS_MIPS128_NaN((AR_MIPS_128 *)b)) {
      /* one of the operands is a NaN - result is unordered */
      return AR_STAT_UNDEFINED;
   }
   
   if (o1 == o2) {
      return AR_STAT_ZERO;
   } else if (o1 < o2) {
      return AR_STAT_NEGATIVE;
   } else {
      return AR_STAT_OK;
   }
}

#else

static int
ar_ifcmp128 (const AR_IEEE_128 *a, const AR_IEEE_128 *b) {

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		long double o1,o2;
		o1 = *(long double *) a;
		o2 = *(long double *) b;

		if (IS_MIPS128_NaN((AR_MIPS_128 *)a) ||
		    IS_MIPS128_NaN((AR_MIPS_128 *)b)) {
			/*
			 * One of the operands is a NaN - result
			 * is unordered.
			 */
			return AR_STAT_UNDEFINED;
		}

		if (o1 == o2) {
			return AR_STAT_ZERO;
		} else if (o1 < o2) {
			return AR_STAT_NEGATIVE;
		} else {
			return AR_STAT_OK;
		}
	}

	if (IS_IEEE128_NaN(a) || IS_IEEE128_NaN(b)) {
		/* one of the operands is a NaN - result is unordered */
		return AR_STAT_UNDEFINED;
	}

	/* Test for [+-]0 == [+-]0 */

	if (a->expo == 0 && !IS_IEEE128_NZ_COEFF(a) &&
		b->expo == 0 && !IS_IEEE128_NZ_COEFF(b))
		return AR_STAT_ZERO;

	/* Remaining tests use -Infinity < -finite < +finite < +Infinity */

	if (a->sign ^ b->sign) {
		/* Signs differ; therefore a<b or a>b */
		return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Same signs--test exponents next */

	if (a->expo ^ b->expo) {
		/* Exponents differ; therefore a<b or a>b */
		if(a->expo < b->expo)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Same sign and exponent--test coefficients */

	if (a->coeff0 ^ b->coeff0) {
		if(a->coeff0 < b->coeff0)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff1 ^ b->coeff1) {
		if(a->coeff1 < b->coeff1)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff2 ^ b->coeff2) {
		if(a->coeff2 < b->coeff2)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff3 ^ b->coeff3) {
		if(a->coeff3 < b->coeff3)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff4 ^ b->coeff4) {
		if(a->coeff4 < b->coeff4)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff5 ^ b->coeff5) {
		if(a->coeff5 < b->coeff5)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}
	if (a->coeff6 ^ b->coeff6) {
		if(a->coeff6 < b->coeff6)
			return a->sign?AR_STAT_OK:AR_STAT_NEGATIVE;
		else
			return a->sign?AR_STAT_NEGATIVE:AR_STAT_OK;
	}

	/* Values are equal */

	return AR_STAT_ZERO;
}
#endif

/* Integer comparison */
static
AR_COMPARE_TYPE
ar_compare_integer (const ar_data *opnd1, const AR_TYPE *opnd1type,
		    const ar_data *opnd2, const AR_TYPE *opnd2type) {

	ar_data temp;
	int subtractflags;

	ar_subtract_integer (&temp, opnd1type, &subtractflags,
			     opnd1, opnd1type, opnd2, opnd2type);

	/* subtractflags == SZVC bits */

	/* Signed comparisons */
	if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED) {
		if ((subtractflags ^ (subtractflags >> 2)) & 0x2)
			return AR_Compare_LT;	/* sign != overflow */
		if (subtractflags & 0x4)
			return AR_Compare_EQ;	/* zero flag set */
		return AR_Compare_GT;
	}

	/* Unsigned comparisons */
	if (subtractflags & 0x1)
		return AR_Compare_LT;	/* carry bit set */
	if (subtractflags & 0x4)
		return AR_Compare_EQ;	/* zero bit set */
	return (AR_Compare_GT);
}


/* Floating-point comparison */
static
AR_COMPARE_TYPE
ar_compare_float (const ar_data *opnd1, const AR_TYPE *opnd1type,
		  const ar_data *opnd2, const AR_TYPE *opnd2type) {

	ar_data re1, im1, re2, im2;
	AR_TYPE reimtype1, reimtype2;
	int status, restat, imstat;
	AR_COMPARE_TYPE recomp, imcomp;

	if (AR_FLOAT_IS_COMPLEX (*opnd1type) == AR_FLOAT_SIMPLE) {

		switch (*opnd1type) {
		case AR_Float_Cray1_64:
		case AR_Float_Cray1_64_F:
			status = ar_cfcmp64 (&opnd1->ar_f64, &opnd2->ar_f64,
					     ROUND_MODE (*opnd1type));
			break;
		case AR_Float_Cray1_128:
			status = ar_cfcmp128 (&opnd1->ar_f128, &opnd2->ar_f128);
			break;
		case AR_Float_IEEE_NR_32:
		case AR_Float_IEEE_ZE_32:
		case AR_Float_IEEE_UP_32:
		case AR_Float_IEEE_DN_32:
			status = ar_ifcmp32 (&opnd1->ar_ieee32, &opnd2->ar_ieee32);
			break;
		case AR_Float_IEEE_NR_64:
		case AR_Float_IEEE_ZE_64:
		case AR_Float_IEEE_UP_64:
		case AR_Float_IEEE_DN_64:
			status = ar_ifcmp64 (&opnd1->ar_ieee64, &opnd2->ar_ieee64);
			break;
		case AR_Float_IEEE_NR_128:
		case AR_Float_IEEE_ZE_128:
		case AR_Float_IEEE_UP_128:
		case AR_Float_IEEE_DN_128:
			status = ar_ifcmp128(&opnd1->ar_ieee128, &opnd2->ar_ieee128);
			break;
		default:
			return AR_Compare_Invalid;
		}

		if (status & AR_STAT_UNDEFINED)
			return AR_Compare_Unord;
		if (status & AR_STAT_ZERO)
			return AR_Compare_EQ;
		if (status & AR_STAT_NEGATIVE)
			return AR_Compare_LT;
		return AR_Compare_GT;

	}

	/* Complex comparison */
	status = ar_decompose_complex (&re1, &im1, &reimtype1,
				       opnd1, opnd1type);
	status |= ar_decompose_complex (&re2, &im2, &reimtype2,
					opnd2, opnd2type);
	if (status & (AR_STAT_INVALID_TYPE))
		return AR_Compare_Invalid;
	recomp = ar_compare_float (&re1, &reimtype1, &re2, &reimtype2);
	imcomp = ar_compare_float (&im1, &reimtype1, &im2, &reimtype2);
	if (recomp == AR_Compare_Invalid || imcomp == AR_Compare_Invalid)
		return AR_Compare_Invalid;
	if (recomp == AR_Compare_Unord || imcomp == AR_Compare_Unord)
		return AR_Compare_Unord;
	if (recomp == AR_Compare_EQ && imcomp == AR_Compare_EQ)
		return AR_Compare_EQ;
	return AR_Compare_NE;
}


/* General dispatch routine for comparison. */
AR_COMPARE_TYPE
AR_compare
		(const AR_DATA *op1, const AR_TYPE *opnd1type,
		 const AR_DATA *op2, const AR_TYPE *opnd2type) {

	ar_data* opnd1 = (ar_data*)op1;
	ar_data* opnd2 = (ar_data*)op2;

	ar_data temp1, temp2;
	AR_TYPE sint64_artype = AR_Int_64_S;
	AR_TYPE shifttype = AR_Int_64_U;

	if (*opnd1type != *opnd2type)
		return AR_Compare_Invalid;

	if (AR_CLASS (*opnd1type) == AR_CLASS_POINTER) {

		if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_FCTN) {
			if (opnd1->ar_i64.part1 == opnd2->ar_i64.part1 &&
			    opnd1->ar_i64.part2 == opnd2->ar_i64.part2 &&
			    opnd1->ar_i64.part3 == opnd2->ar_i64.part3 &&
			    opnd1->ar_i64.part4 == opnd2->ar_i64.part4)
				return AR_Compare_EQ;
			else
				return AR_Compare_NE;
			}

		if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_CHAR) {
			ar_dblshift (&temp1, &shifttype, opnd1, opnd1, 128-3);
			ar_dblshift (&temp2, &shifttype, opnd2, opnd2, 128-3);
		} else {
			temp1 = *opnd1;
			temp2 = *opnd2;
		}

		return ar_compare_integer (&temp1, &sint64_artype,
					   &temp2, &sint64_artype);
	}

	if (AR_CLASS (*opnd1type) == AR_CLASS_INT) {
		if (AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_8 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_16 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_32 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_46 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_64)
			return AR_Compare_Invalid;
		return ar_compare_integer (opnd1, opnd1type, opnd2, opnd2type);
	}

	if (AR_CLASS (*opnd1type) == AR_CLASS_FLOAT)
		return ar_compare_float (opnd1, opnd1type, opnd2, opnd2type);

	return AR_Compare_Invalid;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: compare.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
