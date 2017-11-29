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


/* High-level interface for basic math operations (+,-,*,/,%), as well
 * as implementation code for integer and pointer functions.
 */

#include "arith.internal.h"


/* Integer adder */
int
ar_add_integer
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	int status;
	int carry, overflow;
	long partialsum;
        int opnd1sign, opnd2sign;

	switch (AR_INT_SIZE (*opnd1type)) {
	case AR_INT_SIZE_8:
		opnd1sign = INT8_SIGN(opnd1) != 0;
		opnd2sign = INT8_SIGN(opnd2) != 0;

		partialsum = (long)opnd1->ar_i8.part5 + opnd2->ar_i8.part5;
		result->ar_i8.part5 = partialsum & 0xFF;
		carry = partialsum >> 8;
		ZERO_INT8_UPPER(result);

		if (AR_SIGNEDNESS (*opnd1type) == AR_UNSIGNED)
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else if ((opnd1sign == opnd2sign) &&
			 (opnd1sign != (INT8_SIGN(result) != 0)))
			/*
			 * If the operands have the same sign, and the result
			 * sign is different, signal overflow.
			 */
			status = AR_STAT_OVERFLOW;
		else
			status = AR_STAT_OK;

		/* Inlined from AR_status: */
		if (IS_INT8_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT8_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	case AR_INT_SIZE_16:
		opnd1sign = INT16_SIGN(opnd1) != 0;
		opnd2sign = INT16_SIGN(opnd2) != 0;

		partialsum = (long)opnd1->ar_i64.part4 + opnd2->ar_i64.part4;
		result->ar_i64.part4 = partialsum & 0xFFFF;
		carry = partialsum >> 16;
		ZERO_INT16_UPPER(result);

		if (AR_SIGNEDNESS (*opnd1type) == AR_UNSIGNED)
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else if ((opnd1sign == opnd2sign) &&
			 (opnd1sign != (INT16_SIGN(result) != 0)))
			/*
			 * If the operands have the same sign, and the result
			 * sign is different, signal overflow.
			 */
			status = AR_STAT_OVERFLOW;
		else
			status = AR_STAT_OK;

		/* Inlined from AR_status: */
		if (IS_INT16_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT16_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	case AR_INT_SIZE_32:
		opnd1sign = INT32_SIGN(opnd1) != 0;
		opnd2sign = INT32_SIGN(opnd2) != 0;

		partialsum = (long)opnd1->ar_i64.part4 + opnd2->ar_i64.part4;
		result->ar_i64.part4 = partialsum & 0xFFFF;
		carry = partialsum >> 16;

		partialsum = (long)opnd1->ar_i64.part3 + opnd2->ar_i64.part3 +
			     carry;
		result->ar_i64.part3 = partialsum & 0xFFFF;
		carry = partialsum >> 16;
		ZERO_INT32_UPPER(result);

		if (AR_SIGNEDNESS (*opnd1type) == AR_UNSIGNED)
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else if ((opnd1sign == opnd2sign) &&
			 (opnd1sign != (INT32_SIGN(result) != 0)))
			/*
			 * If the operands have the same sign, and the result
			 * sign is different, signal overflow.
			 */
			status = AR_STAT_OVERFLOW;
		else
			status = AR_STAT_OK;

		/* Inlined from AR_status: */
		if (IS_INT32_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT32_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		opnd1sign = INT64_SIGN(opnd1) != 0;
		opnd2sign = INT64_SIGN(opnd2) != 0;

		partialsum = (long)opnd1->ar_i64.part4 + opnd2->ar_i64.part4;
		result->ar_i64.part4 = partialsum & 0xFFFF;
		carry = partialsum >> 16;

		partialsum = (long)opnd1->ar_i64.part3 + opnd2->ar_i64.part3 +
			     carry;
		result->ar_i64.part3 = partialsum & 0xFFFF;
		carry = partialsum >> 16;

		partialsum = (long)opnd1->ar_i64.part2 + opnd2->ar_i64.part2 +
			     carry;
		result->ar_i64.part2 = partialsum & 0xFFFF;
		carry = partialsum >> 16;

		partialsum = (long)opnd1->ar_i64.part1 + opnd2->ar_i64.part1 +
			     carry;
		result->ar_i64.part1 = partialsum & 0xFFFF;
		carry = partialsum >> 16;

		if (AR_SIGNEDNESS (*opnd1type) == AR_UNSIGNED)
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else if ((opnd1sign == opnd2sign) &&
			 (opnd1sign != (INT64_SIGN(result) != 0)))
			/*
			 * If the operands have the same sign, and the result
			 * sign is different, signal overflow.
			 */
			status = AR_STAT_OVERFLOW;
		else
			status = AR_STAT_OK;

		/* Inlined from AR_status: */
		if (IS_INT64_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT64_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return status;
}

/* Integer subtraction */
int
ar_subtract_integer
		(ar_data *result, const AR_TYPE *resulttype, int *flags,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	int status;
	long sign, zero, overflow, carry;
	long partialdiff;
        int opnd1sign, opnd2sign;

	if (*resulttype != *opnd1type ||
	    *resulttype != *opnd2type ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*opnd1type)) {
	case AR_INT_SIZE_8:
		opnd1sign = INT8_SIGN(opnd1) != 0;
		opnd2sign = INT8_SIGN(opnd2) != 0;

		partialdiff = (long) opnd1->ar_i8.part5 - opnd2->ar_i8.part5;
		result->ar_i8.part5 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;
		ZERO_INT8_UPPER(result);

		/* If the operands differ in sign, and the result sign differs
		 * from the first operand's, signal overflow.
		 */
		overflow = ((opnd1sign != opnd2sign) &&
			    (opnd1sign != (INT8_SIGN(result) != 0)));
		zero = IS_INT8_ZERO(result);
		sign = !!INT8_SIGN(result);

		/* Return the flags to the caller if requested as SZVC bits. */
		if (flags)
			*flags = (sign << 3) | (zero << 2) | (overflow << 1) |
				 carry;

		if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED)
			status = overflow ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		break;

	case AR_INT_SIZE_16:
		opnd1sign = INT16_SIGN(opnd1) != 0;
		opnd2sign = INT16_SIGN(opnd2) != 0;

		partialdiff = (long) opnd1->ar_i64.part4 - opnd2->ar_i64.part4;
		result->ar_i64.part4 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;
		ZERO_INT16_UPPER(result);

		/* If the operands differ in sign, and the result sign differs
		 * from the first operand's, signal overflow.
		 */
		overflow = ((opnd1sign != opnd2sign) &&
			    (opnd1sign != (INT16_SIGN(result) != 0)));
		zero = IS_INT16_ZERO(result);
		sign = !!INT16_SIGN(result);

		/* Return the flags to the caller if requested as SZVC bits. */
		if (flags)
			*flags = (sign << 3) | (zero << 2) | (overflow << 1) |
				 carry;

		if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED)
			status = overflow ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		break;

	case AR_INT_SIZE_32:
		opnd1sign = INT32_SIGN(opnd1) != 0;
		opnd2sign = INT32_SIGN(opnd2) != 0;

		partialdiff = (long) opnd1->ar_i64.part4 - opnd2->ar_i64.part4;
		result->ar_i64.part4 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;

		partialdiff = (long) opnd1->ar_i64.part3 - opnd2->ar_i64.part3 -
			      carry;
		result->ar_i64.part3 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;
		ZERO_INT32_UPPER(result);

		/* If the operands differ in sign, and the result sign differs
		 * from the first operand's, signal overflow.
		 */
		overflow = ((opnd1sign != opnd2sign) &&
			    (opnd1sign != (INT32_SIGN(result) != 0)));
		zero = IS_INT32_ZERO(result);
		sign = !!INT32_SIGN(result);

		/* Return the flags to the caller if requested as SZVC bits. */
		if (flags)
			*flags = (sign << 3) | (zero << 2) | (overflow << 1) |
				 carry;

		if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED)
			status = overflow ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		opnd1sign = INT64_SIGN(opnd1) != 0;
		opnd2sign = INT64_SIGN(opnd2) != 0;

		partialdiff = (long) opnd1->ar_i64.part4 - opnd2->ar_i64.part4;
		result->ar_i64.part4 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;

		partialdiff = (long) opnd1->ar_i64.part3 - opnd2->ar_i64.part3 -
			      carry;
		result->ar_i64.part3 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;

		partialdiff = (long) opnd1->ar_i64.part2 - opnd2->ar_i64.part2 -
			      carry;
		result->ar_i64.part2 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;

		partialdiff = (long) opnd1->ar_i64.part1 - opnd2->ar_i64.part1 -
			      carry;
		result->ar_i64.part1 = partialdiff & 0xFFFF;
		carry = partialdiff < 0;

		/* If the operands differ in sign, and the result sign differs
		 * from the first operand's, signal overflow.
		 */
		overflow = ((opnd1sign != opnd2sign) &&
			    (opnd1sign != (INT64_SIGN(result) != 0)));
		zero = IS_INT64_ZERO(result);
		sign = !!INT64_SIGN(result);

		/* Return the flags to the caller if requested as SZVC bits. */
		if (flags)
			*flags = (sign << 3) | (zero << 2) | (overflow << 1) |
				 carry;

		if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED)
			status = overflow ? AR_STAT_OVERFLOW : AR_STAT_OK;
		else
			status = carry ? AR_STAT_OVERFLOW : AR_STAT_OK;
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return status | AR_status ((AR_DATA*)result, resulttype);
}


/* Pointer + integer computation */
static
int
ar_add_pointer
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	ar_data temp, ptropnd, intopnd;
	AR_TYPE temptype, ptropndtype, intopndtype;
	AR_TYPE shifttype = AR_Int_64_U;

	if (AR_CLASS (*opnd1type) == AR_CLASS_POINTER) {
		ptropnd = *opnd1;
		intopnd = *opnd2;
		ptropndtype = *opnd1type;
		intopndtype = *opnd2type;
	} else {
		ptropnd = *opnd2;
		intopnd = *opnd1;
		ptropndtype = *opnd2type;
		intopndtype = *opnd1type;
	}

	if (*resulttype != ptropndtype ||
	    AR_CLASS (intopndtype) != AR_CLASS_INT ||
	    AR_POINTER_FORMAT (ptropndtype) == AR_POINTER_FCTN)
		return AR_STAT_INVALID_TYPE;

	if (AR_POINTER_FORMAT (ptropndtype) == AR_POINTER_CHAR) {
		/* Turn C char pointer into byte address */
		ar_dblshift (&temp, &shifttype, &ptropnd, &ptropnd, 128-3);
		ptropnd = temp;
	}

	ar_add_integer (result, &intopndtype,
			&ptropnd, &intopndtype,
			&intopnd, &intopndtype);

	if (AR_POINTER_FORMAT (ptropndtype) == AR_POINTER_CHAR) {
		/* Restore byte address into C char pointer */
		ar_dblshift (&temp, &shifttype, result, result, 3);
		result->ar_i64 = temp.ar_i64;
	}

	ar_clear_unused_bits (result, resulttype);

	return AR_STAT_OK;
}


/* Pointer - integer, pointer - pointer computation */
static
int
ar_subtract_pointer
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	ar_data temp1, temp2;
	AR_TYPE temp1type, temp2type;
	AR_TYPE shifttype = AR_Int_64_U;

	if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_FCTN)
		return AR_STAT_INVALID_TYPE;

	if (AR_CLASS (*opnd2type) == AR_CLASS_POINTER) {
		/* pointer difference */
		if (*opnd1type != *opnd2type ||
		    AR_CLASS (*resulttype) != AR_CLASS_INT ||
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_8 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_16 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_32 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_46 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_64)
			return AR_STAT_INVALID_TYPE;
		if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_CHAR) {
			/* Convert C char pointers to byte addresses */
			ar_dblshift (&temp1, &shifttype, opnd1, opnd1, 128-3);
			ar_dblshift (&temp2, &shifttype, opnd2, opnd2, 128-3);
		} else {
			temp1 = *opnd1;
			temp2 = *opnd2;
		}
		return ar_subtract_integer (result, resulttype, (int *)0,
					    &temp1, resulttype,
					    &temp2, resulttype);
	}

	if (AR_CLASS (*opnd2type) == AR_CLASS_INT) {
		if (*resulttype != *opnd1type ||
		    AR_INT_SIZE (*opnd2type) != AR_INT_SIZE_8 &&
		    AR_INT_SIZE (*opnd2type) != AR_INT_SIZE_16 &&
		    AR_INT_SIZE (*opnd2type) != AR_INT_SIZE_32 &&
		    AR_INT_SIZE (*opnd2type) != AR_INT_SIZE_46 &&
		    AR_INT_SIZE (*opnd2type) != AR_INT_SIZE_64)
			return AR_STAT_INVALID_TYPE;
		if (AR_POINTER_FORMAT (*resulttype) == AR_POINTER_CHAR)
			ar_dblshift (&temp1, &shifttype, opnd1, opnd1, 128-3);
		else
			temp1 = *opnd1;
		ar_subtract_integer (result, opnd2type, (int *)0,
				     &temp1, opnd2type,
				     opnd2, opnd2type);
		if (AR_POINTER_FORMAT (*resulttype) == AR_POINTER_CHAR) {
			ar_dblshift (&temp1, &shifttype, result, result, 3);
			result->ar_i64 = temp1.ar_i64;
		}
		ar_clear_unused_bits (result, resulttype);
		return AR_STAT_OK;
	}

	return AR_STAT_INVALID_TYPE;
}


/* General dispatch routine for addition */
int
AR_add	(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;

	int status = AR_STAT_OK, restat, imstat;

	ar_data	tmp1, tmp2;

	if (AR_CLASS (*opnd1type) == AR_CLASS_POINTER ||
	    AR_CLASS (*opnd2type) == AR_CLASS_POINTER)
		return ar_add_pointer (result, resulttype,
				       opnd1, opnd1type,
				       opnd2, opnd2type);

	if (AR_CLASS (*opnd1type) == AR_CLASS_INT) {
		if (*resulttype != *opnd1type ||
		    *resulttype != *opnd2type ||
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_8 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_16 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_32 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_46 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_64)
			return AR_STAT_INVALID_TYPE;
		status = ar_add_integer (result, resulttype,
					 opnd1, opnd1type,
					 opnd2, opnd2type);
		if (AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED)
			status &= ~AR_STAT_OVERFLOW;
		return status;
	}

	if (AR_CLASS (*opnd1type) == AR_CLASS_FLOAT) {

		if (*resulttype != *opnd1type || *resulttype != *opnd2type)
			return AR_STAT_INVALID_TYPE;

		switch (*resulttype) {
		case AR_Float_Cray1_64:
		case AR_Float_Cray1_64_F:
			status = ar_cfadd64 (&result->ar_f64,
					     &opnd1->ar_f64, &opnd2->ar_f64);
                        if (ar_state_register.ar_truncate_bits > 0)
                                ar_CRAY_64_trunc(&result->ar_f64);
                        return status;

		case AR_Float_Cray1_128:
			return ar_cfadd128 (&result->ar_f128,
					    &opnd1->ar_f128, &opnd2->ar_f128);
		case AR_Float_IEEE_NR_32:
		case AR_Float_IEEE_ZE_32:
		case AR_Float_IEEE_UP_32:
		case AR_Float_IEEE_DN_32:
			return ar_ifadd32 (&result->ar_ieee32,
					   &opnd1->ar_ieee32, &opnd2->ar_ieee32,
					   ROUND_MODE (*resulttype));
		case AR_Float_IEEE_NR_64:
		case AR_Float_IEEE_ZE_64:
		case AR_Float_IEEE_UP_64:
		case AR_Float_IEEE_DN_64:
			return ar_ifadd64 (&result->ar_ieee64,
					   &opnd1->ar_ieee64, &opnd2->ar_ieee64,
					   ROUND_MODE (*resulttype));
		case AR_Float_IEEE_NR_128:
		case AR_Float_IEEE_ZE_128:
		case AR_Float_IEEE_UP_128:
		case AR_Float_IEEE_DN_128:
			return ar_ifadd128 (&result->ar_ieee128,
					   &opnd1->ar_ieee128, &opnd2->ar_ieee128,
					   ROUND_MODE (*resulttype));
		case AR_Complex_Cray1_64:
		case AR_Complex_Cray1_64_F:
			restat = ar_cfadd64 (&result->ar_cplx_f64.real,
					     &opnd1->ar_cplx_f64.real,
					     &opnd2->ar_cplx_f64.real);
                        if (ar_state_register.ar_truncate_bits > 0)
                                ar_CRAY_64_trunc(&result->ar_cplx_f64.real);
			imstat = ar_cfadd64 (&result->ar_cplx_f64.imag,
					     &opnd1->ar_cplx_f64.imag,
					     &opnd2->ar_cplx_f64.imag);
                        if (ar_state_register.ar_truncate_bits > 0)
                                ar_CRAY_64_trunc(&result->ar_cplx_f64.imag);
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		case AR_Complex_Cray1_128:
			restat = ar_cfadd128 (&result->ar_cplx_f128.real,
					      &opnd1->ar_cplx_f128.real,
					      &opnd2->ar_cplx_f128.real);
			imstat = ar_cfadd128 (&result->ar_cplx_f128.imag,
					      &opnd1->ar_cplx_f128.imag,
					      &opnd2->ar_cplx_f128.imag);
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		case AR_Complex_IEEE_NR_32:
		case AR_Complex_IEEE_ZE_32:
		case AR_Complex_IEEE_UP_32:
		case AR_Complex_IEEE_DN_32:
		{
			AR_IEEE_32 o1, o2, rslt;

			CPLX32_REAL_TO_IEEE32(o1, opnd1->ar_cplx_ieee32);
			CPLX32_REAL_TO_IEEE32(o2, opnd2->ar_cplx_ieee32);
			restat = ar_ifadd32 (&rslt, &o1, &o2,
					     ROUND_MODE (*resulttype));
			IEEE32_TO_CPLX32_REAL(result->ar_cplx_ieee32, rslt);

			CPLX32_IMAG_TO_IEEE32(o1, opnd1->ar_cplx_ieee32);
			CPLX32_IMAG_TO_IEEE32(o2, opnd2->ar_cplx_ieee32);
			imstat = ar_ifadd32 (&rslt, &o1, &o2,
					     ROUND_MODE (*resulttype));
			IEEE32_TO_CPLX32_IMAG(result->ar_cplx_ieee32, rslt);

			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		}
		case AR_Complex_IEEE_NR_64:
		case AR_Complex_IEEE_ZE_64:
		case AR_Complex_IEEE_UP_64:
		case AR_Complex_IEEE_DN_64:
			restat = ar_ifadd64 (&result->ar_cplx_ieee64.real,
					     &opnd1->ar_cplx_ieee64.real,
					     &opnd2->ar_cplx_ieee64.real,
					     ROUND_MODE (*resulttype));
			imstat = ar_ifadd64 (&result->ar_cplx_ieee64.imag,
					     &opnd1->ar_cplx_ieee64.imag,
					     &opnd2->ar_cplx_ieee64.imag,
					     ROUND_MODE (*resulttype));
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		case AR_Complex_IEEE_NR_128:
		case AR_Complex_IEEE_ZE_128:
		case AR_Complex_IEEE_UP_128:
		case AR_Complex_IEEE_DN_128:
			restat = ar_ifadd128 (&result->ar_cplx_ieee128.real,
					     &opnd1->ar_cplx_ieee128.real,
					     &opnd2->ar_cplx_ieee128.real,
					     ROUND_MODE (*resulttype));
			imstat = ar_ifadd128 (&result->ar_cplx_ieee128.imag,
					     &opnd1->ar_cplx_ieee128.imag,
					     &opnd2->ar_cplx_ieee128.imag,
					     ROUND_MODE (*resulttype));
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		default:
			return AR_STAT_INVALID_TYPE;
		}
	}

	return AR_STAT_INVALID_TYPE;
}


/* General dispatch routine for subtraction */
int
AR_subtract
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;

	int status, restat, imstat;

	ar_data	tmp1, tmp2;

	if (AR_CLASS (*opnd1type) == AR_CLASS_POINTER)
		return ar_subtract_pointer (result, resulttype,
					    opnd1, opnd1type,
					    opnd2, opnd2type);

	if (AR_CLASS (*opnd1type) == AR_CLASS_INT) {
		status = ar_subtract_integer (result, resulttype, (int *)0,
					      opnd1, opnd1type,
					      opnd2, opnd2type);
		if (AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED)
			status &= ~AR_STAT_OVERFLOW;
		return status;
	}

	if (AR_CLASS (*opnd1type) == AR_CLASS_FLOAT) {
		if (*resulttype != *opnd1type || *resulttype != *opnd2type)
			return AR_STAT_INVALID_TYPE;
		switch (*resulttype) {
		case AR_Float_Cray1_64:
		case AR_Float_Cray1_64_F:
			status = ar_cfsub64 (&result->ar_f64,
					     &opnd1->ar_f64, &opnd2->ar_f64);
                        if (ar_state_register.ar_truncate_bits > 0)
                                ar_CRAY_64_trunc(&result->ar_f64);
                        return status;

		case AR_Float_Cray1_128:
			return ar_cfsub128 (&result->ar_f128,
					    &opnd1->ar_f128, &opnd2->ar_f128);
		case AR_Float_IEEE_NR_32:
		case AR_Float_IEEE_ZE_32:
		case AR_Float_IEEE_DN_32:
		case AR_Float_IEEE_UP_32:
			return ar_ifsub32 (&result->ar_ieee32,
					   &opnd1->ar_ieee32, &opnd2->ar_ieee32,
					   ROUND_MODE (*resulttype));
		case AR_Float_IEEE_NR_64:
		case AR_Float_IEEE_ZE_64:
		case AR_Float_IEEE_DN_64:
		case AR_Float_IEEE_UP_64:
			return ar_ifsub64 (&result->ar_ieee64,
					   &opnd1->ar_ieee64, &opnd2->ar_ieee64,
					   ROUND_MODE (*resulttype));
		case AR_Float_IEEE_NR_128:
		case AR_Float_IEEE_ZE_128:
		case AR_Float_IEEE_DN_128:
		case AR_Float_IEEE_UP_128:
			return ar_ifsub128 (&result->ar_ieee128,
					   &opnd1->ar_ieee128, &opnd2->ar_ieee128,
					   ROUND_MODE (*resulttype));
		case AR_Complex_Cray1_64:
		case AR_Complex_Cray1_64_F:
			restat = ar_cfsub64 (&result->ar_cplx_f64.real,
					     &opnd1->ar_cplx_f64.real,
					     &opnd2->ar_cplx_f64.real);
                        if (ar_state_register.ar_truncate_bits > 0)
                                ar_CRAY_64_trunc(&result->ar_cplx_f64.real);
			imstat = ar_cfsub64 (&result->ar_cplx_f64.imag,
					     &opnd1->ar_cplx_f64.imag,
					     &opnd2->ar_cplx_f64.imag);
                        if (ar_state_register.ar_truncate_bits > 0)
                                ar_CRAY_64_trunc(&result->ar_cplx_f64.imag);
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		case AR_Complex_Cray1_128:
			restat = ar_cfsub128 (&result->ar_cplx_f128.real,
					     &opnd1->ar_cplx_f128.real,
					     &opnd2->ar_cplx_f128.real);
			imstat = ar_cfsub128 (&result->ar_cplx_f128.imag,
					     &opnd1->ar_cplx_f128.imag,
					     &opnd2->ar_cplx_f128.imag);
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		case AR_Complex_IEEE_NR_32:
		case AR_Complex_IEEE_ZE_32:
		case AR_Complex_IEEE_UP_32:
		case AR_Complex_IEEE_DN_32:
		{
			AR_IEEE_32 o1, o2, rslt;

			CPLX32_REAL_TO_IEEE32(o1, opnd1->ar_cplx_ieee32);
			CPLX32_REAL_TO_IEEE32(o2, opnd2->ar_cplx_ieee32);
			restat = ar_ifsub32 (&rslt, &o1, &o2,
					     ROUND_MODE (*resulttype));
			IEEE32_TO_CPLX32_REAL(result->ar_cplx_ieee32, rslt);

			CPLX32_IMAG_TO_IEEE32(o1, opnd1->ar_cplx_ieee32);
			CPLX32_IMAG_TO_IEEE32(o2, opnd2->ar_cplx_ieee32);
			imstat = ar_ifsub32 (&rslt, &o1, &o2,
					     ROUND_MODE (*resulttype));
			IEEE32_TO_CPLX32_IMAG(result->ar_cplx_ieee32, rslt);

			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		}
		case AR_Complex_IEEE_NR_64:
		case AR_Complex_IEEE_ZE_64:
		case AR_Complex_IEEE_UP_64:
		case AR_Complex_IEEE_DN_64:
			restat = ar_ifsub64 (&result->ar_cplx_ieee64.real,
					     &opnd1->ar_cplx_ieee64.real,
					     &opnd2->ar_cplx_ieee64.real,
					     ROUND_MODE (*resulttype));
			imstat = ar_ifsub64 (&result->ar_cplx_ieee64.imag,
					     &opnd1->ar_cplx_ieee64.imag,
					     &opnd2->ar_cplx_ieee64.imag,
					     ROUND_MODE (*resulttype));
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		case AR_Complex_IEEE_NR_128:
		case AR_Complex_IEEE_ZE_128:
		case AR_Complex_IEEE_UP_128:
		case AR_Complex_IEEE_DN_128:
			restat = ar_ifsub128 (&result->ar_cplx_ieee128.real,
					     &opnd1->ar_cplx_ieee128.real,
					     &opnd2->ar_cplx_ieee128.real,
					     ROUND_MODE (*resulttype));
			imstat = ar_ifsub128 (&result->ar_cplx_ieee128.imag,
					     &opnd1->ar_cplx_ieee128.imag,
					     &opnd2->ar_cplx_ieee128.imag,
					     ROUND_MODE (*resulttype));
			status = (restat | imstat) &
				 ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		default:
			return AR_STAT_INVALID_TYPE;
		}
	}

	return AR_STAT_INVALID_TYPE;
}


/* Integer negation */
int
ar_negate_integer
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd,   const AR_TYPE *opndtype) {

	ar_data temp, temp1;
	int status;

	if (AR_CLASS (*opndtype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;
	    
	switch (AR_INT_SIZE (*opndtype)) {
	case AR_INT_SIZE_8:
		/* Compute complement */
		temp.ar_i8.part5 = 0xFF ^ opnd->ar_i8.part5;

		/* Add one */
		temp1.ar_i8.part5 = 1;
		break;

	case AR_INT_SIZE_16:
		/* Compute complement */
		temp.ar_i64.part4 = 0xFFFF ^ opnd->ar_i64.part4;

		/* Add one */
		temp1.ar_i64.part4 = 1;
		break;

	case AR_INT_SIZE_32:
		/* Compute complement */
		temp.ar_i64.part3 = 0xFFFF ^ opnd->ar_i64.part3;
		temp.ar_i64.part4 = 0xFFFF ^ opnd->ar_i64.part4;

		/* Add one */
		temp1.ar_i64.part3 = 0;
		temp1.ar_i64.part4 = 1;
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/* Compute complement */
		temp.ar_i64.part1 = 0xFFFF ^ opnd->ar_i64.part1;
		temp.ar_i64.part2 = 0xFFFF ^ opnd->ar_i64.part2;
		temp.ar_i64.part3 = 0xFFFF ^ opnd->ar_i64.part3;
		temp.ar_i64.part4 = 0xFFFF ^ opnd->ar_i64.part4;

		/* Add one */
		temp1.ar_i64.part1 = 0;
		temp1.ar_i64.part2 = 0;
		temp1.ar_i64.part3 = 0;
		temp1.ar_i64.part4 = 1;
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	status = ar_add_integer (result, resulttype,
				 &temp, opndtype,
				 &temp1, opndtype);
	if (AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED)
		status &= ~AR_STAT_OVERFLOW;
	else if((status&AR_STAT_OVERFLOW) && (status&AR_STAT_NEGATIVE)) {
		switch (AR_INT_SIZE (*resulttype)) {
		case AR_INT_SIZE_8:
			if(result->ar_i8.part5 == 0x80)
				status = (status^AR_STAT_OVERFLOW) |
					 AR_STAT_SEMIVALID;
			break;

		case AR_INT_SIZE_16:
			if(result->ar_i64.part4 == 0x8000)
				status = (status^AR_STAT_OVERFLOW) |
					 AR_STAT_SEMIVALID;
			break;

		case AR_INT_SIZE_32:
			if(result->ar_i64.part3 == 0x8000 &&
			   result->ar_i64.part4 == 0)
				status = (status^AR_STAT_OVERFLOW) |
					 AR_STAT_SEMIVALID;
			break;

		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			if (result->ar_i64.part1 == 0x8000 &&
			    result->ar_i64.part2 == 0 &&
			    result->ar_i64.part3 == 0 &&
			    result->ar_i64.part4 == 0)
				status = (status^AR_STAT_OVERFLOW) |
					 AR_STAT_SEMIVALID;
			break;

		default:
			return (AR_STAT_INVALID_TYPE);
		}
	}

	return status;
}


/* Floating-point negation */
int
ar_negate_float
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd,   const AR_TYPE *opndtype) {

	if (*resulttype != *opndtype ||
	    AR_CLASS (*resulttype) != AR_CLASS_FLOAT)
		return AR_STAT_INVALID_TYPE;

	if (AR_FLOAT_FORMAT (*resulttype) == AR_FLOAT_CRAY) {
		if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_64) {
			result->ar_f64 = opnd->ar_f64;
			if (result->ar_f64.expo |
				result->ar_f64.coeff0 | result->ar_f64.coeff1 |
				result->ar_f64.coeff2) {
				result->ar_f64.sign ^= 1;
				return AR_STAT_OK;
			} else
				return AR_STAT_ZERO;
		}
		if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_128) {
			result->ar_f128 = opnd->ar_f128;
			if (result->ar_f128.expo |
			    result->ar_f128.coeff0 | result->ar_f128.coeff1 |
			    result->ar_f128.coeff2 | result->ar_f128.coeff3 |
			    result->ar_f128.coeff4 | result->ar_f128.coeff5) {
				result->ar_f128.sign ^= 1;
				return AR_STAT_OK;
			} else
				return AR_STAT_ZERO;
		}
		return AR_STAT_INVALID_TYPE;
	}

	/* Must be IEEE */
	if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_32) {
		result->ar_ieee32 = opnd->ar_ieee32;
		result->ar_ieee32.sign ^= 1;
		return AR_status ((AR_DATA*)result, resulttype);
	}

	if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_64) {
		result->ar_ieee64 = opnd->ar_ieee64;
		result->ar_ieee64.sign ^= 1;
		return AR_status ((AR_DATA*)result, resulttype);
	}

	if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_128) {
		if (HOST_IS_MIPS) {
			result->ar_mips128 = opnd->ar_mips128;
			result->ar_mips128.sign ^= 1;
			result->ar_mips128.signl ^= 1;
		}
		else {
			result->ar_ieee128 = opnd->ar_ieee128;
			result->ar_ieee128.sign ^= 1;
		}
		return AR_status ((AR_DATA*)result, resulttype);
	}

	return AR_STAT_INVALID_TYPE;
}


static
int
ar_negate_complex
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd,   const AR_TYPE *opndtype) {

	int status, restat, imstat;
	ar_data re, im, nre, nim;
	AR_TYPE reimtype, cplxtype;

	if (*resulttype != *opndtype ||
	    AR_CLASS (*resulttype) != AR_CLASS_FLOAT ||
	    AR_FLOAT_IS_COMPLEX (*resulttype) != AR_FLOAT_COMPLEX)
		return AR_STAT_INVALID_TYPE;

	status = ar_decompose_complex (&re, &im, &reimtype, opnd, opndtype);
	status |= restat = ar_negate_float (&nre, &reimtype, &re, &reimtype);
	status |= imstat = ar_negate_float (&nim, &reimtype, &im, &reimtype);
	status |= ar_compose_complex (result, &cplxtype, &nre, &nim, &reimtype);
	status &= ~(AR_STAT_NEGATIVE | AR_STAT_ZERO);
	status |= restat & imstat & AR_STAT_ZERO;
	return status;
}


/* General dispatch routine for negation */
int
AR_negate
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	if (*resulttype != *opndtype)
		return AR_STAT_INVALID_TYPE;
	if (AR_CLASS (*resulttype) == AR_CLASS_INT)
		return ar_negate_integer (result, resulttype, opnd, opndtype);
	if (AR_CLASS (*resulttype) == AR_CLASS_FLOAT)
		if (AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX)
			return ar_negate_complex (result, resulttype, opnd, opndtype);
		else
			return ar_negate_float (result, resulttype, opnd, opndtype);
	return AR_STAT_INVALID_TYPE;
}


/* Integer multiplication */
int
ar_multiply_integer
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	ar_data multiplicand, multiplier, accum, newaccum, temp;
	AR_TYPE uint32_artype = AR_Int_32_U;
	AR_TYPE uint64_artype = AR_Int_64_U;
	unsigned long partialmult;
	int status = AR_STAT_OK;
	int result_negative;

	int part_a, part_b, part_c, part_d, part_e, part_f, part_g, part_h;

	switch (AR_INT_SIZE (*opnd1type)) {
	case AR_INT_SIZE_8:
		result_negative = AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
				  INT8_SIGN(opnd1);
		if (result_negative)
			ar_negate_integer (&multiplicand, opnd1type, opnd1,
					   opnd1type);
		else
			multiplicand.ar_i8 = opnd1->ar_i8;
		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED &&
		    INT8_SIGN(opnd2)) {
			result_negative ^= 1;
			ar_negate_integer (&multiplier, opnd2type, opnd2,
					   opnd2type);
		} else
			multiplier.ar_i8 = opnd2->ar_i8;

		/* Preload fields into registers */
		part_a = multiplicand.ar_i8.part5;
		part_b = multiplier.ar_i8.part5;

		/* Initialize accumulator to a * b */
		partialmult = part_a * part_b;
		if (partialmult >> 8)
			status |= AR_STAT_OVERFLOW;
		ZERO_INT8_UPPER(&accum);
		accum.ar_i8.part5 = partialmult;

		if (result_negative) {
			ar_negate_integer (result, resulttype, &accum,
					   resulttype);
			if (!INT8_SIGN(result) && !IS_INT8_ZERO(result))
				status |= AR_STAT_OVERFLOW;
		} else {
			result->ar_i8 = accum.ar_i8;
			if (INT8_SIGN(result) &&
			    (AR_SIGNEDNESS(*resulttype) == AR_SIGNED)) {
				status |= AR_STAT_OVERFLOW;
			}
		}

		/* Inlined from AR_status routine: */
		status &= AR_STAT_OVERFLOW;
		if (IS_INT8_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT8_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	case AR_INT_SIZE_16:
		result_negative = AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
				  INT16_SIGN(opnd1);
		if (result_negative)
			ar_negate_integer (&multiplicand, opnd1type, opnd1,
					   opnd1type);
		else
			multiplicand.ar_i64 = opnd1->ar_i64;
		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED &&
		    INT16_SIGN(opnd2)) {
			result_negative ^= 1;
			ar_negate_integer (&multiplier, opnd2type, opnd2,
					   opnd2type);
		} else
			multiplier.ar_i64 = opnd2->ar_i64;

		/* Preload fields into registers */
		part_a = multiplicand.ar_i64.part4;
		part_b = multiplier.ar_i64.part4;

		/* Initialize accumulator to a * b */
		partialmult = part_a * part_b;
		if (partialmult >> 16)
			status |= AR_STAT_OVERFLOW;
		ZERO_INT16_UPPER(&accum);
		accum.ar_i64.part4 = partialmult;

		if (result_negative) {
			ar_negate_integer (result, resulttype, &accum,
					   resulttype);
			if (!INT16_SIGN(result) && !IS_INT16_ZERO(result))
				status |= AR_STAT_OVERFLOW;
		} else {
			result->ar_i64 = accum.ar_i64;
			if (INT16_SIGN(result) &&
			    (AR_SIGNEDNESS(*resulttype) == AR_SIGNED)) {
				status |= AR_STAT_OVERFLOW;
			}
		}

		/* Inlined from AR_status routine: */
		status &= AR_STAT_OVERFLOW;
		if (IS_INT16_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT16_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	case AR_INT_SIZE_32:
		result_negative = AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
				  INT32_SIGN(opnd1);
		if (result_negative)
			ar_negate_integer (&multiplicand, opnd1type, opnd1,
					   opnd1type);
		else
			multiplicand.ar_i64 = opnd1->ar_i64;
		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED &&
		    INT32_SIGN(opnd2)) {
			result_negative ^= 1;
			ar_negate_integer (&multiplier, opnd2type, opnd2,
					   opnd2type);
		} else
			multiplier.ar_i64 = opnd2->ar_i64;

		/* Preload fields into registers */
		part_a = multiplicand.ar_i64.part3;
		part_b = multiplicand.ar_i64.part4;
		part_c = multiplier.ar_i64.part3;
		part_d = multiplier.ar_i64.part4;

		/* Initialize accumulator to b * d */
		partialmult = part_b * part_d;
		ZERO_INT32_UPPER(&accum);
		accum.ar_i64.part3 = (partialmult & 0xFFFF0000) >> 16;
		accum.ar_i64.part4 = partialmult & 0xFFFF;

		newaccum.ar_i64.part1 = newaccum.ar_i64.part2 = 0;

		/*
		 *       a  b
		 *   x   c  d
		 *   --------------------
		 *      ad bd
		 *   ac bc
		 */

		/* .d */
		if (part_d) {

			/* bd computed above */

			/* ad */
			if (part_a) {
				partialmult = part_a * part_d;
				if (partialmult >> 16)
					status |= AR_STAT_OVERFLOW;
				temp.ar_i64.part3 = partialmult;
				temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint32_artype,
							  &accum,
							  &uint32_artype,
							  &temp,
							  &uint32_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}
		}

		/* .c */
		if (part_c) {

			/* bc */
			if (part_b) {
				partialmult = part_b * part_c;
				if (partialmult >> 16)
					status |= AR_STAT_OVERFLOW;
				temp.ar_i64.part3 = partialmult;
				temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint32_artype,
							  &accum,
							  &uint32_artype,
							  &temp,
							  &uint32_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* ac */
			if (part_a)
				status |= AR_STAT_OVERFLOW;
		}

		/* Result is all summed up. */

		if (result_negative) {
			ar_negate_integer (result, resulttype, &accum,
					   resulttype);
			if (!INT32_SIGN(result) && !IS_INT32_ZERO(result))
				status |= AR_STAT_OVERFLOW;
		} else {
			result->ar_i64 = accum.ar_i64;
			if (INT32_SIGN(result) &&
			    (AR_SIGNEDNESS(*resulttype) == AR_SIGNED)) {
				status |= AR_STAT_OVERFLOW;
			}
		}

		/* Inlined from AR_status routine: */
		status &= AR_STAT_OVERFLOW;
		if (IS_INT32_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT32_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		if (*opnd1type == AR_Int_46_S && INT_OVERFLOWS_46_BITS(*opnd1))
			status |= AR_STAT_OVERFLOW;
		if (*opnd2type == AR_Int_46_S && INT_OVERFLOWS_46_BITS(*opnd2))
			status |= AR_STAT_OVERFLOW;

		result_negative = AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
				  INT64_SIGN(opnd1);
		if (result_negative)
			ar_negate_integer (&multiplicand, opnd1type, opnd1,
					   opnd1type);
		else
			multiplicand.ar_i64 = opnd1->ar_i64;
		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED &&
		    INT64_SIGN(opnd2)) {
			result_negative ^= 1;
			ar_negate_integer (&multiplier, opnd2type, opnd2,
					   opnd2type);
		} else
			multiplier.ar_i64 = opnd2->ar_i64;

		/* Preload fields into registers */
		part_a = multiplicand.ar_i64.part1;
		part_b = multiplicand.ar_i64.part2;
		part_c = multiplicand.ar_i64.part3;
		part_d = multiplicand.ar_i64.part4;
		part_e = multiplier.ar_i64.part1;
		part_f = multiplier.ar_i64.part2;
		part_g = multiplier.ar_i64.part3;
		part_h = multiplier.ar_i64.part4;

		/* Initialize accumulator to d * h */
		partialmult = part_d * part_h;
		ZERO_INT32_UPPER(&accum);
		accum.ar_i64.part3 = (partialmult & 0xFFFF0000) >> 16;
		accum.ar_i64.part4 = partialmult & 0xFFFF;

		/*
		 *             a  b  c  d 
		 *   x         e  f  g  h 
		 *   --------------------
		 *            ah bh ch dh
		 *         ag bg cg dg
		 *      af bf cf df
		 *   ae be ce de
		 */

		/* .h */
		if (part_h) {

			/* dh computed above */

			/* ch */
			if (part_c) {
				partialmult = part_c * part_h;
				temp.ar_i64.part1 = temp.ar_i64.part4 = 0;
				temp.ar_i64.part2 = partialmult >> 16;
				temp.ar_i64.part3 = partialmult;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* bh */
			if (part_b) {
				partialmult = part_b * part_h;
				temp.ar_i64.part1 = partialmult >> 16;
				temp.ar_i64.part2 = partialmult;
				temp.ar_i64.part3 = temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* ah */
			if (part_a) {
				partialmult = part_a * part_h;
				if (partialmult >> 16)
					status |= AR_STAT_OVERFLOW;
				temp.ar_i64.part1 = partialmult;
				temp.ar_i64.part2 = temp.ar_i64.part3 =
					temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}
		}

		/* .g */
		if (part_g) {

			/* dg */
			if (part_d) {
				partialmult = part_d * part_g;
				temp.ar_i64.part1 = temp.ar_i64.part4 = 0;
				temp.ar_i64.part2 = partialmult >> 16;
				temp.ar_i64.part3 = partialmult;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* cg */
			if (part_c) {
				partialmult = part_c * part_g;
				temp.ar_i64.part1 = partialmult >> 16;
				temp.ar_i64.part2 = partialmult;
				temp.ar_i64.part3 = temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* bg */
			if (part_b) {
				partialmult = part_b * part_g;
				if (partialmult >> 16)
					status |= AR_STAT_OVERFLOW;
				temp.ar_i64.part1 = partialmult;
				temp.ar_i64.part2 = temp.ar_i64.part3 =
					temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* ag */
			if (part_a)
				status |= AR_STAT_OVERFLOW;
		}

		/* .f */
		if (part_f) {

			/* df */
			if (part_d) {
				partialmult = part_d * part_f;
				temp.ar_i64.part1 = partialmult >> 16;
				temp.ar_i64.part2 = partialmult;
				temp.ar_i64.part3 = temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* cf */
			if (part_c) {
				partialmult = part_c * part_f;
				if (partialmult >> 16)
					status |= AR_STAT_OVERFLOW;
				temp.ar_i64.part1 = partialmult;
				temp.ar_i64.part2 = temp.ar_i64.part3 =
					temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* bf, af */
			if (part_a | part_b)
				status |= AR_STAT_OVERFLOW;
		}

		/* .e */
		if (part_e) {

			/* de */
			if (part_d) {
				partialmult = part_d * part_e;
				if (partialmult >> 16)
					status |= AR_STAT_OVERFLOW;
				temp.ar_i64.part1 = partialmult;
				temp.ar_i64.part2 = temp.ar_i64.part3 =
					temp.ar_i64.part4 = 0;
				status |= ar_add_integer (&newaccum,
							  &uint64_artype,
							  &accum,
							  &uint64_artype,
							  &temp,
							  &uint64_artype);
				accum.ar_i64 = newaccum.ar_i64;
			}

			/* ce, be, ae */
			if (part_a | part_b | part_c)
				status |= AR_STAT_OVERFLOW;
		}

		/* Result is all summed up. */

		if (result_negative) {
			ar_negate_integer (result, resulttype, &accum,
					   resulttype);
			if (!INT64_SIGN(result) && !IS_INT64_ZERO(result))
				status |= AR_STAT_OVERFLOW;
		} else {
			result->ar_i64 = accum.ar_i64;
			if (INT64_SIGN(result) &&
			    (AR_SIGNEDNESS(*resulttype) == AR_SIGNED)) {
				status |= AR_STAT_OVERFLOW;
			}
		}

		if (*resulttype == AR_Int_46_S &&
		    INT_OVERFLOWS_46_BITS (*result))
			status |= AR_STAT_OVERFLOW;

		/* Inlined from AR_status routine: */
		status &= AR_STAT_OVERFLOW;
		if (IS_INT64_ZERO(result))
			status |= AR_STAT_ZERO;
		else if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			 INT64_SIGN(result))
			status |= AR_STAT_NEGATIVE;
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return status;
}


static
int
ar_multiply_float
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	int status;

	ar_data	tmp1, tmp2;

	switch (*opnd1type) {
	case AR_Float_Cray1_64:
	case AR_Float_Cray1_64_F:
		status = ar_cfmul64 (&result->ar_f64,
				     &opnd1->ar_f64, &opnd2->ar_f64,
				     ROUND_MODE (*opnd1type));
                if (ar_state_register.ar_truncate_bits > 0)
                        ar_CRAY_64_trunc(&result->ar_f64);
                return status;

	case AR_Float_Cray1_128:
		return ar_cfmul128 (&result->ar_f128,
				    &opnd1->ar_f128, &opnd2->ar_f128,
				    ROUND_MODE (*opnd1type));
	case AR_Float_IEEE_NR_32:
	case AR_Float_IEEE_ZE_32:
	case AR_Float_IEEE_UP_32:
	case AR_Float_IEEE_DN_32:
		return ar_ifmul32 (&result->ar_ieee32,
				   &opnd1->ar_ieee32, &opnd2->ar_ieee32,
				   ROUND_MODE (*opnd1type));
	case AR_Float_IEEE_NR_64:
	case AR_Float_IEEE_ZE_64:
	case AR_Float_IEEE_UP_64:
	case AR_Float_IEEE_DN_64:
		return ar_ifmul64 (&result->ar_ieee64,
				   &opnd1->ar_ieee64, &opnd2->ar_ieee64,
				   ROUND_MODE (*opnd1type));
	case AR_Float_IEEE_NR_128:
	case AR_Float_IEEE_ZE_128:
	case AR_Float_IEEE_UP_128:
	case AR_Float_IEEE_DN_128:
		return ar_ifmul128 (&result->ar_ieee128,
				   &opnd1->ar_ieee128, &opnd2->ar_ieee128,
				   ROUND_MODE (*opnd1type));
	default:
		return AR_STAT_INVALID_TYPE;
	}
}


static
int
ar_multiply_complex
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	AR_DATA a, b, c, d, ac, bd, ad, bc, re, im;
	AR_TYPE reimtype1, reimtype2, temptype;
	int status, restat, imstat;

	/* (a + bi)*(c + di) = (ac - bd) + (ad + bc)i */

	status =  ar_decompose_complex ((ar_data*)&a, (ar_data*)&b, &reimtype1,
									opnd1, opnd1type);
	status |= ar_decompose_complex ((ar_data*)&c, (ar_data*)&d, &reimtype2,
									opnd2, opnd2type);

	status |= AR_multiply (&ac, &reimtype1, &a, &reimtype1, &c, &reimtype2);
	status |= AR_multiply (&bd, &reimtype1, &b, &reimtype1, &d, &reimtype2);
	status |= AR_multiply (&ad, &reimtype1, &a, &reimtype1, &d, &reimtype2);
	status |= AR_multiply (&bc, &reimtype1, &b, &reimtype1, &c, &reimtype2);

	restat = AR_subtract (&re, &reimtype1, &ac, &reimtype1, &bd, &reimtype1);
	imstat = AR_add      (&im, &reimtype1, &ad, &reimtype1, &bc, &reimtype1);
	status |= restat | imstat;

	status |= ar_compose_complex (result, &temptype,
								  (ar_data*)&re, (ar_data*)&im, &reimtype1);

	status &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
	status |= restat | imstat | AR_STAT_ZERO;
	return status;
}


/* General dispatch routine for multiplication */
int
AR_multiply
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;

	if (*resulttype != *opnd1type || *resulttype != *opnd2type)
		return AR_STAT_INVALID_TYPE;

	if (AR_CLASS (*resulttype) == AR_CLASS_INT)
		if (AR_INT_SIZE (*resulttype) != AR_INT_SIZE_8 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_16 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_32 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_46 &&
		    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_64) {
			return AR_STAT_INVALID_TYPE;
                }
		else {
                        int status;

                        status = ar_multiply_integer(result, resulttype,
                                                     opnd1, opnd1type,
                                                     opnd2, opnd2type);
			if (AR_SIGNEDNESS(*resulttype) == AR_UNSIGNED) {
                                /* turn off overflow flag for unsigned result */
				status &= ~AR_STAT_OVERFLOW;
                        }

                        return (status);
                }

	if (AR_CLASS (*resulttype) == AR_CLASS_FLOAT)
		if (AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX)
			return ar_multiply_complex (result, resulttype,
						    opnd1, opnd1type,
						    opnd2, opnd2type);
		else
			return ar_multiply_float (result, resulttype,
						  opnd1, opnd1type,
						  opnd2, opnd2type);

	return AR_STAT_INVALID_TYPE;
}


/* Integer division */
int
ar_divide_integer
		(ar_data *result1, const AR_TYPE *result1type,
		 ar_data *result2, const AR_TYPE *result2type,
   const ar_data *opnd1,   const AR_TYPE *opnd1type,
   const ar_data *opnd2,   const AR_TYPE *opnd2type) {

	ar_data dividend, divisor, accum, shiftcount, temp;
	ar_data lzdividend, lzdivisor;

	AR_TYPE type8  = AR_Int_8_U;
	AR_TYPE type16 = AR_Int_16_U;
	AR_TYPE type32 = AR_Int_32_U;
	AR_TYPE type64 = AR_Int_64_U;

	int opnd1sign, opnd2sign;
	int seqcount;

	switch (AR_INT_SIZE (*opnd1type)) {
	case AR_INT_SIZE_8:
		ZERO_INT8_UPPER(result1);
		ZERO_INT8_UPPER(result2);

		/* Check for division by zero */
		if (IS_INT8_ZERO(opnd2)) {
			ZERO_INT8(result1);
			ZERO_INT8(result2);
			return AR_STAT_OVERFLOW;
		}

		if (IS_INT8_ZERO(opnd1)) {
			ZERO_INT8(result1);
			ZERO_INT8(result2);
			return AR_STAT_ZERO;
		}

		opnd1sign = INT8_SIGN(opnd1) != 0;
		opnd2sign = INT8_SIGN(opnd2) != 0;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (&dividend, opnd1type,
					   opnd1, opnd1type);
		else
			dividend = *opnd1;

		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED && opnd2sign)
			ar_negate_integer (&divisor, opnd2type,
					   opnd2, opnd2type);
		else
			divisor = *opnd2;

		ZERO_INT8_UPPER(&dividend);
		ZERO_INT8_UPPER(&divisor );
		ZERO_INT8_UPPER(&temp    );

		ZERO_INT8_ALL(&accum);

		/* Compute difference in leading zero counts */
		AR_leadz ((AR_DATA*)&lzdividend, &type8,
			  (AR_DATA*)&dividend, &type8);
		AR_leadz ((AR_DATA*)&lzdivisor, &type8,
			  (AR_DATA*)&divisor, &type8);
		ar_subtract_integer (&shiftcount, &type8, (int *) 0,
				     &lzdivisor, &type8,
				     &lzdividend, &type8);

		if (!INT8_SIGN(&shiftcount)) {
			if (shiftcount.ar_i8.part5 & 0xF8)
				ar_internal_error (2001, __FILE__, __LINE__);
			seqcount = shiftcount.ar_i8.part5 + 1;
			AR_shiftl ((AR_DATA*)&temp, &type8,
				   (AR_DATA*)&divisor, &type8,
				   (AR_DATA*)&shiftcount, &type8);
			divisor = temp;

			shiftcount.ar_i8.part5 = 1;

			do {
				AR_shiftl ((AR_DATA*)&temp, &type8,
					   (AR_DATA*)&accum, &type8,
					   (AR_DATA*)&shiftcount, &type8);
				accum = temp;
				ar_subtract_integer (&temp, &type8, (int *)0,
						     &dividend, &type8,
						     &divisor, &type8);
				if (!INT8_SIGN(&temp)) {
					accum.ar_i8.part5 |= 1;
					dividend = temp;
				}
				AR_shiftr ((AR_DATA*)&temp, &type8,
					   (AR_DATA*)&divisor, &type8,
					   (AR_DATA*)&shiftcount, &type8);
				divisor = temp;

			} while (--seqcount > 0);
		}

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
		    opnd1sign != opnd2sign)
			ar_negate_integer (result1, result1type,
					   &accum, &type8);
		else
			result1->ar_i64 = accum.ar_i64;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (result2, result2type,
					   &dividend, &type8);
		else
			result2->ar_i64 = dividend.ar_i64;
		break;

	case AR_INT_SIZE_16:
		ZERO_INT16_UPPER(result1);
		ZERO_INT16_UPPER(result2);

		/* Check for division by zero */
		if (IS_INT16_ZERO(opnd2)) {
			ZERO_INT16(result1);
			ZERO_INT16(result2);
			return AR_STAT_OVERFLOW;
		}

		if (IS_INT16_ZERO(opnd1)) {
			ZERO_INT16(result1);
			ZERO_INT16(result2);
			return AR_STAT_ZERO;
		}

		opnd1sign = INT16_SIGN(opnd1) != 0;
		opnd2sign = INT16_SIGN(opnd2) != 0;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (&dividend, opnd1type,
					   opnd1, opnd1type);
		else
			dividend = *opnd1;

		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED && opnd2sign)
			ar_negate_integer (&divisor, opnd2type,
					   opnd2, opnd2type);
		else
			divisor = *opnd2;

		ZERO_INT16_UPPER(&dividend);
		ZERO_INT16_UPPER(&divisor );
		ZERO_INT16_UPPER(&temp    );

		ZERO_INT16_ALL(&accum);

		/* Compute difference in leading zero counts */
		AR_leadz ((AR_DATA*)&lzdividend, &type16,
			  (AR_DATA*)&dividend, &type16);
		AR_leadz ((AR_DATA*)&lzdivisor, &type16,
			  (AR_DATA*)&divisor, &type16);
		ar_subtract_integer (&shiftcount, &type16, (int *) 0,
				     &lzdivisor, &type16,
				     &lzdividend, &type16);

		if (!INT16_SIGN(&shiftcount)) {
			if (shiftcount.ar_i64.part4 & 0xFFF0)
				ar_internal_error (2001, __FILE__, __LINE__);
			seqcount = shiftcount.ar_i64.part4 + 1;
			AR_shiftl ((AR_DATA*)&temp, &type16,
				   (AR_DATA*)&divisor, &type16,
				   (AR_DATA*)&shiftcount, &type16);
			divisor = temp;

			shiftcount.ar_i64.part4 = 1;

			do {
				AR_shiftl ((AR_DATA*)&temp, &type16,
					   (AR_DATA*)&accum, &type16,
					   (AR_DATA*)&shiftcount, &type16);
				accum = temp;
				ar_subtract_integer (&temp, &type16, (int *)0,
						     &dividend, &type16,
						     &divisor, &type16);
				if (!INT16_SIGN(&temp)) {
					accum.ar_i64.part4 |= 1;
					dividend = temp;
				}
				AR_shiftr ((AR_DATA*)&temp, &type16,
					   (AR_DATA*)&divisor, &type16,
					   (AR_DATA*)&shiftcount, &type16);
				divisor = temp;

			} while (--seqcount > 0);
		}

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
		    opnd1sign != opnd2sign)
			ar_negate_integer (result1, result1type,
					   &accum, &type16);
		else
			result1->ar_i64 = accum.ar_i64;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (result2, result2type,
					   &dividend, &type16);
		else
			result2->ar_i64 = dividend.ar_i64;
		break;

	case AR_INT_SIZE_32:
		ZERO_INT32_UPPER(result1);
		ZERO_INT32_UPPER(result2);

		/* Check for division by zero */
		if (IS_INT32_ZERO(opnd2)) {
			ZERO_INT32(result1);
			ZERO_INT32(result2);
			return AR_STAT_OVERFLOW;
		}

		if (IS_INT32_ZERO(opnd1)) {
			ZERO_INT32(result1);
			ZERO_INT32(result2);
			return AR_STAT_ZERO;
		}

		opnd1sign = INT32_SIGN(opnd1) != 0;
		opnd2sign = INT32_SIGN(opnd2) != 0;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (&dividend, opnd1type,
					   opnd1, opnd1type);
		else
			dividend = *opnd1;

		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED && opnd2sign)
			ar_negate_integer (&divisor, opnd2type,
					   opnd2, opnd2type);
		else
			divisor = *opnd2;

		ZERO_INT32_UPPER(&dividend);
		ZERO_INT32_UPPER(&divisor );
		ZERO_INT32_UPPER(&temp    );

		ZERO_INT32_ALL(&accum);

		/* Compute difference in leading zero counts */
		AR_leadz ((AR_DATA*)&lzdividend, &type32,
			  (AR_DATA*)&dividend, &type32);
		AR_leadz ((AR_DATA*)&lzdivisor, &type32,
			  (AR_DATA*)&divisor, &type32);
		ar_subtract_integer (&shiftcount, &type32, (int *) 0,
				     &lzdivisor, &type32,
				     &lzdividend, &type32);

		if (!INT32_SIGN(&shiftcount)) {
			if (shiftcount.ar_i64.part3 |
			    shiftcount.ar_i64.part4 & 0xFFE0)
				ar_internal_error (2001, __FILE__, __LINE__);
			seqcount = shiftcount.ar_i64.part4 + 1;
			AR_shiftl ((AR_DATA*)&temp, &type32,
				   (AR_DATA*)&divisor, &type32,
				   (AR_DATA*)&shiftcount, &type32);
			divisor = temp;

			shiftcount.ar_i64.part3 = 0;
			shiftcount.ar_i64.part4 = 1;

			do {
				AR_shiftl ((AR_DATA*)&temp, &type32,
					   (AR_DATA*)&accum, &type32,
					   (AR_DATA*)&shiftcount, &type32);
				accum = temp;
				ar_subtract_integer (&temp, &type32, (int *)0,
						     &dividend, &type32,
						     &divisor, &type32);
				if (!INT32_SIGN(&temp)) {
					accum.ar_i64.part4 |= 1;
					dividend = temp;
				}
				AR_shiftr ((AR_DATA*)&temp, &type32,
					   (AR_DATA*)&divisor, &type32,
					   (AR_DATA*)&shiftcount, &type32);
				divisor = temp;

			} while (--seqcount > 0);
		}

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
		    opnd1sign != opnd2sign)
			ar_negate_integer (result1, result1type,
					   &accum, &type32);
		else
			result1->ar_i64 = accum.ar_i64;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (result2, result2type,
					   &dividend, &type32);
		else
			result2->ar_i64 = dividend.ar_i64;
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/* Check for division by zero */
		if (IS_INT64_ZERO(opnd2)) {
			ZERO_INT64(result1);
			ZERO_INT64(result2);
			return AR_STAT_OVERFLOW;
		}

		if (IS_INT64_ZERO(opnd1)) {
			ZERO_INT64(result1);
			ZERO_INT64(result2);
			return AR_STAT_ZERO;
		}

		opnd1sign = INT64_SIGN(opnd1) != 0;
		opnd2sign = INT64_SIGN(opnd2) != 0;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (&dividend, opnd1type,
					   opnd1, opnd1type);
		else
			dividend = *opnd1;

		if (AR_SIGNEDNESS (*opnd2type) == AR_SIGNED && opnd2sign)
			ar_negate_integer (&divisor, opnd2type, opnd2,
					   opnd2type);
		else
			divisor = *opnd2;

		ZERO_INT64_ALL(&accum);

		/* Compute difference in leading zero counts */
		AR_leadz ((AR_DATA*)&lzdividend, &type64,
			  (AR_DATA*)&dividend, &type64);
		AR_leadz ((AR_DATA*)&lzdivisor, &type64,
			  (AR_DATA*)&divisor, &type64);
		ar_subtract_integer (&shiftcount, &type64, (int *) 0,
				     &lzdivisor, &type64,
				     &lzdividend, &type64);

		if (!INT64_SIGN(&shiftcount)) {
			if (shiftcount.ar_i64.part1 |
			    shiftcount.ar_i64.part2 |
			    shiftcount.ar_i64.part3 |
			    shiftcount.ar_i64.part4 & 0xFFC0)
				ar_internal_error (2001, __FILE__, __LINE__);
			seqcount = shiftcount.ar_i64.part4 + 1;
			AR_shiftl ((AR_DATA*)&temp, &type64,
				   (AR_DATA*)&divisor, &type64,
				   (AR_DATA*)&shiftcount, &type64);
			divisor = temp;

			shiftcount.ar_i64.part1 = shiftcount.ar_i64.part2 =
				shiftcount.ar_i64.part3 = 0;
			shiftcount.ar_i64.part4 = 1;

			do {
				AR_shiftl ((AR_DATA*)&temp, &type64,
					   (AR_DATA*)&accum, &type64,
					   (AR_DATA*)&shiftcount, &type64);
				accum = temp;
				ar_subtract_integer (&temp, &type64,
						     (int *)0,
						     &dividend, &type64,
						     &divisor, &type64);
				if (!INT64_SIGN(&temp)) {
					accum.ar_i64.part4 |= 1;
					dividend = temp;
				}
				AR_shiftr ((AR_DATA*)&temp, &type64,
					   (AR_DATA*)&divisor, &type64,
					   (AR_DATA*)&shiftcount, &type64);
				divisor = temp;

			} while (--seqcount > 0);
		}

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED &&
		    opnd1sign != opnd2sign)
			ar_negate_integer (result1, result1type,
					   &accum, &type64);
		else
			result1->ar_i64 = accum.ar_i64;

		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED && opnd1sign)
			ar_negate_integer (result2, result2type,
					   &dividend, &type64);
		else
			result2->ar_i64 = dividend.ar_i64;
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return AR_STAT_OK;
}


static
int
ar_divide_float
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd1,  const AR_TYPE *opnd1type,
   const ar_data *opnd2,  const AR_TYPE *opnd2type) {

	int status;

	ar_data	tmp1, tmp2;

	switch (*opnd1type) {
	case AR_Float_Cray1_64:
	case AR_Float_Cray1_64_F:
		return ar_cfdiv64 (&result->ar_f64,
				   &opnd1->ar_f64, &opnd2->ar_f64,
				   ROUND_MODE (*opnd1type));
	case AR_Float_Cray1_128:
		return ar_cfdiv128 (&result->ar_f128,
				    &opnd1->ar_f128, &opnd2->ar_f128,
				    ROUND_MODE (*opnd1type));
	case AR_Float_IEEE_NR_32:
	case AR_Float_IEEE_ZE_32:
	case AR_Float_IEEE_UP_32:
	case AR_Float_IEEE_DN_32:
		return ar_ifdiv32 (&result->ar_ieee32,
				   &opnd1->ar_ieee32, &opnd2->ar_ieee32,
				   ROUND_MODE (*opnd1type));
	case AR_Float_IEEE_NR_64:
	case AR_Float_IEEE_ZE_64:
	case AR_Float_IEEE_UP_64:
	case AR_Float_IEEE_DN_64:
		return ar_ifdiv64 (&result->ar_ieee64,
				   &opnd1->ar_ieee64, &opnd2->ar_ieee64,
				   ROUND_MODE (*opnd1type));
	case AR_Float_IEEE_NR_128:
	case AR_Float_IEEE_ZE_128:
	case AR_Float_IEEE_UP_128:
	case AR_Float_IEEE_DN_128:
		return ar_ifdiv128 (&result->ar_ieee128,
				   &opnd1->ar_ieee128, &opnd2->ar_ieee128,
				   ROUND_MODE (*opnd1type));
	default:
		return AR_STAT_INVALID_TYPE;
	}
}


/* General dispatch routine for division */
int
AR_divide
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;

	int status, restat, imstat;
	ar_data modresult, real1, imag1, real2, imag2, realq, imagq;
	AR_TYPE reimtype, scrtype;

	if (*resulttype == *opnd1type &&
	    AR_CLASS (*resulttype) == AR_CLASS_FLOAT &&
	    AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX) {
		status = ar_decompose_complex (&real1, &imag1, &reimtype,
						opnd1, opnd1type);
		if (reimtype == *opnd2type) {
			/* COMPLEX/REAL - short sequence */
			restat = ar_divide_float (&realq, &reimtype,
					    &real1, &reimtype,
					    opnd2, opnd2type);
			imstat = ar_divide_float (&imagq, &reimtype,
					    &imag1, &reimtype,
					    opnd2, opnd2type);
			status |= ar_compose_complex (result, &scrtype,
						      &realq, &imagq,
						      &reimtype);
			status |= restat | imstat;
			status &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
			status |= restat & imstat & AR_STAT_ZERO;
			return status;
		}
	}

	if (*resulttype != *opnd1type || *resulttype != *opnd2type)
		return AR_STAT_INVALID_TYPE;

	if (AR_CLASS (*opnd1type) == AR_CLASS_INT) {
		if (AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_8 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_16 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_32 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_46 &&
		    AR_INT_SIZE (*opnd1type) != AR_INT_SIZE_64)
			return AR_STAT_INVALID_TYPE;
		status = ar_divide_integer (result, resulttype,
					    &modresult, resulttype,
					    opnd1, opnd1type,
					    opnd2, opnd2type);
		if (AR_SIGNEDNESS (*opnd1type) == AR_SIGNED) {
			if ((*opnd1type == AR_Int_46_S &&
			     INT_OVERFLOWS_46_BITS (*opnd1)) ||
			    (*opnd2type == AR_Int_46_S &&
			     INT_OVERFLOWS_46_BITS (*opnd2)) ||
			    (*resulttype == AR_Int_46_S &&
			     INT_OVERFLOWS_46_BITS (*result)))
				status |= AR_STAT_OVERFLOW;
			switch (AR_INT_SIZE (*opnd1type)) {
			case AR_INT_SIZE_8:
				if (!IS_INT8_ZERO(result) &&
				    (INT8_SIGN(opnd1) ^
				     INT8_SIGN(opnd2) ^
				     INT8_SIGN(result)))
					/* Nonzero result with wrong sign */
					status |= AR_STAT_OVERFLOW;
				break;

			case AR_INT_SIZE_16:
				if (!IS_INT16_ZERO(result) &&
				    (INT16_SIGN(opnd1) ^
				     INT16_SIGN(opnd2) ^
				     INT16_SIGN(result)))
					/* Nonzero result with wrong sign */
					status |= AR_STAT_OVERFLOW;
				break;

			case AR_INT_SIZE_32:
				if (!IS_INT32_ZERO(result) &&
				    (INT32_SIGN(opnd1) ^
				     INT32_SIGN(opnd2) ^
				     INT32_SIGN(result)))
					/* Nonzero result with wrong sign */
					status |= AR_STAT_OVERFLOW;
				break;

			case AR_INT_SIZE_46:
			case AR_INT_SIZE_64:
				if (!IS_INT64_ZERO(result) &&
				    (INT64_SIGN(opnd1) ^
				     INT64_SIGN(opnd2) ^
				     INT64_SIGN(result)))
					/* Nonzero result with wrong sign */
					status |= AR_STAT_OVERFLOW;
				break;

			default:
				return (AR_STAT_INVALID_TYPE);
			}
		}

		return status |= AR_status ((AR_DATA*)result, resulttype);
	}

	if (AR_CLASS (*opnd1type) == AR_CLASS_FLOAT)
		if (AR_FLOAT_IS_COMPLEX (*opnd1type) == AR_FLOAT_COMPLEX)
			return ar_divide_complex (result, resulttype,
						  opnd1, opnd1type,
						  opnd2, opnd2type);
		else
			return ar_divide_float (result, resulttype,
						opnd1, opnd1type,
						opnd2, opnd2type);

	return AR_STAT_INVALID_TYPE;
}

/* Mod */
int
AR_modulo
		(AR_DATA *result, const AR_TYPE *resulttype,
   const AR_DATA *opnd1,  const AR_TYPE *opnd1type,
   const AR_DATA *opnd2,  const AR_TYPE *opnd2type) {

	/* NOTE:
	 * AR_modulo is to be removed when all usage has been changed to
	 * AR_mod because 'modulo' is a Fortran 90 intrinsic which returns
	 * different results than the Fortran 77 mod intrinsic.  This will
	 * avoid future confusion and maintenance headaches (hopefully).
	 * (AR_Modulo is currently the interface to the Fortran 90 intrinsic.)
	 */
	return AR_mod(result, resulttype, opnd1, opnd1type, opnd2, opnd2type);
}

int
AR_mod	(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;

	int status;
	ar_data divresult;

	if (*resulttype != *opnd1type ||
	    *resulttype != *opnd2type ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_8 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_16 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_32 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_46 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_64)
		return AR_STAT_INVALID_TYPE;

	status = ar_divide_integer (&divresult, resulttype,
				    result, resulttype,
				    opnd1, opnd1type,
				    opnd2, opnd2type);

	status |= AR_status ((AR_DATA*)result, resulttype);

	if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
            !(status & AR_STAT_ZERO)) {
		switch (AR_INT_SIZE (*opnd1type)) {
		case AR_INT_SIZE_8:
			if (INT8_SIGN(result) ^ INT8_SIGN(opnd1))
				status |= AR_STAT_OVERFLOW;
			break;

		case AR_INT_SIZE_16:
			if (INT16_SIGN(result) ^ INT16_SIGN(opnd1))
				status |= AR_STAT_OVERFLOW;
			break;

		case AR_INT_SIZE_32:
			if (INT32_SIGN(result) ^ INT32_SIGN(opnd1))
				status |= AR_STAT_OVERFLOW;
			break;

		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			if (INT64_SIGN(result) ^ INT64_SIGN(opnd1))
				status |= AR_STAT_OVERFLOW;
			break;
		}
	}

	return status;
}


/* Pointer + integer computation */
int
AR_add_ptr_int
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type,
   const AR_DATA *op3, const AR_TYPE *opnd3type) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;
	ar_data* opnd3  = (ar_data*)op3;

	ar_data ptropnd, sizeopnd, intopnd, signextend;
	AR_TYPE sint64 = AR_Int_64_S;
	int status;

	if (*resulttype != *opnd1type ||
	    AR_CLASS (*opnd1type) != AR_CLASS_POINTER ||
	    AR_CLASS (*opnd2type) != AR_CLASS_INT ||
	    AR_CLASS (*opnd3type) != AR_CLASS_INT ||
	    AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_FCTN)
		return AR_STAT_INVALID_TYPE;

        status = ar_convert_to_integral(&sizeopnd, &sint64, opnd2, opnd2type);
        if (status & ~AR_STAT_ZERO)
                /* the bit offset must be representable as a
                   non-negative 64-bit signed int */
                return AR_STAT_UNDEFINED;

        status = ar_convert_to_integral(&intopnd, &sint64, opnd3, opnd3type);
        status |= ar_multiply_integer(&intopnd, &sint64, &intopnd, &sint64,
                                                         &sizeopnd, &sint64);

        if (INT64_SIGN(&intopnd))
           signextend.ar_i64.part1 = signextend.ar_i64.part2 =
           signextend.ar_i64.part3 = signextend.ar_i64.part4 = 0xffff;
        else
           signextend.ar_i64.part1 = signextend.ar_i64.part2 =
           signextend.ar_i64.part3 = signextend.ar_i64.part4 = 0;

	if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_CHAR) {
		/* Turn char pointer into byte pointer */
		ar_dblshift(&ptropnd, &sint64, opnd1, opnd1, 128-3);

                if (intopnd.ar_i64.part4 & 0x7)
                        /* bit offset must be mappable to a byte offset */
                        return AR_STAT_UNDEFINED;

                /* turn the bit size into byte size */
                ar_dblshift(&intopnd, &sint64, &signextend, &intopnd, 3);
	}
        else if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_WORD) {
                ptropnd = *opnd1;

                if (intopnd.ar_i64.part4 & 0x3f)
                        /* bit offset must be mappable to a word offset */
                        return AR_STAT_UNDEFINED;

                /* turn the bit size into word size */
	        ar_dblshift(&intopnd, &sint64, &signextend, &intopnd, 6);
	}
        else {
                ptropnd = *opnd1;

                if (intopnd.ar_i64.part4 & 0x7)
                        /* bit offset must be mappable to a byte offset */
                        return AR_STAT_UNDEFINED;

                /* turn the bit size into byte size */
                ar_dblshift(&intopnd, &sint64, &signextend, &intopnd, 3);
        }

        status |= ar_add_integer(result, &sint64, &ptropnd, &sint64,
                                                  &intopnd, &sint64);
        
	if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_CHAR) {
		/* Restore char pointer */
		ar_dblshift (result, &sint64, result, result, 3);
	}
        else if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_WORD) {
                if ((AR_POINTER_SIZE (*opnd1type) == AR_POINTER_64) &&
                    (result->ar_i64.part1 & 0xff00) != 0)
                        status |= AR_STAT_OVERFLOW;
                else if ((AR_POINTER_SIZE (*opnd1type) == AR_POINTER_32) &&
                         (result->ar_i64.part1 != 0 ||
                          result->ar_i64.part2 != 0))
                        status |= AR_STAT_OVERFLOW;
                else if ((AR_POINTER_SIZE (*opnd1type) == AR_POINTER_24) &&
                         (result->ar_i64.part1 != 0 ||
                          result->ar_i64.part2 != 0 ||
                          (result->ar_i64.part3 & 0xff00) != 0))
                        status |= AR_STAT_OVERFLOW;
        }

	ar_clear_unused_bits (result, resulttype);
    status = (status & AR_STAT_OVERFLOW) | AR_status((AR_DATA*)result, resulttype);

	return status;
}


/* Pointer - pointer computation */
int
AR_subtract_ptr_ptr
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type,
   const AR_DATA *op3, const AR_TYPE *opnd3type) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;
	ar_data* opnd3  = (ar_data*)op3;

	ar_data ptr1opnd, ptr2opnd, sizeopnd, modresult;
	AR_TYPE sint64 = AR_Int_64_S;
	int status;

	if (*opnd1type != *opnd3type ||
	    AR_CLASS (*opnd1type) != AR_CLASS_POINTER ||
	    AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_FCTN)
		return AR_STAT_INVALID_TYPE;

        if (AR_CLASS (*opnd2type) != AR_CLASS_INT ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

        status = ar_convert_to_integral(&sizeopnd, &sint64, opnd2, opnd2type);
        /* the bit offset must be representable as a non-negative 64-bit
           signed int */
        if (status & ~AR_STAT_ZERO)
                return AR_STAT_UNDEFINED;

	if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_CHAR) {
		/* Turn char pointers into byte pointers */
		ar_dblshift(&ptr1opnd, &sint64, opnd1, opnd1, 128-3);
		ar_dblshift(&ptr2opnd, &sint64, opnd3, opnd3, 128-3);

                /* bit offset must be mappable to a byte offset */
                if (sizeopnd.ar_i64.part4 & 0x7)
                        return AR_STAT_UNDEFINED;
	}
        else if (AR_POINTER_FORMAT (*opnd1type) == AR_POINTER_WORD) {
		/* Turn word pointers into byte pointers */
		ar_dblshift(&ptr1opnd, &sint64, (const ar_data*)&AR_const_zero, opnd1, 128-3);
		ar_dblshift(&ptr2opnd, &sint64, (const ar_data*)&AR_const_zero, opnd3, 128-3);

                /* bit offset must be mappable to a word offset */
                if (sizeopnd.ar_i64.part4 & 0x3f)
                        return AR_STAT_UNDEFINED;
	}
        else {
                ptr1opnd = *opnd1;
                ptr2opnd = *opnd3;

                /* bit offset must be mappable to a byte offset */
                if (sizeopnd.ar_i64.part4 & 0x7)
                        return AR_STAT_UNDEFINED;
        }

        /* turn the bit size into byte size */
        ar_dblshift(&sizeopnd, &sint64, (const ar_data*)&AR_const_zero, &sizeopnd, 3);

        status = ar_subtract_integer(result, &sint64, (int *)0,
                                     &ptr1opnd, &sint64, &ptr2opnd, &sint64);
        status &= AR_STAT_OVERFLOW;  /* keep only the overflow flag */
        status |= ar_divide_integer(result, &sint64, &modresult, &sint64,
                                    result, &sint64, &sizeopnd, &sint64);

        if (modresult.ar_i64.part1 | modresult.ar_i64.part2 |
            modresult.ar_i64.part3 | modresult.ar_i64.part4) {
                 status |= AR_STAT_INEXACT;
        }

	return status;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: math.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
