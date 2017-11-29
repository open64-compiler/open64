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


/* Bitwise operations */

#include "arith.internal.h"

#define bitoper(name,opr)						\
int									\
name (AR_DATA *res, const AR_TYPE *resulttype,				\
      const AR_DATA *op1, const AR_TYPE *opnd1type,			\
      const AR_DATA *op2, const AR_TYPE *opnd2type) {			\
	ar_data* result = (ar_data*)res;				\
	ar_data* opnd1  = (ar_data*)op1;				\
	ar_data* opnd2  = (ar_data*)op2;				\
									\
	if (*opnd1type != *resulttype ||				\
	    *opnd2type != *resulttype ||				\
	    AR_CLASS (*resulttype) != AR_CLASS_INT)			\
		return (AR_STAT_INVALID_TYPE);				\
									\
	switch (AR_INT_SIZE (*opnd1type)) {				\
	case AR_INT_SIZE_8:						\
		ZERO_INT8_UPPER(result);				\
		result->ar_i8.part5 = opnd1->ar_i8.part5 opr		\
				      opnd2->ar_i8.part5;		\
		break;							\
									\
	case AR_INT_SIZE_16:						\
		ZERO_INT16_UPPER(result);				\
		result->ar_i64.part4 = opnd1->ar_i64.part4 opr		\
				       opnd2->ar_i64.part4;		\
		break;							\
									\
	case AR_INT_SIZE_32:						\
		ZERO_INT32_UPPER(result);				\
		result->ar_i64.part3 = opnd1->ar_i64.part3 opr		\
				       opnd2->ar_i64.part3;		\
		result->ar_i64.part4 = opnd1->ar_i64.part4 opr		\
				       opnd2->ar_i64.part4;		\
		break;							\
									\
	case AR_INT_SIZE_46:						\
	case AR_INT_SIZE_64:						\
		result->ar_i64.part1 = opnd1->ar_i64.part1 opr		\
				       opnd2->ar_i64.part1;		\
		result->ar_i64.part2 = opnd1->ar_i64.part2 opr		\
				       opnd2->ar_i64.part2;		\
		result->ar_i64.part3 = opnd1->ar_i64.part3 opr		\
				       opnd2->ar_i64.part3;		\
		result->ar_i64.part4 = opnd1->ar_i64.part4 opr		\
				       opnd2->ar_i64.part4;		\
		break;							\
									\
	default:							\
		return (AR_STAT_INVALID_TYPE);				\
	}								\
									\
	return AR_status ((AR_DATA*)result, resulttype);		\
}

bitoper (AR_bitor, |)
bitoper (AR_bitand, &)
bitoper (AR_bitxor, ^)

#undef bitoper


int
AR_bitcomplement (AR_DATA *res, const AR_TYPE *resulttype,
		  const AR_DATA *op1, const AR_TYPE *opnd1type) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;

	if (*resulttype != *opnd1type ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT)
		return (AR_STAT_INVALID_TYPE);

	switch (AR_INT_SIZE (*opnd1type)) {
	case AR_INT_SIZE_8:
		result->ar_i8.part5 = ~opnd1->ar_i8.part5;
		break;

	case AR_INT_SIZE_16:
		result->ar_i64.part4 = ~opnd1->ar_i64.part4;
		break;

	case AR_INT_SIZE_32:
		result->ar_i64.part3 = ~opnd1->ar_i64.part3;
		result->ar_i64.part4 = ~opnd1->ar_i64.part4;
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		result->ar_i64.part1 = ~opnd1->ar_i64.part1;
		result->ar_i64.part2 = ~opnd1->ar_i64.part2;
		result->ar_i64.part3 = ~opnd1->ar_i64.part3;
		result->ar_i64.part4 = ~opnd1->ar_i64.part4;
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return AR_status ((AR_DATA*)result, resulttype);
}


/* Perform a right circular shift of (opnd1,opnd2) and return the
 * rightmost AR_INT_SIZE bits of the result.
 */
void
ar_dblshift (ar_data *result, const AR_TYPE *resulttype,
	     const ar_data *opnd1,
	     const ar_data *opnd2,
	     int shiftcount) {

	int i, sc, lsc;
	unsigned int chunk [8];

	switch (AR_INT_SIZE (*resulttype)) {
	case AR_INT_SIZE_8:
		if (shiftcount < 0 || shiftcount > 16)
			ar_internal_error (2000, __FILE__, __LINE__);

		/* Split the 16-bit concatenated argument into two chunks of
		 * eight bits, and perform the shift (mod 8) on the chunks.
		 */
		sc = shiftcount % 8;
		lsc = 8 - sc;
		chunk [0] = (opnd1->ar_i8.part5 >> sc) |
			    (opnd2->ar_i8.part5 << lsc);
		chunk [1] = (opnd2->ar_i8.part5 >> sc) |
			    (opnd1->ar_i8.part5 << lsc);

		/* Complete the shift by moving the chunk corresponding to the
		 * rightmost 8 bits into the result.
		 */
		sc = 2 - (shiftcount / 8);
		sc += 1;			/* get rightmost chunk */

		ZERO_INT8_UPPER(result);
		result->ar_i8.part5 = chunk [sc   % 2] & 0xFF;
		break;

	case AR_INT_SIZE_16:
		if (shiftcount < 0 || shiftcount > 32)
			ar_internal_error (2000, __FILE__, __LINE__);

		/* Split the 32-bit concatenated argument into two chunks of
		 * sixteen bits, and perform the shift (mod 16) on the chunks.
		 */
		sc = shiftcount % 16;
		lsc = 16 - sc;
		chunk [0] = (opnd1->ar_i64.part4 >> sc) |
			    (opnd2->ar_i64.part4 << lsc);
		chunk [1] = (opnd2->ar_i64.part4 >> sc) |
			    (opnd1->ar_i64.part4 << lsc);

		/* Complete the shift by moving the chunk corresponding to the
		 * rightmost 16 bits into the result.
		 */
		sc = 2 - (shiftcount / 16);
		sc += 1;			/* get rightmost chunk */

		ZERO_INT16_UPPER(result);
		result->ar_i64.part4 = chunk [sc   % 2] & 0xFFFF;
		break;

	case AR_INT_SIZE_32:
		if (shiftcount < 0 || shiftcount > 64)
			ar_internal_error (2000, __FILE__, __LINE__);

		/* Split the 64-bit concatenated argument into four chunks of
		 * sixteen bits, and perform the shift (mod 16) on the chunks.
		 */
		sc = shiftcount % 16;
		lsc = 16 - sc;
		chunk [0] = (opnd1->ar_i64.part3 >> sc) |
			    (opnd2->ar_i64.part4 << lsc);
		chunk [1] = (opnd1->ar_i64.part4 >> sc) |
			    (opnd1->ar_i64.part3 << lsc);
		chunk [2] = (opnd2->ar_i64.part3 >> sc) |
			    (opnd1->ar_i64.part4 << lsc);
		chunk [3] = (opnd2->ar_i64.part4 >> sc) |
			    (opnd2->ar_i64.part3 << lsc);

		/* Complete the shift by moving the chunks corresponding to the
		 * rightmost 32 bits into the result.
		 */
		sc = 4 - (shiftcount / 16);
		sc += 2;			/* get rightmost 2 chunks */

		ZERO_INT32_UPPER(result);
		result->ar_i64.part3 = chunk [sc++ % 4] & 0xFFFF;
		result->ar_i64.part4 = chunk [sc   % 4] & 0xFFFF;
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		if (shiftcount < 0 || shiftcount > 128)
			ar_internal_error (2000, __FILE__, __LINE__);

		/* Split the 128-bit concatenated argument into eight chunks of
		 * sixteen bits, and perform the shift (mod 16) on the chunks.
		 */
		sc = shiftcount % 16;
		lsc = 16 - sc;
		chunk [0] = (opnd1->ar_i64.part1 >> sc) |
			    (opnd2->ar_i64.part4 << lsc);
		chunk [1] = (opnd1->ar_i64.part2 >> sc) |
			    (opnd1->ar_i64.part1 << lsc);
		chunk [2] = (opnd1->ar_i64.part3 >> sc) |
			    (opnd1->ar_i64.part2 << lsc);
		chunk [3] = (opnd1->ar_i64.part4 >> sc) |
			    (opnd1->ar_i64.part3 << lsc);
		chunk [4] = (opnd2->ar_i64.part1 >> sc) |
			    (opnd1->ar_i64.part4 << lsc);
		chunk [5] = (opnd2->ar_i64.part2 >> sc) |
			    (opnd2->ar_i64.part1 << lsc);
		chunk [6] = (opnd2->ar_i64.part3 >> sc) |
			    (opnd2->ar_i64.part2 << lsc);
		chunk [7] = (opnd2->ar_i64.part4 >> sc) |
			    (opnd2->ar_i64.part3 << lsc);

		/* Complete the shift by moving the chunks corresponding to the
		 * rightmost 64 bits into the result.
		 */
		sc = 8 - (shiftcount / 16);
		sc += 4;			/* get rightmost 4 chunks */

		result->ar_i64.part1 = chunk [sc++ % 8] & 0xFFFF;
		result->ar_i64.part2 = chunk [sc++ % 8] & 0xFFFF;
		result->ar_i64.part3 = chunk [sc++ % 8] & 0xFFFF;
		result->ar_i64.part4 = chunk [sc   % 8] & 0xFFFF;
		break;
	}
}


int
AR_dshiftl (AR_DATA *res, const AR_TYPE *resulttype,
	  const AR_DATA *op1, const AR_TYPE *opnd1type,
	  const AR_DATA *op2, const AR_TYPE *opnd2type,
	  const AR_DATA *cnt, const AR_TYPE *counttype) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;
	ar_data* count  = (ar_data*)cnt;

	if (*resulttype != *opnd1type ||
	    *resulttype != *opnd2type ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*counttype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*resulttype)) {
	case AR_INT_SIZE_8:
		/* Check for shift count not in [0,8] */
		if (count->ar_i8.part5 > 8) {
			ZERO_INT8(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd2, opnd1, 16 - count->ar_i8.part5);
		break;

	case AR_INT_SIZE_16:
		/* Check for shift count not in [0,16] */
		if (count->ar_i64.part4 > 16) {
			ZERO_INT16(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd2, opnd1, 32 - count->ar_i64.part4);
		break;

	case AR_INT_SIZE_32:
		/* Check for shift count not in [0,32] */
		if (count->ar_i64.part3 || count->ar_i64.part4 > 32) {
			ZERO_INT32(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd2, opnd1, 64 - count->ar_i64.part4);
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/* Check for shift count not in [0,64] */
		if (count->ar_i64.part1 | count->ar_i64.part2 |
		    count->ar_i64.part3 || count->ar_i64.part4 > 64) {
			ZERO_INT64(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd2, opnd1, 128 - count->ar_i64.part4);
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	return AR_status ((AR_DATA*)result, resulttype);
}


int
AR_dshiftr (AR_DATA *res, const AR_TYPE *resulttype,
	  const AR_DATA *op1, const AR_TYPE *opnd1type,
	  const AR_DATA *op2, const AR_TYPE *opnd2type,
	  const AR_DATA *cnt, const AR_TYPE *counttype) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd1  = (ar_data*)op1;
	ar_data* opnd2  = (ar_data*)op2;
	ar_data* count  = (ar_data*)cnt;

	if (*resulttype != *opnd1type ||
	    *resulttype != *opnd2type ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*counttype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*resulttype)) {
	case AR_INT_SIZE_8:
		/* Check for shift count not in [0,8] */
		if (count->ar_i8.part5 > 8) {
			ZERO_INT8(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd1, opnd2, count->ar_i8.part5);
		break;

	case AR_INT_SIZE_16:
		/* Check for shift count not in [0,16] */
		if (count->ar_i64.part4 > 16) {
			ZERO_INT16(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd1, opnd2, count->ar_i64.part4);
		break;

	case AR_INT_SIZE_32:
		/* Check for shift count not in [0,32] */
		if (count->ar_i64.part3 || count->ar_i64.part4 > 32) {
			ZERO_INT32(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd1, opnd2, count->ar_i64.part4);
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/* Check for shift count not in [0,64] */
		if (count->ar_i64.part1 | count->ar_i64.part2 |
		    count->ar_i64.part3 || count->ar_i64.part4 > 64) {
			ZERO_INT64(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     opnd1, opnd2, count->ar_i64.part4);
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	return AR_status ((AR_DATA*)result, resulttype);
}


int
AR_shiftl (AR_DATA *res, const AR_TYPE *resulttype,
	 const AR_DATA *opd, const AR_TYPE *opndtype,
	 const AR_DATA *cnt, const AR_TYPE *counttype) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;
	ar_data* count  = (ar_data*)cnt;

	ar_data signextend;

	if (*resulttype != *opndtype ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*counttype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*opndtype)) {
	case AR_INT_SIZE_8:
		/* Check for shift count not in [0,7] */
		if (count->ar_i8.part5 > 7) {
			ZERO_INT8(result);
			return AR_STAT_UNDEFINED;
		}

		ZERO_INT8(&signextend);

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, 16 - count->ar_i8.part5);
		break;

	case AR_INT_SIZE_16:
		/* Check for shift count not in [0,15] */
		if (count->ar_i64.part4 > 15) {
			ZERO_INT16(result);
			return AR_STAT_UNDEFINED;
		}

		ZERO_INT16(&signextend);

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, 32 - count->ar_i64.part4);
		break;

	case AR_INT_SIZE_32:
		/* Check for shift count not in [0,31] */
		if (count->ar_i64.part3 || count->ar_i64.part4 > 31) {
			ZERO_INT32(result);
			return AR_STAT_UNDEFINED;
		}

		ZERO_INT32(&signextend);

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, 64 - count->ar_i64.part4);
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/* Check for shift count not in [0,63] */
		if (count->ar_i64.part1 | count->ar_i64.part2 |
		    count->ar_i64.part3 || count->ar_i64.part4 > 63) {
			ZERO_INT64(result);
			return AR_STAT_UNDEFINED;
		}

		ZERO_INT64(&signextend);

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, 128 - count->ar_i64.part4);
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return AR_status ((AR_DATA*)result, resulttype);
}


int
AR_shiftr (AR_DATA *res, const AR_TYPE *resulttype,
	 const AR_DATA *opd, const AR_TYPE *opndtype,
	 const AR_DATA *cnt, const AR_TYPE *counttype) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;
	ar_data* count  = (ar_data*)cnt;

	int neg;
	ar_data signextend;

	if (*resulttype != *opndtype ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*counttype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*opndtype)) {
	case AR_INT_SIZE_8:
		/* Check for shift count not in [0,7] */
		if (count->ar_i8.part5 > 7) {
			ZERO_INT8(result);
			return AR_STAT_UNDEFINED;
		}

		if (AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED) {
			ZERO_INT8(&signextend);
		} else {
			neg = opnd->ar_i8.part5 >> 7;
			signextend.ar_i8.part5 = 0xFF & -neg;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, count->ar_i8.part5);
		break;

	case AR_INT_SIZE_16:
		/* Check for shift count not in [0,15] */
		if (count->ar_i64.part4 > 15) {
			ZERO_INT16(result);
			return AR_STAT_UNDEFINED;
		}

		if (AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED) {
			ZERO_INT16(&signextend);
		} else {
			neg = opnd->ar_i64.part4 >> 15;
			signextend.ar_i64.part4 = 0xFFFF & -neg;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, count->ar_i64.part4);
		break;

	case AR_INT_SIZE_32:
		/* Check for shift count not in [0,31] */
		if (count->ar_i64.part3 || count->ar_i64.part4 > 31) {
			ZERO_INT32(result);
			return AR_STAT_UNDEFINED;
		}

		if (AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED) {
			ZERO_INT32(&signextend);
		} else {
			neg = opnd->ar_i64.part3 >> 15;
			signextend.ar_i64.part3 = 0xFFFF & -neg;
			signextend.ar_i64.part4 = 0xFFFF & -neg;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, count->ar_i64.part4);
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/* Check for shift count not in [0,63] */
		if (count->ar_i64.part1 | count->ar_i64.part2 |
		    count->ar_i64.part3 || count->ar_i64.part4 > 63) {
			ZERO_INT64(result);
			return AR_STAT_UNDEFINED;
		}

		if (AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED) {
			ZERO_INT64(&signextend);
		} else {
			neg = opnd->ar_i64.part1 >> 15;
			signextend.ar_i64.part1 = 0xFFFF & -neg;
			signextend.ar_i64.part2 = 0xFFFF & -neg;
			signextend.ar_i64.part3 = 0xFFFF & -neg;
			signextend.ar_i64.part4 = 0xFFFF & -neg;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &signextend, opnd, count->ar_i64.part4);
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return AR_status ((AR_DATA*)result, resulttype);
}


int
AR_ishft (AR_DATA *res, const AR_TYPE *resulttype,
	 const AR_DATA *opd, const AR_TYPE *opndtype,
	 const AR_DATA *shft, const AR_TYPE *shifttype) {
	ar_data		*result = (ar_data*)res;
	ar_data		*opnd   = (ar_data*)opd;
	ar_data		*shift   = (ar_data*)shft;

	AR_HOST_SINT64	shift_count;
	int		is_right_shift;

	ar_data		zeros  = { 0x0000, 0x0000, 0x0000, 0x0000 };

	if (*resulttype != *opndtype ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*shifttype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*opndtype) == AR_INT_SIZE_24 ||
	    AR_INT_SIZE(*shifttype) == AR_INT_SIZE_24)
		return AR_STAT_INVALID_TYPE;

	ZERO_INT64(result);

	switch (AR_INT_SIZE (*opndtype)) {
	case AR_INT_SIZE_8:
		/*
		 * Shift has to be in [1,8].
		 */
		INT8_TO_HOST_SINT64(shift_count, shift);
		if (shift_count >= 0)
			is_right_shift = 0;
		else {
			is_right_shift = 1;
			shift_count = -shift_count;
		}

		if (shift_count < 0 || shift_count > 8)
			return AR_STAT_UNDEFINED;

		if (is_right_shift)
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (int) shift_count);
		else
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (16 - (int) shift_count));
		break;

	case AR_INT_SIZE_16:
		/*
		 * Shift has to be in [1,16].
		 */
		INT16_TO_HOST_SINT64(shift_count, shift);
		if (shift_count >= 0)
			is_right_shift = 0;
		else {
			is_right_shift = 1;
			shift_count = -shift_count;
		}

		if (shift_count < 0 || shift_count > 16)
			return AR_STAT_UNDEFINED;

		if (is_right_shift)
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (int) shift_count);
		else
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (32 - (int) shift_count));
		break;

	case AR_INT_SIZE_32:
		/*
		 * Shift has to be in [1,32].
		 */
		INT32_TO_HOST_SINT64(shift_count, shift);
		if (shift_count >= 0)
			is_right_shift = 0;
		else {
			is_right_shift = 1;
			shift_count = -shift_count;
		}

		if (shift_count < 0 || shift_count > 32)
			return AR_STAT_UNDEFINED;

		if (is_right_shift)
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (int) shift_count);
		else
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (64 - (int) shift_count));
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/*
		 * Shift has to be in [1,64].
		 */
		INT64_TO_HOST_SINT64(shift_count, shift);
		if (shift_count >= 0)
			is_right_shift = 0;
		else {
			is_right_shift = 1;
			shift_count = -shift_count;
		}

		if (shift_count < 0 || shift_count > 64)
			return AR_STAT_UNDEFINED;

		if (is_right_shift)
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (int) shift_count);
		else
			ar_dblshift (result, resulttype,
				     &zeros, opnd,
				     (128 - (int) shift_count));
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return AR_status ((AR_DATA*)result, resulttype);
}


int
AR_ishftc (AR_DATA *res, const AR_TYPE *resulttype,
	 const AR_DATA *opd, const AR_TYPE *opndtype,
	 const AR_DATA *shft, const AR_TYPE *shifttype,
	 const AR_DATA *sz, const AR_TYPE *sizetype) {
	ar_data		*result = (ar_data*)res;
	ar_data		*opnd   = (ar_data*)opd;
	ar_data		*shift  = (ar_data*)shft;
	ar_data		*size   = (ar_data*)sz;

	AR_HOST_SINT64	shift_count;
	AR_HOST_SINT64	shift_size;
	int		type_size;

	ar_data		ones8  = { 0x0000, 0x0000, 0x0000, 0x00FF };
	ar_data		ones16 = { 0x0000, 0x0000, 0x0000, 0xFFFF };
	ar_data		ones32 = { 0x0000, 0x0000, 0xFFFF, 0xFFFF };
	ar_data		ones64 = { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF };
	ar_data		*ones;
	ar_data		zeros  = { 0x0000, 0x0000, 0x0000, 0x0000 };

	ar_data		mask;
	ar_data		rotatee;
	ar_data		temp;

	if (*resulttype != *opndtype ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*shifttype) != AR_CLASS_INT ||
	    AR_CLASS (*sizetype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*opndtype) == AR_INT_SIZE_24 ||
	    AR_INT_SIZE(*shifttype) == AR_INT_SIZE_24 ||
	    AR_INT_SIZE(*sizetype) == AR_INT_SIZE_24)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*opndtype)) {
	case AR_INT_SIZE_8:
		INT8_TO_HOST_SINT64(shift_count, shift);
		INT8_TO_HOST_SINT64(shift_size, size);
		type_size = 8;
		ones      = &ones8;
		break;
	case AR_INT_SIZE_16:
		INT16_TO_HOST_SINT64(shift_count, shift);
		INT16_TO_HOST_SINT64(shift_size, size);
		type_size = 16;
		ones      = &ones16;
		break;
	case AR_INT_SIZE_32:
		INT32_TO_HOST_SINT64(shift_count, shift);
		INT32_TO_HOST_SINT64(shift_size, size);
		type_size = 32;
		ones      = &ones32;
		break;
	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		INT64_TO_HOST_SINT64(shift_count, shift);
		INT64_TO_HOST_SINT64(shift_size, size);
		type_size = 64;
		ones      = &ones64;
		break;
	default:
		return (AR_STAT_INVALID_TYPE);
	}

	/*
	 * A right rotation of "shift" is the same as a
	 * left one of "size" - "shift".
	 */
	if (shift_count < 0)
		shift_count = shift_size + shift_count;

	/*
	 * Shift_size has to be in [1, type_size], and
	 * shift_count has to be in [0, shift_size].
	 */
	if (shift_size  < 1 || shift_size  > type_size ||
	    shift_count < 0 || shift_count > shift_size)
		return AR_STAT_UNDEFINED;

	/*
	 * We now have shift_size in [1, type_size), and
	 * shift_count in [1, shift_size).
	 */

	/*
	 * Extract just the unchanged part, and put it in the
	 * result.  Throw away any insignificant high bits
	 * from the operand.
	 */
	ar_dblshift(&mask, resulttype, ones, &zeros,
		    (type_size - (int) shift_size));
	(void) AR_bitand((AR_DATA*) result, resulttype,
			 (AR_DATA*) opnd, resulttype,
			 (AR_DATA*) &mask, resulttype);
	(void) AR_bitand((AR_DATA*) result, resulttype,
			 (AR_DATA*) result, resulttype,
			 (AR_DATA*) ones, resulttype);

	/*
	 * Extract just the part that we'll rotate.
	 */
	ar_dblshift(&mask, resulttype, &zeros, ones,
		    (type_size - (int) shift_size));
	(void) AR_bitand((AR_DATA*) &rotatee, resulttype,
			 (AR_DATA*) opnd, resulttype,
			 (AR_DATA*) &mask, resulttype);

	/*
	 * Shift the right part of the rotated result
	 * right into its correct position.  This
	 * leaves no extraneous bits, so we can then
	 * OR it into the result.
	 */
	ar_dblshift(&temp, resulttype, &zeros, &rotatee,
		    ((int) (shift_size - shift_count)));
	(void) AR_bitor((AR_DATA*) result, resulttype,
			(AR_DATA*) result, resulttype,
			(AR_DATA*) &temp, resulttype);

	/*
	 * Left-justify the left part of the rotated
	 * result, removing the original bits to its
	 * left.
	 */
	ar_dblshift(&temp, resulttype, &rotatee, &zeros,
		    ((int) (shift_size - shift_count)));

	/*
	 * Shift the left part of the rotated result
	 * result right into its correct position.  This
	 * leaves no extraneous bits, so we can then OR
	 * it into the result.
	 */
	ar_dblshift(&temp, resulttype, &zeros, &temp,
		    (type_size - (int) shift_size));
	(void) AR_bitor((AR_DATA*) result, resulttype,
			(AR_DATA*) result, resulttype,
			(AR_DATA*) &temp, resulttype);

	return AR_status ((AR_DATA*)result, resulttype);
}



int
AR_ibits (AR_DATA *res, const AR_TYPE *resulttype,
	 const AR_DATA *opd, const AR_TYPE *opndtype,
	 const AR_DATA *shft, const AR_TYPE *shifttype,
	 const AR_DATA *sz, const AR_TYPE *sizetype) {
	ar_data		*result = (ar_data*)res;
	ar_data		*opnd   = (ar_data*)opd;
	ar_data		*shift  = (ar_data*)shft;
	ar_data		*size   = (ar_data*)sz;

	AR_HOST_SINT64	shift_count;
	AR_HOST_SINT64	shift_size;
	int		type_size;

	ar_data		zeros  = { 0x0000, 0x0000, 0x0000, 0x0000 };

	ar_data		temp;

	if (*resulttype != *opndtype ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*shifttype) != AR_CLASS_INT ||
	    AR_CLASS (*sizetype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*opndtype) == AR_INT_SIZE_24 ||
	    AR_INT_SIZE(*shifttype) == AR_INT_SIZE_24 ||
	    AR_INT_SIZE(*sizetype) == AR_INT_SIZE_24)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*opndtype)) {
	case AR_INT_SIZE_8:
		INT8_TO_HOST_SINT64(shift_count, shift);
		INT8_TO_HOST_SINT64(shift_size, size);
		type_size = 8;
		break;
	case AR_INT_SIZE_16:
		INT16_TO_HOST_SINT64(shift_count, shift);
		INT16_TO_HOST_SINT64(shift_size, size);
		type_size = 16;
		break;
	case AR_INT_SIZE_32:
		INT32_TO_HOST_SINT64(shift_count, shift);
		INT32_TO_HOST_SINT64(shift_size, size);
		type_size = 32;
		break;
	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		INT64_TO_HOST_SINT64(shift_count, shift);
		INT64_TO_HOST_SINT64(shift_size, size);
		type_size = 64;
		break;
	default:
		return (AR_STAT_INVALID_TYPE);
	}

	/*
	 * Shift_size has to be nonnegative, and shift_count
	 * has to be in [0, (type_size - shift_size)].
	 */
	if (shift_size  < 0 ||
	    shift_count < 0 || shift_count > (type_size - shift_size))
		return AR_STAT_UNDEFINED;

	/*
	 * Left shift the operand to throw away the bits above
	 * the ones we want, then right shift it (zero-filling)
	 * to right justify them.
	 */
	ar_dblshift(&temp, resulttype, opnd, &zeros,
		    ((int) shift_count + (int) shift_size));
	ar_dblshift(result, resulttype, &zeros, &temp,
		    (type_size - (int) shift_size));

	return AR_status ((AR_DATA*)result, resulttype);
}


/* Mask creation. Count in [0, AR_INT_SIZE-1] creates a left mask; count
 * in [AR_INT_SIZE, 2*AR_INT_SIZE] makes a right-justified mask.
 */
int
AR_mask (AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *cnt, const AR_TYPE *counttype) {
	ar_data* result = (ar_data*)res;
	ar_data* count  = (ar_data*)cnt;

	ar_data ones8  = { 0x0000, 0x0000, 0x0000, 0x00FF };
	ar_data ones16 = { 0x0000, 0x0000, 0x0000, 0xFFFF };
	ar_data ones32 = { 0x0000, 0x0000, 0xFFFF, 0xFFFF };
	ar_data ones64 = { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF };
	ar_data zeros  = { 0x0000, 0x0000, 0x0000, 0x0000 };

	if (*resulttype != *counttype ||
	    AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_CLASS (*counttype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

	switch (AR_INT_SIZE (*resulttype)) {
	case AR_INT_SIZE_8:
		/* Check for shift count not in [0,16] */
		if (count->ar_i8.part5 > 16) {
			ZERO_INT8(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &ones8, &zeros, count->ar_i8.part5);
		break;

	case AR_INT_SIZE_16:
		/* Check for shift count not in [0,32] */
		if (count->ar_i64.part4 > 32) {
			ZERO_INT16(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &ones16, &zeros, count->ar_i64.part4);
		break;

	case AR_INT_SIZE_32:
		/* Check for shift count not in [0,64] */
		if (count->ar_i64.part3 || count->ar_i64.part4 > 64) {
			ZERO_INT32(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &ones32, &zeros, count->ar_i64.part4);
		break;

	case AR_INT_SIZE_46:
	case AR_INT_SIZE_64:
		/* Check for shift count not in [0,128] */
		if (count->ar_i64.part1 | count->ar_i64.part2 |
		    count->ar_i64.part3 || count->ar_i64.part4 > 128) {
			ZERO_INT64(result);
			return AR_STAT_UNDEFINED;
		}

		/* Use general circular shifter */
		ar_dblshift (result, resulttype,
			     &ones64, &zeros, count->ar_i64.part4);
		break;

	default:
		return (AR_STAT_INVALID_TYPE);
	}

	return AR_status ((AR_DATA*)result, resulttype);
}


/* Leading zero count */
int
AR_leadz (AR_DATA *res, const AR_TYPE *resulttype,
	const AR_DATA *opd, const AR_TYPE *opndtype) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	int leadz, mask = 0x8000, mask8 = 0x80;

	if (AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_8 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_16 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_32 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_46 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_64)
		return AR_STAT_INVALID_TYPE;

	if (AR_CLASS (*opndtype) == AR_CLASS_INT &&
	     AR_INT_SIZE (*opndtype) == AR_INT_SIZE_8) {
		if (opnd->ar_i8.part5)
			for (leadz = 0; !(mask8 & opnd->ar_i8.part5);
			     mask8 >>= 1)
				leadz++;
		else
			leadz = 8;
	}
	else if (AR_CLASS (*opndtype) == AR_CLASS_INT &&
		 AR_INT_SIZE (*opndtype) == AR_INT_SIZE_16) {
		if (opnd->ar_i64.part4)
			for (leadz = 0; !(mask & opnd->ar_i64.part4);
			     mask >>= 1)
				leadz++;
		else
			leadz = 16;
	}
	else if ((AR_CLASS (*opndtype) == AR_CLASS_INT &&
		  AR_INT_SIZE (*opndtype) == AR_INT_SIZE_32) ||
		 (AR_CLASS (*opndtype) == AR_CLASS_FLOAT &&
		  AR_FLOAT_IS_COMPLEX (*opndtype) == AR_FLOAT_SIMPLE &&
		  AR_FLOAT_SIZE(*opndtype) == AR_FLOAT_32)) {
		if (opnd->ar_i64.part3)
			for (leadz = 0; !(mask & opnd->ar_i64.part3);
			     mask >>= 1)
				leadz++;
		else if (opnd->ar_i64.part4)
			for (leadz = 16; !(mask & opnd->ar_i64.part4);
			     mask >>= 1)
				leadz++;
		else
			leadz = 32;
	}
	else if ((AR_CLASS (*opndtype) == AR_CLASS_INT &&
	          (AR_INT_SIZE (*opndtype) == AR_INT_SIZE_46 ||
	           AR_INT_SIZE (*opndtype) == AR_INT_SIZE_64)) ||
	         (AR_CLASS (*opndtype) == AR_CLASS_FLOAT &&
	          AR_FLOAT_IS_COMPLEX (*opndtype) == AR_FLOAT_SIMPLE &&
	          AR_FLOAT_SIZE(*opndtype) == AR_FLOAT_64)) {
		if (opnd->ar_i64.part1)
			for (leadz = 0; !(mask & opnd->ar_i64.part1);
			     mask >>= 1)
				leadz++;
		else if (opnd->ar_i64.part2)
			for (leadz = 16; !(mask & opnd->ar_i64.part2);
			     mask >>= 1)
				leadz++;
		else if (opnd->ar_i64.part3)
			for (leadz = 32; !(mask & opnd->ar_i64.part3);
			     mask >>= 1)
				leadz++;
		else if (opnd->ar_i64.part4)
			for (leadz = 48; !(mask & opnd->ar_i64.part4);
			     mask >>= 1)
				leadz++;
		else
			leadz = 64;
	}
	else {
		return AR_STAT_INVALID_TYPE;
	}

	ZERO_INT16_UPPER(result);
	result->ar_i64.part4 = leadz;

	return AR_status ((AR_DATA*)result, resulttype);
}


/* Bit population count */
int
AR_popcnt (AR_DATA *res, const AR_TYPE *resulttype,
	 const AR_DATA *opd, const AR_TYPE *opndtype) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	unsigned int i, popcnt = 0;

	if (AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_8 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_16 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_32 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_46 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_64)
		return AR_STAT_INVALID_TYPE;

	if (AR_CLASS (*opndtype) == AR_CLASS_INT) {
		switch (AR_INT_SIZE (*opndtype)) {
		case AR_INT_SIZE_8:
			for (i = opnd->ar_i8.part5; i; i >>= 1)
				popcnt += i & 1;
			break;

		case AR_INT_SIZE_16:
			for (i = opnd->ar_i64.part4; i; i >>= 1)
				popcnt += i & 1;
			break;

		case AR_INT_SIZE_32:
			for (i = opnd->ar_i64.part3; i; i >>= 1)
				popcnt += i & 1;
			for (i = opnd->ar_i64.part4; i; i >>= 1)
				popcnt += i & 1;
			break;

		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			for (i = opnd->ar_i64.part1; i; i >>= 1)
				popcnt += i & 1;
			for (i = opnd->ar_i64.part2; i; i >>= 1)
				popcnt += i & 1;
			for (i = opnd->ar_i64.part3; i; i >>= 1)
				popcnt += i & 1;
			for (i = opnd->ar_i64.part4; i; i >>= 1)
				popcnt += i & 1;
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
	}
	else if (AR_CLASS (*opndtype) == AR_CLASS_FLOAT &&
	         AR_FLOAT_IS_COMPLEX (*opndtype) == AR_FLOAT_SIMPLE) {
		for (i = opnd->ar_i64.part3; i; i >>= 1)
			popcnt += i & 1;
		for (i = opnd->ar_i64.part4; i; i >>= 1)
			popcnt += i & 1;
		if (AR_FLOAT_SIZE(*opndtype) == AR_FLOAT_64) {
			for (i = opnd->ar_i64.part1; i; i >>= 1)
				popcnt += i & 1;
			for (i = opnd->ar_i64.part2; i; i >>= 1)
				popcnt += i & 1;
		}
		else if (AR_FLOAT_SIZE(*opndtype) != AR_FLOAT_32) {
			return AR_STAT_INVALID_TYPE;
		}
	}
	else {
		return AR_STAT_INVALID_TYPE;
	}

	result->ar_i64.part1 = result->ar_i64.part2 = result->ar_i64.part3 = 0;
	result->ar_i64.part4 = popcnt;

	return AR_status ((AR_DATA*)result, resulttype);
}


/* Bit population count parity (0 if pop count even, 1 if odd) */
int
AR_poppar (AR_DATA *res, const AR_TYPE *resulttype,
	 const AR_DATA *opd, const AR_TYPE *opndtype) {
	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	unsigned int poppar = 0;

	if (AR_CLASS (*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_8 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_16 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_32 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_46 &&
	    AR_INT_SIZE (*resulttype) != AR_INT_SIZE_64)
		return AR_STAT_INVALID_TYPE;

	if (AR_CLASS (*opndtype) == AR_CLASS_INT) {
		switch (AR_INT_SIZE (*opndtype)) {
		case AR_INT_SIZE_8:
			poppar ^= opnd->ar_i8.part5;
			break;

		case AR_INT_SIZE_16:
			poppar ^= opnd->ar_i64.part4;
			break;

		case AR_INT_SIZE_32:
			poppar ^= opnd->ar_i64.part3 ^
				  opnd->ar_i64.part4;
			break;

		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			poppar ^= opnd->ar_i64.part1 ^
				  opnd->ar_i64.part2 ^
				  opnd->ar_i64.part3 ^
				  opnd->ar_i64.part4;
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
	}
	else if (AR_CLASS (*opndtype) == AR_CLASS_FLOAT &&
	         AR_FLOAT_IS_COMPLEX (*opndtype) == AR_FLOAT_SIMPLE) {
		poppar = opnd->ar_i64.part3 ^ opnd->ar_i64.part4;
		if (AR_FLOAT_SIZE(*opndtype) == AR_FLOAT_64) {
			poppar ^= opnd->ar_i64.part1 ^ opnd->ar_i64.part2;
		}
		else if (AR_FLOAT_SIZE(*opndtype) != AR_FLOAT_32) {
			return AR_STAT_INVALID_TYPE;
		}
	}
	else {
		return AR_STAT_INVALID_TYPE;
	}

	poppar = (poppar & 0xFF) ^ (poppar >> 8);
	poppar = (poppar & 0x0F) ^ (poppar >> 4);
	poppar = (poppar & 0x03) ^ (poppar >> 2);
	poppar = (poppar & 0x01) ^ (poppar >> 1);

	ZERO_INT16_UPPER(result);
	result->ar_i64.part4 = poppar;

	return AR_status ((AR_DATA*)result, resulttype);
}

static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: bits.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
