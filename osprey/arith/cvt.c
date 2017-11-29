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


/* Conversions between types */

#include "arith.internal.h"


int
ar_convert_to_integral
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd,   const AR_TYPE *opndtype) {

	int status = AR_STAT_OK;
	int maxnegint;
	int rsltsz, opndsz;

	if (AR_CLASS (*opndtype) == AR_CLASS_FLOAT) {
		switch (*opndtype) {
		case AR_Float_Cray1_64:
		case AR_Float_Cray1_64_F:
			status = ar_cfix64 (&result->ar_i64, &opnd->ar_f64, 64);
			break;
		case AR_Float_Cray1_128:
			status = ar_cfix128 (&result->ar_i64,
					     &opnd->ar_f128, 64);
			break;
		case AR_Float_IEEE_NR_32:
		case AR_Float_IEEE_ZE_32:
		case AR_Float_IEEE_UP_32:
		case AR_Float_IEEE_DN_32:
			status = ar_ifix32 (&result->ar_i64, &opnd->ar_ieee32,
					    64, ROUND_MODE (*opndtype));
			break;
		case AR_Float_IEEE_NR_64:
		case AR_Float_IEEE_ZE_64:
		case AR_Float_IEEE_UP_64:
		case AR_Float_IEEE_DN_64:
			status = ar_ifix64 (&result->ar_i64, &opnd->ar_ieee64,
					    64, ROUND_MODE (*opndtype));
			break;
		case AR_Float_IEEE_NR_128:
		case AR_Float_IEEE_ZE_128:
		case AR_Float_IEEE_UP_128:
		case AR_Float_IEEE_DN_128:
			status = ar_ifix128 (&result->ar_i64, &opnd->ar_ieee128,
					    64, ROUND_MODE (*opndtype));
			break;
		case AR_Complex_Cray1_64:
		case AR_Complex_Cray1_64_F:
			status = ar_cfix64 (&result->ar_i64,
					    &opnd->ar_cplx_f64.real, 64);
			break;
		case AR_Complex_Cray1_128:
			status = ar_cfix128 (&result->ar_i64,
					     &opnd->ar_cplx_f128.real, 64);
			break;
		case AR_Complex_IEEE_NR_32:
		case AR_Complex_IEEE_ZE_32:
		case AR_Complex_IEEE_UP_32:
		case AR_Complex_IEEE_DN_32:
		{
			AR_IEEE_32 realpart;

			CPLX32_REAL_TO_IEEE32(realpart, opnd->ar_cplx_ieee32);
			status = ar_ifix32 (&result->ar_i64, &realpart,
					    64, ROUND_MODE (*opndtype));
			break;
		}
		case AR_Complex_IEEE_NR_64:
		case AR_Complex_IEEE_ZE_64:
		case AR_Complex_IEEE_UP_64:
		case AR_Complex_IEEE_DN_64:
			status = ar_ifix64 (&result->ar_i64,
					    &opnd->ar_cplx_ieee64.real,
					    64, ROUND_MODE (*opndtype));
			break;
		case AR_Complex_IEEE_NR_128:
		case AR_Complex_IEEE_ZE_128:
		case AR_Complex_IEEE_UP_128:
		case AR_Complex_IEEE_DN_128:
			status = ar_ifix128(&result->ar_i64,
					    &opnd->ar_cplx_ieee128.real,
					    64, ROUND_MODE (*opndtype));
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}

	}

	else if (AR_CLASS (*opndtype) == AR_CLASS_POINTER) {
		result->ar_i64 = opnd->ar_i64;
	}

	else if (AR_CLASS (*opndtype) == AR_CLASS_INT) {
		result->ar_i64 = opnd->ar_i64;
		opndsz = AR_INT_SIZE(*opndtype);
		if(opndsz == AR_INT_SIZE_46) opndsz = AR_INT_SIZE_64;
		rsltsz = AR_INT_SIZE(*resulttype);
		if(rsltsz == AR_INT_SIZE_46) rsltsz = AR_INT_SIZE_64;

		if (AR_SIGNEDNESS (*opndtype) == AR_SIGNED) {
			/* operand is signed; sign extend to 64-bit int */
			if (opndsz == AR_INT_SIZE_8 && INT8_SIGN(opnd)) {
				/* 8-bit operand is negative; extend the sign */
				maxnegint = IS_INT8_UPPER_ZERO(opnd) &&
							(opnd->ar_i8.part5 == 0x80);
				result->ar_i8.part1  = 0xFFFF;
				result->ar_i8.part2  = 0xFFFF;
				result->ar_i8.part3  = 0xFFFF;
				result->ar_i8.part4  =   0xFF;
			}
			else if (opndsz == AR_INT_SIZE_16 && INT16_SIGN(opnd)) {
				maxnegint = IS_INT16_UPPER_ZERO(opnd) &&
							(opnd->ar_i64.part4 == 0x8000);
				/* 16-bit operand is negative; extend the sign*/
				result->ar_i64.part1 = 0xFFFF;
				result->ar_i64.part2 = 0xFFFF;
				result->ar_i64.part3 = 0xFFFF;
			}
			else if (opndsz == AR_INT_SIZE_24 && INT24_SIGN(opnd)) {
				maxnegint = IS_INT24_UPPER_ZERO(opnd) &&
							(opnd->ar_i64.part3 == 0x80) &&
							(opnd->ar_i64.part4 == 0x0);
				/* 24-bit operand is negative; extend the sign*/
				result->ar_i64.part1  = 0xFFFF;
				result->ar_i64.part2  = 0xFFFF;
				result->ar_i64.part3 |= 0xFF00;
			}
			else if (opndsz == AR_INT_SIZE_32 && INT32_SIGN(opnd)) {
				maxnegint = IS_INT32_UPPER_ZERO(opnd) &&
							(opnd->ar_i64.part3 == 0x8000) &&
							(opnd->ar_i64.part4 == 0);
				/* 32-bit operand is negative; extend the sign*/
				result->ar_i64.part1 = 0xFFFF;
				result->ar_i64.part2 = 0xFFFF;
			}
			else
				maxnegint = (opnd->ar_i64.part1 == 0x8000) &&
							(opnd->ar_i64.part2 == 0) &&
							(opnd->ar_i64.part3 == 0) &&
							(opnd->ar_i64.part4 == 0);

			if ((result->ar_i64.part1 & 0x8000) &&
				(AR_SIGNEDNESS (*resulttype) == AR_UNSIGNED)) {
				/* operand is negative and result is unsigned;
				   original value cannot be preserved */
				if(opndsz == rsltsz)
					status |= AR_STAT_SEMIVALID;
				if(opndsz != rsltsz || !maxnegint)
					status |= AR_STAT_OVERFLOW;
			}
		}
		else if (AR_SIGNEDNESS(*resulttype) == AR_SIGNED && rsltsz == opndsz) {
			/* operand is unsigned, same size result is signed */
			switch (AR_INT_SIZE (*opndtype)) {
			case AR_INT_SIZE_8:
				if (INT8_SIGN(opnd)) {
					status |= AR_STAT_SEMIVALID;
					if(opnd->ar_i8.part5 != 0x80)
						status |= AR_STAT_OVERFLOW;
				}
				break;

			case AR_INT_SIZE_16:
				if (INT16_SIGN(opnd)) {
					status |= AR_STAT_SEMIVALID;
					if(opnd->ar_i64.part4 != 0x8000)
						status |= AR_STAT_OVERFLOW;
				}
				break;

			case AR_INT_SIZE_24:
				if (INT24_SIGN(opnd)) {
					status |= AR_STAT_SEMIVALID;
					if(opnd->ar_i64.part3 != 0x80 ||
					   opnd->ar_i64.part4 != 0)
						status |= AR_STAT_OVERFLOW;
				}
				break;

			case AR_INT_SIZE_32:
				if (INT32_SIGN(opnd)) {
					status |= AR_STAT_SEMIVALID;
					if(opnd->ar_i64.part3 != 0x8000 ||
					   opnd->ar_i64.part4 != 0)
						status |= AR_STAT_OVERFLOW;
				}
				break;

			case AR_INT_SIZE_46:
			case AR_INT_SIZE_64:
				if(INT64_SIGN(opnd)) {
					status |= AR_STAT_SEMIVALID;
					if(opnd->ar_i64.part1 != 0x8000 ||
					   opnd->ar_i64.part2 != 0 ||
					   opnd->ar_i64.part3 != 0 ||
					   opnd->ar_i64.part4 != 0)
						status |= AR_STAT_OVERFLOW;
				}
				break;

			default:
				return (AR_STAT_INVALID_TYPE);
			}

			if(status & (AR_STAT_SEMIVALID | AR_STAT_OVERFLOW))
				status |= AR_STAT_NEGATIVE;
		}
		else {
			/* operand is unsigned and result is different size */
			if (INT64_SIGN(result) &&
			    (AR_SIGNEDNESS(*resulttype) == AR_SIGNED)) {
				/* result is negative; we have overflow */
				status |= AR_STAT_OVERFLOW;
			}
		}        
	}
	else
		return AR_STAT_INVALID_TYPE;

	/* At this point, regardless of the original operand type, we've converted
	   it to a 64-bit int.  Now, check for overflow, negative. */

	if(!(status & AR_STAT_SEMIVALID))
		switch (*resulttype) {
                case AR_Int_8_S:
                        if (!(result->ar_i8.part1 == 0xffff &&
                              result->ar_i8.part2 == 0xffff &&
                              result->ar_i8.part3 == 0xffff &&
                              result->ar_i8.part4 ==   0xff &&
                              INT8_SIGN(result) == 0x80)&&
                            !(IS_INT8_UPPER_ZERO(result) &&
                              INT8_SIGN(result) == 0)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        if (INT8_SIGN(result))
                                status |= AR_STAT_NEGATIVE;
                        break;

                case AR_Int_8_U:
                        if (!IS_INT8_UPPER_ZERO(result)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        break;

                case AR_Int_16_S:
                        if (!(result->ar_i64.part1 == 0xffff &&
                              result->ar_i64.part2 == 0xffff &&
                              result->ar_i64.part3 == 0xffff &&
                              INT16_SIGN(result) == 0x8000)&&
                            !(IS_INT16_UPPER_ZERO(result) &&
                              INT16_SIGN(result) == 0)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        if (INT16_SIGN(result))
                                status |= AR_STAT_NEGATIVE;
                        break;

                case AR_Int_16_U:
                        if (!IS_INT16_UPPER_ZERO(result)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        break;

                case AR_Int_24_S:
                        if (!(result->ar_i64.part1 == 0xffff &&
                              result->ar_i64.part2 == 0xffff &&
                              (result->ar_i64.part3&0xff00) == 0xff00 &&
                              INT24_SIGN(result) == 0x0080)&&
                            !(IS_INT24_UPPER_ZERO(result) &&
                              INT24_SIGN(result) == 0)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        if (INT24_SIGN(result))
                                status |= AR_STAT_NEGATIVE;
                        break;

                case AR_Int_24_U:
                        if (!IS_INT24_UPPER_ZERO(result)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        break;

                case AR_Int_32_S:
                        if (!(result->ar_i64.part1 == 0xffff &&
                              result->ar_i64.part2 == 0xffff &&
                              INT32_SIGN(result) == 0x8000)&&
                            !(IS_INT32_UPPER_ZERO(result) &&
                              INT32_SIGN(result) == 0)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        if (INT32_SIGN(result))
                                status |= AR_STAT_NEGATIVE;

                        break;

                case AR_Int_32_U:
                        if (!IS_INT32_UPPER_ZERO(result)) {
                                status |= AR_STAT_OVERFLOW;
                        }
                        break;

                case AR_Int_46_S:
                        if (INT_OVERFLOWS_46_BITS(*result))
                                status |= AR_STAT_OVERFLOW;
                        if (INT64_SIGN(result))
                                status |= AR_STAT_NEGATIVE;
                        break;

                case AR_Int_64_S:
                        if (INT64_SIGN(result))
                                status |= AR_STAT_NEGATIVE;
                        break;
		}

        ar_clear_unused_bits(result, resulttype);

        if (IS_INT64_ZERO(result))
                status |= AR_STAT_ZERO;

        return status;
}

static
int
ar_convert_to_pointer
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd,   const AR_TYPE *opndtype) {

      AR_TYPE unsigned_word_type = AR_Int_64_U;


	if (AR_CLASS (*opndtype) == AR_CLASS_INT)
		/* for some reason, we don't sign extend when converting to a
		   word pointer, but we do if converting to other pointers... */
		if (AR_POINTER_FORMAT (*resulttype) != AR_POINTER_WORD) {
			ar_convert_to_integral (result, &unsigned_word_type,
						opnd, opndtype);
		}
		else {
			result->ar_i64 = opnd->ar_i64;
		}

	else if (AR_CLASS (*opndtype) == AR_CLASS_POINTER) {

		result->ar_i64 = opnd->ar_i64;
		if (*resulttype == *opndtype)
			return AR_STAT_OK;
		/* if either the result or the operand is a byte pointer
                   (and the other isn't from the previous test):  ERROR  */
		if (*resulttype == AR_Pointer_Byte ||
		    *opndtype == AR_Pointer_Byte)
			return AR_STAT_INVALID_TYPE;

		if (AR_POINTER_FORMAT (*resulttype) == AR_POINTER_WORD) {
			if (AR_POINTER_FORMAT (*opndtype) == AR_POINTER_FCTN) {
				/* convert parcel address to word address */
				ar_dblshift(result, &unsigned_word_type,
					(const ar_data*)&AR_const_zero, result, 2);
			}

		} else if (AR_POINTER_FORMAT (*resulttype) == AR_POINTER_FCTN) {
			if (AR_POINTER_FORMAT (*opndtype) == AR_POINTER_WORD) {
				/* convert word address to parcel address */
				ar_dblshift(result, &unsigned_word_type,
					(const ar_data*)&AR_const_zero, result, 126);
			}
		}
	} else
		return AR_STAT_INVALID_TYPE;

	ar_clear_unused_bits (result, resulttype);
	return AR_STAT_OK;
}

int
ar_convert_to_float
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd,   const AR_TYPE *opndtype) {

	ar_data re, im, sint64;
	AR_TYPE reimtype, sint64type = AR_Int_64_S;
	int status = AR_STAT_OK;

	if (AR_CLASS (*opndtype) == AR_CLASS_FLOAT) {

		if (AR_FLOAT_IS_COMPLEX (*opndtype) == AR_FLOAT_COMPLEX)
			status |= ar_decompose_complex (&re, &im, &reimtype,
							opnd, opndtype);
		else {
			re = *opnd;
			reimtype = *opndtype;
		}

		if (AR_FLOAT_FORMAT (*resulttype) == AR_FLOAT_CRAY)
			if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_64)
				if (AR_FLOAT_FORMAT (reimtype) == AR_FLOAT_IEEE)
					if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64)
						status |= ar_itoc64 (&result->ar_f64, &re.ar_ieee64, AR_ROUND_NEAREST);
					else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_32) {
						status |= ar_i32to64 (&im.ar_ieee64, &re.ar_ieee32);
						status |= ar_itoc64 (&result->ar_f64, &im.ar_ieee64, AR_ROUND_NEAREST);
					} else
						return AR_STAT_INVALID_TYPE;
				else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64) {
					result->ar_f64 = re.ar_f64;
					status = AR_status ((AR_DATA*)result, resulttype);
				} else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128)
					status |= ar_c128to64 (&result->ar_f64,
							       &re.ar_f128);
				else
					return AR_STAT_INVALID_TYPE;
			else if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_128)
				if (AR_FLOAT_FORMAT (reimtype) == AR_FLOAT_IEEE)
					if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64)
						status |= ar_i64toc128 (&result->ar_f128, &re.ar_ieee64);
					else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_32) {
						status |= ar_i32to64 (&im.ar_ieee64, &re.ar_ieee32);
						status |= ar_i64toc128 (&result->ar_f128, &im.ar_ieee64);
					} else
						return AR_STAT_INVALID_TYPE;
				else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128) {
					result->ar_f128 = re.ar_f128;
					status = AR_status ((AR_DATA*)result, resulttype);
				} else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64)
					status |= ar_c64to128 (&result->ar_f128,
							       &re.ar_f64);
				else
					return AR_STAT_INVALID_TYPE;
			else
				return AR_STAT_INVALID_TYPE;

		else {
			/* AR_FLOAT_FORMAT (*resulttype) == AR_FLOAT_IEEE */
			if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_32) {
				if (AR_FLOAT_FORMAT (reimtype) == AR_FLOAT_IEEE) {
					if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_32) {
						result->ar_ieee32 = re.ar_ieee32;
						status |= AR_status ((AR_DATA*)result, resulttype);
					} else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64)
						status |= ar_i64to32 (&result->ar_ieee32, &re.ar_ieee64, AR_ROUND_NEAREST);
					else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128) {
						status |= ar_i128to64 (&im.ar_ieee64, &re.ar_ieee128, AR_ROUND_NEAREST);
						status |= ar_i64to32 (&result->ar_ieee32, &im.ar_ieee64, AR_ROUND_NEAREST);
					}
					else
						return AR_STAT_INVALID_TYPE;
				}
				else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128) {
					status |= ar_c128toi64 (&im.ar_ieee64, &re.ar_f128);
					status |= ar_i64to32 (&result->ar_ieee32, &im.ar_ieee64, AR_ROUND_NEAREST);
				} else {
					status |= ar_ctoi64 (&im.ar_ieee64, &re.ar_f64);
					status |= ar_i64to32 (&result->ar_ieee32, &im.ar_ieee64, AR_ROUND_NEAREST);
				}
			}
			else if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_64) {
				if (AR_FLOAT_FORMAT (reimtype) == AR_FLOAT_IEEE) {
					if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_32)
						status |= ar_i32to64 (&result->ar_ieee64, &re.ar_ieee32);
					else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64) {
						result->ar_ieee64 = re.ar_ieee64;
						return AR_status ((AR_DATA*)result, resulttype);
					}
					else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128)
						status |= ar_i128to64 (&result->ar_ieee64, &re.ar_ieee128, AR_ROUND_NEAREST);
					else
						return AR_STAT_INVALID_TYPE;
				}
				else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128) {
					status |= ar_ctoi128 (&im.ar_ieee128, &re.ar_f128);
					status |= ar_i128to64 (&result->ar_ieee64, &im.ar_ieee128, AR_ROUND_NEAREST);
				}
				else {
					status |= ar_ctoi64 (&result->ar_ieee64, &re.ar_f64);
				}
			}

			else if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_128) {
				if (AR_FLOAT_FORMAT (reimtype) == AR_FLOAT_IEEE) {
					if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_32) {
						status |= ar_i32to64 (&im.ar_ieee64, &re.ar_ieee32);
						status |= ar_i64to128 (&result->ar_ieee128, &im.ar_ieee64);
					}
					else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64)
						status |= ar_i64to128 (&result->ar_ieee128, &re.ar_ieee64);
					else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128) {
						result->ar_ieee128 = re.ar_ieee128;
						return AR_status ((AR_DATA*)result, resulttype);
					}
					else
						return AR_STAT_INVALID_TYPE;
				}
				else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128)
					status |= ar_ctoi128 (&result->ar_ieee128, &re.ar_f128);
				else {
					status |= ar_ctoi64 (&im.ar_ieee64, &re.ar_f64);
					status |= ar_i64to128(&result->ar_ieee128, &im.ar_ieee64);
				}
			}

			else if (AR_FLOAT_FORMAT (reimtype) == AR_FLOAT_CRAY)
				if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_128)
					status |= ar_c128toi64 (&result->ar_ieee64, &re.ar_f128);
				else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_64)
					status |= ar_ctoi64 (&result->ar_ieee64, &re.ar_f64);
				else
					return AR_STAT_INVALID_TYPE;
			else if (AR_FLOAT_SIZE (reimtype) == AR_FLOAT_32)
				status |= ar_i32to64 (&result->ar_ieee64, &re.ar_ieee32);
			else {
				result->ar_ieee64 = re.ar_ieee64;
				status = AR_status ((AR_DATA*)result, resulttype);
			}
		}

	} else if (AR_CLASS (*opndtype) == AR_CLASS_INT) {

		/* Convert to signed 64-bit (ignoring status) */
		ar_convert_to_integral (&sint64, &sint64type, opnd, opndtype);

		switch (*resulttype) {
		case AR_Float_Cray1_64:
		case AR_Float_Cray1_64_F:
			status |= ar_cflt64 (&result->ar_f64, &sint64.ar_i64,
					     AR_SIGNEDNESS (*opndtype) == AR_UNSIGNED);
			break;
		case AR_Float_Cray1_128:
			status |= ar_cflt128 (&result->ar_f128, &sint64.ar_i64,
					      AR_SIGNEDNESS (*opndtype) == AR_UNSIGNED);
			break;
		case AR_Float_IEEE_NR_32:
		case AR_Float_IEEE_ZE_32:
		case AR_Float_IEEE_UP_32:
		case AR_Float_IEEE_DN_32:
			status |= ar_iflt32 (&result->ar_ieee32, &sint64.ar_i64,
					     AR_SIGNEDNESS (*opndtype) == AR_UNSIGNED,
					     ROUND_MODE (*resulttype));
			break;
		case AR_Float_IEEE_NR_64:
		case AR_Float_IEEE_ZE_64:
		case AR_Float_IEEE_UP_64:
		case AR_Float_IEEE_DN_64:
			status |= ar_iflt64 (&result->ar_ieee64, &sint64.ar_i64,
					     AR_SIGNEDNESS (*opndtype) == AR_UNSIGNED,
					     ROUND_MODE (*resulttype));
			break;
		case AR_Float_IEEE_NR_128:
		case AR_Float_IEEE_ZE_128:
		case AR_Float_IEEE_UP_128:
		case AR_Float_IEEE_DN_128:
			status |= ar_iflt128 (&result->ar_ieee128, &sint64.ar_i64,
					     AR_SIGNEDNESS (*opndtype) == AR_UNSIGNED,
					     ROUND_MODE (*resulttype));
			break;
		default:
			return AR_STAT_INVALID_TYPE;
		}

	} else
		return AR_STAT_INVALID_TYPE;

	return status;
}


int
ar_convert_to_complex
		(ar_data *result, const AR_TYPE *resulttype,
   const ar_data *opnd,   const AR_TYPE *opndtype) {
 
	ar_data from, re, im, cre, cim;
	AR_TYPE reimtype, parttype, temptype;
	int status = AR_STAT_OK, restat, imstat;

	parttype = (AR_TYPE) (*resulttype ^ AR_FLOAT_COMPLEX);

	if (AR_CLASS (*opndtype) == AR_CLASS_FLOAT &&
	    AR_FLOAT_IS_COMPLEX (*opndtype) == AR_FLOAT_COMPLEX) {
		status |= ar_decompose_complex (&re, &im, &reimtype,
						opnd, opndtype);
		restat = ar_convert_to_float (&cre, &parttype, &re, &reimtype);
		imstat = ar_convert_to_float (&cim, &parttype, &im, &reimtype);
		status |= ar_compose_complex (result, &temptype,
					      &cre, &cim, &parttype);
		status &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
		status |= restat & imstat & AR_STAT_ZERO;
		return status;
	}

	status |= ar_convert_to_float (&cre, &parttype, opnd, opndtype);

	switch (*resulttype) {
	case AR_Complex_Cray1_64:
	case AR_Complex_Cray1_64_F:
		result->ar_cplx_f64.real = cre.ar_f64;
		ZEROCRAY64 (result->ar_cplx_f64.imag);
		break;
	case AR_Complex_Cray1_128:
		result->ar_cplx_f128.real = cre.ar_f128;
		ZEROCRAY128 (result->ar_cplx_f128.imag);
		break;
	case AR_Complex_IEEE_NR_32:
	case AR_Complex_IEEE_ZE_32:
	case AR_Complex_IEEE_UP_32:
	case AR_Complex_IEEE_DN_32:
		IEEE32_TO_CPLX32_REAL(result->ar_cplx_ieee32, cre.ar_ieee32);
		result->ar_cplx_ieee32.isign = 0;
		result->ar_cplx_ieee32.iexpo = 0;
		result->ar_cplx_ieee32.icoeff0 = 0;
		result->ar_cplx_ieee32.icoeff1 = 0;
		break;
	case AR_Complex_IEEE_NR_64:
	case AR_Complex_IEEE_ZE_64:
	case AR_Complex_IEEE_UP_64:
	case AR_Complex_IEEE_DN_64:
		result->ar_cplx_ieee64.real = cre.ar_ieee64;
		ZEROIEEE64 (result->ar_cplx_ieee64.imag);
		break;
	case AR_Complex_IEEE_NR_128:
	case AR_Complex_IEEE_ZE_128:
	case AR_Complex_IEEE_UP_128:
	case AR_Complex_IEEE_DN_128:
		result->ar_cplx_ieee128.real = cre.ar_ieee128;
		ZEROIEEE128 (result->ar_cplx_ieee128.imag);
		break;
	default:
		return AR_STAT_INVALID_TYPE;
	}

	return status;
}


/* General dispatch routine for numeric conversions. */
int
AR_convert
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	if (AR_CLASS (*resulttype) == AR_CLASS_INT)
		return ar_convert_to_integral (result, resulttype, opnd, opndtype);

	if (AR_CLASS (*resulttype) == AR_CLASS_POINTER)
		return ar_convert_to_pointer (result, resulttype, opnd, opndtype);

	if (AR_CLASS (*resulttype) == AR_CLASS_FLOAT)
		if (AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX)
			return ar_convert_to_complex (result, resulttype, opnd, opndtype);
		else
			return ar_convert_to_float (result, resulttype, opnd, opndtype);

	return AR_STAT_INVALID_TYPE;
}


/* Weird "round_int_div" operation */
int
AR_round_int_div
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	if (*resulttype != *opndtype)
		return AR_STAT_INVALID_TYPE;

	if (*resulttype == AR_Float_Cray1_64 ||
	    *resulttype == AR_Float_Cray1_64_F)
		return ar_crnd64 (&result->ar_f64, &opnd->ar_f64);
	else if (*resulttype == AR_Float_Cray1_128)
		return ar_crnd128 (&result->ar_f128, &opnd->ar_f128);
	else
		return AR_STAT_INVALID_TYPE;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: cvt.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
