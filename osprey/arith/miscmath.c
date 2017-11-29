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


/* Miscellaneous arithmetic operations */

#include "arith.internal.h"
#include "int64.h"

#include <string.h>

#if !defined(__mips) && !defined(__sun)
	typedef AR_HOST_UINT64 an_mc_table[129];
	extern int GETPMC(an_mc_table, char *);
	extern long CHECKMC(const char *mcname,
			    AR_HOST_SINT64 *pdtwrd,
			    AR_HOST_SINT64 *pdtstrt,
			    AR_HOST_SINT64 *pdtlen,
			    AR_HOST_SINT64 *mcindx,
			    AR_HOST_SINT64 *mctype,
			    AR_HOST_SINT64 *mcdef,
			    const char *pmtname, ...);
#endif

AR_DATA AR_const_zero = { 0, 0, 0, 0 };
AR_DATA AR_const_one  = { 1, 0, 0, 0 };
AR_DATA AR_const_two  = { 2, 0, 0, 0 };

AR_DATA AR_const_false = { 0, 0, 0, 0 };
AR_DATA AR_const_true  = {-1, 0, 0, 0 };

/* Global state */

ar_state_info ar_state_register = { 0, 0, 0, 0, 0, 0, 0, 0 };

/* Get/set internal state register */

AR_HOST_SINT64
AR_get_state_register()
{
	ar_state_info state_register = ar_state_register;

	state_register.ar_unused_mode_bits = 0x1e;
	return *(AR_HOST_SINT64*)&state_register;
}

int
AR_set_state_register(AR_HOST_SINT64 state_reg)
{
	ar_state_info state_register = *(ar_state_info*)&state_reg;

	if(state_register.ar_unused_mode_bits != 0x1e)
		return AR_STAT_UNDEFINED;

	if(ar_rounding_modes &&
	   !(ar_rounding_modes & (1<<state_register.ar_rounding_mode)))
		return AR_STAT_UNDEFINED;

	if(ar_underflow_modes &&
	   !(ar_underflow_modes & (1<<state_register.ar_underflow_mode)))
		return AR_STAT_UNDEFINED;

	state_register.ar_unused_mode_bits = 0;
	ar_state_register = state_register;
	return AR_STAT_OK;
}

/* Get/set rounding mode */

int
AR_get_rounding_mode()
{
	return ar_state_register.ar_rounding_mode;
}

int
AR_set_rounding_mode(int rounding_mode)
{
	if(ar_rounding_modes &&
	   !(ar_rounding_modes & (1<<rounding_mode)))
		return AR_STAT_UNDEFINED;

	ar_state_register.ar_rounding_mode = rounding_mode;
	return AR_STAT_OK;
}

/* Get/set underflow mode */

int
AR_get_underflow_mode()
{
	return ar_state_register.ar_underflow_mode;
}

int
AR_set_underflow_mode(int underflow_mode)
{
	if(ar_underflow_modes &&
	   !(ar_underflow_modes & (1<<underflow_mode)))
		return AR_STAT_UNDEFINED;

	ar_state_register.ar_underflow_mode = underflow_mode;
	return AR_STAT_OK;
}


/* Get floating point format */

int
AR_get_floating_point_format()
{
	return ar_state_register.ar_float_format;
}

/* Get 128-bit floating point format */

int
AR_get_128bit_format()
{
	return ar_state_register.ar_128bit_format;
}


/* Complex value decomposition */
int
ar_decompose_complex (ar_data *real, ar_data *imag, AR_TYPE *parttype,
		      const ar_data *cplx, const AR_TYPE *cplxtype) {

	*parttype = (AR_TYPE) (*cplxtype ^ AR_FLOAT_COMPLEX);

	if (AR_CLASS (*cplxtype) != AR_CLASS_FLOAT ||
	    AR_FLOAT_IS_COMPLEX (*cplxtype) != AR_FLOAT_COMPLEX)
		return AR_STAT_INVALID_TYPE;

	if (AR_FLOAT_FORMAT (*cplxtype) == AR_FLOAT_CRAY)
		if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_64) {
			real->ar_f64 = cplx->ar_cplx_f64.real;
			imag->ar_f64 = cplx->ar_cplx_f64.imag;
		} else if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_128) {
			real->ar_f128 = cplx->ar_cplx_f128.real;
			imag->ar_f128 = cplx->ar_cplx_f128.imag;
		} else
			return AR_STAT_INVALID_TYPE;
	else if (AR_FLOAT_FORMAT (*cplxtype) == AR_FLOAT_IEEE)
		if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_32) {
			CPLX32_REAL_TO_IEEE32(real->ar_ieee32,
			                      cplx->ar_cplx_ieee32);
			CPLX32_IMAG_TO_IEEE32(imag->ar_ieee32,
			                      cplx->ar_cplx_ieee32);
		} else if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_64) {
			real->ar_ieee64 = cplx->ar_cplx_ieee64.real;
			imag->ar_ieee64 = cplx->ar_cplx_ieee64.imag;
		} else if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_128) {
			real->ar_ieee128 = cplx->ar_cplx_ieee128.real;
			imag->ar_ieee128 = cplx->ar_cplx_ieee128.imag;
		} else
			return AR_STAT_INVALID_TYPE;
	else
		return AR_STAT_INVALID_TYPE;

	return AR_STAT_OK;
}


/* Complex value construction */
int
ar_compose_complex (ar_data *cplx, AR_TYPE *cplxtype,
		    const ar_data *real, const ar_data *imag,
		    const AR_TYPE *parttype) {

	*cplxtype = (AR_TYPE) (*parttype ^ AR_FLOAT_COMPLEX);

	if (AR_CLASS (*cplxtype) != AR_CLASS_FLOAT ||
	    AR_FLOAT_IS_COMPLEX (*cplxtype) != AR_FLOAT_COMPLEX)
		return AR_STAT_INVALID_TYPE;

	if (AR_FLOAT_FORMAT (*cplxtype) == AR_FLOAT_CRAY)
		if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_64) {
			cplx->ar_cplx_f64.real = real->ar_f64;
			cplx->ar_cplx_f64.imag = imag->ar_f64;
		} else if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_128) {
			cplx->ar_cplx_f128.real = real->ar_f128;
			cplx->ar_cplx_f128.imag = imag->ar_f128;
		} else
			return AR_STAT_INVALID_TYPE;
	else if (AR_FLOAT_FORMAT (*cplxtype) == AR_FLOAT_IEEE)
		if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_32) {
                        IEEE32_TO_CPLX32_REAL(cplx->ar_cplx_ieee32,
                                              real->ar_ieee32);
                        IEEE32_TO_CPLX32_IMAG(cplx->ar_cplx_ieee32,
                                              imag->ar_ieee32);
		} else if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_64) {
			cplx->ar_cplx_ieee64.real = real->ar_ieee64;
			cplx->ar_cplx_ieee64.imag = imag->ar_ieee64;
		} else if (AR_FLOAT_SIZE (*cplxtype) == AR_FLOAT_128) {
			cplx->ar_cplx_ieee128.real = real->ar_ieee128;
			cplx->ar_cplx_ieee128.imag = imag->ar_ieee128;
		} else
			return AR_STAT_INVALID_TYPE;
	else
		return AR_STAT_INVALID_TYPE;

	return AR_STAT_OK;
}


int
AR_creal (AR_DATA *result, const AR_TYPE *resulttype,
	const AR_DATA *opnd,   const AR_TYPE *opndtype) {

	ar_data im;
	AR_TYPE reimtype;
	int status;

	status = ar_decompose_complex ((ar_data*)result, &im, &reimtype,
							 (const ar_data*)opnd, opndtype);
	if (reimtype != *resulttype)
		return AR_STAT_INVALID_TYPE;
	return status;
}


int
AR_cimag (AR_DATA *result, const AR_TYPE *resulttype,
	const AR_DATA *opnd,   const AR_TYPE *opndtype) {

	ar_data re;
	AR_TYPE reimtype;
	int status;

	status = ar_decompose_complex (&re, (ar_data*)result, &reimtype,
								  (const ar_data*)opnd, opndtype);
	if (reimtype != *resulttype)
		return AR_STAT_INVALID_TYPE;
	return status;
}


/* Status bit computation */
int
AR_status (const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* opnd = (ar_data*)opd;

	int status = AR_STAT_OK, restat, imstat;
	ar_data re, im;
	AR_TYPE reimtype;

	if (AR_CLASS (*opndtype) == AR_CLASS_INT) {
		switch (AR_INT_SIZE (*opndtype)) {
		case AR_INT_SIZE_8:
			if (IS_INT8_ZERO(opnd))
				status |= AR_STAT_ZERO;
			else if (AR_SIGNEDNESS (*opndtype) == AR_SIGNED &&
				 INT8_SIGN(opnd))
				status |= AR_STAT_NEGATIVE;
			break;
		case AR_INT_SIZE_16:
			if (IS_INT16_ZERO(opnd))
				status |= AR_STAT_ZERO;
			else if (AR_SIGNEDNESS (*opndtype) == AR_SIGNED &&
				 INT16_SIGN(opnd))
				status |= AR_STAT_NEGATIVE;
			break;
		case AR_INT_SIZE_24:
			if (IS_INT24_ZERO(opnd))
				status |= AR_STAT_ZERO;
			else if (AR_SIGNEDNESS (*opndtype) == AR_SIGNED &&
				 INT24_SIGN(opnd))
				status |= AR_STAT_NEGATIVE;
			break;
		case AR_INT_SIZE_32:
			if (IS_INT32_ZERO(opnd))
				status |= AR_STAT_ZERO;
			else if (AR_SIGNEDNESS (*opndtype) == AR_SIGNED &&
				 INT32_SIGN(opnd))
				status |= AR_STAT_NEGATIVE;
			break;
		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			if (IS_INT64_ZERO(opnd))
				status |= AR_STAT_ZERO;
			else if (AR_SIGNEDNESS (*opndtype) == AR_SIGNED &&
				 INT64_SIGN(opnd))
				status |= AR_STAT_NEGATIVE;
			break;
		default:
			status = AR_STAT_INVALID_TYPE;
		}

		return status;
	}

	/* Check for null pointer constant */
	if (AR_CLASS (*opndtype) == AR_CLASS_POINTER)
		if (!(opnd->ar_i64.part1 | opnd->ar_i64.part2 |
		      opnd->ar_i64.part3 | opnd->ar_i64.part4))
			return AR_STAT_ZERO;
		else
			return AR_STAT_OK;

	if (AR_CLASS (*opndtype) == AR_CLASS_FLOAT) {

		switch (*opndtype) {

		case AR_Float_Cray1_64:
		case AR_Float_Cray1_64_F:
			if (opnd->ar_f64.sign)
				status |= AR_STAT_NEGATIVE;
			if (!(opnd->ar_f64.expo | opnd->ar_f64.coeff0 |
			      opnd->ar_f64.coeff1 | opnd->ar_f64.coeff2))
				status |= AR_STAT_ZERO;
			else {
				if (opnd->ar_f64.expo > AR_CRAY_MAX_EXPO)
					status |= AR_STAT_OVERFLOW;
				else if (opnd->ar_f64.expo < AR_CRAY_MIN_EXPO)
					status |= AR_STAT_UNDERFLOW;
			}
			break;

		case AR_Float_Cray1_128:
			if (opnd->ar_f128.sign)
				status |= AR_STAT_NEGATIVE;
			if (!(opnd->ar_f128.expo | opnd->ar_f128.coeff0 |
			      opnd->ar_f128.coeff1 | opnd->ar_f128.coeff2 |
			      opnd->ar_f128.zero | opnd->ar_f128.coeff3 |
			      opnd->ar_f128.coeff4 | opnd->ar_f128.coeff5))
				status |= AR_STAT_ZERO;
			else {
				if (opnd->ar_f128.zero)
					status |= AR_STAT_UNDEFINED;
				if (opnd->ar_f128.expo > AR_CRAY_MAX_EXPO)
					status |= AR_STAT_OVERFLOW;
				else if (opnd->ar_f128.expo < AR_CRAY_MIN_EXPO)
					status |= AR_STAT_UNDERFLOW;
			}
			break;

		case AR_Float_IEEE_NR_32:
		case AR_Float_IEEE_ZE_32:
		case AR_Float_IEEE_UP_32:
		case AR_Float_IEEE_DN_32:
			if (opnd->ar_ieee32.expo > AR_IEEE32_MAX_EXPO)
				if (IS_IEEE32_NZ_COEFF(&opnd->ar_ieee32))
					status |= AR_STAT_UNDEFINED; /* NaN */
				else if (opnd->ar_ieee32.sign)
					status |= AR_STAT_OVERFLOW |
						  AR_STAT_NEGATIVE;  /* -Inf */
				else
					status |= AR_STAT_OVERFLOW;  /* +Inf */
			else if (opnd->ar_ieee32.sign)
				status |= AR_STAT_NEGATIVE;
			if (opnd->ar_ieee32.expo == 0 &&
				!IS_IEEE32_NZ_COEFF(&opnd->ar_ieee32))
				status |= AR_STAT_ZERO;
			break;

		case AR_Float_IEEE_NR_64:
		case AR_Float_IEEE_ZE_64:
		case AR_Float_IEEE_UP_64:
		case AR_Float_IEEE_DN_64:
			if (opnd->ar_ieee64.expo > AR_IEEE64_MAX_EXPO)
				if (IS_IEEE64_NZ_COEFF(&opnd->ar_ieee64))
					status |= AR_STAT_UNDEFINED; /* NaN */
				else if (opnd->ar_ieee64.sign)
					status |= AR_STAT_OVERFLOW |
						  AR_STAT_NEGATIVE;  /* -Inf */
				else
					status |= AR_STAT_OVERFLOW;  /* +Inf */
			else if (opnd->ar_ieee64.sign)
				status |= AR_STAT_NEGATIVE;
			if (opnd->ar_ieee64.expo == 0 &&
				!IS_IEEE64_NZ_COEFF(&opnd->ar_ieee64))
				status |= AR_STAT_ZERO;
			break;

		case AR_Float_IEEE_NR_128:
		case AR_Float_IEEE_ZE_128:
		case AR_Float_IEEE_UP_128:
		case AR_Float_IEEE_DN_128:
			if (HOST_IS_MIPS) {
			  if (opnd->ar_mips128.expo > AR_MIPS128_MAX_EXPO)
			    if (IS_MIPS128_NZ_COEFF(&opnd->ar_mips128))
			      status |= AR_STAT_UNDEFINED; /* NaN */
			    else if (opnd->ar_mips128.sign)
			      status |= AR_STAT_OVERFLOW |
					AR_STAT_NEGATIVE;  /* -Inf */
			    else
			      status |= AR_STAT_OVERFLOW;  /* +Inf */
			  else if (opnd->ar_mips128.sign)
			    status |= AR_STAT_NEGATIVE;
			  if (opnd->ar_mips128.expo == 0 &&
			      opnd->ar_mips128.expol == 0 &&
			      !IS_MIPS128_NZ_COEFF(&opnd->ar_mips128))
			    status |= AR_STAT_ZERO;
			  break;
			}

			if (opnd->ar_ieee128.expo > AR_IEEE128_MAX_EXPO)
				if (IS_IEEE128_NZ_COEFF(&opnd->ar_ieee128))
					status |= AR_STAT_UNDEFINED; /* NaN */
				else if (opnd->ar_ieee128.sign)
					status |= AR_STAT_OVERFLOW |
						  AR_STAT_NEGATIVE;  /* -Inf */
				else
					status |= AR_STAT_OVERFLOW;  /* +Inf */
			else if (opnd->ar_ieee128.sign)
				status |= AR_STAT_NEGATIVE;
			if (opnd->ar_ieee128.expo == 0 &&
				!IS_IEEE128_NZ_COEFF(&opnd->ar_ieee128))
				status |= AR_STAT_ZERO;
			break;

		case AR_Complex_Cray1_64:
		case AR_Complex_Cray1_64_F:
		case AR_Complex_Cray1_128:
		case AR_Complex_IEEE_NR_32:
		case AR_Complex_IEEE_ZE_32:
		case AR_Complex_IEEE_UP_32:
		case AR_Complex_IEEE_DN_32:
		case AR_Complex_IEEE_NR_64:
		case AR_Complex_IEEE_ZE_64:
		case AR_Complex_IEEE_UP_64:
		case AR_Complex_IEEE_DN_64:
		case AR_Complex_IEEE_NR_128:
		case AR_Complex_IEEE_ZE_128:
		case AR_Complex_IEEE_UP_128:
		case AR_Complex_IEEE_DN_128:
			status |= ar_decompose_complex (&re, &im, &reimtype,
						        opnd, opndtype);
			restat = AR_status ((const AR_DATA*)&re, &reimtype);
			imstat = AR_status ((const AR_DATA*)&im, &reimtype);
			status |= restat & imstat & AR_STAT_ZERO;
			status |= (restat | imstat) &
				  (AR_STAT_OVERFLOW | AR_STAT_UNDEFINED |
				   AR_STAT_UNDERFLOW | AR_STAT_INVALID_TYPE);
			break;

		default:
			return AR_STAT_INVALID_TYPE;

		}

		return status;
	}
	
	if (AR_CLASS (*opndtype) == AR_CLASS_LOGICAL) {
		if (!(opnd->ar_i64.part1 | opnd->ar_i64.part2 |
		      opnd->ar_i64.part3 | opnd->ar_i64.part4))
			status = AR_STAT_ZERO;
		return status;
	}

	return AR_STAT_INVALID_TYPE;
}


/* Compute the value of one. Really. */
int
AR_one (AR_DATA *res, const AR_TYPE *type) {

	ar_data* result = (ar_data*)res;

	switch (*type) {
	case AR_Int_16_S:
	case AR_Int_16_U:
	case AR_Int_32_S:
	case AR_Int_32_U:
	case AR_Int_46_S:
	case AR_Int_64_S:
	case AR_Int_64_U:
		result->ar_i64.part1 = 0;
		result->ar_i64.part2 = 0;
		result->ar_i64.part3 = 0;
		result->ar_i64.part4 = 1;
		break;
	case AR_Float_Cray1_64:
	case AR_Float_Cray1_64_F:
		ZEROCRAY64 (result->ar_f64);
		result->ar_f64.expo = AR_CRAY_EXPO_BIAS;
		result->ar_f64.coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		break;
	case AR_Float_Cray1_128:
		ZEROCRAY128 (result->ar_f128);
		result->ar_f128.expo = AR_CRAY_EXPO_BIAS;
		result->ar_f128.coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		break;
	case AR_Float_IEEE_NR_32:
	case AR_Float_IEEE_ZE_32:
	case AR_Float_IEEE_UP_32:
	case AR_Float_IEEE_DN_32:
		ZEROIEEE32 (result->ar_ieee32);
		result->ar_ieee32.expo = AR_IEEE32_EXPO_BIAS;
		break;
	case AR_Float_IEEE_NR_64:
	case AR_Float_IEEE_ZE_64:
	case AR_Float_IEEE_UP_64:
	case AR_Float_IEEE_DN_64:
		ZEROIEEE64 (result->ar_ieee64);
		result->ar_ieee64.expo = AR_IEEE64_EXPO_BIAS;
		break;
	case AR_Float_IEEE_NR_128:
	case AR_Float_IEEE_ZE_128:
	case AR_Float_IEEE_UP_128:
	case AR_Float_IEEE_DN_128:
		ZEROIEEE128 (result->ar_ieee128);
		if (HOST_IS_MIPS)
			result->ar_mips128.expo = AR_MIPS128_EXPO_BIAS;
		else
			result->ar_ieee128.expo = AR_IEEE128_EXPO_BIAS;
		break;
	case AR_Complex_Cray1_64:
	case AR_Complex_Cray1_64_F:
		ZEROCRAY64 (result->ar_cplx_f64.real);
		result->ar_cplx_f64.real.expo = AR_CRAY_EXPO_BIAS;
		result->ar_cplx_f64.real.coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		ZEROCRAY64 (result->ar_cplx_f64.imag);
		break;
	case AR_Complex_Cray1_128:
		ZEROCRAY128 (result->ar_cplx_f128.real);
		result->ar_cplx_f128.real.expo = AR_CRAY_EXPO_BIAS;
		result->ar_cplx_f128.real.coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		ZEROCRAY128 (result->ar_cplx_f128.imag);
		break;
	case AR_Complex_IEEE_NR_32:
	case AR_Complex_IEEE_ZE_32:
	case AR_Complex_IEEE_UP_32:
	case AR_Complex_IEEE_DN_32:
		result->ar_cplx_ieee32.rsign = 0;
		result->ar_cplx_ieee32.rexpo = AR_IEEE32_EXPO_BIAS;
		result->ar_cplx_ieee32.rcoeff0 = 0;
		result->ar_cplx_ieee32.rcoeff1 = 0;

		result->ar_cplx_ieee32.isign = 0;
		result->ar_cplx_ieee32.iexpo = 0;
		result->ar_cplx_ieee32.icoeff0 = 0;
		result->ar_cplx_ieee32.icoeff1 = 0;
		break;
	case AR_Complex_IEEE_NR_64:
	case AR_Complex_IEEE_ZE_64:
	case AR_Complex_IEEE_UP_64:
	case AR_Complex_IEEE_DN_64:
		ZEROIEEE64 (result->ar_cplx_ieee64.real);
		result->ar_cplx_ieee64.real.expo = AR_IEEE64_EXPO_BIAS;
		ZEROIEEE64 (result->ar_cplx_ieee64.imag);
		break;
	case AR_Complex_IEEE_NR_128:
	case AR_Complex_IEEE_ZE_128:
	case AR_Complex_IEEE_UP_128:
	case AR_Complex_IEEE_DN_128:
		ZEROIEEE128 (result->ar_cplx_ieee128.real);
		if (HOST_IS_MIPS)
			result->ar_cplx_mips128.real.expo =
			    AR_MIPS128_EXPO_BIAS;
		else
			result->ar_cplx_ieee128.real.expo =
			    AR_IEEE128_EXPO_BIAS;
		ZEROIEEE128 (result->ar_cplx_ieee128.imag);
		break;
	default:
		return AR_STAT_INVALID_TYPE;
	}

	return AR_STAT_OK;
}


/* Absolute value */
int
AR_abs (AR_DATA *res, const AR_TYPE *resulttype,
  const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	int status;

	if (AR_CLASS (*resulttype) == AR_CLASS_INT) {
		if (*opndtype != *resulttype)
			return AR_STAT_INVALID_TYPE;
		switch (AR_INT_SIZE (*opndtype)) {
		case AR_INT_SIZE_8:
			if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			    INT8_SIGN(opnd)) {
				status = ar_negate_integer (result, resulttype,
							    opnd, opndtype);
				if(status & AR_STAT_NEGATIVE) {
					status &= ~ AR_STAT_SEMIVALID;
					status |= AR_STAT_OVERFLOW;
				}
				return status;
			}

			ZERO_INT8_UPPER(result);
			COPY_INT8(result, opnd);
			return AR_status ((const AR_DATA*)result,
					  resulttype);

		case AR_INT_SIZE_16:
			if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			    INT16_SIGN(opnd)) {
				status = ar_negate_integer (result, resulttype,
							    opnd, opndtype);
				if(status & AR_STAT_NEGATIVE) {
					status &= ~ AR_STAT_SEMIVALID;
					status |= AR_STAT_OVERFLOW;
				}
				return status;
			}

			ZERO_INT16_UPPER(result);
			COPY_INT16(result, opnd);
			return AR_status ((const AR_DATA*)result,
					  resulttype);

		case AR_INT_SIZE_32:
			if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			    INT32_SIGN(opnd)) {
				status = ar_negate_integer (result, resulttype,
							    opnd, opndtype);
				if(status & AR_STAT_NEGATIVE) {
					status &= ~ AR_STAT_SEMIVALID;
					status |= AR_STAT_OVERFLOW;
				}
				return status;
			}

			ZERO_INT32_UPPER(result);
			COPY_INT32(result, opnd);
			return AR_status ((const AR_DATA*)result,
					  resulttype);

		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			if (AR_SIGNEDNESS (*resulttype) == AR_SIGNED &&
			    INT64_SIGN(opnd)) {
				status = ar_negate_integer (result, resulttype,
							    opnd, opndtype);
				if(status & AR_STAT_NEGATIVE) {
					status &= ~ AR_STAT_SEMIVALID;
					status |= AR_STAT_OVERFLOW;
				}
				return status;
			}

			COPY_INT64(result, opnd);
			return AR_status ((const AR_DATA*)result,
					  resulttype);

		default:
			return (AR_STAT_INVALID_TYPE);
		}
	}

	if (AR_CLASS (*resulttype) == AR_CLASS_FLOAT) {

		if (AR_FLOAT_IS_COMPLEX (*opndtype) == AR_FLOAT_COMPLEX)
			return ar_cabs (result, resulttype, opnd, opndtype);

		/* Floating-point absolute value */
		if (*opndtype != *resulttype)
			return AR_STAT_INVALID_TYPE;
		if (AR_FLOAT_FORMAT (*resulttype) == AR_FLOAT_CRAY) {
			if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_64) {
				result->ar_f64 = opnd->ar_f64;
				result->ar_f64.sign = 0;
				return AR_status ((const AR_DATA*)result, resulttype);
			}
			if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_128) {
				result->ar_f128 = opnd->ar_f128;
				result->ar_f128.sign = 0;
				return AR_status ((const AR_DATA*)result, resulttype);
			}
			return AR_STAT_INVALID_TYPE;
		}
		if (AR_FLOAT_FORMAT (*resulttype) == AR_FLOAT_IEEE)
			if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_64) {
				result->ar_ieee64 = opnd->ar_ieee64;
				result->ar_ieee64.sign = 0;
				return AR_status ((const AR_DATA*)result, resulttype);
			} else if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_32) {
				result->ar_ieee32 = opnd->ar_ieee32;
				result->ar_ieee32.sign = 0;
				return AR_status ((const AR_DATA*)result, resulttype);
			} else if (AR_FLOAT_SIZE (*resulttype) == AR_FLOAT_128) {
			    if (HOST_IS_MIPS) {
				result->ar_mips128 = opnd->ar_mips128;
				if (result->ar_mips128.sign) {
				    /* zero high sign, flip low sign */
				    result->ar_mips128.sign = 0;
				    result->ar_mips128.signl ^= 1;
				}
			    }
			    else {
				result->ar_ieee128 = opnd->ar_ieee128;
				result->ar_ieee128.sign = 0;
			    }
			    return AR_status ((const AR_DATA*)result,
					      resulttype);
			} else
				return AR_STAT_INVALID_TYPE;
		return AR_STAT_INVALID_TYPE;
	}

	/* Neither integer or floating-point */
	return AR_STAT_INVALID_TYPE;
}


/* Complex conjugate */
int
AR_conj (AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* result = (ar_data*)res;
	ar_data* opnd   = (ar_data*)opd;

	int status;
	ar_data re, im, negim;
	AR_TYPE reimtype, temptype;

	status = ar_decompose_complex (&re, &im, &reimtype, opnd, opndtype);
	status |= ar_negate_float (&negim, &reimtype, &im, &reimtype);
	status |= ar_compose_complex (result, &temptype,
				      &re, &negim, &reimtype);
	status &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
	return status | AR_status ((const AR_DATA*)result, resulttype);
}



/* Complex construction: a -> (0, ai) */
int
AR_make_imag
		(AR_DATA *result, const AR_TYPE *resulttype,
   const AR_DATA *opnd,   const AR_TYPE *opndtype) {
   return (AR_make_complex(result, resulttype, &AR_const_zero, opndtype,
                                               opnd, opndtype));
}

/* Complex construction: a,b -> (a, bi) */
int
AR_make_complex
		(AR_DATA *res, const AR_TYPE *resulttype,
   const AR_DATA *op1, const AR_TYPE *opnd1type,
   const AR_DATA *op2, const AR_TYPE *opnd2type) {

	/*
	 * Opnd1 and opnd2 are local copies, in case one or the other
	 * is an alias for result.  It may be that only the case where
	 * result and opnd2 are aliased is a problem, but doing both
	 * operands the same way retains symmetry in the source, and
	 * removes the need to analyze the opnd1 case.
	 */

	ar_data* result =  (ar_data*)res;
	ar_data  opnd1  = *(ar_data*)op1;
	ar_data  opnd2  = *(ar_data*)op2;

	if (*opnd1type != *opnd2type ||
	    AR_CLASS (*opnd1type) != AR_CLASS_FLOAT ||
	    AR_FLOAT_IS_COMPLEX (*opnd1type) == AR_FLOAT_COMPLEX ||
	    *resulttype != (*opnd1type | AR_FLOAT_COMPLEX))
		return AR_STAT_INVALID_TYPE;

	switch (*opnd1type) {
	case AR_Float_Cray1_64:
	case AR_Float_Cray1_64_F:
		result->ar_cplx_f64.real = opnd1.ar_f64;
		result->ar_cplx_f64.imag = opnd2.ar_f64;
		break;
	case AR_Float_Cray1_128:
		result->ar_cplx_f128.real = opnd1.ar_f128;
		result->ar_cplx_f128.imag = opnd2.ar_f128;
		break;
	case AR_Float_IEEE_NR_32:
	case AR_Float_IEEE_ZE_32:
	case AR_Float_IEEE_UP_32:
	case AR_Float_IEEE_DN_32:
		IEEE32_TO_CPLX32_REAL(result->ar_cplx_ieee32, opnd1.ar_ieee32);
		IEEE32_TO_CPLX32_IMAG(result->ar_cplx_ieee32, opnd2.ar_ieee32);
		break;
	case AR_Float_IEEE_NR_64:
	case AR_Float_IEEE_ZE_64:
	case AR_Float_IEEE_UP_64:
	case AR_Float_IEEE_DN_64:
		result->ar_cplx_ieee64.real = opnd1.ar_ieee64;
		result->ar_cplx_ieee64.imag = opnd2.ar_ieee64;
		break;
	case AR_Float_IEEE_NR_128:
	case AR_Float_IEEE_ZE_128:
	case AR_Float_IEEE_UP_128:
	case AR_Float_IEEE_DN_128:
		result->ar_cplx_ieee128.real = opnd1.ar_ieee128;
		result->ar_cplx_ieee128.imag = opnd2.ar_ieee128;
		break;
	default:
		return AR_STAT_INVALID_TYPE;
	}

	return AR_status ((const AR_DATA*)result, resulttype);
}


/* Utility routine to force unused bits to zero. */
void
ar_clear_unused_bits (ar_data *opnd, const AR_TYPE *opndtype) {

	if (AR_CLASS (*opndtype) == AR_CLASS_INT) {
		switch (AR_INT_SIZE (*opndtype)) {
		case AR_INT_SIZE_8:
			ZERO_INT8_UPPER(opnd);
			break;
		case AR_INT_SIZE_16:
			ZERO_INT16_UPPER(opnd);
			break;
		case AR_INT_SIZE_24:
			ZERO_INT24_UPPER(opnd);
			break;
		case AR_INT_SIZE_32:
			ZERO_INT32_UPPER(opnd);
			break;
		}
		return;
	}

	if (AR_CLASS (*opndtype) == AR_CLASS_POINTER) {
		if (AR_POINTER_FORMAT (*opndtype) == AR_POINTER_WORD) {
			if (AR_POINTER_SIZE (*opndtype) == AR_POINTER_32)
				opnd->ar_i64.part1 = opnd->ar_i64.part2 = 0;
			else if(AR_POINTER_SIZE(*opndtype) == AR_POINTER_24) {
				opnd->ar_i64.part1 = opnd->ar_i64.part2 = 0;
				opnd->ar_i64.part3 &= 0xFF;
			}
		}
		return;
	}

	ar_internal_error (2004, __FILE__, __LINE__);
}


int
AR_CRAY_64_trunc_bits(int truncbits)
{
   if (truncbits < 0 || truncbits >= AR_CRAY64_COEFF_BITS)
      return AR_STAT_UNDEFINED;

   ar_state_register.ar_truncate_bits = truncbits;
   return AR_STAT_OK;
}


void
ar_CRAY_64_trunc(AR_CRAY_64 *opnd)
{
	int	ntruncated_bits = ar_state_register.ar_truncate_bits;

   if (ntruncated_bits < 16)
   {
      opnd->coeff2 &= ~((1 << ntruncated_bits) - 1);
   }
   else if (ntruncated_bits < 32)
   {
      opnd->coeff2 = 0;
      opnd->coeff1 &= ~((1 << ntruncated_bits-16) - 1);
   }
   else if (ntruncated_bits < 48)
   {
      opnd->coeff2 = 0;
      opnd->coeff1 = 0;
      opnd->coeff0 &= ~((1 << ntruncated_bits-32) - 1);
   }
   else
      ar_internal_error (2008, __FILE__, __LINE__);
}

/* Routine to generate an invalid result value based on type */
void
ar_set_invalid_result(ar_data *result, const AR_TYPE *resulttype)
{
	switch (AR_CLASS(*resulttype)) {

	case AR_CLASS_INT:		/* Generate -MAXINT */
		ZERO64(result->ar_i64);
		switch (AR_INT_SIZE(*resulttype)) {
		case AR_INT_SIZE_8:
			result->ar_i8.part5  = 1 <<  7;
			break;
		case AR_INT_SIZE_16:
			result->ar_i64.part4 = 1 << 15;
			break;
		case AR_INT_SIZE_24:
			result->ar_i64.part3 = 1 <<  7;
			break;
		case AR_INT_SIZE_32:
			result->ar_i64.part3 = 1 << 15;
			break;
		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			result->ar_i64.part1 = 1 << 15;
			break;
		case AR_INT_SIZE_128:
			result->ar_i128.part5 = result->ar_i128.part6 =
			result->ar_i128.part7 = result->ar_i128.part8 = 0;
			result->ar_i128.part1 = 1 << 15;
			break;
		}
		break;

	case AR_CLASS_FLOAT:		/* Generate -Inf */
		switch (AR_FLOAT_SIZE(*resulttype)) {
		case AR_FLOAT_32:
			ZEROIEEE32 (result->ar_ieee32);
			result->ar_ieee32.expo = AR_IEEE32_MAX_EXPO+1;
			break;
		case AR_FLOAT_64:
			if (AR_FLOAT_FORMAT(*resulttype) == AR_FLOAT_IEEE) {
				ZEROIEEE64 (result->ar_ieee64);
				result->ar_ieee64.expo = AR_IEEE64_MAX_EXPO+1;
			}
			else {
				ZEROCRAY64(result->ar_f64);
				result->ar_f64.expo = AR_CRAY_MAX_EXPO+1;
			}
			break;
		case AR_FLOAT_128:
			if (AR_FLOAT_FORMAT(*resulttype) == AR_FLOAT_IEEE) {
				ZEROIEEE128 (result->ar_ieee128);
				result->ar_ieee128.expo = AR_IEEE128_MAX_EXPO+1;
			}
			else {
				ZEROCRAY128(result->ar_f128);
				result->ar_f128.expo = AR_CRAY_MAX_EXPO+1;
			}
			break;
		}
		break;

	default:			/* Generate all 1 bits */
		ZERO64(result->ar_i64);
		NEG64(result->ar_i64);
		break;
	}
}

void
#if defined(__sparc__) || defined(__mips)
ar_nointrin_error_(char* intrin_name) {
	char* name = intrin_name;
#else
#define _fcdtocp(f)     ((char *)(((long)(f))&0xfc000000ffffffff))
#define _fcdlen(f)      ((unsigned)((((long)(f))>>35)&0x7fffff))
AR_NOINTRIN_ERROR(char* intrin_name) {
	int	i;
	char*	name = _fcdtocp(intrin_name);
	for(i=0; i<_fcdlen(intrin_name); i++)
		if(!isalnum(name[i])) break;
	name[i] = '\0';
#endif
	ar_internal_error(2017, name, 1);
}

void
ar_internal_error (int msgnum, char *file, int line) {

	extern char* AR_version;
	char nullptr[] = "";

        PRINTMSG(0, msgnum, Internal, 0, file, line, nullptr, nullptr);
}


/*
 * What architecture is this?
 */
AR_ARCHITECTURE
ar_host(void)
{

#if defined(__mips)
	return AR_Arch_MIPS;
#elif defined(__sun)
	return AR_Arch_SPARC;
#else
	static int		initialized = 0;
	static AR_ARCHITECTURE	host_arch;

	an_mc_table		mctable;
	AR_HOST_SINT64		pdtword;
	AR_HOST_SINT64		pdtstart;
	AR_HOST_SINT64		pdtlen;
	AR_HOST_SINT64		mctidx;
	AR_HOST_SINT64		mctype;
	AR_HOST_SINT64		mcdef;
	char			host_name[9];

	if (!initialized) {
		initialized = 1;

		host_arch = AR_Arch_Unknown;

		if (!GETPMC(mctable, "*host")) {
			ar_internal_error(2019, __FILE__, __LINE__);
		}

		if (!CHECKMC("primary", &pdtword, &pdtstart, &pdtlen,
			     &mctidx, &mctype, &mcdef, "*HOST")) {
			ar_internal_error(2019, __FILE__, __LINE__);
		}

		strncpy(host_name, (char *) &mctable[mctidx], 8);
		host_name[8] = '\0';

		if (strcmp(host_name, "CRAY-XMP") == 0 ||
		    strcmp(host_name, "CRAY-YMP") == 0 ||
		    strcmp(host_name, "CRAY-C90") == 0) {
		    host_arch = AR_Arch_PVP;
		}
		else if (strcmp(host_name, "CRAY-TS") == 0) {
			(void) CHECKMC("ieee", &pdtword, &pdtstart, &pdtlen,
				       &mctidx, &mctype, &mcdef, "*HOST");
			if (mctable[mctidx]) {
				host_arch = AR_Arch_PVP_IEEE;
			}
			else {
				host_arch = AR_Arch_PVP;
			}
		}
		else if (strcmp(host_name,"CRAY-T3D") == 0) {
			host_arch = AR_Arch_T3D;
		}
		else if (strcmp(host_name,"CRAY-T3E") == 0) {
			host_arch = AR_Arch_T3E;
		}
	}

	return host_arch;
#endif
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: miscmath.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
