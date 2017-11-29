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


#include <stdio.h>
#if defined _CRAY && !defined _CRAYMPP

/* Native library routines are never used on Cray systems */

#else	/* ! _CRAY */

#include <signal.h>
#include <setjmp.h>
#include <errno.h>

#include "arith.internal.h"
#include "int64.h"

int ar_rounding_modes = 0xf;	/* All rounding modes allowed */
int ar_underflow_modes = 1<<AR_UNDERFLOW_TO_DENORM;

#if _Solaris || defined(_CRAYMPP) || defined(__mips)

/* Call native, F90-compiled routines to evaluate all functions */

#if !defined(__mips)
#	define NULL				0
#endif
#define IEEE_FLOAT_32		(UNROUNDED_TYPE(AR_Float_IEEE_NR_32))
#define IEEE_FLOAT_64		(UNROUNDED_TYPE(AR_Float_IEEE_NR_64))
#define IEEE_FLOAT_128		(UNROUNDED_TYPE(AR_Float_IEEE_NR_128))
#define IEEE_COMPLEX_32		(UNROUNDED_TYPE(AR_Complex_IEEE_NR_32))
#define IEEE_COMPLEX_64		(UNROUNDED_TYPE(AR_Complex_IEEE_NR_64))
#define IEEE_COMPLEX_128	(UNROUNDED_TYPE(AR_Complex_IEEE_NR_128))

int		status;

static int ar_native1(void (function)(), ar_data *result, const AR_TYPE *resulttype, const ar_data *opnd);

static int ar_native2(void (function)(), ar_data *result, const AR_TYPE *resulttype, const ar_data *opnd1, const ar_data *opnd2);

/* Fortran character index */
int
ar_index (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, long len1, const char *str2, long len2, long backward)
{
	long	index;
	long	back = backward;
        int	status;

	extern long _F90_INDEX(const char *st1, const char *st2, long *back,
			       int len1, int len2);

        index = _F90_INDEX(str1, str2, &back, len1, len2);
        result->ar_i64.part1 = 0;
        result->ar_i64.part2 = 0;
        result->ar_i64.part3 = index>>16;
        result->ar_i64.part4 = index & 0xffff;

        status = AR_STAT_OK;
	switch (*resulttype) {
	case AR_Int_8_S:
		if (( INT8_SIGN(result) && !IS_INT8_UPPER_ONES(result)) ||
		    (!INT8_SIGN(result) && !IS_INT8_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_16_S:
		if (( INT16_SIGN(result) && !IS_INT16_UPPER_ONES(result)) ||
		    (!INT16_SIGN(result) && !IS_INT16_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_32_S:
		if (( INT32_SIGN(result) && !IS_INT32_UPPER_ONES(result)) ||
		    (!INT32_SIGN(result) && !IS_INT32_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_64_S:
                break;

	default:
		return AR_STAT_INVALID_TYPE;
	}
	WORD_SWAP(result->ar_i64);
	return AR_status((AR_DATA*)result, resulttype);
}


/* Fortran character scan */
int
ar_scan (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, long len1, const char *str2, long len2, long backward)
{
	long	index;
	long	back = backward;
        int	status;

	extern long _F90_SCAN(const char *st1, const char *st2, long *back,
			       int len1, int len2);


        index = _F90_SCAN(str1, str2, &back, len1, len2);
        result->ar_i64.part1 = 0;
        result->ar_i64.part2 = 0;
        result->ar_i64.part3 = index>>16;
        result->ar_i64.part4 = index & 0xffff;

        status = AR_STAT_OK;
	switch (*resulttype) {
	case AR_Int_8_S:
		if (( INT8_SIGN(result) && !IS_INT8_UPPER_ONES(result)) ||
		    (!INT8_SIGN(result) && !IS_INT8_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_16_S:
		if (( INT16_SIGN(result) && !IS_INT16_UPPER_ONES(result)) ||
		    (!INT16_SIGN(result) && !IS_INT16_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_32_S:
		if (( INT32_SIGN(result) && !IS_INT32_UPPER_ONES(result)) ||
		    (!INT32_SIGN(result) && !IS_INT32_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_64_S:
                break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	WORD_SWAP(result->ar_i64);
        return AR_status((AR_DATA*)result, resulttype);
}


/* Fortran character verify */
int
ar_verify (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, long len1, const char *str2, long len2, long backward)
{
	long	index;
	long	back = backward;

	extern long _F90_VERIFY(const char *st1, const char *st2, long *back,
			       int len1, int len2);

        index = _F90_VERIFY(str1, str2, &back, len1, len2);
        result->ar_i64.part1 = 0;
        result->ar_i64.part2 = 0;
        result->ar_i64.part3 = index>>16;
        result->ar_i64.part4 = index & 0xffff;

        status = AR_STAT_OK;
	switch (*resulttype) {
	case AR_Int_8_S:
		if (( INT8_SIGN(result) && !IS_INT8_UPPER_ONES(result)) ||
		    (!INT8_SIGN(result) && !IS_INT8_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_16_S:
		if (( INT16_SIGN(result) && !IS_INT16_UPPER_ONES(result)) ||
		    (!INT16_SIGN(result) && !IS_INT16_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_32_S:
		if (( INT32_SIGN(result) && !IS_INT32_UPPER_ONES(result)) ||
		    (!INT32_SIGN(result) && !IS_INT32_UPPER_ZERO(result))) {
                        return AR_STAT_OVERFLOW;
                }
                break;

	case AR_Int_64_S:
                break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	WORD_SWAP(result->ar_i64);
        return AR_status((AR_DATA*)result, resulttype);
}


/* Fortran-90 reshape */
int
ar_reshape (void *result, const void *source, const void *shape,
	    const void *pad, const void *order)
{
	extern void _RESHAPE();

	if(*(char**)source == NULL || *(char**)shape == NULL)
		return AR_STAT_UNDEFINED;

	_RESHAPE(result, source, shape, pad, order);

	return AR_STAT_OK;
}


/* Fortran-90 transfer */
int
ar_transfer (void *result, const void *source, const void *mold, long *length)
{
	long	size;

	extern void _TRANSFER();

	if(*(char**)source == NULL || *(char**)mold == NULL)
		return AR_STAT_UNDEFINED;

	if(length != NULL) {
		size = *length;
		_TRANSFER(result, source, mold, &size);
	}
	else
		_TRANSFER(result, source, mold, (int*)NULL);

	return AR_STAT_OK;
}


/* Fortran-90 modulo */
int
ar_modulo (ar_data *result, const AR_TYPE *resulttype,
	   const ar_data *opnd1, const AR_TYPE *opnd1type,
	   const ar_data *opnd2, const AR_TYPE *opnd2type)
{
        ar_data	result_32;
        ar_data	opnd1_32;
        ar_data	opnd2_32;
        AR_TYPE	int32type = AR_Int_32_S;
	int	status;

#if !defined _CRAYMPP
#define ARMOD   armod_
#define ARMODD  armodd_
#define ARMODXD armodxd_
#define ARMODI  armodi_
#define ARMODJ  armodj_
#endif
	extern void ARMOD  (ar_data *res,
                            const ar_data *arg1, const ar_data *arg2);
	extern void ARMODD (ar_data *res,
                            const ar_data *arg1, const ar_data *arg2);
	extern void ARMODXD(ar_data *res,
                            const ar_data *arg1, const ar_data *arg2);
	extern void ARMODI (ar_data *res,
                            const ar_data *arg1, const ar_data *arg2);
	extern void ARMODJ (ar_data *res,
                            const ar_data *arg1, const ar_data *arg2);

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		return ar_native2(ARMOD, result, resulttype, opnd1, opnd2);

	case IEEE_FLOAT_64:
		return ar_native2(ARMODD, result, resulttype, opnd1, opnd2);

	case IEEE_FLOAT_128:
		return ar_native2(ARMODXD, result, resulttype, opnd1, opnd2);

	default:
		switch (*resulttype) {

		case AR_Int_8_S:
		case AR_Int_16_S:
			ZERO_INT32_ALL(&opnd1_32);
			ZERO_INT32_ALL(&opnd2_32);
			ZERO_INT32_ALL(&result_32);
                        (void) AR_convert((AR_DATA*) &opnd1_32, &int32type,
					  (AR_DATA*)opnd1, resulttype);
                        (void) AR_convert((AR_DATA*) &opnd2_32, &int32type,
					  (AR_DATA*)opnd2, resulttype);
			status = ar_native2(ARMODI,
                                            &result_32, &int32type,
                                            &opnd1_32, &opnd2_32);
                        if (IS_ERROR_STATUS(status))
                                return status;
                        return AR_convert((AR_DATA*) result, resulttype,
                                          (AR_DATA*) &result_32, &int32type);

		case AR_Int_32_S:
			return ar_native2(ARMODI,
                                          result, resulttype, opnd1, opnd2);

		case AR_Int_64_S:
			return ar_native2(ARMODJ,
                                          result, resulttype, opnd1, opnd2);

		default:
			return AR_STAT_INVALID_TYPE;
		}

	}
}


/* Fortran-90 selected_real_kind */
int
ar_selected_real_kind (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd1, const AR_TYPE *opnd1type,
	 const ar_data *opnd2, const AR_TYPE *opnd2type)
{
	int	status;
	AR_TYPE	int32type = AR_Int_32_S;

#if !defined _CRAYMPP
#define ARSELRK arselrk_
#endif
	extern void ARSELRK(ar_data *res, const ar_data *arg1, const ar_data *arg2);

	ZERO_INT32_UPPER(result);
	status = ar_native2(ARSELRK, result, &int32type, opnd1, opnd2);

	switch (*resulttype) {

	case AR_Int_8_S:
	case AR_Int_16_S:
		if (!IS_ERROR_STATUS(status))
			status = AR_convert((AR_DATA*) result, resulttype,
					    (AR_DATA*) result, &int32type);
		break;

	case AR_Int_32_S:
                break;

	case AR_Int_64_S:
		if(result->ar_i64.part3 & 0x8000)
			result->ar_i64.part1 = result->ar_i64.part2 = 0xFFFF;
                break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	return status;
}


/* Square root */
int
ar_sqrt (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype)
{
#if !defined _CRAYMPP
#define	ARSQRT    arsqrt_
#define	ARDSQRT   ardsqrt_
#define	ARXDSQRT  arxdsqrt_
#define	ARCSQRT   arcsqrt_
#define ARCDSQRT  arcdsqrt_
#define ARCXDSQRT arcxdsqrt_
#endif
	extern void ARSQRT   (ar_data *res, const ar_data *arg);
	extern void ARDSQRT  (ar_data *res, const ar_data *arg);
	extern void ARXDSQRT (ar_data *res, const ar_data *arg);
	extern void ARCSQRT  (ar_data *res, const ar_data *arg);
	extern void ARCDSQRT (ar_data *res, const ar_data *arg);
	extern void ARCXDSQRT(ar_data *res, const ar_data *arg);

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		return ar_native1(ARSQRT, result, resulttype, opnd);

	case IEEE_FLOAT_64:
		return ar_native1(ARDSQRT, result, resulttype, opnd);

	case IEEE_FLOAT_128:
		return ar_native1(ARXDSQRT, result, resulttype, opnd);

	case IEEE_COMPLEX_32:
		return ar_native1(ARCSQRT, result, resulttype, opnd);

	case IEEE_COMPLEX_64:
		return ar_native1(ARCDSQRT, result, resulttype, opnd);

	case IEEE_COMPLEX_128:
		return ar_native1(ARCXDSQRT, result, resulttype, opnd);

	default:
		return AR_STAT_INVALID_TYPE;
	}
}


/* Natural (base "e") logarithm */
int
ar_log (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype)
{
#if !defined _CRAYMPP
#define	ARLOG	 arlog_
#define	ARDLOG	 ardlog_
#define	ARXDLOG	 arxdlog_
#define	ARCLOG	 arclog_
#define ARCDLOG	 arcdlog_
#define ARCXDLOG arcxdlog_
#endif
	extern void ARLOG   (ar_data *res, const ar_data *arg);
	extern void ARDLOG  (ar_data *res, const ar_data *arg);
	extern void ARXDLOG (ar_data *res, const ar_data *arg);
	extern void ARCLOG  (ar_data *res, const ar_data *arg);
	extern void ARCDLOG (ar_data *res, const ar_data *arg);
	extern void ARCXDLOG(ar_data *res, const ar_data *arg);

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		return ar_native1(ARLOG, result, resulttype, opnd);

	case IEEE_FLOAT_64:
		return ar_native1(ARDLOG, result, resulttype, opnd);

	case IEEE_FLOAT_128:
		return ar_native1(ARXDLOG, result, resulttype, opnd);

	case IEEE_COMPLEX_32:
		return ar_native1(ARCLOG, result, resulttype, opnd);

	case IEEE_COMPLEX_64:
		return ar_native1(ARCDLOG, result, resulttype, opnd);

	case IEEE_COMPLEX_128:
		return ar_native1(ARCXDLOG, result, resulttype, opnd);

	default:
		return AR_STAT_INVALID_TYPE;
	}
}


/* Exponential ("e" ** x) function */
int
ar_exp (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype)
{
#if !defined _CRAYMPP
#define	AREXP	 arexp_
#define	ARDEXP	 ardexp_
#define	ARXDEXP	 arxdexp_
#define	ARCEXP	 arcexp_
#define ARCDEXP	 arcdexp_
#define ARCXDEXP arcxdexp_
#endif
	extern void AREXP   (ar_data *res, const ar_data *arg);
	extern void ARDEXP  (ar_data *res, const ar_data *arg);
	extern void ARXDEXP (ar_data *res, const ar_data *arg);
	extern void ARCEXP  (ar_data *res, const ar_data *arg);
	extern void ARCDEXP (ar_data *res, const ar_data *arg);
	extern void ARCXDEXP(ar_data *res, const ar_data *arg);

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		return ar_native1(AREXP, result, resulttype, opnd);

	case IEEE_FLOAT_64:
		return ar_native1(ARDEXP, result, resulttype, opnd);

	case IEEE_FLOAT_128:
		return ar_native1(ARXDEXP, result, resulttype, opnd);

	case IEEE_COMPLEX_32:
		return ar_native1(ARCEXP, result, resulttype, opnd);

	case IEEE_COMPLEX_64:
		return ar_native1(ARCDEXP, result, resulttype, opnd);

	case IEEE_COMPLEX_128:
		return ar_native1(ARCXDEXP, result, resulttype, opnd);

	default:
		return AR_STAT_INVALID_TYPE;
	}
}


/* Complex absolute value */
int
ar_cabs (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd, const AR_TYPE *opndtype)
{
#if !defined _CRAYMPP
#define	ARCABS	 arcabs_
#define ARCDABS  arcdabs_
#define ARCXDABS arcxdabs_
#endif
	extern void ARCABS  (ar_data *res, const ar_data *arg);
	extern void ARCDABS (ar_data *res, const ar_data *arg);
	extern void ARCXDABS(ar_data *res, const ar_data *arg);

    if (UNROUNDED_TYPE(*resulttype) == IEEE_FLOAT_32 &&
        UNROUNDED_TYPE(*opndtype)   == IEEE_COMPLEX_32)
		return ar_native1(ARCABS, result, resulttype, opnd);

    if (UNROUNDED_TYPE(*resulttype) == IEEE_FLOAT_64 &&
        UNROUNDED_TYPE(*opndtype)   == IEEE_COMPLEX_64)
		return ar_native1(ARCDABS, result, resulttype, opnd);

    if (UNROUNDED_TYPE(*resulttype) == IEEE_FLOAT_128 &&
        UNROUNDED_TYPE(*opndtype)   == IEEE_COMPLEX_128)
		return ar_native1(ARCXDABS, result, resulttype, opnd);
              
	return AR_STAT_INVALID_TYPE;
}

/* Exponentiation */
int
ar_power(ar_data *result, const AR_TYPE *resulttype,
         const ar_data *base, const AR_TYPE *basetype,
         const ar_data *power, const AR_TYPE *powertype)
{
	int	status;
	ar_data	tbase, tpow;
	AR_TYPE	btype, ptype;
        ar_data	base_32;
        ar_data	tbase_32;
        ar_data	tpow_32;
        ar_data	result_32;
        AR_TYPE	int32type = AR_Int_32_S;

#if !defined _CRAYMPP
#define	ARPOWGG		arpowgg_
#define	ARPOWHH		arpowhh_
#define	ARPOWII		arpowii_
#define	ARPOWJJ		arpowjj_
#define	ARPOWRG		arpowrg_
#define	ARPOWRH		arpowrh_
#define	ARPOWRI		arpowri_
#define	ARPOWRJ		arpowrj_
#define	ARPOWDG		arpowdg_
#define	ARPOWDH		arpowdh_
#define	ARPOWDI		arpowdi_
#define	ARPOWDJ		arpowdj_
#define	ARPOWXDG	arpowxdg_
#define	ARPOWXDH	arpowxdh_
#define	ARPOWXDI	arpowxdi_
#define	ARPOWXDJ	arpowxdj_
#define	ARPOWCG		arpowcg_
#define	ARPOWCH		arpowch_
#define	ARPOWCI		arpowci_
#define	ARPOWCJ		arpowcj_
#define	ARPOWGR		arpowgr_
#define	ARPOWHR		arpowhr_
#define	ARPOWIR		arpowir_
#define	ARPOWJR		arpowjr_
#define	ARPOWRR		arpowrr_
#define	ARPOWDR		arpowdr_
#define	ARPOWXDR	arpowxdr_
#define	ARPOWCR		arpowcr_
#define	ARPOWDD		arpowdd_
#define	ARPOWXDXD	arpowxdxd_
#define	ARPOWCC		arpowcc_
#define ARPOWCDG	arpowcdg_
#define ARPOWCDH	arpowcdh_
#define ARPOWCDI	arpowcdi_
#define ARPOWCDJ	arpowcdj_
#define ARPOWCXDG	arpowcxdg_
#define ARPOWCXDH	arpowcxdh_
#define ARPOWCXDI	arpowcxdi_
#define ARPOWCXDJ	arpowcxdj_
#define ARPOWCDCD	arpowcdcd_
#define ARPOWCXDCXD	arpowcxdcxd_
#endif

	extern void ARPOWGG	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWHH	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWII	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWJJ	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWRG	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWRH	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWRI	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWRJ	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWDG	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWDH	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWDI	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWDJ	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWXDG	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWXDH	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWXDI	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWXDJ	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCG	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCH	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCI	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCJ	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWGR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWHR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWIR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWJR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWRR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWDR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWXDR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCR	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWDD	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWXDXD	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCC	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCDG	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCDH	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCDI	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCDJ	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCXDG	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCXDH	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCXDI	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCXDJ	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCDCD	(ar_data *res, const ar_data *base,
					       const ar_data *power);
	extern void ARPOWCXDCXD	(ar_data *res, const ar_data *base,
					       const ar_data *power);

	/* Prepare for power function evalution by converting
	 * base and power operand types to values supported by
	 * the native power functions.
	 */

	if(AR_CLASS(*basetype) == AR_CLASS_INT) {

		if(UNROUNDED_TYPE(*powertype) == IEEE_FLOAT_32) {
			if(*resulttype == *powertype) {
				switch (*basetype) {
				case AR_Int_8_S:
				case AR_Int_16_S:
					ZERO_INT32_ALL(&base_32);
					(void) AR_convert((AR_DATA*) &base_32,
							  &int32type,
							  (AR_DATA*)base,
							  basetype);
					return ar_native2(ARPOWIR,
							  result, resulttype,
							  &base_32, power);
				case AR_Int_32_S:
					return ar_native2(ARPOWIR,
							  result, resulttype,
							  base, power);
				case AR_Int_64_S:
					return ar_native2(ARPOWJR,
							  result, resulttype,
							  base, power);
				default:
					return AR_STAT_INVALID_TYPE;
				}
			}
			else
				return AR_STAT_INVALID_TYPE;
		}

		btype = ptype = *powertype;
	}
	else if(AR_CLASS(*powertype) == AR_CLASS_INT ||
	        (AR_FLOAT_SIZE(*powertype) == AR_FLOAT_32 &&
		 AR_FLOAT_IS_COMPLEX(*powertype) != AR_FLOAT_COMPLEX)) {

		/* base**I or base**R power functions */

		btype = *basetype;
		ptype = *powertype;
	}

	/* Otherwise, process arg types to simulate power function with
	 * base type == power type using the greatest precision and/or
	 * generality required.
	 */

	else {

		/* Convert to base type == power type using the greatest
		 * precision and/or generality required.
		 */

		if(AR_FLOAT_SIZE(*basetype) >= AR_FLOAT_SIZE(*powertype))
		    btype = (AR_TYPE)
			    (*basetype | AR_FLOAT_IS_COMPLEX(*powertype));
		else
		    btype = (AR_TYPE)
			    (*powertype | AR_FLOAT_IS_COMPLEX(*basetype));

		ptype = btype;
	}

	/*
	 * Verify that the resulttype matches the simulated function's
	 * return type given by the expanded base type.  
	 */

	if(*resulttype != btype)
		return AR_STAT_INVALID_TYPE;

	/*
	 * Setup the operands to the power function converting to the
	 * correct (expanded) type if necessary.
	 */

	status = AR_STAT_OK;
	if(*basetype != btype)
		status = AR_convert((AR_DATA*)&tbase, &btype,
				    (AR_DATA*)base, basetype);
	else
		tbase = *base;

	if(*powertype != ptype)
		status = AR_convert((AR_DATA*)&tpow, &ptype,
				    (AR_DATA*)power, powertype);
	else
		tpow = *power;

	if (IS_ERROR_STATUS(status))
		return status;

	/*
	 * Call the correct native power function determined by
	 * the (expanded) base and power types.
	 */

	switch (UNROUNDED_TYPE(btype)) {

	case IEEE_FLOAT_32:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			switch (ptype) {
			case AR_Int_8_S:
			case AR_Int_16_S:
				ZERO_INT32_ALL(&tpow_32);
				(void) AR_convert((AR_DATA*) &tpow_32,
						  &int32type,
						  (AR_DATA*) &tpow,
						  &ptype);
				return ar_native2(ARPOWRI,
						  result, resulttype,
						  &tbase, &tpow_32);
			case AR_Int_32_S:
				return ar_native2(ARPOWRI,
						  result, resulttype,
						  &tbase, &tpow);
			case AR_Int_64_S:
				return ar_native2(ARPOWRJ,
						  result, resulttype,
						  &tbase, &tpow);
			default:
				return AR_STAT_INVALID_TYPE;
			}
		else
			return ar_native2(ARPOWRR,
					  result, resulttype,
					  &tbase, &tpow);

	case IEEE_FLOAT_64:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			switch (ptype) {
			case AR_Int_8_S:
			case AR_Int_16_S:
				ZERO_INT32_ALL(&tpow_32);
				(void) AR_convert((AR_DATA*) &tpow_32,
						  &int32type,
						  (AR_DATA*) &tpow,
						  &ptype);
				return ar_native2(ARPOWDI,
						  result, resulttype,
						  &tbase, &tpow_32);
			case AR_Int_32_S:
				return ar_native2(ARPOWDI,
						  result, resulttype,
						  &tbase, &tpow);
			case AR_Int_64_S:
				return ar_native2(ARPOWDJ,
						  result, resulttype,
						  &tbase, &tpow);
			default:
				return AR_STAT_INVALID_TYPE;
			}
		else if(AR_FLOAT_SIZE(ptype) == AR_FLOAT_32)
			return ar_native2(ARPOWDR,
					  result, resulttype,
					  &tbase, &tpow);
		else
			return ar_native2(ARPOWDD,
					  result, resulttype,
					  &tbase, &tpow);

	case IEEE_FLOAT_128:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			switch (ptype) {
			case AR_Int_8_S:
			case AR_Int_16_S:
				ZERO_INT32_ALL(&tpow_32);
				(void) AR_convert((AR_DATA*) &tpow_32,
						  &int32type,
						  (AR_DATA*) &tpow,
						  &ptype);
				return ar_native2(ARPOWXDI,
						  result, resulttype,
						  &tbase, &tpow_32);
			case AR_Int_32_S:
				return ar_native2(ARPOWXDI,
						  result, resulttype,
						  &tbase, &tpow);
			case AR_Int_64_S:
				return ar_native2(ARPOWXDJ,
						  result, resulttype,
						  &tbase, &tpow);
			default:
				return AR_STAT_INVALID_TYPE;
			}
		else if(AR_FLOAT_SIZE(ptype) == AR_FLOAT_32)
			return ar_native2(ARPOWXDR,
					  result, resulttype,
					  &tbase, &tpow);
		else
			return ar_native2(ARPOWXDXD,
					  result, resulttype,
					  &tbase, &tpow);

	case IEEE_COMPLEX_32:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			switch (ptype) {
			case AR_Int_8_S:
			case AR_Int_16_S:
				ZERO_INT32_ALL(&tpow_32);
				(void) AR_convert((AR_DATA*) &tpow_32,
						  &int32type,
						  (AR_DATA*) &tpow,
						  &ptype);
				return ar_native2(ARPOWCI,
						  result, resulttype,
						  &tbase, &tpow_32);
			case AR_Int_32_S:
				return ar_native2(ARPOWCI,
						  result, resulttype,
						  &tbase, &tpow);
			case AR_Int_64_S:
				return ar_native2(ARPOWCJ,
						  result, resulttype,
						  &tbase, &tpow);
			default:
				return AR_STAT_INVALID_TYPE;
			}
		else if(AR_FLOAT_IS_COMPLEX(ptype) != AR_FLOAT_COMPLEX)
			return ar_native2(ARPOWCR,
					  result, resulttype,
					  &tbase, &tpow);
		else
			return ar_native2(ARPOWCC,
					  result, resulttype,
					  &tbase, &tpow);

	case IEEE_COMPLEX_64:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			switch (ptype) {
			case AR_Int_8_S:
			case AR_Int_16_S:
				ZERO_INT32_ALL(&tpow_32);
				(void) AR_convert((AR_DATA*) &tpow_32,
						  &int32type,
						  (AR_DATA*) &tpow,
						  &ptype);
				return ar_native2(ARPOWCDI,
						  result, resulttype,
						  &tbase, &tpow_32);
			case AR_Int_32_S:
				return ar_native2(ARPOWCDI,
						  result, resulttype,
						  &tbase, &tpow);
			case AR_Int_64_S:
				return ar_native2(ARPOWCDJ,
						  result, resulttype,
						  &tbase, &tpow);
			default:
				return AR_STAT_INVALID_TYPE;
			}
		else
			return ar_native2(ARPOWCDCD,
					  result, resulttype,
					  &tbase, &tpow);

	case IEEE_COMPLEX_128:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			switch (ptype) {
			case AR_Int_8_S:
			case AR_Int_16_S:
				ZERO_INT32_ALL(&tpow_32);
				(void) AR_convert((AR_DATA*) &tpow_32,
						  &int32type,
						  (AR_DATA*) &tpow,
						  &ptype);
				return ar_native2(ARPOWCXDI,
						  result, resulttype,
						  &tbase, &tpow_32);
			case AR_Int_32_S:
				return ar_native2(ARPOWCXDI,
						  result, resulttype,
						  &tbase, &tpow);
			case AR_Int_64_S:
				return ar_native2(ARPOWCXDJ,
						  result, resulttype,
						  &tbase, &tpow);
			default:
				return AR_STAT_INVALID_TYPE;
			}
		else
			return ar_native2(ARPOWCXDCXD,
					  result, resulttype,
					  &tbase, &tpow);

	default:
                switch (btype) {
                case AR_Int_8_S:
		case AR_Int_16_S:
			ZERO_INT32_ALL(&tbase_32);
			ZERO_INT32_ALL(&tpow_32);
			ZERO_INT32_ALL(&result_32);
			(void) AR_convert((AR_DATA*) &tbase_32,
					  &int32type,
					  (AR_DATA*) &tbase,
					  &btype);
			(void) AR_convert((AR_DATA*) &tpow_32,
					  &int32type,
					  (AR_DATA*) &tpow,
					  &ptype);
                        status = ar_native2(ARPOWII,
					    &result_32, &int32type,
					    &tbase_32, &tpow_32);
			if (IS_ERROR_STATUS(status))
				return status;
			return AR_convert((AR_DATA*) result, resulttype,
					  (AR_DATA*) &result_32, &int32type);
                case AR_Int_32_S:
                        ZERO_INT32_UPPER(result);
                        return ar_native2(ARPOWII,
                                          result, resulttype,
                                          &tbase, &tpow);
                case AR_Int_64_S:
                        return ar_native2(ARPOWJJ,
                                          result, resulttype,
                                          &tbase, &tpow);
                default:
                        return AR_STAT_INVALID_TYPE;
                }
	}
}


/* Native complex division */
int
ar_divide_complex (ar_data *result, const AR_TYPE *resulttype,
		   const ar_data *opnd1, const AR_TYPE *opnd1type,
		   const ar_data *opnd2, const AR_TYPE *opnd2type)
{
#if !defined _CRAYMPP
#define	ARCDIV   arcdiv_
#define	ARCDDIV  arcddiv_
#define	ARCXDDIV arcxddiv_
#endif
	extern void ARCDIV (ar_data *res, const ar_data *num,
					  const ar_data *den);
	extern void ARCDDIV(ar_data *res, const ar_data *num,
					  const ar_data *den);
	extern void ARCXDDIV(ar_data *res, const ar_data *num,
					  const ar_data *den);

	switch(UNROUNDED_TYPE(*resulttype)) {

	case IEEE_COMPLEX_32:
		return ar_native2(ARCDIV, result, resulttype, opnd1, opnd2);

	case IEEE_COMPLEX_64:
		return ar_native2(ARCDDIV, result, resulttype, opnd1, opnd2);

	case IEEE_COMPLEX_128:
		return ar_native2(ARCXDDIV, result, resulttype, opnd1, opnd2);
	}

	return AR_STAT_INVALID_TYPE;
}

static int calling_math_lib = 0;
static jmp_buf math_lib_jmpbuf;

/* Catch floating-point exceptions */
static volatile int fp_error = 0;
static void fptrap (int sig) {
	fp_error = 1;
	signal (SIGFPE, fptrap);
}

/* Catch math library detected errors */
int _lerror() {
	errno = ERANGE;
	return 0;
}

/* These macros are used to protect ourselves when calling math libraries. */

#define ARMOR_ON					\
	oldfpe = signal (SIGFPE, fptrap);		\
	fp_error = 0;					\
	errno = 0;					\
	calling_math_lib = 1;				\
	if (setjmp (math_lib_jmpbuf))			\
		errno = 1;				\
	else

#define ARMOR_OFF(status, result, resulttype)		\
	calling_math_lib = 0;				\
	signal (SIGFPE, oldfpe);			\
	if (fp_error | errno) {				\
		if (fp_error > 0)			\
			status = AR_STAT_OVERFLOW;	\
		else if (fp_error == 0)			\
			status = AR_STAT_UNDEFINED;	\
	} else						\
		status |= AR_status((AR_DATA*)result, resulttype);

static int
ar_native1(void (function)(), ar_data *result, const AR_TYPE *resulttype, const ar_data *opnd)
{
	int status = AR_STAT_OK;
	void (*oldfpe)();

	ARMOR_ON
		(function) (result, opnd);
	ARMOR_OFF (status, result, resulttype);

	return status;

}

static int
ar_native2(void (function)(), ar_data *result, const AR_TYPE *resulttype, const ar_data *opnd1, const ar_data *opnd2)
{
	int status = AR_STAT_OK;
	void (*oldfpe)();

	ARMOR_ON
		(function) (result, opnd1, opnd2);
	ARMOR_OFF (status, result, resulttype);

	return status;

}

#else	/* !(_Solaris || defined(_CRAYMPP) || defined(__mips)) */

/* Use native math library for intrinsic function evaluation */

#include <math.h>
#include <signal.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>

#undef double_t
#undef complex_t
#undef NAT_ROUND
#undef NAT_COMPLEX

#define double_t	double

#if defined(__sparc__) || defined(__mips)

#define NAT_ROUND	AR_Float_IEEE_NR_32
#define NAT_COMPLEX	AR_Complex_IEEE_NR_32

#else	/* IEEE 754 native arithmetic assumed */

#define NAT_ROUND	AR_Float_IEEE_NR_64
#define NAT_COMPLEX	AR_Complex_IEEE_NR_64

#if __svr4__
   #include <complex.h>
   #define complex_t	double complex
#endif

#endif

static AR_TYPE native_round_single = NAT_ROUND;
static AR_TYPE native_complex	   = NAT_COMPLEX;
static AR_TYPE native_double	   = AR_Float_IEEE_NR_64;
static AR_TYPE native_long_double  = AR_Float_IEEE_NR_64;
static AR_TYPE native_dbl_complex  = AR_Complex_IEEE_NR_64;


/* Fortran character index */
int
ar_index (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, const char *str2, const ar_data *backward)
{
	return AR_STAT_INVALID_TYPE;	/* Not available */
}


/* Fortran character scan */
int
ar_scan (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, const char *str2, const ar_data *backward)
{
	return AR_STAT_INVALID_TYPE;	/* Not available */
}


/* Fortran character verify */
int
ar_verify (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, const char *str2, const ar_data *backward)
{
	return AR_STAT_INVALID_TYPE;	/* Not available */
}


/* Fortran-90 reshape */
int
ar_reshape (void *result, const void *source, const void *shape,
	    const void *pad, const void *order)
{
	return AR_STAT_INVALID_TYPE;	/* Not available */
}


/* Fortran-90 transfer */
int
ar_transfer (void *result, const void *source, const void *mold, long *length)
{
	return AR_STAT_INVALID_TYPE;	/* Not available */
}


/* Fortran-90 modulo */
int
ar_modulo (ar_data *result, const AR_TYPE *resulttype,
	   const ar_data *opnd1, const AR_TYPE *opnd1type,
	   const ar_data *opnd2, const AR_TYPE *opnd2type)
{
	return AR_STAT_INVALID_TYPE;	/* Not available */
}

/* Fortran-90 selected_real_kind */
int
ar_selected_real_kind (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd1, const AR_TYPE *opnd1type,
	 const ar_data *opnd2, const AR_TYPE *opnd2type)
{
	return AR_STAT_INVALID_TYPE;	/* Not available */
}

/* Square root */
int
ar_sqrt (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype)
{
	int status;
	ar_data nat_opnd, nat_result;

	/* Check whether operation exists in native library */

        if (*resulttype == native_double) {
                *(double_t *) result = (sqrt) (*(double_t *) opnd);
                return AR_status((AR_DATA*)result, resulttype);
        }

#ifdef complex_t
        if (*resulttype == native_complex) {
                *(complex_t *) result = (csqrt) (*(complex_t *) opnd);
                return AR_status((AR_DATA*)result, resulttype);
        }
#endif

	/* The operation is not a native library operation.
	 * Convert, approximate with native arithmetic, and convert back.
	 */

	if (AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX) {
#ifdef complex_t
		status = AR_convert ((AR_DATA*)&nat_opnd, &native_complex,
				     (AR_DATA*)opnd, opndtype);
		status |= ar_sqrt (&nat_result, &native_complex,
				   &nat_opnd, &native_complex);
		status &= ~(AR_NEGATIVE | AR_ZERO);
		status |= AR_convert ((AR_DATA*)result, resulttype,
				      (AR_DATA*)&nat_result, &native_complex);
#else
		status = AR_STAT_INVALID_TYPE;
#endif
	} else {
		status = AR_convert ((AR_DATA*)&nat_opnd, &native_long_double,
				     (AR_DATA*)opnd, opndtype);
		status |= ar_sqrt (&nat_result, &native_long_double,
				   &nat_opnd, &native_long_double);
		status &= ~(AR_NEGATIVE | AR_ZERO);
		status |= AR_convert ((AR_DATA*)result, resulttype,
				      (AR_DATA*)&nat_result, &native_long_double);
	}

	return status;
}


/* Natural (base "e") logarithm */
int
ar_log (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype)
{
	int status;
	ar_data nat_opnd, nat_result;

	/* Check whether operation exists in native library */

        if (*resulttype == native_double) {
                *(double_t *) result = (log) (*(double_t *) opnd);
                return AR_status((AR_DATA*)result, resulttype);
        }
#ifdef complex_t
        if (*resulttype == native_complex) {
                *(complex_t *) result = (clog) (*(complex_t *) opnd);
                return AR_status((AR_DATA*)result, resulttype);
        }
#endif

	/* The operation is not a native library operation.
	 * Convert, approximate with native arithmetic, and convert back.
	 */

	if (AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX) {
#ifdef complex_t
		status = AR_convert ((AR_DATA*)&nat_opnd, &native_complex,
				     (AR_DATA*)opnd, opndtype);
		status |= ar_log (&nat_result, &native_complex,
				   &nat_opnd, &native_complex);
		status &= ~(AR_NEGATIVE | AR_ZERO);
		status |= AR_convert ((AR_DATA*)result, resulttype,
				      (AR_DATA*)&nat_result, &native_complex);
#else
		status = AR_STAT_INVALID_TYPE;
#endif
	} else {
		status = AR_convert ((AR_DATA*)&nat_opnd, &native_long_double,
				     (AR_DATA*)opnd, opndtype);
		status |= ar_log (&nat_result, &native_long_double,
				   &nat_opnd, &native_long_double);
		status &= ~(AR_NEGATIVE | AR_ZERO);
		status |= AR_convert ((AR_DATA*)result, resulttype,
				      (AR_DATA*)&nat_result, &native_long_double);
	}

	return status;
}


/* Exponential ("e" ** x) function */
int
ar_exp (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype)
{
	int status;
	ar_data nat_opnd, nat_result;

        if (*resulttype == native_double) {
                *(double_t *) result = (exp) (*(double_t *) opnd);
                return AR_status((AR_DATA*)result, resulttype);
        }
#ifdef complex_t
        if (*resulttype == native_complex) {
                *(complex_t *) result = (cexp) (*(complex_t *) opnd);
                return AR_status((AR_DATA*)result, resulttype);
        }
#endif

	/* The operation is not a native library operation.
	 * Convert, approximate with native arithmetic, and convert back.
	 */

	if (AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX) {
#ifdef complex_t
		status = AR_convert ((AR_DATA*)&nat_opnd, &native_complex,
				     (AR_DATA*)opnd, opndtype);
		status |= ar_exp (&nat_result, &native_complex,
				   &nat_opnd, &native_complex);
		status &= ~(AR_NEGATIVE | AR_ZERO);
		status |= AR_convert ((AR_DATA*)result, resulttype,
				      (AR_DATA*)&nat_result, &native_complex);
#else
		status = AR_STAT_INVALID_TYPE;
#endif
	} else {
		status = AR_convert ((AR_DATA*)&nat_opnd, &native_long_double,
				     (AR_DATA*)opnd, opndtype);
		status |= ar_exp (&nat_result, &native_long_double,
				   &nat_opnd, &native_long_double);
		status &= ~(AR_NEGATIVE | AR_ZERO);
		status |= AR_convert ((AR_DATA*)result, resulttype,
				      (AR_DATA*)&nat_result, &native_long_double);
	}

	return status;
}


/* Complex absolute value */
int
ar_cabs (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd, const AR_TYPE *opndtype)
{
	int status;
	ar_data re, im, resq, imsq, sum, nat_opnd, nat_result;
	AR_TYPE reimtype;

#if defined(complex_t)
        if (*resulttype == native_double &&
            *opndtype   == native_complex) {
                *(double_t *) result = (cabs) (*(complex_t *) opnd);
                return AR_status((AR_DATA*)result, resulttype);
        }

	/* The operation is not a native library operation.
	 * Convert, execute, and convert back.
	 */

	status = AR_convert ((AR_DATA*)&nat_opnd, &native_complex,
			     (AR_DATA*)opnd, opndtype);
	status |= ar_cabs (&nat_result, &native_round_single,
			   &nat_opnd, &native_complex);
	status &= ~(AR_NEGATIVE | AR_ZERO);
	status |= AR_convert ((AR_DATA*)result, resulttype,
			      (AR_DATA*)&nat_result, &native_round_single);
#else

        /* If no native complex operations, compute sqrt (real**2 * imag**2). */

        status = ar_decompose_complex (&re, &im, &reimtype,
                                       opnd, opndtype);
        status |= AR_multiply ((AR_DATA*)&resq, &reimtype,
                               (AR_DATA*)&re, &reimtype,
                               (AR_DATA*)&re, &reimtype);
        status |= AR_multiply ((AR_DATA*)&imsq, &reimtype,
                               (AR_DATA*)&im, &reimtype,
                               (AR_DATA*)&im, &reimtype);
        status |= AR_add ((AR_DATA*)&sum, &reimtype,
                          (AR_DATA*)&resq, &reimtype,
                          (AR_DATA*)&imsq, &reimtype);
        status &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
        status |= AR_sqrt ((AR_DATA*)result, resulttype, (AR_DATA*)&sum, &reimtype);
 
#endif

	return status;
}


/* Exponentiation */
int
ar_power(ar_data *result, const AR_TYPE *resulttype,
         const ar_data *base, const AR_TYPE *basetype,
         const ar_data *power, const AR_TYPE *powertype)
{
	int i;
	int status;
	int status2;
	ar_data tbase, tpow, tpow2, temp, one;
	AR_TYPE inttype = AR_Int_64_S;
	static int loopchk = 0;

#ifdef complex_t
        if (*basetype == native_complex || *powertype == native_complex) {
                if (*resulttype != native_complex)
                        return AR_STAT_INVALID_TYPE;
                if (*basetype != native_complex) {
                        status = AR_convert ((AR_DATA*)&tbase, &native_complex,
                                                (AR_DATA*)base, basetype);
                        if (IS_ERROR_STATUS(status))
                                return AR_STAT_INVALID_TYPE;
                } else
                        tbase = *base;
                if (*powertype != native_complex) {
                        status = AR_convert ((AR_DATA*)&tpow, &native_complex,
                                                (AR_DATA*)power, powertype);
                        if (IS_ERROR_STATUS(status))
                                return AR_STAT_INVALID_TYPE;
                } else
                        tpow = *power;
                *(complex_t *) result = (cpow) (*(complex_t *) &tbase,
                                                *(complex_t *) &tpow);
                return AR_status((AR_DATA*)result, resulttype);
        }
#endif

        if (*basetype == native_double ||
            *powertype == native_double) {
                if (*resulttype != native_double)
                        return AR_STAT_INVALID_TYPE;
                if (*basetype != native_double) {
                        status = AR_convert ((AR_DATA*)&tbase, &native_double,
                                                (AR_DATA*)base, basetype);
                        if (IS_ERROR_STATUS(status))
                                return AR_STAT_INVALID_TYPE;
                } else
                        tbase = *base;
                if (*powertype != native_double) {
                        status = AR_convert ((AR_DATA*)&tpow, &native_double,
                                                (AR_DATA*)power, powertype);
                        if (IS_ERROR_STATUS(status))
                                return AR_STAT_INVALID_TYPE;
                } else
                        tpow = *power;
                *(double_t *) result = (pow) (*(double_t *) &tbase,
                                              *(double_t *) &tpow);
                return AR_status((AR_DATA*)result, resulttype);
        }

	/* The operation is not a native library operation.
	 * Convert, approximate with native arithmetic, and convert back.
	 */

	/* Use native exponentiation routines if appropriate. */

	/*
	 *	Native routines will not be used directly.
	 */

	/* Get "1" of the proper type */
	if (AR_one ((AR_DATA*)&one, basetype) & AR_STAT_INVALID_TYPE)
		return AR_STAT_INVALID_TYPE;

	status = AR_status ((AR_DATA*)base, basetype);

	/* 0 ** 0 and 0 ** (neg) are invalid; 0 ** (pos) == 0. */
	if (status & AR_STAT_ZERO) {
		status = AR_status ((AR_DATA*)power, powertype);
		if (status & (AR_STAT_ZERO | AR_STAT_NEGATIVE))
			return AR_STAT_UNDEFINED;
		return AR_convert ((AR_DATA*)result, resulttype, (AR_DATA*)base, basetype);
	}

	/* 1 ** (anything) == 1 */
	if (AR_compare ((AR_DATA*)base, basetype, (AR_DATA*)&one, basetype) == AR_Compare_EQ)
		return AR_convert ((AR_DATA*)result, resulttype, (AR_DATA*)&one, basetype);

	/* Exponentiation by integer powers */
	if (AR_CLASS (*powertype) == AR_CLASS_INT) {

		if (*resulttype != *basetype)
			return AR_STAT_INVALID_TYPE;

		/* (integer) ** 1 == (integer) */
		if (AR_CLASS (*basetype) == AR_CLASS_INT &&
		    AR_compare ((AR_DATA*)power, powertype, (AR_DATA*)&one, powertype) ==
				AR_Compare_EQ)
			return AR_convert ((AR_DATA*)result, resulttype, (AR_DATA*)base, basetype);

		/* (-1) ** (even) == 1, (-1) ** (odd) == -1 */
		AR_negate ((AR_DATA*)&tbase, basetype, (AR_DATA*)&one, basetype);
		if (AR_compare ((AR_DATA*)base, basetype, (AR_DATA*)&tbase, basetype) ==
					AR_Compare_EQ) {
			if (power->ar_i64.part4 & 1)
				return AR_convert ((AR_DATA*)result, resulttype,
						   (AR_DATA*)base, basetype);
			return AR_convert ((AR_DATA*)result, resulttype,
					   (AR_DATA*)&one, basetype);
		}

		/* Compute negative integer powers by computing a power
		 * of a reciprocal.
		 */
		if (INT_SIGN(*powertype, power)) {

			/* (int other than 0, 1, -1) ** (negative) == 0 */
			if (AR_CLASS (*basetype) == AR_CLASS_INT)
				return AR_convert ((AR_DATA*)result, resulttype,
						   (AR_DATA*)&AR_const_zero, &inttype);

			status = AR_divide ((AR_DATA*)&tbase, basetype,
					    (AR_DATA*)&one, basetype,
					    (AR_DATA*)base, basetype);
			status &= AR_ERROR_STATUS;
			if (status) {
				AR_convert ((AR_DATA*)result, resulttype,
					    &AR_const_zero, &inttype);
				return status;
			}
			AR_negate ((AR_DATA*)&tpow, powertype, (AR_DATA*)power, powertype);

		} else {
			tbase = *base;
			tpow = *power;
		}

		/* Perform exponentiation by repeated multiplication */
		status = AR_convert ((AR_DATA*)result, resulttype, (AR_DATA*)&one, basetype);	
		status2 = AR_STAT_OK;

		switch (AR_INT_SIZE (*opnd1type)) {
		case AR_INT_SIZE_8:
			AR_convert ((AR_DATA*) &tpow2, &inttype,
				    (AR_DATA*) &tpow,  opnd1type);
			break;

		case AR_INT_SIZE_16:
			AR_convert ((AR_DATA*) &tpow2, &inttype,
				    (AR_DATA*) &tpow,  opnd1type);
			break;

		case AR_INT_SIZE_32:
			AR_convert ((AR_DATA*) &tpow2, &inttype,
				    (AR_DATA*) &tpow,  opnd1type);
			break;

		case AR_INT_SIZE_46:
		case AR_INT_SIZE_64:
			tpow2 = tpow;
			break;

		default:
			return (AR_STAT_INVALID_TYPE);
		}

		while (!IS_INT64_ZERO(&tpow2)) {
			if (IS_ERROR_STATUS(status2)) {
				AR_convert ((AR_DATA*)result, resulttype,
					    &AR_const_zero, &inttype);
				return status2 & AR_ERROR_STATUS;
			}
			if (tpow2.ar_i64.part4 & 1) {
				status = AR_multiply ((AR_DATA*)result,
						      resulttype,
						      (AR_DATA*)result,
						      resulttype,
						      (AR_DATA*)&tbase,
						      basetype);
				if (IS_ERROR_STATUS(status)) {
					AR_convert ((AR_DATA*)result,
						    resulttype,
						    &AR_const_zero,
						    &inttype);
					return status & AR_ERROR_STATUS;
				}
			}
			status2 = AR_multiply ((AR_DATA*)&tbase, basetype,
					       (AR_DATA*)&tbase, basetype,
					       (AR_DATA*)&tbase, basetype);
			SHRIGHT64 (tpow2.ar_i64);
		}

		return status;
	}

	/*
	 *	Exponentiation by non-native floating powers
	 */

	if(loopchk > 0)
		return AR_STAT_INVALID_TYPE;

	if (AR_CLASS (*basetype) == AR_CLASS_FLOAT &&
	    AR_FLOAT_IS_COMPLEX (*basetype) == AR_FLOAT_COMPLEX ||
	    AR_CLASS (*powertype) == AR_CLASS_FLOAT &&
	    AR_FLOAT_IS_COMPLEX (*powertype) == AR_FLOAT_COMPLEX) {

#ifndef complex_t
		return AR_STAT_INVALID_TYPE;
#else
		/* Compute (native complex) ** (native complex) */
		status  = AR_convert ((AR_DATA*)&tbase, &native_complex, (AR_DATA*)base, basetype);
		status |= AR_convert ((AR_DATA*)&tpow, &native_complex, (AR_DATA*)power, powertype);
		if (!IS_ERROR_STATUS(status)) {
			loopchk++;
			status = ar_power (&temp, &native_complex,
					   &tbase, &native_complex,
					   &tpow, &native_complex);
			loopchk--;
			if (!IS_ERROR_STATUS(status))
				status = AR_convert ((AR_DATA*)result, resulttype,
					  	   (AR_DATA*)&temp, &native_complex);
		}
		return status;
#endif
	}

	/* Compute as (native double prec) ** (native double prec) */
	status  = AR_convert ((AR_DATA*)&tbase, &native_long_double, (AR_DATA*)base, basetype);
	status |= AR_convert ((AR_DATA*)&tpow, &native_long_double, (AR_DATA*)power, powertype);
	if (!IS_ERROR_STATUS(status)) {
		loopchk++;
		status = ar_power (&temp, &native_long_double,
				   &tbase, &native_long_double,
				   &tpow, &native_long_double);
		loopchk--;
		if (!IS_ERROR_STATUS(status))
			status = AR_convert ((AR_DATA*)result, resulttype,
				    	 (AR_DATA*)&temp, &native_long_double);
	}
	return status;
}


/* Native complex division */
int
ar_divide_complex (ar_data *result, const AR_TYPE *resulttype,
		   const ar_data *opnd1, const AR_TYPE *opnd1type,
		   const ar_data *opnd2, const AR_TYPE *opnd2type)
{

	/* Assume types all match (see logic in AR_divide) */

	AR_DATA a, b, c, d, ac, bd, bc, ad, cc, dd, acbd, bcad, ccdd, re, im;
	AR_TYPE reimtype1, reimtype2, temptype;
	int status, restat, imstat;

	status = ar_decompose_complex ((ar_data*)&a, (ar_data*)&b, &reimtype1, opnd1, opnd1type);
	status |= ar_decompose_complex ((ar_data*)&c, (ar_data*)&d, &reimtype2, opnd2, opnd2type);

	/*	PDGCS requests that a different sequence be used when the
	 *	imaginary part of the denominator is zero. A meeting of
	 *	managers on 11/30/93 decided in favor of this expediency.
	 *	Note that we do NOT apply special-case processing when
	 *	the real part of the denominator is zero.
	 */

	imstat = AR_status (&d, &reimtype2);
	if (imstat & AR_STAT_ZERO) {

		/* zero imaginary part, use short sequence */
		restat = AR_divide (&re, &reimtype1,
				    &a, &reimtype1, &c, &reimtype2);
		imstat = AR_divide (&im, &reimtype1,
				    &b, &reimtype1, &c, &reimtype2);

	} else {

		/*
		 *	general sequence:
		 *
		 *      a + bi     (a + bi)(c - di)     (ac + bd)   (bc - ad)i
		 *      ------  =  ----------------  =  --------- + ----------
		 *      c + di     (c + di)(c - di)     c*c + d*d   c*c + d*d
		 */

		status |= AR_multiply (&ac, &reimtype1,
				       &a, &reimtype1, &c, &reimtype2);
		status |= AR_multiply (&bd, &reimtype1, &b,
				       &reimtype1, &d, &reimtype2);
		status |= AR_multiply (&bc, &reimtype1,
				       &b, &reimtype1, &c, &reimtype2);
		status |= AR_multiply (&ad, &reimtype1,
				       &a, &reimtype1, &d, &reimtype2);
		status |= AR_multiply (&cc, &reimtype2,
				       &c, &reimtype2, &c, &reimtype2);
		status |= AR_multiply (&dd, &reimtype2,
				       &d, &reimtype2, &d, &reimtype2);
		status |= AR_add (&acbd, &reimtype1,
				  &ac, &reimtype1, &bd, &reimtype1);
		status |= AR_subtract (&bcad, &reimtype1,
				       &bc, &reimtype1, &ad, &reimtype1);
		status |= AR_add (&ccdd, &reimtype1,
				  &cc, &reimtype1, &dd, &reimtype1);

		restat = AR_divide (&re, &reimtype1,
				    &acbd, &reimtype1, &ccdd, &reimtype1);
		imstat = AR_divide (&im, &reimtype1,
				    &bcad, &reimtype1, &ccdd, &reimtype1);
	}

	status |= ar_compose_complex (result, &temptype, (ar_data*)&re, (ar_data*)&im, &reimtype1);
	status |= restat | imstat;
	status &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
	status |= restat & imstat & AR_STAT_ZERO;
	return status;
}

#endif	/* _Solaris || defined(_CRAYMPP) || defined(__mips) */

/* string -> floating point */
int
ar_convert_str_to_float (ar_data *result, const AR_TYPE *resulttype,
			 const char *str)
{
	return ar_cvt_str_to_float (result, resulttype, str);
}

#endif	/* _CRAY */


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: native.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
