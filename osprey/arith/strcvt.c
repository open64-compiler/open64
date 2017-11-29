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


/* Conversions to and from strings. */

#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#include "arith.internal.h"

/* string -> integer */
int
AR_convert_str_to_int (AR_DATA *res, const AR_TYPE *resulttype,
		       int *bits_used, const char *str, const int *base) {

	ar_data* result = (ar_data*)res;

	ar_data baseval, intval, temp;
        AR_TYPE u64type = AR_Int_64_U;
	int status = AR_STAT_OK;

	if (AR_CLASS(*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*resulttype) == AR_INT_SIZE_128 ||
	    AR_SIGNEDNESS(*resulttype) != AR_UNSIGNED)
		return AR_STAT_INVALID_TYPE;

	ZERO_INT64(result);
	ZERO_INT16_UPPER(&baseval);

	switch (*base) {
	case 2:
	case 8:
	case 10:
	case 16:
		baseval.ar_i64.part4 = *base;
		break;
	default:
		return AR_STAT_UNDEFINED;
	}

	ZERO_INT64(&intval);

	/* Peeled iteration */
	if (*str) {
		if (*str >= '0' && *str <= '9')
			if (*base > (*str-'0'))
				result->ar_i64.part4 = *str - '0';
			else
				return AR_STAT_UNDEFINED;
		else if (*base != 16)
			return AR_STAT_UNDEFINED;
		else if (*str >= 'A' && *str <= 'F')
			result->ar_i64.part4 = *str - 'A' + 10;
		else if (*str >= 'a' && *str <= 'f')
			result->ar_i64.part4 = *str - 'a' + 10;
		else
			return AR_STAT_UNDEFINED;
		str++;
	}

	for (; *str; str++) {
		if (*str >= '0' && *str <= '9')
			if (*base > (*str-'0'))
				intval.ar_i64.part4 = *str - '0';
			else
				return AR_STAT_UNDEFINED;
		else if (*base != 16)
			return AR_STAT_UNDEFINED;
		else if (*str >= 'A' && *str <= 'F')
			intval.ar_i64.part4 = *str - 'A' + 10;
		else if (*str >= 'a' && *str <= 'f')
			intval.ar_i64.part4 = *str - 'a' + 10;
		else
			return AR_STAT_UNDEFINED;
		status |= ar_multiply_integer (&temp, &u64type,
					       result, &u64type,
					       &baseval, &u64type);
		status |= ar_add_integer (result, &u64type,
					  &temp, &u64type,
					  &intval, &u64type);
	}

	if (status & (AR_STAT_UNDEFINED | AR_STAT_INVALID_TYPE))
		ar_internal_error (2002, __FILE__, __LINE__);

        /* Check for overflow of result size */

	switch(AR_INT_SIZE(*resulttype)) {
	case AR_INT_SIZE_8:
		if(!IS_INT8_UPPER_ZERO(result))
			status |= AR_STAT_OVERFLOW;
		break;

	case AR_INT_SIZE_16:
		if(!IS_INT16_UPPER_ZERO(result))
			status |= AR_STAT_OVERFLOW;
		break;

	case AR_INT_SIZE_24:
		if(!IS_INT24_UPPER_ZERO(result))
			status |= AR_STAT_OVERFLOW;
		break;

	case AR_INT_SIZE_32:
		if(!IS_INT32_UPPER_ZERO(result))
			status |= AR_STAT_OVERFLOW;
		break;

	case AR_INT_SIZE_46:
		if(!IS_INT46_UPPER_ZERO(result))
			status |= AR_STAT_OVERFLOW;
		break;
	}

        /* Drop all flags but overflow */
	status &= AR_STAT_OVERFLOW;

	if(status)
		ar_set_invalid_result(result, resulttype);
	else
	        ar_clear_unused_bits(result, resulttype);

	if (bits_used) {
		AR_leadz ((AR_DATA*)&temp, &u64type, (AR_DATA*)result,
			  &u64type);
		*bits_used = 64 - temp.ar_i64.part4;
	}

	switch(AR_INT_SIZE(*resulttype)) {
	case AR_INT_SIZE_8:
		if (IS_INT8_ZERO(result))
			status |= AR_STAT_ZERO;
		break;

	case AR_INT_SIZE_16:
		if (IS_INT16_ZERO(result))
			status |= AR_STAT_ZERO;
		break;

	case AR_INT_SIZE_24:
		if (IS_INT24_ZERO(result))
			status |= AR_STAT_ZERO;
		break;

	case AR_INT_SIZE_32:
		if (IS_INT32_ZERO(result))
			status |= AR_STAT_ZERO;
		break;

	case AR_INT_SIZE_46:
		if (IS_INT46_ZERO(result))
			status |= AR_STAT_ZERO;
		break;

	case AR_INT_SIZE_64:
		if (IS_INT64_ZERO(result))
			status |= AR_STAT_ZERO;
		break;
	}

	return status;
}


/* integer -> string */
int
AR_convert_int_to_str (char *resultstr, const int *base,
		       const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* opnd = (ar_data*)opd;

	ar_data baseval, intval, divresult, modresult;
	AR_TYPE intvaltype;
	int i, isnegative, status;
	char str [66];

	if (AR_CLASS (*opndtype) != AR_CLASS_INT)
		return AR_STAT_INVALID_TYPE;

	/* normalize to a 64-bit value */
	if (*base == 10 && AR_SIGNEDNESS (*opndtype) == AR_SIGNED)
		intvaltype = AR_Int_64_S;
	else
		intvaltype = AR_Int_64_U;
	ar_convert_to_integral (&intval, &intvaltype, opnd, opndtype);

	ZERO_INT16_UPPER(&baseval);

	switch (*base) {
	case 2:
	case 8:
	case 10:
	case 16:
		baseval.ar_i64.part4 = *base;
		break;
	default:
		return AR_STAT_UNDEFINED;
	}

	if (*base == 10 &&
	    AR_SIGNEDNESS (intvaltype) == AR_SIGNED &&
	    INT64_SIGN(&intval)) {
		isnegative = 1;
		intvaltype = AR_Int_64_U;
		ar_negate_integer (&intval, &intvaltype, &intval, &intvaltype);
	} else
		isnegative = 0;

	i = sizeof (str);
	str [--i] = '\0';

	do {
		status = ar_divide_integer (&divresult, &intvaltype,
					    &modresult, &intvaltype,
					    &intval, &intvaltype,
					    &baseval, &intvaltype);
		if (status & (AR_STAT_UNDEFINED | AR_STAT_INVALID_TYPE))
			ar_internal_error (2003, __FILE__, __LINE__);
		str [--i] = modresult.ar_i64.part4 + '0';
		intval = divresult;
	} while (!IS_INT64_ZERO(&intval));

	if (isnegative)
		str [--i] = '-';

	strcpy (resultstr, str + i);

	return AR_STAT_OK;
}

/* string -> floating point (native only) */
int
ar_cvt_str_to_float (ar_data *result, const AR_TYPE *resulttype,
					 const char *str)
{
	int status = AR_STAT_OK;
	char *endptr;

#if _CRAY

	/* Note:  ar_cvt_str_to_float is called on CRAY systems only when all
	 * other intrinsic function evaluation has been disabled.
	 */

	errno = 0;

	switch(AR_FLOAT_SIZE(*resulttype)) {

	case AR_FLOAT_64:
		*((double*)result) = strtod (str, &endptr);
		break;

	case AR_FLOAT_128:
		*((long double*)result) = strtold (str, &endptr);
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}	

#elif _Solaris

/* Input conversion precision mode flags */

#define MODESP		000     /* Single-precision (64-bit) flag */
#define MODEDP		004     /* Double-precision (128-bit) flag */
#define MODEHP		020     /* Half-precision (32-bit) flag */

/* Input conversion exit values */

#define EX_REAL64       3       /* 64-bit real                  */
#define EX_REAL128      4       /* 128-bit real                 */
#define EX_REAL32       5       /* 32-bit real                  */
#define EX_ILLCHAR      -1      /* invalid character            */
#define EX_EXPUFLO      -3      /* floating-point underflow     */
#define EX_EXPOFLO      -4      /* floating-point overflow      */
#define EX_NULLFLD      -5      /* null field (no digits)       */

	long		fw, d, p;

	long*		lcap1;
	long		mode;
	long		stat;

	long		ichars[64];

	/* Unpack char string into top of simulated stack space */

	status = ar_unpack_float_str(ichars, 64, &fw, &d, &p, str);
	if (IS_ERROR_STATUS(status))
		return status;

	switch(AR_FLOAT_SIZE(*resulttype)) {
	case AR_FLOAT_32:
		ZEROIEEE32 (result->ar_ieee32);
		mode = MODEHP;
		break;
	case AR_FLOAT_64:
		ZEROIEEE64 (result->ar_ieee64);
		mode = MODESP;
		break;
	case AR_FLOAT_128:
		ZEROIEEE128 (result->ar_ieee128);
		mode = MODEDP;
		break;
	}

	if(status == AR_STAT_ZERO)
		return AR_STAT_ZERO;

	lcap1 = &ichars[fw];
	_defgu2sd(ichars, &fw, &lcap1, &mode, result, &stat, &d, &p);

	/* 
	 * Process results returned by defgu2sd.  Note that defgu2sd has
	 * already stored the converted number into result.
	 */

	switch (stat) {
	case EX_REAL32:
		result[0].ar_i64.part3 = result[0].ar_i64.part1;
		result[0].ar_i64.part4 = result[0].ar_i64.part2;
		result[0].ar_i64.part1 = result[0].ar_i64.part2 = 0;
	case EX_REAL64:
	case EX_REAL128:
		status = AR_status((AR_DATA*)result, resulttype);
		break;

	case EX_EXPUFLO:
		switch(AR_FLOAT_SIZE(*resulttype)) {
		case AR_FLOAT_32:
			ZEROIEEE32 (result->ar_ieee32);
			break;
		case AR_FLOAT_64:
			ZEROIEEE64 (result->ar_ieee64);
			break;
		case AR_FLOAT_128:
			ZEROIEEE128 (result->ar_ieee128);
			break;
		}
		status = AR_STAT_UNDERFLOW|AR_STAT_ZERO;
		break;
	case EX_EXPOFLO:
		status = AR_STAT_OVERFLOW;
		break;
	default:
		status = AR_STAT_UNDEFINED;
		break;
	}

	return status;

#elif defined(__mips)

	ar_data dval;
	double d;
	long double ld;
	AR_TYPE float_64 = ((AR_TYPE) UNROUNDED_TYPE(AR_Float_IEEE_NR_64));

	/* Define strtod, which isn't defined in stdlib.h on MIPS.  */
	extern double strtod(const char *, char **);
	extern long double strtold(const char *, char **);

	errno = 0;

	switch(AR_FLOAT_SIZE(*resulttype)) {

	case AR_FLOAT_32:
		d = strtod (str, &endptr);
		status = AR_convert((AR_DATA*)result, resulttype,
				    (AR_DATA*)&d, &float_64);
		break;

	case AR_FLOAT_64:
		d = strtod (str, &endptr);
		memcpy(result,&d,sizeof(double));
		break;

	case AR_FLOAT_128:
		ld = strtold (str, &endptr);
		memcpy(result,&ld,sizeof(long double));
		break;
	}

	if (IS_ERROR_STATUS(status))
		return status;

#else

	ar_data dval;
	AR_TYPE float_64 = UNROUNDED_TYPE(AR_Float_IEEE_NR_64);

	/* Define strtod, which isn't defined in stdlib.h on Suns.  */
	extern double strtod(const char *, char **);

	errno = 0;

	switch(AR_FLOAT_SIZE(*resulttype)) {

	case AR_FLOAT_32:
		*(double*)(&dval) = strtod (str, &endptr);
		status = AR_convert((AR_DATA*)result, resulttype,
				    (AR_DATA*)&dval, &float_64);
		break;

	case AR_FLOAT_64:
		*((double*)result) = strtod (str, &endptr);
		break;

	case AR_FLOAT_128:
		*(double*)(&dval) = strtod (str, &endptr);
		status = AR_convert((AR_DATA*)result, resulttype,
				    (AR_DATA*)&dval, &float_64);
		break;
	}

	if (IS_ERROR_STATUS(status))
		return status;
#endif

	if (*endptr)
		/* Conversion stopped before the end of the string */
		return AR_STAT_UNDEFINED;

	if (errno == ERANGE)
		return AR_STAT_OVERFLOW;

	return AR_status((AR_DATA*)result, resulttype) |
	       (status & AR_STAT_UNDERFLOW);
}

int
ar_unpack_float_str(long* ibuf, long maxbuflen, long* w, long* d, long *p,
		    const char *str)
{
	int  i;			/* index into unpacked buffer of chars */
	int  n;			/* numeric value of decimal digit */
	int  t;			/* power of 10 for converting to digits */
	int  x;			/* exponent value */
	int  z;			/* mantissa flag: 1=zero, 0=nonzero, -1=bad */

	*w = 0;			/* index into string, width of string */
	*d = 0;			/* number of digits after decimal pt */
	*p = 0;			/* scale factor */
	 i = 0;

	/* find first non-space character */
	while (*str == ' ' ||
	       *str == '\f' ||
	       *str == '\n' ||
	       *str == '\r' ||
	       *str == '\t' ||
	       *str == '\v')
		str++;
	if (*str == '+' || *str == '-')
		ibuf[i++] = *str++;

	/* scan over leading zero digits */
	if(*str == '0') {
		z = 1;			/* Flag leading zero digits */
		while(*(++str) == '0')
			;
	}
	else
		z = -1;			/* Flag no leading zero digits */

	/* move digits before decimal point to ibuf */
	if (*str >= '0' && *str <= '9'){
		z = 0;			/* Flag nonzero digit before dec pt */
		ibuf[i++] = *str++;
		while (*str >= '0' && *str <= '9'){
			if (i < (maxbuflen-6))
				ibuf[i++] = *str++;
			else {
				/*
				 * Ignore trailing digits of long mantissa,
				 * but adjust the exponent to compensate
				 */
				(*p)--;
				str++;
			}
		}
	}

	/*
	 * if present, move decimal point, digits after decimal point to ibuf
	 */
	if (*str == '.') {
		/* found decimal point */
		if (i == (maxbuflen-6)) i--;	
		ibuf[i++] = *str++;
		if(z == 1) {		/* If all 0 digits before decimal pt */
			while (*str == '0') {
				(*p)++;	/* Use scale factor to ignore 0's */
				str++;
			}
		}
		while (*str >= '0' && *str <= '9') {
			if (i < (maxbuflen-6))	
				ibuf[i++] = *str;
			str++;
			(*d)++;
			z = 0;		/* Flag nonzero mantissa */
		}
	}

	if (z == -1)			/* if no digits in mantissa */
		return AR_STAT_UNDEFINED;

	if (z == 1)	{		/* if only zero digits in mantissa */
		ibuf[i++] = '0';
		*p = 0;
		*w = i;
		if(ibuf[0] == '-')
			return AR_STAT_NEGATIVE | AR_STAT_ZERO;
		else
			return AR_STAT_ZERO;
	}

	/* if present or needs to be adjusted, move exponent to ibuf */
	if (*str == 'e' || *str == 'E') {
		ibuf[i++] = 'E';
		str++;
		if(*p) {		/* If 0's before&after decimal point
					 * or exp, inc for long mantissa */
			x = atoi(str)-*p;
			if(x < 0) {
				ibuf[i++] = '-';
				x = -x;
			}
			if      (x < 10)   t=1;
			else if (x < 100)  t=10;
			else if (x < 1000) t=100;
			else               t=1000;
			while(t > 1) {
				/*
				 * Store scale-adjusted exponent chars
				 */
				n = x/t;
				x -= (n*t);
				t = (t+1)/10;
				ibuf[i++] = '0'+n;
			}
			ibuf[i++] = '0'+x;
			if (*str == '+' || *str == '-')
				str++;
			while (*str >= '0' && *str <= '9')
				str++;
			*p = 0;
		}
		else {
			if (*str == '+' || *str == '-')
				ibuf[i++] = *str++;
			while (*str=='0')
				str++;
			while (*str >= '0' && *str <= '9') {
				if (i == maxbuflen) break;
				ibuf[i++] = *str++;
			}
			if ((ibuf[i-1] < '0' || ibuf[i-1] > '9')
			    && *(str-1)=='0')
				ibuf[i++] = '0';
		}
	}

	if(*str != '\0')		/* if string not null-terminated */
		return AR_STAT_UNDEFINED;

	*w = i;

	return AR_STAT_OK;
}


/* floating point -> string */
int
AR_convert_float_to_str (char *resultstr,
			 const AR_DATA *opd, const AR_TYPE *opndtype) {

	ar_data* opnd = (ar_data*)opd;

	ar_data temp, temp2;
	AR_TYPE temptype;
	int status = AR_STAT_OK;

#if defined _CRAY && !defined _CRAYMPP

	switch (*opndtype) {
	case AR_Float_Cray1_64:
	case AR_Float_Cray1_64_F:
		sprintf (resultstr, "%.14e", opnd->ar_f64);
		break;
	case AR_Float_Cray1_128:
		sprintf (resultstr, "%.27Le", opnd->ar_f128);
		break;
	case AR_Float_IEEE_NR_32:
	case AR_Float_IEEE_ZE_32:
	case AR_Float_IEEE_UP_32:
	case AR_Float_IEEE_DN_32:
		status = AR_status(opd, opndtype);
		if (status & AR_STAT_OVERFLOW)
			sprintf(resultstr, "%sInf",
				(status & AR_STAT_NEGATIVE) ? "-" : "+");
		else if (status & AR_STAT_ZERO)
			sprintf(resultstr, "%s%.7e",
				(status & AR_STAT_NEGATIVE) ? "-" : "", 0.0);
		else if (status & AR_STAT_UNDEFINED)
			strcpy(resultstr, "NaN");
		else if (HOST_IS_IEEE_FLOAT) {
			status = ar_i32to64 (&temp2.ar_ieee64,
					     &opnd->ar_ieee32);
			sprintf (resultstr, "%.7e",
				 (*(double *) &temp2.ar_ieee64));
		}
		else {
			status = ar_i32to64 (&temp2.ar_ieee64,
					     &opnd->ar_ieee32);
			status |= ar_i64toc128 (&temp.ar_f128,
						&temp2.ar_ieee64);
			sprintf (resultstr, "%.16Le", temp.ar_f128);
		}
		break;
	case AR_Float_IEEE_NR_64:
	case AR_Float_IEEE_ZE_64:
	case AR_Float_IEEE_UP_64:
	case AR_Float_IEEE_DN_64:
		status = AR_status(opd, opndtype);
		if (status & AR_STAT_OVERFLOW)
			sprintf(resultstr, "%sInf",
				(status & AR_STAT_NEGATIVE) ? "-" : "+");
		else if (status & AR_STAT_ZERO)
			sprintf(resultstr, "%s%.16e",
				(status & AR_STAT_NEGATIVE) ? "-" : "", 0.0);
		else if (status & AR_STAT_UNDEFINED)
			strcpy(resultstr, "NaN");
		else if (HOST_IS_IEEE_FLOAT) {
			sprintf (resultstr, "%.16e",
				 (*(double *) &opnd->ar_ieee64));
		}
		else
		{
			status = ar_i64toc128 (&temp.ar_f128,
					       &opnd->ar_ieee64);
			sprintf (resultstr, "%.16Le", temp.ar_f128);
		}
		break;
	case AR_Float_IEEE_NR_128:
	case AR_Float_IEEE_ZE_128:
	case AR_Float_IEEE_UP_128:
	case AR_Float_IEEE_DN_128:
		status = AR_status(opd, opndtype);
		if (status & AR_STAT_OVERFLOW)
			sprintf(resultstr, "%sInf",
				(status & AR_STAT_NEGATIVE) ? "-" : "+");
		else if (status & AR_STAT_ZERO)
			sprintf(resultstr, "%s%.34e",
				(status & AR_STAT_NEGATIVE) ? "-" : "", 0.0);
		else if (status & AR_STAT_UNDEFINED)
			strcpy(resultstr, "NaN");
		else if (HOST_IS_IEEE_FLOAT) {
			sprintf (resultstr, "%.34Le",
				 (*(long double *) &opnd->ar_ieee128));
		}
		else
		{
			status = ar_itoc128 (&temp.ar_f128, &opnd->ar_ieee128,
					     ar_state_register.ar_rounding_mode);
			sprintf (resultstr, "%.34Le", temp.ar_f128);
		}
		break;
	default:
		return AR_STAT_INVALID_TYPE;
	}

	return status;

#else

	switch (*opndtype) {
	case AR_Float_Cray1_64:
	case AR_Float_Cray1_64_F:
		status |= ar_ctoi64 (&temp.ar_ieee64, &opnd->ar_f64);
		sprintf (resultstr, "%.15e", *((double *) &temp.ar_ieee64));
		break;
	case AR_Float_Cray1_128:
		status |= ar_ctoi64 (&temp.ar_ieee64, &opnd->ar_f64);
		sprintf (resultstr, "%.29e", *((double *) &temp.ar_ieee64));
		break;
	case AR_Float_IEEE_NR_32:
	case AR_Float_IEEE_ZE_32:
	case AR_Float_IEEE_UP_32:
	case AR_Float_IEEE_DN_32:
		status |= ar_i32to64 (&temp.ar_ieee64, &opnd->ar_ieee32);
		sprintf(resultstr, "%.7e", *((double *) &temp.ar_ieee64));
		break;
	case AR_Float_IEEE_NR_64:
	case AR_Float_IEEE_ZE_64:
	case AR_Float_IEEE_UP_64:
	case AR_Float_IEEE_DN_64:
		sprintf(resultstr, "%.16le", *((double*) &opnd->ar_ieee64));
		break;
	case AR_Float_IEEE_NR_128:
	case AR_Float_IEEE_ZE_128:
	case AR_Float_IEEE_UP_128:
	case AR_Float_IEEE_DN_128:
		/* Use 64-bit output routine for now */
		status |= ar_i128to64 (&temp.ar_ieee64, &opnd->ar_ieee128,
				       AR_ROUND_NEAREST);
		sprintf(resultstr, "%.34le", *((double*) &temp.ar_ieee64));
		break;
	default:
		return AR_STAT_INVALID_TYPE;
	}

	return status;

#endif

}


/* string with hexadecimal floating point -> floating point */
int
AR_convert_hex_str_to_float (AR_DATA *result, const AR_TYPE *resulttype,
                             const char *str) {


        int status = AR_STAT_OK;
        AR_DATA temp;
        const int base = 16;
        int bitsused;
        const AR_TYPE result64 = AR_Int_64_U;
        char save_char;
        char temp_string[17];

        errno = 0;

        switch (*resulttype) {
        case AR_Float_Cray1_64:
        case AR_Float_Cray1_64_F:
        case AR_Float_IEEE_NR_32:
        case AR_Float_IEEE_ZE_32:
        case AR_Float_IEEE_UP_32:
        case AR_Float_IEEE_DN_32:
        case AR_Float_IEEE_NR_64:
        case AR_Float_IEEE_ZE_64:
        case AR_Float_IEEE_UP_64:
        case AR_Float_IEEE_DN_64:
                status = AR_convert_str_to_int(result, &result64, &bitsused,
                                               str, &base);
                break;

        case AR_Float_IEEE_NR_128:
        case AR_Float_IEEE_ZE_128:
        case AR_Float_IEEE_UP_128:
        case AR_Float_IEEE_DN_128:
        case AR_Float_Cray1_128:
                strncpy(temp_string, str, 16);
                temp_string[16] = '\0';
                status = AR_convert_str_to_int(result, &result64, &bitsused,
                                               temp_string, &base);
                status |= AR_convert_str_to_int(&temp, &result64, &bitsused,
                                                str+16, &base);

                result->ar_internal_data_item2 = temp.ar_internal_data_item1;

                break;

        default:
                return AR_STAT_INVALID_TYPE;
        }

        /* return only the AR_STAT_UNDEFINED flag */
        return (status & AR_STAT_UNDEFINED);
}


int
AR_convert_host_sint64_to_int(AR_DATA *result, const AR_TYPE *resulttype,
                              AR_HOST_SINT64 i64val)
{

   AR_TYPE s64type = AR_Int_64_S;

   if (AR_CLASS(*resulttype) != AR_CLASS_INT)
	   return AR_STAT_INVALID_TYPE;

   result->ar_internal_data_item1 = i64val;

   return (AR_convert(result, resulttype, result, &s64type));
}  /* AR_convert_host_sint64_to_int */


int
AR_convert_int_to_host_sint64(AR_HOST_SINT64 *i64val,
                              const AR_DATA *opnd, const AR_TYPE *opndtype)
{
   int status;
   AR_TYPE s64type = AR_Int_64_S;
   AR_DATA s64val;

   if (AR_CLASS(*opndtype) != AR_CLASS_INT)
	   return AR_STAT_INVALID_TYPE;

   status = AR_convert(&s64val, &s64type, opnd, opndtype);

   *i64val =  s64val.ar_internal_data_item1;

   return (status);
}  /* AR_convert_int_to_host_int64 */


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: strcvt.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
