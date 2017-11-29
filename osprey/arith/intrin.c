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


/* Primary arith entry points for intrinsic/library function evaluation */

#include <math.h>
#include <signal.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>

#include "arith.internal.h"
#include "int64.h"


/* Fortran character index function */
int
AR_index(AR_DATA *result, const AR_TYPE *resulttype,
	 const char* str1, const AR_DATA *str1len, const AR_TYPE *str1lentype,
	 const char* str2, const AR_DATA *str2len, const AR_TYPE *str2lentype,
	 const AR_DATA *backward, const AR_TYPE *backwardtype)
{
	int	status;
	long	len1 = str1len->ar_internal_data_item1;
	long	len2 = str2len->ar_internal_data_item1;
	long	back;

	if (AR_CLASS(*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*resulttype) != AR_Int_8_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_16_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_32_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_64_S ||
	    *resulttype != *str1lentype ||
	    *resulttype != *str2lentype ||
	    (backward != NULL && *backwardtype != AR_Logical))
		status = AR_STAT_INVALID_TYPE;
	else if(len1 < 0 || len2 < 0)
		status = AR_STAT_UNDEFINED;
	else {
		if(backward == NULL || (AR_status(backward, backwardtype)&AR_STAT_ZERO))
			back = 0;
		else
			back = 1;
		status = ar_index((ar_data*)result, resulttype,
						  str1, len1, str2, len2, back);
	}

	if (IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Fortran character scan function */
int
AR_scan(AR_DATA *result, const AR_TYPE *resulttype,
	 const char* str1, const AR_DATA *str1len, const AR_TYPE *str1lentype,
	 const char* str2, const AR_DATA *str2len, const AR_TYPE *str2lentype,
	 const AR_DATA *backward, const AR_TYPE *backwardtype)
{
	int	status;
	long	len1 = str1len->ar_internal_data_item1;
	long	len2 = str2len->ar_internal_data_item1;
	long	back;

	if (AR_CLASS(*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*resulttype) != AR_Int_8_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_16_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_32_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_64_S ||
	    *resulttype != *str1lentype ||
	    *resulttype != *str2lentype ||
	    (backward != NULL && *backwardtype != AR_Logical))
		status = AR_STAT_INVALID_TYPE;
	else if(len1 < 0 || len2 < 0)
		status = AR_STAT_UNDEFINED;
	else {
		if(backward == NULL || (AR_status(backward, backwardtype)&AR_STAT_ZERO))
			back = 0;
		else
			back = 1;
		status = ar_scan((ar_data*)result, resulttype,
						 str1, len1, str2, len2, back);
	}

	if (IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Fortran character verify function */
int
AR_verify(AR_DATA *result, const AR_TYPE *resulttype,
	 const char* str1, const AR_DATA *str1len, const AR_TYPE *str1lentype,
	 const char* str2, const AR_DATA *str2len, const AR_TYPE *str2lentype,
	 const AR_DATA *backward, const AR_TYPE *backwardtype)
{
	int	status;
	long	len1 = str1len->ar_internal_data_item1;
	long	len2 = str2len->ar_internal_data_item1;
	long	back;

	if (AR_CLASS(*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*resulttype) != AR_Int_8_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_16_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_32_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_64_S ||
	    *resulttype != *str1lentype ||
	    *resulttype != *str2lentype ||
	    (backward != NULL && *backwardtype != AR_Logical))
		status = AR_STAT_INVALID_TYPE;
	else if(len1 < 0 || len2 < 0)
		status = AR_STAT_UNDEFINED;
	else {
		if(backward == NULL || (AR_status(backward, backwardtype)&AR_STAT_ZERO))
			back = 0;
		else
			back = 1;
		status = ar_verify((ar_data*)result, resulttype,
						   str1, len1, str2, len2, back);
	}

	if (IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Fortran-90 reshape function */
int
AR_reshape(void *result, const void *source, const void *shape,
	   const void *pad, const void *order)
{
	if (result == NULL || source == NULL || shape == NULL)
		return AR_STAT_UNDEFINED;

	return ar_reshape(result, source, shape, pad, order);
}


/* Fortran-90 transfer function */
int
AR_transfer(void *result, const void *source, const void *mold,
	    const AR_DATA *size, const AR_TYPE *sizetype)
{
	long length;

	if (result == NULL || source == NULL || mold == NULL)
		return AR_STAT_UNDEFINED;

	if (size != NULL) {
		if(AR_INT_SIZE(*sizetype) != AR_Int_8_S &&
		   AR_INT_SIZE(*sizetype) != AR_Int_16_S &&
		   AR_INT_SIZE(*sizetype) != AR_Int_32_S &&
		   AR_INT_SIZE(*sizetype) != AR_Int_64_S)
			return AR_STAT_INVALID_TYPE;
		length = size->ar_internal_data_item1;
		if(length <= 0)
			return AR_STAT_UNDEFINED;
		return ar_transfer(result, source, mold, &length);
	}

	return ar_transfer(result, source, mold, (long*)NULL);

}


/* Fortran-90 Modulo */
int
AR_Modulo (AR_DATA *result, const AR_TYPE *resulttype,
	   const AR_DATA *opnd1, const AR_TYPE *opnd1type,
	   const AR_DATA *opnd2, const AR_TYPE *opnd2type)
{
	int status;

	if (*resulttype != *opnd1type || *resulttype != *opnd2type ||
	   (AR_status(opnd1, opnd1type) & AR_STAT_INVALID_TYPE) ||
	   (AR_status(opnd2, opnd2type) & AR_STAT_INVALID_TYPE))
		status = AR_STAT_INVALID_TYPE;
	else
		status = ar_modulo((ar_data*)result, resulttype,
					 (const ar_data*)opnd1, opnd1type,
					 (const ar_data*)opnd2, opnd2type);

	if (IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}

/* Selected_real_kind */
int
AR_selected_real_kind (AR_DATA *result, const AR_TYPE *resulttype,
	 const AR_DATA *opnd1, const AR_TYPE *opnd1type,
	 const AR_DATA *opnd2, const AR_TYPE *opnd2type)
{
	int status;

	if (*resulttype != *opnd1type || *resulttype != *opnd2type ||
	    AR_CLASS(*resulttype) != AR_CLASS_INT ||
	    AR_INT_SIZE(*resulttype) != AR_Int_8_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_16_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_32_S &&
	    AR_INT_SIZE(*resulttype) != AR_Int_64_S)
		status = AR_STAT_INVALID_TYPE;
	else
		status = ar_selected_real_kind((ar_data*)result, resulttype,
									   (const ar_data*)opnd1, opnd1type,
									   (const ar_data*)opnd2, opnd2type);

	if (IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}

/* Square root */
int
AR_sqrt (AR_DATA *result, const AR_TYPE *resulttype,
	 const AR_DATA *opnd, const AR_TYPE *opndtype)
{
	int status;

	if (*resulttype != *opndtype ||
	   (AR_CLASS (*resulttype) != AR_CLASS_FLOAT) ||
	   (AR_status(opnd, opndtype) & AR_STAT_INVALID_TYPE))
		status = AR_STAT_INVALID_TYPE;
	else
		status = ar_sqrt((ar_data*)result, resulttype,
				   (const ar_data*)opnd, opndtype);

	if(IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Natural (base "e") logarithm */
int
AR_log (AR_DATA *result, const AR_TYPE *resulttype,
	const AR_DATA *opnd, const AR_TYPE *opndtype)
{
	int status;

	if (*resulttype != *opndtype ||
	   (AR_CLASS(*resulttype) != AR_CLASS_FLOAT) ||
	   (AR_status(opnd, opndtype) & AR_STAT_INVALID_TYPE))
		status = AR_STAT_INVALID_TYPE;
	else
		status = ar_log((ar_data*)result, resulttype,
				  (const ar_data*)opnd, opndtype);

	if(IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Exponential ("e" ** x) function */
int
AR_exp (AR_DATA *result, const AR_TYPE *resulttype,
	const AR_DATA *opnd, const AR_TYPE *opndtype)
{
	int status;

	if (*resulttype != *opndtype ||
	   (AR_CLASS (*resulttype) != AR_CLASS_FLOAT) ||
	   (AR_status(opnd, opndtype) & AR_STAT_INVALID_TYPE))
		status = AR_STAT_INVALID_TYPE;
	else
		status = ar_exp((ar_data*)result, resulttype,
				  (const ar_data*)opnd, opndtype);

	if(IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Complex absolute value */
int
AR_cabs (AR_DATA *result, const AR_TYPE *resulttype,
	 const AR_DATA *opnd, const AR_TYPE *opndtype)
{
	int status;

	if (AR_CLASS (*opndtype) != AR_CLASS_FLOAT ||
	    AR_FLOAT_IS_COMPLEX (*opndtype) != AR_FLOAT_COMPLEX ||
	    AR_FLOAT_IS_COMPLEX (*resulttype) == AR_FLOAT_COMPLEX)
		status = AR_STAT_INVALID_TYPE;
	else if(!((status = AR_status (opnd, opndtype)) & AR_STAT_OVERFLOW))
		status = ar_cabs((ar_data*)result, resulttype,
				   (const ar_data*)opnd, opndtype);

	if(IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Exponentiation */
int
AR_power(AR_DATA *result, const AR_TYPE *resulttype,
         const AR_DATA *base, const AR_TYPE *basetype,
         const AR_DATA *power, const AR_TYPE *powertype)
{
	int status;

	if (AR_CLASS(*basetype) == AR_CLASS_INT &&
		AR_CLASS(*powertype) == AR_CLASS_INT &&
		*basetype != *powertype)
		status = AR_STAT_INVALID_TYPE;
	else
		status = ar_power((ar_data*)result, resulttype,
					(const ar_data*)base,  basetype,
					(const ar_data*)power, powertype);

	if(IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}

/* string -> floating point */
int
AR_convert_str_to_float (AR_DATA *result, const AR_TYPE *resulttype,
			 const char *str)
{
	int status;

	if(AR_CLASS(*resulttype) != AR_CLASS_FLOAT ||
	   AR_FLOAT_IS_COMPLEX(*resulttype) == AR_FLOAT_COMPLEX)
		status = AR_STAT_INVALID_TYPE;
	else
		status = ar_convert_str_to_float ((ar_data*)result, resulttype, str);

	if(IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: intrin.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
