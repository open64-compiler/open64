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


/* Simulate library intrinsics if this module is loaded */

#include <stdio.h>

#include "arith.internal.h"
#include "int64.h"

#define MAX_DOPE_VECTOR_WORDS	32

#define CRAY_FLOAT_64		(UNROUNDED_TYPE(AR_Float_Cray1_64))
#define CRAY_FLOAT_128		(UNROUNDED_TYPE(AR_Float_Cray1_128))
#define CRAY_COMPLEX_64		(UNROUNDED_TYPE(AR_Complex_Cray1_64))
#define CRAY_COMPLEX_128	(UNROUNDED_TYPE(AR_Complex_Cray1_128))

#define IEEE_FLOAT_32		(UNROUNDED_TYPE(AR_Float_IEEE_NR_32))
#define IEEE_FLOAT_64		(UNROUNDED_TYPE(AR_Float_IEEE_NR_64))
#define IEEE_FLOAT_128		(UNROUNDED_TYPE(AR_Float_IEEE_NR_128))
#define IEEE_COMPLEX_32		(UNROUNDED_TYPE(AR_Complex_IEEE_NR_32))
#define IEEE_COMPLEX_64		(UNROUNDED_TYPE(AR_Complex_IEEE_NR_64))
#define IEEE_COMPLEX_128	(UNROUNDED_TYPE(AR_Complex_IEEE_NR_128))

/* Declarations of simulation support functions */

extern int ar_ext_address(AR_INT_64 *intaddr, const void *extaddr, int nwords);

extern int ar_pass_arg_address(const ar_data *arg, const AR_TYPE *argtype);

extern int ar_pass_ext_address(AR_INT_64 *intaddr, const void *extaddr, int nwords);

extern int ar_pass_fcd_address(const char *str, long lenstr);

extern int ar_pass_arg_value(const ar_data *arg, const AR_TYPE *argtype);

extern int ar_put_real_address(AR_INT_64 *intaddr);

extern int ar_get_function_value(ar_data *result, const AR_TYPE *resulttype);

extern int ar_sim(char* function_name);


/* Fortran character index */
int
ar_index (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, long len1, const char *str2, long len2, long backward)
{
	int status;
	AR_TYPE	type = AR_Logical;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_fcd_address(str1, len1);
	status |= ar_pass_fcd_address(str2, len2);
	if(backward)
		status |= ar_pass_arg_address((ar_data*)&AR_const_true, &type);
	else
		status |= ar_pass_arg_address((ar_data*)&AR_const_false, &type);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (*resulttype) {

	case AR_Int_32_S:
		status = ar_sim("indexi");
		break;

	case AR_Int_46_S:
	case AR_Int_64_S:
		status = ar_sim("index");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Fortran character scan */
int
ar_scan (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, long len1, const char *str2, long len2, long backward)
{
	int status;
	AR_TYPE	type = AR_Logical;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_fcd_address(str1, len1);
	status |= ar_pass_fcd_address(str2, len2);
	if(backward)
		status |= ar_pass_arg_address((ar_data*)&AR_const_true, &type);
	else
		status |= ar_pass_arg_address((ar_data*)&AR_const_false, &type);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (*resulttype) {

	case AR_Int_32_S:
		status = ar_sim("scani");
		break;

	case AR_Int_46_S:
	case AR_Int_64_S:
		status = ar_sim("scan");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Fortran character verify */
int
ar_verify (ar_data *result, const AR_TYPE *resulttype,
	 const char *str1, long len1, const char *str2, long len2, long backward)
{
	int status;
	AR_TYPE	type = AR_Logical;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_fcd_address(str1, len1);
	status |= ar_pass_fcd_address(str2, len2);
	if(backward)
		status |= ar_pass_arg_address((ar_data*)&AR_const_true, &type);
	else
		status |= ar_pass_arg_address((ar_data*)&AR_const_false, &type);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (*resulttype) {

	case AR_Int_32_S:
		status = ar_sim("verifyi");
		break;

	case AR_Int_46_S:
	case AR_Int_64_S:
		status = ar_sim("verify");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Fortran-90 reshape */
int
ar_reshape (void *result, const void *source, const void *shape,
	    const void *pad, const void *order)
{
	int	status;

	char*	addr;

	long long	resfcd1;
	long long	srcfcd1;
	long long	shpfcd1;

	AR_INT_64	extaddr;

	/* Grab the 1st word which is always the 1st word of an fcd */
	memcpy((char*)&resfcd1, (char*)result, 8);
	memcpy((char*)&srcfcd1, (char*)source, 8);
	memcpy((char*)&shpfcd1, (char*)shape, 8);

	if(srcfcd1 == 0 || shpfcd1 == 0)
		return AR_STAT_UNDEFINED;

	status = ar_clear_sim_state(AR_Int_64_S);

	/* If result baseaddr is not NULL, replace it with an ext addr desc */
	if(resfcd1 != 0) {
		memcpy((char*)&addr, (char*)&resfcd1 + sizeof(long long) -
			sizeof(char*), sizeof(char*));
		ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
		if(extaddr.part2)
			memcpy((char*)result, (char*)&extaddr, 8);
		else
			memcpy((char*)result+4, (char*)(&extaddr)+4, 4);
	}
	status |= ar_pass_ext_address(NULL, (const void*)result, MAX_DOPE_VECTOR_WORDS);

	/* Replace baseaddr in source dope vector with an ext addr descriptor */
	memcpy((char*)&addr, (char*)&srcfcd1 + sizeof(long long) -
		sizeof(char*), sizeof(char*));
	ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
	if(extaddr.part2)
		memcpy((char*)source, (char*)&extaddr, 8);
	else
		memcpy((char*)source+4, (char*)(&extaddr)+4, 4);
	status |= ar_pass_ext_address(NULL, source, MAX_DOPE_VECTOR_WORDS);

	/* Replace baseaddr in shape dope vector with an ext addr descriptor */
	memcpy((char*)&addr, (char*)&shpfcd1 + sizeof(long long) -
		sizeof(char*), sizeof(char*));
	ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
	if(extaddr.part2)
		memcpy((char*)shape, (char*)&extaddr, 8);
	else
		memcpy((char*)shape+4, (char*)(&extaddr)+4, 4);
	status |= ar_pass_ext_address(NULL, shape, MAX_DOPE_VECTOR_WORDS);

	/* Replace baseaddr in pad dope vector with an ext addr descriptor */
	if(pad != NULL) {
		memcpy((char*)&addr, (char*)pad + sizeof(long long) -
			sizeof(char*), sizeof(char*));
		ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
		if(extaddr.part2)
			memcpy((char*)pad, (char*)&extaddr, 8);
		else
			memcpy((char*)pad+4, (char*)(&extaddr)+4, 4);
	}
	status |= ar_pass_ext_address(NULL, pad, MAX_DOPE_VECTOR_WORDS);

	/* Replace baseaddr in order dope vector with an ext addr descriptor */
	if(order != NULL) {
		memcpy((char*)&addr, (char*)order + sizeof(long long) -
			sizeof(char*), sizeof(char*));
		ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
		if(extaddr.part2)
			memcpy((char*)order, (char*)&extaddr, 8);
		else
			memcpy((char*)order+4, (char*)(&extaddr)+4, 4);
	}
	status |= ar_pass_ext_address(NULL, order, MAX_DOPE_VECTOR_WORDS);

	if(IS_ERROR_STATUS(status))
		return status;

	status = ar_sim("reshape");

	/* Restore baseaddr fields in dope vectors */
	memcpy((char*)source, (char*)&srcfcd1, 8);
	memcpy((char*)shape, (char*)&shpfcd1, 8);

	if(IS_ERROR_STATUS(status))
		return status;

	return ar_put_real_address((AR_INT_64*)result);
}


/* Fortran-90 transfer */
int
ar_transfer (void *result, const void *source, const void *mold, long *size)
{
	int	status;

	char*	addr;

	long long	resfcd1;
	long long	srcfcd1;
	long long	mldfcd1;

	AR_INT_64	extaddr;
	AR_INT_64	length;

	AR_TYPE inttype = AR_Int_64_S;

	/* Grab the 1st word which is always the 1st word of an fcd */
	memcpy((char*)&resfcd1, (char*)result, 8);
	memcpy((char*)&srcfcd1, (char*)source, 8);
	memcpy((char*)&mldfcd1, (char*)mold, 8);

	if(srcfcd1 == 0 || mldfcd1 == 0)
		return AR_STAT_UNDEFINED;

	status = ar_clear_sim_state(AR_Int_64_S);

	/* If result baseaddr is not NULL, replace it with an ext addr desc */
	if(resfcd1 != 0) {
		memcpy((char*)&addr, (char*)&resfcd1 + sizeof(long long) -
			sizeof(char*), sizeof(char*));
		ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
		if(extaddr.part2)
			memcpy((char*)result, (char*)&extaddr, 8);
		else
			memcpy((char*)result+4, (char*)(&extaddr)+4, 4);
	}
	status |= ar_pass_ext_address(NULL, (const void*)result, MAX_DOPE_VECTOR_WORDS);

	/* Replace baseaddr in source dope vector with an ext addr descriptor */
	memcpy((char*)&addr, (char*)&srcfcd1 + sizeof(long long) -
		sizeof(char*), sizeof(char*));
	ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
	if(extaddr.part2)
		memcpy((char*)source, (char*)&extaddr, 8);
	else
		memcpy((char*)source+4, (char*)(&extaddr)+4, 4);
	status |= ar_pass_ext_address(NULL, source, MAX_DOPE_VECTOR_WORDS);

	/* Replace baseaddr in mold dope vector with an ext addr descriptor */
	memcpy((char*)&addr, (char*)&mldfcd1 + sizeof(long long) -
		sizeof(char*), sizeof(char*));
	ar_ext_address(&extaddr, (const void*)addr, 0x7ffffff);
	if(extaddr.part2)
		memcpy((char*)mold, (char*)&extaddr, 8);
	else
		memcpy((char*)mold+4, (char*)(&extaddr)+4, 4);
	status |= ar_pass_ext_address(NULL, mold, MAX_DOPE_VECTOR_WORDS);

	if(size != NULL) {
		length.part1 = length.part2 = 0;
		length.part3 = *size>>16;
		length.part4 = *size & 0xffff;
		status |= ar_pass_arg_address((ar_data*)&length, &inttype);
	}
	else
		status |= ar_pass_arg_address((ar_data*)size, &inttype);

	if(IS_ERROR_STATUS(status))
		return status;

	status = ar_sim("transfer");

	/* Restore baseaddr fields in dope vectors */
	memcpy((char*)source, (char*)&srcfcd1, 8);
	memcpy((char*)mold, (char*)&mldfcd1, 8);

	if(IS_ERROR_STATUS(status))
		return status;

	return ar_put_real_address((AR_INT_64*)result);
}


/* Fortran-90 modulo */
int
ar_modulo (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd1, const AR_TYPE *opnd1type,
	 const ar_data *opnd2, const AR_TYPE *opnd2type) {

	int status;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_arg_address(opnd1, opnd1type);
	status |= ar_pass_arg_address(opnd2, opnd2type);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		status = ar_sim("modulof");
		break;

	case CRAY_FLOAT_64:
	case IEEE_FLOAT_64:
		status = ar_sim("modulos");
		break;

	case CRAY_FLOAT_128:
	case IEEE_FLOAT_128:
		status = ar_sim("modulod");
		break;

	default:
		switch (*resulttype) {

		case AR_Int_32_S:
		case AR_Int_46_S:
			status = ar_sim("moduloi");
			break;

		case AR_Int_64_S:
			status = ar_sim("moduloj");
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}

	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Fortran-90 selected_real_kind */
int
ar_selected_real_kind (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd1, const AR_TYPE *opnd1type,
	 const ar_data *opnd2, const AR_TYPE *opnd2type) {

	int status;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_arg_address(opnd1, opnd1type);
	status |= ar_pass_arg_address(opnd2, opnd2type);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (*resulttype) {

	case AR_Int_32_S:
		status = ar_sim("selreali");
		break;
	  
	case AR_Int_46_S:
	case AR_Int_64_S:
		status = ar_sim("selrealk");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Square root */
int
ar_sqrt (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd, const AR_TYPE *opndtype) {

	int status;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_arg_value(opnd, opndtype);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		status = ar_sim("hsqrt");
		break;

	case CRAY_FLOAT_64:
	case IEEE_FLOAT_64:
		status = ar_sim("sqrt");
		break;

	case CRAY_FLOAT_128:
	case IEEE_FLOAT_128:
		status = ar_sim("dsqrt");
		break;

	case CRAY_COMPLEX_64:
	case IEEE_COMPLEX_32:
	case IEEE_COMPLEX_64:
		status = ar_sim("csqrt");
		break;

	case CRAY_COMPLEX_128:
	case IEEE_COMPLEX_128:
		status = ar_sim("cdsqrt");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Natural (base "e") logarithm */
int
ar_log (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype) {

	int status;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_arg_value(opnd, opndtype);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		status = ar_sim("hlog");
		break;

        case CRAY_FLOAT_64:
	case IEEE_FLOAT_64:
		status = ar_sim("alog");
		break;

	case CRAY_FLOAT_128:
	case IEEE_FLOAT_128:
		status = ar_sim("dlog");
		break;

	case CRAY_COMPLEX_64:
	case IEEE_COMPLEX_32:
	case IEEE_COMPLEX_64:
		status = ar_sim("clog");
		break;

	case CRAY_COMPLEX_128:
	case IEEE_COMPLEX_128:
		status = ar_sim("cdlog");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Exponential ("e" ** x) function */
int
ar_exp (ar_data *result, const AR_TYPE *resulttype,
	const ar_data *opnd, const AR_TYPE *opndtype) {

	int status;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_arg_value(opnd, opndtype);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (UNROUNDED_TYPE(*resulttype)) {

	case IEEE_FLOAT_32:
		status = ar_sim("hexp");
		break;

	case CRAY_FLOAT_64:
	case IEEE_FLOAT_64:
		status = ar_sim("exp");
		break;

	case CRAY_FLOAT_128:
	case IEEE_FLOAT_128:
		status = ar_sim("dexp");
		break;

	case CRAY_COMPLEX_64:
	case IEEE_COMPLEX_32:
	case IEEE_COMPLEX_64:
		status = ar_sim("cexp");
		break;

	case CRAY_COMPLEX_128:
	case IEEE_COMPLEX_128:
		status = ar_sim("cdexp");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Complex absolute value */
int
ar_cabs (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd, const AR_TYPE *opndtype) {

	int status;

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_arg_value(opnd, opndtype);
	if(IS_ERROR_STATUS(status))
		return status;

	switch (UNROUNDED_TYPE(*resulttype)) {

	case CRAY_FLOAT_64:
	case IEEE_FLOAT_32:
	case IEEE_FLOAT_64:
		status = ar_sim("cabs");
		break;

	case CRAY_FLOAT_128:
	case IEEE_FLOAT_128:
		status = ar_sim("cdabs");
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


/* Exponentiation */
int
ar_power(ar_data *result, const AR_TYPE *resulttype,
         const ar_data *base, const AR_TYPE *basetype,
         const ar_data *power, const AR_TYPE *powertype)
{
	int status;
	ar_data temp;
	AR_TYPE btype, ptype;

	/* Prepare for power function simulation by converting
	 * base and power operand types to values supported by
	 * the simulated power functions.
	 */

	if(AR_CLASS(*basetype) == AR_CLASS_INT)

		btype = ptype = *powertype;

	else if(AR_CLASS(*powertype) == AR_CLASS_INT ||
	        (AR_FLOAT_SIZE(*powertype) <= AR_FLOAT_64 &&
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

		if(AR_FLOAT_SIZE(*basetype) > AR_FLOAT_SIZE(*powertype))
		    btype = (AR_TYPE) (*basetype | AR_FLOAT_IS_COMPLEX(*powertype));
		else
		    btype = (AR_TYPE) (*powertype | AR_FLOAT_IS_COMPLEX(*basetype));

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

	status = ar_clear_sim_state(*resulttype);
	if(*basetype != btype) {
		status |= AR_convert((AR_DATA*)&temp, &btype, (AR_DATA*)base, basetype);
		if(ptype == btype &&
		   AR_FLOAT_SIZE(btype) == AR_FLOAT_128 &&
		   AR_FLOAT_IS_COMPLEX(btype) == AR_FLOAT_COMPLEX)
			status |= ar_pass_arg_address(&temp, &btype);
		else
			status |= ar_pass_arg_value(&temp, &btype);
	}
	else
		if(ptype == btype &&
		   AR_FLOAT_SIZE(btype) == AR_FLOAT_128 &&
		   AR_FLOAT_IS_COMPLEX(btype) == AR_FLOAT_COMPLEX)
			status |= ar_pass_arg_address(base, basetype);
		else
			status |= ar_pass_arg_value(base, basetype);

	if(*powertype != ptype) {
		status  = AR_convert((AR_DATA*)&temp, &ptype, (AR_DATA*)power, powertype);
		if(ptype == btype &&
		   AR_FLOAT_SIZE(btype) == AR_FLOAT_128 &&
		   AR_FLOAT_IS_COMPLEX(btype) == AR_FLOAT_COMPLEX)
			status |= ar_pass_arg_address(&temp, &ptype);
		else
			status |= ar_pass_arg_value(&temp, &ptype);
	}
	else
		if(ptype == btype &&
		   AR_FLOAT_SIZE(btype) == AR_FLOAT_128 &&
		   AR_FLOAT_IS_COMPLEX(btype) == AR_FLOAT_COMPLEX)
			status |= ar_pass_arg_address(power, powertype);
		else
			status |= ar_pass_arg_value(power, powertype);

	if(IS_ERROR_STATUS(status))
		return status;

	/*
	 * Call (simulate) the correct power function determined by
	 * the (expanded) base and power types.
	 */

	switch (UNROUNDED_TYPE(btype)) {

	case CRAY_FLOAT_64:
	case IEEE_FLOAT_32:
	case IEEE_FLOAT_64:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			status = ar_sim("rtoi");
		else
			status = ar_sim("rtor");
		break;

	case CRAY_FLOAT_128:
	case IEEE_FLOAT_128:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			status = ar_sim("dtoi");
		else if(AR_FLOAT_SIZE(ptype) <= AR_FLOAT_64)
			status = ar_sim("dtor");
		else
			status = ar_sim("dtod");
		break;

	case CRAY_COMPLEX_64:
	case IEEE_COMPLEX_32:
	case IEEE_COMPLEX_64:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			status = ar_sim("ctoi");
		else if(AR_FLOAT_IS_COMPLEX(ptype) != AR_FLOAT_COMPLEX)
			status = ar_sim("ctor");
		else
			status = ar_sim("ctoc");
		break;

	case CRAY_COMPLEX_128:
	case IEEE_COMPLEX_128:
		if(AR_CLASS(ptype) == AR_CLASS_INT)
			status = ar_sim("cdtoi");
		else
			status = ar_sim("cdtocd");
		break;

	default:
		switch (btype) {

		case AR_Int_32_S:
		case AR_Int_46_S:
		case AR_Int_64_S:
			status = ar_sim("itoi");
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
	}

	status &= AR_ERROR_STATUS;
	if(status)
		return status;

	return ar_get_function_value(result, resulttype);
}


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

/* string -> floating point */
int
ar_convert_str_to_float (ar_data *result, const AR_TYPE *resulttype,
			 const char *str)
{
	int status;

	int			i;

	long		w, d, p;

	AR_INT_64	fw;
	AR_INT_64	lcap1;
	AR_INT_64	mode;
	AR_INT_64	stat;
	AR_INT_64	xd;
	AR_INT_64	xp;

	long		ichars[64];
	AR_INT_64	unpacked_chars[64];

	extern int	ar_unpack_float_str();

	/* Unpack char string into integer array 1 character per word */

	status = ar_unpack_float_str(ichars, 64, &w, &d, &p, str);
	if (IS_ERROR_STATUS(status))
		return status;

	if(status == AR_STAT_ZERO) {
		ZERO64(result[0].ar_i64);
		if(AR_FLOAT_SIZE(*resulttype) == AR_FLOAT_128)
		  result[0].ar_i128.part5 = result[0].ar_i128.part6 =
		  result[0].ar_i128.part7 = result[0].ar_i128.part8 = 0;
		return AR_STAT_ZERO;
	}

	/* Set up 64-bit arguments for the simulated call */

	ZERO64(fw);
	fw.part4 = w;
	ZERO64(xd);
	xd.part4 = d;
	xp.part4 = p;
	if(p < 0)
		xp.part1 = xp.part2 = xp.part3 = 0xffff;
	else
		xp.part1 = xp.part2 = xp.part3 = 0;

	/* Define the floating point mode of the numeric result */

	ZERO64(mode);
	if(UNROUNDED_TYPE(*resulttype) == IEEE_FLOAT_32)
		mode.part4 = MODEHP;			/* = 32-bit */
	else if(AR_FLOAT_SIZE(*resulttype) == AR_FLOAT_64)
		mode.part4 = MODESP;			/* = 64-bit */
	else if(AR_FLOAT_SIZE(*resulttype) == AR_FLOAT_128)
		mode.part4 = MODEDP;			/* = 128-bit */
	else
		return AR_STAT_INVALID_TYPE;

	for(i=0; i<w; i++) {
		ZERO64(unpacked_chars[i]);
		unpacked_chars[i].part4 = ichars[i];
	}

	/* Define (call-by-address) argument list to:
	 *
	 *    defgu2sd(fca, &fw, &lcap1, &mode, &result, &status, &d, &p)
	 *
	 */

	status  = ar_clear_sim_state(*resulttype);
	status |= ar_pass_ext_address(&lcap1,(const void*)unpacked_chars, 64);
	status |= ar_pass_ext_address(NULL, (const void*)&fw, 1);
	status |= ar_pass_ext_address(NULL, (const void*)&lcap1, 1);
	status |= ar_pass_ext_address(NULL, (const void*)&mode, 1);
	status |= ar_pass_ext_address(NULL, (const void*)result, 4);
	status |= ar_pass_ext_address(NULL, (const void*)&stat, 1);
	status |= ar_pass_ext_address(NULL, (const void*)&xd, 1);
	status |= ar_pass_ext_address(NULL, (const void*)&xp, 1);

	/* Store the unpacked last character address + 1 (into lcap1)
	 * Note that lcap1.part4 was returned with shift count to convert
	 * a 64-bit word offset/index into a address quantity.
	 */

	lcap1.part4 = w<<lcap1.part4;

	if (IS_ERROR_STATUS(status))
		return status;

	/* Simulate low-level input conversion routine, defgu2sd */

	status = ar_sim("defgu2sd");
	if(IS_ERROR_STATUS(status))
		return status;

	/* 
	 * Process results returned by defgu2sd.  Note that defgu2sd has
	 * already stored the converted number into result.
	 */

	memcpy(&status, (char*)(&stat)+8-sizeof(int), sizeof(int));
	switch (status) {
	case EX_REAL32:
		result[0].ar_i64.part3 = result[0].ar_i64.part1;
		result[0].ar_i64.part4 = result[0].ar_i64.part2;
		result[0].ar_i64.part1 = result[0].ar_i64.part2 = 0;
	case EX_REAL64:
	case EX_REAL128:
		status = AR_status((AR_DATA*)result, resulttype);
		break;

	case EX_EXPUFLO:
		ZERO64(result[0].ar_i64);
		if(AR_FLOAT_SIZE(*resulttype) == AR_FLOAT_128)
		  result[0].ar_i128.part5 = result[0].ar_i128.part6 =
		  result[0].ar_i128.part7 = result[0].ar_i128.part8 = 0;
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
}


/* "Normal" complex division algorithm */
int
ar_divide_complex (ar_data *result, const AR_TYPE *resulttype,
		   const ar_data *opnd1, const AR_TYPE *opnd1type,
		   const ar_data *opnd2, const AR_TYPE *opnd2type)
{

	/* Assume types all match (see logic in AR_divide) */

	AR_DATA a, b, c, d, ac, bd, bc, ad, cc, dd, acbd, bcad, ccdd, re, im;
	AR_TYPE reimtype1, reimtype2, temptype;
	int status, restat, imstat;

	status  = ar_decompose_complex ((ar_data*)&a, (ar_data*)&b, &reimtype1,
									opnd1, opnd1type);
	status |= ar_decompose_complex ((ar_data*)&c, (ar_data*)&d, &reimtype2,
									opnd2, opnd2type);

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

	status |= ar_compose_complex (result, &temptype,
								  (ar_data*)&re, (ar_data*)&im, &reimtype1);
	status |= restat | imstat;
	status &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
	status |= restat & imstat & AR_STAT_ZERO;
	return status;
}


#if defined(__sparc__) || defined(__mips)
char *strnstrn(char *str1, long n1, char *str2, long n2)
{
	int	i = 0;
	int	imax = n1-n2;

	while(i <= imax) {
	  if(str1[i] == str2[0] &&
	     strncmp(&str1[i], str2, n2) == 0)
		return &str1[i];
	  i++;
	}

	return NULL;
}
#endif

static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: simulate.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
