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


/* No math library intrinsic folding available if this module is loaded.
 * However, this module may contain other necessary folding routines
 * such as string conversion routines.  In these cases, the high-level
 * interface name (AR_name) must exist here.
 */

#include "arith.internal.h"

int ar_rounding_modes = 0xf;		/* All rounding modes allowed */
int ar_underflow_modes = 1<<AR_UNDERFLOW_TO_DENORM;

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
		status = ar_cvt_str_to_float ((ar_data*)result, resulttype, str);

	if(IS_ERROR_STATUS(status))
		ar_set_invalid_result((ar_data*)result, resulttype);

	return status;
}


/* Complex absolute value */
int
AR_cabs (AR_DATA *result, const AR_TYPE *resulttype,
	 const AR_DATA *opnd, const AR_TYPE *opndtype)
{
	ar_set_invalid_result((ar_data*)result, resulttype);
	return AR_STAT_UNDEFINED;
}
int
ar_cabs (ar_data *result, const AR_TYPE *resulttype,
	 const ar_data *opnd, const AR_TYPE *opndtype)
{
	ar_set_invalid_result((ar_data*)result, resulttype);
	return AR_STAT_UNDEFINED;
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


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: no_intrin.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
