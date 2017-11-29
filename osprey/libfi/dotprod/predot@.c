/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libfi/dotprod/predot@.c	92.1	07/08/99 15:49:55"


#include <cray/dopevec.h>	/* description of Fortran90 dope vector */
#include <cray/portdefs.h>	/* portable definitions header file */
#include "intrin.h"		/* general header file for intrinsics */

#define LEN_DBL		(BITS_PER_WORD * 2)	/* two-word length in bits */
#define LEN_QUAD	(BITS_PER_WORD * 4)	/* four-word length in bits */

/* name of routine */
#define ROUTINE	_PREDOT

#pragma _CRI duplicate _PREDOT as PREDOT@
void
ROUTINE(DopeVectorType * VECTOR_A, DopeVectorType * VECTOR_B,
        long *n, long *locxa, long *incxa, long *locya, long *incya)
{
    void *locx, *locy;
    int  incx, incy;

    locx = (void *) VECTOR_A->base_addr.a.ptr;	/* base address */
    *n = VECTOR_A->dimension[0].extent;	/* number of elements */
    incx = VECTOR_A->dimension[0].stride_mult;	/* increment */
	/*
	 * if double-word elements, divide increment by 2.
	 * if quad-word elements, divide increment by 4.
	 */
    if (VECTOR_A->type_lens.int_len == LEN_DBL)
	incx >>= 1;
    else if (VECTOR_A->type_lens.int_len == LEN_QUAD)
	incx >>= 2;

    locy = (void *) VECTOR_B->base_addr.a.ptr;	/* base address */
    incy = VECTOR_B->dimension[0].stride_mult;	/* increment */
	/*
	 * if double-word elements, divide increment by 2.
	 * if quad-word elements, divide increment by 4.
	 */
    if (VECTOR_B->type_lens.int_len == LEN_DBL)
	incy >>= 1;
    else if (VECTOR_B->type_lens.int_len == LEN_QUAD)
	incy >>= 2;

    /*
     * Store values in arguments
     */

    *locxa = (long) locx;
    *locya = (long) locy;
    *incxa = incx;
    *incya = incy;
    return;
}
