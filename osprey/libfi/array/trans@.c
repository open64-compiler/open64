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


#pragma ident "@(#) libfi/array/trans@.c	92.1	07/07/99 15:52:02"


/***********************************************************************
*  Name: TRANS@                                                        *
*                                                                      *
*  Description:                                                        *
*                                                                      *
*      MATRIX TRANSPOSE INTRINSIC FUNCTION                             *
*                                                                      *
*      Set B = TRANSPOSE(A),                                           *
*                                                                      *
*      where:                                                          *
*                                                                      *
*          B       = the matrix described by dope vector RESULT, and   *
*                                                                      *
*          A       = the matrix described by dope vector MATRIX_A.     *
*                                                                      *
*  Arguments:                                                          *
*                                                                      *
*  RESULT          Dope vector for RESULT matrix                       *
*                                                                      *
*  MATRIX_A        Dope vector for matrix operand                      *
*                                                                      *
*  Author:                                                             *
*  Math Software Group                                                 *
*  Cray Research, Inc.                                                 *
*                                                                      *
************************************************************************/

/*
 * header files
 */
#include <stddef.h>
#include <stdlib.h>
#include <cray/dopevec.h>	/* description of Fortran 90 Dope Vector */
#include <cray/portdefs.h>	/* portable definitions header file */
#include "intrin.h"		/* general header file for intrinsics    */

/*
 * Name of this entry point
 */

#define NAME _TRANS
#define BITS_PER_BYTE   (BITS_PER_WORD / BYTES_PER_WORD)

#ifdef _UNICOS
#pragma _CRI duplicate _TRANS as TRANS@
#endif
void
NAME(DopeVectorType * RESULT, DopeVectorType * MATRIX_A)
{
    long           *A;		/* Matrix A address */
    long           *B;		/* Matrix B address (RESULT) */
    char           *ca;		/* character pointer to A */
    char           *cb;		/* character pointer to B (RESULT) */
    long           n1a, n2a;	/* dimensions of MATRIX_A */
    long           inc1a, inc2a;	/* column and row increments,
					 * matrix A   */
    long           inc1b, inc2b;	/* column and row increments,
					 * matrix B */
    int             bucketsize;	/* number of words per array element (or
				 * number of bytes, if a byte array) */
    long           nbytes;	/* number of bytes total in RESULT array */
    long           nbits;	/* number of bits of RESULT array */
    int             bytealligned;	/* true if byte alligned data
					 * type, else false */
    long           i, j, k;	/* subscripts */

    /*
     * Parse the Dope Vector for MATRIX_A
     */

    n1a = MATRIX_A->dimension[0].extent;
    n2a = MATRIX_A->dimension[1].extent;
    inc1a = MATRIX_A->dimension[0].stride_mult;
    inc2a = MATRIX_A->dimension[1].stride_mult;

    /*
     * Set "bytealligned = true" iff and only if matrix is byte alligned.
     * 
     * Matrix is byte alligned if and only if the type code is "ASCII" or
     * "DERIVEDBYTE".
     * 
     * The bucketsize is the number of words (or bytes) per array element.
     */

    if (MATRIX_A->type_lens.type == DVTYPE_ASCII) {
	/*
	 * Byte alligned
	 */
	bytealligned = 1;
	ca = _fcdtocp(MATRIX_A->base_addr.charptr);
	bucketsize = _fcdlen(MATRIX_A->base_addr.charptr);  /* in bytes */
	nbits = bucketsize * BITS_PER_BYTE;
    } else if (MATRIX_A->type_lens.type == DVTYPE_DERIVEDBYTE ||
  	     MATRIX_A->type_lens.type == DVTYPE_DERIVEDWORD) {
		bytealligned =
		   (MATRIX_A->type_lens.type == DVTYPE_DERIVEDBYTE) ? 1 : 0;

		/* retrieve the number of bits in an element */
		nbits = MATRIX_A->base_addr.a.el_len;
		if (bytealligned) {
			ca = _fcdtocp(MATRIX_A->base_addr.charptr);
			bucketsize = nbits / BITS_PER_BYTE; 
						/* bucketsize in bytes =
						 * bits/bits_per_byte
						 */
		} else {
			A = (long *) MATRIX_A->base_addr.a.ptr;
						/* base address */
			bucketsize = nbits / BITS_PER_WORD;
						/* bucketsize in words =
						 * bits/bits_per_word
						 */
		}
    } else {
	/*
	 * Word alligned, not byte aligned.
	 */
	bytealligned = 0;
	A = (long *) MATRIX_A->base_addr.a.ptr;	/* base address */
	nbits = MATRIX_A->type_lens.int_len;
	bucketsize = nbits / BITS_PER_WORD;	/* bucketsize in words =
				 		 * bucketsize_in_bits
				 		 * divided by bits_per_word
				 		 */
    }

    /*
     * Allocate RESULT array, if necessary.
     */

    if (!RESULT->assoc) {
	/*
	 * Copy RESULT dope vector fields from MATRIX_A dope vector.
	 */
	RESULT->base_addr.a.ptr = (void *) NULL;	/* no address yet */
	RESULT->orig_base = 0;
	RESULT->orig_size = 0;
	/*
	 * Set dimension-specific information for RESULT
	 */
	RESULT->dimension[0].low_bound = 1;
	RESULT->dimension[0].extent = MATRIX_A->dimension[1].extent;
	RESULT->dimension[0].stride_mult = bucketsize;
	RESULT->dimension[1].low_bound = 1;
	RESULT->dimension[1].extent = MATRIX_A->dimension[0].extent;
	RESULT->dimension[1].stride_mult = bucketsize * n2a;
	/*
	 * Allocate the space for RESULT
	 */
	nbits = nbits *
	    RESULT->dimension[0].extent * RESULT->dimension[1].extent;
	nbytes = nbits / BITS_PER_BYTE;		/* byte length = bit length
				 		 * divided by bits_per_byte
				 		 */
	if (nbits != 0) {
		B = (void *) MALLOC(nbytes);
		if (B == NULL) {
	    		ERROR(FENOMEMY);
	    		return;
		}
	}
	RESULT->assoc = 1;
	if ( MATRIX_A->type_lens.type == DVTYPE_ASCII) {
	    RESULT->base_addr.charptr	= _cptofcd( (char *) B, bucketsize);
	} else
	    RESULT->base_addr.a.ptr = (void *) B;
	RESULT->orig_base = (void *) B;
	RESULT->orig_size = nbits;
    }
    /*
     * RESULT now exists (whether or not we just created it).
     * Assign values to pointer and increments.
     */
    if (bytealligned)
	cb = _fcdtocp(RESULT->base_addr.charptr);
    else
	B = (long *) RESULT->base_addr.a.ptr;
    inc1b = RESULT->dimension[0].stride_mult;
    inc2b = RESULT->dimension[1].stride_mult;
    /*
     * At this point, we have all the data set up, and we just have to do
     * the transpose.
     * 
     * There are two cases:
     * 
     * (1) word alligned data, and
     * 
     * (2) byte alligned data.
     */

    if (!bytealligned) {
	/*
	 * Word alligned data
	 */
	for (k = 1; k <= bucketsize; k++) {
	    for (j = 0; j < n2a; j++)
		for (i = 0; i < n1a; i++)
		    B[j * inc1b + i * inc2b] = A[i * inc1a + j * inc2a];

	    A++;		/* address next word */
	    B++;		/* address next word */
	}
    } else {
	/*
	 * Byte alligned data
	 */
	for (k = 1; k <= bucketsize; k++) {
	    for (j = 0; j < n2a; j++)
		for (i = 0; i < n1a; i++)
		    cb[j * inc1b + i * inc2b] = ca[i * inc1a + j * inc2a];

	    ca++;		/* address next byte */
	    cb++;		/* address next byte */
	}
    }
    return;
}
