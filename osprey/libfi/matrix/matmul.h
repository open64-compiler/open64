/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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


/* USMID @(#) libfi/matrix/matmul.h	92.1	02/03/99 09:33:12 */

#include <stdlib.h>
#include <stddef.h>
#include <cray/dopevec.h>	/* description of Fortran 90 Dope Vector */
#include <cray/portdefs.h>	/* description of portable definitions */
#include "intrin.h"		/* general header file for intrinsics    */

/*                                                                      *
 * The helper algorithm of the Matrix Multiplication Intrinsic Function,*
 * MATMUL.                                                              *
 *                                                                      *
 *  This header file is included in each of the specific matrix         *
 *  multiplication routines.                                            *
 *                                                                      *
 *  Author:  Math Software Group,  Cray Research, Inc.                  *
 *                                                                      *
 *  Name:   _premult (static function)                                  *
 *                                                                      *
 *  Description: Preprocessing for matrix multiplication.               *
 *                                                                      *
 *  Parse dopevectors for RESULT, MATRIX_A, MATRIX_B, storing the       *
 *  relevant parameters in variables A, inc1a, inc2a, n1a, n2a, etc.    *
 *                                                                      *
 *  Allocate space for the result array, if necessary, and fill         *
 *  in it's dope vector.                                                *
 *                                                                      *
 *  Return a nonzero-sized array if either A or B is a zero-sized array *
 *  but the result should not be a zero-sized array, i.e., the specific *
 *  extents of the proper dimensions of the array result are known and  *
 *  are not zero.                                                       *
 *                                                                      *
 ***********************************************************************/

typedef struct MatrixDimen {
	void    *A;		/* Matrix A address */
	void    *B;		/* Matrix B address */
	void    *C;		/* Result address (result = matrix C) */

	/*
	 * The number of dimensions of each matrix is either 1 (if a vector),
	 * or 2 (if a matrix).
	 */
	int      ndima;		/* number of dimensions of A */
	int      ndimb;		/* number of dimensions of B */
	int      ndimc;		/* number of dimensions of C */

	/* the DIMENSION of each matrix (in the Fortran sense) */
	int      n1a;		/* dimensions 1 of matrix A */
	int      n2a;		/* dimensions 2 of matrix A */
	int      n1b;		/* dimensions 1 of matrix B */
	int      n2b;		/* dimensions 2 of matrix B */
	int      n1c;		/* dimensions 1 of matrix C */
	int      n2c;		/* dimensions 2 of matrix C */

	/*
	 * The dimensions of the matrix multiplication (For convenience only--
	 * these are the variable names used in the level 3 BLAS routines.)
	 */
	int      m;
	int      n;
	int      k;

	/* The increments in the following definitions are the "increments"
	 * as used by the X-interface in the BLAS 3 routines. They are the
	 * address distances in each dimension, in units of the "bucket
	 * size".  For example, if A is a real matrix, inc1a and inc2a are
	 * given in real words.  If A is a complex matrix, inc1a and inc2a
	 * are given in complex word pairs, so inc1a = 1 to specify every
	 * complex element.  This is different than the definition of the
	 * stride multiplier in the dope vector, which is always given in
	 * real words for word-aligned data, or in bits for byte-aligned
	 * data.  The stride multiplier is the increment multiplied by the
	 * element length.
	 */

	int      inc1a;		/* column and row increments, matrix A */
	int      inc2a;
	int      inc1b; 	/* column and row increments, matrix B */
	int      inc2b;
	int      inc1c;		/* column and row increments, matrix C */
	int      inc2c;
} MatrixDimenType;

#define LEN_DBL  (BITS_PER_WORD * 2)	/* two-word length in bits */
#define LEN_QUAD (BITS_PER_WORD * 4)	/* four-word length in bits */
#define LEN_OCT (BITS_PER_WORD * 8)	/* eight-word length in bits */


#if defined (_UNICOS)
#pragma _CRI inline _premult
#endif

static void
_premult (DopeVectorType * RESULT,
	 DopeVectorType * MATRIX_A,
	 DopeVectorType * MATRIX_B,
	 MatrixDimenType * MATDIM)
{
	long	nbits;		/* size of RESULT matrix in bits */
	long	nbytes;		/* size of RESULT matrix in bytes */
	int	intlen_sm;	/* length in bits divided by bitsperword */

	/* Parse the Dope Vector for MATRIX_A, MATRIX_B; */

	MATDIM->A = (void *) MATRIX_A->base_addr.a.ptr;	/* base address */
	MATDIM->ndima = MATRIX_A->n_dim;	/* number of dimensions */
	MATDIM->n1a = MATRIX_A->dimension[0].extent;
	MATDIM->inc1a = MATRIX_A->dimension[0].stride_mult;
	if (MATRIX_A->type_lens.int_len == LEN_DBL)
		MATDIM->inc1a >>= 1;	/* if double-word elements, divide
				 	 * increment by 2 */
	else if (MATRIX_A->type_lens.int_len == LEN_QUAD)
		MATDIM->inc1a >>= 2;	/* if quad word elements, divide
				 	 * increment by 4 */
	else if (MATRIX_A->type_lens.int_len == LEN_OCT)
		MATDIM->inc1a >>= 3;	/* if quad word elements, divide
				 	 * increment by 8 */

	if (MATDIM->ndima == 1) {
		MATDIM->n2a = 1;		/* default val for extent2 */
		MATDIM->inc2a = MATDIM->inc1a;	/* default val for stride2 */
	} else {
		MATDIM->n2a = MATRIX_A->dimension[1].extent;
		MATDIM->inc2a = MATRIX_A->dimension[1].stride_mult;
		if (MATRIX_A->type_lens.int_len == LEN_DBL)
			MATDIM->inc2a >>= 1;	/* if double-word elements,
				 	 	 * divide increment by 2 */
		else if (MATRIX_A->type_lens.int_len == LEN_QUAD)
			MATDIM->inc2a >>= 2;	/* if quad word elements,
				 	 	 * divide increment by 4 */
		else if (MATRIX_A->type_lens.int_len == LEN_OCT)
			MATDIM->inc2a >>= 3;	/* if quad word elements,
				 	 	 * divide increment by 8 */
    	}

	MATDIM->B = (void *) MATRIX_B->base_addr.a.ptr;	/* base address */
	MATDIM->ndimb = MATRIX_B->n_dim;	/* number of dimensions */
	MATDIM->n1b = MATRIX_B->dimension[0].extent;
	MATDIM->inc1b = MATRIX_B->dimension[0].stride_mult;
	if (MATRIX_B->type_lens.int_len == LEN_DBL)
		MATDIM->inc1b >>= 1;	/* if double-word elements, divide
				 	 * increment by 2 */
	else if (MATRIX_B->type_lens.int_len == LEN_QUAD)
		MATDIM->inc1b >>= 2;	/* if quad word elements, divide
					 * increment by 4 */
	else if (MATRIX_B->type_lens.int_len == LEN_OCT)
		MATDIM->inc1b >>= 3;	/* if quad word elements, divide
					 * increment by 8 */

	if (MATDIM->ndimb == 1) {
		MATDIM->n2b = 1;		/* default val for extent2 */
		MATDIM->inc2b = MATDIM->inc1b;	/* default val for stride2 */
	} else {
		MATDIM->n2b = MATRIX_B->dimension[1].extent;
		MATDIM->inc2b = MATRIX_B->dimension[1].stride_mult;
		if (MATRIX_B->type_lens.int_len == LEN_DBL)
			MATDIM->inc2b >>= 1;	/* if double-word elements,
				 		 * divide increment by 2 */
		else if (MATRIX_B->type_lens.int_len == LEN_QUAD)
			MATDIM->inc2b >>= 2;	/* if quad word elements,
						 * divide increment by 4 */
		else if (MATRIX_B->type_lens.int_len == LEN_OCT)
			MATDIM->inc2b >>= 3;	/* if quad word elements,
						 * divide increment by 8 */
	}

	if ((MATDIM->ndima == 1 && MATDIM->ndimb == 2) ||
 	    (MATDIM->ndima == 2 && MATDIM->ndimb == 1) ||
	    (MATDIM->ndima == 2 && MATDIM->ndimb == 2)) {
		;			/* correct combination of dimensions */
	} else {
#ifdef DEBUG
		printf("debug(premult) ndima=%d, ndimb =%d\n",MATDIM->ndima,
		   MATDIM->ndimb);
#endif
		ERROR(FESCIRNK);
		return;
	}

	/* Allocate RESULT array, if necessary. */
	if (!RESULT->assoc) {
		RESULT->assoc = 0;

		intlen_sm = RESULT->type_lens.int_len / BITS_PER_WORD;
		/* stride multiplier (in real words) = element length
		 * (in bits) / (64 words/bit) i.e., right-shifted
		 * 6 bits.
		 */
		if (intlen_sm == 0)
			intlen_sm = 1;
		RESULT->dimension[0].stride_mult = intlen_sm;
		RESULT->dimension[0].low_bound = 1;
	/* Set dimension-specific information for RESULT */
		if (MATDIM->ndima == 1) {
			RESULT->dimension[0].extent = MATDIM->n2b;
			nbits = MATDIM->n2b * RESULT->type_lens.int_len;

		} else if (MATDIM->ndimb == 1) {
			RESULT->dimension[0].extent = MATDIM->n1a;
			nbits = MATDIM->n1a * RESULT->type_lens.int_len;

		} else {
			RESULT->dimension[0].extent = MATDIM->n1a;

			RESULT->dimension[1].low_bound = 1;
			RESULT->dimension[1].extent = MATDIM->n2b;
			RESULT->dimension[1].stride_mult =
			   MATDIM->n1a * RESULT->dimension[0].stride_mult;
			nbits = MATDIM->n1a * MATDIM->n2b * RESULT->type_lens.int_len;
		}

		/* Allocate the space for RESULT */
		nbytes = nbits >> 3;	/* byte length = bit length/8 */
		if(nbytes != 0) {
			MATDIM->C = (void *) MALLOC(nbytes);
			if (MATDIM->C == NULL) {
				ERROR(FENOMEMY);
				return;
			}
		}
		RESULT->assoc = 1;
		RESULT->base_addr.a.ptr = (void *) MATDIM->C;
		RESULT->orig_base = (void *) MATDIM->C;
		RESULT->orig_size = nbits;
	}

	/* Parse the dope vector for RESULT (whether or not we just created
	 * it).
	 */

	MATDIM->C = (void *) RESULT->base_addr.a.ptr;
	MATDIM->ndimc = RESULT->n_dim;
	MATDIM->n1c = RESULT->dimension[0].extent;
	MATDIM->inc1c = RESULT->dimension[0].stride_mult;
	if (RESULT->type_lens.int_len == LEN_DBL)
		MATDIM->inc1c >>= 1;	/* if double word elements, divide
				 	 * increment by 2 */
	else if (RESULT->type_lens.int_len == LEN_QUAD)
		MATDIM->inc1c >>= 2;	/* if quad word elements, divide
				 	 * increment by 4 */
	else if (RESULT->type_lens.int_len == LEN_OCT)
		MATDIM->inc1c >>= 3;	/* if quad word elements, divide
				 	 * increment by 8 */

	if (MATDIM->ndimc == 1) {
		MATDIM->n2c = 1;		/* default val for extent2 */
		MATDIM->inc2c = MATDIM->inc1c;	/* default val for stride2 */
	} else {
		MATDIM->n2c = RESULT->dimension[1].extent;
		MATDIM->inc2c = RESULT->dimension[1].stride_mult;
		if (RESULT->type_lens.int_len == LEN_DBL)
			MATDIM->inc2c >>= 1;	/* if double word elements,
						 * divide increment by 2 */
		else if (RESULT->type_lens.int_len == LEN_QUAD)
			MATDIM->inc2c >>= 2;	/* if quad word elements,
				 		 * divide increment by 4 */
		else if (RESULT->type_lens.int_len == LEN_OCT)
			MATDIM->inc2c >>= 3;	/* if quad word elements, divide
					 	 * increment by 8 */
	}


	/* Check dimensions for conformability. */

#ifdef DEBUG
	printf("debug(premult) ndima=%d, ndimb=%d, ndimc=%d\n",
	    MATDIM->ndima, MATDIM->ndimb, MATDIM->ndimc);
	printf("debug(premult) n1a=%d, n2a=%d, inc1a=%d, inc2a=%d\n",
	    MATDIM->n1a, MATDIM->n2a, MATDIM->inc1a, MATDIM->inc2a);
	printf("debug(premult) n1b=%d, n2b=%d, inc1b=%d, inc2b=%d\n",
	    MATDIM->n1b, MATDIM->n2b, MATDIM->inc1b, MATDIM->inc2b);
	printf("debug(premult) n1c=%d, n2c=%d, inc1c=%d, inc2c=%d\n",
	    MATDIM->n1c, MATDIM->n2c, MATDIM->inc1c, MATDIM->inc2c);
#endif

	if (MATDIM->ndima == 2 && MATDIM->ndimb == 2) {
		if ((MATDIM->n2a == MATDIM->n1b) &&
		   (MATDIM->n1c == MATDIM->n1a) &&
		   (MATDIM->n2c == MATDIM->n2b)) {

		/* Dimensions are conformable. Copy them to variables m, k,
		 * and n. (This is done for convenience only.)
		 */
			MATDIM->m = MATDIM->n1a;
			MATDIM->k = MATDIM->n2a;
			MATDIM->n = MATDIM->n2b;
		} else {
#ifdef DEBUG
			printf("n1a=%d, n2a=%d, n1b=%d, n2b=%d, n1c=%d, n2c=%d\n", MATDIM->n1a, MATDIM->n2a, MATDIM->n1b,
			   MATDIM->n2b, MATDIM->n1c, MATDIM->n2c);
#endif
			ERROR(FESCICNF);
			return;
		}
	} else if (MATDIM->ndima == 2 && MATDIM->ndimb == 1) {
		if ((MATDIM->n2a == MATDIM->n1b) &&
		   (MATDIM->n1a == MATDIM->n1c)){

		/* Dimensions are conformable. Copy them to variables m, k,
		 * and n. (This is done for convenience only.)
		 */
			MATDIM->m = MATDIM->n1a;
			MATDIM->n = 1;
			MATDIM->k = MATDIM->n2a;
		} else {
#ifdef DEBUG
			printf("n2a=%d, n1b=%d, n1a=%d, n1c=%d\n",MATDIM->n2a,
			   MATDIM->n1b,MATDIM->n1a,MATDIM->n1c);
#endif
			ERROR(FESCICNF);
			return;
		}
	} else if (MATDIM->ndima == 1 && MATDIM->ndimb == 2) {
		if ((MATDIM->n1a == MATDIM->n1b) &&
		    (MATDIM->n1c == MATDIM->n2b)){

		/* Dimensions are conformable. Copy them to variables m, k,
		 * and n. (This is done for convenience only.)
		 */
			MATDIM->m = 1;
			MATDIM->n = MATDIM->n2b;
			MATDIM->k = MATDIM->n1b;
		} else {
#ifdef DEBUG
	    		printf("n1a=%d, n1b=%d, n1c=%d, n2b=%d\n",
	    		   MATDIM->n1a,MATDIM->n1b,MATDIM->n1c,MATDIM->n2b);
#endif
			ERROR(FESCICNF);
			return;
		}
	}
	return;
}
