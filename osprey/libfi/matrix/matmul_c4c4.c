/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libfi/matrix/matmul_c4c4.c	92.1	07/09/99 15:18:08"

#include "matmul.h"

/*
 * Name of this entry point
 */

#define NAME _MATMUL_C4C4

/*
 * Subroutine called to do computation:
 */
#if defined(UNICOS) || defined(UNIX_NAMING_CONVENTION)
#define SUBNAME1 _C4GEMMX
#define SUBNAME2 _C4GEMVX
#elif defined(BUILD_COMPILER_GNU) && defined(BUILD_OS_DARWIN)
#define SUBNAME1 underscore_c4gemmx__
#define SUBNAME2 underscore_c4gemvx__
#else
#define SUBNAME1 _c4gemmx__
#define SUBNAME2 _c4gemvx__
#endif

/*
 * Type of constants alpha and beta
 */
#define RESULTTYPE	_f_comp4

void
NAME(DopeVectorType * RESULT, DopeVectorType * MATRIX_A,
     DopeVectorType * MATRIX_B)
{
    void    SUBNAME1();
    void    SUBNAME2();

    MatrixDimenType matdimdata, *MATDIM;
    const int       noconj = 0;

#if COMPLEX_AS_SCALAR
    const RESULTTYPE one = (RESULTTYPE) 1.0;
    const RESULTTYPE zero = (RESULTTYPE) 0.0;
#else
    const RESULTTYPE one = {{ 1.0, 0.0 }};
    const RESULTTYPE zero = {{ 0.0, 0.0 }};
#endif

        MATDIM = (MatrixDimenType *) &matdimdata;

    /*
     * Parse dope vectors, and perform error checking.
     */

    _premult(RESULT, MATRIX_A, MATRIX_B, MATDIM);

    /*
     * Perform the matrix multiplication.
     */

    if (MATDIM->ndimb == 1) {
	/*
	 * y = Ax
	 */

	SUBNAME2(&noconj, &MATDIM->n1a, &MATDIM->n2a, &one, MATDIM->A,
	       &MATDIM->inc1a, &MATDIM->inc2a, MATDIM->B, &MATDIM->inc1b,
	       &zero, MATDIM->C, &MATDIM->inc1c);
	return;

    } else if (MATDIM->ndima == 1) {
	/*
	 * y = xB, equivalent to y' = B'x'
	 */

	SUBNAME2(&noconj, &MATDIM->n2b, &MATDIM->n1b, &one, MATDIM->B,
	       &MATDIM->inc2b, &MATDIM->inc1b, MATDIM->A, &MATDIM->inc1a,
	       &zero, MATDIM->C, &MATDIM->inc1c);
	return;
    } else {
	/*
	 * C = AB (full matrix multiplication)
	 */

	SUBNAME1(&noconj, &noconj, &MATDIM->n1a, &MATDIM->n2b, &MATDIM->n2a,
		&one, MATDIM->A, &MATDIM->inc1a, &MATDIM->inc2a, MATDIM->B,
		&MATDIM->inc1b, &MATDIM->inc2b, &zero, MATDIM->C,
		&MATDIM->inc1c, &MATDIM->inc2c);
	return;
    }
}
