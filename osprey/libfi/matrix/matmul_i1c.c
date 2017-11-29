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


#pragma ident "@(#) libfi/matrix/matmul_i1c.c	92.1	07/09/99 15:18:08"
#include "matmul.h"

/*
 * Name of this entry point
 */
#define NAME _MATMUL_I1C
/*
 * Name of routine called do computation (if any)
 */
#if defined(BUILD_COMPILER_GNU) && defined(BUILD_OS_DARWIN)
#define SUBNAME underscore_i1sgemmx__
#else /* defined(BUILD_COMPILER_GNU) && defined(BUILD_OS_DARWIN) */
#define SUBNAME _i1sgemmx__
#endif /* defined(BUILD_COMPILER_GNU) && defined(BUILD_OS_DARWIN) */
/*
 * Type of constants alpha and beta
 */
#define RESULTTYPE   _f_real8

void
NAME(DopeVectorType *RESULT, DopeVectorType *MATRIX_A,
     DopeVectorType *MATRIX_B)
{
    void    SUBNAME();
    const RESULTTYPE   one =  (RESULTTYPE) 1.0;
    const RESULTTYPE   zero = (RESULTTYPE) 0.0;
    RESULTTYPE *Br, *Bi;
    RESULTTYPE *Cr, *Ci;
    MatrixDimenType matdimdata, *MATDIM;

        MATDIM = (MatrixDimenType *) &matdimdata;

    /*
     * Parse dope vectors, and perform error checking.
     */

    _premult(RESULT, MATRIX_A, MATRIX_B, MATDIM);

    /*
     * Do real and imaginary parts separately.
     */

    Br = (RESULTTYPE *) MATDIM->B;
    Bi = Br + 1;
    MATDIM->inc1b *= 2;
    MATDIM->inc2b *= 2;

    Cr = (RESULTTYPE *) MATDIM->C;
    Ci = Cr + 1;
    MATDIM->inc1c *= 2;
    MATDIM->inc2c *= 2;

    /*
     * Perform the matrix multiplication.
     */

    /* real part */
    SUBNAME(&MATDIM->m, &MATDIM->n, &MATDIM->k, &one, MATDIM->A,
         &MATDIM->inc1a, &MATDIM->inc2a, Br, &MATDIM->inc1b, &MATDIM->inc2b,
         &zero, Cr, &MATDIM->inc1c, &MATDIM->inc2c);
    /* imaginary part */
    SUBNAME(&MATDIM->m, &MATDIM->n, &MATDIM->k, &one, MATDIM->A,
         &MATDIM->inc1a, &MATDIM->inc2a, Bi, &MATDIM->inc1b, &MATDIM->inc2b,
         &zero, Ci, &MATDIM->inc1c, &MATDIM->inc2c);
    return;
}
