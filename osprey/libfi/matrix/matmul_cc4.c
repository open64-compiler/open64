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


#pragma ident "@(#) libfi/matrix/matmul_cc4.c	92.1	07/09/99 15:18:08"

#include "matmul.h"

/*
 * Name of this entry point
 */
#define NAME _MATMUL_CC4
/*
 * Name of routine called do computation (if any)
 */
#if defined(UNICOS) || defined(UNIX_NAMING_CONVENTION)
#define SUBNAME _S4SGEMMX
#elif defined(BUILD_COMPILER_GNU) && defined(BUILD_OS_DARWIN)
#define SUBNAME underscore_s4sgemmx__
#else
#define SUBNAME _s4sgemmx__
#endif

/*
 * Type of constants alpha and beta
 */
#define RESULTTYPE	_f_real8

void
NAME(DopeVectorType *RESULT, DopeVectorType *MATRIX_A, 
	DopeVectorType *MATRIX_B)
{
    void    SUBNAME();
    RESULTTYPE  *Ar, *Ai;
    _f_real4    *Br, *Bi;
    RESULTTYPE  *Cr, *Ci;
    MatrixDimenType matdimdata, *MATDIM;

    const RESULTTYPE   neg_one =  (RESULTTYPE) (-1.0);
    const RESULTTYPE   one =  (RESULTTYPE) 1.0;
    const RESULTTYPE   zero = (RESULTTYPE) 0.0;

        MATDIM = (MatrixDimenType *) &matdimdata;

    /*
     * Parse dope vectors, and perform error checking.
     */

    _premult(RESULT, MATRIX_A, MATRIX_B, MATDIM);

    /*
     * Do real and imaginary parts separately.
     */

    Ar = (RESULTTYPE *) MATDIM->A;
    Ai = Ar + 1;
    MATDIM->inc1a *= 2;
    MATDIM->inc2a *= 2;

    Br = (_f_real4 *) MATDIM->B;
    Bi = Br + 1;
    MATDIM->inc1b *= 2;
    MATDIM->inc2b *= 2;

    Cr = (RESULTTYPE *) MATDIM->C;
    Ci = Cr + 1;
    MATDIM->inc1c *= 2;
    MATDIM->inc2c *= 2;

    /*
     * Perform the matrix multiplication.
     * Note:
     * (Cr + Ci*i) = (Ar + Ai*i)*(Br + Bi*i)
     *             = (Ar*Br - Ai*Bi) + (Ar*Bi + Ai*Br)*i
     * Do the transposed problem:  C' = B'*A'
     */

    /* real part */
    SUBNAME(&MATDIM->n,&MATDIM->m,&MATDIM->k,&one,Br,&MATDIM->inc2b,
            &MATDIM->inc1b,Ar,&MATDIM->inc2a,&MATDIM->inc1a,&zero,Cr,
            &MATDIM->inc2c,&MATDIM->inc1c);
    SUBNAME(&MATDIM->n,&MATDIM->m,&MATDIM->k,&neg_one,Bi,&MATDIM->inc2b,
            &MATDIM->inc1b,Ai,&MATDIM->inc2a,&MATDIM->inc1a,&one,Cr,
            &MATDIM->inc2c,&MATDIM->inc1c);

    /* imaginary part */
    SUBNAME(&MATDIM->n,&MATDIM->m,&MATDIM->k,&one,Br,&MATDIM->inc2b,
            &MATDIM->inc1b,Ai,&MATDIM->inc2a,&MATDIM->inc1a,&zero,Ci,
            &MATDIM->inc2c,&MATDIM->inc1c);
    SUBNAME(&MATDIM->n,&MATDIM->m,&MATDIM->k,&one,Bi,&MATDIM->inc2b,
            &MATDIM->inc1b,Ar,&MATDIM->inc2a,&MATDIM->inc1a,&one,Ci,
            &MATDIM->inc2c,&MATDIM->inc1c);
    return;
}
