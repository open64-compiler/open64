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


/* USMID @(#) libfi/matrix/mmx_c.h	92.1	10/28/98 22:50:56 */

/* This is a wrapper file for the matrix multiply routines */
#include <cray/dopevec.h>	/* description of Fortran 90 Dope Vector */
#include "intrin.h"
#include "mmx_dlutil.h"

extern void MMMM(DopeVectorType * R, DopeVectorType * A, DopeVectorType * B, void* blas);
extern void MMMV(DopeVectorType * R, DopeVectorType * A, DopeVectorType * B);
extern void MMVM(DopeVectorType * R, DopeVectorType * A, DopeVectorType * B);

void MMDD(DopeVectorType * R, DopeVectorType * A, DopeVectorType * B) 
{
  int ranka,rankb;

  ranka = A->n_dim;
  rankb = B->n_dim;

  if (ranka == 2 && rankb == 2) {
    __f90_open_blas_lib();
    MMMM(R,A,B,BLASROUTINE);
  } else if (ranka == 2 && rankb == 1) {
    MMMV(R,A,B);
  } else if (ranka == 1 && rankb == 2) {
    MMVM(R,A,B);
  } else {
    ERROR(FESCIRNK);
  }
}
