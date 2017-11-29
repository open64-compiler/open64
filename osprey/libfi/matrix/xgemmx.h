C
C
C  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
C
C  This program is free software; you can redistribute it and/or modify it
C  under the terms of version 2.1 of the GNU Lesser General Public License 
C  as published by the Free Software Foundation.
C
C  This program is distributed in the hope that it would be useful, but
C  WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
C
C  Further, this software is distributed without any warranty that it is
C  free of the rightful claim of any third person regarding infringement 
C  or the like.  Any license provided herein, whether implied or 
C  otherwise, applies only to this software file.  Patent licenses, if
C  any, provided herein do not apply to combinations of this program with 
C  other software, or any other product whatsoever.  
C
C  You should have received a copy of the GNU Lesser General Public 
C  License along with this program; if not, write the Free Software 
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
C  USA.
C
C  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
C  Mountain View, CA 94043, or:
C
C  http://www.sgi.com
C
C  For further information regarding this notice, see:
C
C  http://oss.sgi.com/projects/GenInfo/NoticeExplan
C
C

* USMID @(#) libfi/matrix/xgemmx.h	92.1	02/03/99 09:33:12
*
* Purpose
* =======
*
* XGEMMX  performs the matrix-matrix operation
*
*    C = alpha*A*B + beta*C,
*
*
* alpha and beta are scalars, and A, B and C are matrices, with A
* an m by k matrix, B a k by n matrix and C an m by n matrix.
*
* Note:  This routine does not perform error checking on the
*        incoming parameters.  Furthermore, if one wants to
*        compute C = alpha*A'*B + beta*C, just reverse
*        INC1A and INC2A.  Likewise for C and B.
*
* This routine is based on the X interface to the Level 3 BLAS
* routine.
*
* This is the computational part of several general-purpose routines
* that work with various combinations of matrix argument types.  This
* header file is included in the specific routines (e.g., ISGEMMX@,
* which performs integer times real matrix multiplication).
*
* Arguments
* =========
*
* M      - INTEGER.
*          On entry,  M  specifies  the number  of rows  of the  matrix
*          A  and of the  matrix  C.  M  must  be at least  zero.
*          Unchanged on exit.
*
* N      - INTEGER.
*          On entry,  N  specifies the number  of columns of the matrix
*          B and the number of columns of the matrix C. N must be
*          at least zero.
*          Unchanged on exit.
*
* K      - INTEGER.
*          On entry,  K  specifies  the number of columns of the matrix
*          A and the number of rows of the matrix B. K must
*          be at least  zero.
*          Unchanged on exit.
*
* ALPHA  - RESULTDECLARE
*          On entry, ALPHA specifies the scalar alpha.
*          Unchanged on exit.
*
* A      - ARGDECLARE_A *
*          (i.e., a pointer to an array of type ARGDECLARE_A)
*          Unchanged on exit.
*
* INC1A  - INTEGER.
*          Column increment of A.
*          Unchanged on exit.
*
* INC2A  - INTEGER.
*          Row increment of A.
*          Unchanged on exit.
*
* B      - ARGDECLARE_B *
*          (i.e., a pointer to an array of type ARGDECLARE_B)
*          Unchanged on exit.
*
* INC1B  - INTEGER.
*          Column increment of B.
*          Unchanged on exit.
*
* INC2B  - INTEGER.
*          Row increment of B.
*          Unchanged on exit.
*
* BETA   - RESULTDECLARE
*          On entry, BETA  specifies the scalar beta.  When  
*          BETA is supplied as zero then C need not be set on input.
*          Unchanged on exit.
*
* C      - RESULTDECLARE *
*          (i.e., a pointer to an array of type RESULTDECLARE)
*          On exit, the array C is overwritten by the m by n matrix
*          ( alpha*A*B + beta*C ).
*
* INC1C  - INTEGER.
*          Column increment of C.
*          Unchanged on exit.
*
* INC2C  - INTEGER.
*          Row increment of C.
*          Unchanged on exit.
*
*********************************************************************
*                                                                   *
* Author: Math Software Group, Cray Research, Inc.                  *
*                                                                   *
*********************************************************************
*                                                                   *
*      The routine that includes this header file should declare    *
*      the subroutine name and declare the types of                 *
*      the scalar constants ALPHA and BETA, and the types           *
*      of the arrays, A, B, C, and the type of the local            *
*      scalar TEMP, as in the following sample.                     *
*                                                                   *
*       SUBROUTINE XGEMMX( M, N, K, ALPHA, A, INC1A, INC2A, B,      *
*     $                   B, INC1B, INC2B, BETA, C, INC1C, INC2C )  *
*      NOTE:                                                        *
*                                                                   *
*      Scalar constants and local scalar should be declared to have *
*      the same type as the result array.                           *
*                                                                   *
*      .. Scalar Constant Arguments ..                              *
*      REAL               ALPHA, BETA                               *
*      .. Local Scalar ..                                           *
*      REAL               TEMP                                      *
*      .. Array Arguments ..                                        *
*      REAL               A( * ), B( * )                            *
*      REAL               C( * )                                    *
*                                                                   *
*********************************************************************
*     .. Scalar Arguments ..
      INTEGER            M, N, K
      INTEGER            INC1A, INC2A, INC1B, INC2B
      INTEGER            INC1C, INC2C 
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      INTEGER            I, J, L
      INTEGER            IA, IB, IC
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER        ( ONE = 1.0, ZERO = 0.0 )

*     .. Executable Statements ..

*     Quick return if possible.
      IF (M.EQ.0 .OR. N.EQ.0 .OR.
     $    ( (ALPHA.EQ.ZERO).AND.(BETA.EQ.ONE) ) )
     $   RETURN

*     Set the result to zeros and exit
      IF ( K.EQ.0 ) THEN
           DO, J = 1, N
               DO, I = 1, M
                   IC = INC2C*(J - 1) + INC1C*(I - 1) + 1
                   C( IC ) = ZERO
               END DO
           END DO
           RETURN
       END IF

*     And if alpha.eq.zero.

	IF ( ALPHA.EQ.ZERO ) THEN
	    IF ( BETA.EQ.ZERO ) THEN
		DO, J = 1, N
		    DO, I = 1, M
			IC = INC2C*(J - 1) + INC1C*(I - 1) + 1
			C( IC ) = ZERO
		    END DO
		END DO
	    ELSE
		DO, J = 1, N
		    DO, I = 1, M
			IC = INC2C*(J - 1) + INC1C*(I - 1) + 1
			C( IC ) = BETA*C( IC )
		    END DO
		END DO
	    END IF
	    RETURN
	END IF
*
*     Start the operations.
*
*     Form  C := alpha*A*B + beta*C.
*
	DO, J = 1, N
	    IF ( BETA.EQ.ZERO ) THEN
		DO, I = 1, M
		    IC = INC2C*(J - 1) + INC1C*(I - 1) + 1
		    C( IC ) = ZERO
		END DO
	    ELSE IF ( BETA.NE.ONE ) THEN
		DO, I = 1, M
		    IC = INC2C*(J - 1) + INC1C*(I - 1) + 1
		    C( IC ) = BETA*C( IC )
		END DO
	    END IF
	    DO, L = 1, K
		IB = INC2B*(J - 1) + INC1B*(L - 1 ) + 1
		IF ( B( IB ) .NE. ZERO ) THEN
		    TEMP = ALPHA*B( IB )
		    DO, I = 1, M
			IC = INC2C*(J - 1) + INC1C*(I - 1) + 1
			IA = INC2A*(L - 1) + INC1A*(I - 1) + 1
			C( IC ) = C( IC ) + TEMP*A( IA ) 
		    END DO
		END IF
	    END DO
	END DO
	RETURN

*     End of XGEMMX
	END
