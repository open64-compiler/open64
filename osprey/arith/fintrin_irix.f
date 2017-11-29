C
C
C  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
C
C  This program is free software; you can redistribute it and/or modify it
C  under the terms of version 2 of the GNU General Public License as
C  published by the Free Software Foundation.
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
C  You should have received a copy of the GNU General Public License along
C  with this program; if not, write the Free Software Foundation, Inc., 59
C  Temple Place - Suite 330, Boston MA 02111-1307, USA.
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

!$$$      integer function f90_verify(string,set,back)
!$$$      character*(*) string,set
!$$$      logical back
!$$$      f90_verify = verify(string,set,back)
!$$$      end
!$$$
!$$$      integer function f90_scan(string,set,back)
!$$$      character*(*) string,set
!$$$      logical back
!$$$      f90_scan = scan(string,set,back)
!$$$      end
!$$$
!$$$      integer function f90_index(string,substring,back)
!$$$      character*(*) string,substring
!$$$      logical back
!$$$      f90_index = index(string,substring,back)
!$$$      end

      SUBROUTINE ARCDIV(R, X, Y)
      COMPLEX*8 R, X, Y
      R = X/Y
      END

      SUBROUTINE ARCDDIV(R, X, Y)
      COMPLEX*16 R, X, Y
      R = X/Y
      END

      SUBROUTINE ARCXDDIV(R, X, Y)
      COMPLEX*32 R, X, Y
      R = X/Y
      END

      SUBROUTINE ARCABS (R, X)
      COMPLEX*8 X
      REAL*4 R(2)
      INTRINSIC CABS
      R(2) = CABS (X)
      END

      SUBROUTINE ARSQRT (R, X)
      REAL*4 R(2), X(2)
      INTRINSIC SQRT
      R(2) = SQRT (X(2))
      END

      SUBROUTINE ARDSQRT (R, X)
      REAL*8 R, X
      INTRINSIC SQRT
      R = SQRT (X)
      END

      SUBROUTINE ARXDSQRT (R, X)
      REAL*16 R, X
      INTRINSIC SQRT
      R = SQRT (X)
      END

      SUBROUTINE ARCSQRT (R, X)
      INTRINSIC CSQRT
      COMPLEX*8 R, X
      R = CSQRT (X)
      END

      SUBROUTINE ARLOG (R, X)
      REAL*4 R(2), X(2)
      INTRINSIC LOG
      R(2) = LOG (X(2))
      END

      SUBROUTINE ARDLOG (R, X)
      REAL*8 R, X
      R = LOG (X)
      END

      SUBROUTINE ARXDLOG (R, X)
      REAL*16 R, X
      R = LOG (X)
      END

      SUBROUTINE ARCLOG (R, X)
      INTRINSIC CLOG
      COMPLEX*8 R, X
      R = CLOG (X)
      END

      SUBROUTINE AREXP (R, X)
      REAL*4 R(2), X(2)
      INTRINSIC EXP
      R(2) = EXP (X(2))
      END

      SUBROUTINE ARDEXP (R, X)
      REAL*8 R, X
      INTRINSIC EXP
      R = EXP (X)
      END

      SUBROUTINE ARXDEXP (R, X)
      REAL*16 R, X
      INTRINSIC EXP
      R = EXP (X)
      END

      SUBROUTINE ARCEXP (R, X)
      INTRINSIC CEXP
      COMPLEX*8 R, X
      R = CEXP (X)
      END

      SUBROUTINE ARPOWII (IR, I, J)
      INTEGER*4 IR(2), I(2), J(2)
      IR(2) = I(2) ** J(2)
      END

      SUBROUTINE ARPOWJJ (IR, I, J)
      INTEGER*8 IR, I, J
      IR = I ** J
      END

      SUBROUTINE ARPOWRI (R, X, I)
      INTEGER*4 I(2)
      REAL*4 R(2), X(2)
      R(2) = X(2) ** I(2)
      END

      SUBROUTINE ARPOWRJ (R, X, J)
      INTEGER*8 J
      REAL*4 R(2), X(2)
      R(2) = X(2) ** J
      END

      SUBROUTINE ARPOWDI (R, D, I)
      REAL*8 R, D
      INTEGER*4 I(2)
      R = D ** I(2)
      END

      SUBROUTINE ARPOWDJ (R, D, J)
      REAL*8 R, D
      INTEGER*8 J
      R = D ** J
      END

      SUBROUTINE ARPOWXDI (R, D, I)
      REAL*16 R, D
      INTEGER*4 I(2)
      R = D ** I(2)
      END

      SUBROUTINE ARPOWXDJ (R, D, J)
      REAL*16 R, D
      INTEGER*8 J
      R = D ** J
      END

      SUBROUTINE ARPOWCI (R, C, I)
      COMPLEX*8 R, C
      INTEGER*4 I(2)
      R = C ** I(2)
      END

      SUBROUTINE ARPOWCJ (R, C, J)
      COMPLEX*8 R, C
      INTEGER*8 J
      R = C ** J
      END

      SUBROUTINE ARPOWIR (R, I, X)
      REAL*4 R(2), X(2)
      INTEGER*4 I(2)
      R(2) = I(2) ** X(2)
      END

      SUBROUTINE ARPOWJR (R, J, X)
      REAL*4 R(2), X(2)
      INTEGER*8 J
      R(2) = J ** X(2)
      END

      SUBROUTINE ARPOWRR (R, X, Y)
      REAL*4 R(2), X(2), Y(2)
      R(2) = X(2) ** Y(2)
      END

      SUBROUTINE ARPOWDR (R, X, Y)
      REAL*8 R, X
      REAL*4 Y(2)
      R = X ** Y(2)
      END

      SUBROUTINE ARPOWXDR (R, X, Y)
      REAL*16 R, X
      REAL*4 Y(2)
      R = X ** Y(2)
      END

      SUBROUTINE ARPOWCR (R, X, Y)
      COMPLEX*8 R, X
      REAL*4 Y(2)
      R = X ** Y(2)
      END

      SUBROUTINE ARPOWDD (R, X, Y)
      REAL*8 R, X, Y
      R = X ** Y
      END

      SUBROUTINE ARPOWXDXD (R, X, Y)
      REAL*16 R, X, Y
      R = X ** Y
      END

      SUBROUTINE ARPOWCC (R, X, Y)
      COMPLEX*8 R, X, Y
      R = X ** Y
      END

      SUBROUTINE ARMODI (R, I, M)
      INTEGER(4) R(2), I(2), M(2)
      R(2) = MODULO(I(2),M(2))
      END

      SUBROUTINE ARMODJ (R, J, M)
      INTEGER(8) R, J, M
      R = MODULO(J,M)
      END

      SUBROUTINE ARMOD (R, X, Y)
      REAL*4 R(2), X(2), Y(2)
      R(2) = MODULO(X(2),Y(2))
      END

      SUBROUTINE ARMODD (R, X, Y)
      REAL*8 R, X, Y
      R = MODULO(X,Y)
      END

      SUBROUTINE ARMODXD (R, X, Y)
      REAL*16 R, X, Y
      R = MODULO(X,Y)
      END

      SUBROUTINE ARSELRK (K, P, R)
      INTEGER*4 K(2)
      INTEGER*4,OPTIONAL:: P(2), R(2)
      IF(PRESENT(P).AND.PRESENT(R)) THEN
         K(2) = SELECTED_REAL_KIND(P(2), R(2))
      ELSEIF(PRESENT(P)) THEN
         K(2) = SELECTED_REAL_KIND(P(2))
      ELSE
         K(2) = SELECTED_REAL_KIND(R=R(2))
      ENDIF
      END
