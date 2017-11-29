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

      SUBROUTINE ARCDIV(R, X, Y)
      COMPLEX R, X, Y
      R = X/Y
      END

      SUBROUTINE ARCDDIV(R, X, Y)
      CALL AR_NOINTRIN_ERROR("ARCDDIV")
      END

      SUBROUTINE ARCABS (R, X)
      COMPLEX X
      INTRINSIC CABS
      R = CABS (X)
      END

      SUBROUTINE ARSQRT (R, X)
      INTRINSIC SQRT
      R = SQRT (X)
      END

      SUBROUTINE ARDSQRT (R, X)
      INTRINSIC DSQRT
      DOUBLE PRECISION R, X
      R = DSQRT (X)
      END

      SUBROUTINE ARCSQRT (R, X)
      INTRINSIC CSQRT
      COMPLEX R, X
      R = CSQRT (X)
      END

      SUBROUTINE ARLOG (R, X)
      INTRINSIC ALOG
      R = ALOG (X)
      END

      SUBROUTINE ARDLOG (R, X)
      INTRINSIC DLOG
      DOUBLE PRECISION R, X
      R = DLOG (X)
      END

      SUBROUTINE ARCLOG (R, X)
      INTRINSIC CLOG
      COMPLEX R, X
      R = CLOG (X)
      END

      SUBROUTINE AREXP (R, X)
      INTRINSIC EXP
      R = EXP (X)
      END

      SUBROUTINE ARDEXP (R, X)
      INTRINSIC DEXP
      DOUBLE PRECISION R, X
      R = DEXP (X)
      END

      SUBROUTINE ARCEXP (R, X)
      INTRINSIC CEXP
      COMPLEX R, X
      R = CEXP (X)
      END

      SUBROUTINE ARPOWII (IR, I, J)
CDIR$ INTEGER=64
      IR = I ** J
      END

      SUBROUTINE ARPOWRI (R, X, I)
      R = X ** I
      END

      SUBROUTINE ARPOWDI (R, D, I)
      DOUBLE PRECISION R, D
      R = D ** I
      END

      SUBROUTINE ARPOWCI (R, C, I)
      COMPLEX R, C
      R = C ** I
      END

      SUBROUTINE ARPOWIR (R, I, Y)
      R = I ** Y
      END

      SUBROUTINE ARPOWRR (R, X, Y)
      R = X ** Y
      END

      SUBROUTINE ARPOWDR (R, X, Y)
      DOUBLE PRECISION R, X
      R = X ** Y
      END

      SUBROUTINE ARPOWCR (R, X, Y)
      COMPLEX R, X
      R = X ** Y
      END

      SUBROUTINE ARPOWDD (R, X, Y)
      DOUBLE PRECISION R, X, Y
      R = X ** Y
      END

      SUBROUTINE ARPOWCC (R, X, Y)
      COMPLEX R, X, Y
      R = X ** Y
      END

c USMID = "\n%Z%%M%	%I%	%G% %U%\n";
c rcsid = "$Id: fintrin.f,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
