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


      SUBROUTINE ARCDABS (R, X)
      REAL*8 R
      COMPLEX*16 X
      R = ABS (X)
      END

      SUBROUTINE ARCXDABS (R, X)
      REAL*16 R
      COMPLEX*32 X
      R = ABS (X)
      END

      SUBROUTINE ARCDSQRT (R, X)
      COMPLEX*16 R, X
      R = SQRT (X)
      END

      SUBROUTINE ARCXDSQRT (R, X)
      COMPLEX*32 R, X
      R = SQRT (X)
      END

      SUBROUTINE ARCDLOG (R, X)
      COMPLEX*16 R, X
      R = LOG (X)
      END

      SUBROUTINE ARCXDLOG (R, X)
      COMPLEX*32 R, X
      R = LOG (X)
      END

      SUBROUTINE ARCDEXP (R, X)
      COMPLEX*16 R, X
      R = EXP (X)
      END

      SUBROUTINE ARCXDEXP (R, X)
      COMPLEX*32 R, X
      R = EXP (X)
      END

      SUBROUTINE ARPOWCDI (R, C, I)
      COMPLEX*16 R, C
      INTEGER*4 I(2)
      R = C ** I(2)
      END

      SUBROUTINE ARPOWCDJ (R, C, J)
      COMPLEX*16 R, C
      INTEGER*8 J
      R = C ** J
      END

      SUBROUTINE ARPOWCXDI (R, C, I)
      COMPLEX*32 R, C
      INTEGER*4 I(2)
      R = C ** I(2)
      END

      SUBROUTINE ARPOWCXDJ (R, C, J)
      COMPLEX*32 R, C
      INTEGER*8 J
      R = C ** J
      END

      SUBROUTINE ARPOWCDCD (R, X, Y)
      COMPLEX*16 R, X, Y
      R = X ** Y
      END

      SUBROUTINE ARPOWCXDCXD (R, X, Y)
      COMPLEX*32 R, X, Y
      R = X ** Y
      END

c USMID = "\n%Z%%M%	%I%	%G% %U%\n";
c rcsid = "fintrin_dc_sparc.f,v 2.3 1995/09/26 20:40:21 jk Exp";
