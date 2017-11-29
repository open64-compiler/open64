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


      SUBROUTINE ARCDHERE
* Reference this routine as a soft external to see if this file
* has been loaded.
      END

      SUBROUTINE ARCDABS (R, X)
      DOUBLE PRECISION R
      COMPLEX(16) X
      INTRINSIC CDABS
      CALL ARCDHERE
      R = CDABS (X)
      END

      SUBROUTINE ARCDSQRT (R, X)
      INTRINSIC CDSQRT
      COMPLEX(16) R, X
      CALL ARCDHERE
      R = CDSQRT (X)
      END

      SUBROUTINE ARCDLOG (R, X)
      INTRINSIC CDLOG
      COMPLEX(16) R, X
      CALL ARCDHERE
      R = CDLOG (X)
      END

      SUBROUTINE ARCDEXP (R, X)
      INTRINSIC CDEXP
      COMPLEX(16) R, X
      CALL ARCDHERE
      R = CDEXP (X)
      END

      SUBROUTINE ARPOWCDI (R, C, I)
      COMPLEX(16) R, C
c				Remove explicit CDTOI usage when cft90 fixed
      complex(16) cdtoi
      CALL ARCDHERE
c      R = C ** I
      r = cdtoi(c,i)
      END

      SUBROUTINE ARPOWCDCD (R, X, Y)
      COMPLEX(16) R, X, Y
      CALL ARCDHERE
      R = X ** Y
      END

c USMID = "\n%Z%%M%	%I%	%G% %U%\n";
c rcsid = "$Id: fintrin_dc.f,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
