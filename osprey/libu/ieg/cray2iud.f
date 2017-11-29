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


      INTEGER FUNCTION
     +CRAY2IUD@(TYPE, NUM, FORN, BITOFF, CRAY, STRIDE, CRAYCH)

C     The CRAY2IUD@ function converts CRAY Fortran data types to data types for
C     an Ultrix 32-bit platform with IEEE floating-point little-endian 
C     representation.  The REAL and DOUBLE PRECISION data representations
C     are both 64 bits.   This function is a close variant of CRAY2IEU.
C
C     The returned function value is as follows:
C
C         <0  Parameter error; no translation performed
C          0  Translation complete; no errors
C         >0  Translation complete; return value is the number of
C             values that completely overflowed or completely under-
C             flowed.

      IMPLICIT NONE

      INTEGER TYPE 		
      INTEGER NUM
      INTEGER FORN(0:0)
      INTEGER BITOFF
      INTEGER CRAY(0:0)
      INTEGER STRIDE
      CHARACTER * (*) CRAYCH(0:0)

      INTEGER DTREAL
      PARAMETER (DTREAL       = 2)
      INTEGER DTSPECREAL
      PARAMETER (DTSPECREAL   = 8)

      INTEGER           INC

      INTEGER           CRAY2IEU

      INTEGER           NSIZ
      INTEGER           ISIZE(0:8)
      INTEGER           ALSTUF
      INTEGER           PADVAL

      COMMON  /G@IUD@SZ/ NSIZ,ISIZE

C     The following data is used by the I/O library to size the foreign data

      DATA NSIZ /9/
      DATA ISIZE /64, 32, 64, 64, 64, 32, 8, 16, 64/

      INTEGER NTYPE

      EXTERNAL          CRAY2IEU

C     The following data is used by the I/O library to determine padding
C     requirements between character and non-character items in the
C     foreign file.

      COMMON  /G@IUD@AL/ ALSTUF,PADVAL
      DATA ALSTUF,PADVAL /0,0/           ! IEEE/generic does no alignment

      IF (NUMARG() .EQ. 5) THEN
        INC = 1
      ELSE IF (NUMARG() .GE. 6) THEN
        INC = STRIDE
      ELSE
        GOTO 9000 
      ENDIF

C     Map DTREAL (2)  to DTSPECREAL (8).  DTSPECREAL coverts 8-byte reals
C     to 8-byte reals.

      NTYPE = TYPE
      IF (TYPE.EQ.DTREAL) NTYPE = DTSPECREAL

      CRAY2IUD@ = CRAY2IEU(NTYPE, NUM, FORN, BITOFF, CRAY, INC, CRAYCH)
      RETURN

 9000 CONTINUE
      CRAY2IUD@ = -1             !  Some parameter error
      RETURN
CDIR$ ID "@(#) libu/ieg/cray2iud.f	92.0	10/08/98 14:57:41"
      END
