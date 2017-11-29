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


      SUBROUTINE VXGCTI(FPN,DEST,ISB,NUM,IER,INC)
C
C     The VXDCTI subroutine converts Cray 64-bit single-precision
C     floating-point numbers to VAX G_format single-precision
C     floating-point numbers.  Numbers that produce an underflow when
C     converted to VAX format are converted to 32 binary zeros.  Numbers
C     that are in overflow on the CRAY are converted to a "reserved"
C     floating-point representation with the sign bit set if negative.
C     Numbers that are valid on the CRAY but overflow on the VAX are
C     converted to the most positive possible number or most negative
C     possible number, depending on the sign.
C
C     Presently, you must supply a parameter which, in the future,
C     will contain a nonzero value if any numbers converted produced
C     an overflow.  No such indication will be given for underflow.
C
      REAL    FPN(1)              ! Variable or array of any length and
C                                 ! type real, containing Cray 64-bit,
C                                 ! single-precision, floating-point
C                                 ! numbers to convert.
C
      INTEGER DEST(1)             ! Variable or array of type real to
C                                 ! contain the converted values.
C
      INTEGER ISB                 ! Byte number at which to begin
C                                 ! storing the converted results.  Type
C                                 ! integer variable, expression, or
C                                 ! constant.  Bytes are numbered from
C                                 ! 1, beginning at the leftmost byte
C                                 ! position of DEST.
C
      INTEGER NUM                 ! Number of Cray floating-point
C                                 ! numbers to convert.  Type integer
C                                 ! variable, expression, or constant.
C
      INTEGER IER                 ! Overflow indicator of type
C                                 ! integer.  Value is 0 if all Cray
C                                 ! values convert to VAX values without
C                                 ! overflow.  Value is nonzero if one
C                                 ! or more Cray values overflowed in
C                                 ! the conversion.
C
      INTEGER INC                 ! Memory increment for fetching the
C                                 ! number to be converted.  Optional
C                                 ! parameter of type integer variable,
C                                 ! expression, or constant.  Default
C                                 ! value is 1.

      INTEGER ITEMP(64), JTEMP(32)

CDIR$ VFUNCTION VG64O
      INTEGER   VG64O

      IF (NUMARG() .EQ. 5) THEN
        JNC = 1
      ELSE IF (NUMARG() .EQ. 6) THEN
        JNC = INC
      ELSE
        CALL ABORT('VAX conversion routine called with incorrect number 
     +of arguments')
      ENDIF

      IF (NUM .LT. 0) THEN
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF

      NUMC = NUM
      JSB  = ISB
      J    = 1
      IER  = 0

      IF (NUM .EQ. 0) RETURN

   40 CONTINUE

      NUMP = MIN0(NUMC,64)

CDIR$ SHORTLOOP
      DO 80 I = 1, NUMP
        ITEMP(I) = VG64O(DBLE(FPN(J)))
        J        = J + JNC
   80 CONTINUE

      CALL VXMOVE00(ITEMP,1,NUMP*8,DEST,JSB)

      JSB  = JSB + 8*NUMP
      NUMC = NUMC - NUMP
      IF (NUMC .GT. 0) GO TO 40

      RETURN
CDIR$ ID "@(#) libu/vms/vxgcti.f	92.0	10/08/98 14:57:41"
      END
