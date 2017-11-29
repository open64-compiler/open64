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


      SUBROUTINE MOVBIT(A,AZ,NZ,B,BZ)
CDIR$ ID "@(#) libu/util/posix/movbit32.f	92.0	10/08/98 14:57:41"
      IMPLICIT NONE
*
*     Move bits from the source array to the destination array
*
*     This routine has been modified for the SPARC 32-bit architecture.
*     Main changes:
*	1) Using 32 bit words, not 64 bit words
*	2) SPARC uses byte addresses not word addresses
*	3) SPARC 32 bit word is *not* large enough to hold bit addresses,
*		the byte address and bit offsets are always kept in 
*		separate varables, not in one word as the CRAY code did
*	4) The MEMORY POINTER is handled somewhat differently.  SPARC
*		cannot dimension MEMORY(0:0) and index into this space.
*
*
      INTEGER A(0:*)         ! The source array
      INTEGER AZ             ! The bit offset into the source array
      INTEGER NZ             ! The number of bits to move
      INTEGER B(0:*)         ! The destination array
      INTEGER BZ             ! The bit offset into the destination array
*
      INTEGER A0
      INTEGER AFBA
      INTEGER AFWA
      INTEGER ALBA
      INTEGER ALWA
      INTEGER B0
      INTEGER BFBA
      INTEGER BFIRST
      INTEGER BFWA
      INTEGER BLAST
      INTEGER BLBA
      INTEGER BLWA
      INTEGER BPW
*	Bits per word
      PARAMETER (BPW=32)
      INTEGER DELTA
      LOGICAL FORWARD
      INTEGER I
      INTEGER ITEMP
      INTEGER I2
      INTEGER MEMORY(0:1)
      POINTER (MEMPTR,MEMORY)
      SAVE MEMPTR
      DATA MEMPTR/0/
      INTEGER N

      integer shiftl, shiftr, mask, csmg  ! CF90 intrinsic functions.

      A0 = AZ - 1
      B0 = BZ - 1
      N  = NZ
      GO TO 20

*     This entry is the same as MOVBIT, only the bit offsets are ZERO
*     based. (!)

      ENTRY MOVBITZ(A,AZ,NZ,B,BZ)
      A0 = AZ
      B0 = BZ
      N  = NZ
      GO TO 20

      ENTRY STRMOV(A,AZ,NZ,B,BZ)
      A0 = (AZ-1)*8
      B0 = (BZ-1)*8
      N  = NZ*8
   20 CONTINUE

*     Some preliminary limit checks:

      IF (N .LE. 0)RETURN   ! Exit silently if asked to move zero bits

*     SPARC addresses are byte addresses

*     Calculate first and last byte addresses and bit offsets

      AFWA = shiftr(loc(a(0)),2) + shiftr(a0,5)
      AFBA = and(a0,MASK(64-5))
      BFWA = shiftr(loc(b(0)),2) + shiftr(b0,5)
      BFBA = and(b0,MASK(64-5))

      ALWA = shiftr(loc(a(0)),2) + shiftr((a0+n-1),5)
      BLWA = shiftr(loc(b(0)),2) + shiftr((b0+n-1),5)

*     If the source is entirely within one word and if the
*     destination is entirely within one word, use special
*     scalar code to perform the move.

      IF (AFWA .EQ. ALWA .AND. BFWA .EQ. BLWA)THEN
	MEMPTR = shiftl(afwa,2)
        ITEMP = SHIFTR(SHIFTL(MEMORY(0),AFBA),BFBA)
	MEMPTR = shiftl(bfwa,2)
        MEMORY(0) = CSMG(ITEMP,MEMORY(0),SHIFTR(MASK(N),BFBA))
        RETURN
      ENDIF

      ALBA = and(a0+n-1,MASK(64-5))
      BLBA = and(b0+n-1,MASK(64-5))
	MEMPTR = shiftl(bfwa,2)
      BFIRST= MEMORY(0)
	MEMPTR = shiftl(blwa,2)
      BLAST = MEMORY(0)

*     The difference in bit positions determines how we execute the
*     move, so calculate that and store it in DELTA.

      DELTA = AFBA - BFBA
*
*     Determine which way to run the loop.  Under most conditions,
*     the loop will run forward.  The only time the loop must be
*     run backwards is if BFWA>AFWA & BFWA<=ALWA.

      FORWARD = .NOT. (BFWA .GT. AFWA .AND. BFWA .LE. ALWA)
*
*     Check the way the bits line up
*
      IF (DELTA .EQ. 0)THEN
*
*       The words line up bit for bit
*
        IF (FORWARD) THEN
          I2 = AFWA
          DO 23004 I = BFWA,BLWA
 	    MEMPTR = shiftl(i2,2)
            ITEMP = MEMORY(0)
	    MEMPTR = shiftl(i,2)
            MEMORY(0) = ITEMP
            I2 = I2 + 1
23004     CONTINUE
        ELSE
          I2 = ALWA
          DO 23008 I = BLWA,BFWA,-1
 	    MEMPTR = shiftl(i2,2)
            ITEMP = MEMORY(0)
	    MEMPTR = shiftl(i,2)
            MEMORY(0) = ITEMP
            I2 = I2 - 1
23008     CONTINUE
        ENDIF
      ELSE IF (DELTA .GT. 0)THEN
*
*       The destination bit is to the right of the source bit
*
        IF (FORWARD) THEN
          I2 = AFWA
          DO 23010 I = BFWA,BLWA
	    MEMPTR=shiftl(i2,2)
            ITEMP   = OR(SHIFTL(MEMORY(0),    DELTA),
     *                   SHIFTR(MEMORY(1),BPW-DELTA))
	    MEMPTR=shiftl(i,2)
            MEMORY(0)=ITEMP
            I2 = I2 + 1
23010     CONTINUE
        ELSE
          I2 = AFWA+BLWA-BFWA
          DO 23012 I = BLWA,BFWA,-1
	    MEMPTR=shiftl(i2,2)
            ITEMP   = OR(SHIFTL(MEMORY(0),    DELTA),
     *                   SHIFTR(MEMORY(1),BPW-DELTA))
	    MEMPTR=shiftl(i,2)
            MEMORY(0)=ITEMP
            I2 = I2 - 1
23012     CONTINUE
        ENDIF
      ELSE
*
*       The destination bit is to the left of the source bit
*
        DELTA = -DELTA
        IF (FORWARD) THEN
          I2 = AFWA
          DO 23014 I = BFWA,BLWA
	    MEMPTR=shiftl((i2-1),2)
            ITEMP   = OR(SHIFTL(MEMORY(0),BPW-DELTA),
     *                   SHIFTR(MEMORY(1),    DELTA))
	    MEMPTR=shiftl(i,2)
            MEMORY(0)=ITEMP
            I2 = I2 + 1
23014     CONTINUE
        ELSE
          I2 = AFWA+BLWA-BFWA
          DO 23016 I = BLWA,BFWA,-1
	    MEMPTR=shiftl((i2-1),2)
            ITEMP   = OR(SHIFTL(MEMORY(0),BPW-DELTA),
     *                   SHIFTR(MEMORY(1),    DELTA))
	    MEMPTR=shiftl(i,2)
            MEMORY(0)=ITEMP
            I2 = I2 - 1
23016     CONTINUE
        ENDIF
      ENDIF
*

*     Now repair the first and last words
*
      MEMPTR=shiftl(blwa,2)
      MEMORY(0) = CSMG(MEMORY(0),BLAST,MASK(BLBA+1))
      MEMPTR=shiftl(bfwa,2)
      MEMORY(0) = CSMG(BFIRST,MEMORY(0),MASK(BFBA))
      RETURN
      END
