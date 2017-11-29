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


      SUBROUTINE MVC(S1,J1,S2,J2,K)                                     
C                                                                       
C                              M V C                                    
C                                                                       
C     PURPOSE                                                           
C                                                                       
C     MVC moves characters from a source byte address to a              
C     destination byte address.                                         
C                                                                       
C     ENTRY                                                             
C                                                                       
C     S1 - Word address of the source string.                           
C     J1 - Byte offset of the first byte of the source string.          
C          (The high-order byte of the word specified by 'S1' is        
C          numbered byte 1.)                                            
C     S2 - Word address of the destination string                       
C     J2 - Byte offset of the first byte of the destination string.     
C     K  - Number of bytes to move.                                     
C                                                                       
C     EXIT                                                              
C                                                                       
C     The source string will be copied to the destination string.       
C                                                                       
C     METHOD                                                            
C                                                                       
C     The source string is copied to the destination string one byte    
C     at a time, from high-order byte to low-order byte.                
                                                                        
      INTEGER S1(1),J1,S2(1),J2,K                                       
      DO 100 I = 1,K                                                    
      I1 = J1 + I - 1                                                   
      I2 = J2 + I - 1                                                   
      CALL PUTBYT(S2,I2,IGTBYT(S1,I1))                                  
  100 CONTINUE                                                          
      RETURN                                                            
CDIR$ ID "@(#) libu/util/c1/mvc.f	92.0	10/08/98 14:57:41"
      END                                                               
