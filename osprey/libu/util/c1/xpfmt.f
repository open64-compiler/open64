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


      SUBROUTINE XPFMT(ADDRESS,IN,OUT,MODE)                             
      IMPLICIT INTEGER (A-Z)                                            
C                                                                       
C     +----------------------------------------------------------------+
C     |                                                                |
C     |                      X P F M T                                 |
C     |                                                                |
C     | PURPOSE:                                                       |
C     |   XPFMT transforms a binary image of an exchange package into  |
C     |   a printable integer array.  The printable result is 64       |
C     |   characters wide (including a leading 9 character address     |
C     |   field) by 24 lines.  (The first line contains a ruler and    |
C     |   is intended for debugging.)                                  |
C     |                                                                |
C     | ENTRY:                                                         |
C     |   The subroutine requires four parameters:                     |
C     |   ADDRESS:  The nominal address of the exchange package in CRAY|
C     |             memory, given as a binary number.  (If you are     |
C     |             not going to print the first 8 characters of each  |
C     |             line, this parameter need not have a meaningful    |
C     |             value.)                                            |
C     |                                                                |
C     |   IN:       A sixteen word integer array containing the        |
C     |             binary representation of the exchange package.     |
C     |                                                                |
C     |   OUT:      An integer array, nominally dimensioned (8,0:23)   |
C     |             into which the character representation of the     |
C     |             exchange package will be stored.  The input value  |
C     |             of the array is without significance.              |
C     |                                                                |
C     |   MODE:     An integer word indicating the mode in which the   |
C     |             exchange package is to be printed:                 |
C     |               ZERO - use internal evidence                     |
C     |               'X'L - print as an X-MP exchange package         |
C     |               'S'L - print as a 1-S exchange package           |
C     |                                                                |
C     | EXIT:                                                          |
C     |   Upon return to the user, OUT contains a printable            |
C     |   representation of the exchange package.  (The format of      |
C     |   this representation is shown in comment statements below.)   |
C     |                                                                |
C     |   Translation of non-printable characters is not done by this  |
C     |   routine.  It is the responsibility of the calling program to |
C     |   process these characters as required.                        |
C     |                                                                |
C     +----------------------------------------------------------------+
      INTEGER IN(0:15)                                                  
      INTEGER OUT(8,0:23)                                               
      INTEGER XPBLANK(8,0:23)                                           
      SAVE XPBLANK                                                      
      PARAMETER (NMODES=23)                                             
      INTEGER MODES(NMODES)                                             
      SAVE MODES                                                        
      INTEGER ETYPE(2,0:3)                                              
      SAVE ETYPE                                                        
      INTEGER SRMODE(0:3)                                               
      SAVE SRMODE                                                       
      INTEGER XRMODE(0:3)                                               
      SAVE XRMODE                                                       
      INTEGER BLANKS                                                    
      SAVE BLANKS                                                       
      DATA BLANKS/' '/                                                  
CDIR$ EJECT                                                             
C                                                                       
C     The following comments show the initialization of XPBLANK done    
C     by awkward-looking data statements following.                     
C                                                                       
C       |...+....1....+....2....+....3....+....4....+....5....+....6....
C       oooooooo P   ooooooooa       A0 oooooooo aaa  MODES     FLAGS   
C       oooooooo IBA oooooooo        A1 oooooooo aaa OFF  ON  OFF   ON  
C       oooooooo ILA oooooooo        A2 oooooooo aaa MM  MM   PCI  PCI  
C       oooooooo XA      oooo VL ooo A3 oooooooo aaa ICM ICM  MCU  MCU  
C       oooooooo DBA oooooooo        A4 oooooooo aaa IFP IFP  FPE  FPE  
C       oooooooo DLA oooooooo        A5 oooooooo aaa IUM IUM  ORE  ORE  
C       oooooooo                     A6 oooooooo aaa IMM IMM  PRE  PRE  
C       oooooooo                     A7 oooooooo aaa SEI SEI  ME   ME   
C                                                    BDM BDM  IOI  IOI  
C       oooooooo S0 ooooooooooooooooooooooo aaaaaaaa FPS FPS  EEX  EEX  
C       oooooooo S1 ooooooooooooooooooooooo aaaaaaaa WS  WS   NEX  NEX  
C       oooooooo S2 ooooooooooooooooooooooo aaaaaaaa IOR IOR  DL   DL   
C       oooooooo S3 ooooooooooooooooooooooo aaaaaaaa EMA EMA  ICP  ICP  
C       oooooooo S4 ooooooooooooooooooooooo aaaaaaaa SVL SVL            
C       oooooooo S5 ooooooooooooooooooooooo aaaaaaaa                    
C       oooooooo S6 ooooooooooooooooooooooo aaaaaaaa                    
C       oooooooo S7 ooooooooooooooooooooooo aaaaaaaa                    
C                                                                       
C                PROCESSOR  = o    CLUSTER = o PS = o                   
C                ERROR TYPE = aaaaaaaaaaaaaaaa VNU= o                   
C                CHIP SLCT  = oooooo BANK = oo                          
C                READ MODE  = aaaaaa SYNDROME = ooo                     
C                                                                       
      DATA ((XPBLANK(K1,K2),K1=1,5),K2=0,15)/                           
     $   '|...+...','.1....+.','...2....','+....3..','..+....4',        
     $   'oooooooo',' P   ooo','oooooa  ','     A0 ','oooooooo',        
     $   'oooooooo',' IBA ooo','ooooo   ','     A1 ','oooooooo',        
     $   'oooooooo',' ILA ooo','ooooo   ','     A2 ','oooooooo',        
     $   'oooooooo',' XA     ',' oooo VL',' ooo A3 ','oooooooo',        
     $   'oooooooo',' DBA ooo','ooooo   ','     A4 ','oooooooo',        
     $   'oooooooo',' DLA ooo','ooooo   ','     A5 ','oooooooo',        
     $   'oooooooo','        ','        ','     A6 ','oooooooo',        
     $   'oooooooo','        ','        ','     A7 ','oooooooo',        
     $   '        ','        ','        ','        ','        ',        
     $   'oooooooo',' S0 oooo','oooooooo','oooooooo','ooo aaaa',        
     $   'oooooooo',' S1 oooo','oooooooo','oooooooo','ooo aaaa',        
     $   'oooooooo',' S2 oooo','oooooooo','oooooooo','ooo aaaa',        
     $   'oooooooo',' S3 oooo','oooooooo','oooooooo','ooo aaaa',        
     $   'oooooooo',' S4 oooo','oooooooo','oooooooo','ooo aaaa',        
     $   'oooooooo',' S5 oooo','oooooooo','oooooooo','ooo aaaa'/        
C                                                                       
      DATA ((XPBLANK(K1,K2),K1=1,5),K2=16,23)/                          
     $   'oooooooo',' S6 oooo','oooooooo','oooooooo','ooo aaaa',        
     $   'oooooooo',' S7 oooo','oooooooo','oooooooo','ooo aaaa',        
     $   '        ','        ','        ','        ','        ',        
     $   '        ',' PROCESS','OR  = o ','   CLUST','ER = o P',        
     $   '        ',' ERROR T','YPE = aa','aaaaaaaa','aaaaaa V',        
     $   '        ',' CHIP SL','CT  = oo','oooo BAN','K = oo  ',        
     $   '        ',' READ MO','DE  = aa','aaaa SYN','DROME = ',        
     $   '        ','        ','        ','        ','        '/        
                                                                        
                                                                        
      DATA ((XPBLANK(K1,K2),K1=6,8),K2=0,15)/                           
     $   '....+...','.5....+.','...6....',                              
     $   ' aaa  MO','DES     ','FLAGS   ',                              
     $   ' aaa OFF','  ON  OF','F   ON  ',                              
     $   ' aaa MM ',' MM   PC','I  PCI  ',                              
     $   ' aaa ICM',' ICM  MC','U  MCU  ',                              
     $   ' aaa IFP',' IFP  FP','E  FPE  ',                              
     $   ' aaa IUM',' IUM  OR','E  ORE  ',                              
     $   ' aaa IMM',' IMM  PR','E  PRE  ',                              
     $   ' aaa SEI',' SEI  ME','   ME   ',                              
     $   '     BDM',' BDM  IO','I  IOI  ',                              
     $   'aaaa FPS',' FPS  EE','X  EEX  ',                              
     $   'aaaa WS ',' WS   NE','X  NEX  ',                              
     $   'aaaa IOR',' IOR  DL','   DL   ',                              
     $   'aaaa EMA',' EMA  IC','P  ICP  ',                              
     $   'aaaa SVL',' SVL    ','        ',                              
     $   'aaaa    ','        ','        '/                              
C                                                                       
      DATA ((XPBLANK(K1,K2),K1=6,8),K2=16,23)/                          
     $   'aaaa    ','        ','        ',                              
     $   'aaaa    ','        ','        ',                              
     $   '        ','        ','        ',                              
     $   'S = o   ','        ','        ',                              
     $   'NU= o   ','        ','        ',                              
     $   '        ','        ','        ',                              
     $   'ooo     ','        ','        ',                              
     $   '        ','        ','        '/                              
                                                                        
C                                                                       
C      MODES                                                            
C                                                                       
C      The array controlling mode bits is packed:                       
C                                                                       
C      Bits   Meaning                                                   
C      0-3    0=Skip the entry                                          
C             1=CRAY-1S                                                 
C             2=CRAY X-MP                                               
C             3=CRAY-1S and CRAY X-MP                                   
C                                                                       
C      4-18   Output word                                               
C     19-33   Output character position                                 
C     34-48   Input word                                                
C     49-63   Input bit position                                        
C                                                                       
      DATA MODES( 1)/ 03 00003 00056 00002 00047B/                      
      DATA MODES( 2)/ 03 00004 00056 00002 00044B/                      
      DATA MODES( 3)/ 03 00005 00056 00002 00045B/                      
      DATA MODES( 4)/ 03 00006 00056 00002 00046B/                      
      DATA MODES( 5)/ 03 00007 00056 00001 00047B/                      
      DATA MODES( 6)/ 02 00010 00056 00001 00046B/                      
      DATA MODES( 7)/ 02 00011 00056 00001 00045B/                      
      DATA MODES( 8)/ 02 00012 00056 00001 00044B/                      
      DATA MODES( 9)/ 02 00013 00056 00001 00043B/                      
      DATA MODES(10)/ 02 00014 00056 00002 00043B/                      
      DATA MODES(11)/ 03 00003 00067 00003 00037B/                      
      DATA MODES(12)/ 03 00004 00067 00003 00040B/                      
      DATA MODES(13)/ 03 00005 00067 00003 00041B/                      
      DATA MODES(14)/ 03 00006 00067 00003 00042B/                      
      DATA MODES(15)/ 03 00007 00067 00003 00043B/                      
      DATA MODES(16)/ 03 00010 00067 00003 00044B/                      
      DATA MODES(17)/ 03 00011 00067 00003 00045B/                      
      DATA MODES(18)/ 03 00012 00067 00003 00046B/                      
      DATA MODES(19)/ 03 00013 00067 00003 00047B/                      
      DATA MODES(20)/ 02 00014 00067 00003 00017B/                      
      DATA MODES(21)/ 02 00015 00067 00003 00016B/                      
      DATA MODES(22)/ 02 00015 00056 00004 00000B/                      
      DATA MODES(23)/ 02 00016 00056 00003 00000B/                      
                                                                        
      DATA ETYPE/'NONE    ','        ',                                 
     $           'CORRECTA','BLE     ',                                 
     $           'UNCORREC','TABLE   ',                                 
     $           'INVALID ','VALUE-11'/                                 
C                                                                       
      DATA SRMODE/'SCALAR  ','I/O     ','VECTOR  ','FETCH   '/          
      DATA XRMODE/'I/O     ','SCALAR  ','VECTOR  ','FETCH   '/          
CDIR$ EJECT                                                             
C                                                                       
C     Stetement function definitions                                    
C                                                                       
      INTEGER GET, PUT                                                  
      GET(W,SS,N) = AND(MASK(128-N),SHIFTR(W,64-SS-N))
      PUT(W,SS,N,V) = CSMG(SHIFTL(V,64-SS-N),W,SHIFTR(MASK(N),SS))
CDIR$ EJECT                                                             
      IF (MODE .EQ. 0) THEN                                             
        IF (GET(IN(4),16,19) .EQ. 0 .AND. GET(IN(5),16,19) .EQ. 0) THEN 
          MACHINE = 'S'L                                                
        ELSE                                                            
          MACHINE = 'X'L                                                
        ENDIF                                                           
      ELSE                                                              
        MACHINE = MODE                                                  
      ENDIF                                                             
      DO 80 J = 0,23                                                    
      DO 80 I = 1,8                                                     
        OUT(I,J) = XPBLANK(I,J)                                         
   80 CONTINUE                                                          
C                                                                       
C     We will fill in the easy stuff first: octal address, A-registers, 
C     S-registers.                                                      
C                                                                       
C     Fill in the octal address                                         
C                                                                       
      DO 100 I = 1,8                                                    
        CALL B2OCT(OUT(1,I),1,8,ADDRESS+I-1,24)                         
  100 CONTINUE                                                          
      DO 120 I = 10,17                                                  
        CALL B2OCT(OUT(1,I),1,8,ADDRESS+I-2,24)                         
  120 CONTINUE                                                          
C                                                                       
C     Fill in the A-registers                                           
C                                                                       
      DO 140 I = 1,8                                                    
        CALL B2OCT(OUT(1,I),33,8,AND(IN(I-1),MASK(128-24)),24)          
        CALL MVC(IN(I-1),6,OUT(1,I),42,3)                               
  140 CONTINUE                                                          
C                                                                       
C     Fill in the S-registers                                           
C                                                                       
      DO 160 I = 8,15                                                   
        CALL B2OCT(OUT(1,I+2),13,23,IN(I),64)                           
        CALL MVC(IN(I),1,OUT(1,I+2),37,8)                               
  160 CONTINUE                                                          
C                                                                       
C     Fill in the stuff common to both breeds of machine                
C                                                                       
C     The P-register                                                    
C                                                                       
C     What about the PARCEAL ADDRESS.                                   
C     ALSO get bits 38,39 and decode it to A B C or D                   
C                                                                       
      CALL B2OCT(OUT(1,1),14,8,GET(IN(0),16,22),22)                     
      CALL PUTBYT(OUT(1,1),22,GET(IN(0),38,2)+141B)                     
C                                                                       
C     The 1S base/limit or the X-MP instruction base/limit              
C                                                                       
      IF (MACHINE .EQ. 'X'L) THEN                                       
        IBITS = 16                                                      
        IBITL = 19                                                      
        ISHFL =  5                                                      
      ELSE                                                              
        IBITS = 18                                                      
        IBITL = 18                                                      
        ISHFL =  4                                                      
      ENDIF                                                             
      CALL B2OCT(OUT(1,2),14,8,SHIFTL(GET(IN(1),IBITS,IBITL),ISHFL),24) 
      CALL B2OCT(OUT(1,3),14,8,SHIFTL(GET(IN(2),IBITS,IBITL),ISHFL),24) 
C                                                                       
C     The exchange address and vector length                            
C                                                                       
      CALL B2OCT(OUT(1,4),18,4,SHIFTL(GET(IN(3),16,8),4),12)            
      CALL B2OCT(OUT(1,4),26,3,GET(IN(3),24,7),7)                       
C                                                                       
C     Fill in the modes and flag bits                                   
C                                                                       
      DO 200 I = 1,NMODES                                               
        TEMP = MODES(I)                                                 
        MT   = GET(TEMP,0,4)                                            
        OW   = GET(TEMP,4,15)                                           
        OC   = GET(TEMP,19,15)                                          
        IW   = GET(TEMP,34,15)                                          
        IB   = GET(TEMP,49,15)                                          
C                                                                       
C     Blank inapplicable fields                                         
C                                                                       
        IF ((MT .EQ. 0) .OR.                                            
     $      (MT .EQ. 1 .AND. MACHINE .EQ. 'X'L) .OR.                    
     $      (MT .EQ. 2 .AND. MACHINE .EQ. 'S'L)) THEN                   
          CALL MVC(BLANKS,1,OUT(1,OW),OC,8)                             
        ELSE                                                            
          IF (GET(IN(IW),IB,1) .EQ. 0) THEN                             
            CALL MVC(BLANKS,1,OUT(1,OW),OC+4,4)                         
          ELSE                                                          
            CALL MVC(BLANKS,1,OUT(1,OW),OC,4)                           
          ENDIF                                                         
        ENDIF                                                           
  200 CONTINUE                                                          
C                                                                       
C     Fill in the rest of the exchange package depending on machine     
C     type.                                                             
C                                                                       
      IF (MACHINE .EQ. 'X'L) THEN                                       
C                                                                       
C     Data base/limit addresses                                         
C                                                                       
        CALL B2OCT(OUT(1,5),14,8,SHIFTL(GET(IN(4),16,19),5),24)         
        CALL B2OCT(OUT(1,6),14,8,SHIFTL(GET(IN(5),16,19),5),24)         
C                                                                       
C     Processor                                                         
C                                                                       
        CALL B2OCT(OUT(1,19),23,1,GET(IN(0),0,2),2)                     
C                                                                       
C     Cluster                                                           
C                                                                       
        CALL B2OCT(OUT(1,19),38,1,GET(IN(4),37,3),3)                    
C                                                                       
C           Program state                                               
C                                                                       
        CALL B2OCT(OUT(1,19),45,1,GET(IN(4),35,1),1)                    
C                                                                       
C     Error type                                                        
C                                                                       
        CALL MVC(ETYPE(1,GET(IN(0),2,2)),1,OUT(1,20),23,16)             
C                                                                       
C     Vectors not used                                                  
C                                                                       
        CALL B2OCT(OUT(1,20),45,1,GET(IN(2),0,1),1)                     
C                                                                       
C      Chip select                                                      
C                                                                       
        CALL B2OCT(OUT(1,21),23,6,GET(IN(1),2,5),5)                     
C                                                                       
C     Bank                                                              
C                                                                       
        CALL B2OCT(OUT(1,21),37,2,GET(IN(1),7,5),5)                     
C                                                                       
C     Read mode                                                         
C                                                                       
        CALL MVC(XRMODE(GET(IN(1),0,2)),1,OUT(1,22),23,6)               
C                                                                       
C     Syndrome                                                          
C                                                                       
        CALL B2OCT(OUT(1,22),41,3,GET(IN(0),4,8),8)                     
      ELSE                                                              
C                                                                       
C     For the 1S, blank out the DBA, DLA fields and change IBA          
C     and ILA to BA and LA                                              
C                                                                       
        CALL MVC('BA',1,OUT(1,2),10,3)                                  
        CALL MVC('LA',1,OUT(1,3),10,3)                                  
        CALL MVC(BLANKS,1,OUT(1,5),10,8)                                
        CALL MVC(BLANKS,1,OUT(1,6),10,8)                                
        CALL MVC(BLANKS,1,OUT(1,5),18,8)                                
        CALL MVC(BLANKS,1,OUT(1,6),18,8)                                
C                                                                       
C     Blank out the multi-processor stuff                               
C                                                                       
        CALL PUTBYT(OUT(1,19),1,' 'R)                                   
        CALL MVC (OUT(1,19),1,OUT(1,19),2,63)                           
C                                                                       
C     Blank out VU                                                      
C                                                                       
        CALL MVC(BLANKS,1,OUT(1,20),40,6)                               
C                                                                       
C     Read mode                                                         
C                                                                       
        CALL MVC(SRMODE(GET(IN(0),10,2)),1,OUT(1,22),23,6)              
C                                                                       
C     Error type                                                        
C                                                                       
        CALL MVC(ETYPE(1,GET(IN(0),00,2)),1,OUT(1,20),23,16)            
C                                                                       
C     Syndrome                                                          
C                                                                       
        CALL B2OCT(OUT(1,22),41,3,GET(IN(0),2,8),8)                     
C                                                                       
C     Blank out line 21                                                 
C                                                                       
        CALL PUTBYT(OUT(1,21),1,' 'R)                                   
        CALL MVC (OUT(1,21),1,OUT(1,21),2,63)                           
C                                                                       
C     Insert text                                                       
C                                                                       
        CALL MVC ('ERROR AD',1,OUT(1,21),10,8)                          
        CALL MVC ('DRESS = ',1,OUT(1,21),18,8)                          
        CALL MVC ('        ',1,OUT(1,21),26,2)                          
        TEMP = 0                                                        
        TEMP = PUT(TEMP,42, 2,GET(IN(2),14, 2))                         
        TEMP = PUT(TEMP,44,16,GET(IN(1), 0,16))                         
        TEMP = PUT(TEMP,60, 4,GET(IN(0),12, 4))                         
        CALL B2OCT(OUT(1,21),28,8,TEMP,24)                              
      ENDIF                                                             
      RETURN                                                            
      END                                                               
      SUBROUTINE B2OCT(S,J,K,V,N)                                       
      IMPLICIT INTEGER (A-Z)                                            
      INTEGER S(8)                                                      
C                                                                       
C     +----------------------------------------------------------------+
C     |                                                                |
C     |                         B 2 O C T                              |
C     |                                                                |
C     | PURPOSE:                                                       |
C     |   B2OCT places the low order N bits of the full CRAY word V    |
C     |   as octal characters into the low-order positions of the      |
C     |   string defined by S, J, and K.  The string will be padded    |
C     |   with blanks on the left.                                     |
C     |                                                                |
C     |                                                                |
C     |                                                                |
C     +----------------------------------------------------------------+
C                                                                       
C     Stetement function definitions                                    
C                                                                       
      INTEGER GET, PUT                                                  
      GET(W,SS,N) = AND(MASK(128-N),SHIFTR(W,64-SS-N))
      PUT(W,SS,N,V) = CSMG(SHIFTL(V,64-SS-N),W,SHIFTR(MASK(N),SS))
C                                                                       
C     Clear the destination string to blanks                            
C                                                                       
      CALL PUTBYT(S,J,' 'R)                                             
      CALL MVC(S,J,S,J+1,K-1)                                           
C                                                                       
C     At the high-order end, there may be a partial octal character.    
C     How many characters are required?                                 
C                                                                       
      NCH = ICEIL(N,3)                                                  
C                                                                       
C     If there are not enough characters in the substring, then         
C                                                                       
      IF (NCH .GT. K) THEN                                              
C                                                                       
C     Fill the substring with asterisks to show an error                
C                                                                       
        CALL PUTBYT(S,J,'*'R)                                           
        CALL MVC(S,J,S,J+1,K-1)                                         
      ELSE                                                              
C                                                                       
C     Process the high-order octal digit                                
C                                                                       
        JN = J+K-NCH                                                    
        BN = 64-N                                                       
        BL = MOD(N,3)                                                   
        IF (BL .EQ. 0) BL = 3                                           
        CALL PUTBYT(S,JN,GET(V,BN,BL)+'0'R)                             
  100   CONTINUE                                                        
C                                                                       
C     Check for loop termination                                        
C                                                                       
        JN = JN + 1                                                     
        BN = BN + BL                                                    
        BL = 3                                                          
        IF (BN+BL .GT. 64) GO TO 200                                    
          CALL PUTBYT(S,JN,GET(V,BN,BL)+'0'R)                           
          GO TO 100                                                     
  200   CONTINUE                                                        
      ENDIF                                                             
      RETURN                                                            
CDIR$ ID "@(#) libu/util/c1/xpfmt.f	92.0	10/08/98 14:57:41"
      END                                                               
