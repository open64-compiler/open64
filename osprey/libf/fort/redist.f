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



C
C     Functions which support implicit argument redistribution.
C     The functions are called by CRAFT CF77-generated code.
C     There are 14 functions to cover 1-7 dimensions and element sizes
C     of 1 or 2 words.


      subroutine REDIST_1_1W@( src, dst, e1, b1 )

CDIR$ ID "@(#) libf/fort/redist.f	92.0	10/08/98 14:30:10"

      implicit integer(a-z)
      real*8 src( e1 )
      real*8 dst( e1 )
cdir$ unknown_shared src, dst

      dst( 1 : b1 ) = src( 1 : b1 )

      end


      subroutine REDIST_1_2W@( src, dst, e1, b1 )

      implicit integer(a-z)
      complex*16 src( e1 )
      complex*16 dst( e1 )
cdir$ unknown_shared src, dst

      dst( 1 : b1 ) = src( 1 : b1 )

      end


      subroutine REDIST_2_1W@( src, dst, e1, b1, e2, b2 )

      implicit integer(a-z)
      real*8 src( e1, e2 )
      real*8 dst( e1, e2 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2 ) = src( 1 : b1, 1 : b2 )

      end


      subroutine REDIST_2_2W@( src, dst, e1, b1, e2, b2 )

      implicit integer(a-z)
      complex*16 src( e1, e2 )
      complex*16 dst( e1, e2 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2 ) = src( 1 : b1, 1 : b2 )

      end


      subroutine REDIST_3_1W@( src, dst, e1, b1, e2, b2, e3, b3 )

      implicit integer(a-z)
      real*8 src( e1, e2, e3 )
      real*8 dst( e1, e2, e3 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3 ) = src( 1 : b1, 1 : b2, 1 : b3 )

      end


      subroutine REDIST_3_2W@( src, dst, e1, b1, e2, b2, e3, b3 )

      implicit integer(a-z)
      complex*16 src( e1, e2, e3 )
      complex*16 dst( e1, e2, e3 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3 ) = src( 1 : b1, 1 : b2, 1 : b3 )

      end


      subroutine REDIST_4_1W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4 )

      implicit integer(a-z)
      real*8 src( e1, e2, e3, e4 )
      real*8 dst( e1, e2, e3, e4 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4 )

      end


      subroutine REDIST_4_2W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4 )

      implicit integer(a-z)
      complex*16 src( e1, e2, e3, e4 )
      complex*16 dst( e1, e2, e3, e4 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4 )

      end


      subroutine REDIST_5_1W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4, e5, b5 )

      implicit integer(a-z)
      real*8 src( e1, e2, e3, e4, e5 )
      real*8 dst( e1, e2, e3, e4, e5 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5 )

      end


      subroutine REDIST_5_2W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4, e5, b5 )

      implicit integer(a-z)
      complex*16 src( e1, e2, e3, e4, e5 )
      complex*16 dst( e1, e2, e3, e4, e5 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5 )

      end


      subroutine REDIST_6_1W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4, e5, b5, e6, b6 )

      implicit integer(a-z)
      real*8 src( e1, e2, e3, e4, e5, e6 )
      real*8 dst( e1, e2, e3, e4, e5, e6 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6 )

      end


      subroutine REDIST_6_2W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4, e5, b5, e6, b6 )

      implicit integer(a-z)
      complex*16 src( e1, e2, e3, e4, e5, e6 )
      complex*16 dst( e1, e2, e3, e4, e5, e6 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6 )

      end


      subroutine REDIST_7_1W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4, e5, b5, e6, b6, e7, b7 )

      implicit integer(a-z)
      real*8 src( e1, e2, e3, e4, e5, e6, e7 )
      real*8 dst( e1, e2, e3, e4, e5, e6, e7 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6, 1 : b7 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6, 1 : b7 )

      end


      subroutine REDIST_7_2W@( src, dst, e1, b1, e2, b2, e3, b3,
     +           e4, b4, e5, b5, e6, b6, e7, b7 )

      implicit integer(a-z)
      complex*16 src( e1, e2, e3, e4, e5, e6, e7 )
      complex*16 dst( e1, e2, e3, e4, e5, e6, e7 )
cdir$ unknown_shared src, dst

      dst( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6, 1 : b7 ) =
     +  src( 1 : b1, 1 : b2, 1 : b3, 1 : b4, 1 : b5, 1 : b6, 1 : b7 )

      end
