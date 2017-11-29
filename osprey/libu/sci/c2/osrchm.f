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


      SUBROUTINE OSRCHM( N,IARRAY,INC,ITARGET,
     &                   MASK,SHIFT,INDEX,IWHERE,
     &                   INUM )
************************************************************************
**                                                                    **
*         Search an ordered integer array for integer target           *
**                                                                    **
*                                                                      *
*  OSRCHM (N,IARRAY,INC,ITARGET,MASK,SHIFT,INDEX,IWHERE,INUM) is a     *
*    subroutine that returns the index of the first location that      *
*    is equal to the target.  It also returns the index of where       *
*    the target should fit into the array whether it finds a value     *
*    equal to the target or not.  Optionally it will find the          *
*    total number of array elements equal to the target.               *
*                                                                      *
*                                                                      *
*   Inputs:                                                            *
*                                                                      *
*    N        Number of elements of the array to be searched.          *
*    IARRAY   The beginning address of the integer array               *
*              to be searched.                                         *
*    INC      The skip increment distance between elements to be       *
*              searched.  INC should be 1 for contiguous elements      *
*              of memory.  INC should be -1 to find the last           *
*              element with a true condition.                          *
*              A positive skip increment indicates an ascending        *
*              array, while a negative skip increment indicates        *
*              a descending array.                                     *
*                                                                      *
*    ITARGET  Integer target of the search.                            *
*                                                                      *
*    MASK     A mask that is set from the right the side of the        *
*              field of interest in the vector IARRAY                  *
*                                                                      *
*    SHIFT    How much to shift right the IARRAY vector to position    *
*              the field of interest at the right side of the word     *
*                                                                      *
*                                                                      *
*    INUM     If the number of elements of the array equal to the      *
*              target is desired this parameter must be non-zero.      *
*                                                                      *
*   Output:                                                            *
*                                                                      *
*    INDEX    Is the index of the first location in the searched       *
*              array where the target is equal to an element           *
*              of that array.                                          *
*                                                                      *
*             Exceptional cases:                                       *
*              1)  if N is less than 1 then INDEX = 0                  *
*              2)  if no equal array elements then INDEX = N+1         *
*                                                                      *
*    IWHERE   Is the index of the first location in the searched       *
*              array where the target would fit and keep the           *
*              order of the array.  If the target is found             *
*              INDEX = IWHERE.                                         *
*                                                                      *
*             Exceptional case:                                        *
*              1)  if N is less than 1 then INDEX = 0                  *
*                                                                      *
*    INUM     Is the number of elements of the array equal to the      *
*              target.  This will return a value only if asked         *
*              for and at least one target value is found in the       *
*              array.  Otherwise it will always be zero.               *
*                                                                      *
**                                                                    **
************************************************************************
      dimension iarray(n)
      integer shift
*
*     -------------
*     USM What Line
*     -------------
CDIR$ ID "@(#) libu/sci/c2/osrchm.f	92.0	10/08/98 14:57:41"
*
      if (n .lt. 0) go to 500
      j = 1
      if (inc .lt. 0) j = n * (-inc)
      do 100 i= 1, n
      if (and(mask,shiftr(iarray(j),shift)).gt.itarget) go to 200
      if (and(mask,shiftr(iarray(j),shift)).eq.itarget) go to 300
      j = j + inc
  100 continue
      index = n + 1
      iwhere = n + 1
      inum = 0
      return
  200 continue
      index = n + 1
      iwhere = i
      inum = 0
      return
  300 continue
      index = i
      iwhere = i
      if (inum .eq. 0) return
      inum = 1
      do 400 i = i+1, n
      j = j + inc
      if (and(mask,shiftr(iarray(j),shift)).ne.itarget) return
      inum=inum+1
  400 continue
      return
  500 continue
      index = 0
      iwhere = 0
      return
      end
