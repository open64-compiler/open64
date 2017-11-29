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


      subroutine orders (mode,iwork,data,index,n,ireclth,
     1                   ikeylth,iradsiz)
c
c     orders is an internal, fixed-length record sort routine
c
c
c     entry:
c
c     mode     indicates type of sort to be performed:
c
c           0, keys are treated as positive (unsigned) integers
c              over the range [0..(2**64)-1].
c           1, keys are treated as  two's  complement  (signed)
c              integers over the range [-2**63..(2**63)-1].  in
c              fortran:  type integer.
c           2, keys are treated as floating-point numbers.   in
c              fortran:  type real.
c          10, same as mode = 0, but the array  index  has  the
c              initial ordering of the records.
c          11, same as mode = 1, but the array  index  has  the
c              initial ordering of the records.
c          12, same as mode = 2, but the array  index  has  the
c              initial ordering of the records.
c
c     iwork     user-supplied working storage array.   iwork  should
c          be  dimensioned  to  2**(8*iradsiz)  (256 or 65,536)
c          words).  in fortran:
c
c                dimension iwork (256)             if iradsiz=1
c                dimension iwork (65536)           if iradsiz=2
c
c     data      array containing n records of length  ireclth  each.
c          in fortran:
c
c                dimension data (ireclth, n)       - or -
c                dimension data (n)                if ireclth=1
c
c     index     if mode is greater than or equal to 10,  then  index
c          contains  the initial ordering of the records in the
c          array data.  thus, data(i,index(1)) is the ith  word
c          of  the first record and data(i,index(n)) is the ith
c          word of the last record.  each index(i) should be in
c          the  closed  interval [1..n] and unique with respect
c          to all other index(i).  in fortran:
c
c                dimension index (n)
c
c     n         number of records to be sorted.
c
c     ireclth   length of each record in words.  the default is one.
c          the key begins in the first word of each record.
c
c     ikeylth   length of each key in eight-bit bytes.  the  default
c          is eight bytes (one word).
c
c     iradsiz   radix of the sort (number of eight-bit  bytes  which
c          are  to  be  processed  per pass).  the default (and
c          and minimum) is one byte; the maximum is two  bytes.
c          if  the radix is two and the key length is odd, then
c          the first pass is done with a radix of one.
c
c     exit:
c
c     mode      less than zero if error, else unchanged.
c
c     index     sorted, such that for i := 1 to n-1:
c
c          data (key,index (i))) is less than or equal to
c          data (key,index (i+1)))
c
c          the radix pocket sort is stable (i.e., the order  of
c          records with identical keys is preserved).
c
c     error codes:
c
c          -1 = too few arguments (less than five)
c          -2 = too many arguments (greater than eight)
c          -3 = invalid ireclth (less than one)
c          -4 = ikeylth too large or greater than ireclth
c          -5 = invalid iradsiz (not one or two)
c          -6 = invalid ikeylth (less than one)
c          -7 = invalid n (less than one)
c          -8 = invalid mode (not in [0..2,10..12])
c          -9 = mismatched mode  and  ikeylth  (must  be  eight  for
c          floating-point or signed integer sort)
c         -10 = iradsiz greater than ikeylth
c
c     steps:
c
c            1) validate arguments.
c
c            2) if no initial ordering is supplied, then  initialize
c          index(i) to i.
c
c            3) signed integer and floating point  keys  are  trans-
c          formed  by  a  one-to-one  order-preserving  mapping
c          which allows them to be treated as unsigned  integer
c          keys.  this step, if done, is undone in step 5.
c
c            4) for each radix pass (ikeylth div iradsiz) do steps a
c          through  e  (note that keys are processed from least
c          significant radix to most significant radix):
c
c          a) clear iwork array by setting iwork (i) to 0.
c
c          b) for each key, data (i), count the  occurances  of
c             each  radix  (an  8-  or 16-bit slice of the key)
c             into iwork.  upon completion of this  step,  each
c             iwork  (i)  contains  the  number  of keys with a
c             radix of i.
c
c          c) cascade iwork array by setting  each  element  to
c             the sum of all previous elements.  when completed
c             with this step, iwork (i) contains  the  relative
c             position for the first key of radix i.
c
c          d) for each key, data (index  (i)),  compute  a  new
c             index by incrementing iwork (i) and using that as
c             offset into the index array.  that index  element
c             gets  the  new pointer to data (index (i)) in the
c             upper 32 bits.  upon  completion  of  this  step,
c             each index element contains two pointers: the old
c             pointer in the lower 32 bits and the new  pointer
c             in the upper 32 bits.
c
c          e) set new indices by shifting index (i) right by 32
c             bit positions.
c
c            5) transform keys back  to  their  original  values  if
c          necessary.
c
c     each pass of this algorithm executes in order (n) time,  where
c     n is number of elements to be sorted.  the number of passes is
c     the key length divided by the radix size (ikeylth div iradsiz)
c     so  the  overall work function is linear with respect to n, or
c     in other words, order (n).
c
c     radix sorting is described in "the art of computer programming
c     volume 3:  sorting and searching" by donald e. knuth. pp. 170-
c     178.
c
cmrd  Modified by
cmrd
cmrd       Max Dechantsreiter
cmrd       Scientific Libraries Group
cmrd       Cray Research, Inc.
cmrd
cmrd  so that ORDERS does not assign default parameter values to dummy
cmrd  arguments that are not associated with actual arguments.
cmrd
cmrd  This version dated December 18, 1988.
cmrd
c
c\Revision history:
c     10-31-89:  Modified to allow sorts of up to 2**32
c                elements.  In response to SPR #33746.
c                The following code is changed from -
c
c                if ((n.lt.1).or.(n.gt.2**24)) then
c
c                to -
c
c                if ((n.lt.1).or.(n.gt.2**32)) then
c
c                (Sandra Carney)
c
c     10-31-89:  The variable ikeylth is used incorrectly
c                in ORDERS.  The following code is changed
c                from -
c
c                if (ireclth*64.lt.8*ikeylth) then
c
c                to -
c
c                if (ireclth*64.lt.8*ikeylthl) then
c
c                This fixes SPR #34551.  (Sandra Carney)
c
c
c     10-31-89:  Modified so that ORDERS checks for invalid
c                ireclth (less than one or too large) before
c                it checks for ikeylth too large or greater
c                than ireclth.  It therefore passes testcase
c                given in SPR #8672.  (Sandra Carney)
c
      implicit integer (a-z)
      logical switch
      dimension data(1), index(1), iwork (1)
      integer ireclth , ikeylth, iradsiz
*
*     -------------
*     USM What Line
*     -------------
CDIR$ ID "@(#) libu/sci/c2/orders.f	92.0	10/08/98 14:57:41"
c
c     step 1 validate parameters
c
      iarg = numarg()
c
c     if the number of arguments <= 4 mode is returned with value -1
c     and no processing is done
c
      if (iarg.le.4) then
         mode = -1
         return
      endif
c
c     if the number of arguments >= 9 mode is returned with value -2
c     and no processing is done
c
      if (iarg.ge.9) then
         mode = -2
         return
      endif
      if (mode.eq.0) go to 1
      if (mode.eq.1) go to 1
      if (mode.eq.2) go to 1
      if (mode.eq.10) go to 1
      if (mode.eq.11) go to 1
      if (mode.eq.12) go to 1
      mode = -8
      return
 1    continue
cslc  if ((n.lt.1).or.(n.gt.2**24)) then
cslc  if ((n.lt.1).or.(n.gt.2**32)) then
      if (n.lt.1) then
         mode = -7
         return
      endif
      if (iarg.lt.8) then
cmrd     iradsiz = 1
         iradsizl = 1                                                   mrd
      else
         if ((iradsiz.ne.1).and.(iradsiz.ne.2)) then
            mode = -5
            return
         else                                                           mrd
            iradsizl = iradsiz                                          mrd
         endif
      endif
      if (iarg.lt.7) then
cmrd     ikeylth = 8
         ikeylthl = 8                                                   mrd
      else
         if (ikeylth.lt.1) then
            mode = -6
            return
cmrd     endif
cmrd     if ((mode.ne.0).and.(mode.ne.10).and.(ikeylth.ne.8)) then
         else if ((mode.ne.0).and.(mode.ne.10).and.(ikeylth.ne.8)) then mrd
            mode = -9
            return
         else                                                           mrd
            ikeylthl = ikeylth                                          mrd
         endif
      endif
      if (iarg.eq.5) then
cmrd     ireclth = 1
         ireclthl = 1                                                   mrd
      else
cslc     if (ireclth*64.lt.8*ikeylth) then
cslc        mode = -4
cslc        return
cmrd     endif
cmrd     if ((ireclth.lt.1).or.(ireclth.gt.2**24)) then
cslc     else if ((ireclth.lt.1).or.(ireclth.gt.2**24)) then            mrd
cslc     if ((ireclth.lt.1).or.(ireclth.gt.2**24)) then
         if (ireclth.lt.1) then
            mode = -3                                                   !slc
            return                                                      !slc
         elseif (ireclth*64.lt.8*ikeylthl) then                         !slc
           mode = -4                                                    !slc
           return                                                       !slc
         else                                                           mrd
            ireclthl = ireclth                                          mrd
         endif
      endif
cmrd  ireclthl = ireclth
cmrd  ikeylthl = ikeylth
cmrd  iradsizl = iradsiz
      call ordker (mode,iwork,data,index,n,ireclthl,ikeylthl,iradsizl)
      return
      end

      subroutine ordker (mode,iwork,data,index,n,ireclth,
     1                   ikeylth,iradsiz)
c
      implicit integer (a-z)
      logical switch
      dimension data(ireclth,n), index(n), iwork (0:2**(iradsiz*8)-1)
c     when cft77 version 2 becomes available the local common should
c     be used instead of the main memeory
c     local common /tmploc/ iwrkloc1(64) ,indxloc1(64)
      integer iwrkloc1(64),indxloc1(64)
c
c     array tmpiwork is used in call to irecps which vectorizes
c     the first order recurrence to cascade sums
c
      dimension tmpiwork(0:2**(16)-1)
c
c     step 2 preset index if necessary
c
      switch = .false.
      n1 = mod(n,64)
      n2 = n/64
      if (mode.lt.10) then
         do 10 i = 1 , n
            index(i)=i
  10  continue
      endif
c
c     step 3 map keys if necessary
c
      if (mod(mode,10).ne.0) then
         if (mod(mode,10).eq.1) then
               maskt = mask(1)
            do 11 i = 1 , n
               data(1,i) = xor(data(1,i),maskt)
  11        continue
         else
            do 12 i = 1 , n
               data(1,i) = xor(data(1,i),
     +                    cvmgm(mask(64),mask(1),data(1,i)))
   12       continue
         endif
      endif
c
c     initialize some variables
c
      cnt = ikeylth/iradsiz
      wrd = ((cnt-1)/(8/iradsiz))+1
      shf = (8-mod(ikeylth,8))*8
      rad = iradsiz
      if (shf.eq.64) shf = 0
      if ((mod(ikeylth,2).eq.1).and.(iradsiz.eq.2)) then
         switch = .true.
         rad = 1
      endif
c
c     step 4 main loop
c
      do 100 z = 1 , cnt
         msk = mask(128-(rad*8))
c
c        step a:  clear counters
c
         do 110 i = 0 , 2**(rad*8)-1
            tmpiwork(i) = 0
  110    continue
c
c        step b:  accumulate counts
c
         do 120 i = 1 , n1
            iwrkloc1(i) = and(msk,shiftr(data(wrd,i),shf))
  120    continue
         do 121 i = 1 , n1
            inx = iwrkloc1(i)
            tmpiwork(inx) = tmpiwork(inx)+1
  121    continue
         do 127 j = 1 , n2
            k = 64*(j-1)+n1
         do 125 i = 1 , 64
            iwrkloc1(i) = and(msk,shiftr(data(wrd,k+i),shf))
  125    continue
         do 126 i = 1 , 64
            inx = iwrkloc1(i)
            tmpiwork(inx) = tmpiwork(inx)+1
  126    continue
  127    continue
c
c        step c: cascade counts
c
         iwork(0) = 0
         icount = 2**(rad*8)-1
         call irecps(icount,iwork(1),1,tmpiwork,1)
c
c        step d: compute new indices
c
         do 200 i = 1 , n1
            indxloc1(i) = index(i)
            tmp = data(wrd,and(indxloc1(i),mask(96)))
            iwrkloc1(i) = and(msk,shiftr(tmp,shf))
  200    continue
         do 210 i = 1 , n1
            inx = iwrkloc1(i)
            iwork(inx) = iwork(inx)+1
            iwrkloc1(i) = iwork(inx)
  210    continue
cdir$ ivdep
         do 215 i = 1 , n1
            indxloc1(i) = shiftl(indxloc1(i),32)
            indxloc2 = index(iwrkloc1(i))
            index(iwrkloc1(i)) = or(indxloc1(i),indxloc2)
  215    continue
         do 250 j = 1 , n2
            k = 64*(j-1)+n1
            do 230 i = 1 , 64
               indxloc1(i) = index(k+i)
               tmp = data(wrd,and(indxloc1(i),mask(96)))
               iwrkloc1(i) = and(msk,shiftr(tmp,shf))
  230       continue
            do 240 i = 1 , 64
               inx = iwrkloc1(i)
               iwork(inx) = iwork(inx)+1
               iwrkloc1(i) = iwork(inx)
  240       continue
cdir$ ivdep
         do 245 i = 1 , 64
            indxloc1(i) = shiftl(indxloc1(i),32)
            indxloc2 = index(iwrkloc1(i))
            index(iwrkloc1(i)) = or(indxloc1(i),indxloc2)
  245    continue
  250    continue
c
c        step e: set new indices.
c
         do 150 i = 1 , n
            index(i) = shiftr(index(i),32)
  150    continue
c
c        update loop indices
c
         shf = shf+(8*rad)
         if (shf.ge.64) then
            shf = 0
            wrd = wrd-1
         endif
         if (switch) then
            rad = 2
            switch = .false.
         endif
  100 continue
c
c     step 5: restore keys (if necessary)
c
      if (mod(mode,10).ne.0) then
         if (mod(mode,10).eq.1) then
            maskt = mask(1)
            do 111 i = 1 , n
               data(1,i) = xor(data(1,i),maskt)
  111       continue
         else
            do 112 i = 1 , n
               data(1,i) = xor(data(1,i),
     +                    cvmgm(mask(1),mask(64),data(1,i)))
  112       continue
         endif
      endif
      return
      end

