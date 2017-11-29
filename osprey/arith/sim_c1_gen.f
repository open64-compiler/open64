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

	program gencray

	call gen_strtod
	call gen_strtold

	call gen_alog
	call gen_exp
	call gen_sqrt

	call gen_cabs

	call gen_itoi
	call gen_rtoi
	call gen_rtor

	call gen_dlog
	call gen_dexp
	call gen_dsqrt

	call gen_dtoi
	call gen_dtor
	call gen_dtod

	call gen_clog
	call gen_cexp
	call gen_csqrt

	call gen_ctoi
	call gen_ctor
	call gen_ctoc

	call gen_cdlog
	call gen_cdexp
	call gen_cdsqrt

	call gen_cdabs

	call gen_cdtoi
	call gen_cdtocd

	call gen_modi
	call gen_modj
	call gen_mods
	call gen_modd

	call gen_selrk

	stop

	end

	subroutine gen_strtod

	character*22 num

	write(6,"(""STRTOD(.0)"",1(1x,z16.16))") .0
	write(6,"(""STRTOD(0.)"",1(1x,z16.16))") 0.
	write(6,"(""STRTOD(0E0)"",1(1x,z16.16))") 0.
	write(6,"(""STRTOD(0.0E0)"",1(1x,z16.16))") 0.
	write(6,"(""STRTOD(0.0E1)"",1(1x,z16.16))") 0.
	write(6,"(""STRTOD(0.0E-1)"",1(1x,z16.16))") 0.
	write(6,"(""STRTOD(-0.0E+1)"",1(1x,z16.16))") 0.
	write(6,"(""STRTOD(1.00)"",1(1x,z16.16))") 1.00
	write(6,"(""STRTOD(1.0E0)"",1(1x,z16.16))") 1.00
	write(6,"(""STRTOD(0.1E1)"",1(1x,z16.16))") 1.00
	write(6,"(""STRTOD(1.0E+0)"",1(1x,z16.16))") 1.00
	write(6,"(""STRTOD(1.0E-0)"",1(1x,z16.16))") 1.00
	write(6,"(""STRTOD(1E0)"",1(1x,z16.16))") 1.00
	write(6,"(""STRTOD(0.99999999999999)"",1(1x,z16.16))")
     .		0.99999999999999
	write(6,"(""STRTOD(0.999999999999995)"",1(1x,z16.16))")
     .		0.999999999999995
	write(6,"(""STRTOD(0.999999999999999)"",1(1x,z16.16))")
     .		0.999999999999999
	write(6,"(""STRTOD(0.9999999999999999)"",1(1x,z16.16))")
     .		0.9999999999999999
	write(6,"(""STRTOD(0.99999999999999999)"",1(1x,z16.16))")
     .		0.99999999999999999
	write(6,"(""STRTOD(1.23)"",1(1x,z16.16))") 1.23
	write(6,"(""STRTOD(8.3E0)"",1(1x,z16.16))") 8.3E0
	write(6,"(""STRTOD(5.1E-0)"",1(1x,z16.16))") 5.1E-0
	write(6,"(""STRTOD(2.2E+0)"",1(1x,z16.16))") 2.2E+0
	write(6,"(""STRTOD(0.0000456E+7)"",1(1x,z16.16))") 0.0000456E+7
	write(6,"(""STRTOD(0.0000001E-5)"",1(1x,z16.16))") 0.0000001E-5
	write(6,"(""STRTOD(-1.0E-2465)"",1(1x,z16.16))") -1.0E-2465
	write(6,"(""STRTOD(-7.18923E+21)"",1(1x,z16.16))") -7.18923e+21

	do i=1,50
	  x=(ranf()-0.5)*(i+0.1)**84.7
	  if(abs(x).ge.1.e99 .or. abs(x).lt.1.e-99) then
	    write(num,"(e22.14e3)") x
	    read(num,"(e22.14e3)") x
	    write(6,"(""STRTOD("",a22,"")"",1(1x,z16.16))") num,x
	  else
	    write(num,"(e22.15)") x
	    read(num,"(e22.15)") x
	    write(6,"(""STRTOD("",a22,"")"",1(1x,z16.16))") num,x
	  endif
	enddo

	do i=1,50
	  x=(ranf()-0.5)*i*10000
	  if(abs(x).ge.100000.0) then
	    write(num,"(f21.13)") x
	    read(num,"(f21.13)") x
	    write(6,"(""STRTOD("",a21,"")"",1(1x,z16.16))") num,x
	  else
	    x = x*0.01
	    if(abs(x).ge.100.0) then
	      write(num,"(f7.1)") x
	      read(num,"(f7.1)") x
	      write(6,"(""STRTOD("",a7,"")"",1(1x,z16.16))") num,x
	    else
	      write(num,"(f7.3)") x
	      read(num,"(f7.3)") x
	      write(6,"(""STRTOD("",a7,"")"",1(1x,z16.16))") num,x
	    endif
	  endif
	enddo

	write(6,"(""STRTOD(1E-2466)"",1(1x,z16.16))") 2
	write(6,"(""STRTOD(1E+2466)"",1(1x,z16.16))") 1

	return
	end

	subroutine gen_strtold

	character*37 num

	double precision x
	complex cx
	equivalence(cx,x)

	x = 0.0D0
	write(6,"(""STRTOLD(0.)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(.0)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(0E0)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(0.0E0)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(0.0E1)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(0.0E-1)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(-0.0E+1)"",2(1x,z16.16))") cx
	x = 1.0D0
	write(6,"(""STRTOLD(1.00)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(1.0E0)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(0.1E1)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(1.0E+0)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(1.0E-0)"",2(1x,z16.16))") cx
	write(6,"(""STRTOLD(1E0)"",2(1x,z16.16))") cx
	x = 1.875D0
	write(6,"(""STRTOLD(1.875)"",2(1x,z16.16))") cx
	x = 0.99999999999999D0
	write(6,"(""STRTOLD(0.99999999999999)"",2(1x,z16.16))") cx
	x = 0.999999999999994D0
	write(6,"(""STRTOLD(0.999999999999994)"",2(1x,z16.16))") cx
	x = 0.999999999999999D0
	write(6,"(""STRTOLD(0.999999999999999)"",2(1x,z16.16))") cx
	x = 0.9999999999999999D0
	write(6,"(""STRTOLD(0.9999999999999999)"",2(1x,z16.16))") cx
	x = 0.999999999999999999D0
	write(6,"(""STRTOLD(0.999999999999999999)"",2(1x,z16.16))") cx
	x = 1.23D0
	write(6,"(""STRTOLD(1.23)"",2(1x,z16.16))") cx
	x = -7.18923D+21
	write(6,"(""STRTOLD(-7.18923E+21)"",2(1x,z16.16))") cx
	x = -1.0D-2465
	write(6,"(""STRTOLD(-1.0E-2465)"",2(1x,z16.16))") cx

	do i=1,100
	  x=(ranf()-0.5D0)*(i+0.1D0)**87.123
	  if(dabs(x).ge.1.0D99 .or. dabs(x).lt.1.0D-99) then
	    write(num,"(d37.28e3)") x
	    read(num,"(d37.28e3)") x
	    write(6,"(""STRTOLD("",a37,"")"",2(1x,z16.16))") num,cx
	  else
	    write(num,"(d37.29)") x
	    read(num,"(d37.29)") x
	    write(6,"(""STRTOLD("",a37,"")"",2(1x,z16.16))") num,cx
	  endif
	enddo

	write(6,"(""STRTOLD(1E-2466)"",2(1x,z16.16))") 2,0
	write(6,"(""STRTOLD(1E+2466)"",2(1x,z16.16))") 1,0

	return
	end

	subroutine gen_alog

	x = 1.0e2465
	do while(x .gt. 0)
	   z = ALOG(x)
	   write(6,"(""ALOG(R)"",2(1x,z16.16))") x,z
	   x = z
	enddo

	do i=1,100
	   x = RANF()
	   if(x .gt. 0.1) then
	     if(x .lt. 0.2) then
	       x = x * 10.0
	     else if(x .lt. 0.3) then
	       x = x * 1.e2
	     else if(x .lt. 0.4) then
	       x = x * 1.e4
	     else if(x .lt. 0.5) then
	       x = x * 1.e8
	     else if(x .lt. 0.6) then
	       x = x * 1.e16
	     else if(x .lt. 0.7) then
	       x = x * 1.e32
	     else if(x .lt. 0.8) then
	       x = x * 1.e64
	     else if(x .lt. 0.9) then
	       x = x * 1.e128
	     else
	       x = x * 1.e256
	     endif
	   endif
	   z = ALOG(x)
	   write(6,"(""ALOG(R)"",2(1x,z16.16))") x,z
	enddo

	write(6,"(""ALOG(R)"",2(1x,z16.16))") -1.0,4
	return
	end

	subroutine gen_exp

	x = 1.0e-2465
	do while(x .lt. 1.e4)
	   z = EXP(x)
	   write(6,"(""EXP(R)"",2(1x,z16.16))") x,z
	   x = z
	enddo

	x = 0.01
	do while(x .lt. 1.e4)
	   z = EXP(x)
	   write(6,"(""EXP(R)"",2(1x,z16.16))") x,z
	   x = z
	enddo

	x = -1.0
	do while(abs(x) .lt. 1.e4)
	   z = EXP(x)
	   write(6,"(""EXP(R)"",2(1x,z16.16))") x,z
	   x = -1.0/z
	enddo

	do i=1,100
	   x = (RANF()-0.5)*1000.0
	   z = EXP(x)
	   write(6,"(""EXP(R)"",2(1x,z16.16))") x,z
	enddo

	write(6,"(""EXP(R)"",2(1x,z16.16))") 1.e5,4
	return
	end

	subroutine gen_sqrt

	z = 1.0e2465
	x = 0.
	do while(x .ne. z)
	   x = z
	   z = SQRT(x)
	   write(6,"(""SQRT(R)"",2(1x,z16.16))") x,z
	enddo

	z = 1.0e-2465
	x = 0.
	do while(x .ne. z)
	   x = z
	   z = SQRT(x)
	   write(6,"(""SQRT(R)"",2(1x,z16.16))") x,z
	enddo

	write(6,"(""SQRT(R)"",2(1x,z16.16))") -1.0,4
	return
	end

	subroutine gen_cabs

	complex x
	real rcabs

	x = (0.,0.)
	z = rcabs(x)
	write(6,"(""CABS(C)"",3(1x,z16.16))") x,z

	x = (-1.,0.)
	z = rcabs(x)
	write(6,"(""CABS(C)"",3(1x,z16.16))") x,z

	x = (0.,-1.)
	z = rcabs(x)
	write(6,"(""CABS(C)"",3(1x,z16.16))") x,z

	x = (0.1,0.2)
	z = rcabs(x)
	write(6,"(""CABS(C)"",3(1x,z16.16))") x,z

	x = (-0.1,-0.2)
	z = rcabs(x)
	write(6,"(""CABS(C)"",3(1x,z16.16))") x,z

	x = (-0.2,-0.1)
	z = rcabs(x)
	write(6,"(""CABS(C)"",3(1x,z16.16))") x,z

	do i=1,100
	  x = cmplx((ranf()-.5)*10.0**(i+.5),(ranf()-.5)*10.0**(i+.5))
	  z = CABS(x)
	  write(6,"(""CABS(C)"",3(1x,z16.16))") x,z
	enddo

	x = (1.e300,-1.e300)
	z = rcabs(x)
	write(6,"(""CABS(C)"",3(1x,z16.16))") x,z

	return
	end
	real function rcabs(c)
	complex c
	rcabs=cabs(c)
	return
	end

	subroutine gen_itoi

	integer x,y,z

	x = 2
	y = -2
	do while(y .le. 64)
	   z = itoi(x,y)
	   write(6,"(""ITOI(I,I)"",3(1x,z16.16))") x,y,z
	   y = y+1
	enddo

	x = -3
	y = 5
	do while(x .ne. 0)
	   z = itoi(x,y)
	   write(6,"(""ITOI(I,I)"",3(1x,z16.16))") x,y,z
	   y = x
	   x = z
	enddo

	x=0
	y=3000
	write(6,"(""ITOI(I,I)"",3(1x,z16.16))") x,y,itoi(x,y)
	x=1
	y=3000
	write(6,"(""ITOI(I,I)"",3(1x,z16.16))") x,y,itoi(x,y)
	x=2
	y=64
	write(6,"(""ITOI(I,I)"",3(1x,z16.16))") x,y,itoi(x,y)
	x=2
	y=0
	write(6,"(""ITOI(I,I)"",3(1x,z16.16))") x,y,itoi(x,y)
	x=2
	y=-1
	write(6,"(""ITOI(I,I)"",3(1x,z16.16))") x,y,itoi(x,y)

	write(6,"(""ITOI(I,I)"",3(1x,z16.16))") 0,-1,4
	return
	end
	integer function itoi(i,j)
	itoi=i**j
	return
	end

	subroutine gen_rtoi

	integer y

	x = 0.5
	y = 1
	do while(x .gt. 0.0)
	   z = x**y
	   write(6,"(""RTOI(R,I)"",3(1x,z16.16))") x,y,z
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = (RANF()-0.5)*100.0
	   y = (RANF()-0.5)*100.0
	   z = x**y
	   write(6,"(""RTOI(R,I)"",3(1x,z16.16))") x,y,z
	enddo

	x = 0.
	y = 300
	write(6,"(""RTOI(R,I)"",3(1x,z16.16))") x,y,rtoi(x,y)
	x = 1.
	y = 300
	write(6,"(""RTOI(R,I)"",3(1x,z16.16))") x,y,rtoi(x,y)
	x = 2.
	y = 64
	write(6,"(""RTOI(R,I)"",3(1x,z16.16))") x,y,rtoi(x,y)

	write(6,"(""RTOI(R,I)"",3(1x,z16.16))") 1.e100,100,1
	write(6,"(""RTOI(R,I)"",3(1x,z16.16))") 0,-1,4
	return
	end
	real function rtoi(r,i)
	rtoi=r**i
	return
	end

	subroutine gen_rtor

	x = 0.5
	y = 1.0
	do while(x .gt. 0.0)
	   z = x**y
	   write(6,"(""RTOR(R,R)"",3(1x,z16.16))") x,y,z
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = RANF()*100.0
	   y = (RANF()-0.5)*100.0
	   z = x**y
	   write(6,"(""RTOR(R,R)"",3(1x,z16.16))") x,y,z
	enddo

	x = 0.
	y = 300.
	write(6,"(""RTOR(R,R)"",3(1x,z16.16))") x,y,rtor(x,y)
	x = 1.
	y = 300.
	write(6,"(""RTOR(R,R)"",3(1x,z16.16))") x,y,rtor(x,y)
	x = 2.
	y = 64.
	write(6,"(""RTOR(R,R)"",3(1x,z16.16))") x,y,rtor(x,y)

	write(6,"(""RTOR(R,R)"",3(1x,z16.16))") 1.e100,100.,4
	write(6,"(""RTOR(R,R)"",3(1x,z16.16))") 0,-1.0,4
	return
	end
	real function rtor(r,p)
	rtor=r**p
	return
	end

	subroutine gen_dlog

	double precision x,z
	complex cx,cz
	equivalence(cx,x),(cz,z)

	x = 1.0e2465
	do while(x .gt. 0)
	   z = DLOG(x)
	   write(6,"(""DLOG(D)"",4(1x,z16.16))") cx,cz
	   x = z
	enddo

	do i=1,100
	   x = RANF()
	   if(x .gt. 0.1) then
	     if(x .lt. 0.2) then
	       x = x * 10.0
	     else if(x .lt. 0.3) then
	       x = x * 1.e2
	     else if(x .lt. 0.4) then
	       x = x * 1.e4
	     else if(x .lt. 0.5) then
	       x = x * 1.e8
	     else if(x .lt. 0.6) then
	       x = x * 1.e16
	     else if(x .lt. 0.7) then
	       x = x * 1.e32
	     else if(x .lt. 0.8) then
	       x = x * 1.e64
	     else if(x .lt. 0.9) then
	       x = x * 1.e128
	     else
	       x = x * 1.e256
	     endif
	   endif
	   z = DLOG(x)
	   write(6,"(""DLOG(D)"",4(1x,z16.16))") cx,cz
	enddo

	write(6,"(""DLOG(D)"",4(1x,z16.16))") -1.0,0,4,0

	return
	end

	subroutine gen_dexp

	double precision x,z
	complex cx,cz
	equivalence(cx,x),(cz,z)

	x = 1.0e-2465
	do while(x .lt. 1.e4)
	   z = DEXP(x)
	   write(6,"(""DEXP(D)"",4(1x,z16.16))") cx,cz
	   x = z
	enddo

	x = 0.01
	do while(x .lt. 1.e4)
	   z = DEXP(x)
	   write(6,"(""DEXP(D)"",4(1x,z16.16))") cx,cz
	   x = z
	enddo

	x = -1.0
	do while(abs(x) .lt. 1.e4)
	   z = DEXP(x)
	   write(6,"(""DEXP(D)"",4(1x,z16.16))") cx,cz
	   x = -1.0/z
	enddo

	do i=1,100
	   x = (RANF()-0.5)*1000.0
	   z = DEXP(x)
	   write(6,"(""DEXP(D)"",4(1x,z16.16))") cx,cz
	enddo

	write(6,"(""DEXP(D)"",4(1x,z16.16))") 1.e5,0,4,0

	return
	end

	subroutine gen_dsqrt

	double precision x,z
	complex cx,cz
	equivalence(cx,x),(cz,z)

	z = 1.0e2465
	x = 0.
	do while(x .ne. z)
	   x = z
	   z = DSQRT(x)
	   write(6,"(""DSQRT(D)"",4(1x,z16.16))") cx,cz
	enddo

	z = 1.0e-2465
	x = 0.
	do while(x .ne. z)
	   x = z
	   z = DSQRT(x)
	   write(6,"(""DSQRT(D)"",4(1x,z16.16))") cx,cz
	enddo

	write(6,"(""DSQRT(D)"",4(1x,z16.16))") -1.0,0,4,0
	return
	end

	subroutine gen_dtoi

	double precision x,z
	double precision dtoi
	complex cx,cz
	equivalence(cx,x),(cz,z)
	integer y

	x = 0.5
	y = 1
	do while(x .gt. 0.0)
	   z = x**y
	   write(6,"(""DTOI(D,I)"",5(1x,z16.16))") cx,y,cz
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = (RANF()-0.5)*100.0
	   y = (RANF()-0.5)*100.0
	   z = x**y
	   write(6,"(""DTOI(D,I)"",5(1x,z16.16))") cx,y,cz
	enddo

	x=0.
	y=300
	z=dtoi(x,y)
	write(6,"(""DTOI(D,I)"",5(1x,z16.16))") cx,y,cz
	x=1.
	y=300
	z=dtoi(x,y)
	write(6,"(""DTOI(D,I)"",5(1x,z16.16))") cx,y,cz
	x=2.
	y=64
	z=dtoi(x,y)
	write(6,"(""DTOI(D,I)"",5(1x,z16.16))") cx,y,cz

	write(6,"(""DTOI(D,I)"",5(1x,z16.16))") 1.e100,0,100,1,0
	write(6,"(""DTOI(D,I)"",5(1x,z16.16))") 0,0,-1,4,0

	return
	end
	double precision function dtoi(d,i)
	double precision d
	dtoi=d**i
	return
	end

	subroutine gen_dtor

	double precision x,z
	double precision dtor
	complex cx,cz
	equivalence(cx,x),(cz,z)

	x = 0.5
	y = 1.0
	do while(x .gt. 0.0)
	   z = x**y
	   write(6,"(""DTOR(D,R)"",5(1x,z16.16))") cx,y,cz
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = RANF()*100.0
	   y = (RANF()-0.5)*100.0
	   z = x**y
	   write(6,"(""DTOR(D,R)"",5(1x,z16.16))") cx,y,cz
	enddo

	x=0.
	y=300.
	z=dtor(x,y)
	write(6,"(""DTOR(D,R)"",5(1x,z16.16))") cx,y,cz
	x=1.
	y=300.
	z=dtor(x,y)
	write(6,"(""DTOR(D,R)"",5(1x,z16.16))") cx,y,cz
	x=2.
	y=64.
	z=dtor(x,y)
	write(6,"(""DTOR(D,R)"",5(1x,z16.16))") cx,y,cz

	write(6,"(""DTOR(D,R)"",5(1x,z16.16))") 1.e100,0,100.,4,0
	write(6,"(""DTOR(D,R)"",5(1x,z16.16))") 0,0,-1.0,4,0

	return
	end
	double precision function dtor(d,r)
	double precision d
	dtor=d**r
	return
	end

	subroutine gen_dtod

	double precision x,y,z
	double precision dtod
	complex cx,cy,cz
	equivalence(cx,x),(cy,y),(cz,z)

	x = 0.5
	y = 1.0
	do while(x .gt. 0.0)
	   z = x**y
	   write(6,"(""DTOD(D,D)"",6(1x,z16.16))") cx,cy,cz
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = RANF()*100.0
	   y = (RANF()-0.5)*100.0
	   z = x**y
	   write(6,"(""DTOD(D,D)"",6(1x,z16.16))") cx,cy,cz
	enddo

	x=0.
	y=300.
	z=dtod(x,y)
	write(6,"(""DTOD(D,D)"",6(1x,z16.16))") cx,cy,cz
	x=1.
	y=300.
	z=dtod(x,y)
	write(6,"(""DTOD(D,D)"",6(1x,z16.16))") cx,cy,cz
	x=2.
	y=64.
	z=dtod(x,y)
	write(6,"(""DTOD(D,D)"",6(1x,z16.16))") cx,cy,cz

	write(6,"(""DTOD(D,D)"",6(1x,z16.16))") 1.e100,0,100.,0,4,0
	write(6,"(""DTOD(D,D)"",6(1x,z16.16))") 0,0,-1.0,0,4,0

	return
	end
	double precision function dtod(d,dp)
	double precision d,dp
	dtod=d**dp
	return
	end

	subroutine gen_clog

	complex x,z

	x = (1.0e2465,1.0e2465)
	do while(real(x) .gt. 0)
	   z = CLOG(x)
	   write(6,"(""CLOG(C)"",4(1x,z16.16))") x,z
	   x = z
	enddo

	do i=1,100
	   x = cmplx(RANF()-0.5,RANF()-0.5)
	   if(real(x) .gt. 0.1) then
	     if(real(x) .lt. 0.2) then
	       x = x * 10.0
	     else if(real(x) .lt. 0.3) then
	       x = x * 1.e2
	     else if(real(x) .lt. 0.4) then
	       x = x * 1.e4
	     else if(real(x) .lt. 0.5) then
	       x = x * 1.e8
	     else if(real(x) .lt. 0.6) then
	       x = x * 1.e16
	     else if(real(x) .lt. 0.7) then
	       x = x * 1.e32
	     else if(real(x) .lt. 0.8) then
	       x = x * 1.e64
	     else if(real(x) .lt. 0.9) then
	       x = x * 1.e128
	     else
	       x = x * 1.e256
	     endif
	   endif
	   z = CLOG(x)
	   write(6,"(""CLOG(C)"",4(1x,z16.16))") x,z
	enddo

	write(6,"(""CLOG(C)"",4(1x,z16.16))") 0,0,4,0

	return
	end

	subroutine gen_cexp

	complex x,z

	x = (1.0e-2465,1.0e-2465)
	do while(abs(real(x)) .lt. 1.e4)
	   z = CEXP(x)
	   write(6,"(""CEXP(C)"",4(1x,z16.16))") x,z
	   x = z
	enddo

	x = (0.01,0.5)
	do while(abs(real(x)) .lt. 1.e4)
	   z = CEXP(x)
	   write(6,"(""CEXP(C)"",4(1x,z16.16))") x,z
	   x = z
	enddo

	x = (-1.0,-10.3)
	do while(cabs(x) .lt. 1.e4)
	   z = CEXP(x)
	   write(6,"(""CEXP(C)"",4(1x,z16.16))") x,z
	   if(cabs(z) .gt. 1.e-5) then
	     x = -1.0/z
	   else
	     x = 1.e5
	   endif
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*1000.0,(RANF()-0.5)*100.0)
	   z = CEXP(x)
	   write(6,"(""CEXP(C)"",4(1x,z16.16))") x,z
	enddo

	write(6,"(""CEXP(C)"",4(1x,z16.16))") 1.e5,0,4,0

	return
	end

	subroutine gen_csqrt

	complex x,z

	z = (1.0e307,-1.0e307)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = CSQRT(x)
	   write(6,"(""CSQRT(C)"",4(1x,z16.16))") x,z
	enddo

	z = (1.0e-2465,1.0e-2465)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = CSQRT(x)
	   write(6,"(""CSQRT(C)"",4(1x,z16.16))") x,z
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*10000.0,(RANF()-0.5)*100.0)
	   z = CSQRT(x)
	   write(6,"(""CSQRT(C)"",4(1x,z16.16))") x,z
	enddo

	return

	end

	subroutine gen_ctoi

	complex x,z
	complex ctoi
	integer y

	x = (0.5,0.1)
	y = 1
	do while(real(x) .gt. 0.0)
	   z = x**y
	   write(6,"(""CTOI(C,I)"",5(1x,z16.16))") x,y,z
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*1000.0)
	   y = (RANF()-0.5)*100.0
	   z = x**y
	   write(6,"(""CTOI(C,I)"",5(1x,z16.16))") x,y,z
	enddo

	x=(0.,0.)
	y=300
	z=ctoi(x,y)
	write(6,"(""CTOI(C,I)"",5(1x,z16.16))") x,y,z
	x=(1.,1.)
	y=300
	z=ctoi(x,y)
	write(6,"(""CTOI(C,I)"",5(1x,z16.16))") x,y,z
	x=(2.,2.)
	y=64
	z=ctoi(x,y)
	write(6,"(""CTOI(C,I)"",5(1x,z16.16))") x,y,z

	write(6,"(""CTOI(C,I)"",5(1x,z16.16))") 1.e100,0,100,1,0
	write(6,"(""CTOI(C,I)"",5(1x,z16.16))") 0,0,-1,4,0

	return
	end
	complex function ctoi(c,i)
	complex c
	ctoi=c**i
	return
	end

	subroutine gen_ctor

	complex x,z
	complex ctor

	x = (0.5,0.7)
	y = 1.0
	do while(real(x) .gt. 0.0)
	   z = x**y
	   write(6,"(""CTOR(C,R)"",5(1x,z16.16))") x,y,z
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = cmplx((0.5-RANF())*100.0,(0.5-RANF())*1000.0)
	   y = (RANF()-0.5)*100.0
	   z = x**y
	   write(6,"(""CTOR(C,R)"",5(1x,z16.16))") x,y,z
	enddo

	x=(0.,0.)
	y=300.
	z=ctor(x,y)
	write(6,"(""CTOR(C,R)"",5(1x,z16.16))") x,y,z
	x=(1.,1.)
	y=300.
	z=ctor(x,y)
	write(6,"(""CTOR(C,R)"",5(1x,z16.16))") x,y,z
	x=(2.,2.)
	y=64.
	z=ctor(x,y)
	write(6,"(""CTOR(C,R)"",5(1x,z16.16))") x,y,z

	write(6,"(""CTOR(C,R)"",5(1x,z16.16))") 1.e100,0,100.,4,0
	write(6,"(""CTOR(C,R)"",5(1x,z16.16))") 0,0,0,4,0

	return
	end
	complex function ctor(c,r)
	complex c
	ctor=c**r
	return
	end

	subroutine gen_ctoc

	complex x,y,z
	complex ctoc

	x = (0.5,-2.7)
	y = (1.0,1.0)
	do while(abs(real(x)) .lt. 1.e10)
	   z = x**y
	   write(6,"(""CTOC(C,C)"",6(1x,z16.16))") x,y,z
	   x = z
	   y = y + cmplx(RANF()*2.0,(0.5-RANF())*10.0)
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*17.1)
	   y = cmplx((RANF()-0.5)*3.9,(RANF()-0.5)*51.1)
	   z = x**y
	   write(6,"(""CTOC(C,C)"",6(1x,z16.16))") x,y,z
	enddo

	x=(0.,0.)
	y=(300.,300.)
	z=ctoc(x,y)
	write(6,"(""CTOC(C,C)"",6(1x,z16.16))") x,y,z
	x=(1.,1.)
	y=(300.,300.)
	z=ctoc(x,y)
	write(6,"(""CTOC(C,C)"",6(1x,z16.16))") x,y,z
	x=(2.,2.)
	y=(64.,64.)
	z=ctoc(x,y)
	write(6,"(""CTOC(C,C)"",6(1x,z16.16))") x,y,z

	write(6,"(""CTOC(C,C)"",6(1x,z16.16))") 1.e100,0,100.,0,4,0
	write(6,"(""CTOC(C,C)"",6(1x,z16.16))") 0,0,0,0,4,0

	return
	end
	complex function ctoc(c,cp)
	complex c,cp
	ctoc=c**cp
	return
	end

	subroutine gen_cdlog

	complex(16) x,z
	double precision rx,rz,ix,iz
	complex crx,crz,cix,ciz
	equivalence(crx,rx),(crz,rz)
	equivalence(cix,ix),(ciz,iz)

	complex(16) dreal,dimag

	x = cmplx(1.0e307,1.0e307)
	do while(real(x) .gt. 0)
	   z = CDLOG(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDLOG(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	   x = z
	enddo

	do i=1,100
	   x = cmplx(RANF()-0.5,RANF()-0.5)
	   if(real(x) .gt. 0.1) then
	     if(real(x) .lt. 0.2) then
	       x = x * 10.0
	     else if(real(x) .lt. 0.3) then
	       x = x * 1.e2
	     else if(real(x) .lt. 0.4) then
	       x = x * 1.e4
	     else if(real(x) .lt. 0.5) then
	       x = x * 1.e8
	     else if(real(x) .lt. 0.6) then
	       x = x * 1.e16
	     else if(real(x) .lt. 0.7) then
	       x = x * 1.e32
	     else if(real(x) .lt. 0.8) then
	       x = x * 1.e64
	     else if(real(x) .lt. 0.9) then
	       x = x * 1.e128
	     else
	       x = x * 1.e256
	     endif
	   endif
	   z = CDLOG(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDLOG(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	enddo

	write(6,"(""CDLOG(CD)"",8(1x,z16.16))") 0,0,0,0,4,0,0,0

	return
	end

	subroutine gen_cdexp

	complex(16) x,z
	double precision rx,rz,ix,iz
	complex crx,crz,cix,ciz
	equivalence(crx,rx),(crz,rz)
	equivalence(cix,ix),(ciz,iz)

	complex(16) dreal,dimag

	x = cmplx(1.0e-2465,1.0e-2465)
	do while(abs(real(x)) .lt. 1.e4)
	   z = CDEXP(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDEXP(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	   x = z
	enddo

	x = cmplx(0.01,0.5)
	do while(abs(real(x)) .lt. 1.e4)
	   z = CDEXP(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDEXP(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	   x = z
	enddo

	x = cmplx(-1.0,-10.3)
	do while(cdabs(x) .lt. 1.e4)
	   z = CDEXP(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDEXP(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	   if(cdabs(z) .gt. 1.e-5) then
	     x = -1.0/z
	   else
	     x = 1.e5
	   endif
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*1000.0,(RANF()-0.5)*100.0)
	   z = CDEXP(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDEXP(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	enddo

	write(6,"(""CDEXP(CD)"",8(1x,z16.16))") 1.e5,0,0,0,4,0,0,0

	return
	end

	subroutine gen_cdsqrt

	complex(16) x,z
	double precision rx,rz,ix,iz
	complex crx,crz,cix,ciz
	equivalence(crx,rx),(crz,rz)
	equivalence(cix,ix),(ciz,iz)

	complex(16) dreal,dimag

	z = cmplx(1.0e307,-1.0e307)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = CDSQRT(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDSQRT(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	enddo

	z = cmplx(1.0e-2465,1.0e-2465)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = CDSQRT(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDSQRT(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*10000.0,(RANF()-0.5)*100.0)
	   z = CDSQRT(x)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDSQRT(CD)"",8(1x,z16.16))") crx,cix,crz,ciz
	enddo

	return
	end

	subroutine gen_cdabs

	complex(16) x
	double precision z,rx,ix
	complex crx,cix,cz
	equivalence(crx,rx),(cix,ix),(cz,z)

	x = (0.,0.)
	z = CDABS(x)
	rx = dreal(x)
	ix = dimag(x)
	write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz

	x = (-1.,0.)
	z = CDABS(x)
	rx = dreal(x)
	ix = dimag(x)
	write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz

	x = (0.,-1.)
	z = CDABS(x)
	rx = dreal(x)
	ix = dimag(x)
	write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz

	x = (0.1,0.2)
	z = CDABS(x)
	rx = dreal(x)
	ix = dimag(x)
	write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz

	x = (-0.1,-0.2)
	z = CDABS(x)
	rx = dreal(x)
	ix = dimag(x)
	write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz

	x = (-0.2,-0.1)
	z = CDABS(x)
	rx = dreal(x)
	ix = dimag(x)
	write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz

	do i=1,100
	  x = cmplx((ranf()-.5)*10.0**(i+.5),(ranf()-.5)*10.0**(i+.5))
	  z = CDABS(x)
	  rx = dreal(x)
	  ix = dimag(x)
	  write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz
	enddo

	x = (1.e300,-1.e300)
	z = CDABS(x)
	rx = dreal(x)
	ix = dimag(x)
	write(6,"(""CDABS(CD)"",6(1x,z16.16))") crx,cix,cz

	return
	end

	subroutine gen_cdtoi

	complex(16) x,z,CDTOI
	double precision rx,rz,ix,iz
	complex crx,crz,cix,ciz
	equivalence(crx,rx),(crz,rz)
	equivalence(cix,ix),(ciz,iz)
	integer y
	double precision dreal,dimag

	x = (0.5,0.1)
	y = 1
	do while(real(x) .gt. 0.0)
	   z = CDTOI(x,y)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDTOI(CD,I)"",9(1x,z16.16))") crx,cix,y,crz,ciz
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*1000.0)
	   y = (RANF()-0.5)*100.0
	   z = CDTOI(x,y)
	   rx = dreal(x)
	   ix = dimag(x)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDTOI(CD,I)"",9(1x,z16.16))") crx,cix,y,crz,ciz
	enddo

	x=(0.,0.)
	y=300
	z=CDTOI(x,y)
	rx = dreal(x)
	ix = dimag(x)
	rz = dreal(z)
	iz = dimag(z)
	write(6,"(""CDTOI(CD,I)"",9(1x,z16.16))") crx,cix,y,crz,ciz
	x=(1.,1.)
	y=300
	z=CDTOI(x,y)
	rx = dreal(x)
	ix = dimag(x)
	rz = dreal(z)
	iz = dimag(z)
	write(6,"(""CDTOI(CD,I)"",9(1x,z16.16))") crx,cix,y,crz,ciz
	x=(2.,2.)
	y=64
	z=CDTOI(x,y)
	rx = dreal(x)
	ix = dimag(x)
	rz = dreal(z)
	iz = dimag(z)
	write(6,"(""CDTOI(CD,I)"",9(1x,z16.16))") crx,cix,y,crz,ciz

	write(6,"(""CDTOI(CD,I)"",9(1x,z16.16))") 1e100,0,0,0,90,1,0,0,0
	write(6,"(""CDTOI(CD,I)"",9(1x,z16.16))") 0,0,0,0,-1,4,0,0,0

	return
	end

	subroutine gen_cdtocd

	complex(16) x,y,z,CDTOCD
	double precision rx,ry,rz,ix,iy,iz
	complex crx,cry,crz,cix,ciy,ciz
	equivalence(crx,rx),(cry,ry),(crz,rz)
	equivalence(cix,ix),(ciy,iy),(ciz,iz)
	double precision dreal,dimag

	x = (0.5,0.1)
	y = (1.0,1.0)
	do while(real(x) .gt. 0.0)
	   z = CDTOCD(x,y)
	   rx = dreal(x)
	   ix = dimag(x)
	   ry = dreal(y)
	   iy = dimag(y)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDTOCD(CD,CD)"",12(1x,z16.16))")
     .           crx,cix,cry,ciy,crz,ciz
	   x = z
	   y = y+(1.0,-1.0)
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*1000.0)
	   y = cmplx((RANF()-0.25)*10.0,(RANF()-0.75)*100.0)
	   z = CDTOCD(x,y)
	   rx = dreal(x)
	   ix = dimag(x)
	   ry = dreal(y)
	   iy = dimag(y)
	   rz = dreal(z)
	   iz = dimag(z)
	   write(6,"(""CDTOCD(CD,CD)"",12(1x,z16.16))")
     .           crx,cix,cry,ciy,crz,ciz
	enddo

	x=(0.,0.)
	y=(300.,-300.)
	z=CDTOCD(x,y)
	rx = dreal(x)
	ix = dimag(x)
	ry = dreal(y)
	iy = dimag(y)
	rz = dreal(z)
	iz = dimag(z)
	write(6,"(""CDTOCD(CD,CD)"",12(1x,z16.16))")
     .           crx,cix,cry,ciy,crz,ciz
	x=(1.,1.)
	y=(300.,-300.)
	z=CDTOCD(x,y)
	rx = dreal(x)
	ix = dimag(x)
	ry = dreal(y)
	iy = dimag(y)
	rz = dreal(z)
	iz = dimag(z)
	write(6,"(""CDTOCD(CD,CD)"",12(1x,z16.16))")
     .           crx,cix,cry,ciy,crz,ciz
	x=(2.,2.)
	y=(64.,-64.)
	z=CDTOCD(x,y)
	rx = dreal(x)
	ix = dimag(x)
	ry = dreal(y)
	iy = dimag(y)
	rz = dreal(z)
	iz = dimag(z)
	write(6,"(""CDTOCD(CD,CD)"",12(1x,z16.16))")
     .           crx,cix,cry,ciy,crz,ciz

	write(6,"(""CDTOCD(CD,CD)"",12(1x,z16.16))")
     .           1e100,0,1.0,0,99.0,0,-99.0,0,4,0,0,0
	x=(0.,0.)
	y=(-1.0,-1.0)
	z=CDTOCD(x,y)
	rx = dreal(x)
	ix = dimag(x)
	ry = dreal(y)
	iy = dimag(y)
	rz = dreal(z)
	iz = dimag(z)
	write(6,"(""CDTOCD(CD,CD)"",12(1x,z16.16))")
     .           crx,cix,cry,ciy,crz,ciz

	return
	end

	subroutine gen_modi
	integer*6 modi,i,m

	write(6,"(""MODULOI(I,I)"",3(1x,z16.16))")
     .		-1,1,modi(-1,1)
	write(6,"(""MODULOI(I,I)"",3(1x,z16.16))")
     .		1,-1,modi(1,-1)
	write(6,"(""MODULOI(I,I)"",3(1x,z16.16))")
     .		0,1,modi(0,1)
	write(6,"(""MODULOI(I,I)"",3(1x,z16.16))")
     .		1,0,modi(1,0)
        do i=1,100,10
          do j=1,3
            m=2*i*ranf()+j
            write(6,"(""MODULOI(I,I)"",3(1x,z16.16))")
     .		i,m,modi(i,m)
	  enddo
	enddo

	return
	end
	integer*6 function modi(i,m)
	integer*6 i,m
	modi=modulo(i,m)
	return
	end

	subroutine gen_modj
	integer*8 modj,i,m

	write(6,"(""MODULOJ(I,I)"",3(1x,z16.16))")
     .		-1,1,modj(-1,1)
	write(6,"(""MODULOJ(I,I)"",3(1x,z16.16))")
     .		1,-1,modj(1,-1)
	write(6,"(""MODULOJ(I,I)"",3(1x,z16.16))")
     .		0,1,modj(0,1)
	write(6,"(""MODULOJ(I,I)"",3(1x,z16.16))")
     .		1,0,modj(1,0)
        do i=1,100,10
          do j=1,3
            m=2*i*ranf()+j
            write(6,"(""MODULOJ(I,I)"",3(1x,z16.16))")
     .		i,m,modj(i,m)
	  enddo
	enddo

	return
	end
	integer*8 function modj(i,m)
	integer*8 i,m
	modj=modulo(i,m)
	return
	end

	subroutine gen_mods
	real*8 mods,x,y

	write(6,"(""MODULOS(R,R)"",3(1x,z16.16))")
     .		-1.,1.,mods(-1.,1.)
	write(6,"(""MODULOS(R,R)"",3(1x,z16.16))")
     .		1.,-1.,mods(1.,-1.)
	write(6,"(""MODULOS(R,R)"",3(1x,z16.16))")
     .		0,1.,mods(0,1.)
	write(6,"(""MODULOS(R,R)"",3(1x,z16.16))")
     .		1.,0,mods(1.,0)
        do i=1,100
          x=(ranf()-0.5)*100
          y=(ranf()-0.5)*50
          write(6,"(""MODULOS(R,R)"",3(1x,z16.16))")
     .		x,y,mods(x,y)
	enddo

	return
	end
	real*8 function mods(x,y)
	real*8 x,y
	mods=modulo(x,y)
	return
	end

	subroutine gen_modd
	real*16 modd,x,y,z
	complex cx,cy,cz
	equivalence(cx,x),(cy,y),(cz,z)

	x=-1.0d0
	y=1.0d0
	z=modd(x,y)
	write(6,"(""MODULOD(D,D)"",6(1x,z16.16))")
     .		cx,cy,cz
	x=1.0d0
	y=-1.0d0
	z=modd(x,y)
	write(6,"(""MODULOD(D,D)"",6(1x,z16.16))")
     .		cx,cy,cz
	x=0.0d0
	y=1.0d0
	z=modd(x,y)
	write(6,"(""MODULOD(D,D)"",6(1x,z16.16))")
     .		cx,cy,cz
	x=1.0d0
	y=0.0d0
	z=modd(x,y)
	write(6,"(""MODULOD(D,D)"",6(1x,z16.16))")
     .		cx,cy,cz
        do i=1,100
          x=(ranf()-0.3d0)*1.001d2
          y=(ranf()-0.1d0)*5.01d1
          z=modd(x,y)
          write(6,"(""MODULOD(D,D)"",6(1x,z16.16))")
     .		cx,cy,cz
	enddo

	return
	end
	real*16 function modd(x,y)
	real*16 x,y
	modd=modulo(x,y)
	return
	end

	subroutine gen_selrk
	integer p,r,k
	p=-1
	do while(p.lt.50)
	  r=-1
	  do while(r.lt.10000)
	    if(p.lt.0 .and. r.lt.0) then
	      k=selected_real_kind(1)
	    elseif(p.lt.0) then
	      k=selected_real_kind(R=r)
	    elseif(r.lt.0) then
	      k=selected_real_kind(p)
	    else
	      k=selected_real_kind(p,r)
	    endif
	    write(6,"(""SELREALK(I,I)"",3(1x,z16.16))")
     .		p,r,k
	    if(r.le.0) then
	      r=r+1
	    else
	      r=r*20
	    endif
	  enddo
	  if(p.le.0) then
	    p=p+1
	  else
	    p=p*2
	  endif
	enddo
	return
	end

c USMID = "\n%Z%%M%	%I%	%G% %U%\n";
c rcsid = "$Id: sim_c1_gen.f,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
