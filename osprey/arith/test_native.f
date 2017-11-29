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

	subroutine test_native

	call test_strtod
#ifdef LD
	call test_strtold
#endif

	call test_alog
	call test_exp
	call test_sqrt

	call test_cabs

	call test_itoi
	call test_rtoi
	call test_rtor

#ifdef LD
	call test_dlog
	call test_dexp
	call test_dsqrt

	call test_dtoi
	call test_dtor
	call test_dtod
#endif

	call test_clog
	call test_cexp
	call test_csqrt

	call test_ctoi
	call test_ctor
	call test_ctoc

#ifdef LD
	call test_cdlog
	call test_cdexp
	call test_cdsqrt

	call test_cdabs

	call test_cdtoi
	call test_cdtocd
#endif
	return

	end

	subroutine test_strtod

	real*8	x

	x=0.
	call ar_strtod(x)
	x=1.0
	call ar_strtod(x)
	x=0.99999999999999
	call ar_strtod(x)
	x=0.999999999999995
	call ar_strtod(x)
	x=0.999999999999999
	call ar_strtod(x)
	x=0.9999999999999999
	call ar_strtod(x)
	x=0.99999999999999999
	call ar_strtod(x)
	x=1.9375
	call ar_strtod(x)
	x=1.23
	call ar_strtod(x)
	x=-7.18923e+21
	call ar_strtod(x)

	do i=1,50
	  x=(ranf()-0.5)*(i+0.1)**11.7
	  call ar_strtod(x)
	enddo

	do i=1,50
	  x=(ranf()-0.5)*i*10000
	  call ar_strtod(x)
	enddo

	return
	end

#ifdef LD
	subroutine test_strtold

	real*16 x

	x=0.
	call ar_strtold(x)
	x=1.
	call ar_strtold(x)
	x=0.99999999999999D0
	call ar_strtold(x)
	x=0.999999999999995D0
	call ar_strtold(x)
	x=0.999999999999999D0
	call ar_strtold(x)
	x=0.9999999999999999D0
	call ar_strtold(x)
	x=0.99999999999999999D0
	call ar_strtold(x)
	x=1.9375D0
	call ar_strtold(x)
	x=1.23D0
	call ar_strtold(x)
	x=-7.18923D+21
	call ar_strtold(x)

	do i=1,100
	  x=(ranf()-0.5D0)*(i+0.1D0)**80.123
	  call ar_strtold(x)
	enddo

	return
	end
#endif

	subroutine test_alog

	real*8 x,z

	x = 1.0e32
	do while(x .gt. 0)
	   z = LOG(x)
	   call ar_intrin1("ALOG", x, z)
	   x = z
	enddo

	do i=1,100
	   x = RANF()
	   if(x .gt. 0.1) then
	     if(x .lt. 0.2) then
	       x = x * 10.0
	     else if(x .lt. 0.4) then
	       x = x * 1.e2
	     else if(x .lt. 0.6) then
	       x = x * 1.e4
	     else if(x .lt. 0.8) then
	       x = x * 1.e8
	     else
	       x = x * 1.e16
	     endif
	   endif
	   z = LOG(x)
	   call ar_intrin1("ALOG", x, z)
	enddo

	return

	end

	subroutine test_exp

	real*8 x,z

	x = 1.0e-32
	do while(x .lt. 1.e4)
	   z = EXP(x)
	   call ar_intrin1("EXP", x, z)
	   x = z
	enddo

	x = 0.01
	do while(x .lt. 1.e4)
	   z = EXP(x)
	   call ar_intrin1("EXP", x, z)
	   x = z
	enddo

	x = -1.0
	do while(x .gt. 1.0e-14)
	   z = EXP(x)
	   call ar_intrin1("EXP", x, z)
	   x = -z
	enddo

	do i=1,100
	   x = (RANF()-0.5)*10.0
	   z = EXP(x)
	   call ar_intrin1("EXP", x, z)
	enddo

	return
	end

	subroutine test_sqrt

	real*8 x,z

	z = 1.0e32
	x = 0.
	do while(x .ne. z)
	   x = z
	   z = SQRT(x)
	   call ar_intrin1("SQRT", x, z)
	enddo

	z = 1.0e-32
	x = 0.
	do while(x .ne. z)
	   x = z
	   z = SQRT(x)
	   call ar_intrin1("SQRT", x, z)
	enddo

	return
	end

	subroutine test_cabs
#if __sparc__
	complex(8) x
#else
	complex x
#endif
	real*8 z

	x = (0.,0.)
	z = abs(x)
	call ar_intrin1("CABS", x, z)

	x = (-1.,0.)
	z = abs(x)
	call ar_intrin1("CABS", x, z)

	x = (0.,-1.)
	z = abs(x)
	call ar_intrin1("CABS", x, z)

	x = (0.1,0.2)
	z = abs(x)
	call ar_intrin1("CABS", x, z)

	x = (-0.1,-0.2)
	z = abs(x)
	call ar_intrin1("CABS", x, z)

	x = (-0.2,-0.1)
	z = abs(x)
	call ar_intrin1("CABS", x, z)

	do i=1,100
	  x = cmplx((ranf()-.5)*10.0*(i+.5),(ranf()-.5)*10.0*(i+.5))
	  z = abs(x)
	  call ar_intrin1("CABS", x, z)
	enddo

	x = (1.e20,-1.e20)
	z = abs(x)
	call ar_intrin1("CABS", x, z)

	return
	end
	subroutine test_itoi
	integer x,y,z
#ifdef _CRAY
	integer x0,y0,z0
	equivalence(x0,x), (y0,y), (z0,z)
#else
	integer x0(2),y0(2),z0(2)
	equivalence (x0(2),x), (y0(2),y), (z0(2),z)
	x0(1) = 0
	y0(1) = 0
	z0(1) = 0
#endif

	x = 2
	y = -2
	do while(y .lt. 64)
	   z = itoi(x,y)
	   call ar_intrin2("ITOI",x0,y0,z0)
	   y = y+1
	enddo

	x = -3
	y = 5
	do while(x .ne. 0)
	   z = itoi(x,y)
	   call ar_intrin2("ITOI",x0,y0,z0)
	   y = x
	   x = z
	enddo

	x=0
	y=20000
	z=itoi(x,y)
	call ar_intrin2("ITOI",x0,y0,z0)
	x=1
	y=20000
	z=itoi(x,y)
	call ar_intrin2("ITOI",x0,y0,z0)
	x=2
	y=32
	z=itoi(x,y)
	call ar_intrin2("ITOI",x0,y0,z0)
	x=2
	y=0
	z=itoi(x,y)
	call ar_intrin2("ITOI",x0,y0,z0)
	x=2
	y=-1
	z=itoi(x,y)
	call ar_intrin2("ITOI",x0,y0,z0)

	return
	end
	integer function itoi(i,j)

	itoi=i**j
	return
	end

	subroutine test_rtoi

	integer y
	real*8 x,z,rtoi
#ifdef _CRAY
	integer y0
	equivalence (y0,y)
#else
	integer y0(2)
	equivalence (y0(2),y)
	y0(1) = 0
#endif

	x = 0.5
	y = 1
	do while(x .gt. 0.0)
	   z = rtoi(x,y)
	   call ar_intrin2("RTOI", x, y0, z)
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = (RANF()-0.5)*10.0
	   y = (RANF()-0.5)*10.0
	   z = rtoi(x,y)
	    call ar_intrin2("RTOI", x, y0, z)
	enddo

	x=0.
	y=2000
	z=rtoi(x,y)
	call ar_intrin2("RTOI",x,y0,z)
	x=1.
	y=2000
	z=rtoi(x,y)
	call ar_intrin2("RTOI",x,y0,z)
	x=2.
	y=64
	z=rtoi(x,y)
	call ar_intrin2("RTOI",x,y0,z)

	return
	end
	real*8 function rtoi(r,i)
	real*8 r
	rtoi=r**i
	return
	end

	subroutine test_rtor

	real*8 x,y,z,rtor

	x = 0.5
	y = 1.0
	do while(x .gt. 1.e-14)
	   z = rtor(x,y)
	   call ar_intrin2("RTOR",x,y,z)
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = RANF()*10.0
	   y = (RANF()-0.5)*10.0
	   z = rtor(x,y)
	   call ar_intrin2("RTOR",x,y,z)
	enddo

	x=0.
	y=2000.
	call ar_intrin2("RTOR",x,y,rtor(x,y))
	x=1.
	y=2000.
	call ar_intrin2("RTOR",x,y,rtor(x,y))
	x=2.
	y=64.
	call ar_intrin2("RTOR",x,y,rtor(x,y))

	return
	end
	real*8 function rtor(r,p)
	real*8 r,p
	rtor=r**p
	return
	end

#ifdef LD
	subroutine test_dlog

	real*16 x,z

	x = 1.0d32
	do while(x .gt. 1.d-300)
	   z = LOG(x)
	   call ar_intrin1("DLOG",x,z)
	   x = z
	enddo

	do i=1,100
	   x = RANF()
	   if(x .gt. 0.1) then
	     if(x .lt. 0.2) then
	       x = x * 10.0
	     else if(x .lt. 0.4) then
	       x = x * 1.d2
	     else if(x .lt. 0.6) then
	       x = x * 1.d4
	     else if(x .lt. 0.8) then
	       x = x * 1.d8
	     else
	       x = x * 1.d16
	     endif
	   endif
	   z = LOG(x)
	   call ar_intrin1("DLOG",x,z)
	enddo

	return
	end

	subroutine test_dexp

	real*16 x,z

	x = 1.0d-32
	do while(x .lt. 1.d4)
	   z = EXP(x)
	   call ar_intrin1("DEXP",x,z)
	   x = z
	enddo

	x = 0.01
	do while(x .lt. 1.d4)
	   z = EXP(x)
	   call ar_intrin1("DEXP",x,z)
	   x = z
	enddo

	x = -1.0
	do while(x .gt. 1.0d-14)
	   z = EXP(x)
	   call ar_intrin1("DEXP",x,z)
	   x = -z
	enddo

	do i=1,100
	   x = (RANF()-0.5)*10.0
	   z = EXP(x)
	   call ar_intrin1("DEXP",x,z)
	enddo

	return
	end

	subroutine test_dsqrt

	real*16 x,z

	z = 1.0d32
	x = 2.0*z
	do while(x .gt. z)
	   x = z
	   z = SQRT(x)
	   call ar_intrin1("DSQRT",x,z)
	enddo

	z = 1.0d-32
	x = 0.
	do while(x .lt. z)
	   x = z
	   z = SQRT(x)
	   call ar_intrin1("DSQRT",x,z)
	enddo

	return
	end

	subroutine test_dtoi

	real*16 x,z,dtoi
	integer y
#ifdef _CRAY
	integer y0
	equivalence (y0,y)
#else
	integer y0(2)
	equivalence (y0(2),y)
	y0(1) = 0
#endif

	x = 0.5
	y = 1
	do while(x .gt. 1.d-300)
	   z = dtoi(x,y)
	   call ar_intrin2("DTOI",x,y0,z)
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = (RANF()-0.5)*100.0
	   y = (RANF()-0.5)*10.0
	   z = dtoi(x,y)
	   call ar_intrin2("DTOI",x,y0,z)
	enddo

	x=0.
	y=2000
	z=dtoi(x,y)
	call ar_intrin2("DTOI",x,y0,z)
	x=1.
	y=2000
	z=dtoi(x,y)
	call ar_intrin2("DTOI",x,y0,z)
	x=2.
	y=64
	z=dtoi(x,y)
	call ar_intrin2("DTOI",x,y0,z)

	return
	end
	real*16 function dtoi(d,i)
	real*16 d
	dtoi=d**i
	return
	end

	subroutine test_dtor

	real*16 x,z,dtor
	real*8 y

	x = 0.5
	y = 1.0
	do while(x .gt. 1.d-300)
	   z = dtor(x,y)
	   call ar_intrin2("DTOR",x,y,z)
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = RANF()*100.0
	   y = (RANF()-0.5)*10.0
	   z = dtor(x,y)
	   call ar_intrin2("DTOR",x,y,z)
	enddo

	x=0.
	y=2000.
	z=dtor(x,y)
	call ar_intrin2("DTOR",x,y,z)
	x=1.
	y=2000.
	z=dtor(x,y)
	call ar_intrin2("DTOR",x,y,z)
	x=2.
	y=64.
	z=dtor(x,y)
	call ar_intrin2("DTOR",x,y,z)

	return
	end
	real*16 function dtor(d,r)
	real*16 d
	real*8 r
	dtor=d**r
	return
	end

	subroutine test_dtod

	real*16 x,y,z,dtod

	x = 0.5
	y = 1.0
	do while(x .gt. 1.d-300)
	   z = dtod(x,y)
	   call ar_intrin2("DTOD",x,y,z)
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = RANF()*100.0
	   y = (RANF()-0.5)*10.0
	   z = dtod(x,y)
	   call ar_intrin2("DTOD",x,y,z)
	enddo

	x=0.
	y=2000.
	z=dtod(x,y)
	call ar_intrin2("DTOD",x,y,z)
	x=1.
	y=2000.
	z=dtod(x,y)
	call ar_intrin2("DTOD",x,y,z)
	x=2.
	y=64.
	z=dtod(x,y)
	call ar_intrin2("DTOD",x,y,z)

	return
	end
	real*16 function dtod(d,p)
	real*16 d,p
	dtod=d**p
	return
	end
#endif

	subroutine test_clog

#if __sparc__
	complex(8) x,z
#else
	complex x,z
#endif

	x = (1.0e32,1.0e32)
	do while(real(x) .gt. 0)
	   z = log(x)
	   call ar_intrin1("CLOG",x,z)
	   x = z
	enddo

	do i=1,100
	   x = cmplx(RANF()-0.5,RANF()-0.5)
	   if(real(x) .gt. 0.1) then
	     if(real(x) .lt. 0.2) then
	       x = x * 10.0
	     else if(real(x) .lt. 0.4) then
	       x = x * 1.e2
	     else if(real(x) .lt. 0.6) then
	       x = x * 1.e4
	     else if(real(x) .lt. 0.8) then
	       x = x * 1.e8
	     else
	       x = x * 1.e16
	     endif
	   endif
	   z = log(x)
	   call ar_intrin1("CLOG",x,z)
	enddo

	return
	end

	subroutine test_cexp

#if __sparc__
	complex(8) x,z
#else
	complex x,z
#endif

	x = (1.0e-32,1.0e-32)
	do while(abs(x) .lt. 1.e4)
	   z = exp(x)
	   call ar_intrin1("CEXP",x,z)
	   x = z
	enddo

	x = (0.01,0.5)
	do while(abs(x) .lt. 1.e4)
	   z = exp(x)
	   call ar_intrin1("CEXP",x,z)
	   x = z
	enddo

	x = (-1.0,-10.3)
	do while(abs(x) .lt. 1.e4)
	   z = exp(x)
	   call ar_intrin1("CEXP",x,z)
	   if(abs(z) .gt. 1.e-5) then
	     x = -1.0/z
	   else
	     x = 1.e5
	   endif
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*10.0,(RANF()-0.5)*10.0)
	   z = exp(x)
	   call ar_intrin1("CEXP",x,z)
	enddo

	return
	end

	subroutine test_csqrt

#if __sparc__
	complex(8) x,z
#else
	complex x,z
#endif

	z = (1.0e32,-1.0e32)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = sqrt(x)
	   call ar_intrin1("CSQRT",x,z)
	enddo

	z = (1.0e-32,1.0e-32)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = sqrt(x)
	   call ar_intrin1("CSQRT",x,z)
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*10000.0,(RANF()-0.5)*100.0)
	   z = sqrt(x)
	   call ar_intrin1("CSQRT",x,z)
	enddo

	return

	end

	subroutine test_ctoi

#if __sparc__
	complex(8) x,z,ctoi
#else
	complex x,z,ctoi
#endif

	integer y
#ifdef _CRAY
	integer y0
	equivalence (y0,y)
#else
	integer y0(2)
	equivalence (y0(2),y)
	y0(1) = 0
#endif

	x = (0.5,0.1)
	y = 1
	do while(real(x) .gt. 0.0)
	   z = ctoi(x,y)
	   call ar_intrin2("CTOI",x,y0,z)
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*10.0)
	   y = (RANF()-0.5)*10.0
	   z = ctoi(x,y)
	   call ar_intrin2("CTOI",x,y0,z)
	enddo

	x=(0.,0.)
	y=2000
	z=ctoi(x,y)
	call ar_intrin2("CTOI",x,y0,z)
	x=(1.,1.)
	y=2000
	z=ctoi(x,y)
	call ar_intrin2("CTOI",x,y0,z)
	x=(2.,2.)
	y=64
	z=ctoi(x,y)
	call ar_intrin2("CTOI",x,y0,z)

	return
	end
#if __sparc__
	complex(8) function ctoi(c,i)
	complex(8) c
#else
	complex function ctoi(c,i)
	complex c
#endif
	ctoi=c**i
	return
	end

	subroutine test_ctor

#if __sparc__
	complex(8) x,z,ctor
#else
	complex x,z,ctor
#endif
	real*8 y

	x = (0.5,0.7)
	y = 1.0
	do while(real(x) .gt. 0.0)
	   z = ctor(x,y)
	   call ar_intrin2("CTOR",x,y,z)
	   x = z
	   y = y + RANF()*2.0
	enddo

	do i=1,100
	   x = cmplx((0.5-RANF())*100.0,(0.5-RANF())*10.0)
	   y = (RANF()-0.5)*10.0
	   z = ctor(x,y)
	   call ar_intrin2("CTOR",x,y,z)
	enddo

	x=(1.e-32,1.e-32)
	y=10.
	z=ctor(x,y)
	call ar_intrin2("CTOR",x,y,z)
	x=(1.,1.)
	y=10.
	z=ctor(x,y)
	call ar_intrin2("CTOR",x,y,z)
	x=(2.,2.)
	y=64.
	z=ctor(x,y)
	call ar_intrin2("CTOR",x,y,z)

	return
	end
#if __sparc__
	complex(8) function ctor(c,r)
	complex(8) c
#else
	complex function ctor(c,r)
	complex c
#endif
	real*8 r
	ctor=c**r
	return
	end

	subroutine test_ctoc

#if __sparc__
	complex(8) x,y,z,ctoc
#else
	complex x,y,z,ctoc
#endif

	x = (0.5,-2.7)
	y = (1.0,1.0)
	do while(abs(real(x)) .lt. 1.e10 .and.
     &           abs(real(x)) .gt. 1.e-14)
	   z = ctoc(x,y)
	   call ar_intrin2("CTOC",x,y,z)
	   x = z
	   y = y + cmplx(RANF()*2.0,(0.5-RANF())*10.0)
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*7.1)
	   y = cmplx((RANF()-0.5)*3.9,(RANF()-0.5)*2.5)
	   z = ctoc(x,y)
	   call ar_intrin2("CTOC",x,y,z)
	enddo

	x=(1.e-10,1.e-10)
	y=(20.,20.)
	z=ctoc(x,y)
	call ar_intrin2("CTOC",x,y,z)
	x=(1.,1.)
	y=(10.,10.)
	z=ctoc(x,y)
	call ar_intrin2("CTOC",x,y,z)
	x=(2.,2.)
	y=(64.,64.)
	z=ctoc(x,y)
	call ar_intrin2("CTOC",x,y,z)

	return
	end
#if __sparc__
	complex(8) function ctoc(c,p)
	complex(8) c,p
#else
	complex function ctoc(c,p)
	complex c,p
#endif
	ctoc=c**p
	return
	end

#ifdef LD
	subroutine test_cdlog

	complex(16) x,z

	x = cmplx(1.0d32,1.0d32)
	do while(real(x) .gt. 0)
	   z = LOG(x)
	   call ar_intrin1("CDLOG",x,z)
	   x = z
	enddo

	do i=1,100
	   x = cmplx(RANF()-0.5,RANF()-0.5)
	   if(real(x) .gt. 0.1) then
	     if(real(x) .lt. 0.2) then
	       x = x * 10.0
	     else if(real(x) .lt. 0.4) then
	       x = x * 1.d2
	     else if(real(x) .lt. 0.6) then
	       x = x * 1.d4
	     else if(real(x) .lt. 0.8) then
	       x = x * 1.d8
	     else
	       x = x * 1.d16
	     endif
	   endif
	   z = LOG(x)
	   call ar_intrin1("CDLOG",x,z)
	enddo

	return
	end

	subroutine test_cdexp

	complex(16) x,z

	x = cmplx(1.0d-32,1.0d-32)
	do while(abs(real(x)) .lt. 1.d2)
	   z = EXP(x)
	   call ar_intrin1("CDEXP",x,z)
	   x = z
	enddo

	x = cmplx(0.01,0.5)
	do while(abs(real(x)) .lt. 1.d2)
	   z = EXP(x)
	   call ar_intrin1("CDEXP",x,z)
	   x = z
	enddo

	x = cmplx(-1.0,-10.3)
	do while(real(x) .gt. 1.0d-14)
	   z = EXP(x)
	   call ar_intrin1("CDEXP",x,z)
	   x = -z
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*10.0)
	   z = EXP(x)
	   call ar_intrin1("CDEXP",x,z)
	enddo

	return
	end

	subroutine test_cdsqrt

	complex(16) x,z

	z = cmplx(1.0d32,-1.0d32)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = SQRT(x)
	   call ar_intrin1("CDSQRT",x,z)
	enddo

	z = cmplx(1.0d-32,1.0d-32)
	x = 0.
	do while(real(x) .ne. real(z))
	   x = z
	   z = SQRT(x)
	   call ar_intrin1("CDSQRT",x,z)
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*10000.0,(RANF()-0.5)*100.0)
	   z = SQRT(x)
	   call ar_intrin1("CDSQRT",x,z)
	enddo

	return
	end

	subroutine test_cdabs

	complex(16) x
	real*16 z

	x = (0.,0.)
	z = ABS(x)
	call ar_intrin1("CDABS",x,z)

	x = (-1.,0.)
	z = ABS(x)
	call ar_intrin1("CDABS",x,z)

	x = (0.,-1.)
	z = ABS(x)
	call ar_intrin1("CDABS",x,z)

	x = (0.1,0.2)
	z = ABS(x)
	call ar_intrin1("CDABS",x,z)

	x = (-0.1,-0.2)
	z = ABS(x)
	call ar_intrin1("CDABS",x,z)

	x = (-0.2,-0.1)
	z = ABS(x)
	call ar_intrin1("CDABS",x,z)

	do i=1,100
	  x = cmplx((ranf()-.5)*10.0*(i+.5),(ranf()-.5)*10.0*(i+.5))
	  z = ABS(x)
	  call ar_intrin1("CDABS",x,z)
	enddo

	x = (1.d200,-1.d200)
	z = ABS(x)
	call ar_intrin1("CDABS",x,z)

	return
	end

	subroutine test_cdtoi

	complex(16) x,z,xcdtoi
	integer y
#ifdef _CRAY
	integer y0
	equivalence (y0,y)
#else
	integer y0(2)
	equivalence (y0(2),y)
	y0(1) = 0
#endif

	x = (0.5,0.1)
	y = 1
	do while(real(x) .gt. 0.0)
	   z = xcdtoi(x,y)
	   call ar_intrin2("CDTOI",x,y0,z)
	   x = z
	   y = y+1
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*10.0)
	   y = (RANF()-0.5)*10.0
	   z = xcdtoi(x,y)
	   call ar_intrin2("CDTOI",x,y0,z)
	enddo

	x=(0.,0.)
	y=2000
	z=xcdtoi(x,y)
	call ar_intrin2("CDTOI",x,y0,z)
	x=(1.,1.)
	y=200
	z=xcdtoi(x,y)
	call ar_intrin2("CDTOI",x,y0,z)
	x=(2.,2.)
	y=64
	z=xcdtoi(x,y)
	call ar_intrin2("CDTOI",x,y0,z)

	return
	end
	complex(16) function xcdtoi(x,i)
	complex(16) x
	integer i

	xcdtoi = x**i
	return
	end

	subroutine test_cdtocd

	complex(16) x,y,z,xcdtocd

	x = (0.5,0.1)
	y = (1.0,1.0)
	do while(real(x) .gt. 0.0)
	   z = xcdtocd(x,y)
	   call ar_intrin2("CDTOCD",x,y,z)
	   x = z
	   y = y+(1.0,-1.0)
	enddo

	do i=1,100
	   x = cmplx((RANF()-0.5)*100.0,(RANF()-0.5)*50.0)
	   y = cmplx((RANF()-0.25)*10.0,(RANF()-0.75)*8.0)
	   z = xcdtocd(x,y)
	   call ar_intrin2("CDTOCD",x,y,z)
	enddo

	x=(0.,0.)
	y=(2000.,-2000.)
	z=xcdtocd(x,y)
	call ar_intrin2("CDTOCD",x,y,z)
	x=(1.,1.)
	y=(20.,-40.)
	z=xcdtocd(x,y)
	call ar_intrin2("CDTOCD",x,y,z)
	x=(2.,2.)
	y=(64.,-64.)
	z=xcdtocd(x,y)
	call ar_intrin2("CDTOCD",x,y,z)
	x=(0.,0.)
	y=(-1.0,-1.0)
	z=xcdtocd(x,y)
	call ar_intrin2("CDTOCD",x,y,z)

	return
	end
	complex(16) function xcdtocd(x,y)
	complex(16) x,y

	xcdtocd = x**y
	return
	end
#endif


c USMID = "\n%Z%%M%	%I%	%G% %U%\n";
c rcsid = "$Id: test_native.f,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
