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

C $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/c_divf.F,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ 

	complex function c_div(a, b)
	real a(2), b(2), c(2)
	double precision ratio, den
	double precision abr, abi

	abr = b(1)
	abi = b(2)
	if( abr .lt. 0.) then
		abr = - abr
	end if
	if( abi .lt. 0.) then
		abi = - abi
	end if
	if( abr .le. abi ) then
		if(abi .eq. 0) then
C		fatal("complex division by zero")
	 	  call abort()
		end if
		ratio = b(1) / b(2) 
		den = b(2) * (1 + ratio*ratio)
		c(1) = (a(1)*ratio + a(2)) / den
		c(2) = (a(2)*ratio - a(1)) / den
	else 
		ratio = b(2) / b(1)
		den = b(1) * (1 + ratio*ratio)
		c(1) = (a(1) + a(2)*ratio) / den
		c(2) = (a(2) - a(1)*ratio) / den
	end if
	c_div = cmplx(c(1), c(2))
	return
	end



