  //
  //
  //  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
  //
  //  This program is free software; you can redistribute it and/or modify it
  //  under the terms of version 2.1 of the GNU Lesser General Public License 
  //  as published by the Free Software Foundation.
  //
  //  This program is distributed in the hope that it would be useful, but
  //  WITHOUT ANY WARRANTY; without even the implied warranty of
  //  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
  //
  //  Further, this software is distributed without any warranty that it is
  //  free of the rightful claim of any third person regarding infringement 
  //  or the like.  Any license provided herein, whether implied or 
  //  otherwise, applies only to this software file.  Patent licenses, if
  //  any, provided herein do not apply to combinations of this program with 
  //  other software, or any other product whatsoever.  
  //
  //  You should have received a copy of the GNU Lesser General Public 
  //  License along with this program; if not, write the Free Software 
  //  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  //  USA.
  //
  //  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  //  Mountain View, CA 94043, or:
  //
  //  http://www.sgi.com
  //
  //  For further information regarding this notice, see:
  //
  //  http://oss.sgi.com/projects/GenInfo/NoticeExplan
  //
  //


	.section .sdata, "a", "progbits"
	.align	8
	.text

	.proc	__logb
	.global	__logb
	.weak	logb
	logb = __logb

	// double __logb(double x)

	// returns the unbiased exponent of x as a double

 // LEAF(__logb)

__logb:
	getf.d	r14=farg0	// move operand to an integer reg.
	;;
	shr.u	r15=r14,52	// shift off mantissa
	mov	r16=0x7ff
	;;
	and	r15=r15,r16	// mask off sign bit
	;;
	cmp.eq.unc p6,p7=r15,r0	// compare exponent with zero
	;;
 (p6)	br.cond.dptk	small		// branch if exponent == 0
	;;
	cmp.lt.unc p6,p7=r15,r16 // test if exponent == 0x7ff
	;;
 (p6)	br.cond.dptk	infnan		// branch if arg is an inf or NaN
	mov	r16=0x3ff
	;;
	sub	r15=r15,r16	// subtract exponent bias
	;;

small:	// exponent is zero
	cmp.eq.unc p6,p7=r15,r0
	;;
 (p6)	br.cond.dptk	denorm

	// return -infinity

	addl    r16=@gprel(minf),gp
	;;
 	ldfd	f8=[r16]	// return -infinity
	br.ret.sptk  rp

denorm:	// arg is a denorm
	movl	r15=-1022
	;;
	setf.sig f8=r15		// copy unbiased exponent to fpu
	;;
	fcvt.xf	f8=f8		// convert to floating point
	br.ret.sptk	rp
	;;

infnan:	// arg is an inf or NaN
	fabs	f8=f8		// abs value of arg
	br.ret.sptk  rp
	.endp

	.section .sdata

	.align 8
minf:	data8	0xfff0000000000000

