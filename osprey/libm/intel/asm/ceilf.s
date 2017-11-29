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


#include <asm/errno.h>

	.section .sdata, "a", "progbits"
	.align	8
	.text

	.global	errno

	.proc	__ceilf
	.global	__ceilf
	.weak	ceilf
	ceilf = __ceilf

	// float __ceilf(float x)

	// returns the smallest integer >= x

 // LEAF(__ceilf)

__ceilf:
	getf.s	r14=farg0	// move operand to an integer reg.
	;;
	shr.u	r15=r14,23	// shift off mantissa
	mov	r16=0xff
	;;
	and	r15=r15,r16	// mask off sign bit
	mov	r16=0x7f
	;;
	sub	r15=r15,r16	// subtract exponent bias
	;;
	cmp.lt.unc p6,p7=r15,r0	// test if unbiased exponent < 0
	;;
 (p6)	br.cond.dptk	small		// branch if it is
	mov	r16=0x80
	;;
	cmp.eq.unc p6,p7=r16,r15 // test if exponent == 0x7f8
	;;
 (p6)	br.cond.dptk	infornan	// branch if arg is an inf or Nan
	adds	r15=-23,r15
	;;
	cmp.ge.unc p6,p7=r15,r0
	;;
 (p6)	br.ret.sptk	rp		// return if operand >= 2**23
	sub	r15=r0,r15	// compute shift value
	;;
	shr	r14=r14,r15	// shift off fractional bits
	;;
	shl	r14=r14,r15	// shift back
	;;
	setf.s	f9=r14		// move truncated value to fpu
	;;
	fcmp.le.unc.s0 p6,p7=farg0,f9
	mov	f8=f9		// copy trunc(arg) to f8
	;;
 (p6)	br.ret.sptk	rp		// return trunc(arg) if arg <= trunc(arg)
	fadd.s.s0 f8=f1,f9	// otherwise add 1.0 and return
	br.ret.sptk	rp
	;;

small:	// |arg| < 1.0 

	fcmp.eq.unc.s0 p6,p7=f8,f0
	;;
 (p6)	br.ret.sptk	rp		// return arg if arg == 0.0
	fcmp.lt.unc.s0 p6,p7=f8,f0
	;;
 (p6)	br.cond.dptk	retmzero	// branch if arg < 0.0
	mov	f8=f1
	br.ret.sptk	rp		// return 1.0

retmzero:
	addl	r16=@gprel(mzero),gp
	;;
	ldfs	f8=[r16]	// load -0.0
	br.ret.sptk  rp              // return -0.0
	;;
	
infornan: // arg is INF or NaN

	fcmp.unord.unc.s0 p6,p7=f8,f8
	;;
 (p7)	br.ret.sptk	rp		// return if arg is +/- Inf

	// arg is a NaN

	// set errno to EDOM

	addl	r16=@ltoff(errno#),gp
	mov	r14=EDOM
	;;
	ld8	r15=[r16]
	;;
	st4	[r15]=r14	// store into errno

	addl    r16=@gprel(qnan),gp
	;;
 	ldfs	f8=[r16]	// return quiet Nan
	br.ret.sptk  rp
	.endp

	.section .sdata

	.align 4
mzero:	data4	0x80000000
qnan:	data4	0x7fc00000

