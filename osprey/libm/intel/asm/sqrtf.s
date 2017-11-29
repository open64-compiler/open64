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

	.proc	__sqrtf
	.global	__sqrtf
	.weak	sqrtf
	sqrtf = __sqrtf

	.weak	fsqrt
	fsqrt = __sqrtf

	// float sqrtf( float x )

 // LEAF(__sqrtf)

__sqrtf:
	fcmp.unord.unc.s0 p6,p7=f8,f8
	;;
 (p6)	br.cond.dptk		nan	// branch if arg is a NaN
	;;
	frsqrta.s0	f13,p6=farg0	// y2 = ~1/sqrtf(x)
	;;

 (p6)	addl    r16=@gprel(half),gp
	;;
 (p6)	ldfs	f11=[r16],4	// f11 = 0.5
	;;
 (p6)	ldfs	f14=[r16]	// f14 = 0x3f000001
	;;
 (p6)	fmpy.s1	f10=farg0,f13	// g = x*y2
 (p6)	fmpy.s1	f9=f11,f13	// y = 0.5*y2
	;;
 (p6)	fnma.s1	f12=f10,f10,farg0 // d = x - g*g
	;;
 (p6)	fma.s1	f10=f9,f12,f10	// g = g + y*d   16 bit approximation
	;;
 (p6)	fnma.s1	f15=f9,f10,f14	// e = ah - y*g
 (p6)	fnma.s1	f12=f10,f10,farg0 // d = x - g*g
	;;
 (p6)	fma.s1	f9=f15,f13,f9	// y = y + e*y2
	;;
 (p6)	fma.s1	f10=f9,f12,f10   // g = g + y*d   32 bit approximation
	;;
 (p6)	fadd.s1	f13=f9,f9      // y2 = y + y
	;;
 (p6)	fnma.s1	f15=f9,f10,f14	// e = ah - y*g
 (p6)	fnma.s1	f12=f10,f10,farg0 // d = x - g*g
	;;
 (p6)	fma.s1	f9=f15,f13,f9	// y = y + e*y2
	;;
 (p6)	fma.s.s0	f8=f9,f12,f10  // result = g + y*d

 	br.ret.sptk	rp

nan:	// arg is a NaN

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
half:	data4	0x3f000000
ah:	data4	0x3f000001
qnan:	data4	0x7fc00000

