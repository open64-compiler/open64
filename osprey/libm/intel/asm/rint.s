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

	.global errno

	.proc	__rint
	.global	__rint
	.weak	rint
	rint = __rint

	// double __rint(double x)


 // LEAF(__rint)

__rint:
	fcmp.unord.unc.s0 p6,p7=f8,f8
	;;
 (p6)	br.cond.dptk	nanarg		// return if arg is a NaN
	fabs	f9=farg0
	addl    r16=@gprel(twop52),gp
	;;
 	ldfd	f10=[r16]	// load 2**52
	;;
	fcmp.ge.unc.s0	p6,p7=f9,f10
	;;
 (p6)	br.ret.sptk	rp		// return if |x| >= 2**52
	fcvt.fx.s0 f9=f8	// convert to an integer
	;;
	fcvt.xf	f8=f9		// convert back to floating point
	br.ret.sptk	rp
	

nanarg: // arg is a NaN

	// set errno to EDOM

	addl	r16=@ltoff(errno#),gp
	mov	r14=EDOM
	;;
	ld8	r15=[r16]
	;;
	st4	[r15]=r14	// store into errno

	addl    r16=@gprel(qnan),gp
	;;
 	ldfd	f8=[r16]	// return quiet Nan
	br.ret.sptk  rp
	.endp

	.section .sdata

	.align 8
twop52:	data8	0x4330000000000000
qnan:	data8	0x7ff8000000000000

