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

	.proc	__fabsf
	.global	__fabsf
	.weak	fabsf
	fabsf = __fabsf

	// float __fabsf(float x)

	// returns the absolute value of arg

 // LEAF(__fabsf)

__fabsf:
	fcmp.unord.unc.s0 p6,p7=farg0,farg0
	;;
 (p6)	br.cond.dptk	nan		// branch if arg is a NaN

	fabs	f8=farg0
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
qnan:	data4	0x7fc00000

