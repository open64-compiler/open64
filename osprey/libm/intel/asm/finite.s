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

	.proc	__finite
	.global	__finite
	.weak	finite
	finite = __finite

	// int __finite(double x)

	// returns 1 if arg is neither infinity nor NaN,
	// 0 otherwise

 // LEAF(__finite)

__finite:
	getf.d	r14=farg0	// move operand to an integer reg.
	;;
	shr.u	r15=r14,52	// shift off mantissa
	mov	r16=0x7ff
	;;
	and	r15=r15,r16	// mask off sign bit
	;;
	cmp.lt.unc p6,p7=r15,r16 // test if exponent == 0x7ff
	mov	r8=0		// default is 0
	;;
 (p6)	mov	r8=1
	br.ret.sptk	rp

