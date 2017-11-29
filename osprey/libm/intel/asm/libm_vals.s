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

#ifdef __ia64
	.sdata
	.align 4
	.global __libm_qnan_d
__libm_qnan_d:
	data4	0x0,0x7FF80000
	.global __libm_inf_d
__libm_inf_d:
	data4	0x0,0x7FF00000
	.global __libm_neginf_d
__libm_neginf_d:
	data4	0x0,0xFFF00000
	.global __libm_qnan_f
__libm_qnan_f:
	data4	0x7FC00000
	.global __libm_inf_f
__libm_inf_f:
	data4	0x7F800000
	.global __libm_neginf_f
__libm_neginf_f:
	data4	0xFF800000

#endif

#ifdef __ia32
	.section .sdata
	.align 4
	.global __libm_qnan_d
__libm_qnan_d:
	.long	0x0,0x7FF80000
	.global __libm_inf_d
__libm_inf_d:
	.long	0x0,0x7FF00000
	.global __libm_neginf_d
__libm_neginf_d:
	.long	0x0,0xFFF00000
	.global __libm_qnan_f
__libm_qnan_f:
	.long	0x7FC00000
	.global __libm_inf_f
__libm_inf_f:
	.long	0x7F800000
	.global __libm_neginf_f
__libm_neginf_f:
	.long	0xFF800000

#endif
