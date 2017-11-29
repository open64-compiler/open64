/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement
  or the like.  Any license provided herein, whether implied or
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with
  other software, or any other product whatsoever.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

// some const macro used in expand.cxx, exp_loadstore.cxx

#define		MASK8		0xff
#define		MASK16		0xffff
#define		MASK32		0xffffffff
#if defined(__GNUC__)  && (__GNUC__ >= 3) && (__GNUC_MINOR__ >=4)
#define		MASK48		0xffffffffffffULL
#define		MASK64		0xffffffffffffffffULL
#else
#define		MASK48		0xffffffffffffULL
#define		MASK64		0xffffffffffffffffULL
#endif
 
#define		LITERAL_SIZE_2_BYTES		2
#define		LITERAL_SIZE_4_BYTES		4
#define		LITERAL_SIZE_8_BYTES		8

// <<see MIPS run>> section 8.4.1 & MIPS pro's VL WHIRL
#define		MISALIGNED_WORD_ADJUST		3
#define		MISALIGNED_DOUBLE_ADJUST	7

// MAX or MIN value 
#define		MAX_UNSIGNED   	    		-1

// const used in conversion between unsigned LL and float/double
#define		TWO_POWER_63_FLOAT_VALUE 			9223372036854775808.0
#if defined(__GNUC__)  && (__GNUC__ >= 3) && (__GNUC_MINOR__ >=4)
#define		TWO_POWER_63_HEX_VALUE 				0x8000000000000000ULL
#else
#define		TWO_POWER_63_HEX_VALUE 				0x8000000000000000ULL
#endif
#define		TWO_POWER_64_IEEE754_SINGLE_FMT			0x5f800000
#if defined(__GNUC__)  && (__GNUC__ >= 3) && (__GNUC_MINOR__ >=4)
#define		TWO_POWER_64_IEEE754_DOUBLE_FMT			0x43f0000000000000ULL
#else
#define		TWO_POWER_64_IEEE754_DOUBLE_FMT			0x43f0000000000000LL
#endif

