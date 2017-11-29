/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */


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

/*
 * ==========================================================================
 *
 * Module   : unwind_ia64.h -- IA64 unwind descriptor header file
 *	      Do not include directly, use 
 *		- unwind.h for consumer interface
 *		- unwindP.h for producer interface
 * ==========================================================================
 */

#ifndef __SYS_IA64_UNWIND_IA64_H
#define __SYS_IA64_UNWIND_IA64_H

#include <inttypes.h>

/* unwind version */
#define __UNW_VERSION			0x01

/* unwind generic return values */
#define __UNW_OK			0x00
#define __UNW_INTERNAL_ERROR		0x01
#define __UNW_MALLOC_ERROR		0x02
#define __UNW_REALLOC_ERROR		0x03
#define __UNW_MMAP_ERROR		0x04
#define __UNW_SYSCALL_ERROR		0x05
#define __UNW_NULL_ERROR		0x06
#define __UNW_INV_ARG_ERROR		0x07
#define __UNW_INV_SIZE_ERROR		0x08
#define __UNW_INV_OP_ERROR		0x09
#define __UNW_INV_ALIGNMENT_ERROR	0x0a
#define __UNW_INV_VERSION_ERROR		0x0b
#define __UNW_NOT_FOUND_ERROR		0x0c

/* unwind generic types */
#if defined(_ABI32) || defined(TARG_SL)
typedef uint32_t __unw_addr_t;
#else /* _ABI64 */
typedef uint64_t __unw_addr_t;
#endif
typedef uint64_t __unw_dbl_word_t;
typedef uint32_t __unw_word_t;
typedef int32_t __unw_error_t;

/* unwind table entry records */
typedef struct __unw_table_entry_struct {
	__unw_addr_t _start;	/* segment-relative region starting address */
        __unw_addr_t _end;	/* segment-relative region ending address */
        __unw_addr_t _info;	/* pointer to unwind info */
} __unw_table_entry_t;

/* unwind info records/blocks */
typedef struct __unw_info_struct {
        __unw_dbl_word_t _header;	/* unwind info header */
        char _body[1];		 	/* unwind info body */
} __unw_info_t;

/* access macros for unwind descriptors for region headers */
/* (where x is the leading byte) */
#define __UNW_IS_HEADER(x) (!((x) & 0x80))
/* access macros for unwind descriptors for prologue region headers */
/* (where x is the leading byte) */
#define __UNW_IS_PROLOGUE_HEADER(x) ((0x00 == ((x) & 0xe0)) \
				|| (0x40 == ((x) & 0xf8)) \
				|| (0x60 == (x)))
/* access macros for unwind descriptors for body region headers */
/* (where x is the leading byte) */
#define __UNW_IS_BODY_HEADER(x) ((0x20 == ((x) & 0xe0)) \
				|| (0x61 == (x)))
/* access macros for all unwind descriptors */
/* (where x is the leading byte) */
#define __UNW_IS_R1(x) (0x00 == ((x) & 0xc0))
#define __UNW_IS_R2(x) (0x40 == ((x) & 0xe0))
#define __UNW_IS_R3(x) (0x60 == ((x) & 0xe0))
#define __UNW_IS_P1(x) (0x80 == ((x) & 0xe0))
#define __UNW_IS_P2(x) (0xa0 == ((x) & 0xf0))
#define __UNW_IS_P3(x) (0xb0 == ((x) & 0xf8))
#define __UNW_IS_P4(x) (0xb8 == ((x) & 0xff))
#define __UNW_IS_P5(x) (0xb9 == ((x) & 0xff))
#define __UNW_IS_P6(x) (0xc0 == ((x) & 0xe0))
#define __UNW_IS_P7(x) (0xe0 == ((x) & 0xf0))
#define __UNW_IS_P8(x) (0xf0 == ((x) & 0xff))
#define __UNW_IS_P9(x) (0xf1 == ((x) & 0xff))
#define __UNW_IS_P10(x) (0xff == ((x) & 0xff))
#define __UNW_IS_B1(x) (0x80 == ((x) & 0xc0))
#define __UNW_IS_B2(x) (0xc0 == ((x) & 0xe0))
#define __UNW_IS_B3(x) (0xe0 == ((x) & 0xff))
#define __UNW_IS_B4(x) (0xf0 == ((x) & 0xf7))
#define __UNW_IS_X1(x) (0xf9 == ((x) & 0xff))
#define __UNW_IS_X2(x) (0xfa == ((x) & 0xff))
#define __UNW_IS_X3(x) (0xfb == ((x) & 0xff))
#define __UNW_IS_X4(x) (0xfc == ((x) & 0xff))
/* access macros for unwind info records/blocks */
/* (where x is the leading double word) */
#define __UNW_VER(x) ((x) >> 48)
#define __UNW_FLAG_EHANDLER(x) ((x) & 0x0000000100000000LL)
#define __UNW_FLAG_UHANDLER(x) ((x) & 0x0000000200000000LL)
#define __UNW_LENGTH(x) ((x) & 0x00000000ffffffffLL)

/* ========================================================================== */

#ifdef __cplusplus
extern "C" {
#endif

/* general API */

/* little-endian base 128 variable-length decoding */
uint64_t __leb128_decode(char *, uint64_t, uint64_t *);
/* little-endian base 128 variable-length encoding */
uint64_t __leb128_encode(char *, uint64_t, uint64_t);
/* unwind table and/or unwind info processing */
__unw_error_t unwind_process(__unw_error_t (*)(char *, uint64_t, char *,
				uint64_t, void *), void *);
/* unwind table and/or unwind info dumping into ascii */
__unw_error_t unwind_dump2ascii(char *, uint64_t,
				char *, uint64_t, void *);


#ifdef __cplusplus
}
#endif

#endif
