/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* USMID @(#) clibinc/cray/portdefs.h	92.5	10/29/99 21:41:22 */


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

#ifndef _PORTDEFS_H
#define _PORTDEFS_H

#if	defined(__mips)
#include <sgidefs.h>
#elif	defined(_UNICOS)
#include <sys/param.h>
#include <sys/types.h>
#ifdef	_CRAYT3E
#define _UINT64_TYPEDEF 1
#elif	RELEASE_LEVEL >= 9100
#define _UINT64_TYPEDEF 1
#endif
#elif	defined(_SOLARIS)
#include <cray/solaris.h>
#endif

/*
 *	Handy targetting macro definitions.
 */

#include <limits.h>

#define BYTES_PER_WORD	sizeof(int)		/* word size in bytes */
#define BITS_PER_WORD	(BYTES_PER_WORD * CHAR_BIT) /* word size in bits */
#define _BYTES_PER_LONG	sizeof(long)		/* size of long in bytes */
#define _BITS_PER_LONG	(_BYTES_PER_LONG * CHAR_BIT) /* size of long in bits */

#if	defined(_CRAYIEEE) || !defined(_UNICOS)
#define IEEE_FLOATING_POINT			/* IEEE floating point */
#endif

/* these are for a *1 and a *2 integer and logical type size */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
typedef	signed char		int8;		/* 8-bit integer */
typedef	unsigned char		uint8;		/* 8-bit unsigned integer */
typedef short			int16;		/* 16-bit integer */
typedef unsigned short		uint16;		/* 16-bit unsigned integer */
#endif

/*
 *	Typedefs	These typedefs are for use in situations where
 *			a minimum precision is required, irrespective
 *			of the targetted system.
 */

#if	defined(_LITTLE_ENDIAN)

typedef int			int32;		/* 32-bit integer */
typedef unsigned int		uint32;		/* 32-bit unsigned integer */

#if	defined(_LP64)
typedef	long long		int64;		/* 64-bit integer */
typedef	unsigned long long	uint64;		/* 64-bit unsigned integer */
#else
typedef	long long		int64;		/* 64-bit integer */
typedef	unsigned long long	uint64;		/* 64-bit unsigned integer */
#endif

#elif	_MIPS_SZLONG == 64 			/* ---- MIPS 64-bit ---- */

typedef	long long		int64;		/* 64-bit integer */
typedef	unsigned long long	uint64;		/* 64-bit unsigned integer */
typedef int			int32;		/* 32-bit integer */
typedef unsigned int		uint32;		/* 32-bit unsigned integer */


#elif	defined(_WORD32) 			/* ---- MIPS n32,   ---- */
						/* ---- Solaris,    ---- */
						/* ---- and  ABSOFT ---- */

typedef	long long		int64;		/* 64-bit integer */
typedef	unsigned long long	uint64;		/* 64-bit unsigned integer */
typedef int			int32;		/* 32-bit integer */
typedef unsigned int		uint32;		/* 32-bit unsigned integer */


#elif	defined(_CRAY1) || defined(_CRAYMPP)	/* ---- CRAY PVP and MPP ---- */

typedef long                    int64;          /* 64-bit integer */
#if	!_UINT64_TYPEDEF
#define  _UINT64_TYPEDEF 1
typedef unsigned long           uint64;         /* 64-bit unsigned integer */
#endif
typedef short			int32;		/* 32-bit integer */
typedef unsigned short		uint32;		/* 32-bit unsigned integer */

#endif	

/*
 *	Macros
 */
#if defined(_CRAY1) || defined(_CRAYMPP)	/* CRAY PVP and MPP */
#define _PRAGMA_INLINE_2(name)	_Pragma(#name)
#define _PRAGMA_INLINE(name)	_PRAGMA_INLINE_2(inline name)
#else
#define _PRAGMA_INLINE(name)
#endif

#ifndef	_UNICOS

/*
 *	CRI-extension constants which are missing in portable POSIX/ANSI
 *	C header files.
 */

#define EFLEFIRST	1000		/* from errno.h */
#define EFLELAST	1999		/* from errno.h */

/*
 *	CRI-extension function prototypes which are missing in portable 
 *	POSIX/ANSI C header files.
 */

#include <sys/types.h>	/* for size_t */

extern char *strnrstrn(const char *_s1, size_t _n1, const char *_s2,
		       size_t _n2);		/* from string.h */

extern char *strnstrn(const char *_s1, size_t _n1, const char *_s2,
		      size_t _n2);		/* from string.h */

/*
 *	Some CRI C compiler extension intrinsic functions are implemented as
 *	ordinary functions. 
 */

#define INTRINSICS_PARM_TYPE	unsigned long

extern int	_count(INTRINSICS_PARM_TYPE x);
#if	!defined(__mips) || \
	(defined(__mips) && !(defined(__INLINE_INTRINSICS) && \
	defined(_COMPILER_VERSION) && (_COMPILER_VERSION>= 730)))
/* prototype for functions when not compiler intrinsic */
extern long	_dshiftl(INTRINSICS_PARM_TYPE x, INTRINSICS_PARM_TYPE y, int b);
extern long	_dshiftr(INTRINSICS_PARM_TYPE x, INTRINSICS_PARM_TYPE y, int b);
extern int	_leadz(INTRINSICS_PARM_TYPE x);
extern long	_mask(int n);
extern long	_maskr(int n);
extern long	_maskl(int n);
#endif

#ifdef	__mips
extern void
movbit_(void *a, long long *az, long long  *nz, void *b, long long *bz);
extern void
movbitz_(void *a, long long *az, long long *nz, void *b, long long *bz);
extern long	_pack(long *up, char *cp, long bc, int tc);
extern long	_unpack(char *cp, long *up, long bc, int tc);
#elif defined(_LITTLE_ENDIAN)
extern void
movbit_(void *a, int64 *az, int64 *nz, void *b, int64 *bz);
extern void
movbitz_(void *a, int64 *az, int64 *nz, void *b, int64 *bz);
extern long	_pack(long *up, char *cp, long bc, int tc);
extern long	_unpack(char *cp, long *up, long bc, int tc);
#endif


/*
 *	CRI extensions may be permitted (and deactivated) in portable code 
 *	through use of these special definitions.
 */

#define restrict			/* SCC type qualifier extension */
#define _gsys_qtape(x)	(0)		/* CRI online tape support */
#define O_RAW		0x0		/* fcntl.h flag for open(2) */

#ifdef	_SOLARIS
#define LOADED(x)	(x != NULL)	/* Solaris weak externals */
#define LOADED_DATA(x)	(x != NULL)	/* Solaris weak externals */
#else
#define LOADED(x)	(1)		/* CRI soft external symbols */
#define LOADED_DATA(x)	(1)		/* CRI soft external symbols */
#endif

#endif	/* ! _UNICOS */

/*
 * naming conventions, used in libfi/matrix routines 
 */
#undef UNIX_NAMING_CONVENTION 

/*
 * complex result convention, used in libfi/matrix routines 
 * NB: using LITTLE_ENDIAN as synonym for linux here..
 *
 */

#if !defined(_SOLARIS) && !defined(__mips) && !defined(_ABSOFT) && !defined(_LITTLE_ENDIAN)
#define COMPLEX_AS_SCALAR
#else
#undef COMPLEX_AS_SCALAR
#endif

#endif /* _PORTDEFS_H */
