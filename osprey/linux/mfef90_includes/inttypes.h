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

/* ====================================================================
 * ====================================================================
 *
 * Module: inttypes.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 *
 * Revision history:
 *  14-Sep-92 - Original Version
 *  28-Oct-92 - Revisions from 7 October committee meeting.
 *  29-Oct-92 - Minor adjustments based on comments.
 *  26-Feb-93 - Revisions from November committee meeting.
 *  17-Aug-93 - Converted template to SGI-specific form.
 *
 * Description:
 *
 * This header defines types (via typedefs) with size attributes which
 * are independent of the definitions of the base C types provided by
 * the implementation.  It also defines facilities for manipulating
 * objects of those types, i.e. defining constants, printf/scanf
 * support, and conversions.
 *
 * The intent is that, using this header file and the facilities it
 * describes, a user can write C code with fixed-size objects which
 * is interoperable and portable among a broad range of target
 * environments, including 16-bit or 32-bit PCs as well as 32-bit or
 * 64-bit workstations.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef __INTTYPES_H__
#define __INTTYPES_H__

/* The following values encode the current revision of the template.
 * The first "official" release will be 1.0.  Subsequent releases will
 * increment the major ID for significant or incompatible changes, and
 * the minor ID for minor extensions.
 */
#define __INTTYPES_MAJOR	0
#define __INTTYPES_MINOR	1

/* ===========================  TYPES  ============================= */

/* Define the basic fixed-size integer types.  Their intended semantics
 * are to produce objects of precisely the implied size, and to cause
 * (as operands) expression evaluation of either that size or the size
 * of the underlying 'int' type if larger (the normal ANSI C promotion
 * rules).
 *
 * intmax_t and uintmax_t are integer types guaranteed to be the
 * largest supported by the implementation.  They are intended for
 * use in places where code must be able to cope with any integer
 * value.
 *
 * intptr_t and uintptr_t are integer types guaranteed to be exactly
 * the size of a pointer.
 */

#include <standards.h>
#ifndef _LP64
#include <sgidefs.h>

#ifndef __inttypes_INCLUDED
#define __inttypes_INCLUDED
/*
 * These definitions are also in sys/types.h and need to be kept consistent
 */

typedef	signed char		int8_t;	
typedef	unsigned char		uint8_t;
typedef	signed short		int16_t;
typedef	unsigned short		uint16_t;
typedef	signed int		int32_t;
typedef	unsigned int		uint32_t;
typedef	__int64_t           	int64_t;
typedef	__uint64_t            	uint64_t;
typedef __int64_t           	intmax_t;
typedef __uint64_t            	uintmax_t;
typedef signed long int		intptr_t;
typedef unsigned long int	uintptr_t;

#endif
#else
typedef	unsigned char		uint8_t;
typedef	unsigned short		uint16_t;
typedef	unsigned int		uint32_t;
typedef	__uint64_t            	uint64_t;
typedef __int64_t           	intmax_t;
typedef __uint64_t            	uintmax_t;
#endif /* _LP64 */

#if _SGIAPI
/* Define limits for the types, in the manner of limits.h: */
#define INT8_MIN	(-127-1)
#define INT8_MAX	(127)
#define UINT8_MAX	(255u)
#define INT16_MIN	(-32767-1)
#define INT16_MAX	(32767)
#define UINT16_MAX	(65535u)
#define INT32_MIN	(-2147483647-1)
#define INT32_MAX	(2147483647)
#define UINT32_MAX	(4294967295u)
#define INT64_MIN	(-9223372036854775807LL-1)
#define INT64_MAX	(9223372036854775807LL)
#define UINT64_MAX	(18446744073709551615uLL)
#define INTMAX_MIN	(-9223372036854775807LL-1)
#define INTMAX_MAX	(9223372036854775807LL)
#define UINTMAX_MAX	(18446744073709551615uLL)

/* ========================  CONSTANTS  ============================ */

/* Define macros for constants of the above types.  The intent is that:
 *
 *   -	Constants defined using these macros have a specific length and
 *	signedness.
 *
 *   -	Their extensions in contexts requiring extension (e.g.
 *	expressions of greater precision than the specified length)
 *	will be totally determined by that signedness and length.
 *	For example:
 *			INT32_C(0xffffffff)
 *	should define a 32-bit signed negative one, to be sign extended
 *	if the context requires a 64-bit value.  Observe that the
 *	identity macro in an LP64 model would yield a 32-bit unsigned
 *	4 billion something (unsigned int, because the value doesn't
 *	fit in a 32-bit int), whereas in an ILP64 model it would yield
 *	a 64-bit signed 4 billion something (signed int).
 *
 *   -	Use of such a constant will not itself force greater precision
 *	than the specified length.  For example:
 *			UINT32_C(3000000000)
 *	should define a 32-bit unsigned 3 billion, even in an LP64
 *	implementation where the simple constant 3000000000 would be
 *	treated as a signed long 3 billion, forcing the containing
 *	expression to be long.
 */

/* The __CONCAT__ macro may require redefinition for some non-ANSI
 * implementations.  It should concatenate its two arguments without
 * forcing separate tokens.
 */
#define __CONCAT__(A,B) A ## B

#define INT8_C(c)	((int8_t) c)
#define UINT8_C(c)	((uint8_t) __CONCAT__(c,u))
#define INT16_C(c)	((int16_t) c)
#define UINT16_C(c)	((uint16_t) __CONCAT__(c,u))
#define INT32_C(c)	((int32_t) c)
#define UINT32_C(c)	((uint32_t) __CONCAT__(c,u))
#define INT64_C(c)	((int64_t) __CONCAT__(c,ll))
#define UINT64_C(c)	((uint64_t) __CONCAT__(c,ull))
#define INTMAX_C(c)	((int64_t) __CONCAT__(c,ll))
#define UINTMAX_C(c)	((uint64_t) __CONCAT__(c,ull))

/* Constants are not defined for the pointer-sized integers, since the
 * size of the constant value will be known and should be adequate for
 * use in the very rare cases where pointer constants must be defined.
 */

/* ======================  FORMATTED I/O  ========================== */

/* Define extended versions of the printf/scanf libc routines.  These
 * extended routines accept an extended format syntax as follows, and
 * are otherwise identical to the corresponding libc routines.
 *
 * The size specifiers specified by the ANSI standard (i.e. 'h' 'l' and
 * 'L' preceding the conversion specifier) are extended to allow "wNN"
 * instead, indicating that the relevant object has width NN bits, or
 * "wmax" for an intmax_t object.  The valid values of NN are 16, 32,
 * and 64.  For example:
 *
 *	uint16_t u16;
 *	int32_t  s32;
 *	uint64_t u64;
 *	...
 *	i_printf ( "int16 is %w16u; int32 is %#8w32x\n", u16, s32 );
 *	i_scanf ( "%w16o%w64x", &u16, &u64 );
 *
 * The extensions are sequences with explicitly undefined semantics in
 * the ANSI standard, so create no conflicts.
 *
 * The implementation must provide these names as macros, matching the
 * prototypes below.  Therefore, they may be #undefined by a user, and
 * their addresses may NOT be taken.  (The expectation is that they
 * will ultimately be obsoleted by extensions to the underlying ANSI
 * routines' format processing.)
 *
 * The ANSI standard does not specify conversion characters for
 * char-sized integers.  Accordingly, no "w8" modifier is required.
 */
#include <stdarg.h>
#include <stdio.h>

extern int i_fprintf  ( FILE *stream, const char *format, ... );
extern int i_vfprintf ( FILE *stream, const char *format, va_list va );
extern int i_printf   ( const char *format, ... );
extern int i_vprintf  ( const char *format, va_list va );
extern int i_sprintf  ( char *s, const char *format, ... );
extern int i_vsprintf ( char *s, const char *format, va_list va );

extern int i_fscanf   ( FILE *stream, const char *format, ... );
extern int i_scanf    ( const char *format, ... );
extern int i_sscanf   ( char *s, const char *format, ... );

/* ======================  CONVERSIONS  ============================ */

/* The following routines are provided as analogues of the ANSI strtol
 * routines, with analogous semantics.  They must be provided as
 * functions, though the functions may be hidden by macro
 * implementations.  Users may therefore take their addresses, or pass
 * them as actual parameters, by first #undefining the names.
 */
extern	int8_t		strtoi8   (const char *, char **, int);
extern	int16_t		strtoi16  (const char *, char **, int);
extern	int32_t		strtoi32  (const char *, char **, int);
extern	int64_t		strtoi64  (const char *, char **, int);
extern	intmax_t	strtoimax (const char *, char **, int);
extern	uint8_t		strtou8   (const char *, char **, int);
extern	uint16_t	strtou16  (const char *, char **, int);
extern	uint32_t	strtou32  (const char *, char **, int);
extern	uint64_t	strtou64  (const char *, char **, int);
extern	uintmax_t	strtoumax (const char *, char **, int);

/* ======================  ARITHMETIC  ============================ */

/* The following routines are provided as analogues of the ANSI stdlib
 * routines, with analogous semantics.  They must be provided as
 * functions, though the functions may be hidden by macro
 * implementations.  Users may therefore take their addresses, or pass
 * them as actual parameters, by first #undefining the names.
 */
extern	int32_t 	abs_32	( int32_t );
extern	int64_t 	abs_64	( int64_t );
extern	intmax_t 	abs_max	( intmax_t );

typedef struct { int32_t quot;  int32_t rem;  } div32_t;
typedef struct { int64_t quot;  int64_t rem;  } div64_t;
typedef struct { intmax_t quot; intmax_t rem; } divmax_t;
extern	div32_t  div_32	 ( int32_t numer,  int32_t denom );
extern	div64_t  div_64	 ( int64_t numer,  int64_t denom );
extern	divmax_t div_max ( intmax_t numer, intmax_t denom );
#endif /* _SGIAPI */

#endif /* __INTTYPES_H__ */
