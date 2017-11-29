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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/linux/mfef90_includes/cmplrs/host.h,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#ifndef _HOST_H
#define _HOST_H
/*
**  host.h
**
**  Basic type declarations, macros, ... to promote reuse and
**  portability throughout the compiler. 
**
**  Include this file before all others.
*/

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))

typedef int boolean;
#ifndef TRUE
#define TRUE	(1)
#define FALSE	(0)
#endif

typedef char *string;
typedef char char_t;
#ifndef __sgi
/* gb - sys/types.h defines these types, which is included by <signal.h> */
typedef unsigned char uchar_t;
#endif 
typedef signed short short_t;
#ifndef __sgi
typedef unsigned short ushort_t;
#endif
typedef signed int int_t;
#ifndef __sgi
typedef unsigned int uint_t;
#endif
typedef signed long long_t;
#ifndef __sgi
typedef unsigned long ulong_t;
#else
#include <sys/types.h>
#endif

#if defined(_LONGLONG)
typedef signed long long longlong_t;
typedef unsigned long long ulonglong_t;
#else
typedef signed long longlong_t;
typedef unsigned long ulonglong_t;
#endif

typedef signed char int8;
typedef unsigned char uint8;
typedef signed short int16;
typedef unsigned short uint16;
typedef signed int int32;
typedef unsigned int uint32;
typedef longlong_t int64;
typedef ulonglong_t uint64;

typedef void *pointer;          /* a generic pointer type */
typedef double double_t;
typedef float float_t;
typedef int32 fsize_t; /* Size of a "hidden length" when passing Fortran CHARACTER arguments */
/* Another reasonable choice:  (requires <string.h>)
**    typedef size_t fsize_t;
*/


#endif

#if defined(_LANGUAGE_PASCAL)

#if defined(_LONGLONG)
type long_integer = integer64;
type long_cardinal = cardinal64;
#else
type long_integer = integer;
type long_cardinal = cardinal;
#endif
#endif

#endif

