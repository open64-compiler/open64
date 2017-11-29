/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef defs_INCLUDED
#define defs_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: defs.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/defs.h,v $
 *
 * Revision history:
 *  21-Aug-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *
 * Description:
 *
 * This header file contains definitions of ubiquitous configuration
 * parameters used in the microcode compiler, in particular mappings
 * of the host-independent type names used to the host-supported basic
 * types.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *defs_rcs_id = "$Source: /home/bos/bk/kpro64-pending/common/com/SCCS/s.defs.h $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#define PROTOTYPES 1
/* We always want Insist error messages: */
#define Insist_On 1

/* ====================================================================
 *
 * System libraries to be made generally available
 *
 * ====================================================================
 */

/* Make stdio, and string support generally available: */
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <bstring.h>	/* for bzero */

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#include <stdint.h>
#include <limits.h>

#ifdef __cplusplus
extern "C" {
#endif


/* Since <signal.h> doesn't prototype signal, upsetting ccom: */
/* extern void (*signal(int, void (*) (int, ...)))(int, ...); */


/* ====================================================================
 *
 * Characterize the host machine
 *
 * ====================================================================
 */

#ifdef __sgi
# define HOST_SGI
#endif

/***** Note the size of a word (in malloc units): *****/
#ifndef HOST_WORD_SIZE
# define HOST_WORD_SIZE	4
#endif
#define WORDSIZE HOST_WORD_SIZE	/* OBSOLETE */
#if HOST_WORD_SIZE == 2
# define TWO_BYTE_WORDS
#endif
#if HOST_WORD_SIZE == 4
# define FOUR_BYTE_WORDS
#endif
#if HOST_WORD_SIZE == 8
# define EIGHT_BYTE_WORDS
#endif
#if defined(BUILD_OS_DARWIN)
# define BITSPERBYTE	CHAR_BIT
#endif /* defined(BUILD_OS_DARWIN) */

/* Map low indices to low-order bits in the bit vector package: */
#define BV_LITTLE_ENDIAN_BIT_NUMBERING	1

/* Should bit vector packages use table lookup instead of shifts? */

/* ====================================================================
 *
 * The compiler process being compiled
 *
 * The command line must specify one of -DDRIVER, -DFRONT_END_xxx, or
 * -DBACK_END.  The following sets related defines based on those.
 *
 * NOTE: The original Josie sources sometimes used aliases C_FRONT_END
 * for FRONT_END_C, FETYPE_CH for F77_FRONT_END, COMMON_CORE for
 * BACK_END, and ONE_PROC for SINGLE_PROCESS.  These have all been
 * eliminated except for uses of FETYPE_CH specifically to refer to
 * the FORTRAN front end's CHARACTER type.
 *
 * ====================================================================
 */

#define	COMPILER_DRIVER	1
#define	COMPILER_FE_C	2
#define	COMPILER_FE_CC	3
#define	COMPILER_FE_F77	4
#define	COMPILER_FE_F90	5
#define COMPILER_BE	6
#define COMPILER_TDGEN	7

#ifdef DRIVER
# define COMPILER_PROCESS COMPILER_DRIVER
# define COMPILER 1
#endif
#if defined(FRONT_END_C)
# define COMPILER_PROCESS COMPILER_FE_C
# define FRONT_END 1
# define COMPILER 1
#endif
#if defined(FRONT_END_CPLUSPLUS)
# define COMPILER_PROCESS COMPILER_FE_CC
# define FRONT_END 1
# define COMPILER 1
#endif
#ifdef FRONT_END_F77
# define COMPILER_PROCESS COMPILER_FE_F77
# define FRONT_END_FORTRAN 1
# define FRONT_END 1
# define COMPILER 1
#endif
#ifdef FRONT_END_F90
# define COMPILER_PROCESS COMPILER_FE_F90
# define FRONT_END_FORTRAN 1
# define FRONT_END 1
# define COMPILER 1
#endif
#ifdef BACK_END
# define COMPILER_PROCESS COMPILER_BE
# define COMPILER 1
#endif
#ifdef TDGEN
# define COMPILER_PROCESS COMPILER_TDGEN
#endif

/* Are we compiling front end and back end as a single process? */
#if defined(FRONT_END) && defined(BACK_END)
# define SINGLE_PROCESS 1
#endif


/* Allow inline keyword, making it 'static' for debugging if Is_True_On */
#ifdef _LANGUAGE_C
#ifndef inline
#ifdef Is_True_On
#define inline static
#else
#define inline static __inline
#endif
#endif
#endif

#ifdef __GNUC__
/* gcc allows us to put attribute weak after a prototype */
#define WEAK __attribute__((weak))
#else
#define WEAK
#endif


/* ====================================================================
 *
 * Type mapping
 *
 * The following type names are to be used in general to avoid host
 * dependencies.  Two sets are provided.  The first, without a prefix,
 * specifies a minimum bit length for the object being defined of 8,
 * 16, or 32 bits.  It is to be interpreted as a host-efficient type
 * of at least that size.  The second, with a "m" prefix, also gives
 * a minimum bit length, but that bit length is preferred (minimizing
 * memory usage is the priority) if remotely reasonable on the host.
 * The latter types should be used only for objects which will be
 * replicated extensively.
 *
 * ====================================================================
 */

#if (defined(HOST_SGI) || defined(__GNUC__))
typedef signed int	INT;	/* The natural integer on the host */
typedef signed int	INT8;	/* Use the natural integer */
typedef signed int	INT16;	/* Use the natural integer */
typedef signed int	INT32;	/* The natural integer matches */
typedef signed long long INT64;	
typedef unsigned long	INTPTR;	/* Integer the same size as pointer*/
typedef unsigned int	UINT;	/* The natural integer on the host */
typedef unsigned int	UINT8;	/* Use the natural integer */
typedef unsigned int	UINT16;	/* Use the natural integer */
typedef unsigned int	UINT32;	/* The natural integer matches */
typedef unsigned long long UINT64;
typedef int		BOOL;	/* Natural size Boolean value */
typedef signed char	mINT8;	/* Avoid - often very inefficient */
typedef signed short	mINT16;	/* Use a 16-bit integer */
typedef signed int	mINT32;	/* The natural integer matches */
typedef signed long long mINT64;
typedef unsigned char	mUINT8;	/* Use the natural integer */
typedef unsigned short	mUINT16;/* Use a 16-bit integer */
typedef unsigned int	mUINT32;/* The natural integer matches */
typedef unsigned long long mUINT64;
typedef unsigned char	mBOOL;	/* Minimal size Boolean value */

/* Define largest efficient integers for the host machine: */
typedef signed long	INTSC;	/* Scaled integer */
typedef unsigned long	UINTSC;	/* Scaled integer */

/* Define pointer-sized integers for the host machine: */
typedef signed long	INTPS;	/* Pointer-sized integer */
typedef unsigned long	UINTPS;	/* Pointer-sized integer */

#define	INTSC_MAX	INT32_MAX	/* Max scaled int */
#define	INTSC_MIN	INT32_MIN	/* Min scaled int */
#define	UINTSC_MAX	UINT32_MAX	/* Max scaled uint */
#define	INTPS_MAX	INT32_MAX	/* Max pointer-sized int */
#define	INTPS_MIN	INT32_MIN	/* Min pointer-sized int */
#define	UINTPS_MAX	UINT32_MAX	/* Max pointer-sized uint */

/* Define quad-precision floating point for the host machine.
 * WARNING: Depending on the host, this type need not be usable.
 * Instead, see QUAD_TYPE in targ_const.h and its reference to
 * HOST_SUPPORTS_QUAD_FLOAT in config_host.h.  We do this this way to
 * avoid needing to allow the standard type names whenever
 * targ_const.h is included.
 */
#if (defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 400) && _SGIAPI) || defined(__GNUC__)
#define HOST_SUPPORTS_QUAD_FLOAT 1
#else
#define HOST_SUPPORTS_QUAD_FLOAT 0
#endif

#ifndef TARG_LOONGSON
#if HOST_SUPPORTS_QUAD_FLOAT
/* Temporarily remove this to get rid of warnings: */
typedef long double	QUADFP;		/* 128-bit floating point */
#else 
typedef double	QUADFP;		/* 128-bit floating point */
#endif
#else
typedef struct { INT32 qval[4]; } QUADFP;
#endif

#endif /* HOST_SGI || __GNUC__ */

/* We would like a generic memory pointer type, e.g. for use in the
 * memory allocation routines.  Ideally, it is (void *), but some
 * hosts, e.g. ca. 1988 MIPS, can't handle that...
 */
#if(defined(HOST_SGI) || defined(__GNUC__))
typedef void	*MEM_PTR;
#endif /* HOST_SGI || __GNUC__*/

/* Short hand for those who don't like "char *" */
typedef char *STRING;

/* Define the target's basic INT type: */
/* WARNING:  This isn't quite accurate.  A single compiler may
 * support multiple targets with multiple possibilities for these
 * types.  They should be viewed as maximal for the supported
 * targets.
 * TODO:  They aren't there yet (e.g. the 64-bit targets).
 * Determine how to deal with this -- maybe a completely different
 * mechanism is required.
 */
typedef INT64	TARG_INT;	/* Individual objects */
typedef mINT64	mTARG_INT;	/* Table components */
typedef UINT64  TARG_UINT;
typedef mUINT64 mTARG_UINT;

/* Define standard values: */
#ifndef TRUE
#define TRUE	((BOOL) 1)
#endif
#ifndef FALSE
#define FALSE	((BOOL) 0)
#endif

/* Something to use to represent undefined positive integers.  Perahps we
 * could generalize this somehow, but it is useful the way it is.
 */
#ifndef UNDEFINED
#define UNDEFINED -1
#endif

/* Finally, eliminate the standard type names to prevent their
 * accidental use.  We must, however, allow this to be overridden for
 * files which need them, e.g. to use /usr/include expansions which
 * require them.
 */



/* ====================================================================
 *
 * Ubiquitous compiler types and macros
 *
 * We want several compiler standard types to be universally available
 * to allow their use without including their natural defining header
 * files.
 *
 * ====================================================================
 */

/* Define the general TDT table index type: */
typedef UINT16	CLASS_INDEX;	/* Individual objects */
typedef mUINT16	mCLASS_INDEX;	/* Table components */

/* Define the PREG offset type: */
typedef INT32 PREG_NUM;		/* Individual objects */
typedef mINT32 mPREG_NUM;	/* Table components */

/* Define the IDTYPE used by wopt */
typedef mUINT32 IDTYPE;

/* Define standard functions: */
#ifdef MAX
# undef MAX
#endif
#ifdef MIN
# undef MIN
#endif
#define MAX(a,b)	((a>=b)?a:b)
#define MIN(a,b)	((a<=b)?a:b)

#ifdef KEY
#ifdef Is_True_On
static
#else
static __inline__ /* GNU 4.2 does not support non-static C99 inline functions. */
#endif
#else
inline
#endif
INT Max(INT i, INT j)
{
  return MAX(i,j);
}

#ifdef KEY
#ifdef Is_True_On
static
#else
static __inline__ /* GNU 4.2 does not support non-static C99 inline functions. */
#endif
#else
inline
#endif
INT Min(INT i, INT j)
{
  return MIN(i,j);
}


/* --------------------------------------------------------------------
 * VERY_BAD_PTR
 * a memory address that is almost certainly going to cause 
 * a SIG. Many pointer values are initialized to this value to
 * catch any uninitialized uses of that pointer.
 *
 * Actually this could be host dependent, but the value below 
 * is good enough for all current hosts
 * --------------------------------------------------------------------
 */
#define VERY_BAD_PTR (0xfffffff)

/* mingw uses %I64 to print a long long rather than %ll */
#ifdef __MINGW32__
#define LL_FORMAT "I64"
#else
#define LL_FORMAT "ll"
#endif


#ifdef __cplusplus
}
#endif
#endif /* defs_INCLUDED */
