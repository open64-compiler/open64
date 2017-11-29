
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

#ifndef __SGIDEFS_H__
#define __SGIDEFS_H__

#ifdef __cplusplus
extern "C" {
#endif

#ident "$Revision: 1.1.1.1 $"

/*
 * sgidefs.h - SGI/MIPS basic software generation system constants & types
 *
 * This file MUST be includable in any language
 * and in the 'C' case must be ANSI compliant
 * In particular this means NO long long ...
 *
 * Constants and types defined here are to support
 * multi-isa (instruction set architecture) coding
 *
 * Each catagory has a define that the compilation system will set
 * based on the environment the compile is initiated in. Programs
 * can test the define using the manifest constants defined here.
 * (e.g. #if (_MIPS_FPSET == 16) )
 */

/*
 * Floating Point register set
 * Define:
 *	_MIPS_FPSET
 * Can take on the values 16 or 32
 */

/*
 * Instruction Set Architecture
 * Define:
 *	_MIPS_ISA
 */
#define _MIPS_ISA_MIPS1	1	/* R2/3K */
#define _MIPS_ISA_MIPS2	2	/* R4K/6K */
#define _MIPS_ISA_MIPS3	3	/* R4K */
#define _MIPS_ISA_MIPS4	4	/* TFP */

/*
 * Subprogram Interface Model
 * Define:
 *	_MIPS_SIM
 */
#define _MIPS_SIM_ABI32		1	/* MIPS MSIG calling convention */
#define _MIPS_SIM_NABI32	2	/* MIPS new 32-bit abi */
		/* NABI32 is 64bit calling convention but 32bit type sizes) */
#define _MIPS_SIM_ABI64		3	/* MIPS 64 calling convention */

/*
 * Data Types Sizes (C and C++)
 * Defines:
 *	_MIPS_SZINT
 *	_MIPS_SZLONG
 *	_MIPS_SZPTR
 *
 * These can take on the values: 32, 64, 128
 */

/*
 * Compilation Environments
 *	The compiler can offer a set of different compilation environments.
 *	Each one will pre-define the above defines appropriately.
 * 	If you use the _MIPS_* defines, you should include this header file.
 * 	In order to avoid problems when sgidefs.h is not included, the driver
 * 	predefines _MIPS_ISA and _MIPS_SIM to be the actual values (1-4) 
 *	rather than the _MIPS_* names (otherwise, if sgidefs.h is not 
 *	included, we would be defining _MIPS_SIM to be an undefined value).
 * 
 * The following #if conditions will be true in each environment:
 * 	The MIPS ABI (-o32) environment:
 * 	_MIPS_ISA == _MIPS_ISA_MIPS1 or _MIPS_ISA_MIPS2
 *	_MIPS_SIM == _MIPS_SIM_ABI32
 *	_MIPS_FPSET == 16
 *	_MIPS_SZINT == 32
 *	_MIPS_SZLONG == 32
 *	_MIPS_SZPTR == 32
 *
 * 	The new MIPS 32-bit ABI (-n32) environment:
 * 	_MIPS_ISA == _MIPS_ISA_MIPS3 or _MIPS_ISA_MIPS4
 *	_MIPS_SIM == _MIPS_SIM_NABI32
 *	_MIPS_FPSET == 32
 *	_MIPS_SZINT == 32
 *	_MIPS_SZLONG == 32
 *	_MIPS_SZPTR == 32
 *
 * 	The MIPS 64-bit ABI (-64) environment:
 * 	_MIPS_ISA == _MIPS_ISA_MIPS3 or _MIPS_ISA_MIPS4
 *	_MIPS_SIM == _MIPS_SIM_ABI64
 *	_MIPS_FPSET == 32
 *	_MIPS_SZINT == 32
 *	_MIPS_SZLONG == 64
 *	_MIPS_SZPTR == 64 
 */

/*
 * Language Specific
 * Type __psint_t - a pointer sized int - this can be used:
 *	a) when casting a pointer so can perform e.g. a bit operation
 *	b) as a return code for functions incorrectly typed as int but
 *	   return a pointer.
 * User level code can also use the ANSI std ptrdiff_t, defined in stddef.h
 *	in place of __psint_t
 * Type __scint_t - a 'scaling' int - used when in fact one wants an 'int'
 *	that scales when moving to say 64 bit. (e.g. byte counts, bit lens)
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))

/*
 * assumes int is 32 -
 * otherwise there must be some other compiler basic type
 */
#if (_MIPS_SZINT != 32)
#ifdef _MIPS_SZINT
ERROR -- the macro "_MIPS_SZINT" is set to _MIPS_SZINT -- should be 32
#else
ERROR -- the macro "_MIPS_SZINT" is unset (currently, must be set to 32)
#endif
#endif

typedef int __int32_t;
typedef unsigned  __uint32_t;

#if (_MIPS_SZLONG == 64)

typedef long __int64_t;
typedef unsigned long __uint64_t;

#else

/*
 *	64-bit integer types
 *
 *  (1)	In Irix6.1, the compiler implements a new internal type
 *	called  __long_long.   It's purpose is to allow SGI to
 *	define  __int64_t  and __uint64_t  without using
 *	long long or structs or unions.  
 *
 *	The reason for this is that several types in Irix are
 *	being (or will be) promoted to 64-bit integers and will
 *	be typedef'd as either __int64_t  or __uint64_t.  If
 *	these types are defined using  long long, then ANSI 
 *	requires the C compiler to complain because long long
 *	is not standard conforming.  If these types are defined
 *	using a struct or a union, then the programmer can not
 *	use them in arithmetic statements.  The solution we
 *	adopted was to create a new internal type,  __long_long,
 *	that is a synonym for  long long, but whose use does
 *	not violate ANSI rules.
 *
 *	NOTE:	__long_long should not be used anyplace other than
 *		in this header file.  All other 64-bit integer
 *		types should be defined in terms of __int64_t 
 *		or __uint64_t.
 *
 *	
 *  (2)	The reason for the messy set of defines that follow is
 *	that some compilers (old CFRONT, and cc68k) do not
 *	use __long_long.  Therefore, we allow each to choose
 *	which of the following three definitions of 64-bit ints
 *	they want to use:
 *		1.	long long
 *		2.	__long_long
 *		3.	struct { ... }
 */
#if defined(_LONGLONG)
	/*  Its alright to use long long in definitions  */
typedef long long __int64_t;
typedef unsigned long long  __uint64_t;

#else
#if (defined(__cplusplus) && !defined(__EDG)) || defined(m68000)
	/*  old cfront and cc68k can handle neither 
	 *  long long nor __long_long, so we must use
	 *  a structure definition
	 */
typedef union {
	struct {	
		int hi32;
		int lo32;
	} hilo;
	double align;		/* to force 64-bit alignment */
} __int64_t;
typedef union {
	struct {
		unsigned int hi32;
		unsigned int lo32;
	} hilo;
	double align;		/* to force 64-bit alignment */
} __uint64_t;

#else

/* __long_long is a hidden builtin, ansi-compliant 64-bit type */
typedef __long_long __int64_t;
typedef unsigned __long_long __uint64_t;

#endif /* __cplusplus */
#endif /* _LONGLONG */

#endif /* _MIPS_SZLONG */

#if (_MIPS_SZPTR == 32)
typedef __int32_t __psint_t;
typedef __uint32_t __psunsigned_t;
#endif

#if (_MIPS_SZPTR == 64)
typedef __int64_t __psint_t;
typedef __uint64_t __psunsigned_t;
#endif

/*
 * If any fundamental type is 64 bit, then set the scaling type
 * to 64 bit
 */
#if (_MIPS_SZPTR == 64) || (_MIPS_SZLONG == 64) || (_MIPS_SZINT == 64)

/* there exists some large fundamental type */
typedef __int64_t __scint_t;
typedef __uint64_t __scunsigned_t;

#else

/* a 32 bit world */
typedef __int32_t __scint_t;
typedef __uint32_t __scunsigned_t;

#endif 

#endif /* C || C++ */

#ifdef __cplusplus
}
#endif

#endif /* !__SGIDEFS_H__ */
