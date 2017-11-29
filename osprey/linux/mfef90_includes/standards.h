#ifndef __STANDARDS_H__
#define __STANDARDS_H__

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

#ident "$Id: standards.h,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $"

/*
 * We define short forms for the predicates required by various
 * headers to support the various standards.
 *
 * These predicates are used to build up permitted declarations.
 * For ANSI-C specified headers, only ANSI-C definitions need no predicate.
 * Most are restrictive predicates - declaring when a symbol is NOT
 * permitted rather than when they are. Often, if no feature test macro is
 * declared then ALL symbols in a header are visible.(the notable exception
 * is ANSI headers - they MUST contain only ANSI symbols unless some other
 * feature test macro is used.
 * These predicates are built up from the following feature test macros:
 *	_SGI_SOURCE - used to extend ANSI headers to contain SGI specific
 *		      symbols. Both XOPEN and POSIX feature test macros
 *		      must override ALL symbols added by this macro
 *	_XOPEN_SOURCE - enable Xopen symbols and suppress non-Xopen symbols
 *			According to XPG4 section 2.2 defining _XOPEN_SOURCE
 *			should suffice to enable all _POSIX_SOURCE and
 *			_POSIX_C_SOURCE symbols (well, POSIX90 and POSIX1003.2
 *			symbols)
 *	_POSIX_SOURCE
 *	_POSIX_C_SOURCE - enable POSIX symbols and suppress non-POSIX symbols.
 * In ANSI mode, _SGI_SOURCE is not defined.
 *
 * One main assumption here is that for the most part, the symbol space
 * is ever increasing: ANSI -> POSIX90 -> (POSIX93 -> XOPEN4) -> SGI
 * The main hassle is POSIX93 and XPG4
 * For the few things that violate this, special predicates must be used.
 */

/*
 *
 * Note:  The '+0' is used with defines in comparisons in order to 
 *        prevent compiler syntax errors produced when the defined variable
 *        within user programs do not assign a value.
 *
 *        For example, the _XOPEN_SOURCE+0 is needed in order to prevent
 *        compiler syntax errors when _XOPEN_SOURCE is used within 
 *        comparisions and the #define _XOPEN_SOURCE with no value is used
 *        within the application source.  The #define _XOPEN_SOURCE with
 *        no value is used to designate XPG4 conformance.
 */

/*
 * ANSI symbols - In ANSI headers only ANSI symbols are permitted unless
 * additional feature test macros are given.
 * But in other headers we give all symbols (unless restricted by some other
 * feature test macro like POSIX or XOPEN).
 *
 * In non-ANSI headers we want ALL symbols present regardless of whether
 * we are in ANSI more or not.
 * In ANSI headers we want ALL symbols if we not in ANSI mode and only
 * ANSI symbols if we are in ANSI mode UNLESS they ask for more
 * symbols via a feature test macro.
 */

#define _ANSIMODE	(!defined(__EXTENSIONS__))
#define _NO_ANSIMODE	(defined(__EXTENSIONS__) || \
		defined(_SGI_SOURCE) || \
		defined(_POSIX_SOURCE) || \
		defined(_POSIX_C_SOURCE) || \
		defined(_MIPSABI_SOURCE) || \
		defined(_LARGEFILE64_SOURCE) || \
		defined(_XOPEN_SOURCE))

/*
 * POSIX symbols - additions various POSIX standards add over ANSI -
 * mode:ANSI	-> symbols are visible (except in ANSI headers)
 * 	POSIX	-> symbols are visible
 *	SGI	-> symbols are visible
 *	XOPEN	-> symbols are visible
 *
 * POSIX90 - 1003.1a (first POSIX)
 * POSIX2  - 1003.2 (shells & utilities)
 * POSIX93 - 1003.1b (real-time ++)
 * POSIX1C - 1003.1c (pthreads..)
 *
 * Note that _POSIX_SOURCE is obsolete and corresponds to _POSIX_C_SOURCE = 1.
 * _POSIX_C_SOURCE=2 was defined by 1003.2
 * 1003.1b sets _POSIX_C_SOURCE to 199309L.
 */
#define _POSIX90	(defined(_POSIX_SOURCE) || \
			 defined(_POSIX_C_SOURCE) || \
			 defined(_XOPEN_SOURCE) || \
			 defined(_SGI_SOURCE) || \
			 _ANSIMODE)
#define _POSIX2		((defined(_POSIX_C_SOURCE) && (_POSIX_C_SOURCE+0 > 1)) ||\
			 defined(_XOPEN_SOURCE) || \
			 defined(_SGI_SOURCE) || \
			 _ANSIMODE)

/*
 * Note that POSIX93 and XPG4 each have extensions over the other.
 * Thus one can make NO assumptions (as one could previously) that
 * if its POSIX then it must be XOPEN. According to XPG4 (section:2.2) if
 * _POSIX_C_SOURCE is >2 then the the behavior is undefined.
 * For maximal developer latitude, we don't turn off POSIX93 extensions
 * if they specify _XOPEN_SOURCE - they can get both if they wish.
 *
 * New symbols that are both in XPG4 and 1003.1b need to be enabled via:
 * 	#if _POSIX93 || _XOPEN4
 * These symbols shouldn't be visible in POSIX90/2 mode.
 */
#define _POSIX93 \
		((defined(_POSIX_C_SOURCE) && (_POSIX_C_SOURCE+0 >= 199309L)) ||\
		 (defined(_SGI_SOURCE) && _NO_XOPEN4 && _NO_POSIX) || \
		 (_ANSIMODE && _NO_XOPEN4 && _NO_POSIX))
#define _NO_POSIX	(!defined(_POSIX_SOURCE) && !defined(_POSIX_C_SOURCE))

/*
 * POSIX 1003.1c - new symbols made visible by pthreads, etc.
 * None of these are in XPG4.
 * We add _SGI_REENTRANT_FUNCTIONS for backward compatibility
 * Of course these symbols shouldn't be visible when in POSIX90/2/93 mode.
 * These really can't be visible in MIPS ABI mode either.
 */
#define _POSIX1C \
		((defined(_POSIX_C_SOURCE) && (_POSIX_C_SOURCE+0 >= 199506L)) ||\
		 (defined(_SGI_SOURCE) && _NO_XOPEN4 && _NO_POSIX && _NO_ABIAPI) || \
		 defined(_SGI_REENTRANT_FUNCTIONS) || \
		 _XOPEN5 || \
		 (_ANSIMODE && _NO_XOPEN4 && _NO_POSIX && _NO_ABIAPI))

/*
 * predicate for XOPEN XPG4 additions and extensions -
 * mode:ANSI	-> symbols visible (except in ANSI headers)
 * 	POSIX	-> symbols not visible
 * 	SGI	-> symbols visible
 * 	XOPEN	-> symbols visible
 */
#define _XOPEN4		(defined(_XOPEN_SOURCE) || \
				((defined(_SGI_SOURCE) && _NO_POSIX)) || \
				(_ANSIMODE && _NO_POSIX))
#define _NO_XOPEN4	(!defined(_XOPEN_SOURCE) || \
				(defined(_XOPEN_SOURCE) && \
				(_XOPEN_SOURCE+0 >= 500)))
/*
 * Following is a define for XOPEN XPG4 Unix eXtensions
 */
#define _XOPEN4UX \
		((defined(_XOPEN_SOURCE) && \
			defined(_XOPEN_SOURCE_EXTENDED) && \
			_XOPEN_SOURCE_EXTENDED+0 == 1) || \
		(defined(_SGI_SOURCE) && _NO_POSIX && _NO_XOPEN4) || \
		(_ANSIMODE && _NO_POSIX && _NO_XOPEN4))

/*
 * predicate for XOPEN XPG5 additions and extensions -
 * mode:ANSI	-> symbols visible (except in ANSI headers)
 * 	POSIX	-> symbols not visible
 * 	SGI	-> symbols visible
 * 	XPG4	-> symbols visible
 * 	XPG5	-> symbols visible
 */
#define _XOPEN5		((defined(_XOPEN_SOURCE) && \
				(_XOPEN_SOURCE+0 >= 500)) || \
			((defined(_SGI_SOURCE) && _NO_POSIX && _NO_XOPEN4)) || \
				(_ANSIMODE && _NO_POSIX))
#define _NO_XOPEN5	(!defined(_XOPEN_SOURCE) || \
				(defined(_XOPEN_SOURCE) && \
				(_XOPEN_SOURCE+0 < 500)))

/*
 * predicates for SGI extensions
 *	These include 'standard' unix functions/defines that aren't part of
 *	any published standard but are common in the 'standard' unix world.
 * mode:ANSI	-> symbols not visible
 * 	POSIX	-> symbols not visible
 * 	SGI	-> symbols visible
 *	XOPEN	-> symbols not visible
 */
#define _SGIAPI		((defined(_SGI_SOURCE) && \
				_NO_POSIX && _NO_XOPEN4 && _NO_XOPEN5) || \
			(_ANSIMODE && _NO_POSIX && _NO_XOPEN4 && _NO_XOPEN5))

/*
 * predicates for MIPS ABI extensions
 * mode:ANSI	-> symbols not visible
 * 	POSIX	-> symbols not visible
 * 	SGI	-> symbols visible
 *	XOPEN	-> symbols not visible
 *
 * The value that _MIPSABI_SOURCE contains is the MIPS ABI 
 * Black Book (BB for short) revision number.  For example, the 
 * number two (2) refers to the MIPS ABI BB2.0 revision.
 *
 * XXX - Eventually when all references to _ABI_SOURCE get replaced
 *       with _MIPSABI_SOURCE then the below check for _ABI_SOURCE will
 *       be deleted.
 */
#define _ABIAPI \
		((defined(_MIPSABI_SOURCE) && (_MIPSABI_SOURCE+0 >= 2)) || \
		defined(_ABI_SOURCE))
#define _NO_ABIAPI \
		((!defined(_MIPSABI_SOURCE) || (_MIPSABI_SOURCE+0 < 2)) && \
		!defined(_ABI_SOURCE))

/*
 * predicates for MIPS Large File ABI extensions
 * mode:ANSI	-> symbols not visible
 * 	POSIX	-> symbols not visible
 * 	SGI	-> symbols visible
 *	XOPEN	-> symbols not visible
 */
#define _LFAPI		(defined(_LARGEFILE64_SOURCE) || _SGIAPI)

/*
 * predicates to get reentrant functions
 */
#define _REENTRANT_FUNCTIONS	(defined(_SGI_REENTRANT_FUNCTIONS))

#endif /* __STANDARDS_H */
