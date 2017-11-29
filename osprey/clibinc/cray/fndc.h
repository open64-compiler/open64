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

/* USMID @(#) clibinc/cray/fndc.h	92.2	11/16/99 11:55:31 */

#ifndef _FNDC_H
#define _FNDC_H

/*
 * Definitions for Fortran Numeric Data Conversion
 *
 * This information is used in assign processing (libu) and in
 * Fortran unformatted I/O (libf).
 */

/* Define which systems support numeric data conversion */

#if	defined(_UNICOS) || defined(__mips) || defined (_LITTLE_ENDIAN)
#define	NUMERIC_DATA_CONVERSION_ENABLED	1
#else
#define	NUMERIC_DATA_CONVERSION_ENABLED	0
#endif

/*
 * Define numeric data conversion types.  These types are used
 * to index into various tables.
 */

#define NCV_NONE	0	/* No data conversion */
#define NCV_CRAY	1	/* CRAY classic */
#define NCV_IBM		2	/* IBM */
#define NCV_CDC		3	/* CDC 60-bit */
#define NCV_VMS		4	/* DEC VAX/VMS */
#define NCV_IEG		5	/* IEEE Generic (32-/64-bit) */
#define NCV_NVE		6	/* CDC 64-bit */
#define NCV_205		7	/* CDC Cyber 205 */
#define NCV_IEU		8	/* DEC Ultrix (IEG little-endian) */
#define NCV_IED		9	/* IEG, but with double-precision folded to single */
#define NCV_USER	10	/* User defined conversion */
#define NCV_SITE	11	/* Site defined conversion */
#define NCV_IBD		12	/* IBM, but with double-precision folded to single */
#define NCV_VAD		13	/* VMS, but with double-precision folded to single */
#define NCV_IUD		14	/* IEU, but with double-precision folded to single */
#define NCV_T3D		15	/* CRAY T3D/T3E */
				/* (64-bit IEEE with no double-precision or denorms) */
#define NCV_IEL		16	/* IEEE Large (64-/128-bit) */
#define NCV_MIPS	17	/* MIPS (SGI) format */
#define NCV_IA		18	/* Intel Architecture (IA32/IA64) */

#define NCV_MAX		19	/* 1 greater than max type number */

/*
 * Define character set types.
 */

#define CS_NONE		0	/* No character set conversion */
#define CS_ASCII	1	/* ASCII */
#define CS_EBCDIC	2	/* IBM EBCDIC */
#define CS_CDC		3	/* CDC 6-bit display code */
#define CS_USER		4	/* User defined conversion, deferred */
#define CS_SITE		5	/* Site defined conversion, deferred */

#define CS_MAX		6	/* 1 greater than max type number */

/*
 * Define native types.
 */

#ifdef	_CRAY1
# ifdef	_CRAYIEEE
# define NCV_NATIVE	NCV_IEL		/* cray-ts,ieee */
# else
# define NCV_NATIVE	NCV_CRAY	/* cray-ts, cray-c90, etc. */
# endif

#elif	defined(_CRAYMPP)
#define NCV_NATIVE	NCV_T3D		/* cray-t3d, cray-t3e, etc. */

#elif	defined(_MIPSEB)
#define NCV_NATIVE	NCV_MIPS	/* MIPS */

#elif	defined(_LITTLE_ENDIAN)
#define NCV_NATIVE	NCV_IA		/* Intel */

#elif	defined(_WORD32)
#define NCV_NATIVE	NCV_IEG		/* 32-bit platforms */

#endif

#define CS_NATIVE	CS_ASCII

/*
 * This structure is used to build tables of conversion functions
 * indexed by the above NCV data conversion types.  The flags are
 * used to indicate how the functions are to be called, or to
 * indicate special operations that may apply to a broader class
 * of conversions.
 */

struct c_funs_s {
	int	(* to_native)();	/* Foreign-to-native conversion	*/
	int	(* to_foreign)();	/* Native-to-foreign conversion	*/
	unsigned new_style_func:1;	/* 1 if new-style conv. func.	*/
	unsigned cray_int_compat:1;	/* 1 if ints are cray compat.	*/
					/* (twos complment, big-endian)	*/
	unsigned no_ieee_denorms:1;	/* 1 if no ieee denorms allowed	*/
};

/*
 * This structure is used to determine the length in bits of the
 * appropriate data types.  The first word is the number of valid
 * words in the array of type lengths.  The following words correspond
 * to the lengths of the data items of each type.  Note that this
 * structure is used for old-style (Fortran 77) conversion routines
 * and new-style (Fortran 90) conversion routines.  In the later case,
 * it represents the default size of the various types.
 */

struct _dsz_s {
	int	numtypes;	/* number of types in the table */
	int	typlen[9];	/* length of each type (normally 8 types) */
};

/*
 * This structure is used to determine a granularity for each numeric type.
 * The flag states whether alignment should be performed when reading/writing
 * noncharacter items.  The granularity is roughly the 'word' size.
 * padval is the desired padd value when aligning.  No conversion will
 * performed on this word before being written.
 */

struct _dal_s {
	unsigned	pflag:1;	/* Padd flag */
	int		unu:31;		/* Unused */
	int		gran:32;	/* Granularity */
	long long	padval;		/* Usually a word of blanks */
};

/*
 * External arrays
 */

extern struct	c_funs_s	__fndc_ncfunc [NCV_MAX];
extern struct	_dsz_s		*__fndc_f77sz [NCV_MAX];
extern struct	_dsz_s		*__fndc_f90sz [NCV_MAX];
extern struct	_dal_s		*__fndc_align [NCV_MAX];
extern		 int		__fndc_charsz [CS_MAX];

#endif	/* !_FNDC_H */
