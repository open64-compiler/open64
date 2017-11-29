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

/* USMID @(#) clibinc/cray/fmtconv.h	92.2	06/18/99 10:01:22 */


#ifndef	_CRAY_FMTCONV_H
#define	_CRAY_FMTCONV_H

#ifndef	NULL
#  define	NULL	0
#endif

/* Repeatable edit-descriptor types */

#define	TYNN	0	/* Nonnumeric type edit descriptor	*/
#define	TYIN	1	/* Integer type edit descriptor		*/
#define	TYUI	2	/* Unsigned integer type edit descriptor*/
#define	TYFP	3	/* Floating-point type edit descriptor	*/

/* Input and Output Conversion Mode Bits */

#define	MODEDP	004	/* Double-precision flag		*/
#define	MODEHP	020	/* Half-precision (32-bit) flag		*/
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define	MODEWP	040	/* 2-byte-precision (16-bit) flag	*/
#define	MODEBP	0100	/* 1-byte-precision (8-bit) flag	*/
#endif

/* Input Conversion Mode Bits */

#define	MODEBN	001	/* Blanks treated as nulls (ignored)	*/
#define	MODEBZ	002	/* Blanks treated as zeroes		*/

/* Output Conversion Mode Bits */

#define	MODESN	001	/* Numeric sign required flag		*/
#define	MODEUN	002	/* Unsigned datum flag			*/
#define	MODE77	010	/* Fortran 77 conformance flag		*/
#define	MODEMSN	0200	/* Minus sign written for -0.0		*/

/* Input conversion exit values */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define EX_INT8		7	/* Signed 1-byte integer	*/
#define EX_INT16	6	/* Signed 2-byte integer	*/
#endif
#define EX_REAL32	5	/* 32-bit real			*/
#define EX_REAL128	4	/* 128-bit real			*/
#define EX_REAL64	3	/* 64-bit real			*/
#define EX_INTL		2	/* Signed large integer (S-reg)	*/
#define EX_INTS		1	/* Signed small integer (A-reg)	*/
#define EX_ILLCHAR	-1	/* invalid character		*/
#define EX_FIXOFLO	-2	/* fixed-point overflow		*/
#define EX_EXPUFLO	-3	/* floating-point underflow	*/
#define EX_EXPOFLO	-4	/* floating-point overflow	*/
#define EX_NULLFLD	-5	/* null field (no digits)	*/
#define EX_INVLOGI	-10	/* reserved for libf (logical)	*/


/*
 * Define typedefs for generic declarations of the input and output
 * conversion routines.
 */

typedef int ic_func   ( const long *_Fca, const long *_Width, long **_Lcap1,
			const long *_Mode, void *_Result, long *_Statval,
			const long *_Digits, const long *_Scale);

typedef long *oc_func ( const void *_Value, long *_Fca,
			const long *_Mode, const long *_Width,
			const long *_Digits, const long *_Exp,
			const long *_Scale);

/* Input conversion routines */

extern ic_func	_bu2s;		/* Binary Unpacked to Single word	*/
extern ic_func	_defgu2sd;	/* Floating-point (D,E,F,G) Unpacked to
				   Single or Double word		*/
extern ic_func	_iu2s;		/* Integer Unpacked to Single word	*/
extern ic_func	_ou2s;		/* Octal Unpacked to Single word	*/
extern ic_func	_zu2s;		/* Hexadecimal Unpacked to Single word	*/

/* Output conversion routines */

extern oc_func	_s2ub;		/* Single word to Unpacked Binary	*/
extern oc_func	_s2ui;		/* Single word to Unpacked Integer	*/
extern oc_func	_s2uo;		/* Single word to Unpacked Octal	*/
extern oc_func	_s2uz;		/* Single word to Unpacked Hexadecimal	*/
extern oc_func	_sd2udee;	/* Single/Double word to Unpacked D/E	*/
extern oc_func	_sd2uene;	/* Single/Double word to Unpacked EN	*/
extern oc_func	_sd2uese;	/* Single/Double word to Unpacked ES	*/
extern oc_func	_sd2uf;		/* Single/Double word to Unpacked F	*/
extern oc_func	_sd2uge;	/* Single/Double word to Unpacked G	*/

#endif /* !_CRAY_FMTCONV_H */
