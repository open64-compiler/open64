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

/* USMID @(#) clibinc/cray/fortio.h	92.0	10/08/98 14:28:05 */
#ifndef _CRAY_FORTIO_H
#define _CRAY_FORTIO_H

/*
 *	fortio.h	- contains global definitions which pertain
 *			  to Fortran I/O.  These definitions are used
 *			  by libf and by the procstat and procview
 *			  commands.
 */

/************************************************************************/
/*									*/
/*  Constants								*/
/*									*/
/************************************************************************/

/*
 * Fortran I/O types.
 *
 * Modification of this list also requires changes to macro FIO_METHOD.
 */

#define FIO_SF  1       /* sequential formatted */
#define FIO_SU  2       /* sequential unformatted */
#define FIO_DF  3       /* direct formatted */
#define FIO_DU  4       /* direct unformatted */

/*
 *	File structure constant defintions.
 */

#define FS_DEFAULT	0	/* file structure not yet determined	*/

#define STD		1	/* stdio compatible unblocked or text	*/
#define FS_PURE		2	/* pure data (CRAY-2 only)		*/
#define FS_COS		3	/* COS blocked				*/
#define FS_U		4	/* unblocked -s u			*/
#define FS_BIN		5	/* unblocked -s bin (YMP only)		*/
#define FS_TAPE		6	/* interchange tape			*/
#define FS_FDC		7	/* FFIO file			 	*/
#define FS_TAPE50	8	/* interchange tape (special rls 5.0)	*/
#define FS_TEXT		9	/* newline terminated records		*/
#define FS_UNBLOCKED	10	/* no record blocking 			*/
#define FS_AUX		11	/* an auxiliary type of i/o (e.g. aqio)	*/
#define FS_F77		12	/* unformatted structure on workstations*/

/*
 * Other constants.
 */

#define MXUNITSZ  32       /* Max. size of file name: sizeof("fort.nnn") */

/************************************************************************/
/*									*/
/*  Macros								*/
/*									*/
/************************************************************************/

/*
 * FIO_METHOD	- macro which returns a string describing the I/O type
 *		  corresponding to an I/O type code, or NULL if it
 *		  is an invalid code.
 *
 *		  This macro is used by the procstat command and by _fcontext.
 */
#define FIO_METHOD(_C) 				\
	( ((_C) == FIO_SF)?			\
		"sequential formatted"		\
	:( ((_C) == FIO_SU)?			\
		"sequential unformatted"	\
	:( ((_C) == FIO_DF)?			\
		"direct formatted"		\
	:( ((_C) == FIO_DU)?			\
		"direct unformatted"		\
	: 					\
		NULL				\
	))))

/*
 * FIO_STRUCT	- macro which returns a string describing the Fortran file
 *		  file structure corresponding to an integer  structure code.
 *
 *		  This macro is used by the procstat command and by _fcontext.
 */

#define FIO_STRUCT(_C)				\
	( ((_C) == FS_TEXT)?			\
		"text"				\
	:( ((_C) == FS_COS)?			\
		"COS blocked"			\
	:( ((_C) == FS_UNBLOCKED)?		\
		"unblocked"			\
	:( ((_C) == FS_PURE)?			\
		"pure data"			\
	:( ((_C) == FS_U)?			\
		"unblocked"			\
	:( ((_C) == FS_BIN)?			\
		"unblocked"			\
	:( ((_C) == FS_TAPE)?			\
		"tape"				\
	:( ((_C) == FS_TAPE50)?			\
		"tape"				\
	:( ((_C) == STD)?			\
		"unblocked or text"		\
	:( ((_C) == FS_F77)?			\
		"UNIX blocked"			\
	:( ((_C) == FS_FDC)?			\
		"FFIO"				\
	:					\
		NULL				\
	)))))))))))

/************************************************************************/
/*									*/
/*  Typedefs								*/
/*									*/
/************************************************************************/

/*
 *	Define data types to hold unit and record numbers.  These data
 *	types transcend the Fortran run-time library, and so are defined
 *	here.  Note some of the impacts:
 *
 *	1) All the places where a unit/record number is passed into
 *	   the run-time library (and there are many), and
 *
 *	2) All the places where a unit/record number is printed out
 *	   or formatted for printed (e.g., "%lld").
 *
 */

#ifdef	_CRAY		/* KLUDGE for older C/C++ compilers */
typedef long		unum_t;		/* Fortran unit number */
typedef long		recn_t;		/* Fortran record number */
#else
typedef long long	unum_t;		/* Fortran unit number */
typedef long long	recn_t;		/* Fortran record number */
#endif

#endif /* !_CRAY_FORTIO_H */
