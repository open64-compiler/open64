/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#pragma ident "@(#) libf/fio/tables.c	92.2	06/18/99 18:41:02"

/*
 *	tables.c - This module contains tables and structures with
 *		constant data.
 */ 

#include <foreign.h>
#include <fortran.h>
#include <cray/dopevec.h>
#include <cray/mtlock.h>	/* For SHARED_CC_BUG_WORKAROUND */
#include "fio.h"

/*
 *	_f77_to_f90_type_cnvt[]
 *		Table which converts a Fortran 77 type code to a Fortran 90
 *		type code.
 */

const ftype_t
_f77_to_f90_type_cnvt[DT_MAX] = {
	DVTYPE_TYPELESS,	/* DT_NONE	Typeless	*/
	DVTYPE_INTEGER,		/* DT_INT	Integer		*/
	DVTYPE_REAL,		/* DT_REAL	Real		*/
	DVTYPE_REAL,		/* DT_DBLE	Double precision*/
	DVTYPE_COMPLEX,		/* DT_CMPLX	Complex		*/
	DVTYPE_LOGICAL,		/* DT_LOG	Logical		*/
	DVTYPE_ASCII,		/* DT_CHAR	Character	*/
	DVTYPE_INTEGER,		/* DT_SINT	Short integer	*/
	DVTYPE_COMPLEX		/* DT_DBLCOM	Complex		*/
};

/*
 *	_f90_to_f77_type_cnvt[]
 *		Table which converts a Fortran 90 type code to a Fortran 77
 *		type code.
 */

const short
_f90_to_f77_type_cnvt[DVTYPE_NTYPES] = {
	-1,		/* DVTYPE_UNUSED	Unused		*/
	DT_NONE,	/* DVTYPE_TYPELESS	Typeless	*/
	DT_INT,		/* DVTYPE_INTEGER	Integer		*/
	DT_REAL,	/* DVTYPE_REAL		Real		*/
	DT_CMPLX,	/* DVTYPE_COMPLEX	Complex		*/
	DT_LOG,		/* DVTYPE_LOGICAL	Logical		*/
	DT_CHAR,	/* DVTYPE_ASCII		Character	*/
	-1,		/* DVTYPE_DERIVEDBYTE	Unused		*/
	-1		/* DVTYPE_DERIVEDWORD	Unused		*/
};

/*
 *	_f77_type_len[]
 *		Table containing the byte size of each Fortran 77 data type,
 *		indexed by the Fortran 77 data type code.
 */

const short
_f77_type_len[DT_MAX] = {
	sizeof(long),		/* DT_NONE	Typeless	*/
	sizeof(_f_int),		/* DT_INT	Integer		*/
	sizeof(_f_real),	/* DT_REAL	Real		*/
	sizeof(_f_real) * 2,	/* DT_DBLE	Double precision*/
	sizeof(_f_real) * 2,	/* DT_CMPLX	Complex		*/
	sizeof(_f_log),		/* DT_LOG	Logical		*/
	sizeof(char),		/* DT_CHAR	Character	*/
#ifdef _CRAYT3E
	sizeof(_f_int4),	/* DT_SINT	Short integer	*/
#else
	sizeof(_f_int),		/* DT_SINT	Short integer	*/
#endif
	sizeof(_f_real) * 4	/* DT_DBLCOM	Double complex	*/
};

/*
 *	_f90_type_name[]
 *		Table containing the name of each Fortran 90 data type,
 *		indexed by the Fortran 90 data type code.
 */

const char *
_f90_type_name[DVTYPE_NTYPES] = {
	NULL,		/* DVTYPE_UNUSED	Unused		*/
	"boolean",	/* DVTYPE_TYPELESS	Typeless	*/
	"INTEGER",	/* DVTYPE_INTEGER	Integer		*/
	"REAL",		/* DVTYPE_REAL		Real		*/
	"COMPLEX",	/* DVTYPE_COMPLEX	Complex		*/
	"LOGICAL",	/* DVTYPE_LOGICAL	Logical		*/
	"CHARACTER",	/* DVTYPE_ASCII		Character	*/
	NULL,		/* DVTYPE_DERIVEDBYTE	Unused		*/
	NULL 		/* DVTYPE_DERIVEDWORD	Unused		*/
};

/*
 *	_f77_type_name[]
 *		Table containing the name of each Fortran 77 data type,
 *		indexed by the Fortran 77 data type code.
 */

const char *
_f77_type_name[DT_MAX] = {
	"Typeless",		/* DT_NONE	Typeless	*/
	"INTEGER",		/* DT_INT	Integer		*/
	"REAL",			/* DT_REAL	Real		*/
	"DOUBLE PRECISION",	/* DT_DBLE	Double precision*/
	"COMPLEX",		/* DT_CMPLX	Complex		*/
	"LOGICAL",		/* DT_LOG	Logical		*/
	"CHARACTER",		/* DT_CHAR	Character	*/
	"Short INTEGER",	/* DT_SINT	Short integer	*/
	"Double Complex"	/* DT_DBLCOM	Double Complex	*/
};

/*
 *	_charset_cnvt[]
 *		Table which maps a character conversion code into the
 *		corresponding general numeric data conversion type.
 */

const short
_charset_cnvt[CS_MAX]	= {0, 0, NCV_IBM, NCV_CDC, 0, 0};

/*
 *	_ffstat_cnvt[]
 *		Table which maps a return status from ffread into
 *		the corresponding _frch status code.
 */

const short
_ffstat_cnvt[7]	= {
	-1,	/* not used */
	CNT,
	EOR,
	EOF,
	EOD,
	CNT,	/* for FFBOD */
	-1	/* for FFERR */
};

/*
 *	_old_namelist_to_f77_type_cnvt[]	
 *		Table containing the mapping of the old Namelist
 *		Fortran data type codes to Fortran 77 type codes.
 *		The table is indexed by the old type code.
 */

const short
_old_namelist_to_f77_type_cnvt[10] = {
	DT_NONE,		/* Typeless		*/
	DT_SINT,		/* Short integer	*/
	DT_INT,			/* Integer		*/
	DT_REAL,		/* Real			*/
	DT_DBLE,		/* Double precision	*/
	DT_CMPLX,		/* Complex		*/
	DT_CHAR,		/* Character		*/
	-1,			/* Unused		*/
	-1,			/* Unused		*/
	DT_LOG			/* Logical		*/
};

/*
 *	__tip_null[]
 *		Structure containing a null type information packet
 *		for initialization.
 */

type_packet
__tip_null = {DVTYPE_TYPELESS, -1, 0, 0, 0, 0, 1, 0, 0, 0, NULL};

SHARED_CC_BUG_WORKAROUND(_tables_kludge_func)
