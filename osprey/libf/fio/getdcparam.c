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



#pragma ident "@(#) libf/fio/getdcparam.c	92.6	10/07/99 13:07:09"

#include <fortran.h>
#include <cray/nassert.h>
#include <cray/dopevec.h>
#include "fio.h"

#define DT_SPECREAL	8	/* type code for converting single precision
				 * data to double precision data */

/*
 *	Define native sizes of the basic default types (in bits).
 */

#define	INTEGER_SZ	(sizeof(_f_int) << 3)	/* INTEGER */
#define	REAL_SZ		(sizeof(_f_real) << 3)	/* REAL */
#define	DOUBLE_SZ	(sizeof(_f_dble) << 3)	/* DOUBLE */
#define	COMPLEX_SZ	(sizeof(_f_comp) << 3)	/* COMPLEX */
#define	LOGICAL_SZ	(sizeof(_f_log) << 3)	/* LOGICAL */

/*
 * 	Forward references.
 */

static void
_gdc_abort(FIOSPTR css, unit *cup, struct f90_type ts);

#ifdef KEY
static int
#else
int
#endif /* KEY */
_gdc_dflt2len(int ncindex, struct f90_type ts, short dp);

int
_gdc_kind2len(int ncindex, struct f90_type ts);

int
_gdc_star2len(int ncindex, struct f90_type ts);

/*
 *	Return value
 *
 *		 0	If OK
 *		>0	Error code.
 */

int
_get_dc_param(
	FIOSPTR		css,		/* Fortran statement state */
	unit		*cup,		/* Unit pointer */
	struct f90_type	ts,		/* Type descriptor word */
	type_packet	*tip)		/* Type information packet */
{
	register ftype_t type90;	/* Fortran 90/95 data type	*/
	register short	type77;		/* Fortran 77 data type		*/
	register short	cvtype;		/* Pseudo-type conversion index	*/
	register short	newkind;	/* Set if new-style conversion	*/
	register short	dpflag;		/* Set if DOUBLE PRECISION decl.*/
	register int	forlen;		/* Bit length of foreign data	*/
	register int	natlen;		/* Bit length of native data	*/
	register int	ncindex;	/* Numeric conversion index	*/

	/* Assertions */

	assert ( ts.type >= DVTYPE_TYPELESS && ts.type <= DVTYPE_ASCII );

	type90	= tip->type90;
	type77	= tip->type77;
	natlen	= tip->intlen;
	forlen	= 0;		/* Assume foreign size is unknown */
	cvtype	= -1;		/* Assume no available conversion */
	dpflag	= (ts.dpflag || (ts.kind_or_star == DVD_KIND_DOUBLE)) ? 1 : 0;

	if (type90 == DVTYPE_ASCII)
		ncindex	= _charset_cnvt[cup->ucharset];
	else 
		ncindex	= cup->unumcvrt;

	if (ncindex == 0)
		goto done;

	newkind	= __fndc_ncfunc[ncindex].new_style_func;

	if ( !newkind && type77 == -1) {

		/*
		 * Create a f77 type code when we've been called from f90
		 * and we are using an old-style conversion function.  At
		 * this point, we only want to try to identify variables
		 * declared as double precision (or f90 equivalent) and
		 * declared-size integers that map to short integers. Any
		 * REAL KIND/star declaractions that map to a foreign
		 * double precision will be handled at the end of this
		 * routine.
		 */

		type77	= _f90_to_f77_type_cnvt[type90];

		switch (ts.kind_or_star) {

			case DVD_DEFAULT:
			case DVD_KIND_DOUBLE:

				/*
				 * If "default" type REAL, then map to
				 * DOUBLE PRECISION if declared as such.
				 */

				if (type77 == DT_REAL && dpflag)
					type77	= DT_DBLE;
				break;

			case DVD_STAR:
			case DVD_KIND_CONST:

				/*
				 * Map declared-size integers to short
				 * integers if their declared size is
				 * less than the foreign default integer
				 * size.
				 */

				if (type77 == DT_INT && (ts.dec_len << 3) <
					__fndc_f77sz[ncindex]->typlen[DT_INT])
					type77	= DT_SINT;
				break;

			default:
				break;
		} /* switch (ts.kind_or_star) */
	}

#ifdef	DEBUG_FDC
        printf("\n");
	switch (ts.type) {
	case DVTYPE_TYPELESS:	printf("declaration: typeless"); break;
	case DVTYPE_INTEGER:	printf("declaration: INTEGER"); break;
	case DVTYPE_REAL:	printf("declaration: REAL"); break;
	case DVTYPE_COMPLEX:	printf("declaration: COMPLEX"); break;
	case DVTYPE_LOGICAL:	printf("declaration: LOGICAL"); break;
	case DVTYPE_ASCII:	printf("declaration: CHARACTER"); break;
	} /* switch */
	if (ts.dpflag)
		printf(",  dpflag (DOUBLE) set\n");
	else
		printf("\n");
	switch (ts.kind_or_star) {
	case DVD_DEFAULT:	printf("KIND type: DEFAULT\n"); break;
	case DVD_KIND:		printf("KIND type: KIND=expression\n"); break;
	case DVD_STAR:		printf("KIND type: *%d (bytes)\n", ts.dec_len); break;
	case DVD_KIND_CONST:	printf("KIND type: KIND=%d\n", ts.dec_len); break;
	case DVD_KIND_DOUBLE:	printf("KIND type: KIND DOUBLE\n"); break;
	} /* switch */
	printf(" Internal length = %d bits\n", ts.int_len);
#endif

	switch (ts.kind_or_star) {

	case DVD_DEFAULT:
		/*
		 * The variable is default KIND.
		 */
		if (newkind) {
			forlen	= _gdc_dflt2len(ncindex, ts, dpflag);

			if (forlen != 0)
				cvtype	= type90;
		}
		else {
			cvtype	= type77;
			forlen	= __fndc_f77sz[ncindex]->typlen[cvtype];
		}
		break;

	case DVD_KIND_DOUBLE:
		/*
		 * The variable is either REAL (KIND=KIND(1.0D0)) or
		 * COMPLEX (KIND=KIND(1.0D0)).	Currently, double complex
		 * is not supported in the old F77-style numeric data
		 * conversion routines and will be diagnosed as such.
		 */

		if (newkind) {
			forlen	= _gdc_dflt2len(ncindex, ts, 1);

			if (forlen != 0)
				cvtype	= type90;
		}
		else
			if (type90 == DVTYPE_REAL) {
				cvtype	= DT_DBLE;
				forlen	= __fndc_f77sz[ncindex]->typlen[cvtype];
			}
		break;

	case DVD_STAR:
		/*
		 * The variable is declared as *N, where N is the size of
		 * the datum in bytes.
		 */

		forlen	= _gdc_star2len(ncindex, ts);

		if (newkind) {
			cvtype	= type90;

			if (forlen == 0)
				forlen	= ts.dec_len << 3;
		}
		else {
			cvtype	= type77;

			if (forlen == 0)
				forlen	= __fndc_f77sz[ncindex]->typlen[cvtype];
		}
		break;

	case DVD_KIND_CONST:
		/*
		 * The variable is declared as KIND=N, where N is the
		 * size of the datum in bytes.  Note that for COMPLEX,
		 * N is the size of each half.  The _gdc_kind2len routine
		 * will handle some special cases, otherwise the foreign
		 * length is equal to the KIND= expression.
		 */

		forlen	= _gdc_kind2len(ncindex, ts);

		if (newkind) {
			cvtype	= type90;

			if (forlen == 0) {

				forlen	= ts.dec_len << 3;

				if (cvtype == DVTYPE_COMPLEX)
					forlen	= forlen << 1;
			}
		}
		else {
			cvtype	= type77;

			if (forlen == 0)
				forlen	= __fndc_f77sz[ncindex]->typlen[cvtype];
		}
		break;

	case DVD_KIND:
		/*
		 * These are cases for which we cannot do foreign data 
		 * conversion because the KIND= value might vary from
		 * one implementation to another.  We would need to 
		 * simulate the foreign vendor's KIND() function to
	 	 * determine what the foreign length is; and basically,
		 * we don't want to go there.
		 */
		break;
	} /* switch */

#ifdef	DEBUG_FDC
	printf("  Foreign length = %d bits\n", forlen);
#endif

/*
 *	Special case integer if:
 *		1) foreign size and native size are equal
 *	and 	2) foreign integers are native compatible (twos complement,
 *		   big endian)
 */
	if (type90 == DVTYPE_INTEGER && forlen == natlen &&
	    __fndc_ncfunc[ncindex].cray_int_compat ) {
		ncindex	= 0;		/* data needs no conversion */
		goto done;
	}

#ifdef	__mips
/*
 *	Special case ieee conversion on mips.  No conversion required except
 *	for double double floating-point data or a size change.
 */
	if (ncindex == NCV_IEG &&
	    forlen == natlen &&
	     !(type90 == DVTYPE_REAL && natlen == (REAL_SZ << 1)) &&
	     !(type90 == DVTYPE_COMPLEX && natlen == (COMPLEX_SZ << 1)) ) {
		ncindex	= 0;		/* data needs no conversion */
		goto done;
	}
#endif

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
/*
 *	For the old-style conversion routines, there are generally two
 *	variants of REAL and INTEGER conversions available.  Map to the
 *	alternate choice if the foreign data size matches.  If none of the
 *	possibilities matches the foreign length (forlen) then return
 *	a type code of -1 to indicate that we don't know how to convert
 *	this item.
 */
	if ( !newkind && forlen != __fndc_f77sz[ncindex]->typlen[cvtype]) {
		register int	oldcvt = cvtype;
		cvtype	= -1;
		if (oldcvt == DT_INT) {
		    if (forlen == __fndc_f77sz[ncindex]->typlen[DT_SINT])
			cvtype	= DT_SINT;
		}
		else if (oldcvt == DT_REAL) {
		    if (__fndc_f77sz[ncindex]->numtypes > DT_SPECREAL &&
			forlen == __fndc_f77sz[ncindex]->typlen[DT_SPECREAL])
			cvtype	= DT_SPECREAL;
/*
 *			The internal length must match the basic f77 type
 *			length to utilize the old-style data conversion
 *			routines.
 */
		    if (ts.int_len !=
			(_f77_type_len[(cvtype == DT_SPECREAL) ? DT_REAL : cvtype] << 3))
			cvtype	= -1;

		}
	}
#endif

done:
#ifdef	DEBUG_FDC
	printf(" Conversion type  = %d (%s conversion function type)\n", cvtype,
				newkind ? "new" : "old f77");
	printf(" Conversion index = %d\n", ncindex);
#endif

	if (ncindex > 0) {

		tip->extlen	= forlen;
		tip->cnvindx	= ncindex;
		tip->newfunc	= newkind;
		tip->cnvtype	= cvtype;

		if (cup->uwrt)
			tip->cnvfunc	= __fndc_ncfunc[ncindex].to_foreign;
		else
			tip->cnvfunc	= __fndc_ncfunc[ncindex].to_native;

		if (cvtype < 0) {
			if (ABORT_ON_ERROR)
				_gdc_abort(css, cup, ts);

			return(FENCNV90);
		}
	}

	return(0);
}
/*
 *	_gdc_dflt2len - map default kinds to foreign lengths
 *
 */

#ifdef KEY
static int
#else
int
#endif /* KEY */
_gdc_dflt2len(
	int	ncindex,
	struct f90_type	ts,		/* Type descriptor word */
	short	dpflag)			/* One if double-precision, else zero */
{
	register int	forlen;

	forlen	= 0;

	switch (ncindex) {

	case NCV_T3D:	/* CRAY MPP (T3D/T3E) */
		forlen	= ts.int_len;

		if (ts.type == DVTYPE_COMPLEX)
			forlen	= 128;
		else
			if (ts.type > DVTYPE_TYPELESS && ts.type < DVTYPE_ASCII)
				forlen	= 64;
		break;

	case NCV_CRAY:	/* CRAY classic */
	case NCV_IEL:	/* CRAY 64-bit IEEE */
		switch (ts.type) {

		case DVTYPE_INTEGER:
		case DVTYPE_LOGICAL:
			forlen	= 64;
			break;

		case DVTYPE_REAL:
			forlen	= (ts.int_len <= REAL_SZ) ? 64 : 128;
			break;

		case DVTYPE_COMPLEX:
			forlen	= (ts.int_len <= COMPLEX_SZ) ? 128 : 256;
			break;

		case DVTYPE_TYPELESS:
		case DVTYPE_ASCII:
			forlen	= ts.int_len;
			break;
		} /* switch (ts.type) */

		break;

	case NCV_IBM:	/* IBM classic */
	case NCV_IEG:	/* IEEE generic */
	case NCV_IEU:	/* IEEE little-endian */
	case NCV_MIPS:	/* MIPS IEEE (128-bit is double double) */
	case NCV_VMS:	/* DEC VAX/VMS */
#ifdef	_CRAY
		/*
		 * Basically, foreign size equals one-half of internal size.
		 */

		if (ts.type != DVTYPE_TYPELESS && ts.type != DVTYPE_ASCII)
			forlen	= ts.int_len >> 1;
		else
			forlen	= ts.int_len;
#else
		/*
#ifdef KEY
		 * bug 2969: don't want to treat -i8/-r8
		 * differently, contrary to the following comment.
#endif
		 *
		 * Foreign size equals internal size; except variables
		 * that got promoted via -i8/-r8 are treated as if they
		 * had not been promoted.
		 */

		switch (ts.type) {

		case DVTYPE_INTEGER:
		case DVTYPE_LOGICAL:

			assert ( INTEGER_SZ == LOGICAL_SZ );

#ifdef KEY
			forlen	= ts.int_len;
#else
			forlen	= MIN(ts.int_len, INTEGER_SZ);
#endif /* KEY */
			break;

		case DVTYPE_REAL:
#ifdef KEY
			forlen	= ts.int_len;
#else
			forlen	= MIN(ts.int_len, (REAL_SZ << dpflag));
#endif /* KEY */
			break;

		case DVTYPE_COMPLEX:
#ifdef KEY
			forlen	= ts.int_len;
#else
			forlen	= MIN(ts.int_len, (COMPLEX_SZ << dpflag));
#endif /* KEY */
			break;

		case DVTYPE_TYPELESS:
		case DVTYPE_ASCII:
				forlen	= ts.int_len;
			break;
		} /* switch (ts.type) */
#endif
		break;

	case NCV_IED:	/* IEEE_dp */
#ifdef	_CRAY
		if (ts.type == DVTYPE_INTEGER || ts.type == DVTYPE_LOGICAL)
			forlen	= ts.int_len >> 1;
		else
#endif
			forlen	= ts.int_len;
		break;

	default:
		break;
	}

	return(forlen);
}

/*
 *	_gdc_star2len - find special cases for foreign *N lengths
 *
 *	_gdc_kind2len - find special cases for foreign KIND=N lengths
 *
 *		Special case any supported datatypes *N or constant KIND=N
 *		which do not have foreign data lengths equal to N x character 
 *		size.  For example, since INTEGER*2 on IBM has foreign length of
 *		2 bytes or 16 bits, we do not need to special case it here
 *		because it is handled in the default forlen calculation.
 *
 *		Note: *N and KIND=N are equivalent for all data types except
 *		complex.
 */

int
_gdc_star2len(
	int	ncindex,
	struct f90_type	ts)		/* Type descriptor word */
{
	register int	forlen;

	forlen	= 0;

	switch (ncindex) {

	case NCV_CDC:
		if (ts.type == DVTYPE_INTEGER &&
		    (ts.dec_len == 2 || ts.dec_len == 4))
			forlen	= 60;	/* cdc INTEGER*2/4 is 60 bits long */

		else if (ts.type == DVTYPE_REAL && ts.dec_len == 8)
			forlen	= 60;	/* cdc REAL*8 is 10 short bytes long */

		else if (ts.type == DVTYPE_COMPLEX && ts.dec_len == 16)
			forlen	= 120;	/* cdc COMPLEX*16 is 20 short bytes long */

		break;

	case NCV_NVE:
		if (ts.type == DVTYPE_INTEGER &&
		    (ts.dec_len == 2 || ts.dec_len == 4))
			forlen	= 64;	/* nosve INTEGER*2/4 is 64 bits long */
		break;

	case NCV_IEL:	/* CRAY 64-bit IEEE */
	case NCV_CRAY:	/* CRAY Classic */
		if (ts.type == DVTYPE_INTEGER || ts.type == DVTYPE_LOGICAL ||
		    (ts.type == DVTYPE_REAL && ts.dec_len < 8))
			forlen	= 64;

		if (ts.type == DVTYPE_COMPLEX && ts.dec_len < 16)
			forlen	= 128;
		break;

	case NCV_T3D:	/* CRAY MPP (T3D/T3E) */
		if ((ts.type == DVTYPE_INTEGER || ts.type == DVTYPE_LOGICAL) &&
		     ts.dec_len < 4)
			forlen	= 32;

		if (ts.type == DVTYPE_REAL && ts.dec_len > 8)
			forlen	= 64;

		if (ts.type == DVTYPE_COMPLEX && ts.dec_len > 16)
			forlen	= 128;
		break;

	default:
		break;
	}

	return(forlen);
}

int
_gdc_kind2len(
	int		ncindex,
	struct f90_type	ts)		/* Type descriptor word */
{
	register int	forlen;

	forlen	= 0;

	/* special cases would be added here */

	switch (ncindex) {

	case NCV_CRAY:	/* CRAY classic */
	case NCV_IEL:	/* CRAY 64-bit IEEE */
		if (ts.type == DVTYPE_INTEGER || ts.type == DVTYPE_LOGICAL ||
		    (ts.type == DVTYPE_REAL && ts.dec_len < 8))
			forlen	= 64;

		if (ts.type == DVTYPE_COMPLEX && ts.dec_len < 8)
			forlen	= 128;
		break;

	case NCV_T3D:	/* CRAY MPP (T3D/T3E) */
		if ((ts.type == DVTYPE_INTEGER || ts.type == DVTYPE_LOGICAL) &&
		     ts.dec_len < 4)
			forlen	= 32;

		if (ts.type == DVTYPE_REAL && ts.dec_len > 8)
			forlen	= 64;

		if (ts.type == DVTYPE_COMPLEX && ts.dec_len > 8)
			forlen	= 128;
		break;

	default:
		break;
	}

	return(forlen);
}

/*
 *	_gdc_abort
 *
 *		Abort with error FENCNV90 and appropriate error diagnostics.
 */

static void
_gdc_abort(
	FIOSPTR		css,		/* Fortran statement state */
	unit		*cup,		/* Unit pointer */
	struct f90_type	ts)		/* Type descriptor word */
{
	char	*tn;
	char	txt_decl[30];	/* fits largest possible declaration */
	char	*txt_dp;

	if (ts.kind_or_star == DVD_STAR)
		tn	= "%s*%d";
	else if (ts.kind_or_star == DVD_KIND)
		tn	= "%s(KIND=%d)";
	else if (ts.kind_or_star == 3)	/* TEMPORARY: the compiler is setting */
		tn	= "%s*%d";	/* kind_or_star to 3 instead of 2 */
	else
		tn	= "%s";

	(void) sprintf(txt_decl, tn, _f90_type_name[ts.type], ts.dec_len);

	txt_dp	= "";

	if (ts.dpflag && ts.int_len == sizeof(_f_real) << 3)
		txt_dp	= "\n  which was mapped to single precision with the -dp compiler option\n";
	
	_ferr(css, FENCNV90, txt_decl, txt_dp);

	return;
}
