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



#pragma ident "@(#) libf/fio/fmtchk.c	92.2	06/18/99 19:52:04"
#include <cray/fmtconv.h>
#include <cray/format.h>
#include "fmt.h"

/*
 * The _wr_ilchk and _rd_ilchk arrays check edit-descriptors against the
 * internal length (container size in bytes) of an I/O list item.
 *
 * These tables can be viewed as a two-dimensional array with the internal
 * length measured in bytes along the x-axis and the edit-descriptor along the 
 * y-axis.  Each byte along the x-axis is interpreted as follows:
 *
 *	INVALID_INTLEN		- this combination is invalid
 *	!= INVALID_INTLEN	- the mode bits passed to the input or output
 *				  data conversion function.
 *
 * 
 * Character variables should not be indexed into this table.   The internal 
 * length used to lookup complex I/O list items should be the length of only 
 * one component (real or imaginary part) of the complex value.
 */

#define D	((signed char)  MODEDP)		/* 16 byte real */
#define U	((signed char)  MODEUN)		/* unsigned integer output */

#ifdef	MODEHP
#define H	((signed char)  MODEHP)		/* 4 byte data */
#else
#define H	INVALID_INTLEN			/* 4 byte data not supported */
#endif

#ifdef	MODEWP
#define W	((signed char)  MODEWP)		/* 2 byte data */
#else
#define W	INVALID_INTLEN			/* 4 byte data not supported */
#endif

#ifdef	MODEBP
#define B	((signed char)  MODEBP)		/* 1 byte data */
#else
#define B	INVALID_INTLEN			/* 4 byte data not supported */
#endif

#define HU	(H | U)
#define DU	(D | U)
#define WU	(W | U)
#define BU	(B | U)

#define _	INVALID_INTLEN

signed char
_wr_ilchk[LAST_DATA_ED][MAX_SUP_INTLEN] = {
	/* 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 */
	{  0, 0, _, 0, _, _, _, 0, _, _, _, _, _, _, _, 0  },	/* A */
	{ BU,WU, _,HU, _, _, _, U, _, _, _, _, _, _, _, DU },	/* B */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* D */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* E */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* EN */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* ES */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* F */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* G */
	{  B, W, _, H, _, _, _, 0, _, _, _, _, _, _, _, _  },	/* I */
	{  B, W, _, H, _, _, _, 0, _, _, _, _, _, _, _, _  },	/* L */
	{ BU,WU, _,HU, _, _, _, U, _, _, _, _, _, _, _, DU },	/* O */
	{  0, 0, _, 0, _, _, _, 0, _, _, _, _, _, _, _, 0  },	/* R */
	{ BU,WU, _,HU, _, _, _, U, _, _, _, _, _, _, _, DU },	/* Z */
	{  0, 0, _, 0, _, _, _, 0, _, _, _, _, _, _, _, _  }	/* Q */
};

signed char
_rd_ilchk[LAST_DATA_ED][MAX_SUP_INTLEN] = {
	/* 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 */
	{  0, 0, _, 0, _, _, _, 0, _, _, _, _, _, _, _, 0  },	/* A */
	{ BU,WU, _, H, _, _, _, 0, _, _, _, _, _, _, _, _  },	/* B */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* D */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* E */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* EN */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* ES */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* F */
	{  _, _, _, H, _, _, _, 0, _, _, _, _, _, _, _, D  },	/* G */
	{  B, W, _, H, _, _, _, 0, _, _, _, _, _, _, _, _  },	/* I */
	{  B, W, _, H, _, _, _, 0, _, _, _, _, _, _, _, _  },	/* L */
	{ BU,WU, _, H, _, _, _, 0, _, _, _, _, _, _, _, _  },	/* O */
	{  0, 0, _, 0, _, _, _, 0, _, _, _, _, _, _, _, 0  },	/* R */
	{ BU,WU, _, H, _, _, _, 0, _, _, _, _, _, _, _, _  },	/* Z */
	{  0, 0, _, 0, _, _, _, 0, _, _, _, _, _, _, _, _  }	/* Q */
};

/*
 * The _rw_mxdgt array yields the maximum number of "digits" for a
 * specified data edit-descriptor and datum size (in bytes).  This
 * is used for zero-width formatted I/O.
 *
 * The floating-point sizes (D through G edit-descriptors) are set
 * at run-time by the formatted I/O setup routine.
 */

#undef	_
#define	_	((signed char) -100)

signed char
_rw_mxdgt[LAST_DATA_ED][MAX_SUP_INTLEN] = {
	/* 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* A */
	{  8,16, _,32, _, _, _,64, _, _, _, _, _, _, _,127},	/* B */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* D */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* E */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* EN */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* ES */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* F */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* G */
	{  3, 5, _,10, _, _, _,19, _, _, _, _, _, _, _, _ },	/* I */
	{  2, 2, _, 2, _, _, _, 2, _, _, _, _, _, _, _, _ },	/* L */
	{  3, 6, _,11, _, _, _,22, _, _, _, _, _, _, _,44 },	/* O */
	{  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ },	/* R */
	{  2, 4, _, 8, _, _, _,16, _, _, _, _, _, _, _,32 },	/* Z */
	{  0, 0, _, 0, _, _, _, 0, _, _, _, _, _, _, _, _ }	/* Q */
};

/*
 * The following arrays are used to do error checking of edit-descriptors
 * against the data type of I/O list items.  Versions $RNOCHK and $WNOCHK
 * may be loaded with segldr(1).  For example:
 *
 *	-D EQUIV=$WNOCHK($WCHK)		- and/or -
 *	-D EQUIV=$RNOCHK($RCHK)
 *
 * The NOCHK versions allow more combinations of logical, real and integer
 * editing. These loader directives are available on CRI systems only.
 *
 *
 * The CHK77 versions allow only Fortran-77 conforming editing.
 *
 * The CHK90 versions allow only Fortran-90 conforming editing.
 *
 * These tables can be viewed as a two-dimensional array with the data type
 * along the y-axis and the edit-descriptor along the x-axis.  Each position
 * along the x-axis is one bit, interpreted as follows:
 *
 *	0	This edit-descriptor and data type combination is allowed.
 *	1	This edit-descriptor and data type combination is NOT allowed.
 */

/*
 * _RCHK defines the rules for Fortran READs.
 */

fmtchk_t _RCHK[DVTYPE_ASCII] = {
	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Typeless	*/
	{  0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0 },	/* Integer	*/
	{  1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Real		*/
	{  1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Complex	*/
	{  1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};

/*
 * _RNOCHK liberalizes the checks for integer, real and logical.
 */

fmtchk_t _RNOCHK[DVTYPE_ASCII] = {
	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Typeless	*/
	{  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 },	/* Integer	*/
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Real		*/
	{  1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Complex	*/
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};

/*
 * _RCHK77 defines the rules for Fortran-77 READs (strict conformance).
 */

fmtchk_t _RCHK77[DVTYPE_ASCII] = {
	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0 },	/* Typeless	*/
	{  1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1 },	/* Integer	*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1 },	/* Real		*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1 },	/* Complex	*/
	{  1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};

/*
 * _RCHK90 defines the rules for Fortran-90, READs (strict conformance).
 */

fmtchk_t _RCHK90[DVTYPE_ASCII] = {
	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Typeless	*/
	{  1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1 },	/* Integer	*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1 },	/* Real		*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1 },	/* Complex	*/
	{  1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};

/*
 * _WCHK defines the rules for Fortran WRITEs.
 */

fmtchk_t _WCHK[DVTYPE_ASCII] = {
	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Typeless	*/
	{  0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0 },	/* Integer	*/
	{  1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Real		*/
	{  1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Complex	*/
	{  1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};

/*
 * _WNOCHK liberalizes the checks for integer, real and logical.
 */

fmtchk_t _WNOCHK[DVTYPE_ASCII] = {
	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Typeless	*/
	{  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 },	/* Integer	*/
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Real		*/
	{  1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Complex	*/
	{  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};

/*
 * _WCHK77 defines the rules for Fortran-77 WRITEs (strict conformance).
 */

fmtchk_t _WCHK77[DVTYPE_ASCII] = {
	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0 },	/* Typeless	*/
	{  1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1 },	/* Integer	*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1 },	/* Real		*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1 },	/* Complex	*/
	{  1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};

/*
 * _WCHK90 defines the rules for Fortran-90, WRITEs (strict conformance).
 */

fmtchk_t _WCHK90[DVTYPE_ASCII] = {

	/* Q  Z  R  O  L  I  G  F  ES EN E  D  B  A */
	{  1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },	/* Typeless	*/
	{  1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1 },	/* Integer	*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1 },	/* Real		*/
	{  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1 },	/* Complex	*/
	{  1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1 },	/* Logical	*/
	{  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0 }	/* Character	*/
};
