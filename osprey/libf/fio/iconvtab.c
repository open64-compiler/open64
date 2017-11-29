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



#pragma ident "@(#) libf/fio/iconvtab.c	92.2	06/23/99 10:34:31"
#include <cray/dopevec.h>
#include <cray/fmtconv.h>
#include <cray/format.h>
#include <cray/mtlock.h>	/* For SHARED_CC_BUG_WORKAROUND */

/* Local Input Conversion Routines */

extern ic_func	_LU2S;

/*
 *	_iconvtab[]	Table containing pointers to the input
 *			conversion functions for each data edit-
 *			descriptor, indexed by edit-descriptor.
 */

ic_func * const
_iconvtab [LAST_DATA_ED + 1] = {
	 NULL,		/* None	*/
	 NULL,		/* A_ED	*/
	_bu2s,		/* B_ED	*/
	_defgu2sd,	/* D_ED	*/
	_defgu2sd,	/* E_ED	*/
	_defgu2sd,	/* EN_ED*/
	_defgu2sd,	/* ES_ED*/
	_defgu2sd,	/* F_ED	*/
	_defgu2sd,	/* G_ED	*/
	_iu2s,		/* I_ED	*/
	_LU2S,		/* L_ED	*/
	_ou2s,		/* O_ED	*/
	 NULL,		/* R_ED */	/* CRI extension */
	_zu2s,		/* Z_ED	*/
	 NULL		/* Q_ED */	/* MIPSpro extension */
};

/*
 *	_ilditab	Table containing numeric conversion functions,
 *			indexed by data type.  For use with list-directed
 *			input.
 */

ic_func * const
_ilditab[DVTYPE_NTYPES] = {
	NULL,		/* DVTYPE_UNUSED	Unused		*/
	NULL,		/* DVTYPE_TYPELESS	Typeless	*/
	_iu2s,		/* DVTYPE_INTEGER	Integer		*/
	_defgu2sd,	/* DVTYPE_REAL		Real		*/
	_defgu2sd,	/* DVTYPE_COMPLEX	Complex		*/
	_LU2S,		/* DVTYPE_LOGICAL	Logical		*/
	NULL,		/* DVTYPE_ASCII		Character	*/
	NULL,		/* DVTYPE_DERIVEDBYTE	Unused		*/
	NULL		/* DVTYPE_DERIVEDWORD	Unused		*/
};

/*
 *	_idedtab[]	Table containing data edit-descriptors for each
 *			data type, indexed by data type.  For use with
 *			the G edit-descriptor.
 */

short const
_idedtab [DVTYPE_NTYPES] = {
	-1,	/* DVTYPE_UNUSED	Unused		*/
	O_ED,	/* DVTYPE_TYPELESS	Typeless	*/
	I_ED,	/* DVTYPE_INTEGER	Integer		*/
	G_ED,	/* DVTYPE_REAL		Real		*/
	G_ED,	/* DVTYPE_COMPLEX	Complex		*/
	L_ED,	/* DVTYPE_LOGICAL	Logical		*/
	A_ED,	/* DVTYPE_ASCII		Character	*/
	-1,	/* DVTYPE_DERIVEDBYTE	Unused		*/
	-1	/* DVTYPE_DERIVEDWORD	Unused		*/
};

SHARED_CC_BUG_WORKAROUND(_iconvtab_kludge_func)
