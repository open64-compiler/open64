/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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



#pragma ident "@(#) libf/fio/unitseek.c	92.1	06/18/99 18:38:26"

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include <stdio.h>
#include <string.h>
#include "fio.h"
#if defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
#define LIBFSEEK fseek64
#else
#ifdef KEY /* Bug 1678 */
/* Need 64 bit position to support large files */
#define LIBFSEEK fseeko
#else /* KEY Bug 1678 */
#define LIBFSEEK fseek
#endif /* KEY Bug 1678 */
#endif
/*
 *	_unit_seek 
 *		Fortran seek.  This routine exists as a support
 *		routine for direct access I/O.  It handles the
 *		logical or physical seek to the specified record.
 *
 *	input:
 *		cup	unit pointer
 *		recn	record number (1 is the first record)
 *		iost	statement state information
 *
 *	returns:
 *		 0	for success (cup->udalast is also updated)
 *		>0	for error, value is error number
 */

int
_unit_seek(
	unit	*cup,	/* Unit pointer */
	recn_t	recn,	/* Record number */
	int	iost)	/* Statement type */
{
	register int	errn;		/* Error number		*/
	register int	isfmt;		/* 1 if formatted else 0*/
	register long	recl;		/* Record length	*/
	register off_t	pos;		/* File position	*/
	struct ffsw	fst;		/* FFIO status block	*/

	errn	= 0;
	recl	= cup->urecl;		/* Get record length */
	isfmt	= cup->ufmt ? 1 : 0;	/* Formatted? */

	if (recn <= 0)
		return(FEIVRECN);	/* Invalid record number */

	if ((iost & TF_READ) && (recn > cup->udamax))
		return(FENORECN);	/* Reading a nonexistent record */

	switch (cup->ufs) {

		case FS_TEXT:
		case STD:

			/*
			 * Seek to the requested record number.  If formatted
			 * I/O, then adjust the record length to account for
			 * the newline delimiters.
			 */

			assert ( strlen("\n") == 1 );

			recl	= recl + isfmt;
			pos	= (off_t) (recn - 1) * recl;
			if (LIBFSEEK(cup->ufp.std, pos, SEEK_SET) != 0)
				errn	= errno;

			break;

		case FS_FDC:

			/*
			 * Seek to the requested record number and let
			 * the FFIO layer make whatever adjustments are
			 * necessary for record delimiters.
			 */

			pos	= (off_t)(recn - 1) * recl;
			if( XRCALL(cup->ufp.fdc, seekrtn)
					cup->ufp.fdc, pos, 0, &fst) < 0)
				errn	= fst.sw_error;
#if	PURE_ENABLED
			else
				if (cup->ufmt == 0)	/* If unformatted */
					cup->ufbitpos	= offset << 3;
#endif	/* PURE_ENABLED */

			break;

		case FS_TAPE:
		case FS_BIN:
		default:
			errn	= FEINTUNK;	/* Deep weeds */
			break;
	} /* switch */

	/*
	 * If successful seek, update last record processed.  Note that
	 * for formatted I/O, the last record processed will be incremented
	 * in endrec() processing.
	 */

	if (errn == 0)
		cup->udalast	= recn - isfmt;

	return(errn);
}
