/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/unitbksp.c	92.1	06/18/99 18:38:26"

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"
#ifdef __mips
typedef long long _ftelltype;
#define LIBFSEEK fseek64
#define LIBFTELL ftell64
#define FTELLZERO 0LL
#else
#ifdef KEY /* Bug 1678 */
/* Need 64 bit position to support large files. */
typedef off_t _ftelltype;
#define LIBFSEEK fseeko
#define LIBFTELL ftello
#define FTELLZERO ((_ftelltype)0)
#else
typedef long _ftelltype;
#define LIBFSEEK fseek
#define LIBFTELL ftell
#define FTELLZERO 0L
#endif /* KEY Bug 1678 */
#endif


/*
 *	_unit_bksp
 *
 *		Perform a backspace operation on a connected unit.
 *
 *	Return value:
 *
 *		0 on normal completion.  Error status if an error is 
 *		encountered.
 */

int
_unit_bksp(unit *cup)
{
	int		errn;
	unsigned char	tbuf[TBUFSZB], *tp;
	struct ffsw	fst;
	int first_read;

        if (cup->useq == 0)	/* If direct access file */
                return(FEBKSPIV);	/* BACKSPACE invalid on dir. acc.*/

/*
 *      Wait for completion of a preceding asynchronous BUFFER IN/OUT.
 */

	WAITIO(cup, return(cup->uffsw.sw_error));

	if (cup->uwrt) {
		if (cup->utrunc) {
			/*
			 * Truncate file after last sequential write before
			 * this BACKSPACE.
			 */
			errn	= _unit_trunc(cup);
			if (errn != 0)
				return(errn);
		}
		cup->uwrt	= 0;
	}

/*
 *	According to the file structure make the appropriate request to
 *      backspace one logical record. 
 */
	switch( cup->ufs ) {

	case  FS_FDC:
		if ((cup->uend != LOGICAL_ENDFILE) || (cup->uspcproc==1)) {

			errn	= XRCALL(cup->ufp.fdc, backrtn) cup->ufp.fdc,
				&fst);

			if (errn < 0)
				return(fst.sw_error);
		}
		break;
 

	case  FS_TEXT:
	case  STD:
		/*
		 * Can't backspace pipes. sockets, ttys.  
		 */
		if (!cup->useek)
			return(FENOBKPI);	/* can't backspace pipe */
 
		if (!cup->ufmt)		/* if not formatted */
			return(FENOBKSP); /* can't backspace unblocked files */

		if (cup->uend) goto ok;

		first_read = 1;

		for ( ; ; ) {
			ssize_t i;
			size_t	ret;
			_ftelltype x, y;

			y	= LIBFTELL(cup->ufp.std);	
#ifdef KEY /* Bug 1678 */
			if (((_ftelltype) -1) == y) {
			  return FEINTUNK;
			}
#endif /* KEY Bug 1678 */
			x	= y;

			if (x < TBUFSZB)
				x	= 0;
			else
				x -= TBUFSZB;

#ifdef KEY /* Bug 1678 */
			if (LIBFSEEK(cup->ufp.std, x, 0)) {
			  return FEINTUNK;
			}
#else /* KEY Bug 1678 */
			(void) LIBFSEEK(cup->ufp.std, x, 0);
#endif /* KEY Bug 1678 */

			/*
			 * starting at the current character position
			 * minus 2 chars, search backwards for a newline 
			 * character.
			 */

        		ret	= fread(tbuf, 1,(size_t)(y - x), cup->ufp.std);

			if (ret < (y-x) && ferror(cup->ufp.std))
				return(FEBSPNRD);

			/* only the first time fread, we need to skip the
                         * first 2 chars
                         */
                        if (first_read) i = ret - 2;
                        else i = ret;
                        first_read = 0;

			tp	= tbuf;

        		for ( ; i >= 0; i--) {
  				if ( *(tp+i) != '\n') continue;
#ifdef KEY /* Bug 1678 */
				if (LIBFSEEK(cup->ufp.std, (_ftelltype)i+1-ret,
				  1)) {
				  return FEINTUNK;
				}
#else /* KEY Bug 1678 */
				(void) LIBFSEEK(cup->ufp.std, (_ftelltype)i+1-ret, 1);
#endif /* KEY Bug 1678 */
				goto ok;
			}

			if (x == 0) {
#ifdef KEY /* Bug 1678 */
				if (LIBFSEEK(cup->ufp.std, FTELLZERO, 0)) {
				  return FEINTUNK;
				}
#else /* KEY Bug 1678 */
				(void) LIBFSEEK(cup->ufp.std, FTELLZERO, 0);
#endif /* KEY Bug 1678 */
				goto ok;
        		} 
			else
				if (ret <= 0)	/* Should never occur */
					return(FEINTUNK);

#ifdef KEY /* Bug 1678 */
			if (LIBFSEEK(cup->ufp.std, x, 0)) {
			  return FEINTUNK;
			}
#else /* KEY Bug 1678 */
			(void) LIBFSEEK(cup->ufp.std, x, 0);
#endif /* KEY Bug 1678 */
		}
 
	case FS_BIN:
		return(FENOBKSP);


        case FS_AUX:
                errn	= FEMIXAUX;        /* BACKSPACE not allowed on a WAIO,
                                         * MSIO, DRIO, or AQIO file. */
                break;
	default:
		return(FEINTFST);
	}

ok:
	cup->uend	= BEFORE_ENDFILE;
	cup->ulastyp	= DT_NONE;
	cup->urecpos	= 0;

	return(0);
}
