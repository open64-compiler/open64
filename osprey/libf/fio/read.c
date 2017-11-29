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



#pragma ident "@(#) libf/fio/read.c	92.2	06/21/99 10:37:55"

/*
 *	read words
 */

#include <errno.h>
#include <foreign.h>
#include <fortran.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include "fio.h"

#define ret_err(errnum)	{	\
	*words	= 0;		\
	*status = errnum;	\
	goto done;		\
}

#define UBC	5	/* argument number for optional ubc parameter */

static void __READ();

#undef	READ

/*
 *	Read cray words, partial record mode
 */
void
READP(
	_f_int	*unump,
	_f_int	*uda,
	_f_int	*words,
	_f_int	*status,
	_f_int	*ubc)
{
	_f_int	locubc, *ubcp;

	if (_numargs() < UBC) {
		locubc	= 0;
		ubcp	= &locubc;
	}
	else
		ubcp	= ubc;


	__READ(PARTIAL, unump, uda, words, status, ubcp);
}

/*
 *	Read cray words, full record mode
 */
_f_int
READ(
	_f_int	*unump,
	_f_int	*uda,
	_f_int	*words,
	_f_int	*status,
	_f_int	*ubc)
{
	_f_int	locubc, *ubcp;

	if (_numargs() < UBC) {
		locubc	= 0;
		ubcp	= &locubc;
	}
	else
		ubcp	= ubc;

	__READ(FULL, unump, uda, words, status, ubcp);

	return(0);
}

/*
 *	Read words, full or partial record mode
 *
 *	The READ/READP interface does not advance past logical endfile 
 *	records.
 */
static void
__READ(
	int	fulp,
	_f_int	*unump,
	_f_int	*uda,
	_f_int	*words,
	_f_int	*status,
	_f_int	*ubc)
{
	register int	ret;
	int		rstat;
	int		wubc;
	long		wr;
	register unum_t	unum;
	unit		*cup;
	type_packet	tip;
	struct fiostate	cfs;

	unum	= *unump;
	wubc	= *ubc;

	STMT_BEGIN(unum, 0, T_RSU, NULL, &cfs, cup);
/*
 *	If not connected, do an implicit open. 
 */
	if (cup == NULL) {
		int	ostat;

		cup	= _imp_open(&cfs, SEQ, UNF, unum, 1, &ostat);

		if (cup == NULL) 
			ret_err(ostat);
	}

	if (!cup->ok_rd_seq_unf) {
		ret	= _get_mismatch_error(1, T_RSU, cup, &cfs);
		ret_err(ret);
	}

	cup->uwrt	= 0;	
	wr		= 0;
	tip.type90	= DVTYPE_TYPELESS;
	tip.type77	= -1;
	tip.intlen	= sizeof(long) << 3;
	tip.extlen	= tip.intlen;
	tip.elsize	= sizeof(long);
	tip.cnvindx	= 0;
	tip.count	= *words;
	tip.stride	= 1;

	ret	= _frwd(cup, uda, &tip, fulp, &wubc, &wr, &rstat);

	if ( ret == IOERR ) {
		if ( errno == FETAPUTE ) {
			*words	= wr;
			*status	= 4;
		}
		else if (errno >= 5) {
			*words 	= 0;
			*status	= errno;
		}
		else {
			*words	= 0;
			/* Map system errnos 1-4 to */
			/* library errno.  Otherwise, we would */
			/* lose them. */
			switch (errno) {
				case 1:
					*status	= FEKLUDG1;
					break;
				case 2:
					*status	= FEKLUDG2;
					break;
				case 3:
					*status	= FEKLUDG3;
					break;
				case 4:
					*status	= FEKLUDG4;
					break;
			}
		}
	}
	else {
		if ( rstat == EOR ) {
			cup->uend	= BEFORE_ENDFILE;
			*status		= 0;		/* EOR */
			*words		= ret;

			if ( ret == 0 ) 
				*status	= 1;	/* NULL record */
		}
		else if ( rstat == CNT ) {
			cup->uend	= BEFORE_ENDFILE;
			*status		= -1;	/* WORDS REMAIN in record */
			*words		= ret;
		}
		else if ( rstat == EOD) {
			*status		= 3;	/* EOD */
			*words		= 0;
			/* If the user assigned -s tape, return 2 instead of 3*/
			if (cup->ubmx)
				*status	= 2;
		}
		else {	/* rstat == EOF */
			cup->uend	= PHYSICAL_ENDFILE;
			*status		= 2;	/* EOF */
			*words		= 0;
		}
	}

done:
	*ubc	= wubc;

	STMT_END(cup, TF_READ, NULL, &cfs);

	return;
}
