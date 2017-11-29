/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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


#pragma ident "@(#) libu/aqio/aqwait.c	92.1	07/22/99 16:03:39"

#include <errno.h>
#include <liberrno.h>
#include <ffio.h>
#include <fortran.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/listio.h>
#include "aqio.h"

void _aqwait(AQFIL *, _f_int *, _f_int *, struct fflistreq *);

/*
 *	AQWAIT - wait for completion of queued i/o
 */

void
AQWAIT(
_f_int	*aqp,
_f_int	*status,
_f_int	*reply)
{
	AQFIL		*f;
	struct fflistreq *nxtq;
	_f_int		dummy, *lreply;

/*
 *	UNICOS 8.0 and previous quietly permitted fewer than 2 arguments,
 *	even though our documentatiokn for AQWAIT has required >= 2 args
 *	for some time.  Do the service of printing an error message if a
 *	dusty deck code happens to use < 2 arguments.
 */
	if (_numargs() < 2) 
		_lerror(_LELVL_ABORT, FEARGCNT, "AQWAIT", _numargs(), "2 or 3");
/*
 *	reply is an optional argument.
 */
	lreply = reply;
	if (_numargs() < 3) lreply = &dummy;

	f	= (AQFIL *) *aqp;

 	if (f == NULL || f->aq_chk != AQ_CHK) {
		*status = -FEAQBADH;    /* file handle is not valid */
		return;
	}

	if (f->aq_busy == f->aq_nxtq) {
		*status = IDLE;
		return;
	}

	*status = OK;

	AQ_LOCK(aq_lkbusy);
	nxtq = f->aq_nxtq;
	_aqwait(f, status, lreply, nxtq);
	AQ_UNLOCK(aq_lkbusy);
	if (*status < 0 && _numargs() <= 1)
		_lerror(_LELVL_ABORT, -(*status));
	return;
}

/*
 *	_aqwait is an internal routine that waits for a particular set of
 *	requests to finish.  It does no locking, so the aq_lkbusy lock,
 *	at least, must be set by the caller.
 */

void
_aqwait(
	AQFIL		*f,
	_f_int		*status,
	_f_int		*reply,
	struct fflistreq *ptr)
{
	struct fflistreq *base, *limit;
	int		ret, max;
	int		indx;

	base	= f->aq_base;
	limit	= f->aq_limit;
	max	= limit - base;
/*
 *	Start any queued requests.  Locking is done in aqcall, as well as
 *	checking to see if any requests need to be fired.
 */
	_aqcall(f);

/*
 *	Wait for completion of all outstanding requests.
 */
	while (f->aq_busy != ptr) {
		ret = _aqrcl(f, f->aq_busy);
		indx = f->aq_busy - base;
		if(ret != 0 && f->aq_told[indx] == 0) {
			*reply = f->aq_reqid[indx];
			f->aq_told[indx] = 1;	/* user has been told */
			INC_QP(f->aq_busy, limit, max);
			*status = -ret;
			return;
		}
		INC_QP(f->aq_busy, limit, max);
	}
	return;
}

/*
 *	AQRECALL - Wait for a particular I/O request, and all that
 *	preceded it.
 *	The 'reqid' parameter, if present and nonzero forces a wait
 *	until that I/O is complete.  If 'reqid' is not present, or zero,
 *	control is not returned until all outstanding I/O is complete.
 *	The 'reply' parameter is set if an error is encountered in
 *	waiting for preceding requests.  It is set to the request ID of
 *	the entry that got the error.
 */

void
AQRECALL(
	_f_int	*aqp,
	_f_int	*status,
	_f_int	*reqid,
	_f_int	*reply)
{
	AQFIL		*f;
	struct fflistreq *base, *limit, *nxtq, *busy, *eptr;
	int		i;
	int		max;
	int		inside;

	if (_numargs() < 4)
		_lerror(_LELVL_ABORT, FEARGCNT, "AQRECALL", _numargs(), "4");

	f	= (AQFIL *) *aqp;

 	if (f == NULL || f->aq_chk != AQ_CHK) {
		*status = -FEAQBADH;    /* file handle is not valid */
		return;
	}

	base	= f->aq_base;
	limit	= f->aq_limit;
	max	= limit - base;
/*
 *	Lock the tail of the queue so no one can stomp on the entry for
 *	which we are searching.
 */
	AQ_LOCK(aq_lkbusy);
	busy	= f->aq_busy;
/*
 *	Examine the entire queue, regardless of pointers
 */
	for (i = 0 ; i < max ; i++) {
		if ( f->aq_reqid[i] == *reqid ) {
			eptr = &base[i];
/*
 *			Determine if the entry in question is
 *			between busy and nxtq.
 */
			nxtq = f->aq_nxtq;	/* Grab stable copy */
			inside = NO;
			if (busy < nxtq) {
				if (busy <= eptr && eptr < nxtq)
					inside = YES;
			}
			else {
				if (eptr < nxtq || busy <= eptr)
					inside = YES;
			}
/*
 *			Now pointing to entry in question.
 *			Wait for its completion,  _aqwait will set status
 *			appropriately.  If not between nxtq and busy,
 *			assume that if we found it, it must be done.
 */
			*status = IOCOMPLETE;
			if (inside) {
				INC_QP(eptr, limit, max);
				_aqwait(f, status, reply, eptr);
			}
			AQ_UNLOCK(aq_lkbusy);

			return;
		}
	}
	AQ_UNLOCK(aq_lkbusy);
	*status = NOTFOUND;
	return;
}
