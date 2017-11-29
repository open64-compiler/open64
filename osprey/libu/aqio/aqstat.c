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


#pragma ident "@(#) libu/aqio/aqstat.c	92.1	07/01/99 13:50:36"

#include <errno.h>
#include <ffio.h>
#include <fortran.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/listio.h>
#include "aqio.h"

/*
 *	AQSTAT - check status of a particular I/O request.
 *	The optional 'wait' parameter, if present and nonzero forces a wait
 *	until that I/O is complete.
 */

void
AQSTAT(
_f_int	*aqp,
_f_int	*reply,
_f_int	*reqid,
_f_int	*status,
_f_int	*wait)
{
	AQFIL		*f;
	struct fflistreq *base, *limit, *ptr, *busy, *eptr;
	struct ffsw	*istat;
	struct ffsw	dumstat;
	int		indx;
	int		ret;
	int		max;
	int		isbusy;

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
	AQ_LOCK(aq_lknxtq);
	AQ_LOCK(aq_lkbusy);		/* don't nobody move */
	busy	= f->aq_busy;
/*
 *	Examine the entire queue, regardless of pointers
 */
	for (indx = 0 ; indx < max ; indx++) {
		if ( f->aq_reqid[indx] == *reqid ) {
			eptr = &base[indx];
			istat = base[indx].li_status;
			ptr = f->aq_ptr;	/* Grab stable copy */
						/* If it moves, that's OK. */
/*
 *			If the user requested that we wait for this one...
 */
			if (_numargs() > 4 && *wait != 0) {
/*
 *				Determine if the entry in question is
 *				between busy and ptr.
 */
				isbusy = NO;
				if (busy <= ptr) {
					if (busy <= eptr && eptr < ptr)
						isbusy = YES;
				}
				else {
					if (eptr < ptr || busy <= eptr)
						isbusy = YES;
				}
/*
 *				Now pointing to entry in question.
 *				Wait for its completion,  if btwn busy and ptr,
 *				Unlock queue head to allow additions
 *				to the queue while waiting.
 */
				AQ_UNLOCK(aq_lknxtq);
/*
 *				If the requested reqid has not been fired,
 *				fire the listio before the wait.
 */
				if (isbusy == NO) {
					_aqcall(f);
				}
				goto finish_io2;
			}
/*
 *			If user did not request wait, just return the status,
 *			but first check to see if it is done.  If it is, Give
 *			the recall routine a shot at cleanup and finish up as
 *			though a wait had been requested.
 */
			if (f->aq_ffio == YES) {
				AQPOLL(eptr, &dumstat);
				if (FFSTAT(*istat) != FFBSY) {
					goto finish_io1;
				}
			}
			else if ( istat->sw_flag != 0 ) {
					goto finish_io1;
			}

/* 
 *			If the entry is found, but not complete, it is
 *			either QUEUED and active or QUEUED and not yet
 *			activated.
 *			If eptr is between busy and ptr, it is active.
 */
			*status = LQUEUED;	/* not yet active */
			if (busy <= ptr) {
				if (busy <= eptr && eptr < ptr)
					/* queued and active */
					*status = QUEUED;
			}
			else {
				if (eptr < ptr || busy <= eptr)
					/* queued and active */
					*status = QUEUED;
			}

			goto done;
		}
	}
	*status = NOTFOUND;

done:
	AQ_UNLOCK(aq_lkbusy);
	AQ_UNLOCK(aq_lknxtq);
	return;

finish_io1:
	AQ_UNLOCK(aq_lknxtq);

finish_io2:
/*
 *	Do the 'recall' on the request.
 */
	ret = _aqrcl(f, eptr);
/*
 *	The request is now done.  Now that we are about to return the status
 *	of the request, check if this entry is at tail of queue.
 *	If the request is at the tail of the queue free up the entry.
 *	This is probably silly.
 */
	if (f->aq_busy == eptr)
		INC_QP(f->aq_busy, limit, max);

/*
 *	Set return status
 */
	*status = IOCOMPLETE;
	if (ret != 0)
		{
		*status = -ret;
		if (ret == FERDWRER)
			*reply = eptr->li_status->sw_count;
		}
/*
 *	Flag the status as delivered
 */
	f->aq_told[indx] = 1;	/* mark status as having been sent to user */

	AQ_UNLOCK(aq_lkbusy);

	return;
}
