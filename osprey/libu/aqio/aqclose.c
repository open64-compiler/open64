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


#pragma ident "@(#) libu/aqio/aqclose.c	92.1	07/01/99 13:50:36"

#include <sys/types.h>
#include <errno.h>
#include <liberrno.h>
#include <ffio.h>
#include "aqio.h"
 
/*
 *	AQCLOSE - close file for asynchronous queued i/o
 */

void
AQCLOSE(aqp,ustatus,reply)
long *aqp;
long *ustatus;
long *reply;
{
	AQFIL		*f;
	struct fflistreq *list, *limit;
	int		ret;
	int		max;
	int		qd;
	int		indx;
	long		*status, dummy;

/*
 *	Don't store to 'status' if not passed.
 */
	status = &dummy;
	if (_numargs() > 1) status = ustatus;

	*status = 0;

	f	= (AQFIL *) *aqp;

	if (f == NULL || f->aq_chk != AQ_CHK) {
		*status = -FEAQBADH;	/* invalid file handle */
		goto erret;
	}
		
	limit	= f->aq_limit;
	max	= limit - f->aq_base;

	AQ_LOCK(aq_lknxtq);			/* lock em just in case... */
	AQ_LOCK(aq_lkptr);
	qd = COUNT_QE(f->aq_nxtq, f->aq_ptr, max);
	AQ_UNLOCK(aq_lkptr);
	AQ_UNLOCK(aq_lknxtq);
	if (qd > 0) _aqcall(f);

/*
 *	Lock the queue only to encourage errors if another task is doing I/O
 *	while the CLOSE is in progress.
 */
	AQ_LOCK(aq_lknxtq);
	AQ_LOCK(aq_lkbusy);
	AQ_LOCK(aq_lkptr);			/* Lock em all */
	while ( f->aq_busy != f->aq_ptr ) {
		ret = _aqrcl(f, f->aq_busy);
		indx = f->aq_busy - f->aq_base;
		INC_QP(f->aq_busy, limit, max);
/*
 *		If outstanding error, and user not told...
 */
		if (ret != 0 && f->aq_told[indx] == 0) {
			if (_numargs() > 2) *reply = f->aq_reqid[indx];
			f->aq_told[indx] = 1;	/* why not? */
			*status = -ret;
		}
	}

	*aqp	= 0;		/* sever user's connection */

	list = f->aq_base;
	if (f->aq_ffio == YES)
		ret = ffclose(list->li_fildes);	/* no stat, will set errno */
	else
		ret = close(list->li_fildes);

	AQ_UNLOCK(aq_lkptr);
	AQ_UNLOCK(aq_lkbusy);
	AQ_UNLOCK(aq_lknxtq);

	if (ret < 0)
		*status = -errno;

	(void) free(list->li_status);	/* status words all in one block */
	(void) free(f->aq_reqid);
	(void) free(f->aq_told);
#ifdef _MPP_LSTIOKLUDGE
	(void) free(f->aq_cmp_ptr->cmp_base->li_status);
	(void) free(f->aq_cmp_ptr->cmp_desc);
	(void) free(f->aq_cmp_ptr->cmp_base);
	(void) free(f->aq_cmp_ptr);
#endif
	if (f->aq_up != NULL)		/* if connected to Fortran unit */
		_clruptr(f->aq_up);
/*
 *	Mark the file handle structure as invalid before freeing the memory.   
 */
	f->aq_chk = 0;			
	(void) free(f);
erret:
	if (*status != 0 && _numargs() <= 1) 
		_lerror(_LELVL_ABORT, -(*status));
	return;
}
