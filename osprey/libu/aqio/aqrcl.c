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


#pragma ident "@(#) libu/aqio/aqrcl.c	92.1	07/01/99 13:50:36"

#include <sys/types.h>
#include <sys/param.h>
#include <sys/iosw.h>
#include <sys/listio.h>
#include <ffio.h>
#include <liberrno.h>
#include <cray/nassert.h>
#include "aqio.h"

/*
 *	aqrcl - recall one asynchronous i/o list entry.  The recall is
 *		always done on the oldest entry.  The aq_lkbusy lock MUST
 *		be set by the caller prior to the call.
 *
 *	Return value:
 *		>0 is error number
 *		 0 if no error
 */

_aqrcl(f, eptr)
AQFIL *f;
struct fflistreq *eptr;
{
	struct ffsw	*status;
	struct ffsw	ffstat;
	struct fdinfo	*fio;
	int		ret = 0;
	int		ct = 0;
	int		btwn = 0;
	struct fflistreq *fird;
	struct fflistreq *ptr;

	status = eptr->li_status;
	ct = 0;
/*
 *	if eptr is between aq_fird and aq_ptr, then the listio() system
 *	call has not yet been issued for it. In that case, use the lock 
 *	aq_lkfird to wait until it has been started.
 */
	fird = f->aq_fird;	/* Grab stable copies */
	ptr = f->aq_ptr;
	if (ptr >= fird) 
		{
		if (eptr >= fird && eptr < ptr)
			btwn = 1;
		}
	else
		{
		if (eptr >= fird || eptr < ptr)
			btwn = 1;
		}
	if (btwn != 0) 
		{
		if ((f->aq_ffio == YES && FFSTAT(*status) == 0) ||
			(f->aq_ffio != YES && status->sw_flag == 0))
			{
			AQ_LOCK(aq_lkfird);
			AQ_UNLOCK(aq_lkfird);
			}
		}
	if (f->aq_ffio == YES)
		{
/*
 *		Note that we wait for FFSTAT to be set, not sw_flag.
 *		This must be done to guarantee RECALL a shot at cleanup
 *		for the request.
 */
		fio = GETIOB(eptr->li_fildes);
		while (FFSTAT(*status) == 0)
			{
			(void)XRCALL(fio, fcntlrtn) fio, FC_RECALL,
						status, &ffstat);
			if (ct++ > MAXRECALL)
				return(FEINTUNK);
			}
		}
	else
		{
		while (status->sw_flag == 0)
			{
#ifdef _MPP_LSTIOKLUDGE
			/* remove when we have support for compound listio */
			if (eptr->li_nstride > 1) {
				/* A compound request. Wait for all the simple requests */
				/* that comprise it. */
				cmp_recall(f, eptr->li_status->sw_sptr);
			}
			else 
#endif
			{
			_recall(eptr->li_fildes, 1, (struct iosw **)&status);
			if (ct++ > MAXRECALL)
				return(FEINTUNK);
			}
			}
		}
	if (status->sw_count != eptr->li_nbyte*eptr->li_nstride)
		{
		ret = FERDWRER;
		}
	if (status->sw_error != 0)
		{
		ret = status->sw_error;
		}

	return(ret);
}
