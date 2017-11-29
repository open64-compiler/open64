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


#pragma ident "@(#) libu/aqio/aqcall.c	92.1	07/01/99 13:50:36"
#include <cray/nassert.h>
#include <errno.h>
#include <ffio.h>
#include <fortran.h>
#include <liberrno.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/listio.h>
#ifdef _CRAYMPP
#include <sys/iosw.h>
#endif
#include "aqio.h"

extern int G@INTIO;

/*
 *	aqcall - issue a listio request for any queued requests.
 *		No wait operations are performed.
 *
 *	Return value:
 *		0 - errors are returned in the status words on the
 *			individual requests.
 */

void
_aqcall(f)
AQFIL *f;
{
	register int	nent,nadd;
	struct fflistreq *ptr, *nxtq, *limit, *tmpptr;
	struct ffsw	*istat;
	int lkfird = 0;
	int i, istrt;

	limit = f->aq_limit;
/*
 *	Take a copy of nxtq, and lock ptr for the increment.
 *	Don't lock ptr over the listio() syscall.  This is safe,
 *	as all of the entries between ptr and nxtq get fired regardless of
 *	errors, and the danger of nxtq overrunning ptr during this routine
 *	is eliminated by the fact that _aqenq() will ONLY use entries that
 *	are complete.  This depends on _aqenq() setting the sw_flag to zero.
 *	This all will allow us to operate without locking nxtq.
 *	Additional requests can be queued (but not fired) by other
 *	tasks while we are busy doing this.
 *	aq_fird is used to indicate which of the entries have been fired.
 *	aq_fird is used in _aqrcl() to ensure that we do not try to recall
 *	entries that have not been started.
 */
	AQ_LOCK(aq_lkptr);
	ptr = f->aq_ptr;
	nxtq = f->aq_nxtq;
	if (ptr != nxtq) {
		lkfird = 1;
		AQ_LOCK(aq_lkfird);
	}
	f->aq_ptr = nxtq;
	AQ_UNLOCK(aq_lkptr);

	while (ptr != nxtq) {
/*
 *		If wrapped around end, break into parts.
 */
		nadd = nxtq - ptr;
		if (nadd < 0)
			nadd = limit - ptr;
/*
 *		Fire the listio request
 */
		do {
			if (f->aq_ffio == YES) {

/*
 *		fflistio needs to be locked, and the aq_lkfird accomplishes
 *		that.
 */
				nent = fflistio(LC_START, ptr, nadd);
			}
			else
#ifdef _MPP_LSTIOKLUDGE
				nent = _mpp_aq_listio(f, LC_START, (struct listreq *)ptr,
						nadd);
#else
				nent = listio(LC_START, (struct listreq *)ptr,
						nadd);
#endif
		} while (nent == -1 && errno == EINTR && G@INTIO == 0);
		if (nent == -1) {
			/* Should never happen. */
			/* Mark all of them as bad. */
			for (i = 0; i < nadd; i++) {
				istat = ptr->li_status;
				if (istat->sw_error == 0)
					istat->sw_error = errno;
				istat->sw_flag = 1;	/* it is done */
				FFSTAT(*istat) = FFERR;
				ptr++;
			}
			nadd = 0;	/* ptr is already adjusted */
		}
/*
 *		See if any of the requests failed because of EINTR. 
 *		If any did, we will restart them, providing all of the
 *		subsequent requests also failed because of EINTR and there
 *		were no other kinds of failures. Because we know that
 *		all requests have LF_LSEEK and we did not open the file
 *		with append, it should be safe to restart.
 */
		else if ((G@INTIO == 0) && (nent < nadd)) {
			istrt = -1;
			for (i = 0; i < nadd; i++) {
				istat = ptr->li_status;
				if (istat->sw_error != 0) {
					if (istat->sw_error != EINTR) {
						ptr += nadd -i;
						break;
					}
					istrt = i;
					break;
				}
				ptr++;
			}
			if (istrt != -1) {
				tmpptr = ptr + 1;
				for (i = istrt+1; i < nadd; i++) {
					istat = tmpptr->li_status;
					if (istat->sw_error != EINTR) {
						ptr += nadd -istrt;
						break;
					}
					tmpptr++;
				}
			}
		} 
		else {
			ptr += nadd;	
		}

/*
 *		If wrap around, reset pointer to base
 */
		if (ptr == limit) ptr = f->aq_base;
		f->aq_fird = ptr;
	}
	if (lkfird != 0)
		AQ_UNLOCK(aq_lkfird);

	return;
}

/*
 *	Enqueue an AQIO request.
 *	Add one request to the circular queue after ensuring that at
 *	least one slot is free.  If the queue becomes full, invoke
 *	_aqcall() to start the requests.
 */
void
_aqenq(
int	rdwr,
_f_int	*aqp,
void	*cpuadd,
_f_int	*mstride,
_f_int	*blknum,
_f_int	*blocks,
_f_int	*dstride,
_f_int	*incs,
_f_int	*reqid,
_f_int	*queue,
_f_int	*status)
{
	int		told;
	AQFIL		*f;
	struct fflistreq *base, *limit, *busy, *nptr, *nxtq;
	int		indx;
	int		nxtindx;
	int		max;
	int		ret;
	int		nqd;
	int		nfree;
	struct ffsw	*istat;

	f	= (AQFIL *) *aqp;

	if (f == NULL || f->aq_chk != AQ_CHK) {
		*status = -FEAQBADH;	/* invalid file handle */
		return;
	}

	base	= f->aq_base;
	limit	= f->aq_limit;
	max	= limit - base;

	*status = OK;

	AQ_LOCK(aq_lknxtq);		/* We are moving this, lock it. */
	nxtq = f->aq_nxtq;
/*
 *	If queue is full, make sure that at least one entry is free.
 *	Hold the lock on nxtq to make sure that no one else steals the slot
 *	that we free up. Hold aq_lkbusy to call aqrcl().
 */
	AQ_LOCK(aq_lkbusy);
	busy	= f->aq_busy;
	nfree = max - COUNT_QE(nxtq, busy, max);
	if (nfree <= 1) {
/*
 *		If we are going to wait for it to be done, make sure it
 *		gets started!
 */
		if (busy == f->aq_ptr) _aqcall(f);
		ret = _aqrcl(f, busy);	/* Wait for next slot */
/*
 *		one way or another, we don't want to see this one again.
 *		Bump the aq_busy pointer, but only report an error, if
 *		the user has not yet seen it.
 */
		indx = busy - base;
		told = f->aq_told[indx];	/* protect 'told' over bump */
		INC_QP(busy, limit, max);	/* bump pointer */
		f->aq_busy = busy;
/*
 *		If an error, and not yet told, return an error.  Don't set the
 *		told flag, as that's not safe after bumping aq_busy.
 *		The entry in question is now 'outside' so it no longer
 *		matters.
 */
		if (ret != 0 && told == 0) {	/* if err & user not yet told */
			AQ_UNLOCK(aq_lkbusy);
			AQ_UNLOCK(aq_lknxtq);
			*status = -ret;
			return;
		}
	}
	AQ_UNLOCK(aq_lkbusy);		/* let others move busy now. */
/*
 *	We are now guaranteed at least one free slot.  Find it and use it.
 */
	nxtindx = f->aq_nxtq - base;
	nptr = f->aq_nxtq;
	nptr->li_opcode = rdwr;
	nptr->li_buf = (char *)cpuadd;
	nptr->li_offset = *blknum*BSIZE;
	nptr->li_memstride = *mstride*NBPW;
	nptr->li_filstride = *dstride*BSIZE;
	nptr->li_nbyte = *blocks*BSIZE;
	nptr->li_nstride = *incs+1;
#ifdef	_MPP_LSTIOKLUDGE
	if (*incs  == 0) {
/*
 *		If *incs == 0, then we have a simple request.  In this
 *		case, it really should not matter what filstride and
 *		memstride are set to, but the MPP kernel will reject
 *		anything other than 0 or 1.
 */
		nptr->li_filstride = 0;
		nptr->li_memstride = 0;
	}
#endif

	f->aq_reqid[nxtindx] = *reqid;
	f->aq_told[nxtindx] = 0;	/* user not told yet */
/*
 *	Clear the status return word
 */
	istat = nptr->li_status;
	CLRSTAT(*istat);
	FFSTAT(*istat) = 0;

	INC_QP(f->aq_nxtq, limit, max);
	nqd = COUNT_QE(f->aq_nxtq, f->aq_ptr, max);
	AQ_UNLOCK(aq_lknxtq);
/*
 *	If we have been requested to issue the request, or the queue
 *	is full of unissued requests, fire them off.  _aqcall has its
 *	own locking, so we don't need to be locked. aq_lkptr should
 *	be locked before we look at it, but the worst that can happen
 *	would be that we can look at it just before another task bumps
 *	it.  If more than one task detects the full condition and invokes
 *	_aqcall, that's OK.  Once in _aqcall, the lock will be set, and if
 *	the listio() is no longer necessary, _aqcall will simply return.
 */
	if (*queue == 0 || nqd >= (max-1)) {
		_aqcall(f);
	}
	return;
}
#ifdef _MPP_LSTIOKLUDGE

void
cmp_recall(AQFIL *f, struct fflistreq *last)
{
	/* recall all requests between and including cmp_unrcalld and last */

	int indx;
	int done;
	int ct = 0;
	struct fflistreq *nxtrcl;
	struct compound *cmpptr;
	struct fflistreq *lptr;
	int ijk;
	int maxcompound;

	cmpptr = f->aq_cmp_ptr;
	nxtrcl = cmpptr->cmp_unrcalld;
	indx = nxtrcl - cmpptr->cmp_base;
	done = 0;
	maxcompound = cmpptr->cmp_maxreq;
	assert(last >= cmpptr->cmp_base);
	assert(last <= cmpptr->cmp_base+maxcompound-1);
	assert(indx >= 0 && indx < maxcompound);
	do {
		while (nxtrcl->li_status->sw_flag == 0) {
			_recall(nxtrcl->li_fildes, 1, &(nxtrcl->li_status));
			if (ct++ > MAXRECALL)
				nxtrcl->li_status->sw_error = FEINTUNK;
		}		
		lptr = cmpptr->cmp_desc[indx];
		if (lptr != NULL) {
			lptr->li_status->sw_count += nxtrcl->li_status->sw_count;
			cmpptr->cmp_desc[indx] = NULL;
			if (nxtrcl->li_status->sw_error != 0)
				lptr->li_status->sw_error = nxtrcl->li_status->sw_error;
		}
		if (nxtrcl == last){
			done = 1;
		}
		indx++;
		nxtrcl++;
		if (indx == maxcompound) {
			indx = 0;
			nxtrcl = cmpptr->cmp_base;
		}
		if (cmpptr->cmp_desc[indx] != lptr && lptr != NULL) {
			/* done with this compound request */
			lptr->li_status->sw_sptr = 0;
			lptr->li_status->sw_flag = 1;
#ifdef DEBUG
			for (ijk = 0; ijk < maxcompound; ijk++) {
				assert(cmpptr->cmp_desc[ijk] != lptr);
			}
#endif
		}
	}while (!done);
	cmpptr->cmp_unrcalld = nxtrcl;	
}


/*
 * This version of listio handles compound requests
 * It is meant to be called only by AQIO, because it
 * assumes that LF_LSEEK is always set (true in AQIO) and
 * LF_LWAIT is never set (true in AQIO)
 */


static int
_mpp_aq_listio(AQFIL *f, int cmd, struct fflistreq *list, int nent)
	{
	int i,  ret;
	int flag = 0;
	int numdone = 0;
	int compnd,idone;
	struct fflistreq *lptr;

	if (nent < 0)
		{
		errno = EINVAL;
		return(-1);
		}
	/* See if there are any compound requests */
	for (compnd = 0; compnd < nent; compnd++) {
		if (list[compnd].li_nstride > 1) 
			break;
	}
	if (compnd == nent) {
		return(listio(cmd, (struct listreq *)list, nent));
	}
	idone = -1;
	for ( ; ; ) {
		if (compnd > (idone + 1)) {
			/* have some unissued entries before the compound one */
			ret = listio(cmd, (struct listreq *)(list+idone+1), compnd-idone-1);
			if (ret < 0) {
				if (idone == -1)
					return(ret);
				else {
					/* Mark all the rest as in error */
					lptr = list+idone+1;
					for (i = idone+1; i < nent; i++) {
						lptr->li_status->sw_count=0;
						lptr->li_status->sw_error = errno;
						lptr->li_status->sw_flag = 1;
						lptr++;
					}
					return(numdone);
				}
			}
			numdone += ret;
		}
		/* Take care of the compound one */
		if (compnd < nent) {
			numdone++;
			ret = compoundreq(f, cmd, &list[compnd]);
			if (ret < 0) {
				/* doesn't matter */
				/* The individual status will be marked */
			}
		}
		idone = compnd;	
		if (idone == nent-1)
			return(numdone);
		for (compnd = idone+1; compnd < nent; compnd++) {
			if (list[compnd].li_nstride > 1)
				break;
		}
	}
	}

/* This macro issues the broken up compound request. */
/* cmd is the listio request. ptr points to the listreq structure. It */
/* contains only simple requests. Sadd is the number of entries in the  */
/* request. */
#define _CMP_LISTIO(cmd, ptr, sadd, ij) {						\
	struct fflistreq *sptr;									\
	struct fflistreq *tmpptr;								\
	int nadd,istrt;											\
															\
	sptr = ptr;												\
	nadd = sadd;											\
issue##ij:														\
	do {													\
		ret = listio(cmd, (struct listreq *)sptr, nadd);	\
	} while (ret == -1 && errno == EINTR && G@INTIO == 0);	\
	if (ret == -1) {										\
		/* should never happen. */							\
		/* wait for other requests that might comprise */	\
		/* this compound request */							\
		/* First mark all the requests that make up this */ \
		/* one as done. */									\
		for (i = 0; i < nadd; i++) {						\
			istat = sptr[i].li_status;						\
			istat->sw_flag = 1;								\
		}													\
		if (sptr == cmpptr->cmp_base)						\
			cmp_recall(f,cmpptr->cmp_base + maxcompound -1);\
		else												\
			cmp_recall(f,sptr -1);							\
		lptr->li_status->sw_error = errno;					\
		lptr->li_status->sw_flag = 1;						\
		return(-1);											\
	}														\
	else if ((ret < nadd) && (G@INTIO == 0)) {				\
		istrt = -1;											\
		for (i = 0; i < nadd; i++) {						\
			istat = sptr->li_status;						\
			if (istat->sw_error != 0) {						\
				if (istat->sw_error != EINTR) {			\
					break;									\
				}											\
				istrt = i;	/* First one with EINTR */		\
				break;										\
			}												\
			sptr++;												\
		}													\
		if (istrt != -1) {									\
			tmpptr = sptr+1;								\
			for (i = istrt+1; i < nadd; i++ ) {				\
				istat = tmpptr->li_status;					\
				if (istat->sw_error != EINTR) {				\
					goto done##ij;								\
				}											\
				tmpptr++;									\
			}												\
/* sptr is first one that got EINTR */						\
			nadd  = sadd - (sptr  - ptr);					\
			goto issue##ij;										\
		}													\
	}														\
done##ij:														\
	;				\
}
/* compoundreq breaks a compound request into multiple simple requests. */
/* The simple requests are issued as one or more listio calls. */
/* lptr points to the compound request */
static int
compoundreq(f, cmd, lptr)
AQFIL *f;
int cmd;
struct fflistreq *lptr;
{
	int xfer;
	int j;
	int ret;
	int fstride, mstride;
	struct ffsw *istat;
	char *buf;
	int nb,pos,nstr,fd,opcode;
	int available;
	struct compound *cmpptr;
	struct fflistreq *nxt;
	struct fflistreq *initnxt;
	struct fflistreq *clist;
	struct fflistreq **cdesc;
	int indx, maxcompound;
	int i;
	

	nb = lptr->li_nbyte;
	nstr = lptr->li_nstride;
	pos = lptr->li_offset;
	fstride = lptr->li_filstride;
	mstride = lptr->li_memstride;
	if (mstride == 0)
		mstride = nb;
	if (fstride == 0)
		fstride = nb;
	xfer = 0;
	fd = lptr->li_fildes;
	buf = lptr->li_buf;
	opcode = lptr->li_opcode;

	cmpptr = f->aq_cmp_ptr;
	nxt = cmpptr->cmp_nxt;
	maxcompound = cmpptr->cmp_maxreq;
	available = maxcompound - (nxt -cmpptr->cmp_base );
#ifdef DEBUG
	/* go through description loop and make sure none match this request */
	for (i = 0; i < maxcompound; i++) {
		assert(cmpptr->cmp_desc[i] != lptr);
	}
#endif
	/* make sure we wait for the requests we will be writing over. */
	if (available >= nstr) {
		cmp_recall(f,nxt + nstr -1);
	}
	else {
		/* We don't have enough room left in the structure for all of the */
		/* compound requests, so we will have to break up into chunks. */
		/* wait for the first chunk. */
		cmp_recall(f,cmpptr->cmp_base + maxcompound -1);
	}
	initnxt = nxt;
	indx = nxt - cmpptr->cmp_base;
	clist = cmpptr->cmp_base;
	cdesc = cmpptr->cmp_desc;

	for (j = 0 ; j < nstr ; j++) {
			
		assert(nxt <= cmpptr->cmp_base + maxcompound -1);

		clist[indx].li_nbyte = nb;
		clist[indx].li_offset = pos;
		clist[indx].li_buf = buf;
		clist[indx].li_opcode = opcode;
		istat = clist[indx].li_status;
		istat->sw_error = 0;
		istat->sw_flag = 0;
		istat->sw_count = 0;
		buf += mstride;
		pos += fstride;
		cdesc[indx] = lptr;	/* point back to original listio req */
		
		if ((indx ==  maxcompound -1) && (j < nstr-1)) {
			/* fire it off , starting at initnxt */
			_CMP_LISTIO(cmd, initnxt, nxt-initnxt+1, 1);
			initnxt = cmpptr->cmp_base;
			cmp_recall(f,cmpptr->cmp_base + MIN(maxcompound,nstr-(j+1)) -1);
			indx = 0;
			initnxt = nxt = cmpptr->cmp_base;
		}
		else {
			nxt++;
			indx++;
		}
	}
	/* In the course of breaking up this request, the sw_flag might have */
	/* been set by the cmp_recall routine. That could happen if we had */
	/* to recall some of the simple requests that make up this compound */
	/* request. Zero it again, because we know we aren't done. */
	lptr->li_status->sw_flag = 0; 

	/* We always start the requests that comprise the compound request. */
	/* That way, requests are kept in order. */
	_CMP_LISTIO(cmd, initnxt, nxt-initnxt, 2);

	/* Since we know that we aren't using the sw_sptr field of the ffsw */
	/* of the original compound request, use it now to store the pointer */
	/* to the last request that makes up the compound request. */
	lptr->li_status->sw_sptr = nxt-1;
	if (indx == maxcompound)
		cmpptr->cmp_nxt = cmpptr->cmp_base;
	else
		cmpptr->cmp_nxt = nxt;
	
	return(0);
}
#endif
