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


/* USMID @(#) libu/aqio/aqio.h	92.0	10/08/98 14:57:41 */


#include <cray/mtlock.h> 
#ifdef _CRAYT3D
/*
 * We don't have compound listio on the Cray-T3D. So, we have extra library
 * code that allows us to handle compound aqio requests. This is ifdef'd by
 * _MPP_LSTIOKLUDGE
 */
#define _MPP_LSTIOKLUDGE
#endif

/*
 *	Asynchronous Queued I/O File Table
 *	aq_busy	- pointer to the next entry that has not been recalled.
 *	aq_ptr	- pointer to the next entry to be issued to the system.
 *	aq_nxtq - pointer to the next available list entry.
 *
 *	The aq_busy and aq_nxtq pointers describe a circular queue. aq_ptr
 *	is always between them and marks the boundary between those that
 *	have been/are being issued to the system with listio() and those that
 *	have not. aq_fird marks the boundary between those that have been
 *      issued to the system with listio() and those that have not. It is
 *	usually the same as aq_ptr. It lags aq_ptr while the requests
 *	are actually being issued with listio(). 
 *
 *	Locks are provided to ensure that when the pointers are updated,
 *	the update can be performed indivisibly.  Copies are taken of the
 *	pointers in various places and the copied pointers are not always
 *	locked.  Since all pointers only advance, the copies will remain
 *	sufficiently valid for the current operation.
 */
typedef struct {
	long		aq_chk;		/* check word			   */
	struct fflistreq *aq_base;	/* base of aqio listio buffer      */
	struct fflistreq *aq_busy;	/* ptr to next entry not recalled  */
	struct fflistreq *aq_ptr;	/* pointer to next entry to be 	   */
					/* issued to the system            */
	struct fflistreq *aq_nxtq;	/* pointer to next available entry */
	struct fflistreq *aq_fird;	/* indicates which have been fired */
	struct fflistreq *aq_limit;	/* limit of aqio listio buffer     */

	int		*aq_reqid;	/* pointer to aqio request ids     */
	int		*aq_told;	/* user has been notified?         */
/*
 * Since more than one can be held at a given time, the following three
 * locks must be locked in the order that they appear here.
 */
	long		aq_lknxtq;	/* lock word for aq_nxtq	   */
	long		aq_lkbusy;	/* lock word for aq_busy	   */
	long		aq_lkptr;	/* lock word for aq_ptr		   */
	long		aq_lkfird;	/* lock word for aq_fird	   */

	int		aq_ffio;	/* NO for system I/O, YES for FFIO */
	void		*aq_up;		/* points to entry in unit table   */
#ifdef _MPP_LSTIOKLUDGE
	/* when we have compound listio, this can be removed */
	struct compound *aq_cmp_ptr;
#endif
}AQFIL;

#ifdef _MPP_LSTIOKLUDGE
	/* when we have compound listio, this can be removed */
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define DEFMAXCMP 40
struct compound{
	int				cmp_maxreq;		/* maximum number of compound requests */
	struct fflistreq *cmp_base;	/* base of listio structure for compound req. */
	struct fflistreq *cmp_nxt;	/* next one to be used when filling cmp_base */
	struct fflistreq *cmp_unrcalld; /* 1st one unrecalled */
	struct fflistreq **cmp_desc;	/* base of pointers back to original requests */
};
#endif

#define COUNT_QE(endptr, bptr, maxct)					\
		((((endptr) - (bptr)) >= 0) ?				\
			((endptr) - (bptr)) : (((endptr) - (bptr)) + (maxct)))

#define INC_QP(qp, qlimit, maxct)					\
				{					\
				if (((qp)+1) == qlimit)			\
					(qp) -= ((maxct)-1);		\
				else					\
					(qp)++;				\
				}
#ifndef NULL
#define NULL	0
#endif

#ifndef NO
#define NO	0
#endif

#ifndef YES
#define YES	1
#endif

#define MAXRECALL 1000000	/* a whole bunch */

/* 
 * AQSTAT return values 
 */
#define	NOTFOUND	0	/* request ID is not in queue */
#define	FULL		1	/* queue is full (not used) */
#define	LQUEUED		2	/* Request is in the library queue */
#define	STUCK		3	/* queue is stuck (unused) */
#define	QUEUED		4	/* request has been issued to the system */
#define	IOCOMPLETE	5	/* request is complete */

/*
 * AQWAIT return values
 */
#define	OK	0
#define	IDLE	2

# define AQ_LOCK(lockvar) MEM_LOCK(&(f->lockvar))
# define AQ_UNLOCK(lockvar) MEM_UNLOCK(&(f->lockvar))

#define AQ_CHK	'<=AQIO=>'

/*
 *	AQPOLL - For FFIO files only!  Call fffcntl with
 *		FC_ASPOLL to give the layer a shot at processing
 *		completion(s).  This is normally called only if
 *		the sw_flag bit is set in the ffsw.
 *
 *		AQPOLL(struct fflistreq *eptr, struct ffsw *ffstat);
 */

#define	AQPOLL(eptr, ffstat)						\
									\
		(void)XRCALL(GETIOB(eptr->li_fildes), fcntlrtn)		\
			GETIOB(eptr->li_fildes),			\
			FC_ASPOLL,					\
			eptr->li_status,				\
			ffstat);
