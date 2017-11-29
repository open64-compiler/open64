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


#pragma ident "@(#) libu/ffio/fflistio.c	92.1	06/29/99 13:16:47"

#include <sys/types.h>
#if !defined(_ABSOFT)
#include <sys/iosw.h>
#include <sys/listio.h>
#else
#include <cray/iosw.h>
#include <cray/listio.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <ffio.h>
#include <stdlib.h>
#include "sysio.h"
#define MAXRECALL	1000000	/* lots and lots */

int _ffcompound(struct fflistreq *ptr);
static void
_seterr(struct fflistreq *list, int num, struct fdinfo *fio, int ernum);

/*
 * fflistio() is a glue routine that allows listio(2) users to substitute
 * FFIO.  If the lower layer can handle listio(), then it calls that.
 * Otherwise, it breaks up the request into separate seek and i/o calls
 * to the appropriate layers.
 */


fflistio(int cmd, struct fflistreq *list, int nent, ...)
	{
	int narg;
	int i, j, nb, ret, k, ierr, curent;
	int slice;
	int flag = 0;
	int nstr;
	int pos;
	bitptr buf;
	int zero;
	int numdone;
	int numstarted;
	struct fdinfo *fio;
	struct fdinfo *nextfio;
	struct fdinfo *flsh_fio;
	struct ffsw *stat;	/* stat on fflistio call */
	struct ffsw *istat;	/* stat on individual request(s) */
	struct ffsw locstat;	/* stat for 'internal' request(s) */
	struct ffsw dumstat;	/* Dummy status, bit bucket */
	struct fflistreq *loclist = NULL; /* requests may be copied here */
	int *started_list;
	va_list ap;
	int istrt;
	int done;
	int lastmatch;

/*
 *	Point stat to status passed in, if present
 */
#ifdef	_CRAY
	NUMARG(narg);
#else
	narg = 4;
#endif
	stat = &dumstat;
	if (narg > 3)
		{
		va_start(ap, nent);
		stat = va_arg(ap, struct ffsw *);
		}

	if (nent < 0)
		{
		_SETERROR(stat, FDC_ERR_REQ, 0);
		if (narg < 4) errno = dumstat.sw_error;
		return(ERR);
		}

	numdone = 0;
/*
 *	Examine each request. For those that can be handled by
 *	a listio routine in the layer, call that. If the layer
 *	cannot handle a listio request, break each request into
 *	its constituent parts and handle.
 */
	started_list = (int *)calloc(nent, sizeof(int) );
	if (started_list == NULL) 
		{
		_SETERROR(stat, FDC_ERR_NOMEM, 0);
		if (narg < 4) errno = dumstat.sw_error;
		return(ERR);
		}
	done = 0;
	for (curent = 0; curent < nent; curent++)
	   {
	   if (!started_list[curent])
		{
		int copyflag;
		fio = GETIOB(list[curent].li_fildes);
		
		if (fio->can_listio) 
			{
			/*
			 * Look for all of the entries whose fio 
			 * matches this one. If all of the ones that do
			 * are contiguous, use the structure passed in to make
			 * the XRCALL. Otherwise, copy the ones that match to 
			 * a local structure and issue the XRCALL using that. 
			 * The array started_list[] keeps track of entries 
			 * that have been issued. 
			 */
			i = curent+1;
			copyflag = 0;
			numstarted = 1;
			lastmatch = nent-1;	/* in case they all match */
			while (i < nent) 
			   {
			   nextfio = GETIOB(list[i].li_fildes);
			   if (nextfio != fio)
				{
				int indx;
				for (indx = 0; indx < i-curent; indx++)
					started_list[curent+indx] = 1;
				numstarted = i-curent;
				/* lastmatch is index of last matching entry */
				lastmatch = i-1;
				/* Look for matches in the rest of the list */
				for (i = i+1; i < nent; i++)
				   {	
				   nextfio = GETIOB(list[i].li_fildes);
				   if (nextfio == fio)
					{
					if (copyflag == 0)
					   {
					   if (loclist == NULL)
					      {
					      loclist = (struct fflistreq *)malloc(sizeof(struct fflistreq)*nent);
					      if (loclist == NULL)
					         {
					         _seterr(&list[curent],
					            nent-curent, fio, FDC_ERR_NOMEM);
					         for (k = curent; k < nent; k++)
					            {
					            if (!started_list[curent])
					               _SETERROR(list[k].li_status,
					                  FDC_ERR_NOMEM,0);
					            }
					         goto alldone;
					         }
					      }
						/* copy from curent */
					   memcpy(loclist,list+curent,
				              sizeof(struct fflistreq)*numstarted);
				           copyflag = 1;
					   }
					/* mark it as started */
					started_list[i] = 1;
					/* copy it */
					memcpy(loclist+numstarted,
					   list+i,sizeof(struct fflistreq));
					/* keep track of number started */
					numstarted++;
					}
				   }
				}
			   i++;
			   }	
			if (copyflag)
				{
				/* issue xrcall for numstarted entries*/
				ret = XRCALL(fio, listiortn)  cmd, loclist, numstarted, stat);
				}
			else
				{
				/*
				 * A contiguous group of entries, beginning at
				 * curent, are for the same fio 
				 */
				ret = XRCALL(fio, listiortn)  cmd,
				   list + curent, lastmatch +1 - curent, stat);
				}
			if (ret > 0)
				numdone +=ret;
			else 
				{
				/* An error. If we haven't been 
				 * successful at starting any requests,
				 * just return -1
				 */
				if (numdone == 0) 
					goto alldone;
				else
					_seterr(&list[curent],nent-curent,fio, stat->sw_error);
				}
			if (lastmatch == nent-1)
				break;	/* all done */
			}
		else
			{
			istat = list[curent].li_status;
			SET_BPTR(buf, CPTR2BP(list[curent].li_buf));
			nstr = list[curent].li_nstride;
			nb = list[curent].li_nbyte;
			numdone++;	/* bump it now, it's as good as 'started' */
			if (list[curent].li_signo != 0)
				{
				_SETERROR(istat, FDC_ERR_REQ, 0);
				continue;
				}
			if (cmd != LC_START && cmd != LC_WAIT)
				{
				_SETERROR(istat, FDC_ERR_REQ, 0);
				continue;
				}

/*
 *			Processing is quite different for compound requests.
 *			Force them into a separate routine.
 *			For simple requests, let them fire asynchronously.
 */
			zero = 0;	/* just in case... */
			if (nstr > 1)
				{
				ret = _ffcompound(&list[curent]);
				}
			else
				{
				if (list[curent].li_flags == LF_LSEEK)
					{
					pos = list[curent].li_offset;
					ret = XRCALL(fio, seekrtn) fio, pos, 0, &locstat);
					if (ret < 0)
						{
						*istat = locstat;
						continue;
						}
					}
				CLRFFSTAT(*istat);
				if (list[curent].li_flags != 0 &&
				list[curent].li_flags != LF_LSEEK) 
					{
					_SETERROR(istat, FDC_ERR_REQ, 0);
					continue;
					}
				if(list[curent].li_opcode == LO_READ)
					{
					ret = XRCALL(fio, readartn)
					   fio, buf, nb, istat, FULL, &zero);
					}
				else if(list[curent].li_opcode == LO_WRITE)
					{
					ret = XRCALL(fio, writeartn)
					   fio, buf, nb, istat, FULL, &zero);
					}
				else
					{
					_SETERROR(istat, FDC_ERR_REQ, 0);
					continue;
					}
				}
			if ((ret < 0) && istat->sw_error == 0) 
				{
				_SETERROR(istat, errno, 0);
				}
			}
		}	/* !started_list[curent] */
		
	   }	/* for curent */

alldone:
/*
 *	If command was LC_WAIT, we must wait for completion for all those
 *	requests that were successfully started. 
 */
	if (cmd == LC_WAIT && numdone > 0)
		{
		for (i = 0 ; i < nent ; i++)
			{
			int ct = 0;
			fio = GETIOB(list[i].li_fildes);
			istat = list[i].li_status;
			while (FFSTAT(*istat) == 0)
				{
				XRCALL(fio, fcntlrtn)
					fio, FC_RECALL, istat, &dumstat);
/*
 *				Note that _SETERROR also sets FFSTAT
 *				and breaks out of the while loop.
 */
				if (ct++ > MAXRECALL)
					_SETERROR(istat, FDC_ERR_INTERR, 0);
				}
			}
		}
	free(started_list);
	if (loclist != NULL)
		free(loclist);
	if (numdone == 0) numdone = -1;	/* make error more obvious */
	if (narg < 4) errno = dumstat.sw_error;
	return (numdone);
	}
/*
 * Set the status word for all entries in list that match fio.
 * The error field of the status word is set to ernum.
 */
static void
_seterr(struct fflistreq *list, int num, struct fdinfo *fio, int ernum)
	{
	int i;
	struct ffsw *istat;	/* stat on individual request(s) */
	struct fdinfo *newfio;
	for (i = 0; i < num; i++)
		{
		newfio = GETIOB(list[i].li_fildes);
		if (newfio == fio)
			{
			istat = list[i].li_status;
			_SETERROR(istat, ernum, 0);
			}
		}
	}
/*
 * Due to the complexities in asynchronous processing and the ambiguities of
 * the returned status in an asynchronous compound call, all compound requests
 * are forced to be synchronous.
 * Note that this routine is called by the eie (user) layer - so beware
 * of changing its interface.
 */
int
_ffcompound(struct fflistreq *lptr)
	{
	struct ffsw locstat;
	int xfer;
	int zero;
	int j;
	int ret;
	int fstride, mstride;
	struct fdinfo *fio;
	bitptr buf;
	int nstr;
	int nb;
	int pos;
	struct ffsw *istat;
	
	
	istat = lptr->li_status;
	SET_BPTR(buf, CPTR2BP(lptr->li_buf));
	nstr = lptr->li_nstride;
	nb = lptr->li_nbyte;
	*(long *) &locstat = 0;	/* fast zero */
	FFSTAT(locstat) = 0;
	fio = GETIOB(lptr->li_fildes);
	if (lptr->li_flags == LF_LSEEK)
		{
		pos = XRCALL(fio, seekrtn) fio, lptr->li_offset, 0, &locstat);
		}
	else if (lptr->li_flags == 0)
		{
		/* Find out where we are, so that we can stride correctly */
		pos = XRCALL(fio, seekrtn) fio, 0, 1, &locstat);
		}
	else
		{
		_SETERROR(&locstat, FDC_ERR_REQ, 0);
		goto oops;
		}
	if (pos < 0)
		{
		goto oops;
		}

	fstride = lptr->li_filstride;
	mstride = lptr->li_memstride;
	if (mstride == 0)
		mstride = nb;
	if (fstride == 0)
		fstride = nb;
	xfer = 0;

	for (j = 0 ; j < nstr ; j++)
		{
		zero = 0;	/* just in case... */
		if (j != 0)
			{
			pos = XRCALL(fio, seekrtn) fio,
				pos + fstride, 0, &locstat);
			if (pos < 0) goto oops;
			SET_BPTR(buf, INC_BPTR(buf, mstride << 3));
			}
		if(lptr->li_opcode == LO_READ)
			{
			ret = XRCALL(fio, readrtn)
					fio,
					buf,
					nb,
					&locstat,
					PARTIAL,
					&zero
					);
			
			}
		else if(lptr->li_opcode == LO_WRITE)
			{
			ret = XRCALL(fio, writertn)
					fio,
					buf,
					nb,
					&locstat,
					PARTIAL,
					&zero
					);
			}
		else
			{
			_SETERROR(&locstat, FDC_ERR_REQ, 0);
			goto oops;
			}
		if (ret < 0) goto oops;

		xfer += locstat.sw_count;
		}
	istat->sw_count = xfer;
	istat->sw_flag = 1;
	istat->sw_error = 0;
	FFSTAT(*istat) = FFSTAT(locstat);
	return(0);

oops:
	*istat = locstat;
	return(-1);
	}
