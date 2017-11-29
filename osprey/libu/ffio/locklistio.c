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


#pragma ident "@(#) libu/ffio/locklistio.c	92.1	06/29/99 13:16:47"


#include <ffio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <sys/listio.h>
#include "locklyr.h"
#define  MAXRECALL 100000

/*
 * lock listio requests
 * The lock layer will be called only if the lower layer can handle listio 
 * requests.
 */
_lock_listio(
int			cmd,		/* LC_WAIT or LC_START */
struct fflistreq	*list,		/* list of requests (see fflistio) */
int			nreq,		/* number of requests */
struct ffsw		*stat)		/* status structure */
{

	int	i;
	struct fdinfo	*llfio;
	struct fdinfo	*first_fio;
	struct fdinfo	*fio;
	struct fflistreq *copy;		/* copy of the list of requests */
	int	numdone;
	int	curent;
	int	pos;
	bitptr	buf;
	int	nb;
	int	zero;
	int	ret;
	int	nstr;
	struct ffsw	locstat;
	struct ffsw	 *istat;

	if (nreq == 0)
		return(0);
	first_fio = GETIOB(list[0].li_fildes);
	for (i = 0; i < nreq; i++) {
		fio = GETIOB(list[i].li_fildes);
		if (fio != first_fio) {
			_SETERROR(stat, FDC_ERR_LSTIO, 0);
			return (-1);
		}
	}

	llfio = first_fio->fioptr;
	if (llfio->can_listio) {
		copy = malloc(nreq * sizeof(*list));
		if (copy == NULL)
			ERETURN(stat, FDC_ERR_NOMEM, 0);

		for (i = 0; i < nreq; i++) {
			copy[i] = list[i];		/* copy the entry */
			copy[i].li_fildes = (int)llfio;	
		}
		LYR_LOCK(fio);
		numdone = XRCALL(llfio, listiortn) cmd, copy, nreq, stat);
		LYR_UNLOCK(fio);
		free(copy);
		return(numdone);
	}
	else {
		LYR_LOCK(fio);
		numdone = 0;
		for (curent = 0; curent < nreq; curent++) {
			istat = list[curent].li_status;
			SET_BPTR(buf, CPTR2BP(list[curent].li_buf));
			nstr = list[curent].li_nstride;
			nb = list[curent].li_nbyte;
			numdone++;	/* bump it now, it's as good as 'started' */
			if (cmd != LC_WAIT && cmd != LC_START) {
				_SETERROR(istat, FDC_ERR_REQ, 0);
				continue;
			}

/*
 *			Processing is quite different for compound requests.
 *			Force them into a separate routine.
 *			For simple requests, let them fire asynchronously.
 */
			zero = 0;	/* just in case... */
			if (nstr > 1) {
				ret = _ffcompound(&list[curent]);
			}
			else {
				if (list[curent].li_flags == LF_LSEEK) {
					pos = list[curent].li_offset;
					ret = XRCALL(llfio, seekrtn) llfio, pos, 0, &locstat);
					if (ret < 0) {
						*istat = locstat;
						continue;
					}
				}
				else if (list[curent].li_flags != 0) {
					_SETERROR(istat, FDC_ERR_REQ, 0);
					continue;
				}
				CLRFFSTAT(*istat);
				if(list[curent].li_opcode == LO_READ) {
					ret = XRCALL(llfio, readartn)
					   llfio, buf, nb, istat, FULL, &zero);
				}
				else if(list[curent].li_opcode == LO_WRITE) {
					ret = XRCALL(llfio, writeartn)
					   llfio, buf, nb, istat, FULL, &zero);
				}
				else {
					_SETERROR(istat, FDC_ERR_REQ, 0);
					continue;
				}
			}
			if ((ret < 0) && istat->sw_error == 0) {
				_SETERROR(istat, errno, 0);
			}
		
		   }	/* for curent */

/*
 *		If command was LC_WAIT, we must wait for completion for all 
 *		those requests that were successfully started. 
 */
		if (cmd == LC_WAIT && numdone > 0) {
			for (i = 0 ; i < nreq ; i++) {
				int ct = 0;
				istat = list[i].li_status;
				while (FFSTAT(*istat) == 0) {
					XRCALL(llfio, fcntlrtn) llfio,
						 FC_RECALL, istat, &locstat);
/*
 *					Note that _SETERROR also sets FFSTAT
 *					and breaks out of the while loop.
 */
					if (ct++ > MAXRECALL)
						_SETERROR(istat, FDC_ERR_INTERR, 0);
				}
			}
		}
		if (numdone == 0) numdone = -1;	/* make error more obvious */
		LYR_UNLOCK(fio);
		return (numdone);
	}
}

