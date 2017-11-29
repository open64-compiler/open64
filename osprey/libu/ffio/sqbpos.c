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


#pragma ident "@(#) libu/ffio/sqbpos.c	92.2	10/14/99 15:22:06"


#include  <ffio.h>
#include  <fcntl.h>
#include  <errno.h>
#include  "sqbio.h"

/*
 * Positioning requests for the buffering layer
 */

int
_sqb_pos(struct fdinfo *fio, int cmd, long *arg, int len, struct ffsw *stat)
{
int ret = 0;
struct sqb_f *sqb_info;
struct sqbio *sqbptr;
struct sqbio *sqborig;
struct sqbio *s;
struct fdinfo *llfio;
int found = 0;
int nbits;
int sync = -1;

	llfio = fio->fioptr;
	sqb_info = (struct sqb_f *)fio->lyr_info;

	if (fio->rwflag == WRITIN) {
		/* flush buffers and wait for outstanding I/O to finish. */
		if (_sqb_flush(fio, stat) < 0) {
			return(ERR);
		}
	}

	switch(cmd) {
/* For now, this is not supported on SGI systems. */
/* We need to work out what "arg" should be. */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		case FP_RSEEK:
			if ((fio->rwflag == READIN) || 
			   (fio->rwflag == POSITIN)) {
			   if (*arg < 0) {
				/* Seeking backwards */
				/* Are we seeking within the current */
				/* buffer? */
				sqbptr = sqb_info->sqbio_cur;
				if (sqbptr->status == IOACTIVE) {
				   while (sqbptr->iostat.sw_flag == 0 ||
					sqbptr->iostat.sw_stat == 0) {
					ret = XRCALL(llfio,fcntlrtn) llfio,
					FC_RECALL, &(sqbptr->iostat), stat);
					if (ret < 0) {
						return(ERR);
					}
				   }
				   if (FFSTAT(sqbptr->iostat) == FFERR) {
					ERETURN(stat, sqbptr->iostat.sw_error,0);
				   }
				   sqbptr->_cnt = sqbptr->iostat.sw_count<<3;
				   sqbptr->status = IODATA;
				}
				if (sqbptr->status == IODATA) {
					nbits = -(*arg); /* convert to positive */
					nbits = nbits<<3;
					if (nbits <=
					 SUBT_BPTR(sqb_info->_ptr,sqbptr->_base)){
						SET_BPTR(sqb_info->_ptr,
						 INC_BPTR(sqb_info->_ptr,-nbits));
						sqbptr->_cnt += nbits;
						break;
					  }
				}
			   }
			   else {
				/* seeking forward */
				/* Any chance that the position would be in */
				/* our buffers? */
				nbits = *arg << 3;
				if (nbits > sqb_info->nbuf * sqb_info->bufsiz){
					/* won't be in any of the buffers */
					goto a1;
				}
				sqbptr = sqb_info->sqbio_cur;
				sqborig = sqbptr;
				do {
				   if (sqbptr->status == IOACTIVE) {
				      while (sqbptr->iostat.sw_flag == 0 ||
					sqbptr->iostat.sw_stat == 0) {
				   	   ret = XRCALL(llfio,fcntlrtn) llfio,
					   FC_RECALL, &(sqbptr->iostat), stat);
					   if (ret < 0) {
						return(ERR);
					   }
				      }
				      if (FFSTAT(sqbptr->iostat) == FFERR) {
					ERETURN(stat, sqbptr->iostat.sw_error,0);
				      }
				      sqbptr->_cnt = sqbptr->iostat.sw_count<<3;
				      sqbptr->status = IODATA;
				   }
				   if (sqbptr->status == IODATA) {
					if (nbits <= sqbptr->_cnt) {
						/* Desired position is in this buffer */
						sqbptr->_cnt -= nbits;
						/* Clear out buffers that preceeded this */
						s = sqborig;
						for (; s != sqbptr; s= s->nxt) {
							s->status = EMPTY;
							CLRFFSTAT(s->iostat);
						}
						sqb_info->sqbio_cur = sqbptr;
						if (sqbptr != sqborig)
							sqb_info->_ptr = sqbptr->_base;
						SET_BPTR(sqb_info->_ptr,
						 INC_BPTR(sqb_info->_ptr,nbits));
						found = 1;
						break;
					}
					else {
						nbits -= sqbptr->_cnt;
					}
				   }
				   else
					goto a1;	/* all out of data */
				   sqbptr = sqbptr->nxt;
				} while (sqbptr != sqborig);
			   }
			   }
		if (found == 1)
			break;
a1:
			/* fall through */
		case FP_BSEEK:
		case FP_GETPOS:
		case FP_SETPOS:
		case FP_SETTP:
		case FP_SKIPF:
			sync++;	/* Want to sync */
		case FP_SABS:
			sync++;	/* Do not sync for FP_SABS, do sync for */
				/* all others */
			if (fio->rwflag == READIN || fio->rwflag == POSITIN) {
				/* Mark all buffers as empty. Optionally */
				/* synchronize physical position with */
				/* logical position */
				if (_sqb_sync(fio, stat, sync) < 0) {
					return(ERR);
				}
			}
                        ret = XRCALL(llfio,posrtn)
                                        llfio, cmd, arg, len, stat);
			break;
		case FP_GABS:
		/* FP_GABS does not synchronize physical position */
		/* with logical position */
                        ret = XRCALL(llfio,posrtn)
                                        llfio, cmd, arg, len, stat);
			return(ret);	/* No need to reset fio->rwflag */
#endif
		default:
			ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	fio->rwflag = POSITIN;
	return(ret);
#endif
}
