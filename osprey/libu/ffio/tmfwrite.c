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


#pragma ident "@(#) libu/ffio/tmfwrite.c	92.1	06/29/99 13:16:47"

#include "tmfio.h"
#include "sysio.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mtio.h>

static int _tmf_wrard( struct fdinfo *fio, struct ffsw *stat);
static ssize_t _tmf_eovseen(struct tmfio *xfinfo, size_t nbytes, char *dataptr,
	ssize_t ret, struct ffsw *stat);
static int _tmf_ateov(struct fdinfo *fio, struct tmfio *xfinfo,
	size_t nbytes, char *dataptr, ssize_t ret, struct ffsw *stat,
	int *err);

/*
 * This is the tape layer for Irix systems.
 * When the tape is in variable block mode, each user's record 
 * corresponds to a block on tape.
 * This is accomplished by writing the record with 1 write statement.
 * The tape layer's buffer is big enough to hold 1 record.
 * If we get a full write, and nothing else is in the buffer for
 * this record, we can skip copying to the library buffer, and write it
 * directly from the user's space.
 * If we get a partial write, we need to copy to the library buffer.
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count (not used for IBM)
 */
ssize_t
_tmf_write(
	struct fdinfo	*fio,
	bitptr		bufptr,
	size_t		nbytes,
	struct ffsw	*stat,
	int		fulp,
	int		*ubc)
{
	register int	errn;
	ssize_t		ret;
	struct tmfio	*xfinfo;	

	if ((BPBITOFF(bufptr) & 07) != 0) {
		errn	= FDC_ERR_REQ;
		goto eret;
	}
	if (*ubc != 0) {
		errn	= FDC_ERR_UBC;
		goto eret;
	}
        xfinfo	= (struct tmfio *)fio->lyr_info;
/*
 *	If we've been reading, then try to switch the buffer into write mode.
 */
	if (xfinfo->rwflag == READIN) {
		/*
		 * Issue an error if we are not positioned at a record
		 * boundary.   ffweof would terminate the current record, but
 		 * _cos_write overwrites the current record.   We need to
		 * decide which is the proper approach before permitting this
		 * here.
		 */
		if (xfinfo->tmf_base != xfinfo->tmf_bufptr) {
			errn	= FDC_ERR_NOTREC;
			goto eret;
		}
		ret	= _tmf_wrard(fio, stat);
		if (ret < 0) return(ERR);
	}

	if (xfinfo->tmf_tpos) {
		if (_tmf_tpwait(xfinfo) < 0) {
			ERETURN(stat, errno, 0);
		}
	}
	xfinfo->rwflag	= WRITIN;

	if (xfinfo->tmf_speov) {
		/* We're in special processing. */
		/* Reset counter of blocks on tape */
		xfinfo->spblocks	= 0;

		/* If we've read anything from buffer memory, then mark it */
		/* all gone. - for now we don't need to worry about */
		/* this, because we can only have 1 block in buffer memory. */
		/* But if we every do async i/o, this could be a problem. */
	}
	if ((xfinfo->tmf_bufptr == xfinfo->tmf_base) && fulp == FULL) {
/*
 *		This is the entire record, so just write it out
 */
		LOOP_SYSCALL(ret, write(xfinfo->tmf_fd, BPTR2CP(bufptr), nbytes));
		if (ret != nbytes) {
			if (xfinfo->tmf_eovon && !xfinfo->tmf_speov) {
				/* The user has enabled eov processing. */
				/* Determine whether we hit EOV. */
				int err;
				if (_tmf_ateov(fio,xfinfo, nbytes,
					BPTR2CP(bufptr), ret, stat, &err)) {
					/* This is eov */
					/* We need to save away the */
					/* unwritten part of the data, */
					/* and set a flag so we can */
					/* tell the user eov was reached. */
					/* This user's write will return */
					/* a good status. */
					return(_tmf_eovseen(xfinfo, 
						nbytes, BPTR2CP(bufptr), 
						ret, stat));
				}
				if (err != 0) {
					ERETURN(stat, err, ret);
				}
				/* We were able to rewrite the block. */
				/* Carry on. */
			}
			else {
				if (ret < 0) {
					ERETURN(stat, errno, 0);
				}
				else{
					ERETURN(stat, FDC_ERR_WRTERR, ret);
				}
			}
		}
		SETSTAT(stat, FFEOR, ret);
		return(ret);
	}
/*
 *	This must not be the entire record. So, we need to copy it
 *	to our library buffer. 
 */
	if (nbytes + xfinfo->tmf_cnt > xfinfo->tmf_bufsiz) {
		ERETURN(stat, FDC_ERR_MXBLK, 0);
	}
	memcpy(xfinfo->tmf_bufptr, BPTR2CP(bufptr), nbytes);
	xfinfo->tmf_cnt += nbytes;
	xfinfo->tmf_bufptr += nbytes;
	if (fulp == FULL) {
		LOOP_SYSCALL(ret, write(xfinfo->tmf_fd, xfinfo->tmf_base, xfinfo->tmf_cnt));
		xfinfo->tmf_bufptr	= xfinfo->tmf_base;
		if (ret != xfinfo->tmf_cnt) {
			if (xfinfo->tmf_eovon && !xfinfo->tmf_speov) {
				int err;
				if (_tmf_ateov(fio,xfinfo, xfinfo->tmf_cnt,
					xfinfo->tmf_base, ret, stat, &err)) {
					/* This is eov */
					/* We need to save away the */
					/* unwritten part of the data, */
					/* and set a flag so we can */
					/* tell the user eov was reached. */
					/* This write will return OK */
					return(_tmf_eovseen(xfinfo, 
						xfinfo->tmf_cnt,
						xfinfo->tmf_base,
						ret, stat));
				}
				if (err != 0) {
					ERETURN(stat, err, ret);
				}
				/* We were able to rewrite the block. */
				/* Carry on. */
			}
			else {
				xfinfo->tmf_cnt	= 0;
				if (ret < 0) {
					ERETURN(stat, errno, 0);
				}
				else{
					ERETURN(stat, FDC_ERR_WRTERR, ret);
				}
			}
		}
		xfinfo->tmf_cnt	= 0;
		SETSTAT(stat, FFEOR, nbytes);
		return(nbytes);	
	}
	else {

		SETSTAT(stat, FFCNT, nbytes );
		return(nbytes);
	}
			
eret:
	ERETURN(stat, errn, 0);
}


/*
 * Switch from read mode into write mode.
 * We have to do a space command for this to work.
 * We try backspacing then forward spacing
 *
 * Return value:
 *	 0 if OK
 *	-1 if error.  Error code set in stat->sw_error
 */
static int
_tmf_wrard(
	struct fdinfo	*fio,
	struct ffsw	*stat)
{
	struct tmfio	*xfinfo;
	tmfpblk_t	dmnc;

	xfinfo	= (struct tmfio *)fio->lyr_info;

	if (xfinfo->rwflag != READIN)
		return(0);

/*
 *	Discard any data in the buffer. Since this layer does
 *	not do any read-ahead, data would be in the buffer only
 *	if the user had read a partial record.
 */
	xfinfo->tmf_bufptr	= xfinfo->tmf_base;
	xfinfo->tmf_cnt		= 0;
/*
 *	Since we were just reading, we can't be at BOT.
 *      We must move the tape before we can write.  We will
 *      backspace then forwardspace.  A space of 0 doesn't work
 *	with traditional Irix support--I'm assuming it won't
 *	work with TMF either.
 */
	(void) memset(&dmnc, 0, sizeof(dmnc));

	dmnc.rh.length	= sizeof(tmfpblk_t) - sizeof(tmfreqhdr_t);
	dmnc.rh.request	= TR_PBLKS;
	dmnc.count	= -1;

	if (ioctl(xfinfo->tmf_fd, TMFC_DMNREQ, &dmnc) < 0) {
		ERETURN(stat, errno, 0);
	}
	if (dmnc.rh.reply != 0) {
		if ((dmnc.rh.reply != ETFMS) || (dmnc.rh.residual != 0))
			ERETURN(stat, dmnc.rh.reply, 0);
	}

	(void) memset(&dmnc, 0, sizeof(dmnc));

	dmnc.rh.length	= sizeof(tmfpblk_t) - sizeof(tmfreqhdr_t);
	dmnc.rh.request	= TR_PBLKS;
	dmnc.count	= 1;

	if (ioctl(xfinfo->tmf_fd, TMFC_DMNREQ, &dmnc) < 0) {
		ERETURN(stat, errno, 0);
	}
	if (dmnc.rh.reply != 0) {
		if ((dmnc.rh.reply != ETFMS) || (dmnc.rh.residual != 0))
			ERETURN(stat, dmnc.rh.reply, 0);
	}

	return(0);
}

/*
 * Flush the buffer and clean up
 *	This routine should return 0, or -1 on error.
 */
ssize_t
_tmf_flush(struct fdinfo *fio, struct ffsw *stat)
{
	struct tmfio	*xfinfo;
	ssize_t		ret;
	size_t		request;

	xfinfo	= (struct tmfio *)fio->lyr_info;

        if (xfinfo->tmf_tpos) {
                if (_tmf_tpwait (xfinfo) < 0) {
                        ERETURN(stat, errno, 0);
                }
        }
/*
 *	if reading, clear out any unread data in the buffer.
 */

	if (xfinfo->rwflag == READIN) {
		xfinfo->tmf_bufptr	= xfinfo->tmf_base;
		xfinfo->tmf_cnt		= 0;
		return(0);
	}
/*
 *	In write mode.  Write out any uncompleted record.
 */
	request	= xfinfo->tmf_cnt;

	if (xfinfo->rwflag == WRITIN && request != 0) {
		LOOP_SYSCALL(ret, write(xfinfo->tmf_fd, xfinfo->tmf_base, request));
		if (ret != request) {
			if (xfinfo->tmf_eovon && !xfinfo->tmf_speov) {
				/* The user has enabled eov processing */
				/* Determine whether we hit EOV */
				int	err;
				if (_tmf_ateov(fio,xfinfo, xfinfo->tmf_cnt,
					xfinfo->tmf_base, ret, stat, &err)) {
					return(_tmf_eovseen(xfinfo, 
	    					xfinfo->tmf_cnt,
	    					xfinfo->tmf_base,
						ret, stat));
				}
				if (err != 0) {
					ERETURN(stat, err, ret);
				}
				/* We were able to rewrite the block. */
				/* Carry on. */
			}
			else {
				if (ret < 0) {
					ERETURN(stat, errno, 0);
				}
				else{
					ERETURN(stat, FDC_ERR_WRTERR, ret);
				}
			}
		}
		xfinfo->tmf_bufptr	= xfinfo->tmf_base;
		xfinfo->tmf_cnt		= 0;
		if (ret == ERR) {
			ERETURN(stat, errno, 0);
		}
		else if (ret < request) {
			ERETURN(stat, FDC_ERR_WRTERR, 0);
		}
	}
	return(0);
}

/*
 * Write an EOF.
 *	If we can write a tape mark, do so.
 *	Otherwise, return an error.
 */
int
_tmf_weof(struct fdinfo *fio, struct ffsw *stat)
{
	register int	ret;
	struct tmfio	*xfinfo;
	tmfwfm_t	ctl;

	xfinfo	 = (struct tmfio *)fio->lyr_info;

	if (xfinfo->tmf_rwtpmk == 0)
		ERETURN(stat, FDC_ERR_NWEOF, 0);

	if (xfinfo->rwflag == READIN) {
		/*
		 * Issue an error if we are not positioned at a record
		 * boundary.   ffweof would terminate the current record, but
 		 * _cos_write overwrites the current record.   We need to
		 * decide which is the proper approach before permitting this
		 * here.
		 */
		if (xfinfo->tmf_base != xfinfo->tmf_bufptr) {
			_SETERROR(stat, FDC_ERR_NOTREC, 0);
			return(ERR);
		}
		ret	= _tmf_wrard(fio, stat);
		if (ret < 0) return(ERR);
	}
	if(_tmf_flush(fio, stat) == ERR) {
		return(ERR);
	}
	xfinfo->rwflag	= WRITIN;

	(void) memset(&ctl, 0, sizeof(ctl));

	ctl.rh.request	= TR_WFM;
	ctl.rh.length	= sizeof(tmfwfm_t) - sizeof(tmfreqhdr_t);
	ctl.count	= 1;
	ret		= ioctl(xfinfo->tmf_fd, TMFC_DMNREQ, &ctl);

	if (ret < 0) {
		if (xfinfo->tmf_eovon && !xfinfo->tmf_speov) {
			/* The user has enabled eov processing */
			/* Determine whether we hit EOV */
			if (errno == ENOSPC) { 
				/* This is eov */
				/* We need to save away the */
				/* unwritten tapemark */
				/* and set a flag so we can */
				/* tell the user eov was reached. */
				/* This user's write will return a good status */
				xfinfo->tmf_eovhit	= 1;
				xfinfo->tmf_tpmk	= 1;
				xfinfo->tmf_bufptr	= xfinfo->tmf_base;
				xfinfo->tmf_cnt		= 0;
				return(0);
			}
		}
		ERETURN(stat, errno, 0);
	}
	SETSTAT(stat, FFEOF, 0);
	return(0);
}

/*
 * Write an EOD.
 *	Truncate the file.  This is the 'real' end of the data.
 *	We cannot do this if we were reading or if we just positioned.
 */
int
_tmf_weod(struct fdinfo *fio, struct ffsw *stat)
{
	register int	ret;
	struct tmfio	*xfinfo;	

        xfinfo	= (struct tmfio *)fio->lyr_info;

	if (xfinfo->rwflag != WRITIN) {
		ERETURN(stat, FENOENDF, 0);
	}

	ret	= XRCALL(fio, flushrtn) fio, stat);

	if (ret < 0) return(ret);

	SETSTAT(stat, FFEOD, 0);
	return(0);

}

static ssize_t
_tmf_eovseen(
	struct tmfio	*xfinfo, 
	size_t		nbytes, 
	char		*dataptr,
	ssize_t		ret, 
	struct ffsw	*stat)
{
	xfinfo->tmf_eovhit	= 1;

	if (xfinfo->eovbuf == NULL) {
		xfinfo->eovbuf	= malloc(xfinfo->tmf_bufsiz);
		if (xfinfo->eovbuf == NULL) {
			ERETURN(stat,FDC_ERR_NOMEM, 0);
		}
		xfinfo->eovbase	= xfinfo->eovbuf;
	}
	if (ret < 0) {
		(void) memcpy(xfinfo->eovbuf, dataptr, nbytes);
		xfinfo->eovbytes	= nbytes;
		xfinfo->tmf_partblk	= 0;
	}
	else {
		(void) memcpy(xfinfo->eovbuf, dataptr + ret, nbytes - ret);
		xfinfo->eovbytes	= nbytes - ret;
		xfinfo->tmf_partblk	= 1;
	}
	xfinfo->tmf_cnt		= 0;
	xfinfo->tmf_bufptr	= xfinfo->tmf_base;
	SETSTAT(stat, FFEOR, nbytes);
	return(nbytes);
}
/*
 * Determine whether we are at EOV.
 * Returns 0 if not at eov. err will be set to nonzero
 *		if an error occured.
 * Returns 1 if at eov
 */
static int
_tmf_ateov(
	struct fdinfo	*fio,
	struct tmfio	*xfinfo, 
	size_t		nbytes, 
	char		*dataptr,
	ssize_t		ret, 
	struct ffsw	*stat,
	int		*err
)
{
	register int	retval;
	int		usertm;
	ssize_t		nret;
	struct mtget	mtget;

#define RETURN(x) {retval = (x); goto done;}

	*err	= 0;

	if (ret == ERR) {
		if (errno == ENOSPC)
			RETURN(1)	/* At eov */
		else{
			*err	= errno;
			RETURN(0)
		}
	}
	else {
		/* Part of the block was written. */
		/* We do an ioctl to find out if this is due */
		/* to EOV */
		if (ioctl(xfinfo->tmf_fd, MTIOCGET, &mtget) < 0) {
			*err	= errno;
			RETURN(0)
		}
		if ((mtget.mt_dposn & MT_EW) && (mtget.mt_dposn & MT_EOT)) {
			/* Yes, we are at eov. */
			/* We need to do another write so that the */
			/* tape daemon will be informed that we are at */
			/* eov. We expect it to fail with ENOSPC. */
			LOOP_SYSCALL(nret, write(xfinfo->tmf_fd, 
				(dataptr + ret), (nbytes-ret)));
			if (nret == ERR && errno == ENOSPC) {
				RETURN(1)	/* At eov, as expected */
			}
			else if (nret == ERR) {
				/* Totally unexpected. */
				*err	= errno;
				RETURN(0)
			}else {
				/* Also unexpected. */
				/* Now we unfortunately have data that */
				/* we don't want on the tape. */
				/* Back up 2, and try to rewrite the */
				/* whole block. */
				if (_tmf_stpos(fio, FP_TPOS_BACK, 1, 0, 0, 0,
					&usertm, stat) < 0) {
					*err	= stat->sw_error;
					RETURN(0)
				}
				LOOP_SYSCALL(nret, write(xfinfo->tmf_fd,
					dataptr, nbytes));
				if (nret == ERR)
					*err	= errno;
				else if (nret != nbytes)
					*err	= FDC_ERR_WRTERR;
				RETURN(0)
			}
		}
	}
	retval	= 0;

done:
	xfinfo->tmf_cnt		= 0;
	xfinfo->tmf_bufptr	= xfinfo->tmf_base;
	return(retval);
}
