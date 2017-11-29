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


#pragma ident "@(#) libu/ffio/tmfread.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"
#include "sysio.h"
#include <unistd.h>

/*
 * Read a tape file.
 * With this format, one tape block == one record.
 * Parameters:
 *	fio	Pointer to fdinfo block
 *	bufptr	bit pointer to where data is to go.
 *	nbytes	Number of bytes to be read
 *	stat	pointer to status return word
 *	fulp	full or partial read mode flag
 *	ubc	pointer to unused bit count (not used this class of file)
 *
 */
ssize_t
_tmf_read(
	struct fdinfo	*fio,
	bitptr		bufptr,
	size_t		nbytes,
	struct ffsw	*stat,
	int		fulp,
	int		*ubc)
{
	register int	errn;	/* Error code */
	size_t		bytes;	/* Number of bytes actually read */
	ssize_t		ret;	/* Return status */
	struct tmfio	*xf_info;

	xf_info	= (struct tmfio *)fio->lyr_info;

	if (*ubc != 0) {
		errn	= FDC_ERR_UBC;
		goto eret;
	}

	if ((BPBITOFF(bufptr) & 07) != 0) {
		errn	= FDC_ERR_REQ;
		goto eret;
	}
	if (xf_info->rwflag == WRITIN && !xf_info->tmf_speov) {
		/* Read after write is OK in special processing */
		/* otherwise, it is an error. */
		errn	= FDC_ERR_RAWR;
		goto eret;
	}
	if (xf_info->tmf_tpos) {
		if (_tmf_tpwait(xf_info) < 0) {
			errn	= errno;
			goto eret;
		}
	}

	xf_info->rwflag	= READIN;

	if (xf_info->tmf_speov) {
		/* If we ever do async i/o, then we want to read */
		/* synchronously while in special processing. */
		if (xf_info->spblocks == 0) {
			/* We're not reading from the tape. */
			/* We're reading from buffer memory */
			goto spprocread;
		}
	}
/*
 *	If the number of bytes to read is zero and reading in full
 *	record mode, skip to the end of record. If in partial
 *	record mode, the position remains as is.
 *	Are we at the beginning of the record?
 */
	if (xf_info->tmf_cnt == 0) {
/*
 *		We are at the beginning of the record. Must read
 *		from tape. Even if the user has requested a FULL
 *		record, we need to read it in to the buffer, unless
 *		the request is for bufsiz bytes.
 *		That is because, if the user requests less than
 *		is actually in the tape block, the read would
 *		give an error.
 */
		if (nbytes == xf_info->tmf_bufsiz) {
			LOOP_SYSCALL(ret,read(xf_info->tmf_fd, BPTR2CP(bufptr),
			xf_info->tmf_bufsiz));
			bytes	= ret;
		}
		else {
			LOOP_SYSCALL(ret,read(xf_info->tmf_fd, 
				xf_info->tmf_bufptr,
				xf_info->tmf_bufsiz));
			if (ret > 0) {
				bytes	= MIN(ret, nbytes);
				(void) memcpy(BPTR2CP(bufptr),
					xf_info->tmf_bufptr, bytes);
			}
		}

		if (ret > 0) {
			if (ret == nbytes) {
				SETSTAT(stat, FFEOR, bytes);
				if (xf_info->spblocks > 0)
					xf_info->spblocks--;
				return(bytes);
			}
			else if (fulp == FULL) {
				SETSTAT(stat, FFCNT, bytes);
				if (xf_info->spblocks > 0)
					xf_info->spblocks--;
				return(bytes);
			}
			else {
				SETSTAT(stat, FFCNT, bytes);
				xf_info->tmf_bufptr    += bytes;
				xf_info->tmf_cnt	= ret - bytes;
			}
		}
		else if (ret == 0) {
			if (ioctl(xf_info->tmf_fd,TMFC_EOD,0)) {
				SETSTAT(stat, FFEOF, 0);
				return(0);
			}
			SETSTAT(stat, FFEOD, 0);
			return(0);
		}
		else {
			/* Could be EOV, or an error */
			if (errno == ENOSPC) {

				/* If we hit physical eov, */
				/* return an error */
				/* and set eovhit so that */
				/* checktp will tell us we */
				/* hit eov */
/* JAS - should document that ffread will return ENOSPC at eov */
/* also Fortran read will return an error */
				if (xf_info->tmf_eovon)
					xf_info->tmf_eovhit	= 1;
				ERETURN(stat, errno, 0);
			}
			else {
				ERETURN(stat, errno, 0);
			}
		}
	}	
	else {
		/* 
		 * We are in the middle of a record. The entire record,
		 * and nothing more, is already in our buffer. 
 		 */
		bytes	= MIN(nbytes, xf_info->tmf_cnt);
		(void) memcpy(BPTR2CP(bufptr), xf_info->tmf_bufptr, bytes);
		xf_info->tmf_cnt    -= bytes;
		xf_info->tmf_bufptr += bytes;

		if (xf_info->tmf_cnt == 0) {
			xf_info->tmf_bufptr = xf_info->tmf_base;
			if (xf_info->spblocks > 0)
				xf_info->spblocks--;
			SETSTAT(stat, FFEOR, bytes);
		}
		else {
			SETSTAT(stat, FFCNT, bytes);
			if (fulp == FULL) {
				if (xf_info->spblocks > 0)
					xf_info->spblocks--;
				xf_info->tmf_bufptr	= xf_info->tmf_base;
				xf_info->tmf_cnt	= 0;
			}
		}
	}
	return (bytes);
eret:
	ERETURN(stat, errn, 0);

spprocread:
	/* We're reading from buffer memory. */
	/* Right now, we've got only 1 block in buffer memory. */
	if (xf_info->tmf_tpmk) {
		xf_info->tmf_tpmk	= 0;
		SETSTAT(stat, FFEOF, 0);
		return(0);
	}
	else {
		if (xf_info->eovbytes == 0) {
			SETSTAT(stat, FFEOD, 0);
			return(0);
		}
		bytes	= MIN(xf_info->eovbytes, nbytes);
		(void) memcpy(BPTR2CP(bufptr), xf_info->eovbuf, bytes);
		if (fulp == FULL || (nbytes >= xf_info->eovbytes)) {
			xf_info->eovbytes	= 0;
			SETSTAT(stat, FFEOR, bytes);
		}
		else {
			xf_info->eovbytes -= nbytes;
			xf_info->eovbuf += nbytes;
			SETSTAT(stat, FFCNT, bytes);
		}

		return(bytes);
	}
}
