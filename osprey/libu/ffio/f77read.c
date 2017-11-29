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


#pragma ident "@(#) libu/ffio/f77read.c	92.4	11/15/99 17:18:18"


#include <ffio.h>
#include <string.h>
#include "f77io.h"

static int get_segment(struct fdinfo *fio, struct f77_xf *xf_info, struct ffsw *stat);
static int seteod(struct fdinfo *fio, struct ffsw *stat);

/*
 * Read a f77 file.
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 *	ubc	- pointer to unused bit count (not used this class of file)
 *
 */
ssize_t
_f77_xread(
struct fdinfo	*fio,
bitptr		bufptr,
size_t		nbytes,
struct ffsw	*stat,
int		fulp,
int		*ubc)
{
	size_t bytes, moved, bytomove;
	int ret, eorstat;
	struct f77_xf *xf_info;
	char *cbufptr;
	int ernum;

	xf_info = (struct f77_xf *)fio->lyr_info;
	xf_info->flag = xf_info->flag & (~ATEOR);
	if (*ubc != 0){
		ernum = FDC_ERR_UBC;
		goto eret;
	}

	cbufptr = BPTR2CP(bufptr);
	if ((BPBITOFF(bufptr) & 07) != 0) {
		ernum = FDC_ERR_REQ;
		goto eret;
	}
	if (fio->rwflag == WRITIN) {
		/* read after write error */
		ernum = FDC_ERR_RAWR;
		goto eret;
	}

	fio->rwflag = READIN;

/*
 *	If we are at the beginning of a new record, read the
 *	control word.
 */
	if (xf_info->rembytes == 0) {
	       if ( xf_info->_cnt >= RDWLEN ) {
/*
 *		If the entire control word is in the buffer, get it.
 *		Otherwise, we will drop through and get_segment will
 *		handle it.
 */
			long ii = 0;
			char *cb;
			int ijk;
			cb = (char *)&ii;
			for (ijk = 0; ijk < RDWLEN; ijk++) {
				*cb++ = *xf_info->_ptr++;
			}
			xf_info->_cnt -= RDWLEN;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
			ii = (unsigned long) ii >> ((sizeof(ii) - RDWLEN) * 8);
#endif
			if (fio->rtype == TR_UX_MIPS) {SWAPB(ii);}

			xf_info->rembytes = (size_t)ii ;
			xf_info->last_lrdwaddr = xf_info->lrdwaddr;
			xf_info->lrdwaddr = xf_info->lrdwaddr +
				((xf_info->rembytes + RDWLEN*2) );
		}
		else {
			ret = get_segment(fio, xf_info, stat);
			if (ret <= 0){
				return(ret);	/* probable EOF, EOD, orERR */
			}
		}
	}
/*
 *	loop getting and moving data
 */
	bytomove = (nbytes < xf_info->rembytes) ? nbytes: xf_info->rembytes;
	moved = 0;
	eorstat = FFCNT;
	while (bytomove > 0) {

/*
 *		If the buffer is empty, get a new one
 */
		if (xf_info->_cnt == 0){
			ssize_t rret;
			int zero = 0;
                	xf_info->_ptr = xf_info->_base;
                	rret = XRCALL(fio->fioptr, readrtn)fio->fioptr, CPTR2BP(xf_info->_ptr),
                       	 xf_info->_ffbufsiz, stat, PARTIAL, &zero);
	                if (rret < 0) return(ERR);
			if (zero != 0) {
                        	_SETERROR(stat,FDC_ERR_FMT, 0);
	                        return(ERR);
			}
			xf_info->_cnt = rret;
			if (xf_info->_cnt == 0) {
				return(seteod(fio,stat));
			}

		}

		bytes = (bytomove < xf_info->_cnt) ? bytomove: xf_info->_cnt;

/*
 *		Get the data from the buffer
 */
		if (bytes > 0) {
			char *ratptr = (char *)xf_info->_ptr;
			memcpy(cbufptr, xf_info->_ptr, bytes);
			cbufptr += bytes;
			xf_info->_cnt -= bytes;
			xf_info->_ptr += bytes;
			xf_info->rembytes -= bytes;
			bytomove -= bytes;
			moved += bytes;
		}
	} /* end while */
/*
 *	Check for zero length record at beginning of buffer
 */
	if (xf_info->rembytes == 0 ) {
		eorstat = FFEOR;
	}

	if ( eorstat == FFEOR || fulp) {
		/* Move past the ending control word */
		if ( (xf_info->rembytes == 0) &&
			(xf_info->_cnt >= RDWLEN)) {
			xf_info->_cnt -= RDWLEN;
			xf_info->_ptr += RDWLEN;
			xf_info->flag |= ATEOR;
		} else {
			ret = _f77skip2eor(fio, stat);
			if (ret <= 0){
				return(ret);	/* EOF/EOD or error */
			}
		}
	}
			
	SETSTAT(stat, eorstat, moved);
	return (moved);
eret:
	ERETURN(stat, ernum, 0);
}

/*
 * Fetch a segment from the data stream.  
 *	returns:
 *		>0 got it!
 *		=0 EOF or EOD
 *		<0 some error.
 */
static int
get_segment(struct fdinfo *fio, struct f77_xf *xf_info, struct ffsw *stat)
{
	int zero;
	long ii = 0;
	ssize_t rret;
	int ijk;
	int cwremain = 0;
	int newrecord = 0;
	char *cb;


	if (fio->ateod != 0){
		return(seteod(fio,stat));
	}

	zero = 0;
/*
 *	Are we starting a new record?
 */
	if (xf_info->rembytes == 0 ) {
		/* Ready to read a new record. */
		/* First get the record control word. */
		newrecord = 1;
		cb = (char *)&ii;
		if (xf_info->_cnt >= RDWLEN) {
			for (ijk = 0; ijk < RDWLEN; ijk++) {
				*cb++ = *xf_info->_ptr++;
			}
			xf_info->_cnt -= RDWLEN;
		}
		else {
			for (ijk = 0; ijk < xf_info->_cnt; ijk++) {
				*cb++ = *xf_info->_ptr++;
			}
			cwremain = RDWLEN - xf_info->_cnt;
			xf_info->_cnt = 0;
		}
	}
	if (xf_info->_cnt == 0) {
		/* time to read a new buffer */
		xf_info->_ptr = xf_info->_base;
		xf_info->_cnt = 0;
		rret = XRCALL(fio->fioptr, readrtn)fio->fioptr, CPTR2BP(xf_info->_ptr),
			xf_info->_ffbufsiz, stat, PARTIAL, &zero);
		if (rret < 0) return(ERR);
		if (zero != 0) {
			_SETERROR(stat,FDC_ERR_FMT, 0);
			return(ERR);
		}
		xf_info->_cnt = rret;
		if (xf_info->_cnt == 0) {
			return(seteod(fio,stat));
		}
		/* Do we need to get more of the control word? */
		if (cwremain) {
			for (ijk = 0; ijk < cwremain; ijk++) {
				*cb++ = *xf_info->_ptr++;
			}
			xf_info->_cnt -= cwremain;
		}
	}
	if (newrecord) {
		/*
		 * We already read the control word at the start 
		 * of our new record. Decipher it and remember how
		 * many bytes are in this record.
		 */
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
		ii = (unsigned long) ii >> ((sizeof(ii) - RDWLEN) * 8);
#endif
		if (fio->rtype == TR_UX_MIPS) {SWAPB(ii);}

		xf_info->rembytes = (size_t)ii ;
		xf_info->last_lrdwaddr = xf_info->lrdwaddr;
		xf_info->lrdwaddr = xf_info->lrdwaddr +
			((xf_info->rembytes + RDWLEN*2) );
	}
	return(1);
}

/*
 *	Common code to handle EOD return
 */
static
seteod(struct fdinfo *fio, struct ffsw *stat)
{
	struct f77_xf *xf_info;

	xf_info = (struct f77_xf *)fio->lyr_info;
	xf_info->rembytes = 0;
	xf_info->recbytes = 0;
	xf_info->_cnt = 0;
	fio->ateof = 0;
	fio->ateod = 1;
	SETSTAT(stat, FFEOD, 0)
	return(0);
}

/*
 * Skip2eor skips ahead to the end of the record, reading up blocks and
 * records as needed until we hit EOR EOF or EOD
 * We will be positioned right after the trailing control word of this
 * record.
 *	Returns:
 *		<0 - error
 *		=0 - EOF/EOD
 *		>0 - EOR
 */
int
_f77skip2eor(struct fdinfo *fio, struct ffsw *stat)
{
	int ret;
	struct f77_xf *xf_info;
	int cws;
	ssize_t rret;
	int zero = 0;

	xf_info = (struct f77_xf *)fio->lyr_info;
	while (xf_info->rembytes > 0) {
		if (xf_info->rembytes <= xf_info->_cnt) {
			/* All of the remaining data for this record */
			/* is in the buffer. */
			xf_info->_ptr += xf_info->rembytes;
			xf_info->_cnt -= xf_info->rembytes;
			xf_info->rembytes = 0;
		}
		else {
			xf_info->rembytes -= xf_info->_cnt;
			xf_info->_cnt = 0;
			xf_info->_ptr = xf_info->_base;
			ret = get_segment(fio, xf_info, stat);
			if (ret <= 0)
				return(ret);	/* probable EOF/EOD */
		}
	}
	/* Now get past the trailing control word. */
	if (xf_info->_cnt >= RDWLEN) {
		xf_info->_cnt -= RDWLEN;
		xf_info->_ptr += RDWLEN;
	} else {

		cws = RDWLEN - xf_info->_cnt;	
		/* We need to read a new block */
		xf_info->_ptr = xf_info->_base;
		xf_info->_cnt = 0;
		rret = XRCALL(fio->fioptr, readrtn)fio->fioptr, CPTR2BP(xf_info->_ptr),
			xf_info->_ffbufsiz, stat, PARTIAL, &zero);
		if (rret < 0) return(ERR);
		if (zero != 0) {
			_SETERROR(stat,FDC_ERR_FMT, 0);
			return(ERR);
		}
		xf_info->_ptr = xf_info->_ptr + cws;
		xf_info->_cnt = rret;
		if (xf_info->_cnt <= 0) {
			return(seteod(fio,stat));
		}
		xf_info->_cnt -= cws;
	}
	xf_info->flag |= ATEOR;
	return(1);
}


#ifdef __mips
static
off64_t
__f77_xseek(
struct fdinfo	*fio,
off64_t		pos, 
int		whence,
struct ffsw	*stat)
#else
static
_ffseek_t
__f77_xseek(
struct fdinfo	*fio,
off_t		pos, 
int		whence,
struct ffsw	*stat)
#endif
{
#ifdef __mips
	off64_t	  ret, bytesize;
#else
	_ffseek_t ret, bytesize;
#endif
	struct fdinfo *llfio;
	struct f77_xf *xf_info;
	char *cb;
	int zero = 0;
	int ijk;
	long ii;
	ssize_t rret;
/*
 *	We support:
 *	whence = 2; 	seek to end
 *	whence = 0; 	seek to an absolute position.
 *			seek to 0 is equivalent to rewind
 *			otherwise, the position better have been
 *			obtained by a call to this routine, so that
 *			we guarantee it is at a record bdry
 *	whence = 1 and pos = 0;	get current position. Must be at
 *			a record bdry
 */

	xf_info = (struct f77_xf *)fio->lyr_info;

/*
 *	If the file's been writing, complete any partial records, truncate
 *	the file, and flush the buffer.
 *
 */
	if (fio->rwflag == WRITIN) {
		/* If we are not at a record boundary, we cannot */
		/* get our current position. */
		if ((xf_info->recbytes != 0) && (whence == 1) && (pos == 0)) {
			ERETURN(stat,FDC_ERR_NOTREC, 0);
		}
		ret = XRCALL(fio, flushrtn) fio, stat);
		if (ret < 0) return(ERR);

		ret = XRCALL(fio, weodrtn) fio, stat);
		if (ret < 0) return(ERR);
	}

	
	llfio = fio->fioptr;

	switch (whence) {
	case 0:
		/* seek to an absolute position */
		if (pos == 0) { /* Rewind */
			if (fio->rwflag == READIN) {
				/* If the desired position is in the buffer */
				/* avoid the seek system call */
				if ((xf_info->_ptr - xf_info->_base)  >=
					(xf_info->lrdwaddr 
					 - xf_info->rembytes)) {
					xf_info->_cnt += xf_info->_ptr - xf_info->_base;
					xf_info->_ptr = xf_info->_base;
					xf_info->lrdwaddr = 0;
					xf_info->last_lrdwaddr = 0;
					xf_info->rembytes = 0;
					fio->ateof = 0;
					fio->ateod = 0;
					xf_info->flag |= ATEOR;
					return(0);
				}
				
			}
		}
		if (fio->rwflag == READIN) {
			/* flush to shift into neutral */
			ret = XRCALL(fio, flushrtn) fio, stat);
			if (ret < 0) return(ret);
			xf_info->flag |= ATEOR;	/* Because we will be at eor */
		}
		xf_info->rembytes = 0;
		if (pos == 0) {
			/* rewind */
			xf_info->lrdwaddr = 0;
			xf_info->last_lrdwaddr = 0;
			ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
		}
		else {
			/* seek to an absolute position. */
			/* Things won't work right unless the new position is at a record bdry */
			off_t cwpos;
			cwpos = pos -RDWLEN;
			if (cwpos < 0) cwpos = 0;
			ret = XRCALL(llfio, seekrtn) llfio, cwpos, 0, stat);
			if (ret < 0) return ERR;
			xf_info->lrdwaddr = pos;
			/* Read the trailing control word from the previous record. */
			xf_info->_ptr = xf_info->_base;
			rret = XRCALL(fio->fioptr, readrtn)fio->fioptr, 
				CPTR2BP(xf_info->_ptr),RDWLEN, stat, PARTIAL , &zero);
			xf_info->_cnt = 0;
			if (rret < 0)
				ERETURN(stat, FDC_ERR_UXEND, 0);
			if (zero != 0)
				ERETURN(stat, FDC_ERR_FMT, 0);
			ret += RDWLEN;
			xf_info->_cnt = 0;
			if (pos == 0) {
				xf_info->last_lrdwaddr = 0;
				xf_info->lrdwaddr = 0;
			}
			else {
				ii = 0;
				cb = (char *)&ii;
				for (ijk = 0; ijk < RDWLEN; ijk++) {
					*cb++ = *xf_info->_ptr++;
				}
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
				ii = (unsigned long)ii >> ((sizeof(ii) - RDWLEN) *8);
#endif
				if (fio->rtype == TR_UX_MIPS) {SWAPB(ii);}
				xf_info->last_lrdwaddr = pos-ii-(2*RDWLEN);
				xf_info->_ptr = xf_info->_base;
			}
		}
		break;

	case 2:
		/* Seek to end */
		if (pos != 0)
			ERETURN(stat, FDC_ERR_NOSUP, 0);
		/*
		 *	If we've been writing, we are at the end because of
		 *	FFC_WRTRUNC.
		 */
		if (fio->rwflag == WRITIN)
			break;
		/*
		 * 	If we've been reading, reset everything
		 */
		if (fio->rwflag == READIN) {
			ret = XRCALL(fio, flushrtn) fio, stat);
			if (ret < 0) return(ret);
			xf_info->flag |= ATEOR;	/* Because we will be at eor */
		}
		xf_info->rembytes = 0;


		ret = XRCALL(llfio, seekrtn) llfio, 0, whence, stat);
		if (ret < 0) return(ERR);

		bytesize = ret;

		if (bytesize == 0) {	/* if file is empty */
			xf_info->lrdwaddr = 0;
			xf_info->last_lrdwaddr = 0;
		}
		else {		/* if file is not empty */
			long ii = 0;
			int zero = 0;

			if (bytesize < 2*(RDWLEN))
				ERETURN(stat, FDC_ERR_FMT, 0);

			/* Back up over the control word */
			ret = XRCALL(llfio, seekrtn) llfio, -(RDWLEN), whence,
						     stat);
			if (ret < 0) return(ERR);

			/* read the control word */
			ret = XRCALL(llfio, readrtn) llfio, WPTR2BP(&ii),
						     (RDWLEN), stat, PARTIAL,
						     &zero);
			if (ret < 0) return(ERR);

			/*
			 * right justify the data
			 */
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
			ii = (unsigned long) ii >> ((sizeof(ii) - 4) * 8);
#endif
			if (fio->rtype == TR_UX_MIPS) {SWAPB(ii);}

			xf_info->lrdwaddr = bytesize;
			xf_info->last_lrdwaddr = bytesize - (ii + 2*RDWLEN);

		}
		ret = bytesize;		/* return byte offset of EOD */
	case 1:
		/* Get the current position */
		if (pos != 0)
			ERETURN(stat, FDC_ERR_NOSUP, 0);
		if (fio->rwflag == WRITIN){
			/* we already flushed the data */
			ret = xf_info->lrdwaddr;
		}
		else {
			if (fio->rwflag == READIN)
		                if (!(xf_info->flag & ATEOR) &&
					!(fio->ateod ) && 
					!(fio->ateof) )
					ERETURN(stat,FDC_ERR_NOTREC, 0);
			/* In this case we don't reset ptrs, etc */
			return(xf_info->lrdwaddr);
		}
		break;
	}

	fio->rwflag = POSITIN;
	fio->ateof = 0;
	fio->ateod = 0;
	xf_info->recbytes = 0;
	return (ret);
}

/*
 * f77_xseek()
 * Perform limited seek operations.
 *
 * Return value:
 *	>= 0 byte offset in underlying file
 *	<  0 if error
 *
 */
_ffseek_t
_f77_xseek(
struct fdinfo	*fio,
off_t		pos, 
int		whence,
struct ffsw	*stat)
{
#ifdef __mips
	off64_t pos64;
	_ffseek_t ret;
	pos64 = pos;
	ret = __f77_xseek(fio, pos64, whence, stat);
	return(ret);
	
#else
	return(__f77_xseek(fio, pos, whence, stat));
#endif
}

