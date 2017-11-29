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


#pragma ident "@(#) libu/ffio/f77write.c	92.6	11/08/99 11:14:26"


#include <ffio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/listio.h>
#elif defined(__mips)
#include "listio_mips.h"
#endif
#include "f77io.h"

static int _f77_put_record( struct fdinfo *fio, struct ffsw *stat, int fulp);
static int _f77_put_block( struct fdinfo *fio, struct ffsw *stat, size_t bytes);
static int f77_xwrard( struct fdinfo *fio, struct ffsw *stat);

/*
 * The f77 layer makes no particular effort to be aligned in any way
 * within the file, other than trying to write/read full buffers.
 * As long as we are writing/reading sequentially, we should always
 * be aligned on a buffer boundary.  Any positioning will destroy that.
 * Even with no positioning, we may have to seek back and write a control
 * word.  This routine just attempts to make sure that the user's buffer
 * is memory aligned.
 * Returns:
 *		1 if address 'p' is aligned
 *		0 other wise
 */
static int _f77_aligned(char *p)
{
#ifdef	_CRAYMPP
	long	pl;
	/* Want it to be aligned on an 8-word boundary */
#define CACHE_MASK 0x3f
	pl = (long)p;
	if ((pl & CACHE_MASK) == 0)
		return(1);
	return(0);
#elif	_CRAY1
	/* want it be aligned on a word boundary */
	if (BPBITOFF(CPTR2BP(p)) == 0)
		return (1);
	return (0);
#elif	defined(__mips) || defined(_LITTLE_ENDIAN)
	return (1);	/* We know of no particular alignment requirements */
			/* except when O_DIRECT is specified. This layer */
			/* is not set up for O_DIRECT now. */
#else
	return (1);
#endif
}

/*
 * Write a f77 class file
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count (not used for IBM)
 */
ssize_t
_f77_xwrite(
struct fdinfo	*fio,
bitptr		bufptr,
size_t		nbytes,
struct ffsw	*stat,
int		fulp,
int		*ubc)
{
	ssize_t ret;
	size_t  bytomove, moved, bytes;
	struct f77_xf *xfinfo;	
	struct fflistreq list_array[1];
	long left;
	char *cbufptr;
	long ii;
	char *cb;
	int ijk;
	int cwbytes;
	int ernum;
	int zero = 0;

	cbufptr = BPTR2CP(bufptr);
	if ((BPBITOFF(bufptr) & 07) != 0) {
		ernum = FDC_ERR_REQ;
		goto eret;
	}
	if (*ubc != 0){
		ernum = FDC_ERR_UBC;
		goto eret;
	}
        xfinfo = (struct f77_xf *)fio->lyr_info;
/*
 *	If we've been reading, then try to switch the buffer into write mode.
 */
	if (fio->rwflag == READIN) {
		/*
		 * Issue an error if we are not positioned at a record
		 * boundary.   ffweof would terminate the current record, but
 		 * _cos_write overwrites the current record.   We need to
		 * decide which is the proper approach before permitting this
		 * here.
		 */
		if (!(xfinfo->flag & ATEOR) && !(fio->ateod ) && !(fio->ateof)) {
		ernum = FDC_ERR_NOTREC;
		goto eret;
		}
		ret = f77_xwrard(fio, stat);
		if (ret < 0) return(ERR);
	}

	fio->rwflag = WRITIN;

/*
 *	initialize a new record, if needed.
 */
	bytomove = nbytes;
	moved = 0;
/*
 *	Check for record size exceeded.
 */
	if (bytomove > 0) {
		if ((xfinfo->maxrecsize > 0) &&
			(xfinfo->recbytes + bytomove) > xfinfo->maxrecsize){
			ernum = FDC_ERR_MXREC;
			goto eret;
		}
	}
	if (xfinfo->recbytes == 0) {
		/* This is the start of the record */
		ii = bytomove;
		if (fio->rtype == TR_UX_MIPS) {SWAPB(ii);}
		if ((bytomove > 0) || (fulp == FULL)) {
/*
 *			Put our guess at a control word in the buffer.
 *			This is the control word at the beginning of record.
 */
			cwbytes = RDWLEN;
			cb = (char *)&ii;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
			cb += sizeof(ii) - RDWLEN;	/* The control word is only RDWLEN bytes long */
#endif
			if ((xfinfo->_cnt + RDWLEN) >= xfinfo->_ffbufsiz) {
				/* only part of the control word will fit */
				/* in this buffer.  Insert what will fit. */
				for (ijk = 0; ijk < xfinfo->_ffbufsiz - xfinfo->_cnt; ijk++){
					*(xfinfo->_ptr++) = *cb++;
					cwbytes--;
				}
				/* buffer is full. write it out. */
				if (_f77_put_block(fio, stat, (size_t)xfinfo->_ffbufsiz) != 0)
					return(ERR);
			}
			for (ijk = 0; ijk < cwbytes; ijk++){
				*(xfinfo->_ptr++) = *cb++;
			}
			xfinfo->_cnt += cwbytes;
			xfinfo->recbytes += RDWLEN;
			xfinfo->cwwritten = 1;
		}
	}

	else {
		/* This record has already been started. */
		ii = (xfinfo->recbytes + bytomove - RDWLEN) ;
		if (fio->rtype == TR_UX_MIPS) {SWAPB(ii);}
		if (bytomove != 0) {
/*
 *			If the control word at the start of the 
 *			record is in the buffer, update it.
 */
			if (xfinfo->recbytes <= xfinfo->_cnt){
				char *tbptr;
				/* the whole control word is in the buffer */
				cb = (char *)&ii;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
				cb += sizeof(ii) - RDWLEN;	/* The control word is only RDWLEN bytes long */
#endif
				tbptr = xfinfo->_ptr - xfinfo->recbytes;
				for (ijk = 0; ijk < RDWLEN; ijk++)
					*(tbptr++) = *cb++;
				xfinfo->cwwritten = 1;
			}		
			else if ((xfinfo->recbytes - RDWLEN) <= xfinfo->_cnt){
				char *tbptr;
				int istart;
				/* part of the control word is in the buffer */
				/* Insert what will fit. */
				cb = (char *)&ii;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
				cb += sizeof(ii) - RDWLEN;	/* The control word is only RDWLEN bytes long */
#endif
				istart = xfinfo->recbytes -xfinfo->_cnt;
				cb += istart;
				tbptr = xfinfo->_base;
				for (ijk = istart; ijk < RDWLEN; ijk++)
					*(tbptr++) = *cb++;
				xfinfo->cwwritten = 0; /* 0 because this is */
						/* not the whole thing*/
			}
			else 
				xfinfo->cwwritten = 0;
		}
	}
/*
 *	loop putting data in buffer 
 */
	while (bytomove > 0) {
/*
 *		bytes tells when data has been moved.  Set it to zero
 *		unless someone moves some data in the loop
 */
/*
 *		If enough room for bytes, put them in the buffer
 */
		left = xfinfo->_ffbufsiz - xfinfo->_cnt;
		if (left == 0) {
			if (_f77_put_block(fio, stat, (size_t)xfinfo->_cnt) != 0)
				return(ERR);
			left = xfinfo->_ffbufsiz;
#ifdef	__CRAY
#pragma _CRI inline _f77_aligned
#elif	defined(__mips) || defined(_LITTLE_ENDIAN)
#pragma inline _f77_aligned
#endif
			if ((bytomove >= left) && _f77_aligned(cbufptr)) {
				/* We write directly from the user's buffer */
				bytes = bytomove - bytomove%xfinfo->_ffbufsiz;
				ret = XRCALL(fio->fioptr, writertn) fio->fioptr,
					CPTR2BP(cbufptr), bytes, stat, PARTIAL,
					&zero);
				if (ret != bytes){
					return(ERR);
				}	
				bytomove -= bytes;
				cbufptr += bytes;
				moved += bytes;
			}
		}
		bytes = (bytomove < left)? bytomove : left;
		memcpy(xfinfo->_ptr, cbufptr, bytes);
		xfinfo->_cnt += bytes;
		xfinfo->_ptr += bytes;
		cbufptr += bytes;
		bytomove -= bytes;
		moved += bytes;
	}

	xfinfo->recbytes += moved ;
	if (fulp != FULL) {
		SETSTAT(stat, FFCNT, moved );
		return(moved);
	}
/*
 *	Take care of the trailing control word.
 */
	cb = (char *)&ii;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
	cb += sizeof(ii) - RDWLEN;	/* The control word is only RDWLEN bytes long */
#endif
	cwbytes = RDWLEN;
	if ((xfinfo->_cnt + RDWLEN) >= xfinfo->_ffbufsiz) {
/*
 *		First put in as much of the trailing control word as will
 *		fit. We do this so we will always write a full buffer, and
 *		hopefully stay aligned with a sector. However, backspacing
 *		or positioning can mess up our alignment.
 */
		for (ijk = 0; ijk < xfinfo->_ffbufsiz - xfinfo->_cnt; ijk++){
			*(xfinfo->_ptr++) = *cb++;
			cwbytes--;
		}
		if (_f77_put_block(fio, stat, (size_t)xfinfo->_ffbufsiz) != 0)
			return(ERR);
	}
/*
 *	Put in the remainder of the trailing Control Word
 */
	for (ijk = 0; ijk < cwbytes; ijk++)
		*(xfinfo->_ptr++) = *cb++;
	xfinfo->_cnt += cwbytes;
	
	xfinfo->last_lrdwaddr = xfinfo->lrdwaddr; 
	xfinfo->lrdwaddr += xfinfo->recbytes + RDWLEN;

/*
 *	Put the control word at the start of the record if we have 
 * 	not already done so.
 */
	if (xfinfo->cwwritten != 0) {
		SETSTAT(stat, FFEOR, moved );
		xfinfo->recbytes = 0;
		return (moved);
	}
	else {	
		struct fdinfo *llfio;
		cb = (char *)&ii;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
		cb += sizeof(ii) - RDWLEN;	/* The control word is only RDWLEN bytes long */
#endif

		llfio = fio->fioptr;
/*
 *		Seek to last CW, then write it.  Then seek back to
 *		the base address of our current buffer.
 */
		if (xfinfo->do_sylistio) {
#if	!defined(_LITTLE_ENDIAN)
			struct ffsw locstat;
                        list_array[0].li_opcode    = LO_WRITE;
                        list_array[0].li_flags     = LF_LSEEK;
                        list_array[0].li_offset    = xfinfo->last_lrdwaddr;
#ifdef	__mips
                        list_array[0].li_ffioptr   = llfio;
#else
                        list_array[0].li_fildes    = (int)llfio;
#endif
                        list_array[0].li_buf       = cb;
                        list_array[0].li_nbyte     = (size_t)RDWLEN;
                        list_array[0].li_status    = &locstat;
                        list_array[0].li_signo     = 0;
                        list_array[0].li_nstride   = 1;
                        list_array[0].li_filstride = 0;
                        list_array[0].li_memstride = 0;
                        if ( XRCALL(llfio, listiortn)LC_WAIT, list_array,
                                1, stat) < 0)
                                return(ERR);
                        if (locstat.sw_error != 0) {
                                _SETERROR(stat, locstat.sw_error, 0);
                                return(ERR);
                        }
#endif
		}
		else {
			if (XRCALL(llfio, seekrtn) llfio,
				xfinfo->last_lrdwaddr, 0, stat) < 0)
				return(ERR);
			if (XRCALL(llfio, writertn) llfio,
				CPTR2BP(cb), (size_t)RDWLEN ,
				stat, PARTIAL, &zero) < 0)
				return(ERR);
		}
		if (XRCALL(llfio, seekrtn) llfio,
			xfinfo->lrdwaddr - (xfinfo->_cnt ),
			0, stat) < 0)
			return(ERR);
	}
	SETSTAT(stat, FFEOR, moved );
	xfinfo->recbytes = 0;
			
	return (moved);
eret:
	ERETURN(stat, ernum, 0);
}


/*
 * if parameter fulp == FULL, then we terminate a record by
 * filling in the trailing control word, and the beginning control word.
 * if fulp == PARTIAL,  we write out the block if it is full.
 */
static int
_f77_put_record(struct fdinfo *fio, struct ffsw *stat, int fulp)
{
	long ii;
	struct f77_xf *xfinfo;
	int ijk;
	char *cb;

	xfinfo = (struct f77_xf *)fio->lyr_info;

	if (fulp == FULL) {
/*
 *		If the block is full, write it out
 */
		if ((xfinfo->_cnt + RDWLEN) >= xfinfo->_ffbufsiz) {
			if (_f77_put_block(fio, stat, (size_t)xfinfo->_cnt) != 0)
				return(ERR);
		}
		ii = (xfinfo->recbytes - RDWLEN) ;

/*
 *		Put in the trailing Control Word
 */
		if (fio->rtype == TR_UX_MIPS) {SWAPB(ii);}
		cb = (char *)&ii;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
		cb += sizeof(ii) - RDWLEN;	/* The control word is only RDWLEN bytes long */
#endif
		for (ijk = 0; ijk < RDWLEN; ijk++)
			*(xfinfo->_ptr++) = *cb++;
		xfinfo->_cnt += RDWLEN;
		xfinfo->recbytes += RDWLEN;

		xfinfo->last_lrdwaddr = xfinfo->lrdwaddr; 
		xfinfo->lrdwaddr += xfinfo->recbytes;

		cb = (char *)&ii;
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
		cb += sizeof(ii) - RDWLEN;	/* The control word is only RDWLEN bytes long */
#endif
		if (xfinfo->recbytes  <= xfinfo->_cnt) {
			char *tbptr;
			/* entire beginning control word fits in the buffer */
			tbptr = xfinfo->_ptr - xfinfo->recbytes;
			for (ijk = 0; ijk < RDWLEN; ijk++)
				*(tbptr++) = *cb++;
		}
		else {
			struct fdinfo *llfio;
			int zero = 0;

			if ((xfinfo->recbytes - RDWLEN) <= xfinfo->_cnt){
				/* part of the beginning control word */
				/* fits in the buffer. Put it there so */
				/* it will be correct when this buffer is */
				/* written. */
				char *tbptr;
				int istart;
				/* part of the control word is in the buffer */
				/* Insert what will fit. */
				istart = xfinfo->recbytes -xfinfo->_cnt;
				cb += istart;
				tbptr = xfinfo->_base;
				for (ijk = istart; ijk < RDWLEN; ijk++)
					*(tbptr++) = *cb++;
			}
			llfio = fio->fioptr;
/*
 *			Seek to last CW, then write it.  Then seek back to
 *			the base address of our current buffer.
 */
			if (XRCALL(llfio, seekrtn) llfio,
					xfinfo->last_lrdwaddr, 0, stat) < 0)
				return(ERR);
			if (XRCALL(llfio, writertn) llfio,
					CPTR2BP(cb), RDWLEN ,
					stat, PARTIAL, &zero) < 0)
				return(ERR);
			if (XRCALL(llfio, seekrtn) llfio,
					xfinfo->lrdwaddr - (xfinfo->_cnt ),
					0, stat) < 0)
				return(ERR);
		}
		xfinfo->recbytes = 0;
	}
	if ((xfinfo->_cnt + RDWLEN) >= xfinfo->_ffbufsiz) {
		if (_f77_put_block(fio, stat, (size_t)xfinfo->_cnt) != 0)
			return(ERR);
	}

	return(0);
}

/*
 * Write a block out.
 */
static int
_f77_put_block(struct fdinfo *fio, struct ffsw *stat, size_t bytes)
{
	ssize_t ret;
        struct f77_xf *xfinfo;
	int zero = 0;

	xfinfo = (struct f77_xf *)fio->lyr_info;

/*
 *	Write it out.
 */
	ret = XRCALL(fio->fioptr, writertn) fio->fioptr, CPTR2BP(xfinfo->_base),
		bytes, stat, PARTIAL, &zero);
	if (ret >= 0) {
		xfinfo->_cnt = 0;
		xfinfo->_ptr = xfinfo->_base;
		return(0);
	}
	else
		return(ret);
}

/*
 * Switch from read mode into neutral mode.
 *
 * Return value:
 *	 0 if OK
 *	-1 if error.  Error code set in stat->sw_error
 */
static int
f77_xwrard(
struct fdinfo	*fio,
struct ffsw	*stat)
{
	struct f77_xf *xfinfo;

	if (fio->rwflag != READIN)
		return(0);
	xfinfo = (struct f77_xf *)fio->lyr_info;

/*
 *	Can only switch direction on record boundary.  If in
 *	middle of record, go to EOR.
 */
	if (!(xfinfo->flag & ATEOR) && !(fio->ateod ) && !(fio->ateof)) {
		if (_f77skip2eor(fio, stat) < 0)
			return -1;
	}

/*
 *	We need to seek back to our logical position 
 */
	if (XRCALL(fio->fioptr, seekrtn) fio->fioptr, -(xfinfo->_cnt),
		1, stat) < 0)
		return(ERR);

/*
 *	Reset pointers, etc.
 */
	xfinfo->rembytes = 0;
	xfinfo->_ptr = xfinfo->_base;
	xfinfo->_cnt = 0;
	xfinfo->recbytes = 0;
	xfinfo->flag &= ~ATEOR;
	return(0);
}

/*
 * Flush the buffer and clean up
 *	This routine should return 0, or -1 on error.
 */
int
_f77_xflush(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
	struct f77_xf *xfinfo;
/*
 *	if reading, nothing to do...
 */
	xfinfo = (struct f77_xf *)fio->lyr_info;
	if (fio->rwflag == READIN) {
		xfinfo->_ptr = xfinfo->_base;
		xfinfo->_cnt = 0;
		return(0);
	}
/*
 *	In write mode.  Terminate any uncompleted record.
 */
	if (fio->rwflag == WRITIN && xfinfo->recbytes != 0)
		if (_f77_put_record(fio, stat, FULL)){
			return(ERR);
		}
	fio->rwflag = WRITIN;
	if (xfinfo->_cnt != 0)
		if (_f77_put_block(fio, stat, (size_t)xfinfo->_cnt) != 0){
			return(ERR);
		}
	return(0);
}

/*
 * Write an EOF.
 *	Place an EOF mark in the current layer, if defined.  If not
 *	defined, this routine truncates the dataset at the current point
 *	and sets the ateof flag.
 */
int
_f77_xweof(struct fdinfo *fio, struct ffsw *stat)
{
	int	ret;
	struct f77_xf *xfinfo;

	xfinfo = (struct f77_xf *)fio->lyr_info;

/*
 *	If we've been reading try to switch the buffer into write mode.
 */
	if (fio->rwflag == READIN) {

		ret	= f77_xwrard(fio, stat);

		if (ret < 0)
			return(ERR);

		fio->rwflag	= WRITIN;
	}
	else {
		if (fio->ateod != 0)
			ERETURN(stat, FDC_ERR_WPEOD, 0);

/*
 *		Terminate any uncompleted record
 */
		if (fio->rwflag == WRITIN && xfinfo->recbytes != 0) {
			ret	= _f77_put_record(fio, stat, FULL);
			if (ret != 0)
				return(ERR);
		}
		fio->rwflag = WRITIN;
	}
/*
 *	Lay down an EOF control word EOF as data, write it out
 */
	XRCALL(fio->fioptr, weofrtn) fio->fioptr, stat);


	fio->ateof = 1;
	fio->ateod = 0;
	xfinfo->recbytes = 0;
	return(0);
}

/*
 * Write an EOD.
 *	Truncate the file.  This is the 'real' end of the data.
 */
int
_f77_xweod(struct fdinfo *fio, struct ffsw *stat)
{
	int	ret;
	struct fdinfo *llfio;
	struct f77_xf *xfinfo;

	xfinfo = (struct f77_xf *)fio->lyr_info;

	if (fio->rwflag == READIN) {
		ret	= f77_xwrard(fio, stat);

		if (ret < 0)
			return(ERR);

		fio->rwflag	= WRITIN;
	}
	else
		if (fio->ateod != 0) {
			SETSTAT(stat, FFEOD, 0);
			return(0);
		}

	fio->rwflag = WRITIN;
	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret < 0) return(ret);

	llfio = fio->fioptr;
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0) return(ret);

	fio->ateof = 0;
	fio->ateod = 1;
	xfinfo->recbytes = 0;
	xfinfo->rembytes = 0;
	return(0);
}
