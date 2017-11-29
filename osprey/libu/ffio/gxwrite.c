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


#pragma ident "@(#) libu/ffio/gxwrite.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <stdlib.h>
#include "gxio.h"

static int put_segment( struct fdinfo *fio, struct ffsw *stat, int fulp);
static int init_seg( struct fdinfo *fio, struct ffsw *stat);
static int put_block( struct fdinfo *fio, struct ffsw *stat, int64 bits);
static int gen_xwrard( struct fdinfo *fio, struct ffsw *stat);


/*
 * Build a NOS/VE RDW length is the total length of the record,
 *	including the control word. (in bytes!)
 *	prev is the disk address of the previous record or EOF
 */
#define BLD_NVE_RDW(rdw, fio, flag, length, current, prev)		\
	{								\
	rdw.wwords[0] = ((flag)<<56) | (((length)-14) << 8) | ((unsigned)(prev) >> 40); \
	rdw.wwords[1] = ((prev) << 24) | (0x1e << 16);			\
	prev = current;							\
	current += length;						\
	}

/*
 * Build a cyber 205 control word.
 */
#define BLD_205_CW(wcw, fio, xf_info, cwtype, sccfl, bytct, total)	\
		{							\
		wcw.wword = 0;		/* zero word */			\
		wcw.fld.r = 0;						\
		wcw.fld.rsv1 = 0;					\
		wcw.fld.fd = cwtype; /* normal rec */			\
		wcw.fld.wcr = sccfl;					\
		wcw.fld.ps = xf_info->lrdwaddr -			\
				xf_info->last_lrdwaddr;			\
		if (xf_info->lrdwaddr == 0) wcw.fld.ps = 8;		\
		wcw.fld.bc = (bytct) - 8;				\
		/* maintain ODD parity */				\
		wcw.fld.p = _poppar(wcw.wword) ^ 1;			\
									\
		xf_info->last_lrdwaddr = xf_info->lrdwaddr; 		\
		xf_info->lrdwaddr += total >> 3;			\
		}

/*
 * Write a X class file
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count (not used for IBM)
 */
ssize_t
_gen_xwrite(
struct fdinfo	*fio,
bitptr		bufptr,
size_t		nbytes,
struct ffsw	*stat,
int		fulp,
int		*ubc)
	{
	ssize_t ret;
	int64  nbits, bits, moved;
	long left;

	nbits = (uint64)nbytes << 3;
	if (*ubc != 0)
		ERETURN(stat, FDC_ERR_UBC, 0);

/*
 *	If we've been reading, then try to switch the buffer into write mode.
 */
	if (fio->rwflag == READIN)
		{
		/*
		 * Issue an error if we are not positioned at a record
		 * boundary.   ffweof would terminate the current record, but
 		 * _cos_write overwrites the current record.   We need to
		 * decide which is the proper approach before permitting this
		 * here.
		 */
		if (fio->_cnt > 0)
			ERETURN(stat, FDC_ERR_NOTREC, 0);

		ret = gen_xwrard(fio, stat);
		if (ret < 0) return(ERR);
		}

	fio->rwflag = WRITIN;
	moved = 0;
/*
 *	Check for record size exceeded.
 */
	if ((fio->maxrecsize > 0) && (fio->recbits + nbits) > fio->maxrecsize)
		ERETURN(stat, FDC_ERR_MXREC, 0);

/*
 *	loop putting data in buffer and building segments
 */

	while (nbits > 0)
		{
/*
 *		bits tells when data has been moved.  Set it to zero
 *		unless someone moves some data in the loop
 */
		bits = 0;
/*
 *		initialize a new segment, if needed.
 */
		if (fio->recbits == 0)
			{
			if (init_seg(fio, stat) != 0)
				return(ERR);
			}
/*
 *		If enough room for bits, put them in the buffer
 */
		left = fio->_ffbufsiz - fio->_cnt;
		if (left >= nbits)
			{
			bits = nbits;
			PUTDATA(bufptr, fio, bits);
			SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
			}
		else
			{
/*
 *			There is not enough room to put all of the data.
 */
			if (left == 0)
				{
				ret = put_segment(fio, stat, PARTIAL);
				if (ret != 0) return(ERR);
				}
			else
				{
				bits = nbits;
				if (nbits > left) bits = left;
				PUTDATA(bufptr, fio, bits);
				SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
				}
			}
		nbits -= bits;
		moved += bits;
		}

	fio->recbits += moved;
	if (fulp == FULL)
		{
/*
 *		Watch out for NULL writes!
 */
		if (fio->recbits == 0)
			if (init_seg(fio, stat) != 0)
				return(ERR);
		ret = put_segment(fio, stat, fulp); /* this will be FULL */
		if (ret != 0)
			return(ERR);

		SETSTAT(stat, FFEOR, (uint64)moved >> 3);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}
	else
		SETSTAT(stat, FFCNT, (uint64)moved >> 3);
			
	return ((uint64)moved >> 3);
	}

/*
 *	init_seg checks if there is room for SDW.  If not, flush block
 *	Also, if no bits are yet in block, and there is data 
 *	to write, set aside room for the BDW. 
 */
static int
init_seg(struct fdinfo *fio, struct ffsw *stat)
	{
	int rdwlen;
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;

	rdwlen = xf_info->rdwlen;
/*
 *	Init new block if full, or if brand new.
 */
	if ((fio->_cnt + rdwlen) >= fio->_ffbufsiz)
		{
		if (put_block(fio, stat, (int64)fio->_cnt) != 0)
			return(ERR);
		}
	/* leave room for SDW */
	SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, rdwlen));
	fio->_cnt += rdwlen;
	fio->segbits += rdwlen;
	fio->recbits += rdwlen;
	return(0);
	}

/*
 * Terminate a data segment by filling in the SDW and starting a new segment
 */
static int
put_segment(struct fdinfo *fio, struct ffsw *stat, int fulp)
	{
	int scc, rdwlen;
	int len1, cwlen;
	int totbits, padd;
	long ii, *cwptr;
	union nosve_rdw_u rdw;
	union w_205_u wcw;
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;

	rdwlen = xf_info->rdwlen;
	switch(fio->lastscc)
		{
		case SCCFRST:
		case SCCMIDL:
			scc = SCCMIDL;
			break;
		case SCCLAST:
		case SCCFULL:
			scc = SCCFRST;
			break;
		}
	if (fulp == FULL)
		scc -= 1;	/* TRICK to convert MIDL to LAST or */
				/* FIRST to FULL */
/*
 *	If in partial mode, output the block if full.  In full
 *	mode, update the RDW.
 */
	if (fulp == FULL)
		{
		switch(fio->rtype)
			{
#ifdef	_CRAY
			case TR_NVE_V:
/*
 *				Build RDW and update lrdwaddr
 */
				BLD_NVE_RDW(rdw, fio, 0, fio->recbits >> 3,
					xf_info->lrdwaddr,
					xf_info->last_lrdwaddr);
				cwptr = (long *)&rdw.wwords[0];
				break;
		
			case TR_CRAY_V:
				abort();
#ifdef _OLD_F77
			case TR_UX_VAX:
/*
 *				Init new block if full, or if brand new.
 */
				if ((fio->_cnt + rdwlen) >= fio->_ffbufsiz)
					{
					if (put_block(fio, stat, (int64)fio->_cnt) != 0)
						return(ERR);
					}

				ii = (fio->recbits - rdwlen) >> 3;
				SWAPB(ii);
				ii = ii << 32;
				cwptr = &ii;
/*
 *				Put in the trailing Control Word
 */
				PUT_BITS(fio->_ptr, *cwptr, rdwlen);
				SET_BPTR(fio->_ptr,
					INC_BPTR(fio->_ptr, rdwlen));
				fio->_cnt += rdwlen;
				fio->recbits += rdwlen;

				xf_info->last_lrdwaddr = xf_info->lrdwaddr; 
				xf_info->lrdwaddr += fio->recbits >> 3;

				break;
#endif
#endif	/* ifdef _CRAY */
#ifdef _OLD_F77
			case TR_UX_SUN:
/*
 *				Init new block if full, or if brand new.
 */
				if ((fio->_cnt + rdwlen) >= fio->_ffbufsiz)
					{
					if (put_block(fio, stat, (int64)fio->_cnt) != 0)
						return(ERR);
					}
				ii = (uint64)(fio->recbits - rdwlen) >> 3;

				ii = ii << ((sizeof(ii) -4) *8);

				cwptr = &ii;
/*
 *				Put in the trailing Control Word
 */
				PUT_BITS(fio->_ptr, *cwptr, rdwlen);
				SET_BPTR(fio->_ptr,
					INC_BPTR(fio->_ptr, rdwlen));
				fio->_cnt += rdwlen;
				fio->recbits += rdwlen;

				xf_info->last_lrdwaddr = xf_info->lrdwaddr; 
				xf_info->lrdwaddr += fio->recbits >> 3;

				break;
#endif
#ifdef	_CRAY
			case TR_205_W:
				totbits = fio->recbits;
				if (fio->recbits & 077)
					totbits = (fio->recbits + 63) & ~077;

				BLD_205_CW(wcw, fio, xf_info, 00, fio->scc,
					fio->recbits >> 3, totbits);

				padd = totbits - fio->recbits;
				if (padd != 0)
					{
					SET_BPTR(fio->_ptr,
						INC_BPTR(fio->_ptr, padd));
					fio->_cnt += padd;
					fio->recbits += padd;
					}
				cwptr = (long *)&wcw;
				break;
#endif	/* ifdef _CRAY */
			default:
				ERETURN(stat, FDC_ERR_INTERR, 0);
			}
		if (fio->recbits  <= fio->_cnt)
			{
			bitptr tbptr;

			SET_BPTR(tbptr, INC_BPTR(fio->_ptr, -fio->recbits));
			cwlen = rdwlen;
			while(cwlen > 0)
				{
				len1 = cwlen;
				if (len1 > 64) len1 = 64;
				PUT_BITS(tbptr, *cwptr, len1);
				SET_BPTR(tbptr, INC_BPTR(tbptr, len1));
				cwlen -= len1;
				cwptr += 1;
				}
			}
		else
			{
			struct fdinfo *llfio;
			int zero = 0;

			llfio = fio->fioptr;
/*
 *			Seek to last CW, then write it.  Then seek back to
 *			the base address of our current buffer.
 */
			if (XRCALL(llfio, seekrtn) llfio,
					xf_info->last_lrdwaddr, 0, stat) < 0)
				return(ERR);
			if (XRCALL(llfio, writertn) llfio,
					WPTR2BP(cwptr), rdwlen >> 3,
					stat, PARTIAL, &zero) < 0)
				return(ERR);
			if (XRCALL(llfio, seekrtn) llfio,
					xf_info->lrdwaddr - (fio->_cnt >> 3),
					0, stat) < 0)
				return(ERR);
			}
		}
	if ((fio->_cnt + rdwlen) >= fio->_ffbufsiz)
		{
		if (put_block(fio, stat, (int64)fio->_cnt) != 0)
			return(ERR);
		}

	fio->lastscc = scc;
	
	fio->segbits = 0;
	return(0);
	}

/*
 * Write a block out.
 */
static int
put_block(struct fdinfo *fio, struct ffsw *stat, int64 bits)
	{
	ssize_t ret;
	int mode;
	int zero = 0;

	mode = PARTIAL;
/*
 *	Write it out.
 */
	WRITEBLK(ret, fio, (size_t)((uint64)bits >> 3), stat, mode, &zero);
	if (ret < 0)
		return(ret);
	return(0);
	}


/*
 * Switch from read mode into neutral mode.
 *
 * Return value:
 *	 0 if OK
 *	-1 if error.  Error code set in stat->sw_error
 */
static int
gen_xwrard(
struct fdinfo	*fio,
struct ffsw	*stat)
	{
	int		ret;

	if (fio->rwflag != READIN)
		return(0);

/*
 *	Can only switch direction on record boundary.  If in
 *	middle of record, go to EOR.
 */
	if (fio->_cnt > 0)
		{
		int		zero;
		char		dummy;
		struct ffsw	locstat;

		zero = 0;
		if (XRCALL(fio, readrtn) fio,
			CPTR2BP(&dummy), 0,
			&locstat, FULL, &zero) < 0)
			return(ERR);
		}

/*
 *	flushrtn shifts the file into neutral
 */
	ret = XRCALL(fio, flushrtn) fio, stat);
                if (ret < 0) return(ERR);

	return(ret);
	}

/*
 * Flush the buffer and clean up
 *	This routine should return 0, or -1 on error.
 */
int
_gen_xflush(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
/*
 *	if reading, nothing to do...
 */
	if (fio->rwflag == READIN)
		{
		fio->_ptr = fio->_base;
		fio->_cnt = 0;
		fio->segbits = 0;
		fio->scc = SCCFULL;
		fio->lastscc = SCCFULL;
		return(0);
		}
/*
 *	In write mode.  Terminate any uncompleted record.
 */
	fio->rwflag = WRITIN;
	if (fio->segbits != 0)
		if (put_segment(fio, stat, FULL))
			return(ERR);
	if (fio->_cnt != 0)
		if (put_block(fio, stat, (int64)fio->_cnt) != 0)
			return(ERR);
	return(0);
	}

/*
 * Write an EOF.
 *	Place an EOF mark in the current layer, if defined.  If not
 *	defined, this routine truncates the dataset at the current point
 *	and sets the ateof flag.
 */
int
_gen_xweof(struct fdinfo *fio, struct ffsw *stat)
	{
	union nosve_rdw_u rdw;
	union w_205_u wcw;
	long bytes, rdwlen;
	long *cwptr = NULL;
	int ret, mode;
	bitptr pword;
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;

/*
 *	If we've been reading try to switch the buffer into write mode.
 */
	if (fio->rwflag == READIN)
		{
		ret = gen_xwrard(fio, stat);
		if (ret < 0) return(ERR);

		fio->rwflag = WRITIN;
		}
	else
		{
		if (fio->ateod != 0)
			ERETURN(stat, FDC_ERR_WPEOD, 0);

		fio->rwflag = WRITIN;
/*
 *		Terminate any uncompleted segments
 */
		if (fio->segbits != 0)
			{
			ret = put_segment(fio, stat, FULL);
			if (ret != 0)
				return(ERR);
			}
		}
/*
 *	Lay down an EOF control word EOF as data, write it out
 */
	rdwlen = xf_info->rdwlen;
	switch(fio->rtype)
		{
#ifdef	_CRAY
		case TR_CRAY_V:
		case TR_NVE_V:
			BLD_NVE_RDW(	rdw,		/* desc word */
					fio,
					0x04,		/* mark byte */
					14,		/* byte length */
					xf_info->lrdwaddr,
					xf_info->last_lrdwaddr
					);
			cwptr = (long *)&rdw;
			break;
		case TR_205_W:
			BLD_205_CW(wcw, fio, xf_info, 03, 00, 0, rdwlen);
			cwptr = (long *)&wcw;
			break;
#endif	/* ifdef _CRAY */

#ifdef _OLD_F77
		case TR_UX_VAX:
		case TR_UX_SUN:
			XRCALL(fio->fioptr, weofrtn) fio->fioptr, stat);
			rdwlen = 0;
			break;
#endif
		default:
			ERETURN(stat, FDC_ERR_INTERR, 0);
		}

	if (rdwlen != 0)
		{
		int zero = 0;
		SET_BPTR(pword, WPTR2BP(cwptr));
		PUTDATA(pword, fio, rdwlen);
		bytes = fio->_cnt >> 3;
		mode = PARTIAL;
		WRITEBLK(ret, fio, bytes, stat, mode, &zero);
		if (ret < 0) return(ret);
		}

	fio->ateof = 1;
	fio->ateod = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return(0);
	}

/*
 * Write an EOD.
 *	Truncate the file.  This is the 'real' end of the data.
 */
int
_gen_xweod(struct fdinfo *fio, struct ffsw *stat)
	{
	int ret;
	struct fdinfo *llfio;
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;

	/* !! Write after Read!! */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);

	if (fio->ateod != 0)
		{
		SETSTAT(stat, FFEOD, 0);
		return(0);
		}

	fio->rwflag = WRITIN;
/*
 *	Terminate any uncompleted segments
 */
	if (fio->segbits != 0)
		{
		ret = put_segment(fio, stat, FULL);
		if (ret != 0)
			return(ERR);
		}
	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret < 0) return(ret);

	llfio = fio->fioptr;
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0) return(ret);

	fio->ateof = 0;
	fio->ateod = 1;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	xf_info->rembits = 0;
	return(0);
	}
