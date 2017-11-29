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


#pragma ident "@(#) libu/ffio/gxread.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include "gxio.h"

static int get_segment(struct fdinfo *fio, struct ffsw *stat);
static int seteod(struct fdinfo *fio, struct ffsw *stat);
static int skip2eor(struct fdinfo *fio, struct ffsw *stat);

/*
 * Read a generic X class file
 * This class is intended to include the NOS/VE V format and some new formats
 * in the future.
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 *	ubc	- pointer to unused bit count (not used this class of file)
 *
 * Note that xf_info->lscc is used instead of fio->scc to determine record
 * boundaries.  This is because the record segments might be bigger than
 * the buffer, and we still want to read the record in chunks.  The *real*
 * SCC, as found in the file must be remembered, however. lscc is used to
 * 'fool' the higher level routines into seeing the buffer-fulls as
 * individual segments.
 */
ssize_t
_gen_xread(
struct fdinfo	*fio,
bitptr		bufptr,
size_t		nbytes,
struct ffsw	*stat,
int		fulp,
int		*ubc)
{
	int64 bits, moved, nbits;
	int ret, eorstat;
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;

	nbits = (uint64)nbytes << 3;
	if (*ubc != 0 && fio->rtype != TR_CRAY_V)
		ERETURN(stat, FDC_ERR_UBC, 0);

	moved = 0;
	if (fio->rwflag == WRITIN) {
		/* read after write error */
		ERETURN(stat, FDC_ERR_RAWR, 0);
	}

	fio->rwflag = READIN;
/*
 *	If previous segment was empty, get new segment
 */
	if (fio->segbits == 0) {
		ret = get_segment(fio, stat);
		if (ret <= 0)
			return(ret);	/* probable EOF, EOD, orERR */
	}
/*
 *	loop getting segments and moving data
 */
	eorstat = FFCNT;
	while (nbits > 0) {
		bits = nbits;
		if (fio->segbits < nbits)
			bits = fio->segbits;
/*
 *		If segbits is ever negative, we are in deep weeds.
 */
		SANITYCHK (fio->segbits,LT,0);
/*
 *		Get the data from the buffer
 */
		if (bits > 0)
			{
			GETDATA(bufptr, fio, bits);
			SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
			nbits -= bits;
			moved += bits;
			}
/*
 *		Got some bits.  Depending on format, loop around again,
 *		or set status and prepare to return.  For non-segmented formats,
 *		the SCC is always FULL, so no segment spanning is done.
 *
 *		Note that if segbits != 0, then nbits must also be
 */
		if (fio->segbits == 0) {
			if (xf_info->lscc == SCCLAST ||
			    xf_info->lscc == SCCFULL) {
				eorstat = FFEOR;
				nbits = 0; /* exit loop */
			}
			else {
				ret = get_segment(fio, stat);
				if (ret <= 0)
					return(ret);	/* EOF, EOD, or ERR */
			}
		}
	} /* end while */
/*
 *	Check for zero length segment at beginning of buffer
 */
	if (fio->segbits == 0 &&
	    (xf_info->lscc == SCCLAST || xf_info->lscc == SCCFULL))
		eorstat = FFEOR;

	fio->recbits += moved;
	if (fulp || eorstat == FFEOR) {
		ret = skip2eor(fio, stat);
		if (ret <= 0)
			return(ret);	/* EOF/EOD or error */

		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
	}
			
	SETSTAT(stat, eorstat, (uint64)moved >> 3);
	return ((uint64)moved >> 3);
}

/*
 * Fetch a segment from the data stream.  For NOS/VE it reads the 14-byte
 * descriptor word and decodes it, setting up the record length.
 * If the record is not yet exhausted the next chunk of data is read into
 * the buffer
 *	returns:
 *		>0 got it!
 *		=0 EOF or EOD
 *		<0 some error.
 */
static int
get_segment(struct fdinfo *fio, struct ffsw *stat)
{
	union nosve_rdw_u rdw;
	union w_205_u wcw;
	long rdwlen;
	int64 bits;
	int64 dbits;
	long remb;
	int zero, ret, flag;
	long ii;
	struct gen_xf *xf_info;
	ssize_t rret;

	xf_info = (struct gen_xf *)fio->lyr_info;

	if (fio->ateod != 0)
		return(seteod(fio,stat));

	zero = 0;
	rdwlen = xf_info->rdwlen;
again:
	fio->_ptr = fio->_base;
/*
 *	Set the 'local' SCC to the 'actual' SCC.  In effect, assume that the
 *	segment is not bigger than the working buffer.
 */
	xf_info->lscc = fio->scc;
/*
 *	If there is no more in the record, crack a new record
 *	control word.
 */
	if (xf_info->rembits == 0 && xf_info->cwskip == 0)
		{
		READBLK(rret, fio, rdwlen >> 3, stat, PARTIAL, &zero);
		if (rret < 0)
			return(ERR);
		if (fio->_cnt == 0) {
			return(seteod(fio,stat));
		}

		switch(fio->rtype) {
#ifdef _CRAY
		case TR_NVE_V:
/*
 *			get flag byte + length field
 */
			GET_BITS(rdw.wwords[0], fio->_ptr, 56);
			SKIP_BITS(fio, 56);

/*
 *			get prev addr + mark byte
 */
			GET_BITS(rdw.wwords[1], fio->_ptr, 56);
			SKIP_BITS(fio, 56);

			xf_info->rembits = (rdw.wwords[0] << 8 ) >> (16 - 3);
			flag = rdw.wwords[0] >> 56;
			xf_info->last_lrdwaddr = xf_info->lrdwaddr;
			xf_info->lrdwaddr = xf_info->lrdwaddr +
				((unsigned)(xf_info->rembits + rdwlen) >> 3);
			if (((rdw.wwords[1] << 48 ) >> 56 ) != 0x1e)
				ERETURN(stat, FDC_ERR_BADNVE, 0);
/*
 *			Detect EOF control word
 */
			if (flag == 0x04)
				return(seteof(fio, stat));
			fio->scc = SCCFULL;

			break;
#endif	/* ifdef _CRAY */
#ifdef _OLD_F77
		case TR_UX_SUN:
/*
 *			get length
 */
			GET_BITS(ii, fio->_ptr, rdwlen);
			SKIP_BITS(fio, rdwlen);

			/*
			 * right justify the data
			 */
			ii = (unsigned long) ii >> ((sizeof(ii) - 4) * 8);

			xf_info->rembits = (uint64)ii << 3;/* bytes -> bits */

			xf_info->last_lrdwaddr = xf_info->lrdwaddr;
			xf_info->lrdwaddr = xf_info->lrdwaddr +
				((uint64)(xf_info->rembits + rdwlen*2) >> 3);
			fio->scc = SCCFULL;
			xf_info->cwskip = rdwlen;
			break;
#endif
#ifdef	_CRAY
#ifdef _OLD_F77
		case TR_UX_VAX:
/*
 *			get length
 */
			GET_BITS(ii, fio->_ptr, rdwlen);
			SKIP_BITS(fio, rdwlen);

			ii = (unsigned int) ii >> 32;

			/* swap bytes */
			SWAPB(ii);

			xf_info->rembits = ii << 3; /* bytes -> bits */
			xf_info->last_lrdwaddr = xf_info->lrdwaddr;
			xf_info->lrdwaddr = xf_info->lrdwaddr +
				((unsigned)(xf_info->rembits + rdwlen*2) >> 3);
			fio->scc = SCCFULL;
			xf_info->cwskip = rdwlen;
			break;
#endif
		case TR_205_W:
/*
 *			get control word
 */
			GET_BITS(wcw.wword, fio->_ptr, rdwlen);
			SKIP_BITS(fio, rdwlen);

			remb = wcw.fld.bc << 3;
			xf_info->cwskip =((remb + 077) & (~077)) - remb;
			xf_info->rembits = remb;
			flag = wcw.fld.fd;
			xf_info->last_lrdwaddr = xf_info->lrdwaddr;
			xf_info->lrdwaddr = xf_info->lrdwaddr +
				((unsigned)(xf_info->rembits + rdwlen) >> 3);
			fio->scc = SCCFULL;
			xf_info->lscc = fio->scc;
/*
 *			Detect deleted record
 */
			if (flag == 1) {
				ret = skip2eor(fio, stat);
				if (ret < 0) return(ret);
				goto again;
			}
/*
 *			Detect EOG control word.  Map to FFEOF.
 */
			else if (flag == 2) /* if EOG */
				return(seteof(fio, stat));
/*
 *			Detect EOF control word.  Map to FFEOD.
 */
			else if (flag == 3) /* if EOF */
				return(seteod(fio, stat));
			else {
				switch(wcw.fld.wcr) {
				case 0:
					fio->scc = SCCFULL;
					break;
				case 1:
					fio->scc = SCCFRST;
					break;
				case 2:
					fio->scc = SCCMIDL;
					break;
				case 3:
					fio->scc = SCCLAST;
					break;
				default:
					ERETURN(stat, FDC_ERR_SCC, 0);
				}
				xf_info->lscc = fio->scc;
			}
			break;
		case TR_CRAY_V:
#endif	/* defined _CRAY */

		default:
			ERETURN(stat, FDC_ERR_INTERR, 0);
		}
	}
/*
 *	If necessary, carve the segment into buffer-sized parts
 *	Make sure that the cwskip padd/ctrl-word is not split
 */
	bits = xf_info->rembits + xf_info->cwskip;	/* bits to read */
	dbits = xf_info->rembits;			/* data bits */
	if (bits > fio->_ffbufsiz) {
/*
 *		Don't allow a control word to span buffers.
 */
		if (dbits < fio->_ffbufsiz)
			bits = dbits;
		else {
			bits = fio->_ffbufsiz;
			dbits = bits;
		}
		xf_info->lscc = SCCMIDL;
	}
	else
		xf_info->cwskip = 0;
#if DEBUG
	if (bits > xf_info->rembits &&
	    bits < xf_info->rembits + xf_info->cwskip) {
		_xrt_putf("SANITY CHECK: rembits < bits < seg !!\n");
		GTRACEBACK();
		exit(1);
	}
#endif
	fio->_ptr = fio->_base;
	READBLK(rret, fio, (size_t)((uint64)bits >> 3), stat, PARTIAL, &zero);
	if (rret < 0) return(ERR);
	if (fio->_cnt < bits)
		ERETURN(stat, FDC_ERR_UXEND, 0);

	fio->segbits = dbits;
	xf_info->rembits -= dbits;
	return(1);
}

/*
 *	Common code to handle EOF return
 */
static
seteof(struct fdinfo *fio, struct ffsw *stat)
{
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;
	fio->segbits = 0;	/* no data here.. */
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	xf_info->rembits = 0;
	fio->_cnt = 0;
	fio->ateof = 1;
	fio->ateod = 0;
	SETSTAT(stat, FFEOF, 0)
	return(0);
}

/*
 *	Common code to handle EOD return
 */
static
seteod(struct fdinfo *fio, struct ffsw *stat)
{
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;
	fio->segbits = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	xf_info->rembits = 0;
	fio->_cnt = 0;
	fio->ateof = 0;
	fio->ateod = 1;
	SETSTAT(stat, FFEOD, 0)
	return(0);
}

/*
 * Skip2eor skips ahead to the end of the record, reading up blocks and
 * records as needed until the SCC says FFEOR, or hit EOF/EOD.
 *	Returns:
 *		<0 - error
 *		=0 - EOF/EOD
 *		>0 - EOR
 */
static
skip2eor(struct fdinfo *fio, struct ffsw *stat)
{
	int ret;
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;
	if (fio->segbits > 0) {
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		fio->recbits += fio->segbits;
		fio->segbits = 0;
	}

	while (xf_info->lscc == SCCFRST || xf_info->lscc == SCCMIDL) {
		ret = get_segment(fio, stat);
		if (ret <= 0)
			return(ret);	/* probable EOF/EOD */
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		fio->recbits += fio->segbits;
		fio->segbits = 0;
	}
	fio->_cnt = 0;
	return(1);
}



#ifdef __mips
static
off64_t
__gen_xseek(
struct fdinfo	*fio,
off64_t		pos, 
int		whence,
struct ffsw	*stat)
#else
static
_ffseek_t
__gen_xseek(
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
	int rdwlen;
	struct fdinfo *llfio;
	struct gen_xf *xf_info;

/*
 *	Seeks to a nonzero offset or an absolute position are not supported.
 */
	if (pos != 0 || whence == 1)
		ERETURN(stat, FDC_ERR_NOSUP, 0);

	xf_info = (struct gen_xf *)fio->lyr_info;

/*
 *	If the file's been writing, complete any partial records, truncate
 *	the file, and flush the buffer.
 *
 *	Else if we've been reading, call flush to shift into neutral.
 */
	if (fio->rwflag == WRITIN) {
		ret = XRCALL(fio, flushrtn) fio, stat);
		if (ret < 0) return(ERR);

		ret = XRCALL(fio, weodrtn) fio, stat);
		if (ret < 0) return(ERR);
	}
	else if (fio->rwflag == READIN) {
		ret = XRCALL(fio, flushrtn) fio, stat);
		if (ret < 0) return(ret);
	}

/*
 *	Do special X class things
 */
	fio->scc = SCCFULL;
	fio->lastscc = SCCFULL;
	xf_info->lscc = SCCLAST;
	xf_info->rembits = 0;
	
	llfio = fio->fioptr;

	switch (whence) {
	case 0:
		xf_info->lrdwaddr = 0;
		xf_info->last_lrdwaddr = 0;
		ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
		break;

	case 2:
		/*
		 *	If we've been writing, we are at the end because of
		 *	FFC_WRTRUNC.
		 */
		if (fio->rwflag == WRITIN)
			break;

		/*
		 *	Other seeks to the end are supported only for SUN/VAX.
		 */
		if (pos != 0)
			ERETURN(stat, FDC_ERR_NOSUP, 0);
		if (fio->rtype != TR_UX_SUN && fio->rtype != TR_UX_VAX)
			ERETURN(stat, FDC_ERR_NOSUP, 0);

		rdwlen = xf_info->rdwlen;
		ret = XRCALL(llfio, seekrtn) llfio, 0, whence, stat);
		if (ret < 0) return(ERR);

		bytesize = ret;

		if (bytesize == 0) {	/* if file is empty */
			xf_info->lrdwaddr = 0;
			xf_info->last_lrdwaddr = 0;
		}
		else {		/* if file is not empty */
			long ii;
			int zero = 0;

			if (bytesize < 2*(rdwlen/8))
				ERETURN(stat, FDC_ERR_FMT, 0);

			/* Back up over the control word */
			ret = XRCALL(llfio, seekrtn) llfio, -(rdwlen/8), whence,
						     stat);
			if (ret < 0) return(ERR);

			/* read the control word */
			ret = XRCALL(llfio, readrtn) llfio, WPTR2BP(&ii),
						     (rdwlen/8), stat, PARTIAL,
						     &zero);
			if (ret < 0) return(ERR);

			/*
			 * right justify the data
			 */
			ii = (unsigned long) ii >> ((sizeof(ii) - 4) * 8);

			xf_info->lrdwaddr = bytesize;
			xf_info->last_lrdwaddr = bytesize - (ii + 2*rdwlen/8);

		}
		ret = bytesize;		/* return byte offset of EOD */
	}

	fio->rwflag = POSITIN;
	fio->ateof = 0;
	fio->ateod = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return (ret);
}

/*
 * gen_xseek()
 * Perform limited seek operations.
 *
 * Return value:
 *	>= 0 byte offset in underlying file
 *	<  0 if error
 *
 */
_ffseek_t
_gen_xseek(
struct fdinfo	*fio,
off_t		pos, 
int		whence,
struct ffsw	*stat)
{
#ifdef __mips
	off64_t pos64;
	_ffseek_t ret;
	pos64 = pos;
	ret = __gen_xseek(fio, pos64, whence, stat);
	return(ret);
	
#else
	return(__gen_xseek(fio, pos, whence, stat));
#endif
}

