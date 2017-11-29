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


#pragma ident "@(#) libu/ffio/cosread.c	92.1	06/29/99 13:16:47"

#include <string.h>
#include <ffio.h>
#include "cosio.h"

static int _cos_skipr(struct fdinfo *fio, struct ffsw *stat);
static int seteof(struct fdinfo *fio, struct ffsw *stat);
static int seteod(struct fdinfo *fio, struct ffsw *stat);

/*
 *	read Cos blocked sequential file
 *
 *	return: >= 0 for number of bytes moved minus *ubc bits.
 *		  -1 for read error and stat.sw_error contains error code.
 *
 *	while we have copied fewer than the requested number of bits{
 *
 *		if (at block boundary){
 *			get next block
 *			find the next control word
 *			move up to beginning of data
 *		}
 *		Calculate the number of bits remaining in this block
 *
 *		if no bits are remaining in the block{
 *			if the control word indicates EOF{
 *				set _IOEOF flag
 *				return 0 and FFEOF
 *			}
 *			if the control word indicates EOR{
 *				return(number of bytes copied)
 *			}
 *		}
 *
 *		copy n bits, where n = minimum of (number of bits remaining in block,
 *			number of bits left to be copied)
 *
 *		update pointers into data
 *	}
 *	if we have copied all the bits from the record
 *		set error
 *	return the number of bytes copied.
 *			
 */

ssize_t
_cos_read(
struct fdinfo *fio,
bitptr bufptr,
size_t nbytes, 
struct ffsw *stat,
int fulp, 
int *ubc)
	{
	register int	cnt,  size, flag,  ret;
	size_t		rtn;
	int64		cbn;
	int		eorstat;
	ssize_t		moved;
	register _cw_type *cwptr;
	register bitptr	ptr, base;
	register bitptr blklmt, tbp;

	register int	segbits, mbits;
	register size_t	lbits, reqbits;
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;
	
	rtn = 0;
	cbn	= cos_info->cos_cbn;	/* current block number	 */
	cnt	= cos_info->cos_cnt;
	ptr	= fio->_ptr;
	size	= cos_info->cos_size;
	base	= fio->_base;
	cwptr	= cos_info->cos_cwptr;
	blklmt	= cos_info->cos_blklmt;
	flag	= cos_info->cos_flag;

#ifdef DEBUG
	if (FDCTRACE(TRACE_COS))
		_xrt_putf("\t_cos_read: enter: cbn=%x, cwptr=%x, ptr=%x+%x, cw=%x\n",
			cbn, cwptr, BPTR2WP(ptr), BPBITOFF(ptr), *cwptr);
#endif
/*
 *	Check for read after write error
 */
	if (flag & COS_IOWRITE)
		ERETURN(stat, FDC_ERR_RAWR, 0);

	fio->rwflag = READIN;

/*
 *	If EOF was found on the previous request and we got this far again.
 *	Clear the EOF flag and try to read the next record.
 *	Also, clear the WRITE flag.
 */
 
	flag &= ~(COS_IOWRITE | COS_IOEOF);

	reqbits = (nbytes << 3) - *ubc;	/* original number of bits to move */ 
	lbits = reqbits;		/* number of bits left to move */ 
/*
 *	Clear EOR flag only if nbits to be read is non-zero.
 */
	if (reqbits != 0)
		CLEOR(flag);

	if (SUBT_BPTR(ptr, blklmt) == 0)
		{
		FILL(fio,base,cwptr,ptr,blklmt,cnt,size,cbn,stat);
		if (cnt == 0)
			goto llend;

		cbn++;
		SKIPBCW(cwptr, ptr, base, blklmt, cnt,cbn,stat);
		cos_info->cos_blklmt	= blklmt;
		}
/*
 *	Set up segbits.  It is not stored between calls.
 */
	SET_BPTR(tbp, WPTR2BP(cwptr));
	segbits = SUBT_BPTR(tbp, ptr);
	if (cwptr <  (_cw_type *)BPTR2WP(blklmt))
		segbits -= GETUBC(cwptr);
/*
 *	Did we reach the end of the record? 
 *
 *	In the case that an RCW immediately follows the BCW, and the UBC
 *	is nonzero, subtract off the length that we already delivered to
 *	the user.  This only works 'properly' in the case that the whole word
 *	up to the end of block is read.  Data past the end of record can still
 *	be read if the read is not done up to the end of the last word.
 *	Since CALL READ only deals with WORD reads, and the FDC and
 *	FORTRAN code never depends on this length being accurate
 *	when not reading full words, this is OK for now.  However,
 *	it should be fixed.
 */
	if ( segbits <= 0 ) goto hitrcw;

	while(lbits != 0)
		{

		mbits = (segbits < lbits) ? segbits : lbits;
/*
 *		Move the bits to the user area.
 */
		MOV_BITS(bufptr, ptr, mbits);

		SET_BPTR(ptr, INC_BPTR(ptr, mbits));
		SET_BPTR(bufptr, INC_BPTR(bufptr, mbits));
		cnt	-= mbits;
		lbits	-= mbits;
		segbits	-= mbits;
/*
 *		Check for control word hit.  If at block boundary,
 *		move to the next block
 */
		if (segbits == 0)
			{
			if (SUBT_BPTR(ptr, blklmt) == 0)
				{
				FILL(fio,base,cwptr,ptr,blklmt,cnt,size,cbn,stat);
				if (cnt == 0)
					goto llend;
	
				cbn++;
				SKIPBCW(cwptr, ptr, base, blklmt, cnt,cbn,stat);
				cos_info->cos_blklmt	= blklmt;
				}
	
			SET_BPTR(tbp, WPTR2BP(cwptr));
			segbits = SUBT_BPTR(tbp, ptr);
			if (cwptr <  (_cw_type *)BPTR2WP(blklmt))
				segbits -= GETUBC(cwptr);
			if ( segbits <= 0 ) goto hitrcw;
			}
		}

	rtn = nbytes;
	*ubc = (rtn << 3) - reqbits;
	rtn = (reqbits+7) >> 3;
	fio->recbits += reqbits;

	eorstat = FFCNT;

/*
 *	Check if end of record was encountered on read request.  User's
 *	status indicator needs to be set accordingly.  The low-level
 * 	blocked i/o routines require that a skip of the EOR be done.
 */
	cos_info->cos_cnt	= cnt;
	fio->_ptr		= ptr;
	cos_info->cos_cbn	= cbn;	/* current block number	 */
	cos_info->cos_size	= size;
	cos_info->cos_cwptr	= cwptr;
	cos_info->cos_flag	= flag;
 	if (fulp == FULL)
		{
		ret = _cos_skipr(fio, stat);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		if (ret <= 0) return (ret);
		}
	SETSTAT(stat, eorstat, rtn);
	return(rtn);

hitrcw:
	moved = reqbits - lbits;
	if ( GETM(cwptr) == CWEOR )
		{
		if (segbits < 0)
			{
			lbits += GETUBC(cwptr);
			moved = reqbits - lbits;
			if (moved < 0) moved = 0; /* moved too much!  Lie. */
			}
		fio->recbits += moved;
		flag |= COS_IOEOR; /* mark just after EOR */
		rtn = (moved + 7) >> 3;
		*ubc = (rtn << 3) - moved;
		eorstat = FFEOR;
		}
	else if ( GETM(cwptr) == CWEOF )
		{
/*
 *		Must keep track of PFI in case of write after read.
 */
		cos_info->cos_pfi = cbn;
		SET_BPTR(tbp, WPTR2BP(cwptr+1));
		if (SUBT_BPTR(tbp, blklmt) == 0)
			cos_info->cos_pfi = cbn + 1;
		SETEOF(flag);
		rtn = seteof(fio, stat);
		eorstat = FFEOF;
		}
	else if ( GETM(cwptr) == CWEOD)
		{
		SETEOD(flag);
		rtn = seteod(fio, stat);
		eorstat = FFEOD;
		goto done;
		}
	else
		{
		/* Expected a control word. The file must be bad.*/
		_SETERROR(stat, FDC_ERR_BADRCW, 0);
		rtn = ERR;
		goto done;
		}

	SKIPRCW(cwptr, ptr, base, cnt, blklmt, size, stat);
	SETSTAT(stat, eorstat, rtn);

done:
	cos_info->cos_cnt	= cnt;
	fio->_ptr		= ptr;
	cos_info->cos_cbn	= cbn;	/* current block number	 */
	cos_info->cos_size	= size;
	cos_info->cos_cwptr	= cwptr;
	cos_info->cos_flag	= flag;
	return(rtn);

llend:
	fio->recbits += reqbits - lbits;
	if (cbn == -1) /* EOD on first read */
		{
		SETSTAT(stat, FFEOD, 0);
		rtn = 0;
		goto done;
		}
	_SETERROR(stat, FDC_ERR_UXEND, 0);
	rtn = ERR;
	goto done;
	}

/*
 *	_cos_skipr() is used to skip to, and over, one RCW.
 *	It returns 0 for EOF or EOD, and 1 for EOR.
 */
static int
_cos_skipr(
struct fdinfo *fio,
struct ffsw *stat)
{
	register int	cnt, size, flag, rtn;
	int64		cbn;
	_cw_type	*cwptr;
	bitptr		ptr, base, blklmt, tbp;
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;

	rtn = 1;
	cbn	= cos_info->cos_cbn;	/* current block number	 */
	cnt	= cos_info->cos_cnt;
	size	= cos_info->cos_size;
	ptr	= fio->_ptr;
	base	= fio->_base;
	cwptr	= cos_info->cos_cwptr;
	blklmt	= cos_info->cos_blklmt;
	flag	= cos_info->cos_flag;

#ifdef DEBUG
	if (FDCTRACE(TRACE_COS))
		_xrt_putf("\t_cos_skipr: enter: cbn=%x, cwptr=%x, ptr=%x+%x, base=%x+%x\n",
			cbn, cwptr, BPTR2WP(ptr), BPBITOFF(ptr),
			BPTR2WP(base), BPBITOFF(base));
#endif

/*
 *	While cwptr is beyond current block skip segments
 */
	while(cwptr == (_cw_type *)BPTR2WP(blklmt)) {
		cnt -= SUBT_BPTR(blklmt, ptr);
		fio->recbits += SUBT_BPTR(blklmt, ptr);
		ptr = blklmt;
/*
 *		Move to the next block
 */
		FILL(fio, base, cwptr, ptr, blklmt, cnt, size, cbn, stat);
		if (cnt == 0) {
			if (cbn == -1) /* EOD on first read */
				{
				SETSTAT(stat, FFEOD, 0);
				rtn = 0;
			}
			else {
				_SETERROR(stat, FDC_ERR_UXEND, 0);
				rtn = ERR;
			}
			goto srdone;
		}
		cbn++;
		SKIPBCW(cwptr, ptr, base, blklmt, cnt, cbn, stat);
		cos_info->cos_blklmt = blklmt;
	}
	if ( GETM(cwptr) == CWEOR) {
		fio->recbits += SUBT_BPTR(WPTR2BP(cwptr), ptr) - GETUBC(cwptr);
		SKIPRCW(cwptr, ptr, base, cnt, blklmt, size, stat);
		flag |= COS_IOEOR; /* say we're just after EOR */

srdone:
		cos_info->cos_cnt	= cnt;
		cos_info->cos_size	= size;
		fio->_ptr		= ptr;
		cos_info->cos_cbn	= cbn;	/* current block number	 */
		cos_info->cos_cwptr	= cwptr;
		cos_info->cos_flag	= flag;

		return(rtn);
	}
	if(GETM(cwptr) == CWEOF) {		/* crossed EOF */
/*
 *		Must keep track of PFI in case of write after read.
 */
		cos_info->cos_pfi = cbn;
		SET_BPTR(tbp, WPTR2BP(cwptr+1));
		if (SUBT_BPTR(tbp, blklmt) == 0)
			cos_info->cos_pfi = cbn + 1;
		SKIPRCW(cwptr, ptr, base, cnt, blklmt, size, stat);
		SETEOF(flag);
		SETSTAT(stat, FFEOF, 0);
		rtn = 0;
		goto srdone;
	}
	if ( GETM(cwptr) == CWEOD) {
		SETEOF(flag); /* This must be set too! */
		SETEOD(flag);
		SETSTAT(stat, FFEOD, 0);
		rtn = 0;
		goto srdone;
	}
	/* Expected a control word. The file must be bad.*/
	_SETERROR(stat, FDC_ERR_BADRCW, 0);
	rtn = ERR;
	goto srdone;
}

static int
seteod(
struct fdinfo *fio,
struct ffsw *stat)
	{
	fio->ateod = YES;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	SETSTAT(stat, FFEOD, 0)
	return(0);
	}

static int
seteof(
struct fdinfo *fio,
struct ffsw *stat)
	{
	fio->ateof = YES;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	SETSTAT(stat, FFEOF, 0)
	return(0);
	}
