/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/gvread.c	92.2	06/29/99 13:16:47"

#include <ffio.h>
#include <string.h>
#include "gvio.h"

static int scc_vms_convert[4] = {SCCMIDL, SCCFRST, SCCLAST, SCCFULL};
static int scc_nve_convert[4] = {SCCFULL, SCCFRST, SCCMIDL, SCCLAST};

#define	CNTLZ	0x1A	/* Control-Z */

static int _gen_baddata(struct fdinfo *fio, struct ffsw *stat);
static int get_block(struct fdinfo *fio, struct ffsw *stat);
static int get_segment(struct fdinfo *fio, struct ffsw *stat);
static int seteof(struct fdinfo *fio, struct ffsw *stat);
static int skip2bor(struct fdinfo *fio, struct ffsw *stat);
static int skip2eor(struct fdinfo *fio, struct ffsw *stat);

/*
 *	Read a 4-digit ASCII number used in headers.
 */

static int
_DTB(char *p, int *err)
{
	register short	i;
	register int	value;
	register char	ch;

	*err	= 0;
	value	= 0;

	for (i = 0; i < 4; i++) {
		ch	= *(p + i);

		if (ch < '0' || ch > '9')
			*err	= 1;

		value	= (value * 10) + ((int) ch - (int) '0');
	}

	return(value);
}

static const uint32 PAD_PATTERN = 0x5e5e5e5e;

/*
 * Read a generic V class file
 * Parameters:
 *	fio	- fdinfo pointer
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 *	ubc	- pointer to unused bit count (not used this class of file)
 */
_gen_vread(struct fdinfo *fio, bitptr bufptr, int nbytes,struct ffsw *stat,
int fulp, int *ubc)
	{
	int bits, moved, nbits, ret, eorstat;

	nbits = nbytes << 3;
	if (*ubc != 0)
		ERETURN(stat, FDC_ERR_UBC, 0);

	moved = 0;
	if (fio->rwflag == WRITIN)
		/* read after write error */
		ERETURN(stat, FDC_ERR_RAWR, 0);

	fio->rwflag = READIN;
/*
 *	If previous segment was empty, get new segment
 */
	if (fio->segbits == 0)
		{
		ret = get_segment(fio, stat);
		if (ret <= 0)
			{
			if (ret < 0 && stat->sw_error == FETAPUTE)
				{
				(void) _gen_baddata(fio, stat);
				SETSTAT(stat, stat->sw_stat, moved >> 3);
				}	
			return(ret);	/* EOF/EOD or error */
			}
		}
/*
 *	loop getting segments and moving data
 *	Note that one pass through this loop is enforced on non-spanned formats
 */
	eorstat = FFCNT;
	while (nbits > 0)
		{
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
 *		Note that if segbits != 0, then nbits also must be != 0
 */
		if (fio->segbits == 0)
			{
			if (fio->scc == SCCLAST || fio->scc == SCCFULL)
				{
				eorstat = FFEOR;
				nbits = 0; /* exit loop */
				}
			else
				{
				ret = get_segment(fio, stat);
				if (ret <= 0)
					{
					if (ret < 0 && stat->sw_error == FETAPUTE)
					{
						(void) _gen_baddata(fio, stat);
			   			SETSTAT(stat, stat->sw_stat, moved >> 3);
					}	
					return(ret);	/* EOF/EOD or error */
					}
				}
			}
		} /* end while */
/*
 *	Check for zero length segment at beginning of buffer
 */
	if (fio->segbits == 0 && (fio->scc == SCCLAST || fio->scc == SCCFULL))
		eorstat = FFEOR;

	fio->recbits += moved;

	if (fulp)
		{
		ret = skip2eor(fio, stat);
		if (ret <= 0)
			{
			if (ret < 0 && stat->sw_error == FETAPUTE)
				{
					(void) _gen_baddata(fio, stat);
					SETSTAT(stat, stat->sw_stat, moved >> 3);
				}	
			return(ret);	/* EOF/EOD or error */
			}
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}
			
	SETSTAT(stat, eorstat, moved >> 3);
	return (moved >> 3);
	}

/*
 * Fetch a segment from the data stream.
 *	returns:
 *		>0 got it!
 *		=0 EOF or EOD
 *		<0 some error.
 */
static int
get_segment(struct fdinfo *fio, struct ffsw *stat)
	{
	union sdw_u	sdw;
	int		i, sdwlen, scc, scci, err;
	uint64		tword;
	struct gen_vf	*vf_info;

	vf_info = (struct gen_vf *)fio->lyr_info;

again:
	if (fio->_cnt == 0)
		{
		i = get_block(fio,stat);
		if (i <= 0)
			return(i); /* stat is already set.. */
		}
	sdwlen = vf_info->sdwlen;
	if (sdwlen > 0)
		{
		GET_BITS(sdw.wword, fio->_ptr, sdwlen);
		SKIP_BITS(fio,sdwlen);
		}
	fio->lastscc = fio->scc;
	switch(fio->rtype)
		{
		case TR_IBM_U:
		case TR_VMS_V_DSK:
			fio->segbits = fio->_cnt;
			scc = SCCFULL;
			break;
		case TR_IBM_V:
		case TR_IBM_VB:
		case TR_IBM_VBS:
			/* generic SCC codes same as IBM */
			fio->segbits = (sdw.fld.ll << 3) - sdwlen;
			scc = sdw.fld.scc;
			break;
		case TR_VMS_S_DSK:
			fio->segbits = fio->_cnt;
			scci = (sdw.wword >> (64-8)) & 0xFF;
			if (scci == CNTLZ) /* control-Z */
				{
				fio->scc = SCCFULL;
				return(seteof(fio, stat));
				}
			scc = scc_vms_convert[scci];
			break;
		case TR_VMS_V_TP:
		case TR_NVE_D:
/*
 *			Check for padd and go get new block if found
 */
			tword = sdw.wword & 0xFFFFFFFF00000000LL;
			if ((tword >> 32) == PAD_PATTERN)
				{
				/* should check ALL of the bytes here... */
				fio->_cnt = 0;	/* skip to end of block */
				goto again;
				}
			fio->segbits = (_DTB((char *) &tword, &err) - 4) << 3;
			if (err != 0)
				ERETURN(stat, FDC_ERR_FMT, 0);

			scc = SCCFULL;
			break;
		case TR_VMS_S_TP:
/*
 *			Check for padd and go get new block if found
 */
			tword = sdw.wword & 0xFFFFFFFF00000000LL;
			if ((tword >> 32) == PAD_PATTERN)
				{
				/* should check ALL of the bytes here... */
				fio->_cnt = 0;	/* skip to end of block */
				goto again;
				}
			fio->segbits = (_DTB((char *) &tword, &err) - 6) << 3;
			if (err != 0)
				ERETURN(stat, FDC_ERR_FMT, 0);

			scci = (sdw.wword >> (64-32-8)) & 0xFF;
			if (scci == CNTLZ) /* control-Z */
				{
				fio->scc = SCCFULL;
				return(seteof(fio, stat));
				}
			scc = scc_vms_convert[scci];
			break;
		case TR_NVE_S:
/*
 *			Check for padd and go get new block if found
 */
			tword = (sdw.wword & 0x00FFFFFFFF000000LL) << 8;
			if (((tword >> 24) & 0xFFFFFFFF) == PAD_PATTERN)
				{
				/* should check ALL of the bytes here... */
				fio->_cnt = 0;	/* skip to end of block */
				goto again;
				}
			fio->segbits = (_DTB((char *) &tword, &err) - 5) << 3;
			if (err != 0)
				ERETURN(stat, FDC_ERR_FMT, 0);

			scci = (sdw.wword >> (64-8)) & 0xFF;
			if (scci == CNTLZ) /* control-Z */
				{
				fio->scc = SCCFULL;
				return(seteof(fio, stat));
				}
			scc = scc_nve_convert[scci-0x30];
			break;
		case TR_VMS_V_TR:
			/* swap bytes, convert to bits */
			fio->segbits = (((sdw.wword >> 40) & 0xFF00) |
					 (sdw.wword >> 56)) << 3;
			if (fio->segbits > 0x3FFF8)	/* 0xFFFF << 3 */
				ERETURN(stat, FDC_ERR_FMT, 0);

			scc = SCCFULL;
			break;
		case TR_VMS_S_TR:
			/* swap bytes, subtract SCC length, convert to bits */
			fio->segbits = ((((sdw.wword >> 40) & 0xFF00) |
					  (sdw.wword >> 56)) - 2) << 3;
			scci = sdw.wword >> (64-16-8) & 0xFF;
			if (scci == CNTLZ) /* control-Z */
				{
				fio->scc = SCCFULL;
				return(seteof(fio, stat));
				}
			scc = scc_vms_convert[scci];
			break;
		}
	fio->scc = scc;
	if (fio->scc == SCCLAST || fio->scc == SCCMIDL)
		{
		if (fio->lastscc == SCCLAST || fio->lastscc == SCCFULL)
			ERETURN(stat, FDC_ERR_SCC, 0);	/* BAD SCC */
		}
	else if (fio->scc == SCCFRST || fio->scc == SCCFULL)
		{
		if (fio->lastscc == SCCFRST || fio->lastscc == SCCMIDL)
			ERETURN(stat, FDC_ERR_SCC, 0);	/* BAD SCC */
		}
	else
		ERETURN(stat, FDC_ERR_SCC, 0);	/* BAD SCC */
	return(1);
	}

/*
 *	Common code to handle EOF return
 */
static int
seteof(struct fdinfo *fio, struct ffsw *stat)
	{
	fio->segbits = 0;	/* no data here.. */
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
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
	fio->segbits = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->_cnt = 0;
	fio->ateof = 0;
	fio->ateod = 1;
	SETSTAT(stat, FFEOD, 0)
	return(0);
	}
/*
 * Read a block of data as defined by the next 32 bits.  This assumes
 * that the next 32 bits ARE a BDW.
 */
static int
get_block(struct fdinfo *fio, struct ffsw *stat)
	{
	int		bdwlen, zero, req, ret, padd;
	uint64		blklen;
	union bdw_u	bdw;
	struct gen_vf	*vf_info;

	vf_info = (struct gen_vf *)fio->lyr_info;

	if (fio->ateod != 0)
		return(seteod(fio,stat));
/*
 *	get block 'reads' the next block from the file.  It
 *	uses the BDW, if there is one, otherwise it just reads.
 *	If there is a BDW,  it effectively strips it off.
 *	IBM is the only format currently supported that has a true BDW
 */
	zero = 0;
	fio->_ptr = fio->_base;
	bdwlen = vf_info->bdwlen;
	switch(fio->rtype)
		{
		case TR_IBM_V:
		case TR_IBM_VB:
		case TR_IBM_VBS:
			READBLK(ret, fio, bdwlen >> 3, stat, PARTIAL, &zero);
			if (ret < 4)
				{
				if (ret < 0)
					return(ret);
				if (fio->_cnt == 0)
					goto got_end;
				ERETURN(stat, FDC_ERR_FMT, 0);
				}
/*
 *			Pick out the block length from the BDW. We
 *			know that the buffer is word aligned !!
 */
			bdw.wword = *BPTR2WP(fio->_base);
			blklen = bdw.fld.ll;
/*
 *			There better not be an EOR after the BDW
 */
			if (FFSTAT(*stat) != FFCNT)
				ERETURN(stat, FDC_ERR_FMT, 0);

			SANITYCHK(blklen << 3,GT,fio->_ffbufsiz)
			zero = 0;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, bdwlen));
			req = blklen - (bdwlen >> 3);
			if (req < 0) ERETURN(stat, FDC_ERR_FMT, 0);
			READBLK(ret, fio, req, stat, FULL, &zero);
			fio->segbits = 0;
			if (ret < 0)
				return(ERR);
			if (ret == 0 || ret < req)
				ERETURN(stat, FDC_ERR_UXEND, 0);
			break;
/*
 *		For vms transparent type, the sdw doubles as a bdw.
 *		For transparent, it should be reliable, and there should be no
 *		requirement for underlying blocking.  The 'BDW'  is merely
 *		examined and used, NOT stripped off.  Hence, is is re-used,
 *		and stripped when it is processed as an SDW.
 */
		case TR_VMS_V_TR:
		case TR_VMS_S_TR:
/*
 *			Pretend that the SDW is a BDW, just for now.
 */
			bdwlen = 16;
			READBLK(ret, fio, 2, stat, PARTIAL, &zero);
			if (ret < 2)
				{
				if (ret < 0)
					return(ret);
				if (fio->_cnt == 0)
					goto got_end;
				ERETURN(stat, FDC_ERR_UXEND, 0);
				}
			/* load up first word of buffer */
			blklen = *BPTR2WP(fio->_base);
			/* shift down and swap bytes */
			blklen = (blklen >> 56) | ((blklen >> 40) & 0xFF00);
			if (blklen == 0xFFFF)
				return(seteod(fio,stat));
			if (blklen > 32767)
				ERETURN(stat, FDC_ERR_FMT, 0)
			padd = 0;
			if ((blklen & 1) != 0) /* if odd length */
				{
				blklen += 1;
				padd = 8;
				}
			/* read in after SDW */
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, bdwlen));
			READBLK(ret, fio, blklen, stat, PARTIAL, &zero);
			fio->segbits = 0;	/* pretend sdw never read */
			if (ret < 0)
				return(ERR);
			if (ret == 0 || ret < blklen)
				ERETURN(stat, FDC_ERR_UXEND, 0);
			fio->_ptr = fio->_base;
			fio->_cnt += bdwlen;	/* add SDW back in */
			fio->_cnt -= padd;	/* subtract off padd */
			break;
/*
 *		These types have no bdw. Must rely on count, and full mode
 *		If there is no underlying blocking, or device that is
 *		'record mode', then this is detected by reading over
 *		maxblksize.  This is an error.  They also have a rep. for EOF.
 *		A lower-level EOF gets translated to an EOD.
 */
		case TR_VMS_S_DSK:
		case TR_VMS_S_TP:
		case TR_NVE_S:
			{
			int num2read;

			num2read = fio->_ffbufsiz >> 3;
			fio->segbits = 0;
			READBLK(ret, fio, num2read, stat, FULL, &zero);
			if (ret < 0)
				return(ERR);
			if (fio->_cnt == 0)
				{
				switch(FFSTAT(*stat))
					{
					case FFEOR:
						break;
					case FFEOF:
					case FFEOD:
						return(seteod(fio,stat));
					case FFCNT:
					default:
						ERETURN(stat, FDC_ERR_FMT, 0)
					}
				}
			if (stat->sw_stat != FFEOR)
				ERETURN(stat, FDC_ERR_MXBLK, 0);
			break;
			}
/*
 *		These types have no bdw. Must rely on count, and full mode
 *		If there is no underlying blocking, or device that is
 *		'record mode', then this is detected by reading over
 *		maxblksize.  This is an error.
 *		They also have no rep. for EOF, lower level layer must
 *		provide.
 */
		case TR_IBM_U:
		case TR_VMS_V_DSK:
		case TR_VMS_V_TP:
		case TR_NVE_D:
			{
			int num2read;

			num2read = fio->_ffbufsiz >> 3;
			fio->segbits = 0;
			READBLK(ret, fio, num2read, stat, FULL, &zero);
			if (ret < 0)
				return(ERR);
			if (fio->_cnt == 0)
				{
				if (FFSTAT(*stat) != FFEOR)
					goto got_end;
				}
			if (stat->sw_stat != FFEOR)
				ERETURN(stat, FDC_ERR_MXBLK, 0);
			break;
			}
		} /* end case */
	return(1);

got_end:
	if (FFSTAT(*stat) == FFEOF)
		return(seteof(fio, stat));
	else if (FFSTAT(*stat) == FFEOD)
		return(seteod(fio, stat));
	ERETURN(stat, FDC_ERR_FMT, 0)
	
	}

/*
 * Skip2eor skips ahead to the end of the record, reading up blocks and
 * records as needed until the SCC says FFEOR, or hit EOD.
 */
static int
skip2eor(struct fdinfo *fio, struct ffsw *stat)
	{
	int ret;

	if (fio->segbits > 0)
		{
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		fio->segbits = 0;
		}
	while(fio->scc != SCCLAST && fio->scc != SCCFULL)
		{
		ret = get_segment(fio, stat);
		if (ret <= 0)
			return(ret);	/* EOF/EOD or error */
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		fio->segbits = 0;
		}
	return(1);
	}

/*
 * gen_vseek()
 * Allow the user to rewind his foreign file.
 */
_gen_vseek(struct fdinfo *fio, int pos, int whence, struct ffsw *stat)
	{
	struct fdinfo *llfio;
	int ret;

/*
 * For now, only allow rewind to zero
 */
	if (pos != 0 || whence != 0)
		ERETURN(stat, FDC_ERR_NOSUP, 0);

	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret < 0) return(ERR);
/*
 *	Do special V class things
 */
	fio->scc = SCCFULL;
	fio->lastscc = SCCFULL;

	llfio = fio->fioptr;
	ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
	fio->rwflag = POSITIN;
	fio->ateof = 0;
	fio->ateod = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return (ret);
	}

/*
 * Skips to the end of the record. If an error is encountered,
 * "stat" will contain the error number.
 */
static int
_gen_baddata(struct fdinfo *fio, struct ffsw *stat)
	{
	struct gen_vf *vf_info;
	struct ffsw locstat;

	vf_info = (struct gen_vf *)fio->lyr_info;
	if (vf_info->skipbad != 0)
		{
		fio->segbits = 0;

		switch(fio->rtype)
			{
			case TR_IBM_U:
			case TR_IBM_V:
			case TR_IBM_VB:
				fio->scc = SCCFULL;
				break;
			case TR_IBM_VBS:
				/* We don't know where we are in the record */
				fio->scc = SCCUNKN;
				break;
			default:
				ERETURN(stat, FDC_ERR_INTERR,0);
			}
		if (skip2bor(fio, &locstat) < 0)
			{
			ERETURN(stat, locstat.sw_error, 0);
			}
		fio->recbits = 0;
		/* We don't have an accurate count of how many bits were */
		/* in the last record, since it had bad data */
		/* Fortunately, this field doesn't appear to be used. */
		fio->last_recbits = -1;
		}
	return(0);
	}
/*
 * Skip2bor skips ahead to the end of the record, or the beginning of a record,
 * reading up blocks and records as needed until the SCC says
 * FFEOR, or beginning of record, or hit EOD.
 */
static int
skip2bor(struct fdinfo *fio, struct ffsw *stat)
	{
	int ret;

	if (fio->segbits > 0)
		{
		ERETURN(stat,FDC_ERR_INTERR,0);
		}
	while(fio->scc != SCCLAST && fio->scc != SCCFULL)
		{
		ret = get_segment(fio, stat);
		if (ret <= 0)
			return(ret);	/* EOF/EOD or error */
		if (fio->scc == SCCFULL || fio->scc == SCCFRST)
			return(1);
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		fio->segbits = 0;
		}
	return(1);
	}
