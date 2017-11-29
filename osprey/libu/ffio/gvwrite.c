/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/ffio/gvwrite.c	92.2	06/29/99 13:16:47"

#include <errno.h>
#include <ffio.h>
#include <string.h>
#include "gvio.h"

#ifdef KEY
static put_block( struct fdinfo *, struct ffsw *, long );
#endif

static const uint32 carets	= 0x5e5e5e5e; /* '^^^^' */
static const uint64 ctrlz	= 0x1A;
static const uint64 neg1	= 0xffffffffffffffffLL;
static const uint64 seg5	= 0x3030303500000000LL; /* '0005    ' */
static const uint64 soh		= 0x0100;
static int __zero		= 0;

static void init_block();
static int init_seg(struct fdinfo *fio, struct ffsw *stat);
static int put_segment(struct fdinfo *fio, struct ffsw *stat, int fulp);

/*
 *	Generate 4-digit ASCII numbers for use in headers.
 */

static void
_BTD(int value, char *p)
{
	register short	i;

	for (i = 3; i >= 0; i--) {

		*(p+i)	= (char) ((value % 10) + (int) '0');
		value	= value / 10;

	}

	return;
}

/*
 * Write a V class file
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count (not used for IBM)
 */
_gen_vwrite(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	int	ret;
	long	bits, moved, nbits, left;
	struct gen_vf *vf_info;

	nbits = nbytes << 3;
	if (*ubc != 0)
		ERETURN(stat, FDC_ERR_UBC, 0);

	vf_info = (struct gen_vf *)fio->lyr_info;
	if (fio->ateod != 0)
		ERETURN(stat, FDC_ERR_WPEOD, 0);
/*
 *	If NOT writing or just positioned, then error.
 *	Something should be done here to handle this!
 */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);
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
	SANITYCHK (fio->maxblksize,LE,0);

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
		if (fio->segbits == 0)
			{
			if (init_seg(fio, stat) != 0)
				return(ERR);
			}
		left = fio->maxblksize - fio->_cnt;
/*
 *		If enough room for bits, put them in the buffer
 */
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
 *			Handle this case for each of the formats
 */
			switch(fio->rtype)
				{
				case TR_IBM_U:
				case TR_IBM_V:
				case TR_VMS_V_DSK:
				case TR_VMS_V_TR:
					ERETURN(stat, FDC_ERR_INTERR,0);
				case TR_IBM_VB:
				case TR_VMS_V_TP:
				case TR_NVE_D:
					{
					bitptr movefrm, moveto;
					int tempseg;

					/* check above assures segbits > 0 */
					movefrm = vf_info->sdwaddr;
					tempseg = fio->segbits;
/*
 *					Write out block minus partial rec
 */
					if (put_block(fio, stat,
						fio->_cnt - fio->segbits) != 0)
							return(ERR);
/*
 *					move partial record to start of
 *					buffer + BDW
 */
					init_block(fio, stat);
					moveto = fio->_ptr;
					PUTDATA(movefrm, fio, tempseg);
/*
 *					SDW is now moved, update pointer
 */
					vf_info->sdwaddr = moveto;
					break;
					}
				case TR_IBM_VBS:
				case TR_VMS_S_DSK:
				case TR_VMS_S_TP:
				case TR_NVE_S:
				case TR_VMS_S_TR:
					if (left == 0)
						{
						ret = put_segment(fio, stat,
							PARTIAL);
						if (ret != 0)
							return(ERR);
						if (put_block(fio, stat,
								fio->_cnt) != 0)
							return(ERR);
						}
					else
						{
						bits = nbits;
						if (nbits > left)
							bits = left;
						PUTDATA(bufptr, fio, bits);
						SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
						}
					break;
				}
			}
		nbits -= bits;
		moved += bits;
		}

	if (fulp == FULL)
		{
/*
 *		Watch out for NULL writes!
 */
		if (fio->segbits == 0)
			if (init_seg(fio, stat) != 0)
				return(ERR);
		ret = put_segment(fio, stat, fulp); /* this will be FULL */
		if (ret != 0)
			return(ERR);
/*
 *		Some formats flush the block on every segment, others don't.
 *		Note that for those V formats that do, the MBS field
 *		is meaningless.  For S formats, it is not meaningless, but
 *		only defines a maximum segment size.
 */
		switch(fio->rtype)
			{
			case TR_IBM_U:
			case TR_IBM_V:
			case TR_VMS_V_DSK:
			case TR_VMS_V_TR:		/* no need full */
			case TR_VMS_S_DSK:		/* nxt layer defines */
							/* block bdry */
			case TR_VMS_S_TR:
				if (put_block(fio, stat, fio->_cnt) != 0)
					return(ERR);
				break;
			case TR_IBM_VB:
			case TR_IBM_VBS:
			case TR_VMS_V_TP:
			case TR_NVE_D:
			case TR_VMS_S_TP:
			case TR_NVE_S:
				break;
			}
		SETSTAT(stat, FFEOR, moved >> 3);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}
	else
		{
		SETSTAT(stat, FFCNT, moved >> 3);
		fio->recbits += moved;
		}
			
	return (moved >> 3);
	}

/*
 *	init_seg checks if there is room for SDW.  If not, flush block
 *	Also, if no bits are yet in block, and there is data 
 *	to write, set aside room for the BDW. 
 */
static int
init_seg(struct fdinfo *fio, struct ffsw *stat)
	{
	int sdwlen;
	struct gen_vf *vf_info;

	vf_info = (struct gen_vf *)fio->lyr_info;

	sdwlen = vf_info->sdwlen;
/*
 *	Init new block if full, or if brand new.
 */
	if ((fio->_cnt + sdwlen) >= fio->maxblksize)
		{
		if (put_block(fio, stat, fio->_cnt) != 0)
			return(ERR);
		}
	if (fio->_cnt == 0)
		init_block(fio, stat);
	/* leave room for SDW */
	vf_info->sdwaddr = fio->_ptr;
	SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, sdwlen));
	fio->_cnt += sdwlen;
	fio->segbits += sdwlen;
	return(0);
	}

/*
 * Initialize a new block.  Leave room for BDW and save BDW address
 */
static void
init_block(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int bdwlen;
	struct gen_vf *vf_info;

	vf_info = (struct gen_vf *)fio->lyr_info;

	bdwlen = vf_info->bdwlen;

	if (bdwlen != 0)
		{
		vf_info->bdwaddr = fio->_ptr;
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, bdwlen));
		fio->_cnt += bdwlen;
		}
	return;
	}
/*
 * Terminate a data segment by filling in the SDW and starting a new segment
 */
static int
put_segment(struct fdinfo *fio, struct ffsw *stat, int fulp)
	{
	union sdw_u sdw;
	bitptr psdw;
	int scc;
	struct gen_vf *vf_info;

	vf_info = (struct gen_vf *)fio->lyr_info;

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
 *	Set pointer to place where sdw will go, build the SDW, and
 *	poke it into place.
 */
	if (vf_info->sdwlen > 0)
		{
		sdw.wword = 0;
		switch(vf_info->sdwtype)
			{
			case NONE:
				break;
			case SDW_IBM:
				sdw.fld.ll = fio->segbits >> 3;
				break;
			case SDW_D:
				{
				int	temp = fio->segbits >> 3;

				sdw.wword = 0;
				_BTD(temp, (char *) &sdw);
				break;
				}
			case SDW_V:
/*
 *				build swapped bytes. vms V SDW does NOT
 *				include length of count field
 */

				sdw.wword = (fio->segbits >> 3) - 2;
				sdw.wword = (sdw.wword >> 8) |
					   ((sdw.wword << 8) & 0xFF00);
				sdw.wword = sdw.wword << (64-16);
				break;
			case SDW_NVE_S:
				{
				int	temp = fio->segbits >> 3;

				sdw.wword = 0;
				_BTD(temp, (char *) &sdw);
				sdw.wword = sdw.wword >> 8;
				break;
				}
			}
/*
 *		Set the SCC into the reserved space in the SDW
 */
		if (vf_info->scclen != 0)
			{
			register unsigned long	temp;
	
			SET_BPTR(psdw, WPTR2BP((long *) &sdw));
			temp = _scc_tab [vf_info->scctype] [scc];
			/* left justify the scc using the stated length. */
#ifdef	_WORD32
			temp = temp << (32 - vf_info->scclen);
#else
			temp = temp << (64 - vf_info->scclen);
#endif
			SET_BPTR(psdw, INC_BPTR(psdw,vf_info->sccpos));
			PUT_BITS(psdw, temp, vf_info->scclen);
			}
/*
 *		Now put the SDW in the buffer.
 */
		psdw = vf_info->sdwaddr;

#ifdef	_WORD32
		PUT_BITS(psdw, (long)(sdw.wword >> 32), vf_info->sdwlen);
#else
		PUT_BITS(psdw, sdw.wword, vf_info->sdwlen);
#endif
		}
	fio->lastscc = scc;

	fio->segbits = 0;
	return(0);
	}

/*
 * Write a block out.  Fill in the BDW and do the I/O.
 */
static
put_block(fio, stat, bits)
struct fdinfo *fio;
struct ffsw *stat;
long bits;
	{
	int ret, bdwlen, mode;
	union bdw_u bdw;
	bitptr pbdw;
	struct gen_vf *vf_info;

	vf_info = (struct gen_vf *)fio->lyr_info;

/*
 *	Set pointer to place where bdw will go, build the BDW, and
 *	poke it into place.
 */
	bdwlen = vf_info->bdwlen;
	if (bdwlen != 0)
		{
		bdw.wword = 0;
		pbdw = vf_info->bdwaddr;
		switch(vf_info->bdwtype)
			{
			case BDW_IBM:
				bdw.fld.ll = bits >> 3;
				bdw.fld.zero = 0;
#ifdef	_WORD32
				PUT_BITS(pbdw, (long)(bdw.wword >> 32), bdwlen);
#else
				PUT_BITS(pbdw, bdw.wword, bdwlen);
#endif
				break;
			}
		}
/*
 *	Some formats require that blocks be padded out to
 *	multiples of words.  Ensure that this is done.
 */
	switch(fio->rtype)
		{
				case TR_IBM_U:
				case TR_IBM_V:
				case TR_IBM_VB:
				case TR_IBM_VBS:
				case TR_VMS_V_DSK:
				case TR_VMS_S_DSK:
					mode = FULL;
					break;
				case TR_VMS_V_TP:
				case TR_NVE_D:
				case TR_VMS_S_TP:
				case TR_NVE_S:
/*
 *					Padding should be done here, but is not
 *					required.
 */
					mode = FULL;
					break;
				case TR_VMS_V_TR:
				case TR_VMS_S_TR:
					if (((bits >> 3) & 1) != 0)
						{
/*
 *						Must padd out to even word
 *						boundary.  WARNING:  This byte
 *						may exceed the buffer limit
 *						by one byte!  This is OK, as
 *						a few extra bytes are alloc'd.
 *						However, one must beware the
 *						debug code that checks.  We
 *						get away with this only because
 *						WRITEBLK does no checking.
 */
						PUT_BITS(fio->_ptr, carets, 4);
						PUT_BITS(fio->_ptr, carets, 4);
						bits += 8;
						}
					mode = PARTIAL;
					break;
		}
/*
 *	Write it out.
 */
	WRITEBLK(ret, fio, bits >> 3, stat, mode, &__zero);
	if (ret < 0)
		return(ret);
	return(0);
	}

/*
 * Flush the buffer and clean up
 *	This routine should return 0, or -1 on error.
 */
int
_gen_vflush(fio, stat)
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
		fio->scc = FULL;
		fio->lastscc = FULL;
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
		if (put_block(fio, stat, fio->_cnt) != 0)
			return(ERR);
	return(0);
	}

/*
 * Write an EOF.
 *	Place an EOF mark in the current layer, if defined.  If not
 *	defined, put the EOF in the lower level layer.  If *it* can't
 *	represent it, then we get an error.
 */
int
_gen_vweof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	uint64	lword;
	int	ret, bits, mode;
	bitptr	pword;
	struct fdinfo *llfio;

	/* !! Write after Read!! */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);
	if (fio->ateod != 0)
		ERETURN(stat, FDC_ERR_WPEOD, 0);
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
/*
 *	bits represents the quantity of data used to represent the EOF as data.
 *	If it ends up zero, then lower layers will have to handle the EOF.
 */
	bits = 0;
	switch(fio->rtype)
		{
		case TR_IBM_U:
		case TR_IBM_V:
		case TR_IBM_VB:
		case TR_IBM_VBS:
		case TR_VMS_V_DSK:
			mode = FULL;
			break;
		case TR_VMS_S_DSK:
			lword = (ctrlz << 56);
			bits = 16;	/* Two bytes */
			mode = FULL;
			break;
		case TR_VMS_V_TP:
		case TR_NVE_D:
/*
 *			Padding should be done here, but is not
 *			required.
 */
			mode = FULL;
			break;
		case TR_VMS_S_TP:
/*
 *			Put out a one byte segment w/ SCC=0x1A
 *			segment length is 5, count is included.
 */
			lword = seg5 | (ctrlz << 24);
			bits = 40;	/* Five bytes */
/*
 *			Padding should be done here, but is not
 *			required.
 */
			mode = FULL;
			break;
		case TR_NVE_S:
/*
 *			Put out a one byte seg w/ SCC=0x1A
 */
			lword = (ctrlz << 56) | (seg5 >> 8);
			bits = 40;	/* Five bytes */
/*
 *			Padding should be done here, but is not
 *			required.
 */
			mode = FULL;
			break;
		case TR_VMS_V_TR:
			ERETURN(stat, FDC_ERR_NWEOF, 0);

		case TR_VMS_S_TR:
/*
 *			Put out a one byte seg w/ SCC=0x1A
 */
			lword = (soh << 48) | (ctrlz << 40);
			bits = 32;	/* Four bytes */
			mode = PARTIAL;
			break;
		}
/*
 *	If the format represents the EOF as data, write it out
 */
	if (bits > 0)
		{
		if ((fio->_cnt + bits) >= fio->maxblksize)
			{
			if (put_block(fio, stat, fio->_cnt) != 0)
				return(ERR);
			}
		SET_BPTR(pword, WPTR2BP(&lword));
		PUTDATA(pword, fio, bits);
		WRITEBLK(ret, fio, fio->_cnt >> 3, stat, mode, &__zero);
		if (ret < 0) return(ret);
		}
	else
		{
		ret = XRCALL(fio, flushrtn) fio, stat);
		if (ret < 0) return(ret);
		llfio = fio->fioptr;
		ret = XRCALL(llfio, weofrtn) llfio, stat);
		if (ret < 0) return(ret);
		}

	fio->ateof = 1;
	fio->ateod = 0;
	fio->recbits = 0;
	return(0);
	}

/*
 * Write an EOD.
 *	Truncate the file.  This is the 'real' end of the data.
 */
int
_gen_vweod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	uint64	lword;
	int	ret, bits, mode;
	bitptr	pword;
	struct fdinfo *llfio;

	/* !! Write after Read!! */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);
/*
 *	If already at EOD, all done.  Don't try to do it again.
 */
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

	bits = 0;
	switch(fio->rtype)
		{
		case TR_IBM_U:
		case TR_IBM_V:
		case TR_IBM_VB:
		case TR_IBM_VBS:
		case TR_VMS_V_DSK:
		case TR_VMS_S_DSK:
			mode = FULL;
			break;
		case TR_VMS_V_TP:
		case TR_NVE_D:
/*
 *			Padding should be done here, but is not
 *			required.
 */
			mode = FULL;
			break;
		case TR_VMS_S_TP:
		case TR_NVE_S:
/*
 *			Padding should be done here, but is not
 *			required.
 */
			mode = FULL;
			break;
		case TR_VMS_V_TR:
		case TR_VMS_S_TR:
			lword = neg1;
			bits = 16;	/* Two bytes */
			mode = FULL;
			break;
		}
/*
 *	If the format represents the EOD as data, write it out
 *	If not, let the next lower layer do something.
 */
	if (bits > 0)
		{
		if ((fio->_cnt + bits) >= fio->maxblksize)
			{
			if (put_block(fio, stat, fio->_cnt) != 0)
				return(ERR);
			}
		SET_BPTR(pword, WPTR2BP(&lword));
		PUTDATA(pword, fio, bits);
		WRITEBLK(ret, fio, fio->_cnt >> 3, stat, mode, &__zero);
		if (ret < 0) return(ret);
		}
	else
		{
		ret = XRCALL(fio, flushrtn) fio, stat);
		if (ret < 0) return(ret);
		}
	llfio = fio->fioptr;
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0) return(ret);

	fio->ateof = 0;
	fio->ateod = 1;
	fio->recbits = 0;
	return(0);
	}
