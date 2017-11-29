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


#pragma ident "@(#) libu/ffio/gxbksp.c	92.1	06/29/99 13:16:47"
 
#include <ffio.h>
#include "gxio.h"

static int zero = 0;

/*
 * backspace a generic X class file
 * This class is intended to include the NOS/VE V format and some new formats
 * in the future.
 * Parameters:
 *	fd	- file descriptor (dummy)
 *	stat	- pointer to status return word
 *
 * Returns:
 *	0 on success sw_error is EOR, EOF, or BOD
 *	-1 on error with sw_error containing error code
 */

_gen_xbksp(struct fdinfo *fio, struct ffsw *stat)
	{
	struct fdinfo *llfio;
	ssize_t rret;
	int ret, whence, rdwlen, flag;
	ff_offt pos, cwpos;
	long ii;
	union nosve_rdw_u rdw;
	union w_205_u wcw;
	struct gen_xf *xf_info;

	xf_info = (struct gen_xf *)fio->lyr_info;

/*
 *	If the file's been writing, flush previously written data to disk and
 *	truncate.
 */
	if (fio->rwflag == WRITIN) {
		ret = XRCALL(fio, flushrtn) fio, stat);
        	if (ret < 0) return(ERR);

		ret = XRCALL(fio, weodrtn) fio, stat);
        	if (ret < 0) return(ERR);
	}
	
	llfio = fio->fioptr;
	rdwlen = xf_info->rdwlen;
	pos = xf_info->last_lrdwaddr;
/*
 *	Set up pos and rdw length
 */
	cwpos = pos;
#ifdef _OLD_F77
	if (fio->rtype == TR_UX_VAX || fio->rtype == TR_UX_SUN)
		{
		cwpos -= rdwlen >> 3;
		if (cwpos < 0) cwpos = 0;
		}
#endif
/*
 *	Seek to last RDW
 */
	whence = 0;
	if (XRCALL(llfio, seekrtn) llfio, cwpos, whence, stat) < 0)
		return(ERR);
/*
 *	Update current position
 */
	xf_info->lrdwaddr = pos;
/*
 *	read up the control info
 */
	fio->_ptr = fio->_base;
	READBLK(rret, fio, rdwlen >> 3, stat, PARTIAL, &zero);
	if (rret < 0)
		ERETURN(stat, FDC_ERR_UXEND, 0);

	switch (fio->rtype)
		{
#if !defined(_SOLARIS) && !defined(__mips) && !defined(_ABSOFT)
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
/*
 *			Update ptr to previous record
 */
			xf_info->last_lrdwaddr = rdw.wwords[1] >> 16;
			flag = rdw.wwords[0] >> 56;
			if (((rdw.wwords[1] << 48 ) >> 56 ) != 0x1e)
				ERETURN(stat, FDC_ERR_BADNVE, 0);

			fio->ateof = 0;
			if (flag == 0x04)
				fio->ateof = 1;
			break;
		case TR_205_W:
			if (pos == 0)
				{
				xf_info->last_lrdwaddr = 0;
				xf_info->lrdwaddr = 0;
				break;
				}
			GET_BITS(wcw.wword, fio->_ptr, rdwlen);
			xf_info->last_lrdwaddr = pos - wcw.fld.ps;
			abort();
			break;
#endif	/* ifndef _SOLARIS  && __mips */

#ifdef _OLD_F77
		case TR_UX_VAX:
		case TR_UX_SUN:
			if (pos == 0)
				{
				xf_info->last_lrdwaddr = 0;
				xf_info->lrdwaddr = 0;
				break;
				}
/*
 *			Read up the trailing control word from the
 *			previous record.
 */
			GET_BITS(ii, fio->_ptr, rdwlen);

			ii = (unsigned long)ii >> ((sizeof(ii) - 4 ) * 8);
			if (fio->rtype == TR_UX_VAX) SWAPB(ii);
/*
 *			Update ptr to previous record
 */
			xf_info->last_lrdwaddr = (pos - ii) - 2 * (rdwlen >> 3);
			break;
#endif
		}
/*
 *	now seek back again.
 */
	if (XRCALL(llfio, seekrtn) llfio, pos, whence, stat) < 0)
		return(ERR);

	fio->rwflag = POSITIN;
	fio->lastscc = SCCFULL;
	fio->scc = SCCFULL;
	fio->_ptr = fio->_base;
	fio->_cnt = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->segbits = 0;
	xf_info->rembits = 0;
	fio->ateod = 0;
	return(0);
	}
