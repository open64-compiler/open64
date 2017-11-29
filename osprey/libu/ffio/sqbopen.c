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


#pragma ident "@(#) libu/ffio/sqbopen.c	92.2	10/14/99 15:22:06"


#include <fcntl.h>
#include <stdio.h>
#include <malloc.h>
#include <errno.h>
#include <ffio.h>
#include <stdlib.h>
#include <cray/assign.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fxlist.h"
#include "sqbio.h"

DECLARE(BUFA_XLIST);
struct xtr_s BUFA_XLIST_P = { BUFA_XLIST };

/*
 * open routine for bufa layer.
 */

_ffopen_t
_sqb_open(
const char	*name,
int		flags,
mode_t		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*stat,
long		cbits,
int		cblks,
struct gl_o_inf *oinf)
{
	_ffopen_t nextfio;
	long bs;
	int i,nb;
	int ret;
	struct sqb_f *sqb_info;
	struct sqbio *sqptr;
	union spec_u *nspec;
	struct ffc_info_s ffci;
	struct stat sysstat;
	struct ffsw dumstat;
	int isvalid;
	char *buf;
/*
 *	Allocate private storage
 */
	sqb_info = (struct sqb_f *)calloc(sizeof(struct sqb_f), 1);
	if (sqb_info == NULL) {
		errno = FDC_ERR_NOMEM;
		goto errret;
	}
	fio->lyr_info = (char *)sqb_info;

/*
 *	Set flag if -m on is assigned.   bufa layer handles -m on by
 *	default.
 */
	if (oinf->aip != NULL) {
		if (oinf->aip->m_multup_flg && oinf->aip->m_multup) {
			oinf->aip->m_multup_flg |= ATTR_USED;
		}
	}

	nspec = spec;
	NEXT_SPEC(nspec);
	nextfio = _ffopen(name, flags, mode, nspec, stat, cbits, cblks, NULL,
			oinf);
	if (nextfio == _FFOPEN_ERR) return(_FFOPEN_ERR);
	fio->fioptr = (struct fdinfo *)nextfio;
	ret = XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_GETINFO,
		&ffci, stat);
	if (ret < 0) {
		(void)XRCALL(fio->fioptr, closertn) fio->fioptr, &dumstat);
		free(sqb_info);
		return(_FFOPEN_ERR);
	}
	ret = XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_STAT,
		&sysstat, stat);
	if (ret < 0) {
		(void)XRCALL(fio->fioptr, closertn) fio->fioptr, &dumstat);
		free(sqb_info);
		return(_FFOPEN_ERR);
	}
	sqb_info->ffci_flags = ffci.ffc_flags;
/*
 *	Set up buffer size (in bits) and number of buffers
 */
	bs = _ff_nparm_getv(spec, 1, &isvalid);
	if (!isvalid) {
#ifdef _CRAY
		if (sysstat.st_oblksize != 0) {
			bs = sysstat.st_oblksize;
		}
#elif defined(__mips) || defined(_LITTLE_ENDIAN)
		if (sysstat.st_blksize != 0) {
			bs = sysstat.st_blksize;
		}
#endif
		else {
			bs = BUFA_DEF_BUFSIZ;
		}
	}
	else {
		if (bs <= 0) {
			errno = FDC_ERR_BUFSIZ;
			goto errret;
		}
		bs = bs*4096;	/* user specifies in units of 4096 bytes */
	}
	/*
	 * This layer uses MOV_BITS to move data to/from the layer's buffer.
	 * MOV_BITS uses an integer to give the number of bits that can
	 * be moved. Just to be on the safe side, make sure our buffer
	 * size is within these limits.
	 */
	if (bs >= (1<<sizeof(int)*8-5)) {
		if (isvalid) {
			errno = FDC_ERR_BUFSIZ;
			goto errret;
		}
		bs = BUFA_DEF_BUFSIZ;
	}
	sqb_info->bufsiz = bs<<3;

	nb = _ff_nparm_getv(spec, 2, &isvalid);
	if (!isvalid)
		nb = BUFA_DEF_NUMBUF;
	if (nb <= 0) {
		errno = FDC_ERR_NBUF0;
		goto errret;
	}
	sqb_info->nbuf = nb;

	/* allocate space for data buffers */
	
	if ((buf = malloc(bs *nb)) == NULL) {
		errno = FDC_ERR_NOMEM;
		goto errret;
	}
	sqb_info->sqb_buf = CPTR2BP(buf);

	/* allocate an array of structures that describe the data buffers*/
	if ((sqb_info->sqbio_base = 
	   (struct sqbio *)calloc(nb * sizeof(*sqptr),1)) == NULL) {
		errno = FDC_ERR_NOMEM;
		goto errret;
	}
	sqb_info->sqbio_cur = sqb_info->sqbio_base;
	sqb_info->_ptr = sqb_info->sqb_buf;

	fio->rwflag = POSITIN;
	sqptr = sqb_info->sqbio_base;
	for (i = 0; i < nb-1; i++) {
		/* Initialize the structures */
		sqptr->nxt = sqptr+1;
		SET_BPTR(sqptr->_base ,INC_BPTR( sqb_info->sqb_buf , (bs * i)<<3));
		sqptr++;
	}
	sqptr->nxt = sqb_info->sqbio_base;
	SET_BPTR(sqptr->_base ,INC_BPTR( sqb_info->sqb_buf , (bs * (nb-1))<<3));
	

	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

errret:
	(void)XRCALL(fio->fioptr, closertn) fio->fioptr, &dumstat);
	_sqb_clfree(fio);
	_SETERROR(stat, errno, 0)
	return(_FFOPEN_ERR);
}
