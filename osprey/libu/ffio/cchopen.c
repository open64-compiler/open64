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


#pragma ident "@(#) libu/ffio/cchopen.c	92.1	06/29/99 13:16:47"

#include <fcntl.h>
#include <stdio.h>
#include <limits.h>
#include <malloc.h>
#include <errno.h>
#include <ffio.h>
#include <cray/assign.h>
#ifdef _UNICOS_MAX
#include <mpp/globals.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include "cchio.h"
#include "fxlist.h"

DECLARE(CCH_XLIST);
struct xtr_s CCH_XLIST_P = { CCH_XLIST };

/*
 * 	Open routine for "cache" layer.
 *
 *	Returns: pointer to fdinfo block from next lower layer on success.
 *		 _FFOPEN_ERR if an error occurred.
 */

_ffopen_t
_cch_open(
const char	*name,
int		oflags,
mode_t		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*stat,
long		cbits,
int		cblks,
struct gl_o_inf *oinf)
{
	int		i;
	_ffopen_t	nextfio;
	int		nb;		/* number of buffers */
	int64		bs;		/* size of each buffer in bits */
	int		ret;
	int		isvalid;
	char		*s;
	bitptr		bptr;
	struct fdinfo	*nfioptr;
	union spec_u	*nspec;
	struct cch_f	*cch_info;
	struct cch_buf	*cbufs;
	struct stat	fstat;
	struct ffsw	clstat;
	struct ffc_info_s layer_info;
	int64		bypass;
#ifdef __mips
	struct dioattr dio;
	int o_direct = 0;
#endif

#if defined(_CRAY1) || defined(_CRAYMPP)
        oflags |= O_RAW;	/* We believe that bypassing system cache 
				 * enhances performance in most cases. */
#endif

/*
 *	Allocate the layer-specific data area.
 */
	cch_info = (struct cch_f *)calloc(1, sizeof(struct cch_f));
	if (cch_info == NULL)
		goto nomem;
	fio->lyr_info = (char *)cch_info;

	cch_info->optflags = 0;		/* initially, no special options */

/*
 *	Get values from the FFIO spec.
 */
#ifdef CCH_SDS_SUPPORTED
	if (spec->fld.recfmt == TR_CCH_SDS) {
		cch_info->optflags |= CCHOPT_SDS;
        	oflags |= O_SSD;	/* request I/O betw disk and SDS */
	}
#endif

	bs = _ff_nparm_getv(spec, 1, &isvalid) * BITPBLOCK;
	if (!isvalid) {
#ifdef _UNICOS_MAX
		if (_MPP_MPPSIM > 0) {
		/* Running on simulator in user virtual mode. Simulator can */
		/* not handle large reads/writes, so restrict the size */
			bs = CCH_DEF_SIMBUFSIZ * BITPBLOCK;
		}
		else
#endif
		bs = CCH_DEF_BUFSIZ * BITPBLOCK;	/* set default bs */
	}

	if (bs <= 0 || bs >= CCH_MAX_BBUFSIZ) {
		_SETERROR(stat, FDC_ERR_BUFSIZ, 0);
		goto badret;
	}

	nb = _ff_nparm_getv(spec, 2, &isvalid);
	if (!isvalid)
		nb = CCH_DEF_NBUF;			/* set default nb */

	if (nb <= 0) {
		_SETERROR(stat, FDC_ERR_NBUF0, 0);
		goto badret;
	}

	cch_info->nbufs    = nb;
	cch_info->bufs     = NULL;

/*
 *	Set flag if -m on is assigned.
 */
#ifdef __mips
	if (oflags & O_DIRECT)
		o_direct = 1;
#endif
	if (oinf->aip != NULL) {
		if (oinf->aip->m_multup_flg && oinf->aip->m_multup) {
			cch_info->is_multup = 1;
			oinf->aip->m_multup_flg |= ATTR_USED;
		}
#ifdef __mips
		if (oinf->aip->B_direct_flg) {
                	if (oinf->aip->B_direct)
                        	o_direct = 1;
			else
	                        o_direct = 0;
		}
#endif

	}

/*
 *	Allocate the buffer control blocks contiguously.
 */
	if ((cch_info->bufs = 
	    (struct cch_buf *)calloc(nb, sizeof(struct cch_buf))) == NULL)
		goto nomem;
/*
 *	Get the FFIO spec for the next lower layer.
 */
	nspec = spec;
	NEXT_SPEC(nspec);

/*
 *	Open the layers below this one.
 */
	nextfio = _ffopen(name, oflags, mode, nspec, stat, cbits, cblks, NULL,
			oinf);
	if (nextfio == _FFOPEN_ERR) goto badret;

	nfioptr = (struct fdinfo *)nextfio;

/*
 *	Get information about the underlying layer.
 */
	ret = XRCALL(nfioptr,fcntlrtn) nfioptr, FC_STAT, &fstat, stat);
	if (ret == ERR) goto close_badret;

	ret = XRCALL(nfioptr,fcntlrtn) nfioptr, FC_GETINFO, &layer_info, stat);
	if (ret == ERR) goto close_badret;

        if ( layer_info.ffc_flags & FFC_CANSYLISTIO )
                cch_info->do_sylistio = 1;

#ifdef __mips
/* 
 *	Have we been requested to open with O_DIRECT?
 */
	if (o_direct) {
		int nflag;
		int64 bsbyt;

		bsbyt = bs/8;	/* convert buffer size to bytes */
		/* determine buffer size requirements for O_DIRECT */
		ret = XRCALL(nfioptr,fcntlrtn) nfioptr, FC_DIOINFO, &dio, stat);
		if (ret == ERR) goto close_badret;
		/* Adjust the size of the buffers for O_DIRECT's requirements.*/
		if (bsbyt % dio.d_miniosz != 0){
			/* We need to write in these units. */
			bsbyt = bsbyt - bsbyt%dio.d_miniosz;
		}		
		if (bsbyt < dio.d_miniosz) {
			bsbyt = dio.d_miniosz;
		}		
		else if (bsbyt > dio.d_maxiosz) {
			bsbyt  = dio.d_maxiosz;
		}
		if (bsbyt % dio.d_mem != 0){
			/* Each buffer needs to be memaligned */
			/* Since this layer expects all buffers */
			/* to be contiguous, we're out of luck. */
			errno = FDC_ERR_BUFSIZ;
			goto close_badret;
		}
		bs = bsbyt*8;	/* convert back to bits */
		cch_info->maxiosize = dio.d_maxiosz;
		cch_info->miniosize = dio.d_miniosz;
		cch_info->chunksize = dio.d_miniosz;
		cch_info->diskalign = dio.d_miniosz;
		cch_info->memalign = dio.d_mem;
		cch_info->odirect = 1;
	}
	else {
		cch_info->maxiosize = 0;
		cch_info->miniosize = 1;
		cch_info->chunksize = 1;
		cch_info->diskalign = 1;
		cch_info->memalign = 1;
	}
#else
		cch_info->maxiosize = 0;
		cch_info->miniosize = fstat.st_blksize;	/* 1 sector */
		cch_info->chunksize = fstat.st_blksize;	/* 1 sector */
		cch_info->diskalign = fstat.st_blksize ; /* 1 sector */
#ifdef _CRAYMPP
		cch_info->memalign = 0x3f;	/* 8 word boundary */
#else
		cch_info->memalign = 0x7;	/* 1 word boundary */
#endif
#endif
	cch_info->bsize    = bs;
	bypass = _ff_nparm_getv(spec, 3, &isvalid) *BITPBLOCK;
	if (!isvalid){
		/* set default bypass size */
#ifdef __mips
		bypass = bs;	
#else
		bypass = bs*nb;
#endif
	}
	if (bypass <= 0) {
		_SETERROR(stat, FDC_ERR_BADSPC, 0);
		goto close_badret;
	}
	cch_info->bypasssize = bypass;
/*
 *	Allocate the page buffers contiguously.  They are allocated in 
 *	memory or in SDS.
 */
#ifdef CCH_SDS_SUPPORTED
	if (cch_info->optflags & CCHOPT_SDS) {
		int oblk, istat;
		oblk = sdsalloc(nb * BITS2BLOCKS(bs), &istat);
		if (oblk == ERR || istat == ERR) {
			_SETERROR(stat, FENOSDSP, 0);
			goto close_badret;
		}
		s = (char*)NULL + oblk * BYTPBLOCK;	/* byte pointer */
	}
	else
#endif
#ifdef __mips
	if (o_direct) {
		if ((s = memalign(dio.d_mem, nb * BITS2BYTES(bs))) == NULL){
			_SETERROR(stat, FDC_ERR_NOMEM, 0)
			goto close_badret;
		}
	}
	else
#endif
	{
		if ((s = malloc(nb * BITS2BYTES(bs))) == NULL){
			_SETERROR(stat, FDC_ERR_NOMEM, 0)
			goto close_badret;
		}
	}
	cch_info->bufs_alloc = 1;		/* buffer pages now allocated */
			
	SET_BPTR(bptr, CPTR2BP(s));

	cbufs = cch_info->bufs;
	for (i=0; i<nb; i++) {
		SET_BPTR(cbufs[i].buf, bptr);	/* page buffer bit address */
		cbufs[i].filead = -1;		/* indicate buffer is unused */

		SET_BPTR(bptr,(INC_BPTR(bptr,bs)));
	}

	if ((cch_info->savearea = malloc(_CCH_SMSIZ)) == NULL) {
		_SETERROR(stat, FDC_ERR_NOMEM, 0)
		goto close_badret;
	}
/*
 *	Store the size of the underlying file.   For block special files, 
 *	the size is unknown.  Assume infinite size until stat system call
 *	returns the proper size. 
 */
	if (S_ISBLK(fstat.st_mode) && fstat.st_size == 0) {
#ifdef __mips
		cch_info->feof = LONGLONG_MAX;	/* assume infinite size */
#else
		cch_info->feof = LONG_MAX;	/* assume infinite size */
#endif
		cch_info->is_blkspec = 1; 	/* flag block special file */
	}
	else
		cch_info->feof = fstat.st_size << 3;

#ifdef CCH_SDS_SUPPORTED
	if (cch_info->optflags & CCHOPT_SDS) {
		/*
		 * If SDS cache is requested, the underlying layer must support
		 * O_SSD because data transferral between this layer and below
		 * is by backdoor reads and writes.
		 */
		if ((layer_info.ffc_flags & FFC_BCKDOOR) == 0) {
			_SETERROR(stat, FDC_ERR_BCKDOOR, 0);
			goto close_badret;
		}
		/*
		 * The underlying file must have a size which is a multiple of
		 * this layer's granularity.
		 */
		if (cch_info->feof & (BITPBLOCK - 1)) {
			_SETERROR(stat, FDC_ERR_OPNGRAN, 0);
			goto close_badret;
		}
	}
#endif

/*
 *	Initialize the logical size of the file and the current file position.
 */
	cch_info->fsize = cch_info->feof;	/* logical end of file */
	cch_info->cpos  = 0;			/* positioned at start of file*/
	cch_info->cubuf = NULL;			/* no buffer is assigned yet */ 
	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

nomem:
	_cch_clfree(fio);
	_SETERROR(stat, FDC_ERR_NOMEM, 0)
	return(_FFOPEN_ERR);

close_badret:
       	(void) XRCALL(nfioptr,closertn) nfioptr, &clstat);
badret:
	_cch_clfree(fio);
	return(_FFOPEN_ERR);
}
