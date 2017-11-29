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


#pragma ident "@(#) libu/ffio/cosopen.c	92.4	10/07/99 22:14:49"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <malloc.h>
#include <ffio.h>
#ifdef	_UNICOS_MAX
#include <mpp/globals.h>
#endif
#include "cosio.h"
#include "fxlist.h"

DECLARE(COS_XLIST);
struct xtr_s COS_XLIST_P = { COS_XLIST };

/*
 * _cos_open()
 *	Do COS blocking specific things to open a COS file
 */

_ffopen_t
_cos_open(
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
	register int	kick;
	register int	ret;
	int		bs, bitsize;
	_ffopen_t	nextfio;
	char		*bptr;
	union spec_u	*nspec;
	struct cos_f	*cos_info;
	struct stat	sysstat;
	struct ffsw	dumstat;
	struct ffc_info_s ffci;

#ifdef	_CRAY
	flags |= O_RAW;	/* needed for performance on XMP */
#endif

	fio->rwflag = POSITIN;
/*
 *	Allocate private info area
 */
	cos_info = (struct cos_f *)calloc(sizeof(struct cos_f), 1);

	if (cos_info == NULL)
		goto nomem;

	fio->lyr_info = (char *)cos_info;

	cos_info->cos_cnt = 0;

/*
 *	Open the lower layer(s)
 */

	nspec = spec;
	NEXT_SPEC(nspec);

	nextfio = _ffopen(name, flags, mode, nspec, stat, cbits, cblks, NULL,
			oinf);

	if (nextfio == _FFOPEN_ERR) goto badret;

	fio->fioptr = (struct fdinfo *)nextfio;

	ret = XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_STAT,
		&sysstat, stat);

	if (ret < 0)
		{
		(void)XRCALL(fio->fioptr, closertn) fio->fioptr, &dumstat);
		goto badret;
		}

	ret = XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_GETINFO,
		&ffci, stat);

	if (ret < 0)
		{
		(void)XRCALL(fio->fioptr, closertn) fio->fioptr, &dumstat);
		goto badret;
		}

	switch (flags & (O_RDWR | O_RDONLY | O_WRONLY))
		{
		case O_RDWR:
			cos_info->cos_flag = COS_IOREAD | COS_IOWRT;
			break;

		case O_RDONLY:
			cos_info->cos_flag = COS_IOREAD;
			break;

		case O_WRONLY:
			cos_info->cos_flag = COS_IOWRT;
			break;

		default:
			_SETERROR(stat, FDC_ERR_INTERR, 0);
			return(_FFOPEN_ERR);
		}

	cos_info->cos_flag |= COS_IOBOD;
/*
 *	Set up sync/async characteristics.  If auto or zero, default
 *	to sync.
 */
	fio->rtype = spec->fld.recfmt;

	if (spec->fld.recfmt == 0)
		fio->rtype = TR_AUTO;
/*
 *	Compute buffer size.
 *	1) User-specified value, if any, or
 *	2) st_oblksize (UNICOS), or
 *	3) the larger of the COS default and st_blksize.
 */

	kick	= 0;
	bs	= spec->fld.recsize;	/* User-specified buffer size */

	if (bs == 0) {

		kick	= 1;

#ifdef _UNICOS_MAX
		if (_MPP_MPPSIM > 0) {
			/*
			 * If running in user virtual mode, the simulator
			 * can not handle large reads/writes, so pick an
			 * appropriate (smaller) default.
			 */
			bs	= DEF_SIMBSIZE;
			kick	= 0;	/* Don't increase this default */
		}
		else
#endif
#ifdef _CRAY
		if (sysstat.st_oblksize != 0)
			bs	= sysstat.st_oblksize >> BLKSZ2BY;
		else
#endif
		if (sysstat.st_blksize != 0)
			bs	= sysstat.st_blksize >> BLKSZ2BY;

		if (bs < DEF_BSIZE)
			bs	= DEF_BSIZE;
	}

/*
 *	If buffers are big enough, turn on async.  Since async will
 *	use twice as many buffers, double the buffer size if we are
 *	using a default value.
 */
	if (fio->rtype == TR_AUTO)
		{
		if (bs < ASYNC_THRESH)
			fio->rtype	= TR_SYNC;
		else {
			fio->rtype	= TR_ASYNC;
			bs		= bs << kick;
		}
		}

	bitsize	= bs * BLKSZ;
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/*
 *      Although the _ffbufsiz field is declared as long,
 *      these routines have dependencies that restrict the
 *      buffer size to be no bigger than the size of an integer.
 */
        if (bitsize > COS_MAX_BBUFSIZ) {
                _SETERROR(stat, FDC_ERR_BUFSIZ, 0);
                goto badret;
        }
#endif
	if (fio->rtype == TR_ASYNC)
		{
		/* Round up to even number of sectors, then divide in half */
		bitsize	= ((bs + 1) >> 1) * BLKSZ;
		bptr	= malloc(bitsize >> 3);	/* need bytes */

		if (bptr == NULL)
			goto nomem;
	
		SET_BPTR(cos_info->obuf, CPTR2BP(bptr));
		}

	bptr	= malloc(bitsize >> 3);	/* need bytes */

	if (bptr == NULL)
		goto nomem;

	SET_BPTR(fio->_base, CPTR2BP(bptr));
        fio->_ptr			= fio->_base;
        fio->_ffbufsiz			= bitsize;	/* bits */

	cos_info->ffci_flags		= ffci.ffc_flags;
        cos_info->cos_blklmt		= fio->_base;
	cos_info->opos			= -1;	/* flag not active */
	cos_info->bstat.sw_flag		= 1;	/* no outstanding I/O */
	cos_info->bstat.sw_error	= 0;	/* no outstanding I/O error */
	cos_info->bstat.sw_count	= 0;	/* no outstanding I/O count */
	FFSTAT(cos_info->bstat)		= FFBOD; /* Initial point */

        cos_info->cos_cwptr		= (_cw_type *)BPTR2WP(fio->_base);
	cos_info->cos_size		= 0;
	cos_info->cos_cbn		= -1;
	cos_info->cos_pri		= 0;
	cos_info->cos_diskpos		= 0;

	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

nomem:
	_SETERROR(stat, FDC_ERR_NOMEM, 0);

badret:
	_cos_clfree(fio);
	return(_FFOPEN_ERR);
	}
