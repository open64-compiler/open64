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


#pragma ident "@(#) libu/ffio/mropen.c	92.1	06/29/99 13:16:47"


#ifdef DEBUG
int mr_trace_fd;
#endif
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <malloc.h>
#include <errno.h>
#include <ffio.h>
#include <string.h>
#include "fssio.h"
#include "fxlist.h"

DECLARE(MR_XLIST);
struct xtr_s MR_XLIST_P = { MR_XLIST };

/*
 * open routine for memory resident file.
 * If the layer has the SAVE option, try to read the file in from disk.
 */

extern union spec_u *_fss_cpy_spc();

int
_mr_open(
const char	*name,
int		flags,
int		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*stat,
long		cbits,
int		cblks,
struct gl_o_inf *oinf)
{
	int nextfio;
	int ret;
	int llflags;
	long mininc;
	struct fdinfo *nfioptr;
	union spec_u *nspec;
	struct mr_f *mr_info;
	struct ffsw clstat;

/*
 *	Allocate private storage
 */
	mr_info = (struct mr_f *)calloc(sizeof(struct mr_f), 1);
	if (mr_info == NULL) goto nomem;
	fio->lyr_info = (char *)mr_info;
/*
 *	Set up min, max allocation in buffer and min increment
 */
	mr_info->minsize = spec->fss.min;
	mr_info->maxsize = (spec+1)->info.quan;
	mininc = (spec+2)->info.quan;
	if (mininc == 0) mininc = MRMININC;
	mr_info->mininc = mininc;
	if (mr_info->maxsize == 0)
		mr_info->maxsize = 0x3fffffffffff;
	if (mr_info->minsize == 0) {
		if (mininc > mr_info->maxsize) mininc = mr_info->maxsize;
		mr_info->minsize = mininc;
	}
	mr_info->name = strdup(name);
	if (mr_info->name == NULL) goto badret;
		
/*
 *	Internally, both blksize and recsize are in bits!
 */
	mr_info->mrdirty = 0;
	mr_info->overflowed = NO;
	mr_info->ovoff = 0;
	fio->segbits = 0;
	fio->_cnt = 0;
	fio->_ptr = fio->_base;

/*
 *	Open the lower layers
 */
	nspec = spec;
	NEXT_SPEC(nspec);

	if (fio->rtype != TR_FSS_SCR || fio->subtype != FSS_OPT_NOVFL) {
		int write_only = 0;
		llflags = flags;

/*
 *		The lower level file must be readable to allow loading into
 *		memory.
 */	 
		if ((llflags & O_ACCMODE) == O_WRONLY) {
			write_only = 1;
			llflags &= ~O_ACCMODE;
			llflags |= O_RDWR;
		}
		nextfio = _ffopen(name, llflags, mode, nspec, stat, cbits,
			cblks, NULL, oinf);
/*
 *		If write_only and file has no read permissions, then it
 *		may be opened O_WRONLY.  But it better be empty, because if not 
 *		_mr_load will get an error later.
 */
		if (nextfio < 0 && write_only) {
			llflags = flags;
			nextfio = _ffopen(name, llflags, mode, nspec, stat,
				cbits, cblks, NULL, oinf);
		}
		if (nextfio < 0) goto badret;
	}
	else {
		nextfio = 0;
                mr_info->mreof = 0;
	}

	nfioptr = (struct fdinfo *)nextfio;
	fio->fioptr = nfioptr;	/* must do this early for _mr_load() */

	if (fio->rtype == TR_FSS_SCR) {
/*
 *		If the O_CREAT flag is not set, fail.  By definition, the
 *		scratch file does not exist.
 */
		if ((flags & O_CREAT) != O_CREAT) {
			_SETERROR(stat, ENOENT, 0);
			goto close_badret;
		}
/*
 *		This unlinks the underlying file and tells underlying layers
 *		that this is a scratch file. 
 */
		ret = XRCALL(fio,fcntlrtn) fio, FC_SCRATCH,
			&mr_info->scrtch_flgs, stat);
		if (ret == ERR) goto close_badret;
	}
	else {		/* else if TR_FSS_SAVE */
/*
 *		Load the file into the buffer if its size is nonzero.
 */
		ret = _mr_load(fio, mr_info, stat);
		if (ret == ERR) goto close_badret;
	}

	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

nomem:
	ERETURN(stat, FDC_ERR_NOMEM, 0);

close_badret:
	if (nfioptr != NULL)
        	(void) XRCALL(nfioptr,closertn) nfioptr, &clstat);
badret:
	_mr_clfree(fio);
	return(ERR);
}

/*
 *	Load up the file into the buffer
 */
int
_mr_load(fio, mr_info, ffstat)
struct fdinfo *fio;
struct mr_f *mr_info;
struct ffsw *ffstat;
{
	long eofbits;
	int ubc;
	int ret;
	int inc;
	int done;
	int eofhit;
	long space;
	struct ffsw locstat;
	struct stat statbuf;

	locstat.sw_error = 0;
	ret = XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_STAT, &statbuf,
		ffstat);
	if (ret < 0) return(ret);

/*
 *	Loop until end of data reading up the file into the buffer
 */
	eofbits = 0;
	inc = statbuf.st_size << 3;
	if (inc == 0) return(0);
	eofhit = 0;
	locstat.sw_error = 0;
	done = L_ISMORE;
	while(done == L_ISMORE) {
		ubc = 0;
/*
 *		Make sure that there is more space past the EOF.
 *		_mr_grow will force growth to be done with a reasonable
 *		granularity.
 */
		if ((eofbits+inc) > mr_info->mrsize)
			ret = _mr_grow(fio, inc, ffstat);

		inc = 1;
		if (ret < 0)
			return(ERR);
		else if (ret > 0)
			done = L_OVFLWD;	/* if overflowed, last time thru */

		/* calculate space in bytes */
		space = mr_info->mrsize - eofbits;
		space = space >> 3;	/* bits -> bytes.. */
		if (space == 0)
			done = L_OVFLWD;	/* must be overflow. How else?*/
		else {
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_base, eofbits));
			ret = XRCALL(fio->fioptr, readrtn) fio->fioptr,
				fio->_ptr, space, &locstat, PARTIAL, &ubc);
			if (ret < 0) {
				ERETURN(ffstat, locstat.sw_error, 0);
			}
			else if (ret > 0) {
/*
 *				If data after an EOF, issue error.
 */
				if (eofhit!= 0) ERETURN(ffstat,FDC_ERR_NOSUP,0);
				eofbits += ret*8 - ubc;
				SET_BPTR(fio->_ptr,
					INC_BPTR(fio->_ptr, ret*8 - ubc));
			}
			else if (ret == 0) {
				if (FFSTAT(locstat) == FFEOD) done = L_GOTALL;
				if (FFSTAT(locstat) == FFEOF) eofhit = 1;
			}
		}
	}
/*
 *	See if there was more data that would not fit.  If so, then
 *	check if overflow is legal, if so, set the EOF to what it
 *	*really* is.
 */
	mr_info->mreof = eofbits;
	if (done == L_OVFLWD) {
		ret = _fss_overflow(fio, &locstat);
		if (ret < 0) ERETURN(ffstat, locstat.sw_error, 0);
		if (fio->subtype == FSS_OPT_NOVFL)
			ERETURN(ffstat, FDC_ERR_FSSOVF, 0);
		mr_info->mreof = statbuf.st_size << 3;
	}
	ret = XRCALL(fio, seekrtn) fio, 0, 0, ffstat);
	return(ret);
}
