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


#pragma ident "@(#) libu/ffio/cdcopen.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <malloc.h>
#include <ffio.h>
#include "cdcio.h"
#include "fxlist.h"

DECLARE(CDC_XLIST);
struct xtr_s CDC_XLIST_P = { CDC_XLIST };

/*
 * Initialize the state of a CDC format foreign dataset.  Allocate buffers
 * and initialize pointers.
 */

int
_cdc_open(
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
	int nextfio = 0;
	int ll_blocked;
	char *ptr;
	union spec_u *nspec;
	int recsize, blksize;
	struct ffsw *dumstat;
	struct cdc_f *cdc_info;

	recsize = 0;			/* this is ignored */
/*
 *	Blocksize is 512 60 bit words, or 5120 6-bit characters
 */
	blksize = 5120*6;		/* other block sizes not allowed */

/*
 *	Internally, both blksize and recsize are in bits!
 */
	switch(spec->fld.recfmt)
		{
		case TR_CDC_CZ:
			fio->maxrecsize = recsize;
			break;
		case TR_CDC_CS:
		case TR_CDC_IW:
		case TR_CDC_CW:
			fio->maxrecsize = -1;
			break;
		}
	fio->maxblksize = blksize;
/*
 *	Allocate buffer:
 *	block size plus possible 48 bit block terminator plus one 60-bit word
 *	plus 16 slop bytes.
 */
	fio->_ffbufsiz =
		blksize + 48 + 64 + 64 + 7; /* bufsiz in bytes + fudge */
	ptr = malloc((fio->_ffbufsiz >> 3) + 16);
	if (ptr == NULL) goto nomem;
/*
 *	Allocate private storage area
 */
	cdc_info = (struct cdc_f *)calloc(sizeof(struct cdc_f), 1);
	if (cdc_info == NULL) goto nomem;
	fio->lyr_info = (char *)cdc_info;

	SET_BPTR(fio->_base, CPTR2BP(ptr));
	fio->rwflag = POSITIN;
	fio->segbits = 0;
	fio->_cnt = 0;
	fio->_ptr = fio->_base;
/*
 *	Now, open the lower layers...
 */
	nspec = spec;
	NEXT_SPEC(nspec);
	nextfio = _ffopen(name, flags, mode, nspec, stat, cbits, cblks, NULL,
			oinf);
	if (nextfio < 0) goto badret;
	fio->fioptr = (struct fdinfo *)nextfio;

	XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_GETINFO,
		&cdc_info->ffci, &dumstat);
	ll_blocked = cdc_info->ffci.ffc_flags & FFC_REC;

	switch(fio->subtype)
		{
		case TR_CDC_BT_DISK:
			break;		/* either record or stream is OK */
		case TR_CDC_BT_SI:
		case TR_CDC_BT_I:
			if (ll_blocked == 0) /* if not blocked */
				{
				_SETERROR(stat, FDC_ERR_NOBDRY, 0);
				goto badret;
				}
			break;
		}

	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

nomem:
	_SETERROR(stat, FDC_ERR_NOMEM, 0);
badret:
	if (nextfio > 0) XRCALL(fio->fioptr, closertn) fio->fioptr, &dumstat);
	if (BPTR2CP(fio->_base) != NULL) free(BPTR2CP(fio->_base));
	if (fio->lyr_info != NULL) free(fio->lyr_info);
	return(ERR);
	}

/*
 * _cdc_close()
 * Close a cdc stream.  The basic algorithm is as follows:
 *	do cleanup on your layer
 *	if lower level exists
 *		call lower level close routine
 *		free lower level info block
 *	return 0 on success, else failed.
 *
 * Note that it is the callers responsibility to free the memory
 * occupied by the info block of the layer(s) it closes.
 */
_cdc_close(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	int ret, ret2;

	ret = 0;
	ret2 = 0;
/*
 *	CDC formats always truncate on write.
 */
	if (fio->rwflag == WRITIN)
		ret = XRCALL(fio, weodrtn) fio, stat);
	else
		ret = XRCALL(fio, flushrtn) fio, stat);
	if (BPTR2CP(fio->_base) != NULL)
		free(BPTR2CP(fio->_base));	/* free buffer */
	if (fio->lyr_info != NULL)
		free((char *)fio->lyr_info);	/* free private storage */
	llfio = fio->fioptr;
	if (llfio != NULL)
		{
		ret2 = XRCALL(llfio, closertn) llfio, stat);
		free(llfio);		/* free info block */
		}
	if ((ret | ret2) != 0) ret = -1;
	return(ret);
	}
