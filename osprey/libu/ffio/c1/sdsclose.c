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


static char USMID[] = "@(#) libu/ffio/c1/sdsclose.c	92.0	10/08/98 14:57:41";

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <malloc.h>
#include <ffio.h>
#include "../fssio.h"

void _sds_clfree();

/*
 *	close SDS file
 *      Returns: -1 if error occurred
 *		  0 if OK
 */

_sds_close(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
	struct fdinfo *llfio;
	int ret, ret1, ret2, ret3, locret;
	long tfl;
	struct sds_f *sds_info;
	_lociosw *locptr;
	int i;
	struct ffsw locstat;
	int sverrno;
	struct _loclink *loclink;
	

	llfio = fio->fioptr;
	sds_info = (struct sds_f *)fio->lyr_info;

	ret = ret1 = ret2 = ret3 = 0;

/*
 *	Wait for asynchronous requests that have not been recalled.
 */
	loclink = sds_info->loclist;
	while (loclink != NULL) {
		locptr = loclink->loc_first;
		for (i = 0; i < _FSSASYNUM; i++) {
			if (locptr->user_sw != NULL) {
				/* wait for it */
				locret = XRCALL(llfio, fcntlrtn)
					llfio, FC_RECALL, &locptr->local_sw,
					&locstat);         
				if (locret < 0) {
					sverrno = locstat.sw_error;
					ret1 = ERR;
				}
			}
			locptr++;
		}
		loclink = loclink->loc_nxt;
	}
	if (fio->rtype == TR_FSS_LCL) { /* Local, keep the SDS around. */
		ERETURN(stat, FDC_ERR_INTERR, 0); /* better not get here! */
		/* local is not implemented.  If it were, we would... */
		/* save stuff away.  Set sdsalo ptr to NULL so not freed */
		/* in clfree */
		}
	else if (fio->rtype == TR_FSS_SCR) { /* Scratch, Toss it... */
		ret = _sds_freeall(sds_info, stat);

		if (llfio != NULL) {
			ret3 = XRCALL(llfio, closertn)
				llfio, stat);
		}
		sds_info->sdseof = 0;
	}
	else if (fio->rtype == TR_FSS_SAVE) {
/*
 *		Flush/release SDS
 */
		tfl = fio->opn_flags &
				(O_RDWR | O_RDONLY | O_WRONLY | O_APPEND);
		if (tfl != O_RDONLY)
			ret = _sds_unload(fio, stat);
/*
 *		Release the SDS space
 */
		ret2 = _sds_freeall(sds_info, stat);

		ret3 = XRCALL(llfio, closertn) llfio, stat);
	}

	_sds_clfree(fio);

	if ((ret | ret1 | ret2 | ret3) == 0) return(0);
	if (ret1 == ERR)
		SETSTAT(stat,sverrno,0);
	return(ERR);
}

/*
 * _sds_freeall()
 *	Free all of the SDS slices for the file.
 */
_sds_freeall(sds_info, stat)
struct sds_f *sds_info;
struct ffsw *stat;
{
	int i;
	int ret;
	struct sds_alo_s *alop;

	ret = 0;
	if (sds_info->sdsalnum > 0) {
		alop = sds_info->sdsalo;
		for (i = 0 ; i < sds_info->sdsalnum ; i++)
			ret |= sdsfree(alop[i].base);
	}
	if (ret < 0)
		_SETERROR(stat, FDC_ERR_FSSALO, 0);
	return(ret);
}

/*
 * _sds_clfree()
 *	Free the memory blocks used by the sds layer.
 */
void
_sds_clfree(fio)
struct fdinfo *fio;
{
	int ret;	/* only for debug output */
	struct sds_f *sds_info;
	struct _loclink *loclink;
	struct _loclink *locold;

	sds_info = (struct sds_f *)fio->lyr_info;

	if (sds_info->name != NULL) {
		free(sds_info->name);
		sds_info->name = NULL;
	}
	loclink = sds_info->loclist;
	while (loclink != NULL) {
		locold = loclink;
		loclink = loclink->loc_nxt;
		free(locold);
	}
	if(sds_info->sdsalo != NULL)
		free(sds_info->sdsalo);
	if (sds_info->bdfd >= 0) {
		ret = close(sds_info->bdfd);	/* just in case... */
		FSSTRACE(_xrt_putf("%s: close of 0x%x returns 0x%x\n",
			"_sds_close", sds_info->bdfd, ret));
		sds_info->bdfd = -1;
	}
	if(BPTR2CP(fio->_base) != NULL)
		free(BPTR2CP(fio->_base));
	if (BPTR2CP(sds_info->locbuf) != NULL)
		free(BPTR2CP(sds_info->locbuf));
	SET_BPTR(fio->_base, CPTR2BP(NULL));
	SET_BPTR(sds_info->locbuf, CPTR2BP(NULL));
	if (fio->fioptr != NULL) {
		free(fio->fioptr);
		fio->fioptr = NULL;
	}
	if (fio->lyr_info != NULL) {
		free(fio->lyr_info);
		fio->lyr_info = NULL;
	}
	return;
}

/*
 *	Unload the sds data by writing it all to the lower level.
 *	This routine is used in close processing and also when doing overflow
 *	handling.  It must not try to close the lower layer(s).  It only
 *	unloads the data from the sds.
 */

_sds_unload(fio, ffstat)
struct fdinfo *fio;
struct ffsw *ffstat;
{
	struct fdinfo *llfio;
	struct ffsw locstat;
	int ret, ubc;
	int nb;
	int i;
	int xfer;
	int evnblks;
	int bdfd;
	int done;
	int blksize;
	char *cp;
	long ctr;
	long unlsize;		/* number of bits to unload */
	struct sds_f *sds_info;
	struct sds_alo_s *alop;

	sds_info = (struct sds_f *)fio->lyr_info;
/*
 *	Only flush out the SDS if 'dirty'
 */
	if (sds_info->sdsdirty != 0) {
		llfio = fio->fioptr;
/*
 *		Make sure that the buffer is flushed!
 */
		if (fio->_cnt > 0)
			if (XRCALL(fio, flushrtn) fio, ffstat) < 0)
				return(ERR);
		CLRFFSTAT(locstat);
/*
 *		Determine how much to unload.
 */
		ctr = 0;			/* in bits */
		nb = 0;				/* in bytes */
		unlsize = sds_info->sdseof;
		if (unlsize > sds_info->sdssize)
			unlsize = sds_info->sdssize;

		done = L_ISMORE;
/*
 *		If we can, use the backdoor..
 */
		bdfd = sds_info->bdfd;
		blksize = sds_info->dsk_blksize;
		if (bdfd >= 0)	{ /* if we have a back door fd... */
/*
 *			Calculate even number of blocks to load
 */
			evnblks = ((unlsize + 7) >> 3) & ~(blksize - 1);
/*
 *			If back door file descriptor is available, and some
 *			data is 'raw' enough to load, use back door.
 */
			if (bdfd >= 0 && evnblks > 0) {
/* 
 *				Seek back door fd to zero, note that if this
 *				fails, we abort the backdoor operation.
 */
				BDSEEK(ret, bdfd, 0, 0);
				if (ret < 0) goto bad_bdop;

				i = 0;
				alop = sds_info->sdsalo;
				while (nb < evnblks) {
					xfer = BLKS2BYTES(alop[i].size);
					if (xfer > (evnblks - nb))
						xfer = evnblks - nb;
/*
 *					Note that if the I/O fails, we assume
 *					that it is due to something benign,
 *					and just continue. This is most
 *					likely due to the SDS allocations
 *					not being well formed.  i.e. on a DD-60,
 *					an SDS allocation of 3 sectors would
 *					cause an error here.  sds_info->sdsklik
 *					should avoid problems here.
 */
					BDWRITE(ret, bdfd,
						BLKS2WORDS(alop[i].base), xfer);
					if (ret < 0) goto bad_bdop;
					i++;
					nb += ret;	/* total up bytes read */
				}
bad_bdop:
				ctr = nb << 3;
			}
		}
/*
 *		Seek both SDS layer, and system layer
 *		to match current position, including any backdoor operations.
 */
		ret = XRCALL(fio, seekrtn) fio, nb, 0, &locstat);
		if (ret < 0) return(ERR);

		ret = XRCALL(llfio, seekrtn) llfio, nb, 0, &locstat);
		if (ret < 0) return(ERR);
/*
 *		Allocate local buffer if necessary
 */
		if (ctr < unlsize && BPTR2CP(sds_info->locbuf) == NULL) {
			cp = malloc(BLKS2BYTES(SECBSZ));
			if (cp == NULL)
				ERETURN(ffstat, FDC_ERR_NOMEM, 0);
			SET_BPTR(sds_info->locbuf, CPTR2BP(cp));
		}


		while(ctr < unlsize) {
			ubc = 0;
			nb = BLKS2BYTES(SECBSZ);
			if ((nb << 3) > (unlsize - ctr))
				nb = (unlsize - ctr + 7) >> 3;
			ret = XRCALL(fio, readrtn) fio, sds_info->locbuf,
					nb, ffstat, PARTIAL, &ubc);
			if (ret < 0) return(ERR);

			if (ret > 0) {
				ret = XRCALL(llfio, writertn) llfio,
					sds_info->locbuf, ret,
					ffstat, PARTIAL, &ubc);
				if (ret < 0)
					return(ERR);
			}
			ctr += (ret << 3) - ubc;
		}
		ret = XRCALL(llfio, seekrtn)
			llfio, (sds_info->sdseof + 7) >> 3, 0, ffstat);
		if (ret < 0) return(ERR);
		ret = XRCALL(llfio, weodrtn) llfio, ffstat);
		if (ret < 0) return(ERR);
	}
	return(0);
}
