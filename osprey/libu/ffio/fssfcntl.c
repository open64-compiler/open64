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


#pragma ident "@(#) libu/ffio/fssfcntl.c	92.1	06/29/99 13:16:47"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ffio.h>
#include "fssio.h"
#include <cray/mtlock.h>

/*
 * fss (sds and mr) fcntl requests
 *
 * Parameters:
 *	fd	- file descriptor (dummy)
 *	cmd	- command code
 *	arg	- command specific parameter
 *	iostat	- pointer to status return word
 */

int
_fss_fcntl(
struct fdinfo   *fio,
int             cmd,
int             arg,
struct ffsw     *iostat)

{
	struct fdinfo *llfio;
	struct ffc_info_s *ffcp, locinfo;
	struct mr_f *mr_info;
	struct sds_f *sds_info;
	struct stat *stptr;
	struct ffsw *uiostat;
	int ret, llfd;
	int size;
	int alloc, i;
	_lociosw *locptr;
	plock_t *lock_ptr;
	plock_t *rcl_lock;
	struct _loclink *loclink;

	llfd = -1;
	mr_info = (struct mr_f *) fio->lyr_info;
	sds_info = (struct sds_f *) fio->lyr_info;
	llfio = fio->fioptr;
	ret = 0;

	switch(cmd) {
		case FC_GETINFO:
			ffcp = (struct ffc_info_s *)arg;
/*
 *			Only do a call to the lower level if it has been opened
 */
			if (llfio != NULL) {
				ret = XRCALL(llfio,fcntlrtn)
					llfio, FC_GETINFO, &locinfo, iostat);
				llfd = locinfo.ffc_fd; /* fd from lower layer */
			}
			else
				locinfo.ffc_flags = FFC_NOTRN;

			ffcp->ffc_flags = 
				FFC_STRM |	/* stream */
				FFC_WEOD |	/* can write EOD */

				FFC_SEEKA |	/* seek abs */
				FFC_SEEKR |	/* seek relative */
				FFC_SEEKE |	/* seek end */
				FFC_RWND |	/* can rewind */

				FFC_BINARY |	/* can do binary */
				FFC_CODED |	/* can do chars */

				FFC_RDM |	/* can do random */
				FFC_SEQ |	/* can do seq */
				0;

			/* no data transformation */
			ffcp->ffc_flags |= locinfo.ffc_flags & FFC_NOTRN;

			ffcp->ffc_gran = 1;	/* granularity is 1 bit */
			ffcp->ffc_reclen = 0;	/* no rec length */
			ffcp->ffc_fd = llfd;
			break;

		case FC_STAT:
/*
 *			The real size is in SDS, if it has been allocated.
 */
			if (fio->class == CLASS_SDS) {
				size = sds_info->sdseof;
				alloc = sds_info->sdssize;
			}
			else {
				size = mr_info->mreof;
				alloc = mr_info->mrsize;
			}
/*
 *			Fill in most of the fields from the lower layer.
 */
			stptr = (struct stat *)arg;
			if (llfio != NULL) {
				ret = XRCALL(llfio,fcntlrtn)
					llfio, FC_STAT, arg, iostat);
			}
			else {
				/* must be scratch. Zero structure, then */
				/* fill in a few pertinent fields */
				memset(stptr, 0, sizeof(*stptr));
				stptr->st_mode = S_IFCHR | 0600;
				stptr->st_blocks = BITS2BLKS(alloc);
				stptr->st_blksize = 4096;
			}

			stptr->st_size = (size + 7) >> 3;	/* >> bytes */
			break;

		case FC_ASPOLL:
		case FC_RECALL:
			uiostat = (struct ffsw *)arg;
			if ((FFSTAT(*uiostat) != 0) && uiostat->sw_flag == 1)
					break;
			if (fio->class == CLASS_SDS) {
				lock_ptr = &sds_info->locsw_lock;
				rcl_lock = &sds_info->rcllock;
			}
			else {
				lock_ptr = &mr_info->locsw_lock;
				rcl_lock = &mr_info->rcllock;
			}
			MEM_LOCK(rcl_lock);
			MEM_LOCK(lock_ptr);
			if (fio->class == CLASS_SDS)
				loclink = sds_info->loclist;
			else
				loclink = mr_info->loclist;
			locptr = (_lociosw *)uiostat->sw_sptr;
			if (loclink != NULL && locptr != NULL) {
				if (locptr->user_sw == uiostat) {
/*
 *					Unlock lock_ptr, so that other
 *					tasks may issue i/o while we are
 *					in recall. rcl_lock stays 
 *					locked, since 2 tasks recalling 
 *					the same status word would cause
 *					problems.
 */
					MEM_UNLOCK(lock_ptr);
	                                ret = XRCALL(llfio,fcntlrtn)
                                          llfio, cmd, &locptr->local_sw,
					  iostat);
					MEM_LOCK(lock_ptr);
					if (FFSTAT(locptr->local_sw) != 0 &&
					   locptr->local_sw.sw_flag == 1) {
						uiostat->sw_sptr = NULL;
						SETSTAT(uiostat,
					   	  FFSTAT(locptr->local_sw),
					   	  ((locptr->sw_count>>3) + 
					   	  locptr->local_sw.sw_count));
#if defined(_CRAY1) || (defined(_CRAYMPP) && defined(_UNICOS_MAX))
						if (fio->class == CLASS_SDS)
				    	   		_sds_locsw_clear(sds_info,locptr);
						else
#endif
				    	   		_mr_locsw_clear(mr_info,locptr);
			    		}
				   	MEM_UNLOCK(lock_ptr);
					MEM_UNLOCK(rcl_lock);
					return(ret);
				}
			}
			MEM_UNLOCK(lock_ptr);
		        MEM_UNLOCK(rcl_lock);
			break;

		case FC_SETRECL:
		case FC_GETTP:

			if (llfio == NULL)
				ERETURN(iostat, FDC_ERR_NOSUP, 0)
			else
				ret = XRCALL(llfio,fcntlrtn)
					llfio, cmd, arg, iostat);
			break;

		case FC_SCRATCH: {
			int *sflgsp;

			if (fio->class == CLASS_SDS) 
				sflgsp = &sds_info->scrtch_flgs;
			else
				sflgsp = &mr_info->scrtch_flgs;

			if (*sflgsp != 0)
				ret = 0;
			else if (llfio == NULL) {
				if (fio->rtype == TR_FSS_SCR) 
					*sflgsp = SCR_NOFLUSH |
						  SCR_UNLINKED |
						  SCR_SINGLELINK;
				else
					/* shouldn't get here */
					ERETURN(iostat, FDC_ERR_NOSUP, 0)
			}
			else {
				ret = XRCALL(llfio,fcntlrtn) llfio, cmd, sflgsp,
					iostat);

				if (ret < 0)
					*sflgsp = 0;
				else if (fio->rtype == TR_FSS_SCR) 
					*sflgsp |= SCR_NOFLUSH;
			}

			*(int*)arg = *sflgsp;
			break;
		}
		default:
/*
 *			If we get a user or site command code and there is
 *			no lower level layer, then we haven't a clue
 *                      as to what to do, and return an error.
 */
			if ((llfio != NULL) && IS_RESERVED_FC(cmd))
				ret = XRCALL(llfio,fcntlrtn)
					llfio, cmd, arg, iostat);
			else
				ERETURN(iostat, FDC_ERR_NOSUP, 0)
			break;
	}
	return(ret);
}
