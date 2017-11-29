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


#pragma ident "@(#) libu/ffio/cchfcntl.c	92.2	10/07/99 15:40:23"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ffio.h>
#include "cchio.h"

/*
 * cache layer fcntl requests
 *
 * Parameters:
 *	fd	- file descriptor (dummy)
 *	cmd	- command code
 *	arg	- command specific parameter
 *	iostat	- pointer to status return word
 *
 *	Returns: -1 if error occurred
 *		  0 if OK
 */
_cch_fcntl(
struct fdinfo *fio,
int cmd, 
void *arg,
struct ffsw *iostat
)
	{
	struct fdinfo *llfio;
	struct ffc_info_s *ffcp, locinfo;
	struct cch_f *cch_info;
	int ret, llfd;

	cch_info = (struct cch_f *) fio->lyr_info;
	llfio = fio->fioptr;
	ret = 0;

	switch(cmd)
		{
		case FC_GETINFO:
			ffcp = (struct ffc_info_s *)arg;
/*
 *			Only do a call to the lower level if it has been opened
 */
			if (llfio != NULL)
				{
				ret = XRCALL(llfio,fcntlrtn)
					llfio, FC_GETINFO, &locinfo, iostat);
				llfd = locinfo.ffc_fd; /* fd from lower layer */
				}
			else
				{
				locinfo.ffc_flags = 0;
				llfd = -1;
				}

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

			if (cch_info->optflags & CCHOPT_SDS)
				ffcp->ffc_gran = BITPBLOCK;	/* 1 block */
			else
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
				ffcp->ffc_gran = 8;		/* 8 bits */
#else
				ffcp->ffc_gran = 1;		/* 1 bit */
#endif
			ffcp->ffc_reclen = 0;	/* no rec length */
			ffcp->ffc_fd = llfd;
			break;
		case FC_STAT:
/*
 *			Fill in most of the fields from the lower layer.  The
 *			st_size field is copied from fsize.
 */
			ret = XRCALL(llfio,fcntlrtn)
					llfio, FC_STAT, arg, iostat);
			((struct stat *)arg)->st_size = 
					BITS2BYTES(cch_info->fsize);

			break;
		case FC_SETRECL:
		case FC_GETTP:
#ifdef __mips
		case FC_DIOINFO:
#endif
			ret = XRCALL(llfio,fcntlrtn)
					llfio, cmd, arg, iostat);
			break;
		case FC_ASPOLL:
		case FC_RECALL:
			/* all synch for now */
			break;
		default:
			if (IS_RESERVED_FC(cmd))
				{
				ret = XRCALL(llfio,fcntlrtn)
					llfio, cmd, arg, iostat);
				}
			else
				{
				ERETURN(iostat, FDC_ERR_NOSUP, 0)
				}
			break;
		}
	return(ret);
	}
