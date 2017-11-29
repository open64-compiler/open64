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


#pragma ident "@(#) libu/ffio/blxfcntl.c	92.2	08/18/99 14:45:43"

#include <ffio.h>
#include "blxio.h"

/*
 * blankx fcntl requests
 *
 * Parameters:
 *	fio	- Pointer to fdinfo block
 *	cmd	- command code
 *	arg	- command specific parameter
 *	stat	- pointer to status return word
 */
_blx_fcntl(fio, cmd, arg, stat)
struct fdinfo *fio;
int cmd; 
void *arg;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct ffc_info_s *ffcp, locinfo;
	int ret;

	llfio = fio->fioptr;

	ret = 0;
	switch(cmd)
		{
		case FC_GETINFO:
			ffcp = (struct ffc_info_s *)arg;
			ret = XRCALL(llfio,fcntlrtn)
				llfio, FC_GETINFO, &locinfo, stat);

			/* flags are from lower layer */
			ffcp->ffc_flags = locinfo.ffc_flags;
/*
 *			This layer effectively makes it impossible to do
 *			certain things.  Turn those bits off.
 *			Other capabilities are dependent solely upon
 *			the lower level layers.
 */
			ffcp->ffc_flags &= ~(
				FFC_SEEKA |	/* can NOT seek absolute */
				FFC_SEEKR |	/* can NOT seek relative */

				FFC_FIXD |	/* can NOT do fixed len recs */
				FFC_BINARY |	/* can NOT do binary */
				FFC_CANLISTIO |	/* can NOT do listio */
				0);

			ffcp->ffc_gran = 8;	/* granularity is 8 bits */
			ffcp->ffc_reclen = 0;	/* no rec length */
			ffcp->ffc_fd = locinfo.ffc_fd; /* fd from lower layer */
			break;
		case FC_STAT:
		case FC_SETRECL:
		case FC_GETTP:
/*
 *			get what we can from the lower level
 */
			ret = XRCALL(llfio,fcntlrtn) llfio, cmd, arg, stat);
			break;
		case FC_ASPOLL:
		case FC_RECALL:
			break;
		default:
			if (IS_RESERVED_FC(cmd))
				{
				ret = XRCALL(llfio,fcntlrtn)
					llfio, cmd, arg, stat);
				}
			else
				{
				ERETURN(stat, FDC_ERR_NOSUP, 0);
				}
			break;	
		}
	return(ret);
	}

