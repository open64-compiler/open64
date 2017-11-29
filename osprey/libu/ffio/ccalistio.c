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


#pragma ident "@(#) libu/ffio/ccalistio.c	92.1	06/29/99 13:16:47"


#include <errno.h>
#include <ffio.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/listio.h>
#include <sys/types.h>
#include "ccaio.h"

/*
 * _cca_listio
 *
 *	Issue a listio request for the cachea layer.
 *
 * Return Value:
 *
 *	On success, nreq is returned, and the contents of the stat structure are
 *	unspecified.
 *
 *	If an error in setup is encountered, stat is set as follows:
 *
 *		stat->sw_error	= error code
 *		stat->sw_stat	= FFERR
 *		stat->sw_flag	= 1
 *		stat->sw_count	= 0
 *
 *	If an error in I/O request I is detected, the list[I].li_stat
 *	structure will be set as follows:
 *
 *		list[I].li_stat->sw_error	= error code
 *		list[I].li_stat->sw_flag	= 1
 */
_cca_listio( 
int			cmd,		/* LC_START or LC_START */
struct fflistreq	*list,		/* list of requests (see fflistio) */
int			nreq,		/* number of requests */
struct ffsw		*stat)		/* status structure */
{
	int	ret;
	int	i;
	int	n_handled;
	int	status;
	int	zero;
	int	pos;
	bitptr buf;
	struct ffsw	loc_stat;
	struct fdinfo	*fio;
	struct fdinfo	*oldfio;
	struct cca_f *cca_info;

	n_handled = 0;

	oldfio = GETIOB(list[0].li_fildes);
	cca_info = (struct cca_f *)oldfio->lyr_info;
	for (i = 0; i < nreq; i++) {

		fio = GETIOB(list[i].li_fildes);
		if (fio != oldfio) {
			_SETERROR(list[i].li_status, FDC_ERR_LSTIO, 0);
			continue;
		}
		if ( list[i].li_signo != 0 ) {
			_SETERROR(list[i].li_status, FDC_ERR_REQ, 0);
			continue;
		}

		cca_info = (struct cca_f *)fio->lyr_info;
		CLRFFSTAT(*(list[i].li_status));

		SET_BPTR(buf, CPTR2BP(list[i].li_buf));
		if ( list[i].li_nstride > 1 ) {

			status = _ffcompound(&list[i]);
			if (status == 0)
				n_handled++;
			continue;
		}
		if ( list[i].li_flags == LF_LSEEK ) {
			pos = _cca_seek(fio, list[i].li_offset, SEEK_SET,
					   &loc_stat);
			if (pos == -1) {
				*list[i].li_status = loc_stat;
				continue;
			}
		}
		else if (list[i].li_flags != 0) {
			_SETERROR(list[i].li_status, FDC_ERR_REQ, 0);
		}

		zero = 0;
		status = 0;
		if ( cmd == LC_START ) {
			if ( list[i].li_opcode == LO_READ ) {
				status = _cca_reada(fio, buf, list[i].li_nbyte,
				    		    list[i].li_status, FULL,
						    &zero );
			}
			else if (list[i].li_opcode == LO_WRITE ) {
				status = _cca_writea(fio, buf, list[i].li_nbyte,
				    		     list[i].li_status, FULL,
						     &zero );
			}
			else {
				_SETERROR(list[i].li_status, FDC_ERR_REQ, 0);
			}
		}
		else if ( cmd == LC_WAIT ) {
			if ( list[i].li_opcode == LO_READ ) {
				status = _cca_read(fio, buf, list[i].li_nbyte,
				    		   list[i].li_status, FULL,
						   &zero );
			}
			else if (list[i].li_opcode == LO_WRITE ) {
				status = _cca_write(fio, buf, list[i].li_nbyte,
				    		    list[i].li_status, FULL,
						    &zero );
			}
			else {
				_SETERROR(list[i].li_status, FDC_ERR_REQ, 0);
			}
		}
		else {
			_SETERROR(list[i].li_status, FDC_ERR_REQ, 0);
		}
		if (status == ERR) {
			continue;
		}

		n_handled++;
	}

	return( n_handled );
}


