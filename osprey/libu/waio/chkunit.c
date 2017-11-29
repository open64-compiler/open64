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


#pragma ident "@(#) libu/waio/chkunit.c	92.1	06/23/99 13:55:03"

#include <errno.h>
#include "waio.h"

/*
 *	chkunit - check if I/O is complete on dataset
 *
 *	entry:
 *		*index	1-based FIT table index
 *	exit:
 *		*istat	0 if the file has uncompleted I/O activity
 *			1 if all I/O activity for the file has completed
 */
void
CHKUNIT(index,istat)
long *index, *istat;
{
	WAFIL *f;
 
	f = &wafils[*index-1];		/* pointer to WAFIL table */

        if ( WAIO_BUSY(f) ) {
		*istat = 0;
		return;
	}
	*istat = 1;	/*  i/o has been completed  */
/*
 *      Check for errors on completed asynchronous request.
 */
        if ( WAIO_ERROR(f) )
		_errwa("IODONE", "Fatal I/O error on", f, f->wa_iosw.sw_error);

}
