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


static char USMID[] = "@(#) libu/ffio/c1/bmxweof.c	92.0	10/08/98 14:57:41";


#include <sys/types.h>
#include <sys/iosw.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <ffio.h>
#include "bmxio.h"


/*
 * _bmx_weof()
 *	If we can write a tape mark, do so. Otherwise,
 *	return an error.
 */
_bmx_weof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
int ret;

	fio->last_recbits = fio->recbits;
	fio->recbits = 0;

	if ( !(((BMXFIL *)fio->lyr_info)->bmx_rwtpmk) )
		ERETURN(stat, FDC_ERR_NWEOF, 0);

	ret = __bmxwrite( fio->lyr_info, NULL, NULL, BMX_EOF );
	if ( ret < 0 )
		ERETURN(stat, errno, 0);
	fio->rwflag = WRITIN;
	fio->ateof = YES;
	fio->ateod = NO;
	SETSTAT(stat, FFEOF, 0);
	return(ret);
}
