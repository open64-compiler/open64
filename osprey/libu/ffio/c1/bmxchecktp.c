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


static char USMID[] = "@(#) libu/ffio/c1/bmxchecktp.c	92.0	10/08/98 14:57:41";


#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include <fcntl.h>
#include <tapereq.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <sys/bmxctl.h>
#include <sys/dpacket.h>
#include "bmxio.h"

/*
 * Make special end of volume routines soft externals. They
 * will be loaded by SETSP.
 */

#pragma soft _bmx_wait
extern int _bmx_wait();

/*
 *	_bmx_checktp - Checks tape I/O status
 *
 *      f - BMFIL pointer
 *
 *	istat
 *	  -1  No status
 *         0  EOV
 *	   1  tape off reel
 *         2  tape mark detected
 *	   3  blank tape detected
 *
 *      icbuf 
 *	   Number of blocks in library buffers (always 0)
 */

_bmx_checktp(BMXFIL *f,  long *istat, long *icbuf)
{
	struct tsdata tsdata;
	int  ret;


	if (f->bmx_flag & BMXIO_EOV)
		ret = _bmx_wait(f);	/* Wait for i/o to be quiet */
	else
		ret = 0;

	*icbuf = 0;

	if ((f->bmx_flag & BMXIO_SPPROC)||(f->bmx_flag & BMXIO_ERRSEEN) ) {
/*
 *		If we are in special processing, or if our last i/o
 *		returned an error, we need to get information about
 *		blank tape, and tape off reel from the system.
 */
		if(_tape_tptsi(&(f->tsireq), &tsdata, NULL)){
			return(-1);
		}
		if (tsdata.ts_dst & STS_LDTA)
			*istat = 1;	/* tape off reel */

/*
 *		If we are in special processing, get EOV status from
 *		the system. This is because once we enter special processing,
 *		the libraries no longer get an EOV error from writes or reads.
 */
		else if ((f->bmx_flag & BMXIO_SPPROC) &&((tsdata.ts_dst & STS_EOT) != 0))
			*istat = 0; 	/* EOV */
		else if (!(f->bmx_flag & BMXIO_SPPROC) && 
		  (f->bmx_flag & BMXIO_EOVSEEN))
			*istat = 0; 	/* EOV */
		else if (tsdata.ts_dst & STS_BTP)
			*istat = 3;	/* blank tape detected */
		else if (f->bmx_flag & BMXIO_TPMK)
			*istat = 2;	/* tape mark read */
		else
			*istat = -1;	/* no status */
	}
	
	else if (f->bmx_flag & BMXIO_EOVSEEN)
		*istat = 0;	/* eov */
	else if (f->bmx_flag & BMXIO_TPMK)
		*istat = 2;	/* tape mark read */
	else
		*istat = -1;	/* no status */
	if ((ret == -1) && (*istat != 3) && (*istat != 1)){
			/* _bmx_wait failed, and the cause was not */
			/* blank tape or tape off reel */
		return(-1);
	}
	return(0);
}
