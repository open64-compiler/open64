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


static char USMID[] = "@(#) libu/ffio/c1/bmxrewind.c	92.0	10/08/98 14:57:41";


#include <fcntl.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/iosw.h>
#include <sys/bmxctl.h>
#include <sys/file.h>
#include <tapereq.h>
#include <errno.h>
#include "bmxio.h"

/*
 *	rewind on-line tape file
 *	Returns: 0 if everything is OK
 *		-1 if error (errno is set)
 */

_bmx_rewd(BMXFIL *f) 
{
 

	if (_bmx_prepos(f) != 0)
		return(-1);
/*
 * 	Issue rewind request
 */ 
	if ( _tape_rewd( f->bmx_fd ) == -1){
		return(-1);
	}
	f->bmx_tpos = TPOS_REWIND;
	f->bmx_flag &= ~(BMXIO_EOVSEEN);
	f->bmx_flag &= ~(BMXIO_TPMK);
	return(0);

}
/*
 * 	Do necessary stuff before we issue a positioning request
 *	Returns: 0 if everything is OK
 *		-1 if error (errno is set)
 */
_bmx_prepos(BMXFIL *f)
{
	if ( f->bmx_tpos ) { 
		if( _tape_tpwait( f->bmx_fd, &(f->bmx_tpos) ) != 0)
			return(-1);
	}
/*
 *	Make sure that active i/o is quiet before doing rewind 
 */ 
	if( _bmx_quiet(f) < 0){
		/* Ignore read-ahead errors */
		if (f->bmx_flag & BMXIO_RW)
			return(-1);	
	}
/*	
 *	If previous i/o request was a write request make sure the list
 *	is flushed before issuing the rewind.  
 */
	if (__bmxflush(f) < 0){
		return(-1);
	}

/*
 * 	Clear any errors - for example, read ahead may have 
 * 	read a tapemark
 */
	if (_bmx_clrerr(f) < 0)
		return(-1);
	_bmx_clear(f);		/* Clear all lists */
	f->bmx_bufptr  = 0;	/* indicate empty buffer */
	f->bmx_recl   = 0;	/* and clear record length */
	return(0);
}
