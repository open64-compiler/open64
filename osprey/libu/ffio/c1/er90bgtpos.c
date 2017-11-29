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


static char USMID[] = "@(#) libu/ffio/c1/er90bgtpos.c	92.0	10/08/98 14:57:41";


#include  <sys/types.h>
#include  <sys/file.h>
#include  <sys/param.h>
#include  <fcntl.h>
#include  <tapereq.h>
#include  <stdio.h>
#include  <malloc.h>
#include  <errno.h>
#include  <ffio.h>
#include  "er90by.h"

/*
 *	Get tape position for ER90 byte stream
 *	Returns: 0 if OK
 *		-1 if error (errno is set)
 */

_er90b_gettp(struct fdinfo *fio, long *pa, long palen, long synch)
{
	ER90BYT *f;
	struct tsdata	tsdata;
	int 		vsn[MAXVSN];

	f = (ER90BYT *)fio->lyr_info;
	if ( f->tpos ) {
		if( _tape_tpwait( f->fd, &(f->tpos) ) != 0)
			return(-1); 
	}

	if (synch == 1 && fio->rwflag == WRITIN) {
		if(_tape_sync(f->fd) != 0)
			return(-1);
	}

	if( _tape_tptsi (&(f->tsireq), &tsdata, vsn) != 0)
		return(-1);
	_tape_gtpos(&tsdata, vsn, 0, pa, palen);
	return(0);
}
