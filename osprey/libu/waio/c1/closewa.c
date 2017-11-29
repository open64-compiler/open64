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


#pragma ident "@(#) libu/waio/c1/closewa.c	92.1	08/19/99 14:46:33"

#include  <errno.h>
#include  <stdlib.h>
#include  <sys/types.h>
#include  <sys/listio.h>
#include  <malloc.h>
#include  <ffio.h>
#include  "../waio.h"
 
/*
 *	closewa - close word addressable dataset   
 */
CLOSEWA(index)
long *index;
{
	WAFIL *f;
	struct fdinfo *fio;
 
	f = wafils + (*index-1);		/* pointer to WAFIL table */
	fio = GETIOB(f->wa_fd);

	if (f->wa_buffer != (char *)0)
		{
	  	(void)free(f->wa_buffer);	/* free the buffer space */
		f->wa_buffer = 0;
		}

	if (f->wa_fdc)
		{
		if (XRCALL(fio, closertn) fio, &f->wa_iosw) != 0)
			_lerror(_LELVL_ABORT, f->wa_iosw.sw_error);
		}
	else if (close(f->wa_fd) < 0)
		_lerror(_LELVL_ABORT, errno);
	f->wa_fd = 0;  
	if (f->wa_up != NULL)
		_clruptr(f->wa_up);
	return(0);
 
}
