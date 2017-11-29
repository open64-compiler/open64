/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/evntlistio.c	92.2	10/07/99 22:15:19"


#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/iosw.h>
#include <sys/listio.h>
#endif
#include <ffio.h>
#include "listio_mips.h"
#include "evntio.h"

/*
 * _evnt_listio
 *
 * Log a listio operation.
 *
 * Input:
 *	cmd    - command  LC_START or LC_WAIT
 * 	list   - pointer to array of structures (listreq) that describes
 *               request
 *	nreq   - number of I/O requests
 *
 * Output:
 *      ret     - return value from flushrtn
 */
int
_evnt_listio(int cmd, struct fflistreq *list, int nreq, struct ffsw *iostat)
{
	fprintf(stderr, "*** Notice: the Cray event layer is not currently supported.\n");
	fprintf(stderr, "*** Please contact PathScale if you need this functionality.\n");
	abort();
}
