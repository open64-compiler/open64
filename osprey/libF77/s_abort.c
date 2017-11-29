/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/s_abort.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

/* called when a fatal error wishes to terminate execution */

#include <stdio.h>
#include "cmplrs/host.h"
#include <stdlib.h>

extern int up_low(char);

void
s_abort(int32 errno)
{
char *dumpflag, *getenv();
int32 coredump=0;
#ifndef FTN90_IO
void _cleanup();
#endif

	if (dumpflag = getenv("f77_dump_flag")) {
		coredump = up_low(*dumpflag) == 'y' ? 1 : 0;
	}

	if (coredump) {
#ifndef FTN90_IO
		_cleanup();
#endif
		abort();			/* cause a core dump */
	} else {
#ifndef FTN90_IO
		_cleanup();
#endif
		fprintf(stderr,"*** Execution Terminated (%d) ***\n",errno);
		exit(errno);
	}
}
