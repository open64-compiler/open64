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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/subout.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include "cmplrs/host.h"

void
subout(char *varn, int32 offset, char *procn, int32 line)
{
	register int32 i;
	extern void _cleanup(void);

	fprintf(stderr, "Subscript out of range on line %d of procedure ", line);
	for(i = 0 ; i < 8 && *procn!='_' ; ++i)
		putc(*procn++, stderr);
	fprintf(stderr, ".\nAttempt to access the %d-th element of variable ", offset+1);
	for(i = 0 ; i < 6  && *varn!=' ' ; ++i)
		putc(*varn++, stderr);
	fprintf(stderr, ".\n");
	fflush(stdout);
	fflush(stderr);
	abort();
}
