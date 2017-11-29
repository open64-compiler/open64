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


static char USMID[] = "@(#) libf/tape/c1/trace.c	92.0	10/08/98 14:30:10";

/*
 *	f_trace - enter a line into trace file
 */

#ifdef TRACE

#include	<stdio.h>

#define		TRACESZ		4*4096	

char *tracefn  = "libio.trace";
FILE *tracelog = NULL;
char *tracend  = "<<END>>";

f_trace(s, p1, p2, p3)

char	s[];

{
	int ick;

	if ( tracelog == NULL ) {
		(void) unlink(tracefn);
		tracelog = fopen(tracefn,"w+");
		if ( tracelog == NULL ) {
			printf(" trace open failed \n");
			abort();
		}
	}

	fprintf(tracelog,"%8.8s %22.22o %22.22o %22.22o\n",
		s, p1, p2, p3 );
					
	if ( (TRACESZ != 0) && (tracelog != stdout) ) 
		if (ftell(tracelog) > TRACESZ )
			fseek(tracelog, 0, 0);

	if ( tracelog != stdout ) {
		ick = ftell(tracelog);
		fprintf(tracelog,"%8.8s %22.22o %22.22o %22.22o\n",
			tracend, 0, 0, 0);
		fseek(tracelog, ick, 0);
	}

	fflush(tracelog);

}

#endif

