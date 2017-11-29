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


#pragma ident "@(#) libu/util/c1/getlpp.c	92.1	07/07/99 13:22:26"

#include <stdio.h>

/*
 * GETLPP()
 *	Get the LPP environment variable, which determines
 *	the lines-per-page on some applications
 *	Note that decimal is the default interpretation, but a leading
 *	zero, (as in LPP=077) is interpreted as octal and 0x77 is
 *	interpreted as hex.
 *
 *	If LPP is not set, 60 is returned.
 *
 *	Negative LPP values are interpreted as an error, and the
 *	default value of 60 is returned
 */
int
GETLPP()
	{
	int res;
	char *cp;
	extern char *getenv();

	cp = getenv("LPP");
	if (cp == NULL)
		res = 60;
	else
		{
		res = strtol(cp, (char **)NULL, 0);
		if (res < 0)
			res = 60;
		}
	return(res);
	}
