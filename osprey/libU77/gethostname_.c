/*

  Copyright (C) 1999-2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  Further, any
  license provided herein, whether implied or otherwise, is limited to 
  this program in accordance with the express provisions of the 
  GNU Lesser General Public License.  

  Patent licenses, if any, provided herein do not apply to combinations 
  of this program with other product or programs, or any other product 
  whatsoever.  This program is distributed without any warranty that the 
  program is delivered free of the rightful claim of any third person by 
  way of infringement or the like.  

  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/

/* --------------------------------------------------- */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/gethostname_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 * gethostname - return this machines hostname
 *	@(#)gethostname_.c	1.1
 *
 * synopsis:
 *	integer function gethostname (name, len)
 *	character(*) name
 *      integer      len
 *
 * where:
 *	name	will receive the host name
 *	The returned value will be 0 if successful, an error number otherwise.
 */

#include <errno.h>
#include <unistd.h>
#include "externals.h"

extern int
gethostname_ (char *name, int *len, int ftnlen)
{
	char	buf[64];
	int	blen	= sizeof buf;

	if (gethostname (buf, blen) == 0)
	{
		b_char (buf, name, *len);
		return (0);
	}
	else
		return(-1);
}
