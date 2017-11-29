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


#pragma ident "@(#) libu/clib/c1/ssread.c	92.1	07/01/99 12:16:22"

#include  <sys/param.h>
/*
 *	SSREAD - read words from secondary data segment
 */
SSREAD(pds, sds, words )                   
long 	*pds, *sds, *words;
{
	long  n, count;
	char *ermsg ="Error: The number of words to be read by SSREAD must be a multiple of 512\n";
/*
 *	If the number of words to read is zero, simply return.
 */
	if (*words == 0)
		return(0);

	count = *words/512;
	if (count*512 != *words){
		write(2,ermsg,strlen(ermsg));
		return(-1L);
	}
	n = ssread((char*)pds, (char*)*sds, count);
	return(n);
 
}
