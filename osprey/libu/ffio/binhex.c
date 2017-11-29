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


#pragma ident "@(#) libu/ffio/binhex.c	92.1	06/29/99 13:16:47"

#include <stdlib.h>
#include <cray/portdefs.h>
#include "spec_parse.h"

#define HEX_DIGITS	(sizeof(int)*2)

static char hexdigits[] =
	{'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};
static int digihex[] =
	{0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,0,10,11,12,13,14,15};

/*
 * hex2bin converts HEX_DIGITS hex digits to one cray word 
 */
long
_hex2bin(char *str)
	{
	long tword;
	int i;

	tword = 0;
	for (i = 0 ; i < HEX_DIGITS ; i++)
		{
		tword = tword << 4;
		if (str[i] < '0' || str[i] > 'F') abort();
		tword = tword | digihex[str[i] - '0'];
		}
	return(tword);
	}

/*
 * bin2hex converts one cray word to hex format and puts the HEX_DIGITS chars
 * at the location specified
 */
void
_bin2hex(char *str, unsigned long num)
	{
	int i;

	for (i = 0 ; i < HEX_DIGITS ; i++)
		{
		num = _dshiftl(num,num,4);
		*str = hexdigits[num & 0xF];
		str++;
		}
	}

/*
 * Copy a name terminated by space, newline, tab, or null.
 */
int
_cpyname(char *target, char *src)
	{
	int i;

	i = 0;
	while ( *src != ' '  && /* space */
		*src != '\t' && /* tab */
		*src != '\n' && /* newline */
		*src != '\0')   /* NULL */
		{
		*target = *src;
		target++;
		src++;
		i++;
		}
	*target = '\0';
	return(i);
	}


