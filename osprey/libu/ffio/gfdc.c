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


#pragma ident "@(#) libu/ffio/gfdc.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <string.h>
#include <ffio.h>

/*
 * Extract the FDC specification words from the option
 *	string in the file characteristics.
 * This routine allocates memory from the heap and
 * returns a pointer to the array of spec words terminated
 * by a zero word.
 */
union spec_u *
_g_fdc_spec(entry)
char *entry;
	{
	char *opts, *specstr;
	extern char *_g_fchar_opts();
	extern char *_g_fchar_opt();
	extern union spec_u *_dec_fdc_spec();

	opts = _g_fchar_opts(entry);
	if (opts == NULL)
		return(NULL);

	specstr = _g_fchar_opt(opts, 'F');
	if (specstr == NULL)
		return(NULL);
	return(_dec_fdc_spec(specstr));
	}

union spec_u *
_dec_fdc_spec(specstr)
char *specstr;
	{
	int wl, toklen, i;
	union spec_u *words;
	long _hex2bin();
/*
 *	Spec string is terminated by a trailing space.  Find the space
 *	to determine the length
 */
	toklen = strchr(specstr, ' ') - specstr;
	wl = toklen / 17;		/* number of words in spec */
	words = (union spec_u *)malloc(wl*8 + 8);
	i = 0;
	do
		{
		words[i].wword = _hex2bin(specstr);
		i++;
		specstr += 17;	/* skip '+' too */
		} while(i < wl);
	words[i].wword = 0;
	return(words);
	}
