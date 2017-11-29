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


#pragma ident "@(#) libu/util/c1/char.c	92.1	07/07/99 13:22:26"

#include <string.h>
#include <fortran.h> 	

_charlen(f)
_fcd *f;
{
	return(_fcdlen(*f));	/* return length */
}
 
/* 
 * This routine copies from f to s. Trailing blanks are eliminated
 */
_charcpy(s, f, len)
char *s;
_fcd *f;		/* fortran character descriptor */
int len;
{
	int wrdlen;
	char *t;
	char *c;

	t = _fcdtocp(*f);
	c = t+len-1;
	for (wrdlen = len; *c == ' '; c-- ){ 
		wrdlen--;
	}
	(void) strncpy( s, t, wrdlen);
	s = s + wrdlen;
	*s = '\0';

}

/*
 * This routine copies from f to s. Trailing blanks are
 * eliminated. If the length of the string is > maxlen,
 * -1 is returned; otherwise 0 is returned.
 */
_charncpy(s, f, len, maxlen)
char *s;
_fcd *f;
int len;
int maxlen;
{
	int wrdlen;
	char *t;
	char *c;

	t = _fcdtocp(*f);
	c = t+len-1;
	for (wrdlen = len; *c == ' '; c-- ){ 
		wrdlen--;
	}
        if (wrdlen > maxlen)
	{
		return(-1);	/* error */
	}
	(void) strncpy( s, t, wrdlen);
	s = s + wrdlen;
	*s = '\0';
	return(0);
}
 
