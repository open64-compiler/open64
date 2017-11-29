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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/s_cat.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

/* 11/9/89 fix bug 5242 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <alloca.h>
#include "cmplrs/host.h"

extern void s_abort(int32);
#ifndef FTN90_IO
extern void f77fatal (int32, char *);
#endif

void s_cat(string lp, string rpp[], fsize_t rnp[], int32 *np, fsize_t ll)
{
    int32 i, n;
    fsize_t nc;
    string buf=0;
    fsize_t len;
    
    buf = alloca(ll);
    n = *np;
    len = 0;
    for(i = 0 ; i < n ; ++i) {
	nc = (ll-len <= rnp[i]) ? (ll-len) : rnp[i];
	nc = nc < 0 ? 0 : nc;
	memcpy (&buf[len], rpp[i], nc);
	len += nc;
    }
    memcpy(lp,buf,len);
    memset(&lp[len],' ',ll-len);  /* Why not use b_pad? */
}
#ifdef sgi
/* Not used by Ragnarok Fortran, see s_cat_kai.c */

#ifndef FTN90_IO
	/* not used by fortran 90 */
void
s_cat_tmp(char **lp, char *rpp[], int32 rnp[],
	  int32 *np, int32 *ntemp)
{
    int32 i, n;
#define BUFSIZE	8192
    static char buf[BUFSIZE];
    static int32 len_used = 0;
    int32 len;
    char *tmpbuf;
    static struct bufstruct { 
	struct bufstruct *next;
	char buf[1];
    } *bigbuf = 0, *nxtbuf;
    
    n = *np;
    for (i=0, len=0; i<n; i++)
	len += rnp[i];


    if (*ntemp == 0) {
	/* first temporary string in a statement.  Reset all values and
	   free allocated space.  Do this for subroutines only since functions
	   can be recursive in its use of character concatenation.  It's rather
	   dumb to distinguish subroutine/function but this seems to be the
	   most runtime efficient and safe way to do it.
	   */
	len_used = 0;
	while (bigbuf) {
	    nxtbuf = bigbuf->next;
	    free(bigbuf);
	    bigbuf = nxtbuf;
	}
    }
    if (len + len_used > BUFSIZE) {
	nxtbuf = (struct bufstruct *) malloc(len + 4);
	if (!nxtbuf)
	    f77fatal(113,"s_cat_tmp");
	nxtbuf->next = bigbuf;
	bigbuf = nxtbuf;
	tmpbuf = bigbuf->buf;
    } else {
	tmpbuf = &buf[len_used];
	len_used += len;
    }
    for(i = 0, len = 0; i < n ; ++i) {
	memcpy (&tmpbuf[len], rpp[i], rnp[i]);
	len += rnp[i];
    }
    *lp = tmpbuf;
}
#endif /* FTN90_IO */
#endif

void s_cat2(string tp, fsize_t tl, string ap, string bp, fsize_t al, fsize_t bl)
{
  if (al + bl <= tl) {
    /* sources same size as target or smaller than target */
    memcpy(tp,ap,al);		/* copy in first source */
    memcpy(tp+al,bp,bl);	/* copy in second source */
    if (al + bl < tl) {
      /* sources smaller than target */
      memset(&tp[al+bl],' ',tl-(al+bl));  /* blank pad rest of string */
    }
  } else {
    /* al + bl > tl -- sources larger than target */
    if (al <= tl) {
      /* first source same size as target or larger than target */
      memcpy(tp,ap,tl);		/* copy in first tl bytes of first source */
    } else {
      /* first source smaller than target */
      memcpy(tp,ap,al);		/* copy in first source */
      memcpy(tp+al,bp,tl-al);	/* copy in part of second source that fits */
    }
  }
}
