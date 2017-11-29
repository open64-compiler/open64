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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/lib.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include <string.h>
#include <cmplrs/fio.h>

int
setcilist (cilist *x, int u, char *fmt, int rec, int xerr, int end)
{
   x->ciunit = u;
   x->cifmt = fmt;
   x->cirec = rec;
   x->cierr = xerr;
   x->ciend = end;
   return 0;
}

int
setolist (olist *x, int xunit, char *fname, char *sta, char *fm, int rl, char *blnk, int oerr)
{
   x->oerr = oerr;
   x->ounit = xunit;
   x->ofnm = fname;
   x->ofnmlen = (int) strlen (fname);
   x->osta = sta;
   x->ofm = fm;
   x->orl = rl;
   x->oblnk = blnk;
   return 0;
}

int
stcllist (cllist *x, int xunit, char *stat, int cerr)
{
   x->cerr = cerr;
   x->cunit = xunit;
   x->csta = stat;
   return 0;
}

int
setalist (alist *x, int xunit, int aerr)
{
   x->aunit = xunit;
   x->aerr = aerr;
   return 0;
}

int
setcilist64 (cilist64 *x, ftnint u, char *fmt, XINT64 rec, int xerr, int end)
{
   x->ciunit = u;
   x->cifmt = fmt;
   x->cirec = rec;
   x->cierr = xerr;
   x->ciend = end;
   return 0;
}

int
setolist64 (olist64 *x, ftnint xunit, char *fname, char *sta, char *fm, int rl, char *blnk, int oerr)
{
   x->oerr = oerr;
   x->ounit = xunit;
   x->ofnm = fname;
   x->ofnmlen = (int) strlen (fname);
   x->osta = sta;
   x->ofm = fm;
   x->orl = rl;
   x->oblnk = blnk;
   return 0;
}

/* #pragma weak stcllist64 = stcllist */

/* #pragma weak setalist64 = setalist */

