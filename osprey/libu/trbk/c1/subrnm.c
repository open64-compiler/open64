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


#pragma ident "@(#) libu/trbk/c1/subrnm.c	92.1	07/01/99 13:48:28"
#include <string.h>
#include <cray/stk.h>

#define  BTOW(x)   (((x)+7)>>3)
#define  EOS	   '\0'
#define  SUCCESS   0
#define  FAILURE   1
#define  MAX_ENT_LEN 32

/*
 *	Return "subname" as the name to which "fpptr" refers.
 *	"fpptr" is the pointer to the caller's stack frame.
 *	If "fpptr" is in range, _subrnm() returns SUCCESS(0),
 *	otherwise it returns FAILURE(1).
 */

long 
_subrnm(fpptr, subname, baseaddr, argcnt, lineno, la)
struct stack_frame *fpptr;
char subname[];
long *baseaddr, *argcnt, *lineno;
long la;
{
	struct TNB *tnb;
	struct arlst *arlst;
	int nchars;
	char *ptr;

	if (la == 0)
		la = sbreak(0);
	if ((long)fpptr < 4 || (long)fpptr >= la) {
		subname[0] = EOS;
		return (FAILURE);
	}
	tnb = fpptr->tnb;
	*baseaddr = (long)tnb->entry;
	arlst = (struct arlst *)fpptr->prevargs;
	*argcnt = arlst->nargs;
	*lineno = arlst->lineno;
	nchars = tnb->nmlen;
	ptr = (char *)((long *)tnb - BTOW(nchars));
	if (nchars > MAX_ENT_LEN - 1)
		nchars = MAX_ENT_LEN - 1;
	(void) strncpy(subname, ptr, nchars);
	subname[nchars] = EOS;
	return (SUCCESS);
}
