/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


static char *Version = "$Source: /home/bos/bk/kpro64-pending/common/util/SCCS/s.mstack.c $ $Revision: 1.5 $";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "mstack.h"

static int getsp ( int a );
static int fra ( int a );
static char *savestr ( char *str );
#ifndef MONGOOSE_BE
static void make_ftab ( void );
#endif /* MONGOOSE_BE */
static struct frec *search_in_ftab ( int adr );


#if A_UX

struct x {
  struct x *next;
};

stack_lev(b)
  int b;
{
  struct x *l = (struct x *) (((int)(&b)) - 8);
  int a = 0;
  while (l) {
    a++;
    l = l->next;
  }
  return a;
}

trace_stack(a, b)
{
  return stack_lev()-1;
}

#else	/* not A_UX */

char **__Argv;

/*ARGSUSED*/
int trace_stack(a, b)
  int a;
  int b;
{
  return 1;	/* not implemented */
}

#endif	/* not A_UX */

