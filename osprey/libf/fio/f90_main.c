/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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

#include <stdlib.h>


#pragma ident "@(#) libf/fio/main.c	92.1	11/16/99 15:43:33"

#if     defined(__linux)

/*
 * In mixing C and Fortran applications, if 'main' is defined in C
 *  there will be an unresolved symbol 'MAIN__' in __f90_main defined in main.c
 * This file is used to provide a dummy 'MAIN__' implementation.
 */
void __f90_MAIN(void)
{
        /* This function will not be called! */
        abort();
}

#ifdef PATHSCALE_MERGE
typeof(__f90_MAIN) MAIN__ __attribute__((weak, alias("__f90_MAIN")));
#endif

#endif
