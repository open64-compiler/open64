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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/backward.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.3	*/
/* who	ref.	date		description		      */
/* sjc	#1677	25Nov87		Provide dummy f77vms_flag_ for backward */
/*				compatibility with pre-1.21 main.o files*/

/*
  We provide a common f77vms_flag_ so that a main.o file compiled prior to
  1.30 will not get link errors with a 1.30 library. Here's the checkered
  history of this flag:
  
   MIPS1.21:
     f77vms_flag (no trailing underscore)
     main.o exports it as large/small init data
     libI77.a imports it as small undefined (often fails if main.o exported
       it as large undefined)
  
   MIPS1.30-and-later, SGI1.21-and-later:
     f77vms_flag_
     main.o exports it as large/small init data
     libI77.a imports it as undefined
  
Compatibility matrix:
  
					libI77.a
					--------
  main.o		1.10	MIPS1.21	SGI1.21		1.30
  ------		----	--------	-------		----
  1.10			ok	undef		backup.o	backup.o
  MIPS1.21		novms	ok		novms		novms
  SGI1.21		novms	novms		ok		ok
  1.30			novms	novms		ok		ok
  
  ok:		Works as expected
  novms:	Links, but ignores any -vms specified when main.o compiled.
  undef:	Link gets "undefined" error.
  backup.o:	Works as expected, due to this file.
  
*/

/* 
 * This makes a small common symbol which will satisfy libI77's desire to
 * import one. The linker will ignore this if main.o exported one, and
 * thus no large/small conflicts occur if main.o was compiled with -G 0.
 * 
 * See how much trouble one little flag can cause?
 */

#define VMS_FLAGS	4
#include "vmsflags.h"
unsigned short  f77vms_flag_[VMS_FLAGS];

void
__set_f77vms_flag_( flag_arr )
    unsigned short flag_arr[VMS_FLAGS];
{
    int i;
    for (i=0; i<VMS_FLAGS; i++)
	f77vms_flag_[i] = flag_arr[i];
}
