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


/* USMID @(#) libfi/include/intrin.h	92.0	10/08/98 14:37:14 */
#define TYPE_I	1
#define TYPE_J	2
#define TYPE_S	3
#define TYPE_D	4
#define TYPE_C	5
#define TYPE_Z	6
#define TYPE_L	7
#define TYPE_H	8
#define TYPE_U	9
#define TYPE_W	10

/*
 * Macro for memory allocation -- compatible with malloc()
 */

#define MALLOC	malloc

/*
 * Macro for error processing
 */

#include<liberrno.h>
/* 
 * The call to _ferror will be the real code, after mod is integrated to
 * define the error codes used by these routines
 */
#define ERROR(CODE) _lerror( _LELVL_ABORT, CODE)
