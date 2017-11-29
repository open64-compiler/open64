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



typedef struct {
   int             ndims;	/* # of dimensions */
   XINT_TYPE             nels;	/* # of elements */
   XINT_TYPE             baseoff;	/* how to get to (0,...,0) element */
   XINT_TYPE             span[7];	/* span of each dimension +1+ */
}               NAMEDims;		/* dimension descriptor */

 /* +1+ note: only # of dimensions applies here, i.e. actual array is
  * between 0 and 7 elements based on ndims */


typedef struct {
   char            varname[36];	/* name of variable */
   Pointer         varaddr;	/* where it is */
   int             type;	/* its type */
   NAMEDims           *dimp;	/* dimension descriptor */
}               NAMENlentry;	/* namelist entry: 1 for each var */


typedef struct {
   char            nlname[36];	/* name of namelist */
   NAMENlentry         nlvnames[1];	/* array of variable descriptors +2+ */
}               NAMENamelist;

 /* +2+ note: this array is not bounded but is terminated by an entry
  * with a null varname */

