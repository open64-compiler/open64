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



extern void get_cilist64( cilist64 *a64, cilist *a );
extern void get_inlist64(inlist64 *dst, inlist *src);
extern void get_icilist64(icilist64 *dst, icilist *src);
extern void get_olist64(olist64 *dst, olist *src);


#ifdef NO_XXX64_IN_LIBC
extern long long lseek64(int a, long long b, int c);
extern int fseek64(FILE * a, long long b, int c);
extern long long ftell64(FILE *a);
extern int ftruncate64(int a, long long b);
extern int truncate64(const char *a, long long b);
extern int fstat64(int a , struct STAT_TAG *b);
extern int stat64(const char *a, struct STAT_TAG *b);
#endif
