/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/d_prod.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

double d_prod(float *x, float *y)
{
  return((double)(*x)*(double)(*y));
}

#ifdef KEY /* Bug 2623 */
/*
 * The standard Fortran "dprod" intrinsic takes real*4 args and returns
 * a real*8 result. The __q_prod function in file mips/quad/q_prod.c
 * implements an extension which takes real*8 args and returns a real*16
 * result. Since we don't allow real*16, but we do provide an option -r8 which
 * promotes real*4 to real*8 but leaves real*8 alone, it seems user-unfriendly
 * not to provide a version of "dprod" which works with -r8. Here it is. If
 * we ever support real*16 and add the mips/quad files to the shared library,
 * the linker will give a "duplicate symbol error" to remind us to remove this
 * version.
 *
 * (The crayfe/fe90 sources use a cpp symbol _TARGET_OS_LINUX to enable code
 * which knows that real*16 is disallowed, but we can't use that symbol here
 * because it is defined within the front end itself.)
 */
double __q_prod(double *x, double *y)
{
  return (*x)*(*y);
}
#endif /* KEY Bug 2623 */
