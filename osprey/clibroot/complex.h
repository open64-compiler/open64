/* USMID @(#)include/complex.h	100.0	07/11/97 00:26:19 */


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

#ifndef _COMPLEX_H
#define _COMPLEX_H

/* The complex data type is supported by scc 2.0 or later. */
#if __STDC__  && _RELEASE >= 2

#define complex   _Complex

#if _RELEASE >= 4
#define CMPLXF(x,y)	(((float)x)_Cmplx_((float)y))
#define CMPLX(x,y)	(((double)x)_Cmplx_((double)y))
#define CMPLXL(x,y)	(((long double)x)_Cmplx_((long double)y))
#else
#define CMPLXF(x,y)	((float)(x) + (float)(y) * 1.0fi) 
#define CMPLX(x,y)	((double)(x) + (double)(y) * 1.0i)
#define CMPLXL(x,y)	((long double)(x) + (long double)(y) * 1.0Li)
#endif

#include <sys/cdefs.h>

__BEGIN_DECLS
extern double cabs __((double complex _X));
extern double complex ccos __((double complex _X));
extern double complex cexp __((double complex _X));
extern double cimag __((double complex _X));
extern double complex clog __((double complex _X));
extern double complex conj __((double complex _X));
extern double complex cpow __((double complex _X, double complex _Y));
extern double creal __((double complex _X));
extern double complex csin __((double complex _X));
extern double complex csqrt __((double complex _X));
__END_DECLS

#endif /* __STDC__ && _RELEASE >= 2 */

#endif /* !_COMPLEX_H */
