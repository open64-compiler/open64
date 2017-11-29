
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

/* additional single-precision forms */

extern float    hypotf(float, float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (hypotf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    truncf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (truncf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    expf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (expf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    tanhf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (tanhf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    sinhf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sinhf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    coshf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (coshf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    tanf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (tanf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    sinf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sinf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    cosf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (cosf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    atan2f(float, float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (atan2f)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    atanf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (atanf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    asinf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (asinf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    acosf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (acosf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    logf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (logf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    log10f(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (log10f)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    sqrtf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sqrtf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    ceilf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (ceilf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float    floorf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (floorf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#ifdef _LITTLE_ENDIAN
#       define D(h,l) l,h
#endif
#ifdef _BIG_ENDIAN
#       define D(h,l) h,l
#endif

