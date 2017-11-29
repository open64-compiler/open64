
/*
*  Copyright (C) 2008-2009 Advanced Micro Devices, Inc. All Rights Reserved.
*
*  This file is part of libacml_mv.
*
*  libacml_mv is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  libacml_mv is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with libacml_mv.  If not, see
*  <http://www.gnu.org/licenses/>.
*
*/


#include "../inc/libm_amd.h"
#include "../inc/libm_util_amd.h"

#define USE_VALF_WITH_FLAGS
#define USE_NAN_WITH_FLAGS
#define USE_HANDLE_ERRORF
#include "../inc/libm_inlines_amd.h"
#undef USE_VALF_WITH_FLAGS
#undef USE_NAN_WITH_FLAGS
#undef USE_HANDLE_ERRORF

#include "../inc/libm_errno_amd.h"

#ifndef WINDOWS
/* Deal with errno for out-of-range argument */
static inline float retval_errno_edom(float x)
{
  struct exception exc;
  exc.arg1 = (float)x;
  exc.arg2 = (float)x;
  exc.name = (char *)"atanf";
  exc.type = DOMAIN;
  if (_LIB_VERSION == _SVID_)
    exc.retval = HUGE;
  else
    exc.retval = nan_with_flags(AMD_F_INVALID);
  if (_LIB_VERSION == _POSIX_)
    __set_errno(EDOM);
  else if (!matherr(&exc))
    {
      if(_LIB_VERSION == _SVID_)
        (void)fputs("atanf: DOMAIN error\n", stderr);
    __set_errno(EDOM);
    }
  return exc.retval;
}
#endif

#ifdef WINDOWS
#pragma function(atanf)
#endif

float FN_PROTOTYPE(atanf)(float fx)
{

  /* Some constants and split constants. */

  static double piby2 = 1.5707963267948966e+00; /* 0x3ff921fb54442d18 */

  double c, v, s, q, z;
  unsigned int xnan;

  double x = fx;

  /* Find properties of argument fx. */

  unsigned long long ux, aux, xneg;

  GET_BITS_DP64(x, ux);
  aux = ux & ~SIGNBIT_DP64;
  xneg = ux & SIGNBIT_DP64;

  v = x;
  if (xneg) v = -x;

  /* Argument reduction to range [-7/16,7/16] */

  if (aux < 0x3ec0000000000000) /* v < 2.0^(-19) */
    {
      /* x is a good approximation to atan(x) */
      if (aux == 0x0000000000000000)
        return fx;
      else
        return valf_with_flags(fx, AMD_F_INEXACT);
    }
  else if (aux < 0x3fdc000000000000) /* v < 7./16. */
    {
      x = v;
      c = 0.0;
    }
  else if (aux < 0x3fe6000000000000) /* v < 11./16. */
    {
      x = (2.0*v-1.0)/(2.0+v);
      /* c = arctan(0.5) */
      c = 4.63647609000806093515e-01; /* 0x3fddac670561bb4f */
    }
  else if (aux < 0x3ff3000000000000) /* v < 19./16. */
    {
      x = (v-1.0)/(1.0+v);
      /* c = arctan(1.) */
      c = 7.85398163397448278999e-01; /* 0x3fe921fb54442d18 */
    }
  else if (aux < 0x4003800000000000) /* v < 39./16. */
    {
      x = (v-1.5)/(1.0+1.5*v);
      /* c = arctan(1.5) */
      c = 9.82793723247329054082e-01; /* 0x3fef730bd281f69b */
    }
  else
    {

      xnan = (aux > PINFBITPATT_DP64);

      if (xnan)
        {
          /* x is NaN */
#ifdef WINDOWS
          unsigned int uhx;
          GET_BITS_SP32(fx, uhx);
          return handle_errorf("atanf", uhx|0x00400000, _DOMAIN,
                               0, EDOM, fx, 0.0F);
#else
          return x + x; /* Raise invalid if it's a signalling NaN */
#endif
        }
      else if (aux > 0x4190000000000000)
	{ /* abs(x) > 2^26 => arctan(1/x) is
	     insignificant compared to piby2 */
	  if (xneg)
            return valf_with_flags((float)-piby2, AMD_F_INEXACT);
	  else
            return valf_with_flags((float)piby2, AMD_F_INEXACT);
	}

      x = -1.0/v;
      /* c = arctan(infinity) */
      c = 1.57079632679489655800e+00; /* 0x3ff921fb54442d18 */
    }

  /* Core approximation: Remez(2,2) on [-7/16,7/16] */

  s = x*x;
  q = x*s*
    (0.296528598819239217902158651186e0 +
     (0.192324546402108583211697690500e0 +
       0.470677934286149214138357545549e-2*s)*s)/
    (0.889585796862432286486651434570e0 +
     (0.111072499995399550138837673349e1 +
       0.299309699959659728404442796915e0*s)*s);

  z = c - (q - x);

  if (xneg) z = -z;
  return (float)z;
}

weak_alias (__atanf, atanf)
