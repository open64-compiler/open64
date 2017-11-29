
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
#define USE_NANF_WITH_FLAGS
#define USE_HANDLE_ERRORF
#include "../inc/libm_inlines_amd.h"
#undef USE_NANF_WITH_FLAGS
#undef USE_VALF_WITH_FLAGS
#undef USE_HANDLE_ERRORF

#include "../inc/libm_errno_amd.h"

#ifndef WINDOWS
/* Deal with errno for out-of-range argument */
static inline float retval_errno_edom(float x)
{
  struct exception exc;
  exc.arg1 = (double)x;
  exc.arg2 = (double)x;
  exc.name = (char *)"acosf";
  exc.type = DOMAIN;
  if (_LIB_VERSION == _SVID_)
    exc.retval = HUGE;
  else
    exc.retval = nanf_with_flags(AMD_F_INVALID);
  if (_LIB_VERSION == _POSIX_)
    __set_errno(EDOM);
  else if (!matherr(&exc))
    {
      if(_LIB_VERSION == _SVID_)
        (void)fputs("acosf: DOMAIN error\n", stderr);
    __set_errno(EDOM);
    }
  return exc.retval;
}
#endif

#ifdef WINDOWS
#pragma function(acosf)
#endif

float FN_PROTOTYPE(acosf)(float x)
{
  /* Computes arccos(x).
     The argument is first reduced by noting that arccos(x)
     is invalid for abs(x) > 1. For denormal and small
     arguments arccos(x) = pi/2 to machine accuracy.
     Remaining argument ranges are handled as follows.
     For abs(x) <= 0.5 use
     arccos(x) = pi/2 - arcsin(x)
     = pi/2 - (x + x^3*R(x^2))
     where R(x^2) is a rational minimax approximation to
     (arcsin(x) - x)/x^3.
     For abs(x) > 0.5 exploit the identity:
     arccos(x) = pi - 2*arcsin(sqrt(1-x)/2)
     together with the above rational approximation, and
     reconstruct the terms carefully.
  */

  /* Some constants and split constants. */

  static const float
    piby2      = 1.5707963705e+00F; /* 0x3fc90fdb */
  static const double
    pi         = 3.1415926535897933e+00, /* 0x400921fb54442d18 */
    piby2_head = 1.5707963267948965580e+00, /* 0x3ff921fb54442d18 */
    piby2_tail = 6.12323399573676603587e-17; /* 0x3c91a62633145c07 */

  float u, y, s = 0.0F, r;
  int xexp, xnan, transform = 0;

  unsigned int ux, aux, xneg;

  GET_BITS_SP32(x, ux);
  aux = ux & ~SIGNBIT_SP32;
  xneg = (ux & SIGNBIT_SP32);
  xnan = (aux > PINFBITPATT_SP32);
  xexp = (int)((ux & EXPBITS_SP32) >> EXPSHIFTBITS_SP32) - EXPBIAS_SP32;

  /* Special cases */

  if (xnan)
    {
#ifdef WINDOWS
      return handle_errorf("acosf", ux|0x00400000, _DOMAIN, 0,
                           EDOM, x, 0.0F);
#else
      return x + x; /* With invalid if it's a signalling NaN */
#endif
    }
  else if (xexp < -26)
    /* y small enough that arccos(x) = pi/2 */
    return valf_with_flags(piby2, AMD_F_INEXACT);
  else if (xexp >= 0)
    { /* abs(x) >= 1.0 */
      if (x == 1.0F)
        return 0.0F;
      else if (x == -1.0F)
        return valf_with_flags((float)pi, AMD_F_INEXACT);
      else
#ifdef WINDOWS
        return handle_errorf("acosf", INDEFBITPATT_SP32, _DOMAIN,
                             AMD_F_INVALID, EDOM, x, 0.0F);
#else
        return retval_errno_edom(x);
#endif
    }

  if (xneg) y = -x;
  else y = x;

  transform = (xexp >= -1); /* abs(x) >= 0.5 */

  if (transform)
    { /* Transform y into the range [0,0.5) */
      r = 0.5F*(1.0F - y);
#ifdef WINDOWS
      /* VC++ intrinsic call */
      _mm_store_ss(&s, _mm_sqrt_ss(_mm_load_ss(&r)));
#else
      /* Hammer sqrt instruction */
      asm volatile ("sqrtss %1, %0" : "=x" (s) : "x" (r));
#endif
      y = s;
    }
  else
    r = y*y;

  /* Use a rational approximation for [0.0, 0.5] */

  u=r*(0.184161606965100694821398249421F +
       (-0.0565298683201845211985026327361F +
	(-0.0133819288943925804214011424456F -
	 0.00396137437848476485201154797087F*r)*r)*r)/
    (1.10496961524520294485512696706F -
     0.836411276854206731913362287293F*r);

  if (transform)
    {
      /* Reconstruct acos carefully in transformed region */
      if (xneg)
        return (float)(pi - 2.0*(s+(y*u - piby2_tail)));
      else
	{
	  float c, s1;
	  unsigned int us;
	  GET_BITS_SP32(s, us);
	  PUT_BITS_SP32(0xffff0000 & us, s1);
	  c = (r-s1*s1)/(s+s1);
          return 2.0F*s1 + (2.0F*c+2.0F*y*u);
	}
    }
  else
    return (float)(piby2_head - (x - (piby2_tail - x*u)));
}

weak_alias (__acosf, acosf)
