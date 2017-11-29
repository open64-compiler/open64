
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

float FN_PROTOTYPE(modff)(float x, float *iptr)
{
  /* modff splits the argument x into integer and fraction parts,
     each with the same sign as x. */

  unsigned int ux, mask;
  int xexp;

  GET_BITS_SP32(x, ux);
  xexp = ((ux & (~SIGNBIT_SP32)) >> EXPSHIFTBITS_SP32) - EXPBIAS_SP32;

  if (xexp < 0)
    {
      /* abs(x) < 1.0. Set iptr to zero with the sign of x
         and return x. */
      PUT_BITS_SP32(ux & SIGNBIT_SP32, *iptr);
      return x;
    }
  else if (xexp < EXPSHIFTBITS_SP32)
    {
      float r;
      unsigned int ur;
      /* x lies between 1.0 and 2**(24) */
      /* Mask out the bits of x that we don't want */
      mask = (1 << (EXPSHIFTBITS_SP32 - xexp)) - 1;
      PUT_BITS_SP32(ux & ~mask, *iptr);
      r = x - *iptr;
      GET_BITS_SP32(r, ur);
      PUT_BITS_SP32(((ux & SIGNBIT_SP32)|ur), r);
      return r;
    }
  else if ((ux & (~SIGNBIT_SP32)) > 0x7f800000)
    {
      /* x is NaN */
      *iptr = x;
      return x + x; /* Raise invalid if it is a signalling NaN */
    }
  else
    {
      /* x is infinity or large. Set iptr to x and return zero
         with the sign of x. */
      *iptr = x;
      PUT_BITS_SP32(ux & SIGNBIT_SP32, x);
      return x;
    }
}

weak_alias (__modff, modff)
