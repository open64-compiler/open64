
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


#include <emmintrin.h>
#include <math.h>
#ifdef WIN64
#include <fpieee.h>
#else
#include <errno.h>
#endif
#include "../inc/libm_amd.h"
#include "../inc/libm_util_amd.h"


#include "../inc/libm_special.h"

#ifdef WINDOWS
#pragma function(sqrt)
#endif
/*SSE2 contains an instruction SQRTSD. This instruction Computes the square root 
  of the low-order double-precision floating-point value in an XMM register
  or in a 64-bit memory location and writes the result in the low-order quadword 
  of another XMM register. The corresponding intrinsic is _mm_sqrt_sd()*/
double FN_PROTOTYPE(sqrt)(double x)
{
    __m128d X128;
    double result;
    UT64 uresult;

    if(x < 0.0)
    {
        uresult.u64 = 0xfff8000000000000;
        __amd_handle_error(DOMAIN, EDOM, "sqrt", x, 0.0 , uresult.f64);
        return uresult.f64;
    }
    /*Load x into an XMM register*/
    X128 = _mm_load_sd(&x);
    /*Calculate sqrt using SQRTSD instrunction*/
    X128 = _mm_sqrt_sd(X128, X128);
    /*Store back the result into a double precision floating point number*/
    _mm_store_sd(&result, X128);
    return result;
}


