
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
#pragma function(sqrtf)
#endif
/*SSE2 contains an instruction SQRTSS. This instruction Computes the square root 
  of the low-order single-precision floating-point value in an XMM register
  or in a 32-bit memory location and writes the result in the low-order doubleword 
  of another XMM register. The corresponding intrinsic is _mm_sqrt_ss()*/
float FN_PROTOTYPE(sqrtf)(float x)
{
    __m128 X128;
    float result;
    UT32 uresult;

    if(x < 0.0)
    {
        uresult.u32 = 0xffc00000;

        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "sqrt", x, is_x_snan, 0.0f, 0, uresult.f32, 0);
        }

        return uresult.f32;
    }

    /*Load x into an XMM register*/
    X128 = _mm_load_ss(&x);
    /*Calculate sqrt using SQRTSS instrunction*/
    X128 = _mm_sqrt_ss(X128);
    /*Store back the result into a single precision floating point number*/
    _mm_store_ss(&result, X128);
    return result;
}


