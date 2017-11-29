
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
#include <errno.h>

#include "../inc/libm_util_amd.h"
#include "../inc/libm_special.h"

// y = expf(x)
// y = exp(x)

// these codes and the ones in the related .S or .asm files have to match
#define EXP_X_NAN       1
#define EXP_Y_ZERO      2
#define EXP_Y_INF       3

float _expf_special(float x, float y, U32 code)
{
    switch(code)
    {
    case EXP_X_NAN:
        {
#ifdef WIN64
            // y is assumed to be qnan, only check x for snan
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "expf", x, is_x_snan, 0.0f, 0, y, 0);
#else
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#endif
        }
        break;

    case EXP_Y_ZERO:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_UNDERFLOW));
            __amd_handle_errorf(UNDERFLOW, ERANGE, "expf", x, 0, 0.0f, 0, y, 0);
        }
        break;

    case EXP_Y_INF:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_OVERFLOW));
            __amd_handle_errorf(OVERFLOW, ERANGE, "expf", x, 0, 0.0f, 0, y, 0);
        }
        break;
    }


    return y;
}

double _exp_special(double x, double y, U32 code)
{
    switch(code)
    {
    case EXP_X_NAN:
        {
#ifdef WIN64
            __amd_handle_error(DOMAIN, EDOM, "exp", x, 0.0, y);
#else
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#endif
        }
        break;

    case EXP_Y_ZERO:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_UNDERFLOW));
            __amd_handle_error(UNDERFLOW, ERANGE, "exp", x, 0.0, y);
        }
        break;

    case EXP_Y_INF:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_OVERFLOW));
            __amd_handle_error(OVERFLOW, ERANGE, "exp", x, 0.0, y);
        }
        break;
    }


    return y;
}

