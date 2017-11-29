
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

// y = log10f(x)
// y = log10(x)
// y = logf(x)
// y = log(x)

// these codes and the ones in the related .S or .asm files have to match
#define LOG_X_ZERO      1
#define LOG_X_NEG       2
#define LOG_X_NAN       3

static float _logf_special_common(float x, float y, U32 code, char *name)
{
    switch(code)
    {
    case LOG_X_ZERO:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_DIVBYZERO);
            __amd_handle_errorf(SING, ERANGE, name, x, 0, 0.0f, 0, y, 0);
        }
        break;

    case LOG_X_NEG:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
            __amd_handle_errorf(DOMAIN, EDOM, name, x, 0, 0.0f, 0, y, 0);
        }
        break;

    case LOG_X_NAN:
        {
#ifdef WIN64
            // y is assumed to be qnan, only check x for snan
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, name, x, is_x_snan, 0.0f, 0, y, 0);
#else
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#endif
        }
        break;
    }

    return y;
}

float _logf_special(float x, float y, U32 code)
{
    return _logf_special_common(x, y, code, "logf");
}

float _log10f_special(float x, float y, U32 code)
{
    return _logf_special_common(x, y, code, "log10f");
}

float _log2f_special(float x, float y, U32 code)
{
    return _logf_special_common(x, y, code, "log2f");
}

static double _log_special_common(double x, double y, U32 code, char *name)
{
    switch(code)
    {
    case LOG_X_ZERO:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_DIVBYZERO);
            __amd_handle_error(SING, ERANGE, name, x, 0.0, y);
        }
        break;

    case LOG_X_NEG:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
            __amd_handle_error(DOMAIN, EDOM, name, x, 0.0, y);
        }
        break;

    case LOG_X_NAN:
        {
#ifdef WIN64
            __amd_handle_error(DOMAIN, EDOM, name, x, 0.0, y);
#else
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#endif
        }
        break;
    }

    return y;
}

double _log_special(double x, double y, U32 code)
{
    return _log_special_common(x, y, code, "log");
}

double _log10_special(double x, double y, U32 code)
{
    return _log_special_common(x, y, code, "log10");
}

double _log2_special(double x, double y, U32 code)
{
    return _log_special_common(x, y, code, "log2");
}

