
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

// these codes and the ones in the related .S or .asm files have to match
#define POW_X_ONE_Y_SNAN            1
#define POW_X_ZERO_Z_INF            2
#define POW_X_NAN                   3
#define POW_Y_NAN                   4
#define POW_X_NAN_Y_NAN             5
#define POW_X_NEG_Y_NOTINT          6
#define POW_Z_ZERO                  7
#define POW_Z_DENORMAL              8
#define POW_Z_INF                   9

float _powf_special(float x, float y, float z, U32 code)
{
    switch(code)
    {
    case POW_X_ONE_Y_SNAN:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
        }
        break;

    case POW_X_ZERO_Z_INF:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_DIVBYZERO);
            __amd_handle_errorf(SING, ERANGE, "powf", x, 0, y, 0, z, 0);
        }
        break;

    case POW_X_NAN:
    case POW_Y_NAN:
    case POW_X_NAN_Y_NAN:
        {
#ifdef WIN64
            unsigned int is_x_snan = 0, is_y_snan = 0, is_z_snan = 0;
            UT32 xm, ym, zm;
            xm.f32 = x;
            ym.f32 = y;
            zm.f32 = z;
            if(code == POW_X_NAN) { is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 ); }
            if(code == POW_Y_NAN) { is_y_snan = ( ((ym.u32 & QNAN_MASK_32) == 0) ? 1 : 0 ); }
            if(code == POW_X_NAN_Y_NAN) {   is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
                                            is_y_snan = ( ((ym.u32 & QNAN_MASK_32) == 0) ? 1 : 0 ); }
            is_z_snan = ( ((zm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );

            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
            __amd_handle_errorf(DOMAIN, EDOM, "powf", x, is_x_snan, y, is_y_snan, z, is_z_snan);            
#else
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#endif
        }
        break;

    case POW_X_NEG_Y_NOTINT:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
            __amd_handle_errorf(DOMAIN, EDOM, "powf", x, 0, y, 0, z, 0);
        }
        break;

    case POW_Z_ZERO:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_UNDERFLOW));
            __amd_handle_errorf(UNDERFLOW, ERANGE, "powf", x, 0, y, 0, z, 0);
        }
        break;

    case POW_Z_INF:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_OVERFLOW));
            __amd_handle_errorf(OVERFLOW, ERANGE, "powf", x, 0, y, 0, z, 0);
        }
        break;
    }

    return z;
}

double _pow_special(double x, double y, double z, U32 code)
{
    switch(code)
    {
    case POW_X_ONE_Y_SNAN:
        {
#ifdef WIN64
#else
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#endif
        }
        break;

    case POW_X_ZERO_Z_INF:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_DIVBYZERO);
            __amd_handle_error(SING, ERANGE, "pow", x, y, z);
        }
        break;

    case POW_X_NAN:
    case POW_Y_NAN:
    case POW_X_NAN_Y_NAN:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#ifdef WIN64
            __amd_handle_error(DOMAIN, EDOM, "pow", x, y, z);
#endif
        }
        break;

    case POW_X_NEG_Y_NOTINT:
        {
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
            __amd_handle_error(DOMAIN, EDOM, "pow", x, y, z);
        }
        break;

    case POW_Z_ZERO:
    case POW_Z_DENORMAL:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_UNDERFLOW));
            __amd_handle_error(UNDERFLOW, ERANGE, "pow", x, y, z);
        }
        break;

    case POW_Z_INF:
        {
            _mm_setcsr(_mm_getcsr() | (MXCSR_ES_INEXACT|MXCSR_ES_OVERFLOW));
            __amd_handle_error(OVERFLOW, ERANGE, "pow", x, y, z);
        }
        break;
    }

    return z;
}
