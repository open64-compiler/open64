
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

double _sin_cos_special(double x, char *name)
{
    UT64 xu;
	unsigned int is_snan;

	xu.f64 = x;

    if((xu.u64 & EXPBITS_DP64) == EXPBITS_DP64)
    {
        // x is Inf or NaN
        if((xu.u64 & MANTBITS_DP64) == 0x0)
        {
            // x is Inf
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID); 
#ifdef WIN64
            xu.u64 = INDEFBITPATT_DP64;
			__amd_handle_error(DOMAIN, EDOM, name, x, 0, xu.f64);
#else
			xu.u64 = QNANBITPATT_DP64;
            name = *(&name); // dummy statement to avoid warning
#endif
		}
		else {
			// x is NaN
            is_snan = (((xu.u64 & QNAN_MASK_64) == QNAN_MASK_64) ? 0 : 1);
			if(is_snan){
				xu.u64 |= QNAN_MASK_64;
#ifdef WIN64
#else
				_mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#endif
			}
#ifdef WIN64
			__amd_handle_error(DOMAIN, EDOM, name, x, 0, xu.f64);
#endif
		}
		
	}

	return xu.f64;
}

float _sinf_cosf_special(float x, char *name)
{
    UT32 xu;
	unsigned int is_snan;

	xu.f32 = x;

    if((xu.u32 & EXPBITS_SP32) == EXPBITS_SP32)
    {
        // x is Inf or NaN
        if((xu.u32 & MANTBITS_SP32) == 0x0)
        {
            // x is Inf	
            _mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
#ifdef WIN64
            xu.u32 = INDEFBITPATT_SP32;
			__amd_handle_errorf(DOMAIN, EDOM, name, x, 0, 0.0f, 0, xu.f32, 0);
#else
			xu.u32 = QNANBITPATT_SP32; 
            name = *(&name); // dummy statement to avoid warning
#endif
		}
		else {
			// x is NaN
            is_snan = (((xu.u32 & QNAN_MASK_32) == QNAN_MASK_32) ? 0 : 1);
			if(is_snan) {
				xu.u32 |= QNAN_MASK_32;
				_mm_setcsr(_mm_getcsr() | MXCSR_ES_INVALID);
			}
#ifdef WIN64
			__amd_handle_errorf(DOMAIN, EDOM, name, x, is_snan, 0.0f, 0, xu.f32, 0);
#endif
		}
		
	}

	return xu.f32;
}

float _sinf_special(float x)
{
	return _sinf_cosf_special(x, "sinf");
}

double _sin_special(double x)
{
	return _sin_cos_special(x, "sin");
}

float _cosf_special(float x)
{
	return _sinf_cosf_special(x, "cosf");
}

double _cos_special(double x)
{
	return _sin_cos_special(x, "cos");
}

void _sincosf_special(float x, float *sy, float *cy)
{
    float xu = _sinf_cosf_special(x, "sincosf");

	*sy = xu;
	*cy = xu;

	return;
}

void _sincos_special(double x, double *sy, double *cy)
{
    double xu = _sin_cos_special(x, "sincos");

	*sy = xu;
	*cy = xu;

	return;
}
