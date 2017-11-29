
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

#ifdef WIN64
#define EXCEPTION_S _exception
#else
#define EXCEPTION_S exception
#endif



static double convert_snan_32to64(float x)
{
    U64 t;
    UT32 xs;
    UT64 xb;
    
    xs.f32 = x;
    xb.u64 = (((xs.u32 & SIGNBIT_SP32) == SIGNBIT_SP32) ? NINFBITPATT_DP64 : EXPBITS_DP64);
    
    t = 0;
    t = (xs.u32 & MANTBITS_SP32);
    t = (t << 29); // 29 = (52-23)
    xb.u64 = (xb.u64 | t);

    return xb.f64;
}

void __amd_handle_errorf(int type, int error, char *name,
                    float arg1, unsigned int arg1_is_snan,
                    float arg2, unsigned int arg2_is_snan,
                    float retval, unsigned int retval_is_snan)
{
    struct EXCEPTION_S exception_data;

    // write exception info
    exception_data.type = type;
    exception_data.name = name;

    // sNaN float to double conversion can trigger interrupt
    // handle them specially

    if(arg1_is_snan)    { exception_data.arg1 = convert_snan_32to64(arg1); }
    else                { exception_data.arg1 = (double)arg1; }

    if(arg2_is_snan)    { exception_data.arg2 = convert_snan_32to64(arg2); }
    else                { exception_data.arg2 = (double)arg2; }

    if(retval_is_snan)  { exception_data.retval = convert_snan_32to64(retval); }
    else                { exception_data.retval = (double)retval; }

    // call matherr, set errno if matherr returns 0
    if(!matherr(&exception_data))
    {
        errno = error;
    }
}

void __amd_handle_error(int type, int error, char *name,
                   double arg1,
                   double arg2,
                   double retval)
{
    struct EXCEPTION_S exception_data;

    // write exception info
    exception_data.type = type;
    exception_data.name = name;

    exception_data.arg1 = arg1;
    exception_data.arg2 = arg2;
    exception_data.retval = retval;

    // call matherr, set errno if matherr returns 0
    if(!matherr(&exception_data))
    {
        errno = error;
    }
}

