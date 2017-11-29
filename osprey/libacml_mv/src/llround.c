
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


#ifdef WIN64
#include <fpieee.h>
#else
#include <errno.h>
#endif

#include <math.h>
#include "../inc/libm_amd.h"
#include "../inc/libm_util_amd.h"

#include "../inc/libm_special.h"

#ifdef WINDOWS
/*In windows llong long int is 64 bit and long int is 32 bit.
  In Linux long long int and long int both are of size 64 bit*/
long long int FN_PROTOTYPE(llround)(double d)
{
    UT64 u64d;
    UT64 u64Temp,u64result;
    int intexp, shift;
    U64 sign;
    long long int result;

    u64d.f64 = u64Temp.f64 = d;

    if ((u64d.u32[1] & 0X7FF00000) == 0x7FF00000)
    {
        /*else the number is infinity*/
        //Got to raise range or domain error
            __amd_handle_error(DOMAIN, EDOM, "llround", d, 0.0 , (double)SIGNBIT_DP64);
			return SIGNBIT_DP64; /*GCC returns this when the number is out of range*/
    }

    u64Temp.u32[1] &= 0x7FFFFFFF;
    intexp = (u64d.u32[1] & 0x7FF00000) >> 20;
    sign = u64d.u64 & 0x8000000000000000;
    intexp -= 0x3FF;

    /* 1.0 x 2^-1 is the smallest number which can be rounded to 1 */
    if (intexp < -1)
        return (0);

    /* 1.0 x 2^31 (or 2^63) is already too large */
    if (intexp >= 63)
    {
        /*Based on the sign of the input value return the MAX and MIN*/
        result = 0x8000000000000000; /*Return LONG MIN*/
        __amd_handle_error(DOMAIN, EDOM, "lround", d, 0.0 , (double) result);

        return result;
    }

    u64result.f64 = u64Temp.f64;
    /* >= 2^52 is already an exact integer */
    if (intexp < 52)
    {
        /* add 0.5, extraction below will truncate */
        u64result.f64 = u64Temp.f64 + 0.5;
    }

    intexp = ((u64result.u32[1] >> 20) & 0x7ff) - 0x3FF;

    u64result.u32[1] &= 0xfffff;
    u64result.u32[1] |= 0x00100000; /*Mask the last exp bit to 1*/
    shift = intexp - 52;

    if(shift < 0)
        u64result.u64 = u64result.u64 >> (-shift);
    if(shift > 0)
        u64result.u64 = u64result.u64 << (shift);

    result = u64result.u64;

    if (sign)
        result = -result;

    return result;
}

#else //WINDOWS 
/*llroundf is equivalent to the linux implementation of 
  lroundf. Both long int and long long int are of the same size*/
long long int FN_PROTOTYPE(llround)(double d)
{
    long long int result;
    result = FN_PROTOTYPE(lround)(d);
    return result;
}
#endif
