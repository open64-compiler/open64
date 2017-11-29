
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

long int FN_PROTOTYPE(lround)(double d)
{
    UT64 u64d;
    UT64 u64Temp,u64result;
    int intexp, shift;
    U64 sign;
    long int result;

    u64d.f64 = u64Temp.f64 = d;

    if ((u64d.u32[1] & 0X7FF00000) == 0x7FF00000)
    {
        /*else the number is infinity*/
        //Raise range or domain error
        #ifdef WIN64
            __amd_handle_error(DOMAIN, EDOM, "lround", d, 0.0 , (double)SIGNBIT_SP32);
			return (long int )SIGNBIT_SP32;
        #else
            __amd_handle_error(DOMAIN, EDOM, "lround", d, 0.0 , (double)SIGNBIT_DP64);
			return SIGNBIT_DP64; /*GCC returns this when the number is out of range*/
        #endif

    }

    u64Temp.u32[1] &= 0x7FFFFFFF;
    intexp = (u64d.u32[1] & 0x7FF00000) >> 20;
    sign = u64d.u64 & 0x8000000000000000;
    intexp -= 0x3FF;

    /* 1.0 x 2^-1 is the smallest number which can be rounded to 1 */
    if (intexp < -1)
        return (0);

#ifdef WIN64
    /* 1.0 x 2^31 (or 2^63) is already too large */
    if (intexp >= 31)
    {
        /*Based on the sign of the input value return the MAX and MIN*/
        result = 0x80000000; /*Return LONG MIN*/
        
        __amd_handle_error(DOMAIN, EDOM, "lround", d, 0.0 , (double) result);

        return result;
    }


#else
    /* 1.0 x 2^31 (or 2^63) is already too large */
    if (intexp >= 63)
    {
        /*Based on the sign of the input value return the MAX and MIN*/
        result = 0x8000000000000000; /*Return LONG MIN*/
            
        __amd_handle_error(DOMAIN, EDOM, "lround", d, 0.0 , (double) result);

        return result;
    }

#endif

    u64result.f64 = u64Temp.f64;
    /* >= 2^52 is already an exact integer */
#ifdef WIN64
    if (intexp < 23)
#else
    if (intexp < 52)
#endif
    {
        /* add 0.5, extraction below will truncate */
        u64result.f64 = u64Temp.f64 + 0.5;
    }

    intexp = ((u64result.u32[1] >> 20) & 0x7ff) - 0x3FF;

    u64result.u32[1] &= 0xfffff;
    u64result.u32[1] |= 0x00100000; /*Mask the last exp bit to 1*/
    shift = intexp - 52;

#ifdef WIN64
	/*The shift value will always be negative.*/
    u64result.u64 = u64result.u64 >> (-shift);
	/*Result will be stored in the lower word due to the shift being performed*/
    result = u64result.u32[0];
#else
     if(shift < 0)
        u64result.u64 = u64result.u64 >> (-shift);
    if(shift > 0)
        u64result.u64 = u64result.u64 << (shift);

    result = u64result.u64;
#endif



    if (sign)
        result = -result;

    return result;
}

