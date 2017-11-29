
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
long long int FN_PROTOTYPE(llroundf)(float f)
{
    UT32 u32d;
    UT32 u32Temp,u32result;
    int intexp, shift;
    U32 sign;
    long long int  result;

    u32d.f32 = u32Temp.f32 = f;
    if ((u32d.u32 & 0X7F800000) == 0x7F800000)
    {
        /*else the number is infinity*/
		//Got to raise range or domain error
        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = f;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "llroundf", f, is_x_snan, 0.0F , 0,(float)SIGNBIT_DP64, 0);
			return SIGNBIT_DP64; /*GCC returns this when the number is out of range*/
        }

    }

    u32Temp.u32 &= 0x7FFFFFFF;
    intexp = (u32d.u32 & 0x7F800000) >> 23;
    sign = u32d.u32 & 0x80000000;
    intexp -= 0x7F;


    /* 1.0 x 2^-1 is the smallest number which can be rounded to 1 */
    if (intexp < -1)
        return (0);


    /* 1.0 x 2^31 (or 2^63) is already too large */
    if (intexp >= 63)
    {
        result = 0x8000000000000000;
            
        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = f;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "lroundf", f, is_x_snan, 0.0F , 0,(float)result, 0);
        }

        return result;
    }

    u32result.f32 = u32Temp.f32;

    /* >= 2^52 is already an exact integer */
    if (intexp < 23)
    {
        /* add 0.5, extraction below will truncate */
        u32result.f32 = u32Temp.f32 + 0.5F;
    }
    intexp = (u32result.u32 & 0x7f800000) >> 23;
    intexp -= 0x7f;
    u32result.u32 &= 0x7fffff;
    u32result.u32 |= 0x00800000;

    result = u32result.u32;

    /*Since float is only 32 bit for higher accuracy we shift the result by 32 bits
     * In the next step we shift an extra 32 bits in the reverse direction based
     * on the value of intexp*/
    result = result << 32;
    shift = intexp - 55; /*55= 23 +32*/


	if(shift < 0)
		result = result >> (-shift);
	if(shift > 0)
        result = result << (shift);

    if (sign)
        result = -result;
    return result;

}

#else //WINDOWS
/*llroundf is equivalent to the linux implementation of 
  lroundf. Both long int and long long int are of the same size*/
long long int FN_PROTOTYPE(llroundf)(float f)
{
    long long int result;
    result = FN_PROTOTYPE(lroundf)(f);
    return result;

}
#endif

