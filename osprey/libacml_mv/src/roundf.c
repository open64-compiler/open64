
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

float FN_PROTOTYPE(roundf)(float f)
{
    UT32 u32f, u32Temp;
    U32 u32sign, u32exp, u32mantissa;
    int intexp;            /*Needs to be signed */
    u32f.f32 = f;
    u32sign = u32f.u32 & SIGNBIT_SP32;
    if ((u32f.u32 & 0X7F800000) == 0x7F800000)
    {
        //u32f.f32 = f;
        /*Return Quiet Nan.
         * Quiet the signalling nan*/
        if(!((u32f.u32 & MANTBITS_SP32) == 0))
            u32f.u32 |= QNAN_MASK_32;
        /*else the number is infinity*/
        //Raise range or domain error

        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = f;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "roundf", f, is_x_snan, 0.0F , 0,u32f.f32, 0);
        }

		
		return u32f.f32;
    }
    /*Get the exponent of the input*/
    intexp = (u32f.u32 & 0x7f800000) >> 23;
    intexp -= 0x7F;
    /*If exponent is greater than 22 then the number is already
      rounded*/
    if (intexp > 22)
        return f;
    if (intexp < 0)
    {
        u32Temp.f32 = f;
        u32Temp.u32 &= 0x7FFFFFFF;
        /*Add with a large number (2^23 +1) = 8388609.0F 
        to force an overflow*/
        u32Temp.f32 = (u32Temp.f32 + 8388609.0F);
        /*Substract back with t he large number*/
        u32Temp.f32 -= 8388609;
        if (u32sign)
            u32Temp.u32 |= 0x80000000;
        return u32Temp.f32;
    }
    else
    {
        /*if(intexp == -1)
            u32exp = 0x3F800000;       */
        u32f.u32 &= 0x7FFFFFFF;
        u32f.f32 += 0.5;
        u32exp = u32f.u32 & 0x7F800000;
        /*right shift then left shift to discard the decimal
          places*/
        u32mantissa = (u32f.u32 & MANTBITS_SP32) >> (23 - intexp);
        u32mantissa = u32mantissa << (23 - intexp);
        u32Temp.u32 = u32sign | u32exp | u32mantissa;
        return (u32Temp.f32);
    }
}

