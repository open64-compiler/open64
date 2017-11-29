
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
#endif

#include <math.h>
#include <errno.h>
#include "../inc/libm_amd.h"
#include "../inc/libm_util_amd.h"

#include "../inc/libm_special.h"


float FN_PROTOTYPE(ldexpf)(float x, int n)
{
    UT32 val;
    unsigned int sign;
    int exponent;
    val.f32 = x;
    sign = val.u32 & 0x80000000;
    val.u32 = val.u32 & 0x7fffffff;/* remove the sign bit */

    if((val.u32 & 0x7f800000)== 0x7f800000)/* x= nan or x = +-inf*/
        return x;

    if((val.u32 == 0x00000000) || (n==0))/* x= +-0 or n= 0*/
        return x;

    exponent = val.u32 >> 23; /* get the exponent */

	if(exponent == 0)/*x is denormal*/
	{
		val.f32 = val.f32 * VAL_2PMULTIPLIER_SP;/*multiply by 2^24 to bring it to the normal range*/
		exponent = (val.u32 >> 23); /* get the exponent */
		exponent = exponent + n - MULTIPLIER_SP;
		if(exponent < -MULTIPLIER_SP)/*underflow*/
		{
			val.u32 = sign | 0x00000000;

            {
                unsigned int is_x_snan;
                UT32 xm; xm.f32 = x;
                is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
                __amd_handle_errorf(UNDERFLOW, ERANGE, "ldexpf", x, is_x_snan, (float)n , 0,val.f32, 0);
            }
			
			return val.f32;
		}
		if(exponent > 254)/*overflow*/
		{
			val.u32 = sign | 0x7f800000;

            {
                unsigned int is_x_snan;
                UT32 xm; xm.f32 = x;
                is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
                __amd_handle_errorf(OVERFLOW, ERANGE, "ldexpf", x, is_x_snan, (float)n , 0,val.f32, 0);
            }

			
			return val.f32;
		}

		exponent += MULTIPLIER_SP;
		val.u32 = sign | (exponent << 23) | (val.u32 & 0x007fffff);
		val.f32 = val.f32 * VAL_2PMMULTIPLIER_SP;
        return val.f32;
	}

    exponent += n;

    if(exponent < -MULTIPLIER_SP)/*underflow*/
	{
		val.u32 = sign | 0x00000000;

        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(UNDERFLOW, ERANGE, "ldexpf", x, is_x_snan, (float)n , 0,val.f32, 0);
        }

		return val.f32;
	}

    if(exponent < 1)/*x is normal but output is debnormal*/
    {
		exponent += MULTIPLIER_SP;
		val.u32 = sign | (exponent << 23) | (val.u32 & 0x007fffff);
		val.f32 = val.f32 * VAL_2PMMULTIPLIER_SP;
        return val.f32;
    }

    if(exponent > 254)/*overflow*/
	{
        val.u32 = sign | 0x7f800000;

        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(OVERFLOW, ERANGE, "ldexpf", x, is_x_snan, (float)n , 0,val.f32, 0);
        }

        return val.f32;
	}

    val.u32 = sign | (exponent << 23) | (val.u32 & 0x007fffff);/*x is normal and output is normal*/
    return val.f32;
}

