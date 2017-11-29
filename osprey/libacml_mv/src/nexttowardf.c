
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

#include "libm_amd.h"
#include "libm_util_amd.h"
#include "libm_special.h"


float FN_PROTOTYPE(nexttowardf)(float x, long double y)
{


    UT32 checkbits;
    long double dy = (long double) y;
    checkbits.f32=x;

    /* if x == y return y in the type of x */
    if( x == dy )
    {
        return (float) dy;
    }

    /* check if the number is nan */
    if(((checkbits.u32 & ~SIGNBIT_SP32) >= EXPBITS_SP32 ))
    {
        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, ERANGE, "nexttowardf", x, is_x_snan, (float) y , 0,x+x, 0);

        }

        return x+x;
    }

    if( x == 0.0)
    {
        checkbits.u32 = 1;
        if( dy > 0.0 )
             return checkbits.f32;
        else
            return -checkbits.f32;
    }


    /* compute the next heigher or lower value */
    if(((x>0.0F) ^ (dy>x)) == 0)
    {
        checkbits.u32++;
    }
    else
    {
        checkbits.u32--;
    }

    /* check if the result is nan or inf */
    if(((checkbits.u32 & ~SIGNBIT_SP32) >= EXPBITS_SP32 ))
    {
        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, ERANGE, "nexttowardf", x, is_x_snan, (float) y , 0,checkbits.f32, 0);
        }
    }

    return checkbits.f32;
}
