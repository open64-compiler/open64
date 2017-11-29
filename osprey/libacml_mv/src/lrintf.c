
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



long int FN_PROTOTYPE(lrintf)(float x)
{

    UT32 checkbits,val_2p23;
    checkbits.f32=x;

    /* Clear the sign bit and check if the value can be rounded */

    if( (checkbits.u32 & 0x7FFFFFFF) > 0x4B000000)
    {
        /* number cant be rounded raise an exception */
        /* Number exceeds the representable range could be nan or inf also*/

        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "lrintf", x, is_x_snan, 0.0F , 0,(float)x, 0);
        }

		return (long int) x;
    }


    val_2p23.u32 = (checkbits.u32 & 0x80000000) | 0x4B000000;

   /* Add and sub 2^23 to round the number according to the current rounding direction */

    return (long int) ((x + val_2p23.f32) - val_2p23.f32);
}
