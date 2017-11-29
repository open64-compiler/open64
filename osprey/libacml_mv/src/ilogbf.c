
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
#include <limits.h>
#include "../inc/libm_amd.h"
#include "../inc/libm_util_amd.h"

#include "../inc/libm_special.h"

int FN_PROTOTYPE(ilogbf)(float x)
{

    /* Check for input range */
    UT32 checkbits;
    int expbits;
    U32 manbits;
	U32 zerovalue;
    checkbits.f32=x;

    /* Clear the sign bit and check if the value is zero nan or inf.*/
    zerovalue = (checkbits.u32 & ~SIGNBIT_SP32);

    if(zerovalue == 0)
    {
		/* Raise exception as the number zero*/
        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "ilogb", x, is_x_snan, 0.0f, 0, (float) INT_MIN, 0);
        }
		
		return INT_MIN;
    }

    if( zerovalue == EXPBITS_SP32 )
    {
		/* Raise exception as the number is inf */
        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "ilogb", x, is_x_snan, 0.0f, 0, (float) INT_MAX, 0);
        }
		
		return INT_MAX;
    }

    if( zerovalue > EXPBITS_SP32 )
    {
		/* Raise exception as the number is nan */
        {
            unsigned int is_x_snan;
            UT32 xm; xm.f32 = x;
            is_x_snan = ( ((xm.u32 & QNAN_MASK_32) == 0) ? 1 : 0 );
            __amd_handle_errorf(DOMAIN, EDOM, "ilogb", x, is_x_snan, 0.0f, 0, (float) INT_MIN, 0);
        }
        
		return INT_MIN;
    }

    expbits = (int) (( checkbits.u32 << 1) >> 24);

    if(expbits == 0 && (checkbits.u32 & MANTBITS_SP32 )!= 0)
    {
        /* the value is denormalized */
      manbits = checkbits.u32 & MANTBITS_SP32;
      expbits = EMIN_SP32;
      while (manbits < IMPBIT_SP32)
        {
          manbits <<= 1;
          expbits--;
        }
    }
    else
	{
		expbits-=EXPBIAS_SP32;
	}


    return expbits;
}
