
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


int FN_PROTOTYPE(ilogb)(double x)
{


    /* Check for input range */
    UT64 checkbits;
    int expbits;
    U64 manbits;
	U64  zerovalue;
	    /* Clear the sign bit and check if the value is zero nan or inf.*/
	checkbits.f64=x;
    zerovalue = (checkbits.u64 & ~SIGNBIT_DP64);

    if(zerovalue == 0)
    {
        /* Raise exception as the number zero*/
		__amd_handle_error(DOMAIN, EDOM, "ilogb", x, 0.0 ,(double)INT_MIN);
		

         return INT_MIN;
    }

    if( zerovalue == EXPBITS_DP64 )
    {
        /* Raise exception as the number is inf */

		__amd_handle_error(DOMAIN, EDOM, "ilogb", x, 0.0 ,(double)INT_MAX);
		
        return INT_MAX;
    }

    if( zerovalue > EXPBITS_DP64 )
    {
		/* Raise exception as the number is nan */
		__amd_handle_error(DOMAIN, EDOM, "ilogb", x, 0.0 ,(double)INT_MIN);
		

		return INT_MIN;
    }

    expbits = (int) (( checkbits.u64 << 1) >> 53);

    if(expbits == 0 && (checkbits.u64 & MANTBITS_DP64 )!= 0)
    {
        /* the value is denormalized */
      manbits = checkbits.u64 & MANTBITS_DP64;
      expbits = EMIN_DP64;
      while (manbits < IMPBIT_DP64)
        {
          manbits <<= 1;
          expbits--;
        }
    }
    else
	{

		expbits-=EXPBIAS_DP64;
	}


    return expbits;
}
