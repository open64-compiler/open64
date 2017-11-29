
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




double FN_PROTOTYPE(nexttoward)(double x, long double y)
{


    UT64 checkbits;
    long double dy = (long double) y;
    checkbits.f64=x;

    /* if x == y return y in the type of x */
    if( x == dy )
    {
        return (double) dy;
    }

    /* check if the number is nan */
    if(((checkbits.u64 & ~SIGNBIT_DP64) >= EXPBITS_DP64 ))
    {

		__amd_handle_error(DOMAIN, ERANGE, "nexttoward", x, (double)y ,x+x);


        return x+x;
    }

    if( x == 0.0)
    {
        checkbits.u64 = 1;
        if( dy > 0.0 )
             return checkbits.f64;
        else
            return -checkbits.f64;
    }


    /* compute the next heigher or lower value */

    if(((x>0.0) ^ (dy>x)) == 0)
    {
        checkbits.u64++;
    }
    else
    {
        checkbits.u64--;
    }

    /* check if the result is nan or inf */
    if(((checkbits.u64 & ~SIGNBIT_DP64) >= EXPBITS_DP64 ))
    {
		__amd_handle_error(DOMAIN, ERANGE, "nexttoward", x, (double)y ,checkbits.f64);


    }

    return checkbits.f64;
}
