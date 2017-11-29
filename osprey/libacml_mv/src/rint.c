
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


#include "libm_amd.h"
#include "libm_util_amd.h"



double FN_PROTOTYPE(rint)(double x)
{

    UT64 checkbits,val_2p52;
	UT32 sign;
    checkbits.f64=x;

    /* Clear the sign bit and check if the value can be rounded(i.e check if exponent less than 52) */
    if( (checkbits.u64 & 0x7FFFFFFFFFFFFFFF) > 0x4330000000000000)
    {
      /* take care of nan or inf */
      if((checkbits.u32[1] & 0x7ff00000)== 0x7ff00000)
          return x+x;
      else
          return x;
    }

    sign.u32 =  checkbits.u32[1] & 0x80000000;
    val_2p52.u32[1] = sign.u32 | 0x43300000;
    val_2p52.u32[0] = 0;

	/* Add and sub 2^52 to round the number according to the current rounding direction */
    val_2p52.f64 = (x + val_2p52.f64) - val_2p52.f64;

    /*This extra line is to take care of denormals and various rounding modes*/
    val_2p52.u32[1] = ((val_2p52.u32[1] << 1) >> 1) | sign.u32;

     if(x!=val_2p52.f64)
	{
   	     /* Raise floating-point inexact exception if the result differs in value from the argument */
      	    checkbits.u64 = QNANBITPATT_DP64;
     	    checkbits.f64 = checkbits.f64 +  checkbits.f64;        /* raise inexact exception by adding two nan numbers.*/
	}


    return (val_2p52.f64);
}




