
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


#include "../inc/libm_amd.h"
#include "../inc/libm_util_amd.h"
#include <stdio.h>

double  FN_PROTOTYPE(nan)(const char *tagp)
{


    /* Check for input range */
    UT64 checkbits;
    U64 val=0;
    S64 num;
    checkbits.u64  =QNANBITPATT_DP64; 
    if(tagp == NULL)
    {
      return checkbits.f64;
    }

    switch(*tagp)
    {
    case '0': /* base 8 */
                tagp++; 
                if( *tagp == 'x' || *tagp == 'X')
                {
                    /* base 16 */
                    tagp++;
                    while(*tagp != '\0')
                    {
                        
                        if(*tagp >= 'A' && *tagp <= 'F' )
                        {
                            num = *tagp - 'A' + 10;
                        }
                        else
                        if(*tagp >= 'a' && *tagp <= 'f' )
                        {                          
                            num = *tagp - 'a' + 10;  
                        }
                        else
                        {
                            num = *tagp - '0'; 
                        }                        

                        if( (num < 0 || num > 15))
                        {
                            val = QNANBITPATT_DP64;
                            break;
                        }
                        val = (val << 4)  |  num; 
                        tagp++;
                    }
                }
                else
                {
                    /* base 8 */
                    while(*tagp != '\0')
                    {
                        num = *tagp - '0';
                        if( num < 0 || num > 7)
                        {
                            val = QNANBITPATT_DP64;
                            break;
                        }
                        val = (val << 3)  |  num; 
                        tagp++;
                    }
                }
		break;
    default:
                while(*tagp != '\0')
                {
                    val = val*10;
                    num = *tagp - '0';
                    if( num < 0 || num > 9)
                    {
                        val = QNANBITPATT_DP64;
                        break;
                    }
                    val = val + num; 
                    tagp++;
                }
            
    }

   if((val & ~NINFBITPATT_DP64) == 0)
	val = QNANBITPATT_DP64;
	 
    checkbits.u64 = (val | QNANBITPATT_DP64) & ~SIGNBIT_DP64;
    return checkbits.f64  ;
}

