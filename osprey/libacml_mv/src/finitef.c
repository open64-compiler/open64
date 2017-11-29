
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

/* Returns 0 if x is infinite or NaN, otherwise returns 1 */

#ifdef WINDOWS
int FN_PROTOTYPE(finitef)(float x)
#else
int FN_PROTOTYPE(finitef)(float x)
#endif
{

#ifdef WINDOWS

  unsigned int ux;
  GET_BITS_SP32(x, ux);
  return (int)(((ux & ~SIGNBIT_SP32) - PINFBITPATT_SP32) >> 31);

#else

  /* This works on Hammer */
  unsigned int ux=0x7f800000;
  float temp;    
  PUT_BITS_SP32(ux, temp);

 /* float temp = 1.0e444; *//* = infinity = 0x7f800000 */
  volatile int retval;
  retval = 0;
  asm volatile ("andps	%0, %1;" : : "x" (temp), "x" (x));
  asm volatile ("comiss	%0, %1" : : "x" (temp), "x" (x));
  asm volatile ("setnz	%0" : "=g" (retval));
  return retval;

#endif
}

weak_alias (__finitef, finitef)
