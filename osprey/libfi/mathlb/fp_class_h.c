/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libfi/mathlb/fp_class_h.c	92.2	08/02/99 13:59:36"


#include <fortran.h>
#include <cray/portdefs.h>
#include <stdio.h>

#if (defined(_CRAYIEEE) && !defined(__mips)) || defined(_SOLARIS)
#include <fp.h>
#elif defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
#include <fp_class.h>
#endif

#include "fp_class_pc.h"

extern _f_int4 _FP_CLASS_I4_H(_f_real4 x);

/* _FP_CLASS returns a value indicating the class of the argument.
 * The classes are listed in fp_class_pc.h 
 * Argument: 32-bit real value
 * Result:   32-bit integer
 */

_f_int4 _FP_CLASS_I4_H(_f_real4 x)
{
#if defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))

  int x_result;
  x_result = fp_class_f(x);

  switch(x_result) {
  case FP_NEG_INF: {
    return (FOR_K_FP_NEG_INF);
    break;
  }
  case FP_POS_INF: {
    return (FOR_K_FP_POS_INF);
    break;
  }
  case FP_NEG_DENORM: {
    return (FOR_K_FP_NEG_DENORM);
    break;
  }
  case FP_POS_DENORM: {
    return (FOR_K_FP_POS_DENORM);
    break;
  }
  case FP_NEG_ZERO: {
    return (FOR_K_FP_NEG_ZERO);
    break;
  }
  case FP_POS_ZERO: {
    return (FOR_K_FP_POS_ZERO);
    break;
  }
  case FP_SNAN: {
    return (FOR_K_FP_SNAN);
    break;
  }
  case FP_QNAN: {
    return (FOR_K_FP_QNAN);
    break;
  }
  case FP_NEG_NORM: {
    return (FOR_K_FP_NEG_NORM);
    break;
  }
  case FP_POS_NORM: {
    return (FOR_K_FP_POS_NORM);
    break;
  }
  default: {
    return -1;
    break;
  }
  } /* Switch(x_result) */

#elif (defined(_CRAYIEEE) && !defined(__mips)) || defined(_SOLARIS)

  int x_result;
  union _uval_h x_val;

  x_result = fpclassify(x);
  x_val.dwd = x;

  switch(x_result) {
  case FP_NAN: {
    
#ifdef _CRAYT3E /* on the t3e, all NaNs are signal NaNs */
    return (FOR_K_FP_SNAN);
#else
    /* test for quiet/signal on others */
    if(x_val.parts.q_bit) {
      return (FOR_K_FP_QNAN);
    } else {
      return (FOR_K_FP_SNAN);
    }
#endif /* #ifdef _CRAYT3E */

    break;
  }
  case FP_INFINITE: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_INF);
    } else {
      return (FOR_K_FP_POS_INF);
    }
    break;
  }
  case FP_NORMAL: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_NORM);
    } else {
      return (FOR_K_FP_POS_NORM);
    }
    break;
  }
  case FP_SUBNORMAL: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_DENORM);
    } else {
      return (FOR_K_FP_POS_DENORM);
    }
    break;
  }
  case FP_ZERO: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_ZERO);
    } else {
      return (FOR_K_FP_POS_ZERO);
    }
    break;
  }
  default: {
    return -1;
    break;
  }
  } /* End switch(x_result); */

#elif defined(_LITTLE_ENDIAN) && !defined(__sv2)

  union _uval_h x_val;

  x_val.dwd = x;
  if(x_val.parts.exp == 0)
    {
      if(x_val.parts.mant == 0 &&
	 x_val.parts.q_bit == 0)
	{
	  if(x_val.parts.sign)
	    return (FOR_K_FP_NEG_ZERO);
	  else
	    return (FOR_K_FP_POS_ZERO);
	}
      else
	{
	  if(x_val.parts.sign)
	    return (FOR_K_FP_NEG_DENORM);
	  else
	    return (FOR_K_FP_POS_DENORM);
	}
    }
  else if(x_val.parts.exp == IEEE_32_EXPO_MAX)
    {
      if(x_val.parts.mant == 0 &&
	 x_val.parts.q_bit == 0)
	{
	  if(x_val.parts.sign)
	    return (FOR_K_FP_NEG_INF);
	  else
	    return (FOR_K_FP_POS_INF);
	}
      else
	{
	  if(x_val.parts.q_bit)
	    return (FOR_K_FP_QNAN);
	  else
	    return (FOR_K_FP_SNAN);
	}
    }
  else if(x_val.parts.sign)
    return (FOR_K_FP_NEG_NORM);
  else
    return (FOR_K_FP_POS_NORM);

#endif /* #if defined(__mips) ... #elif defined(_CRAYT3E) && defined(__mips) */

  return -1;
  
}

extern _f_int8 _FP_CLASS_I8_H(_f_real4 x);

/* Argument:  32-bit real
 * Result:    64-bit integer
 */

_f_int8 _FP_CLASS_I8_H(_f_real4 x)
{
#if defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))

  int x_result;
  x_result = fp_class_f(x);

  switch(x_result) {
  case FP_SNAN: {
    return (FOR_K_FP_SNAN);
    break;
  }
  case FP_QNAN: {
    return (FOR_K_FP_QNAN);
    break;
  }
  case FP_NEG_INF: {
    return (FOR_K_FP_NEG_INF);
    break;
  }
  case FP_POS_INF: {
    return (FOR_K_FP_POS_INF);
    break;
  }
  case FP_NEG_DENORM: {
    return (FOR_K_FP_NEG_DENORM);
    break;
  }
  case FP_POS_DENORM: {
    return (FOR_K_FP_POS_DENORM);
    break;
  }
  case FP_NEG_ZERO: {
    return (FOR_K_FP_NEG_ZERO);
    break;
  }
  case FP_POS_ZERO: {
    return (FOR_K_FP_POS_ZERO);
    break;
  }
  case FP_NEG_NORM: {
    return (FOR_K_FP_NEG_NORM);
    break;
  }
  case FP_POS_NORM: {
    return (FOR_K_FP_POS_NORM);
    break;
  }
  default: {
    return -1;
    break;
  }
  } /* Switch(x_result) */

#elif (defined(_CRAYIEEE) && !defined(__mips)) || defined(_SOLARIS)

  int x_result;
  union _uval_h x_val;

  x_result = fpclassify(x);
  x_val.dwd = x;

  switch(x_result) {
  case FP_NAN: {
    
#ifdef _CRAYT3E /* on the t3e, all NaNs are signal NaNs */
    return (FOR_K_FP_SNAN);
#else
    if(x_val.parts.q_bit) {
      return (FOR_K_FP_QNAN);
    } else {
      return (FOR_K_FP_SNAN);
    }
#endif /* #ifdef _CRAYT3E */

    break;
  }
  case FP_INFINITE: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_INF);
    } else {
      return (FOR_K_FP_POS_INF);
    }
    break;
  }
  case FP_NORMAL: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_NORM);
    } else {
      return (FOR_K_FP_POS_NORM);
    }
    break;
  }
  case FP_SUBNORMAL: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_DENORM);
    } else {
      return (FOR_K_FP_POS_DENORM);
    }
    break;
  }
  case FP_ZERO: {
    /* Test for pos/neg */
    if(x_val.parts.sign) {
      return (FOR_K_FP_NEG_ZERO);
    } else {
      return (FOR_K_FP_POS_ZERO);
    }
    break;
  }
  default: {
    return -1;
    break;
  }
  } /* End switch(x_result); */

#elif defined(_LITTLE_ENDIAN) && !defined(__sv2)

  union _uval_h x_val;

  x_val.dwd = x;
  if(x_val.parts.exp == 0)
    {
      if(x_val.parts.mant == 0 &&
	 x_val.parts.q_bit == 0)
	{
	  if(x_val.parts.sign)
	    return (FOR_K_FP_NEG_ZERO);
	  else
	    return (FOR_K_FP_POS_ZERO);
	}
      else
	{
	  if(x_val.parts.sign)
	    return (FOR_K_FP_NEG_DENORM);
	  else
	    return (FOR_K_FP_POS_DENORM);
	}
    }
  else if(x_val.parts.exp == IEEE_32_EXPO_MAX)
    {
      if(x_val.parts.mant == 0 &&
	 x_val.parts.q_bit == 0)
	{
	  if(x_val.parts.sign)
	    return (FOR_K_FP_NEG_INF);
	  else
	    return (FOR_K_FP_POS_INF);
	}
      else
	{
	  if(x_val.parts.q_bit)
	    return (FOR_K_FP_QNAN);
	  else
	    return (FOR_K_FP_SNAN);
	}
    }
  else if(x_val.parts.sign)
    return (FOR_K_FP_NEG_NORM);
  else
    return (FOR_K_FP_POS_NORM);

#endif /* #if defined(__mips) ... #elif defined(_CRAYT3E) && defined(__mips) */

  return -1;

}
