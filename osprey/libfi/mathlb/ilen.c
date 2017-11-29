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


#pragma ident "@(#) libfi/mathlb/ilen.c	92.1	07/20/99 10:16:08"

#include <stdio.h>
#include <cray/portdefs.h>
#include <fortran.h>

#ifdef __mips
#include "leadz.h"
#define LOG2(k) ( (64 - _leadz8(k)) - (((k-1) & k) == 0) )
#else
#define LOG2(k) ( (64 - _leadz(k)) - (((k-1) & k) == 0) )
#endif

_f_int1 _ILEN_I1_I1(_f_int1);
_f_int2 _ILEN_I2_I2(_f_int2);
_f_int4 _ILEN_I4_I4(_f_int4);
_f_int8 _ILEN_I8_I8(_f_int8);

/* Function:   _ILEN_I1_I1
 * Input:      8 bit integer
 * Output:     8 bit integer
 * Task:  Returns the length, in bits of the
 *        two's compliment representation of
 *        the integer argument i.
 */
_f_int1 _ILEN_I1_I1(_f_int1 i)
{
  if(i < 0)
    i = -i;
  else
    i = i + 1;

  return (_f_int1) LOG2( (uint64) i);
}

/* Function:   _ILEN_I2_I2
 * Input:      16 bit integer
 * Output:     16 bit integer
 */
_f_int2 _ILEN_I2_I2(_f_int2 i)
{
  if(i < 0)
    i = -i;
  else
    i = i + 1;

  return (_f_int2) LOG2( (uint64) i);
}

/* Function:  _ILEN_I4_I4
 * Input:     32 bit integer
 * Output:    32 bit integer
 */
_f_int4 _ILEN_I4_I4(_f_int4 i)
{
  if(i < 0)
    i = -i;
  else
    i = i + 1;

  return (_f_int4) LOG2( (uint64) i);
}

/* Function:  _ILEN_I8_I8
 * Input:     64 bit integer
 * Output:    64 bit integer
 */
_f_int8 _ILEN_I8_I8(_f_int8 i)
{
  if(i < 0)
    i = -i;
  else
    i = i + 1;

  return (_f_int8) LOG2( (uint64) i);
}
