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


static const char USMID[] = "@(#) libu/util/posix/utilintrin.c	92.0	10/08/98 14:57:41";


#include <fortran.h>
#include <clibdefs.h>
#include <cray/portdefs.h>

static int32 numbits[256] = {
/*   0 */    0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
/*  16 */    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
/*  32 */    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
/*  48 */    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
/*  64 */    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
/*  80 */    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
/*  96 */    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
/* 112 */    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
/* 128 */    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
/* 144 */    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
/* 160 */    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
/* 176 */    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
/* 192 */    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
/* 208 */    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
/* 224 */    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
/* 240 */    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
};

int32 __popcnt1(uint8 x)
{
   int32 r;
   r = numbits[x & 255];
   return (r);
}

int32 __popcnt2(uint16 x)
{
   int32 r;
   r = numbits[x & 255] + numbits[(x>>8) & 255];
   return (r);
}

int32 __popcnt4(uint32 x)
{
   int32 r;
   r = numbits[x & 255] + numbits[(x>>8) & 255] + numbits[(x>>16) &
 255] + 
     numbits[(x>>24) & 255];
   return (r);
}

int32 __popcnt8(uint64 x)
{
   int32 r;
   r = numbits[x & 255] + numbits[(x>>8) & 255] + numbits[(x>>16) &
 255] + 
     numbits[(x>>24) & 255] + numbits[(x>>32) & 255] + numbits[(x>>
40) & 255] + 
       numbits[(x>>48) & 255] + numbits[(x>>56) & 255];
   return (r);
}


int32 __poppar1(uint8 x)
{
   x ^= x >> 4;
   return (0x6996 >> (x & 15)) & 1;
}

int32 __poppar2(uint16 x)
{
   x ^= x >> 8;
   x ^= x >> 4;
   return (0x6996 >> (x & 15)) & 1;
}

int32 __poppar4(uint32 x)
{
   x ^= x >> 16;
   x ^= x >> 8;
   x ^= x >> 4;
   return (0x6996 >> (x & 15)) & 1;
}

int32 __poppar8(uint64 x)
{
   x ^= x >> 32;
   x ^= x >> 16;
   x ^= x >> 8;
   x ^= x >> 4;
   return (0x6996 >> (x & 15)) & 1;
}


int32 __leadz1(uint8 x)
{
   int32 r,t;
   if (x == 0) return (8);
   r = 0;
   for (t=4; t > 0; t >>= 1) {
      if ((x >> t) == 0) {
         r += t;
      } else {
         x >>= t;
      }
   }
   return (r);
}

int32 __leadz2(uint16 x)
{
   int32 r,t;
   if (x == 0) return (16);
   r = 0;
   for (t=8; t > 0; t >>= 1) {
      if ((x >> t) == 0) {
         r += t;
      } else {
         x >>= t;
      }
   }
   return (r);
}

int32 __leadz4(uint32 x)
{
   int32 r,t;
   if (x == 0) return (32);
   r = 0;
   for (t=16; t > 0; t >>= 1) {
      if ((x >> t) == 0) {
         r += t;
      } else {
         x >>= t;
      }
   }
   return (r);
}

int32 __leadz8(uint64 x)
{
   int32 r,t;
   if (x == 0) return (64);
   r = 0;
   for (t=32; t > 0; t >>= 1) {
      if ((x >> t) == 0) {
         r += t;
      } else {
         x >>= t;
      }
   }
   return (r);
}

