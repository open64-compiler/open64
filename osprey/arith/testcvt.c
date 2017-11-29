/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#include <stdio.h>
#include "arith.h"

#ifndef _CRAY
main(int argc, char *argv[])
{
   printf("%s:  cannot be run on this host machine type.\n", argv[0]);
   exit (0);
}
#else  /* _CRAY */

/* We're running on a Cray machine */

#if _RELEASE < 4
#error  Must be compiled with Cray Standard C Compiler, version 4.0 or later.
#endif

/* evaluates to true if a signed int exceeds 46 bits of precision */
#define INT_OVERFLOWS_46_BITS(OPND) \
   (((OPND) & 0xffffe00000000000) != 0xffffe00000000000 && \
    ((OPND) & 0xffffe00000000000) != 0)


typedef unsigned long UINT64;
typedef signed long SINT64;
typedef signed int SINT46;
typedef unsigned short UINT32;
typedef signed short SINT32;
typedef unsigned char UINT8;
typedef signed char SINT8;

typedef struct {
	unsigned int part1: 16;
	unsigned int part2: 16;
	unsigned int part3: 16;
	unsigned int part4: 16;
} AR_INT_64;

AR_INT_64 cool_64_ints[] =
{
   /* march a one bit though a positive 64-bit int */
   {0x0000, 0x0000, 0x0000, 0x0000},
   {0x0000, 0x0000, 0x0000, 0x0001},
   {0x0000, 0x0000, 0x0000, 0x0002},
   {0x0000, 0x0000, 0x0000, 0x0004},
   {0x0000, 0x0000, 0x0000, 0x0008},
   {0x0000, 0x0000, 0x0000, 0x0010},
   {0x0000, 0x0000, 0x0000, 0x0020},
   {0x0000, 0x0000, 0x0000, 0x0040},
   {0x0000, 0x0000, 0x0000, 0x0080},
   {0x0000, 0x0000, 0x0000, 0x0100},
   {0x0000, 0x0000, 0x0000, 0x0200},
   {0x0000, 0x0000, 0x0000, 0x0400},
   {0x0000, 0x0000, 0x0000, 0x0800},
   {0x0000, 0x0000, 0x0000, 0x1000},
   {0x0000, 0x0000, 0x0000, 0x2000},
   {0x0000, 0x0000, 0x0000, 0x4000},
   {0x0000, 0x0000, 0x0000, 0x8000},
   {0x0000, 0x0000, 0x0001, 0x0000},
   {0x0000, 0x0000, 0x0002, 0x0000},
   {0x0000, 0x0000, 0x0004, 0x0000},
   {0x0000, 0x0000, 0x0008, 0x0000},
   {0x0000, 0x0000, 0x0010, 0x0000},
   {0x0000, 0x0000, 0x0020, 0x0000},
   {0x0000, 0x0000, 0x0040, 0x0000},
   {0x0000, 0x0000, 0x0080, 0x0000},
   {0x0000, 0x0000, 0x0100, 0x0000},
   {0x0000, 0x0000, 0x0200, 0x0000},
   {0x0000, 0x0000, 0x0400, 0x0000},
   {0x0000, 0x0000, 0x0800, 0x0000},
   {0x0000, 0x0000, 0x1000, 0x0000},
   {0x0000, 0x0000, 0x2000, 0x0000},
   {0x0000, 0x0000, 0x4000, 0x0000},
   {0x0000, 0x0000, 0x8000, 0x0000},
   {0x0000, 0x0001, 0x0000, 0x0000},
   {0x0000, 0x0002, 0x0000, 0x0000},
   {0x0000, 0x0004, 0x0000, 0x0000},
   {0x0000, 0x0008, 0x0000, 0x0000},
   {0x0000, 0x0010, 0x0000, 0x0000},
   {0x0000, 0x0020, 0x0000, 0x0000},
   {0x0000, 0x0040, 0x0000, 0x0000},
   {0x0000, 0x0080, 0x0000, 0x0000},
   {0x0000, 0x0100, 0x0000, 0x0000},
   {0x0000, 0x0200, 0x0000, 0x0000},
   {0x0000, 0x0400, 0x0000, 0x0000},
   {0x0000, 0x0800, 0x0000, 0x0000},
   {0x0000, 0x1000, 0x0000, 0x0000},
   {0x0000, 0x2000, 0x0000, 0x0000},
   {0x0000, 0x4000, 0x0000, 0x0000},
   {0x0000, 0x8000, 0x0000, 0x0000},
   {0x0001, 0x0000, 0x0000, 0x0000},
   {0x0002, 0x0000, 0x0000, 0x0000},
   {0x0004, 0x0000, 0x0000, 0x0000},
   {0x0008, 0x0000, 0x0000, 0x0000},
   {0x0010, 0x0000, 0x0000, 0x0000},
   {0x0020, 0x0000, 0x0000, 0x0000},
   {0x0040, 0x0000, 0x0000, 0x0000},
   {0x0080, 0x0000, 0x0000, 0x0000},
   {0x0100, 0x0000, 0x0000, 0x0000},
   {0x0200, 0x0000, 0x0000, 0x0000},
   {0x0400, 0x0000, 0x0000, 0x0000},
   {0x0800, 0x0000, 0x0000, 0x0000},
   {0x1000, 0x0000, 0x0000, 0x0000},
   {0x2000, 0x0000, 0x0000, 0x0000},
   {0x4000, 0x0000, 0x0000, 0x0000},
   {0x8000, 0x0000, 0x0000, 0x0000},

   /* march a one bit though a negative 64-bit int */
/* {0x8000, 0x0000, 0x0000, 0x0000}, */  /* already have this one */
   {0x8000, 0x0000, 0x0000, 0x0001},
   {0x8000, 0x0000, 0x0000, 0x0002},
   {0x8000, 0x0000, 0x0000, 0x0004},
   {0x8000, 0x0000, 0x0000, 0x0008},
   {0x8000, 0x0000, 0x0000, 0x0010},
   {0x8000, 0x0000, 0x0000, 0x0020},
   {0x8000, 0x0000, 0x0000, 0x0040},
   {0x8000, 0x0000, 0x0000, 0x0080},
   {0x8000, 0x0000, 0x0000, 0x0100},
   {0x8000, 0x0000, 0x0000, 0x0200},
   {0x8000, 0x0000, 0x0000, 0x0400},
   {0x8000, 0x0000, 0x0000, 0x0800},
   {0x8000, 0x0000, 0x0000, 0x1000},
   {0x8000, 0x0000, 0x0000, 0x2000},
   {0x8000, 0x0000, 0x0000, 0x4000},
   {0x8000, 0x0000, 0x0000, 0x8000},
   {0x8000, 0x0000, 0x0001, 0x0000},
   {0x8000, 0x0000, 0x0002, 0x0000},
   {0x8000, 0x0000, 0x0004, 0x0000},
   {0x8000, 0x0000, 0x0008, 0x0000},
   {0x8000, 0x0000, 0x0010, 0x0000},
   {0x8000, 0x0000, 0x0020, 0x0000},
   {0x8000, 0x0000, 0x0040, 0x0000},
   {0x8000, 0x0000, 0x0080, 0x0000},
   {0x8000, 0x0000, 0x0100, 0x0000},
   {0x8000, 0x0000, 0x0200, 0x0000},
   {0x8000, 0x0000, 0x0400, 0x0000},
   {0x8000, 0x0000, 0x0800, 0x0000},
   {0x8000, 0x0000, 0x1000, 0x0000},
   {0x8000, 0x0000, 0x2000, 0x0000},
   {0x8000, 0x0000, 0x4000, 0x0000},
   {0x8000, 0x0000, 0x8000, 0x0000},
   {0x8000, 0x0001, 0x0000, 0x0000},
   {0x8000, 0x0002, 0x0000, 0x0000},
   {0x8000, 0x0004, 0x0000, 0x0000},
   {0x8000, 0x0008, 0x0000, 0x0000},
   {0x8000, 0x0010, 0x0000, 0x0000},
   {0x8000, 0x0020, 0x0000, 0x0000},
   {0x8000, 0x0040, 0x0000, 0x0000},
   {0x8000, 0x0080, 0x0000, 0x0000},
   {0x8000, 0x0100, 0x0000, 0x0000},
   {0x8000, 0x0200, 0x0000, 0x0000},
   {0x8000, 0x0400, 0x0000, 0x0000},
   {0x8000, 0x0800, 0x0000, 0x0000},
   {0x8000, 0x1000, 0x0000, 0x0000},
   {0x8000, 0x2000, 0x0000, 0x0000},
   {0x8000, 0x4000, 0x0000, 0x0000},
   {0x8000, 0x8000, 0x0000, 0x0000},
   {0x8001, 0x0000, 0x0000, 0x0000},
   {0x8002, 0x0000, 0x0000, 0x0000},
   {0x8004, 0x0000, 0x0000, 0x0000},
   {0x8008, 0x0000, 0x0000, 0x0000},
   {0x8010, 0x0000, 0x0000, 0x0000},
   {0x8020, 0x0000, 0x0000, 0x0000},
   {0x8040, 0x0000, 0x0000, 0x0000},
   {0x8080, 0x0000, 0x0000, 0x0000},
   {0x8100, 0x0000, 0x0000, 0x0000},
   {0x8200, 0x0000, 0x0000, 0x0000},
   {0x8400, 0x0000, 0x0000, 0x0000},
   {0x8800, 0x0000, 0x0000, 0x0000},
   {0x9000, 0x0000, 0x0000, 0x0000},
   {0xa000, 0x0000, 0x0000, 0x0000},
   {0xc000, 0x0000, 0x0000, 0x0000},
/* {0x8000, 0x0000, 0x0000, 0x0000}, */  /* already have this one */

   /* shift 64 one bits through a 64-bit positive int */
/* {0x0000, 0x0000, 0x0000, 0x0001}, */  /* already have this one */
   {0x0000, 0x0000, 0x0000, 0x0003},
   {0x0000, 0x0000, 0x0000, 0x0007},
   {0x0000, 0x0000, 0x0000, 0x000f},
   {0x0000, 0x0000, 0x0000, 0x001f},
   {0x0000, 0x0000, 0x0000, 0x003f},
   {0x0000, 0x0000, 0x0000, 0x007f},
   {0x0000, 0x0000, 0x0000, 0x00ff},
   {0x0000, 0x0000, 0x0000, 0x01ff},
   {0x0000, 0x0000, 0x0000, 0x03ff},
   {0x0000, 0x0000, 0x0000, 0x07ff},
   {0x0000, 0x0000, 0x0000, 0x0fff},
   {0x0000, 0x0000, 0x0000, 0x1fff},
   {0x0000, 0x0000, 0x0000, 0x3fff},
   {0x0000, 0x0000, 0x0000, 0x7fff},
   {0x0000, 0x0000, 0x0000, 0xffff},
   {0x0000, 0x0000, 0x0001, 0xffff},
   {0x0000, 0x0000, 0x0003, 0xffff},
   {0x0000, 0x0000, 0x0007, 0xffff},
   {0x0000, 0x0000, 0x000f, 0xffff},
   {0x0000, 0x0000, 0x001f, 0xffff},
   {0x0000, 0x0000, 0x003f, 0xffff},
   {0x0000, 0x0000, 0x007f, 0xffff},
   {0x0000, 0x0000, 0x00ff, 0xffff},
   {0x0000, 0x0000, 0x01ff, 0xffff},
   {0x0000, 0x0000, 0x03ff, 0xffff},
   {0x0000, 0x0000, 0x07ff, 0xffff},
   {0x0000, 0x0000, 0x0fff, 0xffff},
   {0x0000, 0x0000, 0x1fff, 0xffff},
   {0x0000, 0x0000, 0x3fff, 0xffff},
   {0x0000, 0x0000, 0x7fff, 0xffff},
   {0x0000, 0x0000, 0xffff, 0xffff},
   {0x0000, 0x0001, 0xffff, 0xffff},
   {0x0000, 0x0003, 0xffff, 0xffff},
   {0x0000, 0x0007, 0xffff, 0xffff},
   {0x0000, 0x000f, 0xffff, 0xffff},
   {0x0000, 0x001f, 0xffff, 0xffff},
   {0x0000, 0x003f, 0xffff, 0xffff},
   {0x0000, 0x007f, 0xffff, 0xffff},
   {0x0000, 0x00ff, 0xffff, 0xffff},
   {0x0000, 0x01ff, 0xffff, 0xffff},
   {0x0000, 0x03ff, 0xffff, 0xffff},
   {0x0000, 0x07ff, 0xffff, 0xffff},
   {0x0000, 0x0fff, 0xffff, 0xffff},
   {0x0000, 0x1fff, 0xffff, 0xffff},
   {0x0000, 0x3fff, 0xffff, 0xffff},
   {0x0000, 0x7fff, 0xffff, 0xffff},
   {0x0000, 0xffff, 0xffff, 0xffff},
   {0x0001, 0xffff, 0xffff, 0xffff},
   {0x0003, 0xffff, 0xffff, 0xffff},
   {0x0007, 0xffff, 0xffff, 0xffff},
   {0x000f, 0xffff, 0xffff, 0xffff},
   {0x001f, 0xffff, 0xffff, 0xffff},
   {0x003f, 0xffff, 0xffff, 0xffff},
   {0x007f, 0xffff, 0xffff, 0xffff},
   {0x00ff, 0xffff, 0xffff, 0xffff},
   {0x01ff, 0xffff, 0xffff, 0xffff},
   {0x03ff, 0xffff, 0xffff, 0xffff},
   {0x07ff, 0xffff, 0xffff, 0xffff},
   {0x0fff, 0xffff, 0xffff, 0xffff},
   {0x1fff, 0xffff, 0xffff, 0xffff},
   {0x3fff, 0xffff, 0xffff, 0xffff},
   {0x7fff, 0xffff, 0xffff, 0xffff},
   {0x7fff, 0xffff, 0xffff, 0xfffe},
   {0x7fff, 0xffff, 0xffff, 0xfffc},
   {0x7fff, 0xffff, 0xffff, 0xfff8},
   {0x7fff, 0xffff, 0xffff, 0xfff0},
   {0x7fff, 0xffff, 0xffff, 0xffe0},
   {0x7fff, 0xffff, 0xffff, 0xffc0},
   {0x7fff, 0xffff, 0xffff, 0xff80},
   {0x7fff, 0xffff, 0xffff, 0xff00},
   {0x7fff, 0xffff, 0xffff, 0xfe00},
   {0x7fff, 0xffff, 0xffff, 0xfc00},
   {0x7fff, 0xffff, 0xffff, 0xf800},
   {0x7fff, 0xffff, 0xffff, 0xf000},
   {0x7fff, 0xffff, 0xffff, 0xe000},
   {0x7fff, 0xffff, 0xffff, 0xc000},
   {0x7fff, 0xffff, 0xffff, 0x8000},
   {0x7fff, 0xffff, 0xffff, 0x0000},
   {0x7fff, 0xffff, 0xfffe, 0x0000},
   {0x7fff, 0xffff, 0xfffc, 0x0000},
   {0x7fff, 0xffff, 0xfff8, 0x0000},
   {0x7fff, 0xffff, 0xfff0, 0x0000},
   {0x7fff, 0xffff, 0xffe0, 0x0000},
   {0x7fff, 0xffff, 0xffc0, 0x0000},
   {0x7fff, 0xffff, 0xff80, 0x0000},
   {0x7fff, 0xffff, 0xff00, 0x0000},
   {0x7fff, 0xffff, 0xfe00, 0x0000},
   {0x7fff, 0xffff, 0xfc00, 0x0000},
   {0x7fff, 0xffff, 0xf800, 0x0000},
   {0x7fff, 0xffff, 0xf000, 0x0000},
   {0x7fff, 0xffff, 0xe000, 0x0000},
   {0x7fff, 0xffff, 0xc000, 0x0000},
   {0x7fff, 0xffff, 0x8000, 0x0000},
   {0x7fff, 0xffff, 0x0000, 0x0000},
   {0x7fff, 0xfffe, 0x0000, 0x0000},
   {0x7fff, 0xfffc, 0x0000, 0x0000},
   {0x7fff, 0xfff8, 0x0000, 0x0000},
   {0x7fff, 0xfff0, 0x0000, 0x0000},
   {0x7fff, 0xffe0, 0x0000, 0x0000},
   {0x7fff, 0xffc0, 0x0000, 0x0000},
   {0x7fff, 0xff80, 0x0000, 0x0000},
   {0x7fff, 0xff00, 0x0000, 0x0000},
   {0x7fff, 0xfe00, 0x0000, 0x0000},
   {0x7fff, 0xfc00, 0x0000, 0x0000},
   {0x7fff, 0xf800, 0x0000, 0x0000},
   {0x7fff, 0xf000, 0x0000, 0x0000},
   {0x7fff, 0xe000, 0x0000, 0x0000},
   {0x7fff, 0xc000, 0x0000, 0x0000},
   {0x7fff, 0x8000, 0x0000, 0x0000},
   {0x7fff, 0x0000, 0x0000, 0x0000},
   {0x7ffe, 0x0000, 0x0000, 0x0000},
   {0x7ffc, 0x0000, 0x0000, 0x0000},
   {0x7ff8, 0x0000, 0x0000, 0x0000},
   {0x7ff0, 0x0000, 0x0000, 0x0000},
   {0x7fe0, 0x0000, 0x0000, 0x0000},
   {0x7fc0, 0x0000, 0x0000, 0x0000},
   {0x7f80, 0x0000, 0x0000, 0x0000},
   {0x7f00, 0x0000, 0x0000, 0x0000},
   {0x7e00, 0x0000, 0x0000, 0x0000},
   {0x7c00, 0x0000, 0x0000, 0x0000},
   {0x7800, 0x0000, 0x0000, 0x0000},
   {0x7000, 0x0000, 0x0000, 0x0000},
   {0x6000, 0x0000, 0x0000, 0x0000},
   {0x4000, 0x0000, 0x0000, 0x0000},

   /* shift 64 one bits through a 64-bit negative int */
   {0x8000, 0x0000, 0x0000, 0x0001},
   {0x8000, 0x0000, 0x0000, 0x0003},
   {0x8000, 0x0000, 0x0000, 0x0007},
   {0x8000, 0x0000, 0x0000, 0x000f},
   {0x8000, 0x0000, 0x0000, 0x001f},
   {0x8000, 0x0000, 0x0000, 0x003f},
   {0x8000, 0x0000, 0x0000, 0x007f},
   {0x8000, 0x0000, 0x0000, 0x00ff},
   {0x8000, 0x0000, 0x0000, 0x01ff},
   {0x8000, 0x0000, 0x0000, 0x03ff},
   {0x8000, 0x0000, 0x0000, 0x07ff},
   {0x8000, 0x0000, 0x0000, 0x0fff},
   {0x8000, 0x0000, 0x0000, 0x1fff},
   {0x8000, 0x0000, 0x0000, 0x3fff},
   {0x8000, 0x0000, 0x0000, 0x7fff},
   {0x8000, 0x0000, 0x0000, 0xffff},
   {0x8000, 0x0000, 0x0001, 0xffff},
   {0x8000, 0x0000, 0x0003, 0xffff},
   {0x8000, 0x0000, 0x0007, 0xffff},
   {0x8000, 0x0000, 0x000f, 0xffff},
   {0x8000, 0x0000, 0x001f, 0xffff},
   {0x8000, 0x0000, 0x003f, 0xffff},
   {0x8000, 0x0000, 0x007f, 0xffff},
   {0x8000, 0x0000, 0x00ff, 0xffff},
   {0x8000, 0x0000, 0x01ff, 0xffff},
   {0x8000, 0x0000, 0x03ff, 0xffff},
   {0x8000, 0x0000, 0x07ff, 0xffff},
   {0x8000, 0x0000, 0x0fff, 0xffff},
   {0x8000, 0x0000, 0x1fff, 0xffff},
   {0x8000, 0x0000, 0x3fff, 0xffff},
   {0x8000, 0x0000, 0x7fff, 0xffff},
   {0x8000, 0x0000, 0xffff, 0xffff},
   {0x8000, 0x0001, 0xffff, 0xffff},
   {0x8000, 0x0003, 0xffff, 0xffff},
   {0x8000, 0x0007, 0xffff, 0xffff},
   {0x8000, 0x000f, 0xffff, 0xffff},
   {0x8000, 0x001f, 0xffff, 0xffff},
   {0x8000, 0x003f, 0xffff, 0xffff},
   {0x8000, 0x007f, 0xffff, 0xffff},
   {0x8000, 0x00ff, 0xffff, 0xffff},
   {0x8000, 0x01ff, 0xffff, 0xffff},
   {0x8000, 0x03ff, 0xffff, 0xffff},
   {0x8000, 0x07ff, 0xffff, 0xffff},
   {0x8000, 0x0fff, 0xffff, 0xffff},
   {0x8000, 0x1fff, 0xffff, 0xffff},
   {0x8000, 0x3fff, 0xffff, 0xffff},
   {0x8000, 0x7fff, 0xffff, 0xffff},
   {0x8000, 0xffff, 0xffff, 0xffff},
   {0x8001, 0xffff, 0xffff, 0xffff},
   {0x8003, 0xffff, 0xffff, 0xffff},
   {0x8007, 0xffff, 0xffff, 0xffff},
   {0x800f, 0xffff, 0xffff, 0xffff},
   {0x801f, 0xffff, 0xffff, 0xffff},
   {0x803f, 0xffff, 0xffff, 0xffff},
   {0x807f, 0xffff, 0xffff, 0xffff},
   {0x80ff, 0xffff, 0xffff, 0xffff},
   {0x81ff, 0xffff, 0xffff, 0xffff},
   {0x83ff, 0xffff, 0xffff, 0xffff},
   {0x87ff, 0xffff, 0xffff, 0xffff},
   {0x8fff, 0xffff, 0xffff, 0xffff},
   {0x9fff, 0xffff, 0xffff, 0xffff},
   {0xbfff, 0xffff, 0xffff, 0xffff},
   {0xffff, 0xffff, 0xffff, 0xffff},
   {0xffff, 0xffff, 0xffff, 0xfffe},
   {0xffff, 0xffff, 0xffff, 0xfffc},
   {0xffff, 0xffff, 0xffff, 0xfff8},
   {0xffff, 0xffff, 0xffff, 0xfff0},
   {0xffff, 0xffff, 0xffff, 0xffe0},
   {0xffff, 0xffff, 0xffff, 0xffc0},
   {0xffff, 0xffff, 0xffff, 0xff80},
   {0xffff, 0xffff, 0xffff, 0xff00},
   {0xffff, 0xffff, 0xffff, 0xfe00},
   {0xffff, 0xffff, 0xffff, 0xfc00},
   {0xffff, 0xffff, 0xffff, 0xf800},
   {0xffff, 0xffff, 0xffff, 0xf000},
   {0xffff, 0xffff, 0xffff, 0xe000},
   {0xffff, 0xffff, 0xffff, 0xc000},
   {0xffff, 0xffff, 0xffff, 0x8000},
   {0xffff, 0xffff, 0xffff, 0x0000},
   {0xffff, 0xffff, 0xfffe, 0x0000},
   {0xffff, 0xffff, 0xfffc, 0x0000},
   {0xffff, 0xffff, 0xfff8, 0x0000},
   {0xffff, 0xffff, 0xfff0, 0x0000},
   {0xffff, 0xffff, 0xffe0, 0x0000},
   {0xffff, 0xffff, 0xffc0, 0x0000},
   {0xffff, 0xffff, 0xff80, 0x0000},
   {0xffff, 0xffff, 0xff00, 0x0000},
   {0xffff, 0xffff, 0xfe00, 0x0000},
   {0xffff, 0xffff, 0xfc00, 0x0000},
   {0xffff, 0xffff, 0xf800, 0x0000},
   {0xffff, 0xffff, 0xf000, 0x0000},
   {0xffff, 0xffff, 0xe000, 0x0000},
   {0xffff, 0xffff, 0xc000, 0x0000},
   {0xffff, 0xffff, 0x8000, 0x0000},
   {0xffff, 0xffff, 0x0000, 0x0000},
   {0xffff, 0xfffe, 0x0000, 0x0000},
   {0xffff, 0xfffc, 0x0000, 0x0000},
   {0xffff, 0xfff8, 0x0000, 0x0000},
   {0xffff, 0xfff0, 0x0000, 0x0000},
   {0xffff, 0xffe0, 0x0000, 0x0000},
   {0xffff, 0xffc0, 0x0000, 0x0000},
   {0xffff, 0xff80, 0x0000, 0x0000},
   {0xffff, 0xff00, 0x0000, 0x0000},
   {0xffff, 0xfe00, 0x0000, 0x0000},
   {0xffff, 0xfc00, 0x0000, 0x0000},
   {0xffff, 0xf800, 0x0000, 0x0000},
   {0xffff, 0xf000, 0x0000, 0x0000},
   {0xffff, 0xe000, 0x0000, 0x0000},
   {0xffff, 0xc000, 0x0000, 0x0000},
   {0xffff, 0x8000, 0x0000, 0x0000},
   {0xffff, 0x0000, 0x0000, 0x0000},
   {0xfffe, 0x0000, 0x0000, 0x0000},
   {0xfffc, 0x0000, 0x0000, 0x0000},
   {0xfff8, 0x0000, 0x0000, 0x0000},
   {0xfff0, 0x0000, 0x0000, 0x0000},
   {0xffe0, 0x0000, 0x0000, 0x0000},
   {0xffc0, 0x0000, 0x0000, 0x0000},
   {0xff80, 0x0000, 0x0000, 0x0000},
   {0xff00, 0x0000, 0x0000, 0x0000},
   {0xfe00, 0x0000, 0x0000, 0x0000},
   {0xfc00, 0x0000, 0x0000, 0x0000},
   {0xf800, 0x0000, 0x0000, 0x0000},
   {0xf000, 0x0000, 0x0000, 0x0000},
   {0xe000, 0x0000, 0x0000, 0x0000},
   {0xc000, 0x0000, 0x0000, 0x0000}
};
int num_cool_64_ints = sizeof(cool_64_ints)/sizeof(AR_INT_64);

AR_INT_64 cool_32_ints[] =
{
   /* march a one bit though a positive 32-bit int */
   {0x0000, 0x0000, 0x0000, 0x0000},
   {0x0000, 0x0000, 0x0000, 0x0001},
   {0x0000, 0x0000, 0x0000, 0x0002},
   {0x0000, 0x0000, 0x0000, 0x0004},
   {0x0000, 0x0000, 0x0000, 0x0008},
   {0x0000, 0x0000, 0x0000, 0x0010},
   {0x0000, 0x0000, 0x0000, 0x0020},
   {0x0000, 0x0000, 0x0000, 0x0040},
   {0x0000, 0x0000, 0x0000, 0x0080},
   {0x0000, 0x0000, 0x0000, 0x0100},
   {0x0000, 0x0000, 0x0000, 0x0200},
   {0x0000, 0x0000, 0x0000, 0x0400},
   {0x0000, 0x0000, 0x0000, 0x0800},
   {0x0000, 0x0000, 0x0000, 0x1000},
   {0x0000, 0x0000, 0x0000, 0x2000},
   {0x0000, 0x0000, 0x0000, 0x4000},
   {0x0000, 0x0000, 0x0000, 0x8000},
   {0x0000, 0x0000, 0x0001, 0x0000},
   {0x0000, 0x0000, 0x0002, 0x0000},
   {0x0000, 0x0000, 0x0004, 0x0000},
   {0x0000, 0x0000, 0x0008, 0x0000},
   {0x0000, 0x0000, 0x0010, 0x0000},
   {0x0000, 0x0000, 0x0020, 0x0000},
   {0x0000, 0x0000, 0x0040, 0x0000},
   {0x0000, 0x0000, 0x0080, 0x0000},
   {0x0000, 0x0000, 0x0100, 0x0000},
   {0x0000, 0x0000, 0x0200, 0x0000},
   {0x0000, 0x0000, 0x0400, 0x0000},
   {0x0000, 0x0000, 0x0800, 0x0000},
   {0x0000, 0x0000, 0x1000, 0x0000},
   {0x0000, 0x0000, 0x2000, 0x0000},
   {0x0000, 0x0000, 0x4000, 0x0000},
   {0x0000, 0x0000, 0x8000, 0x0000},

   /* march a one bit though a negative 32-bit int */
/* {0x0000, 0x0000, 0x8000, 0x0000}, */  /* already have this one */
   {0x0000, 0x0000, 0x8000, 0x0001},
   {0x0000, 0x0000, 0x8000, 0x0002},
   {0x0000, 0x0000, 0x8000, 0x0004},
   {0x0000, 0x0000, 0x8000, 0x0008},
   {0x0000, 0x0000, 0x8000, 0x0010},
   {0x0000, 0x0000, 0x8000, 0x0020},
   {0x0000, 0x0000, 0x8000, 0x0040},
   {0x0000, 0x0000, 0x8000, 0x0080},
   {0x0000, 0x0000, 0x8000, 0x0100},
   {0x0000, 0x0000, 0x8000, 0x0200},
   {0x0000, 0x0000, 0x8000, 0x0400},
   {0x0000, 0x0000, 0x8000, 0x0800},
   {0x0000, 0x0000, 0x8000, 0x1000},
   {0x0000, 0x0000, 0x8000, 0x2000},
   {0x0000, 0x0000, 0x8000, 0x4000},
   {0x0000, 0x0000, 0x8000, 0x8000},
   {0x0000, 0x0000, 0x8001, 0x0000},
   {0x0000, 0x0000, 0x8002, 0x0000},
   {0x0000, 0x0000, 0x8004, 0x0000},
   {0x0000, 0x0000, 0x8008, 0x0000},
   {0x0000, 0x0000, 0x8010, 0x0000},
   {0x0000, 0x0000, 0x8020, 0x0000},
   {0x0000, 0x0000, 0x8040, 0x0000},
   {0x0000, 0x0000, 0x8080, 0x0000},
   {0x0000, 0x0000, 0x8100, 0x0000},
   {0x0000, 0x0000, 0x8200, 0x0000},
   {0x0000, 0x0000, 0x8400, 0x0000},
   {0x0000, 0x0000, 0x8800, 0x0000},
   {0x0000, 0x0000, 0x9000, 0x0000},
   {0x0000, 0x0000, 0xa000, 0x0000},
   {0x0000, 0x0000, 0xc000, 0x0000},
/* {0x0000, 0x0000, 0x8000, 0x0000}, */  /* already have this one */

   /* shift 32 one bits through a 32-bit positive int */
/* {0x0000, 0x0000, 0x0000, 0x0001}, */  /* already have this one */
   {0x0000, 0x0000, 0x0000, 0x0003},
   {0x0000, 0x0000, 0x0000, 0x0007},
   {0x0000, 0x0000, 0x0000, 0x000f},
   {0x0000, 0x0000, 0x0000, 0x001f},
   {0x0000, 0x0000, 0x0000, 0x003f},
   {0x0000, 0x0000, 0x0000, 0x007f},
   {0x0000, 0x0000, 0x0000, 0x00ff},
   {0x0000, 0x0000, 0x0000, 0x01ff},
   {0x0000, 0x0000, 0x0000, 0x03ff},
   {0x0000, 0x0000, 0x0000, 0x07ff},
   {0x0000, 0x0000, 0x0000, 0x0fff},
   {0x0000, 0x0000, 0x0000, 0x1fff},
   {0x0000, 0x0000, 0x0000, 0x3fff},
   {0x0000, 0x0000, 0x0000, 0x7fff},
   {0x0000, 0x0000, 0x0000, 0xffff},
   {0x0000, 0x0000, 0x0001, 0xffff},
   {0x0000, 0x0000, 0x0003, 0xffff},
   {0x0000, 0x0000, 0x0007, 0xffff},
   {0x0000, 0x0000, 0x000f, 0xffff},
   {0x0000, 0x0000, 0x001f, 0xffff},
   {0x0000, 0x0000, 0x003f, 0xffff},
   {0x0000, 0x0000, 0x007f, 0xffff},
   {0x0000, 0x0000, 0x00ff, 0xffff},
   {0x0000, 0x0000, 0x01ff, 0xffff},
   {0x0000, 0x0000, 0x03ff, 0xffff},
   {0x0000, 0x0000, 0x07ff, 0xffff},
   {0x0000, 0x0000, 0x0fff, 0xffff},
   {0x0000, 0x0000, 0x1fff, 0xffff},
   {0x0000, 0x0000, 0x3fff, 0xffff},
   {0x0000, 0x0000, 0x7fff, 0xffff},
   {0x0000, 0x0000, 0x7fff, 0xfffe},
   {0x0000, 0x0000, 0x7fff, 0xfffc},
   {0x0000, 0x0000, 0x7fff, 0xfff8},
   {0x0000, 0x0000, 0x7fff, 0xfff0},
   {0x0000, 0x0000, 0x7fff, 0xffe0},
   {0x0000, 0x0000, 0x7fff, 0xffc0},
   {0x0000, 0x0000, 0x7fff, 0xff80},
   {0x0000, 0x0000, 0x7fff, 0xff00},
   {0x0000, 0x0000, 0x7fff, 0xfe00},
   {0x0000, 0x0000, 0x7fff, 0xfc00},
   {0x0000, 0x0000, 0x7fff, 0xf800},
   {0x0000, 0x0000, 0x7fff, 0xf000},
   {0x0000, 0x0000, 0x7fff, 0xe000},
   {0x0000, 0x0000, 0x7fff, 0xc000},
   {0x0000, 0x0000, 0x7fff, 0x8000},
   {0x0000, 0x0000, 0x7fff, 0x0000},
   {0x0000, 0x0000, 0x7ffe, 0x0000},
   {0x0000, 0x0000, 0x7ffc, 0x0000},
   {0x0000, 0x0000, 0x7ff8, 0x0000},
   {0x0000, 0x0000, 0x7ff0, 0x0000},
   {0x0000, 0x0000, 0x7fe0, 0x0000},
   {0x0000, 0x0000, 0x7fc0, 0x0000},
   {0x0000, 0x0000, 0x7f80, 0x0000},
   {0x0000, 0x0000, 0x7f00, 0x0000},
   {0x0000, 0x0000, 0x7e00, 0x0000},
   {0x0000, 0x0000, 0x7c00, 0x0000},
   {0x0000, 0x0000, 0x7800, 0x0000},
   {0x0000, 0x0000, 0x7000, 0x0000},
   {0x0000, 0x0000, 0x6000, 0x0000},
   {0x0000, 0x0000, 0x4000, 0x0000},

   /* shift 32 one bits through a 32-bit negative int */
   {0x0000, 0x0000, 0x8000, 0x0001},
   {0x0000, 0x0000, 0x8000, 0x0003},
   {0x0000, 0x0000, 0x8000, 0x0007},
   {0x0000, 0x0000, 0x8000, 0x000f},
   {0x0000, 0x0000, 0x8000, 0x001f},
   {0x0000, 0x0000, 0x8000, 0x003f},
   {0x0000, 0x0000, 0x8000, 0x007f},
   {0x0000, 0x0000, 0x8000, 0x00ff},
   {0x0000, 0x0000, 0x8000, 0x01ff},
   {0x0000, 0x0000, 0x8000, 0x03ff},
   {0x0000, 0x0000, 0x8000, 0x07ff},
   {0x0000, 0x0000, 0x8000, 0x0fff},
   {0x0000, 0x0000, 0x8000, 0x1fff},
   {0x0000, 0x0000, 0x8000, 0x3fff},
   {0x0000, 0x0000, 0x8000, 0x7fff},
   {0x0000, 0x0000, 0x8000, 0xffff},
   {0x0000, 0x0000, 0x8001, 0xffff},
   {0x0000, 0x0000, 0x8003, 0xffff},
   {0x0000, 0x0000, 0x8007, 0xffff},
   {0x0000, 0x0000, 0x800f, 0xffff},
   {0x0000, 0x0000, 0x801f, 0xffff},
   {0x0000, 0x0000, 0x803f, 0xffff},
   {0x0000, 0x0000, 0x807f, 0xffff},
   {0x0000, 0x0000, 0x80ff, 0xffff},
   {0x0000, 0x0000, 0x81ff, 0xffff},
   {0x0000, 0x0000, 0x83ff, 0xffff},
   {0x0000, 0x0000, 0x87ff, 0xffff},
   {0x0000, 0x0000, 0x8fff, 0xffff},
   {0x0000, 0x0000, 0x9fff, 0xffff},
   {0x0000, 0x0000, 0xbfff, 0xffff},
   {0x0000, 0x0000, 0xffff, 0xffff},
   {0x0000, 0x0000, 0xffff, 0xfffe},
   {0x0000, 0x0000, 0xffff, 0xfffc},
   {0x0000, 0x0000, 0xffff, 0xfff8},
   {0x0000, 0x0000, 0xffff, 0xfff0},
   {0x0000, 0x0000, 0xffff, 0xffe0},
   {0x0000, 0x0000, 0xffff, 0xffc0},
   {0x0000, 0x0000, 0xffff, 0xff80},
   {0x0000, 0x0000, 0xffff, 0xff00},
   {0x0000, 0x0000, 0xffff, 0xfe00},
   {0x0000, 0x0000, 0xffff, 0xfc00},
   {0x0000, 0x0000, 0xffff, 0xf800},
   {0x0000, 0x0000, 0xffff, 0xf000},
   {0x0000, 0x0000, 0xffff, 0xe000},
   {0x0000, 0x0000, 0xffff, 0xc000},
   {0x0000, 0x0000, 0xffff, 0x8000},
   {0x0000, 0x0000, 0xffff, 0x0000},
   {0x0000, 0x0000, 0xfffe, 0x0000},
   {0x0000, 0x0000, 0xfffc, 0x0000},
   {0x0000, 0x0000, 0xfff8, 0x0000},
   {0x0000, 0x0000, 0xfff0, 0x0000},
   {0x0000, 0x0000, 0xffe0, 0x0000},
   {0x0000, 0x0000, 0xffc0, 0x0000},
   {0x0000, 0x0000, 0xff80, 0x0000},
   {0x0000, 0x0000, 0xff00, 0x0000},
   {0x0000, 0x0000, 0xfe00, 0x0000},
   {0x0000, 0x0000, 0xfc00, 0x0000},
   {0x0000, 0x0000, 0xf800, 0x0000},
   {0x0000, 0x0000, 0xf000, 0x0000},
   {0x0000, 0x0000, 0xe000, 0x0000},
   {0x0000, 0x0000, 0xc000, 0x0000},
   {0x0000, 0x0000, 0x8000, 0x0000}
};
int num_cool_32_ints = sizeof(cool_32_ints)/sizeof(AR_INT_64);

AR_INT_64 cool_24_ints[] =
{
   /* march a one bit though a positive 24-bit int */
   {0x0000, 0x0000, 0x0000, 0x0000},
   {0x0000, 0x0000, 0x0000, 0x0001},
   {0x0000, 0x0000, 0x0000, 0x0002},
   {0x0000, 0x0000, 0x0000, 0x0004},
   {0x0000, 0x0000, 0x0000, 0x0008},
   {0x0000, 0x0000, 0x0000, 0x0010},
   {0x0000, 0x0000, 0x0000, 0x0020},
   {0x0000, 0x0000, 0x0000, 0x0040},
   {0x0000, 0x0000, 0x0000, 0x0080},
   {0x0000, 0x0000, 0x0000, 0x0100},
   {0x0000, 0x0000, 0x0000, 0x0200},
   {0x0000, 0x0000, 0x0000, 0x0400},
   {0x0000, 0x0000, 0x0000, 0x0800},
   {0x0000, 0x0000, 0x0000, 0x1000},
   {0x0000, 0x0000, 0x0000, 0x2000},
   {0x0000, 0x0000, 0x0000, 0x4000},
   {0x0000, 0x0000, 0x0000, 0x8000},
   {0x0000, 0x0000, 0x0001, 0x0000},
   {0x0000, 0x0000, 0x0002, 0x0000},
   {0x0000, 0x0000, 0x0004, 0x0000},
   {0x0000, 0x0000, 0x0008, 0x0000},
   {0x0000, 0x0000, 0x0010, 0x0000},
   {0x0000, 0x0000, 0x0020, 0x0000},
   {0x0000, 0x0000, 0x0040, 0x0000},
   {0x0000, 0x0000, 0x0080, 0x0000},

   /* march a one bit though a negative 24-bit int */
/* {0x0000, 0x0000, 0x0080, 0x0000}, */  /* already have this one */
   {0x0000, 0x0000, 0x0080, 0x0001},
   {0x0000, 0x0000, 0x0080, 0x0002},
   {0x0000, 0x0000, 0x0080, 0x0004},
   {0x0000, 0x0000, 0x0080, 0x0008},
   {0x0000, 0x0000, 0x0080, 0x0010},
   {0x0000, 0x0000, 0x0080, 0x0020},
   {0x0000, 0x0000, 0x0080, 0x0040},
   {0x0000, 0x0000, 0x0080, 0x0080},
   {0x0000, 0x0000, 0x0080, 0x0100},
   {0x0000, 0x0000, 0x0080, 0x0200},
   {0x0000, 0x0000, 0x0080, 0x0400},
   {0x0000, 0x0000, 0x0080, 0x0800},
   {0x0000, 0x0000, 0x0080, 0x1000},
   {0x0000, 0x0000, 0x0080, 0x2000},
   {0x0000, 0x0000, 0x0080, 0x4000},
   {0x0000, 0x0000, 0x0080, 0x8000},
   {0x0000, 0x0000, 0x0081, 0x0000},
   {0x0000, 0x0000, 0x0082, 0x0000},
   {0x0000, 0x0000, 0x0084, 0x0000},
   {0x0000, 0x0000, 0x0088, 0x0000},
   {0x0000, 0x0000, 0x0090, 0x0000},
   {0x0000, 0x0000, 0x00a0, 0x0000},
   {0x0000, 0x0000, 0x00c0, 0x0000},
/* {0x0000, 0x0000, 0x0080, 0x0000}, */  /* already have this one */

   /* shift 24 one bits through a 24-bit positive int */
/* {0x0000, 0x0000, 0x0000, 0x0001}, */  /* already have this one */
   {0x0000, 0x0000, 0x0000, 0x0003},
   {0x0000, 0x0000, 0x0000, 0x0007},
   {0x0000, 0x0000, 0x0000, 0x000f},
   {0x0000, 0x0000, 0x0000, 0x001f},
   {0x0000, 0x0000, 0x0000, 0x003f},
   {0x0000, 0x0000, 0x0000, 0x007f},
   {0x0000, 0x0000, 0x0000, 0x00ff},
   {0x0000, 0x0000, 0x0000, 0x01ff},
   {0x0000, 0x0000, 0x0000, 0x03ff},
   {0x0000, 0x0000, 0x0000, 0x07ff},
   {0x0000, 0x0000, 0x0000, 0x0fff},
   {0x0000, 0x0000, 0x0000, 0x1fff},
   {0x0000, 0x0000, 0x0000, 0x3fff},
   {0x0000, 0x0000, 0x0000, 0x7fff},
   {0x0000, 0x0000, 0x0000, 0xffff},
   {0x0000, 0x0000, 0x0001, 0xffff},
   {0x0000, 0x0000, 0x0003, 0xffff},
   {0x0000, 0x0000, 0x0007, 0xffff},
   {0x0000, 0x0000, 0x000f, 0xffff},
   {0x0000, 0x0000, 0x001f, 0xffff},
   {0x0000, 0x0000, 0x003f, 0xffff},
   {0x0000, 0x0000, 0x007f, 0xffff},
   {0x0000, 0x0000, 0x007f, 0xfffe},
   {0x0000, 0x0000, 0x007f, 0xfffc},
   {0x0000, 0x0000, 0x007f, 0xfff8},
   {0x0000, 0x0000, 0x007f, 0xfff0},
   {0x0000, 0x0000, 0x007f, 0xffe0},
   {0x0000, 0x0000, 0x007f, 0xffc0},
   {0x0000, 0x0000, 0x007f, 0xff80},
   {0x0000, 0x0000, 0x007f, 0xff00},
   {0x0000, 0x0000, 0x007f, 0xfe00},
   {0x0000, 0x0000, 0x007f, 0xfc00},
   {0x0000, 0x0000, 0x007f, 0xf800},
   {0x0000, 0x0000, 0x007f, 0xf000},
   {0x0000, 0x0000, 0x007f, 0xe000},
   {0x0000, 0x0000, 0x007f, 0xc000},
   {0x0000, 0x0000, 0x007f, 0x8000},
   {0x0000, 0x0000, 0x007f, 0x0000},
   {0x0000, 0x0000, 0x007e, 0x0000},
   {0x0000, 0x0000, 0x007c, 0x0000},
   {0x0000, 0x0000, 0x0078, 0x0000},
   {0x0000, 0x0000, 0x0070, 0x0000},
   {0x0000, 0x0000, 0x0060, 0x0000},
   {0x0000, 0x0000, 0x0040, 0x0000},

   /* shift 24 one bits through a 24-bit negative int */
   {0x0000, 0x0000, 0x0080, 0x0001},
   {0x0000, 0x0000, 0x0080, 0x0003},
   {0x0000, 0x0000, 0x0080, 0x0007},
   {0x0000, 0x0000, 0x0080, 0x000f},
   {0x0000, 0x0000, 0x0080, 0x001f},
   {0x0000, 0x0000, 0x0080, 0x003f},
   {0x0000, 0x0000, 0x0080, 0x007f},
   {0x0000, 0x0000, 0x0080, 0x00ff},
   {0x0000, 0x0000, 0x0080, 0x01ff},
   {0x0000, 0x0000, 0x0080, 0x03ff},
   {0x0000, 0x0000, 0x0080, 0x07ff},
   {0x0000, 0x0000, 0x0080, 0x0fff},
   {0x0000, 0x0000, 0x0080, 0x1fff},
   {0x0000, 0x0000, 0x0080, 0x3fff},
   {0x0000, 0x0000, 0x0080, 0x7fff},
   {0x0000, 0x0000, 0x0080, 0xffff},
   {0x0000, 0x0000, 0x0081, 0xffff},
   {0x0000, 0x0000, 0x0083, 0xffff},
   {0x0000, 0x0000, 0x0087, 0xffff},
   {0x0000, 0x0000, 0x008f, 0xffff},
   {0x0000, 0x0000, 0x009f, 0xffff},
   {0x0000, 0x0000, 0x00bf, 0xffff},
   {0x0000, 0x0000, 0x00ff, 0xffff},
   {0x0000, 0x0000, 0x00ff, 0xfffe},
   {0x0000, 0x0000, 0x00ff, 0xfffc},
   {0x0000, 0x0000, 0x00ff, 0xfff8},
   {0x0000, 0x0000, 0x00ff, 0xfff0},
   {0x0000, 0x0000, 0x00ff, 0xffe0},
   {0x0000, 0x0000, 0x00ff, 0xffc0},
   {0x0000, 0x0000, 0x00ff, 0xff80},
   {0x0000, 0x0000, 0x00ff, 0xff00},
   {0x0000, 0x0000, 0x00ff, 0xfe00},
   {0x0000, 0x0000, 0x00ff, 0xfc00},
   {0x0000, 0x0000, 0x00ff, 0xf800},
   {0x0000, 0x0000, 0x00ff, 0xf000},
   {0x0000, 0x0000, 0x00ff, 0xe000},
   {0x0000, 0x0000, 0x00ff, 0xc000},
   {0x0000, 0x0000, 0x00ff, 0x8000},
   {0x0000, 0x0000, 0x00ff, 0x0000},
   {0x0000, 0x0000, 0x00fe, 0x0000},
   {0x0000, 0x0000, 0x00fc, 0x0000},
   {0x0000, 0x0000, 0x00f8, 0x0000},
   {0x0000, 0x0000, 0x00f0, 0x0000},
   {0x0000, 0x0000, 0x00e0, 0x0000},
   {0x0000, 0x0000, 0x00c0, 0x0000},
   {0x0000, 0x0000, 0x0080, 0x0000}
};
int num_cool_24_ints = sizeof(cool_24_ints)/sizeof(AR_INT_64);

AR_INT_64 cool_8_ints[] =
{
   /* march a one bit though a positive 8-bit int */
   {0x0000, 0x0000, 0x0000, 0x0000},
   {0x0000, 0x0000, 0x0000, 0x0001},
   {0x0000, 0x0000, 0x0000, 0x0002},
   {0x0000, 0x0000, 0x0000, 0x0004},
   {0x0000, 0x0000, 0x0000, 0x0008},
   {0x0000, 0x0000, 0x0000, 0x0010},
   {0x0000, 0x0000, 0x0000, 0x0020},
   {0x0000, 0x0000, 0x0000, 0x0040},
   {0x0000, 0x0000, 0x0000, 0x0080},

   /* march a one bit though a negative 8-bit int */
/* {0x0000, 0x0000, 0x0000, 0x0080}, */  /* already have this one */
   {0x0000, 0x0000, 0x0000, 0x0081},
   {0x0000, 0x0000, 0x0000, 0x0082},
   {0x0000, 0x0000, 0x0000, 0x0084},
   {0x0000, 0x0000, 0x0000, 0x0088},
   {0x0000, 0x0000, 0x0000, 0x0090},
   {0x0000, 0x0000, 0x0000, 0x00a0},
   {0x0000, 0x0000, 0x0000, 0x00c0},
/* {0x0000, 0x0000, 0x0000, 0x0080}, */  /* already have this one */

   /* shift 8 one bits through a 8-bit positive int */
/* {0x0000, 0x0000, 0x0000, 0x0001}, */  /* already have this one */
   {0x0000, 0x0000, 0x0000, 0x0003},
   {0x0000, 0x0000, 0x0000, 0x0007},
   {0x0000, 0x0000, 0x0000, 0x000f},
   {0x0000, 0x0000, 0x0000, 0x001f},
   {0x0000, 0x0000, 0x0000, 0x003f},
   {0x0000, 0x0000, 0x0000, 0x007f},
   {0x0000, 0x0000, 0x0000, 0x007e},
   {0x0000, 0x0000, 0x0000, 0x007c},
   {0x0000, 0x0000, 0x0000, 0x0078},
   {0x0000, 0x0000, 0x0000, 0x0070},
   {0x0000, 0x0000, 0x0000, 0x0060},
   {0x0000, 0x0000, 0x0000, 0x0040},

   /* shift 8 one bits through a 8-bit negative int */
   {0x0000, 0x0000, 0x0000, 0x0081},
   {0x0000, 0x0000, 0x0000, 0x0083},
   {0x0000, 0x0000, 0x0000, 0x0087},
   {0x0000, 0x0000, 0x0000, 0x008f},
   {0x0000, 0x0000, 0x0000, 0x009f},
   {0x0000, 0x0000, 0x0000, 0x00bf},
   {0x0000, 0x0000, 0x0000, 0x00ff},
   {0x0000, 0x0000, 0x0000, 0x00fe},
   {0x0000, 0x0000, 0x0000, 0x00fc},
   {0x0000, 0x0000, 0x0000, 0x00f8},
   {0x0000, 0x0000, 0x0000, 0x00f0},
   {0x0000, 0x0000, 0x0000, 0x00e0},
   {0x0000, 0x0000, 0x0000, 0x00c0},
   {0x0000, 0x0000, 0x0000, 0x0080}
};
int num_cool_8_ints = sizeof(cool_8_ints)/sizeof(AR_INT_64);


AR_TYPE ar_types[] =
{
   AR_Int_8_S,
   AR_Int_8_U,
   AR_Int_24_S,
   AR_Int_24_U,
   AR_Int_32_S,
   AR_Int_32_U,
   AR_Int_46_S,
   AR_Int_64_S,
   AR_Int_64_U
};
int num_ar_types = sizeof(ar_types) / sizeof(AR_TYPE);


SINT64 act_result[sizeof(cool_64_ints)/sizeof(AR_INT_64)];
int act_status[sizeof(cool_64_ints)/sizeof(AR_INT_64)];

AR_INT_64 test_result[sizeof(cool_64_ints)/sizeof(AR_INT_64)];
int test_status[sizeof(cool_64_ints)/sizeof(AR_INT_64)];

AR_TYPE uint64_artype = AR_Int_64_U;
AR_TYPE sint64_artype = AR_Int_64_S;
AR_TYPE sint46_artype = AR_Int_46_S;
AR_TYPE uint32_artype = AR_Int_32_U;
AR_TYPE sint32_artype = AR_Int_32_S;
AR_TYPE uint24_artype = AR_Int_24_U;
AR_TYPE sint24_artype = AR_Int_24_S;
AR_TYPE uint8_artype = AR_Int_8_U;
AR_TYPE sint8_artype = AR_Int_8_S;


main()
{
   test_64_U();
   test_64_S();
   test_46_S();
   test_32_U();
   test_32_S();
   test_24_U();
   test_24_S();
   test_8_U();
   test_8_S();
}


test_64_U()
{
   int i;


   printf(" Testing convert:  UNSIGNED 64 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT64)(*(UINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(SINT64)(*(UINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (cool_64_ints[i].part1 & 0x8000) {
         act_status[i] |= AR_STAT_NEGATIVE | AR_STAT_SEMIVALID;
         if((cool_64_ints[i].part1 & 0x7fff) || cool_64_ints[i].part2 ||
            cool_64_ints[i].part3 || cool_64_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(SINT46)(*(UINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (cool_64_ints[i].part1 & 0x8000) {
         act_status[i] |= AR_STAT_NEGATIVE | AR_STAT_SEMIVALID;
         if((cool_64_ints[i].part1 & 0x7fff) || cool_64_ints[i].part2 ||
            cool_64_ints[i].part3 || cool_64_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }
      else if (INT_OVERFLOWS_46_BITS(act_result[i]))
      {  /* int too big for 46-bit int */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT32)(*(UINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (*(UINT64 *)&cool_64_ints[i] !=
               (UINT64)(UINT32)*(UINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(UINT64 *)&cool_64_ints[i]) & 0xffffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT32)(*(UINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT32)*(UINT64 *)&cool_64_ints[i] < 0) ^
          (        *(UINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(UINT64 *)&cool_64_ints[i] !=
               (UINT64)(SINT32)*(UINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(UINT64 *)&cool_64_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (*(UINT64 *)&cool_64_ints[i] != (UINT64)act_result[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(UINT64 *)&cool_64_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((*(UINT64 *)&cool_64_ints[i]) & 0x800000) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((*(UINT64 *)&cool_64_ints[i] & 0x800000) != 0) ^
           (*(UINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(UINT64 *)&cool_64_ints[i] !=
               ((act_result[i] & 0x800000) ?
                                        act_result[i] | 0xffffffffff000000 :
                                        act_result[i]))
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT8)(*(UINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (*(UINT64 *)&cool_64_ints[i] !=
               (UINT64)(UINT8)*(UINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 64 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(UINT64 *)&cool_64_ints[i]) & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT8)(*(UINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT8)*(UINT64 *)&cool_64_ints[i] < 0) ^
          (       *(UINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(UINT64 *)&cool_64_ints[i] !=
               (UINT64)(SINT8)*(UINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_64_ints[i], &uint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_64_U */


test_64_S()
{
   int i;


   printf(" Testing convert:    SIGNED 64 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT64)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT64)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (        *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_SEMIVALID;
         if((cool_64_ints[i].part1 & 0x7fff) || cool_64_ints[i].part2 ||
            cool_64_ints[i].part3 || cool_64_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(UINT64)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(SINT64)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT64)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(SINT46)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT46)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (INT_OVERFLOWS_46_BITS(act_result[i]))
      {  /* int too big for 46-bit int */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT32)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT32)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (        *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(UINT32)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) & 0xffffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT32)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT32)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (        *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(SINT32)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (0 ^ (*(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
          /* UNSIGNED 24 bit is always positive */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] != (SINT64)act_result[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((*(SINT64 *)&cool_64_ints[i]) & 0x800000) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((*(SINT64 *)&cool_64_ints[i] & 0x800000) != 0) ^
           (*(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               ((act_result[i] & 0x800000) ?
                                        act_result[i] | 0xffffffffff000000 :
                                        act_result[i]))
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT8)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT8)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (       *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(UINT8)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 64 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT8)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT8)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (       *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(SINT8)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint64_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_64_S */


test_46_S()
{
   int i;


   printf(" Testing convert:    SIGNED 46 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT64)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT64)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (        *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_SEMIVALID;
         if((cool_64_ints[i].part1 & 0x7fff) || cool_64_ints[i].part2 ||
            cool_64_ints[i].part3 || cool_64_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(UINT64)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(SINT64)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT64)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(SINT46)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT46)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (INT_OVERFLOWS_46_BITS(act_result[i]))
      {  /* int too big for 46-bit int */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT32)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT32)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (        *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(UINT32)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) &0xffffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT32)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT32)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (        *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(SINT32)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (0 ^ (*(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
          /* UNSIGNED 24 bit is always positive */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] != (SINT64)act_result[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((*(SINT64 *)&cool_64_ints[i]) & 0x800000) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((*(SINT64 *)&cool_64_ints[i] & 0x800000) != 0) ^
           (*(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               ((act_result[i] & 0x800000) ?
                                        act_result[i] | 0xffffffffff000000 :
                                        act_result[i]))
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (UINT64)(UINT8)(*(SINT64 *)&cool_64_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT8)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (       *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(UINT8)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 46 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_64_ints; i++)
   {
      act_result[i] = (*(SINT64 *)&cool_64_ints[i]) & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT8)(*(SINT64 *)&cool_64_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT8)*(SINT64 *)&cool_64_ints[i] < 0) ^
          (       *(SINT64 *)&cool_64_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT64 *)&cool_64_ints[i] !=
               (SINT64)(SINT8)*(SINT64 *)&cool_64_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_64_ints[i], &sint46_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_46_S */


test_32_U()
{
   int i;


   printf(" Testing convert:  UNSIGNED 32 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(UINT64)(*(UINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(SINT64)(*(UINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(SINT46)(*(UINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(UINT32)(*(UINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(UINT32 *)&cool_32_ints[i]) & 0xffffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_32_ints[i].part3 & 0x8000) {
         act_status[i] |= AR_STAT_NEGATIVE | AR_STAT_SEMIVALID;
         if((cool_32_ints[i].part3 & 0x7fff) || cool_32_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(UINT32 *)&cool_32_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (0 ^ (*(UINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
          /* UNSIGNED 24 bit is always positive */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(UINT32 *)&cool_32_ints[i] != (UINT64)act_result[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(UINT32 *)&cool_32_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((*(UINT32 *)&cool_32_ints[i]) & 0x800000) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((*(UINT32 *)&cool_32_ints[i] & 0x800000) != 0) ^
           (*(UINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(UINT32 *)&cool_32_ints[i] !=
               ((act_result[i] & 0x800000) ?
                                        act_result[i] | 0xffffffffff000000 :
                                        act_result[i]))
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(UINT8)(*(UINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT8)*(UINT32 *)&cool_32_ints[i] < 0) ^
          (       *(UINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(UINT32 *)&cool_32_ints[i] !=
               (UINT32)(UINT8)*(UINT32 *)&cool_32_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 32 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(UINT32 *)&cool_32_ints[i]) & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT8)(*(UINT32 *)&cool_32_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT8)*(UINT32 *)&cool_32_ints[i] < 0) ^
          (       *(UINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(UINT32 *)&cool_32_ints[i] !=
               (UINT32)(SINT8)*(UINT32 *)&cool_32_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_32_ints[i], &uint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_32_U */


test_32_S()
{
   int i;


   printf(" Testing convert:    SIGNED 32 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(UINT64)(*(SINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (*(SINT32 *)&cool_32_ints[i] < 0)
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(SINT64)(*(SINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT64)(*(SINT32 *)&cool_32_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(SINT46)(*(SINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT46)(*(SINT32 *)&cool_32_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(UINT32)(*(SINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_32_ints[i].part3 & 0x8000)
      {
         act_status[i] |= AR_STAT_SEMIVALID;
         if((cool_32_ints[i].part3 & 0x7fff) || cool_32_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(SINT32 *)&cool_32_ints[i]) &0xffffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT32)(*(SINT32 *)&cool_32_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(SINT32 *)&cool_32_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (0 ^ (*(SINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
          /* UNSIGNED 24 bit is always positive */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT32 *)&cool_32_ints[i] != (SINT64)act_result[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(SINT32 *)&cool_32_ints[i]) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((*(SINT32 *)&cool_32_ints[i]) & 0x800000) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((*(SINT32 *)&cool_32_ints[i] & 0x800000) != 0) ^
           (*(SINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT32 *)&cool_32_ints[i] !=
               ((act_result[i] & 0x800000) ?
                                        act_result[i] | 0xffffffffff000000 :
                                        act_result[i]))
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (UINT64)(UINT8)(*(SINT32 *)&cool_32_ints[i]);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (((UINT8)*(SINT32 *)&cool_32_ints[i] < 0) ^
          (       *(SINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT32 *)&cool_32_ints[i] !=
               (SINT32)(UINT8)*(SINT32 *)&cool_32_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 32 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_32_ints; i++)
   {
      act_result[i] = (*(SINT32 *)&cool_32_ints[i]) & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT8)(*(SINT32 *)&cool_32_ints[i]) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((SINT8)*(SINT32 *)&cool_32_ints[i] < 0) ^
          (       *(SINT32 *)&cool_32_ints[i] < 0))
      {   /* sign before and after cast does not match */
         act_status[i] |= AR_STAT_OVERFLOW;
      }
      else if (*(SINT32 *)&cool_32_ints[i] !=
               (SINT32)(SINT8)*(SINT32 *)&cool_32_ints[i])
      {   /* result cast back to original type does not match original value */
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_32_ints[i], &sint32_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_32_S */


test_24_U()
{
   int i;


   printf(" Testing convert:  UNSIGNED 24 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (act_result[i] & 0x800000)
      {
         act_status[i] |= AR_STAT_NEGATIVE | AR_STAT_SEMIVALID;
         if((cool_24_ints[i].part3 & 0x7f) || cool_24_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_24_ints[i].part3 || (cool_24_ints[i].part4 & 0xff00))
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED 24 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (act_result[i] & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE | AR_STAT_OVERFLOW;
      }
      else if (cool_24_ints[i].part3 || (cool_24_ints[i].part4 & 0xff00))
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_24_ints[i], &uint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_24_U */


test_24_S()
{
   int i;


   printf(" Testing convert:    SIGNED 24 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] | (cool_24_ints[i].part3 & 0x80 ? 0xffffffffff000000 : 0);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (act_result[i] & 0x800000)
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] | (cool_24_ints[i].part3 & 0x80 ? 0xffffffffff000000 : 0);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (*(SINT64 *)&act_result[i] < 0)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] | (cool_24_ints[i].part3 & 0x80 ? 0xffffffffff000000 : 0);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (*(SINT46 *)&act_result[i] < 0)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] | (cool_24_ints[i].part3 & 0x80 ? 0xff000000 : 0);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (act_result[i] & 0x800000)
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] | (cool_24_ints[i].part3 & 0x80 ? 0xff000000 : 0);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (*(SINT32 *)&act_result[i] < 0)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (act_result[i] & 0x800000)
      {
         act_status[i] |= AR_STAT_SEMIVALID;
         if((cool_24_ints[i].part3 & 0x7f) || cool_24_ints[i].part4)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i];
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if (act_result[i] & 0x800000)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_24_ints[i].part3 || (cool_24_ints[i].part4 & 0xff00))
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED 24 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_24_ints; i++)
   {
      act_result[i] = *(UINT64 *)&cool_24_ints[i] & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (act_result[i] & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (((cool_24_ints[i].part3 & 0x00ff) != 0x00ff ||
           (cool_24_ints[i].part4 & 0xff80) != 0xff80) &&
          ((cool_24_ints[i].part3 & 0x00ff) != 0 ||
           (cool_24_ints[i].part4 & 0xff80) != 0))
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_24_ints[i], &sint24_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_24_S */


test_8_U()
{
   int i;


   printf(" Testing convert:  UNSIGNED  8 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(UINT64)*((UINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(SINT64)*((UINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(SINT46)*((UINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(UINT32)*((UINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = *((UINT8 *)&cool_8_ints[i]+7) & 0xffffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;
      if ((SINT32)*((UINT8 *)&cool_8_ints[i]+7) < 0) {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = *((UINT8 *)&cool_8_ints[i]+7) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = *((UINT8 *)&cool_8_ints[i]+7) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(UINT8)*((UINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:  UNSIGNED  8 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT8)(SINT8)*((UINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE | AR_STAT_SEMIVALID;
         if(cool_8_ints[i].part4 & 0x7f)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_8_ints[i], &uint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_8_U */


test_8_S()
{
   int i;


   printf(" Testing convert:    SIGNED  8 INT -> UNSIGNED 64 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(UINT64)*((SINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint64_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT ->   SIGNED 64 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(SINT64)*((SINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint64_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT ->   SIGNED 46 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(SINT46)*((SINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint46_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT -> UNSIGNED 32 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(UINT32)*((SINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint32_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT ->   SIGNED 32 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = *((SINT8 *)&cool_8_ints[i]+7) & 0xffffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint32_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT -> UNSIGNED 24 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = *((SINT8 *)&cool_8_ints[i]+7) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint24_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT ->   SIGNED 24 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = *((SINT8 *)&cool_8_ints[i]+7) & 0xffffff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint24_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT -> UNSIGNED  8 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = (UINT64)(UINT8)*((SINT8 *)&cool_8_ints[i]+7);
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_SEMIVALID;
         if(cool_8_ints[i].part4 & 0x7f)
           act_status[i] |=  AR_STAT_OVERFLOW;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &uint8_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");


   printf(" Testing convert:    SIGNED  8 INT ->   SIGNED  8 INT :  ");

   for (i=0; i<num_cool_8_ints; i++)
   {
      act_result[i] = *((SINT8 *)&cool_8_ints[i]+7) & 0xff;
      act_status[i] = (act_result[i] == 0) ? AR_STAT_ZERO : AR_STAT_OK;

      if (cool_8_ints[i].part4 & 0x80)
      {
         act_status[i] |= AR_STAT_NEGATIVE;
      }

      test_status[i] = AR_convert((AR_DATA *)&test_result[i], &sint8_artype,
                                  (AR_DATA *)&cool_8_ints[i], &sint8_artype);

      if (*(SINT64 *)&test_result[i] != act_result[i])
      {
         printf(" ERROR:  incorrect result at %d\n", i);
         abort();
      }

      if (act_status[i] != test_status[i])
      {
         printf(" ERROR:  incorrect status at %d\n", i);
         abort();
      }
   }
   printf("PASSED\n");
}  /* test_8_S */

#endif   /* _CRAY */

static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: testcvt.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
