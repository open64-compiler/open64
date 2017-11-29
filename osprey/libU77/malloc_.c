/*

  Copyright (C) 1999-2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  Further, any
  license provided herein, whether implied or otherwise, is limited to 
  this program in accordance with the express provisions of the 
  GNU Lesser General Public License.  

  Patent licenses, if any, provided herein do not apply to combinations 
  of this program with other product or programs, or any other product 
  whatsoever.  This program is distributed without any warranty that the 
  program is delivered free of the rightful claim of any third person by 
  way of infringement or the like.  

  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/

/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/malloc_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */

/*
#define NaN_single 0x7fbfffff;
 */
#define NaN_single 0xfffa5a5a;
#define NaN_single_byte_0 0x5a;
#define NaN_single_byte_1 0x5a;
#define NaN_single_byte_2 0xfa;

extern char *mips__nan_malloc_(int *sz)
{
  char *p;
  int *q;
  size_t roundoff_size;

  roundoff_size = sizeof(int) * ((*sz + sizeof(int) - 1)/sizeof(int));
  p = (char *) malloc(roundoff_size);
  q = (int *) (p + roundoff_size);
  while (q > (int *) p) {
    *--q = NaN_single;
  }
  return (p + (roundoff_size-*sz));
}

void *__trapuv_malloc(size_t size)
{
  void *p;
  int *q;
  size_t roundoff_size;

  roundoff_size = sizeof(int) * ((size + sizeof(int) - 1)/sizeof(int));
  p = malloc(roundoff_size);
  q = (int *) ((char *)p + roundoff_size);
  while (q > (int *) p) {
    *--q = NaN_single;
  }
  return (void *)((char *)p + (roundoff_size-size));
}
