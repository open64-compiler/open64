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

/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/qsort_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 *
 * quick sort interface
 *
 * calling sequence:
 *	f77external compar
 *	call qsort (array, len, isize, compar)
 *	----
 *	integer*2 function compar (obj1, obj2)
 * where:
 *	array contains the elements to be sorted
 *	len is the number of elements in the array
 *	isize is the size of an element, typically -
 *		4 for integer, float
 *		8 for double precision
 *		(length of character object) for character arrays
 *	compar is the name of an integer*2 function that will return -
 *		<0 if object 1 is logically less than object 2
 *		=0 if object 1 is logically equal to object 2
 *		>0 if object 1 is logically greater than object 2
 *
 * Notes: len & isize must be representable in an integer *4 even though
 * 	the C library
 * 	function qsort will accept 64-bit values for len & isize in
 *	64 bit mode.
 */
#include <stdlib.h>

extern void
qsort_ (void *array, int *len, int *isize, 
	int (*compar)(const void *, const void *))
{
	qsort(array, (size_t)*len, (size_t)*isize, compar);
}


