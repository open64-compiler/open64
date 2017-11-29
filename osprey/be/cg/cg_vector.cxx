/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* =======================================================================
 * =======================================================================
 *
 *  Module: vector.c
 *  $Revision: 1.6 $
 *  $Date: 05/12/05 08:59:05-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_vector.cxx $
 *
 *  Description:
 *  ============
 *
 *  Package for creating and manipulating a vector of 'void *' elements.
 *
 * =======================================================================
 * =======================================================================
 */

#include <stdlib.h>
#include "defs.h"
#include "mempool.h"
#include "errors.h"
#include "cg_vector.h"


VECTOR
VECTOR_Init (INT size, MEM_POOL *pool)
{
  VECTOR v = TYPE_MEM_POOL_ALLOC (VECTOR_STRUCT, pool);
  v->vector = TYPE_MEM_POOL_ALLOC_N (void *, pool, size);
  VECTOR_size(v) = size;
  VECTOR_count(v) = 0;
  return v;
}


void
VECTOR_Add_Element (VECTOR vector, void *element)
{
  INT count = VECTOR_count(vector);
  FmtAssert (count < VECTOR_size(vector), ("VECTOR overflow"));
  VECTOR_element(vector, count) = element;
  count++;
  VECTOR_count(vector) = count;
}


BOOL
VECTOR_Member_Element (VECTOR vector, void *element)
{
  INT i, count = VECTOR_count(vector);
#ifdef KEY
  FmtAssert (count <= VECTOR_size(vector), ("VECTOR overflow"));
#else
  FmtAssert (count < VECTOR_size(vector), ("VECTOR overflow"));
#endif // KEY
  
  for (i = 0; i < count; i++) {
    if (VECTOR_element(vector,i) == element) {
      return TRUE;
    }
  }
  
  return FALSE;
}

void
VECTOR_Sorted_Add_Element (
  VECTOR vector, 
  void *element, 
  VECTOR_ELEMENT_COMPARE compare_func)
{
  INT i;
  INT count = VECTOR_count(vector);
  FmtAssert (count < VECTOR_size(vector), ("VECTOR overflow"));
  for (i = count; i > 0; i--) {
    void *cur_element = VECTOR_element(vector,i-1);
    if (compare_func (cur_element, element) > 0) break;
    VECTOR_element(vector,i) = cur_element;
  }
  VECTOR_element(vector, i) = element;
  count++;
  VECTOR_count(vector) = count;
}


void
VECTOR_Sort (VECTOR vector, VECTOR_ELEMENT_COMPARE compare_func)
{
  qsort (
    &VECTOR_element(vector,0), 
    VECTOR_count(vector), 
    sizeof (void *),
    compare_func);
}

void 
VECTOR_Reset (VECTOR vector)
{
  VECTOR_count(vector) = 0;
}

void
VECTOR_Delete_Element (VECTOR vector, void *element)
{
  INT i, j;
  INT count = VECTOR_count(vector);

  for (i = 0; i < count; i++) {
    if (VECTOR_element(vector,i) == element) {
      for (j = i+1; j < count; j++) {
	VECTOR_element(vector, j-1) = VECTOR_element(vector, j);
      }
      VECTOR_count(vector) = count-1;
      return;
    }
  }
}
