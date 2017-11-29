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
 *  Module: vector.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:22-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_vector.h $
 *
 *  Description:
 *  ============
 *
 *  Package for creating and manipulating a vector of 'void *' elements.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef vector_INCLUDED
#define vector_INCLUDED

#include "defs.h"
#include "mempool.h"

typedef struct {
  void **vector;
  INT16 count;
  INT16 size;
} VECTOR_STRUCT, *VECTOR;

#define VECTOR_element(v,i)	((v)->vector[i])
#define VECTOR_count(v)		((v)->count)
#define VECTOR_size(v)		((v)->size)

typedef INT (*VECTOR_ELEMENT_COMPARE)(const void*, const void*);

extern VECTOR VECTOR_Init (INT size, MEM_POOL *pool);

extern void VECTOR_Add_Element (VECTOR vector, void *element);

extern BOOL VECTOR_Member_Element (VECTOR vector, void *element);

extern void VECTOR_Sorted_Add_Element (
  VECTOR vector, 
  void *element, 
  VECTOR_ELEMENT_COMPARE compare_func);

extern void VECTOR_Sort (VECTOR vector, VECTOR_ELEMENT_COMPARE compare_func);

extern void VECTOR_Delete_Element (VECTOR vector, void *element);

extern void VECTOR_Reset (VECTOR vector);

#endif /* vector_INCLUDED */

