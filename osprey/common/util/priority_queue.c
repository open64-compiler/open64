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


/* ====================================================================
 * ====================================================================
 *
 * Module: priority_queue.c
 *
 * Description:
 *
 *      Priority queue implementation
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "priority_queue.h"

/**=======================================================================
***  Private field access macros
***=======================================================================
**/
#define PRQ_comparison_fn(x)    ((x)->comparison_fn)
#define PRQ_mem_pool(x)         ((x)->mem_pool)
#define PRQ_size(x)             ((x)->size)
#define PRQ_allocated_size(x)   ((x)->allocated_size)
#define PRQ_expansion_factor(x) ((x)->expansion_factor)
#define PRQ_get_index_fn(x)     ((x)->get_index_fn)
#define PRQ_set_index_fn(x)     ((x)->set_index_fn)
#define PRQ_heap_vector(x)      ((x)->heap_vector)


/**=======================================================================
***  Utilities for manipulating the heap
***=======================================================================
**/

/* =======================================================================
 *
 *  PRQ_Ith
 *
 *  See interface description.
 *
 * =======================================================================
 */
void*
PRQ_Ith(
  PRQ*    prq,
  INT32   i
)
{
  return PRQ_heap_vector(prq)[i-1];
}

/* =======================================================================
 *
 *  PRQ_Set_Ith
 *
 *  See interface description.
 *
 * =======================================================================
 */
extern INT32
PRQ_Size(
    PRQ*    prq
)
{
  return PRQ_size(prq);
}

/* =======================================================================
 *
 *  PRQ_Set_Ith
 *
 *  Make 'element' the 'i'-th element of 'prq'.
 *
 * =======================================================================
 */
static void
PRQ_Set_Ith(
  PRQ*   prq,
  INT32  i,
  void*  element
)
{
  if ( PRQ_set_index_fn(prq) != NULL ) {
    PRQ_set_index_fn(prq)(element,i);
  }

  PRQ_heap_vector(prq)[i-1] = element;
}


/**
***  heap_Upheap
***
***     Sifts the heap_element at position 'index' in the heap (1-based)
***     up to its appropriate position in the heap.  If the heap element
***     should not move closer to the root ("up"), it is not moved.  The
***     the new index of the heap_element is returned.
**/

/* =======================================================================
 *
 *  PRQ_Upheap
 *
 *  Sifts the heap_element at position 'index' in the heap (1-based) up to
 *  its appropriate position in the heap.  If the heap element should not
 *  move closer to the root ("up"), it is not moved.  The the new index of
 *  the heap_element is returned.
 *
 * =======================================================================
 */
static INT32
PRQ_Upheap(
  PRQ*  prq,
  INT32 index
)
{
  void* element;

  FmtAssert(index <= PRQ_size(prq) && index > 0,
            ("PRQ_Upheap:  index %d out of bounds %d",
             index,PRQ_size(prq)));

  element = PRQ_Ith(prq,index);

  /* Note we do not perform a complete interchange in the body of the
   * loop.  We wait until we have found the correct position for
   * 'element' before setting it.
   */
  while ( index > 1 ) {
    INT32 parent_index = index / 2;
    void* parent       = PRQ_Ith(prq,parent_index);

    if ( ! PRQ_comparison_fn(prq)(element,parent) )
      break;

    PRQ_Set_Ith(prq,index,parent);
    index = parent_index;
  }

  PRQ_Set_Ith(prq,index,element);

  return index;
}


/* =======================================================================
 *
 *  PRQ_Downheap
 *
 *  Sifts the heap element at position 'index' in the heap (1-based) down
 *  to its appropriate position in the heap.  If the heap element should
 *  not move further from the root ("down"), it is not moved.  The the new
 *  index of the heap_element is returned.
 *
 * =======================================================================
 */
static INT32
PRQ_Downheap(
  PRQ*  prq,
  INT32 index
)
{
  void* element;

  FmtAssert(index <= PRQ_size(prq) && index > 0,
            ("PRQ_down:  index %d out of bounds %d",
             index,PRQ_size(prq)));

  element = PRQ_Ith(prq,index);

  /* Note we do not perform a complete interchange in the body of the
   * loop.  We wait until we have found the correct position for
   * 'element' before setting it.
   */

  for(;;) {
    void* left_child;
    INT32 left_child_index  = index * 2;
    INT32 right_child_index = left_child_index + 1;

    if ( left_child_index > PRQ_size(prq) ) break;

    left_child = PRQ_Ith(prq,left_child_index);

    /* If the right child is in the heap, and is "larger" than the left,
     * try to interchange 'element' with the right child.  If the
     * interchange test fails, 'index' is the correct position for
     * element.
     */
    if (    right_child_index <= PRQ_size(prq)
         && PRQ_comparison_fn(prq)(PRQ_Ith(prq,right_child_index),
                                   left_child)
    ) {
      void* right_child = PRQ_Ith(prq,right_child_index);

      if ( PRQ_comparison_fn(prq)(right_child,element) ) {
        PRQ_Set_Ith(prq,index,right_child);
        index = right_child_index;
      }
      else
        break;
    }
    else if ( PRQ_comparison_fn(prq)(left_child,element) ) {
      /* interchange with left child */
      PRQ_Set_Ith(prq,index,left_child);
      index = left_child_index;
    }
    else
      break;
  }

  PRQ_Set_Ith(prq,index,element);

  return index;
}


/**======================================================================
***  Interface routines
***======================================================================
**/


/* =======================================================================
 *
 *  PRQ_Initialize
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
PRQ_Initialize(
    PRQ*                        prq,
    PRQ_COMPARISON_FUNCTION     comparison_fn,
    PRQ_GET_INDEX_FUNCTION      get_fn,
    PRQ_SET_INDEX_FUNCTION      set_fn,
    MEM_POOL*                   pool,
    INT32                       initial_size,
    INT32                       expansion_factor
)
{
  if ( initial_size <= 0 ) {
    DevWarn("Non positive priority queue initial size %d.  Using 200",
            initial_size);
    initial_size = 200;
  }

  if ( expansion_factor <= 100 ) {
    DevWarn("Priority queue expansion factor should be at least 100.  "
            "Was %d using 200",expansion_factor);
    expansion_factor = 200;
  }

  PRQ_comparison_fn(prq) = comparison_fn;
  PRQ_get_index_fn(prq) = get_fn;
  PRQ_set_index_fn(prq) = set_fn;
  PRQ_mem_pool(prq) = pool;
  PRQ_size(prq) = 0;
  PRQ_allocated_size(prq) = initial_size;
  PRQ_expansion_factor(prq) = expansion_factor;
  PRQ_heap_vector(prq) = TYPE_MEM_POOL_ALLOC_N(void*,pool,initial_size);
}


/* =======================================================================
 *
 *  PRQ_DeleteTop
 *
 *  See interface description.
 *
 * =======================================================================
 */
void*
PRQ_Delete_Top( PRQ* prq )
{
  void* top_element;

  FmtAssert(PRQ_size(prq) > 0,("Deleting from empty heap"));

  top_element = PRQ_Ith(prq,1);
  if ( PRQ_size(prq) == 1 )
    PRQ_size(prq) = 0;
  else {
    /* Move the bottom element to the top nad sift it down to its proper
     * position in the heap.
     */
    void* bottom_element = PRQ_Ith(prq,PRQ_size(prq));

    --PRQ_size(prq);
    PRQ_Set_Ith(prq,1,bottom_element);
    (void) PRQ_Downheap(prq,1);
  }

  return top_element;
}



/* =======================================================================
 *
 *  PRQ_Top
 *
 *  See interface description.
 *
 * =======================================================================
 */
void*
PRQ_Top( PRQ* prq )
{
  FmtAssert(PRQ_size(prq) > 0,("Topping empty queue"));

  return PRQ_Ith(prq,1);
}


/* =======================================================================
 *
 *  PRQ_InsertElement
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
PRQ_Insert(
  PRQ*  prq,
  void* element
)
{
  /* Grow 'heap_vector' if necessary.
   */
  if ( PRQ_size(prq) == PRQ_allocated_size(prq) ) {
    INT32 new_size = (PRQ_size(prq) * PRQ_expansion_factor(prq)) / 100;

    if ( new_size <= PRQ_size(prq) ) {
      DevWarn("Priority queue expansion failed -- forcing expansion by 10");
      new_size = PRQ_size(prq) + 10;
    }

    PRQ_heap_vector(prq) =
      TYPE_MEM_POOL_REALLOC_N(void*,PRQ_mem_pool(prq),PRQ_heap_vector(prq),
                                                      PRQ_allocated_size(prq),
                                                      new_size);
    PRQ_allocated_size(prq) = new_size;
  }

  /* Add element to bottom of heap, and sift it up to its appropriate
   * position.
   */
  ++PRQ_size(prq);
  PRQ_Set_Ith(prq,PRQ_size(prq),element);
  (void) PRQ_Upheap(prq,PRQ_size(prq));
}


/* =======================================================================
 *
 *  PRQ_RemoveElement
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
PRQ_Remove(
  PRQ*  prq,
  void* element
)
{
  INT32 index = UNDEFINED;

  FmtAssert(PRQ_size(prq) > 0,("PRQ_RemoveElement -- empty queue"));

  /* Find the index of 'element' in the heap.If we have an index function,
   * we can get the index of the element directly.  Otherwise we search
   * for it.
   */

  if ( PRQ_get_index_fn(prq) ) {
    index =  PRQ_get_index_fn(prq)(element);
    FmtAssert(element == PRQ_Ith(prq,index),
              ("Invalid priority queue index %d",index));
  }
  else {
    INT32 i;

    for ( i = 1; i <= PRQ_size(prq); ++i ) {
      if ( PRQ_Ith(prq,i) == element ) {
        index = i;
        break;
      }
    }
  }

  FmtAssert(index != UNDEFINED,("Remove a PRQ element not in queue"));

  /* If element is the bottom elment in the heap, simply decrement the
   * size, and we are done.
   */
  if ( index == PRQ_size(prq) )
    --PRQ_size(prq);
  else {
    /* Replace 'element' at position 'index' with the bottom element of
     * the heap.  Then try to sift the newly placed 'bottom_element' up
     * and then down,to make sure it is in its correct position.
     */
    void* bottom_element = PRQ_Ith(prq,PRQ_size(prq));

    /* Reposition bottom_element.
     */
    --PRQ_size(prq);
    PRQ_Set_Ith(prq,index,bottom_element);
    
    /* Try to move it up.  If it moves we know it has found its
     * appropriate position.  Otherwise, try to move it down.
     */
    if ( index == PRQ_Upheap(prq,index) )
      (void) PRQ_Downheap(prq,index);
  }
}


/* =======================================================================
 *
 *  PRQ_Reset
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
PRQ_Reset( PRQ* prq )
{
  PRQ_size(prq) = 0;
}
