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


/**
***                          Priority Queues
***
*** This package implements heap priority queues of pointers.  Given a
*** base type, a typesafe priority queue interface can be generated.
***
*** Generating the interface
*** ========================
***
*** macro PRQ_TYPE(
***     base_type,
***     prq_type
*** )
***
***     Generate a priority queue interface for poiners to objects of the
***     given "base_type".  The resulting priority type is named
***     "prq_type" or struct "prq_type".  The underlying basic queue type
***     is inherently generated as if by:
***
***         PRQ_TYPE(void,PRQ)
***
***     In what follows, substitute "base_type" for "void" and "prq_type"
***     for "PRQ" in order to read the documentation for an instantiated
***     typesafe priority queue type. 
***
*** Reserved prefixes
*** =================
***
***     PRQ
***
*** Exported types
*** ==============
***
*** typedef struct prq PRQ
***
***     This is the type of a priority queue descriptor.  Each queue is
***     defined by a descriptor, which must be allocated by the client.
***
*** Storage management.
*** ==================
***
***  The only dynamically allocated storage required by the PRQ (other
***  than the structure itself) is a vector to hold a heap.  The client of
***  the PRQ should create a memory pool for the heap vector.  The heap
***  vector will be reallocated as the heap grows; to avoid unnecessary
***  copying, the pool should be used for the heap vector alone.  The
***  storage for the heap vector is freed by resetting or deleting the
***  pool.
***
*** Creating PRQs
*** =============
***
*** To create a PRQ, the client must supply two functions that operate
*** on the client heap-element type.
***
*** typedef BOOL (*PRQ_COMPARISON_FUNCTION)(void*,void*)
***
***     This is the type of the comparison function that compares two
***     PRQ-elements and returns a BOOL result.  It should return TRUE if
***     and only if its first argument should precede its second argument
***     in the queue.  It should return FALSE if the two arguments have
***     the same precedence, or if the second argument should precede the
***     first in the heap.
***
*** typedef INT32* (*PRQ_ELEMENT_GET_INDEX_FUNCTION)(void*)
*** typedef void (*PRQ_ELEMENT_SET_INDEX_FUNCTION)(void*,INT32)
***
***     The client heap-element type may OPTIONALLY contain an INT32 field
***     to hold the element's index in the heap. This is useful for fast
***     deletion of the element from the heap.  This field should only be
***     accessed by the heap package.  These typedefs are the types of
***     client supplied function pointers that may be used to get/set
***     the value of this field.
***
*** void PRQ_Initialize(
***     PRQ*                        prq
***     PRQ_COMPARISON_FUNCTION     comparison_fn,
***     PRQ_GET_INDEX_FUNCTION      get_index_fn,
***     PRQ_SET_INDEX_FUNCTION      set_index_fn,
***     MEM_POOL*                   pool,
***     INT32                       initial_size,
***     INT32                       expansion_factor
*** )
***
***     Create a new priority queue.  The meaning of the parameters is:
***
***     prq     A pointer to a PRQ allocated by the client.
***
***     comparison_fn
***             The comparison function to be associated with the priority
***             queue
***
***     get_index_function
***     set_index_fn
***
***             A pointer to a function to get/set the index assiciated
***             with a priority queue element.  Pass NULL for these
***             paramenters if this functionality is unavailable.
***
***     pool    Where to (re)allocate the heap vector
***
***     initial_size
***             This is the number of elements that the new PRQ
***             will be able to hold before it must be expanded.
***
***     expansion_factor
***             This is the amount to expand the PRQ vector when it
***             overflows.  200 means double the size of the table, i.e.,
***             new-size = (old_size * expansion_factor)/100.
***     index_fn
***             This function returns a pointer to the heap index in
***             the PRQ-element.  If the PRQ-elements does NOT have such
***             a field, set 'index_fn' NULL.
***
*** Accessing a PRQ
*** ===============
***
*** void* PRQ_Delete_Top(
***     PRQ*    prq
*** )
***     Deletes and returns the top element in 'heap'.  It is an error if
***     'heap' has no elements.
***
***
*** void* PRQ_Top(
***     PRQ*    prq
*** )
***     Returns the top element in 'prq', WITHOUT changing 'prq'.  It is an
***     error if 'prq' is empty.
***
***
*** void PRQ_Insert(
***     PRQ*    prq
***     void*   element
*** )
***     Adds a new element 'element' to 'prq'.
***
***
*** void PRQ_Remove(
***     PRQ*    prq,
***     void*   element
*** )
***     Removes element 'element' from 'prq'.  It is an error if 'element'
***     is not in the prioiry queue..
***
***     If an 'index_fn' is provided when the heap is created, the element to
***     be deleted can be directly accessed in the heap, and the Remove
***     operation takes O(log n) time.  If no 'index_fn' is provided, the
***     heap is first searched linearly for the element, and then it is
***     removed.  This takes O(nlog n) time.
***
***
*** void PRQ_Reset(
***     PRQ*    prq
*** )
***     Removes all elements from heap, without freeing any storage.
***
***
*** INT32 PRQ_Size(
***     PRQ*    prq
*** )
***     The number of elements currently in the queue.
***
***
*** Looping over the elements of queue
*** ==================================
***
*** void* PRQ_Ith(
***     PRQ*    prq,
***     INT32   i
*** )
***
***     Return the 'i'-th element in the queue.
***
***
*** To iterate over all the elements in the queue (but not in priority
*** order), write code like this:
***
***     INT32 i;
***
***     for ( i = 0; i < PRQ_Size(prq); ++i ) {
***         void* ith = PRQ_Ith(prq,i);
***
*** Iterating over the queue in prioirty order is much more expensive.
*** You have to remove all the elements via PRA_Delete_Top and then add
*** them back.  You're probably better off copying the elements into a
*** vector and sorting it with qsort.
***
**/

/**
***  Implementation note:
***
***     A heap is a complete binary tree that satisfies the heap
***     condition.
***
***     A complete binary tree is a tree in which every leaf has depth d
***     or d-1, and all leaves of depth d are to the left of leaves of
***     depth d-1.  For example, a complete binary tree with 6 nodes.
***
***                       _ 1 _
***                     /       \
***                    _2_     _3
***                   /   \   /
***                  4     5 6
***
***     A complete binary tree can be efficiently represented as an array,
***     where the father of node k is node floor(k/2), and the children of
***     node k are nodes 2k and 2k+1 (if both children exist).  For
***     example, the above tree becomes:
***
***             1 2 3 4 5 6
***
***     The 'heap_vector' field of a PRQ is used to represent a binary
***     tree.  The 'current_size' field gives the number of nodes in the
***     tree.
***
***     The heap condition is that the key of a given node should be
***     greater than or equal to the keys of its children.  The
***     'comparison_function' field of the heap is used to compare
***     the keys.
**/


/** $Revision: 1.1 $
*** $Date: 2005/07/27 02:17:57 $
*** $Author: kevinlo $
*** $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/priority_queue.h,v $
**/

#ifndef prq_INCLUDED
#define prq_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

#ifdef _KEEP_RCS_ID
#ifndef PRQ_RCS_ID
#define PRQ_RCS_ID
static const char prq_rcs_id[] = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/priority_queue.h,v $ $Revision: 1.1 $";
#endif
#endif

#include "defs.h"

struct mem_pool;

typedef BOOL  (*PRQ_COMPARISON_FUNCTION) (void*,void*);
typedef INT32 (*PRQ_GET_INDEX_FUNCTION)(void*);
typedef void (*PRQ_SET_INDEX_FUNCTION)(void*,INT);
typedef void (*PRQ_ELEMENT_PRINT_FUNCTION)(void*);

typedef struct prq {
    /**
    *** No user accessable parts inside.
    **/

    PRQ_COMPARISON_FUNCTION comparison_fn;
                    /*  Function used to compare keys.
                     */
    struct mem_pool* mem_pool;
                    /* Where to [re]allocate
                     */
    INT32 size;
                    /*  Number of elements in the heap.
                     */
    INT32 allocated_size;
                    /*  Number of elements 'heap_vector' can hold.
                     */

    INT32 expansion_factor;
                    /*  Amount to grow heap.
                     */

    PRQ_GET_INDEX_FUNCTION get_index_fn;
    PRQ_SET_INDEX_FUNCTION set_index_fn;
                    /*  Functions to get/set the index of a heap element.
                     */

    void** heap_vector;
                    /*  The vector used to hold the heap elements.  It is
                     *  dynamically allocated.  It is a standard C 0-origin
                     *  vector.  Access utilities are used to map the 1-origin
                     *  indexing used by the heap algorithms onto the 0-origin
                     *  'heap_vector'.
                     */
} PRQ;


extern void
PRQ_Initialize(
    PRQ*                    prq,
    PRQ_COMPARISON_FUNCTION comparison_fn,
    PRQ_GET_INDEX_FUNCTION  get_fn,
    PRQ_SET_INDEX_FUNCTION  set_fn,
    MEM_POOL*               pool,
    INT32                   initial_size,
    INT32                   expansion_factor
);

extern void*
PRQ_Delete_Top(
    PRQ*    prq
);

extern void*
PRQ_Top(
    PRQ*    prq
);

extern void
PRQ_Insert(
    PRQ*    prq,
    void*   element
);

extern void
PRQ_Remove(
    PRQ*    prq,
    void*   element
);

extern void
PRQ_Reset(
    PRQ*    prq
);

extern INT32
PRQ_Size(
    PRQ*    prq
);

extern void*
PRQ_Ith(
    PRQ*    prq,
    INT32   i
);


/* Look Ma, no templates!
 */

/* Generate the names used in the typesafe interface and invoke the low
 * level interface generation macro:
 */
#define TYPE_PRQ(base_type,prq_type)                                    \
    _TYPE_PRQ(base_type,                                                \
              prq_type,                                                 \
              prq_type##_COMPARISON_FUNCTION,                           \
              prq_type##_GET_INDEX_FUNCTION,                            \
              prq_type##_SET_INDEX_FUNCTION,                            \
              prq_type##_Initialize,                                    \
              prq_type##_Delete_Top,                                    \
              prq_type##_Top,                                           \
              prq_type##_Insert,                                        \
              prq_type##_Remove,                                        \
              prq_type##_Reset,                                         \
              prq_type##_Size,                                          \
              prq_type##_Ith)


/* Actually generate the typesafe interface:
 */
#define _TYPE_PRQ(base_type,                                            \
                  prq_type,                                             \
                  comparison_function_type,                             \
                  get_index_function_type,                              \
                  set_index_function_type,                              \
                  initialize_function_name,                             \
                  delete_top_function_name,                             \
                  top_function_name,                                    \
                  insert_function_name,                                 \
                  remove_function_name,                                 \
                  reset_function_name,                                  \
                  size_function_name,                                   \
                  ith_function_name)                                    \
                                                                        \
typedef PRQ prq_type;                                                   \
                                                                        \
typedef INT (*comparison_function_type)(base_type*,base_type*);         \
typedef INT32 (*get_index_function_type)(base_type*);                   \
typedef void (*set_index_function_type)(base_type*,INT32);              \
                                                                        \
/*REFERENCED*/								\
inline base_type*                                                       \
delete_top_function_name(                                               \
  prq_type* prq                                                         \
)                                                                       \
{                                                                       \
  return (base_type*) PRQ_Delete_Top((PRQ*) prq);                       \
}                                                                       \
                                                                        \
/*REFERENCED*/								\
inline base_type*                                                       \
top_function_name(                                                      \
  prq_type* prq                                                         \
)                                                                       \
{                                                                       \
  return (base_type*) PRQ_Top((PRQ*) prq);                              \
}                                                                       \
                                                                        \
/*REFERENCED*/								\
inline void                                                             \
insert_function_name(                                                   \
  prq_type*    prq,                                                     \
  base_type*   element                                                  \
)                                                                       \
{                                                                       \
  PRQ_Insert((PRQ*)prq,(void*)element);                                 \
}                                                                       \
                                                                        \
/*REFERENCED*/								\
inline void                                                             \
remove_function_name(                                                   \
  prq_type*   prq,                                                      \
  base_type*  element                                                   \
)                                                                       \
{                                                                       \
  PRQ_Remove((PRQ*)prq,(void*)element);                                 \
}                                                                       \
                                                                        \
/*REFERENCED*/								\
inline void                                                             \
reset_function_name(                                                    \
  prq_type* prq                                                         \
)                                                                       \
{                                                                       \
  PRQ_Reset((PRQ*)prq);                                                 \
}                                                                       \
                                                                        \
/*REFERENCED*/								\
inline INT32                                                            \
size_function_name(                                                     \
  prq_type*   prq                                                       \
)                                                                       \
{                                                                       \
  return PRQ_Size((PRQ*)prq);                                           \
}                                                                       \
                                                                        \
/*REFERENCED*/								\
inline base_type*                                                       \
ith_function_name(                                                      \
  prq_type* prq,                                                        \
  INT32     i                                                           \
)                                                                       \
{                                                                       \
  return (base_type*)PRQ_Ith((PRQ*)prq,i);                              \
}                                                                       \
                                                                        \
/*REFERENCED*/								\
inline void                                                             \
initialize_function_name(                                               \
  prq_type*                 prq,                                        \
  comparison_function_type  comparison_fn,                              \
  get_index_function_type   get_index,                                  \
  set_index_function_type   set_index,                                  \
  MEM_POOL*                 pool,                                       \
  INT32                     initial_size,                               \
  INT32                     expansion_factor                            \
)                                                                       \
{                                                                       \
  PRQ_Initialize((PRQ*)prq,(PRQ_COMPARISON_FUNCTION)comparison_fn,      \
                           (PRQ_GET_INDEX_FUNCTION)get_index,           \
                           (PRQ_SET_INDEX_FUNCTION)set_index,           \
                           pool,                                        \
                           initial_size,                                \
                           expansion_factor);                           \
}


#ifdef __cplusplus
}
#endif
#endif
