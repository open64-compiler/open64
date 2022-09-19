/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include "defs.h"
#include "cxx_memory.h"
#include "errors.h"

MEM_POOL* _dummy_new_mempool = (MEM_POOL*) -1;
MEM_POOL* _dummy_delete_mempool = (MEM_POOL*) -1;
size_t _dummy_pad = 0;

int         _alloc_callsite_line = 0;
const char *_alloc_callsite_file = NULL;

/* Description of new scheme:
 * ==========================
 *
 * Many C++ implementations don't support calling destructors for 
 * array objects allocated using placement new (which we need since we
 * want to allocate memory from the memory pools rather than malloc).
 * In fact, I believe the language is either ambiguous about this, or
 * explicitly does not support this.
 *
 * We need to mangle new/delete to call MEM_POOL_Alloc and MEM_POOL_FREE
 * instead of malloc and free. 
 *
 * We used to do this using placement new/delete in which we would pass in 
 * extra arguments to new/delete (e.g. mempool) and then have them allocate
 * memory from the right place. To make sure that constructors/destructors 
 * for arrays of objects allocated using placement new were called correctly,
 * Shankar et al had a hack using hash tables to maintain the number of
 * elements and call the destructors correctly. This hack was invoked using
 * -Wf,-Yv to the 5.3 compilers.
 *
 * This hack is gone in Mongoose. We therefore use the following scheme:
 * - instead of using placement new, we communicate with new using global
 *   variables instead of parameters. As a result the standard C++ 
 *   implementation automatically counts the number of array elements, and
 *   calls the constructor/destructor the correct number of times.
 * - we define the CXX_NEW and CXX_NEW_ARRAY macros to set the global
 *   _dummy_new_mempool variable, and the new operator to MEM_POOL_Alloc 
 *   memory from this memory pool. Since CXX_NEW must be an expression, we
 *   cannot have a scope within which to save the incoming value of this dummy,
 *   and therefore there is a likelihood of this variable being overwritten
 *   before it is used. Therefore the new operator resets this dummy to be -1
 *   so that when the actual new comes around, it's value is gone, leading to
 *   a compiler-runtime error. Thus CXX_NEWs cannot be nested.
 *   Note that the last value in the expressions must be the return value
 *   of the new operator, to have the types in the assignment x = CXX_NEW(...)
 *   work out OK.
 * - we define the CXX_DELETE and CXX_DELETE_ARRAY macros to set the global
 *   value of dummy_delete_mempool, which is used by the operator delete
 *   to MEM_POOL_FREE the memory appropriately. Since CXX_DELETE is a 
 *   statement and not an expression, it can have a scope and save the 
 *   incoming value in a stack variable. So CXX_DELETEs can be nested.
 * - the final issue is to make sure that no one ever calls the original 
 *   new/delete operators -- this is arranged by exporting the new and delete
 *   operators (__nw__FUi and __dl__FPv) in be/be/Exported. This overrides
 *   calls to new and delete in libC from __vec_new, __vec_delete, etc.
 *
 *
 *
 * ABANDONED SCHEME:
 * =================
 *
 * I experimented with another scheme in which I explicitly maintained the
 * number of elements of an array in 8 extra bytes of storage before the 
 * pointer for all arrays. This scheme was abandoned since the above works 
 * better, but here are some lessons learnt. The scheme was:
 * - try always using placement new, with the argument to new being the storage
 *   allocated from MEM_POOL_Alloc. E.g.
 *    #define CXX_NEW(type, pool) new (MEM_POOL_Alloc(sizeof(type), pool)) type
 *   But this doesn't work since "type" might have arguments to the constructor
 * - so then we pass the memory pool as an argument to a placement new 
 *   operator that takes arguments.
 *    #define CXX_NEW(constructor, mempool)  (new (mempool) constructor)
 * - for arrays there are no arguments to constructors, so we allocate
 *   8 extra bytes, store the number of elements, and then call the above
 *   new to allocate an array. Since it's placement new, the compiler does
 *   nothing for arrays.
 * - for CXX_DELETE is easy - the operator delete is defined to do NOTHING.
 *   the macro first calls the destructor, then calls MEM_POOL_FREE to free
 *   the storage.
 * - for CXX_DELETE_ARRAY there is a loop -- look for the number of elements,
 *   call the destructor for each element of the array, and then call 
 *   MEM_POOL_FREE.
 * 
 * This scheme mostly worked. The problem came because the expansion of the
 * CXX_DELETE and CXX_DELETE_ARRAY macros contained the "pointer" argument
 * multiple times, and would therefore evaluate it repeatedly. So code like:
 *      while (stack.Elements ())
 *          CXX_DELETE (stack.Pop ());  // or delete_array
 * did not work, since the stack.Pop would happen more than once, lots of
 * times.
 * The fix for this was to change the macro definition to also pass the 
 * type in for the delete macros (at least for CXX_DELETE_ARRAY), but 
 * thankfully we found the solution that we now use. Inline functions did
 * not work since we need a variable of the type being deleted, and we would
 * need an inline function for each type that we want to delete.
 * 
 * The code for this abandoned scheme is in Rohit's workarea, in
 * /hosts/snowy.mti/work/workarea/v7.00/common/util/cxx_memory.{h,cxx}.shank
 */

void* operator new (size_t sz)
#ifdef __GNUC__
                               throw(std::bad_alloc) 
#endif /* __GNUC__ */
{
  void* ptr;
  if (_dummy_new_mempool == (MEM_POOL*) -1) {
#if (__GNUC__ < 3) // to many of this after building with gcc 3.2
    DevWarn("new: _dummy_new_mempool is not yet set; Using Malloc_Mem_Pool");
#endif
    _dummy_new_mempool = Malloc_Mem_Pool;
  }

  ptr = (void *) MEM_POOL_Alloc_P(_dummy_new_mempool,
				  sz+_dummy_pad,
#ifdef Is_True_On
				  _alloc_callsite_line,
				  _alloc_callsite_file);
  _alloc_callsite_file = NULL;
  _alloc_callsite_line = 0;
#else
				0, NULL);
#endif
  _dummy_new_mempool = (MEM_POOL*) -1;
  _dummy_pad = 0;
  return ptr;
}

void operator delete (void* ptr)
#ifdef __GNUC__
                                 throw() 
#endif /* __GNUC__ */
{
  if (_dummy_delete_mempool == (MEM_POOL*) -1) {
#if (__GNUC__ < 3) // to many of this after building with gcc 3.2
    DevWarn("new: _dummy_delete_mempool is not yet set; Using Malloc_Mem_Pool");
#endif
    _dummy_delete_mempool = Malloc_Mem_Pool;
  }

  MEM_POOL_FREE (_dummy_delete_mempool, ptr);
  _dummy_delete_mempool = (MEM_POOL*) -1;
}

#ifdef __GNUC__
void operator delete[] (void* ptr) throw() {
  if (_dummy_delete_mempool == (MEM_POOL*) -1) {
#if (__GNUC__ < 3) // to many of this after building with gcc 3.2
    DevWarn("new: _dummy_delete_mempool is not yet set; Using Malloc_Mem_Pool");
#endif
    _dummy_delete_mempool = Malloc_Mem_Pool;
  }

  MEM_POOL_FREE (_dummy_delete_mempool, ptr);
  _dummy_delete_mempool = (MEM_POOL*) -1;
}
#endif

