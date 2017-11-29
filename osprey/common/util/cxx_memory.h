/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


// -*-C++-*-

/**
*** Description:
***
***	C++ macro replacements for new and delete using memory pools.
***	new and delete should never be used in the compiler;
***	these should be used instead.
***
*** Exported preprocessor macro
***
***	CXX_USE_STANDARD_NEW_AND_DELETE
***
***	    If this preprocessor macro is defined, then the macros below
***	    just ignore the memory pools and call the normal new and delete.
***
*** Exported functions:
***
***	CXX_NEW(constructor, MEM_POOL*)
***
***	    Use CXX_NEW instead of new.  E.g.
***
***		X* x = new X(3,4);
***
***	    should be replaced by
***
***		X* x = CXX_NEW(X(3,4), malloc_pool);
***
***	    (or whatever appropriate pool name).
***
***	CXX_NEW_ARRAY(constructor, elements, MEM_POOL*)
***
***	    Use CXX_NEW_ARRAY instead of new [].  E.g.
***
***		X* x = new X[a*b];
***
***	    should be replaced by
***
***		X* x = CXX_NEW_ARRAY(X, a*b, malloc_pool);
***
***	    (or whatever appropriate pool name).
***
*** CXX_NEW_VARIANT(constructor, pad, MEM_POOL*)
***
***     Like CXX_NEW allocates an instance of type "constructor"
***
***     but allocates "pad" amount of extra storage 
***
***     Could be viewed as X* x = (X*) malloc (sizeof(X)+pad)
***
***     Followed by a call to the constructor for X.
***     
***     There is no C++ equivalent. Therefore not available when using
*** 
***     standard new and delete.
***
***	CXX_DELETE(pointer, MEM_POOL*)
***
***	    Use CXX_DELETE instead of delete.  E.g.
***
***		delete p;
***
***	    should be replaced by
***
***		CXX_DELETE(p, malloc_pool);
***
***	    (or whatever appropriate pool name).
***
***	CXX_DELETE_ARRAY(pointer, MEM_POOL*)
***
***	    Use CXX_DELETE_ARRAY instead of delete[].  E.g.
***
***		delete[] p;
***
***	    should be replaced by
***
***		CXX_DELETE_ARRAY(p, malloc_pool);
***
***	    (or whatever appropriate pool name).
***
***
**/

/** $Revision: 1.1 $
*** $Date: 2005/07/27 02:17:56 $
*** $Author: kevinlo $
*** $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/cxx_memory.h,v $
**/

#ifndef CXX_MEMORY_INCLUDED
#define CXX_MEMORY_INCLUDED

#ifndef mempool_INCLUDED
#include "mempool.h"
#endif

#ifdef CXX_USE_STANDARD_NEW_AND_DELETE

#define CXX_NEW(constructor, mempool)			\
	(new constructor)

#define CXX_NEW_ARRAY(constructor, elements, mempool)	\
	(new constructor[elements])

#define CXX_DELETE(pointer, mempool)			\
	(delete pointer)

#define CXX_DELETE_ARRAY(pointer, mempool)		\
	(delete[] pointer)

#else

//////////////////////////////////////////////////////////////
//  
//              Allocators using MEM_POOL 
//
//////////////////////////////////////////////////////////////
//
#include <new>
  extern MEM_POOL* Delete_Mem_Pool;

// part 1: prototype of overided placement new operator
//
void* operator new (size_t, MEM_POOL*, size_t pad, INT32 ln, const char* file)
#ifdef __GNUC__
    throw(std::bad_alloc) 
#endif
    ;

// part 2: implementation of CXX_NEW and CXX_NEW_VARIANT
//
#ifdef Is_True_On

#define CXX_NEW(constructor, mempool)			\
  (new(mempool, 0, __LINE__, (const char*)__FILE__) constructor)

#define CXX_NEW_VARIANT(constructor, pad, mempool)	\
  (new(mempool, pad, __LINE__, (const char*)__FILE__) constructor)

#else	// Is_True_On

#define CXX_NEW(constructor, mempool)	(new(mempool, 0, 0, NULL) constructor)
#define CXX_NEW_VARIANT(constructor, pad, mp) (new(mp, pad, 0, NULL) constructor)

#endif

// part 3: implementation of CXX_DELETE
//
template<class T> void
cxx_del_opr (T* ptr, MEM_POOL* mp) {
    if (ptr != NULL) {
        ptr->~T();
        MEM_POOL_FREE (mp, (void*)ptr);
    }
}

#define CXX_DELETE(pointer, mp) cxx_del_opr(pointer, mp)

// part 4: implementation of CXX_NEW_ARRAY
//
template <class T>
UINT32 _calc_array_alloc_pad_sz (void) {
    UINT32 sz1 = sizeof(UINT32);
    UINT32 sz2 = __alignof__ (T);

    return sz1 > sz2 ? sz1 : sz2;
}

template <class T> T*
cxx_array_alloc_opr (UINT32 elem_cnt, MEM_POOL* mp, INT32 line, const char* file) {
    
    if (mp == Malloc_Mem_Pool) {
        return new T[elem_cnt];
    }

    UINT32 pad = _calc_array_alloc_pad_sz<T>(); 
    UINT32 sz = sizeof(T) * elem_cnt;
    
    char* ptr;
    #ifdef Is_True_On
        ptr = (char*)MEM_POOL_Alloc_P(mp, sz+pad, line, file);
    #else
        ptr = (char*)MEM_POOL_Alloc_P(mp, sz+pad, 0, NULL);
    #endif
    
    *(UINT32*)ptr = elem_cnt;
    T* elem_ptr = (T*)(void*)(ptr + pad);

    for (INT32 i = 0; i < elem_cnt; i++) {
        new (elem_ptr+i) T();
    }

    return elem_ptr;
}

#ifdef Is_True_On
    #define CXX_NEW_ARRAY(T, elements, mempool)	\
	            cxx_array_alloc_opr<T>((UINT32)(elements), (MEM_POOL*)mempool, \
                 (INT32)__LINE__, (const char*)__FILE__)
#else
    #define CXX_NEW_ARRAY(T, elements, mempool)	\
	            cxx_array_alloc_opr<T>((UINT32)(elements), mempool, 0, NULL)
#endif

// part 5: implementation of CXX_DELETE_ARRAY
//
template <class T> void
cxx_delete_array_opr (T* elem_ptr, MEM_POOL* mp) {

    if (mp == Malloc_Mem_Pool) {
        delete [] elem_ptr;
        return;
    }

    if (elem_ptr != NULL) {
        UINT32 pad = _calc_array_alloc_pad_sz<T>(); 

        char* ptr = (char*)(void*)elem_ptr;
        UINT32 elem_cnt = *(UINT32*)(void*)(ptr - pad);

        for (UINT32 i = 0; i < elem_cnt; i++) {
            (elem_ptr+i)->~T();
        }
        MEM_POOL_FREE (mp, (void*)(ptr-pad));
    }
}

#define CXX_DELETE_ARRAY(pointer, mempool) cxx_delete_array_opr (pointer, mempool)

#define DECL_CXX_ALLOC_AS_FRIEND(T)  \
    friend void cxx_delete_array_opr<T>(T*, MEM_POOL*); \
    friend T* cxx_array_alloc_opr<T> (UINT32, MEM_POOL*, INT32, const char*); \
    friend void cxx_del_opr<T>(T*, MEM_POOL*)

#endif // CXX_USE_STANDARD_NEW_AND_DELETE

#endif
