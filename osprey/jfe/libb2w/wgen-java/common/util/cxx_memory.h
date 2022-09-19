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

/** $Revision: 1.1.1.1 $
*** $Date: 2005/10/21 19:00:00 $
*** $Author: marcel $
*** $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/cxx_memory.h,v $
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

#include <new>
  extern MEM_POOL* Delete_Mem_Pool;

extern MEM_POOL* _dummy_new_mempool;
extern MEM_POOL* _dummy_delete_mempool;

#ifdef Is_True_On
extern int         _alloc_callsite_line;
extern const char *_alloc_callsite_file;
#endif

extern size_t _dummy_pad;

#ifdef Is_True_On
#define CXX_NEW(constructor, mempool)			\
  (_dummy_new_mempool = mempool,			\
   _alloc_callsite_line = __LINE__,			\
   _alloc_callsite_file = __FILE__,			\
   new constructor)

#define CXX_NEW_ARRAY(constructor, elements, mempool)	\
  (_dummy_new_mempool = mempool,			\
   _alloc_callsite_line = __LINE__,			\
   _alloc_callsite_file = __FILE__,			\
   new constructor [elements])

#define CXX_NEW_VARIANT(constructor, pad, mempool)	\
  (_dummy_new_mempool = mempool,			\
   _alloc_callsite_line = __LINE__,			\
   _alloc_callsite_file = __FILE__,			\
   _dummy_pad = pad, new constructor)
#else	// Is_True_On
#define CXX_NEW(constructor, mempool)			\
  (_dummy_new_mempool = mempool,			\
   new constructor)

#define CXX_NEW_ARRAY(constructor, elements, mempool)	\
  (_dummy_new_mempool = mempool,			\
   new constructor [elements])

#define CXX_NEW_VARIANT(constructor, pad, mempool)	\
  (_dummy_new_mempool = mempool,			\
   _dummy_pad = pad, new constructor)
#endif

#define CXX_DELETE(pointer, mempool)			\
do {   \
  MEM_POOL* save_mpool = _dummy_delete_mempool;     \
  _dummy_delete_mempool = mempool;  \
  delete pointer;    \
  _dummy_delete_mempool = save_mpool;   \
  } while (0)

#define CXX_DELETE_ARRAY(pointer, mempool)		\
do {   \
  MEM_POOL* save_mpool = _dummy_delete_mempool;     \
  _dummy_delete_mempool = mempool;  \
  delete [] pointer;    \
  _dummy_delete_mempool = save_mpool;   \
} while (0)

#endif

#endif
