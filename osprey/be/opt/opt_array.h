//-*-c++-*-
//=====================================================================
//
// Module: opt_array.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_array.h,v $
//
// Revision history:
//  01-MAR-95 dahl - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//	template for fixed length arrays
//
//======================================================================
//======================================================================


#ifndef opt_array_INCLUDED
#define opt_array_INCLUDED "opt_array.h"
#ifdef _KEEP_RCS_ID

static char *opt_arrayrcs_id = opt_array_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */


// forward declaration
typedef struct mem_pool MEM_POOL;

#include "defs.h"
#include "opt_defs.h"
#include "opt_sys.h"


//----------------------------------------------------------------------------
// fixed size array template
template <class TT>
class ARRAY {

private:
  MEM_POOL	*_mem_pool;			// memory pool for array
  mUINT32	 _size;				// number of elements in array
  TT		*_array;			// the actual array

  ARRAY<TT>(const ARRAY&);			// PPP what is this ???

public:

  ARRAY(void);                                  // default constructor
  ARRAY(MEM_POOL *pool)                         { _mem_pool = pool;
                                                  _size = 0;
                                                  _array = NULL;
                                                }
  ARRAY(mUINT32, MEM_POOL *);			// constructor
  ~ARRAY(void) { }				// destructor
  void Init(mUINT32 size);                      // Initialize array with size

  MEM_POOL *Mem_Pool(void) { return _mem_pool; }// what pool is being used
  void Free_array(void);			// free the arrays memory
  void Bzero_array(void);			// zero out the array
  mUINT32 Size(void) const { return _size; }	// size of the array
  TT& operator [] (mUINT32 idx) const {		// access array by index idx
	  Is_True(idx < _size,("ARRAY::[%d]:Subscript out of range %d",idx,_size));
	  return (_array[idx]); }
  TT& operator [] (mUINT32 idx) {		// access array by index idx
	  Is_True(idx < _size,("ARRAY::[%d]:Subscript out of range %d",idx,_size));
	  return (_array[idx]); }

}; // end of class ARRAY


//----------------------------------------------------------------------------
// constructor
// can be given a zero size to delay the allocation until later
template <class TT>
ARRAY<TT>::ARRAY(mUINT32 sz, MEM_POOL *mpool)
{
  _mem_pool = mpool;
  _size = sz;
  if (_size > 0) {
    _array = (TT *)OPT_POOL_Alloc(_mem_pool, _size * sizeof(TT),
				  MEM_DUMP_FLAG+15);
    if (_array == NULL)
      ErrMsg(EC_No_Mem,"ARRAY<TT>::Alloc_array");
    Bzero_array();
  } else
    _array = NULL;
}

//----------------------------------------------------------------------------
// Initializer
// can be given a size to allocate array
template <class TT>
void
ARRAY<TT>::Init(mUINT32 size)
{
  _size = size;
  if (_size > 0) {
    _array = (TT *)OPT_POOL_Alloc(_mem_pool, _size * sizeof(TT),
				  MEM_DUMP_FLAG+15);
    if (_array == NULL)
      ErrMsg(EC_No_Mem,"ARRAY<TT>::Alloc_array");
    Bzero_array();
  }
}

// free the array to the memory pool
template <class TT>
void
ARRAY<TT>::Free_array(void)
{
  if (_array != NULL)
    OPT_POOL_FREE(_mem_pool,_array, MEM_DUMP_FLAG+15);
  _size = 0;
  _mem_pool = _array = NULL;
}

// zero out the array
template <class TT>
void
ARRAY<TT>::Bzero_array(void)
{
  if (_array != NULL)
    BZERO(_array,sizeof(TT) * _size);
}

#endif //opt_array_INCLUDED
