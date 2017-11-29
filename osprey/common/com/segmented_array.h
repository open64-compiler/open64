/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/* -*- c++ -*-
 *
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef segmented_array_INCLUDED
#define segmented_array_INCLUDED

#ifndef __SGI_STL_VECTOR_H

#if defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)
#undef short				// get around bogus type defs.
#undef int
#undef long
#endif // defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)

#ifndef __GNUC__
#include <CC/vector>
#else
#include "vector"
#endif

#endif // __SGI_STL_VECTOR_H

#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif // ERRORS_INCLUDED

#ifndef mempool_INCLUDED
#include "mempool.h"
#endif // mempool_INCLUDED

#ifndef mempool_allocator_INCLUDED
#include "mempool_allocator.h"
#endif

#ifdef KEY // needed for building with g++ 3.2
using std::pair;	
using std::vector;	
using std::forward_iterator_tag;	
#endif

// ARRAY_Ptr is pointer to SEG_ARRAY,
// VALUE_Ptr is pointer to SEG_ARRAY::value_type,
// (Changed _PTR to VALUE_Ptr to avoid conflict on cygwin)
// REF is reference to SEG_ARRAY::value_type.
//
// These are template parameters because we need both constant and
// mutable iterators.  For a constant iterator they will be,
// respectively, const SEG_ARRAY*, const SEG_ARRAY::value_type*, and
// const SEG_ARRAY::value_type&.  For a mutable iterator they will be
// SEG_ARRAY*, SEG_ARRAY::value_type*, and SEG_ARRAY::value_type&.

// TO DO: add a conversion from the mutable version to the constant
// version.

template <class ARRAY_Ptr, class T, class VALUE_Ptr, class REF>
class SEGMENTED_ARRAY_ITERATOR
{
public:
  typedef T                         value_type;
  typedef UINT                      difference_type;
  typedef std::forward_iterator_tag iterator_category;
  typedef VALUE_Ptr                 pointer;
  typedef REF                       reference;

private:
    
    ARRAY_Ptr segmented_array;
    VALUE_Ptr ptr;			// pointer to the current element
    VALUE_Ptr segment_last;		// ptr after the current segment
    UINT map_idx;			// index to the map array

private:

    typedef SEGMENTED_ARRAY_ITERATOR self;

public:

    SEGMENTED_ARRAY_ITERATOR (ARRAY_Ptr sa, T* p, T* last, UINT idx) 
	: segmented_array (sa), ptr (p), segment_last (last) {
	    map_idx = sa->Block_index(idx);
    }

    SEGMENTED_ARRAY_ITERATOR (ARRAY_Ptr sa, UINT idx)
	: segmented_array (sa) {
            map_idx = sa->Block_index(idx);
	    ptr = &(sa->Entry(idx));
            segment_last = sa->Block_end(map_idx);
    }

    SEGMENTED_ARRAY_ITERATOR () {}

    REF operator* () const		{ return *ptr; }
    VALUE_Ptr Ptr () const		{ return ptr; }
    VALUE_Ptr operator->() const        { return ptr; }
    UINT Index () const {
      return map_idx * segmented_array->Block_size() +
             (ptr - segmented_array->Block_begin(map_idx));
    }

    self& operator ++ () {
        ++ptr;
	if (ptr == segment_last) {
	    UINT map_entries =
		(segment_last - segmented_array->Block_begin(map_idx)) / 
		segmented_array->Block_size ();
            if (map_idx + map_entries < segmented_array->Block_index_end()) {
                map_idx += map_entries;
                ptr = segmented_array->Block_begin(map_idx);
                segment_last = segmented_array->Block_end(map_idx);
            }
        }
	return *this;
    }

    self operator ++ (int) {
	self tmp = *this;
	++(*this);
	return tmp;
    }

    BOOL operator == (const self& x) const	{ return ptr == x.ptr; }
    BOOL operator != (const self& x) const	{ return !(*this == x); }

}; // SEGMENTED_ARRAY_ITERATOR


template <class T, UINT block_size = 128>
class SEGMENTED_ARRAY
{
private:
    typedef std::pair<T *, BOOL> thingy;
    std::vector<thingy, mempool_allocator<thingy> > map;
    MEM_POOL *pool;
    UINT size_;				// total number of elements inserted
    UINT max_size_;			// total # of elements allocated
    INT block_base;			// idx of the beginning of
					// block (signed so we can set
					// to -1, meaning no block allocated)
    UINT next_block_size;		// size of block to be allocated
    T *block;				// points to the last block

private:
    typedef SEGMENTED_ARRAY<T, block_size> self;

public:
    typedef T base_type;

    typedef T                 value_type;
    typedef value_type*       pointer;
    typedef const value_type* const_pointer;
    typedef value_type&       reference;
    typedef const value_type& const_reference;
    typedef UINT              size_type;
    typedef INT               difference_type;

    typedef SEGMENTED_ARRAY_ITERATOR<self*, T, pointer, reference>
            iterator;
    typedef SEGMENTED_ARRAY_ITERATOR<const self*, T, 
                                     const_pointer, const_reference> 
            const_iterator;

private:

    // private operations
    
    UINT Round_up (UINT s) {
	UINT mask = block_size - 1;
	return (s + mask) & ~mask;
    }

    void Update_Map (T *marker, UINT new_size, BOOL own_memory);

    void Pop_Map ();

    // allocate a block, assume the array is completely filled
    void Allocate ();

    T& New_entry () {
	if (size_ == max_size_) Allocate ();
	return block[size_++ - block_base];
    }
    
    // copy n elements to current buffer, assume no overflow
    void Copy (const T* x, UINT n) {
	std::copy(x, x + n, block + (size_ - block_base));
	size_ += n;
    }

    // block_idx is an index into the map.  This function returns
    // the first map index that points to a different block. 
    UINT next_block_idx(UINT block_idx) const {
      for ( ; block_idx + 1 < map.size() &&
              map[block_idx].first + block_size == map[block_idx + 1].first ;
            ++block_idx)
        {}
      return block_idx + 1;
    }

public:

  SEGMENTED_ARRAY(MEM_POOL *m = Malloc_Mem_Pool) : pool (m), map (m) {
    size_ = max_size_ = next_block_size = 0;
    block_base = -1;
    block = 0;
  }

  ~SEGMENTED_ARRAY() {
    // Free memory from blocks. Map memory gets freed when the map
    // vector is destructed.
    for (typename std::vector<thingy, mempool_allocator<thingy> >::iterator
	   entry = map.begin();
	 entry != map.end();
	 ++entry) {
      // entry->second <==> this map entry owns the block's memory.
      if (entry->second) {
	MEM_POOL_FREE(pool, entry->first);
      }
    }
  }

  UINT Block_size () const	{ return block_size; }

  UINT Size () const		{ return size_; }
  UINT size () const		{ return size_; }

    T& Entry (UINT idx) {
	Is_True (idx < size_, ("Array subscript out of bound"));
	return map[idx / block_size].first[idx % block_size];
    }

    const T& Entry (UINT idx) const {
	Is_True (idx < size_, ("Array subscript out of bound"));
	return map[idx / block_size].first[idx % block_size];
    }
    T& operator[] (UINT idx)             { return Entry(idx); }
    const T& operator[] (UINT idx) const { return Entry(idx); }

    iterator begin () {
	return iterator (this, map[0].first, Block_end (0), 0);
    }

    iterator end () {
	return iterator (this, block + (size_ - block_base),
			 block + (max_size_ - block_base), size_);
    }

    const_iterator begin () const {
	return const_iterator (this, map[0].first, Block_end (0), 0);
    }

    const_iterator end () const {
	return const_iterator (this, block + (size_ - block_base),
                               block + (max_size_ - block_base), size_);
    }

	
    T& New_entry (UINT& idx)	{ idx = size_; return New_entry (); }

    UINT Insert (const T& x);

    void Delete_last () {
	--size_;
	if (size_ == block_base)
	    Pop_Map ();
    }
	    

    void Delete_last (UINT n);

    void Delete_down_to (UINT idx) {
	if (size_ > idx)
	    Delete_last (size_ - idx);
    }

    // insert multiple elements, always copy to new buffer.
    UINT Insert (const T* x, UINT n_elemt);
    
    // similar to insert, except reuse the given buffer if possible
    UINT Transfer (T* x, UINT n_elemt);
    
    // Reserve extra storage.  Actual allocation will be done when the
    // already allocated storage is filled.
    void Reserve (UINT n_elemt)	{
	if (max_size_ - size_ + next_block_size < n_elemt)
	    next_block_size = n_elemt - (max_size_ - size_);
    }

    // return the number of element till the end of the block
    UINT Get_block_size (UINT idx) const {
      UINT block_idx = idx / block_size;
      return std::min(next_block_idx(block_idx) * block_size, size_) - idx;
    }

    UINT Block_index (UINT idx) const { return idx / block_size; }

    // A valid block index n is in the range 0 <= n < Block_index_end().
    UINT Block_index_end () const { return map.size(); }

    T* Block_begin (UINT block_idx)             { return map[block_idx].first; }
    const T* Block_begin (UINT block_idx) const { return map[block_idx].first; }

    T* Block_end(UINT block_idx) {
      return Block_begin(block_idx) +
             (next_block_idx(block_idx) - block_idx) * block_size;
    }

    const T* Block_end(UINT block_idx) const {
      return Block_begin(block_idx) +
             (next_block_idx(block_idx) - block_idx) * block_size;
    }

}; // SEGMENTED_ARRAY


template <class T, UINT block_size>
inline void
SEGMENTED_ARRAY<T,block_size>::Update_Map(T    *marker,
					  UINT  new_size,
					  BOOL  own_memory)
{
  do {
    map.push_back(std::pair<T*, BOOL>(marker, own_memory));
    new_size -= block_size;
    marker += block_size;
    own_memory = FALSE;
  } while (new_size);
} // SEGMENTED_ARRAY<T,block_size>::Update_Map


// deallocate a block and re-adjust the map and other variables
template <class T, UINT block_size>
void
SEGMENTED_ARRAY<T,block_size>::Pop_Map ()
{
    next_block_size += max_size_ - block_base;
    MEM_POOL_FREE (pool, block);

    T *last_map_entry;
    do {
      last_map_entry = (map.end() - 1)->first;
      map.pop_back ();
    } while (last_map_entry != block);

    max_size_ = size_;
    if (size_ > 0) {
      Is_True(size_ >= block_size,
	      ("SEGMENTED_ARRAY: size in limbo"));
      block_base = size_ - block_size;
      UINT idx = block_base / block_size;
      block = map[idx].first;
      while (idx > 0 && map[idx - 1].first + block_size == block) {
	block = map[--idx].first;
	block_base -= block_size;
      }
    }
    else {
      Is_True(map.begin() == map.end(),
	      ("SEGMENTED_ARRAY::Pop_Map: Map should be empty"));
      block_base = -1;
      block = NULL;
    }
} // SEGMENTED_ARRAY<T,block_size>::Pop_Map


template <class T, UINT block_size>
void
SEGMENTED_ARRAY<T,block_size>::Allocate ()
{
    Is_True (size_ == max_size_, ("Invalid internal state in segmented array"));

    UINT new_size;

    if (next_block_size == 0)
	new_size = block_size;
    else {
	new_size = Round_up (next_block_size);
	next_block_size = 0;
    }

    block = (T *) MEM_POOL_Alloc (pool, new_size * sizeof(T));
    max_size_ += new_size;
    block_base = size_;

    Update_Map (block, new_size, TRUE);
    
} // SEGMENTED_ARRAY::Allocate


template <class T, UINT block_size>
void
SEGMENTED_ARRAY<T,block_size>::Delete_last (UINT n)
{
    while (n >= size_ - block_base) {
	n -= size_ - block_base;
	size_ = block_base;
	Pop_Map ();
    }
    
    size_ -= n;
} // Delete_last


template <class T, UINT block_size>
inline UINT
SEGMENTED_ARRAY<T,block_size>::Insert (const T& x)
{
    UINT idx = size_;
    T &entry = New_entry ();

    entry = x;
    return idx;
} // SEGMENTED_ARRAY::Insert


template <class T, UINT block_size>
UINT
SEGMENTED_ARRAY<T,block_size>::Insert (const T* x, UINT n_elemt)
{
    UINT result = size_;
    if (size_ + n_elemt <= max_size_) {
	Copy (x, n_elemt);
	return result;
    }

    UINT space_left = max_size_ - size_;
    Copy (x, space_left);
    n_elemt -= space_left;

    Reserve (n_elemt);
    Allocate ();
    Copy (x + space_left, n_elemt);

    return result;
} // SEGMENTED_ARRAY::Insert


template <class T, UINT block_size>
UINT
SEGMENTED_ARRAY<T,block_size>::Transfer (T* x, UINT n_elemt)
{
    UINT result = size_;

    if (size_ + n_elemt <= max_size_) {
	Copy (x, n_elemt);
	return result;
    }

    UINT space_left = max_size_ - size_;
    if (space_left > 0) {
	Copy (x, space_left);
	n_elemt -= space_left;
	x += space_left;
    }

    if (n_elemt >= block_size) {
	UINT reused_size = n_elemt & ~(block_size - 1);
	block = x;
	Update_Map (block, reused_size, FALSE);
	block_base = size_;
	size_ += reused_size;
	max_size_ += reused_size;
	n_elemt -= reused_size;
	x += reused_size;
	if (next_block_size > reused_size)
	    next_block_size -= reused_size;
	else
	    next_block_size = 0;
    }

    if (n_elemt > 0) {
	Allocate ();
	Copy (x, n_elemt);
    }

    return result;
    
} // SEGMENTED_ARRAY::Transfer


template <class T, UINT block_size, class OP>
inline void
For_all_entries (SEGMENTED_ARRAY<T, block_size>& array, const OP &op,
		 UINT32 first = 0)
{
    UINT last = array.size ();

    while (first < last) {
	T *block = &array[first];
	UINT size = array.Get_block_size (first);
	for (UINT j = 0; j < size; ++j, ++block)
	    op (first + j, block);
	first += size;
    }
}

// The following function is ifdefed out because, until we have
// partial ordering of function templates, the compiler will flag it
// as ambiguous.

template <class T, UINT block_size, class OP>
inline void
For_all_blocks (SEGMENTED_ARRAY<T, block_size>& array, const OP &op)
{
    UINT max_size = array.size ();
    UINT i = 0;

    while (i < max_size) {
	T *block = &array[i];
	UINT size = array.Get_block_size (i);
	op (i, block, size);
	i += size;
    }
}


#define NOT_FOUND ((UINT) -1)

template <class T, UINT block_size, class PREDICATE>
inline UINT
Find_entry_if (const SEGMENTED_ARRAY<T, block_size>& array,
	       const PREDICATE& pred, UINT i = 0)
{
    UINT max_size = array.size ();

    while (i < max_size) {
	const T *block = &array[i];
	UINT size = array.Get_block_size (i);
	for (UINT j = 0; j < size; ++j, ++block)
	    if (pred (i+j, block))
		return i + j;
	i += size;
    }

    return (UINT) NOT_FOUND;
}


// copy the content of one segmented array to another, optionally specified
// the range [first_idx, last_idx) to be copied.
// returns the number of entries copied.
template <class T, UINT block_size>
UINT32
Copy_array_range (const SEGMENTED_ARRAY<T, block_size>& from_array,
		  SEGMENTED_ARRAY<T, block_size>& to_array,
		  UINT32 first_idx = 0, UINT32 last_idx = (UINT32) -1)
{
    if (last_idx > from_array.size ())
	last_idx = from_array.size ();

    Is_True (last_idx >= first_idx, ("Invalid copy range"));

    UINT32 entries = last_idx - first_idx;

    to_array.Reserve (entries);

    while (first_idx < last_idx) {
	const T* block = &from_array[first_idx];
	UINT32 size = from_array.Get_block_size (first_idx);
	if (size > last_idx - first_idx)
	    size = last_idx - first_idx;

	to_array.Insert (block, size);
	first_idx += size;
    }

    return entries;
} // Copy_array_range

#endif /* segmented_array_INCLUDED */
