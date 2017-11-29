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

#ifndef cmplr_segmented_array_INCLUDED
#define cmplr_segmented_array_INCLUDED

#ifndef __SGI_STL_ALGO_H

#if defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)
#undef short				// get around bogus type defs.
#undef int
#undef long
#endif // defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)

#ifndef __GNUC__
#include <CC/algorithm>
#else
#include <algorithm>
#endif

#endif // __SGI_STL_ALGO_H

#ifndef __SGI_STL_VECTOR_H

#if defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)
#undef short				// get around bogus type defs.
#undef int
#undef long
#endif // defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)

#ifndef __GNUC__
#include <CC/vector>
#else
#include <vector>
#endif

#endif // __SGI_STL_VECTOR_H

#ifndef __SGI_STL_SLIST_H

#if defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)
#undef short				// get around bogus type defs.
#undef int
#undef long
#endif // defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)

#include <ext/slist>
#endif // __SGI_STL_LIST_H


#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif // ERRORS_INCLUDED

#ifndef mempool_INCLUDED
#include "mempool.h"
#endif // mempool_INCLUDED

#ifndef segmented_array_INCLUDED
#include "segmented_array.h"	// for SEGMENTED_ARRAY_ITERATOR
#endif // segmented_array_INCLUDED

#ifndef mempool_allocator_INCLUDED
#include "mempool_allocator.h"
#endif

using std::find;
using __gnu_cxx::slist;

// ======================================================================
// ======================================================================
// A RELATED_SEGMENTED_ARRAY is a segmented array that can enter into
// a particular kind of relationship with other
// RELATED_SEGMENTED_ARRAYs. The particular relationship is this: When
// one of the arrays (called the primary one) in the pair grows or
// shrinks, the other one (called the secondary, or child, or kid) grows
// automatically. The secondary array may be primary in relationships
// of its own, in which case its child(ren) is (are) resized
// automatically, etc.
//
// From the time that the relationship is established between two
// segmented arrays until the relationship is dissolved, we maintain
// the invariant that an index is a valid subscript for one of the
// arrays if and only if it is a valid subscript for the other one.
//
// This invariant is maintained only "top-down"; if a
// RELATED_SEGMENTED_ARRAY is a registered kid of another
// RELATED_SEGMENTED_ARRAY, the behavior is undefined if the kid is
// resized by some operation other than the automatic resizing that
// takes place when its parent gets resized.
//
// To accomplish all this in a clean way, we apparently need some
// dirty C++: inheritance!
//
// RELATED_SEGMENTED_ARRAYs are used in the compiler when we want to
// maintain phase-specific tables alongside long-lived tables (usually
// as part of the morass of tables we call the "symbol table"). For
// example, the PREG_TAB lives throughout the compiler (front end
// through code generator), and contains no representation of home
// locations. In the back end, when we assign a home location to a
// PREG, we store the home location information in a secondary table
// of type BE_PREG_TAB. When we begin compiling a PU in the back end,
// we register the BE_PREG_TAB with the PREG_TAB, establishing that
// the PREG_TAB is primary and the BE_PREG_TAB is secondary in that
// relationship. Then we are assured that the BE_PREG_TAB can be
// indexed by exactly those indices that we use for the PREG_TAB. At
// the beginning of the back end's work on each PU, the BE_PREG_TAB is
// registered as a kid of the PREG_TAB using the routine
// growing_table::Register; this registration is the beginning of the
// relationship during which the secondary table (BE_PREG_TAB) is kept
// in sync with all grow/shrink operations that affect the primary
// table (PREG_TAB). The relationship ends when
// growing_table::Un_register is called to dissociate the pair.
//
// A more complicated usage example arises when we want to maintain
// some table in parallel with the multi-layered Symbol_Table
// structure. For an example of how that's done in the F90 front end,
// see mongoose/crayf90/sgi/cwh_stab.i, which declares an auxilliary
// table indexed by ST_IDX and called AUXST_TABLE.
//
// Note that the class T used in RELATED_SEGMENTED_ARRAY should have a constructor
// if it needs any initilization (i.e, be careful about RELATED_SEGMENTED_ARRAY<int> or
// RELATED_SEGMENTED_ARRAY<FOO *>).
//
// ======================================================================
// ======================================================================

// TODO: Add assertions.
//       Make block-growth more efficient.

class growing_table {
protected:
  UINT                  size;	// total number of elements present

private:
  typedef slist<growing_table *> kids_type;

  kids_type kids;	
  
  virtual void Construct_new_entry(void) = 0;
  virtual void Construct_new_entry(UINT n) = 0;
  virtual void Delete_last(void) = 0;
  virtual void Delete_last(UINT n) = 0;

protected:
  // Here each RELATED_SEGMENTED_ARRAY member function that changes
  // the size is represented by a member function. Note that we
  // can represent only primitives that are independent of the
  // segmented array's base type, since this base class isn't (and
  // can't be) a template.
  void Decrease_kids_size(void)
    {
      for (kids_type::iterator kid = kids.begin();
	   kid != kids.end();
	   ++kid) {
        (*kid)->Delete_last();
      }
    }

  void Decrease_kids_size(UINT n)
    {
      for (kids_type::iterator kid = kids.begin();
	   kid != kids.end();
	   ++kid) {
        (*kid)->Delete_last(n);
      }
    }

  void Increase_kids_size(void)
    {
      for (kids_type::iterator kid = kids.begin();
	   kid != kids.end();
	   ++kid) {
        (*kid)->Construct_new_entry();
      }
    }

  void Increase_kids_size(UINT n)
    {
      for (kids_type::iterator kid = kids.begin();
	   kid != kids.end();
	   ++kid) {
        (*kid)->Construct_new_entry(n);
      }
    }

public:

  // Here we have a member function to register a new kid with this
  // growing_table.
  void Register(growing_table &kid)
    {
      // Make sure the new kid is of the right size:
      FmtAssert(kid.size <= size,
		("growing_table::Register: child must not be larger "
		 "than parent"));
      // TODO: Cheesy, inefficient method for now.
      while (kid.size < size) {
	kid.Construct_new_entry();
      }

      // Put the new kid into the list of kids:
      kids.push_front(&kid);
    }

  void Un_register(growing_table &kid)
    {
      kids_type::iterator kid_ptr = find(kids.begin(), kids.end(), &kid);
      if (kid_ptr != kids.end()) {
        kids.erase(kid_ptr);
      }
      else {
	Fail_FmtAssertion("RELATED_SEGMENTED_ARRAY: Cannot un-register "
			  "an unregistered kid");
      }
    }
};

template <class T, UINT block_size = 128>
class RELATED_SEGMENTED_ARRAY : public growing_table {
private:
    typedef std::pair<T *, BOOL> thingy;
    std::vector<thingy, mempool_allocator<thingy> > map;
    MEM_POOL *pool;
    UINT max_size;			// total # of elements allocated
    INT block_base;			// idx of the beginning of
					// block (signed so we can set
					// to -1, meaning no block allocated)
    UINT next_block_size;		// size of block to be allocated
    T *block;				// points to the last block

private:
    
    typedef RELATED_SEGMENTED_ARRAY<T, block_size> self;

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

    virtual void Construct_new_entry(void)
      {
	if (size == max_size) Allocate();
	Increase_kids_size();
	new(&block[size++ - block_base]) T(); // T() makes sure the
					      // default constructor
					      // is called. T without
					      // parens would not
					      // ensure this.
      }

    virtual void Construct_new_entry(UINT n)
      {
	// Cheesy implementation for now to get things working without
	// the risk of having to think.
	for (; n > 0; n--) {
	  Construct_new_entry();
	}
      }

    UINT Round_up (UINT s) {
	UINT mask = block_size - 1;
	return (s + mask) & ~mask;
    }

    void Update_Map (T *marker, UINT new_size, BOOL own_memory);

    void Pop_Map ();

    // allocate a block, assume the array is completely filled
    void Allocate ();

    T& New_entry () {
	if (size == max_size) Allocate ();
	Increase_kids_size();
	return block[size++ - block_base];
    }
    
    // copy n elements to current buffer, assume no overflow
    void Copy (const T* x, UINT n) {
	std::copy(x, x + n, block + (size - block_base));
	size += n;
	Increase_kids_size(n);
    }

    // block_idx is an index into the map.  This function returns
    // the first map index that points to a different block. 
    UINT next_block_idx(UINT block_idx) const {
      for ( ; block_idx + 1 < map.size() &&
              map[block_idx].first + block_size == map[block_idx + 1].first;
            ++block_idx)
        {}
      return block_idx + 1;
    }

public:

  RELATED_SEGMENTED_ARRAY(MEM_POOL *m = Malloc_Mem_Pool) :
    pool (m), map(m) {
      size = max_size = next_block_size = 0;
      block_base = -1;
      block = 0;
  }

  ~RELATED_SEGMENTED_ARRAY() {
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

  UINT Size () const		{ return growing_table::size; }

  T& Entry (UINT idx) {
    Is_True (idx < size, ("Array subscript out of bound"));
    return map[idx / block_size].first[idx % block_size];
  }

  const T& Entry (UINT idx) const {
    Is_True (idx < size, ("Array subscript out of bound"));
    return map[idx / block_size].first[idx % block_size];
  }


  T& operator[] (UINT idx)             { return Entry(idx); }
  const T& operator[] (UINT idx) const { return Entry(idx); }

  iterator begin () {
    return iterator (this, map[0].first, Block_end (0), 0);
  }

  iterator end () {
    return iterator (this, block + (size - block_base),
		     block + (max_size - block_base), size);
  }

  const_iterator begin () const {
    return const_iterator (this, map[0].first, Block_end (0), 0);
  }

  const_iterator end () const {
    return const_iterator (this, block + (size - block_base),
			   block + (max_size - block_base), size);
  }
	
    T& New_entry (UINT& idx)	{ idx = size; return New_entry (); }

    UINT Insert (const T& x);

    virtual void Delete_last () {
      size--;
      Decrease_kids_size();
      if (size == block_base)
	Pop_Map ();
    }

    virtual void Delete_last (UINT n);

    // insert multiple elements, always copy to new buffer.
    UINT Insert (const T* x, UINT n_elemt);
    
    // similar to insert, except reuse the given buffer if possible
    UINT Transfer (T* x, UINT n_elemt);
    
    // Reserve extra storage.  Actual allocation will be done when the
    // already allocated storage is filled.
    void Reserve (UINT n_elemt)	{
	if (max_size - size + next_block_size < n_elemt)
	    next_block_size = n_elemt - (max_size - size);
    }

    // return the number of element till the end of the block
    UINT Get_block_size (UINT idx) const {
      UINT block_idx = idx / block_size;
      return std::min(next_block_idx(block_idx) * block_size, size) - idx;
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

    void Clear(void);
}; // RELATED_SEGMENTED_ARRAY

template <class T, UINT block_size>
inline void
RELATED_SEGMENTED_ARRAY<T,block_size>::Update_Map(T    *marker,
						  UINT  new_size,
						  BOOL  own_memory)
{
  do {
    map.push_back(pair<T*, BOOL>(marker, own_memory));
    new_size -= block_size;
    marker += block_size;
    own_memory = FALSE;         //Only the first entry can be freed for a block that
				//is larger than block_size. By: Jon Hsu, 11 May 2001.
  } while (new_size);
} // RELATED_SEGMENTED_ARRAY<T,block_size>::Update_Map


// deallocate a block and re-adjust the map and other variables
template <class T, UINT block_size>
void
RELATED_SEGMENTED_ARRAY<T,block_size>::Pop_Map ()
{
    next_block_size += max_size - block_base;
    MEM_POOL_FREE (pool, block);

    T *last_map_entry;
    do {
      last_map_entry = (map.end() - 1)->first;
      map.pop_back ();
    } while (last_map_entry != block);

    max_size = size;
    if (size > 0) {
      Is_True(size >= block_size,
	      ("RELATED_SEGMENTED_ARRAY: size in limbo"));
      block_base = size - block_size;
      UINT idx = block_base / block_size;
      block = map[idx].first;
      while (idx > 0 && map[idx - 1].first + block_size == block) {
	block = map[--idx].first;
	block_base -= block_size;
      }
    }
    else {
      Is_True(map.begin() == map.end(),
	      ("RELATED_SEGMENTED_ARRAY::Pop_Map: Map should be empty"));
      block_base = -1;
      block = NULL;
    }
} // RELATED_SEGMENTED_ARRAY<T,block_size>::Pop_Map


template <class T, UINT block_size>
void
RELATED_SEGMENTED_ARRAY<T,block_size>::Allocate ()
{
    Is_True (size == max_size, ("Invalid internal state in segmented array"));

    UINT new_size;

    if (next_block_size == 0)
	new_size = block_size;
    else {
	new_size = Round_up (next_block_size);
	next_block_size = 0;
    }

    block = (T *) MEM_POOL_Alloc (pool, new_size * sizeof(T));
    max_size += new_size;
    block_base = size;

    Update_Map (block, new_size, TRUE);
} // RELATED_SEGMENTED_ARRAY::Allocate


template <class T, UINT block_size>
void
RELATED_SEGMENTED_ARRAY<T,block_size>::Delete_last (UINT n)
{
  while (n >= size - block_base) {
    n -= size - block_base;
    size = block_base;
    Pop_Map ();
  }
  size -= n;
  Decrease_kids_size(n);
} // Delete_last


template <class T, UINT block_size>
inline UINT
RELATED_SEGMENTED_ARRAY<T,block_size>::Insert (const T& x)
{
    UINT idx = size;
    T &entry = New_entry ();

    entry = x;
    return idx;
} // RELATED_SEGMENTED_ARRAY::Insert


template <class T, UINT block_size>
UINT
RELATED_SEGMENTED_ARRAY<T,block_size>::Insert (const T* x, UINT n_elemt)
{
    UINT result = size;
    if (size + n_elemt <= max_size) {
	Copy (x, n_elemt);
	return result;
    }

    UINT space_left = max_size - size;
    Copy (x, space_left);
    n_elemt -= space_left;

    Reserve (n_elemt);
    Allocate ();
    Copy (x + space_left, n_elemt);

    return result;
} // RELATED_SEGMENTED_ARRAY::Insert


template <class T, UINT block_size>
UINT
RELATED_SEGMENTED_ARRAY<T,block_size>::Transfer (T* x, UINT n_elemt)
{
    UINT result = size;

    if (size + n_elemt <= max_size) {
	Copy (x, n_elemt);
	return result;
    }

    UINT space_left = max_size - size;
    if (space_left > 0) {
	Copy (x, space_left);
	n_elemt -= space_left;
	x += space_left;
    }

    if (n_elemt >= block_size) {
	UINT reused_size = n_elemt & ~(block_size - 1);
	block = x;
	Update_Map (block, reused_size, FALSE);
	block_base = size;
	size += reused_size;
	max_size += reused_size;
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
} // RELATED_SEGMENTED_ARRAY::Transfer

// Release all resources and return the RELATED_SEGMENTED_ARRAY to a
// pristine state.
template <class T, UINT block_size>
void
RELATED_SEGMENTED_ARRAY<T,block_size>::Clear(void)
{
  if (growing_table::size > 0) {
    Delete_last(growing_table::size);
  }
  Is_True(map.begin() == map.end(),
	  ("RELATED_SEGMENTED_ARRAY::Clear: Map should be empty"));
}

template <class T, UINT block_size, class OP>
inline void
For_all_entries (RELATED_SEGMENTED_ARRAY<T, block_size>& array,
                 const OP &op,
		 UINT32 first = 0)
{
    UINT last = array.Size ();

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
For_all_blocks (RELATED_SEGMENTED_ARRAY<T, block_size>& array, const OP &op)
{
    UINT max_size = array.Size ();
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
Find_entry_if (const RELATED_SEGMENTED_ARRAY<T, block_size>& array,
	       const PREDICATE& pred, UINT i = 0)
{
    UINT max_size = array.Size ();

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
Copy_array_range (const RELATED_SEGMENTED_ARRAY<T, block_size>& from_array,
		  RELATED_SEGMENTED_ARRAY<T, block_size>& to_array,
		  UINT32 first_idx = 0, UINT32 last_idx = (UINT32) -1)
{
    if (last_idx > from_array.Size ())
	last_idx = from_array.Size ();

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

/*
 * If LABEL.name_idx != 0. This may cause multi define in assembly
 */                                                                                                        

template <class T, UINT block_size>
void
Delete_array_item (const RELATED_SEGMENTED_ARRAY<T, block_size>& from_array,
		  RELATED_SEGMENTED_ARRAY<T, block_size>& to_array,
		  UINT32 first_idx = 0, UINT32 last_idx = (UINT32) -1)
{
  UINT32 index;
  index = to_array.Size() - from_array.Size();
 
  while ( index < to_array.Size()) {
	T* block = &to_array[index];
        if (block->name_idx !=0 )
                block->name_idx = 0;
        index ++;
  }  
}

#endif // cmplr_segmented_array_INCLUDED
