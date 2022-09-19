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


//-*-c++-*-
// A class to support sparse maps based on ID's. An example would be a
// mapping from the CODEREP ID space to EXP_OCCURS pointers. Indeed
// the need for this example was the original motivation for creating
// this class.
//
// Original author: Robert Kennedy
//
// Interface description:
//     All of the following are member functions of
//     template <class NODE_TYPE, class KEY_TYPE> class ID_MAP.
//     NODE_TYPE can be anything; often it is a pointer to some type
//        of node in a data structure.
//     KEY_TYPE  must be something that can participate in integer
//        arithmetic. This restriction could easily be lifted by
//        expanding the implementation of the member function Hash().
//
// ID_MAP<NODE_TYPE, KEY_TYPE> id_map(UINT32     initial_tbl_capacity,
//                                    NODE_TYPE  not_found_value,
//                                    MEM_POOL  *pool,
//                                    BOOL       tracing)
//     (constructor) The parameters to the constructor specify
//     respectively an estimate of the number of entries that will be
//     made in the mapping, the value that should be returned by a
//     failed table-lookup operation, the memory pool from which to
//     allocate space for the mapping, and a tracing flag.
//
// NODE_TYPE Lookup(KEY_TYPE key) const
//     Returns not_found_value (specified in the constructor call) if
//     *this does not contain an entry for key; otherwise returns the
//     entry for key.
//
// void       Insert(KEY_TYPE key, NODE_TYPE node)
//     Adds an entry to the mapping for the given (key, node)
//     pair. Assumes that the mapping does not already contain an
//     entry for the given key.
//
// void       Print(FILE *fp) const
//     Prints detailed, implementation-dependent information about the
//     mapping for debugging purposes.
//
//
// Implementation description:
//     This class is implemented as a hash table using an in-place
//     scheme for resolving collisions. In order to support deletions
//     from the table in a coherent way, we maintain the invariant
//     that the entries in a collision list all share the same hash
//     value, and each hash value corresponds to at most one collision
//     list. The hash table is a dynamically-allocated array of
//     template class ID_MAP_HASH_ENTRY. When the table expands beyond
//     a load factor defined by the private member function
//     Capacity(), it is grown (using OPT_POOL_Realloc). Growing the
//     table necessitates a full rebuild, because the hash function
//     (member function Hash()) obviously should depend on the size of
//     the table. The private member function Enlarge() implements the
//     rebuild process. Because Enlarge() can take quite a bit of time
//     in passing over the entire table, it can be very important to
//     pass a good initial size to the constructor when the ID_MAP is
//     created.
//
//     The implementation maintains a doubly-linked list by table
//     index of free positions in the table; when an entry is
//     required, it is unhooked from this list and used. The head of
//     the free list is the private member _free_list. The special
//     value (-1) denotes the end of a linked list (see the _next and
//     _prev fields in the template class ID_MAP_HASH_ENTRY).
//
//     To perform a lookup operation in the hash table (member
//     function Lookup()), we hash the given key value (member
//     function Hash()) to get a hash value H. We begin a linear
//     search for an ID_MAP_HASH_ENTRY at the table entry _table[H],
//     and proceed by following the _next fields until we find either
//     the end of the collision list (denoted by a _next field
//     containing -1) or an entry containing the requested key.
//
//     When we add an entry (member function Insert()), we grow the
//     table if necessary, and then hash the given key to get a table
//     index H. If _table[H] is not free (a condition denoted by a
//     non-not_found_value _node field), we displace the entry from
//     _table[H] into an entry allocated from the free list. If there
//     is a displaced entry due to the insertion, we maintain the
//     invariant that each nonempty collision list contains keys that
//     hash to exactly one location in the following way: If the
//     displaced entry hashes to a different location from the
//     inserted entry, we traverse the collision list corresponding to
//     the displaced entry, and route the appropriate _next index to
//     the displaced entry's new location. If the displaced entry
//     hashes to the same location as the inserted entry, we simply
//     insert the new entry at the head of the collision list. We
//     insert the given (key, node) pair into _table[H], and chain up
//     _table[H].next to the entry we displaced, if any.
//
//     The most complicated part of the implementation is the part
//     that rehashes the entire table in place when the table
//     grows. This happens in the private member function
//     Enlarge(). If you can think of a simpler way to do an in-place
//     rehash of this sort of table, please come talk to me (Robert
//     Kennedy). The present implementation works by determining the
//     (necessarily large enough) set of table entries that no element
//     currently in the table will hash to, and using that set of
//     entries as auxilliary storage during the global rehash
//     operation. Any scheme that doesn't do something like this
//     appears doomed to have old (pre-growth) structures mingling
//     fatally with new (post-growth, post-rehash) structures in the
//     table.
//


#ifndef id_map_INCLUDED
#define id_map_INCLUDED "id_map.h"

#include "defs.h"
#include "cxx_template.h"
#include "tracing.h"
#include "opt_defs.h"
#include "erglob.h"      // For EC_xxx error codes

namespace idmap {

// Bring in declaration of floor and ceil from /usr/include/math.h
// Can't include math.h directly because of defs.h!

#if defined(_LP64) && defined(_MATH_H)
#else
extern "C" {
extern double   floor(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (floor)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double   ceil(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (ceil)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */
}
#endif /* defined(_LP64) && defined(_MATH_H) */

#define MIN_TABLE_SIZE 16

#define CAPACITY_FACTOR 0.75

// GROWTH_FACTOR must be at least as large as 2.0 * CAPACITY_FACTOR
// for correctness!
#define GROWTH_FACTOR   2.0

template <class NODE_TYPE, class KEY_TYPE> class ID_MAP;

template <class NODE_TYPE, class KEY_TYPE>
class ID_MAP_HASH_ENTRY {
friend class ID_MAP<NODE_TYPE, KEY_TYPE>;
  NODE_TYPE  _node;	// _not_found_value for entries in the free list
  union {
    KEY_TYPE _key;	// For entries in use
    mINT32   _prev;	// For entries in the free list
  };
  mINT32     _next;	// Next in the collision list or the free list

#ifdef DEBUG_ID_MAP
  enum {
    IMHE_UNKNOWN,
    IMHE_FREE,
    IMHE_USED
  } _state;
#endif // DEBUG_ID_MAP
};

template <class NODE_TYPE, class KEY_TYPE>
class ID_MAP {
private:
  NODE_TYPE                                     _not_found_value;
  BOOL                                          _constructed;
  const BOOL                                    _tracing;
  const BOOL                                    _verbose;
  MEM_POOL                               *const _pool;
  ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE> *      _table;

  // If (_table == NULL), then _table_size indicates the desired
  // minimum table size that was set at constructor time, and that is
  // used by Init() to guide allocation of the _table.
  // If (_table != NULL), then _table_size indicates the true size of
  // the _table that's allocated to this ID_MAP.
  UINT32                                        _table_size;
  UINT32                                        _num_entries;
  mINT32                                        _free_list;

  // How many elements are we willing to tolerate in a table of a
  // given size before reallocating?
  static UINT32 Capacity(UINT32 size)
    {
      return (UINT32) floor((double) size * CAPACITY_FACTOR);
    }

  // Given a number of elements, how big a table should we allocate to
  // hold them?
  static UINT32 Size(UINT32 capacity)
    {
      return (UINT32) ceil((double) capacity / CAPACITY_FACTOR);
    }

  void Alloc_table_space(UINT32 table_size);
  void Initialize_table(void);

  mINT32 Hash(const KEY_TYPE key, const UINT32 tbl_size) const
    {
      // First part: convert key into a 32-bit key
      UINT32 key_uint32;
      if (sizeof(key) == sizeof(UINT32)) {
	key_uint32 = *((const UINT32 *) &key);
      }
      else if (sizeof(key) < sizeof(UINT32)) {
	key_uint32 = (*((const UINT32 *) &key) &
		      ((1 << ((sizeof(key) & (sizeof(UINT32)-1)) * 8)) - 1)); 
	// added  "& (sizeof(UINT32)-1)" to suppress shift count warning
	// Performance should not be affected because any reasonable
	// compiler can simpilfy ((sizeof(key) & 0x3) * 8) into a
	// constant.  -Raymond  5/27/98.
      }
      else {
	const UINT32 *p = (const UINT32 *) &key;
	INT i;
	key_uint32 = 0;
	for (i = 0; i < (sizeof(key) / sizeof(UINT32)); i++) {
	  key_uint32 = (key_uint32 << 19) + (key_uint32 >> 13);  // rotate left 19 bits
	  key_uint32 ^= *p++;
	}
      }
      
      // Second part: hashing the 32-bit key
      const UINT64 multiplier = 2654435769LL; // 2^32 * (sqrt(5) - 1) / 2

      mINT32 retval = (tbl_size * ((key_uint32 * multiplier) %
				   0x100000000LL)) >> 32;
      return retval;
    }

  void Add_to_free_list(const mINT32 idx)
    {
      Is_Trace(_tracing && _verbose,
	       (TFile, "ID_MAP::Add_to_free_list(%d)\n", idx));

      if (_free_list != -1) {
	_table[_free_list]._prev = idx;
      }

      _table[idx]._next = _free_list;
      _table[idx]._node = _not_found_value;

      Is_True(Check(_free_list != idx),
	      ("ID_MAP::Add_to_free_list: Must not introduce loop"));

      _free_list = idx;

    }

  void Enlarge(void);

  void Remove_from_free_list(const mINT32 idx)
    {
      Is_Trace(_tracing && _verbose,
	       (TFile, "ID_MAP::Remove_from_free_list(%d)\n", idx));
      Is_True(Check(_table[idx]._node == _not_found_value),
	      ("ID_MAP::Remove_from_free_list: Node must be free: %ld", idx));

      if (_free_list == idx) {
	_free_list = _table[idx]._next;
	Is_True(Check(_free_list != idx),
		("ID_MAP::Remove_from_free_list: Inconsistent free list: %lu",
		 _free_list));
	Is_True(Check(_table[idx]._node == _not_found_value),
		("ID_MAP::Remove_from_free_list: Node must be free: %ld", idx));
      }
      else {
	Is_True(Check(_table[idx]._prev != -1),
		("ID_MAP::Remove_from_free_list: Inconsistent free list"));
	Is_True(Check(_table[idx]._next != idx),
		("ID_MAP::Remove_from_free_list: Loop in free list : %ld",
		 idx));
	_table[_table[idx]._prev]._next = _table[idx]._next;
      }
      if (_table[idx]._next != -1) {
	_table[_table[idx]._next]._prev = _table[idx]._prev;
	_table[idx]._next = -1;
      }
    }

  mINT32 Alloc_from_free_list(void)
    {
      Is_Trace(_tracing && _verbose,
	       (TFile, "ID_MAP::Alloc_from_free_list()..."));
      Is_True(_free_list != -1,
	      ("ID_MAP::Displace: free list must be nonempty; %lu / %lu",
	       _num_entries, _table_size));
      mINT32 idx = _free_list;

      Is_True(_table[_free_list]._node == _not_found_value,
	      ("ID_MAP::Alloc_from_free_list: free list inconsistency"));

      _free_list = _table[_free_list]._next;

      Is_True(Check(idx != _free_list),
	      ("ID_MAP::Alloc_from_free_list: loop in free list"));

      Is_True(_table[_free_list]._node == _not_found_value,
	      ("ID_MAP:Alloc_from_free_list: free list inconsistency"));

      Is_Trace(_tracing && _verbose,
	       (TFile, " returning %d\n", idx));
      return idx;
    }

  BOOL Check(BOOL cond) const
    {
      if (!cond) 
	Print(TFile);
      return cond;
    }

  void Verify(void) const
    {
#ifdef DEBUG_ID_MAP
      for (mINT32 idx = 0; idx < _table_size; idx++) {
	_table[idx]._state =
	  ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>::IMHE_UNKNOWN;
      }
      for (idx = _free_list; idx != -1; idx = _table[idx]._next) {
	Is_True(Check(_table[idx]._state ==
		      ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>::IMHE_UNKNOWN),
		("ID_MAP::Verify: Loop in free list"));
	_table[idx]._state =
	  ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>::IMHE_FREE;
      }
      for (idx = 0; idx < _table_size; idx++) {
	if (_table[idx]._state !=
	    ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>::IMHE_FREE) {
	  // Don't check entries that are in transition from free to
	  // used.
	  Is_True(Check(idx == Entry_lookup(_table[idx]._key)),
		  ("ID_MAP::Verify: Dangling node: idx == %ld", idx));
	  _table[idx]._state =
	    ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>::IMHE_USED;
	}
      }
#endif // DEBUG_ID_MAP
    }

  mINT32 Entry_lookup(KEY_TYPE) const;

public:
  ID_MAP(UINT32     initial_capacity,
	 NODE_TYPE  not_found_value,
	 MEM_POOL  *pool,
	 BOOL       tracing);
  ~ID_MAP(void);

  void       Init(void);
  void       Init(UINT32);  // init with overriding table size

  NODE_TYPE  Lookup(KEY_TYPE) const;

  void Insert(KEY_TYPE, NODE_TYPE);

  void Delete(KEY_TYPE);

  void Print(FILE *) const;
};


// here starts templatized function declarations

template <class KEY_TYPE>
UINT64 Key_as_llu(const KEY_TYPE k)
{
   UINT64 llu;
   
   switch (sizeof(k))
   {
   case 1:
      llu = *(UINT8 *)&k;
      break;
   case 2:
      llu = *(UINT16 *)&k;
      break;
   case 4:
      llu = *(UINT32 *)&k;
      break;
   default:
      llu = *(UINT64 *)&k;
      break;
   }
   return llu;
} // Key_as_llu

      

// The constructor only squirrels away the information required to
// build the object. The object is built only by the Init() function,
// which must be called separately. We implement things this way so we
// can avoid building the object as an automatic variable when we know
// it won't be used.

template <class NODE_TYPE, class KEY_TYPE>
ID_MAP<NODE_TYPE, KEY_TYPE>::ID_MAP(const UINT32          init_capacity,
				    const NODE_TYPE       not_found_value,
				          MEM_POOL *const pool,
				    const BOOL            tracing) :
				 _not_found_value(not_found_value),
				 _pool(pool),
				 _tracing(tracing),
				 _verbose(FALSE),	// change this?
				 _num_entries(0),
				 _constructed(FALSE)
{
  _table      = NULL;
  _table_size = Size(init_capacity);
}

template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Alloc_table_space(UINT32 table_size)
{
  if (_table == NULL) {
    if (table_size < MIN_TABLE_SIZE) {
      table_size = MIN_TABLE_SIZE;
    }
    _table_size = table_size;
    // First allocation of space for _table.
    _table = (ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE> *)
      MEM_POOL_Alloc(_pool,
		     (table_size *
		      sizeof(ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>)));
  }
  else {
    // This ID_MAP is being re-initialized. Use the existing _table if
    // it's big enough, otherwise realloc to the required size.
    if (_table_size < table_size) {
      _table = (ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE> *)
	MEM_POOL_Realloc(_pool,
			 _table,
			 _table_size *
			 sizeof(ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>),
			 table_size *
			 sizeof(ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>));
      _table_size = table_size;
    }
  }

  if (_table == NULL) ErrMsg(EC_No_Mem, "ID_MAP::ID_MAP");

  Is_Trace(_tracing,
	   (TFile, "ID_MAP::ID_MAP: allocated %u entries\n",
	    _table_size));
}

template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Initialize_table(void)
{
  // Put the entire table into the free list.
  _free_list = 0;
  for (mINT32 i = 0; i < _table_size; i++) {
    _table[i]._node = _not_found_value;
    _table[i]._prev = i - 1;
    _table[i]._next = i + 1;
  }
  _table[_table_size - 1]._next = -1;
}

template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Init(UINT32 capacity)
{
  Alloc_table_space(Size(capacity));
  Initialize_table();
}

template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Init(void)
{
  _constructed = TRUE;

  Alloc_table_space(_table_size);
  Initialize_table();
}

template <class X>
inline void Id_map_fprint(FILE *fp, X *x)
{
  x->Print(fp);
}

inline void Id_map_fprint(FILE *fp, IDTYPE *x)
{
  fprintf(fp, "%d\n", *x);
}

inline void Id_map_fprint(FILE *fp, INT x)
{
  fprintf(fp, "%d\n", x);
}

template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Print(FILE *fp) const
{
  fprintf(fp, "Number of entries: %u\n", _num_entries);
  fprintf(fp, "Free list --> %d\n", _free_list);

  for (mINT32 i = 0; i < _table_size; i++) {
    fprintf(fp, "ID_MAP table[%d] : ", i);
    if (_table[i]._node != _not_found_value) {
      fprintf(fp, "[H(%llu)=%d; %d -->] ",
	      Key_as_llu(_table[i]._key),
	      Hash(_table[i]._key, _table_size),
	      _table[i]._next);
      Id_map_fprint(fp, _table[i]._node);
    }
    else {
      fprintf(fp, "<-- %d, 0x%lx, %d -->\n",
	      _table[i]._prev, (INTPTR) _table[i]._node, _table[i]._next);
    }
  }
}

template <class NODE_TYPE, class KEY_TYPE>
ID_MAP<NODE_TYPE, KEY_TYPE>::~ID_MAP(void)
{
  if (_constructed) {
    Verify();
    if (_tracing) {
      // dump out the table before destructing it.
      Is_Trace(TRUE, (TFile, "--- ID_MAP pre-destruct table dump:\n"));
      Print(TFile);
      // Really should call the destructor on the table elements here,
      // and then free the table memory.
    }
    _constructed = FALSE;
  }
}

template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Enlarge(void)
{
  // Enlarge the hash table; here's how we do it:
  // 1. Realloc the table;
  // 2. Decide which nodes in the table will be occupied by entries
  //    (by marking those whose indices are hash values and building
  //     up a free list containing an arbitrary set of entries to be
  //     occupied; the number of used hash values plus the length of
  //     this free list will be the number of entries.)
  // 3. Copy the table's node/key pairs into the set of nodes that
  //    won't be occupied; the enlargement factor must be great enough
  //    that we're guaranteed enough space to do this. Two is always
  //    enough.
  // 4. For each node/key pair in a to-be-unoccupied entry, rehash it,
  //    displacing into a free list node if necessary, and
  //    simultaneously rebuild the free list to contain all unoccupied
  //    entries.
  mINT32 first_new_idx = _table_size;
  UINT32 save_num_entries = _num_entries;

  mINT32 i;	// general-purpose running index

  Is_Trace(_tracing, (TFile, "ID_MAP enlarging from size %d\n",
		      _table_size));
  Is_Trace_cmd(_tracing && _verbose, Print(TFile));

  UINT32 new_table_size = (UINT32) ceil(GROWTH_FACTOR * (double) _table_size);

  _table = (ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE> *)
    MEM_POOL_Realloc(_pool,
		     _table,
		     _table_size *
		     sizeof(ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>),
		     new_table_size *
		     sizeof(ID_MAP_HASH_ENTRY<NODE_TYPE, KEY_TYPE>));

  if (_table == NULL) ErrMsg(EC_No_Mem, "ID_MAP::Enlarge");

  _table_size = new_table_size;

  // Use the _next field of each entry to say whether the entry will
  // be occupied or not. Unoccupied <--> -1.
  for (i = _table_size - 1; i >= first_new_idx; i--) {
    _table[i]._next = -1;
    _table[i]._node = _not_found_value;
  }

  for (; i >= 0; i--) {
    _table[i]._next = -1;
  }

  // Use save_num_entries to count the number of distinct hash values
  // represented.
  for (i = 0; i < first_new_idx; i++) {
    if (_table[i]._node != _not_found_value) {
      mINT32 h = Hash(_table[i]._key, _table_size);
      if (_table[h]._next == -1) {
	Is_True(Check(save_num_entries != 0),
		("ID_MAP::Enlarge: Bad save_num_entries logic"));
	--save_num_entries;
	_table[h]._next = 0;
      }
    }
  }

  // Now grab an occupied for every hash collision. After the
  // following loop, there will be exactly one instance of _next == 0
  // for each entry in the table.
  for (i = 0; save_num_entries > 0; i++) {
    Is_True(i < _table_size,
	    ("ID_MAP::Enlarge: Bad logic"));
    if (_table[i]._next == -1) {
      Is_True(Check(save_num_entries != 0),
	      ("ID_MAP::Enlarge: Bad save_num_entries logic"));
      --save_num_entries;
      _table[i]._next = 0;
    }
  }

  // Find the head of the unoccupied list.
  i = _table_size;
  mINT32 unocc_head = -1;

  while (TRUE) {
    --i;
    if (_table[i]._next == -1) {
      unocc_head = i;
      break;
    }
    Is_True(i > 0, ("ID_MAP::Enlarge: Bad logic"));
  }

  // Put all the entries destined to be unoccupied into the unoccupied
  // list.
  while (i > 0) {
    --i;
    if (_table[i]._next == -1) {
      _table[i]._next = -unocc_head - 2;
      unocc_head = i;
    }
  }

  // Find the first to-be-unoccupied place that isn't already holding
  // a pair.
  mINT32 unocc = unocc_head;
  while (_table[unocc]._node != _not_found_value) {
    Is_True(0 <= unocc && unocc < _table_size,
	    ("ID_MAP::Enlarge: Bad logic"));
    unocc = -_table[unocc]._next - 2;
  }

  _free_list = -1;

  // Now move all node/key pairs into to-be-unoccupied entries and put
  // all to-be-occupied entries into the free list.
  for (i = _table_size - 1; i >= 0; i--) {
    if (_table[i]._next == 0) {
      // This entry will be occupied.
      if (_table[i]._node != _not_found_value) {
	FmtAssert(unocc != -1,
		  ("ID_MAP::Enlarge: Insufficient unoccupied entries.\n"
		   "                 GROWTH_FACTOR too small WRT "
		   "CAPACITY_FACTOR"));
	Is_True(Check(0 <= unocc && unocc < _table_size &&
		      _table[unocc]._node == _not_found_value),
		("ID_MAP::Enlarge: Bad logic"));
	_table[unocc]._node = _table[i]._node;
	_table[unocc]._key  = _table[i]._key;
	do {
	  Is_True(Check(0 <= unocc && unocc < _table_size),
		  ("ID_MAP::Enlarge: Bad logic"));
	  unocc = -_table[unocc]._next - 2;
	} while (_table[unocc]._node != _not_found_value);
      }
      Add_to_free_list(i);
    }
  }

  save_num_entries = _num_entries;

  // Now go through the node/key pairs stored in the unoccupied list
  // and for every pair we find, add its entry to the free list and
  // rehash the pair.
  for (i = unocc_head; i != -1; i = unocc) {
    KEY_TYPE  key  = _table[i]._key;
    NODE_TYPE node = _table[i]._node;

    unocc = -_table[i]._next - 2;
    // There is much room for performance enhancement in the
    // following five lines!
    Add_to_free_list(i);

    if (node != _not_found_value) {
      // Avoid recursive enlargement of the table!
      _num_entries = 0;
      Insert(key, node);
    }
  }

  _num_entries = save_num_entries;

  Is_Trace(_tracing, (TFile, "ID_MAP::Enlarge: Starting verification\n"));
  Verify();
  Is_Trace(_tracing, (TFile, "ID_MAP::Enlarge: Verification passed\n"));
}

template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Insert(const KEY_TYPE  key,
				    const NODE_TYPE node)
{
  Is_Trace(_tracing && _verbose,
	   (TFile, "ID_MAP::Insert(%llu, ...) ...\n", Key_as_llu(key)));

  if (_num_entries + 1 > Capacity(_table_size)) {
    Is_Trace(_tracing,
	     (TFile, "Enlarging: will have %u entries\n"
	      "           table size: %u\n"
	      "       table capacity: %u\n",
	      _num_entries + 1, _table_size,
	      Capacity(_table_size)));
    Enlarge();
    Is_True(_num_entries <= Capacity(_table_size),
	    ("ID_MAP::Insert: Enlarge didn't get enough capacity"));
  }
  else {
    Is_Trace(_tracing && _verbose,
	     (TFile, "Not enlarging: Capacity(%u) = %u\n",
	      _table_size, Capacity(_table_size)));
  }

  mINT32 idx = Hash(key, _table_size);

  Is_Trace(_tracing && _verbose,
	   (TFile, "     ID_MAP::Insert: inserting at %d\n", idx));

  if (_table[idx]._node != _not_found_value) {
    Is_True(Check(_table[idx]._next != _free_list),
	    ("ID_MAP::Insert: inconsistent relocation"));

    mINT32 displaced = Alloc_from_free_list();

    Is_Trace(_tracing && _verbose,
	     (TFile, "    displacing [H(%llu)=%d; %d] to %d\n",
	      Key_as_llu(_table[idx]._key),
	      Hash(_table[idx]._key, _table_size),
	      _table[idx]._next, displaced));

    _table[displaced]._node = _table[idx]._node;
    _table[displaced]._key  = _table[idx]._key;

    Is_True(Check(_table[idx]._next != displaced),
	    ("ID_MAP::Insert: inconsistent relocation"));

    _table[displaced]._next = _table[idx]._next;

    Is_True(Check(idx != displaced),
	    ("ID_MAP::Insert: inconsistent relocation"));
    Is_True(Check(displaced != _free_list),
	    ("ID_MAP::Insert: Bug in Alloc_from_free_list"));

    mINT32 temp_idx = Hash(_table[displaced]._key, _table_size);
    if (idx == temp_idx) {
      // same hash bucket
      _table[idx]._next = displaced;
    }
    else {
      _table[idx]._next = -1;

      // Look for the dangling link to the displaced item
      while (temp_idx != -1 && _table[temp_idx]._next != idx) {
	temp_idx = _table[temp_idx]._next;
      }

#if Is_True_On
      if (temp_idx == -1 || _table[temp_idx]._next != idx) {
	fprintf(TFile, "Insert %llu at idx = %d (%d)\n",
		Key_as_llu(key), idx,
		Hash(key, _table_size));
	fprintf(TFile, " -> displaced = %d\n", displaced);
	fprintf(TFile, "start [H(%llu)=%d]\n", 
		Key_as_llu(_table[displaced]._key),
		Hash(_table[displaced]._key, _table_size));
      }
#endif
      Is_True(Check(temp_idx != -1 && _table[temp_idx]._next == idx),
	      ("ID_MAP::Insert: displaced item not found in hash table."));
      FmtAssert(temp_idx != -1 && _table[temp_idx]._next == idx,
		("ID_MAP::Insert: displaced item not found in hash table."));

      _table[temp_idx]._next = displaced;
    }
  }
  else {
    Remove_from_free_list(idx);
    _table[idx]._next = -1;
  }

  _table[idx]._node = node;
  _table[idx]._key  = key;
  ++_num_entries;
}


template <class NODE_TYPE, class KEY_TYPE> void
ID_MAP<NODE_TYPE, KEY_TYPE>::Delete(const KEY_TYPE key)
{
  mINT32 idx = Hash(key, _table_size);
  mINT32 prev_idx = -1;

  while (idx != -1 &&
	 _table[idx]._node != _not_found_value &&
	 _table[idx]._key != key) {
    prev_idx = idx;
    idx = _table[idx]._next;
  }

  FmtAssert(idx != -1 && _table[idx]._node != _not_found_value,
	    ("ID_MAP::Delete: item not found in hash table."));

  if (prev_idx != -1) {
    _table[prev_idx]._next = _table[idx]._next;
  }
  else {
    mINT32 next_idx = _table[idx]._next;
    if (next_idx != -1) {
      _table[idx]._node = _table[next_idx]._node;
      _table[idx]._key  = _table[next_idx]._key;
      _table[idx]._next = _table[next_idx]._next;
      idx = next_idx;
    }
  }
  Add_to_free_list(idx);
  --_num_entries;
}


template <class NODE_TYPE, class KEY_TYPE> NODE_TYPE
ID_MAP<NODE_TYPE, KEY_TYPE>::Lookup(const KEY_TYPE key) const
{
  mINT32 idx = Entry_lookup(key);
  if (idx == -1) {
    return _not_found_value;
  }
  else {
    return _table[idx]._node;
  }
}

template <class NODE_TYPE, class KEY_TYPE> mINT32
ID_MAP<NODE_TYPE, KEY_TYPE>::Entry_lookup(const KEY_TYPE key) const
{
  mINT32 idx = Hash(key, _table_size);

#if Is_True_On
  mINT32 loopcheck = idx;
#endif

  while (idx != -1 &&
	 _table[idx]._node != _not_found_value &&
	 !(_table[idx]._key == key)) {
    idx = _table[idx]._next;

#if Is_True_On
    if (loopcheck != -1) {
      loopcheck = _table[loopcheck]._next;
    }
    if (loopcheck != -1) {
      loopcheck = _table[loopcheck]._next;
    }
    if (loopcheck != -1) {
      Is_True(Check(loopcheck != idx),
	      ("ID_MAP::Lookup: loop in hash bucket %lu", idx));
    }
#endif
  }
  if (idx == -1 || _table[idx]._node == _not_found_value) {
    return -1;
  }
  else {
    return idx;
  }
}

}

#endif
