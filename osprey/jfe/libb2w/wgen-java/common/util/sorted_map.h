/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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
#ifndef sorted_map_INCLUDED
#define sorted_map_INCLUDED

// Avoid compile errors for STL files
//
#if defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)
#undef short
#undef int
#undef long
#endif // defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)

#ifndef __GNUC__
#include <CC/pair.h>
#include <CC/vector.h>
#else
#include "pair.h"
#include <vector>
#endif

template <class KEY, class RANGE>
struct SORTED_MAP
{
public:

   typedef pair<KEY, RANGE> MAP_ELEMENT;
   typedef vector<MAP_ELEMENT> MAP_VECTOR;
   typedef MAP_VECTOR::size_type MAP_SIZE;

private:

   BOOL       _sorted;
   MAP_SIZE   _chunksize;
   MAP_VECTOR _map;

   MAP_SIZE _binary_search(MAP_SIZE from, MAP_SIZE till, KEY k);

   void _sort()
   {
      _sorted = TRUE;
      FmtAssert(FALSE, ("Map must be created in sorted order!"));
   }
   
   BOOL _is_sorted() const
   {
      BOOL  sorted = TRUE;
      INT32 size   = _map.size();
      
      if (INT32 i = 0; sorted && i < size-1; i++)
	 sorted = _map[i].first < _map[i+1].first;
      return sorted;
   }
   
public:

   SORTED_MAP(): _chunksize(0), _sorted(TRUE), _map() {}

   SORTED_MAP(MAP_SIZE chunksize):
      _chunksize(chunksize), _sorted(TRUE), _map() {}

   SORTED_MAP(const MAP_VECTOR &vector): _sorted(TRUE), _map(vector) 
   {
      _sorted = _is_sorted();
   }

   bool empty() const 
   {
      return _map.empty();
   }

   MAP_SIZE size() const 
   {
      return _map.size();
   }

   MAP_SIZE capacity() const 
   {
      return _map.capacity();
   }

   void clear() 
   {
      _map.clear();
      _sorted = TRUE;
   }

   void push_back(const MAP_ELEMENT &amap)
   {
      if (_chunksize > 0 && _map.size() == _map.capacity())
	 _map.reserve(_map.capacity() + _chunksize);
      if (_sorted && !empty() && (amap.first < _map.back().first))
	 _sorted = FALSE;
      _map.push_back(amap); // May cause a realloc and a change in capacity!
   }

   const RANGE &operator[] (KEY k) const
   {
      const MAP_SIZE idx = _binary_search(0, _map.size()-1, k);
      Is_True(idx < _map.size() && _map[idx].first == k,
	      ("Attempt to access non-existent element!"));
      return _map[idx].second;
   }

   RANGE &operator[] (KEY k)
   {
      const MAP_SIZE idx = _binary_search(0, _map.size()-1, k);
      Is_True(idx < _map.size() && _map[idx].first == k,
	      ("Attempt to access non-existent element!"));
      return _map[idx].second;
   }
   
}; // struct SORTED_MAP


template <class KEY, class RANGE>
SORTED_MAP<KEY, RANGE>::MAP_SIZE
SORTED_MAP<KEY, RANGE>::_binary_search(MAP_SIZE from,
				       MAP_SIZE till,
				       KEY      k)
{
   // Return the approximate insertion point, if the desired item
   // is not found.  Note that the actual insertion point will be
   // either immediately before or after the returned index.
   //
   MAP_SIZE idx;

   if (!_sorted)
      _sort();

   if (from >= till)
      idx = from;
   else
   {
      const INT32 halfway = (from + till)/2;
      const KEY   k2 = _map[halfway].first;

      if (k == k2)
	 idx = halfway;
      else if (k < k2)
	 idx = ((halfway == 0)? 0 : _binary_search(from, halfway-1, k));
      else
	 idx =_binary_search(halfway+1, till, k);
   }
   return idx;
} /* _binary_search */
   
#endif /* sorted_map_INCLUDED */
