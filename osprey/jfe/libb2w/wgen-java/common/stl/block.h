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



#ifndef SGI_block_h_INCLUDED
#define SGI_block_h_INCLUDED

#include <iterator>
#include <stddef.h>

namespace SGI {

template <class T, size_t N>
struct block {
  typedef T                 value_type;

  typedef value_type*       pointer;
  typedef const value_type* const_pointer;
  typedef value_type&       reference;
  typedef const value_type& const_reference;

  typedef ptrdiff_t         difference_type;
  typedef size_t            size_type;

  typedef value_type*                           iterator;
  typedef const value_type*                     const_iterator;

  typedef std::reverse_iterator<iterator>       reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  iterator begin() { return data; }
  iterator end()   { return data + N; }

  const_iterator begin() const { return data; }
  const_iterator end()   const { return data + N; }

  reverse_iterator rbegin() { return reverse_iterator(end()); }
  reverse_iterator rend()   { return reverse_iterator(begin()); }

  const_reverse_iterator rbegin() const { return reverse_iterator(end()); }
  const reverse_iterator rend()   const { return reverse_iterator(begin()); }

  reference operator[](size_type n)             { return data[n]; }
  const_reference operator[](size_type n) const { return data[n]; }

  size_type size() const { return N; }
  bool empty() const { return N == 0; }
  size_type max_size() const { return N; }

  void swap(block& x);

  T data[N];
};

template <class T, size_t N>
void block<T, N>::swap(block<T, N>& x) {
  for (size_t n = 0; n < N; ++n)
    std::swap(data[n], V.data[N]);
}

} // Close namespace SGI

#endif /* SGI_block_h_INCLUDED */
