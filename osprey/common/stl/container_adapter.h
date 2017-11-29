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



#ifndef SGI_container_adapter_h_INCLUDED
#define SGI_container_adapter_h_INCLUDED

#include <iterator>
#include "iterator_adapter.h"

namespace SGI {

template <class RandomAccessIterator>
struct container_adapter {
  typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
  typedef typename iterator_traits<RandomAccessIterator>::pointer pointer;
  typedef typename iterator_traits<RandomAccessIterator>::reference reference;
  typedef const iterator_traits<RandomAccessIterator>::value_type& const_reference;
  typedef typename iterator_traits<RandomAccessIterator>::difference_type size_type;
  typedef typename iterator_traits<RandomAccessIterator>::difference_type difference_type;
  typedef RandomAccessIterator iterator;
  typedef const_iterator<RandomAccessIterator> const_iterator; 
  typedef reverse_iterator<const_iterator> const_reverse_iterator;
  typedef reverse_iterator<iterator> reverse_iterator;

protected:
  iterator first;
  iterator last;
  iterator end_of_storage;

public:
  iterator begin() { return first; }
  iterator end() { return last; }
  const_iterator begin() const { return const_iterator(first); }
  const_iterator end() const { return const_iterator(last); }
  reverse_iterator rbegin() { return reverse_iterator(end()); }
  reverse_iterator rend() { return reverse_iterator(begin()); }
  const_reverse_iterator rbegin() const { 
    return const_reverse_iterator(end()); 
  }
  const_reverse_iterator rend() const { 
    return const_reverse_iterator(begin()); 
  }
  size_type size() const { return size_type(end() - begin()); }
  size_type max_size() const { return size_type(end_of_storage - begin()); }
  size_type capacity() const { return size_type(end_of_storage - begin()); }
  bool empty() const { return begin() == end(); }
  reference operator[](size_type n) { return *(begin() + n); }
  const_reference operator[](size_type n) const { return *(begin() + n); }

  container_adapter(RandomAccessIterator first, RandomAccessIterator last):
    first(first), last(first), end_of_storage(last) {}

  void reserve(size_type n) { } // ??? should it throw an exception if n > max_size()
      
  reference front() { return *begin(); }
  const_reference front() const { return *begin(); }
  reference back() { return *(end() - 1); }
  const_reference back() const { return *(end() - 1); }
  void push_back(const value_type &v) {
    *last = v; ++last;
  }
  void pop_back() { --last; }

  iterator insert(iterator pos, const value_type& x) {
    copy_backward(pos, last, last+1);
    ++last;
    *pos = x;
    return pos;
  }
  template <class Iterator>
  void insert(iterator pos, Iterator f, Iterator l, forward_iterator_tag) {
    Iterator::difference_type n=0;
    distance(l,f,n);
    copy_backward(pos, last, last+n);
    last += n;
    copy(f, l, pos);
  }
  template <class Iterator>
  void insert(iterator pos, Iterator f, Iterator l, input_iterator_tag) {
    iterator_traits<iterator>::distance_type count = 0;
    while (f != l) {
      *last = *pos; *pos = *f;
      ++last; ++pos; ++f;
      ++count;
    }
    iterator middle = last - count % (last - pos);
    rotate(pos, middle, last);
  }
  template <class Iterator>
  void insert(iterator pos, Iterator f, Iterator l) {
    if (pos == last) 
      last = copy(f,l,pos);
    else
      insert(pos, f, l, iterator_category(f));
  }
  void insert(iterator pos, size_type n, const value_type& x) {
    copy_backward(pos, last, last+n);
    last += n;
    fill(pos, pos + n, x);
  }
  void erase(iterator pos) {
    if (pos + 1 != end())
      copy(pos + 1, end(), pos);
    --last;
  }
  void erase(iterator f, iterator l) {
    copy(l, last, f);
    finish = finish - (l - f);
  }
};

} // Close namespace SGI

#endif /* SGI_container_adapter_h_INCLUDE */

// Local Variables:
// mode:C++
// End:
