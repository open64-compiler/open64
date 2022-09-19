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



#ifndef SGI_container_with_membership_h_INCLUDED
#define SGI_container_with_membership_h_INCLUDED

#include <function.h>
#include <vector>
#include <iterator.h>
#include "misc_extension.h"

namespace SGI {

/*
  It keeps at most one value per index in the priority
  queue; push adjusts the value if its less than existing
  value for the given index. Otherwise it pushes it in the priority queue 
  if it never was in the queue and ignores if it was popped out of the
  queue.

  index == 0  ==> never in the container
  index == 1  ==> popped out of the container
  index == n where n > 1,  the container location is (n-2).
  
  */

template <class Value, 
          class IndexFunction = converter<Value, int>, 
          class Compare = less<Value> > 
class heap_with_membership {
public:
  typedef heap_with_membership<Value, IndexFunction, Compare> self;
  typedef typename IndexFunction::result_type index_type;
  typedef Value value_type;
  typedef typename vector<Value>::size_type size_type;
  typedef typename vector<index_type>::size_type index_size_type;
  typedef push_iterator<self> insert_iterator;
protected:
  typedef typename vector<Value>::iterator heap_iterator;
  vector<Value> heap;
  vector<index_type> index;
  IndexFunction i_fun;
  Compare comp;

  void assign(heap_iterator to,  const Value& from) {
    *to = from;
    index[i_fun(from)] = index_type((to - heap.begin()) + 2);
  }
  void sift_up(heap_iterator current, Value value) {
    heap_iterator first = heap.begin();
    size_type hole_index = current - first; 
    size_type parent = (hole_index - 1) >> 1;
    while (hole_index > 0 && comp(*(first + parent), value)) {
      assign(first + hole_index, *(first + parent));
      hole_index = parent;
      parent = (hole_index - 1) >> 1;
    }
    assign(first + hole_index, value);
  }
  void fix_hole(size_type hole_index) { 
    heap_iterator first = heap.begin();
    size_type len = heap.size() - 1;
    size_type second_child = (hole_index + 1) << 1;
    while (second_child < len) {
      if (comp(*(first + second_child), *(first + (second_child - 1))))
	--second_child;
      assign( first + hole_index, *(first + second_child));
      hole_index = second_child;
      second_child = (second_child + 1) << 1;
    }
    if (second_child == len) {
      assign(first + hole_index, *(first + (second_child - 1)));
      hole_index = second_child - 1;
    }
    sift_up(first + hole_index, heap.back());
    heap.pop_back();
  }
    
public:
  heap_with_membership(size_type n) : index(n, index_type(0)) {}
  heap_with_membership(size_type n, Compare c) 
    : index(n, index_type(0)), comp(c) {}
  heap_with_membership(size_type n, IndexFunction i_f) 
    : index(n, index_type(0)), i_fun(i_f) {}
  heap_with_membership(size_type n, IndexFunction i_f, Compare c) 
    : index(n, index_type(0)),  comp(c), i_fun(i_f) {}

  bool empty() const { return heap.empty(); }
  size_type size() const { return heap.size(); }
  const Value& top() const { return *heap.begin(); }
  void push(const Value& x) {
    size_type i_x = i_fun(x);
    index_type ii_x = index[i_x];
    if (ii_x == 0) {
      heap.push_back(x);
      sift_up(heap.end()-1, x);
    } else if (ii_x != 1) {
      heap_iterator p_x = heap.begin() + size_type(ii_x - 2);
      if (comp(x, *p_x)) {
	sift_up(p_x, x);
      }
    }
  }
  void pop() { 
    index[i_fun(heap.front())] = 1;
    fix_hole(size_type(0));
  }
  void remove(index_type i) { 
    size_type tmp = index[i];
    if (0 == tmp) index[i] = 1;
    if (1 < tmp) {
      fix_hole(size_type(tmp - 2)); 
      index[i] = 2;
    }
  }
  int status(index_type i) {
    int tmp = index[i];
    if (0 == tmp) return 0;
    if (1 == tmp) return - 1;
    return 1;
  }
  const Value& operator[](index_size_type n) { return heap[index[n] - 2]; }
  // operator[] is not defined if status(n) <= 0
};

} // Close namespace SGI

#endif /* SGI_container_with_membership_h_INCLUDE */

// Local Variables:
// mode:C++
// End:
