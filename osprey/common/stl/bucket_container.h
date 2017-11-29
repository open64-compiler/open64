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



#ifndef SGI_bucket_container_h_INCLUDED
#define SGI_bucket_container_h_INCLUDED

#include <iterator.h>
#include <vector>
#include <algorithm>

namespace SGI {

/*
BucketContainer
   BucketContainer::value_type
   BucketContainer::size_type
       size_type size()
       bool empty()
       bucket_type bucket(size_type)
       size_type number_of_buckets()
   BucketContainer::bucket_type
      BucketContainer::bucket_type::size_type == size_type 
      BucketContainer::bucket_type::iterator 
          iterator_traits<bucket_type::iterator>::value_type == value_type
          iterator_traits<bucket_type::iterator>::size_type == size_type
	  iterator bucket.begin()
	  iterator bucket.end()
	  size_type size()
	  bool empty()
      BucketContainer::iterator 
          iterator_traits<iterator>::value_type == value_type
          iterator_traits<iterator>::size_type == size_type
          numa_traits<iterator>::type == true_type
      BucketContainer::fast_iterator 
          iterator_traits<fast_iterator>::value_type == value_type
          iterator_traits<fast_iterator>::size_type == size_type
      BucketContainer::bucket_iterator 
          iterator_traits<bucket_iterator>::value_type == bucket_type
          iterator_traits<bucket_iterator>::size_type == size_type
	  iterator_traits<bucket_iterator>::iterator_category == 
	       random_access_iterator_tag
*/
  

//  c is initialized with 0 and with the correct len.
template <class ForwardIterator, class IndexFunction, class Container>
inline void 
counts_in_buckets_unguarded(ForwardIterator first, ForwardIterator last, IndexFunction i_fun, Container& c) 
{
  for (ForwardIterator p = first; p != last; ++p) {
    Container::size_type idx = i_fun(*p);
    ++c[idx];
  }
}

template <class ForwardIterator, class IndexFunction, class Container>
void counts_in_buckets(ForwardIterator first, ForwardIterator last, IndexFunction i_fun, Container& c) 
{
  fill(c.begin(), c.end(), 0);
  for (ForwardIterator p = first; p != last; ++p) {
    Container::size_type idx = i_fun(*p);
    Container::size_type needed = idx+1;
    if (needed > c.size())
      c.insert(c.end(), needed - c.size(), 0);
    ++c[idx];
  }
}

template <class ForwardIterator, class IndexFunction, class Container, class Size_type>
void counts_in_buckets(ForwardIterator first, 
		       ForwardIterator last, 
		       IndexFunction i_fun, 
		       Container& c, 
		       Size_type max_index) 
{
  fill(c.begin(), c.end(), 0);
  Container::size_type needed = max_index+1;
  if (needed > c.size())
    c.insert(c.end(), needed - c.size(), 0);
  counts_in_buckets_unguarded(first, last, i_fun, c);
}


template <class T, class IndexFunction>
class bucket_vector {
public:
  typedef T value_type;
  typedef IndexFunction index_function;
  typedef vector<T>::iterator iterator;
  typedef vector<T>::size_type size_type;

protected:
  typedef bucket_vector<T, IndexFunction> self;
  typedef vector<iterator>::iterator index_iterator;
  typedef vector<iterator>::iterator index_iterator;

public:
  class bucket_type {
    friend self;
  private:
    index_iterator p;
    bucket_type(index_iterator x) : p(x) {}
  public:
    typedef bucket_vector<T, IndexFunction>::iterator iterator;
    iterator begin() { return *p; }
    iterator end() { return *(p + 1); }
    size_type size() { return  end() - begin(); }
    bool empty() { begin() == end(); }
  };
  IndexFunction i_fun;

protected:
  vector<T> data;
  vector<iterator> index;

  template <class ForwardIterator>
  void fill_bucket(ForwardIterator first, ForwardIterator last, vector<size_type>& count)
  {
    count.insert(count.begin(), 0);
    partial_sum(count.begin(), count.end(), count.begin());
    size_type total_count = *(count.end()-1);
    if (total_count > data.size()) 
      data.insert(data.end(), total_count - data.size(), *first);  // ???
    
    for (iterator p = first; p != last; ++p) {
      size_type idx = i_fun(*p);
      data[count[idx]] = *p;
      ++(count[idx]);
    }
    if (count.size() > index.size())
      index.insert(index.end(), count.size() - index.size(), 0);

    index[0] = data.begin();
    for (size_type i = 0; i < count.size()-1; ++i) {
      index[i+1] = data.begin() + count[i];
    }
  }
  template <class ForwardIterator>
  void fill_bucket(ForwardIterator first, ForwardIterator last) 
  {
    vector<size_type> count;
    counts_in_buckets(first, last, i_fun, count);
    fill_bucket(first, last, count);
  }
  template <class ForwardIterator>
  void fill_bucket(ForwardIterator first, ForwardIterator last, size_type max_index) 
  {
    vector<size_type> count;
    counts_in_buckets(first, last, i_fun, count, max_index);
    fill_bucket(first, last, count);
  }

public:
  bucket_type bucket(size_type n) { return bucket_type(index.begin() + n); }
  size_type number_of_buckets() const { return index.size() - 1; }

  iterator begin() { return data.begin(); }
  iterator end()   { return data.end(); }
  size_type size() { return data.size(); }

  template <class ForwardIterator>
  bucket_vector(ForwardIterator first, ForwardIterator last) { fill_bucket(first, last); }
  
  template <class ForwardIterator>
  bucket_vector(ForwardIterator first, 
		ForwardIterator last,
		size_type max_index) {
    fill_bucket(first, last, max_index);
  }
  
  template <class ForwardIterator, class ForwardIterator2>
  bucket_vector(ForwardIterator first, ForwardIterator last,
		ForwardIterator2 first_count, ForwardIterator2 last_count) {
    vector<size_type> count(first_count, last_count);
    fill_bucket(first, last, count.size());
  }
};
	     
} // Close namespace SGI

#endif /* SGI_bucket_container_h_INCLUDE */

// Local Variables:
// mode:C++
// End:
