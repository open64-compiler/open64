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



#ifndef SGI_iterator_adapter_h_INCLUDED
#define SGI_iterator_adapter_h_INCLUDED

#include <iterator>

namespace SGI {
  
template <class Iterator> 
class const_iterator {
protected:
  Iterator current;

public:
  typedef typename iterator_traits<Iterator>::iterator_category iterator_category;
  typedef typename iterator_traits<Iterator>::value_type value_type;
  typedef typename iterator_traits<Iterator>::difference_type difference_type;
  typedef typename iterator_traits<Iterator>::pointer pointer;
  typedef const iterator_traits<Iterator>::value_type& reference;
  typedef const iterator_traits<Iterator>::value_type& const_reference;
  typedef Iterator iterator_type;
  typedef const_iterator<Iterator> self;

  const_iterator() {}
  explicit const_iterator(iterator_type x) : current(x) {}

  const_iterator(const self& x) : current(x.current) {}
  template <class Iter>
  const_iterator(const const_iterator<Iter>& x) : current(x.current) {}
    
  iterator_type base() const { return current; }
  const_reference operator*() const {
    return *current; 
  }
  pointer operator->() const { return &(operator*()); }

  self& operator++() {
    ++current;
    return *this;
  }
  self operator++(int) {
    self tmp = *this;
    ++current;
    return tmp;
  }
  self& operator--() {
    --current;
    return *this;
  }
  self operator--(int) {
    self tmp = *this;
    --current;
    return tmp;
  }

  self operator+(difference_type n) const {
    return self(current + n);
  }
  self operator-(difference_type n) const {
    return self(current - n);
  }
  difference_type operator-(self n) const {
    return difference_type(current - n.current);
  }
  self& operator+=(difference_type n) {
    current += n;
    return *this;
  }
  self& operator-=(difference_type n) {
    current -= n;
    return *this;
  }
  const_reference operator[](difference_type n) const { return *(*this + n); }  
}; 
 
} // Close namespace SGI

#endif /* SGI_iterator_adapter_h_INCLUDE */

// Local Variables:
// mode:C++
// End:
