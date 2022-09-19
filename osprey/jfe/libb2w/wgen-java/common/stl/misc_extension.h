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



#ifndef SGI_misc_extension_h_INCLUDED
#define SGI_misc_extension_h_INCLUDED

#include <iterator>

namespace SGI {

using std::random_access_iterator_tag;
  
template <class T1, class T2, class T3>
struct triple {
  typedef T1 first_type;
  typedef T2 second_type;
  typedef T3 third_type;

  T1 first;
  T2 second;
  T3 third;
  triple() : first(T1()), second(T2()), third(T3()) {}
  triple(const T1& a, const T2& b, const T3& c) : first(a), second(b), third(c) {}

  template <class U1, class U2, class U3>
  triple(const triple<U1, U2, U3>& p) : first(p.first), second(p.second), third(p.third) {}
};

template<class T1, class T2, class T3>
inline triple<T1, T2, T3> 
make_triple(const T1& x1, const T2& x2, const T3& x3) {
  return triple<T1, T2, T3>(x1, x2, x3);
}

template <class T1, class T2, class T3, class T4>
struct quadruple {
  typedef T1 first_type;
  typedef T2 second_type;
  typedef T3 third_type;
  typedef T4 fourth_type;

  T1 first;
  T2 second;
  T3 third;
  T4 fourth;
  quadruple() : first(T1()), second(T2()), third(T3()), fourth(T4()) {}
  quadruple(const T1& a, const T2& b, const T3& c, const T4& d) : 
    first(a), second(b), third(c), fourth(d) {}

  template <class U1, class U2, class U3, class U4>
  quadruple(const quadruple<U1, U2, U3, U4>& p) :
    first(p.first), second(p.second), third(p.third), fourth(p.fourth) {}
};

template<class T1, class T2, class T3, class T4>
inline quadruple<T1, T2, T3, T4>
make_quadruple(const T1& x1, const T2& x2, const T3& x3, const T4& x4) {
  return quadruple<T1, T2, T3, T4>(x1, x2, x3, x4);
}

template <class Tuple>
inline typename Tuple::first_type first(Tuple t) {
  return t.first;
}

template <class Tuple>
inline typename Tuple::second_type second(Tuple t) {
  return t.second;
}

template <class Tuple>
inline typename Tuple::third_type third(Tuple t) {
  return t.third;
}

template <class Tuple>
inline typename Tuple::fourth_type fourth(Tuple t) {
  return t.fourth;
}


template <class Integer>
struct int_iterator {
protected:
  Integer base;
public:
  typedef int_iterator<Integer> self;
  typedef random_access_iterator_tag iterator_category;
  typedef Integer value_type;
  typedef Integer difference_type;
  typedef Integer* pointer;
  typedef Integer& reference;

  self operator ++(int) { self tmp = *this; ++base; return tmp; }
  self &operator ++()   { ++base; return *this; }
  self operator --(int) { self tmp = *this; --base; return tmp; }
  self &operator --()   { --base; return *this; }
  self &operator +=(int n) { base += n; return *this; }
  self &operator -=(int n) { base -= n; return *this; }
  difference_type operator-(const self& x) const { return base - x.base; }
  self operator+(difference_type n) const { return self(base + n); }
  self operator-(difference_type n) const { return self(base - n); }

  Integer operator*() const { return base; }
  Integer operator[](int n) const { return *(*this + n); }

  int_iterator(Integer i):base(i){}
  int_iterator():base(){}
};

template <class Integer>
inline bool operator==(int_iterator<Integer> x, int_iterator<Integer> y) {
  return *x == *y; 
}

template <class Integer>
inline bool operator!=(int_iterator<Integer> x, int_iterator<Integer> y) {
  return *x != *y; 
}

template <class Integer>
inline bool operator<(int_iterator<Integer> x, int_iterator<Integer> y) {
  return *x < *y; 
}

template<class Integer>
inline int_iterator<Integer>
operator+(Integer x, const int_iterator<Integer>& y) {
  return y + x;
}

#ifdef __STL_FUNCTION_TMPL_PARTIAL_ORDER

template <class Integer>
inline bool
operator!=(const int_iterator<Integer>& x, const int_iterator<Integer>& y) {
  return !(x == y);
}

template <class Integer>
inline bool
operator>(const int_iterator<Integer>& x, const int_iterator<Integer>& y) {
  return y < x;
}

template <class Integer>
inline bool
operator<=(const int_iterator<Integer>& x, const int_iterator<Integer>& y) {
  return !(y < x);
}

template <class Integer>
inline bool
operator>=(const int_iterator<Integer>& x, const int_iterator<Integer>& y) {
  return !(x < y);
}

#endif /* __STL_FUNCTION_TMPL_PARTIAL_ORDER */

// This is a holdover from pre-standard STL days.
struct output_iterator {
  typedef std::output_iterator_tag iterator_category;
  typedef void value_type;
  typedef void difference_type;
  typedef void pointer;
  typedef void reference;
};

template <class Container>
class push_iterator : public output_iterator {
protected:
  Container& container;
public:
  push_iterator(Container& x) : container(x) {}
  push_iterator<Container>&
  operator=(const typename Container::value_type& value) { 
    container.push(value);
    return *this;
  }
  push_iterator<Container>& operator*() { return *this; }
  push_iterator<Container>& operator++() { return *this; }
  push_iterator<Container>& operator++(int) { return *this; }
};


template <class Indexable, class IndexFunction>
class index_iterator : public output_iterator {
protected:
  typedef index_iterator<Indexable, IndexFunction> self;
  Indexable& p;
  IndexFunction i_fun;
public:
  index_iterator(Indexable& x, const IndexFunction& f) : p(x), i_fun(f) {}
  template <class T>
  self& operator=(const T& value) { 
    p[i_fun(value)] = value;
    return *this;
  }
  self& operator*() { return *this; }
  self& operator++() { return *this; }
  self& operator++(int) { return *this; }
};


template <class From, class To>
struct converter : public std::unary_function<From, To> {
  To operator()(const From& x) { return To(x); }
};


// IDEA:
// value_trait - a way to extract value type from anything (containers, iterators...)


} // Close namespace SGI

#endif /* SGI_misc_extension_h_INCLUDE */

// Local Variables:
// mode:C++
// End:
