/*
  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

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

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/


/* auto-generated intrinsics for c++ std_vector intrinsic. 2021-10-26 11:57:14 */

  /* std::__1::vector<typename _Tp, typename _Allocator>::vector() */
  if (strcmp("_ZNSt3__16vectorC1Ev", fname) == 0) { return INTRN__ZNSt3__16vectorC1Ev; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(const std::__1::vector::allocator_type & __a) */
  if (strcmp("_ZNSt3__16vectorC1ERKT0_", fname) == 0) { return INTRN__ZNSt3__16vectorC1ERKT0_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(std::__1::vector::size_type __n) */
  if (strcmp("_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeE", fname) == 0) { return INTRN__ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(std::__1::vector::size_type __n, const std::__1::vector::allocator_type & __a) */
  if (strcmp("_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS3_", fname) == 0) { return INTRN__ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS3_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(std::__1::vector::size_type __n, const std::__1::vector::value_type & __x) */
  if (strcmp("_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS2_", fname) == 0) { return INTRN__ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS2_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(std::__1::vector::size_type __n, const std::__1::vector::value_type & __x, const std::__1::vector::allocator_type & __a) */
  if (strcmp("_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS2_RKS3_", fname) == 0) { return INTRN__ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS2_RKS3_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector<typename _InputIterator>(_InputIterator __first, typename enable_if<__is_input_iterator<_InputIterator>::value && !__is_forward_iterator<_InputIterator>::value && is_constructible<value_type, typename iterator_traits<_InputIterator>::reference>::value, _InputIterator>::type __last) */
  if (strcmp("_ZNSt3__16vectorC1ETL0__NS_9enable_ifIXaaaasr19__is_input_iteratorIS1_EE5valuentsr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueES1_E4typeE", fname) == 0) { return INTRN__ZNSt3__16vectorC1ETL0__NS_9enable_ifIXaaaasr19__is_input_iteratorIS1_EE5valuentsr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueES1_E4typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector<typename _InputIterator>(_InputIterator __first, _InputIterator __last, const std::__1::vector::allocator_type & __a, typename enable_if<__is_input_iterator<_InputIterator>::value && !__is_forward_iterator<_InputIterator>::value && is_constructible<value_type, typename iterator_traits<_InputIterator>::reference>::value>::type * ) */
  if (strcmp("_ZNSt3__16vectorC1ETL0__S1_RKT0_PNS_9enable_ifIXaaaasr19__is_input_iteratorIS1_EE5valuentsr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueEvE4typeE", fname) == 0) { return INTRN__ZNSt3__16vectorC1ETL0__S1_RKT0_PNS_9enable_ifIXaaaasr19__is_input_iteratorIS1_EE5valuentsr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueEvE4typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector<typename _ForwardIterator>(_ForwardIterator __first, typename enable_if<__is_forward_iterator<_ForwardIterator>::value && is_constructible<value_type, typename iterator_traits<_ForwardIterator>::reference>::value, _ForwardIterator>::type __last) */
  if (strcmp("_ZNSt3__16vectorC1ETL0__NS_9enable_ifIXaasr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueES1_E4typeE", fname) == 0) { return INTRN__ZNSt3__16vectorC1ETL0__NS_9enable_ifIXaasr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueES1_E4typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector<typename _ForwardIterator>(_ForwardIterator __first, _ForwardIterator __last, const std::__1::vector::allocator_type & __a, typename enable_if<__is_forward_iterator<_ForwardIterator>::value && is_constructible<value_type, typename iterator_traits<_ForwardIterator>::reference>::value>::type * ) */
  if (strcmp("_ZNSt3__16vectorC1ETL0__S1_RKT0_PNS_9enable_ifIXaasr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueEvE4typeE", fname) == 0) { return INTRN__ZNSt3__16vectorC1ETL0__S1_RKT0_PNS_9enable_ifIXaasr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueEvE4typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(const vector<_Tp, _Alloc> & __x) */
  if (strcmp("_ZNSt3__16vectorC1ERKNS_6vectorIT_T0_EE", fname) == 0) { return INTRN__ZNSt3__16vectorC1ERKNS_6vectorIT_T0_EE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(const vector<_Tp, _Alloc> & __x, const std::__1::vector::allocator_type & __a) */
  if (strcmp("_ZNSt3__16vectorC1ERKNS_6vectorIT_T0_EERKS3_", fname) == 0) { return INTRN__ZNSt3__16vectorC1ERKNS_6vectorIT_T0_EERKS3_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(initializer_list<std::__1::vector::value_type> __il) */
  if (strcmp("_ZNSt3__16vectorC1ESt16initializer_listIT_E", fname) == 0) { return INTRN__ZNSt3__16vectorC1ESt16initializer_listIT_E; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(initializer_list<std::__1::vector::value_type> __il, const std::__1::vector::allocator_type & __a) */
  if (strcmp("_ZNSt3__16vectorC1ESt16initializer_listIT_ERKT0_", fname) == 0) { return INTRN__ZNSt3__16vectorC1ESt16initializer_listIT_ERKT0_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(vector<_Tp, _Alloc> && __x) */
  if (strcmp("_ZNSt3__16vectorC1EONS_6vectorIT_T0_EE", fname) == 0) { return INTRN__ZNSt3__16vectorC1EONS_6vectorIT_T0_EE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::vector(vector<_Tp, _Alloc> && __x, const std::__1::vector::allocator_type & __a) */
  if (strcmp("_ZNSt3__16vectorC1EONS_6vectorIT_T0_EERKS3_", fname) == 0) { return INTRN__ZNSt3__16vectorC1EONS_6vectorIT_T0_EERKS3_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::assign<typename _InputIterator>(_InputIterator __first, _InputIterator __last) */
  if (strcmp("_ZNSt3__16vector6assignETL0__S1_", fname) == 0) { return INTRN__ZNSt3__16vector6assignETL0__S1_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::assign(std::__1::vector::size_type __n, std::__1::vector::const_reference __u) */
  if (strcmp("_ZNSt3__16vector6assignENS_13__vector_baseIT_T0_E9size_typeENS4_15const_referenceE", fname) == 0) { return INTRN__ZNSt3__16vector6assignENS_13__vector_baseIT_T0_E9size_typeENS4_15const_referenceE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::assign(initializer_list<std::__1::vector::value_type> __il) */
  if (strcmp("_ZNSt3__16vector6assignESt16initializer_listIT_E", fname) == 0) { return INTRN__ZNSt3__16vector6assignESt16initializer_listIT_E; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::get_allocator() */
  if (strcmp("_ZNKSt3__16vector13get_allocatorEv", fname) == 0) { return INTRN__ZNKSt3__16vector13get_allocatorEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::begin() */
  if (strcmp("_ZNSt3__16vector5beginEv", fname) == 0) { return INTRN__ZNSt3__16vector5beginEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::begin() */
  if (strcmp("_ZNKSt3__16vector5beginEv", fname) == 0) { return INTRN__ZNKSt3__16vector5beginEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::end() */
  if (strcmp("_ZNSt3__16vector3endEv", fname) == 0) { return INTRN__ZNSt3__16vector3endEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::end() */
  if (strcmp("_ZNKSt3__16vector3endEv", fname) == 0) { return INTRN__ZNKSt3__16vector3endEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::rbegin() */
  if (strcmp("_ZNSt3__16vector6rbeginEv", fname) == 0) { return INTRN__ZNSt3__16vector6rbeginEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::rbegin() */
  if (strcmp("_ZNKSt3__16vector6rbeginEv", fname) == 0) { return INTRN__ZNKSt3__16vector6rbeginEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::rend() */
  if (strcmp("_ZNSt3__16vector4rendEv", fname) == 0) { return INTRN__ZNSt3__16vector4rendEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::rend() */
  if (strcmp("_ZNKSt3__16vector4rendEv", fname) == 0) { return INTRN__ZNKSt3__16vector4rendEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::size() */
  if (strcmp("_ZNKSt3__16vector4sizeEv", fname) == 0) { return INTRN__ZNKSt3__16vector4sizeEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::capacity() */
  if (strcmp("_ZNKSt3__16vector8capacityEv", fname) == 0) { return INTRN__ZNKSt3__16vector8capacityEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::empty() */
  if (strcmp("_ZNKSt3__16vector5emptyEv", fname) == 0) { return INTRN__ZNKSt3__16vector5emptyEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::max_size() */
  if (strcmp("_ZNKSt3__16vector8max_sizeEv", fname) == 0) { return INTRN__ZNKSt3__16vector8max_sizeEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::reserve(std::__1::vector::size_type __n) */
  if (strcmp("_ZNSt3__16vector7reserveENS_13__vector_baseIT_T0_E9size_typeE", fname) == 0) { return INTRN__ZNSt3__16vector7reserveENS_13__vector_baseIT_T0_E9size_typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::operator[](std::__1::vector::size_type __n) */
  if (strcmp("_ZNSt3__16vectorixENS_13__vector_baseIT_T0_E9size_typeE", fname) == 0) { return INTRN__ZNSt3__16vectorixENS_13__vector_baseIT_T0_E9size_typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::operator[](std::__1::vector::size_type __n) */
  if (strcmp("_ZNKSt3__16vectorixENS_13__vector_baseIT_T0_E9size_typeE", fname) == 0) { return INTRN__ZNKSt3__16vectorixENS_13__vector_baseIT_T0_E9size_typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::at(std::__1::vector::size_type __n) */
  if (strcmp("_ZNSt3__16vector2atENS_13__vector_baseIT_T0_E9size_typeE", fname) == 0) { return INTRN__ZNSt3__16vector2atENS_13__vector_baseIT_T0_E9size_typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::at(std::__1::vector::size_type __n) */
  if (strcmp("_ZNKSt3__16vector2atENS_13__vector_baseIT_T0_E9size_typeE", fname) == 0) { return INTRN__ZNKSt3__16vector2atENS_13__vector_baseIT_T0_E9size_typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::front() */
  if (strcmp("_ZNSt3__16vector5frontEv", fname) == 0) { return INTRN__ZNSt3__16vector5frontEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::front() */
  if (strcmp("_ZNKSt3__16vector5frontEv", fname) == 0) { return INTRN__ZNKSt3__16vector5frontEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::back() */
  if (strcmp("_ZNSt3__16vector4backEv", fname) == 0) { return INTRN__ZNSt3__16vector4backEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::back() */
  if (strcmp("_ZNKSt3__16vector4backEv", fname) == 0) { return INTRN__ZNKSt3__16vector4backEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::push_back(std::__1::vector::const_reference __x) */
  if (strcmp("_ZNSt3__16vector9push_backENS_13__vector_baseIT_T0_E15const_referenceE", fname) == 0) { return INTRN__ZNSt3__16vector9push_backENS_13__vector_baseIT_T0_E15const_referenceE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::push_back(std::__1::vector::value_type && __x) */
  if (strcmp("_ZNSt3__16vector9push_backEOT_", fname) == 0) { return INTRN__ZNSt3__16vector9push_backEOT_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::pop_back() */
  if (strcmp("_ZNSt3__16vector8pop_backEv", fname) == 0) { return INTRN__ZNSt3__16vector8pop_backEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::insert(std::__1::vector::const_iterator __position, std::__1::vector::const_reference __x) */
  if (strcmp("_ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEENS5_15const_referenceE", fname) == 0) { return INTRN__ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEENS5_15const_referenceE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::insert(std::__1::vector::const_iterator __position, std::__1::vector::value_type && __x) */
  if (strcmp("_ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEEOS3_", fname) == 0) { return INTRN__ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEEOS3_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::insert(std::__1::vector::const_iterator __position, std::__1::vector::size_type __n, std::__1::vector::const_reference __x) */
  if (strcmp("_ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEENS5_9size_typeENS5_15const_referenceE", fname) == 0) { return INTRN__ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEENS5_9size_typeENS5_15const_referenceE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::insert<typename _InputIterator>(std::__1::vector::const_iterator __position, _InputIterator __first, _InputIterator __last) */
  if (strcmp("_ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEETL0__S8_", fname) == 0) { return INTRN__ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEETL0__S8_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::insert(std::__1::vector::const_iterator __position, initializer_list<std::__1::vector::value_type> __il) */
  if (strcmp("_ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEESt16initializer_listIS3_E", fname) == 0) { return INTRN__ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEESt16initializer_listIS3_E; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::erase(std::__1::vector::const_iterator __position) */
  if (strcmp("_ZNSt3__16vector5eraseENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEE", fname) == 0) { return INTRN__ZNSt3__16vector5eraseENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::erase(std::__1::vector::const_iterator __first, std::__1::vector::const_iterator __last) */
  if (strcmp("_ZNSt3__16vector5eraseENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEES7_", fname) == 0) { return INTRN__ZNSt3__16vector5eraseENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEES7_; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::clear() */
  if (strcmp("_ZNSt3__16vector5clearEv", fname) == 0) { return INTRN__ZNSt3__16vector5clearEv; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::resize(std::__1::vector::size_type __sz) */
  if (strcmp("_ZNSt3__16vector6resizeENS_13__vector_baseIT_T0_E9size_typeE", fname) == 0) { return INTRN__ZNSt3__16vector6resizeENS_13__vector_baseIT_T0_E9size_typeE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::resize(std::__1::vector::size_type __sz, std::__1::vector::const_reference __x) */
  if (strcmp("_ZNSt3__16vector6resizeENS_13__vector_baseIT_T0_E9size_typeENS4_15const_referenceE", fname) == 0) { return INTRN__ZNSt3__16vector6resizeENS_13__vector_baseIT_T0_E9size_typeENS4_15const_referenceE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::swap(vector<_Tp, _Alloc> & ) */
  if (strcmp("_ZNSt3__16vector4swapERNS_6vectorIT_T0_EE", fname) == 0) { return INTRN__ZNSt3__16vector4swapERNS_6vectorIT_T0_EE; }
  /* std::__1::vector<typename _Tp, typename _Allocator>::~vector<_Tp, _Alloc>() */
  if (strcmp("_ZNSt3__16vectorD1Ev", fname) == 0) { return INTRN__ZNSt3__16vectorD1Ev; }
/* end */
