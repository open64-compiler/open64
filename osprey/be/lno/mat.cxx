/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


// -*-C++-*-

/** $Revision: 1.6 $
*** $Date: 04/12/21 14:57:14-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.mat.cxx $
**/

#ifndef mat_CXX
#define mat_CXX      "mat.cxx"
#endif

#include <stdint.h>
#ifndef mat_INCLUDED
#include "lnopt_main.h"
#include "mat.h"
#endif

// TODO Compilation hack

extern MEM_POOL LNO_local_pool;

// ***********************************************************************
// 				MAT
// ***********************************************************************

//************** MAT member functions ******************

#ifndef __GNUC__
template<class T>
MAT<T>::MAT(INT r, INT c, MEM_POOL* mp)
{
  _r = r;
  _c = c;
  _rx = _calcx(r);
  _cx = _calcx(c);
  _pool = (mp ? mp : _default_pool);
  if (_rx > 0 && _cx > 0) {
    _data = CXX_NEW_ARRAY(T, _rx*_cx, _pool);
    FmtAssert(_data, ("Bad _data in initialization"));
  }
  else
    _data = 0;
}

template<class T>
MAT<T>::MAT(const MAT<T>& a, MEM_POOL* mp)
{
  _r = a._r;
  _c = a._c;
  _rx = a._rx;
  _cx = a._cx;
  _pool = (mp ? mp : _default_pool);
  if (_rx > 0 && _cx > 0) {
    _data = CXX_NEW_ARRAY(T, _rx*_cx, _pool);
    FmtAssert(_data, ("Bad _data in initialization"));
    memcpy(_data, a._data, _cx*_rx*sizeof(T[1]));
  }
  else
    _data = NULL;
}

template<class T>
INT MAT<T>::_calcx(INT v)
{
  static INT szs[] = {0, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80,
			0x100, 0x200, 0x400, 0x800,
			0x1000, 0x2000, 0x4000, 0x8000,
		      };
  static INT elts = sizeof(szs)/sizeof(INT[1]);

  for (INT i = 0; i < elts; i++) {
    if (v <= szs[i])
      break;
  }

  FmtAssert(i < elts, ("Matrix dimension %d too large\n", v));
  return szs[i];
}

template<class T>
void MAT<T>::_expand(INT rx, INT cx)
{
  FmtAssert(_rx <= rx, ("Senseless call to MAT<T>::_expand()"));
  FmtAssert(_cx <= cx, ("Senseless call to MAT<T>::_expand()"));

  if ((rx == _rx && cx == _cx) || rx == 0 || cx == 0) {
    _rx = rx;
    _cx = cx;
    return;
  }

  T* newdata = CXX_NEW_ARRAY(T, rx*cx, _pool);
  for (INT r = 0; r < Rows(); r++) {
    T* pp = &newdata[r*cx];
    const T* p = &_data[r*_cx];

    for (INT c = 0; c < Cols(); c++)
      *pp++ = *p++;
  }
  if (_data)
    CXX_DELETE_ARRAY(_data, _pool);
  _data = newdata;
  _rx = rx;
  _cx = cx;
}

template<class T>
MAT<T>& MAT<T>::operator =(const MAT<T>& a)
{
  if (&a == this)
    return *this;

  _r = a._r;
  _c = a._c;

  if (a._data == NULL) {
    CXX_DELETE_ARRAY(_data, _pool);
    _rx = a._rx;
    _cx = a._cx;
    _data = NULL;
    return *this;
  }

  if (_rx < a._rx || _cx < a._cx) {
    // Need to reallocate space, because maybe too small.
    if (_data)
      CXX_DELETE_ARRAY(_data, _pool);
    _data = CXX_NEW_ARRAY(T, a._rx * a._cx, _pool);
    FmtAssert(_data, ("Bad assignment to _data"));
    _rx = a._rx;
  }
  else
    FmtAssert(_data, ("missing _data in lhs MAT assignment"));

  _cx = a._cx;	// so that memcpy doesn't need to reshape
  _rx = a._rx;	
  FmtAssert(_data != a._data, ("same data in MAT assignment"));
  memcpy(_data, a._data, _cx*_rx*sizeof(T[1]));

  return *this;
}

template<class T>
MAT<T> MAT<T>::operator +(const MAT<T>& a) const
{
  FmtAssert(Rows() == a.Rows() && a.Cols() == Cols(),
	    ("MATs incompatable (%d,%d) + (%d,%d)",
	     Rows(), Cols(), a.Rows(), a.Cols())) ;

  MAT<T> m(Rows(), Cols(), Default_Pool());

  for (INT r = 0; r < Rows(); rr++) {
    T* pm = &m._data[r*m._cx];
    T* p = &_data[r*_cx];
    T* pa = &a._data[r*a._cx];

    for (INT c = 0; c < Cols(); c++)
      *pm++ = *p++ + *pa++;
  }

  return m;
}

template<class T>
MAT<T>& MAT<T>::operator +=(const MAT<T>& a)
{
  FmtAssert(Rows() == a.Rows() && a.Cols() == Cols(),
	    ("MATs incompatable (%d,%d) + (%d,%d)",
	     Rows(), Cols(), a.Rows(), a.Cols())) ;

  for (INT r = 0; r < Rows(); r++) {
    T* p = &_data[r*_cx];
    const T* pa = &a._data[r*a._cx];

    for (INT c = 0; c < Cols(); c++)
      *p++ += *pa++;
  }

  return *this;
}

template<class T>
MAT<T> MAT<T>::operator -(const MAT<T>& a) const
{
  FmtAssert(Rows() == a.Rows() && a.Cols() == Cols(),
	    ("MATs incompatable (%d,%d) - (%d,%d)",
	     Rows(), Cols(), a.Rows(), a.Cols())) ;

  MAT<T> m(Rows(), Cols(), Default_Pool());

  for (INT r = 0; r < Rows(); r++) {
    T* pm = &m._data[r*m._cx];
    const T* p = &_data[r*_cx];
    const T* pa = &a._data[r*a._cx];

    for (INT c = 0; c < Cols(); c++)
      *pm++ = *p++ - *pa++;
  }

  return m;
}

template<class T>
MAT<T>& MAT<T>::operator -=(const MAT<T>& a)
{
  FmtAssert(Rows() == a.Rows() && a.Cols() == Cols(),
	    ("MATs incompatable (%d,%d) - (%d,%d)",
	     Rows(), Cols(), a.Rows(), a.Cols())) ;

  for (INT r = 0; r < Rows(); r++) {
    T* p = &_data[r*_cx];
    const T* pa = &a._data[r*a._cx];

    for (INT c = 0; c < Cols(); c++)
      *p++ -= *pa++;
  }

  return *this;
}

template<class T>
MAT<T> MAT<T>::operator *(const MAT<T>& a) const
{
  FmtAssert(Cols() == a.Rows(),
	    ("MAT incompatable (%d,%d) * (%d,%d)",
	     Rows(), Cols(), a.Rows(), a.Cols())) ;

  MAT<T> m(Rows(), a.Cols(), Default_Pool());

  m.D_Zero();

  for (INT i = 0; i < Rows(); i++) {
    for (INT k = 0; k < Cols(); k++) {
      T* pm = &m._data[i*m._cx];
      const T* pa = &a._data[k*a._cx];
      const T t = _data[i*_cx+k];
      
      for (INT j = 0; j < a.Cols(); j++)
	*pm++ += t * *pa++;
    }
  }

  return m;
}

// I don't know how to do this in place, so I'll cheat

template<class T>
MAT<T>& MAT<T>::operator *=(const MAT<T>& a)
{
  FmtAssert(Cols() == a.Rows(),
	    ("MAT incompatable (%d,%d) * (%d,%d)",
	     Rows(), Cols(), a.Rows(), a.Cols())) ;

  MAT<T> m = (*this) * a;
  *this = m;
  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Submul(const MAT<T>& mat, INT m, INT n)
{
  MAT<T> mm(m, n, &LNO_local_pool);
  for (INT i = 0; i < m; i++) {
    for (INT j = 0; j < n; j++) {
      T r = T(0);
      for (INT k = 0; k < n; k++)
	r += (*this)(i,k) * mat(k,j);
      mm(i,j) = r;
    }
  }
  for (i = 0; i < m; i++)
    for (INT j = 0; j < n; j++)
      (*this)(i,j) = mm(i,j);
  return *this;
}

template<class T>
MAT<T> MAT<T>::operator *(T a) const
{
  MAT<T> m(Rows(), Cols(), Default_Pool());

  for (INT r = 0; r < Rows(); r++) {
    const T* p  = &_data[r*_cx];
    T* mp = &m._data[r*m._cx];

    for (INT c = 0; c < Cols(); c++)
      *mp++ = *p++ * a;
  }

  return m;
}

template<class T>
MAT<T>& MAT<T>::operator *=(T a)
{
  for (INT r = 0; r < Rows(); r++) {
    T* p = &_data[r*_cx];

    for (INT c = 0; c < Cols(); c++)
      *p++ = (*p) * a;
  }

  return *this;
}

template<class T>
MAT<T> MAT<T>::Trans() const
{
  MAT<T> m(Cols(), Rows());

  for (INT r = 0; r < Rows(); r++)
    for (INT c = 0; c < Cols(); c++)
      m(c,r) = (*this)(r,c);

  return m;
}

template<class T>
MAT<T>& MAT<T>::D_Trans()
{
  MAT<T> m = Trans();
  *this = m;
  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Zero()
{
  for (INT r = 0; r < Rows(); r++) {
    T* p = &_data[r*_cx];
    const T zero = T(0);

    for (INT c = 0; c < Cols(); c++)
      *p++ = zero;
  }

  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Identity()
{
  for (INT r = 0; r < Rows(); r++) {
    T* p = &_data[r*_cx];
    const T zero(0);
    const T one(1);

    for (INT c = 0; c < Cols(); c++)
      *p++ = (c == r) ? one : zero;
  }

  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Swap_Rows(INT r1, INT r2)
{
  if (r1 == r2)
    return *this;

  FmtAssert(r1 < Rows() && r2 < Rows(), ("Bad call to D_Swap_Rows()"));

  T* p1 = &_data[r1*_cx];
  T* p2 = &_data[r2*_cx];

  for (INT cc = 0; cc < Cols(); cc++) {
    T x = *p1;
    *p1++ = *p2;
    *p2++ = x;
  }

  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Swap_Cols(INT c1, INT c2)
{
  if (c1 == c2)
    return *this;

  FmtAssert(c1 < Cols() && c2 < Cols(), ("Bad call to D_Swap_Cols()"));

  for (INT r = 0; r < Rows(); r++) {
    T x = (*this)(r,c1);
    (*this)(r,c1) = (*this)(r,c2);
    (*this)(r,c2) = x;
  }

  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Update_Row(INT r, const T f[])
{
  FmtAssert(r < Rows(), ("Bad call to D_Update_Rows()"));

  T* p = &_data[r*_cx];

  for (INT c = 0; c < Cols(); c++)
    *p++ = f[c];

  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Update_Col(INT c, const T f[])
{
  FmtAssert(c < Cols(), ("Bad call to D_Update_Cols()"));

  for (INT r = 0; r < Rows(); r++)
    (*this)(r,c) = f[r];

  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Add_Row(const T f[])
{
  D_Add_Rows(1, FALSE);
  D_Update_Row(_r-1, f);
  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Add_Col(const T f[])
{
  D_Add_Cols(1, FALSE);
  D_Update_Col(_c-1, f);
  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Add_Rows(INT how_many, BOOL init_to_zero)
{
  Is_True(_r <= _rx, ("D_Add_Rows(): broken row size"));
  Is_True(how_many >= 0, ("D_Add_Rows(): passed how_many=%d", how_many));

  if (_r + how_many > _rx)
    _expand(_calcx(_r + how_many), _cx);

  _r += how_many;

  if (init_to_zero) {
    for (INT r = _r - how_many; r < _r; r++) {
      T* p = &_data[r*_cx];
      for (INT c = 0; c < Cols(); c++)
	*p++ = T(0);
    }
  }
  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Subtract_Rows(INT how_many)
{
  Is_True(_r <= _rx, ("D_Subtract_Rows(): broken row size"));
  Is_True(_r >= how_many, ("D_Subtract_Rows(): subtracting too many rows"));
  Is_True(how_many >= 0, ("D_Subtract_Rows(): passed how_many=%d", how_many));

  _r -= how_many;

  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Add_Cols(INT how_many, BOOL init_to_zero)
{
  Is_True(_c <= _cx, ("D_Add_Cols(): broken col size"));
  Is_True(how_many >= 0, ("D_Add_Cols(): passed how_many=%d", how_many));

  if (_c + how_many > _cx)
    _expand(_rx, _calcx(_c + how_many));

  _c += how_many;

  if (init_to_zero) {
    for (INT r = 0; r < Rows(); r++) {
      T* p = &_data[r*_cx];
      for (INT c = _c - how_many; c < _c; c++)
	p[c] = T(0);
    }
  }
  return *this;
}

template<class T>
MAT<T>& MAT<T>::D_Add_Identity_Rows_and_Cols(INT how_many)
{
  FmtAssert(Rows() == Cols(),
	    ("D_Add_Identity_Rows_and_Cols() requires square matrix"));
  D_Add_Rows(how_many, TRUE);
  D_Add_Cols(how_many, TRUE);
  for (INT r = _r - how_many; r < _r; r++)
    (*this)(r,r) = T(1);
  return *this;
}

template<class T>
BOOL MAT<T>::Is_Identity() const
{
  FmtAssert(Rows() == Cols(), ("Is_Identity() requires square matrix"));
  for (INT r = 0; r < Rows(); r++)
    for (INT c = 0; c < Cols(); c++)
      if ((*this)(r,c) != (r==c))
	return FALSE;
  return TRUE;
}


template<class T>
void MAT<T>::Print(FILE* f) const
{
  for (INT r = 0; r < Rows(); r++) {
    for (INT c = 0; c < Cols(); c++) {
      fprintf(f, " ");
      Print_Element(f, (*this)(r,c));
    }
    fprintf(f, "\n");
  }
}

template<class T>
MAT<T>& MAT<T>::D_Inv()
{
  return *this = Inv();
}

template<class T>
MAT<T> MAT<T>::L() const
{
  MAT<T> rval(Rows(), Rows(), 0);

  for(INT i = 0; i < Rows(); i++) {
    for (INT j = 0; j < Rows(); j++) {
      if (i < j)
	rval(i,j) = FRAC(0);
      else if(i == j)
	rval(i,j) = FRAC(1);
      else if (j < Cols())
	rval(i,j) = (*this)(i,j);
      else
	rval(i,j) = FRAC(0);
    }
  }
  return rval;
}

template<class T>
MAT<T> MAT<T>::U() const
{
  MAT<T> rval(Rows(), Cols(), 0);

  for(INT i = 0; i < Rows(); i++) {
    for(INT j = 0; j < Cols(); j++) {
      if (i <= j)
	rval(i,j) = (*this)(i,j);
      else
	rval(i,j) = FRAC(0);
    }
  }

  return rval;
}

#endif
