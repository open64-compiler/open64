/* -*- c++ -*- */

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

/**
*** Description:
***
***	This interface describes a matrix class and a factored matrix
***	class (a factored representation can be useful, e.g. for computing
***	inverses, quickly solving Ax=b problems, etc).  We discuss each
***	in turn.
***
***	The matrix representation (MAT):  It is a template, and is instantiated
***	for integer, floating, and fraction types.  It should be
***	efficient in representing both small and large matrices.
***
***	Implementation details: There is one allocated chunk of memory:
***	the entire matrix.  There is some extra padding, so that when rows
***	or columns are added or deleted, further calls to new and rewrites
***	of the data will be less frequent.  See _calcx for details.
***
***	Although this is a template class, it is not possible to write
***	general template code for each class instantiation.  For example,
***	inverse is different for MAT<mINT32> and MAT<double>.  So if
***	users of this code wish to instantiate a new matrix class, they
***	will also have to supply some functions that are not defined 
***	automatically.  Details below.  Any type that instantiates a template
***	must have T(0) and T(1) defined to construct an object of type T
***	with values 0 and 1.  mat.cxx contains the general template code
***	and is not to be compiled directly (the prelinker takes care of that),
***	and mat_textra.cxx contains specific instantiation code and does
***	need to be included in the compile.
***
***	Instantiations are provided for
***
***		MAT<mINT32>	a.k.a. IMAT
***		MAT<FRAC>	a.k.a. FMAT
***		MAT<double>	a.k.a. DMAT
***
***	Functions that create a MAT take a memory pool from which to allocate
***	space for the mat.  In certain cases (e.g. operator+), the syntax does
***	not allow specification of a pool.  In those cases, the memory pool
***	specified in MAT<type>::_default_pool is used.
***
*** Reserved Prefixes:
***
***	MAT, IMAT, FMAT, DMAT
***
*** Exported Types:
***
***	IMAT	The same as MAT<mINT32>, a matrix of integers
***	FMAT	The same as MAT<FRAC>, a matrix of fractions
***	DMAT	The same as MAT<double>, a matrix of doubles
***
*** Exported Functions:
***
***	Note that the functions MAT::Inv() and MAT::Print_Element() must
***	be supplied manually for each instantiation type -- see mat.cc.
***
***	Functions for the MAT class:
***
***	MAT::MAT(MEM_POOL* =0)
***
***	    Constructs a 0 x 0 matrix, doesn't allocate space.
***
***	MAT::MAT(INT r, INT c, MEM_POOL* =0)
***
***	    Constructs an rxc matrix, allocates space, uninitialized.
***
***	MAT::MAT(const MAT<T>&, MEM_POOL* =0)
***
***	    Constructs a matrix of same size and data as the arg.  Here the
***	    pool of the created is a copy of the creator's.
***
***	~MAT()
***
***	    Destructs a matrix, freeing space if necessary.
***
***	MAT<T>& operator =(const MAT<T>&)
***
***	    Matrix assignment.  There is no problem reshaping a matrix.
***
***	INT MAT::Rows() const
***
***	    How many (logical) rows in this matrix.
***
***	INT MAT::Cols() const
***
***	    How many (logical) colums in this matrix.
***
***	const T& MAT::operator ()(INT r, INT c) const
***	T& MAT::operator ()(INT r, INT c)
***
***	    The entry in row r, column c.
***
***	MAT<T> MAT::operator +(const MAT<T>&) const
***	MAT<T>& MAT::operator +=(const MAT<T>&)
***	MAT<T> MAT::operator -(const MAT<T>&) const
***	MAT<T>& MAT::operator -=(const MAT<T>&)
***	MAT<T> MAT::operator *(const MAT<T>&) const
***	MAT<T>& MAT::operator *=(const MAT<T>&)
***	MAT<T>& MAT::D_Submul(const MAT<T>& mat, INT m, INT n)
***
***	    Matrix-matrix addition, subtraction, multiplication.  The last
***	    one updates this matrix by multiplying its mxn upper left
***	    submatrix by the nxn upper left submatrix of mat.
***
***	MAT<T> MAT::operator *(T) const
***	MAT<T>& MAT::operator *=(T)
***	friend MAT<T> operator *(T a, MAT<T> const&)
***
***	    Matrix-scalar multiplication.
***
***	Functions with names prepended with D_ are destructive, so that
***	they operate on themselves.  Functions without are non-distructive.
***	I have intentionally not included a complete set of both, because I
***	fear that, for example, a member function of zero() might be used
***	mistakenly instead of D_zero.
***
***	MAT<T>& MAT::D_Zero()
***	MAT<T>& MAT::D_Identity()
***
***	    Overwrite matrix with zeros, with a same sized identity matrix.
***
***	MAT<T>& MAT::D_Swap_Rows(INT r1, INT r2)
***	MAT<T>& MAT::D_Swap_Cols(INT c1, INT c2)
***
***	    Swap given rows or cols in the matrix.
***
***	MAT<T>& MAT::D_Add_Row(const T*)
***	MAT<T>& MAT::D_Add_Col(const T*)
***
***	    Append this vector of T as a new last row/column of this matrix.
***
***	MAT<T>& MAT::D_Add_Rows(INT how_many, BOOL init_to_zero = FALSE)
***	MAT<T>& MAT::D_Add_Cols(INT how_many, BOOL init_to_zero = FALSE)
***
***	    Add the given number of rows or columns to the matrix,
***	    uninitialized unless init_to_zero is set, in which case zero.
***
***	MAT<T>& D_Add_Identity_Rows_and_Cols(INT how_many);
***
***	    Given a square matrix, add the given number more rows and cols
***	    so that it's still square, and make the new entries identity.
***
***	MAT<T>& MAT::D_Update_Row(INT row, const T newrow[])
***	MAT<T>& MAT::D_Update_Col(INT col, const T newcol[])
***
***	    Change row/col to have new entries.
***
***	MAT<T>  Trans() const
***	MAT<T>& D_Trans()
***
***	    Transpose a matrix.  t() makes a copy, and d_t() transposes in
***	    place, as per the naming convention.
***
***	MAT<T>  Inv() const
***	MAT<T>& D_Inv()
***
***	    Invert a matrix.  Notice that for integer matrices, this may not
***	    be possible.  In fact, Inv() is not even defined in the template;
***	    if you instantiate a type, you are responsible for supplying Inv().
***
***	    In the case of IMAT, an Inv() routine is supplied (as it must be
***	    for IMAT to successfuly instantiate).  mat.cc provides two
***	    implementations, one where the matrix is converted into a matrix
***	    of fractions, another where it is converted into a matrix of
***	    doubles.  I believe the doubles is probably more efficient (even
***	    though pivoting is more expensive), so that's the version of inv()
***	    used by IMAT.  This routine returns successfully if and only if
***	    the matrix being factored is unimodular (so that it can return
***	    an integer matrix).  Otherwise, it's an assertion failure.
***
***	    Note that in theory both versions could fail on a unimodular
***	    matrix if precision problems occur.  The double version could
***	    also in theory return an inverse for a matrix that is not
***	    unimodular, but has entries that are all "very close" to integer.
***	    Because the fraction type always knows when it overflows, fractions
***	    are slightly safer.  Also, there is only a problem on large
***	    matrices with large entries, both of which are rare in our
***	    applications.
***
***	    Currenly, all versions use MAT<T>::_default_pool for temporary
***	    computations.
***
***	void MAT<T>::Print(FILE*) const
***
***	    Print the matrix to a file (helpful for debugging).
***
***	FMAT IMAT_to_FMAT(const IMAT&, MEM_POOL* =0)
***	IMAT FMAT_to_IMAT(const FMAT&, MEM_POOL* =0)
***	DMAT IMAT_to_DMAT(const IMAT&, MEM_POOL* =0)
***	IMAT DMAT_to_IMAT(const DMAT&, MEM_POOL* =0)
***
***	    These external functions convert between some diffent
***	    representations.  The returns value uses the return value's
***	    default pool, and that the passed in mem pool is only
***	    where to put the temporary.
***
***	static void Print_Element(FILE*, T)
***
***	    This is not part of the interface, but you must supply this
***	    function when instantiating so that the Print() function knows
***	    how to do this.  There are better ways in C++ to do this, but
***	    since we have to interact with C code, we have to use stdio,
***	    so this is as easy as anything.
***
***	static MAT<T>::MEM_POOL* _set_default_pool(MEM_POOL* pool);
***
***	    Set the default mem pool and return the previous default.
***
***	static MAT<T>::MEM_POOL* Default_Pool();
***
***	    Return the current default mem pool.
**/

/** $Revision: 1.5 $
*** $Date: 04/12/21 14:57:14-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.mat.h $
**/

#ifndef mat_INCLUDED
#define mat_INCLUDED "mat.h"

#ifdef _KEEP_RCS_ID
static char *mat_rcs_id = mat_INCLUDED "$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

// system  headers

#ifndef __STRING_H__
extern "C" {
#include "string.h"
}
#endif

// redefine int

#ifndef defs_INCLUDED
#include "defs.h"
#endif

// rest of the compiler headers

#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif
#ifndef frac_INCLUDED
#include "frac.h"
#endif

// MAT is implemented as _rx by _cx, which may be slightly larger
// than _r by _c to ease the efficiency of add_row, add_col, rm_row, rm_col.

template<class T>
class MAT {
 public:

  MAT(MEM_POOL* mp =0) :
		_r(0), _c(0), _rx(0), _cx(0),
		_pool(mp ? mp : _default_pool),
		_data(0) {}
  MAT(INT r, INT c, MEM_POOL* mp =0);
  MAT(const MAT<T>& a, MEM_POOL* mp =0);
  ~MAT() {if (_data) CXX_DELETE_ARRAY(_data, _pool);}

  MAT<T>&	operator =(const MAT<T>&);

  INT		Rows() const {return _r;}
  INT		Cols() const {return _c;}

  const T& operator ()(INT r, INT c) const {
    Is_True(r < _r && c < _c, ("Bad ref(%d,%d), size(%d,%d)", r, c, _r, _c));
    return _data[r*_cx + c];
  }

  T& operator ()(INT r, INT c) {
    Is_True(r < _r && c < _c, ("Bad ref(%d,%d), size(%d,%d)", r, c, _r, _c));
    return _data[r*_cx + c];
  }

  MAT<T>	operator +(const MAT<T>&) const;
  MAT<T>&	operator +=(const MAT<T>&);
  MAT<T>	operator -(const MAT<T>&) const;
  MAT<T>&	operator -=(const MAT<T>&);
  MAT<T>	operator *(MAT<T> const&) const;
  MAT<T>&	operator *=(MAT<T> const&);

  friend MAT<T>	operator *(T a, MAT<T> const& m) {return m * a;}
  MAT<T>	operator *(T) const;
  MAT<T>&	operator *=(T);

  MAT<T>	Trans() const;
  MAT<T>	Inv() const;
  MAT<T>	L() const;
  MAT<T>	U() const;

  MAT<T>&	D_Zero();
  MAT<T>&	D_Identity();
  MAT<T>&	D_Trans();
  MAT<T>&	D_Inv();
  MAT<T>&	D_Swap_Rows(INT, INT);
  MAT<T>&	D_Swap_Cols(INT, INT);
  MAT<T>&	D_Add_Row(const T f[]);
  MAT<T>&	D_Add_Col(const T f[]);
  MAT<T>&	D_Add_Rows(INT how_many, BOOL = FALSE);
  MAT<T>&	D_Add_Cols(INT how_many, BOOL = FALSE);
  MAT<T>&	D_Add_Identity_Rows_and_Cols(INT how_many);
  MAT<T>&	D_Subtract_Rows(INT how_many);
  MAT<T>&	D_Update_Row(INT row, const T* f);
  MAT<T>&	D_Update_Col(INT col, const T* f);

  MAT<T>&	D_Submul(const MAT<T>& mat, INT m, INT n);

  BOOL		Is_Identity() const;

  void		Print(FILE* f) const;

  static MEM_POOL* Set_Default_Pool(MEM_POOL* mp) {
    MEM_POOL* oldpool = _default_pool;
    _default_pool = mp;
    return oldpool;
  }

  static MEM_POOL* Default_Pool() {return _default_pool;}
  const MEM_POOL* Pool() const {return _pool;}

 private:

  INT		_r;		// current number of rows
  INT		_c;		// current number of cols
  INT		_rx;		// allocated number of rows
  INT		_cx;		// allocated number of cols
  T*		_data;		// column major, _rx by _cx
  MEM_POOL*	_pool;		// from where to alloc and delete the data

  // compute _rx or _cx from _r or _c

  static INT		_calcx(INT);
  static void		Print_Element(FILE* f, T);
  static MEM_POOL*	_default_pool;
  void			_expand(INT rx, INT cx);	// reset rx and cx
};

typedef MAT<mINT32>	IMAT;
typedef MAT<FRAC>	FMAT;
typedef MAT<double>	DMAT;

// Can't make this templates because of C++ bugs.  This uses the memory pool
// of the type it creates.

extern FMAT IMAT_to_FMAT(const IMAT&, MEM_POOL* =0);
extern DMAT IMAT_to_DMAT(const IMAT&, MEM_POOL* =0);
extern IMAT FMAT_to_IMAT(const FMAT&, MEM_POOL* =0);
extern IMAT DMAT_to_IMAT(const DMAT&, MEM_POOL* =0);

#ifdef __GNUC__
// Implementation stuff included here from mat.cxx because g++
// (rightly) doesn't do implicit .cxx file inclusion.

// ***********************************************************************
// 				MAT
// ***********************************************************************

//************** MAT member functions ******************

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

  INT i;
  for (i = 0; i < elts; i++) {
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

  for (INT r = 0; r < Rows(); r++) {
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
  INT i;
  for (i = 0; i < m; i++) {
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

#endif
