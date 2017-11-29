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

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef lu_mat_CXX
#define lu_mat_CXX      "lu_mat.cxx"
static char *lu_mat_template_rcs_id =  lu_mat_CXX "$Revision$";
#endif

#ifndef lu_mat_INCLUDED
#include "lu_mat.h"
#endif

// TODO Compilation hack

#ifndef __GNUC__
// Implementation stuff excluded here because g++
// (rightly) doesn't do implicit .cxx file inclusion.

extern MEM_POOL LNO_local_pool;

template <class T>
LU_MAT<T>& LU_MAT<T>::operator =(const LU_MAT<T>& a)
{
  if (_cpvt_sz < a._cpvt_sz) {
    if (_cpvt)
      CXX_DELETE_ARRAY(_cpvt, _pool);
    _cpvt = CXX_NEW_ARRAY(BOOL, a._cpvt_sz, _pool);
    _cpvt_sz = a._cpvt_sz;
  }
  for (INT i = 0; i < a._cpvt_sz; i++)
    _cpvt[i] = a._cpvt[i];

  if (_interch_sz < a._interch_sz) {
    if (_interch)
      CXX_DELETE_ARRAY(_interch, _pool);
    _interch = CXX_NEW_ARRAY(mINT32, a._interch_sz, _pool);
    _interch_sz = a._interch_sz;
  }
  for (i = 0; i < a._interch_sz; i++)
    _interch[i] = a._interch[i];

  _lu = a._lu;

  return *this;
}

template<class T>
LU_MAT<T>::LU_MAT(MEM_POOL* pool) :
	_pool(pool),
	_lu(0,0,pool),
	_interch(NULL),
	_cpvt(NULL),
	_interch_sz(0),
	_cpvt_sz(0)
{
}

template<class T>
LU_MAT<T>::LU_MAT(const LU_MAT<T>& a, MEM_POOL* pool) :
	_pool(pool),
	_lu(0,0,pool),
	_interch(NULL),
	_cpvt(NULL),
	_interch_sz(0),
	_cpvt_sz(0)
{
  *this = a;
}

template<class T>
LU_MAT<T>::LU_MAT(const LU_MAT<T>& a) :
	_pool(a.pool),
	_lu(0,0,pool),
	_interch(NULL),
	_cpvt(NULL),
	_interch_sz(0),
	_cpvt_sz(0)
{
  *this = a;
}


template<class T>
LU_MAT<T>::LU_MAT(const MAT<T>& m, MEM_POOL* pool) :
	_pool(pool),
	_lu(m.Rows(), 0, pool),
	_interch(CXX_NEW_ARRAY(mINT32, m.Rows(), pool)),
	_cpvt(CXX_NEW_ARRAY(BOOL, m.Cols(), pool)),
        _interch_sz(m.Rows()),
        _cpvt_sz(m.Cols())
{
  T* tmpc = CXX_NEW_ARRAY(T, m.Rows(), &LNO_local_pool);


  for (INT r = 0; r < m.Rows(); r++)
    _interch[r] = r;	// no interchanges

  for (INT c = 0; c < m.Cols(); c++)
    _cpvt[c] = FALSE;	// initialize to no pivot cols

  for (c = 0; c < m.Cols(); c++) {
    for (r = 0; r < m.Rows(); r++)
      tmpc[r] = m(r,c);
    Cfactor_And_Insert(tmpc, FALSE);
  }

  CXX_DELETE_ARRAY(tmpc, &LNO_local_pool);
}

template <class T>
BOOL LU_MAT<T>::Particular_Solution(const T* in, T* x) const
{
  T* inx = CXX_NEW_ARRAY(T, _lu.Rows(), &LNO_local_pool);
  for (INT i = 0; i < _lu.Rows(); i++)
    inx[i] = in[i];
  L_Mul(inx);
  BOOL ok = U_Solve(inx, x) ? TRUE : FALSE;
  CXX_DELETE_ARRAY(inx, &LNO_local_pool);
  return ok;
}

template<class T>
LU_MAT<T>::~LU_MAT()
{
  if (_interch)
    CXX_DELETE_ARRAY(_interch, _pool);
  if (_cpvt)
    CXX_DELETE_ARRAY(_cpvt, _pool);
}

template<class T>
MAT<T> LU_MAT<T>::Inv() const
{
  INT n = _lu.Rows();

  MAT<T> rval(n, n, 0);

  T* tmpc = CXX_NEW_ARRAY(T, n, &LNO_local_pool);
  T* tmpr = CXX_NEW_ARRAY(T, n, &LNO_local_pool);

  FmtAssert(_lu.Rows() == _lu.Cols(), ("inv(): Matrix is not square"));

  for (INT c = 0; c < n; c++)
    FmtAssert(_cpvt[c], ("inv(): matrix apparently singular"));

  for (INT r = 0; r < n; r++) {
    for (INT rr = 0; rr < n; rr++)
      tmpc[rr] = T(r == rr);
    L_Mul(tmpc);
    T* ok = U_Solve(tmpc, tmpr);
    FmtAssert(ok, ("LU_MAT<T>::Inv(): U_Solve failed"));
    rval.D_Update_Col(r, tmpr);
  }

  CXX_DELETE_ARRAY(tmpr, &LNO_local_pool);
  CXX_DELETE_ARRAY(tmpc, &LNO_local_pool);

  return rval;
}

template<class T>
MAT<T> LU_MAT<T>::Unfactor() const
{
  MAT<T> l = _lu.L();
  MAT<T> u = _lu.U();
  INT r = _lu.Rows();

  for (INT i = r - 1; i >= 0; i--) {
    if(_interch[i] != i) {
      Is_True(_interch[i] > i, ("Unfactor problem"));
      for (INT j = 0; j < r; j++) {
	T tmp = l(i,j);
	l(i,j) = l(_interch[i],j);
	l(_interch[i],j) = tmp;
      }
    }
  }

  return l*u;
}

template<class T>
MAT<T> LU_MAT<T>::TUnfactor() const
{
  MAT<T> lut = _lu.Trans();
  MAT<T> l = lut.L();
  MAT<T> u = lut.U();
  INT r = lut.Rows();
  INT c = lut.Cols();

  for (INT i = r - 1; i >= 0; i--) {
    if(_interch[i] != i) {
      Is_True(_interch[i] > i, ("Unfactor problem"));
      for (INT j = 0; j < r; j++) {
	T tmp = l(i,j);
	l(i,j) = l(_interch[i],j);
	l(_interch[i],j) = tmp;
      }
    }
  }

  return l*u;
}

// Make all free variables are zero except for the one specified.

template<class T>
T* LU_MAT<T>::U_Solve(const T* in, T* out, INT free) const
{
  INT rows = _lu.Rows();
  INT cols = _lu.Cols();

  INT zrow = 0;
  for (INT c = 0; c < cols; c++)
    zrow += _cpvt[c];

  for (INT r = zrow; r < rows; r++)
    if (in[r] != T(0))
      return NULL;

  // do the backsolve

  r = zrow - 1;
  for (c = cols-1; c >= 0; c--) {
    if (_cpvt[c]) {
      T t = in[r];
      for (INT cc = c+1; cc < cols; cc++)
	t -= _lu(r,cc) * out[cc];
      out[c] = t/_lu(r,c);
      r--;
    }
    else {
      out[c] = T(c == free);
    }
  }

  return out;
}

template<class T>
void LU_MAT<T>::L_Mul(T* f) const
{
  INT rows = _lu.Rows();
  INT cols = _lu.Cols();

  // first, interchanges

  for (INT r = 0; r < rows; r++) {
    INT rr = _interch[r];
    if (r != rr) {
      T t = f[rr];
      f[rr] = f[r];
      f[r] = t;
    }
  }

  // now apply L

  for (INT c = 0; c < cols; c++)
    for (r = c+1; r < rows; r++)
      f[r] -= f[c] * _lu(r,c);
}

template<class T>
INT LU_MAT<T>::Cfactor(T* col, INT rpvt) const
{
  INT nrpvt;

  INT rows = _lu.Rows();
  INT cols = _lu.Cols();

  L_Mul(col);

  if (rpvt == rows)
    return rows;

  if (Exact_Arithmetic()) {
    // any non-zero pivot will do
    for (nrpvt = rpvt; nrpvt < rows; nrpvt++)
      if (col[nrpvt] != T(0))
	break;
    if (nrpvt == rows)
      nrpvt = rpvt;
  }
  else {
    // select largest pivot
    T mx = T(0);
    INT imx = -1;
    for (nrpvt = rpvt; nrpvt < rows; nrpvt++) {
      T x = col[nrpvt] < 0 ? -col[nrpvt] : col[nrpvt];
      if (x > mx) {
	mx = x;
	imx = nrpvt;
      }
    }
    if (imx == -1)
      nrpvt = rpvt;
    else
      nrpvt = imx;
  }

  if (nrpvt != rpvt) {
    T t = col[rpvt];
    col[rpvt] = col[nrpvt];
    col[nrpvt] = t;
  }

  if (col[rpvt] != T(0)) {
    for (INT r = rpvt+1; r < rows; r++)
      col[r] /= col[rpvt];
  }

  return nrpvt;
}

template<class T>
BOOL LU_MAT<T>::Cfactor_And_Insert(T* w, INT insert_only_nonzero_piv)
{
  INT	c = _lu.Cols();
  INT	r = _lu.Rows();
  INT	curpivrow = 0;

  for (INT i = 0; i < c; i++)
    curpivrow += Is_Pivot(i);
  if(curpivrow == r && insert_only_nonzero_piv) {
    return FALSE;
  }

  Is_True(curpivrow <= r, ("Cfactor_And_Insert: %d <= %d", curpivrow, r));
  INT rrr = Cfactor(w, curpivrow);

  Is_True((curpivrow <= rrr && rrr < r) || (rrr == r && curpivrow == rrr),
	  ("Problem in Cfactor_And_Insert"));

  if (insert_only_nonzero_piv && w[curpivrow] == T(0))  {
    return FALSE;
  }

  if(curpivrow < r) {
    _interch[curpivrow] = rrr;
    if(rrr != curpivrow)
      _lu.D_Swap_Rows(rrr, curpivrow);
    if (_cpvt_sz <= c) {
      BOOL* newpivots = CXX_NEW_ARRAY(BOOL, c+2, _pool);
      _cpvt_sz = c+2;
      for (INT i = 0; i < c; i++)
	newpivots[i] = _cpvt[i];
      CXX_DELETE_ARRAY(_cpvt, _pool);
      _cpvt = newpivots;
    }
    _cpvt[c] = w[curpivrow] != 0;
  }
  else
    _cpvt[c] = FALSE;

  // w's entries already swapped
  if (curpivrow == c) {
    _lu.D_Add_Col(w);
  } else {
    for (INT i = curpivrow + 1; i < r; i++) 
      _lu(i,curpivrow) = w[i]; 
    for (i = curpivrow + 1; i < r; i++)
      w[i] = T(0); 
    _lu.D_Add_Col(w);
  }

  return TRUE;
}

template<class T>
void LU_MAT<T>::Print(FILE*f) const
{
  fprintf(f, "LU matrix output (%d x %d)\n", _lu.Rows(), _lu.Cols());
  _lu.Print(f);

  fprintf(f, "interchange vector:");
  for (INT r = 0; r < _lu.Rows(); r++)
    fprintf(f, " %d", _interch[r]);

  fprintf(f, "   column pivots: ");
  for (INT c = 0; c < _lu.Cols(); c++)
    fprintf(f, "%s", _cpvt[c] ? "T" : "F");
  fprintf(f, "\n");
}

#endif

