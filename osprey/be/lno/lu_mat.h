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
***	The factored matrix representation (LU_MAT): This uses MAT, and so
***	MAT must be instantiated whenever this is.  We provide instantiations
***	for double and FRAC, but not for an integral type, since in general
***	an integral LU factorization does not necessarily exist for an integer
***	matrix.  The memory pools are determined using the constructor only.
***	There is no other way to create an LU_MAT.
***
*** Reserved Prefixes:
***
***	LU
***
*** Exported Types:
***
***	LU_FMAT	The same as LU_MAT<FRAC>, a factored matrix of fractions
***	LU_DMAT	The same as LU_MAT<double>, a factored matrix of doubles
***
*** Exported Functions:
***
***	LU_MAT<T>::LU_MAT(const MAT<T>&, MEM_POOL*)
***	LU_MAT<T>::~LU_MAT()
***
***	    Generate an LU decomposition from a matrix.  This is the only
***	    constructor.  Destructor does the usual.
***
***	LU_MAT<T>::LU_MAT(const LU_MAT<T>&);
***	LU_MAT<T>& LU_MAT<T>::operator =(const MAT<T>&)
***	BOOL LU_MAT<T>::operator ==(const MAT<T>&) const
***	BOOL LU_MAT<T>::operator !=(const MAT<T>&) const
***
***	    Assignment, LU_MAT passing and returning, and comparison
***	    of LU_MATs are currently illegal.
***
***	void LU_MAT<T>::L_Mul(T*) const
***
***	    Multiply the passed in column (in place) by L.
***
***	T* LU_MAT<T>::U_Solve(const T* b, T* x) const
***	T* LU_MAT<T>::U_Solve(const T* b, T* x, INT) const
***
***	    Solves Ux = b, returning 2nd arg, or NULL if can't solve it.
***	    Any entry in x that is free is given a value of zero except
***	    if the third arg specifies a free entry, that entry gets a
***	    value of 1.
***
***	BOOL LU_MAT<T>::Particular_Solution(const T*, T*) const;
***
***	    Return in the second parameter a particular solution.  If there
***	    is none, return false.
***
***	MAT<T> LU_MAT<T>::Inv() const
***
***	    Given the factorization, compute the inverse.  Uses the default
***	    pool for MAT<T>, so it had better be set.
***
***	void LU_MAT<T>::Print(FILE*) const
***
***	    Print this data structure, for debugging.
***
***	BOOL LU_MAT<T>::Print_Element(FILE*, T)
***	BOOL LU_MAT<T>::Exact_Arithmetic()
***
***	    Exact_Arithmetic must be supplies when instantiating
***	    (even though it's private).  It indicates whether the T data
***	    type is exact (e.g. FRAC) or not (e.g. double), so that a
***	    sensible yet efficient pivoting strategy may be employed.
***	    Likewise for Print_Element().
**/

// Here are my comments about the internal representation of the LU 
// matrix.  I've reverse engineered this info by fixing a bug, so it's
// my best estimate of what is going on. 
//
// The LU decomposition can be though of as a series of elementary 
// transformations: 
//   U = L_(n-1) * P_(n-1) * ... * L_1 * P_1 * L_0 * P_0 * A 
// Here, each P is a permutation matrix, and each L is a lower triangular
// matrix with 1's on the diagonal.  The resulting matrix U is upper tri-
// angular.  I also believe that it should be row echelon form.  By this 
// I mean that: 
//       If u_i and u_j are two columns of U and i < j then the number of
//       nonzero elements at the beginning of u_i is <= the number of non-
//       zero elements at the beginning of u_j. 
// In addition to other properties that make U an upper triangular matrix.
// This additional property is required if the number of pivot columns is
// to be equal to the rank of U (and hence of A).  See PV #467253 for a 
// test case where the original code didn't do the right thing. 
// 
// The permutation matrices P_i are represented by the array _interch[]
// which has at most _interch_sz elements of storage allocated for it. 
// Initially we have _interch[i] == i for i = 1, 2, ..., n, where n is 
// the dimension of A.  (I'm assuming that A is square, which it always 
// will be in the cache model analysis.  We store separate row and column
// info, so it's possible that this works for non-square matrices, but I 
// don't know whether anyone is actually using it for that, and there are
// some areas in the code where I suspect the routines are not sufficiently
// general to handle the full m x n case.)  The bottom line is that if 
// _interch[i] == j, then P_i is the permutation matrix which tells us to 
// interchange rows i and j.  Don't expect _interch[] to be a permutation
// vector, it's not one. 
//
// The L_i are stored in the sub-diagonal entries of the LU matrix.  Spe- 
// cifically, L_i == I - LU'_i, where I is the n X n identity matrix and 
// LU'_i is the matrix which is 0 everywhere except for its i-th column 
// below the diagonal, which has the same entries as the LU matrix there. 
//
// The resulting matrix U is given by the upper triangular portion of the 
// LU matrix (including the main diagonal).
//
// There's one more important array _cpvt[] for which we allocate _cpvt_sz
// elements.  This is a boolean array of n elements, and _cpvt[i] is TRUE  
// when the i-th column of U is a pivot column.  Because we reduce U to 
// row echelon form, the number of TRUE entries in _cpvt[] tells us the 
// rank of the matrix U.  Since U has be tranformed from A with a series 
// of elementary transformations, this is also the rank of A.  
//
// This code is used extensively in the cache model to compute LU decom-
// positions of both singular and non-singular matrices.  When computing 
// the H and H_s matrices for reference groups, each row represents a 
// particular array reference pattern (i.e. subscript of an array). 
// 
// Here's an example of computing H and H_s matrices with this code: 
// Suppose:  
//  	   0 -1 1            0 -1 1
//   H =   0 -1 0      H_s = 0 -1 0 
//         1  0 0            0  0 0 
//     
// Note that we got H_s by striking out the last row of H.  This last 
// row represents the stride-1 reference.  Note also that H is non-singular
// with rank of 3, and H_s is singular with rank of 2. 
//
// Calling the constructor LU_MAT(const MAT<T>&, MEM_POOL*) on H gives the 
// following result: 
//
//         1  0 0	_interch[0 1 2] = [2 1 2]  
//   LU =  0 -1 0	_cpvt[0 1 2] = [T T T]  
//         0  1 1 
//
// This means that: 
//
//         0 0 1			        1 0 0	      1  0 0  
//   P_0 = 0 1 0    P_1 = P_2 = L_0 = L_2 = I = 0 1 0 	L_1 = 0  1 0 
//	   1 0 0  			        0 0 1	      0 -1 1 
//
// 	   1  0 0 
//   U =   0 -1 0 = L_1 * P_0 * H  
//         0  0 1 
//
// For H_s we get: 
//
//         0 -1  1 	_interch[0 1 2] = [0 1 2]  
//    LU = 1  0 -1      _cpvt[0 1 2] = [F T T]  
//         0  0  0 	   
//
// This means that: 
//
//  	                               1 0 0          1 0 0      0 -1  1 
//   P_0 = P_1 = P_2 = L_1 = L_2 = I = 0 1 0   L_0 = -1 1 0  U = 0  0 -1 
// 			               0 0 1	      0 0 1	 0  0  0 
// 
//  and U = L_0 * H_s 
// 						-- Comments by RJC   

/** $Revision: 1.5 $
*** $Date: 04/12/21 14:57:14-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lu_mat.h $
**/

#ifndef lu_mat_INCLUDED
#define lu_mat_INCLUDED "lu_mat.h"

static char *lu_mat_rcs_id = lu_mat_INCLUDED "$Revision: 1.5 $";

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
#ifndef mat_INCLUDED
#include "mat.h"
#endif

template<class T>
class LU_MAT {

 public:

  LU_MAT(const MAT<T>&, MEM_POOL*);
  LU_MAT(const LU_MAT<T>&, MEM_POOL*);
  LU_MAT(const LU_MAT<T>&);
  LU_MAT(MEM_POOL*);
  ~LU_MAT();

  T*			U_Solve(const T*, T*, INT = -1) const;
  BOOL			Particular_Solution(const T*, T*) const;
  MAT<T>		Inv() const;
  MAT<T>		Unfactor() const;
  MAT<T>		TUnfactor() const;
  BOOL			Is_Pivot(INT col) const {return _cpvt[col];}
  void			L_Mul(T*) const;
  void			Print(FILE*) const;
  const MAT<T>&		LU_Matrix() const {return _lu;}
  INT			Cfactor(T*, INT) const;	// factor column, given pivot
  BOOL			Cfactor_And_Insert(T*, BOOL);	// overwrites first p.
  LU_MAT<T>&		operator =(const LU_MAT<T>&);

 private:

  MAT<T>		_lu;		// lu decomposition
  mINT32*		_interch;	// so can map back to original
  BOOL*			_cpvt;		// which columns are pivots
  MEM_POOL*		_pool;		// where above data comes from
  INT			_interch_sz;	// bytes allocated for _interch
  INT			_cpvt_sz;	// bytes allocated for _cpvt

  static BOOL		Exact_Arithmetic();	// user supplied
  static void		Print_Element(FILE* f, T);

  // These are intentionally not defined in mat.cxx.  Don't use them.

  LU_MAT();
  BOOL			operator ==(const LU_MAT<T>&) const;
  BOOL			operator !=(const LU_MAT<T>&) const;
};

typedef LU_MAT<FRAC>	LU_FMAT;
typedef LU_MAT<double>	LU_DMAT;

#ifdef __GNUC__
// Implementation stuff included here because g++
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
  INT i;
  for (i = 0; i < a._cpvt_sz; i++)
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
        _pool(a._pool),
        _lu(0,0,a._pool),
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

  INT r;
  for (r = 0; r < m.Rows(); r++)
    _interch[r] = r;    // no interchanges

  INT c;
  for (c = 0; c < m.Cols(); c++)
    _cpvt[c] = FALSE;   // initialize to no pivot cols

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
  INT i;
  for (i = 0; i < _lu.Rows(); i++)
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

  INT c;
  for (c = 0; c < n; c++)
    FmtAssert(_cpvt[c], ("inv(): matrix apparently singular"));

  INT r, rr;
  for (r = 0; r < n; r++) {
    for (rr = 0; rr < n; rr++)
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

  INT i;
  for (i = r - 1; i >= 0; i--) {
    if(_interch[i] != i) {
      Is_True(_interch[i] > i, ("Unfactor problem"));
      INT j;
      for (j = 0; j < r; j++) {
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

  INT i;
  for (i = r - 1; i >= 0; i--) {
    if(_interch[i] != i) {
      Is_True(_interch[i] > i, ("Unfactor problem"));
      INT j; 
      for (j = 0; j < r; j++) {
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

  INT c;
  for (c = 0; c < cols; c++)
    zrow += _cpvt[c];

  INT r;
  for (r = zrow; r < rows; r++)
    if (in[r] != T(0))
      return NULL;

  // do the backsolve

  r = zrow - 1;
  for (c = cols-1; c >= 0; c--) {
    if (_cpvt[c]) {
      T t = in[r];
      INT cc;
      for (cc = c+1; cc < cols; cc++)
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
  INT r;
  for (r = 0; r < rows; r++) {
    INT rr = _interch[r];
    if (r != rr) {
      T t = f[rr];
      f[rr] = f[r];
      f[r] = t;
    }
  }

  // now apply L
  INT c;
  for (c = 0; c < cols; c++)
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
    INT r;
    for (r = rpvt+1; r < rows; r++)
      col[r] /= col[rpvt];
  }

  return nrpvt;
}

template<class T>
BOOL LU_MAT<T>::Cfactor_And_Insert(T* w, INT insert_only_nonzero_piv)
{
  INT   c = _lu.Cols();
  INT   r = _lu.Rows();
  INT   curpivrow = 0;
  INT i;
  for (i = 0; i < c; i++)
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
      INT i;
      for (i = 0; i < c; i++)
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
    INT i;
    for (i = curpivrow + 1; i < r; i++)
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
  INT r;
  for (r = 0; r < _lu.Rows(); r++)
    fprintf(f, " %d", _interch[r]);

  fprintf(f, "   column pivots: ");
  INT c;
  for (c = 0; c < _lu.Cols(); c++)
    fprintf(f, "%s", _cpvt[c] ? "T" : "F");
  fprintf(f, "\n");
}

#endif 

#endif
