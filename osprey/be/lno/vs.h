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


/**
*** Description:
***
***	A vector space class.
***
*** Description of data structure:
***
***	The data structure for a vector space.  Sometimes, it's more
***	convenient to have a list of basis vectors, sometimes an LU
***	decomposition of a matrix whose kernel is the vector space of
***	interest.  We keep a boolean around indicating which representation
***     is currently valid.
***
***	The vector space may be of type fract or double.  Typically,
***	we use this to represent a "vector space" over integers, but in
***	by the definition of vector space there is no such thing, and
***	the mathematics breaks down for integers.
***
***	The integer space is N dimensional.  The member function N()
***	indicates this dimensionality.  The member function D() indicates
***	the dimensionality of the subspace.  So the basis vector matrix,
***	in which each row of the array is a basis vector, is D() x N().
***	The LU matrix is also N() x D(), the LU decomposition of the
***	transpose of the basis vectors.
***	Sometimes one representation is more useful, sometimes the other.
***
***	These use the matrix types.  So you must do something like
***	   MAT<T>::Set_Default_Pool(&LNO_local_pool).
***
*** Exported Types:
***
***	VECTOR_SPACE<class T>
***
*** Exported Functions:
***
***	INT	N() const
***	INT	D() const
***
***	    Dimensionality of total space and vector subspace.
***
***	VECTOR_SPACE(INT n, MEM_POOL *pool, BOOL full = FALSE)
***
***	    Vector space in T^n, either of dimensionality d = 0 if 'full'
***	    is false, otherwise of d = n.
***
***	VECTOR_SPACE(const LU_MAT<T>& lu, MEM_POOL *pool)
***
***	    Make a vector space from Ker(lu).
***
***	VECTOR_SPACE(const VECTOR_SPACE* vs, MEM_POOL* pool)
***
***	    Copy constructor
***
***	VECTOR_SPACE& operator = (const VECTOR_SPACE& vs)
***
***	    Assignment.
***
***	~VECTOR_SPACE()
***
***	    Destruct me.
***
***	const MAT<T>& Basis() const
***
***	    Basis vectors, if you want them.
***
***	const LU_MAT<T>& Lu() const
***
***	    The lu decomposition, if you want it.
***
***	void Print(FILE *f) const
***
***	BOOL In(const T* w) const;
***
***	    Is the vector in the vector space?
***
***	BOOL Insert(T* w)
***
***	    Minimally increase the vector space to include this vector.
***	    Note that 'w' may be modified.  Return TRUE if the dimensionality
***	    is in fact increased.
***
***	VECTOR_SPACE&	operator +=(const VECTOR_SPACE&)
***	VECTOR_SPACE&	operator -=(const VECTOR_SPACE&)
***	VECTOR_SPACE&	operator *=(const VECTOR_SPACE&)
***
***	    Union spaces, subtract (in a but not in b), intersect.
***
***	MAT<T> Proj_Matrix() const
***
***	    A(AtA)-1At, where each basis vector is a column of A
***
***	void Beautify() const
***
***	    Make basis vectors point along axes where possible, and
***	    make basis vectors small integers if easy.  This latter
***	    function done by Reduce_Magnitude(), which is specialized
***	    for each T.  For fractions, it does as described above.
***	    For doubles, it does nothing.
***
***	void Sanity_Check()
***
***	    Assert failure if there's some inconsistency it happens to
***	    check for -- in this case no zero basis vectors.
**/

#ifndef vs_INCLUDED
#define vs_INCLUDED "vs.h"

const static char *vs_rcs_id = vs_INCLUDED "$Revision$";

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
#ifndef mat_INCLUDED
#include "mat.h"
#endif
#ifndef lu_mat_INCLUDED
#include "lu_mat.h"
#endif

typedef struct mem_pool MEM_POOL;

template<class T>
class VECTOR_SPACE {

 public:

  INT D() const {return _lud_is_valid ? _lud->LU_Matrix().Cols() : _bv.Rows();}
  INT N() const {return _lud_is_valid ? _lud->LU_Matrix().Rows() : _bv.Cols();}

  // initialize to null space or, if optional parameter true, to fullspace
  VECTOR_SPACE(INT n, MEM_POOL *pool, BOOL full = FALSE) :
    _bv(n,n,pool), _lud(NULL), _lud_is_valid(FALSE), _pool(pool) {
      if (full)
	_bv.D_Identity();
      else
	_bv.D_Subtract_Rows(n);
  }

  // initialize to kernel of passed in factored matrix
  // note that factoring is problematic, so not so general
  VECTOR_SPACE(const LU_MAT<T>& lu, MEM_POOL* pool);

  // copy a vector space
  VECTOR_SPACE(const VECTOR_SPACE* vs, MEM_POOL* pool) :
    _bv(vs->_bv, pool), _lud(NULL),
    _lud_is_valid(vs->_lud_is_valid), _pool(pool) {}

  VECTOR_SPACE& operator = (const VECTOR_SPACE& vs);

  ~VECTOR_SPACE() {
    if (_lud)
      CXX_DELETE(_lud, _pool);
  }

  const MAT<T>&		Basis() const { Make_Bv(); return _bv; }
  const LU_MAT<T>&	Lu() const { Make_Lu(); return *_lud; }

  void			Print(FILE *f, BOOL = FALSE) const;
  BOOL			In(const T* w) const;
  BOOL			Insert(T* w);
  VECTOR_SPACE&		operator +=(const VECTOR_SPACE&);
  VECTOR_SPACE&		operator -=(const VECTOR_SPACE&);
  VECTOR_SPACE&		operator *=(const VECTOR_SPACE&);
  MAT<T>		Proj_Matrix() const;
  void			Beautify() const;
  void			Sanity_Check() const;
  static void           Reduce_Row(T* row, INT elts);

 private:

  VECTOR_SPACE(const VECTOR_SPACE&);				// undefined
  VECTOR_SPACE();						// undefined

  MAT<T>		_bv;
  LU_MAT<T>*		_lud;
  BOOL			_lud_is_valid;
  MEM_POOL*		_pool;

  BOOL                  Has_Only_Elemetary_Basis_Vectors() const;
  void			Reduce_Magnitude() const;
  static void		Print_Element(FILE*, T);
  void			Make_Bv_Aux() const;
  void			Make_Lu_Aux() const;
  void			Make_Bv() const {
    if (_lud_is_valid)
      ((VECTOR_SPACE*)this)->Make_Bv_Aux();
  }
  void			Make_Lu() const {
    if (!_lud_is_valid)
      ((VECTOR_SPACE*)this)->Make_Lu_Aux();
  }

};

template <class T>
VECTOR_SPACE<T>* Read_VS(const char* cp, T dummy, MEM_POOL* pool);
extern const char* _Skip_Whitespace(const char* cp);	// for use in vs* files

#ifdef __GNUC__
// Implementation stuff included here because g++
// (rightly) doesn't do implicit .cxx file inclusion.

template <class T>
VECTOR_SPACE<T>& VECTOR_SPACE<T>::operator = (const VECTOR_SPACE& vs) {
  _bv = vs._bv;
  _lud_is_valid = vs._lud_is_valid;
  if (_lud)
    CXX_DELETE(_lud, _pool);
  if (_lud_is_valid)
    _lud = CXX_NEW(LU_MAT<T>(*vs._lud, _pool), _pool);
  else
    _lud = NULL;
  return *this;
}

template <class T>
void VECTOR_SPACE<T>::Make_Bv_Aux() const
{
  Is_True(_lud_is_valid, ("Bad call to Make_Bv_Aux()"));

  ((VECTOR_SPACE<T>*)this)->_bv = _lud->Unfactor().Trans();
  ((VECTOR_SPACE<T>*)this)->_lud_is_valid = FALSE;
#ifdef Is_True_On
  Sanity_Check();
#endif
}

template <class T>
void VECTOR_SPACE<T>::Make_Lu_Aux() const
{
  Is_True(!_lud_is_valid, ("Bad call to Make_Bv_Aux()"));

#ifdef Is_True_On
  Sanity_Check();
#endif

  if (_lud == NULL)
    ((VECTOR_SPACE<T>*)this)->_lud = CXX_NEW(LU_MAT<T>(_bv.Trans(), _pool),
                                             _pool);
  else
    *((VECTOR_SPACE<T>*)this)->_lud  = LU_MAT<T>(_bv.Trans(), &LNO_local_pool);

  ((VECTOR_SPACE<T>*)this)->_lud_is_valid = TRUE;
}

template <class T>
void VECTOR_SPACE<T>::Print(FILE *f, BOOL leave_lu) const
{
  if (leave_lu && _lud_is_valid) {
    _lud->Print(f);
    return;
  }

  Make_Bv();

  fprintf(f, "basis vectors: {");
  INT i;
  for (i = 0; i < _bv.Rows(); i++) {
    if (i > 0)
      fprintf(f, ",(");
    else fprintf (f, "(");
    INT j;
    for (j = 0; j < _bv.Cols(); j++) {
      if (j > 0)
        fprintf(f, ",");
      Print_Element(f, _bv(i,j));
    }
    fprintf(f, ")");
  }
  fprintf(f, "}\n");
}

template <class T>
INT VECTOR_SPACE<T>::In(const T* w) const
{
  Make_Lu();

  INT r = _lud->LU_Matrix().Rows();
  INT c = _lud->LU_Matrix().Cols();

  // the zero vector is in every space
  INT i;
  for (i = 0; i < r; i++)
    if(w[i] != 0)
      break;
  if (i == r)
    return TRUE;

  // if _lud has everything, then w is contained.
  INT curpivrow = 0;
  for (i = 0; i < c; i++)
    curpivrow += _lud->Is_Pivot(i);
  if (curpivrow == r)
    return TRUE;

  T *hold = CXX_NEW_ARRAY(T, r, &LNO_local_pool);
  for (i = 0; i < r; i++)
    hold[i] = w[i];

  _lud->Cfactor(hold, curpivrow);

  BOOL rval = hold[curpivrow] == 0;
  CXX_DELETE_ARRAY(hold, &LNO_local_pool);
  return rval;
}

template <class T>
BOOL VECTOR_SPACE<T>::Insert(T* w)
{
  Make_Lu();
  Reduce_Row(w, _bv.Cols());
  BOOL inserted = _lud->Cfactor_And_Insert(w, TRUE);
  return inserted;
}

template <class T>
VECTOR_SPACE<T>& VECTOR_SPACE<T>::operator +=(const VECTOR_SPACE<T>& v)
{
  v.Make_Bv();
  const MAT<T>& mat = &v.Basis();
  INT i;
  for (i = 0; i < mat.Rows(); i++) {
    const T* w = &mat(i,0);
    Insert(w);
  }
  return *this;
}

template <class T>
MAT<T> VECTOR_SPACE<T>::Proj_Matrix() const
{
  // A(AtA)-1At, where each basis vector is a column

  Make_Bv();

  MAT<T> mt(_bv);
  MAT<T> m = mt.Trans();
  MAT<T> mtm = mt * m;
  MAT<T> mtm_inv = mtm.Inv();
  MAT<T> rv = m * mtm_inv * mt;

  return rv;
}

template <class T>
VECTOR_SPACE<T>& VECTOR_SPACE<T>::operator -=(const VECTOR_SPACE<T>& vs)
{
  if (D() == 0 || vs.D() == 0)
    return *this;

  Make_Bv();
  MAT<T> pm = vs.Proj_Matrix();

  VECTOR_SPACE<T> ret(_bv.Cols(), &LNO_local_pool);

  T* space = CXX_NEW_ARRAY(T, vs.N(), &LNO_local_pool);

  vs.Make_Bv();
  INT i;
  for (i = 0; i < D(); i++) {
    const T* v = &_bv(i,0);
    BOOL non_zero = FALSE;
    for (INT j = 0; j < vs.N(); j++) {
      space[j] = v[j];
      for (INT k = 0; k < vs.N(); k++)
        space[j] -= pm(j,k) * v[k];
      if (space[j] != 0)
        non_zero = TRUE;
    }
    if (non_zero)
      ret.Insert(space);
  }

  CXX_DELETE_ARRAY(space, &LNO_local_pool);

  *this = ret;

  return *this;
}

template <class T>
BOOL VECTOR_SPACE<T>::Has_Only_Elemetary_Basis_Vectors() const
{
  Make_Bv();
  INT i;
  for (i = 0; i < _bv.Rows(); i++) {
    const T* w = &_bv(i,0);
    BOOL seen_one = FALSE;
    for (INT j = 0; j < _bv.Cols(); j++) {
      if (w[j] == 1) {
        if (seen_one == TRUE)
          return FALSE;
        seen_one = TRUE;
      }
      else if (w[j] != 0)
        return FALSE;
    }
    FmtAssert(seen_one, ("Zero basis vector"));
  }

  return TRUE;
}

template <class T>
VECTOR_SPACE<T>& VECTOR_SPACE<T>::operator *=(const VECTOR_SPACE<T>& vs)
{
  FmtAssert(N() == vs.N(), ("Illegal intersection %d, %d", N(), vs.N()));

  INT   n = N();

  // degenerate cases

  if (D() == 0)
    return *this;
  else if (vs.D() == 0)
    return *this = vs;

  Make_Bv();
  vs.Make_Bv();

  // Another degenerate case.  If both have only elementary basis vectors,
  // then if it's in *this but not in vs, remove from *this.

  if (Has_Only_Elemetary_Basis_Vectors() &&
      vs.Has_Only_Elemetary_Basis_Vectors()) {
    INT i;
    for (i = 0; i < _bv.Rows(); ) {
      const T* w = &_bv(i,0);
      INT j;
      for (j = 0; j < _bv.Cols(); j++) {
        if (w[j] == 1)
          break;
      }
      FmtAssert(j < _bv.Cols(), ("Bad elementary vector in *this"));
      INT ii;
      for (ii = 0; ii < vs._bv.Rows(); ii++) {
        if (vs._bv(ii,j) == 1)
          break;
      }
      if (ii == vs._bv.Rows()) { // not found, so remove
        if (i != _bv.Rows()-1)
          _bv.D_Swap_Rows(i, _bv.Rows()-1);
        _bv.D_Subtract_Rows(1);
      }
      else
        i++;
    }
    return *this;
  }

  // the matrix Q from Strang pp93-95.  NOTE:  If this routine is not efficient
  // enough (because of lu factorization), then note that an LU factorization
  // already exists in *this.  Possibly, one can just Cfactor_And_Insert
  // the basis vectors from vs.  Exactly how to do this I'm not sure.

  MAT<T> Q(n, D() + vs.D(), &LNO_local_pool);
  INT i;
  for (i = 0; i < D(); i++) {
    for (INT j = 0; j < n; j++)
      Q(j,i) = _bv(i,j);
  }
  for (i = 0; i < vs.D(); i++) {
    INT j;
    for (j = 0; j < n; j++)
      Q(j,D()+i) = vs._bv(i,j);
  }

  // compute ker(Q) and get its basis vectors

  LU_MAT<T> luQ(Q, &LNO_local_pool);
  VECTOR_SPACE<T> kerQ(luQ, &LNO_local_pool);
  kerQ.Make_Bv();

  // for each basis vector in the kernel, the first components tell us
  // what to multiply the vs basis vectors by to get a new basis vector.

  VECTOR_SPACE<T> answer(N(), &LNO_local_pool, FALSE);

  FRAC* tmp = CXX_NEW_ARRAY(FRAC, n, &LNO_local_pool);
  for (i = 0; i < kerQ.D(); i++) {
    // make a new basis in tmp
    INT j;
    for(j = 0; j < n; j++)
      tmp[j] = FRAC(0);
    for (INT ii = 0; ii < D(); ii++) {
      for (INT j = 0; j < n; j++)
        tmp[j] += kerQ._bv(i,ii) * _bv(ii,j);
    }
    FmtAssert(answer.In(tmp) == FALSE, ("Bug in intersection"));
    answer.Insert(tmp);
  }
  CXX_DELETE_ARRAY(tmp, &LNO_local_pool);

  answer.Beautify();
  return *this = answer;
}


// construct the vector space that the kernel of lu.

template <class T>
VECTOR_SPACE<T>::VECTOR_SPACE(const LU_MAT<T>& lu, MEM_POOL *pool) :
    _bv(lu.LU_Matrix().Cols(), lu.LU_Matrix().Cols(), pool), _lud(NULL),
    _lud_is_valid(FALSE), _pool(pool)
{
  T* x = CXX_NEW_ARRAY(T, lu.LU_Matrix().Rows(), &LNO_local_pool);
  T* zeros = CXX_NEW_ARRAY(T, lu.LU_Matrix().Rows(), &LNO_local_pool);
  T* ans = CXX_NEW_ARRAY(T, lu.LU_Matrix().Cols(), &LNO_local_pool);

  _bv.D_Subtract_Rows(_bv.Rows());      // pre-alloced the space
  INT i;
  for (i = 0; i < lu.LU_Matrix().Rows(); i++)
    zeros[i] = T(0);
  INT f;
  for(f = 0; f < lu.LU_Matrix().Cols(); f++) {
    // the kernel has a dimension for each non-pivot column
    if (!lu.Is_Pivot(f)) {
      if (!lu.U_Solve(zeros, ans, f))
        FmtAssert(0, ("Bad usolve in kernel computation"));
      if (!Insert(ans)) {
        FmtAssert(0, ("Bad insert in kernel computation"));
      }
    }
  }

  CXX_DELETE_ARRAY(x, &LNO_local_pool);
  CXX_DELETE_ARRAY(zeros, &LNO_local_pool);
  CXX_DELETE_ARRAY(ans, &LNO_local_pool);
}

template <class T>
void VECTOR_SPACE<T>::Beautify() const
{
  INT i;
  INT j;

  T* x = CXX_NEW_ARRAY(T, N(), &LNO_local_pool);

  VECTOR_SPACE hold(N(), &LNO_local_pool);

  // quick check to see if already beautified: all basis are e's.
  // this is by far the common case, and much faster than going through
  // the entire process.

  BOOL fine = TRUE;

  Make_Bv();
  for (i = 0; fine && i < D(); i++) {
    INT the_non_zero = -1;
    for (j = 0; j < N(); j++) {
      if (_bv(i,j) != 0) {
        if (the_non_zero == -1)
          the_non_zero = j;
        else
          fine = FALSE;
      }
    }
    if (fine)
      // Gee, maybe Beautify shouldn't be const :-)
      (*(MAT<T>*)&_bv)(i,the_non_zero) = T(1);
  }
  if (fine)
    return;

  // something's ugly, so do full-scale beautification

  for (j = 0; j < N(); j++) {
    for(i = 0; i < N(); i++)
      x[i] = T(i==j);
    if (In(x))
      hold.Insert(x);
  }

  *(VECTOR_SPACE<T>*)this -= hold;

  Make_Bv();
  hold.Make_Bv();
  for (i = 0; i < hold.D(); i++)
    ((VECTOR_SPACE<T>*)this)->_bv.D_Add_Row(&hold._bv(i,0));

  CXX_DELETE_ARRAY(x, &LNO_local_pool);

  Reduce_Magnitude();
}


template <class T>
void VECTOR_SPACE<T>::Sanity_Check() const
{
  Make_Bv();
  INT i;
  for (i = 0; i < _bv.Rows(); i++) {
    INT j;
    for (j = 0; j < _bv.Cols(); j++) {
      if (_bv(i,j) != 0)
        break;
    }
    FmtAssert(j < _bv.Cols(), ("Sanity check failed vector space!"));
  }
}

template <class T>
void VECTOR_SPACE<T>::Reduce_Magnitude() const
{
  INT i;

  Make_Bv();

  for (i = 0; i < _bv.Rows(); i++) {
    Reduce_Row((T*)&_bv(i,0), _bv.Cols());
  }
}

#endif

#endif
