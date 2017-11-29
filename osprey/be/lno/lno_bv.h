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


//-*-c++-*-
//
/**
*** Module: lno_bv.h
*** $Revision$
*** $Date$
*** $Author$
*** $Source$
*** 
*** Revision history:
***
***     10-APR-95 dkchen - Original Version
***
*** Description:
*** 
***     This interface describes a bit vector graph class. It can be used to
***     represent bit vectors of arbitrary length. Althought the size of
***     each vector has to be initialized and fixed.
***
*** Exported Types and Functions
*** 
***     BIT_VECTOR
*** 
*** 	The bit vector class
*** 
***         BIT_VECTOR()
***         
***             Default constructor.
***         
***         ~BIT_VECTOR() {}
***         
***             Destructor.
***         
***         BIT_VECTOR(UINT size, MEM_POOL *pool)
***         
***             Construct a bit vector of size 'size' using memory pool
***             'pool'. The same memory pool is also used for results
***             from operations such as '&' and '|' (see below).
***
***	    UINT Size()  
***
***		How many elements in the bit vector
***         
***         void Init(UINT size, MEM_POOL *pool) 
***
***             Same as the constructor.
***
***         void Set(UINT bit_position)
***
***             Set a bit specified by 'bit_position'.
***
***         void Reset(UINT bit_position)
***
***             Reset a bit specified by 'bit_position'.
***
***         BOOL Test(UINT bit_position)
***
***             Test if bit at 'bit_position' is set.
***
***         void Print(FILE *fp)
***
***             Print the bit vector in Hex-decimal format to FILE fp.
***
***         UINT Pop_Count()
***
***             Returns population count of the current bit vector.
***
***         INT Least_Non_Zero()
***
***             Returns least significant non-zero bit or -1 if there is none.
***
***         BOOL operator ==(const BIT_VECTOR &bv1) const 
***         BOOL operator !=(const BIT_VECTOR &bv1) const 
***         BIT_VECTOR& operator &(const BIT_VECTOR &bv1) const 
***         BIT_VECTOR& operator |(const BIT_VECTOR &bv1) const
***         BIT_VECTOR& operator -(const BIT_VECTOR &bv1) const
***         BIT_VECTOR& operator ~() const
***         BIT_VECTOR& operator =(const BIT_VECTOR &bv1)
***         BIT_VECTOR& operator &=(const BIT_VECTOR &bv1)
***         BIT_VECTOR& operator |=(const BIT_VECTOR &bv1)
***
***             Operators with nature interpretations.
***
***	    BOOL Intersects(BIT_VECTOR *bv)
***
***		Does bv intersect with this
***
**/

#ifndef lno_bv_RCS_ID
#define lno_bv_RCS_ID
#ifdef _KEEP_RCS_ID
static char *lno_bv_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */
#endif

#ifndef _lno_bv_INCLUDED
#define _lno_bv_INCLUDED

#include "defs.h"
#include "cxx_memory.h"

#define UINT64_size 64

// The following class implements bit vectors of arbitrary length.
// Interface are provided to do bit set, bit test, pop count and
// bit logical operations such as & and |.
class BIT_VECTOR {
  UINT _size;	// number of bits
  UINT64* _bv;
  MEM_POOL *_pool;
public:
  BIT_VECTOR() { _size=0; _pool=NULL; _bv=NULL; }
  ~BIT_VECTOR() {}
  BIT_VECTOR(UINT size, MEM_POOL *pool) {
    _size=size;
    _bv=CXX_NEW_ARRAY(UINT64, _size/UINT64_size+1, pool);
    for (INT i=_size-1; i>=0; i-=64)
      _bv[i/UINT64_size] = 0;
    _pool=pool;
  }
  UINT Size()  const  { return _size; };
  void Init(UINT size, MEM_POOL *pool) {
    _size=size;
    _bv=CXX_NEW_ARRAY(UINT64, _size/UINT64_size+1, pool);
    for (INT i=_size-1; i>=0; i-=64)
      _bv[i/UINT64_size] = 0;
    _pool=pool;
  }
  void Set(UINT bit_position);
  void Reset(UINT bit_position);
  BOOL Test(UINT bit_position);
  INT Least_Non_Zero();
  void Print(FILE *fp) {
    for (INT i=_size-1; i>=0; i-=64)
      fprintf(fp,"\t0x%16.16llX\n", _bv[i/UINT64_size]);
  }
  BOOL Intersects(BIT_VECTOR *bv) const {
    Is_True(bv->_size==_size, 
      ("Uncomformable sets in BIT_VECTOR::Intersects\nn"));
    for (INT i=_size-1; i>=0; i-=64)
      if (_bv[i/UINT64_size] & bv->_bv[i/UINT64_size])
        return TRUE;
    return FALSE;
  }
  UINT Pop_Count();
  BOOL operator ==(const BIT_VECTOR &bv1) const {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::==().\n"));
    for (INT i=_size-1; i>=0; i-=64)
      if (_bv[i/UINT64_size] != bv1._bv[i/UINT64_size])
        return FALSE;
    return TRUE;
  }
  BOOL operator !=(const BIT_VECTOR &bv1) const {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::!=().\n"));
    for (INT i=_size-1; i>=0; i-=64)
      if (_bv[i/UINT64_size] != bv1._bv[i/UINT64_size])
        return TRUE;
    return FALSE;
  }
  BIT_VECTOR& operator &(const BIT_VECTOR &bv1) const {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::&().\n"));
    BIT_VECTOR* r_bv=CXX_NEW(BIT_VECTOR, _pool);
    r_bv->Init(_size,_pool);
    for (INT i=_size-1; i>=0; i-=64) {
      r_bv->_bv[i/UINT64_size] = 
        _bv[i/UINT64_size] & bv1._bv[i/UINT64_size];
    }
    return *r_bv;
  }
  BIT_VECTOR& operator |(const BIT_VECTOR &bv1) const {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::|().\n"));
    BIT_VECTOR* r_bv=CXX_NEW(BIT_VECTOR, _pool);
    r_bv->Init(_size,_pool);
    for (INT i=_size-1; i>=0; i-=64)
      r_bv->_bv[i/UINT64_size] = 
        _bv[i/UINT64_size] | bv1._bv[i/UINT64_size];
    return *r_bv;
  }
  BIT_VECTOR& operator -(const BIT_VECTOR &bv1) const {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::-().\n"));
    BIT_VECTOR* r_bv=CXX_NEW(BIT_VECTOR, _pool);
    r_bv->Init(_size,_pool);
    for (INT i=_size-1; i>=0; i-=64)
      r_bv->_bv[i/UINT64_size] = 
        _bv[i/UINT64_size] & ~bv1._bv[i/UINT64_size];
    return *r_bv;
  }
  BIT_VECTOR& operator =(const BIT_VECTOR &bv1) {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::=().\n"));
    for (INT i=_size-1; i>=0; i-=64)
      _bv[i/UINT64_size] = bv1._bv[i/UINT64_size];
    return *this;
  }
  BIT_VECTOR& operator ~() const {
    BIT_VECTOR* r_bv=CXX_NEW(BIT_VECTOR, _pool);
    r_bv->Init(_size,_pool);
    for (INT i=_size-1; i>=0; i-=64)
      r_bv->_bv[i/UINT64_size]= ~_bv[i/UINT64_size];
    return *r_bv;
  }
  BIT_VECTOR& operator &=(const BIT_VECTOR &bv1) {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::&=().\n"));
    for (INT i=_size-1; i>=0; i-=64)
      _bv[i/UINT64_size] &= bv1._bv[i/UINT64_size];
    return *this;
  }
  BIT_VECTOR& operator |=(const BIT_VECTOR &bv1) {
    Is_True(bv1._size==_size, ("Uncomformable sets in BIT_VECTOR::|=().\n"));
    for (INT i=_size-1; i>=0; i-=64)
      _bv[i/UINT64_size] |= bv1._bv[i/UINT64_size];
    return *this;
  }
};

#endif		// _lno_bv_INCLUDED

