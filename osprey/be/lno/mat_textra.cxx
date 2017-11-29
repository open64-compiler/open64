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
*** Matrix member functions and friends for which template functions are
*** not adequate.  Thus 'textra' => 'template extra'.
**/

/** $Revision: 1.6 $
*** $Date: 04/12/21 14:57:14-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.mat_textra.cxx $
**/

#define mat_textra_CXX      "mat_textra.cxx"
const static char *rcs_id =   mat_textra_CXX "$Revision: 1.6 $";


#include <stdint.h>
#include "lnopt_main.h"
#include "mat.h"
#include "lu_mat.h"

// ********************* Required for instantiation **********************

template <> MEM_POOL* MAT<mINT32>::_default_pool = NULL;
template <> MEM_POOL* MAT<FRAC>::_default_pool = NULL;
template <> MEM_POOL* MAT<double>::_default_pool = NULL;

// Integer matrix inversion can be performed by making the equivalent fraction
// matrix and inverting that.
// The alternative is to cast to doubles instead of fractions.
// That might well be faster, but may suffer from roundoff.  In either case,
// the routine crashes if the thing is not unimodular.
//
// The #if below chooses between the two.  #if 0 selects double rather than
// fractional inverse.


template<>
IMAT MAT<mINT32>::Inv() const
{
  FmtAssert(_r == _c, ("Matrix not square"));

  MEM_POOL* hold = DMAT::Set_Default_Pool(IMAT::Default_Pool());
  DMAT dmat = IMAT_to_DMAT(*this, IMAT::Default_Pool());
  dmat.D_Inv();
  DMAT::Set_Default_Pool(hold);
  return DMAT_to_IMAT(dmat, IMAT::Default_Pool());
}


// Factor and use the factoring inversion routines

template<>
FMAT MAT<FRAC>::Inv() const		// XXX compiler bug: confused by FMAT
{
  FmtAssert(_r == _c, ("FMAT::Inv(): Matrix not square"));
  LU_FMAT lu(*this, FMAT::Default_Pool());
  return lu.Inv();
}

// Factor and use the factoring inversion routines

template<>
DMAT MAT<double>::Inv() const		// XXX compiler bug: confused by DMAT
{
  FmtAssert(_r == _c, ("Matrix not square"));
  LU_DMAT lu(*this, DMAT::Default_Pool());
  return lu.Inv();
}

template<>
void DMAT::Print_Element(FILE* f, double e)
{
  fprintf(f, "%g", e);
}

template<>
void IMAT::Print_Element(FILE* f, mINT32 e)
{
  fprintf(f, "%d", e);
}

template<>
void FMAT::Print_Element(FILE* f, FRAC e)
{
  e.Print(f);
}

//************** other MAT special functions ************

IMAT FMAT_to_IMAT(const FMAT& a, MEM_POOL* pool)
{
  Is_True(IMAT::Default_Pool(), ("Missing default pool for IMAT"));

  IMAT	x(a.Rows(), a.Cols(), pool);

  for (INT32 r = 0; r < a.Rows(); r++)
    for (INT32 c = 0; c < a.Cols(); c++)
      x(r,c) = a(r,c).Integer();

  return x;
}

IMAT DMAT_to_IMAT(const DMAT& a, MEM_POOL* pool)
{
  Is_True(IMAT::Default_Pool(), ("Missing default pool for IMAT"));

  IMAT	x(a.Rows(), a.Cols(), pool);

  for (INT32 r = 0; r < a.Rows(); r++) {
    for (INT32 c = 0; c < a.Cols(); c++) {
      double d = a(r, c);
      BOOL neg = (d < 0);
      if (neg) d = -d;
      INT32 i = INT32(d+0.5);
      FmtAssert(d-i < 1e-10 && d-i > -1e-10, ("Bad floating inverse"));
      x(r,c) = neg ? -i : i;
    }
  }

  return x;
}

DMAT IMAT_to_DMAT(const IMAT& a, MEM_POOL* pool)
{
  Is_True(DMAT::Default_Pool(), ("Missing default pool for DMAT"));

  DMAT	x(a.Rows(), a.Cols(), pool);

  for (INT32 r = 0; r < a.Rows(); r++)
    for (INT32 c = 0; c < a.Cols(); c++)
      x(r,c) = double(a(r,c));

  return x;
}

FMAT IMAT_to_FMAT(const IMAT& a, MEM_POOL* pool)
{
  Is_True(FMAT::Default_Pool(), ("Missing default pool for FMAT"));

  FMAT	x(a.Rows(), a.Cols(), pool);

  for (INT32 r = 0; r < a.Rows(); r++)
    for (INT32 c = 0; c < a.Cols(); c++)
      x(r,c) = FRAC(a(r,c));

  return x;
}

