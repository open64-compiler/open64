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

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include "frac.h"
#include "lnoutils.h"
#include "ipa_lno_util.h"

BOOL FRAC::Exception = FALSE;

// ********************* Fraction reduction

void FRAC::_reduce2()
{
  if (_d == 0) {
    if (Exception == FALSE) {
      DevWarn("_reduce2() passed zero denominator");
      Exception = TRUE;
    }
    _d = 1;
  }

  // neither is zero
  BOOL neg = _n < 0 ? _d > 0 : _d < 0;

  FRAC_ETY i1 = _n < 0 ? -_n : _n;
  FRAC_ETY i2 = _d < 0 ? -_d : _d;
  FRAC_ETY g = Gcd(i1, i2);

  *this = _checksz((neg ? -i1/g : i1/g), (i2/g));
}

void FRAC::_reduce2(FRAC_ITY n, FRAC_ITY d)
{
  if (_d == 0) {
    if (Exception == FALSE) {
      DevWarn("_reduce2() passed zero denominator");
      Exception = TRUE;
    }
    _d = 1;
  }

  // neither is zero
  INT8 neg = n < 0 ? d > 0 : d < 0;

  FRAC_ITY i1 = n < 0 ? -n : n;
  FRAC_ITY i2 = d < 0 ? -d : d;
  FRAC_ITY g = Gcd(i1, i2);

  *this = _checksz((neg ? -i1/g : i1/g), (i2/g));
}

// ********************* Fraction operations

FRAC FRAC::operator +(FRAC f) const
{
  FRAC_ITY n = (FRAC_ITY) _n * f._d + (FRAC_ITY) f._n * _d;
  FRAC_ITY d = (FRAC_ITY) _d * f._d;
  return FRAC(_checksz(n), _checksz(d));
}

FRAC FRAC::operator -(FRAC f) const
{
  FRAC_ITY n = (FRAC_ITY) _n * f._d - (FRAC_ITY) f._n * _d;
  FRAC_ITY d = (FRAC_ITY) _d * f._d;
  return FRAC(_checksz(n), _checksz(d));
}

FRAC FRAC::operator *(FRAC f) const
{
  FRAC_ITY n = (FRAC_ITY) _n * f._n;
  FRAC_ITY d = (FRAC_ITY) _d * f._d;
  return FRAC(_checksz(n), _checksz(d));
}

FRAC FRAC::operator /(FRAC f) const
{
  // the constructor catches division by zero
  FRAC_ITY n = (FRAC_ITY) _n * f._d;
  FRAC_ITY d = (FRAC_ITY) _d * f._n;
  return FRAC(_checksz(n), _checksz(d));
}

// ********************* Fraction printing

void FRAC::Print(FILE* f) const
{
  if (_d == 1)
    fprintf(f, " %d ", _n);
  else
    fprintf(f, "%d/%d", _n, _d);
}

char* FRAC::Print(char* s) const
{
  if (_d == 1)
    sprintf(s, " %d ", _n);
  else
    sprintf(s, "%d/%d", _n, _d);

  return s + ::strlen(s);
}

