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
***	This is a fraction class.  Numerators and denominators may be
***	as large as FRAC_MAX.  Currently, a FRAC uses
***	64 total bits, 32 for the numerator and 32 for the denominator.
***     Fractions are always reduced: there is no such thing as
***	8/12, for example.  If a fraction numerator or denominator
***     exceeds FRAC_MAX, or if division by zero is about to occur,
***     we set FRAC::Exception to TRUE and emit a DevWarn.
***     We then do something arbitrary, but that well not crash.
***     Thus by setting FRAC::Exception to FALSE before a large computation
***     and testing its value after, we can see whether we obtained the
***     correct result.  Note that FRAC_MAX could be set as large as
***     INT32_MAX, but is set lower during testing to aid in finding
***     potential computation problems.
***
***	A note on memory pools: there is no need for memory pools, because
***	new and delete never are called by the constructor and destructor.
***
*** Reserved Prefix:
***
***	FRAC
***
*** Exported Type:
***
***	FRAC
***
***	    The fraction type.
***
*** Exported Functions:
***
***	FRAC::FRAC()
***	FRAC::FRAC(INT32 n)
***	FRAC::FRAC(INT32 n, INT32 d)
***	FRAC::FRAC(const FRAC& f)
***	~FRAC::FRAC()
***
***	    Construct a fraction of value 0, n, n/d and another fraction
***	    respectively, and destruct a fraction.
***
***
***	FRAC& FRAC::operator =(const FRAC& f);
***
***	    Overwrite one fraction with another.
***
***	BOOL FRAC::operator ==(FRAC f) const
***	BOOL FRAC::operator !=(FRAC f) const
***	BOOL FRAC::operator <(FRAC f) const
***	BOOL FRAC::operator <=(FRAC f) const
***	BOOL FRAC::operator >(FRAC f) const
***	BOOL FRAC::operator >=(FRAC f) const
***
***	    Comparisons between fractions.
***
***	FRAC_ETY FRAC::Integer() const
***	double FRAC::Real() const
***
***	    Conversions.  (Integer() asserts failure if not integral.)
***
***	FRAC  FRAC::operator +(FRAC f) const
***	FRAC  FRAC::operator -(FRAC f) const
***	FRAC  FRAC::operator *(FRAC f) const
***	FRAC  FRAC::operator /(FRAC f) const
***	FRAC& FRAC::operator +=(FRAC f)
***	FRAC& FRAC::operator -=(FRAC f)
***	FRAC& FRAC::operator *=(FRAC f)
***	FRAC& FRAC::operator /=(FRAC f)
***	FRAC  FRAC::operator -() const			(unary)
***	FRAC  FRAC::Abs() const
***	extern FRAC Abs(FRAC);
***
***	    Standard arithmetic functions on fractions.  The assignment
***	    operators return the updated right-hand-side.
***
***	void FRAC::Print(FILE* f) const
***	char* FRAC::Print(char* f) const
***
***	    Write a fraction to a file or string.  If a string, return the
***	    end of the string (the address of the written null byte).
***
***     static BOOL FRAC::Exception;
***
***         Set to TRUE whenever the numerator or denominator exceeds
***         FRAC_MAX in absolute value, or whenever we might have divided
***         by zero or otherwise obtain an incorrect answer.
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef frac_INCLUDED
#define frac_INCLUDED

extern "C" {
#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif
}

// note that 16/32 bit implementations are equally possible, possibly faster,
// definitely untested.  Could have a template for this, but let's keep away
// from those where possible.  Actually, could have a macro as well, but this
// will be good enough.

#define FRAC_ETY	mINT32		/* element type */
#define FRAC_ITY	mINT64		/* intermediate result type, bigger */

#ifdef Is_True_On
/* better to have a smaller number when testing, to point out problems */
#define FRAC_MAX	1000000
#else
/* better to have a larger number in production -- safer. */
#define FRAC_MAX	INT32_MAX
#endif

class FRAC {
 public:
  static BOOL Exception;

  FRAC() {_n = 0; _d = 1;}
  FRAC(FRAC_ETY n) : _n(_checksz(n)), _d(1) {}
  FRAC(FRAC_ETY n, FRAC_ETY d) : _n(n), _d(d) {_reduce();}
  FRAC(const FRAC& f) : _n(f._n), _d(f._d) {}
  // ~FRAC() {} /* forces a slower vector deallocator -- very bad */

  FRAC_ETY N() const {return _n;}
  FRAC_ETY D() const {return _d;}

  FRAC& operator =(const FRAC& f) {_n = f._n; _d = f._d; return *this;}

  BOOL		operator ==(FRAC f) const
    {return _n == f._n && _d == f._d;}
  BOOL		operator !=(FRAC f) const
    {return _n != f._n || _d != f._d;}
  BOOL		operator <(FRAC f) const
    {return FRAC_ITY(_n)*f._d < FRAC_ITY(f._n)*_d;}
  BOOL		operator <=(FRAC f) const
    {return FRAC_ITY(_n)*f._d <= FRAC_ITY(f._n)*_d;}
  BOOL		operator >(FRAC f) const
    {return FRAC_ITY(_n)*f._d > FRAC_ITY(f._n)*_d;}
  BOOL		operator >=(FRAC f) const
    {return FRAC_ITY(_n)*f._d >= FRAC_ITY(f._n)*_d;}

  FRAC_ETY	Integer() const
    {FmtAssert(_d == 1, ("int(%d/%d)", _n, _d)); return _n;}
  double	Real() const
    {return (double(_n))/_d;}

  FRAC		operator +(FRAC f) const;
  FRAC&		operator +=(FRAC f) {return (*this) = (*this) + f;}
  FRAC		operator -(FRAC f) const;
  FRAC&		operator -=(FRAC f) {return (*this) = (*this) - f;}
  FRAC		operator *(FRAC f) const;
  FRAC&		operator *=(FRAC f) {return (*this) = (*this) * f;}
  FRAC		operator /(FRAC f) const;
  FRAC&		operator /=(FRAC f) {return (*this) = (*this) / f;}
  FRAC		operator -() const {return FRAC(-_n, _d, 0);}

  FRAC		Abs() const {return FRAC(_n < 0 ? -_n : _n, _d, 0);}
  friend FRAC	Abs(FRAC f) {return f.Abs();}

  void		Print(FILE* f) const;
  char*		Print(char* f) const;

 private:

  FRAC(FRAC_ITY n, FRAC_ITY d) {_reduce(n, d);}
  FRAC(FRAC_ETY n, FRAC_ETY d, INT32) : _n(n), _d(d) {} // if don't need reduce

  // entry point for reducing fractions
  // probably too much inlining, but much will be constant folded away

  void _reduce() {
    if (_d == 1)
      _n = _checksz(_n);
    else if (_n == 0) {
      _d = 1;
    }
    else if (_n == 1 || _n == -1) {
      _n = _d < 0 ? -_n : _n;
      _d = _checksz(_d < 0 ? -_d : _d);
    }
    else if (_d == -1) {
      _n = _checksz(-_n);
      _d = -_d;
    }
    else
      _reduce2();
  }

  // entry point for reducing intermediate expressions
  void _reduce(FRAC_ITY n, FRAC_ITY d) {
    if (d == 1) {
      _n = _checksz(n);
      _d = 1;
    }
    else if (n == 0) {
      _n = 0;
      _d = 1;
    }
    else if (n == 1 || n == -1) {
      _n = d < 0 ? FRAC_ETY(-n) : FRAC_ETY(n);
      _d = _checksz(d < 0 ? -d : d);
    }
    else if (d == -1) {
      _n = _checksz(-n);
      _d = 1;
    }
    else
      _reduce2(n, d);
  }

  // general reduce, for when _d and _n are not -1, 0, 1.
  // called by _reduce() only

  void _reduce2();
  void _reduce2(FRAC_ITY n, FRAC_ITY d);

  // use these to warn if fractions aren't doing well, and doing
  // the best thing possible from there
  static FRAC_ETY _checksz(FRAC_ETY x) {
    if (!_sz_ok(x)) {
      if (Exception == FALSE) {
        DevWarn("FRAC component too big: %d", x);
        Exception = TRUE;
      }
      x = MIN(x, FRAC_MAX);
      x = MAX(x, -FRAC_MAX);
    }
    return x;
  }
  static FRAC_ETY _checksz(FRAC_ITY x) {
    if (!_sz_ok(x)) {
      if (Exception == FALSE) {
        DevWarn("FRAC component too big: %lld", x);
        Exception = TRUE;
      }
      x = MIN(x, FRAC_MAX);
      x = MAX(x, -FRAC_MAX);
    }
    return FRAC_ETY(x);
  }
  static FRAC _checksz(FRAC_ETY n, FRAC_ETY d) {
    FRAC   f;
    if (!_sz_ok(n) || !_sz_ok(d)) {
      if (Exception == FALSE) {
        DevWarn("FRAC component too big: %d/%d", n, d);
        Exception = TRUE;
      }
      f._n = d == 0 ? 1 : n/d;
      f._d = 1;
    }
    else {
      f._n = n;
      f._d = d;
    }
    return f;
  }
  static FRAC _checksz(FRAC_ITY n, FRAC_ITY d) {
    FRAC   f;
    if (!_sz_ok(n) || !_sz_ok(d)) {
      if (Exception == FALSE) {
        DevWarn("FRAC component too big: %lld/%lld", n, d);
        Exception = TRUE;
      }
      f._n = d == 0 ? 1 : n/d;
      f._d = 1;
    }
    else {
      f._n = n;
      f._d = d;
    }
    return f;
  }

  // _sz_ok: return whether components are too big
  static BOOL _sz_ok(FRAC_ITY x) {
    return (x <= FRAC_MAX && x >= -FRAC_MAX);
  }
  static BOOL _sz_ok(FRAC_ITY n, FRAC_ITY d) {
    return _sz_ok(n) && _sz_ok(d);
  }
  static BOOL _sz_ok(FRAC_ETY x) {
    return (x <= FRAC_MAX && x >= -FRAC_MAX);
  }
  static BOOL _sz_ok(FRAC_ETY n, FRAC_ETY d) {
    return _sz_ok(n) && _sz_ok(d);
  }

  FRAC_ETY	_n;	// any integral value
  FRAC_ETY	_d;	// must be positive.  signed so FRAC_ITY easy
};
#endif
