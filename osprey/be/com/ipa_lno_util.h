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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef ipa_lno_util_INCLUDED
#define ipa_lno_util_INCLUDED

/** Exported types and functions:
***
***     INT32 Gcd(mINT32, mINT32)
***     INT64 Gcd(mINT64, mINT64)
***     INT32 Lcm(mINT32, mINT32)
***     INT64 Lcm(mINT64, mINT64)
***
***         Find the gcd and lcm of pairs.  May be positive, negative or zero.
***         gcd(0,x) = x.  lcm(0,x) = 0.
***
***     INT32 Gcd(const mINT32* vector, INT count)
***     INT64 Gcd(const mINT64* vector, INT count)
***     INT32 Lcm(const mINT32* vector, INT count)
***     INT64 Lcm(const mINT64* vector, INT count)
***
***         Find the gcd and lcm of vectors of integers.  Count must be
***         at least one.  The vectors may contain 0, positive or negatives.
**/

extern INT32 Gcd(mINT32, mINT32);
extern INT64 Gcd(mINT64, mINT64);
extern INT32 Gcd(const mINT32*, INT);
extern INT64 Gcd(const mINT64*, INT);

extern INT32 Lcm(mINT32, mINT32);
extern INT64 Lcm(mINT64, mINT64);
extern INT32 Lcm(const mINT32*, INT);
extern INT64 Lcm(const mINT64*, INT);

#endif
