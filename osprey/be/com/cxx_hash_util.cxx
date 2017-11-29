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
// ====================================================================
// ====================================================================
//
// Module: cxx_hash_util.cxx
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:35-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_hash_util.cxx $
//
// Revision history:
//  07-Dec-95 - Merged user-hash version from IPA
//
// Description:
//
// Non-template support for template hash map implementations in
// cxx_hash.h/cxx_hash.cxx.
//
// ====================================================================
// ====================================================================

#ifdef _KEEP_RCS_ID
#define cxx_hash_util_CXX      "cxx_hash_util.cxx"
static char *rcs_id = cxx_hash_util_CXX" $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "erglob.h"
#include "cxx_hash.h"

// ====================================================================
// ====================================================================
//
// String hash table support:
//
// Borrows from Bob Fraley's hash map STL implementation...
//
// ====================================================================
// ====================================================================

// Utility table for hashing:
static HASH Init_Hashing_Table();
static HASH Hashing_Table[256];
static HASH _dummy_var = Init_Hashing_Table();

// Initialize a "random" table for hashing function:
static HASH
Init_Hashing_Table ( void )
{
  // randomize(); //
  unsigned i;
  for ( i = 0; i < 256; i++ ) {
    Hashing_Table[i] = i | (i << 8);   // >>> i for orig algorithm <<<
  }
  for ( i = 0; i < 256; ++i ) {
    HASH r = (rand() >> 5) & 255;
    HASH xchg = Hashing_Table[i];
    Hashing_Table[i] = Hashing_Table[r];
    Hashing_Table[r] = xchg;
  }
  return 1;
}

// Modified hash function:
HASH
String_Hash::operator() ( const char *  k ) const
{
  HASH res = 0;

  while ( *k ) {
    res = Hashing_Table[(res & 255) ^ *k++] ^ (res << 3);
  }
  return res ;
}

