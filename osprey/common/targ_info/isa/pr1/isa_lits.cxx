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


//
// Generate a list of literal classes and values for the ISA.
/////////////////////////////////////////////////////////

#include <stddef.h>
#include "isa_lits_gen.h"

main ()
{

  ISA_Lits_Begin();

#define TWO32 (1ULL<<32)
#define TWO64 (0)

  LIT_RANGE r_512to511 = ISA_Create_Lit_Range("-512..511", -512, 511);
  LIT_RANGE r_131072to131071 = ISA_Create_Lit_Range("-131072..131072", -131072, 131071);
  LIT_RANGE r_8388608to8388607 = ISA_Create_Lit_Range("-8388608..8388607", -8388608, 8388603);

  ISA_Create_Lit_Class("pr_i4", SIGNED, SignedBitRange(4), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i5", SIGNED, SignedBitRange(5), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i6", SIGNED, SignedBitRange(6), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i7", SIGNED, SignedBitRange(7), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i8", SIGNED, SignedBitRange(8), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i10", SIGNED, SignedBitRange(10), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i13", SIGNED, SignedBitRange(13), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i14", SIGNED, SignedBitRange(14), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i15", SIGNED, SignedBitRange(15), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i16", SIGNED, SignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i20", SIGNED, SignedBitRange(20), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i21", SIGNED, SignedBitRange(21), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i24", SIGNED, SignedBitRange(24), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_i32", SIGNED, SignedBitRange(32), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_u4", UNSIGNED, UnsignedBitRange(4), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_u5", UNSIGNED, UnsignedBitRange(5), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_u8", UNSIGNED, UnsignedBitRange(8), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_u10", UNSIGNED, UnsignedBitRange(10), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_u15", UNSIGNED, UnsignedBitRange(15), LIT_RANGE_END);
  ISA_Create_Lit_Class("pr_u32", UNSIGNED, UnsignedBitRange(32), LIT_RANGE_END);
 
  ISA_Lits_End();
}
