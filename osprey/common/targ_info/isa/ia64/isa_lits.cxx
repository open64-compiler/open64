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
  LIT_RANGE r_1, r_4, r_8, r_16;
  LIT_RANGE r0, r1, r4, r7, r8, r15, r16;
  LIT_RANGE r_127to128, r_128ULLto_1ULL, r_127ULLto_1ULL, r_128Uto_1U, 
	    r_127Uto_1U;
  LIT_RANGE r1to4, r1to3, r1to16, r1to64, r0to127, r1to128;

  ISA_Lits_Begin();

#define TWO32 (1ULL<<32)
#define TWO64 (0)

  r_1 = ISA_Create_Lit_Range("-1", -1, -1);
  r_4 = ISA_Create_Lit_Range("-4", -4, -4);
  r_8 = ISA_Create_Lit_Range("-8", -8, -8);
  r_16 = ISA_Create_Lit_Range("-16", -16, -16);
  r0 = ISA_Create_Lit_Range("0", 0, 0);
  r1 = ISA_Create_Lit_Range("1", 1, 1);
  r4 = ISA_Create_Lit_Range("4", 4, 4);
  r7 = ISA_Create_Lit_Range("7", 7, 7);
  r8 = ISA_Create_Lit_Range("8", 8, 8);
  r15 = ISA_Create_Lit_Range("15", 15, 15);
  r16 = ISA_Create_Lit_Range("16", 16, 16);
  r1to4 = ISA_Create_Lit_Range("1..4", 1, 4);
  r1to3 = ISA_Create_Lit_Range("1..3", 1, 3);
  r1to16 = ISA_Create_Lit_Range("1..16", 1, 16);
  r1to64 = ISA_Create_Lit_Range("1..64", 1, 64);
  r0to127 = ISA_Create_Lit_Range("0..127", 0, 127);
  r1to128 = ISA_Create_Lit_Range("1..128", 1, 128);
  r_127to128 = ISA_Create_Lit_Range("-127..128", -127, 128);
  r_128ULLto_1ULL = ISA_Create_Lit_Range("2^64-128..2^64-1", TWO64-128, TWO64-1);
  r_127ULLto_1ULL = ISA_Create_Lit_Range("2^64-127..2^64", TWO64-127, TWO64-1);
  r_128Uto_1U = ISA_Create_Lit_Range("2^32-128..2^32-1", TWO32-128, TWO32-1);
  r_127Uto_1U = ISA_Create_Lit_Range("2^32-127..2^32", TWO32-127, TWO32);

  // ISA_Create_Lit_Class(name, type, [range,...] LIT_RANGE_END)
  ISA_Create_Lit_Class("i1",  SIGNED, SignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("i8",  SIGNED, SignedBitRange(8), LIT_RANGE_END);
  ISA_Create_Lit_Class("i9",  SIGNED, SignedBitRange(9), LIT_RANGE_END);
  ISA_Create_Lit_Class("i13", SIGNED, SignedBitRange(13), LIT_RANGE_END);
  ISA_Create_Lit_Class("i14", SIGNED, SignedBitRange(14), LIT_RANGE_END);
  ISA_Create_Lit_Class("i16", SIGNED, SignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("i17", SIGNED, SignedBitRange(17), LIT_RANGE_END);
  ISA_Create_Lit_Class("i22", SIGNED, SignedBitRange(22), LIT_RANGE_END);
  ISA_Create_Lit_Class("i25", SIGNED, SignedBitRange(25), LIT_RANGE_END);
  ISA_Create_Lit_Class("i44", SIGNED, SignedBitRange(44), LIT_RANGE_END);
  ISA_Create_Lit_Class("i64", SIGNED, SignedBitRange(64), LIT_RANGE_END);

  ISA_Create_Lit_Class("k2",  UNSIGNED, UnsignedBitRange(2), LIT_RANGE_END);
  ISA_Create_Lit_Class("k4",  UNSIGNED, UnsignedBitRange(4), LIT_RANGE_END);
  ISA_Create_Lit_Class("k5",  UNSIGNED, UnsignedBitRange(5), LIT_RANGE_END);
  ISA_Create_Lit_Class("k6",  UNSIGNED, UnsignedBitRange(6), LIT_RANGE_END);
  ISA_Create_Lit_Class("k7",  UNSIGNED, UnsignedBitRange(7), LIT_RANGE_END);
  ISA_Create_Lit_Class("k8",  UNSIGNED, UnsignedBitRange(8), LIT_RANGE_END);
  ISA_Create_Lit_Class("k9",  UNSIGNED, UnsignedBitRange(9), LIT_RANGE_END);
  ISA_Create_Lit_Class("k21", UNSIGNED, UnsignedBitRange(21), LIT_RANGE_END);
  ISA_Create_Lit_Class("k24", UNSIGNED, UnsignedBitRange(24), LIT_RANGE_END);
  ISA_Create_Lit_Class("k62", UNSIGNED, UnsignedBitRange(62), LIT_RANGE_END);

  ISA_Create_Lit_Class("pmpyshr2", UNSIGNED, r0, r7, r15, r16, LIT_RANGE_END);
  ISA_Create_Lit_Class("fetchadd", SIGNED, r_16, r_8, r_4, r_1, r1, r4, r8, r16,
					   LIT_RANGE_END);
  ISA_Create_Lit_Class("shfadd", UNSIGNED, r1to4, LIT_RANGE_END);
  ISA_Create_Lit_Class("pshfadd", UNSIGNED, r1to3, LIT_RANGE_END);
  ISA_Create_Lit_Class("len4", UNSIGNED, r1to16, LIT_RANGE_END);
  ISA_Create_Lit_Class("len6", UNSIGNED, r1to64, LIT_RANGE_END);
  ISA_Create_Lit_Class("scmp", SIGNED, r_127to128, LIT_RANGE_END);
  ISA_Create_Lit_Class("ucmp1", UNSIGNED, r0to127, r_128ULLto_1ULL, LIT_RANGE_END);
  ISA_Create_Lit_Class("ucmp2", UNSIGNED, r1to128, r_127ULLto_1ULL, LIT_RANGE_END);
  ISA_Create_Lit_Class("ucmp3", UNSIGNED, r0to127, r_128Uto_1U, LIT_RANGE_END);
  ISA_Create_Lit_Class("ucmp4", UNSIGNED, r1to128, r_127Uto_1U, LIT_RANGE_END);

  ISA_Lits_End();
}
