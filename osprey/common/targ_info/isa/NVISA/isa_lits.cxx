/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

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

*/


//
// Generate a list of literal classes and values for the ISA.
/////////////////////////////////////////////////////////

#include <stddef.h>
#include "isa_lits_gen.h"

main ()
{
  ISA_Lits_Begin();

  // ISA_Create_Lit_Class(name, type, [range,...] LIT_RANGE_END)
  ISA_Create_Lit_Class("i8",  SIGNED, SignedBitRange(8), LIT_RANGE_END);
  ISA_Create_Lit_Class("i16", SIGNED, SignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("i32", SIGNED, SignedBitRange(32), LIT_RANGE_END);
  ISA_Create_Lit_Class("i64", SIGNED, SignedBitRange(64), LIT_RANGE_END);

  ISA_Create_Lit_Class("u8",  UNSIGNED, UnsignedBitRange(8), LIT_RANGE_END);
  ISA_Create_Lit_Class("u16", UNSIGNED, UnsignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("u32", UNSIGNED, UnsignedBitRange(32), LIT_RANGE_END);
  ISA_Create_Lit_Class("u64", UNSIGNED, UnsignedBitRange(64), LIT_RANGE_END);

  // floating point literals:  fake as integer bit sizes
  ISA_Create_Lit_Class("f32", UNSIGNED, UnsignedBitRange(32), LIT_RANGE_END);
  ISA_Create_Lit_Class("f64", UNSIGNED, UnsignedBitRange(64), LIT_RANGE_END);

  ISA_Lits_End();
}
