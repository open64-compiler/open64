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


//
// Generate a list of literal classes and values for the ISA.
/////////////////////////////////////////////////////////

#include "isa_lits_gen.h"

main ()
{
  ISA_Lits_Begin();

  ISA_Create_Lit_Class( "simm8",  SIGNED,   SignedBitRange(8),    LIT_RANGE_END );
  ISA_Create_Lit_Class( "uimm8",  UNSIGNED, UnsignedBitRange(8),  LIT_RANGE_END );
  ISA_Create_Lit_Class( "simm16", SIGNED,   SignedBitRange(16),   LIT_RANGE_END );
  ISA_Create_Lit_Class( "uimm16", UNSIGNED, UnsignedBitRange(16), LIT_RANGE_END );
  ISA_Create_Lit_Class( "simm32", SIGNED,   SignedBitRange(32),   LIT_RANGE_END );
  ISA_Create_Lit_Class( "uimm32", UNSIGNED, UnsignedBitRange(32), LIT_RANGE_END );
  ISA_Create_Lit_Class( "simm64", SIGNED,   SignedBitRange(64),   LIT_RANGE_END );
  ISA_Create_Lit_Class( "pcrel32",SIGNED,   SignedBitRange(32),   LIT_RANGE_END);

  ISA_Lits_End();
}
