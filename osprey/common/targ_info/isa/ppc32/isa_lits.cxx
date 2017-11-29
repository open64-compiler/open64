/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
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

  ISA_Create_Lit_Class("simm16", SIGNED,   SignedBitRange(16),   LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm16", UNSIGNED, UnsignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm5",  UNSIGNED, UnsignedBitRange(5),  LIT_RANGE_END);
  ISA_Create_Lit_Class("pc14",   SIGNED,   SignedBitRange(14),   LIT_RANGE_END);
  ISA_Create_Lit_Class("pcrel16", SIGNED,  SignedBitRange(16),   LIT_RANGE_END);
  ISA_Create_Lit_Class("pc26",   UNSIGNED, UnsignedBitRange(26), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm10", UNSIGNED, UnsignedBitRange(10), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm3",  UNSIGNED, UnsignedBitRange(3),  LIT_RANGE_END);

#ifdef TARG_JAVI
  ISA_Create_Lit_Class("uimm1", UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm2", UNSIGNED, UnsignedBitRange(2), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm3", UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm4", UNSIGNED, UnsignedBitRange(4), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm7", UNSIGNED, UnsignedBitRange(7), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm9", UNSIGNED, UnsignedBitRange(9), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm10",UNSIGNED, UnsignedBitRange(10),LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm11",UNSIGNED, UnsignedBitRange(11),LIT_RANGE_END);  
  ISA_Create_Lit_Class("uimm14",UNSIGNED, UnsignedBitRange(14),LIT_RANGE_END);
  ISA_Create_Lit_Class("uid2",  UNSIGNED, UnsignedBitRange(2), LIT_RANGE_END);
  ISA_Create_Lit_Class("uid3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uia1",  UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uia3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uib1",  UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uib3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uic1",  UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uic3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  
#endif  

  ISA_Lits_End();
}
