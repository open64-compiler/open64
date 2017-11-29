/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

//
// Generate a list of literal classes and values for the ISA.
/////////////////////////////////////////////////////////

#include "isa_lits_gen.h"

main ()
{
  ISA_Lits_Begin();

  ISA_Create_Lit_Class("simm16", SIGNED, SignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm16", UNSIGNED, UnsignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm5", UNSIGNED, UnsignedBitRange(5), LIT_RANGE_END);
  ISA_Create_Lit_Class("pcrel16", SIGNED, SignedBitRange(16), LIT_RANGE_END);
  ISA_Create_Lit_Class("pc26", UNSIGNED, UnsignedBitRange(26), LIT_RANGE_END);

#ifdef TARG_SL
  ISA_Create_Lit_Class("uimm1", UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm2", UNSIGNED, UnsignedBitRange(2), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm3", UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm4", UNSIGNED, UnsignedBitRange(4), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm7", UNSIGNED, UnsignedBitRange(7), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm8", UNSIGNED, UnsignedBitRange(8), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm9", UNSIGNED, UnsignedBitRange(9), LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm10",UNSIGNED, UnsignedBitRange(10),LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm11",UNSIGNED, UnsignedBitRange(11),LIT_RANGE_END);  
  ISA_Create_Lit_Class("uimm14",UNSIGNED, UnsignedBitRange(14),LIT_RANGE_END);
  ISA_Create_Lit_Class("uimm15",UNSIGNED, UnsignedBitRange(15),LIT_RANGE_END);
  ISA_Create_Lit_Class("uid2",  UNSIGNED, UnsignedBitRange(2), LIT_RANGE_END);
  ISA_Create_Lit_Class("uid3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uia1",  UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uia3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uib1",  UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uib3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("uic1",  UNSIGNED, UnsignedBitRange(1), LIT_RANGE_END);
  ISA_Create_Lit_Class("uic3",  UNSIGNED, UnsignedBitRange(3), LIT_RANGE_END);
  ISA_Create_Lit_Class("simm5", SIGNED,   SignedBitRange(5),   LIT_RANGE_END);
  ISA_Create_Lit_Class("simm7", SIGNED,   SignedBitRange(7),   LIT_RANGE_END);
  ISA_Create_Lit_Class("pc5",   SIGNED,   SignedBitRange(5),   LIT_RANGE_END);
  ISA_Create_Lit_Class("simm9", SIGNED, SignedBitRange(9), LIT_RANGE_END);
  ISA_Create_Lit_Class("simm10", SIGNED, SignedBitRange(10), LIT_RANGE_END);
#endif  

  ISA_Lits_End();
}
