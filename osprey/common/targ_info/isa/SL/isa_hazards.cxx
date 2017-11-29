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
//  Generate ISA hazard information
//  
//  Generate ISA hazard information
//
//  Note:  A "hazard" is a conflict between instructions which may
//  cause unexpected behavior if the instructions appear at a forbidden
//  distance in the instruction stream.  That is, the hardware does not
//  enforce the required separation, and the software is responsible
//  for avoiding such situations.  Most are avoided via the resource
//  mechanism.  This file handles special cases.
//
///////////////////////////////////////////////////////////////////////

//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:18:01 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa_hazards.cxx,v $


#include <stddef.h>
#include "topcode.h"
#include "targ_isa_subset.h"
#include "isa_hazards_gen.h"


main()
{
  ISA_HAZARD
    result,
    operand,
    errata;

  ISA_Hazards_Begin("MIPS");

  result = Hazard_Create("result");
  operand = Hazard_Create("operand");
  errata = Hazard_Create("errata");

  ISA_Hazards_End();
  return 0;
}
