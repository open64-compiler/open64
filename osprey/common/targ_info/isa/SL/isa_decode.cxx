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
// Generate instruction decoding information.
/////////////////////////////////////
/////////////////////////////////////
//
//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:18:01 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa_decode.cxx,v $

#include "topcode.h"
#include "isa_decode_gen.h"
#include "targ_isa_bundle.h"
 
main()
{
  ISA_Decode_Begin("MIPS");

  STATE unit = Create_Unit_State("unit", 0, 3);
  Initial_State(unit);

  ISA_Decode_End();
  return 0;
}
