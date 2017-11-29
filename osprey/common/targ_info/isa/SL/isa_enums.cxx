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
// Generate a list of enumeration classes and values for the ISA.
/////////////////////////////////////////////////////////

#include <stddef.h>
#include "isa_enums_gen.h"

main ()
{
  ISA_Enums_Begin();

  ISA_Create_Enum_Class ("pfhint",
      "L2_store",		1,
      "L2_load",		0,
      "L1_store",		3,
      "L1_load",		2,
      NULL,	UNDEFINED);	// default value

  ISA_Enums_End();
}

