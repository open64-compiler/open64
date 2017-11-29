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
// Generate a list of PROCESSORS.
/////////////////////////////////////

#include <stddef.h>
#include "proc_gen.h"

main ()
{
  PROC_Create ( "MIPS",
#ifdef TARG_SL
        "sl1_pcore",
        "sl1_dsp",
        "sl2_mcore",
        "sl2_pcore",
        "sl5",
#else
        "r10000",
        "sb1",
#endif
	NULL
  );
}

