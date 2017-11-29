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

#ifndef targ_sim_INCLUDED
#define targ_sim_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* some definitions for the dedicated hardware pregs: */

#define Int_Preg_Min_Offset             1
#define Int_Preg_Max_Offset             31
#define Float_Preg_Min_Offset           32
#define Float_Preg_Max_Offset           63
#define Fcc_Preg_Min_Offset             64
#define Fcc_Preg_Max_Offset             71
#define Last_Dedicated_Preg_Offset      Fcc_Preg_Max_Offset

/* The offsets for return registers are fixed: */
#define First_Int_Preg_Return_Offset	2	/* register v0 */
#define Last_Int_Preg_Return_Offset	3	/* register v1 */
#define First_Float_Preg_Return_Offset	32	/* register f0 */
#define Last_Float_Preg_Return_Offset	34	/* register f2 */
#define Stack_Pointer_Preg_Offset	29	/* register sp */
#define Frame_Pointer_Preg_Offset	30	/* register fp */
#define First_Int_Preg_Param_Offset	4	/* register a0 */
#define First_Float_Preg_Param_Offset	44	/* register f12 */
#define Static_Link_Preg_Offset		2

#define MAX_NUMBER_OF_REGISTERS_FOR_RETURN 2
#define MAX_NUMBER_OF_REGISTER_PARAMETERS 8

/* most of the interface is shared between targets */
#include "targ_sim_core.h"

#ifdef __cplusplus
}
#endif
#endif /* targ_sim_INCLUDED */

