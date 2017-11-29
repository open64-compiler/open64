/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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

#ifndef targ_sim_INCLUDED
#define targ_sim_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

// NVISA/PTX doesn't have dedicated hardware regs,
// but we need some to spoof the calling convention,
// so create arbitrary values which will be translated
// to virtual regs.

#define MAX_NUMBER_OF_REGISTERS_FOR_RETURN 4
#define MAX_NUMBER_OF_REGISTER_PARAMETERS  16 // used in data_layout.cxx
#define MAX_NUMBER_OF_INT_REGISTER_PARAMETERS 16 
#define MAX_NUMBER_OF_FLOAT_REGISTER_PARAMETERS 16

/* The offsets for return registers are fixed: */
#define First_Int32_Preg_Return_Offset	1
#define Last_Int32_Preg_Return_Offset	\
	(First_Int32_Preg_Return_Offset + MAX_NUMBER_OF_REGISTERS_FOR_RETURN -1)
#define First_Int32_Preg_Param_Offset	(Last_Int32_Preg_Return_Offset + 1)
#define Last_Int32_Preg_Param_Offset	\
	(First_Int32_Preg_Param_Offset + MAX_NUMBER_OF_REGISTER_PARAMETERS -1)
#define Stack_Pointer_Preg_Offset	(Last_Int32_Preg_Param_Offset + 1)
#define Frame_Pointer_Preg_Offset	(Stack_Pointer_Preg_Offset + 1)
#define Static_Link_Preg_Offset		(Frame_Pointer_Preg_Offset + 1)
#define First_Int64_Preg_Return_Offset	(Static_Link_Preg_Offset + 1)
#define Last_Int64_Preg_Return_Offset	\
	(First_Int64_Preg_Return_Offset + MAX_NUMBER_OF_REGISTERS_FOR_RETURN -1)
#define First_Int64_Preg_Param_Offset	(Last_Int64_Preg_Return_Offset + 1)
#define Last_Int64_Preg_Param_Offset	\
	(First_Int64_Preg_Param_Offset + MAX_NUMBER_OF_REGISTER_PARAMETERS -1)
#define First_Float32_Preg_Return_Offset (Last_Int64_Preg_Param_Offset + 1)
#define Last_Float32_Preg_Return_Offset	\
	(First_Float32_Preg_Return_Offset + MAX_NUMBER_OF_REGISTERS_FOR_RETURN -1)
#define First_Float32_Preg_Param_Offset	(Last_Float32_Preg_Return_Offset + 1)
#define Last_Float32_Preg_Param_Offset	\
	(First_Float32_Preg_Param_Offset + MAX_NUMBER_OF_REGISTER_PARAMETERS -1)
#define First_Float64_Preg_Return_Offset (Last_Float32_Preg_Param_Offset + 1)
#define Last_Float64_Preg_Return_Offset	\
	(First_Float64_Preg_Return_Offset + MAX_NUMBER_OF_REGISTERS_FOR_RETURN -1)
#define First_Float64_Preg_Param_Offset (Last_Float64_Preg_Return_Offset + 1)
#define Last_Float64_Preg_Param_Offset	\
	(First_Float64_Preg_Param_Offset + MAX_NUMBER_OF_REGISTER_PARAMETERS -1)

/* some definitions for the dedicated hardware pregs: */
#define Int32_Preg_Min_Offset		First_Int32_Preg_Return_Offset
#define Int32_Preg_Max_Offset		Static_Link_Preg_Offset
#define Int64_Preg_Min_Offset		First_Int64_Preg_Return_Offset
#define Int64_Preg_Max_Offset		Last_Int64_Preg_Param_Offset
#define Float32_Preg_Min_Offset		First_Float32_Preg_Return_Offset
#define Float32_Preg_Max_Offset		Last_Float32_Preg_Param_Offset
#define Float64_Preg_Min_Offset		First_Float64_Preg_Return_Offset
#define Float64_Preg_Max_Offset		Last_Float64_Preg_Param_Offset
#define First_Int_Preg_Return_Offset	First_Int32_Preg_Return_Offset
#define First_Int_Preg_Param_Offset	First_Int32_Preg_Param_Offset
#define First_Float_Preg_Return_Offset	First_Float32_Preg_Return_Offset
#define First_Float_Preg_Param_Offset	First_Float32_Preg_Param_Offset
#define Int_Preg_Min_Offset		Int32_Preg_Min_Offset
#define Int_Preg_Max_Offset		Int64_Preg_Max_Offset
#define Float_Preg_Min_Offset		Float32_Preg_Min_Offset
#define Float_Preg_Max_Offset		Float64_Preg_Max_Offset
#define Last_Dedicated_Preg_Offset      Float64_Preg_Max_Offset

#define PUSH_RETURN_ADDRESS_ON_STACK FALSE
#define PUSH_FRAME_POINTER_ON_STACK  FALSE
#define USE_HIGH_LEVEL_PROCEDURE_EXIT TRUE

#define Preg_Offset_Is_Int32(n) \
        ((n) >= Int32_Preg_Min_Offset && (n) <= Int32_Preg_Max_Offset)
#define Preg_Offset_Is_Float32(n) \
        ((n) >= Float32_Preg_Min_Offset && (n) <= Float32_Preg_Max_Offset)
#define Preg_Offset_Is_Int64(n) \
        ((n) >= Int64_Preg_Min_Offset && (n) <= Int64_Preg_Max_Offset)
#define Preg_Offset_Is_Float64(n) \
        ((n) >= Float64_Preg_Min_Offset && (n) <= Float64_Preg_Max_Offset)

/* most of the interface is shared between targets */
#include "targ_sim_core.h"

#ifdef __cplusplus
}
#endif

#endif /* targ_sim_INCLUDED */

