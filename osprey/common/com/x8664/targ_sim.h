/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

/* some definitions for the dedicated hardware pregs: */

#define Int_Preg_Min_Offset             1
#define Int_Preg_Max_Offset             16
#define Float_Preg_Min_Offset           17
#define Float_Preg_Max_Offset           32
#define X87_Preg_Min_Offset             33
#define X87_Preg_Max_Offset             40
#define MMX_Preg_Min_Offset             41
#define MMX_Preg_Max_Offset             48
#define Last_Dedicated_Preg_Offset      MMX_Preg_Max_Offset

  /* The order is assigned to favor the parameter passing. */
  enum { RAX=1, RBX, RBP, RSP, RDI, RSI, RDX, RCX,
	 R8, R9, R10, R11, R12, R13, R14, R15,
  	 XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
	 XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
	 ST0,  ST1,  ST2,   ST3,   ST4,   ST5,   ST6,   ST7,
  	 MM0, MM1, MM2,  MM3,  MM4,  MM5,  MM6,  MM7   };

/* The offsets for return registers are fixed: */
#define First_Int_Preg_Return_Offset	RAX
#define Last_Int_Preg_Return_Offset	RDX
#define First_Float_Preg_Return_Offset	XMM0
#define Last_Float_Preg_Return_Offset	XMM1
#define Stack_Pointer_Preg_Offset	RSP
#define Frame_Pointer_Preg_Offset	RBP
#define First_Int_Preg_Param_Offset	RDI
#define Last_Int_Preg_Param_Offset	R9
#define First_Float_Preg_Param_Offset	XMM0
#define Last_Float_Preg_Param_Offset	XMM7
#define First_X87_Preg_Return_Offset	ST0
#define Last_X87_Preg_Return_Offset	ST1
#define First_MMX_Preg_Return_Offset	MM0
#define Last_MMX_Preg_Return_Offset	MM1
#define Static_Link_Preg_Offset		( Is_Target_64bit() ? R10 : RCX )

#define MAX_NUMBER_OF_REGISTERS_FOR_RETURN 2
#define MAX_NUMBER_OF_REGISTER_PARAMETERS  14 // used in data_layout.cxx
#define MAX_NUMBER_OF_INT_REGISTER_PARAMETERS \
               ( Is_Target_64bit() ? 6 : 0 )  // used in data_layout.cxx
#define MAX_NUMBER_OF_FLOAT_REGISTER_PARAMETERS  \
               ( Is_Target_64bit() ? 8 : 0 )  // used in data_layout.cxx

#define PUSH_RETURN_ADDRESS_ON_STACK TRUE
#define PUSH_FRAME_POINTER_ON_STACK  TRUE
#define USE_HIGH_LEVEL_PROCEDURE_EXIT TRUE

/* most of the interface is shared between targets */
#include "targ_sim_core.h"

/* See X86-64 ABI document */
enum X86_64_PARM_CLASS {
    X86_64_NO_CLASS,
    X86_64_INTEGER_CLASS,
    X86_64_SSE_CLASS,
    X86_64_MEMORY_CLASS,
    X86_64_X87_CLASS,
    X86_64_X87UP_CLASS,
    X86_64_SSEUP_CLASS,
    X86_64_COMPLEX_X87_CLASS
};

#define MAX_CLASSES 4
extern INT Classify_Aggregate(const TY_IDX ty, 
			      enum X86_64_PARM_CLASS classes[MAX_CLASSES], INT byte_offset = 0);

#ifdef __cplusplus
}
#endif

#endif /* targ_sim_INCLUDED */

