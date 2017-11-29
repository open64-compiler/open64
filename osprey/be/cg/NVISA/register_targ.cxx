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


#define INCLUDING_IN_REGISTER // we modify register classes and we're
			      // really part of the register implementation

#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "config.h"
#include "glob.h"
#include "util.h"
#include "calls.h"
#include "data_layout.h"
#include "tn.h"
#include "targ_sim.h"
#include "op.h"
#include "cg_flags.h"

#include "register.h"

/////////////////////////////////////
void
REGISTER_Init_Stacked(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
}

/////////////////////////////////////
void REGISTER_Request_Stacked_Rotating_Register()
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
}

/////////////////////////////////////
REGISTER REGISTER_Request_Stacked_Register(INT has_abi_property,
					   ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return REGISTER_UNDEFINED;  
}

/////////////////////////////////////
REGISTER
REGISTER_Allocate_Stacked_Register(INT has_abi_property,
				   ISA_REGISTER_CLASS rclass,
				   REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return REGISTER_UNDEFINED;  
}

/////////////////////////////////////
void
REGISTER_Unallocate_Stacked_Register (
   ISA_REGISTER_CLASS rclass, REGISTER reg)
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
}

/////////////////////////////////////
BOOL
REGISTER_Is_Allocatable_Stacked_Register (
   ISA_REGISTER_CLASS rclass, REGISTER reg)
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return FALSE;
}

/////////////////////////////////////
REGISTER_SET
REGISTER_Get_Stacked_Avail_Set(INT has_abi_property, ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return REGISTER_SET_EMPTY_SET;
}
    
/////////////////////////////////////
BOOL
REGISTER_Is_Stacked_Output(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  return FALSE;
}

    
/////////////////////////////////////
BOOL
REGISTER_Is_Stacked_Local(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  return FALSE;
}

    
/////////////////////////////////////
BOOL
REGISTER_Is_Stacked(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  return FALSE;
}

/////////////////////////////////////
BOOL
REGISTER_Is_Rotating(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  return FALSE;
}

/////////////////////////////////////
REGISTER
REGISTER_Translate_Stacked_Output(REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return REGISTER_UNDEFINED;
}


/////////////////////////////////////
char *
REGISTER_Stacked_Output_Name (REGISTER reg)
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return NULL;
}


/////////////////////////////////////
INT
REGISTER_Number_Stacked_Local (ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return 0;
}

/////////////////////////////////////
INT
REGISTER_Number_Stacked_Output (ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return 0;
}


/////////////////////////////////////
INT
REGISTER_Number_Stacked_Rotating (ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return 0;
}

INT REGISTER_Number_Stacked_Registers_Available (ISA_REGISTER_CLASS rclass)
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return 0;
}


/////////////////////////////////////
void REGISTER_Reserve_Rotating_Registers(ISA_REGISTER_CLASS rclass, INT n)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
}

/////////////////////////////////////
void
REGISTER_Set_Stacked_Output_Minimum(ISA_REGISTER_CLASS rclass, INT num)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
}


/////////////////////////////////////
REGISTER REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return REGISTER_UNDEFINED;
}

/////////////////////////////////////
REGISTER REGISTER_Last_Rotating_Registers(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return REGISTER_UNDEFINED; 
}


/////////////////////////////////////
REGISTER_SET REGISTER_Get_Requested_Rotating_Registers(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  FmtAssert(FALSE, ("UNIMPLEMENTED"));
  return REGISTER_SET_EMPTY_SET;
}


mISA_REGISTER_CLASS Mtype_RegClass_Map[MTYPE_LAST+1];

void Init_Mtype_RegClass_Map(void)
{
  INT i;
  mISA_REGISTER_CLASS * const map = Mtype_RegClass_Map;

  for (i = 0; i <= MTYPE_LAST; ++i) map[i] = ISA_REGISTER_CLASS_UNDEFINED;

  map[MTYPE_B] = ISA_REGISTER_CLASS_predicate;
  map[MTYPE_I1] = ISA_REGISTER_CLASS_integer16;
  map[MTYPE_I2] = ISA_REGISTER_CLASS_integer16;
  map[MTYPE_I4] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_I8] = ISA_REGISTER_CLASS_integer64;
  map[MTYPE_U1] = ISA_REGISTER_CLASS_integer16;
  map[MTYPE_U2] = ISA_REGISTER_CLASS_integer16;
  map[MTYPE_U4] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U8] = ISA_REGISTER_CLASS_integer64;
  map[MTYPE_F4] = ISA_REGISTER_CLASS_float;
  map[MTYPE_F8] = ISA_REGISTER_CLASS_float64;
}
