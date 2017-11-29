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

  map[MTYPE_I1] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_I2] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_I4] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_I8] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U1] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U2] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U4] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U8] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_F4] = ISA_REGISTER_CLASS_float;
  map[MTYPE_F8] = ISA_REGISTER_CLASS_float;
  map[MTYPE_F10] = ISA_REGISTER_CLASS_float;
  map[MTYPE_F16] = ISA_REGISTER_CLASS_UNDEFINED;
}
