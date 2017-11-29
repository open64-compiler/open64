/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
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


//#define HAS_STACKED_REGISTERS
//#define HAS_ROTATING_REGISTERS

/*
 * Target specific register class routines:
 *
 * Register stack register allocation:
 *
 *	void REGISTER_Init_Stacked(ISA_REGISTER_CLASS rclass)
 *	    Initialize the data to allocate from the register stack for
 *	    a given class (only integer in IA-64).
 *
 *      void REGISTER_Request_Stacked_Rotating_Register()
 *          Allocate the rotating register from stacked_callee_used
 *
 *	REGISTER REGISTER_Request_Stacked_Register(
 *          INT has_abi_property, 
 *          ISA_REGISTER_CLASS rclass					      
 *	)
 *	    Return a register from the register stack that behaves like
 *	    the supplied ABI property (callee saved, caller saved, etc...).
 *
 *	REGISTER REGISTER_Allocate_Stacked_Register
 *	    INT has_abi_property, 
 *	    ISA_REGISTER_CLASS rclass					       
 *	    REGISTER reg						   
 *	)
 *	    Allocate a specific register from the stacked registers that
 *	    behaves like the supplied ABI property.
 *
 *	REGISTER_SET REGISTER_Get_Stacked_Avail_Set
 *	    INT has_abi_property, 
 *	    ISA_REGISTER_CLASS rclass					      
 *	)
 *	    Return a register set of the registers that are currently
 *	    available from the register stack that behaves like
 *	    the supplied ABI property (i.e. the registers of that
 *	    ABI property that have already been allocated).
 *
 *	BOOL REGISTER_Is_Stacked_Local(
 *	    ISA_REGISTER_CLASS rclass,
 *	    REGISTER reg
 *	)
 *	    Return true if the supplied register is a local register
 *	    in the register stack for the given register class.
 *
 *	BOOL REGISTER_Is_Stacked_Output(
 *	    ISA_REGISTER_CLASS rclass,
 *	    REGISTER reg
 *	)
 *	    Return true if the supplied register is an output register
 *	    in the register stack for the given register class.
 *
 *	REGISTER REGISTER_Is_Stacked(
 *	    ISA_REGISTER_CLASS rclass,
 *	    REGISTER reg
 *	)
 *	    Return true if the supplied register is a member of
 *	    the register stack for the given register class.
 *
 *	BOOL REGISTER_Is_Rotating(
 *	    ISA_REGISTER_CLASS rclass,
 *	    REGISTER reg
 *	)
 *	    Return true if the supplied register is a rotating register
 *	    in the register stack for the given register class.
 *
 *	REGISTER REGISTER_Translate_Stacked_Output(
 *	    REGISTER reg
 *	)
 *	    Output registers are allocated from the end of the stacked
 *	    set and input/callee from the front.  This makes it easier
 *	    to prevent over subscribing the register set.  This routine
 *	    will translate the register number of the output register
 *	    to its real number once all the counts are known.
 *
 *	INT REGISTER_Number_Stacked_Output(
 *	    ISA_REGISTER_CLASS rclass
 *	)
 *	INT REGISTER_Number_Stacked_Local(
 *	    ISA_REGISTER_CLASS rclass
 *	)
 *	INT REGISTER_Number_Stacked_Rotating(
 *	    ISA_REGISTER_CLASS rclass
 *	)
 *	    Return number of local, output, and rotating registers  
 *          allocated from the  register stack.
 *
 *      void REGISTER_Reserve_Rotating_Registers(
 *          ISA_REGISTER_CLASS rclass, INT n)
 *           Rerserve n rotating registers.
 *
 *	void REGISTER_Set_Stacked_Output_Minimum(ISA_REGISTER_CLASS rclass,
 *						 INT num)
 *	    Set the minimum number of output registers that must be allocated.
 *	    This is to prevent all of them from being given to over as locals.
 *
 *	BOOL REGISTER_Has_Stacked_Registers(ISA_REGISTER_CLASS rclass)
 *	    Returns TRUE if stacked register set exists for <rclass>.
 *
 * 	char * REGISTER_Stacked_Output_Name (REGISTER reg)
 *		Return software stack name like out0.
 *		If returns NULL then should just use regular register name.
 *
 *      BOOL REGISTER_Has_Rotating_Registers(ISA_REGISTER_CLASS rclass)
 *          Returns TRUE if rotating register set exists for <rclass>.
 *
 *      REGISTER REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS rclass)
 *          Returns the first rotating register for <rclass>.
 *
 *      REGISTER REGISTER_Last_Rotating_Registers(ISA_REGISTER_CLASS rclass)
 *          Returns the last rotating register for <rclass>.
 *
 *      BOOL Is_Predicate_REGISTER_CLASS(ISA_REGISTER_CLASS rclass)
 *          Returns TRUE if the rclass is the predicate register class
 *
 *      REGISTER_SET REGISTER_Get_Requested_Rotating_Registers(ISA_REGISTER_CLASS rclass)
 *          Returns the set of allocated rotating registers
 *
 */

#include "targ_abi_properties.h"

inline BOOL REGISTER_Has_Stacked_Registers(ISA_REGISTER_CLASS rclass) {
  return FALSE;
}
inline BOOL REGISTER_Has_Rotating_Registers(ISA_REGISTER_CLASS rclass) {
  return FALSE;
}

extern void REGISTER_Init_Stacked(ISA_REGISTER_CLASS rclass);
extern REGISTER REGISTER_Request_Stacked_Register(INT has_abi_property,
						  ISA_REGISTER_CLASS rclass);
extern REGISTER REGISTER_Allocate_Stacked_Register(INT has_abi_property,
						   ISA_REGISTER_CLASS rclass,
						   REGISTER reg);
// unallocate a stacked register such that it is not re-used.
extern void REGISTER_Unallocate_Stacked_Register (
   ISA_REGISTER_CLASS rclass, REGISTER reg);

// return whether stacked register is allocatable
extern BOOL REGISTER_Is_Allocatable_Stacked_Register (
   ISA_REGISTER_CLASS rclass, REGISTER reg);

extern REGISTER_SET REGISTER_Get_Stacked_Avail_Set(INT has_abi_property,
						   ISA_REGISTER_CLASS rclass);
extern BOOL REGISTER_Is_Stacked_Output(ISA_REGISTER_CLASS rclass, REGISTER reg);
extern BOOL REGISTER_Is_Rotating(ISA_REGISTER_CLASS rclass, REGISTER reg);
extern BOOL REGISTER_Is_Stacked(ISA_REGISTER_CLASS rclass, REGISTER reg);
extern BOOL REGISTER_Is_Stacked_Local(ISA_REGISTER_CLASS rclass, REGISTER reg);
extern REGISTER REGISTER_Translate_Stacked_Output(REGISTER reg);
extern INT REGISTER_Number_Stacked_Output (ISA_REGISTER_CLASS);
extern INT REGISTER_Number_Stacked_Local (ISA_REGISTER_CLASS);
extern INT REGISTER_Number_Stacked_Rotating (ISA_REGISTER_CLASS);
extern INT REGISTER_Number_Stacked_Registers_Available (ISA_REGISTER_CLASS);

extern void REGISTER_Reserve_Rotating_Registers(ISA_REGISTER_CLASS rclass,
						INT n);
extern void REGISTER_Set_Stacked_Output_Minimum(ISA_REGISTER_CLASS rclass,
						INT num);

extern char * REGISTER_Stacked_Output_Name (REGISTER reg);

extern REGISTER REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS rclass);
extern REGISTER REGISTER_Last_Rotating_Registers(ISA_REGISTER_CLASS rclass);
extern void REGISTER_Request_Stacked_Rotating_Register();

inline BOOL Is_Predicate_REGISTER_CLASS(ISA_REGISTER_CLASS rclass) {
  return FALSE;
}

extern REGISTER_SET REGISTER_Get_Requested_Rotating_Registers(ISA_REGISTER_CLASS rclass);
