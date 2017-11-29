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


// abi_properties_gen.h
/////////////////////////////////////
//
//  Interface for specifying properties (attributes) for various ABIs
//  supported by an architecture.
//
//  void ABI_Properties_Begin(const char *name)
//	Initialize to generate ABI properties information for the architecture 
//	with the given <name>.  The information will be written to the 
//	files targ_abi_properties.[ch].  
//
//  TYPE ABI_PROPERTY
//	An abstract type that represents a property of an ABI.
//	No client visible fields.
//
//  ABI_PROPERTY Create_Reg_Property(const char *name)
//	Used to create a new ABI_PROPERTY.  <name> is the property name.
//	It will be used to define a ABI_PROPERTY_<name> constant.
//
//  Begin_ABI(const char *name)
//	Begin the definition of the ABI named <name>. An ABI definition
//	is ended by another Begin_ABI() call or a call to ABI_Properties_End().
//
//  void Reg_Property(ABI_PROPERTY prop, ISA_REGISTER_CLASS rc, ...)
//	Give a number of registers in class <rc>, the ABI property <prop>,
//	i.e. it identifies the role of particular registers in the ABI.
//	The variable argument specifies the register numbers, terminated
//	by -1.
//  void Reg_Property_Range(ABI_PROPERTY prop, ISA_REGISTER_CLASS rc, 
//		INT minreg, INT maxreg); 
//	Same as Reg_Property except rather than list each register it will
// 	use every register in [minreg..maxreg] range.
//
//  void Reg_Names(ISA_REGISTER_CLASS rc, 
//		   INT minreg, 
//		   INT maxreg, 
//		   const char **names)
//	Give ABI names to the registers in class <rc>. These names typically
//	identify the role of the register in the ABI. If a register is
//	not explicitly given an ABI name, the hardware name is used.
//	The register names are specified in an array pointed to by <names>.
//	<minreg> gives the register number of the first name in the
//	<names> array; <maxreg> gives the last.
//  void Set_Reg_Name(ISA_REGISTER_CLASS rc, INT reg, const char *name);
//      Like Reg_Names, but only set single name.
//
//  void ABI_Properties_End(void)
//      Complete processing of ABI properties.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/abi_properties_gen.h,v $

#include "targ_isa_registers.h"

typedef struct abi_property *ABI_PROPERTY;

extern void ABI_Properties_Begin(const char *name);
extern ABI_PROPERTY Create_Reg_Property(const char *name);
extern void Begin_ABI(const char *name);
extern void Reg_Property(ABI_PROPERTY prop, ISA_REGISTER_CLASS rc, ...);
extern void Reg_Property_Range(ABI_PROPERTY prop, ISA_REGISTER_CLASS rc, 
	INT minreg, INT maxreg);
extern void Reg_Names(ISA_REGISTER_CLASS rc, 
		      INT minreg, 
		      INT maxreg, 
		      const char **names);
extern void Set_Reg_Name(ISA_REGISTER_CLASS rc, INT reg, const char *name);
extern void ABI_Properties_End(void);
