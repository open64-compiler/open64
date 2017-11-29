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


// isa_bundle_gen.h
/////////////////////////////////////
//
//  Interface for packing instructions into an instruction word for all
//  instructions in the ISA.
//
//  void ISA_Bundle_Begin( const char* archname, int bundle_width )
//      Initialize to generate bundle (or template) information for the 
//      architecture with the given <archname>.  The information will be 
//	written to the files targ_isa_bundle.[ch].  The width of the
//	bundle, in bits, is specified by <bundle_width>.
//
//  void ISA_Bundle_End(void)
//      Complete processing of bundle (or template) encoding.
//
/////////////////////////////////////
//
//  TYPE ISA_EXEC_UNIT_TYPE
//      An abstract type that describes the different execution unit types.
//
//  ISA_EXEC_UNIT_TYPE ISA_Exec_Unit_Type_Create (const char* name)
//      Used to create a new ISA_EXEC_UNIT_TYPE. <name> is the execution_type
//      name. 
//  
//  Instruction_Exec_Unit_Group (ISA_EXEC_UNIT_TYPE unit_type, ....)
//      Used to group all TOPs which can execute in <unit_type>.
//
//  TYPE ISA_BUNDLE_TYPE
//      An abstract type that describes different types of bundle encodings.
//
//  ISA_BUNDLE_TYPE ISA_Bundle_Type_Create( const char* name,
//					    const char* asm_name,
//					    int slot_count )
//      Used to create a new ISA_BUNDLE_TYPE.  <name> is the bundle_type name.
//	<asm_name> is the bundle's assembly language name.
//	The rules to encode the execution types within a bundle are described
//      by the routine below. <slot_count> specifies the number of intruction
//	slots in this bundle type. 
//
//  void Slot (int slot_index, Exec_Unit_Type type)
//	The <slot_index> of the current bundling type is reserved for 
//	execution unit <type>.
//
//  void Stop (int slot_index)
//      The <slot_index> of the current bundling type is a stop bit.
//
/////////////////////////////////////
//
//  void ISA_Bundle_Pack_Create (ISA_BUNDLE_PACK_ENDIAN endian)
//	Start the specification of how the fields of a bundle are packed.
//	<endian> specifies if the bundle should be in little or big endian
//	format. The generator handles and cross-endian issues.
//
//  void Pack_Template (int comp_pos, int bundle_pos, int width)
//  void Pack_Slot (int slot, int comp_pos, int bundle_pos, int width)
//	Specify packing for a bundle field. <comp_pos> specifies the
//	the start of the field in the input component value. <bundle_pos>
//	specifies the start of the field in the bundle. <width> specifies
//	the width of the field.
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_bundle_gen.h,v $

#ifndef isa_bundle_gen_INCLUDED
#define isa_bundle_gen_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

typedef struct isa_exec_unit_type *ISA_EXEC_UNIT_TYPE;
typedef struct isa_bundle_type *ISA_BUNDLE_TYPE;

typedef enum {
  ISA_Bundle_Pack_Little_Endian,
  ISA_Bundle_Pack_Big_Endian
} ISA_BUNDLE_PACK_ENDIAN;

extern void ISA_Bundle_Begin ( const char* archname, int bundle_width );

extern ISA_EXEC_UNIT_TYPE ISA_Exec_Unit_Type_Create ( 
	const char* name,
	ISA_EXEC_UNIT_TYPE base_unit );
extern void Instruction_Exec_Unit_Group (ISA_EXEC_UNIT_TYPE unit_type, ...);

extern void ISA_Bundle_Pack_Create (ISA_BUNDLE_PACK_ENDIAN endian);
extern void Pack_Template (int comp_pos, int bundle_pos, int width);
extern void Pack_Slot (int slot, int comp_pos, int bundle_pos, int width);

extern void ISA_Bundle_Type_Create ( const char* name,
				     const char* asm_name,
				     int slot_count );
extern void Slot (int slot_index, ISA_EXEC_UNIT_TYPE type);
extern void Stop (int slot_index);

extern void ISA_Bundle_End(void);

#ifdef __cplusplus
}
#endif
#endif /* isa_bundle_gen_INCLUDED */
