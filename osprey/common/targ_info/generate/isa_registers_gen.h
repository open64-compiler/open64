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


// isa_registers_gen.h
/////////////////////////////////////
//
//  Interface for describing registers.
//
//  void ISA_Registers_Begin( const char* archname )
//	Initialize to generate register information for the architecture
//	with the given <archname>. The information will be written
//	to the files targ_isa_registers.[ch].
//
//  ISA_REGISTER_CLASS
//	An abstract type that holds the properties of a class of
//	registers.
//
//  ISA_REGISTER_CLASS ISA_Register_Class_Create( const char *name,
//						  int bit_size,
//						  bool can_store,
//						  bool multiple_save )
//	Create a new register class. <name> is used to construct
//	an enumeration constant for this register class in the generated
//	output of the form: ISA_REGISTER_CLASS_<name>. <bit_size>
//	specifies the size, in bits, of the registers in the class.
//	<can_store> flags if there exists load and store instructions
//	for registers of this class. <multiple_save> flags if it is
//	possible to save/restore multiple registers of this class at once.
//
//  void ISA_Register_Set( ISA_REGISTER_CLASS rclass,
//			   int min_regnum,
//			   int max_regnum,
//			   const char *def_name_format,
//			   const char **names,
//			   int isa_mask )
//	Describe the registers of a class. <rclass> specifies the register
//	class. <min_regnum> specifies the lowest numbered register.
//	<max_regnum> specifies the highest numbered register.
//	<def_name_format> specifies a printf style format string for 
//	generating the default a register name given a register number.
//	<names> is an array, with index 0 corresponding to <min_regnum>,
//	which specifies reg names -- if an index is NULL, the name
//	is generated using <def_name_format>. <names> itself may be
//	NULL if <def_name_format> is valid for all registes.
//	<isa_mask> indicates which ISA levels this description is valid 
//	for. ISA_Register_Set may be called multiple times for a given 
//	register class if the register usage differs amoung the ISA levels.
//
//  void ISA_Register_Subclass_Create( const char *name,
//				       ISA_REGISTER_CLASS rclass,
//				       int count,
//				       const int *members,
//				       const char **names )
//	Describes a subclass of registers. <name> names the subclass
//	and is used to form an enum identifying the subclass. 
//	<rclass> is the base register class. <count> is the number of
//	members of registers in the subclass. <members> is an array
//	containing the register numbers of the subclass members.
//	<names> contains the assembly language names of the subclass
//	members. The names correspond one-to-one with the <members>
//	elements. If the subclass register name is not different,
//	NULL can be specified in place of the name. Additionaly,
//	NULL can be passed in place of the entire array if there are
//	no subclass specific names.
//
//  void ISA_Registers_End( void )
//      Complete processing.
//
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_registers_gen.h,v $


#ifndef ISA_REGISTERS_GEN_INCLUDED
#define ISA_REGISTERS_GEN_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
#ifndef ISA_REGISTERS_GEN_RCS_ID
#define ISA_REGISTERS_GEN_RCS_ID
#ifdef _KEEP_RCS_ID
static const char isa_registers_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_registers_gen.h,v $ $Revision: 1.1.1.1 $";
#endif
#endif

typedef struct isa_register_class *ISA_REGISTER_CLASS;

void ISA_Registers_Begin( const char* archname );
ISA_REGISTER_CLASS ISA_Register_Class_Create( const char *name,
					      int bit_size,
					      bool can_store,
					      bool multiple_save );
void ISA_Register_Set( ISA_REGISTER_CLASS rclass,
		       int min_regnum,
		       int max_regnum,
		       const char *def_name_format,
		       const char **names,
		       int isa_mask );
void ISA_Register_Subclass_Create( const char *name,
				   ISA_REGISTER_CLASS rclass,
				   int count,
				   const int *members,
				   const char **names );
void ISA_Registers_End( void );

#ifdef __cplusplus
}
#endif
#endif
