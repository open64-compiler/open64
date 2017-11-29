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


// isa_operands_gen.h
/////////////////////////////////////
//
//  Interface for specifying operands and results for various
//  instructions in the ISA.
//
//  void ISA_Operands_Begin( const char* archname )
//      Initialize to generate operands information for the architecture 
//      with the given <archname>.  The information will be written to the 
//      files targ_isa_operands.[ch].  
//
//  type OPERAND_VALUE_TYPE
//      An abstract type that describes different types of operands,
//	including results. An operand type can be a register or an
//	immediate or an enumeration.
//
//  type OPERAND_USE_TYPE
//	An abstract type that describes the useage of an operand.
//
//  OPERAND_VALUE_TYPE ISA_Reg_Opnd_Type_Create( 
//			const char* name, 
//			ISA_REGISTER_CLASS register_class, 
//			ISA_REGISTER_SUBCLASS sub_class,
//			int size, 
//			RTYPE type, 
//			REG_ALIAS_TYPE reg_alias,
//			FP_TYPE is_fp_int )
//      Used to create a new OPERAND_VALUE_TYPE.  
//	<name> is the operand_type name.
//
//  OPERAND_VALUE_TYPE ISA_Lit_Opnd_Type_Create( 
//			const char* name, 
//			int size,
//			RTYPE type, 
//			ISA_LIT_CLASS lc )
//      Used to create a new OPERAND_VALUE_TYPE.  
//	<name> is the operand_type name.
//
//  OPERAND_VALUE_TYPE ISA_Enum_Opnd_Type_Create( 
//			const char* name, 
//			int size,
//			RTYPE type, 
//			ISA_ENUM_CLASS ec )
//      Used to create a new OPERAND_VALUE_TYPE.  
//	<name> is the operand_type name.
//
//  OPERAND_USE_TYPE Create_Operand_Use(const char *name)
//	Create a new OPERAND_USE_TYPE. <name> identifies the usage
//	and is used to form enumerated constants of the form OU_<name>.
//
//  void Instruction_Group( const char *name, 
//			    TOP top, ....., TOP_UNDEFINED )
//      Start a list of instructions that have the same operand/result
//	descriptions. The list of TOPs is terminated by TOP_UNDEFINED.
//
//  void Operand (int operand_index, 
//		  OPERAND_VALUE_TYPE operand_type,
//		  [ OPERAND_USE_TYPE operand_use ])
//	The <operand_index>'th operand of the current instruction group
//      is of <operand_type> and usage <operand_use>. If <operand_use>
//	is omitted it defaults to "undefined".
//
//  void Relocatable (int operand_index)
//	The <operand_index>'th operand of the current instruction group
//      is relocatable. Not supported for results, and there can
//	be only one relocatable operand per topcode.
//
//  void Result (int result_index, OPERAND_VALUE_TYPE result_type)
//	The <result_index>'th result of the current instruction group 
// 	is of <result_type>. 
//
//  void ISA_Operands_End(void)
//      Complete processing of operands/results.
//
//
/////////////////////////////////////

//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_operands_gen.h,v $

#ifndef isa_operands_gen_INCLUDED
#define isa_operands_gen_INCLUDED

#ifdef _KEEP_RCS_ID
static const char isa_operands_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_operands_gen.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

#include "targ_isa_registers.h"
#include "targ_isa_lits.h"
#include "targ_isa_enums.h"

/* Types:
 */

typedef struct operand_value_type *OPERAND_VALUE_TYPE;

// RTYPE (or the range-description type) is used to annotate a proper
// range value to the concerned OPERAND_VALUE_TYPE.  

typedef enum {PCREL, SIGNED, UNSIGNED, UNKNOWN} RTYPE; 

// FP_TYPE description is used to determine the fp-value in a floating register
// i.e. FP32_INT => 32-bit int value in a fp- register (fixed-point)
// or   FP64_INT => 64-bit int value in a fp- register (fixed-point)
// or   INVALID (which includes the rest of the category)

typedef enum {FP32_INT, FP64_INT, INVALID} FP_TYPE; 

typedef struct operand_use_type *OPERAND_USE_TYPE;

/* External functions 
 */

extern void ISA_Operands_Begin ( const char* archname );
extern OPERAND_VALUE_TYPE ISA_Reg_Opnd_Type_Create ( 
		const char* name, 
	  	ISA_REGISTER_CLASS register_class, 
		ISA_REGISTER_SUBCLASS sub_class,
		int size, 
		RTYPE rtype, 
		FP_TYPE is_fp_int );
extern OPERAND_VALUE_TYPE ISA_Lit_Opnd_Type_Create ( 
		const char* name, 
		int size,
		RTYPE rtype, 
		ISA_LIT_CLASS lc );
extern OPERAND_VALUE_TYPE ISA_Enum_Opnd_Type_Create ( 
		const char* name, 
		int size,
		RTYPE rtype, 
		ISA_ENUM_CLASS ec );
extern OPERAND_USE_TYPE Create_Operand_Use(const char *name);
extern void Instruction_Group ( const char *name, ... );
extern void Operand (int operand_index, 
		     OPERAND_VALUE_TYPE operand_type,
		     OPERAND_USE_TYPE operand_use = 0);
extern void Relocatable (int operand_index);
extern void Result (int result_index, OPERAND_VALUE_TYPE result_type);
extern void ISA_Operands_End(void);

#ifdef __cplusplus
}
#endif
#endif /* isa_operands_gen_INCLUDED */
