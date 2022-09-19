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


// isa_print_gen.h
/////////////////////////////////////
//
//  Interface for printing the instructions in an ISA.
//
//  void ISA_Print_Begin( const char* archname )
//      Initialize to generate information to print the instructions for 
//	the architecture with the given <archname>.  The information will 
//	be written to the files targ_isa_print.[ch].  
//
//  TYPE ISA_PRINT_TYPE
//      An abstract type that describes different types of printing formats.
//
//  ISA_PRINT_TYPE ISA_Print_Type_Create( const char* name, 
//					  const char* format_string )
//      Used to create a new ISA_PRINT_TYPE.  <name> is the name of the 
//	print type. The <format_string> specifies the string to be used to 
//	print instructions of this type. The order in which the results
//	and operands are printed is specified using a sequence of the 
//	following two routines.
//
//  void Name (void)
//	Print the instruction's name next.
//
//  void Operand (int operand_index)
//	Print the <operand_index>'th operand next.
//
//  void Result (int result_index)
//	Print the <operand_index>'th result next.
//
//  void Instruction_Print_Group (ISA_PRINT_TYPE print_type, TOP top, ...., 
//				TOP_UNDEFINED)
//	List all the instructions that use <print_type> for printing. All
//	the instructions are enumerated in a list terminated by TOP_UNDEFINED.
//
//  void Set_AsmName_Func(char *(*asmname_func)(TOP topcode))
//	Specifies a function which returns the assembly language name
//	to be used for the given topcode. If no function is specified,
//	the assembly language name is the same as TOP_Name.
//
//  void ISA_Print_End(void)
//      Complete processing of operands/results.
//
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_print_gen.h,v $

#ifndef isa_print_gen_INCLUDED
#define isa_print_gen_INCLUDED

#include "gen_util_gen.h"

#ifdef _KEEP_RCS_ID
static const char isa_operands_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_print_gen.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct isa_print_type *ISA_PRINT_TYPE;

extern void ISA_Print_Begin ( const char* archname );
extern ISA_PRINT_TYPE ISA_Print_Type_Create ( const char* name, 
					      const char *format_string );
extern void Name (void);
extern void Operand (int operand_index);
extern void Result (int result_index);
extern void Instruction_Print_Group ( ISA_PRINT_TYPE print_type, TOP top, ... );
extern void ISA_Print_End(void);
extern void Set_AsmName_Func(const char *(*asmname_func)(TOP topcode));

#ifdef __cplusplus
}
#endif
#endif /* isa_print_gen_INCLUDED */
