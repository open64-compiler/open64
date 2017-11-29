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


// isa_pack_gen.h
/////////////////////////////////////
//
//  Interface for packing instructions into an instruction word for all
//  instructions in the ISA.
//
//  void ISA_Pack_Begin( const char* archname, int inst_bit_size )
//      Initialize to generate instruction packing information for the 
//      architecture with the given <archname>.  The information will be 
//	written to the files targ_isa_pack.[ch].  <inst_bit_size>
//	indicates how many bits in an instruction (operation) word.
//
//  TYPE ISA_PACK_TYPE
//      An abstract type that describes different types of packing formats.
//
//  ISA_PACK_TYPE ISA_Pack_Type_Create( const char* name )
//      Used to create a new ISA_PACK_TYPE.  <name> is the pack_type name.
//	The rules to pack individual operands/results are added using the
//      following two routines.
//
//  TYPE OPND_ADJ_TYPE
//	An abstract type that describes different types of operand adjustments.
//
//  OPND_ADJ_TYPE Create_Operand_Adjustment(const char *name,
//					    const char *adj)
//	Used to create a new operand adjustment. <name> is a descriptive
//	name to be used in comments. <adj> is a string whose contents
//	is a C expression that applies an adjustment to the value
//	of an operand. The macro O_VAL is used to refer to the operand
//	value. The expression should produce a value that is to
//	replace the original value.
//
//  void Instruction_Pack_Group(ISA_PACK_TYPE pack_type, ...)
//      Group instructions which have the same packing format but initialize
//	<opcode_mask> separately for each TOP in this group. The variable
//	part of the parameter list is the pair:
//
//		TOP top, <unsigned-type> opcode_mask
//
//	<unsigned-type> is an unsigned type large enough to hold the
//	instruction word (must correspond with the inst_size parameter
//	of ISA_Pack_Begin). The parameter list is ended by specifing 
//	TOP_UNDEFINED for <top> (<opcode_mask> may be omitted on the 
//	end of list marker).
//
//  void Operand (int operand_index,
//		  int opnd_position, 
//		  int inst_position,
//		  int width)
//	The <operand_index>'th operand of the current packing type is 
//	<width> bits wide. The bits are extracted from the operand at
//	bit <opnd_position> and deposited into the instruction word
//	at bit <inst_position>.
//
//	<opnd_position> will only be non-zero in cases where an operand
//	is split amoung multiple bit fields of an instruction word.
//	In such cases you would include multiple Operand specifications with
//	the same <operand_index>, one for each field.
//
//  void Adjust_Operand(int operand_index, 
//			OPND_ADJ_TYPE pack_adj,
//			OPND_ADJ_TYPE unpack_adj)
//	The <operand_index>'th operand of the current packing type
//	requires an adjustment when going between assembler and binary
//	forms. <pack_adj> specifies the adjustment when packing a binary
//	instruction, and <unpack_adj> specifies the adjustment when
//	unpacking a binary instruction.
//
//  void Result (int result_index, int bit_position, int width)
//	The <result_index>'th result of the current packing type is
//	at <bit_position> in the instruction word and is <width> bits long.
//
//  void Next_Word (void)
//	In multiple word instructions, advance to the next word. Subsequent
//	Operand and Result calls operate on this word.
//
//  void ISA_Pack_Is_Unused(void)
//      If won't be using pack info, then this will generate dummy file
//      so builds easy and don't have to create any pack types.
//
//  void ISA_Pack_End(void)
//      Complete processing of operands/results.
//
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_pack_gen.h,v $

#ifndef isa_pack_gen_INCLUDED
#define isa_pack_gen_INCLUDED

#ifdef _KEEP_RCS_ID
static const char isa_operands_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_pack_gen.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct isa_pack_type *ISA_PACK_TYPE;

typedef struct opnd_adj_type *OPND_ADJ_TYPE;

extern void ISA_Pack_Begin ( const char* archname, int inst_bit_size );
extern ISA_PACK_TYPE ISA_Pack_Type_Create ( const char* name );
extern OPND_ADJ_TYPE Create_Operand_Adjustment(const char *name,
					       const char *adj);
extern void Instruction_Pack_Group (ISA_PACK_TYPE pack_type, ...);
extern void Operand (int operand_index,
		     int opnd_position, 
		     int inst_position, 
		     int width);
extern void Adjust_Operand(int operand_index, 
			   OPND_ADJ_TYPE pack_adj,
			   OPND_ADJ_TYPE unpack_adj);
extern void Result (int result_index, int bit_position, int width);
extern void Next_Word (void);
extern void ISA_Pack_Is_Unused(void);
extern void ISA_Pack_End(void);

#ifdef __cplusplus
}
#endif
#endif /* isa_pack_gen_INCLUDED */
