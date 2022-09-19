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

//
// Generate an ISA containing and group TOPS with similar operands/results 
// format.
/////////////////////////////////////////////////////////
// The instructions are listed below in the order as shown below. 
//
//   1. Real and pseudo instructions
//   2. Simulated instructions.
//
// Within each category, the instructions are arranged roughly in order
// of increasing numbers of operands.
/////////////////////////////////////
//
//  $Revision: 1.177 $
//  $Date: 05/11/11 10:44:29-08:00 $
//  $Author: tkong@hyalite.keyresearch $
//  $Source: common/targ_info/isa/x8664/SCCS/s.isa_operands.cxx $

#include "topcode.h"
#include "isa_operands_gen.h"

main()
{
  ISA_Operands_Begin("x8664");

  /* Literals... */

  const OPERAND_VALUE_TYPE simm8 =
    ISA_Lit_Opnd_Type_Create("simm8",  8,  SIGNED,   LC_simm8);
  const OPERAND_VALUE_TYPE uimm8 =
    ISA_Lit_Opnd_Type_Create("uimm8",  8,  UNSIGNED, LC_uimm8);
  const OPERAND_VALUE_TYPE simm16 =
    ISA_Lit_Opnd_Type_Create("simm16", 16, SIGNED,   LC_simm16);
  const OPERAND_VALUE_TYPE uimm16 =
    ISA_Lit_Opnd_Type_Create("uimm16", 16, UNSIGNED, LC_uimm16);
  const OPERAND_VALUE_TYPE simm32 =
    ISA_Lit_Opnd_Type_Create("simm32", 32, SIGNED,   LC_simm32);
  const OPERAND_VALUE_TYPE uimm32 =
    ISA_Lit_Opnd_Type_Create("uimm32", 32, UNSIGNED, LC_uimm32);
  const OPERAND_VALUE_TYPE simm64 =
    ISA_Lit_Opnd_Type_Create("simm64",  64, SIGNED,  LC_simm64);
  const OPERAND_VALUE_TYPE pcrel32 =
    ISA_Lit_Opnd_Type_Create("pcrel32", 32, SIGNED, LC_pcrel32);


  /* Registers... */

  const OPERAND_VALUE_TYPE int64 =
    ISA_Reg_Opnd_Type_Create("int64", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE int32 =
    ISA_Reg_Opnd_Type_Create("int32", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     32, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE int16 =
    ISA_Reg_Opnd_Type_Create("int16", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     16, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE int8 =
    ISA_Reg_Opnd_Type_Create("int8", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     8, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE rax =
    ISA_Reg_Opnd_Type_Create("rax", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rax,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE eax =
    ISA_Reg_Opnd_Type_Create("eax", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rax,
			     32, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE ax =
    ISA_Reg_Opnd_Type_Create("ax", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rax,
			     16, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE al =
    ISA_Reg_Opnd_Type_Create("al", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rax,
			     8, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE rdx =
    ISA_Reg_Opnd_Type_Create("rdx", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rdx,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE edx =
    ISA_Reg_Opnd_Type_Create("edx", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rdx,
			     32, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE cl =
    ISA_Reg_Opnd_Type_Create("cl", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rcx,
			     8, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE rsp =
    ISA_Reg_Opnd_Type_Create("rsp", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rsp,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE esp =
    ISA_Reg_Opnd_Type_Create("esp", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rsp,
			     32, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE rbp =
    ISA_Reg_Opnd_Type_Create("rbp", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_rbp,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE r11 =
    ISA_Reg_Opnd_Type_Create("r11", ISA_REGISTER_CLASS_integer,
			     ISA_REGISTER_SUBCLASS_r11,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE fp64 =
    ISA_Reg_Opnd_Type_Create("fp64", ISA_REGISTER_CLASS_float,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE fp128 =
    ISA_Reg_Opnd_Type_Create("fp128", ISA_REGISTER_CLASS_float,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     128, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE x87 =
    ISA_Reg_Opnd_Type_Create("x87", ISA_REGISTER_CLASS_x87,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     128, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE rflags =
    ISA_Reg_Opnd_Type_Create("rflags", ISA_REGISTER_CLASS_rflags,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     64, UNSIGNED, INVALID);
  const OPERAND_VALUE_TYPE eflags =
    ISA_Reg_Opnd_Type_Create("eflags", ISA_REGISTER_CLASS_rflags,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     32, UNSIGNED, INVALID);
  const OPERAND_VALUE_TYPE x87_cw =
    ISA_Reg_Opnd_Type_Create("x87_cw", ISA_REGISTER_CLASS_x87_cw,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     16, UNSIGNED, INVALID);

  const OPERAND_VALUE_TYPE mmx =
    ISA_Reg_Opnd_Type_Create("mmx", ISA_REGISTER_CLASS_mmx,
                             ISA_REGISTER_SUBCLASS_UNDEFINED,
                             64, SIGNED, INVALID);
  /* Enums */

  const OPERAND_VALUE_TYPE pfhint =
    ISA_Enum_Opnd_Type_Create("pfhint", 8, UNSIGNED, EC_pfhint);

  /* Operand uses... */

  // a qualifying predicate
  const OPERAND_USE_TYPE predicate = Create_Operand_Use("predicate");
  // a base address (for memory insts)
  const OPERAND_USE_TYPE base = Create_Operand_Use("base");
  // an index added to a base
  const OPERAND_USE_TYPE index = Create_Operand_Use("index");
  // a scale to be multiplied with index 
  const OPERAND_USE_TYPE scale = Create_Operand_Use("scale");
  // an offset added to a base (implies immed val)
  const OPERAND_USE_TYPE offset = Create_Operand_Use("offset");
  // the target of a branch
  const OPERAND_USE_TYPE target = Create_Operand_Use("target");
  // value to be stored
  const OPERAND_USE_TYPE storeval = Create_Operand_Use("storeval");
  // first/left operand or result of an alu operator
  const OPERAND_USE_TYPE opnd1 = Create_Operand_Use("opnd1");
  // second/right operand of an alu operator
  const OPERAND_USE_TYPE opnd2 = Create_Operand_Use("opnd2");
  // third operand of an alu operator
  const OPERAND_USE_TYPE opnd3 = Create_Operand_Use("opnd3");
  // addend/subtrahend operand of a madd
  const OPERAND_USE_TYPE maddend = Create_Operand_Use("maddend");

  Instruction_Group("no operand",
		    TOP_nop,
		    TOP_noop,
		    TOP_enter,
		    TOP_ret,
		    TOP_asm,
		    TOP_begin_pregtn,
		    TOP_end_pregtn,
		    TOP_fwd_bar,
		    TOP_bwd_bar,
		    TOP_mfence,
		    TOP_lfence,
		    TOP_sfence,
		    TOP_emms,
		    TOP_UNDEFINED);

  Instruction_Group("ret imm16",
		    TOP_reti,
		    TOP_UNDEFINED);
  Operand( 0, simm16, opnd1 );

  Instruction_Group("int load imm32",
		    TOP_ldc32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, simm32, opnd1);

  Instruction_Group("int load imm64",
		    TOP_ldc64,
		    TOP_movabsq,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, simm64, opnd1);

  Instruction_Group( "int8 arithmetic",
		     TOP_and8,
		     TOP_or8,
		     TOP_xor8,
		     TOP_UNDEFINED);
  Result(0, int8);
  Operand(0, int8, opnd1);
  Operand(1, int8, opnd2);


  Instruction_Group( "int16 arithmetic",
		     TOP_and16,
		     TOP_or16,
		     TOP_xor16,
		     TOP_UNDEFINED);
  Result(0, int16);
  Operand(0, int16, opnd1);
  Operand(1, int16, opnd2);

  Instruction_Group( "int32 arithmetic",
		     TOP_add32,
		     TOP_adc32,
		     TOP_and32,
		     TOP_or32,
		     TOP_sub32,
		     TOP_sbb32,
		     TOP_xor32,
		     TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);

  Instruction_Group( "int64 arithmetic",
		     TOP_add64,
		     TOP_and64,
		     TOP_or64,
		     TOP_sub64,
		     TOP_xor64,
		     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);

  Instruction_Group( "vector arithmetic",
  		     TOP_mul128v16,
		     TOP_add128v8,
		     TOP_add128v16,
		     TOP_add128v32,
		     TOP_add128v64,
		     TOP_fadd128v32,
		     TOP_fadd128v64,
		     TOP_fhadd128v32,
		     TOP_fhadd128v64,
		     TOP_faddsub128v32,
		     TOP_faddsub128v64,
		     TOP_sub128v8,
		     TOP_sub128v16,
		     TOP_sub128v32,
		     TOP_sub128v64,
		     TOP_fsub128v32,
		     TOP_fsub128v64,
		     TOP_fhsub128v32,
		     TOP_fhsub128v64,
		     TOP_and128v8,
		     TOP_and128v16,
		     TOP_and128v32,
		     TOP_and128v64,
		     TOP_fand128v32,
		     TOP_fand128v64,
		     TOP_or128v8,
		     TOP_or128v16,
		     TOP_or128v32,
		     TOP_or128v64,
		     TOP_for128v32,
		     TOP_for128v64,
		     TOP_xor128v8,
		     TOP_xor128v16,
		     TOP_xor128v32,
		     TOP_xor128v64,
		     TOP_fxor128v32,
		     TOP_fxor128v64,
		     TOP_xorps,
		     TOP_xorpd,
		     TOP_orps,
		     TOP_orpd,
		     TOP_andps,
		     TOP_andpd,
		     TOP_fmax128v32,
		     TOP_fmax128v64,
		     TOP_fmin128v32,
		     TOP_fmin128v64,
		     TOP_fdiv128v32,
		     TOP_fdiv128v64,
		     TOP_fmul128v32,
		     TOP_fmul128v64,
		     TOP_subus128v16,
		     TOP_cmpgt128v8,
		     TOP_cmpgt128v16,
		     TOP_cmpgt128v32,
		     TOP_cmpeq128v8,
		     TOP_cmpeq128v16,
		     TOP_cmpeq128v32,
		     TOP_max128v8,
		     TOP_max128v16,
		     TOP_min128v8,
		     TOP_min128v16,
		     TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, fp128, opnd2);

  Instruction_Group( "vector arithmetic mem opnd",
			   TOP_addx128v8,
			   TOP_addx128v16,
			   TOP_addx128v32,
			   TOP_addx128v64,
			   TOP_faddx128v32,
			   TOP_faddx128v64,
			   TOP_fhaddx128v32,
			   TOP_fhaddx128v64,
			   TOP_faddsubx128v32,
			   TOP_faddsubx128v64,
			   TOP_andx128v8,
			   TOP_andx128v16,
			   TOP_andx128v32,
			   TOP_andx128v64,
			   TOP_fandx128v32,
			   TOP_fandx128v64,
			   TOP_orx128v8,
			   TOP_orx128v16,
			   TOP_orx128v32,
			   TOP_orx128v64,
			   TOP_forx128v32,
			   TOP_forx128v64,
			   TOP_xorx128v8,
			   TOP_xorx128v16,
			   TOP_xorx128v32,
			   TOP_xorx128v64,
			   TOP_fxorx128v32,
			   TOP_fxorx128v64,
			   TOP_fmaxx128v32,
			   TOP_fmaxx128v64,
			   TOP_fminx128v32,
			   TOP_fminx128v64,
			   TOP_fdivx128v32,
			   TOP_fdivx128v64,
			   TOP_fmulx128v32,
			   TOP_fmulx128v64,
			   TOP_subx128v8,
			   TOP_subx128v16,
			   TOP_subx128v32,
			   TOP_subx128v64,
			   TOP_fsubx128v32,
			   TOP_fsubx128v64,
			   TOP_fhsubx128v32,
		           TOP_fhsubx128v64,
		           TOP_cmpgtx128v8,
		           TOP_cmpgtx128v16,
		           TOP_cmpgtx128v32,
		           TOP_cmpeqx128v8,
		           TOP_cmpeqx128v16,
		           TOP_cmpeqx128v32,
		           TOP_maxx128v8,
		           TOP_maxx128v16,
		           TOP_minx128v8,
		           TOP_minx128v16,
			   TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  
  Instruction_Group( "vector arithmetic mem opnd w/ scaled index",
			   TOP_addxx128v8,
			   TOP_addxx128v16,
			   TOP_addxx128v32,
			   TOP_addxx128v64,
			   TOP_faddxx128v32,
			   TOP_faddxx128v64,
			   TOP_fhaddxx128v32,
			   TOP_fhaddxx128v64,
			   TOP_faddsubxx128v32,
			   TOP_faddsubxx128v64,
			   TOP_andxx128v8,
			   TOP_andxx128v16,
			   TOP_andxx128v32,
			   TOP_andxx128v64,
			   TOP_fandxx128v32,
			   TOP_fandxx128v64,
			   TOP_orxx128v8,
			   TOP_orxx128v16,
			   TOP_orxx128v32,
			   TOP_orxx128v64,
			   TOP_forxx128v32,
			   TOP_forxx128v64,
			   TOP_xorxx128v8,
			   TOP_xorxx128v16,
			   TOP_xorxx128v32,
			   TOP_xorxx128v64,
			   TOP_fxorxx128v32,
			   TOP_fxorxx128v64,
			   TOP_fmaxxx128v32,
			   TOP_fmaxxx128v64,
			   TOP_fminxx128v32,
			   TOP_fminxx128v64,
			   TOP_fdivxx128v32,
			   TOP_fdivxx128v64,
			   TOP_fmulxx128v32,
			   TOP_fmulxx128v64,
			   TOP_subxx128v8,
			   TOP_subxx128v16,
			   TOP_subxx128v32,
			   TOP_subxx128v64,
			   TOP_fsubxx128v32,
			   TOP_fsubxx128v64,
			   TOP_fhsubxx128v32,
			   TOP_fhsubxx128v64,
		           TOP_cmpgtxx128v8,
		           TOP_cmpgtxx128v16,
		           TOP_cmpgtxx128v32,
		           TOP_cmpeqxx128v8,
		           TOP_cmpeqxx128v16,
		           TOP_cmpeqxx128v32,
		           TOP_maxxx128v8,
		           TOP_maxxx128v16,
		           TOP_minxx128v8,
		           TOP_minxx128v16,
			   TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);
  
  Instruction_Group( "vector arithmetic mem opnd w/ scaled index w/o base",
			   TOP_addxxx128v8,
			   TOP_addxxx128v16,
			   TOP_addxxx128v32,
			   TOP_addxxx128v64,
			   TOP_faddxxx128v32,
			   TOP_faddxxx128v64,
			   TOP_fhaddxxx128v32,
			   TOP_fhaddxxx128v64,
			   TOP_faddsubxxx128v32,
			   TOP_faddsubxxx128v64,
			   TOP_andxxx128v8,
			   TOP_andxxx128v16,
			   TOP_andxxx128v32,
			   TOP_andxxx128v64,
			   TOP_fandxxx128v32,
			   TOP_fandxxx128v64,
			   TOP_orxxx128v8,
			   TOP_orxxx128v16,
			   TOP_orxxx128v32,
			   TOP_orxxx128v64,
			   TOP_forxxx128v32,
			   TOP_forxxx128v64,
			   TOP_xorxxx128v8,
			   TOP_xorxxx128v16,
			   TOP_xorxxx128v32,
			   TOP_xorxxx128v64,
			   TOP_fxorxxx128v32,
			   TOP_fxorxxx128v64,
			   TOP_fmaxxxx128v32,
			   TOP_fmaxxxx128v64,
			   TOP_fminxxx128v32,
			   TOP_fminxxx128v64,
			   TOP_fdivxxx128v32,
			   TOP_fdivxxx128v64,
			   TOP_fmulxxx128v32,
			   TOP_fmulxxx128v64,
			   TOP_subxxx128v8,
			   TOP_subxxx128v16,
			   TOP_subxxx128v32,
			   TOP_subxxx128v64,
			   TOP_fsubxxx128v32,
			   TOP_fsubxxx128v64,
			   TOP_fhsubxxx128v32,
			   TOP_fhsubxxx128v64,
		           TOP_cmpgtxxx128v8,
		           TOP_cmpgtxxx128v16,
		           TOP_cmpgtxxx128v32,
		           TOP_cmpeqxxx128v8,
		           TOP_cmpeqxxx128v16,
		           TOP_cmpeqxxx128v32,
		           TOP_maxxxx128v8,
		           TOP_maxxxx128v16,
		           TOP_minxxx128v8,
		           TOP_minxxx128v16,
			   TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group( "int8 arithmetic mem opnd",
		     TOP_xorx8,
		     TOP_orx8,
		     TOP_andx8,
		     TOP_UNDEFINED);
  Result(0, int8);
  Operand(0, int8, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group( "int16 arithmetic mem opnd",
		     TOP_xorx16,
		     TOP_orx16,
		     TOP_andx16,
		     TOP_UNDEFINED);
  Result(0, int16);
  Operand(0, int16, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group( "int32 arithmetic mem opnd",
		     TOP_addx32,
		     TOP_subx32,
		     TOP_xorx32,
		     TOP_orx32,
		     TOP_andx32,
		     TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group( "int64 arithmetic mem opnd",
		     TOP_addx64,
		     TOP_subx64,
		     TOP_xorx64,
		     TOP_orx64,
		     TOP_andx64,
		     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group( "int8 arithmetic mem opnd w/ scaled index",
		     TOP_xorxx8,
		     TOP_orxx8,
		     TOP_andxx8,
		     TOP_UNDEFINED);
  Result(0, int8);
  Operand(0, int8,  opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group( "int16 arithmetic mem opnd w/ scaled index",
		     TOP_xorxx16,
		     TOP_orxx16,
		     TOP_andxx16,
		     TOP_UNDEFINED);
  Result(0, int16);
  Operand(0, int16, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group( "int32 arithmetic mem opnd w/ scaled index",
		     TOP_addxx32,
		     TOP_subxx32,
		     TOP_xorxx32,
		     TOP_orxx32,
		     TOP_andxx32,
		     TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group( "int64 arithmetic mem opnd w/ scaled index",
		     TOP_addxx64,
		     TOP_subxx64,
		     TOP_xorxx64,
		     TOP_orxx64,
		     TOP_andxx64,
		     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group( "int8 arithmetic mem opnd w/ scaled index w/o base",
		     TOP_xorxxx8,
		     TOP_orxxx8,
		     TOP_andxxx8,
		     TOP_UNDEFINED);
  Result(0, int8);
  Operand(0, int8,  opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group( "int16 arithmetic mem opnd w/ scaled index w/o base",
		     TOP_xorxxx16,
		     TOP_orxxx16,
		     TOP_andxxx16,
		     TOP_UNDEFINED);
  Result(0, int16);
  Operand(0, int16, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group( "int32 arithmetic mem opnd w/ scaled index w/o base",
		     TOP_addxxx32,
		     TOP_subxxx32,
		     TOP_xorxxx32,
		     TOP_orxxx32,
		     TOP_andxxx32,
		     TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group( "int64 arithmetic mem opnd w/ scaled index w/o base",
		     TOP_addxxx64,
		     TOP_subxxx64,
		     TOP_xorxxx64,
		     TOP_orxxx64,
		     TOP_andxxx64,
		     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group( "unary int8 arithmetic",
  		     TOP_inc8,
		     TOP_dec8,
		     TOP_UNDEFINED);
  Result(0, int8);
  Operand(0, int8, opnd1);

  Instruction_Group( "unary int16 arithmetic",
  		     TOP_inc16,
  		     TOP_dec16,
		     TOP_UNDEFINED);
  Result(0, int16);
  Operand(0, int16, opnd1);

  Instruction_Group( "unary int32 arithmetic",
		     TOP_neg32,
		     TOP_not32,
  		     TOP_inc32,
		     TOP_dec32,
		     TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);

  Instruction_Group( "unary int64 arithmetic",
		     TOP_neg64,
		     TOP_not64,
		     TOP_inc64,
		     TOP_dec64,
		     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);

  Instruction_Group("int32 arithmetic with simm",
		    TOP_xori32,
		    TOP_ori32,
		    TOP_andi32,
		    TOP_addi32,
		    TOP_adci32,
		    TOP_subi32,
		    TOP_sbbi32,
		    TOP_imuli32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, simm32, opnd2);
  
  Instruction_Group("int64 arithmetic with simm",
		    TOP_xori64,
		    TOP_ori64,
		    TOP_andi64,
		    TOP_addi64,
		    TOP_subi64,
		    TOP_imuli64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, simm32, opnd2);

  Instruction_Group("int8 load w/o base or index",
		    TOP_ld8_m,
		    TOP_UNDEFINED);
  Result(0, al);
  Operand(0, simm32, offset);

  Instruction_Group("int16 load w/o base or index",
		    TOP_ld16_m,
		    TOP_UNDEFINED);
  Result(0, ax);
  Operand(0, simm32, offset);


  Instruction_Group("int32 load w/o base or index",
		    TOP_ld32_m,
		    TOP_UNDEFINED);
  Result(0, eax);
  Operand(0, simm32, offset);

  Instruction_Group("int32 load w/o base or index",
		    TOP_ld8_32_n32,
		    TOP_ldu8_32_n32,
		    TOP_ld16_32_n32,
		    TOP_ldu16_32_n32,
		    TOP_ld32_n32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, simm32, offset);

  Instruction_Group("int64 load w/o base or index",
		    TOP_ld64_m,
		    TOP_UNDEFINED);
  Result(0, rax);
  Operand(0, simm32, offset);

  Instruction_Group("int32 load",
		    TOP_ld8_32,
		    TOP_ldu8_32,
		    TOP_ld16_32,
		    TOP_ldu16_32,
		    TOP_ld32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("int64 load",
		    TOP_ld8_64,
		    TOP_ldu8_64,
		    TOP_ld16_64,
		    TOP_ldu16_64,
		    TOP_ld32_64,
		    TOP_ld64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("int8 store w/o base or index",
		    TOP_store8_m,
		    TOP_UNDEFINED);
  Operand(0, al, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int8 store w/o base or index",
		    TOP_store8_n32,
		    TOP_UNDEFINED);
  Operand(0, int8, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int16 store w/o base or index",
		    TOP_store16_m,
		    TOP_UNDEFINED);
  Operand(0, ax, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int16 store w/o base or index",
		    TOP_store16_n32,
		    TOP_UNDEFINED);
  Operand(0, int16, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int32 store w/o base or index",
		    TOP_store32_m,
		    TOP_UNDEFINED);
  Operand(0, eax, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int32 store w/o base or index",
		    TOP_store32_n32,
		    TOP_UNDEFINED);
  Operand(0, int32, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int64 store w/o base or index",
		    TOP_store64_m,
		    TOP_UNDEFINED);
  Operand(0, rax, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int8 store",
		    TOP_store8,
		    TOP_UNDEFINED);
  Operand(0, int8, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int16 store",
		    TOP_store16,
		    TOP_UNDEFINED);
  Operand(0, int16, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int32 store",
		    TOP_store32,
		    TOP_storenti32,
		    TOP_UNDEFINED);
  Operand(0, int32, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int64 store",
		    TOP_store64,
		    TOP_storenti64,
		    TOP_UNDEFINED);
  Operand(0, int64, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("prefetch",
		    TOP_prefetch,
		    TOP_prefetchw,
		    TOP_prefetcht0,
		    TOP_prefetcht1,
		    TOP_prefetchnta,
		    TOP_UNDEFINED);
  Operand(0, pfhint);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("prefetchx",
		    TOP_prefetchx,
		    TOP_prefetchwx,
		    TOP_prefetcht0x,
		    TOP_prefetcht1x,
		    TOP_prefetchntax,
		    TOP_UNDEFINED);
  Operand(0, pfhint);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  Operand(3, int64, index);
  Operand(4, uimm8, scale);

  Instruction_Group("prefetchxx",
		    TOP_prefetchxx,
		    TOP_prefetchwxx,
		    TOP_prefetcht0xx,
		    TOP_prefetcht1xx,
		    TOP_prefetchntaxx,
		    TOP_UNDEFINED);
  Operand(0, pfhint);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("int8_32 mov",
		    TOP_movsbl,
		    TOP_movzbl,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int8, opnd1);

  Instruction_Group("int16_32 mov",
		    TOP_movswl,
		    TOP_movzwl,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int16, opnd1);

  Instruction_Group("int8_64 mov",
		    TOP_movsbq,
		    TOP_movzbq,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int8, opnd1);

  Instruction_Group("int16_64 mov",
		    TOP_movswq,
		    TOP_movzwq,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int16, opnd1);

  Instruction_Group("int32_64 sign-ext mov",
		    TOP_movslq,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int32, opnd1);

  // Print the result as a 32-bit register even though the result is 64-bit,
  // since CG emits mov32 for movzlq.  mov32 expects a 32-bit result.
  Instruction_Group("int32_64 zero-ext mov",
		    TOP_movzlq,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);

  Instruction_Group("int32 mov",
		    TOP_mov32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);

  Instruction_Group("int64 mov",
		    TOP_mov64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);

  Instruction_Group("int32 mult",
		    TOP_imul32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);

  Instruction_Group("int64 mult",
		    TOP_imul64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);

  Instruction_Group("int32 mult uses eax and edx registers",
		    TOP_mul32,
		    TOP_imulx32,
		    TOP_UNDEFINED);
  Result(0, eax);
  Result(1, edx);
  Operand(0, eax, opnd1);
  Operand(1, int32, opnd2);

  Instruction_Group("int64 mult uses rax and rdx registers",
		    TOP_imulx64,
		    TOP_mulx64,
		    TOP_UNDEFINED);
  Result(0, rax);
  Result(1, rdx);
  Operand(0, rax, opnd1);
  Operand(1, int64, opnd2);

  Instruction_Group("div32",
		    TOP_div32,
		    TOP_UNDEFINED);
  Result(0, eax);
  Result(1, edx);
  Operand(0, eax, opnd1);
  Operand(1, edx, opnd2);
  Operand(2, int32, opnd3);

  Instruction_Group("div64",
		    TOP_div64,
		    TOP_UNDEFINED);
  Result(0, rax);
  Result(1, rdx);
  Operand(0, rax, opnd1);
  Operand(1, rdx, opnd2);
  Operand(2, int64, opnd3);

  Instruction_Group("idiv32",
		    TOP_idiv32,
		    TOP_UNDEFINED);
  Result(0, eax);
  Result(1, edx);
  Operand(0, eax, opnd1);
  Operand(1, edx, opnd2);
  Operand(2, int32, opnd3);

  Instruction_Group("idiv64",
		    TOP_idiv64,
		    TOP_UNDEFINED);
  Result(0, rax);
  Result(1, rdx);
  Operand(0, rax, opnd1);
  Operand(1, rdx, opnd2);
  Operand(2, int64, opnd3);

  Instruction_Group("int8 set",
		    TOP_setb,
		    TOP_setae,
		    TOP_setp,
		    TOP_setnp,
		    TOP_sete,
		    TOP_setne,
		    TOP_setbe,
		    TOP_seta,
		    TOP_setl,
		    TOP_setge,
		    TOP_setle,
		    TOP_setg,
		    TOP_UNDEFINED);
  Result(0, int8);
  Operand(0, rflags, opnd1);

  Instruction_Group("cond int mov",
		    TOP_cmovb,
		    TOP_cmovae,
		    TOP_cmovp,
		    TOP_cmovnp,
		    TOP_cmove,
		    TOP_cmovne,
		    TOP_cmovbe,
		    TOP_cmova,
		    TOP_cmovl,
		    TOP_cmovge,
		    TOP_cmovle,
		    TOP_cmovg,
		    TOP_cmovs,
		    TOP_cmovns,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64,  opnd1);
  Operand(1, rflags, opnd2);

  Instruction_Group("cond x87 mov",
		    TOP_fcmovb,
		    TOP_fcmovbe,
		    TOP_fcmovnb,
		    TOP_fcmovnbe,
		    TOP_fcmove,
		    TOP_fcmovne,
		    TOP_fcmovu,
		    TOP_fcmovnu,
		    TOP_UNDEFINED);
  Result(0, x87);
  Operand(0, x87,  opnd1);
  Operand(1, rflags, opnd2);

  Instruction_Group("fp 2 fp",
		    TOP_movsd,
		    TOP_movss,
		    TOP_cvtss2sd,
		    TOP_cvtsd2ss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);

  Instruction_Group("vector cvt",
		    TOP_cvtdq2pd,
		    TOP_cvtdq2ps,
		    TOP_cvtps2pd,
		    TOP_cvtpd2ps,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);

  Instruction_Group("vector cvt w/ trunc",
		    TOP_cvttps2dq,
		    TOP_cvttpd2dq,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);

  Instruction_Group("vector cvt I",
		    TOP_cvtdq2pd_x,
		    TOP_cvtdq2ps_x,
		    TOP_cvtps2pd_x,
		    TOP_cvtpd2ps_x,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("vector cvt w/ trunc I",
		    TOP_cvttps2dq_x,
		    TOP_cvttpd2dq_x,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("vector cvt II",
		    TOP_cvtdq2pd_xx,
		    TOP_cvtdq2ps_xx,
		    TOP_cvtps2pd_xx,
		    TOP_cvtpd2ps_xx,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset); 

  Instruction_Group("vector cvt w/ trunc II",
		    TOP_cvttps2dq_xx,
		    TOP_cvttpd2dq_xx,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset); 

  Instruction_Group("vector cvt III",
		    TOP_cvtdq2pd_xxx,
		    TOP_cvtdq2ps_xxx,
		    TOP_cvtps2pd_xxx,
		    TOP_cvtpd2ps_xxx,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("vector cvt w/ trunc III",
		    TOP_cvttps2dq_xxx,
		    TOP_cvttpd2dq_xxx,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("int32 2 float",
  		    TOP_movg2x,
		    TOP_cvtsi2sd,
		    TOP_cvtsi2ss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int32, opnd1);

  Instruction_Group("int64 2 float",
	  	    TOP_movg2x64,
		    TOP_cvtsi2sdq,
		    TOP_cvtsi2ssq,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int64, opnd1);

  Instruction_Group("fp 2 int32",
	  	    TOP_movx2g,
		    TOP_cvttss2si,
		    TOP_cvttsd2si,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, fp64, opnd1);

  Instruction_Group("fp 2 int64",
		    TOP_movx2g64,
		    TOP_cvttss2siq,
		    TOP_cvttsd2siq,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, fp64, opnd1);

  Instruction_Group("shifts32",
		    TOP_sari32,
		    TOP_shli32,
		    TOP_shri32,
		    TOP_rori8,
		    TOP_rori16,
		    TOP_rori32,
		    TOP_roli8,
		    TOP_roli16,
		    TOP_roli32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, uimm8, opnd2);

  Instruction_Group("shifts64",
		    TOP_sari64,
		    TOP_shli64,
		    TOP_shri64,
		    TOP_rori64,
		    TOP_roli64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, uimm8, opnd2);

  Instruction_Group("variable shifts32",
		    TOP_sar32,
		    TOP_shl32,
		    TOP_shr32,
		    TOP_ror8,
		    TOP_ror16,
		    TOP_ror32,
		    TOP_rol8,
		    TOP_rol16,
		    TOP_rol32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, cl,    opnd2);

  Instruction_Group("variable shift left double",
		    TOP_shld32,
		    TOP_shrd32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);
  Operand(2, cl,    opnd3);

  Instruction_Group("shift left double",
		    TOP_shldi32,
		    TOP_shrdi32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);
  Operand(2, uimm8, opnd3);

  Instruction_Group("variable shifts64",
		    TOP_sar64,
		    TOP_shl64,
		    TOP_shr64,
		    TOP_ror64,
		    TOP_rol64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, cl,    opnd2);

  Instruction_Group("sign32 extend",
		    TOP_cltd,
		    TOP_UNDEFINED);
  Result(0, eax);
  Result(1, edx);
  Operand(0, eax, opnd1);


  Instruction_Group("sign64 extend",
		    TOP_cqto,
		    TOP_UNDEFINED);
  Result(0, rax);
  Result(1, rdx);
  Operand(0, rax, opnd1);

  Instruction_Group("float load w/o base or index",
		    TOP_ldsd_n32,
		    TOP_ldss_n32,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, simm32, offset);

  Instruction_Group("float load",
		    TOP_ldsd,
		    TOP_ldss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("float convert",
		    TOP_cvtsd2ss_x,
		    TOP_cvtsi2sd_x,
		    TOP_cvtsi2ss_x,
		    TOP_cvtsi2sdq_x,
		    TOP_cvtsi2ssq_x,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("x87-stack float load",
		    TOP_fld,
		    TOP_UNDEFINED);
  Operand(0, x87);

  Instruction_Group("x87 float load",
		    TOP_flds,
		    TOP_fldl,
		    TOP_fldt,
		    TOP_filds,
		    TOP_fildl,
		    TOP_fildll,
		    TOP_UNDEFINED);
  Result(0, x87);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("x87 float load w/o index or base",
		    TOP_flds_n32,
		    TOP_fldl_n32,
		    TOP_fldt_n32,
		    TOP_UNDEFINED);
  Result(0, x87);
  Operand(0, simm32, offset);

  Instruction_Group("x87 control-word load",
		    TOP_fldcw,
		    TOP_UNDEFINED);
  Result(0, x87_cw);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("x87 control-word store",
		    TOP_fnstcw,
		    TOP_UNDEFINED);
  Operand(0, x87_cw, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("float load vector w/o base or index",
		    TOP_lddqa_n32,
		    TOP_ldapd_n32,
		    TOP_ldaps_n32,
		    TOP_ldlps_n32,
		    TOP_ldlpd_n32,
		    TOP_ldhpd_n32,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, simm32, offset);

  Instruction_Group("float load vector",
		    TOP_lddqa,
		    TOP_lddqu,
		    TOP_ldhps,
		    TOP_ldlps,
		    TOP_ldhpd,
		    TOP_ldlpd,
		    TOP_ldapd,
		    TOP_ldaps,
		    TOP_fmovsldupx,
		    TOP_fmovshdupx,
		    TOP_fmovddupx,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("float store w/o base or index",
		    TOP_stsd_n32,
		    TOP_stss_n32,
		    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("float store",
		    TOP_stsd,
		    TOP_stss,
		    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("float store vector w/o base or index",
		    TOP_stdqa_n32,
		    TOP_stapd_n32,
		    TOP_staps_n32,
		    TOP_stlps_n32,
		    TOP_stlpd_n32,
		    TOP_sthpd_n32,
		    TOP_UNDEFINED);
  Operand(0, fp128, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("float store vector",
		    TOP_stdqa,
		    TOP_stdqu,
		    TOP_sthpd,
		    TOP_stlpd,
		    TOP_sthps,
		    TOP_stlps,
		    TOP_stapd,
		    TOP_staps,
		    TOP_stntpd,
		    TOP_stntps,
		    TOP_storenti128,
		    TOP_storelpd,
		    TOP_UNDEFINED);
  Operand(0, fp128, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("x87 float store",
		    TOP_fistps,
		    TOP_fistpl,
		    TOP_fistpll,
		    TOP_fists,
		    TOP_fistl,
		    TOP_fisttps,
		    TOP_fisttpl,
		    TOP_fisttpll,
		    TOP_UNDEFINED);
  Operand(0, x87, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("x87 store and pop",
		    TOP_fst,
		    TOP_fstp,
		    TOP_UNDEFINED);
  Operand(0, x87, opnd1);

  Instruction_Group("x87 float store",
		    TOP_fstps,
		    TOP_fstpl,
		    TOP_fstpt,
		    TOP_fsts,
		    TOP_fstl,
		    TOP_UNDEFINED);
  Operand(0, x87, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("x87 float store w/o base or index",
		    TOP_fstps_n32,
		    TOP_fstpl_n32,
		    TOP_fstpt_n32,
		    TOP_fsts_n32,
		    TOP_fstl_n32,
		    TOP_UNDEFINED);
  Operand(0, x87, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("unpack",
		    TOP_unpckhps,
		    TOP_unpcklps,
		    TOP_unpckhpd,
		    TOP_unpcklpd,
		    TOP_punpcklbw,
		    TOP_punpcklwd,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, fp128, opnd2);

  Instruction_Group("shuffle",
		    TOP_shufps,
		    TOP_shufpd,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, fp128, opnd2);
  Operand(2, uimm8, opnd3);

  Instruction_Group("shuffle-int",
		    TOP_pshufd,
		    TOP_pshufw,
		    TOP_pshuflw,
		    TOP_pshufhw,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, uimm8, opnd3);

  Instruction_Group("move-high-low",
		    TOP_movlhps,
		    TOP_movhlps,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);

  Instruction_Group("packed shifts w/ immediate",
		    TOP_psrldq,
		    TOP_psrlq128v64, // psrlq with immediate
		    TOP_pslldq,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, simm8, opnd2);

  Instruction_Group("packed shifts",
		    TOP_psllw,
		    TOP_pslld,
		    TOP_psllq,
		    TOP_psrlw,
		    TOP_psrld,
		    TOP_psrlq,
		    TOP_psraw,
		    TOP_psrad,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);
  Operand(1, fp128, opnd2);

  Instruction_Group("float load indexed",
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int64, base);
  Operand(1, int64);

  Instruction_Group("float store indexed",
		    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, int64, base);
  Operand(2, int64);

  Instruction_Group("float arithmetic",
		    TOP_andnps,
		    TOP_andnpd,
		    TOP_addsd,
		    TOP_addss,
		    TOP_divsd,
		    TOP_divss,
		    TOP_subsd,
		    TOP_subss,
		    TOP_maxsd,
		    TOP_maxss,
		    TOP_minsd,
		    TOP_minss,
		    TOP_mulsd,
		    TOP_mulss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);

  Instruction_Group("x87 float exchange",
		    TOP_fxch,
		    TOP_UNDEFINED);
  Operand(0, x87, opnd1);

  Instruction_Group("x87 float arithmetic",
		    TOP_fadd,
		    TOP_faddp,
		    TOP_fsub,
		    TOP_fsubp,
		    TOP_fsubr,
		    TOP_fsubrp,
		    TOP_fmul,
		    TOP_fmulp,
		    TOP_fdiv,
		    TOP_fdivp,
		    TOP_fdivr,
		    TOP_fdivrp,
		    TOP_UNDEFINED);
  Result( 0, x87 );
  Operand(0, x87, opnd1);
  Operand(1, x87, opnd2);

  Instruction_Group("float arithmetic mem operand",
		    TOP_divxss,
		    TOP_divxsd,
		    TOP_addxss,
		    TOP_addxsd,
		    TOP_subxss,
		    TOP_subxsd,
		    TOP_mulxss,
		    TOP_mulxsd,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("float arithmetic mem operand w/ scaled-index",
		    TOP_divxxss,
		    TOP_divxxsd,
		    TOP_addxxss,
		    TOP_addxxsd,
		    TOP_subxxss,
		    TOP_subxxsd,
		    TOP_mulxxsd,
		    TOP_mulxxss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group("float arithmetic mem operand w/ scaled-index w/o base",
		    TOP_divxxxss,
		    TOP_divxxxsd,
		    TOP_addxxxss,
		    TOP_addxxxsd,
		    TOP_subxxxss,
		    TOP_subxxxsd,
		    TOP_mulxxxsd,
		    TOP_mulxxxss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("float unary arithmetic",
		    TOP_sqrtsd,
		    TOP_sqrtss,
		    TOP_rsqrtss,
		    TOP_rcpss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);

  Instruction_Group("float unary arithmetic extended",
		    TOP_frsqrt128v32,
		    TOP_frcp128v32,
		    TOP_fsqrt128v32,
		    TOP_fsqrt128v64,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);

  Instruction_Group("xmm to xmm",
		    TOP_movdq,
		    TOP_movapd,
		    TOP_movaps,
		    TOP_fmovsldup,
		    TOP_fmovshdup,
		    TOP_fmovddup,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128, opnd1);

  Instruction_Group("x87 load const",
		    TOP_fldz,
		    TOP_UNDEFINED);
  Result( 0, x87 );

  Instruction_Group("x87 to x87",
		    TOP_fmov,
		    TOP_fabs,
		    TOP_fsqrt,
		    TOP_fchs,
		    TOP_frndint,
		    TOP_fcos,
		    TOP_fsin,
		    TOP_UNDEFINED);
  Result(0, x87);
  Operand(0, x87, opnd1);

  Instruction_Group("float madd",
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);
  Operand(2, fp64, maddend);

  Instruction_Group("conditional jump",
		    TOP_jb,
		    TOP_jae,
		    TOP_je,
		    TOP_jne,
		    TOP_jbe,
		    TOP_ja,
		    TOP_jge,
		    TOP_jl,
		    TOP_jle,
		    TOP_jg,
		    TOP_jcxz,
		    TOP_jecxz,
		    TOP_jrcxz,
		    TOP_jp,
		    TOP_jnp,
		    TOP_js,
		    TOP_jns,
		    TOP_UNDEFINED);
  Operand(0, rflags,  opnd1);
  Operand(1, pcrel32, target);

  Instruction_Group("pop32",
		    TOP_popl,
		    TOP_UNDEFINED);
  Result(0, int32);

  Instruction_Group("pop64",
		    TOP_popq,
		    TOP_UNDEFINED);
  Result(0, int64);

  Instruction_Group("push32",
		    TOP_pushl,
		    TOP_UNDEFINED);
  Result(0, esp);
  Operand(0, int32, opnd1);

  Instruction_Group("push64",
		    TOP_pushq,
		    TOP_UNDEFINED);
  Result(0, rsp);
  Operand(0, int64, opnd1);

  Instruction_Group("int8 compare/test with simm",
		    TOP_cmpi8,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int8,  opnd1);
  Operand(1, simm32, opnd2);

  Instruction_Group("int16 compare/test with simm",
		    TOP_cmpi16,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int16,  opnd1);
  Operand(1, simm32, opnd2);

  Instruction_Group("int32 compare/test with simm",
		    TOP_testi32,
		    TOP_cmpi32,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int32,  opnd1);
  Operand(1, simm32, opnd2);

  Instruction_Group("int64 compare/test with simm",
		    TOP_testi64,
		    TOP_cmpi64,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, int64,  opnd1);
  Operand(1, simm32, opnd2);

  Instruction_Group("int compare/test with simm and mem opnd",
		    TOP_cmpxi8,
		    TOP_cmpxi16,
		    TOP_cmpxi32,
		    TOP_UNDEFINED );
  Result(0,  eflags);
  Operand(0, int64, base);
  Operand(1, simm32, offset);
  Operand(2, simm32, opnd2);

  Instruction_Group("int64 compare/test with simm and mem opnd",
		    TOP_cmpxi64,
		    TOP_UNDEFINED );
  Result(0,  rflags);
  Operand(0, int64, base);
  Operand(1, simm32, offset);
  Operand(2, simm32, opnd2);

  Instruction_Group("int compare/test with simm and mem opnd scaled-index",
		    TOP_cmpxxi8,
		    TOP_cmpxxi16,
		    TOP_cmpxxi32,
		    TOP_UNDEFINED );
  Result(0,  eflags);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);
  Operand(4, simm32, opnd2);

  Instruction_Group("int64 compare/test with simm and mem opnd scaled-index",
		    TOP_cmpxxi64,
		    TOP_UNDEFINED );
  Result(0,  rflags);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);
  Operand(4, simm32, opnd2);

  Instruction_Group("int compare/test with simm and mem opnd scaled-index w/o base",
		    TOP_cmpxxxi8,
		    TOP_cmpxxxi16,
		    TOP_cmpxxxi32,
		    TOP_UNDEFINED );
  Result(0,  eflags);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);
  Operand(3, simm32, opnd2);

  Instruction_Group("int64 compare/test with simm and mem opnd scaled-index w/o base",
		    TOP_cmpxxxi64,
		    TOP_UNDEFINED );
  Result(0,  rflags);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);
  Operand(3, simm32, opnd2);

  Instruction_Group("int8 compare/test",
  		    TOP_cmp8,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int8, opnd1);
  Operand(1, int8, opnd2);

  Instruction_Group("int16 compare/test",
		    TOP_cmp16,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int16, opnd1);
  Operand(1, int16, opnd2);

  Instruction_Group("int32 compare/test",
		    TOP_cmp32,
		    TOP_test32,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);

  Instruction_Group("int64 compare/test",
		    TOP_cmp64,
		    TOP_test64,
		    TOP_UNDEFINED);
  Result(0,  rflags);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);

   Instruction_Group("int8 compare/test mem opnd",
  		    TOP_cmpx8,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int8, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int16 compare/test mem opnd",
		    TOP_cmpx16,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int16, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int32 compare/test mem opnd",
		    TOP_cmpx32,
		    TOP_testx32,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int32, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int64 compare/test mem opnd",
		    TOP_cmpx64,
		    TOP_testx64,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, int64, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int8 compare/test mem opnd w/ scaled-index",
  		    TOP_cmpxx8,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int8, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group("int16 compare/test mem opnd w/ scaled-index",
		    TOP_cmpxx16,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int16, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group("int32 compare/test mem opnd w/ scaled-index",
		    TOP_cmpxx32,
		    TOP_testxx32,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int32, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group("int64 compare/test mem opnd w/ scaled-index",
		    TOP_cmpxx64,
		    TOP_testxx64,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, int64, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group("int8 compare/test mem opnd w/ scaled-index w/o base",
  		    TOP_cmpxxx8,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int8, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("int16 compare/test mem opnd w/ scaled-index w/o base",
		    TOP_cmpxxx16,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int16, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("int32 compare/test mem opnd w/ scaled-index w/o base",
		    TOP_cmpxxx32,
		    TOP_testxxx32,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, int32, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("int64 compare/test mem opnd w/ scaled-index w/o base",
		    TOP_cmpxxx64,
		    TOP_testxxx64,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, int64, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("fp ordered compare",
		    TOP_comisd,
		    TOP_comiss,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);

  Instruction_Group("fp ordered compare mem opnd",
		    TOP_comixsd,
		    TOP_comixss,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, fp64, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("fp ordered compare mem opnd w/ scaled-index",
		    TOP_comixxsd,
		    TOP_comixxss,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, fp64, opnd1);
  Operand(1, int64, base);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);
  Operand(4, simm32, offset);

  Instruction_Group("fp ordered compare mem opnd w/ scaled-index w/o base",
		    TOP_comixxxsd,
		    TOP_comixxxss,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, fp64, opnd1);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("x87 fp compare",
		    TOP_fucomi,
		    TOP_fucomip,
		    TOP_UNDEFINED);
  Result(0, rflags);
  Operand(0, x87,  opnd1);
  Operand(1, x87,  opnd2);

  Instruction_Group("fp compare",
		    TOP_cmpss,
		    TOP_cmpsd,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64,  opnd1);
  Operand(1, fp64,  opnd2);
  Operand(2, simm8, opnd3);

  Instruction_Group("fp compare I",
		    TOP_cmpeqss,
		    TOP_cmpltss,
		    TOP_cmpless,
		    TOP_cmpunordss,
		    TOP_cmpneqss,
		    TOP_cmpnltss,
		    TOP_cmpnless,
		    TOP_cmpordss,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64,  opnd1);
  Operand(1, fp64,  opnd2);

  Instruction_Group("fp vector compare",
		    TOP_cmpps,
		    TOP_cmppd,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128,  opnd1);
  Operand(1, fp128,  opnd2);
  Operand(2, simm8, opnd3);

  Instruction_Group("fp vector compare I",
		    TOP_cmpeqps,
		    TOP_cmpltps,
		    TOP_cmpleps,
		    TOP_cmpunordps,
		    TOP_cmpneqps,
		    TOP_cmpnltps,
		    TOP_cmpnleps,
		    TOP_cmpordps,
		    TOP_UNDEFINED);
  Result(0, fp128);
  Operand(0, fp128,  opnd1);
  Operand(1, fp128,  opnd2);

  Instruction_Group("load32 effective addr",
		    TOP_lea32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("load64 effective addr",
		    TOP_lea64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("load32 effective addr w/ indx",
		    TOP_leax32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("load64 effective addr w/ indx",
		    TOP_leax64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("load32 effective addr w/ indx w/o base",
		    TOP_leaxx32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("load64 effective addr w/ indx w/o base",
		    TOP_leaxx64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("int32 load w/ indx",
		    TOP_ldx32,
		    TOP_ldx8_32,
		    TOP_ldxu8_32,
		    TOP_ldx16_32,
		    TOP_ldxu16_32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int64, base);
  Operand(1, simm32, offset);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);

  Instruction_Group("int64 load w/ indx",
		    TOP_ldx64,
		    TOP_ldx8_64,
		    TOP_ldxu8_64,
		    TOP_ldx16_64,
		    TOP_ldxu16_64,
		    TOP_ldx32_64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, base);
  Operand(1, simm32, offset);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);

  Instruction_Group("int32 load w/ indx w/o base",
		    TOP_ldxx32,
		    TOP_ldxx8_32,
		    TOP_ldxxu8_32,
		    TOP_ldxx16_32,
		    TOP_ldxxu16_32,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("int64 load w/ indx w/o base",
		    TOP_ldxx64,
		    TOP_ldxx8_64,
		    TOP_ldxxu8_64,
		    TOP_ldxx16_64,
		    TOP_ldxxu16_64,
		    TOP_ldxx32_64,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("int8 store w/ indx",
		    TOP_storex8,
		    TOP_UNDEFINED);
  Operand(0, int8, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  Operand(3, int64, index);
  Operand(4, uimm8, scale);

  Instruction_Group("int16 store w/ indx",
		    TOP_storex16,
		    TOP_UNDEFINED);
  Operand(0, int16, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  Operand(3, int64, index);
  Operand(4, uimm8, scale);

  Instruction_Group("int32 store w/ indx",
		    TOP_storex32,
		    TOP_storentix32,
		    TOP_UNDEFINED);
  Operand(0, int32, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  Operand(3, int64, index);
  Operand(4, uimm8, scale);

  Instruction_Group("int64 store w/ indx",
		    TOP_storex64,
		    TOP_storentix64,
		    TOP_UNDEFINED);
  Operand(0, int64, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  Operand(3, int64, index);
  Operand(4, uimm8, scale);

  Instruction_Group("int8 store w/ indx w/o base",
		    TOP_storexx8,
		    TOP_UNDEFINED);
  Operand(0, int8, storeval);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("int16 store w/ indx w/o base",
		    TOP_storexx16,
		    TOP_UNDEFINED);
  Operand(0, int16, storeval);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("int32 store w/ indx w/o base",
		    TOP_storexx32,
		    TOP_storentixx32,
		    TOP_UNDEFINED);
  Operand(0, int32, storeval);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("int64 store w/ indx w/o base",
		    TOP_storexx64,
		    TOP_storentixx64,
		    TOP_UNDEFINED);
  Operand(0, int64, storeval);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("fp load w/ indx",
		    TOP_ldssx,
		    TOP_ldsdx,
		    TOP_UNDEFINED);
  Result(0,  fp64);
  Operand(0, int64, base);
  Operand(1, simm32, offset);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);

  Instruction_Group("fp convert w/ indx",
		    TOP_cvtsd2ss_xx,
		    TOP_cvtsi2sd_xx,
		    TOP_cvtsi2ss_xx,
		    TOP_cvtsi2sdq_xx,
		    TOP_cvtsi2ssq_xx,
		    TOP_UNDEFINED);
  Result(0,  fp64);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("fp load vector w/ indx",
		    TOP_lddqax,
		    TOP_lddqux,
		    TOP_ldhpsx,
		    TOP_ldlpsx,
		    TOP_ldhpdx,
		    TOP_ldlpdx,
		    TOP_ldapdx,
		    TOP_ldapsx,
		    TOP_fmovsldupxx,
		    TOP_fmovshdupxx,
		    TOP_fmovddupxx,
		    TOP_UNDEFINED);
  Result(0,  fp128);
  Operand(0, int64, base);
  Operand(1, simm32, offset);
  Operand(2, int64, index);
  Operand(3, uimm8, scale);

  Instruction_Group("fp load w/ indx w/o base",
		    TOP_ldssxx,
		    TOP_ldsdxx,
		    TOP_UNDEFINED);
  Result(0,  fp64);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("fp convert w/ indx w/o base",
		    TOP_cvtsd2ss_xxx,
		    TOP_cvtsi2sd_xxx,
		    TOP_cvtsi2ss_xxx,
		    TOP_cvtsi2sdq_xxx,
		    TOP_cvtsi2ssq_xxx,
		    TOP_UNDEFINED);
  Result(0,  fp64);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("fp load vector w/ indx w/o base",
		    TOP_lddqaxx,
		    TOP_lddquxx,
		    TOP_ldhpsxx,
		    TOP_ldlpsxx,
		    TOP_ldhpdxx,
		    TOP_ldlpdxx,
		    TOP_ldapdxx,
		    TOP_ldapsxx,
		    TOP_fmovsldupxxx,
		    TOP_fmovshdupxxx,
		    TOP_fmovddupxxx,
		    TOP_UNDEFINED);
  Result(0,  fp128);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("fp store w/ indx",
		    TOP_stssx,
		    TOP_stsdx,
		    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  Operand(3, int64, index);
  Operand(4, uimm8, scale);

  Instruction_Group("fp store vector w/ indx",
		    TOP_stdqax,
		    TOP_stdqux,
		    TOP_sthpdx,
		    TOP_stlpdx,
		    TOP_sthpsx,
		    TOP_stlpsx,
		    TOP_stapsx,
		    TOP_stapdx,
		    TOP_stntpdx,
		    TOP_stntpsx,
		    TOP_UNDEFINED);
  Operand(0, fp128, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);
  Operand(3, int64, index);
  Operand(4, uimm8, scale);

  Instruction_Group("fp store w/ indx w/o base",
		    TOP_stssxx,
		    TOP_stsdxx,
		    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("fp store vector w/ indx w/o base",
		    TOP_stdqaxx,
		    TOP_stdquxx,
		    TOP_sthpdxx,
		    TOP_stlpdxx,
		    TOP_sthpsxx,
		    TOP_stlpsxx,
		    TOP_stapsxx,
		    TOP_stapdxx,
		    TOP_stntpdxx,
		    TOP_stntpsxx,
		    TOP_UNDEFINED);
  Operand(0, fp128, storeval);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("jump",
		    TOP_jmp,
		    TOP_call,
		    TOP_UNDEFINED);
  Operand(0, pcrel32, target);

  Instruction_Group("jump register",
		    TOP_ijmp,
		    TOP_icall,
		    TOP_UNDEFINED);
  Operand(0, int64, target);


  Instruction_Group("jump mem",
		    TOP_ijmpx,
		    TOP_icallx,
		    TOP_UNDEFINED);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("jump mem w/ scaled index",
		    TOP_ijmpxx,
		    TOP_icallxx,
		    TOP_UNDEFINED);
  Operand(0, int64, base);
  Operand(1, int64, index);
  Operand(2, uimm8, scale);
  Operand(3, simm32, offset);

  Instruction_Group("jump mem w/ scaled index w/o base",
		    TOP_ijmpxxx,
		    TOP_icallxxx,
		    TOP_UNDEFINED);
  Operand(0, int64, index);
  Operand(1, uimm8, scale);
  Operand(2, simm32, offset);

  Instruction_Group("label",
		    TOP_label,
		    TOP_UNDEFINED);
  Operand(0, pcrel32);
  Relocatable(0);

  Instruction_Group("spadjust",
		    TOP_spadjust,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, simm16, opnd2);

  Instruction_Group("savexmms",
	  	    TOP_savexmms,
		    TOP_UNDEFINED);
  Operand(0, rax, opnd1); 
  Operand(1, uimm8);	// gives number of xmm parameters left in vararg part
  Operand(2, int64, base);  
  Operand(3, simm32, offset); // offset for the first save
  Operand(4, pcrel32, target); // label after the savexmms
  Operand(5, r11);

  Instruction_Group("intrncall",
		    TOP_intrncall,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, uimm16);	// the intrinsic ID

  Instruction_Group("pregtn tuple",
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, uimm16);

  Instruction_Group("leave",
		    TOP_leave,
		    TOP_UNDEFINED);
  Result(0, rsp);
  Operand(0, rbp, opnd1);

  Instruction_Group("zero32",
		    TOP_zero32,
		    TOP_UNDEFINED);
  Result(0, int32);

  Instruction_Group("zero64",
		    TOP_zero64,
		    TOP_UNDEFINED);
  Result(0, int64);

  Instruction_Group("xzero",
		    TOP_xzero32,
		    TOP_xzero64,
		    TOP_UNDEFINED);
  Result(0, fp64);

  Instruction_Group("xzerov",
		    TOP_xzero128v32,
		    TOP_xzero128v64,
		    TOP_UNDEFINED);
  Result(0, fp128);

  Instruction_Group( "int32 arithmetic lock",
		     TOP_lock_add32,
		     TOP_lock_adc32,
		     TOP_lock_and32,
		     TOP_lock_or32,
		     TOP_lock_xor32,
		     TOP_lock_sub32,
		     TOP_UNDEFINED);
  Operand(0, int32, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group( "int64 arithmetic lock",
		     TOP_lock_add64,
		     TOP_lock_and64,
		     TOP_lock_or64,
		     TOP_lock_xor64,
		     TOP_lock_sub64,
		     TOP_UNDEFINED);
  Operand(0, int64, opnd1);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int32 compare & exchange mem opnd lock",
		    TOP_lock_cmpxchg32,
		    TOP_UNDEFINED);
  Result(0,  eflags);
  Operand(0, eax, opnd1);
  Operand(1, int32, opnd2);
  Operand(2, int64, base);
  Operand(3, simm32, offset);

  Instruction_Group("int64 compare & exchange mem opnd lock",
		    TOP_lock_cmpxchg64,
		    TOP_UNDEFINED);
  Result(0,  rflags);
  Operand(0, rax, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, int64, base);
  Operand(3, simm32, offset);

  Instruction_Group( "bsf32",
                     TOP_bsf32,
                     TOP_bsr32,
                     TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);

  Instruction_Group( "bsf64",
                     TOP_bsf64,
                     TOP_bsr64,
                     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);

  Instruction_Group("int64 load to mmx",
		    TOP_ld64_2m,
		    TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, int64, base);
  Operand(1, simm32, offset);

  Instruction_Group("int64 load to mmx w/o base or index",
		    TOP_ld64_2m_n32,
		    TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, simm32, offset);

  Instruction_Group("int64 store from mmx",
		    TOP_store64_fm,
		    TOP_UNDEFINED);
  Operand(0, mmx, storeval);
  Operand(1, int64, base);
  Operand(2, simm32, offset);

  Instruction_Group("int64 store from mmx w/o base or index",
		    TOP_store64_fm_n32,
		    TOP_UNDEFINED);
  Operand(0, mmx, storeval);
  Operand(1, simm32, offset);

  Instruction_Group("int64 mov between mmx",
		    TOP_mov64_m,
		    TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, mmx, opnd1);

  Instruction_Group("mmx packed move",
                    TOP_pmovmskb,
                    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, mmx, opnd1);

  Instruction_Group("packed word modification",
		    TOP_pextrw,
		    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, mmx, opnd1);
  Operand(1, uimm8, opnd2);

  Instruction_Group("packed word insertion",
		    TOP_pinsrw,
		    TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, int32, opnd1);
  Operand(1, uimm8, opnd2);

  Instruction_Group( "mmx vector arithmetic",
		     TOP_add64v8,
		     TOP_add64v16,
		     TOP_add64v32,
		     TOP_sub64v8,
		     TOP_sub64v16,
		     TOP_sub64v32,
		     TOP_paddsb,
		     TOP_paddsw,
		     TOP_psubsb,
		     TOP_psubsw,
		     TOP_paddusb,
		     TOP_paddusw,
		     TOP_psubusb,
		     TOP_psubusw,
		     TOP_pmullw,
		     TOP_pmulhw,
		     TOP_pcmpeqb,
		     TOP_pcmpeqw,
		     TOP_pcmpeqd,
		     TOP_pcmpgtb,
		     TOP_pcmpgtw,
		     TOP_pcmpgtd,
		     TOP_punpckhbw,
		     TOP_punpckhwd,
		     TOP_punpckhdq,
		     TOP_punpckl64v8,
		     TOP_punpckl64v16,
		     TOP_punpckl64v32,
		     TOP_packsswb,
		     TOP_packssdw,
		     TOP_packuswb,
		     TOP_pmulhuw,
		     TOP_pavgb,
		     TOP_pavgw,
		     TOP_psadbw,
		     TOP_max64v8,
		     TOP_max64v16,
		     TOP_min64v8,
		     TOP_min64v16,
		     TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, mmx, opnd1);
  Operand(1, mmx, opnd2);

  Instruction_Group("MMX shuffle-int",
		    TOP_pshufw64v16,
		    TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, mmx, opnd1);
  Operand(1, uimm8, opnd3);


  Instruction_Group( "mov int32 2 mmx",
                     TOP_movi32_2m,
                     TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, int32, opnd1);

  Instruction_Group( "mov int64 2 mmx",
                     TOP_movi64_2m,
                     TOP_UNDEFINED);
  Result(0, mmx);
  Operand(0, int64, opnd1);

  Instruction_Group( "mov mmx 2 int32",
                     TOP_movm_2i32,
                     TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, mmx, opnd1);

  Instruction_Group( "mov mmx 2 int64",
                     TOP_movm_2i64,
                     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, mmx, opnd1);
  
  ISA_Operands_End();
  return 0;
}
