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
// Group TOPS with similar Printing format together. 
/////////////////////////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. Binary Operators
//   2. Jump, Branch and Trap operators
//   3. Multiply/Divide operators
//   4. Load/Store machine operators
//   5. FPU arithmetic/convert operators
//   6. FPU branch operators
//   7. FPU comparison operators
//   8. Coprocessor1 (FP) data movement operations
//   9. MIPS IV only arithmetic/cond_move FPU operations
//  10. Compiler IR and fictional operators
//
// Within each Print_Type instructions are listed in alphabetical order and
// as shown in the ISA manual
/////////////////////////////////////
//
//  $Revision: 1.154 $
//  $Date: 05/11/10 18:45:11-08:00 $
//  $Author: tkong@hyalite.keyresearch $
//  $Source: common/targ_info/isa/x8664/SCCS/s.isa_print.cxx $

#include <stddef.h>
#include <string.h>
#include "topcode.h"
#include "isa_print_gen.h"

// Multiple topcodes map to the same assembly name. To disambiguate the 
// topcodes, we append a suffix to the basename. By convention, the 
// suffix starts with an underscore. To get the assembly name we strip off
// the suffix.

main()
{
  ISA_Print_Begin("x8664");

  Define_Macro("END_GROUP", ";");	// end-of-group marker
  Define_Macro("PREDICATE", "(%s)");	// predicate operand format
  Define_Macro("BEGIN_BUNDLE", "");	// bundle introducer
  Define_Macro("END_BUNDLE", ";");	// bundle terminator

  Set_AsmName_Func(NULL);

  /* imul. */
  ISA_PRINT_TYPE imul =  ISA_Print_Type_Create("imul", "%s %s,%s");
  Name();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( imul,
			   TOP_imul32,
			   TOP_imul64,
			   TOP_imuli32,
			   TOP_imuli64,
			   TOP_UNDEFINED );

  /* operand 1 only */
  ISA_PRINT_TYPE op1 =  ISA_Print_Type_Create("op1", "%s %s");
  Name();
  Operand(1);
  Instruction_Print_Group( op1,
			   TOP_imulx32,
			   TOP_imulx64,
			   TOP_mul32,
			   TOP_mulx64,
			   TOP_fstps_n32,
			   TOP_fstpl_n32,
			   TOP_fstpt_n32,
			   TOP_fsts_n32,
			   TOP_fstl_n32,
			   TOP_UNDEFINED );

  /* operand 2 only */
  ISA_PRINT_TYPE op2 =  ISA_Print_Type_Create("op2", "%s %s");
  Name();
  Operand(2);
  Instruction_Print_Group( op2,
			   TOP_div32,
			   TOP_div64,
			   TOP_idiv32,
			   TOP_idiv64,
			   TOP_UNDEFINED );

  /* cmp */

  ISA_PRINT_TYPE cmp =  ISA_Print_Type_Create("cmp", "%s %s,%s");
  Name();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( cmp,
			   TOP_comisd,
			   TOP_comiss,
			   TOP_cmpi8,
			   TOP_cmpi16,
			   TOP_cmpi32,
			   TOP_cmpi64,
			   TOP_cmp8,
			   TOP_cmp16,
			   TOP_cmp32,
			   TOP_cmp64,
			   TOP_test32,
			   TOP_test64,
			   TOP_testi32,
			   TOP_testi64,
			   TOP_fucomi,
			   TOP_fucomip,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpx =  ISA_Print_Type_Create("cmpx", "%s %s(%s),%s");
  Name();
  Operand(2);
  Operand(1);
  Operand(0);
  Instruction_Print_Group( cmpx,
			   TOP_testx32,
			   TOP_testx64,
			   TOP_cmpx8,
			   TOP_cmpx16,
			   TOP_cmpx32,
			   TOP_cmpx64,
			   TOP_comixsd,
			   TOP_comixss,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxi =  ISA_Print_Type_Create("cmpxi", "%s %s,%s(%s)");
  Name();
  Operand(2);
  Operand(1);
  Operand(0);
  Instruction_Print_Group( cmpxi,
			   TOP_cmpxi8,
			   TOP_cmpxi16,
			   TOP_cmpxi32,
			   TOP_cmpxi64,
			   TOP_UNDEFINED );


  ISA_PRINT_TYPE cmpxx =  ISA_Print_Type_Create("cmpxx", "%s %s(%s,%s,%s),%s");
  Name();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Instruction_Print_Group( cmpxx,
			   TOP_testxx32,
			   TOP_testxx64,
			   TOP_cmpxx8,
			   TOP_cmpxx16,
			   TOP_cmpxx32,
			   TOP_cmpxx64,
			   TOP_comixxsd,
			   TOP_comixxss,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxxi =  ISA_Print_Type_Create("cmpxxi", "%s %s,%s(%s,%s,%s)");
  Name();
  Operand(4);
  Operand(3);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( cmpxxi,
			   TOP_cmpxxi8,
			   TOP_cmpxxi16,
			   TOP_cmpxxi32,
			   TOP_cmpxxi64,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxxx =  ISA_Print_Type_Create("cmpxxx", "%s %s(,%s,%s),%s");
  Name();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Instruction_Print_Group( cmpxxx,
			   TOP_testxxx32,
			   TOP_testxxx64,
			   TOP_cmpxxx8,
			   TOP_cmpxxx16,
			   TOP_cmpxxx32,
			   TOP_cmpxxx64,
			   TOP_comixxxsd,
			   TOP_comixxxss,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxxxi =  ISA_Print_Type_Create("cmpxxxi", "%s %s,%s(,%s,%s)");
  Name();
  Operand(3);
  Operand(2);
  Operand(0);
  Operand(1);
  Instruction_Print_Group( cmpxxxi,
			   TOP_cmpxxxi8,
			   TOP_cmpxxxi16,
			   TOP_cmpxxxi32,
			   TOP_cmpxxxi64,
			   TOP_UNDEFINED );

  /* One result / two operands in x86 style */
  ISA_PRINT_TYPE ropop =  ISA_Print_Type_Create("ropop", "%s %s,%s");
  Name();
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropop,
			   TOP_mul128v16,
			   TOP_add32,
			   TOP_adc32,
			   TOP_add64,
			   TOP_add128v8,
			   TOP_add128v16,
			   TOP_add128v32,
			   TOP_add128v64,
			   TOP_add64v8,
			   TOP_add64v16,
			   TOP_add64v32,
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
			   TOP_pmulhuw,
			   TOP_fadd128v32,
			   TOP_fadd128v64,
			   TOP_faddsub128v32,
			   TOP_faddsub128v64,
			   TOP_fhadd128v32,
			   TOP_fhadd128v64,
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
			   TOP_max128v8,
			   TOP_max128v16,
			   TOP_max64v8,
			   TOP_max64v16,
			   TOP_min128v8,
			   TOP_min128v16,
			   TOP_min64v8,
			   TOP_min64v16,
			   TOP_cmpeq128v8,
			   TOP_cmpeq128v16,
			   TOP_cmpeq128v32,
			   TOP_cmpgt128v8,
			   TOP_cmpgt128v16,
			   TOP_cmpgt128v32,
			   TOP_pcmpeqb,
			   TOP_pcmpeqw,
			   TOP_pcmpeqd,
			   TOP_pcmpgtb,
			   TOP_pcmpgtw,
			   TOP_pcmpgtd,
			   TOP_and8,
			   TOP_and16,
			   TOP_and32,
			   TOP_and64,
			   TOP_andnps,
			   TOP_andnpd,
			   TOP_or8,
			   TOP_or16,
			   TOP_or32,
			   TOP_or64,
			   TOP_sub32,
			   TOP_sbb32,
			   TOP_sub64,
			   TOP_sub128v8,
			   TOP_sub128v16,
			   TOP_sub128v32,
			   TOP_sub128v64,
			   TOP_sub64v8,
			   TOP_sub64v16,
			   TOP_sub64v32,
			   TOP_fsub128v32,
			   TOP_fsub128v64,
			   TOP_fhsub128v32,
			   TOP_fhsub128v64,			   
			   TOP_xor8,
			   TOP_xor16,
			   TOP_xor32,
			   TOP_xor64,
			   TOP_addi32,
			   TOP_adci32,
			   TOP_addi64,
			   TOP_subi32,
			   TOP_sbbi32,
			   TOP_subi64,
			   TOP_andi32,
			   TOP_andi64,
			   TOP_ori32,
			   TOP_ori64,
			   TOP_xori32,
			   TOP_xori64,
			   TOP_ror8,
			   TOP_ror16,
			   TOP_ror32,
			   TOP_ror64,
			   TOP_rori8,
			   TOP_rori16,
			   TOP_rori32,
			   TOP_rori64,
			   TOP_rol8,
			   TOP_rol16,
			   TOP_rol32,
			   TOP_rol64,
			   TOP_roli8,
			   TOP_roli16,
			   TOP_roli32,
			   TOP_roli64,
			   TOP_sar32,
			   TOP_sar64,
			   TOP_sari32,
			   TOP_sari64,
			   TOP_shl32,
			   TOP_shld32,
			   TOP_shrd32,
			   TOP_shl64,
			   TOP_shli32,
			   TOP_shli64,
			   TOP_shr32,
			   TOP_shr64,
			   TOP_shri32,
			   TOP_shri64,
			   TOP_addsd,
			   TOP_addss,
			   TOP_divsd,
			   TOP_divss,
			   TOP_subsd,
			   TOP_subss,
			   TOP_mulsd,
			   TOP_mulss,
			   TOP_maxsd,
			   TOP_maxss,
			   TOP_minsd,
			   TOP_minss,
			   TOP_fadd,
			   TOP_faddp,
			   TOP_fmul,
			   TOP_fmulp,
			   TOP_fdiv,
			   TOP_fdivp,
			   TOP_fdivr,
			   TOP_fdivrp,
			   TOP_fsub,
			   TOP_fsubp,
			   TOP_fsubr,
			   TOP_fsubrp,
			   TOP_subus128v16,
			   TOP_cmpeqps,
			   TOP_cmpltps,
			   TOP_cmpleps,
			   TOP_cmpunordps,
			   TOP_cmpneqps,
			   TOP_cmpnltps,
			   TOP_cmpnleps,
			   TOP_cmpordps,
			   TOP_cmpeqss,
			   TOP_cmpltss,
			   TOP_cmpless,
			   TOP_cmpunordss,
			   TOP_cmpneqss,
			   TOP_cmpnltss,
			   TOP_cmpnless,
			   TOP_cmpordss,
			   TOP_packsswb,
			   TOP_packssdw,
			   TOP_packuswb,
			   TOP_pavgb,
			   TOP_pavgw,
			   TOP_psadbw,
			   TOP_UNDEFINED );

  /* One result / two operands in x86 style w/ mem operand */
  ISA_PRINT_TYPE ropmem =  ISA_Print_Type_Create("ropmem", "%s %s(%s),%s");
  Name();
  Operand(2);
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropmem,
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
			   TOP_maxx128v8,
			   TOP_maxx128v16,
			   TOP_minx128v8,
			   TOP_minx128v16,
			   TOP_cmpeqx128v8,
			   TOP_cmpeqx128v16,
			   TOP_cmpeqx128v32,
			   TOP_cmpgtx128v8,
			   TOP_cmpgtx128v16,
			   TOP_cmpgtx128v32,
			   TOP_subx128v8,
			   TOP_subx128v16,
			   TOP_subx128v32,
			   TOP_subx128v64,
			   TOP_fsubx128v32,
			   TOP_fsubx128v64,
			   TOP_fhsubx128v32,
			   TOP_fhsubx128v64,
			   TOP_addx32,
			   TOP_addx64,
			   TOP_subx32,
			   TOP_subx64,
			   TOP_xorx8,
			   TOP_xorx16,
			   TOP_xorx32,
			   TOP_xorx64,
			   TOP_andx8,
			   TOP_andx16,
			   TOP_andx32,
			   TOP_andx64,
			   TOP_orx8,
			   TOP_orx16,
			   TOP_orx32,
			   TOP_orx64,
			   TOP_addxss,
			   TOP_addxsd,
			   TOP_subxss,
			   TOP_subxsd,
			   TOP_mulxss,
			   TOP_mulxsd,
			   TOP_divxss,
			   TOP_divxsd,
			   TOP_UNDEFINED );

  /* One result / two operands in x86 style w/ mem operand w/ scaled-index */
  ISA_PRINT_TYPE ropmemindex =  ISA_Print_Type_Create("ropmemindex", 
						      "%s %s(%s,%s,%s),%s");
  Name();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropmemindex,
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
			   TOP_maxxx128v8,
			   TOP_maxxx128v16,
			   TOP_minxx128v8,
			   TOP_minxx128v16,
			   TOP_cmpeqxx128v8,
			   TOP_cmpeqxx128v16,
			   TOP_cmpeqxx128v32,
			   TOP_cmpgtxx128v8,
			   TOP_cmpgtxx128v16,
			   TOP_cmpgtxx128v32,
			   TOP_subxx128v8,
			   TOP_subxx128v16,
			   TOP_subxx128v32,
			   TOP_subxx128v64,
			   TOP_fsubxx128v32,
			   TOP_fsubxx128v64,
			   TOP_fhsubxx128v32,
			   TOP_fhsubxx128v64,
			   TOP_addxx32,
			   TOP_addxx64,
			   TOP_subxx32,
			   TOP_subxx64,
			   TOP_xorxx8,
			   TOP_xorxx16,
			   TOP_xorxx32,
			   TOP_xorxx64,
			   TOP_orxx8,
			   TOP_orxx16,
			   TOP_orxx32,
			   TOP_orxx64,
			   TOP_andxx8,
			   TOP_andxx16,
			   TOP_andxx32,
			   TOP_andxx64,
			   TOP_addxxss,
			   TOP_addxxsd,
			   TOP_subxxss,
			   TOP_subxxsd,
			   TOP_mulxxss,
			   TOP_mulxxsd,
			   TOP_divxxss,
			   TOP_divxxsd,
			   TOP_UNDEFINED );

  /* One result / two operands in x86 style w/ mem operand w/o base */
  ISA_PRINT_TYPE ropmemindexx =  ISA_Print_Type_Create("ropmemindexx", 
						       "%s %s(,%s,%s),%s");
  Name();
  Operand(3);
  Operand(1);
  Operand(2);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropmemindexx,
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
			   TOP_maxxxx128v8,
			   TOP_maxxxx128v16,
			   TOP_minxxx128v8,
			   TOP_minxxx128v16,
			   TOP_cmpeqxxx128v8,
			   TOP_cmpeqxxx128v16,
			   TOP_cmpeqxxx128v32,
			   TOP_cmpgtxxx128v8,
			   TOP_cmpgtxxx128v16,
			   TOP_cmpgtxxx128v32,
			   TOP_subxxx128v8,
			   TOP_subxxx128v16,
			   TOP_subxxx128v32,
			   TOP_subxxx128v64,
			   TOP_fsubxxx128v32,
			   TOP_fsubxxx128v64,
			   TOP_fhsubxxx128v32,
			   TOP_fhsubxxx128v64,
			   TOP_addxxx32,
			   TOP_addxxx64,
			   TOP_subxxx32,
			   TOP_subxxx64,
			   TOP_xorxxx8,
			   TOP_xorxxx16,
			   TOP_xorxxx32,
			   TOP_xorxxx64,
			   TOP_orxxx8,
			   TOP_orxxx16,
			   TOP_orxxx32,
			   TOP_orxxx64,
			   TOP_andxxx8,
			   TOP_andxxx16,
			   TOP_andxxx32,
			   TOP_andxxx64,
			   TOP_addxxxss,
			   TOP_addxxxsd,
			   TOP_subxxxss,
			   TOP_subxxxsd,
			   TOP_mulxxxss,
			   TOP_mulxxxsd,
			   TOP_divxxxss,
			   TOP_divxxxsd,
			   TOP_UNDEFINED );

  /* One result / three operands in x86 style */
  ISA_PRINT_TYPE ropopop =  ISA_Print_Type_Create("ropopop", "%s %s,%s,%s");
  Name();
  Operand(2);
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropopop,
			   TOP_cmpss,
			   TOP_cmpsd,
			   TOP_cmpps,
			   TOP_cmppd,
			   TOP_shldi32,
			   TOP_shrdi32,
			   TOP_UNDEFINED );

  /* Operand 0 only */
  ISA_PRINT_TYPE op0 =  ISA_Print_Type_Create("op0", "%s %s");
  Name();
  Operand(0);
  Instruction_Print_Group( op0,
			   TOP_reti,
			   TOP_flds_n32,
			   TOP_fldl_n32,
			   TOP_fldt_n32,
			   TOP_pushl,
			   TOP_pushq,
			   TOP_jmp,
			   TOP_ijmp,
			   TOP_call,
			   TOP_icall,
			   TOP_fxch,
			   TOP_fld,
			   TOP_fmov,
			   TOP_fst,
			   TOP_fstp,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE ijmpx = ISA_Print_Type_Create("opop", "%s %s(%s)");
  Name();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( ijmpx,
			   TOP_ijmpx,
			   TOP_icallx,
			   TOP_flds,
			   TOP_fldl,
			   TOP_fldt,
			   TOP_fldcw,
			   TOP_filds,
			   TOP_fildl,
			   TOP_fildll,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE ijmpxx = ISA_Print_Type_Create("opopopop", "%s %s(%s,%s,%s)");
  Name();
  Operand(3);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( ijmpxx,
			   TOP_ijmpxx,
			   TOP_icallxx,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE ijmpxxx = ISA_Print_Type_Create("opopop", "%s %s(,%s,%s)");
  Name();
  Operand(2);
  Operand(0);
  Operand(1);
  Instruction_Print_Group( ijmpxxx,
			   TOP_ijmpxxx,
			   TOP_icallxxx,
			   TOP_UNDEFINED );

  /* No result / two operands */
  ISA_PRINT_TYPE opop =  ISA_Print_Type_Create("opop", "%s %s");
  Name();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( opop,
			   TOP_jb,
			   TOP_jae,
			   TOP_jp,
			   TOP_jnp,
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
			   TOP_js,
			   TOP_jns,
			   TOP_UNDEFINED );

  /* One result / no operand */
  ISA_PRINT_TYPE r =  ISA_Print_Type_Create("r", "%s %s");
  Name();
  Result(0);
  Instruction_Print_Group( r,
			   TOP_not32,
			   TOP_not64,
			   TOP_neg32,
			   TOP_neg64,
			   TOP_inc8,
			   TOP_inc16,
			   TOP_inc32,
			   TOP_inc64,
			   TOP_dec8,
			   TOP_dec16,
			   TOP_dec32,
			   TOP_dec64,
			   TOP_popl,
			   TOP_popq,
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
			   TOP_UNDEFINED );

  /* Zeroing an integer register */
  ISA_PRINT_TYPE rr =  ISA_Print_Type_Create("rr", "%s %s,%s");
  Name();
  Result(0);
  Result(0);
  Instruction_Print_Group( rr,
  			   TOP_zero32,
  			   TOP_zero64,
  			   TOP_xzero32,
  			   TOP_xzero64,
  			   TOP_xzero128v32,
  			   TOP_xzero128v64,
			   TOP_UNDEFINED );

  /* One result / one operand */
  ISA_PRINT_TYPE rop =  ISA_Print_Type_Create("rop", "%s %s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group( rop,
			   TOP_fmovsldup,
			   TOP_fmovshdup,
			   TOP_fmovddup,
			   TOP_movlhps,
			   TOP_movhlps,
			   TOP_ld8_m,
			   TOP_ld16_m,
			   TOP_ld32_m,
			   TOP_ld64_m,
			   TOP_ldss_n32,
			   TOP_ldsd_n32,
			   TOP_ld8_32_n32,
			   TOP_ldu8_32_n32,
			   TOP_ld16_32_n32,
			   TOP_ldu16_32_n32,
			   TOP_ld32_n32,
			   TOP_ldaps_n32,
			   TOP_ldapd_n32,
			   TOP_lddqa_n32,
			   TOP_ldlps_n32,
			   TOP_ldlpd_n32,
			   TOP_ldhpd_n32,
			   TOP_cvttss2si,
			   TOP_cvttsd2si,
			   TOP_cvttss2siq,
			   TOP_cvttsd2siq,
			   TOP_cvtsi2sd,
			   TOP_cvtsi2ss,
			   TOP_cvtsi2sdq,
			   TOP_cvtsi2ssq,
			   TOP_cvtss2sd,
			   TOP_cvtsd2ss,
			   TOP_cvtdq2pd,
			   TOP_cvtdq2ps,
			   TOP_cvtps2pd,
			   TOP_cvtpd2ps,
			   TOP_cvttps2dq,
			   TOP_cvttpd2dq,
			   TOP_ldc32,
			   TOP_ldc64,
			   TOP_movabsq,
			   TOP_mov32,
			   TOP_mov64,
			   TOP_movsbl,
			   TOP_movzbl,
			   TOP_movswl,
			   TOP_movzwl,
			   TOP_movsbq,
			   TOP_movzbq,
			   TOP_movswq,
			   TOP_movzwq,
			   TOP_movslq,
			   TOP_movzlq,
			   TOP_movsd,
			   TOP_movss,
			   TOP_movdq,
			   TOP_movapd,
			   TOP_movaps,
			   TOP_movx2g64,
			   TOP_movx2g,
			   TOP_movg2x64,
			   TOP_movg2x,
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
			   TOP_frcp128v32,
			   TOP_frsqrt128v32,
			   TOP_sqrtsd,
			   TOP_sqrtss,
			   TOP_rsqrtss,
			   TOP_fsqrt128v32,
			   TOP_fsqrt128v64,
			   TOP_rcpss,
			   TOP_fcmovb,
			   TOP_fcmovbe,
			   TOP_fcmovnb,
			   TOP_fcmovnbe,
			   TOP_fcmove,
			   TOP_fcmovne,
			   TOP_fcmovu,
			   TOP_fcmovnu,
			   TOP_bsf32,
			   TOP_bsf64,
			   TOP_bsr32,
			   TOP_bsr64,
			   TOP_mov64_m,
			   TOP_pmovmskb,
			   TOP_movi32_2m,
			   TOP_movi64_2m,
			   TOP_movm_2i32,
			   TOP_movm_2i64,
			   TOP_ld64_2m_n32,
			   TOP_UNDEFINED );

  /* Two operands / no result */
  ISA_PRINT_TYPE opop1 =  ISA_Print_Type_Create("opop1", "%s %s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group( opop1,
			   TOP_store8_m,
			   TOP_store16_m,
			   TOP_store32_m,
			   TOP_store64_m,
			   TOP_stss_n32,
			   TOP_stsd_n32,
			   TOP_store8_n32,
			   TOP_store16_n32,
			   TOP_store32_n32,
			   TOP_staps_n32,
			   TOP_stapd_n32,
			   TOP_stdqa_n32,
			   TOP_stlps_n32,
			   TOP_stlpd_n32,
			   TOP_sthpd_n32,
			   TOP_store64_fm_n32,
			   TOP_UNDEFINED );

#if 0
  /* No result / three operands */
  ISA_PRINT_TYPE opopop =  ISA_Print_Type_Create("opopop", "%s %s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( opopop,
			   TOP_UNDEFINED );
#endif

  /* regular load */
  ISA_PRINT_TYPE load =  ISA_Print_Type_Create("load", "%s %s(%s),%s");
  Name();
  Result(0);
  Operand(1);
  Operand(0);
  Instruction_Print_Group( load,
			   TOP_ld8_32,
			   TOP_ldu8_32,
			   TOP_ld16_32,
			   TOP_ldu16_32,
			   TOP_ld8_64,
			   TOP_ldu8_64,
			   TOP_ld16_64,
			   TOP_ldu16_64,
			   TOP_ld32,
			   TOP_ld32_64,
			   TOP_ld64,
			   TOP_ldsd,
			   TOP_ldss,
			   TOP_cvtsd2ss_x,
			   TOP_cvtsi2sd_x,
			   TOP_cvtsi2ss_x,
			   TOP_cvtsi2sdq_x,
			   TOP_cvtsi2ssq_x,
			   TOP_cvtdq2pd_x,
			   TOP_cvtdq2ps_x,
			   TOP_cvtps2pd_x,
			   TOP_cvtpd2ps_x,
			   TOP_cvttps2dq_x,
			   TOP_cvttpd2dq_x,
			   TOP_lddqa,
			   TOP_lddqu,
			   TOP_ldlps,
			   TOP_ldhps,
			   TOP_ldlpd,
			   TOP_ldhpd,
			   TOP_ldapd,
			   TOP_ldaps,
			   TOP_lea32,
			   TOP_lea64,
			   TOP_fmovsldupx,
			   TOP_fmovshdupx,
			   TOP_fmovddupx,			   
			   TOP_ld64_2m,
			   TOP_UNDEFINED );

  /* lea instruction with indx */
  ISA_PRINT_TYPE leax =  ISA_Print_Type_Create("leax", "%s %s(%s,%s,%s), %s");
  Name();
  Operand(3);
  Operand(0);
  Operand(1);
  Operand(2);
  Result(0);
  Instruction_Print_Group( leax,
		  	   TOP_leax32,
			   TOP_leax64,
			   TOP_cvtsd2ss_xx,
			   TOP_cvtsi2sd_xx,
			   TOP_cvtsi2ss_xx,
			   TOP_cvtsi2sdq_xx,
			   TOP_cvtsi2ssq_xx,
			   TOP_cvtdq2pd_xx,
			   TOP_cvtdq2ps_xx,
			   TOP_cvtps2pd_xx,
			   TOP_cvtpd2ps_xx,
			   TOP_cvttps2dq_xx,
			   TOP_cvttpd2dq_xx,
			   TOP_UNDEFINED );

  /* lea instruction with indx but without base */
  ISA_PRINT_TYPE leaxx =  ISA_Print_Type_Create("leaxx", "%s %s(,%s,%s), %s");
  Name();
  Operand(2);
  Operand(0);
  Operand(1);
  Result(0);
  Instruction_Print_Group( leaxx,
		  	   TOP_leaxx32,
			   TOP_leaxx64,
			   TOP_cvtsd2ss_xxx,
			   TOP_cvtsi2sd_xxx,
			   TOP_cvtsi2ss_xxx,
			   TOP_cvtsi2sdq_xxx,
			   TOP_cvtsi2ssq_xxx,
			   TOP_cvtdq2pd_xxx,
			   TOP_cvtdq2ps_xxx,
			   TOP_cvtps2pd_xxx,
			   TOP_cvtpd2ps_xxx,
			   TOP_cvttps2dq_xxx,
			   TOP_cvttpd2dq_xxx,
			   TOP_UNDEFINED );

  /* load instruction with indx */
  ISA_PRINT_TYPE ldx = ISA_Print_Type_Create("ldx", "%s %s(%s,%s,%s), %s");
  Name();
  Operand(1);
  Operand(0);
  Operand(2);
  Operand(3);
  Result(0);
  Instruction_Print_Group( ldx,
			   TOP_ldx8_32,
			   TOP_ldx8_64,
			   TOP_ldxu8_32,
			   TOP_ldxu8_64,
			   TOP_ldx16_32,
			   TOP_ldx16_64,
			   TOP_ldxu16_32,
			   TOP_ldxu16_64,
			   TOP_ldx32,
			   TOP_ldx64,
			   TOP_ldssx,
			   TOP_ldsdx,
			   TOP_lddqax,
			   TOP_lddqux,
			   TOP_ldlpsx,
			   TOP_ldhpsx,
			   TOP_ldlpdx,
			   TOP_ldhpdx,
			   TOP_ldx32_64,
			   TOP_ldapdx,
			   TOP_ldapsx,
			   TOP_fmovsldupxx,
			   TOP_fmovshdupxx,
			   TOP_fmovddupxx,			   
			   TOP_UNDEFINED );

  /* load instruction with indx w/o base*/
  ISA_PRINT_TYPE ldxx = ISA_Print_Type_Create("ldxx", "%s %s(,%s,%s), %s");
  Name();
  Operand(2);
  Operand(0);
  Operand(1);
  Result(0);
  Instruction_Print_Group( ldxx,
			   TOP_ldxx8_32,
			   TOP_ldxx8_64,
			   TOP_ldxxu8_32,
			   TOP_ldxxu8_64,
			   TOP_ldxx16_32,
			   TOP_ldxx16_64,
			   TOP_ldxxu16_32,
			   TOP_ldxxu16_64,
			   TOP_ldxx32,
			   TOP_ldxx64,
			   TOP_ldssxx,
			   TOP_ldsdxx,
			   TOP_lddqaxx,
			   TOP_lddquxx,
			   TOP_ldlpsxx,
			   TOP_ldhpsxx,
			   TOP_ldlpdxx,
			   TOP_ldhpdxx,
			   TOP_ldxx32_64,
			   TOP_ldapdxx,
			   TOP_ldapsxx,
			   TOP_fmovsldupxxx,
			   TOP_fmovshdupxxx,
			   TOP_fmovddupxxx,			   
			   TOP_UNDEFINED );

  /* store instruction with indx */
  ISA_PRINT_TYPE storex = ISA_Print_Type_Create("storex", "%s %s,%s(%s,%s,%s)");
  Name();
  Operand(0);
  Operand(2);
  Operand(1);
  Operand(3);
  Operand(4);
  Instruction_Print_Group( storex,
			   TOP_storex8,
			   TOP_storex16,
			   TOP_storex32,
			   TOP_storex64,
			   TOP_storentix32,
			   TOP_storentix64,
			   TOP_stssx,
			   TOP_stsdx,
			   TOP_stdqax,
			   TOP_stntpdx,
			   TOP_stntpsx,
			   TOP_stdqux,
			   TOP_stlpsx,
			   TOP_sthpsx,
			   TOP_stlpdx,
			   TOP_sthpdx,
			   TOP_stapsx,
			   TOP_stapdx,
			   TOP_UNDEFINED );

  /* store instruction with indx w/o base*/
  ISA_PRINT_TYPE storexx = ISA_Print_Type_Create("storexx", "%s %s,%s(,%s,%s)");
  Name();
  Operand(0);
  Operand(3);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( storexx,
			   TOP_storexx8,
			   TOP_storexx16,
			   TOP_storexx32,
			   TOP_storexx64,
			   TOP_storentixx32,
			   TOP_storentixx64,
			   TOP_stssxx,
			   TOP_stsdxx,
			   TOP_stdqaxx,
			   TOP_stdquxx,
			   TOP_stlpsxx,
			   TOP_sthpsxx,
			   TOP_stlpdxx,
			   TOP_sthpdxx,
			   TOP_stntpdxx,
			   TOP_stntpsxx,
			   TOP_stapsxx,
			   TOP_stapdxx,
			   TOP_UNDEFINED );

  /* prefetch */
  ISA_PRINT_TYPE prefetch =  ISA_Print_Type_Create("prefetch", "%s %s(%s)");
  Name();
  Operand(2);
  Operand(1);
  Instruction_Print_Group( prefetch,
			   TOP_prefetch,
			   TOP_prefetchw,
			   TOP_prefetcht0,
			   TOP_prefetcht1,
			   TOP_prefetchnta,
			   TOP_fstps,
			   TOP_fstpl,
			   TOP_fstpt,
			   TOP_fsts,
			   TOP_fstl,
			   TOP_fnstcw,
			   TOP_fistps,
			   TOP_fistpl,
			   TOP_fists,
			   TOP_fistl,
			   TOP_fistpll,
			   TOP_fisttps,
			   TOP_fisttpl,
			   TOP_fisttpll,
			   TOP_UNDEFINED );

  /* prefetch */
  ISA_PRINT_TYPE prefetchx =  ISA_Print_Type_Create("prefetchx", "%s %s(%s,%s,%s)");
  Name();
  Operand(2);
  Operand(1);
  Operand(3);
  Operand(4);
  Instruction_Print_Group( prefetchx,
			   TOP_prefetchx,
			   TOP_prefetchwx,
			   TOP_prefetcht0x,
			   TOP_prefetcht1x,
			   TOP_prefetchntax,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE prefetchxx =  ISA_Print_Type_Create("prefetchxx", "%s %s(,%s,%s)");
  Name();
  Operand(3);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( prefetchxx,
			   TOP_prefetchxx,
			   TOP_prefetchwxx,
			   TOP_prefetcht0xx,
			   TOP_prefetcht1xx,
			   TOP_prefetchntaxx,
			   TOP_UNDEFINED );

  /* regular store */
  ISA_PRINT_TYPE store =  ISA_Print_Type_Create("store", "%s %s,%s(%s)");
  Name();
  Operand(0);
  Operand(2);
  Operand(1);
  Instruction_Print_Group( store,
			   TOP_store8,
			   TOP_store16,
			   TOP_store32,
			   TOP_store64,
			   TOP_storenti32,
			   TOP_storenti64,
			   TOP_stsd,
			   TOP_stss,
			   TOP_stdqa,
			   TOP_stntpd,
			   TOP_stntps,
			   TOP_stdqu,
			   TOP_stlps,
			   TOP_sthps,
			   TOP_stlpd,
			   TOP_sthpd,
			   TOP_staps,
			   TOP_stapd,
			   TOP_store64_fm,
			   TOP_storenti128,
			   TOP_storelpd,
			   TOP_UNDEFINED );

  /* instructions that read-modify-write */
  ISA_PRINT_TYPE rmw =  ISA_Print_Type_Create("rmw", "%s %s,%s(%s)");
  Name();
  Operand(0);
  Operand(2);
  Operand(1);
  Instruction_Print_Group( rmw,
			   TOP_lock_add32,
			   TOP_lock_adc32,
			   TOP_lock_add64,
			   TOP_lock_and32,
			   TOP_lock_and64,
			   TOP_lock_or32,
			   TOP_lock_or64,
			   TOP_lock_xor32,
			   TOP_lock_xor64,
			   TOP_lock_sub32,
			   TOP_lock_sub64,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxchg =  ISA_Print_Type_Create("cmpxchg", "%s %s,%s(%s)");
  Name();
  Operand(1);
  Operand(3);
  Operand(2);
  Instruction_Print_Group( cmpxchg,
			   TOP_lock_cmpxchg32,
			   TOP_lock_cmpxchg64,
			   TOP_UNDEFINED );

  /* unpack ( for replicate ) */
  ISA_PRINT_TYPE unpck = ISA_Print_Type_Create("unpck", "%s %s,%s");
  Name();
  Operand(1);
  Result(0);
  Instruction_Print_Group( unpck,
			   TOP_unpckhpd,
			   TOP_unpckhps,
			   TOP_unpcklpd,
			   TOP_unpcklps,
			   TOP_punpcklwd,
			   TOP_punpcklbw,
			   TOP_punpckhbw,
			   TOP_punpckhwd,
			   TOP_punpckhdq,
			   TOP_punpckl64v8,
			   TOP_punpckl64v16,
			   TOP_punpckl64v32,
			   TOP_UNDEFINED );

  /* shuffle */
  ISA_PRINT_TYPE shuffle = ISA_Print_Type_Create("shuffle", "%s %s,%s,%s");
  Name();
  Operand(2);
  Operand(1);
  Result(0);
  Instruction_Print_Group( shuffle,
			   TOP_shufps,
			   TOP_shufpd,
			   TOP_pshufd,
			   TOP_pshufw,
			   TOP_pshuflw,
			   TOP_pshufhw,
			   TOP_UNDEFINED );

  /* shuffle_int */
  ISA_PRINT_TYPE shuffle_int = 
                          ISA_Print_Type_Create("shuffle_int", "%s %s,%s,%s");
  Name();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( shuffle_int,
			   TOP_pshufd,
			   TOP_pshufw,
			   TOP_pshuflw,
			   TOP_pshufhw,
			   TOP_pextrw,
			   TOP_pinsrw,
			   TOP_pshufw64v16,
			   TOP_UNDEFINED );

  /* shift_packed */
  ISA_PRINT_TYPE shift_packed = ISA_Print_Type_Create("shift_packed", "%s %s,%s");
  Name();
  Operand(1);
  Result(0);
  Instruction_Print_Group( shift_packed,
			   TOP_psrldq,
			   TOP_psrlq128v64,
			   TOP_pslldq,
			   TOP_psllw,
			   TOP_pslld,
			   TOP_psllq,
			   TOP_psrlw,
			   TOP_psrld,
			   TOP_psrlq,
			   TOP_psraw,
			   TOP_psrad,
			   TOP_UNDEFINED );

  /* No results / no operands TODO */
  ISA_PRINT_TYPE no_rop = ISA_Print_Type_Create("no_rop", "%s");
  Name();
  Instruction_Print_Group( no_rop,
			   TOP_nop,
			   TOP_ret,
			   TOP_enter,
			   TOP_leave,
			   TOP_cltd,
			   TOP_cqto,
			   TOP_mfence,
			   TOP_lfence,
			   TOP_sfence,
			   TOP_fchs,
			   TOP_frndint,
			   TOP_fabs,
			   TOP_fsqrt,
			   TOP_fldz,
			   TOP_emms,
			   TOP_fcos,
			   TOP_fsin,
			   TOP_UNDEFINED );

  ISA_Print_End();
}
