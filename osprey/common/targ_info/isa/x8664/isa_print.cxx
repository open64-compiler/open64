/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008 PathScale, LLC.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007 QLogic Corporation.  All Rights Reserved.
 */

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

int main()
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
			   TOP_ucomisd,
			   TOP_ucomiss,
                           TOP_vcomisd,
                           TOP_vcomiss,
                           TOP_vucomisd,
                           TOP_vucomiss,
			   TOP_cmpi8,
			   TOP_cmpi16,
			   TOP_cmpi32,
			   TOP_cmpi64,
			   TOP_cmp8,
			   TOP_cmp16,
			   TOP_cmp32,
			   TOP_cmp64,
			   TOP_test8,
			   TOP_test16,
			   TOP_test32,
			   TOP_test64,
			   TOP_testi8,
			   TOP_testi16,
			   TOP_testi32,
			   TOP_testi64,
			   TOP_fucomi,
			   TOP_fucomip,
                           /* SSE4.1 instructions */
                           TOP_ptest128,
                           /* AVX instructions */
                           TOP_vptest128,
                           TOP_vtestpd,
                           TOP_vtestps,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpx =  ISA_Print_Type_Create("cmpx", "%s %s%s(%s),%s");
  Name();
  Segment();
  Operand(2);
  Operand(1);
  Operand(0);
  Instruction_Print_Group( cmpx,
			   TOP_testx8,
			   TOP_testx16,
			   TOP_testx32,
			   TOP_testx64,
			   TOP_cmpx8,
			   TOP_cmpx16,
			   TOP_cmpx32,
			   TOP_cmpx64,
			   TOP_comixsd,
			   TOP_comixss,
			   TOP_ucomixsd,
			   TOP_ucomixss,
			   TOP_vcomixsd,
			   TOP_vcomixss,
			   TOP_vucomixsd,
			   TOP_vucomixss,
                           /* SSE4.1 instructions */
                           TOP_ptestx128,
                           /* AVX instructions */
                           TOP_vptestx128,
                           TOP_vtestxpd,
                           TOP_vtestxps,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxr =  ISA_Print_Type_Create("cmpxr", "%s %s,%s%s(%s)");
  Name();
  Operand(2);
  Segment();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( cmpxr,
			   TOP_cmpxr8,
			   TOP_cmpxr16,
			   TOP_cmpxr32,
			   TOP_cmpxr64,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxi =  ISA_Print_Type_Create("cmpxi", "%s %s,%s%s(%s)");
  Name();
  Operand(2);
  Segment();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( cmpxi,
			   TOP_cmpxi8,
			   TOP_cmpxi16,
			   TOP_cmpxi32,
			   TOP_cmpxi64,
			   TOP_UNDEFINED );


  ISA_PRINT_TYPE cmpxx =  ISA_Print_Type_Create("cmpxx", "%s %s%s(%s,%s,%s),%s");
  Name();
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Instruction_Print_Group( cmpxx,
			   TOP_testxx8,
			   TOP_testxx16,
			   TOP_testxx32,
			   TOP_testxx64,
			   TOP_cmpxx8,
			   TOP_cmpxx16,
			   TOP_cmpxx32,
			   TOP_cmpxx64,
			   TOP_comixxsd,
			   TOP_comixxss,
			   TOP_ucomixxsd,
			   TOP_ucomixxss,
			   TOP_vcomixxsd,
			   TOP_vcomixxss,
			   TOP_vucomixxsd,
			   TOP_vucomixxss,
                           /* SSE4.1 instructions */
                           TOP_ptestxx128,
                           /* AVX instructions */
                           TOP_vptestxx128,
                           TOP_vtestxxpd,
                           TOP_vtestxxps,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxxr =  ISA_Print_Type_Create("cmpxxr", "%s %s,%s%s(%s,%s,%s)");
  Name();
  Operand(4);
  Segment();
  Operand(3);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( cmpxxr,
			   TOP_cmpxxr8,
			   TOP_cmpxxr16,
			   TOP_cmpxxr32,
			   TOP_cmpxxr64,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxxi =  ISA_Print_Type_Create("cmpxxi", "%s %s,%s%s(%s,%s,%s)");
  Name();
  Operand(4);
  Segment();
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

  ISA_PRINT_TYPE cmpxxx =  ISA_Print_Type_Create("cmpxxx", "%s %s%s(,%s,%s),%s");
  Name();
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Instruction_Print_Group( cmpxxx,
			   TOP_testxxx8,
			   TOP_testxxx16,
			   TOP_testxxx32,
			   TOP_testxxx64,
			   TOP_cmpxxx8,
			   TOP_cmpxxx16,
			   TOP_cmpxxx32,
			   TOP_cmpxxx64,
			   TOP_comixxxsd,
			   TOP_comixxxss,
			   TOP_ucomixxxsd,
			   TOP_ucomixxxss,
			   TOP_vcomixxxsd,
			   TOP_vcomixxxss,
			   TOP_vucomixxxsd,
			   TOP_vucomixxxss,
                           /* SSE4.1 instructions */
                           TOP_ptestxxx128,
                           /* AVX instructions */
                           TOP_vptestxxx128,
                           TOP_vtestxxxpd,
                           TOP_vtestxxxps,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxxxr =  ISA_Print_Type_Create("cmpxxxr", "%s %s,%s%s(,%s,%s)");
  Name();
  Operand(3);
  Segment();
  Operand(2);
  Operand(0);
  Operand(1);
  Instruction_Print_Group( cmpxxxr,
			   TOP_cmpxxxr8,
			   TOP_cmpxxxr16,
			   TOP_cmpxxxr32,
			   TOP_cmpxxxr64,
			   TOP_UNDEFINED );

  /* One result / two operands in x86 style */
  ISA_PRINT_TYPE cmpxxxi =  ISA_Print_Type_Create("cmpxxxi", "%s %s,%s%s(,%s,%s)");
  Name();
  Operand(3);
  Segment();
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
			   TOP_add8,
			   TOP_add16,
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
			   TOP_paddq,
			   TOP_psubsb,
			   TOP_psubsw,
			   TOP_psubq,
			   TOP_paddusb,
			   TOP_paddusw,
			   TOP_psubusb,
			   TOP_psubusw,
			   TOP_pmullw,
			   TOP_pmulhw,
			   TOP_pmulhuw,
			   TOP_pmuludq,
			   TOP_pmaddwd,
			   TOP_pavgb,
			   TOP_pavgw,
			   TOP_psadbw,
			   TOP_paddsb128,
			   TOP_paddsw128,
			   TOP_paddq128,
			   TOP_psubsb128,
			   TOP_psubsw128,
			   TOP_psubq128,
			   TOP_paddusb128,
			   TOP_paddusw128,
			   TOP_psubusb128,
			   TOP_psubusw128,
			   TOP_pmullw128,
			   TOP_pmulhw128,
			   TOP_pmulhuw128,
			   TOP_pmuludq128,
			   TOP_pmaddwd128,
			   TOP_pavgb128,
			   TOP_pavgw128,
			   TOP_psadbw128,
			   TOP_pand,
			   TOP_pandn,
			   TOP_por,
			   TOP_pxor,
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
			   TOP_max64v8,
			   TOP_max64v16,
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
			   TOP_sub8,
			   TOP_sub16,
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
			   TOP_addi8,
			   TOP_addi16,
			   TOP_addi32,
			   TOP_adci32,
			   TOP_addi64,
			   TOP_subi8,
			   TOP_subi16,
			   TOP_subi32,
			   TOP_sbbi32,
			   TOP_subi64,
			   TOP_andi8,
			   TOP_andi16,
			   TOP_andi32,
			   TOP_andi64,
			   TOP_ori8,
			   TOP_ori16,
			   TOP_ori32,
			   TOP_ori64,
			   TOP_xori8,
			   TOP_xori16,
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
			   TOP_cmpeqpd,
			   TOP_cmpltpd,
			   TOP_cmplepd,
			   TOP_cmpunordpd,
			   TOP_cmpneqpd,
			   TOP_cmpnltpd,
			   TOP_cmpnlepd,
			   TOP_cmpordpd,
			   TOP_cmpeqps,
			   TOP_cmpltps,
			   TOP_cmpleps,
			   TOP_cmpunordps,
			   TOP_cmpneqps,
			   TOP_cmpnltps,
			   TOP_cmpnleps,
			   TOP_cmpordps,
			   TOP_cmpeqsd,
			   TOP_cmpltsd,
			   TOP_cmplesd,
			   TOP_cmpunordsd,
			   TOP_cmpneqsd,
			   TOP_cmpnltsd,
			   TOP_cmpnlesd,
			   TOP_cmpordsd,
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
			   TOP_packsswb128,
			   TOP_packssdw128,
			   TOP_packuswb128,
                           /* SSSE3 instructions */
                           TOP_psign128v8,
                           TOP_psign128v16,
                           TOP_psign128v32,
                           TOP_pmulhrsw128,
                           TOP_pmaddubsw128,
                           TOP_phsub128v16,
                           TOP_phsub128v32,
                           TOP_phsubs128v16,
                           TOP_phadd128v16,
                           TOP_phadd128v32,
                           TOP_phadds128v16,
                           /* SSE4.1 instructions */
                           TOP_minu128v8,
                           TOP_maxu128v8,
                           TOP_mins128v8,
                           TOP_maxs128v8,
                           TOP_minu128v16,
                           TOP_maxu128v16,
                           TOP_mins128v16,
                           TOP_maxs128v16,
                           TOP_minu128v32,
                           TOP_maxu128v32,
                           TOP_mins128v32,
                           TOP_maxs128v32,
                           TOP_muldq,
                           TOP_mul128v32,
                           TOP_cmpeq128v64,
                           TOP_packusdw,
                           /* SSE4.2 instructions */
                           TOP_cmpgt128v64,
                           TOP_crc32b,
                           TOP_crc32w,
                           TOP_crc32d,
                           TOP_crc32q,
                           /* AES instructions */
                           TOP_aesenc,
                           TOP_aesenclast,
                           TOP_aesdec,
                           TOP_aesdeclast,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE unarybase = ISA_Print_Type_Create("unarybase", "%s %s%s(%s)");
  Name();
  Segment();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( unarybase,
                           TOP_negxr8,
                           TOP_negxr16,
                           TOP_negxr32,
                           TOP_negxr64,
                           TOP_notxr8,
                           TOP_notxr16,
                           TOP_notxr32,
                           TOP_notxr64,
                           TOP_incxr8,
                           TOP_incxr16,
                           TOP_incxr32,
                           TOP_incxr64,
                           TOP_decxr8,
                           TOP_decxr16,
                           TOP_decxr32,
                           TOP_decxr64,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE unarybaseindex = ISA_Print_Type_Create("unarybaseindex", "%s %s%s(%s,%s,%s)");
  Name();
  Segment();
  Operand(1);
  Operand(0);
  Operand(2);
  Operand(3);
  Instruction_Print_Group( unarybaseindex,
                           TOP_negxxr8,
                           TOP_negxxr16,
                           TOP_negxxr32,
                           TOP_negxxr64,
                           TOP_notxxr8,
                           TOP_notxxr16,
                           TOP_notxxr32,
                           TOP_notxxr64,
                           TOP_incxxr8,
                           TOP_incxxr16,
                           TOP_incxxr32,
                           TOP_incxxr64,
                           TOP_decxxr8,
                           TOP_decxxr16,
                           TOP_decxxr32,
                           TOP_decxxr64,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE unaryindex = ISA_Print_Type_Create("unaryindex", "%s %s%s(,%s,%s)");
  Name();
  Segment();
  Operand(2);
  Operand(0);
  Operand(1);
  Instruction_Print_Group( unaryindex,
                           TOP_negxxxr8,
                           TOP_negxxxr16,
                           TOP_negxxxr32,
                           TOP_negxxxr64,
                           TOP_notxxxr8,
                           TOP_notxxxr16,
                           TOP_notxxxr32,
                           TOP_notxxxr64,
                           TOP_incxxxr8,
                           TOP_incxxxr16,
                           TOP_incxxxr32,
                           TOP_incxxxr64,
                           TOP_decxxxr8,
                           TOP_decxxxr16,
                           TOP_decxxxr32,
                           TOP_decxxxr64,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE unarymem_n32 = ISA_Print_Type_Create("unarymem_n32", "%s %s");
  Name();
  Operand(0);
  Instruction_Print_Group( unarymem_n32,
                           TOP_negxr8_n32,
                           TOP_negxr16_n32,
                           TOP_negxr32_n32,
                           TOP_negxr64_off,
                           TOP_notxr8_n32,
                           TOP_notxr16_n32,
                           TOP_notxr32_n32,
                           TOP_notxr64_off,
                           TOP_incxr8_n32,
                           TOP_incxr16_n32,
                           TOP_incxr32_n32,
                           TOP_incxr64_off,
                           TOP_decxr8_n32,
                           TOP_decxr16_n32,
                           TOP_decxr32_n32,
                           TOP_decxr64_off,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE resbase = ISA_Print_Type_Create("resbase", "%s %s,%s%s(%s)");
  Name();
  Operand(0);
  Segment();
  Operand(2);
  Operand(1);
  Instruction_Print_Group( resbase,
                           TOP_addxr8,
                           TOP_addxr16,
                           TOP_addxr32,
                           TOP_addxr64,
                           TOP_addixr8,
                           TOP_addixr16,
                           TOP_addixr32,
                           TOP_addixr64,
                           TOP_andxr8,
                           TOP_andxr16,
                           TOP_andxr32,
                           TOP_andxr64,
                           TOP_andixr8,
                           TOP_andixr16,
                           TOP_andixr32,
                           TOP_andixr64,
                           TOP_orxr8,
                           TOP_orxr16,
                           TOP_orxr32,
                           TOP_orxr64,
                           TOP_orixr8,
                           TOP_orixr16,
                           TOP_orixr32,
                           TOP_orixr64,
                           TOP_subxr8,
                           TOP_subxr16,
                           TOP_subxr32,
                           TOP_subxr64,
                           TOP_subixr8,
                           TOP_subixr16,
                           TOP_subixr32,
                           TOP_subixr64,
                           TOP_xorxr8,
                           TOP_xorxr16,
                           TOP_xorxr32,
                           TOP_xorxr64,
                           TOP_xorixr8,
                           TOP_xorixr16,
                           TOP_xorixr32,
                           TOP_xorixr64,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE resbaseindex = ISA_Print_Type_Create("resbaseindex", "%s %s,%s%s(%s,%s,%s)");
  Name();
  Operand(0);
  Segment();
  Operand(2);
  Operand(1);
  Operand(3);
  Operand(4);
  Instruction_Print_Group( resbaseindex,
                           TOP_addxxr8,
                           TOP_addxxr16,
                           TOP_addxxr32,
                           TOP_addxxr64,
                           TOP_addixxr8,
                           TOP_addixxr16,
                           TOP_addixxr32,
                           TOP_addixxr64,
                           TOP_andxxr8,
                           TOP_andxxr16,
                           TOP_andxxr32,
                           TOP_andxxr64,
                           TOP_andixxr8,
                           TOP_andixxr16,
                           TOP_andixxr32,
                           TOP_andixxr64,
                           TOP_orxxr8,
                           TOP_orxxr16,
                           TOP_orxxr32,
                           TOP_orxxr64,
                           TOP_orixxr8,
                           TOP_orixxr16,
                           TOP_orixxr32,
                           TOP_orixxr64,
                           TOP_subxxr8,
                           TOP_subxxr16,
                           TOP_subxxr32,
                           TOP_subxxr64,
                           TOP_subixxr8,
                           TOP_subixxr16,
                           TOP_subixxr32,
                           TOP_subixxr64,
                           TOP_xorxxr8,
                           TOP_xorxxr16,
                           TOP_xorxxr32,
                           TOP_xorxxr64,
                           TOP_xorixxr8,
                           TOP_xorixxr16,
                           TOP_xorixxr32,
                           TOP_xorixxr64,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE resindex = ISA_Print_Type_Create("resindex", "%s %s,%s%s(,%s,%s)");
  Name();
  Operand(0);
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( resindex,
                           TOP_addxxxr8,
                           TOP_addxxxr16,
                           TOP_addxxxr32,
                           TOP_addxxxr64,
                           TOP_addixxxr8,
                           TOP_addixxxr16,
                           TOP_addixxxr32,
                           TOP_addixxxr64,
                           TOP_andxxxr8,
                           TOP_andxxxr16,
                           TOP_andxxxr32,
                           TOP_andxxxr64,
                           TOP_andixxxr8,
                           TOP_andixxxr16,
                           TOP_andixxxr32,
                           TOP_andixxxr64,
                           TOP_orxxxr8,
                           TOP_orxxxr16,
                           TOP_orxxxr32,
                           TOP_orxxxr64,
                           TOP_orixxxr8,
                           TOP_orixxxr16,
                           TOP_orixxxr32,
                           TOP_orixxxr64,
                           TOP_subxxxr8,
                           TOP_subxxxr16,
                           TOP_subxxxr32,
                           TOP_subxxxr64,
                           TOP_subixxxr8,
                           TOP_subixxxr16,
                           TOP_subixxxr32,
                           TOP_subixxxr64,
                           TOP_xorxxxr8,
                           TOP_xorxxxr16,
                           TOP_xorxxxr32,
                           TOP_xorxxxr64,
                           TOP_xorixxxr8,
                           TOP_xorixxxr16,
                           TOP_xorixxxr32,
                           TOP_xorixxxr64,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE resmem_n32 = ISA_Print_Type_Create("resmem_n32", "%s %s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group( resmem_n32,
                           TOP_addxr8_n32,
                           TOP_addxr16_n32,
                           TOP_addxr32_n32,
                           TOP_addxr64_off,
                           TOP_addixr8_n32,
                           TOP_addixr16_n32,
                           TOP_addixr32_n32,
                           TOP_addixr64_off,
                           TOP_andxr8_n32,
                           TOP_andxr16_n32,
                           TOP_andxr32_n32,
                           TOP_andxr64_off,
                           TOP_andixr8_n32,
                           TOP_andixr16_n32,
                           TOP_andixr32_n32,
                           TOP_andixr64_off,
                           TOP_orxr8_n32,
                           TOP_orxr16_n32,
                           TOP_orxr32_n32,
                           TOP_orxr64_off,
                           TOP_orixr8_n32,
                           TOP_orixr16_n32,
                           TOP_orixr32_n32,
                           TOP_orixr64_off,
                           TOP_subxr8_n32,
                           TOP_subxr16_n32,
                           TOP_subxr32_n32,
                           TOP_subxr64_off,
                           TOP_subixr8_n32,
                           TOP_subixr16_n32,
                           TOP_subixr32_n32,
                           TOP_subixr64_off,
                           TOP_xorxr8_n32,
                           TOP_xorxr16_n32,
                           TOP_xorxr32_n32,
                           TOP_xorxr64_off,
                           TOP_xorixr8_n32,
                           TOP_xorixr16_n32,
                           TOP_xorixr32_n32,
                           TOP_xorixr64_off,
                           TOP_UNDEFINED );

  /* One result / two operands in x86 style w/ mem operand */
  ISA_PRINT_TYPE ropmem =  ISA_Print_Type_Create("ropmem", "%s %s%s(%s),%s");
  Name();
  Segment();
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
                           /* SSSE3 instructions */
                           TOP_psignx128v8,
                           TOP_psignx128v16,
                           TOP_psignx128v32,
                           TOP_pmulhrswx128,
                           TOP_pmaddubswx128,
                           TOP_phsubx128v16,
                           TOP_phsubx128v32,
                           TOP_phsubsx128v16,
                           TOP_phaddx128v16,
                           TOP_phaddx128v32,
                           TOP_phaddsx128v16,
                           /* SSE4.1 instructions */
                           TOP_minux128v8,
                           TOP_maxux128v8,
                           TOP_minsx128v8,
                           TOP_maxsx128v8,
                           TOP_minux128v16,
                           TOP_maxux128v16,
                           TOP_minsx128v16,
                           TOP_maxsx128v16,
                           TOP_minux128v32,
                           TOP_maxux128v32,
                           TOP_minsx128v32,
                           TOP_maxsx128v32,
                           TOP_muldqx,
                           TOP_mulx128v32,
                           TOP_cmpeqx128v64,
                           TOP_packusdwx,
                           /* SSE4.2 instructions */
                           TOP_cmpgtx128v64,
                           TOP_crc32bx,
                           TOP_crc32wx,
                           TOP_crc32dx,
                           TOP_crc32qx,
                           /* AES instructions */
                           TOP_aesencx,
                           TOP_aesenclastx,
                           TOP_aesdecx,
                           TOP_aesdeclastx,
			   TOP_UNDEFINED );

  /* One result / two operands in x86 style w/ mem operand w/ scaled-index */
  ISA_PRINT_TYPE ropmemindex =  ISA_Print_Type_Create("ropmemindex", 
						      "%s %s%s(%s,%s,%s),%s");
  Name();
  Segment();
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
                           /* SSSE3 instructions */
                           TOP_psignxx128v8,
                           TOP_psignxx128v16,
                           TOP_psignxx128v32,
                           TOP_pmulhrswxx128,
                           TOP_pmaddubswxx128,
                           TOP_phsubxx128v16,
                           TOP_phsubxx128v32,
                           TOP_phsubsxx128v16,
                           TOP_phaddxx128v16,
                           TOP_phaddxx128v32,
                           TOP_phaddsxx128v16,
                           /* SSE4.1 instructions */
                           TOP_minuxx128v8,
                           TOP_maxuxx128v8,
                           TOP_minsxx128v8,
                           TOP_maxsxx128v8,
                           TOP_minuxx128v16,
                           TOP_maxuxx128v16,
                           TOP_minsxx128v16,
                           TOP_maxsxx128v16,
                           TOP_minuxx128v32,
                           TOP_maxuxx128v32,
                           TOP_minsxx128v32,
                           TOP_maxsxx128v32,
                           TOP_muldqxx,
                           TOP_mulxx128v32,
                           TOP_cmpeqxx128v64,
                           TOP_packusdwxx,
                           /* SSE4.2 instructions */
                           TOP_cmpgtxx128v64,
                           TOP_crc32bxx,
                           TOP_crc32wxx,
                           TOP_crc32dxx,
                           TOP_crc32qxx,
                           /* AES instructions */
                           TOP_aesencxx,
                           TOP_aesenclastxx,
                           TOP_aesdecxx,
                           TOP_aesdeclastxx,
			   TOP_UNDEFINED );

  /* One result / two operands in x86 style w/ mem operand w/o base */
  ISA_PRINT_TYPE ropmemindexx =  ISA_Print_Type_Create("ropmemindexx", 
						       "%s %s%s(,%s,%s),%s");
  Name();
  Segment();
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
                           /* SSSE3 instructions */
                           TOP_psignxxx128v8,
                           TOP_psignxxx128v16,
                           TOP_psignxxx128v32,
                           TOP_pmulhrswxxx128,
                           TOP_pmaddubswxxx128,
                           TOP_phsubxxx128v16,
                           TOP_phsubxxx128v32,
                           TOP_phsubsxxx128v16,
                           TOP_phaddxxx128v16,
                           TOP_phaddxxx128v32,
                           TOP_phaddsxxx128v16,
                           /* SSE4.1 instructions */
                           TOP_minuxxx128v8,
                           TOP_maxuxxx128v8,
                           TOP_minsxxx128v8,
                           TOP_maxsxxx128v8,
                           TOP_minuxxx128v16,
                           TOP_maxuxxx128v16,
                           TOP_minsxxx128v16,
                           TOP_maxsxxx128v16,
                           TOP_minuxxx128v32,
                           TOP_maxuxxx128v32,
                           TOP_minsxxx128v32,
                           TOP_maxsxxx128v32,
                           TOP_muldqxxx,
                           TOP_mulxxx128v32,
                           TOP_cmpeqxxx128v64,
                           TOP_packusdwxxx,
                           /* SSE4.2 instructions */
                           TOP_cmpgtxxx128v64,
                           TOP_crc32bxxx,
                           TOP_crc32wxxx,
                           TOP_crc32dxxx,
                           TOP_crc32qxxx,
                           /* AES instructions */
                           TOP_aesencxxx,
                           TOP_aesenclastxxx,
                           TOP_aesdecxxx,
                           TOP_aesdeclastxxx,
			   TOP_UNDEFINED );

  /* SSE4.1 blendv: res reg reg */ 
  ISA_PRINT_TYPE rblendv =  ISA_Print_Type_Create("rblendv", "%s %s,%s");
  Name();
  Operand(2);
  Result(0);
  Operand(0);
  Instruction_Print_Group( rblendv,
                           /* SSE4.1 instructions */
                           TOP_blendv128v8,
                           TOP_fblendv128v64,
                           TOP_fblendv128v32,
			   TOP_UNDEFINED );

  /* SSE4.1 blendv: res mem reg */ 
  ISA_PRINT_TYPE rblendvmem =  ISA_Print_Type_Create("rblendvmem", "%s %s%s(%s),%s");
  Name();
  Operand(3);
  Operand(2);
  Result(0);
  Operand(0);
  Instruction_Print_Group( rblendvmem,
                           /* SSE4.1 instructions */
                           TOP_blendvx128v8,
                           TOP_fblendvx128v64,
                           TOP_fblendvx128v32,
			   TOP_UNDEFINED );

  /* SSE4.1 blendv: res mem w/ scaled index reg */ 
  ISA_PRINT_TYPE rblendvmemindex =  ISA_Print_Type_Create("rblendvmemindex", "%s %s%s(%s,%s,%s),%s");
  Name();
  Operand(5);
  Operand(2);
  Operand(3);
  Operand(4);
  Result(0);
  Operand(0);
  Instruction_Print_Group( rblendvmemindex,
                           /* SSE4.1 instructions */
                           TOP_blendvxx128v8,
                           TOP_fblendvxx128v64,
                           TOP_fblendvxx128v32,
			   TOP_UNDEFINED );

  /* SSE4.1 blendv: res mem w/ scaled index w/o base reg */ 
  ISA_PRINT_TYPE rblendvmemindexx =  ISA_Print_Type_Create("rblendvmemindexx", "%s %s%s(,%s,%s),%s");
  Name();
  Operand(4);
  Operand(2);
  Operand(3);
  Result(0);
  Operand(0);
  Instruction_Print_Group( rblendvmemindexx,
                           /* SSE4.1 instructions */
                           TOP_blendvxxx128v8,
                           TOP_fblendvxxx128v64,
                           TOP_fblendvxxx128v32,
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
                           /* SSSE3 instructions */
                           TOP_palignr128,
                           /* SSE4.1 instructions */
                           TOP_blend128v16,
                           TOP_fblend128v32,
                           TOP_fblend128v64,
                           TOP_fdp128v32,
                           TOP_fdp128v64,
                           TOP_insr128v8,
                           TOP_insr128v16,
                           TOP_insr128v32,
                           TOP_insr128v64,
                           TOP_finsr128v32,
                           TOP_mpsadbw,
                           /* PCLMUL instructions */
                           TOP_pclmulqdq,
			   TOP_UNDEFINED );

  /* One result / x86-style opr, mem, opr */
  ISA_PRINT_TYPE ropmemop =  ISA_Print_Type_Create("ropmemop", "%s %s%s(%s),%s,%s");
  Name();
  Segment();
  Operand(2);
  Operand(1);
  Operand(3);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropmemop,
                           /* SSSE3 instructions */
                           TOP_palignrx128,
                           /* SSE4.1 instructions */
                           TOP_fblendx128v32,
                           TOP_fblendx128v64,
                           TOP_blendx128v16,
                           TOP_mpsadbwx,
                           TOP_fdpx128v32,
                           TOP_fdpx128v64,
                           TOP_insrx128v8,
                           TOP_insrx128v16,
                           TOP_insrx128v32,
                           TOP_insrx128v64,
                           TOP_extrx128v8,
                           TOP_extrx128v16,
                           TOP_extrx128v32,
                           TOP_extrx128v64,
                           TOP_finsrx128v32,
                           TOP_fextrx128v32,
                           /* PCLMUL instructions */
                           TOP_pclmulqdqx,
			   TOP_UNDEFINED );

  /* One result / x86 style opr, mem with scaled-index with base, opr*/
  ISA_PRINT_TYPE ropmemindexop =  ISA_Print_Type_Create("ropmemindexop", "%s %s%s(%s,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(5);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropmemindexop,
                           /* SSSE3 instructions */
                           TOP_palignrxx128,
                           /* SSE4.1 instructions */
                           TOP_blendxx128v16,
                           TOP_fblendxx128v32,
                           TOP_fblendxx128v64,
                           TOP_fdpxx128v32,
                           TOP_fdpxx128v64,
                           TOP_extrxx128v8,
                           TOP_extrxx128v16,
                           TOP_extrxx128v32,
                           TOP_extrxx128v64,
                           TOP_fextrxx128v32,
                           TOP_insrxx128v8,
                           TOP_insrxx128v16,
                           TOP_insrxx128v32,
                           TOP_insrxx128v64,
                           TOP_finsrxx128v32,
                           TOP_mpsadbwxx,
                           /* PCLMUL instructions */
                           TOP_pclmulqdqxx,
			   TOP_UNDEFINED );

  /* One result / x86 style opr, mem with scaled-index w/o base, opr*/
  ISA_PRINT_TYPE ropmemindexxop =  ISA_Print_Type_Create("ropmemindexxop", "%s %s%s(,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(4);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ropmemindexxop,
                           /* SSSE3 instructions */
                           TOP_palignrxxx128,
                           /* SSE4.1 instructions */
                           TOP_blendxxx128v16,
                           TOP_fblendxxx128v32,
                           TOP_fblendxxx128v64,
                           TOP_fdpxxx128v32,
                           TOP_fdpxxx128v64,
                           TOP_insrxxx128v8,
                           TOP_insrxxx128v16,
                           TOP_insrxxx128v32,
                           TOP_insrxxx128v64,
                           TOP_finsrxxx128v32,
                           TOP_extrxxx128v8,
                           TOP_extrxxx128v16,
                           TOP_extrxxx128v32,
                           TOP_extrxxx128v64,
                           TOP_fextrxxx128v32,
                           TOP_mpsadbwxxx,
                           /* PCLMUL instructions */
                           TOP_pclmulqdqxxx,
			   TOP_UNDEFINED );

  /* dest=op(src1, src2), non-x86-style */
  ISA_PRINT_TYPE vropop = ISA_Print_Type_Create("vropop", "%s %s,%s,%s");
  Name();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropop,
                           /* SSE 4.1 instruction */
                           TOP_extr128v8,
                           TOP_extr128v16,
                           TOP_extr128v32,
                           TOP_extr128v64,
                           TOP_fextr128v32,
                           /* AES instruction */
                           TOP_aeskeygenassist,
                           /* XOP instruction */
                           TOP_vprotb,
                           TOP_vprotd,
                           TOP_vprotq,
                           TOP_vprotw,
                           TOP_vprotbi,
                           TOP_vprotdi,
                           TOP_vprotqi,
                           TOP_vprotwi,
                           TOP_vpshab,
                           TOP_vpshad,
                           TOP_vpshaq,
                           TOP_vpshaw,
                           TOP_vpshlb,
                           TOP_vpshld,
                           TOP_vpshlq,
                           TOP_vpshlw,
                           /* AVX instruction */
                           TOP_vadd128v8,
                           TOP_vadd128v32,
                           TOP_vadd128v64,
                           TOP_vadd128v16,
                           TOP_vadds128v8,
                           TOP_vadds128v16,
                           TOP_vaddus128v8,
                           TOP_vaddus128v16,
                           TOP_vfadd128v64,
                           TOP_vfadd128v32,
                           TOP_vfaddsd,
                           TOP_vfaddss,
                           TOP_vfsqrtsd,
                           TOP_vfsqrtss,
                           TOP_vfrsqrtss,
                           TOP_vfrcpss,
                           TOP_vfaddsub128v64,
                           TOP_vfaddsub128v32,
                           TOP_vand128v8,
                           TOP_vand128v16,
                           TOP_vand128v32,
                           TOP_vand128v64,
                           TOP_vfand128v64,
                           TOP_vfand128v32,
                           TOP_vandpd,
                           TOP_vandps,
                           TOP_vandn128v8,
                           TOP_vandn128v16,
                           TOP_vandn128v32,
                           TOP_vandn128v64,
                           TOP_vfandn128v64,
                           TOP_vfandn128v32,
                           TOP_vandnpd,
                           TOP_vandnps,
                           TOP_vpavgb,
                           TOP_vpavgw,
                           TOP_vcmpeq128v8,
                           TOP_vcmpeq128v32,
                           TOP_vcmpeq128v64,
                           TOP_vcmpeq128v16,
                           TOP_vcmpgt128v8,
                           TOP_vcmpgt128v32,
                           TOP_vcmpgt128v64,
                           TOP_vcmpgt128v16,
                           TOP_vcvtsi2sd,
                           TOP_vcvtsi2sdq,
                           TOP_vcvtsi2ss,
                           TOP_vcvtsi2ssq,
                           TOP_vcvtsd2ss,
                           TOP_vcvtss2sd,
                           TOP_vfdiv128v64,
                           TOP_vfdiv128v32,
                           TOP_vdivsd,
                           TOP_vdivss,
                           TOP_vextr128v8,
                           TOP_vextr128v32,
                           TOP_vextr128v64,
                           TOP_vextr128v16,
                           TOP_vfextr128v32,
                           TOP_vfextrf128,
                           TOP_vfhadd128v64,
                           TOP_vfhadd128v32,
                           TOP_vphadd128v32,
                           TOP_vphadd128v16,
                           TOP_vphadds128v16,
                           TOP_vfhsub128v64,
                           TOP_vfhsub128v32,
                           TOP_vphsub128v32,
                           TOP_vphsub128v16,
                           TOP_vphsubs128v16,
                           TOP_vpmaddwd,
                           TOP_vpmaddubsw128,
                           TOP_vmovhlps,
                           TOP_vmovlhps,
                           TOP_vmovsd,
                           TOP_vmovss,
                           TOP_vmaxs128v8,
                           TOP_vmaxs128v32,
                           TOP_vmaxs128v16,
                           TOP_vmaxu128v8,
                           TOP_vmaxu128v32,
                           TOP_vmaxu128v16,
                           TOP_vmins128v8,
                           TOP_vmins128v32,
                           TOP_vmins128v16,
                           TOP_vminu128v8,
                           TOP_vminu128v32,
                           TOP_vminu128v16,
                           TOP_vfmax128v64,
                           TOP_vfmax128v32,
                           TOP_vfmin128v64,
                           TOP_vfmin128v32,
                           TOP_vfmaxsd,
                           TOP_vfmaxss,
                           TOP_vfminsd,
                           TOP_vfminss,
                           TOP_vfmul128v64,
                           TOP_vfmul128v32,
                           TOP_vmulsd,
                           TOP_vmulss,
                           TOP_vmulhuw,
                           TOP_vmulhrsw,
                           TOP_vmulhw,
                           TOP_vmulld,
                           TOP_vmul128v16,
                           TOP_vmul128v32,
                           TOP_vmuludq,
                           TOP_vmuldq,
                           TOP_vor128v8,
                           TOP_vor128v16,
                           TOP_vor128v32,
                           TOP_vor128v64,
                           TOP_vfor128v64,
                           TOP_vfor128v32,
                           TOP_vorpd,
                           TOP_vorps,
                           TOP_vpackssdw,
                           TOP_vpacksswb,
                           TOP_vpackusdw,
                           TOP_vpackuswb,
                           TOP_vfperm128v64,
                           TOP_vfperm128v32,
                           TOP_vfpermi128v64,
                           TOP_vfpermi128v32,
                           TOP_vpsadbw,
                           TOP_vpshuf128v8,
                           TOP_vpshuf128v32,
                           TOP_vpshufw64v16,
                           TOP_vpshufhw,
                           TOP_vpshuflw,
                           TOP_vpslld,
                           TOP_vpsllq,
                           TOP_vpsllw,
                           TOP_vpsrad,
                           TOP_vpsraw,
                           TOP_vpsrld,
                           TOP_vpsrlq,
                           TOP_vpsrlw,
                           TOP_vpslldq,
                           TOP_vpsrldq,
                           TOP_vpslldi,
                           TOP_vpsllqi,
                           TOP_vpsllwi,
                           TOP_vpsradi,
                           TOP_vpsrawi,
                           TOP_vpsrldi,
                           TOP_vpsrlqi,
                           TOP_vpsrlwi,
                           TOP_vsub128v8,
                           TOP_vsub128v32,
                           TOP_vsub128v64,
                           TOP_vsub128v16,
                           TOP_vsubs128v8,
                           TOP_vsubs128v16,
                           TOP_vsubus128v8,
                           TOP_vsubus128v16,
                           TOP_vfsub128v64,
                           TOP_vfsub128v32,
                           TOP_vsubsd,
                           TOP_vsubss,
                           TOP_vpunpckh64v8,
                           TOP_vpunpckh64v32,
                           TOP_vpunpckh64v16,
                           TOP_vpunpckh64v64,
                           TOP_vpunpckl64v8,
                           TOP_vpunpckl64v32,
                           TOP_vpunpckl64v16,
                           TOP_vpunpckl64v64,
                           TOP_vunpckh128v64,
                           TOP_vunpckh128v32,
                           TOP_vunpckl128v64,
                           TOP_vunpckl128v32,
                           TOP_vxor128v8,
                           TOP_vxor128v16,
                           TOP_vxor128v32,
                           TOP_vxor128v64,
                           TOP_vfxor128v64,
                           TOP_vfxor128v32,
                           TOP_vxorpd,
                           TOP_vxorps,
                           TOP_vaesenc,
                           TOP_vaesenclast,
                           TOP_vaesdec,
                           TOP_vaesdeclast,
                           TOP_vaeskeygenassist,
                           TOP_vcmpeqpd,
                           TOP_vcmpltpd,
                           TOP_vcmplepd,
                           TOP_vcmpunordpd,
                           TOP_vcmpneqpd,
                           TOP_vcmpnltpd,
                           TOP_vcmpnlepd,
                           TOP_vcmpordpd,
                           TOP_vcmpeqps,
                           TOP_vcmpltps,
                           TOP_vcmpleps,
                           TOP_vcmpunordps,
                           TOP_vcmpneqps,
                           TOP_vcmpnltps,
                           TOP_vcmpnleps,
                           TOP_vcmpordps,
                           TOP_vcmpeqss,
                           TOP_vcmpltss,
                           TOP_vcmpless,
                           TOP_vcmpunordss,
                           TOP_vcmpneqss,
                           TOP_vcmpnltss,
                           TOP_vcmpnless,
                           TOP_vcmpordss,
                           TOP_vcmpeqsd,
                           TOP_vcmpltsd,
                           TOP_vcmplesd,
                           TOP_vcmpunordsd,
                           TOP_vcmpneqsd,
                           TOP_vcmpnltsd,
                           TOP_vcmpnlesd,
                           TOP_vcmpordsd,
                           TOP_UNDEFINED);

  /* dest=op(src1, memop), non-x86-style */
  ISA_PRINT_TYPE vropmem = ISA_Print_Type_Create("vropmem", "%s %s%s(%s),%s,%s");
  Name();
  Segment();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropmem,
                           /* AES instruction */
                           TOP_aeskeygenassistx,
                           /* XOP instruction */
                           TOP_vprotbxr,
                           TOP_vprotdxr,
                           TOP_vprotqxr,
                           TOP_vprotwxr,
                           TOP_vpshabxr,
                           TOP_vpshadxr,
                           TOP_vpshaqxr,
                           TOP_vpshawxr,
                           TOP_vpshlbxr,
                           TOP_vpshldxr,
                           TOP_vpshlqxr,
                           TOP_vpshlwxr,
                           /* AVX instruction */
                           TOP_vaddx128v8,
                           TOP_vaddx128v32,
                           TOP_vaddx128v64,
                           TOP_vaddx128v16,
                           TOP_vaddsx128v8,
                           TOP_vaddsx128v16,
                           TOP_vaddusx128v8,
                           TOP_vaddusx128v16,
                           TOP_vfaddx128v64,
                           TOP_vfaddx128v32,
                           TOP_vfaddxsd,
                           TOP_vfaddxss,
                           TOP_vfsqrtxsd,
                           TOP_vfsqrtxss,
                           TOP_vfrsqrtxss,
                           TOP_vfrcpxss,
                           TOP_vfaddsubx128v64,
                           TOP_vfaddsubx128v32,
                           TOP_vandx128v8,
                           TOP_vandx128v16,
                           TOP_vandx128v32,
                           TOP_vandx128v64,
                           TOP_vfandx128v64,
                           TOP_vfandx128v32,
                           TOP_vandnx128v8,
                           TOP_vandnx128v16,
                           TOP_vandnx128v32,
                           TOP_vandnx128v64,
                           TOP_vfandnx128v64,
                           TOP_vfandnx128v32,
                           TOP_vpavgbx,
                           TOP_vpavgwx,
                           TOP_vcmpeqx128v8,
                           TOP_vcmpeqx128v32,
                           TOP_vcmpeqx128v64,
                           TOP_vcmpeqx128v16,
                           TOP_vcmpgtx128v8,
                           TOP_vcmpgtx128v32,
                           TOP_vcmpgtx128v64,
                           TOP_vcmpgtx128v16,
                           TOP_vcvtsd2ssx,
                           TOP_vcvtsi2sdx,
                           TOP_vcvtsi2sdqx,
                           TOP_vcvtsi2ssx,
                           TOP_vcvtsi2ssqx,
                           TOP_vcvtss2sdx,
                           TOP_vfdivx128v64,
                           TOP_vfdivx128v32,
                           TOP_vdivxsd,
                           TOP_vdivxss,
                           TOP_vextrx128v8,
                           TOP_vextrx128v32,
                           TOP_vextrx128v64,
                           TOP_vextrx128v16,
                           TOP_vfextrx128v32,
                           TOP_vfextrxf128,
                           TOP_vfhaddx128v64,
                           TOP_vfhaddx128v32,
                           TOP_vphaddx128v32,
                           TOP_vphaddx128v16,
                           TOP_vphaddsx128v16,
                           TOP_vfhsubx128v64,
                           TOP_vfhsubx128v32,
                           TOP_vphsubx128v32,
                           TOP_vphsubx128v16,
                           TOP_vphsubsx128v16,
                           TOP_vpmaddwdx,
                           TOP_vpmaddubswx128,
                           TOP_vmaxsx128v8,
                           TOP_vmaxsx128v32,
                           TOP_vmaxsx128v16,
                           TOP_vmaxux128v8,
                           TOP_vmaxux128v32,
                           TOP_vmaxux128v16,
                           TOP_vminsx128v8,
                           TOP_vminsx128v32,
                           TOP_vminsx128v16,
                           TOP_vminux128v8,
                           TOP_vminux128v32,
                           TOP_vminux128v16,
                           TOP_vfmaxx128v64,
                           TOP_vfmaxx128v32,
                           TOP_vfminx128v64,
                           TOP_vfminx128v32,
                           TOP_vfmaxxsd,
                           TOP_vfmaxxss,
                           TOP_vfminxsd,
                           TOP_vfminxss,
                           TOP_vfmulx128v64,
                           TOP_vfmulx128v32,
                           TOP_vmulxsd,
                           TOP_vmulxss,
                           TOP_vmulhuwx,
                           TOP_vmulhrswx,
                           TOP_vmulhwx,
                           TOP_vmulldx,
                           TOP_vmulx128v16,
                           TOP_vmulx128v32,
                           TOP_vmuludqx,
                           TOP_vmuldqx,
                           TOP_vpackssdwx,
                           TOP_vpacksswbx,
                           TOP_vpackusdwx,
                           TOP_vpackuswbx,
                           TOP_vfpermx128v64,
                           TOP_vfpermx128v32,
                           TOP_vfpermix128v64,
                           TOP_vfpermix128v32,
                           TOP_vorx128v8,
                           TOP_vorx128v16,
                           TOP_vorx128v32,
                           TOP_vorx128v64,
                           TOP_vforx128v64,
                           TOP_vforx128v32,
                           TOP_vpsadbwx,
                           TOP_vpshufx128v8,
                           TOP_vpshufx128v32,
                           TOP_vpshufwx64v16,
                           TOP_vpshufhwx,
                           TOP_vpshuflwx,
                           TOP_vpslldx,
                           TOP_vpsllqx,
                           TOP_vpsllwx,
                           TOP_vpsradx,
                           TOP_vpsrawx,
                           TOP_vpsrldx,
                           TOP_vpsrlqx,
                           TOP_vpsrlwx,
                           TOP_vsubx128v8,
                           TOP_vsubx128v32,
                           TOP_vsubx128v64,
                           TOP_vsubx128v16,
                           TOP_vsubsx128v8,
                           TOP_vsubsx128v16,
                           TOP_vsubusx128v8,
                           TOP_vsubusx128v16,
                           TOP_vfsubx128v64,
                           TOP_vfsubx128v32,
                           TOP_vsubxsd,
                           TOP_vsubxss,
                           TOP_vpunpckhx64v8,
                           TOP_vpunpckhx64v32,
                           TOP_vpunpckhx64v16,
                           TOP_vpunpckhx64v64,
                           TOP_vpunpcklx64v8,
                           TOP_vpunpcklx64v32,
                           TOP_vpunpcklx64v16,
                           TOP_vpunpcklx64v64,
                           TOP_vunpckhx128v64,
                           TOP_vunpckhx128v32,
                           TOP_vunpcklx128v64,
                           TOP_vunpcklx128v32,
                           TOP_vxorx128v8,
                           TOP_vxorx128v16,
                           TOP_vxorx128v32,
                           TOP_vxorx128v64,
                           TOP_vfxorx128v64,
                           TOP_vfxorx128v32,
                           TOP_vaesencx,
                           TOP_vaesenclastx,
                           TOP_vaesdecx,
                           TOP_vaesdeclastx,
                           TOP_vaeskeygenassistx,
                           TOP_vldhpd,
                           TOP_vldlpd,
                           TOP_vldhps,
                           TOP_vldlps,
                           TOP_UNDEFINED);

  /* dest=op(memop, reg), non-x86-style */
  ISA_PRINT_TYPE vrmemop = ISA_Print_Type_Create("vrmemop", "%s %s,%s%s(%s),%s");
  Name();
  Operand(2);
  Segment();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vrmemop,
                           /* SSE 4.1 instruction */
                           TOP_extrx128v8,
                           TOP_extrx128v16,
                           TOP_extrx128v32,
                           TOP_extrx128v64,
                           TOP_fextrx128v32,
                           /* XOP instruction */
                           TOP_vprotbx,
                           TOP_vprotdx,
                           TOP_vprotqx,
                           TOP_vprotwx,
                           TOP_vprotbix,
                           TOP_vprotdix,
                           TOP_vprotqix,
                           TOP_vprotwix,
                           TOP_vpshabx,
                           TOP_vpshadx,
                           TOP_vpshaqx,
                           TOP_vpshawx,
                           TOP_vpshlbx,
                           TOP_vpshldx,
                           TOP_vpshlqx,
                           TOP_vpshlwx,
                           TOP_UNDEFINED);

  /* dest=op(src1, memop with scaled index with base), non-x86-style */
  ISA_PRINT_TYPE vropmemindex = ISA_Print_Type_Create("vropmemindex", "%s %s%s(%s,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropmemindex,
                           /* AES instruction */
                           TOP_aeskeygenassistxx,
                           /* XOP instruction */
                           TOP_vprotbxxr,
                           TOP_vprotdxxr,
                           TOP_vprotqxxr,
                           TOP_vprotwxxr,
                           TOP_vpshabxxr,
                           TOP_vpshadxxr,
                           TOP_vpshaqxxr,
                           TOP_vpshawxxr,
                           TOP_vpshlbxxr,
                           TOP_vpshldxxr,
                           TOP_vpshlqxxr,
                           TOP_vpshlwxxr,
                           /* AVX instruction */
                           TOP_vaddxx128v8,
                           TOP_vaddxx128v32,
                           TOP_vaddxx128v64,
                           TOP_vaddxx128v16,
                           TOP_vaddsxx128v8,
                           TOP_vaddsxx128v16,
                           TOP_vaddusxx128v8,
                           TOP_vaddusxx128v16,
                           TOP_vfaddxx128v64,
                           TOP_vfaddxx128v32,
                           TOP_vfaddxxsd,
                           TOP_vfaddxxss,
                           TOP_vfsqrtxxss,
                           TOP_vfsqrtxxsd,
                           TOP_vfrsqrtxxss,
                           TOP_vfrcpxxss,
                           TOP_vfaddsubxx128v64,
                           TOP_vfaddsubxx128v32,
                           TOP_vfandxx128v32,
                           TOP_vfandnxx128v64,
                           TOP_vfandnxx128v32,
                           TOP_vandxx128v8,
                           TOP_vandxx128v16,
                           TOP_vandxx128v32,
                           TOP_vandxx128v64,
                           TOP_vfandxx128v64,
                           TOP_vandnxx128v8,
                           TOP_vandnxx128v16,
                           TOP_vandnxx128v32,
                           TOP_vandnxx128v64,
                           TOP_vpavgbxx,
                           TOP_vpavgwxx,
                           TOP_vcmpeqxx128v8,
                           TOP_vcmpeqxx128v32,
                           TOP_vcmpeqxx128v64,
                           TOP_vcmpeqxx128v16,
                           TOP_vcmpgtxx128v8,
                           TOP_vcmpgtxx128v32,
                           TOP_vcmpgtxx128v64,
                           TOP_vcmpgtxx128v16,
                           TOP_vcvtsd2ssxx,
                           TOP_vcvtsi2sdxx,
                           TOP_vcvtsi2sdqxx,
                           TOP_vcvtsi2ssxx,
                           TOP_vcvtsi2ssqxx,
                           TOP_vcvtss2sdxx,
                           TOP_vfdivxx128v64,
                           TOP_vfdivxx128v32,
                           TOP_vdivxxsd,
                           TOP_vdivxxss,
                           TOP_vextrxx128v8,
                           TOP_vextrxx128v32,
                           TOP_vextrxx128v64,
                           TOP_vextrxx128v16,
                           TOP_vfextrxx128v32,
                           TOP_vfextrxxf128,
                           TOP_vfhaddxx128v64,
                           TOP_vfhaddxx128v32,
                           TOP_vphaddxx128v16,
                           TOP_vphaddxx128v32,
                           TOP_vphaddsxx128v16,
                           TOP_vfhsubxx128v64,
                           TOP_vfhsubxx128v32,
                           TOP_vphsubxx128v32,
                           TOP_vphsubsxx128v16,
                           TOP_vphsubxx128v16,
                           TOP_vpmaddwdxx,
                           TOP_vpmaddubswxx128,
                           TOP_vmaxsxx128v8,
                           TOP_vmaxsxx128v32,
                           TOP_vmaxsxx128v16,
                           TOP_vmaxuxx128v8,
                           TOP_vmaxuxx128v32,
                           TOP_vmaxuxx128v16,
                           TOP_vminsxx128v8,
                           TOP_vminsxx128v32,
                           TOP_vminsxx128v16,
                           TOP_vminuxx128v8,
                           TOP_vminuxx128v32,
                           TOP_vminuxx128v16,
                           TOP_vfmaxxx128v64,
                           TOP_vfmaxxx128v32,
                           TOP_vfminxx128v64,
                           TOP_vfminxx128v32,
                           TOP_vfmaxxxsd,
                           TOP_vfmaxxxss,
                           TOP_vfminxxsd,
                           TOP_vfminxxss,
                           TOP_vfmulxx128v64,
                           TOP_vfmulxx128v32,
                           TOP_vmulxxsd,
                           TOP_vmulxxss,
                           TOP_vmulhuwxx,
                           TOP_vmulhrswxx,
                           TOP_vmulhwxx,
                           TOP_vmulldxx,
                           TOP_vmulxx128v16,
                           TOP_vmulxx128v32,
                           TOP_vmuludqxx,
                           TOP_vmuldqxx,
                           TOP_vorxx128v8,
                           TOP_vorxx128v16,
                           TOP_vorxx128v32,
                           TOP_vorxx128v64,
                           TOP_vforxx128v64,
                           TOP_vforxx128v32,
                           TOP_vpackssdwxx,
                           TOP_vpacksswbxx,
                           TOP_vpackusdwxx,
                           TOP_vpackuswbxx,
                           TOP_vfpermxx128v64,
                           TOP_vfpermxx128v32,
                           TOP_vfpermixx128v64,
                           TOP_vfpermixx128v32,
                           TOP_vpsadbwxx,
                           TOP_vpshufxx128v8,
                           TOP_vpshufxx128v32,
                           TOP_vpshufwxx64v16,
                           TOP_vpshufhwxx,
                           TOP_vpshuflwxx,
                           TOP_vpslldxx,
                           TOP_vpsllqxx,
                           TOP_vpsllwxx,
                           TOP_vpsradxx,
                           TOP_vpsrawxx,
                           TOP_vpsrldxx,
                           TOP_vpsrlqxx,
                           TOP_vpsrlwxx,
                           TOP_vsubxx128v8,
                           TOP_vsubxx128v32,
                           TOP_vsubxx128v64,
                           TOP_vsubxx128v16,
                           TOP_vsubsxx128v8,
                           TOP_vsubsxx128v16,
                           TOP_vsubusxx128v8,
                           TOP_vsubusxx128v16,
                           TOP_vfsubxx128v64,
                           TOP_vfsubxx128v32,
                           TOP_vsubxxsd,
                           TOP_vsubxxss,
                           TOP_vpunpckhxx64v8,
                           TOP_vpunpckhxx64v32,
                           TOP_vpunpckhxx64v16,
                           TOP_vpunpckhxx64v64,
                           TOP_vpunpcklxx64v8,
                           TOP_vpunpcklxx64v32,
                           TOP_vpunpcklxx64v16,
                           TOP_vpunpcklxx64v64,
                           TOP_vunpckhxx128v64,
                           TOP_vunpckhxx128v32,
                           TOP_vunpcklxx128v64,
                           TOP_vunpcklxx128v32,
                           TOP_vxorxx128v8,
                           TOP_vxorxx128v16,
                           TOP_vxorxx128v32,
                           TOP_vxorxx128v64,
                           TOP_vfxorxx128v64,
                           TOP_vfxorxx128v32,
                           TOP_vaesencxx,
                           TOP_vaesenclastxx,
                           TOP_vaesdecxx,
                           TOP_vaesdeclastxx,
                           TOP_vaeskeygenassistxx,
                           TOP_vldhpdx,
                           TOP_vldhpsx,
                           TOP_vldlpdx,
                           TOP_vldlpsx,
                           TOP_UNDEFINED);

  /* dest=op(memop with scaled index with base, reg), non-x86-style */
  ISA_PRINT_TYPE vrmemindexop = ISA_Print_Type_Create("vrmemindexop", "%s %s,%s%s(%s,%s,%s),%s");
  Name();
  Operand(4);
  Segment();
  Operand(3);
  Operand(0);
  Operand(1);
  Operand(2);
  Result(0);
  Instruction_Print_Group( vrmemindexop,
                           /* SSE 4.1 instruction */
                           TOP_extrxx128v8,
                           TOP_extrxx128v16,
                           TOP_extrxx128v32,
                           TOP_extrxx128v64,
                           TOP_fextrxx128v32,
                           /* XOP instruction */
                           TOP_vprotbxx,
                           TOP_vprotdxx,
                           TOP_vprotqxx,
                           TOP_vprotwxx,
                           TOP_vprotbixx,
                           TOP_vprotdixx,
                           TOP_vprotqixx,
                           TOP_vprotwixx,
                           TOP_vpshabxx,
                           TOP_vpshadxx,
                           TOP_vpshaqxx,
                           TOP_vpshawxx,
                           TOP_vpshlbxx,
                           TOP_vpshldxx,
                           TOP_vpshlqxx,
                           TOP_vpshlwxx,
                           TOP_UNDEFINED);

  /* dest=op(src1, memop with scaled index without base), non-x86-style */
  ISA_PRINT_TYPE vropmemindexx = ISA_Print_Type_Create("vropmemindexx", "%s %s%s(,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropmemindex,
                           /* AES instruction */
                           TOP_aeskeygenassistxxx,
                           /* XOP instruction */
                           TOP_vprotbxxxr,
                           TOP_vprotdxxxr,
                           TOP_vprotqxxxr,
                           TOP_vprotwxxxr,
                           TOP_vpshabxxxr,
                           TOP_vpshadxxxr,
                           TOP_vpshaqxxxr,
                           TOP_vpshawxxxr,
                           TOP_vpshlbxxxr,
                           TOP_vpshldxxxr,
                           TOP_vpshlqxxxr,
                           TOP_vpshlwxxxr,
                           /* AVX instruction */
                           TOP_vaddxxx128v8,
                           TOP_vaddxxx128v32,
                           TOP_vaddxxx128v64,
                           TOP_vaddxxx128v16,
                           TOP_vaddsxxx128v8,
                           TOP_vaddsxxx128v16,
                           TOP_vaddusxxx128v8,
                           TOP_vaddusxxx128v16,
                           TOP_vfaddxxx128v64,
                           TOP_vfaddxxx128v32,
                           TOP_vfaddxxxsd,
                           TOP_vfaddxxxss,
                           TOP_vfsqrtxxxsd,
                           TOP_vfsqrtxxxss,
                           TOP_vfrsqrtxxxss,
                           TOP_vfrcpxxxss,
                           TOP_vfaddsubxxx128v64,
                           TOP_vfaddsubxxx128v32,
                           TOP_vfandxxx128v32,
                           TOP_vfandnxxx128v64,
                           TOP_vfandnxxx128v32,
                           TOP_vandxxx128v8,
                           TOP_vandxxx128v16,
                           TOP_vandxxx128v32,
                           TOP_vandxxx128v64,
                           TOP_vfandxxx128v64,
                           TOP_vandnxxx128v8,
                           TOP_vandnxxx128v16,
                           TOP_vandnxxx128v32,
                           TOP_vandnxxx128v64,
                           TOP_vpavgbxxx,
                           TOP_vpavgwxxx,
                           TOP_vcmpeqxxx128v8,
                           TOP_vcmpeqxxx128v32,
                           TOP_vcmpeqxxx128v64,
                           TOP_vcmpeqxxx128v16,
                           TOP_vcmpgtxxx128v8,
                           TOP_vcmpgtxxx128v32,
                           TOP_vcmpgtxxx128v64,
                           TOP_vcmpgtxxx128v16,
                           TOP_vcvtsd2ssxxx,
                           TOP_vcvtsi2sdxxx,
                           TOP_vcvtsi2sdqxxx,
                           TOP_vcvtsi2ssxxx,
                           TOP_vcvtsi2ssqxxx,
                           TOP_vcvtss2sdxxx,
                           TOP_vfdivxxx128v64,
                           TOP_vfdivxxx128v32,
                           TOP_vdivxxxsd,
                           TOP_vdivxxxss,
                           TOP_vextrxxx128v8,
                           TOP_vextrxxx128v32,
                           TOP_vextrxxx128v64,
                           TOP_vextrxxx128v16,
                           TOP_vfextrxxx128v32,
                           TOP_vfextrxxxf128,
                           TOP_vfhaddxxx128v64,
                           TOP_vfhaddxxx128v32,
                           TOP_vphaddxxx128v16,
                           TOP_vphaddxxx128v32,
                           TOP_vphaddsxxx128v16,
                           TOP_vfhsubxxx128v64,
                           TOP_vfhsubxxx128v32,
                           TOP_vphsubxxx128v32,
                           TOP_vphsubsxxx128v16,
                           TOP_vphsubxxx128v16,
                           TOP_vpmaddwdxxx,
                           TOP_vpmaddubswxxx128,
                           TOP_vmaxsxxx128v8,
                           TOP_vmaxsxxx128v32,
                           TOP_vmaxsxxx128v16,
                           TOP_vmaxuxxx128v8,
                           TOP_vmaxuxxx128v32,
                           TOP_vmaxuxxx128v16,
                           TOP_vminsxxx128v8,
                           TOP_vminsxxx128v32,
                           TOP_vminsxxx128v16,
                           TOP_vminuxxx128v8,
                           TOP_vminuxxx128v32,
                           TOP_vminuxxx128v16,
                           TOP_vfmaxxxx128v64,
                           TOP_vfmaxxxx128v32,
                           TOP_vfminxxx128v64,
                           TOP_vfminxxx128v32,
                           TOP_vfmaxxxxsd,
                           TOP_vfmaxxxxss,
                           TOP_vfminxxxsd,
                           TOP_vfminxxxss,
                           TOP_vfmulxxx128v64,
                           TOP_vfmulxxx128v32,
                           TOP_vmulxxxsd,
                           TOP_vmulxxxss,
                           TOP_vmulhuwxxx,
                           TOP_vmulhrswxxx,
                           TOP_vmulhwxxx,
                           TOP_vmulldxxx,
                           TOP_vmulxxx128v16,
                           TOP_vmulxxx128v32,
                           TOP_vmuludqxxx,
                           TOP_vmuldqxxx,
                           TOP_vorxxx128v8,
                           TOP_vorxxx128v16,
                           TOP_vorxxx128v32,
                           TOP_vorxxx128v64,
                           TOP_vforxxx128v64,
                           TOP_vforxxx128v32,
                           TOP_vpackssdwxxx,
                           TOP_vpacksswbxxx,
                           TOP_vpackusdwxxx,
                           TOP_vpackuswbxxx,
                           TOP_vfpermxxx128v64,
                           TOP_vfpermxxx128v32,
                           TOP_vfpermixxx128v64,
                           TOP_vfpermixxx128v32,
                           TOP_vpsadbwxxx,
                           TOP_vpshufxxx128v8,
                           TOP_vpshufxxx128v32,
                           TOP_vpshufwxxx64v16,
                           TOP_vpshufhwxxx,
                           TOP_vpshuflwxxx,
                           TOP_vpslldxxx,
                           TOP_vpsllqxxx,
                           TOP_vpsllwxxx,
                           TOP_vpsradxxx,
                           TOP_vpsrawxxx,
                           TOP_vpsrldxxx,
                           TOP_vpsrlqxxx,
                           TOP_vpsrlwxxx,
                           TOP_vsubxxx128v8,
                           TOP_vsubxxx128v32,
                           TOP_vsubxxx128v64,
                           TOP_vsubxxx128v16,
                           TOP_vsubsxxx128v8,
                           TOP_vsubsxxx128v16,
                           TOP_vsubusxxx128v8,
                           TOP_vsubusxxx128v16,
                           TOP_vfsubxxx128v64,
                           TOP_vfsubxxx128v32,
                           TOP_vsubxxxsd,
                           TOP_vsubxxxss,
                           TOP_vpunpckhxxx64v8,
                           TOP_vpunpckhxxx64v32,
                           TOP_vpunpckhxxx64v16,
                           TOP_vpunpckhxxx64v64,
                           TOP_vpunpcklxxx64v8,
                           TOP_vpunpcklxxx64v32,
                           TOP_vpunpcklxxx64v16,
                           TOP_vpunpcklxxx64v64,
                           TOP_vunpckhxxx128v64,
                           TOP_vunpckhxxx128v32,
                           TOP_vunpcklxxx128v64,
                           TOP_vunpcklxxx128v32,
                           TOP_vxorxxx128v8,
                           TOP_vxorxxx128v16,
                           TOP_vxorxxx128v32,
                           TOP_vxorxxx128v64,
                           TOP_vfxorxxx128v64,
                           TOP_vfxorxxx128v32,
                           TOP_vaesencxxx,
                           TOP_vaesenclastxxx,
                           TOP_vaesdecxxx,
                           TOP_vaesdeclastxxx,
                           TOP_vaeskeygenassistxxx,
                           TOP_vldhpdxx,
                           TOP_vldhpsxx,
                           TOP_vldlpdxx,
                           TOP_vldlpsxx,
                           TOP_UNDEFINED);

  /* dest=op(memop with scaled index without base, reg), non-x86-style */
  ISA_PRINT_TYPE vrmemindexxop = ISA_Print_Type_Create("vrmemindexxop", "%s %s,%s%s(,%s,%s),%s");
  Name();
  Operand(3);
  Segment();
  Operand(2);
  Operand(0);
  Operand(1);
  Result(0);
  Instruction_Print_Group( vropmemindex,
                           /* SSE 4.1 instruction */
                           TOP_extrxxx128v8,
                           TOP_extrxxx128v16,
                           TOP_extrxxx128v32,
                           TOP_extrxxx128v64,
                           TOP_fextrxxx128v32,
                           /* XOP instruction */
                           TOP_vprotbxxx,
                           TOP_vprotdxxx,
                           TOP_vprotqxxx,
                           TOP_vprotwxxx,
                           TOP_vprotbixxx,
                           TOP_vprotdixxx,
                           TOP_vprotqixxx,
                           TOP_vprotwixxx,
                           TOP_vpshabxxx,
                           TOP_vpshadxxx,
                           TOP_vpshaqxxx,
                           TOP_vpshawxxx,
                           TOP_vpshlbxxx,
                           TOP_vpshldxxx,
                           TOP_vpshlqxxx,
                           TOP_vpshlwxxx,
                           TOP_UNDEFINED);

  /* dest=op(src1, src2, src3), non-x86-style */
  ISA_PRINT_TYPE vropopimm = ISA_Print_Type_Create("vropopimm", "%s %s,%s,%s,%s");
  Name();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropopimm,
                           /* XOP instructions */
                           TOP_vpperm,
                           TOP_vpcmov,
                           TOP_vpmacsdd,
                           TOP_vpmacsdqh,
                           TOP_vpmacsdql,
                           TOP_vpmacssdd,
                           TOP_vpmacssdqh,
                           TOP_vpmacssdql,
                           TOP_vpmacsswd,
                           TOP_vpmacssww,
                           TOP_vpmacswd,
                           TOP_vpmacsww,
                           TOP_vpmadcsswd,
                           TOP_vpmadcswd,
                           TOP_vpcomb,
                           TOP_vpcomd,
                           TOP_vpcomq,
                           TOP_vpcomw,
                           TOP_vpcomub,
                           TOP_vpcomud,
                           TOP_vpcomuq,
                           TOP_vpcomuw,
                           /* AVX instructions */
                           TOP_vpalignr128,
                           TOP_vblend128v16,
                           TOP_vblendv128v8,
                           TOP_vfblendv128v64,
                           TOP_vfblendv128v32,
                           TOP_vfblend128v64,
                           TOP_vfblend128v32,
                           TOP_vpclmulqdq,
                           TOP_vcmpistri,
                           TOP_vcmpistrm,
                           TOP_vfcmp128v64,
                           TOP_vfcmp128v32,
                           TOP_vcmppd,
                           TOP_vcmpps,
                           TOP_vcmpsd,
                           TOP_vcmpss,
                           TOP_vfdp128v64,
                           TOP_vfdp128v32,
                           TOP_vinsr128v8,
                           TOP_vinsr128v32,
                           TOP_vinsr128v64,
                           TOP_vinsr128v16,
                           TOP_vfinsr128v32,
                           TOP_vfinsrf128,
                           TOP_vmpsadbw,
                           TOP_vfperm2f128,
                           TOP_vfshuf128v64,
                           TOP_vfshuf128v32,
                           TOP_vshufpd,
                           TOP_vshufps,
                           TOP_UNDEFINED );


  /* dest=op(src1, mem, src3), non-x86-style */
  ISA_PRINT_TYPE vropmemop = ISA_Print_Type_Create("vropmemop", "%s %s,%s%s(%s),%s,%s");
  Name();
  Operand(3);
  Segment();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropmemop,
                           /* XOP instructions */
                           TOP_vppermx,
                           TOP_vpcmovx,
                           TOP_vpmacsddx,
                           TOP_vpmacsdqhx,
                           TOP_vpmacsdqlx,
                           TOP_vpmacssddx,
                           TOP_vpmacssdqhx,
                           TOP_vpmacssdqlx,
                           TOP_vpmacsswdx,
                           TOP_vpmacsswwx,
                           TOP_vpmacswdx,
                           TOP_vpmacswwx,
                           TOP_vpmadcsswdx,
                           TOP_vpmadcswdx,
                           TOP_vpcombx,
                           TOP_vpcomdx,
                           TOP_vpcomqx,
                           TOP_vpcomwx,
                           TOP_vpcomubx,
                           TOP_vpcomudx,
                           TOP_vpcomuqx,
                           TOP_vpcomuwx,
                           /* AVX instructions */
                           TOP_vpalignrx128,
                           TOP_vblendx128v16,
                           TOP_vblendvx128v8,
                           TOP_vfblendvx128v64,
                           TOP_vfblendvx128v32,
                           TOP_vfblendx128v64,
                           TOP_vfblendx128v32,
                           TOP_vpclmulqdqx,
                           TOP_vcmpistrix,
                           TOP_vcmpistrmx,
                           TOP_vfcmpx128v64,
                           TOP_vfcmpx128v32,
                           TOP_vcmpxsd,
                           TOP_vcmpxss,
                           TOP_vfdpx128v64,
                           TOP_vfdpx128v32,
                           TOP_vinsrx128v8,
                           TOP_vinsrx128v32,
                           TOP_vinsrx128v64,
                           TOP_vinsrx128v16,
                           TOP_vfinsrx128v32,
                           TOP_vfinsrxf128,
                           TOP_vmpsadbwx,
                           TOP_vfperm2xf128,
                           TOP_vfshufx128v64,
                           TOP_vfshufx128v32,
                           TOP_UNDEFINED );
 
  /* dest=op(src1, memindex, src3), non-x86-style */
  ISA_PRINT_TYPE vropmemindexop = ISA_Print_Type_Create("vropmemindexop", "%s %s,%s%s(%s,%s,%s),%s,%s");
  Name();
  Operand(3);
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropmemindexop,
                           /* XOP instructions */
                           TOP_vppermxx,
                           TOP_vpcmovxx,
                           TOP_vpmacsddxx,
                           TOP_vpmacsdqhxx,
                           TOP_vpmacsdqlxx,
                           TOP_vpmacssddxx,
                           TOP_vpmacssdqhxx,
                           TOP_vpmacssdqlxx,
                           TOP_vpmacsswdxx,
                           TOP_vpmacsswwxx,
                           TOP_vpmacswdxx,
                           TOP_vpmacswwxx,
                           TOP_vpmadcsswdxx,
                           TOP_vpmadcswdxx,
                           TOP_vpcombxx,
                           TOP_vpcomdxx,
                           TOP_vpcomqxx,
                           TOP_vpcomwxx,
                           TOP_vpcomubxx,
                           TOP_vpcomudxx,
                           TOP_vpcomuqxx,
                           TOP_vpcomuwxx,
                           /* AVX instructions */
                           TOP_vpalignrxx128,
                           TOP_vblendxx128v16,
                           TOP_vblendvxx128v8,
                           TOP_vfblendvxx128v64,
                           TOP_vfblendvxx128v32,
                           TOP_vfblendxx128v64,
                           TOP_vfblendxx128v32,
                           TOP_vpclmulqdqxx,
                           TOP_vcmpistrixx,
                           TOP_vcmpistrmxx,
                           TOP_vfcmpxx128v64,
                           TOP_vfcmpxx128v32,
                           TOP_vcmpxxsd,
                           TOP_vcmpxxss,
                           TOP_vfdpxx128v64,
                           TOP_vfdpxx128v32,
                           TOP_vinsrxx128v8,
                           TOP_vinsrxx128v32,
                           TOP_vinsrxx128v64,
                           TOP_vinsrxx128v16,
                           TOP_vfinsrxx128v32,
                           TOP_vfinsrxxf128,
                           TOP_vmpsadbwxx,
                           TOP_vfperm2xxf128,
                           TOP_vfshufxx128v64,
                           TOP_vfshufxx128v32,
                           TOP_UNDEFINED );

  /* dest=op(src1, memindexx, src3), non-x86-style */
  ISA_PRINT_TYPE vropmemindexxop = ISA_Print_Type_Create("vropmemindexxop", "%s %s,%s%s(,%s,%s),%s,%s");
  Name();
  Operand(4);
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropmemindexxop,
                           /* XOP instructions */
                           TOP_vppermxxx,
                           TOP_vpcmovxxx,
                           TOP_vpmacsddxxx,
                           TOP_vpmacsdqhxxx,
                           TOP_vpmacsdqlxxx,
                           TOP_vpmacssddxxx,
                           TOP_vpmacssdqhxxx,
                           TOP_vpmacssdqlxxx,
                           TOP_vpmacsswdxxx,
                           TOP_vpmacsswwxxx,
                           TOP_vpmacswdxxx,
                           TOP_vpmacswwxxx,
                           TOP_vpmadcsswdxxx,
                           TOP_vpmadcswdxxx,
                           TOP_vpcombxxx,
                           TOP_vpcomdxxx,
                           TOP_vpcomqxxx,
                           TOP_vpcomwxxx,
                           TOP_vpcomubxxx,
                           TOP_vpcomudxxx,
                           TOP_vpcomuqxxx,
                           TOP_vpcomuwxxx,
                           /* AVX instructions */
                           TOP_vpalignrxxx128,
                           TOP_vblendxxx128v16,
                           TOP_vblendvxxx128v8,
                           TOP_vfblendvxxx128v64,
                           TOP_vfblendvxxx128v32,
                           TOP_vfblendxxx128v64,
                           TOP_vfblendxxx128v32,
                           TOP_vpclmulqdqxxx,
                           TOP_vcmpistrixxx,
                           TOP_vcmpistrmxxx,
                           TOP_vfcmpxxx128v64,
                           TOP_vfcmpxxx128v32,
                           TOP_vcmpxxxsd,
                           TOP_vcmpxxxss,
                           TOP_vfdpxxx128v64,
                           TOP_vfdpxxx128v32,
                           TOP_vinsrxxx128v8,
                           TOP_vinsrxxx128v32,
                           TOP_vinsrxxx128v64,
                           TOP_vinsrxxx128v16,
                           TOP_vfinsrxxx128v32,
                           TOP_vfinsrxxxf128,
                           TOP_vmpsadbwxxx,
                           TOP_vfperm2xxxf128,
                           TOP_vfshufxxx128v64,
                           TOP_vfshufxxx128v32,
                           TOP_UNDEFINED );


  /* dest=op(src1, src2, mem), non-x86-style */
  ISA_PRINT_TYPE vropopmem = ISA_Print_Type_Create("vropopmem", "%s %s%s(%s),%s,%s,%s");
  Name();
  Segment();
  Operand(3);
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropopmem,
                           /* XOP instructions */
                           TOP_vppermxr,
                           TOP_vpcmovxr,
                           TOP_UNDEFINED );

  /* dest=op(src1, src2, memindex), non-x86-style */
  ISA_PRINT_TYPE vropopmemindex = ISA_Print_Type_Create("vropopmemindex", "%s %s%s(%s,%s,%s),%s,%s,%s");
  Name();
  Segment();
  Operand(5);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropopmemindex,
                           /* XOP instructions */
                           TOP_vppermxxr,
                           TOP_vpcmovxxr,
                           TOP_UNDEFINED );

  /* dest=op(src1, src2, memindexx), non-x86-style */
  ISA_PRINT_TYPE vropopmemindexx = ISA_Print_Type_Create("vropopmemindexx", "%s %s%s(,%s,%s),%s,%s,%s");
  Name();
  Segment();
  Operand(4);
  Operand(2);
  Operand(3);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vropopmemindexx,
                           /* XOP instructions */
                           TOP_vppermxxxr,
                           TOP_vpcmovxxxr,
                           TOP_UNDEFINED );

  /* SSE4.1 round pd/ps/sd/ss and AVX pd/ps */
  ISA_PRINT_TYPE round =  ISA_Print_Type_Create("round", "%s %s,%s,%s");
  Name();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( round,
                           /* SSE4.1 instructions */
                           TOP_round128v64,
                           TOP_round128v32,
                           TOP_roundsd,
                           TOP_roundss,
                           /* AVX instructions */
                           TOP_vround128v64,
                           TOP_vround128v32,
                           TOP_UNDEFINED );

  /* round mem */
  ISA_PRINT_TYPE roundmem =  ISA_Print_Type_Create("roundmem", "%s %s,%s%s(%s),%s");
  Name();
  Operand(2);
  Segment();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( roundmem,
                           /* SSE4.1 instructions */
                           TOP_roundx128v64,
                           TOP_roundx128v32,
                           TOP_roundxsd,
                           TOP_roundxss,
                           /* AVX instructions */
                           TOP_vroundx128v64,
                           TOP_vroundx128v32,
                           TOP_UNDEFINED );

  /* round mem w/ scaled index w/ base */
  ISA_PRINT_TYPE roundmemindex =  ISA_Print_Type_Create("roundmemindex", "%s %s,%s%s(%s,%s,%s),%s");
  Name();
  Operand(4);
  Segment();
  Operand(3);
  Operand(0);
  Operand(1);
  Operand(2);
  Result(0);
  Instruction_Print_Group( roundmemindex,
                           /* SSE4.1 instructions */
                           TOP_roundxx128v64,
                           TOP_roundxx128v32,
                           TOP_roundxxsd,
                           TOP_roundxxss,
                           /* AVX instructions */
                           TOP_vroundxx128v64,
                           TOP_vroundxx128v32,
                           TOP_UNDEFINED );

  /* round mem w/ scaled index w/o base */
  ISA_PRINT_TYPE roundmemindexx =  ISA_Print_Type_Create("roundmemindexx", "%s %s,%s%s(,%s,%s),%s");
  Name();
  Operand(3);
  Segment();
  Operand(2);
  Operand(0);
  Operand(1);
  Result(0);
  Instruction_Print_Group( roundmemindexx,
                           /* SSE4.1 instructions */
                           TOP_roundxxx128v64,
                           TOP_roundxxx128v32,
                           TOP_roundxxxsd,
                           TOP_roundxxxss,
                           /* AVX instructions */
                           TOP_vroundxxx128v64,
                           TOP_vroundxxx128v32,
                           TOP_UNDEFINED );
  /* AVX round sd/ss */
  ISA_PRINT_TYPE avxrounds =  ISA_Print_Type_Create("avxrounds", "%s %s,%s,%s,%s");
  Name();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( avxrounds,
                           /* AVX instructions */
                           TOP_vroundsd,
                           TOP_vroundss,
                           TOP_UNDEFINED );

  /* AVX round sd/ss mem */
  ISA_PRINT_TYPE avxroundsmem =  ISA_Print_Type_Create("avxroundsmem", "%s %s,%s%s(%s),%s,%s");
  Name();
  Operand(3);
  Segment();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( avxroundsmem,
                           /* AVX instructions */
                           TOP_vroundxsd,
                           TOP_vroundxss,
                           TOP_UNDEFINED );

  /* AVX round sd/ss mem w/ scaled index w/ base */
  ISA_PRINT_TYPE avxroundsmemindex =  ISA_Print_Type_Create("avxroundsmemindex", "%s %s,%s%s(%s,%s,%s),%s,%s");
  Name();
  Operand(5);
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Result(0);
  Instruction_Print_Group( avxroundsmemindex,
                           /* AVX instructions */
                           TOP_vroundxxsd,
                           TOP_vroundxxss,
                           TOP_UNDEFINED );

  /* AVX round sd/ss mem w/ scaled index w/o base */
  ISA_PRINT_TYPE avxroundsmemindexx =  ISA_Print_Type_Create("avxroundsmemindexx", "%s %s,%s%s(,%s,%s),%s,%s");
  Name();
  Operand(4);
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Result(0);
  Instruction_Print_Group( avxroundsmemindexx,
                           /* AVX instructions */
                           TOP_vroundxxxsd,
                           TOP_vroundxxxss,
                           TOP_UNDEFINED );

  /* SSE4.2 cmpestri/cmpestrm */
  ISA_PRINT_TYPE pcmpestr = ISA_Print_Type_Create("pcmpestr", "%s %s,%s,%s");
  Name();
  Operand(4);
  Operand(2);
  Operand(0);
  Instruction_Print_Group( pcmpestr,
                           /* SSE4.2 instructions */
                           TOP_cmpestri,
                           TOP_cmpestrm,
                           /* AVX instructions */
                           TOP_vcmpestri,
                           TOP_vcmpestrm,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE pcmpestrmem = ISA_Print_Type_Create("pcmpestrmem", "%s %s,%s%s(%s),%s");
  Name();
  Operand(5);
  Segment();
  Operand(3);
  Operand(2);
  Operand(0);
  Instruction_Print_Group( pcmpestrmem,
                           /* SSE4.2 instructions */
                           TOP_cmpestrix,
                           TOP_cmpestrmx,
                           /* AVX instructions */
                           TOP_vcmpestrix,
                           TOP_vcmpestrmx,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE pcmpestrmemindex = ISA_Print_Type_Create("pcmpestrmemindex", "%s %s,%s%s(%s,%s,%s),%s");
  Name();
  Operand(7);
  Segment();
  Operand(5);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(0);
  Instruction_Print_Group( pcmpestr,
                           /* SSE4.2 instructions */
                           TOP_cmpestrixx,
                           TOP_cmpestrmxx,
                           /* AVX instructions */
                           TOP_vcmpestrixx,
                           TOP_vcmpestrmxx,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE pcmpestrmemindexx = ISA_Print_Type_Create("pcmpestrmemindexx", "%s %s,%s%s(,%s,%s),%s");
  Name();
  Operand(6);
  Segment(); 
  Operand(4);
  Operand(2);
  Operand(3);
  Operand(0);
  Instruction_Print_Group( pcmpestr,
                           /* SSE4.2 instructions */
                           TOP_cmpestrixxx,
                           TOP_cmpestrmxxx,
                           /* AVX instructions */
                           TOP_vcmpestrixxx,
                           TOP_vcmpestrmxxx,
                           TOP_UNDEFINED );

  /* SSE4.2 cmpistri/cmpistrm */
  ISA_PRINT_TYPE pcmpistr = ISA_Print_Type_Create("pcmpistr", "%s %s,%s,%s");
  Name();
  Operand(2);
  Operand(1);
  Operand(0);
  Instruction_Print_Group( pcmpistr,
                           /* SSE4.2 instructions */
                           TOP_cmpistri,
                           TOP_cmpistrm,
                           /* AVX instructions */
                           TOP_vcmpistri,
                           TOP_vcmpistrm,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE pcmpistrmem = ISA_Print_Type_Create("pcmpistrmem", "%s %s,%s%s(%s),%s");
  Name();
  Operand(3);
  Segment();
  Operand(2);
  Operand(1);
  Operand(0);
  Instruction_Print_Group( pcmpistrmem,
                           /* SSE4.2 instructions */
                           TOP_cmpistrix,
                           TOP_cmpistrmx,
                           /* AVX instructions */
                           TOP_vcmpistrix,
                           TOP_vcmpistrmx,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE pcmpistrmemindex = ISA_Print_Type_Create("pcmpistrmemindex", "%s %s,%s%s(%s,%s,%s),%s");
  Name();
  Operand(5);
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Instruction_Print_Group( pcmpistr,
                           /* SSE4.2 instructions */
                           TOP_cmpistrixx,
                           TOP_cmpistrmxx,
                           /* AVX instructions */
                           TOP_vcmpistrixx,
                           TOP_vcmpistrmxx,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE pcmpistrmemindexx = ISA_Print_Type_Create("pcmpistrmemindexx", "%s %s,%s%s(,%s,%s),%s");
  Name();
  Operand(4);
  Segment(); 
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Instruction_Print_Group( pcmpistr,
                           /* SSE4.2 instructions */
                           TOP_cmpistrixxx,
                           TOP_cmpistrmxxx,
                           /* AVX instructions */
                           TOP_vcmpistrixxx,
                           TOP_vcmpistrmxxx,
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

  ISA_PRINT_TYPE ijmpx = ISA_Print_Type_Create("opop", "%s %s%s(%s)");
  Name();
  Segment();
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
                           TOP_clflush,
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
			   TOP_not8,
			   TOP_not16,
			   TOP_not32,
			   TOP_not64,
			   TOP_neg8,
			   TOP_neg16,
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
			   TOP_setc,
			   TOP_seto,
			   TOP_sets,
			   TOP_setz,
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

  /* Zeroing an integer register */
  ISA_PRINT_TYPE vrr =  ISA_Print_Type_Create("vrr", "%s %s,%s,%s");
  Name();
  Result(0);
  Result(0);
  Result(0);
  Instruction_Print_Group( vrr,
                           /* AVX instructions */
                           TOP_vxzero128v64,
                           TOP_vxzero64,
                           TOP_vxzero128v32,
                           TOP_vxzero32,
			   TOP_UNDEFINED );

  /* One result / one operand, special form, uses opnd 1 */
  ISA_PRINT_TYPE ropspec =  ISA_Print_Type_Create("ropspec", "%s %s,%s");
  Name();
  Operand(1);
  Result(0);
  Instruction_Print_Group( ropspec,
			   TOP_movsd,
			   TOP_movss,
			   TOP_UNDEFINED );

  /* One result / one operand */
  ISA_PRINT_TYPE rop =  ISA_Print_Type_Create("rop", "%s %s,%s");
  Name();
  Operand(0);
  Result(0);
  Instruction_Print_Group( rop,
			   TOP_fmovsldup,
			   TOP_fmovshdup,
			   TOP_fmovddup,
			   TOP_movlhps,
			   TOP_movhlps,
			   TOP_ld8_abs,
			   TOP_ld16_abs,
			   TOP_ld32_abs,
			   TOP_ld64_abs,
			   TOP_ld64_off,
			   TOP_ldss_n32,
			   TOP_ldsd_n32,
			   TOP_ld8_32_n32,
			   TOP_ldu8_32_n32,
			   TOP_ld16_32_n32,
			   TOP_ldu16_32_n32,
			   TOP_ld32_n32,
			   TOP_ld8_64_off,
			   TOP_ldu8_64_off,
			   TOP_ld16_64_off,
			   TOP_ldu16_64_off,
			   TOP_ld32_64_off,
			   TOP_ldaps_n32,
			   TOP_ldups_n32,
			   TOP_ldapd_n32,
			   TOP_ldupd_n32,
			   TOP_lddqa_n32,
			   TOP_lddqu_n32,
			   TOP_ldlps_n32,
			   TOP_ldhps_n32,
			   TOP_ldlpd_n32,
			   TOP_ldhpd_n32,
                           TOP_stups_n32,
                           TOP_stupd_n32,
			   TOP_cvtss2si,
			   TOP_cvtsd2si,
			   TOP_cvtss2siq,
			   TOP_cvtsd2siq,
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
			   TOP_cvtps2dq,
			   TOP_cvttps2dq,
			   TOP_cvtpd2dq,
			   TOP_cvttpd2dq,
			   TOP_cvtpi2ps,
			   TOP_cvtps2pi,
			   TOP_cvttps2pi,
			   TOP_cvtpi2pd,
			   TOP_cvtpd2pi,
			   TOP_cvttpd2pi,
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
			   TOP_movdq,
			   TOP_movapd,
			   TOP_movaps,
			   TOP_movq2dq,
			   TOP_movdq2q,
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
			   TOP_pmovmskb128,
			   TOP_movi32_2m,
			   TOP_movi64_2m,
			   TOP_movm_2i32,
			   TOP_movm_2i64,
			   TOP_ld64_2m_n32,
			   TOP_ld64_2sse_n32,
			   TOP_movmskps,
			   TOP_movmskpd,
                           /* SSE3 instructions */
                           TOP_pabs128v8,
                           TOP_pabs128v16,
                           TOP_pabs128v32,
                           /* SSE4.1 instructions */
                           TOP_phminposuw,
                           TOP_pmovsxbw,
                           TOP_pmovzxbw,
                           TOP_pmovsxbd,
                           TOP_pmovzxbd,
                           TOP_pmovsxbq,
                           TOP_pmovzxbq,
                           TOP_pmovsxwd,
                           TOP_pmovzxwd,
                           TOP_pmovsxwq,
                           TOP_pmovzxwq,
                           TOP_pmovsxdq,
                           TOP_pmovzxdq,
                           /* SSE4.2 instructions */
                           TOP_popcnt16,
                           TOP_popcnt32,
                           TOP_popcnt64,
                           /* AES instructions */
                           TOP_aesimc,
                           /* XOP instructions */
                           TOP_vphaddbd,
                           TOP_vphaddbq,
                           TOP_vphaddbw,
                           TOP_vphadddq,
                           TOP_vphaddubd,
                           TOP_vphaddubq,
                           TOP_vphaddubw,
                           TOP_vphaddudq,
                           TOP_vphadduwd,
                           TOP_vphadduwq,
                           TOP_vphaddwd,
                           TOP_vphaddwq,
                           TOP_vphsubbw,
                           TOP_vphsubdq,
                           TOP_vphsubwd,
                           TOP_vfrczpd,
                           TOP_vfrczps,
                           TOP_vfrczsd,
                           TOP_vfrczss,
                           /* AVX instructions */
                           TOP_vabs128v8,
                           TOP_vabs128v32,
                           TOP_vabs128v16,
                           TOP_vcvtsd2si,
                           TOP_vcvtsd2siq,
                           TOP_vcvtss2si,
                           TOP_vcvtss2siq,
                           TOP_vcvtdq2pd,
                           TOP_vcvtdq2ps,
                           TOP_vcvtpd2dq,
                           TOP_vcvtpd2dqy,
                           TOP_vcvtpd2ps,
                           TOP_vcvtpd2psy,
                           TOP_vcvtps2dq,
                           TOP_vcvtps2pd,
                           TOP_vcvttpd2dq,
                           TOP_vcvttpd2dqy,
                           TOP_vcvttsd2si,
                           TOP_vcvttsd2siq,
                           TOP_vcvttss2si,
                           TOP_vcvttss2siq,
                           TOP_vcvttps2dq,
                           TOP_vphminposuw,
                           TOP_vpsign128v8,
                           TOP_vpsign128v32,
                           TOP_vpsign128v16,
                           TOP_vmaskmovdqu,
                           TOP_vmovapd,
                           TOP_vmovaps,
                           TOP_vmovddup,
                           TOP_vmovdqa,
                           TOP_vmovshdup,
                           TOP_vmovsldup,
                           TOP_vpmovmskb128,
                           TOP_vmovg2x,
                           TOP_vmovg2x64,
                           TOP_vmovx2g,
                           TOP_vmovx2g64,
                           TOP_vpmovsxbd,
                           TOP_vpmovsxbq,
                           TOP_vpmovsxbw,
                           TOP_vpmovsxdq,
                           TOP_vpmovsxwd,
                           TOP_vpmovsxwq,
                           TOP_vpmovzxbd,
                           TOP_vpmovzxbq,
                           TOP_vpmovzxbw,
                           TOP_vpmovzxdq,
                           TOP_vpmovzxwd,
                           TOP_vpmovzxwq,
                           TOP_vfrcp128v32,
                           TOP_vfrsqrt128v32,
                           TOP_vfsqrt128v64,
                           TOP_vfsqrt128v32,
                           TOP_vlddqu_n32,
                           TOP_vst64_fsse,
                           TOP_vld64_2sse_n32,
                           TOP_vst64_fsse_n32,
                           TOP_vldss_n32,
                           TOP_vldsd_n32,
                           TOP_vlddqa_n32,
                           TOP_vldapd_n32,
                           TOP_vldaps_n32,
                           TOP_vldupd_n32,
                           TOP_vldups_n32,
                           TOP_vstdqa_n32,
                           TOP_vstdqu_n32,
                           TOP_vstapd_n32,
                           TOP_vstaps_n32,
                           TOP_vstlpd_n32,
                           TOP_vstups_n32,
                           TOP_vstupd_n32,
                           TOP_vsthpd_n32,
                           TOP_vsthps_n32,
                           TOP_vstlps_n32,
                           TOP_vaesimc,
			   TOP_UNDEFINED );

  /* One result / two operands */
  ISA_PRINT_TYPE rop2 =  ISA_Print_Type_Create("rop2", "%s %s %s,%s");
  Name();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( rop,
                           TOP_vldhpd_n32,
                           TOP_vldhps_n32,
                           TOP_vldlpd_n32,
                           TOP_vldlps_n32,
			   TOP_UNDEFINED );

  /* One result / one mem opnd */
  ISA_PRINT_TYPE rmem =  ISA_Print_Type_Create("rmem", "%s %s%s(%s),%s");
  Name();
  Segment();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( rmem,
                           /* SSSE3 instructions */
                           TOP_pabsx128v8,
                           TOP_pabsx128v16,
                           TOP_pabsx128v32,
                           /* SSE4.1 instructions */
                           TOP_phminposuwx,
                           TOP_pmovsxbwx,
                           TOP_pmovzxbwx,
                           TOP_pmovsxbdx,
                           TOP_pmovzxbdx,
                           TOP_vldlpd_n32,
                           TOP_pmovsxbqx,
                           TOP_pmovzxbqx,
                           TOP_pmovsxwdx,
                           TOP_pmovzxwdx,
                           TOP_pmovsxwqx,
                           TOP_pmovzxwqx,
                           TOP_pmovsxdqx,
                           TOP_pmovzxdqx,
                           /* SSE4.2 instructions */
                           TOP_popcntx16,
                           TOP_popcntx32,
                           TOP_popcntx64,
                           /* AES instructions */
                           TOP_aesimcx,
                           /* XOP instructions */
                           TOP_vphaddbdx,
                           TOP_vphaddbqx,
                           TOP_vphaddbwx,
                           TOP_vphadddqx,
                           TOP_vphaddubdx,
                           TOP_vphaddubqx,
                           TOP_vphaddubwx,
                           TOP_vphaddudqx,
                           TOP_vphadduwdx,
                           TOP_vphadduwqx,
                           TOP_vphaddwdx,
                           TOP_vphaddwqx,
                           TOP_vphsubbwx,
                           TOP_vphsubdqx,
                           TOP_vphsubwdx,
                           TOP_vfrczpdx,
                           TOP_vfrczpsx,
                           TOP_vfrczsdx,
                           TOP_vfrczssx,
                           /* AVX instructions */
                           TOP_vabsx128v8,
                           TOP_vabsx128v32,
                           TOP_vabsx128v16,
                           TOP_vcvtdq2pdx,
                           TOP_vcvtdq2psx,
                           TOP_vcvtpd2dqx,
                           TOP_vcvtpd2dqyx,
                           TOP_vcvtpd2psx,
                           TOP_vcvtpd2psyx,
                           TOP_vcvtps2dqx,
                           TOP_vcvtps2pdx,
                           TOP_vcvttpd2dqx,
                           TOP_vcvttpd2dqyx,
                           TOP_vcvttps2dqx,
                           TOP_vphminposuwx,
                           TOP_vmovddupx,
                           TOP_vmovshdupx,
                           TOP_vmovsldupx,
                           TOP_vpmovsxbdx,
                           TOP_vpmovsxbqx,
                           TOP_vpmovsxbwx,
                           TOP_vpmovsxdqx,
                           TOP_vpmovsxwdx,
                           TOP_vpmovsxwqx,
                           TOP_vpmovzxbdx,
                           TOP_vpmovzxbqx,
                           TOP_vpmovzxbwx,
                           TOP_vpmovzxdqx,
                           TOP_vpmovzxwdx,
                           TOP_vpmovzxwqx,
                           TOP_vfrcpx128v32,
                           TOP_vfrsqrtx128v32,
                           TOP_vpsignx128v8,
                           TOP_vpsignx128v32,
                           TOP_vpsignx128v16,
                           TOP_vfsqrtx128v64,
                           TOP_vfsqrtx128v32,
                           TOP_vaesimcx,
			   TOP_UNDEFINED );

  /* One result / one mem opnd with scaled-index with base */
  ISA_PRINT_TYPE rmemindex =  ISA_Print_Type_Create("rmemindex", "%s %s%s(%s,%s,%s),%s");
  Name();
  Segment(); 
  Operand(3);
  Operand(0);
  Operand(1);
  Operand(2);
  Result(0);
  Instruction_Print_Group( rmemindex,
                           /* SSSE3 instructions */
                           TOP_pabsxx128v8,
                           TOP_pabsxx128v16,
                           TOP_pabsxx128v32,
                           /* SSE4.1 instructions */
                           TOP_phminposuwxx,
                           TOP_pmovsxbwxx,
                           TOP_pmovzxbwxx,
                           TOP_pmovsxbdxx,
                           TOP_pmovzxbdxx,
                           TOP_pmovsxbqxx,
                           TOP_pmovzxbqxx,
                           TOP_pmovsxwdxx,
                           TOP_pmovzxwdxx,
                           TOP_pmovsxwqxx,
                           TOP_pmovzxwqxx,
                           TOP_pmovsxdqxx,
                           TOP_pmovzxdqxx,
                           /* SSE4.2 instructions */
                           TOP_popcntxx16,
                           TOP_popcntxx32,
                           TOP_popcntxx64,
                           /* AES instructions */
                           TOP_aesimcxx,
                           /* XOP instructions */
                           TOP_vphaddbdxx,
                           TOP_vphaddbqxx,
                           TOP_vphaddbwxx,
                           TOP_vphadddqxx,
                           TOP_vphaddubdxx,
                           TOP_vphaddubqxx,
                           TOP_vphaddubwxx,
                           TOP_vphaddudqxx,
                           TOP_vphadduwdxx,
                           TOP_vphadduwqxx,
                           TOP_vphaddwdxx,
                           TOP_vphaddwqxx,
                           TOP_vphsubbwxx,
                           TOP_vphsubdqxx,
                           TOP_vphsubwdxx,
                           TOP_vfrczpdxx,
                           TOP_vfrczpsxx,
                           TOP_vfrczsdxx,
                           TOP_vfrczssxx,
                           /* AVX instructions */
                           TOP_vabsxx128v8,
                           TOP_vabsxx128v32,
                           TOP_vabsxx128v16,
                           TOP_vphminposuwxx,
                           TOP_vmovddupxx,
                           TOP_vmovshdupxx,
                           TOP_vmovsldupxx,
                           TOP_vpmovsxbdxx,
                           TOP_vpmovsxbqxx,
                           TOP_vpmovsxbwxx,
                           TOP_vpmovsxdqxx,
                           TOP_vpmovsxwdxx,
                           TOP_vpmovsxwqxx,
                           TOP_vpmovzxbdxx,
                           TOP_vpmovzxbqxx,
                           TOP_vpmovzxbwxx,
                           TOP_vpmovzxdqxx,
                           TOP_vpmovzxwdxx,
                           TOP_vpmovzxwqxx,
                           TOP_vfrcpxx128v32,
                           TOP_vfrsqrtxx128v32,
                           TOP_vpsignxx128v8,
                           TOP_vpsignxx128v32,
                           TOP_vpsignxx128v16,
                           TOP_vfsqrtxx128v64,
                           TOP_vfsqrtxx128v32,
                           TOP_vaesimcxx,
			   TOP_UNDEFINED );

  /* One result / one mem opnd with scaled-index w/o base */
  ISA_PRINT_TYPE rmemindexx =  ISA_Print_Type_Create("rmemindexx", "%s %s%s(,%s,%s),%s");
  Name();
  Segment(); 
  Operand(2);
  Operand(0);
  Operand(1);
  Result(0);
  Instruction_Print_Group( rmemindexx,
                           /* SSSE3 instructions */
                           TOP_pabsxxx128v8,
                           TOP_pabsxxx128v16,
                           TOP_pabsxxx128v32,
                           /* SSE4.1 instructions */
                           TOP_phminposuwxxx,
                           TOP_pmovsxbwxxx,
                           TOP_pmovzxbwxxx,
                           TOP_pmovsxbdxxx,
                           TOP_pmovzxbdxxx,
                           TOP_pmovsxbqxxx,
                           TOP_pmovzxbqxxx,
                           TOP_pmovsxwdxxx,
                           TOP_pmovzxwdxxx,
                           TOP_pmovsxwqxxx,
                           TOP_pmovzxwqxxx,
                           TOP_pmovsxdqxxx,
                           TOP_pmovzxdqxxx,
                           /* SSE4.2 instructions */
                           TOP_popcntxxx16,
                           TOP_popcntxxx32,
                           TOP_popcntxxx64,
                           /* AES instructions */
                           TOP_aesimcxxx,
                           /* XOP instructions */
                           TOP_vphminposuwxxx,
                           TOP_vphaddbdxxx,
                           TOP_vphaddbqxxx,
                           TOP_vphaddbwxxx,
                           TOP_vphadddqxxx,
                           TOP_vphaddubdxxx,
                           TOP_vphaddubqxxx,
                           TOP_vphaddubwxxx,
                           TOP_vphaddudqxxx,
                           TOP_vphadduwdxxx,
                           TOP_vphadduwqxxx,
                           TOP_vphaddwdxxx,
                           TOP_vphaddwqxxx,
                           TOP_vphsubbwxxx,
                           TOP_vphsubdqxxx,
                           TOP_vphsubwdxxx,
                           TOP_vfrczpdxxx,
                           TOP_vfrczpsxxx,
                           TOP_vfrczsdxxx,
                           TOP_vfrczssxxx,
                           /* AVX instructions */
                           TOP_vabsxxx128v8,
                           TOP_vabsxxx128v32,
                           TOP_vabsxxx128v16,
                           TOP_vmovddupxxx,
                           TOP_vmovshdupxxx,
                           TOP_vmovsldupxxx,
                           TOP_vpmovsxbdxxx,
                           TOP_vpmovsxbqxxx,
                           TOP_vpmovsxbwxxx,
                           TOP_vpmovsxdqxxx,
                           TOP_vpmovsxwdxxx,
                           TOP_vpmovsxwqxxx,
                           TOP_vpmovzxbdxxx,
                           TOP_vpmovzxbqxxx,
                           TOP_vpmovzxbwxxx,
                           TOP_vpmovzxdqxxx,
                           TOP_vpmovzxwdxxx,
                           TOP_vpmovzxwqxxx,
                           TOP_vpsignxxx128v8,
                           TOP_vpsignxxx128v32,
                           TOP_vpsignxxx128v16,
                           TOP_vfrcpxxx128v32,
                           TOP_vfrsqrtxxx128v32,
                           TOP_vfsqrtxxx128v64,
                           TOP_vfsqrtxxx128v32,
                           TOP_vaesimcxxx,
			   TOP_UNDEFINED );

  /* Two operands / no result */
  ISA_PRINT_TYPE opop1 =  ISA_Print_Type_Create("opop1", "%s %s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group( opop1,
			   TOP_store8_abs,
			   TOP_store16_abs,
			   TOP_store32_abs,
			   TOP_store64_abs,
			   TOP_store64_off,
			   TOP_storei64_off,
			   TOP_stss_n32,
			   TOP_stsd_n32,
			   TOP_store8_n32,
			   TOP_store16_n32,
			   TOP_store32_n32,
			   TOP_storei8_n32,
			   TOP_storei16_n32,
			   TOP_storei32_n32,
			   TOP_staps_n32,
			   TOP_stapd_n32,
			   TOP_stdqa_n32,
			   TOP_stdqu_n32,
			   TOP_stlps_n32,
			   TOP_sthps_n32,
			   TOP_stlpd_n32,
			   TOP_sthpd_n32,
			   TOP_store64_fm_n32,
			   TOP_store64_fsse_n32,
			   TOP_maskmovdqu,
			   TOP_maskmovq,
                           /* AVX instructions */
                           TOP_vstsd_n32,
                           TOP_vstss_n32,
			   TOP_UNDEFINED );


  /* regular load */
  ISA_PRINT_TYPE load =  ISA_Print_Type_Create("load", "%s %s%s(%s),%s");
  Name();
  Segment();
  Operand(1);
  Operand(0);
  Result(0);
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
                           TOP_cvtpd2dq_x,
			   TOP_cvtpd2ps_x,
			   TOP_cvtps2dq_x,
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
			   TOP_ldupd,
			   TOP_ldups,
			   TOP_lea32,
			   TOP_lea64,
			   TOP_fmovsldupx,
			   TOP_fmovshdupx,
			   TOP_fmovddupx,			   
			   TOP_ld64_2m,
			   TOP_ld64_2sse,
                           /* SSE4.1 instructions */
                           TOP_ldntdqa,
                           /* AVX instructions */
                           TOP_vlddqu,
                           TOP_vld64_2sse,
                           TOP_vlddqa,
                           TOP_vldntdqa,
                           TOP_vldapd,
                           TOP_vldaps,
                           TOP_vldss,
                           TOP_vldupd,
                           TOP_vldups,
                           TOP_vldsd,
                           TOP_vfbroadcastss,
                           TOP_vfbroadcastsd,
                           TOP_vfbroadcastf128,
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
                           TOP_cvtpd2dq_xx,
			   TOP_cvtpd2ps_xx,
			   TOP_cvtps2dq_xx,
			   TOP_cvttps2dq_xx,
			   TOP_cvttpd2dq_xx,
                           TOP_vcvtdq2pdxx,
                           TOP_vcvtdq2psxx,
                           TOP_vcvtpd2dqxx,
                           TOP_vcvtpd2dqyxx,
                           TOP_vcvtpd2psxx,
                           TOP_vcvtpd2psyxx,
                           TOP_vcvtps2dqxx,
                           TOP_vcvtps2pdxx,
                           TOP_vcvttpd2dqxx,
                           TOP_vcvttpd2dqyxx,
                           TOP_vcvttps2dqxx,
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
                           TOP_cvtpd2dq_xxx,
			   TOP_cvtpd2ps_xxx,
			   TOP_cvtps2dq_xxx,
			   TOP_cvttps2dq_xxx,
			   TOP_cvttpd2dq_xxx,
                           TOP_vcvtdq2pdxxx,
                           TOP_vcvtdq2psxxx,
                           TOP_vcvtpd2dqxxx,
                           TOP_vcvtpd2dqyxxx,
                           TOP_vcvtpd2psxxx,
                           TOP_vcvtpd2psyxxx,
                           TOP_vcvtps2dqxxx,
                           TOP_vcvtps2pdxxx,
                           TOP_vcvttpd2dqxxx,
                           TOP_vcvttpd2dqyxxx,
                           TOP_vcvttps2dqxxx,
			   TOP_UNDEFINED );

  /* load instruction with indx */
  ISA_PRINT_TYPE ldx = ISA_Print_Type_Create("ldx", "%s %s%s(%s,%s,%s), %s");
  Name();
  Segment();
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
			   TOP_ldupdx,
			   TOP_ldlpsx,
			   TOP_ldhpsx,
			   TOP_ldlpdx,
			   TOP_ldhpdx,
			   TOP_ldx32_64,
			   TOP_ldapdx,
			   TOP_ldapsx,
			   TOP_ldupsx,
			   TOP_fmovsldupxx,
			   TOP_fmovshdupxx,
			   TOP_fmovddupxx,			   
                           /* SSE4.1 instructions */
                           TOP_ldntdqax,
                           /* AVX instructions */
                           TOP_vlddqux,
                           TOP_vldx64_2sse,
                           TOP_vlddqax,
                           TOP_vldntdqax,
                           TOP_vldapdx,
                           TOP_vldapsx,
                           TOP_vldssx,
                           TOP_vldupdx,
                           TOP_vldupsx,
                           TOP_vldsdx,
                           TOP_vfbroadcastxss,
                           TOP_vfbroadcastxsd,
                           TOP_vfbroadcastxf128,
			   TOP_UNDEFINED );

  /* load instruction with indx w/o base*/
  ISA_PRINT_TYPE ldxx = ISA_Print_Type_Create("ldxx", "%s %s%s(,%s,%s), %s");
  Name();
  Segment();
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
			   TOP_ldupdxx,
			   TOP_ldlpsxx,
			   TOP_ldhpsxx,
			   TOP_ldlpdxx,
			   TOP_ldhpdxx,
			   TOP_ldxx32_64,
			   TOP_ldapdxx,
			   TOP_ldupsxx,
			   TOP_ldapsxx,
			   TOP_fmovsldupxxx,
			   TOP_fmovshdupxxx,
			   TOP_fmovddupxxx,			   
                           /* SSE4.1 instructions */
                           TOP_ldntdqaxx,
                           /* AVX instructions */
                           TOP_vlddquxx,
                           TOP_vldxx64_2sse,
                           TOP_vlddqaxx,
                           TOP_vldntdqaxx,
                           TOP_vldapdxx,
                           TOP_vldapsxx,
                           TOP_vldssxx,
                           TOP_vldupdxx,
                           TOP_vldupsxx,
                           TOP_vldsdxx,
                           TOP_vfbroadcastxxss,
                           TOP_vfbroadcastxxsd,
                           TOP_vfbroadcastxxf128,
			   TOP_UNDEFINED );

  /* store instruction with indx */
  ISA_PRINT_TYPE storex = ISA_Print_Type_Create("storex", "%s %s,%s%s(%s,%s,%s)");
  Name();
  Operand(0);
  Segment();
  Operand(2);
  Operand(1);
  Operand(3);
  Operand(4);
  Instruction_Print_Group( storex,
			   TOP_storex8,
			   TOP_storex16,
			   TOP_storex32,
			   TOP_storex64,
			   TOP_storeix8,
			   TOP_storeix16,
			   TOP_storeix32,
			   TOP_storeix64,
			   TOP_storentix32,
			   TOP_storentix64,
			   TOP_stssx,
			   TOP_stsdx,
			   TOP_stntssx,
			   TOP_stntsdx,
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
                           TOP_stupdx,
                           TOP_stupsx,
                           /* SSE4.1 instructions */
                           TOP_stntdqx,
                           /* AVX instructions */
                           TOP_vstx64_fsse,
                           TOP_vstdqax,
                           TOP_vstapdx,
                           TOP_vstapsx,
                           TOP_vstlpdx,
                           TOP_vstsdx,
                           TOP_vstssx,
                           TOP_vstorentxi128,
                           TOP_vstntdqx,
                           TOP_vstntpdx,
                           TOP_vstntpsx,
                           TOP_vstntsdx,
                           TOP_vstntssx,
                           TOP_vstdqux,
                           TOP_vstupdx,
                           TOP_vstupsx,
                           TOP_vsthpdx,
                           TOP_vsthpsx,
                           TOP_vstlpsx,
			   TOP_UNDEFINED );

  /* load_gs_seg_off and load_fs_seg_off are special in that the
     segment prefix is hard-coded into the OP code.  Normally the segment is
     specified through Segment(), which tells CG to determine the segment
     override based on whether the base TN is a pointer to a non-default
     segment, or if the offset symbol has relocation in a non-default segment.
     However, for offset-only address mode, there is no base TN, and sometimes
     the offset is a literal, which doesn't have relocation info. */

  /* load from %gs segemnt with offset only */
  ISA_PRINT_TYPE load_gs_seg_off =
    ISA_Print_Type_Create("load_gs_seg_off", "%s %%gs:%s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group( load_gs_seg_off,
			   TOP_ld32_gs_seg_off,
			   TOP_UNDEFINED );

  /* load from %fs segemnt with offset only */
  ISA_PRINT_TYPE load_fs_seg_off =
    ISA_Print_Type_Create("load_fs_seg_off", "%s %%fs:%s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group( load_fs_seg_off,
			   TOP_ld64_fs_seg_off,
			   TOP_UNDEFINED );

  /* store instruction with indx w/o base*/
  ISA_PRINT_TYPE storexx = ISA_Print_Type_Create("storexx", "%s %s,%s%s(,%s,%s)");
  Name();
  Operand(0);
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Instruction_Print_Group( storexx,
			   TOP_storexx8,
			   TOP_storexx16,
			   TOP_storexx32,
			   TOP_storexx64,
			   TOP_storeixx8,
			   TOP_storeixx16,
			   TOP_storeixx32,
			   TOP_storeixx64,
			   TOP_storentixx32,
			   TOP_storentixx64,
			   TOP_stssxx,
			   TOP_stsdxx,
			   TOP_stntssxx,
			   TOP_stntsdxx,
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
                           TOP_stupdxx,
                           TOP_stupsxx,
                           /* SSE4.1 instructions */
                           TOP_stntdqxx,
                           /* AVX instructions */
                           TOP_vstxx64_fsse,
                           TOP_vstdqaxx,
                           TOP_vstapdxx,
                           TOP_vstapsxx,
                           TOP_vstlpdxx,
                           TOP_vstsdxx,
                           TOP_vstssxx,
                           TOP_vstorentxxi128,
                           TOP_vstntdqxx,
                           TOP_vstntpdxx,
                           TOP_vstntpsxx,
                           TOP_vstntsdxx,
                           TOP_vstntssxx,
                           TOP_vstdquxx,
                           TOP_vstupdxx,
                           TOP_vstupsxx,
                           TOP_vsthpdxx,
                           TOP_vsthpsxx,
                           TOP_vstlpsxx,
			   TOP_UNDEFINED );

  /* prefetch */
  ISA_PRINT_TYPE prefetch =  ISA_Print_Type_Create("prefetch", "%s %s%s(%s)");
  Name();
  Segment();
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
  ISA_PRINT_TYPE prefetchx =  ISA_Print_Type_Create("prefetchx", "%s %s%s(%s,%s,%s)");
  Name();
  Segment();
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

  ISA_PRINT_TYPE prefetchxx =  ISA_Print_Type_Create("prefetchxx", "%s %s%s(,%s,%s)");
  Name();
  Segment();
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
  ISA_PRINT_TYPE store =  ISA_Print_Type_Create("store", "%s %s,%s%s(%s)");
  Name();
  Operand(0);
  Segment();
  Operand(2);
  Operand(1);
  Instruction_Print_Group( store,
			   TOP_store8,
			   TOP_store16,
			   TOP_store32,
			   TOP_store64,
			   TOP_storei8,
			   TOP_storei16,
			   TOP_storei32,
			   TOP_storei64,
			   TOP_storenti32,
			   TOP_storenti64,
			   TOP_stsd,
			   TOP_stss,
			   TOP_stntsd,
			   TOP_stntss,
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
			   TOP_stups,
			   TOP_stupd,
			   TOP_store64_fm,
			   TOP_store64_fsse,
			   TOP_storenti128,
			   TOP_storelpd,
			   TOP_storent64_fm,
                           /* SSE4.1 instructions */
                           TOP_stntdq,
                           /* AVX instructions */
                           TOP_vstdqa,
                           TOP_vstapd,
                           TOP_vstaps,
                           TOP_vstorelpd,
                           TOP_vstlpd,
                           TOP_vstsd,
                           TOP_vstss,
                           TOP_vstorenti128,
                           TOP_vstntdq,
                           TOP_vstntpd,
                           TOP_vstntps,
                           TOP_vstntsd,
                           TOP_vstntss,
                           TOP_vstdqu,
                           TOP_vstupd,
                           TOP_vstups,
                           TOP_vsthpd,
                           TOP_vsthps,
                           TOP_vstlps,
			   TOP_UNDEFINED );

  /* AVX mask load */
  ISA_PRINT_TYPE vmaskload = ISA_Print_Type_Create("vmaskload", "%s %s%s(%s),%s,%s");
  Name();
  Segment();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vmaskload,
                           /* AVX instructions */
                           TOP_vfmaskld128v32,
                           TOP_vfmaskld128v64,
                           TOP_UNDEFINED );

  /* AVX mask load w/ scaled index w/ base */
  ISA_PRINT_TYPE vmaskloadindex = ISA_Print_Type_Create("vmaskloadindex", "%s %s%s(%s,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vmaskloadindex,
                           /* AVX instructions */
                           TOP_vfmaskldx128v32,
                           TOP_vfmaskldx128v64,
                           TOP_UNDEFINED );

  /* AVX mask load w/ scaled index w/o base */
  ISA_PRINT_TYPE vmaskloadindexx = ISA_Print_Type_Create("vmaskloadindexx", "%s %s%s(,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Result(0);
  Instruction_Print_Group( vmaskloadindexx,
                           /* AVX instructions */
                           TOP_vfmaskldxx128v32,
                           TOP_vfmaskldxx128v64,
                           TOP_UNDEFINED );

  /* AVX mask store */
  ISA_PRINT_TYPE vmaskstore = ISA_Print_Type_Create("vmaskstore", "%s %s,%s,%s%s(%s)");
  Name();
  Operand(0);
  Operand(1);
  Segment();
  Operand(3);
  Operand(2);
  Instruction_Print_Group( vmaskstore,
                           /* AVX instructions */
                           TOP_vfmaskst128v32,
                           TOP_vfmaskst128v64,
                           TOP_UNDEFINED );

  /* AVX mask store w/ scaled index w/ base */
  ISA_PRINT_TYPE vmaskstoreindex = ISA_Print_Type_Create("vmaskstoreindex", "%s %s,%s,%s%s(%s,%s,%s)");
  Name();
  Operand(0);
  Operand(1);
  Segment();
  Operand(5);
  Operand(2);
  Operand(3);
  Operand(4);
  Instruction_Print_Group( vmaskstoreindex,
                           /* AVX instructions */
                           TOP_vfmaskstx128v32,
                           TOP_vfmaskstx128v64,
                           TOP_UNDEFINED );

  /* AVX mask store w/ scaled index w/o base */
  ISA_PRINT_TYPE vmaskstoreindexx = ISA_Print_Type_Create("vmaskstoreindexx", "%s %s,%s,%s%s(,%s,%s)");
  Name();
  Operand(0);
  Operand(1);
  Segment();
  Operand(4);
  Operand(2);
  Operand(3);
  Instruction_Print_Group( vmaskstoreindexx,
                           /* AVX instructions */
                           TOP_vfmaskstxx128v32,
                           TOP_vfmaskstxx128v64,
                           TOP_UNDEFINED );

  /* instructions that read-modify-write */
  ISA_PRINT_TYPE rmw =  ISA_Print_Type_Create("rmw", "lock %s %s,%s%s(%s)");
  Name();
  Operand(0);
  Segment();
  Operand(2);
  Operand(1);
  Instruction_Print_Group( rmw,
			   TOP_lock_add8,
			   TOP_lock_add16,
			   TOP_lock_add32,
			   TOP_lock_adc32,
			   TOP_lock_add64,
			   TOP_lock_and8,
			   TOP_lock_and16,
			   TOP_lock_and32,
			   TOP_lock_and64,
			   TOP_lock_or8,
			   TOP_lock_or16,
			   TOP_lock_or32,
			   TOP_lock_or64,
			   TOP_lock_xor8,
			   TOP_lock_xor16,
			   TOP_lock_xor32,
			   TOP_lock_xor64,
			   TOP_lock_sub8,
			   TOP_lock_sub16,
			   TOP_lock_sub32,
			   TOP_lock_sub64,
			   TOP_lock_xadd8,
			   TOP_lock_xadd16,
			   TOP_lock_xadd32,
			   TOP_lock_xadd64,
			   TOP_UNDEFINED );

  /* xchg */
  ISA_PRINT_TYPE xchg =  ISA_Print_Type_Create("xchg", "lock %s %s%s(%s),%s");
  Name();
  Segment();
  Operand(2);
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( xchg,
			   TOP_lock_xchg8,
			   TOP_lock_xchg16,
			   TOP_lock_xchg32,
			   TOP_lock_xchg64,
			   TOP_UNDEFINED );

  ISA_PRINT_TYPE cmpxchg =  ISA_Print_Type_Create("cmpxchg", "lock %s %s,%s%s(%s)");
  Name();
  Operand(1);
  Segment();
  Operand(3);
  Operand(2);
  Instruction_Print_Group( cmpxchg,
			   TOP_lock_cmpxchg8,
			   TOP_lock_cmpxchg16,
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
			   TOP_punpckldq,
			   TOP_punpcklwd128,
			   TOP_punpcklbw128,
			   TOP_punpckldq128,
			   TOP_punpckhbw,
			   TOP_punpckhwd,
			   TOP_punpckhdq,
			   TOP_punpckhbw128,
			   TOP_punpckhwd128,
			   TOP_punpckhdq128,
			   TOP_punpcklqdq,
			   TOP_punpckhqdq,
			   TOP_UNDEFINED );

  /* avx move mask pd/ps */
  ISA_PRINT_TYPE vmovmsk = ISA_Print_Type_Create("vmovmsk", "%s %s,%s");
  Name();
  Operand(0);
  Result(0);
  Instruction_Print_Group( vmovmsk,
                           /* AVX instructions */
                           TOP_vmovmskpd,
                           TOP_vmovmskps,
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

  /* pshufb */
  ISA_PRINT_TYPE pshufb = ISA_Print_Type_Create("pshufb", "%s %s,%s");
  Name();
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( pshufb,
                           /* SSSE3 instructions */
                           TOP_pshuf128v8,
                           TOP_UNDEFINED );

  /* pshufb mem */
  ISA_PRINT_TYPE pshufb_mem = ISA_Print_Type_Create("pshufb_mem", "%s %s%s(%s),%s");
  Name();
  Segment();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( pshufb,
                           /* SSSE3 instructions */
                           TOP_pshufx128v8,
                           TOP_UNDEFINED );

  /* pshufb mem w/ scaled index w/ base */
  ISA_PRINT_TYPE pshufb_memindex = ISA_Print_Type_Create("pshufb_memindex", "%s %s%s(%s,%s,%s),%s");
  Name();
  Segment();
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Result(0);
  Instruction_Print_Group( pshufb,
                           /* SSSE3 instructions */
                           TOP_pshufxx128v8,
                           TOP_UNDEFINED );

  /* pshufb mem w/ scaled index w/o base */
  ISA_PRINT_TYPE pshufb_memindexx = ISA_Print_Type_Create("pshufb_memindexx", "%s %s%s(,%s,%s),%s");
  Name();
  Segment();
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Result(0);
  Instruction_Print_Group( pshufb,
                           /* SSSE3 instructions */
                           TOP_pshufxxx128v8,
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
			   TOP_psllwi,
			   TOP_pslld,
			   TOP_pslldi,
			   TOP_psllq,
			   TOP_psllqi,
			   TOP_psrlw,
			   TOP_psrlwi,
			   TOP_psrld,
			   TOP_psrldi,
			   TOP_psrlq,
			   TOP_psrlqi,
			   TOP_psraw,
			   TOP_psrawi,
			   TOP_psrad,
			   TOP_psradi,
                           TOP_extrq,
                           TOP_insertq,
			   TOP_psllw_mmx,
			   TOP_psllwi_mmx,
			   TOP_pslld_mmx,
			   TOP_pslldi_mmx,
			   TOP_psllq_mmx,
			   TOP_psllqi_mmx,
			   TOP_psrlw_mmx,
			   TOP_psrlwi_mmx,
			   TOP_psrld_mmx,
			   TOP_psrldi_mmx,
			   TOP_psrlq_mmx,
			   TOP_psrlqi_mmx,
			   TOP_psraw_mmx,
			   TOP_psrawi_mmx,
			   TOP_psrad_mmx,
			   TOP_psradi_mmx,
			   TOP_pand_mmx,
			   TOP_pandn_mmx,
			   TOP_por_mmx,
			   TOP_pxor_mmx,
			   TOP_UNDEFINED );

  /* non destructive fma xmm xmm xmm */
  ISA_PRINT_TYPE ndstvecreg =  ISA_Print_Type_Create("ndstvecreg", 
                                                     "%s %s, %s, %s, %s");
  Name();
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( ndstvecreg,
                           TOP_vfmaddss,
                           TOP_vfmaddsd,
                           TOP_vfnmaddss,
                           TOP_vfnmaddsd,
                           TOP_vfmaddps,
                           TOP_vfmaddpd,
                           TOP_vfmaddsubps,
                           TOP_vfmaddsubpd,
                           TOP_vfnmaddps,
                           TOP_vfnmaddpd,
                           TOP_vfmsubss,
                           TOP_vfmsubsd,
                           TOP_vfnmsubss,
                           TOP_vfnmsubsd,
                           TOP_vfmsubps,
                           TOP_vfmsubpd,
                           TOP_vfmsubaddps,
                           TOP_vfmsubaddpd,
                           TOP_vfnmsubps,
                           TOP_vfnmsubpd,
                           TOP_UNDEFINED);

  /* non destructive fma xmm xmm mem opnd */
  ISA_PRINT_TYPE ndstvecregx =  ISA_Print_Type_Create("ndstvecregx", 
						      "%s %s %s(%s), %s, %s, %s");
  Name();
  Segment();
  Operand(3);
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( ndstvecregx,
                           TOP_vfmaddxss,
                           TOP_vfmaddxsd,
                           TOP_vfnmaddxss,
                           TOP_vfnmaddxsd,
                           TOP_vfmaddxps,
                           TOP_vfmaddxpd,
                           TOP_vfmaddsubxps,
                           TOP_vfmaddsubxpd,
                           TOP_vfnmaddxps,
                           TOP_vfnmaddxpd,
                           TOP_vfmsubxss,
                           TOP_vfmsubxsd,
                           TOP_vfnmsubxss,
                           TOP_vfnmsubxsd,
                           TOP_vfmsubxps,
                           TOP_vfmsubxpd,
                           TOP_vfmsubaddxps,
                           TOP_vfmsubaddxpd,
                           TOP_vfnmsubxps,
                           TOP_vfnmsubxpd,
                           TOP_UNDEFINED);

  /* non destructive fma xmm xmm mem opnd w/ scaled index */
  ISA_PRINT_TYPE ndestvecregxx =  ISA_Print_Type_Create("ndestvecregxx", 
						        "%s %s %s(%s,%s,%s), %s, %s, %s");
  Name();
  Segment();
  Operand(5);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( ndestvecregxx,
                           TOP_vfmaddxxss,
                           TOP_vfmaddxxsd,
                           TOP_vfnmaddxxss,
                           TOP_vfnmaddxxsd,
                           TOP_vfmaddxxps,
                           TOP_vfmaddxxpd,
                           TOP_vfmaddsubxxps,
                           TOP_vfmaddsubxxpd,
                           TOP_vfnmaddxxps,
                           TOP_vfnmaddxxpd,
                           TOP_vfmsubxxss,
                           TOP_vfmsubxxsd,
                           TOP_vfnmsubxxss,
                           TOP_vfnmsubxxsd,
                           TOP_vfmsubxxps,
                           TOP_vfmsubxxpd,
                           TOP_vfmsubaddxxps,
                           TOP_vfmsubaddxxpd,
                           TOP_vfnmsubxxps,
                           TOP_vfnmsubxxpd,
                           TOP_UNDEFINED);

  /* One result / two operands in x86 style w/ mem operand w/o base */
  ISA_PRINT_TYPE ndestvecregxxx =  ISA_Print_Type_Create("ndestvecregxxx", 
						         "%s %s %s(,%s,%s), %s, %s, %s");
  Name();
  Segment();
  Operand(4);
  Operand(2);
  Operand(3);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( ndestvecregxxx,
                           TOP_vfmaddxxxss,
                           TOP_vfmaddxxxsd,
                           TOP_vfnmaddxxxss,
                           TOP_vfnmaddxxxsd,
                           TOP_vfmaddxxxps,
                           TOP_vfmaddxxxpd,
                           TOP_vfmaddsubxxxps,
                           TOP_vfmaddsubxxxpd,
                           TOP_vfnmaddxxxps,
                           TOP_vfnmaddxxxpd,
                           TOP_vfmsubxxxss,
                           TOP_vfmsubxxxsd,
                           TOP_vfnmsubxxxss,
                           TOP_vfnmsubxxxsd,
                           TOP_vfmsubxxxps,
                           TOP_vfmsubxxxpd,
                           TOP_vfmsubaddxxxps,
                           TOP_vfmsubaddxxxpd,
                           TOP_vfnmsubxxxps,
                           TOP_vfnmsubxxxpd,
                           TOP_UNDEFINED);

  /* non destructive fma xmm mem opnd xmm */
  ISA_PRINT_TYPE ndstvecregxr =  ISA_Print_Type_Create("ndstvecregxr", 
						       "%s %s %s, %s(%s), %s, %s");
  Name();
  Segment();
  Operand(3);
  Operand(2);
  Operand(1);
  Operand(0);
  Result(0);
  Instruction_Print_Group( ndstvecregxr,
                           TOP_vfmaddxrss,
                           TOP_vfmaddxrsd,
                           TOP_vfnmaddxrss,
                           TOP_vfnmaddxrsd,
                           TOP_vfmaddxrps,
                           TOP_vfmaddxrpd,
                           TOP_vfmaddsubxrps,
                           TOP_vfmaddsubxrpd,
                           TOP_vfnmaddxrps,
                           TOP_vfnmaddxrpd,
                           TOP_vfmsubxrss,
                           TOP_vfmsubxrsd,
                           TOP_vfnmsubxrss,
                           TOP_vfnmsubxrsd,
                           TOP_vfmsubxrps,
                           TOP_vfmsubxrpd,
                           TOP_vfmsubaddxrps,
                           TOP_vfmsubaddxrpd,
                           TOP_vfnmsubxrps,
                           TOP_vfnmsubxrpd,
                           TOP_UNDEFINED);

  /* non destructive fma xmm xmm mem opnd w/ scaled index */
  ISA_PRINT_TYPE ndestvecregxxr =  ISA_Print_Type_Create("ndestvecregxxr", 
						         "%s %s %s, %s(%s,%s,%s), %s, %s");
  Name();
  Segment();
  Operand(5);
  Operand(4);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(0);
  Result(0);
  Instruction_Print_Group( ndestvecregxxr,
                           TOP_vfmaddxxrss,
                           TOP_vfmaddxxrsd,
                           TOP_vfnmaddxxrss,
                           TOP_vfnmaddxxrsd,
                           TOP_vfmaddxxrps,
                           TOP_vfmaddxxrpd,
                           TOP_vfmaddsubxxrps,
                           TOP_vfmaddsubxxrpd,
                           TOP_vfnmaddxxrps,
                           TOP_vfnmaddxxrpd,
                           TOP_vfmsubxxrss,
                           TOP_vfmsubxxrsd,
                           TOP_vfnmsubxxrss,
                           TOP_vfnmsubxxrsd,
                           TOP_vfmsubxxrps,
                           TOP_vfmsubxxrpd,
                           TOP_vfmsubaddxxrps,
                           TOP_vfmsubaddxxrpd,
                           TOP_vfnmsubxxrps,
                           TOP_vfnmsubxxrpd,
                           TOP_UNDEFINED);

  /* One result / two operands in x86 style w/ mem operand w/o base */
  ISA_PRINT_TYPE ndestvecregxxxr =  ISA_Print_Type_Create("ndestvecregxxxr", 
						         "%s %s %s, %s(,%s,%s), %s, %s");
  Name();
  Segment();
  Operand(4);
  Operand(3);
  Operand(1);
  Operand(2);
  Operand(0);
  Result(0);
  Instruction_Print_Group( ndestvecregxxxr,
                           TOP_vfmaddxxxrss,
                           TOP_vfmaddxxxrsd,
                           TOP_vfnmaddxxxrss,
                           TOP_vfnmaddxxxrsd,
                           TOP_vfmaddxxxrps,
                           TOP_vfmaddxxxrpd,
                           TOP_vfmaddsubxxxrps,
                           TOP_vfmaddsubxxxrpd,
                           TOP_vfnmaddxxxrps,
                           TOP_vfnmaddxxxrpd,
                           TOP_vfmsubxxxrss,
                           TOP_vfmsubxxxrsd,
                           TOP_vfnmsubxxxrss,
                           TOP_vfnmsubxxxrsd,
                           TOP_vfmsubxxxrps,
                           TOP_vfmsubxxxrpd,
                           TOP_vfmsubaddxxxrps,
                           TOP_vfmsubaddxxxrpd,
                           TOP_vfnmsubxxxrps,
                           TOP_vfnmsubxxxrpd,
                           TOP_UNDEFINED);

  /* INTEL FMA instructions, reg operand */
  ISA_PRINT_TYPE ifma_ropopop = ISA_Print_Type_Create("ifma_ropopop", "%s %s,%s,%s");
  Name();
  Operand(2);
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ifma_ropopop,
                           TOP_xfmadd132pd,
                           TOP_xfmadd213pd,
                           TOP_xfmadd231pd,
                           TOP_xfmadd132ps,
                           TOP_xfmadd213ps,
                           TOP_xfmadd231ps,
                           TOP_xfmadd132sd,
                           TOP_xfmadd213sd,
                           TOP_xfmadd231sd,
                           TOP_xfmadd132ss,
                           TOP_xfmadd213ss,
                           TOP_xfmadd231ss,
                           TOP_xfmaddsub132pd,
                           TOP_xfmaddsub213pd,
                           TOP_xfmaddsub231pd,
                           TOP_xfmaddsub132ps,
                           TOP_xfmaddsub213ps,
                           TOP_xfmaddsub231ps,
                           TOP_xfmsubadd132pd,
                           TOP_xfmsubadd213pd,
                           TOP_xfmsubadd231pd,
                           TOP_xfmsubadd132ps,
                           TOP_xfmsubadd213ps,
                           TOP_xfmsubadd231ps,
                           TOP_xfmsub132pd,
                           TOP_xfmsub213pd,
                           TOP_xfmsub231pd,
                           TOP_xfmsub132ps,
                           TOP_xfmsub213ps,
                           TOP_xfmsub231ps,
                           TOP_xfmsub132sd,
                           TOP_xfmsub213sd,
                           TOP_xfmsub231sd,
                           TOP_xfmsub132ss,
                           TOP_xfmsub213ss,
                           TOP_xfmsub231ss,
                           TOP_xfnmadd132pd,
                           TOP_xfnmadd213pd,
                           TOP_xfnmadd231pd,
                           TOP_xfnmadd132ps,
                           TOP_xfnmadd213ps,
                           TOP_xfnmadd231ps,
                           TOP_xfnmadd132sd,
                           TOP_xfnmadd213sd,
                           TOP_xfnmadd231sd,
                           TOP_xfnmadd132ss,
                           TOP_xfnmadd213ss,
                           TOP_xfnmadd231ss,
                           TOP_xfnmsub132pd,
                           TOP_xfnmsub213pd,
                           TOP_xfnmsub231pd,
                           TOP_xfnmsub132ps,
                           TOP_xfnmsub213ps,
                           TOP_xfnmsub231ps,
                           TOP_xfnmsub132sd,
                           TOP_xfnmsub213sd,
                           TOP_xfnmsub231sd,
                           TOP_xfnmsub132ss,
                           TOP_xfnmsub213ss,
                           TOP_xfnmsub231ss,
                           TOP_UNDEFINED );
  
  ISA_PRINT_TYPE ifma_ropopmem = ISA_Print_Type_Create("ifma_ropopmem", "%s %s%s(%s),%s,%s");
  Name();
  Segment();
  Operand(3);
  Operand(2);
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ifma_ropopmem,
                           TOP_xfmadd132xpd,
                           TOP_xfmadd213xpd,
                           TOP_xfmadd231xpd,
                           TOP_xfmadd132xps,
                           TOP_xfmadd213xps,
                           TOP_xfmadd231xps,
                           TOP_xfmadd132xsd,
                           TOP_xfmadd213xsd,
                           TOP_xfmadd231xsd,
                           TOP_xfmadd132xss,
                           TOP_xfmadd213xss,
                           TOP_xfmadd231xss,
                           TOP_xfmaddsub132xpd,
                           TOP_xfmaddsub213xpd,
                           TOP_xfmaddsub231xpd,
                           TOP_xfmaddsub132xps,
                           TOP_xfmaddsub213xps,
                           TOP_xfmaddsub231xps,
                           TOP_xfmsubadd132xpd,
                           TOP_xfmsubadd213xpd,
                           TOP_xfmsubadd231xpd,
                           TOP_xfmsubadd132xps,
                           TOP_xfmsubadd213xps,
                           TOP_xfmsubadd231xps,
                           TOP_xfmsub132xpd,
                           TOP_xfmsub213xpd,
                           TOP_xfmsub231xpd,
                           TOP_xfmsub132xps,
                           TOP_xfmsub213xps,
                           TOP_xfmsub231xps,
                           TOP_xfmsub132xsd,
                           TOP_xfmsub213xsd,
                           TOP_xfmsub231xsd,
                           TOP_xfmsub132xss,
                           TOP_xfmsub213xss,
                           TOP_xfmsub231xss,
                           TOP_xfnmadd132xpd,
                           TOP_xfnmadd213xpd,
                           TOP_xfnmadd231xpd,
                           TOP_xfnmadd132xps,
                           TOP_xfnmadd213xps,
                           TOP_xfnmadd231xps,
                           TOP_xfnmadd132xsd,
                           TOP_xfnmadd213xsd,
                           TOP_xfnmadd231xsd,
                           TOP_xfnmadd132xss,
                           TOP_xfnmadd213xss,
                           TOP_xfnmadd231xss,
                           TOP_xfnmsub132xpd,
                           TOP_xfnmsub213xpd,
                           TOP_xfnmsub231xpd,
                           TOP_xfnmsub132xps,
                           TOP_xfnmsub213xps,
                           TOP_xfnmsub231xps,
                           TOP_xfnmsub132xsd,
                           TOP_xfnmsub213xsd,
                           TOP_xfnmsub231xsd,
                           TOP_xfnmsub132xss,
                           TOP_xfnmsub213xss,
                           TOP_xfnmsub231xss,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE ifma_ropopmemindex = ISA_Print_Type_Create("ifma_ropopmemindex", "%s %s%s(%s,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(5);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ifma_ropopmemindex,
                           TOP_xfmadd132xxpd,
                           TOP_xfmadd213xxpd,
                           TOP_xfmadd231xxpd,
                           TOP_xfmadd132xxps,
                           TOP_xfmadd213xxps,
                           TOP_xfmadd231xxps,
                           TOP_xfmadd132xxsd,
                           TOP_xfmadd213xxsd,
                           TOP_xfmadd231xxsd,
                           TOP_xfmadd132xxss,
                           TOP_xfmadd213xxss,
                           TOP_xfmadd231xxss,
                           TOP_xfmaddsub132xxpd,
                           TOP_xfmaddsub213xxpd,
                           TOP_xfmaddsub231xxpd,
                           TOP_xfmaddsub132xxps,
                           TOP_xfmaddsub213xxps,
                           TOP_xfmaddsub231xxps,
                           TOP_xfmsubadd132xxpd,
                           TOP_xfmsubadd213xxpd,
                           TOP_xfmsubadd231xxpd,
                           TOP_xfmsubadd132xxps,
                           TOP_xfmsubadd213xxps,
                           TOP_xfmsubadd231xxps,
                           TOP_xfmsub132xxpd,
                           TOP_xfmsub213xxpd,
                           TOP_xfmsub231xxpd,
                           TOP_xfmsub132xxps,
                           TOP_xfmsub213xxps,
                           TOP_xfmsub231xxps,
                           TOP_xfmsub132xxsd,
                           TOP_xfmsub213xxsd,
                           TOP_xfmsub231xxsd,
                           TOP_xfmsub132xxss,
                           TOP_xfmsub213xxss,
                           TOP_xfmsub231xxss,
                           TOP_xfnmadd132xxpd,
                           TOP_xfnmadd213xxpd,
                           TOP_xfnmadd231xxpd,
                           TOP_xfnmadd132xxps,
                           TOP_xfnmadd213xxps,
                           TOP_xfnmadd231xxps,
                           TOP_xfnmadd132xxsd,
                           TOP_xfnmadd213xxsd,
                           TOP_xfnmadd231xxsd,
                           TOP_xfnmadd132xxss,
                           TOP_xfnmadd213xxss,
                           TOP_xfnmadd231xxss,
                           TOP_xfnmsub132xxpd,
                           TOP_xfnmsub213xxpd,
                           TOP_xfnmsub231xxpd,
                           TOP_xfnmsub132xxps,
                           TOP_xfnmsub213xxps,
                           TOP_xfnmsub231xxps,
                           TOP_xfnmsub132xxsd,
                           TOP_xfnmsub213xxsd,
                           TOP_xfnmsub231xxsd,
                           TOP_xfnmsub132xxss,
                           TOP_xfnmsub213xxss,
                           TOP_xfnmsub231xxss,
                           TOP_UNDEFINED );

  ISA_PRINT_TYPE ifma_ropopmemindexx = ISA_Print_Type_Create("ifma_ropopmemindexx", "%s %s%s(,%s,%s),%s,%s");
  Name();
  Segment();
  Operand(4);
  Operand(2);
  Operand(3);
  Operand(1);
  Result(0);
  Operand(0);
  Instruction_Print_Group( ifma_ropopmemindexx,
                           TOP_xfmadd132xxxpd,
                           TOP_xfmadd213xxxpd,
                           TOP_xfmadd231xxxpd,
                           TOP_xfmadd132xxxps,
                           TOP_xfmadd213xxxps,
                           TOP_xfmadd231xxxps,
                           TOP_xfmadd132xxxsd,
                           TOP_xfmadd213xxxsd,
                           TOP_xfmadd231xxxsd,
                           TOP_xfmadd132xxxss,
                           TOP_xfmadd213xxxss,
                           TOP_xfmadd231xxxss,
                           TOP_xfmaddsub132xxxpd,
                           TOP_xfmaddsub213xxxpd,
                           TOP_xfmaddsub231xxxpd,
                           TOP_xfmaddsub132xxxps,
                           TOP_xfmaddsub213xxxps,
                           TOP_xfmaddsub231xxxps,
                           TOP_xfmsubadd132xxxpd,
                           TOP_xfmsubadd213xxxpd,
                           TOP_xfmsubadd231xxxpd,
                           TOP_xfmsubadd132xxxps,
                           TOP_xfmsubadd213xxxps,
                           TOP_xfmsubadd231xxxps,
                           TOP_xfmsub132xxxpd,
                           TOP_xfmsub213xxxpd,
                           TOP_xfmsub231xxxpd,
                           TOP_xfmsub132xxxps,
                           TOP_xfmsub213xxxps,
                           TOP_xfmsub231xxxps,
                           TOP_xfmsub132xxxsd,
                           TOP_xfmsub213xxxsd,
                           TOP_xfmsub231xxxsd,
                           TOP_xfmsub132xxxss,
                           TOP_xfmsub213xxxss,
                           TOP_xfmsub231xxxss,
                           TOP_xfnmadd132xxxpd,
                           TOP_xfnmadd213xxxpd,
                           TOP_xfnmadd231xxxpd,
                           TOP_xfnmadd132xxxps,
                           TOP_xfnmadd213xxxps,
                           TOP_xfnmadd231xxxps,
                           TOP_xfnmadd132xxxsd,
                           TOP_xfnmadd213xxxsd,
                           TOP_xfnmadd231xxxsd,
                           TOP_xfnmadd132xxxss,
                           TOP_xfnmadd213xxxss,
                           TOP_xfnmadd231xxxss,
                           TOP_xfnmsub132xxxpd,
                           TOP_xfnmsub213xxxpd,
                           TOP_xfnmsub231xxxpd,
                           TOP_xfnmsub132xxxps,
                           TOP_xfnmsub213xxxps,
                           TOP_xfnmsub231xxxps,
                           TOP_xfnmsub132xxxsd,
                           TOP_xfnmsub213xxxsd,
                           TOP_xfnmsub231xxxsd,
                           TOP_xfnmsub132xxxss,
                           TOP_xfnmsub213xxxss,
                           TOP_xfnmsub231xxxss,
                           TOP_UNDEFINED );

  /* {st,ld}mxcsr */
  ISA_PRINT_TYPE stmxcsr =  ISA_Print_Type_Create("stmxcsr", "%s %s%s(%s)");
  Name();
  Segment();
  Operand(1);
  Operand(0);
  Instruction_Print_Group( stmxcsr,
			   TOP_stmxcsr,
			   TOP_ldmxcsr,
                           /* AVX instructions */
                           TOP_vldmxcsr,
                           TOP_vstmxcsr,
			   TOP_UNDEFINED );

  /* GNU compatible TLS global dynamic 64 */
  ISA_PRINT_TYPE tls_gd_64 = ISA_Print_Type_Create("tls_gd_64", "# %s\\n\\t.byte \\t0x66\\n\\tleaq %s(%s), %%rdi\\n\\t.word \\t0x6666\\n\\trex64\\n\\tcall %s");
  Name();
  Operand(0); /* ST# */
  Operand(1); /* rip */
  Operand(2); /* __tls_get_addr */
  Instruction_Print_Group( tls_gd_64,
		           TOP_tls_global_dynamic_64,
			   TOP_UNDEFINED );

  /* GNU compatible TLS global dynamic 32 */
  ISA_PRINT_TYPE tls_gd_32 = ISA_Print_Type_Create("tls_gd_32", "# %s\\n\\tleal %s(,%s,1), %%eax\\n\\tcall %s");
  Name();
  Operand(0); /* ST# */
  Operand(1); /* ebx */
  Operand(2); /* __tls_get_addr */
  Instruction_Print_Group( tls_gd_32,
		           TOP_tls_global_dynamic_32,
			   TOP_UNDEFINED );

  /* GNU compatible TLS local dynamic 64 */
  ISA_PRINT_TYPE tls_ld_64 = ISA_Print_Type_Create("tls_ld_64", "# %s\\n\\tleaq %s(%s), %%rdi\\n\\tcall %s");
  Name();
  Operand(0); /* ST# */
  Operand(1); /* rip */
  Operand(2); /* __tls_get_addr */
  Instruction_Print_Group( tls_ld_64,
		           TOP_tls_local_dynamic_64,
			   TOP_UNDEFINED );

  /* GNU compatible TLS local dynamic 32 */
  ISA_PRINT_TYPE tls_ld_32 = ISA_Print_Type_Create("tls_ld_32", "# %s\\n\\tleal %s(%s), %%eax\\n\\tcall %s");
  Name();
  Operand(0); /* ST# */
  Operand(1); /* ebx */
  Operand(2); /* __tls_get_addr */
  Instruction_Print_Group( tls_ld_32,
		           TOP_tls_local_dynamic_32,
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
                           TOP_vzeroupper,
                           /* SSE3 instructions */
                           TOP_monitor,
                           TOP_mwait,
                           /* AVX instructions */
                           TOP_vzeroall,
			   TOP_UNDEFINED );

  ISA_Print_End();
  return 0;
}
