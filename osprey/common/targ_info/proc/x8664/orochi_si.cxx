/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

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


//  Opteron (Orochi) processor scheduling information
///////////////////////////////////////////////////////////////////
//  
//  Description:
//
//  Generate a scheduling description of an AMD Orochi processor
//  via the si_gen interface.
//
///////////////////////////////////////////////////////////////////


//  $Revision: 1.146 $
//  $Date: 05/11/10 18:45:11-08:00 $
//  $Author: tkong@hyalite.keyresearch $


#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"


void Generate_Orochi (void)
{
  Machine( "orochi", ISA_SUBSET_x86_64 );

  const RESOURCE res_issue  = RESOURCE_Create( "issue",  3 );
  const RESOURCE res_alu    = RESOURCE_Create( "alu",    3 );
  const RESOURCE res_agu    = RESOURCE_Create( "agu",    3 );
  const RESOURCE res_fmul   = RESOURCE_Create( "fmul",   1 );
  const RESOURCE res_fadd   = RESOURCE_Create( "fadd",   1 );
  const RESOURCE res_fstore = RESOURCE_Create( "fstore", 1 );
  const RESOURCE res_loadstore	= RESOURCE_Create( "loadstore", 2 );

  Instruction_Group( "orochi simple alu",
		     TOP_ijmpx,
		     TOP_icallx,
		     TOP_ijmpxx,
		     TOP_icallxx,
		     TOP_ijmpxxx,
		     TOP_icallxxx,
		     TOP_add8,
		     TOP_add16,
		     TOP_add32,
		     TOP_adc32,
		     TOP_add64,
		     TOP_addi8,
		     TOP_addi16,
		     TOP_addi32,
		     TOP_adci32,
		     TOP_addi64,
		     TOP_bswap32,
		     TOP_bswap64,
		     TOP_inc8,
		     TOP_inc16,
		     TOP_inc32,
		     TOP_inc64,
		     TOP_dec8,
		     TOP_dec16,
		     TOP_dec32,
		     TOP_dec64,
		     TOP_and8,
		     TOP_and16,
		     TOP_and32,
		     TOP_and64,
		     TOP_andi8,
		     TOP_andi16,
		     TOP_andi32,
		     TOP_andi64,
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
		     TOP_cmp8,
		     TOP_cmp16,
		     TOP_cmp32,
		     TOP_cmp64,
		     TOP_cmpi8,
		     TOP_cmpi16,
		     TOP_cmpi32,
		     TOP_cmpi64,
		     TOP_neg8,
		     TOP_neg16,
		     TOP_neg32,
		     TOP_neg64,
		     TOP_not8,
		     TOP_not16,
		     TOP_not32,
		     TOP_not64,
		     TOP_or8,
		     TOP_or16,
		     TOP_or32,
		     TOP_or64,
		     TOP_ori8,
		     TOP_ori16,
		     TOP_ori32,
		     TOP_ori64,
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
		     TOP_shldi32,
		     TOP_shrdi32,
		     TOP_shli64,
		     TOP_shr32,
		     TOP_shr64,
		     TOP_shri32,
		     TOP_shri64,
		     TOP_sub8,
		     TOP_sub16,
		     TOP_sub32,
		     TOP_sbb32,
		     TOP_sub64,
		     TOP_subi8,
		     TOP_subi16,
		     TOP_subi32,
		     TOP_sbbi32,
		     TOP_subi64,
		     TOP_xor8,
		     TOP_xor16,
		     TOP_xor32,
		     TOP_xor64,
		     TOP_xori8,
		     TOP_xori16,
		     TOP_xori32,
		     TOP_xori64,
		     TOP_zero32,
		     TOP_zero64,
		     TOP_test8,
		     TOP_test16,
		     TOP_test32,
		     TOP_test64,
		     TOP_testi8,
		     TOP_testi16,
		     TOP_testi32,
		     TOP_testi64,
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
		     TOP_ldc32,
		     TOP_ldc64,
		     TOP_movabsq,
		     TOP_cltd,
		     TOP_cqto,
		     TOP_bsf32, // guess!
		     TOP_bsf64, // guess!
		     TOP_bsr32, // guess!
		     TOP_bsr64, // guess!
		     TOP_mov64_m,
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
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "simple alu w/ mem operand",
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
		     TOP_cmpxxx8,
		     TOP_cmpxxx16,
		     TOP_cmpxxx32,
		     TOP_cmpxxx64,
		     TOP_cmpxxxr8,
		     TOP_cmpxxxr16,
		     TOP_cmpxxxr32,
		     TOP_cmpxxxr64,
		     TOP_cmpxxxi8,
		     TOP_cmpxxxi16,
		     TOP_cmpxxxi32,
		     TOP_cmpxxxi64,
		     TOP_testxxx8,
		     TOP_testxxx16,
		     TOP_testxxx32,
		     TOP_testxxx64,
		     TOP_andx32,
		     TOP_andxx32,
		     TOP_orx32,
		     TOP_orxx32,
		     TOP_orx64,
		     TOP_orxx64,
		     TOP_xorx32,
		     TOP_xorxx32,
		     TOP_xorx64,
		     TOP_xorxx64,
		     TOP_addx32,
		     TOP_addx64,
		     TOP_addxx32,
		     TOP_addxx64,
		     TOP_subx32,
		     TOP_subx64,
		     TOP_subxx32,
		     TOP_subxx64,
		     TOP_testx8,
		     TOP_testxx8,
		     TOP_testx16,
		     TOP_testxx16,
		     TOP_testx32,
		     TOP_testxx32,
		     TOP_testx64,
		     TOP_testxx64,
		     TOP_cmpx8,
		     TOP_cmpxx8,
		     TOP_cmpxr8,
		     TOP_cmpxxr8,
		     TOP_cmpxi8,
		     TOP_cmpxxi8,
		     TOP_cmpx16,
		     TOP_cmpxx16,
		     TOP_cmpxr16,
		     TOP_cmpxxr16,
		     TOP_cmpxi16,
		     TOP_cmpxxi16,
		     TOP_cmpx32,
		     TOP_cmpxx32,
		     TOP_cmpxr32,
		     TOP_cmpxxr32,
		     TOP_cmpxi32,
		     TOP_cmpxxi32,
		     TOP_cmpx64,
		     TOP_cmpxx64,
		     TOP_cmpxr64,
		     TOP_cmpxxr64,
		     TOP_cmpxi64,
		     TOP_cmpxxi64,
		     TOP_xorx8,
		     TOP_xorx16,
		     TOP_xorxx8,
		     TOP_xorxx16,
		     TOP_orx8,
		     TOP_orx16,
		     TOP_orxx8,
		     TOP_orxx16,
		     TOP_andx8,
		     TOP_andx16,
		     TOP_andx64,
		     TOP_andxx8,
		     TOP_andxx16,
		     TOP_andxx64,
		     TOP_addxr8,
		     TOP_addxxr8,
		     TOP_addxxxr8,
		     TOP_addxr8_n32,
		     TOP_addxr16,
		     TOP_addxxr16,
		     TOP_addxxxr16,
		     TOP_addxr16_n32,
		     TOP_addxr32,
		     TOP_addxxr32,
		     TOP_addxxxr32,
		     TOP_addxr32_n32,
		     TOP_addxr64,
		     TOP_addxxr64,
		     TOP_addxxxr64,
		     TOP_addxr64_off,
		     TOP_addixr8,
		     TOP_addixxr8,
		     TOP_addixxxr8,
		     TOP_addixr8_n32,
		     TOP_addixr16,
		     TOP_addixxr16,
		     TOP_addixxxr16,
		     TOP_addixr16_n32,
		     TOP_addixr32,
		     TOP_addixxr32,
		     TOP_addixxxr32,
		     TOP_addixr32_n32,
		     TOP_addixr64,
		     TOP_addixxr64,
		     TOP_addixxxr64,
		     TOP_addixr64_off,
		     TOP_andxr8,
		     TOP_andxxr8,
		     TOP_andxxxr8,
		     TOP_andxr8_n32,
		     TOP_andxr16,
		     TOP_andxxr16,
		     TOP_andxxxr16,
		     TOP_andxr16_n32,
		     TOP_andxr32,
		     TOP_andxxr32,
		     TOP_andxxxr32,
		     TOP_andxr32_n32,
		     TOP_andxr64,
		     TOP_andxxr64,
		     TOP_andxxxr64,
		     TOP_andxr64_off,
		     TOP_andixr8,
		     TOP_andixxr8,
		     TOP_andixxxr8,
		     TOP_andixr8_n32,
		     TOP_andixr16,
		     TOP_andixxr16,
		     TOP_andixxxr16,
		     TOP_andixr16_n32,
		     TOP_andixr32,
		     TOP_andixxr32,
		     TOP_andixxxr32,
		     TOP_andixr32_n32,
		     TOP_andixr64,
		     TOP_andixxr64,
		     TOP_andixxxr64,
		     TOP_andixr64_off,
		     TOP_orxr8,
		     TOP_orxxr8,
		     TOP_orxxxr8,
		     TOP_orxr8_n32,
		     TOP_orxr16,
		     TOP_orxxr16,
		     TOP_orxxxr16,
		     TOP_orxr16_n32,
		     TOP_orxr32,
		     TOP_orxxr32,
		     TOP_orxxxr32,
		     TOP_orxr32_n32,
		     TOP_orxr64,
		     TOP_orxxr64,
		     TOP_orxxxr64,
		     TOP_orxr64_off,
		     TOP_orixr8,
		     TOP_orixxr8,
		     TOP_orixxxr8,
		     TOP_orixr8_n32,
		     TOP_orixr16,
		     TOP_orixxr16,
		     TOP_orixxxr16,
		     TOP_orixr16_n32,
		     TOP_orixr32,
		     TOP_orixxr32,
		     TOP_orixxxr32,
		     TOP_orixr32_n32,
		     TOP_orixr64,
		     TOP_orixxr64,
		     TOP_orixxxr64,
		     TOP_orixr64_off,
		     TOP_subxr8,
		     TOP_subxxr8,
		     TOP_subxxxr8,
		     TOP_subxr8_n32,
		     TOP_subxr16,
		     TOP_subxxr16,
		     TOP_subxxxr16,
		     TOP_subxr16_n32,
		     TOP_subxr32,
		     TOP_subxxr32,
		     TOP_subxxxr32,
		     TOP_subxr32_n32,
		     TOP_subxr64,
		     TOP_subxxr64,
		     TOP_subxxxr64,
		     TOP_subxr64_off,
		     TOP_subixr8,
		     TOP_subixxr8,
		     TOP_subixxxr8,
		     TOP_subixr8_n32,
		     TOP_subixr16,
		     TOP_subixxr16,
		     TOP_subixxxr16,
		     TOP_subixr16_n32,
		     TOP_subixr32,
		     TOP_subixxr32,
		     TOP_subixxxr32,
		     TOP_subixr32_n32,
		     TOP_subixr64,
		     TOP_subixxr64,
		     TOP_subixxxr64,
		     TOP_subixr64_off,
		     TOP_xorxr8,
		     TOP_xorxxr8,
		     TOP_xorxxxr8,
		     TOP_xorxr8_n32,
		     TOP_xorxr16,
		     TOP_xorxxr16,
		     TOP_xorxxxr16,
		     TOP_xorxr16_n32,
		     TOP_xorxr32,
		     TOP_xorxxr32,
		     TOP_xorxxxr32,
		     TOP_xorxr32_n32,
		     TOP_xorxr64,
		     TOP_xorxxr64,
		     TOP_xorxxxr64,
		     TOP_xorxr64_off,
		     TOP_xorixr8,
		     TOP_xorixxr8,
		     TOP_xorixxxr8,
		     TOP_xorixr8_n32,
		     TOP_xorixr16,
		     TOP_xorixxr16,
		     TOP_xorixxxr16,
		     TOP_xorixr16_n32,
		     TOP_xorixr32,
		     TOP_xorixxr32,
		     TOP_xorixxxr32,
		     TOP_xorixr32_n32,
		     TOP_xorixr64,
		     TOP_xorixxr64,
		     TOP_xorixxxr64,
		     TOP_xorixr64_off,
		     TOP_negxr8,
		     TOP_negxxr8,
		     TOP_negxxxr8,
		     TOP_negxr8_n32,
		     TOP_negxr16,
		     TOP_negxxr16,
		     TOP_negxxxr16,
		     TOP_negxr16_n32,
		     TOP_negxr32,
		     TOP_negxxr32,
		     TOP_negxxxr32,
		     TOP_negxr32_n32,
		     TOP_negxr64,
		     TOP_negxxr64,
		     TOP_negxxxr64,
		     TOP_negxr64_off,
		     TOP_notxr8,
		     TOP_notxxr8,
		     TOP_notxxxr8,
		     TOP_notxr8_n32,
		     TOP_notxr16,
		     TOP_notxxr16,
		     TOP_notxxxr16,
		     TOP_notxr16_n32,
		     TOP_notxr32,
		     TOP_notxxr32,
		     TOP_notxxxr32,
		     TOP_notxr32_n32,
		     TOP_notxr64,
		     TOP_notxxr64,
		     TOP_notxxxr64,
		     TOP_notxr64_off,
		     TOP_incxr8,
		     TOP_incxxr8,
		     TOP_incxxxr8,
		     TOP_incxr8_n32,
		     TOP_incxr16,
		     TOP_incxxr16,
		     TOP_incxxxr16,
		     TOP_incxr16_n32,
		     TOP_incxr32,
		     TOP_incxxr32,
		     TOP_incxxxr32,
		     TOP_incxr32_n32,
		     TOP_incxr64,
		     TOP_incxxr64,
		     TOP_incxxxr64,
		     TOP_incxr64_off,
		     TOP_decxr8,
		     TOP_decxxr8,
		     TOP_decxxxr8,
		     TOP_decxr8_n32,
		     TOP_decxr16,
		     TOP_decxxr16,
		     TOP_decxxxr16,
		     TOP_decxr16_n32,
		     TOP_decxr32,
		     TOP_decxxr32,
		     TOP_decxxxr32,
		     TOP_decxr32_n32,
		     TOP_decxr64,
		     TOP_decxxr64,
		     TOP_decxxxr64,
		     TOP_decxr64_off,
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
		     TOP_lock_xchg8,
		     TOP_lock_xchg16,
		     TOP_lock_xchg32,
		     TOP_lock_xchg64,
		     TOP_lock_cmpxchg8,
		     TOP_lock_cmpxchg16,
		     TOP_lock_cmpxchg32,
		     TOP_lock_cmpxchg64,
		     TOP_pextrw,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "packed insert",
                     TOP_pinsrw,
                     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(14);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "lea",
		     TOP_lea32,
		     TOP_lea64,
		     TOP_leaxx32,
		     TOP_leaxx64,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_agu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "lea",
		     TOP_leax32,
		     TOP_leax64,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_agu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "push/pop",
		     TOP_pushl,
		     TOP_pushq,
		     TOP_popl,
		     TOP_popq,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "ret near",
		     TOP_ret,
		     TOP_reti,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "fcmov",
		     TOP_fcmovb,
		     TOP_fcmovbe,
		     TOP_fcmovnb,
		     TOP_fcmovnbe,
		     TOP_fcmove,
		     TOP_fcmovne,
		     TOP_fcmovu,
		     TOP_fcmovnu,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "call",
		     TOP_call,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "dynamic tls call",
		     TOP_tls_global_dynamic_64,
		     TOP_tls_global_dynamic_32,
		     TOP_tls_local_dynamic_64,
		     TOP_tls_local_dynamic_32,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "indirect call/jump",
		     TOP_icall,
		     TOP_ijmp,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "imult32",
		     TOP_mul32,
		     TOP_imul32,
		     TOP_imulx32,
		     TOP_imuli32,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "imult64",
		     TOP_mulx64,
		     TOP_imul64,
		     TOP_imuli64,
		     TOP_imulx64,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("leave",
		    TOP_leave,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("enter",
		    TOP_enter,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(19);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("div32",
		    TOP_div32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(20);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("div64",
		    TOP_div64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(20);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("idiv32",
		    TOP_idiv32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(24);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("idiv64",
		    TOP_idiv64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(24);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("load8_16/32/64",
		    TOP_ld8_abs,
		    TOP_ld16_abs,
		    TOP_ld8_32_n32,
		    TOP_ldu8_32_n32,
		    TOP_ld16_32_n32,
		    TOP_ldu16_32_n32,
		    TOP_ld8_32,
		    TOP_ldx8_32,
		    TOP_ldxx8_32,
		    TOP_ldu8_32,
		    TOP_ldxu8_32,
		    TOP_ldxxu8_32,
		    TOP_ld16_32,
		    TOP_ldx16_32,
		    TOP_ldxx16_32,
		    TOP_ldu16_32,
		    TOP_ldxu16_32,
		    TOP_ldxxu16_32,
		    TOP_ld8_64,
		    TOP_ldx8_64,
		    TOP_ldxx8_64,
		    TOP_ld8_64_off,
		    TOP_ldu8_64,
		    TOP_ldxu8_64,
		    TOP_ldxxu8_64,
		    TOP_ldu8_64_off,
		    TOP_ld16_64,
		    TOP_ldx16_64,
		    TOP_ldxx16_64,
		    TOP_ld16_64_off,
		    TOP_ldu16_64,
		    TOP_ldxu16_64,
		    TOP_ldxxu16_64,
		    TOP_ldu16_64_off,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_agu, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(5);

  Instruction_Group("load32/64",
		    TOP_ld64,
		    TOP_ld32,
		    TOP_ld32_n32,
		    TOP_ld32_64_off,
		    TOP_ld64_off,
		    TOP_ldx64,
		    TOP_ldxx64,
		    TOP_ldx32,
		    TOP_ldxx32,
		    TOP_ld32_64,
		    TOP_ldx32_64,
		    TOP_ldxx32_64,
		    TOP_ld32_abs,
		    TOP_ld64_abs,
		    TOP_ld64_2m,
		    TOP_ld64_2m_n32,
		    TOP_ld32_gs_seg_off,
		    TOP_ld64_fs_seg_off,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_agu, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group("store",
		    TOP_store8_n32,
		    TOP_store16_n32,
		    TOP_store32_n32,
		    TOP_store64_off,
		    TOP_storei8_n32,
		    TOP_storei16_n32,
		    TOP_storei32_n32,
		    TOP_storei64_off,
		    TOP_store8,
		    TOP_storex8,
		    TOP_storexx8,
		    TOP_storei8,
		    TOP_storeix8,
		    TOP_storeixx8,
		    TOP_store16,
		    TOP_storex16,
		    TOP_storexx16,
		    TOP_storei16,
		    TOP_storeix16,
		    TOP_storeixx16,
		    TOP_store32,
		    TOP_storex32,
		    TOP_storexx32,
		    TOP_storei32,
		    TOP_storeix32,
		    TOP_storeixx32,
		    TOP_store64,
		    TOP_storex64,
		    TOP_storexx64,
		    TOP_storei64,
		    TOP_storeix64,
		    TOP_storeixx64,
		    TOP_storenti32,
		    TOP_storentix32,
		    TOP_storentixx32,
		    TOP_storenti64,
		    TOP_storentix64,
		    TOP_storentixx64,
		    TOP_store8_abs,
		    TOP_store16_abs,
		    TOP_store32_abs,
		    TOP_store64_abs,
		    TOP_store64_fm,
		    TOP_store64_fm_n32,
		    TOP_storent64_fm,
		    TOP_maskmovq,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_agu, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(4);

  Instruction_Group("branch",
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
		    TOP_jmp,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_agu, 0);

  Instruction_Group( "sqrtss",
		     TOP_sqrtss,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(29);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group( "sqrtsd",
		     TOP_sqrtsd,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(38);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  // movapd/movaps are really zero cost (reg-reg), EBO handles it
  Instruction_Group( "genfp-mov 1",
		     TOP_movapd,
		     TOP_movaps,
                     TOP_vmovapd,
                     TOP_vmovaps,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd,  0);

  Instruction_Group( "float-mov 1",
		     TOP_movsd,
		     TOP_movss,
		     TOP_movdq,
		     TOP_movhlps,
		     TOP_movlhps,
		     TOP_cmpsd,
		     TOP_cmpss,
		     TOP_fabs,
		     TOP_fchs,
		     TOP_fmov,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd,  0);

  Instruction_Group( "float-load 1",
		     TOP_ldss_n32,
		     TOP_ldsd_n32,
		     TOP_ldss,
		     TOP_ldsd,
		     TOP_ldssx,
		     TOP_ldssxx,
		     TOP_ldsdx,
		     TOP_ldsdxx,
		     TOP_lddqa,
		     TOP_lddqa_n32,
		     TOP_lddqax,
		     TOP_lddqaxx,
                     TOP_vldss_n32,
                     TOP_vldsd_n32,
                     TOP_vldss,
                     TOP_vldsd,
                     TOP_vldssx,
                     TOP_vldssxx,
                     TOP_vldsdx,
                     TOP_vldsdxx,
                     TOP_vlddqa,
                     TOP_vlddqa_n32,
                     TOP_vlddqax,
                     TOP_vlddqaxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group( "float-load 2",
		     TOP_fld,
		     TOP_flds,
		     TOP_flds_n32,
		     TOP_fldl,
		     TOP_fldl_n32,
		     TOP_fldt,
		     TOP_fldt_n32,
		     TOP_fldcw,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group( "float-load vector",
		     TOP_lddqu,
		     TOP_lddqu_n32,
		     TOP_lddqux,
		     TOP_lddquxx,
		     TOP_ldupd,
                     TOP_ldupdx,
                     TOP_ldupdxx, 
		     TOP_ldupd_n32,
		     TOP_ldups,
		     TOP_ldupsx,
		     TOP_ldupsxx,
		     TOP_ldups_n32,
		     TOP_ldmxcsr,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group( "float-load vector II",
		     TOP_ldapd,
		     TOP_ldapd_n32,
		     TOP_ldapdx,
		     TOP_ldapdxx,
		     TOP_ldaps,
		     TOP_ldaps_n32,
		     TOP_ldapsx,
		     TOP_ldapsxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group( "float-load vector low-high packed-single",
		     TOP_ldlps,
		     TOP_ldlps_n32,
		     TOP_ldlpsx,
		     TOP_ldlpsxx,
		     TOP_ldhps,
		     TOP_ldhps_n32,
		     TOP_ldhpsx,
		     TOP_ldhpsxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group( "float-store vector low-high packed-single",
		     TOP_stlps,
		     TOP_stlps_n32,
		     TOP_stlpsx,
		     TOP_stlpsxx,
		     TOP_sthps,
		     TOP_sthps_n32,
		     TOP_sthpsx,
		     TOP_sthpsxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(4);

  Instruction_Group( "float-load vector low-high packed-double",
		     TOP_ldlpd,
		     TOP_ldlpd_n32,
		     TOP_ldlpdx,
		     TOP_ldlpdxx,
		     TOP_ldhpd,
		     TOP_ldhpd_n32,
		     TOP_ldhpdx,
		     TOP_ldhpdxx,
		     TOP_ld64_2sse,
		     TOP_ld64_2sse_n32,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group( "float-store vector low-high packed-double",
		     TOP_stlpd,
		     TOP_stlpd_n32,
		     TOP_stlpdx,
		     TOP_stlpdxx,
		     TOP_sthpd,
		     TOP_sthpd_n32,
		     TOP_sthpdx,
		     TOP_sthpdxx,
		     TOP_storelpd,
		     TOP_store64_fsse,
		     TOP_store64_fsse_n32,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(4);

  Instruction_Group( "float-store",
		     TOP_stss_n32,
		     TOP_stsd_n32,
		     TOP_stss,
		     TOP_stsd,
		     TOP_stntss,
		     TOP_stntsd,
		     TOP_stssx,
		     TOP_stssxx,
		     TOP_stntssx,
		     TOP_stntssxx,
		     TOP_stsdx,
		     TOP_stsdxx,
		     TOP_stntsdx,
		     TOP_stntsdxx,
		     TOP_stdqa,
		     TOP_stdqa_n32,
		     TOP_stdqax,
		     TOP_stdqaxx,
		     TOP_stntpd,
		     TOP_stntpdx,
		     TOP_stntpdxx,
		     TOP_stntps,
		     TOP_stntpsx,
		     TOP_stntpsxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(4);

  Instruction_Group( "x87 float-store",
		     TOP_fstp,
		     TOP_fst,
		     TOP_fstps,
		     TOP_fstps_n32,
		     TOP_fstpl,
		     TOP_fstpl_n32,
		     TOP_fstpt,
		     TOP_fstpt_n32,
		     TOP_fsts,
		     TOP_fsts_n32,
		     TOP_fstl,
		     TOP_fstl_n32,
		     TOP_fnstcw,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(2);

  Instruction_Group( "float-store vector",
		     TOP_stdqu,
		     TOP_stdqu_n32,
		     TOP_stdqux,
		     TOP_stdquxx,
		     TOP_stups,
		     TOP_stupsx,
		     TOP_stupsxx,
		     TOP_stups_n32,
		     TOP_stupd,
		     TOP_stupdx,
		     TOP_stupdxx,
		     TOP_stupd_n32,
		     TOP_maskmovdqu,
		     TOP_stmxcsr,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(4);

  Instruction_Group( "float-store vector II",
		     TOP_staps,
		     TOP_staps_n32,
		     TOP_stapsx,
		     TOP_stapsxx,
		     TOP_stapd,
		     TOP_stapd_n32,
		     TOP_stapdx,
		     TOP_stapdxx,
		     TOP_storenti128,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(4);

  Instruction_Group( "vector cvt I",
		     TOP_cvtdq2ps,
		     TOP_vcvtdq2ps,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "vector cvt II",
		     TOP_cvtps2pd,
		     TOP_cvtdq2pd,
		     TOP_vcvtps2pd,
		     TOP_vcvtdq2pd,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "vector cvt III",
		     TOP_cvtpd2ps,
		     TOP_cvtpd2dq,
		     TOP_cvttpd2dq,
		     TOP_vcvtpd2ps,
		     TOP_vcvtpd2psy,
		     TOP_vcvtpd2dq,
		     TOP_vcvtpd2dqy,
		     TOP_vcvttpd2dq,
		     TOP_vcvttpd2dqy,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "vector cvt IV",
		     TOP_cvtps2dq,
		     TOP_cvttps2dq,
		     TOP_cvtdq2ps_x,
		     TOP_cvtdq2ps_xx,
		     TOP_vcvtps2dq,
		     TOP_vcvttps2dq,
		     TOP_vcvtdq2psx,
		     TOP_vcvtdq2psxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  
  Instruction_Group( "vector cvt I w/ memory operand",
		     TOP_cvtdq2pd_x,
		     TOP_cvtdq2pd_xx,
		     TOP_cvtdq2pd_xxx,
		     TOP_vcvtdq2pdx,
		     TOP_vcvtdq2pdxx,
		     TOP_vcvtdq2pdxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "vector cvt II w/ memory operand",
		     TOP_cvtps2pd_x,
		     TOP_cvtps2pd_xx,
		     TOP_cvtps2pd_xxx,
		     TOP_vcvtps2pdx,
		     TOP_vcvtps2pdxx,
		     TOP_vcvtps2pdxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "vector cvt III w/ memory operand",
                     TOP_cvtpd2dq_x,
		     TOP_cvtpd2ps_x,
		     TOP_cvttpd2dq_x,
                     TOP_cvtpd2dq_xx,
		     TOP_cvtpd2ps_xx,
		     TOP_cvttpd2dq_xx,
                     TOP_cvtpd2dq_xxx,
		     TOP_cvtpd2ps_xxx,
		     TOP_cvttpd2dq_xxx,
                     TOP_vcvtpd2dqx,
                     TOP_vcvtpd2dqyx,
		     TOP_vcvtpd2psx,
		     TOP_vcvtpd2psyx,
		     TOP_vcvttpd2dqx,
		     TOP_vcvttpd2dqyx,
                     TOP_vcvtpd2dqxx,
                     TOP_vcvtpd2dqyxx,
		     TOP_vcvtpd2psxx,
		     TOP_vcvtpd2psyxx,
		     TOP_vcvttpd2dqxx,
		     TOP_vcvttpd2dqyxx,
                     TOP_vcvtpd2dqxxx,
                     TOP_vcvtpd2dqyxxx,
		     TOP_vcvtpd2psxxx,
		     TOP_vcvtpd2psyxxx,
		     TOP_vcvttpd2dqxxx,
		     TOP_vcvttpd2dqyxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "vector cvt IV w/ memory operand",
		     TOP_cvtps2dq_x,
		     TOP_cvtps2dq_xx,
		     TOP_cvtps2dq_xxx,
		     TOP_cvtdq2ps_xxx,
		     TOP_cvttps2dq_x,
		     TOP_cvttps2dq_xx,
		     TOP_cvttps2dq_xxx,
		     TOP_vcvtps2dqx,
		     TOP_vcvtps2dqxx,
		     TOP_vcvtps2dqxxx,
		     TOP_vcvtdq2psxxx,
		     TOP_vcvttps2dqx,
		     TOP_vcvttps2dqxx,
		     TOP_vcvttps2dqxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "int-2-float w/ mem",
		     TOP_cvtsi2ss_x,
		     TOP_cvtsi2ss_xx,
		     TOP_cvtsi2ss_xxx,
		     TOP_cvtsi2ssq_x,
		     TOP_cvtsi2ssq_xx,
		     TOP_cvtsi2ssq_xxx,
		     TOP_vcvtsi2ssx,
		     TOP_vcvtsi2ssxx,
		     TOP_vcvtsi2ssxxx,
		     TOP_vcvtsi2ssqx,
		     TOP_vcvtsi2ssqxx,
		     TOP_vcvtsi2ssqxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "int-2-float",
		     TOP_cvtsi2ss,
		     TOP_cvtsi2ssq,
		     TOP_vcvtsi2ss,
		     TOP_vcvtsi2ssq,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "double-2-float",
		     TOP_cvtsd2ss,
		     TOP_vcvtsd2ss,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "double-2-float w/ mem",
		     TOP_cvtsd2ss_x,
		     TOP_cvtsd2ss_xx,
		     TOP_cvtsd2ss_xxx,
		     TOP_vcvtsd2ssx,
		     TOP_vcvtsd2ssxx,
		     TOP_vcvtsd2ssxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "int-2-double",
		     TOP_cvtsi2sd,
		     TOP_cvtsi2sdq,
		     TOP_vcvtsi2sd,
		     TOP_vcvtsi2sdq,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4); 
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "int-2-double w/ mem",
		     TOP_cvtsi2sd_x,
		     TOP_cvtsi2sd_xx,
		     TOP_cvtsi2sd_xxx,
		     TOP_cvtsi2sdq_x,
		     TOP_cvtsi2sdq_xx,
		     TOP_cvtsi2sdq_xxx,
		     TOP_vcvtsi2sdx,
		     TOP_vcvtsi2sdxx,
		     TOP_vcvtsi2sdxxx,
		     TOP_vcvtsi2sdqx,
		     TOP_vcvtsi2sdqxx,
		     TOP_vcvtsi2sdqxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8); 
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "int-2-float",
		     TOP_movg2x,
		     TOP_movg2x64,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "float-2-int",
		     TOP_movx2g,
		     TOP_movx2g64,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "float-2-int 2",
		     TOP_cvtss2sd,
		     TOP_vcvtss2sd,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "float-2-int 2 mem opnd",
		     TOP_vcvtss2sdx,
		     TOP_vcvtss2sdxx,
		     TOP_vcvtss2sdxxx,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  
  Instruction_Group( "float-2-int 3",
		     TOP_cvtss2si,
		     TOP_cvtsd2si,
		     TOP_cvtss2siq,
		     TOP_cvtsd2siq,
		     TOP_cvttss2si,
		     TOP_cvttsd2si,
		     TOP_cvttss2siq,
		     TOP_cvttsd2siq,
		     TOP_vcvtss2si,
		     TOP_vcvtsd2si,
		     TOP_vcvtss2siq,
		     TOP_vcvtsd2siq,
		     TOP_vcvttss2si,
		     TOP_vcvttsd2si,
		     TOP_vcvttss2siq,
		     TOP_vcvttsd2siq,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "float-2-int 3 mem opnd",
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  
  Instruction_Group( "float-2-int 4",
		     TOP_pmovmskb128,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group("float-alu",
		    TOP_addss,
		    TOP_addsd,
		    TOP_subsd,
		    TOP_subss,
		    TOP_fadd,
		    TOP_faddp,
		    TOP_fsub,
		    TOP_fsubp,
		    TOP_fsubr,
		    TOP_fsubrp,
		    TOP_fmul,
		    TOP_fmulp,
		    TOP_fxch,
		    TOP_fistps,
		    TOP_fistpl,
		    TOP_fists,
		    TOP_fistl,
		    TOP_fistpll,
		    TOP_fisttps,
		    TOP_fisttpl,
		    TOP_fisttpll,
		    TOP_fldz,
                    TOP_vfaddsd,
                    TOP_vfaddss,
                    TOP_vsubsd,
                    TOP_vsubss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu II",
		    TOP_comisd,
		    TOP_comiss,
		    TOP_ucomisd,
		    TOP_ucomiss,
		    TOP_fucomi,
		    TOP_fucomip,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu for float vector class I",
		    TOP_faddsub128v32,
		    TOP_faddsub128v64,
		    TOP_fhadd128v32,
		    TOP_fhadd128v64,
		    TOP_fhsub128v32,
		    TOP_fhsub128v64,
		    TOP_fadd128v32,
		    TOP_fadd128v64,
		    TOP_fsub128v32,
		    TOP_fsub128v64,
                    TOP_vfaddsub128v64,
                    TOP_vfaddsub128v32,
                    TOP_vfadd128v64,
                    TOP_vfadd128v32,
                    TOP_vfsub128v64,
                    TOP_vfsub128v32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu for float vector class I w/ memory operand",
		    TOP_faddsubx128v32,
		    TOP_faddsubx128v64,
		    TOP_fhaddx128v32,
		    TOP_fhaddx128v64,
		    TOP_fhsubx128v32,
		    TOP_fhsubx128v64,
		    TOP_faddsubxx128v32,
		    TOP_faddsubxx128v64,
		    TOP_fhaddxx128v32,
		    TOP_fhaddxx128v64,
		    TOP_fhsubxx128v32,
		    TOP_fhsubxx128v64,
		    TOP_faddsubxxx128v32,
		    TOP_faddsubxxx128v64,
		    TOP_fhaddxxx128v32,
		    TOP_fhaddxxx128v64,
		    TOP_fhsubxxx128v32,
		    TOP_fhsubxxx128v64,
		    TOP_faddx128v32,
		    TOP_faddx128v64,
		    TOP_fsubx128v32,
		    TOP_fsubx128v64,
		    TOP_faddxx128v32,
		    TOP_faddxx128v64,
		    TOP_fsubxx128v32,
		    TOP_fsubxx128v64,
		    TOP_faddxxx128v32,
		    TOP_faddxxx128v64,
		    TOP_fsubxxx128v32,
		    TOP_fsubxxx128v64,
                    TOP_vfaddsubx128v64,
                    TOP_vfaddsubxx128v64,
                    TOP_vfaddsubxxx128v64,
                    TOP_vfaddsubx128v32,
                    TOP_vfaddsubxx128v32,
                    TOP_vfaddsubxxx128v32,
                    TOP_vfaddx128v64,
                    TOP_vfaddxx128v64,
                    TOP_vfaddxxx128v64,
                    TOP_vfaddx128v32,
                    TOP_vfaddxx128v32,
                    TOP_vfaddxxx128v32,
                    TOP_vfsubx128v64,
                    TOP_vfsubxx128v64,
                    TOP_vfsubxxx128v64,
                    TOP_vfsubx128v32,
                    TOP_vfsubxx128v32,
                    TOP_vfsubxxx128v32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu for float vector class II",
		    TOP_fand128v32,
		    TOP_fand128v64,
		    TOP_for128v32,
		    TOP_for128v64,
		    TOP_fxor128v32,
		    TOP_fxor128v64,
		    TOP_andps,
		    TOP_andpd,
		    TOP_xorps,
		    TOP_xorpd,
		    TOP_xzero32,
		    TOP_xzero64,
		    TOP_xzero128v32,
		    TOP_xzero128v64,
		    TOP_orps,
		    TOP_orpd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class II w/ memory operand",
		    TOP_fandx128v32,
		    TOP_fandx128v64,
		    TOP_forx128v32,
		    TOP_forx128v64,
		    TOP_fxorx128v32,
		    TOP_fxorx128v64,
		    TOP_fandxx128v32,
		    TOP_fandxx128v64,
		    TOP_forxx128v32,
		    TOP_forxx128v64,
		    TOP_fxorxx128v32,
		    TOP_fxorxx128v64,
		    TOP_fandxxx128v32,
		    TOP_fandxxx128v64,
		    TOP_forxxx128v32,
		    TOP_forxxx128v64,
		    TOP_fxorxxx128v32,
		    TOP_fxorxxx128v64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu for float vector class III",
		    TOP_fmul128v32,
		    TOP_fmul128v64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class III w/memory operand",
		    TOP_fmulx128v32,
		    TOP_fmulx128v64,
		    TOP_fmulxx128v32,
		    TOP_fmulxx128v64,
		    TOP_fmulxxx128v32,
		    TOP_fmulxxx128v64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu/div for float vector class IV",
		    TOP_fdiv128v32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(24);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu/sqrt for float vector class IV",
		    TOP_fsqrt,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(52);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("x87 cosine",
		    TOP_fcos,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(151);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("x87 sine",
		    TOP_fsin,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(148);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("float-alu for float vector class IV w/ memory operand",
		    TOP_fdivx128v32,
		    TOP_fdivxx128v32,
		    TOP_fdivxxx128v32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(28);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu for float vector class V",
		    TOP_fdiv128v64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(27);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class V w/ memory operand",
		    TOP_fdivx128v64,
		    TOP_fdivxx128v64,
		    TOP_fdivxxx128v64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(31);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-vector fma4",
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
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5); 
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-vector fma4 mem opnd",
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
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu for float vector class VI",
		    TOP_fmax128v32,
		    TOP_fmax128v64,
		    TOP_fmin128v32,
		    TOP_fmin128v64,
		    TOP_psadbw,
		    TOP_psadbw128,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu for float vector class VI w/ memory operand",
		    TOP_fmaxx128v32,
		    TOP_fmaxx128v64,
		    TOP_fminx128v32,
		    TOP_fminx128v64,
		    TOP_fmaxxx128v32,
		    TOP_fmaxxx128v64,
		    TOP_fminxx128v32,
		    TOP_fminxx128v64,
		    TOP_fmaxxxx128v32,
		    TOP_fmaxxxx128v64,
		    TOP_fminxxx128v32,
		    TOP_fminxxx128v64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu for float vector class VII",
		    TOP_fsqrt128v32,
                    TOP_vfsqrt128v32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(29); 
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class VIII",
		    TOP_fsqrt128v64,
                    TOP_vfsqrt128v64,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(38); 
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class IX",
		    TOP_frsqrt128v32,
		    TOP_frcp128v32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class X",
		    TOP_unpcklpd,
		    TOP_unpckhpd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class XI",
		    TOP_unpcklps,
		    TOP_unpckhps,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class XII",
		    TOP_fmovsldup,
		    TOP_fmovshdup,
		    TOP_fmovddup,
		    TOP_shufps,
		    TOP_shufpd,
		    TOP_pshufd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class XII w/ memory operand",
		    TOP_fmovsldupx,
		    TOP_fmovshdupx,
		    TOP_fmovddupx,
		    TOP_fmovsldupxx,
		    TOP_fmovshdupxx,
		    TOP_fmovddupxx,
		    TOP_fmovsldupxxx,
		    TOP_fmovshdupxxx,
		    TOP_fmovddupxxx,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu for float vector class XIIa",
		    TOP_pshufw,
		    TOP_pshuflw,
		    TOP_pshufhw,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("mmx shuffle",
		    TOP_pshufw64v16,
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
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class XIII",
		    TOP_psllw,
		    TOP_pslld,
		    TOP_psllq,
		    TOP_psrlw,
		    TOP_psrld,
		    TOP_psrlq,
		    TOP_psraw,
		    TOP_psrad,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class XIV",
		    TOP_psrldq,
		    TOP_psrlq128v64,
		    TOP_pslldq,
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
		    TOP_packsswb,
		    TOP_packssdw,
		    TOP_packuswb,
		    TOP_packsswb128,
		    TOP_packssdw128,
		    TOP_packuswb128,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector shift imm",
		    TOP_psllwi,
		    TOP_pslldi,
		    TOP_psllqi,
		    TOP_psrlwi,
		    TOP_psrldi,
		    TOP_psrlqi,
		    TOP_psrawi,
		    TOP_psradi,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for float vector class XV",
		    TOP_cmpps,
		    TOP_cmppd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("sse parallel compares",
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
                    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu for float vector class XVI",
		    TOP_cmpgt128v8,
		    TOP_cmpgt128v16,
		    TOP_cmpgt128v32,
		    TOP_cmpeq128v8,
		    TOP_cmpeq128v16,
		    TOP_cmpeq128v32,
		    TOP_pcmpeqb,
		    TOP_pcmpeqw,
		    TOP_pcmpeqd,
		    TOP_pcmpgtb,
		    TOP_pcmpgtw,
		    TOP_pcmpgtd,
		    TOP_max64v8,
		    TOP_max64v16,
		    TOP_min64v8,
		    TOP_min64v16,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu for float vector class XVII",
		    TOP_cmpgtx128v8,
		    TOP_cmpgtx128v16,
		    TOP_cmpgtx128v32,
		    TOP_cmpeqx128v8,
		    TOP_cmpeqx128v16,
		    TOP_cmpeqx128v32,
		    TOP_cmpgtxx128v8,
		    TOP_cmpgtxx128v16,
		    TOP_cmpgtxx128v32,
		    TOP_cmpeqxx128v8,
		    TOP_cmpeqxx128v16,
		    TOP_cmpeqxx128v32,
		    TOP_cmpgtxxx128v8,
		    TOP_cmpgtxxx128v16,
		    TOP_cmpgtxxx128v32,
		    TOP_cmpeqxxx128v8,
		    TOP_cmpeqxxx128v16,
		    TOP_cmpeqxxx128v32,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6); 
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu for int vector",
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
		    TOP_sub128v8,
		    TOP_sub128v16,
		    TOP_sub128v32,
		    TOP_sub128v64,		    
		    TOP_sub64v8,
		    TOP_sub64v16,
		    TOP_sub64v32,
		    TOP_and128v8,
		    TOP_and128v16,
		    TOP_and128v32,
		    TOP_and128v64,		    
		    TOP_or128v8,
		    TOP_or128v16,
		    TOP_or128v32,
		    TOP_or128v64,		    
		    TOP_xor128v8,
		    TOP_xor128v16,
		    TOP_xor128v32,
		    TOP_xor128v64,		    
		    TOP_subus128v16,
		    TOP_pavgb,
		    TOP_pavgw,
		    TOP_pavgb128,
		    TOP_pavgw128,
		    TOP_pand,
		    TOP_pandn,
		    TOP_por,
		    TOP_pxor,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-alu for int mpy vector",
		    TOP_mul128v16,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-alu for mmx int mpy vector",
		    TOP_pmullw,
		    TOP_pmulhw,
		    TOP_pmulhuw,
		    TOP_pmuludq,
		    TOP_pmaddwd,
		    TOP_pmullw128,
		    TOP_pmulhw128,
		    TOP_pmulhuw128,
		    TOP_pmuludq128,
		    TOP_pmaddwd128,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
		    
  Instruction_Group("float-alu for int vector w/ memory operand",
		    TOP_addx128v8,
		    TOP_addx128v16,
		    TOP_addx128v32,
		    TOP_addx128v64,
		    TOP_subx128v8,
		    TOP_subx128v16,
		    TOP_subx128v32,
		    TOP_subx128v64,		    
		    TOP_andx128v8,
		    TOP_andx128v16,
		    TOP_andx128v32,
		    TOP_andx128v64,		    
		    TOP_orx128v8,
		    TOP_orx128v16,
		    TOP_orx128v32,
		    TOP_orx128v64,		    
		    TOP_xorx128v8,
		    TOP_xorx128v16,
		    TOP_xorx128v32,
		    TOP_xorx128v64,		    
		    TOP_addxx128v8,
		    TOP_addxx128v16,
		    TOP_addxx128v32,
		    TOP_addxx128v64,
		    TOP_subxx128v8,
		    TOP_subxx128v16,
		    TOP_subxx128v32,
		    TOP_subxx128v64,		    
		    TOP_andxx128v8,
		    TOP_andxx128v16,
		    TOP_andxx128v32,
		    TOP_andxx128v64,		    
		    TOP_orxx128v8,
		    TOP_orxx128v16,
		    TOP_orxx128v32,
		    TOP_orxx128v64,		    
		    TOP_xorxx128v8,
		    TOP_xorxx128v16,
		    TOP_xorxx128v32,
		    TOP_xorxx128v64,		    
		    TOP_addxxx128v8,
		    TOP_addxxx128v16,
		    TOP_addxxx128v32,
		    TOP_addxxx128v64,
		    TOP_subxxx128v8,
		    TOP_subxxx128v16,
		    TOP_subxxx128v32,
		    TOP_subxxx128v64,		    
		    TOP_andxxx128v8,
		    TOP_andxxx128v16,
		    TOP_andxxx128v32,
		    TOP_andxxx128v64,		    
		    TOP_orxxx128v8,
		    TOP_orxxx128v16,
		    TOP_orxxx128v32,
		    TOP_orxxx128v64,		    
		    TOP_xorxxx128v8,
		    TOP_xorxxx128v16,
		    TOP_xorxxx128v32,
		    TOP_xorxxx128v64,		    
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu w/ memory operand",
		    TOP_addxss,
		    TOP_addxxss,
		    TOP_addxxxss,
		    TOP_addxsd,
		    TOP_addxxsd,
		    TOP_addxxxsd,
		    TOP_subxss,
		    TOP_subxxss,
		    TOP_subxxxss,
		    TOP_subxsd,
		    TOP_subxxsd,
		    TOP_subxxxsd,
		    TOP_filds,
		    TOP_fildl,
		    TOP_fildll,
                    TOP_vfaddxsd,
                    TOP_vfaddxxsd,
                    TOP_vfaddxxxsd,
                    TOP_vfaddxss,
                    TOP_vfaddxxss,
                    TOP_vfaddxxxss,
                    TOP_vsubxsd,
                    TOP_vsubxxsd,
                    TOP_vsubxxxsd,
                    TOP_vsubxss,
                    TOP_vsubxxss,
                    TOP_vsubxxxss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-alu w/ memory operand II",
		    TOP_comixsd,
		    TOP_comixxsd,
		    TOP_comixss,
		    TOP_comixxss,
		    TOP_comixxxsd,
		    TOP_comixxxss,
                    TOP_ucomixsd,
                    TOP_ucomixxsd,
                    TOP_ucomixss,
                    TOP_ucomixxss,
                    TOP_ucomixxxsd,
                    TOP_ucomixxxss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-max/min",
		    TOP_maxsd,
		    TOP_maxss,
		    TOP_minsd,
		    TOP_minss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("float-xorp",
		    TOP_andnps,
		    TOP_andnpd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-xorp 2",
		    TOP_rcpss,
		    TOP_rsqrtss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-mul",
		    TOP_mulsd,
		    TOP_mulss,
                    TOP_vmulsd,
                    TOP_vmulss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-mul w/ mem operand",
		    TOP_mulxsd,
		    TOP_mulxxsd,
		    TOP_mulxxxsd,
		    TOP_mulxss,
		    TOP_mulxxss,
		    TOP_mulxxxss,
                    TOP_vmulxsd,
                    TOP_vmulxxsd,
                    TOP_vmulxxxsd,
                    TOP_vmulxss,
                    TOP_vmulxxss,
                    TOP_vmulxxxss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-divide ss",
		    TOP_divss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(24);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-divide sd",
		    TOP_divsd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(27);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-divide x87",
		    TOP_fdiv,
		    TOP_fdivp,
		    TOP_fdivr,
		    TOP_fdivrp,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(42);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("float-divide 1 w/ mem operand",
		    TOP_divxss,
		    TOP_divxxss,
		    TOP_divxxxss,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(28);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-divide 2 w/ mem operand",
		    TOP_divxsd,
		    TOP_divxxsd,
		    TOP_divxxxsd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(31);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group("float-others",
		    TOP_frndint,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("sync",
		    TOP_mfence,
		    TOP_lfence,
		    TOP_sfence,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_agu, 0);

  Instruction_Group("prefetch",
		    TOP_prefetch,
		    TOP_prefetchw,
		    TOP_prefetcht0,
		    TOP_prefetcht1,
		    TOP_prefetchnta,
		    TOP_prefetchx,
		    TOP_prefetchwx,
		    TOP_prefetcht0x,
		    TOP_prefetcht1x,
		    TOP_prefetchntax,
		    TOP_prefetchxx,
		    TOP_prefetchwxx,
		    TOP_prefetcht0xx,
		    TOP_prefetcht1xx,
		    TOP_prefetchntaxx,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_agu, 0);

  Instruction_Group("nop",
		    TOP_nop,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_alu, 0);

  Instruction_Group("emms",
                    TOP_emms,
                    TOP_UNDEFINED);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group("clflush",
		    TOP_clflush,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_alu, 0);

  Instruction_Group("zeroupper",
                    TOP_vzeroupper,
                    TOP_vzeroall,
                    TOP_UNDEFINED);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group("mov-int-2-mmx",
                    TOP_movi32_2m,
                    TOP_movi64_2m,
                    TOP_UNDEFINED);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("mov-mmx-2-int",
                    TOP_movm_2i32,
                    TOP_movm_2i64,
		    TOP_pmovmskb,
                    TOP_UNDEFINED);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("mov-mmx-2-sse",
                    TOP_movq2dq,
                    TOP_UNDEFINED);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("mov-sse_2_mmx",
                    TOP_movdq2q,
                    TOP_UNDEFINED);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "vector cvt sse <-> mmx 1",
		     TOP_cvtpi2ps,
		     TOP_cvtps2pi,
		     TOP_cvttps2pi,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "vector cvt sse <-> mmx 2",
		     TOP_cvtpi2pd,
		     TOP_cvtpd2pi,
		     TOP_cvttpd2pi,
		     TOP_UNDEFINED );
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("SSE movmsk",
		    TOP_movmskps,
		    TOP_movmskpd,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

 Instruction_Group("SSE4A extract reg opnd",
                    TOP_extrq,
                    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group("SSE4A insert reg opnd",
                    TOP_insertq,
                    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);

  /* SSE3 monitor/mwait */
  Instruction_Group("monitor mwait",
                    TOP_monitor,
                    TOP_mwait,
                    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_agu, 0);

  /* SSSE3 instructions */
  Instruction_Group( "ssse3 misc reg opnd",
                        TOP_psign128v8,
                        TOP_psign128v16,
                        TOP_psign128v32,
                        TOP_pabs128v8,
                        TOP_pabs128v16,
                        TOP_pabs128v32,
                        TOP_palignr128,
                        TOP_pshuf128v8,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "ssse3 misc mem opnd",
                        TOP_psignx128v8,
                        TOP_psignxx128v8,
                        TOP_psignxxx128v8,
                        TOP_psignx128v16,
                        TOP_psignxx128v16,
                        TOP_psignxxx128v16,
                        TOP_psignx128v32,
                        TOP_psignxx128v32,
                        TOP_psignxxx128v32,
                        TOP_pabsx128v8,
                        TOP_pabsxx128v8,
                        TOP_pabsxxx128v8,
                        TOP_pabsx128v16,
                        TOP_pabsxx128v16,
                        TOP_pabsxxx128v16,
                        TOP_pabsx128v32,
                        TOP_pabsxx128v32,
                        TOP_pabsxxx128v32,
                        TOP_palignrx128,
                        TOP_palignrxx128,
                        TOP_palignrxxx128,
                        TOP_pshufx128v8,
                        TOP_pshufxx128v8,
                        TOP_pshufxxx128v8,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "ssse3 hadd reg opnd",
                        TOP_phsub128v16,
                        TOP_phsub128v32,
                        TOP_phsubs128v16,
                        TOP_phadd128v16,
                        TOP_phadd128v32,
                        TOP_phadds128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "ssse3 hadd mem opnd",
                        TOP_phsubx128v16,
                        TOP_phsubxx128v16,
                        TOP_phsubxxx128v16,
                        TOP_phsubx128v32,
                        TOP_phsubxx128v32,
                        TOP_phsubxxx128v32,
                        TOP_phsubsx128v16,
                        TOP_phsubsxx128v16,
                        TOP_phsubsxxx128v16,
                        TOP_phaddx128v16,
                        TOP_phaddxx128v16,
                        TOP_phaddxxx128v16,
                        TOP_phaddx128v32,
                        TOP_phaddxx128v32,
                        TOP_phaddxxx128v32,
                        TOP_phaddsx128v16,
                        TOP_phaddsxx128v16,
                        TOP_phaddsxxx128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "ssse3 mul reg opnd",
                        TOP_pmulhrsw128,
                        TOP_pmaddubsw128,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "ssse3 mul mem opnd",
                        TOP_pmulhrswx128,
                        TOP_pmulhrswxx128,
                        TOP_pmulhrswxxx128,
                        TOP_pmaddubswx128,
                        TOP_pmaddubswxx128,
                        TOP_pmaddubswxxx128,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  /* SSE4.1 instructions */
  Instruction_Group( "sse4.1 pmov reg opnd",
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
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 pmov reg opnd 2",
                        TOP_phminposuw,
                        TOP_pmovsxbwx,
                        TOP_pmovsxbwxx,
                        TOP_pmovsxbwxxx,
                        TOP_pmovzxbwx,
                        TOP_pmovzxbwxx,
                        TOP_pmovzxbwxxx,
                        TOP_pmovsxbdx,
                        TOP_pmovsxbdxx,
                        TOP_pmovsxbdxxx,
                        TOP_pmovzxbdx,
                        TOP_pmovzxbdxx,
                        TOP_pmovzxbdxxx,
                        TOP_pmovsxbqx,
                        TOP_pmovsxbqxx,
                        TOP_pmovsxbqxxx,
                        TOP_pmovzxbqx,
                        TOP_pmovzxbqxx,
                        TOP_pmovzxbqxxx,
                        TOP_pmovsxwdx,
                        TOP_pmovsxwdxx,
                        TOP_pmovsxwdxxx,
                        TOP_pmovzxwdx,
                        TOP_pmovzxwdxx,
                        TOP_pmovzxwdxxx,
                        TOP_pmovsxwqx,
                        TOP_pmovsxwqxx,
                        TOP_pmovsxwqxxx,
                        TOP_pmovzxwqx,
                        TOP_pmovzxwqxx,
                        TOP_pmovzxwqxxx,
                        TOP_pmovsxdqx,
                        TOP_pmovsxdqxx,
                        TOP_pmovsxdqxxx,
                        TOP_pmovzxdqx,
                        TOP_pmovzxdqxx,
                        TOP_pmovzxdqxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 pmov mem opnd",
                        TOP_minux128v8,
                        TOP_minuxx128v8,
                        TOP_minuxxx128v8,
                        TOP_minsx128v8,
                        TOP_minsxx128v8,
                        TOP_minsxxx128v8,
                        TOP_maxux128v8,
                        TOP_maxuxx128v8,
                        TOP_maxuxxx128v8,
                        TOP_maxsx128v8,
                        TOP_maxsxx128v8,
                        TOP_maxsxxx128v8,
                        TOP_minux128v16,
                        TOP_minuxx128v16,
                        TOP_minuxxx128v16,
                        TOP_minsx128v16,
                        TOP_minsxx128v16,
                        TOP_minsxxx128v16,
                        TOP_maxux128v16,
                        TOP_maxuxx128v16,
                        TOP_maxuxxx128v16,
                        TOP_maxsx128v16,
                        TOP_maxsxx128v16,
                        TOP_maxsxxx128v16,
                        TOP_minux128v32,
                        TOP_minuxx128v32,
                        TOP_minuxxx128v32,
                        TOP_maxux128v32,
                        TOP_maxuxx128v32,
                        TOP_maxuxxx128v32,
                        TOP_minsx128v32,
                        TOP_minsxx128v32,
                        TOP_minsxxx128v32,
                        TOP_maxsx128v32,
                        TOP_maxsxx128v32,
                        TOP_maxsxxx128v32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 pmov mem opnd 2",
                        TOP_phminposuwx,
                        TOP_phminposuwxx,
                        TOP_phminposuwxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 misc reg opnd",
                        TOP_round128v32,
                        TOP_roundss,
                        TOP_round128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 misc mem opnd",
                        TOP_roundx128v32,
                        TOP_roundxx128v32,
                        TOP_roundxxx128v32,
                        TOP_roundxss,
                        TOP_roundxxss,
                        TOP_roundxxxss,
                        TOP_roundx128v64,
                        TOP_roundxx128v64,
                        TOP_roundxxx128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 blend reg opnd",
                        TOP_fblend128v32,
                        TOP_fblend128v64,
                        TOP_fblendv128v32,
                        TOP_fblendv128v64,
                        TOP_blendv128v8,
                        TOP_blend128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 blend mem opnd",
                        TOP_fblendx128v32,
                        TOP_fblendxx128v32,
                        TOP_fblendxxx128v32,
                        TOP_fblendx128v64,
                        TOP_fblendxx128v64,
                        TOP_fblendxxx128v64,
                        TOP_fblendvx128v32,
                        TOP_fblendvxx128v32,
                        TOP_fblendvxxx128v32,
                        TOP_fblendvx128v64,
                        TOP_fblendvxx128v64,
                        TOP_fblendvxxx128v64,
                        TOP_blendvx128v8,
                        TOP_blendvxx128v8,
                        TOP_blendvxxx128v8,
                        TOP_blendx128v16,
                        TOP_blendxx128v16,
                        TOP_blendxxx128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 insr extr reg opnd",
                        TOP_insr128v8,
                        TOP_insr128v16,
                        TOP_insr128v32,
                        TOP_insr128v64,
                        TOP_extr128v8,
                        TOP_extr128v16,
                        TOP_extr128v32,
                        TOP_extr128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 insr extr mem opnd",
                        TOP_insrx128v8,
                        TOP_insrxx128v8,
                        TOP_insrxxx128v8,
                        TOP_insrx128v16,
                        TOP_insrxx128v16,
                        TOP_insrxxx128v16,
                        TOP_insrx128v32,
                        TOP_insrxx128v32,
                        TOP_insrxxx128v32,
                        TOP_insrx128v64,
                        TOP_insrxx128v64,
                        TOP_insrxxx128v64,
                        TOP_extrx128v8,
                        TOP_extrxx128v8,
                        TOP_extrxxx128v8,
                        TOP_extrx128v16,
                        TOP_extrxx128v16,
                        TOP_extrxxx128v16,
                        TOP_extrx128v32,
                        TOP_extrxx128v32,
                        TOP_extrxxx128v32,
                        TOP_extrx128v64,
                        TOP_extrxx128v64,
                        TOP_extrxxx128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 misc reg opnd",
                        TOP_roundsd,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 misc mem opnd",
                        TOP_roundxsd,
                        TOP_roundxxsd,
                        TOP_roundxxxsd,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 finsr fextr reg opnd",
                        TOP_finsr128v32,
                        TOP_fextr128v32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 finsr fextr mem opnd",
                        TOP_finsrx128v32,
                        TOP_finsrxx128v32,
                        TOP_finsrxxx128v32,
                        TOP_fextrx128v32,
                        TOP_fextrxx128v32,
                        TOP_fextrxxx128v32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 misc reg opnd",
                        TOP_mpsadbw,
                        TOP_muldq,
                        TOP_mul128v32,
                        TOP_fdp128v32,
                        TOP_fdp128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 misc mem opnd",
                        TOP_mpsadbwx,
                        TOP_mpsadbwxx,
                        TOP_mpsadbwxxx,
                        TOP_muldqx,
                        TOP_muldqxx,
                        TOP_muldqxxx,
                        TOP_fdpx128v32,
                        TOP_fdpxx128v32,
                        TOP_fdpxxx128v32,
                        TOP_vfdpx128v32,
                        TOP_vfdpxx128v32,
                        TOP_vfdpxxx128v32,
                        TOP_fdpx128v64,
                        TOP_fdpxx128v64,
                        TOP_fdpxxx128v64,
                        TOP_vfdpx128v64,
                        TOP_vfdpxx128v64,
                        TOP_vfdpxxx128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 misc mem opnd II",
                        TOP_mulx128v32,
                        TOP_mulxx128v32,
                        TOP_mulxxx128v32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 load ntdqa",
                        TOP_ldntdqa,
                        TOP_ldntdqax,
                        TOP_ldntdqaxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 store ntdqa",
                        TOP_stntdq,
                        TOP_stntdqx,
                        TOP_stntdqxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 pmov reg opnd",
                        TOP_cmpeq128v64,
                        TOP_packusdw,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 pmov mem opnd",
                        TOP_cmpeqx128v64,
                        TOP_cmpeqxx128v64,
                        TOP_cmpeqxxx128v64,
                        TOP_packusdwx,
                        TOP_packusdwxx,
                        TOP_packusdwxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.1 ptest reg opnd",
                        TOP_ptest128,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.1 ptest mem opnd",
                        TOP_ptestx128,
                        TOP_ptestxx128,
                        TOP_ptestxxx128,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  /* SSE4.2 instructions */
  Instruction_Group( "sse4.2 crc reg opnd",
                        TOP_crc32b,
                        TOP_crc32w,
                        TOP_crc32d,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.2 crcq reg opnd",
                        TOP_crc32q,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.2 crc mem opnd",
                        TOP_crc32bx,
                        TOP_crc32bxx,
                        TOP_crc32bxxx,
                        TOP_crc32wx,
                        TOP_crc32wxx,
                        TOP_crc32wxxx,
                        TOP_crc32dx,
                        TOP_crc32dxx,
                        TOP_crc32dxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.2 crcq mem opnd",
                        TOP_crc32qx,
                        TOP_crc32qxx,
                        TOP_crc32qxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.2 pcmp reg opnd",
                        TOP_cmpestri,
                        TOP_cmpestrm,
                        TOP_cmpistri,
                        TOP_cmpistrm,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(30);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.2 pcmp mem opnd",
                        TOP_cmpestrix,
                        TOP_cmpestrixx,
                        TOP_cmpestrixxx,
                        TOP_cmpestrmx,
                        TOP_cmpestrmxx,
                        TOP_cmpestrmxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(34);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.2 pcmp mem opnd 2",
                        TOP_cmpistrix,
                        TOP_cmpistrixx,
                        TOP_cmpistrixxx,
                        TOP_cmpistrmx,
                        TOP_cmpistrmxx,
                        TOP_cmpistrmxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(14);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.2 pcmp reg opnd",
                        TOP_cmpgt128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.2 popcnt reg opnd",
                        TOP_popcnt16,
                        TOP_popcnt32,
                        TOP_popcnt64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "sse4.2 pcmp mem opnd",
                        TOP_cmpgtx128v64,
                        TOP_cmpgtxx128v64,
                        TOP_cmpgtxxx128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.2 popcnt mem opnd",
                        TOP_popcntx16,
                        TOP_popcntxx16,
                        TOP_popcntxxx16,
                        TOP_popcntx32,
                        TOP_popcntxx32,
                        TOP_popcntxxx32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "sse4.2 popcnt64 mem opnd",
                        TOP_popcntx64,
                        TOP_popcntxx64,
                        TOP_popcntxxx64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_alu, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  /* AES instructions */
  Instruction_Group( "aes reg opnd",
                        TOP_aesenc,
                        TOP_aesenclast,
                        TOP_aesdec,
                        TOP_aesdeclast,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7); 
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "aes reg opnd 2",
                        TOP_aeskeygenassist,
                        TOP_aesimc,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5); 
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "aes mem opnd",
                        TOP_aesencx,
                        TOP_aesencxx,
                        TOP_aesencxxx,
                        TOP_aesenclastx,
                        TOP_aesenclastxx,
                        TOP_aesenclastxxx,
                        TOP_aesdecx,
                        TOP_aesdecxx,
                        TOP_aesdecxxx,
                        TOP_aesdeclastx,
                        TOP_aesdeclastxx,
                        TOP_aesdeclastxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(11);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "aes mem opnd",
                        TOP_aeskeygenassistx,
                        TOP_aeskeygenassistxx,
                        TOP_aeskeygenassistxxx,
                        TOP_aesimcx,
                        TOP_aesimcxx,
                        TOP_aesimcxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  /* PCLMUL instructions */
  Instruction_Group( "pclmul reg opnd",
                        TOP_pclmulqdq,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(12);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "pclmul mem opnd",
                        TOP_pclmulqdqx,
                        TOP_pclmulqdqxx,
                        TOP_pclmulqdqxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(16);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  /* XOP instructions */
  Instruction_Group( "xop.desc.sort_by_print reg opnd",
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
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "xop.desc.sort_by_print mem opnd",
                        TOP_vphaddbdx,
                        TOP_vphaddbdxx,
                        TOP_vphaddbdxxx,
                        TOP_vphaddbqx,
                        TOP_vphaddbqxx,
                        TOP_vphaddbqxxx,
                        TOP_vphaddbwx,
                        TOP_vphaddbwxx,
                        TOP_vphaddbwxxx,
                        TOP_vphadddqx,
                        TOP_vphadddqxx,
                        TOP_vphadddqxxx,
                        TOP_vphaddubdx,
                        TOP_vphaddubdxx,
                        TOP_vphaddubdxxx,
                        TOP_vphaddubqx,
                        TOP_vphaddubqxx,
                        TOP_vphaddubqxxx,
                        TOP_vphaddubwx,
                        TOP_vphaddubwxx,
                        TOP_vphaddubwxxx,
                        TOP_vphaddudqx,
                        TOP_vphaddudqxx,
                        TOP_vphaddudqxxx,
                        TOP_vphadduwdx,
                        TOP_vphadduwdxx,
                        TOP_vphadduwdxxx,
                        TOP_vphadduwqx,
                        TOP_vphadduwqxx,
                        TOP_vphadduwqxxx,
                        TOP_vphaddwdx,
                        TOP_vphaddwdxx,
                        TOP_vphaddwdxxx,
                        TOP_vphaddwqx,
                        TOP_vphaddwqxx,
                        TOP_vphaddwqxxx,
                        TOP_vphsubbwx,
                        TOP_vphsubbwxx,
                        TOP_vphsubbwxxx,
                        TOP_vphsubdqx,
                        TOP_vphsubdqxx,
                        TOP_vphsubdqxxx,
                        TOP_vphsubwdx,
                        TOP_vphsubwdxx,
                        TOP_vphsubwdxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "xop fma reg opnd",
                     TOP_vpmacsdqh,
                     TOP_vpmacsdql,
                     TOP_vpmacssdqh,
                     TOP_vpmacssdql,
                     TOP_vpmacsswd,
                     TOP_vpmacssww,
                     TOP_vpmacswd,
                     TOP_vpmacsww,
                     TOP_vpmadcsswd,
                     TOP_vpmadcswd,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "xop fma mem opnd",
                     TOP_vpmacsdqhx,
                     TOP_vpmacsdqhxx,
                     TOP_vpmacsdqhxxx,
                     TOP_vpmacsdqlx,
                     TOP_vpmacsdqlxx,
                     TOP_vpmacsdqlxxx,
                     TOP_vpmacssdqhx,
                     TOP_vpmacssdqhxx,
                     TOP_vpmacssdqhxxx,
                     TOP_vpmacssdqlx,
                     TOP_vpmacssdqlxx,
                     TOP_vpmacssdqlxxx,
                     TOP_vpmacsswdx,
                     TOP_vpmacsswdxx,
                     TOP_vpmacsswdxxx,
                     TOP_vpmacsswwx,
                     TOP_vpmacsswwxx,
                     TOP_vpmacsswwxxx,
                     TOP_vpmacswdx,
                     TOP_vpmacswdxx,
                     TOP_vpmacswdxxx,
                     TOP_vpmacswwx,
                     TOP_vpmacswwxx,
                     TOP_vpmacswwxxx,
                     TOP_vpmadcsswdx,
                     TOP_vpmadcsswdxx,
                     TOP_vpmadcsswdxxx,
                     TOP_vpmadcswdx,
                     TOP_vpmadcswdxx,
                     TOP_vpmadcswdxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "xop fma II reg opnd",
                     TOP_vpmacsdd,
                     TOP_vpmacssdd,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "xop fma II mem opnd",
                     TOP_vpmacsddx,
                     TOP_vpmacsddxx,
                     TOP_vpmacsddxxx,
                     TOP_vpmacssddx,
                     TOP_vpmacssddxx,
                     TOP_vpmacssddxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "xop frcz reg opnd",
                        TOP_vfrczpd,
                        TOP_vfrczps,
                        TOP_vfrczsd,
                        TOP_vfrczss,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "xop frcz mem opnd",
                        TOP_vfrczpdx,
                        TOP_vfrczpdxx,
                        TOP_vfrczpdxxx,
                        TOP_vfrczpsx,
                        TOP_vfrczpsxx,
                        TOP_vfrczpsxxx,
                        TOP_vfrczsdx,
                        TOP_vfrczsdxx,
                        TOP_vfrczsdxxx,
                        TOP_vfrczssx,
                        TOP_vfrczssxx,
                        TOP_vfrczssxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(14);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "xop.desc.sort_by_print reg opnd",
                        TOP_vpcmov,
                        TOP_vpcomb,
                        TOP_vpcomd,
                        TOP_vpcomq,
                        TOP_vpcomw,
                        TOP_vpcomub,
                        TOP_vpcomud,
                        TOP_vpcomuq,
                        TOP_vpcomuw,
                        TOP_vpperm,
                        TOP_vprotbi,
                        TOP_vprotdi,
                        TOP_vprotqi,
                        TOP_vprotwi,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "xop.desc.sort_by_print mem opnd",
                        TOP_vpcmovx,
                        TOP_vpcmovxx,
                        TOP_vpcmovxxx,
                        TOP_vpcmovxr,
                        TOP_vpcmovxxr,
                        TOP_vpcmovxxxr,
                        TOP_vpcombx,
                        TOP_vpcombxx,
                        TOP_vpcombxxx,
                        TOP_vpcomdx,
                        TOP_vpcomdxx,
                        TOP_vpcomdxxx,
                        TOP_vpcomqx,
                        TOP_vpcomqxx,
                        TOP_vpcomqxxx,
                        TOP_vpcomwx,
                        TOP_vpcomwxx,
                        TOP_vpcomwxxx,
                        TOP_vpcomubx,
                        TOP_vpcomubxx,
                        TOP_vpcomubxxx,
                        TOP_vpcomudx,
                        TOP_vpcomudxx,
                        TOP_vpcomudxxx,
                        TOP_vpcomuqx,
                        TOP_vpcomuqxx,
                        TOP_vpcomuqxxx,
                        TOP_vpcomuwx,
                        TOP_vpcomuwxx,
                        TOP_vpcomuwxxx,
                        TOP_vppermx,
                        TOP_vppermxx,
                        TOP_vppermxxx,
                        TOP_vppermxr,
                        TOP_vppermxxr,
                        TOP_vppermxxxr,
                        TOP_vprotbix,
                        TOP_vprotbixx,
                        TOP_vprotbixxx,
                        TOP_vprotdix,
                        TOP_vprotdixx,
                        TOP_vprotdixxx,
                        TOP_vprotqix,
                        TOP_vprotqixx,
                        TOP_vprotqixxx,
                        TOP_vprotwix,
                        TOP_vprotwixx,
                        TOP_vprotwixxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "xop rotate reg opnd 1",
                        TOP_vprotb,
                        TOP_vprotd,
                        TOP_vprotq,
                        TOP_vprotw,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "xop rotate reg opnd 2",
                        TOP_vpshab,
                        TOP_vpshad,
                        TOP_vpshaq,
                        TOP_vpshaw,
                        TOP_vpshlb,
                        TOP_vpshld,
                        TOP_vpshlq,
                        TOP_vpshlw,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "xop rotate mem mem opnd",
                        TOP_vprotbx,
                        TOP_vprotbxx,
                        TOP_vprotbxxx,
                        TOP_vprotbxr,
                        TOP_vprotbxxr,
                        TOP_vprotbxxxr,
                        TOP_vprotdx,
                        TOP_vprotdxx,
                        TOP_vprotdxxx,
                        TOP_vprotdxr,
                        TOP_vprotdxxr,
                        TOP_vprotdxxxr,
                        TOP_vprotqx,
                        TOP_vprotqxx,
                        TOP_vprotqxxx,
                        TOP_vprotqxr,
                        TOP_vprotqxxr,
                        TOP_vprotqxxxr,
                        TOP_vprotwx,
                        TOP_vprotwxx,
                        TOP_vprotwxxx,
                        TOP_vprotwxr,
                        TOP_vprotwxxr,
                        TOP_vprotwxxxr,
                        TOP_UNDEFINED); 
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "xop rotate mem mem opnd",
                        TOP_vpshabx,
                        TOP_vpshabxx,
                        TOP_vpshabxxx,
                        TOP_vpshabxr,
                        TOP_vpshabxxr,
                        TOP_vpshabxxxr,
                        TOP_vpshadx,
                        TOP_vpshadxx,
                        TOP_vpshadxxx,
                        TOP_vpshadxr,
                        TOP_vpshadxxr,
                        TOP_vpshadxxxr,
                        TOP_vpshaqx,
                        TOP_vpshaqxx,
                        TOP_vpshaqxxx,
                        TOP_vpshaqxr,
                        TOP_vpshaqxxr,
                        TOP_vpshaqxxxr,
                        TOP_vpshawx,
                        TOP_vpshawxx,
                        TOP_vpshawxxx,
                        TOP_vpshawxr,
                        TOP_vpshawxxr,
                        TOP_vpshawxxxr,
                        TOP_vpshlbx,
                        TOP_vpshlbxx,
                        TOP_vpshlbxxx,
                        TOP_vpshlbxr,
                        TOP_vpshlbxxr,
                        TOP_vpshlbxxxr,
                        TOP_vpshldx,
                        TOP_vpshldxx,
                        TOP_vpshldxxx,
                        TOP_vpshldxr,
                        TOP_vpshldxxr,
                        TOP_vpshldxxxr,
                        TOP_vpshlqx,
                        TOP_vpshlqxx,
                        TOP_vpshlqxxx,
                        TOP_vpshlqxr,
                        TOP_vpshlqxxr,
                        TOP_vpshlqxxxr,
                        TOP_vpshlwx,
                        TOP_vpshlwxx,
                        TOP_vpshlwxxx,
                        TOP_vpshlwxr,
                        TOP_vpshlwxxr,
                        TOP_vpshlwxxxr,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  /* AVX instructions */
  Instruction_Group( "avx int arith reg opnd",
                     TOP_vandpd,
                     TOP_vfand128v64,
                     TOP_vandps,
                     TOP_vfand128v32,
                     TOP_vandnpd,
                     TOP_vfandn128v64,
                     TOP_vandnps,
                     TOP_vfandn128v32,
                     TOP_vabs128v8,
                     TOP_vabs128v32,
                     TOP_vabs128v16,
                     TOP_vadd128v8,
                     TOP_vadd128v32,
                     TOP_vadd128v64,
                     TOP_vadd128v16,
                     TOP_vadds128v8,
                     TOP_vadds128v16,
                     TOP_vaddus128v8,
                     TOP_vaddus128v16,
                     TOP_vand128v8,
                     TOP_vand128v16,
                     TOP_vand128v32,
                     TOP_vand128v64,
                     TOP_vandn128v8,
                     TOP_vandn128v16,
                     TOP_vandn128v32,
                     TOP_vandn128v64,
                     TOP_vpavgb,
                     TOP_vpavgw,
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
                     TOP_vor128v8,
                     TOP_vor128v16,
                     TOP_vor128v32,
                     TOP_vor128v64,
                     TOP_vpshuf128v8,
                     TOP_vpshuf128v32,
                     TOP_vpshufw64v16,
                     TOP_vpshufhw,
                     TOP_vpshuflw,
                     TOP_vxor128v8,
                     TOP_vxor128v16,
                     TOP_vxor128v32,
                     TOP_vxor128v64,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith mem opnd",
                     TOP_vfandx128v64,
                     TOP_vfandxx128v64,
                     TOP_vfandxxx128v64,
                     TOP_vfandx128v32,
                     TOP_vfandxx128v32,
                     TOP_vfandxxx128v32,
                     TOP_vfandnx128v64,
                     TOP_vfandnxx128v64,
                     TOP_vfandnxxx128v64,
                     TOP_vfandnx128v32,
                     TOP_vfandnxx128v32,
                     TOP_vfandnxxx128v32,
                     TOP_vabsx128v8,
                     TOP_vabsxx128v8,
                     TOP_vabsxxx128v8,
                     TOP_vabsx128v32,
                     TOP_vabsxx128v32,
                     TOP_vabsxxx128v32,
                     TOP_vabsx128v16,
                     TOP_vabsxx128v16,
                     TOP_vabsxxx128v16,
                     TOP_vaddx128v8,
                     TOP_vaddxx128v8,
                     TOP_vaddxxx128v8,
                     TOP_vaddx128v32,
                     TOP_vaddxx128v32,
                     TOP_vaddxxx128v32,
                     TOP_vaddx128v64,
                     TOP_vaddxx128v64,
                     TOP_vaddxxx128v64,
                     TOP_vaddx128v16,
                     TOP_vaddxx128v16,
                     TOP_vaddxxx128v16,
                     TOP_vaddsx128v8,
                     TOP_vaddsxx128v8,
                     TOP_vaddsxxx128v8,
                     TOP_vaddsx128v16,
                     TOP_vaddsxx128v16,
                     TOP_vaddsxxx128v16,
                     TOP_vaddusx128v8,
                     TOP_vaddusxx128v8,
                     TOP_vaddusxxx128v8,
                     TOP_vaddusx128v16,
                     TOP_vaddusxx128v16,
                     TOP_vaddusxxx128v16,
                     TOP_vandx128v8,
                     TOP_vandxx128v8,
                     TOP_vandxxx128v8,
                     TOP_vandx128v16,
                     TOP_vandxx128v16,
                     TOP_vandxxx128v16,
                     TOP_vandx128v32,
                     TOP_vandxx128v32,
                     TOP_vandxxx128v32,
                     TOP_vandx128v64,
                     TOP_vandxx128v64,
                     TOP_vandxxx128v64,
                     TOP_vandnx128v8,
                     TOP_vandnxx128v8,
                     TOP_vandnxxx128v8,
                     TOP_vandnx128v16,
                     TOP_vandnxx128v16,
                     TOP_vandnxxx128v16,
                     TOP_vandnx128v32,
                     TOP_vandnxx128v32,
                     TOP_vandnxxx128v32,
                     TOP_vandnx128v64,
                     TOP_vandnxx128v64,
                     TOP_vandnxxx128v64,
                     TOP_vpavgbx,
                     TOP_vpavgbxx,
                     TOP_vpavgbxxx,
                     TOP_vpavgwx,
                     TOP_vpavgwxx,
                     TOP_vpavgwxxx,
                     TOP_vmaxsx128v8,
                     TOP_vmaxsxx128v8,
                     TOP_vmaxsxxx128v8,
                     TOP_vmaxsx128v32,
                     TOP_vmaxsxx128v32,
                     TOP_vmaxsxxx128v32,
                     TOP_vmaxsx128v16,
                     TOP_vmaxsxx128v16,
                     TOP_vmaxsxxx128v16,
                     TOP_vmaxux128v8,
                     TOP_vmaxuxx128v8,
                     TOP_vmaxuxxx128v8,
                     TOP_vmaxux128v32,
                     TOP_vmaxuxx128v32,
                     TOP_vmaxuxxx128v32,
                     TOP_vmaxux128v16,
                     TOP_vmaxuxx128v16,
                     TOP_vmaxuxxx128v16,
                     TOP_vminsx128v8,
                     TOP_vminsxx128v8,
                     TOP_vminsxxx128v8,
                     TOP_vminsx128v32,
                     TOP_vminsxx128v32,
                     TOP_vminsxxx128v32,
                     TOP_vminsx128v16,
                     TOP_vminsxx128v16,
                     TOP_vminsxxx128v16,
                     TOP_vminux128v8,
                     TOP_vminuxx128v8,
                     TOP_vminuxxx128v8,
                     TOP_vminux128v32,
                     TOP_vminuxx128v32,
                     TOP_vminuxxx128v32,
                     TOP_vminux128v16,
                     TOP_vminuxx128v16,
                     TOP_vminuxxx128v16,
                     TOP_vorx128v8,
                     TOP_vorxx128v8,
                     TOP_vorxxx128v8,
                     TOP_vorx128v16,
                     TOP_vorxx128v16,
                     TOP_vorxxx128v16,
                     TOP_vorx128v32,
                     TOP_vorxx128v32,
                     TOP_vorxxx128v32,
                     TOP_vorx128v64,
                     TOP_vorxx128v64,
                     TOP_vorxxx128v64,
                     TOP_vpshufx128v8,
                     TOP_vpshufxx128v8,
                     TOP_vpshufxxx128v8,
                     TOP_vpshufx128v32,
                     TOP_vpshufxx128v32,
                     TOP_vpshufxxx128v32,
                     TOP_vpshufwx64v16,
                     TOP_vpshufwxx64v16,
                     TOP_vpshufwxxx64v16,
                     TOP_vpshufhwx,
                     TOP_vpshufhwxx,
                     TOP_vpshufhwxxx,
                     TOP_vpshuflwx,
                     TOP_vpshuflwxx,
                     TOP_vpshuflwxxx,
                     TOP_vxorx128v8,
                     TOP_vxorxx128v8,
                     TOP_vxorxxx128v8,
                     TOP_vxorx128v16,
                     TOP_vxorxx128v16,
                     TOP_vxorxxx128v16,
                     TOP_vxorx128v32,
                     TOP_vxorxx128v32,
                     TOP_vxorxxx128v32,
                     TOP_vxorx128v64,
                     TOP_vxorxx128v64,
                     TOP_vxorxxx128v64,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith reg opnd",
                     TOP_vcmppd,
                     TOP_vfcmp128v64,
                     TOP_vcmpps,
                     TOP_vfcmp128v32,
                     TOP_vcmpsd,
                     TOP_vcmpss,
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
                     TOP_vfmax128v64,
                     TOP_vfmax128v32,
                     TOP_vfmaxsd,
                     TOP_vfmaxss,
                     TOP_vfmin128v64,
                     TOP_vfmin128v32,
                     TOP_vfminsd,
                     TOP_vfminss,
                     TOP_vorpd,
                     TOP_vfor128v64,
                     TOP_vorps,
                     TOP_vfor128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp arith mem opnd",
                     TOP_vfcmpx128v64,
                     TOP_vfcmpxx128v64,
                     TOP_vfcmpxxx128v64,
                     TOP_vfcmpx128v32,
                     TOP_vfcmpxx128v32,
                     TOP_vfcmpxxx128v32,
                     TOP_vcmpxsd,
                     TOP_vcmpxxsd,
                     TOP_vcmpxxxsd,
                     TOP_vcmpxss,
                     TOP_vcmpxxss,
                     TOP_vcmpxxxss,
                     TOP_vfmaxx128v64,
                     TOP_vfmaxxx128v64,
                     TOP_vfmaxxxx128v64,
                     TOP_vfmaxx128v32,
                     TOP_vfmaxxx128v32,
                     TOP_vfmaxxxx128v32,
                     TOP_vfmaxxsd,
                     TOP_vfmaxxxsd,
                     TOP_vfmaxxxxsd,
                     TOP_vfmaxxss,
                     TOP_vfmaxxxss,
                     TOP_vfmaxxxxss,
                     TOP_vfminx128v64,
                     TOP_vfminxx128v64,
                     TOP_vfminxxx128v64,
                     TOP_vfminx128v32,
                     TOP_vfminxx128v32,
                     TOP_vfminxxx128v32,
                     TOP_vfminxsd,
                     TOP_vfminxxsd,
                     TOP_vfminxxxsd,
                     TOP_vfminxss,
                     TOP_vfminxxss,
                     TOP_vfminxxxss,
                     TOP_vforx128v64,
                     TOP_vforxx128v64,
                     TOP_vforxxx128v64,
                     TOP_vforx128v32,
                     TOP_vforxx128v32,
                     TOP_vforxxx128v32,
                     TOP_vfxorx128v64,
                     TOP_vfxorxx128v64,
                     TOP_vfxorxxx128v64,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith II reg opnd",
                     TOP_vfhadd128v64,
                     TOP_vfhadd128v32,
                     TOP_vfhsub128v64,
                     TOP_vfhsub128v32,
                     TOP_vphadd128v16,
                     TOP_vphsub128v32,
                     TOP_vphsubs128v16,
                     TOP_vphsub128v16,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(11);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp arith II mem opnd",
                     TOP_vfhaddx128v64,
                     TOP_vfhaddxx128v64,
                     TOP_vfhaddxxx128v64,
                     TOP_vfhaddx128v32,
                     TOP_vfhaddxx128v32,
                     TOP_vfhaddxxx128v32,
                     TOP_vfhsubx128v64,
                     TOP_vfhsubxx128v64,
                     TOP_vfhsubxxx128v64,
                     TOP_vfhsubx128v32,
                     TOP_vfhsubxx128v32,
                     TOP_vfhsubxxx128v32,
                     TOP_vphaddx128v16,
                     TOP_vphaddxx128v16,
                     TOP_vphaddxxx128v16,
                     TOP_vphsubx128v32,
                     TOP_vphsubxx128v32,
                     TOP_vphsubxxx128v32,
                     TOP_vphsubsx128v16,
                     TOP_vphsubsxx128v16,
                     TOP_vphsubsxxx128v16,
                     TOP_vphsubx128v16,
                     TOP_vphsubxx128v16,
                     TOP_vphsubxxx128v16,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(15);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith III reg opnd",
                     TOP_vround128v64,
                     TOP_vround128v32,
                     TOP_vroundsd,
                     TOP_vroundss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp arith III mem opnd",
                     TOP_vroundx128v64,
                     TOP_vroundxx128v64,
                     TOP_vroundxxx128v64,
                     TOP_vroundx128v32,
                     TOP_vroundxx128v32,
                     TOP_vroundxxx128v32,
                     TOP_vroundxsd,
                     TOP_vroundxxsd,
                     TOP_vroundxxxsd,
                     TOP_vroundxss,
                     TOP_vroundxxss,
                     TOP_vroundxxxss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp mul reg opnd",
                     TOP_vpmaddwd,
                     TOP_vpmaddubsw128,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp mul mem opnd",
                     TOP_vpmaddwdx,
                     TOP_vpmaddwdxx,
                     TOP_vpmaddwdxxx,
                     TOP_vpmaddubswx128,
                     TOP_vpmaddubswxx128,
                     TOP_vpmaddubswxxx128,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp rsqrt reg opnd",
                     TOP_vfrsqrt128v32,
                     TOP_vfrsqrtss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp rsqrt mem opnd",
                     TOP_vfrsqrtx128v32,
                     TOP_vfrsqrtxx128v32,
                     TOP_vfrsqrtxxx128v32,
                     TOP_vfrsqrtxss,
                     TOP_vfrsqrtxxss,
                     TOP_vfrsqrtxxxss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp sqrt reg opnd",
                     TOP_vfsqrtss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(29);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp sqrt reg opnd 2",
                     TOP_vfsqrtsd,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(38);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp sqrt mem opnd",
                        TOP_vfsqrtx128v32,
                        TOP_vfsqrtxx128v32,
                        TOP_vfsqrtxxx128v32,
                        TOP_vfsqrtxss,
                        TOP_vfsqrtxxss,
                        TOP_vfsqrtxxxss,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(33);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp sqrt mem opnd 2",
                        TOP_vfsqrtx128v64,
                        TOP_vfsqrtxx128v64,
                        TOP_vfsqrtxxx128v64,
                        TOP_vfsqrtxsd,
                        TOP_vfsqrtxxsd,
                        TOP_vfsqrtxxxsd,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(42);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx div reg opnd",
                     TOP_vfdiv128v64,
                     TOP_vdivsd,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(27);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx div mem opnd",
                     TOP_vfdivx128v64,
                     TOP_vfdivxx128v64,
                     TOP_vfdivxxx128v64,
                     TOP_vdivxsd,
                     TOP_vdivxxsd,
                     TOP_vdivxxxsd,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(31);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx div II reg opnd",
                     TOP_vfdiv128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(24);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx div II mem opnd",
                     TOP_vfdivx128v32,
                     TOP_vfdivxx128v32,
                     TOP_vfdivxxx128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(28);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx div III reg opnd",
                     TOP_vdivss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(24);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx div III mem opnd",
                     TOP_vdivxss,
                     TOP_vdivxxss,
                     TOP_vdivxxxss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(28);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp dppd reg opnd",
                     TOP_vfdp128v64,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(15);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp dpps reg opnd",
                     TOP_vfdp128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(25);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp mul reg opnd",
                     TOP_vmulhuw,
                     TOP_vmulhrsw,
                     TOP_vmulhw,
                     TOP_vmulld,
                     TOP_vmul128v16,
                     TOP_vmuludq,
                     TOP_vmuldq,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp pmulld reg opnd",
                     TOP_vmul128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp mul mem opnd",
                     TOP_vmulhuwx,
                     TOP_vmulhuwxx,
                     TOP_vmulhuwxxx,
                     TOP_vmulhrswx,
                     TOP_vmulhrswxx,
                     TOP_vmulhrswxxx,
                     TOP_vmulhwx,
                     TOP_vmulhwxx,
                     TOP_vmulhwxxx,
                     TOP_vmulx128v16,
                     TOP_vmulxx128v16,
                     TOP_vmulxxx128v16,
                     TOP_vmuludqx,
                     TOP_vmuludqxx,
                     TOP_vmuludqxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp mul mem opnd 2",
                     TOP_vmulldx,
                     TOP_vmulldxx,
                     TOP_vmulldxxx,
                     TOP_vmuldqx,
                     TOP_vmuldqxx,
                     TOP_vmuldqxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp pmulld mem opnd",
                     TOP_vmulx128v32,
                     TOP_vmulxx128v32,
                     TOP_vmulxxx128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx blend reg opnd",
                        TOP_vfblend128v64,
                        TOP_vfblend128v32,
                        TOP_vfblendv128v64,
                        TOP_vfblendv128v32,
                        TOP_vblendv128v8,
                        TOP_vblend128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx blend mem opnd",
                        TOP_vfblendx128v64,
                        TOP_vfblendxx128v64,
                        TOP_vfblendxxx128v64,
                        TOP_vfblendx128v32,
                        TOP_vfblendxx128v32,
                        TOP_vfblendxxx128v32,
                        TOP_vfblendvx128v64,
                        TOP_vfblendvxx128v64,
                        TOP_vfblendvxxx128v64,
                        TOP_vfblendvx128v32,
                        TOP_vfblendvxx128v32,
                        TOP_vfblendvxxx128v32,
                        TOP_vblendvx128v8,
                        TOP_vblendvxx128v8,
                        TOP_vblendvxxx128v8,
                        TOP_vblendx128v16,
                        TOP_vblendxx128v16,
                        TOP_vblendxxx128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6); 
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx pclmul reg opnd",
                        TOP_vpclmulqdq,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(12);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx pclmul mem opnd",
                        TOP_vpclmulqdqx,
                        TOP_vpclmulqdqxx,
                        TOP_vpclmulqdqxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(16);
  Resource_Requirement(res_fmul, 0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx aes reg opnd",
                        TOP_vaesenc,
                        TOP_vaesenclast,
                        TOP_vaesdec,
                        TOP_vaesdeclast,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7); 
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx aes reg opnd 2",
                        TOP_vaesimc,
                        TOP_vaeskeygenassist,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx vcmp reg opnd",
                        TOP_vcmpeq128v8,
                        TOP_vcmpeq128v32,
                        TOP_vcmpeq128v64,
                        TOP_vcmpeq128v16,
                        TOP_vcmpgt128v8,
                        TOP_vcmpgt128v32,
                        TOP_vcmpgt128v64,
                        TOP_vcmpgt128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx vcmp string reg opnd 1",
                        TOP_vcmpestri,
                        TOP_vcmpestrm,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(30);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx vcmp string reg opnd 2",
                        TOP_vcmpistri,
                        TOP_vcmpistrm,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx aes mem opnd",
                        TOP_vaesencx,
                        TOP_vaesencxx,
                        TOP_vaesencxxx,
                        TOP_vaesenclastx,
                        TOP_vaesenclastxx,
                        TOP_vaesenclastxxx,
                        TOP_vaesdecx,
                        TOP_vaesdecxx,
                        TOP_vaesdecxxx,
                        TOP_vaesdeclastx,
                        TOP_vaesdeclastxx,
                        TOP_vaesdeclastxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(11);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx aes mem opnd 2",
                        TOP_vaesimcx,
                        TOP_vaesimcxx,
                        TOP_vaesimcxxx,
                        TOP_vaeskeygenassistx,
                        TOP_vaeskeygenassistxx,
                        TOP_vaeskeygenassistxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx vcmp mem opnd",
                        TOP_vcmpeqx128v8,
                        TOP_vcmpeqxx128v8,
                        TOP_vcmpeqxxx128v8,
                        TOP_vcmpeqx128v32,
                        TOP_vcmpeqxx128v32,
                        TOP_vcmpeqxxx128v32,
                        TOP_vcmpeqx128v64,
                        TOP_vcmpeqxx128v64,
                        TOP_vcmpeqxxx128v64,
                        TOP_vcmpeqx128v16,
                        TOP_vcmpeqxx128v16,
                        TOP_vcmpeqxxx128v16,
                        TOP_vcmpgtx128v8,
                        TOP_vcmpgtxx128v8,
                        TOP_vcmpgtxxx128v8,
                        TOP_vcmpgtx128v32,
                        TOP_vcmpgtxx128v32,
                        TOP_vcmpgtxxx128v32,
                        TOP_vcmpgtx128v64,
                        TOP_vcmpgtxx128v64,
                        TOP_vcmpgtxxx128v64,
                        TOP_vcmpgtx128v16,
                        TOP_vcmpgtxx128v16,
                        TOP_vcmpgtxxx128v16,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx vcmp string mem opnd 1",
                        TOP_vcmpestrix,
                        TOP_vcmpestrixx,
                        TOP_vcmpestrixxx,
                        TOP_vcmpestrmx,
                        TOP_vcmpestrmxx,
                        TOP_vcmpestrmxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(34);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx vcmp string mem opnd 1",
                        TOP_vcmpistrix,
                        TOP_vcmpistrixx,
                        TOP_vcmpistrixxx,
                        TOP_vcmpistrmx,
                        TOP_vcmpistrmxx,
                        TOP_vcmpistrmxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(14);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp align load",
                        TOP_vfmaskld128v32,
                        TOP_vfmaskldx128v32,
                        TOP_vfmaskldxx128v32,
                        TOP_vfmaskld128v64,
                        TOP_vfmaskldx128v64,
                        TOP_vfmaskldxx128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp align load 2",
                        TOP_vldapd,
                        TOP_vldapdx,
                        TOP_vldapdxx,
                        TOP_vldapd_n32,
                        TOP_vldaps,
                        TOP_vldapsx,
                        TOP_vldapsxx,
                        TOP_vldaps_n32,
                        TOP_vmovx2g,
                        TOP_vmovx2g64,
                        TOP_vld64_2sse,
                        TOP_vldx64_2sse,
                        TOP_vldxx64_2sse,
                        TOP_vld64_2sse_n32,
                        TOP_vldhpd,
                        TOP_vldhpdx,
                        TOP_vldhpdxx,
                        TOP_vldhpd_n32,
                        TOP_vldhps,
                        TOP_vldhpsx,
                        TOP_vldhpsxx,
                        TOP_vldhps_n32,
                        TOP_vldlps,
                        TOP_vldlpsx,
                        TOP_vldlpsxx,
                        TOP_vldlps_n32,
                        TOP_vldntdqa,
                        TOP_vldntdqax,
                        TOP_vldntdqaxx,
                        TOP_vldlpd,
                        TOP_vldlpdx,
                        TOP_vldlpdxx,
                        TOP_vldlpd_n32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx int arith reg opnd",
                        TOP_vextr128v8,
                        TOP_vextr128v32,
                        TOP_vextr128v64,
                        TOP_vextr128v16,
                        TOP_vphminposuw,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith reg opnd 2",
                        TOP_vinsr128v8,
                        TOP_vinsr128v32,
                        TOP_vinsr128v64,
                        TOP_vinsr128v16,
                        TOP_vpslldq,
                        TOP_vpsrldq,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith mem opnd",
                     TOP_vextrx128v8,
                     TOP_vextrxx128v8,
                     TOP_vextrxxx128v8,
                     TOP_vextrx128v32,
                     TOP_vextrxx128v32,
                     TOP_vextrxxx128v32,
                     TOP_vextrx128v64,
                     TOP_vextrxx128v64,
                     TOP_vextrxxx128v64,
                     TOP_vextrx128v16,
                     TOP_vextrxx128v16,
                     TOP_vextrxxx128v16,
                     TOP_vphminposuwx,
                     TOP_vphminposuwxx,
                     TOP_vphminposuwxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx int arith mem opnd 2",
                     TOP_vinsrx128v8,
                     TOP_vinsrxx128v8,
                     TOP_vinsrxxx128v8,
                     TOP_vinsrx128v32,
                     TOP_vinsrxx128v32,
                     TOP_vinsrxxx128v32,
                     TOP_vinsrx128v64,
                     TOP_vinsrxx128v64,
                     TOP_vinsrxxx128v64,
                     TOP_vinsrx128v16,
                     TOP_vinsrxx128v16,
                     TOP_vinsrxxx128v16,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx int arith II reg opnd",
                     TOP_vpalignr128,
                     TOP_vpsign128v8,
                     TOP_vpsign128v32,
                     TOP_vpsign128v16,
                     TOP_vsub128v8,
                     TOP_vsub128v32,
                     TOP_vsub128v64,
                     TOP_vsub128v16,
                     TOP_vsubs128v8,
                     TOP_vsubs128v16,
                     TOP_vsubus128v8,
                     TOP_vsubus128v16,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith II mem opnd",
                     TOP_vpalignrx128,
                     TOP_vpalignrxx128,
                     TOP_vpalignrxxx128,
                     TOP_vpsignx128v8,
                     TOP_vpsignxx128v8,
                     TOP_vpsignxxx128v8,
                     TOP_vpsignx128v32,
                     TOP_vpsignxx128v32,
                     TOP_vpsignxxx128v32,
                     TOP_vpsignx128v16,
                     TOP_vpsignxx128v16,
                     TOP_vpsignxxx128v16,
                     TOP_vsubx128v8,
                     TOP_vsubxx128v8,
                     TOP_vsubxxx128v8,
                     TOP_vsubx128v32,
                     TOP_vsubxx128v32,
                     TOP_vsubxxx128v32,
                     TOP_vsubx128v64,
                     TOP_vsubxx128v64,
                     TOP_vsubxxx128v64,
                     TOP_vsubx128v16,
                     TOP_vsubxx128v16,
                     TOP_vsubxxx128v16,
                     TOP_vsubsx128v8,
                     TOP_vsubsxx128v8,
                     TOP_vsubsxxx128v8,
                     TOP_vsubsx128v16,
                     TOP_vsubsxx128v16,
                     TOP_vsubsxxx128v16,
                     TOP_vsubusx128v8,
                     TOP_vsubusxx128v8,
                     TOP_vsubusxxx128v8,
                     TOP_vsubusx128v16,
                     TOP_vsubusxx128v16,
                     TOP_vsubusxxx128v16,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx int arith III reg opnd",
                     TOP_vpsadbw,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith III mem opnd",
                     TOP_vpsadbwx,
                     TOP_vpsadbwxx,
                     TOP_vpsadbwxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx int arith IV reg opnd",
                     TOP_vpslld,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith IV reg opnd 2",
                     TOP_vpsllq,
                     TOP_vpsllw,
                     TOP_vpsrad,
                     TOP_vpsraw,
                     TOP_vpsrld,
                     TOP_vpsrlq,
                     TOP_vpsrlw,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith IV imm opnd",
                     TOP_vpslldi,
                     TOP_vpsllqi,
                     TOP_vpsllwi,
                     TOP_vpsradi,
                     TOP_vpsrawi,
                     TOP_vpsrldi,
                     TOP_vpsrlqi,
                     TOP_vpsrlwi,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx int arith IV mem opnd",
                     TOP_vpsllqx,
                     TOP_vpsllqxx,
                     TOP_vpsllqxxx,
                     TOP_vpslldx,
                     TOP_vpslldxx,
                     TOP_vpslldxxx,
                     TOP_vpsllwx,
                     TOP_vpsllwxx,
                     TOP_vpsllwxxx,
                     TOP_vpsradx,
                     TOP_vpsradxx,
                     TOP_vpsradxxx,
                     TOP_vpsrawx,
                     TOP_vpsrawxx,
                     TOP_vpsrawxxx,
                     TOP_vpsrldx,
                     TOP_vpsrldxx,
                     TOP_vpsrldxxx,
                     TOP_vpsrlqx,
                     TOP_vpsrlqxx,
                     TOP_vpsrlqxxx,
                     TOP_vpsrlwx,
                     TOP_vpsrlwxx,
                     TOP_vpsrlwxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp vinsertf reg opnd",
                        TOP_vfinsrf128,
                        TOP_vfinsr128v32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp arith reg opnd",
                        TOP_vfextrf128,
                        TOP_vfextr128v32,
                        TOP_vmovddup,
                        TOP_vmovshdup,
                        TOP_vmovsldup,
                        TOP_vmovdqa,
                        TOP_vmovsd,
                        TOP_vmovss,
                        TOP_vmovhlps,
                        TOP_vmovlhps,
                        TOP_vpackssdw,
                        TOP_vpacksswb,
                        TOP_vpackusdw,
                        TOP_vpackuswb,
                        TOP_vfperm128v64,
                        TOP_vfpermi128v64,
                        TOP_vfperm128v32,
                        TOP_vfpermi128v32,
                        TOP_vfperm2f128,
                        TOP_vpmovmskb128,
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
                        TOP_vpunpckh64v8,
                        TOP_vpunpckh64v32,
                        TOP_vpunpckh64v16,
                        TOP_vpunpckh64v64,
                        TOP_vpunpckl64v8,
                        TOP_vpunpckl64v32,
                        TOP_vpunpckl64v16,
                        TOP_vpunpckl64v64,
                        TOP_vshufpd,
                        TOP_vfshuf128v64,
                        TOP_vshufps,
                        TOP_vfshuf128v32,
                        TOP_vunpckh128v64,
                        TOP_vunpckh128v32,
                        TOP_vunpckl128v64,
                        TOP_vunpckl128v32,
                        TOP_vxzero128v64,
                        TOP_vxzero64,
                        TOP_vxorpd,
                        TOP_vfxor128v64,
                        TOP_vxorps,
                        TOP_vfxor128v32,
                        TOP_vxzero128v32,
                        TOP_vxzero32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp arith reg opnd 2",
                        TOP_vfrcp128v32,
                        TOP_vfrcpss,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp arith mem opnd",
                        TOP_vfextrxf128,
                        TOP_vfextrxxf128,
                        TOP_vfextrxxxf128,
                        TOP_vfextrx128v32,
                        TOP_vfextrxx128v32,
                        TOP_vfextrxxx128v32,
                        TOP_vfinsrxf128,
                        TOP_vfinsrxxf128,
                        TOP_vfinsrxxxf128,
                        TOP_vfinsrx128v32,
                        TOP_vfinsrxx128v32,
                        TOP_vfinsrxxx128v32,
                        TOP_vpackssdwx,
                        TOP_vpackssdwxx,
                        TOP_vpackssdwxxx,
                        TOP_vpacksswbx,
                        TOP_vpacksswbxx,
                        TOP_vpacksswbxxx,
                        TOP_vpackusdwx,
                        TOP_vpackusdwxx,
                        TOP_vpackusdwxxx,
                        TOP_vpackuswbx,
                        TOP_vpackuswbxx,
                        TOP_vpackuswbxxx,
                        TOP_vpunpckhx64v8,
                        TOP_vpunpckhxx64v8,
                        TOP_vpunpckhxxx64v8,
                        TOP_vpunpckhx64v32,
                        TOP_vpunpckhxx64v32,
                        TOP_vpunpckhxxx64v32,
                        TOP_vpunpckhx64v16,
                        TOP_vpunpckhxx64v16,
                        TOP_vpunpckhxxx64v16,
                        TOP_vpunpckhx64v64,
                        TOP_vpunpckhxx64v64,
                        TOP_vpunpckhxxx64v64,
                        TOP_vpunpcklx64v8,
                        TOP_vpunpcklxx64v8,
                        TOP_vpunpcklxxx64v8,
                        TOP_vpunpcklx64v32,
                        TOP_vpunpcklxx64v32,
                        TOP_vpunpcklxxx64v32,
                        TOP_vpunpcklx64v16,
                        TOP_vpunpcklxx64v16,
                        TOP_vpunpcklxxx64v16,
                        TOP_vpunpcklx64v64,
                        TOP_vpunpcklxx64v64,
                        TOP_vpunpcklxxx64v64,
                        TOP_vfshufx128v64,
                        TOP_vfshufxx128v64,
                        TOP_vfshufxxx128v64,
                        TOP_vfshufx128v32,
                        TOP_vfshufxx128v32,
                        TOP_vfshufxxx128v32,
                        TOP_vunpckhx128v64,
                        TOP_vunpckhxx128v64,
                        TOP_vunpckhxxx128v64,
                        TOP_vunpckhx128v32,
                        TOP_vunpckhxx128v32,
                        TOP_vunpckhxxx128v32,
                        TOP_vunpcklx128v64,
                        TOP_vunpcklxx128v64,
                        TOP_vunpcklxxx128v64,
                        TOP_vunpcklx128v32,
                        TOP_vunpcklxx128v32,
                        TOP_vunpcklxxx128v32,
                        TOP_vfxorx128v32,
                        TOP_vfxorxx128v32,
                        TOP_vfxorxxx128v32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith mem opnd 2",
                        TOP_vmovddupx,
                        TOP_vmovddupxx,
                        TOP_vmovddupxxx,
                        TOP_vmovshdupx,
                        TOP_vmovshdupxx,
                        TOP_vmovshdupxxx,
                        TOP_vmovsldupx,
                        TOP_vmovsldupxx,
                        TOP_vmovsldupxxx,
                        TOP_vpmovsxbdx,
                        TOP_vpmovsxbdxx,
                        TOP_vpmovsxbdxxx,
                        TOP_vpmovsxbqx,
                        TOP_vpmovsxbqxx,
                        TOP_vpmovsxbqxxx,
                        TOP_vpmovsxbwx,
                        TOP_vpmovsxbwxx,
                        TOP_vpmovsxbwxxx,
                        TOP_vpmovsxdqx,
                        TOP_vpmovsxdqxx,
                        TOP_vpmovsxdqxxx,
                        TOP_vpmovsxwdx,
                        TOP_vpmovsxwdxx,
                        TOP_vpmovsxwdxxx,
                        TOP_vpmovsxwqx,
                        TOP_vpmovsxwqxx,
                        TOP_vpmovsxwqxxx,
                        TOP_vpmovzxbdx,
                        TOP_vpmovzxbdxx,
                        TOP_vpmovzxbdxxx,
                        TOP_vpmovzxbqx,
                        TOP_vpmovzxbqxx,
                        TOP_vpmovzxbqxxx,
                        TOP_vpmovzxbwx,
                        TOP_vpmovzxbwxx,
                        TOP_vpmovzxbwxxx,
                        TOP_vpmovzxdqx,
                        TOP_vpmovzxdqxx,
                        TOP_vpmovzxdqxxx,
                        TOP_vpmovzxwdx,
                        TOP_vpmovzxwdxx,
                        TOP_vpmovzxwdxxx,
                        TOP_vpmovzxwqx,
                        TOP_vpmovzxwqxx,
                        TOP_vpmovzxwqxxx,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith mem opnd 3",
                        TOP_vfpermx128v64, 
                        TOP_vfpermxx128v64,
                        TOP_vfpermxxx128v64,
                        TOP_vfpermix128v64,
                        TOP_vfpermixx128v64,
                        TOP_vfpermixxx128v64,
                        TOP_vfpermx128v32,
                        TOP_vfpermxx128v32,
                        TOP_vfpermxxx128v32,
                        TOP_vfpermix128v32,
                        TOP_vfpermixx128v32,
                        TOP_vfpermixxx128v32,
                        TOP_vfperm2xf128,
                        TOP_vfperm2xxf128,
                        TOP_vfperm2xxxf128,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith mem opnd 4",
                        TOP_vfrcpx128v32,
                        TOP_vfrcpxx128v32,
                        TOP_vfrcpxxx128v32,
                        TOP_vfrcpxss,
                        TOP_vfrcpxxss,
                        TOP_vfrcpxxxss,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith II  reg opnd",
                     TOP_vphadd128v32,
                     TOP_vphadds128v16,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp arith II  mem opnd",
                     TOP_vphaddx128v32,
                     TOP_vphaddxx128v32,
                     TOP_vphaddxxx128v32,
                     TOP_vphaddsx128v16,
                     TOP_vphaddsxx128v16,
                     TOP_vphaddsxxx128v16,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp compare scalar ordered reg opnd",
                     TOP_vcomisd,
                     TOP_vcomiss,
                     TOP_vucomisd,
                     TOP_vucomiss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp compare scalar ordered mem opnd",
                     TOP_vcomixsd,
                     TOP_vcomixxsd,
                     TOP_vcomixxxsd,
                     TOP_vcomixss,
                     TOP_vcomixxss,
                     TOP_vcomixxxss,
                     TOP_vucomixsd,
                     TOP_vucomixxsd,
                     TOP_vucomixxxsd,
                     TOP_vucomixss,
                     TOP_vucomixxss,
                     TOP_vucomixxxss,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp align store",
                        TOP_vmaskmovdqu,
                        TOP_vfmaskst128v32,
                        TOP_vfmaskstx128v32,
                        TOP_vfmaskstxx128v32,
                        TOP_vfmaskst128v64,
                        TOP_vfmaskstx128v64,
                        TOP_vfmaskstxx128v64,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp align store 2",
                        TOP_vstapd,
                        TOP_vstapdx,
                        TOP_vstapdxx,
                        TOP_vstapd_n32,
                        TOP_vstaps,
                        TOP_vstapsx,
                        TOP_vstapsxx,
                        TOP_vstaps_n32,
                        TOP_vmovg2x,
                        TOP_vmovg2x64,
                        TOP_vst64_fsse,
                        TOP_vstx64_fsse,
                        TOP_vstxx64_fsse,
                        TOP_vst64_fsse_n32,
                        TOP_vstdqa,
                        TOP_vstdqax,
                        TOP_vstdqaxx,
                        TOP_vstdqa_n32,
                        TOP_vstdqu,
                        TOP_vstdqux,
                        TOP_vstdquxx,
                        TOP_vstdqu_n32,
                        TOP_vsthpd,
                        TOP_vsthpdx,
                        TOP_vsthpdxx,
                        TOP_vsthpd_n32,
                        TOP_vsthps,
                        TOP_vsthpsx,
                        TOP_vsthpsxx,
                        TOP_vsthps_n32,
                        TOP_vstlps,
                        TOP_vstlpsx,
                        TOP_vstlpsxx,
                        TOP_vstlps_n32,
                        TOP_vstorenti128,
                        TOP_vstorentxi128,
                        TOP_vstorentxxi128,
                        TOP_vstntdq,
                        TOP_vstntdqx,
                        TOP_vstntdqxx,
                        TOP_vstntpd,
                        TOP_vstntpdx,
                        TOP_vstntpdxx,
                        TOP_vstntps,
                        TOP_vstntpsx,
                        TOP_vstntpsxx,
                        TOP_vstntsd,
                        TOP_vstntsdx,
                        TOP_vstntsdxx,
                        TOP_vstntss,
                        TOP_vstntssx,
                        TOP_vstntssxx,
                        TOP_vstorelpd,
                        TOP_vstlpd,
                        TOP_vstlpdx,
                        TOP_vstlpdxx,
                        TOP_vstlpd_n32,
                        TOP_vstsd,
                        TOP_vstsdx,
                        TOP_vstsdxx,
                        TOP_vstsd_n32,
                        TOP_vstss,
                        TOP_vstssx,
                        TOP_vstssxx,
                        TOP_vstss_n32,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);
  Store_Available_Time(4);

  Instruction_Group( "avx broadcast reg opnd",
                        TOP_vfbroadcastss,
                        TOP_vfbroadcastxss,
                        TOP_vfbroadcastxxss,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx broadcast reg opnd 2",
                        TOP_vfbroadcastsd,
                        TOP_vfbroadcastxsd,
                        TOP_vfbroadcastxxsd,
                        TOP_vfbroadcastf128,
                        TOP_vfbroadcastxf128,
                        TOP_vfbroadcastxxf128,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp mpsadbw reg opnd",
                     TOP_vmpsadbw,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx fp mul reg opnd",
                     TOP_vfmul128v64,
                     TOP_vfmul128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fmul, 0);

  Instruction_Group( "avx fp mul mem opnd",
                     TOP_vmpsadbwx,
                     TOP_vmpsadbwxx,
                     TOP_vmpsadbwxxx,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx fp mul mem opnd",
                     TOP_vfmulx128v64,
                     TOP_vfmulxx128v64,
                     TOP_vfmulxxx128v64,
                     TOP_vfmulx128v32,
                     TOP_vfmulxx128v32,
                     TOP_vfmulxxx128v32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);
  Resource_Requirement(res_loadstore, 0);

  Instruction_Group( "avx ptest reg opnd",
                     TOP_vptest128,
                     TOP_vtestpd,
                     TOP_vtestps,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);

  Instruction_Group( "avx ptest mem opnd",
                     TOP_vptestx128,
                     TOP_vptestxx128,
                     TOP_vptestxxx128,
                     TOP_vtestxpd,
                     TOP_vtestxxpd,
                     TOP_vtestxxxpd,
                     TOP_vtestxps,
                     TOP_vtestxxps,
                     TOP_vtestxxxps,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx load mxcsr",
                     TOP_vldmxcsr,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx store mxcsr",
                     TOP_vstmxcsr,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx move mask",
                        TOP_vmovmskpd,
                        TOP_vmovmskps,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_alu, 0);

  Instruction_Group( "avx unalign store",
                     TOP_vstupdx,
                     TOP_vstupdxx,
                     TOP_vstupd_n32,
                     TOP_vstupsx,
                     TOP_vstupsxx,
                     TOP_vstups_n32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  Instruction_Group( "avx unalign load",
                     TOP_vlddqu,
                     TOP_vlddqux,
                     TOP_vlddquxx,
                     TOP_vlddqu_n32,
                     TOP_vldupdx,
                     TOP_vldupdxx,
                     TOP_vldupd_n32,
                     TOP_vldupsx,
                     TOP_vldupsxx,
                     TOP_vldups_n32,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);
  Resource_Requirement(res_loadstore, 0);
  Load_Access_Time(4);

  Instruction_Group( "avx unalign reg opnd transfer",
                     TOP_vstupd,
                     TOP_vstups,
                     TOP_vldupd,
                     TOP_vldups,
                     TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fstore, 0);

  /* INTEL FMA instructions */
  Instruction_Group( "intel avx fma reg opnd",
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
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5); 
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group( "intel avx fma mem opnd",
                        TOP_xfmadd132xpd,
                        TOP_xfmadd132xxpd,
                        TOP_xfmadd132xxxpd,
                        TOP_xfmadd213xpd,
                        TOP_xfmadd213xxpd,
                        TOP_xfmadd213xxxpd,
                        TOP_xfmadd231xpd,
                        TOP_xfmadd231xxpd,
                        TOP_xfmadd231xxxpd,
                        TOP_xfmadd132xps,
                        TOP_xfmadd132xxps,
                        TOP_xfmadd132xxxps,
                        TOP_xfmadd213xps,
                        TOP_xfmadd213xxps,
                        TOP_xfmadd213xxxps,
                        TOP_xfmadd231xps,
                        TOP_xfmadd231xxps,
                        TOP_xfmadd231xxxps,
                        TOP_xfmadd132xsd,
                        TOP_xfmadd132xxsd,
                        TOP_xfmadd132xxxsd,
                        TOP_xfmadd213xsd,
                        TOP_xfmadd213xxsd,
                        TOP_xfmadd213xxxsd,
                        TOP_xfmadd231xsd,
                        TOP_xfmadd231xxsd,
                        TOP_xfmadd231xxxsd,
                        TOP_xfmadd132xss,
                        TOP_xfmadd132xxss,
                        TOP_xfmadd132xxxss,
                        TOP_xfmadd213xss,
                        TOP_xfmadd213xxss,
                        TOP_xfmadd213xxxss,
                        TOP_xfmadd231xss,
                        TOP_xfmadd231xxss,
                        TOP_xfmadd231xxxss,
                        TOP_xfmaddsub132xpd,
                        TOP_xfmaddsub132xxpd,
                        TOP_xfmaddsub132xxxpd,
                        TOP_xfmaddsub213xpd,
                        TOP_xfmaddsub213xxpd,
                        TOP_xfmaddsub213xxxpd,
                        TOP_xfmaddsub231xpd,
                        TOP_xfmaddsub231xxpd,
                        TOP_xfmaddsub231xxxpd,
                        TOP_xfmaddsub132xps,
                        TOP_xfmaddsub132xxps,
                        TOP_xfmaddsub132xxxps,
                        TOP_xfmaddsub213xps,
                        TOP_xfmaddsub213xxps,
                        TOP_xfmaddsub213xxxps,
                        TOP_xfmaddsub231xps,
                        TOP_xfmaddsub231xxps,
                        TOP_xfmaddsub231xxxps,
                        TOP_xfmsubadd132xpd,
                        TOP_xfmsubadd132xxpd,
                        TOP_xfmsubadd132xxxpd,
                        TOP_xfmsubadd213xpd,
                        TOP_xfmsubadd213xxpd,
                        TOP_xfmsubadd213xxxpd,
                        TOP_xfmsubadd231xpd,
                        TOP_xfmsubadd231xxpd,
                        TOP_xfmsubadd231xxxpd,
                        TOP_xfmsubadd132xps,
                        TOP_xfmsubadd132xxps,
                        TOP_xfmsubadd132xxxps,
                        TOP_xfmsubadd213xps,
                        TOP_xfmsubadd213xxps,
                        TOP_xfmsubadd213xxxps,
                        TOP_xfmsubadd231xps,
                        TOP_xfmsubadd231xxps,
                        TOP_xfmsubadd231xxxps,
                        TOP_xfmsub132xpd,
                        TOP_xfmsub132xxpd,
                        TOP_xfmsub132xxxpd,
                        TOP_xfmsub213xpd,
                        TOP_xfmsub213xxpd,
                        TOP_xfmsub213xxxpd,
                        TOP_xfmsub231xpd,
                        TOP_xfmsub231xxpd,
                        TOP_xfmsub231xxxpd,
                        TOP_xfmsub132xps,
                        TOP_xfmsub132xxps,
                        TOP_xfmsub132xxxps,
                        TOP_xfmsub213xps,
                        TOP_xfmsub213xxps,
                        TOP_xfmsub213xxxps,
                        TOP_xfmsub231xps,
                        TOP_xfmsub231xxps,
                        TOP_xfmsub231xxxps,
                        TOP_xfmsub132xsd,
                        TOP_xfmsub132xxsd,
                        TOP_xfmsub132xxxsd,
                        TOP_xfmsub213xsd,
                        TOP_xfmsub213xxsd,
                        TOP_xfmsub213xxxsd,
                        TOP_xfmsub231xsd,
                        TOP_xfmsub231xxsd,
                        TOP_xfmsub231xxxsd,
                        TOP_xfmsub132xss,
                        TOP_xfmsub132xxss,
                        TOP_xfmsub132xxxss,
                        TOP_xfmsub213xss,
                        TOP_xfmsub213xxss,
                        TOP_xfmsub213xxxss,
                        TOP_xfmsub231xss,
                        TOP_xfmsub231xxss,
                        TOP_xfmsub231xxxss,
                        TOP_xfnmadd132xpd,
                        TOP_xfnmadd132xxpd,
                        TOP_xfnmadd132xxxpd,
                        TOP_xfnmadd213xpd,
                        TOP_xfnmadd213xxpd,
                        TOP_xfnmadd213xxxpd,
                        TOP_xfnmadd231xpd,
                        TOP_xfnmadd231xxpd,
                        TOP_xfnmadd231xxxpd,
                        TOP_xfnmadd132xps,
                        TOP_xfnmadd132xxps,
                        TOP_xfnmadd132xxxps,
                        TOP_xfnmadd213xps,
                        TOP_xfnmadd213xxps,
                        TOP_xfnmadd213xxxps,
                        TOP_xfnmadd231xps,
                        TOP_xfnmadd231xxps,
                        TOP_xfnmadd231xxxps,
                        TOP_xfnmadd132xsd,
                        TOP_xfnmadd132xxsd,
                        TOP_xfnmadd132xxxsd,
                        TOP_xfnmadd213xsd,
                        TOP_xfnmadd213xxsd,
                        TOP_xfnmadd213xxxsd,
                        TOP_xfnmadd231xsd,
                        TOP_xfnmadd231xxsd,
                        TOP_xfnmadd231xxxsd,
                        TOP_xfnmadd132xss,
                        TOP_xfnmadd132xxss,
                        TOP_xfnmadd132xxxss,
                        TOP_xfnmadd213xss,
                        TOP_xfnmadd213xxss,
                        TOP_xfnmadd213xxxss,
                        TOP_xfnmadd231xss,
                        TOP_xfnmadd231xxss,
                        TOP_xfnmadd231xxxss,
                        TOP_xfnmsub132xpd,
                        TOP_xfnmsub132xxpd,
                        TOP_xfnmsub132xxxpd,
                        TOP_xfnmsub213xpd,
                        TOP_xfnmsub213xxpd,
                        TOP_xfnmsub213xxxpd,
                        TOP_xfnmsub231xpd,
                        TOP_xfnmsub231xxpd,
                        TOP_xfnmsub231xxxpd,
                        TOP_xfnmsub132xps,
                        TOP_xfnmsub132xxps,
                        TOP_xfnmsub132xxxps,
                        TOP_xfnmsub213xps,
                        TOP_xfnmsub213xxps,
                        TOP_xfnmsub213xxxps,
                        TOP_xfnmsub231xps,
                        TOP_xfnmsub231xxps,
                        TOP_xfnmsub231xxxps,
                        TOP_xfnmsub132xsd,
                        TOP_xfnmsub132xxsd,
                        TOP_xfnmsub132xxxsd,
                        TOP_xfnmsub213xsd,
                        TOP_xfnmsub213xxsd,
                        TOP_xfnmsub213xxxsd,
                        TOP_xfnmsub231xsd,
                        TOP_xfnmsub231xxsd,
                        TOP_xfnmsub231xxxsd,
                        TOP_xfnmsub132xss,
                        TOP_xfnmsub132xxss,
                        TOP_xfnmsub132xxxss,
                        TOP_xfnmsub213xss,
                        TOP_xfnmsub213xxss,
                        TOP_xfnmsub213xxxss,
                        TOP_xfnmsub231xss,
                        TOP_xfnmsub231xxss,
                        TOP_xfnmsub231xxxss,
                        TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_fadd, 0);

  Instruction_Group("dummy",
		    TOP_asm,
		    TOP_intrncall,
		    TOP_spadjust,
		    TOP_savexmms,
                    TOP_checkptr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_alu, 0);

  Machine_Done();
}
