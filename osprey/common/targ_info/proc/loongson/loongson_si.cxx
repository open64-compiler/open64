/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//  Intel Itanium (tm) processor scheduling information
/////////////////////////////////////
//  
//  Description:
//
//  Generate a scheduling description of a Intel Itanium (tm) processor
//  via the si_gen interface.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 2006/10/14 02:42:06 $
//  $Author: ylduan $
//  $Source: /home/cvsroot/godson/godson2_n32/src/osprey1.0/common/targ_info/proc/godson2/itanium_si.cxx,v $


#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_issue,
		res_sem,	// for non-pipelinable semaphore insts
                res_memory,	// Memory unit 
                res_M0,		// M0 Memory unit
                res_float,	// Float unit
                res_F0,		// F0 Float unit
                res_integer,	// Integer unit
                res_I0,		// I0 unit
                res_int_or_mem,	// to simulate A-type insts which can execute
				// in either I or M unit
                res_branch,	// Branch unit
		res_B0_or_B1,	// B0 or B1 unit
		res_B0_or_B2,	// B0 or B2 unit
		res_B2;		// B2 unit

void Generate_Loongson (void)
{
  Machine("loongson", ISA_SUBSET_godson);

  res_issue = RESOURCE_Create("issue", 6);
  res_sem = RESOURCE_Create("sem", 1);
  res_int_or_mem = RESOURCE_Create("integer_or_memory", 4);
  res_memory = RESOURCE_Create("memory", 2);
  res_M0 = RESOURCE_Create("memory0", 1);
  res_float = RESOURCE_Create("floating-point", 2);
  res_F0 = RESOURCE_Create("floating-point0", 1);
  res_integer = RESOURCE_Create("integer", 2);
  res_I0 = RESOURCE_Create("integer0", 1);
  res_branch = RESOURCE_Create("branch", 3);
  res_B0_or_B1 = RESOURCE_Create("B0_or_B1", 2);
  res_B0_or_B2 = RESOURCE_Create("B0_or_B2", 2);
  res_B2 = RESOURCE_Create("B2", 1);

  ////////////////////////////////////////////////////
  Instruction_Group("dummy",
		    TOP_asm,
		    TOP_intrncall,
		    TOP_spadjust,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSIALU",
		    TOP_add,
		    TOP_addi,
		    TOP_addiu,
		    TOP_addu,
		    TOP_and,
		    TOP_andi,
		    TOP_beq,
		    TOP_bgez,
		    TOP_bgezal,
		    TOP_bgtz,
		    TOP_blez,
		    TOP_bltz,
		    TOP_bltzal,
		    TOP_bne,
		    TOP_dadd,
		    TOP_daddi,
		    TOP_daddiu,
		    TOP_daddu,
		    TOP_dsll,
		    TOP_dsllv,
		    TOP_dsra,
		    TOP_dsra32,
		    TOP_dsrav,
		    TOP_dsrl,
		    TOP_dsrl32,
		    TOP_dsrlv,
		    TOP_dsub,
		    TOP_dsubu,
		    TOP_j,
		    TOP_jal,
		    TOP_jalr,
		    TOP_jr,
		    TOP_lui,
		    TOP_mfhi,
		    TOP_mflo,
		    TOP_mthi,
		    TOP_mtlo,
		    TOP_nor,
		    TOP_or,
		    TOP_ori,
		    TOP_sll,
		    TOP_sllv,
		    TOP_slt,
		    TOP_slti,
		    TOP_sltiu,
		    TOP_sltu,
		    TOP_sra,
		    TOP_srav,
		    TOP_srl,
		    TOP_srlv,
		    TOP_sub,
		    TOP_subu,
		    TOP_xor,
		    TOP_xori,
		    TOP_break,
		    TOP_nop,
		    TOP_dsll32,
		    TOP_teq,
		    TOP_teqi,
		    TOP_tge,
		    TOP_tgei,
		    TOP_tgeiu,
		    TOP_tgeu,
		    TOP_movz,
		    TOP_movn,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSIMUL",
		    TOP_dmult,
		    TOP_dmultu,
		    TOP_mult,
		    TOP_multu,
		    TOP_mult_g,
		    TOP_multu_g,
		    TOP_dmult_g,
		    TOP_dmultu_g,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSIDIV",
		    TOP_ddiv,
		    TOP_ddivu,
		    TOP_div,
		    TOP_divu,
		    TOP_divlo,
		    TOP_divhi,
		    TOP_divulo,
		    TOP_divuhi,
		    TOP_div_g,
		    TOP_divu_g,
		    TOP_ddiv_g,
		    TOP_ddivu_g,
		    TOP_mod_g,
		    TOP_modu_g,
		    TOP_dmod_g,
		    TOP_dmodu_g,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(33);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSFMOV",
		    TOP_mov_s,
		    TOP_mov_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSFALU",
		    TOP_add_s,
		    TOP_ceil_l_s,
		    TOP_ceil_w_s,
		    TOP_cvt_l_s,
		    TOP_cvt_s_w,
		    TOP_cvt_s_l,
		    TOP_cvt_w_s,
		    TOP_floor_l_s,
		    TOP_floor_w_s,
		    TOP_round_l_s,
		    TOP_round_w_s,
		    TOP_sub_s,
		    TOP_trunc_l_s,
		    TOP_trunc_w_s,
		    TOP_add_d,
		    TOP_ceil_l_d,
		    TOP_ceil_w_d,
		    TOP_cvt_l_d,
		    TOP_cvt_w_d,
		    TOP_floor_l_d,
		    TOP_floor_w_d,
		    TOP_round_l_d,
		    TOP_round_w_d,
		    TOP_sub_d,
		    TOP_trunc_l_d,
		    TOP_trunc_w_d,
		    TOP_cvt_d_w,
		    TOP_cvt_d_l,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(3);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSFSALU",
		    TOP_cvt_s_d,
		    TOP_cvt_d_s,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSFMUL",
		    TOP_mul_s,
		    TOP_mul_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSFSDIV",
		    TOP_div_s,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSFDDIV",
		    TOP_div_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(17);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSMEM",
		    TOP_lb,
		    TOP_lbu,
		    TOP_ld,
		    TOP_ldl,
		    TOP_ldr,
		    TOP_lh,
		    TOP_lhu,
		    TOP_ll,
		    TOP_lld,
		    TOP_lw,
		    TOP_lwl,
		    TOP_lwr,
		    TOP_lwu,
		    TOP_sb,
		    TOP_sc,
		    TOP_scd,
		    TOP_sd,
		    TOP_sdl,
		    TOP_sdr,
		    TOP_sh,
		    TOP_sw,
		    TOP_swl,
		    TOP_swr,
		    TOP_cfc1,
		    TOP_ctc1,
		    TOP_dmfc1,
		    TOP_dmtc1,
		    TOP_lwc1,
		    TOP_mfc1,
		    TOP_mtc1,
		    TOP_swc1,
		    TOP_ldc1,
		    TOP_sdc1,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSCP0",
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSSQRT_S",
		    TOP_sqrt_s,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(16);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSSQRT_D",
		    TOP_sqrt_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(31);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSFMISC",
		    TOP_abs_s,
		    TOP_bc1f,
		    TOP_bc1fl,
		    TOP_bc1t,
		    TOP_bc1tl,
		    TOP_c_t_s,
		    TOP_c_or_s,
		    TOP_c_neq_s,
		    TOP_c_olg_s,
		    TOP_c_uge_s,
		    TOP_c_oge_s,
		    TOP_c_ugt_s,
		    TOP_c_ogt_s,
		    TOP_c_st_s,
		    TOP_c_gle_s,
		    TOP_c_sne_s,
		    TOP_c_gl_s,
		    TOP_c_nlt_s,
		    TOP_c_ge_s,
		    TOP_c_nle_s,
		    TOP_c_gt_s,
		    TOP_c_f_s,
		    TOP_c_un_s,
		    TOP_c_eq_s,
		    TOP_c_ueq_s,
		    TOP_c_olt_s,
		    TOP_c_ult_s,
		    TOP_c_ole_s,
		    TOP_c_ule_s,
		    TOP_c_sf_s,
		    TOP_c_ngle_s,
		    TOP_c_seq_s,
		    TOP_c_ngl_s,
		    TOP_c_lt_s,
		    TOP_c_nge_s,
		    TOP_c_le_s,
		    TOP_c_ngt_s,
		    TOP_neg_s,
		    TOP_abs_d,
		    TOP_c_t_d,
		    TOP_c_or_d,
		    TOP_c_neq_d,
		    TOP_c_olg_d,
		    TOP_c_uge_d,
		    TOP_c_oge_d,
		    TOP_c_ugt_d,
		    TOP_c_ogt_d,
		    TOP_c_st_d,
		    TOP_c_gle_d,
		    TOP_c_sne_d,
		    TOP_c_gl_d,
		    TOP_c_nlt_d,
		    TOP_c_ge_d,
		    TOP_c_nle_d,
		    TOP_c_gt_d,
		    TOP_c_f_d,
		    TOP_c_un_d,
		    TOP_c_eq_d,
		    TOP_c_ueq_d,
		    TOP_c_olt_d,
		    TOP_c_ult_d,
		    TOP_c_ole_d,
		    TOP_c_ule_d,
		    TOP_c_sf_d,
		    TOP_c_ngle_d,
		    TOP_c_seq_d,
		    TOP_c_ngl_d,
		    TOP_c_lt_d,
		    TOP_c_nge_d,
		    TOP_c_le_d,
		    TOP_c_ngt_d,
		    TOP_neg_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0);

  ////////////////////////////////////////////////////
  Instruction_Group("MIPSMADD",
		    TOP_madd_d,
		    TOP_madd_s,
		    TOP_msub_d,
		    TOP_msub_s,
		    TOP_nmadd_d,
		    TOP_nmadd_s,
		    TOP_nmsub_d,
		    TOP_nmsub_s,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  
  Machine_Done();
}

