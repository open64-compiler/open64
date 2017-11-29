/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

//  MIPS processor scheduling information
/////////////////////////////////////
//  
//  Description:
//
//  Generate a scheduling description of a MIPS processor
//  via the si_gen interface.
//
/////////////////////////////////////


//  $Revision: 1.12 $
//  $Date: 2009/05/05 08:27:55 $
//  $Author: ShenRuifen $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/proc/MIPS/r10000_si.cxx,v $


#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_issue,
                res_memory,   // Memory unit 
                res_integer,  // Integer unit
                res_spe,      // special register related unit
                res_mac,
                res_ffe;

void Generate_SL5 (void)
{
  Machine("sl5", ISA_SUBSET_SL5);

  res_issue = RESOURCE_Create("issue", 1);
  res_memory = RESOURCE_Create("memory", 1);
  res_integer = RESOURCE_Create("integer", 1);

  Instruction_Group("movespe",
      TOP_mvtc,
      TOP_mvtc_i,
      TOP_mvtc16,
      TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue,0);
  Resource_Requirement(res_integer, 0); 

  Instruction_Group("default",
      TOP_add,
      TOP_addi,
      TOP_addiu,
      TOP_addu,
      TOP_slt,
      TOP_slti,
      TOP_sltiu,
      TOP_sltu,
      TOP_sub,
      TOP_subu,
      TOP_dadd,
      TOP_daddi,
      TOP_daddiu,
      TOP_daddu,
      TOP_dsub,
      TOP_dsubu,
      TOP_and,
      TOP_andi,
      TOP_lui,
      TOP_nor,
      TOP_or,
      TOP_ori,
      TOP_xor,
      TOP_xori,
      TOP_mfhi,
      TOP_mflo,
      TOP_mthi,
      TOP_mtlo,
      TOP_smult,
      TOP_mc_abs,
      TOP_mc_zc_eq,
      TOP_mc_zc_ne,
      TOP_mc_zc_gt,
      TOP_mc_zc_ge,
      TOP_mc_zc_lt,
      TOP_mc_zc_le,
      TOP_mc_z_eq,
      TOP_mc_z_ne,
      TOP_mc_z_gt,
      TOP_mc_z_ge,
      TOP_mc_z_lt,
      TOP_mc_z_le,
      TOP_mc_zn_eq,
      TOP_mc_zn_ne,
      TOP_mc_zn_gt,
      TOP_mc_zn_ge,
      TOP_mc_zn_lt,
      TOP_mc_zn_le,
      TOP_mc_r_eq,
      TOP_mc_r_ne,
      TOP_mc_r_gt,
      TOP_mc_r_ge,
      TOP_mc_r_lt,
      TOP_mc_r_le,
      TOP_depb,
      TOP_extrbs,
      TOP_extrbu,				
      TOP_mvfc,
      TOP_abs16,
      TOP_add16,
      TOP_add16_i,
      TOP_add16_sp,
      TOP_and16,
      TOP_and16_i,
      TOP_mv16,
      TOP_mv16_i,
      TOP_mvfc16,
      TOP_inv16,
      TOP_or16,
      TOP_or16_i,
      TOP_shll16,
      TOP_shll16_i,
      TOP_shra16,
      TOP_shra16_i,
      TOP_shrl16,
      TOP_shrl16_i,
      TOP_sub16,
      TOP_sub16_i,
      TOP_xor16,
      TOP_xor16_i,
      TOP_movf,
      TOP_movn,
      TOP_movt,
      TOP_movz,
      TOP_sll,
      TOP_sllv,
      TOP_sra,
      TOP_srav,
      TOP_srl,
      TOP_srlv,
      TOP_dsll,
      TOP_dsll32,
      TOP_dsllv,
      TOP_dsra,
      TOP_dsra32,
      TOP_dsrav,
      TOP_dsrl,
      TOP_dsrl32,
      TOP_dsrlv,
      TOP_teq,
      TOP_teqi,
      TOP_tge,
      TOP_tgei,
      TOP_tgeiu,
      TOP_tgeu,
      TOP_tlt,
      TOP_tlti,
      TOP_tltiu,
      TOP_tltu,
      TOP_tne,
      TOP_tnei,
      TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer, 0); 

  Instruction_Group("load",
      TOP_lb,
      TOP_lbu,
      TOP_lh,
      TOP_lhu,
      TOP_lw,
      TOP_lwl,
      TOP_lwr,
      TOP_lwu,
      TOP_ld,
      TOP_ldl,
      TOP_ldr,
      TOP_ll,
      TOP_lld,
      TOP_lwc1,
      TOP_ldc1,
      TOP_ldw16,
      TOP_pop16,
      TOP_ldub16_rs,
      TOP_lduh16_rs,
      TOP_lwxc1,
      TOP_ldxc1,
      TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_memory, 0); 
  Load_Access_Time(1);

  Instruction_Group("store",
      TOP_sb,
      TOP_sh,
      TOP_sw,
      TOP_swl,
      TOP_swr,
      TOP_sc,
      TOP_sd,
      TOP_sdl,
      TOP_sdr,
      TOP_scd,
      TOP_swc1,
      TOP_sdc1,
      TOP_stw16,
      TOP_push16,
      TOP_swxc1,
      TOP_sdxc1,
      TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_memory, 0); 
  Resource_Requirement(res_issue, 0);
  Store_Available_Time(2);

  Instruction_Group("branch",
      TOP_beq,
      TOP_bgez,
      TOP_bgezal,
      TOP_bgtz,
      TOP_blez,
      TOP_bltz,
      TOP_bltzal,
      TOP_bne,
      TOP_j,
      TOP_jal,
      TOP_jalr,
      TOP_jr,
      TOP_syscall,
      TOP_br16_eqz,
      TOP_br16_nez,
      TOP_jr16,
      TOP_jr16_lnk,
      TOP_ret,
      TOP_ret16,
      TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer, 0); 

  Instruction_Group("nop",
      TOP_nop,
      TOP_nop16,
      TOP_break,
      TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("sync",
		    TOP_sync,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer, 0);   
  Resource_Requirement(res_memory, 0);

  Instruction_Group("prefetch",
		    TOP_pref,
		    TOP_prefx,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer, 0);   
  Resource_Requirement(res_memory, 0);


  Instruction_Group("dummy",
      TOP_asm,
      TOP_intrncall,
      TOP_spadjust,
      TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer, 0);   

  Machine_Done();
}
