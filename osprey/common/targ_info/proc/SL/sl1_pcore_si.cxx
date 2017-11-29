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
//  $Date: 2006/04/28 08:27:55 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/proc/MIPS/r10000_si.cxx,v $


#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_issue,
                res_memory,	// Memory unit 
                res_integer,	// Integer unit
                res_spe,            // special register related unit
                res_mac,
                res_ffe,
  res_integer_or_memory;

void Generate_SL1_PCore (void)
{
  Machine("sl1_pcore", ISA_SUBSET_SL1);

  res_issue = RESOURCE_Create("issue", 2);
  res_memory = RESOURCE_Create("memory", 1);
  res_integer = RESOURCE_Create("integer", 1);

  res_mac = RESOURCE_Create("res_mac", 1);
  res_ffe = RESOURCE_Create("res_ffe", 1);
  res_integer_or_memory = RESOURCE_Create("res_int_or_mem", 1); 

#if defined(TARG_SL)
  Instruction_Group("c3-alu", 
                    // new c3
                    TOP_c3_bitr,
                    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer_or_memory, 0);
  Alternative_Resource_Requirement(res_integer, 0);
  Alternative_Resource_Requirement(res_issue, 0);

  Instruction_Group("c3-mvts",
                    // new c3
                    TOP_c3_mvts,
                    TOP_c3_mvtacc,
                    TOP_c3_mvtaddr,
                    TOP_c3_mvtadds,
                    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_mac, 0);
 
  Instruction_Group("c3-mac",
                    // new c3
	            TOP_c3_mvfs,
                    TOP_c3_mvfacc,
                    TOP_c3_mvfaddr,
                    TOP_c3_mvfadds,
                    TOP_c3_aadda,
                    TOP_c3_nega,
                    TOP_c3_cs,
                    TOP_c3_dadd,
                    TOP_c3_dmac,
                    TOP_c3_dmacn,
                    TOP_c3_dmac_a,
                    TOP_c3_dmacn_a,
                    TOP_c3_dmula,
                    TOP_c3_dmulan,
                    TOP_c3_dmula_a,
                    TOP_c3_dmulan_a,
                    TOP_c3_dshll_i,
                    TOP_c3_dshrl_i,
                    TOP_c3_dsub,
                    TOP_c3_lead,
                    TOP_c3_mac,
                    TOP_c3_macn,
                    TOP_c3_mac_a,
                    TOP_c3_macn_a,
                    TOP_c3_mac_ar,
                    TOP_c3_macn_ar,
                    TOP_c3_mac_i,
                    TOP_c3_macn_i,
                    TOP_c3_mula,
                    TOP_c3_mulan,
                    TOP_c3_mula_a,
                    TOP_c3_mula_ar,
                    TOP_c3_mula_i,
                    TOP_c3_muls,
                    TOP_c3_mulus,
                    TOP_c3_revb,
                    TOP_c3_round,
                    TOP_c3_saadd_a,
                    TOP_c3_saaddh_a,
                    TOP_c3_saadds,
                    TOP_c3_saaddsh,
                    TOP_c3_sadda,
                    TOP_c3_sadda_a,
                    TOP_c3_samulh_a,
                    TOP_c3_samulsh,
                    TOP_c3_sasub_a,
                    TOP_c3_sasubh_a,
                    TOP_c3_sasubs,
                    TOP_c3_sasubsh,
                    TOP_c3_shav,
                    TOP_c3_shlafa_i,
                    TOP_c3_shlata_i,
                    TOP_c3_shla_i,
                    TOP_c3_shrafa_i,
                    TOP_c3_shrata_i,
                    TOP_c3_shra_i,
                    TOP_c3_subc,
                    TOP_UNDEFINED);
 Any_Operand_Access_Time(0);
 Any_Result_Available_Time(2);
 Resource_Requirement(res_issue, 0);
 Resource_Requirement(res_mac, 0);

 Instruction_Group("c3-load",
                    // new c3
                    TOP_c3_fftld,
                    TOP_c3_ld,
                    TOP_UNDEFINED);
 Any_Operand_Access_Time(0);
 Any_Result_Available_Time(2);
 Resource_Requirement(res_issue, 0);
 Resource_Requirement(res_memory, 0);
 Alternative_Resource_Requirement(res_integer_or_memory, 0);
 Alternative_Resource_Requirement(res_issue, 0);
 Load_Access_Time(1);

 Instruction_Group("c3-store",
                   // new c3
                   TOP_c3_fftst,
                   TOP_c3_st,
                   TOP_UNDEFINED);
 Any_Operand_Access_Time(0);
 Resource_Requirement(res_issue, 0);
 Resource_Requirement(res_memory, 0);
 Alternative_Resource_Requirement(res_integer_or_memory, 0);
 Alternative_Resource_Requirement(res_issue, 0);
 Store_Available_Time(1);

 Instruction_Group("c3-ffe",
                    // new c3
                    TOP_c3_ffe,
                    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(14);
  Resource_Requirement(res_issue,0);
  Resource_Requirement(res_ffe,0);
#endif

#if defined(TARG_SL)
  Instruction_Group("movespe",
                    TOP_mvtc,
                    TOP_mvtc_i,
                    TOP_mvtc16,
                    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue,0);
  Resource_Requirement(res_integer_or_memory, 0); 
  Alternative_Resource_Requirement(res_integer, 0);
  Alternative_Resource_Requirement(res_issue, 0);
#endif //targ_sl

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
#ifdef TARG_SL		    
		    TOP_loop,
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
#endif	
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
  Resource_Requirement(res_integer_or_memory, 0); 
  Resource_Requirement(res_issue, 0);
  Alternative_Resource_Requirement(res_integer, 0);
  Alternative_Resource_Requirement(res_issue, 0);


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
#ifdef TARG_SL
                    TOP_ldw16,
                    TOP_pop16,
                    TOP_ldub16_rs,
                    TOP_lduh16_rs,
#endif
		    TOP_lwxc1,
		    TOP_ldxc1,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_memory, 0); 
  Alternative_Resource_Requirement(res_integer_or_memory, 0);
  Alternative_Resource_Requirement(res_issue,0);
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
#ifdef TARG_SL
                    TOP_stw16,
                    TOP_push16,
#endif
		    TOP_swxc1,
		    TOP_sdxc1,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_memory, 0); 
  Resource_Requirement(res_issue, 0);
  Alternative_Resource_Requirement(res_integer_or_memory, 0);
  Alternative_Resource_Requirement(res_issue, 0);
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
#if defined(TARG_SL)		    
                    TOP_br16_eqz,
                    TOP_br16_nez,
                    TOP_jr16,
                    TOP_jr16_lnk,
                    TOP_ret,
                    TOP_ret16,
#endif                    
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer_or_memory, 0); 
  Alternative_Resource_Requirement(res_integer, 0);
  Alternative_Resource_Requirement(res_issue,0);


  Instruction_Group("sync",
		    TOP_sync,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer_or_memory, 0);   
  Resource_Requirement(res_memory, 0);

  Instruction_Group("prefetch",
		    TOP_pref,
		    TOP_prefx,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer_or_memory, 0);   
  Resource_Requirement(res_memory, 0);

  Instruction_Group("nop",
		    TOP_nop,
	            TOP_nop16,
		    TOP_break,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("dummy",
		    TOP_asm,
		    TOP_intrncall,
		    TOP_spadjust,
                   TOP_auxbr,
             	    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer_or_memory, 0);   
  Alternative_Resource_Requirement(res_integer, 0);
  Alternative_Resource_Requirement(res_issue,0);

  Machine_Done();
}
