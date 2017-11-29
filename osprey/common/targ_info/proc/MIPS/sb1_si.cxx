/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *
 * Sibyte SB1 scheduling information
 *
 * For documentation on the opcodes and pipelines, see
 * Section 3 of the SB-1 User Manual.
 *
 * Greg Lindahl, 27Nov2002
 *
 */

#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_IBox, res_EX, res_LS, res_FP; /* MDMX units */
static RESOURCE res_EX0, res_EX1, res_LS0, res_LS1, res_FP0, res_FP1;

#ifdef TARG_SL
static RESOURCE res_CP2;
static RESOURCE res_DSP;
#endif

#ifdef TARG_SL2
static RESOURCE res_SL2;
#endif //TARG_SL2



void Generate_SB1 (void)
{
  Machine("sb1", ISA_SUBSET_MIPS4);

  res_IBox = RESOURCE_Create ("IBox", 4);
  res_EX = RESOURCE_Create ("Execution", 2);
  res_LS = RESOURCE_Create ("LoadStore", 2);
  res_FP = RESOURCE_Create ("Float", 2);
  res_EX0 = RESOURCE_Create ("Execution", 1);
  res_LS0 = RESOURCE_Create ("LoadStore", 1);
  res_FP0 = RESOURCE_Create ("Float", 1);
  res_EX1 = RESOURCE_Create ("Execution", 1);
  res_LS1 = RESOURCE_Create ("LoadStore", 1);
  res_FP1 = RESOURCE_Create ("Float", 1);

#ifdef TARG_SL
  res_CP2 = RESOURCE_Create ("Cproc2", 1);
  res_DSP = RESOURCE_Create("dspthread", 1);
#endif //TARG_SL

#ifdef  TARG_SL2
  res_SL2 = RESOURCE_Create ("sl2", 1);
#endif //TARG_SL2

  /* instruction categories and units: Table 19 summarizes
   *
   * However, our model only allows an instruction to be in one
   * instruction group.
   *
   * (X) marks places where we leave off a unit because there
   * are multiple types of units involved (e.g. E0,E1 and LS1)
   *
   * (Y) marks places where we give too broad of a restriction
   * because only one of the pair can execute an instruction
   *
   * 1) Integer ADDs, SUBs, Logical Ops E0, E1, LS1(X) (different pipe)
   * 2) Shifts E0, E1
   * 3) LUI E0, E1, LS1(X)
   * 4) Branches/Jumps E0 (Y)
   * 5) CP1 Branches E0 (Y)
   * 6) Sets E0 (Y)
   * 7) Traps E0 (Y)
   * 8) CLZ/CLO E0 (Y)
   * 9) Conditional Moves E0, E1
   * 10) Integer Multiply/Divide E1 (note unusual latency) (Y)
   * 11) MT/MF HI/LO E1 (Y)
   * 12) MOVT, MOVF E0, E1
   * 13) MOVZ, MOVN E0, E1
   *
   * 14) Integer Loads, Stores, FP Loads, Stores LS0 LS1 (same pipeline)
   * 15) Indexed Loads, Stores LS1 (Y)
   * 16) TLB OPs LS1 (Y)
   * 17) MT/MF CP0 LS1 (Y)
   * 18) Cache Ops LS1 (Y)
   *
   * 19) Normal FP ops FP0, FP1
   * 20) C.cond FP0 (Y)
   * 21) CABS FP0 (this is a MIPS-3D instruction) (Y)
   * "The majority of MIPS-3D instructions can be issued to FP1 pipe only" (Y)
   */


#if defined(TARG_SL)
       /* move to special register*/
       Instruction_Group("mvspe",
                         TOP_mvtc,
                         TOP_mvtc_i,
                         TOP_mvtc16,
                         TOP_UNDEFINED);
       Any_Operand_Access_Time (0);
       Any_Result_Available_Time (1);
       Resource_Requirement (res_IBox, 0);
       Resource_Requirement (res_LS, 0);

	/*16 bit instr*/
	Instruction_Group("Load_store16",
			  TOP_push16,
			  TOP_pop16,
			  TOP_ldw16, 
                          TOP_stw16,
			  TOP_ldub16_rs,
			  TOP_lduh16_rs,
			  TOP_UNDEFINED);
	Any_Operand_Access_Time (0);
	Any_Result_Available_Time (1);
	Resource_Requirement (res_IBox, 0);
	Resource_Requirement (res_LS, 0);
  
	Instruction_Group("arith",
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
			  TOP_sub16,
			  TOP_sub16_i,
			  TOP_xor16,
			  TOP_xor16_i,
			  TOP_UNDEFINED);
	Any_Operand_Access_Time (0);
	Any_Result_Available_Time (1);
	Resource_Requirement (res_IBox, 0);
	Resource_Requirement (res_EX, 0);
  
	Instruction_Group("shift16",
			  TOP_shll16,
			  TOP_shll16_i,
			  TOP_shra16,
                          TOP_shra16_i,
			  TOP_shrl16,
			  TOP_shrl16_i,
			  TOP_UNDEFINED);
	Any_Operand_Access_Time (0);
	Any_Result_Available_Time (1);
	Resource_Requirement (res_IBox, 0);
	Resource_Requirement (res_EX, 0);
  
	Instruction_Group("dummy16",
			  TOP_nop16,
			  TOP_UNDEFINED);
	Any_Operand_Access_Time(0);
	Any_Result_Available_Time(1);						 
#endif

  /* For this instruction group, ALU ops can co-issue, so let's pretend
     that the latency is zero. */

  Instruction_Group ("Load/Store", 
			  TOP_lb, /* type 14, lat 0 */
			  TOP_lbu, /* type 14, lat 0 */
			  TOP_lh, /* type 14, lat 0 */
			  TOP_lhu, /* type 14, lat 0 */
			  TOP_lw, /* type 14, lat 0 */
			  TOP_lwl, /* type 14, lat 0 */
			  TOP_lwr, /* type 14, lat 0 */
			  TOP_sb, /* type 14, lat 0 */
			  TOP_sh, /* type 14, lat 0 */
			  TOP_sw, /* type 14, lat 0 */
			  TOP_swl, /* type 14, lat 0 */
			  TOP_swr, /* type 14, lat 0 */
			  TOP_ll, /* type 14, lat ? */
			  TOP_sc, /* type 14, lat ? */
			  TOP_sync, /* type 14, lat ? */
			  TOP_lwu, /* type 14, lat 0 */
			  TOP_ld, /* type 14, lat 0 */
			  TOP_ldl, /* type 14, lat 0 */
			  TOP_ldr, /* type 14, lat 0 */
			  TOP_lld, /* type 14, lat 0 */
			  TOP_sd, /* type 14, lat 0 */
			  TOP_sdl, /* type 14, lat 0 */
			  TOP_sdr, /* type 14, lat 0 */
			  TOP_scd, /* type 14, lat 0 */
			  TOP_pref, /* type 14, lat ? */
			  TOP_prefx, /* type 14, lat ? */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_LS, 0);

		    /* Table 4: CPU (integer) Arithmetic Instructions */

  Instruction_Group ("Integer ALU group 1",
			  TOP_add, /* type 1, lat 1 */
			  TOP_addi, /* type 1, lat 1 */
			  TOP_addiu, /* type 1, lat 1 */
			  TOP_addu, /* type 1, lat 1 */
			  TOP_sub, /* type 1, lat 1 */
			  TOP_subu, /* type 1, lat 1 */
			  TOP_dadd, /* type 1, lat 1 */
			  TOP_daddi, /* type 1, lat 1 */
			  TOP_daddiu, /* type 1, lat 1 */
			  TOP_daddu, /* type 1, lat 1 */
			  TOP_dsub, /* type 1, lat 1 */
			  TOP_dsubu, /* type 1, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);

  Instruction_Group ("Integer ALU group 1a",
			  TOP_slt, /* type 6, lat 1 */
			  TOP_slti, /* type 6, lat 1 */
			  TOP_sltiu, /* type 6, lat 1 */
			  TOP_sltu, /* type 6, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

  Instruction_Group ("Integer ALU group 2",
			  TOP_div, /* type 10, lat 36 */ /* not pipelined */
			  TOP_divu, /* type 10, lat 36 */ /* not pipelined */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (36);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("Integer ALU group 3",
			  TOP_mult, /* type 10, lat 3 */
			  TOP_multu, /* type 10, lat 3 */
#ifdef TARG_SL
                       TOP_c3_muls,
                       TOP_c3_mulus,

#endif
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (3);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("Integer ALU group 4",
			  TOP_ddiv, /* type 10, lat 68 */ /* not pipelined */
			  TOP_ddivu, /* type 10, lat 68 */ /* not pipelined */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (68);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("Integer ALU group 4",
			  TOP_dmult, /* type 10, lat 4 */ /* half pipelined */
			  TOP_dmultu, /* type 10, lat 4 */ /* half pipelined */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

		    /* Table 5: CPU Logical Instructions */

  Instruction_Group ("CPU Logical",
			  TOP_and, /* type 1, lat 1 */
			  TOP_andi, /* type 1, lat 1 */
			  TOP_lui, /* type 3, lat 1 */
			  TOP_nor, /* type 1, lat 1 */
			  TOP_or, /* type 1, lat 1 */
			  TOP_ori, /* type 1, lat 1 */
			  TOP_xor, /* type 1, lat 1 */
			  TOP_xori, /* type 1, lat 1 */
#if defined(TARG_SL)			  
  			  TOP_depb,
			  TOP_extrbs,
			  TOP_extrbu,	
			  TOP_loop,
#endif				  
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);

		    /* Table 6: CPU Move Instructions */

  Instruction_Group ("CPU move 1",
			  TOP_mfhi, /* type 11, lat 1 */ /* can not co-issue, so it's essentally lat 1 */
			  TOP_mflo, /* type 11, lat 1 */ /* can not co-issue, so it's essentally lat 1 */
			  TOP_mthi, /* type 11, lat 1 */ /* can not co-issue, so it's essentally lat 1 */
			  TOP_mtlo, /* type 11, lat 1 */ /* can not co-issue, so it's essentally lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("CPU move 2",
#if defined(TARG_SL)
	                TOP_mvfc,
                       TOP_mc_abs,
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
			  TOP_mc_zc_eq,
         		  TOP_mc_zc_ne,
			  TOP_mc_zc_gt,
			  TOP_mc_zc_ge,
			  TOP_mc_zc_lt,
			  TOP_mc_zc_le,
#endif		
			  TOP_movf, /* type 12, lat 1 */
			  TOP_movn, /* type 13, lat 1 */
			  TOP_movt, /* type 12, lat 1 */
			  TOP_movz, /* type 13, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);

		    /* Table 7: CPU Shift Instructions */

  Instruction_Group ("CPU Shift",
			  TOP_sll, /* type 2, lat 1 */
			  TOP_sllv, /* type 2, lat 1 */
			  TOP_sra, /* type 2, lat 1 */
			  TOP_srav, /* type 2, lat 1 */
			  TOP_srl, /* type 2, lat 1 */
			  TOP_srlv, /* type 2, lat 1 */
			  TOP_dsll, /* type 2, lat 1 */
			  TOP_dsll32, /* type 2, lat 1 */
			  TOP_dsllv, /* type 2, lat 1 */
			  TOP_dsra, /* type 2, lat 1 */
			  TOP_dsra32, /* type 2, lat 1 */
			  TOP_dsrav, /* type 2, lat 1 */
			  TOP_dsrl, /* type 2, lat 1 */
			  TOP_dsrl32, /* type 2, lat 1 */
			  TOP_dsrlv, /* type 2, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);

		    /* Table 8: CPU Branch and Jump Instructions */

  Instruction_Group ("CPU Branch/Jump",
			  TOP_beq, /* type 4, lat 1 */
			  TOP_bgez, /* type 4, lat 1 */
			  TOP_bgezal, /* type 4, lat 1 */
			  TOP_bgtz, /* type 4, lat 1 */
			  TOP_blez, /* type 4, lat 1 */
			  TOP_bltz, /* type 4, lat 1 */
			  TOP_bltzal, /* type 4, lat 1 */
			  TOP_bne, /* type 4, lat 1 */
			  TOP_j, /* type 4, lat 1 */
			  TOP_jal, /* type 4, lat 1 */
			  TOP_jalr, /* type 4, lat 1 */
			  TOP_jr, /* type 4, lat 1 */
#if defined(TARG_SL)
                       TOP_ret,
                       TOP_br16_eqz,
                       TOP_br16_nez,
                       TOP_jr16,
                       TOP_jr16_lnk,
                       TOP_ret16,
#endif
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

		    /* Table 9: CPU Trap Instructions */

  Instruction_Group ("CPU Trap",
			  TOP_break, /* type 7, lat 1 */
			  TOP_syscall, /* type 7, lat 1 */
			  TOP_teq, /* type 7, lat 1 */
			  TOP_teqi, /* type 7, lat 1 */
			  TOP_tge, /* type 7, lat 1 */
			  TOP_tgei, /* type 7, lat 1 */
			  TOP_tgeiu, /* type 7, lat 1 */
			  TOP_tgeu, /* type 7, lat 1 */
			  TOP_tlt, /* type 7, lat 1 */
			  TOP_tlti, /* type 7, lat 1 */
			  TOP_tltiu, /* type 7, lat 1 */
			  TOP_tltu, /* type 7, lat 1 */
			  TOP_tne, /* type 7, lat 1 */
			  TOP_tnei, /* type 7, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

    /* FPU loads/stores, not listed in SB-1 manual, 6-9 MIPS R4000
     and table 3-14 MIPS64 Architecture Manual */

  Instruction_Group ("FPU Load/Store 1",
			  TOP_lwc1, /* type 14, lat 0 */
			  TOP_swc1, /* type 14, lat 0 */
			  TOP_ldc1, /* type 14, lat 0 */
			  TOP_sdc1, /* type 14, lat 0 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (0);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_LS, 0);

  Instruction_Group ("FPU Load/Store 2",
			  TOP_lwxc1, /* type 15, lat 0 */
			  TOP_ldxc1, /* type 15, lat 0 */
			  TOP_swxc1, /* type 15, lat 0 */
			  TOP_sdxc1, /* type 15, lat 0 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (0);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_LS, 0);
  Resource_Requirement (res_LS1, 0);

    /* Table 23: FPU Arithmetic Instructions (missing paired single) */

  Instruction_Group ("FPU Arithmatic group 0",
			  TOP_abs_s, /* type 19, lat 4 */
			  TOP_abs_d, /* type 19, lat 4 */
			  TOP_add_s, /* type 19, lat 4 */
			  TOP_add_d, /* type 19, lat 4 */

			  TOP_mul_s, /* type 19, lat 4 */
			  TOP_mul_d, /* type 19, lat 4 */
			  TOP_neg_s, /* type 19, lat 4 */
			  TOP_neg_d, /* type 19, lat 4 */
			  TOP_sub_s, /* type 19, lat 4 */
			  TOP_sub_d, /* type 19, lat 4 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

			  /* List of conditions table 6-13 MIPS R4000 User Manual */

  Instruction_Group ("FPU Arithmatic group 0a",
			  TOP_c_f_s, /* type 20, lat 4 */
			  TOP_c_f_d, /* type 20, lat 4 */
			  TOP_c_t_s, /* type 20, lat 4 */
			  TOP_c_t_d, /* type 20, lat 4 */
			  TOP_c_un_s, /* type 20, lat 4 */
			  TOP_c_un_d, /* type 20, lat 4 */
			  TOP_c_or_s, /* type 20, lat 4 */
			  TOP_c_or_d, /* type 20, lat 4 */
			  TOP_c_eq_s, /* type 20, lat 4 */
			  TOP_c_eq_d, /* type 20, lat 4 */
			  TOP_c_neq_s, /* type 20, lat 4 */
			  TOP_c_neq_d, /* type 20, lat 4 */
			  TOP_c_ueq_s, /* type 20, lat 4 */
			  TOP_c_ueq_d, /* type 20, lat 4 */
			  TOP_c_olg_s, /* type 20, lat 4 */
			  TOP_c_olg_d, /* type 20, lat 4 */
			  TOP_c_olt_s, /* type 20, lat 4 */
			  TOP_c_olt_d, /* type 20, lat 4 */
			  TOP_c_uge_s, /* type 20, lat 4 */
			  TOP_c_uge_d, /* type 20, lat 4 */
			  TOP_c_ult_s, /* type 20, lat 4 */
			  TOP_c_ult_d, /* type 20, lat 4 */
			  TOP_c_oge_s, /* type 20, lat 4 */
			  TOP_c_oge_d, /* type 20, lat 4 */
			  TOP_c_ole_s, /* type 20, lat 4 */
			  TOP_c_ole_d, /* type 20, lat 4 */
			  TOP_c_ugt_s, /* type 20, lat 4 */
			  TOP_c_ugt_d, /* type 20, lat 4 */
			  TOP_c_ule_s, /* type 20, lat 4 */
			  TOP_c_ule_d, /* type 20, lat 4 */
			  TOP_c_ogt_s, /* type 20, lat 4 */
			  TOP_c_ogt_d, /* type 20, lat 4 */
			  TOP_c_sf_s, /* type 20, lat 4 */
			  TOP_c_sf_d, /* type 20, lat 4 */
			  TOP_c_st_s, /* type 20, lat 4 */
			  TOP_c_st_d, /* type 20, lat 4 */
			  TOP_c_ngle_s, /* type 20, lat 4 */
			  TOP_c_ngle_d, /* type 20, lat 4 */
			  TOP_c_gle_s, /* type 20, lat 4 */
			  TOP_c_gle_d, /* type 20, lat 4 */
			  TOP_c_seq_s, /* type 20, lat 4 */
			  TOP_c_seq_d, /* type 20, lat 4 */
			  TOP_c_sne_s, /* type 20, lat 4 */
			  TOP_c_sne_d, /* type 20, lat 4 */
			  TOP_c_ngl_s, /* type 20, lat 4 */
			  TOP_c_ngl_d, /* type 20, lat 4 */
			  TOP_c_gl_s, /* type 20, lat 4 */
			  TOP_c_gl_d, /* type 20, lat 4 */
			  TOP_c_lt_s, /* type 20, lat 4 */
			  TOP_c_lt_d, /* type 20, lat 4 */
			  TOP_c_nlt_s, /* type 20, lat 4 */
			  TOP_c_nlt_d, /* type 20, lat 4 */
			  TOP_c_nge_s, /* type 20, lat 4 */
			  TOP_c_nge_d, /* type 20, lat 4 */
			  TOP_c_ge_s, /* type 20, lat 4 */
			  TOP_c_ge_d, /* type 20, lat 4 */
			  TOP_c_le_s, /* type 20, lat 4 */
			  TOP_c_le_d, /* type 20, lat 4 */
			  TOP_c_nle_s, /* type 20, lat 4 */
			  TOP_c_nle_d, /* type 20, lat 4 */
			  TOP_c_ngt_s, /* type 20, lat 4 */
			  TOP_c_ngt_d, /* type 20, lat 4 */
			  TOP_c_gt_s, /* type 20, lat 4 */
			  TOP_c_gt_d, /* type 20, lat 4 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);
  Resource_Requirement (res_FP0, 0);

  Instruction_Group ("FPU Arithmatic group 1",
			  TOP_madd_s, /* type 19, lat 8 */
			  TOP_madd_d, /* type 19, lat 8 */
			  TOP_msub_s, /* type 19, lat 8 */
			  TOP_msub_d, /* type 19, lat 8 */
			  TOP_nmadd_s, /* type 19, lat 8 */
			  TOP_nmadd_d, /* type 19, lat 8 */
			  TOP_nmsub_s, /* type 19, lat 8 */
			  TOP_nmsub_d, /* type 19, lat 8 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (8);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 2",
			  TOP_div_s, /* type 19, lat 24 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (24);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 3",
			  TOP_div_d, /* type 19, lat 32 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (32);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 4",
			  TOP_sqrt_s, /* type 19, lat 28 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (28);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 5",
			  TOP_sqrt_d, /* type 19, lat 40 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (40);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 6",
			  TOP_recip_s, /* type 19, lat 12 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (12);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 7",
			  TOP_recip_d, /* type 19, lat 20 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (20);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 8",
			  TOP_rsqrt_s, /* type 19, lat 16 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (16);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 9",
			  TOP_rsqrt_d, /* type 19, lat 28 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (28);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

		    /* Table 24: FPU Move Instructions */

  Instruction_Group ("FPU Move From",
			  TOP_cfc1, /* type 19, lat 1 */
			  TOP_mfc1, /* type 19, lat 1 */
			  TOP_dmfc1, /* type 19, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0); /* ??? */

  Instruction_Group ("FPU Move To",
			  TOP_ctc1, /* type 19, lat 4 */
			  TOP_mtc1, /* type 19, lat 4 */
			  TOP_dmtc1, /* type 19, lat 4 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0); /* ??? */

  Instruction_Group ("FPU Move/Convert",
			  TOP_mov_s, /* type 19, lat 4 */ /* missing paired single */
			  TOP_mov_d, /* type 19, lat 4 */
			  TOP_movf_s, /* type 19, lat 4 */ /* missing paired single */
			  TOP_movf_d, /* type 19, lat 4 */
			  TOP_movn_s, /* type 19, lat 4 */
			  TOP_movn_d, /* type 19, lat 4 */
			  TOP_movt_s, /* type 19, lat 4 */ /* missing paired single */
			  TOP_movt_d, /* type 19, lat 4 */
			  TOP_movz_s, /* type 19, lat 4 */
			  TOP_movz_d, /* type 19, lat 4 */

			  /* Table 25: FPU Convert Instructions */

			  TOP_cvt_d_s, /* type 19, lat 4 */
			  TOP_cvt_d_w, /* type 19, lat 4 */
			  TOP_cvt_d_l, /* type 19, lat 4 */
			  TOP_cvt_l_s, /* type 19, lat 4 */
			  TOP_cvt_l_d, /* type 19, lat 4 */
			  TOP_cvt_s_d, /* type 19, lat 4 */
			  TOP_cvt_s_w, /* type 19, lat 4 */
			  TOP_cvt_s_l, /* type 19, lat 4 */
			  TOP_cvt_w_s, /* type 19, lat 4 */
			  TOP_cvt_w_d, /* type 19, lat 4 */
			  TOP_ceil_w_s, /* type 19, lat 4 */
			  TOP_ceil_w_d, /* type 19, lat 4 */
			  TOP_ceil_l_s, /* type 19, lat 4 */
			  TOP_ceil_l_d, /* type 19, lat 4 */
			  TOP_floor_w_s, /* type 19, lat 4 */
			  TOP_floor_w_d, /* type 19, lat 4 */
			  TOP_floor_l_s, /* type 19, lat 4 */
			  TOP_floor_l_d, /* type 19, lat 4 */
			  TOP_round_w_s, /* type 19, lat 4 */
			  TOP_round_w_d, /* type 19, lat 4 */
			  TOP_round_l_s, /* type 19, lat 4 */
			  TOP_round_l_d, /* type 19, lat 4 */
			  TOP_trunc_w_s, /* type 19, lat 4 */
			  TOP_trunc_w_d, /* type 19, lat 4 */
			  TOP_trunc_l_s, /* type 19, lat 4 */
			  TOP_trunc_l_d, /* type 19, lat 4 */
			  /* 6 paired single instructions skipped here */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

    /* Table 26: FPU Branch Instructions */

  Instruction_Group ("FPU Branch",
			  TOP_bc1f, /* type 5, lat 1 */
			  TOP_bc1t, /* type 5, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

    /* Table 28: MIPS-3D instructions skipped */

    /* Dummy instructions */

  Instruction_Group ("Dummy",
			  TOP_asm,
			  TOP_intrncall,
			  TOP_spadjust,
			  TOP_begin_pregtn,
			  TOP_end_pregtn,
			  TOP_bwd_bar,
			  TOP_fwd_bar,
			  TOP_label,
			  TOP_nop,
			  TOP_noop,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  

#ifdef TARG_SL2 
  Instruction_Group ("sl2",
      TOP_c2_mvgr_r2g_h_u,
      TOP_c2_mvgr_r2g_h,
      TOP_c2_mvgr_r2g_w,
      TOP_c2_mvgr_r2g_h_u_i,
      TOP_c2_mvgr_r2g_h_i,
      TOP_c2_mvgr_r2g_w_i,
      TOP_c2_mvgr_g2r_ba_lh,
      TOP_c2_mvgr_g2r_ba_hh,
      TOP_c2_mvgr_g2r_ba_w,
      TOP_c2_mvgr_g2r_lh_i,
      TOP_c2_mvgr_g2r_hh_i,
      TOP_c2_mvgr_g2r_w_i,
      TOP_c2_mvgr_g2r_lh,
      TOP_c2_mvgr_g2r_hh,
      TOP_c2_mvgr_g2r_w,
      TOP_c2_mvgr_g2r_bh,
      TOP_c2_mvgr_g2r_bh_u,
      TOP_c2_mvgr_g2r_bv,
      TOP_c2_mvgr_g2r_bv_u,
      TOP_c2_mvgr_g2r_b4_i, 
      TOP_c2_mvgr_g2r_b4,
      TOP_c2_mvgc_c2g,
      TOP_c2_mvgc_g2c,
      TOP_c2_ld_v_b_u,
      TOP_c2_ld_v_b,
      TOP_c2_ld_v_h,
      TOP_c2_ld_v_w,
      TOP_c2_ld_v_sw,
      TOP_c2_ld_v_m_b_u,
      TOP_c2_ld_v_m_b,
      TOP_c2_ld_v_m_h,
      TOP_c2_ld_v_m_w,
      TOP_c2_ld_s_h_u_p,
      TOP_c2_ld_s_h_u,
      TOP_c2_ld_s_h_p,
      TOP_c2_ld_s_h,
      TOP_c2_ld_s_w_p,
      TOP_c2_ld_s_w,
      TOP_c2_st_v_b,
      TOP_c2_st_v_h,
      TOP_c2_st_v_w,
      TOP_c2_st_v_m_b,
      TOP_c2_st_v_m_h,
      TOP_c2_st_v_m_w,
      TOP_c2_st_s_h,
      TOP_c2_st_s_h_p,
      TOP_c2_st_s_w,
      TOP_c2_st_s_w_p,
      TOP_c2_ldi_s_h_u,
      TOP_c2_ldi_s_h,
      TOP_c2_ldi_s_w,
      TOP_c2_ldi_c,
      TOP_c2_ldi_v_b_u,
      TOP_c2_ldi_v_b,
      TOP_c2_ldi_v_h,
      TOP_c2_ldi_v_w,
      TOP_c2_ldi_v_m_b_u,
      TOP_c2_ldi_v_m_b,
      TOP_c2_ldi_v_m_h,
      TOP_c2_ldi_v_m_w,
      TOP_c2_sti_v_b,
      TOP_c2_sti_v_h,
      TOP_c2_sti_v_w,
      TOP_c2_sti_v_m_b,
      TOP_c2_sti_v_m_h,
      TOP_c2_sti_v_m_w,
      TOP_c2_sti_s_h,
      TOP_c2_sti_s_w,
      TOP_c2_sti_c,
      TOP_c2_vadds_h,
      TOP_c2_vadds_w,
      TOP_c2_vadds_p,
      TOP_c2_vadds_h_mode6,
      TOP_c2_vadds_h_mode2,
      TOP_c2_vadds_w_mode6,
      TOP_c2_vadds_w_mode2,
      TOP_c2_vadds_p_mode6,
      TOP_c2_vadds_p_mode2,
      TOP_c2_vsubs_h,
      TOP_c2_vsubs_h_sm,
      TOP_c2_vsubs_h_abs,
      TOP_c2_vsubs_h_abs_sm,
      TOP_c2_vabs_h,
      TOP_c2_vabs_h_sm,
      TOP_c2_vsubs_w,
      TOP_c2_vsubs_w_sm,
      TOP_c2_vsubs_w_abs,
      TOP_c2_vsubs_w_abs_sm,
      TOP_c2_vabs_w,
      TOP_c2_vabs_w_sm,
      TOP_c2_vsubs_p,
      TOP_c2_vsubs_p_sm,
      TOP_c2_vsubs_p_abs,
      TOP_c2_vsubs_p_abs_sm,
      TOP_c2_vabs_p,
      TOP_c2_vabs_p_sm,
      TOP_c2_vmul_h,
      TOP_c2_vmul_w,
      TOP_c2_vneg_h,
      TOP_c2_vneg_w,
      TOP_c2_vneg_p,
      TOP_c2_vshr_p,
      TOP_c2_vshr_h,
      TOP_c2_vshr_w,
      TOP_c2_vshl_p,
      TOP_c2_vshl_h,
      TOP_c2_vshl_w,
      TOP_c2_vclp,
      TOP_c2_vclp_p,
      TOP_c2_vclp_a,
      TOP_c2_vclp_s,
      TOP_c2_vclp_2,
      TOP_c2_vclp_n,
      TOP_c2_vclg_h_lt_and,
      TOP_c2_vclg_h_lt_or,
      TOP_c2_vclg_h_le_and,
      TOP_c2_vclg_h_le_or,
      TOP_c2_vclg_h_eq_and,
      TOP_c2_vclg_h_eq_or,
      TOP_c2_vclg_h_ge_and,
      TOP_c2_vclg_h_ge_or,
      TOP_c2_vclg_h_gt_and,
      TOP_c2_vclg_h_gt_or,
      TOP_c2_vclg_h_and,
      TOP_c2_vclg_h_or,
      TOP_c2_vclg_h_le,
      TOP_c2_vclg_h_lt,
      TOP_c2_vclg_h_ge,
      TOP_c2_vclg_h_gt,
      TOP_c2_vclg_w_lt_and,
      TOP_c2_vclg_w_lt_or,
      TOP_c2_vclg_w_le_and,
      TOP_c2_vclg_w_le_or,
      TOP_c2_vclg_w_eq_and,
      TOP_c2_vclg_w_eq_or,
      TOP_c2_vclg_w_ge_and,
      TOP_c2_vclg_w_ge_or,
      TOP_c2_vclg_w_gt_and,
      TOP_c2_vclg_w_gt_or,
      TOP_c2_vclg_w_and,
      TOP_c2_vclg_w_or,
      TOP_c2_vclg_w_le ,
      TOP_c2_vclg_w_lt ,
      TOP_c2_vclg_w_ge,
      TOP_c2_vclg_w_gt ,
      TOP_c2_vclg_p_lt_and,
      TOP_c2_vclg_p_lt_or,
      TOP_c2_vclg_p_le_and,
      TOP_c2_vclg_p_le_or,
      TOP_c2_vclg_p_eq_and,
      TOP_c2_vclg_p_eq_or,
      TOP_c2_vclg_p_ge_and,
      TOP_c2_vclg_p_ge_or,
      TOP_c2_vclg_p_gt_and,
      TOP_c2_vclg_p_gt_or,
      TOP_c2_vclg_p_and,
      TOP_c2_vclg_p_or,
      TOP_c2_vclg_p_le,
      TOP_c2_vclg_p_eq,
      TOP_c2_vclg_p_ge,
      TOP_c2_vclg_p_gt,
      TOP_c2_vcmov_h_f,
      TOP_c2_vcmov_h_t,
      TOP_c2_vcmov_w_f,
      TOP_c2_vcmov_w_t,
      TOP_c2_lczero_z,
      TOP_c2_vrnd_h,
      TOP_c2_vrnd_w,
      TOP_c2_vspas,
      TOP_c2_vspel_mul_h,
      TOP_c2_vspel_mul_w,
      TOP_c2_vspel_adds,
      TOP_c2_vspel_mac_h,
      TOP_c2_vspel_mac_w,
      TOP_c2_mmul_h,
      TOP_c2_mmul_w,
      TOP_c2_vmov,
      TOP_c2_vmov_swin,
      TOP_c2_vcopy, 
      TOP_c2_vcmpr_h_eq,
      TOP_c2_vcmpr_h_lt,
      TOP_c2_vcmpr_h_le,
      TOP_c2_vcmpr_h_gt,
      TOP_c2_vcmpr_h_ge,
      TOP_c2_vcmpr_w_eq,
      TOP_c2_vcmpr_w_lt,
      TOP_c2_vcmpr_w_le,
      TOP_c2_vcmpr_w_gt,
      TOP_c2_vcmpr_w_ge,
      TOP_c2_sad, 
      TOP_c2_satd, 
      TOP_c2_intra,
      TOP_c2_intra_0_1_9_14_16,
      TOP_c2_intra_2_3_8_10,
      TOP_c2_intra_4,
      TOP_c2_intra_5_11,
      TOP_c2_intra_6,
      TOP_c2_intra_7,
      TOP_c2_intra_12_13,
      TOP_c2_intra_15_17,
      TOP_c2_mvsel_mode0,
      TOP_c2_mvsel_mode1,
      TOP_c2_mvsel_mode2,
      TOP_c2_mvsel_mode345,
      TOP_c2_bcst_q,
      TOP_c2_bcst_i,
      TOP_c2_vlcs_dc,
      TOP_c2_vlcs_ac,
      TOP_c2_vlcs_wb,
TOP_c2_add_shl_g_i,
TOP_c2_add_shr_g_i,
TOP_c2_add_shl_g,
TOP_c2_add_shr_g,
TOP_c2_add_shl_r_h_i,
TOP_c2_add_shr_r_h_i,
TOP_c2_add_shl_r_w_i,
TOP_c2_add_shr_r_w_i,
TOP_c2_add_shl_r_h,
TOP_c2_add_shr_r_h,
TOP_c2_add_shl_r_w,
TOP_c2_add_shr_r_w,
TOP_c2_muls,
      TOP_c2_sub_g_abs_i,
      TOP_c2_subs_g_i,
      TOP_c2_sub_g_abs,
      TOP_c2_subs_g,
      TOP_c2_subs_r_h_i,
      TOP_c2_subs_r_w_i,
      TOP_c2_sub_r_abs_h_i,
      TOP_c2_sub_r_abs_w_i,
      TOP_c2_subs_r_h,
      TOP_c2_subs_r_w,
      TOP_c2_sub_r_abs_h,
      TOP_c2_sub_r_abs_w,
      TOP_c2_muls,
      TOP_c2_mads,
      TOP_c2_smads,
      TOP_c2_min,
      TOP_c2_max,
      TOP_c2_cmov,
      TOP_c2_mov_g,
      TOP_c2_mov_r ,
      TOP_c2_mov_c_i,
      TOP_c2_mov_c,
      TOP_c2_mov_s_i, 
      TOP_c2_mov_s, 
      TOP_c2_clp,
      TOP_c2_clp_i,
      TOP_c2_chkrng,
      TOP_c2_scond_eq,
      TOP_c2_scond_lt,
      TOP_c2_scond_le,
      TOP_c2_scond_gt,
      TOP_c2_scond_ge,
      TOP_c2_scond_eq_i,
      TOP_c2_scond_lt_i,
      TOP_c2_scond_le_i,
      TOP_c2_scond_gt_i,
      TOP_c2_scond_ge_i,
      TOP_c2_scond_r_h_eq,
      TOP_c2_scond_r_h_lt,
      TOP_c2_scond_r_h_le,
      TOP_c2_scond_r_h_gt,
      TOP_c2_scond_r_h_ge,
      TOP_c2_scond_r_h_eq_i,
      TOP_c2_scond_r_h_lt_i,
      TOP_c2_scond_r_h_le_i,
      TOP_c2_scond_r_h_gt_i,
      TOP_c2_scond_r_h_ge_i,
      TOP_c2_scond_r_w_eq,
      TOP_c2_scond_r_w_lt,
      TOP_c2_scond_r_w_le,
      TOP_c2_scond_r_w_gt,
      TOP_c2_scond_r_w_ge,
      TOP_c2_scond_r_w_eq_i,
      TOP_c2_scond_r_w_lt_i,
      TOP_c2_scond_r_w_le_i,
      TOP_c2_scond_r_w_gt_i,
      TOP_c2_scond_r_w_ge_i,      
      TOP_c2_scond_r_h_wb_eq,
      TOP_c2_scond_r_h_wb_lt,
      TOP_c2_scond_r_h_wb_le,
      TOP_c2_scond_r_h_wb_gt,
      TOP_c2_scond_r_h_wb_ge,
      TOP_c2_scond_r_wb_eq_i,
      TOP_c2_scond_r_wb_lt_i,
      TOP_c2_scond_r_wb_le_i,
      TOP_c2_scond_r_wb_gt_i,
      TOP_c2_scond_r_wb_ge_i,
      TOP_c2_scond_r_w_wb_eq,
      TOP_c2_scond_r_w_wb_lt,
      TOP_c2_scond_r_w_wb_le,
      TOP_c2_scond_r_w_wb_gt,
      TOP_c2_scond_r_w_wb_ge,      
      TOP_c2_scond_r_w_wb_eq_i,
      TOP_c2_scond_r_w_wb_lt_i,
      TOP_c2_scond_r_w_wb_le_i,
      TOP_c2_scond_r_w_wb_gt_i,
      TOP_c2_scond_r_w_wb_ge_i,      
      
      TOP_c2_gsums,      
      TOP_c2_wrap,      
      TOP_c2_lczero_z,
      TOP_c2_lczero_nz_fw,
      TOP_c2_lczero_nz_bw,
      TOP_c2_bop_ls,
      TOP_c2_bop_rs,
      TOP_c2_bop_and,
      TOP_c2_bop_or,
      TOP_c2_bop_xor,
      TOP_c2_bop_andxor,
      TOP_c2_bop_ls_i,
      TOP_c2_bop_rs_i  ,
      TOP_c2_bop_and_i,
      TOP_c2_bop_or_i  ,
      TOP_c2_bop_xor_i,
      TOP_c2_bdep_l   ,
      TOP_c2_bdep_m ,
      TOP_c2_bxtr_u_l ,
      TOP_c2_bxtr_s_l ,
      TOP_c2_bxtr_u_m,
      TOP_c2_bxtr_s_m,
      TOP_c2_bxtrr48,
      TOP_c2_bxtrr48_i,
      TOP_c2_sum4_c,
      TOP_c2_sum4_g,
      TOP_c2_sum4_sw,
      TOP_c2_sum4_r ,
      TOP_c2_med, 
      TOP_c2_fork_m,
      TOP_c2_fork_n ,
      TOP_c2_joint,
      TOP_c2_ld_v2g_b_u,
      TOP_c2_ld_v2g_b,
      TOP_c2_ld_v2g_h_u, 
      TOP_c2_ld_v2g_h,   
      TOP_c2_ld_v2g_w,   
      TOP_c2_st_g2v_b, 
      TOP_c2_st_g2v_h, 
      TOP_c2_st_g2v_w, 
      TOP_c2_ldi_v2g_b_u, 
      TOP_c2_ldi_v2g_b,   
      TOP_c2_ldi_v2g_h_u, 
      TOP_c2_ldi_v2g_h,   
      TOP_c2_ldi_v2g_w,   
      TOP_c2_sti_g2v_b,   
      TOP_c2_sti_g2v_h,   
      TOP_c2_sti_g2v_w,   
      TOP_c2_clzob_zd, 
      TOP_c2_clzob_od, 
      TOP_c2_clzob_zd_i, 
      TOP_c2_clzob_od_i, 
      TOP_c2_thctrl_lock,
      TOP_c2_thctrl_unlock,
      TOP_c2_thctrl_deact,
      TOP_c2_thctrl_act,
      TOP_c2_thctrl_mode4, 
      TOP_c2_thctrl_mode5,
      TOP_c2_thctrl_mode6,
      TOP_c2_thctrl_mode7,
      TOP_c2_sum4_saddr, 
      TOP_c2_macro, 
      TOP_c2_shor_l,
      TOP_c2_shor_rl,
      TOP_c2_shor_ra,
      TOP_c2_shadd_l,
      TOP_c2_shadd_rl,
      TOP_c2_shadd_ra,
      TOP_c2_shsub_l,
      TOP_c2_shsub_rl, 
      TOP_c2_shsub_ra, 
      TOP_c2_shor_l_i,
      TOP_c2_shor_rl_i,
      TOP_c2_shor_ra_i,
      TOP_c2_shadd_l_i,
      TOP_c2_shadd_rl_i,
      TOP_c2_shadd_ra_i,
      TOP_c2_shsub_l_i,
      TOP_c2_shsub_rl_i, 
      TOP_c2_shsub_ra_i, 
      TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_SL2, 0);
#endif //TARG_SL2

#if defined(TARG_SL)
Instruction_Group("c3instr",
        TOP_c3_bitc,
        TOP_c3_bitr,
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
        TOP_c3_ld,
        TOP_c3_lead,
        TOP_c3_mac,
        TOP_c3_mac_a,
        TOP_c3_mac_ar,
        TOP_c3_mac_i,
        TOP_c3_macci,
        TOP_c3_maccr,
        TOP_c3_macd,
        TOP_c3_macn,
        TOP_c3_macn_a,
        TOP_c3_macn_ar,
        TOP_c3_macn_i,
        TOP_c3_mula,
        TOP_c3_mula_a,
        TOP_c3_mula_ar,
        TOP_c3_mula_i,
        TOP_c3_mulaci,
        TOP_c3_mulacr,
        TOP_c3_mulan,
        TOP_c3_mvfs,
        TOP_c3_mvfacc,
        TOP_c3_mvfadd,
        TOP_c3_mvfadds,
        TOP_c3_mvts,
        TOP_c3_mvtacc,
        TOP_c3_mvtadd,
        TOP_c3_mvtadds,
        TOP_c3_revb,
        TOP_c3_round,
        TOP_c3_saadd_a,
        TOP_c3_saaddh_a,
        TOP_c3_saadds,
        TOP_c3_saaddsh,
        TOP_c3_saddha,
        TOP_c3_saddha_a,
        TOP_c3_samulh_a,
        TOP_c3_samulsh,
        TOP_c3_sashllh,
        TOP_c3_sasub_a,
        TOP_c3_sasubh_a,
        TOP_c3_sasubs,
        TOP_c3_sasubsh,
        TOP_c3_st,
	TOP_c3_trback,
	TOP_c3_viterbi,
	TOP_c3_fft,
	TOP_c3_fftld,
	TOP_c3_fftst,
        // new C3
        TOP_C3_aadda,
        TOP_C3_nega,
        TOP_C3_bitr,
        TOP_C3_cs,
        TOP_C3_dadd,
        TOP_C3_dmac,
        TOP_C3_dmacn,
        TOP_C3_dmac_a,
        TOP_C3_dmacn_a,
        TOP_C3_dmula,
        TOP_C3_dmulan,
        TOP_C3_dmula_a,
        TOP_C3_dmulan_a,
        TOP_C3_dshll_i,
        TOP_C3_dshrl_i,
        TOP_C3_dsub,
        TOP_C3_ffe,
        TOP_C3_fftld,
        TOP_C3_fftst,
        TOP_C3_ld,
        TOP_C3_lead,
        TOP_C3_mac,
        TOP_C3_mac_a,
        TOP_C3_mac_ar,
        TOP_C3_mac_i,
        TOP_C3_macn,
        TOP_C3_macn_a,
        TOP_C3_macn_ar,
        TOP_C3_macn_i,
        TOP_C3_mula,
        TOP_C3_mula_a,
        TOP_C3_mula_ar,
        TOP_C3_mula_i,
        TOP_C3_mulan,
        TOP_C3_muls,
        TOP_C3_mulus,
        TOP_C3_mvfs,
        TOP_C3_mvts,
        TOP_C3_revb,
        TOP_C3_round,
        TOP_C3_saadd_a,
        TOP_C3_saaddh_a,
        TOP_C3_saadds,
        TOP_C3_saaddsh,
        TOP_C3_sadda,
        TOP_C3_sadda_a,
        TOP_C3_samulh_a,
        TOP_C3_samulsh,
        TOP_C3_sasub_a,
        TOP_C3_sasubh_a,
        TOP_C3_sasubs,
        TOP_C3_sasubsh,
        TOP_C3_shav,
        TOP_C3_shlafa_i,
        TOP_C3_shlata_i,
        TOP_C3_shla_i,
        TOP_C3_shrafa_i,
        TOP_C3_shrata_i,
        TOP_C3_shra_i,
        TOP_C3_st,
        TOP_C3_subc,
        TOP_C3_mvtacc,
        TOP_C3_mvtaddr,
        TOP_C3_mvtadds,
        TOP_C3_mvfacc,
        TOP_C3_mvfaddr,
        TOP_C3_mvfadds,
        // end
        TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_DSP, 0);
#endif
  Machine_Done();
}
