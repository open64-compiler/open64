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
//  Generate ISA bundle information
///////////////////////////////////////

#include <stddef.h>
#include "topcode.h"
#include "isa_bundle_gen.h"

main()
{
  ISA_EXEC_UNIT_TYPE
    R_Unit,  // Reserved unit type
    I_Unit,  // Integer execution type
    M_Unit,  // Memory execution type
    F_Unit,  // Floating-point execution type
    B_Unit,  // Branch execution type
    L_Unit,  // Long immediate type
    B2_Unit, // specific B-unit category in which instruction can occupy
	     // only in slot2 position. 
    I2_Unit; // specific I-unit category for template2 specification. Only
             // movl, break and nop fall in this category and occupy slot2
	     // position.

  ISA_Bundle_Begin("loongson", 64);

  /* ===== Specification for bundle packing  ===== */
  ISA_Bundle_Pack_Create(ISA_Bundle_Pack_Little_Endian);
  Pack_Slot(0, 0, 0, 64);
  
  /* ===== Specification for R_Unit Type ===== */
  R_Unit = ISA_Exec_Unit_Type_Create("R_Unit", NULL);
  Instruction_Exec_Unit_Group(R_Unit,
			      TOP_UNDEFINED);

  /* ===== Specification for I_Unit Type ===== */
  I_Unit = ISA_Exec_Unit_Type_Create("I_Unit", NULL);
  Instruction_Exec_Unit_Group(I_Unit,
			      // Integer:
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
			      TOP_ddiv,
			      TOP_ddivu,
			      TOP_div,
			      TOP_divu,
			      TOP_dmult,
			      TOP_dmultu,
			      TOP_dsll,
			      TOP_dsll32,
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
			      TOP_mfhi,
			      TOP_mfhi,
			      TOP_mflo,
			      TOP_mthi,
			      TOP_mtlo,
			      TOP_mult,
			      TOP_multu,
			      TOP_nor,
			      TOP_or,
			      TOP_ori,
			      TOP_sll,
			      TOP_sllv,
			      TOP_slt,
			      TOP_slti,
			      TOP_sltiu,
			      TOP_sltu,
			      TOP_sub,
			      TOP_subu,
			      TOP_sra,
			      TOP_srav,
			      TOP_srl,
			      TOP_srlv,
			      TOP_xor,
			      TOP_xori,
			      TOP_nop,
			      TOP_break,
			      TOP_dmtc1,
			      TOP_mtc1,
			      TOP_teq,
			      TOP_teqi,
			      TOP_tge,
			      TOP_tgei,
			      TOP_tgeiu,
			      TOP_tgeu,
			      TOP_movz,
			      TOP_movn,
			      TOP_divlo,
			      TOP_divhi,
			      TOP_divulo,
			      TOP_divuhi,
			      TOP_mult_g,
			      TOP_multu_g,
			      TOP_dmult_g,
			      TOP_dmultu_g,
			      TOP_div_g,
			      TOP_divu_g,
			      TOP_ddiv_g,
			      TOP_ddivu_g,
			      TOP_mod_g,
			      TOP_modu_g,
			      TOP_dmod_g,
			      TOP_dmodu_g,

			      TOP_UNDEFINED);

  /* ===== Specification for M_Unit Type ===== */
  M_Unit = ISA_Exec_Unit_Type_Create("M_Unit", NULL);
  Instruction_Exec_Unit_Group(M_Unit,
			      // Memory:
			      TOP_lb,
			      TOP_lbu,
			      TOP_ld,
			      TOP_ldl,
			      TOP_ldr,
			      TOP_lh,
			      TOP_lhu,
			      TOP_ll,
			      TOP_lld,
			      TOP_lui,
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
			      TOP_nop,
			      TOP_break,
			      TOP_cfc1,
			      TOP_ctc1,
			      TOP_ldc1,
			      TOP_lwc1,
			      TOP_sdc1,
			      TOP_swc1,
			      TOP_UNDEFINED);

  /* ===== Specification for B_Unit Type ===== */
  B_Unit = ISA_Exec_Unit_Type_Create("B_Unit", NULL);
  Instruction_Exec_Unit_Group(B_Unit,
			      TOP_UNDEFINED);

  /* ===== Specification for F_Unit Type ===== */
  F_Unit = ISA_Exec_Unit_Type_Create("F_Unit", NULL);
  Instruction_Exec_Unit_Group(F_Unit,
			      TOP_nop,
			      TOP_break,
			      TOP_abs_s,
			      TOP_abs_d,
			      TOP_add_s,
			      TOP_add_d,
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
			      TOP_ceil_l_s,
			      TOP_ceil_l_d,
			      TOP_ceil_w_s,
			      TOP_ceil_w_d,
			      TOP_cvt_d_s,
			      TOP_cvt_d_w,
			      TOP_cvt_d_l,
			      TOP_cvt_l_s,
			      TOP_cvt_l_d,
			      TOP_cvt_s_d,
			      TOP_cvt_s_w,
			      TOP_cvt_s_l,
			      TOP_cvt_w_s,
			      TOP_cvt_w_d,
			      TOP_div_s,
			      TOP_div_d,
			      TOP_dmfc1,
			      TOP_floor_l_s,
			      TOP_floor_l_d,
			      TOP_floor_w_s,
			      TOP_floor_w_d,
			      TOP_mfc1,
			      TOP_mov_s,
			      TOP_mov_d,
			      TOP_mul_s,
			      TOP_mul_d,
			      TOP_neg_s,
			      TOP_neg_d,
			      TOP_round_l_s,
			      TOP_round_l_d,
			      TOP_round_w_s,
			      TOP_round_w_d,
			      TOP_sub_s,
			      TOP_sub_d,
			      TOP_trunc_l_s,
			      TOP_trunc_l_d,
			      TOP_trunc_w_s,
			      TOP_trunc_w_d,
			      TOP_sqrt_s,
			      TOP_sqrt_d,
			      TOP_madd_d,
			      TOP_madd_s,
			      TOP_msub_d,
			      TOP_msub_s,
			      TOP_nmadd_d,
			      TOP_nmadd_s,
			      TOP_nmsub_d,
			      TOP_nmsub_s,
			      TOP_UNDEFINED);

  /* ===== Specification for L_Unit Type ===== */
  L_Unit = ISA_Exec_Unit_Type_Create("L_Unit", NULL);
  Instruction_Exec_Unit_Group(L_Unit,
			      TOP_UNDEFINED);

  /* ===== Specification for I2_Unit Type ===== */
  I2_Unit = ISA_Exec_Unit_Type_Create("I2_Unit", I_Unit);
  Instruction_Exec_Unit_Group(I2_Unit,
			      TOP_nop,
			      TOP_break,
			      TOP_UNDEFINED);
 
  /* ===== Specification for B2_Unit Type ===== */
  B2_Unit = ISA_Exec_Unit_Type_Create("B2_Unit", B_Unit);
  Instruction_Exec_Unit_Group(B2_Unit,
			      TOP_UNDEFINED);

  ISA_Bundle_Type_Create("i",".i",1);
  Slot (0, I_Unit);
  ISA_Bundle_Type_Create("m",".m",1);
  Slot (0, M_Unit);
  ISA_Bundle_Type_Create("f",".f",1);
  Slot (0, F_Unit);
  ISA_Bundle_Type_Create("b",".b",1);
  Slot (0, B2_Unit);
  ISA_Bundle_Type_Create("l",".l",1);
  Slot (0, L_Unit);


  ISA_Bundle_End();
  return 0;
}
