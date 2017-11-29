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
// Group TOPS with similar packing format together. 
/////////////////////////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. ALU
//   2. Integer
//   3. Memory
//   4. Branch
//   5. Float
//
// Within each Pack_Type instructions are listed in the order as shown
// in the IA-64 instructions formats manual
/////////////////////////////////////

#include <stddef.h>
#include "topcode.h"
#include "isa_pack_gen.h"
 
main()
{
  ISA_PACK_TYPE	a1;	// Integer ALU -- Register-Register

  ISA_Pack_Begin("loongson", 41);

  /* ===== A24: Integer ALU for mips ===== */
  a1 = ISA_Pack_Type_Create("a1");
  Operand(0, 0, 0, 0);		// pq
  Operand(1, 0, 21, 5);		// rs
  Operand(2, 0, 16, 5);		// rt
  Result(0, 11, 5);		// rd
  Instruction_Pack_Group(a1,
  	TOP_add,		0x00000000000ULL,
 	TOP_addi,         0x00000000000ULL, 
 	TOP_addiu,         0x00000000000ULL, 
 	TOP_addu,         0x00000000000ULL,
 	TOP_and,         0x00000000000ULL, 
 	TOP_andi,         0x00000000000ULL, 
 	TOP_beq,         0x00000000000ULL, 
 	TOP_bgez,         0x00000000000ULL, 
 	TOP_bgezal,         0x00000000000ULL, 
 	TOP_bgtz,         0x00000000000ULL, 
 	TOP_blez,         0x00000000000ULL, 
        TOP_bltz,         0x00000000000ULL, 
        TOP_bltzal,         0x00000000000ULL, 
        TOP_bne,         0x00000000000ULL, 
        TOP_dadd,         0x00000000000ULL, 
        TOP_daddi,         0x00000000000ULL, 
        TOP_daddiu,         0x00000000000ULL, 
        TOP_daddu,         0x00000000000ULL,
        TOP_ddiv,         0x00000000000ULL, 
        TOP_ddivu,         0x00000000000ULL, 
        TOP_div,         0x00000000000ULL, 
        TOP_divu,         0x00000000000ULL, 
        TOP_dmult,         0x00000000000ULL, 
        TOP_dmultu,         0x00000000000ULL, 
        TOP_dsll,         0x00000000000ULL, 
        TOP_dsllv,         0x00000000000ULL,
        TOP_dsra,         0x00000000000ULL,
        TOP_dsra32,         0x00000000000ULL, 
        TOP_dsrav,         0x00000000000ULL, 
        TOP_dsrl,         0x00000000000ULL, 
        TOP_dsrl32,         0x00000000000ULL,
        TOP_dsrlv,         0x00000000000ULL, 
        TOP_dsub,         0x00000000000ULL,
        TOP_dsubu,         0x00000000000ULL, 
        TOP_j,         0x00000000000ULL, 
        TOP_jal,         0x00000000000ULL, 
        TOP_jalr,         0x00000000000ULL, 
        TOP_jr,         0x00000000000ULL, 
        TOP_lb,         0x00000000000ULL, 
        TOP_lbu,         0x00000000000ULL, 
        TOP_ld,         0x00000000000ULL,
        TOP_ldl,         0x00000000000ULL, 
        TOP_ldr,         0x00000000000ULL, 
        TOP_lh,         0x00000000000ULL, 
        TOP_lhu,         0x00000000000ULL,
        TOP_ll,         0x00000000000ULL, 
        TOP_lld,         0x00000000000ULL, 
        TOP_lui,         0x00000000000ULL,
        TOP_lw,         0x00000000000ULL, 
        TOP_lwl,         0x00000000000ULL, 
        TOP_lwr,         0x00000000000ULL, 
        TOP_lwu,         0x00000000000ULL,
        TOP_mfhi,         0x00000000000ULL, 
        TOP_mflo,         0x00000000000ULL, 
        TOP_mthi,         0x00000000000ULL, 
        TOP_mtlo,         0x00000000000ULL, 
        TOP_mult,         0x00000000000ULL, 
        TOP_multu,         0x00000000000ULL, 
        TOP_nor,         0x00000000000ULL, 
        TOP_or,         0x00000000000ULL, 
        TOP_ori,         0x00000000000ULL, 
        TOP_sb,         0x00000000000ULL, 
        TOP_sc,         0x00000000000ULL, 
        TOP_scd,         0x00000000000ULL,
        TOP_sd,         0x00000000000ULL, 
        TOP_sdl,         0x00000000000ULL, 
        TOP_sdr,         0x00000000000ULL, 
        TOP_sh,         0x00000000000ULL,
        TOP_sll,         0x00000000000ULL, 
        TOP_sllv,         0x00000000000ULL, 
        TOP_slt,         0x00000000000ULL, 
        TOP_slti,         0x00000000000ULL, 
        TOP_sltiu,         0x00000000000ULL, 
        TOP_sltu,         0x00000000000ULL, 
        TOP_sra,         0x00000000000ULL, 
        TOP_srav,         0x00000000000ULL,
        TOP_srl,         0x00000000000ULL, 
        TOP_srlv,         0x00000000000ULL, 
        TOP_sub,         0x00000000000ULL, 
        TOP_subu,         0x00000000000ULL, 
        TOP_sw,         0x00000000000ULL, 
        TOP_swl,         0x00000000000ULL, 
        TOP_swr,         0x00000000000ULL, 
        TOP_xor,         0x00000000000ULL, 
        TOP_xori,         0x00000000000ULL, 
        TOP_break,         0x00000000000ULL,
        TOP_nop,         0x00000000000ULL, 
        TOP_abs_s,         0x00000000000ULL, 
        TOP_add_s,         0x00000000000ULL,
        TOP_bc1f,         0x00000000000ULL, 
        TOP_bc1fl,         0x00000000000ULL, 
        TOP_bc1t,         0x00000000000ULL, 
        TOP_bc1tl,         0x00000000000ULL, 
        TOP_c_t_s,         0x00000000000ULL, 
        TOP_c_or_s,         0x00000000000ULL, 
        TOP_c_neq_s,         0x00000000000ULL, 
        TOP_c_olg_s,         0x00000000000ULL, 
        TOP_c_uge_s,         0x00000000000ULL, 
        TOP_c_oge_s,         0x00000000000ULL, 
        TOP_c_ugt_s,         0x00000000000ULL, 
        TOP_c_ogt_s,         0x00000000000ULL, 
        TOP_c_st_s,         0x00000000000ULL, 
        TOP_c_gle_s,         0x00000000000ULL, 
        TOP_c_sne_s,         0x00000000000ULL, 
        TOP_c_gl_s,         0x00000000000ULL, 
        TOP_c_nlt_s,         0x00000000000ULL, 
        TOP_c_ge_s,         0x00000000000ULL, 
        TOP_c_nle_s,         0x00000000000ULL, 
        TOP_c_gt_s,         0x00000000000ULL, 
        TOP_c_f_s,         0x00000000000ULL, 
        TOP_c_un_s,         0x00000000000ULL, 
        TOP_c_eq_s,         0x00000000000ULL, 
        TOP_c_ueq_s,         0x00000000000ULL, 
        TOP_c_olt_s,         0x00000000000ULL, 
        TOP_c_ult_s,         0x00000000000ULL, 
        TOP_c_ole_s,         0x00000000000ULL, 
        TOP_c_ule_s,         0x00000000000ULL, 
        TOP_c_sf_s,         0x00000000000ULL, 
        TOP_c_ngle_s,         0x00000000000ULL, 
        TOP_c_seq_s,         0x00000000000ULL, 
        TOP_c_ngl_s,         0x00000000000ULL, 
        TOP_c_lt_s,         0x00000000000ULL, 
        TOP_c_nge_s,         0x00000000000ULL, 
        TOP_c_le_s,         0x00000000000ULL, 
        TOP_c_ngt_s,         0x00000000000ULL, 
        TOP_ceil_l_s,         0x00000000000ULL, 
        TOP_ceil_w_s,         0x00000000000ULL, 
        TOP_cfc1,         0x00000000000ULL, 
        TOP_ctc1,         0x00000000000ULL, 
        TOP_cvt_l_s,         0x00000000000ULL, 
        TOP_cvt_s_w,         0x00000000000ULL, 
        TOP_cvt_s_l,         0x00000000000ULL, 
        TOP_cvt_w_s,         0x00000000000ULL, 
        TOP_div_s,         0x00000000000ULL, 
        TOP_dmfc1,         0x00000000000ULL, 
        TOP_dmtc1,         0x00000000000ULL, 
        TOP_floor_l_s,         0x00000000000ULL, 
        TOP_floor_w_s,         0x00000000000ULL, 
        TOP_lwc1,         0x00000000000ULL, 
        TOP_mfc1,         0x00000000000ULL, 
        TOP_mov_s,         0x00000000000ULL, 
        TOP_mtc1,         0x00000000000ULL, 
        TOP_mul_s,         0x00000000000ULL, 
        TOP_neg_s,         0x00000000000ULL, 
        TOP_round_l_s,         0x00000000000ULL, 
        TOP_round_w_s,         0x00000000000ULL, 
        TOP_sub_s,         0x00000000000ULL, 
        TOP_swc1,         0x00000000000ULL, 
        TOP_trunc_l_s,         0x00000000000ULL, 
        TOP_trunc_w_s,         0x00000000000ULL, 
        TOP_abs_d,         0x00000000000ULL, 
        TOP_add_d,         0x00000000000ULL, 
        TOP_c_t_d,         0x00000000000ULL, 
        TOP_c_or_d,         0x00000000000ULL, 
        TOP_c_neq_d,         0x00000000000ULL, 
        TOP_c_olg_d,         0x00000000000ULL, 
        TOP_c_uge_d,         0x00000000000ULL, 
        TOP_c_oge_d,         0x00000000000ULL, 
        TOP_c_ugt_d,         0x00000000000ULL, 
        TOP_c_ogt_d,         0x00000000000ULL,
        TOP_c_st_d,         0x00000000000ULL,
        TOP_c_gle_d,         0x00000000000ULL,
        TOP_c_sne_d,         0x00000000000ULL,
        TOP_c_gl_d,         0x00000000000ULL,
        TOP_c_nlt_d,         0x00000000000ULL,
        TOP_c_ge_d,         0x00000000000ULL,
        TOP_c_nle_d,         0x00000000000ULL,
        TOP_c_gt_d,         0x00000000000ULL,
        TOP_c_f_d,         0x00000000000ULL,
        TOP_c_un_d,         0x00000000000ULL,
        TOP_c_eq_d,         0x00000000000ULL,
        TOP_c_ueq_d,         0x00000000000ULL,
        TOP_c_olt_d,         0x00000000000ULL,
        TOP_c_ult_d,         0x00000000000ULL,
        TOP_c_ole_d,         0x00000000000ULL,
        TOP_c_ule_d,         0x00000000000ULL,
        TOP_c_sf_d,         0x00000000000ULL,
        TOP_c_ngle_d,         0x00000000000ULL,
        TOP_c_seq_d,         0x00000000000ULL,
        TOP_c_ngl_d,         0x00000000000ULL,
        TOP_c_lt_d,         0x00000000000ULL,
        TOP_c_nge_d,         0x00000000000ULL,
        TOP_c_le_d,         0x00000000000ULL,
        TOP_c_ngt_d,         0x00000000000ULL,
        TOP_ceil_l_d,         0x00000000000ULL,
        TOP_ceil_w_d,         0x00000000000ULL,
        TOP_cvt_l_d,         0x00000000000ULL,
        TOP_cvt_s_d,         0x00000000000ULL,
        TOP_cvt_w_d,         0x00000000000ULL,
        TOP_div_d,         0x00000000000ULL,
        TOP_floor_l_d,         0x00000000000ULL,
        TOP_floor_w_d,         0x00000000000ULL,
        TOP_mov_d,         0x00000000000ULL, 
        TOP_mul_d,         0x00000000000ULL, 
        TOP_neg_d,         0x00000000000ULL, 
        TOP_round_l_d,         0x00000000000ULL,
        TOP_round_w_d,         0x00000000000ULL, 
        TOP_sub_d,         0x00000000000ULL, 
        TOP_trunc_l_d,         0x00000000000ULL, 
        TOP_trunc_w_d,         0x00000000000ULL, 
        TOP_cvt_d_s,         0x00000000000ULL, 
        TOP_cvt_d_w,         0x00000000000ULL, 
        TOP_cvt_d_l,         0x00000000000ULL, 
        TOP_ldc1,         0x00000000000ULL, 
        TOP_sdc1,         0x00000000000ULL, 
        TOP_dsll32,         0x00000000000ULL, 
        TOP_sqrt_s,         0x00000000000ULL, 
        TOP_sqrt_d,         0x00000000000ULL, 
        TOP_teq,         0x00000000000ULL, 
        TOP_teqi,         0x00000000000ULL, 
        TOP_tge,         0x00000000000ULL, 
        TOP_tgei,         0x00000000000ULL, 
        TOP_tgeiu,         0x00000000000ULL, 
        TOP_tgeu,         0x00000000000ULL, 
        TOP_madd_d,         0x00000000000ULL,
        TOP_madd_s,         0x00000000000ULL,
        TOP_msub_d,         0x00000000000ULL,
        TOP_msub_s,         0x00000000000ULL,
        TOP_nmadd_d,         0x00000000000ULL,
        TOP_nmadd_s,         0x00000000000ULL,
        TOP_nmsub_d,         0x00000000000ULL,
        TOP_nmsub_s,         0x00000000000ULL,
        TOP_movz,         0x00000000000ULL,
        TOP_movn,         0x00000000000ULL,
        TOP_divlo,        0x00000000000ULL,
        TOP_divhi,        0x00000000000ULL,
        TOP_divulo,        0x00000000000ULL,
        TOP_divuhi,        0x00000000000ULL,

        TOP_mult_g,        0x00000000000ULL,
        TOP_multu_g,        0x00000000000ULL,
        TOP_dmult_g,        0x00000000000ULL,
        TOP_dmultu_g,        0x00000000000ULL,
        TOP_div_g,        0x00000000000ULL,
        TOP_divu_g,        0x00000000000ULL,
        TOP_ddiv_g,        0x00000000000ULL,
        TOP_ddivu_g,        0x00000000000ULL,
        TOP_mod_g,        0x00000000000ULL,
        TOP_modu_g,        0x00000000000ULL,
        TOP_dmod_g,        0x00000000000ULL,
        TOP_dmodu_g,        0x00000000000ULL,
        
        TOP_UNDEFINED);

  ISA_Pack_End();
  return 0;
}
