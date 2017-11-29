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
// Generate instruction decoding information.
/////////////////////////////////////
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/isa/ia64/isa_decode.cxx,v $

#include "topcode.h"
#include "isa_decode_gen.h"
#include "targ_isa_bundle.h"
 
main()
{
  ISA_Decode_Begin("ia64");

  STATE ex_unit = Create_Unit_State("ex_unit", 0, 3);
  STATE integer_or_alu = Create_Inst_State("integer_or_alu", 0, 40, 1);
  STATE memory_or_alu = Create_Inst_State("memory_or_alu", 0, 40, 1);

  STATE alu = Create_Inst_State("alu", 0, 37, 3);
  STATE integer = Create_Inst_State("integer", 0, 37, 3);
  STATE memory = Create_Inst_State("memory", 0, 37, 3);
  STATE floating = Create_Inst_State("floating", 0, 37, 4);
  STATE branch = Create_Inst_State("branch", 0, 37, 4);
  STATE lx_unit = Create_Inst_State("lx_unit", 1, 37, 4);

  Transitions(ex_unit,
        ISA_EXEC_I_Unit, integer_or_alu,
	ISA_EXEC_M_Unit, memory_or_alu,
	ISA_EXEC_B_Unit, branch,
	ISA_EXEC_F_Unit, floating,
	ISA_EXEC_L_Unit, lx_unit, 
	END_TRANSITIONS);

  Transitions(integer_or_alu,
	0, integer,
	1, alu,
	END_TRANSITIONS);

  Transitions(memory_or_alu,
	0, memory,
	1, alu,
	END_TRANSITIONS);

/* ===== ALU instructions ===== */

  STATE alu_mm = Create_Inst_State("alu_mm", 0, 33, 3);
  STATE compare_c = Create_Inst_State("compare_c", 0, 12, 1);
  STATE compare_d = Create_Inst_State("compare_d", 0, 12, 1);
  STATE compare_e = Create_Inst_State("compare_e", 0, 12, 1);

  STATE integer_alu = Create_Inst_State("integer_alu", 0, 27, 6);
  STATE mm_alu = Create_Inst_State("mm_alu", 0, 36, 1);
  STATE mm_alu_1_2 = Create_Inst_State("mm_alu_1_2", 0, 27, 7);
  STATE mm_alu_4 = Create_Inst_State("mm_alu_4", 0, 27, 6);

  STATE compare_c0 = Create_Inst_State("compare_c0", 0, 33, 4);
  STATE compare_c1 = Create_Inst_State("compare_c1", 0, 33, 4);
  STATE compare_d0 = Create_Inst_State("compare_d0", 0, 33, 4);
  STATE compare_d1 = Create_Inst_State("compare_d1", 0, 33, 4);
  STATE compare_e0 = Create_Inst_State("compare_e0", 0, 33, 4);
  STATE compare_e1 = Create_Inst_State("compare_e1", 0, 33, 4);

  Transitions(alu,
	0x8 - 8, alu_mm,
	0x9 - 8, Final(TOP_addl),
	0xc - 8, compare_c,
	0xd - 8, compare_d,
	0xe - 8, compare_e,
	END_TRANSITIONS);

  Transitions(alu_mm,
	0, integer_alu,
	2, mm_alu,
	3, mm_alu,
	4, Final(TOP_adds),
	6, Final(TOP_addp4_i),
	END_TRANSITIONS);

  Transitions(integer_alu,
	 0, Final(TOP_add),
	 1, Final(TOP_add_1),
	 4, Final(TOP_sub_1),
	 5, Final(TOP_sub),
	 8, Final(TOP_addp4),
	12, Final(TOP_and),
	13, Final(TOP_andcm),
	14, Final(TOP_or),
	15, Final(TOP_xor),
	16, Final(TOP_shladd),
	17, Final(TOP_shladd),
	18, Final(TOP_shladd),
	19, Final(TOP_shladd),
	24, Final(TOP_shladdp4),
	25, Final(TOP_shladdp4),
	26, Final(TOP_shladdp4),
	27, Final(TOP_shladdp4),
	37, Final(TOP_sub_i),
	44, Final(TOP_and_i),
	45, Final(TOP_andcm_i),
	46, Final(TOP_or_i),
	47, Final(TOP_xor_i),
	END_TRANSITIONS);

  Transitions(mm_alu,
	0, mm_alu_1_2,
	1, mm_alu_4,
	END_TRANSITIONS);

  Transitions(mm_alu_1_2,
	  0, Final(TOP_padd1),
	  1, Final(TOP_padd1_sss),
	  2, Final(TOP_padd1_uuu),
	  3, Final(TOP_padd1_uus),
	  4, Final(TOP_psub1),
	  5, Final(TOP_psub1_sss),
	  6, Final(TOP_psub1_uuu),
	  7, Final(TOP_psub1_uus),
	 10, Final(TOP_pavg1),
	 11, Final(TOP_pavg1_raz),
	 14, Final(TOP_pavgsub1),
	 36, Final(TOP_pcmp1_eq),
	 37, Final(TOP_pcmp1_gt),
	 64, Final(TOP_padd2),
	 65, Final(TOP_padd2_sss),
	 66, Final(TOP_padd2_uuu),
	 67, Final(TOP_padd2_uus),
	 68, Final(TOP_psub2),
	 69, Final(TOP_psub2_sss),
	 70, Final(TOP_psub2_uuu),
	 71, Final(TOP_psub2_uus),
	 74, Final(TOP_pavg2),
	 75, Final(TOP_pavg2_raz),
	 78, Final(TOP_pavgsub2),
	 80, Final(TOP_pshladd2),
	 81, Final(TOP_pshladd2),
	 82, Final(TOP_pshladd2),
	 83, Final(TOP_pshladd2),
	 88, Final(TOP_pshradd2),
	 89, Final(TOP_pshradd2),
	 90, Final(TOP_pshradd2),
	 91, Final(TOP_pshradd2),
	100, Final(TOP_pcmp2_eq),
	101, Final(TOP_pcmp2_gt),
	END_TRANSITIONS);

  Transitions(mm_alu_4,
	 0, Final(TOP_padd4),
	 4, Final(TOP_psub4),
	36, Final(TOP_pcmp4_eq),
	37, Final(TOP_pcmp4_gt),
	END_TRANSITIONS);

  Transitions(compare_c,
	0, compare_c0,
	1, compare_c1,
	END_TRANSITIONS);

  Transitions(compare_c0,
	 0, Final(TOP_cmp_lt),
	 1, Final(TOP_cmp_eq_and),
	 2, Final(TOP_cmp4_lt),
	 3, Final(TOP_cmp4_eq_and),
	 4, Final(TOP_cmp_i_lt),
	 5, Final(TOP_cmp_i_eq_and),
	 6, Final(TOP_cmp4_i_lt),
	 7, Final(TOP_cmp4_i_eq_and),
	 8, Final(TOP_cmp_z1_gt_and),
	 9, Final(TOP_cmp_z1_ge_and),
	10, Final(TOP_cmp4_z1_gt_and),
	11, Final(TOP_cmp4_z1_ge_and),
	12, Final(TOP_cmp_i_lt),
	13, Final(TOP_cmp_i_eq_and),
	14, Final(TOP_cmp4_i_lt),
	15, Final(TOP_cmp4_i_eq_and),
	END_TRANSITIONS);

  Transitions(compare_c1,
	 0, Final(TOP_cmp_lt_unc),
	 1, Final(TOP_cmp_ne_and),
	 2, Final(TOP_cmp4_lt_unc),
	 3, Final(TOP_cmp4_ne_and),
	 4, Final(TOP_cmp_i_lt_unc),
	 5, Final(TOP_cmp_i_ne_and),
	 6, Final(TOP_cmp4_i_lt_unc),
	 7, Final(TOP_cmp4_i_ne_and),
	 8, Final(TOP_cmp_z1_le_and),
	 9, Final(TOP_cmp_z1_lt_and),
	10, Final(TOP_cmp4_z1_le_and),
	11, Final(TOP_cmp4_z1_lt_and),
	12, Final(TOP_cmp_i_lt_unc),
	13, Final(TOP_cmp_i_ne_and),
	14, Final(TOP_cmp4_i_lt_unc),
	15, Final(TOP_cmp4_i_ne_and),
	END_TRANSITIONS);

  Transitions(compare_d,
	0, compare_d0,
	1, compare_d1,
	END_TRANSITIONS);

  Transitions(compare_d0,
	 0, Final(TOP_cmp_ltu),
	 1, Final(TOP_cmp_eq_or),
	 2, Final(TOP_cmp4_ltu),
	 3, Final(TOP_cmp4_eq_or),
	 4, Final(TOP_cmp_i_ltu),
	 5, Final(TOP_cmp_i_eq_or),
	 6, Final(TOP_cmp4_i_ltu),
	 7, Final(TOP_cmp4_i_eq_or),
	 8, Final(TOP_cmp_z1_gt_or),
	 9, Final(TOP_cmp_z1_ge_or),
	10, Final(TOP_cmp4_z1_gt_or),
	11, Final(TOP_cmp4_z1_ge_or),
	12, Final(TOP_cmp_i_ltu),
	13, Final(TOP_cmp_i_eq_or),
	14, Final(TOP_cmp4_i_ltu),
	15, Final(TOP_cmp4_i_eq_or),
	END_TRANSITIONS);

  Transitions(compare_d1,
	 0, Final(TOP_cmp_ltu_unc),
	 1, Final(TOP_cmp_ne_or),
	 2, Final(TOP_cmp4_ltu_unc),
	 3, Final(TOP_cmp4_ne_or),
	 4, Final(TOP_cmp_i_ltu_unc),
	 5, Final(TOP_cmp_i_ne_or),
	 6, Final(TOP_cmp4_i_ltu_unc),
	 7, Final(TOP_cmp4_i_ne_or),
	 8, Final(TOP_cmp_z1_le_or),
	 9, Final(TOP_cmp_z1_lt_or),
	10, Final(TOP_cmp4_z1_le_or),
	11, Final(TOP_cmp4_z1_lt_or),
	12, Final(TOP_cmp_i_ltu_unc),
	13, Final(TOP_cmp_i_ne_or),
	14, Final(TOP_cmp4_i_ltu_unc),
	15, Final(TOP_cmp4_i_ne_or),
	END_TRANSITIONS);

  Transitions(compare_e,
	0, compare_e0,
	1, compare_e1,
	END_TRANSITIONS);

  Transitions(compare_e0,
	 0, Final(TOP_cmp_eq),
	 1, Final(TOP_cmp_eq_or_andcm),
	 2, Final(TOP_cmp4_eq),
	 3, Final(TOP_cmp4_eq_or_andcm),
	 4, Final(TOP_cmp_i_eq),
	 5, Final(TOP_cmp_i_eq_or_andcm),
	 6, Final(TOP_cmp4_i_eq),
	 7, Final(TOP_cmp4_i_eq_or_andcm),
	 8, Final(TOP_cmp_z1_gt_or_andcm),
	 9, Final(TOP_cmp_z1_ge_or_andcm),
	10, Final(TOP_cmp4_z1_gt_or_andcm),
	11, Final(TOP_cmp4_z1_ge_or_andcm),
	12, Final(TOP_cmp_i_eq),
	13, Final(TOP_cmp_i_eq_or_andcm),
	14, Final(TOP_cmp4_i_eq),
	15, Final(TOP_cmp4_i_eq_or_andcm),
	END_TRANSITIONS);

  Transitions(compare_e1,
	 0, Final(TOP_cmp_eq_unc),
	 1, Final(TOP_cmp_ne_or_andcm),
	 2, Final(TOP_cmp4_eq_unc),
	 3, Final(TOP_cmp4_ne_or_andcm),
	 4, Final(TOP_cmp_i_eq_unc),
	 5, Final(TOP_cmp_i_ne_or_andcm),
	 6, Final(TOP_cmp4_i_eq_unc),
	 7, Final(TOP_cmp4_i_ne_or_andcm),
	 8, Final(TOP_cmp_z1_le_or_andcm),
	 9, Final(TOP_cmp_z1_lt_or_andcm),
	10, Final(TOP_cmp4_z1_le_or_andcm),
	11, Final(TOP_cmp4_z1_lt_or_andcm),
	12, Final(TOP_cmp_i_eq_unc),
	13, Final(TOP_cmp_i_ne_or_andcm),
	14, Final(TOP_cmp4_i_eq_unc),
	15, Final(TOP_cmp4_i_ne_or_andcm),
	END_TRANSITIONS);

/* ===== Integer instructions ===== */

  STATE misc = Create_Inst_State("misc", 0, 33, 3);
  STATE shift_test = Create_Inst_State("shift_test", 0, 33, 3);
  STATE movl = Create_Inst_State("movl", 0, 20, 1);
  STATE mpy_shift = Create_Inst_State("mpy_shift", 0, 32, 5);

  STATE misc_0 = Create_Inst_State("misc_0", 0, 27, 6);

  STATE move_to_br = Create_Inst_State("move_to_br", 0, 22, 1);

  STATE tbit_tnat_0 = Create_Inst_State("tbit_tnat_0", 0, 36, 1);
  STATE tbit_tnat_1 = Create_Inst_State("tbit_tnat_1", 0, 36, 1);
  STATE extract = Create_Inst_State("extract", 0, 13, 1);
  STATE deposit = Create_Inst_State("deposit", 0, 26, 1);

  STATE tbit_tnat_00 = Create_Inst_State("tbit_tnat_00", 0, 12, 2);
  STATE tbit_tnat_01 = Create_Inst_State("tbit_tnat_01", 0, 12, 2);

  STATE tbit_tnat_10 = Create_Inst_State("tbit_tnat_10", 0, 12, 2);
  STATE tbit_tnat_11 = Create_Inst_State("tbit_tnat_11", 0, 12, 2);

  STATE mpy_shift_2 = Create_Inst_State("mpy_shift_2", 0, 28, 4);
  STATE mpy_shift_6 = Create_Inst_State("mpy_shift_6", 0, 28, 4);
  STATE mpy_shift_8 = Create_Inst_State("mpy_shift_8", 0, 28, 4);
  STATE mpy_shift_10 = Create_Inst_State("mpy_shift_10", 0, 28, 4);
  STATE mpy_shift_12 = Create_Inst_State("mpy_shift_12", 0, 28, 4);
  STATE mpy_shift_14 = Create_Inst_State("mpy_shift_14", 0, 28, 4);
  STATE mpy_shift_16 = Create_Inst_State("mpy_shift_16", 0, 28, 4);
  STATE mpy_shift_18 = Create_Inst_State("mpy_shift_18", 0, 28, 4);
  STATE mpy_shift_20 = Create_Inst_State("mpy_shift_20", 0, 28, 4);
  STATE mpy_shift_24 = Create_Inst_State("mpy_shift_24", 0, 28, 4);
  STATE mpy_shift_28 = Create_Inst_State("mpy_shift_28", 0, 28, 4);

  Transitions(integer,
	0, misc,
	4, Final(TOP_dep),
	5, shift_test,
	6, movl,
	7, mpy_shift,
	END_TRANSITIONS);

  Transitions(misc,
	0, misc_0,
	1, Final(TOP_chk_s_i),
	2, Final(TOP_mov_t_pr_i),
	3, Final(TOP_mov_t_pr),
	7, move_to_br,
	END_TRANSITIONS);

  Transitions(misc_0,
	 0, Final(TOP_break_i),
	 1, Final(TOP_nop_i),
	10, Final(TOP_mov_t_ar_i_i),
	16, Final(TOP_zxt1),
	17, Final(TOP_zxt2),
	18, Final(TOP_zxt4),
	20, Final(TOP_sxt1),
	21, Final(TOP_sxt2),
	22, Final(TOP_sxt4),
	24, Final(TOP_czx1_l),
	25, Final(TOP_czx2_l),
	28, Final(TOP_czx1_r),
	29, Final(TOP_czx2_r),
	42, Final(TOP_mov_t_ar_r_i),
	48, Final(TOP_mov_f_ip),
	49, Final(TOP_mov_f_br),
	50, Final(TOP_mov_f_ar_i),
	51, Final(TOP_mov_f_pr),
	END_TRANSITIONS);

  Transitions(move_to_br,
	0, Final(TOP_mov_t_br_i),
	1, Final(TOP_mov_t_br_ret),
	END_TRANSITIONS);

  Transitions(shift_test,
	0, tbit_tnat_0,
	1, tbit_tnat_1,
	2, extract,
	3, deposit,
	6, Final(TOP_shrp),
	7, Final(TOP_dep_i),
	END_TRANSITIONS);

  Transitions(tbit_tnat_0,
	0, tbit_tnat_00,
	1, tbit_tnat_01,
	END_TRANSITIONS);

  Transitions(tbit_tnat_00,
	0, Final(TOP_tbit_z),
	1, Final(TOP_tbit_z_unc),
	2, Final(TOP_tnat_z),
	3, Final(TOP_tnat_z_unc),
	END_TRANSITIONS);

  Transitions(tbit_tnat_01,
	0, Final(TOP_tbit_z_and),
	1, Final(TOP_tbit_nz_and),
	2, Final(TOP_tnat_z_and),
	3, Final(TOP_tnat_nz_and),
	END_TRANSITIONS);

  Transitions(tbit_tnat_1,
	0, tbit_tnat_10,
	1, tbit_tnat_11,
	END_TRANSITIONS);

  Transitions(tbit_tnat_10,
	0, Final(TOP_tbit_z_or),
	1, Final(TOP_tbit_nz_or),
	2, Final(TOP_tnat_z_or),
	3, Final(TOP_tnat_nz_or),
	END_TRANSITIONS);

  Transitions(tbit_tnat_11,
	0, Final(TOP_tbit_z_or_andcm),
	1, Final(TOP_tbit_nz_or_andcm),
	2, Final(TOP_tnat_z_or_andcm),
	3, Final(TOP_tnat_nz_or_andcm),
	END_TRANSITIONS);

  Transitions(extract,
	0, Final(TOP_extr_u),
	1, Final(TOP_extr),
	END_TRANSITIONS);

  Transitions(deposit,
	0, Final(TOP_dep_z),
	1, Final(TOP_dep_i_z),
	END_TRANSITIONS);

  Transitions(movl,
	0, Final(TOP_movl),
	END_TRANSITIONS);

  Transitions(mpy_shift,
	 2, mpy_shift_2,
	 6, mpy_shift_6,
	 8, mpy_shift_8,
	10, mpy_shift_10,
	12, mpy_shift_12,
	14, mpy_shift_14,	
	16, mpy_shift_16,
	18, mpy_shift_18,
	20, mpy_shift_20,
	24, mpy_shift_24,
	28, mpy_shift_28,
	END_TRANSITIONS);

  Transitions(mpy_shift_2,
	 0, Final(TOP_pshr2_u),
	 1, Final(TOP_pmpyshr2_u),
	 2, Final(TOP_pshr2),
	 3, Final(TOP_pmpyshr2),
	 4, Final(TOP_pshl2),
	 5, Final(TOP_pmpyshr2_u),
	 7, Final(TOP_pmpyshr2),
	 9, Final(TOP_pmpyshr2_u),
	11, Final(TOP_pmpyshr2),
	13, Final(TOP_pmpyshr2_u),
	15, Final(TOP_pmpyshr2),
	END_TRANSITIONS);

  Transitions(mpy_shift_6,
	1, Final(TOP_pshr2_i_u),
	3, Final(TOP_pshr2_i),
	9, Final(TOP_popcnt),
	END_TRANSITIONS);

  Transitions(mpy_shift_8,
	 1, Final(TOP_pmin1_u),
	 4, Final(TOP_unpack1_h),
	 5, Final(TOP_pmax1_u),
	 6, Final(TOP_unpack1_l),
	 8, Final(TOP_mix1_r),
	10, Final(TOP_mix1_l),
	11, Final(TOP_psad1),
	END_TRANSITIONS);

  Transitions(mpy_shift_10,
	 0, Final(TOP_pack2_uss),
	 2, Final(TOP_pack2_sss),
	 3, Final(TOP_pmin2),
	 4, Final(TOP_unpack2_h),
	 6, Final(TOP_unpack2_l),
	 7, Final(TOP_pmax2),
	 8, Final(TOP_mix2_r),
	10, Final(TOP_mix2_l),
	13, Final(TOP_pmpy2_r),
	15, Final(TOP_pmpy2_l),
	END_TRANSITIONS);

  Transitions(mpy_shift_12,
	10, Final(TOP_mux1),
	END_TRANSITIONS);

  Transitions(mpy_shift_14,
	 5, Final(TOP_pshl2_i),
	10, Final(TOP_mux2),
	END_TRANSITIONS);

  Transitions(mpy_shift_16,
	0, Final(TOP_pshr4_u),
	2, Final(TOP_pshr4),
	4, Final(TOP_pshl4),
	END_TRANSITIONS);

  Transitions(mpy_shift_18,
	0, Final(TOP_shr_u),
	2, Final(TOP_shr),
	4, Final(TOP_shl),
	END_TRANSITIONS);

  Transitions(mpy_shift_20,
	1, Final(TOP_pshr4_i_u),
	3, Final(TOP_pshr4_i),
	END_TRANSITIONS);

  Transitions(mpy_shift_24,
	 2, Final(TOP_pack4_sss),
	 4, Final(TOP_unpack4_h),
	 6, Final(TOP_unpack4_l),
	 8, Final(TOP_mix4_r),
	10, Final(TOP_mix4_l),
	END_TRANSITIONS);

  Transitions(mpy_shift_28,
	5, Final(TOP_pshl4_i),
	END_TRANSITIONS);

/* ===== Memory instructions ===== */

  STATE sys_mem_0 = Create_Inst_State("sys_mem_0", 0, 33, 3);
  STATE sys_mem_1 = Create_Inst_State("sys_mem_1", 0, 33, 3);
  STATE int_load_store = Create_Inst_State("int_load_store", 0, 36, 1);
  STATE int_load_store_imm = Create_Inst_State("int_load_store_imm", 0, 30, 6);
  STATE fp_load_store = Create_Inst_State("fp_load_store", 0, 36, 1);
  STATE fp_load_store_imm = Create_Inst_State("fp_load_store_imm", 0, 30, 6);

  STATE sys_mem_00 = Create_Inst_State("sys_mem_00", 0, 27, 6);
  STATE sys_mem_10 = Create_Inst_State("sys_mem_10", 0, 27, 6);

  STATE int_load_store_0 = Create_Inst_State("int_load_store_0", 0, 27, 1);
  STATE int_load_store_1 = Create_Inst_State("int_load_store_1", 0, 27, 1);

  STATE int_load_store_nobase = Create_Inst_State("int_load_store_nobase", 0, 30, 6);
  STATE sem_get_fr = Create_Inst_State("sem_get_fr", 0, 30, 6);
  STATE int_load_store_reg = Create_Inst_State("int_load_store_reg", 0, 30, 6);

  STATE fp_load_store_0 = Create_Inst_State("fp_load_store_0", 0, 27, 1);
  STATE fp_load_store_1 = Create_Inst_State("fp_load_store_1", 0, 27, 1);

  STATE fp_load_store_nobase = Create_Inst_State("fp_load_store_nobase", 0, 30, 6);
  STATE fp_load_pair_nobase = Create_Inst_State("fp_load_pair_nobase", 0, 30, 6);
  STATE fp_load_reg = Create_Inst_State("fp_load_reg", 0, 30, 6);
  STATE fp_load_pair_imm = Create_Inst_State("fp_load_pair_imm", 0, 30, 6);

  Transitions(memory,
	0, sys_mem_0,
	1, sys_mem_1,
	4, int_load_store,
	5, int_load_store_imm,
	6, fp_load_store,
	7, fp_load_store_imm,
	END_TRANSITIONS);

  Transitions(sys_mem_0,
	0, sys_mem_00,
	4, Final(TOP_chk_a),
	5, Final(TOP_chk_a),
	6, Final(TOP_chk_f_a),
	7, Final(TOP_chk_f_a),
	END_TRANSITIONS);

  Transitions(sys_mem_00,
	 0, Final(TOP_break_m),
	 1, Final(TOP_nop_m),
	 4, Final(TOP_sum),
	 5, Final(TOP_rum),
	 6, Final(TOP_ssm),
	 7, Final(TOP_rsm),
	10, Final(TOP_loadrs),
	12, Final(TOP_flushrs),
	16, Final(TOP_invala),
	18, Final(TOP_invala_e),
	19, Final(TOP_invala_f_e),
	20, Final(TOP_sum),
	21, Final(TOP_rum),
	22, Final(TOP_ssm),
	23, Final(TOP_rsm),
	32, Final(TOP_fwb),
	34, Final(TOP_mf),
	35, Final(TOP_mf_a),
	36, Final(TOP_sum),
	37, Final(TOP_rum),
	38, Final(TOP_ssm),
	39, Final(TOP_rsm),
	40, Final(TOP_mov_t_ar_i_m),
	48, Final(TOP_srlz_d),
	49, Final(TOP_srlz_i),
	51, Final(TOP_sync_i),
	52, Final(TOP_sum),
	53, Final(TOP_rum),
	54, Final(TOP_ssm),
	55, Final(TOP_rsm),
	END_TRANSITIONS);

  Transitions(sys_mem_1,
	0, sys_mem_10,
	1, Final(TOP_chk_s_m),
	3, Final(TOP_chk_f_s),
	6, Final(TOP_alloc_3),
	END_TRANSITIONS);

  Transitions(sys_mem_10,
	 0, Final(TOP_mov_t_rr),
	 1, Final(TOP_mov_t_dbr),
	 2, Final(TOP_mov_t_ibr),
	 3, Final(TOP_mov_t_pkr),
	 4, Final(TOP_mov_t_pmc),
	 5, Final(TOP_mov_t_pmd),
	 6, Final(TOP_mov_t_msr),
	 9, Final(TOP_ptc_l),
	10, Final(TOP_ptc_g),
	11, Final(TOP_ptc_ga),
	12, Final(TOP_ptr_d),
	13, Final(TOP_ptr_i),
	14, Final(TOP_itr_d),
	15, Final(TOP_itr_i),
	16, Final(TOP_mov_f_rr),
	17, Final(TOP_mov_f_dbr),
	18, Final(TOP_mov_f_ibr),
	19, Final(TOP_mov_f_pkr),
	20, Final(TOP_mov_f_pmc),
	21, Final(TOP_mov_f_pmd),
	22, Final(TOP_mov_f_msr),
	23, Final(TOP_mov_f_cpuid),
	24, Final(TOP_probe_i_r),
	25, Final(TOP_probe_i_w),
	26, Final(TOP_thash),
	27, Final(TOP_ttag),
	30, Final(TOP_tpa),
	31, Final(TOP_tak),
	33, Final(TOP_mov_f_psrum),
	34, Final(TOP_mov_f_ar_m),
	36, Final(TOP_mov_f_cr),
	37, Final(TOP_mov_f_psr),
	41, Final(TOP_mov_t_psrum),
	42, Final(TOP_mov_t_ar_r_m),
	44, Final(TOP_mov_t_cr),
	45, Final(TOP_mov_t_psr),
	46, Final(TOP_itc_d),
	47, Final(TOP_itc_i),
	48, Final(TOP_fc),
	49, Final(TOP_probe_rw_fault),
	50, Final(TOP_probe_r_fault),
	51, Final(TOP_probe_w_fault),
	52, Final(TOP_ptc_e),
	56, Final(TOP_probe_r),
	57, Final(TOP_probe_w),
	END_TRANSITIONS);

  Transitions(int_load_store,
	0, int_load_store_0,
	1, int_load_store_1,
	END_TRANSITIONS);

  Transitions(int_load_store_0,
	0, int_load_store_nobase,
	1, sem_get_fr,
	END_TRANSITIONS);

  Transitions(int_load_store_nobase,
	 0, Final(TOP_ld1),
	 1, Final(TOP_ld2),
	 2, Final(TOP_ld4),
	 3, Final(TOP_ld8),
	 4, Final(TOP_ld1),
	 5, Final(TOP_ld2),
	 6, Final(TOP_ld4),
	 7, Final(TOP_ld8),
	 8, Final(TOP_ld1),
	 9, Final(TOP_ld2),
	10, Final(TOP_ld4),
	11, Final(TOP_ld8),
	12, Final(TOP_ld1),
	13, Final(TOP_ld2),
	14, Final(TOP_ld4),
	15, Final(TOP_ld8),
	16, Final(TOP_ld1),
	17, Final(TOP_ld2),
	18, Final(TOP_ld4),
	19, Final(TOP_ld8),
	20, Final(TOP_ld1),
	21, Final(TOP_ld2),
	22, Final(TOP_ld4),
	23, Final(TOP_ld8),
	27, Final(TOP_ld8_fill),
	32, Final(TOP_ld1),
	33, Final(TOP_ld2),
	34, Final(TOP_ld4),
	35, Final(TOP_ld8),
	36, Final(TOP_ld1),
	37, Final(TOP_ld2),
	38, Final(TOP_ld4),
	39, Final(TOP_ld8),
	40, Final(TOP_ld1),
	41, Final(TOP_ld2),
	42, Final(TOP_ld4),
	43, Final(TOP_ld8),
	48, Final(TOP_st1),
	49, Final(TOP_st2),
	50, Final(TOP_st4),
	51, Final(TOP_st8),
	52, Final(TOP_st1),
	53, Final(TOP_st2),
	54, Final(TOP_st4),
	55, Final(TOP_st8),
	59, Final(TOP_st8_spill),
	END_TRANSITIONS);

  Transitions(sem_get_fr,
	 0, Final(TOP_cmpxchg1),
	 1, Final(TOP_cmpxchg2),
	 2, Final(TOP_cmpxchg4),
	 3, Final(TOP_cmpxchg8),
	 4, Final(TOP_cmpxchg1),
	 5, Final(TOP_cmpxchg2),
	 6, Final(TOP_cmpxchg4),
	 7, Final(TOP_cmpxchg8),
	 8, Final(TOP_xchg1),
	 9, Final(TOP_xchg2),
	10, Final(TOP_xchg4),
	11, Final(TOP_xchg8),
	18, Final(TOP_fetchadd4),
	19, Final(TOP_fetchadd8),
	22, Final(TOP_fetchadd4),
	23, Final(TOP_fetchadd8),
	28, Final(TOP_getf_sig),
	29, Final(TOP_getf_exp),
	30, Final(TOP_getf_s),
	31, Final(TOP_getf_d),
	END_TRANSITIONS);

  Transitions(int_load_store_1,
	0, int_load_store_reg,
	END_TRANSITIONS);

  Transitions(int_load_store_reg,
	 0, Final(TOP_ld1_r),
	 1, Final(TOP_ld2_r),
	 2, Final(TOP_ld4_r),
	 3, Final(TOP_ld8_r),
	 4, Final(TOP_ld1_r),
	 5, Final(TOP_ld2_r),
	 6, Final(TOP_ld4_r),
	 7, Final(TOP_ld8_r),
	 8, Final(TOP_ld1_r),
	 9, Final(TOP_ld2_r),
	10, Final(TOP_ld4_r),
	11, Final(TOP_ld8_r),
	12, Final(TOP_ld1_r),
	13, Final(TOP_ld2_r),
	14, Final(TOP_ld4_r),
	15, Final(TOP_ld8_r),
	16, Final(TOP_ld1_r),
	17, Final(TOP_ld2_r),
	18, Final(TOP_ld4_r),
	19, Final(TOP_ld8_r),
	20, Final(TOP_ld1_r),
	21, Final(TOP_ld2_r),
	22, Final(TOP_ld4_r),
	23, Final(TOP_ld8_r),
	27, Final(TOP_ld8_r_fill),
	32, Final(TOP_ld1_r),
	33, Final(TOP_ld2_r),
	34, Final(TOP_ld4_r),
	35, Final(TOP_ld8_r),
	36, Final(TOP_ld1_r),
	37, Final(TOP_ld2_r),
	38, Final(TOP_ld4_r),
	39, Final(TOP_ld8_r),
	40, Final(TOP_ld1_r),
	41, Final(TOP_ld2_r),
	42, Final(TOP_ld4_r),
	43, Final(TOP_ld8_r),
	END_TRANSITIONS);

  Transitions(int_load_store_imm,
	 0, Final(TOP_ld1_i),
	 1, Final(TOP_ld2_i),
	 2, Final(TOP_ld4_i),
	 3, Final(TOP_ld8_i),
	 4, Final(TOP_ld1_i),
	 5, Final(TOP_ld2_i),
	 6, Final(TOP_ld4_i),
	 7, Final(TOP_ld8_i),
	 8, Final(TOP_ld1_i),
	 9, Final(TOP_ld2_i),
	10, Final(TOP_ld4_i),
	11, Final(TOP_ld8_i),
	12, Final(TOP_ld1_i),
	13, Final(TOP_ld2_i),
	14, Final(TOP_ld4_i),
	15, Final(TOP_ld8_i),
	16, Final(TOP_ld1_i),
	17, Final(TOP_ld2_i),
	18, Final(TOP_ld4_i),
	19, Final(TOP_ld8_i),
	20, Final(TOP_ld1_i),
	21, Final(TOP_ld2_i),
	22, Final(TOP_ld4_i),
	23, Final(TOP_ld8_i),
	27, Final(TOP_ld8_i_fill),
	32, Final(TOP_ld1_i),
	33, Final(TOP_ld2_i),
	34, Final(TOP_ld4_i),
	35, Final(TOP_ld8_i),
	36, Final(TOP_ld1_i),
	37, Final(TOP_ld2_i),
	38, Final(TOP_ld4_i),
	39, Final(TOP_ld8_i),
	40, Final(TOP_ld1_i),
	41, Final(TOP_ld2_i),
	42, Final(TOP_ld4_i),
	43, Final(TOP_ld8_i),
	48, Final(TOP_st1_i),
	49, Final(TOP_st2_i),
	50, Final(TOP_st4_i),
	51, Final(TOP_st8_i),
	52, Final(TOP_st1_i),
	53, Final(TOP_st2_i),
	54, Final(TOP_st4_i),
	55, Final(TOP_st8_i),
	59, Final(TOP_st8_i_spill),
	END_TRANSITIONS);

  Transitions(fp_load_store,
	0, fp_load_store_0,
	1, fp_load_store_1,
	END_TRANSITIONS);

  Transitions(fp_load_store_0,
	0, fp_load_store_nobase,
	1, fp_load_pair_nobase,
	END_TRANSITIONS);

  Transitions(fp_load_store_nobase,
	 0, Final(TOP_ldfe),
	 1, Final(TOP_ldf8),
	 2, Final(TOP_ldfs),
	 3, Final(TOP_ldfd),
	 4, Final(TOP_ldfe),
	 5, Final(TOP_ldf8),
	 6, Final(TOP_ldfs),
	 7, Final(TOP_ldfd),
	 8, Final(TOP_ldfe),
	 9, Final(TOP_ldf8),
	10, Final(TOP_ldfs),
	11, Final(TOP_ldfd),
	12, Final(TOP_ldfe),
	13, Final(TOP_ldf8),
	14, Final(TOP_ldfs),
	15, Final(TOP_ldfd),
	27, Final(TOP_ldf_fill),
	32, Final(TOP_ldfe),
	33, Final(TOP_ldf8),
	34, Final(TOP_ldfs),
	35, Final(TOP_ldfd),
	36, Final(TOP_ldfe),
	37, Final(TOP_ldf8),
	38, Final(TOP_ldfs),
	39, Final(TOP_ldfd),
	44, Final(TOP_lfetch),
	45, Final(TOP_lfetch_excl),
	46, Final(TOP_lfetch_fault),
	47, Final(TOP_lfetch_fault_excl),
	48, Final(TOP_stfe),
	49, Final(TOP_stf8),
	50, Final(TOP_stfs),
	51, Final(TOP_stfd),
	59, Final(TOP_stf_spill),
	END_TRANSITIONS);

  Transitions(fp_load_reg,
	 0, Final(TOP_ldfe_r),
	 1, Final(TOP_ldf8_r),
	 2, Final(TOP_ldfs_r),
	 3, Final(TOP_ldfd_r),
	 4, Final(TOP_ldfe_r),
	 5, Final(TOP_ldf8_r),
	 6, Final(TOP_ldfs_r),
	 7, Final(TOP_ldfd_r),
	 8, Final(TOP_ldfe_r),
	 9, Final(TOP_ldf8_r),
	10, Final(TOP_ldfs_r),
	11, Final(TOP_ldfd_r),
	12, Final(TOP_ldfe_r),
	13, Final(TOP_ldf8_r),
	14, Final(TOP_ldfs_r),
	15, Final(TOP_ldfd_r),
	27, Final(TOP_ldf_r_fill),
	32, Final(TOP_ldfe_r),
	33, Final(TOP_ldf8_r),
	34, Final(TOP_ldfs_r),
	35, Final(TOP_ldfd_r),
	36, Final(TOP_ldfe_r),
	37, Final(TOP_ldf8_r),
	38, Final(TOP_ldfs_r),
	39, Final(TOP_ldfd_r),
	44, Final(TOP_lfetch_r),
	45, Final(TOP_lfetch_r_excl),
	46, Final(TOP_lfetch_r_fault),
	47, Final(TOP_lfetch_r_fault_excl),
	END_TRANSITIONS);

  Transitions(fp_load_store_1,
	0, fp_load_reg,
	1, fp_load_pair_imm,
	END_TRANSITIONS);

  Transitions(fp_load_pair_nobase,
	 1, Final(TOP_ldfp8),
	 2, Final(TOP_ldfps),
	 3, Final(TOP_ldfpd),
	 5, Final(TOP_ldfp8),
	 6, Final(TOP_ldfps),
	 7, Final(TOP_ldfpd),
	 9, Final(TOP_ldfp8),
	10, Final(TOP_ldfps),
	11, Final(TOP_ldfpd),
	13, Final(TOP_ldfp8),
	14, Final(TOP_ldfps),
	15, Final(TOP_ldfpd),
	28, Final(TOP_setf_sig),
	29, Final(TOP_setf_exp),
	30, Final(TOP_setf_s),
	31, Final(TOP_setf_d),
	33, Final(TOP_ldfp8),
	34, Final(TOP_ldfps),
	35, Final(TOP_ldfpd),
	37, Final(TOP_ldfp8),
	38, Final(TOP_ldfps),
	39, Final(TOP_ldfpd),
	END_TRANSITIONS);

  Transitions(fp_load_pair_imm,
	 1, Final(TOP_ldfp8_i),
	 2, Final(TOP_ldfps_i),
	 3, Final(TOP_ldfpd_i),
	 5, Final(TOP_ldfp8_i),
	 6, Final(TOP_ldfps_i),
	 7, Final(TOP_ldfpd_i),
	 9, Final(TOP_ldfp8_i),
	10, Final(TOP_ldfps_i),
	11, Final(TOP_ldfpd_i),
	13, Final(TOP_ldfp8_i),
	14, Final(TOP_ldfps_i),
	15, Final(TOP_ldfpd_i),
	33, Final(TOP_ldfp8_i),
	34, Final(TOP_ldfps_i),
	35, Final(TOP_ldfpd_i),
	37, Final(TOP_ldfp8_i),
	38, Final(TOP_ldfps_i),
	39, Final(TOP_ldfpd_i),
	END_TRANSITIONS);

  Transitions(fp_load_store_imm,
	 0, Final(TOP_ldfe_i),
	 1, Final(TOP_ldf8_i),
	 2, Final(TOP_ldfs_i),
	 3, Final(TOP_ldfd_i),
	 4, Final(TOP_ldfe_i),
	 5, Final(TOP_ldf8_i),
	 6, Final(TOP_ldfs_i),
	 7, Final(TOP_ldfd_i),
	 8, Final(TOP_ldfe_i),
	 9, Final(TOP_ldf8_i),
	10, Final(TOP_ldfs_i),
	11, Final(TOP_ldfd_i),
	12, Final(TOP_ldfe_i),
	13, Final(TOP_ldf8_i),
	14, Final(TOP_ldfs_i),
	15, Final(TOP_ldfd_i),
	27, Final(TOP_ldf_i_fill),
	32, Final(TOP_ldfe_i),
	33, Final(TOP_ldf8_i),
	34, Final(TOP_ldfs_i),
	35, Final(TOP_ldfd_i),
	36, Final(TOP_ldfe_i),
	37, Final(TOP_ldf8_i),
	38, Final(TOP_ldfs_i),
	39, Final(TOP_ldfd_i),
	44, Final(TOP_lfetch_i),
	45, Final(TOP_lfetch_i_excl),
	46, Final(TOP_lfetch_i_fault),
	47, Final(TOP_lfetch_i_fault_excl),
	48, Final(TOP_stfe_i),
	49, Final(TOP_stf8_i),
	50, Final(TOP_stfs_i),
	51, Final(TOP_stfd_i),
	59, Final(TOP_stf_i_spill),
	END_TRANSITIONS);

/* ===== Float instructions ===== */

  STATE fp_misc_0 = Create_Inst_State("fp_misc_0", 0, 33, 1);
  STATE fp_misc_1 = Create_Inst_State("fp_misc_1", 0, 33, 1);
  STATE fp_compare = Create_Inst_State("fp_compare", 0, 33, 1);
  STATE fp_class = Create_Inst_State("fp_class", 0, 12, 1);
  STATE fma_8 = Create_Inst_State("fma_8", 0, 36, 1);
  STATE fma_9 = Create_Inst_State("fma_9", 0, 36, 1);
  STATE fma_a = Create_Inst_State("fma_a", 0, 36, 1);
  STATE fma_b = Create_Inst_State("fma_b", 0, 36, 1);
  STATE fma_c = Create_Inst_State("fma_c", 0, 36, 1);
  STATE fma_d = Create_Inst_State("fma_d", 0, 36, 1);
  STATE fselect = Create_Inst_State("fselect", 0, 34, 3);

  STATE fp_misc_00 = Create_Inst_State("fp_misc_00", 0, 27, 6);
  STATE fp_misc_01 = Create_Inst_State("fp_misc_01", 0, 36, 1);

  STATE fp_misc_10 = Create_Inst_State("fp_misc_10", 0, 27, 6);
  STATE fp_misc_11 = Create_Inst_State("fp_misc_11", 0, 36, 1);

  STATE fp_compare_0 = Create_Inst_State("fp_compare_0", 0, 36, 1);
  STATE fp_compare_1 = Create_Inst_State("fp_compare_1", 0, 36, 1);

  STATE fp_compare_00 = Create_Inst_State("fp_compare_00", 0, 12, 1);
  STATE fp_compare_01 = Create_Inst_State("fp_compare_01", 0, 12, 1);

  STATE fp_compare_10 = Create_Inst_State("fp_compare_10", 0, 12, 1);
  STATE fp_compare_11 = Create_Inst_State("fp_compare_11", 0, 12, 1);

  Transitions(floating,
	 0, fp_misc_0,
	 1, fp_misc_1,
	 4, fp_compare,
	 5, fp_class,
	 8, fma_8,
	 9, fma_9,
	10, fma_a,
	11, fma_b,
	12, fma_c,
	13, fma_d,
	14, fselect,
	END_TRANSITIONS);

  Transitions(fp_misc_0,
	0, fp_misc_00,
	1, fp_misc_01,
	END_TRANSITIONS);

  Transitions(fp_misc_00,
	 0, Final(TOP_break_f),
	 1, Final(TOP_nop_f),
	 4, Final(TOP_fsetc),
	 5, Final(TOP_fclrf),
	 8, Final(TOP_fchkf),
	16, Final(TOP_fmerge_s),
	17, Final(TOP_fmerge_ns),
	18, Final(TOP_fmerge_se),
	20, Final(TOP_fmin),
	21, Final(TOP_fmax),
	22, Final(TOP_famin),
	23, Final(TOP_famax),
	24, Final(TOP_fcvt_fx),
	25, Final(TOP_fcvt_fxu),
	26, Final(TOP_fcvt_fx_trunc),
	27, Final(TOP_fcvt_fxu_trunc),
	28, Final(TOP_fcvt_xf),
	40, Final(TOP_fpack),
	44, Final(TOP_fand),
	45, Final(TOP_fandcm),
	46, Final(TOP_for),
	47, Final(TOP_fxor),
	52, Final(TOP_fswap),
	53, Final(TOP_fswap_nl),
	54, Final(TOP_fswap_nr),
	57, Final(TOP_fmix_lr),
	58, Final(TOP_fmix_r),
	59, Final(TOP_fmix_l),
	60, Final(TOP_fsxt_r),
	61, Final(TOP_fsxt_l),
	END_TRANSITIONS);

  Transitions(fp_misc_01,
	0, Final(TOP_frcpa),
	1, Final(TOP_frsqrta),
	END_TRANSITIONS);

  Transitions(fp_misc_1,
	0, fp_misc_10,
	1, fp_misc_11,
	END_TRANSITIONS);

  Transitions(fp_misc_10,
	16, Final(TOP_fpmerge_s),
	17, Final(TOP_fpmerge_ns),
	18, Final(TOP_fpmerge_se),
	20, Final(TOP_fpmin),
	21, Final(TOP_fpmax),
	22, Final(TOP_fpamin),
	23, Final(TOP_fpamax),
	24, Final(TOP_fpcvt_fx),
	25, Final(TOP_fpcvt_fxu),
	26, Final(TOP_fpcvt_fx_trunc),
	27, Final(TOP_fpcvt_fxu_trunc),
	48, Final(TOP_fpcmp_eq),
	49, Final(TOP_fpcmp_lt),
	50, Final(TOP_fpcmp_le),
	51, Final(TOP_fpcmp_unord),
	52, Final(TOP_fpcmp_neq),
	53, Final(TOP_fpcmp_nlt),
	54, Final(TOP_fpcmp_nle),
	55, Final(TOP_fpcmp_ord),
	END_TRANSITIONS);

  Transitions(fp_misc_11,
	0, Final(TOP_fprcpa),
	1, Final(TOP_fprsqrta),
	END_TRANSITIONS);

  Transitions(fp_compare,
	0, fp_compare_0,
	1, fp_compare_1,
	END_TRANSITIONS);

  Transitions(fp_compare_0,
	0, fp_compare_00,
	1, fp_compare_01,
	END_TRANSITIONS);

  Transitions(fp_compare_00,
	0, Final(TOP_fcmp_eq),
	1, Final(TOP_fcmp_eq_unc),
	END_TRANSITIONS);

  Transitions(fp_compare_01,
	0, Final(TOP_fcmp_lt),
	1, Final(TOP_fcmp_lt_unc),
	END_TRANSITIONS);

  Transitions(fp_compare_1,
	0, fp_compare_10,
	1, fp_compare_11,
	END_TRANSITIONS);

  Transitions(fp_compare_10,
	0, Final(TOP_fcmp_le),
	1, Final(TOP_fcmp_le_unc),
	END_TRANSITIONS);

  Transitions(fp_compare_11,
	0, Final(TOP_fcmp_unord),
	1, Final(TOP_fcmp_unord_unc),
	END_TRANSITIONS);

  Transitions(fp_class,
	0, Final(TOP_fclass_m),
	1, Final(TOP_fclass_m_unc),
	END_TRANSITIONS);

  Transitions(fma_8,
	0, Final(TOP_fma),
	1, Final(TOP_fma_s),
	END_TRANSITIONS);

  Transitions(fma_9,
	0, Final(TOP_fma_d),
	1, Final(TOP_fpma),
	END_TRANSITIONS);

  Transitions(fma_a,
	0, Final(TOP_fms),
	1, Final(TOP_fms_s),
	END_TRANSITIONS);

  Transitions(fma_b,
	0, Final(TOP_fms_d),
	1, Final(TOP_fpms),
	END_TRANSITIONS);

  Transitions(fma_c,
	0, Final(TOP_fnma),
	1, Final(TOP_fnma_s),
	END_TRANSITIONS);

  Transitions(fma_d,
	0, Final(TOP_fnma_d),
	1, Final(TOP_fpnma),
	END_TRANSITIONS);

  Transitions(fselect,
	0, Final(TOP_fselect),
	1, Final(TOP_fselect),
	2, Final(TOP_fselect),
	3, Final(TOP_fselect),
	4, Final(TOP_xma_l),
	6, Final(TOP_xma_hu),
	7, Final(TOP_xma_h),
	END_TRANSITIONS);

/* ===== Branch instructions ===== */

  STATE misc_branch = Create_Inst_State("misc_branch", 0, 27, 6);
  STATE relative_branch = Create_Inst_State("relative_branch", 0, 6, 3);
  STATE indirect_predict = Create_Inst_State("indirect_predict", 0, 27, 6);

  STATE indirect_branch = Create_Inst_State("indirect_branch", 0, 6, 3);
  STATE indirect_return = Create_Inst_State("indirect_return", 0, 6, 6);

  Transitions(branch,
	0, misc_branch,
	1, Final(TOP_br_r_call),
	2, indirect_predict,
	4, relative_branch,
	5, Final(TOP_br_call),
	7, Final(TOP_brp),
	END_TRANSITIONS);

  Transitions(misc_branch,
	 0, Final(TOP_break_b),
	 2, Final(TOP_cover),
	 4, Final(TOP_clrrrb),
	 5, Final(TOP_clrrrb_pr),
	 8, Final(TOP_rfi),
	12, Final(TOP_bsw_0),
	13, Final(TOP_bsw_1),
	16, Final(TOP_epc),
	32, indirect_branch,
	33, indirect_return,
	END_TRANSITIONS);

  Transitions(indirect_branch,
	0, Final(TOP_br_r_cond),
	1, Final(TOP_br_ia),
	END_TRANSITIONS);

  Transitions(indirect_return,
	4, Final(TOP_br_ret),
	END_TRANSITIONS);

  Transitions(indirect_predict,
	 0, Final(TOP_nop_b),
	16, Final(TOP_brp_r),
	17, Final(TOP_brp_ret),
	END_TRANSITIONS);

  Transitions(relative_branch,
	0, Final(TOP_br_cond),
	2, Final(TOP_br_wexit),
	3, Final(TOP_br_wtop),
	5, Final(TOP_br_cloop),
	6, Final(TOP_br_cexit),
	7, Final(TOP_br_ctop),
	END_TRANSITIONS);

  /* ==== L+X-unit instructions ==== */

  STATE misc_lx = Create_Inst_State("misc_lx", 1, 27, 6);

  Transitions(lx_unit,
	 0, misc_lx,
	 6, Final(TOP_movl),
	12, Final(TOP_brl_cond),
	13, Final(TOP_brl_call),
	END_TRANSITIONS);

  Transitions(misc_lx,
	0, Final(TOP_break_x),
	1, Final(TOP_nop_x),
	END_TRANSITIONS);

  Initial_State(ex_unit);

  ISA_Decode_End();
  return 0;
}
