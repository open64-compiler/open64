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

//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/isa/ia64/isa_bundle.cxx,v $

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

  ISA_Bundle_Begin("ia64", 128);

  /* ===== Specification for bundle packing  ===== */
  ISA_Bundle_Pack_Create(ISA_Bundle_Pack_Little_Endian);
  Pack_Template(0, 0, 5);
  Pack_Slot(0, 0, 5, 41);
  Pack_Slot(1, 0, 46, 41);
  Pack_Slot(2, 0, 87, 41);

  /* ===== Specification for R_Unit Type ===== */
  R_Unit = ISA_Exec_Unit_Type_Create("R_Unit", NULL);
  Instruction_Exec_Unit_Group(R_Unit,
			      TOP_UNDEFINED);

  /* ===== Specification for I_Unit Type ===== */
  I_Unit = ISA_Exec_Unit_Type_Create("I_Unit", NULL);
  Instruction_Exec_Unit_Group(I_Unit,

			      // Integer:
			      TOP_pmpyshr2,
			      TOP_pmpyshr2_u,
			      TOP_pmpy2_r,
			      TOP_pmpy2_l,
			      TOP_mix1_r,
			      TOP_mix2_r,
			      TOP_mix4_r,
			      TOP_mix1_l,
			      TOP_mix2_l,
			      TOP_mix4_l,
			      TOP_pack2_uss,
			      TOP_pack2_sss,
			      TOP_pack4_sss,
			      TOP_unpack1_h,
			      TOP_unpack2_h,
			      TOP_unpack4_h,
			      TOP_unpack1_l,
			      TOP_unpack2_l,
			      TOP_unpack4_l,
			      TOP_pmin1_u,
			      TOP_pmax1_u,
			      TOP_pmin2,
			      TOP_pmax2,
			      TOP_psad1,
			      TOP_mux1,
			      TOP_mux2,
			      TOP_pshr2,
			      TOP_pshr4,
			      TOP_shr,
			      TOP_pshr2_u,
			      TOP_pshr4_u,
			      TOP_shr_u,
			      TOP_pshr2_i,
			      TOP_pshr4_i,
			      TOP_pshr2_i_u,
			      TOP_pshr4_i_u,
			      TOP_pshl2,
			      TOP_pshl4,
			      TOP_shl,
			      TOP_pshl2_i,
			      TOP_pshl4_i,
			      TOP_popcnt,
			      TOP_shrp,
			      TOP_extr_u,
			      TOP_extr,
			      TOP_dep_z,
			      TOP_dep_i_z,
			      TOP_dep_i,
			      TOP_dep,
			      TOP_tbit_z,
			      TOP_tbit_z_unc,
			      TOP_tbit_z_and,
			      TOP_tbit_nz_and,
			      TOP_tbit_z_or,
			      TOP_tbit_nz_or,
			      TOP_tbit_z_or_andcm,
			      TOP_tbit_nz_or_andcm,
			      TOP_tnat_z,
			      TOP_tnat_z_unc,
			      TOP_tnat_z_and,
			      TOP_tnat_nz_and,
			      TOP_tnat_z_or,
			      TOP_tnat_nz_or,
			      TOP_tnat_z_or_andcm,
			      TOP_tnat_nz_or_andcm,
			      TOP_break_i,
			      TOP_break,
			      TOP_nop_i,
			      TOP_nop,
			      TOP_chk_s_i,
			      TOP_chk_s,
			      TOP_mov_t_br_i,
			      TOP_mov_t_br_ret,
			      TOP_mov_f_br,
			      TOP_mov_t_pr,
			      TOP_mov_t_pr_i,
			      TOP_mov_f_ip,
			      TOP_mov_f_pr,
			      TOP_mov_t_ar_r_i,
			      TOP_mov_t_ar_r, // Simulated OP, I- or M- unit.
			      TOP_mov_t_ar_i_i,
			      TOP_mov_t_ar_i,
			      TOP_mov_f_ar_i,
			      TOP_mov_f_ar,
			      TOP_zxt1,
			      TOP_zxt2,
			      TOP_zxt4,
			      TOP_sxt1,
			      TOP_sxt2,
			      TOP_sxt4,
			      TOP_czx1_l,
			      TOP_czx2_l,
			      TOP_czx1_r,
			      TOP_czx2_r,
			      TOP_mov_t_br,
			      TOP_tbit_nz,
			      TOP_tbit_nz_unc,
			      TOP_tnat_nz,
			      TOP_tnat_nz_unc,
			      TOP_shl_i,
			      TOP_shr_i_u,
			      TOP_shr_i,

			      // ALU:
			      TOP_add,
			      TOP_add_1,
			      TOP_sub,
			      TOP_sub_1,
			      TOP_addp4,
			      TOP_and,
			      TOP_andcm,
			      TOP_or,
			      TOP_xor,
			      TOP_shladd,
			      TOP_shladdp4,
			      TOP_sub_i,
			      TOP_and_i,
			      TOP_andcm_i,
			      TOP_or_i,
			      TOP_xor_i,
			      TOP_adds,
			      TOP_addp4_i,
			      TOP_addl,
			      TOP_cmp_lt,
			      TOP_cmp_ltu,
			      TOP_cmp_eq,
			      TOP_cmp_lt_unc,
			      TOP_cmp_lt_and,
			      TOP_cmp_lt_or,
			      TOP_cmp_lt_or_andcm,
			      TOP_cmp_le_and,
			      TOP_cmp_le_or,
			      TOP_cmp_le_or_andcm,
			      TOP_cmp_ltu_unc,
			      TOP_cmp_eq_unc,
			      TOP_cmp_eq_and,
			      TOP_cmp_eq_or,
			      TOP_cmp_eq_or_andcm,
			      TOP_cmp_gt_and,
			      TOP_cmp_gt_or,
			      TOP_cmp_gt_orcm,
			      TOP_cmp_gt_andcm,
			      TOP_cmp_gt_and_orcm,
			      TOP_cmp_ge_orcm,
			      TOP_cmp_ge_andcm,
			      TOP_cmp_ge_and_orcm,
			      TOP_cmp_gt_or_andcm,
			      TOP_cmp_ge_and,
			      TOP_cmp_ge_or,
			      TOP_cmp_ge_or_andcm,
			      TOP_cmp_ne_and,
			      TOP_cmp_ne_or,
			      TOP_cmp_ne_or_andcm,
			      TOP_cmp4_lt,
			      TOP_cmp4_lt_and,
			      TOP_cmp4_lt_or,
			      TOP_cmp4_lt_or_andcm,
			      TOP_cmp_lt_orcm,
			      TOP_cmp_lt_andcm,
			      TOP_cmp_lt_and_orcm,
			      TOP_cmp_le_orcm,
			      TOP_cmp_le_andcm,
			      TOP_cmp_le_and_orcm,
			      TOP_cmp4_le_and,
			      TOP_cmp4_le_or,
			      TOP_cmp4_le_or_andcm,
			      TOP_cmp4_gt_and,
			      TOP_cmp4_gt_or,
			      TOP_cmp4_gt_or_andcm,
			      TOP_cmp4_ge_and,
			      TOP_cmp4_ge_and,
			      TOP_cmp4_ge_or,
			      TOP_cmp4_ge_or_andcm,
			      TOP_cmp4_lt_orcm,
			      TOP_cmp4_lt_andcm,
			      TOP_cmp4_lt_and_orcm,
			      TOP_cmp4_le_orcm,
			      TOP_cmp4_le_andcm,
			      TOP_cmp4_le_and_orcm,
			      TOP_cmp4_gt_orcm,
			      TOP_cmp4_gt_andcm,
			      TOP_cmp4_gt_and_orcm,
			      TOP_cmp4_gt_and_orcm,
			      TOP_cmp4_ge_orcm,
			      TOP_cmp4_ge_andcm,
			      TOP_cmp4_ge_and_orcm,
			      TOP_cmp4_ltu,
			      TOP_cmp4_eq,
			      TOP_cmp4_lt_unc,
			      TOP_cmp4_ltu_unc,
			      TOP_cmp4_eq_unc,
			      TOP_cmp4_eq_and,
			      TOP_cmp4_eq_or,
			      TOP_cmp4_eq_or_andcm,
			      TOP_cmp4_ne_and,
			      TOP_cmp4_ne_or,
			      TOP_cmp4_ne_or_andcm,
			      TOP_cmp_z1_gt_and,
			      TOP_cmp_z1_gt_or,
			      TOP_cmp_z1_gt_or_andcm,
			      TOP_cmp_z1_le_and,
			      TOP_cmp_z1_le_or,
			      TOP_cmp_z1_le_or_andcm,
			      TOP_cmp_z1_ge_and,
			      TOP_cmp_z1_ge_or,
			      TOP_cmp_z1_ge_or_andcm,
			      TOP_cmp_z1_lt_and,
			      TOP_cmp_z1_lt_or,
			      TOP_cmp_z1_lt_or_andcm,
			      TOP_cmp4_z1_gt_and,
			      TOP_cmp4_z1_gt_or,
			      TOP_cmp4_z1_gt_or_andcm,
			      TOP_cmp4_z1_le_and,
			      TOP_cmp4_z1_le_or,
			      TOP_cmp4_z1_le_or_andcm,
			      TOP_cmp4_z1_ge_and,
			      TOP_cmp4_z1_ge_or,
			      TOP_cmp4_z1_ge_or_andcm,
			      TOP_cmp4_z1_lt_and,
			      TOP_cmp4_z1_lt_or,
			      TOP_cmp4_z1_lt_or_andcm,
			      TOP_cmp_i_lt,
			      TOP_cmp_i_ltu,
			      TOP_cmp_i_eq,
			      TOP_cmp_i_lt_unc,
			      TOP_cmp_i_ltu_unc,
			      TOP_cmp_i_eq_unc,
			      TOP_cmp_i_eq_and,
			      TOP_cmp_i_eq_or,
			      TOP_cmp_i_eq_or_andcm,
			      TOP_cmp_i_ne_and,
			      TOP_cmp_i_ne_or,
			      TOP_cmp_i_ne_or_andcm,
			      TOP_cmp4_i_lt,
			      TOP_cmp4_i_ltu,
			      TOP_cmp4_i_eq,
			      TOP_cmp4_i_lt_unc,
			      TOP_cmp4_i_ltu_unc,
			      TOP_cmp4_i_eq_unc,
			      TOP_cmp4_i_eq_and,
			      TOP_cmp4_i_eq_or,
			      TOP_cmp4_i_eq_or_andcm,
			      TOP_cmp4_i_ne_and,
			      TOP_cmp4_i_ne_or,
			      TOP_cmp4_i_ne_or_andcm,
			      TOP_padd1,
			      TOP_padd2,
			      TOP_padd4,
			      TOP_padd1_sss,
			      TOP_padd2_sss,
			      TOP_padd1_uuu,
			      TOP_padd2_uuu,
			      TOP_padd1_uus,
			      TOP_padd2_uus,
			      TOP_psub1,
			      TOP_psub2,
			      TOP_psub4,
			      TOP_psub1_sss,
			      TOP_psub2_sss,
			      TOP_psub1_uuu,
			      TOP_psub2_uuu,
			      TOP_psub1_uus,
			      TOP_psub2_uus,
			      TOP_pavg1,
			      TOP_pavg2,
			      TOP_pavg1_raz,
			      TOP_pavg2_raz,
			      TOP_pavgsub1,
			      TOP_pavgsub2,
			      TOP_pcmp1_eq,
			      TOP_pcmp2_eq,
			      TOP_pcmp4_eq,
			      TOP_pcmp1_gt,
			      TOP_pcmp2_gt,
			      TOP_pcmp4_gt,
			      TOP_pshladd2,
			      TOP_pshradd2,
			      TOP_mov,
			      TOP_mov_i,
			      TOP_cmp_eq_and_orcm,
			      TOP_cmp_eq_andcm,
			      TOP_cmp_eq_orcm,
			      TOP_cmp_ge,
			      TOP_cmp_ge_unc,
			      TOP_cmp_geu,
			      TOP_cmp_geu_unc,
			      TOP_cmp_ne,
			      TOP_cmp_ne_and_orcm,
			      TOP_cmp_ne_andcm,
			      TOP_cmp_ne_orcm,
			      TOP_cmp_ne_unc,
			      TOP_cmp4_eq_and_orcm,
			      TOP_cmp4_eq_andcm,
			      TOP_cmp4_eq_orcm,
			      TOP_cmp4_ge,
			      TOP_cmp4_ge_unc,
			      TOP_cmp4_geu,
			      TOP_cmp4_geu_unc,
			      TOP_cmp4_ne,
			      TOP_cmp4_ne_and_orcm,
			      TOP_cmp4_ne_orcm,
			      TOP_cmp4_ne_andcm,
			      TOP_cmp4_ne_unc,
			      TOP_cmp_gt,
			      TOP_cmp_gt_unc,
			      TOP_cmp_gtu,
			      TOP_cmp_gtu_unc,
			      TOP_cmp4_gt,
			      TOP_cmp4_gt_unc,
			      TOP_cmp4_gtu,
			      TOP_cmp4_gtu_unc,
			      TOP_cmp_le,
			      TOP_cmp_le_unc,
			      TOP_cmp_leu,
			      TOP_cmp_leu_unc,
			      TOP_cmp4_le,
			      TOP_cmp4_le_unc,
			      TOP_cmp4_leu,
			      TOP_cmp4_leu_unc,
			      TOP_cmp_i_eq_and_orcm,
			      TOP_cmp_i_eq_andcm,
			      TOP_cmp_i_eq_orcm,
			      TOP_cmp_i_ge,
			      TOP_cmp_i_ge_unc,
			      TOP_cmp_i_geu,
			      TOP_cmp_i_geu_unc,
			      TOP_cmp_i_ne,
			      TOP_cmp_i_ne_and_orcm,
			      TOP_cmp_i_ne_andcm,
			      TOP_cmp_i_ne_orcm,
			      TOP_cmp_i_ne_unc,
			      TOP_cmp4_i_eq_and_orcm,
			      TOP_cmp4_i_eq_andcm,
			      TOP_cmp4_i_eq_orcm,
			      TOP_cmp4_i_ge,
			      TOP_cmp4_i_ge_unc,
			      TOP_cmp4_i_geu,
			      TOP_cmp4_i_geu_unc,
			      TOP_cmp4_i_ne,
			      TOP_cmp4_i_ne_and_orcm,
			      TOP_cmp4_i_ne_andcm,
			      TOP_cmp4_i_ne_orcm,
			      TOP_cmp4_i_ne_unc,
			      TOP_cmp_i_le,
			      TOP_cmp_i_le_unc,
			      TOP_cmp_i_leu,
			      TOP_cmp_i_leu_unc,
			      TOP_cmp4_i_le,
			      TOP_cmp4_i_le_unc,
			      TOP_cmp4_i_leu,
			      TOP_cmp4_i_leu_unc,
			      TOP_cmp_i_gt,
			      TOP_cmp_i_gt_unc,
			      TOP_cmp_i_gtu,
			      TOP_cmp_i_gtu_unc,
			      TOP_cmp4_i_gt,
			      TOP_cmp4_i_gt_unc,
			      TOP_cmp4_i_gtu,
			      TOP_cmp4_i_gtu_unc,
			      TOP_cmp_z1_gt_andcm,
			      TOP_cmp_z1_gt_orcm,
			      TOP_cmp_z1_gt_and_orcm,
			      TOP_cmp_z1_le_andcm,
			      TOP_cmp_z1_le_orcm,
			      TOP_cmp_z1_le_and_orcm,
			      TOP_cmp_z1_ge_andcm,
			      TOP_cmp_z1_ge_orcm,
			      TOP_cmp_z1_ge_and_orcm,
			      TOP_cmp_z1_lt_andcm,
			      TOP_cmp_z1_lt_orcm,
			      TOP_cmp_z1_lt_and_orcm,
			      TOP_cmp4_z1_gt_andcm,
			      TOP_cmp4_z1_gt_orcm,
			      TOP_cmp4_z1_gt_and_orcm,
			      TOP_cmp4_z1_le_andcm,
			      TOP_cmp4_z1_le_orcm,
			      TOP_cmp4_z1_le_and_orcm,
			      TOP_cmp4_z1_ge_andcm,
			      TOP_cmp4_z1_ge_orcm,
			      TOP_cmp4_z1_ge_and_orcm,
			      TOP_cmp4_z1_lt_andcm,
			      TOP_cmp4_z1_lt_orcm,
			      TOP_cmp4_z1_lt_and_orcm,
			      TOP_cmp_z2_gt_andcm,
			      TOP_cmp_z2_gt_orcm,
			      TOP_cmp_z2_gt_and_orcm,
			      TOP_cmp_z2_le_andcm,
			      TOP_cmp_z2_le_orcm,
			      TOP_cmp_z2_le_and_orcm,
			      TOP_cmp_z2_ge_andcm,
			      TOP_cmp_z2_ge_orcm,
			      TOP_cmp_z2_ge_and_orcm,
			      TOP_cmp_z2_lt_andcm,
			      TOP_cmp_z2_lt_orcm,
			      TOP_cmp_z2_lt_and_orcm,
			      TOP_cmp4_z2_gt_andcm,
			      TOP_cmp4_z2_gt_orcm,
			      TOP_cmp4_z2_gt_and_orcm,
			      TOP_cmp4_z2_le_andcm,
			      TOP_cmp4_z2_le_orcm,
			      TOP_cmp4_z2_le_and_orcm,
			      TOP_cmp4_z2_ge_andcm,
			      TOP_cmp4_z2_ge_orcm,
			      TOP_cmp4_z2_ge_and_orcm,
			      TOP_cmp4_z2_lt_andcm,
			      TOP_cmp4_z2_lt_orcm,
			      TOP_cmp4_z2_lt_and_orcm,
			      TOP_cmp_z2_gt_and,
			      TOP_cmp_z2_gt_or,
			      TOP_cmp_z2_gt_or_andcm,
			      TOP_cmp_z2_le_and,
			      TOP_cmp_z2_le_or,
			      TOP_cmp_z2_le_or_andcm,
			      TOP_cmp_z2_ge_and,
			      TOP_cmp_z2_ge_or,
			      TOP_cmp_z2_ge_or_andcm,
			      TOP_cmp_z2_lt_and,
			      TOP_cmp_z2_lt_or,
			      TOP_cmp_z2_lt_or_andcm,
			      TOP_cmp4_z2_gt_and,
			      TOP_cmp4_z2_gt_or,
			      TOP_cmp4_z2_gt_or_andcm,
			      TOP_cmp4_z2_le_and,
			      TOP_cmp4_z2_le_or,
			      TOP_cmp4_z2_le_or_andcm,
			      TOP_cmp4_z2_ge_and,
			      TOP_cmp4_z2_ge_or,
			      TOP_cmp4_z2_ge_or_andcm,
			      TOP_cmp4_z2_lt_and,
			      TOP_cmp4_z2_lt_or,
			      TOP_cmp4_z2_lt_or_andcm,
			      TOP_UNDEFINED);

  /* ===== Specification for M_Unit Type ===== */
  M_Unit = ISA_Exec_Unit_Type_Create("M_Unit", NULL);
  Instruction_Exec_Unit_Group(M_Unit,

			      // Memory:
			      TOP_ld1,
			      TOP_ld2,
			      TOP_ld4,
			      TOP_ld8,
			      TOP_ld8_fill,
			      TOP_ld1_r,
			      TOP_ld2_r,
			      TOP_ld4_r,
			      TOP_ld8_r,
			      TOP_ld8_r_fill,
			      TOP_ld1_i,
			      TOP_ld2_i,
			      TOP_ld4_i,
			      TOP_ld8_i,
			      TOP_ld8_i_fill,
			      TOP_st1,
			      TOP_st2,
			      TOP_st4,
			      TOP_st8,
			      TOP_st8_spill,
			      TOP_st1_i,
			      TOP_st2_i,
			      TOP_st4_i,
			      TOP_st8_i,
			      TOP_st8_i_spill,
			      TOP_ldfs,
			      TOP_ldfd,
			      TOP_ldf8,
			      TOP_ldfe,
			      TOP_ldf_fill,
			      TOP_ldfs_r,
			      TOP_ldfd_r,
			      TOP_ldf8_r,
			      TOP_ldfe_r,
			      TOP_ldf_r_fill,
			      TOP_ldfs_i,
			      TOP_ldfd_i,
			      TOP_ldf8_i,
			      TOP_ldfe_i,
			      TOP_ldf_i_fill,
			      TOP_stfs,
			      TOP_stfd,
			      TOP_stf8,
			      TOP_stfe,
			      TOP_stf_spill,
			      TOP_stfs_i,
			      TOP_stfd_i,
			      TOP_stf8_i,
			      TOP_stfe_i,
			      TOP_stf_i_spill,
			      TOP_ldfps,
			      TOP_ldfpd,
			      TOP_ldfp8,
			      TOP_ldfps_i,
			      TOP_ldfpd_i,
			      TOP_ldfp8_i,
			      TOP_lfetch,
			      TOP_lfetch_excl,
			      TOP_lfetch_fault,
			      TOP_lfetch_fault_excl,
			      TOP_lfetch_r,
			      TOP_lfetch_r_excl,
			      TOP_lfetch_r_fault,
			      TOP_lfetch_r_fault_excl,
			      TOP_lfetch_i,
			      TOP_lfetch_i_excl,
			      TOP_lfetch_i_fault,
			      TOP_lfetch_i_fault_excl,
			      TOP_cmpxchg1,
			      TOP_cmpxchg2,
			      TOP_cmpxchg4,
			      TOP_cmpxchg8,
			      TOP_xchg1,
			      TOP_xchg2,
			      TOP_xchg4,
			      TOP_xchg8,
			      TOP_fetchadd4,
			      TOP_fetchadd8,
			      TOP_setf_sig,
			      TOP_setf_exp,
			      TOP_setf_s,
			      TOP_setf_d,
			      TOP_getf_sig,
			      TOP_getf_exp,
			      TOP_getf_s,
			      TOP_getf_d,
			      TOP_chk_s_m,
			      TOP_chk_s,
			      TOP_chk_f_s,
			      TOP_chk_a,
			      TOP_chk_f_a,
			      TOP_invala,
			      TOP_fwb,
			      TOP_mf,
			      TOP_mf_a,
			      TOP_srlz_d,
			      TOP_srlz_i,
			      TOP_sync_i,
			      TOP_flushrs,
			      TOP_loadrs,
			      TOP_invala_e,
			      TOP_invala_f_e,
			      TOP_fc,
			      TOP_ptc_e,
			      TOP_mov_t_ar_r_m,
			      TOP_mov_t_ar_r, // Simulated OP, I- or M- unit
			      TOP_mov_t_ar_i_m,
			      TOP_mov_t_ar_i,
			      TOP_mov_f_ar_m,
			      TOP_mov_f_ar,
			      TOP_mov_t_cr,
			      TOP_mov_f_cr,
			      TOP_alloc,
			      TOP_alloc_3,
			      TOP_mov_t_psr,
			      TOP_mov_t_psrum,
			      TOP_mov_f_psr,
			      TOP_mov_f_psrum,
			      TOP_break_m,
			      TOP_break,
			      TOP_nop_m,
			      TOP_nop,
			      TOP_probe_r,
			      TOP_probe_w,
			      TOP_probe_i_r,
			      TOP_probe_i_w,
			      TOP_probe_r_fault,
			      TOP_probe_w_fault,
			      TOP_probe_rw_fault,
			      TOP_itc_d,
			      TOP_itc_i,
			      TOP_mov_t_rr,
			      TOP_mov_t_dbr,
			      TOP_mov_t_ibr,
			      TOP_mov_t_pkr,
			      TOP_mov_t_pmc,
			      TOP_mov_t_pmd,
			      TOP_mov_t_msr,
			      TOP_itr_d,
			      TOP_itr_i,
			      TOP_mov_f_rr,
			      TOP_mov_f_dbr,
			      TOP_mov_f_ibr,
			      TOP_mov_f_pkr,
			      TOP_mov_f_pmc,
			      TOP_mov_f_pmd,
			      TOP_mov_f_msr,
			      TOP_mov_f_cpuid,
			      TOP_sum,
			      TOP_rum,
			      TOP_ssm,
			      TOP_rsm,
			      TOP_ptc_l,
			      TOP_ptc_g,
			      TOP_ptc_ga,
			      TOP_ptr_d,
			      TOP_ptr_i,
			      TOP_thash,
			      TOP_ttag,
			      TOP_tpa,
			      TOP_tak,

			      // ALU:
			      TOP_add,
			      TOP_add_1,
			      TOP_sub,
			      TOP_sub_1,
			      TOP_addp4,
			      TOP_and,
			      TOP_andcm,
			      TOP_or,
			      TOP_xor,
			      TOP_shladd,
			      TOP_shladdp4,
			      TOP_sub_i,
			      TOP_and_i,
			      TOP_andcm_i,
			      TOP_or_i,
			      TOP_xor_i,
			      TOP_adds,
			      TOP_addp4_i,
			      TOP_addl,
			      TOP_cmp_lt,
			      TOP_cmp_ltu,
			      TOP_cmp_eq,
			      TOP_cmp_lt_unc,
			      TOP_cmp_ltu_unc,
			      TOP_cmp_eq_unc,
			      TOP_cmp_eq_and,
			      TOP_cmp_eq_or,
			      TOP_cmp_eq_or_andcm,
			      TOP_cmp_ne_and,
			      TOP_cmp_ne_or,
			      TOP_cmp_ne_or_andcm,
			      TOP_cmp4_lt,
			      TOP_cmp4_ltu,
			      TOP_cmp4_eq,
			      TOP_cmp4_lt_unc,
			      TOP_cmp4_ltu_unc,
			      TOP_cmp4_eq_unc,
			      TOP_cmp4_eq_and,
			      TOP_cmp4_eq_or,
			      TOP_cmp4_eq_or_andcm,
			      TOP_cmp4_ne_and,
			      TOP_cmp4_ne_or,
			      TOP_cmp4_ne_or_andcm,
			      TOP_cmp_z1_gt_and,
			      TOP_cmp_z1_gt_or,
			      TOP_cmp_z1_gt_or_andcm,
			      TOP_cmp_z1_le_and,
			      TOP_cmp_z1_le_or,
			      TOP_cmp_z1_le_or_andcm,
			      TOP_cmp_z1_ge_and,
			      TOP_cmp_z1_ge_or,
			      TOP_cmp_z1_ge_or_andcm,
			      TOP_cmp_z1_lt_and,
			      TOP_cmp_z1_lt_or,
			      TOP_cmp_z1_lt_or_andcm,
			      TOP_cmp4_z1_gt_and,
			      TOP_cmp4_z1_gt_or,
			      TOP_cmp4_z1_gt_or_andcm,
			      TOP_cmp4_z1_le_and,
			      TOP_cmp4_z1_le_or,
			      TOP_cmp4_z1_le_or_andcm,
			      TOP_cmp4_z1_ge_and,
			      TOP_cmp4_z1_ge_or,
			      TOP_cmp4_z1_ge_or_andcm,
			      TOP_cmp4_z1_lt_and,
			      TOP_cmp4_z1_lt_or,
			      TOP_cmp4_z1_lt_or_andcm,
			      TOP_cmp_i_lt,
			      TOP_cmp_i_ltu,
			      TOP_cmp_i_eq,
			      TOP_cmp_i_lt_unc,
			      TOP_cmp_i_ltu_unc,
			      TOP_cmp_i_eq_unc,
			      TOP_cmp_i_eq_and,
			      TOP_cmp_i_eq_or,
			      TOP_cmp_i_eq_or_andcm,
			      TOP_cmp_i_ne_and,
			      TOP_cmp_i_ne_or,
			      TOP_cmp_i_ne_or_andcm,
			      TOP_cmp4_i_lt,
			      TOP_cmp4_i_ltu,
			      TOP_cmp4_i_eq,
			      TOP_cmp4_i_lt_unc,
			      TOP_cmp4_i_ltu_unc,
			      TOP_cmp4_i_eq_unc,
			      TOP_cmp4_i_eq_and,
			      TOP_cmp4_i_eq_or,
			      TOP_cmp4_i_eq_or_andcm,
			      TOP_cmp4_i_ne_and,
			      TOP_cmp4_i_ne_or,
			      TOP_cmp4_i_ne_or_andcm,
			      TOP_padd1,
			      TOP_padd2,
			      TOP_padd4,
			      TOP_padd1_sss,
			      TOP_padd2_sss,
			      TOP_padd1_uuu,
			      TOP_padd2_uuu,
			      TOP_padd1_uus,
			      TOP_padd2_uus,
			      TOP_psub1,
			      TOP_psub2,
			      TOP_psub4,
			      TOP_psub1_sss,
			      TOP_psub2_sss,
			      TOP_psub1_uuu,
			      TOP_psub2_uuu,
			      TOP_psub1_uus,
			      TOP_psub2_uus,
			      TOP_pavg1,
			      TOP_pavg2,
			      TOP_pavg1_raz,
			      TOP_pavg2_raz,
			      TOP_pavgsub1,
			      TOP_pavgsub2,
			      TOP_pcmp1_eq,
			      TOP_pcmp2_eq,
			      TOP_pcmp4_eq,
			      TOP_pcmp1_gt,
			      TOP_pcmp2_gt,
			      TOP_pcmp4_gt,
			      TOP_pshladd2,
			      TOP_pshradd2,
			      TOP_mov,
			      TOP_mov_i,
			      TOP_cmp_eq_and_orcm,
			      TOP_cmp_eq_andcm,
			      TOP_cmp_eq_orcm,
			      TOP_cmp_ge,
			      TOP_cmp_ge_unc,
			      TOP_cmp_geu,
			      TOP_cmp_geu_unc,
			      TOP_cmp_ne,
			      TOP_cmp_ne_and_orcm,
			      TOP_cmp_ne_andcm,
			      TOP_cmp_ne_orcm,
			      TOP_cmp_ne_unc,
			      TOP_cmp4_eq_and_orcm,
			      TOP_cmp4_eq_andcm,
			      TOP_cmp4_eq_orcm,
			      TOP_cmp4_ge,
			      TOP_cmp4_ge_unc,
			      TOP_cmp4_geu,
			      TOP_cmp4_geu_unc,
			      TOP_cmp4_ne,
			      TOP_cmp4_ne_and_orcm,
			      TOP_cmp4_ne_orcm,
			      TOP_cmp4_ne_andcm,
			      TOP_cmp4_ne_unc,
			      TOP_cmp_gt,
			      TOP_cmp_gt_unc,
			      TOP_cmp_gtu,
			      TOP_cmp_gtu_unc,
			      TOP_cmp4_gt,
			      TOP_cmp4_gt_unc,
			      TOP_cmp4_gtu,
			      TOP_cmp4_gtu_unc,
			      TOP_cmp_le,
			      TOP_cmp_le_unc,
			      TOP_cmp_leu,
			      TOP_cmp_leu_unc,
			      TOP_cmp4_le,
			      TOP_cmp4_le_unc,
			      TOP_cmp4_leu,
			      TOP_cmp4_leu_unc,
			      TOP_cmp_i_eq_and_orcm,
			      TOP_cmp_i_eq_andcm,
			      TOP_cmp_i_eq_orcm,
			      TOP_cmp_i_ge,
			      TOP_cmp_i_ge_unc,
			      TOP_cmp_i_geu,
			      TOP_cmp_i_geu_unc,
			      TOP_cmp_i_ne,
			      TOP_cmp_i_ne_and_orcm,
			      TOP_cmp_i_ne_andcm,
			      TOP_cmp_i_ne_orcm,
			      TOP_cmp_i_ne_unc,
			      TOP_cmp4_i_eq_and_orcm,
			      TOP_cmp4_i_eq_andcm,
			      TOP_cmp4_i_eq_orcm,
			      TOP_cmp4_i_ge,
			      TOP_cmp4_i_ge_unc,
			      TOP_cmp4_i_geu,
			      TOP_cmp4_i_geu_unc,
			      TOP_cmp4_i_ne,
			      TOP_cmp4_i_ne_and_orcm,
			      TOP_cmp4_i_ne_andcm,
			      TOP_cmp4_i_ne_orcm,
			      TOP_cmp4_i_ne_unc,
			      TOP_cmp_i_le,
			      TOP_cmp_i_le_unc,
			      TOP_cmp_i_leu,
			      TOP_cmp_i_leu_unc,
			      TOP_cmp4_i_le,
			      TOP_cmp4_i_le_unc,
			      TOP_cmp4_i_leu,
			      TOP_cmp4_i_leu_unc,
			      TOP_cmp_i_gt,
			      TOP_cmp_i_gt_unc,
			      TOP_cmp_i_gtu,
			      TOP_cmp_i_gtu_unc,
			      TOP_cmp4_i_gt,
			      TOP_cmp4_i_gt_unc,
			      TOP_cmp4_i_gtu,
			      TOP_cmp4_i_gtu_unc,
			      TOP_cmp_z1_gt_andcm,
			      TOP_cmp_z1_gt_orcm,
			      TOP_cmp_z1_gt_and_orcm,
			      TOP_cmp_z1_le_andcm,
			      TOP_cmp_z1_le_orcm,
			      TOP_cmp_z1_le_and_orcm,
			      TOP_cmp_z1_ge_andcm,
			      TOP_cmp_z1_ge_orcm,
			      TOP_cmp_z1_ge_and_orcm,
			      TOP_cmp_z1_lt_andcm,
			      TOP_cmp_z1_lt_orcm,
			      TOP_cmp_z1_lt_and_orcm,
			      TOP_cmp4_z1_gt_andcm,
			      TOP_cmp4_z1_gt_orcm,
			      TOP_cmp4_z1_gt_and_orcm,
			      TOP_cmp4_z1_le_andcm,
			      TOP_cmp4_z1_le_orcm,
			      TOP_cmp4_z1_le_and_orcm,
			      TOP_cmp4_z1_ge_andcm,
			      TOP_cmp4_z1_ge_orcm,
			      TOP_cmp4_z1_ge_and_orcm,
			      TOP_cmp4_z1_lt_andcm,
			      TOP_cmp4_z1_lt_orcm,
			      TOP_cmp4_z1_lt_and_orcm,
			      TOP_cmp_z2_gt_andcm,
			      TOP_cmp_z2_gt_orcm,
			      TOP_cmp_z2_gt_and_orcm,
			      TOP_cmp_z2_le_andcm,
			      TOP_cmp_z2_le_orcm,
			      TOP_cmp_z2_le_and_orcm,
			      TOP_cmp_z2_ge_andcm,
			      TOP_cmp_z2_ge_orcm,
			      TOP_cmp_z2_ge_and_orcm,
			      TOP_cmp_z2_lt_andcm,
			      TOP_cmp_z2_lt_orcm,
			      TOP_cmp_z2_lt_and_orcm,
			      TOP_cmp4_z2_gt_andcm,
			      TOP_cmp4_z2_gt_orcm,
			      TOP_cmp4_z2_gt_and_orcm,
			      TOP_cmp4_z2_le_andcm,
			      TOP_cmp4_z2_le_orcm,
			      TOP_cmp4_z2_le_and_orcm,
			      TOP_cmp4_z2_ge_andcm,
			      TOP_cmp4_z2_ge_orcm,
			      TOP_cmp4_z2_ge_and_orcm,
			      TOP_cmp4_z2_lt_andcm,
			      TOP_cmp4_z2_lt_orcm,
			      TOP_cmp4_z2_lt_and_orcm,
			      TOP_cmp_z2_gt_and,
			      TOP_cmp_z2_gt_or,
			      TOP_cmp_z2_gt_or_andcm,
			      TOP_cmp_z2_le_and,
			      TOP_cmp_z2_le_or,
			      TOP_cmp_z2_le_or_andcm,
			      TOP_cmp_z2_ge_and,
			      TOP_cmp_z2_ge_or,
			      TOP_cmp_z2_ge_or_andcm,
			      TOP_cmp_z2_lt_and,
			      TOP_cmp_z2_lt_or,
			      TOP_cmp_z2_lt_or_andcm,
			      TOP_cmp4_z2_gt_and,
			      TOP_cmp4_z2_gt_or,
			      TOP_cmp4_z2_gt_or_andcm,
			      TOP_cmp4_z2_le_and,
			      TOP_cmp4_z2_le_or,
			      TOP_cmp4_z2_le_or_andcm,
			      TOP_cmp4_z2_ge_and,
			      TOP_cmp4_z2_ge_or,
			      TOP_cmp4_z2_ge_or_andcm,
			      TOP_cmp4_z2_lt_and,
			      TOP_cmp4_z2_lt_or,
			      TOP_cmp4_z2_lt_or_andcm,
			      TOP_UNDEFINED);

  /* ===== Specification for B_Unit Type ===== */
  B_Unit = ISA_Exec_Unit_Type_Create("B_Unit", NULL);
  Instruction_Exec_Unit_Group(B_Unit,
			      TOP_br_cond,
			      TOP_br_call,
			      TOP_br_r_cond,
			      TOP_br_ia,
			      TOP_br_ret,
			      TOP_br_r_call,
			      TOP_cover,
			      TOP_clrrrb,
			      TOP_clrrrb_pr,
			      TOP_rfi,
			      TOP_bsw_0,
			      TOP_bsw_1,
			      TOP_epc,
			      TOP_break_b,
			      TOP_break,
			      TOP_nop_b,
			      TOP_nop,
			      TOP_br,
			      TOP_br_r,
			      TOP_UNDEFINED);

  /* ===== Specification for F_Unit Type ===== */
  F_Unit = ISA_Exec_Unit_Type_Create("F_Unit", NULL);
  Instruction_Exec_Unit_Group(F_Unit,
			      TOP_fma,
			      TOP_fma_s,
			      TOP_fma_d,
			      TOP_fpma,
			      TOP_fms,
			      TOP_fms_s,
			      TOP_fms_d,
			      TOP_fpms,
			      TOP_fnma,
			      TOP_fnma_s,
			      TOP_fnma_d,
			      TOP_fpnma,
			      TOP_xma_l,
			      TOP_xma_h,
			      TOP_xma_hu,
			      TOP_fselect,
			      TOP_fcmp_eq,
			      TOP_fcmp_lt,
			      TOP_fcmp_le,
			      TOP_fcmp_unord,
			      TOP_fcmp_eq_unc,
			      TOP_fcmp_lt_unc,
			      TOP_fcmp_le_unc,
			      TOP_fcmp_unord_unc,
			      TOP_fcmp_gt,
			      TOP_fcmp_gt_unc,
			      TOP_fcmp_ge,
			      TOP_fcmp_ge_unc,
			      TOP_fcmp_neq,
			      TOP_fcmp_neq_unc,
			      TOP_fcmp_nlt,
			      TOP_fcmp_nlt_unc,
			      TOP_fcmp_nle,
			      TOP_fcmp_nle_unc,
			      TOP_fcmp_ngt,
			      TOP_fcmp_ngt_unc,
			      TOP_fcmp_nge,
			      TOP_fcmp_nge_unc,
			      TOP_fcmp_ord,
			      TOP_fcmp_ord_unc,
			      TOP_fclass_m,
			      TOP_fclass_m_unc,
			      TOP_frcpa,
			      TOP_fprcpa,
			      TOP_frsqrta,
			      TOP_fprsqrta,
			      TOP_fmin,
			      TOP_fmax,
			      TOP_famin,
			      TOP_famax,
			      TOP_fpmin,
			      TOP_fpmax,
			      TOP_fpamin,
			      TOP_fpamax,
			      TOP_fpcmp_eq,
			      TOP_fpcmp_lt,
			      TOP_fpcmp_le,
			      TOP_fpcmp_gt,
			      TOP_fpcmp_ge,
			      TOP_fpcmp_unord,
			      TOP_fpcmp_neq,
			      TOP_fpcmp_nlt,
			      TOP_fpcmp_nle,
			      TOP_fpcmp_ngt,
			      TOP_fpcmp_nge,
			      TOP_fpcmp_ord,
			      TOP_fmerge_s,
			      TOP_fmerge_ns,
			      TOP_fmerge_se,
			      TOP_fmix_lr,
			      TOP_fmix_r,
			      TOP_fmix_l,
			      TOP_fsxt_r,
			      TOP_fsxt_l,
			      TOP_fpack,
			      TOP_fswap,
			      TOP_fswap_nl,
			      TOP_fswap_nr,
			      TOP_fand,
			      TOP_fandcm,
			      TOP_for,
			      TOP_fxor,
			      TOP_fpmerge_s,
			      TOP_fpmerge_ns,
			      TOP_fpmerge_se,
			      TOP_fcvt_fx,
			      TOP_fcvt_fxu,
			      TOP_fcvt_fx_trunc,
			      TOP_fcvt_fxu_trunc,
			      TOP_fpcvt_fx,
			      TOP_fpcvt_fxu,
			      TOP_fpcvt_fx_trunc,
			      TOP_fpcvt_fxu_trunc,
			      TOP_fcvt_xf,
			      TOP_fsetc,
			      TOP_fclrf,
			      TOP_fchkf,
			      TOP_break_f,
			      TOP_break,
			      TOP_nop_f,
			      TOP_nop,
			      TOP_fabs,
			      TOP_fnegabs,
			      TOP_fpabs,
			      TOP_fpnegabs,
			      TOP_fneg,
			      TOP_fpneg,
			      TOP_mov_f,
			      TOP_xma_lu,
			      TOP_xmpy_l,
			      TOP_xmpy_lu,
			      TOP_xmpy_h,
			      TOP_xmpy_hu,
			      TOP_fcvt_xuf,
			      TOP_fcvt_xuf_d,
			      TOP_fcvt_xuf_s,
			      TOP_fnorm,
			      TOP_fnorm_d,
			      TOP_fnorm_s,
			      TOP_fadd,
			      TOP_fadd_d,
			      TOP_fadd_s,
			      TOP_fsub,
			      TOP_fsub_d,
			      TOP_fsub_s,
			      TOP_fmpy,
			      TOP_fmpy_d,
			      TOP_fmpy_s,
			      TOP_fnmpy,
			      TOP_fnmpy_d,
			      TOP_fnmpy_s,
			      TOP_fpmpy,
			      TOP_fpnmpy,
			      TOP_fclass_nm,
			      TOP_fclass_nm_unc,
			      TOP_UNDEFINED);

  /* ===== Specification for L_Unit Type ===== */
  L_Unit = ISA_Exec_Unit_Type_Create("L_Unit", NULL);
  Instruction_Exec_Unit_Group(L_Unit,
			      TOP_movl,
			      TOP_break_x,
			      TOP_nop_x,
			      TOP_UNDEFINED);

  /* ===== Specification for I2_Unit Type ===== */
  I2_Unit = ISA_Exec_Unit_Type_Create("I2_Unit", I_Unit);
  Instruction_Exec_Unit_Group(I2_Unit,
			      TOP_movl,
			      TOP_break,
			      TOP_break_i,
			      TOP_break_x,
			      TOP_nop,
			      TOP_nop_i,
			      TOP_nop_x,
			      TOP_UNDEFINED);
 
  /* ===== Specification for B2_Unit Type ===== */
  B2_Unit = ISA_Exec_Unit_Type_Create("B2_Unit", B_Unit);
  Instruction_Exec_Unit_Group(B2_Unit,
			      TOP_br_wexit,
			      TOP_br_wtop,
			      TOP_br_cloop,
			      TOP_br_cexit,
			      TOP_br_ctop,
			      TOP_br_cond,
			      TOP_br_call,
			      TOP_br_r_cond,
			      TOP_br_ia,
			      TOP_br_ret,
			      TOP_br_r_call,
			      TOP_brp,
			      TOP_brp_r,
			      TOP_brp_ret,
			      TOP_cover,
			      TOP_clrrrb,
			      TOP_clrrrb_pr,
			      TOP_rfi,
			      TOP_bsw_0,
			      TOP_bsw_1,
			      TOP_epc,
			      TOP_break_b,
			      TOP_break,
			      TOP_nop_b,
			      TOP_nop,
			      TOP_br,
			      TOP_br_r,
			      TOP_UNDEFINED);
 
  /* === All legal bundle orderings (32 of them) are specified below. */

  /* ===== Template 0x00 ===== */
  ISA_Bundle_Type_Create("mii", ".mii", 3);
  Slot(0, M_Unit);
  Slot(1, I_Unit);
  Slot(2, I_Unit);

  /* ===== Template 0x01 ===== */
  ISA_Bundle_Type_Create("mii_", ".mii", 3);
  Slot(0, M_Unit);
  Slot(1, I_Unit);
  Slot(2, I_Unit);
  Stop(2);

  /* ===== Template 0x02 ===== */
  ISA_Bundle_Type_Create("mi_i", ".mii", 3);
  Slot(0, M_Unit);
  Slot(1, I_Unit);
  Stop(1);
  Slot(2, I_Unit);

  /* ===== Template 0x03 ===== */
  ISA_Bundle_Type_Create("mi_i_", ".mii", 3);
  Slot(0, M_Unit);
  Slot(1, I_Unit);
  Stop(1);
  Slot(2, I_Unit);
  Stop(2);

  /* ===== Template 0x04 ===== */
  ISA_Bundle_Type_Create("mlx", ".mlx", 3);
  Slot(0, M_Unit);
  Slot(1, L_Unit);
  Slot(2, L_Unit);

  /* ===== Template 0x05 ===== */
  ISA_Bundle_Type_Create("mlx_", ".mlx", 3);
  Slot(0, M_Unit);
  Slot(1, L_Unit);
  Slot(2, L_Unit);
  Stop(2);

  /* ===== Template 0x06 ===== */
  ISA_Bundle_Type_Create("reserved_06", ".0x06", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  /* ===== Template 0x07 ===== */
  ISA_Bundle_Type_Create("reserved_07", ".0x07", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  /* ===== Template 0x08 ===== */
  ISA_Bundle_Type_Create("mmi", ".mmi", 3);
  Slot(0, M_Unit);
  Slot(1, M_Unit);
  Slot(2, I_Unit);

  /* ===== Template 0x09 ===== */
  ISA_Bundle_Type_Create("mmi_", ".mmi", 3);
  Slot(0, M_Unit);
  Slot(1, M_Unit);
  Slot(2, I_Unit);
  Stop(2);

  /* ===== Template 0x0A ===== */
  ISA_Bundle_Type_Create("m_mi", ".mmi", 3);
  Slot(0, M_Unit);
  Stop(0);
  Slot(1, M_Unit);
  Slot(2, I_Unit);

  /* ===== Template 0x0B ===== */
  ISA_Bundle_Type_Create("m_mi_", ".mmi", 3);
  Slot(0, M_Unit);
  Stop(0);
  Slot(1, M_Unit);
  Slot(2, I_Unit);
  Stop(2);

  /* ===== Template 0x0C ===== */
  ISA_Bundle_Type_Create("mfi", ".mfi", 3);
  Slot(0, M_Unit);
  Slot(1, F_Unit);
  Slot(2, I_Unit);

  /* ===== Template 0x0D ===== */
  ISA_Bundle_Type_Create("mfi_", ".mfi", 3);
  Slot(0, M_Unit);
  Slot(1, F_Unit);
  Slot(2, I_Unit);
  Stop(2);

  /* ===== Template 0x0E ===== */
  ISA_Bundle_Type_Create("mmf", ".mmf", 3);
  Slot(0, M_Unit);
  Slot(1, M_Unit);
  Slot(2, F_Unit);

  /* ===== Template 0x0F ===== */
  ISA_Bundle_Type_Create("mmf_", ".mmf", 3);
  Slot(0, M_Unit);
  Slot(1, M_Unit);
  Slot(2, F_Unit);
  Stop(2);

  /* ===== Template 0x10 ===== */
  ISA_Bundle_Type_Create("mib", ".mib", 3);
  Slot(0, M_Unit);
  Slot(1, I_Unit);
  Slot(2, B2_Unit);

  /* ===== Template 0x11 ===== */
  ISA_Bundle_Type_Create("mib_", ".mib", 3);
  Slot(0, M_Unit);
  Slot(1, I_Unit);
  Slot(2, B2_Unit);
  Stop(2);

  /* ===== Template 0x12 ===== */
  ISA_Bundle_Type_Create("mbb", ".mbb", 3);
  Slot(0, M_Unit);
  Slot(1, B_Unit);
  Slot(2, B2_Unit);

  /* ===== Template 0x13 ===== */
  ISA_Bundle_Type_Create("mbb_", ".mbb", 3);
  Slot(0, M_Unit);
  Slot(1, B_Unit);
  Slot(2, B2_Unit);
  Stop(2);

  /* ===== Template 0x14 ===== */
  ISA_Bundle_Type_Create("reserved_14", ".0x14", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  /* ===== Template 0x15 ===== */
  ISA_Bundle_Type_Create("reserved_15", ".0x15", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  /* ===== Template 0x16 ===== */
  ISA_Bundle_Type_Create("bbb", ".bbb", 3);
  Slot(0, B_Unit);
  Slot(1, B_Unit);
  Slot(2, B2_Unit);

  /* ===== Template 0x17 ===== */
  ISA_Bundle_Type_Create("bbb_", ".bbb", 3);
  Slot(0, B_Unit);
  Slot(1, B_Unit);
  Slot(2, B2_Unit);
  Stop(2);

  /* ===== Template 0x18 ===== */
  ISA_Bundle_Type_Create("mmb", ".mmb", 3);
  Slot(0, M_Unit);
  Slot(1, M_Unit);
  Slot(2, B2_Unit);

  /* ===== Template 0x19 ===== */
  ISA_Bundle_Type_Create("mmb_", ".mmb", 3);
  Slot(0, M_Unit);
  Slot(1, M_Unit);
  Slot(2, B2_Unit);
  Stop(2);

  /* ===== Template 0x1A ===== */
  ISA_Bundle_Type_Create("reserved_1a", ".0x1a", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  /* ===== Template 0x1B ===== */
  ISA_Bundle_Type_Create("reserved_1b", ".0x1b", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  /* ===== Template 0x1C ===== */
  ISA_Bundle_Type_Create("mfb", ".mfb", 3);
  Slot(0, M_Unit);
  Slot(1, F_Unit);
  Slot(2, B2_Unit);

  /* ===== Template 0x1D ===== */
  ISA_Bundle_Type_Create("mfb_", ".mfb", 3);
  Slot(0, M_Unit);
  Slot(1, F_Unit);
  Slot(2, B2_Unit);
  Stop(2);

  /* ===== Template 0x1E ===== */
  ISA_Bundle_Type_Create("reserved_1e", ".0x1e", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  /* ===== Template 0x1F ===== */
  ISA_Bundle_Type_Create("reserved_1f", ".0x1f", 3);
  Slot(0, R_Unit);
  Slot(1, R_Unit);
  Slot(2, R_Unit);

  ISA_Bundle_End();
  return 0;
}
