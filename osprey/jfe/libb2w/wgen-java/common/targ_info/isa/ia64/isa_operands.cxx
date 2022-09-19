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
// Generate an ISA containing and group TOPS with similar operands/results 
// format.
/////////////////////////////////////////////////////////
// The instructions are listed below in the order as shown below. 
//
//   1. Real and pseudo instructions
//   2. Simulated instructions.
//
// Within each category, the instructions are arranged roughly in order
// of increasing numbers of operands.
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/isa/ia64/isa_operands.cxx,v $

#include <stddef.h>
#include "topcode.h"
#include "isa_operands_gen.h"

main()
{
  OPERAND_VALUE_TYPE sf, ar, ar_i, ar_m, ec, lc, cr, br, pr, fp64, int64, addl;
  OPERAND_VALUE_TYPE ph, dh, bwh, aclr, ldhint, sthint, lfhint,
		     indwh, ipwh, ih, ldtype, sttype, fldtype,
		     sem, mwh, mbtype4;
  OPERAND_VALUE_TYPE lit1, lit8, lit9, lit14, lit16, lit17, lit22, 
		     lit44, lit64;
  OPERAND_VALUE_TYPE ulit2, ulit4, ulit5, ulit6, ulit7, ulit8, ulit9, ulit21, 
		     ulit24, ulit62;
  OPERAND_VALUE_TYPE pcrel13, pcrel25, pcrel64;
  OPERAND_VALUE_TYPE pmpyshr2, fetchadd, shfadd, pshfadd, len4, len6,
		     scmp, ucmp1, ucmp2, ucmp3, ucmp4;

  OPERAND_USE_TYPE
	  predicate,	// a qualifying predicate
	  base,		// a base address (for memory insts)
	  offset,	// an offset added to a base (implies immed val)
	  postincr,	// a post increment applied to a base address
	  target,	// the target of a branch
	  storeval,	// value to be stored
	  opnd1,	// first/left operand of an alu operator
	  opnd2,	// second/right operand of an alu operator
	  maddend;	// addend/subtrahend operand of a madd

  ISA_Operands_Begin("ia64");

  /* Create the register operand types:
   */
  ar    = ISA_Reg_Opnd_Type_Create("ar", ISA_REGISTER_CLASS_application, 
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, UNSIGNED, INVALID);
  ar_i  = ISA_Reg_Opnd_Type_Create("ar_i", ISA_REGISTER_CLASS_application, 
				   ISA_REGISTER_SUBCLASS_ar_i,
				   64, UNSIGNED, INVALID);
  ar_m  = ISA_Reg_Opnd_Type_Create("ar_m", ISA_REGISTER_CLASS_application, 
				   ISA_REGISTER_SUBCLASS_ar_m,
				   64, UNSIGNED, INVALID);
  ec    = ISA_Reg_Opnd_Type_Create("ec", ISA_REGISTER_CLASS_application, 
				   ISA_REGISTER_SUBCLASS_ec,
				   64, UNSIGNED, INVALID);
  lc    = ISA_Reg_Opnd_Type_Create("lc", ISA_REGISTER_CLASS_application, 
				   ISA_REGISTER_SUBCLASS_lc,
				   64, UNSIGNED, INVALID);
  cr    = ISA_Reg_Opnd_Type_Create("cr", ISA_REGISTER_CLASS_control,
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, UNSIGNED, INVALID);
  br    = ISA_Reg_Opnd_Type_Create("br", ISA_REGISTER_CLASS_branch,
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, UNSIGNED, INVALID);
  pr    = ISA_Reg_Opnd_Type_Create("pr", ISA_REGISTER_CLASS_predicate,
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   1, UNSIGNED, INVALID);
  fp64  = ISA_Reg_Opnd_Type_Create("fp64", ISA_REGISTER_CLASS_float,
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, SIGNED, INVALID);
  int64 = ISA_Reg_Opnd_Type_Create("int64", ISA_REGISTER_CLASS_integer,
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, SIGNED, INVALID);
  addl  = ISA_Reg_Opnd_Type_Create("int64", ISA_REGISTER_CLASS_integer,
				   ISA_REGISTER_SUBCLASS_addl,
				   64, SIGNED, INVALID);

  /* Create the enum operand types:
   */
  ph = ISA_Enum_Opnd_Type_Create("ph", 8, UNSIGNED, EC_ph);
  dh = ISA_Enum_Opnd_Type_Create("dh", 8, UNSIGNED, EC_dh);
  bwh = ISA_Enum_Opnd_Type_Create("bwh", 8, UNSIGNED, EC_bwh);
  aclr = ISA_Enum_Opnd_Type_Create("aclr", 8, UNSIGNED, EC_aclr);
  ldhint = ISA_Enum_Opnd_Type_Create("ldhint", 8, UNSIGNED, EC_ldhint);
  sthint = ISA_Enum_Opnd_Type_Create("sthint", 8, UNSIGNED, EC_sthint);
  lfhint = ISA_Enum_Opnd_Type_Create("lfhint", 8, UNSIGNED, EC_lfhint);
  indwh = ISA_Enum_Opnd_Type_Create("indwh", 8, UNSIGNED, EC_indwh);
  ih = ISA_Enum_Opnd_Type_Create("ih", 8, UNSIGNED, EC_ih);
  ipwh = ISA_Enum_Opnd_Type_Create("ipwh", 8, UNSIGNED, EC_ipwh);
  ldtype = ISA_Enum_Opnd_Type_Create("ldtype", 8, UNSIGNED, EC_ldtype);
  sttype = ISA_Enum_Opnd_Type_Create("sttype", 8, UNSIGNED, EC_sttype);
  fldtype = ISA_Enum_Opnd_Type_Create("fldtype", 8, UNSIGNED, EC_fldtype);
  sem = ISA_Enum_Opnd_Type_Create("sem", 8, UNSIGNED, EC_sem);
  mwh = ISA_Enum_Opnd_Type_Create("mwh", 8, UNSIGNED, EC_mwh);
  mbtype4 = ISA_Enum_Opnd_Type_Create("mbtype4", 4, UNSIGNED, EC_mbtype4);
  sf = ISA_Enum_Opnd_Type_Create("sf", 2, UNSIGNED, EC_sf);

  /* Create the literal operand types:
   */
  lit1   = ISA_Lit_Opnd_Type_Create("lit1",   1, SIGNED, LC_i1);
  lit8   = ISA_Lit_Opnd_Type_Create("lit8",   8, SIGNED, LC_i8);
  lit9   = ISA_Lit_Opnd_Type_Create("lit9",   9, SIGNED, LC_i9);
  lit14  = ISA_Lit_Opnd_Type_Create("lit14", 14, SIGNED, LC_i14);
  lit16  = ISA_Lit_Opnd_Type_Create("lit16", 16, SIGNED, LC_i16);
  lit17  = ISA_Lit_Opnd_Type_Create("lit17", 17, SIGNED, LC_i17);
  lit22  = ISA_Lit_Opnd_Type_Create("lit22", 22, SIGNED, LC_i22);
  lit44  = ISA_Lit_Opnd_Type_Create("lit44", 44, SIGNED, LC_i44);
  lit64  = ISA_Lit_Opnd_Type_Create("lit64", 64, SIGNED, LC_i64);

  ulit2  = ISA_Lit_Opnd_Type_Create("ulit2",   2, UNSIGNED, LC_k2);
  ulit4  = ISA_Lit_Opnd_Type_Create("ulit4",   4, UNSIGNED, LC_k4);
  ulit5  = ISA_Lit_Opnd_Type_Create("ulit5",   5, UNSIGNED, LC_k5);
  ulit6  = ISA_Lit_Opnd_Type_Create("ulit6",   6, UNSIGNED, LC_k6);
  ulit7  = ISA_Lit_Opnd_Type_Create("ulit7",   7, UNSIGNED, LC_k7);
  ulit8  = ISA_Lit_Opnd_Type_Create("ulit8",   8, UNSIGNED, LC_k8);
  ulit9  = ISA_Lit_Opnd_Type_Create("ulit9",   9, UNSIGNED, LC_k9);
  ulit21 = ISA_Lit_Opnd_Type_Create("ulit21", 21, UNSIGNED, LC_k21);
  ulit24 = ISA_Lit_Opnd_Type_Create("ulit24", 24, UNSIGNED, LC_k24);
  ulit62 = ISA_Lit_Opnd_Type_Create("ulit62", 62, UNSIGNED, LC_k62);

  pcrel13 = ISA_Lit_Opnd_Type_Create("pcrel13", 13, PCREL, LC_i13);
  pcrel25 = ISA_Lit_Opnd_Type_Create("pcrel25", 25, PCREL, LC_i25);
  pcrel64 = ISA_Lit_Opnd_Type_Create("pcrel64", 64, PCREL, LC_i64);

  pmpyshr2 = ISA_Lit_Opnd_Type_Create("pmpyshr2", 5, UNSIGNED, LC_pmpyshr2);
  fetchadd = ISA_Lit_Opnd_Type_Create("fetchadd", 6, SIGNED, LC_fetchadd);
  shfadd = ISA_Lit_Opnd_Type_Create("shfadd", 3, UNSIGNED, LC_shfadd);
  pshfadd = ISA_Lit_Opnd_Type_Create("pshfadd", 2, UNSIGNED, LC_pshfadd);
  len4 = ISA_Lit_Opnd_Type_Create("len4", 5, UNSIGNED, LC_len4);
  len6 = ISA_Lit_Opnd_Type_Create("len6", 7, UNSIGNED, LC_len6);
  scmp = ISA_Lit_Opnd_Type_Create("scmp", 9, SIGNED, LC_scmp);
  ucmp1 = ISA_Lit_Opnd_Type_Create("ucmp1", 64, UNSIGNED, LC_ucmp1);
  ucmp2 = ISA_Lit_Opnd_Type_Create("ucmp2", 64, UNSIGNED, LC_ucmp2);
  ucmp3 = ISA_Lit_Opnd_Type_Create("ucmp3", 64, UNSIGNED, LC_ucmp3);
  ucmp4 = ISA_Lit_Opnd_Type_Create("ucmp4", 64, UNSIGNED, LC_ucmp4);

  /* Create the operand uses:
   */
  predicate  = Create_Operand_Use("predicate");
  offset     = Create_Operand_Use("offset");
  base       = Create_Operand_Use("base");
  postincr   = Create_Operand_Use("postincr");
  target     = Create_Operand_Use("target");
  storeval   = Create_Operand_Use("storeval");
  opnd1      = Create_Operand_Use("opnd1");
  opnd2      = Create_Operand_Use("opnd2");
  maddend    = Create_Operand_Use("maddend");

/* =====  ===== */
  Instruction_Group("O_0",
	TOP_bsw_0,	TOP_bsw_1,	TOP_clrrrb,
	TOP_clrrrb_pr,	TOP_cover,	TOP_epc,
	TOP_flushrs,	TOP_loadrs,	TOP_rfi,
	TOP_UNDEFINED);

/* ===== (<%pr%p6>) ===== */
  Instruction_Group("O_1",
	TOP_fwb,	TOP_invala,	TOP_mf,
	TOP_mf_a,	TOP_srlz_d,	TOP_srlz_i,
	TOP_sync_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);

/* ===== (<%pr%p6>) <f7> ===== */
  Instruction_Group("O_2",
	TOP_invala_f_e,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, fp64);

/* ===== (<%pr%p6>) <r7> ===== */
  Instruction_Group("O_3",
	TOP_fc,	TOP_invala_e,	TOP_itc_d,
	TOP_itc_i,	TOP_mov_t_psr,	TOP_mov_t_psrum,
	TOP_ptc_e,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64);

/* ===== (<%pr%p6>) <r7>= ===== */
  Instruction_Group("O_4",
	TOP_mov_f_ip,	TOP_mov_f_pr,	TOP_mov_f_psr,
	TOP_mov_f_psrum,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);

/* ===== (<%pr%p6>) <sf2> ===== */
  Instruction_Group("O_5",
	TOP_fclrf,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, sf);

/* ===== (<%pr%p6>) <imm44> ===== */
  Instruction_Group("O_6",
	TOP_mov_t_pr_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, lit44);

/* ===== <ph>,<dh>,<%tg%b3> ===== */
  Instruction_Group("O_7",
	TOP_br_r,
	TOP_UNDEFINED);
  Operand(0, ph);
  Operand(1, dh);
  Operand(2, br, target);

/* ===== (<%pr%p6>) <uimm21> ===== */
  Instruction_Group("O_8",
	TOP_break,	TOP_break_b,	TOP_break_f,
	TOP_break_i,	TOP_break_m,	TOP_nop,
	TOP_nop_b,	TOP_nop_f,	TOP_nop_i,
	TOP_nop_m,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, ulit21);

/* ===== (<%pr%p6>) <uimm24> ===== */
  Instruction_Group("O_9",
	TOP_rsm,	TOP_rum,	TOP_ssm,
	TOP_sum,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, ulit24);

/* ===== (<%pr%p6>) <uimm62> ===== */
  Instruction_Group("O_10",
	TOP_break_x,	TOP_nop_x,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, ulit62);

/* ===== (<%pr%p6>) <b3>=<r7> ===== */
  Instruction_Group("O_11",
	TOP_mov_t_br,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, br);
  Operand(1, int64);

/* ===== (<%pr%p6>) <f7>=<f7> ===== */
  Instruction_Group("O_12",
	TOP_fcvt_xf,	TOP_mov_f,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64);

/* ===== (<%pr%p6>) <f7>=<r7> ===== */
  Instruction_Group("O_13",
	TOP_setf_d,	TOP_setf_exp,	TOP_setf_s,
	TOP_setf_sig,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, int64);

/* ===== (<%pr%p6>) <r7>,<r7> ===== */
  Instruction_Group("O_14",
	TOP_ptc_g,	TOP_ptc_ga,	TOP_ptc_l,
	TOP_ptr_d,	TOP_ptr_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64);
  Operand(2, int64);

/* ===== (<%pr%p6>) <r7>=<b3> ===== */
  Instruction_Group("O_15",
	TOP_mov_f_br,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, br);

/* ===== (<%pr%p6>) <r7>=<f7> ===== */
  Instruction_Group("O_16",
	TOP_getf_d,	TOP_getf_exp,	TOP_getf_s,
	TOP_getf_sig,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, fp64);

/* ===== (<%pr%p6>) <r7>=<r7> ===== */
  Instruction_Group("O_17",
	TOP_czx1_l,	TOP_czx1_r,	TOP_czx2_l,
	TOP_czx2_r,	TOP_mov,	TOP_popcnt,
	TOP_sxt1,	TOP_sxt2,	TOP_sxt4,
	TOP_tak,	TOP_thash,	TOP_tpa,
	TOP_ttag,	TOP_zxt1,	TOP_zxt2,
	TOP_zxt4,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);

/* ===== (<%pr%p6>) <ar7>=<r7> ===== */
  Instruction_Group("O_18",
	TOP_mov_t_ar_r,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, ar);
  Operand(1, int64);

/* ===== (<%pr%p6>) <cr7>=<r7> ===== */
  Instruction_Group("O_19",
	TOP_mov_t_cr,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, cr);
  Operand(1, int64);

/* ===== (<%pr%p6>) <r7>=<ar7> ===== */
  Instruction_Group("O_20",
	TOP_mov_f_ar,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, ar);

/* ===== (<%pr%p6>) <r7>=<cr7> ===== */
  Instruction_Group("O_21",
	TOP_mov_f_cr,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, cr);

/* ===== (<%pr%p6>) <r7>=[<r7>] ===== */
  Instruction_Group("O_22",
	TOP_mov_f_cpuid,	TOP_mov_f_dbr,	TOP_mov_f_ibr,
	TOP_mov_f_msr,	TOP_mov_f_pkr,	TOP_mov_f_pmc,
	TOP_mov_f_pmd,	TOP_mov_f_rr,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);

/* ===== (<%pr%p6>) <ar7>=<imm8> ===== */
  Instruction_Group("O_23",
	TOP_mov_t_ar_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, ar);
  Operand(1, lit8);

/* ===== (<%pr%p6>) <ar_i7>=<r7> ===== */
  Instruction_Group("O_24",
	TOP_mov_t_ar_r_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, ar_i);
  Operand(1, int64);

/* ===== (<%pr%p6>) <ar_m7>=<r7> ===== */
  Instruction_Group("O_25",
	TOP_mov_t_ar_r_m,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, ar_m);
  Operand(1, int64);

/* ===== (<%pr%p6>) <r7>,<imm17> ===== */
  Instruction_Group("O_26",
	TOP_mov_t_pr,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64);
  Operand(2, lit17);

/* ===== (<%pr%p6>) <r7>,<uimm2> ===== */
  Instruction_Group("O_27",
	TOP_probe_r_fault,	TOP_probe_rw_fault,	TOP_probe_w_fault,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64);
  Operand(2, ulit2);

/* ===== (<%pr%p6>) <r7>=<ar_i7> ===== */
  Instruction_Group("O_28",
	TOP_mov_f_ar_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, ar_i);

/* ===== (<%pr%p6>) <r7>=<ar_m7> ===== */
  Instruction_Group("O_29",
	TOP_mov_f_ar_m,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, ar_m);

/* ===== (<%pr%p6>) <r7>=<imm22> ===== */
  Instruction_Group("O_30",
	TOP_mov_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit22);

/* ===== (<%pr%p6>) <f7>=<%o1%f7> ===== */
  Instruction_Group("O_31",
	TOP_fabs,	TOP_fneg,	TOP_fnegabs,
	TOP_fpabs,	TOP_fpneg,	TOP_fpnegabs,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64, opnd1);

/* ===== (<%pr%p6>) <r7>=<@imm64> ===== */
  Instruction_Group("O_32",
	TOP_movl,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit64);
  Relocatable(1);

/* ===== <bwh>,<ph>,<dh>,<%tg%b3> ===== */
  Instruction_Group("O_33",
	TOP_br_ia,
	TOP_UNDEFINED);
  Operand(0, bwh);
  Operand(1, ph);
  Operand(2, dh);
  Operand(3, br, target);

/* ===== <ph>,<dh>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_34",
	TOP_br,
	TOP_UNDEFINED);
  Operand(0, ph);
  Operand(1, dh);
  Operand(2, pcrel25, target);
  Relocatable(2);

/* ===== <ph>,<dh>,<%tg%@pcrel64> ===== */
  Instruction_Group("O_35",
	TOP_brl,
	TOP_UNDEFINED);
  Operand(0, ph);
  Operand(1, dh);
  Operand(2, pcrel64, target);
  Relocatable(2);

/* ===== (<%pr%p6>) <ar_i7>=<imm8> ===== */
  Instruction_Group("O_36",
	TOP_mov_t_ar_i_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, ar_i);
  Operand(1, lit8);

/* ===== (<%pr%p6>) <ar_m7>=<imm8> ===== */
  Instruction_Group("O_37",
	TOP_mov_t_ar_i_m,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, ar_m);
  Operand(1, lit8);

/* ===== (<%pr%p6>) <f7>=<f7>,<f7> ===== */
  Instruction_Group("O_38",
	TOP_fmerge_ns,	TOP_fmerge_s,	TOP_fmerge_se,
	TOP_fmix_l,	TOP_fmix_lr,	TOP_fmix_r,
	TOP_fpack,	TOP_fpmerge_ns,	TOP_fpmerge_s,
	TOP_fpmerge_se,	TOP_fswap,	TOP_fswap_nl,
	TOP_fswap_nr,	TOP_fsxt_l,	TOP_fsxt_r,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64);
  Operand(2, fp64);

/* ===== (<%pr%p6>) <p6>,<p6>=<r7> ===== */
  Instruction_Group("O_39",
	TOP_tnat_nz,	TOP_tnat_nz_and,	TOP_tnat_nz_or,
	TOP_tnat_nz_or_andcm,	TOP_tnat_nz_unc,	TOP_tnat_z,
	TOP_tnat_z_and,	TOP_tnat_z_or,	TOP_tnat_z_or_andcm,
	TOP_tnat_z_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, int64);

/* ===== (<%pr%p6>) <r7>=<r7>,<r7> ===== */
  Instruction_Group("O_40",
	TOP_add_1,	TOP_mix1_l,	TOP_mix1_r,
	TOP_mix2_l,	TOP_mix2_r,	TOP_mix4_l,
	TOP_mix4_r,	TOP_pack2_sss,	TOP_pack2_uss,
	TOP_pack4_sss,	TOP_pavg1,	TOP_pavg1_raz,
	TOP_pavg2,	TOP_pavg2_raz,	TOP_pavgsub1,
	TOP_pavgsub2,	TOP_probe_r,	TOP_probe_w,
	TOP_psad1,	TOP_sub_1,	TOP_unpack1_h,
	TOP_unpack1_l,	TOP_unpack2_h,	TOP_unpack2_l,
	TOP_unpack4_h,	TOP_unpack4_l,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, int64);

/* ===== (<%pr%p6>) [<r7>]<eq><r7> ===== */
  Instruction_Group("O_41",
	TOP_itr_d,	TOP_itr_i,	TOP_mov_t_dbr,
	TOP_mov_t_ibr,	TOP_mov_t_msr,	TOP_mov_t_pkr,
	TOP_mov_t_pmc,	TOP_mov_t_pmd,	TOP_mov_t_rr,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64);
  Operand(2, int64);

/* ===== (<%pr%p6>) <f7>=<sf2>,<f7> ===== */
  Instruction_Group("O_42",
	TOP_fcvt_fx,	TOP_fcvt_fx_trunc,	TOP_fcvt_fxu,
	TOP_fcvt_fxu_trunc,	TOP_fcvt_xuf,	TOP_fcvt_xuf_d,
	TOP_fcvt_xuf_s,	TOP_fnorm,	TOP_fnorm_d,
	TOP_fnorm_s,	TOP_fpcvt_fx,	TOP_fpcvt_fx_trunc,
	TOP_fpcvt_fxu,	TOP_fpcvt_fxu_trunc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, sf);
  Operand(2, fp64);

/* ===== <indwh>,<ih>,<b3>,<pcrel13> ===== */
  Instruction_Group("O_43",
	TOP_brp_ret,	TOP_brp_r,
	TOP_UNDEFINED);
  Operand(0, indwh);
  Operand(1, ih);
  Operand(2, br);
  Operand(3, pcrel13);

/* ===== (<%pr%p6>) <r7>=<r7>,<uimm2> ===== */
  Instruction_Group("O_44",
	TOP_probe_i_r,	TOP_probe_i_w,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, ulit2);

/* ===== (<%pr%p6>) <r7>=<r7>,<uimm8> ===== */
  Instruction_Group("O_45",
	TOP_mux2,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, ulit8);

/* ===== <r7>=<uimm7>,<uimm7>,<uimm4> ===== */
  Instruction_Group("O_46",
	TOP_alloc_3,
	TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, ulit7);
  Operand(1, ulit7);
  Operand(2, ulit4);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%r7> ===== */
  Instruction_Group("O_47",
	TOP_cmp4_z2_ge_and,	TOP_cmp4_z2_ge_and_orcm,	TOP_cmp4_z2_ge_andcm,
	TOP_cmp4_z2_ge_or,	TOP_cmp4_z2_ge_or_andcm,	TOP_cmp4_z2_ge_orcm,
	TOP_cmp4_z2_gt_and,	TOP_cmp4_z2_gt_and_orcm,	TOP_cmp4_z2_gt_andcm,
	TOP_cmp4_z2_gt_or,	TOP_cmp4_z2_gt_or_andcm,	TOP_cmp4_z2_gt_orcm,
	TOP_cmp4_z2_le_and,	TOP_cmp4_z2_le_and_orcm,	TOP_cmp4_z2_le_andcm,
	TOP_cmp4_z2_le_or,	TOP_cmp4_z2_le_or_andcm,	TOP_cmp4_z2_le_orcm,
	TOP_cmp4_z2_lt_and,	TOP_cmp4_z2_lt_and_orcm,	TOP_cmp4_z2_lt_andcm,
	TOP_cmp4_z2_lt_or,	TOP_cmp4_z2_lt_or_andcm,	TOP_cmp4_z2_lt_orcm,
	TOP_cmp_z2_ge_and,	TOP_cmp_z2_ge_and_orcm,	TOP_cmp_z2_ge_andcm,
	TOP_cmp_z2_ge_or,	TOP_cmp_z2_ge_or_andcm,	TOP_cmp_z2_ge_orcm,
	TOP_cmp_z2_gt_and,	TOP_cmp_z2_gt_and_orcm,	TOP_cmp_z2_gt_andcm,
	TOP_cmp_z2_gt_or,	TOP_cmp_z2_gt_or_andcm,	TOP_cmp_z2_gt_orcm,
	TOP_cmp_z2_le_and,	TOP_cmp_z2_le_and_orcm,	TOP_cmp_z2_le_andcm,
	TOP_cmp_z2_le_or,	TOP_cmp_z2_le_or_andcm,	TOP_cmp_z2_le_orcm,
	TOP_cmp_z2_lt_and,	TOP_cmp_z2_lt_and_orcm,	TOP_cmp_z2_lt_andcm,
	TOP_cmp_z2_lt_or,	TOP_cmp_z2_lt_or_andcm,	TOP_cmp_z2_lt_orcm,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, int64, opnd1);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o2%r7> ===== */
  Instruction_Group("O_48",
	TOP_cmp4_z1_ge_and,	TOP_cmp4_z1_ge_and_orcm,	TOP_cmp4_z1_ge_andcm,
	TOP_cmp4_z1_ge_or,	TOP_cmp4_z1_ge_or_andcm,	TOP_cmp4_z1_ge_orcm,
	TOP_cmp4_z1_gt_and,	TOP_cmp4_z1_gt_and_orcm,	TOP_cmp4_z1_gt_andcm,
	TOP_cmp4_z1_gt_or,	TOP_cmp4_z1_gt_or_andcm,	TOP_cmp4_z1_gt_orcm,
	TOP_cmp4_z1_le_and,	TOP_cmp4_z1_le_and_orcm,	TOP_cmp4_z1_le_andcm,
	TOP_cmp4_z1_le_or,	TOP_cmp4_z1_le_or_andcm,	TOP_cmp4_z1_le_orcm,
	TOP_cmp4_z1_lt_and,	TOP_cmp4_z1_lt_and_orcm,	TOP_cmp4_z1_lt_andcm,
	TOP_cmp4_z1_lt_or,	TOP_cmp4_z1_lt_or_andcm,	TOP_cmp4_z1_lt_orcm,
	TOP_cmp_z1_ge_and,	TOP_cmp_z1_ge_and_orcm,	TOP_cmp_z1_ge_andcm,
	TOP_cmp_z1_ge_or,	TOP_cmp_z1_ge_or_andcm,	TOP_cmp_z1_ge_orcm,
	TOP_cmp_z1_gt_and,	TOP_cmp_z1_gt_and_orcm,	TOP_cmp_z1_gt_andcm,
	TOP_cmp_z1_gt_or,	TOP_cmp_z1_gt_or_andcm,	TOP_cmp_z1_gt_orcm,
	TOP_cmp_z1_le_and,	TOP_cmp_z1_le_and_orcm,	TOP_cmp_z1_le_andcm,
	TOP_cmp_z1_le_or,	TOP_cmp_z1_le_or_andcm,	TOP_cmp_z1_le_orcm,
	TOP_cmp_z1_lt_and,	TOP_cmp_z1_lt_and_orcm,	TOP_cmp_z1_lt_andcm,
	TOP_cmp_z1_lt_or,	TOP_cmp_z1_lt_or_andcm,	TOP_cmp_z1_lt_orcm,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, int64, opnd2);

/* ===== (<%pr%p6>) <f7>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_49",
	TOP_chk_f_s,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, fp64);
  Operand(2, pcrel25, target);
  Relocatable(2);

/* ===== (<%pr%p6>) <f7>=<f7>,<f7>,<f7> ===== */
  Instruction_Group("O_50",
	TOP_fselect,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64);
  Operand(2, fp64);
  Operand(3, fp64);

/* ===== (<%pr%p6>) <lfhint>,[<%bs%r7>] ===== */
  Instruction_Group("O_51",
	TOP_lfetch,	TOP_lfetch_excl,	TOP_lfetch_fault,
	TOP_lfetch_fault_excl,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, lfhint);
  Operand(2, int64, base);

/* ===== (<%pr%p6>) <r7>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_52",
	TOP_chk_s,	TOP_chk_s_i,	TOP_chk_s_m,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64);
  Operand(2, pcrel25, target);
  Relocatable(2);

/* ===== (<%pr%p6>) <r7>=<r7>,<mbtype4> ===== */
  Instruction_Group("O_53",
	TOP_mux1,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, mbtype4);

/* ===== <bwh>,<ph>,<dh>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_54",
	TOP_br_cloop,
	TOP_UNDEFINED);
  Result(0, lc);
  Operand(0, bwh);
  Operand(1, ph);
  Operand(2, dh);
  Operand(3, pcrel25, target);
  Relocatable(3);
  Operand(4, lc);

/* ===== <bwh>,<ph>,<dh>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_55",
	TOP_br_cexit,	TOP_br_ctop,
	TOP_UNDEFINED);
  Result(0, ec);
  Result(1, lc);
  Operand(0, bwh);
  Operand(1, ph);
  Operand(2, dh);
  Operand(3, pcrel25, target);
  Relocatable(3);
  Operand(4, ec);
  Operand(5, lc);

/* ===== (<%pr%p6>) <f7>,<p6>=<sf2>,<f7> ===== */
  Instruction_Group("O_56",
	TOP_fprsqrta,	TOP_frsqrta,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, pr);
  Operand(1, sf);
  Operand(2, fp64);

/* ===== (<%pr%p6>) <sf2>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_57",
	TOP_fchkf,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, sf);
  Operand(2, pcrel25, target);
  Relocatable(2);

/* ===== (<%pr%p6>) <sf2>,<uimm7>,<uimm7> ===== */
  Instruction_Group("O_58",
	TOP_fsetc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, sf);
  Operand(2, ulit7);
  Operand(3, ulit7);

/* ===== <ipwh>,<ih>,<@pcrel25>,<pcrel13> ===== */
  Instruction_Group("O_59",
	TOP_brp,
	TOP_UNDEFINED);
  Operand(0, ipwh);
  Operand(1, ih);
  Operand(2, pcrel25);
  Relocatable(2);
  Operand(3, pcrel13);

/* ===== (<%pr%p6>) <f7>=<%o1%f7>,<%o2%f7> ===== */
  Instruction_Group("O_60",
	TOP_fand,	TOP_fandcm,	TOP_for,
	TOP_fxor,	TOP_xmpy_h,	TOP_xmpy_hu,
	TOP_xmpy_l,	TOP_xmpy_lu,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64, opnd1);
  Operand(2, fp64, opnd2);

/* ===== (<%pr%p6>) <p6>,<p6>=<f7>,<uimm9> ===== */
  Instruction_Group("O_61",
	TOP_fclass_m,	TOP_fclass_m_unc,	TOP_fclass_nm,
	TOP_fclass_nm_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, fp64);
  Operand(2, ulit9);

/* ===== (<%pr%p6>) <p6>,<p6>=<r7>,<uimm6> ===== */
  Instruction_Group("O_62",
	TOP_tbit_nz,	TOP_tbit_nz_and,	TOP_tbit_nz_or,
	TOP_tbit_nz_or_andcm,	TOP_tbit_nz_unc,	TOP_tbit_z,
	TOP_tbit_z_and,	TOP_tbit_z_or,	TOP_tbit_z_or_andcm,
	TOP_tbit_z_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, int64);
  Operand(2, ulit6);

/* ===== (<%pr%p6>) <r7>=<%o1%r7>,<%o2%r7> ===== */
  Instruction_Group("O_63",
	TOP_add,	TOP_addp4,	TOP_and,
	TOP_andcm,	TOP_or,	TOP_padd1,
	TOP_padd1_sss,	TOP_padd1_uus,	TOP_padd1_uuu,
	TOP_padd2,	TOP_padd2_sss,	TOP_padd2_uus,
	TOP_padd2_uuu,	TOP_padd4,	TOP_pcmp1_eq,
	TOP_pcmp1_gt,	TOP_pcmp2_eq,	TOP_pcmp2_gt,
	TOP_pcmp4_eq,	TOP_pcmp4_gt,	TOP_pmax1_u,
	TOP_pmax2,	TOP_pmin1_u,	TOP_pmin2,
	TOP_pmpy2_l,	TOP_pmpy2_r,	TOP_pshl2,
	TOP_pshl4,	TOP_pshr2,	TOP_pshr2_u,
	TOP_pshr4,	TOP_pshr4_u,	TOP_psub1,
	TOP_psub1_sss,	TOP_psub1_uus,	TOP_psub1_uuu,
	TOP_psub2,	TOP_psub2_sss,	TOP_psub2_uus,
	TOP_psub2_uuu,	TOP_psub4,	TOP_shl,
	TOP_shr,	TOP_shr_u,	TOP_sub,
	TOP_xor,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <r7>=<r7>,<r7>,<uimm6> ===== */
  Instruction_Group("O_64",
	TOP_shrp,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, int64);
  Operand(3, ulit6);

/* ===== (<%pr%p6>) <r7>=<r7>,<shfadd>,<r7> ===== */
  Instruction_Group("O_65",
	TOP_shladd,	TOP_shladdp4,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, shfadd);
  Operand(3, int64);

/* ===== (<%pr%p6>) <bwh>,<ph>,<dh>,<%tg%b3> ===== */
  Instruction_Group("O_66",
	TOP_br_ret,	TOP_br_r_cond,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, bwh);
  Operand(2, ph);
  Operand(3, dh);
  Operand(4, br, target);

/* ===== (<%pr%p6>) <f7>=<ldhint>,[<%bs%r7>] ===== */
  Instruction_Group("O_67",
	TOP_ldf_fill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, ldhint);
  Operand(2, int64, base);

/* ===== (<%pr%p6>) <r7>=<%o1%imm8>,<%o2%r7> ===== */
  Instruction_Group("O_68",
	TOP_and_i,	TOP_andcm_i,	TOP_or_i,
	TOP_sub_i,	TOP_xor_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit8, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <r7>=<ldhint>,[<%bs%r7>] ===== */
  Instruction_Group("O_69",
	TOP_ld8_fill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, ldhint);
  Operand(2, int64, base);

/* ===== (<%pr%p6>) <r7>=<r7>,<pshfadd>,<r7> ===== */
  Instruction_Group("O_70",
	TOP_pshladd2,	TOP_pshradd2,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, pshfadd);
  Operand(3, int64);

/* ===== (<%pr%p6>) <r7>=<r7>,<uimm6>,<len6> ===== */
  Instruction_Group("O_71",
	TOP_dep_z,	TOP_extr,	TOP_extr_u,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, ulit6);
  Operand(3, len6);

/* ===== (<%pr%p6>) <f7>,<p6>=<sf2>,<f7>,<f7> ===== */
  Instruction_Group("O_72",
	TOP_fprcpa,	TOP_frcpa,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, pr);
  Operand(1, sf);
  Operand(2, fp64);
  Operand(3, fp64);

/* ===== (<%pr%p6>) <r7>=<%o1%imm14>,<%o2%r7> ===== */
  Instruction_Group("O_73",
	TOP_addp4_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit14, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <r7>=<%o1%r7>,<%o2%uimm5> ===== */
  Instruction_Group("O_74",
	TOP_pshl2_i,	TOP_pshl4_i,	TOP_pshr2_i,
	TOP_pshr2_i_u,	TOP_pshr4_i,	TOP_pshr4_i_u,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, ulit5, opnd2);

/* ===== (<%pr%p6>) <r7>=<%o1%r7>,<%o2%uimm6> ===== */
  Instruction_Group("O_75",
	TOP_shl_i,	TOP_shr_i,	TOP_shr_i_u,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, ulit6, opnd2);

/* ===== (<%pr%p6>) <r7>=<r7>,<r7>,<pmpyshr2> ===== */
  Instruction_Group("O_76",
	TOP_pmpyshr2,	TOP_pmpyshr2_u,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, int64);
  Operand(3, pmpyshr2);

/* ===== <r7>=<uimm7>,<uimm7>,<uimm7>,<uimm7> ===== */
  Instruction_Group("O_77",
	TOP_alloc,
	TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, ulit7);
  Operand(1, ulit7);
  Operand(2, ulit7);
  Operand(3, ulit7);

/* ===== (<%pr%p6>) <aclr>,<f7>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_78",
	TOP_chk_f_a,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, aclr);
  Operand(2, fp64);
  Operand(3, pcrel25, target);
  Relocatable(3);

/* ===== (<%pr%p6>) <aclr>,<r7>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_79",
	TOP_chk_a,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, aclr);
  Operand(2, int64);
  Operand(3, pcrel25, target);
  Relocatable(3);

/* ===== (<%pr%p6>) <r7>=<%o1%@imm14>,<%o2%r7> ===== */
  Instruction_Group("O_80",
	TOP_adds,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit14, opnd1);
  Relocatable(1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <r7>=<%o1%@imm22>,<%o2%r2> ===== */
  Instruction_Group("O_81",
	TOP_addl,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit22, opnd1);
  Relocatable(1);
  Operand(2, addl, opnd2);

/* ===== (<%pr%p6>) <r7>=<imm8>,<uimm6>,<len6> ===== */
  Instruction_Group("O_82",
	TOP_dep_i_z,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit8);
  Operand(2, ulit6);
  Operand(3, len6);

/* ===== (<%pr%p6>) <f7>=<sf2><%o1%f7>,<%o2%f7> ===== */
  Instruction_Group("O_83",
	TOP_famax,	TOP_famin,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, sf);
  Operand(2, fp64, opnd1);
  Operand(3, fp64, opnd2);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%r7>,<%o2%r7> ===== */
  Instruction_Group("O_84",
	TOP_cmp_eq,	TOP_cmp_eq_and,	TOP_cmp_eq_and_orcm,
	TOP_cmp_eq_andcm,	TOP_cmp_eq_or,	TOP_cmp_eq_or_andcm,
	TOP_cmp_eq_orcm,	TOP_cmp_eq_unc,	TOP_cmp_ge,
	TOP_cmp_ge_and,	TOP_cmp_ge_and_orcm,	TOP_cmp_ge_andcm,
	TOP_cmp_ge_or,	TOP_cmp_ge_or_andcm,	TOP_cmp_ge_orcm,
	TOP_cmp_ge_unc,	TOP_cmp_geu,	TOP_cmp_geu_unc,
	TOP_cmp_gt,	TOP_cmp_gt_and,	TOP_cmp_gt_and_orcm,
	TOP_cmp_gt_andcm,	TOP_cmp_gt_or,	TOP_cmp_gt_or_andcm,
	TOP_cmp_gt_orcm,	TOP_cmp_gt_unc,	TOP_cmp_gtu,
	TOP_cmp_gtu_unc,	TOP_cmp_le,	TOP_cmp_le_and,
	TOP_cmp_le_and_orcm,	TOP_cmp_le_andcm,	TOP_cmp_le_or,
	TOP_cmp_le_or_andcm,	TOP_cmp_le_orcm,	TOP_cmp_le_unc,
	TOP_cmp_leu,	TOP_cmp_leu_unc,	TOP_cmp_lt,
	TOP_cmp_lt_and,	TOP_cmp_lt_and_orcm,	TOP_cmp_lt_andcm,
	TOP_cmp_lt_or,	TOP_cmp_lt_or_andcm,	TOP_cmp_lt_orcm,
	TOP_cmp_lt_unc,	TOP_cmp_ltu,	TOP_cmp_ltu_unc,
	TOP_cmp_ne,	TOP_cmp_ne_and,	TOP_cmp_ne_and_orcm,
	TOP_cmp_ne_andcm,	TOP_cmp_ne_or,	TOP_cmp_ne_or_andcm,
	TOP_cmp_ne_orcm,	TOP_cmp_ne_unc,	TOP_cmp4_eq,
	TOP_cmp4_eq_and,	TOP_cmp4_eq_and_orcm,	TOP_cmp4_eq_andcm,
	TOP_cmp4_eq_or,	TOP_cmp4_eq_or_andcm,	TOP_cmp4_eq_orcm,
	TOP_cmp4_eq_unc,	TOP_cmp4_ge,	TOP_cmp4_ge_and,
	TOP_cmp4_ge_and_orcm,	TOP_cmp4_ge_andcm,	TOP_cmp4_ge_or,
	TOP_cmp4_ge_or_andcm,	TOP_cmp4_ge_orcm,	TOP_cmp4_ge_unc,
	TOP_cmp4_geu,	TOP_cmp4_geu_unc,	TOP_cmp4_gt,
	TOP_cmp4_gt_and,	TOP_cmp4_gt_and_orcm,	TOP_cmp4_gt_andcm,
	TOP_cmp4_gt_or,	TOP_cmp4_gt_or_andcm,	TOP_cmp4_gt_orcm,
	TOP_cmp4_gt_unc,	TOP_cmp4_gtu,	TOP_cmp4_gtu_unc,
	TOP_cmp4_le,	TOP_cmp4_le_and,	TOP_cmp4_le_and_orcm,
	TOP_cmp4_le_andcm,	TOP_cmp4_le_or,	TOP_cmp4_le_or_andcm,
	TOP_cmp4_le_orcm,	TOP_cmp4_le_unc,	TOP_cmp4_leu,
	TOP_cmp4_leu_unc,	TOP_cmp4_lt,	TOP_cmp4_lt_and,
	TOP_cmp4_lt_and_orcm,	TOP_cmp4_lt_andcm,	TOP_cmp4_lt_or,
	TOP_cmp4_lt_or_andcm,	TOP_cmp4_lt_orcm,	TOP_cmp4_lt_unc,
	TOP_cmp4_ltu,	TOP_cmp4_ltu_unc,	TOP_cmp4_ne,
	TOP_cmp4_ne_and,	TOP_cmp4_ne_and_orcm,	TOP_cmp4_ne_andcm,
	TOP_cmp4_ne_or,	TOP_cmp4_ne_or_andcm,	TOP_cmp4_ne_orcm,
	TOP_cmp4_ne_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <f7>=<sf2>,<%o1%f7>,<%o2%f7> ===== */
  Instruction_Group("O_85",
	TOP_fadd,	TOP_fadd_d,	TOP_fadd_s,
	TOP_fmax,	TOP_fmin,	TOP_fmpy,
	TOP_fmpy_d,	TOP_fmpy_s,	TOP_fnmpy,
	TOP_fnmpy_d,	TOP_fnmpy_s,	TOP_fpamax,
	TOP_fpamin,	TOP_fpcmp_eq,	TOP_fpcmp_ge,
	TOP_fpcmp_gt,	TOP_fpcmp_le,	TOP_fpcmp_lt,
	TOP_fpcmp_neq,	TOP_fpcmp_nge,	TOP_fpcmp_ngt,
	TOP_fpcmp_nle,	TOP_fpcmp_nlt,	TOP_fpcmp_ord,
	TOP_fpcmp_unord,	TOP_fpmax,	TOP_fpmin,
	TOP_fpmpy,	TOP_fpnmpy,	TOP_fsub,
	TOP_fsub_d,	TOP_fsub_s,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, sf);
  Operand(2, fp64, opnd1);
  Operand(3, fp64, opnd2);

/* ===== (<%pr%p6>) <lfhint>,[<%bs%r7>],<%pi%r7> ===== */
  Instruction_Group("O_86",
	TOP_lfetch_r,	TOP_lfetch_r_excl,	TOP_lfetch_r_fault,
	TOP_lfetch_r_fault_excl,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lfhint);
  Operand(2, int64, base);
  Operand(3, int64, postincr);

/* ===== (<%pr%p6>) <b3>=<bwh>,<ph>,<dh>,<%tg%b3> ===== */
  Instruction_Group("O_87",
	TOP_br_r_call,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, br);
  Operand(1, bwh);
  Operand(2, ph);
  Operand(3, dh);
  Operand(4, br, target);
  Operand(5, ec);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%imm8>,<%o2%r7> ===== */
  Instruction_Group("O_88",
	TOP_cmp4_i_eq,	TOP_cmp4_i_eq_and,	TOP_cmp4_i_eq_and_orcm,
	TOP_cmp4_i_eq_andcm,	TOP_cmp4_i_eq_or,	TOP_cmp4_i_eq_or_andcm,
	TOP_cmp4_i_eq_orcm,	TOP_cmp4_i_eq_unc,	TOP_cmp4_i_ge,
	TOP_cmp4_i_ge_unc,	TOP_cmp4_i_lt,	TOP_cmp4_i_lt_unc,
	TOP_cmp4_i_ne,	TOP_cmp4_i_ne_and,	TOP_cmp4_i_ne_and_orcm,
	TOP_cmp4_i_ne_andcm,	TOP_cmp4_i_ne_or,	TOP_cmp4_i_ne_or_andcm,
	TOP_cmp4_i_ne_orcm,	TOP_cmp4_i_ne_unc,	TOP_cmp_i_eq,
	TOP_cmp_i_eq_and,	TOP_cmp_i_eq_and_orcm,	TOP_cmp_i_eq_andcm,
	TOP_cmp_i_eq_or,	TOP_cmp_i_eq_or_andcm,	TOP_cmp_i_eq_orcm,
	TOP_cmp_i_eq_unc,	TOP_cmp_i_ge,	TOP_cmp_i_ge_unc,
	TOP_cmp_i_lt,	TOP_cmp_i_lt_unc,	TOP_cmp_i_ne,
	TOP_cmp_i_ne_and,	TOP_cmp_i_ne_and_orcm,	TOP_cmp_i_ne_andcm,
	TOP_cmp_i_ne_or,	TOP_cmp_i_ne_or_andcm,	TOP_cmp_i_ne_orcm,
	TOP_cmp_i_ne_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, lit8, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%scmp>,<%o2%r7> ===== */
  Instruction_Group("O_89",
	TOP_cmp4_i_gt,	TOP_cmp4_i_gt_unc,	TOP_cmp4_i_le,
	TOP_cmp4_i_le_unc,	TOP_cmp_i_gt,	TOP_cmp_i_gt_unc,
	TOP_cmp_i_le,	TOP_cmp_i_le_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, scmp, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <r7>=<ldhint>,[<%bs%r7>],<r7> ===== */
  Instruction_Group("O_90",
	TOP_xchg1,	TOP_xchg2,	TOP_xchg4,
	TOP_xchg8,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, ldhint);
  Operand(2, int64, base);
  Operand(3, int64);

/* ===== (<%pr%p6>) <r7>=<r7>,<r7>,<uimm6>,<len4> ===== */
  Instruction_Group("O_91",
	TOP_dep,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64);
  Operand(2, int64);
  Operand(3, ulit6);
  Operand(4, len4);

/* ===== (<%pr%p6>) <b3>=<mwh>,<ih>,<r7>,<pcrel13> ===== */
  Instruction_Group("O_92",
	TOP_mov_t_br_ret,	TOP_mov_t_br_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, br);
  Operand(1, mwh);
  Operand(2, ih);
  Operand(3, int64);
  Operand(4, pcrel13);

/* ===== (<%pr%p6>) <bwh>,<ph>,<dh>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_93",
	TOP_br_cond,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, bwh);
  Operand(2, ph);
  Operand(3, dh);
  Operand(4, pcrel25, target);
  Relocatable(4);

/* ===== (<%pr%p6>) <bwh>,<ph>,<dh>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_94",
	TOP_br_wexit,	TOP_br_wtop,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, ec);
  Operand(1, bwh);
  Operand(2, ph);
  Operand(3, dh);
  Operand(4, pcrel25, target);
  Relocatable(4);
  Operand(5, ec);

/* ===== (<%pr%p6>) <bwh>,<ph>,<dh>,<%tg%@pcrel64> ===== */
  Instruction_Group("O_95",
	TOP_brl_cond,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, bwh);
  Operand(2, ph);
  Operand(3, dh);
  Operand(4, pcrel64, target);
  Relocatable(4);

/* ===== (<%pr%p6>) <lfhint>,[<%bs%r7>],<%pi%imm9> ===== */
  Instruction_Group("O_96",
	TOP_lfetch_i,	TOP_lfetch_i_excl,	TOP_lfetch_i_fault,
	TOP_lfetch_i_fault_excl,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lfhint);
  Operand(2, int64, base);
  Operand(3, lit9, postincr);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%ucmp1>,<%o2%r7> ===== */
  Instruction_Group("O_97",
	TOP_cmp_i_geu,	TOP_cmp_i_geu_unc,	TOP_cmp_i_ltu,
	TOP_cmp_i_ltu_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, ucmp1, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%ucmp2>,<%o2%r7> ===== */
  Instruction_Group("O_98",
	TOP_cmp_i_gtu,	TOP_cmp_i_gtu_unc,	TOP_cmp_i_leu,
	TOP_cmp_i_leu_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, ucmp2, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%ucmp3>,<%o2%r7> ===== */
  Instruction_Group("O_99",
	TOP_cmp4_i_geu,	TOP_cmp4_i_geu_unc,	TOP_cmp4_i_ltu,
	TOP_cmp4_i_ltu_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, ucmp3, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <p6>,<p6>=<%o1%ucmp4>,<%o2%r7> ===== */
  Instruction_Group("O_100",
	TOP_cmp4_i_gtu,	TOP_cmp4_i_gtu_unc,	TOP_cmp4_i_leu,
	TOP_cmp4_i_leu_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, ucmp4, opnd1);
  Operand(2, int64, opnd2);

/* ===== (<%pr%p6>) <f7>=<%o1%f7>,<%o2%f7>,<%ma%f7> ===== */
  Instruction_Group("O_101",
	TOP_xma_h,	TOP_xma_hu,	TOP_xma_l,
	TOP_xma_lu,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64, opnd1);
  Operand(2, fp64, opnd2);
  Operand(3, fp64, maddend);

/* ===== (<%pr%p6>) <r7>=<imm1>,<r7>,<uimm6>,<len6> ===== */
  Instruction_Group("O_102",
	TOP_dep_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit1);
  Operand(2, int64);
  Operand(3, ulit6);
  Operand(4, len6);

/* ===== (<%pr%p6>) <sthint>,[<%bs%r7>]<eq><%sv%f7> ===== */
  Instruction_Group("O_103",
	TOP_stf_spill,	TOP_stf8,	TOP_stfd,
	TOP_stfe,	TOP_stfs,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, sthint);
  Operand(2, int64, base);
  Operand(3, fp64, storeval);

/* ===== (<%pr%p6>) <sthint>,[<%bs%r7>]<eq><%sv%r7> ===== */
  Instruction_Group("O_104",
	TOP_st8_spill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, sthint);
  Operand(2, int64, base);
  Operand(3, int64, storeval);

/* ===== (<%pr%p6>) <f7>=<ldhint>,[<%bs%r7>],<%pi%r7> ===== */
  Instruction_Group("O_105",
	TOP_ldf_r_fill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, int64);
  Operand(1, ldhint);
  Operand(2, int64, base);
  Operand(3, int64, postincr);

/* ===== (<%pr%p6>) <p6>,<p6>=<sf2>,<%o1%f7>,<%o2%f7> ===== */
  Instruction_Group("O_106",
	TOP_fcmp_eq,	TOP_fcmp_eq_unc,	TOP_fcmp_ge,
	TOP_fcmp_ge_unc,	TOP_fcmp_gt,	TOP_fcmp_gt_unc,
	TOP_fcmp_le,	TOP_fcmp_le_unc,	TOP_fcmp_lt,
	TOP_fcmp_lt_unc,	TOP_fcmp_neq,	TOP_fcmp_neq_unc,
	TOP_fcmp_nge,	TOP_fcmp_nge_unc,	TOP_fcmp_ngt,
	TOP_fcmp_ngt_unc,	TOP_fcmp_nle,	TOP_fcmp_nle_unc,
	TOP_fcmp_nlt,	TOP_fcmp_nlt_unc,	TOP_fcmp_ord,
	TOP_fcmp_ord_unc,	TOP_fcmp_unord,	TOP_fcmp_unord_unc,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, pr);
  Result(1, pr);
  Operand(1, sf);
  Operand(2, fp64, opnd1);
  Operand(3, fp64, opnd2);

/* ===== (<%pr%p6>) <r7>=<ldhint>,[<%bs%r7>],<%pi%r7> ===== */
  Instruction_Group("O_107",
	TOP_ld8_r_fill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Result(1, int64);
  Operand(1, ldhint);
  Operand(2, int64, base);
  Operand(3, int64, postincr);

/* ===== (<%pr%p6>) <r7>=<ldtype>,<ldhint>,[<%bs%r7>] ===== */
  Instruction_Group("O_108",
	TOP_ld1,	TOP_ld2,	TOP_ld4,
	TOP_ld8,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, ldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);

/* ===== (<%pr%p6>) <f7>=<fldtype>,<ldhint>,[<%bs%r7>] ===== */
  Instruction_Group("O_109",
	TOP_ldf8,	TOP_ldfd,	TOP_ldfe,
	TOP_ldfs,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);

/* ===== (<%pr%p6>) <b3>=<bwh>,<ph>,<dh>,<%tg%@pcrel25> ===== */
  Instruction_Group("O_110",
	TOP_br_call,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, br);
  Operand(1, bwh);
  Operand(2, ph);
  Operand(3, dh);
  Operand(4, pcrel25, target);
  Relocatable(4);
  Operand(5, ec);

/* ===== (<%pr%p6>) <b3>=<bwh>,<ph>,<dh>,<%tg%@pcrel64> ===== */
  Instruction_Group("O_111",
	TOP_brl_call,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, br);
  Operand(1, bwh);
  Operand(2, ph);
  Operand(3, dh);
  Operand(4, pcrel64, target);
  Relocatable(4);

/* ===== (<%pr%p6>) <f7>=<ldhint>,[<%bs%r7>],<%pi%imm9> ===== */
  Instruction_Group("O_112",
	TOP_ldf_i_fill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, int64);
  Operand(1, ldhint);
  Operand(2, int64, base);
  Operand(3, lit9, postincr);

/* ===== (<%pr%p6>) <r7>=<ldhint>,[<%bs%r7>],<%pi%imm9> ===== */
  Instruction_Group("O_113",
	TOP_ld8_i_fill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Result(1, int64);
  Operand(1, ldhint);
  Operand(2, int64, base);
  Operand(3, lit9, postincr);

/* ===== (<%pr%p6>) <r7>=<sem>,<ldhint>,[<%bs%r7>],<r7> ===== */
  Instruction_Group("O_114",
	TOP_cmpxchg1,	TOP_cmpxchg2,	TOP_cmpxchg4,
	TOP_cmpxchg8,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, sem);
  Operand(2, ldhint);
  Operand(3, int64, base);
  Operand(4, int64);

/* ===== (<%pr%p6>) <f7>=<sf2>,<%o1%f7>,<%o2%f7>,<%ma%f7> ===== */
  Instruction_Group("O_115",
	TOP_fma,	TOP_fma_d,	TOP_fma_s,
	TOP_fms,	TOP_fms_d,	TOP_fms_s,
	TOP_fnma,	TOP_fnma_d,	TOP_fnma_s,
	TOP_fpma,	TOP_fpms,	TOP_fpnma,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, sf);
  Operand(2, fp64, opnd1);
  Operand(3, fp64, opnd2);
  Operand(4, fp64, maddend);

/* ===== (<%pr%p6>) <f7>,<f7>=<fldtype>,<ldhint>,[<%bs%r7>] ===== */
  Instruction_Group("O_116",
	TOP_ldfp8,	TOP_ldfpd,	TOP_ldfps,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, fp64);
  Operand(1, fldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);

/* ===== (<%pr%p6>) <f7>,<f7>=<fldtype>,<ldhint>,[<%bs%r7>] ===== */
  Instruction_Group("O_117",
	TOP_ldfp8_i,	TOP_ldfpd_i,	TOP_ldfps_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, fp64);
  Result(2, int64);
  Operand(1, fldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);

/* ===== (<%pr%p6>) <sttype>,<sthint>,[<%bs%r7>]<eq><%sv%r7> ===== */
  Instruction_Group("O_118",
	TOP_st1,	TOP_st2,	TOP_st4,
	TOP_st8,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, sttype);
  Operand(2, sthint);
  Operand(3, int64, base);
  Operand(4, int64, storeval);

/* ===== (<%pr%p6>) <r7>=<sem>,<ldhint>,[<%bs%r7>],<fetchadd> ===== */
  Instruction_Group("O_119",
	TOP_fetchadd4,	TOP_fetchadd8,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, sem);
  Operand(2, ldhint);
  Operand(3, int64, base);
  Operand(4, fetchadd);

/* ===== (<%pr%p6>) <r7>=<ldtype>,<ldhint>,[<%bs%r7>],<%pi%r7> ===== */
  Instruction_Group("O_120",
	TOP_ld1_r,	TOP_ld2_r,	TOP_ld4_r,
	TOP_ld8_r,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Result(1, int64);
  Operand(1, ldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);
  Operand(4, int64, postincr);

/* ===== (<%pr%p6>) <sthint>,[<%bs%r7>]<eq><%sv%f7>,<%pi%imm9> ===== */
  Instruction_Group("O_121",
	TOP_stf8_i,	TOP_stf_i_spill,	TOP_stfd_i,
	TOP_stfe_i,	TOP_stfs_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, sthint);
  Operand(2, int64, base);
  Operand(3, fp64, storeval);
  Operand(4, lit9, postincr);

/* ===== (<%pr%p6>) <sthint>,[<%bs%r7>]<eq><%sv%r7>,<%pi%imm9> ===== */
  Instruction_Group("O_122",
	TOP_st8_i_spill,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, sthint);
  Operand(2, int64, base);
  Operand(3, int64, storeval);
  Operand(4, lit9, postincr);

/* ===== (<%pr%p6>) <f7>=<fldtype>,<ldhint>,[<%bs%r7>],<%pi%r7> ===== */
  Instruction_Group("O_123",
	TOP_ldf8_r,	TOP_ldfd_r,	TOP_ldfe_r,
	TOP_ldfs_r,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, int64);
  Operand(1, fldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);
  Operand(4, int64, postincr);

/* ===== (<%pr%p6>) <r7>=<ldtype>,<ldhint>,[<%bs%r7>],<%pi%imm9> ===== */
  Instruction_Group("O_124",
	TOP_ld1_i,	TOP_ld2_i,	TOP_ld4_i,
	TOP_ld8_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Result(1, int64);
  Operand(1, ldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);
  Operand(4, lit9, postincr);

/* ===== (<%pr%p6>) <f7>=<fldtype>,<ldhint>,[<%bs%r7>],<%pi%imm9> ===== */
  Instruction_Group("O_125",
	TOP_ldf8_i,	TOP_ldfd_i,	TOP_ldfe_i,
	TOP_ldfs_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Result(1, int64);
  Operand(1, fldtype);
  Operand(2, ldhint);
  Operand(3, int64, base);
  Operand(4, lit9, postincr);

/* ===== (<%pr%p6>) <sttype>,<sthint>,[<%bs%r7>]<eq><%sv%r7>,<%pi%imm9> ===== */
  Instruction_Group("O_126",
	TOP_st1_i,	TOP_st2_i,	TOP_st4_i,
	TOP_st8_i,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, sttype);
  Operand(2, sthint);
  Operand(3, int64, base);
  Operand(4, int64, storeval);
  Operand(5, lit9, postincr);

  Instruction_Group("O_spadjust",
	TOP_spadjust,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, lit64, opnd2);
  Result(0, int64);

/* ===== preg-tn tuple ===== */
  Instruction_Group("O_noop_pregtn",
	TOP_begin_pregtn,	TOP_end_pregtn,
	TOP_UNDEFINED);
  Operand(0, int64);
  Operand(1, lit16);

/* ===== intrncall  R0 <- intrinsic_call (ID, ...) ===== */
  Instruction_Group (	"O_intrncall",
			TOP_intrncall,
			TOP_UNDEFINED);
  Operand (0, lit64);	// the intrinsic ID
  Result  (0, int64);

/* ==== No-op (int) fixups ====== */
  Instruction_Group("O_noop_ifixup",
	TOP_ifixup,
	TOP_UNDEFINED);
  Result(0, int64);

/* ==== No-op (fp) fixups ====== */
  Instruction_Group("O_noop_ffixup",
	TOP_ffixup,	TOP_dfixup,
	TOP_UNDEFINED);
  Result(0, fp64);

/* ====== No-op ======== */
  Instruction_Group("O_noop", 
	TOP_fwd_bar,	TOP_bwd_bar,	TOP_noop,
	TOP_UNDEFINED);

/* ====== Copy Branch Register ======== */
  Instruction_Group("O_copy_br",
	TOP_copy_br,
	TOP_UNDEFINED);
  Operand(0, pr);
  Result(0, br);
  Operand(1, br);

/* ===== asm string ===== */
  Instruction_Group("O_asm",
	TOP_asm,
	TOP_UNDEFINED);

/* ===== Label ===== */
  Instruction_Group("O_label",
	TOP_label,
	TOP_UNDEFINED);
  Operand(0, pcrel64);
  Relocatable(0);

  ISA_Operands_End();
  return 0;
}
