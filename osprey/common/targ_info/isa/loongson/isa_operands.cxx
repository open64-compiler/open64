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

#include <stddef.h>
#include "topcode.h"
#include "isa_operands_gen.h"

main()
{
  OPERAND_VALUE_TYPE sf, ar, ar_i, ar_m, ec, lc, cr, br, pr, fp64, int64, addl;
  OPERAND_VALUE_TYPE ph, dh, bwh, aclr, ldhint, sthint, lfhint,
		     indwh, ipwh, ih, ldtype, sttype, fldtype,
		     sem, mwh, mbtype4;
  OPERAND_VALUE_TYPE lit1, lit8, lit9, lit14, lit16, lit17, lit22, lit26, lit32,
		     lit44, lit48, lit64;
  OPERAND_VALUE_TYPE ulit2, ulit4, ulit5, ulit6, ulit7, ulit8, ulit9, ulit16, ulit20, ulit21, 
		     ulit24, ulit32, ulit48, ulit62, ulit64;
  OPERAND_VALUE_TYPE pcrel13, pcrel16, pcrel25, pcrel26, pcrel64;
  OPERAND_VALUE_TYPE pmpyshr2, fetchadd, shfadd, pshfadd, len4, len6,
		     scmp, ucmp1, ucmp2, ucmp3, ucmp4, hi, lo, fsr;

  OPERAND_USE_TYPE
	  predicate,	// a qualifying predicate
	  base,		// a base address (for memory insts)
	  offset,	// an offset added to a base (implies immed val)
	  postincr,	// a post increment applied to a base address
	  target,	// the target of a branch
	  storeval,	// value to be stored
	  opnd1,	// first/left operand of an alu operator
	  opnd2,	// second/right operand of an alu operator
 	  opnd3,	// 3rd operand of conditional move
	  maddend;	// addend/subtrahend operand of a madd

  ISA_Operands_Begin("loongson");

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
  addl  = ISA_Reg_Opnd_Type_Create("int64_2", ISA_REGISTER_CLASS_integer,
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
  lit26  = ISA_Lit_Opnd_Type_Create("lit26", 26, SIGNED, LC_i26);
  lit32  = ISA_Lit_Opnd_Type_Create("lit32", 32, SIGNED, LC_i32);
  lit44  = ISA_Lit_Opnd_Type_Create("lit44", 44, SIGNED, LC_i44);
  lit48  = ISA_Lit_Opnd_Type_Create("lit48", 48, SIGNED, LC_i48);
  lit64  = ISA_Lit_Opnd_Type_Create("lit64", 64, SIGNED, LC_i64);

  ulit2  = ISA_Lit_Opnd_Type_Create("ulit2",   2, UNSIGNED, LC_k2);
  ulit4  = ISA_Lit_Opnd_Type_Create("ulit4",   4, UNSIGNED, LC_k4);
  ulit5  = ISA_Lit_Opnd_Type_Create("ulit5",   5, UNSIGNED, LC_k5);
  ulit6  = ISA_Lit_Opnd_Type_Create("ulit6",   6, UNSIGNED, LC_k6);
  ulit7  = ISA_Lit_Opnd_Type_Create("ulit7",   7, UNSIGNED, LC_k7);
  ulit8  = ISA_Lit_Opnd_Type_Create("ulit8",   8, UNSIGNED, LC_k8);
  ulit9  = ISA_Lit_Opnd_Type_Create("ulit9",   9, UNSIGNED, LC_k9);
  ulit16 = ISA_Lit_Opnd_Type_Create("ulit16", 16, UNSIGNED, LC_k16);
  ulit20 = ISA_Lit_Opnd_Type_Create("ulit20", 20, UNSIGNED, LC_k20);
  ulit21 = ISA_Lit_Opnd_Type_Create("ulit21", 21, UNSIGNED, LC_k21);
  ulit24 = ISA_Lit_Opnd_Type_Create("ulit24", 24, UNSIGNED, LC_k24);
  ulit32 = ISA_Lit_Opnd_Type_Create("ulit32", 32, UNSIGNED, LC_k32);
  ulit48 = ISA_Lit_Opnd_Type_Create("ulit48", 48, UNSIGNED, LC_k48);
  ulit62 = ISA_Lit_Opnd_Type_Create("ulit62", 62, UNSIGNED, LC_k62);
  ulit64 = ISA_Lit_Opnd_Type_Create("ulit64", 64, UNSIGNED, LC_k64);

  pcrel13 = ISA_Lit_Opnd_Type_Create("pcrel13", 13, PCREL, LC_i13);
  pcrel16 = ISA_Lit_Opnd_Type_Create("pcrel16", 16, PCREL, LC_i16);
  pcrel25 = ISA_Lit_Opnd_Type_Create("pcrel25", 25, PCREL, LC_i25);
  pcrel26 = ISA_Lit_Opnd_Type_Create("pcrel26", 26, PCREL, LC_i26);
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

  hi    = ISA_Reg_Opnd_Type_Create("hi", ISA_REGISTER_CLASS_hilo, 
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, SIGNED, INVALID);
  lo    = ISA_Reg_Opnd_Type_Create("lo", ISA_REGISTER_CLASS_hilo, 
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, SIGNED, INVALID);
  fsr    = ISA_Reg_Opnd_Type_Create("fsr", ISA_REGISTER_CLASS_fpsr, 
				   ISA_REGISTER_SUBCLASS_UNDEFINED,
				   64, SIGNED, INVALID);


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
  opnd3      = Create_Operand_Use("opnd3");
  maddend    = Create_Operand_Use("maddend");


/* ===== (<%pr%p6>) <r7>=<%o1%r7>,<%o2%r7> ===== */
  Instruction_Group("O_1",
	TOP_mult_g, TOP_multu_g, TOP_dmult_g,
	TOP_dmultu_g, TOP_div_g, TOP_divu_g, TOP_ddiv_g,
	TOP_ddivu_g, TOP_mod_g, TOP_modu_g,
	TOP_dmod_g, TOP_dmodu_g,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);


/*=======================*/
  Instruction_Group("O_2",
        TOP_add,
        TOP_addu,
        TOP_and,
        TOP_dadd,
        TOP_daddu,
        TOP_dsllv,
        TOP_dsrav,
        TOP_dsrlv,
        TOP_dsub,
        TOP_dsubu,
        TOP_nor,
        TOP_or,
        TOP_sllv,
        TOP_slt,
        TOP_sltu,
        TOP_srav,
        TOP_srlv,
        TOP_sub,
        TOP_subu,
        TOP_xor,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);

/*=======================*/
  Instruction_Group("O_3",
        TOP_addi,
        TOP_addiu,
        TOP_daddi,
        TOP_daddiu,
        TOP_slti,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, lit16, opnd2);

/*=======================*/
  Instruction_Group("O_4",
        TOP_jr,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, target);

/*=======================*/
  Instruction_Group("O_5",
        TOP_lb,
        TOP_lbu,
        TOP_ld,
        TOP_ldr,
        TOP_lh,
        TOP_lhu,
        TOP_ll,
        TOP_lld,
        TOP_lw,
        TOP_lwr,
        TOP_lwu,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lit16, offset);
  Operand(2, int64, base);

/*=======================*/
  Instruction_Group("O_6",
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
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, storeval);
  Operand(2, lit16, offset);
  Operand(3, int64, base);

/*=======================*/
  Instruction_Group("O_7",
        TOP_teq,
        TOP_tge,
        TOP_tgeu,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);

/*=======================*/
  Instruction_Group("O_8",
        TOP_bgez,
        TOP_bgezal,
        TOP_bgtz,
        TOP_blez,
        TOP_bltz,
        TOP_bltzal,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, pcrel16, target);

/*=======================*/
  Instruction_Group("O_9",
        TOP_beq,
        TOP_bne,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);
  Operand(3, pcrel16, target);

/*=======================*/
  Instruction_Group("O_10",
        TOP_jal,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, pcrel26, target);

/*=======================*/
  Instruction_Group("O_11",
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, int64, target);

/*=======================*/
  Instruction_Group("O_12",
        TOP_lui,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, ulit16, opnd1);

/*=======================*/
  Instruction_Group("O_13",
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);

/*=======================*/
  Instruction_Group("O_14",
        TOP_mthi,
        TOP_mtlo,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);

/*=======================*/
  Instruction_Group("O_15",
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);

/*=======================*/
  Instruction_Group("O_16",
        TOP_dsll,
        TOP_dsra,
        TOP_dsra32,
        TOP_dsrl,
        TOP_dsrl32,
        TOP_sll,
        TOP_sra,
        TOP_srl,
        TOP_dsll32,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, ulit5, opnd2);

/*=======================*/
  Instruction_Group("O_17",
        TOP_abs_s,
        TOP_ceil_l_s,
        TOP_ceil_w_s,
        TOP_cvt_l_s,
        TOP_cvt_s_w,
        TOP_cvt_s_l,
        TOP_cvt_w_s,
        TOP_floor_l_s,
        TOP_floor_w_s,
        TOP_mov_s,
        TOP_neg_s,
        TOP_round_l_s,
        TOP_round_w_s,
        TOP_trunc_l_s,
        TOP_trunc_w_s,
        TOP_abs_d,
        TOP_ceil_l_d,
        TOP_ceil_w_d,
        TOP_cvt_l_d,
        TOP_cvt_s_d,
        TOP_cvt_w_d,
        TOP_floor_l_d,
        TOP_floor_w_d,
        TOP_mov_d,
        TOP_neg_d,
        TOP_round_l_d,
        TOP_round_w_d,
        TOP_trunc_l_d,
        TOP_trunc_w_d,
        TOP_cvt_d_s,
        TOP_cvt_d_w,
        TOP_cvt_d_l,
        TOP_sqrt_s,
        TOP_sqrt_d,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64, opnd1);

/*=======================*/
  Instruction_Group("O_18",
        TOP_add_s,
        TOP_div_s,
        TOP_mul_s,
        TOP_sub_s,
        TOP_add_d,
        TOP_div_d,
        TOP_mul_d,
        TOP_sub_d,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64, opnd1);
  Operand(2, fp64, opnd2);

/*=======================*/
  Instruction_Group("O_19",
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
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, fp64, opnd1);
  Operand(2, fp64, opnd2);

/*=======================*/
  Instruction_Group("O_20",
        TOP_j,
        TOP_bc1f,
        TOP_bc1fl,
        TOP_bc1t,
        TOP_bc1tl,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, pcrel16, target);

/*=======================*/
  Instruction_Group("O_21",
        TOP_dmtc1,
        TOP_mtc1,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, int64, opnd1);

/*=======================*/
  Instruction_Group("O_22",
        TOP_dmfc1,
        TOP_mfc1,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, fp64, opnd1);

/*=======================*/
  Instruction_Group("O_23",
        TOP_lwc1,
        TOP_ldc1,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, lit16, offset);
  Operand(2, int64, base);

/*=======================*/
  Instruction_Group("O_24",
        TOP_swc1,
        TOP_sdc1,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, fp64, storeval);
  Operand(2, lit16, offset);
  Operand(3, int64, base);

/*=======================*/
  Instruction_Group("O_25",
        TOP_andi,
        TOP_ori,
        TOP_sltiu,
        TOP_xori,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, ulit16, opnd2);

/*=======================*/
  Instruction_Group("O_26",
        TOP_break,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, ulit20, opnd1 );

/*=======================*/
  Instruction_Group("O_27",
        TOP_nop,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);

/*=======================*/
  Instruction_Group("O_28",
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64);
  Operand(2, int64, opnd1);
  Operand(3, int64, opnd2);

/*=======================*/
  Instruction_Group("O_29",
        TOP_teqi,
        TOP_tgei,
        TOP_tgeiu,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, lit16, opnd2);

/*=======================*/
  Instruction_Group("O_30",
        TOP_ldl,
        TOP_lwl,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, lit16, offset);
  Operand(3, int64, base);

/*=======================*/
  Instruction_Group("O_31",
        TOP_ddiv,
        TOP_ddivu,
        TOP_div,
        TOP_divu,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, hi);
  Result(1, lo);
  Operand(1, int64);
  Operand(2, int64, opnd1);
  Operand(3, int64, opnd2);

/*=======================*/
  Instruction_Group("O_32",
        TOP_dmult,
        TOP_dmultu,
        TOP_mult,
        TOP_multu,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, hi);
  Result(1, lo);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);

/*=======================*/
  Instruction_Group("O_33",
        TOP_mfhi,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, hi, opnd1);

/*=======================*/
  Instruction_Group("O_34",
        TOP_mflo,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, lo, opnd1);

/*=======================*/
  Instruction_Group("O_35",
        TOP_jalr,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, target);

/*=======================*/
  Instruction_Group("O_36",
        TOP_cfc1,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, fsr, opnd1);

/*=======================*/
  Instruction_Group("O_37",
        TOP_ctc1,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fsr);
  Operand(1, int64, opnd1);

/*=======================*/
  Instruction_Group("O_38",
        TOP_madd_d,
        TOP_madd_s,
        TOP_msub_d,
        TOP_msub_s,
        TOP_nmadd_d,
        TOP_nmadd_s,
        TOP_nmsub_d,
        TOP_nmsub_s,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, fp64);
  Operand(1, fp64, opnd1);
  Operand(2, fp64, opnd2);
  Operand(3, fp64, maddend);

/*=======================*/
  Instruction_Group("O_39",
        TOP_divlo,
        TOP_divhi,
        TOP_divulo,
        TOP_divuhi,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Result(1, hi);
  Result(2, lo);
  Operand(1, int64);
  Operand(2, int64, opnd1);
  Operand(3, int64, opnd2);

/*=======================*/
/* opnd3 is needed to express the dependance between conditional move op and the op before it.
 * Considering following instruction sequence, which do the job of "ret = op0 < op1 ? 0 : op0 - op1":
 *    sltu flag, op0, op1
 *	subu ret, op0, op1
 * 	movn result, val, flag, ret
 * The target of subu is also the operand of movn.
 * When emitting assemble instruction, opnd3 is not printed for assembly language syntax.
 */
  Instruction_Group("O_40",
        TOP_movz,
        TOP_movn,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Result(0, int64);
  Operand(1, int64, opnd1);
  Operand(2, int64, opnd2);
  Operand(3, int64, opnd3);

/* ===== (<%pr%p6>) <uimm21> ===== */
  Instruction_Group("O_41",
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, ulit21);

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

    Instruction_Group("O_spadjust",
	TOP_spadjust,
	TOP_UNDEFINED);
  Operand(0, pr, predicate);
  Operand(1, int64, opnd1);
  Operand(2, lit64, opnd2);
  Result(0, int64);

/* ====== No-op ======== */
  Instruction_Group("O_noop", 
	TOP_fwd_bar,	TOP_bwd_bar,	TOP_noop,
	TOP_UNDEFINED);

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
