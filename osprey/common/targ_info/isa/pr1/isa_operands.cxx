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
//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:18:12 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/pr1/isa_operands.cxx,v $

#include <stddef.h>
#include "topcode.h"
#include "isa_operands_gen.h"

main()
{
  OPERAND_VALUE_TYPE ir, dr, sr, gpbase, sr_lc, sr_pc, sr_sp, sr_gp, sr_ra, sr_pd;

  OPERAND_USE_TYPE
	  predicate,	// a qualifying predicate
	  base,		// a base address (for memory insts)
	  offset,	// an offset added to a base (implies immed val)
	  postincr,	// a post increment applied to a base address
	  target,	// the target of a branch
	  storeval,	// value to be stored
	  opnd1,	// first/left operand of an alu operator
	  opnd2,	// second/right operand of an alu operator
	  opnd3,	// third/right operand of an alu operator
	  maddend;	// addend/subtrahend operand of a madd

  ISA_Operands_Begin("pr1");

  /* Create the register operand types:
   */
  ir  = ISA_Reg_Opnd_Type_Create("ir", ISA_REGISTER_CLASS_integer, 
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
  dr  = ISA_Reg_Opnd_Type_Create("dr", ISA_REGISTER_CLASS_dsp, 
				 ISA_REGISTER_SUBCLASS_UNDEFINED,
				 32, SIGNED, INVALID);
  sr  = ISA_Reg_Opnd_Type_Create("sr", ISA_REGISTER_CLASS_special, 
				 ISA_REGISTER_SUBCLASS_UNDEFINED,
				 32, UNSIGNED, INVALID);
  sr_lc = ISA_Reg_Opnd_Type_Create("sr_lc", ISA_REGISTER_CLASS_special, 
				   ISA_REGISTER_SUBCLASS_sr_lc,
				   32, UNSIGNED, INVALID);
  sr_pc = ISA_Reg_Opnd_Type_Create("sr_pc", ISA_REGISTER_CLASS_special, 
				   ISA_REGISTER_SUBCLASS_sr_pc,
				   32, UNSIGNED, INVALID);
  sr_gp = ISA_Reg_Opnd_Type_Create("sr_gp", ISA_REGISTER_CLASS_special, 
				   ISA_REGISTER_SUBCLASS_sr_gp,
				   32, UNSIGNED, INVALID);
  sr_ra = ISA_Reg_Opnd_Type_Create("sr_ra", ISA_REGISTER_CLASS_special, 
				   ISA_REGISTER_SUBCLASS_sr_ra,
				   32, UNSIGNED, INVALID);
  sr_sp = ISA_Reg_Opnd_Type_Create("sr_sp", ISA_REGISTER_CLASS_special, 
				   ISA_REGISTER_SUBCLASS_sr_sp,
				   32, UNSIGNED, INVALID);
  sr_pd = ISA_Reg_Opnd_Type_Create("sr_pd", ISA_REGISTER_CLASS_special, 
				   ISA_REGISTER_SUBCLASS_sr_pd,
				   1, UNSIGNED, INVALID);
  gpbase = ISA_Reg_Opnd_Type_Create("int", ISA_REGISTER_CLASS_integer,
				   ISA_REGISTER_SUBCLASS_gpbase,
				   32, SIGNED, INVALID); 

 /* Create the literal operand types:
   */
  OPERAND_VALUE_TYPE lit4, lit5, lit6, lit7, lit8, lit15, lit16, lit20;
  OPERAND_VALUE_TYPE lit24, ulit4, ulit5, ulit8;
  OPERAND_VALUE_TYPE lit21, ulit10, ulit15, ulit32, pcrel10;
  OPERAND_VALUE_TYPE pcrel13, pcrel21, pcrel32, len4, len5, scmp;
  lit4   = ISA_Lit_Opnd_Type_Create("lit4",   4, SIGNED, LC_pr_i4);
  lit5   = ISA_Lit_Opnd_Type_Create("lit5",   4, SIGNED, LC_pr_i5);
  lit6   = ISA_Lit_Opnd_Type_Create("lit6",   6, SIGNED, LC_pr_i6);
  lit7   = ISA_Lit_Opnd_Type_Create("lit7",   7, SIGNED, LC_pr_i7);
  lit8   = ISA_Lit_Opnd_Type_Create("lit8",   8, SIGNED, LC_pr_i8);
  lit15  = ISA_Lit_Opnd_Type_Create("lit15", 15, SIGNED, LC_pr_i15);
  lit16  = ISA_Lit_Opnd_Type_Create("lit16", 16, SIGNED, LC_pr_i16);
  lit20  = ISA_Lit_Opnd_Type_Create("lit20", 20, SIGNED, LC_pr_i20);
  lit21  = ISA_Lit_Opnd_Type_Create("lit21", 21, SIGNED, LC_pr_i21);
  lit24  = ISA_Lit_Opnd_Type_Create("lit24", 24, SIGNED, LC_pr_i24);

  ulit4  = ISA_Lit_Opnd_Type_Create("ulit4",   4, UNSIGNED, LC_pr_u4);
  ulit5  = ISA_Lit_Opnd_Type_Create("ulit5",   5, UNSIGNED, LC_pr_u5);
  ulit8  = ISA_Lit_Opnd_Type_Create("ulit8",   8, UNSIGNED, LC_pr_u8);
  ulit10 = ISA_Lit_Opnd_Type_Create("ulit10", 10, UNSIGNED, LC_pr_u10);
  ulit15 = ISA_Lit_Opnd_Type_Create("ulit15", 15, UNSIGNED, LC_pr_u15);
  ulit32 = ISA_Lit_Opnd_Type_Create("ulit32", 32, UNSIGNED, LC_pr_u32);

  pcrel10 = ISA_Lit_Opnd_Type_Create("pcrel10", 10, PCREL, LC_pr_i10);
  pcrel13 = ISA_Lit_Opnd_Type_Create("pcrel13", 13, PCREL, LC_pr_i13);
  pcrel21 = ISA_Lit_Opnd_Type_Create("pcrel21", 21, PCREL, LC_pr_i21);
  pcrel32 = ISA_Lit_Opnd_Type_Create("pcrel32", 32, PCREL, LC_pr_i32);

  len4 = ISA_Lit_Opnd_Type_Create("len4", 5, UNSIGNED, LC_pr_u4);
  len5 = ISA_Lit_Opnd_Type_Create("len5", 6, UNSIGNED, LC_pr_u5);
  scmp = ISA_Lit_Opnd_Type_Create("scmp", 5, SIGNED, LC_pr_i4);

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

/* =====  ===== */
  Instruction_Group("nop",
		    TOP_nop2, TOP_nop3,
		    TOP_UNDEFINED);
  
  //
  Instruction_Group("swi",
		    TOP_sys_swi,
		    TOP_sys_reti,
		    TOP_UNDEFINED);
  Operand(0, ulit5, opnd1);

  //
  Instruction_Group("br",
		    TOP_beq, TOP_beqpf, TOP_beqpt, 
		    TOP_bgt, TOP_bgtpf, TOP_bgtpt,
		    TOP_bge, TOP_bgepf, TOP_bgept,
		    TOP_bne, TOP_bnepf, TOP_bnept,
		    TOP_UNDEFINED);
  Operand(0, ir, opnd1);
  Operand(1, ir, opnd2);
  Operand(2, pcrel13, target);
  Relocatable(2);

  //
  Instruction_Group("br_imm",
		    TOP_beqi, TOP_beqipf, TOP_beqipt, 
		    TOP_bgti, TOP_bgtipf, TOP_bgtipt,
		    TOP_bgei, TOP_bgeipf, TOP_bgeipt,
		    TOP_bnei, TOP_bneipf, TOP_bneipt,
		    TOP_blei, TOP_bleipf, TOP_bleipt,
		    TOP_blti, TOP_bltipf, TOP_bltipt,
		    TOP_UNDEFINED);
  Operand(0, ir, opnd1);
  Operand(1, lit4, opnd2);
  Operand(2, pcrel13, target);
  Relocatable(2);

  //
  Instruction_Group("break",
		    TOP_sys_brk,
		    TOP_UNDEFINED);
  Operand(0, ulit5, opnd1);

  //
  Instruction_Group("coproc",
		    TOP_copr_0, TOP_copr_1, TOP_copr_2, TOP_copr_3, TOP_copr_4, 
		    TOP_copr_5, TOP_copr_6, TOP_copr_7, 
		    TOP_UNDEFINED);
  Operand(0, ulit5, opnd1);

  //
  Instruction_Group("power",
		    TOP_sys_pwr,
		    TOP_UNDEFINED);
  Operand(0, ulit5, opnd1);

  //
  Instruction_Group("b_short",
		    TOP_bcall10, TOP_b10,
		    TOP_UNDEFINED);
  Operand(0, pcrel10, target);
  Relocatable(0);

  //
  Instruction_Group("b_n",
		    TOP_bcall, TOP_b,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, pcrel21, target);
  Relocatable(0);

  //
  Instruction_Group("j_n",
		    TOP_call, TOP_j,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, lit24, target);
  Relocatable(0);

  //
  Instruction_Group("j_i",
		    TOP_icall, TOP_jr,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, ir, opnd1);
 
  //
  Instruction_Group("b_ret",
		    TOP_bret,
		    TOP_UNDEFINED);
  Result(0, sr);

  //
  Instruction_Group("j_ret",
		    TOP_ret,
		    TOP_UNDEFINED);
  Result(0, sr);

  //
  Instruction_Group("bloop",
		    TOP_bloop, TOP_bloopsz,
		    TOP_UNDEFINED);
  Operand(0, ulit10, opnd1);
 
  // designate rotate register
  Instruction_Group("drr",
		    TOP_drr,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, lit6, opnd1);
 

  // 
  Instruction_Group("addi2",
		    TOP_addi2, TOP_subi2, TOP_muli2,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, lit4, opnd1);


  // 
  Instruction_Group("addiu2",
		    TOP_addiu2, TOP_subiu2, TOP_muliu2, TOP_diviu2, TOP_divi2,
		    TOP_ori2, TOP_andi2, TOP_xori2, TOP_noti2,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ulit4, opnd1);

  // 
  Instruction_Group("add2",
		    TOP_add2, TOP_sub2, TOP_div2, TOP_mul2,
		    TOP_addu2, TOP_subu2, TOP_divu2, TOP_mulu2,
		    TOP_or2, TOP_and2, TOP_xor2, TOP_not2,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);

  // 
  Instruction_Group("addi3",
		    TOP_addi3, TOP_subi3, TOP_divi3, TOP_muli3,
		    TOP_ori3, TOP_andi3, TOP_xori3,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, lit15, opnd2);

  // 
  Instruction_Group("addiu3",
		    TOP_addiu3, TOP_subiu3, TOP_diviu3, TOP_muliu3,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, ulit15, opnd2);

  // 
  Instruction_Group("add3",
		    TOP_add3, TOP_sub3, TOP_div3, TOP_mul3,
		    TOP_addu3, TOP_subu3, TOP_divu3, TOP_mulu3,
		    TOP_or3, TOP_and3, TOP_xor3,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, ir, opnd2);
  
  // movt ir, ir
  Instruction_Group("movpred",
		    TOP_movt, TOP_movf, TOP_movtn, TOP_movfn,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);

  // movti ir, imm4
  Instruction_Group("movimmpred",
		    TOP_movti, TOP_movfi, TOP_movtin, TOP_movfin,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, lit4, opnd1);

  // movsr ir, sr
  Instruction_Group("movsr2",
		    TOP_movgp, TOP_movlc, TOP_movsp, TOP_movra,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, sr, opnd1);

  //  mov2sr sr, ir
  Instruction_Group("mov2sr",
		    TOP_mov2gp, TOP_mov2lc, TOP_mov2sp, TOP_mov2ra,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, ir, opnd1);

  //  movi2sr sr, imm
  Instruction_Group("movi2sr",
		    TOP_movi2gp, TOP_movi2lc, TOP_movi2sp, TOP_movi2ra,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, lit6, opnd1);

  // addsr3 sr, sr, imm
  Instruction_Group("addsr3",
		    TOP_addgp3, TOP_addsp3,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, sr, opnd1);
  Operand(1, lit20, opnd2);

  // addsr3 sr, ir, imm
  Instruction_Group("addsrir3",
		    TOP_addlc3, TOP_addra3,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, ir, opnd1);
  Operand(1, lit20, opnd2);

  //
  Instruction_Group("bit",
		    TOP_bit_l0, TOP_bit_l1, TOP_bit_sext, TOP_bit_zext,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);

  //
  Instruction_Group("bits",
		    TOP_dep, TOP_extr,
		    TOP_depu, TOP_extru,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, len5, opnd2);
  Operand(2, len5, opnd3);

  //
  Instruction_Group("bitis",
		    TOP_depi, TOP_extri,
		    TOP_depiu, TOP_extriu,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, ulit5, opnd2);
  Operand(2, len5, opnd3);

  //
  Instruction_Group("shlad",
		    TOP_shlad,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, ir, opnd2);
  Operand(2, len5, opnd3);

  //
  Instruction_Group("shladsr",
		    TOP_shladgp, TOP_shladsp,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, ir, opnd1);
  Operand(1, len5, opnd2);

  //
  Instruction_Group("sh2",
		    TOP_shl2, TOP_shr2,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);

  //
  Instruction_Group("shi2",
		    TOP_shli2, TOP_shri2,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, lit4, opnd1);

  //
  Instruction_Group("sh3",
		    TOP_shl3, TOP_shr3,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, ir, opnd2);

  //
  Instruction_Group("sh3",
		    TOP_shli3, TOP_shri3,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, lit8, opnd2);

  //
  Instruction_Group("tst2",
		    TOP_teq2, TOP_tge2, TOP_tgem2, 
		    TOP_tgt2, TOP_tgtm2, TOP_tne2, TOP_tnem2, 
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);

  //
  Instruction_Group("tsti2",
		    TOP_teqi2, TOP_tgei2,
		    TOP_tgti2, TOP_tnei2,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0,lit4, opnd1);

  //
  Instruction_Group("tstmi2",
		    TOP_tgemi2, TOP_tgtmi2,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0,lit5, opnd1);

  //
  Instruction_Group("tst3",
		    TOP_teq3, TOP_tge3, TOP_tgem3, TOP_tlt3,
		    TOP_tgt3, TOP_tgtm3, TOP_tne3, TOP_tnem3, 
		    TOP_tltm3, TOP_tle3, TOP_tlem3,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, ir, opnd2);

  //
  Instruction_Group("tsti3",
		    TOP_teqi3, TOP_tgei3, TOP_tgemi3,
		    TOP_tgti3, TOP_tgtmi3, TOP_tnei3, TOP_tnemi3,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, lit15, opnd1);

  //
  Instruction_Group("tsti3",
		    TOP_tlti3, TOP_tltmi3, TOP_tlei3, TOP_tlemi3, 
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, lit15, opnd1);
  Operand(1, ir, opnd1);

  //
  Instruction_Group("memstore",
		    TOP_stb, TOP_sth, TOP_stw, 
		    TOP_stb2, TOP_sth2, TOP_stw2, 
		    TOP_stbi, TOP_sthi, TOP_stwi, 
		    TOP_UNDEFINED);
  Operand(0, ir, storeval);
  Operand(1, ir, base);
  Operand(2, lit16, offset);

  // stw sr,imm
  Instruction_Group("srstore2",
		    TOP_stwgp2, TOP_stbsp2, TOP_sthsp2, TOP_stwsp2, 
		    TOP_UNDEFINED);
  Operand(0, ir, storeval);
  Operand(1, sr, base);
  Operand(2, lit7, offset);

  // stw sr,imm
  Instruction_Group("srstore3",
		    TOP_stwgp3, TOP_stbsp3, TOP_sthsp3, TOP_stwsp3, 
		    TOP_UNDEFINED);
  Operand(0, ir, storeval);
  Operand(1, sr, base);
  Operand(2, lit21, offset);

  //
  Instruction_Group("memload",
		    TOP_ldb, TOP_ldh, TOP_ldw, 
		    TOP_ldb2, TOP_ldh2, TOP_ldw2, 
		    TOP_ldbi, TOP_ldhi, TOP_ldwi, 
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, base);
  Operand(1, lit16, offset);

  // ldw sr,imm
  Instruction_Group("srload2",
		    TOP_ldwgp2, TOP_ldbsp2, TOP_ldhsp2, TOP_ldwsp2, 
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, sr, base);
  Operand(1, lit7, offset);

  // ldw sr,imm
  Instruction_Group("srload3",
		    TOP_ldwgp3, TOP_ldbsp3, TOP_ldhsp3, TOP_ldwsp3, 
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, sr, base);
  Operand(1, lit21, offset);

  //
  Instruction_Group("movi",
		    TOP_movi,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ulit32, opnd1);
  Relocatable(0);

  //
  Instruction_Group("media",
		    TOP_pack16, TOP_upack16, TOP_packu16, TOP_upacku16,
		    TOP_pack32, TOP_upack32, TOP_packu32, TOP_upacku32,
		    TOP_UNDEFINED);
  Result(0, ir);
  Operand(0, ir, opnd1);
  Operand(1, ir, opnd2);

  //
  Instruction_Group("O_spadjust",
	TOP_spadjust,
	TOP_UNDEFINED);
  Result(0, ulit32);
  Operand(0, sr, opnd1);
  Operand(1, ulit32, opnd2);

/* ===== preg-tn tuple ===== */
  Instruction_Group("O_noop_pregtn",
	TOP_begin_pregtn,	TOP_end_pregtn,
	TOP_UNDEFINED);
  Operand(0, ir);
  Operand(1, lit16, opnd1);

/* ===== intrncall  R0 <- intrinsic_call (ID, ...) ===== */
  Instruction_Group (	"O_intrncall",
			TOP_intrncall,
			TOP_UNDEFINED);
  Result(0, ir);
  Operand (0, ulit32, opnd1);	// the intrinsic ID
  Operand (1, ulit32, opnd2);	// the intrinsic ID
  Operand (2, ulit32, opnd3);	// the intrinsic ID

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
  Operand(0, pcrel32,target);
  Relocatable(0);

  ISA_Operands_End();
  return 0;
}
