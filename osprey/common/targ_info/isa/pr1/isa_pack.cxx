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
  ISA_Pack_Begin("pr1", 32);
  ISA_PACK_TYPE p1 = ISA_Pack_Type_Create("p1");
  Result(0, 11, 4);
  Operand(0, 0, 11, 4);
  Operand(1, 0, 15, 4);
  Operand(2, 0, 19, 4);
  Instruction_Pack_Group(p1,
			 TOP_add2, 0x00000002ff,
			 TOP_addi2, 0x00000002ff,
			 TOP_addiu2, 0x00000002ff,
			 TOP_addu2, 0x00000002ff,
			 TOP_add3, 0x00000002ff,
			 TOP_addi3, 0x00000002ff,
			 TOP_addiu3, 0x00000002ff,
			 TOP_addu3, 0x00000002ff,
			 TOP_addlc3, 0x00000002ff,
			 TOP_addgp3, 0x00000002ff,
			 TOP_addsp3, 0x00000002ff,
			 TOP_addra3, 0x00000002ff,
			 TOP_and2, 0x00000002ff,
			 TOP_andi2, 0x00000002ff,
			 TOP_and3, 0x00000002ff,
			 TOP_andi3, 0x00000002ff,
			 TOP_b, 0x00000002ff,
			 TOP_b10, 0x00000002ff,
			 TOP_bcall, 0x00000002ff,
			 TOP_bcall10, 0x00000002ff,
			 TOP_beq, 0x00000002ff,
			 TOP_beqpt, 0x00000002ff,
			 TOP_beqpf, 0x00000002ff,
			 TOP_bgt, 0x00000002ff,
			 TOP_bgtpt, 0x00000002ff,
			 TOP_bgtpf, 0x00000002ff,
			 TOP_bge, 0x00000002ff,
			 TOP_bgept, 0x00000002ff,
			 TOP_bgepf, 0x00000002ff,
			 TOP_bloop, 0x00000002ff,
			 TOP_bloopsz, 0x00000002ff,
			 TOP_bne, 0x00000002ff,
			 TOP_bnept, 0x00000002ff,
			 TOP_bnepf, 0x00000002ff,
			 TOP_beqi, 0x00000002ff,
			 TOP_beqipf, 0x00000002ff,
			 TOP_beqipt, 0x00000002ff,
			 TOP_bgti, 0x00000002ff,
			 TOP_bgtipf, 0x00000002ff,
			 TOP_bgtipt, 0x00000002ff,
			 TOP_blti, 0x00000002ff,
			 TOP_bltipf, 0x00000002ff,
			 TOP_bltipt, 0x00000002ff,
			 TOP_bgei, 0x00000002ff,
			 TOP_bgeipf, 0x00000002ff,
			 TOP_bgeipt, 0x00000002ff,
			 TOP_blei, 0x00000002ff,
			 TOP_bleipf, 0x00000002ff,
			 TOP_bleipt, 0x00000002ff,
			 TOP_bnei, 0x00000002ff,
			 TOP_bneipf, 0x00000002ff,
			 TOP_bneipt, 0x00000002ff,
			 TOP_bret, 0x00000002ff,
			 TOP_bit_l0, 0x00000002ff,
			 TOP_bit_l1, 0x00000002ff,
			 TOP_bit_sext, 0x00000002ff,
			 TOP_bit_zext, 0x00000002ff,
			 TOP_copr_0, 0x00000002ff,
			 TOP_copr_1, 0x00000002ff,
			 TOP_copr_2, 0x00000002ff,
			 TOP_copr_3, 0x00000002ff,
			 TOP_copr_4, 0x00000002ff,
			 TOP_copr_5, 0x00000002ff,
			 TOP_copr_6, 0x00000002ff,
			 TOP_copr_7, 0x00000002ff,
			 TOP_drr, 0x00000002ff,
			 TOP_dep, 0x00000002ff,
			 TOP_depi, 0x00000002ff,
			 TOP_depu, 0x00000002ff,
			 TOP_depiu, 0x00000002ff,
			 TOP_extru, 0x00000002ff,
			 TOP_extriu, 0x00000002ff,
			 TOP_extr, 0x00000002ff,
			 TOP_extri, 0x00000002ff,
			 TOP_div2, 0x00000002ff,
			 TOP_divi2, 0x00000002ff,
			 TOP_diviu2, 0x00000002ff,
			 TOP_divu2, 0x00000002ff,
			 TOP_div3, 0x00000002ff,
			 TOP_divi3, 0x00000002ff,
			 TOP_diviu3, 0x00000002ff,
			 TOP_divu3, 0x00000002ff,
			 TOP_j, 0x00000002ff,
			 TOP_call, 0x00000002ff,
			 TOP_jr, 0x00000002ff,
			 TOP_icall, 0x00000002ff,
			 TOP_ret, 0x00000002ff,
			 TOP_ldb, 0x00000002ff,
			 TOP_ldh, 0x00000002ff,
			 TOP_ldw, 0x00000002ff,
			 TOP_ldb2, 0x00000002ff,
			 TOP_ldh2, 0x00000002ff,
			 TOP_ldw2, 0x00000002ff,
			 TOP_ldbi, 0x00000002ff,
			 TOP_ldhi, 0x00000002ff,
			 TOP_ldwi, 0x00000002ff,
			 TOP_ldwgp2, 0x00000002ff,
			 TOP_ldbsp2, 0x00000002ff,
			 TOP_ldhsp2, 0x00000002ff,
			 TOP_ldwsp2, 0x00000002ff,
			 TOP_ldwgp3, 0x00000002ff,
			 TOP_ldbsp3, 0x00000002ff,
			 TOP_ldhsp3, 0x00000002ff,
			 TOP_ldwsp3, 0x00000002ff,
			 TOP_movlc, 0x00000002ff,
			 TOP_movgp, 0x00000002ff,
			 TOP_movsp, 0x00000002ff,
			 TOP_movra, 0x00000002ff,
			 TOP_mov2lc, 0x00000002ff,
			 TOP_mov2gp, 0x00000002ff,
			 TOP_mov2sp, 0x00000002ff,
			 TOP_mov2ra, 0x00000002ff,
			 TOP_movi2lc, 0x00000002ff,
			 TOP_movi2gp, 0x00000002ff,
			 TOP_movi2sp, 0x00000002ff,
			 TOP_movi2ra, 0x00000002ff,
			 TOP_movi, 0x00000002ff,
			 TOP_movt, 0x00000002ff,
			 TOP_movf, 0x00000002ff,
			 TOP_movti, 0x00000002ff,
			 TOP_movfi, 0x00000002ff,
			 TOP_movtn, 0x00000002ff,
			 TOP_movfn, 0x00000002ff,
			 TOP_movtin, 0x00000002ff,
			 TOP_movfin, 0x00000002ff,
			 TOP_mul2, 0x00000002ff,
			 TOP_muli2, 0x00000002ff,
			 TOP_muliu2, 0x00000002ff,
			 TOP_mulu2, 0x00000002ff,
			 TOP_mul3, 0x00000002ff,
			 TOP_muli3, 0x00000002ff,
			 TOP_mulu3, 0x00000002ff,
			 TOP_muliu3, 0x00000002ff,
			 TOP_not2, 0x00000002ff,
			 TOP_noti2, 0x00000002ff,
			 TOP_nop2, 0x00000002ff,
			 TOP_nop3, 0x00000002ff,
			 TOP_or2, 0x00000002ff,
			 TOP_ori2, 0x00000002ff,
			 TOP_or3, 0x00000002ff,
			 TOP_ori3, 0x00000002ff,
			 TOP_pack16, 0x00000002ff,
			 TOP_pack32, 0x00000002ff,
			 TOP_packu16, 0x00000002ff,
			 TOP_packu32, 0x00000002ff,
			 TOP_upack16, 0x00000002ff,
			 TOP_upack32, 0x00000002ff,
			 TOP_upacku16, 0x00000002ff,
			 TOP_upacku32, 0x00000002ff,
			 TOP_shl2, 0x00000002ff,
			 TOP_shli2, 0x00000002ff,
			 TOP_shl3, 0x00000002ff,
			 TOP_shli3, 0x00000002ff,
			 TOP_shr2, 0x00000002ff,
			 TOP_shri2, 0x00000002ff,
			 TOP_shr3, 0x00000002ff,
			 TOP_shri3, 0x00000002ff,
			 TOP_shlad, 0x00000002ff,
			 TOP_shladgp, 0x00000002ff,
			 TOP_shladsp, 0x00000002ff,
			 TOP_stb, 0x00000002ff,
			 TOP_sth, 0x00000002ff,
			 TOP_stw, 0x00000002ff,
			 TOP_stb2, 0x00000002ff,
			 TOP_sth2, 0x00000002ff,
			 TOP_stw2, 0x00000002ff,
			 TOP_stbi, 0x00000002ff,
			 TOP_sthi, 0x00000002ff,
			 TOP_stwi, 0x00000002ff,
			 TOP_stwgp2, 0x00000002ff,
			 TOP_stbsp2, 0x00000002ff,
			 TOP_sthsp2, 0x00000002ff,
			 TOP_stwsp2, 0x00000002ff,
			 TOP_stwgp3, 0x00000002ff,
			 TOP_stbsp3, 0x00000002ff,
			 TOP_sthsp3, 0x00000002ff,
			 TOP_stwsp3, 0x00000002ff,
			 TOP_sub2, 0x00000002ff,
			 TOP_subi2, 0x00000002ff,
			 TOP_subiu2, 0x00000002ff,
			 TOP_subu2, 0x00000002ff,
			 TOP_sub3, 0x00000002ff,
			 TOP_subi3, 0x00000002ff,
			 TOP_subiu3, 0x00000002ff,
			 TOP_subu3, 0x00000002ff,
			 TOP_sys_brk, 0x00000002ff,
			 TOP_sys_swi, 0x00000002ff,
			 TOP_sys_reti, 0x00000002ff,
			 TOP_sys_pwr, 0x00000002ff,
			 TOP_teq2, 0x00000002ff,
			 TOP_teqi2, 0x00000002ff,
			 TOP_tge2, 0x00000002ff,
			 TOP_tgei2, 0x00000002ff,
			 TOP_tgem2, 0x00000002ff,
			 TOP_tgemi2, 0x00000002ff,
			 TOP_tgt2, 0x00000002ff,
			 TOP_tgtm2, 0x00000002ff,
			 TOP_tgti2, 0x00000002ff,
			 TOP_tgtmi2, 0x00000002ff,
			 TOP_tne2, 0x00000002ff,
			 TOP_tnei2, 0x00000002ff,
			 TOP_tnem2, 0x00000002ff,
			 TOP_teq3, 0x00000002ff,
			 TOP_teqi3, 0x00000002ff,
			 TOP_tle3, 0x00000002ff,
			 TOP_tlem3, 0x00000002ff,
			 TOP_tlei3, 0x00000002ff,
			 TOP_tlemi3, 0x00000002ff,
			 TOP_tlt3, 0x00000002ff,
			 TOP_tltm3, 0x00000002ff,
			 TOP_tlti3, 0x00000002ff,
			 TOP_tltmi3, 0x00000002ff,
			 TOP_tge3, 0x00000002ff,
			 TOP_tgei3, 0x00000002ff,
			 TOP_tgem3, 0x00000002ff,
			 TOP_tgemi3, 0x00000002ff,
			 TOP_tgt3, 0x00000002ff,
			 TOP_tgti3, 0x00000002ff,
			 TOP_tgtm3, 0x00000002ff,
			 TOP_tgtmi3, 0x00000002ff,
			 TOP_tne3, 0x00000002ff,
			 TOP_tnei3, 0x00000002ff,
			 TOP_tnem3, 0x00000002ff,
			 TOP_tnemi3, 0x00000002ff,
			 TOP_xor2, 0x00000002ff,
			 TOP_xori2, 0x00000002ff,
			 TOP_xor3, 0x00000002ff,
			 TOP_xori3, 0x00000002ff,
			 TOP_UNDEFINED);
  			 
  ISA_Pack_End();
  return 0;
}
