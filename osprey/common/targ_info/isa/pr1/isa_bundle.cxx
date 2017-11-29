
/*

  Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.

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

*/

//  
//  Generate bundle information
///////////////////////////////////////

#include <stddef.h>
#include "topcode.h"
#include "isa_bundle_gen.h"

main()
{
  ISA_EXEC_UNIT_TYPE
    Fetch_Unit;  // Instruction fetch type

  ISA_Bundle_Begin("pr1", 32);

  ISA_Bundle_Pack_Create(ISA_Bundle_Pack_Little_Endian);
  Pack_Slot(0, 0, 0, 32);

  /* ===== Specification for Fetch_Unit Type ===== */
  Fetch_Unit = ISA_Exec_Unit_Type_Create("Fetch_Unit", NULL);
  Instruction_Exec_Unit_Group(Fetch_Unit,
  TOP_add2,
  TOP_addi2,
  TOP_addiu2,
  TOP_addu2,
  TOP_add3,
  TOP_addi3,
  TOP_addiu3,
  TOP_addu3,
  TOP_addlc3,
  TOP_addgp3,
  TOP_addsp3,
  TOP_addra3,
  TOP_and2,
  TOP_andi2,
  TOP_and3,
  TOP_andi3,
  TOP_b,
  TOP_b10,
  TOP_bcall,
  TOP_bcall10,
  TOP_beq,
  TOP_beqpt,
  TOP_beqpf,
  TOP_bgt,
  TOP_bgtpt,
  TOP_bgtpf,
  TOP_bge,
  TOP_bgept,
  TOP_bgepf,
  TOP_bloop,
  TOP_bloopsz,
  TOP_bne,
  TOP_bnept,
  TOP_bnepf,
  TOP_beqi,
  TOP_beqipf,
  TOP_beqipt,
  TOP_bgti,
  TOP_bgtipf,
  TOP_bgtipt,
  TOP_blti,
  TOP_bltipf,
  TOP_bltipt,
  TOP_bgei,
  TOP_bgeipf,
  TOP_bgeipt,
  TOP_blei,
  TOP_bleipf,
  TOP_bleipt,
  TOP_bnei,
  TOP_bneipf,
  TOP_bneipt,
  TOP_bret,
  TOP_bit_l0,
  TOP_bit_l1,
  TOP_bit_sext,
  TOP_bit_zext,
  TOP_copr_0,
  TOP_copr_1,
  TOP_copr_2,
  TOP_copr_3,
  TOP_copr_4,
  TOP_copr_5,
  TOP_copr_6,
  TOP_copr_7,
  TOP_drr,
  TOP_dep,
  TOP_depi,
  TOP_extr,
  TOP_extri,
  TOP_div2,
  TOP_divi2,
  TOP_diviu2,
  TOP_divu2,
  TOP_div3,
  TOP_divi3,
  TOP_diviu3,
  TOP_divu3,
  TOP_j,
  TOP_call,
  TOP_jr,
  TOP_icall,
  TOP_ret,
  TOP_ldb,
  TOP_ldh,
  TOP_ldw,
  TOP_ldb2,
  TOP_ldh2,
  TOP_ldw2,
  TOP_ldbi,
  TOP_ldhi,
  TOP_ldwi,
  TOP_ldwgp2,
  TOP_ldbsp2,
  TOP_ldhsp2,
  TOP_ldwsp2,
  TOP_ldwgp3,
  TOP_ldbsp3,
  TOP_ldhsp3,
  TOP_ldwsp3,
  TOP_movlc,
  TOP_movgp,
  TOP_movsp,
  TOP_movra,
  TOP_mov2lc,
  TOP_mov2gp,
  TOP_mov2sp,
  TOP_mov2ra,
  TOP_movi2lc,
  TOP_movi2gp,
  TOP_movi2sp,
  TOP_movi2ra,
  TOP_movi,
  TOP_movt,
  TOP_movf,
  TOP_movti,
  TOP_movfi,
  TOP_movtn,
  TOP_movfn,
  TOP_movtin,
  TOP_movfin,
  TOP_mul2,
  TOP_muli2,
  TOP_muliu2,
  TOP_mulu2,
  TOP_mul3,
  TOP_muli3,
  TOP_mulu3,
  TOP_muliu3,
  TOP_not2,
  TOP_noti2,
  TOP_nop2,
  TOP_nop3,
  TOP_or2,
  TOP_ori2,
  TOP_or3,
  TOP_ori3,
  TOP_pack16,
  TOP_pack32,
  TOP_packu16,
  TOP_packu32,
  TOP_upack16,
  TOP_upack32,
  TOP_upacku16,
  TOP_upacku32,
  TOP_shl2,
  TOP_shli2,
  TOP_shl3,
  TOP_shli3,
  TOP_shr2,
  TOP_shri2,
  TOP_shr3,
  TOP_shri3,
  TOP_shlad,
  TOP_shladgp,
  TOP_shladsp,
  TOP_stb,
  TOP_sth,
  TOP_stw,
  TOP_stb2,
  TOP_sth2,
  TOP_stw2,
  TOP_stbi,
  TOP_sthi,
  TOP_stwi,
  TOP_stwgp2,
  TOP_stbsp2,
  TOP_sthsp2,
  TOP_stwsp2,
  TOP_stwgp3,
  TOP_stbsp3,
  TOP_sthsp3,
  TOP_stwsp3,
  TOP_sub2,
  TOP_subi2,
  TOP_subiu2,
  TOP_subu2,
  TOP_sub3,
  TOP_subi3,
  TOP_subiu3,
  TOP_subu3,
  TOP_sys_brk,
  TOP_sys_swi,
  TOP_sys_reti,
  TOP_sys_pwr,
  TOP_teq2,
  TOP_teqi2,
  TOP_tge2,
  TOP_tgei2,
  TOP_tgem2,
  TOP_tgemi2,
  TOP_tgt2,
  TOP_tgtm2,
  TOP_tgti2,
  TOP_tgtmi2,
  TOP_tne2,
  TOP_tnei2,
  TOP_tnem2,
  TOP_teq3,
  TOP_teqi3,
  TOP_tle3,
  TOP_tlem3,
  TOP_tlei3,
  TOP_tlemi3,
  TOP_tlt3,
  TOP_tltm3,
  TOP_tlti3,
  TOP_tltmi3,
  TOP_tge3,
  TOP_tgei3,
  TOP_tgem3,
  TOP_tgemi3,
  TOP_tgt3,
  TOP_tgti3,
  TOP_tgtm3,
  TOP_tgtmi3,
  TOP_tne3,
  TOP_tnei3,
  TOP_tnem3,
  TOP_tnemi3,
  TOP_xor2,
  TOP_xori2,
  TOP_xor3,
  TOP_xori3,
  TOP_asm,
  TOP_intrncall,
  TOP_spadjust,
  TOP_begin_pregtn,
  TOP_end_pregtn,
  TOP_bwd_bar,
  TOP_fwd_bar,
  TOP_label,
  TOP_noop,
  TOP_UNDEFINED
);

  ISA_Bundle_Type_Create("i", ".i", 1);
  Slot (0, Fetch_Unit);

  ISA_Bundle_End();
  return 0;
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
