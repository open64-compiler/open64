
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
// Print assembly for TOPS
/////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>
#include "topcode.h"
#include "isa_print_gen.h"

// Multiple topcodes map to the same assembly name. To disambiguate the 
// topcodes, we append a suffix to the basename. By convention, the 
// suffix starts with an underscore. To get the assembly name we strip off
// the suffix.

main()
{
  ISA_Print_Begin("pr1");

  Define_Macro("END_GROUP", ";");		// end-of-group marker
  Define_Macro("PREDICATE", "(%s)");		// predicate operand format
  Define_Macro("BEGIN_BUNDLE", "");	// bundle introducer
  Define_Macro("END_BUNDLE", ";");		// bundle terminator

  Set_AsmName_Func(NULL);

  /* No results / no operands */
  ISA_PRINT_TYPE no_rop = ISA_Print_Type_Create("no_rop", "%s");
  Name();
  Instruction_Print_Group(no_rop,
			  TOP_bret, TOP_ret, TOP_sys_reti, TOP_nop2, TOP_nop3, 
			  TOP_UNDEFINED);

  /* No results/ one operand */
  ISA_PRINT_TYPE op = ISA_Print_Type_Create("op", "%s\t%s");
  Name();
  Operand(0);
  Instruction_Print_Group(op,
			  TOP_b, TOP_b10, TOP_bcall, TOP_bcall10, 
			  TOP_bloop, TOP_bloopsz, TOP_j, TOP_jr, TOP_icall,
			  TOP_call, TOP_sys_brk, TOP_sys_swi, TOP_sys_pwr, 
			  TOP_copr_0, TOP_copr_1, TOP_copr_2, TOP_copr_3,
			  TOP_copr_4, TOP_copr_5, TOP_copr_6, TOP_copr_7, 
			  TOP_UNDEFINED);

  /* One result / one operand */
  ISA_PRINT_TYPE rop = ISA_Print_Type_Create("rop", "%s\t%s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(rop,
			  TOP_movi, TOP_add2, TOP_addi2, TOP_addiu2, TOP_addu2,
			  TOP_movgp, TOP_movlc, TOP_movsp, TOP_movra,
			  TOP_mov2gp, TOP_mov2lc, TOP_mov2sp, TOP_mov2ra,
			  TOP_movi2gp, TOP_movi2lc, TOP_movi2sp, TOP_movi2ra,
			  TOP_not2, TOP_noti2, 
			  TOP_or2, TOP_ori2, TOP_and2, TOP_andi2, TOP_subiu2,
			  TOP_xor2, TOP_xori2, TOP_sub2, TOP_subi2, TOP_subu2,
			  TOP_div2, TOP_divi2, TOP_divu2, TOP_diviu2, TOP_muliu2,
			  TOP_mul2, TOP_muli2, TOP_mulu2, TOP_shl2, TOP_shli2,
			  TOP_shr2, TOP_shri2, 
			  TOP_tgti2, TOP_teqi2, TOP_tgei2, TOP_tnei2,
			  TOP_tgemi2, TOP_tgtmi2,
			  TOP_bit_sext, TOP_bit_zext, TOP_bit_l0, TOP_bit_l1,
			  TOP_ldbi, TOP_ldhi, TOP_ldwi,
			  TOP_stbi, TOP_sthi, TOP_stwi, TOP_drr,
			  TOP_movt, TOP_movf, TOP_movti, TOP_movfi,
			  TOP_movtn, TOP_movfn, TOP_movtin, TOP_movfin,
			  TOP_UNDEFINED);

  /* No results / two operands */
  ISA_PRINT_TYPE opop = ISA_Print_Type_Create("opop", "%s\t%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(opop,
			  TOP_teq2, TOP_tge2, TOP_tgt2, TOP_tne2,
			  TOP_tgtm2, TOP_tnem2, TOP_tgem2,
			  TOP_tgem2, TOP_tgtm2, TOP_tnem2,
			  TOP_UNDEFINED);


  /* One result / two operands */
  ISA_PRINT_TYPE ropop = ISA_Print_Type_Create("ropop", "%s\t%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(ropop,
			  TOP_add3, TOP_addu3, TOP_addiu3, TOP_addi3, TOP_addlc3,
			  TOP_addgp3, TOP_addsp3, TOP_addra3, TOP_subi3,
			  TOP_and3, TOP_or3, TOP_xor3, TOP_sub3, TOP_subu3, 
			  TOP_subu3, TOP_subiu3, TOP_mul3, TOP_muli3, TOP_mulu3,
			  TOP_muliu3, TOP_andi3, TOP_ori3, TOP_xori3, 
			  TOP_div3, TOP_divi3, TOP_divu3, TOP_diviu3,
			  TOP_shl3, TOP_shr3, TOP_shli3, TOP_shri3,
			  TOP_stb2, TOP_sth2, TOP_stw2, TOP_stwgp2, TOP_stwgp3,
			  TOP_stb, TOP_sth, TOP_stw, TOP_stbsp2, TOP_sthsp2, TOP_stwsp2,
			  TOP_ldbsp3, TOP_ldhsp3, TOP_stwsp3, TOP_stbsp3, TOP_sthsp3,
			  TOP_ldb2, TOP_ldh2, TOP_ldw2, TOP_ldwgp2, TOP_ldwgp3,
			  TOP_ldb, TOP_ldh, TOP_ldw, TOP_ldbsp2, TOP_ldhsp2, TOP_ldwsp2,
			  TOP_ldbsp3, TOP_ldhsp3, TOP_ldwsp3,
			  TOP_pack16, TOP_pack32, TOP_packu16, TOP_packu32,
			  TOP_upack16, TOP_upack32, TOP_upacku16, TOP_upacku32,
			  TOP_shladgp, TOP_shladsp,
			  TOP_UNDEFINED);

  /* One inout / two operands */
  ISA_PRINT_TYPE ioopop = ISA_Print_Type_Create("ioopop", "%s\t%s,%s,%s");
  Name();
  Result(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(ioopop,
			  TOP_tgem3, TOP_tgtm3, TOP_tnem3,
			  TOP_tlem3, TOP_tlt3, TOP_tltm3,
			  TOP_tgemi3, TOP_tgtmi3, TOP_tnemi3,
			  TOP_tlemi3, TOP_tlti3, TOP_tltmi3,
			  TOP_tlemi3, TOP_tltmi3, TOP_teq3, 
			  TOP_tge3, TOP_tgt3, TOP_tne3, TOP_tle3,
			  TOP_tgei3, TOP_tgti3, TOP_tnei3, 		  
			  TOP_teqi3, TOP_tlei3, TOP_tlti3, 
			  TOP_UNDEFINED);

  /* No results / three operands */
  ISA_PRINT_TYPE opopop = ISA_Print_Type_Create("opopop", "%s\t%s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(opopop,
			  TOP_beq, TOP_beqpf, TOP_beqpt, 
			  TOP_bgt, TOP_bgtpf, TOP_bgtpt,
			  TOP_bge, TOP_bgepf, TOP_bgept,
			  TOP_bne, TOP_bnepf, TOP_bnept,		  
			  TOP_beqi, TOP_beqipf, TOP_beqipt,
			  TOP_bgti, TOP_bgtipf, TOP_bgtipt,
			  TOP_blti, TOP_bltipf, TOP_bltipt,
			  TOP_bgei, TOP_bgeipf, TOP_bgeipt,
			  TOP_blei, TOP_bleipf, TOP_bleipt,
			  TOP_bnei, TOP_bneipf, TOP_bneipt,	  
			  TOP_UNDEFINED);

  /* One result / three operands */
  ISA_PRINT_TYPE ropopop = ISA_Print_Type_Create("opopop", "%s\t%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(opopop,
			  TOP_dep, TOP_extr, TOP_depi, TOP_extri,
			  TOP_depu, TOP_extru, TOP_depiu, TOP_extriu,
			  TOP_shlad,
			  TOP_UNDEFINED);

  ISA_Print_End();
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
