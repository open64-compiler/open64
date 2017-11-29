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
//  Generate ISA properties information
///////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. Operator attributes descriptors
//   2. Exception classification descriptors
//   3. Other operator descriptors (mostly for global optimization). 
//
// Within each ISA_PROPERTY instructions are listed in alphabetical order and
// as shown in the ISA manual
/////////////////////////////////////
//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:18:12 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/pr1/isa_properties.cxx,v $


#include <stddef.h>
#include "topcode.h"
#include "isa_properties_gen.h"

main()
{
  ISA_PROPERTY 
    load,   		/* Memory load operator */
    store, 		/* Memory store operator */
    prefetch,		/* Prefetch operator */
    xfer, 		/* Control transfer operator */
    call, 		/* Subprogram call operator */
    cond, 		/* Call/xfer is conditional */
    likely, 		/* Cond call/xfer is likely */
    unalign_ld, 	/* Unaligned load operator */
    unalign_store,	/* Unaligned store operator */
    cond_move, 		/* conditional move */
    uniq_res, 		/* Result must not be opnd */
    same_res,		/* Result must be same as opnd */
    noop, 		/* No-op operator */
    select, 		/* Operator is a machine level select */
    dummy, 		/* No-op doesn't get emitted */
    iadd, 		/* Integer add operator */
    isub, 		/* Integer subtract operator */
    imul, 		/* Integer multiply operator */
    idiv,		/* Integer divide operator */
    flop, 		/* Any proper floating point op */
    fadd, 		/* FP add operator */
    fsub,		/* FP subtract operator */
    fmul, 		/* FP multiply operator */
    fmisc,              /* FP miscellaneous class type */
    madd,		/* The kind that do two at once */
    mmalu,              /* Multimedia ALU operator */
    mmmul,              /* Multimedia multiply operator */
    mmshf,              /* Multimedia shift operator */
    itrap,		/* Integer trap potential */
    safe,		/* Never traps -- always safe */
    ftrap,		/* Floating point trap potential */
    fdiv,		/* Floating point divides */
    sqrt,		/* Square root operator */
    memtrap,		/* Memory trap potential */
    unsafe,		/* Unsafe always */
    defs_fp,		/* Operator defines FP reg */
    defs_fcc,		/* Operator defines FP CC reg */
    defs_fcr,		/* Operator defines FCR reg */
    refs_fcr,		/* Operator uses FCR reg */
    defs_fpu_int,	/* Operator defs int val in FP reg */
    ior,		/* Integer logical OR operator */
    jump,		/* Jump operator */
    ijump,		/* Indirect jump operator */
    ixor,		/* Integer logical exclusive OR operator */
    iand,		/* Integer logical AND operator */
    icmp,		/* Integer compare operator */
    f_group,            /* Instruction must be first in an instruction group */
    l_group,            /* Instruction must be last in an instruction group */
    privileged,         /* Instruction is a privileged instruction */
    simulated,		/* Instruction is simulated, i.e. a macro */    
    predicated,		/* Instruction is predicated */
    access_reg_bank,	/* Instruction accesses rotating register banks */
    side_effects,	/* Instruction has side effects */
    mem_fill_type,      /* Memory instructions which are fill/spill type */
    branch_predict,	/* Branch prediction (but not actual xfer) */
    var_opnds,		/* Variable number of operands AND/OR results */
    two_byte_inst,      /* 2 bytes instructions */
    base_update,	/* Instruction updates 'base' operand */
    loop_start;         /* Instruction marks the start of a zero-cost loop */

  ISA_Properties_Begin ("ia64");

/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */
/* ===== Move operator ====== */
  load = ISA_Property_Create ("mov");
  Instruction_Group (load, 
		        TOP_movgp,
		        TOP_movsp,
		        TOP_movlc,
		        TOP_movra,
		        TOP_mov2gp,
		        TOP_mov2sp,
		        TOP_mov2lc,
		        TOP_mov2ra,
		        TOP_movi2gp,
		        TOP_movi2sp,
		        TOP_movi2lc,
		        TOP_movi2ra,
			TOP_UNDEFINED);


/* ===== Memory load operator ====== */
  load = ISA_Property_Create ("load");
  Instruction_Group (load, 
		        TOP_ldb2,
		        TOP_ldh2,
		        TOP_ldw2,
		        TOP_ldb,
		        TOP_ldh,
		        TOP_ldw,
		        TOP_ldbi,
		        TOP_ldhi,
		        TOP_ldwi,
		        TOP_ldwgp2,
		        TOP_ldwgp3,
		        TOP_ldbsp2,
		        TOP_ldhsp2,
		        TOP_ldwsp2,
		        TOP_ldbsp3,
		        TOP_ldhsp3,
		        TOP_ldwsp3,
			TOP_UNDEFINED);

/* ===== Memory store operator ====== */
  store = ISA_Property_Create ("store");
  Instruction_Group (store, 
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
		        TOP_stwgp3,
		        TOP_stbsp2,
		        TOP_sthsp2,
		        TOP_stwsp2,
		        TOP_stbsp3,
		        TOP_sthsp3,
		        TOP_stwsp3,
			TOP_UNDEFINED);

/* ===== Prefetch operator ====== */
  prefetch = ISA_Property_Create ("prefetch");
  Instruction_Group (prefetch, 
			TOP_UNDEFINED);

/* ===== Memory fill/spill type instructions ====== */
  mem_fill_type = ISA_Property_Create ("mem_fill_type");
  Instruction_Group (mem_fill_type, 
		     TOP_UNDEFINED);

/* ===== Control transfer operator ====== */
  xfer = ISA_Property_Create ("xfer");
  Instruction_Group (xfer,  
			TOP_b,
			TOP_b10,
			TOP_bcall,
			TOP_bcall10,
			TOP_beq,
			TOP_beqpf,
			TOP_beqpt,
			TOP_bgt,
			TOP_bgtpf,
			TOP_bgtpt,
			TOP_bge,
			TOP_bgepf,
			TOP_bgept,
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
			TOP_bloop,
			TOP_bret,
			TOP_j,
			TOP_jr,
			TOP_UNDEFINED);

/* ===== Subprogram call operator ====== */
  call = ISA_Property_Create ("call");
  Instruction_Group (call,  
			TOP_bcall,
			TOP_bcall10,
			TOP_call,
			TOP_icall,
			TOP_UNDEFINED);

/* ===== Call/xfer is conditional ====== */
  cond = ISA_Property_Create ("cond");
  Instruction_Group (cond,  
			TOP_beq,
			TOP_beqpf,
			TOP_beqpt,
			TOP_bgt,
			TOP_bgtpf,
			TOP_bgtpt,
			TOP_bge,
			TOP_bgepf,
			TOP_bgept,
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
			TOP_bloop,
			TOP_UNDEFINED);

/* ===== Cond call/xfer is likely ====== */
  likely = ISA_Property_Create ("likely");
  Instruction_Group (likely, 
			TOP_UNDEFINED);

/* ===== Result def is conditional ====== */
  cond_move = ISA_Property_Create ("cond_move");
  Instruction_Group (cond_move,
                        TOP_tgem2,
                        TOP_tgemi2,
                        TOP_tgtm2,
                        TOP_tgtmi2,
                        TOP_tnem2,
                        TOP_tlem3,
                        TOP_tlemi3,
                        TOP_tltm3,
                        TOP_tltmi3,
                        TOP_tgem3,
                        TOP_tgemi3,
                        TOP_tgtm3,
                        TOP_tgtmi3,
                        TOP_tnem3,
                        TOP_tnemi3,
			TOP_UNDEFINED);

/* ===== Result must not be opnd ====== */
  uniq_res = ISA_Property_Create ("uniq_res");
  Instruction_Group (uniq_res,
			TOP_intrncall,
			TOP_UNDEFINED);

/* ===== Result must be same as opnd ====== */
  same_res = ISA_Property_Create ("same_res");
  Instruction_Group (same_res,
                        TOP_tgem2,
                        TOP_tgtm2,
                        TOP_tnem2,
			TOP_UNDEFINED);

/* ===== Operator is a machine level select ====== */
  select = ISA_Property_Create ("select");
  Instruction_Group (select,
			TOP_UNDEFINED);

/* ===== Unaligned load operator ====== */
  unalign_ld = ISA_Property_Create ("unalign_ld");
  Instruction_Group (unalign_ld,
			TOP_UNDEFINED);

/* ===== Unaligned store operator ====== */
  unalign_store = ISA_Property_Create ("unalign_store");
  Instruction_Group (unalign_store,
			TOP_UNDEFINED);

/* ===== Integer add operator ====== */
  iadd = ISA_Property_Create ("iadd");
  Instruction_Group (iadd,
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
			TOP_spadjust,
			TOP_UNDEFINED);

/* ===== Integer subtract operator ====== */
  isub = ISA_Property_Create ("isub");
  Instruction_Group (isub,
			TOP_sub2,
			TOP_subi2,
			TOP_subiu2,
			TOP_subu2,
			TOP_sub3,
			TOP_subi3,
			TOP_subiu3,
			TOP_subu3,
			TOP_UNDEFINED);

/* ===== Integer multiply operator ====== */
  imul = ISA_Property_Create ("imul");
  Instruction_Group (imul,
			TOP_mul2,
			TOP_muli2,
			TOP_mulu2,
			TOP_muliu2,
			TOP_mul3,
			TOP_muli3,
			TOP_mulu3,
			TOP_muliu3,
			TOP_UNDEFINED);

/* ===== Integer divide operator ====== */
  idiv = ISA_Property_Create ("idiv");
  Instruction_Group (idiv,
			TOP_UNDEFINED);

/* ===== Any proper floating point op ====== */
  flop = ISA_Property_Create ("flop");
  Instruction_Group (flop,
			TOP_UNDEFINED);

/* ===== FP add operator ====== */
  fadd = ISA_Property_Create ("fadd");
  Instruction_Group (fadd,
			TOP_UNDEFINED);

/* ===== FP subtract operator ====== */
  fsub = ISA_Property_Create ("fsub");
  Instruction_Group (fsub,
			TOP_UNDEFINED);

/* ===== FP multiply operator ====== */
  fmul = ISA_Property_Create ("fmul");
  Instruction_Group (fmul,
			TOP_UNDEFINED);

/* ===== FP miscellaneous operator ====== */
  fmisc = ISA_Property_Create ("fmisc");
  Instruction_Group (fmisc,
		     TOP_UNDEFINED);

/* ===== The kind that do two at once ====== */
  madd = ISA_Property_Create ("madd");
  Instruction_Group (madd,
			TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia ALU type ====== */
  mmalu = ISA_Property_Create ("mmalu");
  Instruction_Group (mmalu,
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia shift (MMSHF) type ====== */
  mmshf = ISA_Property_Create ("mmshf");
  Instruction_Group (mmshf,
		        TOP_shl2,
		        TOP_shli2,
		        TOP_shl3,
		        TOP_shli3,
		        TOP_shr2,
		        TOP_shri2,
		        TOP_shr3,
		        TOP_shri3,
		        TOP_dep,
		        TOP_depi,
		        TOP_extr,
		        TOP_extri,
		        TOP_pack16,
		        TOP_pack32,
		        TOP_packu16,
		        TOP_packu32,
		        TOP_upack16,
		        TOP_upack32,
		        TOP_upacku16,
		        TOP_upacku32,
		        TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia multiply (MMMUL) type ====== */
  mmmul = ISA_Property_Create ("mmmul");
  Instruction_Group (mmmul,
		     TOP_UNDEFINED);

  noop = ISA_Property_Create ("noop");
  Instruction_Group (noop,
			TOP_nop2,
			TOP_nop3,
			TOP_noop,
			TOP_UNDEFINED);

  dummy = ISA_Property_Create ("dummy");
  Instruction_Group (dummy,
			TOP_begin_pregtn,
			TOP_end_pregtn,
			TOP_fwd_bar,
			TOP_bwd_bar,
			TOP_label,
			TOP_noop,
			TOP_UNDEFINED);

/* ====================================================================
 *              Exception classification descriptors
 * ====================================================================
 */

  /* ===== Integer trap potential ====== */
  itrap = ISA_Property_Create ("itrap");
  Instruction_Group (itrap,
			TOP_UNDEFINED);

  /* ===== Never traps -- always safe ====== */
  safe = ISA_Property_Create ("safe");
  Instruction_Group (safe,
			// TODO
			TOP_UNDEFINED);

  /* ===== Unsafe always ====== */
  unsafe = ISA_Property_Create ("unsafe");
  Instruction_Group (unsafe, 
			// TODO
			TOP_fwd_bar, TOP_bwd_bar,
			TOP_UNDEFINED);

  /* ===== Floating point trap potential ====== */
  ftrap = ISA_Property_Create ("ftrap");
  Instruction_Group (ftrap, 
			// TODO
			TOP_UNDEFINED);

  /* ===== Floating point divides ====== */
  fdiv = ISA_Property_Create ("fdiv");
  Instruction_Group (fdiv, 
			TOP_UNDEFINED);

  /* ===== Square roots ====== */
  sqrt = ISA_Property_Create ("sqrt");
  Instruction_Group (sqrt, 
			TOP_UNDEFINED);

  /* ===== Memory trap potential ====== */
  memtrap = ISA_Property_Create ("memtrap");
  Instruction_Group (memtrap, 
		        TOP_ldb2,
		        TOP_ldh2,
		        TOP_ldw2,
		        TOP_ldb,
		        TOP_ldh,
		        TOP_ldw,
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
		        TOP_stb2,
		        TOP_sth2,
		        TOP_stw2,
		        TOP_stb,
		        TOP_sth,
		        TOP_stw,
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
			TOP_UNDEFINED);

  /* ===== Instruction must be first in an instruction group ====== */
  f_group = ISA_Property_Create ("f_group");
  Instruction_Group (f_group,
		     TOP_UNDEFINED);

  /* ===== Instruction must be last in an instruction group ====== */
  l_group = ISA_Property_Create ("l_group");
  Instruction_Group (l_group,
		     TOP_UNDEFINED);

  /* ===== Instruction is a privileged instruction ====== */
  privileged = ISA_Property_Create ("privileged");
  Instruction_Group (privileged,
		        TOP_sys_brk,
		        TOP_sys_swi,
		        TOP_sys_reti,
		        TOP_sys_pwr,
		        TOP_UNDEFINED);

/* ====================================================================
 * Other operator descriptors (mostly for global optimization). 
 * TODO: These descriptors should actually be determined from mips_operands. 
 * ====================================================================
 */

/* ===== Operator defines FP CC reg ====== */
  defs_fcc = ISA_Property_Create ("defs_fcc");
  Instruction_Group (defs_fcc,
			TOP_UNDEFINED);

/* ===== Operator defines FCR reg ====== */
  defs_fcr = ISA_Property_Create ("defs_fcr");
  Instruction_Group (defs_fcr,
			// TODO
			TOP_UNDEFINED);

/* ===== Operator uses FCR reg ====== */
  refs_fcr = ISA_Property_Create ("refs_fcr");
  Instruction_Group (refs_fcr, 
			// TODO
			TOP_UNDEFINED);

/* ===== Operator defs int val in FP reg ====== */
  defs_fpu_int = ISA_Property_Create ("defs_fpu_int");
  Instruction_Group (defs_fpu_int, 
			TOP_UNDEFINED);

/* ===== Operator defines FP reg ====== */
  defs_fp = ISA_Property_Create ("defs_fp");
  Instruction_Group (defs_fp, 
			// TODO
			TOP_UNDEFINED);

/* ===== Logical OR operator ====== */
  ior = ISA_Property_Create ("ior");
  Instruction_Group (ior,
			TOP_or2,
			TOP_ori2,
			TOP_or3,
			TOP_ori3,
			TOP_UNDEFINED);

/* ===== Jump operator ====== */
  jump = ISA_Property_Create ("jump");
  Instruction_Group (jump,
		        TOP_call,
			TOP_UNDEFINED);

/* ===== Indirect jump operator ====== */
  ijump = ISA_Property_Create ("ijump");
  Instruction_Group (ijump,
			TOP_ret,
			TOP_UNDEFINED);

/* ===== Logical exclusive OR operator ====== */
  ixor = ISA_Property_Create ("ixor");
  Instruction_Group (ixor,
			TOP_xor2,
			TOP_xori2,
			TOP_xor3,
			TOP_xori3,
			TOP_UNDEFINED);

/* ===== Logical AND operator ====== */
  iand = ISA_Property_Create ("iand");
  Instruction_Group (iand,
			TOP_and2,
			TOP_andi2,
			TOP_and3,
			TOP_andi3,
			TOP_UNDEFINED);

/* ===== Integer compare operator ====== */
  icmp = ISA_Property_Create ("icmp");
  Instruction_Group (icmp,
			TOP_teq2,
			TOP_teqi2,
			TOP_tge2,
			TOP_tgei2,
			TOP_tgem2,
			TOP_tgt2,
			TOP_tgti2,
			TOP_tgtm2,
			TOP_tne2,
			TOP_tnei2,
			TOP_tnem2,
			TOP_teq3,
			TOP_teqi3,
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
			TOP_nop2,
			TOP_nop3,
			TOP_UNDEFINED);

/* ===== Simulated instructions ====== */
  simulated = ISA_Property_Create ("simulated");
  Instruction_Group (simulated,
		        TOP_asm,
		        TOP_intrncall,
		        TOP_spadjust,
		        TOP_UNDEFINED);

/* ===== Predicated instructions ====== */
  predicated = ISA_Property_Create ("predicated");
  Instruction_Group (predicated,
			TOP_UNDEFINED);

/* ===== Instructions access rotating register banks ====== */
  access_reg_bank = ISA_Property_Create ("access_reg_bank");
  Instruction_Group (access_reg_bank,
		     TOP_UNDEFINED);

/* ===== Instructions with side effects ====== */
  side_effects = ISA_Property_Create ("side_effects");
  Instruction_Group (side_effects,
		     TOP_UNDEFINED);

/* ===== Instructions that predict branches ====== */
  branch_predict = ISA_Property_Create ("branch_predict");
  Instruction_Group (branch_predict,
		     TOP_UNDEFINED);

/* ===== Instructions with variable number of operands/results ====== */
  var_opnds = ISA_Property_Create ("var_opnds");
  Instruction_Group (var_opnds,
		     TOP_asm,
		     TOP_intrncall,
		     TOP_UNDEFINED);

/* ===== Instructions that update 'base' operand ====== */
  base_update = ISA_Property_Create ("base_update");
  Instruction_Group (base_update,
		     TOP_ldbi,
		     TOP_ldhi,
		     TOP_ldwi,
		     TOP_stbi,
		     TOP_sthi,
		     TOP_stwi,
		     TOP_UNDEFINED);

/* ===== Instructions that are 2 bytes sizes ====== */
  two_byte_inst = ISA_Property_Create ("2bytes");
  Instruction_Group (two_byte_inst,
		     TOP_add2,
		     TOP_addi2,
		     TOP_addiu2,
		     TOP_addu2,
		     TOP_and2,
		     TOP_andi2,
		     TOP_b10,
		     TOP_bcall10,
		     TOP_bloop,
		     TOP_bloopsz,
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
		     TOP_div2,
		     TOP_divi2,
		     TOP_diviu2,
		     TOP_divu2,
		     TOP_ret,
		     TOP_ldb2,
		     TOP_ldh2,
		     TOP_ldw2,
		     TOP_ldwgp2,
		     TOP_ldbsp2,
		     TOP_ldhsp2,
		     TOP_ldwsp2,
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
		     TOP_not2,
		     TOP_noti2,
		     TOP_nop2,
		     TOP_or2,
		     TOP_ori2,
		     TOP_shl2,
		     TOP_shli2,
		     TOP_shr2,
		     TOP_shri2,
		     TOP_shladgp,
		     TOP_shladsp,
		     TOP_stb2,
		     TOP_sth2,
		     TOP_stw2,
		     TOP_stwgp2,
		     TOP_stbsp2,
		     TOP_sthsp2,
		     TOP_stwsp2,
		     TOP_sub2,
		     TOP_subi2,
		     TOP_subiu2,
		     TOP_subu2,
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
		     TOP_xor2,
		     TOP_xori2,
		     TOP_UNDEFINED);

  ISA_Properties_End();
  return 0;
}
