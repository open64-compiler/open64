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
    var_opnds, 		/* Variable number of operands AND/OR results */
    nor,
    macro,
    unsigned_ext,  	/* Operations with unsigned extension */
    load_ext;    	/* Load with sign/zero extension */

  ISA_Properties_Begin ("loongson");

/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */

/* ===== Memory load operator ====== */
  load = ISA_Property_Create ("load");
  Instruction_Group (load, 
			TOP_lb,
			TOP_lbu,
			TOP_ld,
			TOP_ldl,
			TOP_ldr,
			TOP_lh,
			TOP_lhu,
			TOP_ll,
			TOP_lld,
			TOP_lw,
			TOP_lwl,
			TOP_lwr,
			TOP_lwu,
			TOP_ldc1,
			TOP_lwc1,
			TOP_UNDEFINED);

/* ===== Memory store operator ====== */
  store = ISA_Property_Create ("store");
  Instruction_Group (store, 
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
			TOP_sdc1,
			TOP_swc1,
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
			TOP_beq,
			TOP_bgez,
			TOP_bgezal,
			TOP_bgtz,
			TOP_blez,
			TOP_bltz,
			TOP_bltzal,
			TOP_bne,
			TOP_j,
			TOP_jal,
			TOP_jalr,
			TOP_jr,
			TOP_bc1f,
			TOP_bc1fl,
			TOP_bc1t,
			TOP_bc1tl,
			TOP_UNDEFINED);

/* ===== Subprogram call operator ====== */
  call = ISA_Property_Create ("call");
  Instruction_Group (call,  
			TOP_jal,
			TOP_jalr,
			TOP_UNDEFINED);

/* ===== Call/xfer is conditional ====== */
  cond = ISA_Property_Create ("cond");
  Instruction_Group (cond,  
			TOP_beq,
			TOP_bgez,
			TOP_bgezal,
			TOP_bgtz,
			TOP_blez,
			TOP_bltz,
			TOP_bltzal,
			TOP_bne,
			TOP_bc1f,
			TOP_bc1fl,
			TOP_bc1t,
			TOP_bc1tl,
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

/* ===== Cond call/xfer is likely ====== */
  likely = ISA_Property_Create ("likely");
  Instruction_Group (likely, 
			TOP_UNDEFINED);



/* ===== Result must not be opnd ====== */
  uniq_res = ISA_Property_Create ("uniq_res");
  Instruction_Group (uniq_res,
  			TOP_intrncall,
			TOP_UNDEFINED);

/* ===== Result must be same as opnd ====== */
  same_res = ISA_Property_Create ("same_res");
  Instruction_Group (same_res,
			TOP_madd_d,
			TOP_madd_s,
			TOP_msub_d,
			TOP_msub_s,
			TOP_nmadd_d,
			TOP_nmadd_s,
			TOP_nmsub_d,
			TOP_nmsub_s,
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
  			TOP_spadjust,
			TOP_add,
			TOP_addi,
			TOP_addiu,
			TOP_addu,
			TOP_dadd,
			TOP_daddi,
			TOP_daddiu,
			TOP_daddu,
			TOP_UNDEFINED);

/* ===== Integer subtract operator ====== */
  isub = ISA_Property_Create ("isub");
  Instruction_Group (isub,
			TOP_dsub,
			TOP_dsubu,
			TOP_sub,
			TOP_subu,
			TOP_UNDEFINED);

/* ===== Integer multiply operator ====== */
  imul = ISA_Property_Create ("imul");
  Instruction_Group (imul,
			TOP_dmult,
			TOP_dmultu,
			TOP_mult,
			TOP_multu,
			TOP_mult_g,
			TOP_multu_g,
			TOP_dmult_g,
			TOP_dmultu_g,
			TOP_UNDEFINED);



/* ===== Any proper floating point op ====== */
  flop = ISA_Property_Create ("flop");
  Instruction_Group (flop,
		       TOP_abs_s,
			TOP_abs_d,
			TOP_add_s,
			TOP_add_d,
			TOP_bc1f,
			TOP_bc1fl,
			TOP_bc1t,
			TOP_bc1tl,
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
			TOP_div_s,
			TOP_div_d,
			TOP_mov_s,
			TOP_mov_d,
			TOP_mul_s,
			TOP_mul_d,
			TOP_neg_s,
			TOP_neg_d,
			TOP_sub_s,
			TOP_sub_d,
			TOP_madd_d,
			TOP_madd_s,
			TOP_msub_d,
			TOP_msub_s,
			TOP_nmadd_d,
			TOP_nmadd_s,
			TOP_nmsub_d,
			TOP_nmsub_s,
			TOP_UNDEFINED);

/* ===== FP add operator ====== */
  fadd = ISA_Property_Create ("fadd");
  Instruction_Group (fadd,
			TOP_add_s,
			TOP_add_d,
			TOP_UNDEFINED);

/* ===== FP subtract operator ====== */
  fsub = ISA_Property_Create ("fsub");
  Instruction_Group (fsub,
			TOP_sub_s,
			TOP_sub_d,
			TOP_UNDEFINED);

/* ===== FP multiply operator ====== */
  fmul = ISA_Property_Create ("fmul");
  Instruction_Group (fmul,
			TOP_mul_s,
			TOP_mul_d,
			TOP_UNDEFINED);

/* ===== FP miscellaneous operator ====== */
  fmisc = ISA_Property_Create ("fmisc");
  Instruction_Group (fmisc,
		     TOP_abs_s,
		     TOP_abs_d,
		     TOP_neg_s,
		     TOP_neg_d,
		     TOP_UNDEFINED);

/* ===== The kind that do two at once ====== */
  madd = ISA_Property_Create ("madd");
  Instruction_Group (madd,
			TOP_madd_d,
			TOP_madd_s,
			TOP_msub_d,
			TOP_msub_s,
			TOP_nmadd_d,
			TOP_nmadd_s,
			TOP_nmsub_d,
			TOP_nmsub_s,
			TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia ALU type ====== */
  mmalu = ISA_Property_Create ("mmalu");
  Instruction_Group (mmalu,
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia shift (MMSHF) type ====== */
  mmshf = ISA_Property_Create ("mmshf");
  Instruction_Group (mmshf,
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia multiply (MMMUL) type ====== */
  mmmul = ISA_Property_Create ("mmmul");
  Instruction_Group (mmmul,
		     TOP_UNDEFINED);

  noop = ISA_Property_Create ("noop");
  Instruction_Group (noop,
			TOP_noop,
			TOP_nop,
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



/* ===== Never traps -- always safe ====== */
  safe = ISA_Property_Create ("safe");
  Instruction_Group (safe,
			TOP_UNDEFINED);

/* ===== Unsafe always ====== */
  unsafe = ISA_Property_Create ("unsafe");
  Instruction_Group (unsafe, 
  			TOP_fwd_bar, TOP_bwd_bar,
			TOP_UNDEFINED);

/* ===== Floating point trap potential ====== */
  ftrap = ISA_Property_Create ("ftrap");
  Instruction_Group (ftrap, 
			TOP_UNDEFINED);

/* ===== Floating point divides ====== */
  fdiv = ISA_Property_Create ("fdiv");
  Instruction_Group (fdiv, 
			TOP_div_s,
			TOP_div_d,
			TOP_UNDEFINED);

/* ===== Square roots ====== */
  sqrt = ISA_Property_Create ("sqrt");
  Instruction_Group (sqrt, 
			TOP_sqrt_s,
			TOP_sqrt_d,
			TOP_UNDEFINED);

/* ===== Memory trap potential ====== */
  memtrap = ISA_Property_Create ("memtrap");
  Instruction_Group (memtrap, 
			TOP_lb,
			TOP_lbu,
			TOP_ld,
			TOP_ldl,
			TOP_ldr,
			TOP_lh,
			TOP_lhu,
			TOP_ll,
			TOP_lld,
			TOP_lw,
			TOP_lwl,
			TOP_lwr,
			TOP_lwu,
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
			TOP_ldc1,
			TOP_lwc1,
			TOP_sdc1,
			TOP_swc1,
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
			TOP_UNDEFINED);

/* ===== Operator uses FCR reg ====== */
  refs_fcr = ISA_Property_Create ("refs_fcr");
  Instruction_Group (refs_fcr, 
			TOP_UNDEFINED);

/* ===== Operator defs int val in FP reg ====== */
  defs_fpu_int = ISA_Property_Create ("defs_fpu_int");
  Instruction_Group (defs_fpu_int, 
			TOP_UNDEFINED);

/* ===== Operator defines FP reg ====== */
  defs_fp = ISA_Property_Create ("defs_fp");
  Instruction_Group (defs_fp, 
			TOP_UNDEFINED);

/* ===== Logical OR operator ====== */
  ior = ISA_Property_Create ("ior");
  Instruction_Group (ior,
			TOP_or,
			TOP_ori,
			TOP_UNDEFINED);

/* ===== Jump operator ====== */
  jump = ISA_Property_Create ("jump");
  Instruction_Group (jump,
			TOP_UNDEFINED);

/* ===== Indirect jump operator ====== */
  ijump = ISA_Property_Create ("ijump");
  Instruction_Group (ijump,
			TOP_jalr,
			TOP_jr,
			TOP_UNDEFINED);

/* ===== Logical exclusive OR operator ====== */
  ixor = ISA_Property_Create ("ixor");
  Instruction_Group (ixor,
			TOP_xor,
			TOP_xori,
			TOP_UNDEFINED);

/* ===== Logical AND operator ====== */
  iand = ISA_Property_Create ("iand");
  Instruction_Group (iand,
			TOP_and,
			TOP_andi,
			TOP_UNDEFINED);

/* ===== Integer compare operator ====== */
  icmp = ISA_Property_Create ("icmp");
  Instruction_Group (icmp,
			TOP_slt,
			TOP_slti,
			TOP_sltiu,
			TOP_sltu,
			TOP_UNDEFINED);

/* ===== Simulated instructions ====== */
  simulated = ISA_Property_Create ("simulated");
  Instruction_Group (simulated,
  			TOP_intrncall,
			TOP_asm,
			TOP_spadjust,
			TOP_UNDEFINED);

/* ===== Predicated instructions ====== */
  predicated = ISA_Property_Create ("predicated");
  Instruction_Group (predicated,
 			TOP_spadjust,
  
			TOP_add,
			TOP_addi,
			TOP_addiu,
			TOP_addu,
			TOP_and,
			TOP_andi,
			TOP_beq,
			TOP_bgez,
			TOP_bgezal,
			TOP_bgtz,
			TOP_blez,
			TOP_bltz,
			TOP_bltzal,
			TOP_bne,
			TOP_dadd,
			TOP_daddi,
			TOP_daddiu,
			TOP_daddu,
			TOP_ddiv,
  			TOP_ddivu,
  			TOP_div,
  			TOP_divu,
			TOP_dmult,
			TOP_dmultu,
			TOP_dsll,
			TOP_dsll32,
			TOP_dsllv,
			TOP_dsra,
			TOP_dsra32,
			TOP_dsrav,
			TOP_dsrl,
			TOP_dsrl32,
			TOP_dsrlv,
			TOP_dsub,
			TOP_dsubu,
			TOP_j,
			TOP_jal,
			TOP_jalr,
			TOP_jr,
			TOP_lb,
			TOP_lbu,
			TOP_ld,
			TOP_ldl,
			TOP_ldr,
			TOP_lh,
			TOP_lhu,
			TOP_ll,
			TOP_lld,
			TOP_lui,
			TOP_lw,
			TOP_lwl,
			TOP_lwr,
			TOP_lwu,
			TOP_mfhi,
			TOP_mflo,
			TOP_mthi,
			TOP_mtlo,
			TOP_mult,
			TOP_multu,
			TOP_nor,
			TOP_or,
			TOP_ori,
			TOP_sub,
			TOP_subu,
			TOP_sb,
			TOP_sc,
			TOP_scd,
			TOP_sd,
			TOP_sdl,
			TOP_sdr,
			TOP_sh,
			TOP_sll,
			TOP_sllv,
			TOP_slt,
			TOP_slti,
			TOP_sltiu,
			TOP_sltu,
			TOP_sra,
			TOP_srav,
			TOP_srl,
			TOP_srlv,
			TOP_sw,
			TOP_swl,
			TOP_swr,
			TOP_xor,
			TOP_xori,
			TOP_abs_s,
			TOP_abs_d,
			TOP_add_s,
			TOP_add_d,
			TOP_bc1f,
			TOP_bc1fl,
			TOP_bc1t,
			TOP_bc1tl,
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
			TOP_ceil_l_s,
			TOP_ceil_l_d,
			TOP_ceil_w_s,
			TOP_ceil_w_d,
			TOP_cfc1,
			TOP_ctc1,
			TOP_cvt_d_s,
			TOP_cvt_d_w,
			TOP_cvt_d_l,
			TOP_cvt_l_s,
			TOP_cvt_l_d,
			TOP_cvt_s_d,
			TOP_cvt_s_w,
			TOP_cvt_s_l,
			TOP_cvt_w_s,
			TOP_cvt_w_d,
			TOP_div_s,
			TOP_div_d,
			TOP_dmfc1,
			TOP_dmtc1,
			TOP_floor_l_s,
			TOP_floor_l_d,
			TOP_floor_w_s,
			TOP_floor_w_d,
			TOP_ldc1,
			TOP_lwc1,
			TOP_mfc1,
			TOP_mov_s,
			TOP_mov_d,
			TOP_mtc1,
			TOP_mul_s,
			TOP_mul_d,
			TOP_neg_s,
			TOP_neg_d,
			TOP_round_l_s,
			TOP_round_l_d,
			TOP_round_w_s,
			TOP_round_w_d,
			TOP_sdc1,
			TOP_sub_s,
			TOP_sub_d,
			TOP_swc1,
			TOP_trunc_l_s,
			TOP_trunc_l_d,
			TOP_trunc_w_s,
			TOP_trunc_w_d,
			TOP_sqrt_s,
			TOP_sqrt_d,
			TOP_teq,
			TOP_teqi,
			TOP_tge,
			TOP_tgei,
			TOP_tgeiu,
			TOP_tgeu,
			TOP_break,
			TOP_nop,
			TOP_madd_d,
			TOP_madd_s,
			TOP_msub_d,
			TOP_msub_s,
			TOP_nmadd_d,
			TOP_nmadd_s,
			TOP_nmsub_d,
			TOP_nmsub_s,
			TOP_movz,
			TOP_movn,
			TOP_divlo,
			TOP_divhi,
			TOP_divulo,
			TOP_divuhi,
			TOP_mult_g,
			TOP_multu_g,
			TOP_dmult_g,
			TOP_dmultu_g,
			TOP_div_g,
			TOP_divu_g,
			TOP_ddiv_g,
			TOP_ddivu_g,
			TOP_mod_g,
			TOP_modu_g,
			TOP_dmod_g,
			TOP_dmodu_g,
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

/* ===== Integer trap potential ====== */
  itrap = ISA_Property_Create ("itrap");
  Instruction_Group (itrap,
 			TOP_teq,
			TOP_teqi,
			TOP_tge,
			TOP_tgei,
			TOP_tgeiu,
			TOP_tgeu,
			TOP_break,
			TOP_UNDEFINED);
  
/* ===== Integer divide operator ====== */
  idiv = ISA_Property_Create ("idiv");
  Instruction_Group (idiv,
  			TOP_ddiv,
  			TOP_ddivu,
  			TOP_div,
  			TOP_divu,
  			TOP_divlo,
			TOP_divhi,
			TOP_divulo,
			TOP_divuhi,
			TOP_div_g,
			TOP_divu_g,
			TOP_ddiv_g,
			TOP_ddivu_g,
			TOP_mod_g,
			TOP_modu_g,
			TOP_dmod_g,
			TOP_dmodu_g,
			TOP_UNDEFINED);

    
/* =====nor  mips====== */
  nor = ISA_Property_Create ("nor");
  Instruction_Group (nor,
  		     TOP_nor,
  		     TOP_UNDEFINED);

  
/* ===== Result def is conditional ====== */
  cond_move = ISA_Property_Create ("cond_move");
  Instruction_Group (cond_move,
  			TOP_movz,
			TOP_movn,
			TOP_UNDEFINED);
/* ===== unsigned extension ====== */
  unsigned_ext = ISA_Property_Create ("unsigned_ext");
  Instruction_Group (unsigned_ext,
                       TOP_lbu,
                       TOP_lhu,
                       TOP_lwu,
                       TOP_addiu,
                       TOP_addu,
                       TOP_daddiu,
                       TOP_daddu,
                       TOP_ddivu,
                       TOP_divu,
                       TOP_dmultu,
                       TOP_subu,
                       TOP_multu,
                       TOP_sltu,
                       TOP_sltiu,
                       TOP_subu,
                       TOP_UNDEFINED);
/* ======= load with sign/zero extension=========== */
  load_ext = ISA_Property_Create("load_ext");
  Instruction_Group(load_ext,
  			TOP_lb,
  			TOP_lbu,
  			TOP_lh,
  			TOP_lhu,
  			TOP_ll,
  			TOP_lui,
  			TOP_lw,
  			TOP_lwu,
  			TOP_UNDEFINED);

  ISA_Properties_End();
  return 0;
}
