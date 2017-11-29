/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

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
//  $Revision: 1.46 $
//  $Date: 2001/03/10 01:16:51 $
//  $Author: mtibuild $
//  $Source: /osprey.src/osprey1.0/common/targ_info/isa/ia64/RCS/isa_properties.cxx,v $


#include <stddef.h>
#include "topcode.h"
#include "isa_properties_gen.h"

main()
{
  ISA_PROPERTY 
    load,   		/* Memory load operator */
    store, 		/* Memory store operator */
    memory_8bit,	/* Load/Store of 8 bits */
    memory_16bit,	/* Load/Store of 16 bits */
    memory_32bit,	/* Load/Store of 32 bits */
    memory_64bit,	/* Load/Store of 64 bits */
    fp_loadstore,	/* load/store of fp */
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
    convertible_to16_if_in16,  /* instructions can be safely converted to 16bit
			        * if all inputs are 16bit */
    convertible_to16_if_out16, /* instructions can be safely converted to 16bit
			        * if all outputs are 16bit */
    compare_32,		/* 32-bit integer compare instructions */
    cvt_8to32,		/* convert 8bit to 32bit */
    cvt_16to32,		/* convert 16bit to 32bit */
    cvt_32to8,		/* convert 32bit to 8bit */
    cvt_32to8in32,	/* convert 32bit to 8bit but put in 32bit register */
    cvt_32to16,		/* convert 32bit to 16bit */
    cvt_32to16in32,	/* convert 32bit to 16bit but put in 32bit register */
    load_8in32,		/* load 8 bits in 32bit reg */
    load_16in32,	/* load 16 bits in 32bit reg */
    store_8from32,	/* store 8 bits from 32bit reg */
    store_16from32,	/* store 16 bits from 32bit reg */
    mul_lo_32, 		/* 32-bit multiplication with 32-bit result */
    v2_load,	/* vector load */
    v4_load,	/* vector load */
    v2_store,	/* vector store */
    v4_store,	/* vector store */
    var_opnds;		/* Variable number of operands AND/OR results */

  ISA_Properties_Begin ("nvisa");

/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */

/* ===== Memory load operator ====== */
  load = ISA_Property_Create ("load");
  Instruction_Group (load, 
        TOP_ld_qualifier_space_s8, TOP_ld_qualifier_space_s16, 
	TOP_ld_qualifier_space_s32, TOP_ld_qualifier_space_s64,
        TOP_ld_qualifier_space_u8, TOP_ld_qualifier_space_u16, 
	TOP_ld_qualifier_space_u32, TOP_ld_qualifier_space_u64,
        TOP_ld_qualifier_space_f32, TOP_ld_qualifier_space_f64,
        TOP_ld_qualifier_space_s8_b32, TOP_ld_qualifier_space_s16_b32,
        TOP_ld_qualifier_space_u8_b32, TOP_ld_qualifier_space_u16_b32,
        TOP_ld_qualifier_space_s8_o, TOP_ld_qualifier_space_s16_o, 
	TOP_ld_qualifier_space_s32_o, TOP_ld_qualifier_space_s64_o,
        TOP_ld_qualifier_space_u8_o, TOP_ld_qualifier_space_u16_o, 
	TOP_ld_qualifier_space_u32_o, TOP_ld_qualifier_space_u64_o,
        TOP_ld_qualifier_space_f32_o, TOP_ld_qualifier_space_f64_o,
        TOP_ld_qualifier_space_s8_b32_o, TOP_ld_qualifier_space_s16_b32_o,
        TOP_ld_qualifier_space_u8_b32_o, TOP_ld_qualifier_space_u16_b32_o,
        TOP_ld_qualifier_space_s8_r, TOP_ld_qualifier_space_s16_r, 
	TOP_ld_qualifier_space_s32_r, TOP_ld_qualifier_space_s64_r,
        TOP_ld_qualifier_space_u8_r, TOP_ld_qualifier_space_u16_r, 
	TOP_ld_qualifier_space_u32_r, TOP_ld_qualifier_space_u64_r,
        TOP_ld_qualifier_space_f32_r, TOP_ld_qualifier_space_f64_r,
        TOP_ld_qualifier_space_s8_b32_r, TOP_ld_qualifier_space_s16_b32_r,
        TOP_ld_qualifier_space_u8_b32_r, TOP_ld_qualifier_space_u16_b32_r,
        TOP_ld_qualifier_space_s8_a64, TOP_ld_qualifier_space_s16_a64, 
	TOP_ld_qualifier_space_s32_a64, TOP_ld_qualifier_space_s64_a64,
        TOP_ld_qualifier_space_u8_a64, TOP_ld_qualifier_space_u16_a64, 
	TOP_ld_qualifier_space_u32_a64, TOP_ld_qualifier_space_u64_a64,
        TOP_ld_qualifier_space_f32_a64, TOP_ld_qualifier_space_f64_a64,
        TOP_ld_qualifier_space_s8_b32_a64, TOP_ld_qualifier_space_s16_b32_a64,
        TOP_ld_qualifier_space_u8_b32_a64, TOP_ld_qualifier_space_u16_b32_a64,
        TOP_ld_qualifier_space_s8_a64_o, TOP_ld_qualifier_space_s16_a64_o, 
	TOP_ld_qualifier_space_s32_a64_o, TOP_ld_qualifier_space_s64_a64_o,
        TOP_ld_qualifier_space_u8_a64_o, TOP_ld_qualifier_space_u16_a64_o, 
	TOP_ld_qualifier_space_u32_a64_o, TOP_ld_qualifier_space_u64_a64_o,
        TOP_ld_qualifier_space_f32_a64_o, TOP_ld_qualifier_space_f64_a64_o,
        TOP_ld_qualifier_space_s8_b32_a64_o, TOP_ld_qualifier_space_s16_b32_a64_o,
        TOP_ld_qualifier_space_u8_b32_a64_o, TOP_ld_qualifier_space_u16_b32_a64_o,
        TOP_ld_qualifier_space_s8_a64_r, TOP_ld_qualifier_space_s16_a64_r, 
	TOP_ld_qualifier_space_s32_a64_r, TOP_ld_qualifier_space_s64_a64_r,
        TOP_ld_qualifier_space_u8_a64_r, TOP_ld_qualifier_space_u16_a64_r, 
	TOP_ld_qualifier_space_u32_a64_r, TOP_ld_qualifier_space_u64_a64_r,
        TOP_ld_qualifier_space_f32_a64_r, TOP_ld_qualifier_space_f64_a64_r,
        TOP_ld_qualifier_space_s8_b32_a64_r, TOP_ld_qualifier_space_s16_b32_a64_r,
        TOP_ld_qualifier_space_u8_b32_a64_r, TOP_ld_qualifier_space_u16_b32_a64_r,
			TOP_UNDEFINED);

/* ===== Memory store operator ====== */
  store = ISA_Property_Create ("store");
  Instruction_Group (store, 
        TOP_st_qualifier_space_s8, TOP_st_qualifier_space_s16, 
	TOP_st_qualifier_space_s32, TOP_st_qualifier_space_s64,
        TOP_st_qualifier_space_u8, TOP_st_qualifier_space_u16, 
	TOP_st_qualifier_space_u32, TOP_st_qualifier_space_u64,
        TOP_st_qualifier_space_f32, TOP_st_qualifier_space_f64,
        TOP_st_qualifier_space_s8_b32, TOP_st_qualifier_space_s16_b32,
        TOP_st_qualifier_space_u8_b32, TOP_st_qualifier_space_u16_b32,
        TOP_st_qualifier_space_s8_o, TOP_st_qualifier_space_s16_o, 
	TOP_st_qualifier_space_s32_o, TOP_st_qualifier_space_s64_o,
        TOP_st_qualifier_space_u8_o, TOP_st_qualifier_space_u16_o, 
	TOP_st_qualifier_space_u32_o, TOP_st_qualifier_space_u64_o,
        TOP_st_qualifier_space_f32_o, TOP_st_qualifier_space_f64_o,
        TOP_st_qualifier_space_s8_b32_o, TOP_st_qualifier_space_s16_b32_o,
        TOP_st_qualifier_space_u8_b32_o, TOP_st_qualifier_space_u16_b32_o,
        TOP_st_qualifier_space_s8_r, TOP_st_qualifier_space_s16_r, 
	TOP_st_qualifier_space_s32_r, TOP_st_qualifier_space_s64_r,
        TOP_st_qualifier_space_u8_r, TOP_st_qualifier_space_u16_r, 
	TOP_st_qualifier_space_u32_r, TOP_st_qualifier_space_u64_r,
        TOP_st_qualifier_space_f32_r, TOP_st_qualifier_space_f64_r,
        TOP_st_qualifier_space_s8_b32_r, TOP_st_qualifier_space_s16_b32_r,
        TOP_st_qualifier_space_u8_b32_r, TOP_st_qualifier_space_u16_b32_r,
        TOP_st_qualifier_space_s8_a64, TOP_st_qualifier_space_s16_a64, 
	TOP_st_qualifier_space_s32_a64, TOP_st_qualifier_space_s64_a64,
        TOP_st_qualifier_space_u8_a64, TOP_st_qualifier_space_u16_a64, 
	TOP_st_qualifier_space_u32_a64, TOP_st_qualifier_space_u64_a64,
        TOP_st_qualifier_space_f32_a64, TOP_st_qualifier_space_f64_a64,
        TOP_st_qualifier_space_s8_b32_a64, TOP_st_qualifier_space_s16_b32_a64,
        TOP_st_qualifier_space_u8_b32_a64, TOP_st_qualifier_space_u16_b32_a64,
        TOP_st_qualifier_space_s8_a64_o, TOP_st_qualifier_space_s16_a64_o, 
	TOP_st_qualifier_space_s32_a64_o, TOP_st_qualifier_space_s64_a64_o,
        TOP_st_qualifier_space_u8_a64_o, TOP_st_qualifier_space_u16_a64_o, 
	TOP_st_qualifier_space_u32_a64_o, TOP_st_qualifier_space_u64_a64_o,
        TOP_st_qualifier_space_f32_a64_o, TOP_st_qualifier_space_f64_a64_o,
        TOP_st_qualifier_space_s8_b32_a64_o, TOP_st_qualifier_space_s16_b32_a64_o,
        TOP_st_qualifier_space_u8_b32_a64_o, TOP_st_qualifier_space_u16_b32_a64_o,
        TOP_st_qualifier_space_s8_a64_r, TOP_st_qualifier_space_s16_a64_r, 
	TOP_st_qualifier_space_s32_a64_r, TOP_st_qualifier_space_s64_a64_r,
        TOP_st_qualifier_space_u8_a64_r, TOP_st_qualifier_space_u16_a64_r, 
	TOP_st_qualifier_space_u32_a64_r, TOP_st_qualifier_space_u64_a64_r,
        TOP_st_qualifier_space_f32_a64_r, TOP_st_qualifier_space_f64_a64_r,
        TOP_st_qualifier_space_s8_b32_a64_r, TOP_st_qualifier_space_s16_b32_a64_r,
        TOP_st_qualifier_space_u8_b32_a64_r, TOP_st_qualifier_space_u16_b32_a64_r,
			TOP_UNDEFINED);

  memory_8bit = ISA_Property_Create ("memory_8bit");
  Instruction_Group (memory_8bit, 
        TOP_ld_qualifier_space_s8, TOP_ld_qualifier_space_u8,
        TOP_ld_qualifier_space_s8_b32, TOP_ld_qualifier_space_u8_b32,
        TOP_ld_qualifier_space_s8_o, TOP_ld_qualifier_space_u8_o,
        TOP_ld_qualifier_space_s8_b32_o, TOP_ld_qualifier_space_u8_b32_o,
        TOP_ld_qualifier_space_s8_r, TOP_ld_qualifier_space_u8_r,
        TOP_ld_qualifier_space_s8_b32_r, TOP_ld_qualifier_space_u8_b32_r,
        TOP_st_qualifier_space_s8, TOP_st_qualifier_space_u8,
        TOP_st_qualifier_space_s8_b32, TOP_st_qualifier_space_u8_b32,
        TOP_st_qualifier_space_s8_o, TOP_st_qualifier_space_u8_o,
        TOP_st_qualifier_space_s8_b32_o, TOP_st_qualifier_space_u8_b32_o,
        TOP_st_qualifier_space_s8_r, TOP_st_qualifier_space_u8_r,
        TOP_st_qualifier_space_s8_b32_r, TOP_st_qualifier_space_u8_b32_r,
        TOP_ld_qualifier_space_s8_a64, TOP_ld_qualifier_space_u8_a64,
        TOP_ld_qualifier_space_s8_b32_a64, TOP_ld_qualifier_space_u8_b32_a64,
        TOP_ld_qualifier_space_s8_a64_o, TOP_ld_qualifier_space_u8_a64_o,
        TOP_ld_qualifier_space_s8_b32_a64_o, TOP_ld_qualifier_space_u8_b32_a64_o,
        TOP_ld_qualifier_space_s8_a64_r, TOP_ld_qualifier_space_u8_a64_r,
        TOP_ld_qualifier_space_s8_b32_a64_r, TOP_ld_qualifier_space_u8_b32_a64_r,
        TOP_st_qualifier_space_s8_a64, TOP_st_qualifier_space_u8_a64,
        TOP_st_qualifier_space_s8_b32_a64, TOP_st_qualifier_space_u8_b32_a64,
        TOP_st_qualifier_space_s8_a64_o, TOP_st_qualifier_space_u8_a64_o,
        TOP_st_qualifier_space_s8_b32_a64_o, TOP_st_qualifier_space_u8_b32_a64_o,
        TOP_st_qualifier_space_s8_a64_r, TOP_st_qualifier_space_u8_a64_r,
        TOP_st_qualifier_space_s8_b32_a64_r, TOP_st_qualifier_space_u8_b32_a64_r,
	TOP_ld_qualifier_global_v2_s8_r, TOP_ld_qualifier_global_v2_u8_r, 
	TOP_ld_qualifier_global_v2_s8_b32_r, TOP_ld_qualifier_global_v2_u8_b32_r, 
	TOP_ld_qualifier_global_v2_s8_a64_r, TOP_ld_qualifier_global_v2_u8_a64_r, 
	TOP_ld_qualifier_global_v2_s8_b32_a64_r, TOP_ld_qualifier_global_v2_u8_b32_a64_r, 
	TOP_ld_qualifier_global_v2_s8_o, TOP_ld_qualifier_global_v2_u8_o, 
	TOP_ld_qualifier_global_v2_s8_b32_o, TOP_ld_qualifier_global_v2_u8_b32_o, 
	TOP_ld_qualifier_global_v2_s8_a64_o, TOP_ld_qualifier_global_v2_u8_a64_o, 
	TOP_ld_qualifier_global_v2_s8_b32_a64_o, TOP_ld_qualifier_global_v2_u8_b32_a64_o, 
	TOP_ld_qualifier_global_v4_s8_r, TOP_ld_qualifier_global_v4_u8_r, 
	TOP_ld_qualifier_global_v4_s8_b32_r, TOP_ld_qualifier_global_v4_u8_b32_r, 
	TOP_ld_qualifier_global_v4_s8_a64_r, TOP_ld_qualifier_global_v4_u8_a64_r, 
	TOP_ld_qualifier_global_v4_s8_b32_a64_r, TOP_ld_qualifier_global_v4_u8_b32_a64_r, 
	TOP_ld_qualifier_global_v4_s8_o, TOP_ld_qualifier_global_v4_u8_o, 
	TOP_ld_qualifier_global_v4_s8_b32_o, TOP_ld_qualifier_global_v4_u8_b32_o, 
	TOP_ld_qualifier_global_v4_s8_a64_o, TOP_ld_qualifier_global_v4_u8_a64_o, 
	TOP_ld_qualifier_global_v4_s8_b32_a64_o, TOP_ld_qualifier_global_v4_u8_b32_a64_o, 
	TOP_st_qualifier_global_v2_s8_r, TOP_st_qualifier_global_v2_u8_r, 
	TOP_st_qualifier_global_v2_s8_b32_r, TOP_st_qualifier_global_v2_u8_b32_r, 
	TOP_st_qualifier_global_v2_s8_a64_r, TOP_st_qualifier_global_v2_u8_a64_r, 
	TOP_st_qualifier_global_v2_s8_b32_a64_r, TOP_st_qualifier_global_v2_u8_b32_a64_r, 
	TOP_st_qualifier_global_v2_s8_o, TOP_st_qualifier_global_v2_u8_o, 
	TOP_st_qualifier_global_v2_s8_b32_o, TOP_st_qualifier_global_v2_u8_b32_o, 
	TOP_st_qualifier_global_v2_s8_a64_o, TOP_st_qualifier_global_v2_u8_a64_o, 
	TOP_st_qualifier_global_v2_s8_b32_a64_o, TOP_st_qualifier_global_v2_u8_b32_a64_o, 
	TOP_st_qualifier_global_v4_s8_r, TOP_st_qualifier_global_v4_u8_r, 
	TOP_st_qualifier_global_v4_s8_b32_r, TOP_st_qualifier_global_v4_u8_b32_r, 
	TOP_st_qualifier_global_v4_s8_a64_r, TOP_st_qualifier_global_v4_u8_a64_r, 
	TOP_st_qualifier_global_v4_s8_b32_a64_r, TOP_st_qualifier_global_v4_u8_b32_a64_r, 
	TOP_st_qualifier_global_v4_s8_o, TOP_st_qualifier_global_v4_u8_o, 
	TOP_st_qualifier_global_v4_s8_b32_o, TOP_st_qualifier_global_v4_u8_b32_o, 
	TOP_st_qualifier_global_v4_s8_a64_o, TOP_st_qualifier_global_v4_u8_a64_o, 
	TOP_st_qualifier_global_v4_s8_b32_a64_o, TOP_st_qualifier_global_v4_u8_b32_a64_o, 
			TOP_UNDEFINED);

  memory_16bit = ISA_Property_Create ("memory_16bit");
  Instruction_Group (memory_16bit, 
        TOP_ld_qualifier_space_s16, TOP_ld_qualifier_space_u16,
        TOP_ld_qualifier_space_s16_b32, TOP_ld_qualifier_space_u16_b32,
        TOP_ld_qualifier_space_s16_o, TOP_ld_qualifier_space_u16_o,
        TOP_ld_qualifier_space_s16_b32_o, TOP_ld_qualifier_space_u16_b32_o,
        TOP_ld_qualifier_space_s16_r, TOP_ld_qualifier_space_u16_r,
        TOP_ld_qualifier_space_s16_b32_r, TOP_ld_qualifier_space_u16_b32_r,
        TOP_st_qualifier_space_s16, TOP_st_qualifier_space_u16,
        TOP_st_qualifier_space_s16_b32, TOP_st_qualifier_space_u16_b32,
        TOP_st_qualifier_space_s16_o, TOP_st_qualifier_space_u16_o,
        TOP_st_qualifier_space_s16_b32_o, TOP_st_qualifier_space_u16_b32_o,
        TOP_st_qualifier_space_s16_r, TOP_st_qualifier_space_u16_r,
        TOP_st_qualifier_space_s16_b32_r, TOP_st_qualifier_space_u16_b32_r,
        TOP_ld_qualifier_space_s16_a64, TOP_ld_qualifier_space_u16_a64,
        TOP_ld_qualifier_space_s16_b32_a64, TOP_ld_qualifier_space_u16_b32_a64,
        TOP_ld_qualifier_space_s16_a64_o, TOP_ld_qualifier_space_u16_a64_o,
        TOP_ld_qualifier_space_s16_b32_a64_o, TOP_ld_qualifier_space_u16_b32_a64_o,
        TOP_ld_qualifier_space_s16_a64_r, TOP_ld_qualifier_space_u16_a64_r,
        TOP_ld_qualifier_space_s16_b32_a64_r, TOP_ld_qualifier_space_u16_b32_a64_r,
        TOP_st_qualifier_space_s16_a64, TOP_st_qualifier_space_u16_a64,
        TOP_st_qualifier_space_s16_b32_a64, TOP_st_qualifier_space_u16_b32_a64,
        TOP_st_qualifier_space_s16_a64_o, TOP_st_qualifier_space_u16_a64_o,
        TOP_st_qualifier_space_s16_b32_a64_o, TOP_st_qualifier_space_u16_b32_a64_o,
        TOP_st_qualifier_space_s16_a64_r, TOP_st_qualifier_space_u16_a64_r,
        TOP_st_qualifier_space_s16_b32_a64_r, TOP_st_qualifier_space_u16_b32_a64_r,
	TOP_ld_qualifier_global_v2_s16_r, TOP_ld_qualifier_global_v2_u16_r, 
	TOP_ld_qualifier_global_v2_s16_b32_r, TOP_ld_qualifier_global_v2_u16_b32_r, 
	TOP_ld_qualifier_global_v2_s16_a64_r, TOP_ld_qualifier_global_v2_u16_a64_r, 
	TOP_ld_qualifier_global_v2_s16_b32_a64_r, TOP_ld_qualifier_global_v2_u16_b32_a64_r, 
	TOP_ld_qualifier_global_v2_s16_o, TOP_ld_qualifier_global_v2_u16_o, 
	TOP_ld_qualifier_global_v2_s16_b32_o, TOP_ld_qualifier_global_v2_u16_b32_o, 
	TOP_ld_qualifier_global_v2_s16_a64_o, TOP_ld_qualifier_global_v2_u16_a64_o, 
	TOP_ld_qualifier_global_v2_s16_b32_a64_o, TOP_ld_qualifier_global_v2_u16_b32_a64_o, 
	TOP_ld_qualifier_global_v4_s16_r, TOP_ld_qualifier_global_v4_u16_r, 
	TOP_ld_qualifier_global_v4_s16_b32_r, TOP_ld_qualifier_global_v4_u16_b32_r, 
	TOP_ld_qualifier_global_v4_s16_a64_r, TOP_ld_qualifier_global_v4_u16_a64_r, 
	TOP_ld_qualifier_global_v4_s16_b32_a64_r, TOP_ld_qualifier_global_v4_u16_b32_a64_r, 
	TOP_ld_qualifier_global_v4_s16_o, TOP_ld_qualifier_global_v4_u16_o, 
	TOP_ld_qualifier_global_v4_s16_b32_o, TOP_ld_qualifier_global_v4_u16_b32_o, 
	TOP_ld_qualifier_global_v4_s16_a64_o, TOP_ld_qualifier_global_v4_u16_a64_o, 
	TOP_ld_qualifier_global_v4_s16_b32_a64_o, TOP_ld_qualifier_global_v4_u16_b32_a64_o, 
	TOP_st_qualifier_global_v2_s16_r, TOP_st_qualifier_global_v2_u16_r, 
	TOP_st_qualifier_global_v2_s16_b32_r, TOP_st_qualifier_global_v2_u16_b32_r, 
	TOP_st_qualifier_global_v2_s16_a64_r, TOP_st_qualifier_global_v2_u16_a64_r, 
	TOP_st_qualifier_global_v2_s16_b32_a64_r, TOP_st_qualifier_global_v2_u16_b32_a64_r, 
	TOP_st_qualifier_global_v2_s16_o, TOP_st_qualifier_global_v2_u16_o, 
	TOP_st_qualifier_global_v2_s16_b32_o, TOP_st_qualifier_global_v2_u16_b32_o, 
	TOP_st_qualifier_global_v2_s16_a64_o, TOP_st_qualifier_global_v2_u16_a64_o, 
	TOP_st_qualifier_global_v2_s16_b32_a64_o, TOP_st_qualifier_global_v2_u16_b32_a64_o, 
	TOP_st_qualifier_global_v4_s16_r, TOP_st_qualifier_global_v4_u16_r, 
	TOP_st_qualifier_global_v4_s16_b32_r, TOP_st_qualifier_global_v4_u16_b32_r, 
	TOP_st_qualifier_global_v4_s16_a64_r, TOP_st_qualifier_global_v4_u16_a64_r, 
	TOP_st_qualifier_global_v4_s16_b32_a64_r, TOP_st_qualifier_global_v4_u16_b32_a64_r, 
	TOP_st_qualifier_global_v4_s16_o, TOP_st_qualifier_global_v4_u16_o, 
	TOP_st_qualifier_global_v4_s16_b32_o, TOP_st_qualifier_global_v4_u16_b32_o, 
	TOP_st_qualifier_global_v4_s16_a64_o, TOP_st_qualifier_global_v4_u16_a64_o, 
	TOP_st_qualifier_global_v4_s16_b32_a64_o, TOP_st_qualifier_global_v4_u16_b32_a64_o, 
			TOP_UNDEFINED);

  memory_32bit = ISA_Property_Create ("memory_32bit");
  Instruction_Group (memory_32bit, 
        TOP_ld_qualifier_space_s32, TOP_ld_qualifier_space_u32,
        TOP_ld_qualifier_space_s32_o, TOP_ld_qualifier_space_u32_o,
        TOP_ld_qualifier_space_s32_r, TOP_ld_qualifier_space_u32_r,
        TOP_st_qualifier_space_s32, TOP_st_qualifier_space_u32,
        TOP_st_qualifier_space_s32_o, TOP_st_qualifier_space_u32_o,
        TOP_st_qualifier_space_s32_r, TOP_st_qualifier_space_u32_r,
	TOP_ld_qualifier_space_f32, TOP_ld_qualifier_space_f32_o, TOP_ld_qualifier_space_f32_r,
	TOP_st_qualifier_space_f32, TOP_st_qualifier_space_f32_o, TOP_st_qualifier_space_f32_r,
        TOP_ld_qualifier_space_s32_a64, TOP_ld_qualifier_space_u32_a64,
        TOP_ld_qualifier_space_s32_a64_o, TOP_ld_qualifier_space_u32_a64_o,
        TOP_ld_qualifier_space_s32_a64_r, TOP_ld_qualifier_space_u32_a64_r,
        TOP_st_qualifier_space_s32_a64, TOP_st_qualifier_space_u32_a64,
        TOP_st_qualifier_space_s32_a64_o, TOP_st_qualifier_space_u32_a64_o,
        TOP_st_qualifier_space_s32_a64_r, TOP_st_qualifier_space_u32_a64_r,
	TOP_ld_qualifier_space_f32_a64, TOP_ld_qualifier_space_f32_a64_o, TOP_ld_qualifier_space_f32_a64_r,
	TOP_st_qualifier_space_f32_a64, TOP_st_qualifier_space_f32_a64_o, TOP_st_qualifier_space_f32_a64_r,
	TOP_ld_qualifier_global_v2_s32_r, TOP_ld_qualifier_global_v2_u32_r, 
	TOP_ld_qualifier_global_v2_f32_r, 
	TOP_ld_qualifier_global_v2_s32_a64_r, TOP_ld_qualifier_global_v2_u32_a64_r, 
	TOP_ld_qualifier_global_v2_f32_a64_r,
	TOP_ld_qualifier_global_v2_s32_o, TOP_ld_qualifier_global_v2_u32_o, 
	TOP_ld_qualifier_global_v2_f32_o,
	TOP_ld_qualifier_global_v2_s32_a64_o, TOP_ld_qualifier_global_v2_u32_a64_o, 
	TOP_ld_qualifier_global_v2_f32_a64_o, 
	TOP_ld_qualifier_global_v4_s32_r, TOP_ld_qualifier_global_v4_u32_r, 
	TOP_ld_qualifier_global_v4_f32_r, 
	TOP_ld_qualifier_global_v4_s32_a64_r, TOP_ld_qualifier_global_v4_u32_a64_r, 
	TOP_ld_qualifier_global_v4_f32_a64_r,
	TOP_ld_qualifier_global_v4_s32_o, TOP_ld_qualifier_global_v4_u32_o, 
	TOP_ld_qualifier_global_v4_f32_o,
	TOP_ld_qualifier_global_v4_s32_a64_o, TOP_ld_qualifier_global_v4_u32_a64_o, 
	TOP_ld_qualifier_global_v4_f32_a64_o, 
	TOP_st_qualifier_global_v2_s32_r, TOP_st_qualifier_global_v2_u32_r, 
	TOP_st_qualifier_global_v2_f32_r, 
	TOP_st_qualifier_global_v2_s32_a64_r, TOP_st_qualifier_global_v2_u32_a64_r, 
	TOP_st_qualifier_global_v2_f32_a64_r,
	TOP_st_qualifier_global_v2_s32_o, TOP_st_qualifier_global_v2_u32_o, 
	TOP_st_qualifier_global_v2_f32_o,
	TOP_st_qualifier_global_v2_s32_a64_o, TOP_st_qualifier_global_v2_u32_a64_o, 
	TOP_st_qualifier_global_v2_f32_a64_o, 
	TOP_st_qualifier_global_v4_s32_r, TOP_st_qualifier_global_v4_u32_r, 
	TOP_st_qualifier_global_v4_f32_r, 
	TOP_st_qualifier_global_v4_s32_a64_r, TOP_st_qualifier_global_v4_u32_a64_r, 
	TOP_st_qualifier_global_v4_f32_a64_r,
	TOP_st_qualifier_global_v4_s32_o, TOP_st_qualifier_global_v4_u32_o, 
	TOP_st_qualifier_global_v4_f32_o,
	TOP_st_qualifier_global_v4_s32_a64_o, TOP_st_qualifier_global_v4_u32_a64_o, 
	TOP_st_qualifier_global_v4_f32_a64_o, 
			TOP_UNDEFINED);

  memory_64bit = ISA_Property_Create ("memory_64bit");
  Instruction_Group (memory_64bit, 
        TOP_ld_qualifier_space_s64, TOP_ld_qualifier_space_u64,
        TOP_ld_qualifier_space_s64_o, TOP_ld_qualifier_space_u64_o,
        TOP_ld_qualifier_space_s64_r, TOP_ld_qualifier_space_u64_r,
        TOP_st_qualifier_space_s64, TOP_st_qualifier_space_u64,
        TOP_st_qualifier_space_s64_o, TOP_st_qualifier_space_u64_o,
        TOP_st_qualifier_space_s64_r, TOP_st_qualifier_space_u64_r,
	TOP_ld_qualifier_space_f64, TOP_ld_qualifier_space_f64_o, TOP_ld_qualifier_space_f64_r,
	TOP_st_qualifier_space_f64, TOP_st_qualifier_space_f64_o, TOP_st_qualifier_space_f64_r,
        TOP_ld_qualifier_space_s64_a64, TOP_ld_qualifier_space_u64_a64,
        TOP_ld_qualifier_space_s64_a64_o, TOP_ld_qualifier_space_u64_a64_o,
        TOP_ld_qualifier_space_s64_a64_r, TOP_ld_qualifier_space_u64_a64_r,
        TOP_st_qualifier_space_s64_a64, TOP_st_qualifier_space_u64_a64,
        TOP_st_qualifier_space_s64_a64_o, TOP_st_qualifier_space_u64_a64_o,
        TOP_st_qualifier_space_s64_a64_r, TOP_st_qualifier_space_u64_a64_r,
	TOP_ld_qualifier_space_f64_a64, TOP_ld_qualifier_space_f64_a64_o, TOP_ld_qualifier_space_f64_a64_r,
	TOP_st_qualifier_space_f64_a64, TOP_st_qualifier_space_f64_a64_o, TOP_st_qualifier_space_f64_a64_r,
	TOP_ld_qualifier_global_v2_s64_r, TOP_ld_qualifier_global_v2_u64_r, 
	TOP_ld_qualifier_global_v2_f64_r, 
	TOP_ld_qualifier_global_v2_s64_a64_r, TOP_ld_qualifier_global_v2_u64_a64_r, 
	TOP_ld_qualifier_global_v2_f64_a64_r,
	TOP_ld_qualifier_global_v2_s64_o, TOP_ld_qualifier_global_v2_u64_o, 
	TOP_ld_qualifier_global_v2_f64_o,
	TOP_ld_qualifier_global_v2_s64_a64_o, TOP_ld_qualifier_global_v2_u64_a64_o, 
	TOP_ld_qualifier_global_v2_f64_a64_o, 
	TOP_st_qualifier_global_v2_s64_r, TOP_st_qualifier_global_v2_u64_r, 
	TOP_st_qualifier_global_v2_f64_r, 
	TOP_st_qualifier_global_v2_s64_a64_r, TOP_st_qualifier_global_v2_u64_a64_r, 
	TOP_st_qualifier_global_v2_f64_a64_r,
	TOP_st_qualifier_global_v2_s64_o, TOP_st_qualifier_global_v2_u64_o, 
	TOP_st_qualifier_global_v2_f64_o,
	TOP_st_qualifier_global_v2_s64_a64_o, TOP_st_qualifier_global_v2_u64_a64_o, 
	TOP_st_qualifier_global_v2_f64_a64_o, 
			TOP_UNDEFINED);

  fp_loadstore = ISA_Property_Create ("fp_loadstore");
  Instruction_Group (fp_loadstore, 
	TOP_ld_qualifier_space_f32, TOP_ld_qualifier_space_f32_o, TOP_ld_qualifier_space_f32_r,
	TOP_st_qualifier_space_f32, TOP_st_qualifier_space_f32_o, TOP_st_qualifier_space_f32_r,
	TOP_ld_qualifier_space_f64, TOP_ld_qualifier_space_f64_o, TOP_ld_qualifier_space_f64_r,
	TOP_st_qualifier_space_f64, TOP_st_qualifier_space_f64_o, TOP_st_qualifier_space_f64_r,
	TOP_ld_qualifier_space_f32_a64, TOP_ld_qualifier_space_f32_a64_o, TOP_ld_qualifier_space_f32_a64_r,
	TOP_st_qualifier_space_f32_a64, TOP_st_qualifier_space_f32_a64_o, TOP_st_qualifier_space_f32_a64_r,
	TOP_ld_qualifier_space_f64_a64, TOP_ld_qualifier_space_f64_a64_o, TOP_ld_qualifier_space_f64_a64_r,
	TOP_st_qualifier_space_f64_a64, TOP_st_qualifier_space_f64_a64_o, TOP_st_qualifier_space_f64_a64_r,
			TOP_UNDEFINED);


/************ Definitions for Type Conversion Optimization *************/

  convertible_to16_if_in16 = ISA_Property_Create ("convertible_to16_if_in16");
  Instruction_Group (convertible_to16_if_in16, 
	TOP_mov_s32, TOP_mov_u32, TOP_mov_s32_lit, TOP_mov_u32_lit,
	TOP_and_b32, TOP_or_b32, TOP_xor_b32, TOP_not_b32, TOP_cnot_b32, 
	TOP_and_b32_lit, TOP_or_b32_lit, TOP_xor_b32_lit, 
	TOP_abs_s32, 
	TOP_min_s32, TOP_max_s32, TOP_min_u32, TOP_max_u32, 
	// shr_s32 not safe if 16th bit causes us to fill with 1
	TOP_shr_u32, TOP_shr_u32_lit,
	TOP_div_u32, TOP_rem_u32, TOP_div_u32_lit, TOP_rem_u32_lit,
			TOP_UNDEFINED);

  convertible_to16_if_out16 = ISA_Property_Create ("convertible_to16_if_out16");
  Instruction_Group (convertible_to16_if_out16, 
	TOP_mov_s32, TOP_mov_u32, TOP_mov_s32_lit, TOP_mov_u32_lit,
	TOP_and_b32, TOP_or_b32, TOP_xor_b32, TOP_not_b32,
	TOP_and_b32_lit, TOP_or_b32_lit, TOP_xor_b32_lit, 
	TOP_add_s32, TOP_sub_s32, TOP_add_u32, TOP_sub_u32, 
	TOP_add_s32_lit, TOP_sub_s32_lit, TOP_add_u32_lit, TOP_sub_u32_lit,
	TOP_neg_s32,
	TOP_shl_b32, TOP_shl_b32_lit, 
	TOP_mul_lo_s32, TOP_mul_lo_u32,
	TOP_mul_lo_s32_lit, TOP_mul_lo_u32_lit,
			TOP_UNDEFINED);

  // should maybe be part of convertible_to_16_if_in16
  compare_32 = ISA_Property_Create ("compare_32");
  Instruction_Group (compare_32, 
	TOP_setp_eq_s32, TOP_setp_ne_s32, TOP_setp_lt_s32,
	TOP_setp_le_s32, TOP_setp_gt_s32, TOP_setp_ge_s32,
	TOP_setp_eq_u32, TOP_setp_ne_u32, TOP_setp_lt_u32,
	TOP_setp_le_u32, TOP_setp_gt_u32, TOP_setp_ge_u32,
			TOP_UNDEFINED);

  cvt_8to32 = ISA_Property_Create ("cvt_8to32");
  Instruction_Group (cvt_8to32, 
	TOP_cvt_s32_s8, TOP_cvt_s32_u8, 
	TOP_cvt_u32_s8, TOP_cvt_u32_u8, 
			TOP_UNDEFINED);

  cvt_16to32 = ISA_Property_Create ("cvt_16to32");
  Instruction_Group (cvt_16to32, 
	TOP_cvt_s32_s16, TOP_cvt_s32_u16, 
	TOP_cvt_u32_s16, TOP_cvt_u32_u16, 
			TOP_UNDEFINED);

  cvt_32to8 = ISA_Property_Create ("cvt_32to8");
  Instruction_Group (cvt_32to8, 
	TOP_cvt_s8_s32, TOP_cvt_s8_u32, 
	TOP_cvt_u8_s32, TOP_cvt_u8_u32, 
			TOP_UNDEFINED);

  cvt_32to8in32 = ISA_Property_Create ("cvt_32to8in32");
  Instruction_Group (cvt_32to8in32, 
	TOP_cvt_s8_s32_b32, TOP_cvt_s8_u32_b32, 
	TOP_cvt_u8_s32_b32, TOP_cvt_u8_u32_b32, 
			TOP_UNDEFINED);

  cvt_32to16 = ISA_Property_Create ("cvt_32to16");
  Instruction_Group (cvt_32to16, 
	TOP_cvt_s16_s32, TOP_cvt_s16_u32, 
	TOP_cvt_u16_s32, TOP_cvt_u16_u32, 
			TOP_UNDEFINED);

  cvt_32to16in32 = ISA_Property_Create ("cvt_32to16in32");
  Instruction_Group (cvt_32to16in32, 
	TOP_cvt_s16_s32_b32, TOP_cvt_s16_u32_b32, 
	TOP_cvt_u16_s32_b32, TOP_cvt_u16_u32_b32, 
			TOP_UNDEFINED);

  load_8in32 = ISA_Property_Create ("load_8in32");
  Instruction_Group (load_8in32, 
	TOP_ld_qualifier_space_s8_b32, TOP_ld_qualifier_space_s8_b32_r, TOP_ld_qualifier_space_s8_b32_o,
	TOP_ld_qualifier_space_u8_b32, TOP_ld_qualifier_space_u8_b32_r, TOP_ld_qualifier_space_u8_b32_o,
	TOP_ld_qualifier_space_s8_b32_a64, TOP_ld_qualifier_space_s8_b32_a64_r, TOP_ld_qualifier_space_s8_b32_a64_o,
	TOP_ld_qualifier_space_u8_b32_a64, TOP_ld_qualifier_space_u8_b32_a64_r, TOP_ld_qualifier_space_u8_b32_a64_o,
			TOP_UNDEFINED);

  load_16in32 = ISA_Property_Create ("load_16in32");
  Instruction_Group (load_16in32, 
	TOP_ld_qualifier_space_s16_b32, TOP_ld_qualifier_space_s16_b32_r, TOP_ld_qualifier_space_s16_b32_o,
	TOP_ld_qualifier_space_u16_b32, TOP_ld_qualifier_space_u16_b32_r, TOP_ld_qualifier_space_u16_b32_o,
	TOP_ld_qualifier_space_s16_b32_a64, TOP_ld_qualifier_space_s16_b32_a64_r, TOP_ld_qualifier_space_s16_b32_a64_o,
	TOP_ld_qualifier_space_u16_b32_a64, TOP_ld_qualifier_space_u16_b32_a64_r, TOP_ld_qualifier_space_u16_b32_a64_o,
			TOP_UNDEFINED);

  store_8from32 = ISA_Property_Create ("store_8from32");
  Instruction_Group (store_8from32, 
	TOP_st_qualifier_space_s8_b32, TOP_st_qualifier_space_s8_b32_r, TOP_st_qualifier_space_s8_b32_o,
	TOP_st_qualifier_space_u8_b32, TOP_st_qualifier_space_u8_b32_r, TOP_st_qualifier_space_u8_b32_o,
	TOP_st_qualifier_space_s8_b32_a64, TOP_st_qualifier_space_s8_b32_a64_r, TOP_st_qualifier_space_s8_b32_a64_o,
	TOP_st_qualifier_space_u8_b32_a64, TOP_st_qualifier_space_u8_b32_a64_r, TOP_st_qualifier_space_u8_b32_a64_o,
			TOP_UNDEFINED);

  store_16from32 = ISA_Property_Create ("store_16from32");
  Instruction_Group (store_16from32, 
	TOP_st_qualifier_space_s16_b32, TOP_st_qualifier_space_s16_b32_r, TOP_st_qualifier_space_s16_b32_o,
	TOP_st_qualifier_space_u16_b32, TOP_st_qualifier_space_u16_b32_r, TOP_st_qualifier_space_u16_b32_o,
	TOP_st_qualifier_space_s16_b32_a64, TOP_st_qualifier_space_s16_b32_a64_r, TOP_st_qualifier_space_s16_b32_a64_o,
	TOP_st_qualifier_space_u16_b32_a64, TOP_st_qualifier_space_u16_b32_a64_r, TOP_st_qualifier_space_u16_b32_a64_o,
			TOP_UNDEFINED);

  /* =============== 32-bit multiplication Operations =============== */

  mul_lo_32 = ISA_Property_Create ("mul_lo_32");
  Instruction_Group (mul_lo_32, 
  TOP_mul_lo_s32, TOP_mul_lo_s32_lit, TOP_mul_lo_u32, TOP_mul_lo_u32_lit,
			TOP_UNDEFINED);

/********* End of definitions for Type Conversion Optimization **********/

  v2_load = ISA_Property_Create ("v2_load");
  Instruction_Group (v2_load, 
	TOP_ld_qualifier_global_v2_s8_r, TOP_ld_qualifier_global_v2_u8_r, 
	TOP_ld_qualifier_global_v2_s16_r, TOP_ld_qualifier_global_v2_u16_r, 
	TOP_ld_qualifier_global_v2_s8_b32_r, TOP_ld_qualifier_global_v2_u8_b32_r, 
	TOP_ld_qualifier_global_v2_s16_b32_r, TOP_ld_qualifier_global_v2_u16_b32_r, 
	TOP_ld_qualifier_global_v2_s32_r, TOP_ld_qualifier_global_v2_u32_r, 
	TOP_ld_qualifier_global_v2_s64_r, TOP_ld_qualifier_global_v2_u64_r, 
	TOP_ld_qualifier_global_v2_f32_r, TOP_ld_qualifier_global_v2_f64_r, 
	TOP_ld_qualifier_global_v2_s8_a64_r, TOP_ld_qualifier_global_v2_u8_a64_r, 
	TOP_ld_qualifier_global_v2_s16_a64_r, TOP_ld_qualifier_global_v2_u16_a64_r, 
	TOP_ld_qualifier_global_v2_s8_b32_a64_r, TOP_ld_qualifier_global_v2_u8_b32_a64_r, 
	TOP_ld_qualifier_global_v2_s16_b32_a64_r, TOP_ld_qualifier_global_v2_u16_b32_a64_r, 
	TOP_ld_qualifier_global_v2_s32_a64_r, TOP_ld_qualifier_global_v2_u32_a64_r, 
	TOP_ld_qualifier_global_v2_s64_a64_r, TOP_ld_qualifier_global_v2_u64_a64_r, 
	TOP_ld_qualifier_global_v2_f32_a64_r, TOP_ld_qualifier_global_v2_f64_a64_r, 
	TOP_ld_qualifier_global_v2_s8_o, TOP_ld_qualifier_global_v2_u8_o, 
	TOP_ld_qualifier_global_v2_s16_o, TOP_ld_qualifier_global_v2_u16_o, 
	TOP_ld_qualifier_global_v2_s8_b32_o, TOP_ld_qualifier_global_v2_u8_b32_o, 
	TOP_ld_qualifier_global_v2_s16_b32_o, TOP_ld_qualifier_global_v2_u16_b32_o, 
	TOP_ld_qualifier_global_v2_s32_o, TOP_ld_qualifier_global_v2_u32_o, 
	TOP_ld_qualifier_global_v2_s64_o, TOP_ld_qualifier_global_v2_u64_o, 
	TOP_ld_qualifier_global_v2_f32_o, TOP_ld_qualifier_global_v2_f64_o, 
	TOP_ld_qualifier_global_v2_s8_a64_o, TOP_ld_qualifier_global_v2_u8_a64_o, 
	TOP_ld_qualifier_global_v2_s16_a64_o, TOP_ld_qualifier_global_v2_u16_a64_o, 
	TOP_ld_qualifier_global_v2_s8_b32_a64_o, TOP_ld_qualifier_global_v2_u8_b32_a64_o, 
	TOP_ld_qualifier_global_v2_s16_b32_a64_o, TOP_ld_qualifier_global_v2_u16_b32_a64_o, 
	TOP_ld_qualifier_global_v2_s32_a64_o, TOP_ld_qualifier_global_v2_u32_a64_o, 
	TOP_ld_qualifier_global_v2_s64_a64_o, TOP_ld_qualifier_global_v2_u64_a64_o, 
	TOP_ld_qualifier_global_v2_f32_a64_o, TOP_ld_qualifier_global_v2_f64_a64_o, 
			TOP_UNDEFINED);

  v4_load = ISA_Property_Create ("v4_load");
  Instruction_Group (v4_load, 
	TOP_ld_qualifier_global_v4_s8_r, TOP_ld_qualifier_global_v4_u8_r, 
	TOP_ld_qualifier_global_v4_s16_r, TOP_ld_qualifier_global_v4_u16_r, 
	TOP_ld_qualifier_global_v4_s8_b32_r, TOP_ld_qualifier_global_v4_u8_b32_r, 
	TOP_ld_qualifier_global_v4_s16_b32_r, TOP_ld_qualifier_global_v4_u16_b32_r, 
	TOP_ld_qualifier_global_v4_s32_r, TOP_ld_qualifier_global_v4_u32_r, 
	TOP_ld_qualifier_global_v4_f32_r,
	TOP_ld_qualifier_global_v4_s8_a64_r, TOP_ld_qualifier_global_v4_u8_a64_r, 
	TOP_ld_qualifier_global_v4_s16_a64_r, TOP_ld_qualifier_global_v4_u16_a64_r, 
	TOP_ld_qualifier_global_v4_s8_b32_a64_r, TOP_ld_qualifier_global_v4_u8_b32_a64_r, 
	TOP_ld_qualifier_global_v4_s16_b32_a64_r, TOP_ld_qualifier_global_v4_u16_b32_a64_r, 
	TOP_ld_qualifier_global_v4_s32_a64_r, TOP_ld_qualifier_global_v4_u32_a64_r, 
	TOP_ld_qualifier_global_v4_f32_a64_r,
	TOP_ld_qualifier_global_v4_s8_o, TOP_ld_qualifier_global_v4_u8_o, 
	TOP_ld_qualifier_global_v4_s16_o, TOP_ld_qualifier_global_v4_u16_o, 
	TOP_ld_qualifier_global_v4_s8_b32_o, TOP_ld_qualifier_global_v4_u8_b32_o, 
	TOP_ld_qualifier_global_v4_s16_b32_o, TOP_ld_qualifier_global_v4_u16_b32_o, 
	TOP_ld_qualifier_global_v4_s32_o, TOP_ld_qualifier_global_v4_u32_o, 
	TOP_ld_qualifier_global_v4_f32_o,
	TOP_ld_qualifier_global_v4_s8_a64_o, TOP_ld_qualifier_global_v4_u8_a64_o, 
	TOP_ld_qualifier_global_v4_s16_a64_o, TOP_ld_qualifier_global_v4_u16_a64_o, 
	TOP_ld_qualifier_global_v4_s8_b32_a64_o, TOP_ld_qualifier_global_v4_u8_b32_a64_o, 
	TOP_ld_qualifier_global_v4_s16_b32_a64_o, TOP_ld_qualifier_global_v4_u16_b32_a64_o, 
	TOP_ld_qualifier_global_v4_s32_a64_o, TOP_ld_qualifier_global_v4_u32_a64_o, 
	TOP_ld_qualifier_global_v4_f32_a64_o,
			TOP_UNDEFINED);

  v2_store = ISA_Property_Create ("v2_store");
  Instruction_Group (v2_store, 
	TOP_st_qualifier_global_v2_s8_r, TOP_st_qualifier_global_v2_u8_r, 
	TOP_st_qualifier_global_v2_s16_r, TOP_st_qualifier_global_v2_u16_r, 
	TOP_st_qualifier_global_v2_s8_b32_r, TOP_st_qualifier_global_v2_u8_b32_r, 
	TOP_st_qualifier_global_v2_s16_b32_r, TOP_st_qualifier_global_v2_u16_b32_r, 
	TOP_st_qualifier_global_v2_s32_r, TOP_st_qualifier_global_v2_u32_r, 
	TOP_st_qualifier_global_v2_s64_r, TOP_st_qualifier_global_v2_u64_r, 
	TOP_st_qualifier_global_v2_f32_r, TOP_st_qualifier_global_v2_f64_r, 
	TOP_st_qualifier_global_v2_s8_a64_r, TOP_st_qualifier_global_v2_u8_a64_r, 
	TOP_st_qualifier_global_v2_s16_a64_r, TOP_st_qualifier_global_v2_u16_a64_r, 
	TOP_st_qualifier_global_v2_s8_b32_a64_r, TOP_st_qualifier_global_v2_u8_b32_a64_r, 
	TOP_st_qualifier_global_v2_s16_b32_a64_r, TOP_st_qualifier_global_v2_u16_b32_a64_r, 
	TOP_st_qualifier_global_v2_s32_a64_r, TOP_st_qualifier_global_v2_u32_a64_r, 
	TOP_st_qualifier_global_v2_s64_a64_r, TOP_st_qualifier_global_v2_u64_a64_r, 
	TOP_st_qualifier_global_v2_f32_a64_r, TOP_st_qualifier_global_v2_f64_a64_r, 
	TOP_st_qualifier_global_v2_s8_o, TOP_st_qualifier_global_v2_u8_o, 
	TOP_st_qualifier_global_v2_s16_o, TOP_st_qualifier_global_v2_u16_o, 
	TOP_st_qualifier_global_v2_s8_b32_o, TOP_st_qualifier_global_v2_u8_b32_o, 
	TOP_st_qualifier_global_v2_s16_b32_o, TOP_st_qualifier_global_v2_u16_b32_o, 
	TOP_st_qualifier_global_v2_s32_o, TOP_st_qualifier_global_v2_u32_o, 
	TOP_st_qualifier_global_v2_s64_o, TOP_st_qualifier_global_v2_u64_o, 
	TOP_st_qualifier_global_v2_f32_o, TOP_st_qualifier_global_v2_f64_o, 
	TOP_st_qualifier_global_v2_s8_a64_o, TOP_st_qualifier_global_v2_u8_a64_o, 
	TOP_st_qualifier_global_v2_s16_a64_o, TOP_st_qualifier_global_v2_u16_a64_o, 
	TOP_st_qualifier_global_v2_s8_b32_a64_o, TOP_st_qualifier_global_v2_u8_b32_a64_o, 
	TOP_st_qualifier_global_v2_s16_b32_a64_o, TOP_st_qualifier_global_v2_u16_b32_a64_o, 
	TOP_st_qualifier_global_v2_s32_a64_o, TOP_st_qualifier_global_v2_u32_a64_o, 
	TOP_st_qualifier_global_v2_s64_a64_o, TOP_st_qualifier_global_v2_u64_a64_o, 
	TOP_st_qualifier_global_v2_f32_a64_o, TOP_st_qualifier_global_v2_f64_a64_o, 
			TOP_UNDEFINED);

  v4_store = ISA_Property_Create ("v4_store");
  Instruction_Group (v4_store, 
	TOP_st_qualifier_global_v4_s8_r, TOP_st_qualifier_global_v4_u8_r, 
	TOP_st_qualifier_global_v4_s16_r, TOP_st_qualifier_global_v4_u16_r, 
	TOP_st_qualifier_global_v4_s8_b32_r, TOP_st_qualifier_global_v4_u8_b32_r, 
	TOP_st_qualifier_global_v4_s16_b32_r, TOP_st_qualifier_global_v4_u16_b32_r, 
	TOP_st_qualifier_global_v4_s32_r, TOP_st_qualifier_global_v4_u32_r, 
	TOP_st_qualifier_global_v4_f32_r,
	TOP_st_qualifier_global_v4_s8_a64_r, TOP_st_qualifier_global_v4_u8_a64_r, 
	TOP_st_qualifier_global_v4_s16_a64_r, TOP_st_qualifier_global_v4_u16_a64_r, 
	TOP_st_qualifier_global_v4_s8_b32_a64_r, TOP_st_qualifier_global_v4_u8_b32_a64_r, 
	TOP_st_qualifier_global_v4_s16_b32_a64_r, TOP_st_qualifier_global_v4_u16_b32_a64_r, 
	TOP_st_qualifier_global_v4_s32_a64_r, TOP_st_qualifier_global_v4_u32_a64_r, 
	TOP_st_qualifier_global_v4_f32_a64_r,
	TOP_st_qualifier_global_v4_s8_o, TOP_st_qualifier_global_v4_u8_o, 
	TOP_st_qualifier_global_v4_s16_o, TOP_st_qualifier_global_v4_u16_o, 
	TOP_st_qualifier_global_v4_s8_b32_o, TOP_st_qualifier_global_v4_u8_b32_o, 
	TOP_st_qualifier_global_v4_s16_b32_o, TOP_st_qualifier_global_v4_u16_b32_o, 
	TOP_st_qualifier_global_v4_s32_o, TOP_st_qualifier_global_v4_u32_o, 
	TOP_st_qualifier_global_v4_f32_o,
	TOP_st_qualifier_global_v4_s8_a64_o, TOP_st_qualifier_global_v4_u8_a64_o, 
	TOP_st_qualifier_global_v4_s16_a64_o, TOP_st_qualifier_global_v4_u16_a64_o, 
	TOP_st_qualifier_global_v4_s8_b32_a64_o, TOP_st_qualifier_global_v4_u8_b32_a64_o, 
	TOP_st_qualifier_global_v4_s16_b32_a64_o, TOP_st_qualifier_global_v4_u16_b32_a64_o, 
	TOP_st_qualifier_global_v4_s32_a64_o, TOP_st_qualifier_global_v4_u32_a64_o, 
	TOP_st_qualifier_global_v4_f32_a64_o,
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
	TOP_bra, TOP_bra_p, TOP_bra_np,
	TOP_bra_uni, TOP_bra_uni_p, TOP_bra_uni_np,
	TOP_call, TOP_call_uni,
			TOP_UNDEFINED);

/* ===== Subprogram call operator ====== */
  call = ISA_Property_Create ("call");
  Instruction_Group (call,  
	TOP_call, TOP_call_uni,
			TOP_UNDEFINED);

/* ===== Call/xfer is conditional ====== */
  cond = ISA_Property_Create ("cond");
  Instruction_Group (cond,  
	TOP_bra_p, TOP_bra_np,
			TOP_UNDEFINED);

/* ===== Cond call/xfer is likely ====== */
  likely = ISA_Property_Create ("likely");
  Instruction_Group (likely, 
			TOP_UNDEFINED);

/* ===== Result def is conditional ====== */
  cond_move = ISA_Property_Create ("cond_move");
  Instruction_Group (cond_move,
			TOP_UNDEFINED);

/* ===== Result must not be opnd ====== */
  uniq_res = ISA_Property_Create ("uniq_res");
  Instruction_Group (uniq_res,
			TOP_UNDEFINED);

/* ===== Result must be same as opnd ====== */
  same_res = ISA_Property_Create ("same_res");
  Instruction_Group (same_res,
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
			TOP_UNDEFINED);

/* ===== Integer subtract operator ====== */
  isub = ISA_Property_Create ("isub");
  Instruction_Group (isub,
			TOP_UNDEFINED);

/* ===== Integer multiply operator ====== */
  imul = ISA_Property_Create ("imul");
  Instruction_Group (imul,
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
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia multiply (MMMUL) type ====== */
  mmmul = ISA_Property_Create ("mmmul");
  Instruction_Group (mmmul,
		     TOP_UNDEFINED);

  noop = ISA_Property_Create ("noop");
  Instruction_Group (noop,
			TOP_noop, TOP_nop,
			TOP_UNDEFINED);

  dummy = ISA_Property_Create ("dummy");
  Instruction_Group (dummy,
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
			TOP_UNDEFINED);

/* ===== Logical OR operator ====== */
  ior = ISA_Property_Create ("ior");
  Instruction_Group (ior,
			TOP_UNDEFINED);

/* ===== Jump operator ====== */
  jump = ISA_Property_Create ("jump");
  Instruction_Group (jump,
			TOP_UNDEFINED);

/* ===== Indirect jump operator ====== */
  ijump = ISA_Property_Create ("ijump");
  Instruction_Group (ijump,
			TOP_UNDEFINED);

/* ===== Logical exclusive OR operator ====== */
  ixor = ISA_Property_Create ("ixor");
  Instruction_Group (ixor,
			TOP_UNDEFINED);

/* ===== Logical AND operator ====== */
  iand = ISA_Property_Create ("iand");
  Instruction_Group (iand,
			TOP_UNDEFINED);

/* ===== Integer compare operator ====== */
  icmp = ISA_Property_Create ("icmp");
  Instruction_Group (icmp,
			TOP_UNDEFINED);

/* ===== Simulated instructions ====== */
  simulated = ISA_Property_Create ("simulated");
  Instruction_Group (simulated,
			TOP_asm,
			TOP_intrncall,
			TOP_call, TOP_call_uni, /* needs special processing */
			TOP_spadjust,
			TOP_begin_pregtn, TOP_end_pregtn,
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

  ISA_Properties_End();
  return 0;
}
