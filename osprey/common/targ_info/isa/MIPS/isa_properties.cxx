/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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
//  $Revision: 1.6 $
//  $Date: 2006/05/30 06:52:26 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa_properties.cxx,v $


#include <stddef.h>
#include "topcode.h"
#include "isa_properties_gen.h"

main()
{
  ISA_PROPERTY
    move,   		/* Move operator */
    load,   		/* Memory load operator */
    store, 		/* Memory store operator */
    prefetch,		/* Prefetch operator */
    xfer, 		/* Control transfer operator */
    call, 		/* Subprogram call operator */
    cond, 		/* Call/xfer is conditional */
    likely, 		/* Cond call/xfer is likely */
    unalign_ld, 	/* Unaligned load operator */
    unalign_store,	/* Unaligned store operator */
    unknown_addr,	/* Memory operator potentially access any memory location */
    unknown_memdata,	/* Memory operator potentially alter data loaded/stored */
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
    iop64,		/* 64-bit only integer operator */
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
    exde_bit, /* extract and depost bits */
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
    branch_predict,	/* Branch prediction (but not actual xfer) */
    mem_fill_type,      /* Memory instructions which are fill/spill type */
    var_opnds,		/* Variable number of operands AND/OR results */
    base_update,	/* Instruction updates 'base' operand */
//    loop_start,         /* Instruction marks the start of a zero-cost loop */
#ifdef TARG_SL 
    instr16,          /**/
    mvtc, /*Operator defines special register*/
    opd_mode_instr,
    simd_acc_instr,
    cr_mode_instr,
    scan_instr,
    sclamp_instr,
    dblk_instr,
    defs_ctrl_reg,
    lnk,
    alu_instr,
    c3_load,
    c3_store, 
#ifdef TARG_SL2 
    fork_instr, 
    defs_copc,  /* Operator defines cp2 condition register */
    c2_load,  /* cp2 memory load operator */
    c2_v_load, /* cp2 vbuf memory load */ 
    c2_v_store, /* cp2 vbuf memory store */ 
    c2_s_load,  /* cp2 sbuf memory load */ 
    c2_s_store, /* cp2 sbuf memory store */ 
    c2_store, /* cp2 memory store operator */
    c2_multi_mode_load,
    c2_multi_mode_store,
    c2_has_bypass,
    no_peephole,
#endif 
    use_ctrl_reg,
#endif 
    loop_start;         /* Instruction marks the start of a zero-cost loop */



/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */

/* ===== Move operator ====== */
  move = ISA_Property_Create ("move");
  Instruction_Group (move,
#if defined(TARG_SL)  	
                     TOP_mc_z_eq,
                     TOP_mc_z_ne,
		     TOP_mc_z_gt,
		     TOP_mc_z_ge,
		     TOP_mc_z_lt,
		     TOP_mc_z_le,
		     TOP_mc_zn_eq,
		     TOP_mc_zn_ne,
		     TOP_mc_zn_gt,
		     TOP_mc_zn_ge,
		     TOP_mc_zn_lt,
		     TOP_mc_zn_le,
		     TOP_mc_r_eq,
		     TOP_mc_r_ne,
		     TOP_mc_r_gt,
		     TOP_mc_r_ge,
		     TOP_mc_r_lt,
		     TOP_mc_r_le,
#endif		    
		     TOP_movf,
		     TOP_movn,
		     TOP_movt,
		     TOP_movz,
		     TOP_mov_s,
		     TOP_mov_d,
		     TOP_movf_s,
		     TOP_movf_d,
		     TOP_movn_s,
		     TOP_movn_d,
		     TOP_movt_s,
		     TOP_movt_d,
		     TOP_movz_s,
		     TOP_movz_d,
                     TOP_UNDEFINED);

/* ===== Memory load operator ====== */
  load = ISA_Property_Create ("load");
  Instruction_Group (load,
		      TOP_lb,
		      TOP_lbu,
		      TOP_lh,
		      TOP_lhu,
		      TOP_lw,
		      TOP_ll,
		      TOP_lwu,
		      TOP_ld,
		      TOP_lld,
		      TOP_lwc1,
		      TOP_ldc1,
		      TOP_lwxc1,
		      TOP_ldxc1,
		      TOP_lwl,
		      TOP_lwr,
		      TOP_ldl,
		      TOP_ldr,
#if defined(TARG_SL)
                      TOP_pop16,
                      TOP_ldw16,
                      TOP_ldub16_rs,
                      TOP_lduh16_rs,
		      TOP_c3_dmac_a,
		      TOP_c3_dmacn_a,
		      TOP_c3_dmula_a,
		      TOP_c3_dmulan_a,
		      TOP_c3_mac_a,
		      TOP_c3_mac_ar,
		      TOP_c3_macn_a,
		      TOP_c3_macn_ar,
		      TOP_c3_mula_a,
		      TOP_c3_mula_ar,
		      TOP_c3_saadd_a,
		      TOP_c3_saaddh_a,
		      TOP_c3_saddha_a,
		      TOP_c3_samulh_a,
		      TOP_c3_sasub_a,
		      TOP_c3_sasubh_a,
                      TOP_c3_fftld,
                      TOP_c3_ld,
	              TOP_c3_fft,
	              TOP_c3_viterbi,
                      TOP_c3_trback,
                      // new c3 instruction
                      TOP_C3_dmac_a,
                      TOP_C3_dmacn_a,
                      TOP_C3_dmula_a,
                      TOP_C3_dmulan_a,
                      TOP_C3_ffe,
                      TOP_C3_ld,
                      TOP_C3_fftld,
                      TOP_C3_mac_a,
                      TOP_C3_macn_a,
                      TOP_C3_mac_ar,
                      TOP_C3_macn_ar,
                      TOP_C3_mula_a,
                      TOP_C3_mula_ar,
                      TOP_C3_saadd_a,
                      TOP_C3_sasub_a,
                      TOP_C3_saaddh_a,
                      TOP_C3_sasubh_a,
                      TOP_C3_sadda_a,
                      TOP_C3_samulh_a,
#endif
#ifdef TARG_SL2
                      TOP_c2_ld_v_b_u,
                      TOP_c2_ld_v_b,
                      TOP_c2_ld_v_h,
                      TOP_c2_ld_v_w,
                      TOP_c2_ld_v_sw,
                      TOP_c2_ld_v_m_b_u,
                      TOP_c2_ld_v_m_b,
                      TOP_c2_ld_v_m_h,
                      TOP_c2_ld_v_m_w,
                      TOP_c2_ldi_v2g_b_u,
                      TOP_c2_ldi_v2g_b,
                      TOP_c2_ldi_v2g_h_u,
                      TOP_c2_ldi_v2g_h,
                      TOP_c2_ldi_v2g_w,

                      TOP_c2_ld_s_h_u_p,
                      TOP_c2_ld_s_h_u,
                      TOP_c2_ld_v2g_b_u,
                      TOP_c2_ld_v2g_b,
                      TOP_c2_ldi_s_h_u,
                      TOP_c2_ldi_s_h,
                      TOP_c2_ldi_s_w,
                      TOP_c2_ldi_c,

                      TOP_c2_ld_s_h_p,
                      TOP_c2_ld_s_h,
                      TOP_c2_ldi_v_b_u,
                      TOP_c2_ldi_v_b,
                      TOP_c2_ldi_v_h,
                      TOP_c2_ldi_v_m_b_u,
                      TOP_c2_ldi_v_m_b,
                      TOP_c2_ldi_v_m_h,

                     TOP_c2_ld_s_w_p,
                     TOP_c2_ld_s_w,
                     TOP_c2_ld_v2g_h_u,
                     TOP_c2_ld_v2g_h,
                     TOP_c2_ld_v2g_w,
                     TOP_c2_ldi_v_w,
                     TOP_c2_ldi_v_m_w,
#endif
              TOP_UNDEFINED);

/* ===== SL2 Memory load operator ====== */
#ifdef TARG_SL2
  c2_load = ISA_Property_Create ("c2_load");
  Instruction_Group (c2_load,
                      TOP_c2_ld_v_b_u,
                      TOP_c2_ld_v_b,
                      TOP_c2_ld_v_h,
                      TOP_c2_ld_v_w,
                      TOP_c2_ld_v_sw,
                      TOP_c2_ld_v_m_b_u,
                      TOP_c2_ld_v_m_b,
                      TOP_c2_ld_v_m_h,
                      TOP_c2_ld_v_m_w,
                      TOP_c2_ldi_v2g_b_u,
                      TOP_c2_ldi_v2g_b,
                      TOP_c2_ldi_v2g_h_u,
                      TOP_c2_ldi_v2g_h,
                      TOP_c2_ldi_v2g_w,

                      TOP_c2_ld_s_h_u_p,
                      TOP_c2_ld_s_h_u,
                      TOP_c2_ld_v2g_b_u,
                      TOP_c2_ld_v2g_b,
                      TOP_c2_ldi_s_h_u,
                      TOP_c2_ldi_s_h,
                      TOP_c2_ldi_s_w,
                      TOP_c2_ldi_c,

                      TOP_c2_ld_s_h_p,
                      TOP_c2_ld_s_h,
                      TOP_c2_ldi_v_b_u,
                      TOP_c2_ldi_v_b,
                      TOP_c2_ldi_v_h,
                      TOP_c2_ldi_v_m_b_u,
                      TOP_c2_ldi_v_m_b,
                      TOP_c2_ldi_v_m_h,

                     TOP_c2_ld_s_w_p,
                     TOP_c2_ld_s_w,
                     TOP_c2_ld_v2g_h_u,
                     TOP_c2_ld_v2g_h,
                     TOP_c2_ld_v2g_w,
                     TOP_c2_ldi_v_w,
                     TOP_c2_ldi_v_m_w,
  	            TOP_UNDEFINED);





  c2_v_load = ISA_Property_Create ("c2_v_load");
  Instruction_Group (c2_v_load,
                      TOP_c2_ld_v_b_u,
                      TOP_c2_ld_v_b,
                      TOP_c2_ld_v_h,
                      TOP_c2_ld_v_w,
                      TOP_c2_ld_v_sw,
                      TOP_c2_ld_v_m_b_u,
                      TOP_c2_ld_v_m_b,
                      TOP_c2_ld_v_m_h,
                      TOP_c2_ld_v_m_w,
                      TOP_c2_ldi_v2g_b_u,
                      TOP_c2_ldi_v2g_b,
                      TOP_c2_ldi_v2g_h_u,
                      TOP_c2_ldi_v2g_h,
                      TOP_c2_ldi_v2g_w,
                      TOP_c2_ld_v2g_b_u,
                      TOP_c2_ld_v2g_b,
                      TOP_c2_ldi_v_b_u,
                      TOP_c2_ldi_v_b,
                      TOP_c2_ldi_v_h,
                      TOP_c2_ldi_v_m_b_u,
                      TOP_c2_ldi_v_m_b,
                      TOP_c2_ldi_v_m_h,
                     TOP_c2_ld_v2g_h_u,
                     TOP_c2_ld_v2g_h,
                     TOP_c2_ld_v2g_w,
                     TOP_c2_ldi_v_w,
                     TOP_c2_ldi_v_m_w,
  	            TOP_UNDEFINED);



    c2_s_load = ISA_Property_Create ("c2_s_load");
  Instruction_Group (c2_s_load,
                      TOP_c2_ld_s_h_u_p,
                      TOP_c2_ld_s_h_u,
                      TOP_c2_ldi_s_h_u,
                      TOP_c2_ldi_s_h,
                      TOP_c2_ldi_s_w,
                      TOP_c2_ldi_c,
                      TOP_c2_ld_s_h_p,
                      TOP_c2_ld_s_h,
                     TOP_c2_ld_s_w_p,
                     TOP_c2_ld_s_w,
 	            TOP_UNDEFINED);
  c2_multi_mode_load = ISA_Property_Create("c2_multi_mode_load");
  Instruction_Group(c2_multi_mode_load,
                    TOP_c2_ldi_v_m_b_u,
                    TOP_c2_ldi_v_m_b,
                    TOP_c2_ldi_v_m_h,
                    TOP_c2_ldi_v_m_w,
                    TOP_c2_ld_v_m_b_u,
                    TOP_c2_ld_v_m_b,
                    TOP_c2_ld_v_m_h,
                    TOP_c2_ld_v_m_w,
                    TOP_UNDEFINED);

  c2_multi_mode_store = ISA_Property_Create("c2_multi_mode_store");
  Instruction_Group(c2_multi_mode_store,
                    TOP_c2_sti_v_m_b,
                    TOP_c2_sti_v_m_h,
                    TOP_c2_sti_v_m_w,
                    TOP_c2_st_v_m_b,
                    TOP_c2_st_v_m_h,
                    TOP_c2_st_v_m_w,
                    TOP_UNDEFINED);

  c2_has_bypass = ISA_Property_Create("c2_has_bypass");
  Instruction_Group(c2_has_bypass, 
                   TOP_c2_vadds_h,
		   TOP_c2_vadds_w,
		   TOP_c2_vadds_p,
		   TOP_c2_vadds_h_mode6,
		   TOP_c2_vadds_h_mode2,		   
		   TOP_c2_vadds_w_mode6,
		   TOP_c2_vadds_w_mode2,
		   TOP_c2_vadds_p_mode6,
		   TOP_c2_vadds_p_mode2,
		   TOP_c2_vsubs_h,
		   TOP_c2_vsubs_h_sm,
		   TOP_c2_vsubs_h_abs,
		   TOP_c2_vsubs_h_abs_sm,
		   TOP_c2_vabs_h,
		   TOP_c2_vabs_h_sm,
		   TOP_c2_vsubs_w,
		   TOP_c2_vsubs_w_sm,
		   TOP_c2_vsubs_w_abs,
		   TOP_c2_vsubs_w_abs_sm,
		   TOP_c2_vabs_w,
		   TOP_c2_vabs_w_sm,
		   TOP_c2_vsubs_p,
		   TOP_c2_vsubs_p_sm,
		   TOP_c2_vsubs_p_abs,
		   TOP_c2_vsubs_p_abs_sm,
		   TOP_c2_vabs_p,
		   TOP_c2_vabs_p_sm,
		   TOP_c2_vmul_h,
		   TOP_c2_vmul_w,
		   TOP_c2_vneg_h,
		   TOP_c2_vneg_w,
		   TOP_c2_vneg_p,
		   TOP_c2_vshr_p,
		   TOP_c2_vshr_h,
		   TOP_c2_vshr_w,
		   TOP_c2_vshl_p,
		   TOP_c2_vshl_h,
		   TOP_c2_vshl_w,
		   TOP_c2_vclp,
		   TOP_c2_vclp_p,
		   TOP_c2_vclp_a,
		   TOP_c2_vclp_s,
		   TOP_c2_vclp_2,
		   TOP_c2_vclp_n,
		   TOP_c2_vclg_h_lt_and,
		   TOP_c2_vclg_h_lt_or,
		   TOP_c2_vclg_h_le_and,
		   TOP_c2_vclg_h_le_or,
		   TOP_c2_vclg_h_eq_and,
		   TOP_c2_vclg_h_eq_or,
		   TOP_c2_vclg_h_ge_and,
		   TOP_c2_vclg_h_ge_or,
		   TOP_c2_vclg_h_gt_and,
		   TOP_c2_vclg_h_gt_or,
		   TOP_c2_vclg_h_and,
		   TOP_c2_vclg_h_or,
		   TOP_c2_vclg_h_le,
		   TOP_c2_vclg_h_lt,
		   TOP_c2_vclg_h_ge,
		   TOP_c2_vclg_h_gt,
		   TOP_c2_vclg_w_lt_and,
		   TOP_c2_vclg_w_lt_or,
		   TOP_c2_vclg_w_le_and,
		   TOP_c2_vclg_w_le_or,
		   TOP_c2_vclg_w_eq_and,
		   TOP_c2_vclg_w_eq_or,
		   TOP_c2_vclg_w_ge_and,
		   TOP_c2_vclg_w_ge_or,
		   TOP_c2_vclg_w_gt_and,
		   TOP_c2_vclg_w_gt_or,
		   TOP_c2_vclg_w_and,
		   TOP_c2_vclg_w_or,
		   TOP_c2_vclg_w_le,
		   TOP_c2_vclg_w_lt,
		   TOP_c2_vclg_w_ge,
		   TOP_c2_vclg_w_gt,
		   TOP_c2_vclg_p_lt_and,
		   TOP_c2_vclg_p_lt_or,
		   TOP_c2_vclg_p_le_and,
		   TOP_c2_vclg_p_le_or,
		   TOP_c2_vclg_p_eq_and,
		   TOP_c2_vclg_p_eq_or,
		   TOP_c2_vclg_p_ge_and,
		   TOP_c2_vclg_p_ge_or,
		   TOP_c2_vclg_p_gt_and,
		   TOP_c2_vclg_p_gt_or,
		   TOP_c2_vclg_p_and,
		   TOP_c2_vclg_p_or,
		   TOP_c2_vclg_p_le,
		   TOP_c2_vclg_p_eq,
		   TOP_c2_vclg_p_ge,
		   TOP_c2_vclg_p_gt,
		   TOP_c2_vcmov_h_f,
		   TOP_c2_vcmov_h_t,
		   TOP_c2_vcmov_w_f,
		   TOP_c2_vcmov_w_t,
		   TOP_c2_lczero_z,
		   TOP_c2_lczero_nz_fw,
		   TOP_c2_lczero_nz_bw,
		   TOP_c2_vrnd_h,
		   TOP_c2_vrnd_w,
		   TOP_c2_vspas,
		   TOP_c2_vcmpr_h_eq,
		   TOP_c2_vcmpr_h_lt,
		   TOP_c2_vcmpr_h_le,
		   TOP_c2_vcmpr_h_gt,
		   TOP_c2_vcmpr_h_ge,
		   TOP_c2_vcmpr_w_eq,
		   TOP_c2_vcmpr_w_lt,
		   TOP_c2_vcmpr_w_le,
		   TOP_c2_vcmpr_w_gt,
		   TOP_c2_vcmpr_w_ge,
		   TOP_c2_cmov,
		   TOP_c2_clp,
		   TOP_c2_clp_i,
  	           TOP_UNDEFINED);
#endif

#ifdef TARG_SL
  c3_load = ISA_Property_Create("c3_load");
  Instruction_Group(c3_load,
                    TOP_c3_fftld,
	            TOP_c3_ld,
                    TOP_c3_dmac_a,
                    TOP_c3_dmacn_a,
                    TOP_c3_dmula_a,
                    TOP_c3_dmulan_a,
                    TOP_c3_mac_a,
                    TOP_c3_mac_ar,
                    TOP_c3_macn_a,
                    TOP_c3_macn_ar,
                    TOP_c3_mula_a,
                    TOP_c3_mula_ar,
                    TOP_c3_saadd_a,
                    TOP_c3_saaddh_a,
                    TOP_c3_saddha_a,
                    TOP_c3_samulh_a,
                    TOP_c3_sasub_a,
                    TOP_c3_sasubh_a,
                    TOP_c3_fft,
                    TOP_c3_viterbi,
                    TOP_c3_trback,
                    // new c3 instruction
                    TOP_C3_dmac_a,
                    TOP_C3_dmacn_a,
                    TOP_C3_dmula_a,
                    TOP_C3_dmulan_a,
                    TOP_C3_ffe,
                    TOP_C3_ld,
                    TOP_C3_fftld,
                    TOP_C3_mac_a,
                    TOP_C3_macn_a,
                    TOP_C3_mac_ar,
                    TOP_C3_macn_ar,
                    TOP_C3_mula_a,
                    TOP_C3_mula_ar,
                    TOP_C3_saadd_a,
                    TOP_C3_sasub_a,
                    TOP_C3_saaddh_a,
                    TOP_C3_sasubh_a,
                    TOP_C3_sadda_a,
                    TOP_C3_samulh_a,
	            TOP_UNDEFINED);
  
  c3_store = ISA_Property_Create("c3_store");
  Instruction_Group(c3_store,
                    TOP_c3_fftst,
                    TOP_c3_st,
                    // new c3 instruction version
                    TOP_C3_fftst,
                    TOP_C3_st,
                    TOP_UNDEFINED);

		   /* ===== 16-bit op*/
  instr16 = ISA_Property_Create("instr16");
  Instruction_Group(instr16,
		  TOP_abs16,
		  TOP_add16,
		    TOP_add16_i,
		    TOP_add16_sp,
                    TOP_and16,
                    TOP_and16_i,
                    TOP_jr16,
                    TOP_jr16_lnk,
                    TOP_ldw16,
                    TOP_ldub16_rs,
                    TOP_lduh16_rs,
                    TOP_mv16,
                    TOP_mv16_i,
                    TOP_mvfc16,
                    TOP_mvtc16,
                    TOP_nop16,
                    TOP_inv16,
                    TOP_or16,
                    TOP_or16_i,
                    TOP_pop16,
                    TOP_push16,
                    TOP_ret16,
                    TOP_shll16,
                    TOP_shll16_i,
                    TOP_shra16,
                    TOP_shra16_i,
                    TOP_shrl16,
                    TOP_shrl16_i,
                    TOP_stw16,
                    TOP_sub16,
                    TOP_sub16_i,
                    TOP_xor16,
                    TOP_xor16_i,
                    TOP_br16_eqz,
		    TOP_br16_nez,
  	            TOP_UNDEFINED);
/* ===== define to special register ====== */
  mvtc = ISA_Property_Create("mvtc");
  Instruction_Group(mvtc,
  	                          TOP_mvtc,
  	                          TOP_mvtc16,
                                  TOP_mvtc_i,
  	                          TOP_UNDEFINED);
 /* ===== define to special register ====== */
  lnk = ISA_Property_Create("lnk");
  Instruction_Group(lnk,
        TOP_jr16_lnk,
        TOP_jalr,//jr.lnk                         
        TOP_jal, 
	TOP_UNDEFINED);

 alu_instr = ISA_Property_Create("alu_instr");
 Instruction_Group(alu_instr, 
       	      TOP_abs16,
	      TOP_add16,
	      TOP_add16_i,
	      TOP_add16_sp,
	      TOP_and16,
	      TOP_and16_i,
	      TOP_mv16,
	      TOP_mv16_i,
	      TOP_mvfc16,
	      TOP_mvtc16,
	      TOP_inv16,
	      TOP_or16,
	      TOP_or16_i,
	      TOP_shll16,
	      TOP_shll16_i,
	      TOP_shra16,
	      TOP_shra16_i,
	      TOP_shrl16,
	      TOP_shrl16_i,
	      TOP_sub16,
	      TOP_sub16_i,
	      TOP_xor16,
	      TOP_xor16_i,
	      TOP_add,
	      TOP_addi,
	      TOP_addiu,
	      TOP_addu,
      TOP_slt,
      TOP_slti,
      TOP_sltiu,
      TOP_sltu,
      TOP_sub,
      TOP_subu,
      TOP_and,
      TOP_andi,
      TOP_nor,
      TOP_or,
      TOP_ori,
      TOP_xor,
      TOP_xori,
      TOP_mc_abs,
      TOP_mc_zc_eq,
      TOP_mc_zc_ne,
      TOP_mc_zc_gt,
      TOP_mc_zc_ge,
      TOP_mc_zc_lt,
      TOP_mc_zc_le,
      TOP_mc_z_eq,
      TOP_mc_z_ne,
      TOP_mc_z_gt,
      TOP_mc_z_ge,
      TOP_mc_z_lt,
      TOP_mc_z_le,
      TOP_mc_zn_eq,
      TOP_mc_zn_ne,
      TOP_mc_zn_gt,
      TOP_mc_zn_ge,
      TOP_mc_zn_lt,
      TOP_mc_zn_le,
      TOP_mc_r_eq,
      TOP_mc_r_ne,
      TOP_mc_r_gt,
      TOP_mc_r_ge,
      TOP_mc_r_lt,
      TOP_mc_r_le,
      TOP_depb,
      TOP_extrbs,
      TOP_extrbu,			    
      TOP_movf,
      TOP_movn,
      TOP_movt,
      TOP_movz,
      TOP_sll,
      TOP_sllv,
      TOP_sra,
      TOP_srav,
      TOP_srl,
      TOP_srlv,
      TOP_lui,
      TOP_UNDEFINED); 
#endif


/* ===== Memory store operator ====== */
  store = ISA_Property_Create ("store");
  Instruction_Group (store,
		      TOP_sb,
		      TOP_sh,
		      TOP_sw,
		      TOP_sc,
		      TOP_sd,
		      TOP_scd,
		      TOP_swc1,
		      TOP_sdc1,
		      TOP_swxc1,
		      TOP_sdxc1,
		      TOP_swl,
		      TOP_swr,
		      TOP_sdl,
		      TOP_sdr,
#if defined(TARG_SL)
                      TOP_push16,
                      TOP_stw16,
                      TOP_c3_st,
                      TOP_c3_fftst,
                      // new c3 instruction version
                      TOP_C3_st,
                      TOP_C3_fftst,
#endif                 
#ifdef TARG_SL2   
                      TOP_c2_st_v_b,
                      TOP_c2_st_v_h,
                      TOP_c2_st_v_w,
                      TOP_c2_st_v_m_b,
                      TOP_c2_st_v_m_h,
                      TOP_c2_st_v_m_w,
                      TOP_c2_st_s_h,
                      TOP_c2_st_s_h_p,
                      TOP_c2_st_s_w,
                      TOP_c2_st_s_w_p,
                      TOP_c2_st_g2v_b,
                      TOP_c2_st_g2v_h,
                      TOP_c2_st_g2v_w,
                      TOP_c2_sti_v_b,
                      TOP_c2_sti_v_h,
                      TOP_c2_sti_v_w,
                      TOP_c2_sti_v_m_b,
                      TOP_c2_sti_v_m_h,
                      TOP_c2_sti_v_m_w,
                      TOP_c2_sti_c,
                      TOP_c2_sti_s_h,
                      TOP_c2_sti_s_w,
                      TOP_c2_sti_g2v_b,
                      TOP_c2_sti_g2v_h,
                      TOP_c2_sti_g2v_w,
                      
#endif
                     TOP_UNDEFINED);

#ifdef TARG_SL2
/* ===== SL2 Memory store operator ====== */
  c2_store = ISA_Property_Create ("c2_store");
  Instruction_Group (c2_store,
                      TOP_c2_st_v_b,
                      TOP_c2_st_v_h,
                      TOP_c2_st_v_w,
                      TOP_c2_st_v_m_b,
                      TOP_c2_st_v_m_h,
                      TOP_c2_st_v_m_w,
                      TOP_c2_st_s_h,
                      TOP_c2_st_s_h_p,
                      TOP_c2_st_s_w,
                      TOP_c2_st_s_w_p,
                      TOP_c2_st_g2v_b,
                      TOP_c2_st_g2v_h,
                      TOP_c2_st_g2v_w,
                      TOP_c2_sti_v_b,
                      TOP_c2_sti_v_h,
                      TOP_c2_sti_v_w,
                      TOP_c2_sti_v_m_b,
                      TOP_c2_sti_v_m_h,
                      TOP_c2_sti_v_m_w,
                      TOP_c2_sti_c,
                      TOP_c2_sti_s_h,
                      TOP_c2_sti_s_w,
                      TOP_c2_sti_g2v_b,
                      TOP_c2_sti_g2v_h,
                      TOP_c2_sti_g2v_w,
                     TOP_UNDEFINED);

/* ===== SL2 vbuf Memory store operator ====== */
  c2_v_store = ISA_Property_Create ("c2_v_store");
  Instruction_Group (c2_v_store,
                      TOP_c2_st_v_b,
                      TOP_c2_st_v_h,
                      TOP_c2_st_v_w,
                      TOP_c2_st_v_m_b,
                      TOP_c2_st_v_m_h,
                      TOP_c2_st_v_m_w,
                      TOP_c2_st_g2v_b,
                      TOP_c2_st_g2v_h,
                      TOP_c2_st_g2v_w,
                      TOP_c2_sti_v_b,
                      TOP_c2_sti_v_h,
                      TOP_c2_sti_v_w,
                      TOP_c2_sti_v_m_b,
                      TOP_c2_sti_v_m_h,
                      TOP_c2_sti_v_m_w,
                      TOP_c2_sti_g2v_b,
                      TOP_c2_sti_g2v_h,
                      TOP_c2_sti_g2v_w,
                     TOP_UNDEFINED);

  /* ===== SL2 Memory store operator ====== */
  c2_s_store = ISA_Property_Create ("c2_s_store");
  Instruction_Group (c2_s_store,
                      TOP_c2_st_s_h,
                      TOP_c2_st_s_h_p,
                      TOP_c2_st_s_w,
                      TOP_c2_st_s_w_p,
                      TOP_c2_sti_c,
                      TOP_c2_sti_s_h,
                      TOP_c2_sti_s_w,
                     TOP_UNDEFINED);

  
#endif

/* ===== Prefetch operator ====== */
  prefetch = ISA_Property_Create ("prefetch");
  Instruction_Group (prefetch,
	  	      TOP_pref,
	  	      TOP_prefx,
                     TOP_UNDEFINED);

/* ===== Memory fill/spill type instructions ====== */
  mem_fill_type = ISA_Property_Create ("mem_fill_type");
  Instruction_Group (mem_fill_type,
		     TOP_UNDEFINED);

/* ===== Control transfer operator ====== */
  xfer = ISA_Property_Create ("xfer");
  Instruction_Group (xfer,
		      TOP_j,
		      TOP_jal,
		      TOP_jalr,
		      TOP_jr,
#if defined(TARG_SL)		      
                      TOP_ret,
                      TOP_ret16,
                      TOP_br16_eqz,
                      TOP_br16_nez,
                      TOP_jr16,
                      TOP_jr16_lnk,
#endif
		      TOP_beq,
		      TOP_bgez,
		      TOP_bgezal,
		      TOP_bgtz,
		      TOP_blez,
		      TOP_bltz,
		      TOP_bltzal,
		      TOP_bne,
		      TOP_bc1f,
		      TOP_bc1t,
#ifdef TARG_SL2  //fork_joint
                     TOP_c2_fork_m,
                     TOP_c2_fork_n, 
#endif 
                     TOP_UNDEFINED);

/* ===== Subprogram call operator ====== */
  call = ISA_Property_Create ("call");
  Instruction_Group (call,
		      TOP_jal,
		      TOP_jalr,
#ifdef TARG_SL
		      TOP_jr16_lnk,
#endif
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
		      TOP_bc1t,
#ifdef TARG_SL
		      //TOP_ret16,
		      //TOP_ret,	
                      TOP_br16_eqz,
                      TOP_br16_nez,
#endif 
                     TOP_UNDEFINED);

/* ===== Cond call/xfer is likely ====== */
  likely = ISA_Property_Create ("likely");
  Instruction_Group (likely,
                     TOP_UNDEFINED);

/* ===== Result def is conditional ====== */
  cond_move = ISA_Property_Create ("cond_move");
  Instruction_Group (cond_move,
#if defined(TARG_SL)  
                     TOP_mc_z_eq,
		     TOP_mc_z_ne,
		     TOP_mc_z_gt,
		     TOP_mc_z_ge,
		     TOP_mc_z_lt,
		     TOP_mc_z_le,
#endif                     
		     TOP_movf,
		     TOP_movn,
		     TOP_movt,
		     TOP_movz,
		     TOP_movf_s,
		     TOP_movf_d,
		     TOP_movn_s,
		     TOP_movn_d,
		     TOP_movt_s,
		     TOP_movt_d,
		     TOP_movz_s,
		     TOP_movz_d,
                     TOP_UNDEFINED);

/* ===== Result must not be opnd ====== */
  uniq_res = ISA_Property_Create ("uniq_res");
  Instruction_Group (uniq_res,
                     TOP_UNDEFINED);

/* ===== Result must be same as opnd ====== */
  same_res = ISA_Property_Create ("same_res");
  Instruction_Group (same_res,
		     /* Because asm macros can share the same operand between
		        inputs and outputs, we conservatively mark this 
			property so that the compiler doesn't try to separate
			any common operands. */
#ifdef TARG_SL
                     TOP_depb,
#endif
                     TOP_asm, 
#if defined(TARG_SL)                     
                     TOP_mc_z_eq,
	             TOP_mc_z_ne,
		     TOP_mc_z_gt,
  		     TOP_mc_z_ge,
		     TOP_mc_z_lt,
		     TOP_mc_z_le,
		     TOP_c3_bitc,
		     TOP_c3_dmac,
		     TOP_c3_dmacn,
		     TOP_c3_dmac_a,
  	             TOP_c3_dmacn_a,
  	             TOP_c3_mac,
  	             TOP_c3_mac_a,
  	             TOP_c3_mac_i,
  	             TOP_c3_macci,
                     TOP_c3_macd,
  	             TOP_c3_macn, 
  	             TOP_c3_macn_a,
  	             TOP_c3_macn_ar,
  	             TOP_c3_mac_ar,
  	             TOP_c3_round,
  	             TOP_c3_saddha,
  	             TOP_c3_saddha_a,
                     TOP_c3_viterbi,
                     TOP_c3_trback,
                     TOP_c3_fft,
                     // new c3 instruction
                     TOP_C3_aadda,
                     TOP_C3_dmac,
                     TOP_C3_dmacn,
                     TOP_C3_dmac_a,
                     TOP_C3_dmacn_a,
                     TOP_C3_ffe,
                     TOP_C3_mac,
                     TOP_C3_macn,
                     TOP_C3_mac_a,
                     TOP_C3_macn_a,
                     TOP_C3_mac_ar,
                     TOP_C3_macn_ar,
                     TOP_C3_round,
                     TOP_C3_sadda,
                     TOP_C3_sadda_a,
                     TOP_C3_shav,
                     TOP_C3_shla_i,
#endif		     
#ifdef TARG_SL2
                   TOP_c2_bxtr_u_l,
                   TOP_c2_bxtr_s_l,
                   TOP_c2_bxtr_u_m,
                   TOP_c2_bxtr_s_m,
                   TOP_c2_bdep_l,
                   TOP_c2_bdep_m,
#endif 
                     TOP_UNDEFINED);

/* ===== Operator is a machine level select ====== */
  select = ISA_Property_Create ("select");
  Instruction_Group (select,
                     TOP_UNDEFINED);

/* ===== Unaligned load operator ====== */
  unalign_ld = ISA_Property_Create ("unalign_ld");
  Instruction_Group (unalign_ld,
		      TOP_lwl,
		      TOP_lwr,
		      TOP_ldl,
		      TOP_ldr,
                     TOP_UNDEFINED);

/* ===== Unaligned store operator ====== */
  unalign_store = ISA_Property_Create ("unalign_store");
  Instruction_Group (unalign_store,
		      TOP_swl,
		      TOP_swr,
		      TOP_sdl,
		      TOP_sdr,
		     TOP_UNDEFINED);

/* ===== Unknown addr operator ====== */
  unknown_addr = ISA_Property_Create ("unknown_memdata");
  Instruction_Group (unknown_memdata,
		     TOP_UNDEFINED);

/* ===== Unknown addr operator ====== */
  unknown_addr = ISA_Property_Create ("unknown_addr");
  Instruction_Group (unknown_addr,
		     TOP_UNDEFINED);

/* ===== Integer add operator ====== */
  iadd = ISA_Property_Create ("iadd");
  Instruction_Group (iadd,
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
                     TOP_sub,
		     TOP_subu,
		     TOP_dsub,
		     TOP_dsubu,
                     TOP_UNDEFINED);

/* ===== Integer multiply operator ====== */
  imul = ISA_Property_Create ("imul");
  Instruction_Group (imul,
	  	     TOP_mult,
	  	     TOP_multu,
	  	     TOP_dmult,
	  	     TOP_dmultu,
                     TOP_UNDEFINED);

/* ===== Integer divide operator ====== */
  idiv = ISA_Property_Create ("idiv");
  Instruction_Group (idiv,
	  	     TOP_div,
	  	     TOP_divu,
	  	     TOP_ddiv,
	  	     TOP_ddivu,
                     TOP_UNDEFINED);

/* ===== 64-bit-only integer operator ====== */
  iop64 = ISA_Property_Create ("iop64");
  Instruction_Group (iop64,
		     TOP_dadd,
		     TOP_daddi,
		     TOP_daddiu,
		     TOP_daddu,
		     TOP_dsub,
		     TOP_dsubu,
	  	     TOP_dmult,
	  	     TOP_dmultu,
	  	     TOP_ddiv,
	  	     TOP_ddivu,
		     TOP_dsll,
		     TOP_dsll32,
		     TOP_dsllv,
		     TOP_dsra,
		     TOP_dsra32,
		     TOP_dsrav,
		     TOP_dsrl,
		     TOP_dsrl32,
		     TOP_dsrlv,
		     TOP_dmfc1,
		     TOP_dmtc1,
                     TOP_UNDEFINED);

/* ===== Any proper floating point op ====== */
  flop = ISA_Property_Create ("flop");
  Instruction_Group (flop,
		      TOP_abs_s,
		      TOP_abs_d,
		      TOP_add_s,
		      TOP_add_d,
		      TOP_c_f_s,
		      TOP_c_f_d,
		      TOP_c_t_s,
		      TOP_c_t_d,
		      TOP_c_un_s,
		      TOP_c_un_d,
		      TOP_c_or_s,
		      TOP_c_or_d,
		      TOP_c_eq_s,
		      TOP_c_eq_d,
		      TOP_c_neq_s,
		      TOP_c_neq_d,
		      TOP_c_ueq_s,
		      TOP_c_ueq_d,
		      TOP_c_olg_s,
		      TOP_c_olg_d,
		      TOP_c_olt_s,
		      TOP_c_olt_d,
		      TOP_c_uge_s,
		      TOP_c_uge_d,
		      TOP_c_ult_s,
		      TOP_c_ult_d,
		      TOP_c_oge_s,
		      TOP_c_oge_d,
		      TOP_c_ole_s,
		      TOP_c_ole_d,
		      TOP_c_ugt_s,
		      TOP_c_ugt_d,
		      TOP_c_ule_s,
		      TOP_c_ule_d,
		      TOP_c_ogt_s,
		      TOP_c_ogt_d,
		      TOP_c_sf_s,
		      TOP_c_sf_d,
		      TOP_c_st_s,
		      TOP_c_st_d,
		      TOP_c_ngle_s,
		      TOP_c_ngle_d,
		      TOP_c_gle_s,
		      TOP_c_gle_d,
		      TOP_c_seq_s,
		      TOP_c_seq_d,
		      TOP_c_sne_s,
		      TOP_c_sne_d,
		      TOP_c_ngl_s,
		      TOP_c_ngl_d,
		      TOP_c_gl_s,
		      TOP_c_gl_d,
		      TOP_c_lt_s,
		      TOP_c_lt_d,
		      TOP_c_nlt_s,
		      TOP_c_nlt_d,
		      TOP_c_nge_s,
		      TOP_c_nge_d,
		      TOP_c_ge_s,
		      TOP_c_ge_d,
		      TOP_c_le_s,
		      TOP_c_le_d,
		      TOP_c_nle_s,
		      TOP_c_nle_d,
		      TOP_c_ngt_s,
		      TOP_c_ngt_d,
		      TOP_c_gt_s,
		      TOP_c_gt_d,
		      TOP_div_s,
		      TOP_div_d,
		      TOP_mul_s,
		      TOP_mul_d,
		      TOP_neg_s,
		      TOP_neg_d,
		      TOP_sub_s,
		      TOP_sub_d,
		      TOP_sqrt_s,
		      TOP_sqrt_d,
		      TOP_madd_s,
		      TOP_madd_d,
		      TOP_msub_s,
		      TOP_msub_d,
		      TOP_nmadd_s,
		      TOP_nmadd_d,
		      TOP_nmsub_s,
		      TOP_nmsub_d,
		      TOP_recip_s,
		      TOP_recip_d,
		      TOP_rsqrt_s,
		      TOP_rsqrt_d,
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
		      TOP_recip_s,
		      TOP_recip_d,
		      TOP_rsqrt_s,
		      TOP_rsqrt_d,
		      TOP_neg_s,
		      TOP_neg_d,
		     TOP_UNDEFINED);

/* ===== The kind that do two at once ====== */
  // Used in isa/expand.cxx only and not for any purpose we could use.
  madd = ISA_Property_Create ("madd");
  Instruction_Group (madd,
		      TOP_madd_s,
		      TOP_madd_d,
		      TOP_msub_s,
		      TOP_msub_d,
		      TOP_nmadd_s,
		      TOP_nmadd_d,
		      TOP_nmsub_s,
		      TOP_nmsub_d,
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
                     TOP_nop,
                     TOP_noop,
#if defined(TARG_SL)                     
                     TOP_nop16,
#endif                     
                     TOP_UNDEFINED);

  dummy = ISA_Property_Create ("dummy");
  Instruction_Group (dummy,
		     TOP_begin_pregtn,
		     TOP_end_pregtn,
		     TOP_fwd_bar,
		     TOP_bwd_bar,
		     TOP_label,
                   TOP_noop,
#if defined(TARG_SL) || defined(TARG_SL2)                   
                   TOP_peripheral_rw_begin,
                   TOP_peripheral_rw_end,
#endif                   
		     TOP_UNDEFINED);

/* ====================================================================
 *              Exception classification descriptors
 * ====================================================================
 */

  /* ===== Integer trap potential ====== */
  itrap = ISA_Property_Create ("itrap");
  Instruction_Group (itrap,
		     TOP_teq,
		     TOP_teqi,
		     TOP_tge,
		     TOP_tgei,
		     TOP_tgeiu,
		     TOP_tgeu,
		     TOP_tlt,
		     TOP_tlti,
		     TOP_tltiu,
		     TOP_tltu,
		     TOP_tne,
		     TOP_tnei,
                     TOP_UNDEFINED);

  /* ===== Never traps -- always safe ====== */
  safe = ISA_Property_Create ("safe");
  Instruction_Group (safe,
			// TODO
                     TOP_UNDEFINED);

  /* ===== Unsafe always ====== */
  unsafe = ISA_Property_Create ("unsafe");
  Instruction_Group (unsafe,
                     TOP_fwd_bar, TOP_bwd_bar,
#if defined(TARG_SL) || defined(TARG_sL2)
                     TOP_peripheral_rw_begin,
                     TOP_peripheral_rw_end,
#endif                     
                     TOP_UNDEFINED);

  /* ===== Floating point trap potential ====== */
  ftrap = ISA_Property_Create ("ftrap");
  Instruction_Group (ftrap,
                     TOP_UNDEFINED);

  /* ===== Floating point divides ====== */
  fdiv = ISA_Property_Create ("fdiv");
  Instruction_Group (fdiv,
		     TOP_recip_s,
		     TOP_recip_d,
		     TOP_rsqrt_s,
		     TOP_rsqrt_d,
		     TOP_div_s,
		     TOP_div_d,
                     TOP_UNDEFINED);

  /* ===== Square roots ====== */
  sqrt = ISA_Property_Create ("sqrt");
  Instruction_Group (sqrt,
		      TOP_rsqrt_s,
		      TOP_rsqrt_d,
		      TOP_sqrt_s,
		      TOP_sqrt_d,
                     TOP_UNDEFINED);

  /* ===== Memory trap potential ====== */
  memtrap = ISA_Property_Create ("memtrap");
  Instruction_Group (memtrap,
		      TOP_lb,
		      TOP_lbu,
		      TOP_lh,
		      TOP_lhu,
		      TOP_lw,
		      TOP_ll,
		      TOP_lwu,
		      TOP_ld,
		      TOP_lld,
		      TOP_lwc1,
		      TOP_ldc1,
		      TOP_lwxc1,
		      TOP_ldxc1,
		      TOP_lwl,
		      TOP_lwr,
		      TOP_ldl,
		      TOP_ldr,
		      TOP_sb,
		      TOP_sh,
		      TOP_sw,
		      TOP_sc,
		      TOP_sd,
		      TOP_scd,
		      TOP_swc1,
		      TOP_sdc1,
		      TOP_swxc1,
		      TOP_sdxc1,
		      TOP_swl,
		      TOP_swr,
		      TOP_sdl,
		      TOP_sdr,
#if defined(TARG_SL)
                      TOP_pop16,
                      TOP_ldw16,
                      TOP_ldub16_rs,
                      TOP_lduh16_rs,
                      TOP_push16,
                      TOP_stw16,
                      TOP_c3_ld,
                      TOP_c3_fftld,
                      TOP_c3_st,
                      TOP_c3_fftst,
	              TOP_C3_ld,
                      TOP_C3_fftld,
                      TOP_C3_st,
                      TOP_C3_fftst,
#endif		      
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
		      TOP_c_f_s,
		      TOP_c_f_d,
		      TOP_c_t_s,
		      TOP_c_t_d,
		      TOP_c_un_s,
		      TOP_c_un_d,
		      TOP_c_or_s,
		      TOP_c_or_d,
		      TOP_c_eq_s,
		      TOP_c_eq_d,
		      TOP_c_neq_s,
		      TOP_c_neq_d,
		      TOP_c_ueq_s,
		      TOP_c_ueq_d,
		      TOP_c_olg_s,
		      TOP_c_olg_d,
		      TOP_c_olt_s,
		      TOP_c_olt_d,
		      TOP_c_uge_s,
		      TOP_c_uge_d,
		      TOP_c_ult_s,
		      TOP_c_ult_d,
		      TOP_c_oge_s,
		      TOP_c_oge_d,
		      TOP_c_ole_s,
		      TOP_c_ole_d,
		      TOP_c_ugt_s,
		      TOP_c_ugt_d,
		      TOP_c_ule_s,
		      TOP_c_ule_d,
		      TOP_c_ogt_s,
		      TOP_c_ogt_d,
		      TOP_c_sf_s,
		      TOP_c_sf_d,
		      TOP_c_st_s,
		      TOP_c_st_d,
		      TOP_c_ngle_s,
		      TOP_c_ngle_d,
		      TOP_c_gle_s,
		      TOP_c_gle_d,
		      TOP_c_seq_s,
		      TOP_c_seq_d,
		      TOP_c_sne_s,
		      TOP_c_sne_d,
		      TOP_c_ngl_s,
		      TOP_c_ngl_d,
		      TOP_c_gl_s,
		      TOP_c_gl_d,
		      TOP_c_lt_s,
		      TOP_c_lt_d,
		      TOP_c_nlt_s,
		      TOP_c_nlt_d,
		      TOP_c_nge_s,
		      TOP_c_nge_d,
		      TOP_c_ge_s,
		      TOP_c_ge_d,
		      TOP_c_le_s,
		      TOP_c_le_d,
		      TOP_c_nle_s,
		      TOP_c_nle_d,
		      TOP_c_ngt_s,
		      TOP_c_ngt_d,
		      TOP_c_gt_s,
		      TOP_c_gt_d,
                     TOP_UNDEFINED);

/* ===== Operator defines FCR reg ====== */
  defs_fcr = ISA_Property_Create ("defs_fcr");
  Instruction_Group (defs_fcr,
		      TOP_c_f_s,
		      TOP_c_f_d,
		      TOP_c_t_s,
		      TOP_c_t_d,
		      TOP_c_un_s,
		      TOP_c_un_d,
		      TOP_c_or_s,
		      TOP_c_or_d,
		      TOP_c_eq_s,
		      TOP_c_eq_d,
		      TOP_c_neq_s,
		      TOP_c_neq_d,
		      TOP_c_ueq_s,
		      TOP_c_ueq_d,
		      TOP_c_olg_s,
		      TOP_c_olg_d,
		      TOP_c_olt_s,
		      TOP_c_olt_d,
		      TOP_c_uge_s,
		      TOP_c_uge_d,
		      TOP_c_ult_s,
		      TOP_c_ult_d,
		      TOP_c_oge_s,
		      TOP_c_oge_d,
		      TOP_c_ole_s,
		      TOP_c_ole_d,
		      TOP_c_ugt_s,
		      TOP_c_ugt_d,
		      TOP_c_ule_s,
		      TOP_c_ule_d,
		      TOP_c_ogt_s,
		      TOP_c_ogt_d,
		      TOP_c_sf_s,
		      TOP_c_sf_d,
		      TOP_c_st_s,
		      TOP_c_st_d,
		      TOP_c_ngle_s,
		      TOP_c_ngle_d,
		      TOP_c_gle_s,
		      TOP_c_gle_d,
		      TOP_c_seq_s,
		      TOP_c_seq_d,
		      TOP_c_sne_s,
		      TOP_c_sne_d,
		      TOP_c_ngl_s,
		      TOP_c_ngl_d,
		      TOP_c_gl_s,
		      TOP_c_gl_d,
		      TOP_c_lt_s,
		      TOP_c_lt_d,
		      TOP_c_nlt_s,
		      TOP_c_nlt_d,
		      TOP_c_nge_s,
		      TOP_c_nge_d,
		      TOP_c_ge_s,
		      TOP_c_ge_d,
		      TOP_c_le_s,
		      TOP_c_le_d,
		      TOP_c_nle_s,
		      TOP_c_nle_d,
		      TOP_c_ngt_s,
		      TOP_c_ngt_d,
		      TOP_c_gt_s,
		      TOP_c_gt_d,
		      TOP_ctc1,
                     TOP_UNDEFINED);

/* ===== Operator uses FCR reg ====== */
  refs_fcr = ISA_Property_Create ("refs_fcr");
  Instruction_Group (refs_fcr,
	  	      TOP_cfc1,
	  	      TOP_bc1f,
	  	      TOP_bc1t,
#if defined(TARG_SL)
		    TOP_cvt_d_s,
		    TOP_cvt_s_d,
		    TOP_cvt_d_w,
		    TOP_cvt_d_l,
		    TOP_cvt_s_w,
		    TOP_cvt_s_l,
		    TOP_cvt_l_s,
		    TOP_cvt_l_d,
		    TOP_cvt_w_s,
		    TOP_cvt_w_d,
#endif
                     TOP_UNDEFINED);

/* ===== Operator defs int val in FP reg ====== */
  defs_fpu_int = ISA_Property_Create ("defs_fpu_int");
  Instruction_Group (defs_fpu_int,
	  		// TODO
                     TOP_UNDEFINED);

/* ===== Operator defines FP reg ====== */
  defs_fp = ISA_Property_Create ("defs_fp");
  Instruction_Group (defs_fp,
	  		// TODO
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
                     TOP_j,
                     TOP_UNDEFINED);

/* ===== Indirect jump operator ====== */
  ijump = ISA_Property_Create ("ijump");
  Instruction_Group (ijump,
                     TOP_jr,
                     TOP_UNDEFINED);

/* ===== Extract and Depost bits operator ====== */
  exde_bit = ISA_Property_Create ("exde_bit");
  Instruction_Group (exde_bit,
#ifdef TARG_SL
										TOP_depb,
										TOP_extrbs,
										TOP_extrbu,
#endif
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
	  	     TOP_sltu,
	  	     TOP_sltiu,
                     TOP_UNDEFINED);

/* ===== Simulated instructions ====== */
  simulated = ISA_Property_Create ("simulated");
  Instruction_Group (simulated,
                     TOP_asm,
                     TOP_spadjust,
                     TOP_intrncall,
                     TOP_UNDEFINED);

/* ===== Predicated instructions ====== */
  predicated = ISA_Property_Create ("predicated");
  Instruction_Group (predicated,
                     TOP_UNDEFINED);

/* ===== Instructions access rotating register banks ====== */
  access_reg_bank = ISA_Property_Create ("access_reg_bank");
  Instruction_Group (access_reg_bank,
		     TOP_UNDEFINED);

/* ===== Instructions not subject to peephole (e.g.ebo)  ====== */
#ifdef TARG_SL2
  no_peephole = ISA_Property_Create ("npeep");
  Instruction_Group (no_peephole,
                     TOP_c2_mvgr_r2g_h_u,
		     TOP_c2_mvgr_r2g_h,
		     TOP_c2_mvgr_r2g_w,
		     TOP_c2_mvgr_r2g_h_u_i,
		     TOP_c2_mvgr_r2g_h_i,
		     TOP_c2_mvgr_r2g_w_i,
		     TOP_c2_mvgr_g2r_ba_lh,
		     TOP_c2_mvgr_g2r_ba_hh,
		     TOP_c2_mvgr_g2r_ba_w,
		     TOP_c2_mvgr_g2r_lh_i,
		     TOP_c2_mvgr_g2r_hh_i,
		     TOP_c2_mvgr_g2r_w_i,
		     TOP_c2_mvgr_g2r_lh,
		     TOP_c2_mvgr_g2r_hh,
		     TOP_c2_mvgr_g2r_w,
		     TOP_c2_mvgr_g2r_bh,
		     TOP_c2_mvgr_g2r_bh_u,
		     TOP_c2_mvgr_g2r_bv,
		     TOP_c2_mvgr_g2r_bv_u,
		     TOP_c2_mvgr_g2r_b4_i,
		     TOP_c2_mvgr_g2r_b4,
		     TOP_c2_mvgc_c2g,
		     TOP_c2_mvgc_g2c,
		     TOP_c2_ld_v_b_u,
		     TOP_c2_ld_v_b,
		     TOP_c2_ld_v_h,
		     TOP_c2_ld_v_w,
		     TOP_c2_ld_v_sw,
		     TOP_c2_ld_v_m_b_u,
		     TOP_c2_ld_v_m_b,
		     TOP_c2_ld_v_m_h,
		     TOP_c2_ld_v_m_w,
		     TOP_c2_ld_s_h_u_p,
		     TOP_c2_ld_s_h_u,
		     TOP_c2_ld_s_h_p,
		     TOP_c2_ld_s_h,
		     TOP_c2_ld_s_w_p,
		     TOP_c2_ld_s_w,
		     TOP_c2_st_v_b,
		     TOP_c2_st_v_h,
		     TOP_c2_st_v_w,
		     TOP_c2_st_v_m_b,
		     TOP_c2_st_v_m_h,
		     TOP_c2_st_v_m_w,
		     TOP_c2_st_s_h,
		     TOP_c2_st_s_h_p,
		     TOP_c2_st_s_w,
		     TOP_c2_st_s_w_p,
		     TOP_c2_ldi_s_h_u,
		     TOP_c2_ldi_s_h,
		     TOP_c2_ldi_s_w,
		     TOP_c2_ldi_c,
		     TOP_c2_ldi_v_b_u,
		     TOP_c2_ldi_v_b,
		     TOP_c2_ldi_v_h,
		     TOP_c2_ldi_v_w,
		     TOP_c2_ldi_v_m_b_u,
		     TOP_c2_ldi_v_m_b,
		     TOP_c2_ldi_v_m_h,
		     TOP_c2_ldi_v_m_w,
		     TOP_c2_sti_v_b,
		     TOP_c2_sti_v_h,
		     TOP_c2_sti_v_w,
		     TOP_c2_sti_v_m_b,
		     TOP_c2_sti_v_m_h,
		     TOP_c2_sti_v_m_w,
		     TOP_c2_sti_c,
		     TOP_c2_sti_s_h,
		     TOP_c2_sti_s_w,
		     TOP_c2_vadds_h,
		     TOP_c2_vadds_w,
		     TOP_c2_vadds_p,
		     TOP_c2_vadds_h_mode6,
		     TOP_c2_vadds_h_mode2,
		     TOP_c2_vadds_w_mode6,
		     TOP_c2_vadds_w_mode2,
		     TOP_c2_vadds_p_mode6,
		     TOP_c2_vadds_p_mode2,
		     TOP_c2_vsubs_h,
		     TOP_c2_vsubs_h_sm,
		     TOP_c2_vsubs_h_abs,
		     TOP_c2_vsubs_h_abs_sm,
		     TOP_c2_vabs_h,
		     TOP_c2_vabs_h_sm,
		     TOP_c2_vsubs_w,
		     TOP_c2_vsubs_w_sm,
		     TOP_c2_vsubs_w_abs,
		     TOP_c2_vsubs_w_abs_sm,
		     TOP_c2_vabs_w,
		     TOP_c2_vabs_w_sm,
		     TOP_c2_vsubs_p,
		     TOP_c2_vsubs_p_sm,
		     TOP_c2_vsubs_p_abs,
		     TOP_c2_vsubs_p_abs_sm,
		     TOP_c2_vabs_p,
		     TOP_c2_vabs_p_sm,
		     TOP_c2_vmul_h,
		     TOP_c2_vmul_w,
		     TOP_c2_vneg_h,
		     TOP_c2_vneg_w,
		     TOP_c2_vneg_p,
		     TOP_c2_vshr_p,
		     TOP_c2_vshr_h,
		     TOP_c2_vshr_w,
		     TOP_c2_vshl_p,
		     TOP_c2_vshl_h,
		     TOP_c2_vshl_w,
		     TOP_c2_vclp,
		     TOP_c2_vclp_p,
		     TOP_c2_vclp_a,
		     TOP_c2_vclp_s,
		     TOP_c2_vclp_2,
		     TOP_c2_vclp_n,
		     TOP_c2_vclg_h_lt_and,
		     TOP_c2_vclg_h_lt_or,
		     TOP_c2_vclg_h_le_and,
		     TOP_c2_vclg_h_le_or,
		     TOP_c2_vclg_h_eq_and,
		     TOP_c2_vclg_h_eq_or,
		     TOP_c2_vclg_h_ge_and,
		     TOP_c2_vclg_h_ge_or,
		     TOP_c2_vclg_h_gt_and,
		     TOP_c2_vclg_h_gt_or,
		     TOP_c2_vclg_h_and,
		     TOP_c2_vclg_h_or,
		     TOP_c2_vclg_h_le,
		     TOP_c2_vclg_h_lt,
		     TOP_c2_vclg_h_ge,
		     TOP_c2_vclg_h_gt,
		     TOP_c2_vclg_w_lt_and,
		     TOP_c2_vclg_w_lt_or,
		     TOP_c2_vclg_w_le_and,
		     TOP_c2_vclg_w_le_or,
		     TOP_c2_vclg_w_eq_and,
		     TOP_c2_vclg_w_eq_or,
		     TOP_c2_vclg_w_ge_and,
		     TOP_c2_vclg_w_ge_or,
		     TOP_c2_vclg_w_gt_and,
		     TOP_c2_vclg_w_gt_or,
		     TOP_c2_vclg_w_and,
		     TOP_c2_vclg_w_or,
		     TOP_c2_vclg_w_le,
		     TOP_c2_vclg_w_lt,
		     TOP_c2_vclg_w_ge,
		     TOP_c2_vclg_w_gt,
		     TOP_c2_vclg_p_lt_and,
		     TOP_c2_vclg_p_lt_or,
		     TOP_c2_vclg_p_le_and,
		     TOP_c2_vclg_p_le_or,
		     TOP_c2_vclg_p_eq_and,
		     TOP_c2_vclg_p_eq_or,
		     TOP_c2_vclg_p_ge_and,
		     TOP_c2_vclg_p_ge_or,
		     TOP_c2_vclg_p_gt_and,
		     TOP_c2_vclg_p_gt_or,
		     TOP_c2_vclg_p_and,
		     TOP_c2_vclg_p_or,
		     TOP_c2_vclg_p_le,
		     TOP_c2_vclg_p_eq,
		     TOP_c2_vclg_p_ge,
		     TOP_c2_vclg_p_gt,
		     TOP_c2_vcmov_h_f,
		     TOP_c2_vcmov_h_t,
		     TOP_c2_vcmov_w_f,
		     TOP_c2_vcmov_w_t,
		     TOP_c2_lczero_z,
		     TOP_c2_lczero_nz_fw,
		     TOP_c2_lczero_nz_bw,
		     TOP_c2_vrnd_h,
		     TOP_c2_vrnd_w,
		     TOP_c2_vspas,
		     TOP_c2_vspel_mul_h,
		     TOP_c2_vspel_mul_w,
		     TOP_c2_vspel_adds,
		     TOP_c2_vspel_mac_h,
		     TOP_c2_vspel_mac_w,
		     TOP_c2_mmul_h,
		     TOP_c2_mmul_w,
		     TOP_c2_vmov,
		     TOP_c2_vmov_swin,
		     TOP_c2_vcopy, 
		     TOP_c2_vcmpr_h_eq,
		     TOP_c2_vcmpr_h_lt,
		     TOP_c2_vcmpr_h_le,
		     TOP_c2_vcmpr_h_gt,
		     TOP_c2_vcmpr_h_ge,
		     TOP_c2_vcmpr_w_eq,
		     TOP_c2_vcmpr_w_lt,
		     TOP_c2_vcmpr_w_le,
		     TOP_c2_vcmpr_w_gt,
		     TOP_c2_vcmpr_w_ge,
		     TOP_c2_sad, 
		     TOP_c2_satd, 
		     TOP_c2_intra,
		     TOP_c2_intra_0_1_9_14_16,
		     TOP_c2_intra_2_3_8_10,
		     TOP_c2_intra_4,
		     TOP_c2_intra_5_11,
		     TOP_c2_intra_6,
		     TOP_c2_intra_7,
		     TOP_c2_intra_12_13, 
		     TOP_c2_intra_15_17, 
		     TOP_c2_mvsel_mode0,   
		     TOP_c2_mvsel_mode1,
		     TOP_c2_mvsel_mode2,   
		     TOP_c2_mvsel_mode345,
		     TOP_c2_bcst_q,
		     TOP_c2_bcst_i,
		     TOP_c2_vlcs_dc,
		     TOP_c2_vlcs_ac,
		     TOP_c2_vlcs_wb,
		     TOP_c2_add_shl_g_i,
		     TOP_c2_add_shr_g_i,
		     TOP_c2_add_shl_g,
		     TOP_c2_add_shr_g,
		     TOP_c2_add_shl_r_h_i,
		     TOP_c2_add_shr_r_h_i,
		     TOP_c2_add_shl_r_w_i,
		     TOP_c2_add_shr_r_w_i,
		     TOP_c2_add_shl_r_h,
		     TOP_c2_add_shr_r_h,
		     TOP_c2_add_shl_r_w,
		     TOP_c2_add_shr_r_w,
		     TOP_c2_sub_g_abs_i,
		     TOP_c2_subs_g_i,
		     TOP_c2_sub_g_abs,
		     TOP_c2_subs_g,
		     TOP_c2_subs_r_h_i,
		     TOP_c2_subs_r_w_i,
		     TOP_c2_sub_r_abs_h_i,
		     TOP_c2_sub_r_abs_w_i,
		     TOP_c2_subs_r_h,
		     TOP_c2_subs_r_w,
		     TOP_c2_sub_r_abs_h,
		     TOP_c2_sub_r_abs_w,
		     TOP_c2_muls,
		     TOP_c2_mads,
		     TOP_c2_smads,
		     TOP_c2_min,
		     TOP_c2_max,
		     TOP_c2_cmov,
		     TOP_c2_mov_g,
		     TOP_c2_mov_r,
		     TOP_c2_mov_c_i,
		     TOP_c2_mov_c,
		     TOP_c2_mov_s_i, 
		     TOP_c2_mov_s, 
		     TOP_c2_clp,
		     TOP_c2_clp_i,
		     TOP_c2_chkrng,
		     TOP_c2_scond_r_h_wb_eq,
		     TOP_c2_scond_r_h_wb_lt,
		     TOP_c2_scond_r_h_wb_le,
		     TOP_c2_scond_r_h_wb_gt,
		     TOP_c2_scond_r_h_wb_ge,
		     TOP_c2_scond_r_wb_eq_i,
		     TOP_c2_scond_r_wb_lt_i,
		     TOP_c2_scond_r_wb_le_i,
		     TOP_c2_scond_r_wb_gt_i,
		     TOP_c2_scond_r_wb_ge_i,
		     TOP_c2_scond_r_w_wb_eq,
		     TOP_c2_scond_r_w_wb_lt,
		     TOP_c2_scond_r_w_wb_le,
		     TOP_c2_scond_r_w_wb_gt,
		     TOP_c2_scond_r_w_wb_ge,
		     TOP_c2_scond_r_w_wb_eq_i,
		     TOP_c2_scond_r_w_wb_lt_i,
		     TOP_c2_scond_r_w_wb_le_i,
		     TOP_c2_scond_r_w_wb_gt_i,
		     TOP_c2_scond_r_w_wb_ge_i,
		     TOP_c2_scond_r_h_eq,
		     TOP_c2_scond_r_h_lt,
		     TOP_c2_scond_r_h_le,
		     TOP_c2_scond_r_h_gt,
		     TOP_c2_scond_r_h_ge,
		     TOP_c2_scond_r_h_eq_i,
		     TOP_c2_scond_r_h_lt_i,
		     TOP_c2_scond_r_h_le_i,
		     TOP_c2_scond_r_h_gt_i,
		     TOP_c2_scond_r_h_ge_i,
		     TOP_c2_scond_r_w_eq,
		     TOP_c2_scond_r_w_lt,
		     TOP_c2_scond_r_w_le,
		     TOP_c2_scond_r_w_gt,
		     TOP_c2_scond_r_w_ge,
		     TOP_c2_scond_r_w_eq_i,
		     TOP_c2_scond_r_w_lt_i,
		     TOP_c2_scond_r_w_le_i,
		     TOP_c2_scond_r_w_gt_i,
		     TOP_c2_scond_r_w_ge_i,
		     TOP_c2_scond_eq,
		     TOP_c2_scond_lt,
		     TOP_c2_scond_le,
		     TOP_c2_scond_gt,
		     TOP_c2_scond_ge,
		     TOP_c2_scond_eq_i,
		     TOP_c2_scond_lt_i,
		     TOP_c2_scond_le_i,
		     TOP_c2_scond_gt_i,
		     TOP_c2_scond_ge_i,
		     TOP_c2_bop_ls,
		     TOP_c2_bop_rs,
		     TOP_c2_bop_and,
		     TOP_c2_bop_or,
		     TOP_c2_bop_andxor,
		     TOP_c2_bop_ls_i,
		     TOP_c2_bop_rs_i,
		     TOP_c2_bop_and_i,
		     TOP_c2_bop_or_i,
		     TOP_c2_bop_xor_i,
		     TOP_c2_bdep_l,
		     TOP_c2_bdep_m,
		     TOP_c2_bxtr_u_l,
		     TOP_c2_bxtr_s_l,
		     TOP_c2_bxtr_u_m,
		     TOP_c2_bxtr_s_m,
		     TOP_c2_sum4_c,
		     TOP_c2_sum4_g,
		     TOP_c2_sum4_sw,
		     TOP_c2_sum4_r,
		     TOP_c2_sum4_saddr,    
		     TOP_c2_med,
		     TOP_c2_fork_m,
		     TOP_c2_fork_n ,
		     TOP_c2_joint,
		     TOP_c2_ld_v2g_b_u,
		     TOP_c2_ld_v2g_b,
		     TOP_c2_ld_v2g_h_u, 
		     TOP_c2_ld_v2g_h,   
		     TOP_c2_ld_v2g_w,   
		     TOP_c2_st_g2v_b, 
		     TOP_c2_st_g2v_h, 
		     TOP_c2_st_g2v_w, 
		     TOP_c2_ldi_v2g_b_u, 
		     TOP_c2_ldi_v2g_b,   
		     TOP_c2_ldi_v2g_h_u, 
		     TOP_c2_ldi_v2g_h,   
		     TOP_c2_ldi_v2g_w,   
		     TOP_c2_sti_g2v_b,   
		     TOP_c2_sti_g2v_h,   
		     TOP_c2_sti_g2v_w,
		     TOP_c2_gsums,
		     TOP_c2_wrap,
		     TOP_c2_clzob_zd, 
		     TOP_c2_clzob_od, 
		     TOP_c2_clzob_zd_i, 
		     TOP_c2_clzob_od_i, 
		     TOP_c2_thctrl_lock,
		     TOP_c2_thctrl_unlock,
		     TOP_c2_thctrl_deact,
		     TOP_c2_thctrl_act,
		     TOP_c2_thctrl_mode4, 
		     TOP_c2_thctrl_mode5,
		     TOP_c2_thctrl_mode6,
		     TOP_c2_thctrl_mode7,
                     TOP_c2_shor_l,
                     TOP_c2_shor_rl, 
                     TOP_c2_shor_ra, 
                     TOP_c2_shadd_l, 
                     TOP_c2_shadd_rl,
                     TOP_c2_shadd_ra,
                     TOP_c2_shsub_l,
                     TOP_c2_shsub_rl, 
                     TOP_c2_shsub_ra, 
                     TOP_c2_shor_l_i,
                     TOP_c2_shor_rl_i, 
                     TOP_c2_shor_ra_i, 
                     TOP_c2_shadd_l_i, 
                     TOP_c2_shadd_rl_i,
                     TOP_c2_shadd_ra_i,
                     TOP_c2_shsub_l_i,
                     TOP_c2_shsub_rl_i, 
                     TOP_c2_shsub_ra_i, 
		     TOP_UNDEFINED);
#endif
  /* ===== Instructions with side effects ====== */
  side_effects = ISA_Property_Create ("side_effects");
  Instruction_Group (side_effects,
		  /* Because asm macros can trash memory, we conservatively 
		     mark this property so that the compiler doesn't move
		     instructions around it. */
		  TOP_asm, 
		  TOP_sync,
#ifdef TARG_SL
		  TOP_c3_viterbi,
		  TOP_c3_trback,
		  TOP_c3_fft,
                  // new C3
	          TOP_C3_ffe,
#endif 
#ifdef TARG_SL2
		  TOP_c2_add_shl_g_i,
		  TOP_c2_add_shr_g_i,
		  TOP_c2_add_shl_g,
		  TOP_c2_add_shr_g,
		  TOP_c2_mvsel_mode0,   
		  TOP_c2_mvsel_mode1,
		  TOP_c2_mvsel_mode2,   
		  TOP_c2_mvsel_mode345,
		  TOP_c2_vlcs_wb,
		  TOP_c2_muls,
		  TOP_c2_mads,
		  TOP_c2_smads,
		  TOP_c2_min,
		  TOP_c2_max,
		  TOP_c2_sum4_g,
		  TOP_c2_sum4_sw,
		  TOP_c2_sum4_saddr, 
		  TOP_c2_mov_g,
		  TOP_c2_chkrng,
		  TOP_c2_bdep_l,
		  TOP_c2_bdep_m,
		  TOP_c2_bxtr_u_l,
		  TOP_c2_bxtr_s_l,
		  TOP_c2_bxtr_u_m,
		  TOP_c2_bxtr_s_m,
		  TOP_c2_cmov,
		  TOP_c2_med,
		  TOP_c2_clp,
		  TOP_c2_clp_i,
		  TOP_c2_scond_r_h_wb_eq,
		  TOP_c2_scond_r_h_wb_lt,
		  TOP_c2_scond_r_h_wb_le,
		  TOP_c2_scond_r_h_wb_gt,
		  TOP_c2_scond_r_h_wb_ge,
		  TOP_c2_scond_r_wb_eq_i,
		  TOP_c2_scond_r_wb_lt_i,
		  TOP_c2_scond_r_wb_le_i,
		  TOP_c2_scond_r_wb_gt_i,
		  TOP_c2_scond_r_wb_ge_i,
		  TOP_c2_scond_r_w_wb_eq,
		  TOP_c2_scond_r_w_wb_lt,
		  TOP_c2_scond_r_w_wb_le,
		  TOP_c2_scond_r_w_wb_gt,
		  TOP_c2_scond_r_w_wb_ge,                   
		  TOP_c2_scond_r_w_wb_eq_i,
		  TOP_c2_scond_r_w_wb_lt_i,
		  TOP_c2_scond_r_w_wb_le_i,
		  TOP_c2_scond_r_w_wb_gt_i,
		  TOP_c2_scond_r_w_wb_ge_i,                   
		  TOP_c2_scond_r_h_eq,
		  TOP_c2_scond_r_h_lt,
		  TOP_c2_scond_r_h_le,
		  TOP_c2_scond_r_h_gt,
		  TOP_c2_scond_r_h_ge,
		  TOP_c2_scond_r_h_eq_i,
		  TOP_c2_scond_r_h_lt_i,
		  TOP_c2_scond_r_h_le_i,
		  TOP_c2_scond_r_h_gt_i,
		  TOP_c2_scond_r_h_ge_i,
		  TOP_c2_scond_r_w_eq,
		  TOP_c2_scond_r_w_lt,
		  TOP_c2_scond_r_w_le,
		  TOP_c2_scond_r_w_gt,
		  TOP_c2_scond_r_w_ge,
		  TOP_c2_scond_r_w_eq_i,
		  TOP_c2_scond_r_w_lt_i,
		  TOP_c2_scond_r_w_le_i,
		  TOP_c2_scond_r_w_gt_i,
		  TOP_c2_scond_r_w_ge_i,
		  TOP_c2_scond_eq,
		  TOP_c2_scond_lt,
		  TOP_c2_scond_le,
		  TOP_c2_scond_gt,
		  TOP_c2_scond_ge,
		  TOP_c2_scond_eq_i,
		  TOP_c2_scond_lt_i,
		  TOP_c2_scond_le_i,
		  TOP_c2_scond_gt_i,
		  TOP_c2_scond_ge_i,
		  TOP_c2_bop_ls,
		  TOP_c2_bop_rs,
		  TOP_c2_bop_and,
		  TOP_c2_bop_or,
		  TOP_c2_bop_xor,
		  TOP_c2_bop_andxor,
		  TOP_c2_bop_ls_i,
		  TOP_c2_bop_rs_i,
		  TOP_c2_bop_and_i,
		  TOP_c2_bop_or_i,
		  TOP_c2_bop_xor_i,
		  TOP_c2_satd, 
		  TOP_c2_sad, 
		  TOP_c2_gsums,
		  TOP_c2_wrap,
		  TOP_c2_clzob_zd, 
		  TOP_c2_clzob_od, 
		  TOP_c2_clzob_zd_i, 
		  TOP_c2_clzob_od_i, 
		  TOP_c2_thctrl_lock,
		  TOP_c2_thctrl_unlock,
		  TOP_c2_thctrl_deact,
		  TOP_c2_thctrl_act,
		  TOP_c2_thctrl_mode4, 
		  TOP_c2_thctrl_mode5,
		  TOP_c2_thctrl_mode6,
		  TOP_c2_thctrl_mode7,
		  TOP_c2_joint, 
                  TOP_c2_shor_l,
                  TOP_c2_shor_rl,  
                  TOP_c2_shor_ra,  
                  TOP_c2_shadd_l,      
                  TOP_c2_shadd_rl,   
                  TOP_c2_shadd_ra,   
                  TOP_c2_shsub_l,
                  TOP_c2_shsub_rl, 
                  TOP_c2_shsub_ra, 
#endif
		  TOP_UNDEFINED);

  /* ===== Instructions with branch predictions ====== */
  side_effects = ISA_Property_Create ("branch_predict");
  Instruction_Group (branch_predict,
		  TOP_UNDEFINED);

  /* ===== Instructions with variable number of operands/results ====== */
  var_opnds = ISA_Property_Create ("var_opnds");
  Instruction_Group (var_opnds,
                     TOP_asm,
#ifdef TARG_SL2
                     TOP_c2_ldi_v_m_b_u,
                     TOP_c2_ldi_v_m_b,
                     TOP_c2_ldi_v_m_h,
                     TOP_c2_ldi_v_m_w,
                     TOP_c2_sti_v_m_b,
                     TOP_c2_sti_v_m_h,
                     TOP_c2_sti_v_m_w,
#endif
		     TOP_UNDEFINED);

/* ===== Instructions that update 'base' operand ====== */
  base_update = ISA_Property_Create ("base_update");
  Instruction_Group (base_update,
		     TOP_UNDEFINED);

/* ===== Instructions that mark the head of a zero-cost loop ====== */
  loop_start = ISA_Property_Create ("loop_start");
  Instruction_Group (loop_start,
		     TOP_UNDEFINED);

#ifdef TARG_SL 
/* ===== Operator defines FP CC reg ====== */
  defs_copc = ISA_Property_Create ("defs_copc");
  Instruction_Group (defs_copc,
#ifdef TARG_SL2
                     TOP_c2_scond_r_h_wb_eq,
		     TOP_c2_scond_r_h_wb_lt,
		     TOP_c2_scond_r_h_wb_le,
		     TOP_c2_scond_r_h_wb_gt,
		     TOP_c2_scond_r_h_wb_ge,
		     TOP_c2_scond_r_wb_eq_i,
		     TOP_c2_scond_r_wb_lt_i,
		     TOP_c2_scond_r_wb_le_i,
		     TOP_c2_scond_r_wb_gt_i,
		     TOP_c2_scond_r_wb_ge_i,
		     TOP_c2_scond_r_w_wb_eq,
		     TOP_c2_scond_r_w_wb_lt,
		     TOP_c2_scond_r_w_wb_le,
		     TOP_c2_scond_r_w_wb_gt,
		     TOP_c2_scond_r_w_wb_ge,
		     TOP_c2_scond_r_w_wb_eq_i,
		     TOP_c2_scond_r_w_wb_lt_i,
		     TOP_c2_scond_r_w_wb_le_i,
		     TOP_c2_scond_r_w_wb_gt_i,
		     TOP_c2_scond_r_w_wb_ge_i,
		     TOP_c2_scond_r_h_eq,
		     TOP_c2_scond_r_h_lt,
                     TOP_c2_scond_r_h_le,
                     TOP_c2_scond_r_h_gt,
                     TOP_c2_scond_r_h_ge,
                     TOP_c2_scond_r_h_eq_i,
                     TOP_c2_scond_r_h_lt_i,
                     TOP_c2_scond_r_h_le_i,
                     TOP_c2_scond_r_h_gt_i,
                     TOP_c2_scond_r_h_ge_i,
                     TOP_c2_scond_r_w_eq,
                     TOP_c2_scond_r_w_lt,
                     TOP_c2_scond_r_w_le,
                     TOP_c2_scond_r_w_gt,
                     TOP_c2_scond_r_w_ge,
                     TOP_c2_scond_r_w_eq_i,
                     TOP_c2_scond_r_w_lt_i,
                     TOP_c2_scond_r_w_le_i,
                     TOP_c2_scond_r_w_gt_i,
                     TOP_c2_scond_r_w_ge_i,
                     TOP_c2_scond_eq,
                     TOP_c2_scond_lt,
                     TOP_c2_scond_le,
                     TOP_c2_scond_gt,
                     TOP_c2_scond_ge,
                     TOP_c2_scond_eq_i,
                     TOP_c2_scond_lt_i,
                     TOP_c2_scond_le_i,
                     TOP_c2_scond_gt_i,
                     TOP_c2_scond_ge_i,
                     

#endif 
                     TOP_UNDEFINED);

#ifdef TARG_SL2 //fork_joint
fork_instr  = ISA_Property_Create("fork_instr");
Instruction_Group(fork_instr,  
	            TOP_c2_fork_m, 
	            TOP_c2_fork_n, 
	            TOP_UNDEFINED);
#endif 
#endif 

#ifdef TARG_SL
 use_ctrl_reg = ISA_Property_Create("use_ctrl_reg");
 Instruction_Group(use_ctrl_reg,
 	             TOP_mvfc,
 	             TOP_mvfc16,
 	             TOP_ret,
 	             TOP_ret16,
 	             TOP_jr16,
 	             TOP_jr,
 	             TOP_jr16_lnk,
 	             TOP_jalr,
 	             TOP_loop,
 	             TOP_UNDEFINED);
#endif
  ISA_Properties_End();
  return 0;
}
