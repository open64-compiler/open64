/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_flags.c
 *  $Revision: 1.84 $
 *  $Date: 06/01/19 16:18:29-08:00 $
 *  $Author: fchow@fluorspar.internal.keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_flags.cxx $
 *
 *  Description:
 *  ============
 *
 *  Definition of variables global to all of CG.
 *
 * =======================================================================
 * =======================================================================
 */


#include <stdint.h>
#include "defs.h"
#include "config_targ.h"
#include "cg_flags.h"
#include "cgtarget.h"

BOOL CG_warn_bad_freqs = FALSE;
INT32 CG_skip_after = INT32_MAX;
INT32 CG_skip_before = 0;
INT32 CG_skip_equal = -1;
INT32 CG_local_skip_after = INT32_MAX;
INT32 CG_local_skip_before = 0;
INT32 CG_local_skip_equal = -1;
#ifdef TARG_SL
INT32 CG_local_sched_pu_skip_before = -1;
INT32 CG_local_sched_pu_skip_after  = -1 ;
INT32 CG_local_sched_pu_skip_equal = -1;
INT32 CG_local_sched_bb_skip_before = -1;
INT32 CG_local_sched_bb_skip_after  = -1 ;
INT32 CG_local_sched_bb_skip_equal = -1;
INT32 CG_local_sched_op_skip_after  = -1 ;
INT32 CG_bb_sched_op_num_max = -1;
/* GCM flags */
INT32 CG_GCM_skip_before = -1;
INT32 CG_GCM_skip_after  = -1;
INT32 CG_GCM_skip_equal  = -1;
INT32 CG_GCM_loop_skip_before = -1;
INT32 CG_GCM_loop_skip_after  = -1;
INT32 CG_GCM_loop_skip_equal  = -1;
INT32 CG_GCM_op_skip_before = -1;
INT32 CG_GCM_op_skip_after  = -1;
INT32 CG_GCM_op_skip_equal  = -1;
INT32 CG_GCM_LICM_loop_skip_before = -1;
INT32 CG_GCM_LICM_loop_skip_after = -1;
INT32 CG_GCM_LICM_loop_skip_equal = -1;
INT32 CG_GCM_LICM_op_skip_before = -1;
INT32 CG_GCM_LICM_op_skip_after = -1;
INT32 CG_GCM_LICM_op_skip_equal = -1;
INT32 CG_LOOP_DCE_loop_skip_before = -1;
INT32 CG_LOOP_DCE_loop_skip_after = -1;
INT32 CG_LOOP_DCE_loop_skip_equal = -1;
INT32 CG_LOOP_DCE_op_skip_before = -1;
INT32 CG_LOOP_DCE_op_skip_after = -1;
INT32 CG_LOOP_DCE_op_skip_equal = -1;

BOOL CG_GCM_enable_critical_edge_motion = FALSE;
BOOL CG_GCM_enable_mvtc_optimization = TRUE;
BOOL CG_GCM_enable_reduce_loop_count = FALSE;
BOOL CG_GCM_enable_licm = FALSE;
BOOL CG_GCM_enable_dce = FALSE;
BOOL CG_GCM_enable_rce = FALSE;
BOOL CG_GCM_enable_break_dependence = FALSE;
BOOL CG_GCM_enable_merge_small_bbs = FALSE;
#endif 
BOOL CG_skip_local_hbf = FALSE;
BOOL CG_skip_local_loop = FALSE;
BOOL CG_skip_local_sched = FALSE;
BOOL CG_skip_local_swp = FALSE;
#ifdef TARG_X8664
BOOL CG_cmp_load_exec = FALSE;
BOOL CG_fma3_load_exec = FALSE;
BOOL CG_fma4_load_exec = FALSE;
BOOL CG_128bitstore = TRUE;
BOOL CG_branch_fuse = TRUE;
BOOL CG_dispatch_schedule = FALSE;
BOOL CG_LOOP_nounroll_best_fit_set = FALSE;
BOOL CG_strcmp_expand = TRUE;
BOOL CG_merge_counters_x86 = FALSE;
BOOL CG_merge_counters_x86_set = FALSE;
BOOL CG_interior_ptrs_x86 = FALSE;
BOOL CG_NoClear_Avx_Simd = FALSE;
#endif
BOOL CG_opt_level;
BOOL CG_localize_tns = FALSE;
BOOL CG_localize_tns_Set = FALSE;
#ifdef TARG_X8664
BOOL CG_localize_x87_tns = FALSE;
BOOL CG_localize_x87_tns_Set = FALSE;
BOOL CG_x87_store = FALSE;
#endif
#ifdef TARG_IA64
BOOL CG_Enable_Ldxmov_Support = TRUE;
#endif
BOOL LOCALIZE_using_stacked_regs = TRUE;
BOOL CG_unique_exit = TRUE;

BOOL CG_enable_feedback	     = TRUE;
BOOL CG_enable_reverse_if_conversion = TRUE;
BOOL CG_enable_reverse_if_conversion_overridden = FALSE;
BOOL CG_enable_spec_imul = TRUE;
BOOL CG_enable_spec_idiv = FALSE;
BOOL CG_enable_spec_fdiv = TRUE;
BOOL CG_enable_spec_fsqrt = TRUE;
BOOL CG_enable_spec_imul_overridden = FALSE;
BOOL CG_enable_spec_idiv_overridden = FALSE;
BOOL CG_enable_spec_fdiv_overridden = FALSE;
BOOL CG_enable_spec_fsqrt_overridden = FALSE;
BOOL CG_create_madds = TRUE;
INT32 CG_maxinss = CG_maxinss_default;
INT32 CG_maxblocks = 30;
BOOL GRA_LIVE_Phase_Invoked = FALSE;
BOOL CFLOW_Enable = TRUE;
BOOL CFLOW_opt_before_cgprep = TRUE;
BOOL CFLOW_opt_after_cgprep = TRUE;
BOOL CFLOW_Enable_Unreachable = TRUE;
BOOL CFLOW_Enable_Branch = TRUE;
BOOL CFLOW_Enable_Merge = TRUE;
BOOL CFLOW_Enable_Reorder = FALSE;
BOOL CFLOW_Enable_Freq_Order = TRUE;
#ifdef KEY
BOOL CFLOW_Enable_Freq_Order_On_Heuristics = FALSE;
#endif
BOOL CFLOW_Enable_Clone = TRUE;
BOOL CFLOW_opt_all_br_to_bcond = FALSE;
const char *CFLOW_heuristic_tolerance;
const char *CFLOW_feedback_tolerance;
UINT32 CFLOW_clone_incr=10;
UINT32 CFLOW_clone_min_incr = 15;
UINT32 CFLOW_clone_max_incr = 100;
const char *CFLOW_cold_threshold;

BOOL FREQ_enable = TRUE;
BOOL FREQ_view_cfg = FALSE;
const char *FREQ_frequent_never_ratio = "1000.0";
const char *FREQ_eh_freq = "0.1";
#ifdef KEY
const char *FREQ_non_local_targ_freq = "0.1";
#endif

BOOL CG_enable_rename = TRUE;
BOOL CG_enable_prefetch = FALSE;
BOOL CG_enable_z_conf_prefetch  = FALSE;
BOOL CG_enable_nz_conf_prefetch = FALSE;
BOOL CG_enable_pf_L1_ld = FALSE;
BOOL CG_enable_pf_L1_st = FALSE;
BOOL CG_enable_pf_L2_ld = FALSE;
BOOL CG_enable_pf_L2_st = FALSE;
BOOL CG_exclusive_prefetch = FALSE;

INT32 CG_L1_ld_latency = 0;
INT32 CG_L2_ld_latency = 0;
INT32 CG_z_conf_L1_ld_latency = 0;
INT32 CG_z_conf_L2_ld_latency = 0;
INT32 CG_ld_latency = 0;
INT32 CG_L1_pf_latency = 12;
INT32 CG_L2_pf_latency = 12;

INT32 CG_branch_mispredict_penalty= -1;		/* means not set */
INT32 CG_branch_mispredict_factor= -1;		/* means not set */

BOOL CGSPILL_Rematerialize_Constants = TRUE;
BOOL CGSPILL_Enable_Force_Rematerialization = FALSE;

#ifdef TARG_IA64
BOOL CG_enable_thr = TRUE;
BOOL CG_cond_defs_allowed = TRUE;
BOOL LOCS_Enable_Bundle_Formation = TRUE;
BOOL CG_tail_call = FALSE;
BOOL GCM_Speculative_Loads = FALSE;
BOOL GCM_Predicated_Loads = TRUE;
BOOL CG_tune_do_loop = TRUE;
#else
BOOL CG_enable_thr = FALSE;
BOOL CG_cond_defs_allowed = FALSE;
BOOL LOCS_Enable_Bundle_Formation = FALSE;
#if defined(TARG_LOONGSON) || defined(TARG_MIPS) && !defined(TARG_SL)   
BOOL CG_tail_call = FALSE;
#elif defined(TARG_PPC32)
BOOL CG_tail_call = FALSE;// we have some problem with struct value parameter tail call OPT
#else
BOOL CG_tail_call = TRUE;
#endif
BOOL GCM_Speculative_Loads = FALSE;
BOOL GCM_Predicated_Loads = FALSE;
#endif
UINT32 LOCS_PRE_Enable_Minreg_Level = 0;
BOOL LOCS_PRE_Enable_General_RegPressure_Sched = FALSE;
BOOL LOCS_PRE_Enable_Unroll_RegPressure_Sched = FALSE;
BOOL LOCS_PRE_Enable_Scheduling = TRUE;
BOOL LOCS_POST_Enable_Scheduling = TRUE;
BOOL LOCS_Enable_Scheduling = TRUE;
BOOL IGLS_Enable_HB_Scheduling = TRUE;
BOOL IGLS_Enable_PRE_HB_Scheduling = FALSE;
BOOL IGLS_Enable_POST_HB_Scheduling = TRUE;
BOOL IGLS_Enable_All_Scheduling = TRUE;
#ifdef TARG_SL
BOOL RGN_Enable_All_Scheduling = TRUE;
const char* App_Name;
const char* Cand_List_Pattern = "mi"; 
BOOL CG_SL2_enable_combine_condmv = TRUE;
BOOL CG_SL2_enable_peephole = TRUE;
BOOL CG_SL2_enable_v1buf_expansion = TRUE;
BOOL CG_Enable_Macro_Instr_Combine = TRUE;

INT32 CG_zdl_enabled_level = 2; // default value is 2
INT32 CG_max_zdl_level = 4;
BOOL CG_enable_opt_condmv = TRUE;
BOOL CG_enable_CBUS_workaround = FALSE;
BOOL CG_enable_LD_NOP_workaround = FALSE;
BOOL CG_enbale_C3_AR_dependence_workaround = FALSE;
#endif
#ifdef TARG_LOONGSON
BOOL CG_Enable_RVI_2_CG = FALSE;
BOOL CG_enable_del_base_tn=FALSE;
BOOL CG_enable_auto_add_op=FALSE;
BOOL CG_enable_del_auto_add_op=FALSE;
BOOL CG_enable_float_pointer_example=FALSE;
BOOL CG_enable_too_many_spill=FALSE;
BOOL CG_enable_improve_fp_efficiency=FALSE;
BOOL CG_enable_all_ldst_is_lbsb=FALSE;
BOOL CG_enable_improve_icache_efficiency=FALSE;
#endif
BOOL CG_enable_loop_optimizations = TRUE;
BOOL GCM_Motion_Across_Calls = TRUE;
BOOL GCM_Min_Reg_Usage = TRUE;
BOOL GCM_Pointer_Spec= TRUE;
BOOL GCM_Eager_Ptr_Deref = TRUE;
BOOL GCM_Test = FALSE;
BOOL GCM_Enable_Cflow = TRUE;
BOOL GCM_PRE_Enable_Scheduling = TRUE;
BOOL GCM_POST_Enable_Scheduling = TRUE;
BOOL GCM_Enable_Scheduling = TRUE;
BOOL CGTARG_Enable_Brlikely = TRUE;
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
BOOL Enable_Fill_Delay_Slots = FALSE;
BOOL GCM_Enable_Fill_Delay_Slots = FALSE;
BOOL CG_use_movlpd = TRUE;
BOOL CG_use_setcc = TRUE;
BOOL CG_use_short_form = FALSE;
UINT64 CG_p2align_freq = 10000;
UINT32 CG_p2align_max_skip_bytes = 3;
INT32 CG_movnti = 1000;
BOOL CG_use_incdec = TRUE;
BOOL CG_use_xortozero = TRUE; // bug 8592
BOOL CG_use_xortozero_Set = FALSE;
BOOL CG_use_test = FALSE;
BOOL CG_fold_shiftadd = FALSE;
BOOL CG_use_prefetchnta = FALSE;
BOOL CG_idivbyconst_opt = TRUE;
BOOL CG_fold_constimul = TRUE;
BOOL CG_LOOP_cloop = TRUE;
BOOL CG_use_lddqu = FALSE;
BOOL CG_push_pop_int_saved_regs = FALSE;
BOOL CG_push_pop_int_saved_regs_Set = FALSE;
UINT32 CG_ptr_load_use_latency = 4;
#else
#if defined(TARG_SL)
BOOL Enable_Fill_Delay_Slots = FALSE;
BOOL GCM_Enable_Fill_Delay_Slots = FALSE;
#else
BOOL Enable_Fill_Delay_Slots = TRUE;
BOOL GCM_Enable_Fill_Delay_Slots = TRUE;
#endif
#endif
const char *CGTARG_Branch_Taken_Prob = NULL;
double CGTARG_Branch_Taken_Probability;
BOOL CGTARG_Branch_Taken_Prob_overridden;

BOOL EMIT_pjump_all = TRUE;
BOOL EMIT_use_cold_section = TRUE;
BOOL EMIT_interface_section = TRUE;
BOOL EMIT_stop_bits_for_asm = TRUE;
BOOL EMIT_stop_bits_for_volatile_asm = FALSE;
BOOL EMIT_explicit_bundles = TRUE;
INT32 EMIT_Long_Branch_Limit = DEFAULT_LONG_BRANCH_LIMIT;

INT32 CGEXP_expandconstant = DEFAULT_CGEXP_CONSTANT;
BOOL CG_divrem_opt = FALSE;
BOOL CGEXP_use_copyfcc = TRUE;
BOOL CGEXP_normalize_logical = FALSE;
BOOL CGEXP_gp_prolog_call_shared = TRUE;
BOOL CGEXP_fast_imul = TRUE;
BOOL CGEXP_float_consts_from_ints = TRUE;
#if defined(TARG_SL)
BOOL CGEXP_cvrt_int_div_to_mult = FALSE;
BOOL CGEXP_cvrt_int_div_to_fdiv = FALSE;
BOOL CG_Gen_16bit = TRUE;
BOOL CG_Enable_br16 = TRUE;
INT32 CG_localsch_pre_size = 1;
BOOL CG_dsp_thread = FALSE; // dsp thread in sl1
BOOL CG_check_quadword = TRUE;
BOOL CG_rep_unpaired16 = TRUE;
BOOL CG_ignore_mem_alias = FALSE;
BOOL CG_stack_layout = TRUE;
INT32 CG_ISR = 1;
INT32 CG_Max_Accreg = 4;
INT32 CG_Max_Addreg = 8;
BOOL CG_round_spreg = TRUE;
BOOL CG_check_packed = TRUE;
BOOL CG_branch_taken = TRUE; //option for sl1
#else
BOOL CGEXP_cvrt_int_div_to_mult = TRUE;
BOOL CGEXP_cvrt_int_div_to_fdiv = TRUE;
#endif
BOOL CGEXP_opt_float_div_by_const = TRUE;
#ifdef KEY
BOOL CGEXP_cvrt_int_mult_to_add_shift = TRUE;
#endif

const char *CGEXP_lfhint_L1;
const char *CGEXP_lfhint_L2;
const char *CGEXP_ldhint_L1;
const char *CGEXP_ldhint_L2;
const char *CGEXP_sthint_L1;
const char *CGEXP_sthint_L2;

BOOL LRA_do_reorder = FALSE;
#ifdef TARG_SL2 
BOOL Enable_Checking_Register_Allocation = TRUE;
BOOL CG_sl2 = FALSE;
#endif 
#ifdef TARG_X8664
BOOL LRA_prefer_legacy_regs = FALSE;
#endif
#ifdef KEY
BOOL LRA_prefer_lru_reg = TRUE;		// bug 14303
BOOL LRA_prefer_lru_reg_Set = FALSE;
INT32 LRA_inflate_reg_request = 0;
INT32 LRA_inflate_reg_request_Set = FALSE;
#endif

BOOL GRA_use_old_conflict = FALSE;
BOOL GRA_shrink_wrap      = TRUE;
BOOL GRA_loop_splitting   = TRUE;
#ifdef TARG_IA64
BOOL GRA_home             = FALSE;
#else
BOOL GRA_home             = TRUE;
#endif
BOOL GRA_remove_spills    = TRUE;
BOOL GRA_preference_globals = TRUE;
BOOL GRA_preference_dedicated = TRUE;
BOOL GRA_preference_glue = TRUE;
BOOL GRA_preference_all = TRUE;
BOOL GRA_ensure_spill_proximity = TRUE;
BOOL GRA_choose_best_split = TRUE;
BOOL GRA_use_stacked_regs = TRUE;
BOOL GRA_redo_liveness = FALSE;
BOOL GRA_recalc_liveness = FALSE;
INT32 GRA_non_home_hi = -1;
INT32 GRA_non_home_lo = INT32_MAX;
const char* GRA_call_split_freq_string = "0.1";
const char* GRA_spill_count_factor_string = "0.5";
#ifdef KEY
BOOL GRA_exclude_callee_saved_regs = FALSE;
BOOL GRA_eh_exclude_callee_saved_regs = FALSE;
BOOL GRA_fp_exclude_callee_saved_regs = FALSE;
#endif

#ifdef KEY
// By default OFF, unless specified by the user
BOOL  HB_formation = FALSE;
INT32 HB_if_conversion_cut_off = 10;
#else
BOOL  HB_formation = TRUE;
#endif
#if defined(TARG_SL) || defined(TARG_IA64)
BOOL CG_Enable_REGION_formation = FALSE;
BOOL CG_Enable_Regional_Global_Sched = FALSE;
BOOL CG_Enable_Regional_Local_Sched = TRUE;
BOOL CG_Enable_Include_Memread_Arc = FALSE;
#endif

BOOL  HB_static_freq_heuristics = TRUE;
INT   HB_max_blocks = 20;
const char* HB_max_sched_growth = "4.1";
const char* HB_min_path_priority_ratio = "0.05";
const char* HB_min_priority = "0.002";
const char* HB_call_hazard_multiplier = "0.25";
const char* HB_memory_hazard_multiplier = "1.0";
const char* HB_base_probability_contribution = "0.1";
BOOL  HB_require_alias = TRUE;
BOOL  HB_loops = FALSE;
BOOL  HB_general_use_pq = FALSE;
BOOL  HB_general_from_top = FALSE;
BOOL  HB_allow_tail_duplication = FALSE;
BOOL  HB_exclude_calls = FALSE;
BOOL  HB_exclude_pgtns = TRUE;	// until bugs fixed
#ifdef TARG_IA64
BOOL  HB_skip_hammocks = TRUE;	// until bugs fixed
#else
BOOL  HB_skip_hammocks = FALSE;
#endif
BOOL  HB_simple_ifc = TRUE;
BOOL  HB_simple_ifc_set = FALSE;
INT   HB_min_blocks = 2;
BOOL  GRA_LIVE_Predicate_Aware = FALSE;

/* Recurrence Breaking flags */
#if defined(TARG_IA64) || defined(TARG_MIPS)
BOOL CG_LOOP_fix_recurrences = TRUE;
#else
// Disable fix recurrence because CG_DEF_Op_Opnd_Changed
//   is no longer supported for MIPS.  
//
BOOL CG_LOOP_fix_recurrences = FALSE;
#endif

BOOL CG_LOOP_fix_recurrences_specified = FALSE;
BOOL CG_LOOP_back_substitution = TRUE;
BOOL CG_LOOP_back_substitution_specified = FALSE;
BOOL CG_LOOP_back_substitution_variant = TRUE;
BOOL CG_LOOP_back_substitution_variant_specified = FALSE;
BOOL CG_LOOP_interleave_reductions = TRUE;
BOOL CG_LOOP_interleave_reductions_specified = FALSE;
BOOL CG_LOOP_interleave_posti = TRUE;
BOOL CG_LOOP_interleave_posti_specified = FALSE;
BOOL CG_LOOP_reassociate = TRUE;
BOOL CG_LOOP_reassociate_specified = FALSE;
INT32 CG_LOOP_recurrence_min_omega = 0;
#ifdef TARG_IA64
INT32 CG_LOOP_recurrence_max_omega = 16;  // ia64
#else
INT32 CG_LOOP_recurrence_max_omega = 4;   // mips
#endif
#ifdef KEY
BOOL LOCS_Best = FALSE;
BOOL LOCS_Best_set = FALSE;
BOOL LOCS_Fwd_Scheduling = FALSE;
BOOL LOCS_Fwd_Scheduling_set = FALSE;
UINT32 LOCS_Scheduling_Algorithm = 0;
BOOL LOCS_Scheduling_Algorithm_set = FALSE;
BOOL LOCS_Reduce_Prefetch = FALSE;
BOOL LOCS_Reduce_Prefetch_set = FALSE;
BOOL LOCS_Shallow_Depth = FALSE;
BOOL LOCS_Shallow_Depth_set = FALSE;
BOOL LOCS_Balance_Ready_Types = FALSE;
BOOL LOCS_Balance_Ready_Types_set = FALSE;
BOOL LOCS_Balance_Unsched_Types = FALSE;
BOOL LOCS_Balance_Unsched_Types_set = FALSE;
UINT32 LOCS_Balance_Ready_Int = 80;
BOOL LOCS_Balance_Ready_Int_set = FALSE;
UINT32 LOCS_Balance_Ready_Fp = 40;
BOOL LOCS_Balance_Ready_Fp_set = FALSE;
UINT32 LOCS_Balance_Unsched_Int = 40;
BOOL LOCS_Balance_Unsched_Int_set = FALSE;
UINT32 LOCS_Balance_Unsched_Fp = 60;
BOOL LOCS_Balance_Unsched_Fp_set = FALSE;
BOOL CG_min_spill_loc_size = FALSE;
#if defined(TARG_SL) || defined(TARG_IA64)
BOOL CG_min_stack_size = FALSE;
#else
BOOL CG_min_stack_size = TRUE;
#endif
BOOL flag_test_coverage = FALSE;
OPTION_LIST *Arc_Profile_Region = NULL;
INT32 CG_cse_regs = INT32_MAX - 1000;
INT32 CG_sse_cse_regs = INT32_MAX - 1000;
#endif
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
INT32 CG_sse_load_execute = 0;
INT32 CG_load_execute = 1;
BOOL CG_loadbw_execute = FALSE;
#if defined(TARG_X8664)
INT32 CG_p2align = 2;
#else
INT32 CG_p2align = 0;
#endif
BOOL CG_p2align_split = FALSE;
BOOL CG_loop32 = FALSE;
BOOL CG_compute_to = FALSE;
BOOL CG_valgrind_friendly = TRUE;
BOOL CG_Movext_ICMP = TRUE;
#else
INT32 CG_p2align = 0;
#endif
#ifdef TARG_LOONGSON
BOOL CGEXP_float_use_madd = FALSE;
BOOL CGEXP_use_Loongson2e_MultDivMod = FALSE; 
#endif

// temporary flags for controlling algorithm selection for fdiv, sqrt, etc
const char *CGEXP_fdiv_algorithm = "sgi";
const char *CGEXP_sqrt_algorithm = "sgi";

// Cycle Count Flags
BOOL CG_Enable_Cycle_Count = FALSE;
BOOL Cycle_PU_Enable = FALSE;  
BOOL Cycle_BB_Enable = FALSE;  
const char *Cycle_String = "";

#ifdef TARG_NVISA
// treat auto stack variables as statics put in local space
BOOL CGEXP_auto_as_static = TRUE;
BOOL CGEXP_gen_ccodes = TRUE;

BOOL CG_vector_loadstore = TRUE;	// emit vector loads and stores

BOOL CG_rematerialize_grf = TRUE;

BOOL CG_remove_typeconv = FALSE; // type conversion removal optimization

BOOL CG_optimize_copies = FALSE; // leave off as OCG seems to handle this now

BOOL CG_use_16bit_ops = TRUE;    // try to replace 32bit ops with 16bit ops
BOOL CG_skip_local_16bit = FALSE;
#endif
