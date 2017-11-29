/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. Pathscale LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cgdriver.c
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/cgdriver.cxx,v $
 *
 * Description:
 *
 * Main driver -- command line processing and file name manipulation --
 * for the code generator.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>

#include <errno.h>
#include <elf_stuff.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>	    /* for WHIRL_REVISION */
#include <ctype.h>
#include "defs.h"
#include "config.h"
#include "config_debug.h"
#include "config_list.h"
#include "config_targ_opt.h"
#include "config_opt.h"
#include "cg_flags.h"
#include "controls.h"
#include "flags.h"
#include "erglob.h"
#include "erlib.h"
#include "errors.h"
#include "../cg/init.cxx"           /* force include of Cg_Initializer */
#include "ercg.h"
#include "file_util.h"
#include "glob.h"
#include "timing.h"
#include "tracing.h"
#include "util.h"
#include "mempool.h"

#include "wn.h"			    /* for WN */
#include "opt_alias_interface.h"    /* for ALIAS_MANAGER stuff */
#include "dwarf_DST_mem.h"

#include "bb.h"			    /* for cgemit.h */
#include "cg.h"			    /* CG_Initialize(), etc. */
#include "cgemit.h"		    /* R_Assemble_File() */
#include "cg_swp_options.h"         /* for SWP_Options */
#include "gra.h"                    /* for GRA_optimize_placement... */
#include "ebo.h"		    /* for EBO options */
#include "cgprep.h"		    /* for CGPREP knobs */
#include "cg_dep_graph.h"	    /* for CG_DEP knobs */
#include "cg_dep_graph_update.h"    /* more CG_DEP knobs */
#include "cio.h"                    /* for rw, cicse etc ...*/
#include "cg_loop.h"                /* for unrolling */
#include "cg_loop_recur.h"	    /* recurrence fixing */
#include "cgtarget.h"		    /* target-dependent stuff */
#include "gcm.h"		    /* for GCM options */
#include "cg_sched_est.h"	    /* for CG_SCHED_EST options */
#include "targ_proc_properties.h"
#include "cgdriver_arch.h"
#include "cgdriver.h"
#include "register.h"
#include "pqs_cg.h"
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
#include "ipfec_options.h"
#endif
#ifdef KEY
#include "cg_gcov.h"
#include "flags.h"
#endif
#include "cg_swp.h"
#ifdef TARG_X8664
#include "config_wopt.h"
#endif

extern void Set_File_In_Printsrc(char *);	/* defined in printsrc.c */

extern char *WHIRL_File_Name;

/* ====================================================================
 *
 * Back	end process-specific global data from glob.h.
 *
 * ====================================================================
 */

/* Output requested: */
BOOL Assembly =	FALSE;		/* Assembly code */
BOOL Object_Code = FALSE;	/* Object code */

/* Have	the OP_REGCOPY operations been translated? */
BOOL Regcopies_Translated = FALSE;

/* ====================================================================
 *
 * Local data.
 *
 * ====================================================================
 */

static char *Argv0;		    /* argv[0] from main */

/* Default file	extensions: */
#define	ASM_FILE_EXTENSION ".s"	/* Assembly code file */
#define	OBJ_FILE_EXTENSION ".o"	/* Relocatable object file */
#define DSTDUMP_FILE_EXTENSION ".be.dst" /* DST dump-file extension */

/* Internal flags: */
static BOOL cg_opt_level_overridden = FALSE;

static BOOL CG_tail_call_overridden = FALSE;
static BOOL CG_enable_prefetch_overridden = FALSE;
static BOOL CG_enable_z_conf_prefetch_overridden  = FALSE;
static BOOL CG_enable_nz_conf_prefetch_overridden = FALSE;
static BOOL CG_enable_pf_L1_ld_overridden = FALSE;
static BOOL CG_enable_pf_L1_st_overridden = FALSE;
static BOOL CG_enable_pf_L2_ld_overridden = FALSE;
static BOOL CG_enable_pf_L2_st_overridden = FALSE;
static BOOL CG_L1_ld_latency_overridden;
static BOOL CG_L2_ld_latency_overridden;
static BOOL CG_L1_pf_latency_overridden;
static BOOL CG_L2_pf_latency_overridden;
static BOOL CG_maxinss_overridden = FALSE;
static BOOL Enable_CG_Peephole_overridden = FALSE;
static BOOL EBO_Opt_Level_overridden = FALSE;
static BOOL Integer_Divide_By_Constant_overridden = FALSE;
static BOOL Integer_Divide_Use_Float_overridden = FALSE;
#ifdef TARG_IA64
static BOOL CGPREP_fold_expanded_daddiu_overridden = FALSE;
static BOOL CG_LOOP_create_loop_prologs_overridden = FALSE;
#endif
#ifdef KEY
static BOOL Integer_Multiply_By_Constant_overridden = FALSE;
#endif
static BOOL CG_DEP_Mem_Arc_Pruning_overridden = FALSE;
static BOOL clone_incr_overridden = FALSE;
static BOOL clone_min_incr_overridden = FALSE;
static BOOL clone_max_incr_overridden = FALSE;
static BOOL CFLOW_Enable_Clone_overridden = FALSE;
#ifdef TARG_X8664
BOOL cg_load_execute_overridden = FALSE;
#endif
#ifdef TARG_NVISA
static BOOL CG_use_16bit_ops_overridden = FALSE;
static BOOL CG_rematerialize_grf_overridden= FALSE;
#endif

/* Keep	a copy of the command line options for assembly	output:	*/
static char *option_string;

#ifdef TARG_IA64
extern BOOL SWP_KNOB_fatpoint;
#endif

#if !defined(TARG_NVISA)
/* Software pipelining options: */
static OPTION_DESC Options_CG_SWP[] = {

  /* General software pipelining options */

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "", NULL,
    0, 0, 0,	&Enable_SWP, &Enable_SWP_overridden },

  { OVK_INT32,	OV_INTERNAL,	TRUE, "sched_direction", "sched_dir",
    0, 0, INT32_MAX,	&SWP_Options.Sched_Direction, NULL },
  { OVK_INT32,	OV_INTERNAL,	TRUE, "heuristics", "heur",
    0, 0, INT32_MAX,	&SWP_Options.Heuristics, NULL },
  { OVK_INT32,	OV_INTERNAL,	TRUE, "opt", "opt",
    0, 0, INT32_MAX,	&SWP_Options.Opt_Level, NULL },
#ifdef TARG_IA64
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "do_loop", NULL,
    0, 0, 0,	&SWP_Options.Enable_Do_Loop, NULL },
#endif
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "while_loop", NULL,
    0, 0, 0,	&SWP_Options.Enable_While_Loop, NULL },
#ifdef TARG_IA64
  { OVK_INT32,	OV_INTERNAL,	TRUE, "miss_ratio", "miss_r",
    0, 0, INT32_MAX,	&SWP_Options.Load_Cache_Miss_Ratio, NULL },
  { OVK_INT32,	OV_INTERNAL,	TRUE, "miss_latency", "miss_l",
    0, 0, INT32_MAX,	&SWP_Options.Load_Cache_Miss_Latency, NULL },
#endif
  { OVK_INT32,	OV_INTERNAL,	TRUE, "min_unroll_times", "min_unr",
    0, 0, INT32_MAX,	&SWP_Options.Min_Unroll_Times, &SWP_Options.Max_Unroll_Times_Set },
  { OVK_INT32,	OV_INTERNAL,	TRUE, "max_unroll_times", "max_unr",
    0, 0, INT32_MAX,	&SWP_Options.Max_Unroll_Times, &SWP_Options.Max_Unroll_Times_Set },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "bundle", NULL,
    TRUE, 0, 0,	&SWP_Options.Enable_Bundling, NULL },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "postincr", "posti",
    0, 0, 0,	&SWP_Options.Enable_Post_Incr, NULL },
  { OVK_INT32,	OV_INTERNAL,	TRUE, "start_ii", "start",
    0, 0, INT32_MAX,	&SWP_Options.Starting_II, NULL },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "workaround", "work", 
    0, 0, 0,	&SWP_Options.Enable_Workaround, NULL },
  { OVK_INT32,	OV_INTERNAL,	TRUE, "critical_threshold", "critical",
    0, 0, INT32_MAX,	&SWP_Options.Critical_Threshold, NULL },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "prep_only", "", 
    0, 0, 0,	&SWP_Options.Prep_Only, NULL },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "min_retry", "", 
    0, 0, 0,	&SWP_Options.Min_Retry, NULL },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "implicit_prefetch", "", 
    0, 0, 0,	&SWP_Options.Implicit_Prefetch, &SWP_Options.Implicit_Prefetch_Set },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "predicate_promotion", "", 
    0, 0, 0,	&SWP_Options.Predicate_Promotion, NULL },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "enable_brp", "", 
    0, 0, 0,	&SWP_Options.Enable_BRP, NULL },
#ifdef TARG_IA64
  { OVK_INT32,  OV_INTERNAL,    TRUE, "fb_prob1", "", 
    0, 0, INT32_MAX,  &SWP_Options.FB_Prob1, NULL },
  { OVK_INT32,  OV_INTERNAL,    TRUE, "fb_prob2", "", 
    0, 0, INT32_MAX,  &SWP_Options.FB_Prob2, NULL },
  { OVK_INT32,  OV_INTERNAL,    TRUE, "fb_freq", "", 
    0, 0, INT32_MAX,  &SWP_Options.FB_Freq, NULL },
#endif 
#ifdef SWP_USE_STL
  { OVK_INT32,  OV_INTERNAL,    TRUE, "ops_limit", NULL,
    SWP_OPS_LIMIT, 0, INT32_MAX,  &SWP_Options.OPS_Limit, NULL },
#else
  { OVK_INT32,  OV_INTERNAL,    TRUE, "ops_limit", NULL,
    SWP_OPS_LIMIT, 0, SWP_OPS_LIMIT,  &SWP_Options.OPS_Limit, NULL },
#endif
  { OVK_COUNT }		/* List terminator -- must be last */
};

/* Global register allocator options */
static OPTION_DESC Options_GRA[] = {
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "optimize_placement", "",
    0,0,0,      &GRA_optimize_placement, NULL,
    "Enable/disable movement of spills and restores created during splitting [Default TRUE]."
  },
#ifdef TARG_X8664
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_forced_max", "local_forced_max",
    4, 0, 16,	&GRA_local_forced_max, &GRA_local_forced_max_set,
    "How many locals to force allocate (out of the number requested by LRA) [Default 4]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "unspill", "",
    0,0,0,	&GRA_unspill_enable, NULL,
    "Enable/disable fusing of GRA spills and restores back to registers [Default FALSE]"
  },
#else
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_forced_max", "",
    4, 0, 32,	&GRA_local_forced_max, NULL,
    "How many locals to force allocate (out of the number requested by LRA) [Default 4]"
  },
#endif // TARG_X8664
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "avoid_glue_references_for_locals", "",
    0,0,0,      &GRA_avoid_glue_references_for_locals,NULL,
    "If possible grant the forced locals from the set of registers not referenced for glue copies in the same block.  [Default TRUE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "split_entry_exit_blocks", "",
    0,0,0,	&GRA_split_entry_exit_blocks,NULL,
    "Enable/Disable splitting of entry/exit blocks for callee saved preferencing [Default TRUE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "split_lranges", "",
    0,0,0,      &GRA_split_lranges, NULL,
    "Turn on/off splitting of live ranges [Default TRUE]"
  },
  { OVK_INT32,	OV_INTERNAL, TRUE, "non_split_tn", "",
    4, 0, INT32_MAX,	&GRA_non_split_tn_id, NULL,
    "Turn off live range splitting for a given TN specified by its tn number (n).  [Default -1]"
  },
  { OVK_INT32,	OV_INTERNAL, TRUE, "non_preference_tn", "",
    4, 0, INT32_MAX,	&GRA_non_preference_tn_id, NULL,
    "Turn off preferencing for a given TN specified by its tn number (n). [Default -1]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "use_old_conflict", "",
    0,0,0,      &GRA_use_old_conflict, NULL,
    "Use old conflict graph algorithm ... not functioning at present."
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "shrink_wrap", "",
    0,0,0,      &GRA_shrink_wrap, NULL,
    "Turn on/off shrink wrapping (currently, only for callee saved regs) [Default TRUE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "loop_splitting", "",
    0,0,0,      &GRA_loop_splitting, NULL,
    "Turn on/off loop directed live range splitting [Default TRUE]",
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "home", "",
    0,0,0,      &GRA_home, NULL,
    "Turn on/off gra homing [Default FALSE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "remove_spills", "",
    0,0,0,      &GRA_remove_spills, NULL,
    "Turn on/off gra removal of spill instructions in Optimize_Placment [Default TRUE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "ensure_spill_proximity", "",
    0,0,0,      &GRA_ensure_spill_proximity, NULL,
    "Turn on/off gra placing spills close to use/def in block [Default TRUE]"
  },    
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "choose_best_split", "",
    0,0,0,      &GRA_choose_best_split, NULL,
    "Turn on/off gra choosing best/smallest interim split found [Default TRUE]"
  },    
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "use_stacked_regs", "",
    0,0,0,      &GRA_use_stacked_regs, NULL,
    "Turn on/off gra using stacked registers [Default TRUE]"
  },    
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "redo_liveness", "",
    0,0,0,      &GRA_redo_liveness, NULL,
    "Turn on/off recalculation of liveness [Default FALSE]"
  },    
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "preference_globals", "",
    0,0,0,      &GRA_preference_globals, NULL,
    "Turn on/off gra preferencing of global TNs (other than glue code) [Default TRUE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "preference_dedicated", "",
    0,0,0,      &GRA_preference_dedicated, NULL,
    "Turn on/off gra preferencing with dedicated TNs  [Default TRUE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "preference_glue", "",
    0,0,0,      &GRA_preference_glue, NULL,
    "Turn on/off gra preferencing in glue code [Default TRUE]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "preference_all", "",
    0,0,0,      &GRA_preference_all, NULL,
    "Turn on/off all gra preferencing [Default TRUE]"
  },
  { OVK_INT32,	OV_INTERNAL, TRUE, "non_home_low", "",
    4, 0, INT32_MAX,	&GRA_non_home_lo, NULL,
    "Turn off homing for a TN range specified by its tn numbers.  [Default INT32_MAX]"
  },
  { OVK_INT32,	OV_INTERNAL, TRUE, "non_home_hi", "",
    4, 0, INT32_MAX,	&GRA_non_home_hi, NULL,
    "Turn off homing for a TN range specified by its tn numbers.  [Default -1]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "recalc_liveness", "",
    0,0,0,      &GRA_recalc_liveness, NULL,
    "Turn on/off recomputation of global liveness info [Default FALSE]"
  },    
  { OVK_NAME,   OV_INTERNAL, TRUE,"call_split_freq", "",
    0, 0, 0,	&GRA_call_split_freq_string, NULL,
    "Threshold frequency of block containing a call below which a caller saved register will be preferred and live ranges spanning it will be split [Default .1]"
  },    
  { OVK_NAME,   OV_INTERNAL, TRUE,"spill_count_factor", "",
    0, 0, 0,	&GRA_spill_count_factor_string, NULL,
    "Factor by which count of spills affects the priority of a split.  Only valid under OPT:space [Default 0.5]"
  },    
#ifdef KEY
  { OVK_BOOL,   OV_INTERNAL, TRUE,"exclude_saved_regs", "",
    0, 0, 0,	&GRA_exclude_callee_saved_regs, NULL,
    "If true, callee-saved registers are never used to allocate to variables by GRA"
  },    
  { OVK_BOOL,   OV_INTERNAL, TRUE,"eh_exclude_saved_regs", "",
    0, 0, 0,	&GRA_eh_exclude_callee_saved_regs, NULL,
    "If true, callee-saved registers are never used to allocate to variables in functions with exception handlers"
  },    
  { OVK_BOOL,   OV_INTERNAL, TRUE,"fp_exclude_saved_regs", "",
    0, 0, 0,	&GRA_fp_exclude_callee_saved_regs, NULL,
    "If true, floating-point callee-saved registers are never used to allocate to variables by GRA"
  },    
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "optimize_boundary", "",
    0,0,0,      &GRA_optimize_boundary, &GRA_optimize_boundary_set,
    "Enable/disable reuse of registers in live range boundary basic blocks [Default FALSE]."
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "prioritize_by_density", "",
    0,0,0,      &GRA_prioritize_by_density, &GRA_prioritize_by_density_set,
    "Enable/disable prioritizing live ranges by reference density [Default FALSE]."
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "reclaim", "",
    0,0,0,      &GRA_reclaim_register, &GRA_reclaim_register_set,
    "Enable/disable reclaiming of registers after they have been allocated [Default FALSE]."
  },
#endif // KEY
#ifdef TARG_X8664
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "grant_special_regs", "",
    0,0,0,      &GRA_grant_special_regs, NULL,
    "Force GRA to always grant rax/rcx/rdx, whether LRA needs them or not."
  },
#endif
  { OVK_COUNT }		/* List terminator -- must be last */
};
#endif // ! TARG_NVISA

static OPTION_DESC Options_CG[] = {

  // Generic CG options.

  { OVK_BOOL,	OV_INTERNAL, TRUE, "warn_bad_freqs", "",
    0, 0, 0,	&CG_warn_bad_freqs, NULL },
#ifdef TARG_IA64
  { OVK_BOOL,	OV_INTERNAL, TRUE, "loop_opt", "loop_opt",
    0, 0, 0,	&CG_enable_loop_optimizations, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "tune_do_loop", "tune_do_loop",
    0, 0, 0,    &CG_tune_do_loop, NULL },
#endif
  { OVK_INT32,	OV_INTERNAL, TRUE, "skip_before", "skip_b",
    0, 0, INT32_MAX, &CG_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "skip_after", "skip_a",
    0, 0, INT32_MAX, &CG_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "skip_equal", "skip_e",
    0, 0, INT32_MAX, &CG_skip_equal, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_skip_before", "local_skip_b",
    0, 0, INT32_MAX, &CG_local_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_skip_after", "local_skip_a",
    0, 0, INT32_MAX, &CG_local_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_skip_equal", "local_skip_e",
    0, 0, INT32_MAX, &CG_local_skip_equal, NULL }, 
#ifdef TARG_NVISA
  { OVK_BOOL,	OV_INTERNAL, TRUE, "skip_local_16bit", "",
    0, 0, 0,	&CG_skip_local_16bit, NULL },
#else
  { OVK_BOOL,	OV_INTERNAL, TRUE, "skip_local_hbf", "",
    0, 0, 0,	&CG_skip_local_hbf, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "skip_local_loop", "",
    0, 0, 0,	&CG_skip_local_loop, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "skip_local_swp", "",
    0, 0, 0,	&CG_skip_local_swp, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "skip_local_ebo", "",
    0, 0, 0,	&CG_skip_local_ebo, NULL },
#ifdef TARG_X8664
  { OVK_BOOL,	OV_VISIBLE, TRUE, "cmp_peep", "",
    0, 0, 0,	&CG_cmp_load_exec, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "fma3_peep", "",
    0, 0, 0,	&CG_fma3_load_exec, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "fma4_peep", "",
    0, 0, 0,	&CG_fma4_load_exec, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "dsched", "",
    0, 0, 0,	&CG_dispatch_schedule, NULL },
  { OVK_BOOL,   OV_VISIBLE, TRUE, "nobest_fit", "",
    0, 0, 0,    &CG_LOOP_nounroll_best_fit_set, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "unalign_st", "",
    0, 0, 0,	&CG_128bitstore, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "brfuse", "",
    0, 0, 0,	&CG_branch_fuse, NULL },
  { OVK_BOOL,   OV_VISIBLE, TRUE, "strcmp_expand", "",
    0, 0, 0,    &CG_strcmp_expand, NULL },
  { OVK_BOOL,   OV_VISIBLE, TRUE, "merge_counters_x86", "",
    0, 0, 0,    &CG_merge_counters_x86, &CG_merge_counters_x86_set },
  { OVK_BOOL,   OV_VISIBLE, TRUE, "interior_ptrs", "",
    0, 0, 0,    &CG_interior_ptrs_x86, NULL },
  { OVK_BOOL,   OV_VISIBLE, TRUE, "noavx_clear", "",
    0, 0, 0,    &CG_NoClear_Avx_Simd, NULL },
#endif
  { OVK_BOOL,	OV_INTERNAL, TRUE, "skip_local_sched", "",
    0, 0, 0,	&CG_skip_local_sched, NULL },
#endif //TARG_NVISA
  { OVK_INT32,	OV_INTERNAL, TRUE, "optimization_level", "opt",
    0, 0, MAX_OPT_LEVEL,
                &CG_opt_level, &cg_opt_level_overridden },

#ifdef TARG_NVISA
  { OVK_BOOL,   OV_INTERNAL, TRUE, "optimize_copies", "",
    0, 0, 0,    &CG_optimize_copies, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE, "remove_typeconv", "",
    0, 0, 0,    &CG_remove_typeconv, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE, "rematerialize_grf", "",
    0, 0, 0,    &CG_rematerialize_grf, &CG_rematerialize_grf_overridden},
  { OVK_BOOL,   OV_INTERNAL, TRUE, "use_16bit_ops", "",
    0, 0, 0,    &CG_use_16bit_ops, &CG_use_16bit_ops_overridden},
  { OVK_BOOL,   OV_INTERNAL, TRUE, "vector_loadstore", "",
    0, 0, 0,    &CG_vector_loadstore, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE,"cflow", NULL,
    0, 0, 0, &CFLOW_Enable, NULL },
#endif

#if !defined(TARG_NVISA)
  // EBO options:
  { OVK_BOOL,	OV_INTERNAL, TRUE, "peephole_optimize", "",
    0, 0, 0,	&Enable_CG_Peephole, &Enable_CG_Peephole_overridden },
#ifdef TARG_IA64
  { OVK_BOOL,	OV_INTERNAL, TRUE, "ebo_post_proc_rgn", "",
    0, 0, 0,	&Enable_EBO_Post_Proc_Rgn , NULL},
#endif
  { OVK_BOOL, 	OV_INTERNAL, TRUE, "create_madds", "create_madd",
    0, 0, 0,  &CG_create_madds, NULL },
#ifdef TARG_IA64
  { OVK_BOOL,   OV_INTERNAL, TRUE,  "enable_ipfec_phases", "enable_ipfec",
    0,0,0,      &CG_Enable_Ipfec_Phases, NULL },
  { OVK_BOOL, OV_INTERNAL, TRUE,  "enable_cycle_counting", "enable_cycle",
    0,0,0,      &CG_Enable_Cycle_Count, NULL },
#endif
#ifdef KEY
  { OVK_BOOL, OV_INTERNAL, TRUE,  "test_coverage", "",
    0, 0, 0,    &flag_test_coverage, NULL},
  { OVK_LIST, OV_INTERNAL, FALSE, "profile_proc", "",
    0, 0, 0,    &Arc_Profile_Region, NULL},
  { OVK_LIST, OV_INTERNAL, FALSE, "profile_id1",  "",
    0, 0, 0,    &Arc_Profile_Region, NULL},
  { OVK_LIST, OV_INTERNAL, FALSE, "profile_id2", "",
    0, 0, 0,    &Arc_Profile_Region, NULL},
  { OVK_INT32,  OV_INTERNAL, FALSE,  "cse_regs", "",
    0, INT32_MIN, INT32_MAX,    &CG_cse_regs, NULL},
  { OVK_INT32,  OV_INTERNAL, FALSE,  "sse_cse_regs", "",
    0, INT32_MIN, INT32_MAX,    &CG_sse_cse_regs, NULL},
#endif
#ifdef TARG_X8664
  { OVK_INT32,  OV_INTERNAL, TRUE,  "sse_load_execute", "sse_load_exe",
    0, 0, INT32_MAX,    &CG_sse_load_execute, NULL},
  { OVK_INT32,	OV_INTERNAL, TRUE, "load_execute", "load_exe",
    0, 0, INT32_MAX,	&CG_load_execute, &cg_load_execute_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "loadbw_execute", "loadbw_exe",
    0, 0, 0,	&CG_loadbw_execute, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE, "valgrind_friendly", "valgrind",
    0, 0, 0,    &CG_valgrind_friendly, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE, "movext_icmp", "movext_icmp",
    0, 0, 0,    &CG_Movext_ICMP, NULL },
#endif

  // CG Dependence Graph related options.

  { OVK_BOOL,	OV_INTERNAL, TRUE, "ignore_lno", "",
    0, 0, 0,	&CG_DEP_Ignore_LNO, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "ignore_wopt", "",
    0, 0, 0,	&CG_DEP_Ignore_WOPT, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "addr_analysis", "",
    0, 0, 0,	&CG_DEP_Addr_Analysis, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "verify_mem_deps", "",
    0, 0, 0,	&CG_DEP_Verify_Mem_Deps, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "add_alloca_arcs", "",
    0, 0, 0,	&CG_DEP_Add_Alloca_Arcs, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "relax_xfer_depndnce", "",
    0, 0, 0,	&CG_DEP_Relax_Xfer_Dependence, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "adjust_ooo_latency", "adjust_ooo_latency",
    0, 0, 0,	&CG_DEP_Adjust_OOO_Latency, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE, "prune_mem", "",
    0, 0, INT32_MAX, &CG_DEP_Mem_Arc_Pruning,
    &CG_DEP_Mem_Arc_Pruning_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "prune_depndnce", "",
    0, 0, 0,	&CG_DEP_Prune_Dependence, NULL },

  // Prefetching and load latency options.
 
#ifdef TARG_IA64
  { OVK_BOOL,	OV_INTERNAL, FALSE,"prefetch", "",
#else
  { OVK_BOOL,   OV_INTERNAL, TRUE,"prefetch", "",
#endif
    0, 0, 0, &CG_enable_prefetch, &CG_enable_prefetch_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"z_conf_prefetch", "",
    0, 0, 0, &CG_enable_z_conf_prefetch,
	     &CG_enable_z_conf_prefetch_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"nz_conf_prefetch", "",
    0, 0, 0, &CG_enable_nz_conf_prefetch,
	     &CG_enable_nz_conf_prefetch_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"pf_L1_ld", "",
    0, 0, 0, &CG_enable_pf_L1_ld, &CG_enable_pf_L1_ld_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"pf_L1_st", "",
    0, 0, 0, &CG_enable_pf_L1_st, &CG_enable_pf_L1_st_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"pf_L2_ld", "",
    0, 0, 0, &CG_enable_pf_L2_ld, &CG_enable_pf_L2_ld_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"pf_L2_st", "",
    0, 0, 0, &CG_enable_pf_L2_st, &CG_enable_pf_L2_st_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "exclusive_prefetch", "",
    0, 0, 0, &CG_exclusive_prefetch, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE, "L1_pf_latency", "",
    0, 0, INT32_MAX, &CG_L1_pf_latency, &CG_L1_pf_latency_overridden  },
  { OVK_INT32,	OV_INTERNAL, TRUE, "L2_pf_latency", "",
    0, 0, INT32_MAX, &CG_L2_pf_latency, &CG_L2_pf_latency_overridden },
  { OVK_INT32,	OV_INTERNAL, TRUE, "L1_ld_latency", "",
    0, 0, INT32_MAX, &CG_L1_ld_latency, &CG_L1_ld_latency_overridden },
  { OVK_INT32,	OV_INTERNAL, TRUE, "L2_ld_latency", "",
    0, 0, INT32_MAX, &CG_L2_ld_latency, &CG_L2_ld_latency_overridden },
  { OVK_INT32,	OV_INTERNAL, TRUE, "z_conf_L1_ld_latency", "",
    0, 0, INT32_MAX, &CG_z_conf_L1_ld_latency, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE, "z_conf_L2_ld_latency", "",
    0, 0, INT32_MAX, &CG_z_conf_L2_ld_latency, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE, "ld_latency", "",
    0, 0, INT32_MAX, &CG_ld_latency, NULL },

  // CGLOOP options.

  { OVK_BOOL,	OV_INTERNAL, TRUE, "loop_opt", "loop_opt",
    0, 0, 0,	&CG_enable_loop_optimizations, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "opt_non_trip_countable", "opt_non_trip",
    0, 0, 0,	&CG_LOOP_optimize_non_trip_countable, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "opt_lno_winddown_cache", NULL,
    0, 0, 0,	&CG_LOOP_optimize_lno_winddown_cache, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "opt_lno_winddown_reg", NULL,
    0, 0, 0,	&CG_LOOP_optimize_lno_winddown_reg, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "opt_non_innermost", "opt_non_inner",
    0, 0, 0,	&CG_LOOP_optimize_non_innermost, NULL },

  { OVK_BOOL,	OV_INTERNAL, TRUE,  "fix_recurrences", "",
    0, 0, 0,    &CG_LOOP_fix_recurrences,
		&CG_LOOP_fix_recurrences_specified },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "back_substitution", "",
    0, 0, 0,    &CG_LOOP_back_substitution,
		&CG_LOOP_back_substitution_specified },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "back_substitution_variant", "",
    0, 0, 0,    &CG_LOOP_back_substitution_variant,
		&CG_LOOP_back_substitution_variant_specified },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "interleave_reductions", "",
    0, 0, 0,    &CG_LOOP_interleave_reductions,
		&CG_LOOP_interleave_reductions_specified },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "interleave_posti", "",
    0, 0, 0,    &CG_LOOP_interleave_posti,
		&CG_LOOP_interleave_posti_specified },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "reassociate", "reassoc",
    0, 0, 0,    &CG_LOOP_reassociate,
		&CG_LOOP_reassociate_specified },
  { OVK_INT32, OV_INTERNAL, TRUE, "recurrence_min_omega", "",
    0, 0, INT32_MAX, &CG_LOOP_recurrence_min_omega, NULL },
#ifdef KEY
  { OVK_INT32, OV_INTERNAL, TRUE, "recurrence_max_omega", "",
    0, 0, 16, &CG_LOOP_recurrence_max_omega, NULL },
  { OVK_INT32, OV_INTERNAL, TRUE, "loop_limit", "",
    INT32_MAX, 0, INT32_MAX, &CG_Enable_Loop_Opt_Limit, NULL },
#endif
#ifdef TARG_X8664
  { OVK_BOOL,	OV_INTERNAL, TRUE, "cloop", "",
    0, 0, 0,	&CG_LOOP_cloop, NULL },
#endif
#if defined(TARG_SL)
  { OVK_INT32, OV_INTERNAL, TRUE, "zdl_enabled_level", "",
    INT32_MAX, 0, INT32_MAX, &CG_zdl_enabled_level, NULL },
  { OVK_BOOL,  OV_INTERNAL, TRUE, "opt_condmv", "",
    0, 0, 0,   &CG_enable_opt_condmv, NULL},
  { OVK_BOOL,  OV_INTERNAL, TRUE, "CBUS_workaround", "",
    0, 0, 0,   &CG_enable_CBUS_workaround, NULL},
  { OVK_BOOL,  OV_INTERNAL, TRUE, "LD_NOP_workaround", "",
    0, 0, 0,   &CG_enable_LD_NOP_workaround, NULL},
  { OVK_BOOL,  OV_INTERNAL, TRUE, "C3_AR_dependence_workaround", "",
    0, 0, 0,   &CG_enbale_C3_AR_dependence_workaround, NULL},

  /* For SL2, I need a options to tell what application I'm comping.
   * So I can get the right LUT file
   */
  { OVK_NAME,	OV_INTERNAL, TRUE,"app_name", "",
    0, 0, 0, &App_Name, NULL },

  { OVK_NAME,	OV_INTERNAL, TRUE,"cand_pattern", "",
    0, 0, 0, &Cand_List_Pattern, NULL },

#endif

  // CG Unrolling options - see also OPT:unroll_times_max:unroll_size.
  { OVK_BOOL,	OV_INTERNAL, TRUE,"unroll_non_trip_countable", "unroll_non_trip",
    0, 0, 0, &CG_LOOP_unroll_non_trip_countable, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"unroll_fully", "unroll_full",
    0, 0, 0, &CG_LOOP_unroll_fully, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"unroll_remainder_fully", "unroll_remainder_full",
    0, 0, 0, &CG_LOOP_unroll_remainder_fully, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE,"unroll_fb_req", "",
    0, 0, 0, &CG_LOOP_unroll_fb_required, NULL },

  // Cross Iteration Loop Optimization options.

  { OVK_BOOL,	OV_INTERNAL, TRUE, "cio_copy_removal", "",
    0, 0, 0, &CIO_enable_copy_removal, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "cio_read_removal", "",
    0, 0, 0, &CIO_enable_read_removal, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "cio_write_removal", "",
    0, 0, 0, &CIO_enable_write_removal, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "cio_cse_removal", "",
    0, 0, 0, &CIO_enable_cse_removal, NULL },
  { OVK_INT32, OV_INTERNAL, TRUE, "cio_rw_max_omega", "",
    8, 0, INT32_MAX, &CIO_rw_max_omega, NULL },

  // Control flow optimizations (CFLOW) options.

  { OVK_BOOL,	OV_INTERNAL, TRUE, "unique_exit", "",
    0, 0, 0,	&CG_unique_exit, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "tail_call", "",
    0, 0, 0,	&CG_tail_call, &CG_tail_call_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_before_cgprep", NULL,
    0, 0, 0, &CFLOW_opt_before_cgprep, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_after_cgprep", "cflow_after_cgprep",
    0, 0, 0, &CFLOW_opt_after_cgprep, NULL },
  { OVK_INT32,  OV_INTERNAL, TRUE,"ebo_level", "ebo",
    0, INT32_MIN, INT32_MAX, &EBO_Opt_Level, &EBO_Opt_Level_overridden },
#ifdef KEY
  { OVK_INT32,  OV_INTERNAL, TRUE,"ebo_opt_mask", "",
    0, INT32_MIN, INT32_MAX, &EBO_Opt_Mask, NULL },
#endif
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow", NULL,
    0, 0, 0, &CFLOW_Enable, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_unreachable", "",
    0, 0, 0, &CFLOW_Enable_Unreachable, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_branch", "",
    0, 0, 0, &CFLOW_Enable_Branch, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_merge", "",
    0, 0, 0, &CFLOW_Enable_Merge, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_reorder", "",
    0, 0, 0, &CFLOW_Enable_Reorder, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_clone", "",
    0, 0, 0, &CFLOW_Enable_Clone, &CFLOW_Enable_Clone_overridden },
#ifdef KEY
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_freq_order", "cflow_freq_order",
    0, 0, 0, &CFLOW_Enable_Freq_Order, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_freq_order_on_heu", "cflow_freq_order_on_heu",
    0, 0, 0, &CFLOW_Enable_Freq_Order_On_Heuristics, NULL },
#else
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_freq_order", "",
    0, 0, 0, &CFLOW_Enable_Freq_Order, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_freq_order_on_heu", "",
    0, 0, 0, &CFLOW_Enable_Freq_Order_On_Heuristics, NULL },
#endif // KEY
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cflow_opt_all_br_to_bcond", "",
    0, 0, 0, &CFLOW_opt_all_br_to_bcond, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"cflow_heuristic_tolerance", "",
    0, 0, 0, &CFLOW_heuristic_tolerance, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"cflow_feedback_tolerance", "",
    0, 0, 0, &CFLOW_feedback_tolerance, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE,"cflow_clone_incr", "cflow_clone_i",
    0, 0, 100, &CFLOW_clone_incr, &clone_incr_overridden },
  { OVK_INT32,	OV_INTERNAL, TRUE,"cflow_clone_min_incr", "cflow_clone_mi",
    0, 0, INT32_MAX, &CFLOW_clone_min_incr, &clone_min_incr_overridden },
  { OVK_INT32,	OV_INTERNAL, TRUE,"cflow_clone_max_incr", "cflow_clone_ma",
    0, 0, INT32_MAX, &CFLOW_clone_max_incr, &clone_max_incr_overridden },
  { OVK_NAME,	OV_INTERNAL, TRUE,"cflow_cold_threshold", "",
    0, 0, 0, &CFLOW_cold_threshold, NULL },

  // Frequency heuristic/feedback options.

  { OVK_BOOL,	OV_INTERNAL, TRUE,"enable_frequency", "",
    0, 0, 0, &FREQ_enable, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"eh_freq", "",
    0, 0, 0, &FREQ_eh_freq, NULL },
#ifdef KEY
  { OVK_NAME,	OV_INTERNAL, TRUE,"non_local_targ_freq", "",
    0, 0, 0, &FREQ_non_local_targ_freq, NULL },
#endif
  { OVK_NAME,	OV_INTERNAL, TRUE,"freq_frequent_never_ratio", "",
    0, 0, 0, &FREQ_frequent_never_ratio, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "freq_view_cfg", "",
    0, 0, 0, &FREQ_view_cfg, NULL },
#endif // ! TARG_NVISA

  // Whirl2ops / Expander options.

  { OVK_BOOL,	OV_VISIBLE, TRUE, "divrem_opt", "",
    0, 0, 0,	&CG_divrem_opt, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"fdiv_algorithm", "fdiv",
    0, 0, 0, &CGEXP_fdiv_algorithm, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"sqrt_algorithm", "sqrt",
    0, 0, 0, &CGEXP_sqrt_algorithm, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"use_copyfcc", "",
    0, 0, 0, &CGEXP_use_copyfcc, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE,"expconst", "",
    DEFAULT_CGEXP_CONSTANT, 0, INT32_MAX, &CGEXP_expandconstant, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"normalize_logical", "normalize",
    0, 0, 0, &CGEXP_normalize_logical, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"gp_prolog_call_shared", "gp_prolog",
    0, 0, 0, &CGEXP_gp_prolog_call_shared, NULL },
#ifdef KEY
  { OVK_BOOL,	OV_INTERNAL, TRUE,"integer_multiply_by_constant", "integer_multiply_by_constant",
    0, 0, 0, &CGEXP_cvrt_int_mult_to_add_shift, &Integer_Multiply_By_Constant_overridden },
#endif
  { OVK_BOOL,	OV_INTERNAL, TRUE,"integer_divide_by_constant", "integer_divide_by_constant",
    0, 0, 0, &CGEXP_cvrt_int_div_to_mult, &Integer_Divide_By_Constant_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"integer_divide_use_float", "integer_divide_use_float",
    0, 0, 0, &CGEXP_cvrt_int_div_to_fdiv, &Integer_Divide_Use_Float_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"fast_imul", "",
    0, 0, 0, &CGEXP_fast_imul, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"float_consts_from_ints", "",
    0, 0, 0, &CGEXP_float_consts_from_ints, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"float_div_by_const", "",
    0, 0, 0, &CGEXP_opt_float_div_by_const, NULL },

#ifdef TARG_LOONGSON
  { OVK_BOOL,   OV_VISIBLE, TRUE,"use_loongson2e_multdivmod", "",
    0, 0, 0, &CGEXP_use_Loongson2e_MultDivMod, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE,"float_use_madd", "",
    0, 0, 0, &CGEXP_float_use_madd, NULL },
#endif
  { OVK_NAME,	OV_INTERNAL, TRUE,"lfhint_L1", "",
    0, 0, 0, &CGEXP_lfhint_L1, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"lfhint_L2", "",
    0, 0, 0, &CGEXP_lfhint_L2, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"ldhint_L1", "",
    0, 0, 0, &CGEXP_ldhint_L1, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"ldhint_L2", "",
    0, 0, 0, &CGEXP_ldhint_L2, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"sthint_L1", "",
    0, 0, 0, &CGEXP_sthint_L1, NULL },
  { OVK_NAME,	OV_INTERNAL, TRUE,"sthint_L2", "",
    0, 0, 0, &CGEXP_sthint_L2, NULL },
#ifdef TARG_NVISA
  { OVK_BOOL,	OV_INTERNAL, TRUE,"auto_as_static", "",
    0, 0, 0, &CGEXP_auto_as_static, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"gen_ccodes", "",
    0, 0, 0, &CGEXP_gen_ccodes, NULL },
#endif

  { OVK_BOOL,	OV_INTERNAL, TRUE, "localize", "localize",
    0, 0, 0, &CG_localize_tns, &CG_localize_tns_Set},
#ifdef TARG_X8664
  { OVK_BOOL,	OV_INTERNAL, TRUE, "localize_x87", "localize_x87",
    0, 0, 0, &CG_localize_x87_tns, &CG_localize_x87_tns_Set,
    "Localize x87 floating point variables.  Has no effect on integer variables.  Default off."
  },
#endif
#ifdef TARG_IA64
  { OVK_BOOL,   OV_INTERNAL, TRUE,  "ldxmov", "",
    0,0,0,      &CG_Enable_Ldxmov_Support, NULL },
#endif
  { OVK_BOOL,	OV_INTERNAL, TRUE, "localize_using_stacked_regs", "localize_using_stack",
    0, 0, 0, &LOCALIZE_using_stacked_regs, NULL },

#if !defined(TARG_NVISA)
  // Local Register Allocation (LRA) options.

  { OVK_BOOL,	OV_INTERNAL, TRUE,"rematerialize", "remat",
    0, 0, 0, &CGSPILL_Rematerialize_Constants, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"force_rematerialization", "force_remat",
    0, 0, 0, &CGSPILL_Enable_Force_Rematerialization, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"lra_reorder", "",
    0, 0, 0, &LRA_do_reorder, NULL },
#if defined(TARG_SL)
  { OVK_BOOL,	OV_INTERNAL, TRUE,"check_reg_alloc", "",
    0, 0, 0, &Enable_Checking_Register_Allocation, NULL },
#endif 
#ifdef TARG_X8664
  { OVK_BOOL,	OV_INTERNAL, FALSE, "prefer_legacy_regs", "",
    0, 0, 0, &LRA_prefer_legacy_regs, NULL },
#endif
#ifdef KEY
  { OVK_INT32,	OV_INTERNAL, TRUE, "inflate_reg_request", "inflate_reg",
    0, 0, 100, &LRA_inflate_reg_request, &LRA_inflate_reg_request_Set,
    "Inflate LRA register request by this percentage for innermost loops [Default 0]"},
  { OVK_BOOL,	OV_INTERNAL, FALSE, "prefer_lru_reg", "",
    1, 0, 0, &LRA_prefer_lru_reg, &LRA_prefer_lru_reg_Set },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "min_spill_loc_size", "",
    0,0,0,      &CG_min_spill_loc_size, NULL,
    "Turn on/off minimize spill location size [Default FALSE]"
  },    
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "min_stack_size", "",
    0,0,0,      &CG_min_stack_size, NULL,
    "Turn on/off minimize stack size [Default TRUE]"
  }, 
#endif

  // Global Code Motion (GCM) options.

  {OVK_BOOL,	OV_INTERNAL, TRUE, "gcm", "gcm",
    0, 0, 0, &GCM_Enable_Scheduling, NULL },
  {OVK_BOOL,	OV_INTERNAL, TRUE, "pre_gcm", "pre_gcm",
    0, 0, 0, &GCM_PRE_Enable_Scheduling, NULL },
  {OVK_BOOL,	OV_INTERNAL, TRUE, "post_gcm", "post_gcm",
    0, 0, 0, &GCM_POST_Enable_Scheduling, NULL },
  {OVK_BOOL,	OV_INTERNAL, TRUE, "force_post_gcm", "force_post_gcm",
    0, 0, 0, &GCM_POST_Force_Scheduling, NULL },
  {OVK_BOOL,	OV_INTERNAL, TRUE, "cflow_after_gcm", "cflow_after_gcm",
    0, 0, 0, &GCM_Enable_Cflow, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "cross_call_motion", "",
    0, 0, 0, &GCM_Motion_Across_Calls, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "use_sched_est", "use_sched_est",
    0, 0, 0, &GCM_Use_Sched_Est, NULL},
  {OVK_BOOL,    OV_INTERNAL, TRUE, "pre_spec_loads", "",
    0, 0, 0, &GCM_PRE_Spec_Loads, NULL},
  {OVK_BOOL,    OV_INTERNAL, TRUE, "post_spec_loads", "",
    0, 0, 0, &GCM_POST_Spec_Loads, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "pointer_speculation", "",
    0, 0, 0, &GCM_Pointer_Spec, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "speculative_ptr_deref", "",
    0, 0, 0, &GCM_Eager_Ptr_Deref, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "speculative_loads", "",
    0, 0, 0, &GCM_Speculative_Loads, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "predicated_loads", "",
    0, 0, 0, &GCM_Predicated_Loads, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "forw_circ_motion", "",
    0, 0, 0, &GCM_Forw_Circ_Motion, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_minimize_reg_usage", "",
    0, 0, 0, &GCM_Min_Reg_Usage, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_test", "",
    0, 0, 0, &GCM_Test, NULL},
  {OVK_BOOL,	OV_INTERNAL, TRUE, "skip_gcm", "",
    0, 0, 0, &CG_Skip_GCM, NULL},
  { OVK_INT32,	OV_INTERNAL, TRUE,"gcm_from_bb", "",
    0, 0, INT32_MAX, &GCM_From_BB, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE,"gcm_to_bb", "",
    0, 0, INT32_MAX, &GCM_To_BB, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE,"gcm_result_tn", "",
    0, 0, INT32_MAX, &GCM_Result_TN, NULL },
#ifdef KEY
  // Consider no more than this number of candidate target bb's.
  { OVK_INT32,	OV_INTERNAL, TRUE,"gcm_bb_limit", "",
    0, 0, INT32_MAX, &GCM_BB_Limit, NULL },
#endif

  // Local Scheduling (LOCS) and HyperBlock Scheduling (HBS) options.

  { OVK_BOOL,	OV_INTERNAL, TRUE,"local_scheduler", "local_sched",
    0, 0, 0, &LOCS_Enable_Scheduling, NULL },
  { OVK_INT32,	OV_INTERNAL, TRUE,"pre_minreg_level", "pre_minreg_level",
    0, 1, 2, &LOCS_PRE_Enable_Minreg_Level, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"pre_local_scheduler", "pre_local_sched",
    0, 0, 0, &LOCS_PRE_Enable_Scheduling, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"post_local_scheduler", "post_local_sched",
    0, 0, 0, &LOCS_POST_Enable_Scheduling, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"branch_likely", "branch_l",
    0, 0, 0, &CGTARG_Enable_Brlikely, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"fill_delay_slots", "fill_delay",
    0, 0, 0, &Enable_Fill_Delay_Slots, NULL },
  { OVK_NAME,   OV_INTERNAL, TRUE,"branch_taken_prob", "",
    0, 0, 0,	&CGTARG_Branch_Taken_Prob,
		&CGTARG_Branch_Taken_Prob_overridden},
  { OVK_BOOL,	OV_INTERNAL, TRUE,"locs_form_bundles", "locs_form_bundles",
    0, 0, 0, &LOCS_Enable_Bundle_Formation, NULL },
  {OVK_BOOL,	OV_INTERNAL, TRUE, "pre_hb_scheduler", "pre_hb_sched",
    0, 0, 0, &IGLS_Enable_PRE_HB_Scheduling, NULL },
  {OVK_BOOL,	OV_INTERNAL, TRUE, "post_hb_scheduler", "post_hb_sched",
    0, 0, 0, &IGLS_Enable_POST_HB_Scheduling, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"hb_scheduler", "hb_sched",
    0, 0, 0, &IGLS_Enable_HB_Scheduling, NULL },
#if defined(TARG_SL)
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_sched_pu_skip_before", "local_sched_pu_skip_b",
    -1, 0, INT32_MAX, &CG_local_sched_pu_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_sched_pu_skip_after", "local_sched_pu_skip_a",
    -1, 0, INT32_MAX, &CG_local_sched_pu_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_sched_pu_skip_equal", "local_sched_pu_skip_e",
    -1, 0, INT32_MAX, &CG_local_sched_pu_skip_equal, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_sched_bb_skip_before", "local_sched_bb_skip_b",
    -1, 0, INT32_MAX, &CG_local_sched_bb_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_sched_bb_skip_after", "local_sched_bb_skip_a",
    -1, 0, INT32_MAX, &CG_local_sched_bb_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_sched_bb_skip_equal", "local_sched_bb_skip_e",
    -1, 0, INT32_MAX, &CG_local_sched_bb_skip_equal, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "local_sched_op_skip_after", "local_sched_op_skip_a",
    -1, 0, INT32_MAX, &CG_local_sched_op_skip_after, NULL }, 
  { OVK_INT32, OV_INTERNAL, TRUE, "bb_sched_op_max", "bb_sched_op_max",
    0, 0, INT32_MAX, &CG_bb_sched_op_num_max, NULL },
  /* The following nine are for GCM binary search */
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_skip_before", "gcm_skip_b",
    -1, 0, INT32_MAX, &CG_GCM_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_skip_after", "gcm_skip_a",
    -1, 0, INT32_MAX, &CG_GCM_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_skip_equal", "gcm_skip_e",
    -1, 0, INT32_MAX, &CG_GCM_skip_equal, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_loop_skip_before", "gcm_loop_skip_b",
    -1, 0, INT32_MAX, &CG_GCM_loop_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_loop_skip_after", "gcm_loop_skip_a",
    -1, 0, INT32_MAX, &CG_GCM_loop_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_loop_skip_equal", "gcm_loop_skip_e",
    -1, 0, INT32_MAX, &CG_GCM_loop_skip_equal, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_op_skip_before", "gcm_op_skip_b",
    -1, 0, INT32_MAX, &CG_GCM_op_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_op_skip_after", "gcm_op_skip_a",
    -1, 0, INT32_MAX, &CG_GCM_op_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_op_skip_equal", "gcm_op_skip_e",
    -1, 0, INT32_MAX, &CG_GCM_op_skip_equal, NULL }, 

  // binary search options for LICM in GCM
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_licm_loop_skip_before", "gcm_licm_loop_skip_b",
    -1, 0, INT32_MAX, &CG_GCM_LICM_loop_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_licm_loop_skip_after", "gcm_licm_loop_skip_a",
    -1, 0, INT32_MAX, &CG_GCM_LICM_loop_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_licm_loop_skip_equal", "gcm_licm_loop_skip_e",
    -1, 0, INT32_MAX, &CG_GCM_LICM_loop_skip_equal, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_licm_op_skip_before", "gcm_licm_op_skip_b",
    -1, 0, INT32_MAX, &CG_GCM_LICM_op_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_licm_op_skip_after", "gcm_licm_op_skip_a",
    -1, 0, INT32_MAX, &CG_GCM_LICM_op_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "gcm_licm_op_skip_equal", "gcm_licm_op_skip_e",
    -1, 0, INT32_MAX, &CG_GCM_LICM_op_skip_equal, NULL }, 

  // binary search options for DCE in GCM
  { OVK_INT32,	OV_INTERNAL, TRUE, "loop_dce_loop_skip_before", "loop_dce_loop_skip_b",
    -1, 0, INT32_MAX, &CG_LOOP_DCE_loop_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "loop_dce_loop_skip_after", "loop_dce_loop_skip_a",
    -1, 0, INT32_MAX, &CG_LOOP_DCE_loop_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "loop_dce_loop_skip_equal", "loop_dce_loop_skip_e",
    -1, 0, INT32_MAX, &CG_LOOP_DCE_loop_skip_equal, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "loop_dce_op_skip_before", "loop_dce_op_skip_b",
    -1, 0, INT32_MAX, &CG_LOOP_DCE_op_skip_before, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "loop_dce_op_skip_after", "loop_dce_op_skip_a",
    -1, 0, INT32_MAX, &CG_LOOP_DCE_op_skip_after, NULL }, 
  { OVK_INT32,	OV_INTERNAL, TRUE, "loop_dce_op_skip_equal", "loop_dce_op_skip_e",
    -1, 0, INT32_MAX, &CG_LOOP_DCE_op_skip_equal, NULL }, 

  /* The following are for controlling GCM */
  { OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_enable_critical_edge_motion", "gcm_enable_critical_edge_motion",
    1, 0, 0, &CG_GCM_enable_critical_edge_motion, NULL }, 
  { OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_enable_mvtc_optimization", "gcm_enable_mvtc_opt",
    1, 0, 0, &CG_GCM_enable_mvtc_optimization, NULL }, 
  { OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_enable_reduce_loop_count", "gcm_enable_reduce_loop_count",
    1, 0, 0, &CG_GCM_enable_reduce_loop_count, NULL }, 
  { OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_enable_break_dependence", "gcm_enable_break_dep",
    0, 0, 0, &CG_GCM_enable_break_dependence, NULL }, 
  { OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_licm", "",
    1, 0, 0, &CG_GCM_enable_licm, NULL},
  { OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_dce", "",
    1, 0, 0, &CG_GCM_enable_dce, NULL},
  { OVK_BOOL,	OV_INTERNAL, TRUE, "gcm_rce", "",
    1, 0, 0, &CG_GCM_enable_rce, NULL},

  // Turns of all region scheduling
  { OVK_BOOL,	OV_INTERNAL, TRUE,"all_rgn_scheduler", "all_rgn_sched",
    0, 0, 0, &RGN_Enable_All_Scheduling, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE,"rgn_schedule", "rgn_sched",
    0, 0, 0, &CG_Enable_Regional_Global_Sched , NULL }, 
  { OVK_BOOL,   OV_INTERNAL, TRUE,"rgn_local_schedule", "rgn_local_sched",
    0, 0, 0, &CG_Enable_Regional_Local_Sched , NULL }, 
#endif
#ifdef KEY
  { OVK_BOOL, OV_INTERNAL, TRUE, "local_fwd_scheduler", "local_fwd_sched",
    0, 0, 0, &LOCS_Fwd_Scheduling, &LOCS_Fwd_Scheduling_set },
  { OVK_UINT32,	OV_INTERNAL, TRUE,"local_sched_algorithm", "local_sched_alg",
    0, 0, 2, &LOCS_Scheduling_Algorithm, &LOCS_Scheduling_Algorithm_set,
    "Select basic block instruction scheduling algorithm" },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"locs_best", "",
    0, 0, 0, &LOCS_Best, &LOCS_Best_set,
    "Select best schedule produced by different scheduling heuristics" },
  { OVK_BOOL, OV_INTERNAL, TRUE, "locs_shallow_depth", "",
    0, 0, 0, &LOCS_Shallow_Depth, &LOCS_Shallow_Depth_set },
  { OVK_BOOL, OV_INTERNAL, TRUE, "locs_balance_ready_types", "",
    0, 0, 0, &LOCS_Balance_Ready_Types, &LOCS_Balance_Ready_Types_set,
    "Enable heuristic to balance the number of int and fp OPs in the ready vector" },
  { OVK_UINT32,	OV_INTERNAL, TRUE,"locs_balance_ready_int", "",
    0, 0, 100, &LOCS_Balance_Ready_Int, &LOCS_Balance_Ready_Int_set,
    "The ready vector should contain no more than this percentage of int OPs" },
  { OVK_UINT32,	OV_INTERNAL, TRUE,"locs_balance_ready_fp", "",
    0, 0, 100, &LOCS_Balance_Ready_Fp, &LOCS_Balance_Ready_Fp_set,
    "The ready vector should contain no more than this percentage of fp OPs" },
  { OVK_BOOL, OV_INTERNAL, TRUE, "locs_balance_unsched_types", "",
    0, 0, 0, &LOCS_Balance_Unsched_Types, &LOCS_Balance_Unsched_Types_set,
    "Enable heuristic to balance the number of unscheduled int and fp OPs" },
  { OVK_UINT32,	OV_INTERNAL, TRUE,"locs_balance_unsched_int", "",
    0, 0, 100, &LOCS_Balance_Unsched_Int, &LOCS_Balance_Unsched_Int_set,
    "The unsched OPs should contain no more than this percentage of int OPs" },
  { OVK_UINT32,	OV_INTERNAL, TRUE,"locs_balance_unsched_fp", "",
    0, 0, 100, &LOCS_Balance_Unsched_Fp, &LOCS_Balance_Unsched_Fp_set,
    "The unsched OPs should contain no more than this percentage of fp OPs" },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "locs_reduce_prefetch", "",
    0, 0, 0, &LOCS_Reduce_Prefetch, &LOCS_Reduce_Prefetch_set,
    "Delete prefetches that cannot be scheduled in an unused issue slot" },
#endif

  // Turns of all scheduling (LOCS, HBS, GCM) for triaging.
  { OVK_BOOL,	OV_INTERNAL, TRUE,"all_scheduler", "all_sched",
    0, 0, 0, &IGLS_Enable_All_Scheduling, NULL },

  // Hyperblock formation (HB) options.

  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_formation", "",
    0,0,0,      &HB_formation, NULL,
    "Turn on/off hyperblock formation [Default ON]"
  },    
#ifdef KEY
  { OVK_INT32,	OV_INTERNAL, TRUE,  "ifc_cutoff", "",
    4,0,100,      &HB_if_conversion_cut_off, NULL,
    "What is the cut-off for doing If-conversion"
  },    
#endif
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_static_freq_heuristics", "",
    0,0,0,      &HB_static_freq_heuristics, NULL,
    "Turn on/off hyperblock formation's use of different heuristics in the presence of static frequency analysis [Default ON]"
  },    
  { OVK_INT32,	OV_INTERNAL, TRUE, "hb_max_blocks", "",
    4, 0, 100,	&HB_max_blocks, NULL,
    "How many blocks allowed in a hyperblock [Default architecturally dependent]"
  },
  { OVK_INT32,	OV_INTERNAL, TRUE, "hb_min_blocks", "",
    4, 0, 32,	&HB_min_blocks, NULL,
    "Minimum blocks allowed in a hyperblock [Default 2]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_tail_duplication", "",
    0,0,0,      &HB_allow_tail_duplication, NULL, 
    "Flag to control tail-duplication when forming hyperblocks"
  },   
  { OVK_NAME,	OV_INTERNAL, TRUE, "hb_max_sched_growth", "",
    0, 0, 0,	&HB_max_sched_growth, NULL,
    "Multiplier for max increase in HB sched height [Default:3.0]"
  },
  { OVK_NAME,	OV_INTERNAL, TRUE,"hb_min_path_priority_ratio", "",
    0, 0, 0,	&HB_min_path_priority_ratio, NULL,
    "Ratio to control relative size of paths included in hyperblock [Default: .1]"
  },
  { OVK_NAME,	OV_INTERNAL, TRUE,"hb_min_priority", "",
    0, 0, 0,	&HB_min_priority, NULL,
    "Minimum priority allowed for a hyperblock [Default: .1]"
  },
  { OVK_NAME,	OV_INTERNAL, TRUE,"hb_call_hazard_multiplier", "",
    0, 0, 0,	&HB_call_hazard_multiplier, NULL,
    "Factor by which to reduce path priority in presence of calls [Default: .25]"
  },
  { OVK_NAME,	OV_INTERNAL, TRUE,"hb_memory_hazard_multiplier", "",
    0, 0, 0,	&HB_memory_hazard_multiplier, NULL,
    "Factor by which to reduce path priority in presence of unresolvable memory stores [Default: 1.0]"
  },
  { OVK_NAME,	OV_INTERNAL, TRUE,"hb_base_probability_contribution", "",
    0, 0, 0,	&HB_base_probability_contribution, NULL,
    "Factor to ensure base contribution of path probability to priority [Default: 0.1]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_require_alias", "",
    0,0,0,      &HB_require_alias, NULL,
    "Turn on/off requirement that alias information be present for complex hyperblock formation [Default ON]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_complex_non_loop", "",
    0,0,0,      &HB_complex_non_loop, NULL,
    "Turn on/off complex hyperblock formation for non-loop regions [Default ON]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_simple_ifc", "",
    0,0,0,      &HB_simple_ifc, &HB_simple_ifc_set,
    "Turn on/off simple, always profitable hyperblock formation for non-loop regions [Default ON]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_general_use_pq", "",
    0,0,0,      &HB_general_use_pq, NULL,
    "Turn on/off using priority queue when following side paths in general region id for hyperblocks [Default OFF]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_general_from_top", "",
    0,0,0,      &HB_general_from_top, NULL,
    "Turn on/off following side paths from top of main path in general region id for hyperblocks [Default OFF]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_exclude_calls", "",
    0,0,0,      &HB_exclude_calls, NULL,
    "Disallow blocks with calls during hyperblock formation, temporary workaround before full support for predicate callee-register spilling is included [Default ON]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_exclude_pgtns", "",
    0,0,0,      &HB_exclude_pgtns, NULL,
    "Disallow forming hyperblocks if it consists of any global predicate TNs (PGTNS) [Default ON]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "hb_skip_hammocks", "",
    0,0,0,      &HB_skip_hammocks, NULL,
    "Skip forming hyperblocks on hammocks, cause later pass will do them [Default ON]"
  },
  { OVK_INT32,	OV_INTERNAL, TRUE, "loop_force_ifc", "",
    0, 0, 2,    &CG_LOOP_force_ifc, NULL },
#endif // ! TARG_NVISA

  // Emit options
  { OVK_INT32,	OV_INTERNAL, TRUE,"longbranch_limit", "",
    DEFAULT_LONG_BRANCH_LIMIT, 0, INT32_MAX, &EMIT_Long_Branch_Limit, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"pjump_all", "pjump_all",
    0, 0, 0, &EMIT_pjump_all, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"use_cold_section", "use_cold_section",
    0, 0, 0, &EMIT_use_cold_section, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE,  "emit_asm_dwarf", "",
    0,0,0,      &CG_emit_asm_dwarf, NULL,
    "Turn on/off emission of dwarf data into .s file [Default OFF]"
  },
  { OVK_BOOL,   OV_INTERNAL, TRUE,  "emit_unwind_directives", "",
    0,0,0,      &CG_emit_unwind_directives, NULL,
    "Turn on/off emission of unwind directives into .s file [Default OFF]"
  },
  { OVK_BOOL,   OV_INTERNAL, TRUE,  "emit_unwind_info", "",
#ifdef TARG_X8664
    0,0,0,      &CG_emit_unwind_info, &CG_emit_unwind_info_Set,
#else
    0,0,0,      &CG_emit_unwind_info, NULL,
#endif
    "Turn on/off emission of unwind into .s/.o file [Default OFF]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"volatile_asm_stop", "",
    0, 0, 0, &EMIT_stop_bits_for_volatile_asm, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"emit_stop_bits_for_asm", "",
    0, 0, 0, &EMIT_stop_bits_for_asm, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"emit_explicit_bundles", "",
    0, 0, 0, &EMIT_explicit_bundles, NULL },
#ifdef TARG_IA64
  { OVK_BOOL,	OV_INTERNAL, TRUE,"count_cycles_on_ski", "count_cycle",
    0, 0, 0, &EMIT_count_cycles, NULL,
    "Add stop bit to divide oversubscripted op groups. [Default off]"
  },
#endif
  { OVK_BOOL,	OV_INTERNAL, TRUE, "enable_feedback", "",
    0, 0, 0,	&CG_enable_feedback, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"non_gas_syntax", "non_gas",
    0, 0, 0, &CG_emit_non_gas_syntax, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"inhibit_size_directive", "inhibit_size",
    0, 0, 0, &CG_inhibit_size_directive, NULL },
#ifdef TARG_X8664
  { OVK_BOOL,	OV_INTERNAL, TRUE,"use_movlpd", "",
    0, 0, 0, &CG_use_movlpd, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"use_setcc", "",
    0, 0, 0, &CG_use_setcc, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"short_form", "",
    0, 0, 0, &CG_use_short_form, NULL },
  { OVK_INT32,	OV_VISIBLE, TRUE, "p2align", "p2align",
    2, 0, 2,	&CG_p2align, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "p2align_split", "p2align_split",
    2, 0, 2,	&CG_p2align_split, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "loop32", "loop32",
    0, 0, 0,	&CG_loop32, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "compute_to", "compute_to",
    0, 0, 0,	&CG_compute_to, NULL },
  { OVK_UINT64,	OV_INTERNAL, TRUE, "p2align_freq", "",
    0, 0, UINT64_MAX>>1, &CG_p2align_freq, NULL, "freq threshold for .p2align" },
  { OVK_UINT32,	OV_INTERNAL, TRUE,"p2align_max_skip_bytes", "",
    3, 0, 64, &CG_p2align_max_skip_bytes, NULL, "max skip bytes for .p2align" },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "use_xortozero", "",
    0, 0, 0,	&CG_use_xortozero, &CG_use_xortozero_Set },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "use_test", "",
    0, 0, 0,	&CG_use_test, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "fold_constimul", "",
    0, 0, 0,	&CG_fold_constimul, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "use_incdec", "",
    0, 0, 0,	&CG_use_incdec, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "fold_shiftadd", "", 
    0, 0, 0,	&CG_fold_shiftadd, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "use_prefetchnta", "", 
    0, 0, 0,	&CG_use_prefetchnta, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "idivbyconst_opt", "", 
    0, 0, 0,	&CG_idivbyconst_opt, NULL },
  { OVK_UINT32,	OV_INTERNAL, TRUE, "movnti", "",
    120, -1, UINT32_MAX>>1, &CG_movnti, NULL,
    "Use x86-64's movnti instead of mov when writing memory blocks of this size or larger (in KB).  Value of -1 generates movnti unconditionally." },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "cloop", "",
    0, 0, 0,	&CG_LOOP_cloop, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"use_lddqu", "",
    0, 0, 0, &CG_use_lddqu, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "push_pop_int_saved_regs", "",
    0, 0, 0,	&CG_push_pop_int_saved_regs, &CG_push_pop_int_saved_regs_Set },
  { OVK_UINT32,	OV_INTERNAL, TRUE,"ptr_load_use", "",
    4, 0, 64, &CG_ptr_load_use_latency,  NULL,
    "extra latency between loading a pointer and its use"},
#endif

#ifdef TARG_X8664
  // x87
  { OVK_BOOL,	OV_INTERNAL, TRUE, "x87_store", "",
    0, 0, 0, &CG_x87_store, NULL,
    "Store x87 floating point variables to memory after each computation, in order to reduce the variable's precision from 80 bits to 32/64 bits.  Default off."
  },
#endif
#ifdef TARG_LOONGSON
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "opt_useless_mem_op",
    "opt_useless_mem_op", 0, 0, 0,    &CG_Enable_Opt_Mem_OP, NULL,
    "Remove useless st/ld op after EBO last time"
  },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "opt_useless_st_in_loop",
    "opt_useless_st_in_loop", 0, 0, 0, &CG_Enable_Opt_St_In_Loop, NULL,
    "Remove useless st/ld op after EBO last time"
  },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "enable_enhanced_lra",
    "enable_enhanced_lra", 0, 0, 0,    &CG_Enable_Enhanced_LRA, NULL,
    "Enable use another algorithm for LRA"
  },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "enable_force_enhanced_lra",
    "enable_force_enhanced_lra", 0, 0, 0,    &CG_Enable_Force_Enhanced_LRA, NULL,
    "force to do another algorithm for LRA"
  },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "enable_opt_ld_after_lra",
    "enable_opt_ld_after_lra", 0, 0, 0,    &CG_Enable_Opt_Ld_After_LRA, NULL,
    "Enable opt ld after EBO"
  },
  { OVK_INT32,   OV_VISIBLE,     TRUE, "enable_opt_entry_ra_reg",
    "enable_opt_entry_ra_reg", 0, 0, INT32_MAX,    &CG_Enable_RA_OPT, NULL,
    "Enable opt copy of RA reg in entry"
  },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "enable_sorted_gra",
    "enable_sorted_gra", 0, 0, 0,    &CG_Enable_Sorted_GRA, NULL,
    "Enable opt copy of RA reg in entry"
  },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "ftz",
    "ftz", 0, 0, 0,    &CG_Enable_FTZ, NULL,
    "Enable masking flush-to-zero bit in Floating-exception control register"
  },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "nosched_div",
    "nosched_div", 0, 0, 0,    &CG_NoSched_Divmfhimflo, NULL,
    "not schedule div and mfhi/mflo"
  },  
#endif
#if defined(TARG_SL)
  { OVK_BOOL ,  OV_INTERNAL, TRUE, "instr16","",
     0, 0, 0,	  &CG_Gen_16bit, NULL},
  { OVK_BOOL ,  OV_INTERNAL, TRUE, "br16","",
     0, 0, 0,	  &CG_Enable_br16, NULL},
  { OVK_INT32,  OV_INTERNAL, TRUE,  "pre_size", "",
     0, 0, 100,    &CG_localsch_pre_size,   NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE,  "dsp_thread", "",
     0, 0, 0,        &CG_dsp_thread, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE, "qw_aligned", "",
     0, 0, 0,        &CG_check_quadword, NULL},
  { OVK_BOOL ,  OV_INTERNAL, TRUE, "rep_unpair16","",
     0, 0, 0,	  &CG_rep_unpaired16, NULL},    
  { OVK_BOOL ,  OV_INTERNAL, TRUE, "ignore_mem_alias", "", 
     0, 0, 0,     &CG_ignore_mem_alias, NULL}, 
  {OVK_BOOL ,  OV_INTERNAL, TRUE, "stack_layout","",
     0, 0, 0,	  &CG_stack_layout, NULL},
  { OVK_INT32,   OV_INTERNAL, TRUE,  "isr", "", 
     0, 0, 3,     &CG_ISR,  NULL},
  { OVK_INT32,  OV_INTERNAL, TRUE, "max_accreg", "",
     0, 0, 4,     &CG_Max_Accreg, NULL},
  { OVK_INT32,  OV_INTERNAL, TRUE, "max_addreg", "",
     0, 0, 8,     &CG_Max_Addreg, NULL},
  { OVK_INT32,  OV_INTERNAL, TRUE, "max_loopreg", "",
     0, 0, 4,     &CG_zdl_enabled_level, NULL},
  { OVK_BOOL,  OV_INTERNAL, TRUE, "round_spreg", "",
     0, 0, 0,     &CG_round_spreg, NULL},
  { OVK_BOOL,  OV_INTERNAL, TRUE, "check_packed", "",
     0, 0, 0,     &CG_check_packed, NULL},
  { OVK_BOOL ,  OV_INTERNAL, TRUE, "br_taken","",
     0, 0, 0,	  &CG_branch_taken, NULL},
  { OVK_BOOL,   OV_INTERNAL, TRUE,  "sl2", "",
     0, 0, 0,        &CG_sl2, NULL },
// sl2 specific peephole optimization 
  { OVK_BOOL,	OV_INTERNAL, TRUE,"combine_condmv", "combine_condmv",
    0, 0, 0, &CG_SL2_enable_combine_condmv, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"sl2peephole", "sl2peep",
    0, 0, 0, &CG_SL2_enable_peephole, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"sl2_macro", "sl2_macro",
    0, 0, 0, &CG_Enable_Macro_Instr_Combine, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"exp_v1buf", "exp_v1buf",
    0, 0, 0, &CG_SL2_enable_v1buf_expansion, NULL },
#endif
  // Misc:
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "gra_live_predicate_aware", "",
    0,0,0,      &GRA_LIVE_Predicate_Aware, NULL,
    "Allow GRA_LIVE to be predicate-aware [Default ON]"
  },
  { OVK_BOOL,	OV_INTERNAL, TRUE,  "pqs_disable", "",
    0,0,0,      &PQS_disabled, NULL,
    "Force PQS to be disabled [Default OFF]"
  },
  { OVK_INT32,	OV_INTERNAL, TRUE,"branch_taken_penalty", "",
    0, 0, INT32_MAX, &CGTARG_branch_taken_penalty,
    &CGTARG_branch_taken_penalty_overridden },
#if !defined(TARG_NVISA)
  { OVK_BOOL,   OV_INTERNAL, TRUE, "sched_est_calc_dep_graph", "",
    0, 0, 0,    &CG_SCHED_EST_calc_dep_graph, NULL },
  { OVK_BOOL,   OV_INTERNAL, TRUE, "sched_est_use_locs", "",
    0, 0, 0,    &CG_SCHED_EST_use_locs, NULL },
  { OVK_INT32,   OV_INTERNAL, TRUE, "sched_est_call_cost", "",
    0, 0, INT32_MAX, &CG_SCHED_EST_call_cost, NULL },
#ifndef KEY
  { OVK_BOOL,	OV_INTERNAL, TRUE, "enable_feedback", "",
    0, 0, 0,	&CG_enable_feedback, NULL },
#endif
  { OVK_INT32, OV_INTERNAL, TRUE, "mispredict_branch", "mispredict",
    0, 0, INT32_MAX, &CG_branch_mispredict_penalty, NULL },
  { OVK_INT32, OV_INTERNAL, TRUE, "mispredict_factor", "",
    0, 0, INT32_MAX, &CG_branch_mispredict_factor, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"enable_thr", "",
    0, 0, 0,	&CG_enable_thr, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"reverse_if_conversion", "",
    0, 0, 0,	&CG_enable_reverse_if_conversion,
	       	&CG_enable_reverse_if_conversion_overridden },
#endif // ! TARG_NVISA
  { OVK_INT32,	OV_INTERNAL, TRUE,"body_ins_count_max", "",
    0, 0, INT32_MAX, &CG_maxinss, &CG_maxinss_overridden },
  { OVK_INT32,	OV_INTERNAL, TRUE,"body_blocks_count_max", "",
    0, 0, INT32_MAX, &CG_maxblocks, NULL },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"spec_imul_idiv", "",
    0, 0, 0, &CG_enable_spec_imul, 
      &CG_enable_spec_imul_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"spec_idiv", "",
    0, 0, 0, &CG_enable_spec_idiv, 
	     &CG_enable_spec_idiv_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"spec_fdiv", "",
    0, 0, 0, &CG_enable_spec_fdiv, 
	     &CG_enable_spec_fdiv_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE, "spec_fsqrt", "",
    0, 0, 0, &CG_enable_spec_fsqrt, 
	     &CG_enable_spec_fsqrt_overridden },
  { OVK_BOOL,	OV_INTERNAL, TRUE,"cond_defs", "cond_defs",
    0, 0, 0, &CG_cond_defs_allowed, NULL },

  { OVK_BOOL,	OV_INTERNAL, TRUE,"rename", "",
    0, 0, 0, &CG_enable_rename, NULL },

  { OVK_COUNT }
};

#if defined(TARG_IA64) || defined(TARG_LOONGSON)
/* Ipfec related options: */
//The &IPFEC_... flags are changed into &ORC_... flags.
static OPTION_DESC Options_IPFEC[] = {
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "rgn_form", "",
    0, 0, 0,    &ORC_Enable_Region_Formation, NULL,
    "Use Ipfec region formation instead of original hyperblock formation"},
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "rgn_deco", "",
    0, 0, 0,    &ORC_Enable_Region_Decomposition, NULL,
    "Use Aurora region decomposition"},
 { OVK_INT32,   OV_VISIBLE,     TRUE, "cut_num", "",
    0, 0, 92,    &ORC_Stacked_Cut_Num, NULL,
    "Use cut the number of available stacked registers"},
 { OVK_INT32,   OV_VISIBLE,     TRUE, "spill_num", "",
    0, 0, 30,    &ORC_Stacked_Spill_Num, NULL,
    "Use tune stacked register usage"},
 { OVK_INT32,   OV_VISIBLE,     TRUE, "rgn_dup", "",
    0, 0, 0,    &ORC_Enable_Tail_Duplication, NULL,
    "Use Aurora region tail duplication"},
  { OVK_INT32,   OV_VISIBLE,     TRUE, "rgn_exit", "",
    0, 0, 0,    &ORC_Enable_Exit_Probability, NULL,
    "Use Aurora region exit probability requirement"},
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "if_conv", "", 
    0, 0, 0,    &ORC_Enable_If_Conversion, NULL, 
    "Use Ipfec if-convertor instead of original hyperblock formation" },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "force_if_conv", "", 
    0, 0, 0,    &ORC_Force_If_Conv, NULL, 
    "Use Ipfec if-convertor without profitablity consideration" },
#ifndef TARG_LOONGSON
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "relaxed_if_conv", "",
    0, 0, 0,    &ORC_Relaxed_If_Conv, NULL,
    "Use Ipfec if-convertor with relaxed profability consideration" }, 
#endif
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "combine_exit", "", 
    0, 0, 0,    &ORC_Combine_Exit, NULL, 
    "Enable the combine exits with identical targets" },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "force_para_comp_gen", "", 
    0, 0, 0,    &ORC_Force_Para_Comp_Gen, NULL, 
    "Generate parallel compare without profitablity consideration" },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "para_comp_gen", "", 
    0, 0, 0,    &ORC_Para_Comp_Gen, NULL, 
    "generate parallel compare" },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "disable_merge_bb", "", 
    0, 0, 0,    &ORC_Disable_Merge_BB, NULL, 
    "Use if-convertor without merge basic blocks" },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "prdb", "", 
    0, 0, 0,    &ORC_Enable_PRDB, NULL, 
    "Use Ipfec PRDB instead of original one" },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "bb_verify", "", 
    0, 0, 0,    &ORC_Enable_BB_Verify, NULL, 
    "Use Ipfec BB_Verify to check bb attributes" },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "cflow_after_schedule", "", 
    0, 0, 0,    &ORC_Enable_Opt_after_schedule, NULL, 
    "Use Ipfec cflow_after_schedule to delete empty BBs" },  
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "LICM", "", 
    1, 0, 0,	&ORC_Enable_LICM, NULL, 
    "Loop Invariant Code Motion" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "pre_glos", "", 
    0, 0, 0,	&ORC_Enable_Prepass_GLOS, NULL, 
    "Use Ipfec pre-pass global scheduler" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "post_glos", "", 
    0, 0, 0,	&ORC_Enable_Postpass_GLOS, NULL, 
    "Use Ipfec post-pass global scheduler" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "pre_locs", "", 
    0, 0, 0,	&ORC_Enable_Prepass_LOCS, NULL, 
    "Use Ipfec pre-pass local scheduler" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "post_locs", "", 
    0, 0, 0,	&ORC_Enable_Postpass_LOCS, NULL, 
    "Use Ipfec post-pass local scheduler" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "spec", "", 
    0, 0, 0,	&ORC_Enable_Speculation, NULL, 
    "Enable speculation" },
#ifndef TARG_LOONGSON
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fpld_spec", "", 
    0, 0, 0,	&ORC_Enable_FP_Ld_Speculation, NULL, 
    "Enable floating-point load speculation" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dsra", "", 
    0, 0, 0,	&ORC_Enable_Data_Spec_Res_Aware, NULL, 
    "Enable data speculation resource awareness" },
#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "data_spec", "", 
    0, 0, 0,	&ORC_Enable_Data_Speculation, NULL, 
    "Enable data speculation" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "cntl_spec", "", 
    0, 0, 0,	&ORC_Enable_Cntl_Speculation, NULL, 
    "Enable control speculation" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "cmpsd_tmplt", "", 
    0, 0, 0,	&ORC_Enable_Compressed_Template, NULL, 
    "Turn on using of Compressed Template" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "stress_spec", "", 
    0, 0, 0,	&ORC_Stress_Spec, NULL, 
    "Stress speculation, for debugging purpose" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "glos_reg_pressure_aware", "", 
    1, 0, 0,	&ORC_Glos_Reg_Pressure_Aware, NULL, 
    "Global code motion reg pressure awareness" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "glos_split_entry_bb", "", 
    1, 0, 0,	&ORC_Glos_Split_Entry_BB, NULL, 
    "global code motion split entry block for larger schedule scope" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "glos_split_exit_bb", "", 
    1, 0, 0,	&ORC_Glos_Split_Exit_BB, NULL, 
    "global code motion split exit block for larger schedule scope" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "glos_enable_p_ready_code_motion", "", 
    1, 0, 0,	&ORC_Glos_Enable_P_Ready_Code_Motion, NULL, 
    "Enable P-ready code motion" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "glos_code_motion_across_nested_rgn", "", 
    1, 0, 0,	&ORC_Glos_Code_Motion_Across_Nested_Rgn, NULL, 
    "Enable code motion across nested region" },
  {OVK_BOOL,  OV_VISIBLE, TRUE, "glos_enable_cntl_spec_if_converted_code", "",
    1, 0, 0,    &ORC_Glos_Enable_Cntl_Spec_If_Converted_Code, NULL,
    "Enable control speculation of if-converted code"},
  {OVK_BOOL,  OV_VISIBLE, TRUE, "glos_enable_renaming", "",
    1, 0, 0,    &ORC_Glos_Enable_Renaming, NULL,
    "glos_enable_renaming"},
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "adjust_variable_latency", "", 
    1, 0, 0,	&ORC_Adjust_Variable_Latency, NULL, 
    "Adjust Variable Latency During Code motion" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "glos_motion_across_calls", "", 
    0, 0, 0,	&ORC_Glos_Motion_Across_Calls, NULL, 
    "Enable global code motion across calls" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "pre_bundling", "", 
    0, 0, 0,	&ORC_Enable_Pre_Bundling, NULL, 
    "Turn on bundling on pre-global scheduling" },
   { OVK_INT32,	OV_VISIBLE,	TRUE, "stride_prefetch", "", 
    3, 0, 3,	&ORC_Enable_Stride_Prefetch, NULL, 
    "Turn on stride prefetching" },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "edge_profile_instr", "", 
    0, 0, 0,	&Instru_File_Name, &ORC_Enable_Edge_Profile, 
    "Enable edge profile" },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "value_profile_instr", "", 
    0, 0, 0,	&Value_Instru_File_Name,&ORC_Enable_Value_Profile, 
    "Enable value profile" },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "stride_profile_instr", "", 
    0, 0, 0,	&Stride_Instru_File_Name,&ORC_Enable_Stride_Profile, 
    "Enable value profile" },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "edge_profile_annot", "", 
    0, 0, 0,	&Fb_File_Name, &ORC_Enable_Edge_Profile_Annot, 
    "Enable edge profile" },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "value_profile_annot", "", 
    0, 0, 0,	&Value_Fb_File_Name,&ORC_Enable_Value_Profile_Annot, 
    "Enable value profile" },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "stride_profile_annot", "", 
    0, 0, 0,	&Stride_Fb_File_Name,&ORC_Enable_Stride_Profile_Annot, 
    "Enable value profile" },
  { OVK_NAME,   OV_VISIBLE,  TRUE, "safe_cntl_spec_prob", "",
    0, 0, 0,    &ORC_safe_cntl_spec_prob, &ORC_Enable_Cntl_Speculation,
    "Enable control speculation"},
  { OVK_NAME,   OV_VISIBLE,  TRUE, "unsafe_cntl_spec_prob", "",
    0, 0, 0,    &ORC_unsafe_cntl_spec_prob, &ORC_Enable_Cntl_Speculation,
    "Enable control speculation"},
  { OVK_INT32, OV_INTERNAL, TRUE, "value_instr_range", "", 
    0, 0, INT32_MAX, &Value_Instr_Range, NULL },
  { OVK_INT32, OV_INTERNAL, TRUE, "value_instr_pu_id", "",
    0, 0, INT32_MAX, &Value_Instr_Pu_Id, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "use_random_prob", "", 
    0, 0, 0,	&ORC_Enable_Random_Prob, NULL, 
    "Enable value profile" }, 
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "chk_fail", "", 
    0, 0, 0,	&ORC_Force_CHK_Fail, NULL, 
    "Force every chk fail" }, 
  { OVK_BOOL,   OV_VISIBLE, TRUE, "cascade", "",
    0, 0, 0,    &ORC_Enable_Cascade, NULL,
    "Enable cascaded speculation" },
  { OVK_BOOL,   OV_VISIBLE, TRUE, "hold_uses", "",
    0, 0, 0,    &ORC_Hold_Uses, NULL,
    "Hold the uses of speculative load" },
  { OVK_BOOL,   OV_VISIBLE, TRUE, "profitability", "",
    0, 0, 0,    &ORC_Profitability, NULL,
    "Adjust all ipfec flags considering profitability" },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "chk_compact", "", 
    0, 0, 0,	&ORC_Chk_Compact, NULL, 
    "Whether combine chk split BB" },  
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "load_safety", "", 
    0, 0, 0,	&ORC_Enable_Safety_Load, NULL, 
    "Identify safety load" },   
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "multi_branch", "", 
    0, 0, 0,	&ORC_Enable_Multi_Branch, NULL, 
    "Enable Multiple branch" },   
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "pre_multi_branch", "", 
    0, 0, 0,	&ORC_Enable_Pre_Multi_Branch, NULL, 
    "Enable Previous Multiple branch" },   
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "post_multi_branch", "", 
    0, 0, 0,	&ORC_Enable_Post_Multi_Branch, NULL, 
    "Enable Post Multiple branch" },   
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "cache_ana", "", 
    0, 0, 0,	&ORC_Enable_Cache_Analysis, NULL, 
    "Enable Cache conflict Analysis" },   
/*
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "insert_unat", "", 
    0, 0, 0,	&ORC_Enable_Insert_UNAT, NULL, 
    "Whether Insert unat code" },   
*/

  // Flags for research experiments:
  { OVK_INT32, OV_INTERNAL, TRUE, "care_machine_level", "care_m",
    0, 0, INT32_MAX, &ORC_sched_care_machine, NULL },
  

  { OVK_COUNT }		/* List terminator -- must be last */
};
 /* Cycle Counting related options: */
static OPTION_DESC Options_CYCLE[] = {
  { OVK_BOOL, OV_VISIBLE,     TRUE, "cpe", "",
    0, 0, 0,  &Cycle_PU_Enable, NULL,
    "Cycle count enable" },
  { OVK_NAME, OV_VISIBLE,     TRUE, "cbe", "",
    0, 0, 0,  &Cycle_String, &Cycle_BB_Enable,
    "Cycle count enable" },
  { OVK_COUNT }               /* List terminator -- must be last */
};  

/* VT (Visualization Tool) related options: */
static OPTION_DESC Options_VT[] = {
  { OVK_BOOL,   OV_VISIBLE, TRUE, "bb_op", "",
    0, 0, 0,    &VT_Enable_BB_OP, NULL,
    "Enable a bb's op  visualization"},
  { OVK_BOOL,   OV_VISIBLE, TRUE, "glbl_cfg", "",
    0, 0, 0,    &VT_Enable_Global_CFG, NULL,
    "Enable global control flow graph visualization"},
  { OVK_BOOL,   OV_VISIBLE, TRUE, "rgnl_cfg", "",
    0, 0, 0,    &VT_Enable_Regional_CFG, NULL,
    "Enable regional control flow graph visualization"},
  { OVK_BOOL,   OV_VISIBLE, TRUE, "rgn_tree", "",
    0, 0, 0,    &VT_Enable_Region_Tree, NULL,
    "Enable region tree visualization"},
  { OVK_BOOL,   OV_VISIBLE, TRUE, "bb_dag", "",
    0, 0, 0,    &VT_Enable_BB_DAG, NULL,
    "Enable bb dependence graph visualization"},
  { OVK_BOOL,   OV_VISIBLE, TRUE, "rgnl_dag", "",
    0, 0, 0,    &VT_Enable_Regional_DAG, NULL,
    "Enable regional dependence graph visualization"},
  { OVK_BOOL,   OV_VISIBLE, TRUE, "ptn_gph", "",
    0, 0, 0,    &VT_Enable_Partition_Graph, NULL,
    "Enable partition graph visualization"},

  // options about features about the visualization graph
  { OVK_BOOL,   OV_VISIBLE, TRUE, "cfg_label", "",
    0, 0, 0,    &VT_Enable_CFG_Label, NULL,
    "Enable edge label when visualizing control flow graph"},
  { OVK_BOOL,   OV_VISIBLE, TRUE, "dag_br", "",
    0, 0, 0,    &VT_Enable_DAG_BR, NULL,
    "Enable PREBR and POSTBR dependencies when visualizing dependence graph"},
  
  { OVK_COUNT }		/* List terminator -- must be last */
};

/*Relation options about skip optimization*/
static OPTION_DESC Options_SKIP[] = {
  { OVK_LIST, OV_SHY, FALSE, "locs_skip_bb_before", "locs_skip_bb_b", 
    0, 0, 4096,      &raw_locs_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "locs_skip_bb_after", "locs_skip_bb_a",
    0, 0, 4096,	&raw_locs_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "locs_skip_bb_equal", "locs_skip_bb_e",
    0, 0, 4096,	&raw_locs_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_skip_bb_before", "glos_skip_bb_b", 
    0, 0, 4096,      &raw_glos_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_skip_bb_after", "glos_skip_bb_a",
    0, 0, 4096,	&raw_glos_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_skip_bb_equal", "glos_skip_bb_e",
    0, 0, 4096,	&raw_glos_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_rgn_before", "if_conv_skip_rgn_b",
    0, 0, 4096,	&raw_if_conv_skip_rgn, NULL, 
    "" },
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_rgn_after", "if_conv_skip_rgn_a",
    0, 0, 4096,	&raw_if_conv_skip_rgn, NULL,
    "" }, 
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_rgn_equal", "if_conv_skip_rgn_e",
    0, 0, 4096,	&raw_if_conv_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_area_after", "if_conv_skip_area_a",
    0, 0, 4096,	&raw_if_conv_skip_area, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_area_before", "if_conv_skip_area_b",
    0, 0, 4096,	&raw_if_conv_skip_area, NULL,
    "" },
  {OVK_LIST, OV_SHY, FALSE, "if_conv_skip_area_equal", "if_conv_skip_area_e",
    0, 0, 4096,	&raw_if_conv_skip_area, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_skip_rgn_before", "glos_skip_rgn_b",
    0, 0, 4096,	&raw_glos_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_skip_rgn_after", "glos_skip_rgn_a",
    0, 0, 4096,	&raw_glos_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_skip_rgn_equal", "glos_skip_rgn_e",
    0, 0, 4096,	&raw_glos_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_rgn_before", "spec_skip_rgn_b",
    0, 0, 4096,	&raw_spec_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_rgn_after", "spec_skip_rgn_a",
    0, 0, 4096,	&raw_spec_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_rgn_equal", "spec_skip_rgn_e",
    0, 0, 4096,	&raw_spec_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_bb_before", "spec_skip_bb_b",
    0, 0, 4096,	&raw_spec_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_bb_after", "spec_skip_bb_a",
    0, 0, 4096,	&raw_spec_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_bb_equal", "spec_skip_bb_e",
    0, 0, 4096,	&raw_spec_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "msched_skip_bb_before", "msched_skip_bb_b",
    0, 0, 4096,	&raw_msched_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "msched_skip_bb_after", "msched_skip_bb_a",
    0, 0, 4096,	&raw_msched_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "msched_skip_bb_equal", "msched_skip_bb_e",
    0, 0, 4096,	&raw_msched_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "msched_skip_rgn_before", "msched_skip_rgn_b",
    0, 0, 4096,	&raw_msched_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "msched_skip_rgn_after", "msched_skip_rgn_a",
    0, 0, 4096,	&raw_msched_skip_rgn, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "msched_skip_rgn_equal", "msched_skip_rgn_e",
    0, 0, 4096,	&raw_msched_skip_rgn, NULL,
    "" }, 
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_PU_before", "spec_skip_PU_b",
    0, 0, 4096,	&raw_spec_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_PU_after", "spec_skip_PU_a",
    0, 0, 4096,	&raw_spec_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_PU_equal", "spec_skip_PU_e",
    0, 0, 4096,	&raw_spec_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_PU_before", "if_conv_skip_PU_b",
    0, 0, 4096,	&raw_if_conv_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_PU_after", "if_conv_skip_PU_a",
    0, 0, 4096,	&raw_if_conv_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "if_conv_skip_PU_equal", "if_conv_skip_PU_e",
    0, 0, 4096,	&raw_if_conv_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "PRDB_skip_PU_before", "PRDB_skip_PU_b",
    0, 0, 4096,	&raw_PRDB_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "PRDB_skip_PU_after", "PRDB_skip_PU_a",
    0, 0, 4096,	&raw_PRDB_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "PRDB_skip_PU_equal", "PRDB_skip_PU_e",
    0, 0, 4096,	&raw_PRDB_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "pre_glos_skip_PU_before", "pre_glos_skip_PU_b",
    0, 0, 4096,	&raw_pre_glos_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "pre_glos_skip_PU_after", "pre_glos_skip_PU_a",
    0, 0, 4096,	&raw_pre_glos_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "pre_glos_skip_PU_equal", "pre_glos_skip_PU_e",
    0, 0, 4096,	&raw_pre_glos_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "post_locs_skip_PU_before", "post_locs_skip_PU_b",
    0, 0, 4096,	&raw_post_locs_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "post_locs_skip_PU_after", "post_locs_skip_PU_a",
    0, 0, 4096,	&raw_post_locs_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "post_locs_skip_PU_equal", "post_locs_skip_PU_e",
    0, 0, 4096,	&raw_post_locs_skip_PU, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_op_before", "spec_skip_op_b",
    0, 0, 4096,	&raw_spec_skip_op, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_op_after", "spec_skip_op_a",
    0, 0, 4096,	&raw_spec_skip_op, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "spec_skip_op_equal", "spec_skip_op_e",
    0, 0, 4096,	&raw_spec_skip_op, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "mlbr_skip_bb_before", "mlbr_skip_bb_b", 
    0, 0, 4096,      &raw_mlbr_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "mlbr_skip_bb_after", "mlbr_skip_bb_a",
    0, 0, 4096,	&raw_mlbr_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "mlbr_skip_bb_equal", "mlbr_skip_bb_e",
    0, 0, 4096,	&raw_mlbr_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_rename_skip_bb_before", "glos_rename_skip_bb_b", 
    0, 0, 4096,     &raw_glos_rename_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_rename_skip_bb_after", "glos_rename_skip_bb_a",
    0, 0, 4096,	&raw_glos_rename_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_rename_skip_bb_equal", "glos_rename_skip_bb_e",
    0, 0, 4096,	&raw_glos_rename_skip_bb, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_rename_skip_op_before", "glos_rename_skip_op_b", 
    0, 0, 4096,     &raw_glos_rename_skip_op, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_rename_skip_op_after", "glos_rename_skip_op_a",
    0, 0, 4096,	&raw_glos_rename_skip_op, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "glos_rename_skip_op_equal", "glos_rename_skip_op_e",
    0, 0, 4096,	&raw_glos_rename_skip_op, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "latency2_before", "latency2_b", 
    0, 0, 4096,     &raw_latency2, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "latency2_after", "latency2_a",
    0, 0, 4096,	&raw_latency2, NULL,
    "" },
  { OVK_LIST, OV_SHY, FALSE, "latency2_equal", "latency2_e",
    0, 0, 4096,	&raw_latency2, NULL,
    "" },
  
  { OVK_COUNT }		/* List terminator -- must be last */
};
#endif

OPTION_GROUP Cg_Option_Groups[] = {
  { "CG", ':', '=', Options_CG },
#if !defined(TARG_NVISA)
  { "SWP", ':', '=', Options_CG_SWP },
  { "GRA", ':', '=', Options_GRA },
#endif
#ifdef TARG_IA64
  { "IPFEC", ':', '=', Options_IPFEC },
  { "CYCLE", ':', '=', Options_CYCLE }, 
  { "VT", ':', '=', Options_VT },
  { "SKIP", ':', '=', Options_SKIP },
#endif
#ifdef TARG_LOONGSON
  { "IPFEC", ':', '=', Options_IPFEC },
#endif
  { NULL }		/* List terminator -- must be last */
};


extern INT prefetch_ahead;
INT _prefetch_ahead = 2;
#if defined(BUILD_OS_DARWIN) || !defined(SHARED_BUILD)
/* Apparently not referenced elsewhere; Mach-O can't do aliases */
#define prefetch_ahead (_prefetch_ahead)
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak prefetch_ahead = _prefetch_ahead
#endif /* defined(BUILD_OS_DARWIN) */

/* =======================================================================
 *
 *  Configure_Prefetch
 *
 *  Configure the prefetch flags controlled by prefetch_ahead exported
 *  from LNO. It MUST be called after lno.so has been loaded.
 *
 * =======================================================================
 */
static void
Configure_prefetch_ahead(void)
{
  static INT32 save_L1_pf_latency = -1;
  static INT32 save_L2_pf_latency = -1;
  if ( save_L1_pf_latency < 0 ) {
    save_L1_pf_latency = CG_L1_pf_latency;
    save_L2_pf_latency = CG_L2_pf_latency;
  }
  if (Enable_Prefetch_Ahead_For_Target()) {
    if ( ! CG_L2_pf_latency_overridden )
      if ( prefetch_ahead ) 
	CG_L2_pf_latency = 0;
      else
	CG_L2_pf_latency = save_L2_pf_latency;
    if ( ! CG_L1_pf_latency_overridden )
      if (prefetch_ahead)
	CG_L1_pf_latency = 0;
      else
	CG_L1_pf_latency = save_L1_pf_latency;
  }
}


/* =======================================================================
 *
 *  Configure_Prefetch
 *
 *  Configure the prefetch flags.
 *
 * =======================================================================
 */
static void
Configure_Prefetch(void)
{
  if ( ! OPT_shared_memory) {
	CG_exclusive_prefetch = TRUE;
  }
  /* Detect any of the various cases that cause us to disable 
   * prefetching entirely:
   *   isa < mips4
   *   -CG:prefetch=off
   *   -CG:z_conf_prefetch=off:nz_conf_prefetch=off
   */  
  if (   ! Target_Has_Prefetch()
      || (CG_enable_prefetch_overridden && ! CG_enable_prefetch)
      || (   CG_enable_z_conf_prefetch_overridden 
	  && ! CG_enable_z_conf_prefetch
          && CG_enable_nz_conf_prefetch_overridden 
	  && ! CG_enable_nz_conf_prefetch)
  ) {
disable_prefetch:
    CG_enable_prefetch = FALSE;
    CG_enable_z_conf_prefetch  = FALSE;
    CG_enable_nz_conf_prefetch = FALSE;
    CG_enable_pf_L1_ld = FALSE;
    CG_enable_pf_L1_st = FALSE;
    CG_enable_pf_L2_ld = FALSE;
    CG_enable_pf_L2_st = FALSE;
    return;
  }

  /* At this point, -CG:prefetch was explicitly set to true, or
   * unspecified.
   */
  if ( ! CG_enable_prefetch_overridden ) {
    CG_enable_prefetch = FALSE;

    /* -CG:z_conf_prefetch or -CG:nz_conf_prefetch implicitly
     * set to TRUE, implies we should enable prefetching.
     */
    if (   (   CG_enable_z_conf_prefetch_overridden 
	    && CG_enable_z_conf_prefetch)
        || (   CG_enable_nz_conf_prefetch_overridden 
	    && CG_enable_nz_conf_prefetch)
    ) {
      CG_enable_prefetch = TRUE;
    }

    /* Some targets implicitly enable prefetching.
     */
    else if (Enable_Prefetch_For_Target()) {
      CG_enable_prefetch = TRUE;
    }

    /* No implicit enable of prefetching this time...
     */
    else goto disable_prefetch;
  }

  /* Prefetching is enabled, implicitly or explicitly. Handle any
   * defaults, both target independent and target specific.
   */
  if ( ! CG_enable_z_conf_prefetch_overridden )
    CG_enable_z_conf_prefetch = FALSE;
  if ( ! CG_enable_nz_conf_prefetch_overridden )
    CG_enable_nz_conf_prefetch = TRUE;

  if (Enable_Prefetch_For_Target()) {
    if ( ! CG_L1_ld_latency_overridden ) CG_L1_ld_latency = 8;
#ifndef TARG_IA64
    if ( ! CG_enable_pf_L1_ld_overridden ) CG_enable_pf_L1_ld = TRUE;
    if ( ! CG_enable_pf_L1_st_overridden ) CG_enable_pf_L1_st = TRUE;
#else
    if ( ! CG_enable_pf_L1_ld_overridden ) CG_enable_pf_L1_ld = FALSE;
    if ( ! CG_enable_pf_L1_st_overridden ) CG_enable_pf_L1_st = FALSE;
#endif
    if ( ! CG_enable_pf_L2_ld_overridden ) CG_enable_pf_L2_ld = TRUE;
    if ( ! CG_enable_pf_L2_st_overridden ) CG_enable_pf_L2_st = TRUE;
  } else {
    if ( ! CG_enable_pf_L1_ld_overridden ) CG_enable_pf_L1_ld = TRUE;
    if ( ! CG_enable_pf_L1_st_overridden ) CG_enable_pf_L1_st = TRUE;
    if ( ! CG_enable_pf_L2_ld_overridden ) CG_enable_pf_L2_ld = TRUE;
    if ( ! CG_enable_pf_L2_st_overridden ) CG_enable_pf_L2_st = TRUE;
  }

  /* Finally, check to see if we actually will do any prefetching, and
   * if not, disable prefetching all together.
   */
  if (   ! CG_enable_pf_L1_ld
      && ! CG_enable_pf_L1_st
      && ! CG_enable_pf_L2_ld
      && ! CG_enable_pf_L2_st ) goto disable_prefetch;
}


/* =======================================================================
 *
 *  Configure_CG_Options
 *
 *  After the comand line has been processed and CG_opt_level set, configure
 *  the various CG flags that depend on these two things.
 *  This is also called per PU if the PU opt level changes.
 *
 * =======================================================================
 */
static void
Configure_CG_Options(void)
{
 /* Set code generation options -- see	cg.h: */

  if ( ! CG_localize_tns_Set)
  	CG_localize_tns = (CG_opt_level <= 1);

  if ( ! Enable_SWP_overridden )
  {
    // Enable_SWP = (CG_opt_level > 2) && ! OPT_Space;
#ifdef TARG_IA64
    Enable_SWP = CG_opt_level >= 2;
#else
    Enable_SWP = FALSE;
#endif
  }

  if (CG_opt_level > 2 && !OPT_unroll_size_overridden )
#if defined(KEY) && defined(TARG_X8664) //adjust unroll_size default for em64t and core
    if (Is_Target_EM64T() || Is_Target_Core())
      OPT_unroll_size = 256;
    else
      OPT_unroll_size = 128; 
#elif !defined(TARG_NVISA)
      OPT_unroll_size = 128;
#endif
  
#ifdef TARG_X8664
  if (Is_Target_Orochi() || Is_Target_Barcelona()) {
     // check if default to determine if we use best fit unrolling or not
    if ((OPT_unroll_size == 128) && 
        (OPT_unroll_times == 4) && 
        (WOPT_Enable_WN_Unroll == 1)) {
      if (CG_LOOP_nounroll_best_fit_set == false)
        CG_LOOP_unroll_best_fit = TRUE;
    }
  }
#endif

  if ( OPT_Unroll_Analysis_Set )
  {
    CG_LOOP_unroll_analysis = OPT_Unroll_Analysis;
  }
  CG_LOOP_unroll_times_max = OPT_unroll_times;
  CG_LOOP_unrolled_size_max = OPT_unroll_size;
  CG_LOOP_unroll_level = OPT_unroll_level;

#if defined(TARG_X8664)
  // set reg pressure hueristic flags for prescheduling
  switch (LOCS_PRE_Enable_Minreg_Level) {
  case 1: LOCS_PRE_Enable_General_RegPressure_Sched = TRUE; break;
  case 2: LOCS_PRE_Enable_Unroll_RegPressure_Sched = TRUE; break;
  default:
    break;
  }
#endif

  CG_LOOP_ooo_unroll_heuristics = PROC_is_out_of_order();

  if (OPT_Space)
  {
    CGEXP_expandconstant = 2;
  }

  if (!Integer_Divide_By_Constant_overridden
#ifdef KEY
      && CGEXP_cvrt_int_div_to_mult
#endif
	  ) {
    CGEXP_cvrt_int_div_to_mult = (!OPT_Space) && (CG_opt_level > 0);
  } 

  if (!Integer_Divide_Use_Float_overridden
#ifdef KEY
      && CGEXP_cvrt_int_div_to_fdiv
#endif
	  ) {
    CGEXP_cvrt_int_div_to_fdiv =    !Kernel_Code
				 && Enable_Idiv_In_FPU_For_Target()
				 && !OPT_Space
				 && CG_opt_level > 0;
  }

#ifdef KEY
  if (!Integer_Multiply_By_Constant_overridden &&
      CGEXP_cvrt_int_mult_to_add_shift) {
    CGEXP_cvrt_int_mult_to_add_shift = (!OPT_Space) && (CG_opt_level > 0);
  }
#endif

  if (Kernel_Code && !CG_tail_call_overridden) CG_tail_call = FALSE;

  if (Kernel_Code && !GCM_Speculative_Ptr_Deref_Set)
    GCM_Eager_Ptr_Deref = FALSE;

  if (!CGTARG_Branch_Taken_Prob_overridden)
    CGTARG_Branch_Taken_Prob = "0.95";
  CGTARG_Branch_Taken_Probability = atof(CGTARG_Branch_Taken_Prob);
  
  if ( !CG_enable_spec_idiv_overridden && Enable_Spec_Idiv_For_Target() )
    CG_enable_spec_idiv = FALSE;

  if ( ! CG_LOOP_fix_recurrences_specified
       && (      CG_LOOP_back_substitution
              && CG_LOOP_back_substitution_specified
           ||    CG_LOOP_interleave_reductions
              && CG_LOOP_interleave_reductions_specified
           ||    CG_LOOP_interleave_posti
	      && CG_LOOP_interleave_posti_specified
           ||    CG_LOOP_reassociate 
              && CG_LOOP_reassociate_specified)) {
    CG_LOOP_fix_recurrences = TRUE;
  }

  if ( Enable_SWP && ! Enable_LOH_overridden )
    Enable_LOH = Enable_LOH_For_Target();

  if (!EBO_Opt_Level_overridden) {
    EBO_Opt_Level = (CG_opt_level > 0) ? EBO_Opt_Level_Default : 0;
  }
  Enable_CG_Peephole = (CG_opt_level > 0) ? TRUE : FALSE;
#ifdef TARG_IA64

  if ( IPFEC_Profitability ) {
      // region formation
      IPFEC_Enable_Region_Formation = FALSE;

      // if-conversion
      IPFEC_Enable_If_Conversion = TRUE;
      IPFEC_Force_If_Conv = FALSE;
      IPFEC_Force_Para_Comp_Gen = FALSE;
      IPFEC_Para_Comp_Gen = TRUE;
      IPFEC_Disable_Merge_BB = FALSE;

      // predicate analysis
      IPFEC_Enable_PRDB= TRUE;

      //opt after schedule
      IPFEC_Enable_Opt_after_schedule=TRUE;

      // scheduling
      IPFEC_Enable_Prepass_GLOS = TRUE;
      IPFEC_Enable_Postpass_LOCS = TRUE;

      // specultion
      IPFEC_Enable_Data_Speculation = TRUE;
      IPFEC_Force_CHK_Fail = FALSE;
      IPFEC_Enable_Cascade = TRUE;
      IPFEC_Hold_Uses = FALSE;
      IPFEC_Chk_Compact = TRUE;
      IPFEC_Enable_Safety_Load = TRUE;

      // micro-scheduling
      IPFEC_Enable_Compressed_Template = TRUE;
      IPFEC_Enable_Pre_Bundling = TRUE;
  }
#endif

  /* Enable_Fill_Delay_Slots controls the filling of delay slots in locs
     and gcm */
  if (!Enable_Fill_Delay_Slots_For_Target() || !Enable_Fill_Delay_Slots) 
    GCM_Enable_Fill_Delay_Slots = FALSE;

  /* Clamp body_ins_count_max to max BB length
   */
  if (CG_maxinss_overridden) {
    if (CG_maxinss > Split_BB_Length) {
      Split_BB_Length = CG_maxinss;
    }
  } else {
    CG_maxinss = CG_maxinss_default * CG_opt_level;
    if (CG_maxinss == 0 || CG_maxinss > Split_BB_Length) {
      CG_maxinss = Split_BB_Length;
    }
  }


  /* Set BB clone limits
   */
  if ( Kernel_Code && ! CFLOW_Enable_Clone_overridden ) {
    // if kernel code then want really minimal space,
    // so turn off cloning altogether
    CFLOW_Enable_Clone = FALSE;
  } else if (OPT_Space) {
    if (!clone_incr_overridden) CFLOW_clone_incr = 1;
    if (!clone_min_incr_overridden) CFLOW_clone_min_incr = 1;
    if (!clone_max_incr_overridden) CFLOW_clone_max_incr = 3;
  }

#ifdef TARG_NVISA
    CG_localize_tns = TRUE;
    Enable_SWP = FALSE;
#ifdef FUTURE_SUPPORT
    if (!CG_use_16bit_ops_overridden)
      CG_use_16bit_ops = (Target_ISA < TARGET_ISA_compute_20);
    if (!CG_rematerialize_grf_overridden)
      CG_rematerialize_grf = (Target_ISA < TARGET_ISA_compute_20);
#endif
#endif

  Configure_Prefetch();
#ifdef TARG_X8664
  if ((Target == TARGET_em64t ||
       Target == TARGET_core ||
       Target == TARGET_wolfdale) &&
      ! CG_use_xortozero_Set) {
    CG_use_xortozero = TRUE;
  }

  if (OPT_Space && !CG_use_xortozero_Set)	// Bug 9717
    CG_use_xortozero = TRUE;

  if ((Target == TARGET_barcelona) && 
      !CG_push_pop_int_saved_regs_Set)
    CG_push_pop_int_saved_regs = TRUE;
#endif
}

/* =======================================================================
 *
 *  CG_Configure_Opt_Level
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
CG_Configure_Opt_Level( INT opt_level )
{
  static BOOL opt_level_configured = FALSE;

  if ( opt_level_configured && opt_level == CG_opt_level )
    return;

  if ( opt_level_configured && cg_opt_level_overridden ) {
    /* forget it */
    DevWarn("Attempt to override CG:opt_level=%d flag. Ignored.",CG_opt_level);
    return;
  }

  opt_level_configured = TRUE;

  if ( ! cg_opt_level_overridden )
    CG_opt_level = opt_level;

  Configure_CG_Options();
}


/* ====================================================================
 *
 * Build_Option_String
 *
 * Just	build a	string of all the options passed to the	Common Core
 * process, so the options can be printed at the beginning of the "*.s"
 * file(s).
 *
 * ====================================================================
 */

static void
Build_Option_String (INT argc, char **argv)
{
    INT16 i;
    INT16 arg_size = 0;

    Argv0 = argv[0];		    /* save argv[0] for R_Assemble_File() */

    for (i=1; i<argc; ++i)	    /* skip arg 0 (the program name) */
	if ( argv[i][0] == '-'  && argv[i][1] != 'f')
	    arg_size += ( strlen(argv[i]) + 1 );

    if ( arg_size > 0 ) {
	register char *p;
	
	p = option_string = (char *) malloc(arg_size+1);

	if ( option_string == NULL ) {
	    ErrMsg ( EC_No_Mem, "Build_Option_String" );
	    exit ( 1 );
	}

	p[0] = '\0';
	for (i=1; i<argc; ++i)
	    if ( argv[i][0] == '-'  && argv[i][1] != 'f') {
		register INT len = strlen (argv[i]) + 1;
		if (p != option_string)
		    *p++ = ' ';
		bcopy (argv[i], p, len);
		p += len - 1;
	    }
	
    } else {			    /* no options specified */
	option_string = const_cast<char*>("none");
    }
} /* end: Build_Option_String */

/* ====================================================================
 *
 * Process_Command_Line
 *
 * Process the command line arguments specific to CG.
 *
 * ====================================================================
 */

static void
Process_Command_Line (INT argc, char **argv)
{
    INT16 i;
    char *cp;

    /* Check the command line flags: */
    for ( i=0; i<argc; i++ ) {
	if ( argv[i] != NULL && *(argv[i]) == '-' ) {
	    cp = argv[i]+1;	    /* Pointer to next flag character */

	    /* First try to process as command-line option group */
	    if (Process_Command_Line_Group(cp, Cg_Option_Groups))
		continue;

	    switch ( *cp++ ) {

	    case 'f':		    /* file options */
		/* error case already handled by main driver */
		switch (*cp) {
		case 'a':	    /* Assembly file */
		case 's':
		    Assembly = TRUE;
		    Asm_File_Name = cp + 2;
		    break;

		case 'o':	    /* object file */
		    Object_Code = TRUE;
		    Obj_File_Name = cp + 2;
		    break;

		}
		break;

	    case 's':		    /* -s: Produce assembly file: */
	    case 'S':		    /* -S: Produce assembly file: */
                Assembly = TRUE;
                break;

	    case 't':
                /* handle the -tfprev10 option to fix tfp hardware bugs. */
                if ( strncmp ( cp-1, "tfprev10", 8 ) == 0 ) {
		    No_Quad_Aligned_Branch = TRUE;
                }

                break;
#ifdef TARG_IA64
            case 'O':
                if (!strncasecmp (cp-1, "orc:=",5)) {
                    cp += 4 ;
                    if (!strcasecmp (cp, "on") || !strcasecmp(cp, "true")) {
                        CG_Enable_Ipfec_Phases = TRUE;
                    } else if (!strcasecmp (cp, "off") || !strcasecmp(cp, "false")) {
                        CG_Enable_Ipfec_Phases = FALSE;
                    }
                }
                break;
#endif
	    }
	}
    }
}

/* ====================================================================
 *
 * Prepare_Source
 *
 * Process the source argument and associated files.
 *
 * ====================================================================
 */

static void
Prepare_Source (void)
{
    char *fname;

    /* We've got a source file name -- open other files.
     * We want them to be created in the current directory, so we
     * strip off the filename only from Src_File_Name for use:
     */
#if defined(TARG_SL)
    /* In kernel building, we may generate several object files from
     * single .c file, the good way to name the related files should be
     * according to the object file in the original user command. But the
     * original object file is not transferred to the 'be', so I can use
     * Irb_File_Name instead, which comes from the original object file
     */
    if( Irb_File_Name )
      fname = Last_Pathname_Component ( Irb_File_Name );
    else
#endif
    fname = Last_Pathname_Component ( Src_File_Name );

    /* If we're producing information for CITE, we need an assembly
     * file even if it wasn't explicitly requested:
     */
    if ( List_Cite ) {
      Assembly = TRUE;
    }

    if ( Assembly ) {
	if ( Asm_File_Name == NULL ) {
	    /* Replace source file extension to get assembly file name: */
	    Asm_File_Name = New_Extension (fname, ASM_FILE_EXTENSION );
	}

	/* Open	the ASM	file for compilation: */
	if ( ( Asm_File	= fopen	( Asm_File_Name, "w" ) ) == NULL ) {
	    ErrMsg ( EC_Asm_Open, Asm_File_Name, errno );
	    Terminate (1);
	}
#ifdef TARG_IA64
        if (Create_Cycle_Output) {
                if ( ( Output_h_File = fopen( Output_h_File_Name, "w" ) ) == NULL ) {                        ErrMsg ( EC_Asm_Open, Output_h_File_Name, errno );
                        Terminate (1);
                }
        } 
#endif
    }

    /* Prepare relocatable object file name: */
    if ( Obj_File_Name == NULL ) {
#if defined(KEY) && !defined(TARG_NVISA) 
	// nvisa doesn't need tempnam, which will cause gcc complaint
	/* bug 2025
	   Always create the object file in /tmp, since the current dir might
	   not be writable.
	 */
	char* tmp_fname = tempnam( NULL, NULL );
	Obj_File_Name = New_Extension( tmp_fname, OBJ_FILE_EXTENSION );
#else
	/* Replace source file extension to get	object file: */
	Obj_File_Name =	New_Extension (fname, OBJ_FILE_EXTENSION);
#endif
    }

}

static void
Increment_Register_Name (char **name)
{
	INT i = atoi(*name);
	++i;
	sprintf(*name, "%d", i);
}

static void
Set_Register_Range_Not_Allocatable (char *regname1, char *regname2)
{
  char regname[8];
  char *p;	// points to first digit in regname 
  INT count = 0;
  strcpy(regname,regname1);
  // find where digits start
  for (p = regname; *p && !isdigit(*p); ++p) ;
  FmtAssert( strncmp(regname1, regname2, p - regname) == 0,
	("register range %s-%s doesn't have matching prefixes", 
	regname1, regname2));

  // create each regname in range
  while (strcmp(regname, regname2) != 0) {
	Set_Register_Never_Allocatable (regname);
	Increment_Register_Name (&p);
	++count; if (count > 200) break;	// avoid infinite loop
  }
  Set_Register_Never_Allocatable (regname);
}

struct Set_DREG_Not_Allocatable 
{
    inline void operator() (UINT32, ST_ATTR *st_attr) const {
	if (ST_ATTR_kind (*st_attr) != ST_ATTR_DEDICATED_REGISTER)
	    return;
	PREG_NUM p = ST_ATTR_reg_id(*st_attr);
	Set_Register_Never_Allocatable(p);
    }
};

// some variables can be pre-allocated to registers,
// in which case the symtab will be marked,
// or the command-line may list registers not to be used.
static void
Mark_Specified_Registers_As_Not_Allocatable (void)
{
  OPTION_LIST *ol = Registers_Not_Allocatable;
  char *start;
  char *p;
  char regname[8];
  char regname2[8];

  // go through global dreg list
  if ( ST_ATTR_Table_Size (GLOBAL_SYMTAB)) {
    For_all (St_Attr_Table, GLOBAL_SYMTAB, 
	Set_DREG_Not_Allocatable());
  }

  // now go through command-line list
  if ( ol == NULL ) return;
  for ( ; ol != NULL; ol = OLIST_next(ol) ) {

    /* Check for commas and ranges: */
    p = OLIST_val(ol);
    start = p;
    while ( *p != ':' && *p != 0 ) {
	if ( *p == ',') {
		strncpy (regname, start, p-start+1);
		regname[p-start] = '\0';
    		Set_Register_Never_Allocatable (regname);
		++p;
		start = p;
	}
 	else if (*p == '-' ) {
		strncpy (regname, start, p-start+1);
		regname[p-start] = '\0';
		++p;
		start = p;
		while (*p != ',' && *p != '\0') {
			++p;
		}
		strncpy (regname2, start, p-start+1);
		regname2[p-start] = '\0';
		Set_Register_Range_Not_Allocatable (regname, regname2);
		if (*p == 0) return;
		++p;
		start = p;
	}
	else {
		++p;
	}
    }
    strncpy (regname, start, p-start+1);
    Set_Register_Never_Allocatable (regname);
  }
}

#ifdef KEY
char ** be_command_line_args = NULL;
INT be_command_line_argc = 0;
#endif // KEY

/* ====================================================================
 *
 * main
 *
 * Main entry point and driver for the Code Generator.
 *
 * ====================================================================
 */

void
CG_Process_Command_Line (INT cg_argc, char **cg_argv, INT be_argc, char **be_argv)
{
    extern char *Whirl_Revision;

    if (strcmp (Whirl_Revision, WHIRL_REVISION) != 0)
	FmtAssert (!DEBUG_Ir_Version_Check,
		   ("WHIRL revision mismatch between be.so (%s) and cg.so (%s)",
		    Whirl_Revision, WHIRL_REVISION));

#ifdef KEY
    be_command_line_args = be_argv;
    be_command_line_argc = be_argc;
#endif // KEY

    /* Perform preliminary command line processing: */
    Build_Option_String ( be_argc, be_argv );
    Process_Command_Line ( cg_argc, cg_argv );

    CG_Configure_Opt_Level(Opt_Level);

#ifdef TARG_IA64
   /* Getting the relative skip_list about if_conv, locs etc.*/ 
    locs_skip_bb = IPFEC_Build_Skiplist(raw_locs_skip_bb);
    glos_skip_bb = IPFEC_Build_Skiplist(raw_glos_skip_bb);
    mlbr_skip_bb = IPFEC_Build_Skiplist(raw_mlbr_skip_bb);

    if_conv_skip_rgn = IPFEC_Build_Skiplist(raw_if_conv_skip_rgn);
    if_conv_skip_area = IPFEC_Build_Skiplist(raw_if_conv_skip_area);
    
    glos_skip_rgn = IPFEC_Build_Skiplist(raw_glos_skip_rgn);
    
    spec_skip_bb = IPFEC_Build_Skiplist(raw_spec_skip_bb);
    spec_skip_rgn = IPFEC_Build_Skiplist(raw_spec_skip_rgn);    
    spec_skip_op = IPFEC_Build_Skiplist(raw_spec_skip_op);
    msched_skip_bb = IPFEC_Build_Skiplist(raw_msched_skip_bb);
    msched_skip_rgn = IPFEC_Build_Skiplist(raw_msched_skip_rgn);
    
    spec_skip_PU = IPFEC_Build_Skiplist(raw_spec_skip_PU);
    if_conv_skip_PU = IPFEC_Build_Skiplist(raw_if_conv_skip_PU);
    PRDB_skip_PU = IPFEC_Build_Skiplist(raw_PRDB_skip_PU);
    pre_glos_skip_PU = IPFEC_Build_Skiplist(raw_pre_glos_skip_PU);
    post_locs_skip_PU = IPFEC_Build_Skiplist(raw_post_locs_skip_PU);
    glos_rename_skip_bb = IPFEC_Build_Skiplist(raw_glos_rename_skip_bb);
    glos_rename_skip_op = IPFEC_Build_Skiplist(raw_glos_rename_skip_op);
    latency2 = IPFEC_Build_Skiplist(raw_latency2);
#endif
    Prepare_Source ();
} /* CG_Process_Command_Line */


/* Initialization that needs to be done after the global symtab is read */
void
CG_Init (void)
{
    Set_Error_Phase ( "Codegen Initialization" );
    MEM_POOL_Initialize (&MEM_local_region_pool, "local_region_pool", TRUE /* zero-memory */);
    MEM_POOL_Initialize (&MEM_local_region_nz_pool, "local_region_nz_pool", FALSE /* zero-memory */);

    REGISTER_Begin();	/* initialize the register package */
    Init_Dedicated_TNs ();

    Mark_Specified_Registers_As_Not_Allocatable ();

    EMT_Begin_File ( Argv0, option_string );

    /* this has to be done after LNO has been loaded to grep
     * prefetch_ahead fromn LNO */
    Configure_prefetch_ahead();
#if defined(KEY) && !defined(TARG_SL) && !defined(TARG_NVISA) && !defined(TARG_LOONGSON)
    if (flag_test_coverage || profile_arcs)
      CG_Init_Gcov();

    if (LOCS_Fwd_Scheduling_set) {
      fprintf(stderr, "warning: -CG:local_fwd_sched is deprecated,"
		      " use -CG:local_sched_alg\n");
      if (!LOCS_Scheduling_Algorithm_set) {
	LOCS_Scheduling_Algorithm = LOCS_Fwd_Scheduling ? 1 : 0;
	LOCS_Scheduling_Algorithm_set = TRUE;
      } else {
	fprintf(stderr, "warning: -CG:local_fwd_sched ignored,"
			" conflicts with -CG:local_sched_alg\n");
      }
    }
#ifdef TARG_X8664
    if (Is_Target_Orochi()) {
      // TODO: add CG_dispatch_schedule set to TRUE once
      //       we have binutils support
      if (CG_loop32 == FALSE)
        CG_loop32 = TRUE; 
    }
    if (Is_Target_Orochi() || Is_Target_Barcelona()) {
      if (CG_interior_ptrs_x86) {
        // Enable sib translation and scheduling for register pressure
        // for unrolled loops.
        CG_merge_counters_x86 = TRUE;
        LOCS_PRE_Enable_Unroll_RegPressure_Sched = TRUE;
      } else if (CG_opt_level == 3 && CG_merge_counters_x86_set == FALSE) {
        CG_merge_counters_x86 = TRUE;
      }
    }
#endif //TARG_X8664
#endif // KEY
} /* CG_Init */

#ifdef KEY
extern void CG_End_Final();
#endif
/* Terimination routines for cg */
void
CG_Fini (void)
{
#if defined(KEY) && !defined(TARG_NVISA) && !defined(TARG_LOONGSON)
    extern BOOL profile_arcs;
    if (profile_arcs)
        CG_End_Final();
    if (flag_test_coverage || profile_arcs)
    	CG_End_Gcov();
#endif
    /* List global symbols if desired: */
    if ( List_Symbols ) {
	Print_global_symtab (Lst_File);
    }

    Set_Error_Phase ( "Codegen Emit" );
    /* Finish off the relocatable object file: */
    EMT_End_File();
    MEM_POOL_Delete (&MEM_local_region_pool);
    MEM_POOL_Delete (&MEM_local_region_nz_pool);

#ifdef KEY
    // Check to see if the asm file was written correctly.  Do this by writing
    // one extra char and checking its status.  Bug 11361.
    if (Assembly) {
      int n = fprintf(Asm_File, "\n");
      if (n != 1) {
	ErrMsg(EC_Asm_Write, Asm_File_Name);
	Terminate(1);
      }
    }
#endif
} /* CG_Fini */

