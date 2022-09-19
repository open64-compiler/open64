/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_lno.cxx,v $
 *
 * Revision history:
 *  11-Nov-96 - Original Version.
 *
 * Description:
 *
 * Configure the -LNO option group (included in config.c).
 *
 * ====================================================================
 *
 * TODO:  at a convenient point:
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_lno_rcs_id =
    "$Source: common/com/SCCS/s.config_lno.cxx $ $Revision: 1.65 $";
#endif /* _KEEP_RCS_ID */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_lno.h"
#include "config_cache.h"

/* ====================================================================
 *
 * -LNO: option group
 *
 * Define the defaults for -LNO option group flags.
 *
 * NOTE:  There are two copies of this defined -- one to copy when
 * initializing a new struct to the defaults, and one to serve as the
 * initial bottom of the Current_LNO stack.  Fix both of them when
 * adding/changing things.
 *
 * WARNING:  The current memory model can handle four levels.  In order
 * to change it, it is necessary to change the parameter in the file
 * ... as well as the initial value lists of the structs below, and the
 * _DUP option descriptor macros below.
 *
 * ====================================================================
 */

#define DEFAULT_UNROLL_PROD_MAX 16
#ifdef KEY // bug 5375 changes unroll_max default to 5
#define DEFAULT_UNROLL_MAX 5
#else
#define DEFAULT_UNROLL_MAX 10
#endif

BOOL Run_autopar = FALSE;
#ifdef KEY
BOOL Simd_Align = FALSE;
BOOL Simd_Reallocate_Objects = FALSE;
#endif
static LNO_FLAGS Default_LNO = {
    NULL, /* next */
    NULL, /* _mhd */

    TRUE,  /* Aequiv */
    FALSE, /* Automatic Data Distribution */
    1,     /* Run_AP */
    FALSE, /* Ill_xform_directives */
    TRUE,  /* Backward_substitution */
    FALSE, /* Blind_loop_reversal */
    TRUE,  /* Blocking */
    0,     /* Blocking_Size */
    TRUE,  /* Cache_model_edge_effects */
#ifdef KEY
    0, /* EffectiveCacheSizePct */
#endif
    TRUE, /* Coupled_opts */
    TRUE, /* Cse */
#ifdef KEY
    0,     /* Cse_Loop_Skip_Before */
    10000, /* Cse_Loop_Skip_After */
    10000, /* Cse_Loop_Skip_Equal */
    0,     /* Simd_Skip_Before */
    1000,  /* Simd_Skip_After */
    1000,  /* Simd_Skip_Equal */
    0,     /* Simd_Loop_Skip_Before */
    1000,  /* Simd_Loop_Skip_After */
    1000,  /* Simd_LOop_Skip_Equal */
    0,     /* HoistIf_Skip_Before */
    1000,  /* HoistIf_Skip_After */
    1000,  /* HoistIf_Skip_Equal */
    1,     /* HoistIf_Threshold */
    0,     /* SVR_Skip_Before */
    10000, /* SVR_Skip_After */
    10000, /* SVR_Skip_Equal */
    TRUE,  /* SVR_Phase1 */
    TRUE,  /* SVR */
    0,     /* Unswitch_Skip_Before */
    1000,  /* Unswitch_Skip_After */
    1000,  /* Unswitch_Skip_Equal */
    0,     /* Unswitch_Loop_Skip_Before */
    1000,  /* Unswitch_Loop_Skip_After */
    1000,  /* Unswitch_Loop_Skip_Equal */
    0,     /* Full_Unroll_Skip_Before */
    1000,  /* Full_Unroll_Skip_After */
    1000,  /* Full_Unroll_Skip_Equal */
    0,     /* Skip_Before */
    10000, /* Skip_After */
    10000, /* Skip_Equal */
    0,     /* Apo_Skip_Before */
    10000, /* Apo_Skip_After */
    10000, /* Apo_Skip_Equal */
    0,     /* Apo_Loop_Skip_Before */
    10000, /* Apo_Loop_Skip_After */
    10000, /* Apo_Loop_Skip_Equal */
    0,     /* Dummy_Skip_Before */
    10000, /* Dummy_Skip_After */
    10000, /* Dummy_Skip_Equal */
#endif     /* KEY */
    FALSE, /* Fancy_tile */
    FALSE, /* Run_fiz_fuse */
#ifdef TARG_X8664
    0, /* Fission */
#else
    1, /* Fission */
#endif
    0,      /* Fission_inner_register_limit */
    TRUE,   /* Forward_substitution */
    1,      /* Fusion */
    5,      /* Fusion_peeling_limit */
    1,      /* Gather_Scatter */
    0xfffe, /* Graph_capacity */
    TRUE,   /* Hoist_messy_bounds */
    FALSE,  /* Ignore_pragmas */
    TRUE,   /* Interchange */
    FALSE,
    FALSE, /* Run_lego */
    TRUE,  /* Run_lego_localizer */
    TRUE,  /* Loop_finalization */
    8,     /* Max_do_loop_depth_strict */
    FALSE, /* Mem_sim */
    TRUE,  /* Minvar */
    1,     /* Opt */
    2,     /* Cache_model */
    TRUE,  /* Run_outer */
#ifdef KEY
    30, /* OLF_Upper_Bound */
    15, /* OLF_Lower_Bound */
#endif
    0,     /* Outer_unroll */
    TRUE,  /* Outer_unroll_deep */
    6,     /* Outer_unroll_min_for_further_unroll */
    0,     /* Outer_unroll_max */
    FALSE, /* Outer_unroll_model_only */
    0,     /* Outer_unroll_prod_max */
    TRUE,  /* Outer_unroll_unity */
    2,     /* ou_aggre */
    0,     /* Run_p3 */
    TRUE,  /* Pseudo_lower */
#ifndef KEY
    0,
    FALSE, /* Run_prefetch */
#else
    NO_PREFETCH,
    FALSE, /* Run_prefetch */
    FALSE,
    FALSE, /* Prefetch for store accesses - Prefetch_stores */
    TRUE,  /*Prefetch Invariant (non-constant) Stride */
    8,     /*Prefetch_Strides_Ahead*/
#endif
    2, /* Prefetch_ahead */
#ifdef TARG_IA64
    8, /* Prefetch_iters_ahead */
#else
    2,     /* Prefetch_iters_ahead */
#endif
    2,     /* Prefetch_cache_factor */
    FALSE, /* Prefetch_indirect */
    FALSE,
    FALSE, /* Run_prefetch_manual */
    TRUE,  /* Power_of_two_hack */
    TRUE,  /* Sclrze */
    0,     /* SE_tile_size */
    1,     /* Split_tiles */
    64,    /* Split_tiles_size */
    FALSE, /* Run_test */
    FALSE, /* Test_dump */
    TRUE,  /* Trapezoidal_outer_unroll */
    FALSE, /* Use_malloc */
    TRUE,  /* Use_parm */
    FALSE, /* Verbose */
    TRUE,  /* Version_mp_loops */
#ifndef KEY
    TRUE, /* Run_vintr */
#else
    1,     /* Run_vintr */
    TRUE,  /* Run_vintr_set */
    FALSE, /* Vintr_Verbose */
    1,     /* Run_simd */
    TRUE,  /* Run_simd_set */
    FALSE, /* Simd_Verbose */
    TRUE,  /* Simd_Reduction */
    TRUE,  /* Simd_Avoid_Fusion */
    TRUE,  /* Run_hoistif */
    TRUE,  /* Ignore_Feedback */
    TRUE,  /* Run_unswitch */
    TRUE,  /* Run_unswitch_phase1 */
    TRUE,  /* Run_unswitch_phase2 */
    FALSE, /* Unswitch_Verbose */
    FALSE, /* Prefetch_Verbose */
    FALSE, /* Build_Scalar_Reductions */
#endif    /* KEY */
    TRUE, /* Run_oinvar */
#ifdef TARG_IA64
    1, /* Run_doacross */
#else
    0,     /* Run_doacross */
#endif
    0, /* Preferred_doacross_tile_size */
#ifdef TARG_IA64
    2048, /* Parallel_overhead */
#else
    4096,  /* Parallel_overhead */
#endif
    FALSE, /* Prompl */
    TRUE,  /* IfMinMax */
    FALSE, /* Run_call_info */
    TRUE,  /* Shackle */
    FALSE, /* Cross_loop */
    FALSE, /* IPA_Enabled */
#ifdef TARG_IA64
    100, /* Num_Iters */
#else
    1000,  /* Num_Iters */
#endif
    1, /* Pure_Level */
    5, /* Small_trip_count */
#ifdef TARG_IA64
    1000, /* Assume_Unknown_Trip_Count */
#endif
    (UINT32)-1, /* Local_pad_size */
#ifdef TARG_X8664
    5, /* Full_unrolling */
#else
    8,     /* Full_unrolling */
#endif
#ifdef KEY
    2000, /* Full_unrolling_loop_size_limit */
#ifdef TARG_IA64
    TRUE, /* Full_Unroll_Outer */
#else
    FALSE, /* Full_Unroll_Outer */
#endif
    0,     /* Processors */
    128,   /* Parallel_per_proc_overhead */
    FALSE, /* Apo_use_feedback */
#endif
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} /* buffer[16] */
};

LNO_FLAGS Initial_LNO = {
    NULL,         /* next */
    &Mhd_Options, /* _mhd */

    TRUE,  /* Aequiv */
    FALSE, /* Automatic data distribution */
    1,     /* Run_AP */
    FALSE, /* Ill_xform_directives */
    TRUE,  /* Backward_substitution */
    FALSE, /* Blind_loop_reversal */
    TRUE,  /* Blocking */
    0,     /* Blocking_Size */
    TRUE,  /* Cache_model_edge_effects */
#ifdef KEY
    0, /* EffectiveCacheSizePct */
#endif
    TRUE, /* Coupled_opts */
    TRUE, /* Cse */
#ifdef KEY
    0,     /* Cse_Loop_Skip_Before */
    10000, /* Cse_Loop_Skip_After */
    10000, /* Cse_Loop_Skip_Equal */
    0,     /* Simd_Skip_Before */
    1000,  /* Simd_Skip_After */
    1000,  /* Simd_Skip_Equal */
    0,     /* Simd_Loop_Skip_Before */
    1000,  /* Simd_Loop_Skip_After */
    1000,  /* Simd_LOop_Skip_Equal */
    0,     /* HoistIf_Skip_Before */
    1000,  /* HoistIf_Skip_After */
    1000,  /* HoistIf_Skip_Equal */
    1,     /* HoistIf_Threshold */
    0,     /* SVR_Skip_Before */
    10000, /* SVR_Skip_After */
    10000, /* SVR_Skip_Equal */
    TRUE,  /* SVR_Phase1 */
    TRUE,  /* SVR */
    0,     /* Unswitch_Skip_Before */
    1000,  /* Unswitch_Skip_After */
    1000,  /* Unswitch_Skip_Equal */
    0,     /* Unswitch_Loop_Skip_Before */
    1000,  /* Unswitch_Loop_Skip_After */
    1000,  /* Unswitch_Loop_Skip_Equal */
    0,     /* Full_Unroll_Skip_Before */
    1000,  /* Full_Unroll_Skip_After */
    1000,  /* Full_Unroll_Skip_Equal */
    0,     /* Skip_Before */
    10000, /* Skip_After */
    10000, /* Skip_Equal */
    0,     /* Apo_Skip_Before */
    10000, /* Apo_Skip_After */
    10000, /* Apo_Skip_Equal */
    0,     /* Apo_Loop_Skip_Before */
    10000, /* Apo_Loop_Skip_After */
    10000, /* Apo_Loop_Skip_Equal */
    0,     /* Dummy_Skip_Before */
    10000, /* Dummy_Skip_After */
    10000, /* Dummy_Skip_Equal */
#endif     /* KEY */
    FALSE, /* Fancy_tile */
    FALSE, /* Run_fiz_fuse */
#ifdef TARG_X8664
    0, /* Fission */
#else
    1,     /* Fission */
#endif
    0,      /* Fission_inner_register_limit */
    TRUE,   /* Forward_substitution */
    1,      /* Fusion */
    5,      /* Fusion_peeling_limit */
    1,      /* Gather_Scatter */
    0xfffe, /* Graph_capacity */
    TRUE,   /* Hoist_messy_bounds */
    FALSE,  /* Ignore_pragmas */
    TRUE,   /* Interchange */
    FALSE,
    FALSE, /* Run_lego */
    TRUE,  /* Run_lego_localizer */
    TRUE,  /* Loop_finalization */
    8,     /* Max_do_loop_depth_strict */
    FALSE, /* Mem_sim */
    TRUE,  /* Minvar */
    1,     /* Opt */
    2,     /* Cache_model */
    TRUE,  /* Run_outer */
#ifdef KEY
    30, /* OLF_Upper_Bound */
    15, /* OLF_Lower_Bound */
#endif
    0,     /* Outer_unroll */
    TRUE,  /* Outer_unroll_deep */
    6,     /* Outer_unroll_min_for_further_unroll */
    0,     /* Outer_unroll_max */
    FALSE, /* Outer_unroll_model_only */
    0,     /* Outer_unroll_prod_max */
    TRUE,  /* Outer_unroll_unity */
    2,     /* ou_aggre */
    0,     /* Run_p3 */
    TRUE,  /* Pseudo_lower */
#ifndef KEY
    0,
    FALSE, /* Run_prefetch */
#else
    NO_PREFETCH,
    FALSE, /* Run_prefetch */
    FALSE,
    FALSE, /* Prefetch for store accesses - Prefetch_stores */
    TRUE,  /* Prefetch Invariant Stride */
    8,     /* Prefetch_Strides_Ahead */
#endif
    2, /* Prefetch_ahead */
#ifdef TARG_IA64
    8, /* Prefetch_iters_ahead */
#else
    2,     /* Prefetch_iters_ahead */
#endif
    2,     /* Prefetch_cache_factor */
    FALSE, /* Prefetch_indirect */
    FALSE,
    FALSE, /* Run_prefetch_manual */
    TRUE,  /* Power_of_two_hack */
    TRUE,  /* Sclrze */
    0,     /* SE_tile_size */
    1,     /* Split_tiles */
    64,    /* Split_tiles_size */
    FALSE, /* Run_test */
    FALSE, /* Test_dump */
    TRUE,  /* Trapezoidal_outer_unroll */
    FALSE, /* Use_malloc */
    TRUE,  /* Use_parm */
    FALSE, /* Verbose */
    TRUE,  /* Version_mp_loops */
#ifndef KEY
    TRUE, /* Run_vintr */
#else
    1,     /* Run_vintr */
    TRUE,  /* Run_vintr_set */
    FALSE, /* Vintr_Verbose */
    1,     /* Run_simd */
    TRUE,  /* Run_simd_set */
    FALSE, /* Simd_Verbose */
    TRUE,  /* Simd_Reduction */
    TRUE,  /* Simd_Avoid_Fusion */
    TRUE,  /* Run_hoistif */
    TRUE,  /* Ignore_Feedback */
    TRUE,  /* Run_unswitch */
    TRUE,  /* Run_unswitch_phase1 */
    TRUE,  /* Run_unswitch_phase2 */
    FALSE, /* Unswitch_Verbose */
    FALSE, /* Prefetch_Verbose */
    FALSE, /* Build_Scalar_Reductions */
#endif    /* KEY */
    TRUE, /* Run_oinvar */
#ifdef TARG_IA64
    1, /* Run_doacross */
#else
    0,     /* Run_doacross */
#endif
    0, /* Preferred_doacross_tile_size */
#ifdef TARG_IA64
    2048, /* Parallel_overhead */
#else
    4096,  /* Parallel_overhead */
#endif
    FALSE, /* Prompl */
    TRUE,  /* IfMinMax */
    FALSE, /* Run_call_info */
    TRUE,  /* Shackle */
    FALSE, /* Cross_loop */
    FALSE, /* IPA array section information available */
#ifdef TARG_IA64
    100, /* Num_Iters */
#else
    1000,  /* Num_Iters */
#endif
    1, /* Pure_Level */
    5, /* Small_trip_count */
#ifdef TARG_IA64
    1000, /* Assume_Unknown_Trip_Count */
#endif
    (UINT32)-1, /* Local_pad_size */
#ifdef TARG_X8664
    5, /* Full_unrolling */
#else
    8,     /* Full_unrolling */
#endif
#ifdef KEY
    2000, /* Full_unrolling_loop_size_limit */
#ifdef TARG_IA64
    TRUE, /* Full_Unroll_Outer */
#else
    FALSE, /* Full_Unroll_Outer */
#endif
    0,     /* Processors */
    128,   /* Parallel_per_proc_overhead */
    FALSE, /* Apo_use_feedback */
#endif
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} /* buffer[16] */
};

LNO_FLAGS *Current_LNO = &Initial_LNO;
MHD *Current_MHD = &Mhd_Options;

/* ====================================================================
 * Descriptor for the -LNO option group.
 * ====================================================================
 */

/* Macros for defining primary -LNO group options: */
#define IL Initial_LNO
#define LNOPT_U32(name, abbr, def, lb, ub, var)                                \
  { OVK_UINT32, OV_VISIBLE, TRUE, name, abbr, def, lb, ub, &IL.var, NULL }
#define LNOPT_U32_SET(name, abbr, def, lb, ub, var, var_set)                   \
  {                                                                            \
    OVK_UINT32, OV_VISIBLE, TRUE, name, abbr, def, lb, ub, &IL.var,            \
        &IL.var_set                                                            \
  }
#define LNOPT_BOOL(name, abbr, var)                                            \
  { OVK_BOOL, OV_VISIBLE, TRUE, name, abbr, 1, 0, 1, &IL.var, NULL }
#define LNOPT_BOOL_SET(name, abbr, var, var_set)                               \
  { OVK_BOOL, OV_VISIBLE, TRUE, name, abbr, 1, 0, 1, &IL.var, &IL.var_set }

/* Macros for defining memory hierarchy -LNO group options: */
#define MHO Mhd_Options
#define MHOPT_I32(name, abbr, def, lb, ub, var)                                \
  { OVK_INT32, OV_VISIBLE, TRUE, name, abbr, def, lb, ub, &MHO.var, NULL }
#define MHOPT_I32_SET(name, abbr, def, lb, ub, var, var_set)                   \
  {                                                                            \
    OVK_INT32, OV_VISIBLE, TRUE, name, abbr, def, lb, ub, &MHO.var,            \
        &MHO.var_set                                                           \
  }
#define MHOPT_I32_DUP(name, def, lb, ub, var)                                  \
  MHOPT_I32(name "1", NULL, def, lb, ub, L[0].var),                            \
      MHOPT_I32(name "2", NULL, def, lb, ub, L[1].var),                        \
      MHOPT_I32(name "3", NULL, def, lb, ub, L[2].var),                        \
      MHOPT_I32(name "4", NULL, def, lb, ub, L[3].var)
#define MHOPT_I32_SET_DUP(name, def, lb, ub, var, vset)                        \
  MHOPT_I32_SET(name "1", NULL, def, lb, ub, L[0].var, L[0].vset),             \
      MHOPT_I32_SET(name "2", NULL, def, lb, ub, L[1].var, L[1].vset),         \
      MHOPT_I32_SET(name "3", NULL, def, lb, ub, L[2].var, L[2].vset),         \
      MHOPT_I32_SET(name "4", NULL, def, lb, ub, L[3].var, L[3].vset)
#define MHOPT_NAME(name, abbr, var)                                            \
  { OVK_NAME, OV_VISIBLE, TRUE, name, abbr, 0, 0, 0, &MHO.var, NULL }
#define MHOPT_NAME_DUP(name, var)                                              \
  MHOPT_NAME(name "1", NULL, L[0].var), MHOPT_NAME(name "2", NULL, L[1].var),  \
      MHOPT_NAME(name "3", NULL, L[2].var),                                    \
      MHOPT_NAME(name "4", NULL, L[3].var)
#define MHOPT_BOOL(name, abbr, var)                                            \
  { OVK_BOOL, OV_VISIBLE, TRUE, name, abbr, 0, 0, 1, &MHO.var, NULL }
#define MHOPT_BOOL_SET(name, abbr, var, var_set)                               \
  { OVK_BOOL, OV_VISIBLE, TRUE, name, abbr, 0, 0, 1, &MHO.var, &MHO.var_set }
#define MHOPT_BOOL_DUP(name, var)                                              \
  MHOPT_BOOL(name "1", NULL, L[0].var), MHOPT_BOOL(name "2", NULL, L[1].var),  \
      MHOPT_BOOL(name "3", NULL, L[2].var),                                    \
      MHOPT_BOOL(name "4", NULL, L[3].var)
#define MHOPT_BOOL_SET_DUP(name, var, vset)                                    \
  MHOPT_BOOL_SET(name "1", NULL, L[0].var, L[0].vset),                         \
      MHOPT_BOOL_SET(name "2", NULL, L[1].var, L[1].vset),                     \
      MHOPT_BOOL_SET(name "3", NULL, L[2].var, L[2].vset),                     \
      MHOPT_BOOL_SET(name "4", NULL, L[3].var, L[3].vset)

static OPTION_DESC Options_LNO[] = {

    LNOPT_BOOL("aequiv", NULL, Aequiv),
    LNOPT_U32("ap", NULL, 1, 0, 2, Run_AP),
    LNOPT_BOOL("apply_illegal_xform_directives", "", Ill_xform_directives),
    MHOPT_I32_DUP("associativity", 0, 0, 8192, Associativity),
    MHOPT_I32_DUP("assoc", 0, 0, 8192, Associativity),
    LNOPT_BOOL("auto_dist", NULL, Autodist),
    LNOPT_BOOL("ad", NULL, Autodist),
    LNOPT_BOOL("backward_substitution", NULL, Backward_substitution),
    LNOPT_BOOL("blind_loop_reversal", NULL, Blind_loop_reversal),
    LNOPT_BOOL("blocking", NULL, Blocking),
    LNOPT_U32("blocking_size", NULL, 0, 0, 99999, Blocking_Size),
    LNOPT_BOOL("cache_edge_effects", NULL, Cache_model_edge_effects),
#ifdef KEY
    LNOPT_U32("ecspct", NULL, 0, 0, 100, EffectiveCacheSizePct),
#endif
    MHOPT_NAME_DUP("cache_size", CS_string),
    MHOPT_NAME_DUP("cs", CS_string),
    MHOPT_I32_SET_DUP("clean_miss_penalty", 0, 0, 1000000000,
                      Clean_Miss_Penalty, CMP_Set),
    MHOPT_I32_SET_DUP("cmp", 0, 0, 1000000000, Clean_Miss_Penalty, CMP_Set),
    LNOPT_BOOL("cse", NULL, Cse),
#ifdef KEY
    LNOPT_U32("cse_loop_skip_before", "cse_loop_skip_before", 32, 0, 10000,
              Cse_Loop_Skip_Before),
    LNOPT_U32("cse_loop_skip_after", "cse_loop_skip_after", 32, 0, 10000,
              Cse_Loop_Skip_After),
    LNOPT_U32("cse_loop_skip_equal", "cse_loop_skip_equal", 32, 0, 10000,
              Cse_Loop_Skip_Equal),
    LNOPT_U32("simd_skip_before", "simd_skip_before", 32, 0, 1000,
              Simd_Skip_Before),
    LNOPT_U32("simd_skip_after", "simd_skip_after", 32, 0, 1000,
              Simd_Skip_After),
    LNOPT_U32("simd_skip_equal", "simd_skip_equal", 32, 0, 1000,
              Simd_Skip_Equal),
    LNOPT_U32("simd_loop_skip_before", "simd_loop_skip_before", 32, 0, 1000,
              Simd_Loop_Skip_Before),
    LNOPT_U32("simd_loop_skip_after", "simd_loop_skip_after", 32, 0, 1000,
              Simd_Loop_Skip_After),
    LNOPT_U32("simd_loop_skip_equal", "simd_loop_skip_equal", 32, 0, 1000,
              Simd_Loop_Skip_Equal),
    LNOPT_U32("hoistif_skip_before", "hoistif_skip_before", 32, 0, 1000,
              HoistIf_Skip_Before),
    LNOPT_U32("hoistif_skip_after", "hoistif_skip_after", 32, 0, 1000,
              HoistIf_Skip_After),
    LNOPT_U32("hoistif_skip_equal", "hoistif_skip_equal", 32, 0, 1000,
              HoistIf_Skip_Equal),
    LNOPT_U32("hoistif_threshold", "hoistif_thres", 32, 0, 1000,
              HoistIf_Threshold),
    LNOPT_U32("svr_skip_before", "svr_skip_before", 32, 0, 1000,
              SVR_Skip_Before),
    LNOPT_U32("svr_skip_after", "svr_skip_after", 32, 0, 1000, SVR_Skip_After),
    LNOPT_U32("svr_skip_equal", "svr_skip_equal", 32, 0, 1000, SVR_Skip_Equal),
    LNOPT_BOOL("svr_phase1", NULL, SVR_Phase1),
    LNOPT_BOOL("svr", NULL, SVR),
    LNOPT_U32("unswitch_skip_before", "unswitch_skip_before", 32, 0, 1000,
              Unswitch_Skip_Before),
    LNOPT_U32("unswitch_skip_after", "unswitch_skip_after", 32, 0, 1000,
              Unswitch_Skip_After),
    LNOPT_U32("unswitch_skip_equal", "unswitch_skip_equal", 32, 0, 1000,
              Unswitch_Skip_Equal),
    LNOPT_U32("unswitch_loop_skip_before", "unswitch_loop_skip_before", 32, 0,
              1000, Unswitch_Loop_Skip_Before),
    LNOPT_U32("unswitch_loop_skip_after", "unswitch_loop_skip_after", 32, 0,
              1000, Unswitch_Loop_Skip_After),
    LNOPT_U32("unswitch_loop_skip_equal", "unswitch_loop_skip_equal", 32, 0,
              1000, Unswitch_Loop_Skip_Equal),
    LNOPT_U32("full_unroll_skip_before", "full_unroll_skip_before", 32, 0, 1000,
              Full_Unroll_Skip_Before),
    LNOPT_U32("full_unroll_skip_after", "full_unroll_skip_after", 32, 0, 1000,
              Full_Unroll_Skip_After),
    LNOPT_U32("full_unroll_skip_equal", "full_unroll_skip_equal", 32, 0, 1000,
              Full_Unroll_Skip_Equal),
    LNOPT_U32("skip_before", "skip_b", 32, 0, 10000, Skip_Before),
    LNOPT_U32("skip_after", "skip_a", 32, 0, 10000, Skip_After),
    LNOPT_U32("skip_equal", "skip_e", 32, 0, 10000, Skip_Equal),
    LNOPT_U32("apo_skip_before", "apo_skip_before", 32, 0, 10000,
              Apo_Skip_Before),
    LNOPT_U32("apo_skip_after", "apo_skip_after", 32, 0, 10000, Apo_Skip_After),
    LNOPT_U32("apo_skip_equal", "apo_skip_equal", 32, 0, 10000, Apo_Skip_Equal),
    LNOPT_U32("apo_loop_skip_before", "apo_loop_skip_before", 32, 0, 10000,
              Apo_Loop_Skip_Before),
    LNOPT_U32("apo_loop_skip_after", "apo_loop_skip_after", 32, 0, 10000,
              Apo_Loop_Skip_After),
    LNOPT_U32("apo_loop_skip_equal", "apo_loop_skip_equal", 32, 0, 10000,
              Apo_Loop_Skip_Equal),
    LNOPT_U32("dummy_skip_before", "dummy_skip_before", 32, 0, 10000,
              Dummy_Skip_Before),
    LNOPT_U32("dummy_skip_after", "dummy_skip_after", 32, 0, 10000,
              Dummy_Skip_After),
    LNOPT_U32("dummy_skip_equal", "dummy_skip_equal", 32, 0, 10000,
              Dummy_Skip_Equal),
#endif /* KEY */
    MHOPT_I32_SET_DUP("dirty_miss_penalty", 0, 0, 1000000000,
                      Dirty_Miss_Penalty, DMP_Set),
    MHOPT_I32_SET_DUP("dmp", 0, 0, 1000000000, Dirty_Miss_Penalty, DMP_Set),
    LNOPT_BOOL("fancy_tile", NULL, Fancy_tile),
    LNOPT_BOOL("ff", NULL, Run_fiz_fuse),
#ifdef TARG_X8664
    LNOPT_U32("fission", "fis", 0, 0, 2, Fission),
#else
    LNOPT_U32("fission", "fis", 1, 0, 2, Fission),
#endif
    LNOPT_U32("fission_inner_register_limit", NULL, 32, 0, 99999,
              Fission_inner_register_limit),
    LNOPT_BOOL("forward_substitution", NULL, Forward_substitution),
    LNOPT_U32("fusion", "fus", 1, 0, 2, Fusion),
    LNOPT_U32("fusion_peeling_limit", NULL, 5, 0, 99999, Fusion_peeling_limit),
    LNOPT_U32("gather_scatter", "gath", 1, 0, 100, Gather_Scatter),
    LNOPT_U32("gc", NULL, 0xfffe, 0, 0xfffe, Graph_capacity),
    LNOPT_BOOL("hmb", NULL, Hoist_messy_bounds),
    LNOPT_BOOL("ignore_pragmas", NULL, Ignore_pragmas),
    LNOPT_BOOL("interchange", NULL, Interchange),
    MHOPT_BOOL_SET_DUP("is_memory_level", Is_Mem_Level, Is_Mem_Level_Set),
    MHOPT_BOOL_SET_DUP("is_mem", Is_Mem_Level, Is_Mem_Level_Set),
    LNOPT_BOOL_SET("lego", NULL, Run_lego, Run_lego_set),
    LNOPT_BOOL("lego_local", NULL, Run_lego_localizer),
    MHOPT_I32_DUP("line_size", 0, 0, 8192, Line_Size),
    MHOPT_I32_DUP("ls", 0, 0, 8192, Line_Size),
    MHOPT_I32("loop_overhead_base", NULL, 1, 0, 1000, Loop_Overhead_Base),
    MHOPT_I32("lob", NULL, 1, 0, 1000, Loop_Overhead_Base),
    MHOPT_I32("loop_overhead_memref", NULL, 1, 0, 1000, Loop_Overhead_Memref),
    MHOPT_I32("lom", NULL, 1, 0, 1000, Loop_Overhead_Memref),
    LNOPT_U32("max_depth", NULL, 10, 0, 16, Max_do_loop_depth_strict),
    LNOPT_BOOL("mem_sim", NULL, Mem_sim),
    LNOPT_BOOL("minvariant", "minvar", Minvar),
    MHOPT_I32_SET_DUP("miss_penalty", 0, 0, 1000000000, Miss_Penalty,
                      Miss_Penalty_Set),
    MHOPT_I32_SET_DUP("mp", 0, 0, 1000000000, Miss_Penalty, Miss_Penalty_Set),
    MHOPT_BOOL("non_blocking_loads", NULL, Non_Blocking_Loads),
    MHOPT_BOOL("nbl", NULL, Non_Blocking_Loads),
    LNOPT_U32("opt", NULL, 1, 0, 1, Opt),
    LNOPT_U32("optimize_cache", NULL, 2, 0, 2, Cache_model),
    LNOPT_U32("optc", NULL, 2, 0, 2, Cache_model),
    LNOPT_BOOL("outer", NULL, Run_outer),
#ifdef KEY
    LNOPT_U32("olf_ub", NULL, 30, 0, 200, OLF_Upper_Bound),
    LNOPT_U32("olf_lb", NULL, 15, 0, 200, OLF_Lower_Bound),
#endif
    LNOPT_U32("outer_unroll", NULL, 0, 1, 32, Outer_unroll),
    LNOPT_U32("ou", NULL, 0, 1, 32, Outer_unroll),
    LNOPT_BOOL("outer_unroll_deep", NULL, Outer_unroll_deep),
    LNOPT_BOOL("ou_deep", NULL, Outer_unroll_deep),
    LNOPT_U32("outer_unroll_further", NULL, 6, 0, 99999,
              Outer_unroll_min_for_further_unroll),
    LNOPT_U32("ou_further", NULL, 6, 0, 99999,
              Outer_unroll_min_for_further_unroll),
    LNOPT_U32("outer_unroll_max", NULL, 0, 1, 32, Outer_unroll_max),
    LNOPT_U32("ou_max", NULL, 0, 1, 32, Outer_unroll_max),
    LNOPT_BOOL("outer_unroll_model_only", NULL, Outer_unroll_model_only),
    LNOPT_BOOL("ou_model_only", NULL, Outer_unroll_model_only),
    LNOPT_U32("outer_unroll_prod_max", NULL, 0, 1, 64, Outer_unroll_prod_max),
    LNOPT_U32("ou_prod_max", NULL, 0, 1, 64, Outer_unroll_prod_max),
    LNOPT_BOOL("outer_unroll_unity", NULL, Outer_unroll_unity),
    LNOPT_BOOL("ou_unity", NULL, Outer_unroll_unity),
    LNOPT_U32("ou_aggre", NULL, 2, 0, 3, Outer_unroll_aggre),
    LNOPT_U32("p3", NULL, 0, 0, 2, Run_p3),
    MHOPT_I32_DUP("page_size", 0, 0, 8192, Page_Size),
    MHOPT_I32_DUP("ps", 0, 0, 8192, Page_Size),
    MHOPT_I32_DUP("pct_xwrites_nonhidable", 0, 0, 100,
                  Pct_Excess_Writes_Nonhidable),
    MHOPT_I32_DUP("xwn", 0, 0, 100, Pct_Excess_Writes_Nonhidable),
    LNOPT_BOOL("plower", NULL, Pseudo_lower),
    LNOPT_BOOL("plower_mp", NULL, Pseudo_lower),
#ifndef KEY
    LNOPT_U32_SET("prefetch", "pref", 0, 0, 2, Run_prefetch, Run_prefetch_set),
#else
    LNOPT_U32_SET("prefetch", "pref", NO_PREFETCH, NO_PREFETCH,
                  AGGRESSIVE_PREFETCH, Run_prefetch, Run_prefetch_set),
    LNOPT_BOOL_SET("prefetch_stores", NULL, Prefetch_stores,
                   Prefetch_stores_set),
    LNOPT_BOOL("pf_inv_stride", NULL, Prefetch_invariant_stride),
    LNOPT_U32("pf_stride_ahead", NULL, 8, 0, 128, Prefetch_stride_ahead),
#endif
    LNOPT_U32("prefetch_ahead", NULL, 2, 0, 50, Prefetch_ahead),
    LNOPT_U32("pf_ahead", NULL, 2, 0, 50, Prefetch_ahead),
#ifdef TARG_IA64
    LNOPT_U32("prefetch_iters_ahead", NULL, 8, 0, 50, Prefetch_iters_ahead),
    LNOPT_U32("pf_it_ahead", NULL, 8, 0, 50, Prefetch_iters_ahead),
#else
    LNOPT_U32("prefetch_iters_ahead", NULL, 2, 0, 50, Prefetch_iters_ahead),
    LNOPT_U32("pf_it_ahead", NULL, 2, 0, 50, Prefetch_iters_ahead),
#endif
    LNOPT_U32("prefetch_cache_factor", NULL, 1, 1, 50, Prefetch_cache_factor),
    LNOPT_U32("pf_cf", NULL, 1, 1, 50, Prefetch_cache_factor),
    LNOPT_BOOL("prefetch_indirect", "", Prefetch_indirect),
    MHOPT_BOOL_DUP("prefetch_level", Prefetch_Level),
    MHOPT_BOOL_DUP("pf", Prefetch_Level),
    LNOPT_BOOL_SET("prefetch_manual", "", Run_prefetch_manual,
                   Run_prefetch_manual_set),
    LNOPT_BOOL_SET("pf_manual", NULL, Run_prefetch_manual,
                   Run_prefetch_manual_set),
    LNOPT_BOOL("pwr2", NULL, Power_of_two_hack),
    LNOPT_BOOL("sclrze", NULL, Sclrze),
    LNOPT_U32("se_tile_size", NULL, 0, 0, 99999, SE_tile_size),
    LNOPT_U32("split_tile", NULL, 1, 0, 2, Split_tiles),
    LNOPT_U32("split_tile_size", NULL, 0, 0, 99999, Split_tiles_size),
    LNOPT_BOOL("test", NULL, Run_test),
    LNOPT_BOOL("testdump", NULL, Test_dump),
    MHOPT_I32_SET_DUP("tlb_clean_miss_penalty", 0, 0, 1000000000,
                      TLB_Clean_Miss_Penalty, TLB_CMP_Set),
    MHOPT_I32_SET_DUP("tlbcmp", 0, 0, 1000000000, TLB_Clean_Miss_Penalty,
                      TLB_CMP_Set),
    MHOPT_I32_SET_DUP("tlb_dirty_miss_penalty", 0, 0, 1000000000,
                      TLB_Dirty_Miss_Penalty, TLB_DMP_Set),
    MHOPT_I32_SET_DUP("tlbdmp", 0, 0, 1000000000, TLB_Dirty_Miss_Penalty,
                      TLB_DMP_Set),
    MHOPT_I32_DUP("tlb_entries", 0, 0, 8192, TLB_Entries),
    MHOPT_I32_DUP("tlb", 0, 0, 8192, TLB_Entries),
    MHOPT_I32_SET_DUP("tlb_miss_penalty", 0, 0, 1000000000, TLB_Miss_Penalty,
                      TLB_MP_Set),
    MHOPT_I32_SET_DUP("tlbmp", 0, 0, 1000000000, TLB_Miss_Penalty, TLB_MP_Set),
    MHOPT_BOOL("tlb_noblocking_model", NULL, TLB_NoBlocking_Model),
    MHOPT_I32("tlb_trustworthiness", NULL, 1, 0, 1000, TLB_Trustworthiness),
    LNOPT_BOOL("trapezoidal_outer_unroll", NULL, Trapezoidal_outer_unroll),
    LNOPT_BOOL("use_malloc", NULL, Use_malloc),
    LNOPT_BOOL("use_parm", NULL, Use_parm),
    LNOPT_BOOL("version_mp_loops", NULL, Version_mp_loops),
#ifndef KEY
    LNOPT_BOOL("vintr", NULL, Run_vintr),
#else
    LNOPT_U32_SET("vintr", "vintr", 1, 0, 2, Run_vintr, Run_vintr_set),
    LNOPT_BOOL("vintr_verbose", NULL, Vintr_Verbose),
    LNOPT_U32_SET("simd", "simd", 1, 0, 2, Run_simd, Run_simd_set),
    LNOPT_BOOL("simd_verbose", NULL, Simd_Verbose),
    LNOPT_BOOL("simd_reduction", "simd_red", Simd_Reduction),
    LNOPT_BOOL("simd_avoid_fusion", NULL, Simd_Avoid_Fusion),
    LNOPT_BOOL("hoistif", NULL, Run_hoistif),
    LNOPT_BOOL("ignore_feedback", NULL, Ignore_Feedback),
    LNOPT_BOOL("unswitch", NULL, Run_unswitch),
    LNOPT_BOOL("unswitch_phase1", NULL, Run_unswitch_phase1),
    LNOPT_BOOL("unswitch_phase2", NULL, Run_unswitch_phase2),
    LNOPT_BOOL("unswitch_verbose", NULL, Unswitch_Verbose),
    LNOPT_BOOL("prefetch_verbose", NULL, Prefetch_Verbose),
    LNOPT_BOOL("build_scalar_reductions", NULL, Build_Scalar_Reductions),
#endif /* KEY */
    LNOPT_BOOL("oinvar", NULL, Run_oinvar),
    LNOPT_U32("doacross", NULL, 1, 0, 4, Run_doacross),
    LNOPT_U32("preferred_doacross_tile_size", NULL, 0, 0, 999999,
              Preferred_doacross_tile_size),
#ifndef TARG_IA64
    LNOPT_U32("parallel_overhead", NULL, 4096, 0, 0x7fffffff,
              Parallel_overhead),
#else
    LNOPT_U32("parallel_overhead", NULL, 2600, 0, 0x7fffffff,
              Parallel_overhead),
#endif
    LNOPT_BOOL("prompl", NULL, Prompl),
    LNOPT_BOOL("ifminmax", NULL, IfMinMax),
    LNOPT_BOOL("call_info", NULL, Run_call_info),
    LNOPT_BOOL("loop_finalize", NULL, Loop_finalization),
    LNOPT_BOOL("shackle", NULL, Shackle),
    LNOPT_BOOL("cross_loop", NULL, Cross_loop),
    LNOPT_BOOL("ipa", NULL, IPA_Enabled),
#ifndef TARG_IA64
    LNOPT_U32("trip_count_assumed_when_unknown", "trip_count", 1000, 0,
              UINT32_MAX, Num_Iters),
#else
    LNOPT_U32("num_iters", NULL, 100, 0, UINT32_MAX, Num_Iters),
#endif
    LNOPT_U32("pure", NULL, 1, 0, 2, Pure_Level),
    LNOPT_U32("small_trip_count", NULL, 5, 0, 10, Small_trip_count),
#ifdef TARG_IA64
    LNOPT_U32("assume_unknown_trip_count", NULL, 1000, 0, 1000,
              Assume_Unknown_Trip_Count),
#endif
    LNOPT_U32("local_pad_size", NULL, 0, 0, 1000, Local_pad_size),
    LNOPT_U32("full_unroll", "fu", 0, 0, 100, Full_unrolling),
#ifdef KEY
    LNOPT_U32("full_unroll_size", NULL, 2000, 0, 10000,
              Full_unrolling_loop_size_limit),
    LNOPT_BOOL("full_unroll_outer", NULL, Full_Unroll_Outer),
    LNOPT_U32("processors", NULL, 0, 0, 10000, Num_Processors),
    LNOPT_U32("parallel_pp_overhead", NULL, 128, 0, 0x7fffffff,
              Parallel_per_proc_overhead),
    LNOPT_BOOL("apo_use_feedback", NULL, Apo_use_feedback),
#endif

    {OVK_COUNT} /* List terminator -- must be last */
};

#undef IL

/* ====================================================================
 *
 * LNO_Init_Config
 *
 * Initialize the current top of stack to defaults, by copying the
 * Default_LNO struct to it.  Be careful not to lose the next
 * pointer.
 *
 * WARNING:  We are always picking up the default Mhd_Options struct
 * here.
 *
 * ====================================================================
 */

void LNO_Init_Config(void) {
  LNO_FLAGS *next = LNO_FLAGS_next(Current_LNO);

  *Current_LNO = Default_LNO;
  LNO_FLAGS_next(Current_LNO) = next;
  LNO_FLAGS_mhd(Current_LNO) = &Mhd_Options;
}

/* ====================================================================
 *
 * LNO_Push_Config
 *
 * Push a new struct on top of stack, either a copy of the current
 * TOS, or the defaults.
 *
 * TODO:  To make the option group printing functionality work once the
 * configuration structs start changing, this function must also update
 * the addresses in the group descriptor to point to the current
 * top-of-stack structure.
 *
 * ====================================================================
 */

void LNO_Push_Config(BOOL use_default) {
  LNO_FLAGS *new_flags = (LNO_FLAGS *)malloc(sizeof(LNO_FLAGS));

  if (new_flags == NULL) {
    ErrMsg(EC_No_Mem, "LNO_Push");
  }

  *new_flags = use_default ? Default_LNO : *Current_LNO;
  LNO_FLAGS_next(new_flags) = Current_LNO;
  LNO_FLAGS_mhd(new_flags) = &Mhd_Options;
  Current_LNO = new_flags;
}

/* ====================================================================
 *
 * LNO_Pop_Config
 *
 * Pop a struct from top of stack and return TRUE if the old TOS was
 * not the original TOS, or do nothing and return FALSE.
 *
 * TODO:  To make the option group printing functionality work once the
 * configuration structs start changing, this function must also update
 * the addresses in the group descriptor to point to the current
 * top-of-stack structure.
 *
 * ====================================================================
 */

BOOL LNO_Pop_Config(void) {
  if (Current_LNO->next == NULL) {
    /* This is the bottom of the stack: */
    return FALSE;
  } else {
    /* Deallocate the top element and pop it: */
    LNO_FLAGS *new_flags = LNO_FLAGS_next(Current_LNO);
    free(Current_LNO);
    Current_LNO = new_flags;
    return TRUE;
  }
}

/* ====================================================================
 *
 * LNO_Configure
 *
 * Configure the current top of stack struct.
 *
 * ====================================================================
 */

void LNO_Configure(void) {
  INT i;

#ifdef COMPILER_LICENSING
  /* check licensing for automatic parallelization */
  if (Run_autopar) {
    get_license("auto_pp", FALSE,
                TRUE); /* soft licensing == FALSE; tier licensing == TRUE */
  }
#endif

  /* This option is actually a trace flag: */
  LNO_Verbose = Get_Trace(TP_LNOPT, 0x0004);

  /* Unrolling: of LNO_Outer_Unroll, LNO_Outer_Unroll_Max, and
   * LNO_Outer_Unroll_Prod_Max, only the first OR either or both of the
   * last two may be non-zero (i.e. meaningful).
   */
  if (LNO_Outer_Unroll != 0 && LNO_Outer_Unroll_Max != 0) {
    ErrMsg(EC_Opt_Conflict, "-LNO:outer_unroll", "-LNO:outer_unroll_max",
           "-LNO:outer_unroll");
    LNO_Outer_Unroll_Max = 0;
  }
  if (LNO_Outer_Unroll != 0 && LNO_Outer_Unroll_Prod_Max != 0) {
    ErrMsg(EC_Opt_Conflict, "-LNO:outer_unroll", "-LNO:outer_unroll_prod_max",
           "-LNO:outer_unroll");
    LNO_Outer_Unroll_Prod_Max = 0;
  }
  if (LNO_Outer_Unroll == 0 && LNO_Outer_Unroll_Max == 0 &&
      LNO_Outer_Unroll_Prod_Max == 0) {
    LNO_Outer_Unroll_Prod_Max = DEFAULT_UNROLL_PROD_MAX;
  }
  if (LNO_Outer_Unroll == 0 && LNO_Outer_Unroll_Max == 0) {
    LNO_Outer_Unroll_Max = DEFAULT_UNROLL_MAX;
  }

  /* Cache parameters */

  for (i = 0; i < 4; i++) {
    if (Mhd_Options.L[i].CS_string != NULL &&
        !Atoi_KMG(Mhd_Options.L[i].CS_string, &Mhd_Options.L[i].Size, TRUE)) {
      char buffer[256];

      sprintf(buffer, "-LNO:cache_size%d=%s", i, Mhd_Options.L[i].CS_string);
      ErrMsg(EC_Inv_Opt_Val, buffer);
      Mhd_Options.L[i].Size = 0;
    }

    if (Mhd_Options.L[i].Is_Mem_Level == 1)
      Mhd_Options.L[i].Type = MHD_TYPE_MEM;
    else if (Mhd_Options.L[i].Is_Mem_Level == 0)
      Mhd_Options.L[i].Type = MHD_TYPE_CACHE;

    if (Mhd_Options.L[i].Miss_Penalty_Set) {
      if (Mhd_Options.L[i].CMP_Set) {
        ErrMsg(EC_Opt_Conflict, "-LNO:miss_penalty", "-LNO:clean_miss_penalty",
               "-LNO:miss_penalty");
      }
      if (Mhd_Options.L[i].DMP_Set) {
        ErrMsg(EC_Opt_Conflict, "-LNO:miss_penalty", "-LNO:dirty_miss_penalty",
               "-LNO:miss_penalty");
      }
      Mhd_Options.L[i].Clean_Miss_Penalty = Mhd_Options.L[i].Miss_Penalty;
      Mhd_Options.L[i].Dirty_Miss_Penalty = Mhd_Options.L[i].Miss_Penalty;
    }
    if (Mhd_Options.L[i].TLB_Miss_Penalty != -1) {
      if (Mhd_Options.L[i].TLB_CMP_Set) {
        ErrMsg(EC_Opt_Conflict, "-LNO:tlb_miss_penalty",
               "-LNO:tlb_clean_miss_penalty", "-LNO:tlb_miss_penalty");
      }
      if (Mhd_Options.L[i].TLB_DMP_Set) {
        ErrMsg(EC_Opt_Conflict, "-LNO:tlb_miss_penalty",
               "-LNO:tlb_dirty_miss_penalty", "-LNO:tlb_miss_penalty");
      }
      Mhd_Options.L[i].TLB_Clean_Miss_Penalty =
          Mhd_Options.L[i].TLB_Miss_Penalty;
      Mhd_Options.L[i].TLB_Dirty_Miss_Penalty =
          Mhd_Options.L[i].TLB_Miss_Penalty;
    }
  }
}
