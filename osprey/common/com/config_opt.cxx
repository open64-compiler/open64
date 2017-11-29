/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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
 * Module: config_opt.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_opt.cxx,v $
 *
 * Revision history:
 *  08-Sep-94 - Original Version (wodriver.c)
 *  15-Aug-96 - config_opt.c extracted from config.c
 *
 * Description:
 *
 * Configure the -OPT group (included in config.cxx).
 * Used from front ends, ipl, inline, ipa (ld), and be.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_opt_rcs_id = "$Source: common/com/SCCS/s.config_opt.cxx $ $Revision: 1.35 $";
#endif /* _KEEP_RCS_ID */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_opt.h"
extern BOOL WOPT_Enable_Goto;	/* In case config_wopt.h is later */

/* ====================================================================
 *
 * Global flag variables which reflect the -OPT group options.
 *
 * ====================================================================
 */

/***** Optimization Warning Messages *****/
BOOL Show_OPT_Warnings = TRUE;          /* Display OPT warning messages */

/***** Aliasing control *****/
OPTION_LIST *Alias_Option = NULL;
INT32 Alias_Query_Limit=INT32_MAX;
char *Alias_Query_File=NULL;
INT32 Alias_Nystrom_Solver_Track=0;
BOOL Alias_Nystrom_Global_Cycle_Detection = TRUE;
BOOL Alias_Pointer_Parms = TRUE;        /* Parms ptr indep? */
BOOL Alias_Pointer_Cray = FALSE;        /* Cray pointer semantics? */
#if defined(TARG_SL)
BOOL Alias_Pointer_Types = TRUE;	/* Ptrs to distinct basic types indep? */
#else
BOOL Alias_Pointer_Types = FALSE;       /* Ptrs to distinct basic types indep? */
#endif
BOOL Alias_Not_In_Union  = FALSE;	/* Ptrs point to non-union types */
BOOL Alias_Pointer_Strongly_Typed = FALSE; /* Ptrs to distinct types indep? */
BOOL Alias_Pointer_Named_Data = FALSE;	/* No pointers to named data? */
BOOL Alias_Pointer_Restricted = FALSE;	/* *p and *q not aliased? */
BOOL Alias_Pointer_Disjoint   = FALSE;	/* **p and **q not aliased? */
BOOL Alias_Common_Scalar = FALSE;       /* Distinguish scalar from array */
/* We will normally default Alias_Pointer_Types to TRUE, but can't
 * for K&R C.  This option is set by the driver for K&R C compilations
 * for use in overriding the default -- not intended for user use.
 */
static BOOL Alias_Pointer_Cckr = FALSE;	/* -cckr default rules? */
#if defined(TARG_SL)
static BOOL Alias_Pointer_Types_Set = TRUE;	/* alias=typed set? */
#else
static BOOL Alias_Pointer_Types_Set = FALSE;	/* alias=typed set? */
#endif
static BOOL Alias_Not_In_Union_Set  = FALSE;	/* alias=nounion set? */
BOOL  Alias_F90_Pointer_Unaliased = FALSE;  /* Are F90 pointers unaliased? */

BOOL  Alias_Nystrom_Analyzer = FALSE;  /* Using Nystrom-based alias analysis? */
BOOL  Alias_Analyzer_in_IPA = FALSE;  /* Create nystrom alias in IPA */
UINT32 Current_IPANode_File_PU_Idx = 0; /* current processing IPA node's file pu index */ 

/***** Alignment control *****/
BOOL Align_Object = TRUE;	/* Try to improve alignment of objects */
BOOL Align_Padding = FALSE;	/* Pad objects to natural alignment */

/* These alignment options are only relevant in the back end: */
#ifdef BACK_END
# define ALIGN_INSTS	  &Align_Instructions
#else
# define ALIGN_INSTS	  &Ignore_Int
#endif

/***** Constant folding (simplifier) options *****/
BOOL Enable_Cfold_Aggressive = FALSE;	/* Complex constant folding? */
static BOOL Cfold_Aggr_Set = FALSE;	/* ... option seen? */
BOOL Enable_Cfold_Reassociate = FALSE;	/* Re-association allowed? */
static BOOL Cfold_Reassoc_Set = FALSE;	/* ... option seen? */
BOOL Enable_Cfold_Intrinsics = FALSE;	/* Intrinsic constant folding? */
BOOL Cfold_Intrinsics_Set = FALSE;	/* ... option seen? */
#ifdef TARG_X8664
BOOL CIS_Allowed = TRUE;	/* combine sin(x) and cos(x) to cis(x) */
#else
BOOL CIS_Allowed = FALSE;	/* combine sin(x) and cos(x) to cis(x) */
#endif
static BOOL CIS_Set = FALSE;	/* ... option seen? */
BOOL Enable_CVT_Opt= FALSE;	/* Remove useless convert operators */
BOOL Enable_CVT_Opt_Set= FALSE;	/* ... option seen? */
BOOL Optimize_CVTL_Exp = FALSE;	/* Optimize expansion of CVTL operators */
BOOL Div_Split_Allowed = FALSE;		/* change a/b --> a*1/b ? */
static BOOL Div_Split_Set = FALSE;	/* ... option seen? */
BOOL Fast_Exp_Allowed = FALSE;		/* Avoid exp() calls? */
static BOOL Fast_Exp_Set = FALSE;	/* ... option seen? */
BOOL Fast_IO_Allowed = FALSE;		/* Fast printf/scanf/printw ? */
static BOOL Fast_IO_Set = FALSE;	/* ... option seen? */
BOOL Fast_Sqrt_Allowed = FALSE;		/* sqrt(x) --> x * rsqrt(x) ? */
static BOOL Fast_Sqrt_Set = FALSE;	/* ... option seen? */
#ifdef TARG_X8664
UINT32 Rsqrt_Allowed = 0;		/* 0: use SQRT and DIV		    */
					/* 1: use RSQRT with Newton-Raphson */
					/* 2: use RSQRT			    */
#else
BOOL Rsqrt_Allowed = FALSE;		/* generate RSQRT instruction? */
#endif
static BOOL Rsqrt_Set = FALSE;		/* ... option seen? */
BOOL Recip_Allowed = FALSE;	        /* generate RECIP instruction? */
static BOOL Recip_Set = FALSE;		/* ... option seen? */
BOOL Simp_Fold_Unsigned_Relops = FALSE; /* Constant fold unsigned relops */
static BOOL Simp_Fold_Unsigned_Relops_Set = FALSE;
/* Allow folding which might cause error if overflow occurs? */
BOOL Simp_Unsafe_Relops = FALSE;
static BOOL Simp_Unsafe_Relops_Set = FALSE;
BOOL Simp_Canonicalize = TRUE;          /* Simplifier canonicalization */
BOOL Enable_WN_Simp = TRUE;             /* Use the WHIRL simplifier */
static BOOL Enable_WN_Simp_Set=FALSE;   /* ... option seen? */
# ifdef KEY
INT32 Enable_WN_Simp_Expr_Limit = -1;
#if defined(TARG_SL) 
INT32 OPT_Madd_Height = 0;
#else
INT32 OPT_Madd_Height = 4;
#endif
# endif
BOOL GCM_Eager_Null_Ptr_Deref = FALSE; /* allow speculation past NULL ptr
					  test. assumes page zero as
					  readable */
BOOL GCM_Eager_Null_Ptr_Deref_Set=FALSE;   /* ... option seen? */
BOOL GCM_Speculative_Ptr_Deref= TRUE;  /* allow load speculation of a memory
					  reference that differs by a small
					  offset from some reference location */
BOOL GCM_Speculative_Ptr_Deref_Set=FALSE;   /* ... option seen? */

/***** Limits on optimization *****/
#ifdef TARG_IA64
#define DEFAULT_OLIMIT		24000
#define DEFAULT_O3_OLIMIT	30000	/* allow more time for -O3 compiles */
#else
#define DEFAULT_OLIMIT          6000
#define DEFAULT_O3_OLIMIT       9000    /* allow more time for -O3 compiles */
#endif

#define MAX_OLIMIT		INT32_MAX
INT32 Olimit = DEFAULT_OLIMIT;
static BOOL Olimit_Set = FALSE;
BOOL Olimit_opt = FALSE;	/* use regions? */
static BOOL Olimit_opt_Set = FALSE;

/* Debugging Flags for All Optimizations */
static OPTION_LIST *Opt_Skip = NULL;		/* Raw list */
SKIPLIST *Optimization_Skip_List = NULL;	/* Processed list */
static OPTION_LIST *Region_Skip = NULL;		/* Raw list */
SKIPLIST *Region_Skip_List = NULL;		/* Processed list */
#ifdef KEY
static OPTION_LIST *Goto_Skip = NULL;		/* Raw list */
SKIPLIST *Goto_Skip_List = NULL;		/* Processed list */
#if defined(TARG_SL)
static OPTION_LIST *DDB_Skip = NULL;            /* for delete dead branch */
SKIPLIST *DDB_Skip_List = NULL;                 /* for delete dead branch */
#endif // TARG_SL
#endif

/***** Miscellaneous -OPT: group options *****/
char *Ofast = NULL;		/* -OPT:Ofast platform name */
BOOL OPT_Pad_Common = FALSE;	/* Do internal common block padding? */
BOOL OPT_Reorg_Common = FALSE;	/* Do common block reorganization (split)? */
BOOL OPT_Reorg_Common_Set = FALSE;	/* ... option seen? */
BOOL OPT_Unroll_Analysis = TRUE;	/* Enable unroll limitations? */
BOOL OPT_Unroll_Analysis_Set = FALSE;	/* ... option seen? */
BOOL OPT_Lower_Splitsinglecand = TRUE;
BOOL OPT_Lower_Splitsinglecand_Set = FALSE;
#if defined(TARG_NVISA)
BOOL OPT_Lower_Speculate = TRUE;	/* speculate CAND/CIOR */
#else
BOOL OPT_Lower_Speculate = FALSE;	/* speculate CAND/CIOR */
#endif
BOOL OPT_Lower_Speculate_Set = FALSE;	/* ... option seen? */
BOOL OPT_Lower_Treeheight = FALSE;	/* reassociate commutative ops */
static BOOL OPT_Lower_Treeheight_Set = FALSE;
BOOL OPT_Inline_Divide = TRUE;		/* inline divide sequences */
static BOOL OPT_Inline_Divide_Set = FALSE;
BOOL OPT_Space = FALSE;			/* various text space optimizations */
BOOL Early_MP_Processing = FALSE; /* Do mp lowerering before lno/preopt */
BOOL Implied_Do_Io_Opt = TRUE;	/* Do implied-do loop opt for I/O */
BOOL Cray_Ivdep=FALSE;		/* Use Cray meaning for Ivdep */
BOOL Liberal_Ivdep=FALSE;	/* Use liberal meaning for Ivdep */
BOOL Inhibit_EH_opt=FALSE;	/* Don't remove EH regions without calls */
BOOL Allow_wrap_around_opt = TRUE;
static BOOL Allow_wrap_around_opt_Set = FALSE;	/* ... option seen? */
BOOL Enable_GOT_Call_Conversion = FALSE;	/* %call16 -> %got_disp */
static BOOL Enable_GOT_Call_overridden = FALSE;	/* ... option seen? */
BOOL OPT_recompute_addr_flags = FALSE; /* recompute addr saved */
BOOL OPT_IPA_addr_analysis = TRUE; /* enable the use of IPA addr analysis result */
BOOL Delay_U64_Lowering = TRUE;/* Delay unsigned 64-bit lowering to after wopt */
BOOL OPT_shared_memory = TRUE;	// assume use of shared memory
/* Put each function in its own text section */
BOOL Section_For_Each_Function = FALSE;
BOOL Inline_Intrinsics_Early=FALSE;    /* Inline intrinsics just after VHO */
#if defined(TARG_PPC32)
BOOL Enable_extract_bits=FALSE;
#else
BOOL Enable_extract_bits=TRUE;     /* This is also forced off for MIPS and IA32 in
                                          config_targ.cxx */
#endif
#if defined(TARG_SL)
BOOL Enable_compose_bits=TRUE;
#else
BOOL Enable_compose_bits=FALSE;
#endif

#if defined(__linux__) || defined(BUILD_OS_DARWIN)
BOOL Enable_WFE_DFE = FALSE;
#endif /* __linux __ */


/***** Instrummentation Related Options *****/
#include "profile_type.h"
INT32 Instrumentation_Phase_Num = PROFILE_PHASE_BEFORE_VHO;  /* 0 */
INT32 Instrumentation_Type_Num  = WHIRL_PROFILE;             /* 1 */
BOOL Instrumentation_Enabled = FALSE;
UINT32 Instrumentation_Actions = 0;
BOOL Instrumentation_Unique_Output = FALSE; // always create unique output
OPTION_LIST *Feedback_Option = NULL;
BOOL Outlining_Enabled = FALSE;
BOOL Instrumentation_Enabled_Before = FALSE;

INT32  Optimize_exception_ranges = 1;
BOOL   Optimize_exception_ranges_set = FALSE;
#ifdef KEY
INT32  OPT_Cyg_Instrument = 0;
BOOL   Asm_Memory = FALSE;
BOOL   Align_Unsafe = FALSE; 
UINT32 Div_Exe_Counter = 40000;  /* A number that can avoid regression on facerec. */
UINT32 Div_Exe_Ratio = 50;      /* A cut-off percentage for value profiling. */
UINT32 Div_Exe_Candidates = 2;  /* The top entries that will be considered. */
UINT32 Mpy_Exe_Counter = 90000000;  
UINT32 Mpy_Exe_Ratio = 100;      /* A cut-off percentage for value profiling. */
BOOL   profile_arcs = FALSE;
UINT32 OPT_Lower_To_Memlib = 1;  /* transform to library calls */
INT32  OPT_Threshold_To_Memlib = 256;    /* threshold to transform to mem calls */
INT32  OPT_Enable_Lower_To_Memlib_Limit = -1;
BOOL   OPT_Enable_Simp_Fold = TRUE;  /* enables new float/complex/str/intrinsic foldings */

/* Use Fast Math equivalents (currently from ACML 2.0) 
   for the math library functions. */
BOOL  OPT_Fast_Math = FALSE; 
static BOOL OPT_Fast_Math_Set = FALSE; 

/* Use fast standard library equivalents for some libc functions. */
BOOL  OPT_Fast_Stdlib = TRUE;

/* Internal flag to control MP barrier optimization */
BOOL OPT_MP_Barrier_Opt = FALSE;

BOOL OPT_Icall_Instr = TRUE;	/* enables icall profiling */
BOOL OPT_Int_Value_Instr = TRUE;/* enables integer value profiling */
BOOL OPT_FP_Value_Instr = TRUE;	/* enables value_fp_bin profiling */

/* The following flags are set by GNU-compatible flags, these in turn
   enable other backend optimizations. */
BOOL OPT_Ffast_Math = FALSE;
static BOOL OPT_Ffast_Math_Set = FALSE;
BOOL OPT_Funsafe_Math_Optimizations = FALSE;
static BOOL OPT_Funsafe_Math_Optimizations_Set = FALSE;
BOOL    OPT_Float_Via_Int = FALSE; // when on, perform FP copies using int regs

UINT32 OPT_Malloc_Alg = 0;	/* select malloc algorithm */
BOOL OPT_Malloc_Alg_Set = FALSE; 
INT32 OPT_Hugepage_Heap_Limit = -1;  /* set huge page limit */
BOOL OPT_Hugepage_Heap_Set = FALSE;
INT32 OPT_Hugepage_Attr = 1;  /* set huge page mallopt */
BOOL OPT_Scale=FALSE; // Enable scalability optimization */
BOOL Early_Goto_Conversion = TRUE; // Goto conversion applied before VHO(C/C++)
BOOL Early_Goto_Conversion_Set = FALSE;
#endif	// KEY


BOOL OPT_Enable_WHIRL_SSA = FALSE;  // SSA on WHIRL, disabled by default
BOOL OPT_Enable_BUILD_WHIRL_SSA = FALSE;  // SSA on WHIRL, disabled by default
UINT32 OPT_Struct_Array_Copy = 1; 

// alias analyzer triage value
// all alias tag value less than AA_force_tag_alias_before_dim1 is 
// aliased with all other alias tags.
// all alias tag value less than AA_force_tag_alias_before_dim2 is 
// aliased with tag[AA_force_tag_alias_before_dim1-1]
// 
// triage is find the first error alias tag pair.
UINT32 AA_force_tag_alias_before_dim1 = 0;
UINT32 AA_force_tag_alias_before_dim2 = UINT32_MAX;

// enable control flow optimization for the program with EH regions
// by default it is only enabled in mainopt
BOOL  OPT_Enable_EH_CFG_OPT = FALSE;
BOOL  OPT_Enable_EH_CFG_OPT_Set = FALSE;

/***** Obsolete options *****/
static BOOL Fprop_Limit_Set = FALSE;

/* ====================================================================
 *
 * Descriptor for the -OPT option group.
 *
 * ====================================================================
 */

/* Optimization options: */
static OPTION_DESC Options_OPT[] = {
  { OVK_BOOL, OV_VISIBLE,	TRUE,	"warning",		"warn",
    0, 0, 0,  &Show_OPT_Warnings,     NULL,
    "Control interpretation of possible variable aliasing" },

#ifdef KEY
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "asm_memory", "",
    0, 0, 0,	&Asm_Memory,    NULL,
    "Assumes each asm has memory flag specified even if it is not there"},
  { OVK_BOOL,   OV_INTERNAL,    FALSE, "align_unsafe", "",
    0, 0, 0,    &Align_Unsafe,    NULL,
    "Generate aligned load/store even though it may be unsafe"},
#endif

  { OVK_LIST,	OV_VISIBLE,	TRUE, 	"alias",		"alia",
    0, 0, 0,	&Alias_Option,	NULL,
    "Control interpretation of possible variable aliasing" },

  { OVK_INT32,  OV_INTERNAL, TRUE,  "alias_query_limit", "alias_query_limit",
    INT32_MAX, 0, INT32_MAX,  &Alias_Query_Limit, NULL,
    "Upper bound on alias analysis query - beyond which may alias returned"
  },

  { OVK_NAME, OV_INTERNAL, TRUE, "alias_query_file", "alias_query_file",
      0, 0, 0, &Alias_Query_File, NULL,
    "File specifies responses to alias queries"
  },

  { OVK_INT32, OV_INTERNAL, TRUE, "nystrom_pts_track", "nystrom_pts_track",
      0, 0, INT32_MAX, &Alias_Nystrom_Solver_Track, NULL,
      "Track updates to the points-to set of provided node"
  },

  { OVK_BOOL, OV_INTERNAL, TRUE, "nystrom_global_cycle_detect",
        "nystrom_global_cycle_detect",
        0, 0, 0, &Alias_Nystrom_Global_Cycle_Detection, NULL,
        "Control the use of cycle detection in the ipa constraint graph"
  },

  { OVK_INT32,	OV_SHY,		TRUE, "align_instructions",	"align_i",
    16, 0, 1024, ALIGN_INSTS,	NULL,
    "Align subprogram entries and loops by given byte count" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "align_object",		"align_o",
    0, 0, 0,	&Align_Object,	NULL,
    "Allow realignment of objects to improve memory accesses" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "align_padding",		"align_p",
    0, 0, 0,	&Align_Padding,	NULL,
    "Allow padding of objects to improve memory alignment" },

  { OVK_INT32,  OV_INTERNAL,	TRUE,	"bblength",		"bb",
    DEF_BBLENGTH, MIN_BBLENGTH, MAX_BBLENGTH, &Split_BB_Length, NULL,
    "Restrict BB length by splitting longer BBs" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE,	"cis",			"cis",
    0, 0, 0,	&CIS_Allowed,	 &CIS_Set,
    "Convert sin/cos pairs into a single call" },

  { OVK_INT32,  OV_VISIBLE,	TRUE,	"const_copy_limit",	"const",
    DEF_CONST_COPY_TN_CNT, 0, INT32_MAX,&Const_Copy_TN_CNT, NULL,
    "Avoid constant/copy propagation if there are more than n expressions" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "cray_ivdep",		"cray_ivdep",
    0, 0, 0,    &Cray_Ivdep,	NULL,
    "IVDEP pragma/directive break parallelism-inhibiting dependencies" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "cvt",			"cvt",
    0, 0, 0,	&Enable_CVT_Opt,	&Enable_CVT_Opt_Set,
    "Optimize conversion operators" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "div_split",		"div_split",
    0, 0, 0,	&Div_Split_Allowed, &Div_Split_Set ,
    "Allow splitting of a/b into a*recip(b)" },

#ifdef KEY
  { OVK_UINT32,	OV_INTERNAL,	TRUE, "div_exe_counter",	"",
    0, 0, UINT32_MAX,	&Div_Exe_Counter, NULL ,
    "Restrict div/rem/mod optimization via value profiling" },
  { OVK_UINT32,	OV_INTERNAL,	TRUE, "div_exe_ratio",	"",
    0, 0, 100,	&Div_Exe_Ratio, NULL ,
    "Restrict div/rem/mod optimization via value profiling" },
  { OVK_UINT32,	OV_INTERNAL,	TRUE, "div_exe_candidates",	"",
    0, 0, 10,	&Div_Exe_Candidates, NULL ,
    "Restrict div/rem/mod optimization via value profiling" },
  { OVK_UINT32,	OV_INTERNAL,	TRUE, "mpy_exe_counter",	"",
    0, 0, UINT32_MAX,	&Mpy_Exe_Counter, NULL ,
    "Restrict mpy optimization via value profiling" },
  { OVK_UINT32,	OV_INTERNAL,	TRUE, "mpy_exe_ratio",	"",
    0, 0, 100,	&Mpy_Exe_Ratio, NULL ,
    "Restrict mpy optimization via value profiling" },
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "simp_debug", "simp_debug",
    0, 0, 0,	&OPT_Enable_Simp_Fold, NULL,
    "New foldings in simplifier" },

  { OVK_BOOL, OV_VISIBLE, 	TRUE,	"fast_math", 	"",
#ifdef TARG_IA64
    0, 0, 0, &OPT_Fast_Math, NULL,
#else
    0, 0, 0, &OPT_Fast_Math, &OPT_Fast_Math_Set,
#endif
    "Use Fast Math Library functions from ACML 2.0" },

  { OVK_BOOL, OV_VISIBLE, 	TRUE,	"fast_stdlib", 	"",
    0, 0, 0, &OPT_Fast_Stdlib, NULL,
    "Use fast versions of some standard library functions" },

  { OVK_BOOL, OV_INTERNAL, 	TRUE,	"mp_barrier_opt", "",
    0, 0, 0, &OPT_MP_Barrier_Opt, NULL,
    "Optimize generation of barrier calls for OpenMP" },

  { OVK_INT32, OV_VISIBLE,      TRUE,   "cyg_instr",    "",
    3, 0, 4, &OPT_Cyg_Instrument, NULL,
    "Insert calls to __cyg_profile_func_enter/exit" },

  { OVK_BOOL, OV_INTERNAL, 	TRUE,	"icall_instr", "",
    0, 0, 0, &OPT_Icall_Instr, NULL,
    "perform profiling of indirect calls to get their called functions" },

  { OVK_BOOL, OV_INTERNAL, 	TRUE,	"int_value_instr", "",
    0, 0, 0, &OPT_Int_Value_Instr, NULL,
    "perform profiling of integer values" },

  { OVK_BOOL, OV_INTERNAL, 	TRUE,	"fp_value_instr", "",
    0, 0, 0, &OPT_FP_Value_Instr, NULL,
    "perform profiling of floating-point values" },

  { OVK_BOOL, OV_INTERNAL, 	TRUE,	"ffast_math", "",
    0, 0, 0, &OPT_Ffast_Math, &OPT_Ffast_Math_Set,
    "Determines conformance to IEEE-754 arithmetic rules" },

  { OVK_BOOL, OV_INTERNAL, 	TRUE,	"scale", "",
    0, 0, 0, &OPT_Scale, NULL, "Perform scalability optimizations" },

  { OVK_BOOL, OV_INTERNAL, 	TRUE,	"funsafe_math_optimizations", "",
    0, 0, 0, &OPT_Funsafe_Math_Optimizations,
    &OPT_Funsafe_Math_Optimizations_Set,
    "Determines conformance to IEEE-754 arithmetic rules" },
  { OVK_BOOL, OV_INTERNAL,      TRUE,   "float_via_int", "",
    0, 0, 0, &OPT_Float_Via_Int, NULL,
    "perform floating-point memory copies using integer registers" },

  { OVK_UINT32, OV_VISIBLE,     TRUE,   "malloc_algorithm", "malloc_alg",
    0, 0, 3, &OPT_Malloc_Alg, &OPT_Malloc_Alg_Set,
    "Use alternate malloc algorithm" },

  { OVK_INT32, OV_VISIBLE,     TRUE,    "hugepage_heap_limit", NULL,
    0, -1, 50000, &OPT_Hugepage_Heap_Limit, &OPT_Hugepage_Heap_Set,
    "Set huge page heap limit" },

  { OVK_INT32, OV_VISIBLE, 	TRUE,	"hugepage_attr", NULL,
    1, 0, 127, &OPT_Hugepage_Attr, NULL,
    "Set mallopt in libhugetlbfs" },

  { OVK_BOOL,   OV_INTERNAL,    TRUE, "early_goto_conv", "",
    0, 0, 0,    &Early_Goto_Conversion, &Early_Goto_Conversion_Set,
    "Do GOTO conversion before VHO" },
#endif

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "early_mp",		"early_mp",
    0, 0, 0,	&Early_MP_Processing, NULL,
    "Lower before LNO" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_bit_intrinsics",	"fast_bit",
    0, 0, 0,	&Fast_Bit_Allowed,	&Fast_Bit_Set,
    "Don't check bit count range for Fortran bit intrinsics" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_complex",		"fast_co",
    0, 0, 0,	&Fast_Complex_Allowed,	&Fast_Complex_Set,
    "Use fast algorithms with limited domains for complex norm and divide" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_exp",		"fast_ex",
    0, 0, 0,	&Fast_Exp_Allowed,	&Fast_Exp_Set,
    "Use multiplication and square root for exp() where faster" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_io",		"fast_io",
    0, 0, 0,	&Fast_IO_Allowed,	&Fast_IO_Set,
    "Inline some C I/O routines if __INLINE_INTRINSICS is defined" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_nint",		"fast_nint",
    0, 0, 0,	&Fast_NINT_Allowed,	&Fast_NINT_Set,
    "Use IEEE rounding instead of Fortran rounding for NINT intrinsics" },

#ifdef TARG_X8664
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_anint",		"fast_anint",
    0, 0, 0,	&Fast_ANINT_Allowed,	&Fast_ANINT_Set,
    "Use IEEE rounding instead of Fortran rounding for ANINT intrinsics" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "vcast_complex",	"vcast_complex",
    0, 0, 0,	&Vcast_Complex,	&Vcast_Complex_Set,
    "Convert MTYPE_C8 to MTYPE_V16C8 in the lowerer" },
#endif

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_sqrt",		"fast_sq",
    0, 0, 0,	&Fast_Sqrt_Allowed,	&Fast_Sqrt_Set,
    "May use x*rsqrt(x) for sqrt(x) on machines where faster" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fast_trunc",		"fast_trunc",
    0, 0, 0,	&Fast_trunc_Allowed,	&Fast_trunc_Set,
    "Inline NINT and related intrinsics with limited-domain algorithm" },

  { OVK_BOOL,	OV_SHY,		TRUE, "fold_aggressive",	"fold_ag",
    0, 0, 0,	&Enable_Cfold_Aggressive, &Cfold_Aggr_Set,
    "Allow aggressive expression folding optimizations" },

  { OVK_BOOL,	OV_SHY,		TRUE, "fold_intrinsics",	"fold_i",
    0, 0, 0,	&Enable_Cfold_Intrinsics, &Cfold_Intrinsics_Set,
    "Allow expression folding of Fortran intrinsic calls" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fold_reassociate",	"fold_r",
    0, 0, 0,	&Enable_Cfold_Reassociate, &Cfold_Reassoc_Set,
    "Allow optimizations which reassociate floating point operators" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fold_unsafe_relops",	"fold_unsafe_relops",
    0, 0, 0,	&Simp_Unsafe_Relops, &Simp_Unsafe_Relops_Set,
    "Allow relational operator folding which may cause integer overflow" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fold_unsigned_relops",	"fold_unsigned",
    0, 0, 0,	&Simp_Fold_Unsigned_Relops, &Simp_Fold_Unsigned_Relops_Set,
    "Allow relop folding which may cause unsigned integer overflow" },

  { OVK_BOOL,   OV_VISIBLE,	TRUE, "got_call_conversion",  "got_call",
    0, 0, 0,    &Enable_GOT_Call_Conversion, &Enable_GOT_Call_overridden,
    "Allow function address loads to be moved out of loops" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "goto_conversion",	"goto",
    0, 0, 0,	&WOPT_Enable_Goto, NULL,
    "Enable conversion of GOTO to more structured constructs" },

  { OVK_INT32,	OV_VISIBLE,	TRUE, "IEEE_arithmetic",	"IEEE_a",
    1, 1, 3,	&IEEE_Arithmetic, &IEEE_Arith_Set,
    "Level of conformance to IEEE-754 arithmetic rules" },

  { OVK_BOOL,	OV_SHY,		TRUE, "IEEE_comparisons",	"IEEE_c",
    0, 0, 0,	&Force_IEEE_Comparisons, NULL,
    "Force conforming operations on IEEE-754 NaN and Inf values" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "IEEE_NaN_Inf",		"IEEE_N",
    0, 0, 0,	&Force_IEEE_Comparisons, NULL,
    "Force conforming operations on IEEE-754 NaN and Inf values" },

  { OVK_BOOL,	OV_SHY,		TRUE, "implied_do_io_opt",	NULL,
    1, 0, 0,	&Implied_Do_Io_Opt, NULL,
    "Optimize implied DO I/O to minimize calls in Fortran" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "inline_intrinsics",	"inline_intr",
    0, 0, 0,	&Inline_Intrinsics_Allowed, &Inline_Intrinsics_Set,
    "Allow inlining of Fortran intrinsic functions" },

  { OVK_BOOL,	OV_SHY,		TRUE, "ldx",			"ldx",
    0, 0, 0,	&Indexed_Loads_Allowed,	NULL,
    "Allow generation of indexed load/store operations" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "liberal_ivdep",	"liberal_ivdep",
    0, 0, 0,    &Liberal_Ivdep, NULL,
    "IVDEP pragmas/directives break all dependencies within an array" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "Inhibit_EH_opt",	"Inhibit_EH_opt",
    0, 0, 0,    &Inhibit_EH_opt, NULL,
    "Don't remove EH regions without calls" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "nary",			"nary",
    0, 0, 0,	&Enable_NaryExpr,	&Enable_NaryExpr_Set,
    "Allow N-ary tree height reduction of MADDs" },

  { OVK_NAME,	OV_SHY,		FALSE, "Ofast",			"Ofast",
    0, 0, 0,	&Ofast,		NULL,
    "Tailor options for performance on current target" },

  { OVK_INT32,	OV_VISIBLE,	FALSE, "Olimit",		"Ol",
    DEFAULT_OLIMIT, 0, MAX_OLIMIT,	&Olimit,	&Olimit_Set,
    "Limit size of subprograms which will be optimized" },

  { OVK_BOOL,	OV_INTERNAL,	FALSE, "Olimit_opt",		"Olimit_o",
    0, 0, 0,	&Olimit_opt,	&Olimit_opt_Set,
    "Use regions if Olimit exceeded" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "pad_common",		"pad",
    0, 0, 0,	&OPT_Pad_Common, NULL,
    "Force padding of COMMON blocks to improve cache behavior" },

#ifdef KEY
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "profile_arcs", "",
    0, 0, 0,	&profile_arcs, NULL,
    "arc profiling used by GCC"},
#endif

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "ptr_opt",		"ptr_o",
    0, 0, 0,	&Ptr_Opt_Allowed, NULL,
    "Allow treatment of pointers as arrays when possible in C" },

  { OVK_BOOL,	OV_INTERNAL,	FALSE, "rail",			"rail",
    0, 0, 0,	&Regions_Around_Inner_Loops, NULL,
    "Insert regions around inner loops" },

  { OVK_BOOL,	OV_INTERNAL,	FALSE, "rbi",			"rbi",
    0, 0, 0,	&Region_Boundary_Info, NULL,
    "Create region boundary information" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "recip",		"recip",
    0, 0, 0,	&Recip_Allowed, &Recip_Set,
    "Allow use of recip instruction" },

  { OVK_LIST,	OV_SHY,		FALSE, "region_skip_equal",	"region_skip_e",
    0, 0, 4096,	&Region_Skip,	NULL,
    "Skip optimization of this region" },

  { OVK_LIST,	OV_SHY,		FALSE, "region_skip_before",	"region_skip_b",
    0, 0, 4096,	&Region_Skip,	NULL,
    "Skip optimization of regions before this one" },

  { OVK_LIST,	OV_SHY,		FALSE, "region_skip_after",	"region_skip_a",
    0, 0, 4096,	&Region_Skip,	NULL,
    "Skip optimization of regions after this one" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "reorg_common",		"reorg",
    0, 0, 0,	&OPT_Reorg_Common, &OPT_Reorg_Common_Set,
    "Allow splitting of COMMON blocks to improve cache behavior" },

  { OVK_BOOL,	OV_VISIBLE,	FALSE, "procedure_reorder", "procedure_reorder",
    0, 0, 0,	&Section_For_Each_Function, NULL,
    "Place each function in its own .text section" },

  { OVK_INT32,  OV_VISIBLE,	TRUE, "roundoff",		"ro",
    2, 0, 3,	&Roundoff_Level, &Roundoff_Set,
    "Level of acceptable departure from source roundoff semantics" },

#ifdef TARG_X8664
  { OVK_UINT32,	OV_VISIBLE,	TRUE, "rsqrt",		"rsqrt",
    0, 0, 2,	&Rsqrt_Allowed, &Rsqrt_Set,
    "How to use rsqrt instructions" },
#else
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "rsqrt",		"rsqrt",
    0, 0, 0,	&Rsqrt_Allowed, &Rsqrt_Set,
    "Allow use of rsqrt instruction" },
#endif

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "shared_memory",	"shared_mem",
    0, 0, 0,	&OPT_shared_memory, NULL,
    "Assume use of shared memory" },

  { OVK_LIST,	OV_SHY,		FALSE, "skip_equal",		"skip_e",
    0, 0, 4096,	&Opt_Skip,	NULL,
    "Skip optimization of this subprogram" },

  { OVK_LIST,	OV_SHY,		FALSE, "skip_before",		"skip_b",
    0, 0, 4096,	&Opt_Skip,	NULL,
    "Skip optimization of subprograms before this one" },

  { OVK_LIST,	OV_SHY,		FALSE, "skip_after",		"skip_a",
    0, 0, 4096,	&Opt_Skip,	NULL,
    "Skip optimization of subprograms after this one" },

  { OVK_BOOL,	OV_VISIBLE,	FALSE, "space",		"sp",
    0, 0, 0,    &OPT_Space,	NULL,
    "Bias optimizations to minimize code space" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "split_single_cand",		"",
    0, 0, 0,	&OPT_Lower_Splitsinglecand, &OPT_Lower_Splitsinglecand_Set,
    "Allow splitting of single CAND for enabling if_conversion" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "speculate",		"",
    0, 0, 0,	&OPT_Lower_Speculate, &OPT_Lower_Speculate_Set,
    "Allow speculation for CAND/COR operators" },

  { OVK_BOOL,   OV_INTERNAL,    TRUE, "speculative_null_ptr_deref","",
    0, 0, 0,    &GCM_Eager_Null_Ptr_Deref, &GCM_Eager_Null_Ptr_Deref_Set,
    "Allow speculation of loads above NULL pointer tests" },

  { OVK_BOOL,   OV_INTERNAL,    TRUE, "speculative_ptr_deref","",
    0, 0, 0,    &GCM_Speculative_Ptr_Deref, &GCM_Speculative_Ptr_Deref_Set,
    "Allow speculative loads of memory locations that differ by a small offset from some referenced memory location" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "swp",			"swp",
    0, 0, 0,    &Enable_SWP,	&Enable_SWP_overridden,
    "Enable software pipelining" },

  { OVK_UINT32,   OV_INTERNAL,    FALSE, "transform_to_memlib",   "transform",
    1, 0, 2,    &OPT_Lower_To_Memlib, NULL,
    "Allow loop or struct memory copy/set to be library calls" },

  { OVK_INT32,  OV_INTERNAL,    TRUE, "threshold_to_memlib",    "",
    256, 0, INT32_MAX,    &OPT_Threshold_To_Memlib, NULL,
    "Threshold to transform loop copy/set to be memory library calls" },

  { OVK_INT32,  OV_INTERNAL,    TRUE, "memlib_limit", "memlib_limit",
    INT32_MAX, 0, INT32_MAX,    &OPT_Enable_Lower_To_Memlib_Limit, NULL },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "treeheight",		"",
    0, 0, 0,	&OPT_Lower_Treeheight, &OPT_Lower_Treeheight_Set,
    "Allow tree height reduction" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "inline_divide",		"",
    0, 0, 0,	&OPT_Inline_Divide, &OPT_Inline_Divide_Set,
    "Inline divide and remainder operations if possible" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "unroll_analysis",	"unroll_analysis",
    0, 0, 0,	&OPT_Unroll_Analysis,	&OPT_Unroll_Analysis_Set,
    "Analyze inner loop requirements before unrolling" },

  { OVK_INT32,	OV_VISIBLE,	TRUE, "unroll_size",		"unroll_s",
    0, 0, INT32_MAX, &OPT_unroll_size, &OPT_unroll_size_overridden,
    "Maximum size of loops to be unrolled" },

  { OVK_INT32,	OV_VISIBLE,	TRUE, "unroll_times_max",	"unroll_times",
    0, 0, INT32_MAX, &OPT_unroll_times, &OPT_unroll_times_overridden,
    "Maximum number of times to unroll loops" },

  { OVK_INT32,	OV_VISIBLE,	TRUE, "unroll_level",	"unroll_lev",
    0, 1, 2, &OPT_unroll_level, &OPT_unroll_level,
    "Aggressive level to unroll loops" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "keep_ext",	"keep_ext",
    0, 0, 0, &OPT_keep_extsyms, &OPT_keep_extsyms,
    "Preserve symbol info for externs under ipa" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "wn_simplify",		"wn_simp",
    0, 0, 0,	&Enable_WN_Simp, &Enable_WN_Simp_Set,
    "Enable simplifier" },

  { OVK_BOOL,	OV_VISIBLE,	TRUE, "lower_zdl",	"lower_zdl",
    0, 0, 0,	&OPT_Lower_ZDL,  &OPT_Lower_ZDL_Set,
    "Enable_Lower_ZDL" },

#ifdef KEY
  { OVK_INT32,  OV_INTERNAL,    TRUE, "simp_limit",             "",
    INT32_MAX, 0, INT32_MAX,    &Enable_WN_Simp_Expr_Limit, NULL },

  { OVK_LIST,	OV_SHY,		FALSE, "goto_skip_equal",	"goto_skip_e",
    0, 0, 4096,	&Goto_Skip,	NULL,
    "Skip goto conversion of this subprogram" },

  { OVK_LIST,	OV_SHY,		FALSE, "goto_skip_before",	"goto_skip_b",
    0, 0, 4096,	&Goto_Skip,	NULL,
    "Skip goto conversion of subprograms before this one" },

  { OVK_LIST,	OV_SHY,		FALSE, "goto_skip_after",	"goto_skip_a",
    0, 0, 4096,	&Goto_Skip,	NULL,
    "Skip goto conversion of subprograms after this one" },
#if defined(TARG_SL)
  // For delete-dead-branch
  { OVK_LIST,	OV_SHY,		FALSE, "ddb_skip_equal",	"ddb_skip_e",
    0, 0, 4096,	&DDB_Skip,	NULL,
    "Skip ddb of this branch" },

  { OVK_LIST,	OV_SHY,		FALSE, "ddb_skip_before",	"ddb_skip_b",
    0, 0, 4096,	&DDB_Skip,	NULL,
    "Skip ddb of branch before this one" },

  { OVK_LIST,	OV_SHY,		FALSE, "ddb_skip_after",	"ddb_skip_a",
    0, 0, 4096,	&DDB_Skip,	NULL,
    "Skip ddb of branch after this one" },
#endif
#ifdef TARG_MIPS
  { OVK_INT32,  OV_VISIBLE,    TRUE, "madd_height",    "",
    4, 1, INT32_MAX,    &OPT_Madd_Height, NULL,
    "Maximum length of MADD chain allowed before transforming it to sum of shorter MADD chains"},
#endif
#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "wrap_around_unsafe_opt", "wrap_around_unsafe",
    0, 0, 0,	&Allow_wrap_around_opt,	&Allow_wrap_around_opt_Set,
    "Allow LFTR which may wrap around MAX_INT" },

  /* intrinsic expansion for bzero/blkclr/bcopy/memset/memcpy/memmove */
  {OVK_BOOL,    OV_VISIBLE,	TRUE,	"emulate_memset",       "",
    0, 0, 0,    &Emulate_memset, NULL,
    "Enable inline expansion of memset" },
  {OVK_BOOL,    OV_VISIBLE,	TRUE,	"mem_intrinsics",       "",
    0, 0, 0,    &CG_mem_intrinsics, NULL,
    "Enable inline expansion of memory intrinsics (bzero, blkclr, bcopy, memset, memcpy, memmove)" },
  {OVK_INT32,   OV_VISIBLE,	TRUE,     "memmove_count",        "memmove",
    16, 0, INT32_MAX,   &CG_memmove_inst_count,&CG_memmove_inst_count_overridden,
    "Maximum size of inline expansion of memory intrinsics" },
  {OVK_BOOL,    OV_VISIBLE,	TRUE,     "bcopy_cannot_overlap",         "",
    0, 0, 0,    &CG_bcopy_cannot_overlap, NULL,
    "Assume that source and target of bcopy cannot overlap" },
  {OVK_BOOL,    OV_VISIBLE,	TRUE,     "memcpy_cannot_overlap",        "",
    0, 0, 0,    &CG_memcpy_cannot_overlap,      NULL,
    "Assume that source and target of memcpy cannot overlap" },
  {OVK_BOOL,    OV_VISIBLE,	TRUE,     "memmove_cannot_overlap", "",
    0, 0, 0,    &CG_memmove_cannot_overlap,     NULL,
    "Assume that source and target of memmove cannot overlap" },
  {OVK_BOOL,    OV_VISIBLE,	TRUE,	"memmove_nonconst",       "",
    0, 0, 0,    &CG_memmove_nonconst, NULL,
    "Enable inline expansion of memory intrinsics (bzero, blkclr, bcopy, memset, memcpy, memmove) whose size is not a constant" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "ipa_addr_analysis", "ipa_addr",
    0, 0, 0,	&OPT_IPA_addr_analysis, NULL,
    "Enable the use of IPA address analysis result in the backend"},

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "recompute_addr_flags", "",
    0, 0, 0,	&OPT_recompute_addr_flags, NULL,
    "Recompute address flags in the backend (for debugging)"},

  { OVK_BOOL,  OV_VISIBLE,	TRUE, "instrumentation",	"instr",
    0, 0, 0, &Instrumentation_Enabled, NULL,
    "Allow adding instrumentation instructions for feedback profiling" },

  { OVK_INT32,  OV_VISIBLE,     TRUE, "fb_phase",       "",
    0, 0, 5,    &Instrumentation_Phase_Num, NULL,
    "Phases in the compiler where instrumentation or feedback  needs to be done" },

  { OVK_INT32,  OV_VISIBLE,     TRUE, "fb_type",       "",
    0, 0, 14,    &Instrumentation_Type_Num, NULL,
    "Types in the compiler where instrumentation needs to be done" },

  { OVK_UINT32,  OV_INTERNAL,	TRUE, "instrument_action",		"",
    0, 0, UINT32_MAX,	&Instrumentation_Actions, NULL,
    "Phases in the compiler where instrumentation needs to be done" },

  { OVK_BOOL,	OV_INTERNAL,	FALSE,	"instr_unique_output",	"",
    0, 0, 0,	&Instrumentation_Unique_Output,	NULL,
    "Always create a unique name for the profile data file" },

  { OVK_LIST,	OV_VISIBLE,	TRUE, 	"feedback",		"feed",
    0, 0, 0,	&Feedback_Option,	NULL,
    "Phases in the compiler where feedback needs to be done" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "early_intrinsics",		"",
    0, 0, 0,	&Inline_Intrinsics_Early, NULL,
    "Lower intrinsics early" },

  { OVK_BOOL,	OV_INTERNAL,	TRUE, "delay_u64_lowering",	"delay_u64",
    0, 0, 0,	&Delay_U64_Lowering, NULL,
    "Delay unsigned 64-bit lowering to after wopt" },

  { OVK_BOOL,   OV_INTERNAL,    TRUE, "extract_bits",   "extr",
    0, 0, 0,    &Enable_extract_bits,   NULL,
    "Enable use of extract opcode" },

  { OVK_BOOL,   OV_INTERNAL,    TRUE, "compose_bits",   "",
    0, 0, 0,    &Enable_compose_bits,   NULL,
    "Enable use of compose opcode" },

  { OVK_BOOL, OV_VISIBLE,     FALSE, "ansi_setjmp",           "ansi_setjmp",
    0, 0, 0,  &LANG_Ansi_Setjmp_On,   &LANG_Ansi_Setjmp_Set,
    "C/C++: enable optimization of functions with calls to setjmp" },

  { OVK_INT32, OV_VISIBLE,     1, "exception_range_opt",           "",
    1, 0, 2,  &Optimize_exception_ranges,   &Optimize_exception_ranges_set,
    "Enable control flow optimization for exception ranges" },

#if defined(__linux__) || defined(BUILD_OS_DARWIN)
  { OVK_BOOL,	OV_INTERNAL,	TRUE, "wfe_dfe",	"wfe_dfe",
    0, 0, 0,	&Enable_WFE_DFE,	NULL,
    "Enable dead function elimination in the frontend" },
#endif /* __linux__ */

  { OVK_BOOL,	OV_INTERNAL,	FALSE, "wssa",	NULL,
    0, 0, 0,	&OPT_Enable_WHIRL_SSA,	NULL,
    "Enable building up SSA on WHIRL" },

  { OVK_BOOL,	OV_INTERNAL,	FALSE, "wssa_build",	NULL,
    0, 0, 0,	&OPT_Enable_BUILD_WHIRL_SSA,	NULL,
    "Enable building WHIRL SSA directly on WHIRL" },

  { OVK_UINT32,  OV_INTERNAL,	TRUE, "aa_force_alias_dim1",		"",
    0, 1, UINT32_MAX,	&AA_force_tag_alias_before_dim1, NULL,
    "Triage option for alias analyzer" },

  { OVK_UINT32,  OV_INTERNAL,	TRUE, "aa_force_alias_dim2",		"",
    0, 0, UINT32_MAX,	&AA_force_tag_alias_before_dim2, NULL,
    "Triage option for alias analyzer" },

  { OVK_BOOL,   OV_INTERNAL,     TRUE, "eh_cfg_opt",        "",
    0, 0, 0,    &OPT_Enable_EH_CFG_OPT, &OPT_Enable_EH_CFG_OPT_Set, 
    "Enable CFO for EH regions"},

  { OVK_UINT32,	OV_VISIBLE,	FALSE, "struct_array_copy",	"",
    1, 0, UINT32_MAX,	&OPT_Struct_Array_Copy, NULL,
    "Set the struct split level" },

  /* Obsolete options: */

  { OVK_OBSOLETE,	OV_INTERNAL,	FALSE, "global_limit",		NULL,
    0, 0, 0,		NULL,	NULL,	"" },

  { OVK_OBSOLETE,	OV_INTERNAL,	FALSE, "feopt",		NULL,
    0, 0, 0,		NULL,	NULL,
    "Enable special optimizations in front ends" },

  { OVK_OBSOLETE,	OV_VISIBLE,	FALSE, "fold_arith_limit",	NULL,
    0, 0, 0,		NULL,	 NULL,
    "Limit size of subexpressions to be folded" },

  { OVK_OBSOLETE,	OV_INTERNAL,	FALSE, "fold_float",		NULL,
    0, 0, 0,		NULL,	NULL,
    "Allow expression folding optimizations of floating point" },

  { OVK_OBSOLETE,	OV_INTERNAL,	FALSE, "fprop_limit",		NULL,
    0, 0, INT32_MAX,	NULL,	NULL,	"" },

  { OVK_COUNT }		/* List terminator -- must be last */
};
