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
 * Module: config_opt.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_opt.h,v $
 *
 * Revision history:
 *  05-May-96 - Extracted from be/opt/opt_config.h.
 *
 * Description:
 *
 * Declare global flag variables for -OPT group options.
 * This file is included in common/com/config.c.
 *
 * Declarations of -OPT flags should be put here, instead of in
 * config.h.  The intent is to allow updates of the -OPT group
 * without forcing recompilation of everything that includes config.h.
 * (However, the transfer of the flags' definitions here from config.h
 * is not yet complete, so most of the old ones still require
 * config.h.)
 *
 * ====================================================================
 * WARNING: WHENEVER A NEW FLAG IS ADDED:
 * ###	- Add the flag variable declaration to config_opt.h (here) .
 * ###	- Add the flag variable definition to config_opt.cxx .
 * ###	- Add the option to the group description in config_opt.cxx .
 * ====================================================================
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_opt_INCLUDED
#define config_opt_INCLUDED

#ifndef flags_INCLUDED
#include "flags.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Incomplete types to prevent unnecessary inclusion: */
struct skiplist;

/*********************************************************************
 ***
 *** Flag variable declarations:
 ***
 *********************************************************************
 */

/***** Optimization Warning Messages *****/
extern BOOL Show_OPT_Warnings;		/* Display OPT warning messages */

/***** Aliasing control *****/
extern OPTION_LIST* Alias_Option;
extern INT32 Alias_Query_Limit;
extern char *Alias_Query_File;
extern INT32 Alias_Nystrom_Solver_Track;
extern BOOL  Alias_Nystrom_Global_Cycle_Detection;
extern BOOL Alias_Pointer_Parms;	/* Reference parms indep? */
extern BOOL Alias_Pointer_Types;	/* Ptrs to distinct basic types indep? */
extern BOOL Alias_Not_In_Union;	/* Ptrs point to non-union types */
extern BOOL Alias_Pointer_Strongly_Typed; /* Ptrs to distinct types indep? */
extern BOOL Alias_Pointer_Named_Data;	/* No pointers to named data? */
extern BOOL Alias_Pointer_Restricted;	/* *p and *q not aliased */
extern BOOL Alias_Pointer_Disjoint;     /* **p and **q not aliased */
extern BOOL Alias_Pointer_Cray;         /* Cray pointer semantics? */
extern BOOL Alias_Common_Scalar;        /* Distinguish scalar from other array
                                           in a common block */
extern BOOL  Alias_F90_Pointer_Unaliased;  /* Are F90 pointers unaliased? */
extern BOOL Alias_Nystrom_Analyzer;     /* Are we using Nystrom alias analysis? */
extern BOOL Alias_Analyzer_in_IPA; /* Are we create Nystrom alias analyzer in IPA? */ 
extern UINT32 Current_IPANode_File_PU_Idx; /* current processing IPA node's file pu index */ 

/** lower zdl stuff **/
extern BOOL OPT_Lower_ZDL;
extern BOOL OPT_Lower_ZDL_Set;

/***** Expression folding options *****/
extern BOOL Enable_Cfold_Float;		/* FP constant folding? */
extern BOOL Enable_Cfold_Reassociate;	/* Re-association allowed? */
extern BOOL Enable_Cfold_Intrinsics;	/* Intrinsic constant folding? */
extern BOOL Cfold_Intrinsics_Set;	/* ... option seen? */
extern BOOL CIS_Allowed;	/* sin(x) and cos(x) => cis(x) ? */
extern BOOL Div_Split_Allowed;	/* Change a/b --> a*1/b ? */
#ifdef KEY
extern UINT32 Div_Exe_Counter;	  /* Change a/b --> a/N if b==N ?             */
extern UINT32 Div_Exe_Ratio;	  /* Change a/b --> a/N if b has high ratio   */
extern UINT32 Div_Exe_Candidates; /* The top entries that will be taken care. */
extern UINT32 Mpy_Exe_Counter;	/* Change a*b to a if b==N or 0.0 if b == 0.0 */
extern UINT32 Mpy_Exe_Ratio;	/* Change a*b to a if b==N or 0.0 if b == 0.0 */
#endif
extern BOOL Fast_Exp_Allowed;	/* Avoid exp() calls? */
extern BOOL Fast_IO_Allowed;	/* Fast printf/scanf/printw */
extern BOOL Fast_Sqrt_Allowed;	/* Change sqrt(x) --> x * rsqrt(x) ? */
extern BOOL Optimize_CVTL_Exp;	/* Optimize expansion of CVTL operators */
extern BOOL Enable_CVT_Opt;	/* Optimize expansion of CVT operators */
extern BOOL Force_IEEE_Comparisons;	/* IEEE NaN comparisons? */
extern BOOL Inline_Intrinsics_Early;    /* Inline intrinsics just after VHO */
extern BOOL Enable_extract_bits;     /* Enable use of the extract/compose whirl ops */
extern BOOL Enable_compose_bits;     /* Enable use of the extract/compose whirl ops */

/***** Miscellaneous optimization options *****/
extern BOOL OPT_Pad_Common;	/* Do internal common block padding? */
extern BOOL OPT_Reorg_Common;	/* Do common block reorganization (split)? */
extern BOOL OPT_Reorg_Common_Set;	/* ... option seen? */
extern BOOL OPT_Unroll_Analysis;	/* Enable unroll limitations? */
extern BOOL OPT_Unroll_Analysis_Set;	/* ... option seen? */
extern BOOL GCM_Speculative_Ptr_Deref;   /* allow load speculation of a memory
                                          reference that differs by a small
                                          offset from some reference location*/
extern BOOL GCM_Speculative_Ptr_Deref_Set;   /* ... option seen? */
extern BOOL Early_MP_Processing; /* Do mp lowerering before lno/preopt */
extern BOOL Implied_Do_Io_Opt;	/* Do implied-do loop opt for I/O */
extern BOOL Cray_Ivdep;		/* Use Cray meaning for Ivdep */
extern BOOL Liberal_Ivdep;	/* Use liberal meaning for ivdep */
extern BOOL Inhibit_EH_opt;     /* Don't remove calless EH regions */
extern BOOL OPT_recompute_addr_flags; /* recompute addr saved */
extern BOOL OPT_IPA_addr_analysis; /* enable the use of IPA addr analysis result */ 
extern BOOL Delay_U64_Lowering;/* Delay unsigned 64-bit lowering to after wopt*/
extern BOOL OPT_shared_memory;	// assume use of shared memory

/***** Instrumentation related options *****/
extern INT32 Instrumentation_Phase_Num;
extern INT32 Instrumentation_Type_Num;
extern BOOL Instrumentation_Enabled;
extern BOOL Instrumentation_Enabled_Before;
extern UINT32 Instrumentation_Actions;
extern BOOL Instrumentation_Unique_Output;
extern INT32 Feedback_Phase_Num;
extern OPTION_LIST* Feedback_Option;
#ifdef KEY
extern INT32 OPT_Cyg_Instrument;
extern BOOL profile_arcs;
extern BOOL Asm_Memory;
extern BOOL Align_Unsafe;
extern INT32 Enable_WN_Simp_Expr_Limit;
extern UINT32 OPT_Lower_To_Memlib;
extern INT32 OPT_Threshold_To_Memlib;
extern INT32 OPT_Enable_Lower_To_Memlib_Limit;
extern BOOL OPT_Enable_Simp_Fold;

extern BOOL OPT_Fast_Math;
extern BOOL OPT_Fast_Stdlib;
extern BOOL OPT_MP_Barrier_Opt;
extern BOOL OPT_Icall_Instr;
extern BOOL OPT_Int_Value_Instr;
extern BOOL OPT_FP_Value_Instr;
extern BOOL OPT_Ffast_Math;
extern BOOL OPT_Scale;
extern BOOL OPT_Funsafe_Math_Optimizations;

extern BOOL OPT_Float_Via_Int;
extern UINT32 OPT_Malloc_Alg;
extern INT32 OPT_Hugepage_Heap_Limit;
extern BOOL OPT_Hugepage_Heap_Set;
extern INT32 OPT_Hugepage_Attr;
extern BOOL OPT_Malloc_Alg_Set;
extern BOOL Early_Goto_Conversion;
extern BOOL Early_Goto_Conversion_Set;
extern INT32 OPT_Madd_Height;

extern BOOL OPT_Enable_WHIRL_SSA;  // enable SSA on WHIRL
extern BOOL OPT_Enable_BUILD_WHIRL_SSA; // enable build WSSA driect from WHIRL

extern UINT32 AA_force_tag_alias_before_dim1;
extern UINT32 AA_force_tag_alias_before_dim2;

extern BOOL OPT_Enable_EH_CFG_OPT;
extern BOOL OPT_Enable_EH_CFG_OPT_Set;

extern UINT32 OPT_Struct_Array_Copy; 
#endif
#ifdef __cplusplus
}
#endif
#endif /* config_opt_INCLUDED */
