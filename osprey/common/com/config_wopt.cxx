/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: config_wopt.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_wopt.cxx,v $
 *
 * Revision history:
 *  08-Sep-94 - Original Version (wodriver.c)
 *  05-May-96 - config_wopt.c extracted from wodriver.c
 *
 * Description:
 *
 * Configure the -WOPT group (included in config.c).
 * Used from ipl, inline, ipa (ld), and be.
 *
 * ====================================================================
 * ====================================================================
 */

// This file is included in config.cxx, so it doesn't need its own set of
// standard includes -- only the following:
#include "config_wopt.h"


/* ====================================================================
 *
 * Global flag variables which reflect the -WOPT group options.
 *
 * ====================================================================
 */

/* List of global variables to turn on and off various optimizations */
UINT32 WOPT_Ip_Alias_Class_Limit = UINT32_MAX;	// no limit
UINT32 WOPT_Alias_Class_Limit = UINT32_MAX;	// no limit
INT32  WOPT_Ldx_Ratio_RegIns = 1;
BOOL  WOPT_Enable_Add_Do_Loop_Info = TRUE;
BOOL  WOPT_Enable_Add_Label_Loop_Info = TRUE;
UINT32 WOPT_Enable_Aggressive_Code_Motion = TRUE;
INT32 WOPT_Enable_Aggressive_CM_Limit = INT32_MAX;
INT32 WOPT_Enable_Aggressive_CM_Branch_Threshold = 10;
INT32 WOPT_Enable_Aggressive_CM_Switch_Threshold = 20;
BOOL  WOPT_Enable_Aggressive_dce = TRUE;
BOOL  WOPT_Enable_Aggressive_dce_for_bbs = TRUE;
BOOL  WOPT_Enable_Aggressive_Doloop_Promotion = FALSE;
BOOL  WOPT_Enable_Aggressive_IVR = TRUE;
BOOL  WOPT_Enable_Aggressive_Lftr = TRUE;
BOOL  WOPT_Enable_Aggressive_Phi_Simp = TRUE;
#if defined(TARG_X8664) || defined(TARG_IA32) || defined(TARG_LOONGSON)
INT32 WOPT_Enable_Autoaggstr_Reduction_Threshold = 11;
#else
INT32 WOPT_Enable_Autoaggstr_Reduction_Threshold = 24;
#endif
BOOL  WOPT_Enable_Alias_ANSI = TRUE;
BOOL  WOPT_Enable_Alias_Classification = TRUE;
BOOL  WOPT_Enable_Aggressive_Alias_Classification = TRUE;
BOOL  WOPT_Enable_Disambiguate_Heap_Obj = TRUE;
BOOL  WOPT_Enable_Alias_Class_Fortran_Rule = TRUE;
BOOL  WOPT_Enable_Alias_Qualifer = TRUE;
BOOL  WOPT_Enable_Alias_Ragnarok_Unnamed = TRUE;
#if defined(TARG_SL)
BOOL  WOPT_Enable_Alias_Intrn = TRUE;
BOOL WOPT_Enable_Local_Clsc = TRUE;  /* make local non-overlap live range */
                                     /* vars to be the same variable */
#endif
BOOL  WOPT_Enable_Avoid_Rehash = FALSE; /* SSAPRE to minimize rehashing */
BOOL  WOPT_Enable_Bitwise_DCE = TRUE;
BOOL  WOPT_Enable_CSE_FP_comparison = TRUE;
BOOL  WOPT_Enable_Call_Flag = TRUE;
BOOL  WOPT_Enable_Calls_Break_BB = FALSE;
BOOL  WOPT_Enable_Calls_Break_BB_Set = FALSE;
BOOL  WOPT_Enable_Canon_Expr = TRUE;
BOOL  WOPT_Enable_Combine_Operations = TRUE;
BOOL  WOPT_Enable_Compare_Simp = TRUE;  /* simplify comparisons */
BOOL  WOPT_Enable_Const_PRE = TRUE;
INT32 WOPT_Enable_Const_PRE_Limit = 0;
#ifdef TARG_NVISA
BOOL  WOPT_Enable_Const_Var_PRE = TRUE;
BOOL  WOPT_Enable_Const_Op_PRE = TRUE;
INT32 WOPT_Const_PRE_Float_Size = 128; /* float Consts >= this Size are const PRE candidates */
#endif
BOOL  WOPT_Enable_Copy_Propagate = TRUE;
BOOL  WOPT_Enable_Copy_Prop_Bad_Ops = FALSE;  /* copy prop ops that strength-reduction can't handle */
static BOOL WOPT_Enable_Copy_Prop_Bad_Ops_Set = FALSE;
BOOL  WOPT_Enable_Copy_Prop_LNO_Ops = FALSE;  /* copy prop ops that LNO can't handle */
/* copy prop certain ops into ARRAY subscripts (enabled for mainopt) */
BOOL  WOPT_Enable_Copy_Prop_Ops_Into_Array = FALSE;
BOOL  WOPT_Enable_Copy_Prop_Ops_Into_Array_Set = FALSE;
BOOL  WOPT_Enable_CG_Alias = TRUE;
BOOL  WOPT_Enable_DIVREM = TRUE; /* allow divrem opcode */
BOOL  WOPT_Enable_CRSIMP = TRUE;
BOOL  WOPT_Enable_DCE = TRUE;
BOOL  WOPT_Enable_DCE_Alias = FALSE;
BOOL  WOPT_Enable_DCE_Branch = TRUE;
INT32 WOPT_Enable_DCE_Branch_Pred_Limit = 5;
BOOL  WOPT_Enable_DCE_Global = TRUE;
BOOL  WOPT_Enable_DCE_Label = TRUE;
BOOL  WOPT_Enable_DU_Full = FALSE;
BOOL  WOPT_Enable_DU_Union = TRUE;
BOOL  WOPT_Enable_Dead_CR_Fix = TRUE;
BOOL  WOPT_Enable_Debug_Inconsistent_Ip_Class = FALSE;
BOOL  WOPT_Enable_Estr_FB_Injury = FALSE;
static BOOL  WOPT_Enable_Estr_FB_Injury_Set = FALSE;
BOOL  WOPT_Enable_Exp_PRE = TRUE;
INT32 WOPT_Enable_Exp_PRE_Limit = -1;
BOOL  WOPT_Enable_Fast_Simp = TRUE;
BOOL  WOPT_Enable_Fold2const = TRUE;
#ifdef KEY
BOOL  WOPT_Enable_Fold_Lda_Iload_Istore = TRUE;
#else
BOOL  WOPT_Enable_Fold_Lda_Iload_Istore = FALSE;
#endif
BOOL  WOPT_Enable_LNO_Copy_Propagate = TRUE;
BOOL  WOPT_Enable_FSA = TRUE;
INT32 WOPT_Enable_Generate_Trip_Count = 1;
BOOL  WOPT_Enable_Generate_DU = TRUE;
BOOL  WOPT_Enable_Goto = TRUE;
BOOL  WOPT_Enable_Hoisting = TRUE;
BOOL  WOPT_Enable_Ivar_Hoisting = TRUE;
BOOL  WOPT_Enable_I8_Primary_IV = FALSE;
BOOL  WOPT_Enable_Improved_Addr_Taken = TRUE;
BOOL  WOPT_Enable_Input_Prop = TRUE;
BOOL  WOPT_Enable_Itself_Prop = TRUE;
BOOL  WOPT_Enable_IPAA = FALSE;
BOOL  WOPT_Enable_Ivar_Common = TRUE;
BOOL  WOPT_Enable_Ivar_PRE = TRUE;
BOOL  WOPT_Enable_Ivincr_Cand = FALSE;
BOOL  WOPT_Enable_IVE = TRUE;
BOOL  WOPT_Enable_IVE_Old = FALSE;
BOOL  WOPT_Enable_IVR = TRUE;
INT32 WOPT_Enable_IVR_Expand_Limit = 1000;  /* expand 1000 statements */
BOOL  WOPT_Enable_IVR_Outermost_Loop_Parallel_Region = FALSE;
#ifdef KEY
INT32 WOPT_Enable_Ivr_Limit = -1;
INT32 WOPT_Enable_Ivr_Cand_Limit = -1;
#endif
BOOL  WOPT_Enable_Ldx = FALSE;
BOOL  WOPT_Enable_Lego_Opt = FALSE;
BOOL  WOPT_Enable_LFTR_Ivar = FALSE;
#ifndef TARG_IA64
BOOL  WOPT_Enable_LFTR2 = TRUE;
#else
BOOL  WOPT_Enable_LFTR2 = FALSE;
#endif
INT32 WOPT_Enable_LFTR2_Limit = -1;
BOOL  WOPT_Enable_Load_PRE = TRUE;
INT32 WOPT_Enable_Load_PRE_Limit = -1;
BOOL  WOPT_Enable_Loopinvarexp_Str_Reduction = TRUE;
BOOL  WOPT_Enable_Lower_Short_Circuit = FALSE;
BOOL  WOPT_Enable_Lower_Short_Circuit_Set = FALSE;
#if defined(TARG_SL)
BOOL  WOPT_Enable_MINMAX = FALSE; /* disallow minmax opcode */
#else
BOOL  WOPT_Enable_MINMAX = TRUE; /* allow minmax opcode */
#endif
BOOL  WOPT_Enable_Min_Type = TRUE; 
BOOL  WOPT_Enable_Move_Intrinsicop = TRUE;
BOOL  WOPT_Enable_MP_varref = TRUE;
const BOOL  WOPT_Enable_MP_Const_Prop = TRUE;
BOOL  WOPT_Enable_New_SR = TRUE;
BOOL  WOPT_Enable_SIB = FALSE;
INT32 WOPT_Enable_New_SR_Limit = 0;
BOOL  WOPT_Enable_Output_Copy = TRUE;
INT32 WOPT_Enable_Ocopy_Lookupstmt = 2;
BOOL  WOPT_Enable_Parm = TRUE;
char *WOPT_Enable_Process = NULL;
BOOL  WOPT_Enable_Phi_Simp = TRUE;
BOOL  WOPT_Enable_Prop_Ivar = TRUE;
BOOL  WOPT_Enable_Prop_CSE = FALSE;     /* propagate CSE expressions */
#ifdef KEY
INT32 WOPT_Enable_Prop_Limit = 12;	/* based on bug 3095 */
INT32 WOPT_Enable_Doend_Prop_Limit = 26; /* based on bug 13003 */
#else
INT32 WOPT_Enable_Prop_Limit = 14;	/* this is a guess, PV 468862 */
#endif
INT32 WOPT_Enable_Prop_Weight_Limit = 50000;
#ifdef KEY
BOOL  WOPT_Enable_Prop_Dope = FALSE;	/* propagate dope vector fields? */
#endif
BOOL  WOPT_Enable_Prune = TRUE;		/* temporary, PV 370066 */
BOOL  WOPT_Enable_Replace_Second_IV = TRUE;
BOOL  WOPT_Enable_Replace_While_Loop_Second_IV = TRUE;
BOOL  WOPT_Enable_Restricted_Map = TRUE;
INT32 WOPT_Enable_Rsv_Bits = 16;	/* reserve bit count in itable */
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
BOOL  WOPT_Enable_RVI = FALSE;		/* perform both rvi1 and rvi2 */
#else
BOOL  WOPT_Enable_RVI = TRUE;		/* perform both rvi1 and rvi2 */
#endif
BOOL  WOPT_Enable_RVI1 = TRUE;		/* phase 1 of rvi */
BOOL  WOPT_Enable_RVI2 = TRUE;		/* phase 2 of rvi */
BOOL  WOPT_Enable_Rviistore = TRUE;	/* agg. chi-handling on istore*/
char *WOPT_Enable_Rviskip = NULL;	/* skip this var during RVI */
BOOL  WOPT_Enable_Rvisplit = FALSE;	/* split bbs at every stmt   */
BOOL  WOPT_Enable_Rvivsym = TRUE;	/* ignore vsym in chi lists */
BOOL  WOPT_Enable_Second_Alias_Class = TRUE;
BOOL  WOPT_Enable_Second_Order = TRUE;
char *WOPT_Enable_Skip = NULL;		/* Name of function to skip */
OPTION_LIST *WOPT_Skip = NULL;		/* Skip option list */
SKIPLIST *WOPT_Skip_List = NULL;	/* Processed skiplist */
OPTION_LIST *WOPT_Unroll_Skip = NULL;	/* Skip unroll option list */
SKIPLIST *WOPT_Unroll_Skip_List = NULL;	/* Processed unroll skiplist */
BOOL  WOPT_Enable_SLT = TRUE;
BOOL  WOPT_Enable_Small_Br_Target = FALSE; /* propagation into branch BBs */
INT32  WOPT_Enable_Simple_If_Conv = 1;   /* simple if-conversion at CFG build time: 0 - off, 1 - conservative, 2 - aggressive */
INT32 WOPT_Enable_If_Conv_Limit = 6;    /* max number of leaf nodes allowed in a
					   simple expr in simple if conv */
BOOL  WOPT_Enable_If_Conv_For_Istore = TRUE;   /* if-conversion is applied if lhs is istore */
#if defined(TARG_SL)
BOOL  WOPT_Enable_If_Conv_For_Iload = FALSE;   /* if-conversion is applied if rhs is iload */
#endif
BOOL  WOPT_Enable_Speculation_Defeats_LFTR = TRUE;
BOOL  WOPT_Enable_Str_Red_Use_Context = TRUE; /* use loop content in SR decision */
BOOL  WOPT_Enable_SSA_Minimization = TRUE; /* SSA minimization in SSAPRE */
BOOL  WOPT_Enable_SSA_PRE = TRUE;
BOOL  WOPT_Enable_Store_PRE = TRUE;
INT32 WOPT_Enable_Store_PRE_Limit = -1;
INT32 WOPT_Enable_Local_Rvi_Limit = -1;
BOOL  WOPT_Enable_Update_Vsym = TRUE;
BOOL  WOPT_Enable_Unique_Pt_Vsym = TRUE;
INT32 WOPT_Enable_Value_Numbering = 1; /* 0=OFF, 1=after_pre, 2=befr_n_aftr */
INT32 WOPT_Enable_Vn_Ivc = 1;    /* 0=OFF, see be/opt/opt_vn_ivc.h */
UINT32 WOPT_Enable_Vnfre_After = 0;    /* Disable vnfre after given valnum */
UINT32 WOPT_Enable_Vnfre_Before = UINT32_MAX; /* Disable before given valnum */
BOOL  WOPT_Enable_Verbose = FALSE;
INT32 WOPT_Enable_Verify = 1;
BOOL  WOPT_Enable_Vsym_Unique = FALSE;
BOOL  WOPT_Enable_While_Loop = TRUE;
BOOL  WOPT_Enable_Worklist_Pruning = TRUE;
BOOL  WOPT_Enable_Zero_Version = TRUE;
BOOL  WOPT_Enable_Call_Zero_Version = TRUE;
BOOL  WOPT_Enable_Dse_Aggressive = TRUE;
BOOL  WOPT_Enable_Prop_Aggressive = TRUE;
BOOL  WOPT_Enable_Iload_Prop = TRUE;
BOOL  WOPT_Enable_VN_Full = TRUE;	/* full value number (for ivars) */
BOOL  WOPT_Enable_Simp_Iload = TRUE;   /* simplifier folding iload */
BOOL  WOPT_Enable_Canon_Uplevel=FALSE; /* canonicalize the up level ref */
#if defined(TARG_X8664) || (TARG_SL) // bug 12909
BOOL  WOPT_Enable_Tail_Recur = TRUE;	/* tail recursion */
#else
BOOL  WOPT_Enable_Tail_Recur = FALSE;	/* tail recursion */
#endif
BOOL  WOPT_Enable_Tail_Recur_Set = FALSE;
BOOL  WOPT_Enable_Edge_Placement = TRUE;/* insert BB on critical edge */
BOOL  WOPT_Enable_Backedge_Placement = FALSE; /* insert BB on critical backedge */
BOOL  WOPT_Enable_Source_Order = FALSE;     /* Dump CFG in source order */
BOOL  WOPT_Enable_Cvt_Folding = TRUE; /* enable cvt folding in emitter */
BOOL  WOPT_Enable_Local_Rvi = TRUE;  /* enable fast rvi of locals */
BOOL  WOPT_Enable_Strong_Barrier = FALSE;
BOOL  WOPT_Enable_Aggr_Invariant = TRUE;
BOOL  WOPT_Enable_Shrink = TRUE;
INT32 WOPT_Enable_Extra_Rename_Pass = 5;
BOOL  WOPT_Enable_Extra_Rename_Pass_Set = FALSE;
UINT32 WOPT_Enable_Extra_Preopt_Pass = 0;
BOOL  WOPT_Enable_Bool_Simp = TRUE;
BOOL  WOPT_Enable_Feedback_LPRE = FALSE;
BOOL  WOPT_Enable_Feedback_EPRE = FALSE;
BOOL  WOPT_Enable_CFG_Display = FALSE;     
BOOL  WOPT_Enable_CFG_Merge_Multi_Zone = TRUE;
BOOL  WOPT_Enable_CFG_Merge_Multi_Zone_Set = FALSE;
BOOL  WOPT_Enable_CFG_Opt = TRUE;         /* control flow optimization main switch */
BOOL  WOPT_Enable_CFG_Opt1 = TRUE;
BOOL  WOPT_Enable_CFG_Opt2 = TRUE;
INT32 WOPT_Enable_CFG_Opt2_Limit = 80;    /* Loop butterfly should produce a min of 80% coverage */
BOOL  WOPT_Enable_CFG_Opt3 = FALSE;
BOOL  WOPT_Enable_CFG_Opt4 = FALSE;
INT32  WOPT_Enable_CFG_Opt_Limit = 10;
char *WOPT_Set_Unique_Pt = NULL;
BOOL  WOPT_Enable_Bits_Load_Store = TRUE; // accept LDBITS, STBITS, etc.
BOOL  WOPT_Enable_Epre_Before_Ivr = FALSE; // For running epre early
BOOL  WOPT_Enable_Lpre_Before_Ivr = FALSE; // For running lpre early
BOOL  WOPT_Enable_Spre_Before_Ivr = FALSE; // For running spre early
BOOL  WOPT_Enable_Bdce_Before_Ivr = FALSE; // For running bdce early
BOOL  WOPT_Enable_New_Phase_Ordering = TRUE; // Enables some phases before ivr
BOOL  WOPT_Enable_Pt_Keep_Track_Ptr = TRUE;  // POINTS_TO keeps track of pointer
                                             // of iload/istore
  // POINTS_TO keeps track of complex address of iload/istore. 
BOOL  WOPT_Enable_Aggr_Pt_Keep_Track_Ptr = TRUE; 
BOOL  WOPT_Enable_Noreturn_Attr_Opt = TRUE;
BOOL  WOPT_Enable_Nothrow_Opt = TRUE;
BOOL  WOPT_Enable_Multiver_and_Unroll_Opt = TRUE;
BOOL  WOPT_Enable_Pt_Summary = FALSE;  // points-to summary/annotation 
INT32 WOPT_Enable_Pro_Loop_Limit = -1;  // Limit number of if-condition transformations per function.
INT32 WOPT_Tail_Dup_Max_Clone = -1; // Limit code size bloats (in statement count)
                                                  // due to tail-duplication.
INT32 WOPT_Enable_Pro_Loop_Fusion_Func_Limit = -1; // Enable proactive loop fusion transformation for
                                                  // functions within the limit.
INT32 WOPT_Enable_Pro_Loop_Interchange_Func_Limit = -1; // Enable proactive loop interchange for 
                                                        // functions within the limit.
INT32 WOPT_Enable_Pro_Loop_Ext_Func_Limit = -1; // Enable proactive loop extended transformation for
                                                // functions within the limit.
BOOL  WOPT_Enable_Pro_Loop_Fusion_Trans = TRUE;  // Enables proactive loop fusion transformation
BOOL  WOPT_Enable_Pro_Loop_Interchange_Trans = TRUE; // Enables proactive loop interchange transformation
BOOL WOPT_Enable_Pro_Loop_Ext_Trans = TRUE; // Enables proactive loop extended transformation.
BOOL  WOPT_Simplify_Bit_Op = TRUE; // Enable specialized bit operation optimizations.
BOOL  WOPT_Enable_Reassociation_CSE = TRUE;  // Enables Reassociation based CSE

BOOL  WOPT_Enable_Mem_Clear_Remove = TRUE;  // Enables removal of redundant mem clear after a calloc

BOOL  WOPT_Enable_ZDL = FALSE; // Enable tranformation for ZDL
BOOL  WOPT_Enable_ZDL_Set = FALSE; // Set for tranformation for ZDL
BOOL  WOPT_Enable_ZDL_Early_Exit = FALSE; // Enable early exit loops transformed to ZDL
BOOL  WOPT_ZDL_Innermost_Only = FALSE; // Only Innermost loops can be transformed to ZDL
OPTION_LIST *WOPT_ZDL_Skip = NULL;	/* Skip ZDL option list */
SKIPLIST *WOPT_ZDL_Skip_List = NULL;	/* Processed ZDL skiplist */

#ifdef KEY
BOOL  WOPT_Enable_Preserve_Mem_Opnds = FALSE; // if TRUE, suppress EPRE on 
				// iloads that are operands of FP operations
BOOL  WOPT_Enable_Retype_Expr = FALSE;   // whether to call WN_retype_expr to 
					// change 64-bit operations to 32-bit 
INT32 WOPT_Enable_Folded_Scalar_Limit = 1000; // limit to number of scalars
					// formed by Fold_lda_iload_istore()
INT32 WOPT_Enable_Bdceprop_Limit = -1; 	// to limit the BBs in which BDCE's
					// copy propagation is performed
BOOL WOPT_Enable_Warn_Uninit = FALSE;   // enable warning for detected uninitialized locals
INT32 WOPT_Enable_WN_Unroll = 1;	// 0: disable; 
					// 1: unroll only loop bodies with IFs
					// 2: unroll all loop bodies
BOOL WOPT_Enable_IP_Mod_Ref = FALSE; // use mod/ref information from IPA?
BOOL WOPT_Enable_Invariant_Loop_Bounds = FALSE; // enable assumption that all
				   // induction loops' bounds are loop-invariant
BOOL WOPT_Enable_Subword_Opt = TRUE; // whether to replace 1- or 2-byte-sized
			              // load/store with EXTRACT/COMPOSE
BOOL WOPT_Enable_New_Vsym_Allocation = FALSE;
#endif
BOOL  WOPT_Enable_WOVP = TRUE; // For running write-once variable promotion
BOOL WOPT_Enable_Loop_Multiver = FALSE; // For loop multiversioning
BOOL WOPT_Enable_Loop_Multiver_Set = FALSE;
BOOL WOPT_Enable_Loop_Multiver_Aggressive = FALSE;
BOOL WOPT_Enable_Useless_Store_Elimination = TRUE;
#if defined(TARG_SL)
BOOL WOPT_Enable_STR_Short = FALSE; // multimedia apps are 16bit, the iv does not get > 16bits
#else
BOOL WOPT_Enable_STR_Short = FALSE; 
#endif
#if defined(TARG_NVISA)
BOOL WOPT_Enable_Estr_Outer_Loop = TRUE;  // strength reduce outer loops
BOOL WOPT_Enable_Estr_Const_Opnds = TRUE; // strength reduce ops with const kids
BOOL WOPT_Enable_Estr_Used_Once = TRUE;   // strength reduce ops used only once
BOOL WOPT_Enable_Estr_Early_Exit = FALSE; // strength reduce early exit loops
BOOL WOPT_Enable_Aggressive_Iload_CSE = TRUE; // ignore potential iload vsym aliasing
#endif

BOOL WOPT_Bottom_Test_Loop_Check=FALSE;
INT32 WOPT_Bottom_Test_Loop_Cond_Limit=3;
INT32 WOPT_Bottom_Test_Loop_Body_Limit=5;
/* ====================================================================
 *
 * Descriptor for the -WOPT option group.
 *
 * ====================================================================
 */

/* scalar optimizer options: 
   see common/util/flags.h, typedef struct option_desc for documentation */
static OPTION_DESC Options_WOPT[] = {
  { OVK_UINT32, OV_VISIBLE,    TRUE, "ac_limit",               "",
    UINT32_MAX, 0, UINT32_MAX, &WOPT_Alias_Class_Limit, NULL },
  { OVK_UINT32, OV_VISIBLE,    TRUE, "ip_ac_limit",            "",
    UINT32_MAX, 0, UINT32_MAX, &WOPT_Ip_Alias_Class_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "add_do_loop_info",	"add_do",
    0, 0, 0,	&WOPT_Enable_Add_Do_Loop_Info, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "add_label_loop_info",	"add_label",
    0, 0, 0,	&WOPT_Enable_Add_Label_Loop_Info, NULL },
  { OVK_UINT32,	OV_VISIBLE,	TRUE, "aggcm",		"aggcm",
    1, 0, 2,	&WOPT_Enable_Aggressive_Code_Motion, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "aggcm_limit",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Aggressive_CM_Limit, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "aggcm_switch_threshold",	"aggcm_switch_thres",
    20, 0, INT32_MAX,	&WOPT_Enable_Aggressive_CM_Switch_Threshold, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "aggcm_branch_threshold",	"aggcm_branch_thres",
    10, 0, INT32_MAX,	&WOPT_Enable_Aggressive_CM_Branch_Threshold, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "agglftr",		"agglftr",
    0, 0, 0,	&WOPT_Enable_Aggressive_Lftr, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "aggphi_simp",		"aggphi",
    0, 0, 0,	&WOPT_Enable_Aggressive_Phi_Simp, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "aggdo",		"aggdo",
    0, 0, 0,	&WOPT_Enable_Aggressive_Doloop_Promotion, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "agg_ivr",	"agg_ivr",
    0, 0, 0,	&WOPT_Enable_Aggressive_IVR, NULL },
  { OVK_UINT32, OV_VISIBLE,    TRUE, "aggstr_reduction", "aggstr",
    UINT32_MAX, 0, UINT32_MAX, &WOPT_Enable_Autoaggstr_Reduction_Threshold, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "alias_classification", "alias_class",
    0, 0, 0,    &WOPT_Enable_Alias_Classification, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "agg_alias_classification", "agg_alias_class",
    0, 0, 0,    &WOPT_Enable_Aggressive_Alias_Classification, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "disa_heap_obj", "disa_heap",
    0, 0, 0,    &WOPT_Enable_Disambiguate_Heap_Obj, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "ac_fortran", "",
    0, 0, 0,    &WOPT_Enable_Alias_Class_Fortran_Rule, NULL },
#if defined(TARG_SL)
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "alias_intrn", "alias_intrn",
    0, 0, 0,    &WOPT_Enable_Alias_Intrn, NULL },
#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "avoid_rehash",		"",
    0, 0, 0,	&WOPT_Enable_Avoid_Rehash, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "bdce",		"bdce",
    0, 0, 0,	&WOPT_Enable_Bitwise_DCE, NULL },
  { OVK_BOOL, OV_VISIBLE,    TRUE, "cse_fcmp",    "",
    0, 0, 0,  &WOPT_Enable_CSE_FP_comparison, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "canon_uplevel",	"canon",
    0, 0, 0,	&WOPT_Enable_Canon_Uplevel, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "call_flag",		"call_flag",
    0, 0, 0,	&WOPT_Enable_Call_Flag, NULL },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "calls_break_bb",		"",
    0, 0, 0,	&WOPT_Enable_Calls_Break_BB, &WOPT_Enable_Calls_Break_BB_Set },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "canon_expr",		"canon_expr",
    0, 0, 0,	&WOPT_Enable_Canon_Expr, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "cg_alias",		"cg_alias",
    0, 0, 0,	&WOPT_Enable_CG_Alias, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "class2", "class2",
    0, 0, 0,    &WOPT_Enable_Second_Alias_Class, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "combine_operations",	"combine",
    0, 0, 0,	&WOPT_Enable_Combine_Operations, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "compare_simp",		"compare",
    0, 0, 0,	&WOPT_Enable_Compare_Simp, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "const_pre",		"const_pre",
    0, 0, 0,	&WOPT_Enable_Const_PRE, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "const_pre_limit",	"const_pre_limit",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Const_PRE_Limit, NULL },
#if defined(TARG_NVISA)
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "const_var_pre",          "",
    0, 0, 0,    &WOPT_Enable_Const_Var_PRE, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "const_op_pre",           "",
    0, 0, 0,    &WOPT_Enable_Const_Op_PRE, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "const_pre_float_size",	"const_pre_float_size",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Const_PRE_Float_Size, NULL },
#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "divrem",		"divrem",
    0, 0, 0,	&WOPT_Enable_DIVREM, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "copy_propagate",	"copy",
    0, 0, 0,	&WOPT_Enable_Copy_Propagate, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "copy_ops",		"copy_ops",
    0, 0, 0,	&WOPT_Enable_Copy_Prop_Bad_Ops, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "copy_lno_ops",		"copy_lno",
    0, 0, 0,	&WOPT_Enable_Copy_Prop_LNO_Ops, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "copy_into_array",	"",
    0, 0, 0,	&WOPT_Enable_Copy_Prop_Ops_Into_Array,
                &WOPT_Enable_Copy_Prop_Ops_Into_Array_Set },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "lno_copy_propagation",	"lno_copy",
    0, 0, 0,	&WOPT_Enable_LNO_Copy_Propagate, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "cr_simp",		"cr",
    0, 0, 0,	&WOPT_Enable_CRSIMP, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fastsimp",		"",
    0, 0, 0,	&WOPT_Enable_Fast_Simp, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fold2const",		"fold",
    0, 0, 0,	&WOPT_Enable_Fold2const, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fold_lda_iload_istore",	"fold_lda_iload",
    0, 0, 0,	&WOPT_Enable_Fold_Lda_Iload_Istore, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dead_code_elim",	"dead",
    0, 0, 0,	&WOPT_Enable_DCE, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dce_aggressive",	"dce",
    0, 0, 0,	&WOPT_Enable_Aggressive_dce, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dcebbs_aggressive",	"dcebbs",
    0, 0, 0,	&WOPT_Enable_Aggressive_dce_for_bbs, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dce_alias",		"",
    0, 0, 0,	&WOPT_Enable_DCE_Alias, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dce_branch",		"",
    0, 0, 0,	&WOPT_Enable_DCE_Branch, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "dce_branch_pred_limit",	"",
    5, 0, INT32_MAX,	&WOPT_Enable_DCE_Branch_Pred_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dce_global",		"",
    0, 0, 0,	&WOPT_Enable_DCE_Global, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dce_label",		"",
    0, 0, 0,	&WOPT_Enable_DCE_Label, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "du_full",		"",
    0, 0, 0,	&WOPT_Enable_DU_Full, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "du_union",		"",
    0, 0, 0,	&WOPT_Enable_DU_Union, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dead_cr",		"",
    0, 0, 0,	&WOPT_Enable_Dead_CR_Fix, NULL },
  { OVK_BOOL,   OV_VISIBLE,    TRUE, "debug_class",            "",
    0, 0, 0,    &WOPT_Enable_Debug_Inconsistent_Ip_Class, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "epre",			"epre",
    0, 0, 0,	&WOPT_Enable_Exp_PRE, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "epre_limit",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Exp_PRE_Limit, NULL },
  { OVK_BOOL,   OV_VISIBLE,    TRUE, "estr_fb_injury",       "",
    0, 0, 0,    &WOPT_Enable_Estr_FB_Injury, &WOPT_Enable_Estr_FB_Injury_Set },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "goto_conversion",	"goto",
    0, 0, 0,	&WOPT_Enable_Goto, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "hoisting",		"hoist",
    0, 0, 0,	&WOPT_Enable_Hoisting, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "hoist_ivar",		"hoist_ivar",
    0, 0, 0,	&WOPT_Enable_Ivar_Hoisting, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "i8_iv",		"i8_iv",
    0, 0, 0,	&WOPT_Enable_I8_Primary_IV, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "addr",			"addr",
    0, 0, 0,	&WOPT_Enable_Improved_Addr_Taken, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "icopy_propagate",	"icopy",
    0, 0, 0,	&WOPT_Enable_Input_Prop, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "itself_prop",		"itself",
    0, 0, 0,	&WOPT_Enable_Itself_Prop, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ipaa",			"ipaa",
    0, 0, 0,	&WOPT_Enable_IPAA, NULL },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "ipaa_file",		"ipaa_f",
    0, 0, 0,	&Ipa_File_Name, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ivar_common", 		"ivar",
    0, 0, 0,	&WOPT_Enable_Ivar_Common, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ivar_pre",		"ivar_pre",
    0, 0, 0,	&WOPT_Enable_Ivar_PRE, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ivincr_cand",		"ivincr",
    0, 0, 0,	&WOPT_Enable_Ivincr_Cand, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "iv_elimination",	"iv_elim",
    0, 0, 0,	&WOPT_Enable_IVE, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ive_old",		"ive_old",
    0, 0, 0,	&WOPT_Enable_IVE_Old, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "iv_recognition",	"iv_recog",
    0, 0, 0,	&WOPT_Enable_IVR, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "ivr_expand_limit",	"",
    1000, 0, INT32_MAX,	&WOPT_Enable_IVR_Expand_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "iv_outer_parallel",	"",
    0, 0, 0,	&WOPT_Enable_IVR_Outermost_Loop_Parallel_Region, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "zdl",	"zdl",
    0, 0, 0,	&WOPT_Enable_ZDL, &WOPT_Enable_ZDL_Set },
  { OVK_LIST,   OV_VISIBLE,     TRUE, "zdl_skip_after", "zdl_skip_a",
    0, 0, 0,    &WOPT_ZDL_Skip,      NULL },
  { OVK_LIST,   OV_VISIBLE,     TRUE, "zdl_skip_before", "zdl_skip_b",
    0, 0, 0,    &WOPT_ZDL_Skip,      NULL },
  { OVK_LIST,   OV_VISIBLE,     TRUE, "zdl_skip_equal", "zdl_skip_e",
    0, 0, 0,    &WOPT_ZDL_Skip,       NULL },
#ifdef KEY
  { OVK_INT32,  OV_VISIBLE,    TRUE, "ivr_limit",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Enable_Ivr_Limit, NULL },
  { OVK_INT32,  OV_VISIBLE,    TRUE, "ivr_cand_limit",         "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Enable_Ivr_Cand_Limit, NULL },
#endif
  { OVK_INT32,	OV_VISIBLE,	TRUE, "ldx_ratio_regins",	"ldx_ratio",
    0, 0, 4096,	&WOPT_Ldx_Ratio_RegIns, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ldx",			"ldx",
    0, 0, 0,	&WOPT_Enable_Ldx, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "lego_opt",		"lego",
    0, 0, 0,	&WOPT_Enable_Lego_Opt, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "lftr_ivar",		"lftr_ivar",
    0, 0, 0,	&WOPT_Enable_LFTR_Ivar, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "lftr2",		"lftr2",
    0, 0, 0,	&WOPT_Enable_LFTR2, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "lftr2_limit",		"lftr2_limit",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_LFTR2_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "lpre",			"lpre",
    0, 0, 0,	&WOPT_Enable_Load_PRE, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "lpre_limit",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Load_PRE_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "loop_invarexp",	"loop_invar",
    0, 0, 0,	&WOPT_Enable_Loopinvarexp_Str_Reduction, NULL },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "lower_short_circuit",		"",
    0, 0, 0,	&WOPT_Enable_Lower_Short_Circuit, &WOPT_Enable_Lower_Short_Circuit_Set },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "minmax",		"minmax",
    0, 0, 0,	&WOPT_Enable_MINMAX, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "min_type",		"min_type",
    0, 0, 0,	&WOPT_Enable_Min_Type, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "intrinsic",		"intrinsic",
    0, 0, 0,	&WOPT_Enable_Move_Intrinsicop, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "mp_varref",		"mp_varref",
    0, 0, 0,	&WOPT_Enable_MP_varref, NULL },
#ifdef CHANGE_MIND_AGAIN
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "mp_const_prop",	"mp_const_prop",
    0, 0, 0,	&WOPT_Enable_MP_Const_Prop, NULL },
#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "new_sr",		"",
    0, 0, 0,	&WOPT_Enable_New_SR, NULL },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "sib",		"sib",
    0, 0, 0,	&WOPT_Enable_SIB, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "new_sr_limit",		"new_sr_limit",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_New_SR_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ocopy",		"ocopy",
    0, 0, 0,	&WOPT_Enable_Output_Copy, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "ocopy_lookup_stmtcnt",	"ocopy_lookup",
    0, 0, 4096,	&WOPT_Enable_Ocopy_Lookupstmt, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "parm",			"parm",
    0, 0, 0,	&WOPT_Enable_Parm, NULL },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "process",		"process", 
    0, 0, 0,	&WOPT_Enable_Process, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "phi_simp",		"phi",
    0, 0, 0,	&WOPT_Enable_Phi_Simp, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "prop_ivar",		"",
    0, 0, 0,	&WOPT_Enable_Prop_Ivar, NULL },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "prop_cse",		"",
    0, 0, 0,	&WOPT_Enable_Prop_CSE, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "prop_limit",		"",
    14, 0, INT32_MAX,	&WOPT_Enable_Prop_Limit, NULL }, /* PV 468862 */
  { OVK_INT32, OV_VISIBLE, TRUE, "prop_weight_limit", "",
    14, 0, INT32_MAX, &WOPT_Enable_Prop_Weight_Limit, NULL }, 
#ifdef KEY
  { OVK_INT32,	OV_VISIBLE,	TRUE, "doend_prop_limit",		"",
    14, 0, INT32_MAX,	&WOPT_Enable_Doend_Prop_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "prop_dope",		"",
    0, 0, 0,	&WOPT_Enable_Prop_Dope, NULL },
#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "prune",		"",
    0, 0, 0,	&WOPT_Enable_Prune, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "replace_second_iv",	"",
    0, 0, 0,	&WOPT_Enable_Replace_Second_IV, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "replace_while_loop_second_iv",	"",
    0, 0, 0,	&WOPT_Enable_Replace_While_Loop_Second_IV, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "restricted_map",	"",
    0, 0, 0,	&WOPT_Enable_Restricted_Map, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "rsv_bits",		"rsv",
    0, 0, 4096,	&WOPT_Enable_Rsv_Bits, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "rvi_enable",		"rvi",
    0, 0, 0,	&WOPT_Enable_RVI, NULL },
#if defined(TARG_SL)
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "local_clsc",	"",
    0, 0, 0,	&WOPT_Enable_Local_Clsc, NULL },
#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "rvi1",			"",
    0, 0, 0,	&WOPT_Enable_RVI1, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "rvi2",			"",
    0, 0, 0,	&WOPT_Enable_RVI2, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "rviistore",		"",
    0, 0, 0,	&WOPT_Enable_Rviistore, NULL },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "rviskip",		"", 
    0, 0, 0,	&WOPT_Enable_Rviskip, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "rvisplit",		"",
    0, 0, 0,	&WOPT_Enable_Rvisplit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "rvivsym",		"",
    0, 0, 0,	&WOPT_Enable_Rvivsym, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "second_order",		"second",
    0, 0, 0,	&WOPT_Enable_Second_Order, NULL },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "skip",			"skip", 
    0, 0, 0,	&WOPT_Enable_Skip, NULL },
  { OVK_LIST,	OV_VISIBLE,	TRUE, "skip_after",		"skip_a", 
    0, 0, 0,	&WOPT_Skip,	NULL },
  { OVK_LIST,	OV_VISIBLE,	TRUE, "skip_before",		"skip_b", 
    0, 0, 0,	&WOPT_Skip,	NULL },
  { OVK_LIST,	OV_VISIBLE,	TRUE, "skip_equal",		"skip_e", 
    0, 0, 0,	&WOPT_Skip,	 NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "slt",			"slt",
    0, 0, 0,	&WOPT_Enable_SLT, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "small_br_target",	"small_br",
    0, 0, 0,	&WOPT_Enable_Small_Br_Target, NULL },
  { OVK_BOOL,   OV_VISIBLE,    TRUE, "spec_nix_lftr",  "",
    0, 0, 0,    &WOPT_Enable_Speculation_Defeats_LFTR, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "sr_use_context", "sr_use_context",
    0, 0, 0,    &WOPT_Enable_Str_Red_Use_Context, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "ssa_minimization",	"ssa_min",
    0, 0, 0,    &WOPT_Enable_SSA_Minimization, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ssapre",		"ssapre",
    0, 0, 0,	&WOPT_Enable_SSA_PRE, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "spre",			"spre",
    0, 0, 0,	&WOPT_Enable_Store_PRE, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "spre_limit",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Store_PRE_Limit, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "trip_count",		"trip",
    2, 0, 2,	&WOPT_Enable_Generate_Trip_Count, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "update_vsym",		"update_vsym",
    0, 0, 0,	&WOPT_Enable_Update_Vsym, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "unique_pt_vsym",	"",
    0, 0, 0,	&WOPT_Enable_Unique_Pt_Vsym, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "value_numbering",	"val",
    1, 0, 4, &WOPT_Enable_Value_Numbering, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "verbose",		"v",
    0, 0, 0,	&WOPT_Enable_Verbose, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "verify",		"verify",
    1, 0, 4,	&WOPT_Enable_Verify, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "while_loop",		"while",
    0, 0, 0,	&WOPT_Enable_While_Loop, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "worklist_prune",	"worklist_prune",
    0, 0, 0,    &WOPT_Enable_Worklist_Pruning, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "zero_version",		"zero",
    0, 0, 0,	&WOPT_Enable_Zero_Version, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "call_zero_version",	"call_zero",
    0, 0, 0,	&WOPT_Enable_Call_Zero_Version, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "vsym_unique",		"vsym",
    0, 0, 0,	&WOPT_Enable_Vsym_Unique, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "dse_aggressive",	"dse",
    0, 0, 0,	&WOPT_Enable_Dse_Aggressive, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "prop_aggressive",	"prop",
    0, 0, 0,	&WOPT_Enable_Prop_Aggressive, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "iload_prop",		"iload",
    0, 0, 0,	&WOPT_Enable_Iload_Prop, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "vn_full",		"vn",
    0, 0, 0,	&WOPT_Enable_VN_Full, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "simp_iload",		"",
    0, 0, 0,	&WOPT_Enable_Simp_Iload, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "if_conv",		"",
    2, 0, 2,	&WOPT_Enable_Simple_If_Conv, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "ifconv_limit",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_If_Conv_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ifconv_for_istore",		"",
    0, 0, 0,	&WOPT_Enable_If_Conv_For_Istore, NULL },
#if defined(TARG_SL)
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "ifconv_for_iload",		"",
    0, 0, 0,	&WOPT_Enable_If_Conv_For_Iload, NULL },
#endif
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "tail_recursion",	"tail",
    0, 0, 0,	&WOPT_Enable_Tail_Recur, &WOPT_Enable_Tail_Recur_Set },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "edge_placement",	"edge",
    0, 0, 0,	&WOPT_Enable_Edge_Placement, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "backedge_placement",	"backedge_p",
    0, 0, 0,	&WOPT_Enable_Backedge_Placement, NULL },
  { OVK_BOOL,   OV_VISIBLE,	TRUE, "src_order",		"",
    0, 0, 0,	&WOPT_Enable_Source_Order, NULL },
  { OVK_NAME,	OV_VISIBLE,	TRUE, "set_unique_pt",	"",
    0, 0, 0,	&WOPT_Set_Unique_Pt, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "fold_cvt",		"",
    0, 0, 0,	&WOPT_Enable_Cvt_Folding, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "local_rvi",		"",
    0, 0, 0,	&WOPT_Enable_Local_Rvi, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "strong_barrier",		"",
    0, 0, 0,	&WOPT_Enable_Strong_Barrier, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "aggr_invariant",		"",
    0, 0, 0,	&WOPT_Enable_Aggr_Invariant, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "lrvi_limit",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Local_Rvi_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "shrink",		"",
    0, 0, 0,	&WOPT_Enable_Shrink, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "extra_rename",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Extra_Rename_Pass, &WOPT_Enable_Extra_Rename_Pass_Set },
  { OVK_UINT32,	OV_VISIBLE,	TRUE, "extra_preopt",		"extra_p",
    0, 0, UINT32_MAX,	&WOPT_Enable_Extra_Preopt_Pass, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "bool_simp",		"",
    0, 0, 0,	&WOPT_Enable_Bool_Simp, NULL },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "fb_lpre",              "",
    0, 0, 0,    &WOPT_Enable_Feedback_LPRE, NULL },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "fb_epre",              "",
    0, 0, 0,    &WOPT_Enable_Feedback_EPRE, NULL },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "cfo_display",              "",
    0, 0, 0,    &WOPT_Enable_CFG_Display, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "merge_multi_zone",		"",
    0, 0, 0,	&WOPT_Enable_CFG_Merge_Multi_Zone, &WOPT_Enable_CFG_Merge_Multi_Zone_Set },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "cfo_main",              "",
    0, 0, 0,    &WOPT_Enable_CFG_Opt, NULL },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "cfo1",              "",
    0, 0, 0,    &WOPT_Enable_CFG_Opt1, NULL },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "cfo2",              "",
    0, 0, 0,    &WOPT_Enable_CFG_Opt2, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "min_cfo2_coverage",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_CFG_Opt2_Limit, NULL },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "cfo3",              "",
    0, 0, 0,    &WOPT_Enable_CFG_Opt3, NULL },
  { OVK_BOOL,   OV_VISIBLE,    FALSE, "cfo4",              "",
    0, 0, 0,    &WOPT_Enable_CFG_Opt4, NULL },
  { OVK_INT32,  OV_VISIBLE,    FALSE, "cfo_limit",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Enable_CFG_Opt_Limit, NULL },
  { OVK_INT32,  OV_VISIBLE,    TRUE,  "vn_ivc",	"",
    1, 0, 8,    &WOPT_Enable_Vn_Ivc, NULL },
  { OVK_UINT32,	OV_VISIBLE,	TRUE, "vnfre_after",		"vnfre_a",
    0, 0, UINT32_MAX,	&WOPT_Enable_Vnfre_After, NULL },
  { OVK_UINT32,	OV_VISIBLE,	TRUE, "vnfre_before",		"vnfre_b",
    UINT32_MAX, 0, UINT32_MAX,	&WOPT_Enable_Vnfre_Before, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "bit_field",	"",
    0, 0, 0,	&WOPT_Enable_Bits_Load_Store, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "epre4ivr",	"epre4ivr",
    0, 0, 0,	&WOPT_Enable_Epre_Before_Ivr, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "lpre4ivr",	"lpre4ivr",
    0, 0, 0,	&WOPT_Enable_Lpre_Before_Ivr, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "spre4ivr",	"spre4ivr",
    0, 0, 0,	&WOPT_Enable_Spre_Before_Ivr, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "bdce4ivr",	"bdce4ivr",
    0, 0, 0,	&WOPT_Enable_Bdce_Before_Ivr, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "new_phase_order", "new_phase_order",
    TRUE, 0, 0,	&WOPT_Enable_New_Phase_Ordering, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "pro_loop_fusion_trans", "pro_loop_fusion_trans",
    FALSE, 0, 1, &WOPT_Enable_Pro_Loop_Fusion_Trans, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "pro_loop_interchange_trans", "pro_loop_interchange_trans",
    FALSE, 0, 1, &WOPT_Enable_Pro_Loop_Interchange_Trans, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "pro_loop_ext_trans", "pro_loop_ext_trans",
    FALSE, 0, 1, &WOPT_Enable_Pro_Loop_Ext_Trans, NULL },
  { OVK_BOOL,	OV_VISIBLE, TRUE, "simp_bit_op", "simp_bit_op",
    FALSE, 0, 1, &WOPT_Simplify_Bit_Op, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "reasso_cse", "reasso_cse",
    TRUE, 0, 1, &WOPT_Enable_Reassociation_CSE, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "mem_clear_remove", "mem_clear_remove",
    FALSE, 0, 1, &WOPT_Enable_Mem_Clear_Remove, NULL },
  { OVK_INT32,  OV_VISIBLE,    FALSE, "pro_loop_limit",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Enable_Pro_Loop_Limit, NULL },
  { OVK_INT32,  OV_VISIBLE,    FALSE, "tail_dup_max_clone",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Tail_Dup_Max_Clone, NULL },
  { OVK_INT32,  OV_VISIBLE,    FALSE, "pro_loop_fusion_func_limit",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Enable_Pro_Loop_Fusion_Func_Limit, NULL },
  { OVK_INT32,  OV_VISIBLE,    FALSE, "pro_loop_interchange_func_limit",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Enable_Pro_Loop_Interchange_Func_Limit, NULL },
  { OVK_INT32,  OV_VISIBLE,    FALSE, "pro_loop_ext_func_limit",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Enable_Pro_Loop_Ext_Func_Limit, NULL },

#ifdef KEY
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "mem_opnds", "mem_opnds",
    TRUE, 0, 0,	&WOPT_Enable_Preserve_Mem_Opnds, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "retype_expr", "retype_expr",
    TRUE, 0, 0,	&WOPT_Enable_Retype_Expr, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "folded_scalar_limit",		"",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Folded_Scalar_Limit, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "bdceprop_limit",	"bdceprop_limit",
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_Bdceprop_Limit, NULL },
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "warn_uninit",		"",
    0, 0, 0,	&WOPT_Enable_Warn_Uninit, NULL },
  { OVK_INT32,	OV_VISIBLE,	TRUE, "unroll",	"unroll",
#ifdef TARG_IA64
    INT32_MAX, 0, INT32_MAX,	&WOPT_Enable_WN_Unroll, NULL },
#else
  2, 0, 2,    &WOPT_Enable_WN_Unroll, NULL },
#endif
  { OVK_LIST,   OV_VISIBLE,     TRUE, "unroll_skip_after", "unroll_skip_a",
    0, 0, 0,    &WOPT_Unroll_Skip,      NULL },
  { OVK_LIST,   OV_VISIBLE,     TRUE, "unroll_skip_before", "unroll_skip_b",
    0, 0, 0,    &WOPT_Unroll_Skip,      NULL },
  { OVK_LIST,   OV_VISIBLE,     TRUE, "unroll_skip_equal", "unroll_skip_e",
    0, 0, 0,    &WOPT_Unroll_Skip,       NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "ip_mod_ref", "ip_mod_ref",
    0, 0, 0,    &WOPT_Enable_IP_Mod_Ref, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "invar_loop_bounds", "invar_loop_bounds",
    FALSE, 0, 0, &WOPT_Enable_Invariant_Loop_Bounds, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "subword_opt", "subword_opt",
    FALSE, 0, 0, &WOPT_Enable_Subword_Opt, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "new_vsym", "new_vsym",
    FALSE, 0, 0, &WOPT_Enable_New_Vsym_Allocation, NULL },

#endif
  { OVK_BOOL,	OV_VISIBLE,	TRUE, "wovp",	"wovp",
    0, 0, 0,	&WOPT_Enable_WOVP, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "trk_ptr",   NULL, 
    0, 0, 0,   &WOPT_Enable_Pt_Keep_Track_Ptr, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "aggr_trk_ptr",   NULL, 
    0, 0, 0,   &WOPT_Enable_Aggr_Pt_Keep_Track_Ptr, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "noreturn_attr",   NULL, 
    0, 0, 0,   &WOPT_Enable_Noreturn_Attr_Opt, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "nothrow_opt",   NULL, 
    0, 0, 0,   &WOPT_Enable_Nothrow_Opt, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "multiver_unroll",   NULL, 
    0, 0, 0,   &WOPT_Enable_Multiver_and_Unroll_Opt, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "pt_summary",   NULL, 
    0, 0, 0,   &WOPT_Enable_Pt_Summary, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "loop_multiver",   NULL, 
    0, 0, 0,   &WOPT_Enable_Loop_Multiver, &WOPT_Enable_Loop_Multiver_Set },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "loop_multiver_aggr",   NULL,
    0, 0, 0,   &WOPT_Enable_Loop_Multiver_Aggressive, NULL },
  { OVK_BOOL,  OV_INTERNAL,    TRUE, "useless_store_elimination",   NULL,
    0, 0, 0,   &WOPT_Enable_Useless_Store_Elimination, NULL },
#ifdef TARG_NVISA
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "estr_outer_loop",        "",
    0, 0, 0,    &WOPT_Enable_Estr_Outer_Loop, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "estr_const_opnds",       "",
    0, 0, 0,    &WOPT_Enable_Estr_Const_Opnds, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "estr_used_once", "",
    0, 0, 0,    &WOPT_Enable_Estr_Used_Once, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "estr_early_exit",        "",
    0, 0, 0,    &WOPT_Enable_Estr_Early_Exit, NULL },
  { OVK_BOOL,   OV_VISIBLE,     TRUE, "aggr_iload_cse",        "",
    0, 0, 0,    &WOPT_Enable_Aggressive_Iload_CSE, NULL },
#endif
  { OVK_BOOL,   OV_INTERNAL,    TRUE, "bottom_test_loop_check",    "",
      0, 0, 0,  &WOPT_Bottom_Test_Loop_Check, NULL},
  { OVK_INT32,  OV_VISIBLE,    FALSE, "bottom_test_loop_cond",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Bottom_Test_Loop_Cond_Limit, NULL },
  { OVK_INT32,  OV_VISIBLE,    FALSE, "bottom_test_loop_body",              "",
    INT32_MAX, 0, INT32_MAX,    &WOPT_Bottom_Test_Loop_Body_Limit, NULL },
  { OVK_COUNT }		/* List terminator -- must be last */
};
