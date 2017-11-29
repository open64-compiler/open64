/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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
 * Module: config_wopt.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_wopt.h,v $
 *
 * Revision history:
 *  05-May-96 - Extracted from be/opt/opt_config.h.
 *
 * Description:
 *
 * Declare global flag variables for -WOPT group options.
 * This file is included in common/com/config.c, but should not be
 * otherwise used outside of WOPT.
 *
 * ====================================================================
 * WARNING: WHENEVER A NEW FLAG IS ADDED:
 * ###	- Add its definition to config_wopt.c .
 * ###	- Add it to the group description config_wopt.c .
 * ###	- UPDATE 'class WOPT_SWITCH' DEFINED IN be/opt/opt_main.cxx.
 * ====================================================================
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_wopt_INCLUDED
#define config_wopt_INCLUDED

/* Incomplete types to prevent unnecessary inclusion: */
struct skiplist;

/*********************************************************************
 ***
 *** Anything that may be seen outside of opt_config.c should
 *** be declared in this section.
 ***
 *********************************************************************
 */

extern UINT32 WOPT_Alias_Class_Limit;
extern UINT32 WOPT_Ip_Alias_Class_Limit;
extern BOOL WOPT_Ldx_Ratio_RegIns;
extern BOOL WOPT_Enable_Add_Do_Loop_Info;
extern BOOL WOPT_Enable_Add_Label_Loop_Info;
extern UINT32 WOPT_Enable_Aggressive_Code_Motion;
extern INT32 WOPT_Enable_Aggressive_CM_Limit;	
extern INT32 WOPT_Enable_Aggressive_CM_Switch_Threshold;
extern INT32 WOPT_Enable_Aggressive_CM_Branch_Threshold;
extern BOOL WOPT_Enable_Aggressive_dce;
extern BOOL WOPT_Enable_Aggressive_dce_for_bbs;
extern BOOL WOPT_Enable_Aggressive_Doloop_Promotion;
extern BOOL WOPT_Enable_Aggressive_IVR;
extern BOOL WOPT_Enable_Aggressive_Lftr;
extern BOOL WOPT_Enable_Aggressive_Phi_Simp;
extern INT32 WOPT_Enable_Autoaggstr_Reduction_Threshold;
extern BOOL WOPT_Enable_Alias_Classification;
extern BOOL WOPT_Enable_Aggressive_Alias_Classification;
extern BOOL  WOPT_Enable_Disambiguate_Heap_Obj;
extern BOOL WOPT_Enable_Alias_Class_Fortran_Rule;
#if defined(TARG_SL)
extern BOOL WOPT_Enable_Alias_Intrn;
extern BOOL WOPT_Enable_Local_Clsc;
#endif
extern BOOL WOPT_Enable_Avoid_Rehash;	/* SSAPRE to try to minimize rehashing*/
extern BOOL WOPT_Enable_Backedge_Placement; /* BB on critical backedge */
extern BOOL WOPT_Enable_Bitwise_DCE;
extern BOOL WOPT_Enable_CSE_FP_comparison;
extern BOOL WOPT_Enable_Call_Flag;
extern BOOL WOPT_Enable_Calls_Break_BB;
extern BOOL WOPT_Enable_Calls_Break_BB_Set;
extern BOOL WOPT_Enable_Call_Zero_Version; /* allow zero versions at calls */
extern BOOL WOPT_Enable_Canon_Expr;
extern BOOL WOPT_Enable_Canon_Uplevel;  /* canonicalize the up level ref */
/* allow operations in expression to be combined (or possibly just
 * rearranged to allow other optimizations).
 */
extern BOOL WOPT_Enable_Combine_Operations;
extern BOOL WOPT_Enable_Compare_Simp;
extern BOOL WOPT_Enable_Const_PRE;
extern INT32 WOPT_Enable_Const_PRE_Limit;
#ifdef TARG_NVISA
extern BOOL WOPT_Enable_Const_Var_PRE;  /* lpre of const_vars */
extern BOOL WOPT_Enable_Const_Op_PRE;   /* epre of ops with const{var} kids */
extern INT32 WOPT_Const_PRE_Float_Size; /* lpre of consts starting at this size */
#endif
extern BOOL WOPT_Enable_Copy_Propagate;
extern BOOL WOPT_Enable_Copy_Prop_Bad_Ops;
extern BOOL WOPT_Enable_Copy_Prop_LNO_Ops;
/* copy prop certain ops into ARRAY subscripts */
extern BOOL WOPT_Enable_Copy_Prop_Ops_Into_Array;
extern BOOL WOPT_Enable_Copy_Prop_Ops_Into_Array_Set;
extern BOOL WOPT_Enable_Cvt_Folding;    /* enable folding of CVT/CVTL in emitter */    
extern BOOL WOPT_Enable_DIVREM;		/* allow DIVREM opcode */
extern BOOL WOPT_Enable_CG_Alias;
extern BOOL WOPT_Enable_CRSIMP;         /* simplify coderep */
extern BOOL WOPT_Enable_DCE;
extern BOOL WOPT_Enable_DCE_Alias;	/* extra alias analysis w/DCE */
extern BOOL WOPT_Enable_DCE_Branch;	/* delete redundant condition */
extern INT32 WOPT_Enable_DCE_Branch_Pred_Limit;	/* local search limit */
extern BOOL WOPT_Enable_DCE_Global;	/* delete global stores */
extern BOOL WOPT_Enable_DCE_Label;	/* delete unref'd labels */
extern BOOL WOPT_Enable_Dse_Aggressive;
extern BOOL WOPT_Enable_DU_Full;	/* full DU-info for indirects */
extern BOOL WOPT_Enable_DU_Union;	/* fix DU w/unions */
extern BOOL WOPT_Enable_Estr_FB_Injury; /* use feedback frequency to */
					/* decide when IV updates are */
					/* reparable injuries */
extern BOOL WOPT_Enable_Exp_PRE;
extern INT32 WOPT_Enable_Exp_PRE_Limit;
extern BOOL WOPT_Enable_Dead_CR_Fix;    /* process dead CR in main_emit time */

// Tighten up assertions about interprocedural alias class
extern BOOL WOPT_Enable_Debug_Inconsistent_Ip_Class;

extern BOOL WOPT_Enable_Edge_Placement; /* insert BB on critical edge */
extern BOOL WOPT_Enable_Fast_Simp;	/* temporary 377066 */
extern BOOL WOPT_Enable_Fold2const;
extern BOOL WOPT_Enable_Fold_Lda_Iload_Istore; // for folding in coderep
extern BOOL WOPT_Enable_FSA;
extern BOOL WOPT_Enable_Generate_DU;
extern INT32 WOPT_Enable_Generate_Trip_Count;
extern BOOL WOPT_Enable_Goto;
extern BOOL WOPT_Enable_Hoisting;
extern BOOL WOPT_Enable_Ivar_Hoisting;
extern BOOL WOPT_Enable_I8_Primary_IV;  /* allow primary IV to be I8 */
extern BOOL WOPT_Enable_Iload_Prop;	/* propagate expression containing ivars */
extern BOOL WOPT_Enable_Improved_Addr_Taken;
extern BOOL WOPT_Enable_Input_Prop;     /* copy prop at value number time */
extern BOOL WOPT_Enable_Itself_Prop;     /* copy prop of t=func(t) where func
					    contains only t plus other constants */
extern BOOL WOPT_Enable_IPAA;           /* enable the use of IPA alias analysis result */
extern BOOL WOPT_Enable_IVE;		/* induction-var elimination */
extern BOOL WOPT_Enable_IVE_Old;        /* use old IVE with bug in it */
extern BOOL WOPT_Enable_Ivar_Common;
extern BOOL WOPT_Enable_Ivar_PRE;       /* enable *p as PRE candidate */
extern BOOL WOPT_Enable_Ivincr_Cand;
extern BOOL WOPT_Enable_IVR;		/* induction-var recognition */
extern INT32 WOPT_Enable_IVR_Expand_Limit;  /* limit of expr expansion to search for incr */
#ifdef KEY
extern INT32 WOPT_Enable_Ivr_Limit;
extern INT32 WOPT_Enable_Ivr_Cand_Limit;
#endif
					
/* do ivr for outermost in ||-region */
extern BOOL WOPT_Enable_IVR_Outermost_Loop_Parallel_Region; 
extern BOOL WOPT_Enable_Ldx;            /* index load optimization */
extern BOOL WOPT_Enable_Lego_Opt;       /* max optimization for lego */
extern BOOL WOPT_Enable_LFTR_Ivar;      /* handle expr containing ivars */
extern BOOL WOPT_Enable_LFTR2;          /* linear function test replacement */
extern BOOL WOPT_Enable_LFTR2_Limit;
extern BOOL WOPT_Enable_Load_PRE;
extern INT32 WOPT_Enable_Load_PRE_Limit;
extern BOOL WOPT_Enable_Local_Rvi;      /* enable fast rvi of locals */    
extern INT32 WOPT_Enable_Local_Rvi_Limit;
extern BOOL WOPT_Enable_Loopinvarexp_Str_Reduction;
extern BOOL WOPT_Enable_Lower_Short_Circuit;
extern BOOL WOPT_Enable_Lower_Short_Circuit_Set;
extern BOOL WOPT_Enable_LNO_Copy_Propagate;
extern BOOL WOPT_Enable_MINMAX;		/* allow MINMAX opcode */
extern BOOL WOPT_Enable_Min_Type;       /* use minimum size type in PRE PREG */
extern BOOL WOPT_Enable_Move_Intrinsicop;
extern BOOL WOPT_Enable_MP_varref;      /* trust the var list in the nested procedure */
extern const BOOL WOPT_Enable_MP_Const_Prop;  /* perform const prop into MP region */
extern BOOL WOPT_Enable_New_SR;		/* new strength-reduction */
extern BOOL WOPT_Enable_New_SR_Limit;
extern BOOL WOPT_Enable_SIB;		/* abandon some SR candidate for x86 SIB */
extern BOOL WOPT_Enable_Output_Copy;    /* output copy propagation */
extern BOOL WOPT_Enable_Ocopy_Lookupstmt;
extern BOOL WOPT_Enable_Parm;		/* insert OPTPARM over parms */
extern char *WOPT_Enable_Process;
extern BOOL WOPT_Enable_Phi_Simp;
extern BOOL WOPT_Enable_Prop_Aggressive;/* use inverse function to avoid 
					   overlapping live ranges */
extern BOOL WOPT_Enable_Prop_Ivar;	/* copy propagation thru iload's */
extern BOOL WOPT_Enable_Prop_CSE;       /* copy propagation of CSE expressions */
extern INT32 WOPT_Enable_Prop_Limit;	/* tree height limit in copy prop */
extern INT32 WOPT_Enable_Prop_Weight_Limit; /* tree weight limit in copy prop */
#ifdef KEY
extern INT32 WOPT_Enable_Doend_Prop_Limit; /* tree height limit in copy prop for DOEND BBs */
extern BOOL  WOPT_Enable_Prop_Dope;	/* propagate dope vector fields? */
#endif
extern BOOL WOPT_Enable_Prune;		/* temporary, pv 370066 */
extern BOOL WOPT_Enable_Replace_Second_IV; /* Force replacement of secondary IV */
extern BOOL WOPT_Enable_Replace_While_Loop_Second_IV; /* Force replacement of secondary IV */
extern BOOL  WOPT_Enable_Restricted_Map;
extern INT32 WOPT_Enable_Rsv_Bits;	/* reserve bit count in itable */
extern BOOL WOPT_Enable_RVI;		/* reg-var identification */
extern BOOL WOPT_Enable_RVI1;		/* rvi phase 1 */
extern BOOL WOPT_Enable_RVI2;		/* rvi phase 2 */
extern BOOL WOPT_Enable_Rviistore;	/* agg. chi-handling on istore*/
extern char *WOPT_Enable_Rviskip;	/* skip variable during rvi */
extern BOOL WOPT_Enable_Rvisplit;	/* split bbs at ever stmt */
extern BOOL WOPT_Enable_Rvivsym;	/* ignore vsym in chi lists */
extern BOOL WOPT_Enable_Second_Alias_Class; /* repeat alias class for LNO */
extern BOOL WOPT_Enable_Second_Order;
extern BOOL WOPT_Enable_Simp_Iload;	/* simplifier folding iload */
extern INT32 WOPT_Enable_Simple_If_Conv; /* enable simple if-conversion at CFG build time */
extern INT32 WOPT_Enable_If_Conv_Limit; /* max number of leaf nodes allowed in a
					   simple expr in simple if conv */
extern BOOL WOPT_Enable_If_Conv_For_Istore; /* if-conversion is applied if lhs is istore */
#if defined(TARG_SL)
extern BOOL WOPT_Enable_If_Conv_For_Iload;  /* if-conversion is applied if rhs is iload */
#endif
extern char *WOPT_Enable_Skip;
extern struct option_list *WOPT_Skip;	/* Skip option list */
extern struct skiplist *WOPT_Skip_List;	/* Preprocessed skip list */
extern BOOL WOPT_Enable_SLT;
extern BOOL WOPT_Enable_Small_Br_Target; /* Disable propagation into br BBs */
extern BOOL WOPT_Enable_Source_Order;   /* trace BB's in source order */
extern BOOL WOPT_Enable_Speculation_Defeats_LFTR;
extern BOOL  WOPT_Enable_Str_Red_Use_Context; /* use loop content in SR decision */
extern BOOL WOPT_Enable_SSA_Minimization; /* SSA minimization in SSAPRE */
extern BOOL WOPT_Enable_SSA_PRE;
extern BOOL WOPT_Enable_Store_PRE;
extern INT32 WOPT_Enable_Store_PRE_Limit;
extern BOOL WOPT_Enable_Tail_Recur;	/* tail recursion opt */
extern BOOL WOPT_Enable_Update_Vsym;
extern char *WOPT_Set_Unique_Pt;
extern BOOL WOPT_Enable_Unique_Pt_Vsym;
extern INT32 WOPT_Enable_Value_Numbering; /*0=OFF, 1=after_pre, 2=befr_n_aftr*/
extern INT32 WOPT_Enable_Vn_Ivc;        /* Induction variable coalescing; 0=OFF
					 * See be/opt/opt_vn_ivc.h */
extern UINT32 WOPT_Enable_Vnfre_After;  /* Disable vnfre after given valnum */
extern UINT32 WOPT_Enable_Vnfre_Before; /* Disable vnfre before given valnum */
extern BOOL WOPT_Enable_Verbose;
extern INT32 WOPT_Enable_Verify;	/* verify data structures */
extern BOOL WOPT_Enable_Vsym_Unique;
extern BOOL WOPT_Enable_VN_Full;	/* full value number for ivars */
extern BOOL WOPT_Enable_While_Loop;	/* cvt while-do to do-loop */
extern BOOL WOPT_Enable_Worklist_Pruning;
extern BOOL WOPT_Enable_Zero_Version;
extern BOOL WOPT_Enable_Strong_Barrier; /* disallow any memop motion across a barrier */
extern BOOL WOPT_Enable_Aggr_Invariant; /* aggressive invariant detection */
extern BOOL WOPT_Enable_Shrink;         /* enable live range shrinking */
extern INT32 WOPT_Enable_Extra_Rename_Pass;
extern BOOL  WOPT_Enable_Extra_Rename_Pass_Set;
extern UINT32 WOPT_Enable_Extra_Preopt_Pass; // additional iterations of preopt
extern BOOL  WOPT_Enable_Bool_Simp; 
extern BOOL  WOPT_Enable_Feedback_LPRE;
extern BOOL  WOPT_Enable_Feedback_EPRE;
extern BOOL  WOPT_Enable_CFG_Opt;
extern BOOL  WOPT_Enable_CFG_Display;
extern BOOL  WOPT_Enable_CFG_Merge_Multi_Zone;
extern BOOL  WOPT_Enable_CFG_Merge_Multi_Zone_Set;
extern BOOL  WOPT_Enable_CFG_Opt1;
extern BOOL  WOPT_Enable_CFG_Opt2;
extern INT32 WOPT_Enable_CFG_Opt2_Limit;
extern BOOL  WOPT_Enable_CFG_Opt3;
extern BOOL  WOPT_Enable_CFG_Opt4;
extern BOOL  WOPT_Enable_CFG_Opt_Limit;
extern BOOL  WOPT_Enable_Bits_Load_Store;
extern BOOL  WOPT_Enable_Epre_Before_Ivr; // For running epre early
extern BOOL  WOPT_Enable_Lpre_Before_Ivr; // For running lpre early
extern BOOL  WOPT_Enable_Spre_Before_Ivr; // For running spre early
extern BOOL  WOPT_Enable_Bdce_Before_Ivr; // For running bdce early
extern BOOL  WOPT_Enable_New_Phase_Ordering; // Enables some phases before ivr
extern BOOL  WOPT_Enable_Pt_Keep_Track_Ptr;  // POINTS_TO keep track pointer of ild/istore
  // POINTS_TO keep track complex address of ilod/istore
extern BOOL  WOPT_Enable_Aggr_Pt_Keep_Track_Ptr;  
extern BOOL  WOPT_Enable_Noreturn_Attr_Opt;  // __attribute_((noreturn)) related opt
extern BOOL  WOPT_Enable_Nothrow_Opt; // remove unneceesary RID for calls that won't throw exception
extern BOOL  WOPT_Enable_Multiver_and_Unroll_Opt; // multi-versioning followed by fully unroll
extern BOOL  WOPT_Enable_Pt_Summary;  // Points-to summary/annotation
extern BOOL  WOPT_Enable_Reassociation_CSE; // Enables reassociative CSE
extern BOOL  WOPT_Enable_Pro_Loop_Fusion_Trans; // Enables proactive loop fusion transformation
extern BOOL  WOPT_Enable_Pro_Loop_Interchange_Trans; // Enables proactive loop interchange transformation.
extern BOOL WOPT_Enable_Pro_Loop_Ext_Trans; // Enables proactive loop extended transformation.
extern BOOL  WOPT_Enable_Mem_Clear_Remove;  // Enables removal of redundant mem clear after a calloc
extern INT32 WOPT_Enable_Pro_Loop_Fusion_Func_Limit; // Enable proactive loop fusion transformation
                                                     // for functions within the limit.
extern INT32 WOPT_Enable_Pro_Loop_Interchange_Func_Limit; // Enable proactive loop interchange
                                                          // transformation for functions within the limit.
extern INT32 WOPT_Enable_Pro_Loop_Ext_Func_Limit; // Enable proactive loop extended transformation
                                                  // for functions within the limit.
                                                         
extern INT32 WOPT_Enable_Pro_Loop_Limit;  // Limit number of proactive loop transformations per function.
extern INT32 WOPT_Tail_Dup_Max_Clone; // Limit code size bloats (in statement count)
                                                    // due to tail-duplication.
extern BOOL WOPT_Simplify_Bit_Op; // Enable specialized bit operation optimizations.

#ifdef KEY
extern BOOL  WOPT_Enable_Preserve_Mem_Opnds; // if TRUE, suppress EPRE on 
				// iloads that are operands of FP operations
extern BOOL  WOPT_Enable_Retype_Expr;   // whether to call WN_retype_expr to 
					// change 64-bit operations to 32-bit 
extern INT32 WOPT_Enable_Folded_Scalar_Limit; // to limit number of scalars
					// formed by Fold_Lda_Iload_Istore()
extern INT32 WOPT_Enable_Bdceprop_Limit; // to limit the BBs in which BDCE's
					// copy propagation is performed
extern BOOL WOPT_Enable_Warn_Uninit;   // enable warning for detected uninitialized locals
extern INT32 WOPT_Enable_WN_Unroll;	// 0: disable; 
					// 1: unroll only loop bodies with IFs
					// 2: unroll all loop bodies
extern BOOL WOPT_Enable_IP_Mod_Ref;     // Use mod/ref information from IPA?
extern BOOL WOPT_Enable_Invariant_Loop_Bounds; // enable assumption that all
				   // induction loops' bounds are loop-invariant
extern BOOL WOPT_Enable_Subword_Opt; // whether to replace 1- or 2-byte-sized
			              // load/store with EXTRACT/COMPOSE
extern BOOL WOPT_Enable_New_Vsym_Allocation;
#endif
extern BOOL  WOPT_Enable_WOVP; // For running write-once variable promotion
extern struct option_list *WOPT_Unroll_Skip;    // Skip unroll list 
extern struct skiplist *WOPT_Unroll_Skip_List;  // Preprocessed unroll skip l 
extern BOOL WOPT_Enable_Loop_Multiver;
extern BOOL WOPT_Enable_Loop_Multiver_Aggressive;
extern BOOL WOPT_Enable_Useless_Store_Elimination;
extern BOOL WOPT_Enable_ZDL;
extern BOOL WOPT_Enable_ZDL_Set;
extern BOOL WOPT_Enable_ZDL_Early_Exit;
extern BOOL WOPT_ZDL_Innermost_Only;
extern struct option_list *WOPT_ZDL_Skip;    // Skip ZDL list
extern struct skiplist *WOPT_ZDL_Skip_List; // Preprocessed ZDL skip l
#ifdef TARG_NVISA
extern BOOL WOPT_Enable_Estr_Outer_Loop;  // strength reduce outer loops
extern BOOL WOPT_Enable_Estr_Const_Opnds; // strength reduce ops with const kids
extern BOOL WOPT_Enable_Estr_Used_Once;   // strength reduce ops used only once
extern BOOL WOPT_Enable_Estr_Early_Exit;  // strength reduce early exit loops
extern BOOL WOPT_Enable_Aggressive_Iload_CSE; // ignore potential iload vsym aliasing
#endif

extern BOOL WOPT_Enable_STR_Short;  // whether to assume 16bit IV can cross 16

extern BOOL WOPT_Bottom_Test_Loop_Check;
extern INT32 WOPT_Bottom_Test_Loop_Cond_Limit;
extern INT32 WOPT_Bottom_Test_Loop_Body_Limit;

#endif /* config_wopt_INCLUDED */

