/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_ipa.cxx,v $
 *
 * Revision history:
 *  11-Apr-96 - Original Version, extracted from ipa_option.c.
 *
 * Description:
 *
 * Configure the -IPA and -INLINE groups (included in config.c).
 * Used from ipl, inline, and ipa (ld).
 *
 * ====================================================================
 * ====================================================================
 */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_ipa.h"

/* IPA file variables declared in glob.h: */
char *Ipa_File_Name = NULL;	/* IPAA summary file name */
char *Feedback_Filename = NULL;
char *Annotation_Filename = NULL;
FILE *Ipa_File = NULL;		/* IPAA summary file (if open) */

/* Skiplist support from config.h: */
typedef struct skiplist SKIPLIST;
SKIPLIST *Build_Skiplist ( OPTION_LIST *olist );

/* ====================================================================
 * List of global variables that are set by the -IPA option group
 * ====================================================================
 */

#ifdef KEY
const float DEFAULT_MIN_PROBABILITY = 0.20;
#endif


#define DEFAULT_BLOAT_FACTOR	100
#define DEFAULT_PU_LIMIT	2500
#define DEFAULT_SMALL_PU	30
#define DEFAULT_HARD_LIMIT	(DEFAULT_PU_LIMIT + (DEFAULT_PU_LIMIT >> 2))
#define DEFAULT_SMALL_CALLEE	500
#define DEFAULT_MIN_FREQ	100
#define DEFAULT_MIN_HOTNESS	10
#define DEFAULT_RELA_FREQ	50
#define DEFAULT_INLINE_Max_Pu_Size 5000
#define DEFAULT_CLONE_BLOAT_FACTOR 100
#define DEFAULT_EXTGOT_FACTOR 	200     /* 2 times the estimated EXTGOT entries */
#define DEFAULT_NUM_FORTRAN_INTR 100    /* Arbitary estimate */
#define DEFAULT_MAP_LIMIT	0x1fff0000   /* total size in bytes we
						allow input objects to be
						mapped in at the same time
						without turning into the
						SAVE_SPACE option  
					      */
#define DEFAULT_OUTPUT_FILE_SIZE	10000

#define DEFAULT_MAX_DENSITY		11 // INLINING_TUNINING

#ifdef KEY
#define DEFAULT_ICALL_MIN_FREQ		200
#endif

#define DEFAULT_ICALL_TARGET_MIN_RATE   30

/* #define DEFAULT_GSPACE	65535	-- from config.c */

/* Feature enable flags: */
BOOL IPA_Enable_DFE = TRUE;		/* Dead Function Elimination */
BOOL IPA_Enable_DFE_Set = FALSE;	/* ... explicitly set? */
BOOL IPA_Enable_Inline = TRUE;		/* Inlining */
BOOL IPA_Enable_Picopt = TRUE;		/* PIC optimization */
BOOL IPA_Enable_AutoGnum = TRUE;	/* AutoGnum optimization */
BOOL IPA_Enable_Opt_Alias =FALSE;	/* WOPT uses alias/mod/ref */
BOOL IPA_Enable_Simple_Alias = TRUE;	/* Simple alias/mod/ref */
BOOL IPA_Enable_Addressing = TRUE;	/* Addr_taken analysis */
BOOL IPA_Enable_BarrierFarg = FALSE;	/* Barrier for aliased Fortran actual */
BOOL IPA_Enable_Alias_Class = TRUE;     /* Alias classification */
BOOL IPA_Debug_AC_Temp_Files = FALSE;   /* save alias class temps til done? */
BOOL IPA_Enable_Readonly_Ref = TRUE;	/* find out readonly ref parameter */
BOOL IPA_Enable_Cprop = TRUE;		/* Constant Propagation */
BOOL IPA_Enable_Cprop2 = TRUE;		/* Aggressive constant propagation */
BOOL IPA_Enable_Assert = FALSE;		// generate assert statement for
					// cprop debugging
BOOL IPA_Enable_daVinci = FALSE;	/* Graphical display of call graph */
BOOL IPA_Enable_ipacom = TRUE;		/* Call ipacom after IPA */
BOOL IPA_Enable_final_link = TRUE;	/* Final link step */
BOOL IPA_Enable_Memtrace = FALSE;	/* Memory trace */
BOOL IPA_Enable_DST = TRUE;		/* Generate DST */
BOOL IPA_Enable_DCE = TRUE;		/* Enable Dead Call Elimination */
BOOL IPA_Enable_Exc = TRUE;		/* Enable exception handling */
BOOL IPA_Enable_Recycle = TRUE;		/* Enable recycling of variables */
BOOL IPA_Enable_DVE = TRUE;		/* Enable Dead Variable Elimination */
BOOL IPA_Enable_CGI = TRUE;		/* Enable Constant Global Variable
					 * Identification */
BOOL IPA_Enable_Copy_Prop = TRUE;	/* Copy propagation during inlining */
BOOL IPA_Enable_Padding = TRUE;		/* Intra-Dimension padding 
					 * of common block variables */
UINT32 IPA_Common_Pad_Size = 0;	        /* Amount by which to pad commons */

BOOL IPA_Enable_Cloning = TRUE;         /* Enable Cloning in conjunction */
                                        /* with constant propagation     */
BOOL IPA_Enable_Partial_Inline = TRUE; /* Enable partial inlining */
BOOL IPA_Enable_Lang = FALSE;           /* support inlining across language */
BOOL IPA_Enable_Relocatable_Opt = FALSE;/* support -call_shared optimizations of relocatable objects */
BOOL IPA_Enable_Split_Common = TRUE;    /* Enable split common inside IPA */
BOOL IPA_Enable_Array_Sections = TRUE;  /* Array section analysis in IPA */
BOOL IPA_Enable_Array_Summary = FALSE;  /* Array section summary in IPL */

BOOL IPA_Enable_Scalar_Euse = FALSE;   /* enable scalar euse  */
BOOL IPA_Enable_Scalar_Kill = FALSE;   /* enable scalar kill   */
BOOL IPA_Enable_Common_Const = TRUE; /* enable cprop of common block vars */
BOOL IPA_Enable_Feedback = FALSE;       /* create pragma files, etc.      */

/* Echo back end command lines: */
BOOL	IPA_Echo_Commands = FALSE;

/* max. bloat % of the entire program */
UINT32	IPA_Bloat_Factor = DEFAULT_BLOAT_FACTOR;
BOOL	IPA_Bloat_Factor_Set = FALSE;

#ifdef KEY
float	IPA_Min_Branch_Prob = DEFAULT_MIN_PROBABILITY;
#endif
UINT32	IPA_PU_Limit = DEFAULT_PU_LIMIT;/* Max nodes per PU after inlining */
BOOL	IPA_PU_Limit_Set = FALSE;	/* if IPA_PU_Limit is set by user */

/* absolute max. # of nodes per PU after inlining (1.25 * IPA_PU_Limit) */
UINT32	IPA_PU_Hard_Limit = DEFAULT_HARD_LIMIT;
BOOL	IPA_PU_Hard_Limit_Set = FALSE;

/* Size of small PU that's always inlined */
UINT32	IPA_PU_Minimum_Size = DEFAULT_SMALL_PU;

/* Callees larger than this size are not inlined, except ...  */
UINT32	IPA_Small_Callee_Limit = DEFAULT_SMALL_CALLEE; 

UINT32	IPA_Max_Depth = UINT32_MAX;	/* maximum depth to inline */
UINT32	IPA_Force_Depth = 0;		/* force inlining to depth n
					 * regardless of size */
BOOL	IPA_Force_Depth_Set = FALSE;
BOOL	IPA_Enable_Merge_ty = TRUE;	/* merge types across files */

#ifdef KEY
UINT32	IPA_Max_Jobs = 1;	/* disable concurrent backend compilations */
#else
UINT32	IPA_Max_Jobs = 0;	/* concurrent backend compilations */
#endif
BOOL	IPA_Max_Jobs_Set = FALSE;

/* 100th% of call freq lower than which will not inlined */
UINT32	IPA_Min_Freq = DEFAULT_MIN_FREQ;	


/* For those infrequently invoked PU which however contains hot loops*/
UINT32  IPA_Max_Density = DEFAULT_MAX_DENSITY;// INLINING_TUNINING

/* % of time that an inlined callee is called by its caller */
UINT32	IPA_Rela_Freq = DEFAULT_RELA_FREQ;	

/* only routines "hotter" than this will be inlined */
UINT32  IPA_Min_Hotness = DEFAULT_MIN_HOTNESS;

/* ignore zero-freq. statements when estimating size of a PU */
BOOL IPA_Use_Effective_Size = TRUE;

/* max. gp-relative space available for auto Gnum */
UINT32	IPA_Gspace = DEFAULT_GSPACE - 72;// Kluge to get around gcc problem

/* % of estimtated external got size used for estimating the whole .got size */
UINT32  IPA_Extgot_Factor = 0;

/* number of FORTRAN instrinsics functions existed in this executable,
   used for estimating the number of GOTs */
UINT32  IPA_Num_Fortran_Intrinsics = DEFAULT_NUM_FORTRAN_INTR;
BOOL	IPA_Has_Fortran	= FALSE;

/* user specified -G num */
UINT32	IPA_user_gnum = 0;	

OPTION_LIST *IPA_Skip = NULL;		/* List of skip options */
BOOL	IPA_Skip_Report = FALSE;	/* Report skip count */

BOOL IPA_Enable_Preempt = FALSE;  /* allow the user to specify that
				     functions are not preemptible */

BOOL IPA_Enable_Flow_Analysis = TRUE;	/* flow-sensitive analysis */

/* Maximum limit IPA allows the whole program input to be mapped into
 * the address space without converting into the SAVE_SPACE mode
 */
UINT32 	IPA_Map_Limit = DEFAULT_MAP_LIMIT;

BOOL IPA_Enable_SP_Partition = FALSE;	/* Enable IPA to partition its
					 * call-graph so as to allow
					 * address space conversation
					 */
BOOL IPA_Enable_GP_Partition = FALSE;	/* Enable IPA to partition its
					 * call-graph so as to allow
					 * picopt even if multigot
					 */
BOOL IPA_Space_Access_Mode = DEFAULT_ACCESS_MODE;

BOOL IPA_Enable_Keeplight = TRUE;  /* allow the user to ONLY keep the .I
				     * and .o in the .ipakeep directory
				     */

#ifdef KEY
BOOL IPA_Enable_Icall_Opt = TRUE;   /* allow ipa change icall to call */
BOOL IPA_Enable_EH_Region_Removal = FALSE; // remove useless exception regions
BOOL IPA_Enable_Branch_Heuristic = FALSE; // use branch prob. for inlining
BOOL IPA_Check_Options = TRUE; // check for inconsistent options
BOOL IPA_Clone_List_Actions = FALSE; // report function cloner actions
BOOL IPA_Enable_Pure_Call_Opt = FALSE; // optimize callsites w/o side-effects
INT32 IPA_Pure_Call_skip_before = 0;

BOOL IPA_Enable_Cord = FALSE;		/* will bring up for x86-64 */
// In the absence of feedback, -IPA:pu_reorder defaults to REORDER_DISABLE
PU_REORDER_SCHEME IPA_Enable_PU_Reorder = REORDER_DISABLE; /* Procedure reordering: PathScale version */
BOOL IPA_Enable_PU_Reorder_Set = FALSE;
BOOL IPA_Enable_Ctype = FALSE;		/* Insert array for use by ctype.h. */
BOOL IPA_Consult_Inliner_For_Icall_Opt = TRUE; // Check inlining heuristics
                                               // during icall-opt?
UINT32 IPA_Icall_Min_Freq = DEFAULT_ICALL_MIN_FREQ; // Min freq for icall opt
                                                    // used in IPL.
// the ratio of an icall tartget be called 
// among all the indirect calls at the callsite,
// the ratio is range from 0 to 100. To promote
// an icall target, it must exceed the ratio
UINT32 IPA_Icall_Target_Min_Rate = DEFAULT_ICALL_TARGET_MIN_RATE;
                                  

BOOL IPA_Enable_Source_PU_Order = FALSE;
#ifdef TARG_X8664
UINT32 IPA_Enable_Struct_Opt = 1;
UINT32 IPA_Enable_Global_As_Local = 1;
#else
UINT32 IPA_Enable_Struct_Opt = 0;
UINT32 IPA_Enable_Global_As_Local = 0;
#endif
UINT32 IPA_Update_Struct = 0;		/* temporary, should be removed */
#else
BOOL IPA_Enable_Cord = TRUE;		/* Enable procedure reordering. */
#endif
BOOL IPA_Enable_Linearization = FALSE;  /* Enable linearization of array */
BOOL IPA_Use_Intrinsic = FALSE;		/* load intrinsic libraries */

BOOL IPA_Enable_Inline_Nested_PU = TRUE;  /* enable inlining of nested PU, for f90 */

BOOL IPA_Enable_Reshape = TRUE; 	// reshape analysis for arrays 
BOOL IPA_Enable_Inline_Struct = TRUE;  /* enable inlining of STRUCT for f90 */
BOOL IPA_Enable_Inline_Char_Array = TRUE;  /* enable inlining of Character Array for f90 */
BOOL IPA_Enable_Inline_Optional_Arg = TRUE;  /* enable inlining of functions with optional arguments */
BOOL IPA_Enable_Inline_Struct_Array_Actual = TRUE;  /* enable inlining of STRUCT for f90 when the actual is an array type */
BOOL IPA_Enable_Inline_Var_Dim_Array = TRUE;  /* enable inlining of variable-dimensioned array */
BOOL IPA_Enable_Reorder=FALSE; /*enable field reordering*/

// call preopt during IPA
BOOL IPA_Enable_Preopt = FALSE;          
BOOL IPA_Enable_Preopt_Set = FALSE;

// perform siloed reference analysis during IPA
BOOL IPA_Enable_Siloed_Ref = FALSE;
BOOL IPA_Enable_Siloed_Ref_Set = FALSE;

// maximum number of clones for a call graph node
UINT32 IPA_Max_Node_Clones = 0;
BOOL   IPA_Max_Node_Clones_Set = FALSE;

// maximum % increase of the call graph size through node cloning
UINT32 IPA_Max_Clone_Bloat = DEFAULT_CLONE_BLOAT_FACTOR;

/* Max. "size" of each output file */
UINT32 IPA_Max_Output_File_Size = DEFAULT_OUTPUT_FILE_SIZE;

/* percentage change of the max. output file size */
INT32 IPA_Output_File_Size = 0;

/* This flag is to use the old type merge phase. It should be removed when the old type merge is removed. */
#if !defined(TARG_SL)
BOOL IPA_Enable_Old_Type_Merge = FALSE;  
#else
BOOL IPA_Enable_Old_Type_Merge = TRUE;  //jczhang: Not enabled in SL
#endif

/* enable devirtualization */
BOOL IPA_Enable_Devirtualization = FALSE;
BOOL IPA_Enable_Fast_Static_Analysis_VF = TRUE;
BOOL IPA_Enable_Original_VF = TRUE;
BOOL IPA_Enable_New_VF = TRUE;
BOOL IPA_Inline_Original_VF = TRUE;
BOOL IPA_Inline_New_VF = FALSE;
const char* IPA_Devirtualization_Input_File;

BOOL IPA_During_Original_VF = FALSE;
BOOL IPA_During_New_VF = FALSE;

/* assert whole program mode to enable more aggressive ipo */
BOOL IPA_Enable_Whole_Program_Mode = FALSE;
BOOL IPA_Enable_Whole_Program_Mode_Set = FALSE;

BOOL IPA_Enable_Scale = FALSE;

static OPTION_DESC Options_IPA[] = {
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "addressing",	"",
	  0, 0, 0,		&IPA_Enable_Addressing,	NULL,
	  "Enable address-taken analysis" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "readonly_parameter",	"",
	  0, 0, 0,		&IPA_Enable_Readonly_Ref, NULL,
	  "Enable identification of read-only reference parameters" }, 
    { OVK_BOOL, OV_VISIBLE,	FALSE, "aggr_cprop",		"",
	  0, 0, 0,		&IPA_Enable_Cprop2,	NULL,
	  "Enable aggressive constant propagation" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "alias",	"",
	  0, 0, 0,		&IPA_Enable_Simple_Alias,NULL,
	  "Enable variable mod, use, and alias analysis" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "array_section",	"",
	  0, 0, 0,		&IPA_Enable_Array_Sections, NULL,
	  "Enable interprocedural array section analysis " },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "array_summary",	"",
	  0, 0, 0,		&IPA_Enable_Array_Summary, NULL,
	  "Enable local array section summary" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "scalar_kill",	"",
	  0, 0, 0,		&IPA_Enable_Scalar_Kill, NULL,
	  "Enable scalar kill analysis" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "scalar_euse",	"",
	  0, 0, 0,		&IPA_Enable_Scalar_Euse, NULL,
	  "Enable scalar euse analysis" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "common_cprop",	"",
	  0, 0, 0,		&IPA_Enable_Common_Const, NULL,
	  "Enable common constant propagation" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "autognum",	"",
	  0, 0, 0,		&IPA_Enable_AutoGnum,	NULL,
	  "Enable automatic gp-relative data allocation" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "barrier_aliasfarg",	"",
	  0, 0, 0,		&IPA_Enable_BarrierFarg,	NULL,
/*
	  "Enable barrier generation for inlined Fortran aliased actual arg." ,
	  "Enable barrier gen. for inlined aliased Fortran actual" ,
*/
	  "Enable barrier gen. " },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "cgi",		"",
	  0, 0, 0,		&IPA_Enable_CGI,	NULL,
	  "Enable constant global variable identification" },
    { OVK_BOOL,	OV_SHY,		FALSE, "compile",	"",
	  0, 0, 0,		&IPA_Enable_ipacom,	NULL,
	  "Enable final back-end compilation" }, 
    { OVK_BOOL, OV_SHY,		FALSE, "link",		"",
	  0, 0, 0,		&IPA_Enable_final_link,	NULL,
	  "Enable final link step" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "copy_prop",	"",
	  0, 0, 0,		&IPA_Enable_Copy_Prop,	NULL,
	  "Enable interprocedural copy propagation" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "cprop",	"",
	  0, 0, 0,		&IPA_Enable_Cprop,	NULL,
	  "Enable constant propagation" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "assert", "",
	  0, 0, 0,		&IPA_Enable_Assert,	NULL,
          "Enable assertion for constant propagation" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "clone",	"",
	  0, 0, 0,		&IPA_Enable_Cloning,	NULL,
	  "Enable subprogram cloning" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "partial_inl",	"",
	  0, 0, 0,		&IPA_Enable_Partial_Inline,	NULL,
	  "Enable partial inlining" },
    { OVK_UINT32, OV_INTERNAL,	FALSE, "multi_clone",   "",
           0, 0, UINT32_MAX, &IPA_Max_Node_Clones, &IPA_Max_Node_Clones_Set,
 	  "Maximum clones per call graph node" },
    { OVK_UINT32, OV_INTERNAL,	FALSE, "node_bloat",   "",
          DEFAULT_CLONE_BLOAT_FACTOR, 0, UINT32_MAX, &IPA_Max_Clone_Bloat,NULL,
 	  "Maximum call graph bloat with cloning" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "dce",	        "",
	  0, 0, 0,		&IPA_Enable_DCE,	NULL,
	  "Enable dead code elimination" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "dve",		"",
	  0, 0, 0,		&IPA_Enable_DVE,	NULL,
	  "Enable dead variable elimination" },
    { OVK_UINT32,OV_VISIBLE,	FALSE, "depth",	"",
	  UINT32_MAX, 0, UINT32_MAX,&IPA_Max_Depth,	NULL,
	  "Limit inlining depth" },
    { OVK_UINT32,OV_VISIBLE,	FALSE, "maxdepth",	"",
	  UINT32_MAX, 0, UINT32_MAX,&IPA_Max_Depth,	NULL,
	  "Limit inlining depth" },
    { OVK_UINT32,OV_VISIBLE,	FALSE, "forcedepth",	"",
	  UINT32_MAX, 0, UINT32_MAX, &IPA_Force_Depth,	&IPA_Force_Depth_Set,
	  "Inline to at least this depth" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "dfe",		"",
	  0, 0, 0,		&IPA_Enable_DFE,	&IPA_Enable_DFE_Set,
	  "Enable dead function elimination" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "dst",		"",
	  0, 0, 0,		&IPA_Enable_DST,	NULL},
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "echo_commands", "",
	  0, 0, 0,		&IPA_Echo_Commands,	NULL,
	  "Echo back end compilation commands" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "exc",		"",
	  0, 0, 0,		&IPA_Enable_Exc,	NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "flow_analysis", "",
	  0, 0, 0,		&IPA_Enable_Flow_Analysis,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "gp_partition",	"",
	  0, 0, 0,		&IPA_Enable_GP_Partition,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "graph",	"",
	  0, 0, 0,		&IPA_Enable_daVinci,	NULL,
	  "Generate call-graph display" },
    { OVK_UINT32,OV_VISIBLE,	FALSE, "Gspace",	"",
	  DEFAULT_GSPACE, 0, DEFAULT_GSPACE, &IPA_Gspace, NULL},
    { OVK_UINT32,OV_VISIBLE,	FALSE, "Gnum",	"",
	  DEFAULT_GSPACE, 0, DEFAULT_GSPACE, &IPA_user_gnum, NULL,
	  "Specific size limit for data in gp-relative space" },
    { OVK_UINT32,OV_INTERNAL,	FALSE, "Gfactor",   "",
          DEFAULT_EXTGOT_FACTOR, 0, UINT32_MAX, &IPA_Extgot_Factor, NULL,
	  "Percentage used to multiply the number of External GOTs, for AutoGnum purpose" },
    { OVK_UINT32,OV_VISIBLE,	FALSE, "hard_plimit",	"",
	  DEFAULT_HARD_LIMIT, 0, UINT32_MAX,
	  &IPA_PU_Hard_Limit, &IPA_PU_Hard_Limit_Set }, 
    { OVK_BOOL,	OV_SHY,		FALSE, "ignore_lang",	"",
	  0, 0, 0,		&IPA_Enable_Lang,	NULL},
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "inline",	"",
	  0, 0, 0,		&IPA_Enable_Inline,	NULL,
	  "Enable subprogram inlining" },
    { OVK_NAME,	OV_INTERNAL,	FALSE, "ipaa_summary_file",	"",
	  0, 0, 0,		&Ipa_File_Name,		NULL,
	  "File name for IP alias analysis summary" },
    { OVK_UINT32,OV_INTERNAL,	FALSE, "intrinsics",   "intr",
          DEFAULT_NUM_FORTRAN_INTR, 0, UINT32_MAX,
          &IPA_Num_Fortran_Intrinsics, NULL,
 	  "Number of FORTRAN intrinsics used" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "keeplight",	"",
	  0, 0, 0,		&IPA_Enable_Keeplight,	NULL},
    { OVK_UINT32,OV_INTERNAL,	FALSE, "map_limit",   "",
          DEFAULT_MAP_LIMIT, 0, UINT32_MAX, &IPA_Map_Limit, NULL},
    { OVK_UINT32,OV_VISIBLE,	FALSE, "max_jobs",	"",
	  1, 0, UINT32_MAX,	&IPA_Max_Jobs,		&IPA_Max_Jobs_Set,
	  "Maximum number of concurrent back-end jobs" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "merge_ty",	"",
	  0, 0, 0,		&IPA_Enable_Merge_ty,	NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "use_effective_size", "",
	  0, 0, 0,		&IPA_Use_Effective_Size, NULL},
    { OVK_OBSOLETE,OV_INTERNAL,	FALSE, "min_freq",	"",
	  DEFAULT_MIN_FREQ, 0, 10000, &IPA_Min_Freq,	NULL},
    { OVK_UINT32,OV_SHY,	FALSE, "min_hotness",	"",
	  DEFAULT_MIN_HOTNESS, 0, UINT32_MAX, &IPA_Min_Hotness,	NULL},
    { OVK_UINT32,OV_SHY,	FALSE, "max_density",	"",
	  DEFAULT_MAX_DENSITY, 0, UINT32_MAX, &IPA_Max_Density,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "reshape",	"",
	  0, 0, 0,		&IPA_Enable_Reshape,	NULL,
	  "Reshape analysis for IPA" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "opt_alias",	"",
	  0, 0, 0,		&IPA_Enable_Opt_Alias,	NULL,
	  "Use IPA alias information in WOPT" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE,	"pad",	        "",
	  0, 0, 0,		&IPA_Enable_Padding,	NULL,
          "Enable padding of common block arrays"},
    { OVK_UINT32, OV_INTERNAL,	FALSE, "common_pad_size",       "",
          0, 1, 1000,           &IPA_Common_Pad_Size,   NULL,
 	  "Amount by which to pad common block array dimensions" },
    { OVK_LIST,	OV_SHY,		FALSE, "partition_group",	"",
	  0, 0, 0,		&IPA_Group_Names,	NULL },
    { OVK_LIST,	OV_VISIBLE,	FALSE, "specfile",	"spec",
	  0, 0, 0,		&IPA_Spec_Files,	NULL,
	  "Identify IPA specification filename" },
    { OVK_BOOL,	OV_SHY,		FALSE, "sp_partition",	"",
	  0, 0, 0,		&IPA_Enable_SP_Partition,	NULL},
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "picopt",	"",
	  0, 0, 0,		&IPA_Enable_Picopt,	NULL,
	  "Enable shared code optimizations" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "preempt",	"",
	  0, 0, 0,		&IPA_Enable_Preempt,	NULL},
    { OVK_UINT32, OV_VISIBLE,	FALSE, "plimit",	"",
	  DEFAULT_PU_LIMIT, 0, UINT32_MAX, &IPA_PU_Limit, &IPA_PU_Limit_Set},
    { OVK_UINT32, OV_SHY,	FALSE, "callee_limit", "",
	  DEFAULT_SMALL_CALLEE, 0, UINT32_MAX, &IPA_Small_Callee_Limit, NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "recycle",	"",
	  0, 0, 0,		&IPA_Enable_Recycle,	NULL},
    { OVK_BOOL,	OV_SHY,		FALSE, "relopt",	"",
	  0, 0, 0,		&IPA_Enable_Relocatable_Opt,	NULL},
    { OVK_OBSOLETE, OV_VISIBLE,	FALSE, "rela_freq",	"",
	  DEFAULT_RELA_FREQ, 0, 100, &IPA_Rela_Freq,	NULL},
    { OVK_LIST,	OV_VISIBLE,		FALSE, "skip_after",	"",
	  0, 0, 0,		&IPA_Skip,		NULL},
    { OVK_LIST,	OV_VISIBLE,	FALSE, "skip_before",	"",
	  0, 0, 0,		&IPA_Skip,		NULL},
    { OVK_LIST,	OV_VISIBLE,	FALSE, "skip_equal",	"",
	  0, 0, 0,		&IPA_Skip,		NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "skip_report",	"",
	  0, 0, 0,		&IPA_Skip_Report,	NULL,
	  "Report PU numbers for skip control" },
    { OVK_UINT32, OV_VISIBLE,	FALSE, "small_pu",	"",
	  DEFAULT_SMALL_PU, 1, UINT32_MAX, &IPA_PU_Minimum_Size, NULL },
    { OVK_UINT32, OV_VISIBLE,	FALSE, "space",	"",
	  DEFAULT_BLOAT_FACTOR, 0, UINT32_MAX, &IPA_Bloat_Factor,
	  &IPA_Bloat_Factor_Set}, 
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "split",	"",
	  0, 0, 0,		&IPA_Enable_Split_Common,	NULL,
	  "Enable IPA split common optimization" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "cord",		"",
	  0, 0, 0,		&IPA_Enable_Cord,	NULL,
	  "Enable procedure reordering" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "linear",		"",
	  0, 0, 0,		&IPA_Enable_Linearization,	NULL,
	  "Enable linearization of array subscripts" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "use_intrinsic",	"",
	  0, 0, 0,		&IPA_Use_Intrinsic,	NULL,
	  "Load Intrinsic Libraries" },
    { OVK_BOOL, OV_VISIBLE,     FALSE, "feedback",             "",
          0, 0, 0,              &IPA_Enable_Feedback,   NULL,
          "Create .pragma, .dfe, .dve  files, " },
    { OVK_BOOL, OV_VISIBLE,     FALSE, "class", "",
          0, 0, 0,              &IPA_Enable_Alias_Class, NULL,
         "Enable interprocedural alias classification" },
    { OVK_BOOL, OV_VISIBLE,     FALSE, "ac_temp", "",
          0, 0, 0,              &IPA_Debug_AC_Temp_Files, NULL,
         "Save files from before alias classification" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "nested",             "",
          0, 0, 0,              &IPA_Enable_Inline_Nested_PU,   NULL,
          "Enable inlining of nested PU " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "preopt",             "",
          0, 0, 0,              &IPA_Enable_Preopt, &IPA_Enable_Preopt_Set,
          "Enable calling the preopt" },
    // option -IPA:siloed=on requires -IPA:preopt=on and -OPT:alias=field_sensitive
    { OVK_BOOL, OV_INTERNAL,    FALSE, "siloed",             "",
          0, 0, 0,              &IPA_Enable_Siloed_Ref, &IPA_Enable_Siloed_Ref_Set,
          "Enable siloed reference analysis" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "struct",             "",
          0, 0, 0,              &IPA_Enable_Inline_Struct,   NULL,
          "Enable inlining of PU with F90 structures " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "char",             "",
          0, 0, 0,              &IPA_Enable_Inline_Char_Array,   NULL,
          "Enable inlining of PU with char arrays " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "optional",             "",
          0, 0, 0,              &IPA_Enable_Inline_Optional_Arg,   NULL,
          "Enable inlining of PU with optional arguments " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "array_struct",             "",
          0, 0, 0,              &IPA_Enable_Inline_Struct_Array_Actual,   NULL,
          "Enable inlining of PU with F90 structures when actuals are of ARRAY type " },
    { OVK_INT32, OV_INTERNAL,	FALSE, "output_file_size", "",
	  0, -100, INT32_MAX,	&IPA_Output_File_Size, NULL},
    { OVK_BOOL, OV_INTERNAL,    FALSE, "var_dim_array",             "",
          0, 0, 0,              &IPA_Enable_Inline_Var_Dim_Array,   NULL,
          "Enable inlining of PU with param that is variable-dim array " },
    { OVK_NAME,	OV_INTERNAL,	FALSE, "propagate_feedback_file",	"",
	  0, 0, 0,		&Feedback_Filename,		NULL,
	  "Feedback file name which IPA will propagate to next phase" },
    { OVK_NAME,	OV_INTERNAL,	FALSE, "propagate_annotation_file",	"",
	  0, 0, 0,		&Annotation_Filename,		NULL,
	  "Annotation file name which IPA will propagate to next phase" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "field_reorder",	"",
	  0, 0, 0,		&IPA_Enable_Reorder,	NULL,
	  "Enable field reordering"},
#ifdef KEY
    { OVK_BOOL, OV_INTERNAL,	FALSE, "icall_opt",	"",
	  0, 0, 0,		&IPA_Enable_Icall_Opt,	NULL,
	  "Enable conversion of icall to call"},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "eh_opt",	"",
	  0, 0, 0,		&IPA_Enable_EH_Region_Removal,	NULL,
	  "Enable removal of exception regions"},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "branch",	"",
	  0, 0, 0,		&IPA_Enable_Branch_Heuristic,	NULL,
	  "Enable use of branch probabilities for inlining"},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "check_opt",	"",
	  0, 0, 0,		&IPA_Check_Options,	NULL,
	  "Enable handling of any inconsistent optimization options"},
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "clone_list",	"",
	  0, 0, 0,	&IPA_Clone_List_Actions,	NULL,
	  "Report function cloner actions" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "pure_call",	"",
	  0, 0, 0,	&IPA_Enable_Pure_Call_Opt,	NULL,
	  "Optimize calls to functions without side-effects" },
    { OVK_INT32, OV_INTERNAL,	FALSE, "call_op_skip_b",	"",
	  0, 0, INT32_MAX,	&IPA_Pure_Call_skip_before,	NULL,
	  "For debugging" },
    { OVK_INT32, OV_INTERNAL,	FALSE, "pu_reorder",	"",
	  REORDER_DISABLE, REORDER_DISABLE, REORDER_BY_EDGE_FREQ,
	  &IPA_Enable_PU_Reorder, &IPA_Enable_PU_Reorder_Set,
	  "Enable procedure reordering" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "ctype",	"",
	  0, 0, 0,		&IPA_Enable_Ctype,	NULL,
	  "Enable insertion of ctype.h array"},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "icallopt_needs_inline",  "",
	  0, 0, 0,		&IPA_Consult_Inliner_For_Icall_Opt, NULL,
	  "Check inlining heuristics during icall opt"},
    { OVK_UINT32, OV_INTERNAL,	FALSE, "icall_min_freq",	"",
	  DEFAULT_ICALL_MIN_FREQ, 1, UINT32_MAX, &IPA_Icall_Min_Freq, NULL,
	  "Min freq of icall for icall optimization"},
    { OVK_UINT32, OV_INTERNAL,	FALSE, "icall_min_rate",	"",
	  DEFAULT_ICALL_MIN_FREQ, 1, UINT32_MAX, &IPA_Icall_Target_Min_Rate, NULL,
	  "Min icall target call ratio for icall optimization"},
    { OVK_BOOL, OV_INTERNAL,    FALSE, "source_pu_order",  "",
      0, 0, 0,              &IPA_Enable_Source_PU_Order, NULL,
      "Maintain source-code PU ordering in IPA output"},
    { OVK_BOOL, OV_INTERNAL,    FALSE, "ipa_enable_old_type_merge", "",
      0, 0, 0,              &IPA_Enable_Old_Type_Merge, NULL,
      "Use the old type merge phase in IPA"},
    { OVK_BOOL, OV_INTERNAL,    TRUE, "devirtual_CHA", "",
      0, 0, 0,              &IPA_Enable_Fast_Static_Analysis_VF, NULL,
      "Use devirtualization phase"},
    { OVK_BOOL, OV_VISIBLE,    FALSE, "devirtual_ORIG", "",
      0, 0, 0,              &IPA_Enable_Original_VF, NULL,
      "Use devirtualization phase"},
    { OVK_BOOL, OV_VISIBLE,    FALSE, "devirtual_NEW", "",
      0, 0, 0,              &IPA_Enable_New_VF, NULL,
      "Use devirtualization phase"},
    { OVK_BOOL, OV_VISIBLE,    FALSE, "devirtual_inline", "",
      0, 0, 0,              &IPA_Inline_Original_VF, NULL,
      "Enable devirtualization inlining"},
    { OVK_BOOL, OV_VISIBLE,    FALSE, "aggressive_devirtual_inline", "",
      0, 0, 0,              &IPA_Inline_New_VF, NULL,
      "Enable aggressive devirtualization inlining"},
    { OVK_NAME, OV_VISIBLE,    FALSE, "dv_input", "",
      0, 0, 0,              &IPA_Devirtualization_Input_File, NULL,
      "Use devirtualization phase"},
    { OVK_BOOL, OV_VISIBLE,     FALSE, "whole_program_mode", "",
      0, 0, 0,              &IPA_Enable_Whole_Program_Mode,
                            &IPA_Enable_Whole_Program_Mode_Set,
      "Assert whole program mode"},
    { OVK_BOOL, OV_VISIBLE,     FALSE, "scale", "",
      0, 0, 0,              &IPA_Enable_Scale, NULL,
      "Enable multi-core scalability optimizations"},

#ifdef TARG_X8664
    { OVK_UINT32, OV_INTERNAL,	FALSE, "optimize_struct",	"",
	  1, 0, UINT32_MAX, &IPA_Enable_Struct_Opt, NULL,
#else
    { OVK_UINT32, OV_INTERNAL,	FALSE, "optimize_struct",	"",
	  0, 0, UINT32_MAX, &IPA_Enable_Struct_Opt, NULL,
#endif
	  "Enable IPA struct optimizations"},

#ifdef TARG_X8664
      { OVK_UINT32, OV_INTERNAL,	FALSE, "global_as_local",	"",
	1, 0, UINT32_MAX, &IPA_Enable_Global_As_Local, NULL,
	"Enable global-as-local optimizations"},
#else
      {OVK_UINT32, OV_INTERNAL,	FALSE, "global_as_local",	"",
       0, 0, UINT32_MAX, &IPA_Enable_Global_As_Local, NULL,
       "Enable global-as-local optimizations"},
#endif      
    /* The following option is temporary, and should be removed soon */
    { OVK_UINT32, OV_INTERNAL,	FALSE, "update_struct",	"",
	  0, 0, UINT32_MAX, &IPA_Update_Struct, NULL,
	  "Struct update"},
#endif // KEY
    { OVK_COUNT }	    /* List terminator -- must be last */
};

/* ====================================================================
 * List of global variables that are set by the -INLINE option group.
 * ====================================================================
 */

/* What is the default inlining behavior? */
BOOL	INLINE_Enable = TRUE;	/* If FALSE, disable inliner? */
BOOL	INLINE_All = FALSE;	/* Inline everything possible? */
BOOL    INLINE_Optimize_Alloca = TRUE; /* when inlining calls with alloca fix the stack and pop */
BOOL	INLINE_Enable_Copy_Prop = TRUE; /* Copy Propogation during stand-alone inlining? */
BOOL	INLINE_Enable_Subst_Copy_Prop = FALSE; /* Aggressive substitution of actual for formal and hence copy propogation during stand-alone inlining */
BOOL    INLINE_F90 = TRUE;  /* Enable recognition of F90 in parameter type compatibility */
BOOL	INLINE_None = FALSE;	/* Inline nothing? */
BOOL	INLINE_Exceptions = TRUE;	/* Inline exception code? */
BOOL	INLINE_Keep_PU_Order = FALSE;	/* Retain input PU order? */
BOOL	INLINE_List_Actions = FALSE;	/* List inline actions? */
UINT32	INLINE_Max_Pu_Size = DEFAULT_INLINE_Max_Pu_Size;
                                        /* Max size of pu : default 5000 */
BOOL	INLINE_Preemptible = FALSE;	/* Inline preemptible PUs? */
BOOL	INLINE_Static = FALSE;	        /* Inline static fns? */
BOOL    INLINE_Static_Set = FALSE;	/* ... explicitly set? */
BOOL	INLINE_Aggressive = FALSE; /* inline even non-leaf, out-of-loop calls */
BOOL    INLINE_First_Inline_Calls_In_Loops = TRUE;  /* inline calls in loops more proactively */
BOOL    INLINE_Enable_Split_Common = TRUE;  /* Enable split common: inliner */
BOOL    INLINE_Enable_Auto_Inlining = TRUE; /* Enable automatic inlining analysis */
BOOL	INLINE_Enable_Restrict_Pointers = FALSE; // Allow restrict pointers
					// as formal parameter
#ifdef KEY
BOOL	INLINE_Recursive = TRUE;	// Do recursive inlining
BOOL	INLINE_Param_Mismatch = TRUE;	// Inline even if # of actuals < # of formals
BOOL	INLINE_Type_Mismatch = FALSE;   // Inline even if actuals' type != formals' type
// check parameter compatibility during inlining
CHECK_PARAM_COMPATIBILITY INLINE_Check_Compatibility = RELAXED_CHECK;
BOOL    INLINE_Ignore_Bloat = TRUE;    // Ignore code bloat (-IPA:space)
UINT32   INLINE_Callee_Limit = 0;      // Callee size limit for functions
                                       // marked inline by user
#endif

OPTION_LIST *INLINE_List_Names = NULL;	/* Must/never/file options */
OPTION_LIST *INLINE_Spec_Files = NULL;	/* Specification files */
OPTION_LIST *IPA_Group_Names = NULL;	/* groupings, e.g. partition groupings */
OPTION_LIST *IPA_Spec_Files = NULL;	/* Specification files for IPA options, particularly used for partition groupings */
UINT32	INLINE_Skip_After = UINT32_MAX;
UINT32	INLINE_Skip_Before = 0;
BOOL    INLINE_Array_Bounds = FALSE;    /* "conforming" array bounds */
BOOL    INLINE_Use_Malloc_Mempool = FALSE;    /* Use the malloc mempool for */
BOOL    INLINE_Free_Malloc_Mempool = FALSE;   /* Use the malloc mempool for
						 cloning tree */
BOOL    INLINE_Inlined_Pu_Call_Graph = FALSE; /* impl. 2 of lightweight inliner -- build a call graph with only the PU tagged inline */
BOOL    INLINE_Inlined_Pu_Call_Graph2 = FALSE; /* impl. 3 of lightweight inliner -- build a call graph with only the PU tagged inline and its callers */
BOOL    INLINE_Get_Time_Info = FALSE; 	       /* Generate timing info for different phases of the inliner */

char    *INLINE_Script_Name = NULL;
BOOL   INLINE_Enable_Script = FALSE;;
	
static OPTION_DESC Options_INLINE[] = {
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "",	NULL,
	  0, 0, 0,	&INLINE_Enable,	NULL,
	  "Enable subprogram inlining" },
    { OVK_BOOL, OV_SHY,		FALSE, "aggressive",	"",
	  0, 0, 0,	&INLINE_Aggressive,	NULL },
    { OVK_BOOL, OV_SHY,		FALSE, "bias_calls_in_loops",	"",
	  0, 0, 0,	&INLINE_First_Inline_Calls_In_Loops,	NULL },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "all",	"a",
	  0, 0, 0,	&INLINE_All,	NULL,
	  "Attempt to inline all subprograms" },
    { OVK_BOOL,	OV_SHY,		FALSE, "alloca",	"alloca",
	  0, 0, 0,	&INLINE_Optimize_Alloca,	NULL,
	  "Enable save/restore of stack when inlining calls with alloca" },
    {OVK_BOOL, OV_VISIBLE,	FALSE, "copy_prop_inline",	"copy", 
	  0, 0, 0,      &INLINE_Enable_Copy_Prop, NULL,
	  "Enable inliner copy propagation" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "dfe",	"df",
	  0, 0, 0,	&IPA_Enable_DFE,	&IPA_Enable_DFE_Set,
	  "Enable dead function elimination" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "exceptions",	"exc",
	  0, 0, 0,	&INLINE_Exceptions,	NULL },
    { OVK_LIST,	OV_VISIBLE,	FALSE, "file",	"f",
	  0, 0, 0,	&INLINE_List_Names,	NULL,
	  "Identify files where inliner should search for subprograms" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "keep_pu_order",	"keep_pu",
	  0, 0, 0,	&INLINE_Keep_PU_Order,	NULL,
	  "Preserve source subprogram ordering" },
    { OVK_LIST, OV_VISIBLE,     FALSE, "library", "lib",
          0, 0, 0,      &INLINE_List_Names,     NULL,
         "Identify archive libraries where inliner should search for subprograms" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "list",	"l",
	  0, 0, 0,	&INLINE_List_Actions,	NULL,
	  "Report inliner actions" },
    { OVK_UINT32, OV_VISIBLE,	FALSE, "max_pu_size_inline", "max_pu_size",
	  DEFAULT_INLINE_Max_Pu_Size, 0, UINT32_MAX,
	  &INLINE_Max_Pu_Size, NULL,
	  "Limit size of inlined subprograms" },
    { OVK_LIST,	OV_VISIBLE,	FALSE, "must",	"m",
	  0, 0, 0,	&INLINE_List_Names,	NULL,
	  "Identify subprograms to be inlined" },
    { OVK_LIST,	OV_VISIBLE,	FALSE, "never",	"ne",
	  0, 0, 0,	&INLINE_List_Names,	NULL,
	  "Identify subprograms not to inline" },
    { OVK_BOOL,	OV_VISIBLE,	FALSE, "none",	"no",
	  0, 0, 0,	&INLINE_None,	NULL,
	  "Disable default inlining" },
    { OVK_BOOL,	OV_SHY,		FALSE, "preemptible",	"preempt",
	  0, 0, 0,	&INLINE_Preemptible,	NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "recycle_symbols",	"recycle",
	  0, 0, 0,	&IPA_Enable_Recycle,	NULL },
    { OVK_LIST,	OV_VISIBLE,	FALSE, "specfile",	"sp",
	  0, 0, 0,	&INLINE_Spec_Files,	NULL,
	  "Identify IPA specification filename" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "split",	"",
	  0, 0, 0,		&INLINE_Enable_Split_Common,	NULL,
	  "Enable inliner split common optimization" },
    { OVK_BOOL,	OV_SHY,		FALSE, "static",	"",
	  0, 0, 0,		&INLINE_Static,	&INLINE_Static_Set,
	  "Enable inlining of static functions" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "subst_copy_prop_inline",	"subst", 
	  0, 0, 0,      &INLINE_Enable_Subst_Copy_Prop, NULL,
	  "Enable inliner copy propagation" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "f90param",	"", 
	 0, 0, 0,      &INLINE_F90, NULL,
	 "Enable parameter type checking for F90" },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "auto",	"",
	  0, 0, 0,		&INLINE_Enable_Auto_Inlining,	NULL,
	  "Enable inliner automatic inline analysis" },
    { OVK_BOOL, OV_INTERNAL,	FALSE,	"restrict",	"",
	  0, 0, 0,		&INLINE_Enable_Restrict_Pointers, NULL,
	  "Allow inlining of PUs with restrict pointer as formal parameters" },
#ifdef KEY
    { OVK_BOOL, OV_INTERNAL,	FALSE,	"recurse",	"",
	  0, 0, 0,		&INLINE_Recursive, NULL,
	  "Allow recursive inlining of PUs" },
    { OVK_BOOL, OV_INTERNAL,	FALSE,	"num_mismatch",	"",
	  0, 0, 0,		&INLINE_Param_Mismatch, NULL,
	  "Allow inlining even if # of parameters does not match between call and callee" },
    { OVK_BOOL, OV_INTERNAL,	FALSE,	"type_mismatch",	"",
	  0, 0, 0,		&INLINE_Type_Mismatch, NULL,
	  "Allow inlining even if actuals don't exactly match formals" },
    { OVK_INT32, OV_INTERNAL,	FALSE, "check_types",	"",
	  RELAXED_CHECK, STRICT_CHECK, AGGRESSIVE,
	  &INLINE_Check_Compatibility, NULL, "Check parameter type compatibility during inlining"},
    { OVK_BOOL, OV_INTERNAL,	FALSE,	"ignore_bloat",	"",
	  0, 0, 0,		&INLINE_Ignore_Bloat, NULL,
	  "Ignore code bloat controlled by -IPA:space" },
    { OVK_UINT32, OV_INTERNAL,	FALSE, "callee_size",	"",
	  0, 0, UINT32_MAX, &INLINE_Callee_Limit, NULL,
	  "Callee size limit for functions marked inline by user"},
#endif
    { OVK_LIST,	OV_VISIBLE,	FALSE, "skip",	"s",
	  0, 0, 0,	&INLINE_List_Names,	NULL,
	  "Skip requested CG edges to avoid doing inlining" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "nested",             "",
          0, 0, 0,              &IPA_Enable_Inline_Nested_PU,   NULL,
          "Enable inlining of nested PU " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "struct",             "",
          0, 0, 0,              &IPA_Enable_Inline_Struct,   NULL,
          "Enable inlining of PU with F90 structures " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "char",             "",
          0, 0, 0,              &IPA_Enable_Inline_Char_Array,   NULL,
          "Enable inlining of PU with char arrays " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "optional",             "opt",
          0, 0, 0,              &IPA_Enable_Inline_Optional_Arg,   NULL,
          "Enable inlining of PU with optional arguments " },
    { OVK_LIST,	OV_VISIBLE,	FALSE, "edge",	"e",
	  0, 0, 0,	&INLINE_List_Names,	NULL,
	  "Inline requested CG edges ONLY" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "array_struct",             "",
          0, 0, 0,              &IPA_Enable_Inline_Struct_Array_Actual,   NULL,
          "Enable inlining of PU with F90 structures when actuals are of ARRAY type" },
    { OVK_UINT32, OV_VISIBLE,	FALSE, "skip_after", "",
	  UINT32_MAX, 0, UINT32_MAX, &INLINE_Skip_After, NULL,
	  "Edge number to skip if larger than the specified" },
    { OVK_UINT32, OV_VISIBLE,	FALSE, "skip_before", "",
	  0, 0, UINT32_MAX, &INLINE_Skip_Before, NULL,
	  "Edge number to skip if smaller than the specified" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "var_dim_array",             "",
          0, 0, 0,              &IPA_Enable_Inline_Var_Dim_Array,   NULL,
          "Enable inlining of PU with param that is variable-dim array " },
    { OVK_LIST,	OV_VISIBLE,	FALSE, "in_edge",	"",
	  0, 0, 0,	&INLINE_List_Names,	NULL,
	  "Inline requested CG edges " },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "array_bounds",             "",
          0, 0, 0,              &INLINE_Array_Bounds,   NULL,
          "Is it safe to inline a PU with an array parameter whose outermost dimension size is unknown" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "malloc",             "",
          0, 0, 0,              &INLINE_Use_Malloc_Mempool,   NULL,
          "Use malloc mempool instead of private mempool to clone trees" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "free",             "",
          0, 0, 0,              &INLINE_Free_Malloc_Mempool,   NULL,
          "Free memory malloc'ed by the malloc mempool used in clone trees" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "inlined_pu",             "",
          0, 0, 0,              &INLINE_Inlined_Pu_Call_Graph,   NULL,
          "Lightweight inliner impl 2 -- build a call graph with only PU tagged inline" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "pu_need_inline",          "",
          0, 0, 0,              &INLINE_Inlined_Pu_Call_Graph2,   NULL,
          "Lightweight inliner impl 2 -- build a call graph with only PU tagged inline and callers that call these PUs" },
    { OVK_BOOL, OV_INTERNAL,    FALSE, "time",          "",
          0, 0, 0,              &INLINE_Get_Time_Info,   NULL,
          "Generate timing info for different phase of the inliner" },
    { OVK_NAME,	OV_VISIBLE,	TRUE, "inline_script", "", 
          0, 0, 0,	&INLINE_Script_Name, &INLINE_Enable_Script, 
          "Enable call-site specific inlining based on inline description file" },
    { OVK_COUNT }	    /* List terminator -- must be last */
};
