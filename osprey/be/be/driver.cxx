/* 
 * Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.
 */

/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: driver.cxx
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/be/SCCS/s.driver.cxx $
 *
 * Revision history:
 *  13-Feb-95 - Original Version
 *
 * Description:
 *  Main driver for the entire backend.
 *
 * ====================================================================
 * ====================================================================
 */

#include <sys/types.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>		    /* for wn.h */
#endif /* ! defined(BUILD_OS_DARWIN) */
#include <cmplrs/rcodes.h>
#include <dirent.h>
#ifndef __MINGW32__
#include <libgen.h>
#endif

#include "defs.h"
#if !defined(SHARED_BUILD)
#define load_so(a,b,c)
#else
#include "dso.h"		    /* for load_so() */
#endif
#include "errors.h"		    /* Set_Error_Tables(), etc. */
#include "err_host.tab"		    /* load all the error messages */
#include "erglob.h"		    /* for EC_ errmsg */
#include "erauxdesc.h"		    /* for BE error messages */
#include "mempool.h"		    /* for MEM_Initialze()  */
#include "phase.h"		    /* for PHASE_CG */
#include "be_util.h"                /* for Reset_Current_PU_Count(), etc */
#include "wn.h"			    /* for WN */
#include "driver_util.h"	    /* for Process_Command_Line() */
#include "timing.h"		    /* for Reset_Timer(), etc. */
#include "glob.h"		    /* for Show_Progress */
#include "stab.h"		    /* for ir_bread.h */
#include "pu_info.h"		    /* for PU_Info */
#include "ir_bread.h"		    /* for Read_Global_Info() */
#include "ir_bwrite.h"		    /* for Write_Global_Info(), etc. */
#include "config.h"		    /* for LNO_Path, etc. */
#include "config_opt.h"             /* for Instrumentation_Enabled */
#include "config_list.h"	    /* for List_Enabled, etc. */
#include "config_lno.h"		    /* for LNO_Run_Lego, etc. */
#include "config_cache.h"           /* for Mhd and Mhd_Options */
#include "file_util.h"		    /* for New_Extension () */
#include "xstats.h"		    /* for Print_Stats () */
#include "data_layout.h"	    /* for Initialize_Stack_Frame() */
#include "opt_alias_interface.h"    /* for ALIAS_MANAGER stuff */
#include "wn_lower.h"		    /* for WN_Lower() */
#include "cgdriver.h"		    /* for CG_Init, etc. */
#include "optimizer.h"		    /* for alias manager, etc. */
#include "ori.h"		    /* for Olimit_Region_Insertion */
#include "wodriver.h"		    /* for wopt_main, etc. */
#ifndef BUILD_SKIP_LNO
#include "lnodriver.h"		    /* for lno_main, etc. */
#include "ipl_driver.h"		    /* for ipl_main. etc. */
#endif
#ifndef BUILD_SKIP_WHIRL2C
#include "w2c_driver.h"		    /* for W2C_Process_Command_Line, etc. */
#endif
#ifndef BUILD_SKIP_WHIRL2F
#include "w2f_driver.h"		    /* for W2F_Process_Command_Line, etc. */
#endif
#include "region_util.h"	    /* for Regions_Around_Inner_Loops */
#include "region_main.h"	    /* for REGION_* driver specific routines */
#include "cg/cg.h"	            /* for CG PU-level routines */
#include "tracing.h"                /* For the trace routines */
#include "ir_reader.h"              /* For fdump_tree */
#include "dwarf_DST.h"	    	    /* for Orig_PU_Name */
#include "fb_whirl.h"		    /* for FEEDBACK */
#include "eh_region.h"		    /* for EH_Generate_Range_List, etc. */
#include "vho_lower.h"
#include "iter.h"		    /* PU iterator for loops */
#include "dra_export.h"             /* for DRA routines */
#include "ti_init.h"		    /* for targ_info */
#include "opt_alias_interface.h"    /* for Create_Alias_Manager */
#include "alias_analyzer.h"         /* for AliasAnalyzer */
#include "omp_lower.h"              /* for OMP pre-lowering interface */
#include "cxx_memory.h"		    /* CXX_NEW */
#include "options_stack.h"	    /* for Options_Stack */
#include "be_symtab.h"		    /* for Be_preg_tab */
#include "wb_omp.h"		    /* whirl browser for omp prelowerer */
#include "wb_lwr.h"		    /* whirl browser for lowerer */ 
#include "wn_instrument.h"          /* whirl instrumenter */
#include "mem_ctr.h"
#ifndef ipl_reorder_INCLUDED // for Preprocess_struct_access()
#include "ipl_reorder.h"
#endif
#include "config_ipa.h"    /*for IPA_Enable_Reorder*/
#ifdef KEY
#include "config_wopt.h"    // for WOPT_Enable_Simple_If_Conv
#include "config_vho.h"     // for VHO_Struct_Opt
#include "output_func_start_profiler.h"
#include "goto_conv.h"
#endif
#include "be_memop_annot.h"
#if defined(TARG_SL)  || defined(TARG_MIPS)
#include <dlfcn.h>
#include "topcode.h"
#include "ti_si.h"
#include "isr.h"
#endif
#include "wn_mp.h"    // for Verify_No_MP()
#include "comp_decl.h"

extern ERROR_DESC EDESC_BE[], EDESC_CG[];

#ifdef KEY
#include "demangle.h"
extern "C" char *cplus_demangle (const char *, int);
extern void Recompute_addr_saved_stmt (WN *);
extern void Set_addr_saved_stmt (WN *, BOOL);
extern void CYG_Instrument_Driver(WN *);
#endif

extern void Initialize_Targ_Info(void);

// symbols defined in cg.so
#if defined(SHARED_BUILD)
#if defined(__linux__) || defined(BUILD_OS_DARWIN) || defined(__CYGWIN__) || defined(__MINGW32__)
#if !defined(BUILD_FAST_BIN)
extern void (*CG_Process_Command_Line_p) (INT, char **, INT, char **);
#define CG_Process_Command_Line (*CG_Process_Command_Line_p)

extern void (*CG_Init_p) ();
#define CG_Init (*CG_Init_p)

extern void (*CG_Fini_p) ();
#define CG_Fini (*CG_Fini_p)

extern void (*CG_PU_Initialize_p) (WN*);
#define CG_PU_Initialize (*CG_PU_Initialize_p)

extern void (*CG_PU_Finalize_p) ();
#define CG_PU_Finalize (*CG_PU_Finalize_p)

extern WN* (*CG_Generate_Code_p) (WN*, ALIAS_MANAGER*, DST_IDX, BOOL);
#define CG_Generate_Code (*CG_Generate_Code_p)

extern void (*EH_Generate_Range_List_p) (WN *);
#define EH_Generate_Range_List (*EH_Generate_Range_List_p)

// used to dump information of EH related INITO
extern void (*EH_Dump_INITOs_p) (WN *, FILE *);
#define EH_Dump_INITOs (*EH_Dump_INITOs_p)
#else
extern void CG_Process_Command_Line (INT, char **, INT, char **);
extern void CG_Init ();
extern void CG_Fini ();
extern void CG_PU_Initialize (WN*);
extern void CG_PU_Finalize ();
extern WN* CG_Generate_Code (WN*, ALIAS_MANAGER*, DST_IDX, BOOL);
extern void EH_Generate_Range_List (WN *);
#endif //BUILD_FAST_BIN

#else

#pragma weak CG_Process_Command_Line
#pragma weak CG_Init
#pragma weak CG_Fini
#pragma weak CG_PU_Finalize
#pragma weak CG_PU_Initialize
#pragma weak CG_Generate_Code
#pragma weak EH_Generate_Range_List
#pragma weak EH_Dump_INITOs

#endif // __linux__
#endif // SHARED_BUILD

// symbols defined in wopt.so
#if defined(SHARED_BUILD)
#if defined(__linux__) || defined(BUILD_OS_DARWIN)|| defined(__CYGWIN__) || defined(__MINGW32__)

#if !defined(BUILD_FAST_BIN)
extern void (*wopt_main_p) (INT argc, char **argv, INT, char **);
#define wopt_main (*wopt_main_p)

extern void (*Wopt_Init_p) ();
#define Wopt_Init (*Wopt_Init_p)

extern void (*Wopt_Fini_p) ();
#define Wopt_Fini (*Wopt_Fini_p)

extern WN* (*Perform_Preopt_Optimization_p) (WN *, WN *);
#define Perform_Preopt_Optimization (*Perform_Preopt_Optimization_p)

extern WN* (*Perform_Global_Optimization_p) (WN *, WN *, ALIAS_MANAGER *);
#define Perform_Global_Optimization (*Perform_Global_Optimization_p)

extern WN* (*Pre_Optimizer_p) (INT32, WN*, DU_MANAGER*, ALIAS_MANAGER*);
#define Pre_Optimizer (*Pre_Optimizer_p)

extern void (*choose_from_complete_struct_for_relayout_candidates_p)();
#define choose_from_complete_struct_for_relayout_candidates (*choose_from_complete_struct_for_relayout_candidates_p)

extern DU_MANAGER* (*Create_Du_Manager_p) (MEM_POOL *);
#define Create_Du_Manager (*Create_Du_Manager_p)

extern void (*Delete_Du_Manager_p) (DU_MANAGER *, MEM_POOL *);
#define Delete_Du_Manager (*Delete_Du_Manager_p)

extern BOOL (*Verify_alias_p) (ALIAS_MANAGER *, WN *);
#define Verify_alias (*Verify_alias_p)
#else
extern void wopt_main (INT argc, char **argv, INT, char **);
extern void Wopt_Init ();
extern void Wopt_Fini ();
extern WN* Perform_Preopt_Optimization (WN *, WN *);
extern WN* Perform_Global_Optimization (WN *, WN *, ALIAS_MANAGER *);
extern WN* Pre_Optimizer (INT32, WN*, DU_MANAGER*, ALIAS_MANAGER*);
extern void choose_from_complete_struct_for_relayout_candidates();
extern DU_MANAGER* Create_Du_Manager (MEM_POOL *);
extern void Delete_Du_Manager (DU_MANAGER *, MEM_POOL *);
extern BOOL Verify_alias (ALIAS_MANAGER *, WN *);
#endif // BUILD_FAST_BIN
#else

#pragma weak wopt_main
#pragma weak Wopt_Init
#pragma weak Wopt_Fini
#pragma weak Perform_Global_Optimization
#pragma weak Perform_Preopt_Optimization
#pragma weak Pre_Optimizer
#pragma weak choose_from_complete_struct_for_relayout_candidates
#pragma weak Create_Du_Manager
#pragma weak Delete_Du_Manager
#pragma weak Verify_alias

#endif // __linux__
#endif // SHARED_BUILD

// symbols defined in lno.so
#ifndef BUILD_SKIP_LNO
#if defined(__linux__) || defined(BUILD_OS_DARWIN)

extern void (*lno_main_p) (INT, char**, INT, char**);
#define lno_main (*lno_main_p)

extern void (*Lno_Init_p) ();
#define Lno_Init (*Lno_Init_p)

extern void (*Lno_Fini_p) ();
#define Lno_Fini (*Lno_Fini_p)

extern WN* (*Perform_Loop_Nest_Optimization_p) (PU_Info*, WN*, WN*, BOOL);
#define Perform_Loop_Nest_Optimization (*Perform_Loop_Nest_Optimization_p)

#else 

#pragma weak lno_main
#pragma weak Lno_Init
#pragma weak Lno_Fini
#pragma weak Perform_Loop_Nest_Optimization

#endif // __linux__

// symbols defined in ipl.so

#if defined(__linux__) || defined(BUILD_OS_DARWIN)

extern void (*Ipl_Extra_Output_p) (Output_File *);
#define Ipl_Extra_Output (*Ipl_Extra_Output_p)

extern void (*Ipl_Init_p) ();
#define Ipl_Init (*Ipl_Init_p)

extern void (*Ipl_Fini_p) ();
#define Ipl_Fini (*Ipl_Fini_p)

extern void (*ipl_main_p) (INT, char **);
#define ipl_main (*ipl_main_p)

extern void (*Perform_Procedure_Summary_Phase_p) (WN*, DU_MANAGER*,
						  ALIAS_MANAGER*, void*);
#define Perform_Procedure_Summary_Phase (*Perform_Procedure_Summary_Phase_p)

#ifdef KEY	// bug 3672
extern void (*Preprocess_struct_access_p)(void);
#define Preprocess_struct_access (*Preprocess_struct_access_p)
#endif

#else

#pragma weak ipl_main
#pragma weak Ipl_Init
#pragma weak Ipl_Fini
#pragma weak Ipl_Extra_Output
#pragma weak Perform_Procedure_Summary_Phase

#endif // __linux__
#endif // !BUILD_SKIP_LNO

#include "w2c_weak.h"
#include "w2f_weak.h"

#ifdef BUILD_SKIP_LNO
#define Lno_Init() Fail_FmtAssertion("lno not built")
#define Lno_Fini() Fail_FmtAssertion("lno not built")
#define lno_main(a,b,c,d) Fail_FmtAssertion("lno not built")
#define Perform_Loop_Nest_Optimization(a,b,c,d) NULL
#define Ipl_Init() Fail_FmtAssertion("lno not built")
#define Ipl_Fini() Fail_FmtAssertion("lno not built")
#define ipl_main(a,b) Fail_FmtAssertion("lno not built")
#define Perform_Procedure_Summary_Phase(a,b,c,d) Fail_FmtAssertion("lno not built")
#define Preprocess_struct_access() Fail_FmtAssertion("lno not built")
#define Ipl_Extra_Output(a) Fail_FmtAssertion("lno not built")
#endif // BUILD_SKIP_LNO

static INT ecount = 0;
static BOOL need_wopt_output = FALSE;
static BOOL need_lno_output = FALSE;
static BOOL need_ipl_output = FALSE;
static Output_File *ir_output = 0;

// options stack for PU and region level pragmas
static OPTIONS_STACK *Options_Stack;

static BOOL reset_opt_level = FALSE;
static struct ALIAS_MANAGER *alias_mgr = NULL;

static BOOL Run_Distr_Array = FALSE;
BOOL Run_MemCtr = FALSE;

/* Keep track of which optional components are loaded, where we need
 * to do so.
 */
static BOOL   wopt_loaded = FALSE;
extern BOOL   Whirl2f_loaded;    /* Defined in cleanup.c */
extern BOOL   Whirl2c_loaded;    /* Defined in cleanup.c */

extern void *Current_Dep_Graph;
FILE *DFile = stderr;

static void
load_components (INT argc, char **argv)
{
    INT phase_argc;
    char **phase_argv;

    if (!(Run_lno || Run_wopt || Run_preopt || Run_cg || Run_w2c || Run_w2f
          || Run_w2fc_early || Run_ipl))
      Run_cg = TRUE;		    /* if nothing is set, run CG */

    if (Run_cg || Run_lno || Run_autopar) {
      // initialize target-info before cg or lno
      Initialize_Targ_Info();
    }

    if (Run_ipl) {
      Run_lno = Run_wopt = Run_cg = Run_w2fc_early
	= Run_w2c = Run_w2f = FALSE;
    }

    if (Run_cg) {
      Get_Phase_Args (PHASE_CG, &phase_argc, &phase_argv);
#ifdef TARG_IA64
      load_so ("orc_ict.so", CG_Path, Show_Progress);
      load_so ("orc_intel.so", CG_Path, Show_Progress);
#endif
#if !defined(BUILD_FAST_BIN)
      load_so ("cg.so", CG_Path, Show_Progress);
#endif
      CG_Process_Command_Line (phase_argc, phase_argv, argc, argv);
    }

    if (Run_wopt || Run_preopt || Run_lno || Run_autopar) {
      Get_Phase_Args (PHASE_WOPT, &phase_argc, &phase_argv);
#if !defined(BUILD_FAST_BIN)
      load_so ("wopt.so", WOPT_Path, Show_Progress);
#endif
      wopt_main (phase_argc, phase_argv, argc, argv);
      wopt_loaded = TRUE;
    }

    if (Run_ipl) {
      Get_Phase_Args (PHASE_IPL, &phase_argc, &phase_argv);
      load_so ("ipl.so", Ipl_Path, Show_Progress);
      ipl_main (phase_argc, phase_argv);
    }

    if (Run_lno || Run_autopar) {
      Get_Phase_Args (PHASE_LNO, &phase_argc, &phase_argv);
      load_so ("lno.so", LNO_Path, Show_Progress);
      lno_main (phase_argc, phase_argv, argc, argv);

      // load in ipl.so if we need to perform automatic
      // parallelization and interprocedural analysis has
      // been performed
      if (Run_autopar && LNO_IPA_Enabled) {
	  load_so("ipl.so", Ipl_Path, Show_Progress);
      }
  }

    if (Run_w2c)
    {
      Get_Phase_Args (PHASE_W2C, &phase_argc, &phase_argv);
      load_so("whirl2c.so", W2C_Path, Show_Progress);
      Whirl2c_loaded = TRUE;
      W2C_Process_Command_Line(phase_argc, phase_argv, argc, argv);
    }

    if (Run_w2f)
    {
      Get_Phase_Args (PHASE_W2F, &phase_argc, &phase_argv);
      load_so("whirl2f.so", W2F_Path, Show_Progress);
      Whirl2f_loaded = TRUE;
      W2F_Process_Command_Line(phase_argc, (const char**)phase_argv, argc, (const char**)argv);
    }
} /* load_components */


/* phase-specific initializations that need to be done after reading
 * in the global symbol tables.
 */
static void
Phase_Init (void)
{
    char *output_file_name = Obj_File_Name;

    if (Run_Distr_Array      && 
        (Run_w2c || Run_w2f) &&
        !Run_lno             &&
        !Run_wopt            &&
        !Run_cg)
    {
       /* A special case, where it looks as though we only wish to
        * run some early phases and then put out the flist or clist.
        * Disable the turning on of subsequent phases due to the
        * Run_Distr_Array flag.
        */
       Run_Distr_Array = FALSE;
    }
    if ( LNO_Run_Lego_Set && ( LNO_Run_Lego == FALSE ) )
      Run_Distr_Array = FALSE;

    if (Run_cg)
	CG_Init ();
    if (Run_wopt || Run_preopt)
	Wopt_Init ();
    if (Run_ipl)
	Ipl_Init ();
    if (Run_lno || Run_Distr_Array || Run_autopar)
	Lno_Init ();
    if ( Opt_Level > 0 ) /* run VHO at -O1 and above */
        Vho_Init ();
    if (Run_w2c)
	W2C_Outfile_Init (TRUE/*emit_global_decls*/);
    if (Run_w2f)
	W2F_Outfile_Init ();
    if ((Run_lno || Run_preopt) && !Run_cg && !Run_wopt)
	need_lno_output = TRUE;
    if (Run_wopt && !Run_cg)
	need_wopt_output = TRUE;

    if (Run_ipl) {
	need_ipl_output = TRUE;
	need_lno_output = need_wopt_output = FALSE;
#ifdef KEY
	// bug 11856: If VHO does struct transformations, then IPA cannot
	// legally optimize struct accesses.
	VHO_Struct_Opt = FALSE;
#endif
    }

    if (output_file_name == 0) {
	if (Src_File_Name)
	    output_file_name = Last_Pathname_Component (Src_File_Name);
	else
	    output_file_name = Irb_File_Name;
    }

    if (need_lno_output) {
	Write_BE_Maps = TRUE;
	// Output IR after preopt to .P file, and IR after LNO to .N file.
	if (Run_lno)
	    ir_output = Open_Output_Info(New_Extension(output_file_name,".N"));
	else
	    ir_output = Open_Output_Info(New_Extension(output_file_name,".P"));
    }
    if (need_wopt_output) {
	Write_ALIAS_CLASS_Map = TRUE;
	Write_BE_Maps = TRUE;
	ir_output = Open_Output_Info(New_Extension(output_file_name,".O"));
    }
    if (need_ipl_output) {
	Write_BE_Maps = FALSE;
	ir_output = Open_Output_Info (Obj_File_Name ?
				      Obj_File_Name :
				      New_Extension(output_file_name, ".o"));
    }
    if (Emit_Global_Data) {
	Write_BE_Maps = FALSE;
	ir_output = Open_Output_Info (Global_File_Name);
    }
	
    if (Run_wopt) {
      if (Language != LANG_KR_C) {
        Pad_Global_Arrays();
      }
    }
    if ((Run_cg || Run_wopt) && !Read_Global_Data)
	Allocate_File_Statics();	/* allocate globals */

} /* Phase_Init */


static void
Phase_Fini (void)
{
    CURRENT_SYMTAB = GLOBAL_SYMTAB;

    /* Always finish w2c and w2f first */
    if (Run_w2c)
	W2C_Outfile_Fini (TRUE/*emit_global_decls*/);
    if (Run_w2f)
	W2F_Outfile_Fini ();

    if (Run_Dsm_Cloner || Run_Dsm_Common_Check)
      DRA_Finalize ();

    if ( Opt_Level > 0 ) /* run VHO at -O1 and above */
        Vho_Fini ();
    if (Run_lno || Run_Distr_Array || Run_autopar)
	Lno_Fini ();
    if (Run_ipl)
	Ipl_Fini ();
    if (Run_wopt || Run_preopt)
	Wopt_Fini ();
    if (Run_cg)
	CG_Fini ();

    Verify_SYMTAB (CURRENT_SYMTAB); /* Verifies global SYmtab */
} /* Phase_Fini */

/* static */ char *
Get_Orig_PU_Name (PU_Info * current_pu)
{
    DST_IDX dst;
    DST_INFO *info;
    DST_SUBPROGRAM *PU_attr;

    dst = PU_Info_pu_dst(current_pu);

    if (DST_IS_NULL (dst)) {
       return ST_name(PU_Info_proc_sym(current_pu));
    }

    info = DST_INFO_IDX_TO_PTR (dst);

    if ( (DST_INFO_tag(info) != DW_TAG_subprogram)
        || DST_IS_declaration(DST_INFO_flag(info)) )
    {
	return ST_name(PU_Info_proc_sym(current_pu));
    }
    PU_attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(info), DST_SUBPROGRAM);
    if (PU_attr->def.name.byte_idx < 0) {
      return NULL;
      /* Why not the following line instead? -- RK 960808
       * return ST_name(PU_Info_proc_sym(current_pu));
       */
    }
    return DST_STR_IDX_TO_PTR(DST_SUBPROGRAM_def_name(PU_attr));
}

static void
Save_Cur_PU_Name (char *name, INT rid_id)
{
    if ( Cur_PU_Name == NULL ) {
	/* ST_name will return a pointer into the symbol table, which is
	 * mmap-ed. This causes a problem in the error message routines
	 * when an unexpected signal occurs, because as part of the cleanup
	 * files are closed. To fix the problem we just allocate some
	 * memory and make a copy.
	 * Allocate 8 extra bytes to leave room for RGN suffix.
	 */
	Cur_PU_Name = TYPE_MEM_POOL_ALLOC_N(char, &MEM_pu_nz_pool, 
		strlen(name) + 8);
	Cur_PU_Name = strcpy(Cur_PU_Name, name);
    }
    if (rid_id != 0) {
	/* add RGN suffix */
	sprintf(Cur_PU_Name,"%s.RGN%03d", name, rid_id);
    }
    else if (strlen(name) != strlen(Cur_PU_Name)) {
	/* clear RGN suffix */
	Cur_PU_Name = strcpy(Cur_PU_Name, name);
    }
}


//  Adjust/Lower optimization level based on
//   1. size of PU and Olimit
//   2. existence of non-ANSI setjmp calls
//
static WN *
Adjust_Opt_Level (PU_Info* current_pu, WN *pu, char *pu_name)
{
    INT new_opt_level = 0;
    COMPUTE_PU_OLIMIT;

#ifdef KEY
    // Demangle C++ pu_name.
    char *p, *demangled_pu_name = pu_name;
    BOOL has_demangled_pu_name = FALSE;

    if ((PU_src_lang(Get_Current_PU()) & PU_CXX_LANG) &&
	// C++ mangled names begin with "_Z".
	pu_name[0] == '_' &&
	pu_name[1] == 'Z') {
      p = cplus_demangle(pu_name, DMGL_PARAMS | DMGL_ANSI | DMGL_TYPES);
      if (p) {
	demangled_pu_name = p;
	has_demangled_pu_name = TRUE;
      }
    }
#endif

    if (Get_Trace(TKIND_INFO, TINFO_STATS)) {
	/* Print Olimit stats to trace file: */
	INT PU_Var_Cnt = ST_Table_Size (CURRENT_SYMTAB) +
	    PREG_Table_Size (CURRENT_SYMTAB); 
	fprintf (TFile, "PU_Olimit for %s is %d (bbs=%d,stms=%d,vars=%d)\n", 
		pu_name, PU_Olimit, PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, PU_Var_Cnt);
    }

    if ((Opt_Level > 0 || Run_autopar) && PU_Olimit > Olimit && !Olimit_opt) {
	if (Show_OPT_Warnings)
#ifdef KEY
	  ErrMsg (EC_Olimit_Exceeded, demangled_pu_name, PU_Olimit);
#else
	  ErrMsg (EC_Olimit_Exceeded, pu_name, PU_Olimit);
#endif
	reset_opt_level = TRUE;
    }
    if (((Opt_Level > 0 || Run_autopar) || Olimit_opt)
      && Query_Skiplist ( Optimization_Skip_List, Current_PU_Count() ) )
    {
	if (Show_OPT_Warnings)
#ifdef KEY
	  ErrMsg (EC_Not_Optimized, demangled_pu_name, Current_PU_Count() );
#else
	  ErrMsg (EC_Not_Optimized, pu_name, Current_PU_Count() );
#endif
	reset_opt_level = TRUE;
    } 
    if (/* !LANG_Ansi_Setjmp_On && */ 
      /* 1. Cannot check LANG_Ansi_Setjmp_On because IPA does not pass -LANG group.
	 2. The ST_pu_calls_setjmp is not set unless LANG_Ansi_Setjmp_On = false */
	PU_calls_setjmp (Get_Current_PU ())) {
      reset_opt_level = TRUE;
      new_opt_level = 1;
#ifdef KEY
      ErrMsg (EC_Not_Ansi_Setjmp, demangled_pu_name, Current_PU_Count(),
	      new_opt_level );
#else
      ErrMsg (EC_Not_Ansi_Setjmp, pu_name, Current_PU_Count(), new_opt_level );
#endif
    } 
    if (reset_opt_level) {
	Opt_Level = new_opt_level;
	Run_lno = Run_preopt = Run_wopt = Run_autopar = FALSE;
	alias_mgr = NULL;
	Olimit_opt = FALSE;
    }
#ifdef KEY
#define OLIMIT_WARN_THRESHOLD 50000
    else if (Opt_Level > 1 && !Olimit_opt && 
	     (PU_Olimit > OLIMIT_WARN_THRESHOLD) && Show_OPT_Warnings) {
#ifdef KEY
      ErrMsg (EC_Olimit_Slow, demangled_pu_name, PU_Olimit);
#else
      ErrMsg (EC_Olimit_Slow, pu_name, PU_Olimit);
#endif
    }
#endif

    if ((PU_Olimit > Olimit) && Olimit_opt) {
      /* split into regions (ORI) */
      pu = Olimit_Region_Insertion (pu, Olimit);
      /* 457243, for 7.2 throttle back LNO when Olimit is reached.
       * For 7.2 LNO runs on the PU, for 7.3 it will run on regions.
       * options: -LNO:ou=1:fusion=0
       * variables: UINT32 Outer_unroll = 1, UINT32 Fusion = 0
       * common/com/config_lno.h
       */
      if (Run_lno || Run_Distr_Array || Run_preopt || Run_autopar) {
	LNO_Outer_Unroll = 1;
	LNO_Fusion = 0;
	if (Show_OPT_Warnings)
#ifdef KEY
	  ErrMsg(EC_LNO_Backoff, demangled_pu_name, LNO_Outer_Unroll,
		 LNO_Fusion);
#else
	  ErrMsg(EC_LNO_Backoff, pu_name, LNO_Outer_Unroll, LNO_Fusion);
#endif
      }
    }

#ifdef KEY
    if (has_demangled_pu_name)
      free (demangled_pu_name);
#endif

    return pu;
} /* Adjust_Opt_Level */

static void
Ipl_Processing (PU_Info *current_pu, WN *pu)
{
    struct DU_MANAGER *du_mgr;
    struct ALIAS_MANAGER *al_mgr;

    MEM_POOL_Push (&MEM_local_pool);

    PU_adjust_addr_flags(Get_Current_PU_ST(), pu);

    if (Run_preopt) {
	du_mgr = Create_Du_Manager(MEM_pu_nz_pool_ptr);
	al_mgr = Create_Alias_Manager(MEM_pu_nz_pool_ptr, pu);
	Check_for_IR_Dump_Before_Phase(TP_IPL, pu, "Pre_Optimizer");
	pu = Pre_Optimizer(PREOPT_IPA0_PHASE, pu, du_mgr, al_mgr);
	Check_for_IR_Dump(TP_IPL, pu, "Pre_Optimizer");
#ifdef Is_True_On
	if (Get_Trace (TKIND_ALLOC, TP_IPA)) {
	    fprintf (TFile, "\n%s%s\tMemory allocation information after"
		     " IPA local pre_opt\n%s%s\n", DBar, DBar, DBar, DBar);
	    MEM_Trace ();
	}
#endif
	Delete_Alias_Manager (al_mgr, MEM_pu_nz_pool_ptr);
	Delete_Du_Manager (du_mgr, MEM_pu_nz_pool_ptr);
    } else {
	Set_Error_Phase ( "IPA Summary" );
	Perform_Procedure_Summary_Phase (pu, du_mgr, al_mgr, 0);
    }

    AliasAnalyzer::Delete_Alias_Analyzer();

    /* Write out the current proc */
    Set_PU_Info_tree_ptr(current_pu, pu);

    Write_PU_Info (current_pu);

    MEM_POOL_Pop (&MEM_local_pool);
    
} /* Ipl_Processing */

/*====================================================================*/
/* Compilation time loop: LNO			 		      */
/*====================================================================*/
static WN *
LNO_Processing (PU_Info *current_pu, WN *pu)
{
  REGION_CS_ITER rgn_iter;

  REGION_CS_ITER_init(&rgn_iter, pu);
  /* PV 457243 for Mongoose 7.2 do just the PU (RID_TYPE_func_entry),
     for Mongoose 7.3 use: RID_TYPE_olimit | RID_TYPE_pragma
     These keeps regions out of LNO for 7.2 */
  for (REGION_CS_NoEarlierSub_First(&rgn_iter, pu, RID_TYPE_func_entry);
       REGION_CS_NoEarlierSub_While(&rgn_iter);
       REGION_CS_NoEarlierSub_Next(&rgn_iter)) {

    WN *rwn;

    rwn = REGION_remove_and_mark(pu, &rgn_iter);
    Is_True(rwn != NULL,
	    ("BE driver, IR inconsistency, LNO loop"));
    if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
      fprintf(TFile,"===== BE driver, LNO loop: %s %d, stacked=%d\n",
	      REGION_CS_ITER_is_pu(&rgn_iter) ? "PU" : "RGN",
	      RID_id(REGION_get_rid(rwn)),
	      !REGION_CS_ITER_is_not_stacked(&rgn_iter));
      RID_WN_Tree_Print(TFile, rwn);
    }

    /* Don't (re-)run LNO or preopt on nested mp PU's */
    /* unless early mp processing was requested.  In which case */
    /* LNO and preopt haven't been run on the nexted mp PU's yet. */
    BOOL is_mp = PU_mp (Get_Current_PU ());
    BOOL needs_lno = PU_mp_needs_lno (Get_Current_PU ());

    if (!is_mp || Early_MP_Processing) {
      if (Run_lno || Run_autopar || (Run_Distr_Array && needs_lno)) {

	if (Early_MP_Processing && is_mp) {
          Free_Dep_Graph(); 
	  Current_Dep_Graph = PU_Info_depgraph_ptr(Current_PU_Info);
	}
	rwn = Perform_Loop_Nest_Optimization(current_pu, pu, rwn, 
          TRUE /*can use alloca*/);
	Set_PU_Info_depgraph_ptr(Current_PU_Info,Current_Dep_Graph);
	if (Current_Dep_Graph) {
	  Set_PU_Info_state(Current_PU_Info,WT_DEPGRAPH,Subsect_InMem);
        } else {
	  Set_PU_Info_state(Current_PU_Info,WT_DEPGRAPH,Subsect_Missing);
        }

	Check_for_IR_Dump(TP_LNOPT, rwn, "LNO");
	if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
	  fprintf(TFile,"===== After LNO, %s %d, stacked=%d\n",
		  REGION_CS_ITER_is_pu(&rgn_iter) ? "PU" : "RGN",
		  RID_id(REGION_get_rid(rwn)),
		  !REGION_CS_ITER_is_not_stacked(&rgn_iter));
	  RID_WN_Tree_Print(TFile, rwn);
	  fdump_tree(TFile, rwn);
	  RID_set_print(TFile,REGION_get_rid(rwn));
	}
      } else if (Run_preopt) {
	rwn = Perform_Preopt_Optimization(pu, rwn);
	Check_for_IR_Dump(TP_GLOBOPT, rwn, "PREOPT");
	if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
	  fprintf(TFile,"===== After PREOPT (:p), %s %d, stacked=%d\n",
		  REGION_CS_ITER_is_pu(&rgn_iter) ? "PU" : "RGN",
		  RID_id(REGION_get_rid(rwn)),
		  !REGION_CS_ITER_is_not_stacked(&rgn_iter));
	  RID_WN_Tree_Print(TFile, rwn);
	  fdump_tree(TFile, rwn);
	  RID_set_print(TFile,REGION_get_rid(rwn));
	}
      }
    } else {
      Free_Dep_Graph(); 
      Current_Dep_Graph = PU_Info_depgraph_ptr(Current_PU_Info);
    }

    if (REGION_CS_ITER_is_not_stacked(&rgn_iter)) /* this is tricky */
      pu = rwn;
    REGION_replace_from_mark(rwn, &rgn_iter);

    if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
      fprintf(TFile,"------------ bottom of LNO loop: -----------\n");
      REGION_consistency_check(pu);
      RID_WN_Tree_Print(TFile,pu);
      if (Run_cg)
	fdump_region_tree(TFile,pu);
      else
	fdump_tree(TFile,pu);
      fprintf(TFile,"--------------------------------------------\n");
    }
  } /* end of compilation time region loop */

  return pu;
} /* LNO_Processing */


/* Misc. processing after LNO is done, PU exists as a whole during this
   procedure */
static void
Post_LNO_Processing (PU_Info *current_pu, WN *pu)
{
    BOOL is_user_visible_pu = (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1) || 
                              ((Language == LANG_F90) &&
			       (CURRENT_SYMTAB == GLOBAL_SYMTAB + 2) &&
			       (!Is_Set_PU_Info_flags(current_pu, PU_IS_COMPILER_GENERATED))) ;
    
    /* Only run w2c and w2f on top-level PUs, unless otherwise requested.
     */
    if (Run_w2c && !Run_w2fc_early) {
	if (W2C_Should_Emit_Nested_PUs() || is_user_visible_pu) {
	    if (Cur_PU_Feedback)
		W2C_Set_Frequency_Map(WN_MAP_FEEDBACK);
	    W2C_Outfile_Translate_Pu(pu, TRUE/*emit_global_decls*/);
	}
    }
    if (Run_w2f && !Run_w2fc_early) {
	if (W2F_Should_Emit_Nested_PUs() || is_user_visible_pu) {
	    if (Cur_PU_Feedback)
		W2F_Set_Frequency_Map(WN_MAP_FEEDBACK);
	    W2F_Outfile_Translate_Pu(pu);
	}
    }

    /* only write .N file for PU, no need to replace region because
       REGION_remove_and_mark does nothing for pu (rwn is the pu) */
    if (need_lno_output) {
	Set_PU_Info_tree_ptr(current_pu, pu);
	Write_PU_Info(current_pu);
	Verify_SYMTAB (CURRENT_SYMTAB);
    }
} /* Post_LNO_Processing */

static WN *
WOPT_Processing (PU_Info *current_pu, WN *pu, REGION_CS_ITER *rgn_iter,
		 WN *rwn)
{
    rwn = Perform_Global_Optimization (pu, rwn, alias_mgr);
    Check_for_IR_Dump(TP_GLOBOPT, rwn, "WOPT");
    if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
      fprintf(TFile, "===== driver, after Perform_Global_Optimization");
      fprintf(TFile," RGN %d =====\n", RID_id(REGION_get_rid(rwn)));
      RID_WN_Tree_Print(TFile,pu);
      fdump_tree(TFile,rwn);
      fprintf(TFile,"===== driver ---------------------\n");
      fprintf(TFile,"#### Verify_alias, after mainopt, RGN %d\n",
	      RID_id(REGION_get_rid(rwn)));
      Verify_alias(alias_mgr, rwn);
      if (Run_cg) /* cg has to be loaded to run this routine */
	fdump_region_tree(TFile,rwn);
      fprintf(TFile,"#### Verify_alias, done\n");
    }

    return rwn;
} /* WOPT_Processing */

// Performance region loop WOPT-CG
static WN *
Do_WOPT_and_CG_with_Regions (PU_Info *current_pu, WN *pu)
{
    REGION_CS_ITER rgn_iter;
    BOOL Run_region_bounds;
    BOOL cg_one_time = TRUE;

    /* look over whole PU and set up stack model */
    Initialize_Stack_Frame (pu);	

    // decide whether the PU has regions that need bounds in it
    { RID *rid = REGION_get_rid(pu);
      Is_True(rid != NULL && RID_TYPE_func_entry(rid) && RID_id(rid) == 0,
	      ("Do_WOPT_and_CG_with_Regions, RID is incorrect"));
      Run_region_bounds = RID_contains_bounds(rid);
      Is_Trace(Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) ||
	       Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
	       (TFile,"Driver, RID_contains_bounds(%s) = %c\n",
		ST_name(PU_Info_proc_sym(current_pu)),
		Run_region_bounds ? 'T' : 'F'));
    }

    // One alias_mgr for all regions (both cg and wopt need it).
    // this is the alias manager for all region instances of
    // MainOpt and CG. Preopt uses it's own local alias manager,
    // one for each region. Deleted by Post_Process_PU.
    if ((Run_wopt || Run_region_bounds) && alias_mgr == 0)
      alias_mgr = Create_Alias_Manager(MEM_pu_nz_pool_ptr, pu);

    // Create the region boundary sets, be/region/region_bounds.cxx
    if (Run_region_bounds) {
      Set_Error_Phase("Generate Region Boundaries");
      Generate_region_boundaries(pu, alias_mgr);
    }

    // region loop
    // 1) This iterator first finds the enclosing block for each region so
    //    the region can be replaced once it it processed.
    // 2) REGION_remove_and mark pulls the region out of the WHIRL tree
    //    so we can process it independently, put back by
    //    REGION_replace_from_mark
    // 3) Process_pragma_options changes the compile options based on region
    //    pragmas in the code.
    
    REGION_CS_ITER_init(&rgn_iter, pu);
    for (REGION_CS_NoEarlierSub_First(&rgn_iter, pu,
	   (RID_TYPE)(RID_TYPE_olimit | RID_TYPE_pragma | RID_TYPE_loop));
	 REGION_CS_NoEarlierSub_While(&rgn_iter);
	 REGION_CS_NoEarlierSub_Next(&rgn_iter)) {
      WN *rwn;
      BOOL need_options_pop = FALSE;

      rwn = REGION_remove_and_mark(pu, &rgn_iter);

      Is_True(rwn != NULL, ("BE driver, IR inconsistency, WOPT-CG loop"));
      Is_True(REGION_get_rid(rwn) != NULL, ("BE driver, NULL RID"));
      Is_True(RID_type(REGION_get_rid(rwn)) != RID_TYPE_undefined,
	      ("BE driver, undefined RID type"));

      Set_Current_Region_For_Trace( RID_id(REGION_get_rid(rwn)) );

      if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
	fprintf(TFile,"===== BE driver, WOPT-CG loop: %s %d, stacked=%d\n",
		REGION_CS_ITER_is_pu(&rgn_iter) ? "PU" : "RGN",
		RID_id(REGION_get_rid(rwn)),
		!REGION_CS_ITER_is_not_stacked(&rgn_iter));
	RID_WN_Tree_Print(TFile, rwn);
      }

#ifndef KEY
      // process any region or PU level options pragmas
      { RID *rid = REGION_get_rid(rwn);
	Is_True(rid != NULL, ("BE driver, NULL rid inside region loop"));
	if (RID_options(rid) != NULL) {
	  Options_Stack->Process_Pragma_Options(RID_options(rid));
	  need_options_pop = TRUE;
	} 
      }
#endif

      if (Show_Progress) {
	if (rwn != pu)
	  fprintf(stderr, "... compiling region (%d)\n", 
		  RID_id(REGION_get_rid(rwn)));
	else if (!cg_one_time)	/* must have had regions in pu */
	  fprintf(stderr, "... compiling program unit\n");
      }
      /* if we are processing a region, add that to the name */
      Save_Cur_PU_Name (ST_name(PU_Info_proc_sym(current_pu)), 
			RID_id(REGION_get_rid(rwn)));

      /* Add instrumentation here for wopt. */
      if (Instrumentation_Enabled
	  && (Instrumentation_Type_Num & WHIRL_PROFILE)
	  && (Instrumentation_Phase_Num == PROFILE_PHASE_BEFORE_WOPT)) {
	WN_Instrument(rwn, PROFILE_PHASE_BEFORE_WOPT); 
      } else if (Feedback_Enabled[PROFILE_PHASE_BEFORE_WOPT]) {
	WN_Annotate(rwn, PROFILE_PHASE_BEFORE_WOPT, &MEM_pu_pool);
      }
      Set_Error_Phase ( "Before WOPT" );

      if (Run_wopt) {
	rwn = WOPT_Processing (current_pu, pu, &rgn_iter, rwn);
        if (WN_opcode(rwn) == OPC_FUNC_ENTRY)
          Verify_SYMTAB (CURRENT_SYMTAB);
	if ( Cur_PU_Feedback ) {
	  Cur_PU_Feedback->Verify("after WOPT");
	}
      }

      /* we may still have to print the .O file:		   */
      /* (Olimit stops optimization for one PU but not all)	   */
      /* only write .O file for PU, no need to replace region	   */
      /* because REGION_remove_and_mark does nothing for pu	   */
      if (need_wopt_output && REGION_CS_ITER_is_not_stacked(&rgn_iter)) {
	Set_PU_Info_tree_ptr(current_pu, rwn);
	Write_PU_Info(current_pu);
      }


      /* in case wopt cleared it (temporary backwards-compatibility) */
      Save_Cur_PU_Name (ST_name(PU_Info_proc_sym(current_pu)), 
		RID_id(REGION_get_rid(rwn)));

      /* Add instrumentation here for cg. */
      if (Instrumentation_Enabled
	  && (Instrumentation_Type_Num & WHIRL_PROFILE)
	  && (Instrumentation_Phase_Num == PROFILE_PHASE_BEFORE_CG)) {
	rwn = WN_Lower(rwn, LOWER_SCF, NULL, 
		       "Lower structured control flow");
	WN_Instrument(rwn, PROFILE_PHASE_BEFORE_CG);
      } else if (Feedback_Enabled[PROFILE_PHASE_BEFORE_CG]) {
	rwn = WN_Lower(rwn, LOWER_SCF, NULL, 
		       "Lower structured control flow");
	WN_Annotate(rwn, PROFILE_PHASE_BEFORE_CG, &MEM_pu_pool);
      }
      Set_Error_Phase ( "Before CG" );

      if (Run_cg) { /* lower for cg */
	Set_Error_Phase ("Lowering");
        WB_LWR_Initialize(rwn, alias_mgr);

    /* lowering MLDID/MSTID before lowering to CG */
	if (!Run_wopt ||
	    // OSP 421, MLDID/MSTID is not lowered.
	    Query_Skiplist (WOPT_Skip_List, Current_PU_Count()) ) {

      rwn = WN_Lower(rwn, LOWER_MLDID_MSTID, alias_mgr, 
                       "Lower MLDID/MSTID when not running WOPT");
#ifdef KEY // bug 7298: this flag could have been set by LNO's preopt
      Clear_BE_ST_pu_has_valid_addr_flags(PU_Info_proc_sym(current_pu)); 
#endif
    }
 
#if defined(EMULATE_FLOAT_POINT) && defined(TARG_SL)
 	rwn = WN_Lower(rwn, LOWER_TO_CG | LOWER_FP_EMULATE, alias_mgr, "Lowering to CG/Lowering float point emulate");
#else
	rwn = WN_Lower(rwn, LOWER_TO_CG, alias_mgr, "Lowering to CG");
#endif
#ifdef TARG_IA64
	if (Only_Unsigned_64_Bit_Ops &&
	    (!Run_wopt || Query_Skiplist (WOPT_Skip_List, Current_PU_Count()))) 	  U64_lower_wn(rwn, FALSE);
#endif
        WB_LWR_Terminate();
	if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
	  fprintf(TFile,"===== driver, after lowering\n");
	  RID_WN_Tree_Print(TFile,rwn);
	}

#ifdef TARG_NVISA
        // we want up-to-date addr_saved info
        // in case original addr_saved got optimized away,
        // in which case cg can avoid using local memory.
        Set_BE_ST_pu_needs_addr_flag_adjust(Get_Current_PU_ST());
        PU_adjust_addr_flags(Get_Current_PU_ST(), rwn);
#endif

	if ( Cur_PU_Feedback ) {
	  Cur_PU_Feedback->Verify("after LOWER_TO_CG");
	}

	if (cg_one_time) {  /* do this before processing part of PU */
	  cg_one_time = FALSE;
	  /* move data layout to after whirl lowering	*/
	  Set_Error_Phase("Data Layout");
    	  /* look over whole PU and calculate size of actual area */
    	  Calculate_Stack_Frame_Sizes (rwn);	
	  CG_PU_Initialize(pu);
	}

	if (!REGION_CS_ITER_is_not_stacked(&rgn_iter) ||
	    !REGION_CS_ITER_is_pu(&rgn_iter)) { /* pass over region */
	  DST_IDX tmp_idx;
	  WN *compiled_block;
	  tmp_idx.byte_idx = DST_INVALID_BYTE_IDX; /* mark DST as NULL */
	  FmtAssert(PU_has_region (Get_Current_PU ()),
		    ("BE driver, found region when SYMTAB_has_rgn is off"));
	  if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
	    fprintf(TFile,
		    "===== driver, before CG_Generate_Code (region)\n");
	    RID_WN_Tree_Print(TFile,rwn);
	    RID_set_print(TFile,REGION_get_rid(rwn));
	  }
	  compiled_block = CG_Generate_Code(rwn, alias_mgr, tmp_idx, TRUE);
	  rwn = compiled_block;
	  if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
	    fprintf(TFile,
		    "===== driver, after CG_Generate_Code\n");
	    RID_WN_Tree_Print(TFile,rwn);
	  }
	} // if (!REGION_CS_ITER_is_not_stacked(&rgn_iter) ||
      } // if (Run_cg)

      if (REGION_CS_ITER_is_not_stacked(&rgn_iter)) /* this is tricky */
	pu = rwn;
      REGION_replace_from_mark(rwn, &rgn_iter);
      // put options back the way they were
      if (need_options_pop)
	Options_Stack->Pop_Current_Options();
      if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
	fprintf(TFile,
		"------------ bottom of WOPT-CG loop: ------------\n");
	Is_True(REGION_consistency_check(pu),(""));
	RID_WN_Tree_Print(TFile,pu);
	if (Run_cg)
	  fdump_region_tree(TFile,pu);
	else
	  fdump_tree(TFile,pu);
	fprintf(TFile,"--------------------------------------------\n");
      }
      if (rwn != pu)
	Report_CG_Region_Timing ( Tim_File, Cur_PU_Name );
    }
    Verify_SYMTAB (CURRENT_SYMTAB);
    return pu;
} /* Do_WOPT_and_CG_with_Regions */


static void
Post_Process_Backend (PU_Info *current_pu, WN *pu)
{
  if (Run_cg) {
    if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
      fprintf(TFile,"BE driver, Post_Process_Backend "
	      "+++++++++++++++++++++++++++++++\n");
      RID_WN_Tree_Print(TFile,pu);
      fdump_region_tree(TFile,pu);
      fprintf(TFile,"BE driver, Post_Process_Backend "
	      "+++++++++++++++++++++++++++++++\n");
      fprintf(TFile,"Right before CG_Generate_Code (PU)\n");
    }

    CG_Generate_Code(pu, alias_mgr, PU_Info_pu_dst(current_pu), FALSE);
    CG_PU_Finalize();
  }
  // Delete alias manager after CG finished, PV 525127, 527977
  if (alias_mgr) {
    Delete_Alias_Manager(alias_mgr, MEM_pu_nz_pool_ptr);
    alias_mgr = NULL;
  }

  AliasAnalyzer::Delete_Alias_Analyzer();
} /* Post_Process_Backend */



extern "C" {
  extern void Process_Fill_Align_Pragmas (WN* func_wn);
  extern void Rewrite_Pragmas_On_Structs (WN* block_wn, WN* wn);
}

/***********************************************************************
 *
 * Find all EH regions in the PU, and mark their INITOs as used.
 *
 ***********************************************************************/
static void Update_EHRegion_Inito_Used (WN *wn) {
  if (!wn) return;
  
  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_REGION && WN_ereg_supp(wn)) {
    INITO_IDX ino_idx = WN_ereg_supp(wn);
#ifdef TARG_IA64
    // Note a special case of try label when
    // INITV_kind(INITO_val(ino_idx)) == INITVKIND_LABEL,
    // Shouldn't set the flag ST_IS_NOT_USED
    // Just reserve the value which gfecc pass through
    if (INITV_kind(INITO_val(ino_idx)) != INITVKIND_LABEL){                
      ST *st = INITO_st(ino_idx);
      Clear_ST_is_not_used(st);
    }
#else
    ST *st = INITO_st(ino_idx);
    Clear_ST_is_not_used(st);
#endif
  }

  // now recurse
  if (opr == OPR_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Update_EHRegion_Inito_Used(kid);
      kid = WN_next(kid);
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Update_EHRegion_Inito_Used(WN_kid(wn,kidno));
    }
  }
}

/***********************************************************************
 *
 * This pass is called after preopt+lno+mplowering. 
 * Any of those passes may have deleted EH-regions, but left the
 * INITO sts for those regions hanging around.
 * This pass will search for all used INITOs, and mark the rest unused.
 *
 ***********************************************************************/
static void Update_EHRegion_Inito (WN *pu) {
  INT i;
  INITO *ino;

  // first mark all EH-region STs unused.
  // But as a special case when INITV_kind(INITO_val(ino)) == INITVKIND_LABEL
  // gfecc have set the flag ST_IS_NOT_USED, so though we needn't remarked 
  // it to ST_IS_NOT_USED again, but it's OK as long as don't set this flag 
  // when Update_EHRegion_Inito_Used.
  FOREACH_INITO (CURRENT_SYMTAB, ino, i) {
    ST *st = INITO_st(ino);
    if (ST_sclass(st) == SCLASS_EH_REGION ||
        ST_sclass(st) == SCLASS_EH_REGION_SUPP) {
      Set_ST_is_not_used(st);
    }
  }

  // now find INITO sts that are referenced in WHIRL,
  // and mark them used.
  // Not all the INITO sts referenced in WHIRL will be used.
  Update_EHRegion_Inito_Used (pu);
}

static void
Backend_Processing (PU_Info *current_pu, WN *pu)
{
    {
	/* Always process the first PU for fill-align, since that one contains
	 * the pragmas for global symbols. And, because of IPA, we cannot
	 * depend on the SYMTAB_id being 1 for the first PU.
	 */
	static BOOL done_first_pu = FALSE;
	BOOL needs_fill_align_lowering =
	    PU_needs_fill_align_lowering (Get_Current_PU ());
	if (needs_fill_align_lowering || !done_first_pu) {
	    Process_Fill_Align_Pragmas (pu);
	    done_first_pu = TRUE;
	}
    }

#ifdef KEY
    BOOL need_options_pop = FALSE;

    // process any PU level options pragmas
    WN *block = WN_func_pragmas(pu); 
    for (WN *pragwn = WN_first(block); pragwn; pragwn = WN_next(pragwn))
      if (WN_pragma(pragwn) == WN_PRAGMA_OPTIONS) {
	ST *st = WN_st(pragwn);
	TCON *tc = &Tcon_Table[ST_tcon (st)];
	Options_Stack->Process_Pragma_Options(Targ_String_Address(*tc));
	need_options_pop = TRUE;
      } 
#endif

    PU_adjust_addr_flags(Get_Current_PU_ST(), pu);

    if (Run_MemCtr)
	MemCtr_Add (pu);

    /* Make sure that RETURN_VAL nodes, Return_Val_Preg references and
       MLDID/MSTID nodes have been lowered. This requires its own pass
       because it may have to go back to change the previous CALL statement
       to add a fake parameter. */
    if (WHIRL_Return_Val_On || WHIRL_Mldid_Mstid_On) {
        Is_True(WHIRL_Return_Val_On && WHIRL_Mldid_Mstid_On,
	        ("-INTERNAL:return_val and -INTERNAL:mldid_mstid must be on the same time"));
	pu = WN_Lower (pu, LOWER_RETURN_VAL, NULL,
		       "RETURN_VAL & MLDID/MSTID lowering");
    }

#ifdef KEY // bug 9171
    if (Run_autopar && Early_MP_Processing) {
      Early_MP_Processing = FALSE;
      ErrMsg (EC_No_Apo_Early_Mp);
    }
#endif

    /* If early mp processing has been requested, then do it before running
       lno/preopt. */
    BOOL has_mp = PU_has_mp (Get_Current_PU ());
    if (has_mp && Early_MP_Processing) {
	Set_PU_Info_depgraph_ptr(Current_PU_Info,Current_Dep_Graph);
	Set_PU_Info_state(Current_PU_Info,WT_DEPGRAPH,Subsect_InMem);

	Set_Error_Phase ( "MP Lowering" );
        WB_LWR_Initialize(pu, NULL);
	pu = WN_Lower (pu, LOWER_MP, NULL, "Before MP Lowering");
        Verify_No_MP(WN_func_body(pu));
//Bug 4688
#ifdef KEY
        extern void Post_MP_Processing (WN *);
        Post_MP_Processing (PU_Info_tree_ptr(Current_PU_Info));
#endif
        WB_LWR_Terminate();
    }

    // annotation map should be constructed before LNO
    WN_MEMOP_ANNOT_MGR_Constructor mem_annot_mgr;

    /* Add instrumentation here for lno. */
    if( Instrumentation_Enabled
	&& (Instrumentation_Type_Num & WHIRL_PROFILE)
	&& (Instrumentation_Phase_Num == PROFILE_PHASE_BEFORE_LNO)) {
	WN_Instrument(pu, PROFILE_PHASE_BEFORE_LNO); 
    } else if ( Feedback_Enabled[PROFILE_PHASE_BEFORE_LNO] ) {
      WN_Annotate(pu, PROFILE_PHASE_BEFORE_LNO, &MEM_pu_pool);   
    }
    Set_Error_Phase ( "LNO Processing" );

    if (Run_lno || Run_Distr_Array || Run_preopt || Run_autopar) {
#ifdef KEY // to avoid assertion in PU that represents file-scope asm statement
	if (! (WN_operator(pu) == OPR_FUNC_ENTRY && 
	       ST_asm_function_st(*WN_st(pu)))) 
#endif
	pu = LNO_Processing (current_pu, pu);/* make -O0, -O1, -O2 a bit faster*/
#ifndef KEY 
	// Bug 7283 - the following code cannot handle the case where multiple
	// fields of struct are used in a region. Only the field pointed to by
	// the pragma offset will be replaced.
	if (Run_autopar)
	    Rewrite_Pragmas_On_Structs (NULL, WN_func_body(pu));
#endif
    }

    /* First round output (.N file, w2c, w2f, etc.) */
    Set_Error_Phase ( "Post LNO Processing" );
    Post_LNO_Processing (current_pu, pu);
    if (!Run_wopt && !Run_cg) return;

#ifdef KEY // bug 7741
    if (Run_lno && Run_wopt) {
      // Adapted from code in PU_adjust_addr_flags()
      // Some LDAs may have been removed in preopt, so recompute
      // addr_saved flag.
      Clear_local_symtab_addr_flags(Scope_tab[CURRENT_SYMTAB]);
      Recompute_addr_saved_stmt (pu);
      if (!PU_ftn_lang (Get_Current_PU()))
        Set_addr_saved_stmt(pu, CXX_Alias_Const ||
           (OPT_IPA_addr_analysis && PU_ipa_addr_analysis(Get_Current_PU())));
    }
#endif

    Verify_SYMTAB (CURRENT_SYMTAB);

    /* If no early mp processing has been requested, then do it after running
     * lno/preopt. Do this whether or not wopt is to be run.
     * If we were just running LNO,
     * then the above call to Post_LNO_Processing has written out the
     * WHIRL file and thereby mangled current_pu, so we cannot do the
     * MP lowering.
     * To support an option to see post-LNO, post MP-lowered output,
     * maybe we can just move the call to the lowerer before
     * Post_LNO_Processing.
     */
    has_mp = PU_has_mp (Get_Current_PU ());
    if (has_mp && !Early_MP_Processing) {
	Set_Error_Phase ( "MP Lowering" );
        WB_LWR_Initialize(pu, NULL);
	pu = WN_Lower (pu, LOWER_MP, NULL, "Before MP Lowering");
#ifdef KEY
// Bug 4411
        extern void Post_MP_Processing (WN *);
        Post_MP_Processing (PU_Info_tree_ptr(Current_PU_Info));
#endif
        WB_LWR_Terminate();
    }

#ifdef KEY
    if (PU_cxx_lang (Get_Current_PU()))
#endif
    Update_EHRegion_Inito (pu);

#if defined(TARG_IA64)
    /* Generate EH range table for PU.  The high and low labels are
     * filled in during code generation.
     */
    // trace the info of updated EH INITO      
    if (Get_Trace (TP_EH, 0x0004)) {
      fprintf (TFile, "=======================================================================\n");
      fprintf (TFile, "\t  Used EH INITO info for PU: %s \t\n", 
		      ST_name (current_pu->proc_sym));
      fprintf (TFile, "\t  (After Update_EHRegion_Inito) \t\n");
      fprintf (TFile, "=======================================================================\n");
      EH_Dump_INITOs (pu, TFile);
    }
#endif

    if (Run_cg) 
	EH_Generate_Range_List(pu);

    BOOL save_run_wopt = Run_wopt;
    // I believe I can assert that WN_operator(pu) == OPR_FUNC_ENTRY
    // here, but I won't, simply because I don't need to. Might as
    // well check. -- RK 990713
    if (Run_wopt &&
	(WN_operator(pu) == OPR_FUNC_ENTRY) &&
	ST_asm_function_st(*WN_st(pu))) {
      // Throttle WOPT; we don't want it for PU's that merely wrap up
      // file-scope asm statements.
      Run_wopt = FALSE;
    }

    Set_Error_Phase ( "WOPT/CG Processing" );
    pu = Do_WOPT_and_CG_with_Regions (current_pu, pu);

    Run_wopt = save_run_wopt;

    Set_Error_Phase ( "Post Backend Processing" );
    Post_Process_Backend (current_pu, pu);

#ifdef KEY
    if (need_options_pop)
      Options_Stack->Pop_Current_Options();
#endif
#ifdef KEY // bug 9651
    WN_Reset_Num_Delete_Cleanup_Fns();
#endif
} /* Backend_Processing */

#if defined(TARG_SL)
BOOL Walk_And_Insert_Init_Buf(WN* wn, WN* block)
{  
  OPERATOR opr=WN_operator(wn);
  if( opr == OPR_PRAGMA && WN_pragma( wn ) == WN_PRAGMA_PREAMBLE_END ) {
    TY_IDX ty;
    ST *st;

    ty = Make_Function_Type( MTYPE_To_TY( MTYPE_V ) );
    
    if(Sl2_Ibuf_Name==NULL)		
      st = Gen_Intrinsic_Function( ty, "cmp_init_sl_buf" );
    else
      st = Gen_Intrinsic_Function( ty, Sl2_Ibuf_Name );
	
    Clear_PU_no_side_effects( Pu_Table[ST_pu( st )] );
    Clear_PU_is_pure( Pu_Table[ST_pu( st )] );
    Set_PU_no_delete( Pu_Table[ST_pu( st )] );

    WN *call_initbuf = WN_Call( MTYPE_V, MTYPE_V, 0, st );
    WN_Set_Call_Default_Flags(call_initbuf );		
    WN_INSERT_BlockBefore(block, wn, call_initbuf);
    return TRUE;
  }
  else if(opr==OPR_BLOCK) {
    for ( WN* stmt = WN_first( wn ); stmt; stmt = WN_next( wn) )
      if(Walk_And_Insert_Init_Buf( stmt, wn ))
        return TRUE;
  }
  else {
    for ( INT32 i = 0; i < WN_kid_count( wn ); i++ )
      if(Walk_And_Insert_Init_Buf( WN_kid( wn, i ), wn ))
        return TRUE;
  }
  return FALSE;

}
#endif

static WN *
Preprocess_PU (PU_Info *current_pu)
{
  WN *pu = NULL;

  Initialize_PU_Stats ();  /* Needed for Olimit as well as tracing */

  Current_PU_Info = current_pu;
  MEM_POOL_Push(MEM_pu_nz_pool_ptr);
  MEM_POOL_Push(MEM_pu_pool_ptr);

  Cur_PU_Feedback    = NULL;

  BOOL is_mp_nested_pu = FALSE;

  /* read from mmap area */
  Start_Timer ( T_ReadIR_CU );

#ifdef TARG_LOONGSON
  if (Instrumentation_Enabled)
    Instrumentation_Enabled_Before = TRUE;
#endif
  // The current PU could already be memory as happens when the
  // compiler creates it during back end compilation of an earlier PU. 
  if (PU_Info_state (current_pu, WT_TREE) != Subsect_InMem) {
    Read_Local_Info (MEM_pu_nz_pool_ptr, current_pu);
    if (PU_Info_state (current_pu, WT_FEEDBACK) == Subsect_InMem) {
	const Pu_Hdr* pu_hdr = (const Pu_Hdr*)
	    PU_Info_feedback_ptr (current_pu);
#ifdef KEY
	Cur_PU_Feedback = CXX_NEW (FEEDBACK (PU_Info_tree_ptr (current_pu),
					     MEM_pu_nz_pool_ptr,
					     pu_hdr->pu_num_inv_entries,
					     pu_hdr->pu_num_br_entries,
					     pu_hdr->pu_num_loop_entries,
					     pu_hdr->pu_num_scircuit_entries,
					     pu_hdr->pu_num_call_entries,
					     pu_hdr->pu_num_icall_entries,
					     pu_hdr->pu_num_switch_entries,
					     pu_hdr->pu_num_value_entries,
					     pu_hdr->pu_num_value_fp_bin_entries,
					     pu_hdr->runtime_fun_address),
				   MEM_pu_nz_pool_ptr);
#else
	Cur_PU_Feedback = CXX_NEW (FEEDBACK (PU_Info_tree_ptr (current_pu),
					     MEM_pu_nz_pool_ptr,
					     pu_hdr->pu_num_inv_entries,
					     pu_hdr->pu_num_br_entries,
					     pu_hdr->pu_num_loop_entries,
					     pu_hdr->pu_num_scircuit_entries,
					     pu_hdr->pu_num_call_entries,
					     pu_hdr->pu_num_icall_entries,
					     pu_hdr->pu_num_switch_entries),
				   MEM_pu_nz_pool_ptr);
#endif
	Read_Feedback_Info (Cur_PU_Feedback, PU_Info_tree_ptr (current_pu),
			    *pu_hdr);
	// turn off other feedback I/O
	Instrumentation_Enabled = FALSE;
	BZERO (Feedback_Enabled, (PROFILE_PHASE_LAST-1) * sizeof(BOOL));
    } else
	Cur_PU_Feedback = NULL;
  } else {			    /* retrieve transferred maps */
      // change some globals to define current_pu as the current PU
    Current_Map_Tab = PU_Info_maptab(current_pu);
    Current_pu = &PU_Info_pu(current_pu);
    CURRENT_SYMTAB = PU_lexical_level(*Current_pu);
    if ((PU_is_nested_func(*Current_pu) && PU_mp(*Current_pu)) ||
        Is_Set_PU_Info_flags(current_pu, PU_IS_DRA_CLONE)) {
      is_mp_nested_pu = TRUE;
      // hack to restore nested PU's symtab
      Restore_Local_Symtab(current_pu);

      if (PU_Info_state (current_pu, WT_FEEDBACK) == Subsect_InMem) {
              // Restore FEEDBACK object allocated by MP lowerer
	  Cur_PU_Feedback = (FEEDBACK *) PU_Info_feedback_ptr (current_pu);
	  Is_True(Cur_PU_Feedback, ("invalid PU_Info for feedback"));
              // turn off other feedback I/O
	  Instrumentation_Enabled = FALSE;
          BZERO(Feedback_Enabled, (PROFILE_PHASE_LAST-1) * sizeof(BOOL));
      } else
          Cur_PU_Feedback = NULL;
#ifdef KEY
    } else if (Is_Set_PU_Info_flags(current_pu, PU_IS_PROFILER)) {
      Restore_Local_Symtab(current_pu);
      Cur_PU_Feedback = NULL; 
#endif
    } else {
      Is_True(FALSE, ("Robert doesn't understand where symtabs come from"));
    }
  }

  BE_symtab_alloc_scope_level(CURRENT_SYMTAB);
  Scope_tab[CURRENT_SYMTAB].st_tab->Register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);

  /* NOTE: "pu" is not defined until this point, since the actual
   * (WN *) is calculated by Read_Local_Info().
   */
  pu = PU_Info_tree_ptr(current_pu);

  /* store original pu name */
  Orig_PU_Name = Get_Orig_PU_Name(current_pu);
  Save_Cur_PU_Name(ST_name(PU_Info_proc_sym(current_pu)), 0);

#ifdef KEY // ST_name can be deallocated later
  Set_Current_PU_For_Trace(Cur_PU_Name, 
			   Current_PU_Count());
#else
  Set_Current_PU_For_Trace(ST_name(PU_Info_proc_sym(current_pu)), 
			   Current_PU_Count());
#endif

  Stop_Timer (T_ReadIR_CU);

  /* If -tf is set then IR_dump_wn_addr and IR_dump_wn_id
   * won't be set in Configure().
   */
  if ( Get_Trace( TP_MISC, 0x200 ) ) {
     IR_dump_wn_addr = TRUE;
  }
  if ( Get_Trace( TP_MISC, 0x400 ) ) {
     IR_dump_wn_id = TRUE;
  }

  Check_for_IR_Dump(TP_IR_READ,pu,"IR_READ");

  if (Show_Progress) {
    fprintf(stderr, "Compiling %s(%d)\n",
	    ST_name(PU_Info_proc_sym(current_pu)),
	    Current_PU_Count());
  }

  if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
    fprintf(TFile,"===== BE driver, PU loop: PU %s(%d)\n",
	    ST_name(PU_Info_proc_sym(current_pu)),Current_PU_Count());
  }

  if (Tlog_File) {
    fprintf(Tlog_File,"BEGIN %s\n",ST_name(PU_Info_proc_sym(current_pu)));
  }

  WN_Mem_Push ();

  if (Run_wopt || Run_cg) {	    /* PU level initialization for lowerer */
    Lowering_Initialize();
  }

#ifdef KEY
  if (Early_Goto_Conversion &&
       // bug 14188: by default, disabled for fortran
      (!PU_ftn_lang(Get_Current_PU()) || Early_Goto_Conversion_Set))
  {
      GTABLE goto_table( pu, MEM_pu_pool_ptr );
      goto_table.Remove_Gotos();
      // goto_table gets destructed here
  }
#endif

#if defined (TARG_SL)
  if(Sl2_Inibuf) {
    if( strcmp( Cur_PU_Name, "MAIN__" ) == 0 ||
	  strcmp( Cur_PU_Name, "main" ) == 0 ) {
      WN* body=WN_func_body(pu);
      WN* stmt=NULL;
      for (stmt = WN_first( body ); stmt; stmt = WN_next( stmt ) ) {
        if(Walk_And_Insert_Init_Buf( stmt, body ))	  
          break;
      }

      FmtAssert(stmt!=NULL, ("Insert_Init_Buf failed\n")); 
    }
  }
#endif

  /* Add instrumentation here for vho lower. */
  if ( Instrumentation_Enabled
       && (Instrumentation_Type_Num & WHIRL_PROFILE)
       && (Instrumentation_Phase_Num == PROFILE_PHASE_BEFORE_VHO)) {
    if (!is_mp_nested_pu )
      WN_Instrument(pu, PROFILE_PHASE_BEFORE_VHO); 
  } else if ( Feedback_Enabled[PROFILE_PHASE_BEFORE_VHO] ) {
    WN_Annotate(pu, PROFILE_PHASE_BEFORE_VHO, &MEM_pu_pool);
  }

#ifdef KEY
  /* Insert __cyg_profile_func_enter/exit instrumentation (Bug 570) */
  if ( OPT_Cyg_Instrument > 0 && ! Run_ipl &&
       ( ! PU_no_instrument(Get_Current_PU()) ||
	 PU_has_inlines(Get_Current_PU()) ) ) {
    Set_Error_Phase ( "CYG Instrumenting" );
    CYG_Instrument_Driver( pu );
  }
#endif

  Set_Error_Phase ( "VHO Processing" );
  pu = VHO_Lower_Driver (current_pu, pu);

  if ( Cur_PU_Feedback ) {
    Cur_PU_Feedback->Verify("after VHO lower");
  }

  pu = Adjust_Opt_Level (current_pu, pu, ST_name(PU_Info_proc_sym(current_pu)));

  if (wopt_loaded) {
    Create_Restricted_Map(MEM_pu_nz_pool_ptr);
  }

  /* Always create region pool because there are many 
   * places where they can be introduced. Needed for PUs 
   * with no regions also */
  /* NOTE: part of what REGION_initialize does can be moved
   * to when the .B file is read in. */
  REGION_Initialize (pu, PU_has_region (Get_Current_PU ()));
  return pu;
} /* Preprocess_PU */

static void
Postprocess_PU (PU_Info *current_pu)
{
  if (Tlog_File) {
    fprintf (Tlog_File, "END %s\n", ST_name(PU_Info_proc_sym(current_pu)));
  }

  if (Run_ipl != 0 && (Run_wopt || Run_preopt))
    choose_from_complete_struct_for_relayout_candidates(); // among all the
    // structures marked by ipl while compiling all the functions in this file,
    // choose the most profitable one

  Current_Map_Tab = PU_Info_maptab(current_pu);
 
  REGION_Finalize();

  if (Run_wopt || Run_cg) {
    // delete lowering map
    Lowering_Finalize();
  }

  // Delete alias manager after CG finished ? PV 525127, 527977
  WN_Mem_Pop (); // WN pool

  if (wopt_loaded) {
    Delete_Restricted_Map();
  }

  SYMTAB_IDX scope_level = PU_lexical_level(PU_Info_pu(current_pu));

  Scope_tab[scope_level].st_tab->
    Un_register(*Be_scope_tab[scope_level].be_st_tab);
  Be_scope_tab[scope_level].be_st_tab->Clear();

  Free_Local_Info(current_pu); // deletes all maps
  MEM_POOL_Pop(MEM_pu_nz_pool_ptr);
  MEM_POOL_Pop(MEM_pu_pool_ptr);

} /* Postprocess_PU */

/* compile each PU through all phases before going to the next PU */
static void
Preorder_Process_PUs (PU_Info *current_pu)
{
  INT orig_opt_level = Opt_Level;
  BOOL orig_run_lno = Run_lno;
  BOOL orig_run_preopt = Run_preopt;
  BOOL orig_run_wopt = Run_wopt;
  BOOL orig_olimit_opt = Olimit_opt;

  WN *pu;
#ifdef TARG_X8664
  if (!Force_Frame_Pointer_Set)
  {
    const PU & func =
        Pu_Table [ST_pu (St_Table [PU_Info_proc_sym (current_pu)])];

    // C++ PU having exception regions, or with -g
    if (Debug_Level > 0 
#ifdef KEY
        || PU_has_goto_outer_block(func)
#endif
       )
      Force_Frame_Pointer = true;
    else
      Force_Frame_Pointer = false;
  }
#endif

  Start_Timer(T_BE_PU_CU);

  pu = Preprocess_PU(current_pu);

  // Quick! Before anyone risks creating any PREGs in the back end,
  // register the back end's PREG table with the main PREG table so
  // they will grow together as PREGs are created.
  Scope_tab[CURRENT_SYMTAB].preg_tab->Register(Be_preg_tab);

  WN_verifier(pu);

  Verify_SYMTAB (CURRENT_SYMTAB);

  if (!PU_mp (Get_Current_PU ()) &&
      (Run_Dsm_Cloner || Run_Dsm_Common_Check || Run_Dsm_Check))
    DRA_Processing(current_pu, pu, Cur_PU_Feedback != NULL);

  /* If SYMTAB_IPA_on is set then we have run ipl,
   * and therefore already done OMP_prelowering.
   * So don't do it again.
   */
  if (PU_has_mp (Get_Current_PU ()) && !FILE_INFO_ipa (File_info)) {
    Set_Error_Phase("OMP Pre-lowering");
    WB_OMP_Initialize(pu);
    pu = OMP_Prelower(current_pu, pu);
    WB_OMP_Terminate(); 
  }

  if (Run_ipl) {
    Ipl_Processing (current_pu, pu);
    Verify_SYMTAB (CURRENT_SYMTAB);
  }
  else {
    Backend_Processing (current_pu, pu);
    Verify_SYMTAB (CURRENT_SYMTAB);
  }
  if (reset_opt_level) {
    Opt_Level = orig_opt_level;
    Run_lno = orig_run_lno;
    Run_preopt = orig_run_preopt;
    Run_wopt = orig_run_wopt;
    reset_opt_level = FALSE;
    Olimit_opt = orig_olimit_opt;
  }

  Scope_tab[CURRENT_SYMTAB].preg_tab->Un_register(Be_preg_tab);
  Be_preg_tab.Clear();

  Stop_Timer(T_BE_PU_CU);
  Finish_BE_Timing ( Tim_File, ST_name(PU_Info_proc_sym(current_pu)) );
  Advance_Current_PU_Count();

  Cur_PU_Name = NULL;		// memory will not be leaked; eventual
                                // pop occurs in Postprocess_PU's call
                                // to WN_MEM_Pop. Reset here is
                                // required so Save_Cur_PU_Name will
                                // not misbehave.

  // Print miscellaneous statistics to trace file:
  Print_PU_Stats ();

#ifdef KEY // nested functions are no longer in the global symtab
  ST *st;
  INT i;
  FOREACH_SYMBOL (CURRENT_SYMTAB, st, i)
    if (ST_class(st) == CLASS_FUNC)
      Allocate_Object(st);
#endif

  // Now recursively process the child PU's.

  for (PU_Info *child = PU_Info_child(current_pu);
       child != NULL;
       child = PU_Info_next(child)) {
    Preorder_Process_PUs(child);
  }

  Postprocess_PU (current_pu);
} /* Preorder_Process_PUs */

static void Print_Tlog_Header(INT argc, char **argv)
{
  INT i;
  if (Get_Trace(TP_PTRACE1, TP_PTRACE1_NOHDR))
    return; 
  fprintf(Tlog_File,"1.0\n"); /* initial version number */
  fprintf(Tlog_File,"{ ");
  for (i=0; i<argc; i++)
    fprintf(Tlog_File,"%s ", argv[i]);
  fprintf(Tlog_File,"}\n");
}


#define FEEDBACK_PATH_MAXLEN 1024


static void
Process_Feedback_Options (OPTION_LIST* olist)
{
  if (Feedback_File_Name) {
    // this is the case where feedback files are specified implicitly.
    // We need to search the directory

    // Find last '/' in path
    INT t = strlen(Feedback_File_Name);
    Is_True(t < FEEDBACK_PATH_MAXLEN - 16,
	    ("Process_Feedback_Options: Feedback file name too long(%d)",
	     t));
    while (t > 0 && Feedback_File_Name[t - 1] != '/')
      t--;

    // Separate Feedback_File_Name into directory(path) and a file
    // name prefix.  The last character in path must be '/', so that
    // feedback file names can be appended.
    char path[FEEDBACK_PATH_MAXLEN];
    char *prefix = Feedback_File_Name + t;
    if (t > 0) {
      // Split at the '/'
      strncpy(path, Feedback_File_Name, t);
    } else {
      // No '/' in Feedback_File_Name; use "./" for current directory
      path[0] = '.';
      path[1] = '/';
      t = 2;
    }
    path[t] = '\0';
    char *dir_end = path + t;

    INT prefix_len = strlen(prefix);
    DIR* dirp = opendir(path);
#ifdef KEY
    if (dirp == NULL)
      ErrMsg(EC_FB_No_File, Feedback_File_Name);
#endif
    struct dirent* direntp;
#ifdef KEY
    INT fb_file_count = 0;
#endif
    while ((direntp = readdir(dirp)) != NULL) {
      if (strncmp(direntp->d_name, prefix, prefix_len) == 0) {
	strcpy(dir_end, direntp->d_name);
	Process_Feedback_File(path);
#ifdef KEY
	fb_file_count++;
#endif
      }
    }
    closedir(dirp);

#ifdef KEY	// bug 4837
    if (fb_file_count == 0) {
      ErrMsg(EC_FB_No_File, prefix);
    }
#endif
  }

  OPTION_LIST *ol;
  for (ol = olist; ol != NULL; ol = OLIST_next(ol)) {
    char *val = OLIST_val(ol);
    Process_Feedback_File(val);
  }
} // Process_Feedback_Options

// Provide a place to stop after components are loaded
extern "C" {
  void be_debug(void) {}
}

INT
main (INT argc, char **argv)
{
  INT local_ecount, local_wcount;
  PU_Info *pu_tree;
#if defined(TARG_SL)   || defined(TARG_MIPS)
  void *handle;
#endif  
#ifdef __MINGW32__
  setvbuf(stdout, (char *)NULL, _IOLBF, 0);
  setvbuf(stderr, (char *)NULL, _IOLBF, 0);
#else
  setlinebuf (stdout);
  setlinebuf (stderr);
#endif
  Handle_Signals ();
  MEM_Initialize ();
  Cur_PU_Name = NULL;
  Init_Error_Handler ( 100 );
#if !defined(SHARED_BUILD)
  Set_Error_Tables ( Phases, host_errlist );
#endif
  Set_Error_Line ( ERROR_LINE_UNKNOWN );
  Set_Error_File ( NULL );
  Set_Error_Phase ( "Back End Driver" );
  Set_Error_Descriptor (EP_BE, EDESC_BE);
  Set_Error_Descriptor (EP_CG, EDESC_CG);

  Preconfigure ();
  Process_Command_Line (argc, argv);

  if (Inhibit_EH_opt && Opt_Level > 1) Opt_Level = 1;
  Reset_Timers ();
  Start_Timer(T_BE_Comp);
  Prepare_Source ();
  Initialize_Stats ();
  Configure ();
  Configure_Source(NULL); /* Most configuration variables are set here */
#ifdef Is_True_On
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    MEM_Tracing_Enable();
  }
#endif
  if ( List_Enabled ) {
    Prepare_Listing_File ();
    List_Compile_Options ( Lst_File, "", FALSE, List_All_Options, FALSE );
  }

  Init_Operator_To_Opcode_Table();

  /* decide which phase to call */
  load_components (argc, argv);
  be_debug();

  // process the component internal options
  O64_Driver::GetInstance()->ProcessComponentOption();
  
  MEM_POOL_Push (&MEM_src_pool);
  MEM_POOL_Push (&MEM_src_nz_pool);
  if ( Show_Progress ) {
    fprintf ( stderr, "Compiling %s (%s) -- Back End\n",
	     Src_File_Name, Irb_File_Name );
    fflush ( stderr );
  }
  Set_Error_Source (Src_File_Name);

  // Push initial file level options
  Options_Stack = CXX_NEW(OPTIONS_STACK(&MEM_src_nz_pool), &MEM_src_nz_pool);
  Options_Stack->Push_Current_Options();

  Start_Timer (T_ReadIR_Comp);
  if (Read_Global_Data) {
	// get input from two separate files
  	Irb_File = (FILE *)Open_Global_Input (Global_File_Name);
  	Irb_File = (FILE *)Open_Local_Input (Irb_File_Name);
  }
  else {
  	Irb_File = (FILE *)Open_Input_Info (Irb_File_Name);
  }
  Initialize_Symbol_Tables (FALSE);
  New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);

  INT pu_num;
  pu_tree = Read_Global_Info (&pu_num);

#if defined(TARG_SL)
  if (Run_ipisr) {
    isr_cg = CXX_NEW_ARRAY(ISR_NODE, pu_num, &MEM_src_nz_pool);
    Read_isr_cg(isr_cg, pu_num);
  }
#endif

  Global_PU_Tree = (void *)pu_tree; // expose this pu_tree to the optimizer
  Stop_Timer (T_ReadIR_Comp);

  Initialize_Special_Global_Symbols ();

  // if compiling an ipa-generated file, do not instrument phases that
  // have already been done at ipl time.
  if (FILE_INFO_ipa (File_info)) {
      if (Instrumentation_Enabled
	  && (Instrumentation_Type_Num & WHIRL_PROFILE)
	  && (Instrumentation_Phase_Num <= PROFILE_PHASE_IPA_CUTOFF)) {
	Instrumentation_Enabled = FALSE;
#ifdef KEY // bug 5684: deleting branches interferes with branch profiling
	WOPT_Enable_Simple_If_Conv = FALSE;
#endif
	// Proactive loop nest transformation interferes with branch profiling.
	WOPT_Enable_Pro_Loop_Fusion_Trans = FALSE;
	WOPT_Enable_Pro_Loop_Interchange_Trans = FALSE;
	WOPT_Enable_Pro_Loop_Ext_Trans = FALSE;
	
	Instrumentation_Phase_Num = PROFILE_PHASE_NONE;
      }
  }

  Process_Feedback_Options (Feedback_Option);

  //
  // Ordinarily DRA_Initialize() would run as part of Phase_Init(),
  // but we need to run it early, right here. 
  // The reason is that this code does a pre-scan to determine whether
  // any cloning might be required, and if so, sets the mp_needs_lno
  // bit in file_info. We need to know that before we do any
  // initialization and setup processing for dra/lego (e.g. loading
  // lno/wopt, doing lego_init, etc.
  //
  if (Run_Dsm_Cloner || Run_Dsm_Common_Check) {
    DRA_Initialize();
  }
  BOOL needs_lno = FILE_INFO_needs_lno (File_info);

  if (needs_lno && !Run_ipl) {
    Run_Distr_Array = TRUE;
    if (!Run_lno && !Run_autopar) {
      /* ipl is not running, and LNO has not been loaded */
      /* Has distributed arrays, so load LNO, initialize, and setup */
      INT phase_argc;
      char **phase_argv;

      if (!Run_wopt && !Run_preopt) {
	/* load wopt */
	Get_Phase_Args (PHASE_WOPT, &phase_argc, &phase_argv);
	load_so ("wopt.so", WOPT_Path, Show_Progress);
	wopt_main (phase_argc, phase_argv, argc, argv);
	wopt_loaded = TRUE;
      }

      Get_Phase_Args (PHASE_LNO, &phase_argc, &phase_argv);
      load_so ("lno.so", LNO_Path, Show_Progress);
      lno_main (phase_argc, phase_argv, argc, argv);
    }
  }

  /* initialize the BE symtab. Note that w2cf relies on the BE_ST */
  /* during Phase_Init and Phase_Fini                             */

  BE_symtab_initialize_be_scopes();
  BE_symtab_alloc_scope_level(GLOBAL_SYMTAB);
  SYMTAB_IDX scope_level;
  for (scope_level = 0;
       scope_level <= GLOBAL_SYMTAB;
       ++scope_level) {
    // No need to deal with levels that don't have st_tab's. Currently
    // this should be only zero.
    if (Scope_tab[scope_level].st_tab != NULL) {
      Scope_tab[scope_level].st_tab->
	Register(*Be_scope_tab[scope_level].be_st_tab);
    }
    else {
      Is_True(scope_level == 0,
	      ("Nonexistent st_tab for level %d", scope_level));
    }
  }

  Phase_Init ();

  if (Tlog_File)
    Print_Tlog_Header(argc, argv);

  if (Run_ipl)
    Preprocess_struct_access();// for field reorder
#ifdef KEY
  if (profile_arcs && pu_tree) {
      Output_Func_Start_Profiler.Set_file_name(Src_File_Name);
      Output_Func_Start_Profiler.Set_pu_tree(&pu_tree);
      Output_Func_Start_Profiler.Generate_Func_Start_Profiler_PU();
      Output_Func_Start_Profiler.Fill_In_Func_Body();
  }
#endif

#if defined(TARG_SL)
  if (!Run_ipisr) {
    for (PU_Info *current_pu = pu_tree;
         current_pu != NULL;
         current_pu = PU_Info_next(current_pu)) {
      Preorder_Process_PUs(current_pu);
    }
  } else {
    INT i = 0;
    for (PU_Info *current_pu = pu_tree;
         current_pu != NULL;
         current_pu = PU_Info_next(current_pu), i++) 
    {
       Merge_Parents_Regset(isr_cg[i]); 
       Preorder_Process_PUs(current_pu);
    }
  }
#else
  for (PU_Info *current_pu = pu_tree;
       current_pu != NULL;
       current_pu = PU_Info_next(current_pu)) {
    Preorder_Process_PUs(current_pu);
  }
#endif

  /* Terminate stdout line if showing PUs: */
  if (Show_Progress) {
    fprintf (stderr, "\n");
    fflush (stderr);
  }
  Phase_Fini ();

#ifdef KEY
  //Bug 10252: move list options to here (the end of BE), so that
  // it can show appropriate target-dependent options
  if ( List_Enabled ) {
    Mhd_Options.Merge_Options(Mhd);
    Prepare_Listing_File ();
    List_Compile_Options ( Lst_File, "", FALSE, List_All_Options, FALSE );
  }
#endif

  /* free the BE symtabs. w2cf requires BE_ST in Phase_Fini */

  Is_True(scope_level == GLOBAL_SYMTAB + 1,
	  ("scope_level must be GLOBAL_SYMTAB + 1, left from earlier loop"));

  do {
    --scope_level;
    // No need to deal with levels that don't have st_tab's. Currently
    // this should be only zero.
    if (Scope_tab[scope_level].st_tab != NULL) {
      Scope_tab[scope_level].st_tab->
	Un_register(*Be_scope_tab[scope_level].be_st_tab);
      Be_scope_tab[scope_level].be_st_tab->Clear();
    }
    else {
      Is_True(scope_level == 0,
	      ("Nonexistent st_tab for level %d", scope_level));
    }
  } while (scope_level != 0);

  BE_symtab_free_be_scopes();

  
  if (need_wopt_output || need_lno_output || need_ipl_output) {
    Write_Global_Info (pu_tree);
    if (need_ipl_output)
      Ipl_Extra_Output (ir_output);
    Close_Output_Info ();
  }
  else if (Emit_Global_Data) {
    Write_Global_Info (NULL);	/* even if dummy pu, don't write any pu's */
    Close_Output_Info ();
  }

  /* Print miscellaneous statistics to trace file: */
  Print_Total_Stats ();
  if ((Opt_Level > 0 || Run_autopar) 
	&& Max_Src_Olimit > Olimit && !Olimit_opt && Show_OPT_Warnings) {
    ErrMsg (EC_File_Olimit_Exceeded, Max_Src_Olimit);
  }
    
  Stop_Timer(T_BE_Comp);
  Finish_Compilation_Timing ( Tim_File, Src_File_Name );

  MEM_POOL_Pop ( &MEM_src_pool );
  MEM_POOL_Pop ( &MEM_src_nz_pool );
#ifdef Is_True_On
	if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
	    fprintf (TFile, "\n%s\tMemory allocation information after be\n", DBar);
	    MEM_Trace ();
	}
#endif

  /* If we've seen errors, note them and terminate: */
  if ( Get_Error_Count ( &local_ecount, &local_wcount ) ) {
    ecount += local_ecount;
  }
 
  if ( ecount > 0 ) {
    Terminate(Had_Internal_Error() ? RC_INTERNAL_ERROR : RC_NORECOVER_USER_ERROR) ;
  }

  if (local_wcount && warnings_are_errors) {
    Terminate(RC_USER_ERROR);
  }

  /* Close and delete files as necessary: */
  Cleanup_Files ( TRUE, FALSE );
  exit ( RC_OKAY );
  /*NOTREACHED*/

} /* main */
