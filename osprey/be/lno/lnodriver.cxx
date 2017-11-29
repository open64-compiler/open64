/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: lnodriver.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:13-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lnodriver.cxx $
 *
 * Revision history:
 *  08-Sep-94 - Original Version
 *
 * Description:
 *
 * Main driver -- command line processing and file name manipulation --
 * for the Whirl Loop Nest Optimizer (LNO)
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/elf_whirl.h>	    /* for WHIRL_REVISION */
#include <sys/types.h>	 
#include "pu_info.h" 
#include "defs.h"
#include "config.h"
#include "config_lno.h"
#include "config_wopt.h"
#include "config_debug.h"	    /* for DEBUG_Ir_Version_Check */
#include "errors.h"
#include "../lno/init.cxx"          /* force include of Lno_Initializer */
#include "glob.h"		    /* Irb_File_Name, Cur_PU_Name */
#include "timing.h"		    /* Start/Stop Timer */

#include "wn.h"			    /* WHIRL Node descriptor */
#include "stab.h"		    /* for Current_PU */
#include "region_util.h"	    /* prototype for REGION_new_wn */

#include "optimizer.h"		    /* for DU_Manager and Pre_Optimizer */
#include "lnoptimizer.h"	    /* for Lnoptimizer() */
#include "region_main.h"	    /* prototypes for RBI and RAIL */

#include "debug.h"
#include "cxx_template.h" 
#include "parids.h" 
#include "cxx_memory.h"
#include "ipa_lno_file.h"
#include "lnodriver.h"
#include "erglob.h"
#include "file_util.h"
#include "tracing.h"
#include "ir_reader.h"             // for fdump_tree
#ifdef KEY
#include "wn_simp.h"               // for WN_Simp_Rsqrt_Newton_Raphson
#endif


/* ====================================================================
 *
 * main
 *
 * Main entry point and driver for the Mongoose-LNO compiler.
 *
 * ====================================================================
 */


/*ARGSUSED*/
void
lno_main (INT lno_argc, char **lno_argv, INT be_argc, char **be_argv)
{
    extern char *Whirl_Revision;

    if (strcmp (Whirl_Revision, WHIRL_REVISION) != 0)
	FmtAssert (!DEBUG_Ir_Version_Check,
		   ("WHIRL revision mismatch between be.so (%s) and lno.so (%s)", 
		    Whirl_Revision, WHIRL_REVISION));
} /* lno_main */

extern "C" {
extern void Lego_File_Init (void);
extern void Mp_File_Init (void);
extern void Lego_File_Fini (void);
extern void Generate_Runtime_Stuff ();
extern BOOL Run_Dsm_Check;
}

IPA_LNO_READ_FILE* IPA_LNO_File = NULL; 
extern FILE* STDOUT; 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Open_Input_File
// FUNCTION: Open the IPALNO input file with name 'input_file' and return
//   an IPA_LNO_READ_FILE with information describing the memory map of its
//   contents.  Log error message EC_IPALNO_Open or EC_IPALNO_Revision if
//   appropriate.
//-----------------------------------------------------------------------

static IPA_LNO_READ_FILE* IPA_LNO_Open_Input_File(const char* input_file)
{
  Set_Error_Phase("Reading IPA LNO file");
  IPA_LNO_READ_FILE* IPA_LNO_Input_File =
    CXX_NEW(IPA_LNO_READ_FILE(Malloc_Mem_Pool), Malloc_Mem_Pool);
  INT error_code = IPA_LNO_Input_File->Open_Read_File(input_file);
  Input_File* ifl = IPA_LNO_Input_File->ifl;
  if (error_code == IPALNO_ABI_MISMATCH
      || error_code == IPALNO_REVISION_MISMATCH)
    FmtAssert(ifl != NULL, ("Missing IPALNO revision number"));
  if (error_code == IPALNO_FORMAT_ERROR)
    ErrMsg(EC_IPALNO_Open, input_file, errno);
  else if (error_code == IPALNO_ABI_MISMATCH)
    ErrMsg(EC_IPALNO_Revision, ifl->file_revision, input_file);
  else if (error_code == IPALNO_REVISION_MISMATCH)
    ErrMsg( EC_IPALNO_Revision, ifl->file_revision, input_file);
  else if (error_code == IPALNO_READER_ERROR)
    ErrMsg(EC_IPALNO_Open, input_file, errno);
  return IPA_LNO_Input_File;
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Close_Output_Info
// FUNCTION: Unmap the 'IPA_LNO_Input_File'.
//-----------------------------------------------------------------------

static void IPA_LNO_Close_Output_Info(IPA_LNO_READ_FILE* IPA_LNO_Input_File)
{
  Set_Error_Phase("Reading IPA LNO file");
  if (IPA_LNO_Input_File->ifl != NULL)
    IPA_LNO_Input_File->Close_Read_File();
}

/* Initialization that needs to be done after the global symbol is read */
void
Lno_Init (void)
{
    Set_Error_Phase ( "LNO Initialization" );

#ifdef _NEW_SYMTAB
    if ((LNO_Run_Lego_Set && LNO_Run_Lego) ||
        (!LNO_Run_Lego_Set && FILE_INFO_needs_lno(File_info)))
      Lego_File_Init ();
    else if (Run_Dsm_Check) 
      Generate_Runtime_Stuff ();

    if (FILE_INFO_has_mp(File_info)) 
      Mp_File_Init(); 

#else
    if ((LNO_Run_Lego_Set && LNO_Run_Lego) ||
	 (!LNO_Run_Lego_Set && SYMTAB_mp_needs_lno(Global_Symtab)))
      Lego_File_Init ();
    else if (Run_Dsm_Check)
      Generate_Runtime_Stuff ();

    if (SYMTAB_has_mp(Global_Symtab))
      Mp_File_Init();
#endif


    if (Run_autopar && LNO_IPA_Enabled) 
      IPA_LNO_File = IPA_LNO_Open_Input_File("IPA.LNO"); 
} /* Lno_Init */


/* pu: always a pointer to the enclosing PU, included to get error messages
   	and pragmas
   region_wn: sometimes equal to pu, sometimes smaller (olimit)
   alloca: TRUE means ok to use alloca, FALSE means use malloc
*/
WN *
Perform_Loop_Nest_Optimization (PU_Info* current_pu, WN *pu_wn, 
			        WN *region_wn, BOOL alloca)
{
    WN *opt_pu = NULL;
    struct DU_MANAGER *du_mgr;
    struct ALIAS_MANAGER *alias_mgr;
    if (!alloca) LNO_Use_Malloc = TRUE;
    STDOUT = stdout; 

    MEM_POOL_Popper popper(&MEM_local_pool);

    ST *current_pu_st = WN_st(pu_wn);
    Cur_PU_Name = ST_name(current_pu_st);
    if ( Cur_PU_Name ) {
	/* ST_name will return a pointer into the symbol table, which is
	 * mmap-ed. This causes a problem in the error message routines
	 * when an unexpected signal occurs, because as part of the cleanup
	 * files are closed. To fix the problem we just allocate some
	 * memory and make a copy.
	 */
	Cur_PU_Name = strcpy(
	    TYPE_MEM_POOL_ALLOC_N(char, &MEM_local_pool, 
				  strlen(Cur_PU_Name) + 1),Cur_PU_Name);
    }
    /* if we are processing a region, add that to the name */
    if (region_wn != pu_wn) {
      Cur_PU_Name = TYPE_MEM_POOL_ALLOC_N(char, &MEM_local_pool,
					  strlen(Cur_PU_Name) + 9);
      sprintf(Cur_PU_Name,"%s.RGN%04d", ST_name(current_pu_st),
			    RID_id(REGION_get_rid(region_wn)));
    }
  
    Is_True(!WN_Tree_Has_Duplicate_Labels(region_wn, &MEM_local_pool),
            ("region_wn has duplicate labels on entry to LNO"));

    Start_Timer ( T_Preopt_CU );
    Set_Error_Phase ( "Global Optimizer" );

#ifdef KEY
    // Prevent the WN simplifier from adding new WHIRL nodes.  The new nodes
    // will make the WN map out of sync, breaking DU analysis which relies on
    // the WN map.  Bug 9521.
    WN_Simp_Rsqrt_Newton_Raphson = FALSE;
#endif

    du_mgr = Create_Du_Manager(MEM_pu_nz_pool_ptr);
    alias_mgr = Create_Alias_Manager(MEM_pu_nz_pool_ptr,pu_wn);

    STACK<WN*> st_before_wn(&MEM_local_pool); 
    STACK<INT> st_before_id(&MEM_local_pool); 
    STACK<WN*> st_after_wn(&MEM_local_pool); 
    STACK<INT> st_after_id(&MEM_local_pool); 

    region_wn =
      Pre_Optimizer(PREOPT_LNO_PHASE, region_wn, du_mgr, alias_mgr);
    Check_for_IR_Dump(TP_LNOPT3, region_wn, "LNO PREOPT");

    RID_level(REGION_get_rid(region_wn)) = RL_LNO_PREOPT;
    Is_True(REGION_consistency_check(region_wn),(""));

    if (WOPT_Enable_Pro_Loop_Fusion_Trans || WOPT_Enable_Pro_Loop_Interchange_Trans
	|| WOPT_Enable_Pro_Loop_Ext_Trans) {
      Delete_Du_Manager(du_mgr, MEM_pu_nz_pool_ptr);
      du_mgr = Create_Du_Manager(MEM_pu_nz_pool_ptr);
      region_wn =
	// Calling Proactive_Optimizer, which skips optimizations after proactive
	// loop transformation results in a perf regression in hmmer.
#if 1
	Pre_Optimizer(PREOPT_LNO1_PHASE, region_wn, du_mgr, alias_mgr);
#else
	Proactive_Optimizer(PREOPT_LNO1_PHASE, region_wn, du_mgr, alias_mgr);
#endif

      Check_for_IR_Dump(TP_LNOPT3, region_wn, "LNO1 PREOPT");
      RID_level(REGION_get_rid(region_wn)) = RL_LNO1_PREOPT;
      Is_True(REGION_consistency_check(region_wn),(""));
    }

    Stop_Timer ( T_Preopt_CU );
    Start_Timer ( T_LNO_CU );
    Set_Error_Phase ( "Loop Nest Optimizer" );

    WB_LNO_Initialize(region_wn, du_mgr, alias_mgr, WBC_DISABLE); 
    opt_pu = Lnoptimizer (current_pu, region_wn, du_mgr, alias_mgr);
    REGION_new_wn(opt_pu, region_wn); /*preserve RID <-> WHIRL consistency*/
    RID_level(REGION_get_rid(opt_pu)) = RL_LNO;
    Is_True(REGION_consistency_check(opt_pu),(""));
    WB_LNO_Terminate(); 

#ifdef __REGIONS_IN_LNO__
    if (Regions_Around_Inner_Loops && PU_has_region(Get_Current_PU())) {
       Set_Error_Phase ( "Region Around Inner Loops" );
       Rail(opt_pu);	/* regions around inner loops */
    }
#endif

    if (Region_Boundary_Info && PU_has_region(Get_Current_PU())) {
       Set_Error_Phase ( "Region Boundary Info" );
       Region_Bound_Info(opt_pu, du_mgr, alias_mgr); /* boundary info */
    }

    Copy_Restricted_Map(region_wn, alias_mgr);
    Invalidate_Persistent_Alias_Info(alias_mgr, opt_pu);

    Delete_Du_Manager(du_mgr, MEM_pu_nz_pool_ptr);
    Delete_Alias_Manager(alias_mgr, MEM_pu_nz_pool_ptr);
    
    Stop_Timer ( T_LNO_CU );

    Is_True(!WN_Tree_Has_Duplicate_Labels(opt_pu, &MEM_local_pool),
            ("duplicate labels introduced by Perform_Loop_Nest_Optimization"));

    Cur_PU_Name = NULL; /* the pool containing it is about to disappear */

#ifdef KEY
    // Safe for the simplifier to add new WHIRL nodes again.
    WN_Simp_Rsqrt_Newton_Raphson = TRUE;
#endif

    return opt_pu;
} /* Perform_Loop_Nest_Optimization */


void
Lno_Fini (void)
{
#ifdef _NEW_SYMTAB
  if ((LNO_Run_Lego_Set && LNO_Run_Lego) ||
      (!LNO_Run_Lego_Set && FILE_INFO_needs_lno(File_info))) {
#else
  if ((LNO_Run_Lego_Set && LNO_Run_Lego) ||
      (!LNO_Run_Lego_Set && SYMTAB_mp_needs_lno(Global_Symtab))) {
#endif
    Lego_File_Fini ();
    /* This bit is now maintained throughout compilation.
     * Clear_FILE_INFO_needs_lno(File_info) /
     * (used to be Reset_SYMTAB_mp_needs_lno(Global_Symtab));
     */
  }

  if (Run_autopar && LNO_IPA_Enabled)
    IPA_LNO_Close_Output_Info(IPA_LNO_File);
} /* Lno_Fini */
