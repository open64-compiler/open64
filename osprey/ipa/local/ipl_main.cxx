/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
/* ====================================================================
 * ====================================================================
 *
 * Module: ipl_main.c
 *
 * Revision history:
 *  20-Sep-94 - Original Version 
 *
 * Description:
 *
 * The local phase driver
 *
 * ====================================================================
 * ====================================================================
 */

/*----------------------------------------------------------
 *          Summary Phase
 * This phase reads in the IR files and collects summary 
 * information
 *----------------------------------------------------------*/
#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>	    // for WHIRL revision number
#include <sys/types.h>		    // for ir_bwrite.h
#if defined(__MINGW32__)
#include <WINDOWS.h>
#endif
#include "defs.h"

#define BACK_END		    // needed by config.h
#include "glob.h"		    // for Show_Progress
#include "flags.h"		    // for Process_Command_Line_Group ()
#include "wn.h"			    // for WHIRL related operations
#include "tracing.h"		    // for Get_Trace()
#include "ir_reader.h"		    // for IR_reader_init()
#include "const.h"		    // for CONST_TAB_SIZE
#include "pu_info.h"		    // for ir_bwrite.h
#include "ir_bwrite.h"		    // for IPA_write_summary ()
#include "cxx_memory.h"		    // for CXX_NEW

#include "ipl_summary.h"	    // for summary info data structures
#include "ipl_summarize.h"	    // for summarization info
#include "ipl_bread_write.h"	    // for IPA_irb_write_summary()
#include "ipl_array_bread_write.h"  // for Init_write_asections
#include "optimizer.h"		    // for struct DU_MANAGER ...
#include "ipl_driver.h"		    // for extern "C" declaration
#include "config.h"		    // for Run_preopt
#include "config_debug.h"
#include "config_opt.h"
#include "config_ipa.h"
#include "ipl_main.h"
#include "ipl_summarize_template.h" // put these two template files
#include "ipl_analyze_template.h"   // last in the include list
#include "ipl_cost_template.h" 	    // execution cost analysis 
#include "ipl_outline.h"	    // outline analysis
#include "wb_ipl.h" 
#include "ipa_section_main.h" 	    // utilities
#include "ipl_elfsym.h"		    // for IPL_Write_Elf_Symtab
#include "../local/init.cxx"        // force include of Ipl_Initializer

/* General progress trace: */
BOOL Trace_IPA = FALSE;
BOOL Trace_Perf = FALSE;

BOOL Debug_On = FALSE;
BOOL DoPreopt = FALSE;
BOOL Do_Const = FALSE;
BOOL Do_Par   = FALSE;
BOOL Do_Split_Commons = TRUE;
BOOL Do_Split_Commons_Set = FALSE;
BOOL Do_Common_Const = FALSE;
BOOL IPL_Enable_Outline = FALSE;
BOOL IPL_Enable_Unknown_Frequency = FALSE;
#if defined(__linux__) || defined(BUILD_OS_DARWIN)
BOOL IPL_Generate_Elf_Symtab = TRUE;
#else
BOOL IPL_Generate_Elf_Symtab = FALSE;
#endif // __linux__
#ifdef KEY
UINT32 IPL_Ignore_Small_Loops = 0;
#endif

// temporary placeholder until feedback is fixed
// BOOL FB_PU_Has_Feedback = FALSE;

mUINT8 Optlevel = 0;

static INT driver_argc = 0;
static char **driver_argv;

static OPTION_DESC Options_IPL[] = {
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "debug",	"",
      0, 0, 0,	&Debug_On,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "const",	"",
      0, 0, 0,	&Do_Const,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "par",	"",
      0, 0, 0,	&Do_Par,	NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "outline", "",
      0, 0, 0,  &IPL_Enable_Outline, NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "unknown", "",
      0, 0, 0,  &IPL_Enable_Unknown_Frequency, NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "elf_symtab", "",
      0, 0, 0,	&IPL_Generate_Elf_Symtab, NULL},
#ifdef KEY
    { OVK_UINT32, OV_INTERNAL,	FALSE, "ignore_small_loops", "",
      0, 0, UINT32_MAX, &IPL_Ignore_Small_Loops, NULL},
#endif
    { OVK_COUNT }		    // List terminator -- must be last
};


OPTION_GROUP IPL_Option_Groups[] = {
    { "IPL", ':', '=', Options_IPL },
    { NULL }			    // List terminator -- must be last
};


INT    ICALL_MAX_PROMOTE_PER_CALLSITE = 2;

SUMMARY *Summary;			// class for all the summary work
#ifdef SHARED_BUILD
WN_MAP Parent_Map;
#endif
WN_MAP Summary_Map;
WN_MAP Stmt_Map;

DYN_ARRAY<char*>* Ipl_Symbol_Names = NULL; 
DYN_ARRAY<char*>* Ipl_Function_Names = NULL; 

/* ====================================================================
 *
 * Process_Command_Line
 *
 * Process the command line arguments.  Evaluate all flags except per-
 * source file control flags and set up global options.
 *
 * ====================================================================
 */
static void
Process_Command_Line (INT argc, char **argv)
{
    INT i;

    for (i = 0; i < argc; i++) {
	if (argv[i] != NULL && *(argv[i]) == '-') {

	    char *arg_str = argv[i];
	    if (Process_Command_Line_Group (arg_str+1,
					    IPL_Option_Groups))
	      {
		continue;
	      }
	    if (strcmp (arg_str, "-cmds") == 0) {
		driver_argc = argc - i - 1;
		if (driver_argc > 0)
		    driver_argv = argv + i + 1;
		i = argc;	    /* stop processing */
	    }
	}		   
    }

    if (OPT_Reorg_Common_Set)
      Do_Split_Commons = OPT_Reorg_Common;

    Do_Split_Commons_Set = OPT_Reorg_Common_Set && OPT_Reorg_Common;

    Do_Par = IPA_Enable_Array_Summary && Run_preopt;

    if (Do_Par)
      WOPT_Enable_Generate_DU = TRUE;

    Do_Common_Const = IPA_Enable_Common_Const && Run_preopt;

} // Process_Command_Line

void
ipl_main (INT ipl_argc, char **ipl_argv)
{
    extern char *Whirl_Revision;

    if (strcmp (Whirl_Revision, WHIRL_REVISION) != 0)
	FmtAssert (!DEBUG_Ir_Version_Check,
		   ("WHIRL revision mismatch between be.so (%s) and ipl.so (%s)", 
		    Whirl_Revision, WHIRL_REVISION));
    
    /* Perform preliminary command line processing: */
    Process_Command_Line (ipl_argc, ipl_argv);
    Optlevel = Opt_Level;
} // ipl_main


/* Initialization that needs to be done after the global symtab is read */
void
Ipl_Init (void)
{
    Set_Error_Phase ( "Ipl Initialization" );

    Summary = CXX_NEW (SUMMARY(Malloc_Mem_Pool), Malloc_Mem_Pool);

    // information for writing out array sections
    Init_write_asections (Malloc_Mem_Pool);

} /* Ipl_Init */


/* Initialization of IPL when it's called from IPA */
void
Ipl_Init_From_Ipa (MEM_POOL* pool)
{
  Summary = CXX_NEW (SUMMARY(pool), pool);
  Init_write_asections (pool);
} /* Ipl_Init_From_Ipa */


/*-----------------------------------------------------------------*/
/* entry point into the local phase                                */
/*-----------------------------------------------------------------*/
void
Perform_Procedure_Summary_Phase (WN* w, struct DU_MANAGER *du_mgr,
				 struct ALIAS_MANAGER *alias_mgr,
				 void *emitter)
{
    Trace_IPA = Get_Trace (TP_IPL, TT_IPL_IPA);

    if ( Debug_On )
	IR_reader_init();

    if (IPL_Enable_Outline) {
	const WN* wn = Outline_Split_Point (w, IPA_PU_Minimum_Size,
					    IPA_Small_Callee_Limit / 2);
	if (wn) {
	    fprintf (TFile, "Splitting %s:\n", ST_name (WN_st (w)));
	    fdump_tree (TFile, const_cast<WN*> (wn));
	}
    }

    if (Trace_IPA) {
	fprintf ( TFile, "Summarizing procedure %s \n", ST_name(WN_st(w)) );
    }

    DoPreopt = Run_preopt;
    if (Run_preopt && Cur_PU_Feedback) {
	BOOL not_pass = Cur_PU_Feedback->Verify ("IPL");
	if (not_pass) //FB_VERIFY_CONSISTENT = 0 
	    DevWarn ("Feedback verify fails after preopt");
    }
    
    WB_IPL_Set_Scalar_Summary(Summary);
    WB_IPL_Set_Array_Summary(NULL);
    Summary->Set_du_mgr (du_mgr);
    Summary->Set_alias_mgr (alias_mgr);
    Summary->Set_emitter ((EMITTER *) emitter);
    Summary->Summarize (w);
    WB_IPL_Set_Array_Summary(NULL);
    WB_IPL_Set_Scalar_Summary(NULL);
 
} // Perform_Procedure_Summary_Phase

void
Ipl_Fini (void)
{
    Summary->Set_global_addr_taken_attrib ();
    return;
} // Ipl_Fini


void
Ipl_Extra_Output (Output_File *ir_output)
{
#ifndef KEY
	if(IPA_Enable_Reorder)/*&&Feedback_Enabled[PROFILE_PHASE_BEFORE_VHO] )*/
#endif // !KEY
		Summary->Finish_collect_struct_access();// reorder, free pointers, set hot_flds
    IPA_write_summary(IPA_irb_write_summary, ir_output);

    if ( Get_Trace ( TKIND_IR, TP_IPL ) )
	IPA_Trace_Summary_File ( TFile, ir_output, TRUE, 
	  Ipl_Symbol_Names, Ipl_Function_Names );	

    if (driver_argc > 0)
	WN_write_flags (driver_argc, driver_argv, ir_output);

#if defined(__linux__) || defined(BUILD_OS_DARWIN)
    // write out the Elf version of global symtab
    IPL_Write_Elf_Symtab (ir_output);
#endif // __linux__
    
} // Ipl_Extra_Output
