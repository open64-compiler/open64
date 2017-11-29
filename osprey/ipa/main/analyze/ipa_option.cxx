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
 * Module: ipa_option.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_option.cxx,v $
 *
 * Revision history:
 *  24-Jan-96 - Original Version
 *
 * Description:
 *
 * Handle option processing for IPA.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_option.cxx,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#include <stdint.h>
#include "linker.h"

#pragma weak outfilename	/* So elfdump doesn't barf */

#define USE_STANDARD_TYPES
#include "defs.h"
#include "config.h"
#include "config_opt.h"
#include "erglob.h"		/* Include the error tables */
#include "file_util.h"		/* for creating trace filename */
#include "flags.h"		/* for OPTION_LIST, etc. */
#include "glob.h"		/* for timer output file */
#include "mempool.h"		/* for MEM_Tracing_Enable () */
#include "timing.h"		/* for Initialize_Timing() */
#include "tracing.h"		/* to set up TFile for trace */
#include "strtab.h"		/* for ipc_option.h */
#include "config_lno.h"		// for LNO_Prompl
#include "ipc_option.h"		/* for -INLINE options */
#include "ipa_option.h"		/* to see if trace was specified */

/* Copied from config.c: */
#define MAX_OLIMIT		INT32_MAX

/* IPA file variables declared in glob.h: */
char *Ipa_File_Name = NULL;	/* IPAA summary file name */
FILE *Ipa_File = NULL;		/* IPAA summary file (if open) */

/* Skiplist support from config.h: */
typedef struct skiplist SKIPLIST;
SKIPLIST *Build_Skiplist ( OPTION_LIST *olist );
SKIPLIST *IPA_Skip_List = NULL;	/* List of skip options */

BOOL Trace_IPA = FALSE;		/* Main IPA progress trace */
BOOL Trace_Perf = FALSE;	/* performance trace */
BOOL Trace_IPALNO = FALSE;    /* IPA to LNO correctness trace */

/* flags that do not fall into the option group */
BOOL Verbose = FALSE;
BOOL Demangle = FALSE;

BOOL ProMP_Listing = FALSE;		// TURE means passing
					// -PROMP:next_id=<n> to backend


/* ====================================================================
 *
 * Process_IPA_Options
 *
 * Process the IPA options from an argument list (pre-filtered by ld).
 *
 * ====================================================================
 */

extern "C" void
Process_IPA_Options ( INT argc, char **argv )
{
    int i;

    for (i = 0; i < argc; i++) {
	if (argv[i] != NULL && *(argv[i]) == '-' ) {
	    if (Process_Command_Line_Group (argv[i]+1, Common_Option_Groups))
		continue;

	    switch (argv[i][1]) {

	    case 'I':
		/* We support a -IPA group option with no sub-options, which
		 * won't be recognized by Process_Command_Line_Group, so we
		 * need to tolerate it here to avoid error messages:
		 */
		if ( strcmp ( argv[i], "-IPA" ) != 0 ) {
		    ErrMsg ( EC_Unknown_Flag, argv[i][0], argv[i] );
		}
		break;

	    case 't':
		Process_Trace_Option ( argv[i] );
		break;

	    case 'Y':
		/* Just toss it for now... */
		break;

#ifdef TARG_LOONGSON
            /*add support for 2e and 2f in ipa_link*/
            case 'l':
                 if (strcmp(argv[i],"-loongson2e") == 0) {
                     ld_ipa_opt[LD_IPA_ISA].flag = TOS_LOONGSON_2e; // used TOS_LOONGSON_2e in fake_ld
                 } else if (strcmp(argv[i],"-loongson2f") == 0) {
                    ld_ipa_opt[LD_IPA_ISA].flag = TOS_LOONGSON_2f;
                 } else if (strcmp(argv[i],"-loongson3") == 0) {
                    ld_ipa_opt[LD_IPA_ISA].flag = TOS_LOONGSON_3;
                 }
                 break;
#endif

	    default:			/* What's this? */
		ErrMsg ( EC_Unknown_Flag, argv[i][0], argv[i] );
		break;
	    }
	}
    }

    Configure_IPA();
    
    if (LNO_Prompl)
	ProMP_Listing = TRUE;

    /* Configure Olimit to be max if set to 0: */
    if ( Olimit == 0 ) Olimit = MAX_OLIMIT;

    /* Specfile- and post-processing of -INLINE options: */
    Process_Inline_Options ();

#ifndef _STANDALONE_INLINER
    /* Specfile- and post-processing of GP partition options: */
    Process_IPA_Specfile_Options ();
#endif

    // for -OPT:alias=nystrom
    Configure_Alias_Options();

    /* check consistency for Hard/Soft PU Limits */
    if (IPA_PU_Limit_Set) {
	if (IPA_PU_Hard_Limit_Set) {
	    if (IPA_PU_Hard_Limit < IPA_PU_Limit)
		IPA_PU_Hard_Limit = IPA_PU_Limit;
	} else
	    IPA_PU_Hard_Limit = IPA_PU_Limit + (IPA_PU_Limit >> 2);
    } else if (IPA_PU_Hard_Limit_Set) {
	IPA_PU_Limit = IPA_PU_Hard_Limit;
	IPA_PU_Limit_Set = TRUE;
    }

    if (IPA_Force_Depth_Set) {
	IPA_PU_Limit = UINT32_MAX;
	IPA_Bloat_Factor = UINT32_MAX;
	IPA_Max_Depth = IPA_Force_Depth;
    }

    IPA_Enable_Opt_Alias = FALSE;	/* turn this off for 7.2 */
    /* If WOPT uses alias info, we need to produce it: */
    if ( IPA_Enable_Opt_Alias ) {
	IPA_Enable_Simple_Alias = TRUE;
    }

    if (IPA_Output_File_Size != 0) {
	IPA_Max_Output_File_Size +=
	    IPA_Max_Output_File_Size / 100 * IPA_Output_File_Size;
    }

#if defined(TARG_SL)
    if (ld_ipa_opt[LD_IPA_IPISR].flag) {
        if (IPA_Enable_PU_Reorder != REORDER_DISABLE) 
            DevWarn("IPA_Enable_PU_Reorder is overrided by -ipisr option!"); 
        IPA_Enable_PU_Reorder = REORDER_BY_BFS; 

        if (IPA_Enable_Source_PU_Order) 
            DevWarn("IPA_Enable_Source_PU_Order is overrided by -ipisr option!"); 
        IPA_Enable_Source_PU_Order = FALSE;
    }
#endif

    if ( Get_Trace ( TKIND_ALLOC, TP_IPA ) ) {
	IPA_Enable_Memtrace = TRUE;
	MEM_Tracing_Enable ();
    }

    /* -tt1:1 requests all of the performance trace flags: */
    if ( Get_Trace ( TP_PTRACE1, 1 ) ) {
	Set_Trace ( TP_PTRACE1, 0xffffffff );
	Set_Trace ( TP_PTRACE2, 0xffffffff );
    }

    /* Any IPA TLOG trace flags set? */
    Trace_Perf = Get_Trace (TP_PTRACE1, TP_PTRACE1_INL|TP_PTRACE1_IPA);
    Trace_IPA = Get_Trace ( TP_IPA, IPA_TRACE_IPA );
    Verbose = ld_ipa_opt[LD_IPA_SHOW].flag;
    Demangle = ld_ipa_opt[LD_IPA_DEMANGLE].flag;

    if (ld_ipa_opt[LD_IPA_SHARABLE].flag & F_STATIC) {
	IPA_Enable_Picopt = FALSE;
	IPA_Enable_AutoGnum = FALSE;
    }

    if (!outfilename) {
    	outfilename = (char *)MALLOC(20);
	strcat(outfilename,"a.out");
    }
    /* Build a skip list: */
    IPA_Skip_List = Build_Skiplist ( IPA_Skip );

    /* Get IPAA summary file name if required: */
    if ( IPA_Enable_Simple_Alias && Ipa_File_Name == NULL ) {
	Ipa_File_Name = concat_names ( outfilename, (const string)".ipaa" );
    }

    /* Set up for tracing -- file, timers, etc.: */
    if ( Tracing_Enabled ) {
	char * cmd_file_name = concat_names ( outfilename, (const string)".ipa.t" );

	Set_Trace_File ( cmd_file_name );

	/* -ti64 requests a listing of all the -tt flags: */
	if ( Get_Trace ( TKIND_INFO, TINFO_TFLAGS ) ) {
	    List_Phase_Numbers ();
	}

	/* Initialize timers: */
	if ( Get_Trace ( TKIND_INFO, TINFO_TIME ) ) {
	    Tim_File = TFile;
	    Initialize_Timing ( TRUE );
	}
    }
   
    /* Transformation log file */
    if ( Get_Trace ( TP_PTRACE1, TP_PTRACE1_IPA) ||
	 Get_Trace ( TP_PTRACE1, TP_PTRACE1_IPA_CPROP) ||
	 Get_Trace ( TP_PTRACE1, TP_PTRACE1_IPALNO)) {
	if ( Tlog_File_Name == NULL ) {
	    /* Replace source file extension to get trace file: */
	    Tlog_File_Name =  concat_names ( outfilename, (const string)".tlog" );
	}
	if ( (Tlog_File = fopen ( Tlog_File_Name, "w" ) ) == NULL ) {
	    ErrMsg ( EC_Tlog_Open, Tlog_File_Name, errno );
	    Tlog_File_Name = NULL;
	    Tlog_File = stdout;
	}
    }

    /* Trace options: */
    if ( Get_Trace ( TP_MISC, 128 ) ) {
	Trace_Option_Groups ( TFile, Common_Option_Groups, TRUE );
    } else if ( Trace_IPA || Trace_Perf
	       || Get_Trace ( TP_MISC, 32 ) )
	{
	    Trace_Option_Groups ( TFile, Common_Option_Groups, FALSE );
	}
} /* Process_IPA_Options */
