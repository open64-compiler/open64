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



/*
 *
 * Description:
 *
 * Main driver -- command line processing and file name manipulation --
 * for the IPA Summary Phase
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#include <unistd.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */ 
#include <cmplrs/rcodes.h>	    /* for exit return status */
#ifdef __MINGW32__
#include <WINDOWS.h>
void CG_force_link(void);

#endif /* __MINGW32__ */

#include "defs.h"
#include "config.h"		    /* for Configure() */
#include "config_ipa.h"		    /* for INLINE/IPA options */
#include "flags.h"		    /* for Common_Option_Groups */
#include "file_util.h"		    /* New_Extension() */
#include "glob.h"		    /* for [Tlog|Irb]_File_Name and Tim_File */
#include "err_host.tab"
#include "erglob.h"		    /* Include the error tables */
#include "mempool.h"
#include "wn.h"			    /* for ir_bwrite.h */
#include "pu_info.h"
#include "ir_bwrite.h"		    /* for Close_Output_Info() */
#include "strtab.h"                 /* for ipc_option.h */
#include "ipc_option.h"             /* for -INLINE options */
#include "timing.h"		    /* for Initialize_Timing() */
#include "tracing.h"		    /* for Set_Trace () */
#include "inline.h"

/* Default file	extensions: */
#define	IRB_FILE_EXTENSION ".B"	/* Binary WHIRL IR file */
#define TRC_FILE_EXTENSION ".t"
char* Irb_Output_Name = NULL;

/* Copied from config.c: */
#define MAX_OLIMIT              INT32_MAX

BOOL Verbose = FALSE;

/* This isn't implemented in the standalone inliner (i.e. we don't
 * actually build the list), but we check it, so we need the variable
 * to be defined.
 */
SKIPLIST *IPA_Skip_List = NULL;		/* List of skip options */



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
    INT Src_Count = 0;
    BOOL dashdash_flag = FALSE;

    /* Handle each argument */
    for ( i = 1; i < argc; i++ ) {
        if (argv[i] != NULL && (strcmp(argv[i],"--")==0))  {
	  dashdash_flag = TRUE;
	  continue;
	}
        if ( !dashdash_flag && argv[i] != NULL && *(argv[i]) == '-' ) {

	    if (Process_Command_Line_Group (argv[i]+1, Common_Option_Groups))
		continue;

            switch (argv[i][1]) {

	    case 'c':
		i = argc;	    /* stop processing */
		break;

	    case 'g':
		switch (argv[i][2]) {
		case '0':
		case '1':
		case '2':
		case '3':
		    if (argv[i][3] != 0)
			break;
		    /* else, fall through */
#if ! (defined(linux) || defined(BUILD_OS_DARWIN))
		case 0:
		    if (!IPA_Enable_DFE_Set)
			IPA_Enable_DFE = FALSE;
#endif /* linux */
		}
		break;

            case 'f':
                if (argv[i][2] == 0)
                    ErrMsg (EC_File_Name, '?', argv[i]);
                else if (argv[i][3] != ',' && argv[i][3] != ':')
                    ErrMsg (EC_File_Name, argv[i][2], argv[i]);
                else {
                    switch (argv[i][2]) {
                    case 'B':   /* WHIRL file */
                        Irb_File_Name = argv[i] + 4;
                        break;
		    case 'I':
			Irb_Output_Name = argv[i] + 4;
			break;
 
                    default:
                        ErrMsg ( EC_File_Flag, argv[i][2], argv[i] );
                        break;
                    }
                }
                break;

	    case 'm':		    /* accept and ignore the -m1, -m0 flags */
		break;

	    case 's':
		if (strcmp (argv[i]+1, "show") == 0)
		    Verbose = TRUE;
		else
		    ErrMsg ( EC_Unknown_Flag, argv[i][0], argv[i] );
		break;

	    case 't':
		Process_Trace_Option ( argv[i] );
		break;

	    case 'O':		    /* accept and ignore the -O level */
		switch (argv[i][2]) 
		    {
		    case '3':
			Opt_Level = 3;
			INLINE_Enable_Split_Common = TRUE;
			break;
		    case '2':
			Opt_Level = 2;
			INLINE_Enable_Split_Common = FALSE;
			break;
                    case '0': /* setup O0 for lw_inline for gcc compatible*/
                        Opt_Level = 0;
                        INLINE_Enable_Split_Common = FALSE;
			break;
		    default:
			Opt_Level = 1;
			INLINE_Enable_Split_Common = FALSE;
			break;
		    }
		break;

	    case 'P':
		break;		    /* ignore the -PHASE: option */

	    case 'd':		    /* accept and ignore the -dsm flags */
		if (strncmp (argv[i]+1, "dsm", 3) != 0)
                  ErrMsg ( EC_Unknown_Flag, argv[i][0], argv[i] );
		break;

            default:     /* What's this? */
                ErrMsg ( EC_Unknown_Flag, argv[i][0], argv[i] );
                break;
            }
        } else if (argv[i] != NULL) {
	    dashdash_flag = FALSE;
            Src_Count++;
            Src_File_Name = argv[i];
        }
    }

    /* Configure Olimit to be max if set to 0: */
    if ( Olimit == 0 ) Olimit = MAX_OLIMIT;

    /* Specfile- and post-processing of -INLINE options: */
    Process_Inline_Options ();
    
    IPA_Enable_BarrierFarg = FALSE;


    if ( Get_Trace ( TKIND_ALLOC, TP_INLINE ) ) {
      MEM_Tracing_Enable ();
    }

    if (Src_Count == 0) {
        ErrMsg ( EC_No_Sources );
        exit (RC_USER_ERROR);
    }

    if (Irb_File_Name == NULL)
        Irb_File_Name = New_Extension (Src_File_Name, IRB_FILE_EXTENSION);

    if (Irb_Output_Name == NULL)
	Irb_Output_Name = New_Extension (Src_File_Name, ".I");

    if (Trc_File_Name == NULL) {
      if (Tracing_Enabled)
	Trc_File_Name = New_Extension (Irb_Output_Name, TRC_FILE_EXTENSION);
    } else if ( *Trc_File_Name == '-')
        Trc_File_Name = NULL;

    Set_Trace_File (Trc_File_Name);

    /* -tt1:1 requests all of the performance trace flags: */
    if ( Get_Trace ( TP_PTRACE1, 1 ) ) {
      Set_Trace ( TP_PTRACE1, 0xffffffff );
      Set_Trace ( TP_PTRACE2, 0xffffffff );
    }

    /* tt1:17 requests tlog trace */
    if (Get_Trace ( TP_PTRACE1, TP_PTRACE1_INL )) {
      if ( Tlog_File_Name == NULL ) {
	/* Replace .I file extension to get trace file: */
	Tlog_File_Name =  New_Extension (Irb_Output_Name , ".tlog" );
      }
      if ( (Tlog_File = fopen ( Tlog_File_Name, "w" ) ) == NULL ) {
	ErrMsg ( EC_Tlog_Open, Tlog_File_Name, errno );
	Tlog_File_Name = NULL;
	Tlog_File = stdout;
      }
    }
  
      
    /* -ti64 requests a listing of all the -tt flags: */
    if ( Get_Trace ( TKIND_INFO, TINFO_TFLAGS ) ) {
      List_Phase_Numbers ();
    }


}

/* ====================================================================
 *
 * main
 *
 * Main entry point and driver for the Muse compiler.
 *
 * ====================================================================
 */

main (INT argc, char **argv)
{
    INT32 ecount, local_ecount, local_wcount;

    ecount = 0;
    /* Here are things that every process driver should do as soon as
     * possible upon start-up.
     */
    Handle_Signals();
    MEM_Initialize();
    Dont_Use_WN_Free_List ();

    /* Perform preliminary command line processing: */
    Cur_PU_Name = NULL;
    Init_Error_Handler (100);
#if !defined(SHARED_BUILD)
    Set_Error_Tables ( Phases, host_errlist );
#endif
    Set_Error_Line ( ERROR_LINE_UNKNOWN );
    Set_Error_File (NULL);

#ifdef _LIGHTWEIGHT_INLINER
    Set_Error_Phase ( "Lightweight Inliner" );
#else // _LIGHTWEIGHT_INLINER
    Set_Error_Phase ( "Inliner" );
#endif // _LIGHTWEIGHT_INLINER

    Preconfigure ();
    Process_Command_Line (argc, argv);
    if ( ! INLINE_Enable ) {
      INT rc;
      /* inline is off - link the output file to the input file,
         and report an error if it fails: */
      /* first need to delete existing file, if doesn't exist this
	 gives a message but we ignore it. */
      unlink(Irb_Output_Name);
#ifndef __MINGW32__
      rc = symlink(Irb_File_Name, Irb_Output_Name);
      if (rc != 0)
#else
      rc = CopyFile(Irb_File_Name, Irb_Output_Name, TRUE);
      if (rc == 0)
#endif /* __MINGW32__ */
	ErrMsg ( EC_IR_Create, Irb_Output_Name, errno);
    }
    else {
      Configure ();
      Set_Error_Source (Src_File_Name);
      Init_Operator_To_Opcode_Table();
      BOOL close_output = Inliner(Irb_File_Name, Irb_Output_Name);
 

      if ( Get_Trace ( TKIND_ALLOC, TP_IPA ) ) {
	fprintf ( TFile,
		 "\n%s%s\tMemory allocation information after Inliner\n%s%s\n",
		 DBar, DBar, DBar, DBar );
	MEM_Trace ();
      }

      Cleanup_Files ( TRUE, FALSE );
      if (close_output)
          Close_Output_Info();
    }
      
    /* If we've seen errors, note them and terminate: */
    if ( Get_Error_Count ( &local_ecount, &local_wcount ) ) {
        ecount += local_ecount;
    }

    if ( ecount > 0 ) {
        Terminate (RC_USER_ERROR) ;
    }

    exit (RC_OKAY);
}
