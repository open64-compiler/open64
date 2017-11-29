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
 * Module: whirl2f.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:42 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/whirl2f.cxx,v $
 *
 * Revision history:
 *  07-Nov-94 - Original Version
 *
 * Description:
 *
 *   Defines the main program (whirl2f), which parses the command-
 *   line options and translates an intermediate Very-High or High
 *   level WHIRL file into Fortran.  The command-line options are
 *   processed from left to right without any ordering constraint.
 *   In EBNF notation where '{' indicates 1 or more repititions,
 *   '|' indicates choice, and '[' indicates an optional item:
 *
 *      whirl2f [-FLIST:<opts>] {<Whirl_File_Name>}
 *
 *      <opts> ::= <single_opt>[:<opts>]
 *      <single_opt> ::= show | 
 *                       old_f77 |
 *                       ansi_format |
 *                       no_pragmas  |
 *                       emit_pfetch | 
 *                       emit_linedir |
 *                       src_file=<Src_OutFile_Name> |
 *                       ftn_file=<Ftn_OutFile_Name> |
 *                       loc_file=<LocationMap_OutFile_Name>
 *
 *   Note that the <Whirl_File_Name> is a mandatory option, and in
 *   its abscence a message about usage will be emitted.  If the 
 *   <Src_File_Name> is not given it will be derived from the
 *   <Whirl_File_Name>.  The output file-names will be derived
 *   from the <Src_File_Name>, unless they are explicitly given.
 *
 * ====================================================================
 * ====================================================================
 */
static char *source_file = __FILE__;
static char *rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/whirl2f.cxx,v $ $Revision: 1.1 $";

#include <unistd.h>		    /* for close(), etc. */
#include <elf.h>              /* for wn.h */
#include "whirl2f_common.h"
#include "glob.h"             /* Irb_File_Name, Cur_PU_Name */
#include "flags.h"	      /* for OPTION_GROUP */
#include "pu_info.h"          /* For PU_Info */
#include "err_host.tab"       /* Include the error tables in the driver */

#include "pu_info.h"          /* For PU_Info */
#include "w2f_driver.h"
#include "file_util.h"        /* for New_Extension () */
#include "ir_bread.h"         /* Binary WHIRL reader */


/* ====================================================================
 *
 * Local data.
 *
 * ====================================================================
 */

/* Default file	extensions: */
#define	FTN_FILE_EXTENSION ".f"  /* Fortran file */
#define	IRB_FILE_EXTENSION ".B"	 /* WHIRL file */


/* ====================================================================
 *
 * Usage
 *
 * Give a warning about the usage of this tool, in terms of the
 * command-line options it accepts.
 *
 * ====================================================================
 */

static void
Usage (char *progname)
{
   fprintf(stderr, 
	   "USAGE: in EBNF notation, where '|' indicates choice and '['\n"
	   "indicates an optional item:\n"
	   "\n"
	   "\t%s [-FLIST:<opts>] [-TARG:<t>] [-TENV:<e>] <Whirl_File_Name>\n"
	   "\n"
	   "\t<opts> ::= <single_opt>[:<opts>]\n"
	   "\n"
	   "The <Whirl_File_Name> is a mandatory command-line argument.\n"
	   "We recommend always using the common option -TARG:abi=[32|64].\n" 
	   "\n",
	   progname);
   
   fprintf(stderr,
	   "Each -FLIST:<single_opt> is described below:\n"
	   "\n"
	   "-FLIST:show\n"
	   "\tIndicate the input/output file-names to stderr.\n"
	   "-FLIST:old_f77\n"
	   "\tPrevents emission of calls to intrinsic functions that are not\n"
	   "\tin compilers earlier than version v7.00.  The generated source\n"
	   "\twill include <whirl2f.h>\n"
	   "-FLIST:ansi_format\n"
	   "\tFormat the output according to Fortran 77 rules, with at most\n"
	   "\t72 columns per line.  Without this option, tab formatting is\n"
	   "\temployed without any limit on the line-length.\n"
	   "-FLIST:no_pragmas\n"
	   "\tTurn off emission of pragmas into the Fortran output file.\n"
	   "-FLIST:emit_pfetch\n"
	   "\tEmit comments to indicate prefetch instructions.\n"
	   "-FLIST:emit_linedir\n"
	   "\tEmit #line directives to map the generated statements back to\n"
	   "\tthe corresponding original source statements.\n"
	   "-FLIST:src_file=<Src_File_Name>\n"
	   "\tThe name of the original source program.  When not given,\n"
	   "\tthe <Src_File_Name> is derived from the <Whirl_File_Name>.\n"
	   "-FLIST:ftn_file=<Ftn_OutFile_Name>\n"
	   "\tThe file into which program units will be emitted.  When not\n"
	   "\tgiven, <Ftn_OutFile_Name> is derived from <Src_File_Name>.\n"
	   "-FLIST:loc_file=<Loc_OutFile_Name>\n"
	   "\tThe file for emission of a mapping from positions in the\n"
	   "\tsource to corresponding positions in the original source\n"
	   "\tfile.  Without this option, no such file is created.\n"
	   "\n");
   fprintf(stderr,
	   "Compile the generated <C_File_Name> with \"-dollar -lm\".  When\n"
	   "-FLIST:ftn, then compile the generated <C_File_Name> with\n"
           "\"-D_FORTRAN2C -dollar -lftn\" and possibly other \"-l\" options\n"
	   "to account for libraries referenced in the source.\n"
	   "\n");
   fprintf(stderr,
	   "The generated source will employ \"tab\" formatting, and\n"
	   "identifiers may have the \'\$\' character in their names,\n"
	   "so always compile with the \"-dollar\" option.\n"
	   "\n");
} /* Usage */


/* ====================================================================
 *
 * Process_Common_Options
 *
 * Process the Common_Option_Groups, ignoring all other options.
 *
 * ====================================================================
 */

static void
Process_Common_Options(INT argc,     /* Number of command line arguments */
		       char *argv[], /* Array of command line arguments */
		       char *envp[]) /* Array of environment pointers */
{
   INT argidx;
   
   for (argidx = 1; argidx < argc; argidx++ )
   {
      if (argv[argidx] != NULL)
      {
	 if (argv[argidx][0] == '-' )
	 {
	    (void)Process_Command_Line_Group(&argv[argidx][1], 
					     Common_Option_Groups);
	 }
      }
   }
} /* Process_Common_Options */


/* ====================================================================
 *
 * Local_Terminate
 *
 * Do any necessary cleanup at the start of Terminate().
 *
 * ====================================================================
 */

void
Local_Terminate ( void )
{
  /* Close and delete whirl2f specific files before calling Terminate() */
  W2F_Fini();
}


/* ====================================================================
 *
 * Get_Irb_File_Name
 *
 * Process the command-line arguments to get the input file name,
 * and set Irb_File_Name to this name.  Return the number of
 * input file-name arguments found on this command-line (where
 * Irb_File_Name will be set to the first one encountered).
 *
 * ====================================================================
 */

static INT32
Get_Irb_File_Name(INT argc, char *argv[])
{
   /* Find the WHIRL input file-name.  There should only be one. */
   #define MAX_FNAME_LENGTH 256-7 /* allow for suffix ".W2F.c\0" */
   static char filename[MAX_FNAME_LENGTH+7];
   INT32       argidx;
   INT32       src_count = 0;
   
   for (argidx = 1; argidx < argc; argidx++)
   {
      /* Null argument => end of list: */
      if (argv[argidx][0] == '-' ) 
      {
	 /* Ignore regular options */
      }
      else if (src_count > 0)
	 src_count += 1;
      else if (src_count == 0) /* Presumably a WHIRL input file name */
      {
	 /* Copy the original file-name to a static buffer */
	 Irb_File_Name = argv[argidx];
	 if (strlen(Irb_File_Name) > MAX_FNAME_LENGTH)
	 {
	    Irb_File_Name = strncpy(filename, Irb_File_Name, MAX_FNAME_LENGTH);
	    filename[MAX_FNAME_LENGTH] = '\0';
	    fprintf(stderr, 
		    "WARNING: input file-name truncated to "
		    "(max=%d chars): \"%s\"\n",
		    MAX_FNAME_LENGTH, Irb_File_Name);
	 }
	 else
	    Irb_File_Name = strcpy(filename, Irb_File_Name);

	 src_count = 1;
      } /* If source-file argument */
   } /* While not found */
   return src_count;
} /* Get_Irb_File_Name */


/* ====================================================================
 *
 * main
 *
 * Main entry point and driver for the whirl2c translator.
 *
 * ====================================================================
 */

INT
main (INT argc,       /* Number of command line arguments */
      char *argv[],   /* Array of command line arguments */
      char *envp[])   /* Array of environment pointers */
{
   WN          *pu;
   INT32        ecount = 0, wcount = 0, local_ecount = 0, local_wcount = 0;
   INT32        inp_file_count;
   PU_Info     *pu_tree, *current_pu;

   /* Here are things that every process driver should do as soon as
   * possible upon start-up.
   */
   MEM_Initialize();
   Init_Error_Handler(10);
   Set_Error_Line(ERROR_LINE_UNKNOWN);
   Set_Error_Phase("Whirl2f");
   Set_Error_File(NULL); /* Use stderr */

   Preconfigure(); /* Setup target and host specific info before flags */
   Process_Common_Options(argc, argv, envp); /* Common options */
   Configure();    /* Setup target and host specific info after flags */

   Init_Operator_To_Opcode_Table();
    
   /* Process the source files */
   inp_file_count = Get_Irb_File_Name(argc, argv);
   if (inp_file_count == 0)
   {
      Usage(argv[0]);
      fprintf(stderr, "ERROR: missing input file on command-line\n");
   }
   else if (inp_file_count > 1)
   {
      Usage(argv[0]);
      fprintf(stderr, "ERROR: too many input files on command-line\n");
   }
   else
   {
      /* Get the global symbol table, the string table, the constant table,
       * and the initialization table, before initializing the translation
       * to C.
       */
      (void)Open_Input_Info(Irb_File_Name);
      pu_tree = Read_Global_Info(NULL);
      W2F_Process_Command_Line(argc, argv, argc, argv);
      W2F_Outfile_Init();

      /* Loop thru all the PUs */
      for (current_pu = pu_tree;
	   current_pu != NULL;
	   current_pu = PU_Info_next (current_pu)) {

	 MEM_POOL_Push (MEM_pu_nz_pool_ptr);
	 Read_Local_Info (MEM_pu_nz_pool_ptr, current_pu);
	 pu = PU_Info_tree_ptr(current_pu);

	 W2F_Outfile_Translate_Pu(pu);

	 if (PU_Info_child(current_pu)) {
	    fprintf(stderr, "WARNING: ignoring nested procedures in \"%s\"\n",
		    ST_name(PU_Info_proc_sym(current_pu)));
	 }

	 Free_Local_Info (current_pu);
	 MEM_POOL_Pop (MEM_pu_nz_pool_ptr);
      } /* for each PU */

      W2F_Outfile_Fini();
      Cleanup_Files(TRUE, FALSE);
   }
   
   /* If we've seen errors, note them and terminate: */
   if (Get_Error_Count(&local_ecount, &local_wcount))
   {
      ecount += local_ecount;
      wcount += local_wcount;
   }
   if ( ecount > 0 )
   {
      Local_Terminate();
      Terminate(1);
   }
   
   exit(0);
} /* main */
