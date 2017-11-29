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
 * Module: ir_walker.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:22:18 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/ir_tools/ir_walker.c,v $
 *
 * Revision history:
 *  22-Apr-97 - Original Version
 *
 * Description:
 *
 *   This program, as it stands, doesn't do anything.  It is skeleton
 *   code which opens an existing WHIRL file and walks through all PU's,
 *   symbols, constants, WHIRL trees, etc.  There are two uses for this
 *   code:  a program which analyzes the *.B file and reports things about
 *   it;  and  a program which modifies part of the information and re-writes
 *   the WHIRL file.
 *
 *      ir_walker [-v] [-o <Whirl_File_Name_out>] <Whirl_File_Name>
 *
 *   Note that the <Whirl_File_Name> is a mandatory argument, and in
 *   its absence a message about usage will be emitted.  If the program
 *   is written to output a modified WHIRL file then it will either be
 *   written to the optional -o argument or to the original WHIRL file.
 *   The -v option turns on pu, symbol and WHIRL node tracing.
 *
 * ====================================================================
 * ====================================================================
 */

#include <elf.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <search.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "wn.h"
#include "stab.h"
#include "aux_stab.h"
#include "irbdata.h"
#include "wintrinsic.h"
#include "glob.h"
#include "pu_info.h"
#include "ir_bread.h"
#include "ir_bwrite.h"
#include "file_util.h"


/* ====================================================================
 *
 * Local data.
 *
 * ====================================================================
 */

#define MAX_FNAME_LENGTH 256

char filename_in[MAX_FNAME_LENGTH];
char filename_out[MAX_FNAME_LENGTH];
char temp_filename[MAX_FNAME_LENGTH+16];

char *Irb_File_Name_out = NULL;

struct stat statbuf;

BOOL oflag = FALSE;
BOOL vflag = FALSE;

INT32 indent = 0;
INT32 indent_inc = 2;


/* ====================================================================
 *
 * Adaptation section.
 *
 * These routines are meant to be adapted by the user to do whatever
 * you'd like.  Each routine is called by the walkers for each PU,
 * SYMTAB, symbol and WHIRL node as appropriate.
 *
 * The *_WALK defines control whether or not the corresponding walks
 * take place (to improve performance).
 *
 * The action of the walker is also controlled by the WRITE_IRB symbol.
 * If it is undefined then it is assumed that the IRB file is only
 * read and processed.  If it is defined then it is assumed that
 * changes are being made by the walker and that the IRB file must
 * be re-written.
 *
 * ====================================================================
 */


#undef   ST_WALK
#undef   TY_WALK
#undef   WN_WALK

#undef   WRITE_IRB


/*  Called at the beginning of processing.  */
void
Initialization (void)
{
}


/*  Called for each PU found.  */
void
Process_PU (PU_Info *pu)
{
}


/*  Called for each STAB found.  */
void
Process_STAB (SYMTAB *stab)
{
}


/*  Called for each ST found.  */
void
Process_ST (ST *st)
{
}


/*  Called for each TY found.  */
void
Process_TY (TY *st)
{
}


/*  Called for each WHIRL NODE found.  */
void
Process_WN (WN *wn)
{
}


/*  Called at the end of processing.  */
void
Termination (void)
{
}


/* ====================================================================
 *
 * Usage
 *
 * Give a warning about the usage of this tool, in terms of the
 * command line options it accepts.
 *
 * ====================================================================
 */

static void
Usage (void)
{
   fprintf (stderr, 
	    "USAGE:\n"
	    "\n"
	    "\t [-v] [-o <Whirl_File_Name_out>] <Whirl_File_Name>\n"
	    "\n"
	    "The <Whirl_File_Name> is a mandatory command line argument.\n"
	    "The optional -v flag controls verbose trace messages.\n"
	    "The optional -o flag is used to specify an alternate out file.\n"
	    "\n"
	    );
   
} /* Usage */


/* ====================================================================
 *
 * Get_Irb_File_Name
 *
 * Process the command line arguments to get the input file name,
 * the optional output file name and any other options.  Return the
 * number of input filename arguments found on this command line
 * (where Irb_File_Name will be set to the first one encountered).
 *
 * ====================================================================
 */

static INT32
Get_Irb_File_Name (INT argc, char *argv[])
{
   /* Find the WHIRL input filename.  There should only be one. */
   char *fname;
   INT32       argidx;
   INT32       src_count = 0;
   
   for (argidx = 1; argidx < argc; argidx++)
   {
      /* Null argument => end of list: */
      if (argv[argidx][0] == '-' ) 
      {
	 /* Process regular options */
	if (strcmp (argv[argidx], "-o") == 0) {
#ifdef WRITE_IRB
	  oflag = TRUE;
	  fname = argv[++argidx];
	  if (strlen(fname) > MAX_FNAME_LENGTH)
	  {
	    Irb_File_Name_out = strncpy (filename_out, fname, MAX_FNAME_LENGTH);
	    filename_out[MAX_FNAME_LENGTH] = '\0';
	    fprintf (stderr, 
		     "WARNING: output filename truncated to "
		     "(max=%d chars): \"%s\"\n",
		     MAX_FNAME_LENGTH, fname);
	  }
	  else
	    Irb_File_Name_out = strcpy (filename_out, fname);
#else
	  fprintf (stderr, 
		   "WARNING: cannot specify output file");
	  ++argidx;
#endif
	} else if (strcmp (argv[argidx], "-v") == 0) {
	  vflag = TRUE;
	} else {
	  fprintf (stderr,
		   "WARNING: unrecognized command option "
		   "%s\n",
		   argv[argidx]);
	}
      }
      else if (src_count > 0)
	 src_count += 1;
      else if (src_count == 0) /* Presumably a WHIRL input file name */
      {
	 /* Copy the original filename to a static buffer */
	 fname = argv[argidx];
	 if (strlen(fname) > MAX_FNAME_LENGTH)
	 {
	    Irb_File_Name = strncpy (filename_in, fname, MAX_FNAME_LENGTH);
	    filename_in[MAX_FNAME_LENGTH] = '\0';
	    fprintf (stderr, 
		     "WARNING: input filename truncated to "
		     "(max=%d chars): \"%s\"\n",
		     MAX_FNAME_LENGTH, fname);
	 }
	 else
	    Irb_File_Name = strcpy (filename_in, fname);

	 src_count = 1;
      } /* If source file argument */
   } /* While not found */
   return src_count;
} /* Get_Irb_File_Name */


/* ====================================================================
 *
 * process_stab
 *
 * Process each STAB.
 *
 * ====================================================================
 */

static void
process_stab (SYMTAB *stab)
{

    ST *st;
    TY *ty;

    if (vflag)
      printf ("%*sProcessing STAB: %d\n", indent, "", SYMTAB_id(stab));
    Process_STAB (stab);

#ifdef ST_WALK

    indent += indent_inc;

    for (st = SYMTAB_symbols(stab); st != NULL; st = ST_next(st)) {
      if (vflag)
	printf ("%*sProcessing ST: %s\n", indent, "", ST_name(st));
      Process_ST (st);
    }

    indent -= indent_inc;

#endif

#ifdef TY_WALK

    indent += indent_inc;

    for (ty = SYMTAB_types(stab); ty != NULL; ty = TY_next(ty)) {
      if (vflag)
	printf ("%*sProcessing TY: %s\n", indent, "", TY_name(ty));
      Process_TY (ty);
    }

    indent -= indent_inc;

#endif

}


/* ====================================================================
 *
 * process_wn
 *
 * Process each WN.
 *
 * ====================================================================
 */

static void
process_wn (WN *wn)
{

    INT32 i;
    WN *node;
    OPCODE op;

    if (wn) {

      op = WN_opcode(wn);

      if (vflag)
	printf ("%*sProcessing WN: %s\n", indent, "", OPCODE_name(op));
      Process_WN (wn);

      indent += indent_inc;

      if (op == OPC_BLOCK)
	for (node = WN_first(wn); node; node = WN_next(node))
	  process_wn (node);
      else
	for (i = 0; i < WN_kid_count(wn); i++)
	  process_wn (WN_kid(wn,i));

      indent -= indent_inc;

    }

}


/* ====================================================================
 *
 * process_func
 *
 * Process each function.
 *
 * ====================================================================
 */

static void
process_func (WN *wn)
{

    if (vflag)
      printf ("%*sProcessing FUNC: %s\n", indent, "", ST_name(WN_st(wn)));

#ifdef WN_WALK

    indent += indent_inc;

    process_wn (wn);

    indent -= indent_inc;

#endif

}


/* ====================================================================
 *
 * process_pu
 *
 * Process each PU.
 *
 * ====================================================================
 */

static void
process_pu (PU_Info *pu_tree)
{
    PU_Info *pu;

    for (pu = pu_tree; pu != NULL; pu = PU_Info_next(pu)) {
	Current_PU_Info = pu;
	MEM_POOL_Push (MEM_pu_nz_pool_ptr);
	Read_Local_Info (MEM_pu_nz_pool_ptr, pu);

	if (vflag)
	  printf ("%*sProcessing PU: %s\n", indent, "",
	          ST_name(PU_Info_proc_sym(pu)));
	indent += indent_inc;
	Process_PU (pu);

	process_stab (PU_Info_symtab_ptr(pu));

	process_func (PU_Info_tree_ptr(pu));

	if (PU_Info_child(pu)) {
	    process_pu (PU_Info_child(pu));
	}

#ifdef WRITE_IRB
	Write_PU_Info (pu);
#endif

	indent -= indent_inc;
	Free_Local_Info (pu);
	MEM_POOL_Pop (MEM_pu_nz_pool_ptr);

    }
}


/* ====================================================================
 *
 * main
 *
 * Main entry point and driver for the ir_walker program.
 *
 * ====================================================================
 */

void
main (INT argc,       /* Number of command line arguments */
      char *argv[],   /* Array of command line arguments */
      char *envp[])   /* Array of environment pointers */
{
   WN          *pu;
   INT32        inp_file_count;
   PU_Info     *pu_tree, *current_pu;

   /* Here are things that every process driver should do as soon as
    * possible upon start-up.
    */
   MEM_Initialize ();
   Init_Error_Handler (10);
   Set_Error_Line (ERROR_LINE_UNKNOWN);
   Set_Error_Phase ("IR Walker");
   Set_Error_File (NULL);

   Init_Operator_To_Opcode_Table ();
    
   /* Process the input file */
   inp_file_count = Get_Irb_File_Name (argc, argv);
   if (inp_file_count == 0)
   {
      Usage ();
      fprintf (stderr, "ERROR: missing input file on command line\n");
   }
   else if (inp_file_count > 1)
   {
      Usage ();
      fprintf (stderr, "ERROR: too many input files on command line\n");
   }
   else if (stat (Irb_File_Name, &statbuf) != 0)
   {
      fprintf (stderr, "ERROR: input file (%s) does not exist\n",
	       Irb_File_Name);
   }
   else
   {

#ifdef WRITE_IRB
      /* Setup output file */
      if (!oflag)
	(void) strcpy (filename_out, filename_in);
      (void) sprintf (temp_filename, "%s$%d", filename_out, (INT32) getpid ());
      (void) remove (temp_filename);
#endif

      /* User defined initialization code */
      Initialization ();

      /* Get the global symbol table, the string table, the constant table,
       * and the initialization table.
       */
      (void) Open_Input_Info (Irb_File_Name);
      pu_tree = Read_Global_Info (NULL);

#ifdef WRITE_IRB
      (void) Open_Output_Info (temp_filename);
#endif

      /* Process global symbol table. */
      process_stab (Global_Symtab);

      /* Loop thru all the PUs */
      process_pu (pu_tree);

      /* User defined termination code */
      Termination ();

#ifdef WRITE_IRB
      /* Finish up output file */
      Write_Global_Info (pu_tree);
      Close_Output_Info ();
      (void) remove (filename_out);
      (void) rename (temp_filename, filename_out);
#endif

   }
   
   exit (0);
} /* main */


/* Dummy definitions to satisify references from routines that got pulled
 * in by the header files but are never called
 */
void Signal_Cleanup (INT sig) { }

char * Host_Format_Parm (INT kind, MEM_PTR parm) { return NULL; }

INT8 Debug_Level = 0;
