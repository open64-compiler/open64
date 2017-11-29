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

#ifndef glob_INCLUDED
#define glob_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: glob.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/linux/mfef90_includes/glob.h,v $
 *
 * Revision history:
 *  15-Sep-89 - Original Version
 *  14-Mar-91 - Copied for TP/Muse
 *
 * Description:
 *
 * External interface to routines in the main Muse/TP compiler driver
 * module, basically very general purpose control and trace routines
 * which don't belong anywhere else.
 *
 * Also, external interface to global compiler data which doesn't
 * belong anywhere else.  See com/config.h for a general discussion
 * of where global configuration and option data declarations are
 * placed.
 *
 * All of this data is currently located in the process driver,
 * bedriver.c for the back end.
 *
 * NOTE:  This file is intended ultimately to be included in multiple
 * compiler processes.  We therefore attempt to distinguish between
 * items which are not process-specific and those which are, placing
 * the latter under the appropriate #ifdefs.
 *
 * ====================================================================
 * ====================================================================
 */

/* Dummy struct definitions to keep prototypes happy: */
struct bb;
struct symtab;

#ifdef __cplusplus
struct ST;
#else
struct st;
#endif

/* ====================================================================
 *
 * Non-process-specific data (intended for both front ends and back
 * end).
 *
 * ====================================================================
 */

extern char *Cur_PU_Name;	/* The current program unit name */
extern char *Orig_PU_Name;	/* The original program unit name in source */

/* Kind of the current program unit -- set similarly to Cur_PU_Name */
typedef enum {
  PU_UNKNOWN,
  PU_FUNCTION,
  PU_SUBROUTINE,
  PU_PROGRAM,
  PU_BLOCKDATA,
  PU_MAX_KIND		/* used for bounds checking */
} PU_KIND;
extern PU_KIND Cur_PU_Kind;

/* Current file names: */
extern char *Src_File_Name;	/* Source file */
extern char *Orig_Src_File_Name; /* Original source file passed to driver */
extern char *Cpp_File_Name;	/* cpp-preprocessed file */
extern char *Lst_File_Name;	/* Listing file */
extern char *Err_File_Name;	/* Error file */
extern char *Trc_File_Name;	/* Trace file */
extern char *Tlog_File_Name;	/* Transformation log file */
extern char *Irb_File_Name;	/* ACIR intermediate file */
extern char *IR_File_Name;	/* SGIR intermediate file */
extern char *Ipa_File_Name;	/* IPA file */
extern char *Asm_File_Name;	/* Assembly file */
extern char *Obj_File_Name;	/* Relocatable object file */
extern char *Feedback_File_Name;/* feedback file produced from prof */
#ifndef MONGOOSE_BE
extern char *Lib_File_Name;	/* Program library file */
#endif
extern char *Lib_Lock_Name;	/* Program library lock file */
extern char *DSTdump_File_Name; /* Dwarf intermediate (i.e. DST) dump file */
extern char *Global_File_Name;	/* Global symbol table file */

/* Current file handles if open, NULL otherwise: */
extern FILE *Src_File;		/* Source file */
extern FILE *Cpp_File;		/* cpp-preprocessed file */
extern FILE *Lst_File;		/* Listing file */
extern FILE *Err_File;		/* Error file */
extern FILE *Trc_File;		/* Trace file */
extern FILE *Tlog_File;		/* Transformation log file */
extern FILE *Irb_File;		/* ACIR intermediate file */
extern FILE *IR_File;		/* SGIR intermediate file */
extern FILE *Ipa_File;		/* IPA file */
extern FILE *Asm_File;		/* Assembly file */
extern FILE *Obj_File;		/* Relocatable object file */
extern FILE *Feedback_File;	/* feedback file produced from prof */
extern FILE *Lib_File;		/* Program library file */
extern FILE *Tim_File;		/* Timing info file (usually trace) */
#ifdef MONGOOSE_BE
#define MMAPPED_FILE (1)	/* some components use mmap instead of
				   stream i/o, in those cases the *_File
				   will be set to MMAPPED_FILE to show that
				   the file is active  */
#endif /* MONGOOSE_BE */

extern INT32 Num_Source_Files;

/* Output requested: */
extern BOOL Assembly;		/* Assembly code */
extern BOOL Object_Code;	/* Object code */
extern BOOL Symbol_Table_Out;	/* Symbol table output (list or trace) */
extern BOOL Show_Progress;	/* Report progress to stdout */

#ifndef _NEW_SYMTAB
/* Print symbol table for a program unit after a phase, if enabled: */
extern void Trace_ST (
  INT phase,	/* Phase after which to print */
  char *pname,	/* Print name of phase */
  struct symtab *stab, /* Symtab to print */
  INT32 bb_id	/* basic block to print, or NULL */
);
#endif /* _NEW_SYMTAB */

/* Print memory statistics, if enabled */
extern void Trace_Memory_Allocation (
  INT phase,	/* Phase after which we're printing */
  const char *pname,	/* Print name for phase */
#ifdef __cplusplus
  const ST *pu,
#else
  const struct st *pu,	/* Program unit to print */
#endif
  INT32 bb_id	/* basic block to limit traces to */
);

/* Clean up files after failure: */
extern void Cleanup_Files (
  BOOL report	/* Report errors which occur during file cleanup? */
		/* This should generally be FALSE for failures.   */
  ,BOOL delete_doto /*delete the .o if created */
);

/* Clean up and terminate program with given exit status: */
extern void Terminate ( INT status );

/* ====================================================================
 *
 * Back end process-specific data.
 *
 * ====================================================================
 */

#ifdef BACK_END

/* Front End process info: for consistency checking */
/* 
 * The first two are set in the front end, and examined in the back end.
 * They are also put out to the ipa file.  The third is only used in the 
 * back end to store the Version number of the front end.  (In the front
 * end, this information is stored in "Version".  In the back end, 
 * "Version" contains the version number of the *back* end.  Those
 * variables are initialized in <machine>/<process>/version.c
 */
extern INT32 Fe_Process_Id;
extern INT32 Fe_Process_Time;
extern char *Fe_Version;
extern BOOL Fe_Josie;	/* Did .B come from a Josie front end? */

/* Set FE process info -- defined in flags.c: */
extern void Set_Fe_Info ( INT32 pid, INT32 ptime, char *fe_version );

/* Set source file name: */
extern void Set_File_Name ( char *name );

/* Have the OP_REGCOPY operations been translated? */
extern BOOL Regcopies_Translated;

#else /* ! BACK_END */

/* Our front ends are never Josie front ends: */
# define Fe_Josie	FALSE

#endif /* BACK_END */

#ifdef __cplusplus
}
#endif
#endif /* glob_INCLUDED */
