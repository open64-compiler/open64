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


#ifndef w2f_driver_INCLUDED
#define w2f_driver_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
/* ====================================================================
 * ====================================================================
 *
 * Module: w2f_driver.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:42 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/w2f_driver.h,v $
 *
 * Description:
 *
 * W2F_Should_Emit_Nested_PUs()
 *    Returns TRUE when we should translate nested PUs.
 *
 * W2F_Process_Command_Line()
 *    Process the command line arguments. Evaluate flags pertaining
 *    solely to w2f, assuming these are in the phase_argv list, 
 *    ignoring all others (assuming non-w2f options are processed 
 *    and checked for correctness elsewhere).  argv[0] is assumed to
 *    hold the name of the current program invocation ("whirl2f")
 *    or NULL if there is no such name.  Also, set up the file-names 
 *    (W2F_File) such that these are all non-NULL.  The naming of 
 *    files is based partially on the command-line options, and 
 *    partially on Src_File_Name and Irb_File_Name.  The only 
 *    W2F_File which may be NULL after this call is the W2F_LOC_FILE.
 *
 * W2F_Init()
 *    Basic Initialization of W2F, assuming the command-line has 
 *    already been processed and the global symbol-table, string-
 *    table, and initialization table already have been read in with 
 *    Read_Global_Info().  This initiates all translation modules, 
 *    but opens no output files and does no preliminary translation.
 *
 * W2F_Push_PU()
 *    Must be called before translating an expression within
 *    a PU (i.e. a FUNC_ENTRY).  The new symbol table for the 
 *    given PU will be pushed onto the stack.  After this call, the
 *    locally defined symbol names can be accessed by means of the 
 *    W2CF_Symtab_Nameof_() routines.  This call
 *    also specifies the subtree within the body of the pu, which
 *    we are interested in translating, which *must* exactly match
 *    the subtree passed in a call to W2F_Translate_Wn() or 
 *    W2F_Translate_Wn_Str().
 *
 * W2F_Pop_PU()
 *    This must be called after a PU is done with, and the symbol
 *    table-section for the PU will be popped off the whirl2f
 *    symbol table stack.
 *
 * W2F_Set_Frequency_Map()
 *    Pass in a mapping from WN nodes to feedback frequency information.
 *    Call this for each PU that has such a map.  The actual emission
 *    of this info (in comments) is controlled by the "emit_frequency"
 *    -FLIST option.
 *
 * W2F_Get_Transformed_Src_Path()
 *    Returns the name of the .f file emitted by whirl2f.  Must be
 *    called after W2F_Init().
 *
 * W2F_Def_ST()
 *    Appends a definition for the given symbol to the given
 *    file.  The file must be open with write permission.
 *
 * W2F_Object_Name()
 *    Returns the name of the given function.
 *
 * W2F_Translate_Stid_Lhs()
 *    Given the attributes for an OPR_STID (or similar attributes
 *    derived from other kinds of nodes, such as an OPR_LDID), put an
 *    expression suitable as an assignment lhs in the given string
 *    buffer.
 *
 * W2F_Translate_Istore_Lhs()
 *    Same as W2F_Translate_Stid_Lhs(), but based on the attributes
 *    for an OPR_ISTORE instead of the attributes of an OPR_STID.
 *
 * W2F_Translate_Wn()
 *    Translation of an arbitrary WN tree into Fortran, assuming the 
 *    current PU scope has been pushed onto the whirl2f symbol-
 *    table stack.  The output will be appended to the given file.
 *    If the expression is a FUNC_ENTRY node, then the local symbol
 *    table will be pushed and popped, and the effect is similar 
 *    to W2F_Outfile_Translate_Pu().  The file must be open with 
 *    write permission. 
 *
 * W2F_Translate_Wn_Str()
 *    Same as W2F_Translate_Wn(), but the output is put into the
 *    given string buffer instead of being appended to a file.
 *
 * W2F_Fini()
 *    Finalization of W2F.  This will terminate all translation 
 *    modules and free up their state information (such as symol-
 *    tables, etc).
 *
 *                  EXPORTED OUTPUT-FILE INTERFACE
 *                  ------------------------------
 *
 * This is the easiest interface for using whirl2f, and it
 * maintains output file status and produces a uniform output
 * format based on the -FLIST options.  There are strict rules
 * as to how this interface interacts with the lower-level 
 * interface described above.
 *
 * W2F_Outfile_Init()
 *    Initializes the output-files for whirl2f, based on the
 *    command-line options.  Will call W2F_Init() is this has
 *    not already been done.  Optionally, the file-level 
 *    declarations may be emitted to the generated header file.
 *
 * W2F_Outfile_Translate_Pu()
 *    Translates a PU, presupposing W2F_Outfile_Init() has been 
 *    called.  The PU will be lowered (for Fortran) and the 
 *    output Fortran code will be appended to the output files.
 *
 * W2C_Output_Fini()
 *    Finalizes a W2F translation by closing the output-files, and
 *    calling W2F_Fini().  Call this before the Irb file is closed!
 *
 *        INTERACTION BETWEEN OUTPUT-FILE AND LOW-LEVEL INTERFACE
 *        -------------------------------------------------------
 *
 * 1) There is no ordering constraint between the Init routines.
 *    A redundant call to an init routine is simply ignored.
 *
 * 2) All "Outfile" routines require a W2F_Outfile_Init() call.
 *
 * 3) All "low-level" routines require a W2F_Init() or a 
 *    W2F_Outfile_Init() call.
 *
 * 4) A call to W2F_Push_PU() may be followed by a call to 
 *    W2F_Outfile_Translate_Pu(), in which case the scope will 
 *    not be pushed again and will not be popped when
 *    W2F_Outfile_Translate_Pu() returns.
 *
 * 5) W2F_Fini() has no effect if W2F_Outfile_Init() was called,
 *    since finalization must then be done with a call to
 *    W2F_Output_Fini().
 *
 * 6) Initialization may again occur after a finalization for
 *    a another whirl2f translation.  Two whirl2f translation
 *    tasks cannot be active simultaneously within the same process,
 *    but the same WHIRL tree may be translated multiple times
 *    in a given sequential sequence by the same translation 
 *    process.
 *
 * ====================================================================
 * ====================================================================
 */

           /* Flags set by W2F_Process_Command_Line() */
           /*-----------------------------------------*/

extern BOOL W2F_Verbose;         /* Show translation information */
extern BOOL W2F_Old_F77;         /* Use macros for new intrinsics */
extern BOOL W2F_Ansi_Format;     /* Line-formatting to f77 standard */
extern BOOL W2F_No_Pragmas;      /* Do not emit pragmas */
extern BOOL W2F_Emit_Prefetch;   /* Emit comments for prefetches */
extern BOOL W2F_Emit_All_Regions;/* Emit cmplr-generated regions */
extern BOOL W2F_Emit_Linedirs;   /* Emit preprocessing line-directives */
extern BOOL W2F_Emit_Frequency;  /* Emit feedback frequency information */
extern BOOL W2F_Emit_Pcf;        /* Emitting PCF spellings of pragmas */
extern BOOL W2F_Emit_Omp;        /* Emitting OMP spellings of pragmas */


                     /* Files */
                     /*-------*/

typedef enum W2F_File_Kind
{
   W2F_ORIG_FILE, /* Fortran input file  */
   W2F_FTN_FILE,  /* Fortran output file */
   W2F_LOC_FILE,  /* W2F generated source-to-source location mapping file */
   W2F_NUM_FILES  /* Number of elements in this enumeration */
} W2F_FILE_KIND;

extern FILE *W2F_File[W2F_NUM_FILES];


                     /* Exported Functions */
                     /*--------------------*/

  /* Note that we also export some utilities from W2CF_symtab.h */
extern BOOL W2F_Should_Emit_Nested_PUs(void);
  extern void W2F_Process_Command_Line(INT phase_argc,const char *phase_argv[],
				     INT argc, const char *argv[]);
extern void W2F_Init(void);
extern void W2F_Fini(void);
extern void W2F_Push_PU(WN *pu, WN *body_part_of_interest);
extern void W2F_Pop_PU(void);

extern void W2F_Set_Frequency_Map(WN_MAP frequency_map);

extern const char * W2F_Get_Transformed_Src_Path(void);

extern void W2F_Def_ST(FILE *outfile, ST *st);

extern const char * W2F_Object_Name(ST *func_st);

extern void W2F_Translate_Stid_Lhs(char       *strbuf,
				   UINT        bufsize,
				   ST         *stid_st, 
				   mINT64      stid_ofst, 
				   TY_IDX      stid_ty, 
				   TYPE_ID     stid_mtype);
extern void W2F_Translate_Istore_Lhs(char       *strbuf, 
				     UINT        bufsize,
				     WN         *lhs,
				     mINT64      istore_ofst, 
				     TY_IDX      istore_addr_ty, 
				     TYPE_ID     istore_mtype);

extern void W2F_Translate_Wn(FILE *outfile, WN *wn);
extern void W2F_Translate_Wn_Str(char *strbuf, UINT bufsize, WN *wn);

extern void W2F_Outfile_Init(void);
extern void W2F_Outfile_Translate_Pu(WN *pu);
extern void W2F_Outfile_Fini(void);

extern void W2F_Cleanup(void);

#ifdef __cplusplus
}
#endif
#endif /* w2f_driver_INCLUDED */
