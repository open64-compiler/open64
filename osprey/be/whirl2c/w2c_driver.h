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


#ifndef w2c_driver_INCLUDED
#define w2c_driver_INCLUDED
extern "C" {
/* ====================================================================
 * ====================================================================
 *
 * Module: w2c_driver.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:42:00-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.w2c_driver.h $
 *
 * Description:
 *
 * W2C_Should_Emit_Nested_PUs()
 *    Returns TRUE when we should translate nested PUs.
 *
 * W2C_Process_Command_Line()
 *    Process the command line arguments. Evaluate flags pertaining
 *    solely to w2c, assuming these are in the phase_argv list, 
 *    ignoring all others (assuming non-w2c options are processed 
 *    and checked for correctness elsewhere).  argv[0] is assumed to
 *    hold the name of the current program invocation ("whirl2c")
 *    or NULL if there is no such name.  Also, set up the file-names 
 *    (W2C_File) such that these are all non-NULL.  The naming of 
 *    files is based partially on the command-line options, and 
 *    partially on Src_File_Name and Irb_File_Name.  The only 
 *    W2C_File which may be NULL after this call is the W2C_LOC_FILE.
 *
 * W2C_Init()
 *    Basic Initialization of W2C, assuming the command-line has 
 *    already been processed and the global symbol-table, string-
 *    table, and initialization table already have been read in with 
 *    Read_Global_Info().  This initiates all translation modules, 
 *    but opens no output files and does no preliminary translation.
 *
 * W2C_Push_PU()
 *    Must be called before translating an expression within
 *    a PU (i.e. a FUNC_ENTRY).  The new symbol table for the 
 *    given PU will be pushed onto the stack.  After this call, the
 *    locally defined symbol names can be accessed by means of the 
 *    W2CF_Symtab_Nameof_() routines.  Note that any lowering of
 *    Fortran intrinsics and IO statements must already have 
 *    occurred in the context that calls this routine.  This call
 *    also specifies the subtree within the body of the pu, which
 *    we are interested in translating, which *must* exactly match
 *    the subtree passed in a call to W2C_Translate_Wn() or 
 *    W2C_Translate_Wn_Str().
 *
 * W2C_Pop_PU()
 *    This must be called after a PU is done with, and the symbol
 *    table-section for the PU will be popped off the whirl2c 
 *    symbol table stack.
 *
 * W2C_Set_Frequency_Map()
 *    Pass in a mapping from WN nodes to feedback frequency information.
 *    Call this for each PU that has such a map.  The actual emission
 *    of this info (in comments) is controlled by the "emit_frequency"
 *    -CLIST option.
 *
 * W2C_Get_Transformed_Src_Path()
 *    Returns the name of the .c file emitted by whirl2c.  Must be
 *    called after W2C_Init().
 *
 * W2C_Translate_Global_Types()
 *    Translate all structured types declared at file-level, such
 *    that multiple redeclarations of these will not occcur for
 *    definitions based on these types in local scopes.  At most
 *    one call to this routine should be made between Init and Fini.
 *
 * W2C_Translate_Global_Defs()
 *    Translate the file-level definitions based on references
 *    encountered thus far.  All file-level variables and functions
 *    referenced thus far will be declared.  At most one call to
 *    this routine should be made between Init and Fini.
 *
 * W2C_Object_Name()
 *    Returns the name of the given ST.
 *
 * W2C_Translate_Stid_Lhs()
 *    Given the attributes for an OPR_STID (or similar attributes
 *    derived from other kinds of nodes, such as an OPR_LDID), put an
 *    expression suitable as an assignment lhs in the given string
 *    buffer.
 *
 * W2C_Translate_Istore_Lhs()
 *    Same as W2C_Translate_Stid_Lhs(), but based on the attributes
 *    for an OPR_ISTORE instead of the attributes of an OPR_STID.
 *
 * W2C_Translate_Wn()
 *    Translation of an arbitrary WN tree into C, assuming the 
 *    current PU scope has been pushed onto the whirl2c symbol-
 *    table stack.  The output will be appended to the given file.
 *    If the expression is a FUNC_ENTRY node, then the local symbol
 *    table will be pushed and popped, and the effect is similar 
 *    to W2C_Outfile_Translate_Pu().  The file must be open with 
 *    write permission.
 *
 * W2C_Translate_Wn_Str()
 *    Same as W2C_Translate_Wn(), but the output is put into the
 *    given string buffer instead of being appended to a file.
 *
 * W2C_Fini()
 *    Finalization of W2C.  This will terminate all translation 
 *    modules and free up their state information (such as symol-
 *    tables, etc).
 *
 *                  EXPORTED OUTPUT-FILE INTERFACE
 *                  ------------------------------
 *
 * This is the easiest interface for using whirl2c, and it
 * maintains output file status and produces a uniform output
 * format based on the -CLIST options.  There are strict rules
 * as to how this interface interacts with the lower-level 
 * interface described above.
 *
 * W2C_Outfile_Init()
 *    Initializes the output-files for whirl2c, based on the
 *    command-line options.  Will call W2C_Init() is this has
 *    not already been done.  Optionally, the file-level 
 *    declarations may be emitted to the generated header file.
 *
 * W2C_Outfile_Translate_Pu()
 *    Translates a PU, presupposing W2C_Outfile_Init() has been 
 *    called.  The PU will be lowered (for Fortran) and the 
 *    output C code will be appended to the output files.
 *
 * W2C_Output_Fini()
 *    Finalizes a W2C translation by closing the output-files, and
 *    calling W2C_Fini().  Optionally, this may cause common blocks
 *    (for Fortran-to-C translation) to be appended to the generated
 *    header-file.  Call this before the Irb file is closed!
 *
 *        INTERACTION BETWEEN OUTPUT-FILE AND LOW-LEVEL INTERFACE
 *        -------------------------------------------------------
 *
 * 1) There is no ordering constraint between the Init routines.
 *    A redundant call to an init routine is simply ignored.
 *
 * 2) All "Outfile" routines require a W2C_Outfile_Init() call.
 *
 * 3) All "low-level" routines require a W2C_Init() or a 
 *    W2C_Outfile_Init() call.
 *
 * 4) A call to W2C_Push_PU() may be followed by a call to 
 *    W2C_Outfile_Translate_Pu(), in which case the scope will 
 *    not be pushed again and will not be popped before 
 *    W2C_Outfile_Translate_Pu() returns.  However, this is
 *    only valid when the push is with the intent to translate
 *    the complete function body!
 *
 * 5) W2C_Fini() has no effect if W2C_Outfile_Init() was called,
 *    since finalization must then be done with a call to
 *    W2C_Output_Fini().
 *
 * 6) Initialization may again occur after a finalization for
 *    a second whirl2c translation.   Two whirl2c translation
 *    tasks cannot be active simultaneously within the same process,
 *    but the same WHIRL tree may be translated multiple times
 *    in a given sequential sequence by the same translation 
 *    process.
 *
 * ====================================================================
 * ====================================================================
 */

           /* Flags set by W2C_Process_Command_Line() */
           /*-----------------------------------------*/

extern BOOL W2C_Verbose;         /* Show translation information */
extern BOOL W2C_No_Pragmas;      /* Do not emit pragmas */
extern BOOL W2C_Emit_Adims;      /* Emit comments for array dimensions */
extern BOOL W2C_Emit_Prefetch;   /* Emit comments for prefetches */
extern BOOL W2C_Emit_All_Regions;/* Emit cmplr-generated regions */
extern BOOL W2C_Emit_Linedirs;   /* Emit preprocessing line-directives */
extern BOOL W2C_Emit_Frequency;  /* Emit feedback frequency information */
extern BOOL W2C_Emit_Cgtag;      /* Emit codegen tags for loop */
extern BOOL W2C_Lower_Fortran;   /* Lower Fortran intrinsics and io */
extern BOOL W2F_Emit_Omp;        /* Force OMP pragmas wherever possible */


           /* External data set through the API or otherwise */
           /*------------------------------------------------*/

extern WN_MAP        W2C_Frequency_Map;    /* Frequency mapping */
extern BOOL          W2C_Cplus_Initializer;/* Whether to call C++ init */


                     /* Files */
                     /*-------*/

typedef enum W2C_File_Kind
{
   W2C_ORIG_FILE, /* Original source file (input file) */
   W2C_DOTH_FILE, /* W2C generated .h file */
   W2C_DOTC_FILE, /* W2C generated .c file */
   W2C_LOC_FILE,  /* W2C generated source-to-source location mapping file */
   W2C_NUM_FILES  /* Number of elements in this enumeration */
} W2C_FILE_KIND;

extern FILE *W2C_File[W2C_NUM_FILES];


                     /* Exported Functions */
                     /*--------------------*/

  /* Note that we also export some utilities from W2CF_symtab.h */

extern BOOL W2C_Should_Emit_Nested_PUs(void);
extern void W2C_Process_Command_Line(INT phase_argc, const char* const phase_argv[],
				     INT argc, const char* const argv[]);
extern void W2C_Init(void);
extern void W2C_Fini(void);
extern void W2C_Push_PU(const WN *pu, WN *body_part_of_interest);
extern void W2C_Pop_PU(void);

extern void W2C_Set_Frequency_Map(WN_MAP frequency_map);

extern const char *W2C_Get_Transformed_Src_Path(void);

extern void W2C_Translate_Global_Types(FILE *outfile);
extern void W2C_Translate_Global_Defs(FILE *outfile);

extern const char *W2C_Object_Name(const ST *func_st);

extern void W2C_Translate_Stid_Lhs(char       *strbuf,
				   UINT        bufsize,
				   const ST   *stid_st, 
				   mINT64      stid_ofst, 
				   TY_IDX      stid_ty, 
				   TYPE_ID     stid_mtype);
extern void W2C_Translate_Istore_Lhs(char       *strbuf, 
				     UINT        bufsize,
				     const WN   *lhs,
				     mINT64      istore_ofst, 
				     TY_IDX      istore_addr_ty, 
				     TYPE_ID     istore_mtype);

extern void W2C_Translate_Wn(FILE *outfile, const WN *wn);
extern void W2C_Translate_Wn_Str(char *strbuf, UINT bufsize, const WN *wn);

extern void W2C_Outfile_Init(BOOL emit_global_decls);
extern void W2C_Outfile_Translate_Pu(WN *pu, BOOL emit_global_decls);
extern void W2C_Outfile_Fini(BOOL emit_global_decls);

extern void W2C_Cleanup(void);

}
#endif /* w2c_driver_INCLUDED */
