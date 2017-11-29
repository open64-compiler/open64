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


/* c-or-c++ */

#ifndef _whirl2src_h_included
#define _whirl2src_h_included

/***********************************************************************
 *
 * This set of routines is an interface to whirl2c/whirl2f,
 * and can be used to print whirl in source form.
 *
 * The interface comes in two flavors:
 *  In the common case you would like to reproduce the WHIRL in 
 *  the same language as the original source (either C, C++, or Fortran).
 *  To do so, you need to do the following:
 *      Whirl2Src_Init (func_nd);   // once in the beginning 
 *                                  // func_nd is the current PU.
 *                                  // Must be called for each PU
 *                                  // (multiple calls are harmless)
 *
 *      Whirl2Src_Emit (fp, wn)     // emit wn in src form into FILE* fp
 *      ...
 *      Whirl2Src_Emit (fp, wn)     // ditto, as many Emits as you like
 *
 *
 * If, rather than generating the original source language you 
 * specifically wish one of whirl2c/whirl2f, you use exactly
 * the same interface as above, except the names of the routines change
 * to the following:
 *      Whirl2C_Init (func_nd)
 *      Whirl2C_Emit (fp, wn)
 * and
 *      Whirl2F_Init (func_nd)
 *      Whirl2F_Emit (fp, wn)
 *
 * Some requirements and restrictions are:
 *
 * The whirl2c/f .so are loaded only if you actually call one of these
 * routines. In that case there must be a whirl2c/f .so in your
 * LD_LIBRARY_PATH.
 *
 * Although whirl2c and whirl2f allow printing to either a FILE* 
 * or to a char* buffer, the interface here currently allows printing 
 * to a FILE* only.
 *
 * There are restrictions on what can and cannot be printed. If you
 * need to do something fancier, then you can use specific routines
 * within whirl2c/whirl2f. Some of those routines which are used by 
 * prefetching with the -ls option are included below.
 *
 *
 * Exported functions:
 * -------------------
 *
 *  void Whirl2Src_Init (WN* func_nd);
 *      Based on the source language of the current PU (supplied in
 *      func_nd) (a) load the appropriate DSO (whirl2c/whirl2f) if
 *      not already loaded, and (b) initialize it to process nodes
 *      from the supplie PU (func_nd).
 *
 *  extern void Whirl2Src_Emit (FILE* fp, WN* wn);
 *      Emit the supplied whirl (wn) in appropriate source form
 *      into the supplied FILE*.
 *
 * extern void Whirl2C_Init (WN* func_nd);
 *      Same as Whirl2Src_Init, except use C as the target language.
 *
 * extern void Whirl2C_Emit (FILE* fp, WN* wn);
 *      Emit WHIRL as C code into FILE*.
 *
 * extern void Whirl2F_Init (WN* func_nd);
 *      Same as Whirl2Src_Init, except use Fortran as the target language.
 *
 * extern void Whirl2F_Emit (FILE* fp, WN* wn);
 *      Emit WHIRL as Fortran code into FILE*.
 *     
 *
 *
 ***********************************************************************/

#include "defs.h"
#include "wn.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Whirl2C functions */
extern void W2C_Cleanup(void);
extern void W2C_Init(void);
extern void W2C_Outfile_Fini(BOOL emit_global_decls);
extern void W2C_Outfile_Init(BOOL emit_global_decls);
extern void W2C_Outfile_Translate_Pu(WN *pu, BOOL emit_global_decls);
extern void W2C_Pop_PU(void);
extern void W2C_Process_Command_Line(INT phase_argc, 
                                     const char * const phase_argv[], 
                                     INT argc, 
                                     const char * const argv[]);
extern void W2C_Push_PU(const WN *pu, WN *body_part_of_interest);
extern void W2C_Set_Frequency_Map(WN_MAP frequency_map);
extern BOOL W2C_Should_Emit_Nested_PUs(void);
extern void W2C_Translate_Istore_Lhs(char *strbuf,
                                     UINT bufsize, 
                                     const WN* lhs, 
                                     mINT64 istore_ofst, 
                                     TY_IDX istore_addr_ty,
                                     TYPE_ID istore_mtype);
extern void W2C_Translate_Wn(FILE *outfile, const WN *wn);
extern void W2C_Translate_Wn_Str(char *strbuf, UINT bufsize, const WN *wn);

/* Whirl2F functions */
extern void W2F_Cleanup(void);
extern void W2F_Init(void);
extern void W2F_Outfile_Fini(void);
extern void W2F_Outfile_Init(void);
extern void W2F_Outfile_Translate_Pu(WN *pu);
extern void W2F_Pop_PU(void);
extern void W2F_Process_Command_Line(INT phase_argc, 
                                     const char *phase_argv[],
                                     INT argc, 
                                     const char *argv[]);
extern void W2F_Push_PU(WN *pu, WN *body_part_of_interest);
extern void W2F_Set_Frequency_Map(WN_MAP frequency_map);
extern BOOL W2F_Should_Emit_Nested_PUs(void);
extern void W2F_Translate_Istore_Lhs(char *strbuf, 
                                     UINT bufsize,
                                     WN* lhs, 
                                     mINT64 istore_ofst, 
                                     TY_IDX istore_addr_ty, 
                                     TYPE_ID istore_mtype);
extern void W2F_Translate_Wn(FILE *outfile, WN *wn);
extern void W2F_Translate_Wn_Str(char *strbuf, UINT bufsize, WN *wn);

/* Interface functions for LNO */
extern void Whirl2Src_Init (WN* func_nd);
extern void Whirl2Src_Emit (FILE* fp, WN* wn);

extern void Whirl2C_Init (WN* func_nd);
extern void Whirl2C_Emit (FILE* fp, WN* wn);

extern void Whirl2F_Init (WN* func_nd);
extern void Whirl2F_Emit (FILE* fp, WN* wn);

#ifdef __cplusplus
}
#endif

#endif /* _whirl2src_h_included */
