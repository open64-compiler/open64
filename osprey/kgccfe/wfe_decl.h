/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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


/* WFE == WHIRL Front End */
/* translate gnu decl trees to whirl */

#ifndef wfe_decl_INCLUDED
#define wfe_decl_INCLUDED

#ifndef __cplusplus
typedef int INT;
typedef long long INT64;
typedef int TY_IDX;
#ifdef KEY
typedef int BOOL;
typedef unsigned int UINT;
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* turn a file-scope asm into a hacked-up WHIRL function */
extern void WFE_Assemble_Asm(char *);

/* expand gnu function decl tree into symtab & whirl */
extern void WFE_Start_Function (tree fndecl);

/* called after function body processed, to write out the PU */
extern void WFE_Finish_Function (void);

/* called for each initialized variable */
extern void WFE_Initialize_Decl (tree decl);

#ifdef KEY
// For initialization of any variables  except globals.
/* called for each initialized variable */
extern void WFE_Initialize_Nested_Decl (tree decl);

// Handle to get CURRENT_SYMTAB i.e., current scope level
extern INT WFE_Get_Current_Scope ();

extern bool defer_function;

// Initialize a vector.
extern void Traverse_Aggregate_Vector_Const (ST *, tree, BOOL, UINT);
#endif /* KEY */

/* called for each aggregate initialization */
extern void WFE_Start_Aggregate_Init (tree decl);

/* add padding to aggregate initialization */
extern void WFE_Add_Aggregate_Init_Padding (INT size);

/* add integer to aggregate initialization */
extern void WFE_Add_Aggregate_Init_Integer (INT64 val, INT size);

/* add double to aggregate initialization */
extern void WFE_Add_Aggregate_Init_Real (REAL_VALUE_TYPE val, INT size);

/* add string to aggregate initialization */
extern void WFE_Add_Aggregate_Init_String (const char *s, INT size);

/* add address to aggregate initialization */
extern void WFE_Add_Aggregate_Init_Address (tree t);

/* finish aggregate init, and set size if not previously known */
extern void WFE_Finish_Aggregate_Init (void);

/* generate a temp with extension 'name' having the initialization as specified
   by 'init' */
extern ST *WFE_Generate_Temp_For_Initialized_Aggregate (tree init, char *name);

/* handle __attribute__ ((alias)) */
extern void WFE_Assemble_Alias (tree decl, tree target);

/* handle __attribute__ ((constructor)) */
extern void WFE_Assemble_Constructor (const char *name);

/* handle __attribute__ ((destructor)) */
extern void WFE_Assemble_Destructor (const char *name);

/* call this routine when have a decl that doesn't have an initialization */
extern void WFE_Decl (tree decl);

/* call this routine to determine the return address ST at specified level */
extern ST *WFE_Get_Return_Address_ST (int level);

/* call this routine to save the SP for first alloca in a scope */
extern ST *WFE_Alloca_0 (void);

/* call this routine to assign ST for VLA as well as allocate space for it */
extern ST *WFE_Alloca_ST (tree decl);

/* call this routine to deallocate STs for VLA */
extern void WFE_Dealloca (ST *alloca0_st, tree vars);

/* call this routine to record variables assigned to registers using asm */
extern void WFE_Record_Asmspec_For_ST (tree decl, const char *asmspec, int reg);

/* call this routine to resolve conflicts between duplicate declarations */
extern void WFE_Resolve_Duplicate_Decls (tree olddecl, tree newdecl);

/* call this routine to process pragma weak on encountering pragma */
extern void WFE_Add_Weak();

/* call this routine to process pragma weak declarations at end */
extern void WFE_Weak_Finish();

/* variable to keep track track of ST to be used for varargs */
extern ST *WFE_Vararg_Start_ST;

#ifdef PATHSCALE_MERGE
#ifdef REAL_VALUE_TYPE
float WFE_Convert_Internal_Real_to_IEEE_Single(REAL_VALUE_TYPE);
double    WFE_Convert_Internal_Real_to_IEEE_Double(REAL_VALUE_TYPE);
long double WFE_Convert_Internal_Real_to_IEEE_Double_Extended(REAL_VALUE_TYPE);
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif
