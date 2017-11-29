/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 * File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.2.2 release.
 */

/* WFE == WHIRL Front End */
/* translate gnu decl trees to whirl */

#ifndef wfe_decl_INCLUDED
#define wfe_decl_INCLUDED

extern void Init_Deferred_Function_Stack(void);
extern void Init_Deferred_Decl_Init_Stack(void);

/* expand namespace scope declaration into symtab and whirl */
extern BOOL expanding_function_definition;
extern "C" void WGEN_Expand_Decl (gs_t decl, BOOL can_skip);

/* called after function body processed, to write out the PU */
extern void WGEN_Finish_Function (void);

/* called for each initialized variable */
extern void WGEN_Initialize_Decl (gs_t decl);

#ifdef KEY
// For initialization of any variables  except globals.
// Called for each initialized variable.
extern void WGEN_Initialize_Nested_Decl (gs_t decl);

// Add a VAR_DECL typeinfo to be emitted
extern void gxx_emits_typeinfos (gs_t);

// Add VAR_DECLs and TYPE_DECLs that are to be expanded last.
extern void defer_decl (gs_t);

// Add struct fields whose type we want to expand last.
extern void defer_field (gs_t, FLD_HANDLE);

// Add type whose DST info we want to create last.
extern void defer_DST_type (gs_t, TY_IDX, TY_IDX);

// Add DSTs for the defered types.
extern void add_deferred_DST_types();

// Interface description in .cxx file
extern void template_substituted (gs_t, gs_t, gs_t);
extern gs_t get_substituted (gs_t, gs_t);
extern void push_mp_local_vars (gs_t);
extern gs_t pop_mp_local_vars (void);

// Initialize a vector.
#ifdef NEW_INITIALIZER
extern void Traverse_Aggregate_Vector_Const (WN *, gs_t, BOOL, UINT);
#else
extern void Traverse_Aggregate_Vector_Const (ST *, gs_t, BOOL, UINT);
#endif
// Handle initialization through a modify_expr.
extern void WGEN_Process_Initialization (gs_t);
// Return TRUE if STR is attribute ATTRIB.
extern BOOL is_attribute (const char * str, gs_t attrib);
#endif /* KEY */

/* generate a temp with extension 'name' having the initialization as specified
   by 'init' */
extern ST *WGEN_Generate_Temp_For_Initialized_Aggregate (gs_t init, char *name);
#ifdef NEW_INITIALIZER
extern ST *WGEN_Generate_Initialized_Aggregate(WN *target, gs_t init);
#endif

/* handle __attribute__ ((alias)) */
extern BOOL WGEN_Assemble_Alias (gs_t decl, gs_t target);

/* handle __attribute__ ((constructor)) */
extern void WGEN_Assemble_Constructor (char *name);

/* handle __attribute__ ((destructor)) */
extern void WGEN_Assemble_Destructor (char *name);

/* call this routine when have a decl that doesn't have an initialization */
extern void WGEN_Decl (gs_t decl);

/* call this routine to determine the return address ST at specified level */
extern ST *WGEN_Get_Return_Address_ST (int level);

/* call this routine to save the SP for first alloca in a scope */
extern ST *WGEN_Alloca_0 (void);
/* call this routine to deallocate STs for VLA */
extern void WGEN_Dealloca (ST *, vector<ST*> *);

/* call this routine to resolve conflicts between duplicate declarations */
extern void WGEN_Resolve_Duplicate_Decls (gs_t olddecl, gs_t newdecl);

/* call this routine to mark all the symbols in the weak decls list weak */
extern "C" void WGEN_Weak_Finish(void);

#ifdef KEY
/* call this routine to process leftover symbols with alias targets */
extern "C" void WGEN_Alias_Finish(void);
#endif

/* get the current function declaration.  This just comes from a static
 * global variable in the absence of nested function declarations.
 */

extern gs_t Current_Function_Decl(void);

#ifdef KEY
/* get the current function's entry wn.  This just comes from a static
 * global variable in the absence of nested function declarations.
 */
extern WN *Current_Entry_WN(void);

/* The initializer for the named return value object.  Expand this in place of
 * the DECL_INITIAL in the object's VAR_DECL.
 */
extern gs_t named_ret_obj_initializer;
#endif
/* KEY: ST to represent EXC_PTR_EXPR if C++ exceptions are disabled */
extern ST * Dummy_Exc_Ptr_Expr;
#endif

