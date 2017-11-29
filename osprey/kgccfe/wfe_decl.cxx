/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
 */


/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
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


// translate gnu decl trees to whirl

#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "errors.h"
#include "erfe.h"
extern "C" {
#include "gnu_config.h"
}
#ifdef KEY
// To get HW_WIDE_INT ifor flags.h */
#include "gnu/hwint.h"
#include "erglob.h"  // EC_Unimplemented_Feature
#endif /* KEY */
extern "C" {
#include "gnu/flags.h"
#include "gnu/system.h"
#include "gnu/tree.h"
#include "gnu/toplev.h"
#if defined(TARG_SL) || defined(TARG_MIPS)
#include "function.h"
#endif
#include "c-pragma.h"
}
#if defined(TARG_IA32) || defined(TARG_X8664)
// the definition in gnu/config/i386/i386.h causes problem
// with the enumeration in common/com/ia32/config_targ.h
#undef TARGET_PENTIUM
#endif /* TARG_IA32 */
#if defined(TARG_PPC32)
// the definition in gnu/config/ppc32/rs6000.h causes problem
// with the enumeration in common/com/ppc32/config_targ.h
#undef TARGET_POWERPC
#endif /* TARG_PPC32 */

#ifdef KEY 
#ifdef TARG_MIPS
// ABI_N32 is defined in config/MIPS/mips.h and conflicts with 
// common/com/MIPS/config_targ.h
#undef ABI_N32
#endif /* TARG_MIPS */
#endif /* KEY */

#ifdef __MINGW32__
#include "WINDOWS.h"
#endif /* __MINGW32__ */
#include "glob.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "const.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "tree_symtab.h"
#ifdef KEY // get REAL_VALUE_TYPE
extern "C" {
#include "real.h"
#include "c-common.h"
#include "gnu/c-tree.h"
}
#endif // KEY
#include "wfe_decl.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "tree_cmp.h"
#include "targ_sim.h" // PUSH_RETURN_ADDRESS_ON_STACK

extern FILE *tree_dump_file; // for debugging only

extern PU_Info *PU_Tree_Root;
static PU_Info *PU_Info_Table     [258] = {0};
static ST      *Return_Address_ST [258] = {0};
static INT32    Save_Expr_Table   [258] = {0};
extern INT32    wfe_save_expr_stack_last;
static BOOL map_mempool_initialized = FALSE;
static MEM_POOL Map_Mem_Pool;
ST* WFE_Vararg_Start_ST;
#ifdef KEY
bool defer_function = FALSE;
extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID, ST *);
vector <pair<tree, tree> > alias_vector;
static BOOL finish_alias = FALSE;
#endif

// Because we build inito's piecemeal via calls into wfe for each piece,
// need to keep track of current inito and last initv that we append to.
static INITO_IDX aggregate_inito = 0;
static INITV_IDX last_aggregate_initv = 0;	
static BOOL not_at_root = FALSE;

static int __ctors = 0;
static int __dtors = 0;

extern "C" tree lookup_name (tree);

/* Generate WHIRL representing an asm at file scope (between
  functions). This is an awful hack. */
void
WFE_Assemble_Asm(char *asm_string)
{
  ST *asm_st = New_ST(GLOBAL_SYMTAB);
  ST_Init(asm_st,
	  Str_To_Index (Save_Str (asm_string),
			Global_Strtab),
	  CLASS_NAME,
	  SCLASS_UNKNOWN,
	  EXPORT_LOCAL,
	  (TY_IDX) 0);

  Set_ST_asm_function_st(*asm_st);

  WN *func_wn = WN_CreateEntry(0,
			       asm_st,
			       WN_CreateBlock(),
			       NULL,
			       NULL);

  /* Not sure how much setup of WN_MAP mechanism, etc. we need to do.
   * Pretty certainly we need to set up some PU_INFO stuff just to get
   * this crazy hack of a FUNC_ENTRY node written out to the .B file.
   */

  /* This code patterned after "wfe_decl.cxx":WFE_Start_Function, and
     specialized for the application at hand. */

#ifdef ASM_NEEDS_WN_MAP
    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    /* set up the mem pool for the map table and predefined mappings */
    if (!map_mempool_initialized) {
        MEM_POOL_Initialize(&Map_Mem_Pool,"Map_Mem_Pool",FALSE);
        map_mempool_initialized = TRUE;
    } else {
        MEM_POOL_Pop(&Map_Mem_Pool);
    }

    MEM_POOL_Push(&Map_Mem_Pool);

    /* create the map table for the next PU */
    (void)WN_MAP_TAB_Create(&Map_Mem_Pool);
#endif

    // This non-PU really doesn't need a symbol table and the other
    // trappings of a local scope, but if we create one, we can keep
    // all the ir_bread/ir_bwrite routines much more blissfully
    // ignorant of the supreme evil that's afoot here.

    FmtAssert(CURRENT_SYMTAB == GLOBAL_SYMTAB,
	      ("file-scope asm must be at global symtab scope."));

    New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

    if (Show_Progress) {
      fprintf (stderr, "Asm(%s)\n", ST_name (asm_st));
      fflush (stderr);
    }
    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, func_wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(asm_st);
    PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (asm_st,/*tree=*/0);
    PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();

    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

    Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;
    else
      PU_Tree_Root = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;

  /* This code patterned after "wfe_decl.cxx":WFE_Finish_Function, and
     specialized for the application at hand. */

    // write out all the PU information
    pu_info = PU_Info_Table [CURRENT_SYMTAB];

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    PU_IDX pu_idx;
    PU &pu = New_PU(pu_idx);
    PU_Init(pu, (TY_IDX) 0, CURRENT_SYMTAB);
    Set_PU_no_inline(pu);
    Set_PU_no_delete(pu);
    Set_ST_pu(*asm_st, pu_idx);

    Write_PU_Info (pu_info);

    // What does the following line do?
    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;

    Delete_Scope(CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
}

extern ST* ap_tmp;
extern ST* vararg_ofst;
extern void
WFE_Start_Function (tree fndecl)
{

    if (CURRENT_SYMTAB != GLOBAL_SYMTAB) {

      Set_PU_uplevel (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
    }

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

#ifdef KEY
    wfe_nesting_stack = wfe_loop_stack = wfe_case_stack = wfe_cond_stack = NULL;
#endif // KEY

    /* set up the mem pool for the map table and predefined mappings */
    if (!map_mempool_initialized) {
        MEM_POOL_Initialize(&Map_Mem_Pool,"Map_Mem_Pool",FALSE);
        map_mempool_initialized = TRUE;
    } else {
        MEM_POOL_Pop(&Map_Mem_Pool);
    }

    MEM_POOL_Push(&Map_Mem_Pool);

    /* create the map table for the next PU */
    (void)WN_MAP_TAB_Create(&Map_Mem_Pool);

#ifdef KEY
    if (CURRENT_SYMTAB > 1)
#if defined(TARG_SL)
      ErrMsg (EC_Unimplemented_Feature, "Nested functions",
        Orig_Src_File_Name?Orig_Src_File_Name:Src_File_Name, lineno);
#else
      ErrMsg (EC_Unimplemented_Feature, "Nested functions");
#endif
#endif
    New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

    // handle VLAs in the declaration
    WN *vla_block = WN_CreateBlock ();
    WFE_Stmt_Push (vla_block, wfe_stmk_func_body, Get_Srcpos());

    ST        *func_st;
    ST_EXPORT  eclass;

    //This is a work around to avoid redundant save/restore gp
    if (DECL_WEAK(fndecl))
      eclass = EXPORT_PREEMPTIBLE;
    else if (!TREE_PUBLIC(fndecl))
      eclass = EXPORT_LOCAL;
    else if (DECL_INLINE(fndecl))
      eclass = EXPORT_PROTECTED;
    else
      eclass = EXPORT_PREEMPTIBLE;

#ifdef KEY
    bool extern_inline = FALSE;
#endif
    if (DECL_INLINE (fndecl) && TREE_PUBLIC (fndecl)) {
      if (DECL_EXTERNAL (fndecl) && DECL_ST2 (fndecl) == 0) {
        // encountered first extern inline definition
        ST *oldst = DECL_ST (fndecl);
        DECL_ST (fndecl) = 0;
        func_st =  Get_ST (fndecl);
        DECL_ST (fndecl) = oldst;
        DECL_ST2 (fndecl) = func_st;
#ifdef KEY // bugs 2178, 2152
	extern_inline = TRUE;
#if defined(TARG_MIPS) && !defined(TARG_SL)
       eclass = EXPORT_LOCAL;
#else       
	eclass = EXPORT_PREEMPTIBLE; // bug 14367
#endif	
#endif // KEY
      }
      else {
        // encountered second definition, the earlier one was extern inline
        func_st = Get_ST (fndecl);
        DECL_ST2 (fndecl) = 0;
      }
    }
    else
      func_st = Get_ST (fndecl);

    Set_ST_sclass (func_st, SCLASS_TEXT);
    Set_PU_lexical_level (Pu_Table [ST_pu (func_st)], CURRENT_SYMTAB);
    Set_PU_c_lang (Pu_Table [ST_pu (func_st)]);

#ifdef KEY
    if (DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (fndecl))
      Set_PU_no_instrument (Pu_Table [ST_pu (func_st)]);  // Bug 750
    if (DECL_DECLARED_INLINE_P(fndecl))
      Set_PU_is_marked_inline (Pu_Table [ST_pu (func_st)]);
#endif

    if (DECL_INLINE(fndecl)) {
      Set_PU_is_inline_function (Pu_Table [ST_pu (func_st)]);
      wfe_invoke_inliner = TRUE;
    }
    Set_ST_export (func_st, eclass);

#ifdef KEY
    if (extern_inline)
    {
    	Set_PU_is_extern_inline (Pu_Table [ST_pu (func_st)]); //bugs 2178, 2152
	if (optimize) // bug 2218
	    Set_PU_must_inline (Pu_Table [ST_pu (func_st)]);
    }

    // check and set the main function of program
    if (strcmp (ST_name (func_st), "main") == 0) {
      PU& pu = Pu_Table [ST_pu (func_st)];
      Set_PU_is_mainpu (pu);
      Set_PU_no_inline (pu);
    }

    // bug 2395
    if (DECL_NOINLINE_ATTRIB (fndecl)) {
	Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
#ifdef TARG_NVISA
        TY_IDX func_ty = ST_pu_type(func_st);
        if (!TY_has_prototype (func_ty)) {
          // must have prototype
          ErrMsg(EC_Inline_Prototype, ST_name(func_st));
          Clear_PU_no_inline(ST_pu(func_st));
          Set_PU_must_inline(ST_pu(func_st));
        }

        RETURN_INFO return_info = Get_Return_Info (TY_ret_type(func_ty), No_Simulated);
        if (RETURN_INFO_return_via_first_arg(return_info)) {
          // won't handle calls that return > 4 items.
          ErrMsg(EC_Inline_Return_Values, ST_name(func_st));
          Clear_PU_no_inline(ST_pu(func_st));
          Set_PU_must_inline(ST_pu(func_st));
        }

        PLOC ploc = Setup_Input_Parameter_Locations (func_ty);
        TYLIST_IDX tl = TY_parms(func_ty);
        if (tl != (TYLIST_IDX) NULL) {
          INT i = 0;
          for (; TYLIST_ty(tl); tl = TYLIST_next(tl)) {
            TY_IDX ty = TYLIST_ty(tl);
            if (TY_kind(ty) == KIND_POINTER) {
              // don't allow pointer params in non-inlined functions
              // (cause need to know source of pointer).
              ErrMsg(EC_Inline_Ptr, ST_name(func_st));
              Clear_PU_no_inline(ST_pu(func_st));
              Set_PU_must_inline(ST_pu(func_st));
            }
            ploc = Get_Input_Parameter_Location (ty);
            ploc = First_Input_PLOC_Reg (ploc, ty);
            while (PLOC_is_nonempty(ploc)) {
              ++i;
              if (PLOC_on_stack(ploc) || i > MAX_NUMBER_OF_REGISTER_PARAMETERS)
              {
                // won't handle more than 8 parameter registers
                ErrMsg(EC_Inline_Parameters, ST_name(func_st));
                Clear_PU_no_inline(ST_pu(func_st));
                Set_PU_must_inline(ST_pu(func_st));
                break;
              }
              ploc = Next_Input_PLOC_Reg (ploc);
            }
            // avoid repeating warning
            if (!PU_no_inline(ST_pu(func_st))) break;
          }
        }
    }
    else if (!ST_in_global_mem(func_st)) {
        // default is to inline everything except global funcs
        Set_PU_must_inline (ST_pu (func_st));
#endif
    }

    // bug 2646
    // If there is an 'always_inline' attribute, and the function definition
    // is before the call(s), GNU fe will do it. If the definition is after
    // any call, we need to handle it here.
    if (DECL_ALWAYS_INLINE_ATTRIB (fndecl))
    	Set_PU_must_inline (Pu_Table [ST_pu (func_st)]);
// Fix Bug# 45 (comments below)
    Scope_tab [Current_scope].st = func_st;
#endif // KEY


#if !defined(TARG_NVISA)
    if (lookup_attribute("used", DECL_ATTRIBUTES (fndecl)))  // bug 3697
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
#else
    // TREE_USED means a reference somewhere, not attribute "used"
    // so instead do below check of attribute.
    if (lookup_attribute("global", DECL_ATTRIBUTES (fndecl)) != NULL)
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
#endif

    if (Show_Progress) {
      fprintf (stderr, "Compiling %s \n", ST_name (func_st));
      fflush (stderr);
    }

    INT num_args = 0;
    tree pdecl;
    for (pdecl = DECL_ARGUMENTS (fndecl); pdecl; pdecl = TREE_CHAIN (pdecl)) {
      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else
	++num_args;
    }

#ifndef KEY
// Set the func_st as early as possible (as above)
// Plumhall c65.c failed since Get_TY above ultimately called Get_Current_PU,
// but gave a seg-fault since st was not set. Bug# 45
    Scope_tab [Current_scope].st = func_st;
#endif // !KEY

    WN *body, *wn;
    body = WN_CreateBlock ( );
    wn = WN_CreateEntry ( num_args, func_st, body, NULL, NULL );

#if defined(TARG_PPC32)
	void Set_Current_PU_WN(WN * wn);
	Set_Current_PU_WN(wn);
#endif

    /* from 1..nkids, create idname args */
    INT i = 0;
    for (pdecl = DECL_ARGUMENTS (fndecl); pdecl; pdecl = TREE_CHAIN (pdecl) )
    {
      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      ST *st = Get_ST(pdecl);
      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else {
        if (TY_mtype (arg_ty_idx) == MTYPE_F4 &&
            !TY_has_prototype (ST_pu_type (func_st)))
          Set_ST_promote_parm (st);
          WN_kid(wn,i) = WN_CreateIdname ( 0, ST_st_idx(st) );
          ++i;
      }
    }

    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(func_st);
#ifdef KEY
    if ((Debug_Level >= 2) && (TREE_CODE (fndecl) == FUNCTION_DECL) && 
    	(have_dst_idx (fndecl)))
    {
	DST_INFO_IDX id;
        cp_to_dst_from_tree (&id, &DECL_DST_IDX (fndecl));
	PU_Info_pu_dst (pu_info) = id;
    }
    else
    {
    	PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (func_st,fndecl);
    	if ((Debug_Level >= 2) && (TREE_CODE (fndecl) == FUNCTION_DECL))
	{
	    struct mongoose_gcc_DST_IDX var_idx;
	    cp_to_tree_from_dst (&var_idx, &PU_Info_pu_dst (pu_info));
	    DECL_DST_IDX (fndecl) = var_idx;
    	}
    }
#else
    PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (func_st,fndecl);
#endif // KEY
    PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();

    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

    Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;

    else
    if (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1)
      PU_Tree_Root = pu_info;

    else
      PU_Info_child (PU_Info_Table [CURRENT_SYMTAB -1]) = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;
    Save_Expr_Table [CURRENT_SYMTAB] = wfe_save_expr_stack_last;

    WFE_Stmt_Pop (wfe_stmk_func_body);

    WFE_Stmt_Push (wn, wfe_stmk_func_entry, Get_Srcpos());
    WFE_Stmt_Push (body, wfe_stmk_func_body, Get_Srcpos());

    wn = WN_CreatePragma (WN_PRAGMA_PREAMBLE_END, (ST_IDX) NULL, 0, 0);
    WFE_Stmt_Append (wn, Get_Srcpos());
    WFE_Stmt_Append (vla_block, Get_Srcpos());

    WFE_Vararg_Start_ST = NULL;
    ap_tmp = NULL;
    vararg_ofst = NULL;
#ifndef KEY
// current_function_varargs does not exist any more, current_function_stdarg
// is still available.
    if (current_function_varargs) {
      // the function uses varargs.h
      // throw off the old type declaration as it did not 
      // take into account any arguments
      PU& pu = Pu_Table[ST_pu (func_st)];
      TY_IDX ty_idx;
      TY &ty = New_TY (ty_idx);
      TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO);
      Set_TY_align (ty_idx, 1);
      TYLIST tylist_idx;
      Set_TYLIST_type (New_TYLIST (tylist_idx),
                       Get_TY(TREE_TYPE(TREE_TYPE(fndecl))));
      Set_TY_tylist (ty, tylist_idx);
      for (pdecl = DECL_ARGUMENTS (fndecl); pdecl; pdecl = TREE_CHAIN (pdecl) ) {
	WFE_Vararg_Start_ST = Get_ST(pdecl);
        Set_TYLIST_type (New_TYLIST (tylist_idx), ST_type(WFE_Vararg_Start_ST));
      }
      Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
      Set_TY_is_varargs (ty_idx);
      Set_PU_prototype (pu, ty_idx);
    }
#endif // !KEY
}

#ifdef KEY
// We are not writing out this PU since it has been deferred for after
// processing. So delete this PU from the PU tree.
// TODO: Update for nested functions.
static bool
Search_and_Remove_PU_Info (PU_Info *pu_tree, PU_Info *search, PU_Info *prev=0)
{
    PU_Info *prev_pu = NULL;
    for (PU_Info *pu = pu_tree; pu; pu = PU_Info_next (pu))
    {
	if (pu == search)
	{ // Found it!
	  if (prev_pu)
	  {
	    // If pu is not the last PU, we need to traverse the list to
	    // find the last PU, and assign that pu to 
	    // PU_Info_Table[CURRENT_SYMTAB]
	    // For non-nested PUs, it should be the last PU
	    Is_True (PU_Info_next (pu) == NULL, ("PU should be last PU in list"));
	    // bug 2107
	    PU_Info_Table [CURRENT_SYMTAB] = prev_pu;
	    PU_Info_next (prev_pu) = PU_Info_next (pu);
	  }
	  else
	  { // Must be the first child
	    FmtAssert (prev, ("No previous PU found"));
	    PU_Info_child (prev) = PU_Info_next (pu);
	  }
	  PU_Info_next (pu) = NULL;
	  return TRUE;
	}

	if (PU_Info_child (pu))
	{
	  DevWarn ("Nested PU's not supported");
	  if (Search_and_Remove_PU_Info (PU_Info_child (pu), search, pu))
	  	return TRUE;
	}
    	prev_pu = pu;
    }
    Fail_FmtAssertion ("Deferred PU not found in PU tree");
    return FALSE;
}
#endif

extern void
WFE_Finish_Function (void)
{
    WFE_Check_Undefined_Labels ();
    PU_Info *pu_info = PU_Info_Table [CURRENT_SYMTAB];
    wfe_save_expr_stack_last = Save_Expr_Table [CURRENT_SYMTAB];

    if (CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) {

      DevWarn ("Encountered nested function");
      Set_PU_is_nested_func (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
    }

    // write out all the PU information
    WN *wn = WFE_Stmt_Pop (wfe_stmk_func_body);

    // Insert a RETURN at the end if it does not exist
    if (WN_last (wn) == NULL ||
        (WN_operator (WN_last (wn)) != OPR_RETURN &&
         WN_operator (WN_last (wn)) != OPR_RETURN_VAL)) {
      WN *ret_wn = WN_CreateReturn();
#ifdef TARG_NVISA
      // insert separate line# for return for debugger
      WN_Set_Linenum (ret_wn, Get_Srcpos());
#endif
      WN_INSERT_BlockLast (wn, ret_wn);
    }

    WN *func_wn = WFE_Stmt_Pop (wfe_stmk_func_entry);
    if (PU_has_syscall_linkage (Get_Current_PU ())) {
      Set_PU_no_inline (Get_Current_PU ());
/*
      WN     *pragmas_wn = WN_func_pragmas (func_wn);
      TCON    tcon       = Host_To_Targ_String (MTYPE_STRING, "-O0", 3);
      TY_IDX  ty_idx     = Be_Type_Tbl(MTYPE_I1);
      ST     *st         = Gen_String_Sym (&tcon, ty_idx, FALSE);
      wn = WN_CreatePragma (WN_PRAGMA_OPTIONS, st, 0, 0);
      WN_INSERT_BlockLast (pragmas_wn, wn);
      fdump_tree(stderr, func_wn);
*/
    }

#ifdef KEY
    // bug 1940
    if (defer_function)	// Delete the WN tree
      IPA_WN_DELETE_Tree (Current_Map_Tab, func_wn);
#endif

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

#ifdef KEY
    if (!defer_function)
      Write_PU_Info (pu_info);
    else
    { // We are not writing this PU now, so delete it from PU tree and fix
      // up a few things.
      Set_Max_Region_Id (0);
      if (PU_Tree_Root == pu_info)
      {
	// bug 2107
      	PU_Info_Table [CURRENT_SYMTAB] = PU_Tree_Root = PU_Info_next (pu_info);
	PU_Info_next (pu_info) = NULL;
      }
      else
        Search_and_Remove_PU_Info (PU_Tree_Root, pu_info);
    }
#else
    Write_PU_Info (pu_info);
#endif

    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;

    if (Return_Address_ST [CURRENT_SYMTAB]) {
      Set_PU_has_return_address (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
      Return_Address_ST [CURRENT_SYMTAB] = NULL;
    }

    Delete_Scope (CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
//  if (CURRENT_SYMTAB > GLOBAL_SYMTAB)
//    Current_pu = &Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];
}


void
WFE_Start_Aggregate_Init (tree decl)
{
  if (TREE_STATIC(decl)) {
	ST *st = Get_ST(decl);
	Set_ST_is_initialized(st);
	if (ST_sclass(st) == SCLASS_UGLOBAL ||
	    ST_sclass(st) == SCLASS_EXTERN  ||
	    ST_sclass(st) == SCLASS_COMMON)
		Set_ST_sclass(st, SCLASS_DGLOBAL);
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
  }
}

void
WFE_Add_Aggregate_Init_Padding (INT size)
{
  if (aggregate_inito == 0) return;
  if (size < 0) return;	// actually happens from assemble_zeroes
  INITV_IDX inv = New_INITV();
  INITV_Init_Pad (inv, size);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Integer (INT64 val, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TYPE_ID mtype;
  if (size == 1) mtype = MTYPE_I1;
  else if (size == 2) mtype = MTYPE_I2;
  else if (size == 4) mtype = MTYPE_I4;
  else if (size == 8) mtype = MTYPE_I8;
  else FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Integer unexpected size"));
  INITV_Init_Integer (inv, mtype, val);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

static void
WFE_Add_Init_Block(void)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv_blk = New_INITV();
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv_blk);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv_blk);
  last_aggregate_initv = inv_blk;
}

#ifdef KEY
float
WFE_Convert_Internal_Real_to_IEEE_Single (REAL_VALUE_TYPE real)
{
  INT32 retval;

  Is_True (sizeof(INT32) == sizeof(float),
    ("The return value from REAL_VALUE_TO_TARGET_SINGLE() should be cast to"
     " a integer with the same size as float"));
  REAL_VALUE_TO_TARGET_SINGLE(real, retval);
  return *(float*)(void*)&retval;
}

double
WFE_Convert_Internal_Real_to_IEEE_Double (REAL_VALUE_TYPE real)
{
  long buffer[4];
  int compact_buffer[8];

  REAL_VALUE_TO_TARGET_DOUBLE (real, buffer);
  WFE_Convert_To_Host_Order(buffer);
  if (sizeof(long) > 4) {
    Is_True (sizeof(long) == 8, ("sizeof(long) shold be 64"));
    Is_True (sizeof(int) == 4, ("sizeof(int) should be 32"));
    for (INT i = 0; i < sizeof(buffer)/sizeof(buffer[0]); i++)
      compact_buffer[i] = (int)buffer[i];
    return *(double*)(void*)&compact_buffer[0];
  }
  return *(double*)(void*)&buffer[0];
}

long double
WFE_Convert_Internal_Real_to_IEEE_Double_Extended (REAL_VALUE_TYPE real)
{
  long buffer[4];
  int compact_buffer[8];

  REAL_VALUE_TO_TARGET_LONG_DOUBLE (real, buffer);
  WFE_Convert_To_Host_Order(buffer);
  if (sizeof(long) > 4) {
    for (INT i = 0; i < sizeof(buffer)/sizeof(buffer[0]); i++)
      compact_buffer[i] = buffer[i];
    return *(long double*)(void*)&compact_buffer[0];
  }
  return *(long double*)(void*)&buffer[0];
}
#endif

void 
WFE_Add_Aggregate_Init_Real (REAL_VALUE_TYPE real, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    tc;
  int     t1;
#ifdef KEY
  long     buffer [4];
#else
  int     buffer [4];
#endif // KEY
  switch (size) {
    case 4:
      tc = Host_To_Targ_Float_4 (MTYPE_F4, WFE_Convert_Internal_Real_to_IEEE_Single (real));
      break;
    case 8:
      tc = Host_To_Targ_Float (MTYPE_F8, WFE_Convert_Internal_Real_to_IEEE_Double (real));
      break;
#ifdef TARG_IA64
    case 16:
      tc = Host_To_Targ_Float_10 (MTYPE_F10, WFE_Convert_Internal_Real_to_IEEE_Double_Extended (real));
      break;
#else
    case 12:
    case 16:
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (real, buffer);
      WFE_Convert_To_Host_Order(buffer);
#ifdef TARG_LOONGSON
      tc = Host_To_Targ_Quad (*(QUAD_TYPE *) &buffer);
#else
      //tc = Host_To_Targ_Quad (*(long double *) &buffer);
      tc = Host_To_Targ_Quad (WFE_Convert_Internal_Real_to_IEEE_Double_Extended (real));
#endif
      break;
#endif
    default:
      FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Real unexpected size"));
      break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
} /* WGE_Add_Aggregate_Init_Real */

void 
WFE_Add_Aggregate_Init_Complex (REAL_VALUE_TYPE rval, REAL_VALUE_TYPE ival, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    rtc;
  TCON    itc;
  int     t1;
#ifdef KEY
  long     buffer [4];
#else
  int     buffer [4];
#endif // KEY	
  switch (size) {
    case 8:
      rtc = Host_To_Targ_Float_4 (MTYPE_F4,
	WFE_Convert_Internal_Real_to_IEEE_Single (rval));
      itc = Host_To_Targ_Float_4 (MTYPE_F4,
	WFE_Convert_Internal_Real_to_IEEE_Single (ival));
      break;
    case 16:
      rtc = Host_To_Targ_Float (MTYPE_F8,
	WFE_Convert_Internal_Real_to_IEEE_Double (rval));
      itc = Host_To_Targ_Float (MTYPE_F8,
	WFE_Convert_Internal_Real_to_IEEE_Double (ival));
      break;
    case 24:
    case 32:
#ifdef TARG_LOONGSON
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (rval, buffer);
      WFE_Convert_To_Host_Order(buffer);
      rtc = Host_To_Targ_Quad (*(QUAD_TYPE *) &buffer);
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (ival, buffer);
      WFE_Convert_To_Host_Order(buffer);
      itc = Host_To_Targ_Quad (*(QUAD_TYPE *) &buffer);
#else
      rtc = Host_To_Targ_Quad (
	WFE_Convert_Internal_Real_to_IEEE_Double_Extended (rval));
      itc = Host_To_Targ_Quad (
	WFE_Convert_Internal_Real_to_IEEE_Double_Extended (ival));
#endif
      break;
    default:
      FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Complex unexpected size"));
      break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(rtc), 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
  inv = New_INITV();
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(itc), 1);
  Set_INITV_next(last_aggregate_initv, inv);
  last_aggregate_initv = inv;
}

void 
WFE_Add_Aggregate_Init_String (const char *s, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_String (inv, (char *) s, size);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Symbol (ST *st, WN_OFFSET offset = 0)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Symoff (inv, st, offset);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Label (LABEL_IDX lab)
{
  DevWarn ("taking address of a label at line %d", lineno);
  Set_PU_no_inline (Get_Current_PU ());
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Label (inv, lab, 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
  Set_LABEL_addr_saved (lab);
}

void
WFE_Add_Aggregate_Init_Address (tree init)
{
  switch (TREE_CODE (init)) {

  case VAR_DECL:
  case FUNCTION_DECL:
	WFE_Add_Aggregate_Init_Symbol (Get_ST (init));
	break;

  case STRING_CST:
	{
	TCON tcon = Host_To_Targ_String (MTYPE_STRING,
				       const_cast<char*>TREE_STRING_POINTER(init),
				       TREE_STRING_LENGTH(init));
	ST *const_st = New_Const_Sym (Enter_tcon (tcon), 
		Get_TY(TREE_TYPE(init)));
      	WFE_Add_Aggregate_Init_Symbol (const_st);
	}
    	break;

  case PLUS_EXPR:
	if ( TREE_CODE(TREE_OPERAND(init,0)) == ADDR_EXPR
	  && TREE_CODE(TREE_OPERAND(init,1)) == INTEGER_CST)
	{
		tree addr_kid = TREE_OPERAND(TREE_OPERAND(init,0),0);
		FmtAssert(TREE_CODE(addr_kid) == VAR_DECL
			|| TREE_CODE(addr_kid) == FUNCTION_DECL,
			("expected decl under plus_expr"));
		WFE_Add_Aggregate_Init_Symbol ( Get_ST (addr_kid),
			Get_Integer_Value(TREE_OPERAND(init,1)) );
	}
	else
	{
		WN *init_wn = WFE_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected decl under plus_expr"));
		WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
					       WN_offset (init_wn));
//Bug 555
#ifdef KEY
                Set_ST_initv_in_other_st (WN_st(init_wn));
#endif
		WN_Delete (init_wn);
	}
	break;

  case INTEGER_CST:
	WFE_Add_Aggregate_Init_Integer (Get_Integer_Value (init), Pointer_Size);
	break;

  case LABEL_DECL:
	{
	 	LABEL_IDX label_idx = WFE_Get_LABEL (init, FALSE);
		WFE_Add_Aggregate_Init_Label (label_idx);
	}
	break;

  default:
	{
#ifndef KEY
		WN *init_wn = WFE_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected operator encountered"));
		WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
					       WN_offset (init_wn));
#else
                int tmp_aggr_inito = aggregate_inito;
                int tmp_last_aggregate_initv = last_aggregate_initv;
                WN *init_wn = WFE_Expand_Expr (init);
                aggregate_inito = tmp_aggr_inito;
                last_aggregate_initv = tmp_last_aggregate_initv;
                WFE_Add_Aggregate_Init_Symbol (WN_st(init_wn));
                aggregate_inito = 0;
//Bug 555
                Set_ST_initv_in_other_st (WN_st(init_wn));
#endif
		WN_Delete (init_wn);
	}
      	break;
  }
} /* WFE_Add_Aggregate_Init_Address */

void
WFE_Finish_Aggregate_Init (void)
{
  if (aggregate_inito == 0) return;
  ST *st = INITO_st(aggregate_inito);
  TY_IDX ty = ST_type(st);
#ifdef KEY
  // Fix bug 412; see check-in comments
  if ((TY_size(ty) == 0 && 
       (TY_kind(ty) != KIND_ARRAY ||
	(TY_kind(ty) == KIND_ARRAY && 
	 !ARB_const_ubnd (TY_arb(ty))))) || 
#else
  if (TY_size(ty) == 0 ||      
#endif 
      (TY_kind(ty) == KIND_ARRAY &&
       !ARB_const_ubnd (TY_arb(ty)) &&
       TY_size(ty) <= Get_INITO_Size(aggregate_inito))) {
	// e.g. array whose size is determined by init;
	// fill in with initv size
	Set_TY_size(ty, Get_INITO_Size(aggregate_inito));
	if (TY_kind(ty) == KIND_ARRAY) {
		Set_ARB_const_ubnd (TY_arb(ty));
		Set_ARB_ubnd_val (TY_arb(ty), 
			(TY_size(ty) / TY_size(TY_etype(ty))) - 1 );
	}
  }
  if (last_aggregate_initv == 0) {
    WFE_Add_Aggregate_Init_Padding (0);
  }
  aggregate_inito = 0;
  not_at_root = FALSE;
}


static BOOL
Has_Non_Constant_Init_Value (tree init)
{
  if (init == NULL) {
	return FALSE;
  }
  switch (TREE_CODE(init)) {
  case CONSTRUCTOR:
	return Has_Non_Constant_Init_Value (CONSTRUCTOR_ELTS(init));
  case TREE_LIST:
	{
	tree p;
	for (p = init; p != NULL; p = TREE_CHAIN(p)) {
		if (Has_Non_Constant_Init_Value (TREE_VALUE(p))) {
			return TRUE;
		}
/*
		if (TREE_CODE(TREE_PURPOSE(p)) == FIELD_DECL
			&& DECL_BIT_FIELD(TREE_PURPOSE(p)))
		{
			// if bitfield, then do each element separately
			// rather than combine into initv field.
			return TRUE;
		}
*/
	}
	return FALSE;
	}
  case INTEGER_CST:
  case REAL_CST:
  case STRING_CST:
	return FALSE;
  case NOP_EXPR:
	if (TREE_CODE(TREE_OPERAND(init,0)) == ADDR_EXPR
    	    && TREE_CODE(TREE_OPERAND(TREE_OPERAND(init,0),0)) == STRING_CST) 
		return FALSE;
	else
		return TRUE;
  default:
	return TRUE;
  }
}

// For a dynamic initialization, we can either
// do a series of moves for each element,
// or we can create a static INITO and structure copy that value.
// GCC allows non-constant initial values, 
// so if any of those exist, we need to assign each element.
// Also, if the init is small we can optimize better
// if we make each element assignment be explicit.
// But otherwise, we create the static INITO since that saves code space.
static BOOL
Use_Static_Init_For_Aggregate (ST *st, tree init)
{
#ifdef KEY
        FmtAssert( CURRENT_SYMTAB != GLOBAL_SYMTAB ||
                   !Has_Non_Constant_Init_Value(init),
	           ("Handle this case") );
        // Bug 1210
	if (CURRENT_SYMTAB == GLOBAL_SYMTAB)
	  return TRUE;
#endif
	if (TY_size(ST_type(st)) <= (2*MTYPE_byte_size(Spill_Int_Mtype))) {
		return FALSE;
	}
	else if (Has_Non_Constant_Init_Value(init)) {
		return FALSE;
	}
	else {
		return TRUE;
	}
}


static void
Add_Initv_For_Tree (tree val, UINT size)
{
	WN *init_block;
	WN * init_wn;

	switch (TREE_CODE(val)) {
	case INTEGER_CST:
		WFE_Add_Aggregate_Init_Integer (
			Get_Integer_Value(val), size);
		break;
	case REAL_CST:
		WFE_Add_Aggregate_Init_Real (
			TREE_REAL_CST(val), size);
		break;
#ifdef PATHSCALE_MERGE
	// bug fix for OSP_149
	case COMPLEX_CST:
		WFE_Add_Aggregate_Init_Complex (TREE_REAL_CST(TREE_REALPART(val)), 
			     TREE_REAL_CST(TREE_IMAGPART(val)), size);
		break;
#endif
	case STRING_CST:
		WFE_Add_Aggregate_Init_String (
			TREE_STRING_POINTER(val), size);
		break;
	case NOP_EXPR:
		tree kid;
		kid = TREE_OPERAND(val,0);
		if (TREE_CODE(kid) == ADDR_EXPR
	    		&& TREE_CODE(TREE_OPERAND(kid,0)) == STRING_CST) 
		{
			kid = TREE_OPERAND(kid,0);
			WFE_Add_Aggregate_Init_Address (kid);
			break;
		}
		// fallthru
	default:
		{
		init_block = WN_CreateBlock ();
                WFE_Stmt_Push (init_block, wfe_stmk_func_body, Get_Srcpos());
		init_wn = WFE_Expand_Expr (val);
                WFE_Stmt_Pop (wfe_stmk_func_body);

		if ((WN_opcode (init_wn) == OPC_I4U4CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U4LDA) ||
		    (WN_opcode (init_wn) == OPC_I8U8CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U8LDA)) {
			WFE_Add_Aggregate_Init_Symbol (WN_st (WN_kid0 (init_wn)),
						       WN_offset (WN_kid0 (init_wn)));
//Bug 555
#ifdef KEY
                        Set_ST_initv_in_other_st (WN_st(WN_kid0 (init_wn)));
#endif
			WN_DELETE_Tree (init_wn);
			break;
		}

		if (WN_operator (init_wn) == OPR_LDA) {
			WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
						       WN_offset (init_wn));
//Bug 555
#ifdef KEY
                        Set_ST_initv_in_other_st (WN_st(init_wn));
#endif
			WN_DELETE_Tree (init_wn);
			break;
		}
#ifdef KEY
		else if (WN_operator (init_wn) == OPR_LDA_LABEL) {
		        tree label_decl = 
			  (TREE_CODE(TREE_OPERAND(val, 0)) == ADDR_EXPR)?
			  TREE_OPERAND (TREE_OPERAND (val, 0), 0):
			  TREE_OPERAND (val, 0);
			LABEL_IDX label_idx = 
			  WFE_Get_LABEL (label_decl, FALSE);
			WFE_Add_Aggregate_Init_Label (label_idx);
			WN_DELETE_Tree (init_wn);
			break;		  
		}		
#endif
		else if (WN_operator(init_wn) == OPR_INTCONST) {
			WFE_Add_Aggregate_Init_Integer (
				WN_const_val(init_wn), size);
			break;
		}
#ifdef KEY // bug 3227
		else if ((WN_opcode (init_wn) == OPC_U4I4CVT ||
		          WN_opcode (init_wn) == OPC_I4U4CVT) &&
			  WN_operator (WN_kid0 (init_wn)) == OPR_INTCONST) {
			WFE_Add_Aggregate_Init_Integer (
				WN_const_val(WN_kid0(init_wn)), size);
			break;		  
		}		
#endif
		// following cases for ADD and SUB are needed because the
		// simplifier may be unable to fold due to overflow in the
		// 32-bit offset field
		else if (WN_operator(init_wn) == OPR_ADD) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid0),
				     WN_offset(kid0) + WN_const_val(kid1));
//Bug 555
#ifdef KEY
                          Set_ST_initv_in_other_st (WN_st(kid0));
#endif
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		 	else if (WN_operator(kid1) == OPR_LDA &&
			    WN_operator(kid0) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid1),
				     WN_offset(kid1) + WN_const_val(kid0));
//Bug 555
#ifdef KEY
                          Set_ST_initv_in_other_st (WN_st(kid1));
#endif
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}
		else if (WN_operator(init_wn) == OPR_SUB) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid0),
				     WN_offset(kid0) - WN_const_val(kid1));
//Bug 555
#ifdef KEY
                          Set_ST_initv_in_other_st (WN_st(kid0));
#endif
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}
		FmtAssert(FALSE, ("unexpected tree code %s", 
			tree_code_name[TREE_CODE(val)]));
		}
	}
}

// buffer for simulating the initialized memory unit; it is managed independent
// of host's endianness
class INITBUF { 
public:
  UINT64 ival;

  INITBUF(void) {}
  INITBUF(UINT64 i): ival(i) {}
  ~INITBUF(void) {}
  mUINT8 Nth_byte(INT i) { // i must be from 0 to 7
		      INT rshft_amt = (Target_Byte_Sex == BIG_ENDIAN) ? 7-i : i;
		      return (ival >> (rshft_amt * 8)) & 0xff;
		    }
};

// at entry, assumes that in the current struct, initv for "bytes" bytes have 
// been generated; at exit, "bytes" will be updated with the additional
// bytes that this invocation generates.
static void
Add_Bitfield_Initv_For_Tree (tree val, FLD_HANDLE fld, INT &bytes)
{
  FmtAssert(TREE_CODE(val) == INTEGER_CST,
	    ("initialization value of bitfield expected to be integer, not %s",
	     tree_code_name[TREE_CODE(val)]));
  INT bofst = FLD_bofst(fld);
  INT bsize = FLD_bsize(fld);
  if (bsize == 0)
    return;

  INITBUF ib(Get_Integer_Value(val));
  // truncate ival according to the bitfield size and leave it left-justified
  ib.ival = ib.ival << (64 - bsize);
  // shift the value back right to the precise position within INITBUF
  if (Target_Byte_Sex == BIG_ENDIAN) 
    ib.ival = ib.ival >> bofst;
  else ib.ival = ib.ival >> (64 - bofst - bsize);

  // find number of bytes to output
  INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
  // find number of bytes that have been output with previous bitfields
  INT bytes_out = bytes - FLD_ofst(fld);
  INT i;
  if (bytes_out > 0) {
    // verify that, other than the last output byte, the earlier bytes in 
    // ib are all 0
    for (i = 0; i < bytes_out - 1; i++)
      FmtAssert(ib.Nth_byte(i) == 0, 
		("processing error in Add_Bitfield_Initv_For_Tree"));
    if (ib.Nth_byte(bytes_out-1) != 0) {// merge and change last_aggregate_initv
      if (INITV_kind(last_aggregate_initv) == INITVKIND_VAL) {
        TCON &tc = INITV_tc_val(last_aggregate_initv);
        mUINT8 last_ival = TCON_k0(tc);
        tc.vals.k0 = last_ival | ib.Nth_byte(bytes_out-1);
      }
      else { // need to create a new TCON
        if (INITV_kind(last_aggregate_initv) == INITVKIND_ONE) 
	  INITV_Init_Integer(last_aggregate_initv, MTYPE_I1, 
			     1 | ib.Nth_byte(bytes_out-1));
	else {
	  FmtAssert(INITV_kind(last_aggregate_initv) == INITVKIND_ZERO,
		    ("processing error in static bit field initialization"));
	  INITV_Init_Integer(last_aggregate_initv, MTYPE_I1, 
			     ib.Nth_byte(bytes_out-1));
	}
      }
    }
  }
  // output the remaining bytes
  for (i = bytes_out; i < num_of_bytes; i++)
    WFE_Add_Aggregate_Init_Integer(ib.Nth_byte(i), 1);
  bytes += num_of_bytes - bytes_out;
}

// "bytes" will be updated with the additional bytes that this invocation
// generates stores into
static void
Gen_Assign_Of_Init_Val (ST *st, tree init, UINT offset, UINT array_elem_offset,
	TY_IDX ty, BOOL is_bit_field, UINT field_id, FLD_HANDLE fld, INT &bytes)
{
    WN *init_wn = WFE_Expand_Expr (init);
    if (TREE_CODE(init) == STRING_CST && TY_kind(ty) == KIND_ARRAY)
    {
	// have to store string into address,
	// rather than directy copy assignment,
	// so need special code.
	UINT size = TY_size(ty);
	// OSP, string size > ty_size, only init ty_size
	// Replace TREE_STRING_LENGTH with load_size
	UINT load_size = ( size > TREE_STRING_LENGTH(init) ) ?
	  				TREE_STRING_LENGTH(init) : size;
	TY_IDX ptr_ty = Make_Pointer_Type(ty);
	WN *load_wn = WN_CreateMload (0, ptr_ty, init_wn,
#ifdef KEY // bug 3188
			      WN_Intconst(MTYPE_I4, load_size));
#else
				      WN_Intconst(MTYPE_I4, size));
#endif
	WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
	WFE_Stmt_Append(
		WN_CreateMstore (offset, ptr_ty,
				 load_wn,
				 addr_wn,
#ifdef KEY // bug 3188
			       WN_Intconst(MTYPE_I4, load_size)),
#else
				 WN_Intconst(MTYPE_I4,size)),
#endif
		Get_Srcpos());
#ifdef KEY // bug 3247
	if (size - load_size > 0) {
	  load_wn = WN_Intconst(MTYPE_U4, 0);
	  addr_wn = WN_Lda(Pointer_Mtype, 0, st);
	  WFE_Stmt_Append(
		  WN_CreateMstore (offset+load_size, ptr_ty,
				   load_wn,
				   addr_wn,
			   WN_Intconst(MTYPE_I4,size-load_size)),
		  Get_Srcpos());
	}
#endif
	bytes += size;
    }
    else {
	TYPE_ID mtype = is_bit_field ? MTYPE_BS : TY_mtype(ty);
	if (is_bit_field) { 
	    offset = array_elem_offset;	// uses array element offset instead
	} else
	    field_id = 0;	// uses offset instead
	WFE_Set_ST_Addr_Saved (init_wn);
	WN *wn = WN_Stid (mtype, ST_ofst(st) + offset, st,
		ty, init_wn, field_id);
#ifdef KEY
	if (WFE_Stmt_Top_Kind() == wfe_stmk_for_cond)
	  WFE_Stmt_Append_Before (wn, Get_Srcpos());
	else
#endif
	WFE_Stmt_Append(wn, Get_Srcpos());
#ifdef TARG_SL
       BOOL WN_Need_Append_Intrinsic(WN *rhs);
       if(WN_kid0(wn) && WN_Need_Append_Intrinsic(WN_kid0(wn))) {
          extern void WFE_Stmt_Append_Extend_Intrinsic(WN *wn, WN *master_variable, SRCPOS src);
          WN *ldid_wn = WN_Ldid(mtype, ST_ofst(st)+offset, st, ty, field_id);
          WFE_Stmt_Append_Extend_Intrinsic(wn, ldid_wn, Get_Srcpos());
       }
#endif        
	if (! is_bit_field) 
	  bytes += TY_size(ty);
	else {
	  INT bofst = FLD_bofst(fld);
	  INT bsize = FLD_bsize(fld);
	  // find number of bytes to output
	  INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
	  // find number of bytes that have been output with previous bitfields
	  INT bytes_out = bytes - FLD_ofst(fld);
	  bytes += num_of_bytes - bytes_out;
	}
    }
}

UINT
Traverse_Aggregate_Constructor (
  ST   *st, tree init_list, 
#ifdef KEY
  tree struct_type,
#endif
  tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

UINT
Traverse_Aggregate_Struct (
  ST   *st, tree init_list, tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

// For the specified symbol, generate padding at the offset specified.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
Traverse_Aggregate_Pad (
  ST     *st,
  BOOL   gen_initv,
  UINT   pad,
  UINT   current_offset)
{
  if (gen_initv) {
     WFE_Add_Aggregate_Init_Padding (pad);
  }
  else {
    WN *zero_wn = WN_Intconst(MTYPE_U4, 0);
    WN *pad_wn = WN_Intconst(MTYPE_U4, pad);
    WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
    TY_IDX mstore_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U1)); // char *
    WFE_Stmt_Append (WN_CreateMstore (current_offset, mstore_ty,
                                      zero_wn, addr_wn, pad_wn),
                     Get_Srcpos());
  }
} /* Traverse_Aggregate_Pad */

// The aggregate element for the specified symbol at the current_offset
// is an array having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
Traverse_Aggregate_Array (
  ST   *st,            // symbol being initialized
  tree init_list,      // list of initializers for each array element
  tree type,           // type of array
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset) // offset of array from start of symbol
{
  INT    emitted_bytes = 0;
  INT    pad;
  TY_IDX ty            = Get_TY(type);
  TY_IDX ety           = TY_etype (ty);
  UINT   esize         = TY_size (ety);
  tree   init;

#ifdef KEY
  // Bug 2373 - pad for multi-dimensional arrays correctly  
  if (gen_initv) {
    init = CONSTRUCTOR_ELTS(init_list);
    if (init /* Bug 3720 - initialization list could be empty */ && 
	TY_kind(ty) == KIND_ARRAY) {
      INT index = Get_Integer_Value(TREE_PURPOSE(init));
      while (index > 0) {
	Traverse_Aggregate_Pad (st, gen_initv, esize, current_offset);
	emitted_bytes += esize;
	current_offset = emitted_bytes;
	index --;
      }
    }
  }
#endif
  for (init = CONSTRUCTOR_ELTS(init_list);
       init;
       init = TREE_CHAIN(init)) {
    // loop through each array element
    if (TREE_CODE(TREE_VALUE (init)) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      // update array_elem_offset to current_offset to
      // keep track of where each array element starts

      // OSP_238, see comments about 'Bug 591' below
      INT index = Get_Integer_Value(TREE_PURPOSE(init));
      if ( current_offset/esize < index ) {
    // pad (index - current_offset/esize)*esize bytes
    Traverse_Aggregate_Pad (st, gen_initv,
                (index - current_offset/esize)*esize,
                current_offset);
    emitted_bytes += (index - current_offset/esize)*esize;
    current_offset = emitted_bytes;
      } // End of OSP_238

      Traverse_Aggregate_Constructor (st, TREE_VALUE(init), 
#ifdef KEY
				      TREE_TYPE(type),
#endif
				      TREE_TYPE(type),
                                      gen_initv, current_offset, current_offset,
                                      0);
      emitted_bytes += esize;
    }

    else {
      // initialize SCALARs and POINTERs
      // note that we should not be encountering bit fields
#ifdef KEY
      // Bug 591
      // In gcc-3.2 (updated Gnu front-end), the TREE_PURPOSE(init) 
      // would give us the current_offset where TREE_VALUE(init) goes in. 
      // We need to pad any gaps here. This is unlike the gcc-2.96 front-end
      // whereby output_pending_init_elements calls assemble_zeros which
      // calls WFE_Add_Aggregate_Init_Padding appropriately.
      // We do not want to modify the Gnu front-end (c-typeck.c) and instead
      // hack it inside our front-end.
      INT index = Get_Integer_Value(TREE_PURPOSE(init));
      if ( current_offset/esize < index ) {
	// pad (index - current_offset/esize)*esize bytes
	Traverse_Aggregate_Pad (st, gen_initv, 
				(index - current_offset/esize)*esize,
				current_offset);
	emitted_bytes += (index - current_offset/esize)*esize;
	current_offset = emitted_bytes;
      }	
#endif
      if (gen_initv) {
        Add_Initv_For_Tree (TREE_VALUE(init), esize);
        emitted_bytes += esize;
      }
      else
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init), current_offset, 0,
                                ety, FALSE, 0, FLD_HANDLE (), emitted_bytes);
    }

    current_offset += esize;
  }

#ifdef KEY
  // GCC extension allows arrays to be declared without sizes, and later
  // size defined when initialized. For example,
  /* struct locale_data
     {
     long int filesize;
     union locale_data_value
     {
     const unsigned int *wstr;
     const char *string;
     unsigned int word;
     }
     values [];
     };
     
     const struct locale_data _nl_C_LC_CTYPE =
     {
     0,
     {
     
     { .string = ((void *)0) },
     
     { .string = "upper\0" "lower\0" "alpha\0" "digit\0" "xdigit\0" "space\0"
     "print\0" "graph\0" "blank\0" "cntrl\0" "punct\0" "alnum\0"
     },
     }
     } 
  */
  // Here size of values is determined when _nl_C_LC_CTYPE is initialized.
  if (TY_size(ty) == 0)
    Ty_Table[ty].size = emitted_bytes;
#endif /* KEY */
  // If the entire array has not been initialized, pad till the end
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);

} /* Traverse_Aggregate_Array */

#ifdef KEY
// Given current field and its field_id, Advance_Field_Id increases the 
// field_id to point to the next field in the structure. If the current field 
// is a structure itself, its fields are traversed recursively to correctly 
// update field_id.
  
UINT Advance_Field_Id (FLD_HANDLE field, UINT field_id) {
  if (field.Is_Null())
    return field_id;
  field_id++;
  TY_IDX ty = FLD_type(field);
  if (TY_kind(ty) == KIND_STRUCT) {
    field = TY_fld(ty); // get first field
    while (!field.Is_Null()) {
      // field.Entry()->Print(stdout);
      field_id=Advance_Field_Id(field,field_id);
      field=FLD_next(field);
    }
  }
  return field_id;
}
#endif

// The aggregate element for the specified symbol at the current_offset
// is a struct/class/union having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.
// It accepts the field_id of the struct, and returns the field_id
// of the last element in the struct if it has elements, otherwise
// it returns the field_id passed in for empty structs

UINT
Traverse_Aggregate_Struct (
  ST   *st,               // symbol being initialized
  tree init_list,         // list of initializers for elements in STRUCT
#ifdef KEY
  tree struct_type,       // type of top level structure
#endif
  tree type,              // type of struct
  BOOL gen_initv,         // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset,    // offset from start of symbol for current struct
  UINT array_elem_offset, // if struct is with an array, then it is the
                          //   offset of the outermost struct from the
                          //   array enclosing the struct
                          // if struct is not within an array, it is zero
                          // this is needed when field_id is used to generate
                          //   stores for initialization
  UINT field_id)          // field_id of struct
{
  TY_IDX     ty    = Get_TY(type);       // get WHIRL type
  tree       field = TREE_PURPOSE(type); // get first field in gcc
  FLD_HANDLE fld   = TY_fld (ty);        // get first field in WHIRL

  INT        emitted_bytes = 0;          // keep track of # of bytes initialize;
  INT        current_offset_base = current_offset;
  INT        pad;
  BOOL       is_bit_field;
  tree       init;
  TY_IDX     fld_ty;

#ifdef KEY
  if (CONSTRUCTOR_ELTS(init_list))
    ++field_id; // compute field_id for current field
#endif
  for (init = CONSTRUCTOR_ELTS(init_list);
       init;
       init = TREE_CHAIN(init)) {
    // loop through each initializer specified

#ifdef KEY
    // Bug 14422: Do the first increment outside the loop. Then advance
    // the field_id at the tail end of the loop before moving on to next
    // field, taking into account any fields inside current struct field.
    // The update at the tail end is done only if there is an iteration
    // left.
#else
    ++field_id; // compute field_id for current field
#endif

    // if the initialization is not for the current field,
    // advance the fields till we find it
    if (field && TREE_PURPOSE(init) && TREE_CODE(TREE_PURPOSE(init)) == FIELD_DECL) {
      DevWarn ("Encountered FIELD_DECL during initialization");
      for (;;) {
        if (field == TREE_PURPOSE(init)) {
          break;
        }
#ifndef KEY
        ++field_id;
#else
	field_id = Advance_Field_Id(fld, field_id);
#endif
        field = TREE_CHAIN(field);
        fld = FLD_next(fld);
      }
    }

    // check if we need to pad upto the offset of the field
    pad = FLD_ofst (fld) - emitted_bytes;

    if (pad > 0) {
      Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);
      current_offset += pad;
      emitted_bytes  += pad;
    }

    fld_ty = FLD_type(fld);
    if (TREE_CODE(TREE_VALUE(init)) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      tree element_type;
      element_type = TREE_TYPE(field);
#ifdef KEY
      // The size of the struct have to be adjusted for zero-length arrays 
      // that later get initialized and hence adjusted in size.
      // For an example see Traverse_Aggregate_Array
      INT array_size = TY_size(fld_ty);
#endif
      field_id = Traverse_Aggregate_Constructor (st, TREE_VALUE(init),
#ifdef KEY
						 struct_type,
#endif
                                                 element_type, gen_initv,
                                                 current_offset,
                                                 array_elem_offset, field_id);
#ifdef KEY
      // see comment above 
      if (TY_size(fld_ty) != array_size) {
	FmtAssert ((array_size == 0), 
		   ("Unexpected change in size of array"));
	TY_IDX struct_ty = Get_TY ( struct_type );
	Ty_Table[struct_ty].size +=  TY_size(fld_ty);
      }	
#endif
      emitted_bytes += TY_size(fld_ty);
    }
    else {
      // initialize SCALARs and POINTERs
      is_bit_field = FLD_is_bit_field(fld);
      if (gen_initv) {
        if (! is_bit_field) {
#ifdef KEY
	  // Bug 1021
	  // For fields that are arrays of characters with unspecified 
	  // lengths, update the field size and the structure size here.
	  if (TY_size(fld_ty) == 0 && 
	      TREE_CODE(TREE_VALUE(init)) == STRING_CST &&
	      TREE_STRING_POINTER(TREE_VALUE(init))) {
	    Ty_Table[fld_ty].size = 
	    	strlen(TREE_STRING_POINTER(TREE_VALUE(init)));
	    TY_IDX struct_ty = Get_TY ( struct_type );
	    Ty_Table[struct_ty].size += TY_size(fld_ty);
	  }
#endif	  
          Add_Initv_For_Tree (TREE_VALUE(init), TY_size(fld_ty));
          emitted_bytes += TY_size(fld_ty);
        }
        else { // do 1 byte a time
          Add_Bitfield_Initv_For_Tree (TREE_VALUE(init), fld, emitted_bytes);
          // emitted_bytes updated by the call as reference parameter
        }
      }
      else {
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init),
                                current_offset, array_elem_offset,
#ifndef KEY
                                is_bit_field ? ty : fld_ty,
#else
				is_bit_field ? Get_TY(struct_type) : fld_ty,
#endif
                                is_bit_field, field_id, fld, emitted_bytes);
        // emitted_bytes updated by the call as reference parameter
      }
    }

#ifdef KEY
    // Bug 14422:
    // Count current field before moving to next field. The current field
    // may be of struct type, in which case its fields need to be counted.
    // We increment the field-id here instead of at the start of the loop.
    if (TREE_CHAIN(init)) // only if there is an iteration left
      field_id = Advance_Field_Id(fld, field_id);
#endif
    // advance ot next field
    current_offset = current_offset_base + emitted_bytes;
    field = TREE_CHAIN(field);
    fld = FLD_next(fld);
  }

  // if not all fields have been initialized, then loop through
  // the remaining fields to update field_id
  // Also check to see if any bit fields need to be initialized 
  // to zero to handle the case where the bit field shares the
  // same byte as last bit field which was initialized.
  while ( ! fld.Is_Null()) {
    ++field_id;
    if (!gen_initv && FLD_is_bit_field(fld)) {
      INT bofst = FLD_bofst(fld);
      INT bsize = FLD_bsize(fld);
      // find number of bytes to output
      INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
      // find number of bytes that have been output with previous bitfields
      INT bytes_out = current_offset - FLD_ofst(fld);
      if (num_of_bytes == bytes_out) {
	TY_IDX fld_ty = FLD_type(fld);
#ifdef PATHSCALE_MERGE 
	// bug fix for OSP_94
	//
	TYPE_ID mtyp = TY_mtype(fld_ty);
	mtyp = (mtyp == MTYPE_V) ? MTYPE_I4 : Widen_Mtype(mtyp);
	WN *init_wn = WN_Intconst (mtyp, 0);
#endif
	WN *wn = WN_Stid (MTYPE_BS, ST_ofst(st) + array_elem_offset, st,
#ifndef KEY
			  ty, 
#else
			  Get_TY(struct_type),
#endif
			  init_wn, field_id);
	WFE_Stmt_Append(wn, Get_Srcpos());
      }
    }
#ifdef KEY
    field_id = Advance_Field_Id(fld,field_id)-1;
#endif
    field = TREE_CHAIN(field);
    fld = FLD_next(fld);
  }

  // if not all fields have been initilaized, then check if
  // padding is needed to the end of struct
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);

  return field_id;
} /* Traverse_Aggregate_Struct */

#ifdef KEY
// The aggregate element for the specified symbol at the current_offset
// is a vector.
// If gen_initv is FALSE generate a sequence of stores.
void
Traverse_Aggregate_Vector (
  ST * st,             // symbol being initialized
  tree init_list,      // list of initializers for units in vector
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset, // offset from start of symbol for current vector
  BOOL vec_cst = FALSE)// init_list is a constant or not
{
  tree init;
  INT emitted_bytes = 0;

  if (vec_cst)
    init = TREE_VECTOR_CST_ELTS (init_list);
  else
    init = CONSTRUCTOR_ELTS (init_list);

  for (;
       init;
       init = TREE_CHAIN(init))
  {
    tree unit_type = TREE_TYPE(TREE_VALUE(init));
    tree size = TYPE_SIZE (unit_type);
    Is_True (TREE_CODE (size) == INTEGER_CST,
             ("Traverse_Aggregate_Vector: Vector of variable-sized units?"));
    UINT esize = Get_Integer_Value(size) / BITSPERBYTE;
    if (gen_initv)
    {
      Add_Initv_For_Tree (TREE_VALUE(init), esize);
      emitted_bytes += esize;
    }
    else
      Gen_Assign_Of_Init_Val (st, TREE_VALUE(init),
                              current_offset, 0,
                              Get_TY(unit_type),
                              0, 0, FLD_HANDLE(), emitted_bytes);
    current_offset += esize;
  }
} /* Traverse_Aggregate_Vector */

void
Traverse_Aggregate_Vector_Const (
  ST * st,             // symbol being initialized
  tree init_list,      // list of initializers for units in vector
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset) // offset from start of symbol for current vector
{
  Traverse_Aggregate_Vector (st, init_list, gen_initv, current_offset, TRUE);
}
#endif

// The aggregate element for the specified symbol at the current_offset
// is either an array or  struct/class/union having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.
// It accepts the field_id of the element in the enclosing struct
// used for computing field_ids (0 if no such struct exists)
// If the aggregate element is non-array, it returns the field_id of 
// last field within the aggregate element.
// If the aggregate element is array, then it returns the field_id passed in

UINT
Traverse_Aggregate_Constructor (
  ST   *st,               // symbol being initialized
  tree init_list,         // list of initilaizers for this aggregate
#ifdef KEY
  tree struct_type,       // type of top level struct
#endif
  tree type,              // type of aggregate being initialized
  BOOL gen_initv,         // TRUE  if initializing with INITV,
                          // FALSE if initializing with statements
  UINT current_offset,    // offset from start of symbol for this aggregate
  UINT array_elem_offset,
  UINT field_id)
{
  TY_IDX ty = Get_TY(type);

  INITV_IDX last_aggregate_initv_save;

  if (gen_initv) {

    WFE_Add_Init_Block();
    INITV_Init_Block(last_aggregate_initv, INITV_Next_Idx());
    not_at_root = TRUE;
    last_aggregate_initv_save = last_aggregate_initv;
    last_aggregate_initv = 0;
  }

  if (TY_kind (ty) == KIND_STRUCT) {

    field_id = Traverse_Aggregate_Struct (st, init_list, 
#ifdef KEY
					  struct_type,
#endif
					  type, gen_initv,
                                          current_offset, array_elem_offset,
                                          field_id);
  }

  else
  if (TY_kind (ty) == KIND_ARRAY) {

    Traverse_Aggregate_Array (st, init_list, type, gen_initv, current_offset);
  }

#ifdef KEY // bug 9550
  else
  if (TY_kind (ty) == KIND_SCALAR && MTYPE_is_vector (TY_mtype (ty))) {

    Traverse_Aggregate_Vector (st, init_list, gen_initv, current_offset);
  }
#endif

  else
    Fail_FmtAssertion ("Traverse_Aggregate_Constructor: non STRUCT/ARRAY");

#ifdef KEY
  if (gen_initv && last_aggregate_initv == 0) // for empty list; set to reserved value (bug 961)
    INITV_Init_Block(last_aggregate_initv_save, INITV_IDX_ZERO);
#endif

  // restore current level's last_aggregate_initv and return
  last_aggregate_initv = last_aggregate_initv_save;

  return field_id;
} /* Traverse_Aggregate_Constructor */

static void
Add_Inito_For_Tree (tree init, tree decl, ST *st)
{
  tree kid;
  last_aggregate_initv = 0;
  switch (TREE_CODE(init)) {
  case INTEGER_CST:
	UINT64 val;
	val = Get_Integer_Value (init);
#ifndef TARG_NVISA
        // NVISA doesn't have global section initialized to zero,
        // so want explict inito for it.
	// if section-attribute, keep as dglobal inito
#ifdef TARG_SL
// we don't put vbuf variable, which is initialized with zero, into bss section
	if (val == 0 && ! DECL_SECTION_NAME (decl) && !DECL_VBUF(decl) && !DECL_SBUF(decl)) {
#else 	
	if (val == 0 && ! DECL_SECTION_NAME (decl)) {
#endif 		
		Set_ST_init_value_zero(st);
		if (ST_sclass(st) == SCLASS_DGLOBAL)
			Set_ST_sclass(st, SCLASS_UGLOBAL);
		return;
	}
#endif
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Integer (val, TY_size(ST_type(st)));
	return;
  case REAL_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Real (TREE_REAL_CST(init), 
		TY_size(ST_type(st)));
	return;
  case COMPLEX_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Complex (TREE_REAL_CST(TREE_REALPART(init)), 
					TREE_REAL_CST(TREE_IMAGPART(init)), 
					TY_size(ST_type(st)));
	return;
  case STRING_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_String (TREE_STRING_POINTER(init), 
#ifdef KEY // null character added can cause string length to exceed var length
				       TY_size(ST_type(st)) < TREE_STRING_LENGTH(init) ?
				       TY_size(ST_type(st)) :
#endif
                                       TREE_STRING_LENGTH(init));
	if (TY_size (ST_type(st)) > TREE_STRING_LENGTH(init))
		WFE_Add_Aggregate_Init_Padding ( TY_size (ST_type(st)) -
						 TREE_STRING_LENGTH(init));
	return;
  case NOP_EXPR:
	Add_Inito_For_Tree (TREE_OPERAND(init,0), decl, st);
	return;
  case ADDR_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == VAR_DECL ||
	    TREE_CODE(kid) == FUNCTION_DECL ||
#ifdef KEY
	    TREE_CODE(kid) == COMPOUND_LITERAL_EXPR ||
	    // Bug 956
	    TREE_CODE(kid) == LABEL_DECL ||
#endif // KEY
	    TREE_CODE(kid) == STRING_CST) {
		aggregate_inito = New_INITO (st);
		not_at_root = FALSE;
		WFE_Add_Aggregate_Init_Address (kid);
		return;
	}
  case PLUS_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == ADDR_EXPR) {
		// symbol+offset
		Add_Inito_For_Tree (kid, decl, st);
		kid = TREE_OPERAND(init,1);
		if (INITV_kind(last_aggregate_initv) == INITVKIND_SYMOFF
			&& TREE_CODE(kid) == INTEGER_CST)
		{
			Set_INITV_ofst (last_aggregate_initv,
				Get_Integer_Value(kid));
			return;
		}
	}
	break;
  case MINUS_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == ADDR_EXPR) {
		// symbol-offset
		Add_Inito_For_Tree (kid, decl, st);
		kid = TREE_OPERAND(init,1);
		if (INITV_kind(last_aggregate_initv) == INITVKIND_SYMOFF
			&& TREE_CODE(kid) == INTEGER_CST)
		{
			Set_INITV_ofst (last_aggregate_initv,
				-Get_Integer_Value(kid));
			return;
		}
	}
	break;
  case CONSTRUCTOR:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
	Traverse_Aggregate_Constructor (st, init, 
#ifdef KEY
					TREE_TYPE(init),
#endif
					TREE_TYPE(init),
                                        TRUE /*gen_initv*/, 0, 0, 0);
	return;
  }

  // not recognized, so try to simplify
  WN *init_wn = WFE_Expand_Expr (init);
  if (WN_operator(init_wn) == OPR_INTCONST) {
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Integer (
		WN_const_val(init_wn), TY_size(ST_type(st)));
	return;
  }
  else 
  if (WN_operator(init_wn) == OPR_LDA) {
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn), WN_offset (init_wn));
//Bug 555
#ifdef KEY
        Set_ST_initv_in_other_st (WN_st(init_wn));
#endif
	return;
  }
  else
  if (WN_operator(init_wn) == OPR_ADD) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA &&
        WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      WFE_Add_Aggregate_Init_Symbol (WN_st(WN_kid0(init_wn)),
		WN_offset(WN_kid0(init_wn)) + WN_const_val(WN_kid1(init_wn)));
//Bug 555
#ifdef KEY
      Set_ST_initv_in_other_st (WN_st(WN_kid0(init_wn)));
#endif
      return;
    }
  }
  else
  if (WN_operator(init_wn) == OPR_SUB) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA &&
        WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      WFE_Add_Aggregate_Init_Symbol (WN_st(WN_kid0(init_wn)),
		WN_offset(WN_kid0(init_wn)) - WN_const_val(WN_kid1(init_wn)));
//Bug 555
#ifdef KEY
      WN *kid_of_init_wn = WN_kid0(WN_kid0(init_wn));
      if (WN_has_sym(kid_of_init_wn))
        Set_ST_initv_in_other_st (WN_st(kid_of_init_wn));
#endif
      return;
    }
#ifdef KEY
    else if (WN_operator(WN_kid0(init_wn)) == OPR_CVT) { 
      // ignore cvt if kid is LDA
      WN *tmp_wn = WN_kid0(WN_kid0(init_wn));
      if (WN_operator(tmp_wn) == OPR_LDA) { 
	// Take the address of the first operand to OPR_SUB
	// If the second operand is also a subcomponent in the same struct as 
	// the first operand then we can do a static folding.
	// Example:
	//         struct { char a, b, f[3]; } s;
	//         long i = s.f - &s.b;
	// Here, because, operands belong to the same struct we could do the 
        // following:
	// subtract offset of s.b off s.f
	WN *tmp1_wn = WN_kid0(WN_kid1(init_wn));
	if (WN_operator(tmp1_wn) == OPR_LDA) {
	  if (WN_st(tmp_wn) == WN_st(tmp1_wn)) {
	    aggregate_inito = New_INITO (st);
	    not_at_root = FALSE;
	    WFE_Add_Aggregate_Init_Integer (WN_offset(tmp_wn) - 
					    WN_offset(tmp1_wn), 
					    TY_size(ST_type(st)));
	    return;	    
	  }
	}
      }
    }
#endif
  }
  Fail_FmtAssertion ("unexpected static init tree for %s", ST_name(st));
}


extern ST *
WFE_Generate_Temp_For_Initialized_Aggregate (tree init, char * name)
{
  TY_IDX ty_idx = Get_TY(TREE_TYPE(init));
  ST *temp = New_ST (CURRENT_SYMTAB);
  ST_Init (temp,
#ifdef TARG_NVISA
	Save_Str2 (name, "_init"),
#else
	Save_Str2 (name, ".init"),
#endif
	CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL,
	ty_idx );
  if (TREE_CODE(init) == CONSTRUCTOR
	&& ! Use_Static_Init_For_Aggregate (temp, init)) 
  {
	// do sequence of stores to temp
	Set_ST_sclass(temp, SCLASS_AUTO);	// put on stack
	Traverse_Aggregate_Constructor (temp, init, 
#ifdef KEY
					TREE_TYPE(init),
#endif
					TREE_TYPE(init),
                                        FALSE /*gen_initv*/, 0, 0, 0);
  }
  else {
	// setup inito for temp
	Set_ST_is_initialized(temp);
	aggregate_inito = New_INITO (temp);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
	Traverse_Aggregate_Constructor (temp, init, 
#ifdef KEY
					TREE_TYPE(init),
#endif
					TREE_TYPE(init),
                                        TRUE /*gen_initv*/, 0, 0, 0);
	WFE_Finish_Aggregate_Init ();
  }
  return temp;
}

extern void
WFE_Initialize_Decl (tree decl)
{
#ifdef KEY
  if (CURRENT_SYMTAB != GLOBAL_SYMTAB /* not a global variable */ && 
      !TREE_STATIC(decl) /* not a static variable */)
    return;
#endif /* KEY */
  if (DECL_IGNORED_P(decl)) {
  	// ignore initialization unless really used
	// e.g. FUNCTION and PRETTY_FUNCTION
	return;
  }
  ST *st = Get_ST(decl);
  tree init = DECL_INITIAL(decl);

  if (TREE_STATIC(decl) || DECL_CONTEXT(decl) == NULL) 
  {
	// static or global context, so needs INITO
	if ((ST_sclass(st) == SCLASS_UGLOBAL &&
             !ST_init_value_zero(st)) ||
	    ST_sclass(st) == SCLASS_EXTERN  ||
	    ST_sclass(st) == SCLASS_COMMON)
		Set_ST_sclass(st, SCLASS_DGLOBAL);
	if (!ST_is_initialized(st)) {
		Set_ST_is_initialized(st);
		Add_Inito_For_Tree (init, decl, st);
		WFE_Finish_Aggregate_Init ();
	}
	if (TREE_READONLY(decl))
		Set_ST_is_const_var (st);
  }
  else {
	// mimic an assign
	if (TREE_CODE(init) == CONSTRUCTOR) {
		// is aggregate
		if (Use_Static_Init_For_Aggregate (st, init)) {
			// create inito for initial copy
			// and store that into decl
			ST *copy = WFE_Generate_Temp_For_Initialized_Aggregate(
					init, ST_name(st));
			WN *init_wn = WN_CreateLdid (OPR_LDID, MTYPE_M, MTYPE_M,
				0, copy, ST_type(copy));
			WFE_Stmt_Append(
				WN_CreateStid (OPR_STID, MTYPE_V, MTYPE_M,
					0, st, ST_type(st), init_wn),
				Get_Srcpos());
		}
		else {
			// do sequence of stores for each element
			Traverse_Aggregate_Constructor (st, init, 
#ifdef KEY
							TREE_TYPE(init),
#endif
							TREE_TYPE(init),
							FALSE /*gen_initv*/, 
							0, 0, 0);
		}
		return;
	}
	else {
		INT emitted_bytes;
		Gen_Assign_Of_Init_Val (st, init, 
			0 /*offset*/, 0 /*array_elem_offset*/,
			ST_type(st), FALSE, 0 /*field_id*/,
			FLD_HANDLE(), emitted_bytes);
	}
  }
}

#ifdef KEY
  // For initialization of any variables  except globals.
extern void
WFE_Initialize_Nested_Decl (tree decl)
{
  if (DECL_IGNORED_P(decl)) {
  	// ignore initialization unless really used
	// e.g. FUNCTION and PRETTY_FUNCTION
	return;
  }
  ST *st = Get_ST(decl);
  tree init = DECL_INITIAL(decl);

  if (TREE_STATIC(decl) || DECL_CONTEXT(decl) == NULL) 
  {
	// static or global context, so needs INITO
	if ((ST_sclass(st) == SCLASS_UGLOBAL &&
             !ST_init_value_zero(st)) ||
	    ST_sclass(st) == SCLASS_EXTERN  ||
	    ST_sclass(st) == SCLASS_COMMON)
		Set_ST_sclass(st, SCLASS_DGLOBAL);
	if (!ST_is_initialized(st)) {
		Set_ST_is_initialized(st);
		Add_Inito_For_Tree (init, decl, st);
		WFE_Finish_Aggregate_Init ();
	}
	if (TREE_READONLY(decl))
		Set_ST_is_const_var (st);
  }
  else {
	// mimic an assign
	if (TREE_CODE(init) == CONSTRUCTOR) {
		// is aggregate
		if (Use_Static_Init_For_Aggregate (st, init)) {
			// create inito for initial copy
			// and store that into decl
			ST *copy = WFE_Generate_Temp_For_Initialized_Aggregate(
					init, ST_name(st));
			WN *init_wn = WN_CreateLdid (OPR_LDID, MTYPE_M, MTYPE_M,
				0, copy, ST_type(copy));
			WFE_Stmt_Append(
				WN_CreateStid (OPR_STID, MTYPE_V, MTYPE_M,
					0, st, ST_type(st), init_wn),
				Get_Srcpos());
		}
		else {
			// do sequence of stores for each element
			Traverse_Aggregate_Constructor (st, init, 
#ifdef KEY
							TREE_TYPE(init),
#endif
							TREE_TYPE(init),
							FALSE /*gen_initv*/, 
							0, 0, 0);
		}
		return;
	}
	else {
		INT emitted_bytes;
		Gen_Assign_Of_Init_Val (st, init, 
			0 /*offset*/, 0 /*array_elem_offset*/,
			ST_type(st), FALSE, 0 /*field_id*/,
			FLD_HANDLE(), emitted_bytes);
	}
  }
}

extern INT WFE_Get_Current_Scope (void)
{
  return CURRENT_SYMTAB;
}
#endif /* KEY */

// Called for declarations without initializers.
// Necessary to do Get_ST so things like
// int errno (at global level) get ST
// entries so a variable is emitted in whirl (and thus .o).
//
void
WFE_Decl (tree decl)
{
#ifndef KEY
  if (DECL_INITIAL (decl) != 0) return; // already processed
#endif /* KEY */
  if (DECL_IGNORED_P(decl)) return;
  if (TREE_CODE(decl) != VAR_DECL) return;
#ifndef KEY
  if (DECL_CONTEXT(decl) != 0) return;  // local
  if ( ! TREE_PUBLIC(decl)) return;     // local
  if ( ! TREE_STATIC(decl)) return;     // extern
#endif /* KEY */
  // is something that we want to put in symtab
  // (a global var defined in this file).
  (void) Get_ST(decl);
  return;
}

// If we have not yet seen the base_decl, it may be handled later in
// WFE_Weak_Finish (if it is weak), or in WFE_Alias_Finish (if the base_decl
// has been declared after the alias declaration). We also defer expansion
// for FUNCTION_DECL, because processing of the function defn (possibly
// later) will change the sclass, and we want to give the correct sclass
// to decl.
// The global variable finish_alias determines if we can postpone alias
// processing any further. finish_alias == TRUE implies we cannot postpone.
//
void
WFE_Assemble_Alias (tree decl, tree target)
{
#ifdef Is_True_On
  if (!finish_alias)
    DevWarn ("__attribute alias encountered at line %d", lineno);
#endif
  tree base_decl = lookup_name (target);
#ifndef KEY
  FmtAssert (base_decl != NULL,
             ("undeclared base symbol %s not yet declared in __attribute__ alias is not currently implemented",
              IDENTIFIER_POINTER (target)));
#else
  if ((base_decl == NULL || TREE_CODE (decl) == FUNCTION_DECL) && 
      !finish_alias)
  {
    // add to vector if it is not weak (weak: handled in WFE_Weak_Finish)
    if (!DECL_WEAK (decl))
      alias_vector.push_back (std::make_pair (decl, target));
    return;
  }
#endif // KEY 

#ifdef KEY
  // OSP_7, alias to undefined symbol
  if (base_decl == NULL) {
    Is_True( TREE_CODE(target) == IDENTIFIER_NODE,
		    ("Unexpected TREE_CODE of target, IDENTIFIER_NODE expected"));
    error ("'%s' aliased to undefined symbol '%s'", 
	   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)), 
	   IDENTIFIER_POINTER (target));
    return;
  }
#endif	  
  
  ST *base_st = Get_ST (base_decl);
  ST *st = Get_ST (decl);
  if (ST_is_weak_symbol(st)) {
    Set_ST_sclass (st, SCLASS_EXTERN);
    Set_ST_strong_idx (*st, ST_st_idx (base_st));
  }
  else {
    Set_ST_base_idx (st, ST_st_idx (base_st));
    Set_ST_emit_symbol(st);	// for cg
    Set_ST_sclass (st, ST_sclass (base_st));
    if (ST_is_initialized (base_st))
      Set_ST_is_initialized (st);
#ifdef KEY
    if (ST_init_value_zero (base_st))
      Set_ST_init_value_zero (st);
#endif
  }
#ifdef KEY
  // bug 4981: symbol class of ST must match that of the target
  if (ST_sym_class (st) != ST_sym_class (base_st))
    ErrMsg (EC_Ill_Alias, ST_name (st), ST_name (base_st));

  // bug 5145
  // The above stmt ensures same symbol class for st/base_st.
  if (ST_sym_class (base_st) == CLASS_FUNC)
    Set_PU_no_delete (Pu_Table [ST_pu (base_st)]);
#endif // KEY
/*
  if (ST_is_initialized (base_st)) {
    Set_ST_is_initialized (st);
    if (ST_init_value_zero (base_st))
      Set_ST_init_value_zero (st);
  }
*/
} /* WFE_Assemble_Alias */

void
WFE_Assemble_Constructor (const char *name)
{
  DevWarn ("__attribute__ ((constructor)) encountered at line %d", lineno);
  tree func_decl = lookup_name (get_identifier (name));
  ST *func_st = Get_ST (func_decl);
  INITV_IDX initv = New_INITV ();
  INITV_Init_Symoff (initv, func_st, 0, 1);
  ST *init_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (init_st, Save_Str2i ("__ctors", "_", ++__ctors),
           CLASS_VAR, SCLASS_FSTATIC,
           EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
  Set_ST_is_initialized (init_st);
  INITO_IDX inito = New_INITO (init_st, initv);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                Save_Str (".ctors"));
}

void
WFE_Assemble_Destructor (const char *name)
{
  DevWarn ("__attribute__ ((destructor)) encountered at line %d", lineno);
  tree func_decl = lookup_name (get_identifier (name));
  ST *func_st = Get_ST (func_decl);
  INITV_IDX initv = New_INITV ();
  INITV_Init_Symoff (initv, func_st, 0, 1);
  ST *init_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (init_st, Save_Str2i ("__dtors", "_", ++__dtors),
           CLASS_VAR, SCLASS_FSTATIC,
           EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
  Set_ST_is_initialized (init_st);
  INITO_IDX inito = New_INITO (init_st, initv);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                Save_Str (".dtors"));
  Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
  Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
  Set_ST_addr_saved (func_st);
}

ST *
WFE_Get_Return_Address_ST (int level)
{
  ST *return_address_st = Return_Address_ST [CURRENT_SYMTAB - level];
  if (return_address_st == NULL) {
    return_address_st = New_ST (CURRENT_SYMTAB - level);
    ST_Init (return_address_st, Save_Str ("__return_address"), CLASS_VAR,
	     (PUSH_RETURN_ADDRESS_ON_STACK ? SCLASS_FORMAL : SCLASS_AUTO),
	     EXPORT_LOCAL, Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE));
    Set_ST_is_return_var (return_address_st);
    Return_Address_ST [CURRENT_SYMTAB - level] = return_address_st;
  }

  return return_address_st;
} /* WFE_Get_Return_Address_ST */

ST *
WFE_Alloca_0 (void)
{
  WN *wn;
  TY_IDX ty_idx = Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE);
  ST* alloca_st = Gen_Temp_Symbol (ty_idx, "__alloca");
#ifdef KEY
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, alloca_st);
#endif
  wn = WN_CreateAlloca (WN_CreateIntconst (OPC_I4INTCONST, 0));
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ty_idx, wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  Set_PU_has_alloca (Get_Current_PU ());
  return alloca_st;
} /* WFE_Alloca_0 */

ST *
WFE_Alloca_ST (tree decl)
{
  ST *st = Create_ST_For_Tree (decl);
  ST *alloca_st = New_ST (CURRENT_SYMTAB);
  ST_Init (alloca_st, Save_Str (ST_name (st)),
           CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
           Make_Pointer_Type (ST_type (st), FALSE));
  Set_ST_is_temp_var (alloca_st);
  Set_ST_pt_to_unique_mem (alloca_st);
  Set_ST_base_idx (st, ST_st_idx (alloca_st));

  WN *swn = WFE_Expand_Expr (TYPE_SIZE_UNIT(TREE_TYPE(decl)));
#ifdef TARG_PPC32
  swn = WN_CreateCvtl(OPC_U4CVTL, 32, swn);
#endif
  WN *wn  = WN_CreateAlloca (swn);
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  return st;
} /* WFE_Alloca_ST */

void
WFE_Dealloca (ST *alloca_st, tree vars)
{
  int  nkids = 0;
  tree decl;
  WN   *wn;
  ST   *st;
  ST   *base_st;

  for (decl =vars; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == VAR_DECL && DECL_ST (decl)) {
      st = DECL_ST (decl);
      base_st = ST_base (st);
      if (st != base_st)
        ++nkids;
  }

  wn = WN_CreateDealloca (nkids+1);
  WN_kid0 (wn) = WN_Ldid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st));
  nkids = 0;

  for (decl =vars; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == VAR_DECL && DECL_ST (decl)) {
      st = DECL_ST (decl);
      base_st = ST_base (st);
      if (st != base_st)
        WN_kid (wn, ++nkids) = WN_Ldid (Pointer_Mtype, 0, base_st, ST_type (base_st));
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Dealloca */

void
WFE_Record_Asmspec_For_ST (tree decl, const char *asmspec, int reg)
{
  extern PREG_NUM Map_Reg_To_Preg []; // defined in common/com/arch/config_targ.cxx
  PREG_NUM preg = Map_Reg_To_Preg [reg];
  FmtAssert (preg >= 0,
             ("mapping register %d to preg failed\n", reg));
  ST *st = Get_ST (decl);
  TY_IDX ty_idx = ST_type (st);
  Set_TY_is_volatile (ty_idx);
  Set_ST_type (st, ty_idx);
  Set_ST_assigned_to_dedicated_preg (st);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (CURRENT_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_DEDICATED_REGISTER, preg);
} /* WFE_Record_Asmspec_For_ST */

void
WFE_Resolve_Duplicate_Decls (tree olddecl, tree newdecl)
{
  ST     *st      = DECL_ST(olddecl);
  tree    newtype = TREE_TYPE(newdecl);
  tree    newsize = TYPE_SIZE(newtype);
  TY_IDX  ty      = ST_type (st);

  if (TREE_STATIC(olddecl) == FALSE &&
      TREE_STATIC(newdecl) == TRUE  &&
      TREE_PUBLIC(olddecl) == TRUE  &&
      TREE_PUBLIC(newdecl) == FALSE) {
    Set_ST_sclass (st, SCLASS_FSTATIC);
    Set_ST_export (st, EXPORT_LOCAL);
  }

  if (newsize                           &&
      TREE_CODE(newsize) == INTEGER_CST &&
      TY_size (ty) <= Get_Integer_Value (newsize) / BITSPERBYTE) {
    UINT64 size = Get_Integer_Value (newsize) / BITSPERBYTE;
    Set_TY_size (ty, size);
    if (TY_kind (ty) == KIND_ARRAY) {
      Set_ARB_const_ubnd (TY_arb(ty));
#ifdef KEY
// bug 3042: It may be that we had an incomplete type so that size information
// is zero.
      if (!TY_size (TY_etype (ty)))
      {
        tree newesize = TYPE_SIZE (TREE_TYPE (newtype)); // element size
	if (newesize && TREE_CODE (newesize) == INTEGER_CST)
	{
	  UINT64 esize = Get_Integer_Value (newesize) / BITSPERBYTE;
	  Set_TY_size (TY_etype (ty), esize);
	}
	Is_True (TY_size (TY_etype (ty)), ("Invalid array of 0-size structs"));
      }
#endif // KEY
      Set_ARB_ubnd_val (TY_arb(ty), (size / TY_size(TY_etype(ty))) - 1);
    }
  } 
} /* WFE_Resolve_Duplicate_Decls */


void
WFE_Add_Weak ()
{
#ifdef KEY
  // bug 4916
  if (!weak_decls)
    return;
#endif // KEY
  tree decl = 
	  lookup_name (get_identifier (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME 
				  (TREE_VALUE (weak_decls)))));
  if (decl) {
    ST *st = DECL_ST (decl);
    if (st)
      Set_ST_is_weak_symbol (st);
  }
} /* WFE_Add_Weak */


void
WFE_Weak_Finish ()
{
  tree t;
  for (t = weak_decls; t; t = TREE_CHAIN(t)) {
    if (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TREE_VALUE (t)))) {
      tree decl = 
	      lookup_name (get_identifier ( IDENTIFIER_POINTER 
			      (DECL_ASSEMBLER_NAME (TREE_VALUE (t)))));
      if (!decl) 
        warning ("did not find declaration `%s' for used in #pragma weak", 
			IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME
			       	(TREE_VALUE (t))));
      else {
        ST *st = DECL_ST (decl);
	if (st == NULL && TREE_VALUE(t)) {
	  st = Get_ST (decl);
	}
        if (st) {
#ifndef KEY
          Set_ST_is_weak_symbol (st);
          if (TREE_VALUE(t)) {
            tree base_decl = lookup_name (get_identifier 
			   (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME 
						(TREE_VALUE(t)))));
            if (!base_decl)
               warning ("did not find declaration for `%s' used in \
		#pragma weak", 
	        IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TREE_VALUE(t))));
            else {
              ST *base_st = DECL_ST (base_decl);
              if (base_st)
                Set_ST_strong_idx (*st, ST_st_idx (base_st));
            }
          }
#else
	  if (ST_base_idx (st) != ST_st_idx (st)) {
	    Set_ST_is_weak_symbol (st);
	    continue;
	  }

	  // Have to set it after checking ST_base_idx (st) != ST_st_idx (st)
	  Set_ST_is_weak_symbol (st); 
	  tree alias;
	  alias = lookup_attribute("alias", DECL_ATTRIBUTES (decl));
	  if (!alias)
	    continue;
	  alias = TREE_VALUE (TREE_VALUE (alias));
	  alias = get_identifier (TREE_STRING_POINTER (alias));
	  tree base_decl = lookup_name (alias);
	  if (!base_decl)
	    warning ("did not find declaration for `%s' used in \
		#pragma weak", 
		     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TREE_VALUE(t))));
	  else {
	    ST *base_st = DECL_ST (base_decl);
	    if (ST_is_weak_symbol (st) && ST_sclass(st) != SCLASS_EXTERN)
	      Clear_ST_is_weak_symbol (st); 
	    else if (base_st)
	      Set_ST_strong_idx (*st, ST_st_idx (base_st));
	  }
#endif /* !KEY */
        }
      }
    }
  }
} /* WFE_Weak_Finish */

#ifdef KEY
extern "C" void
WFE_Alias_Finish (void)
{
  finish_alias = TRUE;
  for (INT i=0; i<alias_vector.size(); i++)
    WFE_Assemble_Alias (alias_vector[i].first, alias_vector[i].second);
  alias_vector.clear ();
}
#endif // KEY
