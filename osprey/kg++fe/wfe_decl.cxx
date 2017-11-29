/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified October 9, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.3.1 release.
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

#include <values.h>
#include <sys/types.h>
#include <elf.h>
#include "defs.h"
#include "errors.h"
extern "C" {
#include "gnu_config.h"
}
#ifdef KEY	// get HW_WIDE_INT for flags.h
#include "gnu/hwint.h"
#endif	/* KEY */
extern "C" {
#include "gnu/flags.h"
#include "gnu/system.h"
#include "gnu/toplev.h"
#include "gnu/tree.h"
#include "cp-tree.h"
#include "c-pragma.h"
#ifdef KEY // get REAL_VALUE_TYPE
#include "real.h"
#endif // KEY
}
#undef TARGET_PENTIUM // hack around macro definition in gnu
#if defined(TARG_PPC32)
#undef TARGET_POWERPC
#endif /* TARG_PPC32 */
#include "glob.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "const.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "wfe_decl.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "tree_symtab.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "tree_cmp.h"
#ifdef KEY
#include "wfe_dst.h" // DST_enter_member_function
#endif
#include "targ_sim.h" // PUSH_RETURN_ADDRESS_ON_STACK

extern "C" void check_gnu_errors (int *, int *);
#ifdef KEY
extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID, ST *);
#ifdef Is_True_On
extern "C" int flag_openmp;
#endif
extern "C" tree lookup_name (tree, int);		// in cp/decl.c

static tree WFE_get_thunk_target (tree decl);
static void WFE_Handle_Named_Return_Value(tree fn);

// The initializer for the named return value object.  Expand this in place of
// the DECL_INITIAL in the object's VAR_DECL.
// IMPORTANT:  Doesn't work for nested functions.
tree named_ret_obj_initializer;
extern "C" BOOL pragma_implementation_seen, pragma_interface_seen;

/* ST to represent EXC_PTR_EXPR if C++ exceptions are disabled */
ST * Dummy_Exc_Ptr_Expr = NULL;
#endif /* KEY */

static tree *deferred_function_stack;
static INT32 deferred_function_i;
static INT32 deferred_function_max;

static void 
Init_Deferred_Function_Stack()
{
  deferred_function_max   = 32;
  deferred_function_i     = -1;
  deferred_function_stack =
    (tree *) malloc (sizeof (tree) * deferred_function_max);
}

void
Push_Deferred_Function (tree decl)
{
//fprintf(stderr, "Push_Deferred_Function 1: %p %s\n", decl, IDENTIFIER_POINTER(DECL_NAME(decl)));
  for (INT32 i = deferred_function_i; i != -1; --i)
    if (deferred_function_stack [i] == decl)
      return;
//fprintf(stderr, "Push_Deferred_Function 2: %p %s\n", decl, IDENTIFIER_POINTER(DECL_NAME(decl)));
//fprintf(stderr, "Push_Deferred_Function 2: %s\n", IDENTIFIER_POINTER(DECL_NAME(decl)));
  if (++deferred_function_i == deferred_function_max) {
    deferred_function_max = 2 * deferred_function_max;
    deferred_function_stack =
      (tree *) realloc (deferred_function_stack,
			deferred_function_max * sizeof(tree));
  }

  deferred_function_stack[deferred_function_i] = decl;
}

static tree 
Pop_Deferred_Function (void)
{
  tree decl;
  decl = deferred_function_stack[deferred_function_i--];
//fprintf(stderr, "Pop_Deferred_Function: %p %s\n", decl, IDENTIFIER_POINTER(DECL_NAME(decl)));
  return decl;
}

static tree *deferred_decl_init_stack;
static INT32 deferred_decl_init_i;
static INT32 deferred_decl_init_max;

static void 
Init_Deferred_Decl_Init_Stack ()
{
  deferred_decl_init_max   = 32;
  deferred_decl_init_i     = -1;
  deferred_decl_init_stack =
    (tree *) malloc (sizeof (tree) * deferred_decl_init_max);
} /* Init_Deferred_Decl_Init_Stack */

void
Push_Deferred_Decl_Init (tree decl)
{
  if (++deferred_decl_init_i == deferred_decl_init_max) {
    deferred_decl_init_max = 2 * deferred_decl_init_max;
    deferred_decl_init_stack =
      (tree *) realloc (deferred_decl_init_stack,
			deferred_decl_init_max * sizeof(tree));
  }

  deferred_decl_init_stack[deferred_decl_init_i] = decl;
} /* Push_Deferred_Decl_Init */

static tree
Pop_Deferred_Decl_Init (void)
{
  tree decl;
  decl = deferred_decl_init_stack[deferred_decl_init_i--];
  return decl;
} /* Pop_Deferred_Decl_Init */

extern PU_Info *PU_Tree_Root;
static PU_Info *PU_Info_Table     [258] = {0};
static ST      *Return_Address_ST [258] = {0};
static BOOL map_mempool_initialized = FALSE;
static MEM_POOL Map_Mem_Pool;

static tree curr_func_decl = NULL_TREE;
#ifdef KEY
static tree curr_namespace_decl = NULL_TREE;
#endif

static int __ctors = 0;
static int __dtors = 0;

static void Set_Current_Function_Decl(tree decl)
{
  curr_func_decl = decl;
}

tree Current_Function_Decl(void) {return curr_func_decl;}

// void (*back_end_hook) (tree) = &WFE_Expand_Decl;

#ifdef KEY
// A stack of entry WN's.  The current function's entry WN is at the top of
// stack.
static std::vector<WN*> curr_entry_wn;
static void Push_Current_Entry_WN(WN *wn) { curr_entry_wn.push_back(wn); }
static void Pop_Current_Entry_WN() { curr_entry_wn.pop_back(); }
WN *Current_Entry_WN(void) { return curr_entry_wn.back(); }


// Catch-all for all the functions that g++ turns into assembler, so that we
// won't miss any while translating into WHIRL.
std::vector<tree> gxx_emitted_decls;
// Any typeinfo symbols that we have determined we should emit
std::vector<tree> emit_typeinfos;
// Any var_decls or type_decls that we want to expand last.
std::vector<tree> emit_decls;
// Any asm statements at file scope
std::vector<char *> gxx_emitted_asm;
// Struct fields whose type we want to set last.
std::vector<std::pair<tree, FLD_HANDLE> > defer_fields;

#include <stack>
// Potentially OpenMP private variables. See comment in parse_end_decl(),
// some decls go through grokdeclarator, but are finished later.
// Is_shared_mp_var() requires the decl to be complete, so we push such
// decls in stack, and wait for their completion, before processing them
// as private variables.
std::stack<tree> mp_local_vars;

// Map a variable (e.g. typename X::y z) to its template-instantiated version
class tsubst_var {
  tree orig;      // Original variable, may not have a complete type
  tree sub;       // Template-substituted variable
  tree func_decl; // Containing function decl
  public:
  tsubst_var (tree o, tree s, tree f) : orig(o), sub(s), func_decl(f) {}
  tree equal (tree o, tree f) const {
    return (orig == o && func_decl == f) ? sub : NULL;
  }
};

std::vector<tsubst_var> tsubst_map;

void
template_substituted (tree orig, tree sub, tree func_decl) {
  tsubst_var in (orig, sub, func_decl);
  tsubst_map.push_back (in);
}

tree
get_substituted (tree orig, tree func_decl) {
  vector<tsubst_var>::const_iterator it = tsubst_map.begin();

  for (; it != tsubst_map.end(); it++) {
    tree sub = (*it).equal (orig, func_decl);
    if (sub) return sub;
  }
  return NULL;
}

void
push_mp_local_vars (tree decl)
{
  Is_True (flag_openmp, ("Should not reach here without -mp"));
  mp_local_vars.push (decl);
}

tree
pop_mp_local_vars (void)
{
  Is_True (flag_openmp, ("Should not reach here without -mp"));
  if (mp_local_vars.empty()) return NULL;
  tree decl = mp_local_vars.top ();
  mp_local_vars.pop ();
  return decl;
}

void
gxx_emits_decl(tree t) {
  gxx_emitted_decls.push_back(t);
}

void
gxx_emits_typeinfos (tree t) {
  emit_typeinfos.push_back (t);
}

void
defer_decl (tree t) {
  emit_decls.push_back (t);
}

void
gxx_emits_asm (char *str) {
  gxx_emitted_asm.push_back (str);
}

void
defer_field (tree t, FLD_HANDLE fld) {
  defer_fields.push_back (std::make_pair(t, fld));
}

// Support defer creating DST info until all types are created.
typedef struct {
  union {
    struct {		// for is_member_function = 1
      tree context;
      tree fndecl;
    } member_func;
  } u1;
  int is_member_function : 1;
} DST_defer_misc_info;

// List of things that we need to defer creating DST info for.
std::vector<DST_defer_misc_info *> defer_DST_misc;

typedef struct {
  tree t;
  TY_IDX ttidx;
  TY_IDX idx;
} DST_defer_type_info;

// List of types that we defer creating DST info for.
std::vector<DST_defer_type_info *> defer_DST_types;

void
defer_DST_type (tree t, TY_IDX ttidx, TY_IDX idx)
{
  DST_defer_type_info *p =
    (DST_defer_type_info *) calloc(1, sizeof(DST_defer_type_info));
  p->t = t;
  p->ttidx = ttidx;
  p->idx = idx;
  defer_DST_types.push_back(p);
}

void
defer_DST_member_function (tree context, tree fndecl)
{
  DST_defer_misc_info *p =
    (DST_defer_misc_info *) calloc(1, sizeof(DST_defer_misc_info));
  p->u1.member_func.context = context;
  p->u1.member_func.fndecl = fndecl;
  p->is_member_function = 1;
  defer_DST_misc.push_back (p);
}
#endif

// Return 1 if we actually expand decl.
static int
WFE_Expand_Function_Body (tree decl)
{
  tree body;

#ifdef KEY
  if (expanded_decl(decl) == TRUE)
    return 0;

  // bug 2694
  // If decl is a copy constructor that is not (yet) referenced by the WHIRL,
  // then delay its expansion because we don't know if it is really needed.
  // Currently all copy constructors are marked as needed in the g++ front-end
  // regardless if they are really needed, in case the WHIRL needs them later.
  if (// see if decl should be public
      !(TREE_PUBLIC(decl) && !DECL_COMDAT(decl)) &&
      // check for declared inline (same as cp/semantics.c:expand_body)
      DECL_INLINE(decl) &&
      !flag_keep_inline_functions &&
      // check for copy constructor
      DECL_COPY_CONSTRUCTOR_P(decl) &&
      DECL_COMPLETE_CONSTRUCTOR_P(decl) &&
      // check for reference by WHIRL
      DECL_ASSEMBLER_NAME(decl) &&
      !TREE_SYMBOL_REFERENCED_BY_WHIRL(DECL_ASSEMBLER_NAME(decl))) {
    return 0;
  }

  expanded_decl(decl) = TRUE;
#endif

  (void) WFE_Start_Function(decl);
  Set_Current_Function_Decl(decl);

#ifdef KEY
  WFE_Handle_Named_Return_Value(decl);

  // bug 11869: This example shows there may be a label statement
  // without an explicit scope. In such a scenario, a jump to such
  // a label would find incomplete scope information, and hence not
  // know what cleanups to run. Always have a top-level-scope that
  // covers the entire function.
  Push_Top_Level_Scope(decl);
#endif

  for (body = DECL_SAVED_TREE(decl); body; body = TREE_CHAIN(body))
    Mark_Scopes_And_Labels (body);
#ifdef KEY // bug 11869
  Pop_Top_Level_Scope();
#endif

  for (body = DECL_SAVED_TREE(decl); body; body = TREE_CHAIN(body))
    WFE_Expand_Stmt(body);

  WFE_Finish_Function();
  return 1;
}

/*
 * WFE_Expand_Decl is called with the root of the g++ tree (always a
 * NAMESPACE_DECL) as argument and is thus the top-level routine in
 * tree-to-whirl tranlsation.  WFE_Expand_Decl is not called for
 * every declaration, but only for FUNCTION_DECL, NAMESPACE_DECL, 
 * TYPE_DECL, and VAR_DECL:  that is, for those declarations that
 * can appear in namespace scope.
 */

void WFE_Finish_Function(void);

static void
WFE_Generate_Thunk (tree decl)
{
#ifdef KEY
  if (expanded_decl(decl) == TRUE)
    return;
  expanded_decl(decl) = TRUE;
#endif

  Is_True(decl != NULL &&
          TREE_CODE(decl) == FUNCTION_DECL &&
          DECL_THUNK_P(decl) &&
          TREE_CODE(CP_DECL_CONTEXT(decl)) != NAMESPACE_DECL,
          ("Argument to WFE_Generate_Thunk isn't a thunk"));

  Is_True(DECL_INITIAL(decl) != NULL,
          ("Argument to WFE_Generate_Thunk has null DECL_INITIAL"));

  ST      *thunk_st  = Get_ST (decl);
#ifdef KEY
  // Needed for GCC 3.2.  See comment in WFE_get_thunk_target.
  ST      *func_st   = Get_ST (TREE_OPERAND (WFE_get_thunk_target(decl), 0));
#else
  ST      *func_st   = Get_ST (TREE_OPERAND (DECL_INITIAL(decl), 0));
#endif	// KEY
  TYPE_ID  ret_mtype = TY_mtype (TY_ret_type (ST_pu_type (func_st)));
  WN      *entry_wn  = WFE_Start_Function (decl);
  INT32    nargs     = WN_kid_count (entry_wn) - 3;
  INT32    i;
  ST      *arg_st;
  TY_IDX   arg_ty;
  TYPE_ID  arg_mtype;
  WN      *arg_wn;
  WN      *wn;
  WN      *call_wn;

  // modify this parameter by the delta
#ifdef KEY
  // If kg++fe added a fake arg0 (because the function needs to return the
  // object in memory), then the "this" pointer is at arg1.  Bug 5017.
  TY_IDX ret_ty_idx = Get_TY(TREE_TYPE(TREE_TYPE(decl)));
  if (TY_return_in_mem(ret_ty_idx))
    arg_st = WN_st (WN_kid1 (entry_wn));
  else
#endif
  arg_st = WN_st (WN_kid0 (entry_wn));
  arg_ty = ST_type (arg_st);
  arg_mtype = TY_mtype (arg_ty);

  // Pseudocode:
  //     this += delta;
  //     if (vcall_offset != 0)
  //       this += (*((ptrdiff_t **) this))[vcall_offset];
  wn = WN_Binary (OPR_ADD, arg_mtype,
                  WN_Ldid (arg_mtype, 0, arg_st, arg_ty),
                  WN_Intconst (arg_mtype, THUNK_DELTA(decl)));
  if (THUNK_VCALL_OFFSET(decl) != 0) {
    DevWarn ("Generating thunk with vcall adjustment at line %d\n", lineno);
    TY_IDX pdiff;
    // GCC's ptrdiff_type_node is integer type.  Convert to unsigned because
    // pointers are unsigned.
    switch (TY_mtype(Get_TY(ptrdiff_type_node))) {
      case MTYPE_I4:
      case MTYPE_U4:
	pdiff = MTYPE_To_TY(MTYPE_U4);
	break;
      case MTYPE_I8:
      case MTYPE_U8:
	pdiff = MTYPE_To_TY(MTYPE_U8);
	break;
      default:
	FmtAssert(FALSE, ("WFE_Generate_Thunk unexpected type"));
    }

    TY_IDX p_pdiff  = Make_Pointer_Type(pdiff, FALSE);
    TY_IDX pp_pdiff = Make_Pointer_Type(p_pdiff, FALSE);

    WN* deref = WN_CreateIload(OPR_ILOAD,
                               TY_mtype(p_pdiff), TY_mtype(pp_pdiff),
                               0,
                               p_pdiff, pp_pdiff,
                               WN_Tas(TY_mtype(pp_pdiff), pp_pdiff,
                                      WN_COPY_Tree(wn)));

    // The offset should be int32 because WN_CreateIload's 4th operand is int.
    FmtAssert(TREE_INT_CST_HIGH(THUNK_VCALL_OFFSET(decl)) == 0 ||
	      TREE_INT_CST_HIGH(THUNK_VCALL_OFFSET(decl)) == ~0,
	      ("WFE_Generate_Thunk unexpected integer size"));
    INT32 offset = TREE_INT_CST_LOW(THUNK_VCALL_OFFSET(decl));
    wn = WN_Binary (OPR_ADD, arg_mtype,
                    wn,
                    WN_CreateIload(OPR_ILOAD,
                                   TY_mtype(pdiff), TY_mtype(p_pdiff),
                                   offset, pdiff, p_pdiff, deref));
  }
  wn = WN_Stid (arg_mtype, 0, arg_st, arg_ty, wn);
  WFE_Stmt_Append (wn, Get_Srcpos());

  // generate call to base function
  call_wn = WN_Create (OPR_CALL, ret_mtype, MTYPE_V, nargs);
  WN_st_idx (call_wn) = ST_st_idx (func_st);
  WN_Set_Call_Default_Flags (call_wn);
  WN_Set_Call_Replace_By_Jump (call_wn);
  for (i = 0; i < nargs; i++) {
    arg_st = WN_st (WN_kid (entry_wn, i));
    arg_ty = ST_type (arg_st);
    arg_mtype = TY_mtype (arg_ty);
    arg_wn = WN_Ldid (arg_mtype, 0, arg_st, arg_ty);
    arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
                            arg_ty, WN_PARM_BY_VALUE);
    WN_kid (call_wn, i) = arg_wn;
  }

  if (ret_mtype == MTYPE_V) {

    WFE_Stmt_Append (call_wn, Get_Srcpos());
    wn = WN_CreateReturn ();
    WFE_Stmt_Append (wn, Get_Srcpos());
  }

  else {

    WN *block_wn = WN_CreateBlock ();
    WN_INSERT_BlockLast (block_wn, call_wn);
    wn = WN_Ldid (ret_mtype, -1, Return_Val_Preg, Be_Type_Tbl (ret_mtype));
    wn = WN_CreateComma (OPR_COMMA, Mtype_comparison (ret_mtype), MTYPE_V,
			 block_wn, wn);
    wn = WN_CreateReturn_Val (OPR_RETURN_VAL, ret_mtype, MTYPE_V, wn);
    WFE_Stmt_Append (wn, Get_Srcpos());
  }

  Set_PU_is_thunk (Get_Current_PU ());
  WFE_Finish_Function ();
}

static void process_local_classes()
{
  for (int i = 0; i < local_classes->elements_used; ++i) {
    tree t = VARRAY_TREE(local_classes, i);
      if (t->common.code == RECORD_TYPE ||
	  t->common.code == UNION_TYPE)
	Get_TY(t);
  }
}

void WFE_Expand_Decl(tree decl)
{
  Is_True(decl != NULL && TREE_CODE_CLASS(TREE_CODE(decl)) == 'd',
          ("Argument to WFE_Expand_Decl isn't a decl node"));
/*
  int error_count, sorry_count;
  if (decl == global_namespace) {
   check_gnu_errors (&error_count, &sorry_count);
   if (error_count || sorry_count)
     return;
    Init_Deferred_Function_Stack();
  }
*/
  switch (TREE_CODE(decl)) { 

    case CONST_DECL:
      {
      // there are too much such warnings, it is really nuisance!
      static BOOL once_is_enough=FALSE;
      if (!once_is_enough) {
        DevWarn("WFE_Expand_Decl:  don't know what to do with CONST_DECL");
        once_is_enough = TRUE;
      }
      }
      break;

    case FUNCTION_DECL:
      if (DECL_THUNK_P(decl) &&
          TREE_CODE(CP_DECL_CONTEXT(decl)) != NAMESPACE_DECL) {
        WFE_Generate_Thunk(decl);
      }
#ifdef KEY
      // Handle decls that are aliases for other decls.  Bug 3841.
      else if (DECL_ALIAS_TARGET(decl)) {
	WFE_Assemble_Alias(decl, DECL_ALIAS_TARGET(decl));
	return;
      }
#endif
      else {
        tree body = DECL_SAVED_TREE(decl);
        if (body != NULL_TREE && !DECL_EXTERNAL(decl) &&
#ifndef KEY
	    // For now, emit all template-related funcs from GCC 3.2.  Fix
	    // the code when we have more time, 
            (DECL_TEMPLATE_INFO(decl) == NULL 		   ||
             DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION(decl) ||
             DECL_TEMPLATE_INSTANTIATED(decl) 		   ||
             DECL_TEMPLATE_SPECIALIZATION(decl)) &&
#endif
	     !DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P(decl) &&
	     !DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P(decl)) {

#ifndef KEY	// Don't call Get_ST we don't have a PU to attach the ST to
		// yet.  It seems this Get_ST is unnecessary anyway because
		// Get_ST is called again in WFE_Start_Function, which is
		// called by WFE_Expand_Function_Body below.
         (void) Get_ST(decl);
#endif
         if (!Enable_WFE_DFE) {
          if (CURRENT_SYMTAB != GLOBAL_SYMTAB ||
              DECL_FUNCTION_MEMBER_P(decl)    ||
              strncmp (IDENTIFIER_POINTER(DECL_NAME(decl)), "__tcf", 5) == 0)
            Push_Deferred_Function (decl);
          else {
            WFE_Expand_Function_Body (decl);
#ifndef KEY
            while (deferred_function_i != -1)
              WFE_Expand_Function_Body (Pop_Deferred_Function ());
#endif
          }
         }
        }
      }

      break;

    case NAMESPACE_DECL: {
      /* We assume for now that there are no TREE_LIST nodes among
       * the namespace declarations.  
       */

      /*      if (decl == std_node)
	      break; // ignore namespace std */
      if (DECL_NAMESPACE_ALIAS(decl))
	break;
#ifdef KEY
      tree old_namespace_decl = curr_namespace_decl;
      curr_namespace_decl = decl;
#endif
      tree subdecl;
      for (subdecl = cp_namespace_decls(decl);
	   subdecl != NULL_TREE;
	   subdecl = TREE_CHAIN(subdecl))
	WFE_Expand_Decl(subdecl);
      if (decl == global_namespace)
	process_local_classes(); 
#ifndef KEY
      while (deferred_function_i != -1) {
//      fprintf(stderr, "NAMESPACE_DECL: Pop_Deferred_Function\n");
	WFE_Expand_Function_Body (Pop_Deferred_Function ());
      }
#endif
#ifdef KEY
      curr_namespace_decl = old_namespace_decl;
#endif
      break;
    }

    case TEMPLATE_DECL: {
      if (DECL_CONTEXT(decl) && TREE_CODE(DECL_CONTEXT(decl)) == RECORD_TYPE)
	Get_TY(DECL_CONTEXT(decl));
      tree gentemp = most_general_template(decl);
      for (tree t = DECL_TEMPLATE_SPECIALIZATIONS(gentemp);
           t; t = TREE_CHAIN(t))
	if (TREE_CODE(TREE_VALUE(t)) == FUNCTION_DECL &&
	    !DECL_EXTERNAL(TREE_VALUE(t))	      &&
	    !uses_template_parms (TREE_VALUE(t)))
          WFE_Expand_Decl (TREE_VALUE(t));
	DECL_TEMPLATE_SPECIALIZATIONS(gentemp) = 0; // don't do these twice

      for (tree t = DECL_TEMPLATE_INSTANTIATIONS(gentemp);
	   t; t = TREE_CHAIN(t)) {
	  tree val = TREE_VALUE(t);
	  if (TREE_CODE(val) == RECORD_TYPE &&
	      !uses_template_parms(val))
	    Get_TY(val);
      }

      break;
    }

    case TREE_VEC:
      break;
   
    case TYPE_DECL: {
#ifndef KEY // bug 7341: Generate types only on demand
      tree type = TREE_TYPE(decl);
      (void) Get_TY(type);
#endif
      break;
    }

    case VAR_DECL:
#ifdef KEY
// Don't emit the symbol if these flags don't have proper values.
// TREE_SYMBOL_REFERENCED == 0 does not always mean that we don't need to
// emit the symbol, so this condition may refuse to emit some needed symbols.
// But since we determine ourselves what symbols we NEED to emit, this condition
// here should not harm. And this should be able to prevent expansion of some
// really useless/unreferenced symbols.
// FIXME: If we can use the flag TREE_SYMBOL_REFERENCED as below, we should
// remove uses/definitions of TREE_NOT_EMITTED_BY_GXX.
	if (DECL_ASSEMBLER_NAME_SET_P(decl) && 
	    TREE_NOT_EMITTED_BY_GXX(decl) &&
	    !TREE_SYMBOL_REFERENCED(DECL_ASSEMBLER_NAME(decl))) {
	  expanded_decl(decl) = TRUE;
	  return;
	}

      // Handle decls that are aliases for other decls.  Bug 3841.
      if (DECL_ALIAS_TARGET(decl)) {
	WFE_Assemble_Alias(decl, DECL_ALIAS_TARGET(decl));
	return;
      }

      expanded_decl(decl) = TRUE;
#endif
      (void) Get_ST(decl);
      if (DECL_INITIAL(decl) && !DECL_EXTERNAL(decl)) {
	tree init = DECL_INITIAL(decl);
      	if (TREE_CODE(init) == ERROR_MARK)
	  return;

	if (TREE_CODE(init) == PTRMEM_CST)  {
	  init = cplus_expand_constant(init);
	  DECL_INITIAL(decl) = init;
  	}

	if (TREE_CODE(init) == CONSTRUCTOR) {
  	  tree init_type = TREE_TYPE(init);
	  if (TREE_CODE(init_type) != RECORD_TYPE &&
	      TREE_CODE(init_type) != ARRAY_TYPE  &&
	      TREE_CODE(init_type) != UNION_TYPE
#ifdef KEY
	      && TREE_CODE(init_type) != VECTOR_TYPE
#endif
	     )
	    return;
	  }

	WFE_Initialize_Decl(decl);
      }
      break;

#ifdef KEY
    case USING_DECL:
      break;
#endif

    default:
      Is_True(FALSE, ("Unexpected tree code"));
      break;

  } /* switch */
} /* WFE_Expand_Decl */

static BOOL
function_has_varargs(tree fndecl)
{
  tree fntype  = TREE_TYPE(fndecl);
  tree arglist = TYPE_ARG_TYPES(fntype);

#ifdef KEY // bug 9688: cannot be vararg if there is not a single argument
  if (arglist == NULL_TREE)
    return FALSE;
#endif

  while (arglist != NULL_TREE) {
    if (TREE_VALUE(arglist) == void_type_node)
      return FALSE;
    arglist = TREE_CHAIN(arglist);
  }

  return TRUE;
}

#ifdef KEY
// Contents of the array set up below: 
// exc_ptr ST_IDX, filter ST_IDX, typeinfo INITO_IDX, eh_spec INITO_IDX
static void
Setup_Entry_For_EH (void)
{
    const int lbnd = 0;
    const int hbnd = 3;

    ARB_HANDLE arb = New_ARB();
    ARB_Init (arb, lbnd, hbnd, 4);
    Set_ARB_flags (arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
    STR_IDX str = Save_Str ("__EH_INFO_PER_PU__");
    TY_IDX ty;
    TY_Init (New_TY(ty), (hbnd+1)/* # of entries */ * 4 /* sizeof */, KIND_ARRAY, MTYPE_M, str);
    Set_TY_arb (ty, arb);
    Set_TY_etype (ty, MTYPE_TO_TY_array[MTYPE_U4]);
    ST * etable = New_ST (CURRENT_SYMTAB);
    ST_Init (etable, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
    Set_ST_is_initialized (*etable);
    Set_ST_one_per_pu (etable);

    ST  * exc_ptr_st = New_ST (CURRENT_SYMTAB);
    ST_Init (exc_ptr_st, Save_Str ("__Exc_Ptr__"), CLASS_VAR, SCLASS_AUTO,
			EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));
    Set_ST_one_per_pu (exc_ptr_st);
    INITV_IDX exc_ptr_iv = New_INITV();
    INITV_Set_VAL (Initv_Table[exc_ptr_iv], Enter_tcon (Host_To_Targ (MTYPE_U4,
                                ST_st_idx (exc_ptr_st))), 1);

    ST  * filter_st = New_ST (CURRENT_SYMTAB);
    ST_Init (filter_st, Save_Str ("__Exc_Filter__"), CLASS_VAR, SCLASS_AUTO,
	                EXPORT_LOCAL, MTYPE_To_TY(TARGET_64BIT ? MTYPE_U8 : MTYPE_U4));
    Set_ST_one_per_pu (filter_st);
    INITV_IDX filter_iv = New_INITV();
    INITV_Set_VAL (Initv_Table[filter_iv], Enter_tcon (Host_To_Targ (MTYPE_U4,
                                ST_st_idx (filter_st))), 1);
    Set_INITV_next (exc_ptr_iv, filter_iv);
    // this will be filled in later if there are type-filter entries
    INITV_IDX tinfo = New_INITV();
    INITV_Set_VAL (Initv_Table[tinfo], Enter_tcon (Host_To_Targ (MTYPE_U4,
                                0)), 1);
    Set_INITV_next (filter_iv, tinfo);
    // this will be filled in later if there are exception specifications
    INITV_IDX eh_spec = New_INITV();
    INITV_Set_VAL (Initv_Table[eh_spec], Enter_tcon (Host_To_Targ (MTYPE_U4,
                                0)), 1);
    Set_INITV_next (tinfo, eh_spec);

    Set_PU_misc_info (Get_Current_PU(), New_INITO (ST_st_idx (etable), exc_ptr_iv));
}

// Generate WHIRL representing an asm at file scope (between functions).
// Taken from kgccfe/wfe_decl.cxx
static void
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
#endif

extern WN *
WFE_Start_Function (tree fndecl)
{
    Is_True(fndecl != NULL && TREE_CODE(fndecl) == FUNCTION_DECL,
            ("Bad argument to WFE_Start_Function"));

    WN   *entry_wn;
    BOOL  thunk = DECL_THUNK_P(fndecl) &&
                  TREE_CODE(CP_DECL_CONTEXT(fndecl)) != NAMESPACE_DECL;

#ifdef KEY
    // Add DSTs for all types seen so far.  Do this now because the expansion
    // of formal parameters needs those DSTs.
    add_deferred_DST_types();

    // Clear out the saved expr stack for new function.
    wfe_save_expr_stack_last = -1;

    // Initialize the cleanup level for identifying saved expr's.
    wfe_save_expr_level = 1;
    wfe_last_save_expr_level = 1;

    // Initialize label indexes that we are allowed to use.
    WFE_unusable_label_idx = 0;
    WFE_last_label_idx = 0;

    static tree prev_fndecl;
    if (CURRENT_SYMTAB != GLOBAL_SYMTAB) {
      // Get_Current_PU requires Scope_tab[Current_scope].st, which may or may
      // not be set.  So mark the func using its FUNCTION_DECL node and call
      // Set_PU_uplevel(Get_Current_PU()) later.  (The Get_ST(fndecl) invoked
      // by the current iteration of WFE_Start_Function can lead to the
      // expansion of functions (methods) in the type definition of the current
      // function.  When WFE_Start_Function is called to expand those
      // functions, Scope_tab[Current_scope].st for the original function is
      // not set, and Get_Current_PU() seg faults.)
      func_PU_uplevel(prev_fndecl) = TRUE;
    }
    prev_fndecl = fndecl;
#else
    if (CURRENT_SYMTAB != GLOBAL_SYMTAB)
      Set_PU_uplevel (Get_Current_PU ());
#endif

#ifdef KEY
    try_block_seen = false;
#endif

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

    New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

    if (DECL_SOURCE_FILE (fndecl)) {
      lineno = DECL_SOURCE_LINE (fndecl);
      WFE_Set_Line_And_File (lineno, DECL_SOURCE_FILE (fndecl));
    }

    // handle VLAs in the declaration
    WN *vla_block = WN_CreateBlock ();
    WFE_Stmt_Push (vla_block, wfe_stmk_func_body, Get_Srcpos());

    ST        *func_st;
#ifdef KEY
    static BOOL interface_only = (!pragma_implementation_seen || !pragma_interface_seen);

    // Under g++ 3.2 -O3, don't test for DECL_INLINE because DECL_INLINE is 1
    // for every function.  g++ considers every function as a potential inline
    // candidate.
    ST_EXPORT  eclass;
    // Ensure the following categories of functions can be inlined and DFE'd:
    // 1) 7.1.2.4: An inline function shall be defined in every translation
    //             unit in which it is used and shall have exactly the same
    //             definition in every case.
    //    So functions marked inline are eligible for DFE.
    // 2) A class member function defined within the class (covered by 1).
    // 3) A template function, currently limited to non-member functions.
    // TODO: A::B<U>::C() (from bug 7976) should not be internal,
    // but A::B<U>::C<S>() should be.
    // To be marked XINTERNAL, the above functions must be marked 
    // WEAK (bug 7670)
    //
    // The fix for bug 8211 makes the fix for bug 7550 more conservative.
    // If we have seen both the implementation and interface pragmas, then
    // we assume the worst case and disable the following optimization.
    // Ideally CLASSTYPE_INTERFACE_ONLY should be set, but that value
    // does not propagate correctly here. Also note presence of both pragmas
    // could imply disabling the optimization for one header file, but we
    // don't have a way of knowing that, so we assume it for the entire
    // translation unit.
    //
    if (interface_only &&
        DECL_WEAK (fndecl) &&
        (DECL_DECLARED_INLINE_P (fndecl) ||
         (DECL_LANG_SPECIFIC (fndecl) &&
          DECL_IMPLICIT_INSTANTIATION (fndecl) &&
          DECL_NAMESPACE_SCOPE_P (fndecl))))
      eclass = EXPORT_INTERNAL; // bug 7550
    else if (DECL_WEAK(fndecl)) 
      eclass = EXPORT_PREEMPTIBLE;
    else if (!TREE_PUBLIC(fndecl))
      eclass = EXPORT_LOCAL;
    else if (DECL_INLINE(fndecl))
      eclass = EXPORT_PROTECTED;
    else 
      eclass = EXPORT_PREEMPTIBLE;
#else
    ST_EXPORT  eclass = TREE_PUBLIC(fndecl) && !DECL_INLINE(fndecl)
			 || DECL_WEAK(fndecl) ?
		 EXPORT_PREEMPTIBLE				 :
                 EXPORT_LOCAL;
#endif

#ifndef GPLUSPLUS_FE
    if (DECL_INLINE (fndecl) && TREE_PUBLIC (fndecl)) {
      if (DECL_EXTERNAL (fndecl)) {
        // encountered first extern inline definition
        ST *oldst = DECL_ST (fndecl);
        DECL_ST (fndecl) = 0;
        func_st =  Get_ST (fndecl);
        DECL_ST (fndecl) = oldst;
      }
      else {
        // encountered second definition, the earlier one was extern inline
        func_st = Get_ST (fndecl);
      }
    }
    else
#else
      func_st = Get_ST (fndecl);
#endif /* GPLUSPLUS_FE */

    Set_ST_sclass (func_st, SCLASS_TEXT);
    Set_PU_lexical_level (Pu_Table [ST_pu (func_st)], CURRENT_SYMTAB);
    Set_PU_cxx_lang (Pu_Table [ST_pu (func_st)]);

#ifdef KEY
    if (lookup_attribute("used", DECL_ATTRIBUTES (fndecl)))  // bug 3697
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);

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

    if (Show_Progress) {
      fprintf (stderr, "Compiling %s \n", ST_name (func_st));
      fflush (stderr);
    }

    Scope_tab [Current_scope].st = func_st;
#ifdef KEY
// Insert special variables into the local symtab, store their id's
// in the PU_TAB, to be accessed later in the front-end, WN Lowerer,
// inliner/ipa, and back-end.
    if (key_exceptions)
	Setup_Entry_For_EH ();
    else
        Dummy_Exc_Ptr_Expr = NULL;

    if (func_PU_uplevel(fndecl))
      Set_PU_uplevel (Get_Current_PU ());
#endif

    INT num_args = 0;
    tree pdecl;
#ifdef KEY
    // Needed for GCC 3.2.  See comment in WFE_get_thunk_target.
    pdecl = thunk ? DECL_ARGUMENTS (TREE_OPERAND (WFE_get_thunk_target (fndecl), 0))
                  : DECL_ARGUMENTS (fndecl);
#else
    pdecl = thunk ? DECL_ARGUMENTS (TREE_OPERAND (DECL_INITIAL (fndecl), 0))
                  : DECL_ARGUMENTS (fndecl);
#endif /* KEY */
    for (;
         pdecl;
         pdecl = TREE_CHAIN (pdecl)) {
      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else
	++num_args;
    }

#ifdef KEY
    // Add the fake first param if the function needs to return an object in
    // memory.  Here we only handle the types that the front-end says must
    // return in memory.
    TY_IDX ret_ty_idx = Get_TY(TREE_TYPE(TREE_TYPE(fndecl)));
    if (TY_return_in_mem(ret_ty_idx)) {
      num_args++;
    }
#endif

    WN *body, *wn;
    body = WN_CreateBlock ( );
    entry_wn = WN_CreateEntry ( num_args, func_st, body, NULL, NULL );
    /* from 1..nkids=num_args, create IDNAME args for OPR_FUNC_ENTRY */
    INT i = 0;

#ifdef KEY
    // Create the fake first param.
    if (TY_return_in_mem(ret_ty_idx)) {
      ST *st = New_ST ();
      ST_Init (st, Save_Str2i(".arg", "", i), CLASS_VAR, SCLASS_FORMAL,
	       EXPORT_LOCAL, Make_Pointer_Type(ret_ty_idx, FALSE));
      Set_ST_is_value_parm(st);
      WN_kid(entry_wn,i) = WN_CreateIdname ( 0, ST_st_idx(st) );
      ++i;
    }
#endif

    for (pdecl = thunk ?
#ifdef KEY
		    DECL_ARGUMENTS (TREE_OPERAND (WFE_get_thunk_target (fndecl), 0))
#else
		    DECL_ARGUMENTS (TREE_OPERAND (DECL_INITIAL (fndecl), 0))
#endif /* KEY */
                       : DECL_ARGUMENTS (fndecl);
         pdecl;
         pdecl = TREE_CHAIN (pdecl) )
    {
      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      ST *st;
      if (thunk) {
        st = New_ST ();
        ST_Init (st, Save_Str2i(".arg", "", i), CLASS_VAR,
		 SCLASS_FORMAL, EXPORT_LOCAL, arg_ty_idx);
      }
      else {
#ifdef KEY
	tree passed_type = DECL_ARG_TYPE (pdecl);
	tree nominal_type = TREE_TYPE (pdecl);
	// See if the front-end wants to pass this by invisible reference.
	if (passed_type != nominal_type &&
	    POINTER_TYPE_P (passed_type) &&
	    TREE_TYPE (passed_type) == nominal_type) {
	  // The front-end passes the parm by invisible reference.  The parm is
	  // a reference to the data object instead of the object itself.
	  tree ptr_parm = build_decl(PARM_DECL, NULL_TREE, passed_type);
	  DECL_ARG_TYPE(ptr_parm) = passed_type;
	  st = Get_ST(ptr_parm);

	  // We are done with the parm decl.  Change it to an indirect
	  // reference node so that the rest of the WHIRL translator will see
	  // the dereferenced value whenever it references the node.
	  TREE_SET_CODE(pdecl, INDIRECT_REF);
	  TREE_OPERAND(pdecl, 0) = ptr_parm;
	} else
#endif
	{
	  st = Get_ST(pdecl);
	  if (DECL_ARTIFICIAL(pdecl) && DECL_NAME(pdecl) == this_identifier) {
	    Set_ST_is_this_ptr (st);
	  }
	}
      }

      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else {
        if (TY_mtype (arg_ty_idx) == MTYPE_F4 &&
            !TY_has_prototype (ST_pu_type (func_st)))
          Set_ST_promote_parm (st);
        WN_kid(entry_wn,i) = WN_CreateIdname ( 0, ST_st_idx(st) );
        ++i;
      }
    }

    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, entry_wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(func_st);
    PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (func_st,fndecl);
    PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();

    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

    Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

    // check and set the main function of program
    if (strcmp (ST_name (func_st), "main") == 0) {
      PU& pu = Pu_Table[ST_pu (St_Table [PU_Info_proc_sym (pu_info)])];
      Set_PU_is_mainpu (pu);
      Set_PU_no_inline (pu);
    }

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;

    else
    if (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1)
      PU_Tree_Root = pu_info;

    else
      PU_Info_child (PU_Info_Table [CURRENT_SYMTAB -1]) = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;

    WFE_Stmt_Pop (wfe_stmk_func_body);

    WFE_Stmt_Push (entry_wn, wfe_stmk_func_entry, Get_Srcpos());
    WFE_Stmt_Push (body, wfe_stmk_func_body, Get_Srcpos());

    wn = WN_CreatePragma (WN_PRAGMA_PREAMBLE_END, (ST_IDX) NULL, 0, 0);
    WFE_Stmt_Append (wn, Get_Srcpos());
    WFE_Stmt_Append (vla_block, Get_Srcpos());

    if (function_has_varargs(fndecl) && !thunk) {
      // the function uses varargs.h
      // throw off the old type declaration as it did not 
      // take into account any arguments
      PU& pu = Pu_Table[ST_pu (func_st)];
      TY_IDX ty_idx;
      TY &ty = New_TY (ty_idx);
      TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO);
      Set_TY_align (ty_idx, 1);
      TYLIST tylist_idx;
#ifdef KEY
      // If the front-end adds the fake first param, then convert the function
      // to return void.
      if (TY_return_in_mem(Get_TY(TREE_TYPE(TREE_TYPE(fndecl))))) {
	Set_TYLIST_type (New_TYLIST (tylist_idx), Be_Type_Tbl(MTYPE_V));
      } else
#endif
      Set_TYLIST_type (New_TYLIST (tylist_idx),
                       Get_TY(TREE_TYPE(TREE_TYPE(fndecl))));
      Set_TY_tylist (ty, tylist_idx);
      for (pdecl = DECL_ARGUMENTS (fndecl); pdecl; pdecl = TREE_CHAIN (pdecl) ) {
#ifdef KEY
	// If parm was passed through invisible reference, then we would have
	// changed the parm to be an indirect reference of the parm.
	ST *arg_st = TREE_CODE(pdecl) == INDIRECT_REF ?
		       Get_ST(TREE_OPERAND(pdecl, 0)) : Get_ST(pdecl);
#else
	ST *arg_st = Get_ST(pdecl);
#endif
        Set_TYLIST_type (New_TYLIST (tylist_idx), ST_type(arg_st));
      }
      Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
      Set_TY_is_varargs (ty_idx);
      Set_TY_has_prototype (ty_idx);
      Set_PU_prototype (pu, ty_idx);

	  tree fntype = TREE_TYPE(fndecl);
      if (TREE_CODE(fntype) == METHOD_TYPE) {
          TY_IDX base = Get_TY(TYPE_METHOD_BASETYPE(fntype));
          Set_PU_base_class(pu, base);
      }
    }

    if (!thunk && DECL_GLOBAL_CTOR_P(fndecl)) {
#ifndef KEY
      // GLOBAL_INIT_PRIORITY does not exist any more
      if (GLOBAL_INIT_PRIORITY(fndecl) != DEFAULT_INIT_PRIORITY) {
        DevWarn("Discarding ctor priority %d (default %d) at line %d",
                GLOBAL_INIT_PRIORITY(fndecl),
                DEFAULT_INIT_PRIORITY,
                lineno);
      }
#endif // !KEY

      INITV_IDX initv = New_INITV ();
      INITV_Init_Symoff (initv, func_st, 0, 1);
      Set_ST_addr_saved (func_st);
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
      Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
    }

    if (!thunk && DECL_GLOBAL_DTOR_P(fndecl)) {
#ifndef KEY
      // GLOBAL_INIT_PRIORITY does not exist any more
      if (GLOBAL_INIT_PRIORITY(fndecl) != DEFAULT_INIT_PRIORITY) {
        DevWarn("Discarding dtor priority %d (default %d) at line %d",
                GLOBAL_INIT_PRIORITY(fndecl),
                DEFAULT_INIT_PRIORITY,
                lineno);
      }
#endif // !KEY

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
      Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
    }

#ifdef KEY
    // Tell the rest of the front-end this is the current function's entry wn.
    Push_Current_Entry_WN(entry_wn);

    // Function is defined in current file.  Don't make the function symbol
    // weak even if it is referenced in a cleanup.  Workaround for SLES 8
    // linker bug.  Bug 3758.  (We originally made symbols refereneced in
    // cleanups weak as a workaround to the way we generate cleanup code.  See
    // comment in tree_symtab.cxx.)
    //
    // Don't make function symbols defined in gnu.linkonce sections weak, as
    // this causes ld errors such as:
    //   foo: discarded in section `.gnu.linkonce.t.mangledfoo' from blah.o
    // Bug 5723.
    if (!DECL_ONE_ONLY(fndecl)) {
      if (ST_is_weak_symbol(func_st) &&
	  WEAK_WORKAROUND(func_st) == WEAK_WORKAROUND_made_weak) {
	Clear_ST_is_weak_symbol(func_st);
      }
    }
#endif

    return entry_wn;
}

extern void
WFE_Finish_Function (void)
{
    WFE_Check_Undefined_Labels ();
    PU_Info *pu_info = PU_Info_Table [CURRENT_SYMTAB];

#ifdef KEY
    if (PU_lexical_level (Get_Current_PU()) > 2) {

      DevWarn ("Encountered nested function");
      Set_PU_is_nested_func (Get_Current_PU ());
    }
    if (opt_regions)
    {
    	Check_For_Call_Region ();
	// Since we are finishing a function, we must have terminated all
	// regions. So reset the flag.
	Did_Not_Terminate_Region = FALSE;
    }
#else
    if (CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) {

      DevWarn ("Encountered nested function");
      Set_PU_is_nested_func (Get_Current_PU ());
    }
#endif

    // Insert a RETURN if it does not exist
    WN * wn = WN_last (WFE_Stmt_Top ());
    if (wn == NULL || WN_operator (wn) != OPR_RETURN &&
	WN_operator (wn) != OPR_RETURN_VAL) {
      WFE_Stmt_Append (WN_CreateReturn (), Get_Srcpos ());
    }

    // Add any handler code
    Do_Handlers ();
#ifdef KEY
    if (flag_exceptions)	// check if exceptions are enabled
#endif // KEY
    Do_EH_Cleanups ();
#ifdef KEY
    if (key_exceptions)
    	Do_EH_Tables ();
#endif // KEY

    // write out all the PU information
    WFE_Stmt_Pop (wfe_stmk_func_body);
    WFE_Stmt_Pop (wfe_stmk_func_entry);

    // deallocate the old map table
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    Write_PU_Info (pu_info);

    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;

    if (Return_Address_ST [CURRENT_SYMTAB]) {
      Set_PU_has_return_address (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
#ifdef KEY
      if (PU_must_inline(Get_Current_PU())) {
	DevWarn("Disabling must_inline for PU %s",
		ST_name(Get_Current_PU_ST()));
	Clear_PU_must_inline(Get_Current_PU());
      }
#endif

      Return_Address_ST [CURRENT_SYMTAB] = NULL;
    }

#ifdef KEY
    try_block_seen = false;

    // Restore the previous entry wn, if any.
    Pop_Current_Entry_WN();
#endif

    Delete_Scope (CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
//  if (CURRENT_SYMTAB > GLOBAL_SYMTAB)
//    Current_pu = &Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];
}

// Because we build inito's piecemeal via calls into wfe for each piece,
// need to keep track of current inito and last initv that we append to.
static INITO_IDX aggregate_inito = 0;
static INITV_IDX last_aggregate_initv = 0;	
static BOOL not_at_root = FALSE;

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

#ifdef TARG_IA64
void 
WFE_Add_Aggregate_Init_Real (REAL_VALUE_TYPE real, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    tc;

  switch (size) {
    case 4:
      tc = Host_To_Targ_Float_4 (MTYPE_F4,
	WFE_Convert_Internal_Real_to_IEEE_Single(real));
      break;
    case 8:
      tc = Host_To_Targ_Float (MTYPE_F8,
	WFE_Convert_Internal_Real_to_IEEE_Double(real));
      break;
    case 16:
      tc = Host_To_Targ_Float_10 (MTYPE_F10,
	WFE_Convert_Internal_Real_to_IEEE_Double_Extended(real));
      break;
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
#else
#ifdef KEY	// kgccfe uses WFE_Add_Aggregrate_Init_Real instead of
		// WFE_Add_Aggregate_Init_Double.  Use the former because it is
		// newer and can handle REAL_VALUE_TYPE, which is needed for
		// i386.
	
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
// KEY is already defined above, but this is just to keep what we had earlier
  int     buffer [4];
#endif // KEY
  INT32    rbuf_w[4]; // this is needed when long is 64-bit
  switch (size) {
    case 4:
      REAL_VALUE_TO_TARGET_SINGLE (real, t1);
      tc = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &t1);
      break;
    case 8:
      REAL_VALUE_TO_TARGET_DOUBLE (real, buffer);
      WFE_Convert_To_Host_Order(buffer);
#if (SIZEOF_LONG != 4)
      for (int i = 0; i < 4; i++)
	rbuf_w[i] = buffer[i];
      tc = Host_To_Targ_Float (MTYPE_F8, *(double *) rbuf_w);
#else
      tc = Host_To_Targ_Float (MTYPE_F8, *(double *) buffer);
#endif
      break;
#ifdef KEY
    case 12:
    case 16:
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (real, buffer);
      WFE_Convert_To_Host_Order(buffer);
#if (SIZEOF_LONG != 4)
      for (int i = 0; i < 4; i++)
	rbuf_w[i] = buffer[i];
#ifdef TARG_LOONGSON
      tc = Host_To_Targ_Quad (*(QUAD_TYPE *) rbuf_w);
#else
      tc = Host_To_Targ_Quad (*(long double *) rbuf_w);
#endif
#else
#ifdef TARG_LOONGSON
      tc = Host_To_Targ_Quad (*(QUAD_TYPE *) buffer);
#else
      tc = Host_To_Targ_Quad (*(long double *) buffer);
#endif
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

#else

void 
WFE_Add_Aggregate_Init_Double (double val, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TYPE_ID mtype;
  if (size == 4) mtype = MTYPE_F4;
  else if (size == 8) mtype = MTYPE_F8;
  else FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Double unexpected size"));
  INITV_Init_Float (inv, mtype, val);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}
#endif	// KEY
#endif
void 
WFE_Add_Aggregate_Init_Complex (REAL_VALUE_TYPE rval, REAL_VALUE_TYPE ival, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    rtc;
  TCON    itc;

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
    case 32:
#ifdef TARG_LOONGSON
      long     buffer [4];
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (rval, buffer);
      WFE_Convert_To_Host_Order(buffer);
      INT32 rbuf_w[4]; // this is needed when long is 64-bit
      int i;
      for (i = 0; i < 4; i++)
             rbuf_w[i] = buffer[i];
      QUAD_TYPE qval;
      for(i = 0; i < 4; i++)
             qval.qval[i] = rbuf_w[i];
      rtc = Host_To_Targ_Quad (qval);
      itc = Host_To_Targ_Quad (qval);
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
WFE_Add_Aggregate_Init_String (char *s, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_String (inv, s, size);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Symoff (ST *st, WN_OFFSET offset = 0)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Symoff (inv, st, offset);
  Set_ST_addr_saved (st);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

#ifdef TARG_IA64
void
WFE_Add_Aggregate_Init_Symiplt (ST *st, WN_OFFSET offset = 0)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Symiplt (inv, st, offset);
  Set_ST_addr_saved (st);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}
#endif

void
WFE_Add_Aggregate_Init_Label (LABEL_IDX lab)
{
  DevWarn ("taking address of a label at line %d", lineno);
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
	{
	ST *st = Get_ST (init);
	WFE_Add_Aggregate_Init_Symoff (st);
	}
	break;

  case STRING_CST:
	{
#ifdef KEY
	TCON tcon = Host_To_Targ_String (MTYPE_STRING,
			const_cast<char*>TREE_STRING_POINTER(init),
			TREE_STRING_LENGTH(init));
#else
	TCON tcon = Host_To_Targ_String (MTYPE_STRING,
				       TREE_STRING_POINTER(init),
				       TREE_STRING_LENGTH(init));
#endif // KEY
	ST *const_st = New_Const_Sym (Enter_tcon (tcon), 
		Get_TY(TREE_TYPE(init)));
      	WFE_Add_Aggregate_Init_Symoff (const_st);
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
		WFE_Add_Aggregate_Init_Symoff ( Get_ST (addr_kid),
			Get_Integer_Value(TREE_OPERAND(init,1)) );
	}
	else
	{
		WN *init_wn = WFE_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected decl under plus_expr"));
		WFE_Add_Aggregate_Init_Symoff (WN_st (init_wn),
					       WN_offset (init_wn));
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
		WFE_Add_Aggregate_Init_Symoff (WN_st (init_wn),
					       WN_offset (init_wn));
#else
                int tmp_aggr_inito = aggregate_inito;
                int tmp_last_aggregate_initv = last_aggregate_initv;
                WN *init_wn = WFE_Expand_Expr (init);
                aggregate_inito = tmp_aggr_inito;
                last_aggregate_initv = tmp_last_aggregate_initv;
                WFE_Add_Aggregate_Init_Symoff (WN_st(init_wn));
                aggregate_inito = 0;
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
  if (TY_size(ty) == 0 ||
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
	if (!CONSTRUCTOR_ELTS(init))
	    return TRUE;
	return Has_Non_Constant_Init_Value (CONSTRUCTOR_ELTS(init));
  case TREE_LIST:
	{
	tree p;
	for (p = init; p != NULL; p = TREE_CHAIN(p)) {
		if (Has_Non_Constant_Init_Value (TREE_VALUE(p))) {
			return TRUE;
		}
/*
		if (TREE_PURPOSE(p) != NULL_TREE 	     &&
		    TREE_CODE(TREE_PURPOSE(p)) == FIELD_DECL &&
		    DECL_BIT_FIELD(TREE_PURPOSE(p)))
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
  case ADDR_EXPR: {
	tree t = TREE_OPERAND(init, 0);
	if (DECL_CONTEXT(t) == 0 ||
	    TREE_CODE(DECL_CONTEXT(t)) == NAMESPACE_DECL)
		return FALSE;
	return TRUE;
  }
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
	return !Has_Non_Constant_Init_Value(init);
}


static void
Add_Initv_For_Tree (tree val, UINT size)
{
	switch (TREE_CODE(val)) {
	case INTEGER_CST:
		WFE_Add_Aggregate_Init_Integer (
			Get_Integer_Value(val), size);
		break;
	case REAL_CST:
#ifdef KEY
		WFE_Add_Aggregate_Init_Real (
			TREE_REAL_CST(val), size);
#else
		WFE_Add_Aggregate_Init_Double (
			TREE_REAL_CST(val), size);
#endif
		break;
	case STRING_CST:
#ifdef KEY
		WFE_Add_Aggregate_Init_String (
			const_cast<char*>TREE_STRING_POINTER(val), size);
#else
		WFE_Add_Aggregate_Init_String (
			TREE_STRING_POINTER(val), size);
#endif
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
              else
              if (TREE_CODE(kid) == INTEGER_CST) {
                      WFE_Add_Aggregate_Init_Integer (
                              Get_Integer_Value(kid), size);
		      break;
		}
		// fallthru
	default:
		{
		WN *init_wn;
		init_wn = WFE_Expand_Expr (val);
#ifdef KEY
		if (TREE_CODE(val) == PTRMEM_CST) {
		  FmtAssert(WN_operator(init_wn) == OPR_INTCONST,
		   ("Add_Initv_For_Tree: wrong wn after expanding PTRMEM_CST"));
		  WFE_Add_Aggregate_Init_Integer(WN_const_val(init_wn),
						 Pointer_Size);
		  //WFE_Add_Aggregate_Init_Address (WREE_OPERAND
		  break;
		}
#endif
		
#ifdef TARG_IA64
	       if (TREE_CODE(val) == FDESC_EXPR && WN_operator (init_wn) == OPR_LDA)
	       {
	       	       WFE_Add_Aggregate_Init_Symiplt (WN_st (init_wn),
	                                               WN_offset (init_wn));
	               WN_DELETE_Tree (init_wn);
	               break;
	        }
#endif

		if (WN_operator (init_wn) == OPR_LDA) {
			WFE_Add_Aggregate_Init_Symoff (WN_st (init_wn),
						       WN_offset (init_wn));
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
		// handle converts over LDA
		if ((WN_opcode (init_wn) == OPC_I4U4CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U4LDA) ||
		    (WN_opcode (init_wn) == OPC_I8U8CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U8LDA)) {
			WN *kid0 = WN_kid0(init_wn);
			WFE_Add_Aggregate_Init_Symoff (WN_st (kid0), WN_offset(kid0));
			WN_DELETE_Tree (init_wn);
			break;
		}
		// following cases for ADD and SUB are needed because the
		// simplifier may be unable to fold due to overflow in the
		// 32-bit offset field
		else if (WN_operator(init_wn) == OPR_ADD) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symoff (WN_st (kid0),
				     WN_offset(kid0) + WN_const_val(kid1));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		 	else if (WN_operator(kid1) == OPR_LDA &&
			    WN_operator(kid0) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symoff (WN_st (kid1),
				     WN_offset(kid1) + WN_const_val(kid0));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}
		else if (WN_operator(init_wn) == OPR_SUB) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symoff (WN_st (kid0),
				     WN_offset(kid0) - WN_const_val(kid1));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}

		// bug fix for OSP_132
		else if (WN_operator(init_wn) == OPR_INTCONST) {
			WFE_Add_Aggregate_Init_Integer (WN_const_val(init_wn), size);
			break;	      
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
Gen_Assign_Of_Init_Val (
#ifdef NEW_INITIALIZER
        WN *target,
#else
        ST *st, 
#endif
        tree init, UINT offset, UINT array_elem_offset,
	TY_IDX ty, BOOL is_bit_field, UINT field_id, FLD_HANDLE fld, INT &bytes)
{
#ifdef KEY
    // If the initializer is a call expr and the type must be returned in
    // memory, then tell the call expr to put the result directly into ST.
    if (TY_return_in_mem(ty) &&
	TREE_CODE(init) == CALL_EXPR) {
#ifndef NEW_INITIALIZER
      WN *target = WN_Lda (Pointer_Mtype, 0, st, 0);
      bytes += TY_size(ty);
#endif
      WFE_Expand_Expr (init, TRUE, 0, 0, 0, 0, FALSE, FALSE, target);
      return;
    }
#endif

#ifdef NEW_INITIALIZER
    if (TY_return_in_mem(ty) &&
        TREE_CODE(init) == TARGET_EXPR) {
      // We can not pass the offset to WGEN_Expand_Expr,
      //  because it's not handled in that function, so we make an add here
      Is_True ((WN_operator(target) == OPR_LDID ||
                WN_operator(target) == OPR_LDA),
               ("Bad operator for target") );
      if (WN_offset(target) != 0 || offset != 0) {
          TY_IDX targ_ty = WN_ty(target);
          ST* addr_st = Gen_Temp_Symbol (TY_mtype(targ_ty), "target");
          WN* wn = WN_Stid (TY_mtype(targ_ty), 0, addr_st, targ_ty,
                            WN_Binary (OPR_ADD, Pointer_Mtype, 
				       WN_CopyNode(target),
                                       WN_Intconst(MTYPE_I4, offset) ) );
          WFE_Stmt_Append (wn, Get_Srcpos());
          target = WN_Ldid (TY_mtype(targ_ty), 0, addr_st, targ_ty);
      }
      WFE_Expand_Expr (init, TRUE, 0, 0, 0, 0, FALSE, FALSE, target);
      bytes += TY_size(ty);
      return;
    }
#endif

    WN *init_wn = WFE_Expand_Expr (init);
    // bug fix for OSP_229
    //
    if (!init_wn) {
      Is_True(TREE_CODE(init) == COND_EXPR, 
	      ("Must be COND_EXPR when init_wn equlas to NULL."));
      return;
    }

    if (TREE_CODE(init) == STRING_CST && TY_kind(ty) == KIND_ARRAY)
    {
	// have to store string into address,
	// rather than directy copy assignment,
	// so need special code.
	UINT size = TY_size(ty);
	// OSP, string size > ty_size, only init ty_size
	// Replace TREE_STRING_LENGTH with load_size
	// Althrough C++ prohibit str_lenth longer than ty_size,
	// we still change the code here. ( consistent with C part )
	UINT load_size = ( size > TREE_STRING_LENGTH(init) ) ?
					TREE_STRING_LENGTH(init) : size;
	TY_IDX ptr_ty = Make_Pointer_Type(ty);
	WN *load_wn = WN_CreateMload (0, ptr_ty, init_wn,
#ifdef KEY // bug 3188
			      WN_Intconst(MTYPE_I4, load_size));
#else
				      WN_Intconst(MTYPE_I4, size));
#endif
#ifdef NEW_INITIALIZER
        WN *addr_wn = target;
#else
	WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
#endif
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
#ifdef NEW_INITIALIZER
          addr_wn = target;
#else
	  addr_wn = WN_Lda(Pointer_Mtype, 0, st);
#endif
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
#ifdef NEW_INITIALIZER
        //TY_IDX ptr_ty = Make_Pointer_Type(ty);
        //WN *wn = WN_CreateMstore(offset, ty, init_wn, target, WN_Intconst(MTYPE_I4, TY_size(ty)) );
        WN* wn = NULL;
        Is_True( (WN_operator(target) == OPR_LDID ||
                  WN_operator(target) == OPR_LDA),
                 ("Invalid operator for target"));
        if( WN_operator(target) == OPR_LDID ) {
            TY_IDX ptr_ty = Make_Pointer_Type(ty);
            wn = WN_Istore(mtype, offset, ptr_ty, target, init_wn, field_id);
        }
        else { // OPR_LDA
            ST *st = WN_st(target);
            wn = WN_Stid (mtype, WN_lda_offset(target) + offset, st,
                          ty, init_wn, field_id);
        }
#else
	WN *wn = WN_Stid (mtype, ST_ofst(st) + offset, st,
		ty, init_wn, field_id);
#endif
	WFE_Stmt_Append(wn, Get_Srcpos());
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
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st, 
#endif
  tree init_list, tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

UINT
Traverse_Aggregate_Struct (
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st, 
#endif
  tree init_list, tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

// For the specified symbol, generate padding at the offset specified.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
Traverse_Aggregate_Pad (
#ifdef NEW_INITIALIZER
  WN     *target,
#else
  ST     *st,
#endif
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
#ifdef NEW_INITIALIZER
    WN *addr_wn = target;
#else
    WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
#endif
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
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st,            // symbol being initialized
#endif
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
  tree   next;
  tree	 init_value;	// just as tmp tree for TREE_VALUE(init)'s return

  for (init = CONSTRUCTOR_ELTS(init_list);
       init;
       init = next) {
    // loop through each array element

    next = TREE_CHAIN(init);
    init_value = TREE_VALUE (init);

    if (TREE_CODE(init_value) == PTRMEM_CST)  {
      init_value = cplus_expand_constant(TREE_VALUE(init));
    }

    if (TREE_CODE(init_value) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      // update array_elem_offset to current_offset to
      // keep track of where each array element starts
#ifdef NEW_INITIALIZER
      Traverse_Aggregate_Constructor (target, init_value, TREE_TYPE(type),
#else
      Traverse_Aggregate_Constructor (st, init_value, TREE_TYPE(type),
#endif
                                      gen_initv, current_offset, current_offset,
                                      0);
      emitted_bytes += esize;
    }

    else {
      // initialize SCALARs and POINTERs
      // note that we should not be encountering bit fields
      if (gen_initv) {
	if ((next != NULL) && (TREE_CODE (init_value) == FDESC_EXPR) &&
	    (TREE_CODE (TREE_VALUE (next)) == FDESC_EXPR) &&
	    (TREE_VALUE (init_value) == TREE_VALUE (TREE_VALUE (next))))
	{  
	  init = next;
	  next = TREE_CHAIN(next);
          Add_Initv_For_Tree (TREE_VALUE(init), esize);
          emitted_bytes += (esize << 1);
	  current_offset += (esize << 1);
	  continue;
	}
	else
	{
	  Add_Initv_For_Tree (TREE_VALUE(init), esize);
	  emitted_bytes += esize;
	}
      }
      else
#ifdef NEW_INITIALIZER
        Gen_Assign_Of_Init_Val (target, TREE_VALUE(init), current_offset, 0,
#else
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init), current_offset, 0,
#endif
                                ety, FALSE, 0, FLD_HANDLE (), emitted_bytes);
    }

    current_offset += esize;
  }

  // If the entire array has not been initialized, pad till the end
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
#ifdef NEW_INITIALIZER
    Traverse_Aggregate_Pad (target, gen_initv, pad, current_offset);
#else
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);
#endif

} /* Traverse_Aggregate_Array */

// The aggregate element for the specified symbol at the current_offset
// is a struct/class/union having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.
// It accepts the field_id of the struct, and returns the field_id
// of the last element in the struct if it has elements, otherwise
// it returns the field_id passed in for empty structs

UINT
Traverse_Aggregate_Struct (
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st,               // symbol being initialized
#endif
  tree init_list,         // list of initializers for elements in STRUCT
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
  tree       field = TYPE_FIELDS(type);  // get first field in gcc
  FLD_HANDLE fld   = TY_fld (ty);        // get first field in WHIRL

  INT        emitted_bytes = 0;          // keep track of # of bytes initialize;
  INT        current_offset_base = current_offset;
  INT        pad;
  BOOL       is_bit_field;
  tree       init;
  TY_IDX     fld_ty;

  // account for anonymous WHIRL fields being generated for every direct,
  // nonempty nonvirtual base class.
  // these are generated first in Create_TY_For_Tree (tree_symtab.cxx)

#ifndef KEY     // g++'s class.c already laid out the base types.  Bug 11622.
  if (TYPE_BINFO(type) &&
      BINFO_BASETYPES(TYPE_BINFO(type))) {
    tree basetypes = BINFO_BASETYPES(TYPE_BINFO(type));
    INT32 i;
    for (i = 0; i < TREE_VEC_LENGTH(basetypes); ++i) {
      tree binfo = TREE_VEC_ELT(basetypes, i);
      tree basetype = BINFO_TYPE(binfo);
      if (!is_empty_base_class(basetype) ||
          !TREE_VIA_VIRTUAL(binfo)) {
        ++field_id;
        fld = FLD_next (fld);
        field_id += TYPE_FIELD_IDS_USED(basetype);
      }
    }
  }
#endif

  while (field && TREE_CODE(field) != FIELD_DECL)
    field = next_real_or_virtual_field(type, field);

  for (init = CONSTRUCTOR_ELTS(init_list);
       init;
       init = TREE_CHAIN(init)) {
    // loop through each initializer specified

    ++field_id; // compute field_id for current field

    // if the initialization is not for the current field,
    // advance the fields till we find it
    if (field && TREE_PURPOSE(init) && TREE_CODE(TREE_PURPOSE(init)) == FIELD_DECL) {
      for (;;) {
        if (field == TREE_PURPOSE(init)) {
          break;
        }
#ifdef KEY
	// The same field can be created more than once.  Bug 2708.
        if (DECL_NAME(field) &&
	    DECL_NAME(field) == DECL_NAME(TREE_PURPOSE(init))) {
          break;
        }
#endif
        ++field_id;
        fld = FLD_next (fld);
        field = next_real_or_virtual_field(type, field);
        while (field && TREE_CODE(field) != FIELD_DECL)
          field = next_real_or_virtual_field(type, field);
      }
    }

    // check if we need to pad upto the offset of the field
    pad = FLD_ofst (fld) - emitted_bytes;

    if (pad > 0) {
#ifdef NEW_INITIALIZER
      Traverse_Aggregate_Pad (target, gen_initv, pad, current_offset);
#else
      Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);
#endif
      current_offset += pad;
      emitted_bytes  += pad;
    }

    fld_ty = FLD_type(fld);
    if (TREE_CODE(TREE_VALUE(init)) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      tree element_type;
      element_type = TREE_TYPE(field);
#ifdef NEW_INITIALIZER
      field_id = Traverse_Aggregate_Constructor (target, TREE_VALUE(init),
#else
      field_id = Traverse_Aggregate_Constructor (st, TREE_VALUE(init),
#endif
                                                 element_type, gen_initv,
                                                 current_offset,
                                                 array_elem_offset, field_id);
      emitted_bytes += TY_size(fld_ty);
    }
#ifdef KEY
    // Fields corresponding to pointer-to-member-functions are represented as
    // records with fields __pfn and __delta.  The initializer is a TREE_LIST
    // of __pfn and __delta.  Bug 3143.
    else if (TYPE_PTRMEMFUNC_P(TREE_TYPE(field))) {
      tree element_type;
      element_type = TREE_TYPE(field);
      tree t = cplus_expand_constant(TREE_VALUE(init));
#ifdef NEW_INITIALIZER
      field_id = Traverse_Aggregate_Constructor (target, t,
#else
      field_id = Traverse_Aggregate_Constructor (st, t,
#endif
                                                 element_type, gen_initv,
                                                 current_offset,
                                                 array_elem_offset, field_id);
      emitted_bytes += TY_size(fld_ty);
    }
#endif
    else {
      // initialize SCALARs and POINTERs
      is_bit_field = FLD_is_bit_field(fld);
      if (gen_initv) {
        if (! is_bit_field) {
          Add_Initv_For_Tree (TREE_VALUE(init), TY_size(fld_ty));
          emitted_bytes += TY_size(fld_ty);
        }
        else { // do 1 byte a time
          Add_Bitfield_Initv_For_Tree (TREE_VALUE(init), fld, emitted_bytes);
          // emitted_bytes updated by the call as reference parameter
        }
      }
      else {
#ifdef NEW_INITIALIZER
        Gen_Assign_Of_Init_Val (target, TREE_VALUE(init),
#else
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init),
#endif
                                current_offset, array_elem_offset,
                                is_bit_field ? ty : fld_ty,
                                is_bit_field, field_id, fld, emitted_bytes);
        // emitted_bytes updated by the call as reference parameter
      }
    }

    // advance ot next field
    current_offset = current_offset_base + emitted_bytes;
    fld = FLD_next(fld);
    field = next_real_or_virtual_field(type, field);
    while (field && TREE_CODE(field) != FIELD_DECL)
      field = next_real_or_virtual_field(type, field);
  }

  // if not all fields have been initialized, then loop through
  // the remaining fields to update field_id
  while ( ! fld.Is_Null()) {
    ++field_id;
    fld = FLD_next(fld);
    field = next_real_or_virtual_field(type, field);
    while (field && TREE_CODE(field) != FIELD_DECL)
      field = next_real_or_virtual_field(type, field);
  }

  // if not all fields have been initilaized, then check if
  // padding is needed to the end of struct
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
#ifdef NEW_INITIALIZER
    Traverse_Aggregate_Pad (target, gen_initv, pad, current_offset);
#else
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);
#endif

  return field_id;
} /* Traverse_Aggregate_Struct */

#ifdef KEY
// The aggregate element for the specified symbol at the current_offset
// is a vector.
// If gen_initv is FALSE generate a sequence of stores.
void
Traverse_Aggregate_Vector (
#ifdef NEW_INITIALIZER
  WN * target,
#else
  ST * st,             // symbol being initialized
#endif
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
#ifdef NEW_INITIALIZER
      Gen_Assign_Of_Init_Val (target, TREE_VALUE(init),
#else
      Gen_Assign_Of_Init_Val (st, TREE_VALUE(init),
#endif
                              current_offset, 0,
                              Get_TY(unit_type),
                              0, 0, FLD_HANDLE(), emitted_bytes);
    current_offset += esize;
  }
} /* Traverse_Aggregate_Vector */

void
Traverse_Aggregate_Vector_Const (
#ifdef NEW_INITIALIZER
  WN * target,
#else
  ST * st,             // symbol being initialized
#endif
  tree init_list,      // list of initializers for units in vector
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset) // offset from start of symbol for current vector
{
#ifdef NEW_INITIALIZER
  Traverse_Aggregate_Vector (target, init_list, gen_initv, current_offset, TRUE);
#else
  Traverse_Aggregate_Vector (st, init_list, gen_initv, current_offset, TRUE);
#endif
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
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st,               // symbol being initialized
#endif
  tree init_list,         // list of initilaizers for this aggregate
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

#ifdef NEW_INITIALIZER
    field_id = Traverse_Aggregate_Struct (target, init_list, type, gen_initv,
#else
    field_id = Traverse_Aggregate_Struct (st, init_list, type, gen_initv,
#endif
                                          current_offset, array_elem_offset,
                                          field_id);
  }

  else
  if (TY_kind (ty) == KIND_ARRAY) {

#ifdef NEW_INITIALIZER
    Traverse_Aggregate_Array (target, init_list, type, gen_initv, current_offset);
#else
    Traverse_Aggregate_Array (st, init_list, type, gen_initv, current_offset);
#endif
  }

#ifdef KEY // bug 9550
  else
  if (TY_kind (ty) == KIND_SCALAR && MTYPE_is_vector (TY_mtype (ty))) {

#ifdef NEW_INITIALIZER
    Traverse_Aggregate_Vector (target, init_list, gen_initv, current_offset);
#else
    Traverse_Aggregate_Vector (st, init_list, gen_initv, current_offset);
#endif
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
Add_Inito_For_Tree (tree init, ST *st)
{
  tree kid;
  last_aggregate_initv = 0;
  switch (TREE_CODE(init)) {
  case INTEGER_CST:
	UINT64 val;
	val = Get_Integer_Value (init);
#ifdef TARG_SL
// we don't put vbuf variable, which is initialized with zero, into bss section
	if (val == 0 && !ST_in_vbuf(st) && !ST_in_sbuf(st)) {
#else 	
	if (val == 0 ) {
#endif 		
		Set_ST_init_value_zero(st);
		if (ST_sclass(st) == SCLASS_DGLOBAL)
			Set_ST_sclass(st, SCLASS_UGLOBAL);
		return;
	}
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Integer (val, TY_size(ST_type(st)));
	return;
  case REAL_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
#ifdef KEY
	WFE_Add_Aggregate_Init_Real (TREE_REAL_CST(init), 
		TY_size(ST_type(st)));
#else
	WFE_Add_Aggregate_Init_Double (TREE_REAL_CST(init), 
		TY_size(ST_type(st)));
#endif
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
	WFE_Add_Aggregate_Init_String (const_cast<char*>TREE_STRING_POINTER(init),
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
	Add_Inito_For_Tree (TREE_OPERAND(init,0), st);
	return;
  case ADDR_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == VAR_DECL ||
	    TREE_CODE(kid) == FUNCTION_DECL ||
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
		Add_Inito_For_Tree (kid, st);
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
		Add_Inito_For_Tree (kid, st);
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
#ifdef NEW_INITIALIZER
        WN* target = WN_Lda (Pointer_Mtype, 0, st, 0);
        Traverse_Aggregate_Constructor (target, init, TREE_TYPE(init),
#else
	Traverse_Aggregate_Constructor (st, init, TREE_TYPE(init),
#endif
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
	WFE_Add_Aggregate_Init_Symoff (WN_st (init_wn), WN_offset (init_wn));
	return;
  }
  else
  if (WN_operator(init_wn) == OPR_ADD) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA &&
        WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      WFE_Add_Aggregate_Init_Symoff (WN_st(WN_kid0(init_wn)),
		WN_offset(WN_kid0(init_wn)) + WN_const_val(WN_kid1(init_wn)));
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
  else
  if (WN_operator(init_wn) == OPR_SUB) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA &&
        WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      WFE_Add_Aggregate_Init_Symoff (WN_st(WN_kid0(init_wn)),
		WN_offset(WN_kid0(init_wn)) - WN_const_val(WN_kid1(init_wn)));
      return;
    }
  }
  Fail_FmtAssertion ("unexpected static init tree for %s", ST_name(st));
}


extern ST *
WFE_Generate_Temp_For_Initialized_Aggregate (tree init, char * name)
{
  TY_IDX ty_idx = Get_TY(TREE_TYPE(init));
  ST *temp = New_ST (CURRENT_SYMTAB);
  ST_Init (temp,
	Save_Str2 (name, ".init"),
	CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL,
	ty_idx );
  if (TREE_CODE(init) == CONSTRUCTOR
	&& ! Use_Static_Init_For_Aggregate (temp, init)) 
  {
	// do sequence of stores to temp
	Set_ST_sclass(temp, SCLASS_AUTO);	// put on stack
#ifdef NEW_INITIALIZER
        WN* target = WN_Lda (Pointer_Mtype, 0, temp, 0);
        Traverse_Aggregate_Constructor (target, init, TREE_TYPE(init),
#else
	Traverse_Aggregate_Constructor (temp, init, TREE_TYPE(init),
#endif
                                        FALSE /*gen_initv*/, 0, 0, 0);
  }
  else {
	// setup inito for temp
	Set_ST_is_initialized(temp);
	aggregate_inito = New_INITO (temp);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
#ifdef NEW_INITIALIZER
        WN* target = WN_Lda (Pointer_Mtype, 0, temp, 0);
        Traverse_Aggregate_Constructor (target, init, TREE_TYPE(init),
#else
	Traverse_Aggregate_Constructor (temp, init, TREE_TYPE(init),
#endif
                                        TRUE /*gen_initv*/, 0, 0, 0);
	WFE_Finish_Aggregate_Init ();
  }
  return temp;
}

#ifdef NEW_INITIALIZER
ST* WFE_Generate_Initialized_Aggregate (WN * target, tree init)
{
  Is_True(TREE_CODE(init) == CONSTRUCTOR,
          ("wrong tree code for target"));
  Is_True((WN_operator(target) == OPR_LDID ||
           WN_operator(target) == OPR_LDA),
          ("Invalid target operator"));
  ST* target_st = WN_st(target);

  if (TREE_CODE(init) == CONSTRUCTOR
        && ! Use_Static_Init_For_Aggregate (target_st, init))
  {
        Traverse_Aggregate_Constructor (target, init, TREE_TYPE(init),
                                        FALSE /*gen_initv*/, 0, 0, 0);
        return target_st;
  }
  else {
        // TODO: We do not need to create a temp ST in all cases.
        //  if ST_class(target_st) is FORMAL, we need it indeed.
        DevWarn ("Static initialize %s(%s)\n",
                 ST_name(target_st), Sclass_Name(ST_sclass(target_st)));
        TY_IDX ty_idx = Get_TY(TREE_TYPE(init));
        ST *temp = New_ST (CURRENT_SYMTAB);
        ST_Init (temp,
                Save_Str2 (ST_name(target_st), ".init"),
                CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL,
                ty_idx );
        // setup inito for target_st
        Set_ST_is_initialized(temp);
        aggregate_inito = New_INITO (temp);
        not_at_root = FALSE;
        last_aggregate_initv = 0;
        WN* temp_target = WN_Lda (Pointer_Mtype, 0, temp, 0);
        Traverse_Aggregate_Constructor (temp_target, init, TREE_TYPE(init),
                                        TRUE /*gen_initv*/, 0, 0, 0);
        WFE_Finish_Aggregate_Init ();
        return temp;
  }
}
#endif

static tree init_decl = NULL;

extern void
WFE_Initialize_Decl (tree decl)
{
  if (DECL_IGNORED_P(decl)) {
  	// ignore initialization unless really used
	// e.g. FUNCTION and PRETTY_FUNCTION
	return;
  }
  ST *st = Get_ST(decl);
  tree init = DECL_INITIAL(decl);
  if (init->common.code == VAR_DECL &&
      DECL_CONTEXT(init)	    &&
      TREE_CODE(DECL_CONTEXT(init)) == RECORD_TYPE)
    Get_TY(DECL_CONTEXT(init));

  if (TREE_STATIC(decl) || DECL_CONTEXT(decl) == NULL) 
  {
	// static or global context, so needs INITO
	if (ST_sclass(st) == SCLASS_UGLOBAL && !ST_init_value_zero(st)  ||
	    ST_sclass(st) == SCLASS_EXTERN  			        ||
	    ST_sclass(st) == SCLASS_COMMON)
		Set_ST_sclass(st, SCLASS_DGLOBAL);
	if (!ST_is_initialized(st)) {
		Set_ST_is_initialized(st);
		if (init_decl) {
			Push_Deferred_Decl_Init (decl);
			return;
		}
		init_decl = decl;
		Add_Inito_For_Tree (init, st);
		while (deferred_decl_init_i >= 0) {
			init_decl = Pop_Deferred_Decl_Init ();
			Add_Inito_For_Tree (DECL_INITIAL(init_decl),
					    Get_ST(init_decl));
		}
		init_decl = NULL;
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
#ifdef NEW_INITIALIZER
			WN* target = WN_Lda(Pointer_Mtype, 0, st, 0);
                        Traverse_Aggregate_Constructor (target, init, TREE_TYPE(init),
#else
			Traverse_Aggregate_Constructor (st, init, TREE_TYPE(init),
#endif
                                FALSE /*gen_initv*/, 0, 0, 0);
		}
	}
	else {
		INT emitted_bytes;
#ifdef NEW_INITIALIZER
		WN* target = WN_Lda(Pointer_Mtype, 0, st, 0);
		Gen_Assign_Of_Init_Val (target, init,
#else
		Gen_Assign_Of_Init_Val (st, init, 
#endif
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
		Add_Inito_For_Tree (init, st);
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
#ifdef NEW_INITIALIZER
			WN* target = WN_Lda(Pointer_Mtype, 0, st, 0);
                        Traverse_Aggregate_Constructor (target, init,
#else
			Traverse_Aggregate_Constructor (st, init, 
#endif
							TREE_TYPE(init),
							FALSE /*gen_initv*/, 
							0, 0, 0);
		}
		return;
	}
	else {
		INT emitted_bytes;
#ifdef NEW_INITIALIZER
		WN* target = WN_Lda(Pointer_Mtype, 0, st, 0);
                Gen_Assign_Of_Init_Val (target, init,
#else
		Gen_Assign_Of_Init_Val (st, init, 
#endif
			0 /*offset*/, 0 /*array_elem_offset*/,
			ST_type(st), FALSE, 0 /*field_id*/,
			FLD_HANDLE(), emitted_bytes);
	}
  }
}
#endif /* KEY */

void
WFE_Decl (tree decl)
{
#ifndef KEY
  if (DECL_INITIAL (decl) != 0) return;	// already processed
#endif
  if (DECL_IGNORED_P(decl)) return;
  if (TREE_CODE(decl) != VAR_DECL) return;
#ifndef KEY
  if (DECL_CONTEXT(decl) != 0) return;	// local
  if ( ! TREE_PUBLIC(decl)) return;	// local
#endif
  if ( ! TREE_STATIC(decl)) return;	// extern
  // is something that we want to put in symtab
  // (a global var defined in this file).
  (void) Get_ST(decl);
}

BOOL
WFE_Assemble_Alias (tree decl, tree target)
{
  DevWarn ("__attribute alias encountered at line %d", lineno);
  tree base_decl = NULL;
#ifndef KEY
  base_decl = lookup_name (target, 0);
  FmtAssert (base_decl != NULL,
             ("undeclared base symbol %s not yet declared in __attribute__ alias is not currently implemented",
              IDENTIFIER_POINTER (target)));
#else
  // Handle non-weak aliases, where lookup_name returns NULL.  Example:
  //   namespace foo {
  //     extern "C" {
  //     void func1() {}
  //     typeof(func1) func2 __attribute__((alias ("func1")));
  //     }
  //   }
  // Bug 4393.
  if (DECL_ALIAS_TARGET_DECL(decl)) {
    // Get the target's decl that we found and saved.
    base_decl = DECL_ALIAS_TARGET_DECL(decl);
  } else {
    // Look for target's decl in the current namespace.
    tree subdecl;
    for (subdecl = cp_namespace_decls(curr_namespace_decl);
	 subdecl != NULL_TREE;
	 subdecl = TREE_CHAIN(subdecl)) {
      if (DECL_ASSEMBLER_NAME_SET_P(subdecl) &&
	  !strcmp(IDENTIFIER_POINTER(DECL_ASSEMBLER_NAME(subdecl)),
		  IDENTIFIER_POINTER(target))) {
	base_decl = subdecl;
	break;
      }
    }
    if (base_decl == NULL) {
      // We fix up weak declarations later in WFE_Weak_Finish; so this case is
      // okay to return.
      if (DECL_WEAK(decl))
	return FALSE;
      Is_True(FALSE, ("WFE_Assemble_Alias: cannot find decl for alias target"));
    }
    DECL_ALIAS_TARGET_DECL(decl) = base_decl;
  }
  // Don't expand alias until the target is expanded, so that we can set st's
  // sclass to base_st's sclass.  This may take more than one iteration since
  // the target can be an alias to another target.  Bug 4393.
  if (!expanded_decl(base_decl))
    return FALSE;
  expanded_decl(decl) = TRUE;
#endif // KEY 
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
  return TRUE;
#endif
/*
  if (ST_is_initialized (base_st)) {
    Set_ST_is_initialized (st);
    if (ST_init_value_zero (base_st))
      Set_ST_init_value_zero (st);
  }
*/
} /* WFE_Assemble_Alias */


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
  WN *wn  = WN_CreateAlloca (swn);
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  return st;
} /* WFE_Alloca_ST */

#ifdef KEY
void
WFE_Dealloca (ST * alloca_st, vector<ST*> * vars)
{
  int nkids = vars->size();
  Is_True (nkids > 0, ("No object allocated by alloca?"));

  WN * wn = WN_CreateDealloca (nkids+1);
  WN_kid0 (wn) = WN_Ldid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st));
  nkids = 0;

  while (! vars->empty())
  {
    ST * base_st = vars->back();
    WN_kid (wn, ++nkids) = WN_Ldid (Pointer_Mtype, 0, base_st, ST_type (base_st));
    vars->pop_back();
  }
  WFE_Stmt_Append (wn, Get_Srcpos());
}
#endif // KEY

#ifndef KEY	// obsolete
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
WFE_Record_Asmspec_For_ST (tree decl, char *asmspec, int reg)
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
#endif	// KEY

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
      Set_ARB_ubnd_val (TY_arb(ty), (size / TY_size(TY_etype(ty))) - 1);
    }
  } 
} /* WFE_Resolve_Duplicate_Decls */

/* Defined in varasm.c */
extern tree weak_decls;


/* Mark the ST of the first element of weak_decls as a weak symbol.
   Call this each time a decl is prepended to weak_decls (e.g., in
   declare_weak ()).  */
void
WFE_Add_Weak ()
{
  tree decl = TREE_VALUE (weak_decls);
  if (decl) {
    ST *st = DECL_ST (decl);
    if (st)
      Set_ST_is_weak_symbol (st);
  }
} /* WFE_Add_Weak */


/* The old definition of weak_decls included specialized code for weak
   aliases.  The new weak_decls does not, AFAICT; I don't know what I
   need to do to get weak aliases working again, though.  */

void
WFE_Weak_Finish ()
{
  tree t;
  for (t = weak_decls; t; t = TREE_CHAIN (t)) {
    tree decl = TREE_VALUE (t);
    const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
    if (! TREE_USED (decl))
      continue;
    else {
      ST *st = DECL_ST (decl);

      if (!st)
	st = Get_ST (decl);
      if (st) {
	Set_ST_is_weak_symbol (st);
      }
    }
  }
} /* WFE_Weak_Finish */


void WFE_Process_Type_Decl (tree);
void WFE_Process_Template_Decl (tree);
void WFE_Process_Var_Decl (tree);
void WFE_Process_Function_Decl (tree);
void WFE_Process_Namespace_Decl (tree);
void WFE_Process_Decl (tree);

void
WFE_Process_Class_Decl (tree decl)
{
//fprintf(stderr, "CLASS_DECL: %s\n", IDENTIFIER_POINTER(DECL_NAME(decl)));
  if (cp_type_quals(decl) != TYPE_UNQUALIFIED)
    return;

  if (TYPE_TY_IDX(decl))
    return;

  TYPE_TY_IDX(decl) = MTYPE_B;

  tree  binfo     = TYPE_BINFO(decl);
  tree  basetypes = binfo ? BINFO_BASETYPES(binfo) : 0;
  INT32 i;
  if (basetypes)
    for (i = 0; i < TREE_VEC_LENGTH(basetypes); ++i)
      (void) WFE_Process_Class_Decl (BINFO_TYPE(TREE_VEC_ELT(basetypes, i)));

  tree  field;
  for (field = TYPE_FIELDS (decl);
    field != NULL_TREE;
    field = next_real_or_virtual_field (decl, field)) {
    if (TREE_CODE(field) == TYPE_DECL) {
      tree field_type = TREE_TYPE(field);
      if (field_type &&
          TREE_CODE(field_type) == RECORD_TYPE &&
          field_type != decl) {
        WFE_Process_Class_Decl (field_type);
      }
    } 
  }

  tree method = TYPE_METHODS(decl);
  while (method != NULL_TREE) {
    if (TREE_CODE(method) == FUNCTION_DECL) {
      tree body = DECL_SAVED_TREE(method);
      if (body != NULL_TREE && !DECL_EXTERNAL(method) &&
          !DECL_WEAK(method) &&
          !DECL_INLINE(method) &&
          DECL_ST(method) == NULL &&
          (DECL_TEMPLATE_INFO(method) == NULL              ||
          DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION(method) ||
          DECL_TEMPLATE_INSTANTIATED(method)              ||
          DECL_TEMPLATE_SPECIALIZATION(method))) {
          WFE_Process_Function_Decl (method);
      }
    }
    method = TREE_CHAIN(method);
  }
} /* WFE_Process_Class_Decl */

void
WFE_Process_Type_Decl (tree decl)
{
//fprintf(stderr, "TYPE_DECL: %s\n", IDENTIFIER_POINTER(DECL_NAME(decl)));
  if (TREE_CODE(TREE_TYPE(decl)) == RECORD_TYPE &&
      cp_type_quals(decl) == TYPE_UNQUALIFIED) {
    WFE_Process_Class_Decl (TREE_TYPE(decl));
  }
} /* WFE_Process_Type_Decl */

void
WFE_Process_Template_Decl (tree decl)
{
//fprintf(stderr, "TEMPLATE_DECL: %s\n", IDENTIFIER_POINTER(DECL_NAME(decl)));
  tree gentemp = most_general_template(decl);
  for (tree t = DECL_TEMPLATE_INSTANTIATIONS(gentemp);
       t; t = TREE_CHAIN(t)) {
    tree val = TREE_VALUE(t);
    if (TREE_CODE(val) == RECORD_TYPE &&
        !uses_template_parms(val))
      WFE_Process_Class_Decl (val);
  }
} /* WFE_Process_Template_Decl */

#ifdef KEY
// Return TRUE if DECL is a needed VTT (virtual table table).
bool
decl_is_needed_vtt (tree decl)
{
  bool needed = false;

  // Assume all VTTs are needed.
  if (DECL_NAME(decl) &&
      IDENTIFIER_POINTER(DECL_NAME(decl)) &&
      !strncmp("_ZTT", IDENTIFIER_POINTER(DECL_NAME(decl)), 4)) {
    needed = true;
  }
  return needed;
}
#endif

bool
decl_is_needed_vtable (tree decl)
{
  bool needed = false;
  if (DECL_NAME(decl) &&
      IDENTIFIER_POINTER(DECL_NAME(decl)) &&
#ifdef KEY
      !strncmp("_ZTV", IDENTIFIER_POINTER(DECL_NAME(decl)), 4)
#else
      !strncmp("__vt_", IDENTIFIER_POINTER(DECL_NAME(decl)), 5)
#endif
     ) {
            
    tree entries = CONSTRUCTOR_ELTS (DECL_INITIAL (decl));

    for (; entries; entries = TREE_CHAIN (entries)) {

      tree fnaddr;
      tree fn;

      fnaddr = TREE_VALUE (entries);

#ifdef KEY
      if (TREE_CODE (fnaddr) == NOP_EXPR &&
	  TREE_CODE (TREE_OPERAND (fnaddr, 0)) == ADDR_EXPR) {
	fn = TREE_OPERAND (TREE_OPERAND (fnaddr, 0), 0);  // fn can be VAR_DECL
	
      } else if (TREE_CODE (fnaddr) != ADDR_EXPR) {
        /* This entry is an offset: a virtual base class offset, a
           virtual call offset, and RTTI offset, etc.  */
        continue;
      } else
        fn = TREE_OPERAND (fnaddr, 0);
#else
      if (TREE_CODE (fnaddr) != ADDR_EXPR)
        /* This entry is an offset: a virtual base class offset, a
           virtual call offset, and RTTI offset, etc.  */
        continue;

      fn = TREE_OPERAND (fnaddr, 0);
#endif

#ifdef KEY
      // As shown by bug 3133, some objects are emitted by g++ even though they
      // are weak and external.
      if (DECL_EMITTED_BY_GXX(fn)) {
	needed = TRUE;
	break;
      }
#endif

      if (!DECL_EXTERNAL(fn) &&
          !DECL_WEAK(fn)
#ifndef KEY	// Under g++ 3.2 -O3, all functions are marked DECL_INLINE.
          && !DECL_INLINE(fn)
#endif
	  ) {
        needed = TRUE;
        break;
      }
    }
  }

  return needed;
}

void
WFE_Process_Var_Decl (tree decl)
{
//fprintf(stderr, "VAR_DECL: %s\n", IDENTIFIER_POINTER(DECL_NAME(decl)));
  ST *st;
  if (TREE_PUBLIC(decl)    &&
//    !DECL_WEAK(decl)     &&
      !DECL_EXTERNAL(decl) &&
      !DECL_ST(decl)) {
    if (!DECL_WEAK(decl)
	|| decl_is_needed_vtable (decl)
#ifdef KEY
	|| decl_is_needed_vtt (decl)	// Bug 7442.
#endif
        ) {
#ifdef KEY
      WFE_Expand_Decl(decl);
#else
      DECL_ST(decl) = (ST *) 1;
      Push_Deferred_Function (decl);
#endif
    }
  }
} /* WFE_Process_Var_Decl */

void
WFE_Process_Function_Decl (tree decl)
{
//fprintf(stderr, "FUNCTION_DECL: %s\n", IDENTIFIER_POINTER(DECL_NAME(decl)));
  tree body;
  ST *st;
  body = DECL_SAVED_TREE(decl);
  if (body != NULL_TREE && !DECL_EXTERNAL(decl) &&
      !DECL_ARTIFICIAL(decl) &&
      !DECL_INLINE(decl) &&
      DECL_ST(decl) == NULL &&
      (DECL_TEMPLATE_INFO(decl) == NULL              ||
      DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION(decl) ||
      DECL_TEMPLATE_INSTANTIATED(decl)              ||
      DECL_TEMPLATE_SPECIALIZATION(decl))) {
#ifdef KEY
    set_DECL_ST(decl, (ST *) 1);
#else
    DECL_ST(decl) = (ST *) 1;
#endif
    Push_Deferred_Function (decl);
  }
} /* WFE_Process_Function_Decl */

void
WFE_Process_Decl (tree decl)
{
  switch (TREE_CODE (decl)) {

    case NAMESPACE_DECL:
      WFE_Process_Namespace_Decl (decl);
      break;

    case CONST_DECL:
      break;

    case TYPE_DECL:
      WFE_Process_Type_Decl (decl);
      break;

    case TEMPLATE_DECL:
      WFE_Process_Template_Decl (decl);
      break;

    case VAR_DECL:
      WFE_Process_Var_Decl (decl);
      break;

    case FUNCTION_DECL:
      WFE_Process_Function_Decl (decl);
      break;

    default:
      break;
  }
} /* WFE_Process_Decl */

void
WFE_Process_Namespace_Decl (tree namespace_decl)
{
//fprintf(stderr, "NAMESPACE_DECL: %s\n", IDENTIFIER_POINTER(DECL_NAME(namespace_decl)));
  tree decl;
  if (!DECL_NAMESPACE_ALIAS(namespace_decl) && (namespace_decl != std_node)) {
    for (decl = cp_namespace_decls(namespace_decl);
         decl != NULL_TREE;
         decl = TREE_CHAIN(decl)) {
      WFE_Process_Decl(decl);
    }
  }
} /* WFE_Process_Namespace_Decl */

extern "C"
void
WFE_Expand_Top_Level_Decl (tree top_level_decl)
{
  int error_count, sorry_count;
#ifdef KEY
  tree old_namespace_decl = curr_namespace_decl;
  curr_namespace_decl = top_level_decl;
#endif

  if (top_level_decl == global_namespace) {
   check_gnu_errors (&error_count, &sorry_count);
   if (error_count || sorry_count)
     return;
    Init_Deferred_Function_Stack();
    Init_Deferred_Decl_Init_Stack();
  }

  if (!Enable_WFE_DFE) {
#ifdef KEY
    // Emit asm statements at global scope, before expanding the functions,
    // to prevent them from getting into wrong sections (e.g. .except_table)
    std::vector<char *>::iterator asm_iter;
    for (asm_iter = gxx_emitted_asm.begin();
         asm_iter != gxx_emitted_asm.end();
	 asm_iter++)
      WFE_Assemble_Asm (*asm_iter);
#endif

    WFE_Expand_Decl (top_level_decl);

#ifdef KEY
    // Catch all the functions that are emitted by g++ that we haven't
    // translated into WHIRL.
    std::vector<tree>::iterator it;
    int changed;
    do {
      changed = 0;
      for (it = gxx_emitted_decls.begin(); 
           it != gxx_emitted_decls.end();
           it++) {
        tree decl = *it;
        if (expanded_decl(decl) == TRUE)
          continue;
        if (TREE_CODE(decl) == FUNCTION_DECL) {
	  if (DECL_THUNK_P(decl))
	    WFE_Generate_Thunk(decl);
	  else if (DECL_ALIAS_TARGET(decl))	// Bug 4393.
	    changed |= WFE_Assemble_Alias(decl, DECL_ALIAS_TARGET(decl));
	  else        
	    changed |= WFE_Expand_Function_Body(decl);
	  // Bugs 4471, 3041, 3531, 3572, 4563.
	  // In line with the non-debug compilation, delay the DST entry 
	  // creation for member functions for debug compilation.
	  // This avoids the following problem 
	  // (as noted by Tim in tree_symtab.cxx)
	  // Expanding the methods earlier "will cause error when the
	  // methods are for a class B that appears as a field in an
	  // enclosing class A.  When Get_TY is run for A, it will
	  // call Get_TY for B in order to calculate A's field ID's.
	  // (Need Get_TY to find B's TYPE_FIELD_IDS_USED.)  If
	  // Get_TY uses the code below to expand B's methods, it
	  // will lead to error because the expansion requires the
	  // field ID's of the enclosing record (A), and these field
	  // ID's are not yet defined".
	  if (Debug_Level > 0) {
	    tree context = DECL_CONTEXT(decl);
	    if (context && TREE_CODE(context) == RECORD_TYPE) {
	      // member function
	      // g++ seems to put the artificial ones in the output too 
	      // for some reason. In particular, operator= is there.  We do 
	      // want to omit the __base_ctor stuff though
	      BOOL skip = FALSE;
	      if (IDENTIFIER_CTOR_OR_DTOR_P(DECL_NAME(decl)))
	        skip = TRUE;
	      else if (DECL_THUNK_P(decl)) {
		// Skip thunk to constructors and destructors.  Bug 6427.
	        tree addr = DECL_INITIAL_2(decl);
		Is_True(TREE_CODE(addr) == ADDR_EXPR &&
			TREE_CODE(TREE_OPERAND(addr, 0)) == FUNCTION_DECL,
			("WFE_Expand_Top_Level_Decl: invalid thunk decl"));
		skip =
		  IDENTIFIER_CTOR_OR_DTOR_P(DECL_NAME(TREE_OPERAND(addr, 0)));
	      }
	      if (!skip) {
	      	TY_IDX context_ty_idx = Get_TY(context);
		// The type could have been newly created by the above Get_TY.
		// If so, call add_deferred_DST_types to create the DST for it.
		add_deferred_DST_types();
		DST_INFO_IDX context_dst_idx = TYPE_DST_IDX(context);
		DST_enter_member_function(context, context_dst_idx,
					  context_ty_idx, decl);
	      }
	    }
	  }
        } else if (TREE_CODE(decl) == VAR_DECL) {
	  WFE_Process_Var_Decl (decl);
        } else if (TREE_CODE(decl) == NAMESPACE_DECL) {
	  WFE_Expand_Decl (decl);
        } else {
	  FmtAssert(FALSE, ("WFE_Expand_Top_Level_Decl: invalid node"));
        }
      }
    } while (changed);	// Repeat until emitted all needed copy constructors.
    // Emit any typeinfos that we have referenced
    for (it = emit_typeinfos.begin(); it != emit_typeinfos.end(); ++it) {
    	tree decl = *it;
	if (expanded_decl (decl))
	    continue;
	expanded_decl (decl) = TRUE;
	FmtAssert (TREE_CODE (decl) == VAR_DECL, ("Unexpected node in typeinfo"));
	WFE_Expand_Decl (decl);
    }
#endif

  } else {

    WFE_Process_Namespace_Decl (top_level_decl);

    tree  decl;
    INT32 i;
    for (i = deferred_function_i;  i >= 0; --i) {
       decl = deferred_function_stack [i];
#ifdef KEY
       set_DECL_ST(decl, NULL);
#else
       DECL_ST(decl) = NULL;
#endif
    }

    ST *st;
    for (i = deferred_function_i;  i >= 0; --i) {
       decl = deferred_function_stack [i];
       st = Get_ST (decl);
//   st->Print (stderr, TRUE);
    }

    while (deferred_function_i >= 0) {
      decl = Pop_Deferred_Function ();
      if (TREE_CODE(decl) == FUNCTION_DECL)
        if (DECL_THUNK_P(decl))
          WFE_Generate_Thunk(decl);
        else        
          WFE_Expand_Function_Body (decl);
      else {
        st = DECL_ST(decl);
        if (ST_sclass(st) == SCLASS_EXTERN)
          Set_ST_sclass (st, SCLASS_UGLOBAL);
        WFE_Expand_Decl (decl);
      }
    }
  }

#ifdef KEY
  {
    int i;
    // Can't use iterator to access emit_decls because Get_TY may grow
    // emit_decls, which invalids all iterators.  Use operator[] instead.
    for (i = 0; i < emit_decls.size(); i++) {
      tree decl = emit_decls[i];
      if (TREE_CODE(decl) == VAR_DECL)
	WFE_Expand_Decl(decl);
      else if (TREE_CODE(decl) == RECORD_TYPE ||
	       TREE_CODE(decl) == UNION_TYPE)
	Get_TY(decl);
      else
	Is_True(FALSE, ("WFE_Expand_Top_Level_Decl: unexpected tree type"));
    }
  }

  {
    // Set the type for fields whose type we want to set last.
    // Don't use iterator because defer_fields may grow.
    for (int i = 0; i < defer_fields.size(); i++) {
      tree field = defer_fields[i].first;
      FLD_HANDLE fld = defer_fields[i].second;
      Is_True(TREE_CODE(field) == FIELD_DECL,
	      ("WFE_Expand_Top_Level_Decl: FIELD_DECL not found"));
      // Currently we defer only pointer types.
      Is_True(TREE_CODE(TREE_TYPE(field)) == POINTER_TYPE,
	      ("WFE_Expand_Top_Level_Decl: POINTER_TYPE not found"));
      TY_IDX fty_idx = Get_TY(TREE_TYPE(field));
      Set_FLD_type(fld, fty_idx);
    }
  }

  {
    // Create DST info.
    int i;

    // Add DSTs for types.
    add_deferred_DST_types();

    // Add DSTs for member functions.  Do this after all the types are handled.
    for (i = 0; i < defer_DST_misc.size(); i++) {
      DST_defer_misc_info *p = defer_DST_misc[i];
      if (p->is_member_function) {
	tree context = p->u1.member_func.context;
	DST_INFO_IDX context_dst_idx = TYPE_DST_IDX(context);
	TY_IDX context_ty_idx = Get_TY(context);
	DST_enter_member_function(context, context_dst_idx, context_ty_idx,
				  p->u1.member_func.fndecl);
      }
    }

    // Add DSTs for new types created.  (Don't know if any new type is ever
    // created.)
    add_deferred_DST_types();
  }

  curr_namespace_decl = old_namespace_decl;
#endif
} /* WFE_Expand_Top_Level_Decl */

#ifdef KEY
// Add DSTs for the defered types.  We defer adding DSTs for types in order to
// avoid calling Get_TY on partially constructed structs, since their field IDs
// are not yet valid.  It is safe to call add_deferred_DST_types to add DSTs
// whenever we are sure there are no partially constructed types.
void
add_deferred_DST_types()
{
  static int last_type_index = -1;
  int i;

  for (i = last_type_index + 1; i < defer_DST_types.size(); i++) {
    DST_defer_type_info *p = defer_DST_types[i];
    DST_INFO_IDX dst = Create_DST_type_For_Tree(p->t, p->ttidx, p->idx);
    TYPE_DST_IDX(p->t) = dst;
  }
  last_type_index = defer_DST_types.size() - 1;
}


// Get the target function that the thunk transfers control to.  The target is
// an ADDR_EXPR saved in DECL_INITIAL.  However, in GCC 3.2, the ADDR_EXPR in
// DECL_INITIAL could have been replaced by a BLOCK node.  In that case, get
// the ADDR_EXPR from DECL_INITIAL_2, which saves ADDR_EXPR for just this
// purpose.
static tree
WFE_get_thunk_target (tree decl)
{
  if (TREE_CODE (DECL_INITIAL(decl)) == ADDR_EXPR) {
    return DECL_INITIAL(decl);
  } else {
    Is_True(DECL_INITIAL_2(decl) &&
	    TREE_CODE(DECL_INITIAL_2(decl)) == ADDR_EXPR,
           ("ADDR_EXPR not found for thunk"));
    return DECL_INITIAL_2(decl);
  }
}

// If g++ performed named return value (nrv) optimization on the function FN,
// then modify the gnu tree to reflect the optimization.  That way, the WHIRL
// generated from the tree will automatically include the optimization.
static void
WFE_Handle_Named_Return_Value (tree fn)
{
  named_ret_obj_initializer = NULL_TREE;

  // Quit if the nrv opt was not done to this function.  If there is nrv,
  // current_function_return_value is the VAR_DECL of the local object to be
  // returned.
  tree named_ret_obj = DECL_NAMED_RETURN_OBJECT(fn);
  if (named_ret_obj == NULL_TREE)
    return;

  FmtAssert(TREE_CODE(named_ret_obj) == VAR_DECL,
	 ("WFE_Handle_Named_Return_Value: named return object not a VAR_DECL"));

  // Even though we won't use the variable, record its existence in the symbol
  // table in order to generate DWARF for it.  Bug 4900.
  Get_ST(named_ret_obj);

  // The return type should be returned in memory.
  TY_IDX ret_ty_idx = Get_TY(TREE_TYPE(TREE_TYPE(fn)));
  FmtAssert(TY_return_in_mem(ret_ty_idx),
	    ("WFE_Handle_Named_Return_Value: nrv type not in mem"));

  // Get the ST for the fake first parm.
  WN *first_formal = WN_formal(Current_Entry_WN(), 0);

  // Change the named return object's VAR_DECL node to be an INDIRECT_REF of
  // the fake first parm, so that whenever the VAR_DECL is accessed, it will
  // access the return area.  If the VAR_DECL had an initializer, create a
  // TARGET_EXPR to initialize the indirect ref with this initializer.
  if (DECL_INITIAL(named_ret_obj)) {
    named_ret_obj_initializer = build(TARGET_EXPR, TREE_TYPE(named_ret_obj),
				      named_ret_obj,
				      DECL_INITIAL(named_ret_obj),
				      NULL_TREE, NULL_TREE);
    TREE_SIDE_EFFECTS(named_ret_obj_initializer) = 1;
  }

  tree ptr_var = build_decl(VAR_DECL, NULL_TREE,
			    build_pointer_type(TREE_TYPE(TREE_TYPE(fn))));
  TREE_SET_CODE(named_ret_obj, INDIRECT_REF);
  TREE_OPERAND(named_ret_obj, 0) = ptr_var;
  set_DECL_ST(ptr_var, WN_st(first_formal));

  // Bug 4900 - set the location attribute for the DST entry for named_ret_obj
  // to point to first_formal; and also set DW_OP_deref for the DST entry.
  if (Debug_Level >= 2) {
    DST_INFO_IDX info_idx = DECL_DST_IDX(named_ret_obj);
    DST_ATTR_IDX attr_idx = DST_INFO_attributes(DST_INFO_IDX_TO_PTR(info_idx));
    DST_VARIABLE *attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);
    DST_ASSOC_INFO_fe_ptr(DST_VARIABLE_def_st(attr)) = 
      (void *)ST_st_idx(WN_st(first_formal));
    DST_SET_deref(DST_INFO_flag( DST_INFO_IDX_TO_PTR(info_idx)));
  }
}
#endif
