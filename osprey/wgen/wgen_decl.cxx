/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 PathScale, LLC.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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
extern "C" {
#include "gspin-wgen-interface.h"
}

#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "errors.h"

#include "glob.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "const.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "erglob.h"  // EC_Unimplemented_Feature
#include "wgen_decl.h"
#include "wgen_misc.h"
#include "wgen_dst.h"
#include "wgen_spin_symbol.h"
#include "wgen_expr.h"
#include "wgen_stmt.h"
#include "wgen_tracing.h"
//#include "tree_cmp.h"
#include "wgen_dst.h" // DST_enter_member_function
#include "dwarf_DST_dump.h"
#include "targ_sim.h" // PUSH_RETURN_ADDRESS_ON_STACK
#include "wgen_omp_directives.h"

#ifdef KEY
#include <ext/hash_map>
#endif

extern BOOL c_omit_external; // for C programs, omit generating functions with
			     // gs_decl_external = TRUE, which is the default

int flag_openmp;
gs_t decl_arguments;

static gs_t WGEN_get_thunk_target (gs_t decl);
static gs_t WGEN_get_final_thunk_target (gs_t decl);
static void WGEN_Handle_Named_Return_Value(gs_t fn);
static WN *WGEN_Start_Function(gs_t fndecl);
static void WGEN_Finish_Function(gs_t fndecl);

// The initializer for the named return value object.  Expand this in place of
// the DECL_INITIAL in the object's VAR_DECL.
// IMPORTANT:  Doesn't work for nested functions.
gs_t named_ret_obj_initializer;

/* ST to represent EXC_PTR_EXPR if C++ exceptions are disabled */
ST * Dummy_Exc_Ptr_Expr = NULL;

static gs_t *deferred_function_stack;
static INT32 deferred_function_i;
static INT32 deferred_function_max;
static BOOL flag_keep_inline_functions = FALSE;
static UINT32 WGEN_PU_count = 0;
static BOOL finish_alias = FALSE;

void 
Init_Deferred_Function_Stack()
{
  deferred_function_max   = 32;
  deferred_function_i     = -1;
  deferred_function_stack =
    (gs_t *) malloc (sizeof (gs_t) * deferred_function_max);
}

void
Push_Deferred_Function (gs_t decl)
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
      (gs_t *) realloc (deferred_function_stack,
			deferred_function_max * sizeof(gs_t));
  }

  deferred_function_stack[deferred_function_i] = decl;
}

static gs_t 
Pop_Deferred_Function (void)
{
  gs_t decl;
  decl = deferred_function_stack[deferred_function_i--];
//fprintf(stderr, "Pop_Deferred_Function: %p %s\n", decl, IDENTIFIER_POINTER(DECL_NAME(decl)));
  return decl;
}

static gs_t *deferred_decl_init_stack;
static INT32 deferred_decl_init_i;
static INT32 deferred_decl_init_max;

void 
Init_Deferred_Decl_Init_Stack ()
{
  deferred_decl_init_max   = 32;
  deferred_decl_init_i     = -1;
  deferred_decl_init_stack =
    (gs_t *) malloc (sizeof (gs_t) * deferred_decl_init_max);
} /* Init_Deferred_Decl_Init_Stack */

void
Push_Deferred_Decl_Init (gs_t decl)
{
  if (++deferred_decl_init_i == deferred_decl_init_max) {
    deferred_decl_init_max = 2 * deferred_decl_init_max;
    deferred_decl_init_stack =
      (gs_t *) realloc (deferred_decl_init_stack,
			deferred_decl_init_max * sizeof(gs_t));
  }

  deferred_decl_init_stack[deferred_decl_init_i] = decl;
} /* Push_Deferred_Decl_Init */

static gs_t
Pop_Deferred_Decl_Init (void)
{
  gs_t decl;
  decl = deferred_decl_init_stack[deferred_decl_init_i--];
  return decl;
} /* Pop_Deferred_Decl_Init */

// keep pu and pu_info
extern PU_Info *PU_Tree_Root;
static PU_Info *PU_Info_Table     [258] = {0};


static ST      *Return_Address_ST [258] = {0};
static BOOL map_mempool_initialized = FALSE;
static MEM_POOL Map_Mem_Pool;


// to manage the current function
static gs_t curr_namespace_decl = NULL;

static int __ctors = 0;
static int __dtors = 0;

static gs_t *func_decl_stack = NULL;
static INT func_decl_stack_size = 0;
static INT func_decl_stack_top = -1;

static void Push_Current_Function_Decl(gs_t decl){
  if (func_decl_stack_size == 0) {
    func_decl_stack = (gs_t *)malloc(sizeof(gs_t));
    func_decl_stack_size = 1;
  }
  func_decl_stack_top++;
  if (func_decl_stack_top == func_decl_stack_size) {
    func_decl_stack_size++;
    func_decl_stack = (gs_t *)realloc(func_decl_stack, 
			              func_decl_stack_size * sizeof(gs_t));
  }
  func_decl_stack[func_decl_stack_top] = decl;
}

gs_t Current_Function_Decl(void) {
  return func_decl_stack[func_decl_stack_top];
}

static void Pop_Current_Function_Decl(void){
  func_decl_stack_top--;
}

//to manage the entry_wn, current function's entry_wn always on top
static std::vector<WN*> curr_entry_wn;
static void Push_Current_Entry_WN(WN *wn) { curr_entry_wn.push_back(wn); }
static void Pop_Current_Entry_WN() { curr_entry_wn.pop_back(); }
WN *Current_Entry_WN(void) {
    if (curr_entry_wn.size()==0) {
        return NULL;
    } else {
        return curr_entry_wn.back(); 
    }
}

// Any typeinfo symbols that we have determined we should emit
std::vector<gs_t> emit_typeinfos;
// Any var_decls or type_decls that we want to expand last.
std::vector<gs_t> emit_decls;
// Struct fields whose type we want to set last.
std::vector<std::pair<gs_t, FLD_HANDLE> > defer_fields;

#ifdef KEY
std::vector <std::pair<gs_t, gs_t> > alias_vector;
#endif

#include <stack>
// Potentially OpenMP private variables. See comment in parse_end_decl(),
// some decls go through grokdeclarator, but are finished later.
// Is_shared_mp_var() requires the decl to be complete, so we push such
// decls in stack, and wait for their completion, before processing them
// as private variables.
std::stack<gs_t> mp_local_vars;

// Map a variable (e.g. typename X::y z) to its template-instantiated version
class tsubst_var {
  gs_t orig;      // Original variable, may not have a complete type
  gs_t sub;       // Template-substituted variable
  gs_t func_decl; // Containing function decl
  public:
  tsubst_var (gs_t o, gs_t s, gs_t f) : orig(o), sub(s), func_decl(f) {}
  gs_t equal (gs_t o, gs_t f) const {
    return (orig == o && func_decl == f) ? sub : NULL;
  }
};

std::vector<tsubst_var> tsubst_map;

void
template_substituted (gs_t orig, gs_t sub, gs_t func_decl) {
  tsubst_var in (orig, sub, func_decl);
  tsubst_map.push_back (in);
}

gs_t
get_substituted (gs_t orig, gs_t func_decl) {
  vector<tsubst_var>::const_iterator it = tsubst_map.begin();

  for (; it != tsubst_map.end(); it++) {
    gs_t sub = (*it).equal (orig, func_decl);
    if (sub) return sub;
  }
  return NULL;
}

void
push_mp_local_vars (gs_t decl)
{
  Is_True (flag_openmp, ("Should not reach here without -mp"));
  mp_local_vars.push (decl);
}

gs_t
pop_mp_local_vars (void)
{
  Is_True (flag_openmp, ("Should not reach here without -mp"));
  if (mp_local_vars.empty()) return NULL;
  gs_t decl = mp_local_vars.top ();
  mp_local_vars.pop ();
  return decl;
}

void
gxx_emits_typeinfos (gs_t t) {
  emit_typeinfos.push_back (t);
}

void
defer_decl (gs_t t) {
  emit_decls.push_back (t);
}

void
defer_field (gs_t t, FLD_HANDLE fld) {
  defer_fields.push_back (std::make_pair(t, fld));
}

// Support defer creating DST info until all types are created.
typedef struct {
  union {
    struct {		// for is_member_function = 1
      gs_t context;
      gs_t fndecl;
    } member_func;
  } u1;
  int is_member_function : 1;
} DST_defer_misc_info;

// List of things that we need to defer creating DST info for.
std::vector<DST_defer_misc_info *> defer_DST_misc;

typedef struct {
  gs_t t;
  TY_IDX ttidx;
  TY_IDX idx;
} DST_defer_type_info;

// List of types that we defer creating DST info for.
std::vector<DST_defer_type_info *> defer_DST_types;

void
defer_DST_type (gs_t t, TY_IDX ttidx, TY_IDX idx)
{
  DST_defer_type_info *p =
    (DST_defer_type_info *) calloc(1, sizeof(DST_defer_type_info));
  p->t = t;
  p->ttidx = ttidx;
  p->idx = idx;
  defer_DST_types.push_back(p);
}

void
defer_DST_member_function (gs_t context, gs_t fndecl)
{
  DST_defer_misc_info *p =
    (DST_defer_misc_info *) calloc(1, sizeof(DST_defer_misc_info));
  p->u1.member_func.context = context;
  p->u1.member_func.fndecl = fndecl;
  p->is_member_function = 1;
  defer_DST_misc.push_back (p);
}

// Return 1 if we actually expand decl.
static int
WGEN_Expand_Function_Body (gs_t decl)
{
  gs_t body;

  if (expanded_decl(decl) == TRUE)
    return 0;

  if (gs_decl_saved_tree(decl) == NULL) // wgen
    return 0;

  // bug 2694
  // If decl is a copy constructor that is not (yet) referenced by the WHIRL,
  // then delay its expansion because we don't know if it is really needed.
  // Currently all copy constructors are marked as needed in the g++ front-end
  // regardless if they are really needed, in case the WHIRL needs them later.
  if (// see if decl should be public
      !(gs_tree_public(decl) && !gs_decl_comdat(decl)) &&
      // check for declared inline (same as cp/semantics.c:expand_body)
      gs_decl_inline(decl) &&
      !flag_keep_inline_functions &&
      // check for copy constructor
      gs_decl_copy_constructor_p(decl) &&
      gs_decl_complete_constructor_p(decl) &&
      // check for reference by WHIRL
      gs_decl_assembler_name(decl) &&
      !REFERENCED_BY_WHIRL(gs_decl_assembler_name(decl))) {
    return 0;
  }

#ifdef KEY
  // These DECL_ flags are set for C also, but we don't yet see the need
  // for these flags for C. So wgen considers these flags only for C++.
  if (lang_cplus && !gs_decl_needed(decl) && !gs_decl_reachable(decl))
    return 0;
#endif

  expanded_decl(decl) = TRUE;

  TRACE_EXPAND_GS(decl);
  (void) WGEN_Start_Function(decl);
  Push_Current_Function_Decl(decl);

  WGEN_Handle_Named_Return_Value(decl);

  gs_t bind_expr = gs_decl_saved_tree(decl);

  for (body = bind_expr; body; body = gs_tree_chain(body))
    Mark_Scopes_And_Labels (body);

  for (body = bind_expr; body; body = gs_tree_chain(body))
    WGEN_Expand_Stmt(body);

#ifdef KEY
  // Bug 11904: We should attempt to expand any deferred DSTs now,
  // because VLA types may be in terms of a local symbol, which won't
  // be valid if we go out of scope and process the DST for that type.
  add_deferred_DST_types();
#endif

  WGEN_Finish_Function(decl);
  Pop_Current_Function_Decl();
  WGEN_PU_count++;
  return 1;
}

extern "C" {
void
WGEN_Whirlify_Decl(gs_t decl)
{
  gs_code_t code = gs_tree_code(decl);
  switch(code) {
    case GS_FUNCTION_DECL:
	WGEN_Expand_Function_Body(decl);
	  //Dump_DST(stdout); // wgen TODO: without this, the DST will has missing subprograms
	break;
    case GS_CONST_DECL: // wgen: do nothing
	break;
    case GS_VAR_DECL:
      // Handle decls that are aliases for other decls
      if (gs_decl_alias_target(decl)) {
	WGEN_Assemble_Alias(decl, gs_decl_alias_target(decl));
	break;
      }
      // fall thru
    default: 
	FmtAssert(code != GS_PARM_DECL, ("PARM_DECL node found at top level"));
	if (gs_decl_initial(decl) != 0 && 
	    gs_decl_initial(decl) != gs_error_mark_node())
	  WGEN_Initialize_Decl(decl);
	WGEN_Decl(decl);
	break;
  }
}
}

// KEY: Adjust wn using fixed_offset and virtual_offset. adjust_this
// indicates if we want a this-pointer adjustment, or a return-result
// adjustment.
static WN *
WGEN_Adjust_Thunk (WN * wn, BOOL adjust_this,
                   INT fixed_offset, gs_t virtual_offset) // KEY version
{
  ST * ptr_st = WN_st(wn);
  TY_IDX ptr_ty = ST_type (ptr_st);
  TYPE_ID ptr_mtype = TY_mtype (ptr_ty);

  if (adjust_this)
    wn = WN_Binary (OPR_ADD, ptr_mtype,
                    wn,
                    WN_Intconst (ptr_mtype, fixed_offset));
  if (virtual_offset != 0) {
    DevWarn ("Generating thunk with vcall adjustment at line %d\n", lineno);
    TY_IDX pdiff;
    // GCC's ptrdiff_type_node is integer type.  Convert to unsigned because
    // pointers are unsigned.
    switch (TY_mtype(Get_TY(gs_ptrdiff_type_node()))) {
      case MTYPE_I4:
      case MTYPE_U4:
	pdiff = MTYPE_To_TY(MTYPE_U4);
	break;
      case MTYPE_I8:
      case MTYPE_U8:
	pdiff = MTYPE_To_TY(MTYPE_U8);
	break;
      default:
	FmtAssert(FALSE, ("WGEN_Adjust_Thunk unexpected type"));
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
    FmtAssert(gs_get_integer_value(virtual_offset) >> 32 == 0 ||
	      gs_get_integer_value(virtual_offset) >> 32 == -1,
	      ("WGEN_Adjust_Thunk unexpected integer size"));
    INT32 offset = gs_get_integer_value(virtual_offset);
    wn = WN_Binary (OPR_ADD, ptr_mtype,
                    wn,
                    WN_CreateIload(OPR_ILOAD,
                                   TY_mtype(pdiff), TY_mtype(p_pdiff),
                                   offset, pdiff, p_pdiff, deref));
  }

  if (!adjust_this)
    wn = WN_Binary (OPR_ADD, ptr_mtype,
                    wn,
                    WN_Intconst (ptr_mtype, fixed_offset));
  wn = WN_Stid (ptr_mtype, 0, ptr_st, ptr_ty, wn);

  return wn;
}

// KEY: This function is used to generate a thunk to either adjust
//  the "this" pointer, or
//  the return pointer.
// If the "this" pointer needs to be adjusted, then we need to do
// the following:
//     -----------
//     this += delta;
//     if (virtual_offset != 0)
//       this += (*((ptrdiff_t **) this))[virtual_offset];
//     call target (this);
//     -----------
// Else generate the following:
//     -----------
//     if (virtual_offset != 0)
//       this += (*((ptrdiff_t **) this))[virtual_offset];
//     ret = call target (this);
//     if (ret-is-not-pointer-type /* compile-time */ || ret != NULL)
//       ret += delta;
//     return ret;
//     -----------
static void
WGEN_Generate_Thunk (gs_t decl)
{
#ifdef KEY
  if (expanded_decl(decl) == TRUE)
    return;
  expanded_decl(decl) = TRUE;
#endif

  Is_True(decl != NULL &&
          gs_tree_code(decl) == GS_FUNCTION_DECL &&
          gs_decl_thunk_p(decl) &&
          gs_tree_code(gs_cp_decl_context(decl)) != GS_NAMESPACE_DECL,
          ("Argument to WGEN_Generate_Thunk isn't a thunk"));

  Is_True(gs_thunk_target(decl) != NULL,
          ("Argument to WGEN_Generate_Thunk has null thunk target"));

  ST      *thunk_st  = Get_ST (decl);
#ifdef KEY
  // Needed for GCC 3.2.  See comment in WGEN_get_thunk_target.
  gs_t    thunk_target = WGEN_get_thunk_target(decl);
  ST      *func_st   = Get_ST (thunk_target);
#else
  ST      *func_st   = Get_ST (gs_tree_operand (gs_decl_initial(decl), 0));
#endif	// KEY
  TYPE_ID  ret_mtype = TY_mtype (TY_ret_type (ST_pu_type (func_st)));
  WN      *entry_wn  = WGEN_Start_Function (decl);
  INT32    nargs     = WN_kid_count (entry_wn) - 3;
  INT32    i;
  ST      *arg_st;
  TY_IDX   arg_ty;
  TYPE_ID  arg_mtype;
  WN      *arg_wn;
  WN      *wn;
  WN      *call_wn;

  // adjust this pointer or return pointer?
  BOOL    adjust_this = gs_decl_this_thunk_p (decl); // KEY

  // modify this parameter by the delta
#ifdef KEY
  // If kg++fe added a fake arg0 (because the function needs to return the
  // object in memory), then the "this" pointer is at arg1.  Bug 5017.
  TY_IDX ret_ty_idx = Get_TY(gs_tree_type(gs_tree_type(decl)));
  if (TY_return_in_mem(ret_ty_idx))
    arg_st = WN_st (WN_kid1 (entry_wn));
  else
#endif
  arg_st = WN_st (WN_kid0 (entry_wn));
  arg_ty = ST_type (arg_st);
  arg_mtype = TY_mtype (arg_ty);

  INT fixed_offset = gs_thunk_fixed_offset(decl);
  gs_t virtual_offset = gs_thunk_virtual_offset(decl);
  if (virtual_offset && !adjust_this)
    virtual_offset = gs_binfo_vptr_field(virtual_offset);

  if (adjust_this) {
    // adjust "this" parameter
    WN * thunk_wn = WN_Ldid (arg_mtype, 0, arg_st, arg_ty);
    wn = WGEN_Adjust_Thunk (thunk_wn, TRUE, fixed_offset, virtual_offset);
    WGEN_Stmt_Append (wn, Get_Srcpos());
  }

  // generate call to base function
  call_wn = WN_Create (OPR_CALL, ret_mtype, MTYPE_V, nargs);
  WN_st_idx (call_wn) = ST_st_idx (func_st);
  WN_Set_Call_Default_Flags (call_wn);
  WN_Set_Call_Replace_By_Jump (call_wn);
  // set up args
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

    WGEN_Stmt_Append (call_wn, Get_Srcpos());
    wn = WN_CreateReturn ();
    WGEN_Stmt_Append (wn, Get_Srcpos());
  }

  else {

    WN *block_wn = WN_CreateBlock ();
    WN_INSERT_BlockLast (block_wn, call_wn);
    wn = WN_Ldid (ret_mtype, -1, Return_Val_Preg, TY_ret_type(ST_pu_type(func_st)));
    wn = WN_CreateComma (OPR_COMMA, Mtype_comparison (ret_mtype), MTYPE_V,
			 block_wn, wn);
    if (!adjust_this) {

      // adjust return result
      TY_IDX ret_type = MTYPE_TO_TY_array[ret_mtype];
      ST * ret_st = Gen_Temp_Symbol (ret_type, "__thunk_base");
      wn = WN_Stid (ret_mtype, 0, ret_st, ret_type, wn);
      WGEN_Stmt_Append (wn, Get_Srcpos());
      WN * thunk_wn = WN_Ldid (ret_mtype, 0, ret_st, ret_type);
      wn = WGEN_Adjust_Thunk (thunk_wn, FALSE, fixed_offset, virtual_offset);

      BOOL call_returns_ptr = gs_tree_code
                               (gs_tree_type
                                (gs_tree_type
                                 (thunk_target))) == GS_POINTER_TYPE;
      if (call_returns_ptr) {

        // guard the result adjustment with a check for null-pointer.
        WN * test = WN_NE(ret_mtype, 
                          WN_Ldid (ret_mtype, 0, ret_st, ret_type),
                          WN_Intconst (ret_mtype, 0));
        WN * then_block = WN_CreateBlock();
        WN * else_block = WN_CreateBlock();
        WN * if_stmt = WN_CreateIf (test, then_block, else_block);
        WN_INSERT_BlockLast (then_block, wn);
        WGEN_Stmt_Append (if_stmt, Get_Srcpos());
      }
      
      else {
        WGEN_Stmt_Append (wn, Get_Srcpos());
      }
      // load adjusted result
      wn = WN_Ldid (ret_mtype, 0, ret_st, ret_type);
    }

    wn = WN_CreateReturn_Val (OPR_RETURN_VAL, ret_mtype, MTYPE_V, wn);
    WGEN_Stmt_Append (wn, Get_Srcpos());
  }

  Set_PU_is_thunk (Get_Current_PU ());
  WGEN_Finish_Function (decl);
}

static void process_local_classes()
{
}

static void WGEN_Assemble_Asm(char *asm_string);

void WGEN_Expand_Decl(gs_t decl, BOOL can_skip)
{
  if (decl != NULL && gs_code(decl) == GS_STRING_CST) {
    char *asm_string = gs_tree_string_pointer(decl);
    Set_FILE_INFO_has_global_asm(File_info);
    WGEN_Assemble_Asm (asm_string);
    return;
  }

  Is_True(decl != NULL && gs_tree_code_class(decl) == GS_TCC_DECLARATION,
          ("Argument to WGEN_Expand_Decl isn't a decl node"));
/*
  int error_count, sorry_count;
  if (decl == global_namespace) {
   check_gnu_errors (&error_count, &sorry_count);
   if (error_count || sorry_count)
     return;
    Init_Deferred_Function_Stack();
  }
*/
  switch (gs_tree_code(decl)) { 

    case GS_CONST_DECL:
      // DevWarn("WGEN_Expand_Decl:  don't know what to do with CONST_DECL");
      break;

    case GS_LABEL_DECL:
#ifdef KEY
      WGEN_Get_LABEL(decl, FALSE);
#else
      DevWarn("WGEN_Expand_Decl:  don't know what to do with LABEL_DECL");
#endif
      break;

    case GS_FUNCTION_DECL:
      // Skip if function was never translated by GCC into RTL and so was never
      // needed.
      if (gs_operand(decl, GS_FLAGS) == NULL)
        return;

#ifdef KEY
      // if it is a nested function, needs to process parent first
      if (gs_decl_context(decl) != NULL &&
	  gs_tree_code(gs_decl_context(decl)) == GS_FUNCTION_DECL) {
	WGEN_Expand_Decl(gs_decl_context(decl), FALSE);
      }
#endif
      if (gs_decl_thunk_p(decl) &&
          gs_tree_code(gs_cp_decl_context(decl)) != GS_NAMESPACE_DECL) {
        WGEN_Generate_Thunk(decl);
      }
#ifdef KEY
      // Handle decls that are aliases for other decls.  Bug 3841.
      else if (gs_decl_alias_target(decl)) {
	WGEN_Assemble_Alias(decl, gs_decl_alias_target(decl));
	return;
      }
#endif
      else {
        gs_t body = gs_decl_saved_tree(decl);
        if (body != NULL && 
	     (lang_cplus && !gs_decl_external(decl) ||
	      !lang_cplus && (!c_omit_external || 
			      !gs_decl_external(decl) || 
			      gs_decl_declared_inline_p(decl) || 
			      gs_decl_built_in(decl)/* bug 14254 */ )) &&
	     !gs_decl_maybe_in_charge_constructor_p(decl) &&
	     !gs_decl_maybe_in_charge_destructor_p(decl)) {

#ifndef KEY	// Don't call Get_ST we don't have a PU to attach the ST to
		// yet.  It seems this Get_ST is unnecessary anyway because
		// Get_ST is called again in WGEN_Start_Function, which is
		// called by WGEN_Expand_Function_Body below.
         (void) Get_ST(decl);
#endif
         if (!Enable_WFE_DFE) {
	  // Don't defer a function since Pop_Deferred_Function is called only
	  // if Enable_WFE_DFE is ON.
#ifndef KEY
          if (CURRENT_SYMTAB != GLOBAL_SYMTAB ||
              gs_decl_function_member_p(decl)    ||
              strncmp (gs_identifier_pointer(gs_decl_name(decl)), "__tcf", 5) == 0)
            Push_Deferred_Function (decl);
          else
#endif
            WGEN_Expand_Function_Body (decl);
         }
        }
      }

      break;

    case GS_NAMESPACE_DECL: {
      /* We assume for now that there are no TREE_LIST nodes among
       * the namespace declarations.  
       */

      /*      if (decl == std_node)
	      break; // ignore namespace std */
      if (gs_decl_namespace_alias(decl))
	break;
#ifdef KEY
      gs_t old_namespace_decl = curr_namespace_decl;
      curr_namespace_decl = decl;
#endif
      gs_t subdecl;
      for (subdecl = gs_cp_namespace_decls(decl);
	   subdecl != NULL;
	   subdecl = gs_tree_chain(subdecl))
	WGEN_Expand_Decl(subdecl, TRUE);
#ifndef KEY
      while (deferred_function_i != -1) {
//      fprintf(stderr, "NAMESPACE_DECL: Pop_Deferred_Function\n");
	WGEN_Expand_Function_Body (Pop_Deferred_Function ());
      }
#endif
#ifdef KEY
      curr_namespace_decl = old_namespace_decl;
#endif
      break;
    }

    case GS_TEMPLATE_DECL:
      {
      if (gs_decl_context(decl) && gs_tree_code(gs_decl_context(decl)) == GS_RECORD_TYPE)
	Get_TY(gs_decl_context(decl));
      gs_t gentemp = gs_most_general_template(decl);
      for (gs_t t = gs_decl_template_specializations(gentemp);
           t; t = gs_tree_chain(t))
	if (gs_tree_code(gs_tree_value(t)) == GS_FUNCTION_DECL &&
	    !gs_decl_external(gs_tree_value(t))	      &&
	    !gs_uses_template_parms (gs_tree_value(t)))
          WGEN_Expand_Decl (gs_tree_value(t), TRUE);
	gs_set_decl_template_specializations(gentemp, 0); // don't do these twice

      for (gs_t t = gs_decl_template_instantiations(gentemp);
	   t; t = gs_tree_chain(t)) {
	  gs_t val = gs_tree_value(t);
	  if (gs_tree_code(val) == GS_RECORD_TYPE &&
	      !gs_uses_template_parms(val))
	    Get_TY(val);
      }

      break;
    }

    case GS_TREE_VEC:
      break;
   
    case GS_TYPE_DECL: {
#ifdef KEY
      // bug 7341: Generate types only on demand. But bug 10506 shows we
      // should process a type when it is part of a DECL_EXPR statement,
      // because it may have side-effects.
      if (!can_skip || gs_tree_side_effects(decl))
#endif
      {
        gs_t type = gs_tree_type(decl);
        (void) Get_TY(type);
      }
      break;
    }

    case GS_VAR_DECL:
#ifdef KEY
// Don't emit the symbol if these flags don't have proper values.
// TREE_SYMBOL_REFERENCED == 0 does not always mean that we don't need to
// emit the symbol, so this condition may refuse to emit some needed symbols.
// But since we determine ourselves what symbols we NEED to emit, this condition
// here should not harm. And this should be able to prevent expansion of some
// really useless/unreferenced symbols.
// FIXME: If we can use the flag TREE_SYMBOL_REFERENCED as below, we should
// remove uses/definitions of TREE_NOT_EMITTED_BY_GXX.
	if (gs_decl_assembler_name_set_p(decl) && 
	    gs_tree_not_emitted_by_gxx(decl) &&
	    !gs_tree_symbol_referenced(gs_decl_assembler_name(decl))) {
	  expanded_decl(decl) = TRUE;
	  return;
	}

#ifdef FE_GNU_4_2_0
      // Bug 14091: For a TREE_STATIC variable, check if it is needed.
      // If a symbol is weak, then we can expand it.
      if (lang_cplus && gs_tree_static(decl) &&
          !gs_decl_weak(decl) && !gs_decl_needed(decl)) {
        expanded_decl(decl) = TRUE;
        return;
      }
#endif

      // Handle decls that are aliases for other decls.  Bug 3841.
      if (gs_decl_alias_target(decl)) {
	WGEN_Assemble_Alias(decl, gs_decl_alias_target(decl));
	return;
      }

      expanded_decl(decl) = TRUE;
#endif
      (void) Get_ST(decl);
      if (gs_decl_initial(decl) && !gs_decl_external(decl)) {
	gs_t init = gs_decl_initial(decl);
	gs_code_t code = gs_tree_code(init);
      	if (code == GS_ERROR_MARK)
	  return;

	if (code == GS_PTRMEM_CST)  {
	  init = gs_expanded_ptrmem_cst(init);
	  Is_True(init != NULL,
		  ("WGEN_Expand_Decl: expanded PTRMEM_CST is NULL"));
	  gs_set_decl_initial(decl, init);
  	}

	if (gs_tree_code(init) == GS_CONSTRUCTOR) {
  	  gs_t init_type = gs_tree_type(init);
#ifdef KEY
          // Bug 11261: Sometimes g++ knows not to need an initialization,
          // and hence has incomplete information (and never actually
          // emits that decl). If we fix gs_x_1 for var_decls to process
          // only the current decl, then this problem would go away.
          if (!init_type)
            return;
#endif
	  gs_code_t init_type_code = gs_tree_code(init_type);
	  if (init_type_code != GS_RECORD_TYPE &&
	      init_type_code != GS_ARRAY_TYPE  &&
	      init_type_code != GS_UNION_TYPE
#ifdef KEY
	      && init_type_code != GS_VECTOR_TYPE
#endif
	     )
	    return;
	  }

	WGEN_Initialize_Decl(decl);
      }
      break;

#ifdef KEY
    case GS_USING_DECL:
      // Do nothing.
      break;
#endif

    default:
      Is_True(FALSE, ("Unexpected tree code"));
      break;

  } /* switch */
} /* WGEN_Expand_Decl */

static BOOL
function_has_varargs(gs_t fndecl)
{
  gs_t fntype  = gs_tree_type(fndecl);
  gs_t arglist = gs_type_arg_types(fntype);

#ifdef KEY // bug 9688: cannot be vararg if there is not a single argument
  if (arglist == NULL)
    return FALSE;
#endif

  while (arglist != NULL) {
    if (gs_tree_value(arglist) == gs_void_type_node())
      return FALSE;
    arglist = gs_tree_chain(arglist);
  }

  return TRUE;
}

// Exception Handling
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
    TY_Init (New_TY(ty), (hbnd+1) * 4 , KIND_ARRAY, MTYPE_M, str); /// put attention here
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
    Set_PU_misc_info (Get_Current_PU(),
                      New_INITO (ST_st_idx (etable), exc_ptr_iv));
}

extern TY_IDX Type_For_Function_Returning_Void (void);


// Generate WHIRL representing an asm at file scope (between functions).
// Taken from kgccfe/wfe_decl.cxx
static void
WGEN_Assemble_Asm(char *asm_string)
{
  ST *asm_st = New_ST(GLOBAL_SYMTAB);
  TY_IDX ty_idx = Type_For_Function_Returning_Void();
  ST_Init(asm_st,
          Str_To_Index (Save_Str (asm_string),
                        Global_Strtab),
          CLASS_NAME,
          SCLASS_UNKNOWN,
          EXPORT_LOCAL,
	  ty_idx);
                                                                                
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
                                                                                
  /* This code patterned after "wfe_decl.cxx":WGEN_Start_Function, and
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
    PU_Init(pu, ty_idx, CURRENT_SYMTAB);
    Set_PU_no_inline(pu);
    Set_PU_no_delete(pu);
    Set_ST_pu(*asm_st, pu_idx);
                                                                                
    if (lang_cplus) 
      Set_PU_cxx_lang (Pu_Table [ST_pu (asm_st)]);
    else Set_PU_c_lang (Pu_Table [ST_pu (asm_st)]);

    Write_PU_Info (pu_info);
                                                                                
    // What does the following line do?
    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;
                                                                                
    Delete_Scope(CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
}

#ifdef KEY
BOOL expanding_function_definition = FALSE;
#endif

// str is supposed to be in the form ATTR, while attribute can be
// either of ATTR or __ATTR__ . Return TRUE if they match, else FALSE.
BOOL
is_attribute (const char * str, gs_t attribute)
{
  Is_True (gs_tree_code(attribute) == GS_IDENTIFIER_NODE,
           ("is_attribute: unexpected attribute TREE code"));
  const char * attr = gs_identifier_pointer(attribute);
  INT s_len = strlen(str);
  INT a_len = strlen(attr);

  // in ATTR form ?
  if (s_len == a_len &&
      !strcmp (str, attr))
    return TRUE;

  // in __ATTR__ form ?
  if (s_len + 4 == a_len &&
      attr[0] == '_' &&
      attr[1] == '_' &&
      attr[a_len - 2] == '_' &&
      attr[a_len - 1] == '_' &&
      !strncmp (str, attr + 2, s_len))
    return TRUE;

  return FALSE;
}

//******************************************************
// for a function
//******************************************************
static WN *
WGEN_Start_Function(gs_t fndecl)
{
   FmtAssert(gs_tree_code(fndecl)==GS_FUNCTION_DECL, 
           ("WGEN_Start_Function: not a function"));

    WN *entry_wn;
    BOOL  thunk = gs_decl_thunk_p(fndecl) &&
                  gs_tree_code(gs_cp_decl_context(fndecl)) != GS_NAMESPACE_DECL;

    // Add DSTs for all types seen so far.  Do this now because the expansion
    // of formal parameters needs those DSTs.
    add_deferred_DST_types();

    // Clear out the saved expr stack for new function.
    wgen_save_expr_stack_last = -1;

    // Initialize the cleanup level for identifying saved expr's.
    wgen_save_expr_level = 1;
    wgen_last_save_expr_level = 1;

    // Initialize label indexes that we are allowed to use.
    WGEN_unusable_label_idx = 0;
    WGEN_last_label_idx = 0;

    static gs_t prev_fndecl;
    if (CURRENT_SYMTAB != GLOBAL_SYMTAB) {
      // Get_Current_PU requires Scope_tab[Current_scope].st, which may or may
      // not be set.  So mark the func using its FUNCTION_DECL node and call
      // Set_PU_uplevel(Get_Current_PU()) later.  (The Get_ST(fndecl) invoked
      // by the current iteration of WGEN_Start_Function can lead to the
      // expansion of functions (methods) in the type definition of the current
      // function.  When WGEN_Start_Function is called to expand those
      // functions, Scope_tab[Current_scope].st for the original function is
      // not set, and Get_Current_PU() seg faults.)
      func_PU_uplevel(prev_fndecl) = TRUE;
    }
    prev_fndecl = fndecl;

    try_block_seen = false;

    decl_arguments = NULL;
   
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

    if (gs_decl_source_file (fndecl)) {
      lineno = gs_decl_source_line (fndecl);
      WGEN_Set_Line_And_File (lineno, gs_decl_source_file (fndecl));
    }

    // handle VLAs in the declaration
    WN *vla_block = WN_CreateBlock ();
    WGEN_Stmt_Push (vla_block, wgen_stmk_func_body, Get_Srcpos());

    ST *func_st;

    static BOOL interface_only = (!gs_pragma_implementation(program) || !gs_pragma_interface(program));

    bool extern_inline = FALSE;
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
    // bug 13385: There are some cases where C++ template functions marked
    // inline can still be externally accessible. Rely on GNU's "needed"
    // flag.
    if (lang_cplus) {
      if (interface_only &&
	  !gs_decl_needed (fndecl) && // bug 13385
	  gs_decl_weak (fndecl) &&
	  (gs_decl_declared_inline_p (fndecl) ||
	   (gs_decl_lang_specific (fndecl) &&
	    gs_decl_implicit_instantiation (fndecl) &&
	    gs_decl_namespace_scope_p (fndecl))))
#ifndef TARG_SL
	    eclass = EXPORT_INTERNAL; // bug 7550
#else
           eclass = EXPORT_PREEMPTIBLE;
#endif
      else if (gs_tree_public(fndecl) || gs_decl_weak(fndecl)) {
        if (!gs_decl_declared_inline_p(fndecl)) {
          // global non-inline function has to be preemptible
          // unless it is changed by visibility option/attribute
          eclass = EXPORT_PREEMPTIBLE;
        }
        else {
          // inline function definition should be appeared in every 
          // Translation Unit, it can be hidden (allow address be taken)
          // one example of implicit address taken is dtor address be 
          // used in __cxa_throw() arguments when the type is thrown
          //
          // gnu linker complains about protected symbol illegal relocation
          // for taking its address, the hidden symbol' addres is ok to export 
          // to other DSO or within the same module 
          if (!interface_only || keep_inline_functions || 
              !gs_decl_weak (fndecl)) {
            // there are inline functions not weak function, such as
            // global function's local class member, they have the same
            // export class as hosting function
            //
            // if the inline function is under pragma implementation,
            // or -fkeep-inline-function, it should be PREEMPTIBLE,
            // so it will not be DFEed
            eclass = EXPORT_PREEMPTIBLE;
          }
          else {
#ifndef TARG_SL
            eclass = EXPORT_HIDDEN;
#else 
            eclass = EXPORT_PREEMPTIBLE;
#endif
          }
	} 
      }
      else
	 eclass = EXPORT_LOCAL;
      func_st = Get_ST (fndecl);
      // KEY: bugs 8873, 11247, 11287
      // The following does not differentiate between a function marked
      // extern "C" and extern inline, from one marked extern "C" inline.
      // If this results in (performance) issues where we should not inline
      // functions marked extern "C" inline, then we will need to revisit.
      if (gs_decl_declared_inline_p(fndecl) &&
          gs_decl_lang_flag_2(fndecl) /* DECL_THIS_EXTERN */)
        extern_inline = TRUE;
    }
    else {
      eclass = gs_tree_public(fndecl) ? EXPORT_PREEMPTIBLE : EXPORT_LOCAL;
      if (gs_decl_declared_inline_p(fndecl) && gs_tree_public (fndecl)) {
	if (gs_decl_external(fndecl) && DECL_ST2 (fndecl) == 0) {
	  // encountered first extern inline definition
#ifdef KEY
	  // SiCortex 4800: The above comment does not look correct. The
	  // definition of a function can either reach this branch of the
	  // IF, or the other branch. It cannot reach both, because
	  // WGEN_Start_Function should only be called once for a fndecl.
	  //
	  // Use the ST if it was previously expanded.
	  func_st = Get_ST (fndecl);
	  // DECL_ST should be already set
	  DECL_ST (fndecl) = DECL_ST2 (fndecl) = func_st;
	  extern_inline = TRUE;
	  // Bug 14217: If this is a C extern inline function, it must be
	  // a global function, not XLOCAL (XLOCAL also causes IPA to
	  // modify the function name to make it unique, causing it to be
	  // undefined).
	  eclass = EXPORT_PREEMPTIBLE;
#else

	  ST *oldst = DECL_ST (fndecl);
	  DECL_ST (fndecl) = 0;
	  func_st =  Get_ST (fndecl);
	  DECL_ST (fndecl) = oldst;
	  DECL_ST2 (fndecl) = func_st;
	  extern_inline = TRUE;
	  eclass = EXPORT_LOCAL;
#endif
	}
	else {
	  // encountered second definition, the earlier one was extern inline
	  func_st = Get_ST (fndecl);
	  DECL_ST2(fndecl) = 0;
	}
      }
      else
	func_st = Get_ST (fndecl);
    }
 
    Set_ST_sclass (func_st, SCLASS_TEXT);
    Set_PU_lexical_level (Pu_Table [ST_pu (func_st)], CURRENT_SYMTAB);
    if (lang_cplus) 
      Set_PU_cxx_lang (Pu_Table [ST_pu (func_st)]);
    else Set_PU_c_lang (Pu_Table [ST_pu (func_st)]);
 
    if (gs_decl_declared_inline_p(fndecl))
      Set_PU_is_marked_inline (Pu_Table [ST_pu (func_st)]);
    if (gs_decl_inline(fndecl)) {
      Set_PU_is_inline_function (Pu_Table [ST_pu (func_st)]);
      wgen_invoke_inliner = TRUE;
    }
    Set_ST_export (func_st, eclass);

    gs_symbol_visibility_kind_t vk = 
      (gs_symbol_visibility_kind_t) gs_decl_visibility(fndecl);
    if (gs_tree_public(fndecl) && 
        (gs_decl_visibility_specified(fndecl) ||
         GS_VISIBILITY_DEFAULT != vk)) {
      ST_EXPORT export_class = EXPORT_PREEMPTIBLE;
      switch (vk) {
        case GS_VISIBILITY_DEFAULT:
          export_class = EXPORT_PREEMPTIBLE;
          break;
        case GS_VISIBILITY_PROTECTED:
          export_class = EXPORT_PROTECTED;
          break;
        case GS_VISIBILITY_HIDDEN:
          export_class = EXPORT_HIDDEN;
          break;
        case GS_VISIBILITY_INTERNAL:
          export_class = EXPORT_INTERNAL;
          break;
        default:
          GS_ASSERT(0, "unknown decl visibility");
          break;
      }
      Set_ST_export (func_st, export_class);
    }

    // For extern inline in C++, we ensure the function is inlined at > -O0,
    // when inlining is not disabled. IF the inliner thinks it appropriate,
    // it may also delete the function.
    if (extern_inline)
    {
	if (!lang_cplus)
	  Set_PU_is_extern_inline (Pu_Table [ST_pu (func_st)]); //bugs 2178, 2152
	if (Opt_Level > 0) // bug 2218
	  Set_PU_must_inline (Pu_Table [ST_pu (func_st)]);
    }

    if (gs_decl_attributes(fndecl) && 
        gs_tree_code(gs_decl_attributes(fndecl)) == GS_TREE_LIST) {
      gs_t nd;

      // bug 14180: For C++, the user-written function A with constructor
      // or destructor attribute, as well as the corresponding compiler-
      // generated function B will have the attribute. But the runtime
      // must call only B (which will in turn call A). So for C++, we should
      // handle these attributes only for compiler-generated functions.
      BOOL ctor_dtor_ok = !lang_cplus || gs_decl_artificial(fndecl);

      for (nd = gs_decl_attributes(fndecl);
           nd; nd = gs_tree_chain(nd)) {
	gs_t attr = gs_tree_purpose(nd);
	if (gs_tree_code(attr) == GS_IDENTIFIER_NODE) {
	  // bug 2395
	  BOOL is_ctors = FALSE;
	  if (is_attribute("noinline", attr))
	    Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);

	  // bug 2646
	  // If there is an 'always_inline' attribute, and function definition
	  // is before the call(s), GNU fe will do it. If definition is after
	  // any call, we need to handle it here.
	  // bug 11730: The above is no longer true in the new GNU4 front-end.
	  // Such functions are now not handled in GNU fe for both C/C++.
	  else if (is_attribute("always_inline", attr))
	      Set_PU_must_inline (Pu_Table [ST_pu (func_st)]);

	  // bug 11754
	  else if ((is_ctors = is_attribute("constructor", attr)) ||
	           is_attribute("destructor", attr)) {
	    if (ctor_dtor_ok) {
	      INITV_IDX initv = New_INITV ();
	      INITV_Init_Symoff (initv, func_st, 0, 1);
	      ST *init_st = New_ST (GLOBAL_SYMTAB);
	      ST_Init (init_st, Save_Str2i (is_ctors ? "__ctors" : "__dtors",
	    	     			    "_", ++__ctors),
		       CLASS_VAR, SCLASS_FSTATIC,
		       EXPORT_LOCAL,
		       Make_Pointer_Type (ST_pu_type (func_st), FALSE));
	      Set_ST_is_initialized (init_st);
	      INITO_IDX inito = New_INITO (init_st, initv);
	      ST_ATTR_IDX st_attr_idx;
	      ST_ATTR&	st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
      	      ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                    	    Save_Str (is_ctors ? ".ctors" : ".dtors"));
	    }
	    // There's no use inlining A into B (ref bug 14180 comment above)
	    Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
	    Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
	    Set_ST_addr_saved (func_st);
	  }
#ifdef KEY
	  else if (is_attribute("no_instrument_function", attr)) {
	    Set_PU_no_instrument (Pu_Table [ST_pu (func_st)]);  // Bug 750
	  }
	  else if (is_attribute("used", attr))
	    Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);  // bug 3697
#endif
          else if (is_attribute("malloc", attr)) {
            Set_PU_has_attr_malloc (Pu_Table [ST_pu (func_st)]);
          }
	}
      } 
    }
 
    if (Show_Progress) {
      fprintf (stderr, "Compiling %s(%d)\n", ST_name (func_st), WGEN_PU_count);
      fflush (stderr);
    }

    Scope_tab [Current_scope].st = func_st;

#ifdef KEY
    // bug 8346:
    // This should be set after setting the ST in Scope_tab above. The reason
    // is any ST generated before the above statement will be regenerated,
    // and that is because set_DECL_ST() does not work until the above
    // Scope_tab ST is set. This specifies that it is now OK to fully
    // expand any VLA parameter types.
    expanding_function_definition = TRUE;
#endif

// Insert special variables into the local symtab, store their id's
// in the PU_TAB, to be accessed later in the front-end, WN Lowerer,
// inliner/ipa, and back-end.
    if (emit_exceptions)
       Setup_Entry_For_EH ();
    else
       Dummy_Exc_Ptr_Expr = NULL;

    if (func_PU_uplevel(fndecl))
      Set_PU_uplevel (Get_Current_PU ());

    INT num_args = 0;
    gs_t pdecl;
    // Needed for GCC 3.2.  See comment in WGEN_get_thunk_target.
    pdecl = thunk ? gs_decl_arguments (WGEN_get_final_thunk_target (fndecl))
                  : gs_decl_arguments (fndecl);
    for (;
         pdecl;
         pdecl = gs_tree_chain (pdecl)) {
      TY_IDX arg_ty_idx = Get_TY(gs_tree_type(pdecl));
      if (!WGEN_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else
	++num_args;
    }

    // Add the fake first param if the function needs to return an object in
    // memory.  Here we only handle the types that the front-end says must
    // return in memory.
    TY_IDX ret_ty_idx = Get_TY(gs_tree_type(gs_tree_type(fndecl)));
    if (TY_return_in_mem(ret_ty_idx)) {
      num_args++;
    }
 
    WN *body, *wn;
    body = WN_CreateBlock ( );
    entry_wn = WN_CreateEntry (num_args, func_st, body, NULL, NULL );
    /* from 1..nkids=num_args, create IDNAME args for OPR_FUNC_ENTRY */
    INT i = 0;
 
    // Create the fake first param.
    if (TY_return_in_mem(ret_ty_idx)) {
      ST *st = New_ST ();
      ST_Init (st, Save_Str2i(".arg", "", i), CLASS_VAR, SCLASS_FORMAL,
	       EXPORT_LOCAL, Make_Pointer_Type(ret_ty_idx, FALSE));
      Set_ST_is_value_parm(st);
      WN_kid(entry_wn,i) = WN_CreateIdname ( 0, ST_st_idx(st) );
      ++i;
    }

    for (pdecl = thunk ?
		    gs_decl_arguments (WGEN_get_final_thunk_target (fndecl))
                       : gs_decl_arguments (fndecl);
         pdecl;
         pdecl = gs_tree_chain (pdecl) )
    {
      TY_IDX arg_ty_idx = Get_TY(gs_tree_type(pdecl));
      ST *st;
      if (thunk) {
        st = New_ST ();
        ST_Init (st, Save_Str2i(".arg", "", i), CLASS_VAR,
		 SCLASS_FORMAL, EXPORT_LOCAL, arg_ty_idx);
      }
      else {
#ifdef KEY
	gs_t passed_type = gs_decl_arg_type (pdecl);
	gs_t nominal_type = gs_tree_type (pdecl);
	// See if the front-end wants to pass this by invisible reference.
	if (passed_type != nominal_type &&
	    gs_pointer_type_p (passed_type) &&
	    gs_tree_type (passed_type) == nominal_type) {
	  // The front-end passes the parm by invisible reference.  The parm is
	  // a reference to the data object instead of the object itself.

	  gs_t ptr_parm = gs_build_decl(GS_PARM_DECL, passed_type);
	  gs_set_decl_arg_type(ptr_parm, passed_type);
	  st = Get_ST(ptr_parm);

	  // We are done with the parm decl.  Change it to an indirect
	  // reference node so that the rest of the WHIRL translator will see
	  // the dereferenced value whenever it references the node.
	  _gs_code(pdecl, GS_INDIRECT_REF);
	  gs_set_tree_operand(pdecl, 0, ptr_parm);
	} else
#endif
	st = Get_ST(pdecl);
      }

      if (!WGEN_Keep_Zero_Length_Structs   &&
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
#ifdef KEY // wgen
    if (! thunk)
      decl_arguments = gs_decl_arguments (fndecl);

    // bug 8346: if ty is incomplete, complete it now.
    if (TY_is_incomplete(PU_prototype(Pu_Table[ST_pu(func_st)])))
      Set_PU_prototype (Pu_Table[ST_pu(func_st)], Get_TY(gs_tree_type(fndecl)));

#endif

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

    if (strcmp (ST_name (func_st), "main") == 0) {
      PU& pu = Pu_Table[ST_pu (St_Table [PU_Info_proc_sym (pu_info)])];
      Set_PU_is_mainpu (pu);
      Set_PU_no_inline (pu);
    }     

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;
    else if (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1)
      PU_Tree_Root = pu_info;
    else
      PU_Info_child (PU_Info_Table [CURRENT_SYMTAB -1]) = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;
 
    WGEN_Stmt_Pop (wgen_stmk_func_body);

    WGEN_Stmt_Push (entry_wn, wgen_stmk_func_entry, Get_Srcpos());
    WGEN_Stmt_Push (body, wgen_stmk_func_body, Get_Srcpos());

    wn = WN_CreatePragma (WN_PRAGMA_PREAMBLE_END, (ST_IDX) NULL, 0, 0);
    WGEN_Stmt_Append (wn, Get_Srcpos());
    WGEN_Stmt_Append (vla_block, Get_Srcpos());

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
      if (TY_return_in_mem(Get_TY(gs_tree_type(gs_tree_type(fndecl))))) {
	Set_TYLIST_type (New_TYLIST (tylist_idx), Be_Type_Tbl(MTYPE_V));
      } else
#endif
      Set_TYLIST_type (New_TYLIST (tylist_idx),
                       Get_TY(gs_tree_type(gs_tree_type(fndecl))));
      Set_TY_tylist (ty, tylist_idx);
      for (pdecl = gs_decl_arguments(fndecl); pdecl; pdecl = gs_tree_chain (pdecl) ) {
#ifdef KEY
	// If parm was passed through invisible reference, then we would have
	// changed the parm to be an indirect reference of the parm.
	ST *arg_st = gs_tree_code(pdecl) == GS_INDIRECT_REF ?
		       Get_ST(gs_tree_operand(pdecl, 0)) : Get_ST(pdecl);
#else
	ST *arg_st = Get_ST(pdecl);
#endif
        Set_TYLIST_type (New_TYLIST (tylist_idx), ST_type(arg_st));
      }
      Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
      Set_TY_is_varargs (ty_idx);
      Set_TY_has_prototype (ty_idx);
      Set_PU_prototype (pu, ty_idx);
	  
      gs_t fntype = gs_tree_type(fndecl);
      if (gs_tree_code(fntype) == GS_METHOD_TYPE) {
          TY_IDX base = Get_TY(gs_type_method_basetype(fntype));
          Set_PU_base_class(pu, base);
      }
    }

    if (!thunk && gs_decl_global_ctor_p(fndecl)) {
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

    if (!thunk && gs_decl_global_dtor_p(fndecl)) {
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
    }

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
    if (!gs_decl_one_only(fndecl)) {
      if (ST_is_weak_symbol(func_st) &&
	  WEAK_WORKAROUND(func_st) == WEAK_WORKAROUND_made_weak) {
	Clear_ST_is_weak_symbol(func_st);
      }
    }

#ifdef KEY
    expanding_function_definition = FALSE; // bug 8346
#endif
    return entry_wn;
}

static void
WGEN_Finish_Function (gs_t fndecl)
{
    // This is not defined yet
    //WGEN_Check_Undefined_Labels ();
    PU_Info *pu_info = PU_Info_Table [CURRENT_SYMTAB];

    if (PU_lexical_level (Get_Current_PU()) > 2) {

      DevWarn ("Encountered nested function");
      Set_PU_is_nested_func (Get_Current_PU ());
#ifdef KEY
      PU &parent_pu = Get_Scope_PU(PU_lexical_level(Get_Current_PU()) - 1);
      Set_PU_uplevel(parent_pu);
      // insert in the nested linked list of nested functions in the parent's PU
      INITV_IDX nested_fn_iv = New_INITV();
      INITV_Set_SYMOFF(Initv_Table[nested_fn_iv], 1, ST_st_idx(DECL_ST(fndecl)),
		       0);
      INITO_IDX parent_inito;
      if (PU_misc_info(parent_pu) == INITO_IDX_ZERO) {
	ST *st = New_ST(PU_lexical_level(parent_pu));
	ST_Init (st, Save_Str (".nested_functions"),
		 CLASS_PREG, SCLASS_REG, EXPORT_LOCAL, MTYPE_To_TY(MTYPE_I4));
	parent_inito = New_INITO(ST_st_idx(st), nested_fn_iv);
	Set_PU_misc_info(parent_pu, parent_inito);
      }
      else {
	parent_inito = PU_misc_info(parent_pu);
	Set_INITV_next(nested_fn_iv, INITO_val(parent_inito));
	Set_INITO_val(parent_inito, nested_fn_iv);
      }
#endif
    }
    if (opt_regions)
    {
    	Check_For_Call_Region ();
	// Since we are finishing a function, we must have terminated all
	// regions. So reset the flag.
	Did_Not_Terminate_Region = FALSE;
    }

    // Insert a RETURN if it does not exist
    WN * wn = WN_last (WGEN_Stmt_Top ());
    if (wn == NULL || WN_operator (wn) != OPR_RETURN &&
		      WN_operator (wn) != OPR_RETURN_VAL)
      WGEN_Stmt_Append (WN_CreateReturn (), Get_Srcpos ());

#if defined (KEY) && defined(TARG_IA64)
    if (PU_has_syscall_linkage (Get_Current_PU ())) {
      Set_PU_no_inline (Get_Current_PU ());
    }
#endif

    if (lang_cplus) {
      // Add any handler code
      Do_Handlers ();
      if (emit_exceptions)
	Do_EH_Tables ();
    }

    // write out all the PU information
    WGEN_Stmt_Pop (wgen_stmk_func_body);
    WGEN_Stmt_Pop (wgen_stmk_func_entry);

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
      Return_Address_ST [CURRENT_SYMTAB] = NULL;
    }

#ifdef KEY
    // Bug 14417: Wgen may have set the PU no_inline, so we must reset
    // must_inline.
    if (PU_no_inline (Get_Current_PU()))
      Clear_PU_must_inline (Get_Current_PU());
#endif

    try_block_seen = false;

    // dump_tree(Current_Entry_WN());
    // Restore the previous entry wn, if any.
    Pop_Current_Entry_WN();

    Delete_Scope (CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
 // if (CURRENT_SYMTAB > GLOBAL_SYMTAB)
 //   Current_pu = &Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];
} // end WGEN_Finish_Function

class AGGINIT {
  INITO_IDX _inito;	   // the inito to construct
  INITV_IDX _last_initv;   // point to last of the list of initv's being built
  BOOL _not_root;	   // FALSE if at the first of the initialization values
  			   // 	for this inito
private:
  void WGEN_Add_Aggregate_Init_Integer (INT64 val, INT size);
  void WGEN_Add_Init_Block(void);
  void WGEN_Add_Aggregate_Init_Real (gs_t real, INT size);
  void WGEN_Add_Aggregate_Init_Complex (gs_t rval, gs_t ival, INT size);
  void WGEN_Add_Aggregate_Init_String (char *s, INT size);
  void WGEN_Add_Aggregate_Init_Symbol (ST *st, WN_OFFSET offset = 0);
#ifdef TARG_IA64
  void Add_Aggregate_Init_Symiplt (ST *st, WN_OFFSET offset = 0);
#endif
  void WGEN_Add_Aggregate_Init_Label (LABEL_IDX lab, INT32 flag = INITVLABELFLAGS_UNUSED, mTYPE_ID mtype = MTYPE_UNKNOWN);
  void WGEN_Add_Aggregate_Init_Address (gs_t init);
  void WGEN_Add_Aggregate_Init_Vector (gs_t init_list);
  // For label values, '.L1 - .L2' 
  void Add_Init_For_Label_Values(WN *init_wn, UINT size, BOOL first_child = TRUE, BOOL last_child = TRUE); 
  BOOL WN_Tree_Is_Label_Values(WN* init_wn);   // Is the wn tree label values
  void Add_Init_For_WHIRL(WN *init_wn, UINT size, INT64 ofst);
  void Add_Initv_For_Tree (gs_t val, UINT size);
  void Add_Bitfield_Initv_For_Tree (gs_t val, FLD_HANDLE fld, INT &bytes);
  void Traverse_Aggregate_Pad ( 
#ifdef NEW_INITIALIZER
        WN     *target,
#else
        ST     *st,
#endif
	BOOL   gen_initv,
	UINT   pad,
	UINT   current_offset);
void Traverse_Aggregate_Array (
#ifdef NEW_INITIALIZER
        WN   *target,
#else
	ST   *st,            // symbol being initialized
#endif
	gs_t init_list,      // list of initializers for each array element
	gs_t type,           // type of array
	BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
	UINT current_offset); // offset from start of symbol for current array
  UINT Traverse_Aggregate_Struct (
#ifdef NEW_INITIALIZER
        WN   *target,
#else
	ST   *st,               // symbol being initialized
#endif
	gs_t init_list,         // list of initializers for elements in STRUCT
#ifdef KEY
	gs_t struct_type,       // type of top level structure
#endif
	gs_t type,              // type of struct
	BOOL gen_initv,         // TRUE if initializing with INITV, FALSE for statements
	UINT current_offset,    // offset from start of symbol for current struct
	UINT array_elem_offset, // if struct is with an array, then it is the
				//   offset of the outermost struct from the
				//   array enclosing the struct
				// if struct is not within an array, it is zero
				// this is needed when field_id is used to generate
				//   stores for initialization
	UINT field_id);         // field_id of struct

public:
  AGGINIT(void) { _inito = 0; _last_initv = 0; _not_root = FALSE; }
  AGGINIT(INITO_IDX inito): _inito(inito) { _last_initv = 0; _not_root = FALSE;}
  ~AGGINIT(void) {}

  INITO_IDX Inito(void) const { return _inito; }
  void Set_inito(INITO_IDX inito) { _inito = inito; }
  INITV_IDX Last_initv(void) const { return _last_initv; }

  void WGEN_Add_Aggregate_Init_Padding (INT size);
  void Traverse_Aggregate_Vector (
#ifdef NEW_INITIALIZER
        WN * target,
#else
	ST * st,              // symbol being initialized
#endif
	gs_t init_list,       // list of initializers for units in vector
	TYPE_ID mtyp,         // type of vector
	BOOL gen_initv,       // TRUE if initializing with INITV, FALSE for statements
	UINT current_offset,  // offset from start of symbol for current vector
	BOOL vec_cst = FALSE);// init_list is a constant or not
  UINT Traverse_Aggregate_Constructor (
#ifdef NEW_INITIALIZER
        WN   *target,
#else
	ST   *st,               // symbol being initialized
#endif
	gs_t init_list,         // list of initilaizers for this aggregate
#ifdef KEY
	gs_t struct_type,	  // type of top level struct
#endif
	gs_t type,              // type of aggregate being initialized
	BOOL gen_initv,         // TRUE  if initializing with INITV,
				// FALSE if initializing with statements
	UINT current_offset,    // offset from start of symbol for this aggregate
	UINT array_elem_offset,
	UINT field_id);
  void Add_Inito_For_Tree (gs_t init, ST *st);
};


void
AGGINIT::WGEN_Add_Aggregate_Init_Padding (INT size)
{
  if (_inito == 0) return;
  if (size < 0) return;	// actually happens from assemble_zeroes
  INITV_IDX inv = New_INITV();
  INITV_Init_Pad (inv, size);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
}

void
AGGINIT::WGEN_Add_Aggregate_Init_Integer (INT64 val, INT size)
{
  if (_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TYPE_ID mtype;
  if (size == 1) mtype = MTYPE_I1;
  else if (size == 2) mtype = MTYPE_I2;
  else if (size == 4) mtype = MTYPE_I4;
  else if (size == 8) mtype = MTYPE_I8;
  else FmtAssert(FALSE, ("WGEN_Add_Aggregate_Init_Integer unexpected size"));
  INITV_Init_Integer (inv, mtype, val);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
}

void
AGGINIT::WGEN_Add_Init_Block(void)
{
  if (_inito == 0) return;
  INITV_IDX inv_blk = New_INITV();
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv_blk);
  else if (! _not_root)
    Set_INITO_val(_inito, inv_blk);
  _last_initv = inv_blk;
}

		// kgccfe uses WGEN_Add_Aggregrate_Init_Real instead of
		// WGEN_Add_Aggregate_Init_Double.  Use the former because it is
		// newer and can handle REAL_VALUE_TYPE, which is needed for
		// i386.
	
void 
AGGINIT::WGEN_Add_Aggregate_Init_Real (gs_t real, INT size)
{
  if (_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    tc;
  double  buffer;
  union {
    INT32	qval[4];
    long double ld;
  } ldbuffer;
  switch (size) {
    case 4:
      tc = Host_To_Targ_Float_4 (MTYPE_F4, gs_tree_real_cst_f(real));
      break;
    case 8:
      buffer = gs_tree_real_cst_d(real);
      // Strange, why need convert the order?
      WGEN_Convert_To_Host_Order((long *)&buffer);
      tc = Host_To_Targ_Float (MTYPE_F8, buffer);
      break;
#if defined(TARG_IA64) || defined(TARG_X8664)
    case 12:
    case 16:
      // TODO handle MTYPE_F16
      tc = Host_To_Targ_Float_10(MTYPE_F10, gs_tree_real_cst_ld(real));
      break;
#else
    case 12:
    case 16:
      ldbuffer.qval[2] = 0;
      ldbuffer.qval[3] = 0;
      ldbuffer.ld = gs_tree_real_cst_ld(real);
      tc = Host_To_Targ_Quad (ldbuffer.ld);
      break;
#endif
    default:
      FmtAssert(FALSE, ("WGEN_Add_Aggregate_Init_Real unexpected size"));
      break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), 1);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
} /* WGE_Add_Aggregate_Init_Real */

          	// Use the WGEN_Add_Aggregrate_Init_Complex from kgccfe, because
		// it is newer and can handle REAL_VALUE_TYPE, which is needed
		// for i386.
void 
AGGINIT::WGEN_Add_Aggregate_Init_Complex (gs_t rval, gs_t ival, INT size)
{
  if (_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    rtc;
  TCON    itc;
  int     t1;
  double  buffer;
  union {
    INT32	qval[4];
    long double ld;
  } ldbuffer;
  switch (size) {
    case 8:
      rtc = Host_To_Targ_Float_4 (MTYPE_F4, gs_tree_real_cst_f(rval));
      itc = Host_To_Targ_Float_4 (MTYPE_F4, gs_tree_real_cst_f(ival));
      break;
    case 16:
      buffer = gs_tree_real_cst_d(rval);
      WGEN_Convert_To_Host_Order((long *)&buffer);
      rtc = Host_To_Targ_Float (MTYPE_F8, buffer);

      buffer = gs_tree_real_cst_d(ival);
      WGEN_Convert_To_Host_Order((long *)&buffer);
      itc = Host_To_Targ_Float (MTYPE_F8, buffer);
      break;
#if defined(TARG_IA64) || defined(TARG_X8664)
    case 24:
    case 32:
      // TODO handle MTYPE_F16
      rtc = Host_To_Targ_Float_10(MTYPE_F10, gs_tree_real_cst_ld(rval));
      itc = Host_To_Targ_Float_10(MTYPE_F10, gs_tree_real_cst_ld(ival));
      break;
#else      
    case 24:
    case 32:
      ldbuffer.qval[2] = 0;
      ldbuffer.qval[3] = 0;
      ldbuffer.ld = gs_tree_real_cst_ld(rval);
      rtc = Host_To_Targ_Quad(ldbuffer.ld);

      ldbuffer.ld = gs_tree_real_cst_ld(ival);
      itc = Host_To_Targ_Quad(ldbuffer.ld);
      break;
#endif
    default:
      FmtAssert(FALSE, ("WGEN_Add_Aggregate_Init_Complex unexpected size"));
      break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(rtc), 1);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
  inv = New_INITV();
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(itc), 1);
  Set_INITV_next(_last_initv, inv);
  _last_initv = inv;
}


void 
AGGINIT::WGEN_Add_Aggregate_Init_String (char *s, INT size)
{
  if (_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_String (inv, s, size);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
}

void
AGGINIT::WGEN_Add_Aggregate_Init_Symbol (ST *st, WN_OFFSET offset)
{
  if (_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Symoff (inv, st, offset);
  if (st)
    Set_ST_addr_saved (st);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
}

#ifdef TARG_IA64
void
AGGINIT::Add_Aggregate_Init_Symiplt (ST *st, WN_OFFSET offset)
{
  if (_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Symiplt (inv, st, offset);
  Set_ST_addr_saved (st);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
}
#endif


void
AGGINIT::WGEN_Add_Aggregate_Init_Label (LABEL_IDX lab, INT16 flag, mTYPE_ID mtype)
{
  DevWarn ("taking address of a label at line %d", lineno);
  Set_PU_no_inline (Get_Current_PU ());
  if (_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Label (inv, lab, 1, flag, mtype);
  if (_last_initv != 0)
    Set_INITV_next(_last_initv, inv);
  else if (! _not_root)
    Set_INITO_val(_inito, inv);
  _last_initv = inv;
  Set_LABEL_addr_saved (lab);
}

void
AGGINIT::WGEN_Add_Aggregate_Init_Address (gs_t init)
{
  ST *st;
  switch (gs_tree_code (init)) {

  case GS_VAR_DECL:
  case GS_FUNCTION_DECL:
	{
	st = Get_ST (init);
	WGEN_Add_Aggregate_Init_Symbol (st);
#ifdef KEY // bug 11308
	if (gs_tree_code (init) == GS_VAR_DECL)
	  Set_ST_initv_in_other_st (st);
#endif
	}
	break;
  case GS_STRING_CST:
	{
	TCON tcon = Host_To_Targ_String (MTYPE_STRING,
			const_cast<char*>(gs_tree_string_pointer(init)),
			gs_tree_string_length(init));
	ST *const_st = New_Const_Sym (Enter_tcon (tcon), 
		Get_TY(gs_tree_type(init)));
      	WGEN_Add_Aggregate_Init_Symbol (const_st);
	}
    	break;
  case GS_PLUS_EXPR:
	if ( gs_tree_code(gs_tree_operand(init,0)) == GS_ADDR_EXPR
	  && gs_tree_code(gs_tree_operand(init,1)) == GS_INTEGER_CST)
	{
		gs_t addr_kid = gs_tree_operand(gs_tree_operand(init,0),0);
		gs_code_t addr_kid_code = gs_tree_code(addr_kid);
		FmtAssert(addr_kid_code == GS_VAR_DECL
			|| addr_kid_code == GS_FUNCTION_DECL,
			("expected decl under plus_expr"));
		WGEN_Add_Aggregate_Init_Symbol ( Get_ST (addr_kid),
			gs_get_integer_value(gs_tree_operand(init,1)) );
	}
	else
	{
		WN *init_wn = WGEN_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected decl under plus_expr"));
		WGEN_Add_Aggregate_Init_Symbol (WN_st (init_wn),
					       WN_offset (init_wn));
// bugs 555, 11308
#ifdef KEY
		Set_ST_initv_in_other_st (WN_st(init_wn));
#endif
		WN_Delete (init_wn);
	}
	break;

  case GS_INTEGER_CST:
	WGEN_Add_Aggregate_Init_Integer (gs_get_integer_value (init), Pointer_Size);
	break;

  case GS_LABEL_DECL:
	{
	 	LABEL_IDX label_idx = WGEN_Get_LABEL (init, FALSE);
		WGEN_Add_Aggregate_Init_Label (label_idx);
	}
	break;

  case GS_COMPOUND_LITERAL_EXPR:
  	{
	init = gs_decl_initial(gs_tree_operand(gs_tree_operand(init, 0), 0));
	WGEN_Add_Aggregate_Init_Symbol (NULL); // bug 10507: claim this 
		// INITV_IDX because there may be a BLOCK INITV that has been
		// set to point to this INITV_IDX as the first in the block
		// (see Traverse_Aggregate_Constructor())
	INITV_IDX saved_last_initv = _last_initv;
	st = WGEN_Generate_Temp_For_Initialized_Aggregate(init, "");
	INITV_Init_Symoff (saved_last_initv, st, 0);
	Set_ST_addr_saved (st);
  	}
	break;

  default:
	{
                WN *init_wn = WGEN_Expand_Expr (init);
		WN *wn = init_wn;
		INT iload_ofst = 0;
		if (WN_operator(wn) == OPR_ILOAD) {
		  iload_ofst = WN_offset(wn);
		  wn = WN_kid0(wn);
		}
		// bug fix for OSP_128
		//
		if (WN_operator(wn) == OPR_INTCONST) 
		  WGEN_Add_Aggregate_Init_Integer(WN_const_val(wn) + iload_ofst, Pointer_Size);
		else if (WN_operator(wn) != OPR_ARRAY)
		  WGEN_Add_Aggregate_Init_Symbol (WN_st(wn), WN_offset(wn)+iload_ofst);
		else {
		  BOOL const_indices = TRUE;
		  INT total_displace = 0;
		  // bug 13346 handle nested ARRAY nodes
		  do {
		    INT displace = 0;
		    INT multiplier = 1;
		    for (INT i = 0; i < WN_num_dim(wn); i++) {
		      if (WN_operator(WN_array_index(wn, i)) != OPR_INTCONST ||
			  WN_operator(WN_array_dim(wn, i)) != OPR_INTCONST) {
			const_indices = FALSE;
			break;
		      }
		      displace += WN_const_val(WN_array_index(wn, i)) * multiplier;
		      multiplier *= WN_const_val(WN_array_dim(wn, i));
		    }
		    displace *= WN_element_size(wn);
		    total_displace += displace;
		    wn = WN_array_base(wn);
		  } while (WN_operator(wn) == OPR_ARRAY);

		  FmtAssert(WN_operator(wn) == OPR_LDA,
		  	    ("unhandled array references in initialization"));
		  if (const_indices) {
		    WGEN_Add_Aggregate_Init_Symbol (WN_st(wn),
			  WN_offset(wn) + total_displace + iload_ofst);
		  }
		  else FmtAssert(FALSE, ("cannot resolve to constant address in array"));
		}
		WN_Delete (init_wn);
	}
      	break;
  }
} /* WGEN_Add_Aggregate_Init_Address */

#ifdef KEY // bug 10501
void
AGGINIT::WGEN_Add_Aggregate_Init_Vector (gs_t init_list)
{
  gs_t init;
  UINT vec_size;
  INT i;

  gs_t type_size = gs_type_size(gs_tree_type(init_list));
  Is_True (gs_tree_code (type_size) == GS_INTEGER_CST,
           ("WGEN_Add_Aggregate_Init_Vector: Vector of variable-sized units?"));
  vec_size = gs_get_integer_value (type_size) / BITSPERBYTE;

  init = gs_tree_vector_cst_elts (init_list);

  // find the element size from vector mtype
  TY_IDX vector_type = Get_TY(gs_tree_type(init_list));
  Is_True (MTYPE_is_vector(TY_mtype(vector_type)), 
            ("WGEN_Add_Aggregate_Init_Vector: invalid vector type"));
  TYPE_ID elem_mtype = Mtype_vector_elemtype(TY_mtype(vector_type));
  UINT esize = MTYPE_byte_size(elem_mtype);
  UINT nunits = vec_size / esize;
  gs_code_t code = MTYPE_is_integral(elem_mtype) ? GS_INTEGER_CST : GS_REAL_CST;

  for (i = 0;
       init;
       init = gs_tree_chain(init), i++)
  {
    gs_t value = gs_tree_value(init);
#ifdef Is_True_On
    gs_t unit_type = gs_tree_type(value);
    Is_True (gs_tree_code (gs_type_size (unit_type)) == GS_INTEGER_CST,
             ("WGEN_Add_Aggregate_Init_Vector: Vector of variable-sized units?"));
    Is_True (esize == gs_get_integer_value(gs_type_size(unit_type))/BITSPERBYTE,
             ("WGEN_Add_Aggregate_Init_Vector: variable-sized elements"));
    Is_True (code == gs_tree_code(value),
             ("WGEN_Add_Aggregate_Init_Vector: vector has different type elements"));
#endif

    switch (code)
    {
      case GS_INTEGER_CST:
        WGEN_Add_Aggregate_Init_Integer (gs_get_integer_value(value), esize);
        break;
      case GS_REAL_CST:
        WGEN_Add_Aggregate_Init_Real (value, esize);
        break;
      default:
        Fail_FmtAssertion("WGEN_Add_Aggregate_Init_Vector: unexpected value of vector element");
    }
  }

  // Fill out remaining vector elements with zero.
  for (; i < nunits; i++)
  {
    switch (code)
    {
      case GS_INTEGER_CST:
        WGEN_Add_Aggregate_Init_Integer (0, esize);
        break;
      case GS_REAL_CST: // mimick WGEN_Add_Aggregate_Init_Real()
      {
        Is_True (_inito != 0, ("Null inito"));
        INITV_IDX inv = New_INITV();
        TCON    tc;
        double  buffer;
        switch (esize)
        {
          case 4:
            tc = Host_To_Targ_Float_4 (MTYPE_F4, 0.0);
            break;
          case 8:
            buffer = 0.0;
            WGEN_Convert_To_Host_Order((long *)&buffer);
            tc = Host_To_Targ_Float (MTYPE_F8, buffer);
            break;
          default: // size 12 and 16 should not arise
            Fail_FmtAssertion("WGEN_Add_Aggregate_Init_Vector unexpected size %d", esize);
            break;
        }
        INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), 1);
        if (_last_initv != 0)
          Set_INITV_next(_last_initv, inv);
        else if (! _not_root)
          Set_INITO_val(_inito, inv);
        _last_initv = inv;
      }
      break;
      default:
        Fail_FmtAssertion("WGEN_Add_Aggregate_Init_Vector: unexpected value of vector element");
    }
  }
} /* WGEN_Add_Aggregate_Init_Vector */
#endif


static BOOL
Has_Non_Constant_Init_Value (gs_t init)
{
  if (init == NULL) {
	return FALSE;
  }
  switch (gs_tree_code(init)) {
  case GS_CONSTRUCTOR:
#ifdef FE_GNU_4_2_0
      {
	INT count = gs_constructor_length(init);
	if (!count)
	    return TRUE;
        for (INT i = 0; i < count; i++)
	  if (Has_Non_Constant_Init_Value (gs_constructor_elts_value(init, i)))
	    return TRUE;
        return FALSE;
      }
#else
	if (!gs_constructor_elts(init))
	    return TRUE;
	return Has_Non_Constant_Init_Value (gs_constructor_elts(init));
#endif
  case GS_TREE_LIST:
	{
	gs_t p;
	for (p = init; p != NULL; p = gs_tree_chain(p)) {
		if (Has_Non_Constant_Init_Value (gs_tree_value(p))) {
			return TRUE;
		}
/*
		if (TREE_PURPOSE(p) != NULL_TREE 	     &&
		    gs_tree_code(TREE_PURPOSE(p)) == GS_FIELD_DECL &&
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
  case GS_INTEGER_CST:
  case GS_REAL_CST:
  case GS_STRING_CST:
	return FALSE;
  case GS_NOP_EXPR:
	if (gs_tree_code(gs_tree_operand(init,0)) == GS_ADDR_EXPR
    	    && gs_tree_code(gs_tree_operand(gs_tree_operand(init,0),0)) == GS_STRING_CST) 
		return FALSE;
	else
		return TRUE;
  case GS_ADDR_EXPR: {
	gs_t t = gs_tree_operand(init, 0);
#ifdef KEY // bug 11726
	if (gs_tree_code_class(t) != GS_TCC_DECLARATION)
	  return TRUE;
#endif
	if (gs_decl_context(t) == 0 ||
	    gs_tree_code(gs_decl_context(t)) == GS_NAMESPACE_DECL)
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
Use_Static_Init_For_Aggregate (ST *st, gs_t init)
{
#ifdef KEY // bug 10507
	if (CURRENT_SYMTAB == GLOBAL_SYMTAB)
	  return TRUE;
#endif
	return !Has_Non_Constant_Init_Value(init);
}


void
AGGINIT::Add_Initv_For_Tree (gs_t val, UINT size)
{
	switch (gs_tree_code(val)) {
	case GS_INTEGER_CST:
		WGEN_Add_Aggregate_Init_Integer (
			gs_get_integer_value(val), size);
		break;
	case GS_REAL_CST:
		WGEN_Add_Aggregate_Init_Real (val, size);
		break;
	case GS_STRING_CST:
#ifdef KEY
		// bug 14492: SIZE may be larger than the string length,
		// in which case the remaining should be filled with
		// null/pad bytes, so that they don't get undefined
		// values.
		WGEN_Add_Aggregate_Init_String (
		         const_cast<char*>(gs_tree_string_pointer(val)),
		         size < gs_tree_string_length(val) ?
		              size :
		              gs_tree_string_length(val));
		if (size > gs_tree_string_length(val))
		  WGEN_Add_Aggregate_Init_Padding (size -
						 gs_tree_string_length(val));
#else
		WGEN_Add_Aggregate_Init_String (
			const_cast<char*>(gs_tree_string_pointer(val)), size);
#endif
		break;
#ifdef KEY // bug 11576
	case GS_VECTOR_CST:
		WGEN_Add_Aggregate_Init_Vector(val);
		break;
#endif
	case GS_ADDR_EXPR:
		WGEN_Add_Aggregate_Init_Address (gs_tree_operand(val, 0));
		break;
	case GS_NOP_EXPR:
		gs_t kid;
		kid = gs_tree_operand(val,0);
		// [SC] NOP_EXPR does not change representation, so just recurse on kid.
		Add_Initv_For_Tree (kid, size);
		break;
	default:
		{
        WN *init_wn;
        init_wn = WGEN_Expand_Expr (val);
#ifdef TARG_IA64
		if (gs_tree_code(val) == GS_FDESC_EXPR && WN_operator (init_wn) == OPR_LDA) {
		  Add_Aggregate_Init_Symiplt (WN_st (init_wn),
                  	                      WN_offset (init_wn));
		  WN_DELETE_Tree (init_wn);
		  break;
		}
#endif
		if (WN_operator (init_wn) == OPR_LDA_LABEL) {
		  gs_t label_decl = 
		    (gs_tree_code(gs_tree_operand(val, 0)) == GS_ADDR_EXPR)?
		    gs_tree_operand (gs_tree_operand (val, 0), 0):
		    gs_tree_operand (val, 0);
		  LABEL_IDX label_idx = 
		    WGEN_Get_LABEL (label_decl, FALSE);
		  WGEN_Add_Aggregate_Init_Label (label_idx);
		}
		else Add_Init_For_WHIRL(init_wn, size, 0);
		WN_DELETE_Tree (init_wn);
		break;
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
void
AGGINIT::Add_Bitfield_Initv_For_Tree (gs_t val, FLD_HANDLE fld, INT &bytes)
{
  FmtAssert(gs_tree_code(val) == GS_INTEGER_CST,
	    ("initialization value of bitfield expected to be integer, not %d",
	     gs_tree_code(val)));
  INT bofst = FLD_bofst(fld);
  INT bsize = FLD_bsize(fld);
  if (bsize == 0)
    return;

  INITBUF ib(gs_get_integer_value(val));
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
    if (ib.Nth_byte(bytes_out-1) != 0) {// merge and change _last_initv
      if (INITV_kind(_last_initv) == INITVKIND_VAL) {
        TCON &tc = INITV_tc_val(_last_initv);
        mUINT8 last_ival = TCON_k0(tc);
        tc.vals.k0 = last_ival | ib.Nth_byte(bytes_out-1);
      }
      else { // need to create a new TCON
        if (INITV_kind(_last_initv) == INITVKIND_ONE) 
	  INITV_Init_Integer(_last_initv, MTYPE_I1, 
			     1 | ib.Nth_byte(bytes_out-1));
	else {
	  FmtAssert(INITV_kind(_last_initv) == INITVKIND_ZERO,
		    ("processing error in static bit field initialization"));
	  INITV_Init_Integer(_last_initv, MTYPE_I1, 
			     ib.Nth_byte(bytes_out-1));
	}
      }
    }
  }
  // output the remaining bytes
  for (i = bytes_out; i < num_of_bytes; i++)
    WGEN_Add_Aggregate_Init_Integer(ib.Nth_byte(i), 1);
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
gs_t init, UINT offset, UINT array_elem_offset,
	TY_IDX ty, BOOL is_bit_field, UINT field_id, FLD_HANDLE fld, INT &bytes)
{
#ifdef KEY
    // If the initializer is a call expr and the type must be returned in
    // memory, then tell the call expr to put the result directly into ST.
    if (TY_return_in_mem(ty) &&
	gs_tree_code(init) == GS_CALL_EXPR) {
#ifndef NEW_INITIALIZER
      WN *target = WN_Lda (Pointer_Mtype, 0, st, 0);
      bytes += TY_size(ty);
#endif
      WGEN_Expand_Expr (init, TRUE, 0, 0, 0, 0, FALSE, FALSE, target);
      return;
    }
#endif

#ifdef NEW_INITIALIZER
    if (TY_return_in_mem(ty) &&
        gs_tree_code(init) == GS_TARGET_EXPR) {
      // We can not pass the offset to WGEN_Expand_Expr,
      //  because it's not handled in that function, so we make an add here
      Is_True ((WN_operator(target) == OPR_LDID ||
                WN_operator(target) == OPR_LDA),
               ("Bad operator for target") );
      TY_IDX targ_ty = WN_ty(target);
      ST* addr_st = Gen_Temp_Symbol (TY_mtype(targ_ty), "target");
      WN* wn = NULL;
      if (WN_offset(target) != 0 || offset != 0) {
          DevWarn("Pointer Arithmetic here may introduce bugs!");
          wn = WN_Stid (TY_mtype(targ_ty), 0, addr_st, targ_ty,
                        WN_Binary (OPR_ADD, Pointer_Mtype, 
			           WN_CopyNode(target),
                                   WN_Intconst(MTYPE_I4, offset + WN_offset(target) ) ) );
      }
      else {
          wn = WN_Stid (TY_mtype(targ_ty), 0, addr_st, targ_ty,
                        WN_CopyNode(target) );
      }
      WGEN_Stmt_Append (wn, Get_Srcpos());
      target = WN_Ldid (TY_mtype(targ_ty), 0, addr_st, targ_ty);
      WGEN_Expand_Expr (init, TRUE, 0, 0, 0, 0, FALSE, FALSE, target);
      bytes += TY_size(ty);
      return;
    }
#endif

    WN *init_wn = WGEN_Expand_Expr (init);

    if (gs_tree_code(init) == GS_STRING_CST && TY_kind(ty) == KIND_ARRAY)
    {
	// have to store string into address,
	// rather than directy copy assignment,
	// so need special code.
	UINT size = TY_size(ty);
	// OSP, string size > ty_size, only init ty_size
	// Replace gs_tree_string_length with load_size
	UINT load_size = ( size > gs_tree_string_length(init) ) ?
					gs_tree_string_length(init) : size;
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
	WGEN_Stmt_Append(
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
	  WGEN_Stmt_Append(
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
	WGEN_Set_ST_Addr_Saved (init_wn);
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
	WGEN_Stmt_Append(wn, Get_Srcpos());
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

// Currently expects a modify_expr which is initialization of a variable
// with a string constant. This modify_expr can be generated during
// gimplification of DECL_EXPR. This function can be extended to handle
// new initialization nodes from GNU4.
void
WGEN_Process_Initialization ( gs_t exp )
{
  INT emitted_bytes = 0;
  Is_True (gs_tree_code(exp) == GS_MODIFY_EXPR,
           ("WGEN_Process_Initialization: Unhandled tree code in expression"));

  gs_t lhs = gs_tree_operand(exp,0);
  gs_t init = gs_tree_operand(exp,1);

  Is_True (gs_tree_code(lhs) == GS_VAR_DECL,
           ("WGEN_Process_Initialization: Unhandled tree code in lhs"));
  Is_True (gs_tree_code(init) == GS_STRING_CST,
           ("WGEN_Process_Initialization: Unhandled tree code in init"));

  ST * st = Get_ST(lhs);
#ifdef NEW_INITIALIZER
  WN* target = WN_Lda(Pointer_Mtype, 0, st);
  Gen_Assign_Of_Init_Val (target, init, 0, 0, ST_type(st), FALSE, 0,
                          FLD_HANDLE(), emitted_bytes);
#else
  Gen_Assign_Of_Init_Val (st, init, 0, 0, ST_type(st), FALSE, 0,
                          FLD_HANDLE(), emitted_bytes);
#endif
}

// For the specified symbol, generate padding at the offset specified.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
AGGINIT::Traverse_Aggregate_Pad (
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
     WGEN_Add_Aggregate_Init_Padding (pad);
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
    WGEN_Stmt_Append (WN_CreateMstore (current_offset, mstore_ty,
                                      zero_wn, addr_wn, pad_wn),
                     Get_Srcpos());
  }
} /* Traverse_Aggregate_Pad */

// The aggregate element for the specified symbol at the current_offset
// is an array having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
AGGINIT::Traverse_Aggregate_Array (
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st,            // symbol being initialized
#endif
  gs_t init_list,      // list of initializers for each array element
  gs_t type,           // type of array
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset) // offset from start of symbol for array
{
  INT    emitted_bytes = 0;
  INT    pad;
  TY_IDX ty            = Get_TY(type);
  TY_IDX ety           = TY_etype (ty);
  UINT   esize         = TY_size (ety);
  gs_t   init;
  gs_t   next;

#ifdef KEY // bug 10880
  if (esize == 0)
    return;
#endif

#ifdef FE_GNU_4_2_0
  INT length = gs_constructor_length(init_list);
  gs_t curr_index_elem = length > 0? gs_operand (init_list, GS_CONSTRUCTOR_ELTS_INDEX) : NULL;
  gs_t curr_value_elem = length > 0? gs_operand (init_list, GS_CONSTRUCTOR_ELTS_VALUE) : NULL;
  for (INT idx = 0;
       idx < length;
       idx++) {

    gs_t element_index = gs_operand(curr_index_elem, 0);
    curr_index_elem = gs_operand(curr_index_elem, 1);

    // Bug 591
    // In gcc-3.2 (updated Gnu front-end), the TREE_PURPOSE(init) 
    // would give us the current_offset where TREE_VALUE(init) goes in. 
    // We need to pad any gaps here. This is unlike the gcc-2.96 front-end
    // whereby output_pending_init_elements calls assemble_zeros which
    // calls WFE_Add_Aggregate_Init_Padding appropriately.
    // We do not want to modify the Gnu front-end (c-typeck.c) and instead
    // hack it inside our front-end.
    INT lindex, hindex;
    // wgen bug 10919: need to handle the new RANGE_EXPR for TREE_PURPOSE
    //  while preserving the fix for bug 2373
    if (gs_tree_code(element_index) == GS_RANGE_EXPR) {
      lindex = gs_get_integer_value(gs_tree_operand(element_index,0));
      hindex = gs_get_integer_value(gs_tree_operand(element_index,1));
    }
    else lindex = hindex = gs_get_integer_value(element_index);
    if ( emitted_bytes/esize < lindex ) {
      // pad (lindex - current_offset/esize)*esize bytes
#ifdef NEW_INITIALIZER
      Traverse_Aggregate_Pad (target, gen_initv,
#else
      Traverse_Aggregate_Pad (st, gen_initv, 
#endif
			      (lindex - emitted_bytes/esize)*esize,
			      current_offset);
      current_offset += (lindex - emitted_bytes/esize)*esize;
      emitted_bytes += (lindex - emitted_bytes/esize)*esize;
    }	

    gs_t tree_value = gs_operand(curr_value_elem, 0);
    curr_value_elem = gs_operand(curr_value_elem, 1);
    if (gs_tree_code(tree_value) == GS_PTRMEM_CST)  {
      gs_t t = gs_expanded_ptrmem_cst(tree_value);
      Is_True(t != NULL,
	      ("Traverse_Aggregate_Array: expanded PTRMEM_CST is NULL"));
      gs_constructor_elts_set_value(init_list, idx, t);
      tree_value = gs_constructor_elts_value(init_list, idx);
      Is_True (tree_value == t, ("gspin list assignment error"));
    }

    if (gs_tree_code(tree_value) == GS_CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      // update array_elem_offset to current_offset to
      // keep track of where each array element starts
#ifdef NEW_INITIALIZER
      Traverse_Aggregate_Constructor (target, tree_value,
#else
      Traverse_Aggregate_Constructor (st, tree_value,
#endif
#ifdef KEY
				      gs_tree_type(type),
#endif
				      gs_tree_type(type),
                                      gen_initv, current_offset, current_offset,
                                      0);
      emitted_bytes += esize;
      current_offset += esize;
    }

    else {
#ifdef TARG_IA64
      // There are two FDESC_EXPR in the GS Tree and they are the same. (why?)
      // We need to skip the later if they are equivalent
      if (gen_initv && 
          gs_tree_code(tree_value) == GS_FDESC_EXPR && 
	  idx < length - 1) {
        gs_t next = gs_operand(curr_value_elem, 0); 
        if ((next != NULL) && 
	    (gs_tree_code(next) == GS_FDESC_EXPR) &&
	    (gs_tree_operand(tree_value, 0) == gs_tree_operand(next, 0)) ) {
  	  idx++; // Skip the next one
          curr_index_elem = gs_operand(curr_index_elem, 1);
          curr_value_elem = gs_operand(curr_value_elem, 1);
	  Add_Initv_For_Tree (tree_value, esize);
  	  emitted_bytes += (esize << 1);  // *2, fptr + gp
  	  current_offset += (esize << 1);
  	  continue;
  	}
      }
#endif

      // initialize SCALARs and POINTERs
      // note that we should not be encountering bit fields
      for (INT index = lindex; index <= hindex; index++) {
	if (gen_initv) {
	  Add_Initv_For_Tree (tree_value, esize);
	  emitted_bytes += esize;
	}
	else
#ifdef NEW_INITIALIZER
          Gen_Assign_Of_Init_Val (target, tree_value, current_offset, 0,
#else
	  Gen_Assign_Of_Init_Val (st, tree_value, current_offset, 0,
#endif
				  ety, FALSE, 0, FLD_HANDLE (), emitted_bytes);
	current_offset += esize;
      }
    }
  }
#else
  for (init = gs_constructor_elts(init_list);
       init;
       init = next) {
    // loop through each array element

    next = gs_tree_chain(init);

    // Bug 591
    // In gcc-3.2 (updated Gnu front-end), the TREE_PURPOSE(init) 
    // would give us the current_offset where TREE_VALUE(init) goes in. 
    // We need to pad any gaps here. This is unlike the gcc-2.96 front-end
    // whereby output_pending_init_elements calls assemble_zeros which
    // calls WFE_Add_Aggregate_Init_Padding appropriately.
    // We do not want to modify the Gnu front-end (c-typeck.c) and instead
    // hack it inside our front-end.
    INT lindex, hindex;
    // wgen bug 10919: need to handle the new RANGE_EXPR for TREE_PURPOSE
    //  while preserving the fix for bug 2373
    if (gs_tree_code(gs_tree_purpose(init)) == GS_RANGE_EXPR) {
      lindex = gs_get_integer_value(gs_tree_operand(gs_tree_purpose(init),0));
      hindex = gs_get_integer_value(gs_tree_operand(gs_tree_purpose(init),1));
    }
    else lindex = hindex = gs_get_integer_value(gs_tree_purpose(init));
    if ( emitted_bytes/esize < lindex ) {
      // pad (lindex - current_offset/esize)*esize bytes
#ifdef NEW_INITIALIZER
      Traverse_Aggregate_Pad (target, gen_initv,
#else
      Traverse_Aggregate_Pad (st, gen_initv, 
#endif
			      (lindex - emitted_bytes/esize)*esize,
			      current_offset);
      current_offset += (lindex - emitted_bytes/esize)*esize;
      emitted_bytes += (lindex - emitted_bytes/esize)*esize;
    }	

    gs_t tree_value = gs_tree_value(init);
    if (gs_tree_code(tree_value) == GS_PTRMEM_CST)  {
      gs_t t = gs_expanded_ptrmem_cst(tree_value);
      Is_True(t != NULL,
	      ("Traverse_Aggregate_Array: expanded PTRMEM_CST is NULL"));
      gs_set_tree_value(init, t);
    }

    if (gs_tree_code(gs_tree_value (init)) == GS_CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      // update array_elem_offset to current_offset to
      // keep track of where each array element starts
#ifdef NEW_INITIALIZER
      Traverse_Aggregate_Constructor (target, gs_tree_value(init), 
#else
      Traverse_Aggregate_Constructor (st, gs_tree_value(init), 
#endif
#ifdef KEY
				      gs_tree_type(type),
#endif
				      gs_tree_type(type),
                                      gen_initv, current_offset, current_offset,
                                      0);
      emitted_bytes += esize;
      current_offset += esize;
    }

    else {
      // initialize SCALARs and POINTERs
      // note that we should not be encountering bit fields
#ifdef TARG_IA64
      if (gen_initv) {
        if ((next != NULL) && (gs_tree_code(tree_value) == GS_FDESC_EXPR) &&
            (gs_tree_code(gs_tree_value(next)) == GS_FDESC_EXPR) &&
	    (gs_tree_value(tree_value) == gs_tree_value(gs_tree_value(next))) ) {
          init = next;
	  next = gs_tree_chain(next);
	  Add_Initv_For_Tree (tree_value, esize);
	  emitted_bytes += (esize << 1);
	  current_offset += (esize << 1);
	  continue;
	}
      }
#endif
      for (INT index = lindex; index <= hindex; index++) {
	if (gen_initv) {
	  Add_Initv_For_Tree (gs_tree_value(init), esize);
	  emitted_bytes += esize;
	}
	else
#ifdef NEW_INITIALIZER
          Gen_Assign_Of_Init_Val (target, gs_tree_value(init), current_offset, 0,
#else
	  Gen_Assign_Of_Init_Val (st, gs_tree_value(init), current_offset, 0,
#endif
				  ety, FALSE, 0, FLD_HANDLE (), emitted_bytes);
	current_offset += esize;
      }
    }
  }
#endif

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
#ifdef NEW_INITIALIZER
    Traverse_Aggregate_Pad (target, gen_initv, pad, current_offset);
#else
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);
#endif
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
AGGINIT::Traverse_Aggregate_Struct (
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st,               // symbol being initialized
#endif
  gs_t init_list,         // list of initializers for elements in STRUCT
#ifdef KEY
  gs_t struct_type,       // type of top level structure
#endif
  gs_t type,              // type of struct
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
  gs_t       field = get_first_real_or_virtual_field(type);  // get first field in gcc
  FLD_HANDLE fld   = TY_fld (ty);        // get first field in WHIRL

  INT        emitted_bytes = 0;          // keep track of # of bytes initialize;
  INT        current_offset_base = current_offset;
  INT        pad;
  BOOL       is_bit_field;
  gs_t       init;
  TY_IDX     fld_ty;

  // account for anonymous WHIRL fields being generated for every direct,
  // nonempty nonvirtual base class.
  // these are generated first in Create_TY_For_Tree (tree_symtab.cxx)

#ifndef KEY	// g++'s class.c already laid out the base types.  Bug 11622.
  gs_t type_binfo, basetypes;

  if ((type_binfo = gs_type_binfo(type)) != NULL &&
      (basetypes = gs_binfo_base_binfos(type_binfo)) != NULL) {

    gs_t list;
    for (list = basetypes; gs_code(list) != EMPTY; list = gs_operand(list, 1)) {
      gs_t binfo = gs_operand(list, 0);
      gs_t basetype = gs_binfo_type(binfo);
      if (!is_empty_base_class(basetype) ||
          !gs_binfo_virtual_p(binfo)) {
        ++field_id;
        fld = FLD_next (fld);
        field_id += TYPE_FIELD_IDS_USED(basetype);
      }
    }
  }
#endif	// KEY

  while (field && gs_tree_code(field) != GS_FIELD_DECL)
    field = next_real_field(type, field);

#ifdef FE_GNU_4_2_0
  INT length = gs_constructor_length(init_list);
  for (INT idx = 0;
       idx < length;
       idx++) {

    ++field_id; // compute field_id for current field
    gs_t element_index = gs_constructor_elts_index(init_list, idx);

    // if the initialization is not for the current field,
    // advance the fields till we find it
    if (field && element_index && gs_tree_code(element_index) == GS_FIELD_DECL) {
      // DevWarn ("Encountered FIELD_DECL during initialization");
      for (;;) {
        if (field == element_index) {
          break;
        }
#ifdef KEY
	// The same field can be created more than once.  Bug 2708.
        if (gs_decl_name(field) &&
	    gs_decl_name(field) == gs_decl_name(element_index)) {
          break;
        }
#endif
#ifndef KEY
        ++field_id;
#else
	field_id = Advance_Field_Id(fld, field_id);
#endif
        fld = FLD_next (fld);
        field = next_real_field(type, field);
        while (field && gs_tree_code(field) != GS_FIELD_DECL)
          field = next_real_field(type, field);
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

    gs_t element_value = gs_constructor_elts_value(init_list, idx);
    if (gs_tree_code(element_value) == GS_NOP_EXPR &&
        gs_tree_code(gs_tree_operand(element_value, 0)) == GS_CONSTRUCTOR)
        element_value = gs_tree_operand(element_value, 0);

    fld_ty = FLD_type(fld);
    if (gs_tree_code(element_value) == GS_CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      gs_t element_type;
      element_type = gs_tree_type(field);
#ifdef KEY
      // The size of the struct have to be adjusted for zero-length arrays 
      // that later get initialized and hence adjusted in size.
      // For an example see Traverse_Aggregate_Array
      INT array_size = TY_size(fld_ty);
#endif
#ifdef NEW_INITIALIZER
      field_id = Traverse_Aggregate_Constructor (target, element_value,
#else
      field_id = Traverse_Aggregate_Constructor (st, element_value,
#endif
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
#ifdef KEY
    // Fields corresponding to pointer-to-member-functions are represented as
    // records with fields __pfn and __delta.  The initializer is a TREE_LIST
    // of __pfn and __delta.  Bug 3143.
    else if (gs_type_ptrmemfunc_p(gs_tree_type(field))) {
      gs_t element_type;
      element_type = gs_tree_type(field);
      gs_t init_value = element_value;
      if (gs_tree_code(init_value) == GS_NOP_EXPR)
	init_value = gs_tree_operand(init_value, 0); // bug 10853
      GS_ASSERT(gs_tree_code(init_value) == GS_PTRMEM_CST,
		("Traverse_Aggregate_Struct: GS_PTRMEM_CST not found"));
      // PTRMEM_CST was expanded by GCC's cplus_expand_constant.  Get the
      // result.
      gs_t expanded_ptrmem_cst = gs_expanded_ptrmem_cst(init_value);
      FmtAssert(expanded_ptrmem_cst != NULL,
               ("Traverse_Aggregate_Struct: expanded PTRMEM_CST is NULL"));
#ifdef NEW_INITIALIZER
      field_id = Traverse_Aggregate_Constructor (target, expanded_ptrmem_cst,
#else
      field_id = Traverse_Aggregate_Constructor (st, expanded_ptrmem_cst,
#endif
  						 struct_type,
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
#ifdef KEY
	  // Bug 1021
	  // For fields that are arrays of characters with unspecified 
	  // lengths, update the field size and the structure size here.
	  if (TY_size(fld_ty) == 0 && 
	      gs_tree_code(element_value) == GS_STRING_CST &&
	      gs_tree_string_pointer(element_value)) {
	    Ty_Table[fld_ty].size = 
	    	strlen(gs_tree_string_pointer(element_value));
	    TY_IDX struct_ty = Get_TY ( struct_type );
	    Ty_Table[struct_ty].size += TY_size(fld_ty);
	  }
#endif	  
          Add_Initv_For_Tree (element_value, TY_size(fld_ty));
          emitted_bytes += TY_size(fld_ty);
        }
        else { // do 1 byte a time
          Add_Bitfield_Initv_For_Tree (element_value, fld, emitted_bytes);
          // emitted_bytes updated by the call as reference parameter
        }
      }
      else {
#ifdef NEW_INITIALIZER
        Gen_Assign_Of_Init_Val (target, element_value,
#else
        Gen_Assign_Of_Init_Val (st, element_value,
#endif
                                current_offset, array_elem_offset,
#ifndef KEY
                                is_bit_field ? ty : fld_ty,
#else
				is_bit_field ? Get_TY(struct_type) : fld_ty,
#endif
                                is_bit_field, field_id, fld, emitted_bytes);
        // emitted_bytes updated by the call as reference parameter

        // bug908 open64.net. update field_id for the case of field is struct type.                        
        if (TY_kind(fld_ty) == KIND_STRUCT) {
          FLD_HANDLE field;
          field = TY_fld(fld_ty); // get first field
          while (!field.Is_Null()) {
            field_id=Advance_Field_Id(field,field_id);
            field=FLD_next(field);
          }
        }        
      }
    }

    current_offset = current_offset_base + emitted_bytes;
    fld = FLD_next(fld);
    field = next_real_field(type, field);
    while (field && gs_tree_code(field) != GS_FIELD_DECL)
      field = next_real_field(type, field);
  }
#else // end FE_GNU_4_2_0
#ifdef KEY
  if (gs_constructor_elts(init_list))
    ++field_id; // compute field_id for current field
#endif
  for (init = gs_constructor_elts(init_list);
       init;
       init = gs_tree_chain(init)) {
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
    if (field && gs_tree_purpose(init) && gs_tree_code(gs_tree_purpose(init)) == GS_FIELD_DECL) {
      DevWarn ("Encountered FIELD_DECL during initialization");
      for (;;) {
        if (field == gs_tree_purpose(init)) {
          break;
        }
#ifdef KEY
	// The same field can be created more than once.  Bug 2708.
        if (gs_decl_name(field) &&
	    gs_decl_name(field) == gs_decl_name(gs_tree_purpose(init))) {
          break;
        }
#endif
#ifndef KEY
        ++field_id;
#else
	field_id = Advance_Field_Id(fld, field_id);
#endif
        fld = FLD_next (fld);
        field = next_real_field(type, field);
        while (field && gs_tree_code(field) != GS_FIELD_DECL)
          field = next_real_field(type, field);
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

    gs_t init_value = gs_tree_value(init);
    if ( gs_tree_code(init_value) == GS_NOP_EXPR )
      init_value = gs_tree_operand(init_value, 0);
    
    if ( gs_tree_code(init_value) == GS_CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      gs_t element_type;
      element_type = gs_tree_type(field);
#ifdef KEY
      // The size of the struct have to be adjusted for zero-length arrays 
      // that later get initialized and hence adjusted in size.
      // For an example see Traverse_Aggregate_Array
      INT array_size = TY_size(fld_ty);
#endif

#ifdef NEW_INITIALIZER
      field_id = Traverse_Aggregate_Constructor (target, init_value,
#else
      field_id = Traverse_Aggregate_Constructor (st, init_value,
#endif
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
#ifdef KEY
    // Fields corresponding to pointer-to-member-functions are represented as
    // records with fields __pfn and __delta.  The initializer is a TREE_LIST
    // of __pfn and __delta.  Bug 3143.
    else if (field && gs_type_ptrmemfunc_p(gs_tree_type(field))) {
      gs_t element_type;
      element_type = gs_tree_type(field);

      GS_ASSERT(gs_tree_code(init_value) == GS_PTRMEM_CST,
		("Traverse_Aggregate_Struct: GS_PTRMEM_CST not found"));
      // PTRMEM_CST was expanded by GCC's cplus_expand_constant.  Get the
      // result.
      gs_t expanded_ptrmem_cst = gs_expanded_ptrmem_cst(init_value);
      FmtAssert(expanded_ptrmem_cst != NULL,
               ("Traverse_Aggregate_Struct: expanded PTRMEM_CST is NULL"));
#ifdef NEW_INITIALIZER
      field_id = Traverse_Aggregate_Constructor (target, expanded_ptrmem_cst,
#else
      field_id = Traverse_Aggregate_Constructor (st, expanded_ptrmem_cst,
#endif
  						 struct_type,
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
#ifdef KEY
	  // Bug 1021
	  // For fields that are arrays of characters with unspecified 
	  // lengths, update the field size and the structure size here.
	  if (TY_size(fld_ty) == 0 && 
	      gs_tree_code(gs_tree_value(init)) == GS_STRING_CST &&
	      gs_tree_string_pointer(gs_tree_value(init))) {
	    Ty_Table[fld_ty].size = 
	    	strlen(gs_tree_string_pointer(gs_tree_value(init)));
	    TY_IDX struct_ty = Get_TY ( struct_type );
	    Ty_Table[struct_ty].size += TY_size(fld_ty);
	  }
#endif	  
          Add_Initv_For_Tree (gs_tree_value(init), TY_size(fld_ty));
          emitted_bytes += TY_size(fld_ty);
        }
        else { // do 1 byte a time
          Add_Bitfield_Initv_For_Tree (gs_tree_value(init), fld, emitted_bytes);
          // emitted_bytes updated by the call as reference parameter
        }
      }
      else {
#ifdef NEW_INITIALIZER
        Gen_Assign_Of_Init_Val (target,  gs_tree_value(init),
#else
        Gen_Assign_Of_Init_Val (st, gs_tree_value(init),
#endif
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
    if (gs_tree_chain(init)) // only if there is an iteration left
      field_id = Advance_Field_Id(fld, field_id);
#endif
    // advance to next field
    current_offset = current_offset_base + emitted_bytes;
    fld = FLD_next(fld);
    if (field) // bug 13102
      field = next_real_field(type, field);
    while (field && gs_tree_code(field) != GS_FIELD_DECL)
      field = next_real_field(type, field);
  }
#endif

  // if not all fields have been initialized, then loop through
  // the remaining fields to update field_id
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
	TYPE_ID mtyp = TY_mtype(fld_ty);
	mtyp = (mtyp == MTYPE_V) ? MTYPE_I4 : Widen_Mtype(mtyp);
	WN *init_wn = WN_Intconst (mtyp, 0);
#ifdef NEW_INITIALIZER
        TY_IDX struct_ty = Get_TY ( struct_type );
        //TY_IDX ptr_ty = Make_Pointer_Type(struct_ty);
        //WN *wn = WN_CreateMstore(array_elem_offset, ptr_ty, init_wn, target, WN_Intconst(MTYPE_I4, TY_size(struct_ty)) );
        WN* wn = NULL;
        Is_True( (WN_operator(target) == OPR_LDID ||
                  WN_operator(target) == OPR_LDA),
                 ("Invalid operator for target"));
        if( WN_operator(target) == OPR_LDID ) {
            TY_IDX ptr_ty = Make_Pointer_Type(struct_ty);
            wn = WN_Istore(MTYPE_BS, array_elem_offset, struct_ty, 
                           target, init_wn, field_id);
        }
        else { // OPR_LDA
            ST *st = WN_st(target);
            wn = WN_Stid (MTYPE_BS, WN_lda_offset(target) + array_elem_offset,
                          st, struct_ty, init_wn, field_id);
        }
#else
	WN *wn = WN_Stid (MTYPE_BS, ST_ofst(st) + array_elem_offset, st,
#ifndef KEY
			  ty, 
#else
			  Get_TY(struct_type),
#endif
			  init_wn, field_id);
#endif
	WGEN_Stmt_Append(wn, Get_Srcpos());
      }
    }
#ifdef KEY
    field_id = Advance_Field_Id(fld,field_id)-1;
#endif

    fld = FLD_next(fld);
    field = next_real_field(type, field);
    while (field && gs_tree_code(field) != GS_FIELD_DECL)
      field = next_real_field(type, field);
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
AGGINIT::Traverse_Aggregate_Vector (
#ifdef NEW_INITIALIZER
  WN * target,
#else
  ST * st,             // symbol being initialized
#endif
  gs_t init_list,      // list of initializers for units in vector
  TYPE_ID mtyp,	       // type of vector
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset, // offset from start of symbol for current vector
  BOOL vec_cst)	       // init_list is a constant or not
{
  gs_t init;
  INT emitted_bytes = 0;

  if (vec_cst)
    init = gs_tree_vector_cst_elts (init_list);
#ifndef FE_GNU_4_2_0
  else
    init = gs_constructor_elts (init_list);
#else
  if (!vec_cst)
  {
    INT length = gs_constructor_length(init_list);
    for (INT idx = 0;
         idx < length;
         idx++) {

      gs_t element_value = gs_constructor_elts_value(init_list, idx);

      gs_t unit_type = gs_tree_type(element_value);
      gs_t size = gs_type_size (unit_type);
      Is_True (gs_tree_code (size) == GS_INTEGER_CST,
               ("Traverse_Aggregate_Vector: Vector of variable-sized units?"));
      UINT esize = gs_get_integer_value(size) / BITSPERBYTE;
      if (gen_initv)
      {
        Add_Initv_For_Tree (element_value, esize);
        emitted_bytes += esize;
      }
      else
#ifdef NEW_INITIALIZER
        Gen_Assign_Of_Init_Val (target, element_value,
#else
        Gen_Assign_Of_Init_Val (st, element_value,
#endif
                                current_offset, 0,
                                Get_TY(unit_type),
                                0, 0, FLD_HANDLE(), emitted_bytes);
      current_offset += esize;
    }
  }
  else
#endif

  for (;
       init;
       init = gs_tree_chain(init))
  {
    gs_t unit_type = gs_tree_type(gs_tree_value(init));
    gs_t size = gs_type_size (unit_type);
    Is_True (gs_tree_code (size) == GS_INTEGER_CST,
             ("Traverse_Aggregate_Vector: Vector of variable-sized units?"));
    UINT esize = gs_get_integer_value(size) / BITSPERBYTE;
    if (gen_initv)
    {
      Add_Initv_For_Tree (gs_tree_value(init), esize);
      emitted_bytes += esize;
    }
    else
#ifdef NEW_INITIALIZER
      Gen_Assign_Of_Init_Val (target, gs_tree_value(init),
#else
      Gen_Assign_Of_Init_Val (st, gs_tree_value(init),
#endif
                              current_offset, 0,
                              Get_TY(unit_type),
                              0, 0, FLD_HANDLE(), emitted_bytes);
    current_offset += esize;
  }

  // bug 11615: If the entire vector has not been initialized, pad till the end
  INT pad = MTYPE_byte_size(mtyp) - emitted_bytes;

#ifdef NEW_INITIALIZER
    // When object size can't be obtained from mtyp, get it from
    // the pointed-to object.
    if ((MTYPE_byte_size(mtyp) == 0) && target 
	&& (WN_operator(target) == OPR_LDA)) {
      TY_IDX pointer_type = WN_ty(target);
      TY_IDX object_type = TY_pointed(pointer_type);
      UINT64 object_size = TY_size(object_type);
      pad = object_size - emitted_bytes;
    }
#endif

  if (pad > 0)
#ifdef NEW_INITIALIZER
    Traverse_Aggregate_Pad (target, gen_initv, pad, current_offset);
#else
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);
#endif
} /* Traverse_Aggregate_Vector */

void
Traverse_Aggregate_Vector_Const (
#ifdef NEW_INITIALIZER
  WN * target,
#else
  ST * st,             // symbol being initialized
#endif
  gs_t init_list,      // list of initializers for units in vector
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset) // offset from start of symbol for current vector
{
  AGGINIT agginit;
#ifdef NEW_INITIALIZER
  agginit.Traverse_Aggregate_Vector(target, init_list, WN_desc(target),
#else
  agginit.Traverse_Aggregate_Vector(st, init_list, TY_mtype(ST_type(st)), 
#endif
				    gen_initv, current_offset, TRUE);
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
AGGINIT::Traverse_Aggregate_Constructor (
#ifdef NEW_INITIALIZER
  WN   *target,
#else
  ST   *st,               // symbol being initialized
#endif
  gs_t init_list,         // list of initilaizers for this aggregate
#ifdef KEY
  gs_t struct_type,	  // type of top level struct
#endif
  gs_t type,              // type of aggregate being initialized
  BOOL gen_initv,         // TRUE  if initializing with INITV,
                          // FALSE if initializing with statements
  UINT current_offset,    // offset from start of symbol for this aggregate
  UINT array_elem_offset,
  UINT field_id)
{
  TY_IDX ty = Get_TY(type);

  INITV_IDX last_initv_save;

  if (gen_initv) {

    WGEN_Add_Init_Block();
    INITV_Init_Block(_last_initv, INITV_Next_Idx());
    _not_root = TRUE;
    last_initv_save = _last_initv;
    _last_initv = 0;
  }

  if (TY_kind (ty) == KIND_STRUCT) {
#ifdef NEW_INITIALIZER
    field_id = Traverse_Aggregate_Struct (target, init_list,
#else
    field_id = Traverse_Aggregate_Struct (st, init_list, 
#endif

#ifdef KEY
					  struct_type,
#endif
    					  type, gen_initv,
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
    Traverse_Aggregate_Vector(target, init_list, TY_mtype(ty), gen_initv, current_offset);
#else
    Traverse_Aggregate_Vector(st, init_list, TY_mtype(ty), gen_initv, current_offset);
#endif
  }
#endif

  else
    Fail_FmtAssertion ("Traverse_Aggregate_Constructor: non STRUCT/ARRAY");

#ifdef KEY
  if (gen_initv && _last_initv == 0) // for empty list; set to reserved value (bug 961)
    INITV_Init_Block(last_initv_save, INITV_IDX_ZERO);
#endif

  // restore current level's last_aggregate_initv and return
  _last_initv = last_initv_save;

  return field_id;
} /* Traverse_Aggregate_Constructor */

void
AGGINIT::Add_Init_For_WHIRL(WN *init_wn, UINT size, INT64 ofst)
{
  switch (WN_operator(init_wn)) {
  case OPR_INTCONST:
    WGEN_Add_Aggregate_Init_Integer(WN_const_val(init_wn) + ofst, size);
    return;
  case OPR_LDID:
  case OPR_LDA:
    WGEN_Add_Aggregate_Init_Symbol(WN_st(init_wn), WN_offset(init_wn)+ofst);
// bugs 555, 11308
#ifdef KEY
    Set_ST_initv_in_other_st (WN_st(init_wn));
#endif
    return;
  case OPR_ADD:
    if (WN_operator(WN_kid0(init_wn)) == OPR_INTCONST) {
      Add_Init_For_WHIRL(WN_kid1(init_wn), size, 
      			 ofst + WN_const_val(WN_kid0(init_wn)));
      return;
    } 
    if (WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      Add_Init_For_WHIRL(WN_kid0(init_wn), size, 
      			 ofst + WN_const_val(WN_kid1(init_wn)));
      return;
    }
    if (WN_Tree_Is_Label_Values(init_wn) == TRUE) {
      Add_Init_For_Label_Values(init_wn, size);
      return;
    }
    break;
  case OPR_SUB:
    if (WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      Add_Init_For_WHIRL(WN_kid0(init_wn), size, 
      			 ofst - WN_const_val(WN_kid1(init_wn)));
      return;
    } 
    if (WN_Tree_Is_Label_Values(init_wn) == TRUE) {
      Add_Init_For_Label_Values(init_wn, size);
      return;
    }
    break;
#ifdef KEY // bug 10924
  case OPR_ARRAY: 
    {
      INT displace = 0;
      INT multiplier = 1;
      BOOL const_indices = TRUE;
      for (INT i = 0; i < WN_num_dim(init_wn); i++) {
	if (WN_operator(WN_array_index(init_wn, i)) != OPR_INTCONST ||
	    WN_operator(WN_array_dim(init_wn, i)) != OPR_INTCONST) {
	  const_indices = FALSE;
	  break;
	}
	displace += WN_const_val(WN_array_index(init_wn, i)) * multiplier;
	multiplier *= WN_const_val(WN_array_dim(init_wn, i));
      }
      if (const_indices) {
	displace *= WN_element_size(init_wn);
	Add_Init_For_WHIRL(WN_array_base(init_wn), size, ofst + displace);
	return;
      }
      break;
    }
#endif
  case OPR_CVT:
    if (MTYPE_is_integral(WN_rtype(init_wn)) &&
        MTYPE_is_integral(WN_desc(init_wn))) {
      Add_Init_For_WHIRL(WN_kid0(init_wn), size, ofst);
      return;
    }
    break;
  case OPR_CVTL:
    Add_Init_For_WHIRL(WN_kid0(init_wn), size, ofst);
    return;
  default: ;
  }
  Fail_FmtAssertion("Add_Init_For_WHIRL: unexpected static init tree");
}


static inline
BOOL WN_is_lda_label(WN* wn) {
  if ( WN_operator_is(wn, OPR_LDA_LABEL) )
    return TRUE;
  else if ( WN_operator_is(wn, OPR_CVT) )
    return WN_is_lda_label(WN_kid0(wn));
  else
    return FALSE;
} /* WN_is_lda_label */

static inline
INT32 WN_lda_label_number(WN* wn) {
  if ( WN_operator_is(wn, OPR_LDA_LABEL) )
    return WN_label_number(wn);
  else if ( WN_operator_is(wn, OPR_CVT) )
    return WN_lda_label_number(WN_kid0(wn));
  else {
    FmtAssert( FALSE, ("WN is not a LDA_LABEL or CVT"));
    return 0;
  }
} /* WN_lda_label_number */

static inline
mTYPE_ID Size_To_MTYPE(UINT size) {
  switch ( size ) {
    case 1:  return MTYPE_I1;
    case 2:  return MTYPE_I2;
    case 4:  return MTYPE_I4;
    case 8:  return MTYPE_I8;
    default:
      FmtAssert( FALSE, ("Fix me, unexpected size=%d", size) );
      return MTYPE_UNKNOWN;
  }
} /* Size_To_MTYPE */

BOOL
AGGINIT::WN_Tree_Is_Label_Values(WN* init_wn)
{
  // check if the tree is for initializing label values ( .L1 - .L2 )
  if ( WN_operator(init_wn) == OPR_SUB ) {
    // only allow the substractions on labels
    if ( ! WN_is_lda_label(WN_kid0(init_wn)) ||
         ! WN_is_lda_label(WN_kid1(init_wn)) ) {
      return FALSE;
    }
  }
  else if ( WN_operator(init_wn) == OPR_ADD ) {
    // check children of the add node, which can be OPR_ADD or OPR_SUB
    if ( WN_Tree_Is_Label_Values(WN_kid0(init_wn)) == FALSE ) {
      return FALSE;
    }
    if ( WN_Tree_Is_Label_Values(WN_kid1(init_wn)) == FALSE ) {
      return FALSE;
    }
  }
  else {
    // Other operator is not allowed in label values
    return FALSE;
  }
  return TRUE;
}

void
AGGINIT::Add_Init_For_Label_Values(WN* init_wn, UINT size, BOOL first_child, BOOL last_child)
{
  OPERATOR opr = WN_operator(init_wn);
  Is_True(opr == OPR_ADD || opr == OPR_SUB,
          ("Only OPR_ADD and OPR_SUB is allowed in Label Values"));

  WN* kid0 = WN_kid0(init_wn);
  if ( WN_is_lda_label(kid0) ) {
    Is_True(opr == OPR_SUB,
            ("Only Label Substraction is allowed for LDA_LABEL"));
    WGEN_Add_Aggregate_Init_Label ( WN_lda_label_number(kid0),
             (first_child == TRUE) ? INITVLABELFLAGS_VALUES_FIRST : INITVLABELFLAGS_VALUES_PLUS,
             Size_To_MTYPE(size) );
  }
  else {
    Add_Init_For_Label_Values(kid0, size, first_child, FALSE);
  }

  WN* kid1 = WN_kid1(init_wn);
  if ( WN_is_lda_label(kid1) ) {
    Is_True(opr == OPR_SUB,
            ("Only Label Substraction is allowed for LDA_LABEL"));
    WGEN_Add_Aggregate_Init_Label (  WN_lda_label_number(kid1),
             (last_child == TRUE) ? INITVLABELFLAGS_VALUES_LAST : INITVLABELFLAGS_VALUES_MINUS,
             Size_To_MTYPE(size) );
  }
  else {
    Add_Init_For_Label_Values(kid1, size, FALSE, last_child);
  }
}


static BOOL Is_Aggregate_Init_Zero (gs_t init_list, gs_t type);
static BOOL Is_Aggregate_Init_Zero_Array (gs_t init_list, gs_t type);
static BOOL Is_Aggregate_Init_Zero_Struct (gs_t init_list, gs_t type);
static BOOL Is_Real_Init_Zero (gs_t init, UINT size);

/*
Is_Aggregate_Init_Zero:
Return TRUE if all initializers found in init_list are zeros
*/
static BOOL 
Is_Aggregate_Init_Zero (gs_t init_list, gs_t type)
{
  TY_IDX ty = Get_TY (type);
  TY_KIND kind = TY_kind (ty);

  switch (kind) {
    case KIND_ARRAY:
      return Is_Aggregate_Init_Zero_Array (init_list, type);
    case KIND_STRUCT:
      return Is_Aggregate_Init_Zero_Struct (init_list, type);
    default:
      return FALSE;
  }
}

/*
Is_Aggregate_Init_Zero_Array:
Return TRUE if the array is initialized to all zeros
*/
static BOOL 
Is_Aggregate_Init_Zero_Array (gs_t init_list, gs_t type)
{
#ifdef FE_GNU_4_2_0
  UINT esize = TY_size(TY_etype(Get_TY(type)));
  INT length = gs_constructor_length(init_list);

  gs_t curr_value_elem = length > 0 ? gs_operand (init_list, GS_CONSTRUCTOR_ELTS_VALUE) : NULL;

  for (INT idx = 0; idx < length; idx++) {

    gs_t tree_value = gs_operand(curr_value_elem, 0);

    curr_value_elem = gs_operand(curr_value_elem, 1);

    switch (gs_tree_code (tree_value)) {
      case GS_CONSTRUCTOR:
        if (!Is_Aggregate_Init_Zero (tree_value, gs_tree_type (type)))
          return FALSE;
        break;
      case GS_INTEGER_CST:
        if (gs_get_integer_value(tree_value) != 0)
          return FALSE;
        break;
      case GS_REAL_CST:
        if (!Is_Real_Init_Zero(tree_value, esize))
          return FALSE;
        break;
      default:
        // Unknown / unimplemented types. Return FALSE
        return FALSE;
    }
  }
  return TRUE;
#else
  // Not implemented for old front-end
  return FALSE;
#endif
}

/*
Is_Aggregate_Init_Zero_Struct:
Return TRUE if the struct is initialized to all zeros
*/
static BOOL 
Is_Aggregate_Init_Zero_Struct (gs_t init_list, gs_t type)
{
#ifdef FE_GNU_4_2_0
  TY_IDX     ty    = Get_TY(type);
  gs_t       field = get_first_real_or_virtual_field(type);
  FLD_HANDLE fld   = TY_fld (ty);

  gs_t       init;

  while (field && gs_tree_code(field) != GS_FIELD_DECL)
    field = next_real_field(type, field);

  INT length = gs_constructor_length(init_list);
  for (INT idx = 0; idx < length; idx++) {

    gs_t element_index = gs_constructor_elts_index(init_list, idx);

    // if the initialization is not for the current field,
    // advance the fields till we find it
    if (field && element_index && gs_tree_code(element_index) == GS_FIELD_DECL) {
      for (;;) {
        if (field == element_index) {
          break;
        }
        if (gs_decl_name(field) && gs_decl_name(field) == gs_decl_name(element_index)) {
          break;
        }
        fld = FLD_next (fld);
        field = next_real_field(type, field);
        while (field && gs_tree_code(field) != GS_FIELD_DECL)
          field = next_real_field(type, field);
      }
    }

    gs_t element_value = gs_constructor_elts_value(init_list, idx);
    if (gs_tree_code(element_value) == GS_CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      gs_t element_type;
      element_type = gs_tree_type(field);
      if (!Is_Aggregate_Init_Zero (element_value, element_type))
        return FALSE;
    }
    else if (gs_type_ptrmemfunc_p(gs_tree_type(field))) {
      return FALSE;
    }
    else if (gs_tree_code(element_value) == GS_REAL_CST) {
      if (!Is_Real_Init_Zero (element_value, TY_size(FLD_type(fld))))
        return FALSE;
    }
    else if (gs_tree_code(element_value) == GS_INTEGER_CST) {
      // SCALARs and POINTERs
      if (gs_get_integer_value (element_value) != 0)
        return FALSE;
    }
    else {
      return FALSE;
    }

    // advance to next field
    fld = FLD_next(fld);
    field = next_real_field(type, field);
    while (field && gs_tree_code(field) != GS_FIELD_DECL)
      field = next_real_field(type, field);
  }

  return TRUE;
#else
  // Not implemented for old front-end
  return FALSE;
#endif
}

/*
Is_Real_Init_Zero:
Return TRUE if the binary representation of the floating-point
value is zero on target
*/
static BOOL
Is_Real_Init_Zero (gs_t real, UINT size)
{
  TCON tc;
  switch (size) {
    case 4:
      tc = Host_To_Targ_Float_4 (MTYPE_F4, gs_tree_real_cst_f(real));
      break;
    case 8:
      tc = Host_To_Targ_Float (MTYPE_F8, gs_tree_real_cst_d(real));
      break;
    case 12:
    case 16:
#if defined(TARG_IA64) || defined(TARG_X8664)
      tc = Host_To_Targ_Float_10(MTYPE_F10, gs_tree_real_cst_ld(real));
#else
      tc = Host_To_Targ_Quad (gs_tree_real_cst_ld(real));
#endif
      break;
    default:
      FmtAssert(FALSE, ("Is_Real_Init_Zero unexpected size"));
      break;
  }
  return (tc.vals.uval.u0 == 0 && tc.vals.uval.u1 == 0 &&
          tc.vals.uval.u2 == 0 && tc.vals.uval.u3 == 0);
}

void
AGGINIT::Add_Inito_For_Tree (gs_t init, ST *st)
{
  gs_t kid;
  gs_code_t code;

  _last_initv = 0;
  switch (gs_tree_code(init)) {
  case GS_INTEGER_CST:
	UINT64 val;
	val = gs_get_integer_value (init);
	// For C, don't set INIT_VALUE_ZERO for symbols with assigned
	// sections to ensure they remain DGLOBAL, and get assigned to
	// data section.
	if (val == 0 && (lang_cplus || !ST_has_named_section(st))) {
		Set_ST_init_value_zero(st);
		if (ST_sclass(st) == SCLASS_DGLOBAL)
			Set_ST_sclass(st, SCLASS_UGLOBAL);
		return;
	}
	_inito = New_INITO (st);
	_not_root = FALSE;
	WGEN_Add_Aggregate_Init_Integer (val, TY_size(ST_type(st)));
	return;
  case GS_REAL_CST:
	if ((lang_cplus || !ST_has_named_section(st)) && Is_Real_Init_Zero(init, TY_size(ST_type(st)))) {
		Set_ST_init_value_zero(st);
		if (ST_sclass(st) == SCLASS_DGLOBAL)
			Set_ST_sclass(st, SCLASS_UGLOBAL);
		return;
	}
	_inito = New_INITO (st);
	_not_root = FALSE;
	WGEN_Add_Aggregate_Init_Real (init, TY_size(ST_type(st)));
	return;
  case GS_COMPLEX_CST:
	_inito = New_INITO (st);
	_not_root = FALSE;
	WGEN_Add_Aggregate_Init_Complex(gs_tree_realpart(init), 
					gs_tree_imagpart(init), 
					TY_size(ST_type(st)));
	return;
  case GS_STRING_CST:
	_inito = New_INITO (st);
	_not_root = FALSE;
	WGEN_Add_Aggregate_Init_String (const_cast<char*>(gs_tree_string_pointer(init)),
	  // null character added can cause string length to exceed var length
                                       TY_size(ST_type(st)) < gs_tree_string_length(init) ?
                                       TY_size(ST_type(st)) :
                                       gs_tree_string_length(init));
	if (TY_size (ST_type(st)) > gs_tree_string_length(init))
		WGEN_Add_Aggregate_Init_Padding (TY_size (ST_type(st)) -
						 gs_tree_string_length(init));
	return;
#ifdef KEY // bug 10501
  case GS_VECTOR_CST:
	_inito = New_INITO (st);
	_not_root = FALSE;
	WGEN_Add_Aggregate_Init_Vector (init);
	return;

  case GS_CONVERT_EXPR: // bug 14186
#endif
  case GS_NOP_EXPR: {
	  Add_Inito_For_Tree (gs_tree_operand(init,0), st);
	  return;
        }
  case GS_ADDR_EXPR:
	kid = gs_tree_operand(init,0);
	code = gs_tree_code(kid);
	if (code == GS_VAR_DECL ||
	    code == GS_FUNCTION_DECL ||
	    code == GS_STRING_CST ||
	    code == GS_LABEL_DECL ||
	    code == GS_COMPOUND_LITERAL_EXPR) {
		_inito = New_INITO (st);
		_not_root = FALSE;
		WGEN_Add_Aggregate_Init_Address (kid);
		return;
	}
        break;
  case GS_PLUS_EXPR:
	kid = gs_tree_operand(init,0);
	if (gs_tree_code(kid) == GS_ADDR_EXPR) {
		// symbol+offset
		Add_Inito_For_Tree (kid, st);
		kid = gs_tree_operand(init,1);
		if (INITV_kind(_last_initv) == INITVKIND_SYMOFF
			&& gs_tree_code(kid) == GS_INTEGER_CST)
		{
			Set_INITV_ofst (_last_initv,
				gs_get_integer_value(kid));
			return;
		}
	}
	break;
  case GS_MINUS_EXPR:
	kid = gs_tree_operand(init,0);
	if (gs_tree_code(kid) == GS_ADDR_EXPR) {
		// symbol-offset
		Add_Inito_For_Tree (kid, st);
		kid = gs_tree_operand(init,1);
		if (INITV_kind(_last_initv) == INITVKIND_SYMOFF
			&& gs_tree_code(kid) == GS_INTEGER_CST)
		{
			Set_INITV_ofst (_last_initv,
				-gs_get_integer_value(kid));
			return;
		}
	}
	break;
  case GS_CONSTRUCTOR: {
	if ((lang_cplus || !ST_has_named_section(st)) && Is_Aggregate_Init_Zero(init, gs_tree_type(init))) {
		// If the aggregate is initialized but all its initializers are equal to zero,
		// it can go to BSS
		Set_ST_init_value_zero(st);
		if (ST_sclass(st) == SCLASS_DGLOBAL)
			Set_ST_sclass(st, SCLASS_UGLOBAL);
		return;
	}
	AGGINIT agginit(New_INITO(st));
#ifdef NEW_INITIALIZER
        WN* target = WN_Lda (Pointer_Mtype, 0, st, 0);
        agginit.Traverse_Aggregate_Constructor (target, init,
#else
	agginit.Traverse_Aggregate_Constructor (st, init, 
#endif
#ifdef KEY
					gs_tree_type(init),
#endif
					gs_tree_type(init),
					TRUE /*gen_initv*/, 0, 0, 0);
	return;
  	}
  }

  // not recognized, so try to simplify
  WN *init_wn = WGEN_Expand_Expr (init);
  _inito = New_INITO (st);
  _not_root = FALSE;
  if (WN_operator(init_wn) == OPR_LDA_LABEL) {
    gs_t label_decl = 
      (gs_tree_code(gs_tree_operand(init, 0)) == GS_ADDR_EXPR)?
      gs_tree_operand (gs_tree_operand (init, 0), 0):
      gs_tree_operand (init, 0);
    LABEL_IDX label_idx = 
      WGEN_Get_LABEL (label_decl, FALSE);
    WGEN_Add_Aggregate_Init_Label (label_idx);
  }
  else Add_Init_For_WHIRL(init_wn, TY_size(ST_type(st)), 0);
  WN_DELETE_Tree(init_wn);
}


extern ST *
WGEN_Generate_Temp_For_Initialized_Aggregate (gs_t init, char * name)
{
  TY_IDX ty_idx = Get_TY(gs_tree_type(init));
  ST *temp = New_ST (CURRENT_SYMTAB);
  ST_Init (temp,
	Save_Str2 (name, ".init"),
	CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL,
	ty_idx );
  AGGINIT agginit;
  gs_code_t code = gs_tree_code(init);
  if (code == GS_CONSTRUCTOR
	&& ! Use_Static_Init_For_Aggregate (temp, init)) 
  {
	// do sequence of stores to temp
	Set_ST_sclass(temp, SCLASS_AUTO);	// put on stack
#ifdef NEW_INITIALIZER
        WN* target = WN_Lda (Pointer_Mtype, 0, temp, 0);
        agginit.Traverse_Aggregate_Constructor (target, init,
#else
	agginit.Traverse_Aggregate_Constructor (temp, init, 
#endif
#ifdef KEY
					gs_tree_type(init),
#endif
					gs_tree_type(init),
                                        FALSE /*gen_initv*/, 0, 0, 0);
  }
  else if (code == GS_CONSTRUCTOR) {
	// setup inito for temp
	Set_ST_is_initialized(temp);
	agginit.Set_inito(New_INITO(temp));
#ifdef NEW_INITIALIZER
        WN* target = WN_Lda (Pointer_Mtype, 0, temp, 0);
        agginit.Traverse_Aggregate_Constructor (target, init,
#else
	agginit.Traverse_Aggregate_Constructor (temp, init, 
#endif
#ifdef KEY
					gs_tree_type(init),
#endif
					gs_tree_type(init),
                                        TRUE /*gen_initv*/, 0, 0, 0);
	// following inlined from WGEN_Finish_Aggregate_Init()
	TY_IDX ty = ST_type(temp);
	if (TY_size(ty) == 0 ||
	    (TY_kind(ty) == KIND_ARRAY &&
	     !ARB_const_ubnd (TY_arb(ty)) &&
	     TY_size(ty) <= Get_INITO_Size(agginit.Inito()))) {
	      // e.g. array whose size is determined by init;
	      // fill in with initv size
	      Set_TY_size(ty, Get_INITO_Size(agginit.Inito()));
	      if (TY_kind(ty) == KIND_ARRAY) {
		      Set_ARB_const_ubnd (TY_arb(ty));
		      Set_ARB_ubnd_val (TY_arb(ty), 
			      (TY_size(ty) / TY_size(TY_etype(ty))) - 1 );
	      }
	}
	if (agginit.Last_initv() == 0) {
	  agginit.WGEN_Add_Aggregate_Init_Padding (0);
	}
  }
  else {
    Set_ST_is_initialized(temp);
    AGGINIT agginit;
    agginit.Add_Inito_For_Tree(init, temp);
  }
  return temp;
}

#ifdef NEW_INITIALIZER
ST* WGEN_Generate_Initialized_Aggregate(WN* target, gs_t init)
{
  Is_True(gs_tree_code(init) == GS_CONSTRUCTOR, 
          ("wrong tree code for target"));
  Is_True((WN_operator(target) == OPR_LDID ||
           WN_operator(target) == OPR_LDA),
          ("Invalid target operator"));
  ST* target_st = WN_st(target);

  AGGINIT agginit;
  gs_code_t code = gs_tree_code(init);
  if (! Use_Static_Init_For_Aggregate (target_st, init))
  {
	agginit.Traverse_Aggregate_Constructor (target, init, 
					gs_tree_type(init),
					gs_tree_type(init),
                                        FALSE /*gen_initv*/, 
                                        0 /*currect_ofst*/, 
                                        0, 0);
	return target_st;
  }
  else {
        // TODO: We do not need to create a temp ST in all cases.
        //  if ST_class(target_st) is FORMAL, we need it indeed.
        DevWarn ("Static initialize %s(%s)\n", 
                 ST_name(target_st), Sclass_Name(ST_sclass(target_st)));
        TY_IDX ty_idx = Get_TY(gs_tree_type(init));
	ST *temp = New_ST (CURRENT_SYMTAB);
	ST_Init (temp,
        	 Save_Str2 (ST_name(target_st), ".init"),
        	 CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL,
        	 ty_idx );
	// setup inito for target_st
	Set_ST_is_initialized(temp);
	agginit.Set_inito(New_INITO(temp));
	WN* temp_target = WN_Lda (Pointer_Mtype, 0, temp, 0);

	agginit.Traverse_Aggregate_Constructor (temp_target, init, 
					gs_tree_type(init),
					gs_tree_type(init),
                                        TRUE /*gen_initv*/, 
                                        0, 
                                        0, 0);
	// following inlined from WGEN_Finish_Aggregate_Init()
	TY_IDX ty = ST_type(temp);
	if (TY_size(ty) == 0 ||
	    (TY_kind(ty) == KIND_ARRAY &&
	     !ARB_const_ubnd (TY_arb(ty)) &&
	     TY_size(ty) <= Get_INITO_Size(agginit.Inito()))) {
	      // e.g. array whose size is determined by init;
	      // fill in with initv size
	      Set_TY_size(ty, Get_INITO_Size(agginit.Inito()));
	      if (TY_kind(ty) == KIND_ARRAY) {
		      Set_ARB_const_ubnd (TY_arb(ty));
		      Set_ARB_ubnd_val (TY_arb(ty), 
			      (TY_size(ty) / TY_size(TY_etype(ty))) - 1 );
	      }
	}
	if (agginit.Last_initv() == 0) {
	  agginit.WGEN_Add_Aggregate_Init_Padding (0);
	}
	return temp;
  }
}
#endif

static gs_t init_decl = NULL;

extern void
WGEN_Initialize_Decl (gs_t decl)
{
  if (gs_decl_ignored_p(decl)
#ifdef KEY
      && !lang_cplus
#endif
     ) {
  	// ignore initialization unless really used
	// e.g. FUNCTION and PRETTY_FUNCTION
	return;
  }
  ST *st = Get_ST(decl);
  gs_t init = gs_decl_initial(decl);
  if (gs_tree_code(init) == GS_VAR_DECL &&
      gs_decl_context(init)	    &&
      gs_tree_code(gs_decl_context(init)) == GS_RECORD_TYPE)
    Get_TY(gs_decl_context(init));

  if (gs_tree_static(decl) || gs_decl_context(decl) == NULL) 
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
		AGGINIT agginit;
		agginit.Add_Inito_For_Tree (init, st);
		while (deferred_decl_init_i >= 0) {
			init_decl = Pop_Deferred_Decl_Init ();
			agginit.Add_Inito_For_Tree (gs_decl_initial(init_decl),
					    Get_ST(init_decl));
		}
		init_decl = NULL;
	}
	if (gs_tree_readonly(decl) && !gs_tree_this_volatile(decl))
		Set_ST_is_const_var (st);
  }
  else {
#ifdef KEY
	if (gs_tree_code(init) == GS_COMPOUND_LITERAL_EXPR)
	  init = gs_decl_initial(gs_tree_operand(gs_tree_operand(init, 0), 0));
#endif
	// mimic an assign
	if (gs_tree_code(init) == GS_CONSTRUCTOR) {
		// is aggregate
		if (Use_Static_Init_For_Aggregate (st, init)) {
			// create inito for initial copy
			// and store that into decl
			ST *copy = WGEN_Generate_Temp_For_Initialized_Aggregate(
					init, ST_name(st));
			WN *init_wn = WN_CreateLdid (OPR_LDID, MTYPE_M, MTYPE_M,
				0, copy, ST_type(copy));
			WGEN_Stmt_Append(
				WN_CreateStid (OPR_STID, MTYPE_V, MTYPE_M,
					0, st, ST_type(st), init_wn),
				Get_Srcpos());
		}
		else {
			// do sequence of stores for each element
			AGGINIT agginit;
#ifdef NEW_INITIALIZER
                        WN *target = WN_Lda (Pointer_Mtype, 0, st, 0);
                        agginit.Traverse_Aggregate_Constructor (target, init, 
#else
			agginit.Traverse_Aggregate_Constructor (st, init, 
#endif
#ifdef KEY
				gs_tree_type(init),
#endif
				gs_tree_type(init),
                                FALSE /*gen_initv*/, 0, 0, 0);
		}
	}
	else {
		INT emitted_bytes;
#ifdef NEW_INITIALIZER
                WN *target = WN_Lda (Pointer_Mtype, 0, st, 0);
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


void
WGEN_Decl (gs_t decl)
{
#ifndef KEY
  if (gs_decl_initial (decl) != 0) return;	// already processed
#endif
  if (gs_decl_ignored_p(decl)) return;
  if (gs_tree_code(decl) != GS_VAR_DECL) return;
#ifndef KEY
  if (gs_decl_context(decl) != 0) return;	// local
  if ( ! gs_tree_public(decl)) return;	// local
#endif
  if ( ! gs_tree_static(decl)) return;	// extern
  // is something that we want to put in symtab
  // (a global var defined in this file).
  (void) Get_ST(decl);
}

void WGEN_Process_Type_Decl (gs_t);
void WGEN_Process_Template_Decl (gs_t);
void WGEN_Process_Var_Decl (gs_t);
void WGEN_Process_Function_Decl (gs_t);
void WGEN_Process_Namespace_Decl (gs_t);
void WGEN_Process_Decl (gs_t);

BOOL
WGEN_Assemble_Alias (gs_t decl, gs_t target)
{
  DevWarn ("__attribute alias encountered at line %d", lineno);
  gs_t base_decl = target;
  // Don't expand alias until the target is expanded, so that we can set st's
  // sclass to base_st's sclass.  This may take more than one iteration since
  // the target can be an alias to another target.  Bug 4393.
  if (!expanded_decl(base_decl) && (lang_cplus || !finish_alias))
  {
    if (!lang_cplus) // KEY bug 12778
      alias_vector.push_back (std::make_pair (decl, target));
    return FALSE; // bugs 12602, 12704
  }
  expanded_decl(decl) = TRUE;

  ST *base_st = Get_ST (base_decl);
  ST *st = Get_ST (decl);
  if (ST_is_weak_symbol(st)) {
    Set_ST_sclass (st, SCLASS_EXTERN);
    Set_ST_strong_idx (*st, ST_st_idx (base_st));
  }
  else {
    Set_ST_base_idx (st, ST_st_idx (base_st));
    Set_ST_emit_symbol(st);	// for cg
    if (ST_is_initialized (base_st)) {
      Set_ST_is_initialized (st);
      // bug924 open64.net. global alias symbol with base initialized
      // should set storage class SCLASS_DGLOBAL.
      if (ST_sclass(st) == SCLASS_COMMON || ST_sclass(st) == SCLASS_UGLOBAL) 
        Set_ST_sclass (st, SCLASS_DGLOBAL);
    }
#ifdef KEY
    if (ST_init_value_zero (base_st)) {
      Set_ST_init_value_zero (st);
      // bug924 open64.net. Those base initialized zero
      // symbols should restore to SCLASS_UGLOBAL.
      if (ST_sclass(st) == SCLASS_DGLOBAL)
        Set_ST_sclass (st, SCLASS_UGLOBAL);
    }
#endif
    // bug924 open64.net aliased symbols in COMMON section should be 
    // set to be uninitialized. Since aliased symbols themselves 
    // should never be allocated.
    if (ST_sclass(st) == SCLASS_COMMON) {
      Set_ST_sclass (st, SCLASS_UGLOBAL);
    }
  }
#ifdef KEY
  if (!lang_cplus)
  {
    if (ST_sym_class (st) != ST_sym_class (base_st)) {
      /* open64.net bug 878, change the aliased sym class and type to the base st */
      ErrMsg (EC_Ill_Alias, ST_name (st), ST_name (base_st));
      Set_ST_class(st, ST_class(base_st));
      Set_ST_type(st,ST_type(base_st));
    }

    // bugs 5145, 11993
    if (ST_sym_class (base_st) == CLASS_FUNC)
      Set_PU_no_delete (Pu_Table [ST_pu (base_st)]);
  }

  return TRUE;
#endif
/*
  if (ST_is_initialized (base_st)) {
    Set_ST_is_initialized (st);
    if (ST_init_value_zero (base_st))
      Set_ST_init_value_zero (st);
  }
*/
} /* WGEN_Assemble_Alias */


ST *
WGEN_Get_Return_Address_ST (int level)
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
} /* WGEN_Get_Return_Address_ST */

ST *
WGEN_Alloca_0 (void)
{
  WN *wn;
  TY_IDX ty_idx = Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE);
  ST* alloca_st = Gen_Temp_Symbol (ty_idx, "__alloca");
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, alloca_st);
#endif
  wn = WN_CreateAlloca (WN_CreateIntconst (OPC_I4INTCONST, 0));
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ty_idx, wn);
  WGEN_Stmt_Append (wn, Get_Srcpos());
  Set_PU_has_alloca (Get_Current_PU ());
  return alloca_st;
} /* WGEN_Alloca_0 */

ST *
WGEN_Alloca_ST (gs_t decl)
{
  ST *st = Create_ST_For_Tree (decl);
  ST *alloca_st = New_ST (CURRENT_SYMTAB);
  ST_Init (alloca_st, Save_Str (ST_name (st)),
           CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
           Make_Pointer_Type (ST_type (st), FALSE));
  Set_ST_is_temp_var (alloca_st);
  Set_ST_pt_to_unique_mem (alloca_st);
  Set_ST_base_idx (st, ST_st_idx (alloca_st));
  WN *swn = WGEN_Expand_Expr (gs_type_size_unit(gs_tree_type(decl)));
  WN *wn  = WN_CreateAlloca (swn);
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos());
  return st;
} /* WGEN_Alloca_ST */

void
WGEN_Dealloca (ST * alloca_st, vector<ST*> * vars)
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
  WGEN_Stmt_Append (wn, Get_Srcpos());
}

void
WGEN_Resolve_Duplicate_Decls (gs_t olddecl, gs_t newdecl)
{
  ST     *st      = DECL_ST(olddecl);
  gs_t    newtype = gs_tree_type(newdecl);
  gs_t    newsize = gs_type_size(newtype);
  TY_IDX  ty      = ST_type (st);

  if (gs_tree_static(olddecl) == FALSE &&
      gs_tree_static(newdecl) == TRUE  &&
      gs_tree_public(olddecl) == TRUE  &&
      gs_tree_public(newdecl) == FALSE) {
    Set_ST_sclass (st, SCLASS_FSTATIC);
    Set_ST_export (st, EXPORT_LOCAL);
  }

  if (newsize                           &&
      gs_tree_code(newsize) == GS_INTEGER_CST &&
      TY_size (ty) <= gs_get_integer_value (newsize) / BITSPERBYTE) {
    UINT64 size = gs_get_integer_value (newsize) / BITSPERBYTE;
    Set_TY_size (ty, size);
    if (TY_kind (ty) == KIND_ARRAY) {
      Set_ARB_const_ubnd (TY_arb(ty));
      Set_ARB_ubnd_val (TY_arb(ty), (size / TY_size(TY_etype(ty))) - 1);
    }
  } 
} /* WGEN_Resolve_Duplicate_Decls */

// For each decl in the weak decls list, mark its ST weak.
extern "C" void
WGEN_Weak_Finish ()
{
  gs_t weak_decls = gs_weak_decls(program);

  gs_t list;
  for (list = weak_decls; gs_code(list) != EMPTY; list = gs_operand(list, 1)) {
    gs_t decl = gs_operand(list, 0);
    if (!gs_tree_used (decl))
      continue; // bug 11329
    ST *st = DECL_ST (decl);
    // bug 11329: If we have not yet expanded this decl, we should not need
    // it. But as bug 11516 shows, we should expand it if it has a target
    // (i.e. a strong definition).
    if (!st && !gs_decl_alias_target(decl))
      continue;
    else if (!st)
      st = Get_ST (decl);
    if (ST_base_idx(st) != ST_st_idx(st)) {
      Set_ST_is_weak_symbol(st);
      continue;
    }
    // Have to set it after checking ST_base_idx(st) != ST_st_idx(st)
    Set_ST_is_weak_symbol(st);
    if (gs_decl_alias_target(decl)) {
      ST *base_st = DECL_ST(gs_decl_alias_target(decl));
      if (ST_is_weak_symbol(st) && ST_sclass(st) != SCLASS_EXTERN)
	Clear_ST_is_weak_symbol (st);
      else if (base_st)
	Set_ST_strong_idx (*st, ST_st_idx(base_st));
    }
  }
} // WGEN_Weak_Finish 



void
WGEN_Process_Class_Decl (gs_t decl)
{
//fprintf(stderr, "CLASS_DECL: %s\n", IDENTIFIER_POINTER(gs_decl_name(decl)));

  if (TYPE_TY_IDX(decl))
    return;

  TYPE_TY_IDX(decl) = MTYPE_B;

  gs_t  binfo     = gs_type_binfo(decl);
  gs_t  basetypes = binfo ? gs_binfo_base_binfos(binfo) : 0;

  if (basetypes) {
    gs_t list;
    for (list = basetypes; gs_code(list) != EMPTY; list = gs_operand(list, 1))
      (void) WGEN_Process_Class_Decl (gs_binfo_type(gs_operand(list, 0)));
  }

  gs_t  field;
  for (field = get_first_real_or_virtual_field (decl);
    field != NULL;
    field = next_real_field (decl, field)) {
    if (gs_tree_code(field) == GS_TYPE_DECL) {
      gs_t field_type = gs_tree_type(field);
      if (field_type &&
          gs_tree_code(field_type) == GS_RECORD_TYPE &&
          field_type != decl) {
        WGEN_Process_Class_Decl (field_type);
      }
    } 
  }

  gs_t method = gs_type_methods(decl);
  while (method != NULL) {
    if (gs_tree_code(method) == GS_FUNCTION_DECL) {
      gs_t body = gs_decl_saved_tree(method);
      if (body != NULL && !gs_decl_external(method) &&
          !gs_decl_weak(method) &&
          !gs_decl_inline(method) &&
          DECL_ST(method) == NULL &&
          (gs_decl_template_info(method) == NULL              ||
          gs_decl_friend_pseudo_template_instantiation(method) ||
          gs_decl_template_instantiated(method)              ||
          gs_decl_template_specialization(method))) {
          WGEN_Process_Function_Decl (method);
      }
    }
    method = gs_tree_chain(method);
  }
} /* WGEN_Process_Class_Decl */

void
WGEN_Process_Type_Decl (gs_t decl)
{
} /* WGEN_Process_Type_Decl */

void
WGEN_Process_Template_Decl (gs_t decl)
{
//fprintf(stderr, "TEMPLATE_DECL: %s\n", IDENTIFIER_POINTER(gs_decl_name(decl)));
  gs_t gentemp = gs_most_general_template(decl);
  for (gs_t t = gs_decl_template_instantiations(gentemp);
       t; t = gs_tree_chain(t)) {
    gs_t val = gs_tree_value(t);
    if (gs_tree_code(val) == GS_RECORD_TYPE &&
        !gs_uses_template_parms(val))
      WGEN_Process_Class_Decl (val);
  }
} /* WGEN_Process_Template_Decl */

#ifdef KEY
// Return TRUE if DECL is a needed VTT (virtual table table).
bool
decl_is_needed_vtt (gs_t decl)
{
  bool needed = false;

  // Assume all VTTs are needed.
  if (gs_decl_name(decl) &&
      gs_identifier_pointer(gs_decl_name(decl)) &&
      !strncmp("_ZTT", gs_identifier_pointer(gs_decl_name(decl)), 4)) {
    needed = true;
  }
  return needed;
}
#endif

bool
decl_is_needed_vtable (gs_t decl)
{
  bool needed = false;
  if (gs_decl_name(decl) &&
      gs_identifier_pointer(gs_decl_name(decl)) &&
#ifdef KEY
      !strncmp("_ZTV", gs_identifier_pointer(gs_decl_name(decl)), 4)
#else
      !strncmp("__vt_", gs_identifier_pointer(gs_decl_name(decl)), 5)
#endif
     ) {
            
#ifdef FE_GNU_4_2_0
    INT count = gs_constructor_length (gs_decl_initial (decl));
    for (INT idx = 0;
         idx < count;
         idx++)
#else
    gs_t entries = gs_constructor_elts (gs_decl_initial (decl));

    for (; entries; entries = gs_tree_chain (entries))
#endif
    {

      gs_t fnaddr;
      gs_t fn;

#ifdef FE_GNU_4_2_0
      fnaddr = gs_constructor_elts_value (gs_decl_initial (decl), idx);
#else
      fnaddr = gs_tree_value (entries);
#endif

#ifdef KEY
      if (gs_tree_code (fnaddr) == GS_NOP_EXPR &&
	  gs_tree_code (gs_tree_operand (fnaddr, 0)) == GS_ADDR_EXPR) {
	fn = gs_tree_operand (gs_tree_operand (fnaddr, 0), 0);  // fn can be VAR_DECL
	
      } else if (gs_tree_code (fnaddr) != GS_ADDR_EXPR) {
        /* This entry is an offset: a virtual base class offset, a
           virtual call offset, and RTTI offset, etc.  */
        continue;
      } else
        fn = gs_tree_operand (fnaddr, 0);
#else
      if (gs_tree_code (fnaddr) != GS_ADDR_EXPR)
        /* This entry is an offset: a virtual base class offset, a
           virtual call offset, and RTTI offset, etc.  */
        continue;

      fn = gs_tree_operand (fnaddr, 0);
#endif

#ifdef KEY
      // As shown by bug 3133, some objects are emitted by g++ even though they
      // are weak and external.
      if (gs_decl_emitted_by_gxx(fn)) {
	needed = TRUE;
	break;
      }
#endif

      if (!gs_decl_external(fn) &&
          !gs_decl_weak(fn)
#ifndef KEY	// Under g++ 3.2 -O3, all functions are marked DECL_INLINE.
          && !gs_decl_inline(fn)
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
WGEN_Process_Var_Decl (gs_t decl)
{
//fprintf(stderr, "VAR_DECL: %s\n", IDENTIFIER_POINTER(gs_decl_name(decl)));
  ST *st;
  if (gs_tree_public(decl)    &&
//    !gs_decl_weak(decl)     &&
      !gs_decl_external(decl)
      ) {
    if (!gs_decl_weak(decl)
	|| decl_is_needed_vtable (decl)
#ifdef KEY
	|| decl_is_needed_vtt (decl)	// Bug 7442.
#endif
        ) {
#ifdef KEY
      WGEN_Expand_Decl(decl, TRUE);
#else
      DECL_ST(decl) = (ST *) 1;
      Push_Deferred_Function (decl);
#endif
    }
  }
} /* WGEN_Process_Var_Decl */

void
WGEN_Process_Function_Decl (gs_t decl)
{
//fprintf(stderr, "FUNCTION_DECL: %s\n", IDENTIFIER_POINTER(gs_decl_name(decl)));
  gs_t body;
  ST *st;
  body = gs_decl_saved_tree(decl);
  if (body != NULL && !gs_decl_external(decl) &&
      !gs_decl_artificial(decl) &&
      !gs_decl_inline(decl) &&
      DECL_ST(decl) == NULL &&
      (gs_decl_template_info(decl) == NULL              ||
      gs_decl_friend_pseudo_template_instantiation(decl) ||
      gs_decl_template_instantiated(decl)              ||
      gs_decl_template_specialization(decl))) {
#ifdef KEY
    set_DECL_ST(decl, (ST *) 1);
#else
    DECL_ST(decl) = (ST *) 1;
#endif
    Push_Deferred_Function (decl);
  }
} /* WGEN_Process_Function_Decl */

void
WGEN_Process_Decl (gs_t decl)
{
  switch (gs_tree_code (decl)) {

    case GS_NAMESPACE_DECL:
      WGEN_Process_Namespace_Decl (decl);
      break;

    case GS_CONST_DECL:
      break;

    case GS_TYPE_DECL:
      WGEN_Process_Type_Decl (decl);
      break;

    case GS_TEMPLATE_DECL:
      WGEN_Process_Template_Decl (decl);
      break;

    case GS_VAR_DECL:
      WGEN_Process_Var_Decl (decl);
      break;

    case GS_FUNCTION_DECL:
      WGEN_Process_Function_Decl (decl);
      break;

    default:
      break;
  }
} /* WGEN_Process_Decl */

void
WGEN_Process_Namespace_Decl (gs_t namespace_decl)
{
} /* WGEN_Process_Namespace_Decl */

extern "C"
void
WGEN_Expand_Top_Level_Decl (gs_t top_level_decl)
{
  int error_count, sorry_count;
#ifdef KEY
  gs_t old_namespace_decl = curr_namespace_decl;
  curr_namespace_decl = top_level_decl;
#endif


  if (!Enable_WFE_DFE) {
    // Emit asm statements at global scope, before expanding the functions,
    // to prevent them from getting into wrong sections (e.g. .except_table)
    static BOOL gxx_emitted_asms_expanded = FALSE;

    gs_t list;
    // This check is needed for C, because for C this function can be
    // called multiple times.
    if (!gxx_emitted_asms_expanded) {
      gs_t gxx_emitted_asms_list = gs_gxx_emitted_asms(program);

      for (list = gxx_emitted_asms_list; gs_code(list) != EMPTY; 
    	   list = gs_operand(list, 1)) {
        char *asm_string = gs_s(gs_operand(list, 0));
        WGEN_Assemble_Asm (asm_string);
      }
      gxx_emitted_asms_expanded = TRUE;
    }

    // No decls or all decls have been expanded
    if ( gs_code(top_level_decl) == EMPTY )
      return;

    WGEN_Expand_Decl (top_level_decl, TRUE);

#ifdef KEY
    if (!lang_cplus) {
      curr_namespace_decl = old_namespace_decl;
      return;
    }

    // Catch all the functions that are emitted by g++ that we haven't
    // translated into WHIRL.
    int changed;
    do {
      gs_t gxx_emitted_decls_list = gs_gxx_emitted_decls(program);
      changed = 0;

      for (list = gxx_emitted_decls_list; gs_code(list) != EMPTY;
      	   list = gs_operand(list, 1)) {
	gs_t decl = gs_operand(list, 0);
        if (expanded_decl(decl) == TRUE)
          continue;
        if (gs_tree_code(decl) == GS_FUNCTION_DECL) {
	  if (gs_decl_thunk_p(decl))
	    WGEN_Generate_Thunk(decl);
	  else if (gs_decl_alias_target(decl))	// Bug 4393.
	    changed |= WGEN_Assemble_Alias(decl, gs_decl_alias_target(decl));
	  else        
	    changed |= WGEN_Expand_Function_Body(decl);
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
	    gs_t context = gs_decl_context(decl);
	    if (context && gs_tree_code(context) == GS_RECORD_TYPE) {
	      // member function
	      // g++ seems to put the artificial ones in the output too 
	      // for some reason. In particular, operator= is there.  We do 
	      // want to omit the __base_ctor stuff though
	      BOOL skip = FALSE;
	      if (gs_identifier_ctor_or_dtor_p(gs_decl_name(decl)))
	        skip = TRUE;
	      else if (gs_decl_thunk_p(decl)) {
		// Skip thunk to constructors and destructors.  Bug 6427.
		gs_t target_func = gs_thunk_target(decl);
		Is_True (gs_tree_code(target_func) == GS_FUNCTION_DECL,
		         ("WGEN_Expand_Top_Level_Decl: invalid thunk decl"));
		skip =
		  gs_identifier_ctor_or_dtor_p(gs_decl_name(target_func));
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
        } else if (gs_tree_code(decl) == GS_VAR_DECL) {
	  WGEN_Process_Var_Decl (decl);
        } else if (gs_tree_code(decl) == GS_NAMESPACE_DECL) {
	  WGEN_Expand_Decl (decl, TRUE);
        } else {
	  FmtAssert(FALSE, ("WGEN_Expand_Top_Level_Decl: invalid node"));
        }
      }
    } while (changed);	// Repeat until emitted all needed copy constructors.

    // Emit any typeinfos that we have referenced
    std::vector<gs_t>::iterator it;
    for (it = emit_typeinfos.begin(); it != emit_typeinfos.end(); ++it) {
    	gs_t decl = *it;
	if (expanded_decl (decl))
	    continue;
	expanded_decl (decl) = TRUE;
	FmtAssert (gs_tree_code(decl) == GS_VAR_DECL, ("Unexpected node in typeinfo"));
	WGEN_Expand_Decl (decl, TRUE);
    }
#endif

  } else {

    WGEN_Process_Namespace_Decl (top_level_decl);

    gs_t  decl;
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
      if (gs_tree_code(decl) == GS_FUNCTION_DECL)
        if (gs_decl_thunk_p(decl))
          WGEN_Generate_Thunk(decl);
        else        
          WGEN_Expand_Function_Body (decl);
      else {
        st = DECL_ST(decl);
        if (ST_sclass(st) == SCLASS_EXTERN)
          Set_ST_sclass (st, SCLASS_UGLOBAL);
        WGEN_Expand_Decl (decl, TRUE);
      }
    }
  }
#ifdef KEY
  curr_namespace_decl = old_namespace_decl;
#endif
} /* WGEN_Expand_Top_Level_Decl */

#ifdef KEY
extern "C"
void
WGEN_Expand_Defers()
{
  {
    int i;
    // Can't use iterator to access emit_decls because Get_TY may grow
    // emit_decls, which invalids all iterators.  Use operator[] instead.
    for (i = 0; i < emit_decls.size(); i++) {
      gs_t decl = emit_decls[i];
      gs_code_t code = gs_tree_code(decl);
      if (code == GS_VAR_DECL)
	WGEN_Expand_Decl(decl, TRUE);
      else if (code == GS_RECORD_TYPE ||
	       code == GS_UNION_TYPE)
	Get_TY(decl);
      else
	Is_True(FALSE, ("WGEN_Expand_Top_Level_Decl: unexpected tree type"));
    }
  }

  {
    // Set the type for fields whose type we want to set last.
    // Don't use iterator because defer_fields may grow.
    for (int i = 0; i < defer_fields.size(); i++) {
      gs_t field = defer_fields[i].first;
      FLD_HANDLE fld = defer_fields[i].second;
      Is_True(gs_tree_code(field) == GS_FIELD_DECL,
	      ("WGEN_Expand_Top_Level_Decl: FIELD_DECL not found"));
      // Currently we defer only pointer types.
      Is_True(gs_tree_code(gs_tree_type(field)) == GS_POINTER_TYPE,
	      ("WGEN_Expand_Top_Level_Decl: POINTER_TYPE not found"));
      TY_IDX fty_idx = Get_TY(gs_tree_type(field));
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
	gs_t context = p->u1.member_func.context;
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
}

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
//
// The above is obsoleted by THUNK_TARGET in GCC 4.0, which directly
// gives the function-decl that the thunk transfers control to. Note: 
// THUNK_TARGET may itself be a thunk.
static inline gs_t
WGEN_get_thunk_target (gs_t decl)
{
  Is_True (gs_thunk_target(decl) &&
           gs_tree_code(gs_thunk_target(decl)) == GS_FUNCTION_DECL,
           ("WGEN_get_thunk_target: invalid thunk target"));
  return gs_thunk_target(decl);
}

// The target of a thunk may itself be a thunk. This function returns the
// final target that is not a thunk. We need the non-thunk target to find
// the arguments that it needs, so that the thunks can pass each of them
// forward.
static inline gs_t
WGEN_get_final_thunk_target (gs_t decl)  // KEY
{
  gs_t target = gs_thunk_target(decl);

  Is_True (target && gs_tree_code(target) == GS_FUNCTION_DECL,
           ("WGEN_get_final_thunk_target: invalid thunk target"));
  while (gs_decl_thunk_p(target))
  {
    target = gs_thunk_target(target);
    Is_True (target && gs_tree_code(target) == GS_FUNCTION_DECL,
             ("WGEN_get_final_thunk_target: invalid thunk target"));
  }

  return target;
}


// If g++ performed named return value (nrv) optimization on the function FN,
// then modify the gnu tree to reflect the optimization.  That way, the WHIRL
// generated from the tree will automatically include the optimization.
static void
WGEN_Handle_Named_Return_Value (gs_t fn)
{
  named_ret_obj_initializer = NULL;

  // Quit if the nrv opt was not done to this function.  If there is nrv,
  // current_function_return_value is the VAR_DECL of the local object to be
  // returned.
  gs_t named_ret_obj = gs_decl_named_return_object(fn);

  // GNU replaces named_ret_obj by actual_ret_obj throughout the function.
  // This ensures anything computed into the former is already written into
  // actual_ret_obj, which should be the first fake parameter.
  // We won't do anything with the local named_ret_obj object here, it is
  // used to only generate debugging information.
  gs_t actual_ret_obj = gs_decl_result(fn);
  if (named_ret_obj == NULL)
    return;

  FmtAssert(gs_tree_code(named_ret_obj) == GS_VAR_DECL,
	 ("WGEN_Handle_Named_Return_Value: named return object not a VAR_DECL"));
  FmtAssert(actual_ret_obj && gs_tree_code(actual_ret_obj) == GS_RESULT_DECL,
         ("WGEN_Handle_Named_Return_Value: return object not a RESULT_DECL"));

  // Even though we won't use the variable, record its existence in the symbol
  // table in order to generate DWARF for it.  Bug 4900.
  Get_ST(named_ret_obj);

  TY_IDX ret_ty_idx = Get_TY(gs_tree_type(gs_tree_type(fn)));

  // It is possible that g++ does nvr for complex type.
  // We won't do anything in such case, since lowering phase will handle it
  // according to ABI.
  if (!TY_return_in_mem(ret_ty_idx)) return;
  
  // Get the ST for the fake first parm.
  WN *first_formal = WN_formal(Current_Entry_WN(), 0);

  // Change the return object's RESULT_DECL node to be an INDIRECT_REF of
  // the fake first parm, so that whenever the RESULT_DECL is accessed, it will
  // access the return area.  If the RESULT_DECL had an initializer, create a
  // TARGET_EXPR to initialize the indirect ref with this initializer.
  if (gs_decl_initial(actual_ret_obj)) {
    // wgen TODO: initializer of RESULT_DECL not tested.
    named_ret_obj_initializer = gs_build_target_expr(
				      gs_tree_type(actual_ret_obj),
				      actual_ret_obj,
				      gs_decl_initial(actual_ret_obj),
				      NULL);
    gs_t flags = gs_operand(named_ret_obj_initializer, GS_FLAGS);
    _gs_bv (flags, GS_TREE_SIDE_EFFECTS, 1);
  }

  gs_t ptr_var = gs_build_decl(GS_RESULT_DECL,
			gs_build_pointer_type(gs_tree_type(gs_tree_type(fn))));
  _gs_code(actual_ret_obj, GS_INDIRECT_REF);
  gs_set_tree_operand(actual_ret_obj, 0, ptr_var);
  set_DECL_ST(ptr_var, WN_st(first_formal));

  // Bug 4900 - set the location attribute for the DST entry for named_ret_obj
  // to point to first_formal; and also set DW_OP_deref for the DST entry.
  if (Debug_Level >= 2) {
    DST_INFO_IDX info_idx = DECL_DST_IDX(named_ret_obj);
    DST_ATTR_IDX attr_idx = DST_INFO_attributes(DST_INFO_IDX_TO_PTR(info_idx));
    DST_VARIABLE *attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);
    DST_ASSOC_INFO_fe_ptr(DST_VARIABLE_def_st(attr)) = 
      (void *)(INTPTR)ST_st_idx(WN_st(first_formal));
    DST_SET_deref(DST_INFO_flag( DST_INFO_IDX_TO_PTR(info_idx)));
  }
}

extern "C" void
WGEN_Alias_Finish (void)
{
  finish_alias = TRUE;
  for (INT i=0; i<alias_vector.size(); i++)
    WGEN_Assemble_Alias (alias_vector[i].first, alias_vector[i].second);
  alias_vector.clear ();
}
#endif
