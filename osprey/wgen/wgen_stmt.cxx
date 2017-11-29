/*
 * Copyright (C) 2009-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007, 2008, 2009 PathScale, LLC.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

////////////////////////////////////////////////////////////////////////////////
//
// Copyright 2006 PathScale, Inc. All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
//
// Further, this software is distributed without any warranty that it is
// free of the rightful claim of any third person regarding infringement 
// or the like.  Any license provided herein, whether implied or 
// otherwise, applies only to this software file.  Patent licenses, if 
// any, provided herein do not apply to combinations of this program with 
// other software, or any other product whatsoever.  
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write the Free Software Foundation, Inc., 59
// Temple Place - Suite 330, Boston MA 02111-1307, USA.
////////////////////////////////////////////////////////////////////////////////

extern "C"{
#include "gspin-wgen-interface.h"
}



#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"

#include "srcpos.h"
#include "wgen_misc.h"
#include "wgen_dst.h"
#include "ir_reader.h"
#include "wgen_expr.h"
#include "wgen_stmt.h"
#include "wgen_decl.h"
#include "wgen_spin_symbol.h"
#include "wgen_tracing.h"
#include "targ_sim.h"
#include <ctype.h>
//#include "tree_cmp.h"

#include "wn.h"		// New_Region_Id()
#include "const.h"
int make_symbols_weak = FALSE;	// if TRUE, emit all new symbols as weak
bool in_cleanup = FALSE;	// TRUE if we are expanding code to be executed during stack unwinding

extern "C" {
extern int gs_decode_reg_name (const char *asmspec);
}

extern char *WGEN_Tree_Node_Name(gs_t exp);
#define ENLARGE(x) (x + (x >> 1))

static BOOL  *if_else_info_stack;
static INT32  if_else_info_i;
static INT32  if_else_info_max;

typedef struct case_info_t {
  INT64     case_lower_bound_value;
  INT64     case_upper_bound_value;
  LABEL_IDX case_label_idx;
} CASE_INFO;

static CASE_INFO   *case_info_stack;
static INT32        case_info_i;
static INT32        case_info_max;


typedef struct switch_info_t {
  WN        *index;
  INT32      start_case_index;
  LABEL_IDX  default_label_idx;
} SWITCH_INFO;

static SWITCH_INFO *switch_info_stack;
static INT32        switch_info_i;
static INT32        switch_info_max;

typedef struct break_continue_info_t {
  gs_code_t       tree_code;
  LABEL_IDX break_label_idx;
  LABEL_IDX continue_label_idx;
  gs_t	    scope;
} BREAK_CONTINUE_INFO;

static BREAK_CONTINUE_INFO *break_continue_info_stack;
static INT32		    break_continue_info_i;
static INT32		    break_continue_info_max;

typedef struct label_info_t {
  LABEL_IDX         label_idx;
  unsigned char     symtab_idx;
  unsigned char     defined;
} LABEL_INFO;

static LABEL_INFO  *undefined_labels_stack;
static INT32        undefined_labels_i;
static INT32        undefined_labels_max;

// cmp_idx: for a try_block, it is the label where the first compare
//          for the handlers of this try-block should start.
typedef struct scope_cleanup_info_t {
  gs_t		    stmt;
  LABEL_IDX	    label_idx;
  LABEL_IDX	    cmp_idx;
  bool		    cleanup_eh_only;
  struct vla_ {
    bool		    has_alloca;
    ST * 		    alloca_st;
    vector<ST*>	*	    alloca_sts_vector;
  } vla;
} SCOPE_CLEANUP_INFO;

static SCOPE_CLEANUP_INFO *scope_cleanup_stack;
static INT32	    	   scope_cleanup_i;
static INT32	    	   scope_cleanup_max;

static gs_t	   *scope_stack;
static INT32	    scope_i;
static INT32	    scope_max;

typedef struct temp_cleanup_info_t {
  gs_t		    expr;
  LABEL_IDX	    label_idx;
#ifdef KEY
  bool		    cleanup_eh_only;
#endif
} TEMP_CLEANUP_INFO;

static TEMP_CLEANUP_INFO *temp_cleanup_stack;
static INT32	    	  temp_cleanup_i;
static INT32	    	  temp_cleanup_max;

#ifdef KEY
#include <vector>
#include <algorithm>
#include <functional>
#include <list>
#include <stack>
typedef struct handler_info_t {
  gs_t		    handler;
  vector<gs_t>	    *cleanups;
  vector<SCOPE_CLEANUP_INFO> *scope;
  vector<TEMP_CLEANUP_INFO> *temp_cleanup;
  vector<BREAK_CONTINUE_INFO> *break_continue;
  vector<ST_IDX>    *handler_list; // list of handlers outside this try-catch block
  vector<ST_IDX>    *eh_spec; // eh_spec of the containing region to be used while inside its handler
  LABEL_IDX	    label_idx;
  // cmp_idx: 1st is the label where the first cmp for this handler set 
  // should start. If the 2nd label is non-zero it must be marked 
  // handler_begin
  LABEL_IDX	    cmp_idx[2];
  // label to jmp to after this set of handlers are done
  LABEL_IDX 	    goto_idx;
  LABEL_IDX 	    cleanups_idx;
  bool		    outermost; // handler for outermost try block in PU?
} HANDLER_INFO;

// wgen TODO: This has always one entry, verify this, and replace it with a
// single variable.
std::stack<HANDLER_INFO> handler_stack; // formed from handler_info_stack in Do_Handlers
#else
typedef struct handler_info_t {
  gs_t		    handler;
  LABEL_IDX	    label_idx;
} HANDLER_INFO;
#endif // KEY

static HANDLER_INFO *handler_info_stack;
static INT32	     handler_info_i;
static INT32	     handler_info_max;

#ifdef KEY

bool processing_handler = false;
bool try_block_seen;
typedef struct eh_cleanup_entry {
  gs_t		     tryhandler;	// just for comparison, at present
  vector<gs_t>	     *cleanups;	// emit
  LABEL_IDX	     pad;	// emit
  LABEL_IDX	     start;	// emit after pad and before cleanups
  LABEL_IDX	     goto_idx;  // emit a goto
} EH_CLEANUP_ENTRY;

static std::list<EH_CLEANUP_ENTRY> cleanup_list_for_eh;

class TYPE_FILTER_ENTRY {
  public:
  ST_IDX		st;	// typeinfo
  int			filter;	// action record filter
  friend bool operator== (const TYPE_FILTER_ENTRY&, const TYPE_FILTER_ENTRY&);
};

inline bool operator==(const TYPE_FILTER_ENTRY& x, const TYPE_FILTER_ENTRY& y)
{
	return x.st == y.st;
}

static vector<TYPE_FILTER_ENTRY>	type_filter_vector;
struct cmp_types : 
	public std::binary_function<const TYPE_FILTER_ENTRY, 
				const TYPE_FILTER_ENTRY, bool>
	{
	bool operator () (const TYPE_FILTER_ENTRY &e1, 
			const TYPE_FILTER_ENTRY &e2)
	{
		return (e1.st < e2.st);
	}
};
static vector<ST_IDX>		eh_spec_vector;
// eh_spec_vector is stored into eh_spec_func_end for function-end processing
static vector<ST_IDX>		eh_spec_func_end;
static int current_eh_spec_ofst=1;
static void Do_Cleanups_For_EH (INT =0);
static INITV_IDX lookup_handlers (vector<gs_t> * =0);
static void Generate_unwind_resume (void);

static void WGEN_fixup_target_expr (gs_t retval);

// If non-zero, don't use label indexes less than or equal to this.
LABEL_IDX WGEN_unusable_label_idx;

// The last label index allocated.
LABEL_IDX WGEN_last_label_idx;
#endif // KEY

// Iterator for C++ exception handlers
class HANDLER_ITER {
  gs_t handler;
  INT index;
  INT num_handlers;

  public:
   HANDLER_ITER (gs_t h) : handler(h)
   {
     Is_True (gs_tree_code(h) == GS_HANDLER ||
              gs_tree_code(h) == GS_STATEMENT_LIST,
              ("HANDLER_ITER::HANDLER_ITER: Unexpected handler code"));
     if (gs_tree_code(handler) == GS_HANDLER)
       num_handlers = 1;
     else
       num_handlers = gs_length (gs_statement_list_elts (handler));
   }

   void First (void) { index = 0; }
   void Next (void) { index++; }
   gs_t Current (void) const
   {
     if (gs_tree_code(handler) == GS_HANDLER)
       if (index == 0)
       {
         Is_True (gs_tree_code(handler) == GS_HANDLER,
                  ("HANDLER_ITER::Current(): Expected GS_HANDLER"));
         return handler;
       }
       else
         return NULL;

     // list of handlers
     if (index < num_handlers)
     {
       gs_t h = gs_index(gs_statement_list_elts(handler), index);
       Is_True (gs_tree_code(h) == GS_HANDLER,
                  ("HANDLER_ITER::Current(): Expected GS_HANDLER"));
       return h;
     }
     else
       return NULL;
   }

   BOOL Not_Empty (void) const { return Current() != NULL; }
};

static INT32	    scope_number;

TY_IDX
Type_For_Function_Returning_Void (void)
{
  static TY_IDX result = 0;
  if (result == 0) {
    TY &ty = New_TY (result);
    TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    TYLIST_IDX tylist_idx;
    Set_TYLIST_type (New_TYLIST (tylist_idx), Void_Type);
    Set_TY_tylist (ty, tylist_idx);
#ifdef KEY
    Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
#endif
  }
  
  return result;
}

static ST *
Function_ST_For_String (const char * s)
{
  ST * st = New_ST (GLOBAL_SYMTAB);
  PU_IDX pu_idx;
  PU & pu = New_PU (pu_idx);
  PU_Init (pu, Type_For_Function_Returning_Void (), GLOBAL_SYMTAB + 1);
  ST_Init (st, Save_Str(s),
           CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
  return st;
}

// May be emit cleanup in I-th entry of stack depending on the statement
// in the entry. Called when we are emitting a handler. This function
// is called when the decision whether to emit a cleanup and how to emit it
// depends on the type of statement.
// Wgen TODO: This function probably needs to be called from a few other
// places also, they will be added as the need arises.
static inline void
Maybe_Emit_Handler_Cleanup(INT i)
{
  HANDLER_INFO hi = handler_stack.top();
  gs_t stmt = (*hi.scope) [i].stmt;
  
  if (gs_tree_code(stmt) == GS_CLEANUP_STMT &&
      !(*hi.scope) [i].cleanup_eh_only)
    WGEN_One_Stmt_Cleanup (gs_cleanup_expr(stmt));
  else if (gs_tree_code(stmt) == GS_TRY_FINALLY_EXPR)
    WGEN_One_Stmt_Cleanup (gs_tree_operand(stmt, 1));
}

static INT32
Maybe_Emit_Cleanups (gs_t sentinel)
{
  // Emit all the cleanups required at a transfer of control out of
  // the current scope, e.g. a goto or return.   Note, after this function,
  // we continue in the same scope, so we should not actually pop the
  // cleanups, but for correct processing we must temporarily pop them as we
  // emit them.
  // Therefore, pop them to a temporary stack, then restore them again
  // afterwards.
  INT32 result;
  std::stack<SCOPE_CLEANUP_INFO> saved_cleanups;

  while (scope_cleanup_i >= 0 &&
         scope_cleanup_stack[scope_cleanup_i].stmt != sentinel) {
    SCOPE_CLEANUP_INFO scope_cleanup = scope_cleanup_stack [scope_cleanup_i];
    --scope_cleanup_i;
    saved_cleanups.push(scope_cleanup);
    gs_t stmt = scope_cleanup.stmt;
    if (gs_tree_code(stmt) == GS_CLEANUP_STMT &&
        !scope_cleanup.cleanup_eh_only)
      WGEN_One_Stmt_Cleanup (gs_cleanup_expr(stmt));
    else if (gs_tree_code(stmt) == GS_TRY_FINALLY_EXPR)
      WGEN_One_Stmt_Cleanup (gs_tree_operand(stmt, 1));
  }

  result = scope_cleanup_i;

  while (! saved_cleanups.empty ()) {
    scope_cleanup_stack[++scope_cleanup_i] = saved_cleanups.top ();
    saved_cleanups.pop ();
  }

  return result;
}


static void
Emit_Cleanup(gs_t cleanup)
{
  int saved_make_symbols_weak;

  saved_make_symbols_weak = make_symbols_weak;
  make_symbols_weak = TRUE;
  if (gs_tree_code(cleanup) == GS_IF_STMT) {
    // Mimick WGEN_Expand_If but don't call it, because WGEN_Expand_If calls
    // WGEN_Expand_Stmt which creates temp cleanups.  This leads to infinite
    // loop.
    FmtAssert(gs_then_clause(cleanup) != NULL,
	      ("Emit_Cleanup: then clause should be non-null"));
    FmtAssert(gs_else_clause(cleanup) == NULL,
	      ("Emit_Cleanup: else clause should be null"));
    WN *test = WGEN_Expand_Expr_With_Sequence_Point (gs_if_cond(cleanup),
						    Boolean_type);
    WN *then_block = WN_CreateBlock();
    WN *else_block = WN_CreateBlock();
    WN *if_stmt = WN_CreateIf (test, then_block, else_block);
    WGEN_Stmt_Append (if_stmt, Get_Srcpos());
    WGEN_Stmt_Push (then_block, wgen_stmk_if_then, Get_Srcpos());
    gs_t then_clause = gs_then_clause(cleanup);
    if (gs_tree_code(then_clause) == GS_CLEANUP_STMT)
      then_clause = gs_cleanup_expr(then_clause);
    WGEN_One_Stmt_Cleanup(then_clause);
    WGEN_Stmt_Pop(wgen_stmk_if_then);
  } else {

    if (gs_tree_code(cleanup) == GS_CLEANUP_STMT)
      cleanup = gs_cleanup_expr(cleanup);
    WGEN_One_Stmt_Cleanup (cleanup);
  }
  make_symbols_weak = saved_make_symbols_weak;
}

static void
Push_Scope_Cleanup (gs_t t, bool eh_only=false)
{
  // Don't push a cleanup without a scope
  if (scope_cleanup_i == -1 && gs_tree_code(t) == GS_CLEANUP_STMT)
    return;

  if (++scope_cleanup_i == scope_cleanup_max) {
    scope_cleanup_max = ENLARGE (scope_cleanup_max);
    scope_cleanup_stack =
      (SCOPE_CLEANUP_INFO *) realloc (scope_cleanup_stack,
	 	        scope_cleanup_max * sizeof (SCOPE_CLEANUP_INFO));
  }

  scope_cleanup_stack [scope_cleanup_i].stmt = t;
  if (gs_tree_code(t) == GS_CLEANUP_STMT ||
      gs_tree_code(t) == GS_TRY_CATCH_EXPR ||
      gs_tree_code(t) == GS_TRY_FINALLY_EXPR)
    New_LABEL (CURRENT_SYMTAB, 
	       scope_cleanup_stack [scope_cleanup_i].label_idx);
  else
    scope_cleanup_stack [scope_cleanup_i].label_idx = 0;
  if (gs_tree_code(t) == GS_TRY_BLOCK)
    New_LABEL (CURRENT_SYMTAB, 
	       scope_cleanup_stack [scope_cleanup_i].cmp_idx);
  else
    scope_cleanup_stack [scope_cleanup_i].cmp_idx = 0;
  scope_cleanup_stack [scope_cleanup_i].cleanup_eh_only = eh_only;
  scope_cleanup_stack [scope_cleanup_i].vla.has_alloca = FALSE;
  scope_cleanup_stack [scope_cleanup_i].vla.alloca_st = NULL;
  scope_cleanup_stack [scope_cleanup_i].vla.alloca_sts_vector = 
  						new vector<ST*>();
}

#ifdef KEY
void
Register_Cleanup (gs_t t)
{
  Push_Scope_Cleanup (t);
}
#endif

static void
Push_Handler_Info (gs_t handler, vector<gs_t> *v, 
	vector<SCOPE_CLEANUP_INFO> *scope, vector<TEMP_CLEANUP_INFO> *temp, 
	vector<BREAK_CONTINUE_INFO> *break_continue,
	vector<ST_IDX> *handler_list, vector<ST_IDX> *eh_spec,
	LABEL_IDX label_idx, bool outermost, LABEL_IDX cmp_idx[], 
	LABEL_IDX goto_idx)
{
   if (++handler_info_i == handler_info_max) {
    handler_info_max = ENLARGE (handler_info_max);
    handler_info_stack =
      (HANDLER_INFO *) realloc (handler_info_stack,
                        handler_info_max * sizeof (HANDLER_INFO));
  }

  handler_info_stack [handler_info_i].handler   = handler;
  handler_info_stack [handler_info_i].cleanups = v;
  handler_info_stack [handler_info_i].scope = scope;
  handler_info_stack [handler_info_i].temp_cleanup = temp;
  handler_info_stack [handler_info_i].break_continue = break_continue;
  handler_info_stack [handler_info_i].handler_list = handler_list;
  handler_info_stack [handler_info_i].eh_spec = eh_spec;
  handler_info_stack [handler_info_i].label_idx = label_idx;
  handler_info_stack [handler_info_i].cmp_idx[0] = cmp_idx[0];
  handler_info_stack [handler_info_i].cmp_idx[1] = cmp_idx[1];
  New_LABEL (CURRENT_SYMTAB, handler_info_stack [handler_info_i].cleanups_idx);
  handler_info_stack [handler_info_i].goto_idx = goto_idx;
  handler_info_stack [handler_info_i].outermost = outermost;
}

static void WGEN_Expand_Handlers_Or_Cleanup (const HANDLER_INFO&);

// Called from WGEN_Finish_Function ().
void
Do_Handlers (INT cleanups)
{
#ifdef KEY
  int saved_make_symbols_weak = make_symbols_weak;
  make_symbols_weak = TRUE;

  // enable_cxx_openmp ensures that we do not need any special processing
  // while inside a C++ exception handler.
  if (emit_exceptions
#ifdef FE_GNU_4_2_0
      && !enable_cxx_openmp
#endif
     ) processing_handler = true;
#endif
  while (handler_info_i != -1) {
#ifndef KEY
    LABEL_IDX start_handlers;
    New_LABEL (CURRENT_SYMTAB, start_handlers);
    Set_LABEL_addr_saved (start_handlers);
    WGEN_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, start_handlers, 0, NULL),
		     Get_Srcpos());
#else
    LABEL_IDX start_handlers = handler_info_stack[handler_info_i].cmp_idx[1];
    // TODO: Check if we need to mark this label as LABEL_addr_saved
    // Set handler_begin if there is no other entry point for this try-block
    if (start_handlers)
    {
	Is_True (LABEL_kind (Label_Table[start_handlers]) ==
	         LKIND_BEGIN_HANDLER, ("Wrong label kind, expecting handler_begin"));
        WN * cmp_wn = WN_CreateLabel ((ST_IDX) 0, start_handlers, 0, NULL);
	WN_Set_Label_Is_Handler_Begin (cmp_wn);
	WGEN_Stmt_Append (cmp_wn, Get_Srcpos());
    }
    WN * actual_cmp = WN_CreateLabel ((ST_IDX) 0, 
	       handler_info_stack[handler_info_i].cmp_idx[0], 0, NULL);
    WGEN_Stmt_Append (actual_cmp, Get_Srcpos());

    handler_stack.push (handler_info_stack[handler_info_i]);
#endif // !KEY
    --handler_info_i;
    WGEN_Expand_Handlers_Or_Cleanup (handler_info_stack[handler_info_i+1]);
#ifdef KEY
    handler_stack.pop();
#endif
  }
#ifdef KEY
  processing_handler = false;
  Do_Cleanups_For_EH(cleanups);
  if (emit_exceptions) 
    FmtAssert (cleanup_list_for_eh.size() == cleanups,
               ("EH Cleanup list not completely processed"));

  make_symbols_weak = saved_make_symbols_weak;
#endif
}

static void Call_Rethrow (void);
#ifndef KEY
static void Call_Terminate (void);
#endif // !KEY

//* this is key stuff, feel free to use it
////// start of key stuff
ST *
Get_eh_spec_ST (void)
{
        FmtAssert (!eh_spec_func_end.empty(),("Empty Type Filter Table"));

        ARB_HANDLE arb = New_ARB();
	int eh_spec_size = eh_spec_func_end.size();
        ARB_Init (arb, 0, eh_spec_size-1, sizeof(ST_IDX));
        Set_ARB_flags (arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
        STR_IDX str = Save_Str ("__EH_SPEC_TABLE__");
        TY_IDX ty;
        TY_Init (New_TY(ty), eh_spec_size*sizeof(ST_IDX), KIND_ARRAY, MTYPE_UNKNOWN, str);
        Set_TY_arb (ty, arb);
        Set_TY_etype (ty, MTYPE_TO_TY_array[MTYPE_U4]);
        ST * etable = New_ST (CURRENT_SYMTAB);
        ST_Init (etable, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
        Set_ST_is_initialized (*etable);
	Set_ST_one_per_pu (etable);
        return etable;
}


ST *
Get_typeinfo_ST (void)
{
        FmtAssert (!type_filter_vector.empty(),("Empty Type Filter Table"));

        ARB_HANDLE arb = New_ARB();
        ARB_Init (arb, 0, type_filter_vector.size()-1, sizeof(TYPE_FILTER_ENTRY));
        Set_ARB_flags (arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
        STR_IDX str = Save_Str ("__TYPEINFO_TABLE__");
        FLD_HANDLE fld1 = New_FLD ();
        FLD_Init (fld1, Save_Str ("st"),
                                MTYPE_TO_TY_array[MTYPE_U4], 0);
        FLD_HANDLE fld2 = New_FLD ();
        FLD_Init (fld2, Save_Str ("filter"),
                                MTYPE_TO_TY_array[MTYPE_U4], 4);
        Set_FLD_flags (fld2, FLD_LAST_FIELD);

        TY_IDX struct_ty;
        TY_Init (New_TY(struct_ty), sizeof(TYPE_FILTER_ENTRY), KIND_STRUCT,
                                MTYPE_M, Save_Str ("__TYPEINFO_ENTRY__"));
        Set_TY_fld (struct_ty, fld1);
        TY_IDX ty;
        TY_Init (New_TY(ty), type_filter_vector.size()*sizeof(TYPE_FILTER_ENTRY), KIND_ARRAY, MTYPE_M, str);
        Set_TY_arb (ty, arb);
        Set_TY_etype (ty, struct_ty);
        ST * etable = New_ST (CURRENT_SYMTAB);
        ST_Init (etable, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
        Set_ST_is_initialized (*etable);
	Set_ST_one_per_pu (etable);
        return etable;
}

// Return the st_idx of the exception-pointer variable. Mark it 'private'
// in enclosing openmp regions.
ST_IDX
Get_exception_pointer_symbol (void)
{
  ST_IDX st = TCON_uval (INITV_tc_val (INITO_val (PU_misc_info(Get_Current_PU()))));
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, &St_Table[st], TRUE);
#endif
  return st;
}

// Return the st_idx of the exception filter variable. Mark it 'private'
// in enclosing openmp regions.
ST_IDX
Get_exception_filter_symbol (void)
{
  ST_IDX st = TCON_uval (INITV_tc_val (INITV_next (INITO_val (PU_misc_info(Get_Current_PU())))));
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, &St_Table[st], TRUE);
#endif
  return st;
}

void
Do_EH_Tables (void)
{
        INITV_IDX blk, start;
        INITO_IDX id;

        for (int i=0; i<type_filter_vector.size(); ++i)
        {
                INITV_IDX st = New_INITV();
// Do not use INITV_Init_Integer(), since INITV_Init_Integer()
// silently calls INITV_Set_ONE() if the value is 1. Then you
// try to retrieve later using INITV_tc_val(), and you get an
// assertion failure!
		if (type_filter_vector[i].st)
                    INITV_Set_VAL (Initv_Table[st],
                        Enter_tcon (Host_To_Targ (MTYPE_U4,
                                type_filter_vector[i].st)), 1);
		else
		    INITV_Set_ZERO (Initv_Table[st], MTYPE_U4, 1);

                INITV_IDX filter = New_INITV();
                INITV_Set_VAL (Initv_Table[filter],
                        Enter_tcon (Host_To_Targ (MTYPE_U4,
                                type_filter_vector[i].filter)), 1);
                Set_INITV_next (st, filter);

                if (i == 0)
                {
                        blk = start = New_INITV();
                        INITV_Init_Block (blk, st);
                }
                else
                {
                        INITV_IDX next_blk = New_INITV();
                        INITV_Init_Block (next_blk, st);
                        Set_INITV_next (blk, next_blk);
                        blk = next_blk;
                }
                if (i == (type_filter_vector.size()-1))
                {
                        ST * typeinfo = Get_typeinfo_ST ();
                        id = New_INITO (ST_st_idx(typeinfo), start);
			// Store the inito_idx in the PU
			// 1. exc_ptr 2. filter : Set 3rd entry with inito_idx
			INITV_IDX index = INITV_next (INITV_next (INITO_val (
			               PU_misc_info (Get_Current_PU()))));
			// INITV_Set_VAL resets the next field, so back it up
			// and set it again.
			INITV_IDX bkup = INITV_next (index);
			INITV_Set_VAL (Initv_Table[index], 
				Enter_tcon (Host_To_Targ (MTYPE_U4, id)), 1);
			Set_INITV_next (index, bkup);
                }
        }
        type_filter_vector.clear();

	INITV_IDX prev_st = 0;
        for (int i=0; i<eh_spec_func_end.size(); ++i)
        {
                INITV_IDX st = New_INITV();
		FmtAssert (eh_spec_func_end[i] >= 0, ("Invalid eh-spec entry in front-end"));
		if (eh_spec_func_end[i])
                    INITV_Set_VAL (Initv_Table[st], Enter_tcon (
		    	Host_To_Targ (MTYPE_U4, eh_spec_func_end[i])), 1);
		else
		    INITV_Set_ZERO (Initv_Table[st], MTYPE_U4, 1);

                if (prev_st == 0)
                {
                        start = New_INITV();
                        INITV_Init_Block (start, st);
                }
                else
                        Set_INITV_next (prev_st, st);
		prev_st = st;
        }
	if (!eh_spec_func_end.empty())
	{
		ST * eh_spec = Get_eh_spec_ST ();
		id = New_INITO (ST_st_idx(eh_spec), start);
		INITV_IDX index = INITV_next (INITV_next (INITV_next (
			INITO_val (PU_misc_info(Get_Current_PU())))));
		// INITV_Set_VAL resets the next field, so back it up
		// and set it again.
		INITV_IDX bkup = INITV_next (index);
		INITV_Set_VAL (Initv_Table[index], 
			Enter_tcon (Host_To_Targ (MTYPE_U4, id)), 1);
		Set_INITV_next (index, bkup);
	}
	eh_spec_func_end.clear();
	current_eh_spec_ofst = 1;
}

// Emit all cleanups, and emit a goto after each set of cleanups to the handler.
// FROM == 0, implies, process the entire list. Else, skip FROM cleanups
// from the start of the list.
static void
Do_Cleanups_For_EH (INT from)
{
  std::list<EH_CLEANUP_ENTRY>::iterator start = cleanup_list_for_eh.begin();

#ifdef FE_GNU_4_2_0
  if (enable_cxx_openmp) {
    for (INT i=0; i<from; i++)
      start++;
  }
#endif

  for (std::list<EH_CLEANUP_ENTRY>::iterator i = start;
		i != cleanup_list_for_eh.end(); ++i) {
    EH_CLEANUP_ENTRY e = *i;

    WN *pad_wn = WN_CreateLabel ((ST_IDX) 0, e.pad, 0, NULL);
    WN_Set_Label_Is_Handler_Begin (pad_wn);
    WGEN_Stmt_Append (pad_wn, Get_Srcpos());

    WGEN_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, e.start, 0, NULL), 
    		     Get_Srcpos());

    for (vector<gs_t>::iterator j=e.cleanups->begin();
		j!=e.cleanups->end();++j)
    {
    	gs_t cleanup = *j;
	Emit_Cleanup(cleanup);
    }
    if (e.goto_idx)
	WGEN_Stmt_Append (WN_CreateGoto ((ST_IDX) 0, e.goto_idx), Get_Srcpos());
    else
	Generate_unwind_resume();
  }

#ifdef FE_GNU_4_2_0
  if (enable_cxx_openmp && from)
    cleanup_list_for_eh.erase (start, cleanup_list_for_eh.end());
  else
#endif
  cleanup_list_for_eh.clear();
}

#ifdef FE_GNU_4_2_0
// Called at the end of processing an MP parallel region, to output any
// cleanups that would need to be called if a function within the region
// throws an exception.
void
WGEN_maybe_do_eh_cleanups (void)
{
  if (cleanup_list_for_eh.size()) {
    LABEL_IDX label_idx;
    New_LABEL (CURRENT_SYMTAB, label_idx);
    WGEN_Stmt_Append (WN_CreateGoto ((ST_IDX) 0, label_idx), Get_Srcpos());
    // TODO: Probably need to pass the cleanups to process
    Do_Cleanups_For_EH ();
    WGEN_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, label_idx, 0, NULL),
                      Get_Srcpos());
  }
}
#endif

static void
Pop_Scope_And_Do_Cleanups (void)
{
  Is_True(scope_cleanup_i != -1,
	  ("Pop_Scope_And_Do_Cleanups: scope_cleanup-stack is empty"));

  while (true) {
    gs_t t = scope_cleanup_stack [scope_cleanup_i].stmt;
    if (gs_tree_code(t) != GS_CLEANUP_STMT)
    {
      if (gs_tree_code(t) == GS_BIND_EXPR || gs_tree_code(t) == GS_HANDLER)
      {
	// Leaving scope, so use dealloca for any alloca within the scope
	if (scope_cleanup_stack[scope_cleanup_i].vla.has_alloca)
	  WGEN_Dealloca (scope_cleanup_stack[scope_cleanup_i].vla.alloca_st,
	       scope_cleanup_stack[scope_cleanup_i].vla.alloca_sts_vector);
 	--scope_cleanup_i;
      }
      else if (gs_tree_code(t) == GS_TRY_CATCH_EXPR ||
               gs_tree_code(t) == GS_TRY_FINALLY_EXPR)
        --scope_cleanup_i;
      break;
    }
    Is_True(scope_cleanup_i != -1,
	    ("Pop_Scope_And_Do_Cleanups: no scope_stmt on stack"));
    if (scope_cleanup_stack[scope_cleanup_i].cleanup_eh_only)
    {
    	--scope_cleanup_i;
	continue;
    }
    --scope_cleanup_i;

    gs_t cleanup = scope_cleanup_stack[scope_cleanup_i+1].stmt;
    if (gs_tree_code(cleanup) == GS_CLEANUP_STMT)
      WGEN_One_Stmt_Cleanup (gs_cleanup_expr(cleanup));
    else if (gs_tree_code(cleanup) == GS_TRY_CATCH_EXPR ||
             gs_tree_code(cleanup) == GS_TRY_FINALLY_EXPR)
      WGEN_One_Stmt_Cleanup (gs_tree_operand(cleanup, 1));
  }
}

#ifdef KEY
void Unregister_Cleanup ()
{
  Pop_Scope_And_Do_Cleanups ();
}
#endif

static void
Push_Scope (gs_t t)
{
  if (++scope_i == scope_max) {
    scope_max = ENLARGE (scope_max);
    scope_stack =
      (gs_t *) realloc (scope_stack,
	 	        scope_max * sizeof (gs_t));
  }
  scope_stack[scope_i] = t;
}


void
Push_Temp_Cleanup (gs_t t, bool is_cleanup
#ifdef KEY
, bool cleanup_eh_only
#endif
)
{
  if (!lang_cplus) return;

  // If a guard var is required, conditionalize the cleanup.
  gs_t guard_var = WGEN_Get_Guard_Var();
  if (guard_var != NULL) {
    t = gs_build_if_stmt(gs_c_common_truthvalue_conversion(guard_var), t, NULL);
  }

  if (++temp_cleanup_i == temp_cleanup_max) {
    temp_cleanup_max = ENLARGE (temp_cleanup_max);
    temp_cleanup_stack =
      (TEMP_CLEANUP_INFO *) realloc (temp_cleanup_stack,
				     temp_cleanup_max * 
                                       sizeof (TEMP_CLEANUP_INFO));
  }

  temp_cleanup_stack [temp_cleanup_i].expr = t;
  if (is_cleanup)
    New_LABEL (CURRENT_SYMTAB, temp_cleanup_stack [temp_cleanup_i].label_idx);
  else
    temp_cleanup_stack [temp_cleanup_i].label_idx = 0;
#ifdef KEY
  temp_cleanup_stack [temp_cleanup_i].cleanup_eh_only = cleanup_eh_only;
#endif
}


// Return TRUE if candidate node matches the target node.
static bool
cleanup_matches (gs_t candidate, gs_t target)
{
  if (candidate == target)
    return TRUE;
  // The node could be hidden behind a guard variable.
  if (gs_tree_code(candidate) == GS_IF_STMT &&
      gs_then_clause(candidate) == target)
    return TRUE;
  return FALSE;
}


void
Do_Temp_Cleanups (gs_t t)
{
  if (!lang_cplus) return;

  Is_True(temp_cleanup_i != -1, ("Do_Temp_Cleanups: stack empty"));
  while (!cleanup_matches(temp_cleanup_stack[temp_cleanup_i].expr, t))
  {
    if (temp_cleanup_stack[temp_cleanup_i].cleanup_eh_only) {
// We don't want this cleanup to be emitted here -- it is to be executed only
// if an exception is thrown.
    	--temp_cleanup_i;
	continue;
    }

    Emit_Cleanup (temp_cleanup_stack [temp_cleanup_i].expr);
    --temp_cleanup_i;
  }
  --temp_cleanup_i;

  if (emit_exceptions && processing_handler && 
	!cleanup_matches(temp_cleanup_stack[temp_cleanup_i+1].expr, t))
  {
    HANDLER_INFO hi = handler_stack.top();
    if (hi.temp_cleanup)
    {
      int n = hi.temp_cleanup->size()-1;
      while (!cleanup_matches((*hi.temp_cleanup)[n].expr, t)) {
	gs_t cleanup = (*hi.temp_cleanup) [n].expr;
	Emit_Cleanup(cleanup);
	--n;
      }
    }
  }
}

static void
WGEN_Record_Loop_Switch (gs_code_t tree_code)
{
  INT32 i;
  Is_True(tree_code == GS_DO_STMT ||
  	  tree_code == GS_FOR_STMT ||
  	  tree_code == GS_WHILE_STMT ||
  	  tree_code == GS_SWITCH_STMT,
	  ("WGEN_Record_Loop_Switch: unexpected tree_code"));

  if (++break_continue_info_i == break_continue_info_max) {
    break_continue_info_max = ENLARGE (break_continue_info_max);
    break_continue_info_stack =
      (BREAK_CONTINUE_INFO *) realloc (break_continue_info_stack,
				       break_continue_info_max *
					 sizeof (BREAK_CONTINUE_INFO));
  }

  break_continue_info_stack 
    [break_continue_info_i].tree_code          = tree_code;
  break_continue_info_stack 
    [break_continue_info_i].break_label_idx    = 0;
  break_continue_info_stack 
    [break_continue_info_i].continue_label_idx = 0;
  if (scope_cleanup_i == -1)
    break_continue_info_stack
      [break_continue_info_i].scope = NULL;
  else {
    for (i = scope_cleanup_i;
	 gs_tree_code(scope_cleanup_stack[i].stmt) == GS_CLEANUP_STMT;
	 --i);
#ifdef FE_GNU_4_2_0
    Is_True (i != -1 && 
	     (gs_tree_code(scope_cleanup_stack[i].stmt) == GS_BIND_EXPR ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_HANDLER ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_TRY_CATCH_EXPR ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_TRY_FINALLY_EXPR ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_EH_SPEC_BLOCK ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_TRY_BLOCK),
	    ("WGEN_Record_Loop_Switch: no scope_stmt on stack"));
#else
    Is_True (i != -1 && 
	     (gs_tree_code(scope_cleanup_stack[i].stmt) == GS_BIND_EXPR ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_HANDLER ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_TRY_CATCH_EXPR ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_TRY_FINALLY_EXPR ||
	      gs_tree_code(scope_cleanup_stack[i].stmt) == GS_TRY_BLOCK),
	    ("WGEN_Record_Loop_Switch: no scope_stmt on stack"));
#endif
    break_continue_info_stack
      [break_continue_info_i].scope = scope_cleanup_stack[i].stmt;
  }
} /* WGEN_Record_Loop_Switch */

// KEY: When dummy_expand is true, we don't mean to expand the node into
// a label wn, we only want to update the different _info_stack arrays.
// Also we currently update DECL_LABEL_IDX when this flag is true, but
// we may be able to do it always.
static void
WGEN_Expand_Case (gs_t low, gs_t high
#ifdef KEY
                  , gs_t label_decl, BOOL dummy_expand = FALSE
#endif
                 )
{
  WN        *wn;
  WN        *lower_bound;
  WN        *upper_bound;
  LABEL_IDX  case_label_idx;

  if (high != NULL)
    DevWarn("encountered case range");

  if (low == NULL) {
    if (switch_info_stack [switch_info_i].default_label_idx != 0)
      printf ("Error: duplicate default label");
    else {
      New_LABEL (CURRENT_SYMTAB, case_label_idx);
      switch_info_stack [switch_info_i].default_label_idx = case_label_idx;
    }
  }

  else {
    if (gs_tree_code(low) == GS_VAR_DECL)
      low = gs_decl_initial(low);
    if (high != NULL && gs_tree_code(high) == GS_VAR_DECL)
      high = gs_decl_initial(high);
    lower_bound = WGEN_Expand_Expr (low);
    upper_bound = (high == NULL) ? lower_bound
				      : WGEN_Expand_Expr(high);
    if (++case_info_i == case_info_max) {
      case_info_max   = ENLARGE(case_info_max);
      case_info_stack = (CASE_INFO *) realloc (case_info_stack,
                                               case_info_max * sizeof (CASE_INFO));
    }

    case_info_stack 
      [case_info_i].case_lower_bound_value = 
        (low  == NULL) ? 0 : WN_const_val (lower_bound);
    case_info_stack 
      [case_info_i].case_upper_bound_value = 
        (high == NULL) ? WN_const_val (lower_bound) 
			    : WN_const_val (upper_bound);
    for (int i = switch_info_stack [switch_info_i].start_case_index;
         i < case_info_i; ++i) 
      if (WN_const_val(lower_bound) == 
          case_info_stack [i].case_lower_bound_value)
  	printf ("duplicate case");
    New_LABEL (CURRENT_SYMTAB, case_label_idx);
    case_info_stack [case_info_i].case_label_idx = case_label_idx;
  }

#ifdef KEY
  if (dummy_expand)
  {
    DECL_LABEL_IDX(label_decl) = case_label_idx;
    DECL_SYMTAB_IDX(label_decl) = CURRENT_SYMTAB;
  }
  else
#endif
  {
    wn = WN_CreateLabel ((ST_IDX) 0, case_label_idx, 0, NULL);
    WGEN_Stmt_Append (wn, Get_Srcpos ());
  }
} /* WGEN_Expand_Case */

static void
WGEN_Declare_Nonlocal_Label (gs_t label)
{
  WGEN_Get_LABEL (label, FALSE);
} /* WGEN_Declare_Nonlocal_Label */


/* Generate WHIRL for an asm statement with arguments.
   For now, we don't do all the stuff done by expand_asm_operands;
   instead, we leave much of that stuff until asm lowering time.
   Here, we just build the OPR_ASM node that records the relevant
   information about the asm statement. */

static WN *
idname_from_regnum (int gcc_reg)
{
  if (gcc_reg < 0) {
	DevWarn("unrecognized register name in asm");
  	return NULL;
  }
  else {
	extern PREG_NUM Map_Reg_To_Preg [];
	PREG_NUM preg = Map_Reg_To_Preg [gcc_reg];
	if (preg < 0) {
		DevWarn("couldn't map asm regname to preg");
		return NULL;
	}
	ST *st;
	if (Preg_Offset_Is_Int(preg))
		st = Int_Preg;
	else if (Preg_Offset_Is_Float(preg))
		st = Float_Preg;
#ifdef TARG_X8664
	else if (Preg_Offset_Is_X87(preg))
		st = X87_Preg;
#endif
	else
		FmtAssert (FALSE, ("unexpected preg %d", preg));
  	return WN_CreateIdname((WN_OFFSET) preg, st);
  }
}

char *
remove_plus_modifier(char *s)
{
#define MAX_NON_PLUS_CONSTRAINT_CHARS 7
  static char out[MAX_NON_PLUS_CONSTRAINT_CHARS + 1];
  int i = 0;
  while (i <= MAX_NON_PLUS_CONSTRAINT_CHARS)
    {
      while (*s == '+')
	{
	  ++s;
	}
      out[i++] = *s;
      if (*s == '\0')
	{
	  return out;
	}
      else
	{
	  ++s;
	}
    }
  Fail_FmtAssertion("Constraint string too long");
  /*NOTREACHED*/
}

BOOL
constraint_supported (const char *s)
{
  while (*s != 0) {
    if (*s != 'r' &&
	*s != 'f' &&
	*s != 'm' &&
	*s != '+' &&
	*s != ',' &&
	*s != '=' &&
	(*s < '0' ||
	 *s > '9')) {
      return FALSE;
    }
    ++s;
  }
  return TRUE;
}

ST *
st_of_new_temp_for_expr(const WN *expr)
{
  static unsigned int temp_count = 0;

  static char temp_name[64];

  sprintf(temp_name, "asm.by.address.temp_%u", temp_count++);

  ST *retval = New_ST(CURRENT_SYMTAB);
  
  ST_Init (retval,
	   Save_Str (temp_name),
	   CLASS_VAR,
	   SCLASS_AUTO,
	   EXPORT_LOCAL,
	   MTYPE_To_TY(WN_rtype(expr)));
  return retval;
}

// need to keep track of what kind of constraint a numeric constraint
// refers to (by address or not).  So keep list of constraints.

#define MAX_RECOG_OPERANDS 30
static char *operand_constraint_array[MAX_RECOG_OPERANDS];

static BOOL
constraint_by_address (const char *s)
{
#ifndef TARG_X8664
  if (strchr (s, 'm')) {
#else
  if (strchr (s, 'm') || strchr (s, 'g')) {
#endif
    return TRUE;
  }
  else if (isdigit(*s)) {
    return constraint_by_address (operand_constraint_array[*s - '0']);
  }
  else {
    return FALSE;
  }
}

#ifdef KEY
// Use the OPND_NUM_MAP to update the operand numbers in the CONSTRAINT_STRING.
static void
update_opnd_num(int *opnd_num_map, char *constraint_string)
{
  char *p;

  for (p = constraint_string; *p != '\0'; p++) {
    if (*p >= '0' &&
	*p <= '9') {
      unsigned int old_opnd_num = *p - '0';
      unsigned int new_opnd_num = opnd_num_map[old_opnd_num];
      Is_True(new_opnd_num >= 0 && new_opnd_num <= old_opnd_num,
	      ("update_opnd_num: bad opnd numbers map"));
      *p = new_opnd_num + '0';
    }
  }
}
#endif

static WN *
add_offset(WN_OFFSET  ofst,
	   WN        *address)	// not const; some simplification may occur.
{
  return WN_Binary(OPR_ADD, Pointer_Mtype,
		   WN_Intconst(MTYPE_I8, ofst),
		   address);
}

static WN *
address_of (const WN *wn)
{
  if (WN_operator(wn) == OPR_ILOAD ||
      WN_operator(wn) == OPR_MLOAD) {
    return add_offset(WN_offset(wn), WN_kid0(wn));
  }
  else if ((WN_operator(wn) == OPR_LDID) &&
	   (ST_sclass(WN_st(wn)) != SCLASS_REG)) {
    return WN_Lda (Pointer_Mtype,
		   WN_offset(wn),
		   WN_st(wn),
		   (UINT) 0);
  }
  // No address for this object. This expression is not an lvalue.
  return NULL;
}

/* What OPR_ASM looks like:
 *
 *   Kids: 0 is a block of IDNAMEs referring to
 *         registers that get clobbered. Clobbering of memory and
 *         condition codes is encoded in WN_Asm_Clobbers_Cc() and
 *         WN_Asm_Clobbers_Mem().
 *       1 is a block of PRAGMA or XPRAGMA nodes giving information
 *         about copy-out output operands and their constraints.
 *       2 .. WN_kid_count() - 1 are OPR_ASM_INPUT nodes, each of
 *         which gives a constraint and an rvalue for the
 *         corresponding input to the asm statement.
 *
 * Inputs originate either as input operands to the ASM, or as output
 * operands that are passed by address.
 */

static PREG_NUM asm_neg_preg = -2;

static const int flag_bad_asm_constraint_kills_stmt = 0;

void
Wgen_Expand_Asm_Operands (gs_t  string,
			 gs_t  outputs,
			 gs_t  inputs,
			 gs_t  clobbers,
			 int   vol,
			 char *filename,
			 int   line)
{
  // filename and line are ignored for now; eventually maybe they
  // should be used to generate SRCPOS information on the OPR_ASM_STMT
  // WN.
  //
  // I don't know yet why filename and line are passed for
  // expand_asm_operands but not for other expand_* routines in
  // gnu/stmt.c.

  // inputs can be NULL (bug 12602).
  int ninputs = inputs ? gs_list_length (inputs) : 0;

  gs_t tail;
  char *constraint_string;

#ifdef KEY
  // Map operand numbers in the original asm to operand numbers in the WHIRL
  // asm.  The mapping changes because for 'm' output constraints, the WHIRL
  // asm represents it as an input rather than an output.  This changes the
  // operand positions of all the subsequent output operands.  For example:
  //   asm ("foo %0,%1" : "=rm" (a), "=r" (b) : "1" (b))
  // effectively becomes:
  //   asm ("foo %0,%1" : "=r" (b) : "r" (a), "1" (b))
  // Now we need to rename "1"(b) to "0"(b).
  int opnd_num_map[MAX_RECOG_OPERANDS];
#endif

  // Keep list of output operand constraints so that we know
  // what a numeric constraint refers to.
  int i = 0;
  // Store the constraint strings
  for (tail = outputs; tail; tail = gs_tree_chain (tail)) {
#ifdef KEY
    // Initialize operand numbers map.
    opnd_num_map[i] = i;

    // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
    // first operand gives the string constant.
    constraint_string = 
      const_cast<char*>(gs_tree_string_pointer (gs_tree_value (gs_tree_purpose (tail))));
#else
    constraint_string = gs_tree_string_pointer (gs_tree_purpose (tail));
#endif /* KEY */
    operand_constraint_array[i] = constraint_string;
    ++i;
  }
#ifdef KEY
  opnd_num_map[i] = -1;
#endif

  FmtAssert(i < MAX_RECOG_OPERANDS, ("Too many asm operands"));
  for ( ; i < MAX_RECOG_OPERANDS; ++i) {
    operand_constraint_array[i] = NULL;
  }
  
  // Each occurrence of the "+" constraint modifier is converted to a
  // numeric matching constraint on a new input. In the following
  // loop, we count the number of "+" constraint modifiers so we know
  // how many inputs there will be.
  //
  // Also for the time being we discard the entire ASM construct if
  // there is a constraint we don't recognize. This is so we can
  // test-compile code containing ASM statements that apply to targets
  // we don't support. At the moment, we support only "r", "f", and
  // "m" constraints for IA64, so those are the only ones on which we
  // don't barf. Ideally we would check with some target-specific
  // routine to see which constraints are valid, but we don't want to
  // link gccfe with targ_info or other similar stuff for now.
  for (tail = outputs;
       tail;
       tail = gs_tree_chain (tail))
    {
#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
        const_cast<char*>(gs_tree_string_pointer (gs_tree_value (gs_tree_purpose (tail))));
#else
      constraint_string = (gs_tree_string_pointer (gs_tree_purpose (tail)));
#endif /* KEY */

      if (strchr (constraint_string, '+') ||
	  constraint_by_address (constraint_string))
	{
	  ++ninputs;
	}
      if (flag_bad_asm_constraint_kills_stmt && 
	  !constraint_supported (constraint_string)) {
	DevWarn ("Unrecognized constraint %s; "
		 "asm statement at line %d discarded",
		 constraint_string, lineno);
	return;
      }
    }

  WN *asm_wn = WN_CreateAsm_Stmt (ninputs + 2,
				  const_cast<char*>(gs_tree_string_pointer (string))); // KEY

  WN *clobber_block = WN_CreateBlock ();

  WN_kid0(asm_wn) = clobber_block;

  for (tail = clobbers; tail; tail = gs_tree_chain (tail))
    {
      char *clobber_string =
	const_cast<char *>(gs_tree_string_pointer (gs_tree_value (tail)));  // KEY

      WN *clobber_pragma = NULL;

      int gcc_reg = gs_decode_reg_name(clobber_string);
      if (gcc_reg == -3)
	WN_Set_Asm_Clobbers_Cc(asm_wn);
      else if (gcc_reg == -4)
	WN_Set_Asm_Clobbers_Mem(asm_wn);
      else {
	WN *clobbered_idname = idname_from_regnum (gcc_reg);

      	if (clobbered_idname) {
	  // This is a clobbered register that can be expressed as a
	  // WHIRL dedicated PREG.

	  ST *clobber_st = New_ST(CURRENT_SYMTAB);
	  ST_Init(clobber_st,
		Str_To_Index (Save_Str (clobber_string),
			      Current_Strtab),
		CLASS_NAME,
		SCLASS_UNKNOWN,
		EXPORT_LOCAL,
		(TY_IDX) 0);

	  clobber_pragma = WN_CreateXpragma (WN_PRAGMA_ASM_CLOBBER,
			    ST_st_idx(clobber_st),
			    1);
	  WN_kid0 (clobber_pragma) = clobbered_idname;
      	}
      	else {
	  // This is a clobbered register that cannot be expressed as a
	  // WHIRL dedicated PREG. Make the "asm" volatile because it
	  // clobbers something WHIRL can't see.

	  ST *clobber_st = New_ST(CURRENT_SYMTAB);
	  ST_Init(clobber_st,
		Str_To_Index (Save_Str (clobber_string),
			      Current_Strtab),
		CLASS_NAME,
		SCLASS_UNKNOWN,
		EXPORT_LOCAL,
		(TY_IDX) 0);

	  clobber_pragma = WN_CreatePragma (WN_PRAGMA_ASM_CLOBBER,
			   ST_st_idx(clobber_st),
			   (INT32) 0,
			   (INT32) 0);

	  WN_Set_Asm_Volatile (asm_wn);
        }
      }

      if (clobber_pragma != NULL)
      	WN_INSERT_BlockAfter (clobber_block,
			    WN_last (clobber_block),
			    clobber_pragma);
    }

  WN *output_constraint_block = WN_CreateBlock ();

  WN_kid1(asm_wn) = output_constraint_block;

  i = 2;

  // Expand the by-address output operands before appending the
  // ASM_STMT node so side effects of these operands occur in the
  // right place.
  UINT32 opnd_num = 0;

  for (tail = outputs;
       tail;
       tail = gs_tree_chain (tail))
    {
#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
        const_cast<char*>(gs_tree_string_pointer (gs_tree_value (gs_tree_purpose (tail))));
#else
      constraint_string = (gs_tree_string_pointer (gs_tree_purpose (tail)));
#endif /* KEY */

      if (constraint_by_address(constraint_string)) {
	// This operand is by address, and gets represented as an
	// ASM_INPUT even though the user told us it's an output.
	WN *lhs_rvalue = WGEN_Expand_Expr(gs_tree_value(tail));
	WN *addr_of_lvalue = address_of(lhs_rvalue);
	FmtAssert(addr_of_lvalue != NULL,
		  ("WGEN_Expand_Asm_Operands: output operand must be lvalue"));
	WN_kid (asm_wn, i) =
	  WN_CreateAsm_Input (constraint_string, opnd_num, addr_of_lvalue);
	++i;
#ifdef KEY
	// There is effectively one less output.  Decrement all subsequent
	// output operand positions by one.
	for (int j = opnd_num + 1; opnd_num_map[j] != -1; j++) {
	  opnd_num_map[j]--;
	}
#endif
      }
      ++opnd_num;
    }

  for (tail = inputs;
       tail;
       tail = gs_tree_chain (tail))
    {
      if (gs_tree_purpose (tail) == NULL)
	{
	  Fail_FmtAssertion ("hard register `%s' listed as "
			     "input operand to `asm'",
			     gs_tree_string_pointer (gs_tree_value (tail)) );
	  return;
	}

#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
        const_cast<char*>(gs_tree_string_pointer (gs_tree_value (gs_tree_purpose (tail))));
#else
      constraint_string = gs_tree_string_pointer (gs_tree_purpose (tail));
#endif /* KEY */

      if (flag_bad_asm_constraint_kills_stmt && 
	  !constraint_supported (constraint_string)) {
	DevWarn ("Unrecognized constraint %s; "
		 "asm statement at line %d discarded",
		 constraint_string, lineno);
	return;
      }

      WN *input_rvalue = WGEN_Expand_Expr (gs_tree_value (tail));

      // bugs 14402, 14799: If there is a conversion operator, we need
      // to preserve the conversion. If the address of the input needs
      // to be taken, we should perform any conversion before using its
      // address (bug 14402). For non-address constraints, load of the
      // temporary will contain the updated type (bug 14799).
      BOOL needs_temp = gs_tree_code(gs_tree_value(tail)) == GS_NOP_EXPR;
      // For constant inputs, CG expects to find the constant as a direct
      // kid of the asm_input. So it should not be copied to a temporary.
      BOOL const_input = WN_operator(input_rvalue) == OPR_INTCONST ||
                         (WN_operator(input_rvalue) == OPR_LDA &&
                          ST_sym_class(WN_st(input_rvalue)) == CLASS_CONST);

      if (constraint_by_address(constraint_string)) {
	WN *addr_of_rvalue;
	if ((!needs_temp || const_input) &&  // bug 14402
	    (addr_of_rvalue = address_of(input_rvalue)) != NULL) {
	  // Pass the address of the input rvalue, because the
	  // constraint says we pass the operand by its address.
	  input_rvalue = addr_of_rvalue;
	}
	else {
	  // Create a temporary to hold the value of the expression,
	  // and pass the address of that temporary.
	  ST *temp_st = st_of_new_temp_for_expr(input_rvalue);
	  WN *store_wn = WN_Stid(WN_rtype(input_rvalue),
				 (WN_OFFSET) 0,
				 temp_st,
				 // We may want to get high-level type
				 // of the RHS in the cases where that
				 // information exists, but for now,
				 // just put the low-level type on the
				 // store.
				 MTYPE_To_TY(WN_rtype(input_rvalue)),
				 input_rvalue);
	  WGEN_Stmt_Append (store_wn, Get_Srcpos ());
	  input_rvalue = WN_Lda (Pointer_Mtype,
				 (WN_OFFSET) 0,
				 temp_st,
				 (UINT) 0);
	}
      }
      else if (needs_temp && !const_input) { // bug 14799
	TY_IDX ty_idx = Get_TY(gs_tree_type(gs_tree_value(tail)));
	ST *temp_st = Gen_Temp_Symbol(ty_idx, "asm.input");
	WN *stid_wn = WN_Stid(TY_mtype(ty_idx),
			      0,
			      temp_st,
			      ty_idx,
			      input_rvalue);
	WGEN_Stmt_Append (stid_wn, Get_Srcpos ());
	input_rvalue = WN_Ldid(TY_mtype(ty_idx), 0, temp_st, ty_idx);
      }


#ifdef KEY
      // Get the new operand numbers from map.
      update_opnd_num(opnd_num_map, constraint_string);
#endif

      WN_kid (asm_wn, i) =
	WN_CreateAsm_Input (constraint_string, opnd_num, input_rvalue);
      ++i;
      ++opnd_num;
    }

  // Is Get_Srcpos the right thing to use?
  WGEN_Stmt_Append (asm_wn, Get_Srcpos ());

  // Side effects of copy-out operands occur after the asm. Kind of
  // weird, but that's what GCC does.
  opnd_num = 0;
#ifdef KEY 
  // Bug 5622
  // If (constraint_by_address(constraint_string)) is true then,
  // this operand is by address, and gets represented as an
  // ASM_INPUT even though the user told us it's an output.
  // This should be factored when memory operand constraints (+m) appear 
  // before other output constraints that are both read & write.
  INT nonmem_opnd_num = 0;
#endif
  for (tail = outputs;
       tail;
       tail = gs_tree_chain (tail), ++opnd_num)
    {
#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
        const_cast<char*>(gs_tree_string_pointer (gs_tree_value (gs_tree_purpose (tail))));
#else
      constraint_string = gs_tree_string_pointer (gs_tree_purpose (tail));
#endif /* KEY */

      if (!constraint_by_address(constraint_string)) {
	// This operand is copy-in/copy-out.

	BOOL plus_modifier = (strchr (constraint_string, '+') != NULL);

	char input_opnd_constraint[8];

	if (plus_modifier)
	  {
	    // de-plus the output operand's constraint string.
	    constraint_string = remove_plus_modifier(constraint_string);

	    // Make up a numeric matching constraint string for the
	    // input operand we're going to add.
#ifdef KEY
	    sprintf(input_opnd_constraint, "%d", nonmem_opnd_num);
#else
	    sprintf(input_opnd_constraint, "%d", opnd_num);
#endif
	  }
#ifdef KEY
	nonmem_opnd_num ++;
#endif

	WN *output_rvalue_wn = WGEN_Lhs_Of_Modify_Expr (GS_MODIFY_EXPR,
						       gs_tree_value (tail),
						       NULL,
						       plus_modifier,
						       (TY_IDX) 0, // component type
						       (INT64) 0,  // component offset
						       (UINT32) 0, // field ID
						       FALSE,      // is bit field?
						       NULL,       // dummy rhs kid
						       asm_neg_preg, // preg num
						       FALSE,      // is realpart
						       FALSE);     // is imagpart

	if (plus_modifier)
	  {
	    WN_kid (asm_wn, i) =
	      WN_CreateAsm_Input (input_opnd_constraint,
				  opnd_num,
				  output_rvalue_wn);
	    ++i;
	  }

	// Compute the ST used as the base for the negative PREG
	// reference in the output operand. This duplicates work done in
	// WGEN_Lhs_Of_Modify_Expr.
	TYPE_ID desc = TY_mtype (Get_TY (gs_tree_type (gs_tree_value (tail))));
	ST *preg_st = MTYPE_To_PREG(desc);

	ST *constraint_st = New_ST(CURRENT_SYMTAB);
	ST_Init(constraint_st,
		Str_To_Index (Save_Str (constraint_string),
			      Current_Strtab),
		CLASS_NAME,
		SCLASS_UNKNOWN,
		EXPORT_LOCAL,
		(TY_IDX) 0);

	WN *constraint_pragma =
	  WN_CreatePragma (WN_PRAGMA_ASM_CONSTRAINT,
			   (ST_IDX) ST_st_idx(preg_st),
			   (INT32) ST_st_idx(constraint_st),
			   asm_neg_preg,
			   opnd_num);

	WN_INSERT_BlockAfter (output_constraint_block,
			      WN_last (output_constraint_block),
			      constraint_pragma);
	--asm_neg_preg;
      }
    }

  if (vol)
    {
      WN_Set_Asm_Volatile (asm_wn);
    }
}

LABEL_IDX
WGEN_Get_LABEL (gs_t label, int def)
{
  LABEL_IDX label_idx =  DECL_LABEL_IDX(label);
  SYMTAB_IDX symtab_idx = DECL_SYMTAB_IDX(label);

  if (label_idx == 0
#ifdef KEY
      // Don't use old indexes that we are not supposed to use.
      || label_idx <= WGEN_unusable_label_idx
#endif
     ) {
    New_LABEL (CURRENT_SYMTAB, label_idx);
    DECL_LABEL_IDX(label) = label_idx;
    DECL_SYMTAB_IDX(label) = CURRENT_SYMTAB;
#ifdef KEY
    WGEN_last_label_idx = label_idx;
    // Need a new label wn.
    DECL_LABEL_DEFINED(label) = FALSE;
#endif
    if (!def) {
      if (++undefined_labels_i == undefined_labels_max) {
        undefined_labels_max   = ENLARGE(undefined_labels_max);
        undefined_labels_stack =
          (LABEL_INFO *) realloc (undefined_labels_stack,
                                  undefined_labels_max * sizeof (LABEL_INFO));
      }
      undefined_labels_stack [undefined_labels_i].label_idx  = label_idx;
      undefined_labels_stack [undefined_labels_i].symtab_idx = CURRENT_SYMTAB;
      undefined_labels_stack [undefined_labels_i].defined    = FALSE;
    }
  }
  else {
    if (def) {
      for (int i = undefined_labels_i; i >= 0; --i) {
        if (undefined_labels_stack [i].label_idx  == label_idx &&
            undefined_labels_stack [i].symtab_idx == CURRENT_SYMTAB) {
          undefined_labels_stack [i].defined = TRUE;
          break;
        }
      }
    }
  }

  return label_idx;
} /* WGEN_Get_LABEL */

void
WGEN_Check_Undefined_Labels (void)
{
  INT32 i;
  for (i = undefined_labels_i; i >= 0; --i) {
    LABEL_IDX  label_idx  = undefined_labels_stack [undefined_labels_i].label_idx;
    SYMTAB_IDX symtab_idx = undefined_labels_stack [undefined_labels_i].symtab_idx;
//  fprintf (stderr, "WGEN_Check_Undefined_Labels: %d idx = %8x [%d]\n", i, label_idx, symtab_idx);
    if (LABEL_IDX_level(label_idx) < CURRENT_SYMTAB)
      break;
    FmtAssert (undefined_labels_stack [undefined_labels_i].defined,
               ("label not defined within current function scope"));
  }
  undefined_labels_i = i;
} /* WGEN_Check_Undefined_Labels */


void
WGEN_Stmt_Init (void)
{
  if_else_info_max   = 32;
  if_else_info_i     = -1;
  if_else_info_stack = 
    (BOOL *) malloc (sizeof (BOOL) * if_else_info_max);

  case_info_max      = 32;
  case_info_i        = -1;
  case_info_stack    = 
    (CASE_INFO *) malloc (sizeof (CASE_INFO) * case_info_max);

  switch_info_max    = 32;
  switch_info_i      = -1;
  switch_info_stack  = 
    (SWITCH_INFO *) malloc (sizeof (SWITCH_INFO) * switch_info_max);

  break_continue_info_max   = 32;
  break_continue_info_i     = -1;
  break_continue_info_stack = 
    (BREAK_CONTINUE_INFO *) malloc (sizeof (BREAK_CONTINUE_INFO) *
                                    break_continue_info_max);

  undefined_labels_max   = 32;
  undefined_labels_i     = -1;
  undefined_labels_stack = 
    (LABEL_INFO *) malloc (sizeof (LABEL_INFO) * undefined_labels_max);

  scope_cleanup_max      = 32;
  scope_cleanup_i  	 = -1;
  scope_cleanup_stack    =
    (SCOPE_CLEANUP_INFO *) malloc (sizeof (SCOPE_CLEANUP_INFO) * 
				   scope_cleanup_max);

  scope_max    	         = 32;
  scope_i  	         = -1;
  scope_stack            =
    (gs_t *) malloc (sizeof (gs_t) * scope_max);

  temp_cleanup_max       = 32;
  temp_cleanup_i	 = -1;
  temp_cleanup_stack	 =
    (TEMP_CLEANUP_INFO *) malloc (sizeof (TEMP_CLEANUP_INFO) * 
				  temp_cleanup_max);

  handler_info_max	 = 32;
  handler_info_i	 = -1;
  handler_info_stack     =
    (HANDLER_INFO *) malloc (sizeof(HANDLER_INFO) * handler_info_max);

  scope_number           = 0;
} /* WGEN_Stmt_Init */

#ifdef KEY
// Special case to also handle while we are inside a handler
static void
Cleanup_To_Scope_From_Handler(gs_t scope)
{
  INT32 i = scope_cleanup_i;
  INT32 j = -1;
  Is_True(i != -1, ("Cleanup_To_Scope_From_Handler: scope_cleanup_stack empty"));
  while ((i != -1) && (scope_cleanup_stack [i].stmt != scope)) {
    gs_t stmt = scope_cleanup_stack [i].stmt;
    if (gs_tree_code(stmt) == GS_BIND_EXPR ||
        gs_tree_code(stmt) == GS_HANDLER)
      j = i;
    --i;
  }

  bool found_target_scope = false;
  if (i != -1)	found_target_scope = true;

  if (j != -1) {
    i = scope_cleanup_i;
    while (i != j) {
      if (gs_tree_code(scope_cleanup_stack [i].stmt) == GS_CLEANUP_STMT &&
		!scope_cleanup_stack[i].cleanup_eh_only)
        WGEN_One_Stmt_Cleanup (gs_cleanup_expr(scope_cleanup_stack [i].stmt));
    --i;
    }
  }
  if (found_target_scope) return;

  FmtAssert (processing_handler, ("Invalid scope"));

  HANDLER_INFO hi = handler_stack.top();
  FmtAssert (hi.scope, ("No scope information available"));
  i = hi.scope->size()-1;
  j = -1;
  Is_True(i != -1, ("Cleanup_To_Scope_From_Handler: scope_cleanup_stack empty"));

  while ((*hi.scope)[i].stmt != scope) {
    gs_t stmt = (*hi.scope)[i].stmt;
    if (gs_tree_code(stmt) == GS_BIND_EXPR ||
        gs_tree_code(stmt) == GS_HANDLER)
	j = i;
    --i;
  }
  if (j != -1) {
    i = hi.scope->size()-1;
    while (i != j) {
      if (gs_tree_code((*hi.scope)[i].stmt) == GS_CLEANUP_STMT &&
		!(*hi.scope)[i].cleanup_eh_only)
	WGEN_One_Stmt_Cleanup (gs_cleanup_expr((*hi.scope)[i].stmt));
      --i;
    }
  }
}
#endif // KEY

static void
Cleanup_To_Scope(gs_t scope)
{
  INT32 i = scope_cleanup_i;
  INT32 j = -1;
  Is_True(i != -1, ("Cleanup_To_Scope: scope_cleanup_stack empty"));
  while (scope_cleanup_stack [i].stmt != scope) {
    gs_t stmt = scope_cleanup_stack [i].stmt;
    if (gs_tree_code(stmt) == GS_BIND_EXPR ||
        gs_tree_code(stmt) == GS_HANDLER)
      j = i;
    --i;
  }

  if (j != -1) {
    i = scope_cleanup_i;
    while (i != j) {
#ifdef KEY
      if (gs_tree_code(scope_cleanup_stack [i].stmt) == GS_CLEANUP_STMT &&
		!scope_cleanup_stack[i].cleanup_eh_only)
#else
      if (gs_tree_code(scope_cleanup_stack [i].stmt) == GS_CLEANUP_STMT)
#endif
        WGEN_One_Stmt_Cleanup (gs_cleanup_expr(scope_cleanup_stack [i].stmt));
    --i;
    }
  }
}
 
//
// for (;;) { try { throw 1; } catch (...) { break; } }
// While expanding the handler in the above code, break_continue_info_i
// will be -1, since we are expanding the handler after finishing the
// function. Hence we need to get the break-continue-info from the associated
// try-block.
// bug 2823: For code like:
// try { throw 1;} catch (...) {for (;;) { break; }}
// The for-loop is in the handler and does not have any try-block. So we
// need to set the break-continue labels in 
// WGEN_Expand_Break/ WGEN_Expand_Continue, even though we are inside a handler
//
static void
WGEN_Expand_Break (void)
{
  INT32     i  	      = break_continue_info_i;
  LABEL_IDX label_idx;
  gs_t      scope;
  WN *      wn;

  HANDLER_INFO hi;
  if (processing_handler)
    hi = handler_stack.top();
  if (i == -1)
  {
    FmtAssert (processing_handler && hi.break_continue, ("No break/continue info"));
    label_idx = (*hi.break_continue)[hi.break_continue->size()-1].break_label_idx;
    scope = (*hi.break_continue)[hi.break_continue->size()-1].scope;
  }
  else
  {
    label_idx = break_continue_info_stack[i].break_label_idx;
    scope = break_continue_info_stack[i].scope;
  }

  if (label_idx == 0) {
    // bug 2823: Control can reach here even while processing an
    // exception handler.
    New_LABEL (CURRENT_SYMTAB, label_idx);
    break_continue_info_stack [i].break_label_idx = label_idx;
  }

  wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);

  if (scope)
  {
    if (emit_exceptions && processing_handler)
	Cleanup_To_Scope_From_Handler (scope);
    else
    	Cleanup_To_Scope (scope);
  }
   
  WGEN_Stmt_Append (wn, Get_Srcpos());
}

static void
WGEN_Expand_Continue (void)
{
  INT32     i = break_continue_info_i;
  LABEL_IDX label_idx=0;
  gs_t      scope;

  HANDLER_INFO hi;
  if (processing_handler)
    hi = handler_stack.top();
  if (i == -1) 
  {
    FmtAssert (processing_handler, ("WGEN_Expand_Continue: No break/continue info"));
    scope = (*hi.break_continue)[hi.break_continue->size()-1].scope;
  }
  else scope = break_continue_info_stack [i].scope;
  WN *      wn;
  
  /* find the enclosing loop */
  if (i != -1) {
   while (break_continue_info_stack [i].tree_code == GS_SWITCH_STMT) --i;
   if (i != -1) { 
    label_idx = break_continue_info_stack [i].continue_label_idx;
    if (label_idx == 0) {
      // bug 2823: Control can reach here even while processing an
      // exception handler.
      New_LABEL (CURRENT_SYMTAB, label_idx);
      break_continue_info_stack [i].continue_label_idx = label_idx;
    }
   }
  }

  if (emit_exceptions && processing_handler && !label_idx)
  { // have not yet found the enclosing loop
	INT32 j = hi.break_continue->size()-1;
	while ((*hi.break_continue)[j].tree_code == GS_SWITCH_STMT) --j;
	FmtAssert (j != -1, ("Error with 'continue' in handler"));
	label_idx = (*hi.break_continue)[j].continue_label_idx;
	FmtAssert (label_idx,("WGEN_Expand_Continue: No label to goto"));
  }

  if (scope)
  {
    if (emit_exceptions && processing_handler)
	Cleanup_To_Scope_From_Handler (scope);
    else
    	Cleanup_To_Scope (scope);
  }

  wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  WGEN_Stmt_Append (wn, Get_Srcpos());
} /* WGEN_Expand_Continue */

static void
WGEN_Expand_Loop (gs_t stmt)
{
  gs_t cond, body, incr = NULL, init = NULL;

  WN * loop_stmt;
  WN * loop_test;
  WN * loop_block;
  WN * loop_body;

  TRACE_EXPAND_GS(stmt);
  WGEN_Record_Loop_Switch (gs_tree_code(stmt));

  switch (gs_tree_code(stmt)) {
    case GS_WHILE_STMT:
      cond = gs_while_cond(stmt);
      body = gs_while_body(stmt);
      break;

    case GS_DO_STMT:
      cond = gs_do_cond(stmt);
      body = gs_do_body(stmt);
      break;

    case GS_FOR_STMT:
      incr = gs_for_expr(stmt);
      cond = gs_for_cond(stmt);
      body = gs_for_body(stmt);
      for (init = gs_for_init_stmt(stmt); init; init = gs_tree_chain(init))
	WGEN_Expand_Stmt(init);
      break;

    default:
      Is_True(FALSE, ("WGEN_Expand_Loop: unexpected TREE_CODE"));
      break;
  }

#ifdef KEY
// handle "for (;;) ;"
  if (!cond) {
    loop_test = WN_Intconst (Boolean_type, 1);
  }
  else
#endif // KEY
  if (gs_tree_code(cond) == GS_TREE_LIST &&
      gs_tree_value(cond) == NULL) {
    // handle non terminating loops
    gs_t stmt;
    WN   *cond_block;
    cond_block = WN_CreateBlock ();
    WGEN_Stmt_Push (cond_block, wgen_stmk_while_cond, Get_Srcpos());
    for (stmt = gs_tree_purpose(cond); stmt; stmt = gs_tree_chain(stmt))
      WGEN_Expand_Stmt (stmt);
    WGEN_Stmt_Pop (wgen_stmk_while_cond);
    loop_test = WN_Intconst (Boolean_type, 1);
    if (WN_first (cond_block)) {
      loop_test = WN_CreateComma (OPR_COMMA, Boolean_type, MTYPE_V,
                                  cond_block, loop_test);
    }
    else
      WN_Delete (cond_block);
  }

  else
    loop_test = WGEN_Expand_Expr_With_Sequence_Point (cond, Boolean_type);   

  loop_body = WN_CreateBlock ();

  if (gs_tree_code(stmt) == GS_WHILE_STMT ||
      gs_tree_code(stmt) == GS_FOR_STMT)
    loop_stmt = WN_CreateWhileDo (loop_test, loop_body);
  else
    loop_stmt = WN_CreateDoWhile (loop_test, loop_body);

  WGEN_Stmt_Append (loop_stmt, Get_Srcpos());

  if (body) {
    WGEN_Stmt_Push (loop_body, wgen_stmk_while_body, Get_Srcpos());
    while (body) {
      WGEN_Expand_Stmt (body);
      body = gs_tree_chain(body);
    }

    if (break_continue_info_stack
	  [break_continue_info_i].continue_label_idx) {
      WGEN_Stmt_Append (
	WN_CreateLabel ((ST_IDX) 0,
			break_continue_info_stack
			  [break_continue_info_i].continue_label_idx,
			0, NULL),
	Get_Srcpos());
    }
#ifdef KEY	// bug 3265
    if (incr) {
      Push_Temp_Cleanup(incr, false);
      WGEN_One_Stmt(incr);
      Do_Temp_Cleanups(incr);
    }
#else
    if (incr)
      WGEN_One_Stmt(incr);
#endif

    WGEN_Stmt_Pop (wgen_stmk_while_body);
  }

  if (break_continue_info_stack [break_continue_info_i].break_label_idx) {
    WGEN_Stmt_Append (
      WN_CreateLabel ((ST_IDX) 0,
		      break_continue_info_stack
			[break_continue_info_i].break_label_idx,
		      0, NULL),
      Get_Srcpos());
  }

  --break_continue_info_i;
} /* WGEN_Expand_Loop */

void
WGEN_Expand_Goto (gs_t label)	// KEY VERSION
{
  WN *wn;
  bool in_handler=false;
  vector<gs_t>::reverse_iterator ci, li;
  LABEL_IDX label_idx = WGEN_Get_LABEL (label, FALSE);
  if ((CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) &&
      (LABEL_IDX_level(label_idx) < CURRENT_SYMTAB)) {
    wn = WN_CreateGotoOuterBlock (label_idx, LABEL_IDX_level(label_idx));
    Set_LABEL_target_of_goto_outer_block(label_idx);
    PU &target_pu = Get_Scope_PU(LABEL_IDX_level(label_idx));
    Set_PU_has_nonlocal_goto_label(target_pu);
    Set_PU_has_goto_outer_block (Get_Current_PU ());
  }
  else {
    gs_t scope = LABEL_SCOPE(label);
    if (scope != NULL && scope_cleanup_i != -1) {
      vector<gs_t> Label_scope_nest;
      while (scope) {
      	Label_scope_nest.push_back (scope);
        scope = PARENT_SCOPE (scope);
      }
      INT32 i = scope_cleanup_i;
      while (i != -1) {
        gs_t stmt = scope_cleanup_stack [i].stmt;
        if (gs_tree_code(stmt) == GS_BIND_EXPR ||
            gs_tree_code(stmt) == GS_HANDLER)
            break;
        --i;
      }
      vector<gs_t> Current_scope_nest;
      if (i != -1) {
      	scope = scope_cleanup_stack[i].stmt;
        while (scope) {
                Current_scope_nest.push_back (scope);
                scope = PARENT_SCOPE (scope);
        }
      }

      li=Label_scope_nest.rbegin();
      ci=Current_scope_nest.rbegin();
      for (; li!=Label_scope_nest.rend(), ci!=Current_scope_nest.rend();
      		++li, ++ci)
      	if (*li != *ci) break;
      if (ci!=Current_scope_nest.rend())
      {
        i = Maybe_Emit_Cleanups (*ci);
        if (i == -1)
        {
#ifdef FE_GNU_4_2_0
          FmtAssert (!enable_cxx_openmp,
               ("Scopes should have resolved with enable_cxx_openmp==on"));
#endif
          in_handler = true;
        }
      }
    }

  if (in_handler && (!emit_exceptions || !processing_handler))
  	DevWarn ("Goto in exception handler but exceptions not enabled?");
// If this is a handler, we have just emitted the cleanups within it. 
// Now find out what other cleanups need to be emitted for variables 
// outside the handler.
  if (in_handler && processing_handler && emit_exceptions)
  {
    HANDLER_INFO hi = handler_stack.top();

    INT32 i = hi.scope->size()-1;
    Is_True(i != -1, ("WGEN_Expand_Goto: scope_cleanup_stack empty inside handler"));
    while ((i >= 0) && ((*hi.scope) [i].stmt != *ci))
    {
      Maybe_Emit_Handler_Cleanup (i);
      --i;
    }
  }

    wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  }

  WGEN_Stmt_Append (wn, Get_Srcpos());
} /* WGEN_Expand_Goto */

static void
WGEN_Expand_Computed_Goto (gs_t exp)
{
  DevWarn ("encountered indirect jump");
  Set_PU_no_inline (Get_Current_PU ());
  WN *addr = WGEN_Expand_Expr (exp);
#ifdef KEY // bug 12839
  if (WN_operator(addr) == OPR_LDA) {
    WN_set_operator(addr, OPR_LDID);
    WN_set_desc(addr, WN_rtype(addr));
    WN_set_ty(addr, TY_pointed(WN_ty(addr)));
  }
#endif
  WN *wn   = WN_CreateAgoto (addr);
  WGEN_Stmt_Append (wn, Get_Srcpos());
} /* WGEN_Expand_Computed_Goto */



static void 
WGEN_Expand_If (gs_t stmt)
{
  WN * if_stmt;
  WN * test;
  WN * then_block;
  WN * else_block;

  test = WGEN_Expand_Expr_With_Sequence_Point(gs_if_cond(stmt), Boolean_type);
  then_block = WN_CreateBlock ();
  else_block = WN_CreateBlock ();
  if_stmt    = WN_CreateIf (test, then_block, else_block);
  WGEN_Stmt_Append (if_stmt, Get_Srcpos ());
  if (gs_then_clause(stmt)) {
    WGEN_Stmt_Push (then_block, wgen_stmk_if_then, Get_Srcpos ());
    WGEN_Expand_Stmt (gs_then_clause(stmt));
    WGEN_Stmt_Pop (wgen_stmk_if_then);
  }
  if (gs_else_clause(stmt)) {
    WGEN_Stmt_Push (else_block, wgen_stmk_if_else, Get_Srcpos());
    WGEN_Expand_Stmt (gs_else_clause(stmt));
    WGEN_Stmt_Pop (wgen_stmk_if_else);
  }
} /* WGEN_Expand_If */

void
WGEN_Expand_Label (gs_t label)
{
  LABEL_IDX label_idx = WGEN_Get_LABEL (label, TRUE);
  DECL_SYMTAB_IDX(label) = CURRENT_SYMTAB;

  if (!DECL_LABEL_DEFINED(label)) {
    WN *wn;
    DECL_LABEL_DEFINED(label) = TRUE;
    wn = WN_CreateLabel ((ST_IDX) 0, label_idx, 0, NULL);
    WGEN_Stmt_Append (wn, Get_Srcpos ());
  }
} /* WGEN_Expand_Label */

#ifdef KEY
// Returns whether a COMMA needs to be generated with comma_block as first
// operand, and wn as second operand. The caller should ensure non-empty
// comma_block.
static BOOL
comma_is_not_needed (WN * comma_block, WN * wn)
{
  Is_True (WN_first(comma_block), ("comma_block should not be empty"));

  if (WN_operator(wn) != OPR_LDID)
    return FALSE;

  WN * last_stmt = WN_last(comma_block);

  if (WN_operator(last_stmt) != OPR_STID)
    return FALSE;

  // if we have a nested COMMA expression, this comma is needed.
  if (WN_operator(WN_kid0(last_stmt)) == OPR_COMMA)
    return FALSE;

  ST * sym = WN_st(last_stmt);

  if (sym != WN_st(wn) ||
      Is_Global_Symbol(sym) || // bug 11799
      WN_offset(last_stmt) != WN_offset(wn) ||
      WN_field_id(last_stmt) != WN_field_id(wn))
    return FALSE;

  return TRUE;
}

#endif

void
WGEN_Expand_Return (gs_t stmt, gs_t retval)
{
  WN *wn = NULL;
  WN *block = NULL;

  if (retval == NULL ||
      gs_tree_code(gs_tree_type(gs_tree_type(Current_Function_Decl()))) == GS_VOID_TYPE) {
    if (retval)
      (void) WGEN_Expand_Expr_With_Sequence_Point (retval, MTYPE_V);
#ifdef KEY
    // Don't process cleanups if the return is not a full expr.
    if (gs_stmt_is_full_expr_p (stmt))
#endif
      Do_Temp_Cleanups (stmt);

#ifdef KEY
    if (enclosing_cleanup_point_expr != NULL) {		// Bug 11350
      Do_Temp_Cleanups(enclosing_cleanup_point_expr);
      enclosing_cleanup_point_expr = NULL;
    }
#endif

    Maybe_Emit_Cleanups (NULL);
#ifdef KEY
    if (emit_exceptions && processing_handler) {
	HANDLER_INFO hi = handler_stack.top();
	FmtAssert (hi.scope, ("NULL scope"));
	int j = hi.scope->size()-1;
	while (j != -1) {
	    Maybe_Emit_Handler_Cleanup (j);
	    --j;
	}
    }
#endif
    wn = WN_CreateReturn ();
  }
  else {
    WN *rhs_wn;
    TY_IDX ret_ty_idx = Get_TY(gs_tree_type(gs_tree_type(Current_Function_Decl())));

#ifdef KEY
    bool copied_return_value = FALSE;
    bool need_iload_via_fake_parm = FALSE;
    WN *target_wn = NULL;

    // If the return object must be passed through memory and the return
    // object is created by a TARGET_EXPR, have the TARGET_EXPR write directly
    // to the memory return area.
    if (TY_return_in_mem(ret_ty_idx)) {
      FmtAssert (TY_mtype (ret_ty_idx) == MTYPE_M,
	         ("WGEN_Expand_Return: return_in_mem type is not MTYPE_M"));
      // Skip the NOP_EXPRs, if any, before the TARGET_EXPR.  Bug 3448.
      gs_t t = retval;
      while (gs_tree_code(t) == GS_NOP_EXPR) {
	t = gs_tree_operand(t, 0);
      }
      if (gs_tree_code(t) == GS_TARGET_EXPR) {
	WGEN_fixup_target_expr(t);
	copied_return_value = TRUE;
      } else if (gs_tree_code(t) == GS_CALL_EXPR) {
	// Pass the first fake parm to the called function, so that the called
	// function can write the result directly to the return area.
	// Bug 12837.
	WN *first_formal = WN_formal(Current_Entry_WN(), 0);
	ST *st = WN_st(first_formal);
	target_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
	copied_return_value = TRUE;
	need_iload_via_fake_parm = TRUE;
      } else if (gs_tree_code(t) == GS_RESULT_DECL ||
               (gs_tree_code(t) == GS_INDIRECT_REF &&
                gs_tree_code(gs_tree_operand(t,0)) == GS_RESULT_DECL)) {
        // Check if we have a result_decl or if wgen converted a result_decl
        // into an indirect_ref of it.
        // In that case also the return object has been copied into the
        // first fake parm.
        copied_return_value = TRUE;
      }
    }
#endif

    rhs_wn = WGEN_Expand_Expr_With_Sequence_Point (
		retval,
		TY_mtype (ret_ty_idx)
#ifdef KEY
		, target_wn
#endif
		);

#ifdef KEY
    // rhs_wn is NULL if retval is a call_expr which writes the result directly
    // into the return area.  Manufacture a rhs_wn which is an iload of the
    // fake first parm.  Bug 12837.
    if (rhs_wn == NULL) {
      Is_True(need_iload_via_fake_parm == TRUE,
	      ("WGEN_Expand_Return: unexpected rhs_wn NULL"));
      WN *first_formal = WN_formal(Current_Entry_WN(), 0);
      ST *st = WN_st(first_formal);
      TY_IDX ty_idx = Get_TY(gs_tree_type(retval));
      WN *ldid_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
      rhs_wn = WN_Iload(TY_mtype(ty_idx), 0, ty_idx, ldid_wn);
    }
#endif


    WN * cleanup_block = WN_CreateBlock ();
    WGEN_Stmt_Push (cleanup_block, wgen_stmk_temp_cleanup, Get_Srcpos ());
#ifdef KEY
    // Don't process cleanups if the return is not a full expr.
    if (gs_stmt_is_full_expr_p(stmt))
#endif
      Do_Temp_Cleanups (stmt);

#ifdef KEY
    if (enclosing_cleanup_point_expr != NULL) {		// Bug 11350
      Do_Temp_Cleanups(enclosing_cleanup_point_expr);
      enclosing_cleanup_point_expr = NULL;
    }
#endif

    Maybe_Emit_Cleanups (NULL);
#ifdef KEY
    if (emit_exceptions && processing_handler) {
	HANDLER_INFO hi = handler_stack.top();
	FmtAssert (hi.scope, ("NULL scope"));
	int j = hi.scope->size()-1;
	while (j != -1) {
	    Maybe_Emit_Handler_Cleanup (j);
	    --j;
	}
    }
#endif
    WGEN_Stmt_Pop (wgen_stmk_temp_cleanup);

    if (WN_first (cleanup_block)) {

      if ((gs_tree_code(retval) == GS_TARGET_EXPR || 
	   gs_tree_code(retval) == GS_COMPOUND_EXPR) &&
	  WN_operator(rhs_wn) == OPR_COMMA) {

	WN * insertee = WN_kid0 (rhs_wn);
	if ((WN_has_side_effects (WN_kid1 (rhs_wn)))
#ifdef KEY
// Fix bug in which COMPOUND_EXPR has kid0==OPR_COMMA, kid1==OPR_CSELECT which
// has side-effects.
		&& (gs_tree_code(retval) == GS_TARGET_EXPR)
#endif
	) {
//	  fdump_tree (stderr, rhs_wn);
	  Fail_FmtAssertion ("WGEN_Expand_Return: TARGET_EXPR with cleanup");
	}
	WN_INSERT_BlockAfter (insertee, WN_last (insertee), cleanup_block);
      }
      else {
#ifndef KEY	// bug 3265
	if (WN_has_side_effects (rhs_wn)) {
	  DevWarn ("WGEN_Expand_Return: cleanup block and expressson has side effects");
#endif
	  ST *ret_st = Gen_Temp_Symbol (ret_ty_idx, "__return_val");
#ifdef FE_GNU_4_2_0
	  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, ret_st);
#endif
	  TYPE_ID ret_mtype = TY_mtype (ret_ty_idx);
	  WGEN_Set_ST_Addr_Saved (rhs_wn);
	  wn = WN_Stid (ret_mtype, 0, ret_st, ret_ty_idx, rhs_wn);
	  WGEN_Stmt_Append (wn, Get_Srcpos ());
	  rhs_wn = WN_Ldid (ret_mtype, 0, ret_st, ret_ty_idx);
#ifndef KEY
	}
#endif
	WGEN_Stmt_Append (cleanup_block, Get_Srcpos ());
      }
    }

    // bug 11660: remove any COMMA if it is unnecessary
    // Reworked fix extracted from PSC 3.3 beta.
    // bug 14345: removing the COMMA may cause a C++ call to reside
    // inside an exception region, and the LDID of its return value
    // outside. Fix this by grouping all statements from now until
    // the end inside BLOCK, and appending the BLOCK at the end.
    if (WN_operator(rhs_wn) == OPR_COMMA) {
      WN * comma_block = WN_kid0(rhs_wn);
      if (WN_first(comma_block) &&
          comma_is_not_needed(comma_block, WN_kid1(rhs_wn))) {
        WN * last = WN_last (comma_block);
        WN_EXTRACT_FromBlock (comma_block, last);
        block = WN_CreateBlock();
        WN_INSERT_BlockLast (block, comma_block);
        rhs_wn = WN_kid0 (last);
      }
    }

    if ((!WGEN_Keep_Zero_Length_Structs    &&
         TY_mtype (ret_ty_idx) == MTYPE_M &&
         TY_size (ret_ty_idx) == 0)
#ifdef KEY
	// Just return if the return value is already copied into the return
	// area.
        || copied_return_value
#endif
	) {
      // function returning zero length struct
      if (WN_has_side_effects (rhs_wn)) {
        rhs_wn = WN_CreateEval (rhs_wn);  
        WGEN_Stmt_Append(rhs_wn, Get_Srcpos());
      }
      wn = WN_CreateReturn ();
    }
#ifdef KEY
    else if (TY_return_in_mem(ret_ty_idx)) {
      // Copy the return value into the return area.  Based on code in
      // lower_return_val().

      FmtAssert (TY_mtype (ret_ty_idx) == MTYPE_M, 
		 ("WGEN_Expand_Return: return_in_mem type is not MTYPE_M"));

      WN *first_formal = WN_formal(Current_Entry_WN(), 0);
      TY_IDX tidx = ST_type(WN_st(first_formal));
      FmtAssert (MTYPE_is_pointer(TY_mtype(tidx)),
		 ("WGEN_Expand_Return: fake param is not a pointer"));
      WN *dest_addr = WN_CreateLdid(OPR_LDID, TY_mtype(Ty_Table[tidx]),
				    TY_mtype(Ty_Table[tidx]),
				    WN_idname_offset(first_formal),
				    WN_st(first_formal), tidx);
      // Make sure there's no copy constructor, then do regular store.
      gs_t return_type = gs_tree_type(gs_tree_type(Current_Function_Decl()));
      Is_True(!WGEN_has_copy_constructor(return_type),
	      ("WGEN_Expand_Return: return type has copy constructor"));
      // Create mstore.
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, MTYPE_M, 0, tidx, rhs_wn,
			   dest_addr, 0);
      WGEN_Stmt_Append(wn, Get_Srcpos());
      // Create return.
      wn = WN_CreateReturn ();
    }
#endif
    else if (rhs_wn) {
      WGEN_Set_ST_Addr_Saved (rhs_wn);
#ifdef KEY // bug 15176 force return type to same size as declared return type
      if (TY_mtype(ret_ty_idx) != WN_rtype(rhs_wn))
        rhs_wn = WN_Type_Conversion(rhs_wn, TY_mtype(ret_ty_idx));
#endif
      wn = WN_CreateReturn_Val(OPR_RETURN_VAL, WN_rtype(rhs_wn), MTYPE_V, rhs_wn);
    }
  }
  if (block) {
    if (wn)
      WN_INSERT_BlockLast(block, wn);
    WGEN_Stmt_Append(block, Get_Srcpos());
  } else if (wn) {
    WGEN_Stmt_Append(wn, Get_Srcpos());
  }
} /* WGEN_Expand_Return */

void
Mark_Scopes_And_Labels (gs_t stmt)
{
  if (!stmt) return;

  switch (gs_tree_code(stmt)) {
#ifdef KEY
    case GS_STATEMENT_LIST: {
	gs_t stmt_list = gs_statement_list_elts(stmt);
	gs_t list;
	BOOL new_scope = FALSE;
	// bug 11869: This example shows there may be a label statement
	// without an explicit scope. In such a scenario, a jump to such
	// a label would find incomplete scope information, and hence not
	// know what cleanups to run. Use a statement_list as a top level
	// scope for such instances, assuming every statement will at least
	// have an enclosing statement_list. GNU3 may not have any
	// enclosing statement, requiring it to use function_decl.
	if (scope_i == -1) {
	  PARENT_SCOPE(stmt) = 0;
	  Push_Scope(stmt);
	  new_scope = TRUE;
	}

	for (list = stmt_list; gs_code(list)!=EMPTY; list = gs_operand(list,1))
	  Mark_Scopes_And_Labels(gs_operand(list,0));

	if (new_scope) // End scope
	  --scope_i;
      }
      break;

    case GS_CLEANUP_STMT:
      Mark_Scopes_And_Labels(gs_tree_operand(stmt,0));
      break;

    case GS_TRY_FINALLY_EXPR:
      Mark_Scopes_And_Labels(gs_tree_operand(stmt,0));
      Mark_Scopes_And_Labels(gs_tree_operand(stmt,1));
      break;
#endif

    case GS_COMPOUND_EXPR:
      Mark_Scopes_And_Labels (gs_tree_operand(stmt, 0));
      Mark_Scopes_And_Labels (gs_tree_operand(stmt, 1));
      break;

    case GS_DO_STMT:
#ifdef KEY
    {
      gs_t body = gs_while_body(stmt);
      while (body) {
	Mark_Scopes_And_Labels (body);
  	body = gs_tree_chain(body);
      }
      break;
    }
#else
      Mark_Scopes_And_Labels (DO_BODY(stmt));
      break;
#endif

    case GS_FOR_STMT: {
      gs_t init = gs_for_init_stmt(stmt);
      gs_t cond = gs_for_cond(stmt);
      gs_t body = gs_for_body(stmt);
      while (init) {
	Mark_Scopes_And_Labels (init);
	init = gs_tree_chain(init);
      }
#ifdef KEY
// handle "for (;;) ;"
      if (cond && (gs_tree_code(cond) == GS_TREE_LIST))
#else
      if (gs_tree_code(cond) == GS_TREE_LIST)
#endif // KEY
	Mark_Scopes_And_Labels(cond);
      while (body) {
	Mark_Scopes_And_Labels (body);
  	body = gs_tree_chain(body);
      }
      break;
    }

    case GS_IF_STMT:
      Mark_Scopes_And_Labels (gs_if_cond(stmt));
      Mark_Scopes_And_Labels (gs_then_clause(stmt));
      Mark_Scopes_And_Labels (gs_else_clause(stmt));
      break;

    case GS_LABEL_EXPR:
      if (scope_i == -1)
	LABEL_SCOPE(gs_label_expr_label(stmt)) = NULL;
      else
	LABEL_SCOPE(gs_label_expr_label(stmt)) = scope_stack [scope_i];
      break;

    case GS_BIND_EXPR:
	// Begin scope
	if (scope_i != -1)
	    PARENT_SCOPE(stmt) = scope_stack[scope_i];
	else
	    PARENT_SCOPE(stmt) = 0;
	Push_Scope(stmt);
	Mark_Scopes_And_Labels (gs_bind_expr_body(stmt));
	// End scope
	SCOPE_NUMBER(scope_stack [scope_i]) = ++scope_number;
	--scope_i;
      break;

    case GS_SCOPE_STMT:
      Fail_FmtAssertion ("Mark_Scopes_And_Labels: unexpected SCOPE_STMT");
      break;

    case GS_SWITCH_STMT:
      Mark_Scopes_And_Labels (gs_bind_expr_vars(stmt));
      Mark_Scopes_And_Labels (gs_bind_expr_body(stmt));
      break;

    case GS_TREE_LIST:
      for (gs_t t = gs_tree_purpose(stmt); stmt; stmt = gs_tree_chain(stmt))
	Mark_Scopes_And_Labels(t);
      break;

    case GS_TRY_BLOCK:
    {
      Mark_Scopes_And_Labels (gs_try_stmts(stmt));
      HANDLER_ITER iter (gs_try_handlers(stmt));
      for (iter.First(); iter.Not_Empty(); iter.Next()) {
          gs_t handler = iter.Current();
	  for (gs_t t = gs_handler_body(handler); t; t = gs_tree_chain(t))
	    Mark_Scopes_And_Labels (t);
      }
      break;
    }

    case GS_WHILE_STMT:
#ifdef KEY
    {
      Mark_Scopes_And_Labels (gs_while_cond(stmt));
      gs_t body = gs_while_body(stmt);
      while (body) {
	Mark_Scopes_And_Labels (body);
  	body = gs_tree_chain(body);
      }
      break;
    }
#else
      Mark_Scopes_And_Labels (WHILE_COND(stmt));
      Mark_Scopes_And_Labels (WHILE_BODY(stmt));
      break;
#endif
    
    default:
      break;
  }
}

static void
WGEN_Expand_Start_Case (gs_t selector)
{
#ifdef KEY
  TYPE_ID index_mtype;
  if (gs_tree_code (selector) == GS_TREE_LIST)
  	index_mtype = Mtype_comparison (
                         TY_mtype (Get_TY (gs_tree_type (gs_tree_value(selector))))); 
  else
  	index_mtype = Mtype_comparison (
                         TY_mtype (Get_TY (gs_tree_type (selector)))); 
#else
  TYPE_ID index_mtype = Mtype_comparison (
                         TY_mtype (Get_TY (gs_tree_type (gs_tree_value(selector))))); 
#endif // KEY

  WN *switch_block = WN_CreateBlock ();
  WN *index;
  index = WGEN_Expand_Expr_With_Sequence_Point (selector, index_mtype);

#ifdef KEY
  // The switch index may be needed more than once if it contains case
  // range. As it may have side-effects like a function call, save the
  // index into a temporary, and used the saved value.

  // open64.net bug798. when the switch index is a constant,
  // we don't generate the index var, instead, we use constant the as switch 
  // expression directly. This helps compiler eliminates the unreachable
  // branch code even at O0 phase.

  if ( WN_operator(index) != OPR_INTCONST) {
    ST *save_expr_st = Gen_Temp_Symbol (MTYPE_TO_TY_array[index_mtype], "_switch_index");
    WN *stid = WN_Stid (index_mtype, 0, save_expr_st, MTYPE_TO_TY_array[index_mtype], index);
    WGEN_Stmt_Append(stid, Get_Srcpos());
    index = WN_Ldid(index_mtype, 0, save_expr_st, MTYPE_TO_TY_array[index_mtype]);
  }
#endif

  WGEN_Stmt_Push (switch_block, wgen_stmk_switch, Get_Srcpos());
  if (++switch_info_i == switch_info_max) {
    switch_info_max   = ENLARGE(switch_info_max);
    switch_info_stack = (SWITCH_INFO *) realloc (switch_info_stack,
                                                 switch_info_max * sizeof (SWITCH_INFO));
  }
  switch_info_stack [switch_info_i].index             = index;
  switch_info_stack [switch_info_i].start_case_index  = case_info_i + 1;
  switch_info_stack [switch_info_i].default_label_idx = 0;
  WGEN_Record_Loop_Switch (GS_SWITCH_STMT);
} /* WGEN_Expand_Start_Case */

#ifdef KEY
// Bug 14138: case ranges can be very long, so individual case statements
// should not be generated for each value in the range, neither should
// jump tables be generated in the back-end.
//
// switch (index)
//   case low ... high: goto L;
//
// Generate:
// if (index >= low && index <= high)
//   goto L
//
// Return the IF statement.
static WN *
WGEN_Expand_Case_Range (WN * index, INT64 low, INT64 high, LABEL_IDX label)
{
  const TYPE_ID mtype = WN_rtype (index);
  WN * wn_ldid1 = WN_COPY_Tree (index);
  WN * wn_ldid2 = WN_COPY_Tree (index);
  WN * goto_wn = WN_CreateGoto (label);
  WN_next (goto_wn) = WN_prev (goto_wn) = NULL;
                                                                                
  WN * if_then = WN_CreateBlock ();
  WN_first (if_then) = WN_last (if_then) = goto_wn;
                                                                                
  WN * if_else = WN_CreateBlock ();
  WN * cond = WN_CAND (WN_GE (mtype,
                              wn_ldid1,
                              WN_Intconst (mtype, low)),
                       WN_LE (mtype,
                              wn_ldid2,
                              WN_Intconst (mtype, high)));
  return WN_CreateIf (cond, if_then, if_else);
}
#endif

static void
WGEN_Expand_End_Case (void)
{
  INT32  i;
  INT32  n;
  WN    *switch_wn;
  WN    *switch_block;
  WN    *case_block;
  WN    *case_entry;
  WN    *def_goto;
  WN    *wn;
#ifdef KEY
  WN    *case_range;
#endif
  LABEL_IDX exit_label_idx;

  n = case_info_i - switch_info_stack [switch_info_i].start_case_index + 1;
  if (break_continue_info_stack [break_continue_info_i].break_label_idx)
    exit_label_idx = break_continue_info_stack [break_continue_info_i].break_label_idx;
  else
    New_LABEL (CURRENT_SYMTAB, exit_label_idx);
  if (switch_info_stack [switch_info_i].default_label_idx)
    def_goto = WN_CreateGoto (switch_info_stack [switch_info_i].default_label_idx);
  else
    def_goto = WN_CreateGoto (exit_label_idx);
  case_block = WN_CreateBlock ();
#ifdef KEY
  case_range = WN_CreateBlock ();
#endif
  for (i = switch_info_stack [switch_info_i].start_case_index;
       i <= case_info_i;
       i++) {
    INT64     case_value;
    LABEL_IDX case_label_idx = case_info_stack [i].case_label_idx;
#ifdef KEY
    INT64 low = case_info_stack[i].case_lower_bound_value;
    INT64 high = case_info_stack[i].case_upper_bound_value;
    if (low < high) {
      WN * case_range_cond = 
          WGEN_Expand_Case_Range (switch_info_stack[switch_info_i].index,
                                  low,
                                  high,
                                  case_label_idx);
      WN_INSERT_BlockLast (case_range, case_range_cond);
    }
    else {
      case_entry = WN_CreateCasegoto (low, case_label_idx);
      WN_INSERT_BlockLast (case_block, case_entry);
    }
#else
    for (case_value  = case_info_stack [i].case_lower_bound_value;
         case_value <= case_info_stack [i].case_upper_bound_value;
         case_value++) {
      case_entry = WN_CreateCasegoto (case_value, case_label_idx);
      WN_INSERT_BlockLast (case_block, case_entry);
#ifdef KEY	// bug 2814.  TODO: Port the switch-related code from kgccfe.
      if (case_value == case_info_stack[i].case_upper_bound_value)
	break;
#endif
    }
#endif
  }
  switch_wn = WN_CreateSwitch (n,
                               switch_info_stack [switch_info_i].index,
                               case_block,
                               def_goto,
                               exit_label_idx);
  switch_block = WGEN_Stmt_Pop (wgen_stmk_switch);
#ifdef KEY
  // Append any IF statements for case range in switch.
  WGEN_Stmt_Append (case_range, Get_Srcpos());
  WN_INSERT_BlockFirst (switch_block, switch_wn);
  wn = WN_CreateLabel ((ST_IDX) 0, exit_label_idx, 0, NULL);
  WN_INSERT_BlockLast (switch_block, wn);
  WGEN_Stmt_Append (switch_block, Get_Srcpos());
#else
  WGEN_Stmt_Append (switch_wn, Get_Srcpos ());
  WGEN_Stmt_Append (switch_block, Get_Srcpos ());
  wn = WN_CreateLabel ((ST_IDX) 0, exit_label_idx, 0, NULL);
  WGEN_Stmt_Append (wn, Get_Srcpos ());
#endif
  case_info_i = switch_info_stack [switch_info_i].start_case_index - 1;
  --switch_info_i;
} /* WGEN_Expand_End_Case */

static void
WGEN_Expand_Switch (gs_t stmt)
{
  WGEN_Expand_Start_Case (gs_bind_expr_vars(stmt));
  WGEN_Expand_Stmt       (gs_bind_expr_body(stmt));
  WGEN_Expand_End_Case   ();
  --break_continue_info_i;
}

#ifdef KEY
static void
WGEN_Expand_Switch_Expr (gs_t switch_expr)
{
  Is_True (gs_tree_code(switch_expr) == GS_SWITCH_EXPR,
           ("WGEN_Expand_Switch_Expr: expected switch_expr"));

  // Start switch stmt, handle condition
  WGEN_Expand_Start_Case (gs_tree_operand(switch_expr,0));

  // Switch body
  // Opnd1 == non-null is untested!
  if (gs_tree_operand(switch_expr,1))
    WGEN_Expand_Expr (gs_tree_operand(switch_expr,1), false);
  else
  {
    // Process the case labels, but don't emit the labels
    gs_t opnd2 = gs_tree_operand(switch_expr,2);
    Is_True (opnd2,
             ("WGEN_Expand_Switch_Expr: Both opnd1 and opnd2 of "
              "SWITCH_EXPR cannot be null"));
    Is_True (gs_tree_code(opnd2) == GS_TREE_VEC,
             ("WGEN_Expand_Switch_Expr: expected tree_vec"));
    for (int i = 0; i < gs_tree_vec_length (opnd2); i++)
    {
      gs_t case_label = gs_tree_vec_elt (opnd2, i);
      Is_True (gs_tree_code(case_label) == GS_CASE_LABEL_EXPR,
               ("WGEN_Expand_Switch_Expr: expected case_label_expr"));

      gs_t label = gs_case_label(case_label);
      Is_True (gs_tree_code(label) == GS_LABEL_DECL,
               ("WGEN_Expand_Switch_Expr: expected label_decl"));
      WGEN_Expand_Case (gs_case_low(case_label), gs_case_high(case_label),
                        label, TRUE);
    }

    // Stmts following this switch_expr in the surrounding statement_list
    // will make up the switch body. The case labels jump to the proper
    // label, including jumping to the proper default label. Thus, there
    // will be N>=0 elements at the end of the statement_list that are
    // outside the switch body.
  }

  // End switch stmt, generate casegoto statements
  WGEN_Expand_End_Case ();
  --break_continue_info_i;
} /* WGEN_Expand_Switch_Expr */
#endif

static void
Set_Handler_Labels (gs_t stmt)
{
  if (gs_cleanup_p (stmt))
    return;

  gs_t handlers = gs_try_handlers(stmt);

  HANDLER_ITER iter (handlers);

  for (iter.First(); iter.Not_Empty(); iter.Next()) {
    gs_t handler = iter.Current();
    LABEL_IDX handler_label;
    New_LABEL (CURRENT_SYMTAB, handler_label);
    HANDLER_LABEL(handler) = handler_label;
  }
}

INT
Current_Handler_Count()
{
#ifndef ADD_HANDLER_INFO
  return 0;
#endif
  if (temp_cleanup_i != -1) {
    for (int i = temp_cleanup_i; i != -1; --i) {
      if (temp_cleanup_stack [i].label_idx != 0)
	return 1;
    }
  }

  for (int i = scope_cleanup_i; i != -1; --i) {
    gs_t t = scope_cleanup_stack [i].stmt;
    if (gs_tree_code(t) == GS_CLEANUP_STMT)
      return 1;
    INT result = 0;
    if (gs_tree_code(t) == GS_TRY_BLOCK) {
      for (gs_t handler = gs_try_handlers(t);
           handler;
	   handler = gs_tree_chain(handler))
        ++result;
      return result;
    }
  }

  return 0;
}

static ST_IDX
Tid_For_Handler (gs_t handler)
{
  gs_t t = gs_handler_body (handler);
  while (gs_tree_code(t) != GS_COMPOUND_EXPR)
    t = gs_tree_chain(t);
  t = first_in_compound_expr(t);
  t = gs_tree_type(t);
  return t ? ST_st_idx(Get_ST (gs_tree_operand(t, 0))) : 0;
}

#ifdef ADD_HANDLER_INFO
void
Add_Handler_Info (WN * call_wn, INT i, INT num_handlers)
{
  if (temp_cleanup_i != -1) { 
    for (int i = temp_cleanup_i; i != -1; --i)
      if (temp_cleanup_stack [i].label_idx != 0) {
        WN_kid (call_wn, i++) =
          WN_CreateHandlerInfo (0,
                                temp_cleanup_stack[temp_cleanup_i].label_idx);
        return;
 
      }
  }

  int j = scope_cleanup_i;
  while (gs_tree_code(scope_cleanup_stack [j].stmt) == GS_BIND_EXPR)
    --j;
  gs_t t = scope_cleanup_stack [j].stmt;
  if (gs_tree_code(t) == GS_TRY_BLOCK && gs_tree_code(TRY_HANDLERS(t)) == GS_HANDLER) {
    for (gs_t handler = TRY_HANDLERS(t);
         handler;
         handler = gs_tree_chain(handler))
      WN_kid (call_wn, i++) =
        WN_CreateHandlerInfo (Tid_For_Handler (handler),
			      HANDLER_LABEL (handler));
    return;
  }

  WN_kid (call_wn, i++) =
    WN_CreateHandlerInfo (0, scope_cleanup_stack [j].label_idx);
}  
#endif /* ADD_HANDLER_INFO */

#ifdef KEY
// Given a type tree, return the typeinfo var for the exception tables
static gs_t
Get_typeinfo_var (gs_t t)
{
    gs_t ti_var = 0;
    if (gs_class_type_p(t))
	ti_var = gs_classtype_typeinfo_var (gs_type_main_variant(t));
    else // e.g. ordinary type
	ti_var = gs_typeinfo_decl(t);
    FmtAssert (ti_var, ("Typeinfo of handler unavailable"));
    if (gs_decl_assembler_name_set_p (ti_var) && 
    	gs_tree_not_emitted_by_gxx (ti_var) && 
	!gs_tree_symbol_referenced (gs_decl_assembler_name (ti_var)))
    {
	// Add it to the vector so that we emit them later
	gs_set_tree_not_emitted_by_gxx (ti_var, 0);
	gs_set_tree_symbol_referenced (gs_decl_assembler_name (ti_var), 1);
	gxx_emits_typeinfos (ti_var);
	// bug 11006
	// We could not have processed this before, so reset the flag.
	// Ideally, if this decl is never referenced (as asserted by the
	// above condition), then we shouldn't have got the chance to expand
	// this before, i.e. expanded_decl shouldn't have been set. But
	// in the new GNU 4 front-end, this decl comes as part of the
	// global namespace, causing us to try to process the decl before
	// this point, and fail. So we need to reset the flag for any such
	// scenario.
	expanded_decl (ti_var) = FALSE;
    }
    return ti_var;
}

// Get the handlers for the current try block. Move up in scope and append any
// more handlers that may be present, to INITV.
static INITV_IDX
Create_handler_list (int scope_index)
{
  INITV_IDX type_st, prev_type_st=0, start=0;

  FmtAssert (gs_tree_code(scope_cleanup_stack[scope_index].stmt) == GS_TRY_BLOCK,
			("EH Error"));
  for (int i=scope_index; i>=0; i--)
  {
    gs_t t = scope_cleanup_stack[i].stmt;
    if ((gs_tree_code(t) != GS_TRY_BLOCK) || gs_cleanup_p(t))	continue;

    gs_t h = gs_try_handlers (t);
    if (emit_exceptions)
      FmtAssert (h, ("Create_handler_list: Null handlers"));

    HANDLER_ITER iter (h);
    for (iter.First(); iter.Not_Empty(); iter.Next())
    {
        h = iter.Current();

	type_st = New_INITV();
        gs_t type = gs_handler_type(h);

	ST_IDX st = 0;
	if (type)
	{
	  st = ST_st_idx (Get_ST (Get_typeinfo_var(type)));
	  INITV_Set_VAL (Initv_Table[type_st], Enter_tcon (Host_To_Targ (MTYPE_U4, st)), 1);
	}
	else // catch-all handler
	  INITV_Set_ONE (Initv_Table[type_st], MTYPE_U4, 1);

	if (prev_type_st) Set_INITV_next (prev_type_st, type_st);
	else start = type_st;
	prev_type_st = type_st;
    }
  }
  if (processing_handler)
  {
	INITV_IDX next = lookup_handlers();
	if (prev_type_st) Set_INITV_next (prev_type_st, next);
	else start = next;
  }
  return start;
}

// This function is called when we are in the 'processing_handler' phase,
// i.e. we are in a catch block. Check if this corresponding try-catch
// block is inside a try block any number of levels up, and if any such
// try block is in turn contained in another try block. In that case, we need
// to try all these handlers before we can do stack unwinding.
//
// Also, if called from lookup_cleanups(), i.e. with non-NULL cleanups, append
// any relevant cleanups. Scenario:
//  try {
//    C c1;
//    try {
//      throw E();
//    } catch(...) {
//      throw E(); // lookup_cleanups() should return C 
//    }
//  } catch(...) {
//  }
//
static INITV_IDX
lookup_handlers (vector<gs_t> *cleanups)
{
    HANDLER_INFO hi = handler_stack.top();
    vector<ST_IDX> * h = hi.handler_list;
    INITV_IDX type_st, prev_type_st=0, start=0;
    for (vector<ST_IDX>::iterator i = h->begin(); i != h->end(); ++i)
    {
	type_st = New_INITV();
	ST_IDX st = *i;
	INITV_Set_VAL (Initv_Table[type_st], Enter_tcon (Host_To_Targ (MTYPE_U4, st)), 1);

	if (prev_type_st) Set_INITV_next (prev_type_st, type_st);
	else start = type_st;
	prev_type_st = type_st;
    }
    if (!start)
    {
	start = New_INITV();
	INITV_Set_ZERO (Initv_Table[start], MTYPE_U4, 1);
    }
    if (cleanups)
    {
    	vector<gs_t> * temp = hi.cleanups;
	for (vector<gs_t>::iterator j = temp->begin(); j != temp->end(); ++j)
	    cleanups->push_back (*j);
    }
    return start;
}

LABEL_IDX
New_eh_cleanup_entry (gs_t t, vector<gs_t> *v, LABEL_IDX goto_idx)
{
  EH_CLEANUP_ENTRY e;

  e.tryhandler = t;
  e.cleanups = v;
  e.goto_idx = goto_idx;
  LABEL_IDX pad;
  New_LABEL (CURRENT_SYMTAB, pad);
  Label_Table[pad].kind = LKIND_BEGIN_HANDLER;
  e.pad = pad;
  New_LABEL (CURRENT_SYMTAB, e.start);
  cleanup_list_for_eh.push_back (e);
  return pad;
}

// This is trivial now, since we have ONE specification per function,
// the offset will always be -1. Will need to calculate the offset when we
// consider more than one spec in a function
static void
append_eh_filter (INITV_IDX& iv)
{
  INITV_IDX tmp = iv;
  while (tmp && INITV_next (tmp))
	tmp = INITV_next (tmp);

  INITV_IDX eh_filter = New_INITV();
  INITV_Set_VAL (Initv_Table[eh_filter], Enter_tcon (Host_To_Targ (MTYPE_I4, -current_eh_spec_ofst)), 1);
  if (tmp) Set_INITV_next (tmp, eh_filter);
  else iv = eh_filter;
}

// Zero represents no handler, but possibly an associated landing pad.
static void
append_cleanup (INITV_IDX& iv)
{
  INITV_IDX tmp = iv;
  while (tmp && INITV_next (tmp))
	tmp = INITV_next (tmp);

  INITV_IDX cleanup = New_INITV();
  INITV_Set_VAL (Initv_Table[cleanup], Enter_tcon (Host_To_Targ (MTYPE_U4, 0)), 1);
  if (tmp) Set_INITV_next (tmp, cleanup);
  else iv = cleanup;
}

// current: D1 D2 D3, prev: D1 D2 => emit D3 for current, goto prev
// current: D1 D2 D3, prev: D1 D2 D4 => don't optimize now
static bool
optimize_cleanups (vector<gs_t> * current, vector<gs_t> * prev)
{
  if (prev->size() >= current->size())
  	return false;
  reverse (current->begin(), current->end());
  reverse (prev->begin(), prev->end());
  vector<gs_t>::iterator c = current->begin();
  for (vector<gs_t>::iterator p = prev->begin(); p != prev->end(); ++p, ++c)
  	if (*p != *c)
	    return false;
  // all cleanups in prev are in current, so remove them from current
  // first reverse it back
  reverse (current->begin(), current->end());
  reverse (prev->begin(), prev->end());
  for (int i=0; i<prev->size(); ++i)
  	current->pop_back();
  return true;
}

static bool manual_unwinding_needed (void);

LABEL_IDX
lookup_cleanups (INITV_IDX& iv)
{
  gs_t t=0;
  iv = 0;
  vector<gs_t> *cleanups = new vector<gs_t>();

  if (scope_cleanup_i == -1) 
  {
	iv = New_INITV();
	INITV_Set_ZERO (Initv_Table[iv], MTYPE_U4, 1);
	return 0;
  }
  gs_t temp_cleanup=0;
  for (int i=temp_cleanup_i; i>=0; --i)
  {
	TEMP_CLEANUP_INFO t = temp_cleanup_stack[i];
  	if (t.label_idx)
	{
		// need to call the delete operator
		temp_cleanup = temp_cleanup_stack[i].expr;
		break;
  	}
  }
  int scope_index;
  LABEL_IDX goto_idx=0;
  for (scope_index=scope_cleanup_i; scope_index>=0; scope_index--)
  {
	t = scope_cleanup_stack[scope_index].stmt;
	if (gs_tree_code(t) == GS_CLEANUP_STMT)
		cleanups->push_back (t);
	else if (gs_tree_code(t) == GS_TRY_CATCH_EXPR ||
	         gs_tree_code(t) == GS_TRY_FINALLY_EXPR)
		cleanups->push_back (gs_tree_operand(t,1));
	else if (gs_tree_code(t) == GS_TRY_BLOCK)
	    if (gs_cleanup_p(t)) cleanups->push_back (gs_try_handlers(t));
	    else break;
  	if (temp_cleanup && (cleanups->size() == 1))
	{
		cleanups->push_back (temp_cleanup);
		temp_cleanup = 0;
	}
  }
  if (temp_cleanup)
  	cleanups->push_back (temp_cleanup);
  gs_t h = 0;
  if (gs_tree_code(t) == GS_TRY_BLOCK && scope_index >= 0)
  {
	h = gs_try_handlers (t);
	iv = Create_handler_list (scope_index);
	goto_idx = scope_cleanup_stack[scope_index].cmp_idx;
  }
  else // no enclosing try block
  {
	if (processing_handler)
	{
	    iv = lookup_handlers (cleanups);
	    goto_idx = handler_stack.top().goto_idx;
	}
	else if (cleanups->empty() && eh_spec_vector.empty())
	{
	    iv = New_INITV();
	    INITV_Set_ZERO (Initv_Table[iv], MTYPE_U4, 1);
	    return 0;
	}
  }
  if (!try_block_seen && manual_unwinding_needed())
  	Set_PU_needs_manual_unwinding (Get_Current_PU());
// the following 2 calls can change 'iv'.
// NOTE: CG expects a zero before eh-spec filter
  bool cleanup_appended = false;
  if (PU_needs_manual_unwinding (Get_Current_PU()))
  {
	append_cleanup (iv);
	cleanup_appended = true;
  }
  if (processing_handler)
  {
  	vector<ST_IDX> * eh_spec = handler_stack.top().eh_spec;
	FmtAssert (eh_spec, ("Invalid eh_spec inside handler"));
	if (!eh_spec->empty())
	{
	    if (!cleanup_appended)
	    	append_cleanup (iv);
	    append_eh_filter (iv);
  	}
  }
  else if (!eh_spec_vector.empty())
  {
	if (!cleanup_appended)
	    append_cleanup (iv);
  	append_eh_filter (iv);
  }
  if (!iv)
  { // not yet assigned
	iv = New_INITV();
	INITV_Set_ZERO (Initv_Table[iv], MTYPE_U4, 1);
  }
  if (cleanup_list_for_eh.empty())
  {
	return New_eh_cleanup_entry (h, cleanups, goto_idx);
  }
  else
  {
	EH_CLEANUP_ENTRY e = cleanup_list_for_eh.back();

	// check if we are not in any try-block
	if (h == 0 && e.tryhandler == 0 && !processing_handler &&
	    cleanups->size() != e.cleanups->size())
	{
		if (optimize_cleanups (cleanups, e.cleanups))
		    return New_eh_cleanup_entry (h, cleanups, e.start);
	}

	if ((h != e.tryhandler) || // different try block
		(cleanups->size() != e.cleanups->size())) // # of cleanups doesn't match
	    	return New_eh_cleanup_entry (h, cleanups, goto_idx);
	// same tryblock, same # of cleanups
	for (int j=0; j<cleanups->size(); ++j)
	    if ((*cleanups)[j] != (*(e.cleanups))[j])
	    	return New_eh_cleanup_entry (h, cleanups, goto_idx);
	    return e.pad;
  }
}

// Called at the end of processing a try block, to check if there are
// any outer handlers, if present, store them in the current handler_info.
// wgen TODO
static void
Get_handler_list (vector<ST_IDX> *handler_list)
{
  FmtAssert (gs_tree_code(scope_cleanup_stack[scope_cleanup_i+1].stmt) == 
		GS_TRY_BLOCK, ("EH Error"));
  for (int i=scope_cleanup_i; i>=0; i--)
  {
    gs_t t = scope_cleanup_stack[i].stmt;
    if ((gs_tree_code(t) != GS_TRY_BLOCK) || gs_cleanup_p(t))	continue;

    HANDLER_ITER iter (gs_try_handlers(t));
    for (iter.First(); iter.Not_Empty(); iter.Next())
    {
        gs_t h = iter.Current();
        gs_t type = gs_handler_type(h);
        ST_IDX st = 0;	// catch-all
	if (type)
	    st = ST_st_idx (Get_ST (Get_typeinfo_var(type)));
	handler_list->push_back (st);
    }
  }
  if (processing_handler)
  {
	HANDLER_INFO hi = handler_stack.top();
	for (vector<ST_IDX>::iterator i = hi.handler_list->begin(); 
		i != hi.handler_list->end(); ++i)
	    handler_list->push_back (*i);
  }
}

// Called while processing a try-block (from WGEN_Expand_Try).
// Return in
//      CLEANUPS the set of cleanups that need to be run
//      GOTO_IDX the label to jump to
// if exception is not caught by the handlers of the current try block.
static bool
Get_Cleanup_Info (vector<gs_t> *cleanups, LABEL_IDX *goto_idx)
{
  FmtAssert (gs_tree_code(scope_cleanup_stack[scope_cleanup_i+1].stmt)==GS_TRY_BLOCK,
		("EH Processing Error"));

  for (int i=scope_cleanup_i; i>=0; i--)
  {
	gs_t t = scope_cleanup_stack[i].stmt;
	if (gs_tree_code(t) == GS_CLEANUP_STMT)
		cleanups->push_back (t);
	else if (gs_tree_code(t) == GS_TRY_CATCH_EXPR ||
	         gs_tree_code(t) == GS_TRY_FINALLY_EXPR)
		cleanups->push_back (gs_tree_operand (t, 1));
	else if (gs_tree_code(t) == GS_TRY_BLOCK)
	{ // not the outermost try block
		*goto_idx = scope_cleanup_stack[i].cmp_idx;
		return false;
	}
  }
  if (!processing_handler)
  {
	*goto_idx = 0;
	return true;
  }
  HANDLER_INFO hi = handler_stack.top();
  if (hi.handler_list->empty())
  {
	*goto_idx = 0;
	return true;
  }
  else
  {
	*goto_idx = hi.cleanups_idx;
	return false;
  }
}

// Called at the start of processing a try block
static vector<SCOPE_CLEANUP_INFO> *
Get_Scope_Info (void)
{
  vector<SCOPE_CLEANUP_INFO> *scope = new vector<SCOPE_CLEANUP_INFO>();
  if (processing_handler)
  {
    HANDLER_INFO hi = handler_stack.top();
    if (hi.scope)
      for (vector<SCOPE_CLEANUP_INFO>::iterator i = hi.scope->begin();
		i != hi.scope->end(); ++i)
	scope->push_back (*i);
  }
  FmtAssert (gs_tree_code(scope_cleanup_stack[scope_cleanup_i].stmt) 
		== GS_TRY_BLOCK, ("Scope Error in Get_Scope_Info"));
  for (int i=0; i<scope_cleanup_i; ++i) // Don't include TRY_BLOCK
	scope->push_back(scope_cleanup_stack[i]);
  return scope;
}

static vector<TEMP_CLEANUP_INFO> *
Get_Temp_Cleanup_Info (void)
{
  vector<TEMP_CLEANUP_INFO> *temp = new vector<TEMP_CLEANUP_INFO>();
  if (processing_handler)
  {
    HANDLER_INFO hi = handler_stack.top();
    if (hi.temp_cleanup)
      for (vector<TEMP_CLEANUP_INFO>::iterator i = hi.temp_cleanup->begin();
		i != hi.temp_cleanup->end(); ++i)
	temp->push_back (*i);
  }
  FmtAssert (gs_tree_code(temp_cleanup_stack[temp_cleanup_i].expr) 
		== GS_TRY_BLOCK, ("Scope Error"));
  for (int i=0; i<temp_cleanup_i; ++i)
	temp->push_back(temp_cleanup_stack[i]);
  return temp;
}

void check_for_loop_label (void)
{
  int i = break_continue_info_i;

  if (i != -1) {
   if (!break_continue_info_stack[i].break_label_idx)
      New_LABEL (CURRENT_SYMTAB, break_continue_info_stack[i].break_label_idx);

   while (break_continue_info_stack [i].tree_code == GS_SWITCH_STMT) --i;
      if (i != -1) {
    	LABEL_IDX label_idx = break_continue_info_stack [i].continue_label_idx;
    	if (label_idx == 0) {
      	    New_LABEL (CURRENT_SYMTAB, label_idx);
      	    break_continue_info_stack [i].continue_label_idx = label_idx;
    	}
      }
  }
}

static vector<BREAK_CONTINUE_INFO> *
Get_Break_Continue_Info (void)
{
  vector<BREAK_CONTINUE_INFO> *info = new vector<BREAK_CONTINUE_INFO>();

  check_for_loop_label ();
  if (processing_handler)
  {
    HANDLER_INFO hi = handler_stack.top();
    if (hi.break_continue)
      for (vector<BREAK_CONTINUE_INFO>::iterator i = hi.break_continue->begin();
		i != hi.break_continue->end(); ++i)
	info->push_back (*i);
  }
  FmtAssert (gs_tree_code(scope_cleanup_stack[scope_cleanup_i].stmt) 
		== GS_TRY_BLOCK, ("Scope Error in Get_Break_Continue_Info"));
  for (int i=0; i<=break_continue_info_i; ++i)
	info->push_back(break_continue_info_stack[i]);
  return info;
}

static bool 
manual_unwinding_needed (void)
{
  FmtAssert (!processing_handler, ("Cannot be called from inside handler"));

  if (!eh_spec_vector.empty())	return true;
  bool cleanups_seen = false;
  for (int i=scope_cleanup_i; i>=0; i--)
  {
	gs_t t = scope_cleanup_stack[i].stmt;
	if (gs_tree_code(t) == GS_CLEANUP_STMT)
 	{
		cleanups_seen = true;
		break;
	}
	if (gs_tree_code(t) == GS_TRY_BLOCK)
		Fail_FmtAssertion ("manual_unwinding_needed: Cannot reach here");
  }
  return cleanups_seen;
}

static void
Get_eh_spec (vector<ST_IDX> *in)
{
  vector<ST_IDX> * eh_spec;
  if (processing_handler)
      eh_spec = handler_stack.top().eh_spec;
  else
      eh_spec = &eh_spec_vector;
  FmtAssert (eh_spec, ("Invalid eh_spec"));
  for (int i=0; i<eh_spec->size(); ++i)
      in->push_back ((*eh_spec)[i]);
}
#endif // KEY

static void
WGEN_Expand_Try (gs_t stmt)
{
  LABEL_IDX end_label_idx;
  WN *      end_label_wn;

  /*
   * Don't generate anything if there are no statements in the
   * try-block.
   */

  if (gs_try_stmts(stmt) == NULL)
    return;

#ifdef KEY
  if (!try_block_seen)
  {
    if (manual_unwinding_needed())
	Set_PU_needs_manual_unwinding (Get_Current_PU());
    try_block_seen = true;
  }
#endif

  /* Set start labels for each handler. */
  Set_Handler_Labels(stmt);

  Push_Scope_Cleanup (stmt);

#ifdef KEY
  vector<SCOPE_CLEANUP_INFO> *scope_cleanup = Get_Scope_Info ();
// FIXME: handle temp cleanups for return from handler.
  vector<TEMP_CLEANUP_INFO> *temp_cleanup = 0;
  vector<BREAK_CONTINUE_INFO> *break_continue = Get_Break_Continue_Info ();
  int handler_count=0;
  WN * region_body;
  if (emit_exceptions)
  {
    region_body = WN_CreateBlock();
    WGEN_Stmt_Push (region_body, wgen_stmk_region_body, Get_Srcpos());
    handler_count = cleanup_list_for_eh.size();
  }
#endif // KEY

  /* Generate code for the try-block. */

  for (gs_t s = gs_try_stmts(stmt); s; s = gs_tree_chain(s))
    WGEN_Expand_Stmt(s);
  --scope_cleanup_i;

#ifdef KEY
  LABEL_IDX start = 0;
  if (emit_exceptions)
  {
    WGEN_Stmt_Pop (wgen_stmk_region_body);
    WN * region_pragmas = WN_CreateBlock();
    FmtAssert (cleanup_list_for_eh.size() >= handler_count, ("Cleanups cannot be removed here"));
    LABEL_IDX cmp_idx = scope_cleanup_stack[scope_cleanup_i+1].cmp_idx;
    // We want to process any EH cleanups that have been introduced by
    // the statements above, and not by any nested TRY block.
    // When enabled by enable_cxx_openmp,
    //   cleanups generated while processing a TRY block are handled at
    //   that point, and any cleanups generated by an enclosing TRY block
    //   will be handled during the enclosing TRY block.
    //
    //   cleanup_list_for_eh is updated to not contain any entries generated
    //   by a nested TRY block.
    if (cleanup_list_for_eh.size() > handler_count)
    {
	std::list<EH_CLEANUP_ENTRY>::iterator iter = cleanup_list_for_eh.begin();
    	for (int incr=0; incr<handler_count; ++incr)
	    ++iter;
    	for (; iter != cleanup_list_for_eh.end(); ++iter)
	{
	    EH_CLEANUP_ENTRY entry = *iter;
	    WN_INSERT_BlockLast (region_pragmas, WN_CreateGoto (entry.pad));
	}
    }
    else // ==
    {
	// bug 4550: use a new label to mark handler-begin, don't use
	// the existing cmp_idx since we may have goto to it.
	// i.e. generate 
	// LABEL L1 2
	// LABEL L2
	New_LABEL (CURRENT_SYMTAB, start);
    	Set_LABEL_KIND (Label_Table[start], LKIND_BEGIN_HANDLER);
    	WN_INSERT_BlockLast (region_pragmas, WN_CreateGoto (start));
    }

    // insert the label to go to for an inlined callee
    // This inito is not being used right now.
    TY_IDX ty = MTYPE_TO_TY_array[MTYPE_U4];
    ST * ereg = Gen_Temp_Named_Symbol (ty, "try_label", CLASS_VAR,
                                SCLASS_EH_REGION_SUPP);
    Set_ST_is_initialized (*ereg);
    Set_ST_is_not_used (*ereg);
    INITV_IDX try_label = New_INITV();
    INITV_Init_Label (try_label, cmp_idx, 1);
    INITO_IDX ereg_supp = New_INITO (ST_st_idx(ereg), try_label);
    WGEN_Stmt_Append (WN_CreateRegion (REGION_KIND_TRY, region_body,
    	region_pragmas, WN_CreateBlock(), New_Region_Id(), ereg_supp), 
	Get_Srcpos());
    Set_PU_has_region (Get_Current_PU());
  }
  vector<gs_t> *cleanups = new vector<gs_t>();
  LABEL_IDX cmp_idxs[2];
  cmp_idxs[0] = scope_cleanup_stack[scope_cleanup_i+1].cmp_idx;
  cmp_idxs[1] = start;
  LABEL_IDX goto_idx=0;
  bool outermost = 0;
  if (emit_exceptions) outermost = Get_Cleanup_Info (cleanups, &goto_idx);
  vector<ST_IDX> *handler_list = new vector<ST_IDX>();
  vector<ST_IDX> * eh_spec_list = NULL;
  if (emit_exceptions) 
  {
    Get_handler_list (handler_list);
    eh_spec_list = new vector<ST_IDX>();
    Get_eh_spec (eh_spec_list);
  }
#endif // KEY

  /* Generate a label for the handlers to branch back to. */

  New_LABEL (CURRENT_SYMTAB, end_label_idx);

#ifdef FE_GNU_4_2_0
  if (enable_cxx_openmp)
  {
    /* Jump to code after the try block, in case no exception was thrown. */
    WGEN_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, end_label_idx), Get_Srcpos());
  }
#endif

  /* Handler code will be generated later, at end of function. */

#ifdef KEY
  Push_Handler_Info (gs_try_handlers(stmt), cleanups, scope_cleanup, temp_cleanup,
	break_continue, handler_list, eh_spec_list, end_label_idx, outermost, 
	cmp_idxs, goto_idx);
#else
  Push_Handler_Info (gs_try_handlers(stmt), end_label_idx);
#endif // KEY

#ifdef FE_GNU_4_2_0
  if (enable_cxx_openmp)
    Do_Handlers(handler_count); // pass the # of cleanups to skip from start
#endif

  /* Emit label after handlers. */

  end_label_wn = WN_CreateLabel ((ST_IDX) 0, end_label_idx, 0, NULL);
  WGEN_Stmt_Append (end_label_wn, Get_Srcpos());
} /* WGEN_Expand_Try */

#ifdef KEY
static int
sizeof_eh_spec (gs_t t)
{
  int i=1;
  for (; t; t = gs_tree_chain(t), i++) ;
  return i;
}
#endif

static void
WGEN_Expand_EH_Spec (gs_t stmt)
{
      // This is what g++'s genrtl_eh_spec_block routine (in cp/semantics.c)
      // does:
      //   expand_eh_region_start ();
      //   expand_stmt (EH_SPEC_STMTS (t));
      //   expand_eh_region_end_allowed (...);
#ifdef KEY
      int bkup = current_eh_spec_ofst;
      int initial_size = eh_spec_vector.size();
      if (emit_exceptions)
      {
        // Generally, there is 1 exception specification per function.
        // After inlining (by the GNU front-end or inliner/ipa), the caller
        // function can have multiple specifications. Any inlining by GNU is
        // taken care of here by updating current_eh_spec_ofst.
        // TODO: cmp with -1 before calling unexpected needs to be changed.
        gs_t eh_spec = gs_eh_spec_raises (stmt);
        current_eh_spec_ofst = initial_size+1;
        if (eh_spec_vector.empty())
          eh_spec_vector.reserve (sizeof_eh_spec (eh_spec));
        for (; eh_spec; eh_spec = gs_tree_chain (eh_spec))
        {
          ST_IDX type_st = ST_st_idx (Get_ST ( 
			Get_typeinfo_var(gs_tree_value(eh_spec))));
          eh_spec_vector.push_back (type_st);
	  eh_spec_func_end.push_back (type_st);

          TYPE_FILTER_ENTRY e;
          e.st = type_st;
          e.filter = 0; // do not compare based on filter
          vector<TYPE_FILTER_ENTRY>::iterator f = find(type_filter_vector.begin(), type_filter_vector.end(), e);
          if (f == type_filter_vector.end())
          {
	    e.filter = type_filter_vector.size()+1;
      	    type_filter_vector.push_back (e);
	  }
        }
        eh_spec_vector.push_back (0); // terminator
        eh_spec_func_end.push_back (0);
      }
#endif
#ifdef KEY
      // Bug 8523: This is a #list# of statements, and needs to be iterated through.
      //
      //           (Else, the expansion will happen only for the first statement,
      //           and WGEN_Expand_Stmt() will return right after! The rest of the
      //           statements in the EH_SPEC_STMTS list will be  d r o p p e d  !
      //           (If the first statement were a compound statement, it will not 
      //           immediately be obvious that statements after the first statement
      //           in the EH_SPEC_STMTS are being skipped)).
      //
      gs_t eh_spec_stmt;
      for (eh_spec_stmt = gs_eh_spec_stmts (stmt); eh_spec_stmt != NULL; eh_spec_stmt = gs_tree_chain(eh_spec_stmt))
        WGEN_Expand_Stmt (eh_spec_stmt);
#else
      WGEN_Expand_Stmt (EH_SPEC_STMTS (stmt));
#endif
#ifdef KEY
      if (emit_exceptions)
      { // now clear eh_spec_vector, eh_spec_func_end stays.
      	if (!initial_size) eh_spec_vector.clear();
	else
	{
	    int current_size = eh_spec_vector.size();
	    for (int i=initial_size; i<current_size; ++i)
	    	eh_spec_vector.pop_back();
      	}
      }
      current_eh_spec_ofst = bkup;
#endif
}

static void
Call_Named_Function (ST * st)
{
  WN * call_wn = WN_Create (OPR_CALL, MTYPE_V, MTYPE_V, 0);
  WN_st_idx (call_wn) = ST_st_idx (st);
  WGEN_Stmt_Append (call_wn, Get_Srcpos());
}

void
Call_Throw (void)
{
}

void
Call_Rethrow (void)
{
}

void
Call_Terminate (void)
{
  static ST * st = NULL;
  if (st == NULL) {
    st = Function_ST_For_String ("_ZSt9terminatev");
  }
  Call_Named_Function (st);
}

#ifdef KEY
static void Generate_filter_cmp (int filter, LABEL_IDX goto_idx);
static WN *
Generate_cxa_call_unexpected (void)
{
  ST_IDX exc_ptr_param = Get_exception_pointer_symbol ();
  ST exc_st = St_Table[exc_ptr_param];
  WN* parm_node = WN_Ldid (Pointer_Mtype, 0, &exc_st, ST_type (exc_st));

  TY_IDX idx;
  TY &ptr_ty = New_TY (idx);
  TY_Init (ptr_ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
                        Save_Str ("anon_ptr."));
                                                                                
  ptr_ty.Set_pointed (ST_type(exc_st));
                                                                                
  WN * arg0 = WN_CreateParm (Pointer_Mtype, parm_node, idx, WN_PARM_BY_VALUE);
                                                                                
  ST * st = Function_ST_For_String("__cxa_call_unexpected");
  WN * call_wn = WN_Create (OPR_CALL, Pointer_Mtype, MTYPE_V, 1);
  WN_kid0 (call_wn) = arg0;
  WN_st_idx (call_wn) = ST_st_idx (st);
  return call_wn;
}

static void
Generate_unwind_resume (void)
{
  ST_IDX exc_ptr_param = Get_exception_pointer_symbol ();
  ST exc_st = St_Table[exc_ptr_param];
  WN* parm_node = WN_Ldid (Pointer_Mtype, 0, &exc_st, ST_type (exc_st));

  TY_IDX idx;
  TY &ptr_ty = New_TY (idx);
  TY_Init (ptr_ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
                        Save_Str ("anon_ptr."));
                                                                                
  ptr_ty.Set_pointed (ST_type(exc_st));
                                                                                
  WN * arg0 = WN_CreateParm (Pointer_Mtype, parm_node, idx, WN_PARM_BY_VALUE);
                                                                                
  ST * st = Function_ST_For_String("_Unwind_Resume");
  WN * call_wn = WN_Create (OPR_CALL, Pointer_Mtype, MTYPE_V, 1);
  WN_kid0 (call_wn) = arg0;
  WN_st_idx (call_wn) = ST_st_idx (st);
  WN_Set_Call_Never_Return (call_wn);

// Before calling _Unwind_Resume(), if we have eh-spec, compare filter with
// -1, goto __cxa_call_unexpected call if required. Otherwise fall-through.
  WN *call_unexpected;
  LABEL_IDX goto_unexpected;
  if (!eh_spec_func_end.empty())
  {
	// TODO: The hard-coded -1 most probably needs to be changed to
	// properly handle GNU inlining.
	New_LABEL (CURRENT_SYMTAB, goto_unexpected);
	Generate_filter_cmp (-1, goto_unexpected);
	call_unexpected = Generate_cxa_call_unexpected ();
  }

  if (emit_exceptions)
  	WGEN_Stmt_Push (WN_CreateBlock(), wgen_stmk_region_body, Get_Srcpos());
  WGEN_Stmt_Append (call_wn, Get_Srcpos());
  if (emit_exceptions)
  	Setup_EH_Region (1 /* for _Unwind_Resume */);
// We would ideally want to put it inside the above region, but we cannot
// jmp from outside a region into it.
  if (!eh_spec_func_end.empty())
  {
  	WGEN_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, goto_unexpected, 0, NULL),
    		Get_Srcpos());
  	if (emit_exceptions)
  	    WGEN_Stmt_Push (WN_CreateBlock(), wgen_stmk_region_body, Get_Srcpos());
  	WGEN_Stmt_Append (call_unexpected, Get_Srcpos());
  	if (emit_exceptions)
  	    Setup_EH_Region (1 /* for __cxa_call_unexpected */);
  }
}

static void
Generate_filter_cmp (int filter, LABEL_IDX goto_idx)
{
  ST_IDX filter_param = Get_exception_filter_symbol ();
  const TYPE_ID mtype = TARGET_64BIT ? MTYPE_U8 : MTYPE_U4;
  
  WN * wn_ldid = WN_Ldid (mtype, 0, &St_Table[filter_param],
                                                MTYPE_TO_TY_array[mtype]);
  WN * goto_wn = WN_CreateGoto (goto_idx);
  WN_next (goto_wn) = WN_prev (goto_wn) = NULL;
                                                                                
  WN * if_then = WN_CreateBlock ();
  WN_first (if_then) = WN_last (if_then) = goto_wn;
                                                                                
  WN * if_else = WN_CreateBlock ();
  WN * cmp_value = WN_Intconst (mtype, filter);
  WN * cond = WN_Create (OPR_EQ, WN_rtype (wn_ldid), mtype, 2);
  WN_kid0 (cond) = wn_ldid;
  WN_kid1 (cond) = cmp_value;
                                                                                
  WN * if_blk = WN_CreateIf (cond, if_then, if_else);
                                                                                
  WGEN_Stmt_Append (if_blk, Get_Srcpos());
}
#endif // KEY


// for a catch-all clause, pass a typeinfo of ZERO. This typeinfo needs
// to be handled specially. Moreover, we must not pass 0 for any other
// typeinfo.
static void
WGEN_Expand_Handlers_Or_Cleanup (const HANDLER_INFO &handler_info)
{
  gs_t t = handler_info.handler;
  vector<gs_t> *cleanups = handler_info.cleanups;
  LABEL_IDX label_idx = handler_info.label_idx;
  LABEL_IDX goto_idx = handler_info.goto_idx;
  LABEL_IDX cleanups_idx = handler_info.cleanups_idx;
  bool outermost = handler_info.outermost;
#ifndef KEY
  WGEN_Stmt_Append (
    WN_CreateLabel ((ST_IDX) 0, HANDLER_LABEL(t), 0, NULL),
    Get_Srcpos());
#endif // !KEY
  
  if (gs_tree_code(t) == GS_HANDLER || gs_tree_code(t) == GS_STATEMENT_LIST) {

#ifdef KEY
    HANDLER_ITER iter(t);
    if (emit_exceptions)
    {
      // Generate the compare statements with eh-filter.
      for (iter.First(); iter.Not_Empty(); iter.Next())
      {
        gs_t t_copy = iter.Current();
        gs_t type = gs_handler_type(t_copy);
        ST_IDX  sym = 0;
	if (type) sym = ST_st_idx (Get_ST (Get_typeinfo_var(type)));
        TYPE_FILTER_ENTRY e;
        e.st = sym;
        e.filter = 0; // do not compare based on filter
        vector<TYPE_FILTER_ENTRY>::iterator f = find(type_filter_vector.begin(), type_filter_vector.end(), e);
        if (f == type_filter_vector.end())
        {
	  e.filter = type_filter_vector.size()+1;
      	  type_filter_vector.push_back (e);
	  if (e.st)
	  	Generate_filter_cmp (e.filter, HANDLER_LABEL(t_copy));
	  else // catch-all, so do not compare filter
      		WGEN_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, 
				HANDLER_LABEL(t_copy)), Get_Srcpos());
        }
        else 
	{
	  if (e.st)
	  	Generate_filter_cmp ((*f).filter, HANDLER_LABEL(t_copy));
	  else // catch-all, so do not compare filter
      		WGEN_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, 
				HANDLER_LABEL(t_copy)), Get_Srcpos());
	}
      }

  WGEN_Stmt_Append (
    WN_CreateLabel ((ST_IDX) 0, cleanups_idx, 0, NULL), Get_Srcpos());
// Generate any cleanups that need to be executed before going to the outer
// scope, which would be a handler in the same PU or a call to _Unwind_Resume
      in_cleanup = TRUE;
      for (vector<gs_t>::iterator j=cleanups->begin();
		j!=cleanups->end(); ++j)
    	  WGEN_One_Stmt_Cleanup (gs_cleanup_expr (*j));

      in_cleanup = FALSE;
// generate a call to _Unwind_Resume(struct _Unwind_Exception *)
      if (outermost)
      {
	FmtAssert (goto_idx == 0, ("Goto label should be 0"));
	Generate_unwind_resume ();
      }
      else
      	WGEN_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, goto_idx), Get_Srcpos());
    } // emit_exceptions
#endif // KEY
    // Now, emit the actual exception handler body's.
    for (iter.First(); iter.Not_Empty(); iter.Next()) {
      t = iter.Current();
#ifdef KEY
// need a label in front of each handler, so that we can jump to the
// proper label from 'cmp' above
  WGEN_Stmt_Append (
    WN_CreateLabel ((ST_IDX) 0, HANDLER_LABEL(t), 0, NULL), Get_Srcpos());
#endif
      // Bug 11006: For a catch-all handler, GNU4 does not have a bind_expr
      // statement (because there is no variable to bind to) which helps
      // us to track the scope. So use the handler statement to also
      // indicate a new scope.
      Push_Scope_Cleanup(t);
      gs_t body = gs_handler_body(t);
      for (; body; body = gs_tree_chain(body))
	WGEN_Expand_Stmt (body);
      // Bug 11006: pop handler scope.
      Pop_Scope_And_Do_Cleanups();
      WGEN_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, label_idx),
		       Get_Srcpos());
    }
  } else {
// We will see if control reaches here.
// Let me comment this out, may need to do something else later.
      //Fail_FmtAssertion ("Handle it");
  }    
}

#ifdef FE_GNU_4_2_0
#include "omp_types.h"
#include "omp_directive.h"
static void
WGEN_Expand_Omp (gs_t stmt)
{
  switch (gs_tree_code(stmt))
  {
    case GS_OMP_PARALLEL:
      expand_start_parallel_or_combined_parallel (stmt);
      break;

    case GS_OMP_CRITICAL:
      expand_start_critical (stmt);
      break;

    case GS_OMP_SECTIONS:
      expand_start_sections (stmt);
      break;

    case GS_OMP_SECTION:
      expand_start_section (stmt);
      break;

    case GS_OMP_SINGLE:
      expand_start_single (stmt);
      break;

    case GS_OMP_FOR:
      expand_start_for (stmt);
      break;

    case GS_OMP_MASTER:
      expand_start_master (stmt);
      break;

    case GS_OMP_ORDERED:
      expand_start_ordered (stmt);
      break;

    case GS_OMP_ATOMIC:
      expand_start_atomic (stmt);
      break;

    default:
      Fail_FmtAssertion ("Unexpected stmt node");
  }
}

void
WGEN_Expand_DO (gs_t stmt)
{
  Is_True (gs_tree_code(stmt) == GS_OMP_FOR, ("Unexpected tree code"));
  gs_t init, incr, cond, body;

  WGEN_Record_Loop_Switch  (GS_DO_STMT);

  init = gs_omp_for_init (stmt);
  incr = gs_omp_for_incr (stmt);
  cond = gs_omp_for_cond (stmt);
  body = gs_omp_for_body (stmt);
  expand_start_do_loop (init, cond, incr);
  while (body)
  {
    WGEN_Expand_Stmt (body);
    body = gs_tree_chain(body);
  }

  // label for continue
  if (break_continue_info_stack [break_continue_info_i].continue_label_idx)
  {
      WGEN_Stmt_Append (
	WN_CreateLabel ((ST_IDX) 0,
			break_continue_info_stack
			  [break_continue_info_i].continue_label_idx,
			0, NULL),
	Get_Srcpos());
  }

  // loop ends
  expand_end_do_loop ();

  // label for break
  if (break_continue_info_stack [break_continue_info_i].break_label_idx)
  {
      WGEN_Stmt_Append (
	WN_CreateLabel ((ST_IDX) 0,
			break_continue_info_stack
			  [break_continue_info_i].break_label_idx,
			0, NULL),
	Get_Srcpos());
  }
  --break_continue_info_i;
}
#endif


//*************************************************************************
// to handle the statement
//*************************************************************************
void
WGEN_Expand_Stmt(gs_t stmt, WN* target_wn)
{
    TRACE_EXPAND_GS(stmt);
    if (gs_tree_code(stmt) == GS_LABEL_DECL)
      lineno = gs_decl_source_line(stmt);
    else
    if (gs_tree_code(stmt) != GS_CASE_LABEL_EXPR) {
      if (gs_tree_has_location(stmt) == gs_true) // it would otherwise be -1
	lineno = gs_expr_lineno(stmt);
    }
    //bug 8895, 10632: update current file
    if(gs_tree_has_location(stmt) == gs_true)
     WGEN_Set_Line_And_File (lineno, gs_expr_filename(stmt), TRUE);

    WGEN_Guard_Init_Block_Push(); 
    if (lang_cplus) 
      switch (gs_tree_code_class(stmt)){
      case GS_TCC_REFERENCE:
      case GS_TCC_COMPARISON:
      case GS_TCC_UNARY:
      case GS_TCC_BINARY:
      case GS_TCC_STATEMENT:
      case GS_TCC_EXPRESSION:
	if (gs_stmt_is_full_expr_p(stmt))
	  Push_Temp_Cleanup (stmt, false);
	break;
      default: ;
      }

    switch(gs_tree_code(stmt)){
    case GS_ASM_EXPR:
      Wgen_Expand_Asm_Operands(gs_asm_string     (stmt),
			       gs_asm_outputs    (stmt),
			       gs_asm_inputs     (stmt),
			       gs_asm_clobbers   (stmt),
			       gs_asm_volatile_p (stmt),
			       gs_expr_filename  (stmt),
			       gs_expr_lineno    (stmt));
      break;

    case GS_BREAK_STMT:
      WGEN_Expand_Break ();
      break;

    case GS_CASE_LABEL_EXPR:
#ifdef KEY
      WGEN_Expand_Case (gs_case_low(stmt), gs_case_high(stmt),
                        gs_case_label(stmt));
#else
      WGEN_Expand_Case (gs_case_low(stmt), gs_case_high(stmt));
#endif
      break;

    case GS_CLEANUP_STMT:
      if (opt_regions && Check_For_Call_Region ())
          Did_Not_Terminate_Region = FALSE;
      if (!gs_cleanup_eh_only(stmt))
    	  Push_Scope_Cleanup (stmt);
      else Push_Scope_Cleanup (stmt, true /* cleanup_eh_only */);
      // Expand CLEANUP_BODY
      WGEN_Expand_Stmt (gs_cleanup_body(stmt), target_wn);
      break;

    case GS_COMPOUND_EXPR: 
      WGEN_Expand_Stmt (gs_tree_operand(stmt, 0), target_wn);
      WGEN_Expand_Stmt (gs_tree_operand(stmt, 1), target_wn);
      break;

    case GS_CONTINUE_STMT:
      WGEN_Expand_Continue ();
      break;

    case GS_DECL_EXPR:
      // If the node is an INDIRECT_REF, then it's because we changed it from a
      // VAR_DECL to an INDIRECT_REF for the named return value optimization.
      // In this case, do nothing if the decl has no initializer; otherwise,
      // expand named_ret_obj_initalizer, which is the initializer that we've
      // save as a TARGET_EXPR.
      if (gs_tree_code(gs_decl_expr_decl(stmt)) == GS_INDIRECT_REF) {
	if (named_ret_obj_initializer) {
	  WGEN_Expand_Expr(named_ret_obj_initializer);
	}
	break;
      }
      WGEN_Expand_Decl (gs_decl_expr_decl (stmt), FALSE);
      break;

    case GS_DO_STMT:
      WGEN_Expand_Loop (stmt);
      break;

    case GS_EXPR_STMT:
      WGEN_One_Stmt (gs_expr_stmt_expr(stmt), target_wn);
      break;

    case GS_FOR_STMT:
      WGEN_Expand_Loop (stmt);
      break;

    case GS_GOTO_EXPR: {
      gs_t dest = gs_tree_operand(stmt, 0);
      if (gs_tree_code(dest) == GS_LABEL_DECL)
        WGEN_Expand_Goto (dest);
      else
        WGEN_Expand_Computed_Goto(dest);
      break;
      }

    case GS_IF_STMT:
      WGEN_Expand_If (stmt);
      break;

    case GS_LABEL_EXPR:
      WGEN_Expand_Label (gs_bind_expr_vars(stmt));
      break;

    case GS_RETURN_EXPR: {
      if (opt_regions && Check_For_Call_Region ())
          Did_Not_Terminate_Region = FALSE;
      gs_t t = gs_bind_expr_vars(stmt);
      if (t && gs_tree_code(t) == GS_INIT_EXPR) {
  	Is_True(gs_tree_code(gs_tree_operand(t, 0)) == GS_RESULT_DECL,
			  ("WGEN_Expand_Stmt: expected RESULT_DECL"));
	gs_t t1 = gs_tree_operand(t, 1);
	if (gs_tree_code(t1) == GS_TARGET_EXPR)
  	  gs_set_tree_operand(t1, 2, 0);
	WGEN_Expand_Return (stmt, t1);
      }
      else
	WGEN_Expand_Return(stmt, t);
      return; // We've already called Do_Temp_Cleanups!
    }

    case GS_SCOPE_STMT:
      // wgen TODO: gcc 4.x does not have any SCOPE_STMT, we need to handle
      // all the code executed for SCOPE_STMT.
      Fail_FmtAssertion ("WGEN_Expand_Stmt: Unexpected SCOPE_STMT");
      break;

    case GS_SWITCH_STMT:
      WGEN_Expand_Switch (stmt);
      break;

    case GS_TRY_BLOCK:
      WGEN_Expand_Try (stmt);
      break;

    case GS_WHILE_STMT:
      WGEN_Expand_Loop (stmt);
      break;


    case GS_EH_SPEC_BLOCK:
#ifdef FE_GNU_4_2_0
      // Bug 12603: GNU 4.2.0 does not enclose the statments in EH_SPEC_BLOCK
      // with a BIND_EXPR, so we need to enclose them within a scope here.
      Push_Scope_Cleanup (stmt);
#endif
      WGEN_Expand_EH_Spec (stmt);
#ifdef FE_GNU_4_2_0
      Pop_Scope_And_Do_Cleanups (); // bug 12603
#endif
      break;

    case GS_USING_STMT:
      break;

#ifdef FE_GNU_4_2_0
    case GS_OMP_PARALLEL:
    case GS_OMP_CRITICAL:
    case GS_OMP_SECTION:
    case GS_OMP_SECTIONS:
    case GS_OMP_SINGLE:
    case GS_OMP_FOR:
    case GS_OMP_MASTER:
    case GS_OMP_ORDERED:
    case GS_OMP_ATOMIC:
      WGEN_Expand_Omp (stmt);
      break;
#endif

    case GS_STATEMENT_LIST: {
	gs_t stmt_list = gs_statement_list_elts(stmt);
	gs_t list;
	for (list = stmt_list; gs_code(list)!=EMPTY; list = gs_operand(list,1))
	  WGEN_Expand_Stmt(gs_operand(list,0), target_wn);
      }
      break;

    case GS_BIND_EXPR: {
	gs_t body = gs_bind_expr_body(stmt);
	// Begin new scope
	Push_Scope_Cleanup (stmt);
	WGEN_Expand_Stmt (body);
	// End scope
	Pop_Scope_And_Do_Cleanups ();
      }
      break;

#ifdef KEY
    case GS_SWITCH_EXPR:
      WGEN_Expand_Switch_Expr (stmt);
      break;
#endif

    default:
      WGEN_One_Stmt (stmt, target_wn);
      break;
    }

    if (lang_cplus)
      switch (gs_tree_code_class(stmt)){
      case GS_TCC_REFERENCE:
      case GS_TCC_COMPARISON:
      case GS_TCC_UNARY:
      case GS_TCC_BINARY:
      case GS_TCC_STATEMENT:
      case GS_TCC_EXPRESSION:
	if (gs_stmt_is_full_expr_p(stmt))
	  Do_Temp_Cleanups(stmt);
	break;
      default: ;
      }
     WGEN_Guard_Init_Block_Pop(); 
} /* WGEN_Expand_Stmt */

// RETVAL is a TARGET_EXPR that generates the function return value.  The
// return value is to be returned in the memory pointed to by the fake first
// parm which was inserted by the WHIRL front-end.  This routine transforms the
// GCC tree to make the TARGET_EXPR write the result directly to the memory
// pointed to by the fake parm, instead of having TARGET_EXPR first write to a
// temp var and then copy from the temp var to the return area.  Inserting this
// copy can be incorrect since the copy may involve a copy constructor.  An
// example transformation:
//
//    before                         after
// ------------------------------------------------------
//  return_stmt                   return_stmt
//    init_expr                     init_expr
//      result_decl                   result_decl
//      target_expr                   target_expr
//        var_decl x (decl)             indirect_ref (of fake first parm)
//        compound_expr (init)          compound_expr
//          call_expr                     call_expr
//            addr_expr                     addr_expr
//              var_decl x                    indirect_ref (of fake first parm)
//          var_decl x                      indirect_ref (of fake first parm)
//        call_expr (clnp)              call_expr
//          addr_expr                     addr_expr
//            var_decl x                    indirect_ref (of fake first parm)
//
// In the original tree, all the "var_decl x" nodes are really one single node
// that is referenced many times.  By changing the node from var_decl to
// indirect_ref, all references of x are changed to references of the memory
// pointed to by the fake first parm.
static void
WGEN_fixup_target_expr (gs_t retval)
{
  gs_t decl = gs_tree_operand(retval, 0);
  FmtAssert(gs_tree_code(decl) == GS_VAR_DECL,
	    ("WGEN_fixup_target_expr: VAR_DECL not found in TARGET_EXPR"));

  // Get the ST for the fake first parm.
  WN *first_formal = WN_formal(Current_Entry_WN(), 0);

  // Change the TARGET_EXPR's DECL to be an INDIRECT_REF of the fake first
  // parm.
  gs_t ptr_var = gs_build_decl(GS_VAR_DECL,
  			    gs_build_pointer_type(gs_tree_type(retval)));
  _gs_code(decl, GS_INDIRECT_REF);
  gs_set_tree_operand(decl, 0, ptr_var);
  set_DECL_ST(ptr_var, WN_st(first_formal));
}

// Return TRUE if TYPE has copy constructor.
bool
WGEN_has_copy_constructor (gs_t type)
{
  if (gs_class_type_p(type) &&
      gs_classtype_copy_constructor(type) != NULL)
    return TRUE;

  return FALSE;
}

bool
// Looks up the current scope we are in. Returns true if the current scope
// already has alloca, otherwise false. Modifies parameter idx.
Set_Current_Scope_Has_Alloca (INT & idx)
{
  int i = scope_cleanup_i;
  while (i != -1)
  {
    gs_t stmt = scope_cleanup_stack[i].stmt;
    if (gs_tree_code (stmt) == GS_BIND_EXPR ||
        gs_tree_code (stmt) == GS_HANDLER)
      break;
    i--;
  }
  Is_True (i != -1, ("No scope stmt available"));
  // return the idx for the scope
  idx = i;
  if (scope_cleanup_stack[i].vla.has_alloca) return TRUE;
  scope_cleanup_stack[i].vla.has_alloca = TRUE;
  return FALSE;
}

// Save the original sp for scope 'idx'
void
Set_Current_Scope_Alloca_St (ST * st, int idx)
{
  Is_True (gs_tree_code (scope_cleanup_stack[idx].stmt) == GS_BIND_EXPR ||
           gs_tree_code (scope_cleanup_stack[idx].stmt) == GS_HANDLER,
  	("Unexpected tree code"));
  scope_cleanup_stack[idx].vla.alloca_st = st;
}

// Save st's for kids 1..n of DEALLOCA for scope 'idx'
void
Add_Current_Scope_Alloca_St (ST * st, int idx)
{
  Is_True (gs_tree_code (scope_cleanup_stack[idx].stmt) == GS_BIND_EXPR ||
           gs_tree_code (scope_cleanup_stack[idx].stmt) == GS_HANDLER,
  	("Unexpected tree code"));
  scope_cleanup_stack[idx].vla.alloca_sts_vector->push_back (st);
}

// This function is intended to be a general function to handle different
// pragmas (other than openmp pragmas). Currently it handles
// #pragma options, mips_frequency_hint
//
void
WGEN_Expand_Pragma (gs_t exp)
{
  gs_code_t code = gs_tree_code(exp);

  switch(code) {

    case GS_FREQ_HINT_STMT:
    { 
      // pragma mips_frequency_hint
      MIPS_FREQUENCY_HINT freq_hint;
      Is_True (gs_tree_code (gs_tree_operand(exp, 0)) == GS_STRING_CST,
               ("Expected string constant with mips_frequency_hint"));
      const char * hint = gs_tree_string_pointer (gs_tree_operand(exp, 0));                                                                                
      if (!strcmp (hint, "never")) 
        freq_hint = FREQUENCY_HINT_NEVER;
      else if (!strcmp (hint, "init")) 
        freq_hint = FREQUENCY_HINT_INIT;
      else if (!strcmp (hint, "frequent")) 
        freq_hint = FREQUENCY_HINT_FREQUENT;
      else // Invalid mips_frequency_hint
        break;
                                                                                
      WN * wn = WN_CreatePragma (WN_PRAGMA_MIPS_FREQUENCY_HINT, (ST*)NULL, freq_hint, 0);
      WGEN_Stmt_Append (wn, Get_Srcpos());
      break;
    }

    case GS_ZDL_STMT:
    {
      Is_True (gs_tree_code (gs_tree_operand(exp, 0)) == GS_STRING_CST,
               ("Expected string constant with zdl"));
      const char * hint = gs_tree_string_pointer (gs_tree_operand(exp, 0));
      if (!strcmp (hint, "off")) {
        WN * wn = WN_CreatePragma (WN_PRAGMA_NO_ZDL, (ST*)NULL, 0, 0);
        WGEN_Stmt_Append (wn, Get_Srcpos());
      }
      break;
    }
    
    default:
    {
       FmtAssert(FALSE, ("WGEN_Expand_Pragma: no yet implemented pragma %s", gs_code_name(code)));
    }
  }
}
