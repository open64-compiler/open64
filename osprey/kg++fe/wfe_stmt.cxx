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
 *
 * File modified October 9, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.3.1 release.
 */

#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"

extern "C" {
#include "gnu_config.h"
}
#include "gnu/system.h"

#include "srcpos.h"
#include "gnu/machmode.h"
extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
#include "cp-tree.h"
#include "gnu/output.h"         // For decode_reg_name
}
#include "gnu/flags.h"
#undef TARGET_PENTIUM  // hack around macro definition in gnu
#include "insn-config.h"	// MAX_RECOG_OPERANDS
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "ir_reader.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "wfe_pragma.h"
#include "wfe_decl.h"
#include "tree_symtab.h"
#include "targ_sim.h"
#include <ctype.h>
#include "tree_cmp.h"

#ifdef KEY
#include "wn.h"		// New_Region_Id()
#include "const.h"
int make_symbols_weak = FALSE;	// if TRUE, emit all new symbols as weak
bool in_cleanup = FALSE;	// TRUE if we are expanding code to be executed during stack unwinding
#endif // KEY

extern "C" void error (const char *, ...);

// #define WFE_DEBUG 

char *WFE_Tree_Node_Name (tree op);

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
  int       tree_code;
  LABEL_IDX break_label_idx;
  LABEL_IDX continue_label_idx;
  tree	    scope;
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

typedef struct scope_cleanup_info_t {
  tree		    stmt;
  LABEL_IDX	    label_idx;
#ifdef KEY
  LABEL_IDX	    cmp_idx;
  bool		    cleanup_eh_only;
  struct vla_ {
    bool		    has_alloca;
    ST * 		    alloca_st;
    vector<ST*>	*	    alloca_sts_vector;
  } vla;
#endif // KEY
} SCOPE_CLEANUP_INFO;

static SCOPE_CLEANUP_INFO *scope_cleanup_stack;
static INT32	    	   scope_cleanup_i;
static INT32	    	   scope_cleanup_max;

static tree	   *scope_stack;
static INT32	    scope_i;
static INT32	    scope_max;

typedef struct temp_cleanup_info_t {
  tree		    expr;
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
  tree		    handler;
  vector<tree>	    *cleanups;
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
  LABEL_IDX 	    goto_idx; // label where the current handler should jmp to
  LABEL_IDX 	    cleanups_idx;
  bool		    outermost; // handler for outermost try block in PU?
} HANDLER_INFO;

std::stack<HANDLER_INFO> handler_stack; // formed from handler_info_stack in Do_Handlers
#else
typedef struct handler_info_t {
  tree		    handler;
  LABEL_IDX	    label_idx;
} HANDLER_INFO;
#endif // KEY

static HANDLER_INFO *handler_info_stack;
static INT32	     handler_info_i;
static INT32	     handler_info_max;

typedef struct eh_cleanup_info {
  tree		     cleanup;
  LABEL_IDX	     label_idx;
  LABEL_IDX	     goto_idx;
} EH_CLEANUP_INFO;

static EH_CLEANUP_INFO *eh_cleanup_stack;
static INT32		eh_cleanup_i;
static INT32		eh_cleanup_max;

#ifdef KEY

bool processing_handler = false;
bool try_block_seen;
typedef struct eh_cleanup_entry {
  tree		     tryhandler;	// just for comparison, at present
  vector<tree>	     *cleanups;	// emit
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
static void Do_Cleanups_For_EH (void);
static INITV_IDX lookup_handlers (vector<tree> * =0);
static void Generate_unwind_resume (void);

static void WFE_fixup_target_expr (tree retval);

// If non-zero, don't use label indexes less than or equal to this.
LABEL_IDX WFE_unusable_label_idx;

// The last label index allocated.
LABEL_IDX WFE_last_label_idx;
#endif // KEY

static INT32	    scope_number;

static TY_IDX
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

#ifdef KEY
static void
Emit_Cleanup(tree cleanup)
{
  int saved_make_symbols_weak;

  saved_make_symbols_weak = make_symbols_weak;
  make_symbols_weak = TRUE;
  if (TREE_CODE(cleanup) == IF_STMT) {
    // Mimick WFE_Expand_If but don't call it, because WFE_Expand_If calls
    // WFE_Expand_Stmt which creates temp cleanups.  This leads to infinite
    // loop.
    FmtAssert(THEN_CLAUSE(cleanup) != NULL_TREE,
	      ("Do_Temp_Cleanups: then clause should be non-null"));
    FmtAssert(ELSE_CLAUSE(cleanup) == NULL_TREE,
	      ("Do_Temp_Cleanups: else clause should be null"));
    WN *test = WFE_Expand_Expr_With_Sequence_Point (IF_COND(cleanup),
						    Boolean_type);
    WN *then_block = WN_CreateBlock();
    WN *else_block = WN_CreateBlock();
    WN *if_stmt = WN_CreateIf (test, then_block, else_block);
    WFE_Stmt_Append (if_stmt, Get_Srcpos());
    WFE_Stmt_Push (then_block, wfe_stmk_if_then, Get_Srcpos());
    tree then_clause = THEN_CLAUSE(cleanup);
    if (TREE_CODE(then_clause) == EXPR_WITH_FILE_LOCATION)
      then_clause = EXPR_WFL_NODE(then_clause);
    else if (TREE_CODE(then_clause) == CLEANUP_STMT)
      then_clause = CLEANUP_EXPR(then_clause);
    WFE_One_Stmt_Cleanup(then_clause);
    WFE_Stmt_Pop(wfe_stmk_if_then);
  } else {
    if (TREE_CODE(cleanup) == EXPR_WITH_FILE_LOCATION)
      cleanup = EXPR_WFL_NODE(cleanup);
    else if (TREE_CODE(cleanup) == CLEANUP_STMT)
      cleanup = CLEANUP_EXPR(cleanup);
    WFE_One_Stmt_Cleanup (cleanup);
  }
  make_symbols_weak = saved_make_symbols_weak;
}
#endif
    
static void
#ifdef KEY
Push_Scope_Cleanup (tree t, bool eh_only=false)
#else
Push_Scope_Cleanup (tree t)
#endif
{
  // Don't push a cleanup without a scope
  if (scope_cleanup_i == -1 && TREE_CODE(t) == CLEANUP_STMT)
    return;

  if (++scope_cleanup_i == scope_cleanup_max) {
    scope_cleanup_max = ENLARGE (scope_cleanup_max);
    scope_cleanup_stack =
      (SCOPE_CLEANUP_INFO *) realloc (scope_cleanup_stack,
	 	        scope_cleanup_max * sizeof (SCOPE_CLEANUP_INFO));
  }

  scope_cleanup_stack [scope_cleanup_i].stmt = t;
  if (TREE_CODE(t) == CLEANUP_STMT)
    New_LABEL (CURRENT_SYMTAB, 
	       scope_cleanup_stack [scope_cleanup_i].label_idx);
  else
    scope_cleanup_stack [scope_cleanup_i].label_idx = 0;
#ifdef KEY
  if (TREE_CODE(t) == TRY_BLOCK)
    New_LABEL (CURRENT_SYMTAB, 
	       scope_cleanup_stack [scope_cleanup_i].cmp_idx);
  else
    scope_cleanup_stack [scope_cleanup_i].cmp_idx = 0;
  scope_cleanup_stack [scope_cleanup_i].cleanup_eh_only = eh_only;
  scope_cleanup_stack [scope_cleanup_i].vla.has_alloca = FALSE;
  scope_cleanup_stack [scope_cleanup_i].vla.alloca_st = NULL;
  scope_cleanup_stack [scope_cleanup_i].vla.alloca_sts_vector = 
  						new vector<ST*>();
#endif // KEY
}

#ifdef KEY
static void
Push_Handler_Info (tree handler, vector<tree> *v, 
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
#else
static void
Push_Handler_Info (tree handler, LABEL_IDX label_idx)
{
   if (++handler_info_i == handler_info_max) {
    handler_info_max = ENLARGE (handler_info_max);
    handler_info_stack =
      (HANDLER_INFO *) realloc (handler_info_stack,
                        handler_info_max * sizeof (HANDLER_INFO));
  }

  handler_info_stack [handler_info_i].handler   = handler;
  handler_info_stack [handler_info_i].label_idx = label_idx;
}
#endif // KEY

static void
Push_EH_Cleanup (tree cleanup, LABEL_IDX label_idx, LABEL_IDX goto_idx)
{
  if (++eh_cleanup_i == eh_cleanup_max) {
    eh_cleanup_max = ENLARGE (eh_cleanup_max);
    eh_cleanup_stack =
      (EH_CLEANUP_INFO *) realloc (eh_cleanup_stack,
			    eh_cleanup_max * sizeof (EH_CLEANUP_INFO));
  }

  eh_cleanup_stack[eh_cleanup_i].cleanup   = cleanup;
  eh_cleanup_stack[eh_cleanup_i].label_idx = label_idx;
  eh_cleanup_stack[eh_cleanup_i].goto_idx  = goto_idx;
}

static void WFE_Expand_Handlers_Or_Cleanup (const HANDLER_INFO&);

// Called from WFE_Finish_Function () and Do_EH_Cleanups ().
void
Do_Handlers (void)
{
#ifdef KEY
  int saved_make_symbols_weak = make_symbols_weak;
  make_symbols_weak = TRUE;

  if (key_exceptions) processing_handler = true;
#endif
  while (handler_info_i != -1) {
#ifndef KEY
    LABEL_IDX start_handlers;
    New_LABEL (CURRENT_SYMTAB, start_handlers);
    Set_LABEL_addr_saved (start_handlers);
    WFE_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, start_handlers, 0, NULL),
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
	WFE_Stmt_Append (cmp_wn, Get_Srcpos());
    }
    WN * actual_cmp = WN_CreateLabel ((ST_IDX) 0, 
	       handler_info_stack[handler_info_i].cmp_idx[0], 0, NULL);
    WFE_Stmt_Append (actual_cmp, Get_Srcpos());
#endif // !KEY

#ifdef KEY
    handler_stack.push (handler_info_stack[handler_info_i]);
#endif
    --handler_info_i;
    WFE_Expand_Handlers_Or_Cleanup (handler_info_stack[handler_info_i+1]);
#ifdef KEY
    handler_stack.pop();
#endif
  }
#ifdef KEY
  processing_handler = false;
  Do_Cleanups_For_EH();
  if (key_exceptions) 
    FmtAssert (cleanup_list_for_eh.empty(), ("EH Cleanup list not completely processed"));

  make_symbols_weak = saved_make_symbols_weak;
#endif
}

static void Call_Rethrow (void);
#ifndef KEY
static void Call_Terminate (void);
#endif // !KEY

void
Do_EH_Cleanups (void)
{
#ifndef KEY
  for (int i = 0; i <= eh_cleanup_i; ++i) {
    WFE_Stmt_Append (
     WN_CreateLabel ((ST_IDX) 0, eh_cleanup_stack [i].label_idx,
		     0, NULL),
     Get_Srcpos());
    tree cleanup = eh_cleanup_stack [i].cleanup;
    WFE_One_Stmt (CLEANUP_EXPR(eh_cleanup_stack [i].cleanup));
    LABEL_IDX goto_idx = eh_cleanup_stack [i].goto_idx;
    if (goto_idx)
      WFE_Stmt_Append (
        WN_CreateGoto ((ST_IDX) 0, goto_idx), Get_Srcpos());
    else
      Call_Rethrow();
  }
  	Call_Terminate();
#endif
  eh_cleanup_i = -1;
}

#ifdef KEY
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
			INITO_val (PU_misc_info (Get_Current_PU())))));
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

// This should ultimately replace Do_EH_Cleanups(), at present the latter
// seems redundant.
//
// Emit all cleanups, and emit a goto after each set of cleanups to the handler.
static void
Do_Cleanups_For_EH (void)
{
  for (std::list<EH_CLEANUP_ENTRY>::iterator i = cleanup_list_for_eh.begin();
		i != cleanup_list_for_eh.end(); ++i) {
    EH_CLEANUP_ENTRY e = *i;

    WN *pad_wn = WN_CreateLabel ((ST_IDX) 0, e.pad, 0, NULL);
    WN_Set_Label_Is_Handler_Begin (pad_wn);
    WFE_Stmt_Append (pad_wn, Get_Srcpos());

    WFE_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, e.start, 0, NULL), 
    		     Get_Srcpos());

    for (vector<tree>::iterator j=e.cleanups->begin();
		j!=e.cleanups->end();++j)
    {
    	tree cleanup = *j;
	Emit_Cleanup(cleanup);
    }
    if (e.goto_idx)
	WFE_Stmt_Append (WN_CreateGoto ((ST_IDX) 0, e.goto_idx), Get_Srcpos());
    else
	Generate_unwind_resume();
  }
  cleanup_list_for_eh.clear();
}
#endif // KEY

static void
Pop_Scope_And_Do_Cleanups (void)
{
  Is_True(scope_cleanup_i != -1,
	  ("Pop_Scope_And_Do_Cleanups: scope_cleanup-stack is empty"));

  while (true) {
    tree t = scope_cleanup_stack [scope_cleanup_i].stmt;
    if (TREE_CODE(t) != CLEANUP_STMT) {
      if (TREE_CODE(t) == SCOPE_STMT)
      {
#ifdef KEY
	// Leaving scope, so use dealloca for any alloca within the scope
	if (scope_cleanup_stack[scope_cleanup_i].vla.has_alloca)
	  WFE_Dealloca (scope_cleanup_stack[scope_cleanup_i].vla.alloca_st,
	       scope_cleanup_stack[scope_cleanup_i].vla.alloca_sts_vector);
#endif
 	--scope_cleanup_i;
      }
      break;
    }
    Is_True(scope_cleanup_i != -1,
	    ("Pop_Scope_And_Do_Cleanups: no scope_stmt on stack"));
#ifdef KEY
    if (scope_cleanup_stack[scope_cleanup_i].cleanup_eh_only)
    {
    	--scope_cleanup_i;
	continue;
    }
#endif
    INT j = scope_cleanup_i - 1;
    LABEL_IDX goto_idx = 0;
#ifdef KEY
    while (j != -1 && (TREE_CODE(scope_cleanup_stack [j].stmt) != CLEANUP_STMT
    		|| scope_cleanup_stack[j].cleanup_eh_only)) {
#else
    while (j != -1 && TREE_CODE(scope_cleanup_stack [j].stmt) != CLEANUP_STMT) {
#endif
      if (TREE_CODE(scope_cleanup_stack [j].stmt) == TRY_BLOCK)
	break;
      --j;
    }
    if (j != -1 && TREE_CODE(scope_cleanup_stack [j].stmt) == CLEANUP_STMT)
      goto_idx = scope_cleanup_stack [j].label_idx;
    Push_EH_Cleanup (t,
		     scope_cleanup_stack [scope_cleanup_i]  .label_idx,
		     goto_idx);
    --scope_cleanup_i;
    WFE_One_Stmt_Cleanup (CLEANUP_EXPR(scope_cleanup_stack [scope_cleanup_i+1].stmt));
  }
}       

static void
Push_Scope (tree t)
{
  if (++scope_i == scope_max) {
    scope_max = ENLARGE (scope_max);
    scope_stack =
      (tree *) realloc (scope_stack,
	 	        scope_max * sizeof (tree));
  }
  scope_stack[scope_i] = t;
}

#ifdef KEY
// Called only from wfe_decl.cxx for the top level decl.
void
Push_Top_Level_Scope (tree t)
{
  PARENT_SCOPE (t) = 0;
  Push_Scope (t);
}

// Called only from wfe_decl.cxx for the top level decl.
void Pop_Top_Level_Scope (void)
{
  --scope_i;
}
#endif

void
Push_Temp_Cleanup (tree t, bool is_cleanup
#ifdef KEY
, bool cleanup_eh_only
#endif
)
{
#ifdef KEY
  // If a guard var is required, conditionalize the cleanup.
  tree guard_var = WFE_Get_Guard_Var();
  if (guard_var != NULL_TREE) {
    t = build_stmt((tree_code) IF_STMT,
		   c_common_truthvalue_conversion(guard_var),
		   t, NULL_TREE);
  }
#endif

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
cleanup_matches (tree candidate, tree target)
{
  if (candidate == target)
    return TRUE;

  // The node could be hidden behind a guard variable.
  if (TREE_CODE(candidate) == IF_STMT &&
      THEN_CLAUSE(candidate) == target)
    return TRUE;

  return FALSE;
}


bool
Is_Cleanup_Only (void)
{
  return temp_cleanup_stack[temp_cleanup_i].cleanup_eh_only;
}

void
Do_Temp_Cleanups (tree t)
{
  Is_True(temp_cleanup_i != -1, ("Do_Temp_Cleanups: stack empty"));
#ifdef KEY
  while (!cleanup_matches(temp_cleanup_stack[temp_cleanup_i].expr, t))
#else
  while (temp_cleanup_stack[temp_cleanup_i].expr != t)
#endif
    {
#ifdef KEY
    if (temp_cleanup_stack[temp_cleanup_i].cleanup_eh_only) {
// We don't want this cleanup to be emitted here -- it is to be executed only
// if an exception is thrown.
    	--temp_cleanup_i;
	continue;
    }
#endif
    LABEL_IDX goto_idx = 0;
    INT j = temp_cleanup_i - 1;
    tree cleanup = temp_cleanup_stack [temp_cleanup_i].expr;
#ifdef KEY
    while (j != -1 && (temp_cleanup_stack [j].label_idx == 0 ||
    			temp_cleanup_stack[j].cleanup_eh_only))
#else
    while (j != -1 && temp_cleanup_stack [j].label_idx == 0)
#endif
      --j;
    if (j != -1)
      goto_idx = temp_cleanup_stack [j].label_idx;
    Push_EH_Cleanup (cleanup,
		     temp_cleanup_stack [temp_cleanup_i].label_idx,
		     goto_idx);
#ifdef KEY
    Emit_Cleanup(cleanup);
#else
    WFE_One_Stmt (cleanup);
#endif
    --temp_cleanup_i;
  }
  --temp_cleanup_i;

#ifdef KEY
  if (key_exceptions && processing_handler && 
	!cleanup_matches(temp_cleanup_stack[temp_cleanup_i+1].expr, t))
  {
    HANDLER_INFO hi = handler_stack.top();
    if (hi.temp_cleanup)
    {
      int n = hi.temp_cleanup->size()-1;
      while (!cleanup_matches((*hi.temp_cleanup)[n].expr, t)) {
	LABEL_IDX goto_idx = 0;
	INT j = n - 1;
	tree cleanup = (*hi.temp_cleanup) [n].expr;
	while (j != -1 && (*hi.temp_cleanup) [j].label_idx == 0) --j;
	if (j != -1)
      	    goto_idx = (*hi.temp_cleanup) [j].label_idx;
    	Push_EH_Cleanup (cleanup, (*hi.temp_cleanup) [n].label_idx, goto_idx);
	Emit_Cleanup(cleanup);
	--n;
      }
    }
  }
#endif // KEY
}

static void
WFE_Record_Loop_Switch (int tree_code)
{
  INT32 i;
  Is_True(tree_code == DO_STMT    ||
	  tree_code == FOR_STMT   ||
 	  tree_code == WHILE_STMT ||
          tree_code == SWITCH_STMT,
	  ("WFE_Record_Loop_Switch: unexpected tree_code"));

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
      [break_continue_info_i].scope = NULL_TREE;
  else {
    for (i = scope_cleanup_i;
	 TREE_CODE(scope_cleanup_stack[i].stmt) == CLEANUP_STMT;
	 --i);
    Is_True (i != -1 && 
	     (TREE_CODE(scope_cleanup_stack[i].stmt) == SCOPE_STMT ||
	      TREE_CODE(scope_cleanup_stack[i].stmt) == TRY_BLOCK),
	    ("WFE_Record_Loop_Switch: no scope_stmt on stack"));
    break_continue_info_stack
      [break_continue_info_i].scope = scope_cleanup_stack[i].stmt;
  }
} /* WFE_Record_Loop_Switch */

static void
WFE_Expand_Case (tree low, tree high)
{
  WN        *wn;
  WN        *lower_bound;
  WN        *upper_bound;
  LABEL_IDX  case_label_idx;

  if (high != NULL_TREE)
    DevWarn("encountered case range");

  if (low == NULL_TREE) {
    if (switch_info_stack [switch_info_i].default_label_idx != 0)
      error ("duplicate default label");
    else {
      New_LABEL (CURRENT_SYMTAB, case_label_idx);
      switch_info_stack [switch_info_i].default_label_idx = case_label_idx;
    }
  }

  else {
    if (TREE_CODE(low) == VAR_DECL)
      low = DECL_INITIAL(low);
    if (high != NULL_TREE && TREE_CODE(high) == VAR_DECL)
      high = DECL_INITIAL(high);
    lower_bound = WFE_Expand_Expr (low);
    upper_bound = (high == NULL_TREE) ? lower_bound
				      : WFE_Expand_Expr(high);
    if (++case_info_i == case_info_max) {
      case_info_max   = ENLARGE(case_info_max);
      case_info_stack = (CASE_INFO *) realloc (case_info_stack,
                                               case_info_max * sizeof (CASE_INFO));
    }

    case_info_stack 
      [case_info_i].case_lower_bound_value = 
        (low  == NULL_TREE) ? 0 : WN_const_val (lower_bound);
    case_info_stack 
      [case_info_i].case_upper_bound_value = 
        (high == NULL_TREE) ? WN_const_val (lower_bound) 
			    : WN_const_val (upper_bound);
    for (int i = switch_info_stack [switch_info_i].start_case_index;
         i < case_info_i; ++i) 
      if (WN_const_val(lower_bound) == 
          case_info_stack [i].case_lower_bound_value)
  	error ("duplicate case");
    New_LABEL (CURRENT_SYMTAB, case_label_idx);
    case_info_stack [case_info_i].case_label_idx = case_label_idx;
  }

  wn = WN_CreateLabel ((ST_IDX) 0, case_label_idx, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos ());
} /* WFE_Expand_Case */

static void
WFE_Declare_Nonlocal_Label (tree label)
{
  WFE_Get_LABEL (label, FALSE);
} /* WFE_Expand_Label */


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

static char *operand_constraint_array[MAX_RECOG_OPERANDS];

static BOOL
constraint_by_address (const char *s)
{
#if !defined(TARG_X8664) && !defined(TARG_LOONGSON)
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

void
Wfe_Expand_Asm_Operands (tree  string,
			 tree  outputs,
			 tree  inputs,
			 tree  clobbers,
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

  int ninputs = list_length (inputs);

  tree tail;
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
  for (tail = outputs; tail; tail = TREE_CHAIN (tail)) {
#ifdef KEY
    // Initialize operand numbers map.
    opnd_num_map[i] = i;

    // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
    // first operand gives the string constant.
    constraint_string = 
      const_cast<char*>TREE_STRING_POINTER (TREE_OPERAND (TREE_PURPOSE (tail), 0));
#else
    constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));
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
       tail = TREE_CHAIN (tail))
    {
#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
	const_cast<char*>TREE_STRING_POINTER (TREE_OPERAND (TREE_PURPOSE (tail), 0));
#else
      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));
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
				  const_cast<char*>TREE_STRING_POINTER (string)); // KEY

  WN *clobber_block = WN_CreateBlock ();

  WN_kid0(asm_wn) = clobber_block;

  for (tail = clobbers; tail; tail = TREE_CHAIN (tail))
    {
      char *clobber_string =
	const_cast<char *>TREE_STRING_POINTER (TREE_VALUE (tail));  // KEY

      WN *clobber_pragma = NULL;

      int gcc_reg = decode_reg_name(clobber_string);
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
       tail = TREE_CHAIN (tail))
    {
#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
	const_cast<char*>TREE_STRING_POINTER (TREE_OPERAND (TREE_PURPOSE (tail), 0));
#else
      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));
#endif /* KEY */

      if (constraint_by_address(constraint_string)) {
	// This operand is by address, and gets represented as an
	// ASM_INPUT even though the user told us it's an output.
	WN *lhs_rvalue = WFE_Expand_Expr(TREE_OPERAND(tail, 0));
	WN *addr_of_lvalue = address_of(lhs_rvalue);
	FmtAssert(addr_of_lvalue != NULL,
		  ("WFE_Expand_Asm_Operands: output operand must be lvalue"));
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
       tail = TREE_CHAIN (tail))
    {
      if (TREE_PURPOSE (tail) == NULL_TREE)
	{
	  Fail_FmtAssertion ("hard register `%s' listed as "
			     "input operand to `asm'",
			     TREE_STRING_POINTER (TREE_VALUE (tail)) );
	  return;
	}

#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
	const_cast<char*>TREE_STRING_POINTER (TREE_OPERAND (TREE_PURPOSE (tail), 0));
#else
      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));
#endif /* KEY */

      if (flag_bad_asm_constraint_kills_stmt &&
	  !constraint_supported (constraint_string)) {
	DevWarn ("Unrecognized constraint %s; "
		 "asm statement at line %d discarded",
		 constraint_string, lineno);
	return;
      }

      WN *input_rvalue = WFE_Expand_Expr (TREE_VALUE (tail));

      if (constraint_by_address(constraint_string)) {
	WN *addr_of_rvalue = address_of(input_rvalue);
	if (addr_of_rvalue != NULL) {
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
	  WFE_Stmt_Append (store_wn, Get_Srcpos ());
	  input_rvalue = WN_Lda (Pointer_Mtype,
				 (WN_OFFSET) 0,
				 temp_st,
				 (UINT) 0);
	}
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
  WFE_Stmt_Append (asm_wn, Get_Srcpos ());

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
       tail = TREE_CHAIN (tail), ++opnd_num)
    {
#ifdef KEY
      // In gcc-3.2, TREE_PURPOSE of tail represents a TREE_LIST node whose
      // first operand gives the string constant.
      constraint_string = 
	const_cast<char*>TREE_STRING_POINTER (TREE_OPERAND (TREE_PURPOSE (tail), 0));
#else
      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));
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

	WN *output_rvalue_wn = WFE_Lhs_Of_Modify_Expr (MODIFY_EXPR,
						       TREE_VALUE (tail),
#ifdef TARG_SL
                                                       NULL,
#endif
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
	// WFE_Lhs_Of_Modify_Expr.
	TYPE_ID desc = TY_mtype (Get_TY (TREE_TYPE (TREE_VALUE (tail))));
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
WFE_Get_LABEL (tree label, int def)
{
  LABEL_IDX label_idx =  DECL_LABEL_IDX(label);
  SYMTAB_IDX symtab_idx = DECL_SYMTAB_IDX(label);

  if (label_idx == 0
#ifdef KEY
      // Don't use old indexes that we are not supposed to use.
      || label_idx <= WFE_unusable_label_idx
#endif
     ) {
    New_LABEL (CURRENT_SYMTAB, label_idx);
    DECL_LABEL_IDX(label) = label_idx;
    DECL_SYMTAB_IDX(label) = CURRENT_SYMTAB;
#ifdef KEY
    WFE_last_label_idx = label_idx;
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
/*
    else {
      if (label->decl.label_defined)
        FmtAssert (label->decl.symtab_idx == CURRENT_SYMTAB,
                   ("jumping to a label not defined in current function"));
    }
*/
  }

  return label_idx;
} /* WFE_Get_LABEL */

void
WFE_Check_Undefined_Labels (void)
{
  INT32 i;
  for (i = undefined_labels_i; i >= 0; --i) {
    LABEL_IDX  label_idx  = undefined_labels_stack [undefined_labels_i].label_idx;
    SYMTAB_IDX symtab_idx = undefined_labels_stack [undefined_labels_i].symtab_idx;
//  fprintf (stderr, "WFE_Check_Undefined_Labels: %d idx = %8x [%d]\n", i, label_idx, symtab_idx);
    if (symtab_idx < CURRENT_SYMTAB)
      break;
    FmtAssert (undefined_labels_stack [undefined_labels_i].defined,
               ("label not defined within current function scope"));
  }
  undefined_labels_i = i;
} /* WFE_Check_Undefined_Labels */


void
WFE_Stmt_Init (void)
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
    (tree *) malloc (sizeof (tree) * scope_max);

  temp_cleanup_max       = 32;
  temp_cleanup_i	 = -1;
  temp_cleanup_stack	 =
    (TEMP_CLEANUP_INFO *) malloc (sizeof (TEMP_CLEANUP_INFO) * 
				  temp_cleanup_max);

  handler_info_max	 = 32;
  handler_info_i	 = -1;
  handler_info_stack     =
    (HANDLER_INFO *) malloc (sizeof(HANDLER_INFO) * handler_info_max);

  eh_cleanup_max	 = 32;
  eh_cleanup_i		 = -1;
  eh_cleanup_stack	 =
    (EH_CLEANUP_INFO *) malloc (sizeof (EH_CLEANUP_INFO) * eh_cleanup_max);

  scope_number           = 0;
} /* WFE_Stmt_Init */

#ifdef KEY
// Special case to also handle while we are inside a handler
static void
Cleanup_To_Scope_From_Handler(tree scope)
{
  INT32 i = scope_cleanup_i;
  INT32 j = -1;
  Is_True(i != -1, ("Cleanup_To_Scope_From_Handler: scope_cleanup_stack empty"));
  while ((i != -1) && (scope_cleanup_stack [i].stmt != scope)) {
    if (TREE_CODE(scope_cleanup_stack [i].stmt) == SCOPE_STMT)
      j = i;
    --i;
  }

  bool found_target_scope = false;
  if (i != -1)	found_target_scope = true;

  if (j != -1) {
    i = scope_cleanup_i;
    while (i != j) {
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT &&
		!scope_cleanup_stack[i].cleanup_eh_only)
        WFE_One_Stmt_Cleanup (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
    --i;
    }
  }
  if (found_target_scope) return;

  FmtAssert (processing_handler, ("Invalid scope"));

  HANDLER_INFO hi = handler_stack.top();
  FmtAssert (hi.scope, ("No scope information available"));
  i = hi.scope->size()-1;
  j = -1;
  Is_True(i != 0, ("Cleanup_To_Scope_From_Handler: scope_cleanup_stack empty"));

  while ((*hi.scope)[i].stmt != scope) {
    if (TREE_CODE((*hi.scope)[i].stmt) == SCOPE_STMT)
	j = i;
    --i;
  }
  if (j != -1) {
    i = hi.scope->size()-1;
    while (i != j) {
      if (TREE_CODE((*hi.scope)[i].stmt) == CLEANUP_STMT &&
		!(*hi.scope)[i].cleanup_eh_only)
	WFE_One_Stmt_Cleanup (CLEANUP_EXPR((*hi.scope)[i].stmt));
      --i;
    }
  }
}
#endif // KEY

static void
Cleanup_To_Scope(tree scope)
{
  INT32 i = scope_cleanup_i;
  INT32 j = -1;
  Is_True(i != -1, ("Cleanup_To_Scope: scope_cleanup_stack empty"));
  while (scope_cleanup_stack [i].stmt != scope) {
    if (TREE_CODE(scope_cleanup_stack [i].stmt) == SCOPE_STMT)
      j = i;
    --i;
  }

  if (j != -1) {
    i = scope_cleanup_i;
    while (i != j) {
#ifdef KEY
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT &&
		!scope_cleanup_stack[i].cleanup_eh_only)
#else
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT)
#endif
        WFE_One_Stmt_Cleanup (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
    --i;
    }
  }
}
 
#ifdef KEY
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
// WFE_Expand_Break/ WFE_Expand_Continue, even though we are inside a handler
//
static void
WFE_Expand_Break (void)
{
  INT32     i  	      = break_continue_info_i;
  LABEL_IDX label_idx;
  tree      scope;
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
    if (key_exceptions && processing_handler)
	Cleanup_To_Scope_From_Handler (scope);
    else
    	Cleanup_To_Scope (scope);
  }
   
  WFE_Stmt_Append (wn, Get_Srcpos());
}
#else
static void
WFE_Expand_Break (void)
{
  INT32     i  	      = break_continue_info_i;
  LABEL_IDX label_idx = break_continue_info_stack[i].break_label_idx;
  tree      scope     = break_continue_info_stack[i].scope;
  WN *      wn;

  if (label_idx == 0) {
    New_LABEL (CURRENT_SYMTAB, label_idx);
    break_continue_info_stack [i].break_label_idx = label_idx;
  }

  wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);

  if (scope)
    Cleanup_To_Scope (scope);
   
  WFE_Stmt_Append (wn, Get_Srcpos());
}
/* WFE_Expand_Break */
#endif // KEY

#ifdef KEY
static void
WFE_Expand_Continue (void)
{
  INT32     i = break_continue_info_i;
  LABEL_IDX label_idx=0;
  tree      scope;

  HANDLER_INFO hi;
  if (processing_handler)
    hi = handler_stack.top();
  if (i == -1) 
  {
    FmtAssert (processing_handler, ("WFE_Expand_Continue: No break/continue info"));
    scope = (*hi.break_continue)[hi.break_continue->size()-1].scope;
  }
  else scope = break_continue_info_stack [i].scope;
  WN *      wn;
  
  /* find the enclosing loop */
  if (i != -1) {
   while (break_continue_info_stack [i].tree_code == SWITCH_STMT) --i;
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

  if (key_exceptions && processing_handler && !label_idx)
  { // have not yet found the enclosing loop
	INT32 j = hi.break_continue->size()-1;
	while ((*hi.break_continue)[j].tree_code == SWITCH_STMT) --j;
	FmtAssert (j != -1, ("Error with 'continue' in handler"));
	label_idx = (*hi.break_continue)[j].continue_label_idx;
	FmtAssert (label_idx,("WFE_Expand_Goto: No label to goto"));
  }

  if (scope)
  {
    if (key_exceptions && processing_handler)
	Cleanup_To_Scope_From_Handler (scope);
    else
    	Cleanup_To_Scope (scope);
  }

  wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Continue */
#else
static void
WFE_Expand_Continue (void)
{
  INT32     i = break_continue_info_i;
  LABEL_IDX label_idx;
  tree      scope = break_continue_info_stack [i].scope;
  WN *      wn;

  /* find the enclosing loop */
  while (break_continue_info_stack [i].tree_code == SWITCH_STMT) --i;
  label_idx = break_continue_info_stack [i].continue_label_idx;
  if (label_idx == 0) {
    New_LABEL (CURRENT_SYMTAB, label_idx);
    break_continue_info_stack [i].continue_label_idx = label_idx;
  }

  if (scope)
    Cleanup_To_Scope (scope);

  wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Continue */
#endif // KEY

static void
WFE_Expand_Loop (tree stmt)
{
  tree cond, body, incr = NULL_TREE, init = NULL_TREE;

  WN * loop_stmt;
  WN * loop_test;
  WN * loop_block;
  WN * loop_body;

  WFE_Record_Loop_Switch (TREE_CODE(stmt));

  switch (TREE_CODE(stmt)) {
    case WHILE_STMT:
      cond = WHILE_COND(stmt);
      body = WHILE_BODY(stmt);
      break;

    case DO_STMT:
      cond = DO_COND(stmt);
      body = DO_BODY(stmt);
      break;

    case FOR_STMT:
      incr = FOR_EXPR(stmt);
      cond = FOR_COND(stmt);
      body = FOR_BODY(stmt);
      for (init = FOR_INIT_STMT(stmt); init; init = TREE_CHAIN(init))
	WFE_Expand_Stmt(init);
      break;

    default:
      Is_True(FALSE, ("WFE_Expand_Loop: unexpected TREE_CODE"));
      break;
  }

#ifdef KEY
// handle "for (;;) ;"
  if (!cond) {
    loop_test = WN_Intconst (Boolean_type, 1);
  }
  else
#endif // KEY
  if (TREE_CODE(cond) == TREE_LIST &&
      TREE_VALUE(cond) == NULL) {
    // handle non terminating loops
    tree stmt;
    WN   *cond_block;
    cond_block = WN_CreateBlock ();
    WFE_Stmt_Push (cond_block, wfe_stmk_while_cond, Get_Srcpos());
    for (stmt = TREE_PURPOSE(cond); stmt; stmt = TREE_CHAIN(stmt))
      WFE_Expand_Stmt (stmt);
    WFE_Stmt_Pop (wfe_stmk_while_cond);
    loop_test = WN_Intconst (Boolean_type, 1);
    if (WN_first (cond_block)) {
      loop_test = WN_CreateComma (OPR_COMMA, Boolean_type, MTYPE_V,
                                  cond_block, loop_test);
    }
    else
      WN_Delete (cond_block);
  }

  else
    loop_test = WFE_Expand_Expr_With_Sequence_Point (cond, Boolean_type);   

  loop_body = WN_CreateBlock ();

  if (TREE_CODE(stmt) == WHILE_STMT ||
      TREE_CODE(stmt) == FOR_STMT)
    loop_stmt = WN_CreateWhileDo (loop_test, loop_body);
  else
    loop_stmt = WN_CreateDoWhile (loop_test, loop_body);

  WFE_Stmt_Append (loop_stmt, Get_Srcpos());

  if (body) {
    WFE_Stmt_Push (loop_body, wfe_stmk_while_body, Get_Srcpos());
    while (body) {
      WFE_Expand_Stmt (body);
      body = TREE_CHAIN(body);
    }

    if (break_continue_info_stack
	  [break_continue_info_i].continue_label_idx) {
      WFE_Stmt_Append (
	WN_CreateLabel ((ST_IDX) 0,
			break_continue_info_stack
			  [break_continue_info_i].continue_label_idx,
			0, NULL),
	Get_Srcpos());
    }
#ifdef KEY	// bug 3265
    if (incr) {
      Push_Temp_Cleanup(incr, false);
      WFE_One_Stmt(incr);
      Do_Temp_Cleanups(incr);
    }
#else
    if (incr)
      WFE_One_Stmt(incr);
#endif

    WFE_Stmt_Pop (wfe_stmk_while_body);
  }

  if (break_continue_info_stack [break_continue_info_i].break_label_idx) {
    WFE_Stmt_Append (
      WN_CreateLabel ((ST_IDX) 0,
		      break_continue_info_stack
			[break_continue_info_i].break_label_idx,
		      0, NULL),
      Get_Srcpos());
  }

  --break_continue_info_i;
} /* WFE_Expand_Loop */
  
#ifndef KEY
void
WFE_Expand_Goto (tree label)
{
  WN *wn;
  LABEL_IDX label_idx = WFE_Get_LABEL (label, FALSE);
  if ((CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) &&
      (DECL_SYMTAB_IDX(label) < CURRENT_SYMTAB))
    wn = WN_CreateGotoOuterBlock (label_idx, DECL_SYMTAB_IDX(label));
  else {
    tree scope = LABEL_SCOPE(label);
    if (scope != NULL_TREE && scope_cleanup_i != -1) {
      INT32 scope_number = SCOPE_NUMBER(scope);
      INT32 i = scope_cleanup_i;
      INT32 j = -1;
      while (i != -1) {
	if (TREE_CODE(scope_cleanup_stack [i].stmt) == SCOPE_STMT)
	  if (SCOPE_NUMBER(scope_cleanup_stack [i].stmt) >= scope_number)
	    break;
	  j = i;
        --i;
      }
      if (j != -1) {
        i = scope_cleanup_i;
	while (i != j) {
	  if (TREE_CODE(scope_cleanup_stack[i].stmt) == CLEANUP_STMT)
	    WFE_One_Stmt_Cleanup (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
	--i;
        }
      }
    }

    wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Goto */
#else
void
WFE_Expand_Goto (tree label)	// KEY VERSION
{
  WN *wn;
  bool in_handler=false;
  vector<tree>::reverse_iterator ci, li;
  LABEL_IDX label_idx = WFE_Get_LABEL (label, FALSE);
  if ((CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) &&
      (DECL_SYMTAB_IDX(label) < CURRENT_SYMTAB))
    wn = WN_CreateGotoOuterBlock (label_idx, DECL_SYMTAB_IDX(label));
  else {
    tree scope = LABEL_SCOPE(label);
    if (scope != NULL_TREE && scope_cleanup_i != -1) {
      vector<tree> Label_scope_nest;
      while (scope) {
      	Label_scope_nest.push_back (scope);
	scope = PARENT_SCOPE (scope);
      }
      INT32 i = scope_cleanup_i;
      while (i != -1) {
	if (TREE_CODE(scope_cleanup_stack [i].stmt) == SCOPE_STMT)
	    break;
	--i;
      }
      vector<tree> Current_scope_nest;
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
      	i = scope_cleanup_i;
	Is_True(i != -1, ("WFE_Expand_Goto: scope_cleanup_stack empty"));
  	while ((i >= 0) && (scope_cleanup_stack [i].stmt != *ci))
	{
	      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT &&
			!scope_cleanup_stack[i].cleanup_eh_only)
        	WFE_One_Stmt_Cleanup (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
    	    --i;
  	}
	if (i == -1)
	      in_handler = true;
      }
    }

  if (in_handler && (!key_exceptions || !processing_handler))
  	DevWarn ("Goto in exception handler but exceptions not enabled?");
// If this is a handler, we have just emitted the cleanups within it. 
// Now find out what other cleanups need to be emitted for variables 
// outside the handler.
  if (in_handler && processing_handler && key_exceptions)
  {
    HANDLER_INFO hi = handler_stack.top();

    INT32 i = hi.scope->size()-1;
    Is_True(i != -1, ("WFE_Expand_Goto: scope_cleanup_stack empty inside handler"));
    while ((i >= 0) && ((*hi.scope) [i].stmt != *ci)) {
	if (TREE_CODE((*hi.scope) [i].stmt) == CLEANUP_STMT &&
		!(*hi.scope) [i].cleanup_eh_only)
	    WFE_One_Stmt_Cleanup (CLEANUP_EXPR((*hi.scope) [i].stmt));
        --i;
    }
  }

    wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Goto */
#endif

static void
WFE_Expand_Computed_Goto (tree exp)
{
  DevWarn ("encountered indirect jump");
  WN *addr = WFE_Expand_Expr (exp);
  WN *wn   = WN_CreateAgoto (addr);
  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Computed_Goto */

#ifndef KEY
static
#endif
void 
WFE_Expand_If (tree stmt)
{
  WN * if_stmt;
  WN * test;
  WN * then_block;
  WN * else_block;

  test = WFE_Expand_Expr_With_Sequence_Point (IF_COND(stmt),
					      Boolean_type);
  then_block = WN_CreateBlock ();
  else_block = WN_CreateBlock ();
  if_stmt    = WN_CreateIf (test, then_block, else_block);
  WFE_Stmt_Append (if_stmt, Get_Srcpos ());
  if (THEN_CLAUSE(stmt)) {
    WFE_Stmt_Push (then_block, wfe_stmk_if_then, Get_Srcpos ());
    for (tree t = THEN_CLAUSE(stmt); t; t = TREE_CHAIN(t))
      WFE_Expand_Stmt (t);
    WFE_Stmt_Pop (wfe_stmk_if_then);
  }
  if (ELSE_CLAUSE(stmt)) {
    WFE_Stmt_Push (else_block, wfe_stmk_if_else, Get_Srcpos());
    for (tree t = ELSE_CLAUSE(stmt); t; t = TREE_CHAIN(t))
      WFE_Expand_Stmt (t);
    WFE_Stmt_Pop (wfe_stmk_if_else);
  }
} /* WFE_Expand_If */

void
WFE_Expand_Label (tree label)
{
  LABEL_IDX label_idx = WFE_Get_LABEL (label, TRUE);
  DECL_SYMTAB_IDX(label) = CURRENT_SYMTAB;

  if (!DECL_LABEL_DEFINED(label)) {
    WN *wn;
    DECL_LABEL_DEFINED(label) = TRUE;
    wn = WN_CreateLabel ((ST_IDX) 0, label_idx, 0, NULL);
    WFE_Stmt_Append (wn, Get_Srcpos ());
  }
} /* WFE_Expand_Label */

#ifdef KEY
extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID, ST *);
#endif // KEY

void
WFE_Expand_Return (tree stmt, tree retval)
{
  WN *wn;

  if (retval == NULL_TREE) {
    Do_Temp_Cleanups (stmt);
    int i = scope_cleanup_i;
    while (i != -1) {
#ifdef KEY
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT &&
		!scope_cleanup_stack[i].cleanup_eh_only)
#else
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT)
#endif
        WFE_One_Stmt_Cleanup (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
      --i;
    }
#ifdef KEY
    if (key_exceptions && processing_handler) {
	HANDLER_INFO hi = handler_stack.top();
	FmtAssert (hi.scope, ("NULL scope"));
	int j = hi.scope->size()-1;
	while (j != -1) {
	    if (TREE_CODE((*hi.scope)[j].stmt) == CLEANUP_STMT &&
			!(*hi.scope)[j].cleanup_eh_only)
        	WFE_One_Stmt_Cleanup (CLEANUP_EXPR((*hi.scope) [j].stmt));
	    --j;
	}
    }
#endif
    wn = WN_CreateReturn ();
  }
  else {
    WN *rhs_wn;
    TY_IDX ret_ty_idx = Get_TY(TREE_TYPE(TREE_TYPE(Current_Function_Decl())));

#ifdef KEY
    bool copied_return_value = FALSE;
#ifdef PATHSCALE_MERGE
    bool need_iload_via_fake_parm = FALSE;
    WN *target_wn = NULL;
#endif

    // If the return object must be passed through memory and the return
    // object is created by a TARGET_EXPR, have the TARGET_EXPR write directly
    // to the memory return area.
    if (TY_return_in_mem(ret_ty_idx)) {
      FmtAssert (TY_mtype (ret_ty_idx) == MTYPE_M,
	         ("WFE_Expand_Return: return_in_mem type is not MTYPE_M"));
      // Skip the NOP_EXPRs, if any, before the TARGET_EXPR.  Bug 3448.
      tree t = retval;
      while (TREE_CODE(t) == NOP_EXPR) {
	t = TREE_OPERAND(t, 0);
      }
      if (TREE_CODE(t) == TARGET_EXPR) {
	WFE_fixup_target_expr(t);
	copied_return_value = TRUE;
#ifdef PATHSCALE_MERGE
      } else if (TREE_CODE(t) == CALL_EXPR) {
	// Pass the first fake parm to the called function, so that the called
	// function can write the result directly to the return area.
	// Bug 12837.
	WN *first_formal = WN_formal(Current_Entry_WN(), 0);
	ST *st = WN_st(first_formal);
	target_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
	copied_return_value = TRUE;
	need_iload_via_fake_parm = TRUE;
#endif
      }
    }
#endif

    rhs_wn = WFE_Expand_Expr_With_Sequence_Point (
		retval,
		TY_mtype (ret_ty_idx)
#ifdef PATHSCALE_MERGE
                , target_wn
#endif
                );
#ifdef PATHSCALE_MERGE
    // rhs_wn is NULL if retval is a call_expr which writes the result directly
    // into the return area.  Manufacture a rhs_wn which is an iload of the
    // fake first parm.  Bug 12837.
    if (rhs_wn == NULL) {
      Is_True(need_iload_via_fake_parm == TRUE,
	      ("WFE_Expand_Return: unexpected rhs_wn NULL"));
      WN *first_formal = WN_formal(Current_Entry_WN(), 0);
      ST *st = WN_st(first_formal);
      TY_IDX ty_idx = Get_TY(TREE_TYPE(retval));
      WN *ldid_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
      rhs_wn = WN_Iload(TY_mtype(ty_idx), 0, ty_idx, ldid_wn);
    }
#endif

    WN * cleanup_block = WN_CreateBlock ();
    WFE_Stmt_Push (cleanup_block, wfe_stmk_temp_cleanup, Get_Srcpos ());
    Do_Temp_Cleanups (stmt);
    int i = scope_cleanup_i;
    while (i != -1) {
#ifdef KEY
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT &&
		!scope_cleanup_stack[i].cleanup_eh_only)
#else
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT)
#endif
        WFE_One_Stmt_Cleanup (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
      --i;
    }
#ifdef KEY
    if (key_exceptions && processing_handler) {
	HANDLER_INFO hi = handler_stack.top();
	FmtAssert (hi.scope, ("NULL scope"));
	int j = hi.scope->size()-1;
	while (j != -1) {
	    if (TREE_CODE((*hi.scope)[j].stmt) == CLEANUP_STMT &&
			!(*hi.scope)[j].cleanup_eh_only)
        	WFE_One_Stmt_Cleanup (CLEANUP_EXPR((*hi.scope) [j].stmt));
	    --j;
	}
    }
#endif
    WFE_Stmt_Pop (wfe_stmk_temp_cleanup);

    if (WN_first (cleanup_block)) {

      if (TREE_CODE(retval) == TARGET_EXPR || 
	  TREE_CODE(retval) == COMPOUND_EXPR) {

	WN * insertee = WN_kid0 (rhs_wn);
	if (((WN_operator (rhs_wn) != OPR_COMMA) ||
	    (WN_has_side_effects (WN_kid1 (rhs_wn)))) 
#ifdef KEY
// Fix bug in which COMPOUND_EXPR has kid0==OPR_COMMA, kid1==OPR_CSELECT which
// has side-effects.
		&& (TREE_CODE(retval) == TARGET_EXPR)
#endif
	) {
//	  fdump_tree (stderr, rhs_wn);
	  Fail_FmtAssertion ("WFE_Expand_Return: TARGET_EXPR with cleanup");
	}
	WN_INSERT_BlockAfter (insertee, WN_last (insertee), cleanup_block);
      }
      else {
#ifndef KEY	// bug 3265
	if (WN_has_side_effects (rhs_wn)) {
	  DevWarn ("WFE_Expand_Return: cleanup block and expressson has side effects");
#endif
	  ST *ret_st = Gen_Temp_Symbol (ret_ty_idx, "__return_val");
#ifdef KEY
	  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, ret_st);
#endif
	  TYPE_ID ret_mtype = TY_mtype (ret_ty_idx);
	  WFE_Set_ST_Addr_Saved (rhs_wn);
	  wn = WN_Stid (ret_mtype, 0, ret_st, ret_ty_idx, rhs_wn);
	  WFE_Stmt_Append (wn, Get_Srcpos ());
	  rhs_wn = WN_Ldid (ret_mtype, 0, ret_st, ret_ty_idx);
#ifndef KEY
	}
#endif
	WFE_Stmt_Append (cleanup_block, Get_Srcpos ());
      }
    }
    
    if ((!WFE_Keep_Zero_Length_Structs    &&
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
        WFE_Stmt_Append(rhs_wn, Get_Srcpos());
      }
      wn = WN_CreateReturn ();
    }
#ifdef KEY
    else if (TY_return_in_mem(ret_ty_idx)) {
      // Copy the return value into the return area.  Based on code in
      // lower_return_val().

      FmtAssert (TY_mtype (ret_ty_idx) == MTYPE_M,
		 ("WFE_Expand_Return: return_in_mem type is not MTYPE_M"));

      WN *first_formal = WN_formal(Current_Entry_WN(), 0);
      TY_IDX tidx = ST_type(WN_st(first_formal));
      FmtAssert (MTYPE_is_pointer(TY_mtype(tidx)),
		 ("WFE_Expand_Return: fake param is not a pointer"));
      WN *dest_addr = WN_CreateLdid(OPR_LDID, TY_mtype(Ty_Table[tidx]),
				    TY_mtype(Ty_Table[tidx]),
				    WN_idname_offset(first_formal),
				    WN_st(first_formal), tidx);
      // Make sure there's no copy constructor, then do regular store.
      tree return_type = TREE_TYPE(TREE_TYPE(Current_Function_Decl()));
      Is_True(!WFE_has_copy_constructor(return_type),
	      ("WFE_Expand_Return: return type has copy constructor"));
      // Create mstore.
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, MTYPE_M, 0, tidx, rhs_wn,
			   dest_addr, 0);
      WFE_Stmt_Append(wn, Get_Srcpos());
      // Create return.
      wn = WN_CreateReturn ();
    }
#endif
    else {
      WFE_Set_ST_Addr_Saved (rhs_wn);
      wn = WN_CreateReturn_Val(OPR_RETURN_VAL, WN_rtype(rhs_wn), MTYPE_V, rhs_wn);
    }
  }

  WFE_Stmt_Append(wn, Get_Srcpos());
} /* WFE_Expand_Return */


void
Mark_Scopes_And_Labels (tree stmt)
{
  if (!stmt) return;

  switch (TREE_CODE(stmt)) {
    case COMPOUND_STMT: {
      tree t;
      for (t = COMPOUND_BODY(stmt); t; t = TREE_CHAIN(t))
	Mark_Scopes_And_Labels (t);
      break;
    }

    case DO_STMT:
#ifdef KEY
    {
      tree body = WHILE_BODY(stmt);
      while (body) {
	Mark_Scopes_And_Labels (body);
  	body = TREE_CHAIN(body);
      }
      break;
    }
#else
      Mark_Scopes_And_Labels (DO_BODY(stmt));
      break;
#endif

    case FOR_STMT: {
      tree init = FOR_INIT_STMT(stmt);
      tree cond = FOR_COND(stmt);
      tree body = FOR_BODY(stmt);
      while (init) {
	Mark_Scopes_And_Labels (init);
	init = TREE_CHAIN(init);
      }
#ifdef KEY
// handle "for (;;) ;"
      if (cond && (TREE_CODE(cond) == TREE_LIST))
#else
      if (TREE_CODE(cond) == TREE_LIST)
#endif // KEY
	Mark_Scopes_And_Labels(cond);
      while (body) {
	Mark_Scopes_And_Labels (body);
  	body = TREE_CHAIN(body);
      }
      break;
    }

    case IF_STMT:
      Mark_Scopes_And_Labels (IF_COND(stmt));
      Mark_Scopes_And_Labels (THEN_CLAUSE(stmt));
      Mark_Scopes_And_Labels (ELSE_CLAUSE(stmt));
      break;

    case LABEL_STMT:
      if (scope_i == -1)
	LABEL_SCOPE(LABEL_STMT_LABEL(stmt)) = NULL_TREE;
      else
	LABEL_SCOPE(LABEL_STMT_LABEL(stmt)) = scope_stack [scope_i];
      break;

    case SCOPE_STMT:
      if (SCOPE_BEGIN_P(stmt)) {
#ifdef KEY
	if (scope_i != -1)
	    PARENT_SCOPE(stmt) = scope_stack[scope_i];
	else
	    PARENT_SCOPE(stmt) = 0;
#endif
	Push_Scope(stmt);
      }
      else {
	SCOPE_NUMBER(scope_stack [scope_i]) = ++scope_number;
	--scope_i;
      }
      break;

    case SWITCH_STMT:
      Mark_Scopes_And_Labels (SWITCH_COND(stmt));
      Mark_Scopes_And_Labels (SWITCH_BODY(stmt));
      break;

    case TREE_LIST:
      for (tree t = TREE_PURPOSE(stmt); stmt; stmt = TREE_CHAIN(stmt))
	Mark_Scopes_And_Labels(t);
      break;

    case TRY_BLOCK: {
      tree handler;
      Mark_Scopes_And_Labels (TRY_STMTS(stmt));
      for (handler = TRY_HANDLERS(stmt);
		     handler;
	 	     handler = TREE_CHAIN(handler))
	  for (tree t = HANDLER_BODY(handler); t; t = TREE_CHAIN(t))
	    Mark_Scopes_And_Labels (t);
      break;
      }

    case WHILE_STMT:
#ifdef KEY
    {
      Mark_Scopes_And_Labels (WHILE_COND(stmt));
      tree body = WHILE_BODY(stmt);
      while (body) {
	Mark_Scopes_And_Labels (body);
  	body = TREE_CHAIN(body);
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
WFE_Expand_Start_Case (tree selector)
{
#ifdef KEY
  TYPE_ID index_mtype;
  if (TREE_CODE (selector) == TREE_LIST)
  	index_mtype = Mtype_comparison (
                         TY_mtype (Get_TY (TREE_TYPE (TREE_VALUE(selector))))); 
  else
  	index_mtype = Mtype_comparison (
                         TY_mtype (Get_TY (TREE_TYPE (selector)))); 
#else
  TYPE_ID index_mtype = Mtype_comparison (
                         TY_mtype (Get_TY (TREE_TYPE (TREE_VALUE(selector))))); 
#endif // KEY

  WN *switch_block = WN_CreateBlock ();
  WN *index;
  index = WFE_Expand_Expr_With_Sequence_Point (selector, index_mtype);
  WFE_Stmt_Push (switch_block, wfe_stmk_switch, Get_Srcpos());
  if (++switch_info_i == switch_info_max) {
    switch_info_max   = ENLARGE(switch_info_max);
    switch_info_stack = (SWITCH_INFO *) realloc (switch_info_stack,
                                                 switch_info_max * sizeof (SWITCH_INFO));
  }
  switch_info_stack [switch_info_i].index             = index;
  switch_info_stack [switch_info_i].start_case_index  = case_info_i + 1;
  switch_info_stack [switch_info_i].default_label_idx = 0;
  WFE_Record_Loop_Switch (SWITCH_STMT);
} /* WFE_Expand_Start_Case */

static void
WFE_Expand_End_Case (void)
{
  INT32  i;
  INT32  n;
  WN    *switch_wn;
  WN    *switch_block;
  WN    *case_block;
  WN    *case_entry;
  WN    *def_goto;
  WN    *wn;
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
  for (i = switch_info_stack [switch_info_i].start_case_index;
       i <= case_info_i;
       i++) {
    INT64     case_value;
    LABEL_IDX case_label_idx = case_info_stack [i].case_label_idx;
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
  }
  switch_wn = WN_CreateSwitch (n,
                               switch_info_stack [switch_info_i].index,
                               case_block,
                               def_goto,
                               exit_label_idx);
  switch_block = WFE_Stmt_Pop (wfe_stmk_switch);
  WFE_Stmt_Append (switch_wn, Get_Srcpos ());
  WFE_Stmt_Append (switch_block, Get_Srcpos ());
  wn = WN_CreateLabel ((ST_IDX) 0, exit_label_idx, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos ());
  case_info_i = switch_info_stack [switch_info_i].start_case_index - 1;
  --switch_info_i;
} /* WFE_Expand_End_Case */

static void
WFE_Expand_Switch (tree stmt)
{
  WFE_Expand_Start_Case (SWITCH_COND(stmt));
  WFE_Expand_Stmt       (SWITCH_BODY(stmt));
  WFE_Expand_End_Case   ();
  --break_continue_info_i;
}

static void
Set_Handler_Labels (tree stmt)
{
  for (tree handler = TRY_HANDLERS(stmt);
       handler;
       handler = TREE_CHAIN (handler)) {
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
    tree t = scope_cleanup_stack [i].stmt;
    if (TREE_CODE(t) == CLEANUP_STMT)
      return 1;
    INT result = 0;
    if (TREE_CODE(t) == TRY_BLOCK) {
      for (tree handler = TRY_HANDLERS(t);
           handler;
	   handler = TREE_CHAIN(handler))
        ++result;
      return result;
    }
  }

  return 0;
}

static ST_IDX
Tid_For_Handler (tree handler)
{
  tree t = HANDLER_BODY (handler);
  while (TREE_CODE(t) != COMPOUND_STMT)
    t = TREE_CHAIN(t);
  t = COMPOUND_BODY(t);
  t = TREE_TYPE(t);
  return t ? ST_st_idx(Get_ST (TREE_OPERAND(t, 0))) : 0;
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
  while (TREE_CODE(scope_cleanup_stack [j].stmt) == SCOPE_STMT)
    --j;
  tree t = scope_cleanup_stack [j].stmt;
  if (TREE_CODE(t) == TRY_BLOCK && TREE_CODE(TRY_HANDLERS(t)) == HANDLER) {
    for (tree handler = TRY_HANDLERS(t);
         handler;
         handler = TREE_CHAIN(handler))
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
static tree
Get_typeinfo_var (tree t)
{
    tree ti_var = 0;
    if (CLASS_TYPE_P(t))
	ti_var = CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (t));
    else // e.g. ordinary type
	ti_var = IDENTIFIER_GLOBAL_VALUE (mangle_typeinfo_for_type(t));
    FmtAssert (ti_var, ("Typeinfo of handler unavailable"));
    if (DECL_ASSEMBLER_NAME_SET_P (ti_var) && 
    	TREE_NOT_EMITTED_BY_GXX (ti_var) && 
	!TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (ti_var)))
    {
	// Add it to the vector so that we emit them later
	TREE_NOT_EMITTED_BY_GXX (ti_var) = 0;
	TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (ti_var)) = 1;
	gxx_emits_typeinfos (ti_var);
    }
    return ti_var;
}

// Get the handlers for the current try block. Move up in scope and append any
// more handlers that may be present, to INITV.
static INITV_IDX
Create_handler_list (int scope_index)
{
  INITV_IDX type_st, prev_type_st=0, start=0;

  FmtAssert (TREE_CODE(scope_cleanup_stack[scope_index].stmt) == TRY_BLOCK,
			("EH Error"));
  for (int i=scope_index; i>=0; i--)
  {
    tree t = scope_cleanup_stack[i].stmt;
    if ((TREE_CODE(t) != TRY_BLOCK) || CLEANUP_P(t))	continue;

    tree h = TRY_HANDLERS (t);
    if (key_exceptions)
    {
    FmtAssert (h, ("Create_handler_list: Null handlers"));
    FmtAssert (TREE_CODE(h) == HANDLER, ("Create_handler_list: TREE_CODE HANDLER expected"));
    }
    while (h)
    {
	type_st = New_INITV();
        tree type = HANDLER_TYPE(h);

	ST_IDX st = 0;
	if (type) st = ST_st_idx (Get_ST (Get_typeinfo_var(type)));
	INITV_Set_VAL (Initv_Table[type_st], Enter_tcon (Host_To_Targ (MTYPE_U4, st)), 1);

	h = TREE_CHAIN(h);
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
lookup_handlers (vector<tree> *cleanups)
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
    	vector<tree> * temp = hi.cleanups;
	for (vector<tree>::iterator j = temp->begin(); j != temp->end(); ++j)
	    cleanups->push_back (*j);
    }
    return start;
}

LABEL_IDX
New_eh_cleanup_entry (tree t, vector<tree> *v, LABEL_IDX goto_idx)
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

static void
append_catch_all (INITV_IDX& iv)
{
  INITV_IDX tmp = iv;
  while (tmp && INITV_next (tmp))
	tmp = INITV_next (tmp);

  INITV_IDX catch_all = New_INITV();
  INITV_Set_VAL (Initv_Table[catch_all], Enter_tcon (Host_To_Targ (MTYPE_U4, 0)), 1);
  if (tmp) Set_INITV_next (tmp, catch_all);
  else iv = catch_all;
}

// current: D1 D2 D3, prev: D1 D2 => emit D3 for current, goto prev
// current: D1 D2 D3, prev: D1 D2 D4 => don't optimize now
static bool
optimize_cleanups (vector<tree> * current, vector<tree> * prev)
{
  if (prev->size() >= current->size())
  	return false;
  reverse (current->begin(), current->end());
  reverse (prev->begin(), prev->end());
  vector<tree>::iterator c = current->begin();
  for (vector<tree>::iterator p = prev->begin(); p != prev->end(); ++p, ++c)
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
  tree t=0;
  iv = 0;
  vector<tree> *cleanups = new vector<tree>();

  if (scope_cleanup_i == -1) 
  {
	iv = New_INITV();
	INITV_Set_ZERO (Initv_Table[iv], MTYPE_U4, 1);
	return 0;
  }
  tree temp_cleanup=0;
  for (int i=temp_cleanup_i; i>=0; --i)
  {
	TEMP_CLEANUP_INFO t = temp_cleanup_stack[i];
  	if (t.label_idx && t.cleanup_eh_only)
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
	if (TREE_CODE(t) == CLEANUP_STMT)
		cleanups->push_back (t);
	if (TREE_CODE(t) == TRY_BLOCK)
	    if (CLEANUP_P(t)) cleanups->push_back (TRY_HANDLERS(t));
	    else break;
  	if (temp_cleanup && (cleanups->size() == 1))
	{
		cleanups->push_back (temp_cleanup);
		temp_cleanup = 0;
	}
  }
  if (temp_cleanup)
  	cleanups->push_back (temp_cleanup);
  tree h = 0;
  if (TREE_CODE(t) == TRY_BLOCK)
  {
	h = TRY_HANDLERS (t);
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
  bool catch_all_appended = false;
  if (PU_needs_manual_unwinding (Get_Current_PU()))
  {
	append_catch_all (iv);
	catch_all_appended = true;
  }
  if (processing_handler)
  {
  	vector<ST_IDX> * eh_spec = handler_stack.top().eh_spec;
	FmtAssert (eh_spec, ("Invalid eh_spec inside handler"));
	if (!eh_spec->empty())
	{
	    if (!catch_all_appended)
	    	append_catch_all (iv);
	    append_eh_filter (iv);
  	}
  }
  else if (!eh_spec_vector.empty())
  {
	if (!catch_all_appended)
	    append_catch_all (iv);
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
static void
Get_handler_list (vector<ST_IDX> *handler_list)
{
  FmtAssert (TREE_CODE(scope_cleanup_stack[scope_cleanup_i+1].stmt) == 
		TRY_BLOCK, ("EH Error"));
  for (int i=scope_cleanup_i; i>=0; i--)
  {
    tree t = scope_cleanup_stack[i].stmt;
    if ((TREE_CODE(t) != TRY_BLOCK) || CLEANUP_P(t))	continue;

    tree h = TRY_HANDLERS (t);
    if (key_exceptions)
    {
    FmtAssert (h, ("Get_handler_list: Null handlers"));
    FmtAssert (TREE_CODE(h) == HANDLER, ("Get_handler_list: TREE_CODE HANDLER expected"));
    }
    while (h)
    {
        tree type = HANDLER_TYPE(h);
        ST_IDX st = 0;	// catch-all
	if (type)
	    st = ST_st_idx (Get_ST (Get_typeinfo_var(type)));
	handler_list->push_back (st);
	h = TREE_CHAIN (h);
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

static bool
Get_Cleanup_Info (vector<tree> *cleanups, LABEL_IDX *goto_idx)
{
  FmtAssert (TREE_CODE(scope_cleanup_stack[scope_cleanup_i+1].stmt)==TRY_BLOCK,
		("EH Processing Error"));

  for (int i=scope_cleanup_i; i>=0; i--)
  {
	tree t = scope_cleanup_stack[i].stmt;
	if (TREE_CODE(t) == CLEANUP_STMT)
		cleanups->push_back (t);
	if (TREE_CODE(t) == TRY_BLOCK)
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
  FmtAssert (TREE_CODE(scope_cleanup_stack[scope_cleanup_i].stmt) 
		== TRY_BLOCK, ("Scope Error in Get_Scope_Info"));
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
  FmtAssert (TREE_CODE(temp_cleanup_stack[temp_cleanup_i].expr) 
		== TRY_BLOCK, ("Scope Error"));
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

   while (break_continue_info_stack [i].tree_code == SWITCH_STMT) --i;
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
  FmtAssert (TREE_CODE(scope_cleanup_stack[scope_cleanup_i].stmt) 
		== TRY_BLOCK, ("Scope Error in Get_Break_Continue_Info"));
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
	tree t = scope_cleanup_stack[i].stmt;
	if (TREE_CODE(t) == CLEANUP_STMT)
 	{
		cleanups_seen = true;
		break;
	}
	if (TREE_CODE(t) == TRY_BLOCK)
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
WFE_Expand_Try (tree stmt)
{
  LABEL_IDX end_label_idx;
  WN *      end_label_wn;

  /*
   * Don't generate anything if there are no statements in the
   * try-block.
   */

  if (TRY_STMTS(stmt) == NULL_TREE)
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

  // bug fix for OSP_159 & OSP_160 
  if (!opt_regions)
    Push_Scope_Cleanup (stmt);

#ifdef KEY
// FIXME: handle temp cleanups for return from handler.
  vector<TEMP_CLEANUP_INFO> *temp_cleanup = 0;
  int handler_count=0;
  WN * region_body;
  if (key_exceptions)
  {
    region_body = WN_CreateBlock();
    WFE_Stmt_Push (region_body, wfe_stmk_region_body, Get_Srcpos());
    
    // bug fix for OSP_159 & OSP_160
    if (opt_regions)
      Push_Scope_Cleanup (stmt);
    handler_count = cleanup_list_for_eh.size();
  }
  vector<SCOPE_CLEANUP_INFO> *scope_cleanup = Get_Scope_Info ();
  vector<BREAK_CONTINUE_INFO> *break_continue = Get_Break_Continue_Info ();
#endif // KEY

  /* Generate code for the try-block. */

  for (tree s = TRY_STMTS(stmt); s; s = TREE_CHAIN(s))
    WFE_Expand_Stmt(s);
  // bug fix for OSP_159 & OSP_160
  if (!opt_regions)
    --scope_cleanup_i;

#ifdef KEY
  LABEL_IDX start = 0;
  if (key_exceptions)
  {
    WFE_Stmt_Pop (wfe_stmk_region_body);
    // bug fix for OSP_159 & OSP_160
    if (opt_regions)
      --scope_cleanup_i;
    WN * region_pragmas = WN_CreateBlock();
    FmtAssert (cleanup_list_for_eh.size() >= handler_count, ("Cleanups cannot be removed here"));
    LABEL_IDX cmp_idx = scope_cleanup_stack[scope_cleanup_i+1].cmp_idx;
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
    WFE_Stmt_Append (WN_CreateRegion (REGION_KIND_TRY, region_body,
    	region_pragmas, WN_CreateBlock(), New_Region_Id(), ereg_supp), 
	Get_Srcpos());
    Set_PU_has_region (Get_Current_PU());
    
    //The following code creat a new TY for the ST that is created
    //above. Because in CG, we will get the size of the ST from its
    //TY, we should get its right size from the INITO attach with 
    //it, and write it into a new TY
    UINT inito_size;
    TY_IDX tyi;     
    TY& zty = New_TY(tyi);
    inito_size = Get_INITO_Size (ereg_supp);
    TY_Init (zty, inito_size, KIND_STRUCT, MTYPE_M,
	     ereg -> u1.name_idx);
    Set_TY_align (tyi, 4);
    ST_Init (ereg, TY_name_idx(zty),CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, tyi);
    Set_ST_is_initialized (ereg);
    Set_ST_is_not_used (ereg);
  }
  vector<tree> *cleanups = new vector<tree>();
  LABEL_IDX cmp_idxs[2];
  cmp_idxs[0] = scope_cleanup_stack[scope_cleanup_i+1].cmp_idx;
  cmp_idxs[1] = start;
  LABEL_IDX goto_idx=0;
  bool outermost = 0;
  if (key_exceptions) outermost = Get_Cleanup_Info (cleanups, &goto_idx);
  vector<ST_IDX> *handler_list = new vector<ST_IDX>();
  vector<ST_IDX> * eh_spec_list = NULL;
  if (key_exceptions) 
  {
    Get_handler_list (handler_list);
    eh_spec_list = new vector<ST_IDX>();
    Get_eh_spec (eh_spec_list);
  }
#endif // KEY

  /* Generate a label for the handlers to branch back to. */

  New_LABEL (CURRENT_SYMTAB, end_label_idx);

  /* Handler code will be generated later, at end of function. */

#ifdef KEY
  Push_Handler_Info (TRY_HANDLERS(stmt), cleanups, scope_cleanup, temp_cleanup,
	break_continue, handler_list, eh_spec_list, end_label_idx, outermost, 
	cmp_idxs, goto_idx);
#else
  Push_Handler_Info (TRY_HANDLERS(stmt), end_label_idx);
#endif // KEY

  /* Emit label after handlers. */

  end_label_wn = WN_CreateLabel ((ST_IDX) 0, end_label_idx, 0, NULL);
  WFE_Stmt_Append (end_label_wn, Get_Srcpos());
} /* WFE_Expand_Try */

#ifdef KEY
static int
sizeof_eh_spec (tree t)
{
  int i=1;
  for (; t; t = TREE_CHAIN(t), i++) ;
  return i;
}
#endif

static void
WFE_Expand_EH_Spec (tree stmt)
{
      // This is what g++'s genrtl_eh_spec_block routine (in cp/semantics.c)
      // does:
      //   expand_eh_region_start ();
      //   expand_stmt (EH_SPEC_STMTS (t));
      //   expand_eh_region_end_allowed (...);
#ifdef KEY
      int bkup = current_eh_spec_ofst;
      int initial_size = eh_spec_vector.size();
      if (key_exceptions)
      {
        // Generally, there is 1 exception specification per function.
        // After inlining (by the GNU front-end or inliner/ipa), the caller
        // function can have multiple specifications. Any inlining by GNU is
        // taken care of here by updating current_eh_spec_ofst.
        // TODO: cmp with -1 before calling unexpected needs to be changed.
        tree eh_spec = EH_SPEC_RAISES (stmt);
        current_eh_spec_ofst = initial_size+1;
        if (eh_spec_vector.empty())
          eh_spec_vector.reserve (sizeof_eh_spec (eh_spec));
        for (; eh_spec; eh_spec = TREE_CHAIN (eh_spec))
        {
          ST_IDX type_st = ST_st_idx (Get_ST ( 
			Get_typeinfo_var(TREE_VALUE(eh_spec))));
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
      //           and WFE_Expand_Stmt() will return right after! The rest of the
      //           statements in the EH_SPEC_STMTS list will be  d r o p p e d  !
      //           (If the first statement were a compound statement, it will not 
      //           immediately be obvious that statements after the first statement
      //           in the EH_SPEC_STMTS are being skipped)).
      //
      tree eh_spec_stmt;
      for (eh_spec_stmt = EH_SPEC_STMTS (stmt); eh_spec_stmt != NULL; eh_spec_stmt = TREE_CHAIN(eh_spec_stmt))
        WFE_Expand_Stmt (eh_spec_stmt);
#else
      WFE_Expand_Stmt (EH_SPEC_STMTS (stmt));
#endif
#ifdef KEY
      if (key_exceptions)
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
  WFE_Stmt_Append (call_wn, Get_Srcpos());
}

void
Call_Throw (void)
{
}

void
Call_Rethrow (void)
{
}

void Call_Terminate (void)
{
#ifdef KEY
  static ST * st = NULL;
  if (st == NULL) {
    st = Function_ST_For_String ("_ZSt9terminatev");
  }
  Call_Named_Function (st);
#else
#endif // KEY
}

#ifdef KEY
static void Generate_filter_cmp (int filter, LABEL_IDX goto_idx);
static WN *
Generate_cxa_call_unexpected (void)
{
  ST_IDX exc_ptr_param = TCON_uval (INITV_tc_val (INITO_val (PU_misc_info (Get_Current_PU()))));
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
  ST_IDX exc_ptr_param = TCON_uval (INITV_tc_val (INITO_val (PU_misc_info (Get_Current_PU()))));
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

  if (key_exceptions)
  	WFE_Stmt_Push (WN_CreateBlock(), wfe_stmk_region_body, Get_Srcpos());
  WFE_Stmt_Append (call_wn, Get_Srcpos());
  if (key_exceptions)
  	Setup_EH_Region (1 /* for _Unwind_Resume */);
// We would ideally want to put it inside the above region, but we cannot
// jmp from outside a region into it.
  if (!eh_spec_func_end.empty())
  {
  	WFE_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, goto_unexpected, 0, NULL),
    		Get_Srcpos());
  	if (key_exceptions)
  	    WFE_Stmt_Push (WN_CreateBlock(), wfe_stmk_region_body, Get_Srcpos());
  	WFE_Stmt_Append (call_unexpected, Get_Srcpos());
  	if (key_exceptions)
  	    Setup_EH_Region (1 /* for __cxa_call_unexpected */);
  }
}

static void
Generate_filter_cmp (int filter, LABEL_IDX goto_idx)
{
  ST_IDX filter_param = TCON_uval (INITV_tc_val (INITV_next (INITO_val (PU_misc_info (Get_Current_PU())))));
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
                                                                                
  WFE_Stmt_Append (if_blk, Get_Srcpos());
}
#endif // KEY

// for a catch-all clause, pass a typeinfo of ZERO. This typeinfo needs
// to be handled specially. Moreover, we must not pass 0 for any other
// typeinfo.
static void
WFE_Expand_Handlers_Or_Cleanup (const HANDLER_INFO &handler_info)
{
  tree t = handler_info.handler;
  vector<tree> *cleanups = handler_info.cleanups;
  LABEL_IDX label_idx = handler_info.label_idx;
  LABEL_IDX goto_idx = handler_info.goto_idx;
  LABEL_IDX cleanups_idx = handler_info.cleanups_idx;
  bool outermost = handler_info.outermost;
#ifndef KEY
  WFE_Stmt_Append (
    WN_CreateLabel ((ST_IDX) 0, HANDLER_LABEL(t), 0, NULL),
    Get_Srcpos());
#endif // !KEY
  
  if (TREE_CODE(t) == HANDLER) {

#ifdef KEY
    if (key_exceptions)
    {
      tree t_copy = t;
      while (t_copy)
      {
        tree type = HANDLER_TYPE(t_copy);
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
      		WFE_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, 
				HANDLER_LABEL(t_copy)), Get_Srcpos());
        }
        else 
	{
	  if (e.st)
	  	Generate_filter_cmp ((*f).filter, HANDLER_LABEL(t_copy));
	  else // catch-all, so do not compare filter
      		WFE_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, 
				HANDLER_LABEL(t_copy)), Get_Srcpos());
	}
        t_copy = TREE_CHAIN(t_copy);
      }

  WFE_Stmt_Append (
    WN_CreateLabel ((ST_IDX) 0, cleanups_idx, 0, NULL), Get_Srcpos());
// Generate any cleanups that need to be executed before going to the outer
// scope, which would be a handler in the same PU or a call to _Unwind_Resume
      in_cleanup = TRUE;
      for (vector<tree>::iterator j=cleanups->begin();
		j!=cleanups->end(); ++j)
    	  WFE_One_Stmt_Cleanup (CLEANUP_EXPR (*j));

      in_cleanup = FALSE;
// generate a call to _Unwind_Resume(struct _Unwind_Exception *)
      if (outermost)
      {
	FmtAssert (goto_idx == 0, ("Goto label should be 0"));
	Generate_unwind_resume ();
      }
      else
      	WFE_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, goto_idx), Get_Srcpos());
    } // key_exceptions
#endif // KEY
    while (t) {
#ifdef KEY
// need a label in front of each handler, so that we can jump to the
// proper label from 'cmp' above
  WFE_Stmt_Append (
    WN_CreateLabel ((ST_IDX) 0, HANDLER_LABEL(t), 0, NULL), Get_Srcpos());
#endif
      tree body = HANDLER_BODY(t);
      for (; body; body = TREE_CHAIN(body))
	WFE_Expand_Stmt (body);
      WFE_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, label_idx),
		       Get_Srcpos());
      t = TREE_CHAIN(t);
    }
  } else {
// We will see if control reaches here.
// Let me comment this out, may need to do something else later.
      //Fail_FmtAssertion ("Handle it");
  }    
}

#ifdef KEY
#include "omp_directive.h"
static void
WFE_Expand_Omp (tree stmt)
{
  switch (stmt->omp.choice)
  {
    case parallel_dir_b:
      expand_start_parallel ((struct parallel_clause_list *) stmt->omp.omp_clause_list);
      break;
    case parallel_dir_e:
      expand_end_parallel ();
      break;
    case for_dir_b:
      expand_start_for ((struct for_clause_list *) stmt->omp.omp_clause_list);
      break;
    case for_dir_e:
      expand_end_for ();
      break;
    case sections_cons_b:
      expand_start_sections ((struct sections_clause_list *) stmt->omp.omp_clause_list);
      break;
    case sections_cons_e:
      expand_end_sections ();
      break;
    case section_cons_b:
      expand_start_section ();
      break;
    case section_cons_e:
      expand_end_section ();
      break;
    case single_cons_b:
      expand_start_single ((struct single_clause_list *) stmt->omp.omp_clause_list);      break;
    case single_cons_e:
      expand_end_single ();
      break;
    case par_for_cons_b:
      expand_start_parallel_for ((struct parallel_for_clause_list *) stmt->omp.omp_clause_list);
      break;
    case par_for_cons_e:
      expand_end_parallel_for ();
      break;
    case par_sctn_cons_b:
      expand_start_parallel_sections ((struct parallel_sections_clause_list *) stmt->omp.omp_clause_list);
      break;
    case par_sctn_cons_e:
      expand_end_parallel_sections ();
      break;
    case master_cons_b:
      expand_start_master ();
      break;
    case master_cons_e:
      expand_end_master ();
      break;
    case critical_cons_b:
      expand_start_critical ((tree) stmt->omp.omp_clause_list);
      break;
    case critical_cons_e:
      expand_end_critical ();
      break;
   case barrier_dir:
      expand_barrier ();
      break;
    case flush_dir:
      expand_flush ((tree) stmt->omp.omp_clause_list);
      break;
    case atomic_cons_b:
      expand_start_atomic ();
      break;
    case atomic_cons_e:
      expand_end_atomic ();
      break;
    case ordered_cons_b:
      expand_start_ordered ();
      break;
    case ordered_cons_e:
      expand_end_ordered ();
      break;
    case thdprv_dir:
      expand_threadprivate ((tree) stmt->omp.omp_clause_list);
      break;
    case options_dir:
    case exec_freq_dir:
      WFE_Expand_Pragma (stmt);
      break;

#ifdef TARG_SL  // fork_joint
    case sl2_sections_cons_b:
    case sl2_minor_sections_cons_b:		
      expand_start_sl2_sections (stmt->omp.choice == sl2_minor_sections_cons_b);
      break;
    case sl2_sections_cons_e:
      expand_end_sl2_sections ();
      break;
    case sl2_section_cons_b:
    case sl2_minor_section_cons_b:		
      expand_start_sl2_section (stmt->omp.choice == sl2_minor_section_cons_b);
      break;
    case sl2_section_cons_e:
    case sl2_minor_section_cons_e:		
      expand_end_sl2_section ();
      break;
#endif 

                                                                                
    default:
      Fail_FmtAssertion ("Unexpected stmt node");
  }
}

void
WFE_Expand_DO (tree stmt)
{
  tree init, incr, cond, body;

  WFE_Record_Loop_Switch  (TREE_CODE (stmt));

  init = FOR_INIT_STMT (stmt);
  incr = FOR_EXPR(stmt);
  cond = FOR_COND(stmt);
  body = FOR_BODY(stmt);
  expand_start_do_loop (init, cond, incr);
  while (body)
  {
    WFE_Expand_Stmt (body);
    body = TREE_CHAIN(body);
  }

  // label for continue
  if (break_continue_info_stack [break_continue_info_i].continue_label_idx)
  {
      WFE_Stmt_Append (
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
      WFE_Stmt_Append (
	WN_CreateLabel ((ST_IDX) 0,
			break_continue_info_stack
			  [break_continue_info_i].break_label_idx,
			0, NULL),
	Get_Srcpos());
  }
  --break_continue_info_i;
}
#endif // KEY

void
WFE_Expand_Stmt(tree stmt, WN* target_wn)
{
#ifdef WFE_DEBUG
  fprintf (stderr,
           "{( WFE_Expand_Expr: %s\n", WFE_Tree_Node_Name (stmt)); // ")}"
#endif /* WFE_DEBUG */

#ifdef KEY
#endif

 if (TREE_CODE(stmt) == LABEL_DECL)
   lineno = DECL_SOURCE_LINE(stmt);
 else
 if (TREE_CODE(stmt) != CASE_LABEL)
   lineno = STMT_LINENO(stmt);

 if (STMT_IS_FULL_EXPR_P(stmt))
    Push_Temp_Cleanup (stmt, false);
    
  switch (TREE_CODE(stmt)) {
    case ASM_STMT:
      Wfe_Expand_Asm_Operands (ASM_STRING    (stmt),
			       ASM_OUTPUTS   (stmt),
			       ASM_INPUTS    (stmt),
			       ASM_CLOBBERS  (stmt),
			       ASM_VOLATILE_P(stmt),
			       NULL,
			       0);
      break;

    case BREAK_STMT:
      WFE_Expand_Break ();
      break;

    case CASE_LABEL:
      WFE_Expand_Case (CASE_LOW(stmt), CASE_HIGH(stmt));
      break;

    case CLEANUP_STMT:
#ifdef KEY
      if (opt_regions && Check_For_Call_Region ())
          Did_Not_Terminate_Region = FALSE;
      if (!CLEANUP_EH_ONLY(stmt))
    	  Push_Scope_Cleanup (stmt);
      else Push_Scope_Cleanup (stmt, true /* cleanup_eh_only */);
#else
      Push_Scope_Cleanup (stmt);
#endif
      break;

    case COMPOUND_STMT: {
      tree t;
      for (t = COMPOUND_BODY(stmt);
	   t != NULL;
	   t = TREE_CHAIN(t))
	WFE_Expand_Stmt (t, target_wn);
      break;
    }

    case CONTINUE_STMT:
      WFE_Expand_Continue ();
      break;

    case DECL_STMT:
#ifdef KEY
      // If the node is an INDIRECT_REF, then it's because we changed it from a
      // VAR_DECL to an INDIRECT_REF for the named return value optimization.
      // In this case, do nothing if the decl has no initializer; otherwise,
      // expand named_ret_obj_initalizer, which is the initializer that we've
      // save as a TARGET_EXPR.
      if (TREE_CODE(DECL_STMT_DECL(stmt)) == INDIRECT_REF) {
	if (named_ret_obj_initializer) {
	  WFE_Expand_Expr(named_ret_obj_initializer);
	}
	break;
      }
#endif
      WFE_Expand_Decl (DECL_STMT_DECL (stmt));
      break;

    case DO_STMT:
      WFE_Expand_Loop (stmt);
      break;

    case EXPR_STMT:
      WFE_One_Stmt (EXPR_STMT_EXPR(stmt), target_wn);
      break;

    case FOR_STMT:
#ifdef KEY
      if (TREE_ADDRESSABLE (stmt))	// OpenMP DO loop
        WFE_Expand_DO (stmt);
      else
#endif
      WFE_Expand_Loop (stmt);
      break;

    case GOTO_STMT: {
      tree dest = GOTO_DESTINATION(stmt);
      if (TREE_CODE(dest) == LABEL_DECL)
        WFE_Expand_Goto (dest);
      else
        WFE_Expand_Computed_Goto(dest);
      break;
    }

    case IF_STMT:
      WFE_Expand_If (stmt);
      break;

    case LABEL_STMT:
      WFE_Expand_Label (LABEL_STMT_LABEL(stmt));
      break;

    case RETURN_STMT: {
#ifdef KEY
      if (opt_regions && Check_For_Call_Region ())
          Did_Not_Terminate_Region = FALSE;
      tree t = RETURN_STMT_EXPR(stmt);
#else
      tree t = RETURN_EXPR(stmt);
#endif // KEY
      if (t && TREE_CODE(t) == INIT_EXPR) {
  	Is_True(TREE_CODE(TREE_OPERAND(t, 0)) == RESULT_DECL,
			  ("WFE_Expand_Stmt: expected RESULT_DECL"));
	tree t1 = TREE_OPERAND(t, 1);
	if (TREE_CODE(t1) == TARGET_EXPR)
  	  TREE_OPERAND(t1, 2) = 0;
	WFE_Expand_Return (stmt, t1);
      }
      else
	WFE_Expand_Return(stmt, t);
      return; // We've already called Do_Temp_Cleanups!
    }

    case SCOPE_STMT:
#ifdef KEY
      if (opt_regions && Check_For_Call_Region ())
          Did_Not_Terminate_Region = FALSE;
#endif
      if (SCOPE_BEGIN_P(stmt))
	Push_Scope_Cleanup (stmt);
      else
	Pop_Scope_And_Do_Cleanups ();
      break;

#ifndef KEY
    case SUBOBJECT:
      break;
#endif // !KEY

    case SWITCH_STMT:
      WFE_Expand_Switch (stmt);
      break;

    case TRY_BLOCK:
#ifdef KEY
      // bug fix for OSP_159 & OSP_160 
      if (opt_regions && Check_For_Call_Region ())
        Did_Not_Terminate_Region = FALSE;
#endif
      WFE_Expand_Try (stmt);
      break;

    case WHILE_STMT:
      WFE_Expand_Loop (stmt);
      break;

#ifndef KEY
    case CTOR_STMT:
      DevWarn("Encountered CTOR_STMT (%s).  Ignoring.",
              (CTOR_BEGIN_P(stmt) ? "begin" : "end"));
      break;
#endif // !KEY

    case FILE_STMT:
      /* Simple enough to handle.  */
      input_filename = FILE_STMT_FILENAME (stmt);
#ifdef KEY //bug 10632,8895: File changed. Don't worry lineno
      WFE_Set_Line_And_File (lineno, input_filename);
#endif
      break;

    case EH_SPEC_BLOCK:
      WFE_Expand_EH_Spec (stmt);
      break;

    case USING_STMT:
      break;

#ifdef KEY
    case OMP_MARKER_STMT:
      WFE_Expand_Omp (stmt);
      break;
#endif // KEY

    default:
      Is_True(FALSE,
              ("WFE_Expand_Stmt: Unexpected statement node %s", WFE_Tree_Node_Name (stmt)));
      break;
  } /* switch */
  
  if (STMT_IS_FULL_EXPR_P(stmt))
    Do_Temp_Cleanups (stmt);

#ifdef WFE_DEBUG
  fprintf (stderr, // "{("
           ")} WFE_Expand_Expr: %s\n", WFE_Tree_Node_Name (stmt));
#endif /* WFE_DEBUG */

} /* WFE_Expand_Stmt */

#ifdef KEY
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
WFE_fixup_target_expr (tree retval)
{
  tree decl = TREE_OPERAND(retval, 0);
  FmtAssert(TREE_CODE(decl) == VAR_DECL,
	    ("WFE_fixup_target_expr: VAR_DECL not found in TARGET_EXPR"));

  // Get the ST for the fake first parm.
  WN *first_formal = WN_formal(Current_Entry_WN(), 0);

  // Change the TARGET_EXPR's DECL to be an INDIRECT_REF of the fake first
  // parm.
  tree ptr_var = build_decl(VAR_DECL, NULL_TREE,
  			    build_pointer_type(TREE_TYPE(retval)));
  TREE_SET_CODE(decl, INDIRECT_REF);
  TREE_OPERAND(decl, 0) = ptr_var;
  set_DECL_ST(ptr_var, WN_st(first_formal));
}

// Return TRUE if TYPE has copy constructor.
bool
WFE_has_copy_constructor (tree type)
{
  if (CLASS_TYPE_P(type) &&
      CLASSTYPE_COPY_CONSTRUCTOR(type) != NULL)
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
    if (TREE_CODE (scope_cleanup_stack[i].stmt) == SCOPE_STMT)
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
  Is_True (TREE_CODE (scope_cleanup_stack[idx].stmt) == SCOPE_STMT,
  	("Unexpected tree code"));
  scope_cleanup_stack[idx].vla.alloca_st = st;
}

// Save st's for kids 1..n of DEALLOCA for scope 'idx'
void
Add_Current_Scope_Alloca_St (ST * st, int idx)
{
  Is_True (TREE_CODE (scope_cleanup_stack[idx].stmt) == SCOPE_STMT,
  	("Unexpected tree code"));
  scope_cleanup_stack[idx].vla.alloca_sts_vector->push_back (st);
}

// This function is intended to be a general function to handle different
// pragmas (other than openmp pragmas). Currently it handles
// #pragma options, mips_frequency_hint
//
void
WFE_Expand_Pragma (tree exp)
{
  switch (exp->omp.choice)
  {
    case options_dir:
    { // pragma options
      TCON tcon;
      exp = (tree) exp->omp.omp_clause_list;
      tcon = Host_To_Targ_String (MTYPE_STRING,
                                  const_cast<char*>TREE_STRING_POINTER(exp),
                                  TREE_STRING_LENGTH(exp) - 1 /* ignore \0 */);
      TY_IDX ty_idx = Get_TY(TREE_TYPE(exp));
      ST * st = New_Const_Sym (Enter_tcon (tcon), ty_idx);  
      TREE_STRING_ST (exp) = st;
      WN * wn = WN_CreatePragma (WN_PRAGMA_OPTIONS, st, 0, 0);
      WN * func_wn = WFE_Find_Stmt_In_Stack (wfe_stmk_func_entry);
      WN_INSERT_BlockLast (WN_func_pragmas(func_wn), wn);
      break;
    }
    case exec_freq_dir:
    { // pragma mips_frequency_hint
      MIPS_FREQUENCY_HINT freq_hint;
      Is_True (TREE_CODE ((tree) exp->omp.omp_clause_list) == STRING_CST,
               ("Expected string constant with mips_frequency_hint"));
      const char * hint = TREE_STRING_POINTER ((tree) exp->omp.omp_clause_list);                                                                                
      if (!strcmp (hint, "never")) freq_hint = FREQUENCY_HINT_NEVER;
      else if (!strcmp (hint, "init")) freq_hint = FREQUENCY_HINT_INIT;
      else if (!strcmp (hint, "frequent")) freq_hint = FREQUENCY_HINT_FREQUENT;
      else // Invalid mips_frequency_hint
        break;
                                                                                
      WN * wn = WN_CreatePragma (WN_PRAGMA_MIPS_FREQUENCY_HINT, (ST*)NULL, freq_hint, 0);
      WFE_Stmt_Append (wn, Get_Srcpos());
      break;
    }
  }
}
#endif
