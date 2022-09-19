/*
  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

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

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

#ifndef CLANG2WHIRL_STMT_H
#define CLANG2WHIRL_STMT_H

// clang forward declarations
#include <vector>
#include "clangdecl.h"
// open64 forward declarations
#include "open64decl.h"
#include "clanginc.h"
#include "c2w_builder.h"
#include "c2w_map.h"

typedef struct break_continue_info_t {
  clang::Stmt::StmtClass       tree_code;
  LABEL_IDX break_label_idx;
  LABEL_IDX continue_label_idx;
} BREAK_CONTINUE_INFO;

static INT32                break_continue_info_i = -1;
static INT32                break_continue_info_max = 32;
static BREAK_CONTINUE_INFO *break_continue_info_stack =
    (BREAK_CONTINUE_INFO *) malloc (sizeof (BREAK_CONTINUE_INFO) *
                                    break_continue_info_max);

typedef struct temp_cleanup_info_t {
  clang::Expr *            expr;
  LABEL_IDX         label_idx;
  bool              cleanup_eh_only;
} TEMP_CLEANUP_INFO;

static INT32              temp_cleanup_i = -1;
static INT32              temp_cleanup_max = 32;
static TEMP_CLEANUP_INFO *temp_cleanup_stack =
    (TEMP_CLEANUP_INFO *) malloc (sizeof (TEMP_CLEANUP_INFO) *
                                  temp_cleanup_max);;

// cmp_idx: for a try_block, it is the label where the first compare
//          for the handlers of this try-block should start.
typedef struct scope_cleanup_info_t {
  const clang::Stmt              *stmt;
  LABEL_IDX         label_idx;
  LABEL_IDX         cmp_idx;
  bool              cleanup_eh_only;
  struct vla_ {
    bool                    has_alloca;
    ST *                    alloca_st;
    vector<ST*> *           alloca_sts_vector;
  } vla;
} SCOPE_CLEANUP_INFO;

static INT32               scope_cleanup_i = -1;
static INT32               scope_cleanup_max = 32;
static SCOPE_CLEANUP_INFO *scope_cleanup_stack =
  (SCOPE_CLEANUP_INFO *) malloc (sizeof (SCOPE_CLEANUP_INFO) *
                                 scope_cleanup_max);

typedef std::pair<clang::QualType, ST_IDX> DTOR_ST_PAIR;
typedef std::vector<DTOR_ST_PAIR> DTOR_ST_VECTOR;

typedef struct handler_info_t {
  const clang::CXXTryStmt    *handler;
  DTOR_ST_VECTOR             *cleanups;
  vector<SCOPE_CLEANUP_INFO> *scope;
  vector<TEMP_CLEANUP_INFO> *temp_cleanup;
  vector<BREAK_CONTINUE_INFO> *break_continue;
  vector<ST_IDX>    *handler_list; // list of handlers outside this try-catch block
  vector<ST_IDX>    *eh_spec; // eh_spec of the containing region to be used while inside its handler
  LABEL_IDX         label_idx;
  // cmp_idx: 1st is the label where the first cmp for this handler set
  // should start. If the 2nd label is non-zero it must be marked
  // handler_begin
  LABEL_IDX         cmp_idx[2];
  // label to jmp to after this set of handlers are done
  LABEL_IDX         goto_idx;
  LABEL_IDX         cleanups_idx; //call uwind???
  bool              outermost; // handler for outermost try block in PU?
} HANDLER_INFO;

static INT32         handler_info_i = -1;
static INT32         handler_info_max = 32;
static HANDLER_INFO *handler_info_stack =
    (HANDLER_INFO *) malloc (sizeof(HANDLER_INFO) * handler_info_max);;

void Push_dtor_for_copy_ctor(clang::QualType ty, ST_IDX st);

void Push_dtor_call_stack(clang::QualType ty, ST_IDX st);

BOOL Is_dtor_call_stack_empty();

bool Maybe_do_eh_cleanups();

namespace wgen {

class WhirlBuilder;

class WhirlStmtBuilder {
private:
  WhirlBuilder *_builder;

  STMT_LABEL_IDX_MAP _stmt_label_map;

public:
  WhirlStmtBuilder(WhirlBuilder *builder);
  
  ~WhirlStmtBuilder();

private:
  void Call_unwind(SRCPOS spos);

  LABEL_IDX Lookup_cleanups(WN *wn, INITV_IDX& iv, bool is_dtor_or_ctor_call);

  INITV_IDX Create_handler_list(int scope_index);

  WN *ConvertGCCAsmStmt(const clang::GCCAsmStmt *stmt);
  
  WN *ConvertAttributedStmt(const clang::AttributedStmt *stmt);
  
  WN *ConvertBreakStmt(const clang::BreakStmt *stmt);
  
  WN *ConvertCallExpr(const clang::CallExpr *stmt);

  WN *ConvertCXXMemberCallExpr(const clang::CXXMemberCallExpr *stmt);

  WN *ConvertCXXOperatorCallExpr(const clang::CXXOperatorCallExpr *stmt);
  
  WN *ConvertCapturedStmt(const clang::CapturedStmt *stmt);
  
  void Pop_scope_and_do_cleanups();

  void SetEHRegionForCall(WN *blk, WN *wn);

  WN *ConvertContinueStmt(const clang::ContinueStmt *stmt);

  void Do_cleanups_for_eh(INT from);

  void Emit_cleanup(const clang::QualType type, ST_IDX st_idx, bool need_region);
  
  void Expand_handlers_or_cleanup(const HANDLER_INFO &handler_info);

  void Emit_begin_catch(const clang::CXXCatchStmt *stmt);

  void Build_filter_cmp(int filter, LABEL_IDX label, bool is_throw);

  void AppendStmt(const clang::Stmt *stmt);

  WN *ConvertCXXCatchStmt(const clang::CXXCatchStmt *stmt);
  
  WN *ConvertCXXForRangeStmt(const clang::CXXForRangeStmt *stmt);
  
  WN *ConvertCXXThrowExpr(const clang::CXXThrowExpr *stmt);

  WN *ConvertCXXTryStmt(const clang::CXXTryStmt *stmt);

  WN *Init_var_decl(const clang::VarDecl*);

  void Handle_variable_array(clang::QualType type, ST_IDX st_idx);
  
  WN *ConvertDeclStmt(const clang::DeclStmt *stmt);

  LABEL_IDX Get_label_idx(const clang::Stmt* stmt);

  template<typename _T>
  WN *ConvertLoopStmt(const _T *stmt);

  WN *ConvertDoStmt(const clang::DoStmt *stmt);
  
  WN *ConvertStmtExpr(const clang::StmtExpr *stmt);
  
  WN *ConvertForStmt(const clang::ForStmt *stmt);
  
  WN *ConvertGotoStmt(const clang::GotoStmt *stmt);
  
  WN *ConvertIfStmt(const clang::IfStmt *stmt);
  
  WN *ConvertIndirectGotoStmt(const clang::IndirectGotoStmt *stmt);
  
  WN *ConvertLabelStmt(const clang::LabelStmt *stmt);
  
  WN *ConvertReturnStmt(const clang::ReturnStmt *stmt);
  
  WN *ConvertSwitchStmt(const clang::SwitchStmt *stmt);

  WN *ConvertCaseStmt(const clang::CaseStmt *stmt);
  
  WN *ConvertDefaultStmt(const clang::DefaultStmt *stmt);

  WN *ConvertWhileStmt(const clang::WhileStmt *stmt);
  
  WN *ConvertExprWithCleanups(const clang::ExprWithCleanups *stmt);
  
  WN *ConvertCXXNewExpr(const clang::CXXNewExpr *stmt);

  WN *ConvertCXXDeleteExpr(const clang::CXXDeleteExpr *stmt);

  void pop_dtor_call_for_return(WN *block);

  void pop_dtor_call_stack(WN *block, bool gen_dtor);
  
  SRCPOS  SetSrcPos(clang::SourceLocation sl) { return _builder->SetSrcPos(sl); }
  SRCPOS  GetSrcPos(void)                     { return _builder->GetSrcPos(); }

  void HandleConditionVar(const clang::Decl *decl);

  ST *Gen_alloca_0();

  void Gen_dealloca(ST *alloca_st, vector<ST*> *vars);

public:
  WN *ConvertStmt(const clang::Stmt *stmt);
  
  WN *ConvertCompoundStmt(const clang::CompoundStmt *stmt, ST_IDX st_idx = ST_IDX_ZERO);

  // The function will emit constructor call
  WN *Emit_cxx_constructor_call(const clang::VarDecl *decl);

  // The function will emit destructor call
  WN *Emit_cxx_destructor_call(clang::QualType type, ST_IDX arg_st);

  WN *ConvertExpr(const clang::Expr *stmt, Result target = Result::nwNone());

  void Build_eh_type_info();

  WN *Setup_eh_region(WN *wn, SRCPOS spos, bool for_unwinding, bool is_dtor_or_ctor_call = FALSE);

  void Do_handlers(INT cleanups = 0);

  void pop_dtor_for_copy_ctor_stack(WN *block, clang::QualType ret_ty = clang::QualType());

  bool Set_current_scope_has_alloca(int &idx);

  void Set_current_scope_alloca_st(ST *st, int idx);

  void Add_current_scope_alloca_st(ST *st, int idx);

};

}

#endif /* CLANG2WHIRL_STMT_H */
